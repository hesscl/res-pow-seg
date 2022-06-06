
#dependencies
library(tidyverse)
library(sf)
library(furrr)

#working directory
setwd("H:/res-pow-seg")

#load NHGIS county shapefiles
county_2000 <- st_read("./input/geo/US_county_2000.shp")
county_2010 <- st_read("./input/geo/US_county_2010.shp")

#load NHGIS tract estimates
tract_2000_a <- read_csv("./input/nhgis0213_csv/nhgis0213_ds146_2000_tract.csv")
tract_2000_b <- read_csv("./input/nhgis0213_csv/nhgis0213_ds151_2000_tract.csv")
tract_2000 <- inner_join(tract_2000_a, tract_2000_b)

tract_2010_a <- read_csv("./input/nhgis0213_csv/nhgis0213_ds176_20105_tract.csv")
tract_2010_b <- read_csv("./input/nhgis0213_csv/nhgis0213_ds177_20105_tract.csv")
tract_2010 <- inner_join(tract_2010_a, tract_2010_b)

tract_2016_a <- read_csv("./input/nhgis0213_csv/nhgis0213_ds225_20165_tract.csv")
tract_2016_b <- read_csv("./input/nhgis0213_csv/nhgis0213_ds226_20165_tract.csv")
tract_2016 <- inner_join(tract_2016_a, tract_2016_b)

#load the 2000 blockgroup to tract crosswalk
tract_cw <- read_csv("./input/cw/crosswalk_2000_2010.csv")

#load NHGIS tract shapefiles
tract_2000_shp <- st_read("./input/geo/US_tract_2000.shp")
tract_2010_shp <- st_read("./input/geo/US_tract_2010.shp")
tract_2016_shp <- st_read("./input/geo/US_tract_2016.shp")

#load NHGIS cbsa shapefile
cbsa_2010 <- st_read("./input/geo/US_cbsa_2010.shp")
cbsa <- st_read("./input/geo/US_cbsa_2013.shp")

#load NHGIS CBSA estimates
cbsa_2016_a <- read_csv("./input/nhgis0195_csv/nhgis0195_ds225_20165_2016_cbsa.csv")
cbsa_2016_b <- read_csv("./input/nhgis0195_csv/nhgis0195_ds226_20165_2016_cbsa.csv")
cbsa_2016 <- inner_join(cbsa_2016_a, cbsa_2016_b)

#load NHGIS place shapefile
place <- st_read("./input/geo/US_place_2010.shp")

#load Holian and Kahn CBD coordinates
cbd_geocodes <- read_csv("./input/CBD_geocodes.csv") %>%
  select(CBSAFP10 = CBSA_code, cbd_lat = CBDlat, cbd_lng = CBDlon) %>%
  mutate(CBSAFP10 = as.character(CBSAFP10))


#### A. Load and munge tract CTPP extracts to tract-year table -----------------

#create vector of CTPP extract files we want to read in and process
ctpp_tract_files <- Sys.glob("./input/ctpp/tract-earnings/*.csv")
names(ctpp_tract_files) <- ctpp_tract_files
line_skips <- c(2, 2, rep(3, 4))

process_ctpp <- function(filepath){
  
  #vary the number of line skips that read.csv uses based on obs year
  n_skips <- ifelse(str_detect(filepath, "2000"), 2, 3)
  
  #vary the column names based on obs year
  if(str_detect(filepath, "2000")){
    file_col_names <- c("tract_str", "sex", "earnings_str", "est_type", "value", "blah")
    } else{
      file_col_names <- c("tract_str", "earnings_str", "est_type", "value")
    }
  
  #load with read.csv since it handles the format better
  file <- read.csv(filepath, skip = n_skips,
                   col.names = file_col_names)
  
  #now process the geographic identifiers
  processed <- file %>%
    mutate(type = ifelse(str_detect(filepath, "pow"), "pow", "res"),
           year = case_when(str_detect(filepath, "2000_") ~ "2000",
                            str_detect(filepath, "2006_2010_") ~ "2006-2010 ACS",
                            str_detect(filepath, "2012_2016_") ~ "2012-2016 ACS")) %>%
    filter(earnings_str != "", est_type == "Estimate") %>%
    select(-starts_with("sex"), -starts_with("blah")) %>%
    filter(!tract_str %in% c("Canada", "Mexico", "Other Foreign Countries")) %>%
    mutate(tractfp = str_split_fixed(tract_str, ", ", n = 3)[,1],
           tractfp = str_remove_all(tractfp, "Census Tract "),
           tractfp_pre = str_split_fixed(tractfp, "\\.", n = 2)[,1],
           tractfp_post = str_split_fixed(tractfp, "\\.", n = 2)[,2],
           tractfp_post = ifelse(is.na(tractfp_post), "00", tractfp_post),
           tractfp = paste0(str_pad(tractfp_pre, 4, "left", "0"),
                            str_pad(tractfp_post, 2, "left", "0")),
           countylsad = str_split_fixed(tract_str, ", ", n = 3)[,2],
           state = str_split_fixed(tract_str, ", ", n = 3)[,3],
           county = str_remove_all(countylsad, " County| Parish| Census Area| Municipality| City and Borough| Borough| city"),
           county = ifelse(state == "Illinois" & county == "La Salle", "LaSalle", county),
           county = ifelse(state == "Louisiana" & county == "LaSalle", "La Salle", county),
           county = ifelse(state == "New Mexico" & county == "Dona Ana", "Do?a Ana", county)) %>%
    mutate_at(vars(county, countylsad), ~ str_trim(., side = "both"))
  
  #now process the earnings related columns
  processed <- processed %>%
    filter(!str_detect(earnings_str, "Total")) %>%
    mutate(earnings_lower = str_split_fixed(earnings_str, " to ", n = 2)[,1],
           earnings_lower = case_when(
             str_detect(earnings_lower, "loss|No earnings") ~ 0,
             TRUE ~ parse_number(earnings_lower)),
           earnings_upper = str_split_fixed(earnings_str, " to ", n = 2)[,2],
           earnings_upper = case_when(
             str_detect(earnings_str, "No earnings") ~ 4999,
             str_detect(earnings_str, "or more") ~ Inf,
             str_detect(earnings_str, "or loss") ~ 4999,
             TRUE ~ parse_number(earnings_upper)),
           value = parse_number(value)) %>%
    mutate(earnings_lower = ifelse(earnings_lower == 1, 0, earnings_lower)) %>%
    group_by(tractfp, county, countylsad, state, type, year, earnings_lower, earnings_upper) %>%
    summarize(estimate = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(state, county, tractfp) %>%
    mutate(earnings_cat = case_when(
      earnings_upper < 25000 ~ "0-24999",
      earnings_upper >= 25000 & earnings_upper < 50000 ~ "25000-49999",
      earnings_upper >= 50000 & earnings_upper < 75000 ~ "50000-74999",
      earnings_upper >= 75000 ~ "75000+"))
    
  #return the processed file
  processed
} 

#test <- process_ctpp(ctpp_tract_files[3])
#glimpse(test)

plan(multisession, workers = 6)

ctpp <- future_map(ctpp_tract_files, process_ctpp)
ctpp <- reduce(ctpp, bind_rows)

plan(sequential)

#0-24999
#25000-49999
#50000-74999
#75000-Inf

glimpse(ctpp)

ctpp <- ctpp %>%
  mutate(countylsad = ifelse(countylsad == "Richmond County" & state == "Virginia",
                             "Richmond city", countylsad)) %>% #we want to just collapse the city/county tract parts into one
  group_by(tractfp, state, county, countylsad, type, year, earnings_cat) %>%
  summarize(total = sum(estimate)) %>%
  ungroup() %>%
  mutate(earnings_cat = str_replace_all(earnings_cat, "-", "_"),
         earnings_cat = str_replace_all(earnings_cat, "\\+", "_or_more")) %>%
  pivot_wider(id_cols = c(tractfp, county, countylsad, state, year, type), 
              names_from = earnings_cat, names_prefix = "earn_", values_from = total)


#### B. Append proper geographic identifiers to the CTPP data ------------------

#the CTPP data don't seem to use this set of county delineations, but state IDs
#still needed for crosswalking to the 2010 data
state_crosswalk <- county_2000 %>%
  st_drop_geometry() %>%
  mutate(state = STATENAM, 
         statefp = str_sub(NHGISST, 1, 2)) %>%
  distinct(state, statefp)

#prep the 2010 data to be joined on time-invariant basis, also ensure DC included
county <- county_2010 %>%
  st_drop_geometry() %>%
  select(county = NAME10, countylsad = NAMELSAD10, 
         cbsafp10 = CBSAFP10, statefp = STATEFP10, countyfp = COUNTYFP10) %>%
  mutate_at(vars(county, countylsad), ~ str_trim(., side = "both")) %>%
  left_join(state_crosswalk) %>%
  filter(!is.na(cbsafp10))

#join the state and county fips codes to the CTPP data
ctpp <- left_join(ctpp, county) 

#now construct a tract ID that we can hopefully use in other contexts
ctpp <- ctpp %>% 
  filter(!is.na(statefp)) %>%
  mutate(geoid = paste0(statefp, countyfp, tractfp))


#### C. Add tract geometry -----------------------------------------------------

#2000 tracts
tract_2000_shp <- tract_2000_shp %>%
  mutate(year = "2000",
         geoid = paste0(str_sub(GISJOIN2, 1, 2),
                        str_sub(GISJOIN2, 4, 6),
                        str_sub(GISJOIN2, 8, 13))) %>%
  select(year, geoid, GISJOIN, geometry)

#2006-2010 ACS tracts
tract_2010_shp <- tract_2010_shp %>% 
  mutate(year = "2006-2010 ACS") %>%
  select(year, geoid = GEOID10, GISJOIN, geometry)

#2012-2016 ACS tracts
tract_2016_shp <- tract_2016_shp %>% 
  mutate(year = "2012-2016 ACS") %>%
  select(year, geoid = GEOID, GISJOIN, geometry)

#bind together
tract_shp <- bind_rows(tract_2000_shp, tract_2010_shp, tract_2016_shp)

#append to ctpp by geoid
ctpp <- left_join(tract_shp, ctpp)

#look at match quality
ctpp %>% filter(!geoid %in% tract_shp$geoid) %>% pull(tractfp) %>% table


#### D. Prepare to metro name, distance to CBD ---------------------------------

cbd_geocodes <- cbd_geocodes %>%
  rename(cbsafp10 = CBSAFP10) %>%
  st_as_sf(coords = c("cbd_lng", "cbd_lat"), remove = FALSE) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(cbsa_2010)) %>%
  rowwise() %>%
  mutate(cbd_lng = st_coordinates(geometry)[,1],
         cbd_lat = st_coordinates(geometry)[,2]) %>%
  ungroup() %>%
  st_drop_geometry()

tract_2010_cent <- tract_2010_shp %>%
  st_centroid() %>%
  st_join(cbsa_2010 %>% select(cbsafp10 = CBSAFP10, metro_name = NAME10)) %>%
  left_join(cbd_geocodes) %>%
  filter(!is.na(metro_name))

tract_2010_cent <- tract_2010_cent %>% 
  rowwise() %>%
  mutate(trt_lng = st_coordinates(geometry)[,1],
         trt_lat = st_coordinates(geometry)[,2]) %>%
  ungroup() %>%
  mutate(dist_to_cbd = sqrt((trt_lng - cbd_lng)^2 + (trt_lat - cbd_lat)^2))

tract_2010_cent <- tract_2010_cent %>%
  st_drop_geometry() %>%
  filter(!is.na(dist_to_cbd)) %>%
  select(geoid, GISJOIN, cbsafp10, metro_name, dist_to_cbd)


#### Harmonize the geography as best as possible -------------------------------

#remove unnecessary cols from crosswalk
tract_cw <- tract_cw %>% select(-(placefp10:changetype))

#use crosswalk to convert 2000 tract estimates to 2010 (much closer to ACS)
ctpp_2000 <- ctpp %>%
  filter(year == "2000") %>%
  st_drop_geometry() %>%
  left_join(tract_cw, by = c("geoid" = "trtid00")) %>%
  group_by(type, year, trtid10) %>%
  summarize_at(vars(starts_with("earn")), ~ sum(. * weight)) %>%
  ungroup() %>%
  rename(geoid = trtid10) %>%
  left_join(tract_2010_shp %>% select(-year)) %>%
  filter(!is.na(geoid)) %>%
  left_join(ctpp %>% st_drop_geometry() %>% 
              filter(year == "2006-2010 ACS") %>% 
              distinct(geoid, cbsafp10, tractfp, state, county, countylsad))

#now replace the 2000 delineated estimates with our 2010 delineated estimates
ctpp <- ctpp %>%
  filter(year != "2000") %>%
  bind_rows(ctpp_2000)

#identify whether centroid falls within place
tract_places <- tract_2010_shp %>%
  select(GISJOIN) %>%
  st_centroid() %>%
  st_join(place %>% select(place_name = NAME10, prin_city = PCICBSA10)) %>%
  st_drop_geometry()

#join place information to the ctpp data
ctpp <- inner_join(ctpp, tract_places)

#join cbsa and distance to CBD data frame
ctpp <- inner_join(ctpp, tract_2010_cent) %>%
  filter(!is.na(metro_name)) %>%
  group_by(geoid) %>%
  fill(countylsad, statefp, countyfp) %>%
  ungroup() %>%
  st_drop_geometry()

#remove tracts without CTPP estimates
ctpp <- ctpp %>%
  filter(!is.na(type))


#### Calculate income diversity ------------------------------------------------

ctpp <- ctpp %>%
  mutate(tot_workers = earn_0_24999 + earn_25000_49999 + earn_50000_74999 + earn_75000_or_more,
         shr_earn_0_24999 = earn_0_24999/tot_workers,
         shr_earn_25000_49999 = earn_25000_49999/tot_workers,
         shr_earn_50000_74999 = earn_50000_74999/tot_workers,
         shr_earn_75000_or_more = earn_75000_or_more/tot_workers,
         earn_div = shr_earn_0_24999 * ifelse(shr_earn_0_24999 != 0, log(1/shr_earn_0_24999), 0) + 
           shr_earn_25000_49999 * ifelse(shr_earn_25000_49999 != 0, log(1/shr_earn_25000_49999), 0) +
           shr_earn_50000_74999 * ifelse(shr_earn_50000_74999 != 0, log(1/shr_earn_50000_74999), 0) + 
           shr_earn_75000_or_more * ifelse(shr_earn_75000_or_more != 0, log(1/shr_earn_75000_or_more), 0)) 


#### Save the processed table --------------------------------------------------

write_csv(ctpp, "./input/ctpp-earnings-data.csv")
