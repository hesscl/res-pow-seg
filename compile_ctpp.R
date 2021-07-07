
#dependencies
library(tidyverse)
library(sf)

#working directory
setwd("H:/res-pow-seg")

#load NHGIS county shapefiles
county_2000 <- st_read("./input/geo/US_county_2000.shp")
county_2010 <- st_read("./input/geo/US_county_2010.shp")

#load NHGIS tract shapefile
tract_2000 <- st_read("./input/geo/US_tract_2000.shp")
tract_2010 <- st_read("./input/geo/US_tract_2010.shp")
tract_2016 <- st_read("./input/geo/US_tract_2016.shp")

#load NHGIS cbsa shapefile
cbsa <- st_read("./input/geo/US_cbsa_2010.shp")


#### A. Load and munge CTPP extracts to tract-year table -----------------------

#create vector of CTPP extract files we want to read in and process
ctpp_files <- Sys.glob("./input/ctpp/*.csv")
names(ctpp_files) <- ctpp_files
line_skips <- c(4, rep(3, 5))

#all have the same set of column names
ctpp_colnames <- c("trt_string", "total_est", "total_moe", "w_est", "w_moe", 
  "b_est", "b_moe", "a_est", "a_moe", "o_est", "o_moe", "h_est", "h_moe", "hw_est", 
  "hw_moe", "hb_est", "hb_moe", "ha_est", "ha_moe", "ho_est", "ho_moe", "nh_est", 
  "nh_moe","nhw_est", "nhw_moe", "nhb_est", "nhb_moe", "nha_est", "nha_moe", 
  "nho_est", "nho_moe", "empty")

#map each file string through read_csv and do a little processing
ctpp <- list(file = ctpp_files, filename = names(ctpp_files), skips = line_skips) %>%
  pmap(function(file, filename, skips){
    read_csv(file, skip = skips, col_names = ctpp_colnames, col_types = NULL) %>%
           select(-empty) %>%
           slice(-c(1:4)) %>%
           mutate_at(vars(everything(), -trt_string), parse_number) %>%
           mutate(file = filename)}) %>%
  reduce(bind_rows) %>%
  filter(!str_detect(trt_string, "American Community Survey")) %>%
  mutate(year = str_remove_all(file, "./input/ctpp/|residence.csv|workplace.csv"),
         type = ifelse(str_detect(file, "workplace"), "pow", "res"),
         year = case_when(year == "2000_" ~ "2000",
                          year == "2006_2010_" ~ "2006-2010",
                          year == "2012_2016_" ~ "2012-2016"),
         trt_string = iconv(trt_string, "latin1", "UTF-8"),
         trt_name = str_split_fixed(trt_string, ",", n = 2)[,1],
         tractfp = str_remove_all(trt_name, "Census Tract "),
         tractfp_pre = str_split_fixed(tractfp, "\\.", n = 2)[,1],
         tractfp_post = str_split_fixed(tractfp, "\\.", n = 2)[,2],
         tractfp_post = ifelse(is.na(tractfp_post), "00", tractfp_post),
         tractfp = paste0(str_pad(tractfp_pre, 4, "left", "0"),
                            str_pad(tractfp_post, 2, "left", "0")),
         trt_county_state = str_split_fixed(trt_string, ", ", n = 2)[,2],
         countylsad = str_split_fixed(trt_county_state, ", ", n = 2)[,1],
         countylsad = str_trim(countylsad),
         county = str_remove_all(countylsad, " County| Parish| Census Area| Municipality| City and Borough| Borough| city"),
         county = str_trim(county),
         state = str_split_fixed(trt_county_state, ", ", n = 2)[,2]) %>%
  select(-file, -trt_county_state) %>%
  pivot_wider(id_cols = c(trt_string, year, trt_name, tractfp, county, countylsad, state),
              names_from = type, names_glue = "{type}_{.value}",
              values_from = c(ends_with("_est"), ends_with("_moe"))) %>%
  filter(!state %in% c("Puerto Rico", ""), trt_name != "Tract 999999") %>%
  mutate(county = ifelse(state == "Illinois" & county == "La Salle", "LaSalle", county),
         county = ifelse(state == "Louisiana" & county == "LaSalle", "La Salle", county),
         county = ifelse(state == "New Mexico" & county == "Dona Ana", "Doña Ana", county))

#check result
glimpse(ctpp)


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
  select(county = NAME10, cbsafp = CBSAFP10, countylsad = NAMELSAD10, statefp = STATEFP10, countyfp = COUNTYFP10) %>%
  left_join(state_crosswalk) %>%
  filter(!(state == "Virginia" & countyfp == "159"))

#join the state and county fips codes to the CTPP data
ctpp <- left_join(ctpp, county) 

#now construct a tract ID that we can hopefully use in other contexts
ctpp <- ctpp %>% 
  filter(!is.na(statefp)) %>%
  mutate(geoid = paste0(statefp, countyfp, tractfp))


#### C. Add tract geometry -----------------------------------------------------

tract_2000 <- tract_2000 %>%
  mutate(year = "2000",
         geoid = paste0(str_sub(GISJOIN2, 1, 2),
                        str_sub(GISJOIN2, 4, 6),
                        str_sub(GISJOIN2, 8, 13))) %>%
  select(year, geoid, geometry)

tract_2010 <- tract_2010 %>% 
  mutate(year = "2006-2010") %>%
  select(year, geoid = GEOID10, geometry)

tract_2016 <- tract_2016 %>% 
  mutate(year = "2012-2016") %>%
  select(year, geoid = GEOID, geometry)

tract <- bind_rows(tract_2000, tract_2010, tract_2016)

ctpp <- left_join(tract, ctpp)

ctpp %>% filter(!geoid %in% tract$geoid) %>% pull(tractfp) %>% table


#### D. Add metro name, filter to metros ---------------------------------------

cbsa <- cbsa%>% 
  st_drop_geometry %>% 
  filter(MEMI10 == 1) %>% 
  select(cbsafp = CBSAFP10, metro_name = NAME10)

ctpp <- ctpp %>%
  inner_join(cbsa)


#### D. Save to disk -----------------------------------------------------------

save(ctpp, file = "./input/ctpp-neigh-chg-data.RData")
                        