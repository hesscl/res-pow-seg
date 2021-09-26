
#dependencies
library(tidyverse)
library(sf)

#working directory
setwd("H:/res-pow-seg")

#load NHGIS county shapefiles
county_2000 <- st_read("./input/geo/US_county_2000.shp")
county_2010 <- st_read("./input/geo/US_county_2010.shp")

#load NHGIS tract estimates
tract_2000_a <- read_csv("./input/nhgis0189_csv/nhgis0189_ds146_2000_tract.csv")
tract_2000_b <- read_csv("./input/nhgis0189_csv/nhgis0189_ds151_2000_tract.csv")
tract_2000 <- inner_join(tract_2000_a, tract_2000_b)
tract_2010 <- read_csv("./input/nhgis0189_csv/nhgis0189_ds176_20105_2010_tract.csv")
tract_2016 <- read_csv("./input/nhgis0189_csv/nhgis0189_ds225_20165_2016_tract.csv")

#load the 2000 blockgroup to tract crosswalk
tract_cw <- read_csv("./input/cw/crosswalk_2000_2010.csv")

#load NHGIS tract shapefiles
tract_2000_shp <- st_read("./input/geo/US_tract_2000.shp")
tract_2010_shp <- st_read("./input/geo/US_tract_2010.shp")
tract_2016_shp <- st_read("./input/geo/US_tract_2016.shp")

#load NHGIS cbsa shapefile
cbsa <- st_read("./input/geo/US_cbsa_2010.shp")

#load NHGIS place shapefile
place <- st_read("./input/geo/US_place_2010.shp")


#### A. Load and munge tract CTPP extracts to tract-year table -----------------

#create vector of CTPP extract files we want to read in and process
ctpp_tract_files <- Sys.glob("./input/ctpp/tract/*.csv")
names(ctpp_tract_files) <- ctpp_tract_files
line_skips <- c(4, 4, rep(3, 4))

#all have the same set of column names
ctpp_colnames <- c("trt_string", "total_est", "total_moe", "w_est", "w_moe", 
  "b_est", "b_moe", "a_est", "a_moe", "o_est", "o_moe", "h_est", "h_moe", "hw_est", 
  "hw_moe", "hb_est", "hb_moe", "ha_est", "ha_moe", "ho_est", "ho_moe", "nh_est", 
  "nh_moe","nhw_est", "nhw_moe", "nhb_est", "nhb_moe", "nha_est", "nha_moe", 
  "nho_est", "nho_moe", "empty")

#map each file string through read_csv and do a little processing
ctpp <- list(file = ctpp_tract_files, 
             filename = names(ctpp_tract_files), 
             skips = line_skips) %>%
  pmap(function(file, filename, skips){
    read.csv(file, skip = skips, col.names = ctpp_colnames) %>%
               select(-empty) %>%
               slice(-c(1:4)) %>%
    mutate(across(where(is.character) & !trt_string, parse_number)) %>%
    mutate(file = filename)}) %>%
  reduce(bind_rows) %>%
  filter(!str_detect(trt_string, "American Community Survey")) %>%
  mutate(year = str_remove_all(file, "./input/ctpp/tract/|residence.csv|workplace.csv"),
         type = ifelse(str_detect(file, "workplace"), "pow", "res"),
         year = case_when(year == "2000_" ~ "2000",
                          year == "2006_2010_" ~ "2006-2010 ACS",
                          year == "2012_2016_" ~ "2012-2016 ACS"),
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
         county = ifelse(state == "New Mexico" & county == "Dona Ana", "Doña Ana", county)) %>%
  mutate_at(vars(ends_with("_est"), ends_with("_moe")), ~ ifelse(is.na(.), 0, .))

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


#### D. Add metro name, filter to metros ---------------------------------------

cbsa <- cbsa %>% 
  st_drop_geometry %>% 
  filter(MEMI10 == 1) %>% 
  select(cbsafp = CBSAFP10, metro_name = NAME10)

ctpp <- ctpp %>%
  inner_join(cbsa)


#### E. Prepare and append other tract estimates -------------------------------

# #2000 estimates
# tract_2000 <- tract_2000 %>%
#   mutate(trt_tot_pop = FL5001,
#          trt_tot_nhw = FMS001,
#          trt_tot_nhb = FMS002,
#          trt_tot_nha = FMS004 + FMS005,
#          trt_tot_h = FMS008 + FMS009 + FMS010 + FMS011 + FMS012 + FMS013 + FMS014,
#          trt_tot_pov = GN6001,
#          trt_tot_pov_det = GN6001 + GN6002,
#          year = "2000") %>%
#   select(GISJOIN, year, starts_with("trt_"))
# 
# #2010 estimates
# tract_2010 <- tract_2010 %>%
#   mutate(trt_tot_pop = JMAE001,
#          trt_tot_nhw = JMJE003,
#          trt_tot_nhb = JMJE004,
#          trt_tot_nha = JMJE006 + JMJE007,
#          trt_tot_h = JMJE012,
#          trt_tot_pov = JOCE002 + JOCE003,
#          trt_tot_pov_det = JOCE001,
#          year = "2006-2010 ACS") %>%
#   select(GISJOIN, year, starts_with("trt_"))
# 
# #2016 estimates
# tract_2016 <- tract_2016 %>%
#   mutate(trt_tot_pop = AF2LE001,
#          trt_tot_nhw = AF2UE003,
#          trt_tot_nhb = AF2UE004,
#          trt_tot_nha = AF2UE006 + AF2UE007,
#          trt_tot_h = AF2UE012,
#          trt_tot_pov = AF43E002 + AF43E003,
#          trt_tot_pov_det = AF43E001,
#          year = "2012-2016 ACS") %>%
#   select(GISJOIN, year, starts_with("trt_"))
# 
# #now combine into single data frame
# tract <- bind_rows(tract_2000, tract_2010, tract_2016)
# 
# #join to CTPP data by tract and year vals
# ctpp <- left_join(ctpp, tract)
# 
# #make sure there's no grouping applied to the table
# ctpp <- ungroup(ctpp)

ctpp <- ctpp %>%
   select(-(trt_string:state))


#### Harmonize the geography as best as possible -------------------------------

#remove unnecessary cols from crosswalk
tract_cw <- tract_cw %>% select(-(placefp10:changetype))

#use crosswalk to convert 2000 tract estimates to 2010 (much closer to ACS)
ctpp_2000 <- ctpp %>%
  filter(year == "2000") %>%
  st_drop_geometry() %>%
  left_join(tract_cw, by = c("geoid" = "trtid00")) %>%
  group_by(year, trtid10) %>%
  summarize_at(vars(starts_with("res"), starts_with("pow"), starts_with("trt_tot")),
            ~ sum(. * weight)) %>%
  ungroup() %>%
  rename(geoid = trtid10) %>%
  left_join(tract_2010_shp %>% select(-year)) %>%
  filter(!is.na(geoid)) %>%
  left_join(ctpp %>% st_drop_geometry() %>% 
              filter(year == "2006-2010 ACS") %>% 
              select(geoid, cbsafp, statefp, countyfp, metro_name)) %>%
  filter(!is.na(metro_name))

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


#### Now compute compositions since counts are good to go ----------------------

#now mutate a few columns for residential / nighttime racial/eth composition
ctpp <- ctpp %>%
  mutate(trt_shr_nhw_pm = res_nhw_est/res_total_est,
         trt_shr_nhb_pm = res_nhb_est/res_total_est,
         trt_shr_h_pm = res_h_est/res_total_est,
         trt_shr_nha_pm = res_nha_est/res_total_est,
         trt_shr_nho_pm = (res_total_est - (res_nhw_est + res_nhb_est + res_h_est + res_nha_est))/res_total_est)

#compute place of work / daytime race/eth comp
ctpp <- ctpp %>%
  mutate(trt_shr_nhw_am = pow_nhw_est/pow_total_est,
         trt_shr_nhb_am = pow_nhb_est/pow_total_est,
         trt_shr_h_am = pow_h_est/pow_total_est,
         trt_shr_nha_am = pow_nha_est/pow_total_est,
         trt_shr_nho_am = (pow_total_est - (pow_nhw_est + pow_nhb_est + pow_h_est + pow_nha_est))/pow_total_est)

#compute intraday changes
ctpp <- ctpp %>%
  mutate(chg_nhw = trt_shr_nhw_am -  trt_shr_nhw_pm,
         chg_nhb = trt_shr_nhb_am - trt_shr_nhb_pm,
         chg_nha = trt_shr_nha_am - trt_shr_nha_pm,
         chg_h = trt_shr_h_am - trt_shr_h_pm,
         chg_nho = trt_shr_nho_am - trt_shr_nho_pm) 


#### F. Save to disk -----------------------------------------------------------

save(ctpp, file = "./input/ctpp-neigh-chg-data.RData")
                        