
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

#load NHGIS tract shapefiles
tract_2000_shp <- st_read("./input/geo/US_tract_2000.shp")
tract_2010_shp <- st_read("./input/geo/US_tract_2010.shp")
tract_2016_shp <- st_read("./input/geo/US_tract_2016.shp")

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

tract_2000_shp <- tract_2000_shp %>%
  mutate(year = "2000",
         geoid = paste0(str_sub(GISJOIN2, 1, 2),
                        str_sub(GISJOIN2, 4, 6),
                        str_sub(GISJOIN2, 8, 13))) %>%
  select(year, geoid, GISJOIN, geometry)

tract_2010_shp <- tract_2010_shp %>% 
  mutate(year = "2006-2010") %>%
  select(year, geoid = GEOID10, GISJOIN, geometry)

tract_2016_shp <- tract_2016_shp %>% 
  mutate(year = "2012-2016") %>%
  select(year, geoid = GEOID, GISJOIN, geometry)

tract_shp <- bind_rows(tract_2000_shp, tract_2010_shp, tract_2016_shp)

ctpp <- left_join(tract_shp, ctpp)

ctpp %>% filter(!geoid %in% tract_shp$geoid) %>% pull(tractfp) %>% table


#### D. Add metro name, filter to metros ---------------------------------------

cbsa <- cbsa%>% 
  st_drop_geometry %>% 
  filter(MEMI10 == 1) %>% 
  select(cbsafp = CBSAFP10, metro_name = NAME10)

ctpp <- ctpp %>%
  inner_join(cbsa)


#### E. Prepare and append other tract estimates -------------------------------

#2000 estimates
tract_2000 <- tract_2000 %>%
  mutate(trt_tot_pop = FL5001,
         trt_tot_nhw = FMS001,
         trt_tot_nhb = FMS002,
         trt_tot_nha = FMS004 + FMS005,
         trt_tot_h = FMS008 + FMS009 + FMS010 + FMS011 + FMS012 + FMS013 + FMS014,
         trt_med_hh_inc = GMY001,
         trt_tot_pov = GN6001,
         trt_tot_pov_det = GN6001 + GN6002,
         year = "2000") %>%
  select(GISJOIN, year, starts_with("trt_"))

#2010 estimates
tract_2010 <- tract_2010 %>%
  mutate(trt_tot_pop = JMAE001,
         trt_tot_nhw = JMJE003,
         trt_tot_nhb = JMJE004,
         trt_tot_nha = JMJE006 + JMJE007,
         trt_tot_h = JMJE012,
         trt_med_hh_inc = JOIE001,
         trt_tot_pov = JOCE002 + JOCE003,
         trt_tot_pov_det = JOCE001,
         year = "2006-2010") %>%
  select(GISJOIN, year, starts_with("trt_"))

#2016 estimates
tract_2016 <- tract_2016 %>%
  mutate(trt_tot_pop = AF2LE001,
         trt_tot_nhw = AF2UE003,
         trt_tot_nhb = AF2UE004,
         trt_tot_nha = AF2UE006 + AF2UE007,
         trt_tot_h = AF2UE012,
         trt_med_hh_inc = AF49E001,
         trt_tot_pov = AF43E002 + AF43E003,
         trt_tot_pov_det = AF43E001,
         year = "2012-2016") %>%
  select(GISJOIN, year, starts_with("trt_"))

#now combine into single data frame
tract <- bind_rows(tract_2000, tract_2010, tract_2016)

#join to CTPP data by tract and year vals
ctpp <- left_join(ctpp, tract)

#make sure there's no grouping applied to the table
ctpp <- ungroup(ctpp)

#now mutate a few columns for nighttime racial/eth composition
ctpp <- ctpp %>%
  mutate(trt_tot_nho = trt_tot_pop - (trt_tot_nhw + trt_tot_nhb + trt_tot_nha + trt_tot_h),
         trt_shr_nhw_pm = trt_tot_nhw/trt_tot_pop,
         trt_shr_nhb_pm = trt_tot_nhb/trt_tot_pop,
         trt_shr_h_pm = trt_tot_h/trt_tot_pop,
         trt_shr_nha_pm = trt_tot_nha/trt_tot_pop,
         trt_shr_nho_pm = trt_tot_nho/trt_tot_pop)

#compute "daytime" compositions using total pop estimates + day among workers
ctpp <- ctpp %>%
  mutate(trt_tot_nhw_am = trt_tot_nhw - (res_nhw_est - pow_nhw_est),
         trt_tot_nhb_am = trt_tot_nhb - (res_nhb_est - pow_nhb_est),
         trt_tot_h_am = trt_tot_h - (res_h_est - pow_h_est),
         trt_tot_nha_am = trt_tot_nha - (res_nha_est - pow_nha_est),
         trt_tot_nho_am = trt_tot_nho - (res_nho_est - pow_nho_est)) %>%
  mutate_at(vars(ends_with("_am")), ~ ifelse(. < 0, 0, .)) %>%
  mutate(trt_tot_pop_am = trt_tot_nhw_am + trt_tot_nhb_am + trt_tot_h_am + 
           trt_tot_nha_am + trt_tot_nho_am) %>%
  mutate(trt_shr_nhw_am = trt_tot_nhw_am/trt_tot_pop_am,
         trt_shr_nhb_am = trt_tot_nhb_am/trt_tot_pop_am,
         trt_shr_h_am = trt_tot_h_am/trt_tot_pop_am,
         trt_shr_nha_am = trt_tot_nha_am/trt_tot_pop_am,
         trt_shr_nho_am = trt_tot_nho_am/trt_tot_pop_am) 


#### F. Save to disk -----------------------------------------------------------

save(ctpp, file = "./input/ctpp-neigh-chg-data.RData")
                        