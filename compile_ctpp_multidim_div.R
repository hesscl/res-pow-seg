
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


#### Function to process the trt strings ---------------------------------------

process_trt_string <- function(df){
 df %>%
    mutate(trt_string = iconv(trt_string, "latin1", "UTF-8"),
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
           state = str_split_fixed(trt_county_state, ", ", n = 2)[,2])
}


#### Load the age estimates ----------------------------------------------------

# 16-24
# 25-44
# 45-64
# 65-over

#identify the files we need to process
age_filepaths <- Sys.glob("./input/ctpp/multidim-div/ctpp_2012_2016_age_*")

#load them and combine into single data frame
age_tbl <- map_dfr(age_filepaths, function(filepath){
  read.csv(filepath, skip = 2, 
           col.names = c("trt_string", "age", "est_type", "value", "X")) %>%
    filter(est_type == "Estimate", !str_detect(age, "Total")) %>%
    mutate(value = parse_number(value)) %>%
    select(-X, -est_type) %>%
    as_tibble() %>%
    mutate(type = ifelse(str_detect(filepath, "pow"), "pow", "res"))
}) 

#then process tract information, collapse the age categories and pivot wider
age_tbl <- age_tbl %>%
  process_trt_string() %>%
  mutate(age_cat = case_when(
    age %in% c("16 and 17 years", "18 to 20 years",  "18 to 24 years", "21 to 24 years") ~ "16_24",
    age %in% c("25 to 34 years", "25 to 44 years", "35 to 44 years") ~ "25_44",
    age %in% c("45 to 59 years", "60 to 64 years") ~ "45_64",
    age %in% c("65 to 74 years", "75 years and over") ~ "65_or_older"
  )) %>%
  group_by(type, tractfp, countylsad, county, state, age_cat) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(type, tractfp, countylsad, county, state), 
              names_from = age_cat, names_prefix = "tot_age_",
              values_from = value)


#### Load the language estimates -----------------------------------------------

#identify the files we need to process
lang_filepaths <- Sys.glob("./input/ctpp/multidim-div/ctpp_2012_2016_language_*")

#load them and combine into single data frame
lang_tbl <- map_dfr(lang_filepaths, function(filepath){
  read.csv(filepath, skip = 2,
           col.names = c("trt_string", "blah", "lang", "est_type", "value", "X")) %>%
    filter(est_type == "Estimate", !str_detect(lang, "Total")) %>%
    mutate(value = parse_number(value)) %>%
    select(-X, -blah, -est_type) %>%
    as_tibble() %>%
    mutate(type = ifelse(str_detect(filepath, "pow"), "pow", "res"))
}) 

#then process tract information, collapse the age categories and pivot wider
lang_tbl <- lang_tbl %>%
  process_trt_string() %>%
  mutate(lang_cat = case_when(
    lang == "Speak only English" ~ "eng",
    lang == "Spanish" ~ "span",
    TRUE ~ "oth"
  )) %>%
  group_by(type, tractfp, countylsad, county, state, lang_cat) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(type, tractfp, countylsad, county, state), 
              names_from = lang_cat, names_prefix = "tot_lang_",
              values_from = value)


#### Load the sex estimates ----------------------------------------------------

#identify the files we need to process
sex_filepaths <- Sys.glob("./input/ctpp/multidim-div/ctpp_2012_2016_sex_*")

#load them and combine into single data frame
sex_tbl <- map_dfr(sex_filepaths, function(filepath){
  read.csv(filepath, skip = 2,
           col.names = c("trt_string", "sex", "est_type", "value", "X")) %>%
    filter(est_type == "Estimate", !str_detect(sex, "Both")) %>%
    mutate(value = parse_number(value)) %>%
    select(-X, -est_type) %>%
    as_tibble() %>%
    mutate(type = ifelse(str_detect(filepath, "pow"), "pow", "res"))
}) 

#then process tract information, collapse the age categories and pivot wider
sex_tbl <- sex_tbl %>%
  process_trt_string() %>%
  mutate(sex = tolower(sex)) %>%
  pivot_wider(id_cols = c(type, tractfp, countylsad, county, state), 
              names_from = sex, names_prefix = "tot_sex_",
              values_from = value)


#### Load the poverty estimates ------------------------------------------------

#identify the files we need to process
pov_filepaths <- Sys.glob("./input/ctpp/multidim-div/ctpp_2012_2016_poverty_*")

#load them and combine into single data frame
pov_tbl <- map_dfr(pov_filepaths, function(filepath){
  read.csv(filepath, skip = 2,
           col.names = c("trt_string", "pov_status", "blah", "est_type", "value", "X")) %>%
    filter(est_type == "Estimate", !str_detect(pov_status, "Total")) %>%
    mutate(value = parse_number(value)) %>%
    select(-X, -blah, -est_type) %>%
    as_tibble() %>%
    mutate(type = ifelse(str_detect(filepath, "pow"), "pow", "res"))
}) 

pov_tbl <- pov_tbl %>%
  process_trt_string() %>%
  mutate(pov_cat = case_when(
    pov_status == "100 to 149 percent of the poverty level" ~ "100_149_pct_pov",
    pov_status == "At or above 150 percent of the poverty level" ~ "150_pct_plus_pov",
    pov_status == "Below 100 percent of the poverty level" ~ "below_100_pct_pov"
  )) %>%
  pivot_wider(id_cols = c(type, tractfp, countylsad, county, state), 
              names_from = pov_cat, names_prefix = "tot_",
              values_from = value)


#### Join the data -------------------------------------------------------------

#they are all tract-timeofday level so join on all identifiers
ctpp <- age_tbl %>%
  inner_join(sex_tbl) %>%
  inner_join(lang_tbl) %>%
  inner_join(pov_tbl)

#check the result
glimpse(ctpp)


#### Append proper geographic identifiers to the CTPP data --------------------

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


#### Save to disk --------------------------------------------------------------

write_csv(ctpp, "./input/ctpp_multidim_div.csv")

