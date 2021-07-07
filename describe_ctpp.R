
#dependencies
library(tidyverse)
library(sf)

#load the data
load("./input/ctpp-neigh-chg-data.RData")


####  Diagnostics --------------------------------------------------------------

#plot non-hispanic black counts for chicago metro area
test_chi <- ctpp %>% filter(cbsafp == "16980")

ggplot(test_chi, aes(fill = res_nhb_est)) + 
  facet_wrap(~ year) + 
  geom_sf(color = NA)

#plot non-hispanic white % for Seattle metro area
test_sea <- ctpp %>% filter(cbsafp == "42660")

ggplot(test_sea, aes(fill = res_nhw_est/res_total_est)) + 
  facet_wrap(~ year) + 
  geom_sf(color = NA)


#### Filter metro set ----------------------------------------------------------

ctpp_metros <- ctpp %>%
  st_drop_geometry() %>%
  filter(year == "2006-2010") %>%
  group_by(cbsafp) %>%
  summarize(nhb = sum(res_nhb_est, na.rm = TRUE) >= 3000,
            nhw = sum(res_nhw_est, na.rm = TRUE) >= 3000,
            h = sum(res_h_est, na.rm = TRUE) >= 3000,
            nha = sum(res_nha_est, na.rm = TRUE) >= 3000) %>%
  mutate_at(vars(everything(), -cbsafp), ~ ifelse(., cbsafp, NA)) %>%
  select(-cbsafp)
  

#### Trends in segregation -----------------------------------------------------

res_seg <- ctpp %>%
  st_drop_geometry %>%
  filter(!is.na(res_nhb_est), !is.na(res_nhw_est)) %>%
  group_by(year, cbsafp, metro_name) %>%
  summarize(metro_n = length(unique(geoid)),
            metro_nhb = sum(res_nhb_est),
            metro_pop = sum(res_total_est),
            dis_nhb_nhw = (.5) * sum(abs(res_nhb_est/sum(res_nhb_est) - 
                                               res_nhw_est/sum(res_nhw_est)))) %>%
  mutate(type = "Residence")

pow_seg <- ctpp %>%
  st_drop_geometry %>%
  filter(!is.na(pow_nhb_est), !is.na(pow_nhw_est)) %>%
  group_by(year, cbsafp, metro_name) %>%
  summarize(metro_n = length(unique(geoid)),
            metro_nhb = sum(pow_nhb_est),
            metro_pop = sum(pow_total_est),
            dis_nhb_nhw = (.5) * sum(abs(pow_nhb_est/sum(pow_nhb_est) - 
                                           pow_nhw_est/sum(pow_nhw_est)))) %>%
  mutate(type = "Workplace")


nhb_seg <- bind_rows(res_seg, pow_seg) %>%
  filter(cbsafp %in% ctpp_metros$nhb) %>%
  mutate(seg = "Black-White")

nhb_seg_trends <- seg %>%
  group_by(type, year) %>%
  summarize(n = n(),
            mean = weighted.mean(dis_nhb_nhw, w = metro_nhb))
seg_trends

#### Neighborhood change matrix ------------------------------------------------

#what are the typical intraday trajectories for a given neighborhood
