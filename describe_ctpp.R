
#dependencies
library(tidyverse)
library(sf)

setwd("H:/res-pow-seg")

#load the data
load("./input/ctpp-neigh-chg-data.RData")

#remove cases without any people
ctpp <- ctpp %>%
  filter(trt_tot_pop > 0)


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

#black-white
nhb_res_seg <- ctpp %>%
  st_drop_geometry %>%
  filter(!is.na(res_nhb_est), !is.na(res_nhw_est)) %>%
  group_by(year, cbsafp, metro_name) %>%
  summarize(metro_n = length(unique(geoid)),
            metro_nhb = sum(res_nhb_est),
            metro_pop = sum(res_total_est),
            dis_nhb_nhw = (.5) * sum(abs(res_nhb_est/sum(res_nhb_est) - 
                                               res_nhw_est/sum(res_nhw_est)))) %>%
  mutate(type = "Residence")

nhb_pow_seg <- ctpp %>%
  st_drop_geometry %>%
  filter(!is.na(pow_nhb_est), !is.na(pow_nhw_est)) %>%
  group_by(year, cbsafp, metro_name) %>%
  summarize(metro_n = length(unique(geoid)),
            metro_nhb = sum(pow_nhb_est),
            metro_pop = sum(pow_total_est),
            dis_nhb_nhw = (.5) * sum(abs(pow_nhb_est/sum(pow_nhb_est) - 
                                           pow_nhw_est/sum(pow_nhw_est)))) %>%
  mutate(type = "Workplace")

nhb_seg <- bind_rows(nhb_res_seg, nhb_pow_seg) %>%
  filter(cbsafp %in% ctpp_metros$nhb) %>%
  mutate(seg = "Black-White")

#latinx-white
h_res_seg <- ctpp %>%
  st_drop_geometry %>%
  filter(!is.na(res_h_est), !is.na(res_nhw_est)) %>%
  group_by(year, cbsafp, metro_name) %>%
  summarize(metro_n = length(unique(geoid)),
            metro_h = sum(res_h_est),
            metro_pop = sum(res_total_est),
            dis_h_nhw = (.5) * sum(abs(res_h_est/sum(res_h_est) - 
                                           res_nhw_est/sum(res_nhw_est)))) %>%
  mutate(type = "Residence")

h_pow_seg <- ctpp %>%
  st_drop_geometry %>%
  filter(!is.na(pow_h_est), !is.na(pow_nhw_est)) %>%
  group_by(year, cbsafp, metro_name) %>%
  summarize(metro_n = length(unique(geoid)),
            metro_h = sum(pow_h_est),
            metro_pop = sum(pow_total_est),
            dis_h_nhw = (.5) * sum(abs(pow_h_est/sum(pow_h_est) - 
                                           pow_nhw_est/sum(pow_nhw_est)))) %>%
  mutate(type = "Workplace")

h_seg <- bind_rows(h_res_seg, h_pow_seg) %>%
  filter(cbsafp %in% ctpp_metros$h) %>%
  mutate(seg = "Latinx-White")


#asian-white
nha_res_seg <- ctpp %>%
  st_drop_geometry %>%
  filter(!is.na(res_nha_est), !is.na(res_nhw_est)) %>%
  group_by(year, cbsafp, metro_name) %>%
  summarize(metro_n = length(unique(geoid)),
            metro_nha = sum(res_nha_est),
            metro_pop = sum(res_total_est),
            dis_nha_nhw = (.5) * sum(abs(res_nha_est/sum(res_nha_est) - 
                                         res_nhw_est/sum(res_nhw_est)))) %>%
  mutate(type = "Residence")

nha_pow_seg <- ctpp %>%
  st_drop_geometry %>%
  filter(!is.na(pow_nha_est), !is.na(pow_nhw_est)) %>%
  group_by(year, cbsafp, metro_name) %>%
  summarize(metro_n = length(unique(geoid)),
            metro_nha = sum(pow_nha_est),
            metro_pop = sum(pow_total_est),
            dis_nha_nhw = (.5) * sum(abs(pow_nha_est/sum(pow_nha_est) - 
                                         pow_nhw_est/sum(pow_nhw_est)))) %>%
  mutate(type = "Workplace")

nha_seg <- bind_rows(nha_res_seg, nha_pow_seg) %>%
  filter(cbsafp %in% ctpp_metros$nha) %>%
  mutate(seg = "Asian/PI-White")


#compute weighted average according to metro pops
nhb_seg_trends <- nhb_seg %>%
  group_by(seg, type, year) %>%
  summarize(n = n(),
            mean = weighted.mean(dis_nhb_nhw, w = metro_nhb))

h_seg_trends <- h_seg %>%
  group_by(seg, type, year) %>%
  summarize(n = n(),
            mean = weighted.mean(dis_h_nhw, w = metro_h))

nha_seg_trends <- nha_seg %>%
  group_by(seg, type, year) %>%
  summarize(n = n(),
            mean = weighted.mean(dis_nha_nhw, w = metro_nha))

seg_trends <- bind_rows(nhb_seg_trends, h_seg_trends, nha_seg_trends)

seg_trends_gg <- ggplot(seg_trends, aes(x = year, y = mean, linetype = type,
                                        color = seg, shape = seg,
                       group = fct_cross(type, seg))) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "Average Dissimilarity Index\n", color = "", 
       shape = "", linetype = "Type")

seg_trends_gg

ggsave(filename = "./output/seg_trends.pdf", seg_trends_gg,
       width = 8, height = 6, dpi = 300)


#### Neighborhood change matrix ------------------------------------------------


ctpp <- ctpp %>%
  mutate(am_cat = case_when(
    
    #predominant compositions
    trt_shr_nhw_am >= .90 ~ "White",
    trt_shr_nhb_am >= .90 ~ "Black",
    trt_shr_nha_am >= .90 ~ "Asian/PI",
    trt_shr_h_am >= .90 ~ "Latinx",
    
    #shared compositions
    trt_shr_nhw_am < .90 & trt_shr_nhw_am >= .50 &
      trt_shr_nhb_am > .10 & trt_shr_nhb_am < .50 &
      trt_shr_h_am < .10 & trt_shr_nha_am < .10 & trt_shr_nho_am < .10 ~ "White-Black",
   
    trt_shr_nhw_am < .90 & trt_shr_nhw_am >= .50 &
      trt_shr_h_am > .10 & trt_shr_h_am < .50 &
      trt_shr_nhb_am < .10 & trt_shr_nha_am < .10 & trt_shr_nho_am < .10 ~ "White-Latinx",
    
    TRUE ~ "Multiethnic"
  )) %>%
  mutate(pm_cat = case_when(
    
    #predominant compositions
    trt_shr_nhw_pm >= .50 ~ "White",
    trt_shr_nhb_pm >= .50 ~ "Black",
    trt_shr_nha_pm >= .50 ~ "Asian/PI",
    trt_shr_h_pm >= .50 ~ "Latinx",
    TRUE ~ "Multiethnic"
  ))


ctpp %>%
  st_drop_geometry() %>%
  group_by(year, am_cat) %>%
  tally() %>%
  group_by(year) %>%
  mutate(shr = n/sum(n)) %>%
  print(n = Inf)

ctpp %>%
  st_drop_geometry() %>%
  group_by(year, am_cat) %>%
  summarize(nhw = mean(trt_shr_nhw_am, na.rm = TRUE),
            nhb = mean(trt_shr_nhb_am, na.rm = TRUE),
            h = mean(trt_shr_h_am, na.rm = TRUE),
            nha = mean(trt_shr_nha_am, na.rm = TRUE),
            nho = mean(trt_shr_nho_am, na.rm = TRUE)) %>%
  filter(am_cat %in% c("Multiethnic", "White"))





























