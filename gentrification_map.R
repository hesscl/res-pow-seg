
#dependencies
library(tidyverse)
library(sf)
library(ggeffects)

setwd("H:/res-pow-seg")

#load the data
load("./input/ctpp-neigh-chg-data.RData")

ctpp <- ctpp %>%
  mutate(am_cat = case_when(
    
    #predominant compositions
    trt_shr_nhw_am >= .75 ~ "Predominantly White",
    trt_shr_nhb_am >= .75 ~ "Predominantly Black",
    trt_shr_nha_am >= .75 ~ "Predominantly Asian",
    trt_shr_h_am >= .75 ~ "Predominantly Latinx",
    
    #shared compositions
    trt_shr_nhw_am < .75 & trt_shr_nhw_am >= .50 ~ "White-Mixed",
    trt_shr_nhb_am < .75 & trt_shr_nhb_am >= .50  ~ "Black-Mixed",
    trt_shr_h_am < .75 & trt_shr_h_am >= .50  ~ "Latinx-Mixed",
    trt_shr_nha_am < .75 & trt_shr_nha_am >= .50  ~ "Asian-Mixed",
    
    #the rest
    TRUE ~ "Multiethnic"
  )) %>%
  
  mutate(pm_cat = case_when(
    
    #predominant compositions
    trt_shr_nhw_pm >= .75 ~ "Predominantly White",
    trt_shr_nhb_pm >= .75 ~ "Predominantly Black",
    trt_shr_nha_pm >= .75 ~ "Predominantly Asian",
    trt_shr_h_pm >= .75 ~ "Predominantly Latinx",
    
    #shared compositions
    trt_shr_nhw_pm < .75 & trt_shr_nhw_pm >= .50 ~ "White-Mixed",
    trt_shr_nhb_pm < .75 & trt_shr_nhb_pm >= .50  ~ "Black-Mixed",
    trt_shr_h_pm < .75 & trt_shr_h_pm >= .50  ~ "Latinx-Mixed",
    trt_shr_nha_pm < .75 & trt_shr_nha_pm >= .50  ~ "Asian-Mixed",
    
    #the rest
    TRUE ~ "Multiethnic"
  )) %>%
  mutate(pm_am = fct_cross(pm_cat, am_cat, sep = ", "),
         pm_am_res_blk = ifelse(str_detect(pm_am, "Predominantly Black"), as.character(pm_am), "Other"),
         pm_am_res_blk = as.factor(pm_am_res_blk),
         pm_am_res_lat = ifelse(str_detect(pm_am, "Predominantly Latinx"), as.character(pm_am), "Other"),
         pm_am_res_lat = as.factor(pm_am_res_lat),
         pm_am_res_mult = ifelse(str_detect(pm_am, "Multiethnic"), as.character(pm_am), "Other"),
         pm_am_res_mult = as.factor(pm_am_res_mult),
         predom_am = ifelse(str_detect(am_cat, "Predominantly"), "Segregated Daytime", "Not Segregated Daytime"),
         predom_pm = ifelse(str_detect(pm_cat, "Predominantly"), "Segregated Nighttime", "Not Segregated Nighttime"),
         predom_pm_am = fct_cross(predom_pm, predom_am, sep = ", "),
         mult_am = ifelse(str_detect(am_cat, "Multiethnic"), "Multiethnic Daytime", "Not Multiethnic Daytime"),
         mult_pm = ifelse(str_detect(pm_cat, "Multiethnic"), "Multiethnic Nighttime", "Not Multiethnic Nighttime"),
         mult_pm_am = fct_cross(mult_pm, mult_am, sep = ", "))
         

largest_50 <- ctpp %>%
  st_drop_geometry %>%
  filter(year == "2012-2016") %>%
  group_by(cbsafp) %>%
  summarize(tot_pop = sum(trt_tot_pop, na.rm = TRUE)) %>%
  top_n(50, tot_pop) %>%
  pull(cbsafp)


make_intraday_maps <- function(metro){
  
  map_data <- ctpp %>% filter(cbsafp == metro, year %in% c("2000", "2012-2016"))
  short_metro_name <- map_data %>% 
    rowwise() %>%
    mutate(metro_name = str_split_fixed(metro_name, "-|\\,", n = 2)[, 1],
           metro_name = str_remove_all(metro_name, "\\."),
           metro_name = str_replace_all(metro_name, " |/", "_")) %>% 
    pull(metro_name) %>% 
    unique 
  
  prin_city <- map_data %>%
    filter(prin_city == "Y") %>%
    summarize()
  
  largest_place <- map_data %>%
    filter(prin_city == "Y") %>%
    group_by(place_name) %>%
    summarize(tot_pop = sum(trt_tot_pop, na.rm = TRUE)) %>%
    top_n(1, tot_pop) %>%
    pull(place_name) %>%
    unique()
  
  map_data_largest_place <- map_data %>%
    filter(place_name == largest_place)
  
  seg_gg <- ggplot(map_data, aes(fill = predom_pm_am)) +
    facet_grid(~ year) +
    geom_sf(color = NA) +
    geom_sf(data = prin_city, fill = NA) +
    scale_fill_manual(values = rev(RColorBrewer::brewer.pal(7, "RdBu")[c(2, 3, 5, 6)])) +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 2)) +
    labs(fill = "", title = paste0("Intraday Change for Segregated Compositions (i.e. >= 75% one group) in ", 
                                   short_metro_name, " CBSA"))
  
  ggsave(filename = paste0("./output/seg_choro/", tolower(short_metro_name), "_predom_pm_am_choro.pdf"), 
                           seg_gg, width = 10, height = 8, dpi = 300)
  
  mult_gg <- ggplot(map_data, aes(fill = mult_pm_am)) +
    facet_grid(~ year) +
    geom_sf(color = NA) +
    geom_sf(data = prin_city, fill = NA) +
    scale_fill_manual(values = rev(RColorBrewer::brewer.pal(7, "PuOr")[c(2, 3, 5, 6)])) +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 2)) +
    labs(fill = "", title = paste0("Intraday Change for Multiethnic Compositions (i.e. < 50% all groups) in ", 
                                   short_metro_name, " CBSA"))
  
  ggsave(filename = paste0("./output/mult_choro/", tolower(short_metro_name), "_multeth_pm_am_choro.pdf"), 
         mult_gg, width = 10, height = 8, dpi = 300)
  
  seg_largest_place_gg <- ggplot(map_data_largest_place, aes(fill = predom_pm_am)) +
    facet_grid(~ year) +
    geom_sf(color = NA) +
    scale_fill_manual(values = rev(RColorBrewer::brewer.pal(7, "RdBu")[c(2, 3, 5, 6)])) +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 2)) +
    labs(fill = "", title = paste0("Intraday Change for Segregated Compositions (i.e. >= 75% one group) in ", 
                                   largest_place))
  
  ggsave(filename = paste0("./output/seg_choro_largest_place/", tolower(short_metro_name), "_predom_pm_am_largest_place_choro.pdf"), 
         seg_largest_place_gg, width = 10, height = 8, dpi = 300)
  
  mult_largest_place_gg <- ggplot(map_data_largest_place, aes(fill = mult_pm_am)) +
    facet_grid(~ year) +
    geom_sf(color = NA) +
    scale_fill_manual(values = rev(RColorBrewer::brewer.pal(7, "PuOr")[c(2, 3, 5, 6)])) +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 2)) +
    labs(fill = "", title = paste0("Intraday Change for Multiethnic Compositions (i.e. < 50% all groups) in ", 
                                  largest_place))
  
  ggsave(filename = paste0("./output/mult_choro_largest_place/", tolower(short_metro_name), "_multeth_pm_am_largest_place_choro.pdf"), 
         mult_largest_place_gg, width = 10, height = 8, dpi = 300)
  
}

map(largest_50, make_intraday_maps)
       



