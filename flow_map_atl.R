
#dependencies
library(tidyverse)
library(sf)
library(ggeffects)

setwd("H:/res-pow-seg")

#load the data
load("./input/ctpp-neigh-chg-data.RData")

#read in 2012-2016 ACS data for descriptive analysis
acs_2012_2016 <- read_csv("./input/nhgis0192_csv/nhgis0192_ds225_20165_2016_tract.csv") %>%
  left_join(read_csv("./input/nhgis0192_csv/nhgis0192_ds226_20165_2016_tract.csv")) %>%
  select(GISJOIN, STATEA, COUNTYA, TRACTA, AF2LE001:AGL1M001) %>%
  mutate(trt_tot_pop = AF2LE001/10000,
         trt_shr_rec_mover = AF2WE003/AF2WE001,
         trt_shr_col_grad = (AF4OE022+AF4OE023+AF4OE024+AF4OE025)/AF4OE001,
         trt_shr_pov = (AF43E002+AF43E003)/AF43E001,
         trt_med_hh_inc = AF49E001/10000,
         trt_shr_emp = AF67E004/AF67E003,
         trt_shr_vac = AF7OE003/AF7OE001,
         trt_shr_rent_occ = AF7PE003/AF7PE001,
         trt_shr_sfh = AF8EE002/AF8EE001,
         trt_med_yr_blt = AF8IE001 - 1960,
         trt_med_gross_rent = AF89E001,
         trt_med_val = AF9LE001/10000)

#read in flow data for georgia tracts
ctpp_flow <- read_csv("./input/ctpp/ga_flow/ctpp_ga_flows_by_minority_status.csv", 
                      col_names = c("res_tract_str", "pow_tract_str", "total_est", "total_moe",
                                    "nhw_est", "nhw_poe", "oth_est", "oth_poe"),
                      skip = 6)

tract_2010 <- read_csv("./input/nhgis0189_csv/nhgis0189_ds176_20105_2010_tract.csv") %>%
  filter(STATE == "Georgia") %>%
  select(GISJOIN, STATE, COUNTYLSAD = COUNTY, STATEFP = STATEA, 
         COUNTYFP = COUNTYA, TRACTFP = TRACTA) %>%
  mutate(geoid = paste0(STATEFP, COUNTYFP, TRACTFP)) %>%
  rename_at(vars(everything(), -GISJOIN), tolower) 

tract_2010_shp <- st_read("./input/geo/US_tract_2010.shp")


res_tract_2010 <- tract_2010 %>%
  rename_all(~ paste0("res_", .))

pow_tract_2010 <- tract_2010 %>%
  rename_all(~ paste0("pow_", .)) 


#### Make the categories -------------------------------------------------------

ctpp <- ctpp %>%
  mutate(am_cat = case_when(
    
    #predominant compositions
    trt_shr_nhw_am >= .75 ~ "Predominantly White",
    trt_shr_nhb_am >= .75 ~ "Predominantly Black",
    trt_shr_nha_am >= .75 ~ "Predominantly Asian",
    trt_shr_h_am >= .75 ~ "Predominantly Latinx",
    
    #shared compositions
    trt_shr_nhw_am < .75 & trt_shr_nhw_am >= .50 ~ "White-Mixed",
    (trt_shr_nhb_am < .75 & trt_shr_nhb_am >= .50) |
      (trt_shr_h_am < .75 & trt_shr_h_am >= .50) |
      (trt_shr_nha_am < .75 & trt_shr_nha_am >= .50)  ~ "Non-White-Mixed",
    
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
    (trt_shr_nhb_pm < .75 & trt_shr_nhb_pm >= .50) |
      (trt_shr_h_pm < .75 & trt_shr_h_pm >= .50) |
      (trt_shr_nha_pm < .75 & trt_shr_nha_pm >= .50)  ~ "Non-White-Mixed",
    
    #the rest
    TRUE ~ "Multiethnic"
  ))

#### Map types in ATL to scope out areas ---------------------------------------

ctpp_atl <- ctpp %>% filter(cbsafp == "12060") %>% st_as_sf()

ggplot(ctpp_atl, aes(fill = pm_cat)) +
  facet_wrap(~ year) +
  geom_sf(color = NA) +
  scale_fill_brewer(palette = "Set2")

#### 


ga_tract_centroids <- tract_2010_shp %>%
  filter(STATEFP10 %in% ctpp_atl$statefp) %>%
  select(GISJOIN, geometry) %>%
  st_centroid() %>%
  rowwise() %>%
  mutate(lng = geometry[[1]][1],
         lat = geometry[[1]][2]) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  select(GISJOIN, lat, lng)

res_ga_tract_centroids <- ga_tract_centroids %>%
  rename_all(~ paste0("res_", .))

pow_ga_tract_centroids <- ga_tract_centroids %>%
  rename_all(~ paste0("pow_", .))

ctpp_flow <- ctpp_flow %>%
  mutate(res_tract_str = iconv(res_tract_str, "latin1", "UTF-8"),
         res_trt_name = str_split_fixed(res_tract_str, ",", n = 2)[,1],
         res_tractfp = str_remove_all(res_trt_name, "Census Tract "),
         res_tractfp_pre = str_split_fixed(res_tractfp, "\\.", n = 2)[,1],
         res_tractfp_post = str_split_fixed(res_tractfp, "\\.", n = 2)[,2],
         res_tractfp_post = ifelse(is.na(res_tractfp_post) | res_tractfp_post == "", "00", res_tractfp_post),
         res_tractfp = paste0(str_pad(res_tractfp_pre, 4, "left", "0"),
                          str_pad(res_tractfp_post, 2, "left", "0")),
         res_trt_county_state = str_split_fixed(res_tract_str, ", ", n = 2)[,2],
         res_countylsad = str_split_fixed(res_trt_county_state, ", ", n = 2)[,1],
         res_countylsad = str_trim(res_countylsad),
         res_county = str_remove_all(res_countylsad, " County| Parish| Census Area| Municipality| City and Borough| Borough| city"),
         res_county = str_trim(res_county),
         res_state = str_split_fixed(res_trt_county_state, ", ", n = 2)[,2]) %>%
  mutate(pow_tract_str = iconv(pow_tract_str, "latin1", "UTF-8"),
         pow_trt_name = str_split_fixed(pow_tract_str, ",", n = 2)[,1],
         pow_tractfp = str_remove_all(pow_trt_name, "Census Tract "),
         pow_tractfp_pre = str_split_fixed(pow_tractfp, "\\.", n = 2)[,1],
         pow_tractfp_post = str_split_fixed(pow_tractfp, "\\.", n = 2)[,2],
         pow_tractfp_post = ifelse(is.na(pow_tractfp_post) | pow_tractfp_post == "", "00", pow_tractfp_post),
         pow_tractfp = paste0(str_pad(pow_tractfp_pre, 4, "left", "0"),
                              str_pad(pow_tractfp_post, 2, "left", "0")),
         pow_trt_county_state = str_split_fixed(pow_tract_str, ", ", n = 2)[,2],
         pow_countylsad = str_split_fixed(pow_trt_county_state, ", ", n = 2)[,1],
         pow_countylsad = str_trim(pow_countylsad),
         pow_county = str_remove_all(pow_countylsad, " County| Parish| Census Area| Municipality| City and Borough| Borough| city"),
         pow_county = str_trim(pow_county),
         pow_state = str_split_fixed(pow_trt_county_state, ", ", n = 2)[,2]) %>%
  left_join(res_tract_2010) %>%
  left_join(pow_tract_2010) %>%
  left_join(res_ga_tract_centroids) %>%
  left_join(pow_ga_tract_centroids) %>%
  filter(res_countyfp %in% ctpp_atl$countyfp)

atl_tracts <- tract_2010_shp %>%
  filter(STATEFP10 %in% ctpp_atl$statefp, 
         COUNTYFP10 %in% ctpp_atl$countyfp) 


flow_tbl <- ctpp_atl %>%
  filter(statefp == "13", year == "2012-2016 ACS") %>%
  select(res_GISJOIN = GISJOIN, year, pm_cat, am_cat) %>%
  st_drop_geometry()

ctpp_flow <- left_join(ctpp_flow, flow_tbl)

ggplot(atl_tracts) +
  #facet_grid(~ year) +
  geom_sf(color = "black")

atl_flow <- ctpp_flow %>%
  filter(res_statefp %in% ctpp_atl$statefp, res_countyfp %in% ctpp_atl$countyfp,
         pow_statefp %in% ctpp_atl$statefp, pow_countyfp %in% ctpp_atl$countyfp)

atl_flow$pm_cat <- factor(atl_flow$pm_cat)
atl_flow$pm_cat <- factor(atl_flow$pm_cat, levels = levels(atl_flow$pm_cat)[c(7,1,6,3,2,5,4)])


atl_border <- ctpp_atl %>%
  filter(year == "2012-2016 ACS", place_name == "Atlanta") %>%
  summarize()

pred_wht_atl_flow <- atl_flow %>%
  filter(pow_geoid %in% c("13057090701")) %>%
  select(res_GISJOIN, res_lng, res_lat, pow_GISJOIN, pow_lng, pow_lat, nhw_est, oth_est) %>%
  pivot_longer(cols = -c(res_GISJOIN:pow_lat)) %>%
  mutate(est_type = case_when(name == "nhw_est" ~ "Workers who are Non-Hispanic Whites",
                              name == "oth_est" ~ "Workers who are not Non-Hispanic Whites")) %>%
  mutate(cat = "Predominantly White (PM)")

pred_wht_atl_flow_gg <- ggplot() +
  facet_grid(~ est_type) +
  geom_sf(data = ctpp_atl %>% filter(year == "2012-2016 ACS"), 
          aes(fill = pm_cat), lwd = .01, color = NA) +
  geom_sf(data = atl_border, fill = NA, color = "black", linetype = 3, lwd = .35) +
  geom_segment(data = pred_wht_atl_flow %>% filter(value >= 1), 
               aes(x = res_lng, xend = pow_lng, y = res_lat, yend = pow_lat,
                   alpha = value), 
               lineend = "round") +
  scale_alpha_continuous(limits = c(0, 200), breaks = c(25, 50, 75, 100)) +
  scale_fill_brewer(palette = "Pastel2") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical") +
  guides(fill = guide_legend(nrow = 3)) +
  labs(x = "", y = "", fill = "Nighttime Composition", alpha = "Total Workers",
       subtitle = "Predominantly White (PM) / Predominantly White (AM) Tract")

ggsave(filename = "./output/pred_wht_atl_flow.pdf", pred_wht_atl_flow_gg,
       width = 8, height = 6, dpi = 300)



pred_blk_atl_flow <- atl_flow %>%
  filter(pow_geoid %in% c("13121007300")) %>%
  select(res_GISJOIN, res_lng, res_lat, pow_GISJOIN, pow_lng, pow_lat, nhw_est, oth_est) %>%
  pivot_longer(cols = -c(res_GISJOIN:pow_lat)) %>%
  mutate(est_type = case_when(name == "nhw_est" ~ "Workers who are Non-Hispanic Whites",
                              name == "oth_est" ~ "Workers who are not Non-Hispanic Whites"))%>%
  mutate(cat = "Predominantly Black (PM)")

pred_blk_atl_flow_gg <- ggplot() +
  facet_grid(~ est_type) +
  geom_sf(data = ctpp_atl %>% filter(year == "2012-2016 ACS"), 
          aes(fill = pm_cat), lwd = .01, color = NA) +
  geom_sf(data = atl_border, fill = NA, color = "black", linetype = 3, lwd = .35) +
  geom_segment(data = pred_blk_atl_flow %>% filter(value >= 1), 
               aes(x = res_lng, xend = pow_lng, y = res_lat, yend = pow_lat,
                   alpha = value), 
               lineend = "round") +
  scale_alpha_continuous(limits = c(0, 200), breaks = c(25, 50, 75, 100)) +
  scale_fill_brewer(palette = "Pastel2") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical") +
  guides(fill = guide_legend(nrow = 3)) +
  labs(x = "", y = "", fill = "Nighttime Composition", alpha = "Total Workers",
       subtitle = "Predominantly Black (PM) / Non-White-Mixed (AM) Tract")

ggsave(filename = "./output/pred_blk_atl_flow.pdf", pred_blk_atl_flow_gg,
       width = 9, height = 6, dpi = 300)


mult_atl_flow <- atl_flow %>%
  filter(pow_geoid == "13121000600") %>%
  select(res_GISJOIN, res_lng, res_lat, pow_GISJOIN, pow_lng, pow_lat, nhw_est, oth_est) %>%
  pivot_longer(cols = -c(res_GISJOIN:pow_lat)) %>%
  mutate(est_type = case_when(name == "nhw_est" ~ "Workers who are Non-Hispanic Whites",
                              name == "oth_est" ~ "Workers who are not Non-Hispanic Whites")) %>%
  mutate(cat = "Multiethnic (PM)")

mult_atl_flow_gg <- ggplot() +
  facet_grid(est_type ~ .) +
  geom_sf(data = ctpp_atl %>% filter(year == "2012-2016 ACS"), 
          aes(fill = pm_cat), lwd = .01, color = NA) +
  geom_sf(data = atl_border, fill = NA, color = "black", linetype = 3, lwd = .35) +
  geom_segment(data = mult_atl_flow %>% filter(value >= 1), 
               aes(x = res_lng, xend = pow_lng, y = res_lat, yend = pow_lat,
                   alpha = value), 
               lineend = "round") +
  scale_alpha_continuous(limits = c(0, 200), breaks = c(25, 50, 75, 100)) +
  scale_fill_brewer(palette = "Pastel2") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.text.x = element_text(),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 3)) +
  labs(x = "", y = "", fill = "Nighttime Composition", alpha = "Total Workers",
       subtitle = "Multiethnic (PM) / Multiethnic (PM) Tract")

ggsave(filename = "./output/mult_atl_flow.pdf", mult_atl_flow_gg,
       width = 9, height = 6, dpi = 300)


library(patchwork)

small_mult_atl_flow_gg <- 
  (pred_wht_atl_flow_gg / pred_blk_atl_flow_gg / mult_atl_flow_gg) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "bottom",
        legend.box = "vertical")

ggsave(filename = "./output/small_mult_atl_flow.pdf", small_mult_atl_flow_gg,
       width = 7, height = 14, dpi = 300)





plot_atl_flow <- bind_rows(pred_wht_atl_flow, pred_blk_atl_flow, mult_atl_flow)

plot_atl_flow$cat <- factor(plot_atl_flow$cat)
plot_atl_flow$cat <- factor(plot_atl_flow$cat, levels = levels(plot_atl_flow$cat)[c(2,1,3)])

small_mult_atl_flow_gg <- ggplot() +
  facet_grid(est_type ~ cat) +
  geom_sf(data = ctpp_atl %>% filter(year == "2012-2016 ACS"), 
          aes(fill = pm_cat), lwd = .01, color = NA) +
  geom_sf(data = atl_border, fill = NA, color = "black", linetype = 3, lwd = .35) +
  geom_segment(data = plot_atl_flow %>% filter(value >= 1), 
               aes(x = res_lng, xend = pow_lng, y = res_lat, yend = pow_lat,
                   alpha = value), 
               lineend = "round") +
  scale_alpha_continuous(limits = c(0, 200), breaks = c(25, 50, 75, 100)) +
  scale_fill_brewer(palette = "Pastel2") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.text.x = element_text(),
        legend.position = "bottom",
        legend.box = "vertical") +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = "", y = "", fill = "Nighttime Composition", alpha = "Total Workers")

small_mult_atl_flow_gg

ggsave(filename = "./output/small_mult_atl_flow.pdf", small_mult_atl_flow_gg,
       width = 11, height = 8, dpi = 300)


























plot_flow <- atl_flow %>%
  select(am_cat, res_GISJOIN, res_lng, res_lat, pow_GISJOIN, pow_lng, pow_lat, nhw_est, oth_est) %>%
  pivot_longer(cols = -c(am_cat, res_GISJOIN:pow_lat)) %>%
  mutate(est_type = case_when(name == "nhw_est" ~ "Workers who are Non-Hispanic Whites",
                              name == "oth_est" ~ "Workers who are not Non-Hispanic Whites"))



ggplot() +
  geom_sf(data = atl_tracts, color = NA, fill = "grey70") +
  geom_sf(data = ctpp_atl %>% filter(year == "2012-2016 ACS"), 
          aes(fill = am_cat), color = NA) +
  facet_wrap(~ pm_cat) +
  geom_segment(data = plot_flow %>% filter(value > 10), 
               aes(x = res_lng, xend = pow_lng, y = res_lat, yend = pow_lat,
                   alpha = value, color = est_type)) +
  scale_alpha_continuous(range = c(0.1, 0.5)) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))
