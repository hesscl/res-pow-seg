
#dependencies
library(tidyverse)
library(sf)
library(ggeffects)
library(stargazer)
library(sandwich)
library(nnet)

setwd("H:/res-pow-seg")

#load the data
load("./input/ctpp-neigh-chg-data.RData")

tract_2010_shp <- st_read("./input/geo/US_tract_2010.shp")

#load PUMS CBSA estimates
pums_cbsa_est <- read_csv("./input/pums/pums_metro_est.csv")

#load tract data to use for multinomial models
acs_2012_2016 <- read_csv("./input/nhgis0197_csv/nhgis0197_ds225_20165_tract.csv")


#### Traditional categorization method -----------------------------------------

ctpp <- ctpp %>%
  mutate(am_cat = case_when(
    
    #predominant compositions
    trt_shr_nhw_am >= .75 ~ "Predominantly White",
    trt_shr_nhb_am >= .75 ~ "Predominantly Black",
    trt_shr_nha_am >= .75 ~ "Predominantly Asian",
    trt_shr_h_am >= .75 ~ "Predominantly Latinx",
    
    #shared compositions
    trt_shr_nhw_am < .75 & trt_shr_nhw_am >= .50 ~ "White-Mixed",
    # trt_shr_nhb_am < .75 & trt_shr_nhb_am >= .50 ~ "Black-Mixed",
    # trt_shr_h_am < .75 & trt_shr_h_am >= .50  ~ "Latinx-Mixed",
    # trt_shr_nha_am < .75 & trt_shr_nha_am >= .50  ~ "Asian-Mixed",
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
    # trt_shr_nhb_am < .75 & trt_shr_nhb_am >= .50 ~ "Black-Mixed",
    # trt_shr_h_am < .75 & trt_shr_h_am >= .50  ~ "Latinx-Mixed",
    # trt_shr_nha_am < .75 & trt_shr_nha_am >= .50  ~ "Asian-Mixed",
    (trt_shr_nhb_pm < .75 & trt_shr_nhb_pm >= .50) |
      (trt_shr_h_pm < .75 & trt_shr_h_pm >= .50) |
      (trt_shr_nha_pm < .75 & trt_shr_nha_pm >= .50)  ~ "Non-White-Mixed",
    
    #the rest
    TRUE ~ "Multiethnic"
  ))


####  Diagnostics --------------------------------------------------------------

#plot non-hispanic black counts for chicago metro area
test_chi <- ctpp %>%
  filter(cbsafp10 == "16980", year == "2012-2016 ACS") %>%
  mutate(chg_nhb_rank = rank(chg_nhb),
         chg_h_rank = rank(chg_h),
         chg_nha_rank = rank(chg_nha)) %>%
  ungroup()

test_chi %>% 
  st_drop_geometry() %>%
  filter(chg_nhb_rank %in% 1:10) %>% 
  select(chg_nhb, pm_cat, am_cat, place_name)

test_chi %>% 
  st_drop_geometry %>%
  filter(chg_h_rank %in% 1:10) %>% 
  select(chg_h, pm_cat, am_cat, place_name)

test_chi %>% 
  st_drop_geometry %>%
  filter(chg_nha_rank %in% 1:10) %>% 
  select(chg_nha, pm_cat, am_cat, place_name)

ggplot(test_chi %>% filter(year == "2012-2016 ACS"), aes(fill = chg_nhb)) +
  facet_wrap(~ year) +
  geom_sf(color = NA) +
  scale_fill_gradient2() +
  geom_sf(data = test_chi %>% filter(chg_nhb_rank %in% 1:10), 
          color = "red", lwd = 1) 
ggsave(filename = "./output/diag_maps/chi_top_10_chg_nhb.pdf",
       width = 8, height = 6, dpi = 300)

ggplot(test_chi %>% filter(year == "2012-2016 ACS"), aes(fill = chg_h)) +
  facet_wrap(~ year) +
  geom_sf(color = NA) +
  scale_fill_gradient2() +
  geom_sf(data = test_chi %>% filter(chg_h_rank %in% 1:10), 
          color = "red", lwd = 1) 
ggsave(filename = "./output/diag_maps/chi_top_10_chg_h.pdf",
       width = 8, height = 6, dpi = 300)

ggplot(test_chi %>% filter(year == "2012-2016 ACS"), aes(fill = chg_nha)) +
  facet_wrap(~ year) +
  geom_sf(color = NA) +
  scale_fill_gradient2() +
  geom_sf(data = test_chi %>% filter(chg_nha_rank %in% 1:10), 
          color = "red", lwd = 1) 
ggsave(filename = "./output/diag_maps/chi_top_10_chg_nha.pdf",
       width = 8, height = 6, dpi = 300)


test_sea <- ctpp %>%
  filter(cbsafp10 == "42660", year == "2012-2016 ACS") %>%
  mutate(chg_nhb_rank = rank(chg_nhb),
         chg_h_rank = rank(chg_h),
         chg_nha_rank = rank(chg_nha)) %>%
  ungroup()

test_sea %>% 
  st_drop_geometry() %>%
  filter(chg_nhb_rank %in% 1:10) %>% 
  select(chg_nhb, pm_cat, am_cat, place_name)

test_sea %>% 
  st_drop_geometry %>%
  filter(chg_h_rank %in% 1:10) %>% 
  select(chg_h, pm_cat, am_cat, place_name)

test_sea %>% 
  st_drop_geometry %>%
  filter(chg_nha_rank %in% 1:10) %>% 
  select(chg_nha, pm_cat, am_cat, place_name)

ggplot(test_sea %>% filter(year == "2012-2016 ACS"), aes(fill = chg_nhb)) +
  facet_wrap(~ year) +
  geom_sf(color = NA) +
  scale_fill_gradient2() +
  geom_sf(data = test_sea %>% filter(chg_nhb_rank %in% 1:10), 
          color = "red", lwd = 2) 
ggsave(filename = "./output/diag_maps/sea_top_10_chg_nhb.pdf",
       width = 8, height = 6, dpi = 300)

ggplot(test_sea %>% filter(year == "2012-2016 ACS"), aes(fill = chg_h)) +
  facet_wrap(~ year) +
  geom_sf(color = NA) +
  scale_fill_gradient2() +
  geom_sf(data = test_sea %>% filter(chg_h_rank %in% 1:10), 
          color = "red", lwd = 2) 
ggsave(filename = "./output/diag_maps/sea_top_10_chg_h.pdf",
       width = 8, height = 6, dpi = 300)

ggplot(test_sea %>% filter(year == "2012-2016 ACS"), aes(fill = chg_nha)) +
  facet_wrap(~ year) +
  geom_sf(color = NA) +
  scale_fill_gradient2() +
  geom_sf(data = test_sea %>% filter(chg_nha_rank %in% 1:10), 
          color = "red", lwd = 2) 
ggsave(filename = "./output/diag_maps/sea_top_10_chg_nha.pdf",
       width = 8, height = 6, dpi = 300)


test_phl <- ctpp %>%
  filter(cbsafp10 == "37980", year == "2012-2016 ACS") %>%
  mutate(chg_nhb_rank = rank(chg_nhb),
         chg_h_rank = rank(chg_h),
         chg_nha_rank = rank(chg_nha)) %>%
  ungroup()

test_phl %>% 
  st_drop_geometry() %>%
  filter(chg_nhb_rank %in% 1:10) %>% 
  select(chg_nhb, pm_cat, am_cat, place_name)

test_phl %>% 
  st_drop_geometry %>%
  filter(chg_h_rank %in% 1:10) %>% 
  select(chg_h, pm_cat, am_cat, place_name)

test_phl %>% 
  st_drop_geometry %>%
  filter(chg_nha_rank %in% 1:10) %>% 
  select(chg_nha, pm_cat, am_cat, place_name)

ggplot(test_phl %>% filter(year == "2012-2016 ACS"), aes(fill = chg_nhb)) +
  facet_wrap(~ year) +
  geom_sf(color = NA) +
  scale_fill_gradient2() +
  geom_sf(data = test_phl %>% filter(chg_nhb_rank %in% 1:10), 
          color = "red", lwd = 2) 
ggsave(filename = "./output/diag_maps/phl_top_10_chg_nhb.pdf",
       width = 8, height = 6, dpi = 300)

ggplot(test_phl %>% filter(year == "2012-2016 ACS"), aes(fill = chg_h)) +
  facet_wrap(~ year) +
  geom_sf(color = NA) +
  scale_fill_gradient2() +
  geom_sf(data = test_phl %>% filter(chg_h_rank %in% 1:10), 
          color = "red", lwd = 2) 
ggsave(filename = "./output/diag_maps/phl_top_10_chg_h.pdf",
       width = 8, height = 6, dpi = 300)

ggplot(test_phl %>% filter(year == "2012-2016 ACS"), aes(fill = chg_nha)) +
  facet_wrap(~ year) +
  geom_sf(color = NA) +
  scale_fill_gradient2() +
  geom_sf(data = test_phl %>% filter(chg_nha_rank %in% 1:10), 
          color = "red", lwd = 2) 
ggsave(filename = "./output/diag_maps/phl_top_10_chg_nha.pdf",
       width = 8, height = 6, dpi = 300)

# 
# #plot non-hispanic white % for Seattle metro area
# test_sea <- ctpp %>% filter(cbsafp10 == "42660")
# 
# ggplot(test_sea, aes(fill = res_nhw_est/res_total_est)) + 
#   facet_wrap(~ year) + 
#   geom_sf(color = NA)


#### Filter tract and metro set ------------------------------------------------

ctpp_geo <- ctpp %>%
  select(geoid, year, geometry)

#remove cases without any people
ctpp <- ctpp %>%
  st_drop_geometry() %>%
  filter(res_total_est > 0, pow_total_est > 0) %>%
  mutate(fct_year = factor(year),
         prin_city = ifelse(is.na(prin_city) | prin_city == "N", FALSE, TRUE))

#identify metros with 3k of each race/eth group
ctpp_metros <- ctpp %>%
  filter(year == "2000") %>%
  group_by(cbsafp10) %>%
  summarize(nhb = sum(res_nhb_est, na.rm = TRUE) >= 3000,
            nhw = sum(res_nhw_est, na.rm = TRUE) >= 3000,
            h = sum(res_h_est, na.rm = TRUE) >= 3000,
            nha = sum(res_nha_est, na.rm = TRUE) >= 3000) %>%
  mutate_at(vars(everything(), -cbsafp10), ~ ifelse(., cbsafp10, NA)) %>%
  select(-cbsafp10)

ctpp <- ctpp %>%
  filter(!is.na(trt_shr_nhw_am), !is.na(trt_shr_nhb_am), !is.na(trt_shr_nha_am), !is.na(trt_shr_h_am),
         cbsafp10 %in% ctpp_metros$nhb, cbsafp10 %in% ctpp_metros$nhw, 
         cbsafp10 %in% ctpp_metros$h, cbsafp10 %in% ctpp_metros$nha) 


#### Neighborhood change matrix ------------------------------------------------

chg_grid <- expand_grid(pm_cat = unique(ctpp$pm_cat),
                        am_cat = unique(ctpp$am_cat),
                        year = unique(ctpp$year))

chg_matrix <- ctpp %>%
  group_by(year, pm_cat, am_cat) %>% 
  tally %>%
  mutate(shr = n/sum(n)) %>%
  right_join(chg_grid) %>%
  mutate(n = ifelse(is.na(n), 0, n),
         shr = ifelse(is.na(shr), 0, shr)) %>%
  mutate(pm_cat = fct_reorder(pm_cat, n))
  

# chg_matrix$pm_cat <- factor(chg_matrix$pm_cat)
# chg_matrix$pm_cat <- factor(chg_matrix$pm_cat, levels = rev(levels(chg_matrix$pm_cat)[c(4, 5, 3, 2, 6, 1)]))
# 
chg_matrix$am_cat <- factor(chg_matrix$am_cat)
chg_matrix$am_cat <- factor(chg_matrix$am_cat, levels = levels(chg_matrix$pm_cat))

chg_matrix_gg <- ggplot(chg_matrix %>% filter(year %in% c("2000", "2012-2016 ACS")), 
       aes(y = pm_cat, x = am_cat, fill = shr, 
           label = paste0(round(shr, 2) * 100, "%"))) +
  facet_grid(~ year) +
  coord_equal() +
  geom_tile(color = "Black") +
  geom_label(color = "Black", fill = "white", size = 3) +
  scale_fill_gradient(limits = c(0, 1), low = "white", high = "black") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.spacing = unit(.5, "in"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  guides(fill = "none") +
  labs(x = "\nDaytime Worker 16+ Composition", 
       y = "Nighttime Worker 16+ Composition\n")

chg_matrix_gg

ggsave(filename = "./output/neigh_transition_matrix.pdf", chg_matrix_gg,
       width = 10, height = 6, dpi = 300)

chg_grid_city_sub <- expand_grid(pm_cat = unique(ctpp$pm_cat),
                                 am_cat = unique(ctpp$am_cat),
                                 year = unique(ctpp$year),
                                 prin_city = c(TRUE, FALSE))

chg_matrix_city_sub <- ctpp %>%
  group_by(prin_city, year, pm_cat, am_cat) %>% 
  tally %>%
  mutate(shr = n/sum(n)) %>%
  right_join(chg_grid_city_sub) %>%
  mutate(n = ifelse(is.na(n), 0, n),
         shr = ifelse(is.na(shr), 0, shr)) %>%
  mutate(pm_cat = fct_reorder(pm_cat, n))

# chg_matrix$pm_cat <- factor(chg_matrix$pm_cat)
# chg_matrix$pm_cat <- factor(chg_matrix$pm_cat, levels = rev(levels(chg_matrix$pm_cat)[c(4, 5, 3, 2, 6, 1)]))
# 
chg_matrix_city_sub$am_cat <- factor(chg_matrix_city_sub$am_cat)
chg_matrix_city_sub$am_cat <- factor(chg_matrix_city_sub$am_cat, levels = levels(chg_matrix_city_sub$pm_cat))

chg_matrix_city_gg <- ggplot(chg_matrix_city_sub %>% filter(year %in% c("2000", "2012-2016 ACS"),
                                                                prin_city), 
                        aes(y = pm_cat, x = am_cat, fill = shr, 
                            label = paste0(round(shr, 2) * 100, "%"))) +
  facet_grid(~ year) +
  coord_equal() +
  geom_tile(color = "Black") +
  geom_label(color = "Black", fill = "white", size = 3) +
  scale_fill_gradient(limits = c(0, 1), low = "white", high = "black") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.spacing = unit(.5, "in"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  guides(fill = "none") +
  labs(x = "\nDaytime Worker 16+ Composition", 
       y = "Nighttime Worker 16+ Composition\n",
       title = "Urban Neighborhood Subsample")

chg_matrix_city_gg

ggsave(filename = "./output/neigh_transition_matrix_city.pdf", chg_matrix_city_gg,
       width = 10, height = 6, dpi = 300)


chg_matrix_sub_gg <- ggplot(chg_matrix_city_sub %>% filter(year %in% c("2000", "2012-2016 ACS"),
                                                        !prin_city), 
                             aes(y = pm_cat, x = am_cat, fill = shr, 
                                 label = paste0(round(shr, 2) * 100, "%"))) +
  facet_grid(~ year) +
  coord_equal() +
  geom_tile(color = "Black") +
  geom_label(color = "Black", fill = "white", size = 3) +
  scale_fill_gradient(limits = c(0, 1), low = "white", high = "black") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.spacing = unit(.5, "in"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  guides(fill = "none") +
  labs(x = "\nDaytime Worker 16+ Composition", 
       y = "Nighttime Worker 16+ Composition\n",
       title = "Suburban Neighborhood Subsample")

chg_matrix_sub_gg

ggsave(filename = "./output/neigh_transition_matrix_sub.pdf", chg_matrix_sub_gg,
       width = 10, height = 6, dpi = 300)


#### Mapping intraday change ---------------------------------------------------

# ggplot(ctpp %>% filter(cbsafp10 == "42660"), 
#        aes(fill = chg_nhb)) +
#   geom_sf(color = NA) +
#   scale_fill_gradient2()
# 
# ggplot(ctpp %>% filter(cbsafp10 == "42660"), 
#        aes(fill = chg_nhw)) +
#   geom_sf(color = NA) +
#   scale_fill_gradient2()
# 
# ggplot(ctpp %>% filter(cbsafp10 == "16980"), 
#        aes(fill = chg_nhw)) +
#   geom_sf(color = NA) +
#   scale_fill_gradient2()
# 

#### Predict intraday change just based on types -------------------------------

ctpp$city_sub <- ifelse(ctpp$prin_city, "City", "Suburb")

#start by defining a table with outcomes and labels as cols
intraday <- tibble(outcome = paste0("chg_", c("nhb", "nhw", "nha", "nho", "h")),
                   race_eth = c("Non-Latinx Black", "Non-Latinx White", "Non-Latinx Asian",
                                "Non-Latinx Other", "Latinx"))

#now fit the basic unweighted model to each outcome
intraday$ols_fit <- map(intraday$outcome, 
                       ~ lm(as.formula(paste0(.x, "~ pm_cat * fct_year")), ctpp))

#allow for different trends by city/sub location
intraday$ols_fit_city_sub <- map(intraday$outcome, 
                                 ~ lm(as.formula(paste0(.x, "~ pm_cat * fct_year * prin_city")), ctpp))

#now fit the basic weighted model to each outcome
intraday$wls_fit <- map(intraday$outcome, 
                        ~ lm(as.formula(paste0(.x, "~ pm_cat * fct_year")), ctpp,
                             weights = ctpp[[paste0("res_", .x, "_est")]]))

#now fit the basic model with metro FE to each outcome
intraday$fe_fit <- map(intraday$outcome, 
                    ~ lm(as.formula(paste0(.x, "~ pm_cat * fct_year + factor(cbsafp10)")), ctpp))

#now generate marginal means estimated over fixed effects
intraday$ols_margins <- map(intraday$outcome, 
                           ~ tibble(ggemmeans(lm(as.formula(paste0(.x, "~ pm_cat * fct_year")), ctpp), 
                                              terms = c("fct_year", "pm_cat"),
                                              vcov.fun = "vcovHC")))

intraday$ols_margins_city_sub <- map(intraday$outcome, 
                            ~ tibble(ggemmeans(lm(as.formula(paste0(.x, "~ pm_cat * fct_year * city_sub")), ctpp), 
                                               terms = c("fct_year", "pm_cat", "city_sub"),
                                               vcov.fun = "vcovHC")))

intraday$wls_margins <- map(intraday$outcome, 
                            ~ tibble(ggemmeans(lm(as.formula(paste0(.x, "~ pm_cat * fct_year + factor(cbsafp10)")), ctpp,
                                                  weights = ctpp[[paste0("res_total_est")]]), 
                                               terms = c("fct_year", "pm_cat"),
                                               vcov.fun = "vcovHC")))

intraday$fe_margins <- map(intraday$outcome, 
                        ~ tibble(ggemmeans(lm(as.formula(paste0(.x, "~ pm_cat * fct_year + factor(cbsafp10)")), ctpp), 
                                           terms = c("fct_year", "pm_cat"),
                                           vcov.fun = "vcovCL",
                                           vcov.args = list(cluster = "cbsafp10"))))
                             
#prep the data we'll use for plotting
intraday_preds <- intraday %>%
  select(race_eth, ols_margins) %>%
  unnest(ols_margins) %>%
  select(fct_year = x, pred = predicted, lower = conf.low, upper = conf.high, pm_cat = group, race_eth) 

intraday_preds_fe <- intraday %>%
  select(race_eth, fe_margins) %>%
  unnest(fe_margins) %>%
  select(fct_year = x, pred = predicted, lower = conf.low, upper = conf.high, pm_cat = group, race_eth) 

intraday_preds_wls <- intraday %>%
  select(race_eth, wls_margins) %>%
  unnest(wls_margins) %>%
  select(fct_year = x, pred = predicted, lower = conf.low, upper = conf.high, pm_cat = group, race_eth) 

intraday_preds <- intraday_preds %>%
  mutate(pm_cat = paste(pm_cat, "(PM)"))

#sort the pm composition categories as we want
intraday_preds$pm_cat <- factor(intraday_preds$pm_cat)
intraday_preds$pm_cat <- factor(intraday_preds$pm_cat, levels = levels(intraday_preds$pm_cat)[c(4,5,3,6,7,2,1)])

#sort the race/ethnicity categories as we want
intraday_preds$race_eth <- factor(intraday_preds$race_eth)
intraday_preds$race_eth <- factor(intraday_preds$race_eth, levels = levels(intraday_preds$race_eth)[c(3,1,2,4,5)])


#create the two plots
pred_predom_gg <- ggplot(intraday_preds %>% 
                          filter(pm_cat %in% c("Predominantly Black (PM)", "Predominantly Latinx (PM)", 
                                               "Predominantly Asian (PM)", "Predominantly White (PM)"),
                                 race_eth != "Non-Latinx Other"), 
                          aes(y = race_eth, x = pred, xmin = lower, xmax = upper, 
                              fill = fct_rev(fct_year))) +
  facet_grid(race_eth ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = position_dodge(width = 1), height  = .5) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(5, "Greys")[2:4]) +
  scale_x_continuous(limits = c(-.45, .45), labels = scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.spacing.x = unit(.2, "in"),
        strip.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "")

pred_non_predom_gg <- ggplot(intraday_preds %>% 
                              filter(!pm_cat %in% c("Predominantly Black (PM)", "Predominantly Latinx (PM)", 
                                                   "Predominantly Asian (PM)", "Predominantly White (PM)"),
                                     race_eth != "Non-Latinx Other"), 
                            aes(y = race_eth, x = pred, xmin = lower, xmax = upper, 
                                fill = fct_rev(fct_year))) +
  facet_grid(race_eth ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = position_dodge(width = 1), height  = .5) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(5, "Greys")[2:4]) +
  scale_x_continuous(limits = c(-.45, .45), labels = scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.spacing.x = unit(.2, "in"),
        strip.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "")

#save to disk
ggsave(filename = "./output/ols_intraday_chg_predom_comps.pdf", pred_predom_gg,
       width = 10, height = 6, dpi = 300)
ggsave(filename = "./output/ols_intraday_chg_non_predom_comps.pdf", pred_non_predom_gg,
       width = 8, height = 6, dpi = 300)

#assemble coefficient table
stargazer(intraday$ols_fit,
          se = map(intraday$ols_fit, ~ sqrt(diag(vcovHC(.)))),
          keep.stat = c("n"),
          style = "demography",
          title = "Linear Regressions of Intraday Change",
          model.names = FALSE, 
          model.numbers = FALSE,
          dep.var.labels.include = TRUE,
          float.env = "table",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          notes = c("Heteroskedasticity Consistent Standard Errors in Parentheses; + p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = FALSE,
          column.sep.width = "2pt",
          out = "./output/ols_intraday_coef_table.tex")


#### Explain variations in intraday change -------------------------------------

#prep the columns we are going to explain intraday trajectories with
acs_2012_2016 <- acs_2012_2016 %>% 
  mutate(trt_tot_pop = AF2LE001/10000,
         trt_shr_col_grad = (AF4OE022+AF4OE023+AF4OE024+AF4OE025)/AF4OE001,
         trt_shr_pov = (AF43E002+AF43E003)/AF43E001,
         trt_med_hh_inc = AF49E001/10000,
         trt_shr_emp = AF67E004/AF67E003,
         trt_shr_vac = AF7OE003/AF7OE001,
         trt_shr_rent_occ = AF7PE003/AF7PE001,
         trt_med_yr_blt = AF8IE001 - 1960,
         trt_shr_drive_alone = AF3BE003/AF3BE001,
         trt_shr_mgmt_biz = (AF7AE003 + AF7AE039)/AF7AE001,
         trt_shr_wht_collar = (AF7LE013+AF7LE014+AF7LE018+AF7LE028+
                                 AF7LE040+AF7LE041+AF7LE045+AF7LE055)/AF7LE001) %>%
  select(GISJOIN, starts_with("trt_"))

#join the 2012-2016 wave to the prepped ACS data
ctpp_2016 <- ctpp %>% 
  filter(year == "2012-2016 ACS") %>%
  select(-trt_tot_pop) %>%
  left_join(acs_2012_2016)

#flag whether big change occurred since modeling a 7 category outcome is too much
ctpp_2016 <- ctpp_2016 %>%
  ungroup() %>%
  mutate(pm_set = case_when(
    str_detect(pm_cat, "Predominantly Black|Predominantly Latinx|Predominantly Asian") ~ "Predominant Non-White",
    pm_cat == "Predominantly White" ~ "Predominant White",
    str_detect(pm_cat, "Mixed") ~ "Mixed",
    str_detect(pm_cat, "Multiethnic") ~ "Multiethnic",
    ),
    pm_set = relevel(as.factor(pm_set), ref = "Predominant White")) %>%
  mutate(am_set = case_when(
    str_detect(am_cat, "Predominantly Black|Predominantly Latinx|Predominantly Asian") ~ "Predominant Non-White",
    am_cat == "Predominantly White" ~ "Predominant White",
    str_detect(am_cat, "Mixed") ~ "Mixed",
    str_detect(am_cat, "Multiethnic") ~ "Multiethnic",
  ),
  am_set = relevel(as.factor(am_set), ref = "Mixed"))

#need this to be factor since ggeffects does not play nice with logical values
ctpp_2016$prin_city <- as.factor(ctpp_2016$prin_city)
ctpp_2016$dist_to_cbd <- ctpp_2016$dist_to_cbd/1000

#estimate multinomial logistic regression models for the intraday trajectories of 
#each pm category
multinom_nhb_model <- multinom(am_set ~ trt_shr_emp + trt_shr_pov + trt_shr_col_grad + trt_shr_wht_collar + 
                             trt_med_yr_blt + trt_shr_drive_alone + dist_to_cbd, 
                           data = ctpp_2016 %>% filter(pm_cat == "Predominantly Black"))
summary(multinom_nhb_model)

multinom_h_model <- multinom(am_set ~ trt_shr_emp + trt_shr_pov + trt_shr_col_grad + trt_shr_wht_collar + 
                           trt_med_yr_blt + trt_shr_drive_alone + dist_to_cbd, 
                         data = ctpp_2016 %>% filter(pm_cat == "Predominantly Latinx"))
summary(multinom_h_model)

multinom_nhw_model <- multinom(am_set ~ trt_shr_emp + trt_shr_pov + trt_shr_col_grad + trt_shr_wht_collar + 
                             trt_med_yr_blt + trt_shr_drive_alone + dist_to_cbd, 
                           data = ctpp_2016 %>% filter(pm_cat == "Predominantly White"))
summary(multinom_nhw_model)

multinom_nha_model <- multinom(am_set ~ trt_shr_emp + trt_shr_pov + trt_shr_col_grad + trt_shr_wht_collar + 
                             trt_med_yr_blt + trt_shr_drive_alone + dist_to_cbd, 
                           data = ctpp_2016 %>% filter(pm_cat == "Predominantly Asian"))
summary(multinom_nha_model)

multinom_mult_model <- multinom(am_set ~ trt_shr_emp + trt_shr_pov + trt_shr_col_grad + trt_shr_wht_collar + 
                             trt_med_yr_blt + trt_shr_drive_alone + dist_to_cbd, 
                           data = ctpp_2016 %>% filter(pm_cat == "Multiethnic"))
summary(multinom_mult_model)

multinom_w_mixed_model <- multinom(am_set ~ trt_shr_emp + trt_shr_pov + trt_shr_col_grad + trt_shr_wht_collar + 
                              trt_med_yr_blt + trt_shr_drive_alone + dist_to_cbd, 
                            data = ctpp_2016 %>% filter(pm_cat == "White-Mixed"))
summary(multinom_w_mixed_model)

multinom_nw_mixed_model <- multinom(am_set ~ trt_shr_emp + trt_shr_pov + trt_shr_col_grad + trt_shr_wht_collar + 
                                 trt_med_yr_blt + trt_shr_drive_alone + dist_to_cbd, 
                               data = ctpp_2016 %>% filter(pm_cat == "Non-White-Mixed"))
summary(multinom_nw_mixed_model)


### Save model tables ----------------------------------------------------------

stargazer(multinom_nhb_model, multinom_h_model, 
          title = "Multinomial logistic regressions of daytime composition for tracts with Predominantly Black and Latinx nighttime compositions (daytime reference category = Mixed)",
          out = "./output/multinom_predom_nhb_h_coef_table.tex",
          style = "demography", 
          float.env = "sidewaystable",
          covariate.labels = c("Prop. Employed", "Prop. Below FPL", "Prop. w/College Degree", 
                               "Prop. in White Collar Industry", "Median Year HU Built",
                               "Prop. Driving Alone to Work", "Distance to CBD"))

stargazer(multinom_nha_model, multinom_nhw_model, 
          title = "Multinomial logistic regressions of daytime composition for tracts with Predominantly Asian and White nighttime compositions (daytime reference category = Mixed)",
          out = "./output/multinom_predom_nha_nhw_coef_table.tex",
          style = "demography",
          float.env = "sidewaystable",
          covariate.labels = c("Prop. Employed", "Prop. Below FPL", "Prop. w/College Degree", 
                               "Prop. in White Collar Industry", "Median Year HU Built",
                               "Prop. Driving Alone to Work", "Distance to CBD"))

stargazer(multinom_nw_mixed_model, multinom_w_mixed_model, 
          title = "Multinomial logistic regressions of daytime composition for tracts with Non-White-Mixed and White-Mixed nighttime compositions (daytime reference category = Mixed)",
          out = "./output/multinom_predom_nw_mixed_w_mixed_coef_table.tex",
          style = "demography",          
          float.env = "sidewaystable",
          covariate.labels = c("Prop. Employed", "Prop. Below FPL", "Prop. w/College Degree", 
                               "Prop. in White Collar Industry", "Median Year HU Built",
                               "Prop. Driving Alone to Work", "Distance to CBD"))

stargazer(multinom_mult_model, 
          title = "Multinomial logistic regressions of daytime composition for tracts with Multiethnic nighttime compositions (daytime reference category = Mixed)",
          out = "./output/multinom_predom_mult_coef_table.tex",
          style = "demography",
          covariate.labels = c("Prop. Employed", "Prop. Below FPL", "Prop. w/College Degree", 
                               "Prop. in White Collar Industry.", "Median Year HU Built",
                               "Prop. Driving Alone to Work", "Distance to CBD"))


### Tract poverty rate predictions ---------------------------------------------

#generate marginal predictions for each model
h_pov_preds <- ggeffect(multinom_h_model, terms = "trt_shr_pov [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly Latinx")

nhb_pov_preds <- ggeffect(multinom_nhb_model, terms = "trt_shr_pov [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly Black")

nhw_pov_preds <- ggeffect(multinom_nhw_model, terms = "trt_shr_pov [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly White")

mult_pov_preds <- ggeffect(multinom_mult_model, terms = "trt_shr_pov [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Multiethnic")

w_mixed_pov_preds <- ggeffect(multinom_w_mixed_model, terms = "trt_shr_pov [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "White-Mixed")

nw_mixed_pov_preds <- ggeffect(multinom_nw_mixed_model, terms = "trt_shr_pov [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Non-White-Mixed")

#combine the prediction data frames and prep for plotting
pov_preds <- bind_rows(h_pov_preds, nhb_pov_preds, nhw_pov_preds, mult_pov_preds,
                       w_mixed_pov_preds, nw_mixed_pov_preds) %>%
  mutate(response.level = str_replace_all(response.level, "\\.", " "),
         response.level = str_replace_all(response.level, "Non White", "Non-White"),
         pm_cat = paste(pm_cat, "(PM)"))
pov_preds$pm_cat <- factor(pov_preds$pm_cat)
pov_preds$pm_cat <- factor(pov_preds$pm_cat, levels = levels(pov_preds$pm_cat)[c(3, 4, 5, 6, 2, 1)])

#make the small multiple plot for the association of tract characteristic with trajectories
pov_preds_gg <- ggplot(pov_preds, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high,
                                      color = response.level, fill = response.level, shape = response.level)) +
  facet_wrap(~ pm_cat) +
  geom_ribbon(alpha = .25, color = NA) +
  geom_line() +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(19, 15, 17, 23)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "\n% of Residents Below FPL", y = "Predicted Probability of Daytime Type\n",
       color = "Daytime Composition", fill = "Daytime Composition", shape = "Daytime Composition")

#print to screen
pov_preds_gg

#write to disk
ggsave(filename = "./output/multinom_shr_pov.pdf", pov_preds_gg,
       width = 9, height = 6, dpi = 300)


### Tract % college grad predictions -------------------------------------------

#generate marginal predictions for each model
h_col_grad_preds <- ggeffect(multinom_h_model, terms = "trt_shr_col_grad [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly Latinx")

nhb_col_grad_preds <- ggeffect(multinom_nhb_model, terms = "trt_shr_col_grad [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly Black")

nhw_col_grad_preds <- ggeffect(multinom_nhw_model, terms = "trt_shr_col_grad [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly White")

mult_col_grad_preds <- ggeffect(multinom_mult_model, terms = "trt_shr_col_grad [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Multiethnic")

w_mixed_col_grad_preds <- ggeffect(multinom_w_mixed_model, terms = "trt_shr_col_grad [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "White-Mixed")

nw_mixed_col_grad_preds <- ggeffect(multinom_nw_mixed_model, terms = "trt_shr_col_grad [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Non-White-Mixed")

#combine the prediction data frames and prep for plotting
col_grad_preds <- bind_rows(h_col_grad_preds, nhb_col_grad_preds, nhw_col_grad_preds, mult_col_grad_preds,
                       w_mixed_col_grad_preds, nw_mixed_col_grad_preds)%>%
  mutate(response.level = str_replace_all(response.level, "\\.", " "),
         response.level = str_replace_all(response.level, "Non White", "Non-White"),
         pm_cat = paste(pm_cat, "(PM)"))
col_grad_preds$pm_cat <- factor(col_grad_preds$pm_cat)
col_grad_preds$pm_cat <- factor(col_grad_preds$pm_cat, levels = levels(col_grad_preds$pm_cat)[c(3, 4, 5, 6, 2, 1)])

#make the small multiple plot for the association of tract characteristic with trajectories
col_grad_preds_gg <- ggplot(col_grad_preds, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high,
                                                color = response.level, fill = response.level, shape = response.level)) +
  facet_wrap(~ pm_cat) +
  geom_ribbon(alpha = .25, color = NA) +
  geom_line() +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(19, 15, 17, 23)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "\n% of Residents w/College Degree", y = "Predicted Probability of Daytime Type\n",
       color = "Daytime Composition", fill = "Daytime Composition", shape = "Daytime Composition")

#print to screen
col_grad_preds_gg

#write to disk
ggsave(filename = "./output/multinom_shr_col_grad.pdf", col_grad_preds_gg,
       width = 9, height = 6, dpi = 300)


### Tract % employed predictions -----------------------------------------------

#generate marginal predictions for each model
h_emp_preds <- ggeffect(multinom_h_model, terms = "trt_shr_emp [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly Latinx")

nhb_emp_preds <- ggeffect(multinom_nhb_model, terms = "trt_shr_emp [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly Black")

nhw_emp_preds <- ggeffect(multinom_nhw_model, terms = "trt_shr_emp [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly White")

mult_emp_preds <- ggeffect(multinom_mult_model, terms = "trt_shr_emp [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Multiethnic")

w_mixed_emp_preds <- ggeffect(multinom_w_mixed_model, terms = "trt_shr_emp [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "White-Mixed")

nw_mixed_emp_preds <- ggeffect(multinom_nw_mixed_model, terms = "trt_shr_emp [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Non-White-Mixed")

#combine the prediction data frames and prep for plotting
emp_preds <- bind_rows(h_emp_preds, nhb_emp_preds, nhw_emp_preds, mult_emp_preds,
                            w_mixed_emp_preds, nw_mixed_emp_preds) %>%
  mutate(response.level = str_replace_all(response.level, "\\.", " "),
         response.level = str_replace_all(response.level, "Non White", "Non-White"),
         pm_cat = paste(pm_cat, "(PM)"))
emp_preds$pm_cat <- factor(emp_preds$pm_cat)
emp_preds$pm_cat <- factor(emp_preds$pm_cat, levels = levels(emp_preds$pm_cat)[c(3, 4, 5, 6, 2, 1)])

#make the small multiple plot for the association of tract characteristic with trajectories
emp_preds_gg <- ggplot(emp_preds, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high,
                                      color = response.level, fill = response.level, shape = response.level)) +
  facet_wrap(~ pm_cat) +
  geom_ribbon(alpha = .25, color = NA) +
  geom_line() +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(19, 15, 17, 23)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "\n% of Residents 16+ Employed", y = "Predicted Probability of Daytime Type\n",
       color = "Daytime Composition", fill = "Daytime Composition", shape = "Daytime Composition")

#print to screen
emp_preds_gg

#write to disk
ggsave(filename = "./output/multinom_shr_emp.pdf", emp_preds_gg,
       width = 9, height = 6, dpi = 300)


### Tract % white collar predictions -------------------------------------------

#generate marginal predictions for each model
h_wht_collar_preds <- ggeffect(multinom_h_model, terms = "trt_shr_wht_collar [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly Latinx")

nhb_wht_collar_preds <- ggeffect(multinom_nhb_model, terms = "trt_shr_wht_collar [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly Black")

nhw_wht_collar_preds <- ggeffect(multinom_nhw_model, terms = "trt_shr_wht_collar [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly White")

mult_wht_collar_preds <- ggeffect(multinom_mult_model, terms = "trt_shr_wht_collar [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Multiethnic")

w_mixed_wht_collar_preds <- ggeffect(multinom_w_mixed_model, terms = "trt_shr_wht_collar [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "White-Mixed")

nw_mixed_wht_collar_preds <- ggeffect(multinom_nw_mixed_model, terms = "trt_shr_wht_collar [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Non-White-Mixed")

#combine the prediction data frames and prep for plotting
wht_collar_preds <- bind_rows(h_wht_collar_preds, nhb_wht_collar_preds, nhw_wht_collar_preds, mult_wht_collar_preds,
                       w_mixed_wht_collar_preds, nw_mixed_wht_collar_preds) %>%
  mutate(response.level = str_replace_all(response.level, "\\.", " "),
         response.level = str_replace_all(response.level, "Non White", "Non-White"),
         pm_cat = paste(pm_cat, "(PM)"))
wht_collar_preds$pm_cat <- factor(wht_collar_preds$pm_cat)
wht_collar_preds$pm_cat <- factor(wht_collar_preds$pm_cat, levels = levels(wht_collar_preds$pm_cat)[c(3, 4, 5, 6, 2, 1)])

#make the small multiple plot for the association of tract characteristic with trajectories
wht_collar_preds_gg <- ggplot(wht_collar_preds, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high,
                                                    color = response.level, fill = response.level, shape = response.level)) +
  facet_wrap(~ pm_cat) +
  geom_ribbon(alpha = .25, color = NA) +
  geom_line() +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(19, 15, 17, 23)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "\n% of Residents 16+ Employed in White Collar Industries", y = "Predicted Probability of Daytime Type\n",
       color = "Daytime Composition", fill = "Daytime Composition", shape = "Daytime Composition")

#print to screen
wht_collar_preds_gg

#write to disk
ggsave(filename = "./output/multinom_shr_wht_collar.pdf", wht_collar_preds_gg,
       width = 9, height = 6, dpi = 300)


### Tract % driving alone to work predictions ----------------------------------

#generate marginal predictions for each model
h_drive_alone_preds <- ggeffect(multinom_h_model, terms = "trt_shr_drive_alone [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly Latinx")

nhb_drive_alone_preds <- ggeffect(multinom_nhb_model, terms = "trt_shr_drive_alone [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly Black")

nhw_drive_alone_preds <- ggeffect(multinom_nhw_model, terms = "trt_shr_drive_alone [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly White")

mult_drive_alone_preds <- ggeffect(multinom_mult_model, terms = "trt_shr_drive_alone [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Multiethnic")

w_mixed_drive_alone_preds <- ggeffect(multinom_w_mixed_model, terms = "trt_shr_drive_alone [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "White-Mixed")

nw_mixed_drive_alone_preds <- ggeffect(multinom_nw_mixed_model, terms = "trt_shr_drive_alone [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Non-White-Mixed")

#combine the prediction data frames and prep for plotting
drive_alone_preds <- bind_rows(h_drive_alone_preds, nhb_drive_alone_preds, nhw_drive_alone_preds, mult_drive_alone_preds,
                            w_mixed_drive_alone_preds, nw_mixed_drive_alone_preds) %>%
  mutate(response.level = str_replace_all(response.level, "\\.", " "),
         response.level = str_replace_all(response.level, "Non White", "Non-White"),
         pm_cat = paste(pm_cat, "(PM)"))
drive_alone_preds$pm_cat <- factor(drive_alone_preds$pm_cat)
drive_alone_preds$pm_cat <- factor(drive_alone_preds$pm_cat, levels = levels(drive_alone_preds$pm_cat)[c(3, 4, 5, 6, 2, 1)])

#make the small multiple plot for the association of tract characteristic with trajectories
drive_alone_preds_gg <- ggplot(drive_alone_preds, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high,
                                                      color = response.level, fill = response.level, shape = response.level)) +
  facet_wrap(~ pm_cat) +
  geom_ribbon(alpha = .25, color = NA) +
  geom_line() +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(19, 15, 17, 23)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "\n% of Residents 16+ Who Drive Alone to Work", y = "Predicted Probability of Daytime Type\n",
       color = "Daytime Composition", fill = "Daytime Composition", shape = "Daytime Composition")

#print to screen
drive_alone_preds_gg

#write to disk
ggsave(filename = "./output/multinom_shr_drive_alone.pdf", drive_alone_preds_gg,
       width = 9, height = 6, dpi = 300)


### Tract median year built predictions ----------------------------------------

#generate marginal predictions for each model
h_med_yr_blt_preds <- ggeffect(multinom_h_model, terms = "trt_med_yr_blt [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly Latinx")

nhb_med_yr_blt_preds <- ggeffect(multinom_nhb_model, terms = "trt_med_yr_blt [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly Black")

nhw_med_yr_blt_preds <- ggeffect(multinom_nhw_model, terms = "trt_med_yr_blt [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly White")

mult_med_yr_blt_preds <- ggeffect(multinom_mult_model, terms = "trt_med_yr_blt [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Multiethnic")

w_mixed_med_yr_blt_preds <- ggeffect(multinom_w_mixed_model, terms = "trt_med_yr_blt [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "White-Mixed")

nw_mixed_med_yr_blt_preds <- ggeffect(multinom_nw_mixed_model, terms = "trt_med_yr_blt [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Non-White-Mixed")

#combine the prediction data frames and prep for plotting
med_yr_blt_preds <- bind_rows(h_med_yr_blt_preds, nhb_med_yr_blt_preds, nhw_med_yr_blt_preds, mult_med_yr_blt_preds,
                               w_mixed_med_yr_blt_preds, nw_mixed_med_yr_blt_preds) %>%
  mutate(response.level = str_replace_all(response.level, "\\.", " "),
         response.level = str_replace_all(response.level, "Non White", "Non-White"),
         pm_cat = paste(pm_cat, "(PM)"))
med_yr_blt_preds$pm_cat <- factor(med_yr_blt_preds$pm_cat)
med_yr_blt_preds$pm_cat <- factor(med_yr_blt_preds$pm_cat, levels = levels(med_yr_blt_preds$pm_cat)[c(3, 4, 5, 6, 2, 1)])

#make the small multiple plot for the association of tract characteristic with trajectories
med_yr_blt_preds_gg <- ggplot(med_yr_blt_preds, 
                              aes(x = x + 1960, y = predicted, ymin = conf.low, ymax = conf.high,
                                  color = response.level, fill = response.level, shape = response.level)) +
  facet_wrap(~ pm_cat) +
  geom_ribbon(alpha = .25, color = NA) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(19, 15, 17, 23)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "\nMedian Year Housing Units Built", y = "Predicted Probability of Daytime Type\n",
       color = "Daytime Composition", fill = "Daytime Composition", shape = "Daytime Composition")

#print to screen
med_yr_blt_preds_gg

#write to disk
ggsave(filename = "./output/multinom_med_yr_blt.pdf", med_yr_blt_preds_gg,
       width = 9, height = 6, dpi = 300)


#### Distance to CBD -----------------------------------------------------------

#generate marginal predictions for each model
h_dist_to_cbd_preds <- ggeffect(multinom_h_model, terms = "dist_to_cbd [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly Latinx")

nhb_dist_to_cbd_preds <- ggeffect(multinom_nhb_model, terms = "dist_to_cbd [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly Black")

nhw_dist_to_cbd_preds <- ggeffect(multinom_nhw_model, terms = "dist_to_cbd [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Predominantly White")

mult_dist_to_cbd_preds <- ggeffect(multinom_mult_model, terms = "dist_to_cbd [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Multiethnic")

w_mixed_dist_to_cbd_preds <- ggeffect(multinom_w_mixed_model, terms = "dist_to_cbd [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "White-Mixed")

nw_mixed_dist_to_cbd_preds <- ggeffect(multinom_nw_mixed_model, terms = "dist_to_cbd [meansd]", ci.lvl = .95) %>%
  as_tibble() %>%
  mutate(pm_cat = "Non-White-Mixed")

#combine the prediction data frames and prep for plotting
dist_to_cbd_preds <- bind_rows(h_dist_to_cbd_preds, nhb_dist_to_cbd_preds, nhw_dist_to_cbd_preds, mult_dist_to_cbd_preds,
                              w_mixed_dist_to_cbd_preds, nw_mixed_dist_to_cbd_preds) %>%
  mutate(response.level = str_replace_all(response.level, "\\.", " "),
         response.level = str_replace_all(response.level, "Non White", "Non-White"),
         pm_cat = paste(pm_cat, "(PM)"))
dist_to_cbd_preds$pm_cat <- factor(dist_to_cbd_preds$pm_cat)
dist_to_cbd_preds$pm_cat <- factor(dist_to_cbd_preds$pm_cat, levels = levels(dist_to_cbd_preds$pm_cat)[c(3, 4, 5, 6, 2, 1)])

#make the small multiple plot for the association of tract characteristic with trajectories
dist_to_cbd_preds_gg <- ggplot(dist_to_cbd_preds, 
                              aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high,
                                  color = response.level, fill = response.level, shape = response.level)) +
  facet_wrap(~ pm_cat) +
  geom_ribbon(alpha = .25, color = NA) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(19, 15, 17, 23)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "\nEuclidean Distance to CBD (km)", y = "Predicted Probability of Daytime Type\n",
       color = "Daytime Composition", fill = "Daytime Composition", shape = "Daytime Composition")

#print to screen
dist_to_cbd_preds_gg

#write to disk
ggsave(filename = "./output/multinom_dist_to_cbd.pdf", dist_to_cbd_preds_gg,
       width = 9, height = 6, dpi = 300)


