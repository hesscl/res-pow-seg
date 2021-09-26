
#dependencies
library(tidyverse)
library(sf)
library(ggeffects)
library(stargazer)
library(sandwich)

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
         trt_med_val = AF9LE001/10000) %>%
  # mutate(trt_tot_pop = AF2LE001,
  #        trt_shr_rec_mover = AF2WE003/AF2WE001,
  #        trt_shr_col_grad = (AF4OE022+AF4OE023+AF4OE024+AF4OE025)/AF4OE001,
  #        trt_shr_pov = (AF43E002+AF43E003)/AF43E001,
  #        trt_med_hh_inc = AF49E001,
  #        trt_shr_emp = AF67E004/AF67E003,
  #        trt_shr_vac = AF7OE003/AF7OE001,
  #        trt_shr_rent_occ = AF7PE003/AF7PE001,
  #        trt_shr_sfh = AF8EE002/AF8EE001,
  #        trt_med_yr_blt = AF8IE001,
  #        trt_med_gross_rent = AF89E001,
  #        trt_med_val = AF9LE001) %>%
  select(GISJOIN, starts_with("trt_")) %>%
  mutate_at(vars(starts_with("trt")), ~ as.numeric(scale(.)))


####  Diagnostics --------------------------------------------------------------

#plot non-hispanic black counts for chicago metro area
 test_chi <- ctpp %>% filter(cbsafp == "16980")
 
ggplot(test_chi, aes(fill = res_nhb_est)) +
  facet_wrap(~ year) +
  geom_sf(color = NA)
# 
# #plot non-hispanic white % for Seattle metro area
# test_sea <- ctpp %>% filter(cbsafp == "42660")
# 
# ggplot(test_sea, aes(fill = res_nhw_est/res_total_est)) + 
#   facet_wrap(~ year) + 
#   geom_sf(color = NA)


#### Filter tract and metro set ------------------------------------------------

#remove cases without any people
ctpp <- ctpp %>%
  filter(res_total_est > 0, pow_total_est > 0) %>%
  st_drop_geometry() %>%
  mutate(fct_year = factor(year),
         prin_city = ifelse(is.na(prin_city) | prin_city == "N", FALSE, TRUE))

#identify metros with 3k of each race/eth group
ctpp_metros <- ctpp %>%
  filter(year == "2000") %>%
  group_by(cbsafp) %>%
  summarize(nhb = sum(res_nhb_est, na.rm = TRUE) >= 3000,
            nhw = sum(res_nhw_est, na.rm = TRUE) >= 3000,
            h = sum(res_h_est, na.rm = TRUE) >= 3000,
            nha = sum(res_nha_est, na.rm = TRUE) >= 3000) %>%
  mutate_at(vars(everything(), -cbsafp), ~ ifelse(., cbsafp, NA)) %>%
  select(-cbsafp)

ctpp <- ctpp %>%
  filter(!is.na(trt_shr_nhw_am), !is.na(trt_shr_nhb_am), !is.na(trt_shr_nha_am), !is.na(trt_shr_h_am),
         cbsafp %in% ctpp_metros$nhb, cbsafp %in% ctpp_metros$nhw, 
         cbsafp %in% ctpp_metros$h, cbsafp %in% ctpp_metros$nha) 


#### Test clustering -----------------------------------------------------------
# library(mclust)
# 
# comp_data <- ctpp %>%
#   st_drop_geometry() %>%
#   filter(cbsafp %in% ctpp_metros$nhb) %>%
#   filter_at(vars(starts_with("trt_shr")), ~ !is.na(.)) %>%
#   select(GISJOIN, year, starts_with("trt_shr")) %>%
#   select(-starts_with("trt_shr_nho"))
# 
# test_bic <- mclustBIC(comp_data %>% select(ends_with("_pm")))
# 
# test_model <- Mclust(comp_data %>% select(ends_with("_pm")), x = test_bic)
# 
# pred <- predict(test_model, newdata = comp_data %>% select(ends_with("_am")))
# 
# test_data$pm_cat <- test_model$classification
# 
# 
# pm_cat <- test_data %>% 
#   group_by(cat) %>%
#   summarize_at(vars(starts_with("trt")), median) %>%
#   arrange(desc(trt_shr_nhw_pm)) %>%
#   mutate(pm_cat = case_when(cat == 1 ~ "Predominantly White",
#                             cat == 7 ~ "Predominantly White",
#                             cat == 5 ~ "White-Black",
#                             cat == 2 ~ "White-Latinx",
#                             cat == 3 ~ "White-Asian",
#                             cat == 8 ~ "Latinx-White",
#                             cat == 6 ~ "Black-White",
#                             cat == 4 ~ "Multiethnic",
#                             cat == 9 ~ "Multiethnic")) %>%
#   select(pm_cat, cat)
# 
# pm_cat <- left_join(test_data, pm_cat) %>%
#   select(GISJOIN, year, pm_cat)
# 
# ctpp <- inner_join(ctpp, pm_cat)

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

# ggplot(ctpp %>% filter(cbsafp == "42660"), 
#        aes(fill = chg_nhb)) +
#   geom_sf(color = NA) +
#   scale_fill_gradient2()
# 
# ggplot(ctpp %>% filter(cbsafp == "42660"), 
#        aes(fill = chg_nhw)) +
#   geom_sf(color = NA) +
#   scale_fill_gradient2()
# 
# ggplot(ctpp %>% filter(cbsafp == "16980"), 
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
                    ~ lm(as.formula(paste0(.x, "~ pm_cat * fct_year + factor(cbsafp)")), ctpp))

#now fit the descriptive model without metro FE to each outcome
intraday$desc_fit <- map(intraday$outcome, 
                       ~ lm(as.formula(paste0(.x, "~ trt_tot_pop + trt_shr_rec_mover + trt_shr_pov +
                                              trt_med_hh_inc + trt_shr_emp + trt_shr_vac + trt_shr_rent_occ +
                                              trt_shr_sfh + trt_med_yr_blt")), 
                            ctpp %>% filter(year == "2012-2016 ACS") %>% left_join(acs_2012_2016)))

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
                            ~ tibble(ggemmeans(lm(as.formula(paste0(.x, "~ pm_cat * fct_year + factor(cbsafp)")), ctpp,
                                                  weights = ctpp[[paste0("res_total_est")]]), 
                                               terms = c("fct_year", "pm_cat"),
                                               vcov.fun = "vcovHC")))

intraday$fe_margins <- map(intraday$outcome, 
                        ~ tibble(ggemmeans(lm(as.formula(paste0(.x, "~ pm_cat * fct_year + factor(cbsafp)")), ctpp), 
                                           terms = c("fct_year", "pm_cat"),
                                           vcov.fun = "vcovCL",
                                           vcov.args = list(cluster = "cbsafp"))))

intraday$desc_coef <- map(intraday$desc_fit, broom::tidy) 
                             
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



intraday_preds$pm_cat <- factor(intraday_preds$pm_cat)
intraday_preds$pm_cat <- factor(intraday_preds$pm_cat, levels = levels(intraday_preds$pm_cat)[c(7,1,6,3,2,5,4)])

intraday_preds$race_eth <- factor(intraday_preds$race_eth)
intraday_preds$race_eth <- factor(intraday_preds$race_eth, levels = levels(intraday_preds$race_eth)[c(3,1,2,4,5)])

pred_predom_gg <- ggplot(intraday_preds %>% 
                                    filter(pm_cat %in% c("Predominantly Black", "Predominantly Latinx", 
                                                         "Predominantly Asian", "Predominantly White"),
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
                                    filter(!pm_cat %in% c("Predominantly Black", "Predominantly Latinx", 
                                                         "Predominantly Asian", "Predominantly White"),
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
          #order = c(1, 2, 4, 3 , 5),
          # covariate.labels = c("Period (1 = post)", "Treated ",
          #                      "Period X Treatment", "Listed Area (sqft)"),
          model.names = FALSE, 
          model.numbers = FALSE,
          dep.var.labels.include = TRUE,
          # dep.var.labels = c("Allen, TX Treatment", "ACS Commuting Treatment"),
          float.env = "table",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          notes = c("Heteroskedasticity Consistent Standard Errors in Parentheses; + p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = FALSE,
          column.sep.width = "2pt",
          out = "./output/ols_intraday_coef_table.tex")


















#save them to disk
ggsave(filename = "./output/ols_intraday_chg_pred_blk_lat_asi.pdf", pred_blk_lat_asi_gg,
       width = 8, height = 6, dpi = 300)
ggsave(filename = "./output/ols_fe_intraday_chg_pred_blk_lat_asi.pdf", pred_blk_lat_asi_fe_gg,
       width = 8, height = 6, dpi = 300)

ggsave(filename = "./output/ols_intraday_chg_wht_mxd.pdf", pred_wht_mxd_gg,
       width = 6, height = 6, dpi = 300)
ggsave(filename = "./output/ols_fe_intraday_chg_wht_mxd.pdf", pred_wht_mxd_fe_gg,
       width = 6, height = 6, dpi = 300)

ggsave(filename = "./output/ols_intraday_chg_mlt.pdf", pred_mlt_gg,
       width = 6, height = 6, dpi = 300)
ggsave(filename = "./output/ols_fe_intraday_chg_mlt.pdf", pred_mlt_fe_gg,
       width = 6, height = 6, dpi = 300)

































#prep the data we'll use for plotting
intraday_preds <- intraday %>%
  select(race_eth, wls_margins) %>%
  unnest(wls_margins) %>%
  select(fct_year = x, pred = predicted, lower = conf.low, upper = conf.high, pm_cat = group, race_eth) %>%
  filter(fct_year %in% c("2000", "2012-2016"))

#make a set of plots
pred_blk_lat_wls_gg <- ggplot(intraday_preds %>% filter(pm_cat %in% c("Predominantly Black", "Predominantly Latinx")), 
                          aes(y = race_eth, x = pred, xmin = lower, xmax = upper, fill = fct_rev(fct_year))) +
  facet_grid(race_eth ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = "dodge") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "",
       subtitle = "Model: Weighted Least Squares, No Metro FE")

pred_wht_mxd_wls_gg <- ggplot(intraday_preds %>% filter(pm_cat %in% c("Predominantly White", "White-Mixed")), 
                          aes(y = race_eth, x = pred, xmin = lower, xmax = upper, fill = fct_rev(fct_year))) +
  facet_grid(race_eth ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = "dodge") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "",
       subtitle = "Model: Weighted Least Squares, No Metro FE")

pred_mlt_wls_gg <- ggplot(intraday_preds %>% filter(pm_cat %in% c("Multiethnic")), 
                      aes(y = race_eth, x = pred, xmin = lower, xmax = upper, fill = fct_rev(fct_year))) +
  facet_grid(race_eth ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = "dodge") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "",
       subtitle = "Model: Weighted Least Squares, No Metro FE")

pred_asi_wls_gg <- ggplot(intraday_preds %>% filter(pm_cat %in% c("Predominantly Asian")), 
                      aes(y = race_eth, x = pred, xmin = lower, xmax = upper, fill = fct_rev(fct_year))) +
  facet_grid(race_eth ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = "dodge") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "",
       subtitle = "Model: Weighted Least Squares, No Metro FE")

#print them to screen
pred_blk_lat_wls_gg
pred_wht_mxd_wls_gg
pred_mlt_wls_gg
pred_asi_wls_gg

#save them to disk
ggsave(filename = "./output/wls_intraday_chg_blk_lat.pdf", pred_blk_lat_wls_gg,
       width = 10, height = 6, dpi = 300)
ggsave(filename = "./output/wls_intraday_chg_wht_mxd.pdf", pred_wht_mxd_wls_gg,
       width = 10, height = 6, dpi = 300)
ggsave(filename = "./output/wls_intraday_chg_mlt.pdf", pred_mlt_wls_gg,
       width = 6, height = 6, dpi = 300)
ggsave(filename = "./output/wls_intraday_chg_asi.pdf", pred_asi_wls_gg,
       width = 6, height = 6, dpi = 300)



#### Model neighborhood factors associated with more/less of particular chg ----

intraday_desc_coef <- intraday %>%
  select(race_eth, desc_coef) %>%
  unnest(desc_coef) %>%
  filter(race_eth != "Non-Latinx Other", term != "(Intercept)") %>%
  mutate(race_eth = relevel(as.factor(race_eth), "Non-Latinx Black")) %>%
  group_by(race_eth) %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  ungroup()

#intraday_desc_coef$race_eth <- factor(intraday_desc_coef$race_eth)
#intraday_desc_coef$race_eth <- factor(intraday_desc_coef$race_eth, levels = levels(intraday_desc_coef$race_eth)[c(3,1,2,4)])

var_labels <- rev(c("Total Population (1000s)", "Prop. Vacant HU", "Prop. SFH HU",
                "Prop Renter Occupied", "Prop. Recent Movers", "Prop. Below FPL",
                "Prop Employed", "Median Year Built (centered 1960)", "Median HH Income (10Ks)"))

intraday_desc_gg <- ggplot(intraday_desc_coef, aes(x = estimate,
                               xmin = estimate - 1.96 * std.error,
                               xmax = estimate + 1.96 * std.error,
                               y = term)) +
  facet_grid(~ race_eth) +
  geom_vline(xintercept = 0) +
  geom_errorbar(width = .5) +
  geom_point() +
  scale_y_discrete(labels = var_labels) +
  theme_bw() +
  labs(x = "\nStandardized Regression Coefficient", y = "", 
       subtitle = "Correlates of intraday change for 2012-2016 ACS period")

intraday_desc_gg

ggsave("./output/desc_chg_2012_2016.pdf", intraday_desc_gg,
       width = 10, height = 8, dpi = 300)
