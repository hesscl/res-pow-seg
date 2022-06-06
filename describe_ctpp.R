
#dependencies
library(tidyverse)
library(sf)
library(ggeffects)
library(stargazer)
library(xtable)
library(sandwich)
library(lmtest)
library(broom)
library(fixest)

#set working directory
setwd("H:/res-pow-seg")

#load the race/ethncity data
load("./input/ctpp-neigh-chg-data.RData")

#load the earnings data
ctpp_earn <- read_csv("./input/ctpp-earnings-data.csv")

#load the multidimensional diversity data
ctpp_mdiv <- read_csv("./input/ctpp_multidim_div.csv")

#load 2010 tract shapefile
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


####  Diagnostics --------------------------------------------------------------

# #plot non-hispanic black counts for chicago metro area
# test_chi <- ctpp %>%
#   filter(cbsafp10 == "16980", year == "2012-2016 ACS") %>%
#   mutate(chg_nhb_rank = rank(chg_nhb),
#          chg_h_rank = rank(chg_h),
#          chg_nha_rank = rank(chg_nha)) %>%
#   ungroup()
# 
# test_chi %>% 
#   st_drop_geometry() %>%
#   filter(chg_nhb_rank %in% 1:10) %>% 
#   select(chg_nhb, pm_cat, am_cat, place_name)
# 
# test_chi %>% 
#   st_drop_geometry %>%
#   filter(chg_h_rank %in% 1:10) %>% 
#   select(chg_h, pm_cat, am_cat, place_name)
# 
# test_chi %>% 
#   st_drop_geometry %>%
#   filter(chg_nha_rank %in% 1:10) %>% 
#   select(chg_nha, pm_cat, am_cat, place_name)
# 
# ggplot(test_chi %>% filter(year == "2012-2016 ACS"), aes(fill = chg_nhb)) +
#   facet_wrap(~ year) +
#   geom_sf(color = NA) +
#   scale_fill_gradient2() +
#   geom_sf(data = test_chi %>% filter(chg_nhb_rank %in% 1:10), 
#           color = "red", lwd = 1) 
# ggsave(filename = "./output/diag_maps/chi_top_10_chg_nhb.pdf",
#        width = 8, height = 6, dpi = 300)
# 
# ggplot(test_chi %>% filter(year == "2012-2016 ACS"), aes(fill = chg_h)) +
#   facet_wrap(~ year) +
#   geom_sf(color = NA) +
#   scale_fill_gradient2() +
#   geom_sf(data = test_chi %>% filter(chg_h_rank %in% 1:10), 
#           color = "red", lwd = 1) 
# ggsave(filename = "./output/diag_maps/chi_top_10_chg_h.pdf",
#        width = 8, height = 6, dpi = 300)
# 
# ggplot(test_chi %>% filter(year == "2012-2016 ACS"), aes(fill = chg_nha)) +
#   facet_wrap(~ year) +
#   geom_sf(color = NA) +
#   scale_fill_gradient2() +
#   geom_sf(data = test_chi %>% filter(chg_nha_rank %in% 1:10), 
#           color = "red", lwd = 1) 
# ggsave(filename = "./output/diag_maps/chi_top_10_chg_nha.pdf",
#        width = 8, height = 6, dpi = 300)
# 
# 
# test_sea <- ctpp %>%
#   filter(cbsafp10 == "42660", year == "2012-2016 ACS") %>%
#   mutate(chg_nhb_rank = rank(chg_nhb),
#          chg_h_rank = rank(chg_h),
#          chg_nha_rank = rank(chg_nha)) %>%
#   ungroup()
# 
# test_sea %>% 
#   st_drop_geometry() %>%
#   filter(chg_nhb_rank %in% 1:10) %>% 
#   select(chg_nhb, pm_cat, am_cat, place_name)
# 
# test_sea %>% 
#   st_drop_geometry %>%
#   filter(chg_h_rank %in% 1:10) %>% 
#   select(chg_h, pm_cat, am_cat, place_name)
# 
# test_sea %>% 
#   st_drop_geometry %>%
#   filter(chg_nha_rank %in% 1:10) %>% 
#   select(chg_nha, pm_cat, am_cat, place_name)
# 
# ggplot(test_sea %>% filter(year == "2012-2016 ACS"), aes(fill = chg_nhb)) +
#   facet_wrap(~ year) +
#   geom_sf(color = NA) +
#   scale_fill_gradient2() +
#   geom_sf(data = test_sea %>% filter(chg_nhb_rank %in% 1:10), 
#           color = "red", lwd = 2) 
# ggsave(filename = "./output/diag_maps/sea_top_10_chg_nhb.pdf",
#        width = 8, height = 6, dpi = 300)
# 
# ggplot(test_sea %>% filter(year == "2012-2016 ACS"), aes(fill = chg_h)) +
#   facet_wrap(~ year) +
#   geom_sf(color = NA) +
#   scale_fill_gradient2() +
#   geom_sf(data = test_sea %>% filter(chg_h_rank %in% 1:10), 
#           color = "red", lwd = 2) 
# ggsave(filename = "./output/diag_maps/sea_top_10_chg_h.pdf",
#        width = 8, height = 6, dpi = 300)
# 
# ggplot(test_sea %>% filter(year == "2012-2016 ACS"), aes(fill = chg_nha)) +
#   facet_wrap(~ year) +
#   geom_sf(color = NA) +
#   scale_fill_gradient2() +
#   geom_sf(data = test_sea %>% filter(chg_nha_rank %in% 1:10), 
#           color = "red", lwd = 2) 
# ggsave(filename = "./output/diag_maps/sea_top_10_chg_nha.pdf",
#        width = 8, height = 6, dpi = 300)
# 
# 
# test_phl <- ctpp %>%
#   filter(cbsafp10 == "37980", year == "2012-2016 ACS") %>%
#   mutate(chg_nhb_rank = rank(chg_nhb),
#          chg_h_rank = rank(chg_h),
#          chg_nha_rank = rank(chg_nha)) %>%
#   ungroup()
# 
# test_phl %>% 
#   st_drop_geometry() %>%
#   filter(chg_nhb_rank %in% 1:10) %>% 
#   select(chg_nhb, pm_cat, am_cat, place_name)
# 
# test_phl %>% 
#   st_drop_geometry %>%
#   filter(chg_h_rank %in% 1:10) %>% 
#   select(chg_h, pm_cat, am_cat, place_name)
# 
# test_phl %>% 
#   st_drop_geometry %>%
#   filter(chg_nha_rank %in% 1:10) %>% 
#   select(chg_nha, pm_cat, am_cat, place_name)
# 
# ggplot(test_phl %>% filter(year == "2012-2016 ACS"), aes(fill = chg_nhb)) +
#   facet_wrap(~ year) +
#   geom_sf(color = NA) +
#   scale_fill_gradient2() +
#   geom_sf(data = test_phl %>% filter(chg_nhb_rank %in% 1:10), 
#           color = "red", lwd = 2) 
# ggsave(filename = "./output/diag_maps/phl_top_10_chg_nhb.pdf",
#        width = 8, height = 6, dpi = 300)
# 
# ggplot(test_phl %>% filter(year == "2012-2016 ACS"), aes(fill = chg_h)) +
#   facet_wrap(~ year) +
#   geom_sf(color = NA) +
#   scale_fill_gradient2() +
#   geom_sf(data = test_phl %>% filter(chg_h_rank %in% 1:10), 
#           color = "red", lwd = 2) 
# ggsave(filename = "./output/diag_maps/phl_top_10_chg_h.pdf",
#        width = 8, height = 6, dpi = 300)
# 
# ggplot(test_phl %>% filter(year == "2012-2016 ACS"), aes(fill = chg_nha)) +
#   facet_wrap(~ year) +
#   geom_sf(color = NA) +
#   scale_fill_gradient2() +
#   geom_sf(data = test_phl %>% filter(chg_nha_rank %in% 1:10), 
#           color = "red", lwd = 2) 
# ggsave(filename = "./output/diag_maps/phl_top_10_chg_nha.pdf",
#        width = 8, height = 6, dpi = 300)
# 
# # 
# # #plot non-hispanic white % for Seattle metro area
# # test_sea <- ctpp %>% filter(cbsafp10 == "42660")
# # 
# # ggplot(test_sea, aes(fill = res_nhw_est/res_total_est)) + 
# #   facet_wrap(~ year) + 
# #   geom_sf(color = NA)


#### Filter tract and metro set ------------------------------------------------

#create a copy of the geometry for later
ctpp_geo <- ctpp %>%
  select(geoid, year, geometry)

#remove cases without any people, drop geometry for aspatial analysis
ctpp <- ctpp %>%
  st_drop_geometry() %>%
  filter(res_total_est > 0, pow_total_est > 0) %>%
  mutate(fct_year = factor(year),
         prin_city = ifelse(is.na(prin_city) | prin_city == "N", FALSE, TRUE))

#identify metros with 3k of each race/eth group
ctpp_metros <- ctpp %>%
  filter(year == "2000") %>%
  group_by(cbsafp10) %>%
  summarize(nhb = sum(res_nhb_est, na.rm = TRUE) >= 1000,
            nhw = sum(res_nhw_est, na.rm = TRUE) >= 1000,
            h = sum(res_h_est, na.rm = TRUE) >= 1000,
            nha = sum(res_nha_est, na.rm = TRUE) >= 1000,
            tot_pop = sum(res_total_est, na.rm = TRUE) >= 50000) %>%
  mutate_at(vars(everything(), -cbsafp10), ~ ifelse(., cbsafp10, NA)) %>%
  select(-cbsafp10)

ctpp <- ctpp %>%
  filter(!is.na(trt_shr_nhw_am), !is.na(trt_shr_nhb_am), !is.na(trt_shr_nha_am), !is.na(trt_shr_h_am),
         cbsafp10 %in% ctpp_metros$tot_pop,
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
  mutate(pm_cat = fct_reorder(pm_cat, n),
         sum = sum(n)) 

chg_matrix$pm_cat <- factor(chg_matrix$pm_cat)
chg_matrix$pm_cat <- factor(chg_matrix$pm_cat, levels = rev(levels(chg_matrix$pm_cat)[c(6, 4, 5, 3, 7, 2, 1)]))

chg_matrix$am_cat <- factor(chg_matrix$am_cat)
chg_matrix$am_cat <- factor(chg_matrix$am_cat, levels = levels(chg_matrix$pm_cat))

# chg_matrix <- chg_matrix %>%
#   mutate(pm_cat = paste(pm_cat, "\n(n =", n))

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
  labs(x = "\nDaytime Racial/Ethnic Composition", 
       y = "Nighttime Racial/Ethnic Composition\n")

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

chg_matrix_city_sub$pm_cat <- factor(chg_matrix_city_sub$pm_cat)
chg_matrix_city_sub$pm_cat <- factor(chg_matrix_city_sub$pm_cat, levels = rev(levels(chg_matrix_city_sub$pm_cat)[c(6, 4, 5, 3, 7, 2, 1)]))

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
  labs(x = "\nDaytime Racial/Ethnic Composition", 
       y = "Nighttime Racial/Ethnic Composition\n",
       title = "Urban Neighborhoods")

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
  labs(x = "\nDaytime Racial/Ethnic Composition", 
       y = "Nighttime Racial/Ethnic Composition\n",
       title = "Suburban Neighborhoods")

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

#start by defining a table with outcomes and labels as cols
intraday <- tibble(outcome = paste0("chg_", c("nhb", "nhw", "nha", "nho", "h")),
                   race_eth = c("Non-Latinx Black", "Non-Latinx White", "Non-Latinx Asian",
                                "Non-Latinx Other", "Latinx"))

#now fit the basic unweighted model to each outcome
intraday$ols_fit <- map(intraday$outcome, 
                       ~ lm(as.formula(paste0(.x, "~ pm_cat * fct_year")), ctpp))

intraday$ols_crse <- map(intraday$ols_fit, 
                         ~ sqrt(diag(vcovCL(., cluster = ~ GISJOIN))))

#now fit the basic model with metro FE to each outcome
# intraday$fe_fit <- map(intraday$outcome, 
#                     ~ lm(as.formula(paste0(.x, "~ pm_cat * fct_year + factor(cbsafp10)")), ctpp))

# intraday$fe_crse <- map(intraday$fe_fit, 
#                          ~ sqrt(diag(vcovCL(., cluster = ~ GISJOIN))))

#now generate marginal means estimated over fixed effects
intraday$ols_margins <- map(intraday$outcome, 
                           ~ tibble(ggemmeans(lm(as.formula(paste0(.x, "~ pm_cat * fct_year")), ctpp), 
                                              terms = c("fct_year", "pm_cat"),
                                              vcov.fun = "vcovCL",
                                              vcov.args = list(cluster = "GISJOIN"))))

# intraday$fe_margins <- map(intraday$outcome, 
#                         ~ tibble(ggemmeans(lm(as.formula(paste0(.x, "~ pm_cat * fct_year + factor(cbsafp10)")), ctpp), 
#                                            terms = c("fct_year", "pm_cat"),
#                                            vcov.fun = "vcovCL",
#                                            vcov.args = list(cluster = "cbsafp10"))))
                             
#prep the data we'll use for plotting
intraday_preds <- intraday %>%
  select(race_eth, ols_margins) %>%
  unnest(ols_margins) %>%
  select(fct_year = x, pred = predicted, lower = conf.low, upper = conf.high, pm_cat = group, race_eth) 

# intraday_preds_fe <- intraday %>%
#   select(race_eth, fe_margins) %>%
#   unnest(fe_margins) %>%
#   select(fct_year = x, pred = predicted, lower = conf.low, upper = conf.high, pm_cat = group, race_eth) 

intraday_preds <- intraday_preds %>%
  mutate(race_eth = case_when(race_eth == "Non-Latinx White" ~ "NL White",
                              race_eth == "Non-Latinx Black" ~ "NL Black",
                              race_eth == "Latinx" ~ "Latinx",
                              race_eth == "Non-Latinx Asian" ~ "NL Asian",
                              race_eth == "Non-Latinx Other" ~ "NL Other"),
         pm_cat = paste0("Nighttime:\n", pm_cat))

intraday_preds$race_eth <- factor(intraday_preds$race_eth)
intraday_preds$race_eth <- factor(intraday_preds$race_eth, levels = levels(intraday_preds$race_eth)[c(5, 3, 1, 2, 4)])

intraday_preds$pm_cat <- factor(intraday_preds$pm_cat)
intraday_preds$pm_cat <- factor(intraday_preds$pm_cat, levels = levels(intraday_preds$pm_cat)[c(6,4,5,3,7,2,1)])

#create the OLS plots
pred_predom_gg <- ggplot(intraday_preds %>% filter(race_eth != "Non-Latinx Other"), 
                          aes(x = race_eth, y = pred, ymin = lower, ymax = upper, 
                              fill = fct_year)) +
  facet_wrap(~ pm_cat, nrow = 2) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbar(position = position_dodge(width = 1), width  = .5) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(7, "Greys")[c(3,5,7)]) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(legend.position = c(.88, .2),
        panel.spacing = unit(.25, "in"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(y = "Predicted Intraday Change\n", x = "\nRacial/Ethnic Composition", fill = "Period")

#print to screen
pred_predom_gg

#save to disk
ggsave(filename = "./output/ols_intraday_chg_preds.pdf", pred_predom_gg,
       width = 8, height = 6, dpi = 300)

#assemble OLS coefficient table
stargazer(intraday$ols_fit,
          se = intraday$ols_crse,
          keep.stat = c("n"),
          style = "demography",
          title = "Linear regressions of intraday change in racial and ethnic composition, 2000 through 2012-2016 ACS",
          model.names = FALSE, 
          model.numbers = FALSE,
          dep.var.labels.include = TRUE,
          float.env = "table",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001),
          notes = c("Clustered standard errors in parentheses; * p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = FALSE,
          column.sep.width = "2pt",
          out = "./output/ols_intraday_coef_table.tex")


#### Look at change in total population ----------------------------------------

res_total_model <- glm(res_total_est ~ pm_cat,
                       family = "gaussian",
                       ctpp)
summary(res_total_model)

pow_total_model <- glm(pow_total_est ~ pm_cat,
                       family = "gaussian",
                       ctpp)
summary(pow_total_model)

total_pred_grid <- expand_grid(pm_cat = unique(ctpp$pm_cat))
total_pred_grid$`Neighborhood` <- predict(res_total_model, newdata = total_pred_grid)
total_pred_grid$`Workhood` <- predict(pow_total_model, newdata = total_pred_grid)
total_pred_grid <- total_pred_grid %>%
  pivot_longer(-pm_cat)

total_pred_gg <- ggplot(total_pred_grid, aes(x = pm_cat, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "", y = "Average Total Workers 16+\n", fill = "")

ggsave(filename = "./output/total_workers_am_pm.pdf", total_pred_gg,
       width = 8, height = 6, dpi = 300)


#### Look at association of segregation with typical flows ---------------------

#make a spatial polygons data frame we can use with OasisR functions
ctpp_geo_seg <- semi_join(ctpp_geo, ctpp) %>%
  arrange(geoid, year) %>%
  as_Spatial()

#make our metro summary table
ctpp <- ctpp %>%
  ungroup() %>%
  mutate(trt_shr_nho_am = ifelse(trt_shr_nho_am < 0, 0, trt_shr_nho_am),
         trt_shr_nho_pm = ifelse(trt_shr_nho_pm < 0, 0, trt_shr_nho_pm)) %>%
  mutate(race_eth_div_am = trt_shr_nhw_am * ifelse(trt_shr_nhw_am != 0, log(1/trt_shr_nhw_am), 0) + 
                           trt_shr_nhb_am * ifelse(trt_shr_nhb_am != 0, log(1/trt_shr_nhb_am), 0) +
                           trt_shr_h_am * ifelse(trt_shr_h_am != 0, log(1/trt_shr_h_am), 0) + 
                           trt_shr_nha_am * ifelse(trt_shr_nha_am != 0, log(1/trt_shr_nha_am), 0) + 
                           trt_shr_nho_am * ifelse(trt_shr_nho_am != 0, log(1/trt_shr_nho_am), 0),
         
         race_eth_div_pm = trt_shr_nhw_pm * ifelse(trt_shr_nhw_pm != 0, log(1/trt_shr_nhw_pm), 0) + 
                           trt_shr_nhb_pm * ifelse(trt_shr_nhb_pm != 0, log(1/trt_shr_nhb_pm), 0) +
                           trt_shr_h_pm * ifelse(trt_shr_h_pm != 0, log(1/trt_shr_h_pm), 0) + 
                           trt_shr_nha_pm * ifelse(trt_shr_nha_pm != 0, log(1/trt_shr_nha_pm), 0) + 
                           trt_shr_nho_pm * ifelse(trt_shr_nho_pm != 0, log(1/trt_shr_nho_pm), 0))

avg_flow <- ctpp %>%
  arrange(geoid, year) %>%
  group_by(cbsafp10, metro_name, year) %>%
  summarize(n_tract = n(),
            avg_div_whood = mean(race_eth_div_am),
            avg_div_nhood = mean(race_eth_div_pm),
            avg_div_whood_city = mean(race_eth_div_am[prin_city]),
            avg_div_nhood_city = mean(race_eth_div_pm[prin_city]),
            avg_div_whood_sub = mean(race_eth_div_am[!prin_city]),
            avg_div_nhood_sub = mean(race_eth_div_pm[!prin_city]),
            multi_theil_nhood = OasisR::HMulti(cbind(res_nha_est, res_nhb_est, res_h_est, res_nhw_est)),
            iso_nhood = OasisR::xPx(cbind(res_nhb_est+res_nha_est+res_h_est+res_nho_est, res_nhw_est))[1],
            spat_multi_theil_nhood = OasisR::spatmultiseg(cbind(res_nha_est, res_nhb_est, res_h_est, res_nhw_est),
                                                          spatobj = ctpp_geo_seg)[3],
            spat_iso_nhood = OasisR::spatinteract(cbind(res_nhb_est+res_nha_est+res_h_est+res_nho_est, res_nhw_est),
                                                        spatobj = ctpp_geo_seg)[1,1],
            spat_iso_nhb_nhood = OasisR::spatinteract(cbind(res_nhb_est, res_nhw_est),
                                                      spatobj = ctpp_geo_seg)[1,1],
            spat_iso_nha_nhood = OasisR::spatinteract(cbind(res_nha_est, res_nhw_est),
                                                      spatobj = ctpp_geo_seg)[1,1],
            spat_iso_h_nhood = OasisR::spatinteract(cbind(res_h_est, res_nhw_est),
                                                      spatobj = ctpp_geo_seg)[1,1],
            n_div_whood = sum(am_cat == "Multiethnic"),
            n_div_nhood = sum(pm_cat == "Multiethnic"),
            state = max(statefp),
            tot_pop = sum(res_total_est),
            log_pop = log10(sum(res_total_est)),
            tot_nhw = sum(res_nhw_est),
            tot_nhb = sum(res_nhb_est),
            tot_h = sum(res_h_est),
            tot_nha = sum(res_nha_est),
            tot_nho = sum(res_nho_est),
            tot_for_born = sum(trt_tot_for_born),
            tot_suburb = sum(res_total_est[!prin_city]),
            avg_chg_nhw = mean(chg_nhw),
            avg_chg_nhb = mean(chg_nhb),
            avg_chg_nha = mean(chg_nha),
            avg_chg_h = mean(chg_h),
            med_chg_nhw = median(chg_nhw),
            med_chg_nhb = median(chg_nhb),
            med_chg_nha = median(chg_nha),
            med_chg_h = median(chg_h),
            dis_nhb_nhw = unique(dis_nhb_nhw),
            dis_h_nhw = unique(dis_h_nhw),
            dis_nha_nhw = unique(dis_nha_nhw),
            ) %>%
  mutate(year = as.factor(year),
         met_shr_nhw = tot_nhw/tot_pop,
         met_shr_nhb = tot_nhb/tot_pop,
         met_shr_h = tot_h/tot_pop,
         met_shr_nha = tot_nha/tot_pop,
         met_shr_nho = tot_nho/tot_pop,
         met_shr_sub = tot_suburb/tot_pop,
         met_shr_for_born = tot_for_born/tot_pop,
         shr_div_whood = n_div_whood/n_tract,
         shr_non_div_whood = 1 - shr_div_whood,
         shr_div_nhood = n_div_nhood/n_tract,
         shr_non_div_nhood = 1 - shr_div_nhood) %>%
  ungroup()


#### Look at places where diverse workhoods are most prevalent -----------------

#model average workhood diversity level with Theil H
avg_div_whood_model_0 <- feols(avg_div_whood ~ year, 
                               data = avg_flow)
summary(avg_div_whood_model_0)

avg_div_whood_model_1 <- feols(avg_div_whood ~ year | cbsafp10, 
                                 data = avg_flow)
summary(avg_div_whood_model_1)

avg_div_whood_model_2 <- feols(avg_div_whood ~ year + avg_div_nhood | cbsafp10, 
                               data = avg_flow)
summary(avg_div_whood_model_2)

avg_div_whood_model_3 <- feols(avg_div_whood ~ year + avg_div_nhood + multi_theil_nhood | 
                                 cbsafp10, 
                               data = avg_flow)
summary(avg_div_whood_model_3)

avg_div_whood_model_4 <- feols(avg_div_whood ~ year + avg_div_nhood + multi_theil_nhood +
                                 log_pop + met_shr_nha + met_shr_nhb + met_shr_h + met_shr_nho +
                                 met_shr_sub + met_shr_for_born | 
                                 cbsafp10, 
                               data = avg_flow)
summary(avg_div_whood_model_4)

etable(avg_div_whood_model_1, avg_div_whood_model_2, avg_div_whood_model_3, avg_div_whood_model_4)

#save these model coefficient tables to disk
etable(avg_div_whood_model_1, avg_div_whood_model_2, avg_div_whood_model_3, avg_div_whood_model_4,
       digits = 3, digits.stats = 3,
       dict = c("year2006-2010ACS" = "Year = 2006-2010 ACS",
                "year2012-2016ACS" = "Year = 2012-2016 ACS",
                "avg_div_nhood" = "Average Neighborhood Diversity",
                "multi_theil_nhood" = "Multiethnic Information Index ($H$)",
                "log_pop" = "log(Metro Total Population)",
                "met_shr_nha" = "Metro Share NL Asian",
                "met_shr_nhb" = "Metro Share NL Black",
                "met_shr_h" = "Metro Share Latinx",
                "met_shr_nho" = "Metro Share NL Other",
                "met_shr_sub" = "Metro Share Suburban",
                "met_shr_for_born" = "Metro Share Foreign Born",
                "avg_div_whood" = "Average Workhood Diversity",
                "cbsafp10" = "Metro"),
       replace = TRUE, signifCode = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
       style.tex = style.tex(var.title = "", stats.title = " ",
                             depvar.title = "", model.title = "", model.format = "",
                             line.top = "\\toprule", line.bottom = "\\bottomrule"),
       title = "Fixed effect linear regressions of average workhood diversity",
       file = "./output/avg_div_whood_coef_table.tex")


#model average workhood diversity with xPx
avg_div_whood_model_3_iso <- feols(avg_div_whood ~ year + avg_div_nhood + iso_nhood | 
                                 cbsafp10, 
                               data = avg_flow)
summary(avg_div_whood_model_3_iso)

avg_div_whood_model_4_iso <- feols(avg_div_whood ~ year + avg_div_nhood + iso_nhood +
                                     log_pop + met_shr_nha + met_shr_nhb + met_shr_h + met_shr_nho +
                                     met_shr_sub + met_shr_for_born |
                                     cbsafp10, 
                               data = avg_flow)
summary(avg_div_whood_model_4_iso)

#save these model coefficient tables to disk
etable(avg_div_whood_model_1, avg_div_whood_model_2, avg_div_whood_model_3_iso, avg_div_whood_model_4_iso,
       digits = 3, digits.stats = 3,
       dict = c("year2006-2010ACS" = "Year = 2006-2010 ACS",
                "year2012-2016ACS" = "Year = 2012-2016 ACS",
                "avg_div_nhood" = "Average Neighborhood Diversity",
                "iso_nhood" = "Isolation Index",
                "log_pop" = "log(Metro Total Population)",
                "met_shr_nha" = "Metro Share NL Asian",
                "met_shr_nhb" = "Metro Share NL Black",
                "met_shr_h" = "Metro Share Latinx",
                "met_shr_nho" = "Metro Share NL Other",
                "met_shr_sub" = "Metro Share Suburban",
                "met_shr_for_born" = "Metro Share Foreign Born",
                "avg_div_whood" = "Average Workhood Diversity",
                "cbsafp10" = "Metro"),
       replace = TRUE, signifCode = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
       style.tex = style.tex(var.title = "", stats.title = " ",
                             depvar.title = "", model.title = "", model.format = "",
                             line.top = "\\toprule", line.bottom = "\\bottomrule"),
       title = "Fixed effect linear regressions of average workhood diversity using Isolation to measure segregation",
       file = "./output/avg_div_whood_coef_table_iso.tex")


#### Change in rank ------------------------------------------------------------

chg_rank <- avg_flow %>%
  ungroup() %>%
  filter(year == "2012-2016 ACS") %>%
  top_n(50, tot_pop) %>%
  mutate(whood_div_rank = rank(-avg_div_whood),
         nhood_div_rank = rank(-avg_div_nhood),
         chg_rank = whood_div_rank - nhood_div_rank,
         metro_name = fct_reorder(factor(metro_name), avg_div_whood)) %>%
  select(metro_name, chg_rank, avg_div_nhood, avg_div_whood, whood_div_rank, nhood_div_rank,
         spat_multi_theil_nhood) %>%
  arrange(chg_rank)

chg_rank_gg <- ggplot() +
  geom_segment(data = chg_rank, aes(y = metro_name, yend = metro_name, 
                                    x = avg_div_nhood, xend = avg_div_whood),
               lwd = .5) +
  geom_point(data = chg_rank, aes(y = metro_name, x = avg_div_nhood, 
                                  color = "Nighttime", fill = "Nighttime", shape = "Nighttime"),
             size = 3) +
  # geom_text(data = chg_rank, aes(y = metro_name, x = avg_div_nhood, label = nhood_div_rank),
  #           color = "white", size = 2.5) +
  geom_point(data = chg_rank, aes(y = metro_name, x = avg_div_whood, 
                                  color = "Daytime", fill = "Daytime", shape = "Daytime"),
             size = 3) +
  # geom_text(data = chg_rank, aes(y = metro_name, x = avg_div_whood, label = whood_div_rank),
  #           color = "black", size = 2.5) +
  scale_shape_manual(values = c(21, 22)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("white", "black")) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  labs(x = "\nAverage Tract Racial/Ethnic Diversity", y = "", color = "", shape = "", fill = "")

chg_rank_gg 

ggsave(filename = "./output/chg_rank_dotplot.pdf", chg_rank_gg,
       width = 7, height = 9, dpi = 300)


#### Change in diversity and earnings ------------------------------------------

# ctpp_earn_sum <- ctpp_earn %>%
#   group_by(year, geoid, cbsafp10) %>%
#   summarize(earn_div_diff = earn_div[type == "pow"] - earn_div[type == "res"],
#             res_earn_div = unique(earn_div[type == "res"]),
#             pow_earn_div = unique(earn_div[type == "pow"]),
#             cbsafp10 = as.character(cbsafp10)) %>%
#   ungroup() %>%
#   filter(!is.na(res_earn_div), !is.na(pow_earn_div))
# 
# ctpp <- left_join(ctpp, ctpp_earn) 

#earnings are problematic because the representation changes due to inflation
#no way to adjust given aggregated data

ggplot(ctpp_earn, aes(x = earn_div, color = year)) +
  facet_grid(~ type) +
  geom_density()


#### Association of different types of diversity change ------------------------

ctpp_mdiv_tract <- ctpp_mdiv %>%
  mutate(tot_workers = tot_sex_male + tot_sex_female,
         shr_16_24 = tot_age_16_24/tot_workers,
         shr_25_44 = tot_age_25_44/tot_workers,
         shr_45_64 = tot_age_45_64/tot_workers,
         shr_65_plus = tot_age_65_or_older/tot_workers,
         shr_male = tot_sex_male/tot_workers,
         shr_female = tot_sex_female/tot_workers,
         shr_eng_lang = tot_lang_eng/tot_workers,
         shr_span_lang = tot_lang_span/tot_workers,
         shr_oth_lang = tot_lang_oth/tot_workers,
         shr_below_pov = tot_below_100_pct_pov/tot_workers,
         shr_100_149_pov = tot_100_149_pct_pov/tot_workers,
         shr_150_plus_pov = tot_150_pct_plus_pov/tot_workers,
         
         age_div = shr_16_24 * ifelse(shr_16_24 != 0, log(1/shr_16_24), 0) +
           shr_25_44 * ifelse(shr_25_44 != 0, log(1/shr_25_44), 0) +
           shr_45_64 * ifelse(shr_45_64 != 0, log(1/shr_45_64), 0) +
           shr_65_plus * ifelse(shr_65_plus != 0, log(1/shr_65_plus), 0),
         
         sex_div = shr_male * ifelse(shr_male != 0, log(1/shr_male), 0) +
           shr_female * ifelse(shr_female != 0, log(1/shr_female), 0),
         
         lang_div = shr_eng_lang * ifelse(shr_eng_lang != 0, log(1/shr_eng_lang), 0) +
           shr_span_lang * ifelse(shr_span_lang != 0, log(1/shr_span_lang), 0) +
           shr_oth_lang * ifelse(shr_oth_lang != 0, log(1/shr_oth_lang), 0),
         
         inc_div = shr_below_pov * ifelse(shr_below_pov != 0, log(1/shr_below_pov), 0) +
           shr_100_149_pov * ifelse(shr_100_149_pov != 0, log(1/shr_100_149_pov), 0) +
           shr_150_plus_pov * ifelse(shr_150_plus_pov != 0, log(1/shr_150_plus_pov), 0)) %>%
  group_by(geoid) %>%
  summarize(res_age_div = (age_div[type == "res"]/log(4)) * 100,
            pow_age_div = (age_div[type == "pow"]/log(4)) * 100,
            res_sex_div = (sex_div[type == "res"]/log(2)) * 100,
            pow_sex_div = (sex_div[type == "pow"]/log(2)) * 100,
            res_lang_div = (lang_div[type == "res"]/log(3)) * 100,
            pow_lang_div = (lang_div[type == "pow"]/log(3)) * 100,
            res_inc_div = (inc_div[type == "res"]/log(3)) * 100,
            pow_inc_div = (inc_div[type == "pow"]/log(3)) * 100) %>%
  mutate(chg_age_div = pow_age_div - res_age_div,
         chg_sex_div = pow_sex_div - res_sex_div,
         chg_lang_div = pow_lang_div - res_lang_div,
         chg_inc_div = pow_inc_div - res_inc_div)

glimpse(ctpp_mdiv_tract)

ctpp_mdiv_tract <- inner_join(ctpp_mdiv_tract, ctpp %>% filter(year == "2012-2016 ACS")) %>%
  mutate(am_pm_cat = fct_cross(am_cat, pm_cat),
         chg_race_eth_div = race_eth_div_am - race_eth_div_pm)

chg_age_div <- lm(chg_age_div ~ am_pm_cat, 
                  data = ctpp_mdiv_tract)
summary(chg_age_div)
chg_age_div_preds <- ggpredict(chg_age_div, terms = "am_pm_cat") %>% 
  as_tibble() %>%
  mutate(am_cat = str_split_fixed(x, ":", n = 2)[, 1],
         pm_cat = str_split_fixed(x, ":", n = 2)[, 2],
         model = "Age Heterogeneity")

chg_age_div_preds$pm_cat <- factor(chg_age_div_preds$pm_cat)
chg_age_div_preds$pm_cat <- factor(chg_age_div_preds$pm_cat, levels = rev(levels(chg_age_div_preds$pm_cat)[c(6, 4, 5, 3, 7, 2, 1)]))

chg_age_div_preds$am_cat <- factor(chg_age_div_preds$am_cat)
chg_age_div_preds$am_cat <- factor(chg_age_div_preds$am_cat, levels = levels(chg_age_div_preds$pm_cat))


ggplot(chg_age_div_preds, aes(x = am_cat, y = pm_cat, fill = predicted)) +
  facet_grid(~ model) +
  coord_equal() +
  geom_tile(color = "Black") +
  #geom_label(color = "Black", fill = "white", size = 3) +
  scale_fill_gradient2(midpoint = mean(ctpp_mdiv_tract$chg_age_div)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.spacing = unit(.5, "in"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "\nDaytime Racial/Ethnic Composition", 
       y = "Nighttime Racial/Ethnic Composition\n")

#sex

chg_sex_div <- lm(chg_sex_div ~ am_pm_cat, 
                  data = ctpp_mdiv_tract)
summary(chg_sex_div)
chg_sex_div_preds <- ggpredict(chg_sex_div, terms = "am_pm_cat") %>% 
  as_tibble() %>%
  mutate(am_cat = str_split_fixed(x, ":", n = 2)[, 1],
         pm_cat = str_split_fixed(x, ":", n = 2)[, 2],
         model = "Sex Heterogeneity")

chg_sex_div_preds$pm_cat <- factor(chg_sex_div_preds$pm_cat)
chg_sex_div_preds$pm_cat <- factor(chg_sex_div_preds$pm_cat, levels = rev(levels(chg_sex_div_preds$pm_cat)[c(6, 4, 5, 3, 7, 2, 1)]))

chg_sex_div_preds$am_cat <- factor(chg_sex_div_preds$am_cat)
chg_sex_div_preds$am_cat <- factor(chg_sex_div_preds$am_cat, levels = levels(chg_sex_div_preds$pm_cat))


ggplot(chg_sex_div_preds, aes(x = am_cat, y = pm_cat, fill = predicted)) +
  facet_grid(~ model) +
  coord_equal() +
  geom_tile(color = "Black") +
  #geom_label(color = "Black", fill = "white", size = 3) +
  scale_fill_gradient2(midpoint = mean(ctpp_mdiv_tract$chg_sex_div)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.spacing = unit(.5, "in"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "\nDaytime Racial/Ethnic Composition", 
       y = "Nighttime Racial/Ethnic Composition\n")

#lang

chg_lang_div <- lm(chg_lang_div ~ am_pm_cat, 
                  data = ctpp_mdiv_tract)
summary(chg_lang_div)
chg_lang_div_preds <- ggpredict(chg_lang_div, terms = "am_pm_cat") %>% 
  as_tibble() %>%
  mutate(am_cat = str_split_fixed(x, ":", n = 2)[, 1],
         pm_cat = str_split_fixed(x, ":", n = 2)[, 2],
         model = "Language Heterogeneity")

chg_lang_div_preds$pm_cat <- factor(chg_lang_div_preds$pm_cat)
chg_lang_div_preds$pm_cat <- factor(chg_lang_div_preds$pm_cat, levels = rev(levels(chg_lang_div_preds$pm_cat)[c(6, 4, 5, 3, 7, 2, 1)]))

chg_lang_div_preds$am_cat <- factor(chg_lang_div_preds$am_cat)
chg_lang_div_preds$am_cat <- factor(chg_lang_div_preds$am_cat, levels = levels(chg_lang_div_preds$pm_cat))


ggplot(chg_lang_div_preds, aes(x = am_cat, y = pm_cat, fill = predicted)) +
  facet_grid(~ model) +
  coord_equal() +
  geom_tile(color = "Black") +
  #geom_label(color = "Black", fill = "white", size = 3) +
  scale_fill_gradient2(midpoint = mean(ctpp_mdiv_tract$chg_lang_div)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.spacing = unit(.5, "in"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "\nDaytime Racial/Ethnic Composition", 
       y = "Nighttime Racial/Ethnic Composition\n")

#inc

chg_inc_div <- lm(chg_inc_div ~ am_pm_cat, 
                   data = ctpp_mdiv_tract)
summary(chg_inc_div)
chg_inc_div_preds <- ggpredict(chg_inc_div, terms = "am_pm_cat") %>% 
  as_tibble() %>%
  mutate(am_cat = str_split_fixed(x, ":", n = 2)[, 1],
         pm_cat = str_split_fixed(x, ":", n = 2)[, 2],
         model = "Income Heterogeneity")

chg_inc_div_preds$pm_cat <- factor(chg_inc_div_preds$pm_cat)
chg_inc_div_preds$pm_cat <- factor(chg_inc_div_preds$pm_cat, levels = rev(levels(chg_inc_div_preds$pm_cat)[c(6, 4, 5, 3, 7, 2, 1)]))

chg_inc_div_preds$am_cat <- factor(chg_inc_div_preds$am_cat)
chg_inc_div_preds$am_cat <- factor(chg_inc_div_preds$am_cat, levels = levels(chg_inc_div_preds$pm_cat))


ggplot(chg_inc_div_preds, aes(x = am_cat, y = pm_cat, fill = predicted)) +
  facet_grid(~ model) +
  coord_equal() +
  geom_tile(color = "Black") +
  #geom_label(color = "Black", fill = "white", size = 3) +
  scale_fill_gradient2(midpoint = mean(ctpp_mdiv_tract$chg_inc_div)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.spacing = unit(.5, "in"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "\nDaytime Racial/Ethnic Composition", 
       y = "Nighttime Racial/Ethnic Composition\n")

chg_div_preds <- bind_rows(chg_age_div_preds, chg_sex_div_preds,
                           chg_lang_div_preds, chg_inc_div_preds)

chg_div_fig <- ggplot(chg_div_preds, aes(x = am_cat, y = pm_cat, fill = predicted)) +
  facet_wrap(~ model) +
  coord_equal() +
  geom_tile(color = "Black") +
  #geom_label(color = "Black", fill = "white", size = 3) +
  scale_fill_gradient2(midpoint = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.spacing = unit(.25, "in"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "bottom",
        legend.key.width = unit(.75, "in"),
        legend.key.height = unit(.1, "in")) +
  labs(x = "\nDaytime Racial/Ethnic Composition", 
       y = "Nighttime Racial/Ethnic Composition\n",
       fill = "Average Intraday Change ")

chg_div_fig

ggsave(filename = "./output/chg_mdiv_am_pm_cat.pdf", chg_div_fig,
       width = 8, height = 8, dpi = 300)


#### Correlation of racial/ethnic change with nonracial heterogeneity ----------

library(margins)

tract_2010_area <- tract_2010_shp %>%
  st_drop_geometry() %>%
  mutate(geoid = GEOID10, 
         aland_mi2 = ALAND10/2589988,
         log_aland_mi2 = log(aland_mi2))

ctpp_mdiv_tract <- ctpp_mdiv_tract %>%
  left_join(tract_2010_area) %>%
  mutate_at(vars(chg_race_eth_div, chg_age_div, chg_sex_div, chg_lang_div, chg_inc_div),
            scale) %>%
  mutate(trt_tot_pop_1k = trt_tot_pop/1000)

div_mdiv_lm <- feols(chg_race_eth_div ~ chg_age_div + chg_sex_div + chg_lang_div + chg_inc_div +
                      trt_tot_pop_1k + log_aland_mi2 | cbsafp10, 
                     ctpp_mdiv_tract)

summary(div_mdiv_lm)

nhb_mdiv_lm <- feols(chg_nhb ~ chg_age_div + chg_sex_div + chg_lang_div + chg_inc_div +
                      trt_tot_pop_1k + log_aland_mi2 | cbsafp10, 
                     ctpp_mdiv_tract)

summary(nhb_mdiv_lm)

nhw_mdiv_lm <- feols(chg_nhw ~ chg_age_div + chg_sex_div + chg_lang_div + chg_inc_div +
                      trt_tot_pop_1k + log_aland_mi2 | cbsafp10, 
                     ctpp_mdiv_tract)

summary(nhw_mdiv_lm)

h_mdiv_lm <- feols(chg_h ~ chg_age_div + chg_sex_div + chg_lang_div + chg_inc_div +
                    trt_tot_pop_1k + log_aland_mi2 | cbsafp10, 
                   ctpp_mdiv_tract)

summary(h_mdiv_lm)

nha_mdiv_lm <- feols(chg_nha ~ chg_age_div + chg_sex_div + chg_lang_div + chg_inc_div +
                      trt_tot_pop_1k + log_aland_mi2 | cbsafp10, 
                     ctpp_mdiv_tract)

summary(nha_mdiv_lm)

etable(div_mdiv_lm, nhw_mdiv_lm, nhb_mdiv_lm, h_mdiv_lm, nha_mdiv_lm,
       tex = TRUE, replace = TRUE, 
       file = "./output/mdiv_coef_table.tex", 
       title = "Linear regressions of intraday change in racial and ethnic diversity and composition on nonracial intraday change, 2012-2016 ACS",
       dict = c("chg_age_div" = "Age Diversity Change",
                "chg_sex_div" = "Sex Diversity Change",
                "chg_lang_div" = "Language Diversity Change",
                "chg_inc_div" = "Income Diversity Change",
                "trt_tot_pop_1k" = "Tract Population (1000s)",
                "log_aland_mi2" = "Logged Tract Area (mi^2)",
                "cbsafp10" = "Metro",
                "chg_race_eth_div" = "Chg. Ethnoracial Div.",
                "chg_nhw" = "Chg. NL White",
                "chg_nhb" = "Chg. NL Black",
                "chg_h" = "Chg. Latinx",
                "chg_nha" = "Chg. NL Asian"),
       digits = 3, digits.stats = 3,
       signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
       style.tex = style.tex(var.title = "", stats.title = " ",
                             depvar.title = "", model.title = "", model.format = "",
                             line.top = "\\toprule", line.bottom = "\\bottomrule"))




