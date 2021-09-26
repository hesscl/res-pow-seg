
#prep the data we'll use for plotting
intraday_preds <- intraday %>%
  select(race_eth, ols_margins_city_sub) %>%
  unnest(ols_margins_city_sub) %>%
  select(prin_city = facet, fct_year = x, pred = predicted, lower = conf.low, upper = conf.high, pm_cat = group, race_eth) %>%
  filter(fct_year %in% c("2000", "2012-2016"))

#make a set of plots
pred_blk_lat_gg <- ggplot(intraday_preds %>% filter(pm_cat %in% c("Predominantly Black", 
                                                                  "Predominantly Latinx", "Predominantly Asian")), 
                          aes(y = race_eth, x = pred, xmin = lower, xmax = upper, fill = fct_rev(fct_year))) +
  facet_grid(prin_city ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = "dodge") +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "",
       subtitle = "Model: Ordinary Least Squares, No Metro FE")

pred_wht_mxd_gg <- ggplot(intraday_preds %>% filter(pm_cat %in% c("Predominantly White", "White-Mixed")), 
                          aes(y = race_eth, x = pred, xmin = lower, xmax = upper, fill = fct_rev(fct_year))) +
  facet_grid(prin_city ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = "dodge") +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "",
       subtitle = "Model: Ordinary Least Squares, No Metro FE")

pred_mlt_gg <- ggplot(intraday_preds %>% filter(pm_cat %in% c("Multiethnic", "Non-White-Mixed")), 
                      aes(y = race_eth, x = pred, xmin = lower, xmax = upper, fill = fct_rev(fct_year))) +
  facet_grid(prin_city ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = "dodge") +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "",
       subtitle = "Model: Ordinary Least Squares, No Metro FE")



#print them to screen
pred_blk_lat_gg
pred_wht_mxd_gg
pred_mlt_gg

#save them to disk
ggsave(filename = "./output/ols_intraday_chg_blk_lat_city_sub.pdf", pred_blk_lat_gg,
       width = 10, height = 6, dpi = 300)
ggsave(filename = "./output/ols_intraday_chg_wht_mxd_city_sub.pdf", pred_wht_mxd_gg,
       width = 10, height = 6, dpi = 300)
ggsave(filename = "./output/ols_intraday_chg_mlt_city_sub.pdf", pred_mlt_gg,
       width = 6, height = 6, dpi = 300)









































