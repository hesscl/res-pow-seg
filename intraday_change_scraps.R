#make a set of plots
pred_blk_lat_asi_gg <- ggplot(intraday_preds %>% filter(pm_cat %in% c("Predominantly Black", 
                                                                      "Predominantly Latinx", "Predominantly Asian")), 
                              aes(y = race_eth, x = pred, xmin = lower, xmax = upper, 
                                  fill = fct_rev(fct_year))) +
  facet_grid(race_eth ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = position_dodge(width = 1), height  = .5) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(-.5, .5), labels = scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "", 
       subtitle = "Model: Ordinary Least Squares, No Metro FE")

#make a set of plots
pred_blk_lat_asi_fe_gg <- ggplot(intraday_preds_fe %>% 
                                   filter(pm_cat %in% c("Predominantly Black", "Predominantly Latinx", 
                                                        "Predominantly Asian", "Predominantly White")), 
                                 aes(y = race_eth, x = pred, xmin = lower, xmax = upper, 
                                     fill = fct_rev(fct_year))) +
  facet_grid(race_eth ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = position_dodge(width = 1), height  = .5) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(-.5, .5), labels = scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "", 
       subtitle = "Model: Ordinary Least Squares, Metro FE")


pred_wht_mxd_gg <- ggplot(intraday_preds %>% filter(pm_cat %in% c("Predominantly White", "White-Mixed")), 
                          aes(y = race_eth, x = pred, xmin = lower, xmax = upper, fill = fct_rev(fct_year))) +
  facet_grid(race_eth ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = position_dodge(width = 1), height  = .5) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(-.5, .5), labels = scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "",
       subtitle = "Model: Ordinary Least Squares, No Metro FE")

pred_wht_mxd_fe_gg <- ggplot(intraday_preds_fe %>% filter(pm_cat %in% c("Predominantly White", "White-Mixed")), 
                             aes(y = race_eth, x = pred, xmin = lower, xmax = upper, fill = fct_rev(fct_year))) +
  facet_grid(race_eth ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = position_dodge(width = 1), height  = .5) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(-.5, .5), labels = scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "",
       subtitle = "Model: Ordinary Least Squares, Metro FE")

pred_wht_mxd_wls_gg <- ggplot(intraday_preds_fe %>% filter(pm_cat %in% c("Predominantly White", "White-Mixed")), 
                              aes(y = race_eth, x = pred, xmin = lower, xmax = upper, fill = fct_rev(fct_year))) +
  facet_grid(race_eth ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = position_dodge(width = 1), height  = .5) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(-.5, .5), labels = scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "",
       subtitle = "Model: Weighted Least Squares, includes Metro FE")

pred_mlt_gg <- ggplot(intraday_preds %>% filter(pm_cat %in% c("Multiethnic", "Non-White-Mixed")), 
                      aes(y = race_eth, x = pred, xmin = lower, xmax = upper, fill = fct_rev(fct_year))) +
  facet_grid(race_eth ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = position_dodge(width = 1), height  = .5) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(-.5, .5), labels = scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "",
       subtitle = "Model: Ordinary Least Squares, No Metro FE")

pred_mlt_fe_gg <- ggplot(intraday_preds_fe %>% filter(pm_cat %in% c("Multiethnic", "Non-White-Mixed")), 
                         aes(y = race_eth, x = pred, xmin = lower, xmax = upper, fill = fct_rev(fct_year))) +
  facet_grid(race_eth ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = position_dodge(width = 1), height  = .5) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(-.5, .5), labels = scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "",
       subtitle = "Model: Ordinary Least Squares, Metro FE")

pred_mlt_wls_gg <- ggplot(intraday_preds_wls %>% filter(pm_cat %in% c("Multiethnic", "Non-White-Mixed")), 
                          aes(y = race_eth, x = pred, xmin = lower, xmax = upper, fill = fct_rev(fct_year))) +
  facet_grid(race_eth ~ pm_cat, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(position = position_dodge(width = 1), height  = .5) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(-.5, .5), labels = scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "\nPredicted Intraday Change", y = "", fill = "",
       subtitle = "Model: Weighted Least Squares, includes Metro FE")



#print them to screen
pred_blk_lat_asi_gg
pred_wht_mxd_gg
pred_mlt_gg


pred_blk_lat_asi_fe_gg
pred_wht_mxd_fe_gg
pred_mlt_fe_gg