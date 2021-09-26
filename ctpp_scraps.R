ctpp %>%
  group_by(year) %>%
  tally()

intraday_chg <- ctpp %>%
  group_by(year, pm_cat, am_cat) %>%
  tally() %>%
  group_by(year, pm_cat) %>%
  mutate(shr = n/sum(n),
         total = sum(n))
intraday_chg

intraday_chg_gg <- ggplot(intraday_chg %>% filter(total > 1000), 
                          aes(x = year, y = shr, fill = am_cat)) +
  facet_wrap(~ pm_cat) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(fill = "Daytime Racial/Ethnic Composition")

intraday_chg_gg

ggsave(filename = "./output/intraday_chg_stacked_bar.pdf", intraday_chg_gg,
       width = 8, height = 6, dpi = 300)

intraday_chg %>%
  pivot_wider(id_cols = c(year, pm_cat, total),
              names_from = am_cat, values_from = c(n, shr)) %>%
  select(year, pm_cat, total, starts_with("shr")) %>%
  arrange(pm_cat, year)








intraday_chg_seg <- ctpp %>%
  group_by(year, cbsafp) %>%
  mutate(dis_nhb_nhw = (.5) * sum(abs(trt_tot_nhb/sum(trt_tot_nhb) - 
                                        trt_tot_nhw/sum(trt_tot_nhw)))) %>%
  ungroup() %>%
  mutate(seg_cat = case_when(dis_nhb_nhw >= .8 ~ "Extreme Segregation",
                             dis_nhb_nhw < .8 & dis_nhb_nhw >= .6 ~ "High Segregation",
                             dis_nhb_nhw < .6 & dis_nhb_nhw >= .4 ~ "Moderate Segregation",
                             dis_nhb_nhw < .4 ~ "Low Segregation"),
         seg_cat = fct_reorder(as.factor(seg_cat), .x = dis_nhb_nhw)) %>%
  group_by(year, seg_cat, pm_cat, am_cat) %>%
  tally() %>%
  group_by(year, seg_cat, pm_cat) %>%
  mutate(shr = n/sum(n),
         total = sum(n))

intraday_chg_seg_gg <- ggplot(intraday_chg_seg %>% 
                                filter(seg_cat != "Low Segregation",
                                       pm_cat != "Asian"), 
                              aes(x = year, y = shr, fill = am_cat)) +
  facet_grid(seg_cat ~ pm_cat) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(fill = "Daytime Racial/Ethnic Composition")

intraday_chg_seg_gg

ggsave(filename = "./output/intraday_chg_seg_stacked_bar.pdf", intraday_chg_seg_gg,
       width = 12, height = 8, dpi = 300)



avg_comp <- ctpp %>%
  group_by(year, am_cat) %>%
  summarize(nhw = mean(trt_shr_nhw_am, na.rm = TRUE),
            nhb = mean(trt_shr_nhb_am, na.rm = TRUE),
            h = mean(trt_shr_h_am, na.rm = TRUE),
            nha = mean(trt_shr_nha_am, na.rm = TRUE),
            nho = mean(trt_shr_nho_am, na.rm = TRUE)) 
avg_comp















