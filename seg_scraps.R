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
  mutate(seg = "Asian-White")


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

ggsave(filename = "./output/etc/seg_trends.pdf", seg_trends_gg,
       width = 8, height = 6, dpi = 300)
