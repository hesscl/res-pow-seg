seattle <- ctpp %>% filter(cbsafp == "42660", year %in% c("2000", "2012-2016"))
san_fran <- ctpp %>% filter(cbsafp == "41860", year %in% c("2000", "2012-2016"))
wash_dc <- ctpp %>% filter(cbsafp == "47900", year %in% c("2000", "2012-2016"))

chicago <- ctpp %>% filter(cbsafp == "16980", year %in% c("2000", "2012-2016"))
philly <- ctpp %>% filter(cbsafp == "37980", year %in% c("2000", "2012-2016"))
detroit <- ctpp %>% filter(cbsafp == "19820", year %in% c("2000", "2012-2016"))

chicago_gg <- ggplot(chicago, aes(fill = mult_pm_am)) +
  facet_grid(~ year) +
  geom_sf(color = NA) +
  theme(legend.position = "bottom") 

chicago_gg

philly_gg <- ggplot(philly, aes(fill = mult_pm_am)) +
  facet_grid(~ year) +
  geom_sf(color = NA) +
  theme(legend.position = "bottom") 

philly_gg

wash_dc_gg <- ggplot(wash_dc, aes(fill = mult_pm_am)) +
  facet_grid(~ year) +
  geom_sf(color = NA) +
  theme(legend.position = "bottom") 

wash_dc_gg

seattle_gg <- ggplot(seattle, aes(fill = mult_pm_am)) +
  facet_grid(~ year) +
  geom_sf(color = NA) +
  theme(legend.position = "bottom") 

seattle_gg

san_fran_gg <- ggplot(san_fran, aes(fill = mult_pm_am)) +
  facet_grid(~ year) +
  geom_sf(color = NA) +
  theme(legend.position = "bottom") 

san_fran_gg


chicago_seg_gg <- ggplot(chicago, aes(fill = predom_pm_am)) +
  facet_grid(~ year) +
  geom_sf(color = NA) +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(7, "RdBu")[c(2, 3, 5, 6)])) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(fill = "", title = "Intraday Change for Segregated Compositions (i.e. >= 75% one group) in Chicago Metro Area")

chicago_seg_gg
ggsave(filename = "./output/choro/chicago_predom_pm_am_choro.pdf", chicago_seg_gg,
       width = 14, height = 10, dpi = 300)

philly_seg_gg <- ggplot(philly, aes(fill = predom_pm_am)) +
  facet_grid(~ year) +
  geom_sf(color = NA) +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(7, "RdBu")[c(2, 3, 5, 6)])) +
  theme_void() +
  theme(legend.position = "bottom") 

philly_seg_gg
ggsave(filename = "./output/choro/philly_predom_pm_am_choro.pdf", philly_seg_gg,
       width = 14, height = 10, dpi = 300)

detroit_seg_gg <- ggplot(detroit, aes(fill = predom_pm_am)) +
  facet_grid(~ year) +
  geom_sf(color = NA) +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(7, "RdBu")[c(2, 3, 5, 6)])) +
  theme_void() +
  theme(legend.position = "bottom") 

detroit_seg_gg
ggsave(filename = "./output/choro/detroit_predom_pm_am_choro.pdf", detroit_seg_gg,
       width = 14, height = 10, dpi = 300)

seattle_seg_gg <- ggplot(seattle, aes(fill = predom_pm_am)) +
  facet_grid(~ year) +
  geom_sf(color = NA) +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(7, "RdBu")[c(2, 3, 5, 6)])) +
  theme_void() +
  theme(legend.position = "bottom") 

seattle_seg_gg
ggsave(filename = "./output/choro/seattle_predom_pm_am_choro.pdf", seattle_seg_gg,
       width = 14, height = 10, dpi = 300)

san_fran_seg_gg <- ggplot(san_fran, aes(fill = predom_pm_am)) +
  facet_grid(~ year) +
  geom_sf(color = NA) +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(7, "RdBu")[c(2, 3, 5, 6)])) +
  theme_void() +
  theme(legend.position = "bottom") 

san_fran_seg_gg
ggsave(filename = "./output/choro/san_fran_predom_pm_am_choro.pdf", san_fran_seg_gg,
       width = 14, height = 10, dpi = 300)

wash_dc_seg_gg <- ggplot(wash_dc, aes(fill = predom_pm_am)) +
  facet_grid(~ year) +
  geom_sf(color = NA) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(7, "RdBu")[c(2, 3, 5, 6)]) +
  theme_void() +
  theme(legend.position = "bottom") 

wash_dc_seg_gg
ggsave(filename = "./output/choro/wash_dc_predom_pm_am_choro.pdf", wash_dc_seg_gg,
       width = 14, height = 10, dpi = 300)











ggsave(filename = "./output/seattle_pm_am_res.pdf", seattle_gg,
       width = 12, height = 10, dpi = 300)
