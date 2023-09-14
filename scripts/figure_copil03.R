x11(); plot(sxt_sub$mean.sst)
x11(); plot(sxt_sub$stdv.sst)
x11(); plot(hab_sub$stdv.sst)


ggplot() +
  geom_sf(data = maps$GLP) +
  geom_sf(
    data = bio %>% st_crop(st_bbox(maps$GLP)),
    aes(col = type, alpha = type)
  ) +
  scale_color_manual(
    values = c("pink2", "blue"), labels = c("Absence", "Présence")
  ) +
  scale_alpha_manual(values = c(0.6, 1), labels = c("Absence", "Présence")) +
  theme_map() +
  theme(legend.title = element_blank())
