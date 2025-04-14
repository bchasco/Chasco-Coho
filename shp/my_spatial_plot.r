myYear <- c(1998,2009,2019)
g1 <- ggplot() + 
  theme_bw() +
  coord_cartesian(xlim = c(min(coastXY$X)*0.95, max(coastXY$X)*1.05)) +
  # geom_polygon(data=or_xy, aes(x=X/1000, y=Y/1000), color="black", fill="lightgrey")+ #Map
  # geom_raster(data=p[p$JuvYr%in%(seq(1998,2019,3)),], aes(UTM_E_km, UTM_N_km, fill = (omega_s)), inherit.aes = FALSE) +
  geom_raster(data=p[p$JuvYr%in%myYear,], aes(UTM_E_km, UTM_N_km, fill = exp(est)), inherit.aes = FALSE) +
  facet_wrap(~JuvYr, nrow = length(myYear)) +
  # geom_polygon(data=coastXY,
  #              aes(x=X, y=Y), color="black", fill="transparent", inherit.aes = FALSE)+ #Map
  scale_fill_viridis_c() +
  ylab("Northing (km)") + 
  xlab("Easting (km)")

g2 <- ggplot() + 
  theme_bw() +
  coord_cartesian(xlim = c(min(coastXY$X)*0.95, max(coastXY$X)*1.05)) +
  geom_polygon(data=or_xy, aes(x=X/1000, y=Y/1000), color="black", fill="lightgrey")+ #Map
  # geom_raster(data=p[p$JuvYr%in%(seq(1998,2019,3)),], aes(UTM_E_km, UTM_N_km, fill = (omega_s)), inherit.aes = FALSE) +
  geom_raster(data=p[p$JuvYr%in%myYear,], aes(UTM_E_km, UTM_N_km, fill = (omega_s)), inherit.aes = FALSE) +
  facet_wrap(~JuvYr, nrow = length(myYear)) +
  # geom_polygon(data=coastXY,
  #              aes(x=X, y=Y), color="black", fill="transparent", inherit.aes = FALSE)+ #Map
  scale_fill_viridis_c() +
  ylab("Northing (km)") + 
  xlab("Easting (km)")

g3 <- ggplot() + 
  theme_bw() +
  coord_cartesian(xlim = c(min(coastXY$X)*0.95, max(coastXY$X)*1.05)) +
  geom_polygon(data=or_xy, aes(x=X/1000, y=Y/1000), color="black", fill="lightgrey")+ #Map
  # geom_raster(data=p[p$JuvYr%in%(seq(1998,2019,3)),], aes(UTM_E_km, UTM_N_km, fill = (omega_s)), inherit.aes = FALSE) +
  geom_raster(data=p[p$JuvYr%in%myYear,], aes(UTM_E_km, UTM_N_km, fill = (epsilon_st)), inherit.aes = FALSE) +
  facet_wrap(~JuvYr, nrow = length(myYear)) +
  # geom_polygon(data=coastXY,
  #              aes(x=X, y=Y), color="black", fill="transparent", inherit.aes = FALSE)+ #Map
  scale_fill_viridis_c() +
  ylab("Northing (km)") + 
  xlab("Easting (km)")

cowplot::plot_grid(g1, g2, g3, ncol=3, labels = c('est', 'omega', 'epsilon'))
