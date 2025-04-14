rm(list=ls())
source("./code/function_wrangle_data.r")
source("./code/function_model_search.r")
source("./code/function_run_models.r")
source("./code/function_model_exploration.r")

require(sdmTMB)
library(tidyr)
library(dplyr)


load("./output/temporal_output_Spwn_2021.rData")
spwn <- output
load("./output/temporal_output_rear_2021.rData")
rear <- output


#Juveniles
rear_df <- rear$exploratory$sdm$best_fit$data
rear_quant <- quantile(rear_df$dens, probs = c(0.05, 0.5, 0.95))
mesh <- sdmTMB::make_mesh(rear_df[rear_df$yr <= 2021,], 
                          xy_cols = c("UTM_E_km","UTM_N_km"), cutoff = 10)
rear_fit <- sdmTMB(rear$exploratory$sdm$best_mod, 
                   data = rear_df,
                   mesh = mesh,
                   family = tweedie(link = "log"),
                   time = "yr",
                   spatial = rear$exploratory$sdm$best_sp,
                   spatiotemporal = rear$exploratory$sdm$best_st,
                   anisotropy = TRUE,
                   # extra_time = unique(obs_data$yr[obs_data$yr <= yr_i]), #Why is this necessary if the years are the same?
                   silent=TRUE)

rear_pred <- predict(rear_fit)
rear_df$residuals <- residuals(rear_fit)

g <- ggplot(rear_pred, aes(UTM_E_km, UTM_N_km, col = omega_s)) + 
  scale_color_viridis_c() +
  geom_point(size = 2.5) + 
  # facet_wrap(~yr) +
  coord_fixed() + 
  theme_bw() +
  ylab("Northing (km)") + 
  ylab("Easting (km)") + 
  theme(panel.grid = element_blank()) +
  labs(fill = "Spaital + \nSpatiotemporal effects",
       color = "Spaital \neffects")

ggsave(g, file = "./output/Supplemental_Fig_2.tiff", dpi = 300, width = 8, height = 10, units = "in")

g <- ggplot(rear_pred, aes(UTM_E_km, UTM_N_km, col = epsilon_st)) + 
  scale_color_viridis_c() +
  geom_point(size = 1.5) + 
  facet_wrap(~yr, ncol = 6) +
  coord_fixed() + 
  theme_bw() +
  ylab("Northing (km)") + 
  ylab("Easting (km)") + 
  theme(panel.grid = element_blank()) +
  labs(fill = "Spatio-\ntemporal effects",
       color = "Spatio-\ntemporal \neffects")

ggsave(g, file = "./output/Supplemental_Fig_3.tiff", dpi = 300, width = 8, height = 10, units = "in")

#Spawners
spwn_df <- spwn$exploratory$sdm$best_fit$data
spwn_quant <- quantile(spwn_df$dens, probs = c(0.05, 0.5, 0.95))
mesh <- sdmTMB::make_mesh(spwn_df[spwn_df$yr <= 2021,], 
                          xy_cols = c("UTM_E_km","UTM_N_km"), cutoff = 10)
spwn_fit <- sdmTMB(spwn$exploratory$sdm$best_mod, 
                   data = spwn_df,
                   mesh = mesh,
                   family = tweedie(link = "log"),
                   time = "yr",
                   spatial = spwn$exploratory$sdm$best_sp,
                   spatiotemporal = spwn$exploratory$sdm$best_st,
                   anisotropy = TRUE,
                   # extra_time = unique(df$yr[df$yr <= 2021]), #Why is this necessary if the years are the same?
                   silent=TRUE)

spwn_pred <- predict(spwn_fit)

g <- ggplot(spwn_pred, aes(UTM_E_km, UTM_N_km, col = epsilon_st)) + 
  scale_color_viridis_c() +
  geom_point(size = 1.) + 
  facet_wrap(~yr, ncol = 6) +
  coord_fixed() + 
  theme_bw() +
  ylab("Northing (km)") + 
  ylab("Easting (km)") + 
  theme(panel.grid = element_blank()) +
  labs(fill = "Spatiotemporal effects",
       color = "Spatio-\ntemporal \neffects")

print(g)
ggsave(g, file = "./output/Supplemental_Fig_1.tiff", dpi = 300, width = 8, height = 10, units = "in")
  