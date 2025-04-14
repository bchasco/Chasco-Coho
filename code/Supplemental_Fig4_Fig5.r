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
df <- rear$exploratory$sdm$best_fit$data
rear_quant <- quantile(df$dens, probs = c(0.05, 0.5, 0.95))
mesh <- sdmTMB::make_mesh(df[df$yr <= 2021,], 
                          xy_cols = c("UTM_E_km","UTM_N_km"), cutoff = 10)
rear_fit <- sdmTMB(rear$exploratory$sdm$best_mod, 
                   data = df,
                   mesh = mesh,
                   family = tweedie(link = "log"),
                   time = "yr",
                   spatial = rear$exploratory$sdm$best_sp,
                   spatiotemporal = rear$exploratory$sdm$best_sp,
                   anisotropy = TRUE,
                   # extra_time = unique(obs_data$yr[obs_data$yr <= yr_i]), #Why is this necessary if the years are the same?
                   silent=TRUE)

#Spawners
df <- spwn$exploratory$sdm$best_fit$data
spwn_quant <- quantile(df$dens, probs = c(0.05, 0.5, 0.95))
mesh <- sdmTMB::make_mesh(df[df$yr <= 2021,], 
                          xy_cols = c("UTM_E_km","UTM_N_km"), cutoff = 10)
spwn_fit <- sdmTMB(spwn$exploratory$sdm$best_mod, 
                   data = df,
                   mesh = mesh,
                   family = tweedie(link = "log"),
                   time = "yr",
                   spatial = spwn$exploratory$sdm$best_sp,
                   spatiotemporal = spwn$exploratory$sdm$best_st,
                   anisotropy = TRUE,
                   extra_time = unique(df$yr[df$yr <= 2021]), #Why is this necessary if the years are the same?
                   silent=TRUE)

g <- plot_anisotropy(spwn_fit) + theme_bw()
ggsave(g, file = "./output/Supplemental_Fig_4.tiff", dpi = 300, width = 6, height = 4, units = "in")
g <- plot_anisotropy(rear_fit) + theme_bw()
ggsave(g, file = "./output/Supplemental_Fig_5.tiff", dpi = 300, width = 6, height = 4, units = "in")
