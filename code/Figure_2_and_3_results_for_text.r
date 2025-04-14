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
df <- function_wrangle_data(stage='rear',
                            dir = getwd(),
                            maxYr = 2021)

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

print(summary(rear_fit))
print(summary(spwn_fit))
