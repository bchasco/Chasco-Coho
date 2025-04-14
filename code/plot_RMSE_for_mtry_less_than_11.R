load('./output/temporal_output_rear_2021.rData')
# load('./output/temporal_output_Spwn_2021.rData')
tmp <- (output$exploratory$rf$grid_search[output$exploratory$rf$grid_search$mtry<=6,])
library(dplyr)
library(tidyr)
mean_rmse <- output$exploratory$rf$grid_search %>% 
  group_by(mod = mod,
           mtry = mtry,
           ntree = ntree) %>%
  summarise(mean = mean(rmse))

#Best model
mean_rmse[mean_rmse$mean == min(mean_rmse$mean),]

#mean for realistic models
mean(mean_rmse$mean[mean_rmse$mtry < 7])  
