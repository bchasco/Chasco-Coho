stage <- "rear"
load(paste0("output/output_",stage,".rData"))

#Capture the best model for each model
rf <- output[['exploratory']][['rf']]$grid_search %>%
  group_by(n_years_ahead, mod, mtry, ntree) %>%
  summarise(mean = mean(rmse)) %>%
  filter(mean < 1e6, mod == 6) %>%
  arrange(mean)

gam <- output[['exploratory']][['gam']]$grid_search %>%
  group_by(n_years_ahead, mod )%>%
  summarise(mean = mean(rmse)) %>%
  filter(mean < 1e6, mod == 5) %>%
  arrange(mean)

sdm <- output[['exploratory']][['sdm']]$grid_search %>%
  group_by(n_years_ahead, mod, sp, st)%>%
  summarise(mean = mean(rmse)) %>%
  filter(mean < 1e6, mod ==2) %>%
  arrange(mean)


stage <- "Spwn"
load(paste0("output/output_",stage,".rData"))


#Capture the best model for each model
rf <- output[['exploratory']][['rf']]$grid_search %>%
  group_by(n_years_ahead, mod, mtry, ntree) %>%
  summarise(mean = mean(rmse)) %>%
  filter(mean < 1e6, mod == 6) %>%
  arrange(mean)

gam <- output[['exploratory']][['gam']]$grid_search %>%
  group_by(n_years_ahead, mod )%>%
  summarise(mean = mean(rmse)) %>%
  filter(mean < 1e6, mod == 5) %>%
  arrange(mean)

sdm <- output[['exploratory']][['sdm']]$grid_search %>%
  group_by(n_years_ahead, mod, sp, st)%>%
  summarise(mean = mean(rmse)) %>%
  filter(mean < 1e6, mod ==2) %>%
  arrange(mean)

print(rf)
print(gam)
print(sdm)
