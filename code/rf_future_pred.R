library(randomForest)
library(dplyr)

d = read.csv("JuvData.csv")
d = dplyr::filter(d, !is.na(MWMT_Index))
d$STRM_ORDER = as.factor(d$STRM_ORDER)
d$CLASS_Rank = as.factor(d$CLASS_Rank)

n_years_ahead = 0# can be 0, 1, 2
# use avg predictions for last 5 years to train model
n_test = 5
test_years = seq(max(d$JuvYr)-n_test+1, max(d$JuvYr))

grid_search = expand.grid(mtry = seq(3,15,2), ntree = seq(200,1000,100),
                          test_years = test_years, rmse=0)

# find model with lowest out of sample rmse
rf_pred = list()
for(i in 1:nrow(grid_search)) {
  
  train = dplyr::filter(d, JuvYr < (grid_search$test_years[i] - n_years_ahead + 1))  
  test = dplyr::filter(d, JuvYr == grid_search$test_years[i]) 
  fit = randomForest(Juv.km ~ STRM_ORDER + StrmSlope + 
                       #MaxGradD + 
                       WidthM +
                       OUT_DIST + 
                       #CLASS_Rank + 
                       StrmPow + 
                       #MAnnSed + 
                       #Barriers + 
                       MWMT_Index + 
                       SolMean + 
                       W3Dppt + 
                       SprPpt + 
                       IP_COHO +
                       UTM_E + 
                       UTM_N + 
                       JuvYr, 
                     mtry = grid_search$mtry[i],
                     ntree = grid_search$ntree[i],
                     data=train)
  rf_pred[[i]] = predict(fit, test)
  grid_search$rmse[i] = sqrt(mean((rf_pred[[i]] - test$Juv.km)^2))
  
}

# saveRDS(grid_search,paste0("output/rf_",n_years_ahead,"yr.rds"))
# 
dplyr::group_by(grid_search,mtry,ntree) %>% 
  dplyr::summarize(mean_rmse=mean(rmse)) %>% 
  dplyr::arrange(mean_rmse)
# for 0 step ahead, best model is 11/ 600, rmse ~ 153
# for 1 step ahead, best model is mtry=3 / ntree = 900, and rmse = 350.2042
# for 2 step ahead, best model is mtry=3 / ntree = 600, and rmse = 349.9797