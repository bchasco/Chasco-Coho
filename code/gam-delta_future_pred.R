library(mgcv)
library(dplyr)
library(pROC)
d = read.csv("JuvData.csv")
d = dplyr::filter(d, !is.na(MWMT_Index))
d$STRM_ORDER = as.factor(d$STRM_ORDER)
d$CLASS_Rank = as.numeric(d$CLASS_Rank)


for(tt in 1:3) {
  
n_years_ahead = tt - 1# can be 0, 1, 2
# use avg predictions for last 5 years to train model
n_test = 5
test_years = seq(max(d$JuvYr)-n_test+1, max(d$JuvYr))


grid_search = expand.grid(test_years = test_years, rmse=0,auc=0,
                          year = c("JuvYr","s(JuvYr)",""),
                          space = c("s(UTM_E, UTM_N)","s(UTM_E, UTM_N,JuvYr)",""),
                          req = "s(MWMT_Index,k=4) + s(W3Dppt,k=4) + s(SprPpt,k=4) + s(IP_COHO,k=4)",
                          sol = c("s(SolMean,k=4)",""),
                          strm = c("STRM_ORDER"),
                          rnk = c("s(CLASS_Rank,k=3)",""),
                          cov1 = c("s(StrmSlope,k=4)"),
                          cov2 = c("s(MaxGradD,k=4)"),
                          cov3 = c("s(WidthM,k=4)",""),
                          cov4 = c("s(OUT_DIST,k=4)",""),
                          cov5 = c("s(StrmPow,k=4)",""),
                          cov6 = c("s(MAnnSed,k=4)",""),
                          cov7 = c("s(Barriers,k=4)",""),
                          stringsAsFactors = FALSE)

# find model with lowest out of sample rmse
gam_pred = list()
for(i in 1:nrow(grid_search)) {
 
   train = dplyr::filter(d, JuvYr < (grid_search$test_years[i] - n_years_ahead + 1))  
   test = dplyr::filter(d, JuvYr == grid_search$test_years[i]) 
   
   train$present = as.factor(ifelse(train$Juv.km>0,1,0))
   test$present = as.factor(ifelse(test$Juv.km>0,1,0))
   train$log_juv.km = log(train$Juv.km)
   
   indx = which(grid_search[i,] =="")
   formula = paste("present", paste(grid_search[i,-c(1:8,indx)], collapse=" + "), sep=" ~ ")
   # This returns the formula:
   #as.formula(formula)
   fit = gam(as.formula(formula),
             family = "binomial",
             data = train)
   
   gam_pred[[i]] = predict(fit, test,type="response")
   #grid_search$rmse[i] = sqrt(mean((gam_pred[[i]] - test$Juv.km)^2))
   grid_search$auc[i] = as.numeric(auc(test$present, gam_pred[[i]]))

  indx = which(grid_search[i,] =="")
  formula = paste("log_juv.km", paste(grid_search[i,-c(1:8,indx)], collapse=" + "), sep=" ~ ")
  # This returns the formula:
  #as.formula(formula)
  fit = gam(as.formula(formula),
            data = dplyr::filter(train, Juv.km>0))
  
  total_pred = gam_pred[[i]] * exp(predict(fit, test))
  grid_search$rmse[i] = sqrt(mean((total_pred - test$Juv.km)^2))
  #grid_search$auc[i] = as.numeric(auc(test$present, gam_pred[[i]]))
  print(i)
}

saveRDS(grid_search,paste0("output/gam-delta",n_years_ahead,"yr.rds"))

}

grid_search %>% dplyr::arrange(grid_search, rmse)
# for 0 step ahead, best model is 11/ 600, rmse ~ 153
# for 1 step ahead, best model is mtry=3 / ntree = 900, and rmse = 350.2042
# for 2 step ahead, best model is mtry=3 / ntree = 600, and rmse = 349.9797