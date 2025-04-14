#First fit to all of the data.
#from 1998 to 

load("combined_model_fit_diagnostics.rData")

mape <- function(est,obs){return(sum((est-obs)/obs)/length(obs))}

forecast_yrs <- 2017:2021

mape_top_pred <- forecast_yrs
mape_rand_pred <- forecast_yrs
mape_annual_pred <- forecast_yrs
mape_annual_three_pred <- forecast_yrs


#naming for output
survey_type <- "Temporal"
life_stage <- 'Spwn'
endYr <- max(forecast_yrs)

#Get the observed data
load(paste0("./output/",survey_type,"_output_",life_stage,"_",endYr,".rData"))
obs_data <- function_wrangle_data(stage = life_stage, dir = NA)

fit <- output$exploratory$sdm$best_fit
comp <- data.frame(sample = NA, 
                   forecast_yrs = NA, 
                   mape_top_pred = NA, 
                   mape_rand_pred = NA,
                   mape_annual_pred = NA, 
                   mape_annual_three_pred = NA)

nrep <- 1

survey_yrs <- 2017:2021
icnt <- 1
for(j in list(c("annual", "annua"),c("annual","annua","three"))){
  for(i in survey_yrs){
    print(paste("forecast year", i))
    print(paste("panel", j))
    
    #Training data up to forecast year
    train <- obs_data[obs_data$yr < i,]
    #Data for survey design
    survey <- obs_data[(obs_data$Panel%in%j) & 
                                  obs_data$yr%in%(min(survey_yrs):i),]
    
    #idx data for the whole year
    idx <- obs_data[obs_data$yr%in%(i),]

    mesh <- make_mesh(rbind(train,survey), c("UTM_E_km", "UTM_N_km"), cutoff = 10)
    
    #Predict survey design
    tmp_fit <- sdmTMB(fit$formula[[1]],
                  data = rbind(train,survey),
                  mesh = mesh,
                  family = tweedie(link = "log"),
                  time = "yr",
                  spatial = fit$spatial,
                  spatiotemporal = fit$spatiotemporal,
                  anisotropy = TRUE, #Why is this necessary if the years are the same?
                  silent=TRUE)
    
    #Predict oob  
    pred <- predict(tmp_fit, newdata = rbind(train,idx))
    RMSE <- sqrt(mean((exp(pred$est[pred$yr==i])-rbind(train,idx)$dens[pred$yr==i])^2))
    
    comp[icnt,] <- c(i,j,RMSE)      
  }
}

save(comp, file = "analysis_of_top_rivers_Spwn.rData")
