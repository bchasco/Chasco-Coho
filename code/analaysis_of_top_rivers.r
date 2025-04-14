#First fit to all of the data.
#from 1998 to 

load("combined_model_fit_diagnostics.rData")

mape <- function(est,obs){return(sum((est-obs)/obs)/length(obs))}

forecast_yrs <- 2017:2021

mape_top_pred <- forecast_yrs
mape_rand_pred <- forecast_yrs
mape_annual_pred <- forecast_yrs
mape_annual_three_pred <- forecast_yrs

life_stage <- 'Spwn'
#Get the observed data
load(paste0("./output/output_",life_stage,".rData"))
obs_data <- function_wrangle_data(stage = life_stage, dir = NA)
fit <- output$exploratory$sdm$best_fit

comp <- data.frame(sample = NA, 
                   forecast_yrs = NA, 
                   mape_top_pred = NA, 
                   mape_rand_pred = NA,
                   mape_annual_pred = NA, 
                   mape_annual_three_pred = NA)

nrep <- 10

for(j in 1:nrep){
  icnt <- 1
  for(i in forecast_yrs){
    print(paste("forecast year", i))
    print(paste("rep", j))
    
    forecast_yr <- i
    
    
    burn_yrs <- obs_data[obs_data$yr < forecast_yr,]
    burn_yrs$fYr <- as.factor(burn_yrs$yr)
    
    # top_pops <- arr$pi[arr$stat=="KS" & arr$col=="black" & arr$Life_stage=="Juveniles"] 
    # top_pop_data <- obs_data[(obs_data$PopGrp%in%top_pops) & obs_data$yr%in%(2017:forecast_yr),]
    # top_pop_data$fYr <- as.factor(top_pop_data$yr)
    # top_data <- rbind(burn_yrs,top_pop_data)

    annual_pop_data <- obs_data[(obs_data$Panel%in%c('annual')) & 
                                  obs_data$yr%in%(2017:forecast_yr),]

    annual_pop_data$fYr <- as.factor(annual_pop_data$yr)
    annual_data <- rbind(burn_yrs,annual_pop_data)

    annual_three_pop_data <- obs_data[(obs_data$Panel%in%c('annual','three')) & 
                                        obs_data$yr%in%(2017:forecast_yr),]
    
    annual_three_pop_data$fYr <- as.factor(annual_three_pop_data$yr)
    annual_three_data <- rbind(burn_yrs,annual_three_pop_data)
    
    # top_pops <- arr$pi[arr$stat=="KS" & arr$col=="black" & arr$Life_stage=="Juveniles"] 
    # top_pop_data <- obs_data[(obs_data$PopGrp%in%top_pops) & obs_data$yr%in%(2017:forecast_yr),]
    # top_pop_data$fYr <- as.factor(top_pop_data$yr)
    # top_data <- rbind(burn_yrs,top_pop_data)
    
    if(i == min(forecast_yrs)){
      rand_pop <- sample(1:24, size = 6)
    }
    rand_pops <- unique(obs_data$PopGrp)[rand_pop]
    
    rand_pop_data <- obs_data[(obs_data$PopGrp%in%rand_pops) & obs_data$yr%in%(2017:forecast_yr),]
    rand_pop_data$fYr <- as.factor(rand_pop_data$yr)
    rand_data <- rbind(burn_yrs,rand_pop_data)
    
    # pred_data <- obs_data[(obs_data$PopGrp%in%top_pops)==FALSE & obs_data$yr%in%(2017:forecast_yr),]
    # pred_data$fYr <- as.factor(pred_data$yr)
    
    rand_pred_data <- obs_data[(obs_data$PopGrp%in%rand_pops)==FALSE & obs_data$yr%in%(2017:forecast_yr),]
    rand_pred_data$fYr <- as.factor(rand_pred_data$yr)

    annual_pred_data <- obs_data[obs_data$Panel%in%c("annual") & obs_data$yr%in%(2017:forecast_yr),]
    annual_pred_data$fYr <- as.factor(annual_pred_data$yr)

    annual_three_pred_data <- obs_data[obs_data$Panel%in%c("annual",'three') & obs_data$yr%in%(2017:forecast_yr),]
    annual_three_pred_data$fYr <- as.factor(annual_three_pred_data$yr)
    
    if(j ==1){
      # mesh <- make_mesh(top_data, c("UTM_E_km", "UTM_N_km"), cutoff = 10)
      # fit <- sdmTMB(fit$formula[[1]],
      #               data = top_data,
      #               mesh = mesh,
      #               family = tweedie(link = "log"),
      #               time = "yr",
      #               spatial = TRUE,
      #               spatiotemporal = TRUE,
      #               anisotropy = TRUE, #Why is this necessary if the years are the same?
      #               silent=TRUE)
      # top_pred <- predict(fit, newdata = rbind(burn_yrs,pred_data))
      # top_pred_oob <- top_pred[!(top_pred$PopGrp%in%top_pops) & top_pred$yr==forecast_yr,]
      
      # mape_top_pred[icnt] <- mape(top_pred_oob$dens,exp(top_pred_oob$est))
      
      mesh <- make_mesh(annual_data, c("UTM_E_km", "UTM_N_km"), cutoff = 10)
      fit <- sdmTMB(fit$formula[[1]],
                    data = annual_data,
                    mesh = mesh,
                    family = tweedie(link = "log"),
                    time = "yr",
                    spatial = fit$spatial,
                    spatiotemporal = fit$spatiotemporal,
                    anisotropy = TRUE, #Why is this necessary if the years are the same?
                    silent=TRUE)
      
      annual_pred_oob <- obs_data[!(obs_data$Panel%in%c("annual")) & obs_data$yr==forecast_yr,]
      annual_pred <- predict(fit, newdata = rbind(burn_yrs,annual_pred_oob))
      
      mape_annual_pred[icnt] <- mape(annual_pred_oob$dens,
                                     exp(annual_pred$est[annual_pred$yr==forecast_yr]))
      
      mesh <- make_mesh(annual_three_data, c("UTM_E_km", "UTM_N_km"), cutoff = 10)
      fit <- sdmTMB(fit$formula[[1]],
                    data = annual_three_data,
                    mesh = mesh,
                    family = tweedie(link = "log"),
                    time = "yr",
                    spatial = fit$spatial,
                    spatiotemporal = fit$spatiotemporal,
                    anisotropy = TRUE, #Why is this necessary if the years are the same?
                    silent=TRUE)
      annual_three_pred_oob <- obs_data[!(obs_data$Panel%in%c("annual",'three')) & 
                                          obs_data$yr==forecast_yr,]
      annual_three_pred <- predict(fit, 
                                   newdata = rbind(burn_yrs,annual_three_pred_oob))
      mape_annual_three_pred[icnt] <- mape(annual_three_pred_oob$dens,
                                           exp(annual_three_pred$est[annual_three_pred$yr==forecast_yr]))
      
    }
    
    
    mesh <- make_mesh(rand_data, c("UTM_E_km", "UTM_N_km"), cutoff = 10)
    fit <- sdmTMB(fit$formula[[1]],
                  data = rand_data,
                  mesh = mesh,
                  family = tweedie(link = "log"),
                  time = "yr",
                  spatial = fit$spatial,
                  spatiotemporal = fit$spatiotemporal,
                  anisotropy = TRUE, #Why is this necessary if the years are the same?
                  silent=TRUE)
    rand_pred_oob <- obs_data[!(obs_data$PopGrp%in%rand_pops) & obs_data$yr==forecast_yr,]
    rand_pred <- predict(fit, newdata = rbind(burn_yrs,rand_pred_oob))
    mape_rand_pred[icnt] <- mape(rand_pred_oob$dens,exp(rand_pred$est[rand_pred$yr==forecast_yr]))

    
    icnt <- icnt + 1  
  }
  comp <- rbind(comp,cbind(sample=rep(j,length(forecast_yrs)),forecast_yrs,mape_top_pred,mape_rand_pred,mape_annual_pred,mape_annual_three_pred))
}

save(comp, file = "analysis_of_top_rivers_Spwn.rData")
