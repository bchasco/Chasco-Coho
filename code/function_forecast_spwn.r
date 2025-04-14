rm(list=ls())
library(sp)
library(glmmTMB)
library(raster)
library(ggplot2)
library(viridis)
library(viridisLite)
library(cowplot)
library(RANN)
library(tidyr)
library(dplyr)
library(sdmTMB)
library(mgcv)
library(randomForest)
#2e) Now you need the covariates for your model
stage <- "Spwn" #rear or Spwn

#Grab the data for stage
source("./code/function_wrangle_data.r")


maxYr = 2021

#Read in the juvenile data, change AUC.Mi to dens
#Read in the spawner data, change AUC.Mi to dens
juv <- read.csv(paste0('C:/noaa/LARGE_Data/DataSpwn_2023_05_04.csv'),
               header=TRUE,
               dec=".",
               stringsAsFactors = FALSE) %>%
  mutate('dens' = AUC.Mi) %>% 
  mutate('yr' = SpwnYr) %>% 
  mutate(UTM_E = as.numeric(as.character(UTM_E))) %>%
  mutate(UTM_N = as.numeric(UTM_N)) %>%
  mutate(UTM_E_km =UTM_E/1000) %>%
  mutate(UTM_N_km = UTM_N/1000) %>%
  filter_at(vars(dens), all_vars(!is.na(.))) #get rid of anything without a density
juv <- juv[juv$yr<=maxYr,]
mean_SprPpt <- mean(juv$SprPpt)
mean_MWMT_Index <- mean(juv[,'MWMT_Index'])
sd_SprPpt <- sd(juv$SprPpt)
sd_MWMT_Index <- sd(juv[,'MWMT_Index'])

juv <- function_wrangle_data(stage="Spwn", dir = getwd(), maxYr = 2021)
#row bind the data based on common column headings
depVars <- c('STRM_ORDER','LifeStage','dens','yr','PopGrp')
coVars <- c('UTM_E','UTM_N'
            ,'WidthM','W3Dppt',
            'MWMT_Index','StrmPow',
            'SprPpt','IP_COHO', 
            'SolMean','StrmSlope',
            'OUT_DIST'
)


#Get the forecasted environmental conditions for juveniles and spawners
SprPpt <- read.csv("./data/Final2080_precip.csv")
MWMT_Index_sp <- read.csv("./data/MWMT_Indx_2080_spwn.csv")


#Now replace with 2021 with the new data
forecast_data <- juv %>%
  group_by(STRM_ORDER,
           IP_COHO,
           # yr, #you want to leave out forecast year and get all of the unique locations
           ID_Num) %>%
  summarize(UTM_E_km = mean(na.omit(UTM_E_km)),
            UTM_N_km = mean(na.omit(UTM_N_km)),
            dens = 0,
            StrmSlope = StrmSlope,
            WidthM = WidthM, 
            SolMean = SolMean,
            OUT_DIST = OUT_DIST,
            StrmPow = StrmPow,
            MWMT_Index  = MWMT_Index,
            W3Dppt = W3Dppt,
            SprPpt = SprPpt,
            yr = yr) %>%
  mutate(fYr = as.factor(yr)) %>%
  mutate(fSTRM_ORDER = as.factor(STRM_ORDER))

#set the covariate that deviate yearly to a zero except for 2021
forecast_data[forecast_data$yr!=2021, c('MWMT_Index','W3Dppt', 'SprPpt')] <- 0

#Now create a data.frame for all unique sites
df_unique <- forecast_data
df_unique <- df_unique %>%
  distinct(STRM_ORDER, IP_COHO, ID_Num, UTM_E_km, UTM_E_km, dens, StrmSlope, WidthM, SolMean, OUT_DIST, StrmPow, .keep_all = TRUE)

#Now grab just the 2021
df_2021 <- juv[juv$yr == 2021,]

#Now find the nearest neighbor from the 2021 dataframe and map it to the unique site dataframe
x <- RANN::nn2(df_2021[,c('UTM_N_km','UTM_E_km')], df_unique[,c('UTM_N_km','UTM_E_km')], k = 1)

#Now go back and fill in the c('MWMT_Index','W3Dppt', 'SprPpt') for all years not 2021
for(i in 1:nrow(df_unique)){
  if(df_unique$yr[i]!=2021){
    df_unique[i,c('MWMT_Index', 'SprPpt')] <- df_2021[x$nn.idx[i],c('MWMT_Index','SprPpt')]
  }
}
#so you can compare apples to apples
#make 'W3Dppt'= 0 because it can't be forecasted into the future
df_unique$W3Dppt <- 0

#now make all of the years in the df_uniq data equal to 2021
df_unique$yr  <- 2021
df_unique$fYr  <- as.factor(df_unique$yr)


#Now create a data with all the data that's not 2021, plus the 
#unique 2021 data 
#just get the columns that are necessary for the model foecasts
myCols <- na.omit(match(names(df_unique),names(juv)))

#This is the 2021 dataset for all locations 
forecast_data_2021 <- rbind(juv[juv$yr!=2021,myCols],
                            df_unique)


#Make the 2021 forecast
load("./output/spatial_output_Spwn_2021.rData")
pred2021 <- predict(output$exploratory$sdm$best_fit,
                    forecast_data_2021)

#Now create the 2080 
forecast_data_2080 <- forecast_data_2021


#map all of the 
#Even if sites were sampled in 2021 we want to use the site
for(j in 1:nrow(forecast_data_2080)){
  if(forecast_data_2080$yr[i]==2021)
  #Start by getting the basic site-specific dependent information
  # forecast_data_[j,''] <- juv[MWMT_Index_sp$SiteID[j]==juv$ID_Num,depVars][1,]
  forecast_data_2080[j,"MWMT_Index"] <- (mean(MWMT_Index_sp$MWMT_Indx2080[MWMT_Index_sp$SiteID==forecast_data_2080$ID_Num[j]])  - mean_MWMT_Index)/sd_MWMT_Index
  
  forecast_data_2080[j,"SprPpt"] <- (mean(SprPpt$Spring[SprPpt$IDNUM==forecast_data_2080$ID_Num[j]])  - mean_SprPpt)/sd_SprPpt
}


# #just get the columns that are necessary for the model foecasts
# myCols <- na.omit(match(names(forecast_data),names(tmp_forecast_data)))

#Combine the new 2021 data which is actually the 2080 forecast data
#With all of the other years the sdmTMB needs
pred2080 <- predict(output$exploratory$sdm$best_fit,
                na.omit(forecast_data_2080))
pred2080$yr[pred2080$yr==2021] <- 2080

pred_juv <- rbind(pred2080[,],
              pred2021)

e_range <- round(range(pred_juv$UTM_E_km),1)
n_range <- round(range(pred_juv$UTM_N_km),1)

grid <- expand.grid(year = c(2021,2080),
                    utm_e_km = seq((e_range[1]-5),(e_range[2]+5),1),
                    utm_n_km = seq((n_range[1]-5),(n_range[2]+5),1))
grid$est <- 0

idx2021 <- RANN::nn2(pred_juv[pred_juv$yr==2021,c('UTM_E_km','UTM_N_km')],grid[grid$year == 2021, c('utm_e_km','utm_n_km')],1)
idx2080 <- RANN::nn2(pred_juv[pred_juv$yr==2080,c('UTM_E_km','UTM_N_km')],grid[grid$year == 2080, c('utm_e_km','utm_n_km')],1)
grid$est[grid$year == 2021] <- pred_juv$est[pred_juv$yr==2021][idx2021$nn.idx]
grid$est[grid$year == 2080] <- pred_juv$est[pred_juv$yr==2080][idx2080$nn.idx]

write.csv(file = "./output/forecasted_spwn.csv", grid)

plot(pred_juv$UTM_E_km[pred_juv$yr==2021],
     pred_juv$UTM_N_km[pred_juv$yr==2021], 
     col =heat.colors(250)[250-round(exp(pred_juv$est[pred_juv$yr==2021]))])

plot(pred_juv$UTM_E_km[pred_juv$yr==2080],
     pred_juv$UTM_N_km[pred_juv$yr==2080], 
     col =heat.colors(250)[250-round(exp(pred_juv$est[pred_juv$yr==2080]))])
