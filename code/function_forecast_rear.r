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

#Read in the original juvenile data, change AUC.Mi to dens
if(stage=='rear'){
  original <- read.csv(paste0('C:/noaa/LARGE_Data/1DataExportJuv_2023_04_18.csv'), 
                       header=TRUE,
                       dec=".",
                       stringsAsFactors = FALSE)
}
if(stage=='Spwn'){
  original <- read.csv(paste0('C:/noaa/LARGE_Data/DataSpwn_2023_05_04.csv'),
                  header=TRUE,
                  dec=".",
                  stringsAsFactors = FALSE)
}
mean_SprPpt <- mean(original$SprPpt)
mean_MWMT_Index <- mean(original[,'MWMT_Index'])
sd_SprPpt <- sd(original$SprPpt)
sd_MWMT_Index <- sd(original[,'MWMT_Index'])

#wrangle the data and get it into the right form for the model
x <- function_wrangle_data(stage=stage, dir = getwd(), maxYr = 2021)
x <- x[x$yr<=maxYr,]

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

MWMT_Index <- read.csv(paste0("./data/MWMT_Indx_2080_",stage,".csv"))


#Get the unique ID and year 
#You want to retain as much of the data as possible before forecasting
#everything except MWMT_Index, W3Dppt, and SrPpt are time-invariant
#So you can keep all that information
df <- x %>%
  group_by(ID_Num) %>%
  summarize(UTM_E_km = mean(na.omit(UTM_E_km)),
            UTM_N_km = mean(na.omit(UTM_N_km)),
            dens = 0,
            STRM_ORDER = min(na.omit(STRM_ORDER)),
            IP_COHO = mean(na.omit(IP_COHO)),
            StrmSlope = mean(na.omit(StrmSlope)),
            WidthM = mean(na.omit(WidthM)), 
            SolMean = mean(na.omit(SolMean)),
            OUT_DIST = mean(na.omit(OUT_DIST)),
            StrmPow = mean(na.omit(StrmPow)),
            MWMT_Index  = mean(MWMT_Index),
            W3Dppt = 0, #we can't forecast this variable. so to compare apples to apples, we leave this out
            SprPpt = mean(SprPpt)) %>%
  # mutate(fYr = as.factor(yr)) %>%
  mutate(fSTRM_ORDER = as.factor(STRM_ORDER))
df$yr <- 2021
df$fYr <- as.factor(df$yr)

#Map the closest MWMT and SprPrt observations
nn <- RANN::nn2(x[x$yr==maxYr,c('UTM_N_km','UTM_E_km')],
                df[,c('UTM_N_km','UTM_E_km')],
                k = 1)
df$MWMT_Index <- x$MWMT_Index[x$yr==maxYr][nn$nn.idx]
df$SprPpt <- x$SprPpt[x$yr==maxYr][nn$nn.idx]

#Range of the forecast grid
e_range <- round(range(x$UTM_E_km),1)
n_range <- round(range(x$UTM_N_km),1)

#Create an expanded grid the 2021 data
grid <- expand.grid(UTM_E_km = seq((e_range[1]-5),(e_range[2]+5),1),
                    UTM_N_km = seq((n_range[1]-5),(n_range[2]+5),1))

#Now map the forecast_data_to_grid locations
nn.grid <- RANN::nn2(df[,c('UTM_E_km','UTM_N_km')],grid, k = 1)
df_grid <- df[nn.grid$nn.idx,]
df_grid[,c('UTM_E_km','UTM_N_km')] <- grid

#Now create a dataframe with all the data that's not 2021, plus the 
#expanded grid of 2021 data 
#just get the columns that are necessary for the model foecasts
myCols <- na.omit(match(names(df_grid),names(x)))

#This is the 2021 dataset for all locations 
forecast_data_2021 <- rbind(x[x$yr!=2021,myCols],
                            df_grid)


#Make the 2021 forecast
load(paste0("./output/spatial_output_",stage,"_2021.rData"))
pred2021 <- predict(output$exploratory$sdm$best_fit,
                    forecast_data_2021)

p2021 <- pred2021[pred2021$yr==maxYr,c('UTM_E_km','UTM_N_km','est')]
write.csv(p2021,file="./output/pred_2021.csv")
plot(p2021[,c('UTM_E_km','UTM_N_km')], col = heat.colors(2000)[2000-round(exp(p2021$est))])
# #just get the columns that are necessary for the model foecasts
# myCols <- na.omit(match(names(forecast_data),names(tmp_forecast_data)))


#Now create the 2080 
forecast_data_2080 <- forecast_data_2021

#Not all locations in the prediction grid have forecasted Precipition information
#Get the GPS coordinates for the precip information and use that to predict 2080 for the grid
SprPpt[,c('UTM_N_km','UTM_E_km')] <- x[match(SprPpt$IDNUM,x$ID_Num),c('UTM_N_km','UTM_E_km')]
MWMT_Index[,c('UTM_N_km','UTM_E_km')] <- x[match(MWMT_Index$ID_Num,x$ID_Num),c('UTM_N_km','UTM_E_km')]

#Now map the forecast_data_to_grid locations
nn.SprPpt <- RANN::nn2(na.omit(SprPpt[,c('UTM_N_km','UTM_E_km')]),
                       grid[,c('UTM_N_km','UTM_E_km')], k = 1)

nn.MWMT <- RANN::nn2(na.omit(MWMT_Index[,c('UTM_N_km','UTM_E_km')]),
                     grid[,c('UTM_N_km','UTM_E_km')], k = 1)

#Rescale the forecasted environmental data
forecast_data_2080$MWMT_Index[forecast_data_2080$yr==2021] <- (MWMT_Index$MWMT_Indx2080[nn.MWMT$nn.idx] - mean_MWMT_Index)/sd_MWMT_Index
forecast_data_2080$SprPpt[forecast_data_2080$yr==2021] <- (SprPpt$Spring[nn.SprPpt$nn.idx] - mean_SprPpt)/ sd_SprPpt

#Combine the new 2021 data which is actually the 2080 forecast data
#With all of the other years the sdmTMB needs
pred2080 <- predict(output$exploratory$sdm$best_fit,
                na.omit(forecast_data_2080))
#rename the year from 2021 to 2080
pred2080$yr[pred2080$yr==2021] <- 2080

p2080 <- pred2080[pred2080$yr==2080,c('UTM_E_km','UTM_N_km','est')]
write.csv(p2080,file="./output/pred_2080.csv")

plot(p2080[,c('UTM_E_km','UTM_N_km')], col = heat.colors(2000)[2000-round(exp(p2080$est))])

pred <- rbind(pred2080[,],
              pred2021)


write.csv(file = paste0("./output/forecasted_",stage,".csv"), pred[,c('yr', 'UTM_E_km','UTM_N_km','est')])

