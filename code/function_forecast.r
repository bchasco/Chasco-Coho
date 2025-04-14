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
juv <- read.csv(paste0('C:/noaa/LARGE_Data/1DataExportJuv_2023_04_18.csv'), 
                header=TRUE,
                dec=".",
                stringsAsFactors = FALSE) %>% 
  mutate('dens' = Juv.km) %>% 
  mutate('yr' = as.integer(JuvYr)) %>% 
  mutate(UTM_E = as.numeric(UTM_E)) %>%
  mutate(UTM_N = as.numeric(UTM_N)) %>%
  mutate(STRM_ORDER = as.integer(STRM_ORDER)) %>%
  filter_at(vars(dens,UTM_E,UTM_N), all_vars(!is.na(.)))#get rid of anything without a density
juv <- juv[juv$yr<=maxYr,]


#Read in the spawner data, change AUC.Mi to dens
sp <- read.csv(paste0('C:/noaa/LARGE_Data/DataSpwn_2023_05_04.csv'),
               header=TRUE,
               dec=".",
               stringsAsFactors = FALSE) %>%
  mutate('dens' = AUC.Mi) %>% 
  mutate('yr' = SpwnYr) %>% 
  mutate(UTM_E = as.numeric(as.character(UTM_E))) %>%
  mutate(UTM_N = as.numeric(UTM_N)) %>%
  filter_at(vars(dens), all_vars(!is.na(.))) #get rid of anything without a density
sp <- sp[sp$yr<=maxYr,]

sp <- function_wrangle_data(stage="spwn", dir = getwd(), maxYr = 2021)
#row bind the data based on common column headings
depVars <- c('STRM_ORDER','LifeStage','dens','yr','PopGrp','ID_Num')
coVars <- c('UTM_E','UTM_N'
            ,'WidthM','W3Dppt',
            'MWMT_Index','StrmPow',
            'SprPpt','IP_COHO', 
            'SolMean','StrmSlope',
            'OUT_DIST'
)


#get the scales 
j_sc <- apply(juv[,coVars],2,function(x){return(c(mean(x),sd(x)))})
sp_sc <- apply(sp[,coVars],2,function(x){return(c(mean(x),sd(x)))})

#Get the locations of each site.
sp_loc <- sp %>%
  group_by(SiteID) %>%
  summarize(UTM_E = mean(UTM_E), UTM_N = mean(UTM_N))

juv_loc <- juv %>%
  group_by(SiteID) %>%
  summarize(UTM_E = mean(UTM_E), UTM_N = mean(UTM_N))

#Get the forecasted environmental conditions for juveniles and spawners
SprPpt <- read.csv("./data/precip.csv")
MWMT_Index_sp <- read.csv("./data/MWMT_Indx_2080_spwn.csv")
MWMT_Index_juv <- read.csv("./data/MWMT_Indx_2080_rear.csv")


#Now replace with 2021 with the new data

f_juv <- juv %>%
  group_by(UTM_E,
           UTM_N,
           STRM_ORDER,
           IP_COHO,
           SiteID) %>%
  summarize(dens = 0,
            StrmSlope = 0,
            WidthM = 0, 
            SolMean = 0,
            OUT_DIST = 0,
            StrmPow = 0,
            MWMT_Index  = 0,
            W3Dppt = 0, 
            SprPpt = 0)

f_spwn <- sp %>%
  group_by(UTM_E,
           UTM_N,
           STRM_ORDER,
           IP_COHO,
           SiteID) %>%
  summarize(dens = 0,
            StrmSlope = 0,
            WidthM = 0, 
            SolMean = 0,
            OUT_DIST = 0,
            StrmPow = 0,
            MWMT_Index  = 0,
            W3Dppt = 0, 
            SprPpt = 0)

for(i in 1:nrow(f_juv)){
  f_juv$MWMT_Index[i] <- (mean(MWMT_Index_juv$MWMT_Indx2080[MWMT_Index_juv$SiteID==f_juv$SiteID[i]]) - mean(juv$MWMT_Index))/sd(juv$MWMT_Index)
  f_juv$SprPpt[i] <- (mean(SprPpt$Spring[SprPpt$IDNUM==f_juv$SiteID[i]])  - mean(juv$SprPpt))/sd(juv$SprPpt)
}

for(i in 1:nrow(f_spwn)){
  f_spwn$MWMT_Index[i] <- (mean(MWMT_Index_juv$MWMT_Indx2080[MWMT_Index_juv$SiteID==f_spwn$SiteID[i]])  - mean(sp$MWMT_Index))/sd(sp$MWMT_Index)
  f_spwn$SprPpt[i] <- (mean(SprPpt$Spring[SprPpt$IDNUM==f_spwn$SiteID[i]])  - mean(sp$SprPpt))/sd(sp$SprPpt)
}

f_spwn$UTM_E_km <- f_spwn$UTM_E/1000
f_spwn$UTM_N_km <- f_spwn$UTM_N/1000
sp$UTM_E_km <- sp$UTM_E/1000
sp$UTM_N_km <- sp$UTM_N/1000
sp$fYr <- as.factor(sp$yr)
sp <- na.omit(sp)

sptmp <- sp[,c(depVars,coVars)]
f_spwn$yr <- 2021
f_spwn$ID_Num <- f_spwn$siteID
f_spwn$LifeStage <- "spwn"
m <- names(sptmp)[na.omit(match(names((f_spwn)),names((sptmp))))]
forecast_df <- rbind(f_spwn[,m],sptmp[,m])
forecast_df$fYr <- as.factor(forecast_df$yr)

mesh <- sdmTMB::make_mesh()
pred <- predict(output$exploratory$sdm$best_fit,
                sp)
