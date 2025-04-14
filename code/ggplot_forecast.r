rm(list=ls())
library(sp)
library(glmmTMB)
library(raster)
library(ggplot2)
library(viridis)
library(viridisLite)
library(cowplot)
library(RANN)

#2e) Now you need the covariates for your model
stage <- "Spwn" #rear or Spwn

#Grab the data for stage
source("./code/function_wrangle_data.r")
df <- function_wrangle_data(stage=stage,
                            dir = root)
df <- na.omit(df)

#You need the unscaled raw data
source("./code/function_wrangle_data_forecast.r")
df_f <- function_wrangle_data_forecast(stage=stage,
                            dir = root)
load(paste0("output/output_",stage,".rData"))

SprPpt <- read.csv("./data/precip.csv")
MWMT <- read.csv("./data/MWMT_Indx_2080_spwn.csv")

sc <- scale(df_f$SprPpt[df_f$SprPpt!=(-9999.0)])
sc_MW <- scale(df_f$MWMT_Index)

#Match up the SprPpt ID with the data IDs
spr_sp <- SprPpt$Spring[match(df$ID_Num[df$yr==2019],SprPpt$IDNUM[SprPpt$year==2019])]
mwmt <- MWMT$MWMT_Indx2080[match(df$ID_Num[df$yr==2019],MWMT$SiteID)]

#Subset by unique locations, non-duplicated df
nd.df <- df_f[!duplicated(df[,c('UTM_E_km','UTM_N_km','PopGrp','ID_Num')]),c('UTM_E_km','UTM_N_km','PopGrp','ID_Num')]
nd <- nrow(nd.df)
nd.df$UTM_E <- nd.df$UTM_E_km*1000
nd.df$UTM_N <- nd.df$UTM_N_km*1000
nd.df[,names(df)[!names(df)%in%names(nd.df)]] <- 0 #add all of the column heading
nd.df <- do.call("rbind", replicate(length(unique(df$yr)), nd.df, simplify = FALSE))
nd.df$yr <- rep(unique(df$yr),each=nd)
nd.df$fYr <- as.factor(nd.df$yr)

coVars <- c('WidthM','W3Dppt',
            'MWMT_Index','StrmPow',
            'SprPpt','IP_COHO',
            'SolMean','StrmSlope',
            'OUT_DIST','STRM_ORDER' 
)



for(iii in coVars){
  for(y in unique(nd.df$yr)){
    kmean <- nn2(df[df$yr==y,c('UTM_E','UTM_N')],nd.df[nd.df$yr==y,c('UTM_E','UTM_N')], k=1)
    nd.df[nd.df$yr==y,iii] <- df[kmean$nn.idx,iii]
  }
}

nd.df$fSTRM_ORDER <- as.factor(nd.df$fSTRM_ORDER)
levels(nd.df$fSTRM_ORDER) <- levels(df$fSTRM_ORDER)


fit <- output$exploratory$sdm$best_fit

tmp_df <- df
SprPpt$Spring_sc <- (SprPpt$Spring - mean(df_f$SprPpt))/sd(df_f$SprPpt)
MWMT$sc_2080 <- (MWMT$MWMT_Indx2080 - mean(df_f$MWMT_Index))/sd(df_f$MWMT_Index)
tmp_df$SprPpt[tmp_df$yr==2019]<-SprPpt$Spring_sc[match(tmp_df$ID_Num[df$yr==2019],SprPpt$IDNUM[SprPpt$year==2019])]
tmp_df$MWMT_Index[tmp_df$yr==2019]<-MWMT$sc_2080[match(tmp_df$ID_Num[df$yr==2019],MWMT$SiteID)]

tmp_nd.df <- nd.df
tmp_nd.df$SprPpt[tmp_nd.df$yr==2019] <- SprPpt$Spring_sc[match(tmp_nd.df$ID_Num[tmp_nd.df$yr==2019],SprPpt$IDNUM[SprPpt$year==2019])]
tmp_nd.df$MWMT_Index[tmp_nd.df$yr==2019]<-MWMT$sc_2080[match(tmp_nd.df$ID_Num[tmp_nd.df$yr==2019],MWMT$SiteID)]

pred_2019 <- exp(predict(fit, df[!is.na(tmp_df$SprPpt) & !is.na(tmp_df$MWMT_Index),])$est)
pred_2080 <- exp(predict(fit, tmp_df[!is.na(tmp_df$SprPpt)& !is.na(tmp_df$MWMT_Index),])$est)
pred_nd_2019 <- exp(predict(fit, nd.df[!is.na(tmp_nd.df$SprPpt)& !is.na(tmp_nd.df$MWMT_Index),])$est)
pred_nd_2080 <- exp(predict(fit, tmp_nd.df[!is.na(tmp_nd.df$SprPpt)& !is.na(tmp_nd.df$MWMT_Index),])$est)
# tmp <- cbind(nd.df,pred$est)

nd.df <- cbind(nd.df[!is.na(tmp_nd.df$SprPpt) & !is.na(tmp_nd.df$MWMT_Index),],pred_nd_2019,pred_nd_2080)
nd.df$ESU <- "Oregon"
df <- cbind(df[!is.na(tmp_df$SprPpt)& !is.na(tmp_df$MWMT_Index),],pred_2019,pred_2080)
df$ESU <- "Oregon"


tmp <- nd.df %>%
  filter(yr == 2019) %>%
  group_by(PopGrp) %>%
  summarise(mean_2019 = sum(pred_nd_2019),
            mean_2080 = sum(pred_nd_2080),
            UTM_E_km_2080 = sum(UTM_E_km*pred_nd_2080)/sum(pred_nd_2080),
            UTM_E_km_2019 = sum(UTM_E_km*pred_nd_2019)/sum(pred_nd_2019),
            UTM_E_km_diff = sum(UTM_E_km*pred_nd_2080)/sum(pred_nd_2080) - sum(UTM_E_km*pred_nd_2019)/sum(pred_nd_2019),
            UTM_N_km_diff = sum(UTM_N_km*pred_nd_2080)/sum(pred_nd_2080) - sum(UTM_N_km*pred_nd_2019)/sum(pred_nd_2019),
            ratio = sum(pred_nd_2080)/sum(pred_nd_2019)
  )%>%
  mutate(data = 'Watershed') %>%
  bind_rows(df %>%
  filter(yr == 2019) %>%
  group_by(PopGrp) %>%
  summarise(mean_2019 = sum(pred_2019),
            mean_2080 = sum(pred_2080),
            UTM_E_km_2080 = sum(UTM_E_km*pred_2080)/sum(pred_2080),
            UTM_E_km_2019 = sum(UTM_E_km*pred_2019)/sum(pred_2019),
            UTM_E_km_diff = sum(UTM_E_km*pred_2080)/sum(pred_2080) - sum(UTM_E_km*pred_2019)/sum(pred_2019),
            UTM_N_km_diff = sum(UTM_N_km*pred_2080)/sum(pred_2080) - sum(UTM_N_km*pred_2019)/sum(pred_2019),
            ratio = sum(pred_2080)/sum(pred_2019),
  )%>%
  mutate(data = 'Observed locations')) %>%
  pivot_longer(!c(PopGrp,data), names_to = "Year", values_to = "Index")

  g1 <- ggplot(tmp[tmp$Year=="ratio" & tmp$data=="Observed locations",], aes(x = PopGrp, y = Index, fill = data)) +
    # facet_wrap(~data, nrow = 2, scales = "free") +
    theme_bw()   +
    # scale_y_continuous(limits = c(0.6,1.2)) +
    ylab("2080:2019 ratio of abundance index") +
    xlab("") +
    geom_bar(position = 'dodge',stat = "identity", fill = "grey") +
    scale_y_continuous(expand = c(0, 0)) +
    theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
    geom_hline(yintercept = 1) +
    theme(legend.position = 'none')
    
  
  g3 <- ggplot(tmp[tmp$Year=="UTM_E_km_diff" | tmp$Year=="UTM_N_km_diff",], aes(x = PopGrp, y = Index, fill = data)) +
    facet_wrap(~Year, nrow = 2, scales = "free") +
    theme_bw()   +
    # scale_y_continuous(limits = c(0.6,1.2)) +
    ylab("Kilometers") + 
    geom_bar(position = 'dodge',stat = "identity") +
    theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) 
  
  stage <- "rear" #rear or Spwn
  
  #Grab the data for stage
  source("./code/function_wrangle_data.r")
  df <- function_wrangle_data(stage=stage,
                              dir = root)
  df <- na.omit(df)
  
  #You need the unscaled raw data
  source("./code/function_wrangle_data_forecast.r")
  df_f <- function_wrangle_data_forecast(stage=stage,
                                         dir = root)
  load(paste0("output/output_",stage,".rData"))
  
  SprPpt <- read.csv("./data/precip.csv")
  MWMT <- read.csv("./data/MWMT_Indx_2080_rear.csv")
  
  sc <- scale(df_f$SprPpt)
  sc_MW <- scale(df_f$MWMT_Index)
  hist(df_f$SprPpt/10)
  hist(df_f$MWMT_Index)
  spr_sp <- SprPpt$Spring[match(df$ID_Num[df$yr==2019],SprPpt$IDNUM[SprPpt$year==2019])]
  mwmt <- MWMT$MWMT_Indx2080[match(df$ID_Num[df$yr==2019],MWMT$SiteID)]
  
  #Subset by unique locations, non-duplicated df
  nd.df <- df_f[!duplicated(df[,c('UTM_E_km','UTM_N_km','PopGrp','ID_Num')]),c('UTM_E_km','UTM_N_km','PopGrp','ID_Num')]
  nd <- nrow(nd.df)
  nd.df$UTM_E <- nd.df$UTM_E_km*1000
  nd.df$UTM_N <- nd.df$UTM_N_km*1000
  nd.df[,names(df)[!names(df)%in%names(nd.df)]] <- 0 #add all of the column heading
  nd.df <- do.call("rbind", replicate(length(unique(df$yr)), nd.df, simplify = FALSE))
  nd.df$yr <- rep(unique(df$yr),each=nd)
  nd.df$fYr <- as.factor(nd.df$yr)
  
  #Use kmeans to map the stream in the original data frame to 
  #the prediction dataframe
  # kmean <- nn2(df[,c('UTM_E','UTM_N')],nd.df[,c('UTM_E','UTM_N')], k=1)
  
  coVars <- c('WidthM','W3Dppt',
              'MWMT_Index','StrmPow',
              'SprPpt','IP_COHO',
              'SolMean','StrmSlope',
              'OUT_DIST','STRM_ORDER' 
  )
  
  
  
  for(iii in coVars){
    for(y in unique(nd.df$yr)){
      kmean <- nn2(df[df$yr==y,c('UTM_E','UTM_N')],nd.df[nd.df$yr==y,c('UTM_E','UTM_N')], k=1)
      nd.df[nd.df$yr==y,iii] <- df[kmean$nn.idx,iii]
    }
  }
  
  nd.df$fSTRM_ORDER <- as.factor(nd.df$fSTRM_ORDER)
  levels(nd.df$fSTRM_ORDER) <- levels(df$fSTRM_ORDER)
  
  
  fit <- output$exploratory$sdm$best_fit
  
  tmp_df <- df
  SprPpt$Spring_sc <- (SprPpt$Spring - mean(df_f$SprPpt))/sd(df_f$SprPpt)
  MWMT$sc_2080 <- (MWMT$MWMT_Indx2080 - mean(df_f$MWMT_Index))/sd(df_f$MWMT_Index)
  tmp_df$SprPpt[tmp_df$yr==2019]<-SprPpt$Spring_sc[match(tmp_df$ID_Num[df$yr==2019],SprPpt$IDNUM[SprPpt$year==2019])]
  tmp_df$MWMT_Index[tmp_df$yr==2019]<-MWMT$sc_2080[match(tmp_df$ID_Num[df$yr==2019],MWMT$SiteID)]
  
  tmp_nd.df <- nd.df
  tmp_nd.df$SprPpt[tmp_nd.df$yr==2019] <- SprPpt$Spring_sc[match(tmp_nd.df$ID_Num[tmp_nd.df$yr==2019],SprPpt$IDNUM[SprPpt$year==2019])]
  tmp_nd.df$MWMT_Index[tmp_nd.df$yr==2019]<-MWMT$sc_2080[match(tmp_nd.df$ID_Num[tmp_nd.df$yr==2019],MWMT$SiteID)]
  
  pred_2019 <- exp(predict(fit, df[!is.na(tmp_df$SprPpt),])$est)
  pred_2080 <- exp(predict(fit, tmp_df[!is.na(tmp_df$SprPpt),])$est)
  pred_nd_2019 <- exp(predict(fit, nd.df[!is.na(tmp_nd.df$SprPpt),])$est)
  pred_nd_2080 <- exp(predict(fit, tmp_nd.df[!is.na(tmp_nd.df$SprPpt),])$est)
  # tmp <- cbind(nd.df,pred$est)
  
  nd.df <- cbind(nd.df[!is.na(tmp_nd.df$SprPpt),],pred_nd_2019,pred_nd_2080)
  nd.df$ESU <- "Oregon"
  df <- cbind(df[!is.na(tmp_df$SprPpt),],pred_2019,pred_2080)
  df$ESU <- "Oregon"
  
  tmp <- #nd.df %>%
    # filter(yr == 2019) %>%
    # group_by(PopGrp) %>%
    # summarise(mean_2019 = sum(pred_nd_2019),
    #           mean_2080 = sum(pred_nd_2080),
    #           UTM_E_km_2080 = sum(UTM_E_km*pred_nd_2080)/sum(pred_nd_2080),
    #           UTM_E_km_2019 = sum(UTM_E_km*pred_nd_2019)/sum(pred_nd_2019),
    #           UTM_E_km_diff = sum(UTM_E_km*pred_nd_2080)/sum(pred_nd_2080) - sum(UTM_E_km*pred_nd_2019)/sum(pred_nd_2019),
    #           UTM_N_km_diff = sum(UTM_N_km*pred_nd_2080)/sum(pred_nd_2080) - sum(UTM_N_km*pred_nd_2019)/sum(pred_nd_2019),
    #           ratio = sum(pred_nd_2080)/sum(pred_nd_2019)
    # )  %>%
    # mutate(data = 'Watershed') %>%
    bind_rows(
      df %>%
                filter(yr == 2019) %>%
                group_by(PopGrp) %>%
                summarise(mean_2019 = sum(pred_2019),
                          mean_2080 = sum(pred_2080),
                          UTM_E_km_2080 = sum(UTM_E_km*pred_2080)/sum(pred_2080),
                          UTM_E_km_2019 = sum(UTM_E_km*pred_2019)/sum(pred_2019),
                          UTM_E_km_diff = sum(UTM_E_km*pred_2080)/sum(pred_2080) - sum(UTM_E_km*pred_2019)/sum(pred_2019),
                          UTM_N_km_diff = sum(UTM_N_km*pred_2080)/sum(pred_2080) - sum(UTM_N_km*pred_2019)/sum(pred_2019),
                          ratio = sum(pred_2080)/sum(pred_2019)
                )%>%
                mutate(data = 'Observed locations'))%>%
    pivot_longer(!c(PopGrp,data), names_to = "Year", values_to = "Index")
  
  mean(tmp[tmp$Year=="ratio" & tmp$data=="Observed locations",]$Index)
  
  g2 <- ggplot(tmp[tmp$Year=="ratio" & tmp$data=="Observed locations",], aes(x = PopGrp, y = Index, fill = data)) +
    # facet_wrap(~data, nrow = 2, scales = "free") +
    theme_bw()   +
    # scale_y_continuous(limits = c(0.6,1.2)) +
    ylab("2080:2019 ratio of abundance index") +
    xlab("") +
    geom_bar(position = 'dodge',stat = "identity", fill = "grey") +
    scale_y_continuous(expand = c(0, 0)) +
    theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
    geom_hline(yintercept = 1) +
    theme(legend.position = 'none')

  g4 <- ggplot(tmp[tmp$Year=="UTM_E_km_diff" | tmp$Year=="UTM_N_km_diff",], aes(x = PopGrp, y = Index, fill = data)) +
    facet_wrap(~Year, nrow = 2, scales = "free") +
    theme_bw()   +
    # scale_y_continuous(limits = c(0.6,1.2)) +
    ylab("Kilometers") + 
    scale_y_continuous(expand = c(0, 0)) +
    geom_bar(position = 'dodge',stat = "identity") +
    theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) 
  print(g3)  
  
png("./output/ggplot_forecast_location.png", height = 400, width = 600)    
ggpubr::ggarrange(g4,g3,ncol=2,labels = c('A','B','C','D'), common.legend = TRUE, legend = 'right')  
dev.off()

png("./output/ggplot_forecast_index.png", height = 400, width = 600)    
ggpubr::ggarrange(g2,g1,ncol=2,labels = c('A','B'), common.legend = TRUE, legend = 'right')  
dev.off()
