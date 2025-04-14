# spatial_index <- function(arg_no_covars=TRUE){
  library(sp)
  library(glmmTMB)
  library(raster)
  library(ggplot2)
  library(viridis)
  library(viridisLite)
  library(cowplot)
  library(RANN)
#   
  
  root <- getwd()
  
  stage <- "rear" #rear or Spwn
  df <- function_wrangle_data(stage=stage,
                              dir = root,
                              maxYr = 2021)
  
  load(paste0("output/output_",stage,".rData"))

  #Subset by unique locations, non-duplicated df
  nd.df <- df[!duplicated(df[,c('UTM_E_km','UTM_N_km')]),c('UTM_E_km','UTM_N_km')]
  nd <- nrow(nd.df)
  nd.df$UTM_E <- nd.df$UTM_E_km*1000
  nd.df$UTM_N <- nd.df$UTM_N_km*1000
  nd.df[,names(df)[!names(df)%in%names(nd.df)]] <- 0 #add all of the column heading
  nd.df <- do.call("rbind", replicate(length(unique(df$yr)), nd.df, simplify = FALSE))
  nd.df$STRM_ORDER <- 1
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
  nd.df$fSTRM_ORDER <- as.factor(nd.df$STRM_ORDER)
  levels(nd.df$fSTRM_ORDER) <- levels(df$fSTRM_ORDER)
  
  

  fit <- output$exploratory$sdm$best_fit
  pred1 <-exp(predict(fit, nd.df)$est)
  # tmp <- cbind(nd.df,pred$est)
  tmp <- cbind(nd.df,pred1)
  names(tmp)[ncol(tmp)] <- "pred1"
  tmp$mod <- "GLMM \n (sdmTMB)"
  p <- rbind(p,tmp)
  
