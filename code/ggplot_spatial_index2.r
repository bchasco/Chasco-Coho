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
  
  # png('output/spatial_index2.png',
  #     height = 600, width = 600, pointsize = 14)
  
  stage <- "Spwn" #rear or Spwn
  #Grab the data for stage
  df <- function_wrangle_data(stage=stage,
                              dir = root)
  
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
  
  fit <- output$exploratory$gam$best_fit
  # pred <- exp(predict(fit, df))#
  pred1 <- exp(predict(fit, nd.df))
  p <- cbind(nd.df,pred1)
  # p <- cbind(df,pred) 
  p$mod <- "GAMM \n (mgcv)"
  
  ff <- output$exploratory$rf$best_mod
  mtry <- output$exploratory$rf$best_mtry
  ntree <- output$exploratory$rf$best_ntree
  fit <- randomForest(ff,
                      mtry = mtry,
                      ntree = ntree,
                      data = df)
  pred1 <- predict(fit, nd.df)
  tmp <- cbind(nd.df,pred1)
  tmp$mod <- "RF \n (randomForest)"
  p <- rbind(p,tmp)
  # 
  
  fit <- output$exploratory$sdm$best_fit
  mesh <- make_mesh(nd.df, c("UTM_E_km", "UTM_N_km"), cutoff = 10)
  pred1 <-exp(predict(fit, nd.df)$est)
  # tmp <- cbind(nd.df,pred$est)
  tmp <- cbind(nd.df,pred1)
  names(tmp)[ncol(tmp)] <- "pred1"
  tmp$mod <- "GLMM \n (sdmTMB)"
  p <- rbind(p,tmp)
  
  ag <- aggregate(list(est = p$pred1), 
                  by=list(yr = p$yr, mod = p$mod), 
                  sum)

  odfw_est <- read.csv("./data/ESU_Estimates.csv")
  if(stage=="Spwn"){
    odfw <- data.frame(yr=odfw_est$Brood.Year,
                       mod = "odfw",
                       'Estimate' = odfw_est$Spawners)
  }
  if(stage=="rear"){
    odfw <- data.frame(yr = odfw_est$Parr.Year,
                       mod = "odfw",
                       'Estimate' = odfw_est$TotalParr)
  }
  
  names(ag)[ncol(ag)] <- "Estimate"
  ag <- cbind(ag,
              odfw$Estimate[odfw$yr%in%ag$yr])
  names(ag)[ncol(ag)] <- "ODFW"
  ag$stage <- "Spawners"
  

  
  stage <- "rear" #rear or Spwn
  df <- function_wrangle_data(stage=stage,
                              dir = root)
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
  
  
  fit <- output$exploratory$gam$best_fit
  # pred <- exp(predict(fit, df))#
  pred1 <- exp(predict(fit, nd.df))
  p <- cbind(nd.df,pred1)
  # p <- cbind(df,pred) 
  p$mod <- "GAMM \n (mgcv)"
  
  ff <- output$exploratory$rf$best_mod
  mtry <- output$exploratory$rf$best_mtry
  ntree <- output$exploratory$rf$best_ntree
  fit <- randomForest(ff,
                      mtry = mtry,
                      ntree = ntree,
                      data = df)
  pred1 <- predict(fit, nd.df)
  tmp <- cbind(nd.df,pred1)
  tmp$mod <- "Random forest \n (randomForest)"
  p <- rbind(p,tmp)
  # 
  
  fit <- output$exploratory$sdm$best_fit
  pred1 <-exp(predict(fit, nd.df)$est)
  # tmp <- cbind(nd.df,pred$est)
  tmp <- cbind(nd.df,pred1)
  names(tmp)[ncol(tmp)] <- "pred1"
  tmp$mod <- "GLMM \n (sdmTMB)"
  p <- rbind(p,tmp)
  
  ag2 <- aggregate(list(est = p$pred1), by=list(yr = p$yr, mod = p$mod), sum)
  odfw_est <- read.csv("./data/ESU_Estimates.csv")
  if(stage=="Spwn"){
    odfw <- data.frame(yr=odfw_est$Brood.Year,
                       mod = "odfw",
                       'Estimate' = odfw_est$Spawners)
  }
  if(stage=="rear"){
    odfw <- data.frame(yr=odfw_est$Parr.Year,
                       mod = "odfw",
                       'Estimate' = odfw_est$TotalParr)
  }
  
  names(ag2)[ncol(ag2)] <- "Estimate"
  ag2 <- cbind(ag2,
              odfw$Estimate[odfw$yr%in%ag2$yr])
  names(ag2)[ncol(ag2)] <- "ODFW"
  ag2$stage <- "Juvenile"
  
  ag3 <- rbind(ag,
              ag2)
  
  g <- ggplot(ag3, aes(x = ODFW,
                      y = Estimate,
                      colour = mod)) +
    geom_point(size = 3) +
    facet_wrap(~stage, nrow=2, scales = "free")+
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
    labs(color = "Model estimate")+
    ylab("Model estimate") +
    xlab("ODFW estimate")
  # 
  print(g)
  


  # dev.off()
# 
#   return(nd.df)  
# }

  