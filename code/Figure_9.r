#Get the observed data
library(sdmTMB)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
load("./output/output_rear.rData")
obs_data <- output$exploratory$sdm$best_fit$data
mod <- output$exploratory$sdm$best_mod
st <- output$exploratory$sdm$best_st
sp <- output$exploratory$sdm$best_sp
  

#These rivers are based on the analysis in 
#rear_All_pops_RMSE.png, in the script 
high_rivers_rear <- c("Nestucca", "Siuslaw", 'Lower Umpqua', 'Coos', "South Umpqua")
low_rivers_rear <- c("Beaver", "Siltcoos", 'Alsea', 'Necanicum', "Salmon")
rear_list <- list(high_RMSE_rivers = high_rivers_rear,
                  low_RMSE_rivers = low_rivers_rear,
                  all_rivers = unique(obs_data$PopGrp))
Panel_list <- list(all_panels = unique(obs_data$Panel),
                   all_panels = unique(obs_data$Panel),
                   annual_triannual = c("annual","annua","three"))

icnt <- 1
rivers <- unique(obs_data$PopGrp)
rivers <- rivers[!(1:length(rivers))%in%grep("Dependent", rivers)]

recreate_analysis <- TRUE

if(recreate_analysis){
  yrs <- 2017:2021
  for(yr_i in yrs){
    
    #Fit to the full data set
    obs_mesh <- sdmTMB::make_mesh(obs_data[obs_data$yr <= yr_i,], 
                                  xy_cols = c("UTM_E_km","UTM_N_km"), cutoff = 10)
    obs_fit <- sdmTMB(mod, 
                      data = obs_data[obs_data$yr <= yr_i,],
                      mesh = obs_mesh,
                      family = tweedie(link = "log"),
                      time = "yr",
                      spatial = sp,
                      spatiotemporal = st,
                      anisotropy = TRUE,
                      # extra_time = unique(obs_data$yr[obs_data$yr <= yr_i]), #Why is this necessary if the years are the same?
                      silent=TRUE)
    
    
    #loop over the rivers for the year of interest  
    for(rr in rivers){ #
      print(paste(rr,yr_i))
      #Make sure that the year of interest has been sampled for this river
      if(yr_i%in%unique(obs_data$yr[obs_data$PopGrp==rr])){
        
        #create the prediction data set
        pred_data <- obs_data[obs_data$yr <= yr_i, ]
        #fill in missing years with dummy data
        missing_years <- !(unique(pred_data[,]$yr)%in%unique(pred_data[pred_data$PopGrp==rr,]$yr))
        if(sum(missing_years)>0){
          missing_pred <- do.call(rbind,lapply(1:sum(missing_years==TRUE), function(x) pred_data[pred_data$PopGrp==rr,][1,]))
          missing_pred$yr <- unique(pred_data[,]$yr)[missing_years==TRUE]
          missing_pred[,6:17] <- 0
          pred_data <- rbind(pred_data,
                             missing_pred)
        }
        
        #Indexes for the full data      
        obs_pred <- predict(obs_fit, 
                            newdata = pred_data[pred_data$PopGrp==rr,], 
                            return_tmb_object=TRUE,
                            re_form_iid = NA)
        if(yr_i==min(yrs)){
          ind_with_full_data <- sdmTMB::get_index(obs_pred) %>% 
            filter(yr <= yr_i) %>% 
            mutate(data = "full",
                   cen_river = "all_rivers",
                   river = rr,
                   panel = "all_panels")
        }else{
          ind_with_full_data <- sdmTMB::get_index(obs_pred) %>% 
            filter(yr == yr_i) %>% 
            mutate(data = "full",
                   cen_river = "all_rivers",
                   river = rr,
                   panel = "all_panels")
        }
        
        for(cen_rivers in 1:length(rear_list)){
          #Create the censored dataset  
          # print(paste(yr_i,rr,cen_rivers))
          data_before_year_of_interest <- obs_data %>% 
            filter(yr < min(yrs))
          data_after_year_of_interest <- obs_data[obs_data$yr >= min(yrs) & obs_data$yr <= yr_i, ] %>% 
            filter((PopGrp %in% rear_list[[cen_rivers]])) %>% 
            filter(Panel %in% Panel_list[[cen_rivers]])
          
          censored_data <- rbind(data_before_year_of_interest,
                                 data_after_year_of_interest)
          #Fit to the cencored data
          cen_mesh <- sdmTMB::make_mesh(censored_data, 
                                        xy_cols = c("UTM_E_km","UTM_N_km"), cutoff = 10)
          cen_fit <- sdmTMB(mod, 
                            censored_data,
                            mesh = cen_mesh,
                            family = tweedie(link = "log"),
                            time = "yr",
                            spatial = sp,
                            spatiotemporal = st,
                            anisotropy = TRUE,
                            extra_time = unique(obs_data$yr[obs_data$yr <= yr_i]), #Why is this necessary if the years are the same?
                            silent=TRUE)
          #indexes for the censored data
          cen_pred <- predict(cen_fit, 
                              newdata = pred_data[pred_data$PopGrp==rr,], 
                              return_tmb_object=TRUE, 
                              re_form_iid = NA)
          ind_with_censored_data <- sdmTMB::get_index(cen_pred) %>% 
            filter(yr == yr_i) %>% 
            mutate(data = "censored",
                   cen_river = names(rear_list)[cen_rivers],
                   river = rr,
                   panel = names(Panel_list)[cen_rivers])
          
          #Store the output 
          if(icnt == 1){
            out <- rbind(ind_with_censored_data,
                         ind_with_full_data)
          }else{
            if(cen_rivers==1){
              out <- rbind(out,
                           ind_with_full_data,
                           ind_with_censored_data)
            }else{
              out <- rbind(out,
                           ind_with_censored_data)
            }
          }
          icnt <- icnt + 1
        }
      }
    }
  }
  # saveRDS(out, file = "output/Figure_9_rear.rds")
}else{
  # out <- readRDS("output/Figure_9_rear.rds")
}

list_num <- 2

g1 <-  out %>%
  filter(yr >= 2017) %>% 
  # mutate(obs = obs$sum[match(river, obs$river)]) %>%
  # ggplot(aes(x = yr, y = log_est, group = paste(cen_river,panel), color = paste(cen_river,panel))) +
  # geom_errorbar(aes(ymin = log_est - 1.96*se, ymax = log_est + 1.96 * se, color = paste(cen_river,panel)), position = position_dodge(width = 0.5), width = 0.2, size = 1.) +
  ggplot(aes(x = yr, y = est, group = paste(cen_river,panel), color = paste(cen_river,panel))) +
  geom_errorbar(aes(ymin = lwr, ymax = upr, color = paste(cen_river,panel)), position = position_dodge(width = 0.5), width = 0.2, size = 1.) +
  facet_wrap(~river, ncol = 1, scales = "free") +
  geom_point(aes(),position = position_dodge(width = 0.5), size = 2.5, alpha = 1) +
  theme_bw() +
  xlab("Juvenile year") +
  labs(color = "Populations surveyed") +
  ylab("Juvenile abundance") +
  theme(panel.grid = element_blank(), ) +
  theme(text = element_text(size = 12)) +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(legend.position = "none")

#Now estimate the slope of the last five years considering the errors in variables

trend_out <- as.data.frame(matrix(0,1,7))
names(trend_out) <- c("River","Panels","Censor","trend","trend.sd","status","status.sd")

icnt <- 1
for(r in unique(out$river)){
  for(m in unique(out$panel)){
    for(cc in unique(out$cen_river)){
      d <- out[out$river == r&
                 out$panel==m & 
                 out$cen_river == cc &
                 out$yr >= 2017,]
      if(nrow(d)>1){
        # mod <- lm(log_est~yr, data = d, weights = 1/(se))
        trend <- lm(est~yr, data = d, weights = 1/(est*se))
        status <- lm(est~1, data = d, weights = 1/(est*se))
        trend_out[icnt,] <- c(r,m,cc,
                              (summary(trend)$coefficients['yr',1]),(summary(trend)$coefficients['yr',2]),
                              (summary(status)$coefficients[,1]),(summary(status)$coefficients[,2]))
        icnt <- icnt + 1
      }      
    }
  }
}

trend_out$exp <- paste(trend_out$Censor,"\n",trend_out$Panels)
trend_out$trend <- as.numeric(trend_out$trend)
trend_out$trend.sd <- as.numeric(trend_out$trend.sd)
trend_out$status <- as.numeric(trend_out$status)
trend_out$status.sd <- as.numeric(trend_out$status.sd)
trend_out$trend.upr <- trend_out$trend + 1.96 * trend_out$trend.sd
trend_out$trend.lwr <- trend_out$trend - 1.96 * trend_out$trend.sd
trend_out$status.upr <- trend_out$status + 1.96 * trend_out$status.sd
trend_out$status.lwr <- trend_out$status - 1.96 * trend_out$status.sd

plot_data <- trend_out[rep(1:nrow(trend_out),each = 1000),]
plot_data$trend.x <- 0
plot_data$trend.y <- 0
plot_data$status.x <- 0
plot_data$status.y <- 0
for(r in unique(plot_data$River)){
  for(e in unique(plot_data$exp)){
    
    plot_data$status.x[plot_data$River==r & plot_data$exp==e] <- seq(min(plot_data$status[plot_data$River==r & plot_data$exp==e]-3*plot_data$status.sd[plot_data$River==r & plot_data$exp==e]),
                                                                     max(plot_data$status[plot_data$River==r & plot_data$exp==e]+3*plot_data$status.sd[plot_data$River==r & plot_data$exp==e]),
                                                                     length.out= 1000)
    plot_data$status.y[plot_data$River==r& plot_data$exp==e] <- rnorm(1000,
                                                                      plot_data$status[plot_data$River==r & plot_data$exp==e],
                                                                      plot_data$status.sd[plot_data$River==r & plot_data$exp==e])
    plot_data$trend.x[plot_data$River==r & plot_data$exp==e] <- seq(min(plot_data$trend[plot_data$River==r & plot_data$exp==e]-3*plot_data$trend.sd[plot_data$River==r & plot_data$exp==e]),
                                                                    max(plot_data$trend[plot_data$River==r & plot_data$exp==e]+3*plot_data$trend.sd[plot_data$River==r & plot_data$exp==e]),
                                                                    length.out= 1000)
    plot_data$trend.y[plot_data$River==r& plot_data$exp==e] <- rnorm(1000,
                                                                     plot_data$trend[plot_data$River==r & plot_data$exp==e],
                                                                     plot_data$trend.sd[plot_data$River==r & plot_data$exp==e])
  }
}

g2 <- plot_data %>% 
  ggplot(aes(y = as.factor(exp), x =  status.y)) +
  facet_wrap(~River, ncol = 1, scales = "free") +
  geom_violin(aes(color = exp, color = exp, fill = exp), alpha = 0.7) +
  theme_bw() +
  ylab("") +
  xlab("Ave. juvenile abund. 2017- 2021") +
  theme(panel.grid = element_blank(), ) +
  theme(text = element_text(size = 12)) +
  # theme(axis.text.y = element_blank()) +
  theme(legend.position = "none") +
  geom_vline(xintercept = 0) 

g3 <- plot_data %>% 
  ggplot(aes(y = as.factor(exp), x =  trend.y)) +
  facet_wrap(~River, ncol = 1, scales = "free") +
  geom_violin(aes(color = exp, color = exp, fill = exp), alpha = 0.7) +
  theme_bw() +
  ylab("") +
  xlab("Annual juvenile change 2017- 2021") +
  theme(panel.grid = element_blank(), ) +
  theme(text = element_text(size = 12)) +
  theme(legend.position = "none") +
  geom_vline(xintercept = 0) 


g <- ggarrange(g1,g2,g3,ncol = 3)
ggsave(g, file = "output/Figure_9.png", width = 10, height = 10)
