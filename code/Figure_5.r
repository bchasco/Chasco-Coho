#Get the observed data
rm(list=ls())
load("./output/temporal_output_spwn_2021.rData")
obs_data <- output$exploratory$sdm$best_fit$data
mod <- output$exploratory$sdm$best_mod
st <- output$exploratory$sdm$best_st
sp <- output$exploratory$sdm$best_sp

obs_mesh <- sdmTMB::make_mesh(obs_data[obs_data$yr <= 2021,],
                              xy_cols = c("UTM_E_km","UTM_N_km"), cutoff = 10)
obs_fit <- sdmTMB(mod,
                  data = obs_data[obs_data$yr <= 2021,],
                  mesh = obs_mesh,
                  family = tweedie(link = "log"),
                  time = "yr",
                  spatial = sp,
                  spatiotemporal = st,
                  anisotropy = TRUE,
                  extra_time = unique(obs_data$yr), #Why is this necessary if the years are the same?
                  silent=TRUE)

icnt <- 1
for(p_i in na.omit(unique(obs_data$PopGrp))){
        #Indexes for the full data      
        newData <- obs_data[obs_data$PopGrp==p_i,]
        
        yrs <- unique(obs_data$yr)
        yrs_not_found <- yrs[!(yrs%in%unique(newData$yr))]
        if(length(yrs_not_found)>0){
          # break;
        }
        
        pred <- predict(obs_fit, 
                            newdata = newData, 
                            return_tmb_object=TRUE,
                            re_form_iid = NA)
        if(icnt == 1){
          ind <- cbind(sdmTMB::get_index(pred),p_i)
        }else{
          ind <- rbind(ind,
                       cbind(sdmTMB::get_index(pred),p_i))
        }
        print(icnt)
        icnt <- icnt + 1
}

g <- ind %>% 
  filter(!(p_i %in% c("MCDependent","NCDependent","MSDependent","Tenmile", "Tahkenitch", "Siltcoos"))) %>% 
  ggplot(aes(x = yr, y = est)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  facet_wrap(~p_i, scales = "free_y", ncol = 3) +
  theme_bw() +
  ylab("Estimated abundance for surveyed locations") + 
  xlab("Survey year") +
  theme(panel.grid =  element_blank())

obs_sum <- obs_data %>% 
  filter(!(PopGrp %in% c("MCDependent","NCDependent","MSDependent","Tenmile", "Tahkenitch", "Siltcoos"))) %>% 
  mutate(p_i = PopGrp) %>% 
  group_by(p_i, yr) %>% 
  summarize(sum = sum(dens)) 
  
g <- g +
  geom_point(data = obs_sum, aes(x = yr, y = sum), color = "red", size = 1.5, alpha = 0.5) 

print(g)
ggsave(filename = "./output/Figure_5.tiff", g, width = 8, height = 10, units = "in", dpi = 300)


ind_2 <- ind %>% 
  filter(!(p_i %in% c("MCDependent","NCDependent","MSDependent","Tenmile", "Tahkenitch", "Siltcoos"))) %>% 
  mutate(obs = obs_sum$sum[match(paste(p_i,yr),paste(obs_sum$p_i,obs_sum$yr))]
  )

print("Percent of spawner predictions out the 95%CI")
print(sum(ind_2$obs > ind_2$upr | ind_2$obs < ind_2$lwr)/nrow(ind_2))
