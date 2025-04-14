library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

#Run through models

plotlist <- list()
icnt <- 1
dataType <- "spatial"
endYr <- 2021
rmse_mean <- NA
for(sp in c("spatial",'temporal')){
  for(i in c("rear")){
    stage <- i
    load(paste0("output/",sp,"_output_",i,"_",endYr,".rData"))
    
    ifelse(stage=="rear", lims <- c(140,380), lims <- c(8,35))
    
    if(sp=="spatial"){
      gam <- output$project$gam$grid_search %>%  
        mutate(model = "GAMM") %>% 
        dplyr::select(c(model,survey_type,rmse))
      
      sdm <- output$project$sdm$grid_search %>%  
        mutate(model = "GLMM") %>% 
        dplyr::select(c(model,survey_type,rmse))
      
      rf <- output$project$rf$grid_search %>%  
        mutate(model = "Random Foret") %>% 
        dplyr::select(c(model,survey_type,rmse))
      
      if(is.na(rmse_mean)){
        rmse_mean <- dplyr::bind_rows(gam,sdm,rf) %>% 
          dplyr::group_by(model,survey_type) %>%
          dplyr::summarise(rmse_mean = mean(rmse)) #%>%
        
        rmse_mean <- as.data.frame(rmse_mean)
        rmse_mean$survey_type <- unlist(lapply(rmse_mean$survey_type, function(x){paste(x,collapse = ",")}))
        
        rmse_mean$survey <- as.factor(rmse_mean$survey)
        levels(rmse_mean$survey) <- c("Exploratory", "One year ahead\nAnnual sites", "One year ahead\nAnnual + Three sites", "One year ahead\nIndex sites")
      }else{
        tmp <- dplyr::bind_rows(gam,sdm,rf) %>% 
          dplyr::group_by(model,survey_type) %>%
          dplyr::summarise(tmp = mean(rmse)) #%>%
        
        tmp <- as.data.frame(tmp)
        tmp$survey_type <- unlist(lapply(tmp$survey_type, function(x){paste(x,collapse = ",")}))
        
        tmp$survey <- as.factor(tmp$survey)
        levels(tmp$survey) <- c("Exploratory", "One year ahead\nAnnual sites", "One year ahead\nAnnual + Three sites", "One year ahead\nIndex sites")

        rmse_mean <- rbind(rmse_mean,tmp)
        
      }
    }
    if(sp=="temporal"){
      gam <- (output$project$gam$grid_search) %>%  
        mutate(model = "GAMM") %>% 
        dplyr::select(c(model,n_years_ahead,rmse))
      
      sdm <- (output$project$sdm$grid_search) %>%  
        mutate(model = "GLMM") %>% 
        dplyr::select(c(model,n_years_ahead,rmse))
      
      rf <- output$project$rf$grid_search %>%  
        mutate(model = "Random\nforest") %>% 
        dplyr::select(c(model,n_years_ahead,rmse))
      
      if(is.na(rmse_mean)){
        rmse_mean <- dplyr::bind_rows(gam,sdm,rf) %>% 
          dplyr::group_by(model,n_years_ahead) %>%
          dplyr::summarise(rmse_mean = mean(rmse)) 
        
        names(rmse_mean)[which(names(rmse_mean)=="survey_type")] <- "survey_type"
        
        rmse_mean$survey <- rmse_mean$survey_type
        # rmse_mean$rrmse <- rmse_mean$rmse_mean/min(rmse_mean$rmse_mean)
        rmse_mean$survey[rmse_mean$survey==0] <- "Exploratory"
        rmse_mean$survey[rmse_mean$survey==1] <- "One year ahead\nall sites"
        rmse_mean$survey[rmse_mean$survey==2] <- "Two years ahead\nall sites"
        rmse_mean$survey <- as.factor(rmse_mean$survey)
        
      }else{
        tmp <- dplyr::bind_rows(gam,sdm,rf) %>% 
          dplyr::group_by(model,n_years_ahead) %>%
          dplyr::summarise(tmp = mean(rmse)) 
        
        names(tmp)[which(names(tmp)=="survey_type")] <- "survey_type"
        
        tmp$survey <- tmp$survey_type
        # tmp$rrmse <- tmp$tmp/min(tmp$tmp)
        tmp$survey[tmp$survey==0] <- "Exploratory"
        tmp$survey[tmp$survey==1] <- "One year ahead\nall sites"
        tmp$survey[tmp$survey==2] <- "Two years ahead\nall sites"
        tmp$survey <- as.factor(tmp$survey)
        
        rmse_mean <- rbind(rmse_mean,tmp)
        
      }
    }    
  }
  

  ifelse(stage=="rear", lims <- c(140,380), lims <- c(8,40))
  # plotlist[[icnt]] <- ggplot(aes(y = rmse_mean, x = model, fill = (survey)),
  #                            data = rmse_mean) +
  #   geom_bar(position="dodge", stat="identity") +
  #   scale_fill_grey()+
  #   ylab("") +
  #   xlab("") +
  #   guides(fill=guide_legend(title="Spatial analysis")) +
  #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #   coord_cartesian(ylim=lims)
  # 
  # # print(p)
  # icnt <- icnt + 1
  
}
# 
# gg <- ggpubr::ggarrange(plotlist = plotlist,
#                         ncol = 2,
#                         legend = 'right',
#                         labels = c("A","B"),
#                         common.legend = TRUE)
# # 
# gg <- ggpubr::annotate_figure(gg,
#                 left = ggpubr::text_grob("Mean RMSE Survery 2015 to 2019", color = "black", rot = 90),
#                 fig.lab = "", fig.lab.face = "bold")
# 
# 
# print(gg)
# 
# ggsave(file = paste("./output/ggplot_predictive_temporal_rmse_",endYr - 4,"_", endYr,".png"), gg, device = "png", dpi = 300, height = 4, width = 6, units="in")
# 
# dev.off()
# 
