library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

#Run through models

plotlist <- list()
dataType <- "spatial"
endYr <- 2021
icnt <- 1
for(i in c("rear","Spwn")){
  rmse_mean <- NA
  for(sp in c("spatial",'temporal')){
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
        mutate(model = "Random forest") %>% 
        dplyr::select(c(model,survey_type,rmse))
      
      if(sum(is.na(rmse_mean))){
        rmse_mean <- dplyr::bind_rows(gam,sdm,rf) %>% 
          dplyr::group_by(model,survey_type) %>%
          dplyr::summarise(rmse_mean = mean(rmse)) #%>%
        
        rmse_mean <- as.data.frame(rmse_mean)
        rmse_mean$survey_type <- unlist(lapply(rmse_mean$survey_type, function(x){paste(x,collapse = ",")}))
        
        rmse_mean$survey <- as.factor(rmse_mean$survey)
        levels(rmse_mean$survey) <- c("Exploratory", "No years ahead\nAnnual sites", "No years ahead\nAnnual + Tri-annual sites", "No years ahead\nIndex sites")
      }else{
        tmp <- dplyr::bind_rows(gam,sdm,rf) %>% 
          dplyr::group_by(model,survey_type) %>%
          dplyr::summarise(tmp = mean(rmse)) #%>%
        
        tmp <- as.data.frame(tmp)
        tmp$survey_type <- unlist(lapply(tmp$survey_type, function(x){paste(x,collapse = ",")}))
        
        tmp$survey <- as.factor(tmp$survey)
        levels(tmp$survey) <- c("Exploratory", "No years ahead\nAnnual sites", "No years ahead\nAnnual + Tri-annual sites", "No years ahead\nIndex sites")

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
        mutate(model = "Random forest") %>% 
        dplyr::select(c(model,n_years_ahead,rmse))
      
      if(sum(is.na(rmse_mean))){
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
          dplyr::summarise(rmse_mean = mean(rmse)) 
        
        names(tmp)[which(names(tmp)=="n_years_ahead")] <- "survey_type"
        
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
  plotlist[[icnt]] <- ggplot(aes(y = rmse_mean, x = model, fill = (survey)),
                             data = rmse_mean) +
    geom_bar(position="dodge", stat="identity") +
    scale_fill_viridis_d()+
    ylab("") +
    xlab("") +
    guides(fill=guide_legend(title="Predicted data")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    coord_cartesian(ylim=lims)

  # print(p)
  icnt <- icnt + 1
  
}

gg <- ggpubr::ggarrange(plotlist = plotlist,
                        ncol = 1,
                        legend = 'right',
                        labels = c("A","B"),
                        common.legend = TRUE)
#
gg <- ggpubr::annotate_figure(gg,
                left = ggpubr::text_grob(paste0("Mean RMSE for predictions ",endYr-4," to ",endYr), color = "black", rot = 90),
                fig.lab = "", fig.lab.face = "bold")


print(gg)

# ggsave(file = paste("./output/ggplot_predictive_rmse_",endYr - 4,"_", endYr,".png"), gg, device = "png", dpi = 300, height = 7, width = 7, units="in")

# dev.off()
# 
