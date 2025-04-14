library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

#Run through models
plotlist <- list()
rmse_out <- list()

icnt <- 1
library(mgcv)
library(sdmTMB)
library(randomForest)
for(i in c('rear','Spwn')){
  stage <- i
  load(paste0("output/temporal_output_",stage,"_",2021,".rData"))
  
  gam <- (output$project$gam$grid_search) %>%  
    mutate(model = "GAMM") %>% 
    dplyr::select(c(model,n_years_ahead,rmse))
  
  sdm <- (output$project$sdm$grid_search) %>%  
    mutate(model = "GLMM") %>% 
    dplyr::select(c(model,n_years_ahead,rmse))
  
  rf <- output$project$rf$grid_search %>%  
    mutate(model = "Random\nforest") %>% 
    dplyr::select(c(model,n_years_ahead,rmse))
  
  
  rmse_mean[[icnt]] <- dplyr::bind_rows(gam,sdm,rf) %>% 
    dplyr::group_by(model,n_years_ahead) %>%
    dplyr::summarise(rmse_mean = mean(rmse)) %>%
    group_by(model) %>% 
    mutate(min = min(rmse_mean)) %>% 
    mutate(rel_rmse = (rmse_mean-min)/min)
  # dplyr::bind_rows(rmse_mean1)
  
  ifelse(stage=="rear", lims <- c(140,380), lims <- c(8,35))
  
  plotlist[[icnt]] <- ggplot(aes(y = rmse_mean[[icnt]], x = model, fill = as.factor(n_years_ahead)), 
               data = rmse_mean[,]) + 
    geom_bar(position="dodge", stat="identity") +
    scale_fill_grey()+
    ylab("") +
    xlab("") +
    guides(fill=guide_legend(title="Number of \nyears ahead")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    coord_cartesian(ylim=lims)
  icnt <- icnt + 1
}

gg <- ggarrange()

# rmse_mean 
# ggsave(file = "./output/ggplot_predictive_rmse_2017_2021.png", gg, device = "png", dpi = 300, height = 4, width = 6, units="in")

  # dev.off()

