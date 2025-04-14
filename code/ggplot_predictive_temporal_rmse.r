library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

#Run through models
plotlist <- list()
icnt <- 1
endYr <- 2021
for(i in c('rear','Spwn')){
  stage <- i
  load(paste0("output/temporal_output_",stage,"_",endYr,".rData"))
  
  gam <- (output$project$gam$grid_search) %>%  
    mutate(model = "GAMM") %>% 
    dplyr::select(c(model,n_years_ahead,rmse))
  
  sdm <- (output$project$sdm$grid_search) %>%  
    mutate(model = "GLMM") %>% 
    dplyr::select(c(model,n_years_ahead,rmse))
  
  rf <- output$project$rf$grid_search %>%  
    mutate(model = "Random\nforest") %>% 
    dplyr::select(c(model,n_years_ahead,rmse))
  
  
  rmse_mean <- dplyr::bind_rows(gam,sdm,rf) %>% 
    dplyr::group_by(model,n_years_ahead) %>%
    dplyr::summarise(rmse_mean = mean(rmse)) 
  
  rmse_mean$rrmse <- rmse_mean$rmse_mean/min(rmse_mean$rmse_mean)
  rmse_mean$n_years_ahead[rmse_mean$n_years_ahead==0] <- "Exploratory\n(zero years ahead)"
  rmse_mean$n_years_ahead[rmse_mean$n_years_ahead==1] <- "One year ahead"
  rmse_mean$n_years_ahead[rmse_mean$n_years_ahead==2] <- "Two years ahead"
  rmse_mean$n_years_ahead <- as.factor(rmse_mean$n_years_ahead)
  
  ifelse(stage=="rear", lims <- c(140,380), lims <- c(8,40))
  
  plotlist[[icnt]] <- ggplot(aes(y = rmse_mean, x = model, fill = as.factor(n_years_ahead)), 
               data = rmse_mean[,]) + 
    geom_bar(position="dodge", stat="identity") +
    scale_fill_grey()+
    ylab("") +
    xlab("") +
    guides(fill=guide_legend(title="Temporal analysis")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    coord_cartesian(ylim=lims)
  icnt <- icnt + 1
}

gg1 <- ggpubr::ggarrange(plotlist = plotlist,
                        ncol = 2,
                        legend = 'right',
                        labels = c("A","B"),
                        common.legend = TRUE)

gg1 <- ggpubr::annotate_figure(gg1,
                left = ggpubr::text_grob(paste0("Mean RMSE ",endYr - 4," to ", endYr), color = "black", rot = 90),
                fig.lab = "", fig.lab.face = "bold")

print(gg1)
ggsave(file = paste("./output/ggplot_predictive_rmse_",endYr - 4,"_", endYr,".png"), gg, device = "png", dpi = 300, height = 7, width = 5, units="in")
dev.off()

