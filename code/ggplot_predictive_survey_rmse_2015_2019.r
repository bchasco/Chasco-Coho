library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

#Run through models
plotlist <- list()
icnt <- 1
maxYr <- 2019
for(i in c('rear','Spwn')){
  stage <- i
  load(paste0("output/spatial_output_",i,"_",maxYr,".rData"))
  

  gam <- output$project$gam$grid_search %>%  
    mutate(model = "GAMM") %>% 
    dplyr::select(c(model,survey_type,rmse))

  sdm <- output$project$sdm$grid_search %>%  
    mutate(model = "GLMM") %>% 
    dplyr::select(c(model,survey_type,rmse))
  
  rf <- output$project$rf$grid_search %>%  
    mutate(model = "Random Foret") %>% 
    dplyr::select(c(model,survey_type,rmse))

  rmse_mean <- dplyr::bind_rows(gam,sdm,rf) %>% 
    dplyr::group_by(model,survey_type) %>%
    dplyr::summarise(rmse_mean = mean(rmse)) #%>%
  
  rmse_mean <- as.data.frame(rmse_mean)
  rmse_mean$survey_type <- unlist(lapply(rmse_mean$survey_type, function(x){paste(x,collapse = ",")}))

  rmse_mean$survey <- as.factor(rmse_mean$survey)
  levels(rmse_mean$survey) <- c("Exploratory \n (All sites)", "Annual sites", "Annual + Three sites", "Index sites")
  
  ifelse(stage=="rear", lims <- c(140,380), lims <- c(8,40))
  
  plotlist[[icnt]] <- ggplot(aes(y = rmse_mean, x = model, fill = (survey)),
               data = rmse_mean) +
    geom_bar(position="dodge", stat="identity") +
    scale_fill_grey()+
    ylab("") +
    xlab("") +
    guides(fill=guide_legend(title="Spatial analysis")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    coord_cartesian(ylim=lims)

  # print(p)
  icnt <- icnt + 1
  
}

gg <- ggpubr::ggarrange(plotlist = plotlist,
                        ncol = 2,
                        legend = 'right',
                        labels = c("A","B"),
                        common.legend = TRUE)

gg <- ggpubr::annotate_figure(gg,
                left = ggpubr::text_grob(paste0("Mean RMSE Survery ",maxYr-4," to ",maxYr), color = "black", rot = 90),
                fig.lab = "", fig.lab.face = "bold")

print(gg)
ggsave(file = "./output/ggplot_predictive_survey_rmse_2015_2019.png", gg, device = "png", dpi = 300, height = 4, width = 6, units="in")
dev.off()
