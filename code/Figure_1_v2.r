library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

#Run through models

plotlist <- list()
rmse_mean <- list()
dataType <- "spatial"
endYr <- 2021
icnt <- 1
out <- list()
for(i in c("rear",'Spwn')){

  #Load the projections  
  load(paste0("output/",'spatial',"_output_",i,"_",endYr,".rData"))
  
  gam <- output$project$gam$grid_search %>%  
    mutate(model = "GAMM") %>% 
    dplyr::select(c(model,n_years_ahead,survey_GRTS_type, rmse))
  
  sdm <- output$project$sdm$grid_search %>%  
    mutate(model = "GLMM") %>% 
    dplyr::select(c(model,n_years_ahead,survey_GRTS_type, rmse))
  
  rf <- output$project$rf$grid_search %>%  
    mutate(model = "Random forest") %>% 
    dplyr::select(c(model,n_years_ahead,survey_GRTS_type, rmse))
  
  df <- dplyr::bind_rows(gam,sdm,rf)   %>% 
    mutate(survey_GRTS_type = as.character(survey_GRTS_type))
  
  
  #load the exploratory
  load(paste0("output/",'temporal',"_output_",i,"_",endYr,".rData"))
  
  gam <- output$exploratory$gam$grid_search %>%  
    mutate(model = "GAMM") %>% 
    mutate(survey_GRTS_type = "Train") %>% 
    dplyr::select(c(model,n_years_ahead,survey_GRTS_type,rmse))
  
  sdm <- output$exploratory$sdm$grid_search %>%  
    mutate(model = "GLMM") %>% 
    mutate(survey_GRTS_type = "Train") %>% 
    dplyr::select(c(model,n_years_ahead,survey_GRTS_type, rmse))
  
  rf <- output$exploratory$rf$grid_search %>%  
    mutate(model = "Random forest") %>% 
    mutate(survey_GRTS_type = "Train") %>% 
    dplyr::select(c(model,n_years_ahead,survey_GRTS_type, rmse))
  
  df <- dplyr::bind_rows(df,gam,sdm,rf) 
  
  
  df$survey_GRTS_type[df$survey_GRTS_type == 'c("annua", "annual", "Index", "nine", "once", "Supplemental", "three")'] <- "All panels"
  df$survey_GRTS_type[df$survey_GRTS_type == 'c("annual", "annua", "three")'] <- "Annual + triennial"
  df$survey_GRTS_type[df$survey_GRTS_type == 'c("annual", "annua")'] <- "Annual"
  df$survey_GRTS_type[df$survey_GRTS_type == 'c("Index)'] <- "Index"
  df$model[df$model=="Random Forest"] <- "RF"
  df$survey <- df$survey_GRTS_type    
  df <- df %>% 
    mutate(survey = factor(survey, levels = c("Train", "All panels", "Annual + triennial", "Annual", "Index")))
  df$stage <- i
 out[[i]] <- df 
 
 df <- out %>% 
   bind_rows()
 
 tmp <- df %>% 
   group_by(model, n_years_ahead,survey,stage) %>%  
   summarize(rmse = mean(rmse)) %>% 
   group_by(n_years_ahead,survey,stage) %>%  
   mutate(rel_rmse = rmse)#(rmse - min(rmse))/min(rmse))
 
 

}
        

 gg <- ggplot(tmp,aes(x = survey, y = rel_rmse, fill = model)) +
  geom_col(position = position_dodge()) +
  facet_wrap(n_years_ahead~stage, ncol = 2, scales = "free") +
  theme_classic()

# ggsave(file = paste0("./output/Figure_1_",endYr - 4,"_", endYr,".png"), gg, device = "png", dpi = 300, height = 7, width = 7, units="in")

