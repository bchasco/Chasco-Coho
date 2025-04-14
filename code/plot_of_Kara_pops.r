out <- readRDS("rear_kara_pops.rds")
# out <- output$project$sdm$grid_search
out$n_years_ahead <- paste(out$n_years_ahead, "years forecasted ahead")
out$survey_pop_type <- unlist(lapply(out$survey_pop_type, function(x) return(paste(x, collapse = ","))))
out$survey_GRTS_type <- unlist(lapply(out$survey_GRTS_type,function(x)return(paste(x,collapse = ","))))
out$survey_GRTS_type[out$survey_GRTS_type=="annual,annua"] <- "Annual"
out$survey_GRTS_type[out$survey_GRTS_type=="annual,annua,three"] <- "Annual + Three"
out$survey_GRTS_type[out$survey_GRTS_type=="annua,annual,Index,nine,once,Supplemental,three"] <- "All GRTS"
out$survey_pop_type[out$survey_pop_type!="All pops"] <- "Kara pops"
out$survey_pop_type[out$survey_pop_type=="All pops"] <- "All pops"
out$survey_GRTS_type <- factor(out$survey_GRTS_type, levels = c("All GRTS", "Annual + Three", "Annual"))

out_df <- out %>%
group_by(n_years_ahead, survey_GRTS_type, survey_pop_type) %>%
summarise(mean = mean(rmse))

g <- ggplot(out_df, aes(y = mean, x = survey_GRTS_type)) +
geom_col(aes(fill = survey_pop_type), position = "dodge") +
facet_wrap(~n_years_ahead, ncol = 1, 
             scales = 'free_y', 
             drop = TRUE) +
  coord_cartesian(ylim=c(290,NA)) +
  ylab("Mean RMSE (2017 to 2021)") +
  xlab("Survey design") + 
  theme_bw()# Set y-axis limits
print(g)
ggsave(g, file = "Spwn_Kara_populations.png", width = 8, height = 10, units = "in")
