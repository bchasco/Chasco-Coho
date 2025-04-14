library(ggplot2)
#Run through models
load("C:/noaa/projects/ODFW-Coho/output/output_Spwn.rData")
assign("gam",output$project$gam$grid_search)
gam <- gam %>%  
  mutate(mod = "gam") %>% 
  select(c(mod,n_years_ahead,rmse))

assign("sdm",output$project$sdm$grid_search)
sdm <- sdm %>%  
  mutate(mod = "sdm") %>% 
  select(c(mod,n_years_ahead,rmse))

assign("rf",output$project$rf$grid_search)
rf <- rf %>%  
  mutate(mod = "rf") %>% 
  select(c(mod,n_years_ahead,rmse))

rmse_mean <- bind_rows(gam,sdm,rf) %>% 
  group_by(mod,n_years_ahead) %>%
  summarize(rmse = mean(rmse, na.rm = TRUE))

g <- ggplot(aes(y = rmse, x = mod, fill = as.factor(n_years_ahead)), 
            data = rmse_mean) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Five-year mean RMSE") +
  xlab(" Model ") +
  guides(fill=guide_legend(title="Number of \nyears ahead"))

print(g)
