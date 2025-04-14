library(tidyr)
library(dplyr)

load("analysis_of_top_rivers_rear.Rdata")
comp <- na.omit(comp) %>% 
  pivot_longer(comp, 
               cols = starts_with("mape"),
               names_to = "Method", 
               values_to = "MAPE") %>%
  filter(sample == 1 | Method == "mape_rand_pred") %>%
  group_by(Method) %>%
  summarise(val = mean(abs(MAPE)), sd = sd(abs(MAPE)))

gg <-   ggplot2::ggplot(comp,aes(x=Method, y = val)) +
  geom_col()

print(gg)

  

