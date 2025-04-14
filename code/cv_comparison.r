#These files are outputted from forecast watersheds
rear <- readRDS("./output/rear_survey_abundance.rds")[[1]] %>% mutate('Life_Stage' = "Juvenile" )
spwn <- readRDS("./output/spwn_survey_abundance.rds")[[1]] %>% mutate('Life_Stage' = "Spawners")


options(dplyr.summarise.inform = FALSE)

arr <- bind_rows(rear, spwn)  %>%  
  group_by(Life_Stage, pi) %>%
  summarise(CV = mean(sqrt(exp(se^2))-1),            
            KS = ks.test(unlist(est),unlist(ind))$p.value) %>%
  arrange(desc(KS), .by_group = TRUE) %>%
  mutate(KS_col = ifelse(row_number(KS)<=10, FALSE, TRUE)) %>% #all the top models are now 2
  arrange(CV, .by_group = TRUE) %>%
  mutate(CV_col = ifelse(row_number(CV)<=10, TRUE, FALSE)) %>% #all the top models are now 2
  mutate(col = ifelse(KS_col & CV_col, TRUE, FALSE)) %>% #all the top models are now 2
  arrange(desc(KS), .by_group = TRUE) %>%
  pivot_longer(cols = c("KS", "CV"),
               names_to = "stat")

l <- unique(arr$Life_Stage)
stat <- unique(arr$stat)
  ggplot(data = arr[arr$Life_Stage==l[1] & arr$stat==stat[2],],
         aes(x = reorder(pi, (value)), y = value)) + 
    geom_col(aes(fill = col))
  
# arr_cv <- arr_KS %>%
#   arrange(mean_cv, .by_group = TRUE)

# spwn_arr <- spwn[,] %>%
#   mutate(Lifestage = "Spawners") %>%
#   %>% group_by(pi) %>%
#   summarise(mean_cv = mean(sqrt(exp(se^2))-1),
#             KS = ks.test(unlist(est),unlist(ind))$p.value,
#   ) %>%
#   mutate(Life_stage = 'Spawners') %>%
#   mutate(col = 1) %>% 
#   pivot_longer()
# 
# spwn_KS <- spwn_arr %>%
#   arrange(desc(KS))
# spwn_cv <- spwn_arr %>%
#   arrange(ad)
# 
# spwn_arr$col[1:10][spwn_arr$pi[1:10]%in%rear_arr$pi[1:10]] <- 2
# rear_arr$col[1:10][rear_arr$pi[1:10]%in%spwn_arr$pi[1:10]] <- 2
# 

rr <- ggplot(rear_arr, aes(x = reorder(pi,desc(KS)), y = KS)) + 
  geom_col(aes(fill=col)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust = 0.3)) +
  xlab('') +
  ylab('') +
  ylim(0,1)

ss <- ggplot(spwn_arr, aes(x = reorder(pi,desc(KS)), y = KS)) + 
  geom_col(aes(fill=col)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust = 0.3)) +
  xlab('') +
  ylab('') +
  ylim(0,1)


cvplot <-ggpubr::ggarrange(rr,ss, labels = c("A","B"),
                  ncol = 1,
                  hjust = -5,
                  legend = 'none')
# Annotate the figure by adding a common labels
png("cv_comparison.png", height = 400, width = 300)

ggpubr::annotate_figure(cvplot,
            bottom = ggpubr::text_grob("Population group"),
            left = ggpubr::text_grob("Average CV", rot = 90))

dev.off()


