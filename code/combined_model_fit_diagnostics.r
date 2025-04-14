#rear 
spwn <- readRDS("./output/spwn_survey_abundance.rds")[[1]] %>%  
  dplyr::mutate(Life_stage =  "Spawners")

rear <- readRDS("./output/rear_survey_abundance.rds")[[1]] %>%
  dplyr::mutate(Life_stage = "Juveniles")

head(rear)

arr <- bind_rows(rear, spwn)  %>%
  group_by(Life_stage, pi) %>%
  summarise(CV = mean(sqrt(exp(se^2))-1),
            KS = ks.test(unlist(est),unlist(ind))$p.value) %>%
  arrange(desc(KS), .by_group = TRUE) %>%
  mutate(KS_col = ifelse(row_number(KS)<=10, FALSE, TRUE)) %>% #all the top models are now 2
  arrange(CV, .by_group = TRUE) %>%
  mutate(CV_col = ifelse(row_number(CV)<=10, TRUE, FALSE)) %>% #all the top models are now 2
  mutate(col = ifelse(KS_col & CV_col, "black", "white")) %>% #all the top models are now 2
  arrange(desc(KS), .by_group = TRUE) %>%
  pivot_longer(cols = c("KS", "CV"),
               names_to = "stat")

juv_pi <- unique(arr$pi[arr$col=="black" & arr$Life_stage=="Juveniles"])
spwn_pi <- unique(arr$pi[arr$col=="black" & arr$Life_stage=="Spawners"])
texture_pi <- juv_pi[juv_pi%in%spwn_pi]
arr$col[arr$pi%in%texture_pi] <- "black"

g <- list()
gi <- 1
for(i in unique(arr$Life_stage)){
  for(j in unique(arr$stat)){
    mylab <- "Kolmogrov-Smirnov"
    if(j=="CV"){
      mylab <- "Average coeffcient variation"
      g[[gi]] <- ggplot(data = arr[arr$Life_stage==i & arr$stat==j,], aes(fill=as.factor(col), x = reorder(pi,value), y = value))
      # g[[gi]] <- g[[gi]]
    }else{
      g[[gi]] <- ggplot(data = arr[arr$Life_stage==i & arr$stat==j,], aes(fill=as.factor(col), x = reorder(pi,desc(value)), y = value)) +
        ylim(0,1.1)
    }
    g[[gi]] <- g[[gi]] + 
      geom_col(aes(), width = 0.8, colour = "black") +
      ylab(mylab) +
      xlab('') +
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(legend.position = "none") +
      scale_fill_manual(values = c("black", "darkgrey", "white")) + 
      scale_y_continuous(expand = c(0,0))

    gi <- gi + 1
  }    
}
gg <- ggpubr::ggarrange(plotlist = g, labels = c(LETTERS[1:4]))

ggsave(paste0('./output/combined_model_fit_diagnostics.png'), 
       gg, 
       device = "png", 
       height = 9, width = 8, 
       units = 'in', 
       dpi = 500)
# ggpubr::annotate_figure()

save(arr, file = "combined_model_fit_diagnostics.rData")
