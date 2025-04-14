
if (require("ggplot2", quietly = TRUE)) {
  ggplot(comb, aes(yr, est)) + 
    geom_errorbar(aes(ymin = lwr, ymax = upr), 
                  width = 0.4,
                  alpha = 1) +
    geom_point(aes(x = yr, y = est), color= "white", 
               shape=21, 
               color = "white",
               stroke = 0,
               fill="white",
               size = 1.5) +
    geom_point(aes(x = yr, y = est), color= "black", 
               shape=21, 
               color = "black",
               stroke = 2,
               fill="grey",
               size = 1., 
               alpha = 0.3) +
    geom_point(aes(x = yr, y = ind),
               stroke = 0,
               color= "blue",
               position=position_nudge(x = 0.),
               inherit.aes = FALSE,
               alpha = 0.5,
               size = 2.5) +
    facet_wrap(~ factor(pi,levels = yy$pi), scales = "free", ncol = 5)  +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))+ 
    ylab("Survey abundance") +
    xlab("Brood year")
}
