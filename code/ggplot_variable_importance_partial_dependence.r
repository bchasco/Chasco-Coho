df_juv <- function_wrangle_data(stage='rear')
df_adult <- function_wrangle_data(stage='Spwn')

#There are some weird outliers
df_adult <- df_adult[df_adult$SprPpt>(-4),]
df_adult <- df_adult[df_adult$UTM_N_km>(4000),]
df_juv <- df_juv[df_juv$StrmPow<10,]
df_juv <- df_juv[df_juv$StrmSlope<=4.5,]
# W3Dppt +
# SprPpt +
j_frm = formula(gsub("[\r\n\t]", "","dens ~
                   STRM_ORDER +
                   StrmSlope +
                   WidthM +
                   OUT_DIST +
                   SolMean +
                   MWMT_Index +
                   StrmPow +
                   IP_COHO +
                   yr +
                   UTM_E_km +
                   UTM_N_km")) #Rf interactions are implicit

# STRM_ORDER +
# WidthM +
s_frm = formula(gsub("[\r\n\t]", "","dens ~
                   StrmSlope +
                   OUT_DIST +
                   SolMean +
                   MWMT_Index +
                   W3Dppt +
                   SprPpt +
                   StrmPow +
                   IP_COHO +
                   yr +
                   UTM_E_km +
                   UTM_N_km")) #Rf interactions are implicit
  
juv <- randomForest(j_frm, data = df_juv,
                  mtry = 3,
                  ntree  = 500, 
                  importance = TRUE,
                  scale = FALSE)
spwn <- randomForest(s_frm, data = df_adult,
                    mtry = 3,
                    ntree  = 500, 
                    importance = TRUE,
                    scale = FALSE)

imp_juv <- as.data.frame(importance(juv))
imp_spwn <- as.data.frame(importance(spwn))
names(imp_juv)[1] <- 'DecreaseInAccuracy' #column name is unworkable
names(imp_spwn)[1] <- 'DecreaseInAccuracy' #column name is unworkable
# Tidy up and sort the data frame
tmp_juv <- imp_juv %>% 
  mutate(names = rownames(imp_juv)) %>%
  arrange(desc(DecreaseInAccuracy)) %>%
  mutate(Stage="Juveniles")
tmp_spwn <- imp_spwn %>% 
  mutate(names = rownames(imp_spwn)) %>%
  arrange(desc(DecreaseInAccuracy)) %>%
  mutate(Stage="Spawners")
imp <- rbind(tmp_juv,tmp_spwn)
varImpPlot(spwn,
           main="")


sp <- tmp_spwn %>% 
  # top_n(10, DecreaseInAccuracy) %>% 
  ggplot(aes(x = reorder(names, DecreaseInAccuracy),y = DecreaseInAccuracy)) +
  # facet_wrap(~Stage, scales = "free") +
  geom_col() +
  coord_flip() +
  labs(title = "",
       subtitle = "",
       x= "",
       y= "Mean Decrease in Accuracy",
       caption = "") +
  theme(plot.caption = element_text(face = "italic"))
jv <- tmp_juv %>% 
  # top_n(10, DecreaseInAccuracy) %>% 
  ggplot(aes(x = reorder(names, DecreaseInAccuracy),y = DecreaseInAccuracy)) +
  # facet_wrap(~Stage, scales = "free") +
  geom_col() +
  coord_flip() +
  labs(title = "",
       subtitle = "",
       x= "",
       y= "Mean Decrease in Accuracy",
       caption = "") +
  theme(plot.caption = element_text(face = "italic"))

print(sp)
print(jv)

library(cowplot)


l <- nrow(tmp_juv)
pd <- expand.grid(sp=c("Juveniles","Spawners"),x = 1:51, ef = c("High accur. variables","Low accr. variables"), var = c(1,2,3,4))
pd[,c('y')] <- NA
icnt <- 1
ef <- rep(c("High accur. variables","Low accr. variables"), each = 2)

for (i in c(1:2,(l-1):l)) { #take the top two and the bottom two 
    x <- partialPlot(spwn,
                df_adult,
                tmp_spwn$names[i],
                xlab=tmp_spwn$names[i],
                main="",
                plot = FALSE)
    
    pd[pd$sp=="Spawners" & pd$var==icnt,c('ef')][1:length(x$x)] <- ef[icnt]
    pd[pd$sp=="Spawners" & pd$var==icnt,c('x','y')][1:length(x$x),] <- cbind(x$x,x$y)
    pd[pd$sp=="Spawners" & pd$var==icnt,c('var')][1:length(x$x)] <- tmp_spwn$names[i]
    icnt <- icnt + 1
}
icnt <- 1
for (i in c(1:2,(l-1),l)) { #take the bottom 
  x <- partialPlot(juv,
                   df_juv,
                   tmp_juv$names[i],
                   xlab=tmp_juv$names[i],
                   main="",
                   plot = FALSE)
  pd[pd$sp=="Juveniles" & pd$var==icnt,c('ef')][1:length(x$x)] <- ef[icnt]
  pd[pd$sp=="Juveniles" & pd$var==icnt,c('x','y')][1:length(x$x),] <- cbind(x$x,x$y)
  pd[pd$sp=="Juveniles" & pd$var==icnt,c('var')][1:length(x$x)] <- tmp_juv$names[i]
  icnt <- icnt + 1
}
pd <- na.omit(pd)

g_juv <- ggplot(pd[pd$sp=="Juveniles",],aes(x = x, y = y)) +
  geom_line() + 
  ylab("Juvenile density") +
  facet_wrap(ef~var, scales = "free_x")
g_sp <- ggplot(pd[pd$sp=="Spawners",],aes(x = x, y = y)) +
  geom_line() + 
  ylab("Adult density") +
  facet_wrap(ef~var, scales = 'free_x')
# print(g)

png("./output/ggplot_variable_importance_partial_dependence.png")
plot_grid(jv, sp, g_juv, g_sp, labels = c('A', 'B', 'C','D'), label_size = 12)
dev.off()
