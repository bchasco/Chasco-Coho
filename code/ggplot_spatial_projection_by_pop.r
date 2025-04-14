library(sp)
library(glmmTMB)
library(raster)
library(ggplot2)
library(viridis)
library(viridisLite)
library(cowplot)
library(RANN)

#2e) Now you need the covariates for your model
stage <- "Spwn" #rear or Spwn

#Grab the data for stage
df <- function_wrangle_data(stage=stage,
                            dir = root)
load(paste0("output/output_",stage,".rData"))

odfw_est <- read.csv("./data/ESU_Estimates.csv")
odfw_est$pop <- "Aggregate"

odfw_pop <- read.csv("C:/noaa/large_data/Coho_Oregon_Coast_default_Independent_Population_Abundance.csv")
odfw_pop$pop <- odfw_pop$Independent.Population
odfw_pop$pop[odfw_pop$pop=="Tillamook"] <- "Tillamook Bay" 

#Subset by unique locations, non-duplicated df
nd.df <- df[!duplicated(df[,c('UTM_E_km','UTM_N_km','PopGrp')]),c('UTM_E_km','UTM_N_km','PopGrp')]
nd <- nrow(nd.df)
nd.df$UTM_E <- nd.df$UTM_E_km*1000
nd.df$UTM_N <- nd.df$UTM_N_km*1000
nd.df[,names(df)[!names(df)%in%names(nd.df)]] <- 0 #add all of the column heading
nd.df <- do.call("rbind", replicate(length(unique(df$yr)), nd.df, simplify = FALSE))
nd.df$yr <- rep(unique(df$yr),each=nd)
nd.df$fYr <- as.factor(nd.df$yr)

#Use kmeans to map the stream in the original data frame to 
#the prediction dataframe
# kmean <- nn2(df[,c('UTM_E','UTM_N')],nd.df[,c('UTM_E','UTM_N')], k=1)

coVars <- c('WidthM','W3Dppt',
            'MWMT_Index','StrmPow',
            'SprPpt','IP_COHO',
            'SolMean','StrmSlope',
            'OUT_DIST','STRM_ORDER' 
)



for(iii in coVars){
  for(y in unique(nd.df$yr)){
    kmean <- nn2(df[df$yr==y,c('UTM_E','UTM_N')],nd.df[nd.df$yr==y,c('UTM_E','UTM_N')], k=1)
    nd.df[nd.df$yr==y,iii] <- df[kmean$nn.idx,iii]
  }
}

nd.df$fSTRM_ORDER <- as.factor(nd.df$fSTRM_ORDER)
levels(nd.df$fSTRM_ORDER) <- levels(df$fSTRM_ORDER)


fit <- output$exploratory$gam$best_fit
#Predict the year in questions
pred1 <- exp(predict(fit, nd.df))
p <- cbind(nd.df,pred1)
p$mod <- "GAM \n (mgcv)"

ff <- output$exploratory$rf$best_mod
mtry <- output$exploratory$rf$best_mtry
ntree <- output$exploratory$rf$best_ntree
fit <- randomForest(ff,
                    mtry = mtry,
                    ntree = ntree,
                    data = df)
pred1 <- predict(fit, nd.df)
tmp <- cbind(nd.df,pred1)
tmp$mod <- "Random forest \n (randomForest)"
p <- rbind(p,tmp)
# 

fit <- output$exploratory$sdm$best_fit
pred1 <-exp(predict(fit, nd.df)$est)
# tmp <- cbind(nd.df,pred$est)
tmp <- cbind(nd.df,pred1)
names(tmp)[ncol(tmp)] <- "pred1"
tmp$mod <- "GLMM \n (sdmTMB)"
p <- rbind(p,tmp)

ag <- aggregate(list(est = p$pred1), by=list(yr = p$yr, mod = p$mod, pop = p$PopGrp), sum)
if(stage=="Spwn"){
  odfw <- data.frame(yr=odfw_est$Brood.Year,
                     mod = "Aggregate",
                     pop = "Agggregate",
                     'Estimate' = odfw_est$Spawners)
}
if(stage=="rear"){
  odfw <- data.frame(yr = odfw_est$Parr.Year,
                     mod = "Aggregate",
                     pop = "Aggregate",
                     'Estimate' = odfw_est$TotalParr)
}

names(ag)[ncol(ag)] <- "Estimate"
ag <- rbind(ag,
            odfw[odfw$yr%in%ag$yr,])
ag$stage <- stage

sub_pop <- ag[ag$pop%in%odfw_pop$pop,]
sub_pop <- sub_pop[sub_pop$pop!="Sixes",] 
sub_pop <- sub_pop %>%
  group_by(yr,mod,stage) %>%
  summarise(Estimate = sum(Estimate)) %>% 
  mutate(pop = 'Aggregate') %>%
  relocate(pop, .before=Estimate) %>%
  bind_rows(sub_pop)

png(paste0("./output/ggplot_spatial_projection_by_pop",stage,".png"),width=800,height=500)

odfw_pop_ag <- odfw_pop[odfw_pop$pop!='Sixes',] %>%
  group_by(spawning_year) %>%
  summarise(Estimate = sum(CohoNaturalOriginSpawners)) %>%
  mutate(pop = "Aggregate",stage = "Spwn")

g <- ggplot(sub_pop[sub_pop$pop=="Aggregate",], aes(x = yr,
                     y = Estimate,
                     colour = mod)) +
  geom_line(size = 0.9) +
  facet_wrap(~pop, scales = "free_y")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(color = "Model estimate")+
  ylab("Estimate") +
  xlab("Brood year")
if(stage=="Spwn")
#   g <- g +
#   geom_point(data = odfw_pop[odfw_pop$pop!='Sixes',], aes(x=spawning_year, y = CohoNaturalOriginSpawners),
#              inherit.aes = FALSE)+
  geom_point(data = odfw_pop_ag, aes(x=spawning_year, y = Estimate),
             inherit.aes = FALSE)
if(stage=="rear")
  g <- g +
  geom_point(data = odfw[,], aes(x=yr, y = Estimate),
             inherit.aes = FALSE)
# 
print(g)

dev.off()
print(g)
