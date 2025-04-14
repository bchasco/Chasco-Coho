#Load the shape file.
library(rgdal)
library(sf)
library("lemon")
library(ggplot2)

life_stage <- "rear"

#Get the stream lines
# tmpDir <- "C:/noaa/large_data/coho_stream_net/CohoNetmapClip.shp"
# stream_data <- readOGR(dsn = tmpDir, stringsAsFactors = F)
# 
# #Get the boundaries for the populations
# tmpDir <- "C:/noaa/projects/odfw-coho/shp/Coho_oc_pop-utm83.shp"
# bndy_data <- readOGR(dsn = tmpDir, stringsAsFactors = F)

#Get the observed data
load(paste0("./output/output_",life_stage,".rData"))
obs_data <- output$exploratory$sdm$best_fit$data
fit <- output$exploratory$sdm$best_fit

#The GIS and data population names are different
data_pop_names <- unique(output$exploratory$sdm$best_fit$data$PopGrp)
data_pop_names[data_pop_names%in%c("Alsea", "Yaquina", "Siuslaw")] <- paste(data_pop_names[data_pop_names%in%c("Alsea", "Yaquina", "Siuslaw")], "River")
data_pop_names[data_pop_names%in%c("Tenmile")] <- paste(data_pop_names[data_pop_names%in%c("Tenmile")], "Creek")
data_pop_names <- data_pop_names[!(data_pop_names%in%c("Tenmile Creek", "Tahkenitch", "Siltcoos"))]


gis_pop_names <- bndy_data$POPULATION

myFunc <- function(pi){
  print(pi)
  # if(length(grep(pi,gis_pop_names)>0)){
  #   p_i <- grep(pi,gis_pop_names)

    # #pop i boundary
    # bndy_i <- as.data.frame(bndy_data@polygons[[p_i]]@Polygons[[1]]@coords)
    # names(bndy_i) <- c("X","Y")
    # 
    # #Get the correct boundary
    # Sr1 <- Polygon(cbind(bndy_i$X/1000,bndy_i$Y/1000))
    # 
    # #Streams within boundary
    # pin <- point.in.polygon(stream_data@data[,'X_MID']/1000,
    #                         stream_data@data[,'Y_MID']/1000,
    #                         Sr1@coords[,1],
    #                         Sr1@coords[,2])
    # #Data within boundary  
    # pin2 <- point.in.polygon(obs_data$UTM_E_km,
    #                          obs_data$UTM_N_km,
    #                          Sr1@coords[,1],
    #                          Sr1@coords[,2])
    pred_data <- obs_data[obs_data$PopGrp == pi, ]
    print(table(pred_data$fYr))
    y_obs <- table(pred_data$fYr)==0
    print(y_obs)
    if(sum(y_obs)>0){
      missing_yrs <- as.numeric(names(y_obs)[y_obs])
      print(missing_yrs)
      #add some dummy data
      for(ii in missing_yrs){
        pred_data <- rbind(pred_data,obs_data[obs_data$fYr==ii, ][1,])
      }
    }
  
    pred <- predict(fit, newdata = pred_data, return_tmb_object=TRUE)
    ind <- sdmTMB::get_index(pred)
    ind <- ind[ind$yr%in%as.numeric(names(y_obs)[!y_obs]),]
    ind$pi <- pi
    ind$p_i <- 1
    return(ind)
  # }
}


#get rid of the trib we don't want
obs_data <- obs_data[!(obs_data$PopGrp%in%c('Tenmile','Tahkenitch','Siltcoos')),]
dependent <- grep("Dependent",obs_data$PopGrp)
obs_data <- obs_data[-dependent,]

#combine everything into a flat array.
#I'm sure there's a tidyr r way to do this
forecast_names <- unique(obs_data$PopGrp)

out <- list()
for(pi in unique(obs_data$PopGrp)){
  out[[pi]] <- myFunc(pi)
}

#Get the data
#Change the names so there all the same.
source(".\\code\\function_wrangle_data.r")
sp <- function_wrangle_data(life_stage, dir = "")
# sp$PopGrp[sp$PopGrp%in%c("Alsea", "Yaquina", "Siuslaw")] <- paste(sp$PopGrp[sp$PopGrp%in%c("Alsea", "Yaquina", "Siuslaw")], "River")
# sp$PopGrp[sp$PopGrp%in%c("Tenmile")] <- paste(sp$PopGrp[sp$PopGrp%in%c("Tenmile")], "Creek")

#Create the observation data set
obs <- sp %>%
  dplyr::group_by(PopGrp, yr) %>%
  dplyr::summarize(ind = sum(dens)) %>%
  dplyr::rename("pi" = "PopGrp")

out <- do.call(rbind,out)
if (require("ggplot2", quietly = TRUE)) {
  g <- ggplot(out[out$pi%in%unique(out$pi),], aes(yr, est)) + 
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
    geom_point(data = obs[obs$pi%in%unique(out$pi),],
               aes(x = yr, y = ind),
               stroke = 0,
               color= "blue",
               position=position_nudge(x = 0.),
               inherit.aes = FALSE,
               alpha = 0.5,
               size = 2.5) +
    ylab("Estimated abundance for surveyed locations") + 
    xlab("Survey year") +
    facet_wrap(~pi, scales = "free", ncol = 3)  +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))
}

ggsave(paste0('./output/forecast_watersheds_',life_stage,'.png'), 
       g, 
       device = "png", 
       height = 9, width = 8, 
       units = 'in', 
       dpi = 500)

#Survey counts
# cnt <- list()
# for(pi in unique(obs_data$PopGrp)){
#   cnt[[pi]] <- data.frame(table(obs_data$yr[obs_data$PopGrp==pi]))
#   names(cnt[[pi]]) <- c("yr","survey.count")
#   cnt[[pi]]$pi <- pi  
# }
# 
# cnt <- do.call(rbind,cnt)
# cnt$pi[cnt$pi%in%c("Alsea", "Yaquina", "Siuslaw")] <- paste(cnt$pi[cnt$pi%in%c("Alsea", "Yaquina", "Siuslaw")], "River")
# cnt$pi[cnt$pi%in%c("Tenmile")] <- paste(cnt$pi[cnt$pi%in%c("Tenmile")], "Creek")
# 
# if (require("ggplot2", quietly = TRUE)) {
#   ggplot(cnt[cnt$pi%in%unique(out$pi),], aes(x = yr, y = survey.count)) + 
#     geom_bar(stat="identity") +
#     # facet_wrap(~pi, scales = "free", ncol = 3)  +
#     facet_rep_wrap(~pi, scales = "free", ncol = 3)  +
#     theme_bw() +
#     theme(axis.text.x=element_text(angle=90,hjust=1)) 
# }


