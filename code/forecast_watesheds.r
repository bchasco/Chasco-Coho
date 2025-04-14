#Load the shape file.
library(rgdal)
library(sf)

# # #Get the stream lines
# tmpDir <- "C:/noaa/large_data/coho_stream_net/CohoNetmapClip.shp"
# stream_data <- readOGR(dsn = tmpDir, stringsAsFactors = F)
# 
# #Getthe boundaries for the populations
# tmpDir <- "C:/noaa/projects/odfw-coho/shp/Coho_oc_pop-utm83.shp"
# bndy_data <- readOGR(dsn = tmpDir, stringsAsFactors = F)

#Get the observed data
load("./output/output_spwn.rData")
obs_data <- output$exploratory$sdm$best_fit$data
fit <- output$exploratory$sdm$best_fit

#The GIS and data populations are different
data_pop_names <- unique(output$exploratory$sdm$best_fit$data$PopGrp)
data_pop_names[data_pop_names%in%c("Alsea", "Yaquina", "Siuslaw")] <- paste(data_pop_names[data_pop_names%in%c("Alsea", "Yaquina", "Siuslaw")], "River")
data_pop_names[data_pop_names%in%c("Tenmile")] <- paste(data_pop_names[data_pop_names%in%c("Tenmile")], "Creek")
data_pop_names <- data_pop_names[!(data_pop_names%in%c("Tenmile Creek", "Tahkenitch", "Siltcoos"))]

gis_pop_names <- bndy_data$POPULATION

myFunc <- function(pi){
  print(pi)
  if(length(grep(pi,gis_pop_names)>0)){
    p_i <- grep(pi,gis_pop_names)

    
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
  
    print(table(pred_data$fYr))
    print(as.numeric(names(y_obs)[!y_obs]))
    pred <- predict(fit, newdata = pred_data, return_tmb_object=TRUE)
    ind <- sdmTMB::get_index(pred)
    ind <- ind[ind$yr%in%as.numeric(names(y_obs)[!y_obs]),]
    ind$pi <- pi
    ind$p_i <- p_i
    return(ind)
  }
}


out <- list()
for(pi in data_pop_names[1:3]){
  out[[pi]] <- myFunc(pi)
  print(pi)
}

source(".\\code\\function_wrangle_data.r")
sp <- function_wrangle_data("Spwn", dir = "C:/NOAA/LARGE_data/Spwn.csv")
sp$PopGrp[sp$PopGrp%in%c("Alsea", "Yaquina", "Siuslaw")] <- paste(sp$PopGrp[sp$PopGrp%in%c("Alsea", "Yaquina", "Siuslaw")], "River")
sp$PopGrp[sp$PopGrp%in%c("Tenmile")] <- paste(sp$PopGrp[sp$PopGrp%in%c("Tenmile")], "Creek")

obs <- sp %>%
  dplyr::group_by(PopGrp, yr) %>%
  dplyr::summarize(ind = sum(dens)) %>%
  dplyr::rename("pi" = "PopGrp")

xx <- do.call(rbind,out)
xx <- xx[xx$pi!="Tenmile Creek",]


yy <- xx %>% 
  group_by(pi) %>% 
  summarise( mean_cv = mean(sqrt(exp(se^2))-1)) %>% 
  arrange(mean_cv) 

comb <- merge(xx,obs,by.x = c('pi','yr'),by.y=c('pi','yr'))

spwn_survey_abundance <- list(comb = comb)
saveRDS(spwn_survey_abundance, "spwn_survey_abundance.rds")
