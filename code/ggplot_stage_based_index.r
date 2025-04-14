library(sp)
library(glmmTMB)
library(sdmTMB)
library(raster)
library(ggplot2)
library(viridis)
library(viridisLite)
library(cowplot)
library(tidyr)
library(dplyr)
#Predict the spatiotemporal effects for 2019

#Before we can plot the predictions, we have to do a couple of this
# #1) Create the grid,
# #1a) Create the grid
# x_seq <- seq(min(df$UTM_E_km),max(df$UTM_E_km)*1.1,by = 2) #UTM N
# y_seq <- seq(min(df$UTM_N_km),max(df$UTM_N_km)*1.1,by = 5) #UTM N
# vizloc_xy = expand.grid( x=x_seq, y=y_seq) #Spatial field
# vizloc_xy$p <- 1
# #1b) transform gridded data into coordinates and raster
# coordinates(vizloc_xy) <- ~x+y
# # coerce to SpatialPixelsDataFrame
# gridded(vizloc_xy) <- TRUE
# # coerce to raster
# rasterDF <- raster(vizloc_xy)

#2) Remove all grid cells from rasterDF that are outside of OC ESU
#2a) Get Oregon spatial ESU
dir <- "C:/NOAA/PROJECTS/ODFW-Coho/"
setwd(paste0(dir,"shp/"))
shape = rgdal::readOGR(".","OregonCoast_coho_ESU-utm83") #will load the shapefile to your dataset.
OR_ESU <- as.data.frame(shape@polygons[[1]]@Polygons[[1]]@coords)
names(OR_ESU) <- c("X","Y")
setwd(dir) #restore root directory
coastXY <- data.frame(X=OR_ESU$X,Y=OR_ESU$Y)/1000 #coastline polygon
Sr1 = Polygon(cbind(OR_ESU$X/1000,OR_ESU$Y/1000))
SpP = SpatialPolygons(list(Polygons(list(Sr1), "s1")), 1:1) #1:1 is the trick, can't say 1



#2b)#Get Oregon boundary
LatLong2UTM <- function(x,y,ID,zone){ #I'm sure this function is redudant with sdmTMB
  xy <- data.frame(ID=ID, X = x, Y=y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=", zone, "ellps=WG84", sep='')))
  return(as.data.frame(res))
}

or <- map_data("state","oregon")
or_utm <- add_utm_columns(data.frame("longitude"=or$long,"latitude"=or$lat))
# or_xy <- LatLong2UTM(or$long,or$lat,or$order,10)
# Sr1 = Polygon(cbind(xy$X,xy$Y))



#Flag that needs to be incorporated
no_covars <- TRUE

#Loop over the two stage in the life history
for(myStage in c('rear')){
  myStage <- 'rear'
  stage <- myStage
  source("./code/function_wrangle_data.r")
  df <- function_wrangle_data(stage = myStage)
  if(no_covars){
    load(paste0("./output/output_st_",stage,".rData"))
  }else{
    load(paste0("./output/output_",stage,".rData"))
  }
  
  #Subset by unique locations, non-duplicated df
  nd.df <- df[!duplicated(df[,c('UTM_E_km','UTM_N_km')]) & df$PopGrp=="Siletz",c('UTM_E_km','UTM_N_km')]
  nd <- nrow(nd.df)
  print(nd)
  nd.df$UTM_E <- nd.df$UTM_E_km * 1000
  nd.df$UTM_N <- nd.df$UTM_N_km * 1000
  nd.df[,names(df)[!names(df)%in%names(nd.df)]] <- 0 #add all of the column heading
  nd.df <- do.call("rbind", replicate(length(unique(df$yr)), nd.df, simplify = FALSE))
  nd.df$STRM_ORDER <- 1
  nd.df$fSTRM_ORDER <- as.factor(1)
  nd.df$yr <- rep(unique(df$yr),each=nd)
  nd.df$fYr <- as.factor(nd.df$yr)
  
  # fit <- output$exploratory$gam$best_fit
  # pred <- exp(predict(fit, nd.df))
  # p <- cbind(nd.df,pred)
  # p$mod <- "GAM \n (mgcv)"
  # 
  # fit <- output$exploratory$rf$best_fit
  # pred <- predict(fit, nd.df)
  # tmp <- cbind(nd.df,pred)
  # tmp$mod <- "Random forest \n (randomForest)"
  # p <- rbind(nd.df,tmp)
  # 
  
  fit <- output$exploratory$sdm$best_fit
  pred<-exp(predict(fit, nd.df)$est)
  # tmp <- cbind(nd.df,pred)
  # tmp$mod <- "GLMM \n (sdmTMB)"
  # assign(myStage,na.omit(rbind(p,tmp)))
  
  assign(myStage,na.omit(p))
}

rear$pop <- ""
setwd(paste0(dir,"shp/"))
pops = rgdal::readOGR(".","Coho_oc_pop-utm83") #will load the shapefile to your dataset.
pop_obs <- nd.df[,c('UTM_E_km','UTM_N_km')]
for(pi in 1:10){#length(pops$POPULATION)){
  if(!is.na(pi) & pi!=''){
    pop_i <- as.data.frame(pops@polygons[[pi]]@Polygons[[1]]@coords)
    names(pop_i) <- c("X","Y")
    setwd(dir) #restore root directory
    Sr1 = Polygon(cbind(pop_i$X/1000,pop_i$Y/1000))
    pin <- point.in.polygon(rear[,'UTM_E_km'],
                            rear[,'UTM_N_km'],
                            Sr1@coords[,1],
                            Sr1@coords[,2])
    rear$pop[pin==1] <- pops$POPULATION[pi]
    print(sum(pin))
  }
}

ag <- aggregate(list(y = rear$pred), by=list(mod=rear$mod, pop=rear$pop,yr=rear$yr),sum)
ag <- ag[ag$pop!='',]
g <- ggplot(data=ag, aes(x = yr, y = y, color=pop)) + geom_line() + facet_wrap(~mod, nrow=2)
print(g)
