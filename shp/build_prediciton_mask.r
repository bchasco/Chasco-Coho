
#Before we can plot the predictions, we have to do a couple of this
#1) Create the grid,
#1a) Create the grid
x_seq <- seq(min(df$UTM_E_km),max(df$UTM_E_km),by = 5) #UTM N
y_seq <- seq(min(df$UTM_N_km),max(df$UTM_N_km),by = 5) #UTM N
vizloc_xy = expand.grid( x=x_seq, y=y_seq) #Spatial field
vizloc_xy$p <- 1
#1b) transform gridded data into coordinates and raster
coordinates(vizloc_xy) <- ~x+y
# coerce to SpatialPixelsDataFrame
gridded(vizloc_xy) <- TRUE
# coerce to raster
rasterDF <- raster(vizloc_xy)

#2) Remove all grid cells from rasterDF that are outside of OC ESU
#2a) Get Oregon spatial ESU
dir <- "C:/NOAA/PROJECTS/ODFW-Coho/"
setwd(paste0(dir,"shp/"))
shape = rgdal::readOGR(".","OregonCoast_coho_ESU-utm83") #will load the shapefile to your dataset.
OR_ESU <- as.data.frame(shape@polygons[[1]]@Polygons[[1]]@coords)
names(OR_ESU) <- c("X","Y")
setwd(dir) #restore root directory
coastXY <- data.frame(X=OR_ESU$X,Y=OR_ESU$Y)/1000 #coastline polygon


#2b)#Get Oregon boundary
LatLong2UTM <- function(x,y,ID,zone){ #I'm sure this function is redudant with sdmTMB
  xy <- data.frame(ID=ID, X = x, Y=y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=", zone, "ellps=WG84", sep='')))
  return(as.data.frame(res))
}

or <- map_data("state","oregon")
or_xy <- LatLong2UTM(or$long,or$lat,or$order,10)
# Sr1 = Polygon(cbind(xy$X,xy$Y))
Sr1 = Polygon(cbind(OR_ESU$X/1000,OR_ESU$Y/1000))
SpP = SpatialPolygons(list(Polygons(list(Sr1), "s1")), 1:1) #1:1 is the trick, can't say 1


#2c) Create the OC ESU mask
myMask <- mask(rasterDF,SpP)
tmp <- as.data.frame(cbind(coordinates(myMask),p = myMask@data@values))
predmask <- na.omit(tmp)

#2d) Because you use PopGrp as a factor, we need to assign population group to each coordinate value
dir <- "C:/NOAA/PROJECTS/ODFW-Coho/"
setwd(paste0(dir,"shp/"))
pops = rgdal::readOGR(".","Coho_oc_pop-utm83") #will load the shapefile to your dataset.
GIS_poly_names <- pops@data$POPULATION
n_id <- 1:length(GIS_poly_names)
ODFW_names <- unique(df$PopGrp)

pop_poly <- data.frame(UTM_E_km = NA, 
                       UTM_N_km = NA, 
                       PopGrp = NA)

for(i in 1:length(ODFW_names)){
  # i <- 5
  #Map the population names in GIS database to ODFW names
  pop_IDs <- na.omit(n_id[str_detect(GIS_poly_names,as.character(ODFW_names[i]))])
  if(length(pop_IDs)>0){
    for(j in pop_IDs){
      # j <- pop_IDs[1]
      pop_GIS <- pops@polygons[[j]]@Polygons
      Sr_pop = Polygon(pop_GIS[[1]]@coords/1000)
      SpPop = SpatialPolygons(list(Polygons(list(Sr_pop), "s1")), 1:1) #1:1 is the trick, can't say 1
      
      pop_mask <- mask(rasterDF,SpPop)
      # tmp <- na.omit(as.data.frame(cbind(coordinates(pop_mask),p = pop_mask@data@values,pop=ODFW_names[i], popID = i)))
      tmp_pop_poly <- as.data.frame(cbind(coordinates(pop_mask),
                                          PopGrp=as.character(ODFW_names[i])))
      
      names(tmp_pop_poly)[1:2] <- c("UTM_E_km","UTM_N_km")
      pop_poly <- rbind(tmp_pop_poly[!is.na(pop_mask@data@values),],
                        pop_poly)
    }
  }
}

#2e) Now you need the covariates for your model
p.df <- pop_poly #start with population groups
p.df[,names(df)[!names(df)%in%names(pop_poly)]] <- 0 #add all of the column heading
p.df <- do.call("rbind", replicate(length(unique(df$JuvYr)), p.df, simplify = FALSE))
p.df$JuvYr <- rep(unique(df$JuvYr),each=nrow(pop_poly))
p.df$PopGrp <- as.factor(p.df$PopGrp)
p.df$UTM_E_km <- as.numeric(p.df$UTM_E_km)
p.df$UTM_N_km <- as.numeric(p.df$UTM_N_km)

p <- predict(fit, newdata=p.df)#[p.df$JuvYr%in%unique(df$JuvYr),])
