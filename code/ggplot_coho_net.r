library(rgdal)
tmpDir <- "C:/noaa/large_data/coho_stream_net/CohoNetmapClip.shp"
shp <- readOGR(dsn = tmpDir, stringsAsFactors = F)
map <- ggplot() + geom_line(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
