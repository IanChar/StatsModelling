library(ggplot2)
library(ggmap)
lon <- as.numeric(lon.lat[,1])
lat <- as.numeric(lon.lat[,2])
# determine a reasonable center for map, 
# this could fail in some places (near poles, 180th meridian)
# also google appears to shift things slightly
center = paste(min(lat)+(max(lat)-min(lat))/2,
               min(lon)+(max(lon)-min(lon))/2, sep=" ")

# get map image from google
map <- get_map(location = center, zoom = 11, maptype = "terrain", 
               source = "google")

# start a ggplot. it won't plot til we type p
p <- ggmap(map)

# add points last so they are on top
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
colors <- c("blue", "green", "red")
colorClust <- colors[as.numeric(as.character(apt$interest_level)) + 1]
p <- p + geom_point(data=data.frame(lon.lat),aes(x=lon, y=lat),colour=colorClust,size=0.3)

# display plot
p 
