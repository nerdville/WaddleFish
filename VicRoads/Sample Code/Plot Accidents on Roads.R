require(data.table)
require(ggplot2)
require(ggmap)

setwd('C:/DAWG/VicRoads/Data')
roads <- fread('../Final_Data/roads.csv')
acc <- fread('../Final_Data/accidents_train.csv')

setkey(roads,ROAD_ID)
setkey(acc, ROAD_ID)

#Create global variables to fill with plot function
plot.roads <- NULL
plot.acc <- NULL
plot.map <- NULL
PLOT <- function(road.name='.*',LON = range(roads$Longitude), LAT = range(roads$Latitude), road.id=NA, acc.id=NA){

  if (all(!is.na(road.id))){
    plot.roads <<- roads[ROAD_ID %in% road.id]
  } else {
    plot.roads <<- roads[like(ROAD_NAME, road.name) &
                       between(Longitude,range(LON,na.rm=TRUE)[1],range(LON,na.rm=TRUE)[2]) &
                       between(Latitude, range(LAT,na.rm=TRUE)[1],range(LAT,na.rm=TRUE)[2]) &
                       !is.na(Longitude) &
                       !is.na(Latitude)]
  }
  if (nrow(plot.roads) == 0) stop('No Road Data in the Plot')
  
  plot.acc <<- acc[0] #initialise plot of accidents
  if (all(!is.na(acc.id))) {
    plot.acc <<- acc[ACCIDENT_NO %in% acc.id]
  } else {
    plot.acc <<- acc[ROAD_ID %in% plot.roads$ROAD_ID]
  }


  range.lon <- range(plot.roads$Longitude, na.rm=TRUE)
  range.lat <- range(plot.roads$Latitude, na.rm=TRUE)
  range.ctr <- c(mean(range.lon),mean(range.lat))
  range.siz <- max(c(diff(range.lon), diff(range.lat)))
  range.bb <- rep(range.ctr,2) + range.siz * c(-1.25,-1.25,1.1,1.1)
  
  google.zoom <- matrix(nrow=16,ncol=2)
  google.zoom[5,] <- c(28,25)
  google.zoom[6,] <- c(14,11)
  google.zoom[7,] <- c(7,5)
  google.zoom[8,] <- c(3.5, 2.8)
  google.zoom[9,] <- c(1.75, 1.4)
  google.zoom[10,] <- c(0.9, 0.7)
  google.zoom[11,] <- c(0.45, 0.35)
  google.zoom[12,] <- c(0.225, 0.175)
  google.zoom[13,] <- c(0.11, 0.09)
  google.zoom[14,] <- c(0.06, 0.05)
  google.zoom[15,] <- c(0.03, 0.03)
  google.zoom[16,] <- c(0.015, 0.015)
  range.z <- 16
  while (any(google.zoom[range.z,] < range.siz & range.z>5)) range.z <- range.z-1
  
  plot.map <<- get_googlemap(center=range.ctr, zoom=range.z)
  plot.roads[,ROAD_TYPE:= ifelse(CARRIAGEWAY==3,'Two Way', 'Divided')]
  
  gg <- ggmap(plot.map) +
    geom_point(data=plot.roads, aes(group=ROAD_NAME, x=Longitude, y=Latitude,col=ROAD_TYPE), size=2.0)
  
  if (nrow(plot.acc)>0){
    gg <- gg +
        geom_point(data=plot.acc,aes(x=plot.acc$Longitude, y=plot.acc$Latitude), shape='+', size=7, col='red')
  }
  gg
    
}

#Examples:
PLOT('HUME')
PLOT(LON=c(146,147), LAT=c(-36,-37))
PLOT(road.id = 2720:2740, acc.id = 'T20100032597')

