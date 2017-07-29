

#Using great circles from flowing data tutorial (https://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/)
library(maps) 
library(geosphere) 
xlim <- c(-171.738281, -56.601563) 
ylim <- c(12.039321, 71.856229) 
map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim) 

#This draws a great circle arc from California to Maine.
lat_ca <- 39.164141 
lon_ca <- -121.640625 
lat_me <- 45.213004 
lon_me <- -68.906250 
inter <- gcIntermediate(c(lon_ca, lat_ca), c(lon_me, lat_me), n=50, addStartEnd=TRUE) 
lines(inter) 

lat_tx <- 29.954935 
lon_tx <- -98.701172 
inter2 <- gcIntermediate(c(lon_ca, lat_ca), c(lon_tx, lat_tx), n=50, addStartEnd=TRUE)  
lines(inter2, col="red") 

airports <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header=TRUE) 
flights <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/flights.csv", header=TRUE, as.is=TRUE)

map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim) 
fsub <- flights[flights$airline == "AA",] 
for (j in 1:length(fsub$airline)) { 
  air1 <- airports[airports$iata == fsub[j,]$airport1,] 
  air2 <- airports[airports$iata == fsub[j,]$airport2,] 
  inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE) 
  lines(inter, col="black", lwd=0.8) 
} 




#If we have the time, we can insert an image to use in the network graph
# install.packages("png")
library(png)

img.1 <- readPNG("./images/news.png")
img.2 <- readPNG("./images/user.png")

V(net2)$raster <- list(img.1, img.2)[V(net2)$type+1]

plot(net2, vertex.shape="raster", vertex.label=NA,
     vertex.size=16, vertex.size2=16, edge.width=2)

