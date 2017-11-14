


################################################################################
################################     Experiments      ##########################
################################################################################

#Trips where people took bikes from milwaukee and wood and returned to the same station
head(milwood)
wicker_map<-get_map(location='wicker park', zoom=15, maptype='roadmap') 
ggmap(wicker_map)+
  geom_point(aes(x=milwood_loop$from_long, 
                 y=milwood_loop$from_lat,
                 size=(milwood_loop$duration)),
             data=milwood_loop, alpha=.5)





########################################################################
#     Trace the path of one bike
chicago<-get_map(location='chicago', zoom=12, maptype='roadmap') 
bike734 <- subset(final_df_with_trip_frequencies,final_df_with_trip_frequencies$bike==734)
bike734

d <- data.frame(Time=bike734$start,fromlat=bike734$to_lat,fromlon=bike734$to_long,tolat=bike734$to_lat,tolon=bike734$to_lat)
d
require(data.table)
DT <- data.table(d)
head(DT)
#  Sort on 'Time' to order the observations based on the time the bike was taken out
setkey(DT,Time )
head(DT)

p <- ggmap(chicago)
p <- p + geom_point(data=d,aes(x=fromlon,y=fromlat),color="red",size=2)
p
p + geom_path(data=d,aes(x=tolon,y=tolat),color="blue",size=1) +
  ggtitle("Station connections map") +
  xlab("Longitude") +
  ylab("Latitude")

########################################################################








########################################################################
#Compute distances between coordinates of Divvy stations
require(geosphere)
library(geosphere)
x <- data.frame(milwood$from_lat,milwood$from_long)
y <- data.frame(milwood$to_lat,milwood$to_long)
x
y
distances <- distm(x,y,fun=distHaversine)

#Using the gcintermediate package to create lines between coordinate points
inter2 <- gcIntermediate(c(lon_ca, lat_ca), c(lon_tx, lat_tx), n=50, addStartEnd=TRUE)  
lines(inter2, col="red") 
########################################################################





#####################################################################################

#Make a network graph of the trips from the 10 busiest stations

#####################################################################################
install.packages("igraph")
library(igraph)
library(ggmap)
library(ggplot2)
library(maps)
library(geosphere)


#Circle graph with data from milwood
g3 <- graph.data.frame(from_to,directed=FALSE)
g3


#Plot all stations that riders go to from Milwaukee and Wood
plot(g3, layout=layout.circle, vertex.size=8,
     vertex.label=NA,
     main="All destinations from Milwaukee and Wood")


# Copy from_to and drop 49 rows out of 50
m4 <- from_to[1:nrow(from_to) %% 50 == 1, ]
g4 <- graph.data.frame(m4, directed=FALSE)

#Circle with labels
plot(g4, layout=layout.circle, vertex.size=4,
     vertex.shape="square",
     vertex.label=V(g4)$From,main="Divvy Paths",
     vertex.label.cex=0.8,
     vertex.label.color="black")

# View the edges
E(g4)
# Set some of the labels to "M"
#E(g)[c(2,11,19)]$label <- "M"
# Set color of all to grey, and then color a few red
E(g4)$color <- "skyblue"
E(g4)[c(40,41,42,43)]$color <- "red"      #Make these select paths stand out by color
#Plot again with the newly colored edges
plot(g4, layout=layout.circle, vertex.size=4,
     vertex.shape="square",
     vertex.label=V(g4)$From,main="Divvy Paths",
     vertex.label.cex=0.8,
     vertex.label.color="black")
#################################################################################
#################################################################################




