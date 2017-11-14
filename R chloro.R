install.packages(c("plyr", "ggplot2","rgeos", "maptools"))
food <- FoodSrvcByCounty
food
county <- food$V1
county
state <- food$V2
state
fs97 <- food$V3
fs97
fs02 <- food$V4
fs02
fs07 <- food$V5
fs07


library(ggmap)
all_states <- map_data("state")
all_states

#plot all states with ggplot
p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
p

#To see one state at a time:
states <- subset(all_states, region %in% c( "illinois" ) )
i <- ggplot()
i <- i + geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
i



schools <- schools
schools
long <- schools$long
lat <- schools$lat
enrollment <- schools$enrollment

p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white" )
p <- p + geom_point( data=schools, aes(x=long, y=lat, size = enrollment), color="coral1") + scale_size(name="Total enrollment")
p <- p + geom_text( data=schools, hjust=0.5, vjust=-0.5, aes(x=long, y=lat, label=label), colour="gold2", size=4 )
p





#Plotting airport locations
airports <- Airports
airport_number <- airports$V1
name <- airports$V2
city <- airports$V3
country <- airports$V4
lat <- airports$V7
long <- airports$V8
altitude <- airports$V8

p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white" )
p <- p + geom_point( data=airports, aes(x=long, y=lat, size = altitude), color="coral1") + scale_size(name="Altitude")
p <- p + geom_text( data=airports, hjust=0.5, vjust=-0.5, aes(x=long, y=lat, label=label), colour="gold2", size=4 )
p




