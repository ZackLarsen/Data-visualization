#                      CSC 465 Group Project  Zack Larsen

################################################################################################
##################################  Making visualizations ######################################
#Set the working directory to where the datasets are

setwd("E:/DePaul/CSC 465/Project")  #On PC 
setwd("~/Desktop/Sandisk/DePaul/CSC 465/Project") #On Mac

#################################################################################
#                               load in the data

#final_df <- read.csv("final_df.csv")
#head(final_df)

#final_df_with_frequencies <- read.csv("final_df_with_frequencies.csv")
#head(final_df_with_frequencies)

#Describe the dataset
#summarise(final_df_with_frequencies$duration)


milwood <- read.csv("milwood.csv")
head(milwood)

milwood_loop <- read.csv("milwood_loop.csv")
head(milwood_loop)

milwood_freq <- read.csv("milwood_freq.csv")
head(milwood_freq)


milwood_trip_freq <- read.csv("milwood_trip_freq.csv")
head(milwood_trip_freq)

#new_milwaukee <- 

new_milwaukee_with_frequencies <- read.csv("new_milwaukee_with_frequencies.csv")
head(new_milwaukee_with_frequencies)

#Create a subset with only unique stations
stations <- subset(final_df_with_frequencies,!duplicated(final_df_with_frequencies$from_name))
stations


from_to <- data.frame(From=milwood$from_name,To=milwood$to_name)
head(from_to)

final_df_with_trip_frequencies <- read.csv("final_df_with_trip_frequencies.csv")
head(final_df_with_trip_frequencies)

colnames(final_df_with_trip_frequencies)
final_df_with_trip_frequencies$Path.frequency

#find the max frequency of any path
max(final_df_with_trip_frequencies$Path.frequency) 

#################################################################################
#Must load ggmap package ahead of time
install.packages("ggmap")
library(ggmap)
library(ggplot2)


#Make a map of the Milwaukee and Wood station
map<-get_map(location='wicker park', zoom=13, maptype='terrain') 
ggmap(map)+ 
  geom_point(aes(x=milwood$to_long, 
                 y=milwood$to_lat,
                 size=2), 
             data=milwood, alpha=.5)

#Compute average age by gender of milwood riders
tapply(milwood$age, milwood$gender, mean)
#Compute average duration by gender of milwood riders
tapply(milwood$duration, milwood$gender, mean)
#Compute average duration by user type of milwood riders
tapply(milwood$duration, milwood$user_type, mean)

#Compute number of trips per station
head(final_df)
library(plyr)
counts <- count(final_df,"from_name")
counts
counts$from_name

#Compute number of unique trips (trips that have the same from_name and to_name as other trips)
trips <- ddply(final_df, .(final_df$from_name,final_df$to_name), nrow)
head(trips)
names(trips) <- c("from_name","to_name","trip_freq")
head(trips)
head(milwood_freq)
unique_milwaukee <- merge(trips,milwood_freq,by=c("from_name","to_name"))
unique_milwaukee




#Map all divvy stations with size mapped to frequency of trips
station_map <- get_map(location='chicago',zoom=14,maptype='roadmap')
ggmap(station_map) +
  geom_point(aes(x=stations$from_long,y=stations$from_lat,size=(stations$freq)), 
             data=stations,col="black",shape=1)


#####################################################################################
#####################################################################################

#Plot a map with paths between points
wicker_map<-get_map(location='wicker park', zoom=15, maptype='roadmap') 
#Using made up coordinates just to show path between two points
d <- data.frame(lat=c(41.90765,41.90700,41.90,41.908),lon=c(-87.67255,-87.67000,-87.6722,-87.67350))
d
p <- ggmap(wicker_map)
p <- p + geom_point(data=d,aes(x=lon,y=lat),color="red",size=3)
p + geom_path(data=d,aes(x=lon,y=lat),color="black",size=1) + ggtitle("Station connections map")

#Using line instead of path, because we don't want to follow an ordered path
p + geom_line(data=d,aes(x=lon,y=lat),color="black",size=1) + ggtitle("Station connections map")


#####################################################################################
#Using coordinates from our dataset with color mapping
head(new_milwaukee_with_frequencies)
chicago<-get_map(location='chicago', zoom=12, maptype='roadmap') 

d <- data.frame(lat=new_milwaukee_with_frequencies$to_lat,
                lon=new_milwaukee_with_frequencies$to_lon,  
                freq=new_milwaukee_with_frequencies$new_freq)
d
p <- ggmap(chicago)+
  geom_point(data=d,aes(x=lon,y=lat),color="red",size=2)+
  geom_line(data=d,aes(x=lon,y=lat),color=d$freq,size=2) +
  ggtitle("Station connections map") +
  xlab("Longitude") +
  ylab("Latitude")
p

#####################################################################################
#Using the frequencies to map the size of the points
max(d$freq) #Most frequent destination from milwaukee and wood. It is the clybourn station metra
520/10979 #Proportion of the time that someone went from milwaukee and wood to marshfield and cortland

chicago<-get_map(location='chicago', zoom=12, maptype='roadmap') 
d <- data.frame(lat=new_milwaukee_with_frequencies$to_lat,
                lon=new_milwaukee_with_frequencies$to_lon,  
                freq=new_milwaukee_with_frequencies$new_freq)
d
p <- ggmap(chicago)+
  geom_point(data=d,aes(x=lon,y=lat),color="red",size=d$freq)
p

z <- p + geom_line(data=d,aes(x=lon,y=lat),color="black",size=2) +
  ggtitle("Station connections map") +
  xlab("Longitude") +
  ylab("Latitude")
z


#####################################################################################
#Same as above but plotting only 10 busiest paths
library(plyr)
#Sort the dataframe in decreasing order by path frequency
sorted <- new_milwaukee_with_frequencies[order(-new_milwaukee_with_frequencies$new_freq),]
sorted
#Create something that will contain the names of the 10 most popular station names
top20names <- count(new_milwaukee_with_frequencies,"to_name")
top20names <- top20names[order(-top20names$freq),]
top20names

top20list <- NULL #This initializes a list of the 10 most popular destination station ID's

top20names$to_name[1]
top20names$freq[1]

for(i in 1:20){
  Name <- as.character(top20names$to_name[i])
  FREQ <- top20names$freq[i]
  phrase <- paste("#",i,"most popular destination is",Name,"with a frequency of",FREQ,"trips")
  print(phrase)
  top20list[i] <- Name
}

top20list
top20list[1]

#Make a subset of the coordinate data where the to_station is in the top10list
top20coords <- subset(new_milwaukee_with_frequencies,
                      new_milwaukee_with_frequencies$to_name==top20list[1] |
                      new_milwaukee_with_frequencies$to_name==top20list[2] | 
                      new_milwaukee_with_frequencies$to_name==top20list[3] |
                      new_milwaukee_with_frequencies$to_name==top20list[4] | 
                      new_milwaukee_with_frequencies$to_name==top20list[5] |
                      new_milwaukee_with_frequencies$to_name==top20list[6] | 
                      new_milwaukee_with_frequencies$to_name==top20list[7] |
                      new_milwaukee_with_frequencies$to_name==top20list[8] | 
                      new_milwaukee_with_frequencies$to_name==top20list[9] |
                      new_milwaukee_with_frequencies$to_name==top20list[10] |
                      new_milwaukee_with_frequencies$to_name==top20list[11] |
                      new_milwaukee_with_frequencies$to_name==top20list[12] | 
                      new_milwaukee_with_frequencies$to_name==top20list[13] |
                      new_milwaukee_with_frequencies$to_name==top20list[14] | 
                      new_milwaukee_with_frequencies$to_name==top20list[15] |
                      new_milwaukee_with_frequencies$to_name==top20list[16] | 
                      new_milwaukee_with_frequencies$to_name==top20list[17] |
                      new_milwaukee_with_frequencies$to_name==top20list[18] | 
                      new_milwaukee_with_frequencies$to_name==top20list[19] |
                      new_milwaukee_with_frequencies$to_name==top20list[20])


chicago<-get_map(location='leavitt and milwaukee chicago', zoom=13, maptype='roadmap') 
d <- data.frame(lat=top20coords$to_lat,lon=top20coords$to_lon)
p <- ggmap(chicago) +
  geom_point(data=d,aes(x=lon,y=lat),color="red",size=0.045*top20coords$new_freq,shape=15) +
  ggtitle("Top 20 destinations from Milwaukee and Wood") +
  xlab("Longitude") +
  ylab("Latitude")
p

#Connect with lines
p + geom_line(data=top20coords,aes(x=top20coords$to_lon,y=top20coords$to_lat),color=top20coords$new_freq,size=1,alpha=0.7) +
  ggtitle("Station connections map") +
  xlab("Longitude") +
  ylab("Latitude")

#####################################################################################
#####################################################################################
# At this point, what we want to do is find a way to connect the lines using the 
# frequency counts for each path, but geom_line and geom_path don't accomlplish this.



##################################################################
#Using coordinates from our dataset with line width mapping
chicago<-get_map(location='chicago', zoom=12, maptype='roadmap') 
d <- data.frame(lat=milwood_trip_freq$to_lat,lon=milwood_trip_freq$to_long)
d
p <- ggmap(chicago)
p <- p + geom_point(data=d,aes(x=lon,y=lat),color="red",size=2)
p + geom_line(data=d,aes(x=lon,y=lat),color="black",size=0.00000045*milwood_trip_freq$Path.frequency) +
  ggtitle("Station connections map") +
  xlab("Longitude") +
  ylab("Latitude")

#################################################################
#Attempting to map color of connection lines to unique trip frequency counts
chicago<-get_map(location='chicago', zoom=12, maptype='roadmap') 
d <- data.frame(lat=unique_milwaukee$to_lat,lon=unique_milwaukee$to_long)
d
p <- ggmap(chicago)
p <- p + geom_point(data=d,aes(x=lon,y=lat),color="red",size=2)
p + geom_line(data=d,aes(x=lon,y=lat),color=unique_milwaukee$trip_freq,size=2) +
  ggtitle("Station connections map") +
  xlab("Longitude") +
  ylab("Latitude")


#################################################################
#Attempting to map size of connection lines to unique trip frequency counts
chicago<-get_map(location='chicago', zoom=12, maptype='roadmap') 
d <- data.frame(lat=unique_milwaukee$to_lat,lon=unique_milwaukee$to_long)
d
p <- ggmap(chicago)
p <- p + geom_point(data=d,aes(x=lon,y=lat),color="red",size=2)
p + geom_line(data=d,aes(x=lon,y=lat),color="black",size=unique_milwaukee$trip_freq) +
  ggtitle("Station connections map") +
  xlab("Longitude") +
  ylab("Latitude")





























