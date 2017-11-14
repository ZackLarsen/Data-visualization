############################## CSC 465 Group Project############################
#Set the working directory to where the datasets are
setwd("E:/DePaul/CSC 465/Project") # On PC
setwd("~/Desktop/Sandisk/DePaul/Past classes/CSC 465/Project") #On Mac

################################################################################
################################Loading in the data#############################
################################################################################

#Stacking all 4 quarters on top of each other- we want to take the files
#that are available on Divvy and merge them together by rows
divvyq1 <- read.csv("Divvy_Trips_2015-Q1.csv", header=T, as.is=T)
divvyq2 <- read.csv("Divvy_Trips_2015-Q2.csv", header=T, as.is=T)
divvy7 <- read.csv("Divvy_Trips_2015_07.csv", header=T, as.is=T)
divvy8 <- read.csv("Divvy_Trips_2015_08.csv", header=T, as.is=T)
divvy9 <- read.csv("Divvy_Trips_2015_09.csv", header=T, as.is=T)
divvyq4 <- read.csv("Divvy_Trips_2015_Q4.csv", header=T, as.is=T)

#To stack the rows of two data frames, use rbind:
all.rows <- rbind(divvyq1,divvyq2,divvy7,divvy8,divvy9,divvyq4)

#Taking a look at the merged dataset for all of 2015
head(all.rows)

# At this point, we have a data object containing all of the data for 2015, named all.rows

# Now, we need to find the data that includes the coordinates for the stations, because Divvy doesn't 
# include that in their datasets:
#Station Location data from Chicago data portal (Divvy_Stations_2015.csv)
# This location data can be found at: https://data.cityofchicago.org/Transportation/Divvy-Bicycle-Stations/bbyy-e7gq
#Must import this as a .csv first
#I have changed the locations file so it only includes the columns I need, and I changed the names. The columns are as follows:
# id, name(station name), latitude, longitude, dpcapacity, landmark
locations <- read.csv("Divvy_Stations_2015.csv",header=T,as.is=T)
locations
ldf <- as.data.frame(locations)
head(ldf)





# Now we have all of our data loaded in, so it's time to do some munging:

#################################################################################
###################################Munging#######################################
#################################################################################

#Make two dataframes for the coordinates; one for the beginning and one for the ending station
from_ldf <- ldf
head(from_ldf)
colnames(from_ldf)[1] <- "from_station_id"
head(from_ldf)
#Second, make a new df where the id is renamed to to_station_id 
to_ldf <- ldf
head(to_ldf)
colnames(to_ldf)[1] <- "to_station_id"
head(to_ldf)


#First, we have to merge the all.rows dataframe with the ldf on "from_station_name" and
#rename the columns in that new df so that each column begins with
# _from
from_merge <- merge(all.rows,from_ldf,by="from_station_id")
head(from_merge)
colnames(from_merge)[13] <- "from_station_name"
colnames(from_merge)[14] <- "from_station_lat"
colnames(from_merge)[15] <- "from_station_long"
colnames(from_merge)[16] <- "from_station_capacity"
colnames(from_merge)[17] <- "from_station_landmark"
head(from_merge)


#Then, we have to merge the all.rows dataframe with the ldf on "to_station_name"
#First, we have to merge the rdf with the ldf on "to_station_name" and
#rename the columns in that new df so that each column begins with
# _to

to_merge <- merge(all.rows,to_ldf,by="to_station_id")
head(to_merge)
colnames(to_merge)[13] <- "to_station_name"
colnames(to_merge)[14] <- "to_station_lat"
colnames(to_merge)[15] <- "to_station_long"
colnames(to_merge)[16] <- "to_station_capacity"
colnames(to_merge)[17] <- "to_station_landmark"
head(to_merge)



#Finally, we have to merge the to_merge and the from_merge data frames
#together so we have a data frame with the geo location data for both the 
#from station and the to station
merge_df <- merge(to_merge,from_merge,by="trip_id")
head(merge_df)
#There are duplicated columns, so we have to remove starttime.y, stoptime.y,bikeid.y,
#tripduration.y,usertype.y,gender.y, and birthyear.y
final_df <- data.frame(merge_df$trip_id,
                       merge_df$starttime.x,
                       merge_df$stoptime.x,
                       merge_df$bikeid.x,
                       merge_df$tripduration.x,
                       merge_df$usertype.x,
                       merge_df$gender.x,
                       merge_df$birthyear.x,
                       merge_df$to_station_id.x,
                       merge_df$to_station_name.x,
                       merge_df$to_station_lat,
                       merge_df$to_station_long,
                       merge_df$to_station_capacity,
                       merge_df$to_station_landmark,
                       merge_df$from_station_id.x,
                       merge_df$from_station_name.x,
                       merge_df$from_station_lat,
                       merge_df$from_station_long,
                       merge_df$from_station_capacity,
                       merge_df$from_station_landmark
)
head(final_df)
#We can rename the data frame columns here to make it more appealing
colnames(final_df)
names(final_df) <- c("tripid",
                     "start",
                     "stop",
                     "bike",
                     "duration",
                     "user_type",
                     "gender",
                     "birth_year",
                     "to_id",
                     "to_name",
                     "to_lat",
                     "to_long",
                     "to_capacity",
                     "to_landmark",
                     "from_id",
                     "from_name",
                     "from_lat",
                     "from_long",
                     "from_capacity",
                     "from_landmark")

colnames(final_df)



#Define a variable for the coordinates
from_lat <- final_df$from_lat
from_long <- final_df$from_long
from_location <- paste(from_lat,',',from_long)
head(from_location)

to_lat <- final_df$to_lat
to_long <- final_df$to_long
to_location <- paste(to_lat,',',to_long)
head(to_location)

final_df$from_location <- from_location
final_df$to_location <- to_location
head(final_df)

final_df$age <- 2016 - final_df$birth_year   #Calculate the age of the riders based on birth year

head(final_df)
#Writing final_df out to a .csv file 
write.csv(final_df, file = "final_df.csv")











#################################################################################
#################################################################################
                      #Make a few subsets of final_df

setwd("~/Desktop/Sandisk/DePaul/CSC 465/Project") #On Mac
final_df <- read.csv("final_df.csv")

milwood <- subset(final_df,final_df$from_name=='Wood St & Milwaukee Ave')
write.csv(milwood, file = "milwood.csv")




#Add the frequency counts to the final_df
head(final_df)
library(plyr)
counts <- count(final_df,"from_name")
counts$from_name
final_df_with_frequencies <- merge(final_df,counts,by="from_name") #Merge the frequency counts to the final_df
head(final_df_with_frequencies)
final_df_with_frequencies$freq


# What we really need is a column that has both the from station and the to station
# pasted together as a string. We can call this the path name.
final_df_with_frequencies$path_name <- paste('From',final_df_with_frequencies$from_name,'to',final_df_with_frequencies$to_name)
write.csv(final_df_with_frequencies, file = "final_df_with_frequencies.csv")
final_df_with_frequencies <- read.csv("final_df_with_frequencies.csv")
head(final_df_with_frequencies)



#Then, we can use plyr to make counts of unique trip names
trip_counts <- count(final_df_with_frequencies,"path_name")
trip_counts
head(trip_counts)

final_df_with_trip_frequencies <- merge(final_df_with_frequencies,trip_counts,by="path_name")
head(final_df_with_trip_frequencies)
colnames(final_df_with_trip_frequencies)[27] <- "Path frequency"
colnames(final_df_with_trip_frequencies)
write.csv(final_df_with_trip_frequencies, file = "final_df_with_trip_frequencies.csv")












#####################################################################
#Make a dataset for use in Tableau as a hub and spoke diagram
setwd("~/Desktop/Sandisk/DePaul/CSC 465/Project") #On Mac
combined <- read.csv("new_milwaukee_with_frequencies.csv")
head(combined)
colnames(combined)
#what we need here are the columns tripid, pathname, from lat, from lon, 
#to lat, to lon, and a new column that is either 1 or 2 for path order
#note that frequency goes only in the to_spoke dataset
#from_hub will be the dataset containing the from station
from_hub <- data.frame(tripid=combined$tripid,path=combined$path,
                          lat=combined$from_lat,lon=combined$from_lon,
                          freq=NA,
                          path_order=1)
head(from_hub)

#what we need here are the columns tripid, pathname, from lat, froma lon, 
#to lat, to lon, frequency, and a new column that is either 1 or 2 for path order

#to_spoke will be the dataset containing the to station
to_spoke <- data.frame(tripid=combined$tripid,path=combined$path,
                          lat=combined$to_lat,lon=combined$to_lon,
                          freq=combined$new_freq,
                          path_order=2)

head(to_spoke)



# This is a function to combine the two dataframes one row at a time:

# zipFastener for TWO dataframes of unequal length (1 for columns,2 for rows)
zipFastener <- function(df1, df2, along=2)
{
  # parameter checking
  if(!is.element(along, c(1,2))){
    stop("along must be 1 or 2 for rows and columns
         respectively")
  }
  # if merged by using zip feeding along the columns, the
  # same no. of rows is required and vice versa
  if(along==1 & (ncol(df1)!= ncol(df2))) {
    stop ("the no. of columns has to be equal to merge
          them by zip feeding")
  }
  if(along==2 & (nrow(df1)!= nrow(df2))) {
    stop ("the no. of rows has to be equal to merge them by
          zip feeding")
  }
  # zip fastener preperations
  d1 <- dim(df1)[along]
  d2 <- dim(df2)[along]
  i1 <- 1:d1           # index vector 1
  i2 <- 1:d2 + d1      # index vector 2
  # set biggest dimension dMax
  if(d1==d2) {
    dMax <- d1
  } else if (d1 > d2) {
    length(i2) <- length(i1)    # make vectors same length, 
    dMax <- d1                  # fill blanks with NAs   
  } else  if(d1 < d2){
    length(i1) <- length(i2)    # make vectors same length,
    dMax <- d2                  # fill blanks with NAs   
  }
  # zip fastener operations
  index <- as.vector(matrix(c(i1, i2), ncol=dMax, byrow=T))
  index <- index[!is.na(index)]         # remove NAs
  
  if(along==1){
    colnames(df2) <- colnames(df1)   # keep 1st colnames                  
    res <- rbind(df1,df2)[ index, ]  # reorder data frame
  }
  if(along==2) res <- cbind(df1,df2)[ , index]           
  
  return(res)
}


zipped <- zipFastener(from_hub,to_spoke,1)
#Reset the index column
rownames(zipped) <- 1:nrow(zipped)
#Write the new dataset to file
write.csv(zipped, file = "hub_and_spoke.csv")
hub_and_spoke <- read.csv("hub_and_spoke.csv")
head(hub_and_spoke)

################################################################################################
#####################################Done munging data##########################################
################################################################################################

# Now, we can take this hub_and_spoke.csv file and import it into tableau to create the hub and spoke diagram

