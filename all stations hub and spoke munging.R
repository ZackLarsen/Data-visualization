#####################################################################
#Make a dataset for use in Tableau as a hub and spoke diagram
setwd("~/Desktop/Sandisk/DePaul/CSC 465/Project") #On Mac
all_stations_combined <- read.csv("final_df_with_trip_frequencies.csv")
head(all_stations_combined)
max(all_stations_combined$freq.x)
colnames(all_stations_combined)
all_stations_combined <- all_stations_combined[,-1]
all_stations_combined <- all_stations_combined[,-27]
all_stations_combined <- all_stations_combined[,-26]
head(all_stations_combined)

# Rename a column    colnames(all_stations_combined)[26] <- "freq"


#Add the frequency counts of the unique paths to the data frame
library(plyr)
counts <- count(all_stations_combined,"path_name")
counts
#Check the max count frequency
max(counts$freq)

all_stations_combined<- merge(all_stations_combined,counts,by="path_name") #Merge the frequency counts to the final_df
head(all_stations_combined)



#what we need here are the columns tripid, pathname, from lat, from lon, 
#to lat, to lon, and a new column that is either 1 or 2 for path order
#note that frequency goes only in the to_spoke dataset
#from_hub will be the dataset containing the from station
from_hub <- data.frame(tripid=all_stations_combined$tripid,path=all_stations_combined$path_name,
                       lat=all_stations_combined$from_lat,lon=all_stations_combined$from_long,
                       freq=NA,filter_freq=all_stations_combined$freq,
                       path_order=1)
head(from_hub)

#what we need here are the columns tripid, pathname, from lat, froma lon, 
#to lat, to lon, frequency, and a new column that is either 1 or 2 for path order

#to_spoke will be the dataset containing the to station
to_spoke <- data.frame(tripid=all_stations_combined$tripid,path=all_stations_combined$path_name,
                       lat=all_stations_combined$to_lat,lon=all_stations_combined$to_long,
                       freq=all_stations_combined$freq,filter_freq=all_stations_combined$freq,
                       path_order=2)

head(to_spoke)




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
head(zipped)
colnames(zipped)
    #Get rid of the redundant index named column "X"
    #zipped <- zipped[,-1]
all_stations_hub_and_spoke <- zipped

#Write the new dataset to file
write.csv(all_stations_hub_and_spoke, file = "all_stations_hub_and_spoke.csv")

#Load back in and check out
all_stations_hub_and_spoke <- read.csv("all_stations_hub_and_spoke.csv")
head(all_stations_hub_and_spoke)

#check to see the highest frequency counts
max(all_stations_hub_and_spoke$freq)





