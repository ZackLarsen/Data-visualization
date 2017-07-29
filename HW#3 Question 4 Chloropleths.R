# HW#3 Question 4
install.packages("choroplethr")
install.packages("choroplethrMaps")
library(choroplethr)
library(choroplethrMaps)
#Load FULL_FIPS.csv here, then assign it to a variable below
FULL_FIPS <- FULL_FIPS
head(FULL_FIPS)
#We have to subset the food service by county dataset where state is blank.
#This will give us the aggregated data to do the state choropleth
FS_aggregate <- subset(FoodSrvcByCounty,FoodSrvcByCounty$V2=='')
head(FS_aggregate)
FS_aggregate
#Rename the columns here
names(FS_aggregate) <- c("State","City","FS97","FS02","FS97")
#Drop City, because it is an empty vector. 
FS_DF <- data.frame(FS_aggregate$State,FS_aggregate$FS97,FS_aggregate$FS02,FS_aggregate$FS97)
head(FS_DF)
#Rename the columns
names(FS_DF) <- c("State","FS97","FS02","FS07")
head(FS_DF)
#Make a dataframe to pass to the state choropleth function
STATE_DF <- data.frame(FS_DF$State,FS_DF$FS97)  
head(STATE_DF)
names(STATE_DF) <- c("region","value")
head(STATE_DF)

#Make a data frame for the counties choropleth, which includes the fips codes and fs97 value
COUNTY_DF <- data.frame(FULL_FIPS$V1,FULL_FIPS$V4)
head(COUNTY_DF)

######################################################################################
#Choropleth by state
#The state names must be lowercase
lower = as.data.frame(sapply(STATE_DF, tolower)) 
lower
#Remove duplicates. In this case, district of columbia appears twice at rows 10 and 11
#Also, we need to remove the United States in row 1
rows_to_keep <- c(FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,
                  TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,
                  TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
                  TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
                  TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
                  TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
                  TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
                  TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
                  TRUE,TRUE,TRUE,TRUE,TRUE
                  )
nodup <- lower[rows_to_keep,]
nodup

state_choro <- state_choropleth(nodup,legend="%",num_colors=1) +
  ggtitle("Food service 97 by state") +
  coord_map()
state_choro

state_choropleth(states_df,title="Food Service By State",legend="Food Service")





#Example
data(df_pop_state)
head(df_pop_state)
state_choropleth(df_pop_state, 
                 title  = "US 2012 State Population Estimates", 
                 legend = "Population")


######################################################################################
#Choropleth by county
head(COUNTY_DF)
names(COUNTY_DF) <- c("region","value")
head(COUNTY_DF)


choro_illinois = county_choropleth(COUNTY_DF, state_zoom="illinois", legend = "%",
                                   num_colors=1) + 
  ggtitle("Food service 97 by county in Texas") +
  coord_map()  # Adds a Mercator projection
choro_illinois


#Examples
data(df_pop_county)
county_choropleth(df_pop_county, 
                  title  = "US 2012 County Population Estimates", 
                  legend = "Population")

######################################################################################
#Diffusion cartogram of state data
library(maps) # For map data
states_map <- map_data("state")
head(states_map)
head(FS_DF)

#Make the states in the FS_DF lowercase
FS_DF_lower <- data.frame(state=tolower(FS_DF$State),FS_DF)
head(FS_DF_lower)
FS_DF_2 <- data.frame(FS_DF_lower$state,FS_DF_lower$FS97)
head(FS_DF_2)
#Merge the states map and food service data sets together
food_map <- merge(states_map,FS_DF_2,by.x="region",by.y="FS_DF_lower.state")
head(food_map)
library(plyr)
food_map <- arrange(food_map,group,order)
head(food_map)


ggplot(food_map, aes(x=long, y=lat, group=group, fill=food_map$FS_DF_lower.FS97)) +
  geom_polygon(colour="black") +
  coord_map("polyconic") + 
  ggtitle("Diffusion cartogram of FS97 levels by state") +
  ylab("Latitude") + 
  xlab("Longitude")















