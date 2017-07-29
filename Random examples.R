

######################################################################################
#Examples


#From R graphics cookbook
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimes
library(maps) # For map data
states_map <- map_data("state")
# Merge the data sets together
crime_map <- merge(states_map, crimes, by.x="region", by.y="state")
# After merging, the order has changed, which would lead to polygons drawn in
# the incorrect order. So, we sort the data.
head(crime_map)
library(plyr) # For arrange() function
# Sort by group, then order
crime_map <- arrange(crime_map, group, order)
head(crime_map)
ggplot(crime_map, aes(x=long, y=lat, group=group, fill=Assault)) +
  geom_polygon(colour="black") +
  coord_map("polyconic")

#The preceding example used the default color scale, which goes from dark to light blue.
#If you want to show how the values diverge from some middle value, you can use
#scale_fill_gradient2(), as shown in Figure 13-36:

ggplot(crimes, aes(map_id = state, fill=Assault)) +
  geom_map(map = states_map, colour="black") +
  scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B",
                       midpoint=median(crimes$Assault)) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("polyconic")

#The previous example mapped continuous values to fill, but we could just as well use
#discrete values. It’s sometimes easier to interpret the data if the values are discretized.
#For example, we can categorize the values into quantiles and show those quantiles, as
#in Figure 13-37:
#  13.18. Creating a Choropleth Map | 315
# Find the quantile bounds
qa <- quantile(crimes$Assault, c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
qa

# Add a column of the quantile category
crimes$Assault_q <- cut(crimes$Assault, qa,
                        labels=c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                        include.lowest=TRUE)
crimes
# Generate a discrete color palette with 5 values
pal <- colorRampPalette(c("#559999", "grey80", "#BB650B"))(5)
pal

ggplot(crimes, aes(map_id = state, fill=Assault_q)) +
  geom_map(map = states_map, colour="black") +
  scale_fill_manual(values=pal) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("polyconic") +
  labs(fill="Assault Rate\nPercentile")


#Another way to make a choropleth, but without needing to merge the map data with
#the value data, is to use geom_map(). As of this writing, this will render maps faster than
#the method just described.
#For this method, the map data frame must have columns named lat, long, and re
#gion. In the value data frame, there must be a column that is matched to the region
#column in the map data frame, and this column is specified by mapping it to the map_id
#aesthetic. For example, this code will have the same output as the first example
#(Figure 13-35):
# The 'state' column in the crimes data is to be matched to the 'region' column
# in the states_map data
ggplot(crimes, aes(map_id = state, fill=Assault)) +
  geom_map(map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("polyconic")




library(maps)
library(dplyr)

data(county.fips)

## Set up fake df_pop_county data frame
df_pop_county <- data.frame(region=county.fips$fips)
df_pop_county$value <- county.fips$fips
y <- df_pop_county$value
df_pop_county$color <- gray(y / max(y))

## merge population data with county.fips to make sure color column is
## ordered correctly.
counties <- county.fips %>% left_join(df_pop_county, by=c('fips'='region'))
map("county", fill=TRUE, col=counties$color)





#data(df_pop_state)
#data(df_state_demographics)
#state_choropleth(df_pop_state, title="2012 Population by State", legend="Population")
library(dplyr)
states_w_less_than_1m <- df_pop_state %>%
  filter(value < 1000000) %>% 
  mutate(value="<1M")

states_w_more_than_1m <- df_pop_state %>%
  filter(!(region %in% states_w_less_than_1m$region)) %>%
  mutate(value=">1M")

#Merge the above two dataframes
states_pop_seg_by_million <- data.frame(rbind(states_w_more_than_1m, states_w_less_than_1m))

state_choropleth(states_pop_seg_by_million)




names(df_state_demographics)
#Create a dataset with state and per capita
df_state_percapita <- data.frame(region=df_state_demographics$region, value=df_state_demographics$per_capita_income)

df_state_percapita$region <- as.character(df_state_percapita$region)

#Plot it on the choropleth
state_choropleth(df_state_percapita)

#Let’s do the exercise, now at county-level. This is available in the df_pop_county dataset.
data(df_pop_county)
county_choropleth(df_pop_county)
#How about zooming on just one state and looking into it? Let’s zoom on california
county_choropleth(df_pop_county, state_zoom="california")
#What about Illinois?
county_choropleth(df_pop_county, state_zoom="illinois")


#For this, need to install choroplethrZip package. The package is huge. ~60MB. Keep that in mind.
library(devtools)
#Uncomment and run the package installation, if you don't have it
install_github("arilamstein/choroplethrZip@v1.3.0")
library(choroplethrZip)
install.packages("mapproj")
library(mapproj)

library(ggplot2)
data(df_pop_zip)
zip_choropleth(df_pop_zip, state_zoom = "new york", title="Population of New York State by county") + coord_map()

zip_choropleth(df_pop_zip, county_zoom = 36061) + coord_map()
