food <- FoodSrvcByCounty
install.packages("choroplethr")
install.packages("choroplethrMaps")
library(choroplethr)
data(df_pop_state)
data(df_state_demographics)
state_choropleth(df_pop_state, title="2012 Population by State", legend="Population")
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

