divvyq4 <- Divvy_Trips_2015_Q4
head(divvyq4)
divvyq4$age <- 2016-divvyq4$birthyear


#Plot #1 (Histogram of birth years of riders)
ggplot(divvyq4, aes(x=divvyq4$birthyear)) +
  geom_histogram(binwidth = 1,fill="skyblue",color="black") + 
  xlim(1940,2000) +
  ggtitle("Distribution of riders' birth years") +
  xlab("Year of birth") +
  ylab("Number of riders per birth year")


#Again, but with ages instead of birth years
ggplot(divvyq4, aes(x=divvyq4$age)) +
  geom_histogram(binwidth = 1,fill="pink",color="black") + 
  xlim(15,70) +
  ggtitle("Distribution of riders' ages") +
  xlab("Age") +
  ylab("Number of riders per age bin")







#Plot #2 (Histogram of trip duration)
ggplot(divvyq4, aes(x=divvyq4$tripduration)) +
  geom_histogram(binwidth = 20,fill="aquamarine",color="black") + 
  ggtitle("Distribution of trip duration") +
  xlab("Trip duration") +
  ylab("Frequency of duration values") +
  xlim(50,1500) 
  #scale_x_continuous(breaks=seq(0,1500,50))





#Plot #3 (Most popular departure stations)
station <- factor(divvyq4$from_station_name)
station_reorder <- reorder(divvyq4$from_station_name,count,FUN=mean)


ggplot(divvyq4, aes(x=reorder(station, -station))) +
  geom_bar(fill="aquamarine",color="black") + 
  ggtitle("Distribution of departure stations") +
  xlab("Station") +
  ylab("Number of departures") 


#Messing around with reordering of categorical x axis
ggplot(divvyq4, aes(x=reorder(station, -divvyq4$tripduration),y=divvyq4$tripduration)) +
  geom_bar(fill="aquamarine",color="black") + 
  ggtitle("Distribution of departure stations") +
  xlab("Station") +
  ylab("Number of departures") 






#Plot#4 (Violin plot comparing distribution of trip duration between men and women)
#We want to subset where the user type is "subscriber" so we can 
#access the gender data
library(dplyr)
Subscribers <- subset(divvyq4,divvyq4$usertype=='Subscriber')
head(Subscribers)
gender <- Subscribers$gender
library(plyr)
male_trip <- subset(Subscribers,gender=="Male")
male_trip_mean <- summarise(male_trip, Trip = mean(male_trip$tripduration))
female_trip <- subset(Subscribers,gender=="Female")
female_trip_mean <-summarise(female_trip, Trip = mean(female_trip$tripduration))


#Looking at trip duration
ggplot(Subscribers,aes(x=Subscribers$tripduration)) +
  geom_histogram(binwidth=50,fill="pink",colour="blue") + 
  ggtitle("Histogram of trip duration") +
  ylab("Number of trips per bin") +
  xlim(0,1800) +
  xlab("Trip duration")


p <- ggplot(male_trip,aes(x=factor(male_trip$tripduration)))
p + geom_violin() 


p2 <- ggplot(female_trip,aes(x=factor(female_trip$tripduration)))
p2 + geom_violin() 





#Plot #5(Who commutes to farther stations: men or women?)







#Plot #6(Average age of riders arriving at Milwaukee and Wood station)
mwood <- subset(divvyq4, divvyq4$to_station_name == 'Wood St & Milwaukee Ave')
head(mwood)
ggplot(mwood,aes(x=mwood$age)) +
  geom_histogram(binwidth = 1,fill="pink", colour="blue") + 
  ggtitle("Distribution of ages for riders arriving at Milwaukee and Wood")

ggplot(mwood,aes(x=mwood$age)) +
  geom_histogram(binwidth = 1,fill="pink", colour="blue")



#Plot #7(Distribution of trip duration to milwaukee and wood station)
ggplot(mwood,aes(x=mwood$tripduration)) +
  geom_histogram(binwidth = 20,fill="darkgrey", colour="pink") + 
  xlim(0,2500) +
  ggtitle("Trip duration distribution to Milwaukee and Wood")


#Plot #8(Violin plot of rider age to Milwaukee and Wood)
ggplot(mwood,aes(x=mwood$age,y=mwood$tripduration)) + 
  geom_violin(fill="pink") + 
  ylim(0,3100)







