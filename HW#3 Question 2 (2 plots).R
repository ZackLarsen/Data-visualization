# HW#3 Question 2
#Load in the divvy 2015 q4 data     Divvy_Trips_2015_Q4.csv
divvyq4 <- Divvy_Trips_2015_Q4
head(divvyq4)
subscribers <- subset(divvyq4,divvyq4$usertype=='Subscriber')
head(subscribers)
subscribers$age <- 2016 -subscribers$birthyear

#Violin plot
ggplot(subscribers,aes(x=subscribers$gender,y=subscribers$age)) +
  geom_violin(fill="skyblue") + 
  ggtitle("Violin plot of age and gender") + 
  xlab("Gender") + 
  ylab("Age") + 
  ylim(15,80)


#Box plot
ggplot(subscribers,aes(x=subscribers$gender,y=subscribers$age)) +
  geom_boxplot(fill="red") + 
  ggtitle("Boxplot of age by divvy rider gender") + 
  ylab("Rider Age") + 
  xlab("Rider Gender")

#Choropleth
#We'll do this in tableau, so we must write the data frame to a .csv file
write.csv(subscribers, file = "subs.csv")
