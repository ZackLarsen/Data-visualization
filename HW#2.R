#HW#2 Zack Larsen

#Problem 2

#2a
# reading in data from a csv file
perception <- read.csv("PerceptionExperiment2007-2015Fall.csv")
perception <-PerceptionExperiment2007.2015Fall
head(perception)


abs_error <- abs(perception$Response-perception$TrueValue)
abs_error
test <- perception$Test
perception$abs_error <- abs_error


#2c
#This is a univariate scatterplot of absolute errors by test
qplot(abs_error, test, data=perception, size=1, color=factor(Test),main="Plot of Absolute Errors by Test",xlab='Absolute Error',ylab='Test')





#2c
error <- (perception$TrueValue-perception$Response)
error
perception$error <- error
#This is a univariate scatterplot of erros by test
qplot(error, test, data=perception, size=1, color=factor(Test),main="Plot of Errors by Test",xlab='Error',ylab='Test')




#2d Subset of subjects 56-73
subset2d <- subset(perception, Subject>56 & Subject <73)
head(subset2d)

qplot(abs_error, Display, data=subset2d, size=1, color=factor(Display),main="Plot of Absolute Errors by Display",xlab='Absolute Error',ylab='Display') + ylim(0,3)




#2e
test2e <- perception$Test
test2e
subset2e <- subset(perception,Display==1 & Subject >55 & Subject <65 & test2e=='Vertical Distance, Non-Aligned')
subset2e

vert_disp_one <- subset(perception,Display==1 & test2e=='Vertical Distance, Non-Aligned')
vert_disp_one

ggplot(vert_disp_one,aes(x=vert_disp_one$Trial,y=vert_disp_one$Response)) + geom_boxplot() + ggtitle("Problem #2e: looking for outliers")


#Violin
ggplot(vert_disp_one,aes(x=vert_disp_one$Trial,y=vert_disp_one$Response)) + geom_violin(fill = "skyblue", colour = "red") + ggtitle("Problem #2e: looking for outliers") + xlab("Trial") + ylab("Response") + theme(axis.title.y = element_text(face="bold", colour="#990000", size=20),axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) + theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),axis.text.x  = element_text(angle=90, vjust=0.5, size=16))  



# Horizontal Violin
ggplot(vert_disp_one,aes(x=vert_disp_one$Trial,y=vert_disp_one$Response)) + geom_violin(fill = "skyblue", colour = "red") + geom_jitter() + ggtitle("Horizontal version of the violin plot for problem #2e") + coord_flip()




#2f
perception$pos <- perception$error>=0
pos <- perception$pos
positives <- subset(perception,error >= 0)
negatives <- subset(perception,error <0)

#Here, I am creating a bar plot for 2f that plots the error by test so we can visualize which test were
#overestimated and which were underestimated
heights <- tapply(perception$error, perception$Test, mean)
barplot(heights)
barplot(heights,
      main="Mean Error by Test",
      xlab="Test", 
      ylab="Error",
      col='green')

#For my analysis of 2f, I want to see if there is over- and under- estimation present in 
# tests with true values that are high and low, respectively.
#Here, I create subsets of the data with high or low values
subset2fhigh <- subset(perception,TrueValue>0.79)
subset2flow <- subset(perception,TrueValue<0.21)
#Next, I want to create 2 separate graphs plotting errors by test 
high_heights <- tapply(subset2fhigh$error, subset2fhigh$Test, mean)
low_heights <- tapply(subset2flow$error, subset2flow$Test, mean)

barplot(high_heights,
        main="Mean Error by Test",
        xlab="Test", 
        ylab="Error",
        col='green')

barplot(low_heights,
        main="Mean Error by Test",
        xlab="Test", 
        ylab="Error",
        col='pink')



#This is a way to get a bar graph indicating different colors for positive errors and negative errors
ggplot(perception, aes(x=Test, y=error, fill=pos)) +
  geom_bar(stat="identity", position="identity")






#########################################################

#Problem 3

#3a
intel <- read.csv("Intel1990-2000.csv")
adjclose <- intel$Adj.Close
date <- intel$Date
volume <- intel$Volume
head(intel)

# convert date info in format 'mm/dd/yyyy'
strDates <- c(date)
dates <- as.Date(strDates, "%m/%d/%Y")
intel$dates <- dates

#Using DePaul computer
intel2 <- Intel1990.2000
head(intel2)
intel2$day = seq(nrow(intel2))    # Add a sequential count of the records to indicate the "day"
head(intel2)
library(ggplot2)

ggplot(intel2, aes(x=day, y=Adj.Close)) + geom_line(aes(size=Volume)) + ggtitle("Problem #3a: Adjusted close by day")



#3b Using volume to alter colour
ggplot(intel2, aes(x=day, y=Adj.Close)) + geom_line(aes(colour=Volume)) + ggtitle("Problem #3b: Adjusted close by day")
#Below, we have changed the size in order to see the colour better
ggplot(intel2, aes(x=day, y=Adj.Close)) + geom_line(aes(size=Volume,colour=Volume)) + ggtitle("Problem #3b: Adjusted close by day, size adjusted")




#3c
ggplot(intel2, aes(x=day, y=Adj.Close)) + geom_line(aes(size=Volume)) + ggtitle("Problem #3a: Adjusted close by day")
ggplot(intel2, aes(x=day, y=Adj.Close)) + geom_line(aes(size=Volume)) + ggtitle("Problem #3c: Adjusted close by day, with log10 scaling") + scale_x_log10() + scale_y_log10()

# To use log2 scaling. I'm having issues here, however, because "scales" package is not installing correctly
library(scales)     # Need the scales package
sp + scale_y_continuous(trans=log2_trans())



#3d see tableau for this question


#3e see tableau for this question



#############################################################################
#Problem 4

#4a
montana <- MontanaPopulationData
head(montana)
library(ggplot2)
library(scales)
library(MASS)
sp <-ggplot(montana, aes(x=Year, y=Population)) + geom_line(colour="red",size=4)
sp + scale_y_continuous(trans=log2_trans())  + ggtitle("Problem #4a: Montana data, with log2 scaling")


#4b which years had the greatest percentage increase in population? (Using log-base-10 here for orders of magnitude)
bp <- ggplot(montana, aes(x=Year, y=Population)) + geom_line(colour="red",size=4) 
bp +  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + ggtitle("Problem #4b: Montana data, with log10 scaling") + theme(axis.title.y = element_text(face="bold", colour="#990000", size=20),axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) + theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),axis.text.x  = element_text(angle=90, vjust=0.5, size=16))
 
montana$percentage <- montana$Population/142924  
percent <- ggplot(montana, aes(x=Year, y=percentage)) + geom_line(colour="red",size=4) 
percent
percent +  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + ggtitle("Problem #4b: Montana data, with log10 scaling") + theme(axis.title.y = element_text(face="bold", colour="#990000", size=20),axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) + theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),axis.text.x  = element_text(angle=90, vjust=0.5, size=16))
montana$percentage

#4c
bp +  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + ggtitle("Problem #4b: Montana data, with log10 scaling") + theme(axis.title.y = element_text(face="bold", colour="#990000", size=20),axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) + theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),axis.text.x  = element_text(angle=90, vjust=0.5, size=16))
ggplot(montana, aes(x=Year, y=Population)) + geom_line(colour="blue",size=4) + annotation_logticks() + ggtitle("Problem #4c: Montana data, with log scaling to show 15% increases") + scale_y_log10() + xlim(1890,2000)






#######################################################################################
#Problem 5

#5a
messier <- MessierData
messier
head(messier)
ggplot(messier, aes(x=messier$Messier.., y=messier$Apparent.Magnitude)) + geom_point(colour="blue",size=4) + ggtitle("Problem #5a: Messier data, demonstrating link between messier number and apparent magnitude")

#5b
ggplot(messier, aes(x=messier$Kind, y=messier$Distance..LY.)) + geom_point(colour="turquoise",size=6,shape=16) + ggtitle("Problem #5b: Messier data, demonstrating link between kind and distance in light years") + scale_y_log10() + scale_x_discrete(limits=c("Asterism","Diffuse Nebula","Open Cluster","Planetary Nebula","Star Cloud","Globular Cluster","Galaxy"))

#5c scatter plot
ggplot(messier, aes(x=messier$Distance..LY., y=messier$Apparent.Magnitude,colour=messier$Kind)) + geom_point(size=4,shape=11) + ggtitle("Problem #5c: Scatterplot of distance vs. magnitude") + scale_x_log10() + scale_y_reverse()

#5d
ggplot(messier, aes(x=messier$Distance..LY., y=messier$Apparent.Magnitude,colour=messier$Kind,size=messier$Size.....)) + geom_point(shape=11) + ggtitle("Problem #5d: Scatterplot of distance vs. magnitude, adjusting size by angular size") + scale_x_log10() + scale_y_reverse()


#########################################################################################
#6a
portland <- PortlandWaterLevel2003
head(portland)
portland$hour = seq(nrow(portland))
head(portland)
four_days <- subset(portland,hour>2 & hour<96)
ggplot(four_days, aes(x=four_days$hour,y=four_days$WL)) + geom_point(colour="grey60") + stat_smooth() +ggtitle("Problem #6a")

#This is to create a line bar of the change in water level by hour, with no smoothing
days_subset <- subset(portland,hour>2 & hour<240)
sp <- ggplot(days_subset, aes(x=days_subset$hour,y=days_subset$WL))
sp + geom_point(colour="grey60") + stat_smooth() 
sp + geom_line(colour="blue") + ggtitle("Problem #6a with no smoothing, over 10 days ")

#6b
January <- subset(portland,portland$hour<100)
January
ggplot(January, aes(x=January$hour,y=January$WL)) + geom_line(size=2,colour="Blue") + ggtitle("Problem #6b : water level tidal cycle, shown over approx. 4 days")




















###################################RANDOM NOTES##############################
# logscale (look in R Graphics Cookbook for more detail)
library(MASS)
p <- ggplot(Animals, aes(x=body, y=brain, label=rownames(Animals))) +
  geom_text(size=3)
p
p + scale_x_log10() + scale_y_log10()




ds = as.data.frame(intel2)
head(ds)
ds$day = seq(nrow(ds))    # Add a sequential count of the records to indicate the "day"
head(ds)

library(reshape)
head(ds)
IntelStock = melt(ds, id=c("day"),"Adj.Close","Volume")
head(IntelStock)
names(IntelStock)[2] = "Volume"
head(IntelStock)
ds[6]



