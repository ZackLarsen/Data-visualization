install.packages("ggplot2")
library("ggplot2")

sex = InfantData$Sex
height = InfantData$Height.in
weight = InfantData$Weight.lbs
female = subset(InfantData,sex =='F')
male = subset(InfantData,sex=='M')

sex
height
weight
female
male

with(InfantData,plot(weight,height,main="Scatterplot of height vs. weight of infants",xlab="Height",ylab="Weight",col= ifelse(sex == 'F',"red","blue")))

with(InfantData,plot(weight,height,main="Scatterplot of height vs. weight of infants",xlab="Height",ylab="Weight",col= ifelse(sex == 'F',"pink","blue")))



#3b
#First, we have to make subsets of the data by sex:
female = subset(InfantData,sex =='F')
male = subset(InfantData,sex=='M')
female
male
ym = male$Height.in
xm = male$Weight.lbs
yf = female$Height.in
xf = female$Weight.lbs
male_model = lm(ym~xm)
female_model = lm(yf~xf)

#Then, we need to plot the data for both sexes.
with(InfantData,plot(weight,height,main="Scatterplot of height vs. weight of infants",xlab="Height",ylab="Weight",col= ifelse(sex == 'F',"coral2","blue"),col.main="blue",col.axis="red",col.lab="darkgreen",pch=2,cex=1.5))
grid()
#Lastly, we need to plot a trend line for each individual sex.
abline(male_model,col="blue",lwd=3)
abline(female_model,col="coral1",lwd=3)
legend(height,weight, labels, col=c(blue, coral2))





#Making a regression model with line of fit plotted on the scatterplot
y = weight
x = height
model <- lm(y ~ x)
summary(model)
plot(x,y,main="Scatterplot of height vs weight of infants",xlab="weight",ylab="height",col="red")
abline(model)


#Adding a grid to the plot:
with(InfantData,plot(weight,height,main="Scatterplot of height vs. weight of infants",xlab="Height",ylab="Weight",col= ifelse(sex == 'F',"coral2","blue"),col.main="blue",col.axis="red",col.lab="darkgreen",pch=as.integer(sex),cex=1.5,type='n'))
grid()
points(weight,height)
abline(male_model,col="blue",lwd=3)
abline(female_model,col="coral1",lwd=3)
