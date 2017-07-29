install.packages("ggplot2")
library("ggplot2")
head(mtcars)
head(diamonds)

#For some reason, R can't find the file below:
tbl <- read.csv("transactions.csv")


head(transactions)

with(transactions,plot(Category,Amount,main="Plot of spending for each category", xlab="Category",ylab="Purchase amount",type="n"))
grid()
#This is to add the points after creating the grid so the grid doesn't cover up the points,
#but for some reason the points already get plotted in my first command above
with(transactions,points(Category,Amount))


with(transactions,plot(Category,Amount,main="Plot of spending for each category", xlab="Category",ylab="Purchase amount"))

with(transactions,hist(Amount))

#To get descriptive statistics on a vector, in this case "Amount"
with(transactions,summary(Amount))

#This gives us a summary of all of our vectors in the "transactions" data set,
#with descriptive statistics only on the numeric vectors:
summary(transactions)

#This will convert all of our values in the "Amount" column to z-scores
with(transactions,scale(Amount))

#Here, we can create a random vector x with specified mean of 100 and sd of 15. There will
#be 50 observations in the vector. Then, we can do a t-test to see if the means that we
#supply could feasibly be the mean of the population. Note that the p-value of the last test
#is equal to 1, because we tested a mean that is exactly equal to the sample estimate of the mean:
x <- rnorm(50, mean=100, sd=15) 
t.test(x, mu=95)
t.test(x, mu=105)
t.test(x, mu=15)
t.test(x, mu=103.8769)



#To plot individual vectors from a data set, use the code below. Note that in this example,
#mpg and wt are the two vectors being plotted from the mtcars dataset
with(mtcars,plot(mpg,wt))

with(diamonds,plot(clarity,price,main="Plot of color versus price", xlab="Color",ylab="Price"))
