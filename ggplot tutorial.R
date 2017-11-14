#This is the ggplot2 tutorial from CSC 465
install.packages("ggplot2")
library("ggplot2")
head(diamonds)
head(mtcars)

# qplot histogram
qplot(clarity, data=diamonds, fill=cut, geom="bar")

#ggplot histogram -> same output
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar() 
ggplot(diamonds, aes(price,fill=cut)) + geom_bar()

qplot(wt, mpg, data=mtcars)
qplot(log(wt), mpg - 10, data=mtcars)
qplot(wt, mpg, data=mtcars, color=qsec)

# change size of points (hint: color/colour, hint: set aesthetic/mapping)
qplot(wt, mpg, data=mtcars, color=qsec, size=3) 
qplot(wt, mpg, data=mtcars, colour=qsec, size=I(3))
# use alpha blending
qplot(wt, mpg, data=mtcars, alpha=qsec)


# continuous scale vs. discrete scale
head(mtcars)
qplot(wt, mpg, data=mtcars, colour=cyl) 
levels(mtcars$cyl)
qplot(wt, mpg, data=mtcars, colour=factor(cyl))


# use different aesthetic mappings
qplot(wt, mpg, data=mtcars, shape=factor(cyl)) 
qplot(wt, mpg, data=mtcars, size=qsec)
# combine mappings (hint: hollow points, geom-concept, legend combination)
qplot(wt, mpg, data=mtcars, size=qsec, color=factor(carb))
qplot(wt, mpg, data=mtcars, size=qsec, color=factor(carb), shape=I(1)) 
qplot(wt, mpg, data=mtcars, size=qsec, shape=factor(cyl), geom="point") 
qplot(wt, mpg, data=mtcars, size=factor(cyl), geom="point")


# bar-plot
qplot(factor(cyl), data=mtcars, geom="bar")
# flip plot by 90°
qplot(factor(cyl), data=mtcars, geom="bar") + coord_flip() 

# difference between fill/color bars
qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(cyl)) 
qplot(factor(cyl), data=mtcars, geom="bar", colour=factor(cyl))

# fill by variable
qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(gear))
# use different display of bars (stacked, dodged, identity)
head(diamonds)
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="stack") 
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="dodge") 
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="fill") 
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="identity")


#Code below doesn't work. Position is deprecated.
qplot(clarity, data=diamonds, geom="freqpoly", group=cut, colour=cut, position="identity") 
qplot(clarity, data=diamonds, geom="freqpoly", group=cut, colour=cut, position="stack")


# using pre-calculated tables or weights (hint: usage of ddply in package plyr)
install.packages(plyr)
table(diamonds$cut)
t.table <- ddply(diamonds, c("clarity", "cut"), "nrow") 
head(t.table)
qplot(cut, nrow, data=t.table, geom="bar")
qplot(cut, nrow, data=t.table, geom="bar", stat="identity")
qplot(cut, nrow, data=t.table, geom="bar", stat="identity", fill=clarity)
qplot(cut, data=diamonds, geom="bar", weight=carat)
qplot(cut, data=diamonds, geom="bar", weight=carat, ylab="carat")