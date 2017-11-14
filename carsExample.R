
library(ggplot2)
head(mtcars)

qplot(wt, mpg, data=mtcars, color=cyl)

qplot(wt, mpg, data=mtcars, color=factor(cyl))

qplot(wt, mpg, data=mtcars, shape=factor(cyl), size=5)

qplot(wt, mpg, data=mtcars, size=factor(cyl), geom="point")

qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(cyl), color=factor(cyl))

qplot(factor(cyl), data=mtcars, geom="bar", stat="identity")

qplot(factor(cyl), data=mtcars, geom="bar") + coord_flip()

#######################################################################
# If we want to change the presentation of heirarchical bar graphs
# We need to move to the full ggplot.  We'll take this one step at a
# time.  You will need the most recent version of ggplot, 2.0 or higher
# You can check the version in the Packages list in the lower-right 
# panel of RStudio.  If your version is < 2.0, download the newer version
# by re-installing the package.
#
# The first parameter to ggplot is the dataset you are graphing.  
# The second parameter is an "aesthetic" which defines what columns
# of the dataset will be used for each feature of the graph, such 
# as the x and y axes.
#######################################################################

ggplot(diamonds, aes(x=clarity))

#######################################################################
# This draws an empty graph but with the clarity "categories" on the
# x-axis.
#
# The important thing to realize is that ggplot is a function that doesn't
# draw anything by itself.  You need to "add" effects to it with +
# The easiest effect to add is the "bar" geometry, which will draw a
# bar graph.  This is EXACTLY like saying 'geom="bar"' in qplot.
# By default we get a histogram of the diamonds in the dataset with
# the various clarities.  I.e. the y-axis becomes the "count" of the 
# records.
#######################################################################

ggplot(diamonds, aes(x=clarity)) + geom_bar()

#######################################################################
# Adding parameters to geom_bar can make the graph more helpful.  For
# instance, setting the "fill" property of the main ggplot aesthetic 
# to the "cut" column in the data can create a stacked bar chart with 
# each clarity bar subdivided by the different diamond cuts.
#######################################################################

ggplot(diamonds, aes(x=clarity, fill=cut)) + geom_bar()

#######################################################################
# Finally, the geom_bar() function can use various "positions" to tell
# ggplot how to arrange the bars for the various cuts.
#######################################################################

# stack is the default, so this produces the same graph as before
ggplot(diamonds, aes(x=clarity, fill=cut)) + geom_bar(position="stack")
ggplot(diamonds, aes(x=clarity, fill=cut)) + geom_bar(position="dodge")  # side-by-side bars in each
ggplot(diamonds, aes(x=clarity, fill=cut)) + geom_bar(position="fill")  # percentage stacks
ggplot(diamonds, aes(x=clarity, fill=cut)) + geom_bar(position="identity")  # Over plotted bars (bars behind one another)

######################################################################
# There are many options that you can add onto a ggplot graph, such
# as trend lines.  Compare the following two commands
######################################################################

ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() + geom_smooth(method="lm")

# or we can remove the standard error region
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() + geom_smooth(method="lm", se=F)

# It will even automatically separate the trend lines for different 
# groups if we set the color based on a categorical value
ggplot(mtcars, aes(x=wt, y=mpg, color=factor(cyl))) + 
        geom_point() + geom_smooth(method="lm", se=F)

######################################################################
# Be sure to explore all the geometry types.  We will be encountering
# many of them over then next weeks.
######################################################################

ggplot(mtcars, aes(x=cyl, y=mpg, fill=factor(cyl))) + geom_boxplot()
