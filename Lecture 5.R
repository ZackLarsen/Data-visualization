head(EuStockMarkets)

# we want to add fields, so make a copy
ds = as.data.frame(EuStockMarkets)
ds$day = seq(nrow(ds))    # Add a sequential count of the records to indicate the "day"
# No, it is not totally correct, but in the absence of this information ...
library(reshape)

head(ds)

# Melt collapses columns into one field like the "pivot" feature in Tableau
# The id parameter here tells what fields to keep (i.e. not "melt")
EuStock = melt(ds, id=c("day"))
names(EuStock)[2] = "Index"
head(EuStock)


# mapping the thickness of a line to a value
# data from linegraphs.r
ggplot(ds, aes(day, DAX)) + geom_line(aes(size=DAX))
ggplot(ds, aes(day, DAX)) + geom_line(aes(size=CAC))
ggplot(ds, aes(day, DAX)) + geom_line(aes(colour=CAC))


#smoothing
ggplot(EuStock, aes(day,value)) + geom_line(aes(colour=Index))

ggplot(EuStock, aes(day,value)) + geom_line(aes(colour=Index)) +
  facet_wrap(~ Index)

apropos("facet_")



library(gcookbook) # for heightweight dataset
head(heightweight)
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn))

# with a linear model (lm) smooth, 95% conf interval shaded
sp + geom_point() + stat_smooth(method=lm)

# control the confident interval
sp + geom_point() + stat_smooth(method=lm, level=0.99)
sp + geom_point() + stat_smooth(method=lm, se=FALSE) # none

# emphasize the fit line with color
sp + geom_point(colour="blue") +
  stat_smooth(method=lm, se=FALSE, colour="green")


# non-linear smoothing (LOESS)
sp + geom_point(colour="grey60") + stat_smooth() # same as below
sp + geom_point(colour="grey60") + stat_smooth(method=loess)

# no confidence region, black line on grey
sp + geom_point(colour="grey60") +
  stat_smooth(colour="black", se=FALSE) # same as below

# map size to points on scatterplot
# we have age and height, so let's do weight to size of point
sp + geom_point(aes(size=weightLb))
# or color to age (not much point)
sp + geom_point(aes(size=weightLb, colour=ageMonth))

# get the subset of data where the subject is female
femHW <- heightweight[heightweight$sex == "f" ,]#The extra comma specifies all columns
spfem <- ggplot(heightweight, aes(x=ageYear, y=heightIn))
spfem + geom_point(colour="grey60") +
  stat_smooth(colour="black", se=FALSE) # same as below


# logscale (look in R Graphics Cookbook for more detail)
library(MASS)
p <- ggplot(Animals, aes(x=body, y=brain, label=rownames(Animals))) +
  geom_text(size=3)
p
p + scale_x_log10() + scale_y_log10()

# Remember there is some date manipulate stuff in the examples
# from before.  Take a look at the lubridate library.
# Get started with polar stuff from the R Graphics Cookbook also
