# plotting densities in 2D  (mostly from R Graphics Cookbook)

# using the eruptions data from Old Faithful
head(faithful)

# remember we did 1d before:
ggplot(diamonds, aes(depth, fill = cut, colour = cut)) + geom_density(alpha = 0.1)

# let's look at these distributions
ggplot(faithful, aes(eruptions)) + geom_density()
ggplot(faithful, aes(waiting)) + geom_density()
ggplot(faithful, aes(waiting)) + geom_density() + geom_density(aes(eruptions))

# The base plot
p <- ggplot(faithful, aes(x=eruptions, y=waiting))
p + geom_point() + stat_density2d()
# can map the 'height' also, with ..level..
p + stat_density2d(aes(colour=..level..))
# Map density estimate to fill color
p + stat_density2d(aes(fill=..density..), geom="raster", contour=FALSE)
# With points, and map density estimate to alpha
p + geom_point() +
  stat_density2d(aes(alpha=..density..), geom="tile", contour=FALSE)

# geom raster vs geom tile are just different ways of rendering it.
# geom raster should be more efficient.  It's good to know both
# because sometimes one or the other will render funny in PDF

# the h argument allows you to control the fineness of the grid
p + stat_density2d(aes(fill=..density..), geom="raster",
                   contour=FALSE, h=c(.5,5))

# want a much less dense sort of thing?  Here's the binned version  
p + stat_bin2d(aes(fill=..density..))
