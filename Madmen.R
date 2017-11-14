install.packages("gcookbook")
install.packages("igraph")

library(igraph)
library(gcookbook) # For the data set
madmen2
mm <- as.data.frame(madmen)
mm
#Create a graph object
g <- graph.data.frame(madmen2, directed=TRUE)

# Remove unnecessary margins
par(mar=c(0,0,0,0))
plot(g, layout=layout.fruchterman.reingold, vertex.size=8, edge.arrow.size=0.5,
     vertex.label=NA)

g <- graph.data.frame(madmen, directed=FALSE)
par(mar=c(0,0,0,0)) # Remove unnecessary margins
plot(g, layout=layout.circle, vertex.size=8, vertex.label=NA)

#####################################################################################
#Circle graph with data from milwood
milwood <- subset(divvyq4,divvyq4$from_station_name=='Wood St & Milwaukee Ave')
milwood
from_to <- data.frame(From=milwood$from_station_name,To=milwood$to_station_name)
head(from_to)
g3 <- graph.data.frame(from_to,directed=FALSE)
plot(g3, layout=layout.circle, vertex.size=8,
     vertex.label=NA,
     main="All destinations from Milwaukee and Wood")

# Copy madmen and drop 19 rows out of 20
m4 <- from_to[1:nrow(from_to) %% 20 == 1, ]
g4 <- graph.data.frame(m4, directed=FALSE)


#Circle with labels
plot(g4, layout=layout.circle, vertex.size=4,
     vertex.shape="square",
     vertex.label=V(g4)$From,main="Divvy Paths",
     vertex.label.cex=0.8,
     vertex.label.color="black")




#Plot images as the node shapes
install.packages("png")
library(png)
img.1 <- readPNG("Divvy-FB-blue.png")
img.2 <- readPNG("./images/user.png")
33
V(net2)$raster <- list(img.1, img.2)[V(net2)$type+1]
plot(net2, vertex.shape="raster", vertex.label=NA,
     vertex.size=16, vertex.size2=16, edge.width=2)
#####################################################################################




#Circle with labels
plot(g, layout=layout.circle, vertex.size=8, 
     vertex.label=V(g)$name,main="Madmen",
     vertex.label.cex=0.8,
     vertex.label.color="black")



# Copy madmen and drop every other row
m <- madmen[1:nrow(madmen) %% 2 == 1, ]
g <- graph.data.frame(m, directed=FALSE)
# Print out the names of each vertex
V(g)$name


plot(g, layout=layout.fruchterman.reingold,
     vertex.size = 4, # Smaller nodes
     vertex.label = V(g)$name, # Set the labels
     vertex.label.cex = 0.8, # Slightly smaller font
     vertex.label.dist = 0.4, # Offset the labels
     vertex.label.color = "black")

