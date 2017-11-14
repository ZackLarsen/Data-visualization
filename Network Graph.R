setwd("E:/DePaul/CSC 465/Project")
setwd("~/Desktop/Datasets")
install.packages("igraph")
install.packages("network") 
install.packages("sna")
install.packages("ndtv")
install.packages("RColorBrewer")
library(ggplot2)
library(igraph)
library(network)
library(sna)
library(ndtv)
library(RColorBrewer)
display.brewer.all()

#Load in the data containing the network info
nodes <- read.csv("Divvy_Trips_2015_Q4.csv", header=T, as.is=T)
#links <- read.csv("", header=T, as.is=T)
head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

#Aggregate the dataframe so that non-unique trips are weighted where the weight is mapped to the thickness
#of the links
links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL


#Turn dataframe into an igraph network object
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
class(net)

#Plot the igraph network object
plot(net, edge.arrow.size=.4,vertex.label=NA)
#Remove loops
net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net, edge.arrow.size=.4,vertex.label=NA)

# Plot with curved edges (edge.curved=.1) and reduce arrow size:
plot(net, edge.arrow.size=.4, edge.curved=.1)

# Set edge color to light gray, the node & border color to orange
# Replace the vertex label with the node names stored in "media"
plot(net, edge.arrow.size=.2, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(net)$media, vertex.label.color="black")



# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]


# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")

plot(net)



#Plot a circle graph with the net igraph object from above
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
V(net)$size <- 8
V(net)$frame.color <- "white"
V(net)$color <- "orange"
V(net)$label <- ""       #Here is where we would specify that the label for each node should be the station name
E(net)$arrow.mode <- 0
plot(net)
l <- layout_in_circle(net)
plot(net, layout=l)




#Plot a circle network graph
net.bg <- sample_pa(80) 
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- ""       #Here is where we would specify that the label for each node should be the station name
E(net.bg)$arrow.mode <- 0
plot(net.bg)

l <- layout_in_circle(net.bg)
plot(net.bg, layout=l)



###################################################################################
#Plot a circle network graph just for milwaukee and wood
milwood <- subset(divvyq4,divvyq4$from_station_name=="Wood St & Milwaukee Ave")
milwood

#Turn the divvy dataframe into an igraph network object
d1 <- data.frame(from=milwood$from_station_name,to=milwood$to_station_name)
v1 <- data.frame(name=milwood$to_station_name,id=milwood$to_station_id)
net2 <- graph_from_data_frame(d=d1, vertices=v1, directed=T) 
class(net2)

#Plot the circle network graph
l2 <- layout_in_circle(net2)
plot(net2,layout=l2)
###################################################################################





###################################################################################
#Plot an arc diagram of the 20 most popular trips


###################################################################################



#Heatmap
netm <- get.adjacency(net, attr="weight", sparse=F)
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media

palf <- colorRampPalette(c("gold", "dark orange")) 
heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(100), 
        scale="none", margins=c(10,10) )




#Histogram
divvyq4 <- read.csv("Divvy_Trips_2015_Q4.csv", header=T, as.is=T)
head(divvyq4)
trips <- aggregate(divvyq4[,7], divvyq4[,-7], sum)
trips <- trips[order(trips$from_station_name, trips$to_station_name),]
colnames(trips)[4] <- "weight"
rownames(trips) <- NULL


ggplot(trips, aes(x=reorder(trips$from_station_id,-trips$weight))) +
  geom_histogram(binwidth=.5, colour="black", fill="white")















#Using different dataset
nodes2 <- read.csv("Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)

#Examine the data:
head(nodes2)
head(links2)

#We can see that links2 is an adjacency matrix for a two-mode network:
links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)