#try netbiov plot
#first to use igraph
library(igraph)
library(tidyverse)
nodes <- read_delim("../data/Dataset1-Media-Example-NODES.csv",
                    delim = ",",
                    col_names = T)
nodes$X6 <- NULL
nodes$col[nodes$type.label == 'Newspaper'] <- 'red'
nodes$col[nodes$type.label == 'TV'] <- 'green'
nodes$col[nodes$type.label == 'Online'] <- 'yellow'

links <- read_csv("../data/Dataset1-Media-Example-EDGES.csv",col_names = T)
links <- aggregate(links[,3],links[,-3],sum)
links <- dplyr::arrange(links,from,to)
net <- graph.data.frame(d = links,vertices = nodes,directed = F)
kk <- mst.plot(g1,expression = rnorm(vcount(g1)),v.size = 1)
cols <- nodes$col
net_plot <- mst.plot(net,bg = 'white',v.lab = nodes$media,vertex.color = cols[1:16])
