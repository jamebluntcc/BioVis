#try netbiov plot
#first to use igraph
library(netbiov)
library(igraph)
library(sna)
library(intergraph)
library(tidyverse)
nodes <- read_delim("../data/Dataset1-Media-Example-NODES.csv",
                    delim = ",",
                    col_names = T)
nodes$X6 <- NULL
nodes$col[nodes$type.label == 'Newspaper'] <- 'red'
nodes$col[nodes$type.label == 'TV'] <- 'green'
nodes$col[nodes$type.label == 'Online'] <- 'yellow'
data("PPI_Athalina")
links <- read_csv("../data/Dataset1-Media-Example-EDGES.csv",col_names = T)
links <- aggregate(links[,3],links[,-3],sum)
links <- dplyr::arrange(links,from,to)
net <- graph.data.frame(d = links,vertices = nodes,directed = F)
kk <- mst.plot(g1,expression = rnorm(vcount(g1)),v.size = 1)
cols <- nodes$col
net_plot <- mst.plot(net,bg = 'white',v.lab = nodes$media,vertex.color = cols[1:16])
#for test
link_data <- read_delim("../data/CytoscapeInput-edges.txt",delim = "\t",col_names = T)
node_data <- read_delim("../data/CytoscapeInput-nodes.txt",delim = "\t",col_names = T)
link_data <- link_data[c(1,2,3)]
node_data <- node_data[c(1,3)]
names(node_data)[2] <- "nodecolor"
link_data_selected <- dplyr::filter(link_data,weight > 0.8)
graph_data <- graph.data.frame(link_data_selected[,c(1,2)],vertices = node_data[,1],directed = F)
graph_data <- simplify(graph_data)  #dorp the multigraph

pdf(file = 'netbioV_test.pdf',width = 8,height = 6)
mst.plot(graph_data)
dev.off()
opar <- par(no.readonly = T)
par(mfrow=c(1,2),mar=c(0,0,0,0))
plot(graph_data,vertex.label = NA,vertex.size = 3,layout=layout.fruchterman.reingold)
plot(graph_data,vertex.label = NA,vertex.size = 3,layout=layout.kamada.kawai)
par(opar)

par(mfrow=c(1,2),mar=c(0,0,0,0))
mst.plot.mod(graph_data,layout.function=layout.fruchterman.reingold)
mst.plot.mod(graph_data,layout.function=layout.kamada.kawai)
#load data
data("PPI_Athalina")
data("modules_PPI_Athalina")
# pdf('plot_abstract_module.pdf',width = 8,height = 6)
# id <- plot.abstract.module(g1,mod.list = lm,
#                            layout.function = layout.graphopt,
#                            color.random = T,
#                            node.lab = F,
#                            v.size = 1)
# dev.off()
par(mfrow=c(1,2),mar=c(0,0,0,0))
id.1 <- plot.abstract.module(g1,mod.list = lm,
                           layout.function = layout.graphopt,
                           color.random = T,
                           node.lab = F,
                           v.size = 1)
pdf('test_plot_abstract_module.pdf',width = 8,height = 6)
id.2 <- plot.abstract.module(graph_data,
                           layout.function = layout.graphopt,
                           color.random = T,
                           node.lab = F,
                           v.size = 1)
dev.off()

