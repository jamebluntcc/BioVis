#wgcna plot
library(network)
library(sna)
library(intergraph)
library(igraph)
library(data.table)
library(dplyr)
options(stringsAsFactors = F)
argv <- commandArgs(T)
edges_data <- argv[1]
node_data <- argv[2]
wgcna_edges <- read.delim(edges_data,header = T)
wgcna_nodes <- read.delim(node_data,header = T)
wgcna_edges <- wgcna_edges[1:3]
wgcna_nodes <- wgcna_nodes[c(1,3)]
names(wgcna_nodes)[2] <- c("nodeColor")
if(length(unique(wgcna_nodes$nodeColor)) <= 1){
  stop("wgcna node color error")
}
#--- network function----
net_work <- function(plotcord,social_mat,net,node.data,XD = 3.5){
  plotcord$name <- colnames(social_mat)
  names(node.data) <- c("nodeName","color")
  node.data$color <- as.character(node.data$color)
  plotcord$color <- node.data$color[match(plotcord$name,node.data$nodeName,nomatch = 0)]
  edglist <- as.matrix.network.edgelist(net)
  edge_plot <- data.frame(plotcord[edglist[,1],c(1,2)], plotcord[edglist[,2],c(1,2)])
  colnames(edge_plot) <-  c("X1","Y1","X2","Y2")
  gnet_edge <- ggplot(data=edge_plot)  + 
    geom_segment(aes(x=X1*XD, y=Y1*XD, xend = X2*XD, yend = Y2*XD), 
                 size = .4,colour="grey",alpha = .4)+
    xlab("")+ylab("")+scale_x_continuous(breaks = NULL)+
    scale_y_continuous(breaks = NULL)
  values <- plotcord$color
  names(values) <- values
  gnet_point <- gnet_edge +
    geom_point(aes(X1*XD, X2*XD,colour = color), 
               data=plotcord,alpha = .7)+scale_color_manual(values = values)
  return(gnet_point)
}
#---net work theme----
net_theme <- theme_bw()+theme(
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)
theme_set(net_theme)
# #for test
# test_edges <- sample_n(wgcna_edges,10000)
# test_nodes <- wgcna_nodes[wgcna_nodes$nodeName %in% c(test_edges$fromNode,test_edges$toNode),]
graph_data <- simplify(graph.data.frame(wgcna_edges,directed = F))
net <- asNetwork(graph_data)
m <- as.matrix.network.adjacency(net) 
plotcord <- data.frame(gplot.layout.fruchtermanreingold(m,NULL))
#---plot out---
my_network <- net_work(plotcord,social_mat = m,net = net,node.data = test_nodes)
ggsave(filename = "wgcna_network.png",plot = my_network,width = 8,height = 6,dpi = 300,type = "cairo")
ggsave(filename = "wgcna_network.pdf",plot = my_network,width = 8,height = 6,dpi = 300)
