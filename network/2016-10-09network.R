#2016-10-10
#net work
##########
argv <- commandArgs(T)
options(stringsAsFactors = F)
#load packages
library(ggplot2)
library(sna)
library(intergraph)
library(igraph)
library(RColorBrewer)
library(data.table) 
library(dplyr)
#--- network function----
net_work <- function(plotcord,net,node.data,XD = 3.5){
  #add columns
  plotcord$degree <- igraph::degree(graph_data)
  plotcord$name <- colnames(social_mat)
  plotcord$color <- node.data$color[match(plotcord$name,node.data$nodeName,nomatch = 0)]
  edglist <- as.matrix.network.edgelist(net)
  edge_plot <- data.frame(plotcord[edglist[,1],c(1,2)], plotcord[edglist[,2],c(1,2)])
  colnames(edge_plot) <-  c("X1","Y1","X2","Y2")
  #---
  gnet_edge <- ggplot(data=edge_plot)  + 
    geom_segment(aes(x=X1*XD, y=Y1*XD, xend = X2*XD, yend = Y2*XD), 
                 size = .4,colour="grey",alpha = .4)+
    xlab("")+ylab("")+scale_x_continuous(breaks = NULL)+
    scale_y_continuous(breaks = NULL)
  #---
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
#read data
edge_data <- fread(argv[1])
node_data <- fread(argv[2])
#for a test:
test_data_edge <- dplyr::sample_n(edge_data,100000)
graph_data <- graph.data.frame(test_data_edge[,1:2,with = F],directed = F)
graph_data <- simplify(graph_data)  #dorp the multigraph
#由ig 到 network
net <- asNetwork(graph_data)
# get sociomatrix
m <- as.matrix.network.adjacency(net) # get sociomatrix

layout_par <- list(
  niter = 1000,
  cell.jitter = 0.001,  #越小越好
  cell.pointpointrad = 80,
  cell.pointcellrad = 100
)
plotcord <- data.frame(gplot.layout.fruchtermanreingold(m,layout_par)) 
network_plot <- net_work(plotcord,net,node_data)
#find RPW8_data
# RPW8_data <- data.frame(X1 = 107.6027,Y1 = -408.8046,
#                         X2 = 107.6027+100,Y2 = 100+-408.8046,label = "RPW8")
# 
# gg <- gg + geom_text(aes(X2*3.5, Y2*3.5,label = label),data = RPW8_data)+
#   geom_segment(aes(x=X1*3.5, y=Y1*3.5, xend = X2*3.5, yend = Y2*3.5), 
#                size = .4,colour="black",data = RPW8_data)
#---- change legend----
network_plot <- network_plot + theme(legend.position = c(0.45,0.05),
                            legend.key = element_blank(),
                            legend.background = element_blank(),
                            legend.direction = "horizontal",
                            legend.justification = "right",
                            legend.text = element_text(face = "italic",size = rel(0.5)),
                            legend.title = element_text(face = "italic",size = rel(0.8))
)

ggsave(filename = "net_plot_adj.png",plot = network_plot,width = 8,height = 6)
ggsave(filename = "net_plot.pdf",plot = network_plot,width = 8,height = 6)

