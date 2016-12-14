# #2016-12-13 wgcna plot
#--------new way to wgcna------
#by ggnetwork
library(ggnetwork)
library(network)
library(sna)
library(intergraph)
library(igraph)
library(data.table)
library(dplyr)
options(stringsAsFactors = F)
wgcna_edge <- read.delim("CytoscapeInput-edges.txt",header = T)
wgcna_node <- read.delim("CytoscapeInput-nodes.txt",header = T)
wgcna_edge <- wgcna_edge[c(1,2,3)]
wgcna_node <- wgcna_node[c(1,3)]
names(wgcna_node)[2] <- c("nodeColor")
#wgcna_edge_selected <- dplyr::filter(wgcna_edge,weight > 0.2)
graph_data <- graph.data.frame(wgcna_edge[1:2],directed = F)
graph_data <- simplify(graph_data)  #dorp the multigraph
#由ig 到 network
net <- asNetwork(graph_data)
#net %v% "color" <- wgcna_node$nodeColor
#ggnetwork transform net class to df 
df <- ggnetwork(net,layout = "fruchtermanreingold")
df$color <- wgcna_node$nodeColor[match(df$vertex.names,wgcna_node$nodeName,nomatch = 0)]
write.table(df,"wgcna_data.txt",quote = F,row.names = F,sep = "\t")
net_color <- df$color
names(net_color) <- net_color
network <- ggplot(df,aes(x = x, y = y, xend = xend, yend = yend))+
  geom_edges(curvature = 0.1,size = 0.4,alpha = 0.4,color = "grey")+theme_blank()+
  geom_point(aes(x=x,y=y,colour=color),size = 0.8)+scale_color_manual(values = net_color)
ggsave(filename = "net_work.png",plot = network,width = 12,height = 8,type = "cairo",dpi = 300)

