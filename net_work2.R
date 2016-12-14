library(igraph)
library(network)
library(sna)
library(RColorBrewer)
library(png)
library(tidyverse)
options(stringsAsFactors = F)
options(bitmapType='cairo')
#read data
wgcna_edge <- read.delim("CytoscapeInput-edges.txt",header = T)
wgcna_node <- read.delim("CytoscapeInput-nodes.txt",header = T)
wgcna_edge <- wgcna_edge[c(1,2,3)]
wgcna_node <- wgcna_node[c(1,3)]
names(wgcna_node)[2] <- c("nodeColor")
wgcna_edge_selected <- dplyr::filter(wgcna_edge,weight > 0.2)
net <- graph_from_data_frame(d=wgcna_edge,vertices=wgcna_node,directed=F) 
net <- simplify(net) 
png(file = 'network.png',width = 12,height = 6,units = "in",res = 300)
plot(net,layout=layout_with_fr,edge.curved=0.2,
     vertex.frame.color=NA,vertex.size=3,vertex.label=NA,
     vertex.color=sapply(wgcna_node$nodeColor,adjustcolor,0.8))
dev.off()
# you can put legend by fllow code
# legend(x=-1.2, y=-0.3, c("TpyeA","TpyeB", "TpyeC"), pch=19,
#        col=c("turquoise","grey","blue"),bty="n",ncol=1)
