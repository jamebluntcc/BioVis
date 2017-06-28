#-----------------
#bioFabric network
#-----------------
library(RBioFabric)

link_data <- read_delim("../data/CytoscapeInput-edges.txt",col_names = T,delim = "\t")
node_data <- read_delim("../data/CytoscapeInput-nodes.txt",col_names = T,delim = "\t")

wgcna_data <- list(wgcna_link = link_data,
                   wgcna_node = node_data)
#clean data
wgcna_clean <- function(wgcna_data,weight_data=0.5){
  names(wgcna_data$wgcna_link)[1:3] <- c('from','to','weight')
  wgcna_data$wgcna_link <- wgcna_data$wgcna_link %>% 
    select(from:weight) %>% filter(weight > weight_data)
  names(wgcna_data$wgcna_node)[c(1,3)] <- c('node','color')
  wgcna_data$wgcna_node <- wgcna_data$wgcna_node %>% 
    select(node,color)
  if(length(unique(wgcna_data$wgcna_node$color)) <= 1){
    stop('wgcna node color must be > 1')
  }
  return(wgcna_data)
}
graph_data <- wgcna_clean(wgcna_data)

height <- vcount(graph_data)
width <- ecount(graph_data)
aspect <- height / width;
plotWidth <- 100.0
plotHeight <- plotWidth * (aspect * 1.2)
pdf("myBioFabricOutput.pdf", width=plotWidth, height=plotHeight)
bioFabric(graph_data)
dev.off()