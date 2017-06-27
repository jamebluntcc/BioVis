#-------------
#wgcna network
#-------------
load_packages <- c('argparser','igraph','tidyverse','ggraph')
lapply(load_packages, 
       require, character.only=T)
p <- argparser::arg_parser('this is a R script to plot network')
p <- add_argument(p,'edges_data_path',help = 'wgcna edges file path',type = "character")
p <- add_argument(p,'nodes_data_path',help = 'wgcna nodes file path',type = "character")
p <- add_argument(p,'out_path',help = 'output network plot path',type = "character",default='./')
argv <- parse_args(p)

link_data <- read_delim(argv$edges_data_path,col_names = T,delim = "\t")
node_data <- read_delim(argv$nodes_data_path,col_names = T,delim = "\t")

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

test_data <- wgcna_clean(wgcna_data)
graph_data <- graph.data.frame(test_data$wgcna_link,test_data$wgcna_node,directed = F) %>% simplify()
#print as pdf&png
network <- ggraph(graph_data,layout = 'kk')+
  geom_edge_link()+
  geom_node_point(aes(color=color))

ggsave(filename = paste(argv$out_path,'wgcna_network.pdf',sep = "/"),plot = network,width = 8,height = 6)
ggsave(filename = paste(argv$out_path,'wgcna_network.pdf',sep = "/"),plot = network,width = 8,height = 6)
