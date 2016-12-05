#2016-12-05
#this is a R script to plot heatmap 
#pheatmap is the main plot package,RColorBrewer and wesanderson is color schemes
# need_packages <- c("pheatmap","RColorBrewer","wesanderson")
# sapply(need_packages,function(x){if(!require(x)) install.packages(x)})
library(pheatmap)
library(RColorBrewer)
library(wesanderson)
library(stringr)
argv <- commandArgs(T)
options(stringsAsFactors = F)
heatmap_data_path <- argv[1]
heatmap_data_path <- str_c(test_data,"test_heatmap_data.100.txt",sep = "/")
heatmap_data <- read.delim(heatmap_data_path,header = T)
heatmap_data <- apply(heatmap_data,2,as.numeric)
#annotation_col
annotation_col = data.frame(
  group = factor(rep(c("group1", "group2"), c(3,3)))
)
ann_colors <- list(
  group = c(group1 = "#E41A1C",group2 ="#377EB8")
)
rownames(annotation_col) <- colnames(heatmap_data)
my_heatmap_color <- wes_palette("Zissou", 100, type = "continuous")
pdf(file = "test_heatmap.pdf",width = 12,height = 12,onefile = T)
pheatmap(mat = heatmap_data,scale = "column",show_rownames = F,border_color = "grey",
         annotation_col = annotation_col,
         annotation_colors  = ann_colors,
         annotation_names_col = F,
         treeheight_row = 0)
dev.off()
