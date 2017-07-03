#2016-12-05
#this is a R script to plot heatmap 
#load package
library(stringr)
library(pheatmap)
library(tidyverse)
library(RColorBrewer)
#argv <- commandArgs(T)
#options(stringsAsFactors = F)
#heatmap_data_path <- argv[1]
heatmap_data_path <- str_c("../data","test_heatmap_data.100.txt",sep = "/")
heatmap_data <- read_delim(heatmap_data_path,col_names = T,delim = "\t")
# set annotation_col
annotation_col = data.frame(
  group = factor(rep(c("group1", "group2"),each=3))
)
ann_colors <- list(
  group = c(group1 = "#E41A1C",group2 ="#377EB8")
)
rownames(annotation_col) <- colnames(heatmap_data) #mapping data
#print as png&pdf
png(file = "pheatmap_demo.png",width = 800,height = 800)
pheatmap(mat = heatmap_data,scale = "column",show_rownames = F,border_color = NA,
         annotation_col = annotation_col,
         annotation_colors  = ann_colors,
         annotation_names_col = F,
         treeheight_row = 0)
dev.off()
pdf(file = "pheatmap_demo.pdf",width = 12,height = 12,onefile = T)
pheatmap(mat = heatmap_data,scale = "column",show_rownames = F,border_color = NA,
         annotation_col = annotation_col,
         annotation_colors  = ann_colors,
         annotation_names_col = F,
         treeheight_row = 0)
dev.off()