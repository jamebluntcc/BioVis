#PCA plot
#changed on 2017-3-10
library(tidyverse,quietly = T)
library(stringr,quietly = T)
library(argparser,quietly = T)
options(stringAsFactors = F)
#-----pca_theme-----
pca_theme  <- function(base_size=14) {
  require('grid',quietly = T)
  require('ggthemes',quietly = T)
  (theme_foundation(base_size=base_size)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.key.size= unit(0.5, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
}
p <- argparser::arg_parser('this is a R script to plot PCA')
p <- add_argument(p,'file_path',help = 'pca file path',type = "character")
p <- add_argument(p,'group_sample',help = 'pca group information',type = "character")
p <- add_argument(p,'out_path',help = 'putout pca plot path',type = "character")
argv <- parse_args(p)

PCA_data <- read.delim(argv$file_path,header = T)
group_sample <- read.delim(argv$group_sample,header = F)
outpath <- argv$out_path
PCA_data_mat <- t(apply(PCA_data[,2:dim(PCA_data)[2]],2,as.numeric))
PCA <- prcomp(PCA_data_mat) #no scale
Summary_PCA <- summary(PCA)
group_name <- group_sample$V1
sample_name <- group_sample$V2
PCA_data <- as.data.frame(PCA$x[,1:length(sample_name)])
rownames(PCA_data) <- sample_name
PCA_data$Sample <- sample_name 
PCA_data$Group <- group_name
PCA_plot <- function(PCA_data){
  p <- ggplot(PCA_data,aes(PC1,PC2))+
    geom_point(aes(colour = Group),size = rel(3.0))+
    geom_text(aes(label=Sample),vjust=0,hjust=0.5,color = "black",size = rel(2.0))+
    geom_vline(xintercept = 0,linetype = 2,colour = "grey60",size = rel(0.5))+
    geom_hline(yintercept = 0,linetype = 2,colour = "grey60",size =rel(0.5))+
    pca_theme()+
    labs(title = "PCA",
         x = paste0("PC1: ",Summary_PCA$importance[2,1]*100,"% variance"),
         y = paste0("PC2: ",Summary_PCA$importance[2,2]*100,"% variance"))
  p
}
ggsave(filename = paste(outpath,"PCA.pdf",sep = "/"),plot = PCA_plot(PCA_data),width = 8,height = 6)
ggsave(filename = paste(outpath,"PCA.png",sep = "/"),plot = PCA_plot(PCA_data),width = 8,height = 6,type='cairo',dpi = 300)
