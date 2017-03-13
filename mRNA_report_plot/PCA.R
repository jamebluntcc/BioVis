#PCA plot
#changed on 2017-3-10
library(data.table)
library(tidyverse,quietly = T)
library(stringr,quietly = T)
library(argparser,quietly = T)
options(stringAsFactors = F)
#-----pca_theme-----
theme_onmath <- function(base_size = 14){
  theme_bw()+
    theme(panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = 'black'),
          axis.text = element_text(color = 'black',face = 'bold'),
          axis.title = element_text(face = 'bold',size = base_size),
          axis.title.x = element_text(vjust = -0.2),
          axis.title.y = element_text(angle=90,vjust =2),
          plot.title = element_text(face = "bold",size = rel(1.2), hjust = 0.5),
          legend.key = element_blank(),
          legend.margin = unit(0,'cm'),
          legend.title = element_text(face = 'italic'),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold")
    )
}
#color and fill
scale_color_onmath <- function(...){
  library(scales)
  discrete_scale("colour","onmath",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...) 
}
scale_fill_onmath <- function(...){
  library(scales)
  discrete_scale("colour","onmath",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...) 
}
p <- argparser::arg_parser('this is a R script to plot PCA')
p <- add_argument(p,'file_path',help = 'pca file path',type = "character")
p <- add_argument(p,'group_sample',help = 'pca group information',type = "character")
p <- add_argument(p,'out_path',help = 'putout pca plot path',type = "character")
argv <- parse_args(p)
#file_path <- '/media/zxchen/3dddd6c4-2700-41a5-a677-15b165fa4e64/project/mRNA/2017/OM-mRNA-7-Rice-P20170305/pca_test'
# PCA_data <- fread(paste(file_path,'genes.TMM.EXPR.matrix',sep = '/'),header = T)
# group_sample <- fread(paste(file_path,'group_sample',sep = '/'),header = F)
PCA_data <- fread(argv$file_path,header = T)
group_sample <- fread(argv$group_sample,header = F)
PCA_data_mat <- t(apply(PCA_data[,2:dim(PCA_data)[2],with=F],2,as.numeric))
PCA <- prcomp(PCA_data_mat) #no scale
Summary_PCA <- summary(PCA)
PCA_data <- as.data.frame(PCA$x[,1:dim(group_sample)[1]])
sample_name <- rownames(PCA$x)
match_index <- match(sample_name,group_sample$V2,nomatch = 0)
group_name <- group_sample$V1[match_index]
PCA_data$Sample <- sample_name 
PCA_data$Group <- group_name
PCA_plot <- function(PCA_data){
  p <- ggplot(PCA_data,aes(PC1,PC2))+
    geom_point(aes(colour = Group),size = rel(3.0))+
    geom_text(aes(label=Sample),vjust=0,hjust=0.5,color = "black",size = rel(2.0))+
    geom_vline(xintercept = 0,linetype = 2,colour = "grey60",size = rel(0.5))+
    geom_hline(yintercept = 0,linetype = 2,colour = "grey60",size =rel(0.5))+
    theme_onmath()+scale_color_onmath()+
    labs(title = "PCA",
         x = paste0("PC1: ",Summary_PCA$importance[2,1]*100,"% variance"),
         y = paste0("PC2: ",Summary_PCA$importance[2,2]*100,"% variance"))
  p
}
ggsave(filename = paste(argv$out_path,"PCA.pdf",sep = "/"),plot = PCA_plot(PCA_data),width = 8,height = 6)
ggsave(filename = paste(argv$out_path,"PCA.png",sep = "/"),plot = PCA_plot(PCA_data),width = 8,height = 6,type='cairo',dpi = 300)
