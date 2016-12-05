#PCA plot
library(tidyverse)
library(stringr)
options(stringAsFactors = F)
argv <- commandArgs(T)
PCA_data_path <- argv[1]
PCA_data <- read.delim(str_c("test_data",PCA_data_path,sep = "/"),header = T)
PCA_data <- t(apply(merge_data[,4:dim(merge_data)[2]],2,as.numeric))
PCA <- prcomp(PCA_data) #no scale
Summary_PCA <- summary(PCA)
sample_name <- c('L1','L2','L3','B1','B2','B3')
PCA_data <- as.data.frame(PCA$x[,1:length(sample_name)])
rownames(PCA_data) <- sample_name
PCA_data$Repeat <- substr(sample_name,1,2)
PCA_data$Group <- rep(c("L","B"),each = 3)
p <- ggplot(PCA_data,aes(PC1,PC2))+
  geom_point(aes(colour = Group),size = rel(3.0))+
  geom_text(aes(label=Repeat),vjust=0,hjust=0.5,color = "grey60")+
  geom_vline(xintercept = 0,linetype = 2,colour = "grey60",size = rel(0.5))+
  geom_hline(yintercept = 0,linetype = 2,colour = "grey60",size =rel(0.5))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())+
  labs(title = "PCA",
       x = paste0("PC1: ",Summary_PCA$importance[2,1]*100,"% variance"),
       y = paste0("PC2: ",Summary_PCA$importance[2,2]*100,"% variance")) 
ggsave(filename = "PCA.pdf",plot = p,width = 8,height = 6)