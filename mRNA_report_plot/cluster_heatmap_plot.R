# 2016-9-9 
#plot:cluster and heatmap plots
#
#
###
library(ggplot2)
library(pheatmap)
library(gridExtra)
library(reshape2)
#----
argv <- commandArgs(T)
file_path <- argv[1]
output_path <- argv[2]
#----
my_theme <- theme_calc()+theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 90)
)
theme_set(my_theme)

#setwd("C:\\Users\\Administrator\\Desktop\\R\\project\\2016-9-9\\fig_cluser\\seed")
data <- list()
all_files <- dir()
files <- all_files[grep('subcluster',all_files)]
for(i in 1:length(files)){
  data[[i]] <- read.delim(file = files[i],header = T,sep = '\t')
}
data_df <- list()
for(i in 1:length(data)){
  data_df[[i]] <- as.data.frame(cbind(rownames(t(data[[i]])),
                          t(data[[i]])),row.names = 1:dim(data[[i]])[2],
                          stringsAsFactors = F)
  data_df[[i]][,2:dim(data_df[[i]])[2]] <- apply(data_df[[i]][,2:dim(data_df[[i]])[2]],2,as.numeric)
  data_df[[i]]$mean <- apply(data_df[[i]][,2:dim(data_df[[i]])[2]],1,mean) %>% round(digits = 3)
  data_df[[i]]$num <- 1:dim(data_df[[i]])[1]
  names(data_df[[i]]) <- c('name',colnames(t(data[[i]])),'mean','num')
}
# for line plot
plot_line <- function(data){
  melt_df <- melt(data,id = c('name','num'))
  melt_df$color <- ifelse(melt_df$variable == 'mean','blue','grey60')
  col <- melt_df$color;names(col) <- col
  gg <- ggplot(aes(x=reorder(name,num),y=value,group = variable,colour = color),data = melt_df)+
    geom_line()+geom_point()+expand_limits(y = c(0,1))+
    scale_y_continuous(breaks = seq(from = 0,to = 1,by = 0.2),
                       labels = seq(from = 0,to = 1,by = 0.2))+
    scale_color_manual(values = col)+
    #theme(axis.title = element_text(size = rel(0.5)))+
    xlab("")+ylab("")+guides(color = F)
  gg
}
#for heatmap
mat_data <- NULL
for(i in data){
  mat_data <- rbind(mat_data,i)
}
mat_data$name <- row.names(mat_data)
data <- scale(mat_data[,-dim(mat_data)[2]])
ord <- hclust(dist(data))$order
pd <- as.data.frame(data)
pd$name <- rownames(mat_data)
pd.m <- melt(pd,id = 'name')
pd.m$name <- factor(pd.m$name,levels = rownames(mat_data)[ord],labels = rownames(mat_data)[ord])
pd.m$variable <- factor(pd.m$variable,levels = colnames(mat_data),labels = colnames(mat_data))

heatmap <- ggplot(pd.m,aes(variable,name))+geom_tile(aes(fill = value),width = 1,colour = 'grey60')+
  scale_fill_gradientn(colors = colorRampPalette(c("#377EB8","white","#E41A1C"))(50))+xlab("")+
  ylab("")+guides(fill = F)+
  theme(panel.border = element_blank(),
        axis.ticks = element_blank())
        #axis.title = element_text(size = rel(0.8)))

pdf(file = paste(output_path,'fig_cluster_seed.pdf',sep = '/'),width = 14,height = 15)
grid.arrange(heatmap,plot_line(data_df[[1]]),plot_line(data_df[[2]]),plot_line(data_df[[3]]),
             plot_line(data_df[[4]]),plot_line(data_df[[5]]),ncol = 3,
             layout_matrix = cbind(c(1,1,4),c(1,1,5),c(2,3,6)))
dev.off()










