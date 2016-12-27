#2016-10-13
#plot1 boxplot
#plot2 violin plot
#plot3 density plot
######
library(reshape2)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(gridExtra)
options(stringsAsFactors = F)
argv <- commandArgs(T)
Gene.tmp_file_path <- argv[1]
plot_output_path <- argv[2]
# Gene.tmp_file_path <- 'merge_plot_data.xls'
# plot_output_path <- ''
theme_set(theme_bw()+theme(axis.text.x = element_text(angle = -90,color = 'black',
                                                        vjust = 0.5,hjust = 0),
                           legend.key = element_blank()))

data <- read.delim(Gene.tmp_file_path,header = T)
data.m <- melt(data[,1:6],id = 'id')
calc_color <- colorRampPalette(calc_pal()(12))(length(unique(data.m$variable)))
data.m$value <- log10(data.m$value + 1)

boxplot <- ggplot(data.m,aes(x=variable,y=value,fill=variable))+
  geom_boxplot(notch = T)+guides(fill = F)+
  scale_fill_manual(values = calc_color)+xlab("")+ylab("")
violin <- ggplot(data.m,aes(x=variable,y=value,fill=variable))+
  geom_violin()+guides(fill = guide_legend(nrow = 8,title = 'sample'))+
  scale_fill_manual(values = calc_color)+xlab("")+ylab("")
density_plot <- ggplot(data.m,aes(value,fill = variable))+
  geom_density(alpha = .4)+
  scale_fill_manual(values = calc_color)+
  theme(axis.text.x = element_text(angle = 0))+
  guides(fill = guide_legend(nrow = 8,title = 'sample'))+xlab("")
#merge plot
ggsave(filename = paste(plot_output_path,'merge_plot.png',sep = ''),
       plot = grid.arrange(boxplot,violin,density_plot,nrow = 2,
             layout_matrix = rbind(c(1,2),c(3,3))),
       width = length(unique(data.m$variable)) * 2,height = 10)




