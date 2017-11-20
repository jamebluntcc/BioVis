# write by chencheng 
# on 2017-11-08 
# snp index plot

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(gridExtra)
library(RColorBrewer)

# argvs
argv <- commandArgs(TRUE)
bed_out_file_path = argv[1]
output_path = argv[2]
path_c = str_split(bed_out_file_path, pattern = '/')[[1]]
filename = str_split(path_c[length(path_c)], pattern = '\\.')[[1]][1]
GroupA = str_split(filename, pattern = '_')[[1]][1]
GroupB = str_split(filename, pattern = '_')[[1]][2]
# set theme
my_theme <- theme_bw()+theme(
  panel.border = element_rect(colour = 'black'),
  strip.background = element_blank(),
  axis.title = element_text(face = 'bold',color='black'),
  axis.text = element_text(color = 'black'),
  plot.title = element_text(hjust = 0.5)
)
theme_set(my_theme)

# feature functions 
tidy_sliderWindow_data <- function(data,header){
  names(data) <- paste0('X',1:dim(data)[2])
  data <- mutate(na.omit(data),X9=(X6)/(X5+X6),X10=X8/(X7+X8))
  data <- dplyr::filter(data,X4 > 10)
  data <- data[,c(1,2,9,10)]
  if(length(header) != 4){
    stop('header length must be 4!')
  }else{
    names(data) <- header
  }
  return(data)
}

merge_plot <- function(data,filename,
                       col=brewer.pal(3,'Set2'),
                       break_seq,
                       label_seq,
                       labels){
  names(data) <- c('chr','pos','V1','V2','V3')
  # data$chr <- paste0('chr',data$chr)
  chr_set <- unique(data$chr)
  for(i in chr_set){
    up_p <- ggplot(data[which(data$chr==i),],aes(x=pos,y=V1))+
      geom_point(color=col[1],size=rel(0.8))+
      annotate('text',x=1.2e8,y=0.85,label=labels[1],size=6)+
      geom_smooth(se=F,color='red')+
      xlab("Postion")+ylab("SNP Index")+
      scale_x_continuous(breaks = break_seq,
                         labels = paste0(label_seq,'M'))+
      scale_y_continuous(breaks = seq(from=0,to=1,by=0.2),
                         labels = seq(from=0,to=1,by=0.2))+
      guides(color=guide_legend(title = ""))+ggtitle(label = i)
    md_p <- ggplot(data[which(data$chr==i),],aes(x=pos,y=V2))+
      geom_point(color=col[2],size=rel(0.8))+
      annotate('text',x=1.2e8,y=0.85,label=labels[2],size=6)+
      geom_smooth(se=F,color='red')+
      xlab("Postion")+ylab("SNP Index")+
      scale_x_continuous(breaks = break_seq,
                         labels = paste0(label_seq,'M'))+
      scale_y_continuous(breaks = seq(from=0,to=1,by=0.2),
                         labels = seq(from=0,to=1,by=0.2))+
      guides(color=guide_legend(title = ""))
    do_p <- ggplot(data[which(data$chr==i),],aes(x=pos,y=V3))+
      geom_point(color=col[3],size=rel(0.8))+
      geom_smooth(se=F,color='red')+
      annotate('text',x=1.2e8,y=0.85,label="paste(Delta,snp-index)",size=6,parse=T)+
      xlab("Postion")+ylab(expression(paste(Delta,'snp-index')))+
      scale_x_continuous(breaks = break_seq,
                         labels = paste0(label_seq,'M'))+
      scale_y_continuous(breaks = seq(from=-1,to=1,by=0.5),
                         labels = seq(from=-1,to=1,by=0.5))+
      guides(color=guide_legend(title = ""))
    merge_p <- grid.arrange(up_p, md_p, do_p, ncol=1, nrow =3)
    ggsave(filename = paste0(filename,i,'.pdf'),plot = merge_p,width = 8,height = 12)
    ggsave(filename = paste0(filename,i,'.png'),plot = merge_p,width = 8,height = 12, type='cairo')
  }
}

# main 
snp_sw_data <- read_delim(bed_out_file_path,delim = "\t",col_names = F)
tidy_sw_data <- tidy_sliderWindow_data(snp_sw_data,header = c("Chrom","Pos","groupA","groupB"))
tidy_sw_data <- dplyr::mutate(tidy_sw_data,DIFF=groupA-groupB)
merge_plot(tidy_sw_data,
           filename = paste(output_path, paste0(filename,"_"), sep = "/"),
           break_seq = seq(from=0,to=8e8,by=2e8),
           label_seq = seq(from=0,to=800,by=200),
           labels = c(GroupA,GroupB))
