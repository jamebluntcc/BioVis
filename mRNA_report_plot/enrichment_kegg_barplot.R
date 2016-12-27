#2016-10-20
#for mRNA report enrichment KEGG/GO  barplot
#input KEGG dir
#2016-10-28 changed
##
library(tidyverse)
library(cowplot)
library(ggthemes)
library(dplyr)
library(cowplot)
library(gridExtra)
options(stringsAsFactors = F)
argv <- commandArgs(T)
#list KEGG dir
KEGG.all <- read.delim('gga.all.gene.blasttab',header = T)
term_count <- unique(KEGG.all[,1]) %>% length()
#term_count <- KEGG.all %>% select(X1) %>% distinct() %>% count()
KEGG.subset_up <- read.delim("N42_vs_N7.N42-UP.subset.txt",header = T)
KEGG.subset_down <- read.delim("N42_vs_N7.N7-UP.subset.txt",header = T)
KEGG.up_count <- unique(KEGG.subset_up$id) %>% length()
KEGG.down_count <- unique(KEGG.subset_down$id) %>% length()
#----
kegg_table_path <- 'C:/Users/Administrator/Desktop/R/project/2016-10-19/Analysis_results/enrichment_analysis/KEGG/KEGG_enrich_table'
KEGG_dir <- list.dirs(kegg_table_path)[-1]
#----
kegg_data <- function(data,type,color,bar_num=15,KEGG_count){
  KEGG_data <- data[,c(1,4,5,7)]
  names(KEGG_data) <- c('Term','Input_number','Background_number','P_value')
  KEGG_data <- KEGG_data[KEGG_data$P_value < 0.05,]
  if(dim(KEGG_data)[1] > bar_num){
    KEGG_data <- KEGG_data[1:bar_num,]
  }
  for(i in 1:dim(KEGG_data)[1]){
    if(nchar(KEGG_data$Term[i]) > 15){
      KEGG_data$Term[i] <- paste0(substr(KEGG_data$Term[i],1,15),'...')
    }
  }
  #KEGG_data$Corrected.P.Value <- -log10(KEGG_data$Corrected.P.Value)
  KEGG_data$type <- type
  KEGG_data$color <- color
  KEGG_data$expected <- KEGG_data$Background_number / term_count * KEGG_count
  KEGG_data <- KEGG_data %>% dplyr::filter(Input_number > expected)
  if(dim(KEGG_data)[1] == 0){
    stop('KEGG data empty!')
  }
  KEGG_data
}

KEGG_barplot <- function(KEGG_merge_data){
  p <- ggplot(KEGG_merge_data,aes(y=Input_number,x=Term,fill=type))+
    geom_bar(stat = 'identity')+geom_bar(aes(x=Term,y=expected),fill='black',stat = 'identity')+
    geom_text(aes(label='P<0.05'),angle=90,vjust=0.5,hjust=0)+
    ylim(c(0,max(KEGG_merge_data$Input_number)+max(KEGG_merge_data$Input_number)*0.1)+
    scale_fill_manual(values = c('up'='red','down'='blue','expected'='black'),
                      breaks = c('expected','up','down'),labels = c('expected','up','down'))+
    guides(fill = guide_legend(title = ""))+ylab("Input.number")+xlab("Term")+
    facet_grid(.~color,scales = "free",space = "free")+
    theme_bw()+theme(legend.key = element_blank(),
                     axis.text.x = element_text(angle = -90))
  p
}
#----
kegg_plot <- list()
for(i in 1:length(KEGG_dir)){
  sample_files <- list.files(KEGG_dir[i])
  KEGG_up_data <- read.delim(paste(KEGG_dir[i],sample_files[2],sep = "/"),header = T)
  KEGG_down_data <- read.delim(paste(KEGG_dir[i],sample_files[3],sep = "/"),header = T)
  KEGG_up_data <- kegg_data(KEGG_up_data,type = 'up',color = "Up",bar_num = 15,KEGG_count = KEGG.up_count)
  KEGG_down_data <- kegg_data(KEGG_down_data,type = 'down',color = "Down",bar_num = 15,KEGG_count = KEGG.down_count)
  KEGG_merge_data <- rbind(KEGG_up_data,KEGG_down_data)
  KEGG_merge_data_by <- KEGG_merge_data %>% group_by(type) %>% arrange(Input_number)
  KEGG_merge_data_by$Term <- factor(KEGG_merge_data_by$Term,levels = KEGG_merge_data_by$Term)
  KEGG_merge_data_by[dim(KEGG_merge_data_by)[1]+1,] <- KEGG_merge_data_by[dim(KEGG_merge_data_by)[1],]
  KEGG_merge_data_by[dim(KEGG_merge_data_by)[1],c('Input_number','expected')] <- 0
  KEGG_merge_data_by[dim(KEGG_merge_data_by)[1],"type"] <- 'expected'
  kegg_barplot <- KEGG_barplot(KEGG_merge_data_by)
  kegg_plot[[i]] <- kegg_barplot
  ggsave(filename = paste(unlist(strsplit(KEGG_dir[i],split = './'))[2],'KEGG_barplot.png',sep = '_'),plot = kegg_barplot,width = 6,height = 8)
  ggsave(filename = paste(unlist(strsplit(KEGG_dir[i],split = './'))[2],'KEGG_barplot.pdf',sep = '_'),plot = kegg_barplot,width = 6,height = 8)
  KEGG_merge_plot <- marrangeGrob(kegg_plot,ncol = 2,nrow = 1)
  ggsave(filename = 'KEGG_merge_barplot.png',plot = KEGG_merge_plot,width = 10,height = 10)
}



