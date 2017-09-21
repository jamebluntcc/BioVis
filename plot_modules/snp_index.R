library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(gridExtra)
library(RColorBrewer)
#-- define func--
deal_snp_cell <- function(cell,split = ','){
  if(nchar(cell) == 1){
    return(c(0,0))
  }else{
    temp_cell <- as.numeric(str_split(cell,pattern = split)[[1]])
    if(sum(temp_cell) == 0 || any(is.na(temp_cell))){
      return(c(0,0))
    }else if(length(temp_cell) == 2){
      return(temp_cell)
    }else{
      return(c(0,0))
    }
  }
}

tidy_snp_data <- function(data,fileName='snp_tidy.txt'){
  names(data) <- c('V1','V2','V3','V4')
  df_3 <- as.data.frame(t(sapply(data$V3,deal_snp_cell)))
  df_4 <- as.data.frame(t(sapply(data$V4,deal_snp_cell)))
  data$V5 <- df_3$V1
  data$V6 <- df_3$V2
  data$V7 <- df_4$V1
  data$V8 <- df_4$V2
  data <- dplyr::filter(data,(V6 / (V5+V6) >= 0.3 & (V5+V6) >= 5) |
                                   (V8 / (V7+V8) >= 0.3 & (V7+V8) >= 5))
  data <- data[,c(1,2,5,6,7,8)]
  write.table(data,fileName,quote = F,row.names = F,sep = "\t")
}

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

my_theme <- theme_bw()+theme(
  panel.border = element_rect(colour = 'black'),
  strip.background = element_blank(),
  axis.title = element_text(face = 'bold',color='black'),
  axis.text = element_text(color = 'black'),
  plot.title = element_text(hjust = 0.5)
)
theme_set(my_theme)

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
      annotate('text',x=1.2e8,y=0.85,label=labels[1],size=6,parse=T)+
      geom_smooth(se=F,color='red')+
      xlab("Postion")+ylab("SNP Index")+
      scale_x_continuous(breaks = break_seq,
                         labels = paste0(label_seq,'M'))+
      scale_y_continuous(breaks = seq(from=0,to=1,by=0.2),
                         labels = seq(from=0,to=1,by=0.2))+
      guides(color=guide_legend(title = ""))+ggtitle(label = i)
    md_p <- ggplot(data[which(data$chr==i),],aes(x=pos,y=V2))+
      geom_point(color=col[2],size=rel(0.8))+
      annotate('text',x=1.2e8,y=0.85,label=labels[2],size=6,parse=T)+
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
    ggsave(filename = paste0(filename,i,'.png'),plot = merge_p,width = 8,height = 12)
  }
}

plot_manhatton_diff_snp_index <- function(data,filename='snp_diff_manhatton',w=12,h=8){
  plot_data <- data
  plot_data$cumpos <- 0
  plot_data$col <- ''
  each_chr_max_pos <- plot_data %>% group_by(Chrom) %>% summarise(max_chr=max(Pos))
  chr_sets <- unique(plot_data$Chrom)
  snp_plot_col <- rep(brewer.pal(9,'Set1'),5)[1:length(chr_sets)]
  add_chr_num <- cumsum(each_chr_max_pos$max_chr)
  max_len <- ceiling(add_chr_num[length(add_chr_num)] * 1.2)
  for(i in 2:length(chr_sets)){
    plot_data$cumpos[which(plot_data$Chrom == chr_sets[i])] <- plot_data$Pos[which(plot_data$Chrom == chr_sets[i])]+add_chr_num[i-1]
  }
  plot_data$cumpos[which(plot_data$Chrom == chr_sets[1])] <- plot_data$Pos[which(plot_data$Chrom == chr_sets[1])]
  for(i in 1:length(chr_sets)){
    plot_data$col[which(plot_data$Chrom == chr_sets[i])] <- snp_plot_col[i]
  }
  names(snp_plot_col) <- snp_plot_col
  gg <- ggplot(plot_data,aes(x=cumpos,y=DIFF,color=col))+
    geom_point()+
    scale_x_continuous(breaks = seq(from=0,to=max_len,length.out = length(chr_sets)),
                       labels = chr_sets)+
    scale_color_manual(values = snp_plot_col)+
    guides(color=F)+xlab("Postion")+ylab(expression(paste(Delta,'snp-index')))+
    ylim(c(-1,1))+theme(panel.grid.minor = element_blank())
  ggsave(paste(filename,'png',sep = "."),plot = gg,width = w,height = h)
  ggsave(paste(filename,'pdf',sep = "."),plot = gg,width = w,height = h)
}
#-- read file and tidy--
snp_data <- read_delim('data/mRNA.ZYKY.hq.snp.ann.merge.table',col_names = T,delim = "\t")
snp_data_tidy <- dplyr::select(snp_data,c('Chrom','Pos','M','W'))
snp_data_tidy <- tidy_snp_data(snp_data_tidy)
snp_sw_data <- read_delim("snp_tidy.bed3.out",delim = "\t",col_names = F)
tidy_sw_data <- tidy_sliderWindow_data(snp_sw_data,header = c("Chrom","Pos","M","W"))
tidy_sw_data <- dplyr::mutate(tidy_sw_data,DIFF=M-W)

library(gridExtra)
merge_plot(tidy_sw_data,
           filename = "snp_MvsW_",
           break_seq = seq(from=0,to=8e8,by=2e8),
           label_seq = seq(from=0,to=800,by=200),
           labels = c("italic(M)","italic(W)"))

plot_manhatton_diff_snp_index(tidy_sw_data,filename = "snpindex_MvsW_manhatton")