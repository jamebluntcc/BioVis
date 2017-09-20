library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
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

tidy_snp_data <- function(data,fileName){
  names(data) <- c('V1','V2','V3','V4')
  df_3 <- as.data.frame(t(sapply(data$V3,deal_snp_cell)))
  df_4 <- as.data.frame(t(sapply(data$V4,deal_snp_cell)))
  data$V5 <- df_3$V1
  data$V6 <- df_3$V2
  data$V7 <- df_4$V1
  data$V8 <- df_4$V2
  data <- dplyr::filter(snp_hq_tidy,(V6 / (V5+V6) >= 0.3 & (V5+V6) >= 5) |
                                    (V8 / (V7+V8) >= 0.3 & (V7+V8) >= 5))
  write.table(data,fileName,quote = F,row.names = F,sep = "\t")
}

tidy_sliderWindow_data <- function(data,header){
  names(data) <- paste0('X',1:dim(data)[1])
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
                       labels,
                       break_seq,
                       label_seq){
  names(data) <- c('chr','pos','V1','V2','V3')
  data$chr <- paste0('chr',data$chr)
  text_df <- data.frame(x=7e7,y=0.85)
  chr_set <- unique(data$chr)
  for(i in chr_set){
    up_p <- ggplot(data[which(data$chr==i),],aes(x=pos,y=V1))+
      geom_point(color=col[1],size=rel(0.8))+
      geom_text(data = text_df,aes(x=x,y=y,label=labels[1]),size=8)+
      geom_smooth(se=F,color='red')+
      xlab("Postion")+ylab("SNP Index")+
      scale_x_continuous(breaks = break_seq,
                         labels = paste0(label_seq,'M'))+
      scale_y_continuous(breaks = seq(from=0,to=1,by=0.2),
                         labels = seq(from=0,to=1,by=0.2))+
      guides(color=guide_legend(title = ""))+ggtitle(label = i)
    md_p <- ggplot(data[which(data$chr==i),],aes(x=pos,y=V2))+
      geom_point(color=col[2],size=rel(0.8))+
      geom_text(data = text_df,aes(x=x,y=y,label=labels[2]),size=8)+
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
      geom_text(data = text_df,aes(x=x,y=y,label=labels[3]),size=8)+
      xlab("Postion")+ylab("DIFF")+
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
