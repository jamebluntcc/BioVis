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

tidy_snp_data <- function(data){
  names(data) <- c('V1','V2','V3','V4')
  df_3 <- as.data.frame(t(sapply(data$V3,deal_snp_cell)))
  df_4 <- as.data.frame(t(sapply(data$V4,deal_snp_cell)))
  data$V5 <- df_3$V1
  data$V6 <- df_3$V2
  data$V7 <- df_4$V1
  data$V8 <- df_4$V2
  return(data)
}
#-- read file and tidy--
dna_hq <- read_delim('data/dna.snp.hq.ann.table',col_names = T,delim = "\t")
snp <- read_delim('data/snp.xls',col_names = T,delim = "\t")
chrset <- 1:10
dna_hq_tidy <- dplyr::select(dna_hq,c('Chrom','Pos','G-1','K-2'))
snp_tidy <- dplyr::select(snp,c('CHROM','POS','S','R'))
index <- match(dna_hq_tidy$Chrom,chrset,nomatch = 0)
dna_hq_tidy <- dna_hq_tidy[which(index!=0),]
snp_tidy <- snp_tidy[which(match(snp$CHROM,chrset,nomatch = 0)!=0),]
snp_tidy <- tidy_snp_data(snp_tidy)
dna_hq_tidy <- tidy_snp_data(dna_hq_tidy)
snp_select <- dplyr::filter(snp_tidy,(V6 / (V5+V6) >= 0.3 & (V5+V6) >= 5) |
                                     (V8 / (V7+V8) >= 0.3 & (V7+V8) >= 5))
dna_hq_select <- dplyr::filter(dna_hq_tidy,(V6 / (V5+V6) >= 0.3 & (V5+V6) >= 5) |
                                     (V8 / (V7+V8) >= 0.3 & (V7+V8) >= 5))
write.table(snp_select,"snp_select.txt",quote = F,row.names = F,sep = "\t")
write.table(dna_hq_select,"dna_hq_select.txt",quote = F,row.names = F,sep = "\t")
#--plot snp data--
dna_hq_treat <- read.delim("data/dna_hq_select.bed3.out.treat",header = F,sep = "\t")
snp_treat <- read.delim("data/snp_select.bed3.out.treat",header = F,sep = "\t")
dna_hq_treat <- mutate(na.omit(dna_hq_treat),V9=(V6)/(V5+V6),V10=V8/(V7+V8))
dna_hq_treat <- dplyr::filter(dna_hq_treat,V4 > 10)
dna_hq_treat2 <- dna_hq_treat[,c(1,2,9,10)]
names(dna_hq_treat2) <- c('chr','pos','G_1','K_2')

snp_treat <- mutate(na.omit(snp_treat),V9=(V6)/(V5+V6),V10=V8/(V7+V8))
snp_treat <- dplyr::filter(snp_treat,V4 > 10)
snp_treat <- select(snp_treat,c('V1','V2','V9','V10'))
names(snp_treat) <- c('chr','pos','S','R')
my_theme <- theme_bw()+theme(
  panel.border = element_rect(colour = 'black'),
  strip.background = element_blank(),
  axis.title = element_text(face = 'bold',color='black'),
  axis.text = element_text(color = 'black'),
  plot.title = element_text(hjust = 0.5)
)
theme_set(my_theme)
merge_plot <- function(data,filename,col=brewer.pal(3,'Set2'),labels){
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
      scale_x_continuous(breaks = seq(from=0,to=8e7,by=2e7),
                         labels = paste0(seq(from=0,to=80,by=20),'M'))+
      scale_y_continuous(breaks = seq(from=0,to=1,by=0.2),
                         labels = seq(from=0,to=1,by=0.2))+
      guides(color=guide_legend(title = ""))+ggtitle(label = i)
    md_p <- ggplot(data[which(data$chr==i),],aes(x=pos,y=V2))+
      geom_point(color=col[2],size=rel(0.8))+
      geom_text(data = text_df,aes(x=x,y=y,label=labels[2]),size=8)+
      geom_smooth(se=F,color='red')+
      xlab("Postion")+ylab("SNP Index")+
      scale_x_continuous(breaks = seq(from=0,to=8e7,by=2e7),
                         labels = paste0(seq(from=0,to=80,by=20),'M'))+
      scale_y_continuous(breaks = seq(from=0,to=1,by=0.2),
                         labels = seq(from=0,to=1,by=0.2))+
      guides(color=guide_legend(title = ""))
    do_p <- ggplot(data[which(data$chr==i),],aes(x=pos,y=V3))+
      geom_point(color=col[3],size=rel(0.8))+
      geom_smooth(se=F,color='red')+
      geom_text(data = text_df,aes(x=x,y=y,label=labels[3]),size=8)+
      xlab("Postion")+ylab("DIFF")+
      scale_x_continuous(breaks = seq(from=0,to=8e7,by=2e7),
                         labels = paste0(seq(from=0,to=80,by=20),'M'))+
      scale_y_continuous(breaks = seq(from=-1,to=1,by=0.5),
                         labels = seq(from=-1,to=1,by=0.5))+
      guides(color=guide_legend(title = ""))
    merge_p <- grid.arrange(up_p, md_p, do_p, ncol=1, nrow =3)
    ggsave(filename = paste0(filename,i,'.pdf'),plot = merge_p,width = 8,height = 12)
    ggsave(filename = paste0(filename,i,'.png'),plot = merge_p,width = 8,height = 12)
  }
}
snp_treat$DIFF <- snp_treat$S - snp_treat$R
merge_plot(snp_treat,filename = 'snp_',labels = c('S','R','Diff'))
dna_hq_treat2$DIFF <- dna_hq_treat2$G_1 - dna_hq_treat2$K_2
merge_plot(dna_hq_treat2,filename = 'dna_hq_',labels = c('G_1','K_2','Diff'))
#---
