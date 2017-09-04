# 2017-09-02
# gene_struct tarck chunk plot
options(stringsAsFactors = F)
library(ggplot2)
library(RColorBrewer)
library(stringr)

gene_split <- function(str,split){
  return(str_trim(str_split(str_split(str,split)[[1]][2],';')[[1]][1]))
}

get_fpkm <- function(gene_data){
  transcript_index = which(gene_data$V3 == 'transcript')
  transcript_fpkm <- as.vector(sapply(gene_data$V9[transcript_index],
                            FUN = gene_split,split='FPKM'))
  return(transcript_fpkm)
}



deal_data <- function(gene_data){
  gene_data$var <- sapply(gene_data$V9,FUN = gene_split,split='cov')
  gene_data$var <- as.numeric(gene_data$var)
  gene_data$gene_id <- sapply(gene_data$V9,FUN = gene_split,split='gene_id')
  gene_data$transcript_id <- sapply(gene_data$V9,FUN = gene_split,split='transcript_id')
  gene_data$y1 = NA
  gene_data$y2 = NA
  gene_data$num = NA
  gene_data$line = NA
 
  gene_type <- unique(gene_data$gene_id)
  if(length(gene_type) != 1){
    stop('gene id must only one')
  }
  transcript_index = which(gene_data$V3 == 'transcript')
  max_num = dim(gene_data)[1]
  tmp = 0
  for(i in 1:(length(transcript_index)-1)){
      tmp = tmp + 0.5
      gene_data$y1[(transcript_index[i]+1):(transcript_index[i+1]-1)] <- tmp
      tmp <- tmp + 1
      gene_data$y2[(transcript_index[i]+1):(transcript_index[i+1]-1)] <- tmp
      gene_data$num[(transcript_index[i]+1):(transcript_index[i+1]-1)] <- i
  }
  # add last transcript id
  gene_data$y1[(transcript_index[i+1]+1):max_num] <- tmp + 0.5
  gene_data$y2[(transcript_index[i+1]+1):max_num] <- tmp + 1.5
  gene_data$num[(transcript_index[i+1]+1):max_num] <- i + 1
  tmp2 = 1
  gene_data$line[which(gene_data$num == 1)] <- 1
  for(i in 2:max(gene_data$num,na.rm = T)){
      tmp2 = tmp2 + 1.5
      gene_data$line[which(gene_data$num == i)] <- tmp2
    }
  gene_data <- gene_data[,c(1,3,4,5,10,11,12,13,14,15,16)]
  names(gene_data) <- c('chr','exon','start','end','coverage','gene_id','transcript_id','y1','y2','num','line')
  return(na.omit(gene_data))
}

gene_chunk_plot <- function(tidyData,show_fpkm=F,print=F,w=12,fileName='plot'){
  num = length(unique(tidyData$transcript_id))
  if(any(show_fpkm)){
    Ylabels <- paste(unique(tidyData$transcript_id),
                     paste('FPKM',show_fpkm,sep = ":"),sep = "\n")
  }else{
    Ylabels <- unique(tidyData$transcript_id)
  }
  p <- ggplot(data=tidyData)+
    scale_x_continuous(name="postion")+
    scale_y_continuous(name="transcript id",
                       breaks = unique(tidyData$line),
                       labels = Ylabels)+
    geom_hline(aes(yintercept = line),size=0.8)+
    geom_rect(mapping=aes(xmin=start, xmax=end, ymin=y1, ymax=y2, fill=coverage))+
    scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(n = 7,name="RdYlBu")))(100))+
    theme_bw()+theme(panel.grid = element_blank(),
                     axis.title = element_text(face = 'bold', size = rel(1.2)),
                     axis.text = element_text(face = 'bold',color = 'black'))
  if(print){
    print(p)
  }else{
    ggsave(plot = p,filename = paste(fileName,'png',sep = '.'),width = w,height = num * 1.5)
    ggsave(plot = p,filename = paste(fileName,'pdf',sep = '.'),width = w,height = num * 1.5)
  }
}
# for test
gene_data_part1 <- read.delim("../data/MHd.x.gtf",header = F,sep = "\t")
gene_data_part2 <- read.delim("../data/WHd.x.gtf",header = F,sep = "\t")
tidy_data <- deal_data(gene_data_part1)
gene_chunk_plot(tidy_data,show_fpkm = F,fileName = 'Mhd')
tidy_data <- deal_data(gene_data_part2)
show_fpkm = get_fpkm(gene_data_part2)
gene_chunk_plot(tidy_data,show_fpkm = show_fpkm,fileName = 'WHd')

  
  
  
  