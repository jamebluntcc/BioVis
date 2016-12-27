#2016-7-5 for enrichment barplot
#changed on 2016-10-30
library(ggplot2)
library(ggthemes)
library(dplyr)
library(cowplot)
args<-commandArgs(TRUE)
options(stringsAsFactors = F)
#for test
enrich_type <- args[1]
name <- args[2]
go_enrich1 <- args[3]
go_enrich2 <- args[4]
diff_list1 <- args[5]
diff_list2 <- args[6]
go_file <- args[7]
out_dir <- args[8]

enrich_type = "GO"
name <- "A-a_vs_A-b"
name1  <- unlist(strsplit(name,"_vs_"))[1]
name2 <- unlist(strsplit(name,"_vs_"))[2]
GO.A_a <- read.delim(go_enrich1)
GO.A_b <- read.delim(go_enrich2)
GO.subsetA <- read.delim("A-a-UP.subset.txt", header = F)
GO.subsetB <- read.delim("A-b-UP.subset.txt", header = F)
GO.all <- read.delim("tmp.Triticum_aestivum.go.txt", sep = ",")

GO.A_a <- read.delim("A-a_vs_A-b.A-a-UP.GO.enrich.xls")
GO.A_b <- read.delim("A-a_vs_A-b.A-b-UP.GO.enrich.xls")
GO.subsetA <- read.delim("A-a-UP.subset.txt")
GO.subsetB <- read.delim("A-b-UP.subset.txt")
GO.all <- read.delim("tmp.Triticum_aestivum.go.txt",sep = ",")
term_count <- length(unique(GO.all[,1]))
GO.A_count <- length(unique(GO.subsetA[,1]))
GO.B_count <- length(unique(GO.subsetB[,1]))

#cut overlong name
drop_overlong_name <- function(data,n=15){
  for(i in 1:dim(data)[1]){
    if(nchar(data$term[i]) > n){
      data$term[i] <- paste0(substr(data$term[i],1,n),'...')
    }
  }
  return(data)
}

#select data
data_select <- function(data){
  if(length(which(data$pvalue <= 0.05)) > 20){
    data <- data[which(data$pvalue <= 0.05)[1:20],]
  }else{
    data <- data[which(data$pvalue <= 0.05),]
  }
  return(data)
}

#prepare data
enrich_data <- function(data,color){
  data <- data[,c("over_represented_pvalue","numDEInCat","numInCat","term","ontology")]
  names(data)[1] <- 'pvalue'
  data <- data_select(drop_overlong_name(data))
  data$color <- color
  return(data)
}
data_setA <- enrich_data(GO.A_a,color = 'down')
data_setB <- enrich_data(GO.A_b,color = 'up')

#barpolt
theme_set(theme_bw()+theme(
  axis.text.x = element_text(angle = -90,hjust = 0,vjust = 0.5)
))
data_setA$type <- 'down'
data_setB$type <- 'up'
label_a <- paste("No.","name1"," up-regulated genes",sep = " ")
label_b <- paste("No.","name2"," up-regulated genes",sep = " ")

all_data <- rbind(data_setA,data_setB) 
all_data <- all_data[,c("term","numDEInCat","numInCat","ontology","type","color")]
all_data$expected <- all_data$numInCat / term_count * c(GO.A_count,GO.B_count)
all_data_by <- group_by(all_data,type)
all_data_by <- arrange(all_data_by,numDEInCat)
all_data_by$term <- factor(all_data_by$term,levels = all_data_by$term)
#---- add expected----
all_data_by <- dplyr::filter(all_data_by,numDEInCat > expected)
#-- add black color
all_data_by[dim(all_data_by)[1]+1,] <- all_data_by[dim(all_data_by)[1],]
all_data_by[dim(all_data_by)[1],c('numDEInCat','expected')] <- 0
all_data_by[dim(all_data_by)[1],"label"] <- 'expected'

#---plot----
theme_set(theme_bw())
p <- ggplot(all_data_by,aes(x=term,y=numDEInCat,fill=type))+
  geom_bar(stat = 'identity',width = 0.75)+geom_bar(aes(x=term,y=expected),fill='black',stat = 'identity',width = 0.75)+
  geom_text(aes(label='P<0.05'),angle=90,vjust=0.5,hjust=0)+
  scale_fill_manual(values = c('up'='red','down'='blue','expected'='black'),
                    breaks = c('expected','up','down'),labels = c('expected','up','down'))+
  facet_grid(.~color,scales = "free",space = "free")+
  guides(fill = guide_legend(title = ""))+
  ylab("Input.number")+xlab("Term")+
  ylim(c(0,max(all_data_by$numDEInCat)+max(all_data_by$numDEInCat)*0.1))+
  theme(legend.key = element_blank())
p
#-----changed it here----
merge_plot <- cowplot::plot_grid(p,p,p,p,p,p,labels = LETTERS[1:9],ncol = 3,nrow = 2)
ggsave(filename = 'GO_merge_barplot.png',plot = merge_plot,width = 16,height = 8)

