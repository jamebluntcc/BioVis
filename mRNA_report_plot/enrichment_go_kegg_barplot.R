#2016-12-22
#kegg and go barplot
library(ggplot2)
library(dplyr)
options(stringsAsFactors = F)
argv <- commandArgs(T)
#for test:
#report_dir <- '/media/zxchen/3dddd6c4-2700-41a5-a677-15b165fa4e64/project/Analysis_project_backup/OM-mRNA-Medicago_truncatula-P20161206/'
kegg_all <- '/home/public/database/kegg/blast_annotate/mtr.gene.blasttab'
kegg_tables <- '/media/zxchen/3dddd6c4-2700-41a5-a677-15b165fa4e64/project/Analysis_project_backup/OM-mRNA-Medicago_truncatula-P20161206/OM-mRNA-15-Medicago_truncatula/mRNA_analysis_results/enrichment_analysis/KEGG/KEGG_enrich_table/'
all_lists_dir <- '/media/zxchen/3dddd6c4-2700-41a5-a677-15b165fa4e64/project/Analysis_project_backup/OM-mRNA-Medicago_truncatula-P20161206/3.diff/Diff_list/'
go_all <- '/home/public/database/go/mtr.go.txt'
go_tables <- '/media/zxchen/3dddd6c4-2700-41a5-a677-15b165fa4e64/project/Analysis_project_backup/OM-mRNA-Medicago_truncatula-P20161206/OM-mRNA-15-Medicago_truncatula/mRNA_analysis_results/enrichment_analysis/GO/GO_enrich_table/'
find_count <- function(count_data){
  return(length(unique(count_data[,1])))
}
#----for kegg----
#---for go----
kegg.all <- read.delim(kegg_all,header = F)
kegg_term_count <- find_count(kegg.all)
go.all <- read.delim(go_all,header = F)
go_term_count <- find_count(go.all)
go_tables_dirs <- list.files(go_tables)
kegg_tables_dirs <- list.files(kegg_tables)
all_list_files <- list.files(all_lists_dir)
#---for test one kegg plot----
test_one_dir <- paste(kegg_tables,kegg_tables_dirs[1],sep = "/")
test_one_dir_go <- paste(go_tables,go_tables_dirs[1],sep = "/")
test_one_files_go <- list.files(test_one_dir_go)
test_one_files <- list.files(test_one_dir)
#use python
test_one_data1 <- read.delim(paste(test_one_dir,"group1_vs_group2.group1-UP.KEGG.enrich.xls",sep = "/"),header = T)
test_one_data2 <- read.delim(paste(test_one_dir,"group1_vs_group2.group2-UP.KEGG.enrich.xls",sep = "/"),header = T)
test_one_data1_go <- read.delim(paste(test_one_dir_go,"group1_vs_group2.group1-UP.GO.enrich.xls",sep = "/"),header = T)
test_one_data2_go <- read.delim(paste(test_one_dir_go,"group1_vs_group2.group2-UP.GO.enrich.xls",sep = "/"),header = T)
test_one_list1 <- read.delim(paste(all_lists_dir,"group1_vs_group2.group1-UP.list",sep = "/"),header = F)
test_one_list2 <- read.delim(paste(all_lists_dir,"group1_vs_group2.group2-UP.list",sep = "/"),header = F)
test_one_list1_count <- find_count(test_one_list1)
test_one_list2_count <- find_count(test_one_list2)

enrich_data <- function(data,type,label,show_num,max_name_length,all_count,term_count){
  if(type == "kegg" & all(c("X.Term","Input.number","Background.number","P.Value","Corrected.P.Value") %in% names(data))){
    data <- data[,c("X.Term","Input.number","Background.number","P.Value","Corrected.P.Value")]
  }else if(type == "go" & all(c("over_represented_pvalue","numDEInCat","numInCat","term","ontology") %in% names(data))){
  data <- data[,c("term","numDEInCat","numInCat","over_represented_pvalue","qvalue","ontology")]
  }else stop('check your data and rownames!')
  names(data)[1:5] <- c('Term','Input_number','Background_number','P_value','Corrected_P_Value')
  if(length(which(data$P_value < 0.05)) >= show_num){
    data <- data[which(data$P_value < 0.05),][1:show_num,]
  }else{
    data <- data[which(data$P_value < 0.05),]
  }
  for(i in 1:dim(data)[1]){
    if(nchar(data$Term[i]) > max_name_length){
      data$Term[i] <- paste0(substr(data$Term[i],1,max_name_length),'...')
    }
  }
  data$label <- label
  data$sign <- ""
  data$sign <- ifelse(data$Corrected_P_Value < 0.05,'*','')
  data$expected <- data$Background_number / all_count * term_count
  data <- data %>% dplyr::filter(Input_number > expected)
  if(dim(data)[1] == 0){
    stop('data empty!')
  }
  data <- arrange(data,Input_number)
  data
}
test_go <- enrich_data(data = test_one_data1_go,type = "go",label = "up",show_num = 20,max_name_length = 20,all_count = go_term_count,term_count = test_one_list1_count)
  
kegg_barplot <- function(KEGG_merge_data,x_lab = "Input Number",y_lab = "Term"){
  p <- ggplot(KEGG_merge_data,aes(y=Input_number,x=Term,fill=label))+
    geom_bar(stat = 'identity')+geom_bar(aes(x=Term,y=expected),fill='black',stat = 'identity')+
    geom_text(aes(label = sign),vjust=0.5,hjust = -0.2,fontface = "bold",size = 6)+
    ylim(c(0,max(KEGG_merge_data$Input_number)+max(KEGG_merge_data$Input_number)*0.1))+
           scale_fill_manual(values = c('up'='red','down'='blue','expected'='black'),
                             breaks = c('expected','up','down'),labels = c('expected','up','down'))+
           guides(fill = guide_legend(title = ""))+ylab(y_lab)+xlab(x_lab)+
           theme_bw()+theme(legend.key = element_blank(),
                            axis.text.x = element_text(angle = -90))+coord_flip()
         p
}
#---kegg barplot---
KEGG_up_data <- enrich_data(test_one_data1,type = 'kegg',label = "up",show_num = 15,max_name_length = 20,all_count = kegg_term_count,term_count = test_one_list1_count)
KEGG_down_data <- enrich_data(test_one_data2,type = 'kegg',label = "down",show_num = 15,max_name_length = 20,all_count = kegg_term_count,term_count = test_one_list2_count)
KEGG_merge_data <- rbind(KEGG_up_data,KEGG_down_data)
KEGG_merge_data$Term <- factor(KEGG_merge_data$Term,levels = KEGG_merge_data$Term)
#add expected
KEGG_merge_data[dim(KEGG_merge_data)[1]+1,] <- KEGG_merge_data[dim(KEGG_merge_data)[1],]
KEGG_merge_data[dim(KEGG_merge_data)[1],c('Input_number','expected')] <- 0
KEGG_merge_data[dim(KEGG_merge_data)[1],"label"] <- 'expected'
kegg_barplot(KEGG_merge_data)

test_go1 <- enrich_data(data = test_one_data1_go,type = "go",label = "up",show_num = 20,max_name_length = 20,all_count = go_term_count,term_count = test_one_list1_count)
test_go2 <- enrich_data(data = test_one_data2_go,type = "go",label = "down",show_num = 20,max_name_length = 20,all_count = go_term_count,term_count = test_one_list2_count)
go_merge_data <- rbind(test_go1,test_go2) 
go_merge_data <- arrange(go_merge_data,ontology,label,Input_number)
go_merge_data$Term <- factor(go_merge_data$Term,levels = go_merge_data$Term)
#add expected
go_merge_data[dim(go_merge_data)[1]+1,] <- go_merge_data[dim(go_merge_data)[1],]
go_merge_data[dim(go_merge_data)[1],c('Input_number','expected')] <- 0
go_merge_data[dim(go_merge_data)[1],"label"] <- 'expected'
#---test---
p <- ggplot(go_merge_data,aes(y=Input_number,x=Term,fill=label))+
  geom_bar(stat = 'identity')+geom_bar(aes(x=Term,y=expected),fill='black',stat = 'identity')+
  geom_text(aes(label = sign),vjust=0.5,hjust = -0.2,fontface = "bold",size = 6)+
  ylim(c(0,max(go_merge_data$Input_number)+max(go_merge_data$Input_number)*0.1))+
  scale_fill_manual(values = c('up'='red','down'='blue','expected'='black'),
                    breaks = c('expected','up','down'),labels = c('expected','up','down'))+
  guides(fill = guide_legend(title = ""))+ylab('y_lab')+xlab('x_lab')+
  facet_grid(ontology~.,scales = "free_y",space = "free_y")+coord_flip()
p
#----theme set-----
enrich_theme <- theme_bw()+theme(
  legend.key = element_blank(),
  axis.text.x = element_text(color = "black",face = "bold"),
  axis.text.y = element_text(hjust = 1,vjust = 0.5,color = "black",face = "bold"),
  panel.grid.minor.x  = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  strip.text = element_text(face = "bold"),
  strip.background = element_blank()
)
theme_set(enrich_theme)
#----



