#--------------
#manhattan plot
#--------------
library(qqman)
library(RColorBrewer)
library(tidyverse)
library(data.table)
GWAS_data <- fread('../data/GWAS_test.assoc')
GWAS_data <- dplyr::select(GWAS_data,CHR,SNP,BP,P)
manhattan_col <- brewer.pal('Set1',n = 9)
png('manhattan.png',width = 1200,height = 800)
manhattan(na.omit(GWAS_data),chr = "CHR",p = "P",snp = 'SNP',annotatePval = 1e-05,col = manhattan_col)
dev.off()