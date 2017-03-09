#merge enrichment barplot
library(data.table)
library(ggplot2)

data <- fread("/media/zxchen/3dddd6c4-2700-41a5-a677-15b165fa4e64/project/Analysis_project_backup/OM-mRNA-Medicago_truncatula-P20161206/test/merged.go.table.txt")
#----theme set-----
enrich_theme <- theme_bw()+theme(
  legend.key = element_blank(),
  axis.text.x = element_text(color = "black",face = "bold",angle = 90,hjust = 1,vjust = 0.5, size = rel(0.6)),
  axis.text.y = element_text(color = "black",face = "bold", size = rel(0.6)),
  axis.title.y = element_text(color = "black",face = "bold",size = rel(0.6)),
  panel.grid.minor.x  = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  strip.text = element_blank(),
  strip.background = element_blank(),
  legend.text = element_text(size = rel(0.6))
)
theme_set(enrich_theme)
