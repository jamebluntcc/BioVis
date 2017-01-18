#venn plot
#venn plot by gplots
library(gplots)
oneName <- function() paste(sample(LETTERS,5,replace=TRUE),collapse="")
geneNames <- replicate(1000,oneName())
GroupA <- sample(geneNames,400,replace = F)
GroupB <- sample(geneNames,300,replace = F)
GroupC <- sample(geneNames,200,replace = F)
GroupD <- sample(geneNames,500,replace = F)
input <- list(`1`=GroupA,`2`=GroupB,`3`=GroupC)
gplots::venn(input) #no fill color
#venn plot by vennDiagram
library(VennDiagram)
names(input) <- c('A','B','C')
VennDiagram::venn.diagram(
  x = input,filename = "test4.png",
  col = "transparent",fill = c("cornflowerblue", "green", "yellow"), #set fill color
  #label.col = c("orange", "white", "darkorchid4"), set number color
  cex = 1.5,
  fontfamily = "serif",
  fontface = "bold",
  #cat.col = c("darkblue", "darkgreen"), #set label color
  cat.cex = 1.5,
  #cat.pos = c(-10,10),  #set label pos
  #cat.dist = 0.07,
  cat.fontfamily = "serif",
  rotation.degree = 0,
  margin = 0.1  #set plot's size margin more biger the size more samll
)
                          
