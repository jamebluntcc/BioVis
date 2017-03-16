#----------------------------------------
#this script to plot venn by chencheng
#use vennDiagram library
#create on 2017-3-15
#----------------------------------------
#----create test data----
library(argparser)
oneName <- function() paste(sample(LETTERS,5,replace=TRUE),collapse="")
geneNames <- replicate(1000,oneName())
GroupA <- sample(geneNames,400,replace = F)
GroupB <- sample(geneNames,300,replace = F)
GroupC <- sample(geneNames,200,replace = F)
GroupD <- sample(geneNames,500,replace = F)
#-----venn plot by vennDiagram-----
if(!require(VennDiagram)){
  install.packages("VennDiagram")
  library(VennDiagram)
}
# p <- argparser::arg_parser('this is a R script to plot Venn')
# p <- add_argument(p,'file_path',help = 'pca file path',type = "character")
# p <- add_argument(p,'group_sample',help = 'pca group information',type = "character")
# p <- add_argument(p,'out_path',help = 'putout pca plot path',type = "character")
# argv <- parse_args(p)
fill_col <- c("#386cb0","#fdb462","#7fc97f","#ef3b2c")
venn_plot <- function(venn_data,fill_colour,file_name,main = NULL){
  VennDiagram::venn.diagram(
    x = venn_data,filename = file_name,main = main,
    col = "transparent",fill = fill_colour, #set fill color
    label.col = "white", #set number color
    cex = 1.5,
    fontfamily = "serif",
    fontface = "bold",
    cat.col = fill_colour, #set label color
    cat.cex = 1.5,
    cat.fontfamily = "serif",
    rotation.degree = 0,
    margin = 0.1  #set plot's size margin more biger the size more samll
  )
}

                          
