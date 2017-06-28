#-------------
# netbiov plot
#-------------
library(netbiov)
library(igraph)
#loading data
data("PPI_Athalina")
data("modules_PPI_Athalina")
#netbioV
opar <- par(no.readonly = T)
mst.plot(g1,layout.function = layout.fruchterman.reingold,expression = rnorm(vcount(g1)),v.size = 1)
#set color
my_col_func <- colorRampPalette(RColorBrewer::brewer.pal(9,'Set1'))
my_col <- my_col_func(24)
#use plot.modules
plot.modules(g1,
             mod.list = lm,
             modules.color = my_col,
             mod.lab = T,
             lab.color = 'orange4',
             lab.cex = 1,
             layout.function = layout.kamada.kawai)