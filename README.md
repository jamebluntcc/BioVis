# onmath visualiztion

## venn plot

venn plot 我们使用R的[VennDiagram](https://cran.r-project.org/web/packages/VennDiagram/VennDiagram.pdf)包进行可视化,设定最大可以画4个group的venn。

```r
oneName <- function() paste(sample(LETTERS,5,replace=TRUE),collapse="")
geneNames <- replicate(1000,oneName())
GroupA <- sample(geneNames,400,replace = F)
GroupB <- sample(geneNames,300,replace = F)
GroupC <- sample(geneNames,200,replace = F)
GroupD <- sample(geneNames,500,replace = F)
```

我们将VennDiagram封装在一个函数中,设置好一些基本的参数,当自己在实际项目中需要进行修改一些VennDiagram中的参数时,可以直接在函数中进行修改然后再保存运行。
```
fill_col <- c("#386cb0","#fdb462","#7fc97f","#ef3b2c")
venn_plot(venn_data = list(A=GroupA,B=GroupB),fill_colour = fill_col[1:2],file_name = 'venn_test_group2.png')
```
![venn plot group=2](./demo_plots/venn_test_group2.png)

```
fill_col <- c("#386cb0","#fdb462","#7fc97f","#ef3b2c")
venn_plot(venn_data = list(A=GroupA,B=GroupB,C=GroupC),fill_colour = fill_col[1:3],file_name = 'venn_test_group3.png')
```
![venn plot group=3](./demo_plots/venn_test_group3.png)

```
fill_col <- c("#386cb0","#fdb462","#7fc97f","#ef3b2c")
venn_plot(venn_data = list(A=GroupA,B=GroupB,C=GroupC,D=GroupD),fill_colour = fill_col,file_name = 'venn_test_group4.png')
```
![venn plot group=4](./demo_plots/venn_test_group4.png)

### VennDiagram's param
 - file type:png
