#!/bin/bash/
chr_wd=$1
input_file=$2
#output_file=$3
#bedtools makewindows -g chr.size -w $window -s $slider > chr_wd
#cut -f 1-2,7-8 $input_file > "$input_file".cut
#python clean_snp.py --filename "$inputfile".cut --output data/"$inputfile".cut --cellsplit '|'
awk -vOFS='\t' 'NR>1 {print $1,$2-1,$2,1,$3,$4,$5,$6}' "$input_file" > "$input_file".bed
bedtools map -a $chr_wd -b "$input_file".bed -c 4,5,6,7,8 -o sum,sum,sum,sum,sum | sed -re 's/\./0/g' > "$input_file".bed.out
#R script snp_index_by_bed.R "$input_file".bed.out $output_file
