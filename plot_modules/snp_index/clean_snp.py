# coding:utf-8
import click


@click.command()
@click.option('--filename', help='info')
@click.option('--output', help='output')
@click.option('--filesplit', help=r'file split \t', default='\t')
@click.option('--cellsplit', help=r'cell split ,', default=',')
def clean_snp(filename, output, filesplit, cellsplit):
    with open(filename, 'r+') as info:
        outfile = open(output, 'w+')
        head = info.readline().strip()
        row = info.readline().strip()
        while (row):
            row_list = row.split(filesplit)
	    #print row_list
            chr, pos = row_list[:2]
            out_line = [chr, pos]
            for each in row_list[2:]:
                if each == '0':
                    out_line.extend(['0', '0'])
                else:
                    if len(each.split(cellsplit)) > 2:
                        tmp = [int(k) for k in each.split(cellsplit)]
                        max_one = max(tmp)
                        min_one = min(tmp)
                        out_line.extend([str(max_one), str(min_one)])
                    else:
                        out_line.extend(each.split(cellsplit))
            outfile.write('\t'.join(out_line) + '\n')
            row = info.readline().strip()
        outfile.close()
    print "done"


if __name__ == '__main__':
    clean_snp()
