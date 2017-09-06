# coding=utf-8
import re
import sys
import click


def find_gtf_value(strings, key):
    pattern = r'{0} \"(.*?)\";'.format(key)
    p = re.compile(pattern)
    result = p.search(strings)
    if result:
        return result.group(1)
    else:
        return ''


@click.command()
@click.option('--filename', help='a gtf file.')
@click.option('--keys', default=['gene_id', 'transcript_id', 'cov', 'FPKM'],
              help='extract gtf value list')
def tidy_gtf_file(filename, keys):
    '''
    extract gtf file value
    '''
    try:
        f = open(filename, 'r+')
        with open('tidy_' + filename, 'w+') as tf:
            for line in f:
                each_list = line.split("\t")
                value_list = [find_gtf_value(each_list[-1], key) for key in keys]
                write_list = each_list[:len(each_list)-1]
                write_list.extend(value_list)
                tf.write('\t'.join(write_list)+'\n')
    except IOError:
        print 'not find gtf file!'
        sys.exit(1)
    finally:
        f.close()


if __name__ == '__main__':
    tidy_gtf_file()
