import os
import click
import subprocess
#from get_group_table import get_group_table
basedir = './'
SNP_INDEX_PATH = './'

@click.command()
@click.option('--bedfile', help='bed slider file')
@click.option('--filepath', help='snp index compare table')
@click.option('--outdir', help='a dir which save your plots')
def run_script(bedfile,filepath, outdir):
    # run bash&R script for snp index
    # bash generate bed.out file
    print '--start--'
    bedCmd = 'sh cmd.sh {0} {1}'.format(bedfile, filepath)
    subprocess.call(bedCmd,shell=True)
    # add if bed path exists
    if not os.path.exists(outdir):
        os.mkdir(outdir)
    Rcmd = 'Rscript {path} {bed_path} {outpath}'.format(path=os.path.join(basedir,
                                                        		 'snp_index_by_bed.R'),
                                              		bed_path=filepath+'.bed.out',
                                                        outpath=outdir)
    #print Rcmd
    subprocess.call(Rcmd,shell=True)
    print '--end--'
    return 'done'


if __name__ == '__main__':
    run_script()
