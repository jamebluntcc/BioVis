import os
import click
import glob
import subprocess

@click.command()
@click.option('--snpdir', help='snp index ann.xls dir')
@click.option('--cellsplit', help='snp index cellsplit', default=',')
@click.option('--slidewindow', help='slide window file')
@click.option('--outdir', help='plot output dir')
def run_index(snpdir, cellsplit, slidewindow, outdir):
    '''
    input: snp index dir
    return: snp index plot
    '''
    search_pattern = os.path.join(snpdir, '*.ann.xls')
    snp_files = glob.glob(search_pattern)
    for file in snp_files:
        info = file.split('.')[0]
        outfile = '_'.join(info.split('vs'))
        cutCmd = 'cut -f 1-2,8-9 {0} > {1}'.format(file, info + '.cut')
        subprocess.call(cutCmd, shell=True)
        cleanCmd = "python clean_snp.py --filename {cutfile} --output {outfile} --cellsplit {cellsplit}".format(
            cutfile=info + '.cut',
            outfile=outfile,
            cellsplit="'" + cellsplit + "'"
        )
        subprocess.call(cleanCmd, shell=True)
        plotCmd = 'python run_snp_index.py --bedfile {bedfile} --filepath {filepath} --outdir {outdir}'.format(
            bedfile=slidewindow,
            filepath=outfile,
            outdir=info)
        print plotCmd
        subprocess.call(plotCmd, shell=True)
    return None


if __name__ == '__main__':
    run_index()
