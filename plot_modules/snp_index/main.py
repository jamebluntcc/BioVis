import os
import click
import glob
import subprocess
basedir = os.path.abspath(os.path.dirname(__file__))

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
    plot_dirs = []
    for file in snp_files:
        info = file.split('.')[-3].strip('/')
        outfile = '_'.join(info.split('vs'))
        cutCmd = 'cut -f 1-2,8-9 {0} > {1}'.format(file, os.path.join(snpdir, info) + '.cut')
        subprocess.call(cutCmd, shell=True)
        cleanCmd = "python {clean_snp_file} --filename {cutfile} --output {outfile} --cellsplit {cellsplit}".format(
            clean_snp_file=os.path.join(basedir, 'clean_snp.py'),
            cutfile=os.path.join(snpdir, info) + '.cut',
            outfile=os.path.join(snpdir, outfile),
            cellsplit="'" + cellsplit + "'"
        )
        subprocess.call(cleanCmd, shell=True)
        plotCmd = 'python {run_snp_index_file} --bedfile {bedfile} --filepath {filepath} --outdir {outdir}'.format(
            run_snp_index_file=os.path.join(basedir, 'run_snp_index.py'),
            bedfile=slidewindow,
            filepath=os.path.join(snpdir, outfile),
            outdir=os.path.join(outdir, info))
        subprocess.call(plotCmd, shell=True)
        subprocess.call('rm {0}'.format(os.path.join(snpdir, outfile)), shell=True)
        plot_dirs.append(info)
    # add rm & zip file func
    rm_middle_file(snpdir)
    zip_plots(outdir, plot_dirs)
    return None


def rm_middle_file(snpdir):
    rm_set = ['*.cut', '*.bed', '*.bed.out']
    for suffix in rm_set:
        rm_files = glob.glob(os.path.join(snpdir, suffix))
        for file in rm_files:
            subprocess.call('rm {}'.format(file), shell=True)


def zip_plots(outdir, plot_dir):
    for dir in plot_dir:
        out_path = os.path.join(outdir, dir)
        subprocess.call('zip {0} {1}'.format('.'.join([out_path, 'zip']), out_path), shell=True)
        subprocess.call('rm -rf {0}'.format(out_path), shell=True)


if __name__ == '__main__':
    run_index()
