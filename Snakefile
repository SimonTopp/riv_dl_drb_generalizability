#from snakemake.io import expand
import numpy as np

configfile: "config.yml"
from gwn.generate_training_data_drb import prep_data
from gwn.viz import plot_results
from gwn.train_drb import train
import gwn.post_proc_utils as ppu
from gwn.util import asRunConfig

subdirs = ['LLO1', 'LLO2', 'LLO3']
ensemble = [f"_{i}" for i in np.arange(10)]

rule all:
    input:
        expand("{outdir}/{subdir}/{ensemble}/{metric_type}_metrics.csv",
            outdir=config["out_dir"],
            subdir=subdirs,
            ensemble = ensemble,
            metric_type=['overall', 'month', 'reach']),
        expand("{outdir}/asRunConfig.yml",
            outdir=config['out_dir']),
        expand("{outdir}/Snakefile",
            outdir=config['out_dir']),
        expand("{outdir}/{subdir}/{ensemble}/figs/training_loss.png",
            outdir=config["out_dir"],
            subdir=subdirs,
            ensemble = ensemble),
        expand("{outdir}/{subdir}/prepped.npz",
            outdir=config['out_dir'],
            subdir=subdirs)

def get_llo_arg(wildcards):
    if wildcards.subdir == 'LLO1':
        return 1
    elif wildcards.subdir == 'LLO2':
        return 2
    elif wildcards.subdir == 'LLO3':
        return 3

rule as_run_config:
    output:
        "{outdir}/asRunConfig.yml"
    #group: "prep"
    run:
        asRunConfig(config,output[0])


rule copy_snakefile:
    output:
        "{outdir}/Snakefile"
    #group: "prep"
    shell:
        """
        scp Snakefile {output[0]}
        """

rule prep_io_data:
    input:
        config['obs_temp'],
        config['sntemp_file'],
    output:
        "{outdir}/{subdir}/prepped.npz",
    #group: "prep"
    threads: 3
    params:
        llo_group=get_llo_arg
    run:
        prep_data(input[0],input[1],
            x_vars=config['x_vars'],
            train_start_date=config['train_start_date'],
            train_end_date=config['train_end_date'],
            val_start_date=config['val_start_date'],
            val_end_date=config['val_end_date'],
            test_start_date=config['test_start_date'],
            test_end_date=config['test_end_date'],
            seq_length=config['seq_length'],
            offset=config['offset'],
            out_file=output[0],
            normalize_y=config['scale_y'],
            distfile=config['dist_file'],
            dist_idx_name="rowcolnames",
            dist_type=config['dist_type'],
            clip_y=config['clip_y'],
            lto=config['lto'],
            lto_type=config['lto_type'], # wildcards.subdir,
            llo=params.llo_group,
            test_group=config['test_group'],
        )

rule load_module:
    output:
        touch("{prep_dir}/{run_dir}/module.loader")
    #group: "{subdir}_{ensemble}_train"
    shell:
        """
        module load analytics cuda10.1/toolkit/10.1.105 
        """

rule train:
    input:
        "{prep_dir}/prepped.npz",
        "{prep_dir}/{run_dir}/module.loader"
    output:
        "{prep_dir}/{run_dir}/adjmat_out.csv",
        "{prep_dir}/{run_dir}/train_log.csv",
        "{prep_dir}/{run_dir}/weights_best_val.pth",
    #group: "{subdir}_{ensemble}_train"
    resources:
        nvidia_gpu=1
    threads: 4
    #params: run_dir = get_out_dir
    run:
        train(input[0],
            wildcards.get_out_dir,
            batch_size=config['batch_size'],
            epochs=config['epochs'],
            epochs_pre=config['epochs_pre'],
            #expid=wildcards.subdir,
            kernel_size=config['kernel_size'],
            layer_size=config['layer_size'],
            scale_y=config['scale_y'],
            learning_rate=config['learning_rate']
        )

rule predict:
    input:
        "{prep_dir}/prepped.npz",
        "{prep_dir}/{run_dir}/weights_best_val.pth",
    output:
        "{prep_dir}/{run_dir}/prepped_preds.npz",
    #group: "{subdir}_{ensemble}_train"
    resources:
        nvidia_gpu=1
    threads: 4
    #params: run_dir = get_out_dir
    run:
        ppu.predict(input[0],
            wildcards.get_out_dir,
            batch_size=config['batch_size'],
            #expid=wildcards.subdir,
            kernel_size=config['kernel_size'],
            layer_size=config['layer_size'],
            scale_y=config['scale_y'],
            learning_rate=config['learning_rate'],
        )

rule calc_ci:
    input:
        "{prep_dir}/prepped.npz",
        "{prep_dir}/{run_dir}/prepped_preds.npz",
    output:
        "{prep_dir}/{run_dir}/conf_ints.npz",
    resources:
        nvidia_gpu=1
    threads: 4
    #group: "{subdir}_{ensemble}_train"
    #params: run_dir=get_out_dir
    run:
        ppu.calc_uq(
            wildcards.get_out_dir,
            input[0],
            input[1],
            quantile=config['uq_quant']
        )


rule combine_outputs:
    input:
        "{prep_dir}/prepped.npz",
        "{prep_dir}/{run_dir}/prepped_preds.npz",
        "{prep_dir}/{run_dir}/conf_ints.npz",
    output:
        "{prep_dir}/{run_dir}/combined_results.csv"
    #group: "{subdir}_eval"
    threads: 2
    run:
        ppu.combine_outputs(f"{wildcards.prep_dir/wildcards.run_dir}",
            input[0],
            input[1],
            input[2],
            config['scale_y'],
            config['clean_prepped'])


def get_grp_arg(wildcards):
    if wildcards.metric_type == 'overall':
        return None
    elif wildcards.metric_type == 'month':
        return 'month'
    elif wildcards.metric_type == 'reach':
        return 'seg_id_nat'


rule group_metrics:
    input:
        "{prep_dir}/{run_dir}/combined_results.csv"
    output:
        "{prep_dir}/{run_dir}/{metric_type}_metrics.csv"
    #group: "{subdir}_eval"
    params:
        grp_arg=get_grp_arg
    run:
        ppu.partition_metrics(input[0],
            group=params.grp_arg,
            outfile=output[0])

rule viz:
    input:
        "{prep_dir}/{run_dir}/adjmat_out.csv",
        "{prep_dir}/{run_dir}/combined_results.csv",
        "{prep_dir}/{run_dir}/train_log.csv",
    output:
        "{prep_dir}/{run_dir}/figs/training_loss.png",
    #group: "{subdir}_eval"
    threads: 2
    run:
        plot_results(f"{wildcards.prep_dir/wildcards.run_dir}")
