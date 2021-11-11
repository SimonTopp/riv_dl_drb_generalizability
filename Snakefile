#from snakemake.io import expand

configfile: "config.yml"
from gwn.generate_training_data_drb import prep_data
from gwn.viz import plot_results
from gwn.train_drb import train
import gwn.post_proc_utils as ppu
from gwn.util import asRunConfig

subdirs = [f"repeat_{i}" for i in range(10)]

rule all:
    input:
        expand("{outdir}/{subdir}/{metric_type}_metrics.csv",
            outdir=config["out_dir"],
            subdir=subdirs,
            metric_type=['overall', 'month', 'reach']),
        expand("{outdir}/asRunConfig.yml", outdir = config['out_dir']),
        expand("{outdir}/{subdir}/figs/training_loss.png",
            outdir=config["out_dir"],
            subdir=config['subdir'])


rule as_run_config:
    output:
        "{outdir}/{subdir}/asRunConfig.yml"
    run:
        asRunConfig(config,output[0])


def get_llo_arg(wildcards):
    if wildcards.subdir == 'LLO1':
        return 1
    elif wildcards.subdir == 'LLO2':
        return 2
    elif wildcards.subdir == 'LLO3':
        return 3

rule prep_io_data:
    input:
         config['obs_temp'],
         config['sntemp_file'],
    output:
        "{outdir}/prepped.npz",
    run:
        prep_data(input[0], input[1],
            x_vars=config['x_vars'],
            train_start_date=config['train_start_date'],
            train_end_date=config['train_end_date'],
            val_start_date=config['val_start_date'],
            val_end_date=config['val_end_date'],
            test_start_date=config['test_start_date'],
            test_end_date=config['test_end_date'],
            seq_length= config['seq_length'],
            offset= config['offset'],
            out_file= output[0], #out_dir + "/prepped.npz",
            normalize_y=config['scale_y'],
            distfile=config['dist_file'],
            dist_idx_name="rowcolnames",
            dist_type=config['dist_type'],
            clip_y=config['clip_y'],
            lto = config['lto'],
            lto_type = config['lto_type'],
            llo=config['llo'],
            test_group=config['test_group'],
        )

rule train:
    input:
        "{outdir}/prepped.npz",
    output:
        "{outdir}/{subdir}/adjmat_out.csv",
        #config['out_dir'] + "/{seq_length}_{offset}/{kernel_size}_{layer_size}/adjmat_pre_out.csv",
        "{outdir}/{subdir}/train_log.csv",
        "{outdir}/{subdir}/weights_best_val.pth",
    resources:
        nvidia_gpu=1
    run:
        train(input[0],
            config['out_dir'],
            batch_size=config['batch_size'],
            epochs=config['epochs'],
            epochs_pre=config['epochs_pre'],
            expid= wildcards.subdir,
            kernel_size= config['kernel_size'],
            layer_size= config['layer_size'],
            scale_y=config['scale_y'],
            learning_rate=config['learning_rate']
            )

rule predict:
    input:
        "{outdir}/prepped.npz",
        "{outdir}/{subdir}/weights_best_val.pth",
    output:
        "{outdir}/{subdir}/prepped_preds.npz",
    resources:
        nvidia_gpu=1
    run:
        ppu.predict(input[0],
            config['out_dir'],
            batch_size=config['batch_size'],
            expid= wildcards.subdir,
            kernel_size=config['kernel_size'],
            layer_size=config['layer_size'],
            scale_y=config['scale_y'],
            learning_rate=config['learning_rate'],
            )

rule calc_ci:
    input:
        "{outdir}/prepped.npz",
        "{outdir}/{subdir}/prepped_preds.npz",
    output:
        "{outdir}/{subdir}/conf_ints.npz",
    resources:
        nvidia_gpu=1
    run:
        ppu.calc_uq(
            f"{wildcards.outdir}/{wildcards.subdir}",
            input[0],
            input[1],
            quantile = config['uq_quant']
        )

rule combine_outputs:
    input:
        "{outdir}/prepped.npz",
        "{outdir}/{subdir}/prepped_preds.npz",
        "{outdir}/{subdir}/conf_ints.npz",
    output:
        "{outdir}/{subdir}/combined_results.csv"
    run:
        ppu.combine_outputs(f"{wildcards.outdir}/{wildcards.subdir}",
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
        "{outdir}/{subdir}/combined_results.csv"
    output:
         "{outdir}/{subdir}/{metric_type}_metrics.csv"
    params:
        grp_arg = get_grp_arg
    run:
        ppu.partition_metrics(input[0],
                         group=params.grp_arg,
                         outfile=output[0])

rule viz:
    input:
        "{outdir}/{subdir}/adjmat_out.csv",
        "{outdir}/{subdir}/combined_results.csv",
        "{outdir}/{subdir}/train_log.csv",
    output:
        "{outdir}/{subdir}/figs/training_loss.png",
    run:
        plot_results(f"{wildcards.outdir}/{wildcards.subdir}")
