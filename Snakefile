import os
configfile: "config.yml"

from gwn.generate_training_data_drb import prep_data
from gwn.test_drb import plot_results
from gwn.train_drb import train

out_dir = config['out_dir']
expid = config['expid']

sequences = [30,60,90,120,180,365]
offsets = [1,.1]

out_dir = out_dir + '/' + expid

rule all:
    input:
        directory(out_dir+"/figs")#/training_loss.png"

rule prep_io_data:
    input:
         config['obs_temp'],
         config['obs_flow'],
         config['sntemp_file'],
        # config['dist_matrix'],
    output:
        out_dir+"/prepped.npz"
    run:
        prep_data(input[0], input[1], input[2],
            x_vars=config['x_vars'],
            train_start_date=config['train_start_date'],
            train_end_date=config['train_end_date'],
            val_start_date=config['val_start_date'],
            val_end_date=config['val_end_date'],
            test_start_date=config['test_start_date'],
            test_end_date=config['test_end_date'],
            primary_variable=config['primary_variable'],
            seq_length=config['seq_length'],
            offset=config['offset'],
            out_file=out_dir + "/prepped.npz",
            normalize_y=config['scale_y']
        )

rule train:
    input:
        out_dir+"/prepped.npz",
        adjdata= config['adjdata'],
    output:
        out_dir+"/adjmat_out.csv",
        out_dir+"/adjmat_pre_out.csv",
        out_dir+"/test_results.csv",
        out_dir+"/train_log.csv",

    run:
        train(input[0],
            input[1],
            config['out_dir'],
            batch_size=config['batch_size'],
            epochs=config['epochs'],
            epochs_pre=config['epochs_pre'],
            expid=config['expid'],
            kernel_size=config['kernel_size'],
            layer_size=config['layer_size'],
            clean_prepped=config['clean_prepped'],
            scale_y=config['scale_y']
            )

rule viz:
    input:
        out_dir+"/adjmat_out.csv",
        out_dir+"/adjmat_pre_out.csv",
        out_dir+"/test_results.csv",
        out_dir+"/train_log.csv",
    output:
        out_dir+"/figs/training_loss.png"
    run:
        plot_results(out_dir)

