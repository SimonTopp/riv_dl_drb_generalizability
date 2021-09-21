configfile: "config.yml"
from gwn.generate_training_data_drb import prep_data
from gwn.test_drb import plot_results
from gwn.train_drb import train
from gwn.predict import predict

rule all:
    input:
        expand("{outdir}/{seq_length}_{offset}/figs/training_loss.png",
            outdir=config["out_dir"],
            seq_length=config["seq_length"],
            offset=config['offset']),
        expand("{outdir}/{seq_length}_{offset}/config.yml",
            outdir=config["out_dir"],
            seq_length=config["seq_length"],
            offset=config['offset']),

rule copy_config:
    output:
        config['out_dir']+"/{seq_length}_{offset}/config.yml"
    shell:
        """
        scp config.yml {output[0]}
        """

rule prep_io_data:
    input:
         config['obs_temp'],
         config['obs_flow'],
         config['sntemp_file'],
    output:
        config['out_dir']+"/{seq_length}_{offset}/prepped.npz",
    run:
        prep_data(input[0], input[1], input[2],
            x_vars=config['x_vars'],
            y_vars=config['y_vars'],
            train_start_date=config['train_start_date'],
            train_end_date=config['train_end_date'],
            val_start_date=config['val_start_date'],
            val_end_date=config['val_end_date'],
            test_start_date=config['test_start_date'],
            test_end_date=config['test_end_date'],
            primary_variable=config['primary_variable'],
            seq_length= int(wildcards.seq_length), #config['seq_length'],
            offset= float(wildcards.offset), #config['offset'],
            out_file= output[0], #out_dir + "/prepped.npz",
            normalize_y=config['scale_y'],
            distfile=config['dist_file'],
            dist_idx_name="rowcolnames",
            dist_type=config['dist_type'],
        )

rule train:
    input:
        config['out_dir'] + "/{seq_length}_{offset}/prepped.npz",
    output:
        config['out_dir'] + "/{seq_length}_{offset}/adjmat_out.csv",
        config['out_dir'] + "/{seq_length}_{offset}/adjmat_pre_out.csv",
        config['out_dir'] + "/{seq_length}_{offset}/train_log.csv",
        config['out_dir'] + "/{seq_length}_{offset}/weights_final.pth",

    run:
        train(input[0],
            config['out_dir'],
            batch_size=config['batch_size'],
            epochs=config['epochs'],
            epochs_pre=config['epochs_pre'],
            expid= str(wildcards.seq_length)+"_"+str(wildcards.offset),
            kernel_size=config['kernel_size'],
            layer_size=config['layer_size'],
            clean_prepped=config['clean_prepped'],
            scale_y=config['scale_y'],
            learning_rate=config['learning_rate']
            )

rule predict:
    input:
        config['out_dir'] + "/{seq_length}_{offset}/prepped.npz",
        config['out_dir'] + "/{seq_length}_{offset}/weights_final.pth",
    output:
        config['out_dir'] + "/{seq_length}_{offset}/test_results.csv",
    run:
        predict(input[0],
            config['out_dir'],
            batch_size=config['batch_size'],
            expid= str(wildcards.seq_length)+"_"+str(wildcards.offset),
            kernel_size=config['kernel_size'],
            layer_size=config['layer_size'],
            clean_prepped=config['clean_prepped'],
            scale_y=config['scale_y'],
            learning_rate=config['learning_rate']
            )

rule viz:
    input:
        config['out_dir'] + "/{seq_length}_{offset}/adjmat_out.csv",
        config['out_dir'] + "/{seq_length}_{offset}/adjmat_pre_out.csv",
        config['out_dir'] + "/{seq_length}_{offset}/test_results.csv",
        config['out_dir'] + "/{seq_length}_{offset}/train_log.csv",
    output:
        config['out_dir']+"/{seq_length}_{offset}/figs/training_loss.png",
    run:
        plot_results(config['out_dir']+"/"+str(wildcards.seq_length)+"_"+str(wildcards.offset))

