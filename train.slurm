#!/bin/sh

#SBATCH --account=watertemp
#SBATCH -p gpu
#SBATCH --time=2-00:00:00
#SBATCH --job-name=train_gwn_drb_30_10_rmse
#SBATCH -o train_full_30_10_rmse.out
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --mail-type=NONE
#SBATCH --mail-user=stopp@usgs.gov
#SBATCH --hint=compute_bound
#SBATCH --gres=gpu:1                     # number of GPU's you are requesting

# load analytics and cuda modules
module load analytics cuda10.0/toolkit/10.0.130

#  

# Generate training data and train the model
run_training -e gwn --no-node-list "python train_drb.py --data data/DRB_30_10 --adjdata data/adj_mat/adj_mx_full.pkl --batch_size 15 --seq_length 30 --out_dim 3 --epochs_pre 30 --epochs 70 --expid full_30_10Rmse --adjtype transition --gcn_bool --addaptadj --print_every 10 --kernel_size 4 --layer_size 2 --offset .1"