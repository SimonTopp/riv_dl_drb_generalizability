import os.path
import argparse
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import cm
import seaborn as sns

def plot_adj(out_dir,adj_mat, outfile):
    df = pd.read_csv(os.path.join(out_dir,adj_mat))
    sns.heatmap(df, cmap = cm.get_cmap('viridis'))
    plt.savefig(os.path.join(out_dir,outfile))
    plt.close()


def plot_ts(out_dir):
    test_df = pd.read_csv(os.path.join(out_dir, 'test_results.csv'))
    counts = test_df.dropna().groupby('seg_id_nat').size()
    filt = counts[counts > 1000].index.tolist()[:4]
    test_df_filt = test_df[test_df.seg_id_nat.isin(filt)]
    df_piv = test_df_filt.melt(id_vars=['date','seg_id_nat']).dropna()
    df_piv['date']= pd.to_datetime(df_piv['date'])
    fg = sns.FacetGrid(data = df_piv,row='seg_id_nat',hue='variable',aspect=3, sharex=False)
    fg.map(plt.scatter,'date','value', alpha=.2,s=2).add_legend()
    fg.savefig(os.path.join(out_dir,'figs/ExampleTS.png'))
    plt.close()


def rmse(predictions, targets):
    return np.sqrt(((predictions - targets) ** 2).mean())

def plot_rmse_dist(out_dir):
    test_df = pd.read_csv(os.path.join(out_dir, 'test_results.csv'))
    overall = rmse(test_df.temp_ob, test_df.temp_pred)

    rmses = []
    for i in np.unique(test_df.seg_id_nat):
        df = test_df[test_df.seg_id_nat == i]
        rmse_seg = rmse(df.temp_ob, df.temp_pred)
        rmses.append(rmse_seg)

    plt.hist(rmses, bins='auto')  # arguments are passed to np.histogram
    plt.title(f"Distribution of Reach Scale RMSE: Overall- {overall:.3f}")
    plt.savefig(os.path.join(out_dir,'figs/rmse_dist.png'))
    plt.close()

def plot_train_log(out_dir): ## Train log
    log = pd.read_csv(os.path.join(out_dir,'train_log.csv'))
    tTrain = np.mean(log.time[log.split == 'train'])
    tVal = np.mean(log.time[log.split=='val'])
    sns.lineplot(data= log, x='epoch', y='rmse',hue='split').set(title = f"Time Train: {tTrain:.3f}, Time Infr: {tVal:.3f}")
    plt.savefig(os.path.join(out_dir,'figs/training_loss.png'))
    plt.close()


def plot_results(out_dir):
    os.makedirs(os.path.join(out_dir, 'figs'), exist_ok=True)
    plot_adj(out_dir,'adjmat_out.csv', 'figs/adj_mat_ft.png')
    plot_adj(out_dir, 'adjmat_pre_out.csv', 'figs/adj_mat_pre.png')
    plot_ts(out_dir)
    plot_rmse_dist(out_dir)
    plot_train_log(out_dir)
