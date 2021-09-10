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
    viridis = cm.get_cmap('viridis')
    test_df = pd.read_csv(os.path.join(out_dir, 'test_results.csv'))
    test_df['date'] = pd.to_datetime(test_df['date'])
    test_df = test_df.set_index('date').loc['2011-10-01':'2019-09-30']
    counts = test_df.dropna().groupby('seg_id_nat').size()
    filt = counts[counts > 1000].index.tolist()[:4]
    test_df_filt = test_df[test_df.seg_id_nat.isin(filt)].reset_index()
    test_df_filt['ci_low'] = np.where(test_df_filt['ci_low'] < 0,0,test_df_filt['ci_low'])
    fg = sns.FacetGrid(data=test_df_filt, row='seg_id_nat', aspect=3, sharex=False, sharey=False)
    fg.map(plt.scatter, 'date', 'temp_ob', alpha=.2, s=2, label='Observed', color=viridis(.3))
    fg.map(plt.scatter, 'date', 'temp_pred', alpha=.2, s=2, label='Predicted', color=viridis(.8))
    fg.map(plt.fill_between, 'date', 'ci_low', 'ci_high',
           alpha=0.4,
           edgecolor=None,
           color='#d3d3d3',
           linewidth=0,
           zorder=1,
           label="Uncertainty")
    fg.add_legend().set_axis_labels(y_var='Temp (°C)', x_var='Date')
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
