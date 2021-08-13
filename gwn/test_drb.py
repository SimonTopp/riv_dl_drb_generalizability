import os.path
import argparse
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import cm
import seaborn as sns

parser = argparse.ArgumentParser()
parser.add_argument('--out_dir',type=str,default='../data/out',help='data path')
parser.add_argument('--expid',type=str,default='default',help='experiment id')

args = parser.parse_args(['--expid', 'FigTest'])

out_dir = os.path.join(args.out_dir, args.expid)
os.makedirs(os.path.join(out_dir,'figs'), exist_ok=True)

df = pd.read_csv(os.path.join(out_dir,'adjmat_out.csv'))
plt.figure()
sns.heatmap(df, cmap = cm.get_cmap('viridis'))
plt.savefig(os.path.join(out_dir,'figs/adjMat.png'))

test_df = pd.read_csv(os.path.join(out_dir, 'test_results.csv'))

counts = test_df.dropna().groupby('seg_id_nat').size()
filt = counts[counts > 1000].index.tolist()[:3]
test_df_filt = test_df[test_df.seg_id_nat.isin(filt)]

df_piv = test_df_filt.melt(id_vars=['date','seg_id_nat']).dropna()

plt.figure()
fg = sns.FacetGrid(data = df_piv,row='seg_id_nat',hue='variable',aspect=3)
fg.map(plt.scatter,'date','value', alpha=.2,s=2).set_xticklabels([]).add_legend()
fg.savefig(os.path.join(out_dir,'figs/ExampleTS.png'))


def rmse(predictions, targets):
    return np.sqrt(((predictions - targets) ** 2).mean())

overall = rmse(test_df.temp_ob, test_df.temp_pred)

rmses = []
for i in np.unique(test_df.seg_id_nat):
    df = test_df[test_df.seg_id_nat == i]
    rmse_seg = rmse(df.temp_ob, df.temp_pred)
    rmses.append(rmse_seg)

plt.figure()
plt.hist(rmses, bins='auto')  # arguments are passed to np.histogram
plt.title(f"Distribution of Reach Scale RMSE: Overall- {overall:.3f}")
plt.savefig(os.path.join(out_dir,'figs/rmse_dist.png'))

## Train log
log = pd.read_csv(os.path.join(out_dir,'train_log.csv'))
tTrain = np.mean(log.time[log.split == 'train'])
tVal = np.mean(log.time[log.split=='val'])
plt.figure()
sns.lineplot(data= log, x='epoch', y='rmse',hue='split').set(title = f"Time Train: {tTrain:.3f}, Time Infr: {tVal:.3f}")
plt.savefig(os.path.join(out_dir,'figs/training_loss.png'))
