### Utilities for post-processing PGDL outputs.
### Some of these functions have been taken from the River-dl repository led by Jeff Sadler
## https://github.com/USGS-R/river-dl

import time
import torch
import numpy as np
import os.path
import pandas as pd
import gwn.util as util
from torch import nn
import torch.optim as optim



def predict(data_in,
            out_dir,
            batch_size=20,
            expid='default',
            kernel_size=3,
            layer_size=3,
            learning_rate=0.001,
            randomadj=False,
            n_blocks=4,
            scale_y=False,
            ):
    out_dir = os.path.join(out_dir, expid)
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

    data, dataloader, engine = util.load_model(data_in,
                                               out_dir,
                                               batch_size,
                                               kernel_size=kernel_size,
                                               layer_size=layer_size,
                                               learning_rate=learning_rate,
                                               randomadj=randomadj,
                                               n_blocks=n_blocks,
                                               scale_y=scale_y,
                                               load_weights=True,
                                               pad=True)

    #scaler = dataloader["scaler"]
    #out_dim = data['y_train'].shape[1]

    preds = {}
    preds['preds_pre_train'] = engine.predict('pre_train', dataloader,device)[:dataloader['y_pre_train'].shape[0],...].detach().cpu().numpy()
    preds['preds_train'] = engine.predict('train', dataloader, device)[:dataloader['y_train'].shape[0],...].detach().cpu().numpy()
    preds['preds_val'] = engine.predict('val', dataloader, device)[:dataloader['y_val'].shape[0],...].detach().cpu().numpy()
    preds['preds_test'] = engine.predict('test', dataloader,device)[:dataloader['y_test'].shape[0],...].detach().cpu().numpy()


    np.savez_compressed(os.path.join(out_dir, 'prepped_preds.npz'), **preds)

    return data

##############
#### UQ Utilities following Lu et al (in review)
##############

def flatten_preds(preds):
    return preds.reshape(-1,1)

def flatten_drivers(drivers, in_dim, out_dim, clip_x = False):
    if clip_x:
        drivers = drivers[:,-out_dim:,...]
    return drivers.reshape(-1,in_dim)

def flatten_obs(obs,out_dim):
    obs = obs[:,-out_dim:,...]
    return obs.reshape(-1,1)

def prep_uq_data(x,y, y_hat):
    in_dim = x.shape[3]
    out_dim = y_hat.shape[1]
    x = flatten_drivers(x, in_dim,out_dim,clip_x=True)
    y_hat= flatten_preds(y_hat).squeeze()
    y = flatten_obs(y,out_dim).squeeze()
    residuals = y-y_hat
    x_up = x[residuals > 0]
    x_down = x[residuals < 0]
    y_up = residuals[residuals>0]
    y_down = -1.0 * residuals[residuals<0]
    return x_up,x_down,x, y_up, y_down, y, y_hat


class UQ_Net_std(nn.Module):

    def __init__(self,len_x):
        super(UQ_Net_std, self).__init__()
        self.fc1 = nn.Linear(len_x, 200)
        self.fc2 = nn.Linear(200, 1)
        self.fc2.bias = torch.nn.Parameter(torch.tensor([10.0]))


    def forward(self, x):
        x = torch.relu(self.fc1(x))
        x = torch.sqrt(torch.square(self.fc2(x)) + 1e-10)

        return x

    def UQ_loss(self, x, output, ydata):

        loss = torch.mean((output[:,0] - ydata[:,0])**2)

        return loss


def load_data_uq(cat_data,
                 predictions,
                 batch_size
                 ):

    if isinstance(cat_data,str):
        cat_data = np.load(os.path.join(cat_data, 'prepped.npz'))
    if isinstance(predictions,str):
        predictions = np.load(os.path.join(predictions))

    scaler = util.StandardScaler(mean=cat_data['y_mean'][0], std=cat_data['y_std'][0])

    data = {}
    categories = ['pre_train','train','val','test']
    for cat in categories:
        x_up, x_down, x, y_up, y_down,y, y_hat  = prep_uq_data(cat_data[f'x_{cat}'],
                                                  cat_data[f'y_{cat}'],
                                                  predictions[f'preds_{cat}'])
        data[f'{cat}_up_loader'] = util.DataLoader(x_up, y_up, batch_size)
        data[f'{cat}_down_loader'] = util.DataLoader(x_down,y_down,batch_size)
        data[f'x_{cat}'] = x
        data[f'y_{cat}'] = y
        data[f'y_hat_{cat}'] = y_hat
        data['scaler'] = scaler

    return data

def train_uq(out_dir,
             model,
             ci_bound,
             dataloader,
             epochs_pre,
             epochs,
             weights=None,
             early_stopping=20,
             ):

    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

    if weights:
        model.load_state_dict(weights)

    criterion = nn.MSELoss()
    optimizer = optim.SGD(model.parameters(), lr=0.01)
    ## Pretraining
    best_mse = 1000 #will get overwritten
    model.train()
    pre_train_mse=[]
    t1 = time.time()
    dataloader[f"pre_train_{ci_bound}_loader"].shuffle()
    for i in range(1,epochs_pre+1):
        for iter, (x, y) in enumerate(dataloader[f'pre_train_{ci_bound}_loader'].get_iterator()):
            optimizer.zero_grad()
            trainx = torch.Tensor(x).to(device)
            trainy = torch.Tensor(y).to(device)
            output = model(trainx)
            loss = criterion(output, trainy.unsqueeze(1))
            if torch.isnan(loss):
                print(output, trainy)
                exit()
            loss.backward()
            optimizer.step()
            pre_train_mse.append(loss.item())
        if i%15 == 0:
            print(f'PT Epoch {i}: loss {np.mean(pre_train_mse):0.4f}')
    t2 = time.time()
    print(f"Total pretrain time: {t2-t1}")

    for i in range(1, epochs + 1):
        train_mse = []
        dataloader[f'train_{ci_bound}_loader'].shuffle()
        for iter, (x, y) in enumerate(dataloader[f'train_{ci_bound}_loader'].get_iterator()):
            optimizer.zero_grad()
            trainx = torch.Tensor(x).to(device)
            trainy = torch.Tensor(y).to(device)
            output = model(trainx)
            loss = criterion(output, trainy.unsqueeze(1))
            if torch.isnan(loss):
                print(output, trainy)
                exit()
            loss.backward()
            optimizer.step()
            train_mse.append(loss.item())

        valid_mse = []
        model.eval()
        for iter, (x, y) in enumerate(dataloader[f'val_{ci_bound}_loader'].get_iterator()):
            testx = torch.Tensor(x).to(device)
            testy = torch.Tensor(y).to(device)
            output = model(testx)
            loss = criterion(output, testy.unsqueeze(1))
            valid_mse.append(loss.item())

        mtrain_mse = np.mean(train_mse)
        mvalid_mse = np.mean(valid_mse)
        if i%15 == 0:
            log = 'Epoch: {:03d}, Train MSE: {:.4f}, Valid MSE: {:.4f}'
            print(log.format(i,mtrain_mse, mvalid_mse),flush=True)

        if mvalid_mse < best_mse:
            torch.save(model.state_dict(), os.path.join(out_dir,f"weights_uq_{ci_bound}.pth"))
            best_mse = mvalid_mse
            epochs_since_best = 0
        else:
            epochs_since_best += 1
        if epochs_since_best > early_stopping:
            print(f"Early Stopping at Epoch {i}")
            break

    return model

def calc_uq(out_dir,
            prepped,
            preds,
            quantile=0.90):
    if isinstance(prepped, str):
        prepped = np.load(prepped)

    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    len_x=prepped['x_train'].shape[3]

    dataloader = load_data_uq(prepped, preds, 500)

    net_up = UQ_Net_std(len_x).to(device)

    net_up = train_uq(out_dir, net_up, 'up', dataloader, 50, 200)

    net_down = UQ_Net_std(len_x).to(device)

    net_down = train_uq(out_dir, net_down, 'down', dataloader, 50, 200)

    # ------------------------------------------------------
    # Determine how to move the upper and lower bounds


    ytrain = dataloader['y_train']
    output = torch.Tensor(dataloader['y_hat_train'][~np.isnan(ytrain)]).to(device).unsqueeze(1)
    x_train = dataloader['x_train'][~np.isnan(ytrain)]
    x_train = torch.Tensor(x_train).to(device)
    ytrain = torch.Tensor(ytrain[~np.isnan(ytrain)]).to(device).unsqueeze(1)

    num_outlier = int(ytrain.shape[0] * (1 - quantile) / 2)

    net_up.eval()
    output_up = net_up(x_train)
    net_down.eval()
    output_down = net_down(x_train)

    c_up0 = 0.0
    c_up1 = 10.0

    f0 = (ytrain >= output + c_up0 * output_up).sum() - num_outlier
    f1 = (ytrain >= output + c_up1 * output_up).sum() - num_outlier

    n_iter = 1000
    iter = 0
    while iter <= n_iter and f0 * f1 < 0:  ##f0 != 0 and f1 != 0:

        c_up2 = (c_up0 + c_up1) / 2.0
        ##Count the number of obs > the upper bound
        f2 = (ytrain >= output + c_up2 * output_up).sum() - num_outlier

        if f2 == 0:
            break
        elif f2 > 0:
            c_up0 = c_up2
            f0 = f2
        else:
            c_up1 = c_up2
            f1 = f2
        print('{}, f0: {}, f1: {}, f2: {}'.format(iter, f0, f1, f2))

    c_up = c_up2

    c_down0 = 0.0
    c_down1 = 10.0

    f0 = (ytrain <= output - c_down0 * output_down).sum() - num_outlier
    f1 = (ytrain <= output - c_down1 * output_down).sum() - num_outlier

    n_iter = 1000
    iter = 0

    while iter <= n_iter and f0 * f1 < 0:  ##f0 != 0 and f1 != 0:

        c_down2 = (c_down0 + c_down1) / 2.0
        f2 = (ytrain <= output - c_down2 * output_down).sum() - num_outlier

        if f2 == 0:
            break
        elif f2 > 0:
            c_down0 = c_down2
            f0 = f2
        else:
            c_down1 = c_down2
            f1 = f2
        print('{}, f0: {}, f1: {}, f2: {}'.format(iter, f0, f1, f2))
    c_down = c_down2

    x_test = torch.Tensor(dataloader['x_test']).to(device)
    y_hat_test = dataloader['y_hat_test']

    x_val = torch.Tensor(dataloader['x_val']).to(device)
    y_hat_val = dataloader['y_hat_val']


    y_up_test = net_up(x_test).detach().cpu().numpy().squeeze()
    y_down_test = net_down(x_test).detach().cpu().numpy().squeeze()


    y_up_val = net_up(x_val).detach().cpu().numpy().squeeze()
    y_down_val = net_down(x_val).detach().cpu().numpy().squeeze()


    ci_out = {
        "ci_low_test": y_hat_test - c_down * y_down_test,
        "ci_high_test": y_hat_test + c_up * y_up_test,
        "ci_low_val": y_hat_val - c_down * y_down_val,
        "ci_high_val": y_hat_val + c_up * y_up_val
    }
    outfile = os.path.join(out_dir,'conf_ints.npz')
    np.savez_compressed(outfile, **ci_out)

    return ci_out


def combine_outputs(out_dir, io_data, preds, ci, unscale = True, clean_prepped = False):

    prepped = np.load(io_data)
    predictions = np.load(preds)
    conf_intervals = np.load(ci)
    out_dim = predictions['preds_val'].shape[1]
    scaler = util.StandardScaler(mean = prepped['y_mean'][0], std = prepped['y_std'][0])

    categories = ['test', 'val']
    df_out = pd.DataFrame(columns=['date','seg_id_nat','observed','predicted', 'ci_low','ci_high','partition'])
    for cat in categories:
        d = {
        "date": prepped[f'dates_{cat}'][:,-out_dim:,...].flatten(),
        "seg_id_nat": prepped[f'ids_{cat}'][:,-out_dim:,...].flatten(),
        "observed":prepped[f'y_{cat}'].flatten(),
        "predicted":predictions[f'preds_{cat}'].flatten(),
        "ci_low":conf_intervals[f'ci_low_{cat}'],
        "ci_high":conf_intervals[f'ci_high_{cat}'],
        }
        df = pd.DataFrame(data = d)
        df['partition'] = cat
        df_out = pd.concat([df_out,df])
    df_out.set_index('partition', inplace=True)
    if unscale:
        df_out[['observed','predicted', 'ci_low','ci_high']] = df_out[['observed','predicted', 'ci_low','ci_high']].apply(lambda x: scaler.inverse_transform(x))

    if clean_prepped:
        os.remove(io_data)
        os.remove(preds)
        os.remove(ci)
    df_out = df_out.round(5)
    df_out.to_csv(os.path.join(out_dir,'combined_results.csv'))

    return df_out
'''
def monthly_summary(df, partition):
    df.
'''
## Some of these functions have

def nse(y_true, y_pred):
    y_pred = y_pred[~np.isnan(y_true)]
    y_true = y_true[~np.isnan(y_true)]

    mean = np.nanmean(y_true)
    deviation = y_true - mean
    error = y_pred-y_true
    numerator = np.sum(np.square(error))
    denominator = np.sum(np.square(deviation))
    return 1 - numerator / denominator


def rmse_eval(y_true, y_pred):
    y_pred = y_pred[~np.isnan(y_true)]
    y_true = y_true[~np.isnan(y_true)]
    n = len(y_true)
    sum_squared_error = np.sum(np.square(y_pred-y_true))
    rmse = np.sqrt(sum_squared_error/n)
    return rmse


def piw(ci_high, ci_low):
    piw = ci_high - ci_low
    return np.mean(piw)

def picp(ci_high,ci_low, y_true):
    ci_low = ci_low[~np.isnan(y_true)]
    ci_high = ci_high[~np.isnan(y_true)]
    y_true = y_true[~np.isnan(y_true)]
    n_in = len(y_true[(y_true > ci_low) & (y_true < ci_high)])
    return n_in/len(y_true)


def calc_metrics(df):
    """
    calculate metrics (e.g., rmse and nse)
    :param df:[pd dataframe] dataframe of observations and predictions for
    one reach. dataframe must have columns "obs" and "pred"
    :return: [pd Series] various evaluation metrics (e.g., rmse and nse)
    """

    obs = df["observed"].values
    pred = df["predicted"].values
    ci_high = df['ci_high'].values
    ci_low = df['ci_low'].values
    if len(obs[~np.isnan(obs)]) > 10:
        metrics = {
            "rmse": rmse_eval(obs, pred),
            "nse": nse(obs, pred),
            'piw': piw(ci_high,ci_low),
            "picp": picp(ci_high, ci_low,obs)
        }
    else:
        metrics = {
            "rmse": np.nan,
            "nse": np.nan,
            "piw":np.nan,
            "picp": np.nan
        }
    return pd.Series(metrics)


def partition_metrics(
        df,
        spatial_idx_name="seg_id_nat",
        time_idx_name="date",
        group=None,
        outfile=None
):
    """
    calculate metrics for a certain group (or no group at all) for a given
    partition and variable
    :param pred_file: [str] path to predictions feather file
    :param obs_file: [str] path to observations zarr file
    :param partition: [str] data partition for which metrics are calculated
    :param spatial_idx_name: [str] name of column that is used for spatial
        index (e.g., 'seg_id_nat')
    :param time_idx_name: [str] name of column that is used for temporal index
        (usually 'time')
    :param group: [str or list] which group the metrics should be computed for.
    Currently only supports 'seg_id_nat' (segment-wise metrics), 'month'
    (month-wise metrics), ['seg_id_nat', 'month'] (metrics broken out by segment
    and month), and None (everything is left together)
    :param outfile: [str] file where the metrics should be written
    :return: [pd dataframe] the condensed metrics
    """
    if isinstance(df,str):
        df = pd.read_csv(df).set_index('partition')
        df.date = pd.to_datetime(df.date)
    var_metrics_list = []
    for i in ['test','val']:
        data = df.loc[i,]
        if not group:
            metrics_overall = calc_metrics(data)
            metrics_overall['group'] = 'Overall'
            temps_high = calc_metrics(data.loc[data.observed > 20])
            temps_high['group'] = 'Above20'
            temps_low = calc_metrics(data.loc[data.observed < 10])
            temps_low['group'] = 'Below10'
            # need to convert to dataframe and transpose so it looks like the
            # others
            metrics = pd.concat(pd.DataFrame(i).T for i in [metrics_overall, temps_low, temps_high])
        elif group == "seg_id_nat":
            metrics = data.groupby(spatial_idx_name).apply(calc_metrics).reset_index()
        elif group == "month":
            metrics = (
            data.groupby(data[time_idx_name].dt.month).apply(calc_metrics).reset_index()
            )
        else:
            raise ValueError("group value not valid")
        metrics["partition"] = i
        var_metrics_list.append(metrics)
    var_metrics = pd.concat(var_metrics_list).round(5)
    if outfile:
        var_metrics.to_csv(outfile, header=True, index=False)
    return var_metrics