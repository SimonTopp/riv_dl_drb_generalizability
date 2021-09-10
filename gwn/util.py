import pickle
import numpy as np
import os
import scipy.sparse as sp
import torch
from scipy.sparse import linalg
import torch.nn as nn
import torch.optim as optim


class DataLoader(object):
    def __init__(self, xs, ys, batch_size, pad_with_last_sample=True):
        """
        :param xs:
        :param ys:
        :param batch_size:
        :param pad_with_last_sample: pad with the last sample to make number of samples divisible to batch_size.
        """
        self.batch_size = batch_size
        self.current_ind = 0
        if pad_with_last_sample:
            num_padding = (batch_size - (len(xs) % batch_size)) % batch_size
            x_padding = np.repeat(xs[-1:], num_padding, axis=0)
            y_padding = np.repeat(ys[-1:], num_padding, axis=0)
            xs = np.concatenate([xs, x_padding], axis=0)
            ys = np.concatenate([ys, y_padding], axis=0)
        self.size = len(xs)
        self.num_batch = int(self.size // self.batch_size)
        self.xs = xs
        self.ys = ys

    def shuffle(self):
        permutation = np.random.permutation(self.size)
        xs, ys = self.xs[permutation], self.ys[permutation]
        self.xs = xs
        self.ys = ys

    def get_iterator(self):
        self.current_ind = 0

        def _wrapper():
            while self.current_ind < self.num_batch:
                start_ind = self.batch_size * self.current_ind
                end_ind = min(self.size, self.batch_size * (self.current_ind + 1))
                x_i = self.xs[start_ind: end_ind, ...]
                y_i = self.ys[start_ind: end_ind, ...]
                yield (x_i, y_i)
                self.current_ind += 1

        return _wrapper()

class StandardScaler():
    """
    Standard the input
    """

    def __init__(self, mean, std):
        self.mean = mean
        self.std = std

    def transform(self, data):
        return (data - self.mean) / self.std

    def inverse_transform(self, data):
        return (data * self.std) + self.mean


def load_pickle(pickle_file):
    try:
        with open(pickle_file, 'rb') as f:
            pickle_data = pickle.load(f)
    except UnicodeDecodeError as e:
        with open(pickle_file, 'rb') as f:
            pickle_data = pickle.load(f, encoding='latin1')
    except Exception as e:
        print('Unable to load data ', pickle_file, ':', e)
        raise
    return pickle_data

def load_adj(pkl_filename, adjtype):
    sensor_ids, sensor_id_to_ind, adj = load_pickle(pkl_filename)
    '''
    if adjtype == "scalap":
        adj = [calculate_scaled_laplacian(adj_mx)]
    elif adjtype == "normlap":
        adj = [calculate_normalized_laplacian(adj_mx).astype(np.float32).todense()]
    elif adjtype == "symnadj":
        adj = [sym_adj(adj_mx)]
    elif adjtype == "transition":
        adj = [asym_adj(adj_mx)]
    elif adjtype == "doubletransition":
        adj = [asym_adj(adj_mx), asym_adj(np.transpose(adj_mx))]
    elif adjtype == "identity":
        adj = [np.diag(np.ones(adj_mx.shape[0])).astype(np.float32)]
    else:
        error = 0
        assert error, "adj type not defined"
    '''
    return sensor_ids, sensor_id_to_ind, adj


def load_dataset(cat_data, batch_size, valid_batch_size= None, test_batch_size=None):
    if isinstance(cat_data,str):
        cat_data = np.load(os.path.join(cat_data, 'prepped.npz'))
    data = {}
    for category in ['pre_train', 'train', 'val', 'test']:
        if category!='pre_train':
            data['y_' + category] = cat_data['y_'+ category]
            data['x_' + category] = cat_data['x_'+ category]
        if category=='pre_train':
            data['y_' + category] = cat_data['y_' + category]
            data['x_' + category] = cat_data['x_train']
    scaler = StandardScaler(mean = cat_data['y_mean'][cat_data['y_vars']=='seg_tave_water'], std = cat_data['y_std'][cat_data['y_vars']=='seg_tave_water'])
    # Data format
    #for category in ['pre_train', 'train', 'val', 'test']:
    #    data['x_' + category][..., 0] = scaler.transform(data['x_' + category][..., 0])
    data['pre_train_loader'] = DataLoader(data['x_pre_train'],data['y_pre_train'], batch_size)
    data['train_loader'] = DataLoader(data['x_train'], data['y_train'], batch_size)
    data['val_loader'] = DataLoader(data['x_val'], data['y_val'], valid_batch_size)
    data['test_loader'] = DataLoader(data['x_test'], data['y_test'], test_batch_size)
    data['scaler'] = scaler
    return data


def rmse(y_pred, y_true):
    num_y_true = torch.count_nonzero(~torch.isnan(y_true))
    if num_y_true > 0:
        zero_or_error = torch.where(
            torch.isnan(y_true), torch.zeros_like(y_true), y_pred - y_true
        )
        sum_squared_errors = torch.sum(torch.square(zero_or_error))
        rmse_loss = torch.sqrt(sum_squared_errors / num_y_true)
    else:
        rmse_loss = 0.0
    return rmse_loss


def mae(y_pred, y_true):
    num_y_true = torch.count_nonzero(~torch.isnan(y_true))
    if num_y_true > 0:
        zero_or_error = torch.where(
            torch.isnan(y_true), torch.zeros_like(y_true), y_pred - y_true
        )
        sum_errors = torch.sum(torch.abs(zero_or_error))
        mae_loss = sum_errors/num_y_true
    else:
        mae_loss = 0.0
    return mae_loss

def mape(y_pred, y_true):
    num_y_true = torch.count_nonzero(~torch.isnan(y_true))
    if num_y_true > 0:
        zero_or_error = torch.where(
            torch.isnan(y_true), torch.zeros_like(y_true), torch.abs((y_pred-y_true)/(y_true + 1E-5))
        )
        mape_loss = torch.sum(zero_or_error)/num_y_true
    else:
        mape_loss = 0.0
    return mape_loss

def metric(pred, real):
    masked_mae = mae(pred,real).item()
    masked_mape = mape(pred,real).item()
    masked_rmse = rmse(pred,real).item()
    return masked_mae,masked_mape,masked_rmse

## UQ Utilities following Lu et al (in review)

class UQ_Net_std(nn.Module):

    def __init__(self):
        super(UQ_Net_std, self).__init__()
        self.fc1 = nn.Linear(10, 200)
        self.fc2 = nn.Linear(200, 1)
        self.fc2.bias = torch.nn.Parameter(torch.tensor([10.0]))


    def forward(self, x):
        x = torch.relu(self.fc1(x))
        x = torch.sqrt(torch.square(self.fc2(x)) + 1e-10)

        return x

    def UQ_loss(self, x, output, ydata):

        loss = torch.mean((output[:,0] - ydata[:,0])**2)

        return loss


def calc_uq(xtrain,
            ytrain,
            y_pred_train,
            x_test,
            y_pred_test,
            scalar,
            scale_y = True,
            quantile=0.90):

    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    ##Flatten everythign
    len_x=xtrain.shape[3]

    x_test = x_test.swapaxes(1,2).reshape(-1,len_x)
    y_pred_test=y_pred_test.reshape(-1,1)
    y_pred_train=y_pred_train.reshape(-1,1)
    xtrain = xtrain.swapaxes(1,2).reshape(-1,len_x)
    ytrain= ytrain.swapaxes(1,2).reshape(-1,1)

    mask = np.isfinite(ytrain).flatten()
    xtrain=xtrain[mask,...]
    ytrain=ytrain[mask,...]
    ytrain=torch.Tensor(ytrain).to(device)
    y_pred_train=y_pred_train[mask,...]
    xtrain=torch.Tensor(xtrain).to(device)

    criterion = nn.MSELoss()
    # Generate difference data
    diff = ytrain - y_pred_train

    y_up_data = diff[diff > 0].unsqueeze(1)
    x_up_data = xtrain[diff.flatten() > 0,...]#.unsqueeze(1)

    y_down_data = -1.0 * diff[diff < 0].unsqueeze(1)
    x_down_data = xtrain[diff.flatten() < 0] #.unsqueeze(1)

    net_up = UQ_Net_std().to(device)
    net_up.zero_grad()
    optimizer = optim.SGD(net_up.parameters(), lr=0.01)
    Max_iter = 4000

    for i in range(Max_iter):
        optimizer.zero_grad()
        output = net_up(x_up_data)
        loss = criterion(output, y_up_data)
        # loss = net_up.UQ_loss(x_up_data, output, y_up_data)

        if torch.isnan(loss):
            print(output, y_up_data)
            exit()

        if i % 1000 == 0: print(i, loss)

        loss.backward()
        optimizer.step()

    net_down = UQ_Net_std().to(device)
    net_down.zero_grad()
    optimizer = optim.SGD(net_down.parameters(), lr=0.01)

    for i in range(Max_iter):
        optimizer.zero_grad()
        output = net_down(x_down_data)
        # loss = net_up.UQ_loss(x_down_data, output, y_down_data)
        loss = criterion(output, y_down_data)

        if torch.isnan(loss):
            print(output, y_down_data)
            exit()

        if i % 1000 == 0: print(i, loss)

        loss.backward()
        optimizer.step()

    # ------------------------------------------------------
    # Determine how to move the upper and lower bounds
    num_outlier = int(ytrain.shape[0] * (1 - quantile) / 2)

    #output = net(xtrain)
    output = y_pred_train
    output_up = net_up(xtrain)
    output_down = net_down(xtrain)

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

    x_test = torch.Tensor(x_test).to(device)
    y_up = net_up(x_test).detach().numpy()#.squeeze()
    y_down = net_down(x_test).detach().numpy()#.squeeze()

    if scale_y:
        ci_low = scalar.inverse_transform(y_pred_test - c_down * y_down)
        ci_high = scalar.inverse_transform(y_pred_test + c_up * y_up)
    else:
        ci_low = y_pred_test - c_down * y_down
        ci_high = y_pred_test + c_up * y_up

    return ci_low, ci_high