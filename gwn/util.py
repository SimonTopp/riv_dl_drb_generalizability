import pickle
import numpy as np
import os
import scipy.sparse as sp
import torch
from scipy.sparse import linalg
import torch.nn as nn
import torch.optim as optim
from gwn.model import  *


###########
#Training Utilities
###########
class DataLoader(object):
    def __init__(self, xs, ys, batch_size, pad_with_last_sample=False):
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
        return (data.cpu() * self.std) + self.mean


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

def load_adj(pkl_filename):
    sensor_ids, sensor_id_to_ind, adj = load_pickle(pkl_filename)
    return sensor_ids, sensor_id_to_ind, adj


##############
### Error Metrics
#############


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
            torch.isnan(y_true), torch.zeros_like(y_true), (y_pred - y_true)
        )
        sum_errors = torch.sum(torch.abs(zero_or_error))
        mae_loss = sum_errors/num_y_true
    else:
        mae_loss = 0.0
    return mae_loss


def metric(pred, real):
    masked_mae = mae(pred,real).item()
    masked_rmse = rmse(pred,real).item()
    return masked_mae,masked_rmse

##############
#### UQ Utilities following Lu et al (in review)
##############

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

def calc_uq(xtrain,
            ytrain,
            y_pred_train,
            x_test,
            y_pred_test,
            scalar,
            scale_y = True,
            quantile=0.90):

    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    len_x=xtrain.shape[3]
    #print("training on " + device)

    ###### Flatten everything for training
    x_test = x_test.reshape(-1,len_x)
    x_test=torch.Tensor(x_test).to(device)
    y_pred_test=y_pred_test.reshape(-1,1)
    y_pred_train=y_pred_train.reshape(-1,1)
    xtrain = xtrain.reshape(-1,len_x)
    xtrain = torch.Tensor(xtrain).to(device)
    ytrain= ytrain.reshape(-1,1)
    ytrain=torch.Tensor(ytrain).to(device)

    mask = torch.isfinite(ytrain).flatten()
    xtrain=xtrain[mask,...]
    ytrain=ytrain[mask,...]
    y_pred_train=y_pred_train[mask,...]

    criterion = nn.MSELoss()
    # Generate difference data
    diff = ytrain - y_pred_train

    y_up_data = diff[diff > 0].unsqueeze(1)
    x_up_data = xtrain[diff.flatten() > 0,...]#.unsqueeze(1)

    y_down_data = -1.0 * diff[diff < 0].unsqueeze(1)
    x_down_data = xtrain[diff.flatten() < 0] #.unsqueeze(1)

    net_up = UQ_Net_std(len_x).to(device)
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

    net_down = UQ_Net_std(len_x).to(device)
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

    net_up.eval()
    net_down.eval()
    #x_test = torch.Tensor(x_test).to(device)

    y_up = net_up(x_test).detach().cpu().numpy()#.squeeze()
    y_down = net_down(x_test).detach().cpu().numpy()#.squeeze()

    if scale_y:
        ci_low = scalar.inverse_transform(y_pred_test.cpu() - c_down * y_down)
        ci_high = scalar.inverse_transform(y_pred_test.cpu() + c_up * y_up)
    else:
        ci_low = y_pred_test - c_down * y_down
        ci_high = y_pred_test + c_up * y_up

    return ci_low, ci_high


def load_model(data_in,
               out_dir,
               batch_size=20,
               kernel_size=3,
               layer_size=3,
               learning_rate=0.001,
               randomadj=False,
               n_blocks=4,
               scale_y=False,
               load_weights=False):

    data = np.load(data_in)
    out_dim = data['y_train'].shape[1]
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

    adj_mx = data['dist_matrix']
    dataloader = load_dataset(data, batch_size, batch_size, batch_size)
    scaler = dataloader['scaler']

    supports = [torch.tensor(adj_mx).to(device).float()]
    in_dim = len(data['x_cols'])
    num_nodes = adj_mx.shape[0]

    if randomadj:
        adjinit = None
    else:
        adjinit = supports[0]

    engine = trainer(scaler, in_dim, num_nodes, learning_rate, device, supports,
                     adjinit, out_dim, kernel_size, n_blocks, layer_size, scale_y)

    if load_weights:
        engine.model.load_state_dict(torch.load(out_dir + "/weights_final.pth"))

    return data, dataloader, engine


class trainer():
    def __init__(self, scaler, in_dim, num_nodes, lrate, device, supports, aptinit, out_dim, kernel,
                 blocks, layers, scale_y, nhid=32, wdecay=0.0001, dropout=0.3, gcn_bool=True, addaptadj=True):
        self.model = gwnet(device, num_nodes, dropout, supports=supports, gcn_bool=gcn_bool, addaptadj=addaptadj,
                           aptinit=aptinit, in_dim=in_dim, out_dim=out_dim, residual_channels=nhid,
                           dilation_channels=nhid, skip_channels=nhid * 4, end_channels=nhid * 8, kernel_size=kernel,
                           blocks=blocks, layers=layers)
        #skip and end were 8 and 16 respectively
        self.model.to(device)
        self.optimizer = optim.Adam(self.model.parameters(), lr=lrate, weight_decay=wdecay)
        self.loss = rmse #was mae
        self.scaler = scaler
        self.clip = 5
        self.scale_y = scale_y

    def train(self, input, real_val):
        self.model.train()
        self.optimizer.zero_grad()
        input = nn.functional.pad(input,(1,0,0,0))
        #input = self.scaler.transform(input)
        output = self.model(input)
        output = output.transpose(1,3)
        #output = [batch_size,12,num_nodes,1]
        real = torch.unsqueeze(real_val,dim=1)
        predict = output

        assert real.shape == predict.shape, "Output dims not right, increase kernel or layer size"

        loss = self.loss(predict, real)
        loss.backward()
        if self.clip is not None:
            torch.nn.utils.clip_grad_norm_(self.model.parameters(), self.clip)
        self.optimizer.step()
        if self.scale_y:
            real = self.scaler.inverse_transform(real.cpu())
            predict = self.scaler.inverse_transform(output.detach().cpu())
            metrics = metric(predict,real)
        else:
            metrics = metric(predict,real)
        return metrics

    def eval(self, input, real_val):
        self.model.eval()
        input = nn.functional.pad(input,(1,0,0,0))
        #input = self.scaler.transform(input)
        output = self.model(input)
        output = output.transpose(1,3)
        #output = [batch_size,12,num_nodes,1]
        real = torch.unsqueeze(real_val,dim=1)
        predict = output
        loss = self.loss(predict, real)
        if self.scale_y:
            #real = real.to(type=torch.float64)
            #real = self.scaler.inverse_transform(real)
            predict = self.scaler.inverse_transform(output.detach().cpu()).float()
            metrics = metric(predict,real.cpu())
        else:
            metrics = metric(predict,real)
        return metrics