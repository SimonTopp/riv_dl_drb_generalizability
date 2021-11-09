import pickle

import numpy
import numpy as np
import os

import torch
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
        if torch.is_tensor(data):
            return (data.cpu() * self.std) + self.mean
        else:
            return (data * self.std) + self.mean


def load_dataset(cat_data, batch_size, valid_batch_size= None, test_batch_size=None, pad = False):
    if isinstance(cat_data,str):
        cat_data = np.load(os.path.join(cat_data, 'prepped.npz'))
    data = {}
    for category in ['pre_train', 'train', 'val', 'test']:
        data['y_' + category] = cat_data['y_'+ category]
        data['x_' + category] = cat_data['x_'+ category]

    scaler = StandardScaler(mean = cat_data['y_mean'][0], std = cat_data['y_std'][0])
    # Data format

    data['pre_train_loader'] = DataLoader(data['x_pre_train'],data['y_pre_train'], batch_size, pad_with_last_sample=pad)
    data['train_loader'] = DataLoader(data['x_train'], data['y_train'], batch_size, pad_with_last_sample=pad)
    data['val_loader'] = DataLoader(data['x_val'], data['y_val'], valid_batch_size, pad_with_last_sample=pad)
    data['test_loader'] = DataLoader(data['x_test'], data['y_test'], test_batch_size, pad_with_last_sample=pad)
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


def load_model(data_in,
               out_dir,
               batch_size=20,
               kernel_size=3,
               layer_size=3,
               learning_rate=0.001,
               randomadj=False,
               n_blocks=4,
               scale_y=False,
               load_weights=False,
               pad = False):

    data = np.load(data_in)
    out_dim = data['y_train'].shape[1]
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

    adj_mx = data['dist_matrix']
    dataloader = load_dataset(data, batch_size, batch_size, batch_size, pad)
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
        engine.model.load_state_dict(torch.load(out_dir + "/weights_best_val.pth"))

    return data, dataloader, engine


class trainer():
    def __init__(self, scaler, in_dim, num_nodes, lrate, device, supports, aptinit, out_dim, kernel,
                 blocks, layers, scale_y, nhid=32, wdecay=0.0001, dropout=0.3, gcn_bool=True, addaptadj=True):
        self.model = gwnet(device, num_nodes, dropout, supports=supports, gcn_bool=gcn_bool, addaptadj=addaptadj,
                           aptinit=aptinit, in_dim=in_dim, out_dim=out_dim, residual_channels=nhid,
                           dilation_channels=nhid, skip_channels=nhid * 8, end_channels=nhid * 16, kernel_size=kernel,
                           blocks=blocks, layers=layers)
        #skip and end were 8 and 16 respectively
        self.model.to(device)
        self.optimizer = optim.Adam(self.model.parameters(), lr=lrate, weight_decay=wdecay)
        self.loss = rmse #was mae
        self.scaler = scaler
        self.clip = 3
        self.scale_y = scale_y
        self.scheduler = optim.lr_scheduler.LambdaLR(
            self.optimizer, lr_lambda=lambda epoch: 0.97 ** epoch)

    def train(self, input, real_val):
        self.model.train()
        self.optimizer.zero_grad()
        input = nn.functional.pad(input,(1,0,0,0))
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
            real = self.scaler.inverse_transform(real.cpu())
            predict = self.scaler.inverse_transform(output.detach().cpu())
            metrics = metric(predict, real)
        else:
            metrics = metric(predict,real)
        return metrics

    def predict(self, partition, dataloader,device):
        self.model.eval()
        predicted = []
        for iter, (x, y) in enumerate(dataloader[f"{partition}_loader"].get_iterator()):
            trainx = torch.Tensor(x).to(device)
            trainx = trainx.transpose(1, 3)
            with torch.no_grad():
                output = self.model(trainx)#.transpose(1, 3)
            predicted.append(output)
        predicted = torch.cat(predicted, dim=0)
        return predicted