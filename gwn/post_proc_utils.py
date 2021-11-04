import time

import torch
import numpy as np
import os.path
import pandas as pd
import gwn.util as util

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
            outfile=None,
            clean_prepped=True,
            quantile=0.9):
    out_dir = os.path.join(out_dir, expid, f"{kernel_size}_{layer_size}")
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
                                               load_weights=True)

    scaler = dataloader["scaler"]
    out_dim = data['y_train'].shape[1]

    preds = {

    }
    data['preds_pre_train'] = engine.predict('pre_train', dataloader)
    data['preds_train'] = engine.predict('train', dataloader)
    data['preds_val'] = engine.predict('val', dataloader)
    data['preds_test'] = engine.predict('test', dataloader)

    if outfile:
        np.savez_compressed(os.path.join(out_dir, 'prepped_preds.npz'), **data)

    return data

    '''

    engine.model.eval()
    outputs_pre=[]
    for iter, (x, y) in enumerate(dataloader['pre_train_loader'].get_iterator()):
        trainx = torch.Tensor(x).to(device)
        trainx = trainx.transpose(1,3)
        with torch.no_grad():
            output = engine.model(trainx).transpose(1,3)
        outputs_pre.append(output)
    outputs_pre = torch.cat(outputs_pre, dim=0).squeeze()
    outputs_train = []
    for iter, (x, y) in enumerate(dataloader['train_loader'].get_iterator()):
        trainx = torch.Tensor(x).to(device)
        output = model(trainx)
        outputs.append(output)

    check =data['sf']


    train_y = dataloader['y_train']
    train_x = dataloader['x_train']
    idx = np.arange(0, len(train_y)+1, step=batch_size)
    engine.model.eval()
    for i in idx:
        x = torch.Tensor(train_x[i:i+batch_size,...]).to(device)
        if x.shape[0]==0:
            break
        x = x.transpose(1,3)
        with torch.no_grad():
            preds=engine.model(x)#.transpose(1,3)
        outputs.append(preds)
    preds_train = torch.cat(outputs, dim=0).squeeze()

    outputs = []
    test_y = dataloader['y_test']
    test_x = dataloader['x_test']
    idx = np.arange(0, len(test_y) + 1, step=batch_size)
    for i in idx:
        x = torch.Tensor(test_x[i:i + batch_size, ...]).to(device)
        if x.shape[0]==0:
            break
        x = x.transpose(1, 3)
        with torch.no_grad():
            preds = engine.model(x)#.transpose(1, 3)
        outputs.append(preds)
    '''
    preds_test = torch.cat(outputs, dim=0).squeeze()
    print('Done with Predictions')

    ##Calculate UQ bounds

    ######Change to match updatea
    ci_low, ci_high = util.calc_uq(train_x[:, -out_dim:, ...],  # xtrain,
                                   train_y,  # dataloader['y_train'],   #ytrain
                                   preds_train,  # y_pred_train,
                                   test_x[:, -out_dim:, ...],  # dataloader['x_test'],    #x_test,
                                   preds_test,  # y_pred_test,
                                   scaler,
                                   scale_y=True,
                                   quantile=quantile)

    if scale_y:
        yhat = scaler.inverse_transform(preds_test)

    test_dates = data['dates_test'].squeeze()
    test_ids = data['ids_test'].squeeze()

    test_ids = test_ids[:, -out_dim:, ...]
    test_dates = test_dates[:, -out_dim:, ...]

    def prepped_array_to_df(data_array, obs, dates, ids, ci_high, ci_low):

        df_obs = pd.DataFrame(obs.flatten(), columns=['temp_ob'])
        df_preds = pd.DataFrame(data_array.flatten(), columns=['temp_pred'])
        df_dates = pd.DataFrame(dates.flatten(), columns=["date"])
        df_ids = pd.DataFrame(ids.flatten(), columns=["seg_id_nat"])
        df_cih = pd.DataFrame(ci_high.flatten(), columns=['ci_high'])
        df_cil = pd.DataFrame(ci_low.flatten(), columns=['ci_low'])
        df = pd.concat([df_dates, df_ids, df_preds, df_obs, df_cih, df_cil], axis=1)
        return df

    ## Save the results of the test data
    test_df = prepped_array_to_df(yhat.cpu().numpy(), test_y, test_dates, test_ids, ci_high.cpu().numpy(),
                                  ci_low.cpu().numpy())
    test_df.to_csv(out_dir + '/test_results.csv', index=False)

    ## Remove the prepped data
    if clean_prepped:
        os.remove(data_in)


##############
#### UQ Utilities following Lu et al (in review)
##############

def flatten_preds(preds):
    return preds.reshape(-1,1)

def flatten_drivers(drivers, in_dim):
    return drivers.reshape(-1,in_dim)

def flatten_obs(obs,out_dim):
    obs = obs[:,-out_dim:,...]
    return obs.reshape(-1,1)

def prep_uq_data(x,y, y_hat):
    in_dim = x.shape[3]
    out_dim = y_hat.shape[1]
    x = flatten_drivers(x, in_dim)
    y_hat= flatten_preds(y_hat)
    y = flatten_obs(y,out_dim)
    residuals = y-y_hat
    x_up = x[residuals > 0]
    x_down = x[residuals < 0]
    y_up = residuals[residuals>0]
    y_down = residuals[residuals<0]
    return x_up,x_down, y_up, y_down


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
        predictions = np.load(os.path.join(predictions,'prepped_preds.npz'))

    scaler = StandardScaler(mean=cat_data['y_mean'][0], std=cat_data['y_std'][0])

    data = {}
    for cat in categories:
        x_up, x_down, y_up, y_down = prep_uq_data(cat_data[f'x_{cat}'], cat_data[f'y_{cat}'], predictions[f'preds_{cat}'])
        data[f'{cat}_up_loader'] = util.DataLoader(x_up,y_up, batch_size)
        data[f'{cat}_down_loader'] = util.DataLoader(x_down,y_down,batch_size)
    data['scaler'] = scaler

    return data

def train_uq(model, ci_bound, dataloader, epochs_pre, epochs, weights = None, early_stopping = 20):
    if weights:
        model.load_state_dict(weights)

    criterion = nn.MSELoss()
    optimizer = optim.SGD(model.parameters(), lr=0.01)
    ## Pretraining
    best_mse = 100 #will get overwritten
    model.train()
    pre_train_mse=[]
    t1 = time.time()
    data[f"pre_train_{ci_bound}_loader"].shuffle()
    for i in range(1,epochs_pre+1):
        for iter, (x, y) in enumerate(dataloader[f'pre_train_{ci_bound}_loader'].get_iterator()):
            optimizer.zero_grad()
            trainx = torch.Tensor(x).to(device)
            trainy = torch.Tensor(y).to(device)
            output = model(trainx)
            loss = criterion(output, trainy)
            if torch.isnan(loss):
                print(output, trainy)
                exit()
            loss.backward()
            optimizer.step()
            pre_train_mse.append(loss)
        print(f'Epoch {i}: loss {np.mean(pre_train_mse)}')
    t2 = time.time()
    print(f"Total pretrain time: {t2-t1}")

    for i in range(1, epochs + 1):
        train_mse = []
        data[f'train_{ci_bound}_loader'].shuffle()
        for iter, (x, y) in enumerate(dataloader[f'train_{ci_bound}_loader'].get_iterator()):
            optimizer.zero_grad()
            trainx = torch.Tensor(x).to(device)
            trainy = torch.Tensor(y).to(device)
            output = model(trainx)
            loss = criterion(output, trainy)
            if torch.isnan(loss):
                print(output, trainy)
                exit()
            loss.backward()
            optimizer.step()
            train_mse.append(loss)

        valid_mse = []
        model.eval()
        for iter, (x, y) in enumerate(dataloader[f'val_{ci_bound}_loader'].get_iterator()):
            testx = torch.Tensor(x).to(device)
            testy = torch.Tensor(y).to(device)
            output = model(testx)
            loss = criterion(output, testy)
            valid_mse.append(loss)

        mtrain_mse = np.mean(train_mse)
        mvalid_mse = np.mean(valid_mse)

        log = 'Epoch: {:03d}, Train MSE: {:.4f}, Valid MSE: {:.4f}'
        print(log.format(i,mtrain_mse, mvalid_rmse),flush=True)

        if mvalid_mse < best_mse:
            torch.save(model.state_dict(), os.path.join(out_dir,f"weights_uq_{cat}.pth"))
            best_mse = mvalid_mse
            epochs_since_best = 0
        else:
            epochs_since_best += 1
        if epochs_since_best > early_stopping:
            print(f"Early Stopping at Epoch {i}")
            break


    # Save the training log
    train_log.to_csv(os.path.join(out_dir,'uq_'+ci_bound+'_train_log.csv'),index=False)
    bestid = np.argmin(his_loss)
    model.load_state_dict(torch.load(
        out_dir+'/tmp/uq_'+ci_bound+'_'+expid+"_epoch_" + str(bestid + 1) + "_" + str(round(his_loss[bestid], 2)) + ".pth"))
    torch.save(model.state_dict(), out_dir + "/uq_"+ci_bound+'_'+"_weights_final.pth")

    return model

def calc_uq(out_dir,
            prepped,
            preds,
            scale_y = True,
            quantile=0.90):

    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    len_x=xtrain.shape[3]

    dataloader = load_data_uq(prepped, preds, 500)

    train_uq(model, ci_bound, dataloader, epochs_pre, epochs, weights=None, early_stopping=20):
    net_up = UQ_Net_std(len_x).to(device)
    net_up = train_uq(net_up, 'up', dataloader, 50, 100)

    net_down = UQ_Net_std(len_x).to(device)

    net_down = train_uq(net_down, 'down', dataloader, 50, 100)

    # ------------------------------------------------------
    # Determine how to move the upper and lower bounds

    num_outlier = int(data['y_train'].shape[0] * (1 - quantile) / 2)

    output = data['y_preds_train']
    net_up.eval()
    output_up = net_up(data['x_train'])
    net_down.eval()
    output_down = net_down(data['x_train'])

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


    y_up = net_up(data['x_test']).detach().cpu().numpy()#.squeeze()
    y_down = net_down(data['x_test').detach().cpu().numpy()#.squeeze()

    if scale_y:
        ci_low = scalar.inverse_transform(y_pred_test.cpu() - c_down * y_down)
        ci_high = scalar.inverse_transform(y_pred_test.cpu() + c_up * y_up)
    else:
        ci_low = y_pred_test - c_down * y_down
        ci_high = y_pred_test + c_up * y_up

    return ci_low, ci_high
