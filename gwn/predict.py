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

    '''
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
    ci_low, ci_high = util.calc_uq(train_x[:,-out_dim:,...],         #xtrain,
                                   train_y,                         #dataloader['y_train'],   #ytrain
                                   preds_train,                     #y_pred_train,
                                   test_x[:,-out_dim:,...],          # dataloader['x_test'],    #x_test,
                                   preds_test,                      #y_pred_test,
                                   scaler,
                                   scale_y=True,
                                   quantile=quantile)

    if scale_y:
        yhat = scaler.inverse_transform(preds_test)

    test_dates = data['dates_test'].squeeze()
    test_ids = data['ids_test'].squeeze()

    test_ids = test_ids[:,-out_dim:,...]
    test_dates = test_dates[:,-out_dim:,...]

    def prepped_array_to_df(data_array, obs, dates, ids, ci_high, ci_low):

        df_obs = pd.DataFrame(obs.flatten(), columns=['temp_ob'])
        df_preds = pd.DataFrame(data_array.flatten(), columns=['temp_pred'])
        df_dates = pd.DataFrame(dates.flatten(), columns=["date"])
        df_ids = pd.DataFrame(ids.flatten(), columns=["seg_id_nat"])
        df_cih = pd.DataFrame(ci_high.flatten(), columns=['ci_high'])
        df_cil = pd.DataFrame(ci_low.flatten(), columns =['ci_low'])
        df = pd.concat([df_dates, df_ids, df_preds, df_obs, df_cih,df_cil], axis=1)
        return df

    ## Save the results of the test data
    test_df = prepped_array_to_df(yhat.cpu().numpy(), test_y, test_dates, test_ids, ci_high.cpu().numpy(),ci_low.cpu().numpy())
    test_df.to_csv(out_dir + '/test_results.csv', index=False)

    ## Remove the prepped data
    if clean_prepped:
        os.remove(data_in)