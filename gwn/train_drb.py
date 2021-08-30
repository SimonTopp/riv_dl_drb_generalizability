import shutil

import torch
import numpy as np
import time
import os.path
#import matplotlib.pyplot as plt
from gwn.engine import trainer
import pandas as pd
import gwn.util as util
import pickle

def train(data_in,
          adj_data,
          out_dir,
          batch_size=20,
          epochs=50,
          epochs_pre=25,
          expid='default',
          kernel_size=3,
          layer_size=3,
          clean_prepped=False,
          print_every=15,
          learning_rate=0.0001,
          dropout=0.3,
          weight_decay=0.0001,
          gcn_bool=True,
          addaptadj=True,
          randomadj=False,
          n_blocks=4,
          nhid=32,
          scale_y=False):
    
    out_dir = os.path.join(out_dir,expid)
    #out_dir = args.out_dir
    os.makedirs(os.path.join(out_dir,'tmp'),exist_ok=True)

    # Save the namespace for reloading the model
    #f = open(os.path.join(out_dir,"namespace.pkl"), "wb")
    #pickle.dump(locals(), f)
    #f.close()

    data = np.load(data_in)
    out_dim = data['y_train'].shape[1]

    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(f"Training on {device}")
    #sensor_ids, sensor_id_to_ind, adj_mx = util.load_adj(args.adjdata,args.adjtype)
    sensor_ids, sensor_id_to_ind, adj_mx = util.load_pickle(adj_data)
    dataloader = util.load_dataset(data, batch_size, batch_size, batch_size)
    scaler = dataloader['scaler']
    #supports = [torch.tensor(i).to(device) for i in adj_mx]
    supports = [torch.tensor(adj_mx).to(device).float()]


    in_dim = len(data['x_cols'])
    num_nodes = adj_mx.shape[0]

    if randomadj:
        adjinit = None
    else:
        adjinit = supports[0]

    #if args.aptonly:
    #    supports = None

    engine = trainer(scaler, in_dim, num_nodes, nhid, dropout,
                         learning_rate, weight_decay, device, supports, gcn_bool, addaptadj,
                         adjinit, out_dim, kernel_size, n_blocks, layer_size, scale_y)

    print("start pre_training...", flush=True)
    ptrain_time = []
    train_log = pd.DataFrame(columns = ['split','epoch','rmse','time'])
    for i in range(1,epochs_pre+1):
        #if i % 10 == 0:
            #lr = max(0.000002,args.learning_rate * (0.1 ** (i // 10)))
            #for g in engine.optimizer.param_groups:
                #g['lr'] = lr
        train_mae = []
        train_mape = []
        train_rmse = []
        t1 = time.time()
        dataloader['pre_train_loader'].shuffle()
        for iter, (x, y) in enumerate(dataloader['pre_train_loader'].get_iterator()):
            trainx = torch.Tensor(x).to(device)
            trainx= trainx.transpose(1, 3)
            trainy = torch.Tensor(y).to(device)
            trainy = trainy.transpose(1, 3)
            metrics = engine.train(trainx, trainy[:,0,:,:])
            train_mae.append(metrics[0])
            train_mape.append(metrics[1])
            train_rmse.append(metrics[2])
            if iter % print_every == 0 :
                log = 'Pre_Iter: {:03d}, Train MAE: {:.4f}, Train MAPE: {:.4f}, Train RMSE: {:.4f}'
                print(log.format(iter, train_mae[-1], train_mape[-1], train_rmse[-1]),flush=True)
        t2 = time.time()
        train_log = train_log.append({'split':'pre_train','epoch':i,'rmse':np.mean(train_rmse),'time':t2-t1}, ignore_index=True)
        ptrain_time.append(t2-t1)
        #torch.save(engine.model.state_dict(), args.save+args.expid+"pre_epoch_"+str(i)+"_.pth")
    print("Average Pre_Training Time: {:.4f} secs/epoch".format(np.mean(ptrain_time)))

    ## Save the pre-train adjacency matrix
    adp = torch.nn.functional.softmax(torch.nn.functional.relu(torch.mm(engine.model.nodevec1, engine.model.nodevec2)), dim=1)
    #adp.to(torch.device('cpu'))
    adp = adp.cpu().detach().numpy()
    adp = adp*(1/np.max(adp))
    df = pd.DataFrame(adp)
    df.to_csv(os.path.join(out_dir,'adjmat_pre_out.csv'), index=False)
    #print("Average Inference Time: {:.4f} secs".format(np.mean(val_time)))

    print("start training...",flush=True)
    his_loss =[]
    val_time = []
    train_time = []
    for i in range(1,epochs+1):
        #if i % 10 == 0:
            #lr = max(0.000002,args.learning_rate * (0.1 ** (i // 10)))
            #for g in engine.optimizer.param_groups:
                #g['lr'] = lr
        train_mae = []
        train_mape = []
        train_rmse = []
        t1 = time.time()
        dataloader['train_loader'].shuffle()
        for iter, (x, y) in enumerate(dataloader['train_loader'].get_iterator()):
            trainx = torch.Tensor(x).to(device)
            trainx= trainx.transpose(1, 3)
            trainy = torch.Tensor(y).to(device)
            trainy = trainy.transpose(1, 3)
            metrics = engine.train(trainx, trainy[:,0,:,:])
            train_mae.append(metrics[0])
            train_mape.append(metrics[1])
            train_rmse.append(metrics[2])
            if iter % print_every == 0 :
                log = 'Iter: {:03d}, Train MAE: {:.4f}, Train MAPE: {:.4f}, Train RMSE: {:.4f}'
                print(log.format(iter, train_mae[-1], train_mape[-1], train_rmse[-1]),flush=True)
        t2 = time.time()
        train_time.append(t2-t1)
        #validation
        valid_mae = []
        valid_mape = []
        valid_rmse = []


        s1 = time.time()
        for iter, (x, y) in enumerate(dataloader['val_loader'].get_iterator()):
            testx = torch.Tensor(x).to(device)
            testx = testx.transpose(1, 3)
            testy = torch.Tensor(y).to(device)
            testy = testy.transpose(1, 3)
            metrics = engine.eval(testx, testy[:,0,:,:])
            valid_mae.append(metrics[0])
            valid_mape.append(metrics[1])
            valid_rmse.append(metrics[2])
        s2 = time.time()
        log = 'Epoch: {:03d}, Inference Time: {:.4f} secs'
        print(log.format(i,(s2-s1)))
        val_time.append(s2-s1)
        mtrain_mae = np.mean(train_mae)
        mtrain_mape = np.mean(train_mape)
        mtrain_rmse = np.mean(train_rmse)

        mvalid_mae = np.mean(valid_mae)
        mvalid_mape = np.mean(valid_mape)
        mvalid_rmse = np.mean(valid_rmse)
        his_loss.append(mvalid_rmse)

        log = 'Epoch: {:03d}, Train MAE: {:.4f}, Train MAPE: {:.4f}, Train RMSE: {:.4f}, Valid MAE: {:.4f}, Valid MAPE: {:.4f}, Valid RMSE: {:.4f}, Training Time: {:.4f}/epoch'
        print(log.format(i, mtrain_mae, mtrain_mape, mtrain_rmse, mvalid_mae, mvalid_mape, mvalid_rmse, (t2 - t1)),flush=True)
        train_log = train_log.append({'split':'train','epoch':i,'rmse':mtrain_rmse,'time':t2-t1}, ignore_index=True)
        train_log = train_log.append({'split': 'val', 'epoch': i, 'rmse': mvalid_rmse, 'time': s2 - s1}, ignore_index=True)

        torch.save(engine.model.state_dict(), out_dir+'/tmp/'+expid+"_epoch_"+str(i)+"_"+str(round(mvalid_rmse,2))+".pth")
    print("Average Training Time: {:.4f} secs/epoch".format(np.mean(train_time)))
    print("Average Inference Time: {:.4f} secs".format(np.mean(val_time)))

    # Save the training log
    train_log.to_csv(os.path.join(out_dir,'train_log.csv'),index=False)

    #testing
    bestid = np.argmin(his_loss)
    engine.model.load_state_dict(torch.load(out_dir+'/tmp/'+expid+"_epoch_"+str(bestid+1)+"_"+str(round(his_loss[bestid],2))+".pth"))


    outputs = []
    realy = torch.Tensor(dataloader['y_test']).to(device)
    realy = realy.transpose(1,3)[:,0,:,:]

    for iter, (x, y) in enumerate(dataloader['test_loader'].get_iterator()):
        testx = torch.Tensor(x).to(device)
        testx = testx.transpose(1,3)
        with torch.no_grad():
            preds = engine.model(testx).transpose(1,3)
        outputs.append(preds.squeeze())

    yhat = torch.cat(outputs,dim=0)
    yhat = yhat[:realy.size(0),...]

    test_dates = np.transpose(data['dates_test'], (0, 3, 2, 1)).squeeze()
    test_ids = np.transpose(data['ids_test'], (0, 3, 2, 1)).squeeze()

    test_ids = test_ids[:, :, -out_dim:]
    test_dates = test_dates[:, :, -out_dim:]

    def prepped_array_to_df(data_array, obs, dates, ids):

        df_obs = pd.DataFrame(obs.flatten(), columns=['temp_ob'])
        df_preds = pd.DataFrame(data_array.flatten(), columns=['temp_pred'])
        df_dates = pd.DataFrame(dates.flatten(), columns=["date"])
        df_ids = pd.DataFrame(ids.flatten(), columns=["seg_id_nat"])
        df = pd.concat([df_dates, df_ids, df_preds, df_obs], axis=1)
        return df

    ## Save the results of the test data
    test_df = prepped_array_to_df(np.array(yhat.cpu()), np.array(realy.cpu()), test_dates, test_ids).dropna()
    test_df.to_csv(out_dir + '/test_results.csv', index=False)

    print("Training finished")
    print("The valid loss on best model is", str(round(his_loss[bestid],4)))
    
    torch.save(engine.model.state_dict(), out_dir+"/weights_final_"+str(round(his_loss[bestid],2))+".pth")

    ## Save the final adjacency matrix
    adp = torch.nn.functional.softmax(torch.nn.functional.relu(torch.mm(engine.model.nodevec1, engine.model.nodevec2)), dim=1)
    adp.to(torch.device('cpu'))
    adp = adp.cpu().detach().numpy()
    adp = adp*(1/np.max(adp))
    df = pd.DataFrame(adp)
    df.to_csv(os.path.join(out_dir,'adjmat_out.csv'), index=False)

    # Remove temporary weights
    shutil.rmtree(out_dir+'/tmp')

    ## Remove the prepped data
    if clean_prepped:
        os.remove(data_in)
'''
if __name__ == "__main__":
    t1 = time.time()
    main()
    t2 = time.time()
    print("Total time spent: {:.4f}".format(t2-t1))

'''