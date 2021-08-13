import shutil

import torch
import numpy as np
import argparse
import time
import util
import os.path
#import matplotlib.pyplot as plt
from engine import trainer
from generate_training_data_drb import prep_data
import pandas as pd
import util
import pickle


## Train Args that might change
parser = argparse.ArgumentParser()
parser.add_argument('--out_dir',type=str,default='../data/out',help='data path')
parser.add_argument('--adjdata',type=str,default='../data/in/adj_mat/adj_mx_full.pkl',help='adj data path')
parser.add_argument('--seq_length',type=int,default=60,help='')
parser.add_argument('--out_dim',type=int, default=60,help = 'Period of output predictions')
parser.add_argument('--batch_size',type=int,default=20,help='batch size')
parser.add_argument('--epochs',type=int,default=50,help='')
parser.add_argument('--epochs_pre',type=int,default=25,help='')
parser.add_argument('--expid',type=str,default='default',help='experiment id')
parser.add_argument('--kernel_size',type=int, default=3) #2
parser.add_argument('--layer_size',type=int,default=3) #2
## Train args that will rarely change
parser.add_argument('--print_every',type=int,default=15,help='')
parser.add_argument('--learning_rate',type=float,default=0.0001,help='learning rate')
parser.add_argument('--dropout',type=float,default=0.3,help='dropout rate')
parser.add_argument('--weight_decay',type=float,default=0.0001,help='weight decay rate')
## Train args that will likely always be defaults
parser.add_argument('--gcn_bool',action='store_false',help='whether to add graph convolution layer')
parser.add_argument('--addaptadj',action='store_false',help='whether add adaptive adj')
parser.add_argument('--randomadj',action='store_true',help='whether random initialize adaptive adj')
parser.add_argument('--n_blocks',type=int, default=4)
parser.add_argument('--nhid',type=int,default=32,help='')

## Data Prep that might change
parser.add_argument("--obs_temper_file", type=str, default='../river-dl/data/in/obs_temp_full')
parser.add_argument('--obs_flow_file',type=str, default='../river-dl/data/in/obs_flow_full')
parser.add_argument("--pretrain_file",type=str,default='../river-dl/data/in/uncal_sntemp_input_output')
parser.add_argument("--x_vars",nargs='+',default=["seg_rain", "seg_tave_air", "seginc_swrad", "seg_length", "seginc_potet", "seg_slope", "seg_humid", "seg_elev", "seg_upstream_inflow",'seginc_sroff'])
parser.add_argument("--offset",type=float, default=1)

## Data Prep args that will rarely change
parser.add_argument('--train_start_date',nargs='+', default= ['1985-10-01', '2016-10-01'])
parser.add_argument("--train_end_date",nargs='+',default=['2006-09-30', '2020-09-30'])
parser.add_argument("--val_start_date",type=str, default='2006-10-01')
parser.add_argument("--val_end_date",type=str,default='2011-09-30')
parser.add_argument("--test_start_date",nargs='+',default=['1980-10-01', '2011-10-01', '2020-10-01'])
parser.add_argument("--test_end_date",nargs='+',default=['1985-09-30', '2016-09-30', '2021-09-30'])
parser.add_argument("--y_vars",nargs="+",default=['seg_tave_water'])
parser.add_argument("--primary_variable",type=str,default='temp')


#args = parser.parse_args()
args = parser.parse_args(['--epochs', '5'])
args.obs_flow_file='../data/in/obs_flow_subset'
args.obs_temper_file = '../data/in/obs_temp_subset'
args.pretrain_file = '../data/in/uncal_sntemp_input_output_subset'
args.adjdata = '../data/in/adj_mat/adj_mx_subset.pkl'
args.epochs_pre = 5
args.expid = 'FigTest'
args.kernel_size = 4
args.out_dim = 60
args.offset = 1

def main():
    
    print(args)

    out_dir = os.path.join(args.out_dir,args.expid)
    os.makedirs(os.path.join(out_dir,'tmp'),exist_ok=True)

    # Save the namespace for reloading the model
    f = open(os.path.join(out_dir,"namespace.pkl"), "wb")
    pickle.dump(vars(args), f)
    f.close()

    data = prep_data(obs_temper_file=args.obs_temper_file,
        obs_flow_file=args.obs_flow_file,
        pretrain_file=args.pretrain_file,
        train_start_date=args.train_start_date,
        train_end_date=args.train_end_date,
        val_start_date=args.val_start_date,
        val_end_date=args.val_end_date,
        test_start_date=args.test_start_date,
        test_end_date=args.test_end_date,
        x_vars=args.x_vars,
        y_vars=args.y_vars,
        primary_variable=args.primary_variable,
        seq_length=args.seq_length,
        period=args.out_dim,
        offset=args.offset)
        #out_file = args.data)
        #set seed
        #torch.manual_seed(args.seed)
        #np.random.seed(args.seed)
        #load data

    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(f"Training on {device}")
    #sensor_ids, sensor_id_to_ind, adj_mx = util.load_adj(args.adjdata,args.adjtype)
    sensor_ids, sensor_id_to_ind, adj_mx = util.load_pickle(args.adjdata)
    dataloader = util.load_dataset(data, args.batch_size, args.batch_size, args.batch_size)
    scaler = dataloader['scaler']
    #supports = [torch.tensor(i).to(device) for i in adj_mx]
    supports = [torch.tensor(adj_mx).to(device).float()]
    in_dim = len(args.x_vars)
    num_nodes = adj_mx.shape[0]

    if args.randomadj:
        adjinit = None
    else:
        adjinit = supports[0]

    #if args.aptonly:
    #    supports = None

    engine = trainer(scaler, in_dim, num_nodes, args.nhid, args.dropout,
                         args.learning_rate, args.weight_decay, device, supports, args.gcn_bool, args.addaptadj,
                         adjinit, args.out_dim, args.kernel_size, args.n_blocks, args.layer_size)

    print("start pre_training...", flush=True)
    phis_loss =[]
    pval_time = []
    ptrain_time = []
    train_log = pd.DataFrame(columns = ['split','epoch','rmse','time'])
    for i in range(1,args.epochs_pre+1):
        #if i % 10 == 0:
            #lr = max(0.000002,args.learning_rate * (0.1 ** (i // 10)))
            #for g in engine.optimizer.param_groups:
                #g['lr'] = lr
        train_loss = []
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
            train_loss.append(metrics[0])
            train_mape.append(metrics[1])
            train_rmse.append(metrics[2])
            if iter % args.print_every == 0 :
                log = 'Pre_Iter: {:03d}, Train Loss: {:.4f}, Train MAPE: {:.4f}, Train RMSE: {:.4f}'
                print(log.format(iter, train_loss[-1], train_mape[-1], train_rmse[-1]),flush=True)
        t2 = time.time()
        train_log = train_log.append({'split':'pre_train','epoch':i,'rmse':np.mean(train_loss),'time':t2-t1}, ignore_index=True)
        ptrain_time.append(t2-t1)
        #torch.save(engine.model.state_dict(), args.save+args.expid+"pre_epoch_"+str(i)+"_.pth")
    print("Average Pre_Training Time: {:.4f} secs/epoch".format(np.mean(ptrain_time)))
    #print("Average Inference Time: {:.4f} secs".format(np.mean(val_time)))

    print("start training...",flush=True)
    his_loss =[]
    val_time = []
    train_time = []
    for i in range(1,args.epochs+1):
        #if i % 10 == 0:
            #lr = max(0.000002,args.learning_rate * (0.1 ** (i // 10)))
            #for g in engine.optimizer.param_groups:
                #g['lr'] = lr
        train_loss = []
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
            train_loss.append(metrics[0])
            train_mape.append(metrics[1])
            train_rmse.append(metrics[2])
            if iter % args.print_every == 0 :
                log = 'Iter: {:03d}, Train Loss: {:.4f}, Train MAPE: {:.4f}, Train RMSE: {:.4f}'
                print(log.format(iter, train_loss[-1], train_mape[-1], train_rmse[-1]),flush=True)
        t2 = time.time()
        train_time.append(t2-t1)
        #validation
        valid_loss = []
        valid_mape = []
        valid_rmse = []


        s1 = time.time()
        for iter, (x, y) in enumerate(dataloader['val_loader'].get_iterator()):
            testx = torch.Tensor(x).to(device)
            testx = testx.transpose(1, 3)
            testy = torch.Tensor(y).to(device)
            testy = testy.transpose(1, 3)
            metrics = engine.eval(testx, testy[:,0,:,:])
            valid_loss.append(metrics[0])
            valid_mape.append(metrics[1])
            valid_rmse.append(metrics[2])
        s2 = time.time()
        log = 'Epoch: {:03d}, Inference Time: {:.4f} secs'
        print(log.format(i,(s2-s1)))
        val_time.append(s2-s1)
        mtrain_loss = np.mean(train_loss)
        mtrain_mape = np.mean(train_mape)
        mtrain_rmse = np.mean(train_rmse)

        mvalid_loss = np.mean(valid_loss)
        mvalid_mape = np.mean(valid_mape)
        mvalid_rmse = np.mean(valid_rmse)
        his_loss.append(mvalid_loss)

        log = 'Epoch: {:03d}, Train Loss: {:.4f}, Train MAPE: {:.4f}, Train RMSE: {:.4f}, Valid Loss: {:.4f}, Valid MAPE: {:.4f}, Valid RMSE: {:.4f}, Training Time: {:.4f}/epoch'
        print(log.format(i, mtrain_loss, mtrain_mape, mtrain_rmse, mvalid_loss, mvalid_mape, mvalid_rmse, (t2 - t1)),flush=True)
        train_log = train_log.append({'split':'train','epoch':i,'rmse':mtrain_loss,'time':t2-t1}, ignore_index=True)
        train_log = train_log.append({'split': 'val', 'epoch': i, 'rmse': mvalid_loss, 'time': s2 - s1}, ignore_index=True)

        torch.save(engine.model.state_dict(), out_dir+'/tmp/'+args.expid+"_epoch_"+str(i)+"_"+str(round(mvalid_loss,2))+".pth")
    print("Average Training Time: {:.4f} secs/epoch".format(np.mean(train_time)))
    print("Average Inference Time: {:.4f} secs".format(np.mean(val_time)))

    # Save the training log
    train_log.to_csv(os.path.join(out_dir,'train_log.csv'),index=False)

    #testing
    bestid = np.argmin(his_loss)
    engine.model.load_state_dict(torch.load(out_dir+'/tmp/'+args.expid+"_epoch_"+str(bestid+1)+"_"+str(round(his_loss[bestid],2))+".pth"))


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
    assert yhat.shape == realy.shape, "Output dims not right, increase kernel or layer size"

    #data = np.load(args.data + '/data.npz')
    period = data['period'][0]

    test_dates = np.transpose(data['dates_test'], (0, 3, 2, 1)).squeeze()
    test_ids = np.transpose(data['ids_test'], (0, 3, 2, 1)).squeeze()

    if ~np.isnan(period):
        test_ids = test_ids[:, :, -period:]
        test_dates = test_dates[:, :, -period:]

    def prepped_array_to_df(data_array, obs, dates, ids):

        df_obs = pd.DataFrame(obs.flatten(), columns=['temp_ob'])
        df_preds = pd.DataFrame(data_array.flatten(), columns=['temp_pred'])
        df_dates = pd.DataFrame(dates.flatten(), columns=["date"])
        df_ids = pd.DataFrame(ids.flatten(), columns=["seg_id_nat"])
        df = pd.concat([df_dates, df_ids, df_preds, df_obs], axis=1)
        return df

    ## Save the results of the test data
    test_df = prepped_array_to_df(np.array(yhat), np.array(realy), test_dates, test_ids).dropna()
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

if __name__ == "__main__":
    t1 = time.time()
    main()
    t2 = time.time()
    print("Total time spent: {:.4f}".format(t2-t1))
