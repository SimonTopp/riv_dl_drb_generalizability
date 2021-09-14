import shutil
import torch
import numpy as np
import time
import os.path
#import matplotlib.pyplot as plt
import gwn.engine
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
          learning_rate=0.001,
          dropout=0.3,
          gcn_bool=True,
          addaptadj=True,
          randomadj=False,
          n_blocks=4,
          scale_y=False):
    
    out_dir = os.path.join(out_dir,expid)
    #out_dir = args.out_dir
    os.makedirs(os.path.join(out_dir,'tmp'),exist_ok=True)
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

    data, dataloader, engine = util.load_model(data_in,
               adj_data,
               out_dir,
               batch_size,
               kernel_size=kernel_size,
               layer_size=layer_size,
               learning_rate=learning_rate,
               randomadj=randomadj,
               n_blocks=n_blocks,
               scale_y=scale_y,
               load_weights=False)



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
            train_mae.append(metrics[0].item())
            train_mape.append(metrics[1].item())
            train_rmse.append(metrics[2].item())
            #if iter % print_every == 0 :
             #   log = 'Pre_Iter: {:03d}, Train MAE: {:.4f}, Train MAPE: {:.4f}, Train RMSE: {:.4f}'
             #   print(log.format(iter, train_mae[-1], train_mape[-1], train_rmse[-1]),flush=True)
        t2 = time.time()
        mtrain_mae = np.mean(train_mae)
        mtrain_mape = np.mean(train_mape)
        mtrain_rmse = np.mean(train_rmse)
        train_log = train_log.append({'split':'pre_train','epoch':i,'rmse':mtrain_rmse,'time':t2-t1}, ignore_index=True)
        ptrain_time.append(t2-t1)
        log = 'PT_Epoch: {:03d}, PTrain MAE: {:.4f}, PTrain MAPE: {:.4f}, PTrain RMSE: {:.4f}, PTraining Time: {:.4f}/epoch'
        print(log.format(i, mtrain_mae, mtrain_mape, mtrain_rmse,(t2 - t1)),flush=True)
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
            #if iter % print_every == 0 :
            #    log = 'Iter: {:03d}, Train MAE: {:.4f}, Train MAPE: {:.4f}, Train RMSE: {:.4f}'
            #    print(log.format(iter, train_mae[-1], train_mape[-1], train_rmse[-1]),flush=True)
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

    bestid = np.argmin(his_loss)
    engine.model.load_state_dict(torch.load(out_dir+'/tmp/'+expid+"_epoch_"+str(bestid+1)+"_"+str(round(his_loss[bestid],2))+".pth"))
    torch.save(engine.model.state_dict(), out_dir + "/weights_final.pth")

    print("Training finished")
    print("The valid loss on best model is", str(round(his_loss[bestid],4)))
    

    ## Save the final adjacency matrix
    adp = torch.nn.functional.softmax(torch.nn.functional.relu(torch.mm(engine.model.nodevec1, engine.model.nodevec2)), dim=1)
    adp.to(torch.device('cpu'))
    adp = adp.cpu().detach().numpy()
    adp = adp*(1/np.max(adp))
    df = pd.DataFrame(adp)
    df.to_csv(os.path.join(out_dir,'adjmat_out.csv'), index=False)

    # Remove temporary weights
    shutil.rmtree(out_dir+'/tmp')

'''
if __name__ == "__main__":
    t1 = time.time()
    main()
    t2 = time.time()
    print("Total time spent: {:.4f}".format(t2-t1))

'''