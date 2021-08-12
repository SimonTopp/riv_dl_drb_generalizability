import util
import argparse
from model import *
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

parser = argparse.ArgumentParser()
parser.add_argument('--device',type=str,default='cuda:3',help='')
parser.add_argument('--data',type=str,default='data/DRB_gwn_full',help='data path')
parser.add_argument('--adjdata',type=str,default='data/DRB_gwn_full/adj_mx.pkl',help='adj data path')
parser.add_argument('--adjtype',type=str,default='doubletransition',help='adj type')
parser.add_argument('--gcn_bool',action='store_true',help='whether to add graph convolution layer')
parser.add_argument('--aptonly',action='store_true',help='whether only adaptive adj')
parser.add_argument('--addaptadj',action='store_true',help='whether add adaptive adj')
parser.add_argument('--randomadj',action='store_true',help='whether random initialize adaptive adj')
parser.add_argument('--seq_length',type=int,default=365,help='')
parser.add_argument('--nhid',type=int,default=32,help='')
parser.add_argument('--in_dim',type=int,default=8,help='inputs dimension')
parser.add_argument('--num_nodes',type=int,default=456,help='number of nodes')
parser.add_argument('--batch_size',type=int,default=1,help='batch size')
parser.add_argument('--learning_rate',type=float,default=0.001,help='learning rate')
parser.add_argument('--dropout',type=float,default=0.3,help='dropout rate')
parser.add_argument('--weight_decay',type=float,default=0.0001,help='weight decay rate')
parser.add_argument('--checkpoint',type=str,help='')
parser.add_argument('--plotheatmap',type=str,default='True',help='')
#parser.add_argument('--seed',type=int,default=99,help='random seed')
parser.add_argument('--save',type=str,default='./train_val_drb/',help='save path')
parser.add_argument('--expid',type=str,default='default',help='experiment id')

#args = parser.parse_args()

args = parser.parse_args(['--data', 'data/DRB_gwn_full'])

args.device = 'cpu'
args.gcn_bool = True
args.addaptadj = True
args.randomadj = True
args.checkpoint = 'train_val_drb/dt_best_1.78.pth'
args.data = 'data/DRB_gwn_full'
#args.adjtype = 'transition'

def main():
    device = torch.device(args.device)

    _, _, adj_mx = util.load_adj(args.adjdata,args.adjtype)
    supports = [torch.tensor(i).to(device) for i in adj_mx]
    if args.randomadj:
        adjinit = None
    else:
        adjinit = supports[0]

    if args.aptonly:
        supports = None
    model = gwnet(device, args.num_nodes, args.dropout, supports=supports, gcn_bool=args.gcn_bool, addaptadj=args.addaptadj,
                   in_dim=args.in_dim, out_dim=args.seq_length, residual_channels=args.nhid, dilation_channels=args.nhid,
                  skip_channels=args.nhid * 8, end_channels=args.nhid * 16, aptinit=adjinit)
    #model =  gwnet(device, args.num_nodes, args.dropout, supports=supports, gcn_bool=args.gcn_bool, addaptadj=args.addaptadj, aptinit=adjinit)
    model.to(device)
    #dict = torch.load(args.checkpoint, map_location=torch.device('cpu'))
    model.load_state_dict(torch.load(args.checkpoint, map_location=torch.device('cpu')))
    model.eval()


    print('model load successfully')

    dataloader = util.load_dataset(args.data, args.batch_size, args.batch_size, args.batch_size)
    scaler = dataloader['scaler']
    outputs = []
    realy = torch.Tensor(dataloader['y_test']).to(device)
    realy = realy.transpose(1,3)[:,0,:,:]

    for iter, (x, y) in enumerate(dataloader['test_loader'].get_iterator()):
        testx = torch.Tensor(x).to(device)
        testx = testx.transpose(1,3)
        with torch.no_grad():
            preds = model(testx).transpose(1,3)
        outputs.append(preds.squeeze())

    yhat = torch.cat(outputs,dim=0)
    yhat = yhat[:realy.size(0),...]


    amae = []
    amape = []
    armse = []
    for i in range(12):
        pred = scaler.inverse_transform(yhat[:,:,i])
        real = realy[:,:,i]
        metrics = util.metric(pred,real)
        log = 'Evaluate best model on test data for horizon {:d}, Test MAE: {:.4f}, Test MAPE: {:.4f}, Test RMSE: {:.4f}'
        print(log.format(i+1, metrics[0], metrics[1], metrics[2]))
        amae.append(metrics[0])
        amape.append(metrics[1])
        armse.append(metrics[2])

    log = 'On average over 12 horizons, Test MAE: {:.4f}, Test MAPE: {:.4f}, Test RMSE: {:.4f}'
    print(log.format(np.mean(amae),np.mean(amape),np.mean(armse)))


    if args.plotheatmap == "True":
        adp = F.softmax(F.relu(torch.mm(model.nodevec1, model.nodevec2)), dim=1)
        device = torch.device('cpu')
        adp.to(device)
        adp = adp.cpu().detach().numpy()
        adp = adp*(1/np.max(adp))
        df = pd.DataFrame(adp)
        sns.heatmap(df, cmap="RdYlBu")
        #plt.savefig("./emb"+ '.pdf')

    y12 = realy[:,99,11].cpu().detach().numpy()
    yhat12 = scaler.inverse_transform(yhat[:,99,11]).cpu().detach().numpy()

    y3 = realy[:,99,2].cpu().detach().numpy()
    yhat3 = scaler.inverse_transform(yhat[:,99,2]).cpu().detach().numpy()

    df2 = pd.DataFrame({'real12':y12,'pred12':yhat12, 'real3': y3, 'pred3':yhat3})
    df2.to_csv('./wave.csv',index=False)


if __name__ == "__main__":
    main()


names = ['x', 'y', 'z']
index = pd.MultiIndex.from_product([range(s)for s in realy.shape], names=names)
df = pd.DataFrame({'realy': realy.flatten()}, index=index)['realy']
df = df.unstack(level='y').swaplevel().sort_index()
df.index.names = ['DOY', 'Year']
df.reset_index(inplace=True)

_, ids,_ = util.load_adj(args.adjdata,args.adjtype)
segIds = list(ids.keys())
df.columns = ['DOY','Year'] +segIds
df = df.dropna(axis=1,thresh=200)
df = df.sort_values(by=['Year','DOY'])

predicted = scaler.inverse_transform(yhat)
index = pd.MultiIndex.from_product([range(s)for s in predicted.shape], names = names)
dfpreds = pd.DataFrame({'predicted':predicted.flatten()}, index = index)['predicted']
dfpreds = dfpreds.unstack(level = 'y').swaplevel().sort_index()
dfpreds.index.names = ["DOY", 'Year']
dfpreds.reset_index(inplace=True)
dfpreds.columns = ["DOY","Year"] +segIds
dfpreds = dfpreds[list(df.columns)]
dfpreds = dfpreds.sort_values(by=['Year','DOY'])

def plotter(seg):
    actual = df[seg]
    predicted = dfpreds[seg]
    fig, ax = plt.subplots()
    x = range(len(actual))
    ax.plot(x,actual, label = 'Actual')
    ax.plot(x,predicted, label = 'Predicted')
    ax.legend()

df.columns
plotter('1703')
dfpreds.filter('Year' is 1)