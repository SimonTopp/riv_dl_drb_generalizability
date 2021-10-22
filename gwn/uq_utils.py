import util

##############
#### UQ Utilities following Lu et al (in review)
##############

def load_data_uq(cat_data,
                 preds_pre_train,
                 preds_train,
                 preds_val,
                 preds_test,
                 batch_size
                 ):

    if isinstance(cat_data,str):
        cat_data = np.load(os.path.join(cat_data, 'prepped.npz'))

    data = {}
    data['y_preds_pre_train'] = preds_pre_train.reshape(-1,1)
    data['y_preds_train'] = preds_train.reshape(-1, 1)
    data['y_preds_val'] = preds_val.reshape(-1, 1)
    data['y_preds_test'] = preds_test.reshape(-1, 1)

    len_x = cat_data['x_train'].shape[3]
    categories = ['pre_train','train','val','test']

    for cat in categories:
        data['x_'+cat] = cat_data['x_'+cat].reshape(-1, len_x)
        data['y_'+cat] = cat_data['y_'+cat].reshape(-1,1)
        data[cat+'_resid'] = cat_data['y_'+cat] - cat_data['y_preds_'+cat]
        data[cat+'resid_up'] = data[cat+'_resid'][cat+'_resid' > 0].unsqueeze(1)
        data[cat + 'resid_down'] = data[cat + '_resid'][cat + '_resid' < 0].unsqueeze(1)
        data[cat+'x_up'] = data['x_'+cat][data[cat+'_resid'].flatten() >0,...]
        data[cat + 'x_down'] = data['x_' + cat][data[cat + '_resid'].flatten() < 0, ...]

    scaler = util.StandardScaler(mean=cat_data['y_mean'][0],
                                 std=cat_data['y_std'][0])

    for cat in categories:
        for i in ['up','down']:
            data[f"{cat}_{i}_loader']" = util.DataLoader(data[f"{cat}_x_{i}"], data[f"{cat}_resid_{i}"], batch_size)

    data['scaler'] = scaler

    return data

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


def train_uq(model, data, epochs_pre, epochs, out_dir, ci_bound):
    tmp_path = os.path.join(out_dir, 'tmp')
    os.makedirs(tmp_path, exist_ok=True)

    criterion = nn.MSELoss()
    optimizer = optim.SGD(net_up.parameters(), lr=0.01)
    train_log = pd.DataFrame(columns=['split', 'epoch', 'mse'])

    ## Pretraining
    model.train()
    for i in range(1,epochs_pre+1):
        data[f"pre_train_{ci_bound}_loader'].shuffle()
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

    for i in range(1, epochs + 1):
        train_mse = []
        data['train_{ci_bound}_loader'].shuffle()
        model.train()

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
        his_loss.append(mvalid_mse)

        log = 'Epoch: {:03d}, Train MSE: {:.4f}, Valid MSE: {:.4f}'
        print(log.format(i,mtrain_mse, mvalid_rmse),flush=True)
        train_log = train_log.append({'split':'train','epoch':i,'mse':mtrain_mse}, ignore_index=True)
        train_log = train_log.append({'split': 'val', 'epoch': i, 'mse': mvalid_mse}, ignore_index=True)
        torch.save(engine.model.state_dict(), out_dir+'/tmp/uq_'+ci_bound+'_'+expid+"_epoch_"+str(i)+"_"+str(round(mvalid_rmse,2))+".pth")

    # Save the training log
    train_log.to_csv(os.path.join(out_dir,'uq_'+ci_bound+'_train_log.csv'),index=False)
    bestid = np.argmin(his_loss)
    model.load_state_dict(torch.load(
        out_dir+'/tmp/uq_'+ci_bound+'_'+expid+"_epoch_" + str(bestid + 1) + "_" + str(round(his_loss[bestid], 2)) + ".pth"))
    torch.save(model.state_dict(), out_dir + "/uq_"+ci_bound+'_'+"_weights_final.pth")

    return model



def calc_uq(out_dir,
            prepped,
            y_pred_pre_train,
            y_pred_train,
            y_pred_val,
            y_pred_test,
            scale_y = True,
            quantile=0.90):

    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    len_x=xtrain.shape[3]
    #print("training on " + device)

    data = load_data_uq(prepped, y_pred_pre_train,y_pred_train,y_pred_val,y_pred_test,500)

    net_up = UQ_Net_std(len_x).to(device)

    net_up = train_uq(net_up, data, 25, 75, out_dir, 'up')

    net_down = UQ_Net_std(len_x).to(device)

    net_down = train_uq(net_down, data, 25, 75, out_dir, 'down')

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
