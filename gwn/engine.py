import torch.optim as optim
from gwn.model import *
import gwn.util as util

class trainer():
    def __init__(self, scaler, in_dim, num_nodes, nhid , dropout, lrate, wdecay, device, supports, gcn_bool, addaptadj, aptinit, out_dim, kernel, blocks, layers):
        self.model = gwnet(device, num_nodes, dropout, supports=supports, gcn_bool=gcn_bool, addaptadj=addaptadj,
                           aptinit=aptinit, in_dim=in_dim, out_dim=out_dim, residual_channels=nhid,
                           dilation_channels=nhid, skip_channels=nhid * 4, end_channels=nhid * 8, kernel_size=kernel, layers=layers)
        #skip and end were 8 and 16 respectively
        self.model.to(device)
        self.optimizer = optim.Adam(self.model.parameters(), lr=lrate, weight_decay=wdecay)
        self.loss = util.rmse #was mae
        self.scaler = scaler
        self.clip = 5

    def train(self, input, real_val):
        self.model.train()
        self.optimizer.zero_grad()
        input = nn.functional.pad(input,(1,0,0,0))
        #input = self.scaler.transform(input)
        output = self.model(input)
        output = output.transpose(1,3)
        #output = [batch_size,12,num_nodes,1]
        real = torch.unsqueeze(real_val,dim=1)
        #real = self.scaler.inverse_transform(real)
        #predict = self.scaler.inverse_transform(output)
        predict = output

        loss = self.loss(predict, real)
        loss.backward()
        if self.clip is not None:
            torch.nn.utils.clip_grad_norm_(self.model.parameters(), self.clip)
        self.optimizer.step()
        mape = util.masked_mape(predict,real).item()
        rmse = util.masked_rmse(predict,real).item()
        return loss.item(),mape,rmse

    def eval(self, input, real_val):
        self.model.eval()
        input = nn.functional.pad(input,(1,0,0,0))
        #input = self.scaler.transform(input)
        output = self.model(input)
        output = output.transpose(1,3)
        #output = [batch_size,12,num_nodes,1]
        real = torch.unsqueeze(real_val,dim=1)
        #real = self.scaler.inverse_transform(real)
        #predict = self.scaler.inverse_transform(output)
        predict = output
        loss = self.loss(predict, real)
        mape = util.masked_mape(predict,real).item()
        rmse = util.masked_rmse(predict,real).item()
        return loss.item(),mape,rmse
