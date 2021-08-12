from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import argparse
import numpy as np
import os
import pandas as pd
import util

import os.path

import pandas as pd
import numpy as np
import yaml
import xarray as xr
import datetime
import pickle

def scale(dataset, std=None, mean=None):
    """
    scale the data so it has a standard deviation of 1 and a mean of zero
    :param dataset: [xr dataset] input or output data
    :param std: [xr dataset] standard deviation if scaling test data with dims
    :param mean: [xr dataset] mean if scaling test data with dims
    :return: scaled data with original dims
    """
    if not isinstance(std, xr.Dataset) or not isinstance(mean, xr.Dataset):
        std = dataset.std(skipna=True)
        mean = dataset.mean(skipna=True)
    # adding small number in case there is a std of zero
    scaled = (dataset - mean) / (std + 1e-10)
    check_if_finite(std)
    check_if_finite(mean)
    return scaled, std, mean


def sel_partition_data(dataset, start_dates, end_dates):
    """
    select the data from a date range or a set of date ranges
    :param dataset: [xr dataset] input or output data with date dimension
    :param start_dates: [str or list] fmt: "YYYY-MM-DD"; date(s) to start period
    (can have multiple discontinuos periods)
    :param end_dates: [str or list] fmt: "YYYY-MM-DD"; date(s) to end period
    (can have multiple discontinuos periods)
    :return: dataset of just those dates
    """
    # if it just one date range
    if isinstance(start_dates, str):
        if isinstance(end_dates, str):
            return dataset.sel(date=slice(start_dates, end_dates))
        else:
            raise ValueError("start_dates is str but not end_date")
    # if it's a list of date ranges
    elif isinstance(start_dates, list) or isinstance(start_dates, tuple):
        if len(start_dates) == len(end_dates):
            data_list = []
            for i in range(len(start_dates)):
                date_slice = slice(start_dates[i], end_dates[i])
                data_list.append(dataset.sel(date=date_slice))
            return xr.concat(data_list, dim="date")
        else:
            raise ValueError("start_dates and end_dates must have same length")
    else:
        raise ValueError("start_dates must be either str, list, or tuple")


def separate_trn_tst(
    dataset,
    train_start_date,
    train_end_date,
    val_start_date,
    val_end_date,
    test_start_date,
    test_end_date,
):
    """
    separate the train data from the test data according to the start and end
    dates. This assumes your training data is in one continuous block and all
    the dates that are not in the training are in the testing.
    :param dataset: [xr dataset] input or output data with dims
    :param train_start_date: [str or list] fmt: "YYYY-MM-DD"; date(s) to start
    train period (can have multiple discontinuos periods)
    :param train_end_date: [str or list] fmt: "YYYY-MM-DD"; date(s) to end train
     period (can have multiple discontinuos periods)
    :param val_start_date: [str or list] fmt: "YYYY-MM-DD"; date(s) to start
     validation period (can have multiple discontinuos periods)
    :param val_end_date: [str or list] fmt: "YYYY-MM-DD"; date(s) to end
    validation period (can have multiple discontinuos periods)
    :param test_start_date: [str or list] fmt: "YYYY-MM-DD"; date(s) to start
    test period (can have multiple discontinuos periods)
    :param test_end_date: [str or list] fmt: "YYYY-MM-DD"; date(s) to end test
    period (can have multiple discontinuos periods)
    """
    train = sel_partition_data(dataset, train_start_date, train_end_date)
    val = sel_partition_data(dataset, val_start_date, val_end_date)
    test = sel_partition_data(dataset, test_start_date, test_end_date)
    return train, val, test


def split_into_batches(data_array, seq_len=365, offset=1):
    """
    split training data into batches with size of batch_size
    :param data_array: [numpy array] array of training data with dims [nseg,
    ndates, nfeat]
    :param seq_len: [int] length of sequences (i.e., 365)
    :param offset: [float] 0-1, how to offset the batches (e.g., 0.5 means that
    the first batch will be 0-365 and the second will be 182-547)
    :return: [numpy array] batched data with dims [nbatches, nseg, seq_len
    (batch_size), nfeat]
    """
    combined = []
    for i in range(int(1 / offset)):
        start = int(i * offset * seq_len)
        idx = np.arange(start=start, stop=data_array.shape[1] + 1, step=seq_len)
        split = np.split(data_array, indices_or_sections=idx, axis=1)
        # add all but the first and last batch since they will be smaller
        combined.extend([s for s in split if s.shape[1] == seq_len])
    combined = np.asarray(combined)
    return combined


def read_multiple_obs(obs_files, x_data):
    """
    read and format multiple observation files. we read in the pretrain data to
    make sure we have the same indexing.
    :param obs_files: [list] list of filenames of observation files
    :param pre_train_file: [str] the file of pre_training data
    :return: [xr dataset] the observations in the same time
    """
    obs = [x_data.sortby(["seg_id_nat", "date"])]
    for filename in obs_files:
        ds = xr.open_zarr(filename)
        obs.append(ds)
        if "site_id" in ds.variables:
            del ds["site_id"]
    obs = xr.merge(obs, join="left")
    obs = obs[["temp_c", "discharge_cms"]]
    obs = obs.rename(
        {"temp_c": "seg_tave_water", "discharge_cms": "seg_outflow"}
    )
    return obs


def reshape_for_training(data):
    """
    reshape the data for training
    :param data: training data (either x or y or mask) dims: [nbatch, nseg,
    len_seq, nfeat/nout]
    :return: reshaped data [nbatch * nseg, len_seq, nfeat/nout]
    """
    n_batch, n_seg, seq_len, n_feat = data.shape
    return np.reshape(data, [n_batch * n_seg, seq_len, n_feat])


def get_exclude_start_end(exclude_grp):
    """
    get the start and end dates for the exclude group
    :param exclude_grp: [dict] dictionary representing the exclude group from
    the exclude yml file
    :return: [tuple of datetime objects] start date, end date
    """
    start = exclude_grp.get("start_date")
    if start:
        start = datetime.datetime.strptime(start, "%Y-%m-%d")

    end = exclude_grp.get("end_date")
    if end:
        end = datetime.datetime.strptime(end, "%Y-%m-%d")
    return start, end



def convert_batch_reshape(dataset, seq_len=365, offset=1, y = False, period = np.nan):
    """
    convert xarray dataset into numpy array, swap the axes, batch the array and
    reshape for training
    :param dataset: [xr dataset] data to be batched
    :param seq_len: [int] length of sequences (i.e., 365)
    :param offset: [float] 0-1, how to offset the batches (e.g., 0.5 means that
    the first batch will be 0-365 and the second will be 182-547)
    :return: [numpy array] batched and reshaped dataset
    """
    # convert xr.dataset to numpy array
    dataset = dataset.transpose("seg_id_nat", "date")

    arr = dataset.to_array().values

    # if the dataset is empty, just return it as is
    if dataset.date.size == 0:
        return arr

    # before [nfeat, nseg, ndates]; after [nseg, ndates, nfeat]
    # this is the order that the split into batches expects
    arr = np.moveaxis(arr, 0, -1)

    # batch the data
    # after [nbatch, nseg, seq_len, nfeat]
    batched = split_into_batches(arr, seq_len=seq_len, offset=offset)

    # reshape data
    # after [nseq, seq_len, nseg, nfeat]
    #reshaped = reshape_for_training(batched)
    reshaped = np.moveaxis(batched, [0,1,2,3], [0,2,1,3])
    if y & np.isfinite(period):
        reshaped = reshaped[:,-period:,...]

    return reshaped


def coord_as_reshaped_array(dataset, coord_name, seq_len=365, offset=1):
    # I need one variable name. It can be any in the dataset, but I'll use the
    # first
    first_var = next(iter(dataset.data_vars.keys()))
    coord_array = xr.broadcast(dataset[coord_name], dataset[first_var])[0]
    new_var_name = coord_name + "1"
    dataset[new_var_name] = coord_array
    reshaped_np_arr = convert_batch_reshape(
        dataset[[new_var_name]], seq_len=seq_len, offset=offset
    )
    return reshaped_np_arr


def check_if_finite(xarr):
    assert np.isfinite(xarr.to_array().values).all()


def prep_data(
    obs_temper_file,
    obs_flow_file,
    pretrain_file,
    #distfile,
    train_start_date,
    train_end_date,
    val_start_date,
    val_end_date,
    test_start_date,
    test_end_date,
    x_vars=None,
    y_vars= ["seg_tave_water", "seg_outflow"],
    seq_length = 365,
    offset = 1,
    period = None,
    primary_variable="temp",
    #catch_prop_file=None,
    #exclude_file=None,
    #log_q=False,
    out_file=None,
    #segs=None,
    normalize_y=False,
):
    """
    prepare input and output data for DL model training read in and process
    data into training and testing datasets. the training and testing data are
    scaled to have a std of 1 and a mean of zero
    :param obs_temper_file: [str] temperature observations file (csv)
    :param obs_flow_file:[str] discharge observations file (csv)
    :param pretrain_file: [str] the file with the pretraining data (SNTemp data)
    :param distfile: [str] path to the distance matrix .npz file
    :param train_start_date: [str or list] fmt: "YYYY-MM-DD"; date(s) to start
    train period (can have multiple discontinuos periods)
    :param train_end_date: [str or list] fmt: "YYYY-MM-DD"; date(s) to end train
    period (can have multiple discontinuos periods)
    :param val_start_date: [str or list] fmt: "YYYY-MM-DD"; date(s) to start
    validation period (can have multiple discontinuos periods)
    :param val_end_date: [str or list] fmt: "YYYY-MM-DD"; date(s) to end
    validation period (can have multiple discontinuos periods)
    :param test_start_date: [str or list] fmt: "YYYY-MM-DD"; date(s) to start
    test period (can have multiple discontinuos periods)
    :param test_end_date: [str or list] fmt: "YYYY-MM-DD"; date(s) to end test
    period (can have multiple discontinuos periods)
    :param x_vars: [list] variables that should be used as input. If None, all
    of the variables will be used
    :param primary_variable: [str] which variable the model should focus on
    'temp' or 'flow'. This determines the order of the variables.
    :param catch_prop_file: [str] the path to the catchment properties file. If
    left unfilled, the catchment properties will not be included as predictors
    :param exclude_file: [str] path to exclude file
    :param log_q: [bool] whether or not to take the log of discharge in training
    :param out_file: [str] file to where the values will be written
    :returns: training and testing data along with the means and standard
    deviations of the training input and output data
            'y_trn_pre': batched, scaled, and centered output data for entire
                         period of record of SNTemp [n_samples, seq_len, n_out]
            'y_obs_trn': batched, scaled, and centered output observation data
                         for the training period
            'y_trn_obs_std': standard deviation of the y observations training
                             data [n_out]
            'y_trn_obs_mean': mean of the observation training data [n_out]
            'y_obs_tst': un-batched, unscaled, uncentered observation data for
                         the test period [n_yrs, n_seg, len_seq, n_out]
            'dates_ids_trn: batched dates and national seg ids for training data
                            [n_samples, seq_len, 2]
            'dates_ids_tst: un-batched dates and national seg ids for testing
                            data [n_yrs, n_seg, len_seq, 2]
    """
    ds_pre = xr.open_zarr(pretrain_file)

    x_data = ds_pre[x_vars]

    # make sure we don't have any weird input values
    check_if_finite(x_data)
    x_trn, x_val, x_tst = separate_trn_tst(
        x_data,
        train_start_date,
        train_end_date,
        val_start_date,
        val_end_date,
        test_start_date,
        test_end_date,
    )

    x_scl, x_std, x_mean = scale(x_data)

    x_trn_scl, _, _ = scale(x_trn, std=x_std, mean=x_mean)
    x_val_scl, _, _ = scale(x_val, std=x_std, mean=x_mean)
    x_tst_scl, _, _ = scale(x_tst, std=x_std, mean=x_mean)

    y_obs = read_multiple_obs([obs_temper_file, obs_flow_file], x_data)
    y_obs = y_obs[y_vars]
    y_pre = ds_pre[y_vars]

    y_obs_trn, y_obs_val, y_obs_tst = separate_trn_tst(
        y_obs,
        train_start_date,
        train_end_date,
        val_start_date,
        val_end_date,
        test_start_date,
        test_end_date,
    )
    y_pre_trn, y_pre_val, y_pre_tst = separate_trn_tst(
        y_pre,
        train_start_date,
        train_end_date,
        val_start_date,
        val_end_date,
        test_start_date,
        test_end_date,
    )

    if normalize_y:
        # scale y training data and get the mean and std
        y_obs_trn, y_std, y_mean = scale(y_obs_trn)
        y_pre_trn, _, _ = scale(y_pre_trn, y_std, y_mean)
    else:
        _, y_std, y_mean = scale(y_obs_trn)

    data = {
        "x_train": convert_batch_reshape(x_trn_scl, offset=offset, seq_len=seq_length),
        "x_val": convert_batch_reshape(x_val_scl, offset=offset, seq_len=seq_length),
        "x_test": convert_batch_reshape(x_tst_scl, offset=offset, seq_len=seq_length),
        "x_std": x_std.to_array().values,
        "x_mean": x_mean.to_array().values,
        "x_cols": np.array(x_vars),
        "ids_train": coord_as_reshaped_array(x_trn, "seg_id_nat", offset=offset, seq_len=seq_length),
        "dates_train": coord_as_reshaped_array(x_trn, "date", offset=offset, seq_len=seq_length),
        "ids_val": coord_as_reshaped_array(x_val, "seg_id_nat", offset=offset, seq_len=seq_length),
        "dates_val": coord_as_reshaped_array(x_val, "date", offset=offset, seq_len=seq_length),
        "ids_test": coord_as_reshaped_array(x_tst, "seg_id_nat", offset=offset, seq_len=seq_length),
        "dates_test": coord_as_reshaped_array(x_tst, "date", offset=offset, seq_len=seq_length),
        "y_pre_train": convert_batch_reshape(y_pre_trn, offset=offset, seq_len=seq_length, y=True, period=period),
        "y_train": convert_batch_reshape(y_obs_trn, offset=offset, seq_len=seq_length, y=True, period=period),
        "y_val": convert_batch_reshape(y_obs_val, offset=offset, seq_len=seq_length, y=True, period=period),
        "y_test": convert_batch_reshape(y_obs_tst, offset=offset, seq_len=seq_length, y=True, period=period),
        "y_vars": np.array(y_vars),
        'period': np.array([period]),
        'y_pre_train_val': convert_batch_reshape(y_pre_val, offset=offset, seq_len=seq_length, y=True, period=period),
        'y_pre_train_test': convert_batch_reshape(y_pre_tst, offset=offset, seq_len=seq_length, y=True, period=period),
        "y_std": y_std.to_array().values,
        "y_mean": y_mean.to_array().values,
        }

    if out_file:
        if not os.path.exists(out_file):
            os.makedirs(out_file)
            os.makedirs(out_file+'/train_val_drb')
            os.makedirs(out_file+'/train_val_drb/tmp')


        '''
        np.savez_compressed(os.path.join(out_file, 'pre_train.npz'),
                            x=data['x_train'],
                            y=data['y_pre_train'])

        np.savez_compressed(os.path.join(out_file,'train.npz'),
                            x=data['x_train'],
                            y=data['y_obs_train'],
                            )

        np.savez_compressed(os.path.join(out_file, 'test.npz'),
                            x=data['x_test'],
                            y=data['y_obs_tst'],
                            )

        np.savez_compressed(os.path.join(out_file,'val.npz'),
                            x=data['x_val'],
                            y=data['y_obs_val'],
                            )
        '''
        np.savez_compressed(os.path.join(out_file,'data.npz'), **data)
    return data


def prep_adj_matrix(infile, dist_type, out_file=None):
    """
    process adj matrix.
    **The resulting matrix is sorted by seg_id_nat **
    :param infile:
    :param dist_type: [str] type of distance matrix ("upstream", "downstream" or
    "updown")
    :param out_file:
    :return: [numpy array] processed adjacency matrix
    """
    adj_matrices = np.load(infile)
    adj = adj_matrices[dist_type]
    adj_full = sort_dist_matrix(adj, adj_matrices["rowcolnames"])
    adj = adj_full[2]
    adj = np.where(np.isinf(adj), 0, adj)
    adj = -adj
    mean_adj = np.mean(adj[adj != 0])
    std_adj = np.std(adj[adj != 0])
    adj[adj != 0] = adj[adj != 0] - mean_adj
    adj[adj != 0] = adj[adj != 0] / std_adj
    adj[adj != 0] = 1 / (1 + np.exp(-adj[adj != 0]))

    I = np.eye(adj.shape[0])
    A_hat = adj.copy() + I
    D = np.sum(A_hat, axis=1)
    D_inv = D ** -1.0
    D_inv = np.diag(D_inv)
    A_hat = np.matmul(D_inv, A_hat)
    if out_file:
        out_dm = [adj_full[0], adj_full[1], A_hat]
        with open(out_file, 'wb') as f:
            pickle.dump(out_dm, f, protocol=2)

    return adj_full[0], adj_full[1], A_hat


def sort_dist_matrix(mat, row_col_names):
    """
    sort the distance matrix by seg_id_nat
    :return:
    """
    df = pd.DataFrame(mat, columns=row_col_names, index=row_col_names)
    df = df.sort_index(axis=0)
    df = df.sort_index(axis=1)
    sensor_id_to_ind = {}
    for i, sensor_id in enumerate(df.columns):
        sensor_id_to_ind[sensor_id] = i

    return row_col_names, sensor_id_to_ind, df

##check = prep_adj_matrix('data_DRB/distance_matrix_subset.npz', 'upstream', 'data/adj_mat/adj_mx_subset.pkl')
#if __name__ == "__main__":
'''
check2 = prep_data(obs_temper_file='data/obs_temp_full',
    obs_flow_file='data/obs_flow_full',
    pretrain_file='data/uncal_sntemp_input_output',
    train_start_date=['1985-10-01', '2016-10-01'],
    train_end_date=['2006-09-30', '2020-09-30'],
    val_start_date='2006-10-01',
    val_end_date='2016-09-30',
    test_start_date=['1980-10-01', '2020-10-01'],
    test_end_date=['1985-09-30', '2021-09-30'],
    x_vars=["seg_rain", "seg_tave_air", "seginc_swrad", "seg_length", "seginc_potet", "seg_slope", "seg_humid",
          "seg_elev"],
    y_vars=['seg_tave_water'],
    primary_variable='temp',
    seq_length=60,
    period=15,
    offset=1,
    out_file = 'data/test')

'''