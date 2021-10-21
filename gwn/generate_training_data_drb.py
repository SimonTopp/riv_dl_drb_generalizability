from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import os
import os.path
import pandas as pd
import numpy as np
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


def pull_lto_years(obs, ntest = 10, nval = 5, test='max'):
    """
    Pull years with min and max temperature values for leave time out tests
    :param obs: [csv] temperature observations to pull min and max years from
    :param ntest: [int] number of years to set aside for testing
    :param nval: [int] number of years to set aside for validation
    :parm test: [str] whether to set aside max temp years or min temp years for testing (option = 'max','min')
    """
    temp_df = pd.read_csv(obs)
    temp_df.date = pd.to_datetime(temp_df.date)
    temp_df = temp_df[temp_df.date >= '1980-10-01']

    temp_df['water_year'] = temp_df.date.dt.year.where(temp_df.date.dt.month < 10, temp_df.date.dt.year + 1)
    mean_temp = temp_df[['mean_temp_c', 'water_year']].groupby('water_year').mean()
    if test=='max':
        years_test = mean_temp.sort_values('mean_temp_c', ascending=False)[0:ntest]
    if test=='min':
        years_test = mean_temp.sort_values('mean_temp_c')[0:ntest]

    mean_temp = mean_temp[~mean_temp.index.isin(years_test.index)]

    ## We prefer consecutive years, we'll say 2000 and next five years that aren't in test
    ## This overlaps with original val years and has some back to back years for hot runs
    years_val = mean_temp.loc[mean_temp.index > 2000].iloc[0:nval]

    years_train = mean_temp[~mean_temp.index.isin(years_val.index)]

    def wy_to_date(wys):
        return {'start': [f"{i-1}-10-01" for i in wys.index], 'end': [f"{i}-9-30" for i in wys.index]}

    train_out = wy_to_date(years_train)
    test_out = wy_to_date(years_test)
    val_out = wy_to_date(years_val)

    return train_out, test_out, val_out

def llo_mask(seg_csv, partition, test_group, test = False):
    test_groups = pd.read_csv(seg_csv)
    segs = test_groups.loc[test_groups.test_group == test_group]
    segs = segs.seg_id_nat.dropna().astype(int)

    if test:
        partition_masked = partition.where(partition.seg_id_nat.isin(segs), np.nan)
    else:
        partition_masked = partition.where(~partition.seg_id_nat.isin(segs), np.nan)

    return partition_masked

def separate_trn_tst(
    dataset,
    train_start_date,
    train_end_date,
    val_start_date,
    val_end_date,
    test_start_date,
    test_end_date,
    lto=False,
    lto_type = 'max',
    llo=False,
    test_group=1,
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
    if lto:
        trn, tst, val = pull_lto_years('data/in/temperature_observations_drb.csv', test=lto_type)
        train = sel_partition_data(dataset, trn['start'],trn['end'])
        val = sel_partition_data(dataset, val['start'],val['end'])
        test = sel_partition_data(dataset,tst['start'],tst['end'])

    else:
        train = sel_partition_data(dataset, train_start_date, train_end_date)
        val = sel_partition_data(dataset, val_start_date, val_end_date)
        test = sel_partition_data(dataset, test_start_date, test_end_date)

    if llo:
        train = llo_mask('data/in/DRB_spatial/llo_groups.csv', train, test_group, test = False)
        val = llo_mask('data/in/DRB_spatial/llo_groups.csv', val, test_group, test=False)
        test = llo_mask('data/in/DRB_spatial/llo_groups.csv', test, test_group, test=True)

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
    if offset>1:
        period = offset
    else:
        period = int(offset*seq_len)
    num_batches = int(data_array.shape[1]//period)
    combined=[]
    for i in range(num_batches+1):
        idx = int(period*i)
        batch = data_array[:,idx-seq_len:idx,...]
        combined.append(batch)
    combined = [b for b in combined if b.shape[1]==seq_len]
    combined = np.asarray(combined)
    return combined


def convert_batch_reshape(dataset, seq_len=365, offset=1, y = False):
    """
    convert xarray dataset into numpy array, swap the axes, batch the array and
    reshape for training
    :param dataset: [xr dataset] data to be batched
    :param seq_len: [int] length of sequences (i.e., 365)
    :param offset: [float] 0-1, how to offset the batches (e.g., 0.5 means that
    the first batch will be 0-365 and the second will be 182-547)
    :param period: [int] 0-1, what proportion of the sequence to predict on
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
    if y and offset<1:
        period = int(seq_len*offset)
        reshaped = reshaped[:,-period:,...]
    elif y and offset > 1:
        period=int(offset)
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
    #obs_flow_file,
    pretrain_file,
    train_start_date,
    train_end_date,
    val_start_date,
    val_end_date,
    test_start_date,
    test_end_date,
    x_vars=None,
    #y_vars= ["seg_tave_water", "seg_outflow"],
    seq_length = 365,
    offset = 1,
    distfile=None,
    dist_idx_name="rowcolnames",
    dist_type="updown",
    out_file=None,
    normalize_y=False,
    clip_y=True,
    lto=False,
    lto_type='max',
    llo=False,
    test_group=1,
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

    x_data = ds_pre[x_vars].sortby(['seg_id_nat','date'])

    # make sure we don't have any weird input values
    check_if_finite(x_data)
    x_scl, x_std, x_mean = scale(x_data)

    x_trn, x_val, x_tst = separate_trn_tst(
        x_scl, #x_data,
        train_start_date,
        train_end_date,
        val_start_date,
        val_end_date,
        test_start_date,
        test_end_date,
        lto=lto,
        lto_type = lto_type
    )



    y_obs = [x_data.sortby(["seg_id_nat", "date"])]
    ds = xr.open_zarr(obs_temper_file)
    y_obs.append(ds)
    if "site_id" in ds.variables:
        del ds["site_id"]
    y_obs = xr.merge(y_obs, join="left")
    y_obs = y_obs[["temp_c"]].rename({'temp_c':'seg_tave_water'})

    y_pre = ds_pre[['seg_tave_water']]

    y_pre = xr.where(y_pre < -10, np.nan, y_pre)

    y_obs_trn, y_obs_val, y_obs_tst = separate_trn_tst(
        y_obs,
        train_start_date,
        train_end_date,
        val_start_date,
        val_end_date,
        test_start_date,
        test_end_date,
        lto=lto,
        lto_type=lto_type,
        llo=llo,
        test_group=test_group,
    )
    '''
    y_pre_trn, y_pre_val, y_pre_tst = separate_trn_tst(
        y_pre,
        train_start_date,
        train_end_date,
        val_start_date,
        val_end_date,
        test_start_date,
        test_end_date,
        lto=lto,
        lto_type=lto_type
    )
    '''

    if normalize_y:
        # scale y training data and get the mean and std
        y_obs_trn, y_std, y_mean = scale(y_obs_trn)
        y_pre, _, _ = scale(y_pre, y_std, y_mean)
    else:
        _, y_std, y_mean = scale(y_obs_trn)

    data = {
        "x_pre_train": convert_batch_reshape(x_scl, offset=offset, seq_len=seq_length),
        "x_train": convert_batch_reshape(x_trn, offset=offset, seq_len=seq_length),
        "x_val": convert_batch_reshape(x_val, offset=offset, seq_len=seq_length),
        "x_test": convert_batch_reshape(x_tst, offset=offset, seq_len=seq_length),
        "x_std": x_std.to_array().values,
        "x_mean": x_mean.to_array().values,
        "x_cols": np.array(x_vars),
        "ids_train": coord_as_reshaped_array(x_trn, "seg_id_nat", offset=offset, seq_len=seq_length),
        "dates_train": coord_as_reshaped_array(x_trn, "date", offset=offset, seq_len=seq_length),
        "ids_val": coord_as_reshaped_array(x_val, "seg_id_nat", offset=offset, seq_len=seq_length),
        "dates_val": coord_as_reshaped_array(x_val, "date", offset=offset, seq_len=seq_length),
        "ids_test": coord_as_reshaped_array(x_tst, "seg_id_nat", offset=offset, seq_len=seq_length),
        "dates_test": coord_as_reshaped_array(x_tst, "date", offset=offset, seq_len=seq_length),
        "y_pre_train": convert_batch_reshape(y_pre, offset=offset, seq_len=seq_length, y=clip_y),
        "y_train": convert_batch_reshape(y_obs_trn, offset=offset, seq_len=seq_length, y=clip_y),
        "y_val": convert_batch_reshape(y_obs_val, offset=offset, seq_len=seq_length, y=clip_y),
        "y_test": convert_batch_reshape(y_obs_tst, offset=offset, seq_len=seq_length, y=clip_y),
        'offset': np.array([offset]),
        "y_std": y_std.to_array().values,
        "y_mean": y_mean.to_array().values,
        }

    if distfile:
        data["dist_matrix"] = prep_adj_matrix(
            infile=distfile,
            dist_type=dist_type,
            dist_idx_name=dist_idx_name,
            #segs=segs,
        )

    if out_file:
        if not os.path.exists(os.path.split(out_file)[0]):
            os.makedirs(os.path.split(out_file)[0])

        np.savez_compressed(out_file, **data)
    return data


def sort_dist_matrix(mat, row_col_names, segs=None):
    """
    sort the distance matrix by id
    :return:
    """
    if segs is not None:
        row_col_names = row_col_names.astype(type(segs[0]))
    df = pd.DataFrame(mat, columns=row_col_names, index=row_col_names)
    if segs:
        df = df[segs]
        df = df.loc[segs]
    df = df.sort_index(axis=0)
    df = df.sort_index(axis=1)
    return df


def prep_adj_matrix(infile, dist_type, dist_idx_name, segs=None, out_file=None):
    """
    process adj matrix.
    **The resulting matrix is sorted by id **
    :param infile: [str] path to the distance matrix .npz file
    :param dist_type: [str] type of distance matrix ("upstream", "downstream" or
    "updown")
    :param dist_idx_name: [str] name of index to sort dist_matrix by. This is
    the name of an array in the distance matrix .npz file
    :param segs: [list-like] which segments to prepare the data for
    :param out_file: [str] path to save the .npz file to
    :return: [numpy array] processed adjacency matrix
    """
    adj_matrices = np.load(infile)
    adj = adj_matrices[dist_type]
    adj = sort_dist_matrix(adj, adj_matrices[dist_idx_name], segs=segs)
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
        np.savez_compressed(out_file, dist_matrix=A_hat)
    return A_hat
