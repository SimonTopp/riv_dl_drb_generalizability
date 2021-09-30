import numpy as np
import pandas as pd
import datetime as dt
import matplotlib.pyplot as plt

### Lets start with temperature
### Function to pull hottest and coolest years based on mean watertemp
def pull_lto_years(obs, ntest = 10, nval = 5, test='max'):
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

trn, ts, val =pull_lto_years('data/in/temperature_observations_drb.csv', test='min')


### Function to split up segments by ecoregion for block CV