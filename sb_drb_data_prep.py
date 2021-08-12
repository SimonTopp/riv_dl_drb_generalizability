# -*- coding: utf-8 -*-
"""
Created on Thu Aug  5 13:17:25 2021

@author: jbarclay
"""

### modifying sciencebase data to be prepped for river-dl
#https://www.sciencebase.gov/catalog/item/5f6a287382ce38aaa2449131
#https://www.sciencebase.gov/catalog/item/5f6a289982ce38aaa2449135

import os
import numpy as np
import pandas as pd
import xarray as xr

os.chdir(r'C:\Users\jbarclay\OneDrive - DOI\StreamTemp\Analysis\Data\ScienceBaseData')


#SNTemp inputs and outputs
sntempDF = pd.read_csv("sntemp_inputs_outputs/sntemp_inputs_outputs.csv")
sntempDF.date=sntempDF.date.astype('datetime64[ns]')
sntempDF.set_index(['date','seg_id_nat'],inplace=True, drop=True)
sntempArr = sntempDF.to_xarray()
sntempArr.to_zarr('uncal_sntemp_input_output', mode='w')



#flow observations
flowDF = pd.read_csv("flow_observations_drb/flow_observations_drb.csv")
flowDF = flowDF[['seg_id_nat','date','discharge_cms']]
flowDF.date=flowDF.date.astype('datetime64[ns]')
nSegs = len(np.unique(flowDF.seg_id_nat))
nDates = len(np.unique(flowDF.date))
flowDF.set_index(['seg_id_nat','date'], drop=True, inplace=True)
flowArr = flowDF.to_xarray().chunk({'seg_id_nat':nSegs,'date':nDates})
flowArr.to_zarr('obs_flow_full', mode='w')




#temp observations
tempDF = pd.read_csv("temperature_observations_drb/temperature_observations_drb.csv")
#drop unneeded columns
tempDF = tempDF[['seg_id_nat','date','mean_temp_c']]
tempDF.rename(columns={'mean_temp_c':'temp_c'},inplace=True)
tempDF.date=tempDF.date.astype('datetime64[ns]')
nSegs = len(np.unique(tempDF.seg_id_nat))
nDates = len(np.unique(tempDF.date))
tempDF.set_index(['seg_id_nat','date'], drop=True, inplace=True)
tempArr = tempDF.to_xarray().chunk({'seg_id_nat':nSegs,'date':nDates})
tempArr.to_zarr('obs_temp_full', mode='w')


