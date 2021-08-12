#!/bin/bash

JPORT=`shuf -i 8400-9400 -n 1`

export PATH="$PATH:$HOME/miniconda3/bin"
source activate rgcn2
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/cm/local/apps/cuda/libs/current/lib64

echo "ssh -N -L $JPORT:`hostname`:$JPORT $USER@tallgrass.cr.usgs.gov"

jupyter lab --ip '*' --no-browser --port $JPORT --notebook-dir $HOME &

wait
