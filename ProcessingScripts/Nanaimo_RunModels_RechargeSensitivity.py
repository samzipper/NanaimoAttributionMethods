## Nanaimo_RunModels_RechargeSensitivity.py
# This script is intended to run the Nanaimo LD model at different recharge
# levels with no pumping and plot the water table depth.

import os
import flopy
import flopy.utils.binaryfile as bf
import numpy as np
import matplotlib.pyplot as plt

# where is your MODFLOW-2005 executable?
path2mf = 'C:/Users/Sam/Dropbox/Work/Models/MODFLOW/MF2005.1_12/bin/mf2005.exe'

# path to Tom D's modflow model files
dir_mf = 'Z:/2.active_projects/TomDallemagne/PROJECTS/MAIN_PROJECT/MAP_LD/MAP_LD_10LAY_ELEVATION/ED_MAP_LD_10LAY_ELEVATION.data/MODFLOW/NumericalGrid1/Run/MODFLOW-2005'

# path to new folder to store runs
dir_new = 'Z:/2.active_projects/Zipper/2.Model_data/NanaimoAttributionMethods/models/LD/ELEV'

# model name
modelname = 'ED_MAP_LD_10LAY_ELEVATION'

# load model
mf = flopy.modflow.Modflow.load(modelname+".MODFLOW.IN", 
                                model_ws=dir_mf, 
                                verbose=False,
                                check=False, 
                                exe_name=path2mf)

# base package
bas = mf.bas6

# there are several 'island' cells (no-flow on all sides) 
# leading to major instability when recharge is added: disable this
ibound = bas.ibound[:,:,:]
ibound[:,28,33] = 0
ibound[:,86,70] = 0
bas.ibound = ibound

# extract discretization info
dis = mf.dis

# get bottom
botm = dis.botm[9,:,:]
top = dis.top[:,:]

# extract recharge
rch = mf.rch

# extract lpf, pcg
lpf = mf.lpf
pcg = mf.pcg
pcg.mxiter = 100
pcg.iter1 = 30
pcg.hclose = 0.01
pcg.rclose = 0.01
    
## start with NORCH case
# move workspace
mf.model_ws = os.path.join(dir_new, 'NORCH')

# set recharge
rch.rech = 0

# write input
mf.write_input()

# run model
success, mfoutput = mf.run_model()
if not success:
    raise Exception('MODFLOW did not terminate normally.')

### look at output
## look at head output
# Create the headfile object
h = bf.HeadFile(os.path.join(mf.model_ws, modelname+'.hds') , text='head')
head = h.get_data(totim=1)
    
# set nodata values to nan
head[head < -1e+10] = np.nan  # dry cells
head[head > 1e+10] = np.nan  # inactive cells

# save head to text file
wtd = top - head[9,:,:]
np.savetxt(os.path.join(mf.model_ws, 'head.txt'), head[9,:,:], delimiter=',')
np.savetxt(os.path.join(mf.model_ws, 'wtd.txt'), wtd, delimiter=',')

# plot
plt.imshow(wtd)
plt.colorbar()

## now: go through different recharge magnitudes
for rech_rate in [10,100,1000]:
    print('rch is ', rech_rate)
    
    # move workspace
    mf.model_ws = os.path.join(dir_new, 'RCH'+str(rech_rate))

    # set recharge
    rch.rech = rech_rate/(1000*365)  # convert mm/yr to m/day

    # write input
    mf.write_input()

    # run model
    success, mfoutput = mf.run_model()
    if not success:
        raise Exception('MODFLOW did not terminate normally.')

    # look at output
    h = bf.HeadFile(os.path.join(mf.model_ws, modelname+'.hds') , text='head')
    head = h.get_data(totim=1)

    # set nodata values to nan
    head[head < -1e+10] = np.nan  # dry cells
    head[head > 1e+10] = np.nan  # inactive cells

    # save head to text file
    wtd = top - head[9,:,:]
    np.savetxt(os.path.join(mf.model_ws, 'head.txt'), head[9,:,:], delimiter=',')
    np.savetxt(os.path.join(mf.model_ws, 'wtd.txt'), wtd, delimiter=',')
    