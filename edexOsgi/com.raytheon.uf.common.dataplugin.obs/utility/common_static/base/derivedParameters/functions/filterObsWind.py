##
##

import numpy as np
from numpy import *
np.set_printoptions(suppress=True)

# Wind < 35 ---- FOR SPD
def execute(spd):
    for count, i in enumerate(spd):
        if i >= 35:
            spd[count]=-9999
        if np.isnan(i):
            spd[count]=-9999
    return spd

# 35 =< Wind < 50 ---- FOR SPD
def execute2(spd):
    for count, i in enumerate(spd):
        if (i < 35) or (i >=50):
            spd[count]=-9999
        if np.isnan(i):
            spd[count]=-9999
    return spd

# Wind >= 50 ---- FOR SPD
def execute3(spd):
    for count, i in enumerate(spd):
        if i < 50:
            spd[count]=-9999
        if np.isnan(i):
            spd[count]=-9999
    return spd

# Wind < 35 ---- FOR GST
def execute4(gst):
    for count, i in enumerate(gst):
        if i >= 35:
            gst[count]=-9999
        if np.isnan(i):
            gst[count]=-9999
    return gst

# 35 =< Wind < 50 ---- FOR GST
def execute5(gst):
    for count, i in enumerate(gst):
        if (i < 35) or (i >=50):
            gst[count]=-9999
        if np.isnan(i):
            gst[count]=-9999
    return gst

# Wind >= 50 ---- FOR GST
def execute6(gst):
    for count, i in enumerate(gst):
        if i < 50:
            gst[count]=-9999
        if np.isnan(i):
            gst[count]=-9999
    return gst

# Wind < 35 ---- FOR DIR
def execute7(spd, gst, dir):
    for count, i in enumerate(spd):
        if (i >= 35) or (gst[count] >= 35):
            dir[count]=-9999
        if np.isnan(dir[count]):
            dir[count]=-9999
    return dir

# 35 =< Wind < 50 ---- FOR DIR
def execute8(spd, gst, dir):
    for count, i in enumerate(spd):
        if ((i < 35) or (i >=50)) or ((gst[count] < 35) or (gst[count] >=50)):
            dir[count]=-9999
        if np.isnan(dir[count]):
            dir[count]=-9999
    return dir

# Wind >= 50 ---- FOR DIR
def execute9(spd, gst, dir):
    for count, i in enumerate(spd):
        if (i < 50) or (gst[count] < 50):
            dir[count]=-9999
        if np.isnan(dir[count]):
            dir[count]=-9999
    return dir