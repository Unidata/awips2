##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

import numpy as np
from numpy import *
np.set_printoptions(suppress=True)

# Wind < 20 ---- FOR SPD
def execute(spd):
    for count, i in enumerate(spd):
        if i >= 20:
            spd[count]=-9999
        if np.isnan(i):
            spd[count]=-9999
    return spd

# 20 =< Wind < 34 ---- FOR SPD
def execute2(spd):
    for count, i in enumerate(spd):
        if (i < 20) or (i >=34):
            spd[count]=-9999
        if np.isnan(i):
            spd[count]=-9999
    return spd

# 34 =< Wind < 48 ---- FOR SPD
def execute3(spd):
    for count, i in enumerate(spd):
        if (i < 34) or (i >=48):
            spd[count]=-9999
        if np.isnan(i):
            spd[count]=-9999
    return spd

# 48 =< Wind < 64 ---- FOR SPD
def execute4(spd):
    for count, i in enumerate(spd):
        if (i < 48) or (i >=64):
            spd[count]=-9999
        if np.isnan(i):
            spd[count]=-9999
    return spd

# Wind >= 64 ---- FOR SPD
def execute5(spd):
    for count, i in enumerate(spd):
        if i < 64:
            spd[count]=-9999
        if np.isnan(i):
            spd[count]=-9999
    return spd

# Wind < 20 ---- FOR GST
def execute6(gst):
    for count, i in enumerate(gst):
        if i >= 20:
            gst[count]=-9999
        if np.isnan(i):
            gst[count]=-9999
    return gst

# 20 =< Wind < 34 ---- FOR GST
def execute7(gst):
    for count, i in enumerate(gst):
        if (i < 20) or (i >=34):
            gst[count]=-9999
        if np.isnan(i):
            gst[count]=-9999
    return gst

# 34 =< Wind < 48 ---- FOR GST
def execute8(gst):
    for count, i in enumerate(gst):
        if (i < 34) or (i >=48):
            gst[count]=-9999
        if np.isnan(i):
            gst[count]=-9999
    return gst

# 48 =< Wind < 64 ---- FOR GST
def execute9(gst):
    for count, i in enumerate(gst):
        if (i < 48) or (i >=64):
            gst[count]=-9999
        if np.isnan(i):
            gst[count]=-9999
    return gst

# Wind >= 64 ---- FOR GST
def execute10(gst):
    for count, i in enumerate(gst):
        if i < 64:
            gst[count]=-9999
        if np.isnan(i):
            gst[count]=-9999
    return gst

# Wind < 20 ---- FOR DIR
def execute11(spd, gst, dir):
    for count, i in enumerate(spd):
        if (i >= 35) or (gst[count] >= 35):
            dir[count]=-9999
        if np.isnan(dir[count]):
            dir[count]=-9999
    return dir

# 20 =< Wind < 34 ---- FOR DIR
def execute12(spd, gst, dir):
    for count, i in enumerate(spd):
        if ((i < 20) or (i >=34)) or ((gst[count] < 20) or (gst[count] >=34)):
            dir[count]=-9999
        if np.isnan(dir[count]):
            dir[count]=-9999
    return dir

# 34 =< Wind < 48 ---- FOR DIR
def execute13(spd, gst, dir):
    for count, i in enumerate(spd):
        if ((i < 34) or (i >=48)) or ((gst[count] < 34) or (gst[count] >=48)):
            dir[count]=-9999
        if np.isnan(dir[count]):
            dir[count]=-9999
    return dir

# 48 =< Wind < 64 ---- FOR DIR
def execute14(spd, gst, dir):
    for count, i in enumerate(spd):
        if ((i < 48) or (i >=64)) or ((gst[count] < 48) or (gst[count] >=64)):
            dir[count]=-9999
        if np.isnan(dir[count]):
            dir[count]=-9999
    return dir

# Wind >= 64 ---- FOR DIR
def execute15(spd, gst, dir):
    for count, i in enumerate(spd):
        if (i < 64) or (gst[count] < 64):
            dir[count]=-9999
        if np.isnan(dir[count]):
            dir[count]=-9999
    return dir