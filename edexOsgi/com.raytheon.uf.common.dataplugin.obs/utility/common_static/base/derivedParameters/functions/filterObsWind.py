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