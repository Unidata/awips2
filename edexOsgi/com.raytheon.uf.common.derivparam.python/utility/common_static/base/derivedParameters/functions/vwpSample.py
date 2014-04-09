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
###

from numpy import isnan

def execute(wSp, WD, wW, rms):
    "Calculate Sampling String."
    sampleStrings = list()
    for i in range(len(wSp)):
        if (isnan(WD[i]) or WD[i] <= -8888):
            windDirStr = '***'
        else:
            windDirStr = "%3.3ddeg " % WD[i]
        if (isnan(wSp[i]) or wSp[i] <= -8888):
            windSpdStr = '***'
        else:
            windSpdStr = "%dkts " % (wSp[i] * 1.944)
        if (isnan(rms[i]) or rms[i] <= -8888):
            rmsStr = '***'
        else:
            rmsStr = "rms:%.0fkts " % rms[i]
        if (isnan(wW[i]) or wW[i] <= -8888):
            wCompStr = '***'
        else:
            wCompStr = "w:%.0fcm/s " % wW[i]
        sampleStrings.append(windDirStr + windSpdStr + rmsStr + wCompStr)
    return sampleStrings

        
    