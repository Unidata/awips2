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

## @file ThetaE.py
#
# Calculate equivalent potential temperature (Theta E), EPT. 
# 
# ----------------------------------------------------------------
import gridslice
from numpy import clip
from numpy import exp
from numpy import log
from numpy import sqrt
from numpy import power
from numpy import zeros
from numpy import where

##
# Calculate equivalent potential temperature (Theta E) from 
# pressure(mb), Temperature(degrees K) and Relative Humdity(0 to 100).
# This function accepts numpy arrays of the appropriate values.
#
# @param P: Pressure in millibars
# @param T: Temperature in degrees K
# @param RH: Relative humidity from 0 to 100
# @return: Equivalent potential temperature in degrees K
# @rtype: numpy array or Python float
#
def execute(P,T,RH):
    "Calculate equivalent potential temperature (Theta E) from \
    presssure(mb), Temperature(degrees K) and RH(0 to 100)."
    # symbolic constants
    epsilon = 0.622
    L_cp = 2540 
    
    # 0 to 100 is valid for RH, but we can't take log(0), so fudge value slightly
    rhqc = clip(RH,0.01,100.0)
    # do the math
    powval = -0.0091379024 * T
    powval += 22.05565
    powval -= 6106.396/T
    eee = rhqc * exp(powval)
    b = 26.66082 - log(eee)
     
    val = b*b
    val -= 223.1986
    val = sqrt(val)
    tdp = (b - val)
    tdp /= 0.0182758048

    val = tdp * 0.0015945203
    val += 41.178204/T
    val -= 0.37329638
    val *= T-tdp
    tc = tdp - val
              
    w = eee/(P - eee)
    w *= epsilon

    powval = w * L_cp
    powval /= tc
       
    EPT = T * exp(powval)
    EPT *= power(1000/P, 0.286)
    return EPT
