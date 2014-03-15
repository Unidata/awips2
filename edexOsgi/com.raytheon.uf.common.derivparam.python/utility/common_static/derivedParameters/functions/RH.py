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

from numpy import exp

def execute1(T, DpT):
    "Calculate Relative Humidity from Temperature and Dewpoint Temp."
    RH = T-DpT
    RH *= 0.0091379024
    RH += 6106.396/T
    RH -= 6106.396/DpT
    RH = exp(RH)
    RH *= 100
    return RH

def execute2(P, T, SHx):
    "Calculate Relative Humidity from Pressure, Temp, and Spec Humidity."
    # Constants from the Fortran code.
    a = 22.05565
    b = 0.0091379024
    c = 6106.396
    epsilonx1k = 622.0
    
    shxDenom = SHx * 0.378
    shxDenom += epsilonx1k
    
    tDenom = -b*T
    tDenom += a
    tDenom -= c/T
    
    RH = P * SHx
    RH /= shxDenom
    RH /= exp(tDenom)

    return RH

