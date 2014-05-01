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

import gridslice
import meteolib
import numpy
from numpy import equal
from numpy import where
from numpy import zeros
from numpy import empty
from numpy import concatenate
from numpy import greater
from numpy import less
from unit import pascalToMilliBar
import gridslice

def execute1(pressure, sfcPress):
    sfcPress = sfcPress.reshape(- 1, 1);
    result = concatenate((sfcPress, pressure), 1)
    return pascalToMilliBar(result)

def execute2(levels, staElev):
    staElev = staElev.reshape(- 1, 1);
    hft2m = 30.48
    levelsInMeters = where(equal(levels, - 9999), - 9999, levels * 30.48) 
    GH = concatenate((staElev, levelsInMeters), 1)
    return meteolib.ztopsa(GH)

def execute3(numProfLvlsStation, MB):
    ret = zeros(numProfLvlsStation.shape, 'float32')
    ret.fill(MB)
    return ret

def execute4(prCloudStation,lowCldStation,midCldStation,hiCldStation):
    prCloudMB = prCloudStation/100
    CCPval = ccpExecute(lowCldStation,midCldStation,hiCldStation)
    isCeiling = where(greater(CCPval, 0.5), CCPval, -9999)
    prCloudMB[isCeiling == -9999] = -9999
    prCloudMB[prCloudStation == -9999] = -9999
    return prCloudMB;

def execute5(height, elevation):
    # array height is in meters
    # scalar elevation is in meters
    pressure = where(equal(height, - 9999), - 9999, height + elevation)
    pressure = meteolib.ztopsa(pressure)
    return pressure

def execute6(numLevelsStation, MB):
    ret = zeros(numLevelsStation.shape, 'float32')
    ret.fill(MB)
    return ret

def ccpExecute(lowCldStation,midCldStation,hiCldStation):
    P = maximum(lowCldStation,midCldStation,hiCldStation)
    return where(equal(P, -9999), -9999, P/100);
