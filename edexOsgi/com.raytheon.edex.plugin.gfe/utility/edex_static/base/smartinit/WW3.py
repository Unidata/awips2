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

from Init import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from WW3 model
## output.
##
##--------------------------------------------------------------------------
class WW3Forecaster(Forecaster):
    def __init__(self, srcdb, destdb):
        Forecaster.__init__(self, srcdb, destdb)

##--------------------------------------------------------------------------
##  Calculates wave height from the WW3 model.  Converts to feet from meters
##--------------------------------------------------------------------------
    def calcWaveHeight(self, htsgw_SFC):
        # Convert meters to feet
        grid = htsgw_SFC * 3.281
        # Return the new value
        return grid

##--------------------------------------------------------------------------
##  Calculates wind wave height from the WW3 model.  Converts to feet from meters
##--------------------------------------------------------------------------
    def calcWindWaveHgt(self, wvhgt_SFC):
        # Convert meters to feet
        grid = wvhgt_SFC * 3.281
        # Return the new value
        return grid

##--------------------------------------------------------------------------
##  Calculates wind from the WW3 model.  Convert from meters/sec to knots.
##--------------------------------------------------------------------------
    def calcWind(self, wind_SFC):
        # extract the wind speed and direction
        mag = wind_SFC[0].copy()
        mag[greater(wind_SFC[0], 100)] = 0
        mag *= 1.94 # convert
        dir = wind_SFC[1].copy()
        dir[greater(wind_SFC[0], 100)] = 0
        dir.clip(0, 359.5, dir)
        return (mag, dir)

##--------------------------------------------------------------------------
##  Calculates Primary Swell from WW3 model.
# Note: have to use wave height for magnitude since that info not available.
##--------------------------------------------------------------------------
    def calcSwell(self, swell_OSEQD1, swdir_OSEQD1):
        # extract the wind speed and direction
        mag = swell_OSEQD1
        dir = clip(swdir_OSEQD1, 0, 359.5)
        return (mag, dir)

##--------------------------------------------------------------------------
##  Calculates Secondary Swell from WW3 model.
# Note: have to use wave height for magnitude since that info not available.
##--------------------------------------------------------------------------
    def calcSwell2(self, swell_OSEQD2, swdir_OSEQD2):
        # extract the wind speed and direction
        mag = swell_OSEQD2
        dir = clip(swdir_OSEQD2, 0, 359.5)
        return (mag, dir)

##--------------------------------------------------------------------------
##  Calculates Primary Period from WW3 model.
##--------------------------------------------------------------------------
    def calcPeriod(self, swper_OSEQD1):
        return clip(swper_OSEQD1, 0, 60)

##--------------------------------------------------------------------------
##  Calculates Secondary Period from WW3 model.
##--------------------------------------------------------------------------
    def calcPeriod2(self, swper_OSEQD2):
        return clip(swper_OSEQD2, 0, 60)

def main():
    WW3Forecaster().run()
