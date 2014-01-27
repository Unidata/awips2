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
## Module that calculates surface weather elements from GLWM model
## output.
##
##--------------------------------------------------------------------------
class GLWMForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "GLWM", "GLWM")

##--------------------------------------------------------------------------
##  Calculates Significant wave height of combined wind waves and swells
##--------------------------------------------------------------------------
    def calcSigWaveHgt(self, htsgw_SFC):
        # Convert meters to feet
        grid = htsgw_SFC * 3.281

        # Return the new value
        return grid

##--------------------------------------------------------------------------
##  Calculates Significant wave height of wind waves
##--------------------------------------------------------------------------
    def calcWindWaveHgt(self, wvhgt_SFC):
        # Convert meters to feet
        grid = wvhgt_SFC * 3.281

        # Return the new value
        return grid

##--------------------------------------------------------------------------
##  Calculates Direction of wind waves.
##--------------------------------------------------------------------------
    def calcWindWaveDir(self, wvdir_SFC):
        # use constant speed for display purpose only
        mag = where(logical_and(less(wvdir_SFC, 360), greater(wvdir_SFC, 0)), 10.0, -9999.0)
        dir = clip(wvdir_SFC, 0, 360)
        return (mag, dir)

##--------------------------------------------------------------------------
##  Calculates Wind wave peak period
##--------------------------------------------------------------------------
    def calcWindWavePeriod(self, wvper_SFC):
        return clip(wvper_SFC, 0, 100)


def main():
    GLWMForecaster().run()