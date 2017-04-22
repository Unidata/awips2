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
## Module that calculates surface weather elements from GLERL model
## output.
##
##--------------------------------------------------------------------------
class GLERLForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "GLERL", "GLERL")

##--------------------------------------------------------------------------
##  Calculates wave height from the GLERL model.  Converts to feet from meters
##--------------------------------------------------------------------------
    def calcWaveHeight(self, wvhgt_SFC):
        #  Convert meters to feet
        grid = wvhgt_SFC * 3.28
        grid[greater_equal(grid, 50.0)] = 0.0
        return grid


##--------------------------------------------------------------------------
##  Calculates Primary Swell from GLERL model.
# Note: have to use wave height for magnitude since that info not available.
##--------------------------------------------------------------------------
    def calcSwell(self, WaveHeight, wvdir_SFC):
        # extract the wind speed and direction
        mag = WaveHeight
        dir = clip(wvdir_SFC, 0, 359.5)
        return (mag, dir)

##--------------------------------------------------------------------------
##  Calculates Primary Period from GLERL model.
##--------------------------------------------------------------------------
    def calcPeriod(self, wvper_SFC):
        return clip(wvper_SFC, 0, 60)

def main():
    GLERLForecaster().run()
