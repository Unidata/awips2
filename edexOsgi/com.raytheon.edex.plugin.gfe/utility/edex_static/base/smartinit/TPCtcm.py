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
## Module that calculates surface wind from TPC tcm model.
##
##--------------------------------------------------------------------------
class TPCtcmForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "TPCtcm", "TPCtcm")

##--------------------------------------------------------------------------
##  Converts the wind from m/s to knots, Wind
##--------------------------------------------------------------------------
    def calcWind(self, wind_SFC):
        mag = wind_SFC[0]  # get the wind grids
        dir = wind_SFC[1]  # get wind dir
        mag = mag * 1.94    # convert to knots
        dir = clip(dir, 0, 359.5)
        return (mag, dir)      # assemble speed and dir into a tuple

def main():
    TPCtcmForecaster().run()
