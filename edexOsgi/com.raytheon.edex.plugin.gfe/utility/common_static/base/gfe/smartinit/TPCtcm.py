##
##

##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
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
