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
from WW3 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from WW3 model
## output.
##
##--------------------------------------------------------------------------
class GlobalWaveForecaster(WW3Forecaster):
    def __init__(self):
        WW3Forecaster.__init__(self, "GlobalWave", "GlobalWave")

def main():
    GlobalWaveForecaster().run()