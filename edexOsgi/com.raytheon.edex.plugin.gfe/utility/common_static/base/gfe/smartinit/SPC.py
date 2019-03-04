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
## Module that calculates surface weather elements from SPC model
## output.
##
##--------------------------------------------------------------------------
class SPCForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "SPC", "SPC")

##-------------------------------------------------------------------------
## Returns the SPC Tornado Probability grids
##--------------------------------------------------------------------------
    def calcPTOR(self, ptor_SFC):
        return ptor_SFC

    def calcPRSVR(self, prsvr_SFC):
        return prsvr_SFC

    def calcPRSIGSV(self, prsigsv_SFC):
        return prsigsv_SFC

def main():
    SPCForecaster().run()
