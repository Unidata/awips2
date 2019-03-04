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
## Module that calculates surface weather elements from RFC QPF model output.
##
##--------------------------------------------------------------------------
class RFCQPFForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "RFCQPF", "RFCQPF")

##--------------------------------------------------------------------------
##  Calculates HPC QPF from the QPF model
##--------------------------------------------------------------------------
    def calcQPF(self, tpHPC_SFC):
        #  Assign the value filtering out everything above 1000 mm
        grid = tpHPC_SFC.copy()
        grid[greater(tpHPC_SFC, 1000)] = 0.0
        grid /= 25.4
        return grid.clip(0, 5, grid)  # clip at zero and 5 inches

def main():
    RFCQPFForecaster().run()

