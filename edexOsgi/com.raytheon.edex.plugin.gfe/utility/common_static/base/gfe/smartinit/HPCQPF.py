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
## Module that calculates surface weather elements from HPC QPF model output.
##
##--------------------------------------------------------------------------
class HPCQPFForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "HPCQPF", "HPCQPF")

##--------------------------------------------------------------------------
##  Calculates HPC QPF from the QPF model
##--------------------------------------------------------------------------
    def calcQPF(self, tpHPC_SFC):
        #  Assign the value filtering out everything above 1000 mm
        grid = where(greater(tpHPC_SFC, 1000), float32(0.0), tpHPC_SFC / 25.4)
        return clip(grid, 0, 5)  # clip at zero and 5 inches

def main():
    HPCQPFForecaster().run()

