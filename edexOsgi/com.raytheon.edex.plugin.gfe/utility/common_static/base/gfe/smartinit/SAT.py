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

class SATForecaster(Forecaster):

##-------------------------------------------------------------------------
##  ifpInit module to calculate surface weather values from satellite data
##-------------------------------------------------------------------------
    def __init__(self):
        Forecaster.__init__(self, "Satellite", "SAT")

    def gvar2T(self, ir):
        t = where(ir < 177, (660 - ir) / 2.0, (418 - ir))
        t = t - 273
        return t

    def calcIR11E(self, ir11East_SFC):
        return self.gvar2T(ir11East_SFC)

    def calcIR13E(self, ir13East_SFC):
        return self.gvar2T(ir13East_SFC)

    def calcIR39E(self, ir39East_SFC):
        return self.gvar2T(ir39East_SFC)

    def calcWaterVaporE(self, waterVaporEast_SFC):
        return self.gvar2T(waterVaporEast_SFC)

    def calcFogE(self, IR11E, IR39E):
        return IR11E - IR39E

    def calcVisibleE(self, visibleEast_SFC):
        return visibleEast_SFC

    def calcIR11W(self, ir11West_SFC):
        return self.gvar2T(ir11West_SFC)

    def calcIR13W(self, ir13West_SFC):
        return self.gvar2T(ir13West_SFC)

    def calcIR39W(self, ir39West_SFC):
        return self.gvar2T(ir39West_SFC)

    def calcWaterVaporW(self, waterVaporWest_SFC):
        return self.gvar2T(waterVaporWest_SFC)

    def calcFogW(self, IR11W, IR39W):
        return IR11W - IR39W

    def calcVisibleW(self, visibleWest_SFC):
        return visibleWest_SFC

def main():
    SATForecaster().run()
