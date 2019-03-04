##
##

##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
##

#HPCGuide Smart Init
#based on local smart init provided by B. Rasch (BYZ)

from Init import *
class HPCGuideForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "HPCGuide",  "HPCGuide")

    def calcMaxT(self, mxt_FHAG2):
        MaxT = self.KtoF(mxt_FHAG2)
        return MaxT

    def calcMinT(self, mnt_FHAG2):
        MinT = self.KtoF(mnt_FHAG2)
        return MinT

    def calcTd(self, dpt_FHAG2):
        Td = self.KtoF(dpt_FHAG2)
        return Td

    def calcWind(self, wind_FHAG10):
        mag, dir = wind_FHAG10
        mag = self.convertMsecToKts(mag)   # convert to knots from m/s
        return (mag, dir)

    def calcPoP(self, pop_SFC):
        return clip(pop_SFC, 0.0, 100.0)  # clip

    def calcSky(self, tcc_EA):
        return clip(tcc_EA, 0.0, 100.0)  # clip

def main():
    HPCGuideForecaster().run()