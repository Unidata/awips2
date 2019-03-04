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
class MSASForecaster(Forecaster):
     def __init__(self):
         Forecaster.__init__(self, "MSAS", "MSAS")

     def calcT(self, pot_SFC, alti_SFC, stopo):
         topoft = stopo * 3.2808
         altimb = alti_SFC / 100.0
         tsl = 288.0 * power((altimb / 1013.25), 0.190357143)
         tst = (6.5 * topoft) / 3280.8
         psfcmb = altimb * exp(log((tsl - tst) / tsl) * 5.253283302)
         tsfck = pot_SFC * pow((psfcmb / 1000.0), 0.287)
         return self.KtoF(tsfck)

     def calcTd(self, dpt_SFC):
         return self.KtoF(dpt_SFC)

     def calcWind(self, wind_SFC):
         mag = wind_SFC[0]
         dir = wind_SFC[1]
         mag = mag * 1.94
         dir = clip(dir, 0, 359.5)
         return (mag, dir)

def main():
     MSASForecaster().run()
