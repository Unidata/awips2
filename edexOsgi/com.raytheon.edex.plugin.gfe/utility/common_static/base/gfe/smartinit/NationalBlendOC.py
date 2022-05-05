##
#This software was developed and / or modified by NOAA/NWS/OCP/ASDT##

##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
##

# ----------------------------------------------------------------------------
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    02/23/2018      #20395        wkwock         Added NBM3.1 elements.
#    03/10/2020      DCS20781      wkwock         Add NBM 3.2 elements

from Init import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from SPC model
## output.
##
##--------------------------------------------------------------------------
class NationalBlendOCForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "NationalBlendOC", "NationalBlend")

    def calcWGS50pct(self, wgs50pct_FHAG10):
        return wgs50pct_FHAG10

    def calcWS50Prcntl10m(self, ws50pct_FHAG10):
        return ws50pct_FHAG10

    def calcWS50Prcntl30m(self, ws50pct_FHAG30):
        return ws50pct_FHAG30

    def calcWS50Prcntl80m(self, ws50pct_FHAG80):
        return ws50pct_FHAG80

    def calcVis50pct(self, vis50pct_SFC):
        return self.convertMtoSM(vis50pct_SFC)

    def calcT50pct(self, t50pct_FHAG2):
        return self.convertKtoF(t50pct_FHAG2)

    def calcPMSL10pct(self, pmsl10pct_MSL):
        return pmsl10pct_MSL / 100.0

    def calcPMSL50pct(self, pmsl50pct_MSL):
        return pmsl50pct_MSL / 100.0

    def calcPMSL90pct(self, pmsl90pct_MSL):
        return pmsl90pct_MSL / 100.0

    def windChillCalc(self, T, Wind):
        mag = Wind[0] * 1.15077945
        WindChill = where(less_equal(mag, 1), T, 35.74 + (0.6215 * T) - (35.75 * (mag ** 0.16)) + (0.4275 * T * (mag ** 0.16)))

        # clip values where WindChill > T
        WindChill = where(greater(WindChill, T), T, WindChill)

        # substitute the temperature if WindChill >= 51 degrees
        WindChill = where(greater_equal(T, 51), T, WindChill)
        return WindChill

    def calcWs90pct (selfself, ws90pct_FHAG10):
        return ws90pct_FHAG10


def main():
    NationalBlendOCForecaster().run()

if __name__ == "__main__":
    main()