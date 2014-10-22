from Init import *
import os

##--------------------------------------------------------------------------
## Module that calculates surface elements from SWAN model output.
##--------------------------------------------------------------------------
class nwpsCG1Forecaster(Forecaster):
    def __init__(self):
       Forecaster.__init__(self, "nwpsCG1", "nwpsCG1")

##--------------------------------------------------------------------------
##  Calculates wave height from the SWAN model.  Converts to feet from
##  meters
##--------------------------------------------------------------------------

    def calcWaveHeight(self, htsgw_SFC):
        # Convert meters to feet
        grid = htsgw_SFC * 3.281
        # Return the new value
        return grid

#########################

    def calcSigWavHgt(self, htsgw_SFC, dirpw_SFC):
        # Convert meters to feet
        mag = htsgw_SFC * 3.281
        dir = clip(dirpw_SFC, 0, 359.5)
        # Return the new value
        return (mag,dir)

##--------------------------------------------------------------------------
##  Calculates SWAN Scalar Significant Swell Height. Converts to feet from meter
##--------------------------------------------------------------------------

    def calcSwanSwell(self, swell_SFC):
        # Convert meters to feet
        grid = where(greater(swell_SFC, 50), 0.0, swell_SFC * 3.28)
        # Return the new value
        return grid

##--------------------------------------------------------------------------
##  Calculates Primary Period from SWAN model.
##--------------------------------------------------------------------------

    def calcPeriod(self, perpw_SFC):

        period = clip(perpw_SFC, 0, 25)

        return period

##------------------------------------------------------------------------------
## Create the partitioned fields: Waves & Periods
##-----------------------------------------------------------------------------

    def calcWave1(self, SWELL_OSEQD1):
        # Convert meters to feet
        grid = SWELL_OSEQD1 * 3.281
        # Return the new value
        return grid

    def calcWave2(self, SWELL_OSEQD2):
        # Convert meters to feet
        grid = SWELL_OSEQD2 * 3.281
        # Return the new value
        return grid

    def calcWave3(self, SWELL_OSEQD3):
        # Convert meters to feet
        grid = SWELL_OSEQD3 * 3.281
        # Return the new value
        return grid

    def calcWave4(self, SWELL_OSEQD4):
        # Convert meters to feet
        grid = SWELL_OSEQD1 * 3.281
        # Return the new value
        return grid

    def calcPeriod1(self, SWPER_OSEQD1):
        # Convert meters to feet
        grid = SWPER_OSEQD1 * 3.281
        # Return the new value
        return grid

    def calcPeriod2(self, SWPER_OSEQD2):
        # Convert meters to feet
        grid = SWPER_OSEQD2 * 3.281
        # Return the new value
        return grid

    def calcPeriod3(self, SWPER_OSEQD3):
        # Convert meters to feet
        grid = SWPER_OSEQD3 * 3.281
        # Return the new value
        return grid

    def calcPeriod4(self, SWPER_OSEQD4):
        # Convert meters to feet
        grid = SWPER_OSEQD1 * 3.281
        # Return the new value
        return grid

################################################################################

os.system('/awips2/GFESuite/bin/sendGfeMessage -s -m "SWAN WAVE GRIDS ARE NOW IN GFE"')

def main():
    nwpsCG1Forecaster().run()

if __name__ == "__main__":
    main()
