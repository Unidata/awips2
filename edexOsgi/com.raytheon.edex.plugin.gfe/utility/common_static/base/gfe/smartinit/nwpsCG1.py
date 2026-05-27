##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
##
#
# nwpsCG1.py - Joe Maloney 2016-07-08
#
#     Init module for all nwpsCG1 domains.  Can also be used for nwpsCG2-5.
#
# SOFTWARE HISTORY
#  Date         Ticket#    Engineer    Description
#  ------------ ---------- ----------- --------------------------
#  07/08/2016               jmaloney   Initial creation
#  12/06/2017   DCS20267    psantos    Add rip current guidance: rip current 
#                                      prob, dune erosion prob and overwash prob.
#  10/24/2019   DCS21768    psantos    NWPS v1.3: add six water level parameters
##

from Init import *
import os

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##
##--------------------------------------------------------------------------
class nwpsCG1Forecaster(Forecaster):
    def __init__(self, srcdb, destdb):
        Forecaster.__init__(self, srcdb, destdb)

##--------------------------------------------------------------------------
##  Calculates wave height from the nwpsCG1 model.  Converts to feet from meters
##--------------------------------------------------------------------------
    def calcWaveHeight(self, htsgw_SFC):
        # Convert meters to feet
        grid = htsgw_SFC * 3.281
        # Return the new value
        return grid

##--------------------------------------------------------------------------
##  Calculates sig wave height vector from the nwpsCG1 model.  Converts to feet from meters
##--------------------------------------------------------------------------
    def calcPeakWaveDir(self, dirpw_SFC):
        # use constant speed for display purpose only
        mag = where(logical_and(less(dirpw_SFC, 360), greater(dirpw_SFC, 0)), float32(10.0), float32(-9999.0))
        dir = clip(dirpw_SFC, 0, 360)
        return (mag,dir)

##--------------------------------------------------------------------------
##  Calculates Swan Swell from nwpsCG1 model.  Convert from meters to feet
##--------------------------------------------------------------------------
    def calcSwanSwell(self, swell_SFC):
        grid = swell_SFC * 3.281
        return grid

##--------------------------------------------------------------------------
##  Calculates Period from nwpsCG1 model.
##--------------------------------------------------------------------------
    def calcPeriod(self, perpw_SFC):
        period = clip(perpw_SFC, 0, 25)
        return period

##--------------------------------------------------------------------------
##  Calculates Rip Current Probability from nwpsCG1 model.
##--------------------------------------------------------------------------
    def calcRipProb(self, ripcop_SFC):
        grid = clip(ripcop_SFC, 0, 100)
        return grid

##--------------------------------------------------------------------------
##  Calculates Dune Erosion Probability from nwpsCG1 model.
##--------------------------------------------------------------------------
    def calcErosionProb(self, erosnp_SFC):
        grid = clip(erosnp_SFC, 0, 100)
        return grid

##--------------------------------------------------------------------------
##  Calculates Dune Overwash Probability from nwpsCG1 model.
##--------------------------------------------------------------------------
    def calcOverwashProb(self, owashp_SFC):
        grid = clip(owashp_SFC, 0, 100)
        return grid

##--------------------------------------------------------------------------
##  Calculates Total Water Level due to Tides, Wind, and Waves from nwpsCG1 model.  
##  Convert from meters to feet
##--------------------------------------------------------------------------
    def calcTwlWaves(self, twlwav_SFC):
        grid = twlwav_SFC * 3.281
        return grid
    
##--------------------------------------------------------------------------
##  Calculates Total Water Level Increase due to Waves from nwpsCG1 model.  
##  Convert from meters to feet
##--------------------------------------------------------------------------
    def calcRunUp(self, runup_SFC):
        grid = runup_SFC * 3.281
        return grid

##--------------------------------------------------------------------------
##  Calculates Mean Total Water Level Increase due to Waves from nwpsCG1 model.  
##  Convert from meters to feet
##--------------------------------------------------------------------------
    def calcSetUp(self, setup_SFC):
        grid = setup_SFC * 3.281
        return grid

##--------------------------------------------------------------------------
##  Calculates Time-Varying Total Water Level Increase due to Waves from nwpsCG1 model.  
##  Convert from meters to feet
##--------------------------------------------------------------------------
    def calcSwash(self, swash_SFC):
        grid = swash_SFC * 3.281
        return grid

##--------------------------------------------------------------------------
##  Calculates Total Water Level Above Dune Toe.  Convert from meters to feet
##--------------------------------------------------------------------------
    def calcTwlDT(self, twldt_SFC):
        grid = twldt_SFC * 3.281
        return grid

##--------------------------------------------------------------------------
##  Calculates Total Water Level Above Dune Crest.  Convert from meters to feet
##--------------------------------------------------------------------------
    def calcTwlDC(self, twldc_SFC):
        grid = twldc_SFC * 3.281
        return grid
    
##--------------------------------------------------------------------------
##  Notify user of receipt of nwpsCG1 data.
##--------------------------------------------------------------------------
    def notifyGFE(self, siteId):
       chkfile = "/tmp/nwps/CG1"
       chkfiledir = "/tmp/nwps"
       try:
           os.makedirs(chkfiledir)
       except OSError:
           pass
       if not os.path.isfile(chkfile):
           open(chkfile, 'a').close()
           os.system('/awips2/GFESuite/bin/sendGfeMessage -s -m "WCOSS ' + siteId + ' SWAN WAVE GRIDS ARE NOW IN GFE"')
       filemodtime = os.stat(chkfile).st_mtime
       twominutesago = time.time() - 120
       if (twominutesago - filemodtime) > 0:
           os.utime(chkfile, None)
           os.system('/awips2/GFESuite/bin/sendGfeMessage -s -m "WCOSS ' + siteId + ' SWAN WAVE GRIDS ARE NOW IN GFE"')



def main():
    nwpsCG1Forecaster().run()
