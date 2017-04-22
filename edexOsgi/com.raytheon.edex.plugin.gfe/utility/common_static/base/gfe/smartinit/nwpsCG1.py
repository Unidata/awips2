##
#
# nwpsCG1.py - Joe Maloney 2016-07-08
#
#     Init module for all nwpsCG1 domains.  Can also be used for nwpsCG2-5.
#

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
           os.system('/awips2/GFESuite/bin/sendGfeMessage -s -m "WCOSS ' + siteId + 'SWAN WAVE GRIDS ARE NOW IN GFE"')
       filemodtime = os.stat(chkfile).st_mtime
       twominutesago = time.time() - 120
       if (twominutesago - filemodtime) > 0:
           os.utime(chkfile, None)
           os.system('/awips2/GFESuite/bin/sendGfeMessage -s -m "WCOSS ' + siteId + 'SWAN WAVE GRIDS ARE NOW IN GFE"')


def main():
    nwpsCG1Forecaster().run()

