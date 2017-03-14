##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
#
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
#
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
#
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

# LAPS Smart Init module

from Init import *

class LAPSForecaster(Forecaster):

##-------------------------------------------------------------------------
##  ifpInit module to calculate surface weather values from LAPS analysis
##-------------------------------------------------------------------------
    def __init__(self):
        Forecaster.__init__(self, "LAPS", "LAPS")

##-------------------------------------------------------------------------
##  Converts LAPS surface temp from K to F
##-------------------------------------------------------------------------
    def calcT(self, t_SFC):
        return self.KtoF(t_SFC)

##-------------------------------------------------------------------------
##  Converts LAPS surface dew point temp from K to F
##-------------------------------------------------------------------------
    def calcTd(self, dpt_SFC):
        return self.KtoF(dpt_SFC)

##-------------------------------------------------------------------------
## Convert LAPS surface wind speed from m/x to knots
##-------------------------------------------------------------------------
    def calcWind(self, wind_SFC):
        mag = wind_SFC[0]
        dir = wind_SFC[1]
        mag = mag * 1.94   # convert m/s to knots
        dir = clip(dir, 0, 359.5)
        return (mag, dir)

##-------------------------------------------------------------------------
## Uses LAPS skyCover grid to make IFP sky grids
##-------------------------------------------------------------------------
    def calcSky(self, ccov_SFC):
        return clip(ccov_SFC, 0, 100)

##-------------------------------------------------------------------------
## Uses LAPS one-hour snowAmt grids to initialize SnowAmt
##-------------------------------------------------------------------------
    def calcSnowAmt(self, s1hr_SFC):
        return s1hr_SFC * 3.28 * 12

##-------------------------------------------------------------------------
## Uses LAPS precip grids to initialize QPF
##-------------------------------------------------------------------------
    def calcQPF(self, pc_SFC):
        return pc_SFC * 3.28 * 12

##-------------------------------------------------------------------------
## Return the LAPS low level radar analysis
##-------------------------------------------------------------------------
    def calcRadar(self, llr_SFC):
        return llr_SFC

##-------------------------------------------------------------------------
## Uses LAPS weather grids to initialize weather
##-------------------------------------------------------------------------
    def calcWx(self, spt_SFC):
        wx = spt_SFC.astype(int8)
        outOfAreaMask = logical_or(greater(wx, 7),less(wx, 0))
        wx[outOfAreaMask] = 0
        return (wx, self._getWxKey())
        
##-------------------------------------------------------------------------
## Internal function that defines the weather key
##-------------------------------------------------------------------------
    def _getWxKey(self):
        key = ['<NoCov>:<NoWx>:<NoInten>:<NoVis>:',
               "Wide:R:-:<NoVis>:", "Wide:S:-:<NoVis>:",
               "Wide:ZR:-:<NoVis>:", "Wide:IP:-:<NoVis>:",
               "Def:H:<NoInten>:<NoVis>:", "Wide:L:-:<NoVis>:",
               "Wide:ZL:-:<NoVis>:"]
        return key


def main():
    LAPSForecaster().run()
