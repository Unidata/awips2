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
from Init import *

##--------------------------------------------------------------------------
## Smart Init to transform NAM DNG5 D2D grids into corresponding GFE grids
##--------------------------------------------------------------------------
class NamDNG5Forecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "NamDNG5", "NamDNG5")

##--------------------------------------------------------------------------
##  Wind - change m/s to kts
##--------------------------------------------------------------------------
    def calcWind(self, wind_SFC):
        mag=wind_SFC[0]
        direc=wind_SFC[1]
        newmag=self.convertMsecToKts(mag)
        return (newmag,direc)

    def calcWindGust(self, wgs_SFC):
        gust = self.convertMsecToKts(wgs_SFC)
        return gust

    def calcTransWind(self, ws_BLD0, wd_BLD0):
        mag = self.convertMsecToKts(ws_BLD0)
        return (mag, wd_BLD0)
##--------------------------------------------------------------------------
##  QPF - change mm to inches and clip greater than 1000mm
##--------------------------------------------------------------------------
    def calcQPF3(self, tp3hr_SFC):
        grid = where(greater(tp3hr_SFC, 1000), 0.0, tp3hr_SFC / 25.4)
        return clip(grid, 0, 5)  # clip at zero and 5 inches

    def calcQPF6(self, tp6hr_SFC):
        grid = where(greater(tp6hr_SFC, 1000), 0.0, tp6hr_SFC / 25.4)
        return clip(grid, 0, 10)  # clip at zero and 10 inches

    def calcQPF12(self, tp12hr_SFC):
        grid = where(greater(tp12hr_SFC, 1000), 0.0, tp12hr_SFC / 25.4)
        return clip(grid, 0, 15)  # clip at zero and 15 inches

##--------------------------------------------------------------------------
##  Snow fall - convert from meters to inches
##--------------------------------------------------------------------------
    def calcSnowAmt(self, snowd3hr_SFC):
        return snowd3hr_SFC * 39.37

    def calcSnowAmt6(self, snowd6hr_SFC):
        return snowd6hr_SFC * 39.37

##--------------------------------------------------------------------------
##  PoP
##--------------------------------------------------------------------------
    def calcPoP(self, prcp3hr_SFC):
        return clip(prcp3hr_SFC, 0, 100)

    def calcPoP6(self, prcp6hr_SFC):
        return clip(prcp6hr_SFC, 0, 100)

    def calcPoP12(self, prcp12hr_SFC):
        return clip(prcp12hr_SFC, 0, 100)

##--------------------------------------------------------------------------
##  Sky
##--------------------------------------------------------------------------
    def calcSky(self, tcc_SFC):
        grid = tcc_SFC
        return clip(grid, 0, 100)
##-------------------------------------------------------------------------
##  Visibility - convert from M to SM
##-------------------------------------------------------------------------
    def calcVis(self, vis_SFC):
        return vis_SFC * 0.00062137

##--------------------------------------------------------------------------
##  T - change K to F
##--------------------------------------------------------------------------
    def calcT(self, t_SFC):
	return self.KtoF(t_SFC)
##--------------------------------------------------------------------------
##  Td - change K to F
##--------------------------------------------------------------------------
    def calcTd(self,dpt_SFC):
	return self.KtoF(dpt_SFC)
##--------------------------------------------------------------------------
##  3hr MaxT and MinT (convert from K to F) - needed for MaxT and MinT
##--------------------------------------------------------------------------
    def calcMaxT3(self, mxt3hr_SFC):
        return self.KtoF(mxt3hr_SFC)

    def calcMinT3(self, mnt3hr_SFC):
        return self.KtoF(mnt3hr_SFC)
##--------------------------------------------------------------------------
##  MaxT and MinT - compute from 3hr max and min T
##--------------------------------------------------------------------------
    def calcMaxT(self, MaxT3, MaxT):
        if MaxT is None:
            return MaxT3
        return maximum(MaxT, MaxT3)

    def calcMinT(self, MinT3, MinT):
        if MinT is None:
            return MinT3
        return minimum(MinT, MinT3)
##--------------------------------------------------------------------------
##  RH - calculated from T and Td
##--------------------------------------------------------------------------
    def calcRH(self, T, Td):
        Vt=self.vaprtf(T)
        Vd=self.vaprtf(Td)
        return ( (Vd / Vt) * 100.0)

    def vaprtf(self,tf):
        tc=(tf-32.0)*5.0/9.0
        vapr=6.112*exp((17.67*tc)/(tc+243.5))
        return vapr

##--------------------------------------------------------------------------
##  MaxRH - maximum of 3hr max RHs
##  MinRH - minimum of hourly RHs 
##--------------------------------------------------------------------------
    def calcMaxRH3(self, maxRH3hr_SFC):
        return maxRH3hr_SFC

    def calcMaxRH(self, MaxRH3, MaxRH):
        if MaxRH is None:
            return MaxRH3
        return maximum(MaxRH, MaxRH3)

    def calcMinRH(self, RH, MinRH):
        if MinRH is None:
            return RH
        return minimum(MinRH, RH)

##-------------------------------------------------------------------------
## Mixing Height - convert from M to Ft
##-------------------------------------------------------------------------
    def calcMixHgt(self, geh_BLD0):
        return geh_BLD0 * 3.28084

##-------------------------------------------------------------------------
## Snow level - convert from M to Ft
##-------------------------------------------------------------------------
    def calcSnowLevel(self, gh_WBZ0):
        return gh_WBZ0 * 3.28084


def main():
    NamDNG5Forecaster().run()
