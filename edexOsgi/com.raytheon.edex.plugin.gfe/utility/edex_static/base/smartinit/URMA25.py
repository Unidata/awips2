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
#
##--------------------------------------------------------------------------
## Smart Init to transform URMA25 D2D grids into corresponding GFE URMA25 grids
##
##--------------------------------------------------------------------------
class URMA25Forecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "URMA25", "URMA25")
##--------------------------------------------------------------------------
##  Wind - change m/s to kts
##-------------------------------------------
    def calcWind(self, wind_FHAG10):
        mag=wind_FHAG10[0]
        direc=wind_FHAG10[1]
        newmag=self.convertMsecToKts(mag)
        return (newmag,direc)
##--------------------------------------------------------------------------
##  QPE - change mm to inches and clip greater than 1000mm
##--------------------------------------------------------------------------
    def calcQPE(self, tp6hr_SFC):
        grid = tp6hr_SFC.copy()
        grid[greater(tp6hr_SFC, 1000)] = 0.0
        grid /= 25.4
        return grid.clip(0, 10, grid)  # clip at zero and 10 inches

    #==========================================================================
    #
    #  QPF6 - sums up all QPF grids within each 6 hour period
    #
##    def calcQPE06(self, QPE, QPE06, ctime):
##        modelhour = time.gmtime(ctime[0])
##        forecastHR = modelhour[3]
##        if forecastHR in [0, 6, 12, 18]:
##           QPE06 = 0
##        if QPE06 is None:
##           QPE06 = 0
##        QPE06 = QPE06 + QPE
##        return QPE06
##--------------------------------------------------------------------------
##  Sky
##--------------------------------------------------------------------------
    def calcSky(self, tcc_EA):
        grid = tcc_EA
        return clip(grid, 0, 100)
##--------------------------------------------------------------------------
##  T - change K to F
##--------------------------------------------------------------------------
    def calcT(self, t_FHAG2):
        return self.KtoF(t_FHAG2)
##--------------------------------------------------------------------------
##  Td - change K to F
##--------------------------------------------------------------------------
    def calcTd(self,dpt_FHAG2):
        return self.KtoF(dpt_FHAG2)
##--------------------------------------------------------------------------
##  MaxT and MinT - max and min of hourly Ts
##--------------------------------------------------------------------------
    def calcMaxT(self, T, MaxT):
        if MaxT is  None:
            return T
        return maximum(MaxT, T)

    def calcMinT(self, T, MinT):
        if MinT is  None:
            return T
        return minimum(MinT, T)
##--------------------------------------------------------------------------
##  Surface Pressure
##--------------------------------------------------------------------------
    def calcPressure(self, p_SFC):
        return p_SFC
##--------------------------------------------------------------------------
##  Pressure Analysis Uncertainty
##--------------------------------------------------------------------------
    def calcPressUnc(self, perranl_SFC):
        return perranl_SFC
##--------------------------------------------------------------------------
##  RH - calculated from T and Td, rather than from input specific humidity
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
##  MaxRH and MinRH - max and min of hourly RHs
##--------------------------------------------------------------------------
    def calcMaxRH(self, RH, MaxRH):
        if MaxRH is None:
            return RH
        return maximum(MaxRH, RH)

    def calcMinRH(self, RH, MinRH):
        if MinRH is None:
            return RH
        return minimum(MinRH, RH)
##--------------------------------------------------------------------------
##  T Analysis Uncertainty - change K to F
##--------------------------------------------------------------------------
    def calcTUnc(self, terranl_FHAG2):
        return terranl_FHAG2 * 1.8
##--------------------------------------------------------------------------
##  Td Analysis Uncertainty - change K to F
##--------------------------------------------------------------------------
    def calcTdUnc(self,dpterranl_FHAG2):
        return dpterranl_FHAG2 * 1.8
##--------------------------------------------------------------------------
##  Wind Analysis Uncertainty - change m/s to kts
##--------------------------------------------------------------------------
    def calcWSpdUnc(self, wserranl_FHAG10):
        mag = wserranl_FHAG10
        newmag=self.convertMsecToKts(mag)
        return newmag

    def calcWDirUnc(self, wderranl_FHAG10):
        return wderranl_FHAG10
    
    def calcWGustUnc(self, wgserranl_FHAG10):
        mag = wgserranl_FHAG10
        newmag=self.convertMsecToKts(mag)
        return newmag
##--------------------------------------------------------------------------
##  Sky Analysis Uncertainty
##--------------------------------------------------------------------------
    def calcSkyUnc(self, tccerranl_EA):
        grid = tccerranl_EA
        return clip(grid, 0, 100)
##--------------------------------------------------------------------------
##  Visibility
##--------------------------------------------------------------------------
    def calcVis(self, vis_SFC):
        return self.convertMtoSM(vis_SFC)

    def calcVisUnc(self, viserranl_SFC):
        return self.convertMtoSM(viserranl_SFC)
    ##--------------------------------------------------------------------------
    ##  Gust Wind - change m/s to kts
    ##--------------------------------------------------------------------------
    def calcWindGust(self, wgs_FHAG10):
        newmag=self.convertMsecToKts(wgs_FHAG10)
        return(newmag)
    
def main():
    URMA25Forecaster().run()
