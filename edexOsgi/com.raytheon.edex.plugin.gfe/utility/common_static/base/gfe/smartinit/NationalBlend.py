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

from numpy import *

from Init import *
import numpy as np

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from SPC model
## output.
##
##--------------------------------------------------------------------------
class NationalBlendForecaster(Forecaster):

    def __init__(self):
        Forecaster.__init__(self, "NationalBlend", "NationalBlend")

    #===========================================================================
    #  Apparent Temperature
    #===========================================================================
    def calcApparentT(self, T, RH, Wind):
        windMag = Wind[0] * 1.15
        hiMask = T > 79
        wcMask = (windMag > 3) & (T <= 50)
        appT = T.copy()
        if any(hiMask):
            # Only calculate Heat Index for T >= 80F
            appT[hiMask] = (-42.379 + (2.04901523 * T) + (10.14333127 * RH) + \
                             (-0.22475541 * T * RH) + (-0.00683783 * T*T) + \
                             (-0.05481717 * RH*RH) + (0.00122874 * T*T * RH) + \
                             (0.00085282 * T * RH*RH) + (-0.00000199 * T*T * RH*RH))[hiMask]
            # Make low RH adjustment
            downMask = ((RH < 13.0) & ((T >= 80) & (T <= 112)))
            appT[downMask] = (appT - (((13.0 - RH) / 4.0) * \
                                      sqrt((17.0 - abs(T - 95.0)) / 17)))[downMask]
            
            # Make High RH adjustment
            upMask = ((RH > 85.0) & ((T >= 80.0) & (T <= 87.0)))
            appT[upMask] = (appT + (((RH - 85.0) / 10.0) * ((87 - T) / 5.0)))[upMask]
        if any(wcMask):
            # Only calculate Wind Chill for T <= 50F AND wind speed > 3 MPH
            appT[wcMask] = (35.74 + (0.6215 * T) - (35.75 * power(windMag, 0.16)) + \
                            (0.4275 * T * power(windMag, 0.16)))[wcMask]
            mask = appT > T
            appT[mask] = T[mask]
        return appT

    def calcCeiling(self, cc_CLG):

        Ceiling =  (cc_CLG) * 3.28083989501 ###  convert from meters to feet
        #Ceiling =  (cc_CLG) * 0.0328083989501 ###  convert from meters to hunderds of feet
        #  D2D displays -3.28m in clear areas outside the fog areas, so we can take advantage
        #  this infornmation to distinguish clear areas from the dense fog accociated with 0
        #  values.
        Ceiling = where(less(Ceiling, 0), -30000, Ceiling)

        return Ceiling

    def smoothpm(self,grid,k,mask=None,onlyMaskedData=1):
       """smoothpm - smooths grid by averaging over plus and minus k gridpoints, which
       means an average over a square 2k+1 gridpoints on a side.

       If mask is specified (an integer grid of 1s and 0s), only modify points that
       have mask=1, not any other points.

       If a mask is specified, the default is for only the points inside the mask to
       influence the smoothed points.  this keeps data from outside the mask
       "bleeding" into the area being smoothed.  If, however, you want the data
       outside the mask to impact the smoothed data, set onlyMaskedData=0 (it defaults
       to 1). Setting onlyMaskedData to -1 is the same as 1, but it returns the data 
       without clipping it to the mask. This is used for fillMissingClimo which takes 
       advantage of the small expanded amount of data that is created along the edges 
       of the masked data.

       Near the edges of the grid, the average is over fewer points than in the center
       of the grid - because some of the points in the averaging window would be off
       the grid. It just averages over the points that it can.  For example, on the
       edge gridpoint - it can only come inside k points - so the average is over only
       k+1 points in that direction (though over all 2k+1 points in the other
       direction - if possible)

       This is much faster than shifting the grid multiple times and adding them up.
       Instead it uses the cumsum function in numpy - which gives you cumulative sum
       across a row/column.  Total across the 2k+1 points is the cumsum at the last
       point minus the cumsum at the point before the first point. Only edge points
       need special handling - and the cumsum is useful there too.
       """

       k=int(k) # has to be integer number of gridpoints
       if (k<1): # has to be a positive number of gridpoints
          return grid
       if len(grid.shape)!=2: # has to be a 2-d grid
          return grid
       (ny,nx)=grid.shape
       k2=k*2
       #
       #  Remove the minimum and divide by the range from the grid so
       #  that when cumsum accumulates the sum over a full row or
       #  column that it doesn't get so big that precision is lost
       #  might be lost.  This makes the 'gridmin' grid have all points
       #  ranging from 0 to 1.
       #
       fullmin=minimum.reduce(minimum.reduce(grid))
       fullmax=maximum.reduce(maximum.reduce(grid))
       fullrange=fullmax-fullmin
       if fullrange<0.001:
           fullrange=0.001
       gridmin=(grid-fullmin)/fullrange
       #
       #  When there is no mask the code is much simpler
       #
       if ((mask is None) or (onlyMaskedData==0)):
          #
          #  Average over the first (y) dimension - making the 'mid' grid
          #
          mid=grid*0.0
          c=cumsum(gridmin,0)
          nym1=ny-1
          midy=int((ny-1.0)/2.0)
          ymax=min(k+1,midy+1)
          #
          #  Handle the edges
          #
          for j in range(ymax):
             jk=min(j+k,nym1)
             jk2=max(nym1-j-k-1,-1)
             mid[j,:]=c[jk,:]/float(jk+1)
             if jk2==-1:
                mid[nym1-j,:]=c[nym1,:]/float(jk+1)
             else:
                mid[nym1-j,:]=(c[nym1,:]-c[jk2,:])/float(jk+1)
          #
          #  The really fast part for the middle of the grid
          #
          if ((k+1)<=(ny-k)):
             mid[k+1:ny-k,:]=(c[k2+1:,:]-c[:-k2-1,:])/float(k2+1)
          #
          #  Average over the second (x) dimension - making the 'out' grid
          #
          c=cumsum(mid,1)
          out=grid*0.0
          nxm1=nx-1
          midx=int((nx-1.0)/2.0)
          xmax=min(k+1,midx+1)
          #
          #  Handle the edges
          #
          for j in range(xmax):
             jk=min(j+k,nxm1)
             jk2=max(nxm1-j-k-1,-1)
             out[:,j]=c[:,jk]/float(jk+1)
             if jk2==-1:
                out[:,nxm1-j]=c[:,nxm1]/float(jk+1)
             else:
                out[:,nxm1-j]=(c[:,nxm1]-c[:,jk2])/float(jk+1)
          #
          #  The really fast part for the middle of the grid
          #
          if ((k+1)<=(nx-k)):
             out[:,k+1:nx-k]=(c[:,k2+1:]-c[:,:-k2-1])/float(k2+1)
          #
          #  Multiply by the range and add the minimum back in
          #
          out=(out*fullrange)+fullmin
          if ((onlyMaskedData==0)and(mask is not None)):
              out[mask<1]=grid[mask<1]
       #
       #  When there is a Mask specified, it makes the code a bit more
       #  difficult. We have to find out how many points were in each
       #  cumsum value - and we have to deal with possible divide-by-zero
       #  errors for points where no masked points were in the average
       #
       else:
          #
          #  Sum over the first (y) dimension - making the 'mid' grid
          #
          mask=clip(mask,0,1)
          gridmin1=where(mask,gridmin,0)
          mid=grid*0.0
          midd=grid*0.0
          c=cumsum(gridmin1,0)
          d=cumsum(mask,0)
          nym1=ny-1
          midy=int((ny-1.0)/2.0)
          ymax=min(k+1,midy+1)
          #
          #  Handle the edges
          #
          for j in range(ymax):
             jk=min(j+k,nym1)
             jk2=max(nym1-j-k-1,-1)
             mid[j,:]=c[jk,:]
             midd[j,:]=d[jk,:]
             if jk2==-1:
                mid[nym1-j,:]=c[nym1,:]
                midd[nym1-j,:]=d[nym1]
             else:
                mid[nym1-j,:]=(c[nym1,:]-c[jk2,:])
                midd[nym1-j,:]=d[nym1,:]-d[jk2,:]
          #
          #  The really fast part for the middle of the grid
          #
          if ((k+1)<=(ny-k)):
             mid[k+1:ny-k,:]=(c[k2+1:,:]-c[:-k2-1,:])
             midd[k+1:ny-k,:]=d[k2+1:,:]-d[:-k2-1,:]
          #
          #  Sum over the second (x) dimension - and divide by
          #  the number of points (but make sure number of points
          #  is at least 1) - making the 'out' grid
          #
          c=cumsum(mid,1)
          d=cumsum(midd,1)
          out=grid*0.0
          nxm1=nx-1
          midx=int((nx-1.0)/2.0)
          xmax=min(k+1,midx+1)
          #
          #  Handle the edges
          #
          for j in range(xmax):
             jk=min(j+k,nxm1)
             jk2=max(nxm1-j-k-1,-1)
             out[:,j]=c[:,jk]/maximum(d[:,jk],1)
             if jk2==-1:
                out[:,nxm1-j]=c[:,nxm1]/maximum(d[:,nxm1],1)
             else:
                out[:,nxm1-j]=(c[:,nxm1]-c[:,jk2])/maximum((d[:,nxm1]-d[:,jk2]),1)
          #
          #  The really fast part for the middle of the grid
          #
          if ((k+1)<=(nx-k)):
             out[:,k+1:nx-k]=(c[:,k2+1:]-c[:,:-k2-1])/maximum((d[:,k2+1:]-d[:,:-k2-1]),1)
          #
          #  Multiply by the range and add the minimum back in
          #
          out=(out*fullrange)+fullmin
          if onlyMaskedData == -1:
              return out
          out[mask<1]=grid[mask<1]
       return out

##-------------------------------------------------------------------------
##  Converts cc from meters to hundreds of feet and put in CloudBasePrimary
##-------------------------------------------------------------------------
    def calcCloudBasePrimary(self, cc_CBL):

        CloudBasePrimary =  (cc_CBL) * 0.0328083989501 ###  convert from meters to hundreds of feet
        #  D2D displays -3.28m in clear areas outside the fog areas, so we can take advantage
        #  this information to distinguish clear areas from the dense fog accociated with 0
        #  values.
        CloudBasePrimary = where(less(CloudBasePrimary, 0), 250, CloudBasePrimary)

        return CloudBasePrimary

    def calcMaxRH(self, MAXRH12hr_FHAG2):
        return MAXRH12hr_FHAG2

    def calcMaxT(self, mxt_FHAG2):
        return  self.KtoF(mxt_FHAG2)

    #===========================================================================
    # Calc MaxTwAloft - Convert K to C
    #===========================================================================
    def calcMaxTwAloft(self, maxtw_FHAG61040000):
        return (maxtw_FHAG61040000 - 273.15)

    def calcMinRH(self, MINRH12hr_FHAG2):
        return MINRH12hr_FHAG2

    def calcMinT(self, mnt_FHAG2):
        return  self.KtoF(mnt_FHAG2)

    #===========================================================================
    # NegativeEnergyLowLevel
    #===========================================================================
    def calcNegativeEnergyLowLevel(self, nbe_SFC):
        return nbe_SFC

    #===========================================================================
    # Positive Energy Aloft
    #===========================================================================
    def calcPositiveEnergyAloft(self, pbe_FHAG61040000):
        return pbe_FHAG61040000

    def calcPoP(self, pop_SFC):
        return pop_SFC

    def calcPoP01(self, pop1hr_SFC):
        return pop1hr_SFC

    def calcPoP6(self, pop6hr_SFC):
        return pop6hr_SFC

    #==========================================================================
    #
    #  QPF - just convert input QPF from millimeters to inches
    #
    def calcQPF(self, tp6hr_SFC):
        # Remove really bad data.
        tp6hr_SFC = np.where(tp6hr_SFC >= 1000.0, 0.0, tp6hr_SFC).astype(np.float32)
        #  Convert from millimeters to inches
        return tp6hr_SFC / 25.4

    ##--------------------------------------------------------------------------
    ##  QPF - change mm to inches
    ##--------------------------------------------------------------------------
    def calcQPF1(self, tp1hr_SFC):
        # Remove really bad data.
        tp1hr_SFC = np.where(tp1hr_SFC >= 1000.0, 0.0, tp1hr_SFC).astype(np.float32)
        #  Convert from millimeters to inches
        return tp1hr_SFC / 25.4

    #===========================================================================
    #  RH - calculate RH using the equation provided by the NBM Team
    #===========================================================================
    def calcRH(self, T, Td):
        Tc = 0.556 * (T - 32.0)
        Tdc = 0.556 * (Td - 32.0)
        satVaporPress = 6.1078 * np.exp((Tc * 17.269) / (Tc + 237.3))
        vaporPress = 6.1078 * np.exp((Tdc * 17.269) / (Tdc + 237.3))
        return np.clip((vaporPress / satVaporPress) * 100, 2, 100)

    #===========================================================================
    # Sky
    #===========================================================================
    def calcSky(self, tcc_SFC):
        return clip(tcc_SFC, 0, 100)

    #===========================================================================
    # Snow Level - convert from M to Ft
    #===========================================================================
    def calcSnowLevel(self, snowlvl_FH0):
        return snowlvl_FH0 * 3.28084

    #===========================================================================
    # Snow Ratio
    #===========================================================================
    def calcSnowRatio(self, snowratio_SFC):
        return snowratio_SFC

    #===========================================================================
    #  Temperatures
    #===========================================================================
    def calcT(self, t_FHAG2):
        return  self.KtoF(t_FHAG2)

    ##--------------------------------------------------------------------------
    ##  Td - change K to F
    ##--------------------------------------------------------------------------
    def calcTd(self, dpt_FHAG2):
        return self.KtoF(dpt_FHAG2)

    def calcTstmPrb3(self, thp3hr_SFC):
        return thp3hr_SFC

    def calcTstmPrb6(self, thp6hr_SFC):
        return thp6hr_SFC

    #===========================================================================
    #  Visibility - convert from meters to statute miles
    #===========================================================================
    def calcVisibility(self, vis_SFC):
        return vis_SFC * 0.00062137

    #===========================================================================
    #  Calculate WPC provided wind
    #===========================================================================
    def calcWind(self, wind_FHAG10):
        mag, dir = wind_FHAG10
        mag = self.convertMsecToKts(mag)    # convert to knots from m/s
        return (mag, dir)

    ##--------------------------------------------------------------------------
    ##  Gust Wind - change m/s to kts
    ##--------------------------------------------------------------------------
    def calcWindGust(self, wgs_FHAG10):
        newmag=self.convertMsecToKts(wgs_FHAG10)
        return(newmag)
    ##--------------------------------------------------------------------------
    ##  
    ##--------------------------------------------------------------------------
    def calcProbRefreezeSleet(self, ptyperefip_SFC):
        return(ptyperefip_SFC)
    def calcProbIcePresent(self, prbcldice_EA):
        return(prbcldice_EA)
    def calcPotSleet(self, ptypicepellets_SFC):
        return(ptypicepellets_SFC)
    def calcPotRain(self, ptyprain_SFC):
        return(ptyprain_SFC)
    def calcPotSnow(self, ptypsnow_SFC):
        return(ptypsnow_SFC)
    def calcPotFreezingRain(self, ptypfreezingrain_SFC):
        return(ptypfreezingrain_SFC)

    def calcTransWind(self, trwspd_EA, trwdir_EA):
        newmag=self.convertMsecToKts(trwspd_EA)
        return (newmag, trwdir_EA)

    def calcTropWind(self, tpwspd_FHAG10, tpwdir_FHAG10):
        newmag=self.convertMsecToKts(tpwspd_FHAG10)
        return (newmag, tpwdir_FHAG10)

    def calcLLWS(self, wind_FHAG0610):
        mag, dir = wind_FHAG0610
        Mask = mag >= 150
        mag = self.convertMsecToKts(mag)    # convert to knots from m/s
        mag[Mask] = 0
        dir[Mask] = 0
        return (mag, dir)

    def calcSnowAmt(self, totsn6hr_SFC):
        return (totsn6hr_SFC * 1000) / 25.4

    def calcMixHgt(self, mixht_EA):
        return mixht_EA / 0.3048

    def calcVentRate(self, vrate_EA):
        return vrate_EA

    def calcHaines(self, hindex6hr_EA):
        return hindex6hr_EA

    def calcFosBerg(self, fosindx6hr_SFC):
        return fosindx6hr_SFC

    def calcLLWSHgt(self, llwshgt_FHAG0610):
        return llwshgt_FHAG0610 * 32.808398951

    def calcRadar(self, maxref1hr_FHAG1000):
        return maxref1hr_FHAG1000

    def calcSigWaveHgt(self, htsgw_SFC):
        return htsgw_SFC / 0.3048

    def calcWx(self, pwther_SFC):
        #https://graphical.weather.gov/definitions/defineWx.html
        wx = pwther_SFC.astype(int8)
        outOfAreaMask = logical_or(greater(wx, 7),less(wx, 0))
        wx[outOfAreaMask] = 0

        wxtypes = ["<NoCov>:<NoWx>:<NoInten>:<NoVis>:",
                   "Chc:R:-:<NoVis>:",
                   "SChc:R:-:<NoVis>:",
                   "Lkly:R:-:<NoVis>:",
                   "Def:R:-:<NoVis>:",
                   "Def:R:m:<NoVis>:",
                   "Lkly:R:m:<NoVis>:",
                   "Chc:R:m:<NoVis>:",
                   "SChc:R:m:<NoVis>:",
                   "Def:R:+:<NoVis>:",
                   "Lkly:R:+:<NoVis>:",
                   "Chc:R:+:<NoVis>:",
                   "Iso:T:<NoInten>:<NoVis>:^SChc:RW:-:<NoVis>:",
                   "Chc:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:",
                   "Chc:RW:-:<NoVis>:^Sct:T:<NoInten>:<NoVis>:",
                   "SChc:S:-:<NoVis>:^SChc:R:-:<NoVis>:",
                   "Chc:S:-:<NoVis>:^Chc:R:-:<NoVis>:",
                   "SChc:S:-:<NoVis>:",
                   "SChc:R:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Chc:S:-:<NoVis>:",
                   "Chc:R:-:<NoVis>:^Chc:S:-:<NoVis>:",
                   "Lkly:S:-:<NoVis>:^Lkly:R:-:<NoVis>:",
                   "Lkly:S:-:<NoVis>:",
                   "Lkly:S:m:<NoVis>:",
                   "Lkly:S:m:<NoVis>:^Lkly:R:m:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^Chc:S:m:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^Chc:S:-:<NoVis>:",
                   "Def:S:-:<NoVis>:^Def:R:-:<NoVis>:",
                   "Def:S:-:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^Lkly:S:-:<NoVis>:",
                   "Def:S:m:<NoVis>:",
                   "Def:R:-:<NoVis>:^Lkly:S:-:<NoVis>:",
                   "Def:S:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Lkly:S:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Def:S:-:<NoVis>:^SChc:R:-:<NoVis>:",
                   "Chc:S:m:<NoVis>:",
                   "SChc:S:m:<NoVis>:",
                   "Def:S:+:<NoVis>:",
                   "Lkly:S:+:<NoVis>:",
                   "Chc:R:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Def:R:-:<NoVis>:^Chc:S:-:<NoVis>:",
                   "Def:S:+:<NoVis>:^Def:R:+:<NoVis>:",
                   "Def:S:m:<NoVis>:^Chc:R:m:<NoVis>:",
                   "Def:S:-:<NoVis>:^Chc:R:-:<NoVis>:",
                   "Lkly:S:m:<NoVis>:^SChc:R:m:<NoVis>:",
                   "Def:S:m:<NoVis>:^Def:R:m:<NoVis>:",
                   "Def:S:m:<NoVis>:^Chc:ZR:m:<NoVis>:",
                   "Lkly:S:m:<NoVis>:^SChc:ZR:m:<NoVis>:",
                   "Lkly:S:-:<NoVis>:^SChc:R:-:<NoVis>:",
                   "Def:S:m:<NoVis>:^SChc:ZR:m:<NoVis>:",
                   "Def:S:m:<NoVis>:^SChc:R:m:<NoVis>:",
                   "Chc:S:-:<NoVis>:^SChc:R:-:<NoVis>:",
                   "Lkly:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:",
                   "Chc:S:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Lkly:S:+:<NoVis>:^Lkly:R:+:<NoVis>:",
                   "Lkly:RW:-:<NoVis>:^Sct:T:<NoInten>:<NoVis>:",
                   "Lkly:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Lkly:S:-:<NoVis>:^Chc:R:-:<NoVis>:",
                   "Chc:S:+:<NoVis>:",
                   "Chc:S:m:<NoVis>:^Chc:R:m:<NoVis>:",
                   "Lkly:S:m:<NoVis>:^Chc:ZR:m:<NoVis>:",
                   "Def:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Chc:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "SChc:S:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Chc:ZR:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Chc:ZR:-:<NoVis>:^Chc:S:-:<NoVis>:",
                   "SChc:ZR:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "SChc:ZR:-:<NoVis>:",
                   "Def:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:",
                   "SChc:S:m:<NoVis>:^SChc:R:m:<NoVis>:",
                   "Chc:RW:m:<NoVis>:^Iso:T:<NoInten>:<NoVis>:",
                   "Lkly:RW:m:<NoVis>:^Iso:T:<NoInten>:<NoVis>:",
                   "Chc:RW:m:<NoVis>:^Sct:T:<NoInten>:<NoVis>:",
                   "Lkly:RW:m:<NoVis>:^Sct:T:<NoInten>:<NoVis>:",
                   "Def:RW:-:<NoVis>:^Sct:T:<NoInten>:<NoVis>:",
                   "Def:RW:m:<NoVis>:^Sct:T:<NoInten>:<NoVis>:",
                   "Def:RW:m:<NoVis>:^Iso:T:<NoInten>:<NoVis>:",
                   "Chc:R:m:<NoVis>:^Chc:S:m:<NoVis>:",
                   "Def:S:-:<NoVis>:^Lkly:R:-:<NoVis>:",
                   "Chc:S:m:<NoVis>:^SChc:ZR:m:<NoVis>:",
                   "Def:S:+:<NoVis>:^Chc:ZR:+:<NoVis>:",
                   "Lkly:ZR:-:<NoVis>:^Chc:S:-:<NoVis>:",
                   "Lkly:S:-:<NoVis>:^Lkly:ZR:-:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Lkly:ZR:-:<NoVis>:^Lkly:S:-:<NoVis>:",
                   "Def:ZR:-:<NoVis>:^Chc:S:-:<NoVis>:",
                   "Def:ZR:-:<NoVis>:^Lkly:S:-:<NoVis>:",
                   "Def:S:-:<NoVis>:^Lkly:ZR:-:<NoVis>:",
                   "Def:R:-:<NoVis>:^Def:S:-:<NoVis>:",
                   "Lkly:S:m:<NoVis>:^Chc:R:m:<NoVis>:",
                   "Def:R:m:<NoVis>:^Chc:S:m:<NoVis>:",
                   "SChc:IP:-:<NoVis>:^SChc:S:-:<NoVis>:^SChc:R:-:<NoVis>:",
                   "Chc:IP:-:<NoVis>:^Chc:S:-:<NoVis>:^Chc:R:-:<NoVis>:",
                   "Def:S:+:<NoVis>:^Chc:R:+:<NoVis>:",
                   "Def:R:m:<NoVis>:^Lkly:S:m:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^Lkly:S:m:<NoVis>:",
                   "Def:RW:+:<NoVis>:^Iso:T:<NoInten>:<NoVis>:",
                   "Chc:R:m:<NoVis>:^SChc:S:m:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Chc:R:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^Chc:IP:-:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^Chc:IP:m:<NoVis>:",
                   "Chc:R:-:<NoVis>:^Chc:IP:-:<NoVis>:",
                   "Chc:R:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Chc:R:m:<NoVis>:^Chc:IP:m:<NoVis>:",
                   "Chc:R:-:<NoVis>:^SChc:IP:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Def:R:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Def:R:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Def:R:m:<NoVis>:^SChc:ZR:m:<NoVis>:",
                   "Def:R:-:<NoVis>:^Chc:IP:-:<NoVis>:",
                   "Def:R:m:<NoVis>:^Chc:IP:m:<NoVis>:",
                   "Lkly:R:+:<NoVis>:^SChc:IP:+:<NoVis>:",
                   "Def:R:+:<NoVis>:^SChc:IP:+:<NoVis>:",
                   "Lkly:R:+:<NoVis>:^SChc:ZR:+:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^SChc:ZR:m:<NoVis>:",
                   "Def:R:m:<NoVis>:^Chc:ZR:m:<NoVis>:",
                   "Lkly:R:+:<NoVis>:^Chc:ZR:+:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^Chc:IP:m:<NoVis>:^SChc:ZR:m:<NoVis>:",
                   "Def:R:+:<NoVis>:^Chc:IP:+:<NoVis>:",
                   "Lkly:R:+:<NoVis>:^Chc:IP:+:<NoVis>:",
                   "Def:R:m:<NoVis>:^Chc:IP:m:<NoVis>:^SChc:ZR:m:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^Chc:IP:m:<NoVis>:^Chc:ZR:m:<NoVis>:",
                   "Lkly:R:+:<NoVis>:^Chc:IP:+:<NoVis>:^SChc:ZR:+:<NoVis>:",
                   "Def:R:+:<NoVis>:^Chc:IP:+:<NoVis>:^SChc:ZR:+:<NoVis>:",
                   "Def:R:m:<NoVis>:^SChc:IP:m:<NoVis>:^SChc:ZR:m:<NoVis>:",
                   "Def:R:+:<NoVis>:^SChc:IP:+:<NoVis>:^SChc:ZR:+:<NoVis>:",
                   "Def:R:+:<NoVis>:^SChc:ZR:+:<NoVis>:",
                   "Def:R:m:<NoVis>:^Chc:IP:m:<NoVis>:^Chc:ZR:m:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^Chc:IP:m:<NoVis>:^Chc:R:m:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^SChc:IP:m:<NoVis>:^SChc:ZR:m:<NoVis>:",
                   "Lkly:R:+:<NoVis>:^SChc:ZR:+:<NoVis>:^SChc:IP:+:<NoVis>:",
                   "Def:R:+:<NoVis>:^SChc:ZR:+:<NoVis>:^SChc:IP:+:<NoVis>:",
                   "Def:R:+:<NoVis>:^Chc:ZR:+:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^Lkly:IP:m:<NoVis>:",
                   "Lkly:R:+:<NoVis>:^SChc:IP:+:<NoVis>:^SChc:ZR:+:<NoVis>:",
                   "Lkly:R:+:<NoVis>:^Chc:ZR:+:<NoVis>:^SChc:IP:+:<NoVis>:",
                   "Def:R:+:<NoVis>:^Chc:ZR:+:<NoVis>:^SChc:IP:+:<NoVis>:",
                   "Def:R:m:<NoVis>:^SChc:ZR:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Def:ZR:m:<NoVis>:^Chc:R:m:<NoVis>:",
                   "Lkly:R:+:<NoVis>:^Chc:IP:+:<NoVis>:^Chc:ZR:+:<NoVis>:",
                   "Def:R:+:<NoVis>:^Chc:IP:+:<NoVis>:^Chc:ZR:+:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^Chc:ZR:m:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^Chc:R:m:<NoVis>:",
                   "Def:R:+:<NoVis>:^Lkly:IP:+:<NoVis>:^SChc:ZR:+:<NoVis>:",
                   "Def:R:m:<NoVis>:^Lkly:IP:m:<NoVis>:^SChc:ZR:m:<NoVis>:",
                   "Lkly:R:+:<NoVis>:^Lkly:IP:+:<NoVis>:^SChc:ZR:+:<NoVis>:",
                   "Def:R:+:<NoVis>:^Lkly:IP:+:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^Chc:IP:m:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^Lkly:IP:m:<NoVis>:",
                   "Lkly:IP:-:<NoVis>:^Lkly:R:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^Lkly:IP:m:<NoVis>:^SChc:ZR:m:<NoVis>:",
                   "Lkly:R:+:<NoVis>:^Lkly:IP:+:<NoVis>:",
                   "Def:R:m:<NoVis>:^Lkly:IP:m:<NoVis>:",
                   "Lkly:IP:-:<NoVis>:^Lkly:R:-:<NoVis>:",
                   "Def:IP:-:<NoVis>:^Def:R:-:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Lkly:IP:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Lkly:IP:-:<NoVis>:^Lkly:ZR:-:<NoVis>:",
                   "Chc:R:m:<NoVis>:^SChc:ZR:m:<NoVis>:",
                   "Lkly:ZR:+:<NoVis>:^Chc:R:+:<NoVis>:",
                   "Chc:S:+:<NoVis>:^Chc:R:+:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Chc:IP:-:<NoVis>:^Chc:R:-:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^SChc:R:m:<NoVis>:",
                   "Lkly:IP:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Chc:S:m:<NoVis>:^SChc:R:m:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^SChc:IP:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Chc:ZR:m:<NoVis>:^SChc:R:m:<NoVis>:",
                   "Chc:ZR:m:<NoVis>:^Chc:R:m:<NoVis>:",
                   "Chc:R:m:<NoVis>:^Chc:ZR:m:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^SChc:ZR:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Chc:R:m:<NoVis>:^SChc:ZR:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^SChc:IP:m:<NoVis>:^SChc:S:m:<NoVis>:",
                   "Lkly:IP:-:<NoVis>:",
                   "Chc:R:m:<NoVis>:^SChc:IP:m:<NoVis>:^SChc:ZR:m:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^SChc:IP:m:<NoVis>:^SChc:S:m:<NoVis>:",
                   "Chc:ZR:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Chc:R:m:<NoVis>:^Chc:IP:m:<NoVis>:^SChc:ZR:m:<NoVis>:",
                   "Def:R:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^SChc:S:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^SChc:S:m:<NoVis>:",
                   "Chc:ZR:m:<NoVis>:^SChc:S:m:<NoVis>:",
                   "Def:ZR:m:<NoVis>:^Chc:IP:m:<NoVis>:^Chc:S:m:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^Chc:IP:m:<NoVis>:^Chc:S:m:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^Chc:S:m:<NoVis>:^Chc:IP:m:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^Chc:ZR:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Chc:R:m:<NoVis>:^Chc:ZR:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Def:ZR:m:<NoVis>:^Chc:S:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^Chc:ZR:m:<NoVis>:^Chc:IP:m:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^Chc:R:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^SChc:R:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^Chc:S:m:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^Chc:S:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Def:ZR:m:<NoVis>:^Chc:S:m:<NoVis>:^Chc:IP:m:<NoVis>:",
                   "Lkly:IP:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Lkly:IP:-:<NoVis>:^Lkly:R:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^Lkly:IP:m:<NoVis>:^Chc:ZR:m:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^Chc:R:m:<NoVis>:^Chc:IP:m:<NoVis>:",
                   "Def:R:-:<NoVis>:^Chc:ZR:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Def:R:m:<NoVis>:^SChc:IP:m:<NoVis>:^SChc:S:m:<NoVis>:",
                   "Def:R:m:<NoVis>:^Chc:IP:m:<NoVis>:^Chc:S:m:<NoVis>:",
                   "Def:ZR:m:<NoVis>:^Chc:S:m:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^Lkly:S:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Lkly:IP:-:<NoVis>:^Chc:ZR:-:<NoVis>:^Chc:S:-:<NoVis>:",
                   "Lkly:IP:-:<NoVis>:^SChc:ZR:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Lkly:IP:-:<NoVis>:^SChc:S:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^SChc:ZR:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Def:R:-:<NoVis>:^SChc:IP:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Def:R:-:<NoVis>:^SChc:ZR:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^Lkly:S:m:<NoVis>:",
                   "Lkly:IP:-:<NoVis>:^Chc:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^Lkly:IP:m:<NoVis>:^Chc:R:m:<NoVis>:",
                   "Def:R:m:<NoVis>:^Chc:ZR:m:<NoVis>:^Chc:IP:m:<NoVis>:",
                   "Def:ZR:m:<NoVis>:^Chc:R:m:<NoVis>:^Chc:IP:m:<NoVis>:",
                   "Def:IP:-:<NoVis>:^Lkly:ZR:-:<NoVis>:",
                   "Def:IP:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Def:ZR:m:<NoVis>:^Def:IP:m:<NoVis>:^Chc:R:m:<NoVis>:",
                   "Def:ZR:m:<NoVis>:^Lkly:IP:m:<NoVis>:^Chc:R:m:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:^Chc:IP:m:<NoVis>:^SChc:R:m:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^Chc:ZR:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Def:R:m:<NoVis>:^Chc:ZR:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^Chc:IP:m:<NoVis>:^Chc:S:m:<NoVis>:",
                   "Lkly:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:^Chc:IP:-:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^Chc:ZR:-:<NoVis>:^Chc:IP:-:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^SChc:IP:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Chc:R:m:<NoVis>:^Chc:IP:m:<NoVis>:^Chc:S:m:<NoVis>:",
                   "Lkly:S:-:<NoVis>:^Lkly:ZR:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Lkly:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Def:IP:-:<NoVis>:^Def:ZR:-:<NoVis>:",
                   "Def:ZR:m:<NoVis>:^Chc:IP:m:<NoVis>:^Chc:R:m:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^Chc:IP:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Lkly:ZR:-:<NoVis>:^Chc:R:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Def:ZR:-:<NoVis>:^Chc:R:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Def:ZR:m:<NoVis>:^Chc:R:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Chc:ZR:m:<NoVis>:^Chc:IP:m:<NoVis>:^Chc:S:m:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^Chc:IP:-:<NoVis>:^Chc:S:-:<NoVis>:",
                   "Def:ZR:-:<NoVis>:^Chc:R:-:<NoVis>:",
                   "Def:ZR:-:<NoVis>:^Chc:IP:-:<NoVis>:^Chc:R:-:<NoVis>:",
                   "Def:R:-:<NoVis>:^Chc:IP:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Def:R:-:<NoVis>:^Chc:ZR:-:<NoVis>:^Chc:IP:-:<NoVis>:",
                   "Def:ZR:-:<NoVis>:^Chc:R:-:<NoVis>:^Chc:IP:-:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^Chc:IP:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^SChc:S:m:<NoVis>:",
                   "Chc:ZR:m:<NoVis>:^Chc:S:m:<NoVis>:",
                   "Lkly:ZR:-:<NoVis>:^Chc:IP:-:<NoVis>:^Chc:S:-:<NoVis>:",
                   "Lkly:ZR:-:<NoVis>:^Chc:R:-:<NoVis>:",
                   "Lkly:ZR:-:<NoVis>:^Chc:R:-:<NoVis>:^Chc:IP:-:<NoVis>:",
                   "Lkly:IP:-:<NoVis>:^Chc:ZR:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Def:IP:-:<NoVis>:^Chc:ZR:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Def:IP:-:<NoVis>:^SChc:ZR:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Def:IP:-:<NoVis>:^Chc:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Def:IP:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Def:IP:-:<NoVis>:",
                   "Def:ZR:-:<NoVis>:^Lkly:IP:-:<NoVis>:",
                   "Def:ZR:-:<NoVis>:^Lkly:IP:-:<NoVis>:^Chc:R:-:<NoVis>:",
                   "Def:R:-:<NoVis>:^Lkly:IP:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Def:IP:-:<NoVis>:^Chc:ZR:-:<NoVis>:^Chc:S:-:<NoVis>:",
                   "Def:ZR:-:<NoVis>:^Def:IP:-:<NoVis>:",
                   "Def:R:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Def:R:m:<NoVis>:^Lkly:IP:m:<NoVis>:^Lkly:S:m:<NoVis>:",
                   "Def:IP:-:<NoVis>:^Lkly:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Def:IP:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Lkly:R:m:<NoVis>:^Lkly:IP:m:<NoVis>:^Lkly:S:m:<NoVis>:",
                   "Def:IP:-:<NoVis>:^Def:S:-:<NoVis>:^Def:R:-:<NoVis>:",
                   "Def:S:-:<NoVis>:^Chc:IP:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Lkly:IP:-:<NoVis>:^Lkly:S:-:<NoVis>:^Lkly:R:-:<NoVis>:",
                   "Def:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:^Chc:IP:-:<NoVis>:",
                   "Def:IP:-:<NoVis>:^Def:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Def:IP:-:<NoVis>:^Chc:S:-:<NoVis>:",
                   "Def:R:-:<NoVis>:^SChc:IP:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Lkly:ZR:-:<NoVis>:",
                   "Lkly:ZR:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Def:S:-:<NoVis>:^Lkly:IP:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Def:R:-:<NoVis>:^Chc:IP:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Lkly:ZR:-:<NoVis>:^SChc:R:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Lkly:ZR:-:<NoVis>:^SChc:IP:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Def:ZR:m:<NoVis>:^Def:IP:m:<NoVis>:^Def:S:m:<NoVis>:",
                   "Def:S:m:<NoVis>:^SChc:IP:m:<NoVis>:",
                   "Def:S:-:<NoVis>:^Chc:IP:-:<NoVis>:",
                   "Def:S:-:<NoVis>:^Def:R:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Def:S:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Def:IP:-:<NoVis>:^Lkly:S:-:<NoVis>:",
                   "Def:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Def:IP:-:<NoVis>:^Def:S:-:<NoVis>:",
                   "Def:S:-:<NoVis>:^Lkly:IP:-:<NoVis>:",
                   "Lkly:ZR:m:<NoVis>:",
                   "Def:IP:-:<NoVis>:^Def:S:-:<NoVis>:^Lkly:ZR:-:<NoVis>:",
                   "Def:S:-:<NoVis>:^Lkly:ZR:-:<NoVis>:^Lkly:IP:-:<NoVis>:",
                   "Chc:R:-:<NoVis>:^SChc:ZR:-:<NoVis>:",
                   "Def:ZR:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Lkly:ZR:-:<NoVis>:^Chc:IP:-:<NoVis>:",
                   "Def:ZR:-:<NoVis>:^Lkly:IP:-:<NoVis>:^Lkly:S:-:<NoVis>:",
                   "Lkly:ZR:-:<NoVis>:^Lkly:IP:-:<NoVis>:^Lkly:S:-:<NoVis>:",
                   "Lkly:ZR:-:<NoVis>:^SChc:R:-:<NoVis>:",
                   "Lkly:ZR:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Chc:ZR:-:<NoVis>:^Chc:R:-:<NoVis>:",
                   "Chc:ZR:-:<NoVis>:^Chc:IP:-:<NoVis>:^Chc:S:-:<NoVis>:",
                   "Chc:ZR:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Chc:ZR:-:<NoVis>:^SChc:R:-:<NoVis>:",
                   "Chc:R:-:<NoVis>:^Chc:IP:-:<NoVis>:^Chc:S:-:<NoVis>:",
                   "Chc:ZR:-:<NoVis>:",
                   "Chc:ZR:-:<NoVis>:^Chc:IP:-:<NoVis>:",
                   "Chc:ZR:-:<NoVis>:^SChc:IP:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Chc:R:-:<NoVis>:^Chc:ZR:-:<NoVis>:",
                   "Lkly:R:-:<NoVis>:^Lkly:IP:-:<NoVis>:^Lkly:S:-:<NoVis>:",
                   "Chc:ZR:-:<NoVis>:^Chc:IP:-:<NoVis>:^SChc:S:-:<NoVis>:",
                   "Chc:ZR:-:<NoVis>:^Chc:S:-:<NoVis>:^Chc:IP:-:<NoVis>:",
                   "Chc:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Chc:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:^Chc:IP:-:<NoVis>:",
                   "Chc:ZR:-:<NoVis>:^Chc:S:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Chc:ZR:-:<NoVis>:^SChc:S:-:<NoVis>:^SChc:IP:-:<NoVis>:",
                   "Def:S:+:<NoVis>:^SChc:ZR:+:<NoVis>:",
                   "SChc:R:-:<NoVis>:^SChc:IP:-:<NoVis>:^SChc:S:-:<NoVis>:",
               ]

        
        return (wx, wxtypes)

    def calcSnowAmt01(self, totsn1hr_SFC):
        return (totsn1hr_SFC * 1000) / 25.4

    def calcIceAccum01(self, ficeac1hr_SFC):
        return (ficeac1hr_SFC * 1000) / 25.4

    def calcIceAccum(self, ficeac6hr_SFC):
        return (ficeac6hr_SFC * 1000) / 25.4

    def calcQPF10Prcntl(self, tp10pct6hr_SFC):
        return tp10pct6hr_SFC / 25.4

    def calcQPF50Prcntl(self, tp50pct6hr_SFC):
        return tp50pct6hr_SFC / 25.4
    
    def calcQPF90Prcntl(self, tp90pct6hr_SFC):
        return tp90pct6hr_SFC / 25.4

    def calcQPF10Prcntl24hr (self, tp10pct24hr_SFC):
        return tp10pct24hr_SFC / 25.4

    def calcQPF50Prcntl24hr (self, tp50pct24hr_SFC):
        return tp50pct24hr_SFC / 25.4

    def calcQPF90Prcntl24hr (self, tp90pct24hr_SFC):
        return tp90pct24hr_SFC / 25.4

    def calcTstmPrb1(self, thp1hr_SFC):
        return thp1hr_SFC

    def calcDryTstmPrb(self, drytpprob3hr_SFC):
        return drytpprob3hr_SFC

    def calcTstmPrb12(self, thp12hr_SFC):
        return thp12hr_SFC
    
    def calcDswrf (self, dswrf_SFC):
        return dswrf_SFC
    
    def calcFreezingSpray (self, frzspr_SFC):
        return frzspr_SFC
    
    def calcIceAccum05PrcntlF24hr (self, ficeac5pct24hr_SFC):
        return ficeac5pct24hr_SFC

    def calcIceAccum10PrcntlF24hr (self, ficeac10pct24hr_SFC):
        return ficeac10pct24hr_SFC

    def calcIceAccum25PrcntlF24hr (self, ficeac25pct24hr_SFC):
        return ficeac25pct24hr_SFC

    def calcIceAccum50PrcntlF24hr (self, ficeac50pct24hr_SFC):
        return ficeac50pct24hr_SFC

    def calcIceAccum75PrcntlF24hr (self, ficeac75pct24hr_SFC):
        return ficeac75pct24hr_SFC

    def calcIceAccum90PrcntlF24hr (self, ficeac90pct24hr_SFC):
        return ficeac90pct24hr_SFC

    def calcIceAccum95PrcntlF24hr (self, ficeac95pct24hr_SFC):
        return ficeac95pct24hr_SFC

    def calcIceAccum05PrcntlF48hr (self, ficeac5pct48hr_SFC):
        return ficeac5pct48hr_SFC

    def calcIceAccum10PrcntlF48hr (self, ficeac10pct48hr_SFC):
        return ficeac10pct48hr_SFC

    def calcIceAccum25PrcntlF48hr (self, ficeac25pct48hr_SFC):
        return ficeac25pct48hr_SFC

    def calcIceAccum50PrcntlF48hr (self, ficeac50pct48hr_SFC):
        return ficeac50pct48hr_SFC

    def calcIceAccum75PrcntlF48hr (self, ficeac75pct48hr_SFC):
        return ficeac75pct48hr_SFC

    def calcIceAccum90PrcntlF48hr (self, ficeac90pct48hr_SFC):
        return ficeac90pct48hr_SFC

    def calcIceAccum95PrcntlF48hr (self, ficeac95pct48hr_SFC):
        return ficeac95pct48hr_SFC

    def calcIceAccum05PrcntlF72hr (self, ficeac5pct72hr_SFC):
        return ficeac5pct72hr_SFC

    def calcIceAccum10PrcntlF72hr (self, ficeac10pct72hr_SFC):
        return ficeac10pct72hr_SFC

    def calcIceAccum25PrcntlF72hr (self, ficeac25pct72hr_SFC):
        return ficeac25pct72hr_SFC

    def calcIceAccum50PrcntlF72hr (self, ficeac50pct72hr_SFC):
        return ficeac50pct72hr_SFC

    def calcIceAccum75PrcntlF72hr (self, ficeac75pct72hr_SFC):
        return ficeac75pct72hr_SFC

    def calcIceAccum90PrcntlF72hr (self, ficeac90pct72hr_SFC):
        return ficeac90pct72hr_SFC

    def calcIceAccum95PrcntlF72hr (self, ficeac95pct72hr_SFC):
        return ficeac95pct72hr_SFC

    def calcPrecipDur (self, pcpdur12hr_SFC):
        return pcpdur12hr_SFC
    
    def calcSnowAmt05PrcntlF24hr (self, totsn5pct24hr_SFC):
        return totsn5pct24hr_SFC

    def calcSnowAmt10PrcntlF24hr (self, totsn10pct24hr_SFC):
        return totsn10pct24hr_SFC

    def calcSnowAmt25PrcntlF24hr (self, totsn25pct24hr_SFC):
        return totsn25pct24hr_SFC

    def calcSnowAmt50PrcntlF24hr (self, totsn50pct24hr_SFC):
        return totsn50pct24hr_SFC

    def calcSnowAmt75PrcntlF24hr (self, totsn75pct24hr_SFC):
        return totsn75pct24hr_SFC

    def calcSnowAmt90PrcntlF24hr (self, totsn90pct24hr_SFC):
        return totsn90pct24hr_SFC

    def calcSnowAmt95PrcntlF24hr (self, totsn95pct24hr_SFC):
        return totsn95pct24hr_SFC

    def calcSnowAmt05PrcntlF48hr (self, totsn5pct48hr_SFC):
        return totsn5pct48hr_SFC

    def calcSnowAmt10PrcntlF48hr (self, totsn10pct48hr_SFC):
        return totsn10pct48hr_SFC

    def calcSnowAmt25PrcntlF48hr (self, totsn25pct48hr_SFC):
        return totsn25pct48hr_SFC

    def calcSnowAmt50PrcntlF48hr (self, totsn50pct48hr_SFC):
        return totsn50pct48hr_SFC

    def calcSnowAmt75PrcntlF48hr (self, totsn75pct48hr_SFC):
        return totsn75pct48hr_SFC

    def calcSnowAmt90PrcntlF48hr (self, totsn90pct48hr_SFC):
        return totsn90pct48hr_SFC

    def calcSnowAmt95PrcntlF48hr (self, totsn95pct48hr_SFC):
        return totsn95pct48hr_SFC

    def calcSnowAmt05PrcntlF72hr (self, totsn5pct72hr_SFC):
        return totsn5pct72hr_SFC

    def calcSnowAmt10PrcntlF72hr (self, totsn10pct72hr_SFC):
        return totsn10pct72hr_SFC

    def calcSnowAmt25PrcntlF72hr (self, totsn25pct72hr_SFC):
        return totsn25pct72hr_SFC

    def calcSnowAmt50PrcntlF72hr (self, totsn50pct72hr_SFC):
        return totsn50pct72hr_SFC

    def calcSnowAmt75PrcntlF72hr (self, totsn75pct72hr_SFC):
        return totsn75pct72hr_SFC

    def calcSnowAmt90PrcntlF72hr (self, totsn90pct72hr_SFC):
        return totsn90pct72hr_SFC

    def calcSnowAmt95PrcntlF72hr (self, totsn95pct72hr_SFC):
        return totsn95pct72hr_SFC

    def calcVis50pct (self, vis50pct_SFC):
        return vis50pct_SFC

def main():
    NationalBlendForecaster().run()

if __name__ == "__main__":
    main()
