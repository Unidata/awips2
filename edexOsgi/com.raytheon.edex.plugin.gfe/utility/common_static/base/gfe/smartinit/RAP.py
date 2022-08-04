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
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Apr 19, 2018  7271     randerso  Renamed and/or removed models. Code cleanup.
#
##

##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
##

from Init import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from RAP model
## output.
##
##--------------------------------------------------------------------------
class RAPForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "RAP")

##--------------------------------------------------------------------------
## These levels will be used to create vertical soundings.  These are
## defined here since they are model dependent.
##--------------------------------------------------------------------------
    def levels(self):
        return ["MB1000", "MB950", "MB900", "MB850", "MB800", "MB750", "MB700",
                "MB650", "MB600", "MB550", "MB500", "MB450", "MB400",
                "MB350", "MB300"]

##-------------------------------------------------------------------------
## Returns the maximum of the specified MaxT and the T grids
##--------------------------------------------------------------------------
    def calcMaxT(self, T, MaxT):
        if MaxT is None:
            return T
        return maximum(MaxT, T)

##-------------------------------------------------------------------------
## Returns the minimum of the specified MinT and T grids
##--------------------------------------------------------------------------
    def calcMinT(self, T, MinT):
        if MinT is None:
            return T
        return minimum(MinT, T)

##-------------------------------------------------------------------------
## Calculates the temperature at the elevation indicated in the topo
## grid.  This tool simply interpolates the temperature value from
## model's isobaric temperature cube.
##-------------------------------------------------------------------------
    def calcT(self, t_FHAG2, t_BL030, t_BL6090, t_BL150180,
              p_SFC, topo, stopo, gh_c, t_c):
        p = self.newGrid(-1)
        tmb = self.newGrid(-1)
        tms = self.newGrid(-1)
        # go up the column to figure out the surface pressure
        for i in range(1, gh_c.shape[0]):
            higher = greater(gh_c[i], topo)
            # interpolate the pressure at topo height
            val = self.linear(gh_c[i], gh_c[i - 1],
                              log(self.pres[i]), log(self.pres[i - 1]), topo)
            val.clip(-.00001, 10, val)
            
            m = logical_and(equal(p, -1), higher)
            p[m] = exp(val)[m]
            # interpolate the temperature at true elevation
            tval1 = self.linear(gh_c[i], gh_c[i - 1], t_c[i], t_c[i - 1], topo)
            
            m = logical_and(equal(tmb, -1), higher)
            tmb[m] = tval1[m]
            # interpolate the temperature at model elevation
            tval2 = self.linear(gh_c[i], gh_c[i - 1], t_c[i], t_c[i - 1], stopo)
            
            m = logical_and(equal(tms, -1), greater(gh_c[i], stopo))
            tms[m] = tval2[m]

        p_SFC /= 100
        # define the pres. of each of the boundary layers
        pres = [p_SFC, p_SFC - 15, p_SFC - 75, p_SFC - 168]

        # list of temperature grids
        temps = [t_FHAG2, t_BL030, t_BL6090, t_BL150180]
        st = self.newGrid(-1)
        # Calculate the lapse rate in units of pressure
        for i in range(1, len(pres)):
            val = self.linear(pres[i], pres[i - 1], temps[i], temps[i - 1], p)
            gm = greater(pres[i - 1], p)
            lm = less_equal(pres[i], p)
            mask = logical_and(gm, lm)
            
            m = logical_and(equal(st, -1), mask)
            st[m] = val[m]            

        # where topo level is above highest level in BL fields...use tmb
        m = logical_and(equal(st,-1),less(p,p_SFC-135))
        st[m] = tmb[m]

        # where topo level is below model surface...use difference
        # of t at pressure of surface and tFHAG2 and subtract from tmb
        m = equal(st, -1)
        st[m] = (tmb - tms + t_FHAG2)[m]
        return self.KtoF(st)


##-------------------------------------------------------------------------
## Calculates dew point from the specified pressure, temp and rh
## fields.
##-------------------------------------------------------------------------
    def calcTd(self, p_SFC, T, t_FHAG2, stopo, topo, rh_FHAG2):
        # at the model surface
        sfce = rh_FHAG2 / 100 * self.esat(t_FHAG2)
        w = (0.622 * sfce) / ((p_SFC + 0.0001) / 100 - sfce)
        # at the true surface
        tsfce = self.esat(self.FtoK(T))
        dpdz = 287.04 * t_FHAG2 / (p_SFC / 100 * 9.8) # meters / millibar
        newp = p_SFC / 100 + (stopo - topo) / dpdz
        ws = (0.622 * tsfce) / (newp - tsfce)
        rh = w / ws
        # Finally, calculate the dew point
        tsfcesat = rh * tsfce
        tsfcesat.clip(0.00001, tsfcesat, tsfcesat)
        b = 26.66082 - log(tsfcesat)
        td = (b - sqrt(b * b - 223.1986)) / 0.0182758048
        td = self.KtoF(td)
        
        m = w > ws
        td[m] = T[m]
        return td

##-------------------------------------------------------------------------
##  Calculates RH from the T and Td grids
##-------------------------------------------------------------------------
    def calcRH(self, T, Td):
        Tc = .556 * (T - 32.0)
        Tdc = .556 * (Td - 32.0)
        Vt = 6.11 * pow(10, (Tc * 7.5 / (Tc + 237.3)))
        Vd = 6.11 * pow(10, (Tdc * 7.5 / (Tdc + 237.3)))
        RH = (Vd / Vt) * 100.0
        # Return the new value
        return RH

##-------------------------------------------------------------------------
## Returns the maximum of the specified MaxRH and the RH grids
##--------------------------------------------------------------------------
    def calcMaxRH(self, RH, MaxRH):
        if MaxRH is None:
            return RH
        return maximum(MaxRH, RH)

##-------------------------------------------------------------------------
## Returns the minimum of the specified MinRH and RH grids
##--------------------------------------------------------------------------
    def calcMinRH(self, RH, MinRH):
        if MinRH is None:
            return RH
        return minimum(MinRH, RH)

##-------------------------------------------------------------------------
##  Calculates QPF from the total precip field out of the model
##-------------------------------------------------------------------------
    def calcQPF(self, cp_SFC, lgsp_SFC):
        tp_SFC = cp_SFC + lgsp_SFC
        qpf = tp_SFC / 25.4   # convert from millimeters to inches
        return qpf

    def calcSky(self, rh_c, gh_c, topo, p_SFC):
        return self.skyFromRH(rh_c, gh_c, topo, p_SFC)

##-------------------------------------------------------------------------
##  Calculates Prob. of Precip. based on QPF and RH cube.  Where there
##  is QPF > 0 ramp the PoP from (0.01, 35%) to 100%.  Then in areas
##  of QPF < 0.2 raise the PoP if it's very humid.
##-------------------------------------------------------------------------
    def calcPoP(self, gh_c, rh_c, QPF, topo):
        rhavg = where(less(gh_c, topo), float32(-1), rh_c)
        rhavg[greater(gh_c, topo + 5000 * 0.3048)] = -1
        count = not_equal(rhavg, -1)
        rhavg[equal(rhavg, -1)] = 0
        count = add.reduce(count, 0, dtype=float32)
        rhavg = add.reduce(rhavg, 0)
        ## add this much based on humidity only
        dpop = where(count, rhavg / (count + .001), 0) - 70.0
        dpop[less(dpop, -30)] = -30
        ## calculate the base PoP
        pop = where(less(QPF, 0.02), QPF * 1000, QPF * 350 + 13)
        pop += dpop   # add the adjustment based on humidity
        pop.clip(0, 100, pop)  # clip to 100%
        return pop

##-------------------------------------------------------------------------
##  Calculates the Freezing level based on height and temperature
##  cubes.  Finds the height at which freezing occurs.
##-------------------------------------------------------------------------
    def calcFzLevel(self, gh_FRZ):
        return gh_FRZ / 0.3048

##-------------------------------------------------------------------------
##  Calculates the Snow level based on wet-bulb zero height.
##-------------------------------------------------------------------------
    def calcSnowLevel(self, gh_c, t_c, rh_c):
        # Only use the levels that are >= freezind (plus one level)
        # This is a performance and memory optimization
        clipindex = 2
        for i in range(t_c.shape[0] - 1, -1, -1):
            if maximum.reduce(maximum.reduce(t_c[i])) >= 273.15:
                clipindex = i + 1
                break
        gh_c = gh_c[:clipindex, :, :]
        t_c = t_c[:clipindex, :, :]
        rh_c = rh_c[:clipindex, :, :]

        snow = self.newGrid(-1)
        #
        #  make pressure cube
        #
        pmb = ones_like(gh_c)
        for i in range(gh_c.shape[0]):
           pmb[i] = self.pres[i]
        pmb.clip(1, 1050, pmb)
        #
        #  convert temps to C and limit to reasonable values
        #
        tc = t_c - 273.15
        tc.clip(-120, 60, tc)
        #
        #  limit RH to reasonable values
        #
        rh = clip(rh_c, 0.5, 99.5)
        #
        #  calculate the wetbulb temperatures
        #     (this is expensive - even in numeric python - and somewhat
        #      wasteful, since you do not need to calculate the wetbulb
        #      temp for all levels when it may cross zero way down toward
        #      the bottom.  Nevertheless - all the gridpoints will cross
        #      zero at different levels - so you cannot know ahead of time
        #      how high up to calculate them.  In the end - this was the
        #      most expedient way to code it - and it works - so I stuck
        #      with it.
        #
        wetb = self.Wetbulb(tc, rh, pmb)
        tc = rh = pmb = None
        #
        #  find the zero level
        #
        for i in range(1, gh_c.shape[0]):
           try:
              val = gh_c[i - 1] + (gh_c[i] - gh_c[i - 1]) / (wetb[i] - wetb[i - 1])\
                 * (-wetb[i - 1])
           except:
              val = gh_c[i]
           m = logical_and(equal(snow, -1), less_equal(wetb[i], 0))
           snow[m] = val[m]
        #
        #  convert to feet
        #
        snow /= 0.3048

        return snow

##-------------------------------------------------------------------------
##  Calculates Snow amount based on the Temp, Freezing level, QPF,
##  topo and Weather grid
##-------------------------------------------------------------------------
    def calcSnowAmt(self, T, FzLevel, QPF, topo, Wx):
        # figure out the snow to liquid ratio
        snowr = T * -0.5 + 22.5
        snowr[less(T, 9)] = 20
        snowr[greater_equal(T, 30)] = 0
        # calc. snow amount based on the QPF and the ratio
        snowamt = where(less_equal(FzLevel - 1000, topo / 0.3048),
                        snowr * QPF, float32(0))
        # Only make snow at points where the weather is snow
        snowmask = logical_or(equal(Wx[0], 1), equal(Wx[0], 3))
        snowmask = logical_or(snowmask, logical_or(equal(Wx[0], 7),
                                                   equal(Wx[0], 9)))
        snowamt[logical_not(snowmask)] = 0
        return snowamt

##--------------------------------------------------------------------------
##  Calculate the Haines index based on the temp and RH cubes
##  Define self.whichHainesIndex to be "HIGH", "MEDIUM", or "LOW".
##  Default is "HIGH".
##--------------------------------------------------------------------------
    def calcHaines(self, t_c, rh_c):
        return self.hainesIndex(self.whichHainesIndex, t_c, rh_c)

##-------------------------------------------------------------------------
##  Calculates the mixing height for the given sfc temperature,
##  temperature cube, height cube and topo
##-------------------------------------------------------------------------
    def calcMixHgt(self, topo, t_c, gh_c):
        mask = greater_equal(gh_c, topo)
        pt = []
        for i in range(len(self.pres)):   # for each pres. level
            p = self.newGrid(self.pres[i]) # get the pres. value in mb
            tmp = self.ptemp(t_c[i], p)    # calculate the pot. temp
            pt = pt + [tmp]                # add to the list
        pt = array(pt)
        pt[logical_not(mask)] = 0
        avg = add.accumulate(pt, 0)
        count = add.accumulate(mask, 0)
        mh = self.newGrid(-1)
        # for each pres. level, calculate a running avg. of pot temp.
        # As soon as the next point deviates from the running avg by
        # more than 3 deg. C, interpolate to get the mixing height.
        for i in range(1, avg.shape[0]):
            runavg = avg[i] / (count[i] + .0001)
            diffpt = pt[i] - runavg
            # calc. the interpolated mixing height
            tmh = self.linear(pt[i], pt[i - 1], gh_c[i], gh_c[i - 1], runavg)
            # assign new values if the difference is greater than 3
            m = logical_and(logical_and(mask[i], equal(mh, -1)), greater(diffpt, 3))
            mh[m] = tmh[m]
        mh -= topo
        mh /= 0.3048  # convert to feet
        return mh

##-------------------------------------------------------------------------
##  Converts the lowest available wind level from m/s to knots
##-------------------------------------------------------------------------
    def calcWind(self, wind_FHAG10):
        mag = wind_FHAG10[0]  # get the wind grids
        winddir = wind_FHAG10[1]
        mag += 1.94   # convert m/s to knots
        winddir.clip(0, 359.5, winddir)
        return (mag, winddir)

##-------------------------------------------------------------------------
##  Calculates the wind at 3000 feet AGL.
##-------------------------------------------------------------------------
    def calcFreeWind(self, gh_c, wind_c, topo):
        wm = wind_c[0]
        wd = wind_c[1]
        # Make a grid that's topo + 3000 feet (914 meters)
        fatopo = topo + 914.4
        # find the points that are above the 3000 foot level
        mask = greater_equal(gh_c, fatopo)
        # initialize the grids into which the value are stored
        famag = self.newGrid(-1)
        fadir = self.newGrid(-1)
        # start at the bottom and store the first point we find that's
        # above the topo + 3000 feet level.
        for i in range(wind_c[0].shape[0]):
            m = logical_and(equal(famag, -1), mask[i])
            famag[m] = wm[i][m]
            
            m = logical_and(equal(fadir, -1), mask[i])
            fadir[m] = wd[i][m]
            
        fadir.clip(0, 359.5, fadir)  # clip the value to 0, 360
        famag *= 1.94           # convert to knots
        return (famag, fadir)          # return the tuple of grids

##-------------------------------------------------------------------------
##  Calculates the average wind vector in the mixed layer as defined
##  by the mixing height.  This function creates a mask that identifies
##  all grid points between the ground and the mixing height and calculates
##  a vector average of the wind field in that layer.
##-------------------------------------------------------------------------
    def calcTransWind(self, MixHgt, wind_c, gh_c, topo):
        nmh = MixHgt * 0.3048 # convert MixHt from feet -> meters
        u, v = self._getUV(wind_c[0], wind_c[1])  # get the wind grids
        # set a mask at points between the topo and topo + MixHt
        mask = logical_and(greater_equal(gh_c, topo),
                           less_equal(gh_c, nmh + topo))
        # set the points outside the layer to zero
        u[logical_not(mask)] = 0
        v[logical_not(mask)] = 0
        mask = add.reduce(mask).astype(float32)
        mmask = mask + 0.0001
        # calculate the average value in the mixed layerlayer
        u = where(mask, add.reduce(u) / mmask, float32(0))
        v = where(mask, add.reduce(v) / mmask, float32(0))
        # convert u, v to mag, dir
        tmag, tdir = self._getMD(u, v)
        tdir.clip(0, 359.5, tdir)
        tmag *= 1.94             # convert to knots
        tmag.clip(0, 125, tmag)  # clip speed to 125 knots
        return (tmag, tdir)

##-------------------------------------------------------------------------
## Uses a derivation of the Bourgouin allgorithm to calculate precipitation
## type, and other algorithms to determine the coverage and intensity.
## The Bourgoin technique figures out precip type from calculating how
## long a hydrometer is exposed to alternating layers of above zero (C) and
## below zero temperature layers.  This tool calculates at each grid point
## which of the four Bourgouin cases apply.  Then the appropriate algorithm
## is applied to that case that further refines the precip. type.  Once the
## type is determined, other algorithms are used to determine the coverage
## and intensity. See the Weather and Forecasting Journal article Oct. 2000,
## "A Method to Determine Precipitation Types", by Pierre Bourgouin
##-------------------------------------------------------------------------
    def calcWx(self, QPF, T, p_SFC, t_c, gh_c, topo, lgsp_SFC, cp_SFC):
        tp_SFC = cp_SFC + lgsp_SFC
        gh_c = gh_c[:13, :, :]
        t_c = t_c[:13, :, :]
        T = self.FtoK(T)
        p_SFC = p_SFC / 100  # sfc pres. in mb
        pres = self.pres
        a1 = self.empty()
        a2 = self.empty()
        a3 = self.empty()
        aindex = self.empty()
        # Go through the levels to identify each case type 0-3
        for i in range(1, gh_c.shape[0] - 1):
            # get the sfc pres. and temp.
            pbot = where(greater(gh_c[i - 1], topo), pres[i - 1], p_SFC)
            tbot = where(greater(gh_c[i - 1], topo), t_c[i - 1], T)
            # Calculate the area of this layer in Temp/pres coordinates
            a11, a22, cross = self.getAreas(pbot, tbot, pres[i], t_c[i])
            topomask = greater(gh_c[i], topo)
            
            m = logical_and(equal(aindex, 0), topomask)
            a1[m] += a11[m]
            
            m = logical_and(equal(aindex, 1), topomask)
            a2[m] += a11[m]
            
            m = logical_and(equal(aindex, 2), topomask)
            a3[m] += a11[m]
            
            topomask = logical_and(topomask, cross)
            aindex = where(topomask, aindex + 1, aindex)
            
            m = logical_and(equal(aindex, 0), topomask)
            a1[m] += a22[m]
            
            m = logical_and(equal(aindex, 1), topomask)
            a2[m] += a22[m]
            
            m = logical_and(equal(aindex, 2), topomask)
            a3[m] += a22[m]

        # Now apply a different algorithm for each type
        key = ['<NoCov>:<NoWx>:<NoInten>:<NoVis>:',
               "Wide:S:-:<NoVis>:", "Wide:R:-:<NoVis>:",
               "Wide:S:-:<NoVis>:^Wide:R:-:<NoVis>:",
               'Wide:ZR:-:<NoVis>:', 'Wide:IP:-:<NoVis>:',
               'Wide:ZR:-:<NoVis>:^Wide:IP:-:<NoVis>:',
               "Sct:SW:-:<NoVis>:", "Sct:RW:-:<NoVis>:",
               "Sct:SW:-:<NoVis>:^Sct:RW:-:<NoVis>:",
               "Chc:ZR:-:<NoVis>:", 'Chc:IP:-:<NoVis>:',
               'Chc:ZR:-:<NoVis>:^Chc:IP:-:<NoVis>:']

        wx = self.empty(int8)
        # Case d (snow)
        snowmask = equal(aindex, 0)
        wx[logical_and(snowmask, greater(a1, 0))] = 2
        wx[logical_and(snowmask, less_equal(a1, 0))] = 1

        # Case c (rain / snow / rainSnowMix)
        srmask = equal(aindex, 1)
        wx[logical_and(srmask, less(a1, 5.6))] = 1
        wx[logical_and(srmask, greater(a1, 13.2))] = 2
        wx[logical_and(srmask,
                               logical_and(greater_equal(a1, 5.6),
                                           less(a1, 13.2)))] = 3

        # Case a (Freezing Rain / Ice Pellets)
        ipmask = equal(aindex, 2)
        ipm = greater(a1, a2 * 0.66 + 66)
        wx[logical_and(ipmask, ipm)] = 5
        zrm = less(a1, a2 * 0.66 + 46)
        wx[logical_and(ipmask, zrm)] = 4
        zrm = logical_not(zrm)
        ipm = logical_not(ipm)
        wx[logical_and(ipmask, logical_and(zrm, ipm))] = 6

        # Case b (Ice pellets / rain)
        cmask = greater_equal(aindex, 3)
        ipmask = logical_and(less(a3, 2), cmask)
        wx[logical_and(ipmask, less(a1, 5.6))] = 1
        wx[logical_and(ipmask, greater(a1, 13.2))] = 2
        wx[logical_and(ipmask, logical_and(greater_equal(a1, 5.6),
                                                   less_equal(a1, 13.2)))] = 3

        ipmask = logical_and(greater_equal(a3, 2), cmask)
        wx[logical_and(ipmask, greater(a1, 66 + 0.66 * a2))] = 5
        wx[logical_and(ipmask, less(a1, 46 + 0.66 * a2))] = 4
        wx[logical_and(ipmask, logical_and(greater_equal(a1, 46 + 0.66 * a2),
                                           less_equal(a1, 66 + 0.66 * a2)))] = 6

        # Make showers (scattered/Chc)
        convecMask = greater(cp_SFC / (tp_SFC + .001), 0.5)
        wx[logical_and(not_equal(wx, 0), convecMask)] += 6

        # No wx where no qpf
        wx[less(QPF, 0.01)] = 0
        return(wx, key)

##-------------------------------------------------------------------------
## Calculates chance of wetting rain based on QPF.
##-------------------------------------------------------------------------
    def calcCWR(self, QPF):
        m1 = less(QPF, 0.01)  # all the places that are dry
        m2 = greater_equal(QPF, 0.3)  # all the places that are wet
        #  all the places that are 0.01 to 0.10
        m3 = logical_and(greater_equal(QPF, 0.01), less_equal(QPF, 0.1))
        #  all the places that are 0.1 to 0.3
        m4 = logical_and(greater(QPF, 0.1), less(QPF, 0.3))
        # assign 0 to the dry grid point, 100 to the wet grid points,
        # and a ramping function to all point in between
        cwr = where(m1, float32(0), where(m2, float32(100),
                                 where(m3, 444.4 * (QPF - 0.01) + 10,
                                       where(m4, 250 * (QPF - 0.1) + 50,
                                             QPF))))
        return cwr

##-------------------------------------------------------------------------
## Calculates Lightning Activity Level based on total precip., lifted index
## and 3-D relative humidity.
##-------------------------------------------------------------------------
    def calcLAL(self, bli_SFC, lgsp_SFC, cp_SFC, rh_c, rh_FHAG2):
        tp_SFC = cp_SFC + lgsp_SFC
        lal = self.newGrid(1)
        # Add one to lal if we have 0.5 mm of precip.
        lal[logical_and(greater(cp_SFC, 0), greater(tp_SFC / cp_SFC, 0.5))] += 1

        #  make an average rh field
        midrh = add.reduce(rh_c[6:9], 0) / 3
        # Add one to lal if mid-level rh high and low level rh low
        lal[logical_and(greater(midrh, 70), less(rh_FHAG2, 30))] += 1

        # Add on to lal if lifted index is <-3 and another if <-5
        lal[less(bli_SFC, -3)] += 1
        lal[less(bli_SFC, -5)] += 1
        return lal

def main():
    RAPForecaster().run()