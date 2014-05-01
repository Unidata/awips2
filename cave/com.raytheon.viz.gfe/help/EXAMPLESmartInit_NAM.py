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
## Module that calculates surface weather elements from NAM model
## output.
##
##--------------------------------------------------------------------------
class NAM12Forecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "NAM12", "NAM12")

##--------------------------------------------------------------------------
## These levels will be used to create vertical soundings.  These are
## defined here since they are model dependent.
##--------------------------------------------------------------------------
    def levels(self):
        return ["MB1000", "MB950", "MB900","MB850","MB800","MB750",
                "MB700","MB650","MB600","MB550", "MB500",
                "MB450", "MB400", "MB350"]

##--------------------------------------------------------------------------
## Returns the maximum of the specified MaxT and the T grids
##--------------------------------------------------------------------------
    def calcMaxT(self, T, MaxT):
        if MaxT is None:
            return T
        return maximum(MaxT, T)

##--------------------------------------------------------------------------
## Returns the minimum of the specified MinT and T grids
##--------------------------------------------------------------------------
    def calcMinT(self, T, MinT):
        if MinT is None:
            return T
        return minimum(MinT, T)

##--------------------------------------------------------------------------
## Calculates the temperature at the elevation indicated in the topo
## grid.  This tool uses the model's boundary layers to calculate a lapse
## rate and then applies that lapse rate to the difference between the
## model topography and the true topography.  This algorithm calculates
## the surface temperature for three different sets of points: those that
## fall above the boundary layer, in the boundary layer, and below the
## boundary layer.
##--------------------------------------------------------------------------
    def calcT(self, t_FHAG2, t_BL030, t_BL3060, t_BL6090, t_BL90120,
              t_BL12015, p_SFC, topo, stopo, gh_c, t_c):
        p = self._minus
        tmb = self._minus
        tms = self._minus
        # go up the column to figure out the surface pressure
        for i in xrange(1, gh_c.shape[0]):
            higher = greater(gh_c[i], topo)
            # interpolate the pressure at topo height
            val = self.linear(gh_c[i], gh_c[i-1],
                              log(self.pres[i]), log(self.pres[i-1]), topo)
            val = clip(val, -.00001, 10)
            p = where(logical_and(equal(p, -1), higher),
                      exp(val), p)
            # interpolate the temperature at true elevation
            tval1 = self.linear(gh_c[i], gh_c[i-1], t_c[i], t_c[i-1], topo)
            tmb = where(logical_and(equal(tmb, -1), greater(gh_c[i], topo)),
                        tval1, tmb)
            # interpolate the temperature at model elevation
            tval2 = self.linear(gh_c[i], gh_c[i-1], t_c[i], t_c[i-1], stopo)
            tms = where(logical_and(equal(tms, -1), greater(gh_c[i], stopo)),
                        tval2, tms)

        p_SFC = p_SFC / 100  # get te surface pres. in mb
        # define the pres. of each of the boundary layers
        pres = [p_SFC, p_SFC - 15, p_SFC - 45, p_SFC - 75, p_SFC - 105,
                p_SFC - 135]
        # list of temperature grids
        temps = [t_FHAG2, t_BL030, t_BL3060, t_BL6090, t_BL90120, t_BL12015]
        st = self._minus
        # Calculate the lapse rate in units of pressure
        for i in xrange(1, len(pres)):
            val = self.linear(pres[i], pres[i-1], temps[i], temps[i-1], p)
            gm = greater(pres[i-1], p)
            lm = less_equal(pres[i], p)
            mask = logical_and(gm, lm)
            st = where(logical_and(equal(st, -1), mask),
                       val, st)

        # where topo level is above highest level in BL fields...use tmb
        st = where(logical_and(equal(st,-1),less(p,p_SFC-135)),tmb,st)

        # where topo level is below model surface...use difference
        # of t at pressure of surface and tFHAG2 and subtract from tmb
        st = where(equal(st, -1), tmb - tms + t_FHAG2, st)
        return self.KtoF(st)

##--------------------------------------------------------------------------
## Calculates dew point from the specified pressure, temp and rh
## fields.
##--------------------------------------------------------------------------
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
        tsfcesat = clip(tsfcesat, 0.00001, tsfcesat)
        b = 26.66082 - log(tsfcesat)
        td = (b - sqrt(b * b - 223.1986)) / 0.0182758048
        td = self.KtoF(td)
        td = where(w > ws, T, td)
        return td

##-------------------------------------------------------------------------
##  Calculates RH from the T and Td grids
##-------------------------------------------------------------------------
    def calcRH(self, T, Td):
        Tc = .556 * (T - 32.0)
        Tdc = .556 * (Td - 32.0)
        Vt = 6.11 * pow(10,(Tc * 7.5 / (Tc + 237.3)))
        Vd = 6.11 * pow(10,(Tdc * 7.5 / (Tdc + 237.3)))
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

##--------------------------------------------------------------------------
## Calculates QPF from the total precip field out of the model
##--------------------------------------------------------------------------
    def calcQPF(self, tp_SFC):
        qpf = tp_SFC / 25.4   # convert from millimeters to inches
        return qpf

    def calcSky(self, rh_c, gh_c, topo, p_SFC):
        return self.skyFromRH(rh_c, gh_c, topo, p_SFC)

##--------------------------------------------------------------------------
##  Calculates Prob. of Precip. based on QPF and RH cube.  Where there
##  is QPF > 0 ramp the PoP from (0.01, 35%) to 100%.  Then in areas
##  of QPF < 0.2 raise the PoP if it's very humid.
##--------------------------------------------------------------------------
    def calcPoP(self, gh_c, rh_c, QPF, topo):
        rhavg = where(less(gh_c, topo), -1, rh_c)
        rhavg = where(greater(gh_c, topo + (5000 * 12 * 2.54) / 100),
                      -1, rhavg)
        count = where(not_equal(rhavg, -1), 1, 0)
        rhavg = where(equal(rhavg, -1), 0, rhavg)
        count = add.reduce(count, 0)
        rhavg = add.reduce(rhavg, 0)
        ## add this much based on humidity only
        dpop = where(count, rhavg / (count + .001), 0) - 70.0
        dpop = where(less(dpop, -30), -30, dpop)
        ## calculate the base PoP
        pop = where(less(QPF, 0.02), QPF * 1000, QPF * 350 + 13)
        pop = pop + dpop   # add the adjustment based on humidity
        pop = clip(pop, 0, 100)  # clip to 100%
        return pop

##--------------------------------------------------------------------------
##  Calculates the Freezing level based on height and temperature
##  cubes.  Finds the height at which freezing occurs.
##--------------------------------------------------------------------------
    def calcFzLevel(self, gh_c, t_c, topo):
        fzl = self._minus
        # for each level in the height cube, find the freezing level
        for i in xrange(gh_c.shape[0]):
            try:
                val = gh_c[i-1] + (gh_c[i] - gh_c[i-1]) / (t_c[i] - t_c[i-1])\
                      * (273.15 - t_c[i-1])
            except:
                val = gh_c[i]
            ## save the height value in fzl
            fzl = where(logical_and(equal(fzl, -1),
                                    less_equal(t_c[i], 273.15)), val, fzl)

        return fzl * 3.28   # convert to feet

##-------------------------------------------------------------------------
##  Calculates the Snow level based on wet-bulb zero height.
##-------------------------------------------------------------------------
    def calcSnowLevel(self, gh_c, t_c, rh_c):
        # Only use the levels that are >= freezind (plus one level)
        # This is a performance and memory optimization
        clipindex = 2
        for i in xrange(t_c.shape[0]-1, -1, -1):
            if maximum.reduce(maximum.reduce(t_c[i])) >= 273.15:
                clipindex = i + 1
                break
        gh_c = gh_c[:clipindex,:,:]
        t_c = t_c[:clipindex,:,:]
        rh_c = rh_c[:clipindex,:,:]

        snow=self._minus
        #
        #  make pressure cube
        #
        pmb=ones(gh_c.shape)
        for i in xrange(gh_c.shape[0]):
           pmb[i]=self.pres[i]
        pmb=clip(pmb,1,1050)
        #
        #  convert temps to C and limit to reasonable values
        #
        tc=t_c-273.15
        tc=clip(tc,-120,60)
        #
        #  limit RH to reasonable values
        #
        rh=clip(rh_c,0.5,99.5)
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
        wetb=self.Wetbulb(tc,rh,pmb)
        tc = rh = pmb = None
        #
        #  find the zero level
        #
        for i in xrange(1, gh_c.shape[0]):
           try:
              val=gh_c[i-1]+(gh_c[i]-gh_c[i-1])/(wetb[i]-wetb[i-1])\
                 *(-wetb[i-1])
           except:
              val=gh_c[i]
           snow=where(logical_and(equal(snow,-1),less_equal(wetb[i],0)),
                      val,snow)
        #
        #  convert to feet
        #
        snow=snow*3.28

        return snow

##--------------------------------------------------------------------------
##  Calculates Snow amount based on the Temp, Freezing level, QPF,
##  topo and Weather grid
##--------------------------------------------------------------------------
    def calcSnowAmt(self, T, FzLevel, QPF, topo, Wx):
        # figure out the snow to liquid ratio
        m1 = less(T, 9)
        m2 = greater_equal(T, 30)
        snowr = T * -0.5 + 22.5
        snowr = where(m1, 20, snowr)
        snowr = where(m2, 0, snowr)
        # calc. snow amount based on the QPF and the ratio
        snowamt = where(less_equal(FzLevel - 1000, topo * 3.28),
                        snowr * QPF, 0)
        # Only make snow at points where the weather is snow
        snowmask = logical_or(equal(Wx[0], 1), equal(Wx[0], 3))
        snowmask = logical_or(snowmask, logical_or(equal(Wx[0], 7),
                                                   equal(Wx[0], 9)))
        snowamt = where(snowmask, snowamt, 0)
        return snowamt

##--------------------------------------------------------------------------
##  Calculate the Haines index based on the temp and RH cubes
##  Define self.whichHainesIndex to be "HIGH", "MEDIUM", or "LOW".
##  Default is "HIGH".
##--------------------------------------------------------------------------
    def calcHaines(self, t_c, rh_c):
        return self.hainesIndex(self.whichHainesIndex, t_c, rh_c)

##--------------------------------------------------------------------------
##  Calculates the mixing height for the given sfc temperature,
##  temperature cube, height cube and topo
##--------------------------------------------------------------------------
    def calcMixHgt(self, T, topo, t_c, gh_c):
        mask = greater_equal(gh_c, topo) # points where height > topo
        pt = []
        for i in xrange(len(self.pres)):   # for each pres. level
            p = self._empty + self.pres[i] # get the pres. value in mb
            tmp = self.ptemp(t_c[i], p)    # calculate the pot. temp
            pt = pt + [tmp]                # add to the list
        pt = array(pt)
        pt = where(mask, pt, 0)
        avg = add.accumulate(pt, 0)
        count = add.accumulate(mask, 0)
        mh = self._minus
        # for each pres. level, calculate a running avg. of pot temp.
        # As soon as the next point deviates from the running avg by
        # more than 3 deg. C, interpolate to get the mixing height.
        for i in xrange(1, avg.shape[0]):
            runavg = avg[i] / (count[i] + .0001)
            diffpt = pt[i] - runavg
            # calc. the interpolated mixing height
            tmh = self.linear(pt[i], pt[i-1], gh_c[i], gh_c[i-1], runavg)
            # assign new values if the difference is greater than 3
            mh = where(logical_and(logical_and(mask[i], equal(mh, -1)),
                                   greater(diffpt, 3)), tmh, mh)
        return (mh - topo) * 3.28

##--------------------------------------------------------------------------
##  Converts the lowest available wind level from m/s to knots
##--------------------------------------------------------------------------
    def calcWind(self, wind_FHAG10):
        mag = wind_FHAG10[0]  # get the wind grids
        dir = wind_FHAG10[1]  # get wind dir
        mag = mag * 1.94    # convert to knots
        dir = clip(dir, 0, 359.5)
        return (mag, dir)      # assemble speed and dir into a tuple

##--------------------------------------------------------------------------
##  Calculates the wind at 3000 feet AGL.
##--------------------------------------------------------------------------
    def calcFreeWind(self, gh_c, wind_c, topo):
        wm = wind_c[0]
        wd = wind_c[1]
        # Make a grid that's topo + 3000 feet (914 meters)
        fatopo = topo + 914.4 # 3000 feet
        # find the points that are above the 3000 foot level
        mask = greater_equal(gh_c, fatopo)
        # initialize the grids into which the value are stored
        famag = self._minus
        fadir = self._minus
        # start at the bottom and store the first point we find that's
        # above the topo + 3000 feet level.
        for i in xrange(wind_c[0].shape[0]):
            # Interpolate (maybe)
            famag = where(equal(famag, -1),
                          where(mask[i], wm[i], famag), famag)
            fadir = where(equal(fadir, -1),
                          where(mask[i], wd[i], fadir), fadir)
        fadir = clip(fadir, 0, 359.5)  # clip the value to 0, 360
        famag = famag * 1.94           # convert to knots
        return (famag, fadir)          # return the tuple of grids

##--------------------------------------------------------------------------
##  Calculates the average wind vector in the mixed layer as defined
##  by the mixing height.  This function creates a mask that identifies
##  all grid points between the ground and the mixing height and calculates
##  a vector average of the wind field in that layer.
##--------------------------------------------------------------------------
    def calcTransWind(self, MixHgt, wind_c, gh_c, topo):
        nmh = MixHgt * 0.3048 # convert MixHt from feet -> meters
        u, v = self._getUV(wind_c[0], wind_c[1])  # get the wind grids
        # set a mask at points between the topo and topo + MixHt
        mask = logical_and(greater_equal(gh_c, topo),
                           less_equal(gh_c, nmh + topo))
        # set the points outside the layer to zero
        u = where(mask, u, 0)
        v = where(mask, v, 0)
        mask = add.reduce(mask) # add up the number of set points vert.
        mmask = mask + 0.00001
        # calculate the average value in the mixed layerlayer
        u = where(mask, add.reduce(u) / mmask, 0)
        v = where(mask, add.reduce(v) / mmask, 0)
        # convert u, v to mag, dir
        tmag, tdir = self._getMD(u, v)
        tdir = clip(tdir, 0, 359.5)
        tmag = tmag  * 1.94  # convert to knots
        tmag = clip(tmag, 0, 125)  # clip speed to 125 knots
        return (tmag, tdir)


##--------------------------------------------------------------------------
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
##--------------------------------------------------------------------------
    def calcWx(self, QPF, T, p_SFC, t_c, gh_c, topo, tp_SFC, cp_SFC,
               bli_BL0180):
        gh_c = gh_c[:13,:,:]
        t_c = t_c[:13,:,:]
        T = self.FtoK(T)
        p_SFC = p_SFC / 100  # sfc pres. in mb
        pres = self.pres
        a1 = zeros(topo.shape)
        a2 = zeros(topo.shape)
        a3 = zeros(topo.shape)
        aindex = zeros(topo.shape)
        # Go through the levels to identify each case type 0-3
        for i in xrange(1, gh_c.shape[0] - 1):
            # get the sfc pres. and temp.
            pbot = where(greater(gh_c[i-1], topo), pres[i-1], p_SFC)
            tbot = where(greater(gh_c[i-1], topo), t_c[i-1], T)
            # Calculate the area of this layer in Temp/pres coordinates
            a11, a22, cross = self.getAreas(pbot, tbot, pres[i], t_c[i])
            topomask = greater(gh_c[i], topo)
            a1 = where(logical_and(equal(aindex, 0), topomask),
                       a1 + a11, a1)
            a2 = where(logical_and(equal(aindex, 1), topomask),
                       a2 + a11, a2)
            a3 = where(logical_and(equal(aindex, 2), topomask),
                       a3 + a11, a3)
            topomask = logical_and(topomask, cross)
            aindex = where(topomask, aindex + 1, aindex)
            a1 = where(logical_and(equal(aindex, 0), topomask),
                       a1 + a22, a1)
            a2 = where(logical_and(equal(aindex, 1), topomask),
                       a2 + a22, a2)
            a3 = where(logical_and(equal(aindex, 2), topomask),
                       a3 + a22, a3)

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

        wx = self._empty
        # Case d (snow)
        snowmask = equal(aindex, 0)
        wx = where(logical_and(snowmask, greater(a1, 0)), 2, wx)
        wx = where(logical_and(snowmask, less_equal(a1, 0)), 1, wx)

        # Case c (rain / snow / rainSnowMix)
        srmask = equal(aindex, 1)
        wx = where(logical_and(srmask, less(a1, 5.6)), 1, wx)
        wx = where(logical_and(srmask, greater(a1, 13.2)), 2, wx)
        wx = where(logical_and(srmask,
                               logical_and(greater_equal(a1, 5.6),
                                           less(a1, 13.2))), 3, wx)


        # Case a (Freezing Rain / Ice Pellets)
        ipmask = equal(aindex, 2)
        ipm = greater(a1, a2 * 0.66 + 66)
        wx = where(logical_and(ipmask, ipm), 5, wx)
        zrm = less(a1, a2 * 0.66 + 46)
        wx = where(logical_and(ipmask, zrm), 4, wx)
        zrm = logical_not(zrm)
        ipm = logical_not(ipm)
        wx = where(logical_and(ipmask, logical_and(zrm, ipm)), 6, wx)

        # Case b (Ice pellets / rain)
        cmask = greater_equal(aindex, 3)
        ipmask = logical_and(less(a3, 2), cmask)
        wx = where(logical_and(ipmask, less(a1, 5.6)), 1, wx)
        wx = where(logical_and(ipmask, greater(a1, 13.2)), 2, wx)
        wx = where(logical_and(ipmask, logical_and(greater_equal(a1, 5.6),
                                                   less_equal(a1, 13.2))),
                   3, wx)

        ipmask = logical_and(greater_equal(a3, 2), cmask)
        wx = where(logical_and(ipmask, greater(a1, 66 + 0.66 * a2)), 5, wx)
        wx = where(logical_and(ipmask, less(a1, 46 + 0.66 * a2)), 4, wx)
        wx = where(logical_and(ipmask,
                               logical_and(greater_equal(a1, 46 + 0.66 * a2),
                                           less_equal(a1, 66 + 0.66 * a2))),
                   6, wx)

        # Make showers (scattered/Chc)
        convecMask = greater(cp_SFC / (tp_SFC + .001), 0.5)
        wx = where(logical_and(not_equal(wx, 0), convecMask), wx + 6, wx)

        # Thunder
        for i in xrange(len(key)):
            tcov = string.split(key[i], ":")[0]
            if tcov == "Chc" or tcov == "<NoCov>":
                tcov = "Sct"
            key.append(key[i] + "^" + tcov
                       + ":T:<NoInten>:<NoVis>:")
        wx = where(less_equal(bli_BL0180, -3), wx + 13, wx)

        # No wx where no qpf
        wx = where(less(QPF, 0.01), 0, wx)
        return(wx, key)

##--------------------------------------------------------------------------
## Calculates chance of wetting rain based on QPF.
##--------------------------------------------------------------------------
    def calcCWR(self, QPF):
        m1 = less(QPF, 0.01)  # all the places that are dry
        m2 = greater_equal(QPF, 0.3)  # all the places that are wet
        #  all the places that are 0.01 to 0.10
        m3 = logical_and(greater_equal(QPF, 0.01), less_equal(QPF, 0.1))
        #  all the places that are 0.1 to 0.3
        m4 = logical_and(greater(QPF, 0.1), less(QPF, 0.3))
        # assign 0 to the dry grid point, 100 to the wet grid points,
        # and a ramping function to all point in between
        cwr = where(m1, 0, where(m2, 100,
                                 where(m3, 444.4 * (QPF - 0.01) + 10,
                                       where(m4, 250 * (QPF - 0.1) + 50,
                                             QPF))))
        return cwr

##--------------------------------------------------------------------------
## Calculates Lightning Activity Level based on total precip., lifted index
## and 3-D relative humidity.
##--------------------------------------------------------------------------
    def calcLAL(self, bli_BL0180, tp_SFC, cp_SFC, rh_c, rh_FHAG2):
        lal = self._empty + 1
        # Add one to lal if we have 0.5 mm of precip.
        lal = where(logical_and(logical_and(greater(tp_SFC, 0),
                                            greater(cp_SFC, 0)),
                                greater(tp_SFC / cp_SFC, 0.5)), lal + 1, lal)

        #  make an average rh field
        midrh = add.reduce(rh_c[6:9], 0) / 3
        # Add one to lal if mid-level rh high and low level rh low
        lal = where(logical_and(greater(midrh, 70), less(rh_FHAG2, 30)),
                    lal + 1, lal)

        # Add on to lal if lifted index is <-3 and another if <-5
        lal = where(less(bli_BL0180, -3), lal + 1, lal)
        lal = where(less(bli_BL0180, -5), lal + 1, lal)
        return lal


def main():
    NAM12Forecaster().run()

