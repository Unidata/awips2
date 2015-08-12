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
## Module that calculates surface weather elements from GFS75 model
## output.
##
##--------------------------------------------------------------------------
class GFS75Forecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "GFS75")

##--------------------------------------------------------------------------
## These levels will be used to create vertical soundings.  These are
## defined here since they are model dependent.
##--------------------------------------------------------------------------
    def levels(self):
        return ["MB1000", "MB950", "MB900", "MB850",
                "MB800", "MB750", "MB700",
                "MB650", "MB600", "MB500", "MB450", "MB400", "MB350",
                "MB300"]

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
    def calcT(self, t_FHAG2, stopo, topo):
#    Temperature drops by .0074 C/meter per Hans' computations
        elevationDiff = topo - stopo # in m.
        tcorr = elevationDiff * 0.00714 #K
        return self.KtoF(t_FHAG2 - tcorr)

##-------------------------------------------------------------------------
## Calculates dew point from the specified pressure, temp and rh
## fields.
##-------------------------------------------------------------------------
    def calcTd(self, gh_c, t_c, rh_BL030, rh_MB850, rh_MB800, rh_MB750, rh_MB700, rh_MB650,
               t_MB1000, t_MB900, t_MB850, t_MB800, t_MB750, t_MB700, t_MB650, topo):

        tmb = self._minus

        #calc sfc_temp at topo
        for i in xrange(1, gh_c.shape[0]):

            #interpolate temp in this layer
            tval1 = self.linear(gh_c[i], gh_c[i - 1], t_c[i], t_c[i - 1], topo)
            tmb = where(logical_and(equal(tmb, -1), greater(gh_c[i], topo)), tval1, tmb)

        temp = self.KtoF(tmb)

        rh = rh_BL030
        rh = where(logical_and(greater_equal(topo, 1327),less(topo, 1828)),rh_MB850, rh)
        rh = where(logical_and(greater_equal(topo, 1828),less(topo, 2438)),rh_MB800, rh)
        rh = where(logical_and(greater_equal(topo, 2438),less(topo, 3048)),rh_MB750, rh)
        rh = where(logical_and(greater_equal(topo, 3048),less(topo, 3657)),rh_MB700, rh)
        rh = where(logical_and(greater_equal(topo, 3657),less(topo, 4267)),rh_MB650, rh)

        rh_linear = self.linear(0, 4400, 1.0, 1.001, topo)
        rh = rh * rh_linear
        rh = clip(rh, 3.0, 100.0)


        rh = rh + .01
        Tc = .556 * (temp - 32.0)

        ret = .9 * Tc
        ret1 = 112 + ret
        ret2 = ret1 * (rh ** .125) / 1.7783
        Tdc = ret2 - 112 + .1 * Tc
        Td = (1.8 * Tdc) + 32

        return Td

##-------------------------------------------------------------------------
##  Calculates RH from the T and Td grids
##-------------------------------------------------------------------------
    def calcRH(self, rh_BL030, rh_MB850, rh_MB800, rh_MB750, rh_MB700, rh_MB650, topo):
        rh = rh_BL030

        rh = where(logical_and(greater_equal(topo, 1327),less(topo, 1828)),rh_MB850, rh)
        rh = where(logical_and(greater_equal(topo, 1828),less(topo, 2438)),rh_MB800, rh)
        rh = where(logical_and(greater_equal(topo, 2438),less(topo, 3048)),rh_MB750, rh)        
        rh = where(logical_and(greater_equal(topo, 3048),less(topo, 3657)),rh_MB700, rh)
        rh = where(logical_and(greater_equal(topo, 3657),less(topo, 4267)),rh_MB650, rh)

        rh_linear = self.linear(0, 4400, 1.0, 1.001, topo)

        rh = rh * rh_linear
        rh = clip(rh, 3.0, 100.0)

        return rh

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
    def calcQPF(self, tp_SFC):
        qpf = tp_SFC / 25.4   # convert from millimeters to inches
        return qpf

    def calcSky(self, rh_c, topo):
        rh_c = rh_c[:8, :, :]
        rh_c[less_equal(rh_c, 25.0)] = 25.0
        rh_c[less_equal(rh_c, 100.0)] = 100.0

        rh900 = rh_c[2]
        rh850 = rh_c[3]
        rh800 = rh_c[4]
        rh750 = rh_c[5]
        rh700 = rh_c[6]

        index900 = self.linear(25, 100, 0, 100, rh900)
        index850 = self.linear(25, 100, 0, 100, rh850)
        index800 = self.linear(25, 100, 0, 100, rh800)
        index750 = self.linear(25, 100, 0, 125, rh750)
        index700 = self.linear(25, 100, 0, 150, rh700)

        skyindex = index900 + index850 + index800 + index750 + index700
        skyindex = clip(skyindex, 0, 500)

        sky = self.linear(0, 500, 0, 100, skyindex)

        return sky

##-------------------------------------------------------------------------
##  Calculates Prob. of Precip. based on QPF and RH cube.  Where there
##  is QPF > 0 ramp the PoP from (0.01, 35%) to 100%.  Then in areas
##  of QPF < 0.2 raise the PoP if it's very humid.
##-------------------------------------------------------------------------
    def calcPoP(self, rh_c, pvv_MB700, topo):
        # use only the first 11 levels (up to 500MB)
        rh_c = rh_c[:8, :, :]
        rh_c[less_equal(rh_c, 43.0)] = 43.0
        rh_c[greater_equal(rh_c, 100.0)] = 100.0

        rh900 = rh_c[2]
        rh850 = rh_c[3]
        rh800 = rh_c[4]
        rh750 = rh_c[5]
        rh700 = rh_c[6]
        omega700 = pvv_MB700

        popindex900 = self.linear(43, 100, 0, 60, rh900)
        popindex850 = self.linear(43, 100, 0, 70, rh850)
        popindex800 = self.linear(43, 100, 0, 80, rh800)
        popindex750 = self.linear(43, 100, 0, 90, rh750)
        popindex700 = self.linear(43, 100, 0, 100, rh700)

        omegacorpos = self.linear(0, -2, 0, 50, omega700)
        omegacorneg = self.linear(0, 1, 0, -50, omega700)

        popindex = popindex900 + popindex850 + popindex800 + popindex750 + popindex700 + omegacorpos + omegacorneg

        popindex = clip(popindex, 0, 500)

        pop = self.linear(0, 500, 0, 100, popindex)

        return pop

##-------------------------------------------------------------------------
##  Calculates the Freezing level based on height and temperature
##  cubes.  Finds the height at which freezing occurs.
##-------------------------------------------------------------------------
    def calcFzLevel(self, gh_c, t_c, topo):
        fzl = self._minus

        # for each level in the height cube, find the freezing level
        for i in xrange(gh_c.shape[0]):
            try:
                val = gh_c[i - 1] + (gh_c[i] - gh_c[i - 1]) / (t_c[i] - t_c[i - 1])\
                      * (273.15 - t_c[i - 1])
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
        for i in xrange(t_c.shape[0] - 1, -1, -1):
            if maximum.reduce(maximum.reduce(t_c[i])) >= 273.15:
                clipindex = i + 1
                break
        gh_c = gh_c[:clipindex, :, :]
        t_c = t_c[:clipindex, :, :]
        rh_c = rh_c[:clipindex, :, :]

        snow = self._minus
        #
        #  make pressure cube
        #
        pmb = ones(gh_c.shape)
        for i in xrange(gh_c.shape[0]):
           pmb[i] = self.pres[i]
        pmb = clip(pmb, 1, 1050)
        #
        #  convert temps to C and limit to reasonable values
        #
        tc = t_c - 273.15
        tc = clip(tc, -120, 60)
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
        for i in xrange(1, gh_c.shape[0]):
           try:
              val = gh_c[i - 1] + (gh_c[i] - gh_c[i - 1]) / (wetb[i] - wetb[i - 1])\
                 * (-wetb[i - 1])
           except:
              val = gh_c[i]
           snow = where(logical_and(equal(snow, -1), less_equal(wetb[i], 0)),
                      val, snow)
        #
        #  convert to feet
        #
        snow = snow * 3.28

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
        snowamt = where(less_equal(FzLevel - 1000, topo * 3.28),
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
    def calcMixHgt(self, T, topo, t_c, gh_c):
        mask = greater_equal(gh_c, topo) # points where height > topo
        pt = []
        for i in xrange(len(self.pres)):   # for each pres. level
            p = self._empty + self.pres[i] # get the pres. value in mb
            tmp = self.ptemp(t_c[i], p)    # calculate the pot. temp
            pt = pt + [tmp]                # add to the list
        pt = array(pt)
        # set up masks
        pt[logical_not(mask)] = 0

        avg = add.accumulate(pt, 0)
        count = add.accumulate(mask, 0)
        mh = self._minus
        # for each pres. level, calculate a running avg. of pot temp.
        # As soon as the next point deviates from the running avg by
        # more than 3 deg. C, interpolate to get the mixing height.
        for i in xrange(1, avg.shape[0]):
            runavg = avg[i] / (count[i] + .0001) # calc. running avg
            diffpt = pt[i] - runavg  # calc. difference
            # calc. the interpolated mixing height
            tmh = self.linear(pt[i], pt[i - 1], gh_c[i], gh_c[i - 1], runavg)
            # assign new values if the difference is greater than 3
            mh = where(logical_and(logical_and(mask[i], equal(mh, -1)),
                                   greater(diffpt, 3)), tmh, mh)
        return (mh - topo) * 3.28  # convert to feet

##-------------------------------------------------------------------------
##  Converts the lowest available wind level from m/s to knots
##-------------------------------------------------------------------------
    def calcWind(self, wind_FHAG10, wind_MB850, wind_MB800, wind_MB750, wind_MB700, wind_MB650, topo):
        mag = wind_FHAG10[0] * 1.94  # get the wind speed and convert
        dir = wind_FHAG10[1]         # get the wind direction

        mag = where(logical_and(greater_equal(topo, 1327), less(topo, 1828)), wind_MB850[0] * 1.94, mag)
        dir = where(logical_and(greater_equal(topo, 1327), less(topo, 1828)), wind_MB850[1], dir)

        mag = where(logical_and(greater_equal(topo, 1828), less(topo, 2438)), wind_MB800[0] * 1.94, mag)
        dir = where(logical_and(greater_equal(topo, 1828), less(topo, 2438)), wind_MB800[1], dir)

        mag = where(logical_and(greater_equal(topo, 2438), less(topo, 3048)), wind_MB750[0] * 1.94, mag)
        dir = where(logical_and(greater_equal(topo, 2438), less(topo, 3048)), wind_MB750[1], dir)

        mag = where(logical_and(greater_equal(topo, 3048), less(topo, 3657)), wind_MB700[0] * 1.94, mag)
        dir = where(logical_and(greater_equal(topo, 3048), less(topo, 3657)), wind_MB700[1], dir)

        mag = where(logical_and(greater_equal(topo, 3657), less(topo, 4267)), wind_MB650[0] * 1.94, mag)
        dir = where(logical_and(greater_equal(topo, 3657), less(topo, 4267)), wind_MB650[1], dir)

        mag_cor_up = self.linear(1327, 4267, 1, 1.3, topo)

        mag_cor_lo = self.linear(1327, 50, 1, 0.7, topo)

        mag = where(logical_and(greater_equal(topo, 1327),less(topo, 4267)), mag*mag_cor_up, mag)
        mag = where(logical_and(greater_equal(topo, 50),less(topo, 1327)), mag*mag_cor_lo, mag)

        dir = clip(dir, 0, 359.5)

        return (mag, dir)

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
        famag = self._minus
        fadir = self._minus
        # start at the bottom and store the first point we find that's
        # above the topo + 3000 feet level.
        for i in xrange(wind_c[0].shape[0]):
            famag = where(logical_and(equal(famag, -1), mask[i]), wm[i], famag)
            fadir = where(logical_and(equal(fadir, -1), mask[i]), wd[i], fadir)
        fadir = clip(fadir, 0, 360)  # clip the value to 0, 360
        famag = famag * 1.94    # convert to knots
        return (famag, fadir)   # return the tuple of grids

##-------------------------------------------------------------------------
##  Calculates the average wind vector in the mixed layer as defined
##  by the mixing height.  This function creates a mask that identifies
##  all grid points between the ground and the mixing height and calculates
##  a vector average of the wind field in that layer.
##-------------------------------------------------------------------------
    def calcTransWind(self, MixHgt, wind_c, gh_c, topo):
        nmh = MixHgt * 0.3048  # convert MixHt from feet -> meters
        u, v = self._getUV(wind_c[0], wind_c[1])  # get the wind grids
        # set a mask at points between the topo and topo + MixHt
        mask = logical_and(greater_equal(gh_c, topo),
                           less_equal(gh_c, nmh + topo))
        # set the points outside the layer to zero
        if type(u) is ndarray:
            u[logical_not(mask)] = 0
        else:
            u = where(mask, u, float32(0))
        if type(v) is ndarray:
            v[logical_not(mask)] = 0
        else:
            v = where(mask, v, float32(0))
        mask = add.reduce(mask) # add up the number of set points vert.
        mmask = mask + 0.0001
        # calculate the average value in the mixed layerlayer
        u = where(mask, add.reduce(u) / mmask, float32(0))
        v = where(mask, add.reduce(v) / mmask, float32(0))
        # convert u, v to mag, dir
        tmag, tdir = self._getMD(u, v)
        tmag = tmag * 1.94   # convert to knots
        tmag = clip(tmag, 0, 125)  # clip speed to 125 knots
        return (tmag, tdir)


    def calcWx(self, t_MB850, t_MB700, t_MB500, rh_MB850, rh_MB700, cape_SFC, PoP):
        # first calculate K-index to establish heavy rain threat
        t850 = t_MB850 - 273.2
        t700 = t_MB700 - 273.2
        t500 = t_MB500 - 273.2
        rh850 = rh_MB850
        rh700 = rh_MB700

        ratio1 = ((log10(rh850 / 100.0) / 7.5) + (t850 / (t850 + 237.3)))
        td850 = ((237.3 * ratio1) / (1.0 - ratio1))
        ratio2 = ((log10(rh700 / 100.0) / 7.5) + (t700 / (t700 + 237.3)))
        td700 = ((237.3 * ratio2) / (1.0 - ratio2))
        kindex = ((t850 - t500) + td850 - (t700 - td700))

        # now on to the weather

        key = ['<NoCov>:<NoWx>:<NoInten>:<NoVis>:',
               "Iso:RW:m:<NoVis>:",
               "Sct:RW:m:<NoVis>:",
               "Lkly:RW:m:<NoVis>:",
               "Ocnl:RW:m:<NoVis>:",
               "Iso:RW:+:<NoVis>:",
               "Sct:RW:+:<NoVis>:",
               "Lkly:RW:+:<NoVis>:",
               "Ocnl:RW:+:<NoVis>:"]

        wx = zeros(self._empty.shape, dtype=int8)
        wx[less_equal(PoP, 14.4)] = 0

        hvymask = greater_equal(kindex, 35)
        wx[logical_and(hvymask, logical_and(greater(PoP, 14.4), less(PoP, 24.4)))] = 5
        wx[logical_and(hvymask, logical_and(greater(PoP, 24.4), less(PoP, 54.4)))] = 6
        wx[logical_and(hvymask, logical_and(greater(PoP, 54.4), less(PoP, 74.4)))] = 7
        wx[logical_and(hvymask, greater(PoP, 74.4))] = 8

        lgtmask = less(kindex, 35)
        wx[logical_and(lgtmask, logical_and(greater(PoP, 14.4), less(PoP, 24.4)))] = 1
        wx[logical_and(lgtmask, logical_and(greater(PoP, 24.4), less(PoP, 54.4)))] = 2
        wx[logical_and(lgtmask, logical_and(greater(PoP, 54.4), less(PoP, 74.4)))] = 3
        wx[logical_and(lgtmask, greater(PoP, 74.4))] = 4

        # Thunder
        for i in xrange(len(key)):
            tcov = string.split(key[i], ":")[0]
            if tcov == "<NoCov>":
                tcov = "Iso"
            key.append(key[i] + "^" + tcov + ":T:<NoInten>:<NoVis>:")
        wx[logical_and(greater(PoP, 14.4), greater_equal(cape_SFC, 1000))] += 9

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
    def calcLAL(self, tp_SFC, sli_SFC, rh_c, rh_BL030):
        bli = sli_SFC  # surface lifted index
        ttp = full_like(self._empty, 0.00001)   # nearly zero grid
        lal = ones_like(self._empty)  # initialize the return grid to 1
        # Add one to lal if QPF > 0.5
        lal[logical_and(greater(ttp, 0), greater(tp_SFC / ttp, 0.5))] += 1

        #  make an average rh field
        midrh = add.reduce(rh_c[6:9], 0) / 3
        # Add one to lal if mid-level rh high and low level rh low
        lal[logical_and(greater(midrh, 70), less(rh_BL030, 30))] += 1

        # Add on to lal if lifted index is <-3 and another if <-5
        lal[less(bli, -3)] += 1
        lal[less(bli, -5)] += 1

        return lal

def main():
    GFS75Forecaster().run()
