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

##-------------------------------------------------------------------------
## Model that produces surface weather elements from  model
## output.
##
class HIRESWnmmForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "HIRESWnmm")
        self.oldqpf = 0.0 #initializes a temporary grid to zero

####--------------------------------------------------------------------------
#### These levels will be used to create vertical soundings.  These are
#### defined here since they are model dependent.
####--------------------------------------------------------------------------
    def levels(self):
        return ["MB1000", "MB925", "MB850", "MB700", "MB500", "MB400", "MB300"]

####-------------------------------------------------------------------------
#### Returns the maximum of the specified MaxT and the T grids
####--------------------------------------------------------------------------
    def calcMaxT(self, T, MaxT):
        if MaxT is None:
            return T
        return maximum(MaxT, T)

####-------------------------------------------------------------------------
#### Returns the minimum of the specified MinT and T grids
####--------------------------------------------------------------------------
    def calcMinT(self, T, MinT):
        if MinT is None:
            return T
        return minimum(MinT, T)


####-------------------------------------------------------------------------
#### Calculates the temperature at the elevation indicated in the topo
#### grid.  This tool simply interpolates the temperature value from
#### model's isobaric temperature cube.
####-------------------------------------------------------------------------
    def calcT(self, t_FHAG2, t_BL030, t_BL3060, t_BL6090, t_BL90120,
               t_BL120150, p_SFC, topo, stopo, gh_c, t_c):
        p_SFC = p_SFC / 100  # get the surface pres. in mb
        pres = [p_SFC, p_SFC - 15, p_SFC - 45, p_SFC - 75,
                p_SFC - 105, p_SFC - 135]
        temps = [t_FHAG2, t_BL030, t_BL3060, t_BL6090, t_BL90120,
                 t_BL120150]
        return self._calcT(temps, pres, topo, stopo, gh_c, t_c)

    def _calcT(self, temps, pres, topo, stopo, gh_c, t_c):
        p = self._minus
        tmb = self._minus
        tms = self._minus
        # go up the column to figure out the surface pressure
        for i in xrange(1, gh_c.shape[0]):
            higher = greater(gh_c[i], topo)  # identify points > topo
            # interpolate the pressure at topo height
            val = self.linear(gh_c[i], gh_c[i - 1],
                              log(self.pres[i]), log(self.pres[i - 1]), topo)
            val[greater(val, 500)] = 500
            val = clip(val, -.00001, 10)
            p = where(logical_and(equal(p, -1), higher),
                      exp(val), p)
            # interpolate the temperature at true elevation
            tval1 = self.linear(gh_c[i], gh_c[i - 1], t_c[i], t_c[i - 1], topo)
            tmb = where(logical_and(equal(tmb, -1), greater(gh_c[i], topo)),
                        tval1, tmb)
            # interpolate the temperature at model elevation
            tval2 = self.linear(gh_c[i], gh_c[i - 1], t_c[i], t_c[i - 1], stopo)
            tms = where(logical_and(equal(tms, -1), greater(gh_c[i], stopo)),
                        tval2, tms)


        # define the pres. of each of the boundary layers
        st = self._minus
        # Calculate the lapse rate in units of pressure
        for i in xrange(1, len(pres)):
            val = self.linear(pres[i], pres[i - 1], temps[i], temps[i - 1], p)
            gm = greater(pres[i - 1], p)
            lm = less_equal(pres[i], p)
            mask = logical_and(gm, lm)
            st = where(logical_and(equal(st, -1), mask),
                       val, st)            
        # where topo level is above highest level in BL fields...use tmb
        st = where(logical_and(equal(st,-1),less(p, pres[-1])), tmb, st)

        # where topo level is below model surface...use difference
        # of t at pressure of surface and tFHAG2 and subtract from tmb
        st = where(equal(st, -1), tmb - tms + temps[0], st)
        return self.KtoF(st)

####-------------------------------------------------------------------------
#### Calculates dew point from the specified pressure, temp and rh
#### fields.
####-------------------------------------------------------------------------
    def calcTd(self, p_SFC, T, t_FHAG2, stopo, topo, rh_FHAG2):
        # at the model surface
        sfce = rh_FHAG2 / 100 * self.esat(t_FHAG2) # partial pres of H2O
        w = (0.622 * sfce) / ((p_SFC + 0.0001) / 100 - sfce)# meters / millibar
        # at the true surface
        tsfce = self.esat(self.FtoK(T))    # saturation vap.pres. at sfc
        dpdz = 287.04 * t_FHAG2 / (p_SFC / 100 * 9.8) # meters / millibar
        newp = p_SFC / 100 + (stopo - topo) / dpdz  # adj. sfc press.
        ws = (0.622 * tsfce) / (newp - tsfce)  # sat. mixing ratio
        rh = w / ws  #  calc relative humidity

        # Finally, calculate the dew point
        tsfcesat = rh * tsfce
        tsfcesat = clip(tsfcesat, 0.00001, tsfcesat)
        b = 26.66082 - log(tsfcesat)
        td = (b - sqrt(b * b - 223.1986)) / 0.0182758048
        td = self.KtoF(td)
        td = where(w > ws, T, td)
        return td

####-------------------------------------------------------------------------
####  Calculates RH from the T and Td grids
####-------------------------------------------------------------------------
    def calcRH(self, T, Td):
        Tc = .556 * (T - 32.0)
        Tdc = .556 * (Td - 32.0)
        Vt = 6.11 * pow(10, (Tc * 7.5 / (Tc + 237.3)))
        Vd = 6.11 * pow(10, (Tdc * 7.5 / (Tdc + 237.3)))
        RH = (Vd / Vt) * 100.0
        # Return the new value
        return RH

####-------------------------------------------------------------------------
#### Returns the maximum of the specified MaxRH and the RH grids
####--------------------------------------------------------------------------
    def calcMaxRH(self, RH, MaxRH):
        if MaxRH is None:
            return RH
        return maximum(MaxRH, RH)

####-------------------------------------------------------------------------
#### Returns the minimum of the specified MinRH and RH grids
####--------------------------------------------------------------------------
    def calcMinRH(self, RH, MinRH):
        if MinRH is None:
            return RH
        return minimum(MinRH, RH)

####-------------------------------------------------------------------------
####  Calculates QPF from the total precip field out of the model
####-------------------------------------------------------------------------
    def calcQPF(self, tp_SFC):
        qpf = tp_SFC / 25.4   # convert from millimeters to inches
        return qpf

##--------------------------------------------------------------------------
##  Calculates the Freezing level based on height and temperature
##  cubes.  Finds the height at which freezing occurs.
##--------------------------------------------------------------------------
##    def calcFzLevel(self, gh_c, t_c, topo):
##        fzl = self._minus
##
##        # for each level in the height cube, find the freezing level
##        for i in xrange(gh_c.shape[0]):
##            try:
##                val = gh_c[i-1] + (gh_c[i] - gh_c[i-1]) / (t_c[i] - t_c[i-1])\
##                      * (273.15 - t_c[i-1])
##            except:
##                val = gh_c[i]
##            ## save the height value in fzl
##            fzl = where(logical_and(equal(fzl, -1),
##                                    less_equal(t_c[i], 273.15)), val, fzl)
##
##        return fzl * 3.28   # convert to feet

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
#        pt = where(mask, pt, 0)
        pt[logical_not(mask)] = 0
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
            tmh = self.linear(pt[i], pt[i - 1], gh_c[i], gh_c[i - 1], runavg)
            # assign new values if the difference is greater than 3
            mh = where(logical_and(logical_and(mask[i], equal(mh, -1)),
                                   greater(diffpt, 3)), tmh, mh)
        return (mh - topo) * 3.28  # convert to feet


####-------------------------------------------------------------------------
####  Converts the lowest available wind level from m/s to knots
####-------------------------------------------------------------------------
    def calcWind(self, wind_FHAG10):
        mag = wind_FHAG10[0] * 1.94  # get the wind speed and convert
        dir = wind_FHAG10[1]         # get wind dir
        return (mag, dir)           # assemble speed and dir into a tuple

####-------------------------------------------------------------------------
####  Calculates the wind at 3000 feet AGL.
####-------------------------------------------------------------------------
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
            famag = where(equal(famag, -1),
                          where(mask[i], wm[i], famag), famag)
            fadir = where(equal(fadir, -1),
                          where(mask[i], wd[i], fadir), fadir)
        fadir = clip(fadir, 0, 360)  # clip the value to 0, 360
        famag = famag * 1.94    # convert to knots
        return (famag, fadir)   # return the tuple of grids

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
        u[logical_not(mask)] = 0
        v[logical_not(mask)] = 0
        mask = add.reduce(mask) # add up the number of set points vert.
        mmask = mask + 0.0001
        # calculate the average value in the mixed layerlayer
        u = where(mask, add.reduce(u) / mmask, 0)
        v = where(mask, add.reduce(v) / mmask, 0)
        # convert u, v to mag, dir
        tmag, tdir = self._getMD(u, v)
        tdir = clip(tdir, 0, 359.5)
        tmag = tmag * 1.94  # convert to knots
        tmag = clip(tmag, 0, 125)  # clip speed to 125 knots
        return (tmag, tdir)

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
def main():
    HIRESWnmmForecaster().run()
