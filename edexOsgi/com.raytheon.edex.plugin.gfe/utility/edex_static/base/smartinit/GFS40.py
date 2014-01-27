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
## Module that calculates surface weather elements from GFS80 model
## output.
##
##--------------------------------------------------------------------------
class GFS40Forecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "GFS40", "GFS40")

####--------------------------------------------------------------------------
#### These levels will be used to create vertical soundings.  These are
#### defined here since they are model dependent.
####--------------------------------------------------------------------------
    def levels(self):
        return ["MB1000", "MB975", "MB950", "MB925", "MB900", "MB875", "MB850",
                "MB825", "MB800", "MB775", "MB750", "MB725", "MB700",
                "MB675", "MB650", "MB625", "MB600", "MB575", "MB550",
                "MB525", "MB500", "MB450", "MB400", "MB350", "MB300"]

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
               t_BL120150, t_BL150180, p_SFC, topo, stopo, gh_c, t_c):
        p_SFC = p_SFC / 100  # get the surface pres. in mb
        pres = [p_SFC, p_SFC - 15, p_SFC - 45, p_SFC - 75,
                p_SFC - 105, p_SFC - 135]
        temps = [t_FHAG2, t_BL030, t_BL3060, t_BL6090, t_BL90120,
                 t_BL120150, t_BL150180]
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

####-------------------------------------------------------------------------
####  Calculates sky (0-100)  from the total precip field out of the model
####-------------------------------------------------------------------------

    ###########################################################################
    #  Calculates Sky condition (fractional cloud cover) from model RH at 
    #  specific pressure levels.  Uses reduced equations from Walcek, MWR June 
    #  1994.  Adds up the amount of fractional clouds calculated at each layer
    #   based on topography (i.e. no clouds below ground) then divides by a 
    #  suggested number of layers to produce an average sky.
    ###########################################################################
    def calcSky(self, rh_c, gh_c, topo, p_SFC, rh_BL030, rh_BL3060, rh_BL6090,
                rh_BL90120, rh_BL120150, rh_BL150180):

        tmpP_SFC = p_SFC.copy()
        tmpP_SFC /= 100.0       # convert surfp to millibars
        x = 560.0               # delta x (85km - 850km)

        #  Define a percentage of f100 to use as a filter (0.0 - 1.0)
        #  Remember f100 is an exponential function, so changes will be more 
        #  pronounced in the 0.5-1.0 range than the 0.0-0.5 range.
        percent = 0.37

        #  Define a list of boundary layer levels to include
        BL_levels = ['BL030', 'BL3060', 'BL6090', 'BL90120', 'BL120150',
                     'BL150180']

        #  Construct a boundary layer pressure and RH cube
        bl_Pcube = []
        bl_RHcube = []

        #  Place all BL RH levels into a cube
        bl_RHcube += [rh_BL030]
        bl_RHcube += [rh_BL3060]
        bl_RHcube += [rh_BL6090]
        bl_RHcube += [rh_BL90120]
        bl_RHcube += [rh_BL120150]
        bl_RHcube += [rh_BL150180]

        bl_RHcube = array(bl_RHcube)


        #  Make a cube of boundary layer pressures 
        for lvl in BL_levels:
            if lvl == 'BL030':
                tmpP = tmpP_SFC - 15.0
            elif lvl == 'BL3060':
                tmpP = tmpP_SFC - 45.0
            elif lvl == 'BL6090':
                tmpP = tmpP_SFC - 75.0
            elif lvl == 'BL90120':
                tmpP = tmpP_SFC - 105.0
            elif lvl == 'BL120150':
                tmpP = tmpP_SFC - 135.0
            elif lvl == 'BL150180':
                tmpP = tmpP_SFC - 165.0
            bl_Pcube += [tmpP]
        bl_Pcube = array(bl_Pcube)


        # Make a model level pressure cube
        pmb = ones_like(gh_c)
        for i in xrange(gh_c.shape[0]):
            pmb[i] = self.pres[i]


        #  Convert BL pressures to sigma levels
        BL_sigma = bl_Pcube / tmpP_SFC
        del bl_Pcube
        BL_sigma = clip(BL_sigma, 0.1, 1.0)

        #  Convert model level pressure cube to sigma surfaces
        pp = pmb / tmpP_SFC
        del tmpP_SFC
        pp = clip(pp, 0.1, 1.0)


        #  Account for topography in the model cube, don't need to worry about
        #  this with the BL cube since those are guaranteed to be above ground
        tmpRH_c = where(less(gh_c, topo), 0.0, rh_c)

        #=======================================================================
        #  Create new RH and sigma cubes

        newRH_c = []
        newSigma_c = []

        #  See which boundary layer levels have pressures > lowest "signficant"
        #  model level pressure
        for bl_i in xrange(BL_sigma.shape[0]):

            #  Make a mask to identify which points from the boundary
            #  layer level have greater pressure than lowest "significant"
            #  model level
            BL_mask = greater(BL_sigma[bl_i], pp[0])

            #  See how many points we've found
            count = sum(sum(BL_mask, 1))

            #  If there are no points - don't include this BL level
            if count == 0:
                continue

            #  Compute a temporary RH grid where it is lower than the lowest 
            #  "significant" model level data
            tmpRH = where(BL_mask, bl_RHcube[bl_i], 0.0)


            #  Compute a temporary sigma grid for this boundary layer level
            #  where it is lower than the lowest "significant" model level
            tmpSigma = where(BL_mask, BL_sigma[bl_i], 0.0)

            #  Insert this level into the new RH and sigma cubes
            newRH_c += [tmpRH]
            newSigma_c += [tmpSigma]


        #  Add lowest "significant" model level to RH and sigma cubes
        newRH_c += [tmpRH_c[0]]
        newSigma_c += [pp[0]]


        #  Insert boundary layer RH into RH cube where appropriate
        for lvl in xrange(1, len(self.levels())):

            #  Look at each boundary layer level
            for bl_i in xrange(BL_sigma.shape[0]):

                #  Make a mask to identify which points from the boundary
                #  layer level fall between the surrounding "significant"
                #  model levels
                BL_mask = logical_and(greater(BL_sigma[bl_i], pp[lvl]),
                                      less(BL_sigma[bl_i], pp[lvl - 1]))

                #  See how many points we've found
                count = sum(sum(BL_mask, 1))

                #  If there are no points - don't include this BL level
                if count == 0:
                    continue

                #  Compute a temporary RH grid where it is between the two 
                #  "significant" model level data
                tmpRH = where(BL_mask, bl_RHcube[bl_i], 0.0)


                #  Compute a temporary sigma grid for this boundary layer level
                #  where it is between the two "significant" model levels
                tmpSigma = where(BL_mask, BL_sigma[bl_i], 0.0)

                #  Insert this level into the new RH and sigma cubes
                newRH_c += [tmpRH]
                newSigma_c += [tmpSigma]


            #  Add top of layer we just examined to RH and sigma cube
            newRH_c += [tmpRH_c[lvl]]
            newSigma_c += [pp[lvl]]

        del bl_RHcube
        del BL_sigma
        del tmpRH_c
        
        #  Finish off the new cubes
        newRH_c = array(newRH_c)
        newSigma_c = array(newSigma_c)

        #  Determine maximum possible sky fraction 
        fmax = 78.0 + x / 15.5

        #  Compute sky fraction for both pressure cubes 
        f100 = where(less(newSigma_c, 0.7),
                     fmax * (newSigma_c - 0.1) / 0.6,
                     30.0 + (1.0 - newSigma_c) * (fmax - 30.0) / 0.3)

        #  Compute RH depression at 37% f100 [ (1-RHe) in Walcek ]              
        c = 0.196 + (0.76 - x / 2834.0) * (1.0 - newSigma_c)
        
        del newSigma_c

        #  Compute critical RH threshold to use as a filter
        #  Note (percent * f100)/f100 = percent
        try:
            rhCrit = log(percent) * c + 1.0
        except:
            rhCrit = 0.0

        #  Ensure "critical RH" is valid
        rhCrit = clip(rhCrit, 0.0, 1.0)

        #  Compute sky fraction for the model cube
        c = (newRH_c / 100.0 - 1.0) / c
        c = exp(c)
        f = minimum(f100 * c, 100.0)

        #  Where RH is less than the critical value, set it to 0 contribution
        f[less(newRH_c / 100.0, rhCrit)] = 0.0
        
        del newRH_c

        #  Compress cubes vertically
        f = self.squishZ(f, (f.shape[0] / 5) - 1)        #  was 5

        #  Convert sky fractions to an actual percentage
        f[4] *= 0.25
        f /= 100.0

        sky = f[0]
        for i in xrange(1, f.shape[0]):
            sky = sky + f[i] - sky * f[i]

        grid = sky * 100.0

        return grid

####-------------------------------------------------------------------------
####  Calculates Prob. of Precip. based on QPF and RH cube.  Where there
####  is QPF > 0 ramp the PoP from (0.01, 35%) to 100%.  Then in areas
####  of QPF < 0.2 raise the PoP if it's very humid.
####-------------------------------------------------------------------------
    def calcPoP(self, gh_c, rh_c, QPF, topo):
        rhavg = where(less(gh_c, topo), -1, rh_c)
#        rhavg = where(greater(gh_c, topo + (5000 * 12 * 2.54) / 100),
#                      -1, rhavg)
        rhavg[greater(gh_c, topo + (5000 * 12 * 2.54) / 100)] = -1
        count = where(not_equal(rhavg, -1), 1, 0)
#        rhavg = where(equal(rhavg, -1), 0, rhavg)
        rhavg[equal(rhavg, -1)] = 0
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

####-------------------------------------------------------------------------
####  Calculates the Freezing level based on height and temperature
####  cubes.  Finds the height at which freezing occurs.
####-------------------------------------------------------------------------
    def calcFzLevel(self, gh_FRZ):
        return gh_FRZ * 3.28

####-------------------------------------------------------------------------
####  Calculates the Snow level based on wet-bulb zero height.
####-------------------------------------------------------------------------
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

####-------------------------------------------------------------------------
####  Calculates Snow amount based on the Temp, Freezing level, QPF,
####  topo and Weather grid
####-------------------------------------------------------------------------
#    def calcSnowAmt(self, T, FzLevel, QPF, topo, Wx):
#        # figure out the snow to liquid ratio
#        m1 = less(T, 9)
#        m2 = greater_equal(T, 30)
#        snowr = T * -0.5 + 22.5
#        snowr = where(m1, 20, snowr)
#        snowr = where(m2, 0, snowr)
#        # calc. snow amount based on the QPF and the ratio
#        snowamt = where(less_equal(FzLevel - 1000, topo * 3.28),
#                        snowr * QPF, 0)
#        # Only make snow at points where the weather is snow
#        snowmask = logical_or(equal(Wx[0], 1), equal(Wx[0], 3))
#        snowmask = logical_or(snowmask, logical_or(equal(Wx[0], 7),
#                                                   equal(Wx[0], 9)))
#        snowamt = where(snowmask, snowamt, 0)
#        return snowamt

###########################################################
## GRR Snow Init - begin
## This Routine Does SnowAmt - Not SnowRatio
###########################################################
    def calcSnowAmt(self, T, QPF, gh_c, t_c, rh_c, pvv_c, gh_MB925, gh_MB800, gh_MB850, gh_MB700, gh_MB750, gh_MB650, gh_MB600, gh_MB550):

        #t_c is tCube, rh_c is rhCube, etc.
        #we do not want the lowest 4 levels in the cubes
        gh_c = gh_c[4:, :, :]
        t_c = t_c[4:, :, :]
        rh_c = rh_c[4:, :, :]
        pvv_c = pvv_c[4:, :, :]


        print "Got", len(t_c), "t grids and", len(rh_c), "rh grids"

        # Some thresholds used throughout the tool
        dryRH = 75.0  # dry atm below this value
        lrMin = 10.0  # lapse rate minimum
        lrMax = 6.5  # laspe rate maximum
        lrMaxAdj = 0.3 # max lapse rate adjustment value

        # extract the shapes and make some variables
        #cubeShape = (len(t_c) - 1, t_c.shape[1], t_c.shape[2])
        cubeShape = (len(t_c), t_c.shape[1], t_c.shape[2])
        gridShape = (t_c.shape[1], t_c.shape[2])
        layerSR = zeros(cubeShape, dtype = float)
        pvvAvg = zeros(cubeShape, dtype = float)
        pvvSum = zeros(gridShape, dtype = float)

        #print "cubeShape = ", cubeShape

        for i in range(len(gh_c) - 1):
        #for i in range(len(gh_c)):
            #print "processing layer", gh_c[i]
            # calculate the average temp and rh in the layer
            avgTemp = t_c[i] - 273.15 # Convert to C
            avgRH = rh_c[i]

            # get the base snowRatio based on the avgTemp
            layerSR[i] = self.baseSnowRatio(avgTemp)

            # adjust snowRatio based on lapseRate
            #lr = -(t_c[i+1] - t_c[i])
            #lrAdj = where(greater_equal(lr,6.5), 1.0 + ((lr - lrMin) / (lrMax - lrMin)) * lrMaxAdj, 1.0)
            #layerSR[i] = layerSR[i] * lrAdj

            # Calc avg pressure vertical velocity, scale based on RH and sum
            # reverse the pvvAvg sign so up is positive
            pvvAvg[i] = -10 * (pvv_c[i])
            # clip downward vertical velocities
            pvvAvg[i] = where(less(pvvAvg[i], 0.0), 0.0, pvvAvg[i])
            # Scale vertical velocity as a function of the square of RH.
            # This scaling will efectively negate a snowratio contribution in
            # layers that are dry.
            pvvAvg[i] = where(less(avgRH, 80.0), pvvAvg[i] * ((avgRH * avgRH) / 6400.0), pvvAvg[i])
            pvvSum = pvvSum + pvvAvg[i]

        # Normalize the layerSnowRatio based on the pvv fraction of the total
        totalSnowRatio = zeros(gridShape, dtype = float)
        #tweak the pvvSum grid to avoid division by zero
        pvvSum = where(less_equal(pvvSum, 0.0), .0001, pvvSum)
        for i in range(len(layerSR)):
            srGrid = layerSR[i] * pvvAvg[i] / pvvSum
            totalSnowRatio = totalSnowRatio + srGrid

        # Finally clip the snowRatio to zero under two conditions
        # cube where min colum temp > -8.0C and rh > 75%
        # This is basically Baumgardt - Top Down Approach - No ice No dice!
        mask = logical_and(less(t_c, 265.15), greater_equal(rh_c, 50.0))
        mask = sum(mask)  # reduce to single level by adding bits verically
        totalSnowRatio = where(equal(mask, 0), 0.0, totalSnowRatio)

        thicknessSnowRatio = zeros(gridShape, dtype = float)

#########################################################
#  Pick an applicable thickness scheme for your area

        myThickness = "850-700"
        #myThickness = "925-700"
        #myThickness = "850-650"
        #myThickness = "800-600"
        #myThickness = "750-550"

##########################################################

        if myThickness == "850-700":
            thicknessSnowRatio = 20.0 - pow(((gh_MB700 - gh_MB850) - 1437.0) / 29.0 , 2)
        elif myThickness == "925-700":
            thicknessSnowRatio = 20.0 - pow(((gh_MB700 - gh_MB925) - 2063.0) / 41.0 , 2)
        elif myThickness == "850-650":
            thicknessSnowRatio = 20.0 - pow(((gh_MB650 - gh_MB850) - 1986.0) / 39.0 , 2)
        elif myThickness == "800-600":
            thicknessSnowRatio = 20.0 - pow(((gh_MB600 - gh_MB800) - 2130.0) / 42.0 , 2)
        else:  # "750-500"  
            thicknessSnowRatio = 20.0 - pow(((gh_MB550 - gh_MB750) - 2296.0) / 45.0 , 2)

        thicknessSnowRatio = where(less(thicknessSnowRatio, 0.0), 0.0, thicknessSnowRatio)

        totalSnowRatio = (totalSnowRatio * 0.50) + (thicknessSnowRatio * 0.50)
        totalSnowRatio = where(less_equal(pvvSum, 100.0), (totalSnowRatio * 0.01 * pvvSum) + (thicknessSnowRatio * (1.0 - pvvSum * 0.01)), totalSnowRatio)
        totalSnowRatio = where(less(pvvSum, 1.0), thicknessSnowRatio, totalSnowRatio)

        # If there's any layer above 0.0C, snowRatio gets 0
        mask = greater(t_c, 272.65)
        mask = sum(mask) # reduce to single level by adding bits vertically
        # if mask == 0, nowhere in the column is temp < 0.5C
        totalSnowRatio = where(equal(mask, 0), totalSnowRatio, 0.0)
        
        #Calculate Snowfall - taper to zero from 31 to 34 F.
        snowfall = QPF * totalSnowRatio
        snowfall = where(greater(T, 31.0), pow(35.0 - T, 2) / 16.0 * snowfall , snowfall)

        snowfall = where(greater(T, 35.0), 0.0 , snowfall)

        # Return the new value
        return snowfall


    ### Given a grid of temperature in Celcius, this method computes
    ### the base snowRatio based on the spline curve as defined by the
    ### coefficients.
    def baseSnowRatio(self, tGrid):
        #  set up the spline coefficients
        tThresh = [-30.0, -21.0, -18.0, -15.0, -12.0, -10.0, -8.0, -5.0, -3.0, 2.0]
        a = [9.0, 21.0, 31.0, 35.0, 26.0, 15.0, 9.0, 5.0, 4.0]
        b = [0.4441, 3.1119, 2.8870, -0.6599, -5.2475, -4.5685, -1.9786, -0.7544, -0.3329]
        c = [0.0, 0.2964, -0.3714, -0.8109, -0.7183, 1.0578, 0.2372, 0.1709, 0.0399]
        d = [0.0110, -0.0742, -0.0488, 0.0103, 0.2960, -0.1368, -0.0074, -0.0218, -0.0027]

        # Initialize the coeficient grids
        aGrid = zeros(tGrid.shape) + a[-1]   #last value in list
        bGrid = zeros(tGrid.shape) + b[-1]
        cGrid = zeros(tGrid.shape) + c[-1]
        dGrid = zeros(tGrid.shape) + d[-1]
        tDiff = zeros(tGrid.shape, dtype = float)

        # define grids of coefficients based on tGrid
        for i in range(len(tThresh) - 1):
            mask1 = greater_equal(tGrid, tThresh[i])
            mask2 = less(tGrid, tThresh[i + 1])
            mask = logical_and(mask1, mask2) # area b/w threshold
            tDiff = where(mask, tGrid - tThresh[i], tDiff)
            aGrid = where(mask, a[i], aGrid)
            bGrid = where(mask, b[i], bGrid)
            cGrid = where(mask, c[i], cGrid)
            dGrid = where(mask, d[i], dGrid)

        # Do the calcuation using the grids of spline coefficients
        baseRatio = aGrid + bGrid * tDiff + cGrid * tDiff * tDiff \
                    + dGrid * pow(tDiff, 3)

        # Clip the snowRatio grid to 10.0 where tGrid is outside limits
        #baseRatio = where(greater(tGrid, 1.0), 0.0, baseRatio)
        #baseRatio = where(less(tGrid, tThresh[0]), 10.0, baseRatio)

        return baseRatio


###############################################################################
##  END-- GRR Snow Init
###############################################################################

###########################################################
## GRR SnowRatio Init - begin
## This routine does SnowRatio - Not SnowAmt!
###########################################################
    def calcSnowRatio(self, gh_c, t_c, rh_c, pvv_c, gh_MB925, gh_MB800, gh_MB850, gh_MB750, gh_MB700, gh_MB650, gh_MB600, gh_MB550):

        #t_c is tCube, rh_c is rhCube, etc.
        #we do not want the lowest 4 levels in the cubes
        gh_c = gh_c[4:, :, :]
        t_c = t_c[4:, :, :]
        rh_c = rh_c[4:, :, :]
        pvv_c = pvv_c[4:, :, :]


        print "Got", len(t_c), "t grids and", len(rh_c), "rh grids"

        # Some thresholds used throughout the tool
        dryRH = 75.0  # dry atm below this value
        lrMin = 10.0  # lapse rate minimum
        lrMax = 6.5  # laspe rate maximum
        lrMaxAdj = 0.3 # max lapse rate adjustment value

        # extract the shapes and make some variables
        #cubeShape = (len(t_c) - 1, t_c.shape[1], t_c.shape[2])
        cubeShape = (len(t_c), t_c.shape[1], t_c.shape[2])
        gridShape = (t_c.shape[1], t_c.shape[2])
        layerSR = zeros(cubeShape, dtype = float)
        pvvAvg = zeros(cubeShape, dtype = float)
        pvvSum = zeros(gridShape, dtype = float)

        #print "cubeShape = ", cubeShape

        for i in range(len(gh_c) - 1):
        #for i in range(len(gh_c)):
            #print "processing layer", gh_c[i]
            # calculate the average temp and rh in the layer
            avgTemp = t_c[i] - 273.15 # Convert to C
            avgRH = rh_c[i]

            # get the base snowRatio based on the avgTemp
            layerSR[i] = self.baseSnowRatio(avgTemp)

            # adjust snowRatio based on lapseRate
            #lr = -(t_c[i+1] - t_c[i])
            #lrAdj = where(greater_equal(lr,6.5), 1.0 + ((lr - lrMin) / (lrMax - lrMin)) * lrMaxAdj, 1.0)
            #layerSR[i] = layerSR[i] * lrAdj

            # Calc avg pressure vertical velocity, scale based on RH and sum
            # reverse the pvvAvg sign so up is positive
            pvvAvg[i] = -10 * (pvv_c[i])
            # clip downward vertical velocities
            pvvAvg[i] = where(less(pvvAvg[i], 0.0), 0.0, pvvAvg[i])
            # Scale vertical velocity as a function of the square of RH.
            # This scaling will efectively negate a snowratio contribution in
            # layers that are dry.
            pvvAvg[i] = where(less(avgRH, 80.0), pvvAvg[i] * ((avgRH * avgRH) / 6400.0), pvvAvg[i])
            pvvSum = pvvSum + pvvAvg[i]

        # Normalize the layerSnowRatio based on the pvv fraction of the total
        totalSnowRatio = zeros(gridShape, dtype = float)
        #tweak the pvvSum grid to avoid division by zero
        pvvSum = where(less_equal(pvvSum, 0.0), .0001, pvvSum)

        for i in range(len(layerSR)):
            srGrid = layerSR[i] * pvvAvg[i] / pvvSum
            totalSnowRatio = totalSnowRatio + srGrid

        # Finally clip the snowRatio to zero under two conditions
        # cube where min colum temp > -8.0C and rh > 75%
        # This is basically Baumgardt - Top Down Approach - No ice No dice!
        mask = logical_and(less(t_c, 265.15), greater_equal(rh_c, 50.0))
        mask = sum(mask)  # reduce to single level by adding bits verically
        totalSnowRatio = where(equal(mask, 0), 0.0, totalSnowRatio)

        thicknessSnowRatio = zeros(gridShape, dtype=float)

#########################################################
#  Pick an applicable thickness scheme for your area

        myThickness = "850-700"
        #myThickness = "925-700"
        #myThickness = "850-650"
        #myThickness = "800-600"
        #myThickness = "750-550"

##########################################################

        if myThickness == "850-700":
            thicknessSnowRatio = 20.0 - pow(((gh_MB700 - gh_MB850) - 1437.0) / 29.0 , 2)
        elif myThickness == "925-700":
            thicknessSnowRatio = 20.0 - pow(((gh_MB700 - gh_MB925) - 2063.0) / 41.0 , 2)
        elif myThickness == "850-650":
            thicknessSnowRatio = 20.0 - pow(((gh_MB650 - gh_MB850) - 1986.0) / 39.0 , 2)
        elif myThickness == "800-600":
            thicknessSnowRatio = 20.0 - pow(((gh_MB600 - gh_MB800) - 2130.0) / 42.0 , 2)
        else:  # "750-500"  
            thicknessSnowRatio = 20.0 - pow(((gh_MB550 - gh_MB750) - 2296.0) / 45.0 , 2)



        thicknessSnowRatio = where(less(thicknessSnowRatio, 0.0), 0.0, thicknessSnowRatio)
        totalSnowRatio = (totalSnowRatio * 0.50) + (thicknessSnowRatio * 0.50)
        totalSnowRatio = where(less_equal(pvvSum, 100.0), (totalSnowRatio * 0.01 * pvvSum) + (thicknessSnowRatio * (1.0 - pvvSum * 0.01)), totalSnowRatio)
        totalSnowRatio = where(less(pvvSum, 1.0), thicknessSnowRatio, totalSnowRatio)

        # If there's any layer above 0.0C, snowRatio gets 0
        mask = greater(t_c, 272.65)
        mask = sum(mask) # reduce to single level by adding bits vertically
        # if mask == 0, nowhere in the column is temp < 0.5C
        totalSnowRatio = where(equal(mask, 0), totalSnowRatio, 0.0)  

        # Return the new value
        return totalSnowRatio


    ### Given a grid of temperature in Celcius, this method computes
    ### the base snowRatio based on the spline curve as defined by the
    ### coefficients.
    def baseSnowRatio(self, tGrid):
        #  set up the spline coefficients
        tThresh = [-30.0, -21.0, -18.0, -15.0, -12.0, -10.0, -8.0, -5.0, -3.0, 2.0]
        a = [9.0, 21.0, 31.0, 35.0, 26.0, 15.0, 9.0, 5.0, 4.0]
        b = [0.4441, 3.1119, 2.8870, -0.6599, -5.2475, -4.5685, -1.9786, -0.7544, -0.3329]
        c = [0.0, 0.2964, -0.3714, -0.8109, -0.7183, 1.0578, 0.2372, 0.1709, 0.0399]
        d = [0.0110, -0.0742, -0.0488, 0.0103, 0.2960, -0.1368, -0.0074, -0.0218, -0.0027]

        # Initialize the coeficient grids
        aGrid = zeros(tGrid.shape) + a[-1]   #last value in list
        bGrid = zeros(tGrid.shape) + b[-1]
        cGrid = zeros(tGrid.shape) + c[-1]
        dGrid = zeros(tGrid.shape) + d[-1]
        tDiff = zeros(tGrid.shape, dtype = float)

        # define grids of coefficients based on tGrid
        for i in range(len(tThresh) - 1):
            mask1 = greater_equal(tGrid, tThresh[i])
            mask2 = less(tGrid, tThresh[i + 1])
            mask = logical_and(mask1, mask2) # area b/w threshold
            tDiff = where(mask, tGrid - tThresh[i], tDiff)
            aGrid = where(mask, a[i], aGrid)
            bGrid = where(mask, b[i], bGrid)
            cGrid = where(mask, c[i], cGrid)
            dGrid = where(mask, d[i], dGrid)

        # Do the calcuation using the grids of spline coefficients
        baseRatio = aGrid + bGrid * tDiff + cGrid * tDiff * tDiff \
                    + dGrid * pow(tDiff, 3)

        # Clip the snowRatio grid to 10.0 where tGrid is outside limits
        #baseRatio = where(greater(tGrid, 1.0), 0.0, baseRatio)
        #baseRatio = where(less(tGrid, tThresh[0]), 10.0, baseRatio)

        return baseRatio


###############################################################################
##  END-- GRR SnowRatio Init
###############################################################################
####--------------------------------------------------------------------------
####  Calculate the Haines index based on the temp and RH cubes
####  Define self.whichHainesIndex to be "HIGH", "MEDIUM", or "LOW".
####  Default is "HIGH".
####--------------------------------------------------------------------------
    def calcHaines(self, t_c, rh_c):
        return self.hainesIndex(self.whichHainesIndex, t_c, rh_c)


####-------------------------------------------------------------------------
####  Calculates the mixing height for the given sfc temperature,
####  temperature cube, height cube and topo
####-------------------------------------------------------------------------
    def calcMixHgt(self, T, topo, t_c, gh_c):
        mask = greater_equal(gh_c, topo) # points where height > topo
        pt = []
        for i in xrange(len(self.pres)):   # for each pres. level
            p = self._empty + self.pres[i] # get the pres. value in mb
            tmp = self.ptemp(t_c[i], p)    # calculate the pot. temp
            pt = pt + [tmp]                # add to the list
        pt = array(pt)
        # set up masks
#        pt = where(mask, pt, 0)
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

####-------------------------------------------------------------------------
####  Calculates the average wind vector in the mixed layer as defined
####  by the mixing height.  This function creates a mask that identifies
####  all grid points between the ground and the mixing height and calculates
####  a vector average of the wind field in that layer.
####-------------------------------------------------------------------------
    def calcTransWind(self, MixHgt, wind_c, gh_c, topo):
        nmh = MixHgt * 0.3048  # convert MixHt from feet -> meters
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
        tmag = tmag * 1.94   # convert to knots
        tmag = clip(tmag, 0, 125)  # clip speed to 125 knots
        return (tmag, tdir)

####-------------------------------------------------------------------------
#### Uses a derivation of the Bourgouin allgorithm to calculate precipitation
#### type, and other algorithms to determine the coverage and intensity.
#### The Bourgoin technique figures out precip type from calculating how
#### long a hydrometer is exposed to alternating layers of above zero (C) and
#### below zero temperature layers.  This tool calculates at each grid point
#### which of the four Bourgouin cases apply.  Then the appropriate algorithm
#### is applied to that case that further refines the precip. type.  Once the
#### type is determined, other algorithms are used to determine the coverage
#### and intensity. See the Weather and Forecasting Journal article Oct. 2000,
#### "A Method to Determine Precipitation Types", by Pierre Bourgouin
####-------------------------------------------------------------------------
    def calcWx(self, QPF, T, t_c, gh_c, p_SFC, topo, sli_SFC):
        gh_c = gh_c[:13, :, :]
        t_c = t_c[:13, :, :]
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
            pbot = where(greater(gh_c[i - 1], topo), pres[i - 1], p_SFC)
            tbot = where(greater(gh_c[i - 1], topo), t_c[i - 1], T)
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

        wx = zeros(self._empty.shape, dtype = byte)
        # Case d (snow)
        snowmask = equal(aindex, 0)
#        wx = where(logical_and(snowmask, greater(a1, 0)), 2, wx)
#        wx = where(logical_and(snowmask, less_equal(a1, 0)), 1, wx)
        wx[logical_and(snowmask, greater(a1, 0))] = 2
        wx[logical_and(snowmask, less_equal(a1, 0))] = 1

        # Case c (rain / snow / rainSnowMix)
        srmask = equal(aindex, 1)
#        wx = where(logical_and(srmask, less(a1, 5.6)), 1, wx)
#        wx = where(logical_and(srmask, greater(a1, 13.2)), 2, wx)
#        wx = where(logical_and(srmask,
#                               logical_and(greater_equal(a1, 5.6),
#                                           less(a1, 13.2))), 3, wx)
        wx[logical_and(srmask, less(a1, 5.6))] = 1
        wx[logical_and(srmask, greater(a1, 13.2))] = 2
        wx[logical_and(srmask,
                               logical_and(greater_equal(a1, 5.6),
                                           less(a1, 13.2)))] = 3


        # Case a (Freezing Rain / Ice Pellets)
        ipmask = equal(aindex, 2)
        ipm = greater(a1, a2 * 0.66 + 66)
#        wx = where(logical_and(ipmask, ipm), 5, wx)
        wx[logical_and(ipmask, ipm)] = 5
        zrm = less(a1, a2 * 0.66 + 46)
#        wx = where(logical_and(ipmask, zrm), 4, wx)
        wx[logical_and(ipmask, zrm)] = 4
        zrm = logical_not(zrm)
        ipm = logical_not(ipm)
#        wx = where(logical_and(ipmask, logical_and(zrm, ipm)), 6, wx)
        wx[logical_and(ipmask, logical_and(zrm, ipm))] = 6

        # Case b (Ice pellets / rain)
        cmask = greater_equal(aindex, 3)
        ipmask = logical_and(less(a3, 2), cmask)
#        wx = where(logical_and(ipmask, less(a1, 5.6)), 1, wx)
#        wx = where(logical_and(ipmask, greater(a1, 13.2)), 2, wx)
#        wx = where(logical_and(ipmask, logical_and(greater_equal(a1, 5.6),
#                                                   less_equal(a1, 13.2))),
#                   3, wx)
        wx[logical_and(ipmask, less(a1, 5.6))] = 1
        wx[logical_and(ipmask, greater(a1, 13.2))] = 2
        wx[logical_and(ipmask,
                               logical_and(greater_equal(a1, 5.6),
                                           less(a1, 13.2)))] = 3

        ipmask = logical_and(greater_equal(a3, 2), cmask)
#        wx = where(logical_and(ipmask, greater(a1, 66 + 0.66 * a2)), 5, wx)
#        wx = where(logical_and(ipmask, less(a1, 46 + 0.66 * a2)), 4, wx)
#        wx = where(logical_and(ipmask,
#                               logical_and(greater_equal(a1, 46 + 0.66 * a2),
#                                           less_equal(a1, 66 + 0.66 * a2))),
#                   6, wx)
        wx[logical_and(ipmask, greater(a1, 66 + 0.66 * a2))] = 5
        wx[logical_and(ipmask, less(a1, 46 + 0.66 * a2))] = 4
        wx[logical_and(ipmask, logical_and(greater_equal(a1, 5.6),
                                                   less_equal(a1, 13.2)))] = 6

        # Make showers (scattered/Chc)
#         convecMask = greater(cp_SFC / (tp_SFC + .001), 0.5)
#         wx = where(logical_and(not_equal(wx, 0), convecMask), wx + 6, wx)

        # Thunder
        for i in xrange(len(key)):
            tcov = string.split(key[i], ":")[0]
            if tcov == "Chc" or tcov == "<NoCov>":
                tcov = "Sct"
            key.append(key[i] + "^" + tcov
                       + ":T:<NoInten>:<NoVis>:")
        wx = where(less_equal(sli_SFC, -3), wx + 13, wx)

        # No wx where no qpf
#        wx = where(less(QPF, 0.01), 0, wx)
        wx[less(QPF, 0.01)] = 0

        return(wx, key)

####-------------------------------------------------------------------------
#### Calculates chance of wetting rain based on QPF.
####-------------------------------------------------------------------------
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

####-------------------------------------------------------------------------
#### Calculates Lightning Activity Level based on total precip., lifted index
#### and 3-D relative humidity.
####-------------------------------------------------------------------------
    def calcLAL(self, tp_SFC, sli_SFC, rh_c, rh_BL030):
        bli = sli_SFC  # surface lifted index
        ttp = self._empty + 0.00001   # nearly zero grid
        lal = self._empty + 1         # initialize the return grid to 1
        # Add one to lal if QPF > 0.5
        lal = where(logical_and(logical_and(greater(tp_SFC, 0),
                                            greater(ttp, 0)),
                                greater(tp_SFC / ttp, 0.5)), lal + 1, lal)
        #  make an average rh field
        midrh = add.reduce(rh_c[6:9], 0) / 3
        # Add one to lal if mid-level rh high and low level rh low
        lal = where(logical_and(greater(midrh, 70), less(rh_BL030, 30)),
                    lal + 1, lal)

        # Add on to lal if lifted index is <-3 and another if <-5
        lal = where(less(bli, -3), lal + 1, lal)
        lal = where(less(bli, -5), lal + 1, lal)

        return lal


def main():
    GFS40Forecaster().run()
