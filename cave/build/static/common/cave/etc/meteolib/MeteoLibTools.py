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

#
# Python ported from original C and Fortran meteolib functions.
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/11/08                      mnash       Initial Creation.
#    01/09/08                      njensen     Added more methods and comments,
#                                                fixed bugs.
#    
# 

import MeteoLibConstants as meteo
import time
import math
import Forecast
from numpy import *

class MeteoLibTools():
    def __init__(self):
        pass
        
    def interp1(self, y1, y3, x1, x2, x3):
        if x3 == x1:
            x1 = x1 - 0.01
        val = y1 + ((y3 - y1) * ((x2 - x1) / (x3 - x1)))
        return val
    
    # Calculates virtual temperature as a function of temperature, dewpoint,
    # and pressure
    def virttemp(self, temp, dew, press):
        kelvinTemp = temp
        kelvinDew = dew
        
        if kelvinTemp < 100:
            kelvinTemp = kelvinTemp + 273.15
        
        if kelvinDew < 100:
            kelvinDew = kelvinDew + 273.15 
                
        if kelvinTemp < 0.0 or kelvinTemp > 373.15 :
            virttemp = meteo.FLAG
            return virttemp
        
        if kelvinDew < 173.15:
            virttemp = kelvinTemp
            return virttemp
        
        e = self.esat(kelvinDew)
        w = (0.622 * e) / (press - e)
        virttemp = kelvinTemp * (1 + (0.6 * w))
        
        return virttemp
    
    # specific humidity
    def spechum2(self, pressures, dewpoints):
        """ Routine to calculate saturation specific humidity from the dewpoint
            and pressure. """
        ni = pressures.shape[1]
        nj = pressures.shape[0]
        specHumid = zeros(pressures.shape, 'float32')
        for i in range(ni):
            for j in range(nj):                
                if math.ceil(pressures[j][i]) > meteo.FLG or math.ceil(dewpoints[j][i]) > meteo.FLG:
                    specHumid[j][i] = meteo.FLAG
                else:                    
                    k = dewpoints[j][i]                    
                    if k < 80.0:                        
                        k = k + meteo.KELVIN_ZERO
                        eee = exp(33.09376 - 0.0091379024 * k - 6106.396 / k)
                        specHumid[j][i] = eee / (pressures[j][i] - 0.00060771703 * eee)
        return specHumid
    
    # Determine the convective gust potential
    def gusts(self, p, t, td):
        gstpot = meteo.TOP_FLG
        gstsave = 0
        np = len(p)
        
        # Make sure calculation is possible.
        if p[0] < 700 or p[np - 1] > 300:
            return gstpot
        
        # Compute the 700 mb dew point depression.
        t700 = self.pvalue(700.0, p, t)
        td700 = self.pvalue(700, p, td)
        dd700 = t700 - td700
        
        # Loop from 470 to 530 mb every 5 mb and choose the largest convective
        # gust potential.
        for i in range(470, 530, 5) :
            plift = i
            t500 = self.pvalue(plift, p, t)
            td500 = self.pvalue(plift, p, td)
            dd500 = t500 - td500
            plev1 = plift - 100
            plev2 = plift - 200     
                    
            # Compute the LCL of the 500 mb parcel.
            tcb, pcb, ier = self.tplcl(t500, td500, plift)
        
            # Compute the dry and moist adiabats through the LCL of the 500 mb parcel.
            thdpar, eptpar = self.deftrk(tcb, pcb)
        
            # Compute the parcel temperature at 400 mb.
            if pcb < plev1 :
                tp400 = thdpar * math.pow((plev1 / 1000.0), 0.286)
            else :
                etpar = eptpar * math.pow((plev1 / 1000.0), 0.286)
                tp400 = self.temp_of_te(etpar, plev1)
        
            # Compute the environmental temperature at 400 mb.
            t400 = self.pvalue(plev2, p, t)
            
            # Compute the parcel temperature at 300 mb.
            if pcb < plev2 :
                tp300 = thdpar * math.pow((plev2 / 1000.0), 0.286)
            else :
                etpar = eptpar * math.pow((plev2 / 1000.0), 0.286)
                tp300 = self.temp_of_te(etpar, plev2)
                    
            # Compute the environmental temperature at 300 mb.
            t300 = self.pvalue(plev2, p, t)
        
            # Compute the ui index.
            ui = (t400 - tp400) + (t300 - tp300)
        
            g = self.cvgust(dd700, ui)
        
            # Save the largest value.
            if g > gstsave :
                gstpot = g
        
            gstsave = g
        return gstpot
    
    # Computes the pressure and height of the freezing level in a sounding
    def frzlev(self, height, pressures, heights, temperatures):
        frzlev = zeros((1, 2), 'float32')
        nlvls = len(pressures[0]) - 1
        if temperatures[0][0] > meteo.KELVIN_ZERO :
            for i in range(1, nlvls):
                if temperatures[0][i] < meteo.KELVIN_ZERO :
                    hFreeze = self.interp1(height[0][i], height[0][i - 1], meteo.KELVIN_ZERO, temperatures[0][i - 1])
                    pFreeze = self.interp1(pressure[0][i], pressure[0][i - 1], height[0][i], hFreeze, heights[0][i - 1])
                    frzlev[0][0] = pFreeze
                    frzlev[0][1] = hFreeze
                    return frzlev
        else :
            pFreeze = pressures[0][0]
            hFreeze = 0
            
        
        frzlev[0][0] = pFreeze
        frzlev[0][1] = hFreeze
        return frzlev
    
    # Determines the pressure, temperature, and height of the wet-bulb zero
    def wbzero(self, elevation, pressures, heights, temperatures, dewpoints):
        wetBulbReturns = zeros((1, 3), 'float32')
        #initialize the outputs
        pressurewbzero = pressures[0][0]
        heightwbzero = elevation
        tempwbzero = temperatures[0][0]
        nlvls = len(pressures) - 1
        
        thetaE = self.ept(temperatures[0], dewpoints[0], pressures[0])
        tWet1 = self.tsa(thetaE, pressures[0])
        
        if tWet1 < meteo.KELVIN_ZERO:
            wetBulbReturns[0][0] = pressurewbzero
            wetBulbReturns[0][1] = heightwbzero
            wetBulbReturns[0][2] = tempwbzero
            return wetBulbReturns
        
        for i in range(1, nlvls):
            thetaE = self.ept(temperatures[i], dewpoints[i], pressures[i])
            tWet2 = self.tsa(thetaE, pressures[i])
            
            if tWet2 < meteo.KELVIN_ZERO :
                heightwbzero = self.interp1(heights[i], heights[i - 1], tWet1, meteo.KELVIN_ZERO, tWet2)
                pressurewbzero = self.interp1(pressures[i], pressures[i - 1], heights[i], heightwbzero, heights[i - 1])
                tempwbzero = self.interp1(temperatures[i], temperatures[i - 1], heights[i], heightwbzero, heights[i - 1])
                
                wetBulbReturns[0][0] = pressurewbzero
                wetBulbReturns[0][1] = heightwbzero
                wetBulbReturns[0][2] = tempwbzero
                return wetBulbReturns  
            
            tWet1 = tWet2
        
        wetBulbReturns[0][0] = pressurewbzero
        wetBulbReturns[0][1] = heightwbzero
        wetBulbReturns[0][2] = tempwbzero
        return wetBulbReturns
    
    # calculates a layer mean wind (direction and speed) given 'TOP' and
    # 'BOT' of the layer defined in kilometers above ground level
    def avwind(self, elev, top, bot, hw, pw, tw, uw, vw):
        """ This subroutine calculate a layer mean wind (direction and speed) given
            given 'TOP' and 'BOT' of the layer defined in kilometers above
            ground level.
            
            History.
            --------
            D. Baker       01 Jul 84    Original version.
            
            Description of input and output.
            --------------------------------
            On input:
            ---------
            ELEV        Real          Station elevation (m agl).
            TOP         Real          Top of desired mean wind layer (km agl).
            BOT         Real          Bottom of desired mean wind layer (km agl).
            HW          Real Array    Wind level heights (m asl).
            TW          Real Array    Wind level temperatures (kelvin).
            UW          Real Array    Wind level u-components (m/s).
            VW          Real Array    Wind level v-components (m/s).
            NW          Integer       Number of wind levels passed.
            
            On output:
            ----------
            UAVG        Real          Layer mean u-component wind
            VAVG        Real          Layer mean v-component wind
            AVDIR       Real          Layer mean wind direction (deg).
            AVSPD       Real          Layer mean wind speed (m/s). """
        
        wind = zeros((1, 4), 'float32')
        KM2M = 1000
        RCONST = 287
        
        avdir = meteo.TOP_FLG
        avspd = meteo.TOP_FLG
        nw = len(hw) - 1
        
        su = 0
        sv = 0
        wgt = 0
        
        # Calculate the top and bottom of the desired layer in meters
        # above ground level.  Exit with 'flag' values assigned to mean wind
        # if unexpected conditions occur, or if sounding not deep enough to
        # perform calculation.
        top1 = top * KM2M + elev
        bot1 = bot * KM2M + elev
        
        if top1 <= bot1 :
            return None, None, avdir, avspd
        
        if uw[0] > meteo.FLG or vw[0] > meteo.FLG :
            wind[0][0] = None
            wind[0][1] = None
            wind[0][2] = avdir
            wind[0][3] = avspd                            
            return wind
        
        if hw[0] > bot1 or hw[nw] < top1 :
            wind[0][0] = None
            wind[0][1] = None
            wind[0][2] = avdir
            wind[0][3] = avspd                            
            return wind
            
        # Loop 100 interpolates u and v components to the level that is
        # to be the bottom of the layer for which the mean wind is
        # desired.  The subscript in the height array that is at or just
        # above the bottom is saved.
        ibot = 0
        ubot = uw[ibot]
        vbot = vw[ibot]
        
        for i in range(1, nw) :
            if hw[i] >= bot1 :
                ubot = self.interp1(uw[i], uw[i - 1], hw[i], bot1, hw[i - 1])
                vbot = self.interp1(vw[i], vw[i - 1], hw[i], bot1, hw[i - 1])
                ibot = i
                break
        
        # Loop 200 interpolates u and v components to the level that is
        # to be the top of the layer for which the mean wind is
        # desired.  The subscript in the height array that is at or just below
        # the top is saved.
        itop = nw
        utop = uw[nw]
        vtop = vw[nw]
        
        for i in range(nw - 1, 0, - 1):
            if hw[i] <= top1:
                utop = self.interp1(uw[i], uw[i + 1], hw[i], top1, hw[i + 1])
                vtop = self.interp1(vw[i], vw[i + 1], hw[i], top1, hw[i + 1])
                itop = i
        
        # Check to see if we are only dealing with wind-only data
        # i.e., profiler, VWP. etc.  If so skip the density-weighted
        # process and just sum the u- and v-components
        if tw[ibot] > 350.0 or tw[ibot] <= 150.0 :
            # Just sum up the u- and v-components if we have only wind
            # data w/o temp profiles (no density-weighted process possible).
            density = (pw[ibot] * 100) / (tw[ibot] * RCONST)
            su = density * ubot
            sv = density * vbot
            wgt = 1
            for i in range(ibot + 1, itop) :
                su = su + uw[i]
                sv = sv + density * vw[i]
                wgt = wgt + 1
                
            su = su + utop
            sv = sv + vtop
            wgt = wgt + 1
        else :
            #  Begin the density-weighted sum process by first considering the 
            #  component derived from the interpolated data at the 'bot' and 
            #  the level immediately above.
            density = (pw[ibot] * 100) / (tw[ibot] * RCONST)
            su = density * ubot
            sv = density * vbot
            wgt = density
            
            # Now, loop through all levels (until within one level of the
            # interpolated top of the layer).
            for i in range(ibot + 1, itop) :
                density = (pw[i] * 100) / (tw[i] * RCONST)
                su = su + density * uw[i]
                sv = sv + density * vw[i]
                wgt = wgt + density
        
            # Similarly to above, finish the process by incorporating the last
            # sublayer, from one level below the interpolated top to the
            # top.
            density = (pw[itop] * 100.0) / (tw[itop] * RCONST)
            su = su + density * utop
            sv = sv + density * vtop
            wgt = wgt + density
        
        # Finally, calculate the layer mean wind speed and direction.
        uavg = su / wgt
        vavg = sv / wgt
        
        avdir, avspd = self.ddff(uavg, vavg)                
        
        
        wind[0][0] = uavg
        wind[0][1] = vavg
        wind[0][2] = avdir
        wind[0][3] = avspd                            
        return wind
    
    # Calculates the storm relative helicity from the surface to 3000 meters
    # above ground level
    def helicity(self, hw, pw, uw, vw, elev, ztop, diravg, spdavg):
        """ This subroutine calculates the storm relative helicity from
            the surfce to 3000 meters above ground level.  Code lifted
            from the original FORTRAN code routine 'generate.for'.
            
            History.
            --------
            D. Perry       27 Feb 97    'Original' version.
            
            Description of input and output.
            --------------------------------
            On input:
            ---------
            HW          Real Array    Wind level heights (m asl).
            PW          Real Array    Wind level pressures (mb).
            UW          Real Array    Wind level u-components (m/s).
            VW          Real Array    Wind level v-components (m/s).
            NW          Integer       Number of wind levels passed.
            ELEV        Real          Station elevation (m agl).
            ztop        Real          Top of desired helicity layer (m agl).
            diravg      Real          0-6km avg wind direction (deg).
            spdavg      Real          0-6km avg wind speed (m/s).
            
            On output:
            stmdir      Real          0-6km Storm Motion Dir (30 deg to right of diravg)
            stmspdkts   Real          0-6km Storm Motion Spd (75% of spdavg)
            SRHel       Real          Storm Relative Helicity (m2/s2). """
        
        storm = zeros((1, 3), 'float32')
        M2KTS = 1.94254
        helicity = meteo.TOP_FLG
        SRHel = meteo.TOP_FLG
        nw = len(hw)
        
        # Calculate helicity.
        for topidx in range(1, nw - 1) :
            if hw[topidx] - elev >= ztop :
                break
        if (hw[topidx] - elev) < ztop :            
            storm[0][0] = meteo.TOP_FLG
            storm[0][1] = meteo.TOP_FLG
            storm[0][2] = SRHel
            return storm
        topidx = topidx - 1
        wgt2 = (ztop + elev - hw[topidx]) / (hw[topidx + 1] - hw[topidx])
        wgt1 = 1.0 - wgt2
        utop = uw[topidx] * wgt1 + uw[topidx + 1] * wgt2
        vtop = vw[topidx] * wgt1 + vw[topidx + 1] * wgt2
        
        helicity = 0.0
        for i in range(topidx - 1) :
            helicity = helicity - uw[i] * vw[i] * vw[i + 1] + uw[i + 1] * vw[i]
        
        helicity = helicity - uw[topidx] * vtop + utop * vw[topidx]
        ghx = vtop - vw[0]
        ghy = utop + uw[0]
                
        # Calculate default storm wind using previously calculated
        # 0-6 km density-weighted average wind.
        stmdir = diravg + 30.0
        if stmdir > 360.0 :
            stmdir = stmdir - 360.0
        
        stmspdkts = spdavg * M2KTS * 0.75
        stmspdms = spdavg * 0.75
        
        ustorm, vstorm = self.uvcomp(stmdir, stmspdms)
        SRHel = helicity + ghx * ustorm + ghy * vstorm
        
        storm[0][0] = stmdir
        storm[0][1] = stmspdkts
        storm[0][2] = SRHel
        return storm
    
    # formula for forecast function
    def drange(self, thi, tlo):
        return (0.55556 * (thi - 32.0)) - (0.55556 * (tlo - 32.0))
    
    # gets the julian day
    def cv_date2jul(self, year, month, day):
        t = time.mktime((year, month, day, 0, 0, 0, 0, 0, 0))
        return time.gmtime(t)[7]
    
    # determines the solar radiation
    def solax(self, julday, month, slat, tyminc, tstart, tstop):
        COS89 = 0.01745241
        SDF = [0.058, 0.060, 0.071, 0.097, 0.121, 0.134, 0.136, 0.122, 0.092, 0.073, 0.063, 0.057]
        OPTDPH = [0.142, 0.144, 0.156, 0.180, 0.196, 0.205, 0.207, 0.201, 0.177, 0.160, 0.149, 0.142]
     
        tau = (meteo.TWOPI * julday) / meteo.YRLEN
        
        ratio = 1 / (1 - meteo.ECC * math.cos(tau))
        
        # Compute the solar declination
        declnatn = meteo.SDECMX * math.sin(meteo.TWOPI * (julday - meteo.VEDAY) / meteo.YRLEN)
        phi = slat * meteo.DEG2RAD
        
        darc = 0.25 * tyminc
        hlfangl = 0.5 * darc
        anginc = darc
        
        # Compute the starting angle and the hour angle which is zero at solar noon
        strtang = 180 + tstart * 15
        hrang = strtang + hlfangl
        
        #convert time increment to fraction of an hour.  Initialize time and solar radiation (tsrad)
        dt = tyminc / 60
        time = tstart
        tsrad = 0
        
        while True :
            delta = declnatn
            sinx = math.sin(phi) * math.sin(delta)
            cosx = math.cos(phi) * math.cos(delta)
            cosz = sinx + (cosx * (math.cos(hrang * meteo.DEG2RAD)))
        
            if abs(cosz) < COS89 :
                if cosz < 0 :
                    cosz = - COS89
                    if cosz > 0 :
                        cosz = COS89
        
            secz = 1 / cosz
        
            sdrct = meteo.CN * meteo.S * math.pow(ratio, 2) * cosz * math.exp((- OPTDPH[month]) * secz)
            sdiff = SDF[month] * (sdrct / (meteo.CN * meteo.CN))
            radx = (sdrct + sdiff) * tyminc
        
            if radx > 0 :
                tsrad = tsrad + radx
        
            hrang = hrang + anginc
            time = time + dt
            if time > tstop :
                break   
        return tsrad
        
    def eqp (self, deltap, pressures, heights, temperatures, dewpoints):
        """ This routine derives sounding temperature and dew point data at pressure
            intervals given by DELTAP.

            History.
            --------                    
            D. Baker       27 Dec 85    Original version.
            
            Description of input and output.
            --------------------------------
            On input:
            ---------                
            DELTAP      Real          Pressure interval at which to compute T/Td.
            P           Real Array    Sounding pressures levels (mb).
            T           Real Array    Sounding temperatures (C).
            TD          Real Array    Sounding dew points (C).
            N           Integer       Number of uninterpolated sounding levels input.
            
            On output:
            ----------               
            PP          Real Array    New array of sounding pressures at DELTAP mb.
            TT          Real Array    New array of sounding temperatures at DELTAP mb.
            TTD         Real Array    New array of sounding dewpoints at DELTAP mb.
            NN          Integer       Number of new sounding levels returned. """
            
        n = pressures.shape[0] - 1
        if round(pressures[0][0]) % round(deltap) == 0:
            pbot = round(pressures[0][0] - deltap)
        else :
            pbot = round(pressures[0][0] - (round(pressures[0][0]) % round(deltap)))
        
        ptop = round(pressures[n][0])
        pinc = round(deltap)            

        outPressures = []
        outHeights = []
        outTemperatures = []
        outDewpoints = []
        nn = 0
        pres = pbot
        
        for pres in range(int(pbot), int(ptop), int(- pinc)) :
            outPressures.append(pres)
            nn = nn + 1            
        
        # Derive temperature and dew point at the DELTAP mb intervals.
        j = 1
        k = 0
        for k in range(nn) :
            for i in range(j, n) :
                if pressures[i][0] <= outPressures[k] and pressures[i - 1][0] >= outPressures[k] :
                    p1 = math.log(pressures[i - 1][0])
                    p2 = math.log(outPressures[k])
                    p3 = math.log(pressures[i][0])
                    outHeights.append(self.interp1(heights[i - 1][0], heights[i][0], p1, p2, p3))
                    outTemperatures.append(self.interp1(temperatures[i - 1][0], temperatures[i][0], p1, p2, p3))
                    outDewpoints.append(self.interp1(dewpoints[i - 1][0], dewpoints[i][0], p1, p2, p3))
                    j = i
                    break
        return outPressures, outHeights, outTemperatures, outDewpoints
    
    #Calculates saturation vapor pressure in millibars as a function of either Kelvin of Celsius temperature.
    def esat(self, temperature):
        
        kelvinTemp = temperature
        if kelvinTemp < 100 :
            kelvinTemp = kelvinTemp + 273.15
            
        if kelvinTemp < 0.0 or kelvinTemp > 373.15 :
            return meteo.FLAG
        
        if kelvinTemp < 173.15 :
            return 3.777647e-05
        
        return math.exp(26.660820 - 0.0091379024 * kelvinTemp - 6106.3960 / kelvinTemp)
    
    # computes the mean relative humidity in the 150 mb layer 
    # just above the surface, the 150 mb layer above that, and 
    # the 200 mb layer above the second layer
    def rhbar(self, endlvl, nclyr, sfcp, p, tl, tdl):
        
        sumrh = 0
        i = 0
        istart = i
        lyr = 0

        mrh = []
        while True:
            e = self.esat(tl[i]) 
            w = (0.622 * e) / (p[i] - e)
            es = self.esat(tdl[i])
            ws = (0.622 * es) / (p[i] - es)
            rh = 100 * (ws / w)
            sumrh = sumrh + rh
            if p[i] > endlvl[lyr] :
                i = i + 1
            idiv = i - istart + 1
            mrh.insert(lyr, sumrh / idiv)
            
            if mrh[lyr] < 0 :
                mrh[lyr] = 0
            if mrh[lyr] > 100 :
                mrh[lyr] = 100
            
            if lyr < (nclyr - 1) :
                lyr = lyr + 1
                sumrh = 0
                istart = i
            else :
                break
        return mrh
    
    # determines the max temp
    def mxtp(self, ansol, deltap, sfcp, p2, tl, deltaz):
        sfct = tl[0]
        
        lvl = len(deltaz)
        dt = 0.70 * ansol
        okay = True
        while (okay == True):
            pressure = sfcp
            dpress = p2
            rbot = (sfcp - p2) / deltap
        
            dsum = 0
            adtemp = sfct + dt
            ctmax = adtemp
        
            for mlyr in range(lvl):
                r = 1
                if mlyr == 1 :
                    r = rbot
                tdiff = adtemp - tl[mlyr]
                dsum = dsum + tdiff * r
                delz = deltaz[mlyr]
                adtemp = adtemp - (meteo.DRYLAPS * delz)
                press = dpress
                dpress = press - deltap
                if (dpress < 400):
                    break
            
            deldt = dsum - ansol
            if abs(deldt) <= 2 :
                ftmax = ctmax
                break
            elif deldt > 2 and dsum > 0 :
                dt = dt - 0.5 * deldt / dsum
                if dt > 0 :
                    continue
            else :
                ctmax = sfct
                ftmax = ctmax
                break
            
        return ftmax
        
    
    # Calculate the trigger temp and soaring index
    def tsoar(self, elev, p, z, t, theta, tpmax):
        """ DARE-II TSOAR Program for Denver Aviation and soaring community.
            History.
            --------
                  L. Rasmussen     8 Feb 89       Original version.
                  J. Wakefield    20 Oct 89       Some tinkering.  Added PTLXEC parameter.
            
            Description of input and output.
            --------------------------------
            On input:
            ---------
            Elev          Station elevation
            p             Raob pressures (mb)
            z             Raob heights (m)
            t             Raob temperatures (degrees Celsius)
            theta         The potential temperatures of the sounding data
            nl            number of levels in sounding
            Tpmax         Forecast max temperature
            
            On output:
            ----------
            PTLXEC        Potential temperature (C) of forecast TMax.
            zlnec         The height (m) of the level of minimum effective convection.
            tlnec         The temperature (C) of the level of min effective convection.
            zlxec         The height (m) of the maximum thermal altitude
            tlxec         The temperature (C) of the LXEC (max thermal alt)
            Soarindx      The soaring index in ft/min.
            trigtemp      The trigger temperature (C) """

        
        trigtemp = meteo.FLG
        zlnec = meteo.FLG
        tlnec = meteo.FLG
        zlxec = meteo.FLG
        tlxec = meteo.FLG
        soarindx = meteo.FLG
        t500 = meteo.FLG
        
        nl = p.shape[0]
        
        # Determine if the station elevation is above or below 2000 ft msl (609.57 m),
        # then add either 4000 ft (1219.14 m) or 3000 ft (914.36 m) to compute the LNEC.
        # From the LNEC compute the interpolated pressure and temperature at the LNEC,
        # and finally compute the trigger temperature by following the potential
        #temperature line going thru the LNEC temperature down to the surface.
        if elev > 609.57 :
            zlnec = elev + 1219.14
        else :
            zlnec = elev + 914.36
         
        # Compute the potential temp. of the forecast max temperature, then run up the
        # sounding and locate or interpolate the height and pressure of the Lxec.
        ptlxec = tpmax * math.pow((1000.0 / p[0]), 0.286)
         
        # initialize Plnec, Plxec, &Z500
        i = 0
        plnec = p[i] + (p[i + 1] - p[i]) * ((zlnec - z[i]) / z[i + 1] - z[i])
        plxec = p[i] + (p[i + 1] - p[i]) * ((ptlxec - theta[i]) / (theta[i + 1] - theta[i]))
         
        z500 = z[i]
         
        for i in range(nl - 1):
            if ((zlnec >= z[i]) and (zlnec <= z[i + 1])) :
                tlnec = t[i] + (t[i + 1] - t[i]) * ((zlnec - z[i]) / (z[i + 1] - z[i]))
                plnec = p[i] + (p[i + 1] - p[i]) * ((zlnec - z[i]) / (z[i + 1] - z[i]))
                                 
            # Save info at 500 mb, in case the max thermal alt. is above 500 mb and must be
            # truncated at 500 mb/18000 ft.                                     
            if p[i] <= 500.0 and t500 == meteo.FLG :
                t500 = t[i]
                z500 = z[i]
                p500 = p[i]
            
            if ptlxec >= theta[i] and ptlxec <= theta[i + 1] :
                if theta[i] == theta[i + 1] :                    
                    zlxec = z[i]
                    plxec = p[i]
                    tlxec = t[i]
                else :
                    zlxec = z[i] + (z[i + 1] - z[i]) * ((ptlxec - theta[i]) / (theta[i + 1] - theta[i]))
                    plxec = p[i] + (p[i + 1] - p[i]) * ((ptlxec - theta[i]) / (theta[i + 1] - theta[i]))
                    tlxec = t[i] + (t[i + 1] - t[i]) * ((ptlxec - theta[i]) / (theta[i + 1] - theta[i]))
            
        
        # compute the potential temperature of the trigger temperature.
        thetaTrig = tlnec * math.pow((1000.0 / plnec), 0.286)
        trigTemp = ((thetaTrig) * math.pow((p[0] / 1000.), 0.286))
                
        # Truncate the height of the LXEC to 18000 ft, due to FAA positive controlled
        # airspace or possible soaring flight in IFR conditions by penetrating into
        # clouds.
        if plxec < 500.0 :
            zlxec = z500
            tlxec = t500
            plxec = p500
        
        # Compute the Soaring Index if the fcst max temperature is equal to or exceeds
        # the trigger temperature.
        if tpmax >= trigTemp and tlnec != meteo.FLG and tlxec != meteo.FLG:            
            soarindx = 3 * ((zlxec / 0.3048 / 100.0) + 10.0 * (tlnec - tlxec))            
        else :
            soarindx = meteo.TOP_FLG
        
        tsoarReturn = zeros((1, 7), 'float32')
        tsoarReturn[0][0] = ptlxec
        tsoarReturn[0][1] = zlnec
        tsoarReturn[0][2] = tlnec
        tsoarReturn[0][3] = zlxec
        tsoarReturn[0][4] = tlxec
        tsoarReturn[0][5] = soarindx
        tsoarReturn[0][6] = trigTemp
        
        return tsoarReturn
    
    # Computes the pressure, height, temperature of the convective condensation level from a sounding
    def cclpar(self, mix, p, ht, t):
        """ This routine computes the pressure, height, and temperature of the
            convective condensation level (CCL) from a sounding.
            
            History.
            --------                    
            Don Baker      15 Jun 85    Original version.
            Dale Perry        Sep 96    Adapted code for WFO
            
            Description of input and output.
            --------------------------------
            On input:
            ---------                
            MIX         Real          Mixing ratio used to intersect the sounding (g/kg).
            P           Real Array    Sounding pressures (mb).
            HT          Real Array    Sounding heights (m asl).
            T           Real Array    Sounding temperatures (K).
            NLVLS       Integer       Number of sounding levels passed.
            
            On output:
            ----------               
            PCCL        Pressure of the convective condensation level (mb).
            TCCL        Temperature of the convective condensation level (K).
            HCCL        Height of the convective condensation level (m asl).
            User notes:
            -----------
            1) The low level mean mixing ratio is input to this routine...
               computed outside.
            2) On days with a strong low level inversion, the convective
               temperature may seem low because the strict definition is
               used in the computation (i.e., where the low level mixing
               ratio line first intersects the sounding). """
        ccl = zeros((1, 3), 'float32')
        TOLER = 0.05
        
        i = 0
        for i in range(len(p)) :
            t1 = self.temp_mixratio(p[i], mix)
            t2 = t[i]
            if t1 >= t2 :
                break
        
        if i == 0 : # CCL at the surface 
            pccl = p[0][0]
            hccl = ht[0][0]
            tccl = t[0][0]
            ccl[0][0] = pccl
            ccl[0][1] = tccl
            ccl[0][2] = hccl
            return ccl
    
        pt = p[i]
        pb = p[i - 1]
        plog1 = log(p[i])
        plog3 = log(p[i - 1])
                
        # Iterate to find the CCL.  Keep cutting level in half until the
        # point of intersection is found.
        for count in range(100) :
            pm = 0.5 * (pt + pb)
            plog2 = math.log(pm)
            t1 = self.temp_mixratio(pm, mix)
            t2 = self.interp1(t[i], t[i - 1], plog1, plog2, plog3)
            if abs(t1 - t2) <= TOLER :
                pccl = pm
                tccl = t1
                hccl = self.interp1(ht[i], ht[i - 1], plog1, math.log(pccl), plog3)
                ccl[0][0] = pccl
                ccl[0][1] = tccl
                ccl[0][2] = hccl
                return ccl
            if (t1 - t2) > TOLER :
                pt = pm
            if (t2 - t1) > TOLER :
                pb = pm
        
        ccl[0][0] = pccl
        ccl[0][1] = tccl
        ccl[0][2] = hccl
        return ccl
        
    # calculates the temperature at a given pressure and mixing ratio
    def temp_mixratio (self, press, mixratio):
        sat = (press * mixratio) / (0.622 + mixratio)
        b = 26.66082 - log10(sat)        
        tempmr = (b - math.sqrt(b * b - 223.1986)) / 0.0182758048        
        return tempmr
    
    
    def eqlev(self, p, ht, tp, te, plfc, eptpar):
        eqlevReturn = zeros((1, 3), 'float32')
        TOLER = 0.1
        npar = len(p)
        
        if te[npar - 1] <= tp[npar - 1] :
            peqlev = meteo.MISSING
            heqlev = meteo.MISSING
            teqlev = meteo.MISSING
            eqlevReturn[0][0] = peqlev
            eqlevReturn[0][1] = heqlev
            eqlevReturn[0][2] = teqlev
            return eqlevReturn
        
        ii = 0
        for i in range(npar) :
            if p[i] <= plfc :
                ii = i
                if ii == npar :
                    eqlevReturn[0][0] = peqlev
                    eqlevReturn[0][1] = heqlev
                    eqlevReturn[0][2] = teqlev
                    return eqlevReturn
                break
        for j in range(npar - 1, ii, - 1) :
            if te[j] <= tp[j] :
                pt = p[j + 1]
                pb = p[j]
                plog1 = math.log(pt)
                plog3 = math.log(pb)
            
                for count in range(100) :
                    pm = 0.5 * (pt + pb)
                    plog2 = math.log(pm)
                    tem = self.interp1(te[j + 1], te[j], plog1, plog2, plog3)
                    etpar = eptpar * math.pow((pm / 1000.0), 0.286)
                    tpm = self.temp_of_te(etpar, pm)
                    if  math.fabs(tpm - tem) < TOLER :
                        peqlev = pm
                        heqlev = self.interp1(ht[j + 1], ht[j], plog1, plog2, plog3)
                        teqlev = tem
                        eqlevReturn[0][0] = peqlev
                        eqlevReturn[0][1] = heqlev
                        eqlevReturn[0][2] = teqlev
                        return eqlevReturn
                
                    if tpm - tem > TOLER :
                        pb = pm
                        if tem - tpm > TOLER :
                            pt = pm

        eqlevReturn[0][0] = peqlev
        eqlevReturn[0][1] = heqlev
        eqlevReturn[0][2] = teqlev
        return eqlevReturn
        
    
    # determines the size of the hail
    def hailsiz (self, vvmax):
        DRAG = 0.55
        RHOICE = 900.0
        RHOAIR = 1.0033
        GE = 9.8
        hailsize = 2 * ((3 * DRAG * RHOAIR * (vvmax * vvmax)) / (8 * meteo.GE * RHOICE)) * 100
        return hailsize        
        
    def vvel(self, pcb, peqlev, p, ht, tp, tve, tvp, wlcl):
        water = []
        DRAG = 0.33
        ENTRN = 0.67
        npar = len(p)
        for i in range(npar) :
            e = self.esat(tp[i])
            water.append(wlcl - ((0.622 * e) / (p[i] - e)))
        vv = zeros(p.shape, 'float32') 
        vv[0] = 0.0
        vvmax = vv[0]
        vv2 = 0
        for i in range(1, npar) :
            if p[i] > pcb :
                dh = ht[i] - ht[i - 1]
                wm = 0.0005 * (water[i] + water[i - 1])
                tem = 0.5 * (tve[i] + tve[i - 1])
                tpm = 0.5 * (tvp[i] + tvp[i - 1])
                dtmp = tpm - tem
                vv2 = vv[i - 1][0] * vv[i - 1][0] + 2 * meteo.GE * dh * (ENTRN * dtmp / tem - DRAG * wm)
                if vv2 > 0.0 :
                    vv[i][0] = math.sqrt(vv2)
                if p[i] >= peqlev :
                    if vvmax < vv[i][0] :
                        vvmax = vv[i][0]
     
        return vv, vvmax
    
    # Compute the air density at each level of a sounding
    def density(self, p, tvir):
        for i in range(len(p)) :
            rho[i] = 100.0 * p[0][i] / (287.04 * tvir[i])
        return rho
    
    # estimates the cloud top based on the undiluted parcel vertical velocity profile
    def ctop(self, p, ht, vv, peqlev):
        cldtop = meteo.MISSING
        npar = len(p)
        
        if peqlev == meteo.MISSING or peqlev == 0:
            return cldtop
        
        for i in range(npar) :
            if p[i][0] <= peqlev :
                break
        
        if i == npar :
            return cldtop
        
        for j in range(i, npar) :
            if vv[j] < 0.01 :
                cldtop = ht[j][0]
                return cldtop
        
        return cldtop

    # Calculates the equivalent temperature of a temperature and pressure using the adiabatic definition
    def adiabatic_te(self, temp, press):
        e = math.exp(26.660820 - 0.0091379024 * temp - 6106.396 / temp)
        e = 0.622 * e / (press - e)
        return temp * math.exp(2740.0 * e / temp)
        
    
    # Computes the rectangular wind components given wind direction and speed
    def uvcomp (self, dir, spd):
        RPD = 0.0174533
        if type(dir) is not ndarray:
            dirAr = zeros((1, 1), 'float32')
            spdAr = zeros((1, 1), 'float32')
            dirAr[0][0] = dir
            spdAr[0][0] = spd
            dir = dirAr
            spd = spdAr            
        nlvls = len(dir)
        
        u = zeros(dir.shape, 'float32')
        v = zeros(dir.shape, 'float32')
        
        for i in range(nlvls):
            if dir[i] < 0 or dir[i] > 360 or spd[i] < 0 or spd[i] > 250 :
                u[i] = meteo.TOP_FLG
                v[i] = meteo.TOP_FLG
            else :
                angle = RPD * dir[i]
                u[i] = (- spd[i]) * math.sin(angle)
                v[i] = (- spd[i]) * math.cos(angle)
        
        return u, v
    
    # Compute the value of some parameter in a sounding at a given pressure level using log pressure interpolation
    def pvalue(self, pres, p, param):
        # Make sure that the computation is possible.
        np = len(p)
        value = meteo.TOP_FLG
        if pres < p[np - 1] or pres > p[0] :
            return value
        
        # Check and see if the lowest pressure is equal to the desired pressure.
        if math.fabs(p[0] - pres) < 0.1 :
            value = param[0]
            return value
        
        # Determine value of parameter at level "pres" in the sounding.
        for i in range(1, np - 1) :
            if p[i] <= pres :
                p1 = math.log(p[i - 1])
                p2 = math.log(pres)
                p3 = math.log(p[i])
                value = self.interp1(param[i - 1], param[i], p1, p2, p3)
                return value
        return value
    
    
    
    def tplcl(self, tk, td, pinit):
        """ Calculate the temperature and pressure at the LCL given the 
            parcel's initial temperature, dew point and pressure.

            History:
            E. Thaler 13 Mar 1997 - original version

            On input:

            tk    = parcel temperature in Celsius
            td    = parcel dew point temperature in Celsius
            pinit = initial pressure level of the parcel

            On output:

            tl    = temperature (K) at the LCL 
            pl    = pressure (mb) at the LCL
            ier   = >0 if successful completion, <0 if failure """

        i = 0
        ier = - 1
        p0 = 1000.0
        tl = 0
        pl = 0
        
        # Convert temperature/dewpoint to K and compute 
        # initial potential temperature.
        tkk = tk + meteo.KELVIN_ZERO
        tdk = td + meteo.KELVIN_ZERO
        theta = tkk * math.pow((p0 / pinit), 0.2854)
        
        # Use Newton's method to compute the temperature
        # at the LCL using Eq. (14) in Bolton (MWR 1980)

        tlold = tdk
        
        for i in range(20):
            fprime = (0.001266 / tlold) - (1 / (math.pow(tlold, 2)))
            dtl = (1.0 / tlold) - (1.0 / tdk) + (0.001266 * math.log(tlold / tdk)) - (0.000514 * math.log(tkk / tdk))
            dtl = tlold - (dtl / fprime)
            
            # If we have converged to a solution, compute the pressure at
            # the LCL and return, otherwise iterate again.
            # njensen changed the check from 1.0e-7 to 1.0e-3
            if math.fabs(dtl - tlold) <= 1.0e-3 :
                pl = p0 * (math.pow((dtl / theta), (1.0 / 0.2854)))
                tl = dtl
                ier = 1
                return tl, pl, ier
            else:
                tlold = dtl

        return tl, pl, ier
    
    # Computes the values of the dry adiabat from the initial parcel pressure to the LCL    
    def deftrk(self, tcb, pcb):
        thdpar = tcb * math.pow((1000.0 / pcb), 0.286)
        etpar = self.adiabatic_te(tcb, pcb)
        eptpar = etpar * math.pow((1000.0 / pcb), 0.286)
        # eptpar = self.mytw(tcb,tcb,pcb)
        return thdpar, eptpar
    
    def mytw (self, k, kd, p):
        """ Calculate the isobaric wet-bulb temperature
        
        ported from the fortran function mytw from calctw.f
        
        @param K
                    temperature in degrees K
        @param Kd
                    dewpoint in degrees K
        @param p
                    pressure in millibars
        @return
                    isobaric wet-bulb temperature in degrees K
        
        """
        
        # This function takes temperature in degrees K, dewpoint in degrees K
        # and pressure in millibars and returns the isobaric wet-bulb temperature
        # C in degrees K using an iterative technique.  For a given guess for the
        # wet bulb temp, one tries to do an energy balance, matching cp*(T-Tw) to
        # (esat(Tw)-esat(Td))*eps*L/p*.
        
        # c0, c1, and c2 are the same constants as from the esat.f function.
        # f = cp/(L*epsilon).
        
        f = 0.0006355
        c0 = 26.66082
        c1 = 0.0091379024
        c2 = 6106.3960
        
        # Special cases of Td >= T or a ridiculously low T.
        if kd >= k :
            kw = (k + kd) / 2
            return kw
        elif k < 100 :
            kw = k
            return kw
        
        # Special case of a ridiculously high saturation vapor pressure.
        ew = c0 - c1 * k - c2 / k
        
        if ew > 10.0 :
            kw = (k + kd) / 2
            return kw
        
        ew = math.exp(ew)
        
        # kw is our current guess for wet-bulb, ed the vapor pressure corresponding
        # to the depoint.  Deal with case of a ridiculously small dewpoint vapor
        # pressure.
        
        kdx = kd
        ed = c0 - c1 * kdx - c2 / kdx
        while True:
            if ed < - 50.0 :
                kdx = kdx + 10
                ed = c0 - c1 * kdx - c2 / kdx
            else :
                break
        
        ed = math.exp(ed)
        fp = p * f
        s = (ew - ed) / (k - kdx)
        
        kw = (k * fp + kdx * s) / (fp + s)
        
        # At each step of the iteration, esat(Tw)-esat(Td) is compared to
        # (T-Tw)*p/(eps*L).  When that difference is less than one part in 
        # 10000 of esat(Tw), or ten iterations have been done, the iteration stops.
        # This is basically trying to find the value of kw where de is 0.  The
        # value s is the derivative of de with respect to kw, a fairly standard
        # numerical technique for finding the zero value of a function.
        
        for l in range (10) :
            ew = c0 - c1 * kw - c2 / kw
            if ew < - 50.0 or ew > 10.0 :
                break
            ew = math.exp(ew)
            de = fp * (k - kw) + ed - ew
            if math.abs(de / ew) < 1e-5 :
                continue
            s = ew * (c1 - c2 / (kw * kw)) - fp
            kw = kw - de / s
        
        return kw
                    
    # Compute the convective gust potential by locating the 700 mb dewpoint depression
    # and the upper level stability index on the nomogram
    def cvgust(self, dd7, ui):
        gstpot = 0
        if dd7 <= 10 :
            if ui <= 5 :
                gstpot = 1
            elif ui > 5 :
                if self.eq1(ui) >= dd7 :
                    gstpot = 1
        
        if gstpot != 0 :
            return gstpot
        
        if ui >= 5 :
            if dd7 >= 10 and dd7 <= 25 :
                gstpot = 2
            elif dd7 < 10 :
                if self.eq1(ui) <= dd7 :
                    gstpot = 2
            elif dd7 > 25 :
                if self.eq3(ui) >= dd7 :
                    gstpot = 2
        
        if gstpot != 0 :
            return gstpot
        
        if dd7 >= 10 and dd7 <= 15 :
            if ui <= 5 :
                gstpot = 3
        elif ui <= 5 and ui >= 3.5 :
            if dd7 >= 10 and dd7 <= 25 :
                gstpot = 3
            elif dd7 >= 25 :
                if self.eq2(ui) >= dd7 and self.eq3(ui) <= dd7 :
                    gstpot = 3
        elif ui >= 5 :
            if self.eq3(ui) <= dd7 :
                gstpot = 3
                
        if gstpot != 0 :
            return gstpot
        
        if dd7 >= 15 and dd7 <= 25 :
            if ui <= 5 :
                gstpot = 4
        elif dd7 >= 25 :
            if self.eq2(ui) <= dd7 :
                gstpot = 4
        
        return gstpot
    
    # used in cvgust
    def eq1(self, x):
        return (- 2 * x + 20)
    # used in cvgust
    def eq2(self, x):
        return (3.33 * x + 13.33)
    # used in cvgust
    def eq3(self, x):
        return (2 * x + 15)

    # This routine computes the wind direction (deg) and speed given rectangular wind components
    def ddff(self, u, v):
        DPR = 57.29578
        dir = zeros(u.shape, 'float32')
        spd = zeros(u.shape, 'float32')
        for i in range(len(u)) :
            if u[i] > 150 or u[i] < - 150 or v[i] > 150 or v[i] < - 150 :
                dir[i] = meteo.MISSING
                spd[i] = meteo.MISSING
            elif math.fabs(u[i]) < 0.05 and math.fabs(v[i]) < 0.05 :
                dir[i] = 0.0
                spd[i] = 0.0
            else :
                dir[i] = DPR * (math.atan2(u[i], v[i])) + 180
                spd[i] = math.sqrt(u[i] * u[i] + v[i] * v[i])
        
        return dir, spd
    
    
    def liftedp(self, p, t, ht, tvir, pcb, hcb, tcb, wcb, thdpar, eptpar, pl, tl):        
        nlvls = p.shape[0]
        pp = zeros(p.shape, 'float32')
        htp = zeros(ht.shape, 'float32')
        tp = zeros(t.shape, 'float32')
        tvirp = zeros(tvir.shape, 'float32')
        te = zeros(t.shape, 'float32')
        tvire = zeros(t.shape, 'float32')
        doStep2 = True
        if pcb > meteo.FLG:
            nparcel = 0
        else :
            # Step 1.  Assign first element of arrays to initial parcel values.
            # this may involve interpolating environmental sounding data slightly
            # to get data at mean parcel pressure.
            
            # Note:  Subscript J refers to lifted parcel comparison arrays.
            #        Subscript K refers to environmental sounding arrays.
            j = - 1
            k = 0
            for ik in range(nlvls) :
                k = ik
                if p[k][0] > pl :
                    continue
                if p[k][0] <= pcb : 
                    break
                j = j + 1
                pp[j] = p[k]
                htp[j] = ht[k]
                tp[j] = thdpar * ((pp[j] / 1000.0) ** 0.286)                
                tdp = self.temp_mixratio(pp[j], wcb)
                ###tvirp[j] = self.virttemp(tp[j], tdp, pp[j])
                # njensen changed the following line cause temp_mixratio was ported
                # correctly and produces invalid data
                tvirp[j] = self.virttemp(tp[j], tp[j], pp[j])
                te[j] = t[k]
                tvire[j] = tvir[k]
                                            
            if fabs(p[0][0] - pcb) < 0.1 :
                doStep2 = False
                        
            # Step 2.  Compute array values at the LCL.
            if doStep2:
                j = j + 1
                print pp, j, pcb
                pp[j] = pcb
                htp[j] = hcb
                tp[j] = tcb
                tvirp[j] = self.virttemp(tp[j], tp[j], pp[j])
                            
                # Case where the LCL is at the surface
                if j == 1 :
                    te[j] = t[k]
                    tvire[j] = tvir[k]
                else :
                    # LCL is above the surface
                    plog1 = math.log(p[k - 1])
                    plog2 = math.log(pcb)
                    plog3 = math.log(p[k])
                    te[j] = self.interp1(t[k - 1], t[k], plog1, plog2, plog3)
                    tvire[j] = self.interp1(tvir[k - 1], tvir[k], plog1, plog2, plog3)
                
                if(p[k] == pcb):
                    k = k + 1
                        
            for k in range(k, nlvls - 1) :
                j = j + 1                
                pp[j] = p[k]
                htp[j] = ht[k]
                etpar = eptpar * ((pp[j] / 1000.0) ** 0.286)
                tp[j] = self.temp_of_te(etpar, pp[j])
                tvirp[j] = self.virttemp(tp[j], tp[j], pp[j])
                te[j] = t[k]
                tvire[j] = tvir[k]
                        
            nparcel = j                        
            
        return pp, htp, tp, tvirp, te, tvire, nparcel
    
    
    def ept(self, t, td, p):
        """ THIS FUNCTION RETURNS THE EQUIVALENT POTENTIAL TEMP EPT           
            (KELVIN) FOR A PARCEL OF AIR INITIALLY AT TEMP T (KELVIN),
            DEW POINT TD (KELVIN) AND PRESSURE P (MILLIBARS).

            BAKER,SCHLATTER 17-MAY-1982     Original version

            THE FORMULA USED
            IS EQ.(43) IN BOLTON, DAVID, 1980: "THE COMPUTATION OF EQUIVALENT
            POTENTIAL TEMPERATURE," MONTHLY WEATHER REVIEW, VOL. 108, NO. 7
            (JULY), PP. 1046-1053. THE MAXIMUM ERROR IN EPT IN 0.3C.  IN MOST
            CASES THE ERROR IS LESS THAN 0.1C."""

        # COMPUTE THE MIXING RATIO (GRAMS OF WATER VAPOR PER KILOGRAM OF
        #    DRY AIR).
        tdc = td - 273.16
        w = self.wmr(p, tdc)
        
        # COMPUTE THE TEMP (CELSIUS) AT THE LIFTING CONDENSATION LEVEL.
        tc = t - 273.16
        tlcl = self.tcon(tc, tdc)
        tk = t # + 273.16
        tl = tlcl + 273.16
        d1 = 1e3 / p
        d2 = (1.0 - w * 2.8e-4) * 0.2854
        pt = tk * pow(d1, d2)
        eptk = pt * exp((3.376 / tl - 0.00254) * w * (w * 8.1e-4 + 1.0))
        ret_val = eptk # - 273.16
        return ret_val

    def wmr(self, p, t):
        """ THIS FUNCTION APPROXIMATES THE MIXING RATIO WMR (GRAMS OF WATER
            VAPOR PER KILOGRAM OF DRY AIR) GIVEN THE PRESSURE P (MB) AND THE
            TEMPERATURE T (CELSIUS)
            
            BAKER,SCHLATTER 17-MAY-1982     Original version

            THE FORMULA USED IS GIVEN ON P. 302 OF THE
            SMITHSONIAN METEOROLOGICAL TABLES BY ROLAND LIST (6TH EDITION).

            EPS = RATIO OF THE MEAN MOLECULAR WEIGHT OF WATER (18.016 G/MOLE)
            TO THAT OF DRY AIR (28.966 G/MOLE)
            THE NEXT TWO LINES CONTAIN A FORMULA BY HERMAN WOBUS FOR THE
            CORRECTION FACTOR WFW FOR THE DEPARTURE OF THE MIXTURE OF AIR
            AND WATER VAPOR FROM THE IDEAL GAS LAW. THE FORMULA FITS VALUES
            IN TABLE 89, P. 340 OF THE SMITHSONIAN METEOROLOGICAL TABLES,
            BUT ONLY FOR TEMPERATURES AND PRESSURES NORMALLY ENCOUNTERED IN
            IN THE ATMOSPHERE. """                        
                    
        eps = 0.62197
        
        x = (t - 12.5 + 7500.0 / p) * 0.02
        wfw = p * 4.5e-6 + 1.0 + x * 0.0014 * x
        fwesw = wfw * self.esw(t)
        r = eps * fwesw / (p - fwesw)

        # CONVERT R FROM A DIMENSIONLESS RATIO TO GRAMS/KILOGRAM.
        ret_val = r * 1e3
        return ret_val

    def esw(self, t):
        """ THIS FUNCTION RETURNS THE SATURATION VAPOR PRESSURE ESW (MILLIBARS) */
            OVER LIQUID WATER GIVEN THE TEMPERATURE T (CELSIUS).

            BAKER,SCHLATTER 17-MAY-1982     Original version

            THE POLYNOMIAL APPROXIMATION BELOW IS DUE TO HERMAN WOBUS, A
            MATHEMATICIAN WHO WORKED AT THE NAVY WEATHER RESEARCH FACILITY,
            NORFOLK, VIRGINIA, BUT WHO IS NOW RETIRED. THE COEFFICIENTS OF THE
            POLYNOMIAL WERE CHOSEN TO FIT THE VALUES IN TABLE 94 ON PP. 351-353
            OF THE SMITHSONIAN METEOROLOGICAL TABLES BY ROLAND LIST (6TH ED).
            THE APPROXIMATION IS VALID FOR -50 < T < 100C. """


        es0 = 6.1078

        # ES0 = SATURATION VAPOR RESSURE OVER LIQUID WATER AT 0C 
        pol = t * (t * (t * (t * (t * (t * (t * (t * (t * 
 - 3.0994571e-20 + 1.1112018e-17) - 1.7892321e-15) + 
                2.1874425e-13) - 2.9883885e-11) + 4.3884187e-9) - 
                6.1117958e-7) + 7.8736169e-5) - 0.0090826951) + 0.99999683

        # Computing 8th power
        r1 = pol
        r1 *= r1
        r1 *= r1
        ret_val = es0 / (r1 * r1)
        return ret_val
    
    
    def tcon(self, t, d):
        """ THIS FUNCTION RETURNS THE TEMPERATURE TCON (CELSIUS) AT THE LIFTING */
            CONDENSATION LEVEL, GIVEN THE TEMPERATURE T (CELSIUS) AND THE
            DEW POINT D (CELSIUS).

            BAKER,SCHLATTER 17-MAY-1982     Original version """

        # COMPUTE THE DEW POINT DEPRESSION S.

        s = t - d;

        # THE APPROXIMATION BELOW, A THIRD ORDER POLYNOMIAL IN S AND T,
        # IS DUE TO HERMAN WOBUS. THE SOURCE OF DATA FOR FITTING THE
        # POLYNOMIAL IS UNKNOWN.

        dlt = s * (t * 0.001278 + 1.2185 + s * (s * 1.173e-5
 - 0.00219 - t * 5.2e-6))
        ret_val = t - dlt
        return ret_val
    
    def tsa(self, os, pres):
        """ Very little documentation on these following routines, so unsure 
            of the origin and derivation of these algorithms. """

        rocp = 0.28571482

        a = os # +273.16
        tq = 253.16
        d = 120.0
        
        i = 0
        for i in range(12):
            tqk = tq - 273.16
            d /= 2
            x = a * exp(- 2.6518986 * self.w(tqk, pres) / tq) - tq * pow((1000.0 / pres), rocp)            
            if (fabs(x) <= 0.0):
                break
            if x < 0.0:
                sign = - 1
            else:
                sign = 1            
            tq += (d * sign)

        return tq # -273.16
    
    def w(self, temp, pres):
        """ Very little documentation on these following routines, so unsure 
            of the origin and derivation of these algorithms. """
            
        x = self.esat(temp)
        return (622.0 * x / (pres - x))
    
    def temp_of_te(self, te, press):
        import Temp_of_te
        return Temp_of_te.temp_of_te(te,press)

    def capeFunc(self, usetv, p_dat_PPointer, tve_dat_PPointer, p0, th0, sh0):
        import CapeFunc
        return CapeFunc.capeFunc(usetv, p_dat_PPointer, tve_dat_PPointer, p0, th0, sh0)
    
    def lfcpar(self, eptpar, pcb, tcb, hcb, t1, t2, p1, ht1):
        """ his routine computes the level of free convection of a rising parcel.
        History.
         --------                    
         Don Baker      01 Jun 85    Original version.
         Dale Perry        Oct 96    Adapted code to work with WFO

         Description of input and output.
         --------------------------------
         On input:
         ---------                
         EPTPAR      Real          Moist adiabat along which parcel rises above
                                   the LCL (K).
         PCB         Real          LCL pressure (mb).
         TCB         Real          LCL temperature (K).
         HCB         Real          LCL height (m asl).
         T1          Real Array    Parcel temperatures at lifted parcel levels (K).
         T2          Real Array    Sounding temperatures at parcel levels (K).
         P1          Real Array    Lifted parcel pressure levels (mb).
         HT1         Real Array    Lifted parcel level heights (m asl).
         NPAR        Integer       Number of lifted parcel levels passed.

         On output:
         ----------               
         PLFC1        Real          Level of free convection pressure (mb).
         HLFC1        Real          Level of free convection height (m asl).
         TLFC1        Real          Level of free convection temperature (K). """
         
        lfcReturn = zeros((1, 6), 'float32')
        TOLER = 0.05
        npar = p.shape[0]
        print "npar=", npar
        # Find the location in the parcel arrays that corresponds to the LCL
        i = 0
        for ii in range(npar) :
            i = ii
            if math.fabs(p1[i] - pcb) < 0.1 :
                break
            else :
                continue
        print "found pressure at ", i
        # Initially assign flag values to the LFC in case no buoyancy exists.
        plfc1 = meteo.TOP_FLG
        hlfc1 = meteo.TOP_FLG
        tlfc1 = meteo.TOP_FLG
        plfc2 = meteo.TOP_FLG
        hlfc2 = meteo.TOP_FLG
        tlfc2 = meteo.TOP_FLG
        
        if i == npar :
            lfcReturn[0][0] = plfc1
            lfcReturn[0][1] = hlfc1
            lfcReturn[0][2] = tlfc1
            lfcReturn[0][3] = plfc2
            lfcReturn[0][4] = hlfc2
            lfcReturn[0][5] = tlfc2
            return lfcReturn
        
        # Check and see if parcel is positively buoyant at the LCL already.  If
        # this is true, then the LFC is coincident with the LCL.  This may be
        # the case in 00Z soundings when a super-adiabatic layer exists near
        # the surface.
        
        if t1[i] >= t2[i] :
            plfc1 = pcb
            hlfc1 = hcb
            tlfc1 = tcb
            lfcReturn[0][0] = plfc1
            lfcReturn[0][1] = hlfc1
            lfcReturn[0][2] = tlfc1
            lfcReturn[0][3] = plfc2
            lfcReturn[0][4] = hlfc2
            lfcReturn[0][5] = tlfc2
            return lfcReturn
        
        # Loop upward from the LCL until the parcel temperature becomes warmer
        # than the environment.  If this does not occur, no positive buoyancy
        # exists and the routine exits with whatever flag value was assigned to
        # the level of free convection.
        # To prevent a stack out of bounds error when I=1, set it equal to the
        # next level if I=1.
        
        if i == 0 : 
            i = 1
        
        runLoop = True
        print "entering loop1 with i=", i
        for j in range(i, npar) :
            if t1[j] >= t2[j] :
                pt = p1[j]
                pb = p1[j - 1]
                plog1 = math.log(p1[j])
                plog3 = math.log(p1[j - 1])
                
                print "entering inner loop1 j=", j
                for count in range(100) :
                    pm = 0.5 * (pb + pt)
                    plog2 = math.log(pm)
                    etpar = eptpar * math.pow((pm / 1000.0), 0.286)
                    t1m = self.temp_of_te(etpar, pm)
                    t2m = self.interp1(t2[j], t2[j - 1], plog1, plog2, plog3)
                    if math.fabs(t1m - t2m) <= TOLER :
                        plfc1 = pm
                        hlfc1 = self.interp1(ht1[j], ht1[j - 1], plog1, math.log(plfc1), plog3)
                        tlfc1 = t1m
                        runLoop = False;
                        print "attempting to break out of loop 1"
                        break
                    if (t1m - t2m) > TOLER :
                        pt = pm
                    if (t2m - t1m) > TOLER :
                        pb = pm
                if runLoop != True :
                    break
                else :
                    continue

        # Continue looping to find a possible second LFC per conditions
        # above rules.
        j = j + 1
        print "entering loop2 with j=", j
        for k in range(j, npar) :
            if t1[k] >= t2[k] :
                pt = p1[k]
                pb = p1[k - 1]
                plog1 = math.log(p1[k])
                plog3 = math.log(p1[k - 1])
                
                print "entering inner loop2 k=", k
                for count in range(100) :
                    pm = 0.5 * (pb + pt)
                    plog2 = math.log(pm)
                    etpar = eptpar * math.pow(pm / 1000.0, 0.286)
                    t1m = self.temp_of_te(etpar, pm)
                    t2m = self.interp1(t2[k], t2[k - 1], plog1, plog2, plog3)
                    if math.fabs(t1m - t2m) <= TOLER :
                        plfc2 = pm
                        hlfc2 = self.interp1(ht1[k], ht1[k - 1], plog1, math.log(plfc2, plog3))
                        tlfc2 = t1m
                        lfcReturn[0][0] = plfc1
                        lfcReturn[0][1] = hlfc1
                        lfcReturn[0][2] = tlfc1
                        lfcReturn[0][3] = plfc2
                        lfcReturn[0][4] = hlfc2
                        lfcReturn[0][5] = tlfc2
                        print "exiting loop2 k=", k
                        return lfcReturn
                    if (t1m - t2m) > TOLER :
                        pt = pm
                    if (t2m - t1m) > TOLER :
                        pb = pm
                            
        lfcReturn[0][0] = plfc1
        lfcReturn[0][1] = hlfc1
        lfcReturn[0][2] = tlfc1
        lfcReturn[0][3] = plfc2
        lfcReturn[0][4] = hlfc2
        lfcReturn[0][5] = tlfc2
        return lfcReturn
                        
    def richno(self, ht, hw, uw, vw, rho, buoy):
        """ Statement of purpose.
        Compute the dimensionless bulk Richardson number as defined by
        Weisman and Klemp (1982).
        History.
        --------                    
        Tom Schlatter  Late 1982    Original code based on MWR article by 
                                     Weisman and Klemp (1982).
        D. Baker       01 Jun 84    Removed computation of positive energy...
                                     made it an input argument.
        D. Baker       01 Jul 85    Updated code for documentation.
        J. Ramer       16 Jun 92    Added divide-by-zero prevention.
        D. Perry       10 Oct 96    Adapted code for WFO    

        Description of input and output.
        --------------------------------
        On input:
        ---------                
        HT              Sounding heights (m asl).
        HW              Heights of wind reports (m asl).
        UW              Wind u-components (m/s).
        VW              Wind v-components (m/s).
        RHO             Air density at each sounding level (kg/m**3).
        BUOY            Positive buoyant energy (J/kg).

        On output:
        ----------               
        RICHNUM         Dimensionless bulk Richardson number. """
    
    
        mnl = 500
        nlvls = ht.shape[0]
        nw = uw.shape[0]
        HALFKM = 500.0
        SIXKM = 6000.0
        richnum = meteo.MISSING
        rhow = rho
        # Interpolate an air density value to each reported wind level
        if nlvls != nw :
            rhow = self.wndrho(rho, ht, hw)
        else :
            for i in range(nlvls) :
                rhow[i] = rho[i]
        
        # QC
        qc = 1
        for i in range (2, nw) :
            if uw[i] != uw[0] and vw[i] != vw[0] :
                qc = 0
        
        if nlvls < 3 or nlvls > 500 :
            qc = 1
            
        for i in range(nw) :
            if rhow[i] <= 0.0 :        
                qc = 1
                break
        
        for i in range(2, nw) :
            if (hw[i] - hw[i - 1]) <= 0.0 :
                qc = 1
                break
        
        for i in range(2, nlvls) :
            if (ht[i] - ht[i - 1]) <= 0.0 :
                qc = 1
        
        if qc == 1 :
            return richnum
    
        # initialize sums
    
        sumu = 0
        sumv = 0
        sumr = 0
        sumul = 0
        sumvl = 0
        sumrl = 0
        
        # define shear layer bounds (above ground level)
        hbl = hw[0] + HALFKM
        htop = hw[0] + SIXKM
        
        if hw[nw] < htop or hw[1] > htop :
            return richnum
        
        # Loop to calculate shear terms
        
        i = 0
        rulay = 0.5 * (rhow[i] * uw[i])
        rvlay = 0.5 * (rhow[i] * vw[i])
        rlay = 0.5 * rhow[i]
        dz = hw[i]
        
        for i in range(1, nw) :
            rulay = 0.5 * (rhow[i] * uw[i] + rhow[i - 1] * uw[i - 1])
            rvlay = 0.5 * (rhow[i] * vw[i] + rhow[i - 1] * vw[i - 1])
            rlay = 0.5 * (rhow[i] + rhow[i - 1])
            dz = hw[i] - hw[i - 1]
            if hw[i] > htop :
                break
            sumu = sumu + rulay * dz
            sumv = sumv + rvlay * dz
            sumr = sumr + rlay * dz
            if hw[i] > hbl and i > 1 :
                sumul = sumul + rulay * dz
                sumvl = sumvl + rvlay * dz
                sumrl = sumrl + rlay * dz
        
        sumu = sumu + rulay * dz
        sumv = sumv + rvlay * dz
        sumr = sumr + rlay * dz
        
        if sumr <= 0.0 :
            u6 = 0.0
            v6 = 0.0
        else : 
            u6 = sumu / sumr
            v6 = sumv / sumr
    
        if sumrl <= 0.0 :
            ul = 0.0
            vl = 0.0
        else :
            ul = sumul / sumrl
            vl = sumvl / sumrl
        
        # calculate one half the square of the shear vector in the lowest 6 km
        u6 = u6 - ul
        v6 = v6 - vl
        ske = 0.5 * (u6 * u6 + v6 * v6)
        
        # compute the bulk richardson number
        
        if ske > 0 :
            richnum = buoy / ske
        
        return richnum
    
    def wndrho(self, rho, ht, hw):
        """ PURPOSE:
            --------
            INTERPOLATE TO DETERMINE DENSITY AT WIND LEVELS GIVEN DENSITY AT
            PRESSURE LEVELS IN A SOUNDING.  INTERPOLATION IS LINEAR BY HEIGHT.

            T. Schlatter    late 82         Probable original author.
            D. Baker        17 Dec 85       Added doc and indentation (?)
            D. Baker (?)    after Dec 85    Replaced 100 loop with 300 loop.  It
                                             appears that the interpolation is out.
            J. Wakefield    17 Nov 92       Added parameter list documentation.
            D. Perry        Sep 96          Adapted code to work with WFO.

            Argument    I/O                Description
            --------    ---        -----------------------------------------------
            Rho          I        Density (kg m-3) at sounding levels.
            Ht           I        Heights (m) at sounding levels.
            NLvls        I        Number of sounding levels.
            HW           I        Heights (m) of wind obs.
            NW           I        Number of wind obs.
            RhoW         O        Density interpolated to wind obs heights. """
        
        
        # Interpolate to derive density at wind heights
        j = 0
        nw = len(hw)
        skip = False
        for i in range(nw) :
            if skip == True :
                break
            k = j
            for j in range(k, nlvls - 1) :
                if hw[i] >= ht[j] and hw[i] <= ht[j + 1] :
                    rhow[i] = self.interp1(rho[j], rho[j + 1], ht[j], hw[i], ht[j + 1])
                    skip = True
                    break
        
        rhow[0] = rho[0]
        k1 = 0
        k2 = 1
        
        for i in range(1, nw) :
            if ht[k2] < hw[i] :
                k1 = k2
                k2 = k2 + 1
                if k2 > nlvls :
                    for j in range(i, nw) :
                        rhow[j] = rho[k1]
                        return rhow
        
        rhow[i] = self.interp1(rho[k1], rho[k2], ht[k1], hw[i], ht[k2])
        
        return rhow
    
    def lclpar(self, meanmix, ts, p, ht, t, td):
        """ Statement of purpose.
        ---------------------
        This routine computes the pressure, height, and temperature of the
        lifting condensation level (LCL) from a sounding.
        
        History.
        --------                    
        Dale Perry   20 Sep 96    Bootlegged version of cclpar.f modified for
                                  determining the LCL.
        
        Description of input and output.
        --------------------------------
        On input:
        ---------                
        MEANMIX      Mixing ratio used to intersect the sounding (g/kg).
        TS           Surface temp (12Z-forecast max temp;00Z-sfc temp) (K). 
        P            Sounding pressures (mb).
        HT           Sounding heights (m asl).
        T            Sounding temperatures (K).
        TD           Sounding dewpoint temperatures (K).
        
        On output:
        ----------               
        PLCL         Pressure of the lifting condensation level (mb).
        TLCL         Temperature of the lifting condensation level (K).
        HTLCL        Height of the lifting condensation level (m asl).
        
        User notes:
        -----------
        The low level mean mixing ratio is input to this routine...
        computed outside. """

        TOLER = 0.5
        nlvls = len(p)
        lfcReturn = zeros((1, 3), 'float32')
        
        # Loop up through sounding until mixing ratio line corsses the dry 
        # adiabat through the surface temperature.  Initially set the LCL
        # parameters to MISSING values in case no LCL is found
            
        plcl = meteo.TOP_FLG
        hlcl = meteo.TOP_FLG
        tlcl = meteo.TOP_FLG
        t2 = ts * math.pow(1000.0 / p[0], 0.286)
        
        for i in range(nlvls) :
            t1 = self.temp_mixratio(p[i], meanmix)
            t1 = t1 * math.pow(1000.0 / p[i], 0.286)
            if t1 >= t2 :
                break
        
        if i == 1 : #LCL at the surface
            plcl = p[0]
            hlcl = ht[0]
            tlcl = t[0]
            lfcReturn[0][0] = plcl
            lfcReturn[0][1] = hlcl
            lfcReturn[0][2] = tlcl
            return lfcReturn
        
        # We were at the top of the sounding, but 'I' got incremented one more
        # beyond.  Reset it to the top of the sounding index 'NLVLS'
        if i > nlvls :
            i = nlvls - 1
        
        pt = p[i]
        pb = p[i - 1]
        plog1 = math.log(p[i])
        plog3 = math.log(p[i - 1])
        
        # Iterate to find the LCL.  Keep cutting level in half until the point
        # of intersection is found
        
        for count in range(100) :
            pm = 0.5 * (pt + pb)
            plog2 = math.log(pm)
            t1 = self.temp_mixratio(pm, meanmix)
            t1 = t1 * math.pow(1000.0 / pm, 0.286)
            if math.fabs(t1 - t2) <= TOLER :
                plcl = pm
                tlcl = t1 * math.pow(plcl / 1000.0, 0.286)            
                hlcl = self.interp1(ht[i], ht[i - 1], plog1, math.log(plcl), plog3)
                lfcReturn[0][0] = plcl
                lfcReturn[0][1] = hlcl
                lfcReturn[0][2] = tlcl
                return lfcReturn 
            if (t1 - t2) > TOLER :
                pt = pm
            if (t2 - t1) > TOLER :
                pb = pm
        
        lfcReturn[0][0] = plcl
        lfcReturn[0][1] = hlcl
        lfcReturn[0][2] = tlcl
        return lfcReturn
        
#    def posarea(self,plfc,peqlev,tlfc,teqlev,hlfc,heqlev,eptpar,p,ht,te,tp):
#        NL = 500
#        
#        cin = 0.0
#        buoy = meteo.FLG
#        idx1 = 0
#        npar = len(p)
#        
#        for i in range(npar):
#            if p[i] < plfc :
#                idx1 = i - 1
#                break
#        
#        peq = peqlev
#        heq = heqlev
#        teq = teqlev
#        
#        if peqlev > -meteo.FLG :
#            peq = p[npar - 1]
#            heq = ht[npar - 1]
#            teq = te[npar - 1]
#            
#        for j in range(npar) :
#            if p[j] < peq :
#                idx2 = j
#                idx2 = npar - 1
#                break
#    
#        pl[0] = plfc
#        htl[0] = hlfc
#        tel[0] = tlfc
#        
#        nparl = 0
#        for i in range(idx1 + 1, idx2 - 1) :
#            nparl = nparl + 1
#            pl[nparl] = p[i]
#            htl[nparl] = ht[i]
#            tel[nparl] = te[i]
#        
#        nparl = nparl + 1
#        pl[nparl] = peq
#        htl[nparl] = heq
#        tel[nparl] = teq
#        
#        vdif = (heq-hlfc) / 25
#        
#        
#    
#    
#    def intpos(self,vdif) :
#        j = 1
#        #for i in range(j)
#        
#    
#    def negarea(self,plfc,tlfc,hlfc,eptpar,p,ht,te,tp):
#        NL = 500
#        nparl = 0
#        npar = len(p)
#        
#        for i in range(npar) :
#            if p[i] > plfc : 
#                pl[i] = p[i]
#                htl[i] = ht[i]
#                tel[i] = te[i]
#            else :
#                pl[i] = plfc
#                htl[i] = hlfc
#                tel[i] = tlfc
#                nparl = i
#                break
#        
#        vdif = (hlfc - ht[0]) / 25
#        if vdif < 35 :
#            vdif = 35
#        
#        if nparl > 1 and hlfc - ht[0] > 0 :
#            #call intpos
#        

##                                                                      
