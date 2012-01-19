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
# Interface for retrieving combinations
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/08/08                      mnash       Initial Creation.
#    
# 

import MeteoLibTools
import ForecastConstants as fore
import MeteoLibConstants as meteoCons
import math
from numpy import *

class Forecast:
   
    def __init__(self):
        self.__mete = MeteoLibTools.MeteoLibTools()
   
    def forecast(self, yr, mon, day, hr, minute, stnId, snow, slat, slon, p, h, t, td):
        
        #Exit if we dont have a lat/lon
        if slat < -90.0 or slat > 90.0 or slon < -180.0 or slon > 180.0 :
            forecastMaxTemp = t[0][0]
            return forecastMaxTemp
                
        hour = hr + (minute / 60.0)
        
        # Compute the julian day
        julday = self.__mete.cv_date2jul(yr,mon,day)
        
        if stnId not in fore.TMAX.keys():
            stnId = 'MISC'
        
        #Determine diurnal range (degrees celsius)        
        trng = self.__mete.drange(fore.TMAX.get(stnId)[mon], fore.TMIN.get(stnId)[mon])
        
        tstart = hour + (slon/15)
        
        decl = fore.SDECMX * math.sin(2 * math.pi * (julday - fore.VEDAY) / fore.YRLEN)
        
        hilat = 0
        
        # Check for high latitude conditions where the sun never rises nor sets.
        # If so, tsrad is assumed to be undefined, and set to 0.
        
        # Define the angle of maximum sun height
        max_alt = (1.570796327 - abs(slat) * math.pi / 180) + decl
        
        # Define the angle of minimum sun height
        min_alt = (abs(slat) * math.pi / 180) - (1.570796327 - decl)

        if max_alt < 0.0 or min_alt > 0.0 :
            tsrad = 0.0
            hilat = 1
        else :
            haset = math.acos(math.tan(slat * math.pi / 180) * math.tan(decl))
            tset = 12 + 12 * (haset / math.pi)\
            # The solar input is allowed until 2 hours before sunset...subtract 2 hours.
            tstop = tset - 2
            t_julday = julday
            t_mon = mon
            t_slat = slat
            
            tsrad = self.__mete.solax(t_julday,t_mon,t_slat,fore.TYMINC,tstart,tstop)
            
        tc = t - meteoCons.KELVIN_ZERO
        tdc = td - meteoCons.KELVIN_ZERO
                
        thePressures, theHeights, theTemperatures, theDewpoints = self.__mete.eqp(fore.DELTAP, p, h, tc, tdc)
        
        sfcp = thePressures[0]
        sfct = theTemperatures[0]
        sfctd = theDewpoints[0]

        
        albdhi = fore.ALBMAX.get(stnId) / 100
        albdlo = fore.ALBMIN.get(stnId) / 100
        
        albdo = albdlo + 0.2 * snow * (albdhi - albdlo)
        
        if albdo > albdhi :
            albdo = albdhi
        
        endlvl = array([0,0,0])
        endlvl[0] = sfcp - 150
        endlvl[1] = sfcp - 300
        endlvl[2] = sfcp - 500
        
        mrh = self.__mete.rhbar(endlvl,fore.NCLYR,sfcp,thePressures,theTemperatures,theDewpoints)
        
        tcld = 0.0
        cover = []
        for i in range(fore.NCLYR): 
            cover.insert(i, 0.0001 * mrh[i] * mrh[i])
            tcld = max(tcld,cover[i])
        
        stemp = sfct + (0.5 * trng) + meteoCons.KELVIN_ZERO
        
        ulw = fore.EMIS * meteoCons.STEFMAN_BOLTZMANN_CONSTANT * (math.pow(stemp,4))
        
        a=0.61
        b=0.05
        
        vpmm = 0.75 * self.__mete.esat(sfctd)
        dlwclr = ulw * (a + b * math.sqrt(vpmm))
        
        cldcor = 1-0.01 * (fore.CL *mrh[0] + fore.CM *mrh[1] + fore.CH * mrh[2])
        if cldcor < 0.0 :
            cldcor = 0.0
            
        flw = (ulw - dlwclr) * cldcor
        
        if hilat == 1 :
            blw = flw
        else :
            blw = flw * (tstop - tstart) * 60
        
        
        sw = (1-albdo) * (1-tcld) * tsrad
        
        heat = sw - blw
        if heat <= 0.0 :
            forecastMaxTemp = sfct + 273.15
            return forecastMaxTemp
        
        cnv = fore.CP * fore.DELTAP * (fore.CMBD / meteoCons.GRAVITY)
        asol = heat / cnv
        deltaz = []
        for i in range(1,len(t[0])-1) :
            deltaz.insert(i-1,(h[0][i] - h[0][i-1]))
        forecastMaxTemp = self.__mete.mxtp(asol,fore.DELTAP,sfcp,thePressures[1],theTemperatures,deltaz)
        forecastMaxTemp = forecastMaxTemp + 273.15        
        return forecastMaxTemp