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
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Apr 24, 2018  7275     randerso  Re-implemented based on NWS_ENPWAVE from NIC
#                                  and calcWind from WW3. Code cleanup.
##
##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
##

from Init import Forecaster
import numpy as np

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from WW3 model
## output.
##
##--------------------------------------------------------------------------
class ENPwaveForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "ENPwave", "ENPwave")

    # From: ../../methods/calcPeriod/perpw.calcPeriod.py
    #===========================================================================
    #  Calculate Primary Period
    #===========================================================================
    def calcPeriod(self, perpw_SFC):
        return perpw_SFC.clip(0 , 60)

    # From: ../../methods/calcPeriod2/persw.calcPeriod2.py
    #===========================================================================
    #  Calculate Secondary Period
    #===========================================================================
    def calcPeriod2(self, persw_SFC):
        return persw_SFC.clip(0 , 60)

    # From: ../../methods/calcSwell/dirpw.calcSwell.py
    #==============================================================================
    #  Calculates the Primary Swell from ENP model.
    #    Note: have to use wave height for magnitude since that info not available.
    #==============================================================================
    def calcSwell(self, WaveHeight, dirpw_SFC):
        # extract the wind speed and direction
        magnitude = WaveHeight
        direction = dirpw_SFC.clip(0, 359.5)
        return (magnitude, direction)

    # From: ../../methods/calcSwell2/dirsw.calcSwell2.py
    #==============================================================================
    #  Calculates the Secondary Swell from ENP model.
    #    Note: have to use wave height for magnitude since that info not available.
    #==============================================================================
    def calcSwell2(self, WaveHeight, dirsw_SFC):
        # extract the wind speed and direction
        magnitude = WaveHeight
        direction = dirsw_SFC.clip(0, 359.5)
        return (magnitude, direction)

    # From: ../../methods/calcWaveHeight/htsgw.calcWaveHeight.py
    #===========================================================================
    #  Calculates wave height from the ENP model.  Converts to feet from meters
    #===========================================================================
    def calcWaveHeight(self, htsgw_SFC):

        return htsgw_SFC * 3.281

    #===========================================================================
    #  Calculates wind. Convert from meters/sec to knots.
    #===========================================================================
    def calcWind(self, wind_SFC):
        # extract the wind speed and direction
        magnitude = wind_SFC[0].copy()
        direction = wind_SFC[1].copy()

        # mask out invalid values
        mask = np.greater(wind_SFC[0], 100)
        magnitude[mask] = 0
        direction[mask] = 0

        # convert to kts and clip direction
        magnitude *= 1.94
        direction.clip(0, 359.5, direction)
        return (magnitude, direction)


def main():
    ENPwaveForecaster().run()       