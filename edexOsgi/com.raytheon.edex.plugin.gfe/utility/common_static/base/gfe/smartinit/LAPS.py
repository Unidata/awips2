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
# Mar 29, 2018  7063     randerso  Updated to reflect changes in LAPS parameter
#                                  names. Code cleanup.  
#
##

##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
##

# LAPS Smart Init module

import Init
import numpy as np

class LAPSForecaster(Init.Forecaster):

##-------------------------------------------------------------------------
##  ifpInit module to calculate surface weather values from LAPS analysis
##-------------------------------------------------------------------------
    def __init__(self):
        Init.Forecaster.__init__(self, "LAPS", "LAPS")

##-------------------------------------------------------------------------
##  Converts LAPS surface temp from K to F
##-------------------------------------------------------------------------
    def calcT(self, t_SFC):
        return self.KtoF(t_SFC)

##-------------------------------------------------------------------------
##  Converts LAPS surface dew point temp from K to F
##-------------------------------------------------------------------------
    def calcTd(self, dpt_SFC):
        return self.KtoF(dpt_SFC)

##-------------------------------------------------------------------------
## Convert LAPS surface wind speed from m/x to knots
##-------------------------------------------------------------------------
    def calcWind(self, wind_SFC):
        magnitude = wind_SFC[0]
        direction = wind_SFC[1]
        magnitude = magnitude * 1.94   # convert m/s to knots
        direction = np.clip(direction, 0, 359.5)
        return (magnitude, direction)

##-------------------------------------------------------------------------
## Uses LAPS skyCover grid to make IFP sky grids
##-------------------------------------------------------------------------
    def calcSky(self, ccov_SFC):
        return np.clip(ccov_SFC, 0, 100)

##-------------------------------------------------------------------------
## Uses LAPS one-hour snowAmt grids to initialize SnowAmt
##-------------------------------------------------------------------------
    def calcSnowAmt(self, totsn_SFC):
        return totsn_SFC * 3.28 * 12

##-------------------------------------------------------------------------
## Uses LAPS precip grids to initialize QPF
##-------------------------------------------------------------------------
    def calcQPF(self, tp_SFC):
        return tp_SFC * 3.28 * 12

##-------------------------------------------------------------------------
## Return the LAPS low level radar analysis
##-------------------------------------------------------------------------
    def calcRadar(self, llr_SFC):
        return llr_SFC

##-------------------------------------------------------------------------
## Uses LAPS weather grids to initialize weather
##-------------------------------------------------------------------------
    def calcWx(self, ptyp_SFC):
        wx = ptyp_SFC.astype(np.int8)
        outOfAreaMask = np.logical_or(np.greater(wx, 7),np.less(wx, 0))
        wx[outOfAreaMask] = 0
        return (wx, self._getWxKey())
        
##-------------------------------------------------------------------------
## Internal function that defines the weather key
##-------------------------------------------------------------------------
    def _getWxKey(self):
        key = ['<NoCov>:<NoWx>:<NoInten>:<NoVis>:',
               "Wide:R:-:<NoVis>:", "Wide:S:-:<NoVis>:",
               "Wide:ZR:-:<NoVis>:", "Wide:IP:-:<NoVis>:",
               "Def:H:<NoInten>:<NoVis>:", "Wide:L:-:<NoVis>:",
               "Wide:ZL:-:<NoVis>:"]
        return key


def main():
    LAPSForecaster().run()
