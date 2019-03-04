##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# SnowAmt_fm_SnowRatio_and_QPF
#
# Author: Brian Meade, GRR, 12/07
# ----------------------------------------------------------------------------
# short tool which simply computes SnowAmt by multiplying SnowRatio times QPF.
# SnowRatio is a local grid which is created by a smartInit which applies
# the Cobb Snow Algoritm
#-----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

ToolType = "numeric"
WeatherElementEdited = "SnowAmt"
from numpy import *

# Set up Class
import SmartScript


class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)


    def execute(self, QPF, T, SnowRatio, SnowAmt, varDict, GridTimeRange):
 
        snowfall = QPF * SnowRatio
        snowfall = where(greater(T, 32.0), pow(36.0 - T,2)/16.0 * snowfall , snowfall) 
        snowfall[greater(T, 35.0)] = 0.0

        # Return the new value
        return snowfall

       
