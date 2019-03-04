##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
# 
# WindChillTool.py
#
# Author: Dankers, WFO Boulder, August 2001
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

WeatherElementEdited = "WindChill"
ToolType = "numeric"
from numpy import *
from math import *
import SmartScript


class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, T, Wind):
        "This tool derives the Wind Chill index from Temp and Wind Speed"
        
        mag = Wind[0] * 1.15
        WindChill = where(less_equal(mag, 3), T, 35.74 + (0.6215 * T) - 
                   (35.75 * (mag ** 0.16)) + (0.4275 * T * (mag ** 0.16)))
        
        # clip values where WindChill > T
        WindChill = where(greater(WindChill, T), T, WindChill)
        
        # substitute the temperature if WindChill >= 51 degrees
        WindChill = where(greater_equal(T, 51), T, WindChill)

        return WindChill
 
