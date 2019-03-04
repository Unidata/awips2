##
##

# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TC_WindGust_Tool
#
# Author: G. RADER
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

ToolType = "numeric"
WeatherElementEdited = "WindGust"
from numpy import *

import SmartScript
## For available commands, see SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, Wind):
        "Calculates Wind Gusts with "

        magnitude = Wind[0]

        WindGust = self.empty()
        WindGust = where(greater_equal(magnitude,  0.0), magnitude*1.40, WindGust)
        WindGust = where(greater_equal(magnitude, 46.0), magnitude*1.38, WindGust)
        WindGust = where(greater_equal(magnitude, 47.0), magnitude*1.36, WindGust)
        WindGust = where(greater_equal(magnitude, 48.0), magnitude*1.34, WindGust)
        WindGust = where(greater_equal(magnitude, 49.0), magnitude*1.32, WindGust)
        WindGust = where(greater_equal(magnitude, 50.0), magnitude*1.30, WindGust)
        WindGust = where(greater_equal(magnitude, 62.0), magnitude*1.28, WindGust)
        WindGust = where(greater_equal(magnitude, 63.0), magnitude*1.26, WindGust)
        WindGust = where(greater_equal(magnitude, 64.0), magnitude*1.24, WindGust)
        WindGust = where(greater_equal(magnitude, 65.0), magnitude*1.23, WindGust)
        
        return WindGust

