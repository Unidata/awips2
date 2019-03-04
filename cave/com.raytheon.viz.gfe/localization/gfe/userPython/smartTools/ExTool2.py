##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ExTool2
#
# Author:
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "SnowAmt"
from numpy import *
import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, QPF, T):
        "Tool to calculate SnowAmt"

        SnowAmt = where(less(T, 20), QPF * 40,
                               where(less(T, 25), QPF * 20,
                               QPF * 10))
        # Return the new value
        return SnowAmt
