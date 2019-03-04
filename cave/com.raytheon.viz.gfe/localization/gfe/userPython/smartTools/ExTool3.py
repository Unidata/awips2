##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ExTool3
#
# Author:
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "SnowAmt"
from numpy import *
import SmartScript

VariableList = [
        ("Enter elevation" , 5000, "numeric"),
       ]
class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, QPF, T, Topo, varDict):
        "Tool to calculate SnowAmt"

        # Set up Variables from the varDict
        elevation = varDict["Enter elevation"]

        SnowAmt = where(less(T, 20), QPF * 18,
                               where(less(T, 25), QPF * 14,
                               QPF * 10))
        SnowAmt = where(less(Topo, elevation), float32(0), QPF)
        # Return the new value
        return SnowAmt
