##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ExTool1
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

    def execute(self, QPF):
        "Tool to return QPF multiplied by 10"

        # Determine new value
        SnowAmt = QPF * 10

        # Return the new value
        return SnowAmt
