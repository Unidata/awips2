# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# VentRate
#
# Author:
#
# Ported to AWIPS II by Tom LeFebvre
#
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "VentRate"
from numpy import *
HideTool = 0

# Set up Class
import SmartScript

class Tool(SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, MixHgt, TransWind):
        "Calculates ventilation rate from Mixing Height and Transport Wind."

        # Determine new value
        TransWndMag = TransWind[0]
        VentRate = TransWndMag * MixHgt

        # Return the new value
        return VentRate





#
## ----------------------------------------------------------------------------
## This software is in the public domain, furnished "as is", without technical
## support, and with no warranty, express or implied, as to its usefulness for
## any purpose.
##
## VentRate
##
## Author:
## ----------------------------------------------------------------------------
#
#ToolType = "numeric"
#WeatherElementEdited = "VentRate"
#from Numeric import *
#HideTool = 0
#
## Set up Class
#import SmartScript
### For available commands, see SmartScript
#
#class Tool (SmartScript.SmartScript):
#    def __init__(self, dbss):
#        SmartScript.SmartScript.__init__(self, dbss)
#
#    def execute(self, MixHgt, TransWind):
#        "Calculates ventilation rate from Mixing Height and Transport Wind."
#
#        # Determine new value
#        TransWndMag = TransWind[0]
#        VentRate = TransWndMag * MixHgt
#
#        # Return the new value
#        return VentRate
