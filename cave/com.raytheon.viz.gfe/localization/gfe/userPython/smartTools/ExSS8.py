##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ExSS8
#
# Author:
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "PoP"
from numpy import *

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, PoP, Wx):
         # Assign PoP based on Wx

          PoP[self.wxMask(Wx, "<NoCov>:")] = 0

          # Here we need to require a regular expression to avoid confusion between "Chc" and "SChc" and "Sct" and "WSct"
          PoP[self.wxMask(Wx, "^Chc:", 1)] = 25
          PoP[self.wxMask(Wx, "^Sct:", 1)] = 55

          PoP[self.wxMask(Wx, "Wide:")] = 80

          return PoP
