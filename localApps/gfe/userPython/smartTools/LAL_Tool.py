# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# LAL_Tool - Uses weather, pop and qpf to assign LAL
#
#           No local configuration is required for this tool
#
# Author: Brian Brong 8-22-02
#
# Ported to AWIPS II by Tom LeFebvre
#
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "LAL"
from numpy import *
HideTool = 0
from WxMethods import *

# Set up Class
import SmartScript

class Tool(SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, Wx, PoP, QPF, LAL, varDict):
        "Assigns LAL based on Wx, PoP, and QPF"
        LAL = 1

        # QPF mask for LAL 6
        qpfmask = less(QPF, 0.10)

        # Wx mask for Thunder in the Wx grids
        wxmask = self.wxMask(Wx, ":T:")

        #dry thunderstorm Mask
        dryTmask = logical_and(qpfmask, wxmask)
        
        print "Dry mask has", sum(sum(dryTmask)), "points"
        
        # Masks for LAL values 2-6 based on PoP and wxmask
        # Trouble with using the Wx grid is GFE does not understand
        # Widely Scattered for LAL 3.  Will use a PoP range 20 to 35 for LAL = 3
        lal2 = logical_and(wxmask, logical_and(greater_equal(PoP, 10), less(PoP, 20)))
        lal3 = logical_and(wxmask, logical_and(greater_equal(PoP, 20), less(PoP, 35)))
        lal4 = logical_and(wxmask, logical_and(greater_equal(PoP, 35), less(PoP, 60)))
        lal5 = logical_and(wxmask, greater_equal(PoP, 60))                  
        lal6 = logical_and(dryTmask, greater_equal(LAL,3))

        #  Assign LAL values 2-5 based on LAL masks
        LAL = where(lal2, 2, where(lal3, 3, where(lal4, 4, where(lal5, 5, LAL))))

        # Assign LAL 6 where dryTmask and LAL >= 3
        LAL[lal6] = 6

        # Return the new value
        return LAL
