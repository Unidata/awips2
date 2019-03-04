##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ExSS7
#
# Author:
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "Wx"
from numpy import *

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, PoP, Wx):
        # Assign Wx based on PoP

        # Separate Wx into components
        wxValues = Wx[0].copy()
        keys = Wx[1]

        wxValues[less(PoP, 20)] = self.getIndex("<NoCov>:<NoWx>:<NoInten>:<NoVis>:",keys)
        wxValues[logical_and(greater_equal(PoP, 20), less(PoP, 35))] = self.getIndex("Chc:R:-:<NoVis>:" ,keys)
        wxValues[logical_and(greater_equal(PoP, 35), less(PoP, 55))] = self.getIndex("Sct:RW:m:<NoVis>:" ,keys)
        wxValues[greater_equal(PoP, 55)] = self.getIndex("Wide:R:+:<NoVis>:" ,keys)

        return (wxValues, keys)
