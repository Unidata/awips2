##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# AdjustValue_Up.py
#
# Author: njensen
# ----------------------------------------------------------------------------

##
# This is a base file that is not intended to be overridden.
##

ToolType = "numeric"
WeatherElementEdited = "variableElement"
# All weather elements except Hazards and Wx
ScreenList = ["SCALAR", "VECTOR"]
from numpy import *

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, variableElement, variableElement_DeltaValue):
        "Adjusts Value Up using Delta Value within Active Edit Area."
                
        if type(variableElement) is list:
            # vector magnitude
            veMode = self.getVectorEditMode()
            if "Both" == veMode or "Magnitude Only" == veMode:
                variableElement[0] = variableElement[0] + variableElement_DeltaValue
            if "Direction Only" == veMode:
                variableElement[1] += float32(10.0)
                variableElement[1] %= 360
        else:        
            # scalar
            variableElement = variableElement + variableElement_DeltaValue
            
        return variableElement
    