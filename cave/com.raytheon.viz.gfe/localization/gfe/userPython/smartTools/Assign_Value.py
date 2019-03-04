##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Assign_Value.py
#
# Author: njensen
# ----------------------------------------------------------------------------

##
# This is a base file that is not intended to be overridden.
##

ToolType = "numeric"
WeatherElementEdited = "variableElement"
from numpy import *

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, variableElement, variableElement_PickUpValue):
        "Assign the value for the Active Weather Element within the Active Edit Area."
        
        if type(variableElement_PickUpValue) is list:            
            # for vector data
            variableElement[0].fill(variableElement_PickUpValue[0])
            variableElement[1].fill(variableElement_PickUpValue[1])
        elif type(variableElement_PickUpValue) is str:
            # for discrete/wx data            
            newval = self.getIndex(variableElement_PickUpValue, variableElement[1])
            variableElement[0].fill(newval)
        else:
            # for scalar data
            variableElement.fill(variableElement_PickUpValue)
        return variableElement
    