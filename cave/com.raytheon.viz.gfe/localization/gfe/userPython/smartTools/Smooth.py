##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Smooth.py
#
# Author: njensen / chammack
# ----------------------------------------------------------------------------

##
# This is a base file that is not intended to be overridden.
##

ToolType = "numeric"
WeatherElementEdited = "variableElement"
from numpy import *
import math

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, WEname, editArea, GridTimeRange):
        parm = self.getParmByExpr(WEname)
        time = GridTimeRange.startTime().javaDate()

        parm.smooth(time, editArea)
        return None
