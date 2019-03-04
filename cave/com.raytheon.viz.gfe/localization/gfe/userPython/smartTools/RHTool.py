##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# RHTool
#
# Author:
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

ToolType = "numeric"
WeatherElementEdited = "RH"
from numpy import *

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, T, Td):
        "This smart tool uses temp and dew pt to derive RH"

        # Determine new value
        Tc = .556 * (T - 32.0)
        Tdc = .556 * (Td - 32.0)

        Vt = 6.11 * pow(10,(Tc * 7.5 / (Tc + 237.3)))
        Vd = 6.11 * pow(10,(Tdc * 7.5 / (Tdc + 237.3)))

        RH = (Vd / Vt) * 100.0


        # Return the new value
        return RH


