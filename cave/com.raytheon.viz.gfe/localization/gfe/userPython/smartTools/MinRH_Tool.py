##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# MinRH_Tool
#
# Author:
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

ToolType = "numeric"
WeatherElementEdited = "MinRH"
from numpy import *

# Set up Class
import SmartScript
## For available commands, see SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)


    def execute(self, GridTimeRange):
        "Returns the minimum RH over the selected timeRange"

        MinRH = self.getGrids("Fcst", "RH", "SFC", GridTimeRange, 'Min', 0)

        return MinRH

