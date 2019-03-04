##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# MaxRH_Tool
#
# Author:
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

ToolType = "numeric"
WeatherElementEdited = "MaxRH"
from numpy import *

# You can screen the elements for which your tool will appear by using
# a ScreenList.  For example:
#

# Set up Class
import SmartScript
## For available commands, see SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, GridTimeRange):
        "Returns the maximum RH over the selected timeRange"
        
        # Fetch the maximum over the GridTimeRange
        MaxRH = self.getGrids("Fcst", "RH", "SFC", GridTimeRange, 'Max', 0)

        # Return the new value
        return MaxRH

