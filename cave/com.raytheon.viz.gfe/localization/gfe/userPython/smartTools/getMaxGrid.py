##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# getMaxGrid - generic smart tool to get the max value of all grids of the
#            element defined by varDict["Element"] into the active element.
#            The active element should span multiple grids of
#            varDict["Element"]. This tool is designed to be
#            called by a procedure and not run interactively
#
# Author: Paul Jendrowski WFO Blacksburg, VA (RNK)
#         paul.jendrowski@noaa.gov
# Version: 1.0    Date: 02/21/2003
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

ToolType = "numeric"
WeatherElementEdited = "variableElement"
from numpy import *

# You can screen the elements for which your tool will appear by using
# a ScreenList.  For example:
#
# This tool is normally run from a procedure so hide it!
ScreenList = [""]

# Set up Class
import SmartScript
## For available commands, see SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, GridTimeRange, varDict):
        "Gets max value of another set of grids. This tool should be run from a procedure and not interactively."

        if varDict is None or not varDict.has_key("Element"):
            msg="getMaxGrid - No element defined."
            msg += " This tool should not be run interactively!"
            self.noData(msg)
        we = varDict["Element"]
        grid = self.getGrids("Fcst",we,"SFC",GridTimeRange,mode="Max")

        # Return the new value
        return grid

