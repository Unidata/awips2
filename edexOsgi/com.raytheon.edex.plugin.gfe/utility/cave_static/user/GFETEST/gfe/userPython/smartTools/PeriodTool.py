
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# PeriodTool
#
# Author:
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "Period"
from numpy import *

# You can screen the elements for which your tool will appear by using
# a ScreenList.  For example:
#
#ScreenList = ["T","Td"]
#ScreenList = ["SCALAR","VECTOR","WEATHER","DISCRETE"]

### If desired, Set up variables to be solicited from the user:
##VariableList = [
##         ("Variable name1" , defaultValue1, "numeric"),
##         ("Variable name2" , "default value2", "alphaNumeric"),
##         ("Variable name3" , ["default value1", "default value2"], "check",
##                       ["value1", "value2", "value3"]),
##         ("Variable name4" , "default value4", "radio",
##                       ["value1", "value2", "value3"]),
##         ("Variable name5" , defaultValue, "scale",
##                       [minValue, maxValue], resolution),
##         ("Variable name6" , "", "model"),
##         ("Variable name7" , "", "D2D_model"),
##         ("Label contents" , "", "label"),
##         ("", dialogHeight, "scrollbar"),
##        ]

# Set up Class
import SmartScript
## For available commands, see SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Required Method: Execute
    #  Called once for each grid
    # Fill in the arguments you want to use -- WeatherElement1, WeatherElement2...

    def execute(self, Swell):
        "Put your tool description here"

    ##    # Set up Variables from the varDict (see VariableList below)
    ##    var1 = varDict["Variable name1"]
    ##    var2 = varDict["Variable name2"]

        # Determine new value
        Period = 100 / (Swell[0] + 10.0)
        # Return the new value
        return Period

    # Optional Methods
##        # These methods can have the additional argument:
##        # ToolTimeRange -- selected time range over which we are running the tool
##    def preProcessTool(self, varDict):
##        # Called once at beginning of Tool
##        # Cannot have WeatherElement or Grid arguments
##        pass
##    def postProcessTool(self, varDict):
##        # Called once at end of Tool
##        # Cannot have WeatherElement or Grid arguments
##        pass

## What is "self"????
##    "Self" refers to this Tool class instance.  Don't worry much about it.
##    All you need to do is:
##       -- Make sure to list "self" as the first argument of
##          method Definitions:
##              def _myMethod(self, arg1, arg2)
##       -- When calling your methods, use self._methodName omitting
##          "self" as the first argument:
##              x = self._myMethod(arg1, arg2)
##

## Error Handling
##   Call self.abort(errorString) to stop execution of your tool and
##     display a message to the user.
##   For example:
##     if x > 1000:
##        self.abort("x is too large")
##
##   Call self.noData(messageString) to stop execution of your tool
##     and return a "NoData" error which can be checked by a Procedure.
