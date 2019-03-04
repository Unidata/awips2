##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# MinRHTool
#
# Author:
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "MinRH"
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


    def execute(self, GridTimeRange):
        "Put your tool description here"

        MinRH = self.getGrids("Fcst", "RH", "SFC", GridTimeRange, 'Min', 0)

        return MinRH

