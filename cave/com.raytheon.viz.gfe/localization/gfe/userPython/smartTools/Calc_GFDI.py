# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Calc_GFDI.py
#
# Author: dtomalak
# ----------------------------------------------------------------------------


ToolType = "numeric"
WeatherElementEdited = "GFDI"
from numpy import *
HideTool = 0

# You can screen the elements for which your tool will appear by using
# a ScreenList.  For example:
#
ScreenList = ["GFDI"]
#ScreenList = ["SCALAR","VECTOR","WEATHER","DISCRETE"]


# Set up Class
import SmartScript
import time
# For available commands, see SmartScript


class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Required Method: Execute
    #  %comment
    # Fill in the arguments you want to use -- WeatherElement1, WeatherElement2...

    def execute(self, GFDI, WindGust,Wind, RH, Curing, T, GridTimeRange,editArea, varDict):
        "Calculates the GFDI based on T, RH, Wind and Curing grids"

        # Snag the edit area and the time range passed to the tool
        myTimeRange=GridTimeRange

        # Calculate T in C. 
        tc = 5.0/9.0*(T-32.0)

        # Calculate the mean of the sustained winds and gust. Convert to kph 

        windSustained = self.getGrids("Fcst","Wind","SFC", myTimeRange, noDataError=0)
        windGust = self.getGrids("Fcst","WindGust","SFC", myTimeRange, noDataError=0)
        wspd_kph = (windSustained[0]+windGust)* 1.85325*0.5

        # get the Curing and RH values
        
        Curing = self.getGrids("Fcst", "Curing", "SFC", myTimeRange, noDataError=0)
        rh = self.getGrids("Fcst", "RH", "SFC", myTimeRange, noDataError=0)

        # Calculate the GFDI:
        
        GFDI = pow(10,(0.009254
                         -0.004096*pow((100.0-Curing),1.536)
                         +0.01201*tc
                         +0.2789*pow(wspd_kph,0.5)
                         -0.09577*pow(rh,0.5) ))
        
        # Round it to integers and clip max value to 150.
        GFDI = around(GFDI,0)
        GFDI = clip(GFDI,0.1,150.0)

#        maxFDI =  maximum.reduce(maximum.reduce(GFDI))
#        minFDI =  minimum.reduce(minimum.reduce(GFDI))                  
#        print "min/max = ", minFDI, maxFDI
        
        return GFDI
    

    # Optional Methods
#        # These methods can have the additional argument:
#        # ToolTimeRange -- selected time range over which we are running the tool
#    def preProcessTool(self, varDict):
#        # Called once at beginning of Tool
#        # Cannot have WeatherElement or Grid arguments
#        pass
#    def postProcessTool(self, varDict):
#        # Called once at end of Tool
#        # Cannot have WeatherElement or Grid arguments
#        pass

# What is "self"????
#    "Self" refers to this Tool class instance.  Don't worry much about it.
#    All you need to do is:
#       -- Make sure to list "self" as the first argument of
#          method Definitions:
#              def _myMethod(self, arg1, arg2)
#       -- When calling your methods, use self._methodName omitting
#          "self" as the first argument:
#              x = self._myMethod(arg1, arg2)
#

# Error Handling
#   Call self.abort(errorString) to stop execution of your tool and
#     display a message to the user.
#   For example:
#     if x > 1000:
#        self.abort("x is too large")
#
#   Call self.noData(messageString) to stop execution of your tool
#     and return a "NoData" error which can be checked by a Procedure.
