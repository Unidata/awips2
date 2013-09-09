##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# EnufCloudForPoP
#
# Author: Tom Mazza
#
# Last Submitted to Repository:     Fri 25 Jan 02   1.0
# Last Modified:                    Fri 25 Oct 02   1.1
# ----------------------------------------------------------------------------

ToolType = "numeric"
from numpy import *

WeatherElementEdited = "Sky"

# You can screen the elements for which your tool will appear by using
# a ScreenList.  For example:
#
#ScreenList = ["T","Td"]
#ScreenList = ["SCALAR","VECTOR","WEATHER"]

# include this line to use methods for Weather Keys
# from WxMethods import *

### If desired, Set up variables to be solicited from the user:
VariableList = [
##         ("Greater or by factor ?" , "add", "alphaNumeric"),
##         ("Variable name3" , ["default value1", "default value2"], "check",
##                       ["value1", "value2", "value3"]),
         ("Sky vs PoP Relationship:" , "add", "radio",
                       ["add", "multiply", "Sky Limit"]),
         ("For add, multiply (smaller factor), by how much ?" , "20", "numeric"),
         ("For Sky Limit, only Sky less than Limit affected; it is raised to the Limit:", "", "label"),
         ("Enter Sky Limit: the minimum Sky cover needed to support Wx:" , 60, "numeric"),
#         ("Enter minimum PoP for measurable precip:", 15, "numeric"),
         ("Enter Sky cover for 5% PoP:" , 30, "numeric"),
##         ("Variable name5" , defaultValue, "scale",
##                       [minValue, maxValue]),
##         ("Variable name6" , "", "model"),
##         ("Variable name7" , "", "D2D_model"),
##         ("Label contents" , "", "label"),
        ]

# Set up Class
import SmartScript
## For available commands, see SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Required Method: Execute
    #  Called once for each grid
    # Fill in the arguments you want to use -- WeatherElement1, WeatherElement2...

    def execute(self, Sky, PoP, varDict):
        "Allows one to ensure there is enough cloud to support the corrsponding PoP.  \
        The user decides by how much, or by what factor, the cloud should be greater."

    ##    # Set up Variables from the varDict (see VariableList below)
    ##    var1 = varDict["Variable name1"]
    ##    var2 = varDict["Variable name2"]

        # Determine new value
        if (self._operator == "add"):
            mainAddMask = logical_and(greater_equal(PoP, 5), less(Sky, PoP + self._factor))
            Sky = where(logical_and(less(PoP, 5), less_equal(Sky, PoP * self._lofactor)), PoP * self._lofactor,
                  where(logical_and(mainAddMask, less_equal(PoP, self._breathingRoom)), PoP + self._factor,
                  where(greater(PoP, self._breathingRoom), 100, Sky)))
        elif self._operator == "multiply":
            Sky = where(logical_and(less(Sky, PoP * self._factor), less_equal(PoP * self._factor, 100)), PoP * self._factor,
                  where(logical_and(less(Sky, PoP * self._factor), greater(PoP * self._factor, 100)), 100, Sky))
        else:
            lowSkyMask = less(Sky, self._SkyLimit)
            Sky = where(logical_and(lowSkyMask, greater_equal(PoP, self._PoPLimit)), self._SkyLimit,
                        where(logical_and(lowSkyMask, less_equal(Sky, self._SkyLimit - (self._PoPLimit - PoP) * self._slope)),
                              (self._SkyLimit - (self._PoPLimit - PoP) * self._slope), Sky))            

    #    PoP = where(logical_and(curMask, greater_equal(PoP, (self._minPoP - abs(self._SkyLimit - Sky)/self._slope))),
    #                (self._minPoP - abs(self._SkyLimit - Sky)/self._slope),
    #                PoP)

        # Return the new value
        return Sky

    # Optional Methods
##        # These methods can have the additional argument:
##        # ToolTimeRange -- selected time range over which we are running the tool

    def preProcessTool(self, varDict):
        # Called once at beginning of Tool
        # Cannot have WeatherElement or Grid arguments

        #  Get thresholds for Sky cover
        #  Get method
        self._operator = varDict["Sky vs PoP Relationship:"]
        
        #  For Add or Multiply:
        #  Idea is Sky is greater than PoP, either by a fixed amount
        #  (add), or by a factor.  The value for sky, of course, cannot
        #  be greater than 100; this is where 'breathingRoom' comes in
        #  (for 'add').  Sky is left alone if it is already large enough.
        self._factor = varDict["For add, multiply (smaller factor), by how much ?"]
        #  For Add:
        self._lofactor = float(self._factor + 5) / 5.
        #  For Muultply:
        self._breathingRoom = 100 - self._factor

        
        #  For Sky Limit:
#        self._PoPLimit = varDict["Enter minimum PoP for measurable precip:"]         ##  15
        self._PoPLimit = 15
        self._SkyLimit = varDict["Enter Sky Limit: the minimum Sky cover needed to support Wx:"]  ##  60
        self._SkyMin = varDict["Enter Sky cover for 5% PoP:"]                      ##  30

        #  Make sure minimum PoP for measurable precip is 15-25%
        if self._PoPLimit < 15.0:
            self._PoPLimit = 15.0
        elif self._PoPLimit > 25.0:
            self._PoPLimit = 25.0

        #  Make sure minimum Sky cover for Wx is 50-100%
        if self._SkyLimit < 50:
            self._SkyLimit = 50
        elif self._SkyLimit > 100:
            self._SkyLimit = 100

        #  Make sure Sky cover for 5% PoP is 10-100%
        if self._SkyMin < 10:
            self._SkyMin = 10
        elif self._SkyMin > 100:
            self._SkyMin = 100

        #  Make sure Sky cover for 5% PoP < minimum Sky cover for Wx
        if self._SkyMin > self._SkyLimit - 15:
            self._SkyMin = self._SkyLimit - 15

        #  Compute slope to use for this line
        self._slope = (self._SkyLimit - self._SkyMin) / (self._PoPLimit - 5.0)
        
##    def postProcessTool(self, varDict):
##        # Called once at end of Tool
##        # Cannot have WeatherElement or Grid arguments
##        pass
##    def preProcessGrid(self, varDict):
##        # Called once at beginning of each Grid
##        # Can have Grid arguments (T_Grid),
##        # but not point arguments (T)
##        pass
##    def postProcessGrid(self, varDict):
##        # Called once at end of Grid
##        # Can have Grid arguments (T_Grid),
##        # but not point arguments (T)
##        pass

## What is "self"????
##    "Self" refers to this Tool class instance.  Don't worry much about it.
##    All you need to do is:
##       -- Make sure to list "self" as the first argument of
##          method Definitions:
##              def myMethod(self, arg1, arg2)
##       -- When Calling your methods, use self.methodName omitting
##          "self" as the first argument:
##              x = self.myMethod(arg1, arg2)
##

## Error Handling
##   Call self.abort(errorString) to stop execution of your tool and
##     display a message to the user.
##   For example:
##     if x > 1000:
##        self.abort("x is too large")
##
##   Call self.noData(messageString) to stop execution of your tool
##     and return a "NoData" error which can be checked by a Script.
