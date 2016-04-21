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
# MakeTmpGrid - Creates a temporary grid of the active element. The new grid
#               has model name of "Temp" and element name of "tmp<WEname>"
#
# Author: Paul Jendrowski WFO Blacksburg, VA (RNK)
#         paul.jendrowski@noaa.gov
# Version: 3.0    Date: 11/08/2004
# Change History:
#    11/08/2004 - Added support for Wx and Vector grids.
#    07/17/2003 - Changed timeConstraints in call to createGrid for RPP21.a7
#                    values changed from hours to seconds
#                  Added logic to let calling procedure optionally specify
#                    name of new grid in argDict (backward compatible)
#    02/21/2003 - original version
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "None"

# You can screen the elements for which your tool will appear by using
# a ScreenList.  For example:
#
#ScreenList = ["T","Td"]
ScreenList = ["SCALAR","VECTOR","WEATHER"]

# Set up Class
import SmartScript
import GridInfo

## For available commands, see SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Required Method: Execute
    #  Called once for each grid
    # Fill in the arguments you want to use -- WeatherElement1, WeatherElement2...

    def execute(self, WEname, variableElement, variableElement_GridInfo,
                GridTimeRange, varDict):
        "Creates a temporary grid of the active element"
        
        if self._gridName == "None":
            name = "tmp" + WEname
        else:
            name = self._gridName

        theParm = self.getParm("Fcst", WEname, "SFC")
        pInfo = theParm.getGridInfo()
        if pInfo is not None:
            pInfo = GridInfo.GridInfo(gridParmInfo=pInfo, gridTime=GridTimeRange)
        pType = variableElement_GridInfo.getGridType().ordinal()
        #print WEname,"is type",pType
# Not exactly sure how to set the timeConstraints, but (1,1,1) seems to work
        # Determine new value
        hr = 3600
        #print "_getGfeHourVersion=",hr
        tc = (0, 1 * hr, 1 * hr)
        if pType == 1:
            self.createGrid(self._model, name, "SCALAR", variableElement, GridTimeRange,
              descriptiveName = name, timeConstraints = tc,
              precision = pInfo.precision(),
              minAllowedValue = pInfo.minLimit(),
              maxAllowedValue = pInfo.maxLimit(),
              units = pInfo.units(),
              rateParm = pInfo.rateParm())
        elif pType == 2:                 
            self.createGrid(self._model, name, "VECTOR", variableElement, GridTimeRange,
                descriptiveName = name)
        elif pType == 3:                 
            self.createGrid(self._model, name, "WEATHER", variableElement, GridTimeRange,
                descriptiveName = name)
        else:
            self.noData("This tool does not support element enumeration=" +`pType`+"!")

        # Return the new value
        return None

    def preProcessTool(self, varDict):
        # Set up for invocation from user as Edit Action or Procedure
        self._model = "Temp"
        self._gridName = "None"
        self._calledfrom = "User"
        
        if varDict != None:
            if varDict.has_key("Model"):
                self._model = varDict["Model"]
            if varDict.has_key("gridName"):
                self._gridName = varDict["gridName"]
            self._calledfrom = "Proc"
        
# The post process tool sets the temporary grid as active and fits the color
# bar to fit to the data.
    def postProcessTool(self, WEname, ToolTimeRange):
    # If this was called interactively, make the tmp grid active
        #print "In postProcessTool"
##        if WEname == "Wind":
##            return
        if self._calledfrom == "User":
            name = "tmp" + WEname
            try:
                self.setActiveElement(self._model, name, "SFC", ToolTimeRange, fitToData=1)
            except:
                pass
