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
# getGridsTool - generic smart tool to use the SmartScript class getGrids 
#            method to get the max, sum, etc. value of all grids of the
#            element defined by varDict["Element"] into the active element.
#            The active element should span multiple grids of
#            varDict["Element"]. This tool is designed to be
#            called by a procedure and not run interactively.
#            This is a more generic version of getSumGrids and getMaxGrid
#            SmartTools and could be used to replace these tools by adding
#            varDict["Mode"] = "method" to the calling procedure. Method
#            is any value accepted by the "mode" argument to getGrids plus
#            "Last" to get the last grid and "MaxTime" to get the grid with
#            the largest percentage time coverage.
#
# Author: Paul Jendrowski WFO Blacksburg, VA (RNK)
#         paul.jendrowski@noaa.gov
# Version: 1.0    Date: 11/08/2004
# ----------------------------------------------------------------------------

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
        """Gets value of another set of grids.
        This tool should be run from a procedure and not interactively."""

        # Check for required arguments in varDict
        if varDict == None:
            msg="getGridsTool - No element defined.\n"
            msg += " This tool should not be run interactively!"
            self.noData(msg)
            
        if varDict.has_key("Element"):
            we = varDict["Element"]
        else:
            msg="getGridsTool - No element defined.\n"
            msg += " This tool should not be run interactively!"
            self.noData(msg)
            
        if varDict.has_key("Mode"):
            self.__getMode = varDict["Mode"]
        else:
            msg="getGridsTool - No Mode defined.\n"
            msg += " This tool should not be run interactively!"
            self.noData(msg)
            
        if self.__getMode == "MaxTime" or self.__getMode == "Last":
            # Determine the Wx grids that correspond to this grid
            # Note:  There could be more than one Wx grid within
            # the grid time range. If this is the case,
            # Wx_GridInfo will be a list.
            # If not, make it into a list for processing.
            gridInfo = self.getGridInfo("Fcst", varDict["Element"], "SFC",
                                        GridTimeRange)
            
            if gridInfo is None:
                Wx_GridInfo = []
            elif isinstance(gridInfo, (list,tuple)):
                Wx_GridInfo = gridInfo
            else:
                Wx_GridInfo = [gridInfo]
                
            if self.__getMode == "MaxTime":

                # Determine the percentage of GridTimeRange (PoP Grid) that each
                #  Wx grid takes up.  Put this percentage into a list.
                #  (Note: we have to name the percentage list with the prefix,
                #  self.__, so that it can be used in the execute method).
                max=0
                i=0
                index = -1
                for info in Wx_GridInfo:
                    wxDuration = float(GridTimeRange.intersection(
                        info.gridTime()).duration())
                    if wxDuration > max:
                        index=i
                        max = wxDuration
                    i += 1
            elif len(Wx_GridInfo) > 0:
                index = len(Wx_GridInfo)-1
            else:
                index = -1
                
        if (self.__getMode == "MaxTime" or self.__getMode == "Last") and index >= 0:
            WxLst = self.getGrids("Fcst", we, "SFC", GridTimeRange, mode="List", noDataError=0)
            if WxLst is not None and len(WxLst) > index:
                Wx = WxLst[index]
            else:
                Wx = None
        else:
            Wx = self.getGrids("Fcst",we,"SFC",GridTimeRange,
                               mode=self.__getMode, noDataError=0)

        # Returning None is bad.
        # If this is a temporary grid, try to get a grid from the permanent grid.
        # This is most likely the scratch grid we're filling in, but it beats nothing.
        if Wx is None and len(we) > 3 and we[0:3]=="tmp":
                we = we[3:]
                Wx = self.getGrids("Fcst", we, "Sfc", GridTimeRange, mode="Last")
                
        
        # Return the new value
        return Wx
