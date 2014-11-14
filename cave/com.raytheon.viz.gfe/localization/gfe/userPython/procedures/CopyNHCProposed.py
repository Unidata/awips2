#--------------------------------------------------------------------------
# SVN: $Revision$ - $Date$
#
#     Converted with gfePorter R3342 on Oct 01, 2013 18:43 GMT
#     Not tested. Remove these 2 lines when ported and tested.
#
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CopyNHCProposed
#
# Author: T LeFebvre/P. Santos
# Last Modified: Sept 18, 2014
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Populate"]

# The ToolList is optional, but recommended, if you are calling
# Smart Tools from your Script.
# If present, it can be used to show which grids will be
# modified by the Script.

ToolList = []

### If desired, Set up variables to be solicited from the user:
##  If your script calls Smart Tools, this VariableList should cover
##  cover all the variables necessary for the tools.

import SmartScript
from numpy import *
import TimeRange
import AbsTime
import time, re

## For documentation on the available commands,
##   see the SmartScript Utility, which can be viewed from
##   the Edit Actions Dialog Utilities window
Supported_elements=["ProposedSS"]
VariableList = [("Choose Hazards:" , ["ProposedSS"], "check", Supported_elements),
                # ["ProposedSS"]),
                ]

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Makes a timeRange based on the specified start and end times.
    # If no times are specified, returns the largest timeRange possible.
    # This method will work in either an AWIPS I or AWIPS II environment.
    def makeTimeRange(self, start=0, end=0):            
        
        try:  # AWIPS 1 code
            import TimeRange
            import AbsTime
            import AFPS
            if start == 0 and end == 0:
                return TimeRange.TimeRange.allTimes()
            
            startTime = AbsTime.AbsTime(start)
            endTime = AbsTime.AbsTime(end)
    
            tr = TimeRange.TimeRange(startTime, endTime)

        except:   # AWIPS 2 code
            import TimeRange, AbsTime
            if start == 0 and end == 0:
                return TimeRange.allTimes()
            
            startTime = AbsTime.AbsTime(start)
            endTime = AbsTime.AbsTime(end)
    
            tr = TimeRange.TimeRange(startTime, endTime)

        return tr

    def makeNewTimeRange(self, hours):

        startTime = int(time.time() / 3600) * 3600
        endTime = startTime + hours * 3600

        timeRange = self.makeTimeRange(startTime, endTime)

        return timeRange

    def getWEInventory(self, modelName, WEName, timeRange=None):

        if timeRange is None:
            timeRange = self.makeTimeRange()

        gridInfo = self.getGridInfo(modelName, WEName, "SFC", timeRange)
        trList = []
        for g in gridInfo:
            start = g.gridTime().startTime().unixTime()
            end = g.gridTime().endTime().unixTime()
            tr = self.makeTimeRange(start, end)
            trList.append(tr)

        return trList
    
    def execute(self, varDict):

        #  Assign a timeRange from now to 48 hours from now
        timeRange = self.makeNewTimeRange(48)

        #  Copy the ISC data into a grid

        hazardsToCopy = varDict["Choose Hazards:"]

        if len(hazardsToCopy) == 0:
            self.statusBarMsg("You must choose at least one hazard.", "U")
            return 

        weNames = ["ProposedSS"]
        #weNames = ["ProposedSS"]

        # Remove any pre-existing grids first
        for weName in weNames:

            if weName not in hazardsToCopy:
                continue

            trList = self.getWEInventory("Fcst", weName)
            for delTR in trList:
                self.deleteGrid("Fcst", weName, "SFC", delTR)
            
        # Copy any Proposed nc grids into the Fcst
        for weName in weNames:

            if weName not in hazardsToCopy:
                continue
            
            iscWeName = weName + "nc"
            trList = self.getWEInventory("ISC", iscWeName, timeRange)

            if len(trList) == 0:
                continue

            gridTR = trList[-1]  # only interested in the latest grid
            
            iscGrid, iscKeys = self.getGrids("ISC", iscWeName, "SFC", gridTR)

            self.createGrid("Fcst", weName, "DISCRETE", (iscGrid, iscKeys),
                            timeRange, discreteKeys=iscKeys, discreteOverlap=0,
                            discreteAuxDataLength=0, defaultColorTable="StormSurgeHazards")
