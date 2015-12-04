# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CopyNHCProposed
#
# Author:
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
import time, re

try:
    from Numeric import *
except:
    from numpy import *

import GridManipulation

## For documentation on the available commands,
##   see the SmartScript Utility, which can be viewed from
##   the Edit Actions Dialog Utilities window
VariableList = [("Choose Hazards:" , "", "check",
                # ["ProposedSS", "ProposedWW1", "ProposedWW2"]),
                 ["ProposedSS"]),
                ]

class Procedure (GridManipulation.GridManipulation, SmartScript.SmartScript):
    def __init__(self, dbss):
        GridManipulation.GridManipulation.__init__(self)
        SmartScript.SmartScript.__init__(self, dbss)

    def makeNewTimeRange(self, hours):

        startTime = int(time.time() / 3600) * 3600
        endTime = startTime + hours * 3600

        timeRange = self.GM_makeTimeRange(startTime, endTime)

        return timeRange
   
    def execute(self, varDict):

        #  Assign a timeRange from now to 48 hours from now
        timeRange = self.makeNewTimeRange(48)

        #  Copy the ISC data into a grid

        hazardsToCopy = varDict["Choose Hazards:"]

        if len(hazardsToCopy) == 0:
            self.statusBarMsg("You must choose at least one hazard.", "U")
            return 

        #weNames = ["ProposedSS", "ProposedWW1", "ProposedWW2"]
        weNames = ["ProposedSS"]

        # Remove any pre-existing grids first
        for weName in weNames:

            if weName not in hazardsToCopy:
                continue

            trList = self.GM_getWEInventory(weName)
            for delTR in trList:
                self.deleteGrid("Fcst", weName, "SFC", delTR)
            
        # Copy any Proposed nc grids into the Fcst
        for weName in weNames:

            if weName not in hazardsToCopy:
                continue
            
            iscWeName = weName + "nc"
            trList = self.GM_getWEInventory(iscWeName, "ISC", timeRange)

            if len(trList) == 0:
                continue

            gridTR = trList[-1]  # only interested in the latest grid
            
            iscGrid, iscKeys = self.getGrids("ISC", iscWeName, "SFC", gridTR)

            self.createGrid("Fcst", weName, "DISCRETE", (iscGrid, iscKeys),
                            timeRange, discreteKeys=iscKeys, discreteOverlap=0,
                            discreteAuxDataLength=0, defaultColorTable="StormSurgeHazards")

