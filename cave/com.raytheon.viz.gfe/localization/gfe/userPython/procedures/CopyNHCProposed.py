# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CopyNHCProposed - Version 3.0
#
# Author: T LeFebvre/P. Santos
#
# ----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Dec 10, 2015            T LeFebvre
# Sep 19, 2016 19293      randerso    Changes for 2017 Tropical Season
# Feb 27, 2017 122217     T Lefebvre  Fix to calDiff Grid Call (sending both boolean and grid instead of just grid)
#
########################################################################

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

import re, time

import AbsTime
import LogStream
import TimeRange
import TropicalUtility
import numpy as np


# The GUI portion is commented out for now, since we have only one element.
# This may be restored when other hazards are supported.
# Supported_elements=["ProposedSS"]
# VariableList = [("Choose Hazards:" , ["ProposedSS"], "check", Supported_elements),
#                 # ["ProposedSS"]),
#                 ]
class Procedure (TropicalUtility.TropicalUtility):
    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)

    def execute(self):

        #  Assign a timeRange from now to 48 hours from now
        start = int(self._gmtime().unixTime() / 3600) * 3600
        end = start + 48 * 3600
        timeRange = self.GM_makeTimeRange(start, end)

        # Commented out until this tool suports more than one hazard
#         #  Copy the ISC data into a grid
#         hazardsToCopy = varDict["Choose Hazards:"]
#
#         if len(hazardsToCopy) == 0:
#             self.statusBarMsg("You must choose at least one hazard.", "U")
#             return
        #  Delete any existing collaboration difference grids
        self.unloadWE("Fcst", "CollabDiffSS", "SFC")

        # Hard-coded to the only element supported by this tool.
        hazardsToCopy = ["ProposedSS"]

        # This list should come from the GUI. Hard-coded for now.
        weNames = ["ProposedSS"]  # Eventually should come from the GUI

        # Remove any pre-existing grids first
        for weName in weNames:

            if weName not in hazardsToCopy:
                continue

            trList = self.GM_getWEInventory(weName, "Fcst")
            for delTR in trList:
                self.deleteGrid("Fcst", weName, "SFC", delTR)

        # Copy any Proposed nc grids into the Fcst
        for weName in weNames:

            if weName not in hazardsToCopy:
                continue

            iscWeName = weName + "nc"
            trList = self.GM_getWEInventory(iscWeName, "ISC", timeRange=timeRange)

            if len(trList) == 0:
                LogStream.logProblem("No grids found for ", iscWeName)
                continue

            gridTR = trList[-1]  # only interested in the latest grid          

            proposedGrid = self.getGrids("ISC", iscWeName, "SFC", gridTR)
            (iscGrid, iscKeys) = proposedGrid

            start = gridTR.endTime().unixTime() - (48 * 3600)
            end = gridTR.endTime().unixTime()
            createTR = self.GM_makeTimeRange(start, end)

            self.createGrid("Fcst", weName, "DISCRETE", (iscGrid, iscKeys),
                            createTR, discreteKeys=iscKeys, discreteOverlap=0,
                            discreteAuxDataLength=0, defaultColorTable="StormSurgeHazards")
            
            # This If section added during Jan 2017 SWiT. It creates a diff grid between incoming NHC ProposedSS and what is already in Hazards Grid.
            if weName == "ProposedSS":
                #hazTRlist = self.GM_getWEInventory("Hazards", "Fcst", gridTR)
                hazTRlist = self.GM_getWEInventory("Hazards", "Fcst")
                print "hazTRlist: ", hazTRlist               
                
                if len(hazTRlist) == 0:
                    self.statusBarMsg("No Hazards grids found. No Diff to calculate.", "U")
                else:
                    
                    HazardList = []
                    anySSHazardsFound = False
                    
                    #  See if we have any SS hazards
                    for hazTR in hazTRlist:

                        #  Assume we have SS hazards
                        ssHazardsFound = False

                        hazardsGrid = self.getGrids("Fcst", "Hazards", "SFC", hazTR)
                                                    
                        (hazGrid, hazKeys) = hazardsGrid
                            
                        for key in hazKeys:
#                             print "key", key
                            if "SS." in key:
                                ssHazardsFound = True
                                anySSHazardsFound = True
                                break
#                         print  "B4 anySSHazardsFound, ssHazardsFound", anySSHazardsFound, ssHazardsFound
                        #  If this Hazards grid is not None
                        HazardList.append((ssHazardsFound, hazardsGrid))

                    print len(HazardList), len(hazTRlist)

                    for index in range(len(HazardList)):
                        print "hazTR: ", hazTRlist[index]
                        
                        #  Get the state of this Hazards grid
                        ssFound, hazardsGrid =  HazardList[index]                    
#                         hazardsGrid = self.getGrids("Fcst", "Hazards", "SFC", hazTR)
                        
                        if hazTRlist[index].overlaps(createTR):
#                         if hazTR.startTime().unixTime() < createTR.endTime().unixTime():
                            
#                             print "\n\nHazard time range is good"
                            #  If any SS hazards were found, only produce the 
                            #  diff for grids which had a SS hazard. Otherwise,
                            #  compare all grids
                            
                            if not anySSHazardsFound or (anySSHazardsFound and ssFound):
                                self.calcDiffGrid(HazardList[index][1], proposedGrid, "CollabDiffSS",  hazTRlist[index], isWFO=True)                        
        # Display the Storm Surge area to remind forecasters of the domain
        stormSurgeEditArea = self.getEditArea("StormSurgeWW_EditArea_Local")
        self.setActiveEditArea(stormSurgeEditArea)
