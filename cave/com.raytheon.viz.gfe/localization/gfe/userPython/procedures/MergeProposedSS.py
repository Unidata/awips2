# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# MergeProposedSS.py
# Version 3.0 - 13 April 2016
# Authors: Nate Hardin, Tom LeFebvre, Pablo Santos
# ----------------------------------------------------------------------------
#
#
# SOFTWARE HISTORY
#
# Date          Ticket#    Engineer    Description
# ------------- ---------- ----------- -----------------------------------------
# Sep 19, 2016  19293      randerso    Initial baseline check in
# Feb 09, 2017  6128       randerso    Fix removal from combined hazards.
#                                      Correctly handle multiple grids in the 
#                                      48 hour window
# Mar 30, 2017             swhite      Precvnts from running if hazard grids are not merged or are not locked
# Nov 16, 2017             RA/PS       Fixed problem with ProposedSSnc TR not properly reflected on Final Hazards grid.
#
########################################################################

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Populate"]

import time

import HazardUtils
import LogStream
import TimeRange
import TropicalUtility
import numpy as np


class Procedure (TropicalUtility.TropicalUtility):
    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._hazUtils = HazardUtils.HazardUtils(dbss, None)
        self._ssWeName = "ProposedSSnc"
        self._ssDbName = "ISC"

    # Checks for possible conflicts between existing NHC hazards and the
    # WFO hazards. Returns the list of CWAs that conflict in any way with
    # the NHC hazards.
    # Uses TropicalUtility:anyHazardConflice
    def checkForAnyConflicts(self, cwaMask):
        # Fetch the ISC PropsoedSS grid

        propTRList = self.GM_getWEInventory(self._ssWeName, self._ssDbName)
        proposedGrid = self.getGrids(self._ssDbName, self._ssWeName, "SFC", propTRList[-1])

        hazTRs = self.GM_getWEInventory("Hazards")

        currentTime = self._gmtime()

        for tr in hazTRs:
            # We're only interested in future hazard grids
            if tr.endTime() < currentTime:
                continue

            hazardGrid = self.getGrids("Fcst", "Hazards", "SFC", tr)

            if self.anyHazardConflicts(hazardGrid, proposedGrid, cwaMask):
                return True

        return False

    def makeEmptyHazardGrid(self, timeRange):

        # Make an empty grid.
        hazGrid = self.empty(np.int8)
        hazKeys = ["<None>"]
        # Save it
        self.createGrid("Fcst", "Hazards", "DISCRETE", (hazGrid, hazKeys), timeRange)


    # Removes all hazards found in the specified keyList from the Hazards grid.
    # Any hazard matching any key in the keyList is removed.
    def removeHazards(self, keyList):

        #  Fetch the hazards inventory
        trList = self.GM_getWEInventory("Hazards")

        for tr in trList:
            hazGrid, hazKeys = self.getGrids("Fcst", "Hazards", "SFC", tr)

            for hazKey in hazKeys:
                subKeys = hazKey.split("^")
                for key in keyList:
                    for subKey in subKeys:
                        if subKey.startswith(key):  
                            # found a match
                            self._hazUtils._removeHazard("Hazards", tr, subKey)


    def execute(self):
        
        #see if the Hazards WE is loaded in the GFE, if not abort the tool
        if not self._hazUtils._hazardsLoaded():
            self.statusBarMsg("Hazards Weather Element must be loaded in "+\
              "the GFE before running MergeProposedSS.", "S")
            self.cancel()

        #ensure there are no temp grids loaded, refuse to run
        if self._hazUtils._tempWELoaded():
            self.statusBarMsg("There are temporary hazard grids loaded. " + \
                "Please merge all hazards grids before running MergeProposedSS.", "S")
            self.cancel()

        #ensure grid is not locked by another user
        if self.lockedByOther('Hazards', 'SFC'):
            self.statusBarMsg("There are conflicting locks (red locks - owned by others) on Hazards.  " + \
                "Please resolve these before running MergeProposedSS", "S")
            self.cancel()

        start = int(self._gmtime().unixTime() / 3600) * 3600
        end= start + 48 * 3600
        propTR = self.GM_makeTimeRange(start, end)

        # Make sure we have a ProposedSS before we begin
        propTRList = self.GM_getWEInventory(self._ssWeName, dbase = self._ssDbName, timeRange=propTR)

        if len(propTRList) == 0:
            self.statusBarMsg("No ProposedSS Fcst grid found. Tool aborting.", "S")
            return

        # set propTR to span the full inventory of ProposedSS grids
        propTR = self.GM_makeTimeRange(propTRList[0].startTime().unixTime(), propTRList[-1].endTime().unixTime())

        # Make the cwa mask
        siteID = self.getSiteID()
        cwaMask = self.encodeEditArea(siteID)

        # Check each site for any conflicts and return the list of conflicting sites
        if self.checkForAnyConflicts(cwaMask):
            self.statusBarMsg("Hazard ETN conflicts between " + siteID + " and ProposedSS from NHC ISC database.", "U")
            return

        # First remove all SS hazards from the Hazard grid
        ssKeys = ["SS.A", "SS.W"]
        self.removeHazards(ssKeys)

        # If there were not any wfo hazards found, make empty grid(s) based on the ProposedSS timeRange
        hazTRList = self.GM_getWEInventory("Hazards")
        if len(hazTRList) == 0:

            # Make empty hazard grids with same timeRanges as ProposedSS
            for tr in propTRList:
                self.makeEmptyHazardGrid(tr)

        # Make new Hazards grids at the beginning and/or end, if needed
        else:
            if propTR.startTime() < hazTRList[0].startTime(): # add a new Hazard grid at the beginning
                newTR = TimeRange.TimeRange(propTR.startTime(), hazTRList[0].startTime())
                self.makeEmptyHazardGrid(newTR)

            # Check and make one at the end, if needed
            if hazTRList[-1].endTime() < propTR.endTime():
                 newTR = TimeRange.TimeRange(hazTRList[-1].endTime(), propTR.endTime())
                 self.makeEmptyHazardGrid(newTR)

            # Finally split the Hazards grid at the propTR
            self.splitCmd(["Hazards"], propTR)

        # Refetch the hazards inventory, since it may have changed
        hazTRList = self.GM_getWEInventory("Hazards")

        # Get the ProposedSS grid
        propGrid, propKeys = self.getGrids(self._ssDbName, self._ssWeName, "SFC", propTRList[-1])

        # Now add ProposedSS keys back into the hazard grids
        for hazTR in hazTRList:

            # Only add Proposed hazards where the Hazards overlap
            if not hazTR.overlaps(propTR):
                continue

            for propKey in propKeys:
                if propKey != "<None>":
                    propIndex = self.getIndex(propKey, propKeys)
                    mask = propGrid == propIndex
                    self._hazUtils._addHazard("Hazards", hazTR, propKey, mask, combine=1)

