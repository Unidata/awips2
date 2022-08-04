# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# MergeProposedWindWW.py
#
# This procedure merges the hazards found in the ProposedTropWindWW grid onto
# The hazard grid. First conflict checking with ETNS is performed. If any
# incompatabilites are found, the tool aborts.
#
# Author: tlefebvre
#
# March 27, 2020 21020      tlefebvr    Initial version.
# April  8,  2020 21020     tlefebvr    Added fetching of coastal mask.
# April  9,  2020 21020     tlefebvr    Added imports
# April 16, 2020 21020      tlefebvr    Using nhcCoastalMask more efficiently
# May    6, 2020 21020      tlefebvr    Code Clean-up.
# May   13, 2020 21020      tlefebvr    Changed Inland option to wfoMask.
# May   27, 2020 21020      tlefebvr    Fixed bug that removed hazards first.
# May   27, 2020 21020      tlefebvr    Fixed bug wiped out local hazards.
# May   27, 2020 21020      tlefebvr    Hazard grid changes where Proposed grid
#                                       is not None.
# May   28, 2020 21020      tlefebvr    Fixed issue with conflicting hazards
# June   3, 2020 21020      tlefebvr    Addressed code review comments
# ----------------------------------------------------------------------------

MenuItems = ["Populate"]

import TropicalUtility
import HazardUtils
import numpy as np
import TimeRange
import WindWWUtils
import ZoneMap
import re

VariableList = []
VariableList.append(("Merge Hazards from:", "Coastal Only", "radio", ["Coastal Only", "Coastal and Inland"]))

class Procedure (TropicalUtility.TropicalUtility):
    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._hazUtils = HazardUtils.HazardUtils(dbss, None)

         # This will make the NHCCoastal edit area
        self._WindWWUtils = WindWWUtils.WindWWUtils(self._dbss)
        self._zoneMap = ZoneMap.ZoneMap(self._dbss)
                
    def checkForAnyConflicts(self, cwaMask, weName, dbName):
        """
        Checks for possible conflicts between existing NHC hazards and the
        WFO hazards. Returns the list of CWAs that conflict in any way with
        the NHC hazards. Uses TropicalUtility:anyHazardConflict
        """
        # Fetch the Proposed grid
        propTRList = self.GM_getWEInventory(weName, dbName)
        proposedGrid = self.getGrids(dbName, weName, "SFC", propTRList[-1])

        hazTRs = self.GM_getWEInventory("Hazards")

        currentTime = self._gmtime()

        for tr in hazTRs:
            # We're only interested in future hazard grids
            if tr.endTime() < currentTime:
                continue

            hazardGrid = self.getGrids(self.mutableID(), "Hazards", "SFC", tr)

            if self.anyHazardConflicts(hazardGrid, proposedGrid, cwaMask):
                return True

        return False

    def makeEmptyHazardGrid(self, timeRange):
        """
        Makes an empty Hazards grid.
        """
        # Make an empty grid.
        hazGrid = self.empty(np.int8)
        hazKeys = ["<None>"]
        # Save it
        self.createGrid("Fcst", "Hazards", "DISCRETE", (hazGrid, hazKeys), timeRange)

    def fetchNHCZoneMask(self):
        """
        Fetches and returns the NHCCoastalZone mask.
        """
        try: # Try to read the nhcZones
            nhcEA = self.getEditArea("WindWWNHCZones")
            nhcZoneMask = self.encodeEditArea(nhcEA)
        except: # Can't find it so make it from scratch
            self.statusBarMsg("Making NHCZone mask. Stand by.", "R")
            nhcZoneList = self._WindWWUtils.breakpointZoneList()
            # Set the mask
            self.statusBarMsg("Making Coastal edit area. Stand by.", "S")
            nhcZoneMask = self._zoneMap.maskFromZoneList(nhcZoneList)
            # Convert to edit area and save
            nhcEA = self.decodeEditArea(nhcZoneMask)
            self.saveEditArea("WindWWNHCZones", nhcEA)

        return nhcZoneMask

    def stripETN(self, hazKey):
        """
        Remove the ETN from the hazKey and return the result.
        """
        # Remove everything past and including the colon ":"
        return re.sub(r':\d{4}', "", hazKey)
    
    def removeWindHazards(self, weName, timeRange, mask):
        """
        Removes all the tropical wind hazards from the specified grid and time.
        """
        tropicalWindKeys =  ["TR.A", "HU.A", "TR.W", "HU.W"]
        
        hazGrid, hazKeys = self.getGrids(self.mutableID(), weName, "SFC", timeRange)
        for hazKey in hazKeys:
            subKeys = hazKey.split("^")
            for subKey in subKeys:
                keyNoETN = self.stripETN(subKey)
                if keyNoETN not in tropicalWindKeys:
                    continue

                self._hazUtils._removeHazard(weName, timeRange, subKey, mask)

    def execute(self, editArea, timeRange, varDict):

        # See if the Hazards WE is loaded in the GFE, if not abort the tool
        if not self._hazUtils._hazardsLoaded():
            self.statusBarMsg("Hazards Weather Element must be loaded in "+\
              "the GFE before running MergeProposedWindWW.", "S")
            self.cancel()

        # Ensure there are no temp grids loaded, refuse to run
        if self._hazUtils._tempWELoaded():
            self.statusBarMsg("There are temporary hazard grids loaded. " + \
                "Please merge all hazards grids before running MergeProposedWW.", "S")
            self.cancel()

        # Ensure grid is not locked by another user
        if self.lockedByOther('Hazards', 'SFC'):
            self.statusBarMsg("There are conflicting locks (red locks - owned by others) on Hazards.  " + \
                "Please resolve these before running MergeProposedWindWW", "S")
            self.cancel()

        # Get the coastal mask
        self._coastalMask = self.fetchNHCZoneMask()

        proposedWEName = "ProposedTropWindWW"
        proposedDBName = self.mutableID()

        # Make sure we have a ProposedTropWindWW before we begin
        propTRList = self.GM_getWEInventory(proposedWEName, dbase=proposedDBName)
        if len(propTRList) == 0:
            self.statusBarMsg("No ProposedTropWindWW Fcst grid found. Tool aborting.", "S")
            return        

        userOption = varDict["Merge Hazards from:"]

        # Make the CWA mask
        siteID = self.getSiteID()
        cwaMask = self.encodeEditArea(siteID)
        # Make a mask that will be used to restrict the area that will be merged.
        if userOption == "Coastal Only":
            # Set the mask to the Coastal mask           
            self._hazardMask = self._coastalMask
        else: # Set the mask to the entire domain
            self._hazardMask = cwaMask

        # Get the ProposedTropWindWW grid and the Hazard grid.
        # set propTR to span the full inventory of ProposedWW grids
        propTR = self.GM_makeTimeRange(propTRList[0].startTime().unixTime(), propTRList[-1].endTime().unixTime())

        # Check each site for any conflicts and return the list of conflicting sites
        if self.checkForAnyConflicts(cwaMask, proposedWEName, proposedDBName):
            self.statusBarMsg("Hazard conflicts between Hazard grid and ProposedTropWindWW from NHC.\n"\
                              "Check Wind ETN or for CF Hazards.", "U")
            return

        # If there were not any wfo hazard grids found, make empty grid(s) based on the ProposedWW timeRange
        hazTRList = self.GM_getWEInventory("Hazards")
        if not hazTRList:
            # Make empty hazard grids with same timeRanges as ProposedWW
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

        # Re-fetch the hazards inventory, since it may have changed
        hazTRList = self.GM_getWEInventory("Hazards")

        # Get the ProposedWW grid
        propGrid, propKeys = self.getGrids(proposedDBName, proposedWEName, "SFC", propTRList[-1])

        # Find areas where the Prop grid is not None and only operate on those areas
        noneIndex = self.getIndex("<None>", propKeys)
        propHazMask = ~(propGrid == noneIndex)

        # Now that hazards are separated so update each one individually
        for hazTR in hazTRList:
            # Only add Proposed hazards where the Hazards overlap
            if not hazTR.overlaps(propTR):
                continue
            # Remove current hazards as these will be replaced.
            removeMask = propHazMask & self._hazardMask
            self.removeWindHazards("Hazards", hazTR, removeMask)

            for propKey in propKeys:
                if propKey != "<None>":
                    propIndex = self.getIndex(propKey, propKeys)
                    mask = (propGrid == propIndex) & self._hazardMask & propHazMask
                    self._hazUtils._addHazard("Hazards", hazTR, propKey, mask, combine=1)

        # Separate the hazards for easier updating
        self._hazUtils._separateHazardGrids()
    

