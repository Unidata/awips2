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
# Sept   1, 2020 21020      tlefe/santos Changes to GUI per user request
# Sept   9, 2020 21020      tlefe/santos Fixed issue where hazards were not removed
#                                        Coastal zones.
# Sept   19, 2020 21020      tlefebvr    Fixed so that only upgrades to local hazards
#                                        are allowed.
# Sept   21, 2020 21020      tlefebvr    Code clean-up.
# Sept   24, 2020 21020      tlefebvr    Local Hazards are not considered when stripping ETN
# Sept   25, 2020 21020      tlefebvr    Fixed bug in upgradeMask.
# Sept   28, 2020 21020      tlefebvr    Restored removeHazards method and fixed issue with
#                                        None Prop key. Upgrade mask of zero makes no changes.
# Oct     6, 2020 21020      tlefebvr    If no inland upgrade, hazard grid is left untouched.
# Oct.   21, 2020 22033      tlefebvr    Changed the way the upgrade mask is calculated.
# Oct.   25, 2020 22033      tlefebvr    More tweaks to upgrade mask..
# Oct.   29, 2020 22033      tlefebvr    Fixed adding coastal Hazards.
# Oct.   30, 2020 22033      tlefebvr    Better solution to assigning Coastal Hazards
# Nov.   3,  2020 22033      tlefebvr    Coastal areas not being updated when Prop==Haz.
# Nov.   4,  2020 22033      tlefebvr    Yet another change to fix coastal zones.
# Nov.   5,  2020 22033      tlefebvr    Changed GUI based on WFO type and added back Hazard
#                                        grid when it gets deleted by removeHazards
# Nov.  12,  2020 22033      tlefebvr    No longer adding grids to fill out the Propoised timeRange.
# Nov.  16,  2020 22033      tlefebvr    Redesign of code that add/removes hazards.
# Nov.  22,  2020 22033      tlefebvr    Another tweak to when the NHC hazard is added on the coast.
# Nov.  22,  2020 22033      tlefebvr    More refinements to NHC hazards on the coast.
# Feb.  10,  2021 22033      tlefebvr    Passing in wfo mask to ZoneMap to purify zoneList.
# Feb.  11,  2021 22033      tlefebvr    Fixed bugs realated to above change.
# Feb.  15,  2021 22033      tlefebvr    Fixed issue with Proposed grid not populating Hazards grid
#                                        if there was any hazard grid completely in the past.
# Feb.  16,  2021 22033      tlefebvr    Fixed bug when empty hazards and split Hazards grid at
#                                        Proposed grid time boundary.
# Feb.  16,  2021 22033      tlefebvr    Removed debug createGrid statements.
# Mar.   3,  2021 22033       tlefebvr    Some adjustments to the final Hazards inventory.
# Mar.   8,  2021 22033       tlefebvr    Final adjustments to the final Hazards inventory.
# Mar.   8,  2021 22033       tlefebvr    Fixed bug in calculating clip mask.
# Mar.   8,  2021 22033       tlefebvr    Fixed bug in GUI logic..
# Mar.   8,  2021 22033       tlefebvr    Another attempt at fixing modified coastal areas.
# Mar.   9   2021 22033       tlefebvr    Only inland hazards should be removed before adding.
# Jul.  29   2021 22531       tlefebvr    Final code clean-up before check-in.
# Aug.  24   2021 22531       tlefebvr    Spell-corrected a comment.
# Feb,   1,  2022 22531       tlefebvr    p3 fix
# Apr   13,  2022 22531       tlefebvr    Made a few changes for Python3 compatibility.
# Sep   15,  2022 22531          santos/composano COde fixes post 21.4.1-13 testing.
#----------------------------------------------------------------------------

MenuItems = ["Hazards"]

import TropicalUtility
import HazardUtils
import ProcessVariableList
import numpy as np
import TimeRange, AbsTime
import WindWWUtils
import ZoneMap


class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._hazUtils = HazardUtils.HazardUtils(dbss, None)

         # This will make the NHCCoastal edit area
        self._WindWWUtils = WindWWUtils.WindWWUtils(self._dbss)
        # Make the CWA mask
        siteID = self.getSiteID()
        natCenters = self.getNationalCenterIDs()
        self._cwaMask = self.newGrid(True, np.bool)
        if siteID not in natCenters:
            self._cwaMask = self.encodeEditArea(siteID)

        self._zoneMap = ZoneMap.ZoneMap(self._dbss, self._cwaMask)

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
        nhcEA = self._WindWWUtils.fetchNHCZonesEditArea()
        nhcZoneMask = self.encodeEditArea(nhcEA)
        return nhcZoneMask

    def windSubKey(self, key):
        """
        Returns the first subkey that matches key.
        """
        subKeys = self._hazUtils._getSubKeys(key)
        for subKey in subKeys:
            if subKey in self._windKeys:
                return subKey
        return "<None>"

    def calcUpgradeMask(self, propGrid, propKeys, hazGrid, hazKeys):
        """
        Rank each grid and then calculate the upgrade areas using the rank.
        """
        hazRank = self._WindWWUtils.calcRankGrid((hazGrid, hazKeys))
        propRank = self._WindWWUtils.calcRankGrid((propGrid, propKeys))

        # Add points where there's an upgrade proposed.
        return hazRank < propRank

    def removeWindHazards(self, weName, timeRange, mask):
        """
        Removes all the tropical wind hazards from the specified grid and time.
        """
        tropicalWindKeys = ["TR.A", "HU.A", "TR.W", "HU.W"]
        # Make sure the grid is still there.
        updatedTRList = self.GM_getWEInventory("Hazards")
        if timeRange not in updatedTRList:
            return

        hazGrid, hazKeys = self.getGrids(self.mutableID(), weName, "SFC", timeRange)
        for hazKey in hazKeys:
            subKeys = hazKey.split("^")
            for subKey in subKeys:
                keyNoETN = self._WindWWUtils.stripETN(subKey)
                if keyNoETN not in tropicalWindKeys:
                    continue

                self._hazUtils._removeHazard(weName, timeRange, subKey, mask)

    def fillHazardGaps(self, proposedWEName, timeRange):
        """
        Fills in any gaps in the Hazards inventory based on timeRange.
        """
        hazTRList = self.GM_getWEInventory("Hazards")
        if not hazTRList:
            # Make empty hazard grids with same timeRange as ProposedWW
            self.makeEmptyHazardGrid(timeRange)
        # See if we need to add a hazard grid at the end of the inventory
        elif hazTRList[-1].endTime().unixTime() < timeRange.endTime().unixTime():
            start = max(hazTRList[-1].endTime().unixTime(), timeRange.startTime().unixTime())
            end = timeRange.endTime().unixTime()
            tr = TimeRange.TimeRange(AbsTime.AbsTime(start), AbsTime.AbsTime(end))
            self.makeEmptyHazardGrid(tr)

        self.splitCmd(["Hazards"], timeRange)

    def execute(self, editArea, timeRange, varDict):
        # Get the coastal mask
        self._coastalMask = self.fetchNHCZoneMask()

        # Inland offices don't have a choice, so figure out if it's a coastal office and
        # make the GUI with choices if so.
        userOption = "Inland Only"
        if (self._cwaMask & self._coastalMask).any():
            variableList = []
            variableList.append(("Merge Hazards from:", "Coastal Only", "radio", ["Coastal Only", "Coastal and Inland"]))
            varDict = {}
            processVarList = ProcessVariableList.ProcessVariableList("Merge Hazards from:", variableList, varDict)
            status = processVarList.status()
            if status.upper() != "OK":
                self.cancel()
                return
            else:
                userOption = varDict["Merge Hazards from:"]

        # See if the Hazards WE is loaded in the GFE, if not abort the tool
        if not self._hazUtils._hazardsLoaded():
            self.statusBarMsg("Hazards Weather Element must be loaded in " + \
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

        proposedWEName = "ProposedTropWindWW"
        proposedDBName = self.mutableID()
        self._windKeys = ["<None>", "TR.A", "HU.A", "TR.W", "HU.A^TR.W", "TR.W^HU.A", "HU.W"]

        # Make sure we have a ProposedTropWindWW before we begin
        propTRList = self.GM_getWEInventory(proposedWEName, dbase=proposedDBName)
        if not propTRList:
            self.statusBarMsg("No ProposedTropWindWW Fcst grid found. Tool aborting.", "S")
            return

        inlandMask = self._cwaMask & np.logical_xor(self._cwaMask, self._coastalMask)

        # Check each site for any conflicts and return the list of conflicting sites
        if self.checkForAnyConflicts(self._cwaMask, proposedWEName, proposedDBName):
            self.statusBarMsg("Hazard conflicts between Hazard grid and ProposedTropWindWW from NHC.\n"\
                              "Check Wind ETN or for CF Hazards.", "U")
            return

        # Calculate the timeRange over which to fill gaps in the Hazards inventory
        hazTRList = self.GM_getWEInventory("Hazards")
        self.fillHazardGaps(proposedWEName, propTRList[0])

        # Get the ProposedWW grid
        propGrid, propKeys = self.getGrids(proposedDBName, proposedWEName, "SFC", propTRList[-1])

        # Fetch the Hazards grid composite over all times.
        compHazGrid, hazKeys = self._WindWWUtils.getAllTropicalHazards()
        # Calculate areas where the Proposed hazard is an upgrade from the Hazard grid

        # Calculate the area that will be updated
        upgradeMask = self.calcUpgradeMask(propGrid, propKeys, compHazGrid, hazKeys)

        # Modify the Hazards grids by areas defined by comparing the composite hazards to the Proposed hazards
        propNoneIndex = self.getIndex("<None>", propKeys)
        compNoneIndex = self.getIndex("<None>", hazKeys)
        compIsNone = compHazGrid == compNoneIndex

        # Re-fetch the hazards inventory, since it may have changed
        hazTRList = self.GM_getWEInventory("Hazards")
        for hazTR in hazTRList:
            # Don't operate on grids earlier than the Proposed grid time
            if hazTR.endTime() <= propTRList[0].startTime():
                continue

            hazGrid = self.getGrids(self.mutableID(), "Hazards", "SFC", hazTR)
            rankGrid = self._WindWWUtils.calcRankGrid(hazGrid, rankList=self._windKeys)
            noneIndex = self._windKeys.index("<None>")
            hazIsNone = rankGrid == noneIndex

            # Remove wind hazards from the upgradeMask (inland only) as these will be changed.
            if "Inland" in userOption:
                self.removeWindHazards("Hazards", hazTR, upgradeMask & inlandMask)

            # Add Wind hazards.
            for propKey in propKeys:
                propIndex = self.getIndex(propKey, propKeys)
                # Add Inland
                if "Inland" in userOption:
                    clipMask = (propGrid == propIndex) & inlandMask
                    inlandUpdateMask = (compIsNone & hazIsNone) | (~compIsNone & ~hazIsNone & upgradeMask)
                    updateMask = clipMask & inlandUpdateMask
                    if updateMask.any():
                        self._hazUtils._addHazard("Hazards", hazTR, propKey, updateMask, combine=1)
                # Add Coastal
                if "Coastal" in userOption:
                    clipMask = (propGrid == propIndex) & self._coastalMask
                    modifyMask = (~compIsNone & ~hazIsNone) | (compIsNone & hazIsNone)
                    mask = clipMask & modifyMask
                    if mask.any():
                        self.removeWindHazards("Hazards", hazTR, mask)
                        self._hazUtils._addHazard("Hazards", hazTR, propKey, mask, combine=1)

        # Separate the hazards so that forecasters won't need to do this manually.
        self._hazUtils._separateHazardGrids()