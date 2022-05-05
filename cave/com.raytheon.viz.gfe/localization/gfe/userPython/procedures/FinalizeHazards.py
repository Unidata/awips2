# ------------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# FinalizeHazards.py
# Version 3.0 - 15 April 2016 - Code cleanup and refactor
#
# Author: Nate Hardin, Tom LeFebvre
# ------------------------------------------------------------------------------
#
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# Sep 19, 2016 19293      randerso    Initial baseline check in
# Mar 07, 2017 29986      randerso    Fix issue with no trop wind in Hazardswfo
# Apr 10, 2017 6233       randerso    Reworked per Shannon and Pablo's requirements
# Jun 28, 2017 20102      ryu         Remind user to have ProposedSS grid created if it 
#                                     is missing when merge storm surge is selected.
# Jul 07, 2017 20102      ryu         Recreate hazard grids from existing SS hazards
#                                     when ISC wind grids are missing and ProposedSS
#                                     grid is not merged
#
################################################################################

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = [""]

import TimeRange
import TropicalUtility
import numpy as np

VariableList = [("Merge Storm Surge Hazards?", "No", "radio", ["Yes", "No"]), ]

class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)

    # Extracts a selected set of hazards for a specified weather element
    def extractSelectedHazards(self, hazardList, element, database):

        #  Get an inventory of the source element
        trList = self.GM_getWEInventory(element, database)

        hazGridList = []
        hazTRList = []

        #  Initialize a new mask to clip Wind hazards to land-areas only
        cwaMaskNoMarine = self.encodeEditArea("WFO_AllLand")

        #  Get the current time
        currentTime = self._gmtime()

        #  For each time range in the source inventory
        for tr in trList:

            #  Skip all grids which are in the past
            if tr.endTime() < currentTime:
                continue

            #  Get get the source grid
            sourceGrid = self.getGrids(database, element, "SFC", tr)

            #  Get ready to construct a new Hazards grid
            newGrid = self.empty(np.int8)
            newKeys = ["<None>"]

            # For each hazard, extract the area from the source grid
            # and set those same points in our temporary grid.
            for haz in hazardList:

                #  Find all the areas in the domain where this hazard exists
                keyMaskList = self.extractHazards(sourceGrid, haz)

                #  Process all areas identified to have current tropical hazard
                for hazKey, hazMask in keyMaskList:

                    #  Filter out undesired keys
                    pureKey = self.purifyKey(hazKey, hazardList)

                    #  If there is nothing left to do, move on to next hazard
                    if pureKey == "":
                        continue

                    #  Poke the values into the new hazard grid, ignoring
                    #  marine-based hazards
                    hazMask &= cwaMaskNoMarine
                    iscIndex = self.getIndex(pureKey, newKeys)
                    newGrid[hazMask] = iscIndex

            #  If the current grid is equal to the last grid, just update the
            #  endTime of the last grid
            if len(newKeys) > 1:
                if tr != trList[0]:
                    if len(hazGridList) > 0:
                        if np.array_equal(hazGridList[-1][0], newGrid):
                            hazTRList[-1] = TimeRange.TimeRange(
                                        hazTRList[-1].startTime(), tr.endTime())
                            continue

            #  Save this grid, but only if it's not empty
            if len(newKeys) > 1:
                hazTRList.append(tr)
                hazGridList.append((newGrid, newKeys))

        return hazTRList, hazGridList


    #  Checks for possible conflicts between existing NHC hazards and the
    #  WFO hazards extracted from the ISC grid. Returns the list of CWAs
    #  that conflict in any way with the NHC hazards.
    def checkForAnyConflicts(self, cwaList, wfoGrid, nhcGrid):

        #  Make a set of all WFOs with conflicting tropical hazards
        conflictingSites = set()

        for cwa in cwaList:
            try: 
                cwaMask = self.encodeEditArea(cwa.upper())
            except:
                continue
            
            if cwaMask is None:
                continue
                
            #  If there are any tropical hazard conflicts
            if self.anyHazardConflicts(wfoGrid, nhcGrid, cwaMask):
                #  Add this WFO to the conflicting office list
                conflictingSites.add(cwa)

        return conflictingSites


    def execute(self, varDict):

        #  Extract the wind hazards out of the WFO ISC hazard grids.
        #  This returns hazard-like grids to which we will add Surge hazards
        windTRList, windGrids = self.extractSelectedHazards(["TR.W", "TR.A", "HU.W", "HU.A"],
                                                            "Hazardswfo", "ISC")

        propTRList = self.GM_getWEInventory("ProposedSS")

        if len(propTRList) > 1:
            self.statusBarMsg("Multiple ProposedSS grids found. " +
                              "Please remove all but the current grid and re-run FinalizeHazards.",
                              "U")
            return


        mergeSurge = varDict["Merge Storm Surge Hazards?"] == "Yes"
        if not mergeSurge:
            # Extract SS grids from existing Hazards
            ssTRList, ssGrids = self.extractSelectedHazards(["SS.A", "SS.W"], "Hazards", "Fcst")

            prevSSMask = None

            #  Create empty grids over SS time ranges if there are no ISC wind grids
            if len(windTRList) == 0 and len(ssTRList) > 0:
                grid = (self.empty(dtype=np.int8), ["<None>"])
                windTRList = ssTRList
                windGrids = [grid] * len(windTRList)

            # elements to be saved/published
            elementsToSave = ["Hazards"]

        else:  # Merge surge == Yes
            # Ensure there is a ProposedSS grid
            if len(propTRList) == 0:
                msg = "There are no ProposedSS grids. "\
                      "A None ProposedSS grid is required to cancel all SS W/Ws. "\
                      "Have SSU create a None grid and Save, then rerun FinalizeHazards."
                self.statusBarMsg(msg, "U")
                return

            # save mask where there were SS hazards
            propTR = propTRList[0]
            prevHazTRList = self.GM_getWEInventory("Hazards", timeRange=propTR)
            prevSSMask = self.empty(np.bool)
            for tr in prevHazTRList:
                hazardGrid, hazardKeys = self.getGrids("Fcst", "Hazards", "SFC", tr)

                #  Look for SS. hazards
                for key in hazardKeys:
                    if "SS." in key:
                        hazardIndex = self.getIndex(key, hazardKeys)
                        prevSSMask |= (hazardGrid == hazardIndex)


            #  If there were not any WFO ISC hazards found, make an empty grid
            #  based on the ProposedSS timeRange
            if len(windTRList) == 0:
                #  Make empty hazard grid
                windTRList.append(propTR)
                hazGrid = self.empty(np.int8)
                hazKeys = ["<None>"]
                windGrids.append((hazGrid, hazKeys))

            elementsToSave = ["Hazards", "InitialSS", "ProposedSS"]

            # Extract SS grids from ProposedSS
            ssTRList, ssGrids = self.extractSelectedHazards(["SS.A", "SS.W"], "ProposedSS", "Fcst")

        # Check for conflicts
        conflictingSites = set()
        for windTR, windGrid in zip(windTRList, windGrids):
            for ssTR, ssGrid in zip(ssTRList, ssGrids):
                if ssTR.overlaps(windTR):
                    conflictingSites.update(self.checkForAnyConflicts(self._surgeWfos, windGrid, ssGrid))

        #  If there are any conflicting sites, report which ones and punt
        if len(conflictingSites) > 0:
            siteStr = " ".join(conflictingSites)

            self.statusBarMsg("Conflicting hazards were found from the " +
                              "following sites: " + siteStr, "U")
            return

        #  Delete all existing Hazard Grids since we're starting from scratch
        self.deleteCmd(["Hazards"], TimeRange.allTimes())

        #  Save the new hazard grids to the database
        for tr, grid in zip(windTRList, windGrids):
            self.createGrid("Fcst", "Hazards", "DISCRETE", grid, tr)

        #  Now add SS keys back into the hazard grids
        for hazTR in windTRList:
            for ssTR, ssGrid in zip(ssTRList, ssGrids):

                # Only add SS hazards where the Hazards overlap
                if not hazTR.overlaps(ssTR):
                    continue

                #  Process all the keys in the SS grid
                grid, keys = ssGrid
                for index, key in enumerate(keys):

                    #  If this is a "<None>" key, ignore it
                    if "None" in  key:
                        continue

                    #  Get the area where this hazard is located
                    mask = grid == index

                    #  Combine surge hazard with current hazards
                    self._hazUtils._addHazard("Hazards", ssTR, key, mask, combine=1)

        if mergeSurge:
            # Copy new ProposedSS grid to InitialSS
            proposedSS = self.getGrids("Fcst", "ProposedSS", "SFC", propTR)
            self.createGrid("Fcst", "InitialSS", "DISCRETE", proposedSS, propTR)

        #  Save and publish all the grids we will need to keep
        self.saveElements(elementsToSave)
        self.publishElements(elementsToSave, TimeRange.allTimes())

        # clean up after collaboration
        self.unloadWE("Fcst", "CollabDiffSS", "SFC")

        #  send the final message
        self.notifyWFOs("Hazards", prevSSMask)

        # let forecaster know the procedure is completed
        self.statusBarMsg("Procedure completed. Run CreateNatlTCVZoneGroups.", "A")
