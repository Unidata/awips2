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
#
################################################################################

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["None"]

import HazardUtils
import LogStream
import ProcessVariableList
import TimeRange
import TropicalUtility
import numpy as np

VariableList = [("Merge Storm Surge Hazards?", "No", "radio", ["Yes", "No"]), ]

class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)


    # Extracts just wind hazards out of the ISC grids and returns a list
    # of hazard grid
    def makeISCWindHazardGrids(self):
        tropicalWindHazList = ["TR.W", "TR.A", "HU.W", "HU.A"]

        #  Get a list of all times which have WFO Hazards
        trList = self.GM_getWEInventory("Hazardswfo", "ISC")

        hazGridList = []
        hazTRList = []

        #  Initialize a new mask to clip Wind hazards to land-areas only
        cwaMaskNoMarine = self.encodeEditArea("WFO_AllLand")

        #  Get the current time
        currentTime = self._gmtime()

        #  Process all the times which have WFO Hazards
        for tr in trList:

            #  Skip all grids which are in the past
            if tr.endTime() < currentTime:
                continue

            #  Get all the WFO Hazards for this time
            wfoHazGrid = self.getGrids("ISC", "Hazardswfo", "SFC", tr)

            #  Get ready to construct a new Hazards grid
            iscHazGrid = self.empty(np.int8)
            iscHazKeys = ["<None>"]

            # For each wind hazard, extract the area from the ISC grid
            # and set those same points in our temporary grid.
            for haz in tropicalWindHazList:

                #  Find all the areas in the domain where this hazard exists
                keyMaskList = self.extractHazards(wfoHazGrid, haz)

                #  Process all areas identified to have current tropical hazard
                for hazKey, hazMask in keyMaskList:

                    #  Filter out non-tropical hazards
                    windHazKey = self.purifyKey(hazKey, tropicalWindHazList)

                    #  If there is nothing left to do, move on to next hazard
                    if windHazKey == "":
                        continue

                    #  Poke the values into the new ISC hazard grid, ignoring
                    #  marine-based tropical wind hazards
                    hazMask &= cwaMaskNoMarine
                    iscIndex = self.getIndex(windHazKey, iscHazKeys)
                    iscHazGrid[hazMask] = iscIndex

            #  If the current grid is equal to the last grid, just update the
            #  endTime of the last grid
            if len(iscHazKeys) > 1:
                if tr != trList[0]:
                    if len(hazGridList) > 0:
                        if np.array_equal(hazGridList[-1][0], iscHazGrid):
                            hazTRList[-1] = TimeRange.TimeRange(
                                        hazTRList[-1].startTime(), tr.endTime())
                            continue

            #  Save this grid, but only if it's not empty
            if len(iscHazKeys) > 1:
                hazTRList.append(tr)
                hazGridList.append((iscHazGrid, iscHazKeys))

        return hazTRList, hazGridList


    #  Checks for possible conflicts between existing NHC hazards and the
    #  WFO hazards extracted from the ISC grid. Returns the list of CWAs
    #  that conflict in any way with the NHC hazards.
    def checkForAnyConflicts(self, cwaList, propTR):

        #  Make a list of all WFOs with conflicting tropical hazards
        conflictingSites = []

        trList = self.GM_getWEInventory("Hazardswfo", "ISC")

        proposedGrid = self.getGrids("Fcst", "ProposedSS", "SFC", propTR)

        #  Make a dictionary containing each CWA's grid mask, so we only need
        #  to make it once.
        cwaMasks = {}
        for cwa in cwaList:
            cwaMasks[cwa] = self.encodeEditArea(cwa.upper())

        #  Get the current time
        currentTime = self._gmtime()

        #  Process all the available WFO Hazards
        for tr in trList:

            # We're only interested in future hazard grids
            if tr.endTime() < currentTime:
                continue

            #  Get all the WFO Hazards for this time
            hazardGrid = self.getGrids("ISC", "Hazardswfo", "SFC", tr)

            #  See if there are any hazard conflicts with any WFO
            for cwa in cwaList:

                #  If there are any tropical hazard conflicts
                if self.anyHazardConflicts(hazardGrid, proposedGrid,
                                           cwaMasks[cwa]):

                    #  Add this WFO to the conflicting office list - if its
                    #  not already there
                    if cwa not in conflictingSites:
                        conflictingSites.append(cwa)

        return conflictingSites


    def execute(self, varDict):

        #  Extract the wind hazards out of the WFO ISC hazard grids.
        #  This returns hazard-like grids to which we will add Surge hazards
        hazTRList, hazGrids = self.makeISCWindHazardGrids()

        propTRList = self.GM_getWEInventory("ProposedSS")
        if len(hazTRList) == 0 and  len(propTRList) == 0:
            self.statusBarMsg("No Proposed or WFO ISC grids found. Aborting.",
                          "S")
            return

        if len(propTRList) > 1:
            self.statusBarMsg("Multiple ProposedSS grids found. " +
                              "Please remove all but the current grid and re-run FinalizeHazards.",
                              "U")
            return

        if varDict["Merge Storm Surge Hazards?"] == "No" or len(propTRList) == 0:

            #  Delete all existing Hazard Grids since we're starting from scratch
            self.deleteCmd(["Hazards"], TimeRange.allTimes())

            for grid, tr in zip(hazGrids, hazTRList):
                self.createGrid("Fcst", "Hazards", "DISCRETE", grid, tr)

            # Save and publish the Hazards grids
            self.saveElements(["Hazards"])
            self.publishElements(["Hazards"], TimeRange.allTimes())

        else:
            propTR = propTRList[0]
            conflictingSites = self.checkForAnyConflicts(self._surgeWfos, propTR)

            #  If there are any conflicting sites, report which ones and punt
            if len(conflictingSites) > 0:
                siteStr = " ".join(conflictingSites)

                self.statusBarMsg("Conflicting hazards were found from the " +
                                  "following sites: " + siteStr, "U")
                return

            #=======================================================================
            #  If there were not any WFO ISC hazards found, make an empty grid
            #  based on the ProposedSS timeRange
            propGrid, propKeys = self.getGrids("Fcst", "ProposedSS", "SFC", propTR)

            if len(hazTRList) == 0:
                #  Make empty hazard grid
                hazTRList.append(propTR)
                hazGrid = self.empty(np.int8)
                hazKeys = ["<None>"]
                hazGrids.append((hazGrid, hazKeys))


            # save mask where there were SS hazards
            prevHazTRList = self.GM_getWEInventory("Hazards", timeRange=propTR)
            prevSSMask = self.empty(np.bool)
            for tr in prevHazTRList:
                hazardGrid, hazardKeys = self.getGrids("Fcst", "Hazards", "SFC", tr)

                #  Look for SS. hazards
                for key in hazardKeys:
                    if "SS." in key:
                        hazardIndex = self.getIndex(key, hazardKeys)
                        prevSSMask |= (hazardGrid == hazardIndex)


            #  Delete all existing Hazard Grids since we're starting from scratch
            self.deleteCmd(["Hazards"], TimeRange.allTimes())

            #  Save the new hazard grids to the database
            for grid, tr in zip(hazGrids, hazTRList):
                self.createGrid("Fcst", "Hazards", "DISCRETE", grid, tr)

            #  If we could not find a time range for the Hazards grid
            if len(hazTRList) == 0:

                #  Use the time range from the PropossedSS grid instead
                hazTRList = propTRList

            #  Now add ProposedSS keys back into the hazard grids
            for hazTR in hazTRList:

                # Only add Proposed hazards where the Hazards overlap
                if not hazTR.overlaps(propTR):
                    continue

                #  Process all the keys in the PropossedSS grids
                for propIndex, propKey in enumerate(propKeys):

                    #  If this is a "<None>" key, ignore it
                    if "None" in  propKey:
                        continue

                    #  Get the area where this hazard is located, and it intersects
                    #  with land
                    mask = propGrid == propIndex

                    #  Combine surge hazard with current hazards
                    self._hazUtils._addHazard("Hazards", hazTR, propKey, mask,
                                              combine=1)

            # Copy new ProposedSS grid to InitialSS
            proposedSS = self.getGrids("Fcst", "ProposedSS", "SFC", propTR)
            self.createGrid("Fcst", "InitialSS", "DISCRETE", proposedSS, propTR)

            #  Get the components of each grid
            propGrid, propKeys = proposedSS

            #  Get the index we need to find areas of no hazards
            noneIndex = self.getIndex("<None>", propKeys)

            #  See where there are hazards (should only be SS hazards)
            propSSMask = (propGrid != noneIndex)

            # Mask any area that has or previously had an SS hazard
            anyChanges = prevSSMask | propSSMask

            #  Save all the grids we modified, clean up after collaboration
            self.unloadWE("Fcst", "CollabDiffSS", "SFC")
            self.saveElements(["Hazards", "InitialSS", "ProposedSS"])

            #  Publish all the grids we will need to keep
            self.publishElements(["Hazards", "InitialSS", "ProposedSS"],
                             TimeRange.allTimes())

            #  If we are not in test mode - send the final message
            if not self._testMode:
                self.notifyWFOs("Hazards", anyChanges)

        # let forecaster know the procedure is completed
        self.statusBarMsg("Procedure completed. Run CreateNatlTCVZoneGroups.", "A")

