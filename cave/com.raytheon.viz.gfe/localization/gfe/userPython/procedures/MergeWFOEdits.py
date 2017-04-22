# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# MergeWFOEdits - Version 3.1
#
# Author: LeFebvre, Santos
#
# ----------------------------------------------------------------------------
#
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# Apr 13, 2016            LeFebvre    Code cleanup and refactor
# Sep 10, 2016            Santos      Fix fetching of split wfo grids from
#                                     ProposedSSwfo ISC db
# Sep 19, 2016 19293      randerso    Initial baseline check in
#
########################################################################

import TimeRange
import TropicalUtility
import numpy as np


# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["None"]

bogusWFO = ["XYZYX"] # This allows no default selected for the check buttons when the GUI appears
VariableList = [
                ("WFOs", bogusWFO, "check", ["CAR", "GYX", "BOX", "OKX", "PHI", "LWX",
                                             "AKQ", "MHX", "ILM", "CHS", "JAX", "MLB",
                                             "MFL", "KEY", "TBW", "TAE", "MOB", "LIX",
                                             "LCH", "HGX", "CRP", "BRO"]),
                ]

class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)

    # Fetch the ProposedSSwfo grid comprising the last grid where any site has a hazard defined.
    def fetchProposedWFOGrid(self, cwaList):

        # Fetch the WFO ISC grid inventory
        weName = "ProposedSSwfo"
        wfoTRs = self.GM_getWEInventory(weName, "ISC")

        # No grids, time to go home
        if len(wfoTRs) == 0:
            self.statusBarMsg("No " + weName +  " grids found to merge into ProposedSS.", "U")
            return None

        cGrid = self.empty(np.int8)
        cKeys = ["<None>"]

        wfoTRs.reverse()   # process the grids latest to oldest

        foundCWAs = []
        for tr in wfoTRs:
            #print tr
            wfoSSGrid = self.getGrids("ISC", weName, "SFC", tr)
            hazGrid, hazKeys = wfoSSGrid
            #print "keys:", hazKeys
            for cwa in cwaList:
                eacwa = "ISC_" + cwa
                #print cwa

                # Skip cwas we've already found
                if cwa in foundCWAs:
                    continue

                ea = self.getEditArea(eacwa)
                if not ea:
                    self.statusBarMsg("Edit area for CWA " + cwa + " not found.", "S")
                    continue

                cwaMask = self.encodeEditArea(ea)
                for hazKey in hazKeys:
                    if hazKey == "<None>":
                        continue
                    print "hazKey", hazKey
                    hazIndex = self.getIndex(hazKey, hazKeys)
                    hazMask = (hazGrid == hazIndex)
                    overlap = hazMask & cwaMask
                    if overlap.any():
                        newIndex = self.getIndex(hazKey, cKeys)
                        print "added key now:", cKeys
                        cGrid[overlap] = newIndex
                        if not cwa in foundCWAs:
                            foundCWAs.append(cwa)

        return cGrid, cKeys

    def execute(self, editArea, varDict):

        cwas = varDict["WFOs"]
        if cwas == bogusWFO:
            self.statusBarMsg("Please select a valid WFO.", "U")
            return

        # Make a time range to find the initial storm surge hazards
        # Truncate to top of last hour
        start = int(self._gmtime().unixTime() / 3600) * 3600
        end = start + 48 * 3600 # 2 days later
        timeRange48Hour = self.GM_makeTimeRange(start, end)

        # Fetch the proposed grid and the WFO ISC grid
        ssTRs = self.GM_getWEInventory("ProposedSS","Fcst" )
        if len(ssTRs) > 0:
            propSSGrid = self.getGrids("Fcst", "ProposedSS", "SFC", ssTRs[-1])
        else:
            self.statusBarMsg("No PropsedSS grids found.", "U")
            return

        # Fetch InitialSS grid
        ssTRs = self.GM_getWEInventory("InitialSS", "Fcst" )
        if len(ssTRs) > 0:
            initSSGrid = self.getGrids("Fcst", "InitialSS", "SFC", ssTRs[-1])
        else:
            self.statusBarMsg("No InitialSS grids found.", "U")
            return

        # Replaced above with this code to fetch a combined Hazards grid
        wfoSSGrid = self.fetchProposedWFOGrid(cwas)

        # Calculate the overlap of selected WFO's ISC areas and selected area
        selectedMask = self.encodeEditArea(editArea)

        # If no points selected, select all points
        if not selectedMask.any():
            selectedMask = self.newGrid(True, np.bool)

        #  Make an empty mask grid, so we can use it to add up all the WFO areas
        cwaMask = self.newGrid(False, np.bool)

        #  Process all the selected CWAs
        for cwa in cwas:

            #  Get the ISC edit area mask for this office
            mask = self.encodeEditArea("ISC_" + cwa.upper())
            cwaMask = cwaMask | mask

            # Check for conflicts with just this CWA
            if self.anyHazardConflicts(initSSGrid, wfoSSGrid, mask):
                self.statusBarMsg(
                    "WFO " + cwa + " conflicts with the InitialSS grid." + \
                    " Check for discrepancy in either event code or ETN in " + \
                    "incoming WFO ISC grids.", "S")
                return

        # Use the intersection of the CWA areas and the selected area
        selectedMask = selectedMask & cwaMask

        # Further reduce the area to just the StormSurge editArea
        ssMask = self.encodeEditArea("StormSurgeWW_EditArea")
        selectedMask = selectedMask & ssMask

        # Now check for conflicts with each WFO and the InitialSS grid.
        wfoKeys = wfoSSGrid[1]
        noneWfoIndex = self.getIndex("<None>", wfoKeys)

        # Finally merge the WFO ISC grids into the ProposedSS grid
        wfoGrid, wfoKeys = wfoSSGrid
        ssGrid, ssKeys = propSSGrid
        ssGrid = ssGrid.copy()  # make a copy so we don't affect the original
        ssKeys = list(ssKeys)   # make a copy

        #  Process all the WFO proposed hazards
        for wfoIndex, wfoKey in enumerate(wfoKeys):

            #  Identify where this WFO hazard intersects the selected area
            mask = (wfoGrid == wfoIndex) & selectedMask

            #  Get the index of this hazard key within the NHC hazards
            ssIndex = self.getIndex(wfoKey, ssKeys)

            #  Convert the WFO key index to an NHC key index
            ssGrid[mask] = ssIndex

        #  Put the merged ProposedSS back together
        proposedGrid = (ssGrid, ssKeys)

        #  Delete any existing collaboration difference grids
        self.unloadWE("Fcst", "CollabDiffSS", "SFC")

        #  Delete all versions of this weather element
        self.deleteCmd(["ProposedSS"], TimeRange.allTimes())

        #  Create the grid, so we can see it
        self.createGrid("Fcst", "ProposedSS", "DISCRETE", proposedGrid,
                        timeRange48Hour)

        #  Calculate the new difference grid for this time range
        self.calcDiffGrid(initSSGrid, proposedGrid, "CollabDiffSS",
                          timeRange48Hour)

