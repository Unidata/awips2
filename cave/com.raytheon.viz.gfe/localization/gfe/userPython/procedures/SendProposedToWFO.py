# ------------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# SendProposedToWFO
# Version 3.0 - Code cleanup and refactoring
# Author: Tom LeFebvre and Pablo Santos
# ------------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# Sep 19, 2016 19293      randerso    Initial baseline check in
# Feb 21, 2017 29544      randerso    Set anyChanges to None when calling
#                                     notifyWFOs so only those WFOs with active
#                                     surge event are notified
#
################################################################################

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["None"]

import ProcessVariableList
import TropicalUtility
import numpy as np


class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)


    def execute(self):

        #  Create a GUI
#         variableList = []
#         variableList.append(("Send pop-up banner to WFOs", "Yes", "radio",
#                              ["Yes", "No"]))
# 
#         # Display the GUI
#         # Doing this here so we get OK/Cancel instead of Run/Run Dismiss/Cancel
#         varDict = {}
#         processVarList = ProcessVariableList.ProcessVariableList(
#             "Send WFO pop-up banner", variableList, varDict)
#         status = processVarList.status()
#         if status.upper() != "OK":
#             self.cancel()

        # Copy proposed to initial grid.
        propWEName = "ProposedSS"
        trList = self.GM_getWEInventory(propWEName, self._mutableID)
        if len(trList) == 0:
            self.statusBarMsg("No " + propWEName + " grid found", "S")
            return

        propGrid, propKeys = self.getGrids(self._mutableID, propWEName, "SFC",
                                           trList[0])

        # Fetch the storm surge edit area and make the mask
        ssEditArea = self.getEditArea("StormSurgeWW_EditArea")

        boolMask = self.empty(np.bool)
        ssMask = self.encodeEditArea(ssEditArea)

        # Set all points to None outside the StormSurge edit area
        noneIndex = self.getIndex("<None>", propKeys)
        propGrid[~ssMask] = noneIndex
        self.createGrid(self._mutableID, propWEName, "DISCRETE",
                        (propGrid, propKeys), trList[0])

        # Replace the Initial grid with proposed
        initWEName = propWEName.replace("Proposed", "Initial")
        self.createGrid(self._mutableID, initWEName, "DISCRETE",
                        (propGrid, propKeys), trList[0])

        #  Keep only the last proposed grid
        self.removeEarlierTRs(initWEName)

        #  Handle fields which should be masked to the storm surge area only
        maskedElements = ["InundationMax", "InundationTiming",
                          "SurgeHtPlusTideMHHW", "SurgeHtPlusTideMLLW",
                          "SurgeHtPlusTideMSL", "SurgeHtPlusTideNAVD"]
        
        savedElements = ["ProposedSS", "InitialSS"]
        
        #  Process those masked fields
        for weName in maskedElements:
            trList = self.GM_getWEInventory(weName)

            #  If there is nothing to do - say so
            if len(trList) == 0:
#                self.statusBarMsg("No " + weName + " grid found.", "S")
                continue
            
            # Add to saved element list if element exists
            savedElements.append(weName)
            
            #  Get the limits for this field
            minLimit, maxLimit = self.getParmMinMaxLimits(self._mutableID,
                                                          weName)

            #  Process all the grids we have left
            for tr in trList:

                #  Get the grid for this time range
                grid = self.getGrids(self._mutableID, weName, "SFC", tr)

                #  Mask the grid outside of the mask area
                grid[~ssMask] = minLimit

                #  Put the masked grid back into this time range
                self.createGrid(self._mutableID, weName, "SCALAR", grid, tr)

        #  Make a list of fields which will be automatically saved
#        savedElements = ["ProposedSS", "InitialSS"] + maskedElements

        #  Save those fields
        self.saveElements(savedElements)

        #  Notify the WFOS, as appropriate
        testMode = self._testMode
        if not testMode:
            self.notifyWFOs("ProposedSS", anyChanges=None)
            self.statusBarMsg("Procedure completed. Sent pop-up banners to WFOs", "A")
