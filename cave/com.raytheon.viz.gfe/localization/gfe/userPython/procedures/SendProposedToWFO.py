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
# May 16, 2019 21020      tlefebvr    Modified to send Wind hazard messages
#                                     as well as Storm Surge
# May 16, 2019 20464      tlefebvr    Changed interface to support any set of 
#                                     AT* bulletins and added code to run the
#                                     script to send JSON files t AWIPS WAN.
# Apr 21, 2020 20464      tlefebvr    Added EP bins to GUI.
# Apr 21, 2020 20464      tlefebvr    Added code to check for empty GUI lists.
# May 01, 2020 22033      tlefebvr    Added support for CPHC
# May 05, 2020 22033      tlefebvr    Added code to text command to indicate gfe
#                                     PRACTICE mode or not. Added StormSurge to
#                                     CPHC menu.
# May 06, 2020 22033      tlefebvr    Code clean-up.
# May 12, 2020 22033      psantos     Adjusted code made during vlab down.
# May 14, 2020 22033      tlefebvr    Modified to use ***Sites methods in WWUTils
# May 15, 2020            psantos     Added code to always send json file in case 
#                                     only coastal hazards being pushed with no guidance grid.
# May 21, 2020            tlefebvre   Addressed code review comments. 
# May 30, 2020            tlefebvre   Changed wfoList to all wind and stormSurge
#                                     sites defined in TropicalUtility. 
# June 1, 2020            tlefebvre   Removed import of Set as it is built-in.
#
################################################################################

MenuItems = ["Populate"]

import TropicalUtility
import ProcessVariableList
import numpy as np
import WindWWUtils
import TimeRange
import os

class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._dbss = dbss

    def sendStormSurgeMessageToWFOs(self):
        """
        Copies the Proposed grid into the Initial grid. Save the elements and
        send a notification to the WFOs.
        """
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
                continue
            # Add to saved element list if element exists
            savedElements.append(weName)
            #  Get the limits for this field
            minLimit, maxLimit = self.getParmMinMaxLimits(self._mutableID, weName)

            #  Process all the grids we have left
            for tr in trList:
                #  Get the grid for this time range
                grid = self.getGrids(self._mutableID, weName, "SFC", tr)
                #  Mask the grid outside of the mask area
                grid[~ssMask] = minLimit
                #  Put the masked grid back into this time range
                self.createGrid(self._mutableID, weName, "SCALAR", grid, tr)

        #  Save those fields
        self.saveElements(savedElements)

        #  Notify the WFOS, as appropriate
        testMode = self._testMode
        if not testMode:
            self.notifyWFOs("ProposedSS", anyChanges=None)
            self.statusBarMsg("Procedure completed. Sent pop-up banners to WFOs", "A")

    def sendWindMessageToWFOs(self, stormList, siteID):
        """
        Send a message to any WFO that overlaps any of the hazards defined in the 
        ProposedTropWindWW grid. This alerts forecasters that guidance is available.
        """
        if siteID in self._WindWWUtils.NHCSites():
            wfoList = list(set(self._surgeWfos + self._windWfos))
        elif siteID in self._WindWWUtils.HFOSites():
            wfoList = ["HFO"]
        elif siteID in self._WindWWUtils.GUMSites():
            wfoList = ["GUM"]
        #print("WFO list:", wfoList)
        # Push the JSON file to the sites
        for storm in stormList:
            mode = " operational"
            if self.gfeOperatingMode() == "PRACTICE":
                mode = " practice"               
            execStr = "/localapps/runtime/RecommendWindWatchWarning/nhc_pushJsonFile.sh MIAJSN" + storm + mode
            os.system(execStr)

        weName = "ProposedTropWindWW"
        level = "SFC"
        trList = self.GM_getWEInventory(weName, self.mutableID(), level)

        if len(trList) == 0:
            self.statusBarMsg("ProposedTropWindWW not found. Sending grid aborted. But Json file with coastal hazards if any sent.", "S")
            return

        hazGrid, hazKeys = self.getGrids(self.mutableID(), weName, level, trList[0])
        noneIndex = self.getIndex("<None>", hazKeys)

        # Find places where any hazard is defined
        hazMask = hazGrid != noneIndex

        wfosToSend = []
        for wfo in wfoList:
            try:
                mask = self.encodeEditArea(wfo)
            except AttributeError:
                self.statusBarMsg("Warning! Edit Area for " + wfo + " was not found", "S")
                continue

            # See if this WFO overlaps with the hazard mask
            overlap = mask & hazMask

            if not overlap.any(): # skip this WFO if no overlap
                continue

            wfosToSend.append(wfo)

        testMode = self._testMode
        message = weName + " NHC wind guidance have been sent."
        if not testMode:
            self.sendMessageToWfos(wfosToSend, message, testMode)
            self.statusBarMsg("Procedure completed. Sent pop-up banners to WFOs", "A")
            
        savedElements = ["ProposedTropWWGuidance","ProposedTropWindWW"]
        self.saveElements(savedElements)
            
        return

    def execute(self, varDict):
        
        # Instantiate the WindWWUtils modules
        self._WindWWUtils = WindWWUtils.WindWWUtils(self._dbss)

        variableList = []
        
        siteID = self.getSiteID()
        basinBins = self._WindWWUtils._basinBins
        variableList.append(("Hazard Type:", ["Wind Hazards"], "check", ["Storm Surge Hazards", "Wind Hazards"]))
        title = "Select Hazard Type and Bin"
        if siteID in self._WindWWUtils.NHCSites():
            variableList.append(("Select AT Storms:", [], "check", basinBins["Atlantic"]))
            variableList.append(("Select EP Storms:", [], "check", basinBins["Eastern Pacific"]))
        elif siteID in self._WindWWUtils.HFOSites():
            variableList.append(("Select CP Storms:", [], "check", basinBins["Central Pacific"]))
        elif siteID in self._WindWWUtils.GUMSites():
            variableList.append(("Select WP Storms:", [], "check", basinBins["Western Pacific"]))
           
        # Display the GUI
        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList(
            title, variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            self.cancel()

        # Get the Hazard Type
        if siteID in self._WindWWUtils.NHCSites():
            hazTypesToSend = varDict["Hazard Type:"]
        elif siteID in self._WindWWUtils.HFOSites():
            hazTypesToSend = "Wind Hazards"
        elif siteID in self._WindWWUtils.GUMSites():
            hazTypesToSend = "Wind Hazards"
            
        if not hazTypesToSend:
            self.statusBarMsg("Please select a Hazard type.", "S")
            return
        
        stormList = []
        if siteID in self._WindWWUtils.NHCSites():
            stormList += varDict["Select AT Storms:"]
            stormList += varDict["Select EP Storms:"]
        elif siteID in self._WindWWUtils.HFOSites():
            stormList += varDict["Select CP Storms:"]
        elif siteID in self._WindWWUtils.GUMSites():
            stormList += varDict["Select WP Storms:"]
        if not stormList:
            self.statusBarMsg("Please select a storm.", "S")
            return
                
        if "Storm Surge Hazards" in hazTypesToSend:
            self.sendStormSurgeMessageToWFOs()
        if "Wind Hazards" in hazTypesToSend:
            self.sendWindMessageToWFOs(stormList, siteID)

        return

