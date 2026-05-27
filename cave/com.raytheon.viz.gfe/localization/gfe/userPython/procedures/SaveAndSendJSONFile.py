# ------------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# SaveAndSendJSONFile
# Version 3.1
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
# May 14, 2020 22033      tlefebvr    Modified to use ***Sites methods in WWUtils
# May 15, 2020            psantos     Added code to always send json file in case
#                                     only coastal hazards being pushed with no guidance grid.
# May 21  2020            tlefebvre   Addressed code review comments.
# May 30  2020            tlefebvre   Changed wfoList to all wind and stormSurge
#                                     sites defined in TropicalUtility.
# Jun  1  2020            tlefebvre   Removed import of Set as it is built-in.
# Jul 23  2020            tlefebvre   WFOs are computed using JSON file instead of grid.
# Jul 24  2020            tlefebvre   Enforced that one and only one storm can be selected.
# Aug 13  2020            tlefebvre   Added archiving advisory to local file.
# May 10 2021 22531       tlefebvre   Changed data file location to edex/data/share.
# Jun 21 2021 22531       tlefebvre   Remove references to Storm Surge hazards.
# Jul 29 2021 22531       tlefebvre   Final code clean-up before check-in.
# Aug 24 2021 22531       tlefebvre   Fixed logic around makedirs and building paths.
# Dec  6 2021 22531       tlefebvre   Redesigned tool to save Proposed and Guidance grids
#                                     to JSON file before sending notification to WFOs.
# Dec  9 2021 22531       tlefebvre   Added changes to support "inlandZoneDict".
# Dec 10 2021 22531       tlefebvre   Fixed minor issue with fetching zones.
# Jan  6 2022 22531       tlefebvre   RecommendWindWindWW no longer required to run.
# Jan 10 2022 22531       tlefebvre   Re-added SaveToJSONFile.
# Jan 20 2022 22531       psantos/scamp Added 3rd arg with gfesiteID when calling pushJsonFile
# Feb 24 2022 22531       lefebvre    Added call to display saved JSON hazards and a GUI to prompt
#                                     forecaster to check hazards before disseminating.
# Apr 13 2022 22531       tlefebvre   Made a few changes for Python3 compatibility.
# Sep 15 2022 22531       santos/scamp Fixes post 21.4.1-13 testing.
# May  4 2023 2036298     santos/scamp Modified code to take out coastal hazards from the
#                                      guidance grid. Kept those internal to NHC.
################################################################################

MenuItems = ["None"]

import TropicalUtility
import ProcessVariableList
import numpy as np
import WindWWUtils
import ZoneMap
import os
import errno
import json


class Procedure(TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._dbss = dbss
        # Instantiate the WindWWUtils modules
        self._WindWWUtils = WindWWUtils.WindWWUtils(self._dbss)
        self._zoneMap = ZoneMap.ZoneMap(self._dbss)
        self._path = self._WindWWUtils.getDataFilePath()

    def getETN(self, stormInfo, pil):
        """
        Lookup and return the ETN from the stormName and stormInfo
        """
        etnDict = self._etnDict

        stormNumber = stormInfo["stormNumber"]
        stormID = stormInfo["stormID"]
        etnValue = etnDict[stormID[0:2]]

        return str(stormNumber + etnValue)

    def stripETN(self, hazKey):
        """
        Remove the ETN from the hazKey and return the result.
        """
        while ":" in hazKey:
            pos = hazKey.find(":")
            if pos >= 0:
                delStr = hazKey[pos:pos + 5]
                hazKey = hazKey.replace(delStr, "")
        return hazKey

    def saveJSONToFile(self, advisoryDict):
        """
        Writes the specified advisory info to a local file for archiving.
        """
        stormName = advisoryDict["stormName"]
        advisoryNumber = advisoryDict["advisoryNumber"]
        pil = advisoryDict["pil"]

        practiceMode = self.gfeOperatingMode() == "PRACTICE"

        filepath = self._WindWWUtils.getArchiveFilePath(practiceMode, stormName)

        try:
            os.makedirs(filepath)
        except OSError as e:
            if e.errno != errno.EEXIST:
                raise

        filename = os.path.join(filepath, f"{pil}_{advisoryNumber}")
        with open(filename, 'w') as outfile:
            json.dump(advisoryDict, outfile, indent=4)

    def sendWindMessageToWFOs(self, storm, siteID):
        """
        Send a message to any WFO that overlaps any of the hazards defined in the
        ProposedTropWindWW grid. This alerts forecasters that guidance is available.
        Also saves the elements "ProposedTropWindWW" and "ProposedTropWWGuidance".
        """
        if siteID in self.NHCSites():
            wfoList = list(set(self._windWfos))
        elif siteID in self.HFOSites():
            wfoList = ["HFO"]
        elif siteID in self.GUMSites():
            wfoList = ["GUM"]

        # Push the JSON file to the sites
        mode = " operational "
        if self.gfeOperatingMode() == "PRACTICE":
            mode = " practice "
        execStr = os.path.join(self._path, "scripts", f"nhc_pushJsonFile.sh MIAJSN{storm}{mode}{siteID}")
        os.system(execStr)

        wfosToSend = []
        hazardList = ["TR.A", "HU.A", "TR.W", "TR.W^HU.A", "HU.W"]
        stormInfoDict = self._WindWWUtils.fetchStormInfo(hazardList)

        # Fetch the stormInfo from the JSON file
        if storm not in stormInfoDict:
            self.statusBarMsg(f"Error!! Bulletin  {storm} not found in stormInfo.", "S")
            return

        # Get both the coastal and inland zones into a single list
        coastalDict = {}
        inlandDict = {}
        if "zoneDict" in stormInfoDict[storm]:
            coastalDict = stormInfoDict[storm]["zoneDict"]
        if "inlandZoneDict" in stormInfoDict[storm]:
            inlandDict = stormInfoDict[storm]["inlandZoneDict"]
        zoneList = []
        for haz in coastalDict:
            zoneList += coastalDict[haz]
        for haz in inlandDict:
            zoneList += inlandDict[haz]
        zoneMask = self.empty(np.bool)
        # Make a mask covering all the zones
        for zone in zoneList:
            try:
                zoneMask |= self.encodeEditArea(zone)
            except AttributeError:
                self.statusBarMsg(f"Warning! Edit Area for  {zone} was not found", "S")
                continue

        # Figure out which WFOs are included in the list of zones
        for wfo in wfoList:
            try:
                wfoMask = self.encodeEditArea(wfo)
            except AttributeError:
                self.statusBarMsg(f"Warning! Edit Area for {wfo} was not found", "S")
                continue
            if (wfoMask & zoneMask).any():
                if wfo not in wfosToSend:
                    wfosToSend.append(wfo)

        testMode = self._testMode
        weName = "ProposedTropWindWW"
        message = weName + " ATTENTION: NHC has sent wind watches and warnings that impact "
        message += f"portions of your area. Run CopyProposedTropWindWW with {storm} "
        message += "and review the output ProposedTropWindWW grid."
        if not testMode:
            self.sendMessageToWfos(wfosToSend, message, testMode)
            self.statusBarMsg("Procedure completed. Sent pop-up banners to WFOs", "A")

        savedElements = ["ProposedTropWWGuidance", "ProposedTropWindWW"]
        self.saveElements(savedElements)

        return

    def saveZonesToJSONFile(self, zonesDict):
        """
        Save the contents of the zonesDict into the JSON file.
        """
        siteID = self.getSiteID()

        for weName in zonesDict:
            if weName == "ProposedTropWWGuidance":
                self._stormInfo["guidanceZoneDict"] = zonesDict[weName]
                self._stormInfo["guidanceZoneAllDict"] = zonesDict["ProposedTropWWGuidanceAll"]
            elif weName == "ProposedTropWindWW":
                self._stormInfo["inlandZoneDict"] = zonesDict[weName]

            self._WindWWUtils.saveAdvisory(self._stormInfo["pil"], self._stormInfo, siteID)

        return

    def getTropWindWWGrids(self, weList):
        """
        Fetch the Proposed and Guidance grids.
        """
        gridDict = {}
        for weName in weList:
            trList = self.GM_getWEInventory(weName)
            if not trList:
                continue
            timeRange = trList[-1]
            gridDict[weName] = self.getGrids(self.mutableID(), weName, "SFC", timeRange)

        return gridDict

    def extractZonesFromGrids(self, gridDict, stormBin):

        etn = self.getETN(self._stormInfo, stormBin)
        zoneDicts = {}
        for weName in gridDict:
            # Filter out all coastal zones for the Proposed, but not Guidance
            if weName == "ProposedTropWWGuidance":
                # inlandMask = self.newGrid(True, np.bool)
                inlandMask = ~self._zoneMap.fetchNHCZonesMask()
                allZonesMask = self.newGrid(True, np.bool)
            elif weName == "ProposedTropWindWW":
                inlandMask = ~self._zoneMap.fetchNHCZonesMask()

            zoneDict = {}
            zoneDictAll = {}
            hazGrid, hazKeys = gridDict[weName]
            for hazKey in hazKeys:
                # Filter out all other storms
                if etn not in hazKey:
                    continue
                hazIndex = self.getIndex(hazKey, hazKeys)
                hazMask = (hazGrid == hazIndex) & inlandMask
                if hazMask.any():
                    hazNoETN = self.stripETN(hazKey)
                    zoneDict[hazNoETN] = self._zoneMap.getOverlappingZoneNames(hazMask)
                if weName == "ProposedTropWWGuidance":
                    hazAllMask = (hazGrid == hazIndex) & allZonesMask
                    if hazAllMask.any():
                        hazNoETN = self.stripETN(hazKey)
                        zoneDictAll[hazNoETN] = self._zoneMap.getOverlappingZoneNames(hazAllMask)

            zoneDicts[weName] = zoneDict
            if weName == "ProposedTropWWGuidance":
                zoneDicts["ProposedTropWWGuidanceAll"] = zoneDictAll

        return zoneDicts

    def execute(self, varDict):

        variableList = []

        siteID = self.getSiteID()
        basinBins = self._basinBins
        # Hazard type hard-wired for Wind Hazards for now.
        title = "Select Bin"
        if siteID in self.NHCSites():
            variableList.append(("Select AT Storms:", [], "check", basinBins["Atlantic"]))
            variableList.append(("Select EP Storms:", [], "check", basinBins["Eastern Pacific"]))
        elif siteID in self.HFOSites():
            variableList.append(("Select CP Storms:", [], "check", basinBins["Central Pacific"]))
        elif siteID in self.GUMSites():
            variableList.append(("Select WP Storms:", [], "check", basinBins["Western Pacific"]))

        # Display the GUI
        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList(
            title, variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            self.cancel()

        stormList = []
        if siteID in self.NHCSites():
            stormList += varDict["Select AT Storms:"]
            stormList += varDict["Select EP Storms:"]
        elif siteID in self.HFOSites():
            stormList += varDict["Select CP Storms:"]
        elif siteID in self.GUMSites():
            stormList += varDict["Select WP Storms:"]
        if len(stormList) != 1:
            self.statusBarMsg("Please select ONE and only ONE storm.", "S")
            return

        selectedBin = stormList[0]
        stormInfoDicts = self._WindWWUtils.fetchStormInfo()
        if selectedBin not in stormInfoDicts:
            self.statusBarMsg(selectedBin + " was not found. Aborting.", "S")

        self._stormInfo = stormInfoDicts[selectedBin]

        # Fetch the grids
        weList = ["ProposedTropWindWW", "ProposedTropWWGuidance"]
        gridDict = self.getTropWindWWGrids(weList)

        # Extract the zones for the selected storm the grids.
        zonesDict = self.extractZonesFromGrids(gridDict, selectedBin)
        # Save the results in the JSON file.
        self.saveZonesToJSONFile(zonesDict)

        self.saveJSONToFile(self._stormInfo)

        # Display the contents of the newly written JSON file
        self._WindWWUtils.displayJSONHazards("AllUSJSONHazards", self._zoneMap)

        # Display a GUI to prompt the forecaster to check the grid generated from the JSON file.
        varDict = {}
        title = "Double Check Your Hazards!"
        label = "Did you check the outgoing JSON hazard grid?"
        variableList = [(label, "No", "radio", ["Yes", "No"])]
        processVarList = ProcessVariableList.ProcessVariableList(title, variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            self.cancel()

        if varDict[label] == "No":
            self.cancel()

        # Storm Surge option will be restored later.
        self.sendWindMessageToWFOs(selectedBin, siteID)
