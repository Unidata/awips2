# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ArchiveHazardGridsToJSON.py
#
# Author: lefebvre
#
# SOFTWARE HISTORY
#
# Date         Ticket#  Engineer    Description
# ------------ -------- ----------- ------------------------------------------
# Sep 27, 2021 22531    tlefebvr   Changed to archive all hazard grids not just the last.
# Jan  7, 2022 22531    tlefebvr   Removed unused parameters in execute statement.
# Jan 10, 2022 22531    tlefebvr   Changed the way the hazard watch and warning were calculated.
#                                  to support multiple storms.
# Apr 13, 2022 22531    tlefebvr    Made a few changes for Python3 compatibility.
# Sep 15, 2022 22531    santos/camposano Fixes post 21.4.1 code review
# Jul 14, 2023 2036298  santos/camposano Fix to enable to run from command line with runProcedure for automation.
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["None"]

VariableList = []

import SmartScript
import WindWWUtils
import ZoneMap
import numpy as np
import TropicalUtility
import string
import ProcessVariableList
import os, copy, errno
import json


class Procedure(TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._WindWWUtils = WindWWUtils.WindWWUtils(dbss)
        # Fetch the zone map. This will create one if it's not there
        self._zoneMap = ZoneMap.ZoneMap(dbss)

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

    def tropicalHazard(self, hazKey):
        """
        Returns True if the specified hazard is of tropical wind type.
        """
        for windKey in self._windKeys:
            if windKey in hazKey:
                return True
        return False

    def makeHazardZoneDict(self, etnMask):
        """
        Returns a dictionary containing all zones where a Tropical Wind Hazard is in effect
        over the specified mask.
        """
        # Initialize the dictionary
        hazardGrid = self._WindWWUtils.getAllTropicalHazards()
        zoneDict = {}
        for haz in self._windKeys:
            zoneDict[haz] = []

        hazGrid, hazKeys = hazardGrid
        for hazGridKey in hazKeys:
            if hazGridKey == "<None>":
                continue
            hazKey = hazGridKey

            if self.tropicalHazard(hazKey):
                hazIndex = self.getIndex(hazKey, hazKeys)
                mask = (hazGrid == hazIndex) & etnMask
                zoneList = self._zoneMap.getOverlappingZoneNames(mask)

                hazNoETN = self.stripETN(hazKey)
                # Treat these two key identically
                if hazNoETN == "HU.A^TR.W":
                    hazNoETN = "TR.W^HU.A"

                zoneDict[hazNoETN] += zoneList

        return zoneDict

    def extractETN(self, stormInfo):
        """
        Calculates the ETN based on the specified stormInfo. We need to store the etn in the JSON file!
        """
        basinIDToBinID = {
            "AL": "AT",
            "EP": "EP",
            "CP": "CP",
            "WP": "PQ",
        }

        stormID = stormInfo["stormID"]
        basinID = stormID[0:2]
        etnDict = self._etnDict
        if basinID not in basinIDToBinID:
            self.statusBarMsg("Invalid basinID found for bin:" + stormInfo['pil'], "S")
            return None

        etnBase = etnDict[basinID]
        etnDetail = stormID[2:4]
        if not etnDetail.isdigit():
            self.statusBarMsg("Invalid storm number found in stormID for bin:" + stormInfo['pil'], "S")
            return None
        # Make the whole etn as an integer.
        etn = etnBase + int(etnDetail)
        return etn

    def getETNMask(self, etn):
        """
        Fetches the mask where any hazard contains the specified etn.
        """
        mask = self.empty(np.bool)
        trList = self.GM_getWEInventory("Hazards")
        if not trList:
            return mask

        for tr in trList:
            hazGrid, hazKeys = self.getGrids(self.mutableID(), "Hazards", "SFC", tr)
            for hazKey in hazKeys:
                if etn in hazKey:
                    hazIndex = self.getIndex(hazKey, hazKeys)
                    mask |= (hazGrid == hazIndex)

        return mask

    def getBinListFromUser(self, binList):
        """
        Gets the list of bins to archive from the user.
        """
        # Define the GUI variables.
        varDict = {}
        variableList = []
        label = "Select bins to archive:"
        guiItems = copy.copy(binList)
        if len(binList) > 1:
            guiItems.append("Archive All Bins")
        variableList.append((label, [], "check", guiItems))

        # Display the GUI
        processVarList = ProcessVariableList.ProcessVariableList(
            "Select Bins to Archive", variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            return []
        # Check for "All". Return all bins, if found
        if "Archive All Bins" in varDict[label]:
            return binList

        return varDict[label]

    def archiveToJSONFile(self, advisoryDict):
        """
        Writes the specified advisory info to a local file for archiving.
        Creates the directory path if needed and with permission.
        """
        siteID = self.getSiteID()
        if siteID in self._basinDomains["Western Pacific"]:
            advisoryDict = self._WindWWUtils.convertHazard(advisoryDict, "TY", "HU")

        stormName = advisoryDict["stormName"]
        advisoryNumber = advisoryDict["advisoryNumber"]
        pil = advisoryDict["pil"]

        practiceMode = self.gfeOperatingMode() == "PRACTICE"

        # /awips2/edex/data/share/RecommendWindWW/archives/JSON/Practice/Test1
        filepath = self._WindWWUtils.getArchiveFilePath(practiceMode, stormName)

        try:
            os.makedirs(filepath)
        except OSError as e:
            if e.errno != errno.EEXIST:
                raise

        filename = os.path.join(filepath, pil + "_" + advisoryNumber)
        with open(filename, 'w') as outfile:
            json.dump(advisoryDict, outfile, indent=4)

    def execute(self, varDict=None):
        self._windKeys = ["TR.A", "HU.A", "TR.W", "TR.W^HU.A", "HU.W", ]
        allStorms = self._WindWWUtils.fetchStormInfo()
        activeBins = sorted(allStorms.keys())

        # A trigger is configured to kick this procedure off
        # so that a varDict will exist already.
        if varDict:
            binList = varDict.get("Storm:", [])
        else:
            # However, it will prompt user for bin selection if procedure is run manually
            binList = self.getBinListFromUser(activeBins)

        # Process each selected bin.
        for bin in binList:
            etn = str(self.extractETN(allStorms[bin]))
            mask = self.getETNMask(etn)
            # Skip areas with no points.
            if not mask.any():
                continue

            zoneDict = self.makeHazardZoneDict(mask)

            stormInfo = allStorms[bin]
            # Add the hazards zones to the stormInfo
            stormInfo["hazardsDict"] = zoneDict
            # Re-save the stormInfo dict in the JSON file.
            self.archiveToJSONFile(stormInfo)
