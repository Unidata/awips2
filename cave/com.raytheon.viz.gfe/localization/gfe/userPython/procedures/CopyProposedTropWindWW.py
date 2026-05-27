## ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CopyProposedTropWindWW
#
# This procedure reads the JSON files and crate a Hazard-like grid for viewing.
# The displayed hazards are clipped to these areas for display.
#
# Author: lefebvre
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# Mar 18, 2020 21020      tlefebvr   Initial version.
# Mar 29, 2020 21020      tlefebvr   Added ETNs to hazard keys.
# Apr  6, 2020 21020      tlefebvr   Fixed ETN issue and added code to read
#                                    JSON file from a the text db.
# Apr  9, 2020 21020      tlefebvr   Added GUI to prompt for bulletins and strip
#                                    AFOS header off of text product.
# Apr 13, 2020 21020      tlefebvr   Added EastPac button and more error messaging
#                                    when failing to get or decode text product.
# Apr 16, 2020 21020      tlefebvr   Fixed filterKeys for calcDiffGrid and call
#                                    calcDiffGrid. Added removal of Prop grids.
# Apr 22, 2020 21020      tlefebvr   Fixed ETNs so it works in all basins.
# May  6, 2020 21020      tlefebvr   Ported to HPA domain and code clean-up.
# May 12, 2020 22033      tlefebvr   Changed TimeRange for Diff grid to 48 hours.
# May 13, 2020 22033      tlefebvr   Limited diff grid to CWA area if it's a WFO.
# May 15, 2020 22033      tlefebvr   Fixed issue with empty JSON (no W/Ws).
# May 21, 2020 22033      tlefebvr   Addressed code review comments.
# May 25, 2020 22033      tlefebvr   Fixed a small bug introduced with previous version.
# May 26, 2020 22033      tlefebvr   Using subprocess to fetch text from textdb.
# Jun  3, 2020 22033      tlefebvr   Fixed SiteID comparision to lists.
# Jun  3, 2020 22033      tlefebvr   Removed explicit call to get text in PRACTICE mode.
# Aug 14, 2020 22033      tlefebvr/santos   Fetching text product differently in practice mode.
# Aug 17, 2020 22033      tlefebvr   Cleaned up Practice mode code.
# Sep  8, 2020 22033      tlefebvr   Fixed Edit area names issue.
# Sep  9, 2020 22033      tlefebvr   Added conflict checking.
# Oct  9, 2020 22033      tlefebvr   Changed Hazard grid to integrated ove time for comparison.
# Oct 14, 2020 22033      tlefebvr   Completely re-implemented calcDiffGrid so it works.
# Oct 15, 2020 22033      tlefebvr   Fixed a couple of bugs related to calcDiffGrid.
# Oct 21, 2020 22033      tlefebvr   Refactored some code and moved to WindWWUtils.
# Oct 25, 2020 22033      tlefebvr   Moved getAllTropicalHazards to WindWWUtils.
# Mar 25, 2021 22033      tlefebvr   Changed flavor of error message when no Hazards grid is found.
# May 10, 2021 22033      tlefebvr   Added guidance zones final display
# Jun 16, 2021 22033      tlefebvr   Fixed Guidance weName.
# Jul 29, 2021 22531      tlefebvr   Final code clean-up before check-in.
# Aug 24, 2021 22531      tlefebvr   Removed exraneous import. Removed getTextProductFromDB in concert
#                                    with change to similar SmartScript method.
# Sep 13, 2021  8657      randerso   Changed to use etnBaseForBasin()
#                                    Additional refactoring and code cleanup
# Dec  9, 2021 22531      tlefebvr   Added changes to support "inlandZoneDict".
# Jan  6, 2022 22531      tlefebvr   Calling WindWWUtils version of getTextProdFromDB.
# Feb  1, 2022 22531      tlefebvr   Python3 fix
# Apr 13, 2022 22531      tlefebvr   Made a few changes for Python3 compatibility.
# Sep 15, 2022 22531      santos/composano/white Fixes post 21.4.1-13 baseline.
# Sep 29, 2023 2036298    santos/composano/white Additional fixes post 21.4.1-13 baseline.
# Jul 12, 2024 2037586    alockleigh Added override for getIndex to treat DiscreteKeys
#                                    with flipped subkeys as distinct.
################################################################################

MenuItems = ["Hazards"]

import AbsTime, TimeRange
import ProcessVariableList
import WindWWUtils
import numpy as np
import json
import operator
from functools import reduce


class Procedure(WindWWUtils.WindWWUtils):

    def __init__(self, dbss):
        WindWWUtils.WindWWUtils.__init__(self, dbss)

    def getZoneList(self, bulletin, hazard, dictKey):
        """
        Fetches the list of zones for the specified bulletin and hazard.
        """
        zoneList = []

        if dictKey in self._stormInfoDict[bulletin]:
            if hazard in self._stormInfoDict[bulletin][dictKey]:
                zoneList = self._stormInfoDict[bulletin][dictKey][hazard]

        return zoneList

    def getStormNumber(self, bulletin):
        """
        Fetches the storm number from the specified bulletin.
        """
        stormNumber = None
        if bulletin in self._stormInfoDict:
            stormNumber = self._stormInfoDict[bulletin]["stormNumber"]

        return stormNumber

    def addETNToHazardKey(self, hazKey, etnStr):
        """
        Adds the ETN string to the specified hazKey
        """
        hazList = hazKey.split("^")
        hazList = [hazKey + ":" + etnStr for hazKey in hazList]
        newHaz = "^".join(hazList)

        return newHaz

    # Updates the GFE spatial display based on the specified list of BP names
    def updateDisplay(self, weName):
        """
        Updates the spatial GFE display based on the current state of the
        stormInfo data.
        """
        hazKeys = ["<None>"]

        grid = self.empty(np.int8)
        # Assign the values to the grid for the CONUS breakpoints`
        for bulletin in self._bulletinList:

            stormNumber = self.getStormNumber(bulletin)
            basin = self._stormInfoDict[bulletin]["stormID"][0:2]
            if stormNumber is None:
                continue

            for hazard in self._rankedHazList:
                if hazard == "<None>":
                    continue
                hazMask = self.empty(np.bool)
                # Fetch the zone list based on the weName
                if "Guidance" in weName:
                    dictKeyList = ["guidanceZoneDict"]
                else:
                    dictKeyList = ["zoneDict", "inlandZoneDict"]
                eaList = []
                for dictKey in dictKeyList:
                    eaList += self.getZoneList(bulletin, hazard, dictKey)
                for eaName in eaList:
                    if eaName not in self._editAreaNames:
                        continue
                    try:
                        ea = self.getEditArea(eaName)
                        hazMask |= self.encodeEditArea(ea) & self._cwaMask
                    except AttributeError:
                        self.statusBarMsg(eaName + " edit area was was not found.", "S")
                        continue
                if not hazMask.any():
                    continue

                etnValue = self.etnBaseForBasin(basin)
                etnStr = str(etnValue + stormNumber)
                if "Guidance" in weName:
                    hazIndex = self.getIndex(hazard, self._rankedHazList)
                else:
                    hazardKey = self.addETNToHazardKey(hazard, etnStr)
                    hazIndex = self.getIndex(hazardKey, hazKeys)
                grid[hazMask] = hazIndex
        # Create the grid showing the breakpoint areas
        if "Guidance" in weName:
            hazKeys = self._rankedHazList
        self.createGrid(self.mutableID(), weName, "DISCRETE", (grid, hazKeys), self._timeRange,
                        defaultColorTable="GFE/windHaz", discreteKeys=hazKeys,
                        discreteOverlap=1, discreteAuxDataLength=5)

        # This is commented out for now as it causes a crash from time to time
        # self.setActiveElement(self.mutableID(), weName, "SFC", self._timeRange)
        return grid, hazKeys

    #  Calculates a difference grid (added versus removed)
    def calcDiffGrid(self, initialGrid, proposedGrid, diffName, timeRange,
                     filterKeys=None, isWFO=False):
        """
        Calculate a temporary grid that shows the difference between the initial discrete
        grid and the the proposedGrid. -1=hazard removed, 0=no change, 1=hazard added,
        2=hazard changed.
        """
        if filterKeys is None:
            filterKeys = ["<None>", "TR.A", "HU.A", "TR.W", "HU.A^TR.W", "TR.W^HU.A", "HU.W", ]

        beforeGrid = self.calcRankGrid(initialGrid, filterKeys)
        afterGrid = self.calcRankGrid(proposedGrid, filterKeys)

        # No change case (0) is assumed unless detected otherwise
        diffGrid = self.empty()
        # Removed Hazard
        diffGrid[(beforeGrid > 0) & (afterGrid == 0) & self._cwaMask] = -1
        # New Hazard
        diffGrid[(beforeGrid == 0) & (afterGrid > 0) & self._cwaMask] = 1
        # Changed Hazard
        diffGrid[((beforeGrid > 0) & (afterGrid > 0)) & (beforeGrid != afterGrid) & self._cwaMask] = 2

        #  Add this temporary grid to the grid manager so it can be seen
        self.createGrid(self.mutableID(), diffName, "SCALAR", diffGrid, timeRange,
                        descriptiveName="Diff Between NHC and WFO", defaultColorTable="GFE/diffSS",
                        precision=0, minAllowedValue=-1.0, maxAllowedValue=2.0)

    def extractJSONText(self, textList):
        """
        Strip the header text from the text product.
        """
        startStr = "{\n"
        if startStr in textList:
            startIndex = textList.index(startStr)
            finalText = textList[startIndex:]
        else:
            self.statusBarMsg("Error parsing JSON text from text product.", "S")
            finalText = ""

        return finalText

    def fetchStormInfoFromTextProduct(self):
        """
        Fetch the text product from the text database,
        convert the text to a dictionary and return in
        a dictionary indexed by bulletin.
        """
        stormInfoDict = {}

        for bulletin in self._bulletinList:
            productID = "MIAJSN" + bulletin
            # Use the OPERATIONAL textdb no matter the gfeOperatingMode.
            text = self.getTextProductFromDB(productID, mode="OPERATIONAL")
            if text:
                # Make a single string out of lists of strings
                text = reduce(operator.concat, text)
            else:
                continue

            text = self.extractJSONText(text)

            jsonDict = json.loads(text)
            if jsonDict is None:
                continue

            stormInfoDict[bulletin] = jsonDict

        return stormInfoDict

    def removeOldPropGrids(self):
        """
        Removes all previous Proposed grids.
        """
        timeRange = TimeRange.allTimes()
        weList = ["ProposedTropWindWW", "ProposedTropWWGuidance", "WindHazardsDiff"]
        for weName in weList:
            self.deleteGrid(self.mutableID(), weName, "SFC", timeRange)

    def noHazardsFound(self, stormInfoDict, bulletinList):
        """
        Returns true if no hazards were found in list of bulletins.
        """
        for bulletin in bulletinList:
            if bulletin in stormInfoDict:
                if "Breakpoints" not in stormInfoDict[bulletin]:
                    return True

                for phenSig in stormInfoDict[bulletin]["Breakpoints"]:
                    if "Breakpoints" in stormInfoDict[bulletin]:
                        if len(stormInfoDict[bulletin]["Breakpoints"]) > 0:
                            return False
        return True

    def fetchEditAreaNames(self):
        """
        Fetches all the edit area names available.
        """
        # Trim the list to Public zones
        allEANames = self.editAreaList()
        editAreaList = [ea for ea in allEANames if len(ea) >= 3 and ea[2] == "Z"]
        return editAreaList

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

    def getIndex(self, uglyStr, keys):
        """
        Override of SmartScript.getIndex so that the index of the correct DiscreteKey is returned.
        """
        if (uglyStr in keys):
            return keys.index(uglyStr)

        if len(keys) >= 256:
            raise IndexError("Attempting to create more than 256 hazard keys")

        keys.append(uglyStr)
        return len(keys) - 1


    # Main method that sets up the GUI and enters the event loop
    def execute(self, varDict):

        self._rankedHazList = ["<None>", "TR.A", "HU.A", "TR.W", "HU.A^TR.W", "TR.W^HU.A", "HU.W", ]

        siteID = self.getSiteID()

        if siteID in self.HFOSites():
            binInfo = [("Central Pacific Storm\nBin Number:", self._basinBins["Central Pacific"])]
        elif siteID in self.GUMSites():
            binInfo = [("Western Pacific Storm\nBin Number:", self._basinBins["Western Pacific"])]
        elif siteID in self.NHCSites():
            binInfo = [("Atlantic Storm\nBin Number:", self._basinBins["Atlantic"]),
                       ("Eastern Pacific Storm\nBin Number:", self._basinBins["Eastern Pacific"]),
                       ]
        elif siteID in self._windWfos:
            binInfo = [("Atlantic Storm\nBin Number:", self._basinBins["Atlantic"]),
                       ]
        elif siteID in ['SGX', 'LOX']:
            binInfo = [("Eastern Pacific Storm\nBin Number:", self._basinBins["Eastern Pacific"]),
                       ]
        else:
            self.statusBarMsg(f"{siteID} is not a known Tropical site", "U")
            return

        variableList = []
        # Build the GUI
        for basinName, binList in binInfo:
            variableList.append((basinName, [], "check", binList))
        # Display the GUI
        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList("Select Bins", variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            self.cancel()
        # Extract the selections
        self._bulletinList = []
        for basinName, binList in binInfo:
            self._bulletinList += varDict[basinName]

        # If PRACTICE mode, change the bulletin name so we fetch the correct text product
        if self.gfeOperatingMode() == "PRACTICE":
            for i, bulletin in enumerate(self._bulletinList):
                self._bulletinList[i] = "WK" + bulletin[-1]

        if not self._bulletinList:
            self.statusBarMsg("Please select at least one bulletin.", "S")
            return

        self.removeOldPropGrids()

        self._editAreaNames = self.fetchEditAreaNames()

        # Get the WFO edit area and mask
        cwaEA = self.getEditArea(siteID)
        self._cwaMask = self.encodeEditArea(cwaEA)

        # Make a timeRange used for displaying the grid, one day long starting now.
        start = int((self._gmtime().unixTime()) / 3600) * 3600
        end = start + 48 * 3600
        self._timeRange = TimeRange.TimeRange(AbsTime.AbsTime(start),
                                              AbsTime.AbsTime(end))

        self._stormInfoDict = self.fetchStormInfoFromTextProduct()

        # Make the Proposed grid, display it and return it
        proposedWEName = "ProposedTropWindWW"
        proposedDBName = self.mutableID()

        propGrid, propKeys = self.updateDisplay(proposedWEName)
        guideGrid, guideKeys = self.updateDisplay("ProposedTropWWGuidance")
        # Check each site for any conflicts and return the list of conflicting sites
        if self.checkForAnyConflicts(self._cwaMask, proposedWEName, proposedDBName):
            self.statusBarMsg("Hazard conflicts between Hazard grid and ProposedTropWindWW from NHC." \
                              " Do not save. Revert the Proposed grids in Fcst db and Check Wind ETN.", "U")
            return

        # Fetch the Hazards grid composite over all times.
        hazGrid, hazKeys = self.getAllTropicalHazards()
        if hazGrid is None:
            self.statusBarMsg("No Hazards grid was found. No difference grid made.", "A")
            return

        self.createGrid(self.mutableID(), "CompositeWindHazards", "DISCRETE", (hazGrid, hazKeys), self._timeRange,
                        defaultColorTable="GFE/windHaz", discreteKeys=hazKeys,
                        discreteOverlap=1, discreteAuxDataLength=10)
        # Make the difference grid
        self.calcDiffGrid((hazGrid, hazKeys), (propGrid, propKeys), "WindHazardsDiff",
                          self._timeRange, self._rankedHazList)
