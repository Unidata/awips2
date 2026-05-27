# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# SummarizeArchivedHazards.py
#
# This tool performs the following steps;
# 1) Fetches all of the currently archived storm names.
# 2) Presents the list of names, user selects one storm.
# 3) All of the archived JSON files are retreived and statistics calculated for:
#    - the most severe hazard per zone per forecast type for the life of the storm
#    - forecast types: zone(coastal+inland), pure guidance, offical forecast (hazard)
# 4) The max wind for each zone over the life of the storm is calculated using GFE "Wind".
# 5) All values (most severe per zone x forecast type, max wind in zone) saved to CSV file.
# 6) Each line in the CSV file contains the following:
#    ZoneID (e.g., "FLZ001"), Forecast Type (zone, guidance, official), MaxWind value
#
# Author: lefebvre
#
# Sep.  11, 2021       tlefebvr   Initial version with all stated features.
# Oct.  21, 2021       tlefebvr   Added check for PRACTICE mode when making path.
# Dec.  11, 2021       tlefebvr   Added code to handle inland zone field in JSON file.
# Apr   13, 2022       tlefebvr    Made a few changes for Python3 compatibility.
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["None"]

VariableList = []
import TropicalUtility
import Exceptions
import AbsTime, TimeRange
import numpy as np
import SmartScript
import WindWWUtils
import ProcessVariableList
import ZoneMap
import json
import csv
import os, errno


class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        SmartScript.SmartScript.__init__(self, dbss)
        self._WindWWUtils = WindWWUtils.WindWWUtils(dbss)
        self._zoneMap = ZoneMap.ZoneMap(dbss)

    def fetchArchivedStormNames(self):
        filePath = self._WindWWUtils.getDataFilePath()
        filePath = os.path.join(filePath, "archives", "JSON")
        gfeMode = self.gfeOperatingMode()
        if gfeMode == "PRACTICE":
            filePath = os.path.join(filePath, "Practice")
        stormNames = os.listdir(filePath)
        stormNames.sort()
        return stormNames

    def getWEInventory(self, modelName, WEName, level, timeRange=None):
        """
        Get the inventory for the specified weather element.
        """
        allTimes = TimeRange.allTimes()

        if timeRange is None:
            timeRange = allTimes

        trList = []
        # getGridInfo will just die if the modelName or weName is not valid
        # so wrap it in a try block and return [] if it fails
        try:
            gridInfo = self.getGridInfo(modelName, WEName, level, timeRange)
        except Exceptions.EditActionError:
            return trList

        for g in gridInfo:
            if timeRange.overlaps(g.gridTime()):
                trList.append(g.gridTime())

        return trList

    def getAllStormInfos(self, stormName):
        """
        Returns all of the advisories for the specified stormName stored by "bin_advisoryNumber."
        """

        allAdvisories = {}

        practiceMode = self.gfeOperatingMode() == "PRACTICE"
        archiveFilePath = self._WindWWUtils.getArchiveFilePath(practiceMode, stormName)

        advisoryList = os.listdir(archiveFilePath)

        for advisory in advisoryList:
            fileName = os.path.join(archiveFilePath, advisory)
            with open(fileName, 'r') as advFile:
                stormInfo = json.load(advFile)
                allAdvisories[advisory] = stormInfo

        return allAdvisories

    def hazRank(self, hazKey):
        """
        Returns the rank of the specified hazard.
        """
        if hazKey not in self._rankedKeys:
            return 0
        return self._rankedKeys.index(hazKey)

    def displayMaxHazardGrid(self, maxDict, weName):
        """
        Displays a grid based on the contents of maxDict.
        """
        # Initialize masks one for each hazard type
        hazGrid = self.empty(np.int8)
        hazKeys = self._rankedKeys

        for zoneID in maxDict:
            mask = self._zoneMap.maskFromZoneList([zoneID])
            maxHaz = maxDict[zoneID]
            hazIndex = self.getIndex(maxHaz, hazKeys)
            hazGrid[mask] = hazIndex

        self.createGrid(self.mutableID(), weName, "DISCRETE", (hazGrid, hazKeys), self._timeRange,
                        discreteKeys=hazKeys, discreteOverlap=1, discreteAuxDataLength=5,
                        defaultColorTable="GFE/ProposedWind")

        return

    def getMaxWindSpeedGrid(self, pil):
        """
        Fetch the observational wind data and return the max wind value at each grid point.
        """
        # Find the database
        availableDBs = self.availableDatabases()
        match = "_D2D_GTCM" + pil
        for db in availableDBs:
            if match in db.modelIdentifier():
                modelDBID = db
                break
        trList = self.getWEInventory(modelDBID, "uW", "FHAG10")
        if len(trList) == 0:
            self.statusBarMsg("No GTCM data found for: " + pil, "S")
            return None, None

        timeRange = TimeRange.TimeRange(trList[0].startTime(), trList[-1].endTime())
        uGrids = self.getGrids(modelDBID, "uW", "FHAG10", timeRange, mode="List")
        vGrids = self.getGrids(modelDBID, "vW", "FHAG10", timeRange, mode="List")

        magList = []
        for i in range(len(uGrids) - 1):
            # Filter out -200 values
            uGrids[i][uGrids[i] < -100.0] = 0.0
            vGrids[i][vGrids[i] < -100.0] = 0.0
            magList.append(np.sqrt(np.square(uGrids[i]) + np.square(vGrids[i])))

        # Convert m/s to knots
        cube = np.array(magList)
        # Get the max at each grid cell and convert the knots
        maxWind = cube.max(axis=0) * 1.944

        self.createGrid(self.mutableID(), "MaxWind", "SCALAR", maxWind, timeRange,
                            minAllowedValue=0, maxAllowedValue=400, defaultColorTable="GFE/TCMWinds")

        return maxWind

    def getMaxInZone(self, maxGrid, zoneID):
        """
        Calculates the max value of the grid found anywhere inside the specified zone.
        """
        mask = self._zoneMap.maskFromZoneList([zoneID]) & self._wfoMask
        return np.amax(maxGrid * mask)

    def saveCSVFiles(self, stormName, maxDicts, maxWindDict):
        """
        Creates and save the csv files, one for each dictionary in maxDicts.
        """
        # A simple dict so that the filenames are meaningful.
        fileDict = {
            "zoneMax": "ProposedMaxHazard",
            "inlandZoneMax": "InlandZoneMaxHazard",
            "guidanceZoneMax": "GuidanceMaxHazard",
            "hazardsMax": "OfficialMaxHazard"}

        practiceMode = self.gfeOperatingMode() == "PRACTICE"
        basePath = self._WindWWUtils.getDataFilePath()

        dirPath = os.path.join(basePath, "archives", "StormStats", "CSV", stormName, "Practice" if practiceMode else "")

        try:
            os.makedirs(dirPath)
        except OSError as e:
            if e.errno != errno.EEXIST:
                raise

        # Define the directory path and create the directory, if needed.
        try:
            os.makedirs(dirPath)
        except OSError as e:
            if e.errno != errno.EEXIST:
                raise

        for maxDict in maxDicts:
            fileName = os.path.join(dirPath, fileDict[maxDict] + ".csv")
            with open(fileName, 'w', newline='') as csvFile:
                csvWriter = csv.writer(csvFile)
                for zoneID in maxDicts[maxDict]:
                    csvWriter.writerow([zoneID, maxDicts[maxDict][zoneID], str(maxWindDict[zoneID])])
        self.statusBarMsg("Wrote CSV files for " + stormName + " to directory: " + fileName, "A")

        return

    def getPILFromStormName(self, stormName, stormInfoDicts):
        for adv in stormInfoDicts:
            if "pil" in stormInfoDicts[adv]:
                return stormInfoDicts[adv]["pil"]

        self.statusBarMsg("PIL not found in selected StormInfo.", "S")
        return ""

    def getStormNameFromUser(self, stormNameList):
        """
        Gets the storm name from the user.
        """
        # Define the GUI variables.
        varDict = {}
        variableList = []
        label = "Select a storm to process:"
        variableList.append((label, [], "check", stormNameList))

        # Display the GUI
        processVarList = ProcessVariableList.ProcessVariableList(
            "Select Storm:", variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            return ""

        if varDict[label]:
            return varDict[label][0]

        return []

    def getWFOListFromUser(self):
        """
        Gets the wfo list name from the user.
        """
        # Define the GUI variables.
        varDict = {}
        variableList = []
        label = "Select the list of WFOs to process:"
        variableList.append((label, [], "check", sorted(self._windWfos)))

        # Display the GUI
        processVarList = ProcessVariableList.ProcessVariableList(
            "Select Storm:", variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            return ""

        if varDict[label]:
            return varDict[label]

        return []

    def getForecastZoneList(self):
        eaList = self.editAreaList("TCV_Zones")
        return eaList

    def zoneNotFound(self, zoneID, zoneDict, guidanceDict, hazardDict):
        if zoneID in zoneDict or zoneID in guidanceDict or zoneID in hazardDict:
            return False
        return True

    def getWFOMask(self, wfoList):
        """
        Calculate the mask that corresponds to the list of WFOs defined above.
        """
        if not wfoList:
            return np.ones(self.getGridShape(), np.bool)

        mask = self.empty(np.bool)
        for wfo in wfoList:
            mask |= self.encodeEditArea(wfo)
        return mask

    def execute(self, editArea, timeRange, varDict):
        self._rankedKeys = self._WindWWUtils._rankedWindKeys

        start = int(self._gmtime().unixTime() / 3600) * 3600
        end = start + (24 * 3600)
        self._timeRange = TimeRange.TimeRange(AbsTime.AbsTime(start), AbsTime.AbsTime(end))
        # initialize
        dictNames = ["zone", "inlandZone", "guidanceZone", "hazards"]

        wfoList = self.getWFOListFromUser()
        self._wfoMask = self.getWFOMask(wfoList)
        self._validZoneList = self._zoneMap.getOverlappingZoneNames(self._wfoMask)

        allStorms = self.fetchArchivedStormNames()
        selectedStorm = self.getStormNameFromUser(allStorms)
        if selectedStorm == "":
            return

        stormInfoDicts = self.getAllStormInfos(selectedStorm)

        pil = self.getPILFromStormName(selectedStorm, stormInfoDicts)
        if pil == "":
            return

        maxWindGrid = self.getMaxWindSpeedGrid(pil)

        maxDicts = {}
        maxWindDict = {}
        windThreshold = 34.0

        allZones = self.getForecastZoneList()

        # Initialize the max dicts
        for dictName in dictNames:
            maxDicts[dictName + "Max"] = {}

        for adv in stormInfoDicts:
            stormInfo = stormInfoDicts[adv]
            for dictName in dictNames:
                jsonDictName = dictName + "Dict"
                if jsonDictName not in stormInfo:
                    continue
                zoneDict = stormInfo[jsonDictName]
                maxDictName = dictName + "Max"

                for haz in zoneDict:
                    zoneList = zoneDict[haz]
                    for zoneID in zoneList:
                        if zoneID not in self._validZoneList:
                            continue
                        if zoneID not in maxWindDict:
                            maxWindDict[zoneID] = self.getMaxInZone(maxWindGrid, zoneID)
                        # First time for this zone
                        if zoneID not in maxDicts[maxDictName]:
                            maxDicts[maxDictName][zoneID] = haz
                        else:
                            if self.hazRank(maxDicts[maxDictName][zoneID]) < self.hazRank(haz):
                                maxDicts[maxDictName][zoneID] = haz
        # Check for any zones that exceeded the 34 knots threshold and include them to count misses.
        for zoneID in allZones:
            if zoneID not in self._validZoneList:
                continue
            if self.zoneNotFound(zoneID, maxDicts["zoneMax"], maxDicts["guidanceZoneMax"], maxDicts["hazardsMax"]):
                maxWindForZone = self.getMaxInZone(maxWindGrid, zoneID)
                if maxWindForZone >= windThreshold:
                    maxWindDict[zoneID] = self.getMaxInZone(maxWindGrid, zoneID)
                    maxDicts["zoneMax"][zoneID] = "<None>"
                    maxDicts["guidanceZoneMax"][zoneID] = "<None>"
                    maxDicts["hazardsMax"][zoneID] = "<None>"

        # Display the hazards as grids
        for maxDictName in maxDicts:
            weName = selectedStorm + maxDictName.capitalize()
            self.displayMaxHazardGrid(maxDicts[maxDictName], weName)
        # Save the processed data in CSV files.
        self.saveCSVFiles(selectedStorm, maxDicts, maxWindDict)