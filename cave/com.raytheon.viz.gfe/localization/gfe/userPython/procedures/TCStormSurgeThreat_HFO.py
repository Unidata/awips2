# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TCStormSurgeThreat
#
# Original Author: Tom LeFebvre/Pablo Santos
# ------------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# 04/26/2019: DCS 21021   S. White    Added ability for HFO to create a ProposedSS
#                                     grid as part of the SurgeThreat process
# 06/17/2019: DCS 21021   N. Hardin   Cleaning and Refactoring for code review
#  -----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace lower priority version of the file.
##
# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards

MenuItems = ["None"]

import TropicalUtility, LogStream
import SmartScript
import numpy as np
import TimeRange
import AbsTime
import time
import re

VariableList = [('''INSTRUCTIONS: Prior to running this procedure, do the following:
1. Go to WeatherElement Groups and load SurgeThreatMEOWs
2. Run the get_SURGE procedure for MEOWs, not MOMs, under the Populate menu in GFE for the islands where impacts
   are expected, then wait about 5 minutes for the MEOW database to populate in D2D.

TO RUN THIS PROCEDURE:
To create the surge grids once the MEOW data is available in D2D, make an edit area based on the output where surge is expected (i.e. the Hilo coast)
and run with Replace (first time or event reset) or Append (append values to existing grids) if changes are needed.
Choose the inundation value desired for the edit area (i.e. 2 ft) and impact times (in relation to most recent model run time) for each edit area.

If existing grids reflect current thinking, choose Keep Current and grids will be moved to the current time.

For each running, choose the bin/advisory PIL for ProposedSS creation.''', "", "label"),

('''Replace all Inundation grids, append to existing, or keep current?''', "Keep Current", "radio", ["Append", "Replace", "Keep Current"]),
("Choose Bin/Product ID of storm", "XZYGZ", "radio", ["CP1", "CP2", "CP3", "CP4", "CP5", "EP1", "EP2", "EP3", "EP4", "EP5"]),
("", "", "label"),
('''Storm Surge Inundation Impacts/Threat Legend:
>1-3 ft = Elevated
>3-6 ft = Moderate
>6-9 ft = High
> 9 ft = Extreme''', "", "label"),
("InundationMax Height (from MEOW output):", 1.1, "scale", [0.1, 12.1], 1.0),
('''Set the start and end times for which you would like to see the inundation
(i.e. perhaps you would like to see the surge threat occur from hour 12 to 24.''', "", "label"),
("Start Hour for Inundation Timing", 0, "scale", [0.0, 96.0], 6.0),
("End Hour for Inundation Timing", 6, "scale", [0.0, 102.0], 6.0),
]

class Procedure (TropicalUtility.TropicalUtility):
    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)

    ############### USER OVERRIDES #############
    def getThreatWE_Name(self):
        '''
        Weather Element Name of Threat
        '''
        return "StormSurgeThreat"

    def setInundationThreshold(self):
        '''
        Inundation Threshold for Warning
        '''
        return 3.0

    def createKeyMap(self):
        '''
        Defines mapping between UI names and key names
        '''
        return {"Elevated": "Elevated",
                "Moderate" : "Moderate",
                "High": "High",
                "Extreme": "Extreme",
                }

    def createElementList(self):
        '''
        List elements for which Threats/Information will be created
        '''
        return ['StormSurgeThreat','InundationMax']

    def createKeyList(self):
        '''
        List defining order in which grid values are set. Must be lowest to highest
        '''
        return ["Elevated", "Mod", "High", "Extreme"]

    def getWE_NameList(self):
        '''
        Returns list of Weather Element Names to create for Proposed storm surge grids
        '''
        return ["ProposedSS"]

    ############################################
    def chooseValidStorm(self, varDict):
        '''
        Creates status bar message if storm bin number is invalid
        '''
        if varDict["Choose Bin/Product ID of storm"] == "XZYGZ":
            self.statusBarMsg("You must choose a storm bin number", "U")
            return

    def baseGuidanceTime(self):
        '''
        Compute base time for guidance
        '''
        startTime = int((self._gmtime().unixTime() - (2 * 3600)) / (6 * 3600)) * (6 * 3600)
        return startTime

    def getEtnFromTCP(self, bin):
        '''
        Creates ETN from TCP
        '''
        tcp = self.getTextProductFromDB("TCP" + bin)
        senderSearch = None
        if len(tcp) == 0:
            self.statusBarMsg("TCP" + bin + " does not exist in textdb", "A")
            return
        else:
            senderSearch = re.search("(?im)^(?P<sender>(NWS (National |Central Pacific )?Hurricane Center|" +
                                 "National Weather Service).*?)$", tcp)

        if senderSearch is not None:
            sender = senderSearch.group("sender")
            senderParts = sender.split(" ")
            stormNumber = senderParts[-1]
            if len(stormNumber) == 8 and \
                stormNumber[0:2].isalpha() and \
                stormNumber[2:].isdigit():
                self._stormBasin = stormNumber[0:2]
                self._stormID = stormNumber[2:4]
                if self._stormBasin == "EP":
                    baseEtn = "20"
                elif self._stormBasin == "CP":
                    baseEtn = "30"
                else:
                    self.statusBarMsg("This TCP does not have a correct basin", "U")
                    return
                curETN = baseEtn + self._stormID
            else:
                self.statusBarMsg("Could not determine storm number from TCP", "U")
                return
        else:
                self.statusBarMsg("Could not determine storm number from TCP", "U")
                return

        return curETN

    def trimTimeRange(self, weName, timeRange):
        '''
        Trim time range for weather element
        '''
        trList = self.GM_getWEInventory(weName)
        if len(trList) == 0:
            return
        self.splitCmd([weName], timeRange)
        if trList > 1:
            tr = self.GM_makeTimeRange(trList[0].startTime().unixTime(),
                                       timeRange.startTime().unixTime())
            self.deleteCmd([weName], tr)

        tr = trList[-1]
        grid = self.getGrids("Fcst", weName, "SFC", tr)

        self.createGrid("Fcst", weName, "DISCRETE", grid, timeRange)

    def makeInundationMaxGrid(self, timingGrids, trList):
        '''
        Create Inundation Max Grid
        '''
        itCube = np.array(timingGrids)
        maxGrid = np.amax(itCube, axis=0)

        now = int(self._gmtime().unixTime() / 3600) * 3600
        maxTimeRange = self.GM_makeTimeRange(now, now + 48 * 3600)

        self.createGrid(self.mutableID, "InundationMax", "SCALAR", maxGrid, maxTimeRange)

        return maxGrid

        '''
        **************************************************************************************
        This procedure was written to extract datum corrections from the VDATUMS D2D
        Database. It is not yet implemented because the VDATUMS database has not been
        finalized.
        '''
#
#     def deleteAllGrids(self, weList):
#         '''
#         Deletes all grids of weather elements in lise
#         '''
#         for weName in weList:
#             trList = self.GM_getWEInventory(weName)
#             if len(trList) == 0:
#                 continue
#             start = trList[0].startTime().unixTime()
#             end = trList[-1].endTime().unixTime()
#             tr = self.GM_makeTimeRange(start, end)
#
#             self.deleteCmd([weName], tr)
#
#         return
#
#     def getVDATUM(self, weName, limit):
#         '''
#         Fetches VDATUMS for specific siteID for Weather elements
#         '''
#         siteID = self.getSiteID()
#         dbName = siteID + "_D2D_VDATUMS"
#
#         grid = self.getGrids(dbName, weName, "SFC", TimeRange.allTimes(),
#                               mode="First")
#
#         if grid is None:
#             msgStr = weName + " does not exist in the VDATUMS model. "
#             self.statusBarMsg(msgStr, "S")
#
#         mask = (grid <= limit)
#         grid /= 0.3048
#         grid[mask] = -80.0
#
#         return grid
#
#     def getMSLtoNAVD(self):
#         '''
#         Extract MSL to NAVD88 corrections from the VDATUMS D2D Database
#         '''
#         return self.getVDATUM("MSLtoNAVD88", -0.40)
#
#     def getMSLtoMLLW(self):
#         '''
#         Extract MSL to MLLW corrections from the VADTUMS D2D Database.
#         '''
#         return self.getVDATUM("MSLtoMLLW", 0.0)
#
#     def getMSLtoMHHW(self):
#         '''
#         Extract MSL to MHHW corrections from the VDATUMS D2D Database
#         '''
#         return self.getVDATUM("MSLtoMHHW", -3.09)
#
#     def getNAVDtoMLLW(self):
#         '''
#         Extract NAVD88 to MLLW corrections from the VDATUMS D2D Database
#         '''
#         return self.getVDATUM("NAVD88toMLLW", -2.20)
#
#     def getNAVDtoMHHW(self):
#         '''
#         Extract NAVD88 to MLLW corrections from the VDATUM D2D Database
#         '''
#         return self.getVDATUM("NAVD88toMHHW", -3.40)
#
    def makeTimingTRs(self, baseTime, endTime):
        '''
        Makes a list of timeRanges that will be used to make InundationTiming grids
        '''
        trList = []
        start = baseTime
        end = baseTime + 6 * 3600
        while end <= endTime:
            tr = TimeRange.TimeRange(AbsTime.AbsTime(start), AbsTime.AbsTime(end))
            trList.append(tr)
            start = end
            end += 6 * 3600

        return trList

    def getTimingGrids(self):
        '''
        Method to get timing grids
        '''

        baseTime = self.baseGuidanceTime()
        endTime = baseTime + 102 * 3600
        gridList= []
        trList = self.makeTimingTRs(baseTime, endTime)

        for tr in trList:
            timingGrid = self.empty()
            gridList.append(timingGrid)

        return trList, gridList

    def modifyExistingGrids(self, varDict, editArea, ssea):
        '''
        Modifies existing grids (append or replace) for further manipulation
        '''
        inundationHeight = float(varDict["InundationMax Height (from MEOW output):"])
        inunStartHour = float(varDict["Start Hour for Inundation Timing"])
        inunEndHour = float(varDict["End Hour for Inundation Timing"])

        selectedMask = self.encodeEditArea(editArea)
        if not selectedMask.any():
            self.statusBarMsg("Please define an area over which to assign the inundation values.", "S")
            return

        self.modifyMask = selectedMask & ssea
        if not self.modifyMask.any():
            self.statusBarMsg("Please define an area that intersects the StormSurgeEditArea to assign the inundation values.", "S")
            return

        if inunStartHour >= inunEndHour:
            self.statusBarMsg("Please define the end hour after the start hour.", "S")
            return

        surgePctGrid = self.empty()

        if self.makeOption == "Append":
            imTRList = self.GM_getWEInventory("InundationMax", self.mutableID, "SFC")
            if len(imTRList) > 0:
                imTR = imTRList[0]
                surgePctGrid = self.getGrids(self.mutableID, "InundationMax", "SFC", imTR)

        surgePctGrid[self.modifyMask] = inundationHeight

        return inundationHeight, inunStartHour, inunEndHour, surgePctGrid, selectedMask

    def replaceTimingGrids(self, inunStartHour, inunEndHour, surgePctGrid, baseTime):
        '''
        Replaces timing grids
        '''
        trList, timingGrids = self.getTimingGrids()

        for i in range(len(trList)):
            start = trList[i].startTime().unixTime()
            end = trList[i].endTime().unixTime()

            if (start - baseTime) / 3600 >= inunStartHour and (end - baseTime) / 3600 <= inunEndHour:
                timingGrids[i] = surgePctGrid

        timeRange = TimeRange.allTimes()
        self.deleteCmd(["InundationTiming"], timeRange)
        for i in range(len(trList)):
            timingGrids[i].clip(0.0, 100.0, timingGrids[i])
            self.createGrid(self.mutableID, "InundationTiming", "SCALAR", timingGrids[i], trList[i])

    def appendTimingGrids(self, inunStartHour, inunEndHour, baseTime, ssea):
        '''
        Appends timing grids for InundationTiming with new information
        '''
        itTRList = self.GM_getWEInventory("InundationTiming", self.mutableID, "SFC")
        if len(itTRList) == 0:
            self.statusBarMsg("No InundationTiming grids found at all. Run with Replace first.", "S")
            return

        itGrids = []
        trList = []
        for tr in itTRList:
            start = tr.startTime().unixTime()
            end = tr.endTime().unixTime()
            if (start - baseTime) / 3600 >= inunStartHour and (end - baseTime) / 3600 <= inunEndHour:
                grid = self.getGrids(self.mutableID, "InundationTiming", "SFC", tr)
                itGrids.append(grid)
                trList.append(tr)

        if len(itGrids) == 0:
            self.statusBarMsg("No InundationTiming grids found for selected start and end hours. Run with Replace.", "S")
            return

        for i in range(len(trList)):
            itGrids[i][self.modifyMask] = inundationHeight

            self.createGrid(self.mutableID, "InundationTiming", "SCALAR", itGrids[i], trList[i])

        timingGrids = []
        for tr in itTRList:
            grid = self.getGrids(self.mutableID, "InundationTiming", "SFC", tr)
            grid[~ssea] = 0.0
            timingGrids.append(grid)


        surgePctGrid = self.makeInundationMaxGrid(timingGrids, itTRList)

    def keepCurrentGrids(self):
        '''
        Keeps current grids for InundationTiming
        '''
        trList = self.GM_getWEInventory("InundationMax", self.mutableID, "SFC")
        if len(trList) == 0:
            self.statusBarMsg("No InundationMax grids found. Run with Replace.", "S")
            return

        surgePctGrid = self.getGrids("Fcst", "InundationMax", "SFC", trList[-1])

        trList = self.GM_getWEInventory("InundationTiming", self.mutableID, "SFC")
        curTR = self.GM_makeTimeRange(trList[0].startTime().unixTime(), (trList[-1].endTime().unixTime()))

        baseTime = self.baseGuidanceTime()
        timeRange = self.GM_makeTimeRange(baseTime, baseTime + 102 * 3600)

        if curTR != timeRange:
            if trList > 1:
                tr = self.GM_makeTimeRange(trList[0].startTime().unixTime(),
                                           timeRange.startTime().unixTime())
                self.deleteCmd(["InundationTiming"], tr)

            tr = trList[-1]
            grid = self.getGrids("Fcst", "InundationTiming", "SFC", tr)

            trNew = self.GM_makeTimeRange(trList[-1].endTime().unixTime(),
                                       timeRange.endTime().unixTime())

            self.createGrid("Fcst", "InundationTiming", "SCALAR", grid, trNew)

            self.splitCmd(["InundationTiming"], trNew)

        return surgePctGrid

    def setInundationMaxValues(self, surgePctGrid):
        '''
        Next line introduced on Jan 2017 SWiT. It forces points in InundationMax that are > 1 and < 1.5 to 1.5. This is because TCV rounds to
        nearest one foot for categorical HTI threat level consistency with inundation graphic. Not doing this would cause TCV to throw away zones that
        might have more than 3% coverage of inundation > 1 but less than 1.5 altogether. Changing TCV to key on anything with InundationMax >= 1 would not
        do because it would then include zones in TCV with inundation forecasts of less than 1 but >= 0.5 overdoing the threat.
        '''
        surgePctGrid[(surgePctGrid > 1.0) & (surgePctGrid < 1.5)] = 1.5

        return surgePctGrid


    def createThreshDict(self, keyMap):
        '''
        Creates threshold dictionary using keys in keyMap
        '''
        threshDict = {}

        for key in keyMap:

            if keyMap[key] == "Extreme":
                threshDict[keyMap[key]] = 9
            elif keyMap[key] == "High":
                threshDict[keyMap[key]] = 6
            elif keyMap[key] == "Mod":
                threshDict[keyMap[key]] = 3
            elif keyMap[key] == "Elevated":
                threshDict[keyMap[key]] = 1

        return threshDict

    def createNewGrids(self, elementList, keyList, surgePctGrid, timeRange, threatKeys, ssea, threatWEName):
        '''
        Removes old guidance grids and replaces them with new grids defined in elementList
        '''
        cTime = int(self._gmtime().unixTime()/ 3600) * 3600
        startTime = AbsTime.AbsTime(cTime - 48*3600)
        endTime = startTime + 240*3600
        deleteTimeRange = TimeRange.TimeRange(startTime, endTime)

        for elem in elementList:
            self.deleteCmd([elem], deleteTimeRange)

        coastalThreat = self.empty(np.int8)
        surgePctGrid.clip(0.0, 100.0, surgePctGrid)
        self.createGrid(self.mutableID, "InundationMax", "SCALAR", surgePctGrid, timeRange, precision=2)

        keyMap = self.createKeyMap()
        threshDict = self.createThreshDict(keyMap)

        for key in keyList:
            thresh = threshDict[key]
            keyIndex = self.getIndex(key, threatKeys)
            coastalMask = ssea & np.greater(surgePctGrid, thresh)
            coastalThreat[coastalMask] = keyIndex

        self.createGrid(self.mutableID, threatWEName, "DISCRETE",
                        (coastalThreat, threatKeys), timeRange,
                        discreteKeys=threatKeys,
                        discreteOverlap=0,
                        discreteAuxDataLength=2,
                        defaultColorTable="ghls_new")

    def checkForUpgrade(self):
        '''
        Alert forecaster about potential upgrade when keeping current
        '''
        proposedSSTRList = self.GM_getWEInventory("ProposedSS")
        hazSSGrid, hazSSKeys = self.getGrids("Fcst", "ProposedSS", "SFC", proposedSSTRList[-1])
        if hazSSKeys != ["<None>"]:
            self.statusBarMsg("Check ProposedSS for possible needed change to warning from watch (threshold 36 hours). Review InundationTiming grids and manually modify ProposedSS as needed!", "U")

    def logEvent(self, t0):
        '''
        Logs event time
        '''
        t1 = time.time()
        LogStream.logEvent("Finished TCStormSurgeThreat in %f.4 ms" % ((t1-t0) * 1000))

    def createProposedSS_Mask(self, varDict, inunStartHour, surgePctGrid, ssea, selectedMask):
        '''
        Create mask based on Storm Surge edit area
        '''
        bin = varDict["Choose Bin/Product ID of storm"]
        curETN = self.getEtnFromTCP(bin)
        inundationThresh = self.setInundationThreshold()

        if inunStartHour < 36:
            ssAddKey = "SS.W:" + str(curETN)

        elif 36 <= inunStartHour <= 48:
            ssAddKey = "SS.A:" + str(curETN)
        else:
            ssAddKey = "<None>"

        surgeMask = surgePctGrid > inundationThresh
        surgeMask &= ssea
        surgeMask &= selectedMask

        return ssAddKey, surgeMask

    def createEmptyGrid(self, ssAddKey, surgeMask):
        '''
        Creates empty grid which will be populated with ProposedSS
        '''
        ssGrid = self.empty(np.int8)
        ssKeys = ["<None>", ssAddKey]

        ssIndex = self.getIndex(ssAddKey, ssKeys)
        ssGrid[surgeMask] = ssIndex

        return ssGrid

    def createHazardSS(self, proposedSSTRList):
        '''
        Extract the existing SS Hazards from the Hazard grid
        and insert those hazards in the SS grid. So iterate over each Hazard grid
        and add SS values as we go
        '''
        if len(proposedSSTRList) == 0 or self.makeOption == "Replace":
            hazSSGrid = self.empty(np.int8)
            hazSSKeys = ["<None>"]
        else:
            hazSSGrid, hazSSKeys = self.getGrids("Fcst", "ProposedSS", "SFC", proposedSSTRList[-1])

        return hazSSGrid, hazSSKeys

    def checkForConflicts(self, hazTRList, ssGrid, ssKeys, selectedMask):
        '''
        Check Hazard Grid and ProposedSS Grid for conflicts
        '''
        for tr in hazTRList:
            hazGrid = self.getGrids("Fcst", "Hazards", "SFC", tr)
            if self.anyHazardConflictsByPoint(hazGrid, (ssGrid, ssKeys), selectedMask):
                self.statusBarMsg("ETNs do not match Hazards grid in selected area. Please Revert your grids.", "U")
                return

        return hazGrid

    def mergeHazardGrid(self, hazTRList, hazSSGrid, hazSSKeys, hazGrid):
        '''
        Merge any existing SS hazards into the ProposedSS grid
        '''
        for hazTR in hazTRList:
            hazGrid, hazKeys = self.getGrids("Fcst", "Hazards", "SFC", hazTR)
            (hazSSGrid, hazSSKeys) = self.mergeCertainHazards(
                    (hazSSGrid, hazSSKeys), (hazGrid, hazKeys), hazTR,
                    ["SS.W", "SS.A"])

        return (hazSSGrid, hazSSKeys)

    def updateNoHazardsAreas(self, ssAddKey, hazSSKeys, surgeMask, hazSSGrid):
        '''
        Update these hazards where there was no hazard, using the surge grid
        '''
        noneIndex = self.getIndex("<None>", hazSSKeys)
        ssIndex = self.getIndex(ssAddKey, hazSSKeys)
        mask = surgeMask & (hazSSGrid == noneIndex)
        hazSSGrid[mask] = ssIndex

        return hazSSGrid

    def upgradeSS(self, ssAddKey, hazSSKeys, hazSSGrid, surgeMask):
        '''
        Upgrade watch areas to warnings over edit area, if necessary
        '''
        if "SS.W" in ssAddKey:
            etn = self.getETN(ssAddKey)
            watchKey = "SS.A:" + etn

            ssWatchIndex = self.getIndex(watchKey, hazSSKeys)
            ssWarningIndex = self.getIndex(ssAddKey, hazSSKeys)
            mask = (hazSSGrid == ssWatchIndex) & surgeMask
            hazSSGrid[mask] = ssWarningIndex
        return hazSSGrid

    def createFinalizedSS_Grid(self, hazSSGrid, hazSSKeys):
        '''
        Creates new and final storm surge hazard grid(s)
        '''
        weNameList = self.getWE_NameList()
        now = int(self._gmtime().unixTime() / 3600) * 3600
        timeRange = self.GM_makeTimeRange(now, now + 48 * 3600)

        proposedGrid = (hazSSGrid, hazSSKeys)

        for weName in weNameList:
            self.trimTimeRange(weName, timeRange)
            self.createGrid("Fcst", weName, "DISCRETE",
                            (hazSSGrid, hazSSKeys), timeRange)

        return proposedGrid

    def createDiffGrid(self, proposedGrid, timeRange):
        '''
        Creates difference grid so forecaster can visualize differences between ProposedSS and Hazard grid
        '''
        hazTRList = self.GM_getWEInventory("Hazards")
        if len(hazTRList) == 0:
            self.statusBarMsg("No Hazards grids found. No Diff to calculate.", "A")
        else:

            HazardList = []
            anySSHazardsFound = False

            for hazTR in hazTRList:
                ssHazardsFound = False

                hazardsGrid = self.getGrids("Fcst", "Hazards", "SFC", hazTR)

                (hazGrid, hazKeys) = hazardsGrid

                for key in hazKeys:
                    if "SS." in key:
                        ssHazardsFound = True
                        anySSHazardsFound = True
                        break
                HazardList.append((ssHazardsFound, hazardsGrid))

            for index in range(len(HazardList)):
                ssFound, hazardsGrid =  HazardList[index]

                if hazTRList[index].overlaps(timeRange):
                    if not anySSHazardsFound or (anySSHazardsFound and ssFound):
                        self.calcDiffGrid(HazardList[index][1], proposedGrid, "CollabDiffSS",  hazTRList[index], isWFO=True)

    def execute(self, varDict, editArea, timeRange):
        self.chooseValidStorm(varDict)

        t0 = time.time()
        self.mutableID = self.mutableID()
        self.makeOption = varDict['''Replace all Inundation grids, append to existing, or keep current?''']
        ssea = self.encodeEditArea("StormSurgeWW_EditArea_Local")
        threatWEName = self.getThreatWE_Name()

        if self.makeOption == "Replace" or self.makeOption == "Append":
            inundationHeight, inunStartHour, inunEndHour, surgePctGrid, selectedMask = self.modifyExistingGrids(varDict, editArea, ssea)
            baseTime = self.baseGuidanceTime()
            if self.makeOption == "Replace":
                self.replaceTimingGrids(inunStartHour, inunEndHour, surgePctGrid, baseTime)
            elif self.makeOption == "Append":
                self.appendTimingGrids(inunStartHour, inunEndHour, baseTime, ssea, inundationHeight)
        elif self.makeOption == "Keep Current":
            surgePctGrid = self.keepCurrentGrids()

        surgePctGrid = self.setInundationMaxValues(surgePctGrid)

        threatKeys = self.getDiscreteKeys(threatWEName)

        elementList = self.createElementList()
        keyList = self.createKeyList()

        self.createNewGrids(elementList, keyList, surgePctGrid, timeRange, threatKeys, ssea, threatWEName)

        if self.makeOption == "Keep Current":
            self.checkForUpgrade()
        else:
            ssAddKey, surgeMask = self.createProposedSS_Mask(varDict, inunStartHour, surgePctGrid, ssea, selectedMask)
            ssGrid = self.createEmptyGrid(ssAddKey, surgeMask)
            hazTRList = self.GM_getWEInventory("Hazards")
            proposedSSTRList = self.GM_getWEInventory("ProposedSS")
            hazSSGrid, hazSSKeys = self.createHazardSS(proposedSSTRList)
            hazGrid = self.checkForConflicts(hazTRList, hazSSGrid, hazSSKeys, selectedMask)
            (hazSSGrid, hazSSKeys) = self.mergeHazardGrid(hazTRList, hazSSGrid, hazSSKeys, hazGrid)
            hazSSGrid = self.updateNoHazardsAreas(ssAddKey, hazSSKeys, surgeMask, hazSSGrid)
            hazSSGrid = self.upgradeSS(ssAddKey, hazSSKeys, hazSSGrid, surgeMask)
            proposedGrid = self.createFinalizedSS_Grid(hazSSGrid, hazSSKeys)

        self.createDiffGrid(proposedGrid, timeRange)

        self.logEvent(t0)
