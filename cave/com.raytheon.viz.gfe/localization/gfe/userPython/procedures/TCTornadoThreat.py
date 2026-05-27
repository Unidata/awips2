# ----------------------------------------------------------------------------
#
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TCTornadoThreat
#
# Author: Tom LeFebvre/Pablo Santos
#
# ----------------------------------------------------------------------------
# SOFTWARE HISTORY
#
# Date         Ticket#     Engineer           Description
# -----------  ----------  -----------        --------------------------
# 02/03/2020   DR21872     psantos            To adapt to new SPC Thresholds and Day 2 Individual
#                                             Hazards and better matching against SPC Probability
#                                             to Categorical Mapping.
# 07/08/2020   DR22123     dharrigan,psantos  Fix bug that was causing tool to miss Day 3 SPC
#                                             guidance when ran after 00Z.
# 02/01/2021   DR22451     psantos            Show a banner to the forecaster letting
#                                             them know to wait until the whole new Day 1 to
#                                             Day 3 cycle data was in before running the tool.
# 06/23/2021   DR22701     mscalora,psantos   Fix to properly determine Day 3. Tool logic broke
#                                             when they added SPC days 4-7 probabilities with
#                                             NIC 11.
#
# 07/12/2021   DCS22519    jlamb              Cleaned up code, added time shift option
# 02/15/2022   DCS22519    jlamb              Made GUI more intuitive based on feedback
# 05/07/2022   DCS22519    jlamb              Fixed problem with move-forward option
# ----------------------------------------------------------------------------
##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Populate"]

import TropicalUtility
import time
import TimeRange
import numpy as np
import LogStream

VariableList = [
    (
        "Run Option",
        "Recalculate grid using latest data",
        "radio",
        ["Recalculate grid using latest data", "Keep existing grid and shift forward"],
    ),
]


class Procedure(TropicalUtility.TropicalUtility):
    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)

    def determineDay(self, modelTime, validTime):
        """
        SPC data for Days 1, 2, and 3 come into the system with different model cycle
        times. Not as part of the same model cycle with forecast hours 24, 48, and 72.
        Worse, they can come at different times. Because of that this is coded as shown
        below to identify Days 1, 2, and 3. SPC never sends the data before 05Z for the
        new Day 1-4 cycle. Because of this, you should never run the tool between 05-08Z
        unless you confirm first in your system the new Days 1 to 3 cycles are in the
        system.
        """

        diff = (validTime - modelTime) // 3600
        if diff < 30:
            return 1
        elif diff >= 30 and diff < 54:
            return 2
        elif diff >= 54 and diff < 78:
            return 3

        return 0

    def getTornadoGrid(self, SPCVarName, dayNum):

        # Narrow the search window
        searchTR = self.createTimeRange(self.curHr - 12, self.curHr + 96)

        for modelName in self.modelList:

            # Look through valid D2D_SPC databases until we find data
            trList = self.GM_getWEInventory(
                SPCVarName, modelName, "SFC", timeRange=searchTR
            )
            if not trList:
                continue

            currentTime = int(time.time())

            for tr in trList:
                gridDayNum = self.determineDay(currentTime, tr.startTime().unixTime())
                if gridDayNum == dayNum:
                    grid = self.getGrids(modelName, SPCVarName, "SFC", tr)
                    LogStream.logVerbose("modelName, tr:", SPCVarName, modelName, tr)
                    return grid, 1

        return None, 0

    def adjustTornadoGrid(self, tornadoThreat, threatKeys, var, dayNum, extThreshold):
        """Adjusts an existing threat grid using additional SPC data."""

        D2DGrid, NumberOfDays = self.getTornadoGrid(var, dayNum)
        if D2DGrid is None:
            return tornadoThreat, NumberOfDays

        # finds all places in the extreme grid >= to the extThreshold
        xMask = D2DGrid >= extThreshold

        # increment the threat where these masks intersect
        lowMask = tornadoThreat > 0
        mask = lowMask & xMask

        # make sure we're not incrementing too far
        extremeIndex = self.getIndex("Extreme", threatKeys)

        # increment the category.  This code assumes that the categories are
        # defined in increasing order of severity.
        tornadoThreat[mask] += 1

        # Clip the adjusted grid to the maximum allowed value - extremeIndex
        tornadoThreat = np.clip(tornadoThreat, 0, extremeIndex)

        return tornadoThreat, NumberOfDays

    def setTornadoGrid(self, tornadoThreat, threatKeys, var, dayNum, threshDict):

        D2DGrid, NumberOfDays = self.getTornadoGrid(var, dayNum)
        if D2DGrid is None:
            return tornadoThreat, NumberOfDays

        # Account for offices using the four key arrangement
        if "Very Low" not in threatKeys and "Very Low" in threshDict:
            threshDict["Very Low"] = "Elevated"

        # Set the grid values based on the tornado prob grid and thresholds
        for key in sorted(threshDict):
            thresh = int(key)
            keyIndex = self.getIndex(threshDict[key], threatKeys)

            # make a temp grid where the thresholds are exceeded
            tempGrid = self.empty(dtype=np.int8)
            tempGrid[D2DGrid >= thresh] = keyIndex

            # calculate areas where this temp grid exceeds the threatGrid
            mask = tempGrid > tornadoThreat

            # update the threatGrid for these areas only
            tornadoThreat[mask] = keyIndex

        return tornadoThreat, NumberOfDays

    def execute(self, varDict):

        # Current hour
        self.curHr = self._gmtime().timetuple().tm_hour

        threatWEName = "TornadoThreat"

        # Setup time ranges
        threatTR = self.createTimeRange(self.curHr, self.curHr + 8, "Zulu")
        allTR = self.createTimeRange(self.curHr - 24, self.curHr + 24, "Zulu")

        if varDict["Run Option"] == "Keep existing grid and shift forward":

            # Search for active grids over this time range
            thisInv = self.GM_getWEInventory(
                threatWEName, self.mutableID(), "SFC", allTR
            )
            if not thisInv:
                self.statusBarMsg(
                    "You chose to move forward existing grid but {} is "
                    "missing from Fcst db".format(threatWEName),
                    "S",
                )
                return

            # Get value from last grid of this type
            lastGridVal = self.getGrids(
                self.mutableID(), threatWEName, "SFC", thisInv[-1], noDataError=0
            )

            # No adjustment needed
            if (
                thisInv[-1].startTime() == threatTR.startTime()
                and thisInv[-1].endTime() == threatTR.endTime()
            ):
                return

            # Delete existing grid
            self.deleteAllGrids(threatWEName)

            # Recreate the grid over the desired time range
            self.createGrid(
                self.mutableID(),
                threatWEName,
                "DISCRETE",
                lastGridVal,
                threatTR,
            )

            return

        threatKeys = self.getDiscreteKeys(threatWEName)

        # Set up the data for processing the various grids.
        # Each entry consists of the D2D variable to be checked,
        # the day number of that grid, and a dictionary that defines
        # each threshold value and the corresponding discrete value.
        # Note the grids will be processed in the order defined in
        # this list.
        actionList = [
            (
                "ptor",
                1,
                {
                    2: "Elevated",
                    10: "Mod",
                    15: "High",
                    30: "Extreme",
                },
                "sigtrndprob",
                10,
            ),
            (
                "ptor",
                2,
                {
                    2: "Elevated",
                    10: "Mod",
                    15: "High",
                    30: "Extreme",
                },
                "sigtrndprob",
                10,
            ),
            (
                "prsvr",
                3,
                {
                    5: "Elevated",
                    15: "Mod",
                },
                "prsigsv",
                10,
            ),
        ]

        # Get list of available SPC D2D databases, listed latest to earliest
        self.modelList = []
        for i in range(0, -20, -1):
            thisDb = self.findDatabase("D2D_SPC", i)
            if not thisDb.isValid():
                break

            self.modelList.append(thisDb)

        # make a grid of zeros.  This will be the TornadoThreat grid
        tornadoThreat = self.empty(dtype=np.int8)

        TotalDays1 = 0
        TotalDays2 = 0
        for var, dayNum, threshDict, xVar, xThreshold in actionList:
            tornadoThreat, NumberOfDays1 = self.setTornadoGrid(
                tornadoThreat, threatKeys, var, dayNum, threshDict
            )

            TotalDays1 += NumberOfDays1

            # now adjust the grid based on the extreme grid category
            tornadoThreat, NumberOfDays2 = self.adjustTornadoGrid(
                tornadoThreat, threatKeys, xVar, dayNum, xThreshold
            )

            TotalDays2 += NumberOfDays2

        LogStream.logVerbose(
            "TotalDays1 and TotalDays2 for var1 and var2 are: ", TotalDays1, TotalDays2
        )

        if TotalDays1 != 3 or TotalDays2 != 3:
            self.statusBarMsg(
                "Did Not Retrieve SPC Probabilities For Days 1, 2, and 3. This likely "
                "means you ran the tool between 05Z and 08Z and the new Day 1-3 data "
                "is still coming in. Check SPC website and re-run after tornado "
                "probabilities for Days 1 and 2 AND severe weather probabilities for "
                "Day 3 have all been posted.",
                "S",
            )
            return

        # remove any old grids that are lying around
        self.deleteAllGrids(threatWEName)

        # create the TornadoThreat Grid
        self.createGrid(
            self.mutableID(),
            threatWEName,
            "DISCRETE",
            (tornadoThreat, threatKeys),
            threatTR,
            discreteKeys=threatKeys,
            discreteOverlap=0,
            discreteAuxDataLength=2,
            defaultColorTable="Hazards",
        )

        return