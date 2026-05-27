# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TCStormSurgeThreat
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer     Description
# ------------ ---------- -----------  --------------------------
# Author: Tom LeFebvre/Pablo Santos
#
# April 20, 2012 - To use gridded MSL TO NAVD and MSL to MLLW
# corrections and to get rid of Very Low.
# Last Modified: June 7, 2012 Shannon White - To fix the handling of time
# for A2 so it works for both real time and displaced real time
# Migrated TC Coastal Flood for AWIPS2. Updated 6/22/2012.  S.O.
# March 11, 2014 to adapt to new PSURGE 2.0/PHISH and VDATUM Datasets in A1. PS
# May 21, 2014: for new PHISH but in AWIPS 2: PS/SW
# Aug 13, 2014: To rename SurgeHtPlustTide to InundationMax and incorporate InundationTiming. PS
# Sept 17, 2014: To finalize changes and clean up for 2015initial Baseline Check in.
# Sept 18, 2014: Added code to pull grids from NHC via ISC if PHISH not
# Available on time. Left inactive (commented out) for the moment until that can be fully tested later
# in 2014 or in 2015.
# LeFebvre/Santos, July 27, 2015: Expanded Manual options to include Replace and Add options.
# This allows sites to specify manually different threat levels across different edit areas and time ranges.
# See 2015HTIUserGuide for details.
# Feb 11, 2016 LeFebvre (16.2.1): Added code to create zero grids and manual grids when
# PSURGE not available. Added checks for current guidance for PHISH and ISC options.
# April 14, 2016: Lefebvre/Santos: Added multabledb to restore ISC option
# 6/20/2016 - Santos: Added code to fix issue of old grid not being deleted when running Manual/Add option.
# 7/15/2016 - Lefebvre/Santos: Added Code to improved Manual Options, numpy compatibility and future builds,
# common methods. Fixed Smoothing Algorithm. inundation grid zeroed out where MHHW <=0.
# 9/8/2016 - Santos: Updated copyISC method to better handle when grids missing in ISC db.
# VERSION 17.1.1 = The one checked in.
# 9/26/16 - LeFebvre - Removed commented out code to pass code review.
# 10/20/16 - Santos - Removed code that stops procedure from running when guidance for current
# advisory is not available and instead advises forecaster.
# 11/3/2016: Santos - Addressed Code Review Comments.
# 12/21/2016: Santos - Added option to adjust InundationMax from manually adjusted InundationTiming grid.
# Also when running with PHISH or PETSS option computes InundationMax from comp max of InundationTiming for consistency. Previously
# they were both retrieved indply from model source and with smoothing it would result in minor differences between
# InundationMax and InundationTiming.
# 01/08/2017: Modified BE CAREFUL line when alerting forecaster PSURGE Data is still from a previous cycle.
# 01/09/2017: Renamed UpdateInunMax in GUI for clarity. Also, introduced on Jan 2017 SWiT ability for procedure to force InundationMax that are >= 1 and < 1.5 to 1.5.
# This is because TCV rounds to nearest one foot for categorical HTI threat level consistency with inundation graphic. Not doing this would cause TCV to throw away zones that
# might have more than 3% coverage of inundation >= 1 but less than 1.5 altogether. Changing TCV to key on anything with InundationMax >= 1 would not
# do because it would then include zones in TCV with inundation forecasts of less than 1 but >= 0.5 overdoing the threat.
# 07/20/2017: Enabled PETSS option for 2018. PS
# 10/11/2017: LeFebvre - GFE: tool failed due to an old grid being present (DR 20309)
# 11/15/2017: Tweaked during SWiT to better handle extended PSurge/PETTS Guidance out to 102 hours,
# improved UpdateInunMax option and made changes to makeInundationTiming methods to accomodate new TCs for
# the TPCSurgeProb and PETSS dbs.
# 03/20/2018 Check in Pablo's fix.
# 4/3/2018 - Additional fixes needed to enable Manual options to work out to 102 hours.
# 9/18/2018 - Make ISC default and have 8-hour InundationMax grid created when running
# 1/28/2020 - Return default to PHISH and change Manual cap to 20 feet
# 7/12/2021 - Santos/Lamb: Added option to move grid forward in time 6 hours.
# Dec 17, 2021  8342     sharbison Changes for Performance Logging.
# 3/1/2022 - Lamb: Changed minimum value from 1.5 to 1.1 since TCV formatter is being
#            enhanced to provide better low-end inundation values (around one foot).
#  ----------------------------------------------------------------------------

# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace lower priority version of the file.

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards

MenuItems = ["Populate"]

import AbsTime
import TimeRange
import TropicalUtility
import numpy as np
import time
from PerformanceStatusHandler import PerformanceStatusHandler

VariableList = [
    (
        "DEFAULT: Typical. Should only be changed in coordination with NHC SS Unit",
        "",
        "label",
    ),
    (
        "Forecast Confidence? - (Applies to PHISH/PETSS Only)",
        "Typical (10% Exceedance; for most systems anytime within 48 hours)",
        "radio",
        [
            "Typical (10% Exceedance; for most systems anytime within 48 hours)",
            "Medium (20% Exceedance; for well-behaved systems within 12 hours of event)",
            "High (30% Exceedance; for well-behaved systems within 6-12 hours of event)",
            "Higher (40% Exceedance; for well-behaved systems within 6 hours of the event)",
            "Highest (50% Exceedance; for well-behaved systems at time of the event)",
        ],
    ),
    ("Grid Smoothing?", "Yes", "radio", ["Yes", "No"]),
    (
        "Make grids from \nPHISH, PETSS, ISC, Manually, or \nKeep Existing Grids (shift forward)?\n",
        "Manually Replace",
        "radio",
        [
            "Manually Replace",
            "Manually Add",
            "UpdateInunMax (Edit Inundation Timing Grids)",
            "ISC",
            "PHISH",
            "PETSS",
            "Keep Existing Grids (shift forward)",
        ],
    ),
    (
        "Manual Inundation settings: Time ranges below relative to advisory model cycle",
        "",
        "label",
    ),
    ("Inundation Height:", 1.0, "scale", [0.0, 30.0], 1.0),
    ("Start Hour for Inundation Timing", 0, "scale", [0.0, 96.0], 6.0),
    ("End Hour for Inundation Timing", 6, "scale", [0.0, 102.0], 6.0),
]

# To get "around one foot" of inundation in TCV, we'll set Inundation grid to this
# value (changed from 1.5 to 1.1 on 3/1/22 to support SITE-level TCV formatter for 2022)
# This change is part of Conf Item 21-23 to improve low-end inundation values in TCV.
aroundOneFoot = 1.1


class Procedure(TropicalUtility.TropicalUtility):
    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)

    def baseGuidanceTime(self):
        """Compute a base time for this guidance"""

        startTime = int((self._gmtime().unixTime() - (2 * 3600)) / (6 * 3600)) * (
            6 * 3600
        )
        return startTime

    def makeNewTimeRange(self, hours):
        """Make a time range of x hours duration from the current time"""

        cTime = int(self._gmtime().unixTime() / 3600) * 3600
        startTime = AbsTime.AbsTime(cTime)
        endTime = startTime + (hours * 3600)
        timeRange = TimeRange.TimeRange(startTime, endTime)

        return timeRange

    def getModelIDList(self, matchStr):
        """Method to find all database versions for the specified model"""

        #  Make a list of all available parameters
        availParms = self.availableParms()

        #  Initialize a list of the database identifiers we want to keep
        modelList = []

        #  Look through every parameter, then check the database id
        for pName, level, dbID in availParms:
            modelId = dbID.modelIdentifier()
            if matchStr in modelId:
                if modelId not in modelList:
                    modelList.append(modelId)

        return modelList

    def getExceedanceHeight(self, modelName, pctStr, level):
        """Method to get the selected exceedance height data"""

        dbName = self.getSiteID() + "_D2D_" + modelName

        modelIDList = self.getModelIDList(modelName)
        modelIDList.sort()

        if not modelIDList:
            return None

        weName = "Surge" + pctStr + "Pct"
        trList = self.GM_getWEInventory(weName, dbName, level)

        if not trList:  # No grids found for this database
            return None

        baseTime = self.baseGuidanceTime()

        if baseTime > trList[0].startTime().unixTime():
            self.statusBarMsg(
                "BE CAREFUL: {} IS STILL FROM A PREVIOUS "
                "ADVISORY/MODEL CYCLE".format(modelName),
                "A",
            )

        #  Make a new time range to span all current data
        timeRange = self.GM_makeTimeRange(
            trList[0].startTime().unixTime(), trList[-1].endTime().unixTime()
        )

        grid = self.getGrids(dbName, weName, level, timeRange, mode="Max")

        #  Convert current surge values from meters to feet
        mask = grid <= -100
        grid /= 0.3048
        grid[mask] = -80.0

        return grid

    def makeInundationTiming(
        self, modelName, pctStr, level, smoothThreatGrid, mutableID, ssea, MHHWMask
    ):
        """Method to create the inundation timing grids"""

        dbName = self.getSiteID() + "_D2D_" + modelName
        weName = "Surge" + pctStr + "Pctincr"

        # get the StormSurgeProb inventory
        surgeTRList = self.GM_getWEInventory(weName, dbName, level)
        if not surgeTRList:
            self.statusBarMsg("No PHISH grid found.", "U")
            return

        # Make timeRanges for all 13 grids. Start with the beginning of the first
        # Phish grid
        # snap to 6 hour period
        baseTime = int(surgeTRList[0].startTime().unixTime() / (6 * 3600)) * (6 * 3600)
        endTime = int(surgeTRList[-1].endTime().unixTime() / (6 * 3600)) * (6 * 3600)
        if endTime < surgeTRList[-1].endTime().unixTime():
            endTime += 6 * 3600
        trList = self.makeTimingTRs(baseTime, endTime)

        timingGrids = []

        self.deleteAllGrids("InundationTiming")
        for tr in trList:

            if tr in surgeTRList:
                phishGrid = self.getGrids(dbName, weName, level, tr)
            else:
                phishGrid = self.empty()

            # For consistency we need to add smoothing here too as we do in execute.
            if phishGrid is None:
                self.statusBarMsg("No PHISH grid available for:" + repr(tr), "S")
                continue

            if smoothThreatGrid == "Yes":
                mask = (phishGrid > 0.0) & ssea
                smoothedPhish = self.GM_smoothGrid(phishGrid, 3, mask)
                phishGrid[mask] = smoothedPhish[mask]

            #  Convert units from meters to feet
            grid = phishGrid / 0.3048
            grid.clip(0.0, 100.0, grid)
            grid[~ssea] = 0.0
            grid[MHHWMask] = 0.0
            # Forces values to be mentioned of around 1.0 in the TCV
            grid[(grid >= 1.0) & (grid < 1.5)] = aroundOneFoot
            timingGrids.append(grid)
            self.createGrid(
                mutableID, "InundationTiming", "SCALAR", grid, tr, precision=1
            )

        return trList, timingGrids

    def makeInundationMaxGrid(self, timingGrids, trList):

        itCube = np.array(timingGrids)
        maxGrid = np.amax(itCube, axis=0)

        now = int(self._gmtime().unixTime() / 3600) * 3600
        maxTimeRange = self.GM_makeTimeRange(now, now + 48 * 3600)

        self.createGrid(
            self.mutableID(), "InundationMax", "SCALAR", maxGrid, maxTimeRange
        )

        return maxGrid

    def getVDATUM(self, weName, limit):
        siteID = self.getSiteID()
        dbName = siteID + "_D2D_VDATUMS"

        grid = self.getGrids(dbName, weName, "SFC", TimeRange.allTimes(), mode="First")

        if grid is None:
            msgStr = weName + " does not exist in the VDATUMS model. "
            self.statusBarMsg(msgStr, "S")

        mask = grid <= limit
        grid /= 0.3048
        grid[mask] = -80.0

        # Converted from meters to feet
        return grid

    def getMSLtoNAVD(self):
        return self.getVDATUM("MSLtoNAVD88", -0.40)

    def getMSLtoMLLW(self):
        return self.getVDATUM("MSLtoMLLW", 0.0)

    def getMSLtoMHHW(self):
        return self.getVDATUM("MSLtoMHHW", -3.09)

    def getNAVDtoMLLW(self):
        return self.getVDATUM("NAVD88toMLLW", -2.20)

    def getNAVDtoMHHW(self):
        return self.getVDATUM("NAVD88toMHHW", -3.40)

    def copyISCGridstoFcst(self, elementList, mutableID):
        """Copies specified weather elements in elementList into the Fcst database."""

        # Initialize all the grids we plan to return
        surgePctGrid = None
        surgePctGridMSL = None
        surgePctGridMLLW = None
        surgePctGridMHHW = None
        surgePctGridNAVD = None

        baseTime = self.baseGuidanceTime()

        # Remove all the grids first before replacing them later
        self.deleteAllGrids(elementList)

        # Amended to distinguish when inundation grids are available but not datum ones.
        for weName in elementList:
            GridsCheck = True
            iscWeName = weName + "nc"

            # get the inventory for the ISC grids
            try:
                trList = self.GM_getWEInventory(iscWeName, "ISC", "SFC", self._allTR)
            except:
                GridsCheck = False

            if not trList:
                GridsCheck = False

            if weName in ["InundationMax", "InundationTiming"] and not GridsCheck:
                self.statusBarMsg(
                    "No inundation grids found in ISC database for {}. Stopping. "
                    "Revert Forecast db.".format(iscWeName),
                    "S",
                )
                return None, None, None, None, None

            if not GridsCheck:
                self.statusBarMsg(
                    "No datum grids in ISC database for {}. Proceeding "
                    "without it.".format(iscWeName),
                    "S",
                )

            # Make sure that the ISC grids are current
            if GridsCheck:
                if baseTime > trList[0].startTime().unixTime():
                    if weName in ["InundationMax", "InundationTiming"]:
                        self.statusBarMsg(
                            "ISC grids for inundation element {} are not current. "
                            "They correspond to a previous cycle. Aborting. Revert "
                            "Forecast db.".format(iscWeName),
                            "S",
                        )
                        return None, None, None, None, None
                    else:
                        self.statusBarMsg(
                            "ISC grids for datum element {} are not "
                            "current. They correspond to a previous "
                            "cycle. Proceeding without "
                            "it.".format(iscWeName),
                            "S",
                        )
                        GridsCheck = False

            for tr in trList:
                grid = self.getGrids("ISC", iscWeName, "SFC", tr)
                if iscWeName in ["InundationMaxnc", "InundationTimingnc"]:
                    grid.clip(0.0, 100.0, grid)
                    grid[(grid >= 1.0) & (grid < 1.5)] = aroundOneFoot
                else:
                    grid.clip(-30.0, 100.0, grid)

                if iscWeName == "InundationTimingnc":
                    self.createGrid(mutableID, weName, "SCALAR", grid, tr, precision=2)
                elif iscWeName == "InundationMaxnc":
                    surgePctGrid = grid
                    self.createGrid(mutableID, weName, "SCALAR", grid, tr, precision=2)
                elif iscWeName == "SurgeHtPlusTideMSLnc" and GridsCheck:
                    surgePctGridMSL = grid
                elif iscWeName == "SurgeHtPlusTideMLLWnc" and GridsCheck:
                    surgePctGridMLLW = grid
                elif iscWeName == "SurgeHtPlusTideMHHWnc" and GridsCheck:
                    surgePctGridMHHW = grid
                elif iscWeName == "SurgeHtPlusTideNAVDnc" and GridsCheck:
                    surgePctGridNAVD = grid

        return (
            surgePctGrid,
            surgePctGridMSL,
            surgePctGridMLLW,
            surgePctGridMHHW,
            surgePctGridNAVD,
        )

    def makeTimingTRs(self, baseTime=None, endTime=None):
        """Make a list of timeRanges that will be used to make InundationTiming grids."""

        if baseTime is None:
            baseTime = self.baseGuidanceTime()

        if endTime is None:
            endTime = baseTime + 102 * 3600

        # Make the inundation timing grids
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

        baseTime = self.baseGuidanceTime()
        endTime = baseTime + 102 * 3600
        gridList = []
        trList = self.makeTimingTRs(baseTime, endTime)

        for tr in trList:
            timingGrid = self.empty()
            gridList.append(timingGrid)

        return trList, gridList

    def execute(self, varDict, editArea, timeRange):
        perfLog = PerformanceStatusHandler("GFE")
        t0 = time.perf_counter()

        mutableID = self.mutableID()

        makeOption = varDict.get(
            "Make grids from \nPHISH, PETSS, ISC, Manually, "
            "or \nKeep Existing Grids (shift forward)?\n"
        )
        # Calculate some times
        self.utcTime = self._gmtime().timetuple()
        self.utcSecs = self._gmtime().unixTime()
        self.utcHr = self.utcTime.tm_hour

        # Calculate 00Z of the current day in epoch seconds
        self.utc00 = (
            self.utcSecs
            - (self.utcTime.tm_hour * 3600)
            - (self.utcTime.tm_min * 60)
            - self.utcTime.tm_sec
        )

        # Desired time range for all but InundationTiming grid
        htiTR = self.createTimeRange(self.utcHr, self.utcHr + 8, "Zulu")

        # Search for active grids over this time range
        self._allTR = self.createTimeRange(self.utcHr - 24, self.utcHr + 240, "Zulu")

        if makeOption == "Keep Existing Grids (shift forward)":

            for elem in ["StormSurgeThreat", "InundationMax", "InundationTiming"]:
                thisInv = self.GM_getWEInventory(elem, mutableID, "SFC", self._allTR)
                if not thisInv:
                    self.statusBarMsg(
                        "You chose to move forward existing grid but {} is "
                        "missing from Fcst db".format(elem),
                        "S",
                    )
                    return

            for surgeGrid in [
                "StormSurgeThreat",
                "InundationMax",
                "InundationTiming",
                "SurgeHtPlusTideMHHW",
                "SurgeHtPlusTideMLLW",
                "SurgeHtPlusTideNAVD",
                "SurgeHtPlusTideMSL",
            ]:

                thisInv = self.GM_getWEInventory(
                    surgeGrid, mutableID, "SFC", self._allTR
                )

                if not thisInv:
                    continue

                # Get times from last grid of this type
                lastST = thisInv[-1].startTime()
                lastET = thisInv[-1].endTime()

                # Get value from last grid of this type
                lastGridVal = self.getGrids(
                    mutableID, surgeGrid, "SFC", thisInv[-1], noDataError=0
                )

                # InundationTiming grids can't just be shifted
                if surgeGrid == "InundationTiming":

                    # Get list of hypothetical InundationTiming grid time ranges
                    timingTRList = self.makeTimingTRs()

                    # If end times don't match, extend the last available
                    # InundationTiming grid to fill gaps at the end
                    if lastET != timingTRList[-1].endTime():

                        # Add max of 4 new InundationTiming grids at end
                        for tr in list(reversed(timingTRList))[0:4]:
                            if tr not in thisInv:
                                self.createGrid(
                                    mutableID,
                                    "InundationTiming",
                                    "SCALAR",
                                    lastGridVal,
                                    tr,
                                )

                    # Delete stale InundationTiming grids
                    for tr in thisInv:
                        if tr not in timingTRList:
                            self.deleteCmd([surgeGrid], tr)

                    continue

                # Processing all other grid elements
                else:

                    # No adjustment needed
                    if lastST == htiTR.startTime() and lastET == htiTR.endTime():
                        self.statusBarMsg(
                            "No time shifting needed for {}".format(surgeGrid),
                            "S",
                        )
                        continue

                    # Delete existing grid
                    self.deleteAllGrids(surgeGrid)

                    if surgeGrid == "StormSurgeThreat":
                        gridType = "DISCRETE"
                    else:
                        gridType = "SCALAR"

                    # Recreate the grid over the desired time range
                    self.createGrid(
                        mutableID,
                        surgeGrid,
                        gridType,
                        lastGridVal,
                        htiTR,
                    )

            return

        self._timeRange = timeRange

        # List of elements
        # See if we should copy from ISC. If so, do the copy and exit
        smoothThreatGrid = varDict["Grid Smoothing?"]

        ssea = self.encodeEditArea("StormSurgeWW_EditArea")

        confidenceStr = varDict["Forecast Confidence? - (Applies to PHISH/PETSS Only)"]

        # extract the percent value from this string
        pctPos = confidenceStr.find("%")
        pctStr = confidenceStr[pctPos - 2 : pctPos]

        threatWEName = "StormSurgeThreat"

        surgePctGrid = None
        surgePctGridMSL = None
        surgePctGridMLLW = None
        surgePctGridMHHW = None
        surgePctGridNAVD = None

        if makeOption in ["PHISH", "PETSS"]:

            # Now get the psurge
            if makeOption == "PHISH":
                modelName = "TPCSurgeProb"
            else:
                modelName = "PETSS"
            surgePctGrid = self.getExceedanceHeight(modelName, pctStr, "FHAG0")
            if surgePctGrid is None:
                message = "No inundation data found for " + modelName
                self.statusBarMsg(message, "S")
                return

            phishMask = ~ssea
            surgePctGrid[phishMask] = 0.0
            surgePctGridNAVD = self.getExceedanceHeight(modelName, pctStr, "SFC")
            if surgePctGridNAVD is None:
                message = "No Surge plus Tide NAVD data found for " + modelName
                self.statusBarMsg(message, "S")
                return

            surgePctGridNAVD[phishMask] = -80.0
            if surgePctGrid is None or surgePctGridNAVD is None:
                return

            # The following lines are the gridded vdatum corrections.
            msltonavd = self.getMSLtoNAVD()
            navdtomllw = self.getNAVDtoMLLW()
            navdtomhhw = self.getNAVDtoMHHW()

            # Apply 3x3 smooth within the surge zone for values greater than 1 as to
            # not underplay areas adjacent to zero value pixels. If you apply a
            # smoother, for consistency among storm surge plus tide and derived
            # grids, it must be done here.
            if smoothThreatGrid == "Yes":
                mask = (surgePctGridNAVD > -10.0) & ssea
                surgePctGridNAVD = self.GM_smoothGrid(surgePctGridNAVD, 3, mask)

            navdMask = surgePctGridNAVD > -80.0
            mask = (msltonavd > -80.0) & navdMask & ssea

            #  MSL Grid
            surgePctGridMSL = surgePctGridNAVD - msltonavd
            surgePctGridMSL[~mask] = -80.0

            #  MLLW Grid
            mask = (navdtomllw > -80.0) & navdMask
            surgePctGridMLLW = surgePctGridNAVD + navdtomllw
            surgePctGridMLLW[~mask] = -80.0

            #  MHHW Grid
            mask = (navdtomhhw > -80.0) & navdMask
            surgePctGridMHHW = surgePctGridNAVD + navdtomhhw
            surgePctGridMHHW[~mask] = -80.0

            #  Diff Grid Between MLLW and MHHW (i.e tidal range)
            mask = (surgePctGridMLLW > -80.0) & (surgePctGridMHHW > -80.0)
            surgeDiffMLLWMHHW = surgePctGridMLLW - surgePctGridMHHW
            surgeDiffMLLWMHHW[~mask] = -80.0

            #  Mask
            MHHWMask = surgePctGridMHHW <= 0.0

            trList, timingGrids = self.makeInundationTiming(
                modelName, pctStr, "FHAG0", smoothThreatGrid, mutableID, ssea, MHHWMask
            )
            # surgePctGrid and InundationMax recomputed from InundationTiming sequence
            # for consistency
            surgePctGrid = self.makeInundationMaxGrid(timingGrids, trList)

        elif makeOption == "ISC":

            elementList = [
                "InundationMax",
                "InundationTiming",
                "SurgeHtPlusTideMSL",
                "SurgeHtPlusTideMLLW",
                "SurgeHtPlusTideNAVD",
                "SurgeHtPlusTideMHHW",
            ]
            (
                surgePctGrid,
                surgePctGridMSL,
                surgePctGridMLLW,
                surgePctGridMHHW,
                surgePctGridNAVD,
            ) = self.copyISCGridstoFcst(elementList, mutableID)

            # if you look in CopyISC method if either InundationMax or
            # InundationTiming is missing the procedure stops all together and
            # notifies forecaster.
            if surgePctGrid is None:
                return

        elif makeOption in ["Manually Replace", "Manually Add"]:

            inundationHeight = float(varDict["Inundation Height:"])
            inunStartHour = float(varDict["Start Hour for Inundation Timing"])
            inunEndHour = float(varDict["End Hour for Inundation Timing"])

            selectedMask = self.encodeEditArea(editArea)
            if not selectedMask.any():
                self.statusBarMsg(
                    "Please define an area over which to assign the "
                    "inundation values.",
                    "S",
                )
                return

            modifyMask = selectedMask & ssea
            if not modifyMask.any():
                self.statusBarMsg(
                    "Please define an area that intersects the StormSurgeEditArea to "
                    "assign the inundation values.",
                    "S",
                )

                return

            if inunStartHour >= inunEndHour:
                self.statusBarMsg(
                    "Please define the end hour after the start hour.", "S"
                )
                return

            surgePctGrid = self.empty()

            # Fetch the old grids if we're adding
            if (
                varDict.get(
                    "Make grids from \nPHISH, PETSS, ISC, Manually, or "
                    "\nKeep Existing Grids (shift forward)?\n"
                )
                == "Manually Add"
            ):
                imTRList = self.GM_getWEInventory("InundationMax", mutableID, "SFC")
                if imTRList:
                    imTR = imTRList[0]
                    surgePctGrid = self.getGrids(
                        mutableID, "InundationMax", "SFC", imTR
                    )

            surgePctGrid[modifyMask] = inundationHeight

            # Make the timing grids
            baseTime = self.baseGuidanceTime()
            # Make new grids and replace all IT grids
            if makeOption == "Manually Replace":
                trList, timingGrids = self.getTimingGrids()

                for i in range(len(trList)):
                    # only modify grids in the specified time range
                    start = trList[i].startTime().unixTime()
                    end = trList[i].endTime().unixTime()

                    if (
                        (start - baseTime) // 3600 >= inunStartHour and
                        (end - baseTime) // 3600 <= inunEndHour
                    ):
                        # populate only where needed
                        timingGrids[i] = surgePctGrid

                timeRange = TimeRange.allTimes()
                self.deleteCmd(["InundationTiming"], timeRange)
                for i in range(len(trList)):
                    timingGrids[i].clip(0.0, 100.0, timingGrids[i])
                    arndOneFt = (timingGrids[i] >= 1.0) & (timingGrids[i] < 1.5)
                    timingGrids[i][arndOneFt] = aroundOneFoot
                    self.createGrid(
                        mutableID,
                        "InundationTiming",
                        "SCALAR",
                        timingGrids[i],
                        trList[i],
                    )

            # Just replace the selected grid points over the selected time
            elif makeOption == "Manually Add":
                # Fetch the existing IT grids
                itTRList = self.GM_getWEInventory("InundationTiming", mutableID, "SFC")
                if not itTRList:
                    self.statusBarMsg("No InundationTiming grids found at all.", "S")
                    return
                # Fetch the grids
                itGrids = []
                trList = []
                for tr in itTRList:
                    start = tr.startTime().unixTime()
                    end = tr.endTime().unixTime()
                    if (
                        (start - baseTime) // 3600 >= inunStartHour and
                        (end - baseTime) // 3600 <= inunEndHour
                    ):
                        grid = self.getGrids(
                            mutableID, "InundationTiming", "SFC", tr)
                        itGrids.append(grid)
                        trList.append(tr)

                if not itGrids:
                    self.statusBarMsg(
                        "No InundationTiming grids found for selected start "
                        "and end hours.",
                        "S",
                    )
                    return

                # Surgically insert grid values into the InundationTiming grids over the
                # selected hours
                for i in range(len(trList)):
                    itGrids[i][modifyMask] = inundationHeight  # poke in the values
                    arndOneFt = (itGrids[i] >= 1.0) & (itGrids[i] < 1.5)
                    itGrids[i][arndOneFt] = aroundOneFoot
                    self.createGrid(
                        mutableID, "InundationTiming", "SCALAR", itGrids[i], trList[i]
                    )

                timingGrids = []
                for tr in itTRList:
                    grid = self.getGrids(
                        self.mutableID(), "InundationTiming", "SFC", tr
                    )
                    grid[~ssea] = 0.0
                    timingGrids.append(grid)

                surgePctGrid = self.makeInundationMaxGrid(timingGrids, itTRList)

        elif makeOption == "UpdateInunMax (Edit Inundation Timing Grids)":

            self.deleteAllGrids(
                [
                    "InundationMax",
                    "SurgeHtPlusTideMSL",
                    "SurgeHtPlusTideMLLW",
                    "SurgeHtPlusTideNAVD",
                    "SurgeHtPlusTideMHHW",
                    "SurgeHtPlusTideMLLW",
                ]
            )

            itTRList = self.GM_getWEInventory("InundationTiming", mutableID, "SFC")

            if not itTRList:
                self.statusBarMsg(
                    "No InundationTiming grids found at all. Inundation grids "
                    "required to exist when running with this option. Otherwise run "
                    "with Manual Replace Option.",
                    "S",
                )
                return

            timingGrids = []

            # Fetch all the timing grids
            for tr in itTRList:
                grid = self.getGrids(self.mutableID(), "InundationTiming", "SFC", tr)
                grid[~ssea] = 0.0
                grid[(grid >= 1.0) & (grid < 1.5)] = aroundOneFoot
                timingGrids.append(grid)
                self.deleteGrid(mutableID, "InundationTiming", "SFC", tr)
                self.createGrid(
                    mutableID, "InundationTiming", "SCALAR", grid, tr, precision=1
                )

            # Finally create the surge grid which will be saved as the InundationMax
            surgePctGrid = self.makeInundationMaxGrid(timingGrids, itTRList)

            # Done with manual options

        # Next line introduced on Jan 2017 SWiT. It forces points in InundationMax
        # that are >= 1 and < 1.5 to 1.1. This is because TCV rounds to nearest one
        # foot for categorical HTI threat level consistency with inundation graphic.
        # Not doing this would cause TCV to throw away zones that might have more than
        # 3% coverage of inundation >= 1 but less than 1.5 altogether. Changing TCV to
        # key on anything with InundationMax >= 1 would not do because it would then
        # include zones in TCV with inundation forecasts of less than 1 but >= 0.5
        # overdoing the threat.

        surgePctGrid[(surgePctGrid >= 1.0) & (surgePctGrid < 1.5)] = aroundOneFoot

        threatKeys = self.getDiscreteKeys(threatWEName)

        # Threshold dictionary
        threshDict = {
            "Elevated": 1,
            "Mod": 3,
            "High": 6,
            "Extreme": 9,
        }

        # make a timeRange - 6 hours long
        elementList = [
            "StormSurgeThreat",
            "InundationMax",
            "SurgeHtPlusTideMSL",
            "SurgeHtPlusTideMLLW",
            "SurgeHtPlusTideNAVD",
            "SurgeHtPlusTideMHHW",
        ]

        # Remove old guidance grids and replace them with the new grids
        # Delete the old grids first
        cTime = int(self._gmtime().unixTime() / 3600) * 3600
        startTime = AbsTime.AbsTime(cTime - 48 * 3600)
        endTime = startTime + 240 * 3600
        deleteTimeRange = TimeRange.TimeRange(startTime, endTime)

        for elem in elementList:
            self.deleteCmd([elem], deleteTimeRange)

        if makeOption not in [
            "Manually Replace",
            "Manually Add",
            "UpdateInunMax (Edit Inundation Timing Grids)",
        ]:

            if surgePctGridMSL is not None:
                surgePctGridMSL.clip(-30.0, 100.0, surgePctGridMSL)
                self.createGrid(
                    mutableID,
                    "SurgeHtPlusTideMSL",
                    "SCALAR",
                    surgePctGridMSL,
                    htiTR,
                    precision=2,
                )

            if surgePctGridMLLW is not None:
                surgePctGridMLLW.clip(-30.0, 100.0, surgePctGridMLLW)
                self.createGrid(
                    mutableID,
                    "SurgeHtPlusTideMLLW",
                    "SCALAR",
                    surgePctGridMLLW,
                    htiTR,
                    precision=2,
                )

            if surgePctGridNAVD is not None:
                surgePctGridNAVD.clip(-30.0, 100.0, surgePctGridNAVD)
                self.createGrid(
                    mutableID,
                    "SurgeHtPlusTideNAVD",
                    "SCALAR",
                    surgePctGridNAVD,
                    htiTR,
                    precision=2,
                )

            if surgePctGridMHHW is not None:
                surgePctGridMHHW.clip(-30.0, 100.0, surgePctGridMHHW)
                self.createGrid(
                    mutableID,
                    "SurgeHtPlusTideMHHW",
                    "SCALAR",
                    surgePctGridMHHW,
                    htiTR,
                    precision=2,
                )

        # Make the grid. Start with the existing grid if we have one otherwise zeros
        coastalThreat = self.empty(np.int8)
        surgePctGrid.clip(0.0, 100.0, surgePctGrid)
        self.createGrid(
            mutableID, "InundationMax", "SCALAR", surgePctGrid, htiTR, precision=2
        )

        # Yet another list to define the order in which we set grid values
        # This order must be ranked lowest to highest
        keyList = ["Elevated", "Mod", "High", "Extreme"]

        # Set the grid values based on the surgePctGrid grid and thresholds
        for key in keyList:
            thresh = threshDict[key]
            keyIndex = self.getIndex(key, threatKeys)
            coastalMask = ssea & np.greater(surgePctGrid, thresh)
            coastalThreat[coastalMask] = keyIndex

        #       create the CoastalThreat Grid
        self.createGrid(
            mutableID,
            threatWEName,
            "DISCRETE",
            (coastalThreat, threatKeys),
            htiTR,
            discreteKeys=threatKeys,
            discreteOverlap=0,
            discreteAuxDataLength=2,
            defaultColorTable="Hazards",
        )

        perfLog.logDuration("Computing TCStormSurgeThreat", time.perf_counter()-t0)

        return
