# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CreateProposedSS - Version 4.0
#
# Author: Lefebvre/Belk/Hardin/Santos/Trogdon
#
# ----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Jun 04, 2014                     incorporate Inundation Grid and Incremental Grid
# Apr 12, 2016                     cleanup code and refactor to use GridManipulation
#                                  and TropicalUtility
# Jul 14, 2016                     Fixed Smoothing, Added Manual Options,
#                                  refactored code a little bit.
# Sep 19, 2016  19293    randerso  Initial baseline check in
#
# Nov 13-18 2016 - Further tweaks made during SWIT testing
# Dec 15, 2016 - Added UpdateInunMax option
# Dec 21, 2016 - Deleted saveElements line in main execute
#
########################################################################

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["None"]

import sys, time, re, os

import AbsTime
import ProcessVariableList
import TimeRange
import TropicalUtility
import numpy as np


class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)


    def trimTimeRange(self, weName, timeRange):

        #  Get a list of all grids we have for this parameter
        trList = self.GM_getWEInventory(weName)
        if len(trList) == 0:  # nothing more to do
            return

        #  Fragment all grids to their smallest time range
        self.splitCmd([weName], timeRange)

        #  If there is more than 1 grid to deal with
        if trList > 1:

            #  Make a time range from the start of the first grid, to the start
            #  of the desired time range
            tr = self.GM_makeTimeRange(trList[0].startTime().unixTime(),
                                       timeRange.startTime().unixTime())

            #  Delete all grids in this interim time range
            self.deleteCmd([weName], tr)

        #  Get the parameter values from the last time range in the list
        tr = trList[-1]
        grid = self.getGrids("Fcst", weName, "SFC", tr)

        #  Make a new grid with these values, using the desired time range
        self.createGrid("Fcst", weName, "DISCRETE", grid, timeRange)


    def getTPCSurgeProbModelTime(self, modelSource, pctStr, level):

        siteID = self.getSiteID()
        if modelSource == "PETSS":
            dbName = siteID + "_D2D_" + modelSource
        else:
            dbName = siteID + "_D2D_TPCSurgeProb" + modelSource

        weName = "Surge" + pctStr + "Pct"
        trList = self.GM_getWEInventory(weName, dbName, level)

        if len(trList) == 0:
            msgStr = dbName + " " + weName + " " + level + " does not exist in the TPCSurge model. "
            self.statusBarMsg(msgStr, "S")
            return None

        modelStart = trList[0].startTime()

        return modelStart

    def getExceedanceHeight(self, modelSource, pctStr, level):

        siteID = self.getSiteID()
        if modelSource == "PETSS":
            dbName = siteID + "_D2D_" + modelSource
        else:
            dbName = siteID + "_D2D_TPCSurgeProb" + modelSource        
        
        weName = "Surge" + pctStr + "Pct"
        trList = self.GM_getWEInventory(weName, dbName, level)

        print "getExceedanceHeight.....TRList for ", dbName, weName, pctStr, level

        # Return None if no grids were found. This should post an error to the user.
        if len(trList) == 0:
            return None

        modelStart = self.getTPCSurgeProbModelTime(modelSource, pctStr, level)

        # Didn't find grid in the Fcst database so fetch it from  model database
        grid = self.getGrids(dbName, weName, level, trList[-1])

        mask = grid <= -10.0  # invalid point - in meters
        grid /= 0.3048  # convert meters to feet
        grid[mask] = -80.0 # reset values less than -10.0 meters to -80.0 feet

        if level == "SFC":
            return grid, modelStart
        else:
            return grid

    #  Create InundationTiming grids from PHISH
    def makeTimingGridsFromModel(self, modelSource, pctStr, level, ssea, MHHWMask):

        siteID = self.getSiteID()
        if modelSource == "PETSS":
            dbName = siteID + "_D2D_" + modelSource
        else:
            dbName = siteID + "_D2D_TPCSurgeProb" + modelSource
        
        weName = "Surge" + pctStr + "Pctincr"
        trList = self.GM_getWEInventory(weName, dbName, level)

        print "makeTimingGridsFromModel.....TRList for ", dbName, weName, pctStr, level

        if len(trList) == 0:
            self.statusBarMsg("No grids available for model:" + dbName, "S")
            return

        # make timeRanges based on the current time not the time of the model
        timingTRList = self.makeTimingTRs(self.baseGuidanceTime())

        gridList = []
        for tr in trList:
            start = tr.startTime().unixTime() - 6 * 3600
            end = tr.startTime().unixTime()
            tr6 = TimeRange.TimeRange(AbsTime.AbsTime(start),
                                      AbsTime.AbsTime(end))

            phishGrid = self.getGrids(dbName, weName, level, tr)
            if phishGrid is None:
                self.statusBarMsg("No PHISH grid available for:" + repr(tr), "S")
                continue

            phishMask = phishGrid > -25.0
            phishGrid[phishMask] /= 0.3048  #  convert meters to feet
            phishGrid[~phishMask] = -80.0

            phishMask = (phishGrid > 0.0) & ssea
            phishGrid[phishMask] = self.GM_smoothGrid(phishGrid, 3, phishMask)[phishMask]

            phishGrid = np.clip(phishGrid, 0.0, 100.0)
            phishGrid[~ssea] = 0.0
            phishGrid[MHHWMask] = 0.0

            gridList.append(phishGrid)


        for i, grid in enumerate(gridList):
            self.createGrid("Fcst", "InundationTiming", "SCALAR", grid, timingTRList[i], precision=1)


    def makeInundationMaxGrid(self, timingGrids, trList):

        itCube = np.array(timingGrids)
        maxGrid = np.amax(itCube, axis=0)

        now = int(self._gmtime().unixTime() / 3600) * 3600
        maxTimeRange = self.GM_makeTimeRange(now, now + 48 * 3600)

        self.createGrid(self.mutableID(), "InundationMax", "SCALAR", maxGrid, maxTimeRange)
        
        return maxGrid

    # Fetch the VDATUM grid and mask all points below the specified value.
    def getVDATUMSGrid(self, weName, threshold):
        siteID = self.getSiteID()
        dbName = siteID + "_D2D_VDATUMS"

        # First check to see if the grid has been stored as a temporary grid
        # and return that. Purely for performance purposes
        fcstTRList = self.GM_getWEInventory(weName)
        if len(fcstTRList) > 0:
            grid = self.getGrids("Fcst", weName, "SFC", fcstTRList[0])
            return grid

        trList = self.GM_getWEInventory(weName, dbName)

        if len(trList) == 0:
            msgStr = weName + " does not exist in the VDATUMS model. "
            self.statusBarMsg(msgStr, "S")
            # TODO: Should we return here or throw an exception since there's no grid to operate on?

        # TODO: why are we looping over all the time ranges retrieving grid
        # we're not using. We should just use the last tr in the list.
        # There's probably only one anyway.
        for tr in trList:
            grid = self.getGrids(dbName, weName, "SFC", tr, mode="First")

        mask = grid <= threshold  # points below threshold

        grid /= 0.3048  # convert meters to feet
        grid[mask] = -80.0  # set points to min value below threshold

        return grid

    # returns the combined key.  Enforces the rule that keys with the
    # same phen returns the one key with the highest priority sig.
    def combinedKey(self, subKeys, newKey):
        if newKey is None:
            return subKeys

        subKeyList = subKeys.split("^")

        # check for same keys
        if newKey in subKeyList:
            return subKeys

        defaultCombo = subKeys + "^" + newKey

        # check for non-VTEC key
        if "." not in newKey:
            return defaultCombo

        # more exceptions - these phens are above the law
        exceptions = ["TO", "SV", "FF"]
        sigList = ["W", "Y", "A"]
        if self.keyPhen(newKey) in exceptions:
            return defaultCombo

        for sk in subKeyList:
            if self.keyPhen(sk) == self.keyPhen(newKey):
                subSig = self.keySig(sk)
                newSig = self.keySig(newKey)
                if subSig == newSig:
                    return subKeys

                if subSig not in sigList or newSig not in sigList:
                    continue

                if sigList.index(subSig) > sigList.index(newSig):
                    subKeys = subKeys.replace(sk, newKey)

                return subKeys

        return defaultCombo

    # makes a new hazard given the oldKey and a new watch phen,
    # sig and etn.
    def makeNewKey(self, oldKey, phenSig):
        # check for the dumb cases
        if oldKey == "<None>" or oldKey == phenSig:
            return phenSig

        # split up the key, add the hazard, sort, and reassemble
        parts = oldKey.split("^")
        parts.append(phenSig)
        parts.sort()  # makes sure the same set of subKeys look the same
        # assemble the new key
        newKey = ""
        for p in parts:
            if newKey == "":
                newKey = p
            else:
                newKey = self.combinedKey(newKey, p)
        # just in case
        if newKey == "":
            newKey = "<None>"

        return newKey


    # Returns a list of unique keys for the specified grid and mask
    def getUniqueKeys(self, byteGrid, keys, mask=None):

        uniqueKeys = []
        for keyIndex, key in enumerate(keys):
            valueMask = byteGrid == keyIndex
            valueMask &= mask
            if valueMask.any():
                uniqueKeys.append(key)

        return uniqueKeys

    # adds the specified hazard to weName over the specified timeRange
    # and spatially over the specified mask.  Combines the specified
    # hazard with the existing hazards by default.  For replaceMode,
    # specify 0 in the combineField
    def combineHazards(self, targetGrid, addHaz, mask, combine=1):

        byteGrid, hazKey = targetGrid
        uniqueKeys = self.getUniqueKeys(byteGrid, hazKey, mask)
        for uKey in uniqueKeys:
            if combine:
                newKey = self.makeNewKey(uKey, addHaz)
            else:  # replace
                newKey = addHaz

            oldIndex = self.getIndex(uKey, hazKey)
            newIndex = self.getIndex(newKey, hazKey)

            # calculate the mask - intersection of mask and oldIndex values
            editMask = (byteGrid == oldIndex) & mask

            # poke in the new values
            byteGrid[editMask] = newIndex

        return (byteGrid, hazKey)

    def addHazard(self, targetGrid, addHaz, mask, combine=1):

        # Only interested in SS subKeys, so extract that part first
        ssKey = ""
        subKeys = self.getSubKeys(addHaz)
        for subKey in subKeys:
            if "SS" in subKey:
                ssKey = subKey

        if ssKey == "":
            print "SS subKey not found in key:", addHaz
            return

        targetByteGrid, targetKeys = self.combineHazards(targetGrid,
            ssKey, mask, combine)

        return targetByteGrid, targetKeys

    def deleteAllGrids(self, weList):

        for weName in weList:
            trList = self.GM_getWEInventory(weName)
            if len(trList) == 0:
                continue
            start = trList[0].startTime().unixTime()
            end = trList[-1].endTime().unixTime()
            tr = self.GM_makeTimeRange(start, end)

            self.deleteCmd([weName], tr)

        return

    def makeDiffGrid(self):
        # Get the ProposedSS grid
        trList = self.GM_getWEInventory("ProposedSS")
        if len(trList) != 1:
            self.statusBarMsg("No ProposedSS grids found.", "S")
            return
        else:
            proposedTR = trList[0]

        # Get the Raw guidance grid
        trList = self.GM_getWEInventory("tempProposedSS")
        if len(trList) != 1:
            self.statusBarMsg("No tempProposedSS grids found.", "S")
            return
        else:
            rawGuidTR = trList[0]

        proposedSSGrid = self.getGrids("Fcst", "ProposedSS", "SFC", proposedTR)
        rawGuidSSGrid = self.getGrids("Fcst", "tempProposedSS", "SFC", rawGuidTR)

        self.calcDiffGrid(proposedSSGrid, rawGuidSSGrid, "PrevGuidDiffSS",
                          proposedTR)

        return

    def baseGuidanceTime(self):
        startTime = int((self._gmtime().unixTime() - (2 * 3600)) / (6 * 3600)) * (6 * 3600)
        return startTime

    # Make a list of timeRanges that will be used to make InundationTiming grids
    def makeTimingTRs(self, baseTime):
        # Make the inundation timing grids
        trList = []
        for t in range(0, 78, 6):
            start = baseTime + t * 3600
            end = baseTime + (t + 6) * 3600
            tr = TimeRange.TimeRange(AbsTime.AbsTime(start), AbsTime.AbsTime(end))
            trList.append(tr)

        return trList

    def getTimingGrids(self):

        baseTime = self.baseGuidanceTime()
        trList = self.makeTimingTRs(baseTime)

        gridList = []
        for tr in trList:
            gridList.append(self.empty(np.float32))

        return trList, gridList

    def execute(self, editArea):

        editAreaMask = self.encodeEditArea(editArea)

        #  If we did not find an edit are mask
        if editArea is None or (not editAreaMask.any()):
            #  Select the entire domain
            editAreaMask = self.newGrid(True, np.bool)

        # Extract the info from all storms
        stormList = self.extractStormInfo()

        stormNames = []

        mutableID = self.mutableID()

        # make a list of the active storms to pass into the variableList
        for sDict in stormList:
            stormNames.append(sDict["stormName"])

        variableList = []
        bogusStormName = "WXYZZYXW"
        variableList.append(("Data Source", "N-SBN (Default)", "radio",
                             ["N-SBN (Default)", "Backup", "PETSS", "Manual Replace", "Manual Add", "UpdateInunMax"
                              ]))
        variableList.append(("Indicate Your Situational Forecast Confidence", "Typical (Combined; 10% Exceedance)",
                 "radio", ["Typical (Combined; 10% Exceedance)",
                           "Medium (Combined; 20% Exceedance)",
                           "High (Combined; 30% Exceedance)",
                           "Higher (Combined; 40% Exceedance)",
                           "Highest (Combined; 50% Exceedance)"]))
        variableList.append(("StormName", bogusStormName, "radio", stormNames))
        variableList.append(("Hazard", "Storm Surge Watch", "radio",
                             ["Storm Surge Watch", "Storm Surge Warning"]))
        variableList.append(("Raw Guidance or Proposed SS Grid?", "ProposedSS", "radio",
                             ["Raw Guidance", "ProposedSS"]))
        variableList.append(("Inundation Threshold" , 3, "scale", [1, 8], 1))
        variableList.append(("Manual Inundation settings:", "", "label"))
        variableList.append(("Inundation Height" , 3, "scale", [1, 8], 1))
        variableList.append(("Start Hour for Inundation Timing", 0, "scale", [0.0, 72.0], 6.0))
        variableList.append(("End Hour for Inundation Timing", 6, "scale", [0.0, 78.0], 6.0))

#        variableList.append(("Make Inunudation Timing Grids?", "Yes", "radio", ["Yes", "No"]))

        # Display the GUI and check for cancel
        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList("StormSurgeWW", variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            self.cancel()

        sourceOption = varDict["Data Source"]

        if sourceOption == "N-SBN (Default)":
            self._dataSource = "LoRes"
        elif sourceOption == "Backup":
            self._dataSource = "Manual"
        elif sourceOption == "PETSS":
            self._dataSource = "PETSS"
        elif sourceOption == "Manual Replace":
            self._dataSource = "LoRes"
        elif sourceOption == "Manual Add":
            self._dataSource = "LoRes"

        # Fetch the StormSurge edit area
        ssEditArea = self.getEditArea("StormSurgeWW_EditArea")
        ssea = self.encodeEditArea(ssEditArea)

        # Below you can configure different edit areas to specify different tide corrections
        inundationThresh = varDict["Inundation Threshold"]

        tempAddReplace = varDict["Raw Guidance or Proposed SS Grid?"]

        # Make sure a storm was selected
        stormName = varDict["StormName"]
        if stormName == bogusStormName:
            self.statusBarMsg("Please select a storm name.", "U")
            return

        # Extract storm number for selected storm
        for sDict in stormList:
            if sDict["stormName"] == stormName:
                stormNum = int(sDict["stormNumber"])
                lastModified = sDict["lastModified"]

        # Make sure that the storm info has been updated within the last 7 hours
        if self._gmtime().unixTime() - lastModified > 7 * 3600:
            self.statusBarMsg("StormInfo for " + stormName + " is old. " + \
                              "Please update StormInfo first.", "U")
            return

       #  Ensure this is a national VTEC number
        if stormNum < 1000:
            stormNum = int(stormNum + 1000)

        confidenceStr = varDict["Indicate Your Situational Forecast Confidence"]

        # extract the percent value from this string
        pctPos = confidenceStr.find("%")
        pctStr = confidenceStr[pctPos - 2:pctPos]

        now = int(self._gmtime().unixTime() / 3600) * 3600
        timeRange = self.GM_makeTimeRange(now, now + 48 * 3600)

        if sourceOption in ["N-SBN (Default)", "Backup", "PETSS"]:

            if stormNum is None:
                self.abort("You must supply the storm!")
                return

            # Now get the P-surge
            surgePctGrid = self.getExceedanceHeight(self._dataSource, pctStr, "FHAG0")

            # Stop the tool if we didn't get the grid we wanted
            if surgePctGrid is None:
                self.statusBarMsg("No StormSurge guidance found for source " + self._dataSource + ".", "S")
                return

            surgePctGrid = np.clip(surgePctGrid, 0.0, 100.0)
            surgePctGrid[~ssea] = 0.0

            # Get NAVD grids
            surgePctGridNAVD, modelStart = self.getExceedanceHeight(self._dataSource, pctStr, "SFC")
            surgePctGridNAVD = np.clip(surgePctGridNAVD, -80.0, 100.0)
            surgePctGridNAVD[~ssea] = -80.0

            # smooth grids
            surgePctGrid = self.GM_smoothGrid(surgePctGrid, 3, (surgePctGrid > 0.0) & ssea)
            surgePctGridNAVD = self.GM_smoothGrid(surgePctGridNAVD, 3, (surgePctGridNAVD > -10.0) & ssea)

            # Calculate and display surge guidance grids
            msltonavd = self.getVDATUMSGrid("MSLtoNAVD88", -0.40)
            navdtomllw = self.getVDATUMSGrid("NAVD88toMLLW", -2.20)
            navdtomhhw = self. getVDATUMSGrid("NAVD88toMHHW", -3.40)

            validSurgeMask = surgePctGridNAVD > -80.0
            wTopoMask = validSurgeMask & (msltonavd > -80.0)

            surgePctGridMSL = self.newGrid(-80.0)
            surgePctGridMSL[wTopoMask] = (surgePctGridNAVD - msltonavd)[wTopoMask]
            mllwMask = validSurgeMask & (navdtomllw > -80.0)

            surgePctGridMLLW = self.newGrid(-80.0)
            surgePctGridMLLW[mllwMask] = (surgePctGridNAVD + navdtomllw)[mllwMask]
            mhhwMask = validSurgeMask & (navdtomhhw > -80.0)

            surgePctGridMHHW = self.newGrid(-80.0)
            surgePctGridMHHW[mhhwMask] = (surgePctGridNAVD + navdtomhhw)[mhhwMask]

            surgePctGridMSL = np.clip(surgePctGridMSL, -30.0, 100.0)
            surgePctGridMLLW = np.clip(surgePctGridMLLW, -30.0, 100.0)
            surgePctGridNAVD = np.clip(surgePctGridNAVD, -30.0, 100.0)
            surgePctGridMHHW = np.clip(surgePctGridMHHW, -30.0, 100.0)

            # Clip the MHHW grid at 0.0
            MHHWMask = surgePctGridMHHW <= 0.0
            surgePctGrid[MHHWMask] = 0.0

            weList = ["InundationMax", "InundationTiming", "SurgeHtPlusTideMSL", "SurgeHtPlusTideNAVD", "SurgeHtPlusTideMHHW", "SurgeHtPlusTideMLLW"]

            self.deleteAllGrids(weList)

            now = int(self._gmtime().unixTime() / 3600) * 3600
            guidanceTR = self.GM_makeTimeRange(now, now + 48 * 3600)

            self.createGrid("Fcst", "SurgeHtPlusTideMSL", "SCALAR",
                            surgePctGridMSL, guidanceTR, precision=2)

            self.createGrid("Fcst", "SurgeHtPlusTideMLLW", "SCALAR",
                            surgePctGridMLLW, guidanceTR, precision=2)

            self.createGrid("Fcst", "SurgeHtPlusTideNAVD", "SCALAR",
                            surgePctGridNAVD, guidanceTR, precision=2)

            self.createGrid("Fcst", "SurgeHtPlusTideMHHW", "SCALAR",
                            surgePctGridMHHW, guidanceTR, precision=2)

            self.createGrid("Fcst", "SurgeHtPlusTideMLLW", "SCALAR",
                            surgePctGridMLLW, guidanceTR, precision=2)

        elif sourceOption in ["Manual Replace", "Manual Add"]:
            # Figure out the total number of points
            gridSize = self.getGridShape()
            totalPoints = gridSize[0] * gridSize[1]
            # Make sure the user selected a real edit area before continuing
            selectedMask = self.encodeEditArea(editArea) # make the mask based on the selected edit area
            if editArea is None or (not selectedMask.any()) or np.count_nonzero(selectedMask) == totalPoints:
                self.statusBarMsg("Please select an edit area before running the Manual Replace or Manual Add option." , "S")
                return

#            modelStart = self.getTPCSurgeProbModelTime("LoRes", pctStr, "SFC")

            inundationHeight = float(varDict["Inundation Height"])
            inunStartHour = float(varDict["Start Hour for Inundation Timing"])
            inunEndHour = float(varDict["End Hour for Inundation Timing"])

            modifyMask = selectedMask & ssea

            if inunStartHour >= inunEndHour:
                self.statusBarMsg("Please define the end hour after the start hour.", "S")
                return

            surgePctGrid = self.empty(np.float32)

            # Fetch the old grids if we're adding
            if sourceOption == "Manual Add":
                imTRList = self.GM_getWEInventory("InundationMax", self.mutableID(), "SFC")
                if len(imTRList) > 0:
                    imTR = imTRList[0]
                    surgePctGrid = self.getGrids(mutableID, "InundationMax", "SFC", imTR)

            surgePctGrid = surgePctGrid * 0   # reset the surgePctGrid
            surgePctGrid[modifyMask] = inundationHeight   # poke in the new values

            # Make the timing grids
            baseTime = self.baseGuidanceTime()

            self.deleteAllGrids(["InundationMax","SurgeHtPlusTideMSL", "SurgeHtPlusTideMLLW",
                                 "SurgeHtPlusTideNAVD", "SurgeHtPlusTideMHHW", "SurgeHtPlusTideMLLW"])
            
            if sourceOption == "Manual Replace":   # Make new grids and replace all IT grids
                self.deleteAllGrids(["InundationTiming"])

                trList, timingGrids = self.getTimingGrids()  # fetch empty grids with times

                for i, tr in enumerate(trList):
                    start = tr.startTime().unixTime()
                    end = tr.endTime().unixTime()

                    if (start - baseTime) / 3600 >= inunStartHour and (end - baseTime) / 3600 <= inunEndHour:
                        timingGrids[i][selectedMask] = inundationHeight  # populate only where needed
                        timingGrids[i][~ssea] = 0.0

                for i, tr in enumerate(trList):
                    timingGrids[i] = np.clip(timingGrids[i], 0.0, 100.0)
                    self.createGrid(mutableID, "InundationTiming", "SCALAR", timingGrids[i], tr)

                # Finally create the surge grid which will be saved as the InundationMax
                itCube = np.array(timingGrids)
                surgePctGrid = np.amax(itCube, axis = 0)

                self.makeInundationMaxGrid(timingGrids, trList)

            elif sourceOption == "Manual Add": # Just replace the selected grid points over the selected time
                # Fetch the existing IT grids
                itTRList = self.GM_getWEInventory("InundationTiming", self.mutableID(), "SFC")
                if len(itTRList) == 0:
                    self.statusBarMsg("No InundationTiming grids found at all. Inundation grids required to exist when running with this option. Otherwise run with Manual Replace Option.", "S")
                    return

                timingGrids = []
                trList = []

                # Fetch all the timing grids
                for tr in itTRList:
                    grid = self.getGrids(mutableID, "InundationTiming", "SFC", tr)
                    timingGrids.append(grid)
                    trList.append(tr)

                # Now poke in the selected value in each grid we need to modify
                for i in range(len(timingGrids)):
                    start = trList[i].startTime().unixTime()
                    end = trList[i].endTime().unixTime()
                    if (start - baseTime) / 3600 >= inunStartHour and (end - baseTime) / 3600 <= inunEndHour:
                        timingGrids[i][modifyMask] = inundationHeight # poke in the values and create the grids

                # Delete the grids before re-creating them
                self.deleteAllGrids(["InundationTiming"])

                # Create the InundationTiming grids
                for i in range(len(timingGrids)):
                    self.createGrid(mutableID, "InundationTiming", "SCALAR", timingGrids[i], trList[i])

                # Finally create the surge grid which will be saved as the InundationMax
                itCube = np.array(timingGrids)
                surgePctGrid = np.amax(itCube, axis = 0)

                self.makeInundationMaxGrid(timingGrids, itTRList)

        else: # Then this is UpdateInunMax
                
            self.deleteAllGrids(["InundationMax","SurgeHtPlusTideMSL", "SurgeHtPlusTideMLLW",
                                 "SurgeHtPlusTideNAVD", "SurgeHtPlusTideMHHW", "SurgeHtPlusTideMLLW"])

            itTRList = self.GM_getWEInventory("InundationTiming", self.mutableID(), "SFC")
            
            if len(itTRList) == 0:
                self.statusBarMsg("No InundationTiming grids found at all. Inundation grids required to exist when running with this option. Otherwise run with Manual Replace Option.", "S")
                return
                
            timingGrids = []
                
            # Fetch all the timing grids
            for tr in itTRList:
                grid = self.getGrids(self.mutableID(), "InundationTiming", "SFC", tr)
                timingGrids.append(grid)
                               
        # Finally create the surge grid which will be saved as the InundationMax
               
            try:
                surgePctGrid = self.empty(np.float32)
            except AttributeError:
                surgePctGrid = np.zeros(self.getGridShape(), np.float32)
            
            surgePctGrid = self.makeInundationMaxGrid(timingGrids, itTRList)

            #return

                # Done with manual options

        # Get the hazard key based on the GUI
        hazardType = varDict["Hazard"]

        # Make the key with the storm number
        if hazardType == "Storm Surge Watch":
            ssAddKey = "SS.A:" + str(stormNum)
            # ssAddKey = "SS.W:" + str(stormNum)
        elif hazardType == "Storm Surge Warning":
            ssAddKey = "SS.W:" + str(stormNum)
            # ssAddKey = "SS.A:" + str(stormNum)
        else:
            ssAddKey = "<None>"

        # Calculate the new pSurge mask restricting to the storm surge
        # edit area and the selected edit area
        pSurgeMask = surgePctGrid > inundationThresh
        pSurgeMask = pSurgeMask & ssea
        pSurgeMask = pSurgeMask & editAreaMask

        #############################  Now make the Hazard grid and check for conflicts  ########################

        # Make an empty grid which will be populated
        ssGrid = self.empty(np.int8)
        ssKeys = ["<None>", ssAddKey]

        ssIndex = self.getIndex(ssAddKey, ssKeys)
        ssGrid[pSurgeMask] = ssIndex

        # If we're making the temp grid, just make it now with no Hazard merging and return
        hazTRList = self.GM_getWEInventory("Hazards")
        if tempAddReplace == "Raw Guidance":
            for tr in hazTRList:
                hazGrid = self.getGrids("Fcst", "Hazards", "SFC", tr)

                if self.anyHazardConflictsByPoint(hazGrid, (ssGrid, ssKeys), editAreaMask):
                    self.statusBarMsg("ETNs do not match Hazards grid in selected area for Raw Guidance.", "U")
                    return

            self.createGrid("Fcst", "tempProposedSS" , "DISCRETE", (ssGrid, ssKeys), timeRange)
            self.makeDiffGrid()
            return

        proposedSSTRList = self.GM_getWEInventory("ProposedSS")

        # Next we need to extract the existing SS Hazards from the Hazard grid
        # and insert those hazards in the SS grid so we never lose them.
        # So iterate over each Hazard grid and add SS values as we go
        if len(proposedSSTRList) == 0:
            hazSSGrid = self.empty(np.int8)
            hazSSKeys = ["<None>"]

        #  A ProposedSS grid already exists - start with the last one
        else:
            hazSSGrid, hazSSKeys = self.getGrids("Fcst", "ProposedSS", "SFC", proposedSSTRList[-1])

        # Get the grids to check for conflicts
        for tr in hazTRList:
            hazGrid = self.getGrids("Fcst", "Hazards", "SFC", tr)
            if self.anyHazardConflictsByPoint(hazGrid, (ssGrid, ssKeys), editAreaMask):
                self.statusBarMsg("ETNs do not match Hazards grid in selected area. Please Revert your grids.", "U")
                return

        print "No conflicts found...."

        #  Merge any existing SS hazards into the ProposedSS grid
        if len(hazTRList) > 0:
            for hazTR in hazTRList:
                hazGrid, hazKeys = self.getGrids("Fcst", "Hazards", "SFC", hazTR)

                #  Merge the selected hazards, in this case SS.A and SS.W,
                #  into the existing ProposedSS grid
                (hazSSGrid, hazSSKeys) = self.mergeCertainHazards(
                        (hazSSGrid, hazSSKeys), (hazGrid, hazKeys), hazTR,
                        ["SS.W", "SS.A"])

        # Update these hazards where there was no hazard, using the pSurge grid
        noneIndex = self.getIndex("<None>", hazSSKeys)
        ssIndex = self.getIndex(ssAddKey, hazSSKeys)
        mask = pSurgeMask & (hazSSGrid == noneIndex)
        hazSSGrid[mask] = ssIndex

        # Finally upgrade Watch areas to Warnings over the edit area, if necessary
        if "SS.W" in ssAddKey:

            # Find Watch points over the edit area
            print "Upgrading watches to warnings."
            # Find the key containing "SS.A" that matches the ETN
            etn = self.getETN(ssAddKey)
            watchKey = "SS.A:" + etn

            ssWatchIndex = self.getIndex(watchKey, hazSSKeys)
            ssWarningIndex = self.getIndex(ssAddKey, hazSSKeys)
            mask = (hazSSGrid == ssWatchIndex) & pSurgeMask
            hazSSGrid[mask] = ssWarningIndex

        #  Now create the new storm surge hazard grid(s)
        #  Create a new time range
#         start = modelStart.unixTime() - 80 * 3600
#         end = start + 78 * 3600
#         timeRange = TimeRange.TimeRange(AbsTime.AbsTime(start),
#                                         AbsTime.AbsTime(end))

#***********************************************************************************

        weNameList = ["ProposedSS", "InitialSS"]
        now = int(self._gmtime().unixTime() / 3600) * 3600
        timeRange = self.GM_makeTimeRange(now, now + 48 * 3600)

        # Make an InitialSS grid if it's the first time
        for weName in weNameList:
            self.trimTimeRange(weName, timeRange)
            self.createGrid("Fcst", weName, "DISCRETE",
                            (hazSSGrid, hazSSKeys), timeRange)

        # Make the timing grids and the max grid from the model
        if sourceOption in ["N-SBN (Default)", "Backup", "PETSS"]:
            self.makeTimingGridsFromModel(self._dataSource, pctStr, "FHAG0", ssea, MHHWMask)
            print "Creating new InundationMax grid..............................................."
            self.createGrid("Fcst", "InundationMax", "SCALAR", surgePctGrid,
                            timeRange, precision=1)

