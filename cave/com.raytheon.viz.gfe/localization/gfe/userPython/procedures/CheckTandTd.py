##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CheckTandTd
#
# Author: Tom LeFebvre
#
# Version Date: 4 January 2006
# Version: 6.5
#
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify
MenuItems = ["Consistency"]

VariableList = [("Check or Force:" , "Check Only", "radio",
                 ["Check Only", "Force: TMin<=T<=TMax\n and Td<=T"]),
                ]

import SmartScript
import TimeRange
import AbsTime
from numpy import *

MODEL = "Fcst"
LEVEL = "SFC"

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    ##
    # Get the list of time ranges at the grid whose element name is WEName
    # contains grids. The model and level of the weather element are assumed
    # to be MODEL and LEVEL, respectively.
    # 
    # @param WEName: Name of a weather element
    # @type WEName: string
    # @return: time ranges at which WEName has data.
    # @rtype: Python list of Python TimeRange objects
    def getWEInventory(self, WEName):
        yesterday = self._gmtime() - (2 * 24 * 3600) # two days ago
        later = self._gmtime() + 10 * 24 * 3600  # 10 days from now
        allTimes = TimeRange.TimeRange(yesterday, later)
        parm = self.getParm(MODEL, WEName, LEVEL);
        inv = parm.getGridInventory(allTimes.toJavaObj())
        trList = []
        for gd in inv:
            tr = TimeRange.TimeRange(gd.getGridTime())
            trList.append(tr)
        return trList

    ##
    # Get time ranges locked by other workstations for the weather element named
    # weName. The model for weName is taken from this object's mutableID() method;
    # the level is LEVEL.
    # @param weName: Name of a weather element.
    # @type weName: string
    # @return: time ranges locked by others
    # @rtype: Python list of TimeRanges; if asJava is True, these are Java 
    # TimeRanges, otherwise they are Python TimeRanges. 
    def getLocksByOthers(self, weName):
        # returns list of time ranges locked by others for this weather element
        parm = self.getParm(self.mutableID(), weName, LEVEL)
        if parm is None:
            return []
        lt = parm.getLockTable()
        jlok = lt.lockedByOther();
        lbo = []
        for i in xrange(jlok.size()):
            tr = jlok.get(i)
            tr = TimeRange.TimeRange(tr)
            lbo.append( tr )
        return lbo

    ##
    # Filter trList, returning only the time ranges that overlap timeRange.
    # @param timeRange: the time range to test against
    # @type timeRange: a Python TimeRange
    # @param trList: the list of time ranges to filter
    # @type trList: Python list of Python TimeRanges
    # @return: The time ranges in trList that overlap timeRange.
    # @rtype: a Python list of Python time ranges
    def overlappingTRs(self, timeRange, trList):
        newTRList = []
        for tr in trList:
            if timeRange.overlaps(tr):
                newTRList.append(tr)

        return newTRList

    ##
    # method so that timeRanges will be sorted earliest to latest
    # @param first: The first time range to compare
    # @type first: Python TimeRange
    # @param last: The second time range to compare
    # @type last: Python TimeRange
    # @return: -1 if first starts before last, 1 if first starts after last,
    #          and 0 if first and last start at the same time.
    # @rtype: integer
    def trSortMethod(self, first, last):
        if first.startTime() < last.startTime():
            return -1
        elif first.startTime() == last.startTime():
            return 0
        else:
            return 1

    ##
    # Concatenate minTRList and maxTRList and sort by starting times.
    # Duplicate time ranges are NOT eliminated.
    # @param minTRList: time ranges of the minT grid
    # @type minTRList: Python list of Python TimeRange objects.
    # @param maxTRList: time ranges of the maxT grid
    # @type maxTRList: Python list of Python TimeRange objects.
    # @return: The combined and sorted collection.
    # @rtype: Python list of Python TimeRange objects
    def combineInventoryLists(self, minTRList, maxTRList):
        bigList = minTRList + maxTRList
        bigList.sort(self.trSortMethod)
        return bigList

    ##
    # Main entry point of this procedure. If varDict["Check or Force"] is 
    # "Check Only", temporary grids will be created. Otherwise, the minT, maxT,
    # T, and Td grids may be changed.
    # @param varDict: Determines whether temporary grids are created or 
    # temperature grids are modified.
    # @type varDict: Python dictionary of strings to strings 
    def execute(self, varDict):
        checkOnly = varDict["Check or Force:"] == "Check Only"

        # remove any temporary WEs we created
        weList = ["TLessThanMin", "TGreaterThanMax", "TdGreaterThanT",
                  "MinGreaterThanMax", "MaxLessThanMin"]
        for we in weList:
            parm = self.getParm(MODEL, we, LEVEL)
            if parm is not None:
                self.unloadWE(MODEL, we, LEVEL)

        self.setToolType("numeric")

        # get all the grids for all elements upfront and update as we modify
        # any grids.  We need to do this because the GFE caches the original
        # version of all grids and there's no way yet to turn this off.

        minTRList = self.getWEInventory("MinT")
        minTDict = self.getGrids(MODEL, "MinT", LEVEL, minTRList, mode = "First")
        
        maxTRList = self.getWEInventory("MaxT")
        maxTDict = self.getGrids(MODEL, "MaxT", LEVEL, maxTRList, mode = "First")
        
        TTRList = self.getWEInventory("T")
        tDict = self.getGrids(MODEL, "T", LEVEL, TTRList, mode = "First")
        
        TdTRList = self.getWEInventory("Td")
        tdDict = self.getGrids(MODEL, "Td", LEVEL, TdTRList, mode = "First")
        
        # get the all locks by other users, so we can detect they are locked
        # before attempting to modify them
        minTLocks = self.getLocksByOthers("MinT")
        maxTLocks = self.getLocksByOthers("MaxT")
        tLocks = self.getLocksByOthers("T")
        tdLocks = self.getLocksByOthers("Td")

        # get the list of edit areas
        eaList = self.editAreaList()

        # get the local WFO domain and make a mask with it
        # local sites may wish to use a different maks so that a larger area
        # is operated on by the tool - for example marine sites may wish to
        # expand it to marine zones as well as land.
        # To change the area, simply use a locally-defined edit area instead
        # of self.getSiteID().  Example:  siteID = "CWAPlusMarineZones"
        #siteID = self.getSiteID() - this was set in A2 - changed to A1 below
        siteID = "ISC_Send_Area"
        if siteID in eaList:  # make sure the edit area is there
            siteEA = self.getEditArea(siteID)  # get the edit area
            siteMask = self.encodeEditArea(siteEA)  # make a mask with siteEA
            siteMask = siteMask.astype(bool8)
        else:
            topo = self.getGridShape()
            siteMask = ones(topo, bool8)
            print siteID, "edit area not found.  Using entire GFE domain."

        # Ensure that MinT <= MaxT first
        minMaxList = self.combineInventoryLists(minTRList, maxTRList)
        foundProblem = False
        for i in xrange(0, len(minMaxList) - 1):
            if minMaxList[i+1] in minTRList:   # previous max modifies min
                maxTR = minMaxList[i]
                minTR = minMaxList[i+1]
                # Make sure these TRs really exist in the inventory
                if maxTR not in maxTRList:
                    continue
                if minTR not in minTRList:
                    continue
                
                minGrid = minTDict[minTR]
                maxGrid = maxTDict[maxTR]

                mask = (minGrid > maxGrid) & siteMask
                if not sometrue(mask):   # make sure some points are set
                    continue
                
                foundProblem = True

                if checkOnly:
                    self.createGrid(MODEL, "MinGreaterThanMax", "SCALAR", mask.astype('float32'),
                                    minTR, minAllowedValue=0.0, maxAllowedValue= 1.0)
                else:  # force the change
                    if minTR in minTLocks:
                        msg = "Can't modify MinT grid at " + str(minTR) + \
                              " locked by another user."
                        self.statusBarMsg(msg, "S")
                        continue
                    # calculate and modify the MinT grid
                    minGrid[mask] = maxGrid[mask]
                    self.createGrid(MODEL, "MinT", "SCALAR", minGrid, minTR)
                    minTDict[minTR] = minGrid # update the minT dictionary

            elif minMaxList[i+1] in maxTRList:   # previous min modifies max
                minTR = minMaxList[i]
                maxTR = minMaxList[i+1]
                # Make sure these TRs really exist in the inventory
                if maxTR not in maxTRList:
                    continue
                if minTR not in minTRList:
                    continue
                maxGrid = maxTDict[maxTR]
                minGrid = minTDict[minTR]

                mask = (maxGrid < minGrid) & siteMask
                if not sometrue(mask):   # make sure some points are set
                    continue
                
                foundProblem = True

                if checkOnly:
                    self.createGrid(MODEL, "MaxLessThanMin", "SCALAR", mask.astype('float32'),
                                    maxTR, minAllowedValue=0.0, maxAllowedValue= 1.0)
                else:   # force the change
                    if maxTR in maxTLocks:
                        msg = "Can't modify MaxT grid at " + str(maxTR) + \
                              " locked by another user."
                        self.statusBarMsg(msg, "S")
                        continue
                    # calculate and modify the MaxT grid
                    maxGrid[mask] = minGrid[mask]
                    self.createGrid(MODEL, "MaxT", "SCALAR", maxGrid, maxTR)
                    # update the minT dictionary with the modified minT grid
                    maxTDict[maxTR] = maxGrid


        # Now check for T < MinT
        for tr in minTRList:
            minTGrid = minTDict[tr]
            
            tInv = self.overlappingTRs(tr, TTRList)
            if tInv == []:  # empty list, keep going
                continue
            
            for tymeRng in tInv:
                # find points in the siteMask where T < MinT
                tGrid = tDict[tymeRng]
                tTooLow = (tGrid < minTGrid) & siteMask
                if not sometrue(tTooLow):
                    continue
                
                foundProblem = True

                if checkOnly:  # just make a grid showing the mask where T < MinT
                    self.createGrid(MODEL, "TLessThanMin", "SCALAR", tTooLow.astype('float32'), tymeRng,
                                    minAllowedValue=0.0, maxAllowedValue= 1.0)
                else: # force T to the MinT value
                    if tymeRng in tLocks:
                        msg = "Can't modify T grid at " + str(tymeRng) + \
                              " locked by another user."
                        self.statusBarMsg(msg, "S")
                        continue
                    tGrid[tTooLow] = minTGrid[tTooLow]
                    self.createGrid(MODEL, "T", "SCALAR", tGrid, tymeRng)
                    tDict[tymeRng] = tGrid   # update the tDict


        # check for T > MaxT
        for tr in maxTRList:
            # get the grid first
            maxTGrid = maxTDict[tr]

            # then warp the end time so we include T grids ending at 01z
            startTime = tr.startTime() 
            endTime = tr.endTime().unixTime() 
            roundedTime = int((endTime + 43200) / 86400) * 86400 + 3600
            endTime = max(endTime, roundedTime)
            endTime = AbsTime.AbsTime(endTime)
            timeRange = TimeRange.TimeRange(startTime, endTime)
            
            # use the warpedTR to fetch the T inventory
            tInv = self.overlappingTRs(timeRange, TTRList)
            if tInv == []:  # empty list, keep going
                continue
            
            for tymeRng in tInv:
                # find points in the siteMask where T > MaxT
                tGrid = tDict[tymeRng]
                tTooHigh = (tGrid > maxTGrid) & siteMask
                if not sometrue(tTooHigh):   # make sure some points are set
                    continue

                foundProblem = True

                if checkOnly:  # just make a grid
                    self.createGrid(MODEL, "TGreaterThanMax", "SCALAR", tTooHigh.astype('float32'), tymeRng,
                                    minAllowedValue=0.0, maxAllowedValue= 1.0)
                else:  # force T to the MaxT value
                    if tymeRng in tLocks:
                        msg = "Can't modify T grid at " + str(tymeRng) + \
                              " locked by another user."
                        self.statusBarMsg(msg, "S")
                        continue
                    tGrid[tTooHigh] = maxTGrid[tTooHigh]
                    self.createGrid(MODEL, "T", "SCALAR", tGrid, tymeRng)
                    tDict[tymeRng] = tGrid   # update the tDict

        # Now check T < Td
        for tr in TTRList:

            # make sure there's a matching Td grid
            if not tr in TdTRList:
                continue

            tGrid = tDict[tr]
            tdGrid = tdDict[tr]

            # find points in the siteMask where Td > T
            TdTooHigh = (tdGrid > tGrid) & siteMask
            if not sometrue(TdTooHigh):  # make sure some points are set
                continue
            
            foundProblem = True

            if checkOnly:   # just make a grid
                self.createGrid(MODEL, "TdGreaterThanT", "SCALAR", TdTooHigh.astype('float32'), tr,
                                minAllowedValue=0.0, maxAllowedValue= 1.0)
            else: # force Td <= T
                if tr in tdLocks:
                    msg = "Can't modify Td grid at " + str(tInv[i]) + \
                          " locked by another user."
                    self.statusBarMsg(msg, "S")
                    continue
                tdGrid[TdTooHigh] = tGrid[TdTooHigh]
                self.createGrid(MODEL, "Td", "SCALAR", tdGrid, tr)
                tdDict[tr] = tdGrid   # update the tdDict


        if not foundProblem: 
            msg = "CheckTandTd found no inconsistencies."
            self.statusBarMsg(msg, "R")

