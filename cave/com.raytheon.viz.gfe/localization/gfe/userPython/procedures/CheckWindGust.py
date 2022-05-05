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
# CheckWindGust
#
# Author: Tom Mazza, based on Tom LeFebvre's CheckTandTd
#
# Version Date: 6 Oct 2006
# Version: 1.0
#
# Modified by Tom Mazza 6 Jun 2005 to use local ISC_Send_Area
# and to redo RH, and, if loaded in GE at the ttime, HeatIndex and WindChill,
# on "Force: TMin<=T<=TMax\n and Td<=T" option anytime T and / or Td are
# changed (change on Td only does not affect WindChill).
#
# 7/27/2015    yteng     Use the time range selected in the Grid Manager if any, 
#                        and retrived teh necessary grids for improved efficiency
#
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify
MenuItems = ["Consistency"]

VariableList = [("Check or Force:" , "Check Only", "radio",
                 ["Check Only", "Force: WindGust>=Wind"])]

import SmartScript
import time
import TimeRange
from JUtil import JavaWrapperClass
from numpy import *

MODEL = "Fcst"
LEVEL = "SFC"
DAY_IN_SECS = 24 * 3600

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)


    def getWEInventory(self, WEName, timeRange=None):
        if timeRange is None:
            yesterday = self._gmtime() - (2 * DAY_IN_SECS) # two days ago
            later = self._gmtime() + 10 * DAY_IN_SECS  # 10 days from now
            timeRange = TimeRange.TimeRange(yesterday, later)
        if isinstance(timeRange, JavaWrapperClass):
            timeRange = timeRange.toJavaObj()
        parm = self.getParm(MODEL, WEName, LEVEL)
        inv = parm.getGridInventory(timeRange)
        trList = []
        for gd in inv:
            tr = TimeRange.TimeRange(gd.getGridTime())
            trList.append(tr)

        return trList


    def getLocksByOthers(self, weName):
        # returns list of time ranges locked by others for this weather element
        parm = self.getParm(self.mutableID(), weName, LEVEL)
        if parm is None:
            return []
        lt = parm.getLockTable()
        jlok = lt.lockedByOther()
        lbo = []
        for i in range(jlok.size()):
            tr = jlok.get(i)
            tr = TimeRange.TimeRange(tr)
            lbo.append( tr )
        return lbo

    def overlappingTRs(self, timeRange, trList):
        newTRList = []
        for tr in trList:
            if timeRange.overlaps(tr):
                newTRList.append(tr)

        return newTRList

    def combineInventoryLists(self, minTRList, maxTRList):
        bigList = minTRList + maxTRList
        bigList.sort(key=lambda x: x.startTime())
        return bigList

    def execute(self, timeRange, varDict):
        checkOnly = varDict["Check or Force:"] == "Check Only"
        startWindChill = 10     ## First month to report wind chill
        endWindChill = 4        ## Last month to report wind chill

        #  Get local edit area simply by using the baseline edit area
        eaList = self.editAreaList()
        siteID = self.getSiteID()

        if siteID in eaList:  # make sure the edit area is there
            siteEA = self.getEditArea(siteID)  # get the edit area
            siteMask = self.encodeEditArea(siteEA)  # make a mask with siteEA
            siteMask = siteMask.astype(bool8)
        else:
            topo = self.getGridShape()
            siteMask = ones(topo, bool8)
            print(siteID, "edit area not found.  Using entire GFE domain.")

        errorsFound = "no"  #  To keep track of any errors found for
                            #  status bar message at the end.

        # remove any temporary WEs we created
        weList = ["WindGustLessThanWindSpeed"]
        for we in weList:
            parm = self.getParm(MODEL, we, LEVEL)
            if parm is not None:
                self.unloadWE(MODEL, we, LEVEL)

        self.setToolType("numeric")
        
        # if no timeRange selected then make a big timeRange
        if timeRange is None or not timeRange.isValid():
            start = self._gmtime() - (2 * DAY_IN_SECS) # two days ago
            end = self._gmtime() + (10 * DAY_IN_SECS)  # 10 days from now
            timeRange = TimeRange.TimeRange(start, end)

        # get all the grids for all elements upfront and update as we modify
        # any grids.  We need to do this because the GFE caches the original
        # version of all grids and there's no way yet to turn this off.

        WindDirDict = {}
        WindSpeedDict = {}
        WindGustDict = {}

        WindTRList = self.getWEInventory("Wind", timeRange)
        for tr in WindTRList:
            grid = self.getGrids(MODEL, "Wind", LEVEL, tr, mode = "First")
            WindDirDict[tr] = grid[1]
            WindSpeedDict[tr] = grid[0]

        WindGustTRList = self.getWEInventory("WindGust", timeRange)
        WindGustDict = self.getGrids(MODEL, "WindGust", LEVEL, WindGustTRList, mode = "First")

        # get the all locks by other users, so we can detect they are locked
        # before attempting to modify them
        WindGustLocks = self.getLocksByOthers("WindGust")

        nowZ = time.gmtime(time.time())
        curMon = nowZ[1]

        WindChangeTools = []
        if curMon >= startWindChill or curMon <= endWindChill:
            WindChangeTools.append(("WindChillTool", "WindChill"))

        WindGustOnlyChangeTools = []
        databaseID = self.findDatabase(MODEL)

        # Now check WindGust >= WindSpeed  # was T < Td
        for tr in WindTRList:

            # make sure there's a matching WindGust grid
            if not tr in WindGustTRList:
                continue

            WindSpeedGrid = WindSpeedDict[tr]
            WindGustGrid = WindGustDict[tr]

            # find points in the siteMask where WindGust < Wind
            mask = (WindGustGrid < WindSpeedGrid) & siteMask
            if not sometrue(mask):   # make sure some points are set
                continue

            errorsFound = "yes"
            if checkOnly:   # just make a grid
                self.createGrid(MODEL, "WindGustLessThanWindSpeed", "SCALAR", mask.astype(float32), tr, minAllowedValue=0.0, maxAllowedValue= 1.0)
            else: # force WindGust >= WindSpeed
                if tr in WindGustLocks:
                    msg = "Can't modify WindGust grid at " + str(tInv[i]) + \
                          " locked by another user."
                    self.statusBarMsg(msg, "S")
                    continue
                editArea = self.decodeEditArea(mask.astype(float32))
                WindGustGrid = where(mask, WindSpeedGrid, WindGustGrid)
                self.createGrid(MODEL, "WindGust", "SCALAR", WindGustGrid, tr)
                WindGustDict[tr] = WindGustGrid   # update the tdDict
                for toolName, elementName in WindChangeTools:
                    parm = (elementName, LEVEL, databaseID)
                    if (toolName in WindGustOnlyChangeTools) and (parm in self.loadedParms()):
                            gridInfo = self.getGridInfo(MODEL, elementName, LEVEL, tr)
                            if gridInfo == []:
                                self.createFromScratchCmd([elementName], tr,
                                                          repeat=1, duration=1)
                            error = self.callSmartTool(toolName, elementName,
                                                       editArea, tr)
                            if error is not None:
                                break


        # Send a message to the status bar
        if errorsFound == "yes":
            self.statusBarMsg('CheckWindGust completed - One or more Flags on the play.', 'R')
        else:
            self.statusBarMsg('CheckWindGust completed - No Flags!', 'R')
