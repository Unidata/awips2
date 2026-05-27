# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# MergeProposedSS.py
# Version 3.0 - 13 April 2016
# Authors: Nate Hardin, Tom LeFebvre, Pablo Santos
# ----------------------------------------------------------------------------
#
#
# SOFTWARE HISTORY
#
# Date          Ticket#    Engineer    Description
# ------------- ---------- ----------- -----------------------------------------
# Sep 19, 2016  19293      randerso    Initial baseline check in
# Feb 09, 2017  6128       randerso    Fix removal from combined hazards.
#                                      Correctly handle multiple grids in the
#                                      48 hour window
# Mar 30, 2017             swhite      Precvnts from running if hazard grids are not merged or are not locked
# Nov 16, 2017             RA/PS       Fixed problem with ProposedSSnc TR not properly reflected on Final Hazards grid.
# Aug 24, 2018  20727      ryu         Change hazard conflict message per Shannon.
# Apr 24, 2019  21021      swhite      Adjust so grid is Fcst not ISC for HFO
# Jun 18, 2019  21021      nhardin     Code refactor/clean up
# Jan 26, 2021  22443      nhardin     Remove circular logic
########################################################################

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazard
MenuItems = ["Populate"]

import time
import HazardUtils
import LogStream
import TimeRange
import TropicalUtility
import SmartScript
import numpy as np


class Procedure (TropicalUtility.TropicalUtility):
    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._hazUtils = HazardUtils.HazardUtils(dbss, None)
        if self.getSiteID() == 'HFO':
            self._ssWeName = "ProposedSS"
            self._ssDbName = "Fcst"
        else:
            self._ssWeName = 'ProposedSSnc'
            self._ssDbName = 'ISC'

    def checkGFE_ForConflicts(self):
        '''
        Checks for various conflicts/errors with GFE before running tool
        '''
        if not self._hazUtils._hazardsLoaded():
            self.statusBarMsg("Hazards Weather Element must be loaded in "+
              "the GFE before running MergeProposedSS.", "S")
            self.cancel()

        if self._hazUtils._tempWELoaded():
            self.statusBarMsg("There are temporary hazard grids loaded. " +
                "Please merge all hazards grids before running MergeProposedSS.", "S")
            self.cancel()

        if self.lockedByOther('Hazards', 'SFC'):
            self.statusBarMsg("There are conflicting locks (red locks - owned by others) on Hazards.  " +
                "Please resolve these before running MergeProposedSS", "S")
            self.cancel()

    def createPropTR(self):
        '''
        Creates time range for ProposedSS, checks ProposedSS exists,
        then set TR to span full inventory of ProposedSS
        '''
        start = int(self._gmtime().unixTime() / 3600) * 3600
        end = start + 48 * 3600
        timeRange = self.GM_makeTimeRange(start, end)
        propTRList = self.GM_getWEInventory(self._ssWeName, dbase = self._ssDbName, timeRange=timeRange)
        propTRList = self.checkProposedSS(propTRList)

        propTR = self.GM_makeTimeRange(propTRList[0].startTime().unixTime(), propTRList[-1].endTime().unixTime())

        return propTR, propTRList

    def checkForMaskConflicts(self):
        '''
        Checks each site for any conflicts and returns a list of conflicting sites
        '''
        siteID = self.getSiteID()
        cwaMask = self.encodeEditArea(siteID)

        if self.checkForAnyConflicts(cwaMask):
            self.statusBarMsg("Hazard ETN conflicts between " + siteID +
                              " and ProposedSS. Check Wind and Surge ETNs.", "U")
            self.cancel()

    def removeHazards(self, keyList):
        '''
        Removes all hazards found in specific keyList from Hazards grid.
        Any hazard matching any key in the keyList is removed.
        '''
        trList = self.GM_getWEInventory("Hazards")

        for tr in trList:
            hazGrid, hazKeys = self.getGrids("Fcst", "Hazards", "SFC", tr)

            for hazKey in hazKeys:
                subKeys = hazKey.split("^")
                for key in keyList:
                    for subKey in subKeys:
                        if subKey.startswith(key):
                            self._hazUtils._removeHazard("Hazards", tr, subKey)

    def makeHazardGrids(self, hazTRList, propTR, propTRList):
        '''
        Creates Hazard Grids based on hazTRList
        '''
        if len(hazTRList) == 0:
            for tr in propTRList:
                self.makeEmptyHazardGrid(tr)
        else:
            if propTR.startTime() < hazTRList[0].startTime():
                newTR = TimeRange.TimeRange(propTR.startTime(), hazTRList[0].startTime())
                self.makeEmptyHazardGrid(newTR)
            if hazTRList[-1].endTime() < propTR.endTime():
                 newTR = TimeRange.TimeRange(hazTRList[-1].endTime(), propTR.endTime())
                 self.makeEmptyHazardGrid(newTR)
            self.splitCmd(["Hazards"], propTR)

        hazTRList = self.GM_getWEInventory("Hazards")

        return hazTRList

    def addProposedSS_ToHazards(self, hazTRList, propTR, propTRList):
        '''
        Adds ProposedSS to Hazard Grid
        '''
        propGrid, propKeys = self.getGrids(self._ssDbName, self._ssWeName, "SFC", propTRList[-1])

        for hazTR in hazTRList:
            if not hazTR.overlaps(propTR):
                continue

            for propKey in propKeys:
                if propKey != "<None>":
                    propIndex = self.getIndex(propKey, propKeys)
                    mask = propGrid == propIndex
                    self._hazUtils._addHazard("Hazards", hazTR, propKey, mask, combine=1)

    def checkForAnyConflicts(self, cwaMask):
        '''
        Checks for conflicts with any future NHC and WFO hazards. Returns
        list of CWAs conflicting with NHC hazards. Uses TropicalUtility:anyHazardConflict
        '''
        propTRList = self.GM_getWEInventory(self._ssWeName, self._ssDbName)
        proposedGrid = self.getGrids(self._ssDbName, self._ssWeName, "SFC", propTRList[-1])

        hazTRs = self.GM_getWEInventory("Hazards")

        currentTime = self._gmtime()

        for tr in hazTRs:
            if tr.endTime() < currentTime:
                continue

            hazardGrid = self.getGrids("Fcst", "Hazards", "SFC", tr)

            if self.anyHazardConflicts(hazardGrid, proposedGrid, cwaMask):
                return True

        return False

    def makeEmptyHazardGrid(self, timeRange):
        '''
        Creates an empty hazard grid
        '''
        hazGrid = self.empty(np.int8)
        hazKeys = ["<None>"]
        self.createGrid("Fcst", "Hazards", "DISCRETE", (hazGrid, hazKeys), timeRange)

    def checkProposedSS(self, propTRList):
        '''
        Make sure we have a ProposedSS before we begin
        '''
        if not propTRList:
            self.statusBarMsg("No " + self._ssWeName + " Fcst grid found. Tool aborting.", "U")
            self.cancel()

        return propTRList

    def execute(self):
        self.checkGFE_ForConflicts()
        propTR, propTRList = self.createPropTR()
        self.checkForMaskConflicts()
        self.removeHazards(["SS.A", "SS.W"])
        hazTRList = self.GM_getWEInventory("Hazards")
        hazTRList = self.makeHazardGrids(hazTRList, propTR, propTRList)
        self.addProposedSS_ToHazards(hazTRList, propTR, propTRList)
        if self.getSiteID() == 'HFO':
            self.callProcedure('Finalize_KML')