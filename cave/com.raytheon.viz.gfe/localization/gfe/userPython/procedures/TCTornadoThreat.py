# ----------------------------------------------------------------------------
#
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TornadoFloodThreat
#
# Author: Tom LeFebvre/Pablo Santos
# Updated: April 16, 2012 to lower Low Category Threshold and hide Sliding Bars
# Migrated procedure for AWIPS2. Updated 6/22/2012.  S.O.
# Last Modified: Sept 19, 2014: Updated Low to Elevated for 2015 Official Implementation. PS
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Populate"]

import SmartScript
import time
import sys
import AbsTime
import TimeRange

VariableList = [("NOTE: Day 1 Tornado Probabilities Used by Procedure:", "", "label"),
                #("Very Low" , "02", "radio", ["02"]),
                ("Elevated" , "02", "radio", ["02"]),
                ("Mod" ,"15", "radio", ["15"]),
                ("High","30", "radio", ["30"]),
                ("Extreme" ,"45", "radio", ["45"]),
                ("Day 2: Prob Svr Wx (ptotsvr) >= 5: Elevated; ptotsvr >= 60%: Mod", "", "label"),
                ("Day 2: Prob Sig Svr Wx (ptotxsvr) >= 10: Elevated -> Mod and Mod -> High", "", "label"),
                ("Day 3: Prob Svr Wx (ptotsvr) >= 5: Elevated", "", "label"),
                ("Day 3: Prob Sig Svr Wx (ptotxsvr) >= 10: Elevated -> Mod", "", "label"),
                ("NOTE: After applying logic above", "", "label"),
                ("threat level is the max composite from Day 1-3", "", "label")               
                ]

try:
    from Numeric import *
except:
    from numpy import *

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Make a timeRange based on the start and end int times
    def makeTimeRange(self, start, end):
        
        try:  # AWIPS 1 code
            import AFPS
            startTime = AFPS.AbsTime(start)
            endTime = AFPS.AbsTime(end)
    
            tr = AFPS.TimeRange(startTime, endTime)

        except:   # AWIPS 2 code
            import TimeRange, AbsTime
            startTime = AbsTime.AbsTime(start)
            endTime = AbsTime.AbsTime(end)
    
            tr = TimeRange.TimeRange(startTime, endTime)

        return tr

    def variableExists(self, modelName, weName, weLevel):

        # it turns out the the modelName will not match the dbID().model()
        # directly, so it needs to be massaged a bit.
        parts = modelName.split("_")
        if len(parts) >= 4:
            modelName = parts[3]
            
        availParms = self.availableParms()    
        for pName, level, dbID in availParms:
            if dbID.modelName().find(modelName) > -1:
                if pName.find(weName) > -1 and level.find(weLevel) > -1:
                    return True

        return False

    def getWEInventory(self, modelName, WEName, timeRange = None):

        allTimes = TimeRange.allTimes()
        
        if timeRange is None:
            timeRange = allTimes

        weLevel = "SFC"

        if not self.variableExists(modelName, WEName, weLevel):
            #print WEName, "does not exist in model", modelName
            return []

        try:
            gridInfo = self.getGridInfo(modelName, WEName, weLevel, timeRange)
        except:
            #print "GridInfo failed for:", modelName, WEName, weLevel, timeRange
            return []

        trList = []
        for g in gridInfo:
            start = g.gridTime().startTime().unixTime()
            end = g.gridTime().endTime().unixTime()
            tr = self.makeTimeRange(start, end)
            if timeRange.overlaps(tr):
                trList.append(tr)

        return trList    

    # get the current time, truncates to the last six hour value.
    # returns a timeRange with this startTime until 24 hrs from this time
    def make6hrTimeRange(self):
        startTime = int(self._gmtime().unixTime()/ (3600 * 6)) * 3600 * 6
        endTime = startTime + (3600 * 6)
        
        timeRange = self.makeTimeRange(startTime, endTime)

        return timeRange

    # Returns a list of dbIdentifiers that match the specified model
    # name, weName and level.
    def getModelList(self, modelName, weName, weLevel):
        modelList = []

        availParms = self.availableParms()

        for pName, level, dbID in availParms:
            if dbID.modelName().find(modelName) > -1:
                if pName.find(weName) > -1:
                    if level.find(weLevel) > -1:
                        if dbID.modelIdentifier() not in modelList:   
                            modelList.append(dbID.modelIdentifier())
        return modelList

    def determineDay(self, modelTime, validTime):

        diff = (validTime - modelTime) / 3600
        if diff < 36:
            return 1
        elif diff >= 36 and diff < 60:
            return 2
        elif  diff >= 60:
            return 3

        return 0

    # returns a unix time based on the specified model ID.
    def getModelTime(self, modelName):

        timeStr = modelName[-13:]

        year = int(timeStr[0:4])
        month = int(timeStr[4:6])
        day = int(timeStr[6:8])
        hour = int(timeStr[9:11])

        absTime = AbsTime.absTimeYMD(year, month, day, hour, 0, 0)
        absTime = AbsTime.absTimeYMD(year, month, day, hour, 0, 0)
        
        return absTime.unixTime()

    def getTornadoGrid(self, varName, dayNum):
        siteID = self.getSiteID()
        SPCModelName = siteID + "_D2D_SPC"
        SPCVarName = varName
        SPCLevel = "SFC"

        modelList = self.getModelList("SPC", SPCVarName, SPCLevel)
        #print "Processing varName, dayNum: ", varName, dayNum

        hours24 = 24 * 3600

        for modelName in modelList:

            trList = self.getWEInventory(modelName, SPCVarName)
            #print "modelName trList:", modelName, trList
            if len(trList) == 0:   # no grids found for this version
                continue   # go on to older versions

            modelTime = self.getModelTime(modelName)
            # get the current time rounded to the nearest 12Z
            currentTime = (int(time.time() / hours24) * hours24) + (12 * 3600)

            for tr in trList:
                gridDayNum = self.determineDay(currentTime,
                                               tr.startTime().unixTime())
                #print "modelName, modelTime, and gridDayNum:", modelName, modelTime, gridDayNum
                if gridDayNum == dayNum:
                    grid = self.getGrids(modelName, SPCVarName, SPCLevel, tr)
                    return grid

        return None

    # This method adjusts an existing threat grid
    def adjustTornadoGrid(self, tornadoThreat, threatKeys, var, dayNum, extThreshold):
        D2DGrid = self.getTornadoGrid(var, dayNum)
        if D2DGrid is None:
            #print "No grid found for", var, "day:", dayNum
            return tornadoThreat

        # Account for offices using the four key arrangement
        # Just change the "Very Low" to "Low" in the threshDict
        #if "Very Low" not in threatKeys:
        #    # find all places greater than "Very Low or Low" in the tornadoThreat
        #    lowIndex = self.getIndex("Low", threatKeys)
        #else:
        #    lowIndex = self.getIndex("Very Low", threatKeys)
        #lowMask = greater_equal(tornadoThreat, lowIndex)

        lowMask = greater(tornadoThreat, 0)

        # finds all places in the extreme grid >= to the extThreshold
        xMask = greater_equal(D2DGrid, extThreshold)

        # increment the threat where these masks intersect
        mask = lowMask & xMask

        # make sure we're not incremening too far
        extremeIndex = self.getIndex("Extreme", threatKeys)
        extremeMask = equal(tornadoThreat, extremeIndex)
        mask = mask - extremeMask  # subtract the extreme area

        # increment the category.  This code assumes that the categories are
        # defined in increaing order of severity.
        tornadoThreat[mask] += 1

        return tornadoThreat
        

    def setTornadoGrid(self, tornadoThreat, threatKeys, var, dayNum, threshDict):

        D2DGrid = self.getTornadoGrid(var, dayNum)
        if D2DGrid is None:
            #print "No grid found for", var, "day:", dayNum
            return tornadoThreat

        # Account for offices using the four key arrangement
        # Just change the "Very Low" to "Low" in the threshDict
        #print "THREATKEYS ARE: ", threatKeys
        #print "THRESHDICT IS: ", threshDict.keys()       
        if "Very Low" not in threatKeys:
            for key in threshDict.keys():
                if threshDict[key] == "Very Low":
                    threshDict[key] = "Elevated"
        dictKeys = threshDict.keys()
            
        #print "unsorted dictKeys: ", dictKeys
        dictKeys.sort()  # sort lowest to highest value
        #print "sorted dictKeys: ", dictKeys
       

        # Set the grid values based on the tornado prob grid and thresholds
        for key in dictKeys:
            thresh = int(key)
            #print "THRESH IS: ", thresh
            keyIndex = self.getIndex(threshDict[key], threatKeys)
            # make a temp grid where the thresholds are exceeded
            tempGrid = where(greater_equal(D2DGrid, thresh), int32(keyIndex), int32(0))
            # calculate areas where this temp grid exceeds the threatGrid
            mask = greater(tempGrid, tornadoThreat)
            # update the threatGrid for these areas only
            tornadoThreat[mask] = keyIndex

        return tornadoThreat
                    
    def execute(self, varDict):

        threatWEName = "TornadoThreat"
        
        threatKeys = self.getDiscreteKeys(threatWEName)
       
        # make a dict to store thresholds from the UI
        ptorDict = {}

        for key in threatKeys:
            if key == "None":
                continue
            ptorDict[varDict[key]] = key

        #print "***************************"
        #print "ptorDict is:", ptorDict
        #print "***************************"

        # Set up the data for processing the various grids.
        # Each entry consists of the D2D variable to be checked,
        # the day number of that grid, and a dictionary that defines
        # each threshold value and the corresponding discrete value.
        # Note the grids will be processed in the order defined in
        # this list.
        actionList = [
            ("ptor", 1, ptorDict, "sigtrndprob", 10),  # ptorDict comes from the GUI
            
            ("prsvr", 2, { 5 : "Very Low",
                            #15 : "Elevated",
                             5 : "Elevated",
                            60 : "Mod",
                           },
                            "prsigsv", 10),

            ("prsvr", 3, { 5 : "Very Low",
                            #15 : "Elevated",
                             5 : "Elevated",
                             
                           },
                            "prsigsv", 10),

                    ]

         # make a grid of zeros.  This will be the TornadoThreat grid
        tornadoThreat = self.empty()

        for var, dayNum, threshDict, xVar, xThreshold in actionList:
            tornadoThreat = self.setTornadoGrid(tornadoThreat, threatKeys,
                                                var, dayNum, threshDict)

            # now adjust the grid based on the extreme grid category
            tornadoThreat = self.adjustTornadoGrid(tornadoThreat, threatKeys,
                                                   xVar, dayNum, xThreshold)

        # make a timeRange - 6 hours long, rounded to nearest hour
        startTime = int(self._gmtime().unixTime()/ 3600) * 3600
        endTime = startTime + (6 * 3600)
        threatTR = self.makeTimeRange(startTime, endTime)

        # remove any old grids that are lying around
        startTime = int(self._gmtime().unixTime()/ 3600) * 3600 - (24 * 3600)
        endTime = startTime + (24 * 3600 * 10)
        removeTR = self.makeTimeRange(startTime, endTime)
        self.deleteCmd([threatWEName], removeTR)

        # create the TornadoThreat Grid
        self.createGrid("Fcst", threatWEName, "DISCRETE",
                        (tornadoThreat, threatKeys), threatTR,
                        discreteKeys=threatKeys,
                        discreteOverlap=0,
                        discreteAuxDataLength=2,
                        defaultColorTable="Hazards")

        return
