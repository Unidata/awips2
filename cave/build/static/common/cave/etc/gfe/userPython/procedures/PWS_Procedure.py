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
# Authors: Tom LeFebvre, Pablo Santos
# Last Modified: Dec 10, 2010  -  new equations to process 12 hour incremental wind speed probability grids (PWS(D,N)) from 6 hourly pws34 and pws64 grids.
#                March 23, 2011 - Corrected for proper accounting of input inventory (pws34 and pws64)
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Populate"]

import SmartScript
import AbsTime
import TimeRange
import string
import time
from numpy import *

LEVEL = "SFC"

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # caluclates maximum value at each grid point
    def maxGrid(self, gridList):

        if len(gridList) < 1:
            return None

        gridMax = zeros(gridList[0].shape) - 100000.0  # a large negative number
        for grid in gridList:
            gridMax = where(grid > gridMax, grid, gridMax)

        return gridMax

    def intGrid(self, gridList):

        if len(gridList) < 1:
            return None
        
        gridInt = []
        i=0
        while i < len(gridList):
            gridInt.append(gridList[i] - gridList[i])
            i += 1

        i=1
        while i < len(gridList):

            gridInt[i] = gridList[i] - gridList[i-1]
            i += 1

        return gridInt

    # return a list of modelIDs that match the specified string.
    def getModelIDList(self, matchStr):

        availParms = self.availableParms()

        modelList = []
        for pName, level, dbID in availParms:
            modelId = dbID.modelIdentifier()
            if modelId.find(matchStr) > -1:
                if modelId not in modelList:
                    modelList.append(modelId)
    
        return modelList

    # Get grid inventory for the specified info
    def getWEInventory(self, modelName, WEName, level, timeRange):
        weTR = TimeRange.allTimes().toJavaObj()
        gridInfo = self.getGridInfo(modelName, WEName, level, weTR)

        trList = []
        for g in gridInfo:
            start = g.gridTime().startTime().unixTime() * 1000
            end = g.gridTime().endTime().unixTime() * 1000
            tr = TimeRange.TimeRange(start,end)
            if tr.overlaps(timeRange):
                trList.append(tr)   

        return trList

    # determines the latest pws model currently available
    def getLatestPWSModel(self):
        # Changed to FCSTRGEN to match experimental dataset
        #modelIDList = self.getModelIDList("FCSTRGEN")
        modelIDList = self.getModelIDList("TPCProb")
        modelIDList.sort()

        if len(modelIDList) == 0:
            self.statusBarMsg("HERE No PWS models found in your inventory.", "S")
            return ""

        # the last one is the latest
        return modelIDList[-1]

    # Examines the full inventory and computes a timeRange that encompasses
    # all grids
    def getModelTimeRange(self, modelID, param, weNames):
        before = AbsTime.current() - (7 * 24 * 3600) # 7 days ago
        later = AbsTime.current() + 8 * 24 * 3600  # 8 days from now
        timeRange = TimeRange.TimeRange(before, later)
        self.deleteCmd(weNames, timeRange)
        gridInv = self.getGridInfo(modelID, param, "FHAG10", timeRange)
        if len(gridInv) == 0:
            self.statusBarMsg("No grids available for model:" + modelID, "S")
            return None
        
        minTime = later.unixTime()
        maxTime = before.unixTime()
        for g in gridInv:
            start = g.gridTime().startTime().unixTime()
            end = g.gridTime().endTime().unixTime()
            if start < minTime:
                minTime = start
            if end > maxTime:
                maxTime = end



        # adjust the times since this data is cumulative probabilities
        #minTime = minTime - (6 * 3600)
        #minTime = minTime + (14*3600)
        #maxTime = maxTime - 3600
        modelTR = TimeRange.TimeRange(AbsTime.AbsTime(minTime), 
                                      AbsTime.AbsTime(maxTime))
        #modelTR_adv = TimeRange.TimeRange(AbsTime.AbsTime(minTime+(3*3600)), AbsTime.AbsTime(maxTime))
        #self.remove(['pwsD34'],minTime - (24*3600),minTime, 'Fcst')
        #self.remove(['pwsN34'],minTime - (24*3600),minTime, 'Fcst')
        #self.remove(['pwsD64'],minTime - (24*3600),minTime, 'Fcst')
        #self.remove(['pwsN64'],minTime - (24*3600),minTime, 'Fcst')

        #print "MODELTR", modelTR, minTime
        return modelTR, minTime, maxTime

    # Calculate the PWS grid using the formula:
    #
    #   pwsGrid = pws1 + (prob2 - prob1)
    #
    # where pws1 and pws2 are the first and second incremental wind speed probability input grids in the PWS(D,N)
    # time interval and prob1 and prob2 are the first and second cumulative wind speed probability input grid in the
    # PWS(D,N) time interval.  There's a little extra code devoted to making sure that
    # the grids are in the correct time order, since this is very important in the equation.


    def makePWSScratch(self, weName, timeRange):
        # make the D2D elementName
        pwsWE = "pws" + weName[-2:]
        probWE = "prob" + weName[-2:]

        #print "pwsWE and probWE are: ", pwsWE, probWE

        # fetch the pws and prob grids
        modelName = self.getLatestPWSModel()

        modelLevel = "FHAG10"

        # To make sure that we're fetching model grids in the right temporal
        # order, get the inventory and sort them first before fetching.
        pwsInv = self.getWEInventory(modelName, pwsWE, modelLevel, timeRange)
        probInv = self.getWEInventory(modelName, probWE, modelLevel, timeRange)
        count = 0

        for item in pwsInv:
            count+=1

        if count > 1:
            print "pwsInv is GOOD"
        else:
            print "pwsInv is BAD DELETING"
            self.deleteCmd([weName],timeRange)
        
        return 

    def makePWSGrid(self, weName, timeRange):
        # make the D2D elementName
        pwsWE = "pws" + weName[-2:]
        probWE = "prob" + weName[-2:]

        # print "pwsWE and probWE are: ", pwsWE, probWE

        # fetch the pws and prob grids
        modelName = self.getLatestPWSModel()

        modelLevel = "FHAG10"

        # To make sure that we're fetching model grids in the right temporal
        # order, get the inventory and sort them first before fetching.
        pwsInv = self.getWEInventory(modelName, pwsWE, modelLevel, timeRange)
        probInv = self.getWEInventory(modelName, probWE, modelLevel, timeRange)

        pwsInv.sort()
        probInv.sort()

        # Now get the grids individually to ensure proper time order
        pws1 = self.getGrids(modelName, pwsWE, modelLevel, pwsInv[0],
                             mode="First")
        pws2 = self.getGrids(modelName, pwsWE, modelLevel, pwsInv[1],
                             mode="First")
        prob1 = self.getGrids(modelName, probWE, modelLevel, probInv[0],
                             mode="First")
        prob2 = self.getGrids(modelName, probWE, modelLevel, probInv[1],
                             mode="First")

        # Calculate the grid        
        grid = pws1 + prob2 - prob1

        # clip the grid at 100.0 percent
        grid = clip(grid, 0.0, 100.0)
        
        return grid

    # main method
    def execute(self):
        
        modelID = self.getLatestPWSModel()
        weNames = ["prob34", "prob64", "pws34int", "pws64int"]
        modelTR_cum = self.getModelTimeRange(modelID, "prob34", weNames)
        weNames = ["pwsD34", "pwsD64", "pwsN34", "pwsN64"]        
        modelTR_inc = self.getModelTimeRange(modelID, "pws34", weNames)

        timeRange = modelTR_cum[0]
        minTime = modelTR_cum[1]
        maxTime = modelTR_cum[2]


# THE FOLLOWING SEGMENT POPULATES THE CUMULATIVE PROBABILITIES (prob34 and prob64)
# AND CALCULATES AND POPULATES THE INTERVAL PROBABILITIES (pws34int and pws64int)
# FROM THE CUMULATIVE PROBABILITIES. THESE ARE FOR USE BY THE NEW HLS FORMATTER
# AS WELL AS THEIR COMPANION GRAPHICAL COMPONENT, THAT IS, THE TROPICAL IMPACT
# GRAPHICS.

        gridList_int34 = self.getGrids(modelID, "prob34", "FHAG10",
                                       timeRange, mode = "List")
        nt = len(gridList_int34)
#        print "LENGTH OF GRID IS: ", nt
        
        tr = TimeRange.TimeRange(AbsTime.AbsTime(minTime+3*3600), AbsTime.AbsTime(maxTime))
        self.createGrid("Fcst", "prob34", "SCALAR", gridList_int34[nt-1], tr)
        intGrid34 = self.intGrid(gridList_int34)      

        gridList_int64 = self.getGrids(modelID, "prob64", "FHAG10",
                                       timeRange, mode = "List")                              
        nt=len(gridList_int64)
        tr = TimeRange.TimeRange(AbsTime.AbsTime(minTime+3*3600), AbsTime.AbsTime(maxTime))
        self.createGrid("Fcst", "prob64", "SCALAR", gridList_int64[nt-1], tr)
        intGrid64 = self.intGrid(gridList_int64)

        i=1
        while i < len(intGrid34):
            minT = minTime + (i-1)*21600
            maxTime = minT + 21600
            tr = TimeRange.TimeRange(AbsTime.AbsTime(minT), AbsTime.AbsTime(maxTime))
            self.createGrid("Fcst", "pws34int", "SCALAR", intGrid34[i], tr)
            i += 1
        i=1
        while i < len(intGrid64):
            minT = minTime + (i-1)*21600
            maxTime = minT + 21600
            tr = TimeRange.TimeRange(AbsTime.AbsTime(minT), AbsTime.AbsTime(maxTime))
            self.createGrid("Fcst", "pws64int", "SCALAR", intGrid64[i], tr)
            i += 1
#
# THE FOLLOWING POPULATES THE INCREMENTAL PROBABILITIES (pwsD34, pwsD64, pwsN34, and pwsN64)
# FOR USE BY THE TROPICAL ZFP AND CWF, THE SO CALLED EXPRESSIONS OF UNCERTAINTY.
#
        timeRange = modelTR_inc[0]
        # print "TIME RANGE IS: ", timeRange

        # create grids from scratch that match the model inventory
        weNames = ["pwsD34", "pwsD64", "pwsN34", "pwsN64"]
        self.createFromScratchCmd(weNames, timeRange)

        for weName in weNames:
            trList = self.getWEInventory("Fcst", weName, "SFC", timeRange)
            for tr in trList:
                #print "weName TR IS: ", weName, tr
                self.makePWSScratch(weName, tr)

        for weName in weNames:
            trList = self.getWEInventory("Fcst", weName, "SFC", timeRange)
            for tr in trList:
                #print "weName TR IS: ", weName, tr
                probGrid = self.makePWSGrid(weName, tr)
                self.createGrid("Fcst", weName, "SCALAR", probGrid, tr)

        # Post the model time we used to the GFE status bar
        modelTimeStr = modelID[-13:]
        self.statusBarMsg(modelTimeStr + " used to make pws grids.", "R")
