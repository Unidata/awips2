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
#                July 18, 2017 - Added option to select Preliminary of Official probablistic model source.  -Tom
#                Feb 28, 2020 - Added Time of Arrival and Departure Paramters Code - PS
#                Feb 1, 2021 - Modified to produce a time range in the output TOATOD elements that does not vary. (DR22453)
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Populate"]

import SmartScript
import AbsTime
import TimeRange
import LogStream

import ProcessVariableList
from numpy import *

EASTERN_PACIFIC_SITES = ["LOX","SGX"]
EASTERN_PACIFIC_BINS = ["EP1","EP2","EP3","EP4","EP5"]
EASTERN_CENTRAL_PACIFIC_SITES = ["NH1","HFO","HPA","ONP"]
EASTERN_CENTRAL_PACIFIC_BINS= EASTERN_PACIFIC_BINS + ["CP1","CP2","CP3","CP4","CP5"]
WESTERN_PACIFIC_SITES = ["GUM","PQE","PQW"]
WESTERN_PACIFIC_BINS = ["PQ1","PQ2","PQ3","PQ4","PQ5"]
ATLANTIC_GULF_BINS = ["AT1","AT2","AT3","AT4","AT5"]

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # caluclates maximum value at each grid point
    def maxGrid(self, gridList):

        if len(gridList) < 1:
            return None

        gridMax = maximum.reduce(gridList)
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

        # Use the model source selected from the GUI        
        modelIDList = self.getModelIDList(self._probWindModelSource)
        modelIDList.sort()

        if len(modelIDList) == 0:
            self.statusBarMsg("No PWS models found in your inventory.", "S")
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
            if "toa10" not in param:
                self.statusBarMsg("No grids " + param + " available for model:" + modelID, "S")
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

        if len(pwsInv) > 1:
            LogStream.logUse("pwsInv is GOOD")
        else:
            LogStream.logUse("pwsInv is BAD DELETING")
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
        prob1 = self.getGrids(modelName, probWE, modelLevel, probInv[0],
                             mode="First")
        prob2 = self.getGrids(modelName, probWE, modelLevel, probInv[1],
                             mode="First")

        # Calculate the grid        
        grid = pws1 + prob2 - prob1

        # clip the grid at 100.0 percent
        grid = clip(grid, 0.0, 100.0)
        
        return grid

    def MakeTOAandTODGrids(self, modelID, weNamesModelTOA, modelTR_toa):

        timeRangeTOA = modelTR_toa[0]
        modelEndTime = modelTR_toa[1]
        TOAMaxTime = modelTR_toa[2]    
        modelStartTime = modelEndTime - 120*3600
        modelTimeRange = TimeRange.TimeRange(AbsTime.AbsTime(modelStartTime), AbsTime.AbsTime(modelEndTime))

        weNameDict = {"toa1034" : "TOA1034",
                      "toa5034" : "TOA5034",
                      "tod5034" : "TOD5034",
                      "tod9034" : "TOD9034",
                      "toa1050" : "TOA1050",
                      "toa5050" : "TOA5050",
                      "tod5050" : "TOD5050",
                      "tod9050" : "TOD9050",
                      "toa1064" : "TOA1064",
                      "toa5064" : "TOA5064",
                      "tod5064" : "TOD5064",
                      "tod9064" : "TOD9064",
                      }

        for weName in weNamesModelTOA: 

            for name, grid in weNameDict.items():
                if name in weName:
                    fgrid = grid   

            gridTOATOD = self.getGrids(modelID, weName, "FHAG10", timeRangeTOA, noDataError=0)
            tr1=modelStartTime
            if gridTOATOD is not None:
                for i in range(1,21):
                    grid = self.newGrid(0.0)
                    tr2 = tr1 + 6*3600
                    if "toa" in weName:
                        mask = (gridTOATOD >= tr1) & (gridTOATOD < tr2)
                    else:
                        mask = (gridTOATOD > tr1) & (gridTOATOD <= tr2)
                    if mask.any():
                        grid[mask] = 1.0
                    tr = TimeRange.TimeRange(AbsTime.AbsTime(tr1), AbsTime.AbsTime(tr2))
                    #print "TIME RANGE LIST ELEMENT IS: ", tr
                    self.createGrid("Fcst", fgrid, "SCALAR", grid, tr, minAllowedValue=0.0, maxAllowedValue=1.0)
                    tr1 = tr2


    # main method
    def execute(self, varDict):
        GFEDomainname = self.getSiteID()
        #print "Domain is: ", GFEDomainname
        if GFEDomainname in EASTERN_CENTRAL_PACIFIC_SITES: # Site is TAFB EPAC/HFO/OPC
            buttonList = EASTERN_CENTRAL_PACIFIC_BINS
        elif GFEDomainname in WESTERN_PACIFIC_SITES:
            buttonList = WESTERN_PACIFIC_BINS
        elif GFEDomainname in EASTERN_PACIFIC_SITES:
            buttonList = EASTERN_PACIFIC_BINS
        else: # Every other site in the Atlantic CONUS from BRO to CAR. Will not work with Pacific or West Coast.
            buttonList = ATLANTIC_GULF_BINS

        if varDict is None:  # This means the tool is being run interactively, so make the GUI.
            VariableList = [("Probabilistic Wind Source?", "Preliminary", "radio", ["Official", "Preliminary"]),
                ("Storm? This is the Bin Number for your Arrival/Departure Time Parameters:", [], "radio", buttonList),
                ]
            varDict = {}
            processVarList = ProcessVariableList.ProcessVariableList("TPCWindProb", VariableList, varDict, None)
            status = processVarList.status()
            if status != "OK":
                return
    
        # Fetch the model source and define the model name
        sourceDB = varDict["Probabilistic Wind Source?"]
        if sourceDB == "Official":
            self._probWindModelSource = "TPCProb"
        elif sourceDB == "Preliminary":
            self._probWindModelSource = "TPCProbPrelim"
        else:
            self.statusBarMsg("Unknown model source selected. Aborting.", "U")
            return
        
        
        modelID = self.getLatestPWSModel()
        if modelID == "":
            self.statusBarMsg("The selected model source was not found. Aborting.", "U")
            return
        
        storm = varDict["Storm? This is the Bin Number for your Arrival/Departure Time Parameters:"]     
        if not storm:
            self.statusBarMsg("Please Select the Storm Bin Number for your Time of Arrival/Departure Parameters. Aborting", "U")
            return
          
        stormNameDict = {"PQ1" : "WP1",
                         "PQ2" : "WP2",
                         "PQ3" : "WP3",
                         "PQ4" : "WP4",
                         "PQ5" : "WP5",
                      }  
        
        for name, basin in stormNameDict.items():
            if storm in name:
                storm = basin
        
        #print("modelID:", len(modelID))
        weNames = ["prob34", "prob64", "pws34int", "pws64int"]
        modelTR_cum = self.getModelTimeRange(modelID, "prob34", weNames)
        weNames = ["pwsD34", "pwsD64", "pwsN34", "pwsN64"]        
        modelTR_inc = self.getModelTimeRange(modelID, "pws34", weNames)
        if modelTR_cum is None or len(modelTR_cum) < 3 or modelTR_inc is None or len(modelTR_inc) < 3:
            return 
        
        binNum = storm.lower()      
        binNum = "kt" + binNum
        weNamesTOA = ["TOA1034","TOA1050","TOA1064","TOA5034","TOA5050","TOA5064","TOD5034","TOD5050","TOD5064","TOD9034","TOD9050","TOD9064"]
        modelTR_toa = self.getModelTimeRange(modelID, "toa1034"+binNum, weNamesTOA)
        modelTR_toa50 = self.getModelTimeRange(modelID, "toa1050"+binNum, weNamesTOA)
        modelTR_toa64 = self.getModelTimeRange(modelID, "toa1064"+binNum, weNamesTOA)
        weNamesModelTOA = ["{}{}".format(name.lower(), binNum) for name in weNamesTOA]

        if modelTR_toa is not None:
            self.MakeTOAandTODGrids(modelID, weNamesModelTOA, modelTR_toa)
        else:
            self.statusBarMsg("No TOA/TOD grids for this cycle. You are only getting the regular probability grids", "S")
        
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

