# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# PopulateWithERP.py
#
# Author: lefebvre
# This procedure copies the latest version of the hi res QPF grids which is the tpHPCndfd parameter
# from WPC grid found in the D2D HPCERP GFE model db. These grids are transmitted by WPC 4 times a day.
# The 00Z and 12Z cycles of data go out to day 7. 06Z and 18Z only go out 72 hours. The procedure
# makes an inventory check and populates the QPF grids using the latest HPCERP tpHPCndfd or a combination 
# of the latest HPCERP db and the previous one if the latest one is not completely in the system yet. 
# When the QPF grids are populated from combination of the latest model cycle and the previous model cycle the 
# tool will pop a banner telling you so. Also. the tool copies the guidance into 6 hours QPF grids 
# regardless whether the time constraint of your QPF grids is 1, 3, or 6 hours.
#
# If a time range is preselected in the Grid Manager, the tool only runs for that time range. Otherwise if no 
# time range is preselected the tool runs from the current 6 hours period out to 7 days using the latest
# guidance available as far out as it is available at the time the tool is ran.
#
# The tpHPCndfd parameter in the HPCERP D2D model db in GFE is the high resolution 2.5 km WPC QPF guidance.
# As of AWIPS build 16.1.2 and 16.2.1 this parameter is equivalent to accessing the following guidance from
# the D2D Volume Browser:
# 
# 1. From Sources -> SfcGrid -> HPC
# 2. From Fields -> Precip -> 6hr Precip Accum 
# 3. From Planes -> Misc -> Sfc
# 
# Contributor: P. Santos

# Version 2.2 - 27 March 2016
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Edit"]
import LogStream, time
from math import *
import AbsTime, TimeRange

VariableList = []

import time
import AbsTime
import SmartScript


class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        
        
    # Returns the duration for the QPF parm in hours
    def getQPFGridDuration(self):
        parm = self.getParm("Fcst", "QPF", "SFC")
        
        parmDuration = parm.getGridInfo().getTimeConstraints().getDuration()

        return parmDuration / 3600
        
    # Fetch the inventory as a list of timeRanges for the specified model
    # and element. Trim the list to the specified timeRange, if any. 
    def getWEInventory(self, modelName, WEName, timeRange=None):

        allTimes = TimeRange.allTimes()
        
        if timeRange is None:
            timeRange = allTimes

        trList = []
        # getGridInfo will just die if the modelName or weName is not valid
        # so wrap it in a try block and return [] if it fails
        try:
            gridInfo = self.getGridInfo(modelName, WEName, "SFC", timeRange)
        except:
            return trList

        for g in gridInfo:
            if timeRange.overlaps(g.gridTime()):
                trList.append(g.gridTime())

        return trList
    
    # Returns a list of model names matching the specified model name,
    # weather element name and level
    def getModelList(self, modelName, weName, weLevel):
        modelList = []

        availParms = self.availableParms()

        for pName, level, dbID in availParms:
            if dbID.modelName().find(modelName) > -1:
                if pName.find(weName) > -1:
                    if level.find(weLevel) > -1:
                        if dbID.modelIdentifier() not in modelList:   
#                             modelList.append(dbID.modelIdentifier())
                            modelList.append(dbID)
        return modelList
    
    # Fetch the ERP grids at the times specified by the gridTRList. Always fetches the latest model that
    # contains the requested grid.
    def getERPGrids(self, gridTRList):
        # get the list of all available models. They come sorted latest to oldest. 
        modelList = self.getModelList(self._ERPModelName, self._ERPVarName, self._ERPLevel)
        modelList = modelList[0:2]   # trim the list to the last two versions
               
        # make a dict and fill with grids with the default value.
        # These will be replaced when we find real grids
        gridDict = {}
        usedPreviousModel = False
        
        for gridTR in gridTRList:
            gridDict[gridTR] = None  # initialize to None
            for model in modelList:
                modelTRList = self.getWEInventory(model, self._ERPVarName)
                for modelTR in modelTRList:
                    if modelTR.overlaps(gridTR): # Find the grid that overlaps
                        grid = self.getGrids(model, self._ERPVarName, "SFC", modelTR)
                        if grid is None:
                            break
                        gridDict[gridTR] = grid
                        
                        # See if we used a previous model version
                        if model != modelList[0]:
                            usedPreviousModel = True
                        break # found a grid, time to stop
                    
                if gridDict[gridTR] is not None: # if we found a grid, break from the model loop
                    break
            
        if usedPreviousModel:
            self.statusBarMsg("Some grids were populated with a previous version of " + self._ERPModelName, "S")    
        
        return gridDict
    
    # Make a list of timeRanges rounded and synched to the 6 hour synoptic times.
    def makeTimeRangeList(self, selectedTimeRange):
        
        sixHrs = 6 * 3600
        days = 7
        now = int(time.time() / sixHrs) * (sixHrs)
        trList = []
        for t in range(now, now + (days * 24 * 3600), sixHrs):
            start = AbsTime.AbsTime(t)
            end = AbsTime.AbsTime(t + sixHrs)
            tr = TimeRange.TimeRange(start, end)
            if tr.overlaps(selectedTimeRange) or not selectedTimeRange.isValid():
                trList.append(tr)
            
        return trList

    
    def execute(self, editArea, timeRange, varDict):
        
        
        self._ERPModelName = "HPCERP"
        self._ERPVarName = "tpHPCndfd"
        self._ERPLevel = "SFC"
        
        qpfDuration = self.getQPFGridDuration()
        
        if qpfDuration not in [1, 3, 6]:
            self.statusBarMsg("Your QPF grid duration is not compatible with WPC QPF durations.", "S")
            return
        
        modelList = self.getModelList(self._ERPModelName, self._ERPVarName, self._ERPLevel)
        if len(modelList) == 0:
            self.statusBarMsg("No HPCERP models found to populate. Game over.", "S")
            return

        # Fetch the latest model and check its inventory            
#         latestERPModel = modelList[0] # Fetch the latest model and check its inventory

        # Data checks - if any one fails, the tool aborts

#         # If not at least 72 hours, determined by the first and last available grid, bail out.
#         modelTRList = self.getWEInventory(latestERPModel, self._ERPVarName)
#         if len(modelTRList) == 0:
#             self.statusBarMsg("The latest HPCERP model has no grids yet.", "S")
#             return
        
#         # Make sure the first grid starts near the model time     
#         firstGridTime = int(modelTRList[0].startTime().unixTime() / (3600 * 6)) * (3600* 6)
#         if firstGridTime != latestERPModel.modelTime().unixTime():
#             self.statusBarMsg("The latest HPCERP model has not completely arrived (First Grid too late).", "S")
#             return
        
#         # Make sure there's enough grids in the latest model 
#         lastGridTime = int(modelTRList[-1].startTime().unixTime() / (3600 * 6)) * (3600* 6)
#         if (lastGridTime - firstGridTime) / 3600 < 72:
#             self.statusBarMsg("The latest HPCERP model has not completely arrived.(not enough grids", "S")
#             return
               
        # Make a list of TimeRanges that we will use to populate the QPF. Trim to the selected timeRange
        trList = self.makeTimeRangeList(timeRange)
        erpGridDict = self.getERPGrids(trList)

        erpTRList = erpGridDict.keys()
        
        # Make sure all the grids are there, or bail
        for erpTR in erpTRList:
            if erpGridDict[erpTR] is None:
                self.statusBarMsg("Missing guidance at " + str(erpTR) + " Aborting.", "S")
                return
        
        for tr in trList:
            for erpTR in erpTRList:
                if erpTR.overlaps(tr):
                    if erpGridDict[erpTR] is None:
                        break
                    grid = erpGridDict[erpTR] / 25.4 # convert mm to inches
                    self.createGrid("Fcst", "QPF", "SCALAR", grid, tr)
                    
        return

            
