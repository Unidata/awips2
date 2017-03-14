# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# StormSurgeThreat
#
# Author: Tom LeFebvre/Pablo Santos
# April 20, 2012 - To use gridded MSL TO NAVD and MSL to MLLW
# corrections and to get rid of Very Low.
# Last Modified: June 7, 2012 Shannon White - To fix the handling of time
# for A2 so it works for both real time and displaced real time 
# Migrated TC Coastal Flood for AWIPS2. Updated 6/22/2012.  S.O.
# March 11, 2014 to adapt to new PSURGE 2.0/PHISH and VDATUM Datasets in A1. PS
# May 21, 2014: for new PHISH but in AWIPS 2: PS/SW
# Aug 13, 2014: To rename SurgeHtPlustTide to InundationMax and incorporate InundationTiming. PS
# Sept 17, 2014: To finalize changes and clean up for 2015initial Baseline Check in.
# 
# Sept 18, 2014: Added code to pull grids from NHC via ISC if PHISH not
# Available on time. Left inactive (commented out) for the moment until that can be fully tested later
# in 2014 or in 2015.
# LeFebvre/Santos, July 27, 2015: Expanded Manual options to include Replace and Add options. 
# This allows sites to specify manually different threat levels across different edit areas and time ranges. 
# See 2015HTIUserGuide for details.
# 
# Feb 11, 2016 LeFebvre (16.1.2): Added code to create zero grids and manual grids when
# PSURGE not available. Added checks for current guidance for PHISH and ISC options.
#
# April 14, 2016: Lefebvre/Santos: Added multabledb to restore ISC option
#
# 6/20/2016 - Santos: Added code to fix issue of old grid not being deleted when running Manual/Add option.
# Last Modified: 
# 7/12/2016 DR19157 lshi: Add 3 foot option for Inundation Height for TCStormSurgeThreat values

# ----------------------------------------------------------------------------
# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards

MenuItems = ["Populate"]

import SmartScript
import numpy as np
import TimeRange
import AbsTime
import time
import sys

VariableList = [("DEFAULT: Typical. Should only be changed in coordination with NHC SS Unit", "", "label"),
                ("Forecast Confidence?", "Typical (10% Exceedance; for most systems anytime within 48 hours)",
##                 "radio", ["Low (Prob-only; 10% Exceedance; for ill behaved systems)",
                 "radio", ["Typical (10% Exceedance; for most systems anytime within 48 hours)",
                           "Medium (20% Exceedance; for well-behaved systems within 12 hours of event)",
                           "High (30% Exceedance; for well-behaved systems within 6-12 hours of event)",
                           "Higher (40% Exceedance; for well-behaved systems within 6 hours of the event)",
                           "Highest (50% Exceedance; for well-behaved systems at time of the event)"]),
                ("Grid Smoothing?", "Yes", "radio", ["Yes","No"]),
                ("Make grids from \nPHISH, ISC, or Manually?", "PHISH", "radio", ["PHISH", "ISC", "Manually Replace", "Manually Add"]),
                ("Manual Inundation settings:", "", "label"),
                ("Inundation Height:", 1.0, "scale", [0.0, 3.0], 1.0),
                ("Start Hour for Inundation Timing", 0, "scale", [0.0, 72.0], 6.0),
                ("End Hour for Inundation Timing", 6, "scale", [0.0, 78.0], 6.0),
                ]

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def getWEInventory(self, modelName, WEName, level):
        allTimes = TimeRange.allTimes().toJavaObj()
        gridInfo = self.getGridInfo(modelName, WEName, level, allTimes)
        trList = []
        for g in gridInfo:
            start = g.gridTime().startTime().unixTime()
            end = g.gridTime().endTime().unixTime()
            tr = TimeRange.TimeRange(AbsTime.AbsTime(start),
                                AbsTime.AbsTime(end))
            trList.append(tr)

        return trList

    def baseGuidanceTime(self):
        startTime = int((self._gmtime().unixTime() - (2 * 3600)) / (6 * 3600)) * (6 * 3600)
#         print "BASETIME IS: ", startTime

        return startTime


    def getAvgTopoGrid(self, topodb):

        siteID = self.getSiteID()
#        print "********************\n TOPO IS: ", topodb
        dbName = siteID + "_D2D_" + topodb

        weName = "avgTopo"
#        timeRange = TimeRange.allTimes().toJavaObj()
        trList = self.getWEInventory(dbName, weName, "SFC")
        
        #print "NED Topo list is", trList
        
        if len(trList)== 0:
            #print "CRAP!!!"
            return
        for tr in trList:
#            print "My time is", tr
            topoGrid = self.getGrids(dbName, weName, "SFC", tr, mode="First")
           

        # convert to feet
        topoGrid = topoGrid * 3.281
        #topoVal = topoGrid.copy()
        min = -16000
        max = 16000.0
        mask1 = topoGrid < min
        mask2 = topoGrid > max
        topoGrid[mask1] = -80
        topoGrid[mask2] = self.getTopo()[mask2]       
        return topoGrid

    def makeNewTimeRange(self, hours):

        cTime = int(self._gmtime().unixTime()/ 3600) * 3600
        startTime = AbsTime.AbsTime(cTime)
        endTime = startTime + (hours * 3600)
        timeRange = TimeRange.TimeRange(startTime, endTime)

        return timeRange

    def getModelIDList(self, matchStr):

        availParms = self.availableParms()

        modelList = []
        for pName, level, dbID in availParms:
            modelId = dbID.modelIdentifier()
            if modelId.find(matchStr) > -1:
                if modelId not in modelList:
                    modelList.append(modelId)
    
        return modelList

    def getExceedanceHeight(self, pctStr, level):
        
        ap = self.availableParms()
        dbName = self.getSiteID() + "_D2D_TPCSurgeProb"

        modelIDList = self.getModelIDList("TPCSurgeProb")
        modelIDList.sort()

        if len(modelIDList) == 0:
            self.statusBarMsg("No pSurge databases found in your inventory.", "S")
            return None

        surgeModel = modelIDList[-1]
        
        weName = "Surge" + pctStr + "Pct"        
        trList = self.getWEInventory(dbName, weName, level)
                
        if len(trList) == 0:
            self.statusBarMsg("No grids were found in the latest database " + str(surgeModel), "A")
            return None

        #baseTime = self.getBaseGuidanceTime() + (6 * 3600) # model data will be offset 6 hours
        baseTime = self.baseGuidanceTime() + (6 * 3600) # model data will be offset 6 hours
        
        if baseTime > trList[0].startTime().unixTime():
            self.statusBarMsg("TPCSurgeProb database is not current. Aborting", "A")
            return None

        #print "Retreiving ", weName, " at ", level
        for tr in trList:
            grid = self.getGrids(dbName, weName, level, tr, mode="Max")


        surgeVal = grid.copy()
        mask = surgeVal > -100
        grid = np.where(mask,surgeVal*3.28, np.float32(-80.0))

        return grid  # convert meters to feet 

    def makePhishGrid(self, pctStr, level, smoothThreatGrid, mutableID):

        siteID = self.getSiteID()
        dbName = siteID + "_D2D_TPCSurgeProb"

        weName = "Surge" + pctStr + "Pctincr"
        #print "Attempting to retrieve: ", weName, level
        # get the StormSurgeProb inventory
        surgeTRList = self.getWEInventory(dbName, weName, level)
        if len(surgeTRList) == 0:
            self.statusBarMsg("No PHISH grid found.", "U")
            return

        # Make timeRanges for all 13 grids. Start with the beginning of the first Phish grid
        baseTime = int(surgeTRList[0].startTime().unixTime() / (6 * 3600)) * (6 * 3600) #snap to 6 hour period
        trList = self.makeTimingTRs(baseTime)
        
        n = 1
        for tr in trList:
            start = tr.startTime().unixTime() - 6*3600
            if n == 1:
                starttimeghls = tr.startTime().unixTime() - 48*3600
                trdelete = TimeRange.TimeRange(AbsTime.AbsTime(starttimeghls - 100*3600),
                                               AbsTime.AbsTime(starttimeghls + 100*3600))
                self.deleteCmd(['InundationTiming'], trdelete)
                n = n + 1   
            end = tr.startTime().unixTime()
            tr6 = TimeRange.TimeRange(AbsTime.AbsTime(start),
                                      AbsTime.AbsTime(end))
            
            surgeTR = TimeRange.TimeRange(tr.startTime(), AbsTime.AbsTime(tr.startTime().unixTime() + 3600))
            if surgeTR in surgeTRList:
                phishGrid = self.getGrids(dbName, weName, level, surgeTR)
            else:
                phishGrid = np.zeros(self.getGridShape(), 'f')
                
#
# For consistency we need to add smoothing here too as we do in execute.
#
            if phishGrid is None:
                self.statusBarMsg("No PHISH grid available for:" + repr(tr), "S")
                continue       
            
            if smoothThreatGrid is "Yes":
                phishGrid = np.where(np.greater(phishGrid, 0.0), self.smoothGrid(phishGrid,3), phishGrid)

            grid = np.where(phishGrid>-100, phishGrid*3.28, np.float32(-80.0)) # Convert units from meters to feet
            self.createGrid(mutableID, "InundationTiming", "SCALAR", grid, tr6, precision=1)

        return

#**************************************************************************************
# THis procedure was written to extract MSL to NAVD corrections from the VDATUMS D2D
# Database. It is not yet implemented because the VDATUMS database has not been
# finalized.

    def getMSLtoNAVD(self):
        siteID = self.getSiteID()
        dbName = siteID + "_D2D_VDATUMS"

        weName = "MSLtoNAVD88"
        trList = self.getWEInventory(dbName, weName, "SFC")

        if len(trList) == 0:
            msgStr = weName + " does not exist in the VDATUMS model. "
            self.statusBarMsg(msgStr, "S")

        #maxGrid = self.empty()

        for tr in trList:
            grid = self.getGrids(dbName, weName, "SFC", tr, mode="First")
            #maxGrid = maximum(grid, -100.0)   # calculate the max as we go

        #maxGrid = where(greater(maxGrid,-100.0), maxGrid*3.28, maxGrid)
        conversionGrid = grid.copy()
        mask = conversionGrid>-0.40
        grid = np.where(mask, conversionGrid*3.28, np.float32(-80.0))

        #return maxGrid  # convert meters to feet
        return grid


# THis procedure was written to extract MSL to MLLW corrections from the VDATUMS D2D
# Database. It is not yet implemented because the VDATUMS database has not been
# finalized.

    def getMSLtoMLLW(self):
        siteID = self.getSiteID()
        dbName = siteID + "_D2D_VDATUMS"

        weName = "MSLtoMLLW"
        trList = self.getWEInventory(dbName, weName, "SFC")

#         if len(trList) == 0:
#             msgStr = weName + " does not exist in the VDATUMS model. "
#             self.statusBarMsg(msgStr, "S")

        #maxGrid = self.empty()

        for tr in trList:
            grid = self.getGrids(dbName, weName, "SFC", tr, mode="First")
            #maxGrid = maximum(grid, -100.0)   # calculate the max as we go

        #maxGrid = where(greater(maxGrid,-100.0),maxGrid*3.28, maxGrid)
        conversionGrid = grid.copy()
        mask = conversionGrid>0.0
        grid = np.where(mask,conversionGrid *3.28, np.float32(-80.0))

        #return maxGrid  # convert meters to feet
        return grid

    
# THis procedure was written to extract MSL to MHHW corrections from the VDATUMS D2D
# Database. It is not yet implemented because the VDATUMS database has not been
# finalized.

    def getMSLtoMHHW(self):
        siteID = self.getSiteID()
        dbName = siteID + "_D2D_VDATUMS"

        weName = "MSLtoMHHW"
        trList = self.getWEInventory(dbName, weName, "SFC")

        if len(trList) == 0:
            msgStr = weName + " does not exist in the VDATUMS model. "
            self.statusBarMsg(msgStr, "S")

        #maxGrid = self.empty()

        for tr in trList:
            grid = self.getGrids(dbName, weName, "SFC", tr, mode="First")
            #maxGrid = maximum(grid, -100.0)   # calculate the max as we go

        #maxGrid = where(greater(maxGrid,-100.0),maxGrid*3.28, maxGrid)
        conversionGrid = grid.copy()
        mask = conversionGrid>-3.09
        grid = np.where(mask, conversionGrid*3.28, np.float32(-80.0))

        #return maxGrid  # convert meters to feet
        return grid

# THis procedure was written to extract NAVD88 to MLLW corrections from the VDATUMS D2D
# Database. It is not yet implemented because the VDATUMS database has not been
# finalized.

    def getNAVDtoMLLW(self):
        siteID = self.getSiteID()
        dbName = siteID + "_D2D_VDATUMS"

        weName = "NAVD88toMLLW"
        trList = self.getWEInventory(dbName, weName, "SFC")

#         if len(trList) == 0:
#             msgStr = weName + " does not exist in the VDATUMS model. "
#             self.statusBarMsg(msgStr, "S")

        #maxGrid = self.empty()

        for tr in trList:
            grid = self.getGrids(dbName, weName, "SFC", tr, mode="First")
            #maxGrid = maximum(grid, -100.0)   # calculate the max as we go

        #maxGrid = where(greater(maxGrid,-100.0),maxGrid*3.28, maxGrid)
        conversionGrid=grid.copy()
        mask = conversionGrid>-2.20
        grid = np.where(mask, conversionGrid*3.28, np.float32(-80.0))

        #return maxGrid  # convert meters to feet
        return grid


# THis procedure was written to extract NAVD88 to MLLW corrections from the VDATUMS D2D
# Database. It is not yet implemented because the VDATUMS database has not been
# finalized.

    def getNAVDtoMHHW(self):
        siteID = self.getSiteID()
        dbName = siteID + "_D2D_VDATUMS"

        weName = "NAVD88toMHHW"
        trList = self.getWEInventory(dbName, weName, "SFC")

#         if len(trList) == 0:
#             msgStr = weName + " does not exist in the VDATUMS model. "
#             self.statusBarMsg(msgStr, "S")

        #maxGrid = self.empty()

        for tr in trList:
            grid = self.getGrids(dbName, weName, "SFC", tr, mode="First")
            #maxGrid = maximum(grid, -100.0)   # calculate the max as we go

        #maxGrid = where(greater(maxGrid,-100.0),maxGrid*3.28, maxGrid)
        conversionGrid = grid.copy()
        mask = conversionGrid>-3.40
        grid = np.where(mask, conversionGrid*3.28, np.float32(-80.0))

        #return maxGrid  # convert meters to feet
        return grid

    def smoothGrid(self, grid, factor):
        # factors of less than 3 are useless or dangerous
        if factor < 3:
            return grid

        half = int(factor)/ 2
        sg = np.zeros(grid.shape,"f8")
        count = np.zeros(grid.shape,"f8")
        gridOfOnes = np.ones(grid.shape,"f8")
        for y in range(-half, half + 1):
            for x in range(-half, half + 1):
                if y < 0:
                    yTargetSlice = slice(-y, None, None)
                    ySrcSlice = slice(0, y, None)
                if y == 0:
                    yTargetSlice = slice(0, None, None)
                    ySrcSlice = slice(0, None, None)
                if y > 0:
                    yTargetSlice = slice(0, -y, None)
                    ySrcSlice = slice(y, None, None)
                if x < 0:
                    xTargetSlice = slice(-x, None, None)
                    xSrcSlice = slice(0, x, None)
                if x == 0:
                    xTargetSlice = slice(0, None, None)
                    xSrcSlice = slice(0, None, None)
                if x > 0:
                    xTargetSlice = slice(0, -x, None)
                    xSrcSlice = slice(x, None, None)

                target = [yTargetSlice, xTargetSlice]
                src = [ySrcSlice, xSrcSlice]
                sg[target] = np.where(np.greater(grid[src],-80.0),sg[target] + grid[src],sg[target])
                count[target] = np.where(np.greater(grid[src],-80.0),count[target] + gridOfOnes[src],count[target])

        return np.where(np.greater(count,0.0), sg / count, np.float32(-80.0))
    
    
    # Copies the specified weather elements in elementList into the Fcst database.
    def copyISCGridstoFcst(self, elementList, mutableID):
        
        # Initialize all the grids we plan to return
        
        surgePctGrid = None
        surgePctGridMSL = None
        surgePctGridMLLW = None
        surgePctGridMHHW = None
        surgePctGridNAVD = None
        
        baseTime = self.baseGuidanceTime()
        
        for weName in elementList:
            iscWeName = weName + "nc"
            # get the inventory for the ISC grids
            try:
                trList = self.getWEInventory("ISC", iscWeName, "SFC")
            except:
                self.statusBarMsg("No grids found in ISC database for " + iscWeName, "S")
                return None, None, None, None, None
            
            if len(trList) == 0:
                self.statusBarMsg("No grids found in ISC database for " + iscWeName, "S")
                return None, None, None, None, None
            
            # Make sure that the ISC grids are current
            if baseTime > trList[0].startTime().unixTime():
            #if trList[0].startTime().unixTime() != baseTime:
                self.statusBarMsg("ISC grids for element " + weName + " are not current. Aborting.", "S")
                return None, None, None, None, None

        # If we made it this far, delete the existing grids so there's no confusion
            if weName == "InundationTiming":
                timeRange = TimeRange.allTimes()
                self.deleteCmd(["InundationTiming"], timeRange)   
   
            # Fetch the ISC grid and create the same grid in the Fcst database
            for tr in trList:
                grid = self.getGrids("ISC", iscWeName, "SFC", tr)
                if iscWeName == "InundationTimingnc":
                    self.createGrid(mutableID, weName, "SCALAR", grid, tr, precision=2) 
                elif iscWeName == "InundationMaxnc":
                    surgePctGrid = grid
                elif iscWeName == "SurgeHtPlusTideMSLnc":
                    surgePctGridMSL = grid
                elif iscWeName == "SurgeHtPlusTideMLLWnc":
                    surgePctGridMLLW = grid
                elif iscWeName == "SurgeHtPlusTideMHHWnc":
                    surgePctGridMHHW = grid     
                elif iscWeName == "SurgeHtPlusTideNAVDnc":
                    surgePctGridNAVD = grid      

        return surgePctGrid,surgePctGridMSL,surgePctGridMLLW,surgePctGridMHHW,surgePctGridNAVD
    
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
        gridList = []
        trList = self.makeTimingTRs(baseTime)
        
        for tr in trList:
            timingGrid = self.empty()
            gridList.append(timingGrid)
            
        return trList, gridList
    
    def execute(self, varDict, editArea):
        
        mutableID = self.mutableID()
        
        # List of elements       
        # See if we should copy from ISC. If so, do the copy and exit
        smoothThreatGrid = varDict["Grid Smoothing?"]
        makeOption = varDict["Make grids from \nPHISH, ISC, or Manually?"] 
        topodb = "NED"
        #topodb = varDict["Topographic Database?"]

        stormSurgeEditArea = self.getEditArea("StormSurgeWW_EditArea")
        ssea = self.encodeEditArea(stormSurgeEditArea)

        Topo = self.getAvgTopoGrid(topodb)

        confidenceStr = varDict["Forecast Confidence?"]
             
        # extract the percent value from this string
        pctPos = confidenceStr.find("%")
        pctStr = confidenceStr[pctPos - 2:pctPos]
        
        threatWEName = "StormSurgeThreat"

        #print "pctStr is: ", pctStr

        if makeOption == "PHISH":
           
            # Now get the psurge
            surgePctGrid = self.getExceedanceHeight(pctStr, "FHAG0")
            surgePctGridNAVD = self.getExceedanceHeight(pctStr, "SFC")
            
            if surgePctGrid is None or surgePctGridNAVD is None:
                return
            
            #print "retrieved my grids"   
#
# The following lines are the gridded vdatum corrections.
#
            msltonavd = self.getMSLtoNAVD()
            msltomllw = self.getMSLtoMLLW()
            msltomhhw = self.getMSLtoMHHW()
            navdtomllw = self.getNAVDtoMLLW()
            navdtomhhw = self.getNAVDtoMHHW()  

# Apply 3x3 smooth within the surge zone
# for values greater than 1 as to not underplay areas adjacent to zero value pixels.       
# If you apply a smoother, for consistency among storm surge plus tide and derived
# grids, it must be done here.

            if smoothThreatGrid is "Yes":
                surgePctGrid = np.where(np.greater(surgePctGrid, 0.0), self.smoothGrid(surgePctGrid,3), surgePctGrid)
                surgePctGridNAVD = np.where(np.greater(surgePctGridNAVD, -10.0), self.smoothGrid(surgePctGridNAVD,3), surgePctGridNAVD)
        
            mask1 = np.logical_and(np.greater(msltonavd, -80.0),np.greater(surgePctGridNAVD,-80.0))
            surgePctGridMSL= np.where(mask1, surgePctGridNAVD - msltonavd, np.float32(-80.0)) # MSL Grid     
            surgePctGridMLLW = np.where(np.greater(navdtomllw,-80.0) & np.greater(surgePctGridNAVD,-80.0), \
                                        surgePctGridNAVD + navdtomllw, np.float32(-80.0)) # MLLW Grid
            surgePctGridMHHW = np.where(np.greater(navdtomhhw,-80.0) & np.greater(surgePctGridNAVD,-80.0), \
                                        surgePctGridNAVD + navdtomhhw, np.float32(-80.0)) # MHHW Grid
            surgeDiffMLLWMHHW = np.where(np.greater(surgePctGridMLLW,-80.0) & np.greater(surgePctGridMHHW, -80.0), \
                                         surgePctGridMLLW-surgePctGridMHHW, np.float32(-80.0)) # Diff Grid Between MLLW and MHHW   

            self.makePhishGrid(pctStr, "FHAG0", smoothThreatGrid, mutableID) 

        elif makeOption == "ISC":

            elementList = ["InundationMax","InundationTiming", "SurgeHtPlusTideMSL","SurgeHtPlusTideMLLW",
                           "SurgeHtPlusTideNAVD","SurgeHtPlusTideMHHW"]
            surgePctGrid,surgePctGridMSL,surgePctGridMLLW,surgePctGridMHHW,surgePctGridNAVD = self.copyISCGridstoFcst(elementList, mutableID)
            if surgePctGrid is None or surgePctGridMSL is None or surgePctGridMLLW is None or \
               surgePctGridMHHW is None or surgePctGridNAVD is None:
                return

        elif makeOption == "Manually Replace" or makeOption == "Manually Add":
            
            inundationHeight = float(varDict["Inundation Height:"])
            inunStartHour = float(varDict["Start Hour for Inundation Timing"])
            inunEndHour = float(varDict["End Hour for Inundation Timing"]) 

            selectedMask = self.encodeEditArea(editArea)
            if not selectedMask.any():
                self.statusBarMsg("Please define an area over which to assign the inundation values.", "S")
                return

            modifyMask = selectedMask & ssea
            if not modifyMask.any():
                self.statusBarMsg("Please define an area that intersects the StormSurgeEditArea to assign the inundation values.", "S")
                return              # Calculate the intersection of the SSEditArea and selected editAre

            if inunStartHour >= inunEndHour:
                self.statusBarMsg("Please define the end hour after the start hour.", "S")
                return

            surgePctGrid = self.empty()

        # Fetch the old grids if we're adding
            if varDict["Make grids from \nPHISH, ISC, or Manually?"] == "Manually Add":
                imTRList = self.getWEInventory(mutableID, "InundationMax", "SFC")
                print "InnundationTFList:", imTRList
                if len(imTRList) > 0:
                    print "got pctSurgegrid"
                    imTR = imTRList[0]
                    surgePctGrid = self.getGrids(mutableID, "InundationMax", "SFC", imTR)
       
            surgePctGrid[modifyMask] = inundationHeight
                 
            # Make the timing grids
            baseTime = self.baseGuidanceTime()
#            trList = self.makeTimingTRs(baseTime)
            trList, timingGrids = self.getTimingGrids()
#             print "TRLIST IS: ", trList
            
            for i in range(len(trList)):
                # only modify grid in the specified time range
                start = trList[i].startTime().unixTime()
                end = trList[i].endTime().unixTime()                        
                
                if (start - baseTime) / 3600 >= inunStartHour and (end - baseTime) / 3600 <= inunEndHour:                
                    timingGrids[i] = surgePctGrid # populate only where needed
                
            timeRange = TimeRange.allTimes()
            self.deleteCmd(["InundationTiming"], timeRange)                #self.createGrid(mutableID, "InundationTiming", "SCALAR", timingGrids[i], trList[i])            
            for i in range(len(trList)):
                self.createGrid(mutableID, "InundationTiming", "SCALAR", timingGrids[i], trList[i])
            
        #threatWEName = "StormSurgeThreat"
         
        threatKeys = self.getDiscreteKeys(threatWEName)
 
        # Define a mapping between UI names and key names
        # keyMap = {"Very Low" :"Very Low",
        keyMap = {"Elevated" : "Elevated", 
                  "Moderate" : "Mod",
                  "High" : "High",
                  "Extreme" : "Extreme",
                  }
         
        threshDict = {}  # a dict to store thresholds from the UI
 
        for key in keyMap.keys():
 
            if   keyMap[key] == "Extreme":
                threshDict[keyMap[key]] = 9
            elif keyMap[key] == "High":
                threshDict[keyMap[key]] = 6
            elif keyMap[key] == "Mod":
                threshDict[keyMap[key]] = 3
            elif keyMap[key] == "Elevated":
                threshDict[keyMap[key]] = 1
                             
            #print "threshDict[keyMap[key]]: ", keyMap[key], threshDict[keyMap[key]]
 
        # make a timeRange - 6 hours long
        elementList = ["StormSurgeThreat","InundationMax","SurgeHtPlusTideMSL","SurgeHtPlusTideMLLW",
                       "SurgeHtPlusTideNAVD","SurgeHtPlusTideMHHW"]

        # make a new timeRange that will be used to create new grids
        timeRange = self.makeNewTimeRange(6)
        
        # Remove old guidance grids and replace them with the new grids          
        # Delete the old grids first
        cTime = int(self._gmtime().unixTime()/ 3600) * 3600
        startTime = AbsTime.AbsTime(cTime - 48*3600)
        endTime = startTime + 240*3600
        deleteTimeRange = TimeRange.TimeRange(startTime, endTime)
               
        for elem in elementList:
            self.deleteCmd([elem], deleteTimeRange)         

        if makeOption != "Manually Replace" and makeOption != "Manually Add":         
            self.createGrid(mutableID, "SurgeHtPlusTideMSL", "SCALAR", surgePctGridMSL,
                            timeRange, precision=2)
            self.createGrid(mutableID, "SurgeHtPlusTideMLLW", "SCALAR", surgePctGridMLLW,
                            timeRange, precision=2)
            self.createGrid(mutableID, "SurgeHtPlusTideNAVD", "SCALAR", surgePctGridNAVD,
                            timeRange, precision=2)
            self.createGrid(mutableID, "SurgeHtPlusTideMHHW", "SCALAR", surgePctGridMHHW,
                            timeRange, precision=2)

        # Make the grid. Start with the existing grid if we have one otherwise zeros
        coastalThreat = self.empty(np.int8)
        
        self.createGrid(mutableID, "InundationMax", "SCALAR", surgePctGrid, timeRange, precision=2)          
 
        # Yet another list to define the order in which we set grid values
        # This order must be ranked lowest to highest
        #keyList = ["Very Low", "Elevated", "Mod", "High", "Extreme"]
        keyList = ["Elevated", "Mod", "High", "Extreme"]
 
        # Set the grid values based on the surgePctGrid grid and thresholds
        for key in keyList:
            #print "THRESHOLD FOR KEY IS: ", key, threshDict[key]
            thresh = threshDict[key]
            keyIndex = self.getIndex(key, threatKeys)
            coastalMask = ssea & np.greater_equal(surgePctGrid, thresh)
            coastalThreat[coastalMask] = keyIndex
 
#       create the CoastalThreat Grid
        self.createGrid(mutableID, threatWEName, "DISCRETE",
                        (coastalThreat, threatKeys), timeRange,
                        discreteKeys=threatKeys,
                        discreteOverlap=0,
                        discreteAuxDataLength=2,
                        defaultColorTable="Hazards")

        return
