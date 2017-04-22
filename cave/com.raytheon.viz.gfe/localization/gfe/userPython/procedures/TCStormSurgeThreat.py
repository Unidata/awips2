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
# Feb 11, 2016 LeFebvre (16.2.1): Added code to create zero grids and manual grids when
# PSURGE not available. Added checks for current guidance for PHISH and ISC options.
#
# April 14, 2016: Lefebvre/Santos: Added multabledb to restore ISC option
#
# Last Modified: 
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
# 01/09/2017: Renamed UpdateInunMax in GUI for clarity. Also, introduced on Jan 2017 SWiT ability for procedure to force InundationMax that are > 1 and < 1.5 to 1.5. 
# This is because TCV rounds to nearest one foot for categorical HTI threat level consistency with inundation graphic. Not doing this would cause TCV to throw away zones that
# might have more than 3% coverage of inundation > 1 but less than 1.5 altogether. Changing TCV to key on anything with InundationMax >= 1 would not
# do because it would then include zones in TCV with inundation forecasts of less than 1 but >= 0.5 overdoing the threat.
#  ----------------------------------------------------------------------------
# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards

MenuItems = ["Populate"]

import TropicalUtility, LogStream
import SmartScript
import numpy as np
import TimeRange
import AbsTime
import time
import sys

VariableList = [("DEFAULT: Typical. Should only be changed in coordination with NHC SS Unit", "", "label"),
                ("Forecast Confidence? - (Applies to PHISH Only)", "Typical (10% Exceedance; for most systems anytime within 48 hours)",
##                 "radio", ["Low (Prob-only; 10% Exceedance; for ill behaved systems)",
                 "radio", ["Typical (10% Exceedance; for most systems anytime within 48 hours)",
                           "Medium (20% Exceedance; for well-behaved systems within 12 hours of event)",
                           "High (30% Exceedance; for well-behaved systems within 6-12 hours of event)",
                           "Higher (40% Exceedance; for well-behaved systems within 6 hours of the event)",
                           "Highest (50% Exceedance; for well-behaved systems at time of the event)"]),
                ("Grid Smoothing?", "Yes", "radio", ["Yes","No"]),
                ("Make grids from \nPHISH, ISC, or Manually?", "PHISH", "radio", ["PHISH", "ISC", "Manually Replace", "Manually Add", "UpdateInunMax (Edit Inundation Timing Grids)"]),
#                ("Make grids from \nPHISH, PETSS, ISC, or Manually?", "PHISH", "radio", ["PHISH", "PETSS", "ISC", "Manually Replace", "Manually Add", "UpdateInunMax (Edit Inundation Timing Grids)"]),
                ("Manual Inundation settings: Time ranges below relative to advisory model cycle", "", "label"),
                ("Inundation Height:", 1.0, "scale", [0.0, 3.0], 0.1),
                ("Start Hour for Inundation Timing", 0, "scale", [0.0, 72.0], 6.0),
                ("End Hour for Inundation Timing", 6, "scale", [0.0, 78.0], 6.0),
                ]

class Procedure (TropicalUtility.TropicalUtility):
    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)

    #  Compute a base time for this guidance
    def baseGuidanceTime(self):
        startTime = int((self._gmtime().unixTime() - (2 * 3600)) / (6 * 3600)) * (6 * 3600)
        return startTime

    #  Method to get the average topography for each grid point
    def getAvgTopoGrid(self, topodb):

        siteID = self.getSiteID()
#        print "********************\n TOPO IS: ", topodb
        dbName = siteID + "_D2D_" + topodb

        weName = "avgTopo"
#        timeRange = TimeRange.allTimes().toJavaObj()
        trList = self.GM_getWEInventory(weName, dbName, "SFC")
        
        #print "NED Topo list is", trList
        
        if len(trList)== 0:
            #print "CRAP!!!"
            return
        for tr in trList:
#            print "My time is", tr
            topoGrid = self.getGrids(dbName, weName, "SFC", tr, mode="First")
        
        #  Convert topography from meters to feet
        topoGrid /= 0.3048
        min = -16000
        max = 16000.0
        mask2 = (topoGrid > max)
        topoGrid[topoGrid < min] = -80
        topoGrid[mask2] = self.getTopo()[mask2]       
        return topoGrid

    #  Make a time range of x hours duration from the current time
    def makeNewTimeRange(self, hours):

        cTime = int(self._gmtime().unixTime()/ 3600) * 3600
        startTime = AbsTime.AbsTime(cTime)
        endTime = startTime + (hours * 3600)
        timeRange = TimeRange.TimeRange(startTime, endTime)

        return timeRange

    #  Method to find all database versions for the specified model
    def getModelIDList(self, matchStr):

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

    #  Method to get the selected exceedance height data 
    def getExceedanceHeight(self, modelName, pctStr, level):
        
        ap = self.availableParms()
        dbName = self.getSiteID() + "_D2D_" + modelName

        modelIDList = self.getModelIDList(modelName)
        modelIDList.sort()

        if len(modelIDList) == 0:
            return None

        surgeModel = modelIDList[-1]
        
        weName = "Surge" + pctStr + "Pct"        
        trList = self.GM_getWEInventory(weName, dbName, level)
                
        if len(trList) == 0:  # No grids found for this database
            return None

        baseTime = self.baseGuidanceTime() + (6 * 3600) # model data will be offset 6 hours
        
        if baseTime > trList[0].startTime().unixTime():
            #modelCycle = AbsTime.AbsTime(self.baseGuidanceTime() - (6*3600))
            message = "BE CAREFUL: " + modelName + " IS STILL FROM A PREVIOUS ADVISORY/MODEL CYCLE"        
            self.statusBarMsg(message, "A")
            #return None

        #print "Retrieving ", weName, " at ", level
        #  Make a new time range to span all current data
        timeRange = self.GM_makeTimeRange(trList[0].startTime().unixTime(),
                                          trList[-1].endTime().unixTime())
        
        grid = self.getGrids(dbName, weName, level, timeRange, mode="Max")

#         for tr in trList:
#             grid = self.getGrids(dbName, weName, level, tr, mode="Max")

        #  Convert current surge values from meters to feet  
        mask = (grid <= -100)
        grid /= 0.3048
        grid[mask] = -80.0
#         grid[mask] = np.where(mask,surgeVal*3.28, np.float32(-80.0))

        return grid  # convert meters to feet 

    #  Method to create the inundation timing grids
    def makeInundationTiming(self, modelName, pctStr, level, smoothThreatGrid, mutableID, ssea, MHHWMask):

        dbName = self.getSiteID() + "_D2D_" + modelName
        weName = "Surge" + pctStr + "Pctincr"
        #print "Attempting to retrieve: ", weName, level
        # get the StormSurgeProb inventory
        surgeTRList = self.GM_getWEInventory(weName, dbName, level)
        if len(surgeTRList) == 0:
            self.statusBarMsg("No PHISH grid found.", "U")
            return

        # Make timeRanges for all 13 grids. Start with the beginning of the first Phish grid
        baseTime = int(surgeTRList[0].startTime().unixTime() / (6 * 3600)) * (6 * 3600) #snap to 6 hour period
        trList = self.makeTimingTRs(baseTime)
               
        timingGrids = []
        
        n = 1
        for tr in trList:
            start = tr.startTime().unixTime() - 6*3600
            if n == 1:
                starttimeghls = tr.startTime().unixTime() - 48*3600
                trdelete = TimeRange.TimeRange(AbsTime.AbsTime(starttimeghls - 100*3600),
                                               AbsTime.AbsTime(starttimeghls + 160*3600))
                self.deleteCmd(['InundationTiming'], trdelete)
                n = n + 1   
            end = tr.startTime().unixTime()
            tr6 = TimeRange.TimeRange(AbsTime.AbsTime(start),
                                      AbsTime.AbsTime(end))
            
            surgeTR = TimeRange.TimeRange(tr.startTime(), AbsTime.AbsTime(tr.startTime().unixTime() + 3600))
            if surgeTR in surgeTRList:
                phishGrid = self.getGrids(dbName, weName, level, surgeTR)
            else:
                phishGrid = self.empty()
                
#
# For consistency we need to add smoothing here too as we do in execute.
#
            if phishGrid is None:
                self.statusBarMsg("No PHISH grid available for:" + repr(tr), "S")
                continue       
            
            #print "SmoothThreatGrid:", smoothThreatGrid
            if smoothThreatGrid == "Yes":
#                 mask = np.greater(phishGrid, 0.0) & ssea
#                 phishGrid = np.where(np.greater(phishGrid, 0.0), self.GM_smoothGrid(phishGrid, 3, mask), phishGrid)
                mask = (phishGrid > 0.0) & ssea
                smoothedPhish = self.GM_smoothGrid(phishGrid, 3, mask)
                phishGrid[mask] = smoothedPhish[mask]

            #  Convert units from meters to feet
#             mask = (phishGrid <= -100)
            grid = phishGrid / 0.3048
#             grid[mask] = -80.0
#             grid = np.where(phishGrid>-100, phishGrid*3.28, np.float32(-80.0)) # Convert units from meters to feet
            
            grid.clip(0.0, 100.0, grid)
            grid[~ssea] = 0.0
            grid[MHHWMask] = 0.0
            timingGrids.append(grid)
            self.createGrid(mutableID, "InundationTiming", "SCALAR", grid, tr6, precision=1)

        return trList,timingGrids

    def makeInundationMaxGrid(self, timingGrids, trList):

        itCube = np.array(timingGrids)
        maxGrid = np.amax(itCube, axis=0)

        now = int(self._gmtime().unixTime() / 3600) * 3600
        maxTimeRange = self.GM_makeTimeRange(now, now + 48 * 3600)

        self.createGrid(self.mutableID(), "InundationMax", "SCALAR", maxGrid, maxTimeRange)
        
        return maxGrid

#**************************************************************************************
# THis procedure was written to extract MSL to NAVD corrections from the VDATUMS D2D
# Database. It is not yet implemented because the VDATUMS database has not been
# finalized.

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

    def getVDATUM(self, weName, limit):
        siteID = self.getSiteID()
        dbName = siteID + "_D2D_VDATUMS"

        grid = self.getGrids(dbName, weName, "SFC", TimeRange.allTimes(),
                              mode="First")

        if grid is None:
            msgStr = weName + " does not exist in the VDATUMS model. "
            self.statusBarMsg(msgStr, "S")

        mask = (grid <= limit)
        grid /= 0.3048
        grid[mask] = -80.0

        # Converted from meters to feet
        return grid

# This procedure was written to extract MSL to NAVD88 corrections from the VDATUMS D2D
# Database.
        
    def getMSLtoNAVD(self):       
        return self.getVDATUM("MSLtoNAVD88", -0.40)


# This procedure was written to extract MSL to MLLW corrections from the VDATUMS D2D
# Database.

    def getMSLtoMLLW(self):
        return self.getVDATUM("MSLtoMLLW", 0.0)
    
# This procedure was written to extract MSL to MHHW corrections from the VDATUMS D2D
# Database.

    def getMSLtoMHHW(self):
        return self.getVDATUM("MSLtoMHHW", -3.09)

# This procedure was written to extract NAVD88 to MLLW corrections from the VDATUMS D2D
# Database. 
    def getNAVDtoMLLW(self):
        return self.getVDATUM("NAVD88toMLLW", -2.20)

# This procedure was written to extract NAVD88 to MLLW corrections from the VDATUMS D2D
# Database.

    def getNAVDtoMHHW(self):
        return self.getVDATUM("NAVD88toMHHW", -3.40)
    
    # Copies the specified weather elements in elementList into the Fcst database.
    def copyISCGridstoFcst(self, elementList, mutableID):
        
        # Initialize all the grids we plan to return
        
        surgePctGrid = None
        surgePctGridMSL = None
        surgePctGridMLLW = None
        surgePctGridMHHW = None
        surgePctGridNAVD = None
        
        baseTime = self.baseGuidanceTime()

        # Remove all the grids first before replacing them later
            
        self.deleteCmd(elementList, TimeRange.allTimes())
 
# Amended To distinguish when inundation grids are available but not datum ones.
        for weName in elementList:
            #print "Processing ISC ", weName
            GridsCheck = True
            iscWeName = weName + "nc"
            # get the inventory for the ISC grids
            try:
                trList = self.GM_getWEInventory(iscWeName, "ISC", "SFC")
            except:
                GridsCheck = False
            
            if len(trList) == 0:
                GridsCheck = False

            if (weName == "InundationMax" or weName == "InundationTiming") and not GridsCheck:
                self.statusBarMsg("No inundation grids found in ISC database for " + iscWeName + ". Stopping. Revert Forecast db.", "S")
                return None, None, None, None, None

            if not GridsCheck:
                self.statusBarMsg("No datum grids in ISC database for " + iscWeName + ". Proceeding without it.", "S")
                
            # Make sure that the ISC grids are current
            if GridsCheck:
                if baseTime > trList[0].startTime().unixTime():
                    if weName == "InundationMax" or weName == "InundationTiming":
                        self.statusBarMsg("ISC grids for inundation element " + iscWeName + " are not current. They correspond to a previous cycle. Aborting. Revert Forecast db.", "S")
                        return None, None, None, None, None
                    else:
                        self.statusBarMsg("ISC grids for datum element " + iscWeName + " are not current. They correspond to a previous cycle. Proceeding without it.", "S")                    
                        GridsCheck = False                
                        
            for tr in trList:
                grid = self.getGrids("ISC", iscWeName, "SFC", tr)
                if iscWeName == "InundationMaxnc" or iscWeName == "InundationTimingnc":
                    grid.clip(0.0, 100.0, grid)
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
    
    def execute(self, varDict, editArea, timeRange):
        
        t0 = time.time()
        
        self._timeRange = timeRange
        
        mutableID = self.mutableID()
        
        # List of elements       
        # See if we should copy from ISC. If so, do the copy and exit
        smoothThreatGrid = varDict["Grid Smoothing?"]

        makeOption = varDict["Make grids from \nPHISH, ISC, or Manually?"] 
        topodb = "NED"

        ssea = self.encodeEditArea("StormSurgeWW_EditArea")

        Topo = self.getAvgTopoGrid(topodb)

        confidenceStr = varDict["Forecast Confidence? - (Applies to PHISH Only)"]
             
        # extract the percent value from this string
        pctPos = confidenceStr.find("%")
        pctStr = confidenceStr[pctPos - 2:pctPos]
        
        threatWEName = "StormSurgeThreat"

        #print "pctStr is: ", pctStr
        surgePctGrid = None
        surgePctGridMSL = None
        surgePctGridMLLW = None
        surgePctGridNHHW = None
        surgePctGridNAVD = None

        if makeOption == "PHISH" or makeOption == "PETSS":
           
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
            if smoothThreatGrid == "Yes":
                #mask = np.greater(surgePctGrid, 0.0) & ssea
                #surgePctGrid = np.where(np.greater(surgePctGrid, 0.0), self.GM_smoothGrid(surgePctGrid,3, mask), surgePctGrid)

#                 mask = np.greater(surgePctGridNAVD, -10.0) & ssea
#                 surgePctGridNAVD = np.where(np.greater(surgePctGridNAVD, -10.0), self.GM_smoothGrid(surgePctGridNAVD,3, mask), surgePctGridNAVD)

                mask = (surgePctGridNAVD > -10.0) & ssea
                surgePctGridNAVD = self.GM_smoothGrid(surgePctGridNAVD, 3, mask)
        
#             surgePctGridMSL= np.where(mask1, surgePctGridNAVD - msltonavd, np.float32(-80.0)) # MSL Grid     
            navdMask = (surgePctGridNAVD > -80.0)
            mask = (msltonavd > -80.0) & navdMask & ssea
            
            #  MSL Grid            
            surgePctGridMSL = surgePctGridNAVD - msltonavd 
            surgePctGridMSL[~mask] = -80.0

#             surgePctGridMLLW = np.where(np.greater(navdtomllw,-80.0) & np.greater(surgePctGridNAVD,-80.0), \
#                                         surgePctGridNAVD + navdtomllw, np.float32(-80.0)) # MLLW Grid
            
            #  MLLW Grid
            mask = (navdtomllw > -80.0) & navdMask
            surgePctGridMLLW = surgePctGridNAVD + navdtomllw
            surgePctGridMLLW[~mask] = -80.0
            
#             surgePctGridMHHW = np.where(np.greater(navdtomhhw,-80.0) & np.greater(surgePctGridNAVD,-80.0), \
#                                         surgePctGridNAVD + navdtomhhw, np.float32(-80.0)) # MHHW Grid
            #  MHHW Grid
            mask = (navdtomhhw > -80.0) & navdMask
            surgePctGridMHHW = surgePctGridNAVD + navdtomhhw
            surgePctGridMHHW[~mask] = -80.0

#             surgeDiffMLLWMHHW = np.where(np.greater(surgePctGridMLLW,-80.0) & np.greater(surgePctGridMHHW, -80.0), \
#                                          surgePctGridMLLW-surgePctGridMHHW, np.float32(-80.0)) # Diff Grid Between MLLW and MHHW
            
            #  Diff Grid Between MLLW and MHHW (i.e tidal range)
            mask = (surgePctGridMLLW > -80.0) & (surgePctGridMHHW > -80.0)
            surgeDiffMLLWMHHW = surgePctGridMLLW - surgePctGridMHHW
            surgeDiffMLLWMHHW[~mask] = -80.0

            #  Mask 
            MHHWMask = surgePctGridMHHW <= 0.0
            
            #surgePctGrid[MHHWMask] = 0.0

            trList,timingGrids = self.makeInundationTiming(modelName, pctStr, "FHAG0", smoothThreatGrid, mutableID, ssea, MHHWMask)
            #surgePctGrid and InundationMax recomputed from InundationTiming sequence for consistency
            surgePctGrid = self.makeInundationMaxGrid(timingGrids, trList)

        elif makeOption == "ISC":

            elementList = ["InundationMax","InundationTiming", "SurgeHtPlusTideMSL","SurgeHtPlusTideMLLW",
                           "SurgeHtPlusTideNAVD","SurgeHtPlusTideMHHW"]
            surgePctGrid,surgePctGridMSL,surgePctGridMLLW,surgePctGridMHHW,surgePctGridNAVD = self.copyISCGridstoFcst(elementList, mutableID)
            # if you look in CopyISC method if either InundationMax or InundationTiming is missing the procedure stops all together and notifies forecaster.
            if surgePctGrid is None:
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
            #if varDict["Make grids from \nPHISH, PETSS, ISC, or Manually?"] == "Manually Add":
            if varDict["Make grids from \nPHISH, ISC, or Manually?"] == "Manually Add":
                imTRList = self.GM_getWEInventory("InundationMax", mutableID, "SFC")
                if len(imTRList) > 0:
                    imTR = imTRList[0]
                    surgePctGrid = self.getGrids(mutableID, "InundationMax", "SFC", imTR)
       
            surgePctGrid[modifyMask] = inundationHeight
                 
            # Make the timing grids
            baseTime = self.baseGuidanceTime()
            if makeOption == "Manually Replace":   # Make new grids and replace all IT grids
                trList, timingGrids = self.getTimingGrids()
            
                for i in range(len(trList)):
                    # only modify grids in the specified time range
                    start = trList[i].startTime().unixTime()
                    end = trList[i].endTime().unixTime()                        
                    
                    if (start - baseTime) / 3600 >= inunStartHour and (end - baseTime) / 3600 <= inunEndHour:                
                        timingGrids[i] = surgePctGrid # populate only where needed
                    
                timeRange = TimeRange.allTimes()
                self.deleteCmd(["InundationTiming"], timeRange)
                for i in range(len(trList)):
                    timingGrids[i].clip(0.0, 100.0, timingGrids[i])
                    self.createGrid(mutableID, "InundationTiming", "SCALAR", timingGrids[i], trList[i])
                    
            elif makeOption == "Manually Add": # Just replace the selected grid points over the selected time
                # Fetch the existing IT grids
                itTRList = self.GM_getWEInventory("InundationTiming", mutableID, "SFC")
                if len(itTRList) == 0:
                    self.statusBarMsg("No InundationTiming grids found at all.", "S")
                    return
                #Fetch the grids
                itGrids = []
                trList = []
                for tr in itTRList:
                    start = tr.startTime().unixTime()
                    end = tr.endTime().unixTime()                        
                    #print "Checking tr:", tr
                    if (start - baseTime) / 3600 >= inunStartHour and (end - baseTime) / 3600 <= inunEndHour:                                    
                        grid = self.getGrids(mutableID, "InundationTiming", "SFC", tr)
                        itGrids.append(grid)
                        trList.append(tr)
                        
                if len(itGrids) == 0:
                    self.statusBarMsg("No InundationTiming grids found for selected start and end hours.", "S")
                    return
                    
                # Surgically insert grid values into the InundationTiming grids over the selected hours
                for i in range(len(trList)):
                    itGrids[i][modifyMask] = inundationHeight # poke in the values
                    
                    self.createGrid(mutableID, "InundationTiming", "SCALAR", itGrids[i], trList[i])  

        elif makeOption == "UpdateInunMax (Edit Inundation Timing Grids)":
            
            self.deleteAllGrids(["InundationMax","SurgeHtPlusTideMSL", "SurgeHtPlusTideMLLW",
                                 "SurgeHtPlusTideNAVD", "SurgeHtPlusTideMHHW", "SurgeHtPlusTideMLLW"])

            itTRList = self.GM_getWEInventory("InundationTiming", mutableID, "SFC")           
            
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
                surgePctGrid = self.empty()
            
            surgePctGrid = self.makeInundationMaxGrid(timingGrids, itTRList)

            #return
                # Done with manual options            
        
# Next line introduced on Jan 2017 SWiT. It forces points in InundationMax that are > 1 and < 1.5 to 1.5. This is because TCV rounds to 
# nearest one foot for categorical HTI threat level consistency with inundation graphic. Not doing this would cause TCV to throw away zones that
# might have more than 3% coverage of inundation > 1 but less than 1.5 altogether. Changing TCV to key on anything with InundationMax >= 1 would not
# do because it would then include zones in TCV with inundation forecasts of less than 1 but >= 0.5 overdoing the threat.

        surgePctGrid[(surgePctGrid > 1.0) & (surgePctGrid < 1.5)] = 1.5 
        
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
 
            if keyMap[key] == "Extreme":
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

        if makeOption != "Manually Replace" and makeOption != "Manually Add" and makeOption != "UpdateInunMax (Edit Inundation Timing Grids)":
            if surgePctGridMSL is not None:
                surgePctGridMSL.clip(-30.0, 100.0, surgePctGridMSL)
                self.createGrid(mutableID, "SurgeHtPlusTideMSL", "SCALAR", surgePctGridMSL,
                                timeRange, precision=2)
            if surgePctGridMLLW is not None:
                surgePctGridMLLW.clip(-30.0, 100.0, surgePctGridMLLW)
                self.createGrid(mutableID, "SurgeHtPlusTideMLLW", "SCALAR", surgePctGridMLLW,
                                timeRange, precision=2)
            if surgePctGridNAVD is not None:
                surgePctGridNAVD.clip(-30.0, 100.0, surgePctGridNAVD)
                self.createGrid(mutableID, "SurgeHtPlusTideNAVD", "SCALAR", surgePctGridNAVD,
                                timeRange, precision=2)
            if surgePctGridMHHW is not None:
                surgePctGridMHHW.clip(-30.0, 100.0, surgePctGridMHHW)
                self.createGrid(mutableID, "SurgeHtPlusTideMHHW", "SCALAR", surgePctGridMHHW,
                                timeRange, precision=2)

        # Make the grid. Start with the existing grid if we have one otherwise zeros
        coastalThreat = self.empty(np.int8)
        surgePctGrid.clip(0.0, 100.0, surgePctGrid)
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
            #coastalMask = ssea & np.greater_equal(surgePctGrid, thresh)
            coastalMask = ssea & np.greater(surgePctGrid, thresh)
            coastalThreat[coastalMask] = keyIndex
 
#       create the CoastalThreat Grid
        self.createGrid(mutableID, threatWEName, "DISCRETE",
                        (coastalThreat, threatKeys), timeRange,
                        discreteKeys=threatKeys,
                        discreteOverlap=0,
                        discreteAuxDataLength=2,
                        defaultColorTable="Hazards")

        t1 = time.time()
        LogStream.logEvent("Finished TCStormSurgeThreat in %f.4 ms" % ((t1-t0) * 1000))

        return
