# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# InlandFloodThreat
#
# Author: lefebvre,santos
# Last Modified: April 4, 2012 - Fixed to handle negative gridded FFG.
# Migrated procedure for AWIPS2. Updated 6/22/2012.   S.O.
# 
# Comment on 05/21/2014 (Santos): Some sites used QPF and others QPF6hr. Check that parameter 
# in getQPFGrid method. Also in AWIPS 2 FFG is gridded and called just FFG. 
# This is fixed in getRFCFFGModels method.
#
# LeFevbre/Santos: This is the version being turned in for baseline in 16.1.2 as of 12/7/2015. It includes fixes
#for new ERP data changes that took place in Summer of 2015 and better handling of grid points where there
#is no FFG guidance available.

# 
#Search for COMMENTS to see any local config step you might need to take.
# ----------------------------------------------------------------------------

MenuItems = ["Populate"]

VariableList = [("Gridded/Text FFG Blending Factor" , 0.75, "scale", [0, 1], 0.05)]

import SmartScript
import time
import popen2
import sys
import AbsTime
import TimeRange
 
# # For AWIPS 1/ AWIPS 2 compatibility
# try:
#     from Numeric import *
# except:
#     from numpy import *

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
            #print 40 * "*"
            #print "start and end times are:", startTime, endTime
            tr = TimeRange.TimeRange(startTime, endTime)

        return tr
    
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
            start = g.gridTime().startTime().unixTime()
            end = g.gridTime().endTime().unixTime()
            tr = self.makeTimeRange(start, end)
            if timeRange.overlaps(tr):
                trList.append(tr)   

        return trList

    # get the current time, truncates to the last six hour value.
    # returns a timeRange with this startTime until 72 hrs from this time
    
    def make72hrTimeRange(self, startTime):
        
        # Make the end time 3 days from the startTime
        end = startTime + (72 * 3600)
        # Convert them to AbsTimes
        startTime = AbsTime.AbsTime(startTime)
        endTime = AbsTime.AbsTime(end)
        
        timeRange = TimeRange.TimeRange(startTime, endTime)

        return timeRange

    # returns a list of timeRange with the specified duration in hours over the
    # specified timeRange
    def makeTimeRangeList(self, timeRange, duration):
        trList = []
        sTime = timeRange.startTime().unixTime()
        delta = duration * 3600
        while sTime < timeRange.endTime().unixTime():
            trList.append(self.makeTimeRange(sTime, sTime + delta))
            sTime = sTime + delta

        return trList
        
    # Returns a list of database IDs matching the specified model name,
    # weather element name and level
    def getModelList(self, modelName, weName, weLevel):
        modelList = []

        availParms = self.availableParms()

        for pName, level, dbID in availParms:
            if modelName in dbID.modelName():
                if weName in pName:
                    if weLevel in level:
                        if dbID not in modelList:   
                            modelList.append(dbID)
        return modelList

    # A small algorithm to determine the day number
    def determineDay(self, modelTime, validTime):

        diff = (validTime - modelTime) / 3600
        if diff < 48:
            return 1
        elif diff < 72:
            return 2
        else:
            return 3

        return 0
    
    def baseModelTime(self, modelTime):
        
        oneDay = 3600 * 24
        halfDay = 3600 * 12
        baseTime = (int((modelTime + halfDay) / oneDay) * oneDay) - halfDay
        
        return baseTime
    
    def getLatestERPAnchorTime(self):
        ERPModelName = "HPCERP"
        ERPVarName = "ppffg"      # This variable commented out 
        ERPLevel = "SFC"
        # get the list of all available models. They come sorted latest to oldest. 
        modelList = self.getModelList(ERPModelName, ERPVarName, ERPLevel)
        
        if len(modelList) > 0:
            anchorTime = self.baseModelTime(modelList[0].modelTime().unixTime())
            return anchorTime
        
        self.statusBarMsg("No ERP Guidance found.", "S")
        return None

    def getERPGrids(self):
        ERPModelName = "HPCERP"
        
        ERPVarName = "ppffg" 
        ERPLevel = "SFC"

        # make a dict and fill with grids with the default value.
        # These will be replaced when we find real grids
        gridDict = {}
        for i in range(1, 4):
            gridDict[i] = None

        # get the list of all available models. They come sorted latest to oldest. 
        modelList = self.getModelList(ERPModelName, ERPVarName, ERPLevel)
       
        # Debug output. Remove after testing
#         print "ERP models available:"
#         for m in modelList:
#             print m, "baseTime:", time.asctime(time.gmtime(self.baseModelTime(m.modelTime().unixTime())))
            
        # Calculate the nominal time we're searching for based on the latest model
        anchorTime = self.baseModelTime(modelList[0].modelTime().unixTime())
        
        # Debug output. Remove after testing
        #print "anchorTime:", time.asctime(time.gmtime(anchorTime))
        
        # keep only the most recent grids
        for model in modelList:
            
            # Only process models with a base time matching the latest base time. Ignore all others.
            # This will cause the tool to abort if all the current grids are not yet in.
            if self.baseModelTime(model.modelTime().unixTime()) != anchorTime:
                # Debug output. remove after testing
                print "Ignoring model at:", time.asctime(time.gmtime(model.modelTime().unixTime()))
                continue
            
            trList = self.getWEInventory(model, ERPVarName)
            modelTime = model.modelTime()

            for tr in trList:
                dayNum = self.determineDay(anchorTime, tr.startTime().unixTime())
                grid = self.getGrids(model, ERPVarName, ERPLevel, tr, mode="First")
                if gridDict[dayNum] is None:
                    gridDict[dayNum] = grid
                    print "modelTime and dayNum are: ", modelTime, dayNum
           
            # Check to see if we found all the grids we need
            allGridsFound = True
            for dayNum in range(1, 4):
                if gridDict[dayNum] is None:
                    allGridsFound = False
            
            # We found all the grids, return them
            if allGridsFound:
                return gridDict

        # After processing all the models, make sure we found all the grids we need.
        for i in range(1, 4):
            if gridDict[i] == None:
                errorStr = "Day" + str(i) + " grid not found in AWIPS database."
                self.statusBarMsg(errorStr, "S")
                return None
            
        return gridDict
    
    # Use this method for testing if you have no luck getting products
    # directly from the text database
    def getTextProductFromFile(self, filename):
        # replace the filename with one on your system
        filename = "/tmp/FFGData/" + filename + ".txt"
        f = file(filename, 'r')
        textList = []
        line = f.readline()
        textList.append(line)
        while line != "":
            line = f.readline()
            textList.append(line)
        f.close()
        return textList

    # Retrieves a text product from the text database
    def getTextProductFromDB(self, productID):
        cmd = "textdb -r " + productID

        (stdout, stdin, stderr) = popen2.popen3(cmd)

        textList = []
        line = stdout.readline()
        textList.append(line)
        while line != "":
            line = stdout.readline()
            textList.append(line)
        return textList

    # given a text product, this method decodes the ffg values and
    # returns a dictionary  {area : value}
    def decodeFFGText(self, ffgText):
        ffgDict = {}
        for s in ffgText:
            parts = s.split()
            if len(parts) < 4:
                continue
            if len(parts[0]) != 6:
                continue
            if parts[0][2] != "Z":
                continue
            area = parts[0]
            value6hr = float(parts[3][:-1])  # strip the "/"
            ffgDict[area] = value6hr

        return ffgDict

    # Fetch all of the gridded FFG model names and return just the
    # latest version of each type (region)
    def getRFCFFGModelName(self, rfcName):

        # Find all the models matching this description.
        modelList = self.getModelList(rfcName, "FFG0624hr", "SFC")

        for model in modelList:
            # WARNING!!! This check should be more specific to the DBID string.
            if model.modelIdentifier().find(rfcName) > -1:
                return model
        
        return None

    # Smooths the specified grid by the specified factor
    # With factor == 3, 3x3 smooth, factor == 5 5x5 smooth, etc.
    # Even factors (4, 6, 8,...) round up to the next odd value
    # If factors <3 are specified, the unmodified grid is returned.
    def smoothGrid(self, grid, factor=3):
        # factors of less than 3 are useless or dangerous
        if factor < 3:
            return grid
        st = time.time()
        half = int(factor)/ 2
        sg = zeros(grid.shape, 'f8')
        count = zeros(grid.shape, 'f8')
        gridOfOnes = ones(grid.shape, 'f8')
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
                sg[target] = sg[target] + grid[src]
                count[target] = count[target] + gridOfOnes[src]
        return sg / count
    
    # Returns the list of RFCs that overlap the local GFE domain
    # as defined by getSiteID().
    def getOverlappingRFCs(self):
        # The list of all the RFCs
        RFCEditAreas = ["ISC_PTR", "ISC_RSA", "ISC_STR", "ISC_ACR", "ISC_KRF", "ISC_MSR", "ISC_TUA", "ISC_FWR", "ISC_ORN",
                        "ISC_TIR", "ISC_ALR", "ISC_RHA", "ISC_TAR"]

        cwaEA = self.getSiteID()
        cwaMask = self.encodeEditArea(cwaEA)

        eaList = self.editAreaList()
        
        rfcList = []
        
        for rfc in RFCEditAreas:
            
            rfcMask = None
            if rfc in eaList:
                rfcArea = self.getEditArea(rfc) 
                rfcMask = self.encodeEditArea(rfc)
            
            if rfcMask is None:
                continue
            
            overlap = logical_and(cwaMask, rfcMask)
            
            if overlap.any():
                rfcList.append(rfc)
        
        return rfcList

    # First try to access the gridded FFG from the D2D files.  If they exist
    # mosaic all the ones we find and return the composite.  If we can't
    # find any gridded FFG, then fetch the FFG text product, decode it,
    # and create a patchwork grid from the guidance value in each county
    # or basin.
    def getRFCFlashFloodGrid(self, productList, varDict):

        ffgGrid = self.empty()
        foundGrids = False
        factor = varDict["Gridded/Text FFG Blending Factor"]
        
        RFCList = self.getOverlappingRFCs()
        #print "RFCList is: ", RFCList

        ffgWEName = "FFG0624hr"
        # Fetch the gridded FFG, mosaic these into a single grid
        for rfc in RFCList:
                        
            tmplist = rfc.split('_');
            rfcsid = tmplist[1];
            rfcName = 'FFG' + rfcsid;
            # Find the model for this RFC
            modelName = self.getRFCFFGModelName(rfcName)
            
            #print "modelName:", modelName
            if modelName is None:
                self.statusBarMsg("No FFG database found for " + rfc, "S")
                continue
            
            trList = self.getWEInventory(modelName, ffgWEName)
            if len(trList) == 0:
                self.statusBarMsg("No FFG grids found in database " + modelName, "S")
                continue
            
            # get the first grid
            tempGrid = self.getGrids(modelName, ffgWEName, "SFC", trList[0], mode="First")
            # Make an rfc mask
            rfcEA = self.getEditArea(rfc)
            rfcMask = self.encodeEditArea(rfcEA)
            
            mask = (tempGrid > 0.0) & rfcMask
            ffgGrid[mask] = tempGrid[mask]
            ffgGrid[mask] /= 25.4
            foundGrids = True
            
            

        # Comment this in to see intermediate FFG grid from gridded guidance
##        tr = self.getTimeRange("Today")
##        self.createGrid("Fcst","FFGFromGrid", "SCALAR", ffgGrid, tr,
##                            minAllowedValue = -1, maxAllowedValue=100,
##                            precision=2)

        
        # Make another FFG grid from the text guidance
        editAreaList = self.editAreaList()

        ffgTextGrid = self.empty()
        for prod in productList:

            # Uncomment the next line to fetch FFG data from a file
##            ffgText = self.getTextProductFromFile(prod)

            # Use this method to retrieve FFG data from the text database
            ffgText = self.getTextProductFromDB(prod)
            
            ffgDict = self.decodeFFGText(ffgText)

            for area in ffgDict.keys():
                if area not in editAreaList:
                    continue
                refArea = self.getEditArea(area)
                mask = self.encodeEditArea(refArea)
                value = ffgDict[area]
                ffgTextGrid[mask] = value

        # Comment this in to see intermediate FFG from text guidance
##        tr = self.getTimeRange("Today")
##        self.createGrid("Fcst","FFGFromText", "SCALAR", ffgTextGrid, tr,
##                            minAllowedValue = -1, maxAllowedValue=100,
##                            precision=2)

        # Since the gridded FFG tends to have lots of holes in it,
        # fill those holes with the text version of the FFG where the
        # gridded FFG is less than its non-zero average.

        # if we found the grids fill in it in with values from the text products.
        if foundGrids:
            # get the non-zero gridded average
            mask = greater(ffgGrid, 0.0)
            maskSum = sum(sum(mask))
            gridSum = sum(sum(ffgGrid))
            if maskSum > 0:
                gridAvg = gridSum / maskSum
            else:  # nothing to do but return the text grid
                return ffgTextGrid
            
            # fill in textFFG where griddedFFG is less than average
            #print "AVERAGE IS: ", gridAvg
            ffgGrid = where(less(ffgGrid, gridAvg * factor), ffgTextGrid, ffgGrid)           
        else:
            ffgGrid = ffgTextGrid

        return ffgGrid

    # Returns the QPF sum over the specified timeRange
    def getQPFGrid(self, timeRange):
#
# This assumes QPF has a constraint of TC6NG. If not and your office uses QPF6 or QPF6hr you will need to change this here
# accordingly.
#
        trList = self.getWEInventory("Fcst", "QPF", timeRange)
        if len(trList) == 0:
            return None

        qpfGrid = self.empty()
        for tr in trList:
            grid = self.getGrids("Fcst", "QPF", "SFC", timeRange, mode="First")
            qpfGrid += grid

        return qpfGrid

    def execute(self, varDict):

        ### CONFIGURATION SECTION  ################################
        ### Levels must exactly match the levels in the inland threat
        ### weather element.
        ratios = [0.0, 0.75, 1.0, 2.0, 100.0]
        erps = [0.0, 5.0, 10.0, 15.0, 100.0]
        cumqpfs = [0.0, 5.0, 10.0, 15.0, 20.0]

        threatMatrix = [
            ["None",     "Very Low", "Elevated",     "Mod"    ], # lowest ERP
            ["Very Low", "Elevated",      "Mod",     "High"   ],
            ["Elevated",      "Mod",      "High",    "Extreme"],
            ["Mod",      "High",     "Extreme", "Extreme"], # highest # ERP
            ] #  low ------ QPF/FFG ratio -------->high
                        
        # COMMENTS: The list of FFG products that contain FFG data for your WFO
        # The following is an example for Miami. Default list is emply. You must
        # populate it with your CWA FFG guidance.
        #productList = ["ATLFFGMFL", "ATLFFGTBW", "ATLFFGMLB","ATLFFGKEY"]
        productList = []
        if len(productList) == 0:
            self.statusBarMsg("You have not configured Text FFG in Procedure. Create a site level copy, and configure your text FFG Guidance. Search for COMMENTS in the procedure.", "S")

        ###   END CONFIGURATION SECTION  #################################

        ## Replace the "Very Low" with "Elevated"  if "Very Low" dows not exist
        ## in the list of discrete keys
        
        try:
            threatKeys = self.getDiscreteKeys("FloodingRainThreat")
        except:
            threatKeys = ["None", "Elevated", "Mod", "High", "Extreme"]
        
        # replace "Very Low" with "Elevated"
        if "Very Low" not in threatKeys:  
            for i in range(len(threatMatrix)):
                for j in range(len(threatMatrix[i])):
                    if threatMatrix[i][j] == "Very Low":
                        threatMatrix[i][j] = "Elevated"


        # Find the nominal start time when we will be making grids
        anchorTime = self.getLatestERPAnchorTime()
        
        # make a 72 hour timeRange and a list of 6 hour timeRanges based on the anchorTime
        timeRange = self.make72hrTimeRange(anchorTime)
        
        print "Anchor TimeRange:", timeRange

        trList = self.makeTimeRangeList(timeRange, 6)

        maxFloodThreat = self.empty()
        
        # Fetch the FFG grid either from gridded data or the text product
        #print "Getting FFG Grid Now: "
        ffgGrid = self.getRFCFlashFloodGrid(productList,varDict)
        #print "GOT FFG Grid"

        # calculate the areas where the FFG is missing. We will fill these values with None eventually        
        missingFFGMask = less_equal(ffgGrid, 0.0)
        
        # get the ERP grids and stuff them in six hour time blocks to match
        # the cummulative QPF grids will create later
        erpGridDict = self.getERPGrids()
        
        if erpGridDict is None:  # We're in that window where we should wait.
            self.statusBarMsg("The current ERP guidance is not yet completely available. Please re-run this tool at a later time.", "S")
            return

        for i in range(len(trList)):
            
            erpGrid = erpGridDict[int(i / 4 + 1)]

            tr = trList[i]
            #print "TIME RANGE IS:", tr

            qpfGrid = self.getQPFGrid(tr)

            if ffgGrid is None or qpfGrid is None:
                self.statusBarMsg("FlashFlood or QPF grids missing at timeRange:" +
                                  str(tr), "S")
                continue
            if erpGrid is None:
                self.statusBarMsg("ERP grids missing at timeRange:" +
                                  str(tr), "S")
                continue

            tempffgGrid = where(equal(ffgGrid, 0.0), float32(1000.0), ffgGrid)
            ratioGrid = qpfGrid / tempffgGrid
            ratioGrid[equal(ffgGrid, 0.0)] = 0.0
            
            # Clip the ratioGrid to 8.0 to prevent problems when displaying
            ratioGrid.clip(0.0, 8.0, ratioGrid)
                        
            self.createGrid("Fcst", "ERP", "SCALAR", erpGrid, tr,
                            minAllowedValue = -1, maxAllowedValue=100,
                            precision=2)
            self.createGrid("Fcst", "FFGNEW", "SCALAR", ffgGrid, tr,
                            minAllowedValue = 0, maxAllowedValue=10,
                            precision=2)
            self.createGrid("Fcst", "QPFtoFFGRatio", "SCALAR",
                            ratioGrid, tr, precision=2,
                            minAllowedValue = 0, maxAllowedValue=1000)

            floodThreat = self.empty()
            
            for e in range(len(erps) - 1):
                for r in range(len(ratios) - 1):
                    eMin = erps[e]
                    eMax = erps[e+1]
                    rMin = ratios[r]
                    rMax = ratios[r+1]
                    ratioMask = logical_and(greater_equal(ratioGrid, rMin),
                                            less(ratioGrid, rMax))
                    erpMask = logical_and(greater_equal(erpGrid, eMin), 
                                          less(erpGrid, eMax))
                    mask = logical_and(ratioMask, erpMask)
                    keyIndex = self.getIndex(threatMatrix[r][e], threatKeys)
                    floodThreat[mask] = keyIndex
                    
            # Now set the values we found missing to the None key
            noneIndex = self.getIndex("None", threatKeys)
            floodThreat[missingFFGMask] = noneIndex

            # Create the grid
            self.createGrid("Fcst", "FloodThreat", "DISCRETE",
                        (floodThreat, threatKeys), tr,
                        discreteKeys=threatKeys,
                        discreteOverlap=0,
                        discreteAuxDataLength=2,
                        defaultColorTable="gHLS_new")        
            
            maxFloodThreat = maximum(floodThreat, maxFloodThreat)

        # make a 6 hour TimeRange
        # startTime = trList[0].startTime()
        startTime = int(self._gmtime().unixTime()/ 3600) * 3600 - (24 * 3600)
        endTime = startTime + (24 * 3600 * 10)
        dbTR = self.makeTimeRange(startTime, endTime) 
        cTime = int(self._gmtime().unixTime()/ 3600)  * 3600 
        end = cTime + (6*3600)
        threatTR = self.makeTimeRange(cTime, end)     
        self.deleteCmd(['FloodingRainThreat'], dbTR) 
        self.createGrid("Fcst", "FloodingRainThreat", "DISCRETE",
                        (maxFloodThreat, threatKeys), threatTR,
                        discreteKeys=threatKeys,
                        discreteOverlap=0,
                        discreteAuxDataLength=2,
                        defaultColorTable="gHLS_new")
                           
        
        return 
