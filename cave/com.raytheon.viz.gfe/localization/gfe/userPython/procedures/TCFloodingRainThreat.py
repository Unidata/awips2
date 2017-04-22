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
# Last Modified
# 7/15/2016 - Lefebvre/Santos: working on code to add PQPF to the algorithm.
# 9/2/2016 - Lefebvre/Santos: Finished integrating {QPF into the algorithm
# 9/7/2016 - Lefebvre/Santos: Added better logic for grid missing messages and Don't use guidance option.
# 9/7/2016 - Lefebvre/Santos: Change ppffg timeRanges to anchor on 12Z cycles. 
# VERSION 17.1.1 = The one checked in.
# 11/14/2016 - Santos - Modified at testbed in Silver Springs to fix overlap variable to do the composite 
# of the rfc list edit areas, not just the overlap with cwa mask. Commented out statusBarMsg for the ppffg inventories.
# 
#Search for COMMENTS to see any local config step you might need to take.
# ----------------------------------------------------------------------------

MenuItems = ["Populate"]

VariableList = [("Gridded/Text FFG Blending Factor" , 0.00, "scale", [0, 1], 0.05),
                ("Probabilistic QPF Exceedance Level to use:", "10%", "radio", ["Don't Use Prob Guidance", "10%", "50%"]),

# Use the above line for the GUI when testing is finished. the line below will be enabled when we are ingesting 20%, 30%, and 40% percentiles..................................................... 
#                ("Probabilistic QPF Exceedance Level to use:", "10%", "radio", ["Don't Use Prob Guidance", "10%", "20%", "30%", "40%", "50%"]),
                ]

import SmartScript
import time
import popen2
import sys
import AbsTime
import TimeRange
import GridManipulation
import numpy as np


class Procedure (GridManipulation.GridManipulation):
    def __init__(self, dbss):
        GridManipulation.GridManipulation.__init__(self, dbss)

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
            trList.append(self.GM_makeTimeRange(sTime, sTime + delta))
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
        offset = 3600 * 0  # hours after which we expect models to arrive
        baseTime = (int((modelTime + offset) / oneDay) * oneDay) - offset
        
        return baseTime
    
    def getProbBaseTime(self):
        ERPModelName = "HPCERP"
        ERPVarName = "TP10pct6hr"
        ERPLevel = "SFC"
        # get the list of all available models. They come sorted latest to oldest. 
        modelList = self.getModelList(ERPModelName, ERPVarName, ERPLevel)
        
        if len(modelList) == 0:
            self.statusBarMsg("No ERP Guidance found.", "S")
            return None
        
        for model in modelList:
            trList = self.GM_getWEInventory(ERPVarName, model)
            if len(trList) == 0:
                continue
            
            latestHr = (trList[-1].startTime().unixTime() - model.modelTime().unixTime()) / 3600
            
            # return the time of the first model we find with enough data
            if latestHr >= 72:
                return model.modelTime()
            
        # If we get here, we have found no models with 72 hours of data so return the latest model time
        self.statusBarMsg("No model runs found with 72 hours of grids. Using latest model")
        return modelList[0].modelTime()
            
    # Find the time of the model with a day 3 grid and truncate the modelTime to the last 12Z
    def getPpffgBaseTime(self):
        ERPModelName = "HPCERP"
        ERPVarName = "ppffg"
        ERPLevel = "SFC"
        # get the list of all available models. They come sorted latest to oldest. 
        modelList = self.getModelList(ERPModelName, ERPVarName, ERPLevel)
        
        if len(modelList) == 0:
            self.statusBarMsg("No ERP Guidance found for ppffg.", "S")
            return None
        
        for model in modelList:
            trList = self.GM_getWEInventory(ERPVarName, model)
            if len(trList) == 0:
                continue
            
            latestHr = (trList[-1].startTime().unixTime() - model.modelTime().unixTime()) / 3600
            
            # return the time of the first model we find with enough data
            if latestHr > 48:
                
                #print "model time with day 3 grid:", model.modelTime()
                modelTime = model.modelTime().unixTime() - (12 * 3600)
                
                # truncate the model time to the last 12Z cycle
                baseTime = int(modelTime / (3600 * 24)) * (3600 * 24) + (12 * 3600)
                return AbsTime.AbsTime(baseTime)
            
        # If we get here, we have found no models with 72 hours of data so return the latest model time
        self.statusBarMsg("No model runs found with 72 hours of grids. Using latest model")
        return modelList[0].modelTime()
    
    # Format a timeRange string into something smaller and readable
    def trStr(self, tr):
        return str(tr)[5:7] + "." + str(tr)[11:13] + "-" + str(tr)[29:31] + "." + str(tr)[35:37] + "Z" 

    # Fetch ERP probabilistic data using the latest available model. In some cases 
    # the grids retrieved may originate from more than one model version. In all cases,
    # latest guidance available for each time slot will returned.
    # Returns a dictionary with key as timeRange and grid as the data.    
    def getERPGuidance(self, weName, trList):
        
        ERPModelName = "HPCERP"
        ERPLevel = "SFC"

        #  Get the list of all available models. They come sorted latest to oldest. 
        modelList = self.getModelList(ERPModelName, weName, ERPLevel)
        if len(modelList) == 0:  # No grids found for the model/weName combination
            return {}  # So just return an empty GridDict
                       
        # For each timeRange, find the model with the latest grid and save that
        gridDict = {}
        for tr in trList:
            
            # Determine the equivalent d2D timeRange based on GFE QPF tr in the trList 
            d2dTR = self.GM_makeTimeRange(tr.endTime().unixTime(), tr.endTime().unixTime() + 3600)

            foundGrid = False
            for model in modelList:
            
            #  See if the ERP grids we want are in this model cycle
                d2dInv = self.GM_getWEInventory(weName, model)
                
                
                if d2dTR not in d2dInv:
                    continue          
                
                # Fetch the grid and save it
                grid = self.getGrids(model, weName, ERPLevel, d2dTR, mode="First")
                gridDict[tr] = grid
                
                modelStr = str(model.modelTime())[0:13] + "Z"
                
                ###  DEBUG  DEBUG   DEBUG  #### Remove when testing is complete
                #print "WE:", weName, "GFE TimeRange:", self.trStr(tr), "using model at:", modelStr, " valid:", self.trStr(d2dTR)
                ###  DEBUG  DEBUG   DEBUG  #### Remove when testing is complete
                
                if modelList.index(model) != 0:
                    # Suppress messages for ppffg for the last timeRange only, since ppffg only arrives once per day
                    if weName == "ppffg" and tr == trList[-1]:
                        pass
#                     else:
#                         self.statusBarMsg("Using " + modelStr + " model for time period " + self.trStr(tr), "S")
#                     
                foundGrid = True
                break
            
            # If we get here, no model was found with the TR we want
            if not foundGrid:
                gridDict[tr] = None
                self.statusBarMsg("No Probabilistic WPC QPF found for timeRange:" + str(tr), "S")
            
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


    # Returns the list of RFCs that overlap the local GFE domain
    # as defined by getSiteID().
    def getOverlappingRFCs(self):
        # The list of all the RFCs
        RFCEditAreas = ["ISC_PTR", "ISC_RSA", "ISC_STR", "ISC_ACR", "ISC_KRF",
                        "ISC_MSR", "ISC_TUA", "ISC_FWR", "ISC_ORN", "ISC_TIR",
                        "ISC_ALR", "ISC_RHA", "ISC_TAR"]

##        cwaEA = self.getSiteID()
##        cwaMask = self.encodeEditArea(cwaEA)
        #cwaMask = self.encodeEditArea(self.getSiteID())

        eaList = self.editAreaList()
        
        rfcList = []
        
        for rfc in RFCEditAreas:
            
            rfcMask = None
            if rfc in eaList:
                rfcMask = self.encodeEditArea(rfc)
            
            if rfcMask is None:
                continue
            
            #overlap = cwaMask & rfcMask
            overlap = rfcMask
            
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
            
            trList = self.GM_getWEInventory(ffgWEName, modelName)
            if len(trList) == 0:
                self.statusBarMsg("No FFG grids found in database " + modelName, "S")
                continue
            
            #  Get the first grid
            tempGrid = self.getGrids(modelName, ffgWEName, "SFC", trList[0], mode="First")

            #  Make a mask of the RFC domain
            rfcMask = self.encodeEditArea(rfc)
            
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
            mask = ffgGrid > 0.0
            maskSum = np.sum(np.sum(mask))
            gridSum = np.sum(np.sum(ffgGrid))
            
            if maskSum > 0:
                gridAvg = gridSum / maskSum
            else:  # nothing to do but return the text grid
                return ffgTextGrid
            
            # fill in textFFG where griddedFFG is less than average
            ffgMask = ffgGrid < (gridAvg * factor)
            ffgGrid[ffgMask] = ffgTextGrid[ffgMask]
            #ffgGrid = where(less(ffgGrid, gridAvg * factor), ffgTextGrid, ffgGrid)           
        else:
            ffgGrid = ffgTextGrid

        return ffgGrid

    # Returns the QPF sum over the specified timeRange
    def getQPFGrid(self, timeRange):
#
#  This assumes QPF has a constraint of TC6NG. If not and your office uses QPF6
#  or QPF6hr you will need to change this here accordingly.
#
        #  Inventory all QPF grids from the Fcst database
        trList = self.GM_getWEInventory("QPF", timeRange=timeRange)
        if len(trList) == 0:
            return None

        qpfGrid = self.empty()
        for tr in trList:
            grid = self.getGrids("Fcst", "QPF", "SFC", timeRange, mode="First")
            qpfGrid += grid

        return qpfGrid
    
    def getOverlappingTR(self, tr6, tr24List):
        
        for tr24 in tr24List:
            if tr6.overlaps(tr24):
                return tr24
        return None

    def execute(self, varDict):

#         # Find the nominal start time when we will be making grids
#         start = int(time.time() / (24 * 3600)) * (24 * 3600)  # self._gmtime()
#         for i in range(start, start + (72 * 3600),(6 * 3600)):
#             bTime = self.baseModelTime(i)
#             print "time:", time.asctime(time.gmtime(i)), "BaseTime:", time.asctime(time.gmtime(bTime))
            

        ### CONFIGURATION SECTION  ################################
        ### Levels must exactly match the levels in the inland threat
        ### weather element.
        ratios = [0.0, 0.75, 1.0, 2.0, 100.0]
        erps = [0.0, 2.0, 10.0, 15.0, 100.0]   # CHANGE TO [0.0, 2.0, 5.0, 10.0, 100.0] ?????
        cumqpfs = [0.0, 5.0, 10.0, 15.0, 20.0]  # What is this? We're not using this anywhere

        threatMatrix = [
            ["None",     "Very Low", "Elevated", "Mod"    ], # lowest ERP
            ["Very Low", "Elevated", "Mod",      "High"   ],
            ["Elevated", "Mod",      "High",     "Extreme"],
            ["Mod",      "High",     "Extreme",  "Extreme"], # highest # ERP
            ] #  low ------ QPF/FFG ratio -------->high
                        
        # COMMENTS: The list of FFG products that contain FFG data for your WFO
        # The following is an example for Miami. Default list is emply. You must
        # populate it with your CWA FFG guidance.
#        productList = ["ATLFFGMFL", "ATLFFGTBW", "ATLFFGMLB","ATLFFGKEY"]
        productList = ["ATLFFGMFL", "ATLFFGTBW", "ATLFFGMLB","ATLFFGKEY"]
        if len(productList) == 0:
            self.statusBarMsg("You have not configured Text FFG in Procedure. Create a site level copy, and configure your text FFG Guidance. Search for COMMENTS in the procedure.", "S")

        ###   END CONFIGURATION SECTION  #################################

        ## Replace the "Very Low" with "Elevated"  if "Very Low" does not exist
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
                        
        baseTime = self.getProbBaseTime().unixTime()
        anchorTimeRange = self.GM_makeTimeRange(baseTime, baseTime + (3600 * 72))
        
        ppffgBaseTime = self.getPpffgBaseTime().unixTime()
        ppffgTimeRange = self.GM_makeTimeRange(ppffgBaseTime, ppffgBaseTime + (3600 * 72))

        #print "Prob TimeRange:", anchorTimeRange
        #print "ppffg TimeRange", ppffgTimeRange

        # make a 72 hour timeRange and a list of 6 hour timeRanges based on the anchorTime
        probTRList = self.makeTimeRangeList(anchorTimeRange, 6)
        ppffgTRList = self.makeTimeRangeList(ppffgTimeRange, 24)
     
        # Fetch the probabilistic value selected by the user and make the weName
        probGridDict = {}
        probStr = varDict["Probabilistic QPF Exceedance Level to use:"]
        
        # If we're not using prob guidance, fill the dictionary with grids of zeros
        if probStr == "Don't Use Prob Guidance":
            for tr in probTRList:
                probGridDict[tr] = np.zeros(self.getGridShape(), np.float32)
            probWEName = "TR00pct6hr"
        # If we're using prob guidance, make the prob string into an integer so we can figure out the weName
        else:
            probStr = probStr[:-1]
            # Make an int value so we can subtract it from 100 to get the weName
            try:
                probValue = int(probStr)  # This should always succeed, but just in case...
            except:
                self.statusBarMsg("Error parsing probability string. " + probStr, "U")
                return
            # Fetch the probabilistic guidance
            probWEName = "TP" + str(100 - probValue) + "pct6hr"
            probGridDict = self.getERPGuidance(probWEName, probTRList)
            if not probGridDict:   # no prob grids found for this weName
                self.statusBarMsg(probWEName + " ERP guidance is not available. Please re-run this tool with a different prob exceeedance level or use Don't Use Prob Guidance option in GUI.", "S")
                return
            
        # Create an empty discrete grid
        maxFloodThreat = np.zeros(self.getGridShape(), np.int8)
        
        # Fetch the FFG grid either from gridded data or the text product
        ffgGrid = self.getRFCFlashFloodGrid(productList, varDict)

        # calculate the areas where the FFG is missing. We will fill these values with None eventually
        missingFFGMask = ffgGrid <= 0.0
        
        # Get the ERP grids and stuff them in six hour time blocks to match
        # the cummulative QPF grids will create later
        ppffgGridDict = self.getERPGuidance("ppffg", ppffgTRList)
        
        if not ppffgGridDict:  # Didn't find any ppffg guidance
            self.statusBarMsg("The current ERP guidance is not available. Please re-run this tool at a later time.", "S")
            return
        
        #### DEBUG DEBUG  DEBUG ########################################################################################

        for tr in probTRList:
            self.createGrid("Fcst", probWEName, "SCALAR", probGridDict[tr]/25.4, tr, precision=2)
            
        for tr in ppffgTRList:
            self.createGrid("Fcst", "ERP", "SCALAR", ppffgGridDict[tr], tr, precision=2)
            
        #### DEBUG DEBUG  DEBUG ########################################################################################
        

        for i in range(len(probTRList)):
            probTR = probTRList[i]
            #print "probTR:", probTR
            # get the EPR grid
            
            # Fetch the erp grids and reference by timeRange (24 hours each)
            # Use the probTR in this loop and timeRange.overlaps to figure out which erp grid to use.
            # All other code should be the same
            ppffgTR = self.getOverlappingTR(probTR, ppffgTRList)
            if ppffgTR is None:
                continue
                        
            ppffgGrid = ppffgGridDict[ppffgTR]  #+  in pct (%)
            # get the probabilistic grid
            pQPFGrid = probGridDict[probTR] / 25.4 # convert mm to inches

            qpfFcstGrid = self.getQPFGrid(probTR)
            
            # Get the maximum of the Fcst QPF and the ERP probabilistic grid
            qpfGrid = np.maximum(qpfFcstGrid, pQPFGrid)
            
            self.createGrid("Fcst", "MaxFcstPQPF", "SCALAR", qpfGrid, probTR, precision=2)

            if ffgGrid is None or qpfGrid is None:
                self.statusBarMsg("FlashFlood or QPF grids missing at timeRange:" +
                                  str(probTR), "S")
                continue
            
            if ppffgGrid is None:
                self.statusBarMsg("ERP grids missing at timeRange:" + str(probTR), "S")
                continue

            tempffgGrid = ffgGrid * 1.0
            tempffgGrid[ffgGrid == 0.0] = 1000.0
            ratioGrid = qpfGrid / tempffgGrid
            ratioGrid[ffgGrid == 0.0] = 0.0
            
#             tempffgGrid = where(equal(ffgGrid, 0.0), float32(1000.0), ffgGrid)
#             ratioGrid = qpfGrid / tempffgGrid
#             ratioGrid[equal(ffgGrid, 0.0)] = 0.0
            
            # Clip the ratioGrid to 8.0 to prevent problems when displaying
            ratioGrid.clip(0.0, 1000.0, ratioGrid)
                        
#             self.createGrid("Fcst", "ERP", "SCALAR", ppffgGrid, probTR,
#                             minAllowedValue = -1, maxAllowedValue=100, precision=2)
            self.createGrid("Fcst", "FFG", "SCALAR", ffgGrid, probTR,
                            minAllowedValue = 0, maxAllowedValue=10, precision=2)
            self.createGrid("Fcst", "QPFtoFFGRatio", "SCALAR", ratioGrid, probTR, 
                            minAllowedValue = 0, maxAllowedValue=1000, precision=2)

            floodThreat = np.zeros(self.getGridShape(), np.int8)
            
            for e in range(len(erps) - 1):
                for r in range(len(ratios) - 1):
                    eMin = erps[e]
                    eMax = erps[e+1]
                    rMin = ratios[r]
                    rMax = ratios[r+1]
                    ratioMask = (ratioGrid >= rMin) & (ratioGrid < rMax)
                    erpMask = (ppffgGrid >= eMin) & (ppffgGrid < eMax)
                    mask = ratioMask & erpMask
                    
#                     ratioMask = logical_and(greater_equal(ratioGrid, rMin),
#                                             less(ratioGrid, rMax))
#                     erpMask = logical_and(greater_equal(ppffgGrid, eMin), 
#                                           less(ppffgGrid, eMax))
#                     mask = logical_and(ratioMask, erpMask)

                    keyIndex = self.getIndex(threatMatrix[r][e], threatKeys)
                    floodThreat[mask] = keyIndex
                    
            # Now set the values we found missing to the None key
            noneIndex = self.getIndex("None", threatKeys)
            floodThreat[missingFFGMask] = noneIndex

            # Create the grid
            self.createGrid("Fcst", "FloodThreat", "DISCRETE",
                        (floodThreat, threatKeys), probTR,
                        discreteKeys=threatKeys,
                        discreteOverlap=0,
                        discreteAuxDataLength=2,
                        defaultColorTable="gHLS_new")        
            
            maxFloodThreat = np.maximum(floodThreat, maxFloodThreat)

        # Make a big timeRange and delete all the FloodingRainThreat grids
        startTime = int(self._gmtime().unixTime()/ 3600) * 3600 - (24 * 3600)
        endTime = startTime + (24 * 3600 * 10)
        dbTR = self.GM_makeTimeRange(startTime, endTime) 
        cTime = int(self._gmtime().unixTime()/ 3600)  * 3600 
        end = cTime + (6*3600)
        threatTR = self.GM_makeTimeRange(cTime, end)     
        self.deleteCmd(['FloodingRainThreat'], dbTR) 
        self.createGrid("Fcst", "FloodingRainThreat", "DISCRETE",
                        (maxFloodThreat, threatKeys), threatTR,
                        discreteKeys=threatKeys,
                        discreteOverlap=0,
                        discreteAuxDataLength=2,
                        defaultColorTable="gHLS_new")
                           
        
        return 
