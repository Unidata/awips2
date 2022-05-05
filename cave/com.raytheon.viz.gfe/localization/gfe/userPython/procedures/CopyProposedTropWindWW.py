## ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.

#
# CopyProposedTropWindWW
#
# This procedure reads the JSON files and crate a Hazard-like grid for viewing.
# The displayed hazards are clipped to these areas for display.
#
# Author: lefebvre
#
# March 18 2020 21020      tlefebvr    Initial version.
# March 29 2020 21020      tlefebvr    Added ETNs to hazard keys.
# April  6 2020 21020      tlefebvr    Fixed ETN issue and added code to read
#                                      JSON file from a the text db.
# April  9 2020 21020      tlefebvr    Added GUI to prompt for bulletins and strip
#                                      AFOS header off of text product.
# April 13 2020 21020      tlefebvr    Added EastPac button and more error messaging
#                                      when failing to get or decode text product.
# April 16, 2020 21020      tlefebvr   Fixed filterKeys for calcDiffGrid and call
#                                      calcDiffGrid. Added removal of Prop grids.
# April 22, 2020 21020      tlefebvr   Fixed ETNs so it works in all basins.
# May    6, 2020 21020      tlefebvr   Ported to HPA domain and code clean-up.
# May   12, 2020 22033      tlefebvr   Changed TimeRange for Diff grid to 48 hours.
# May   13, 2020 22033      tlefebvr   Limited diff grid to CWA area if it's a WFO.
# May   15, 2020 22033      tlefebvr   Fixed issue with empty JSON (no W/Ws).
# May   21, 2020 22033      tlefebvr   Addressed code review comments.
# May   25, 2020 22033      tlefebvr   Fixed a small bug introduced with previous version.
# May   26, 2020 22033      tlefebvr   Using subprocess to fetch text from textdb.
# June   3, 2020 22033      tlefebvr   Fixed SiteID comparision to lists.
# June   3, 2020 22033      tlefebvr   Removed explicit call to get text in PRACTICE mode. 
################################################################################

MenuItems = ["Populate"]

import AbsTime, TimeRange
import TropicalUtility
import WindWWUtils
import numpy as np
import ProcessVariableList
import functools
import json
import operator

class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._dbss = dbss

        self._WindWWUtils = WindWWUtils.WindWWUtils(self._dbss)

    def getZoneList(self, bulletin, hazard):
        """
        Fetches the list of zones for the specified bulletin and hazard.
        """
        zoneList = []
        if "zoneDict" in self._stormInfoDict[bulletin]:
            if hazard in self._stormInfoDict[bulletin]["zoneDict"]:
                zoneList = self._stormInfoDict[bulletin]["zoneDict"][hazard]

        return zoneList
    
    def getStormNumber(self, bulletin):
        """
        Fetches the storm number from the specified bulletin.
        """
        stormNumber = None
        if bulletin in self._stormInfoDict:
            stormNumber = self._stormInfoDict[bulletin]["stormNumber"]
        
        return stormNumber
    
    def addETNToHazardKey(self, hazKey, etnStr):
        """
        Adds the ETN string to the specified hazKey
        """
        hazList = hazKey.split("^")
        hazList = [hazKey + ":" + etnStr for hazKey in hazList]
        newHaz = "^".join(hazList)

        return newHaz
            
    # Updates the GFE spatial display based on the specified list of BP names
    def updateDisplay(self):
        """
        Updates the spatial GFE display based on the current state of the
        stormInfo data.
        """
        hazKeys = ["<None>"]

        etnDict = self._WindWWUtils.etnDict()

        grid = self.empty(np.int8)
        # Assign the values to the grid for the CONUS breakpoints`
        for bulletin in self._bulletinList:
            stormNumber = self.getStormNumber(bulletin)
            if stormNumber is None:
                continue
            
            for hazard in self._fullHazList:
                if hazard == "<None>":
                    continue
                hazMask = self.empty(np.bool)
                
                eaList = self.getZoneList(bulletin, hazard)
                
                for eaName in eaList:

                    try:
                        ea = self.getEditArea(eaName)
                        hazMask |= self.encodeEditArea(ea) & self._cwaMask
                    except AttributeError:
                        self.statusBarMsg(eaName + " edit area was was not found.", "S")
                        continue
                
                etnValue = etnDict[bulletin[0:2]]
                etnStr = str(etnValue + stormNumber)
                hazardKey = self.addETNToHazardKey(hazard, etnStr)
                hazIndex = self.getIndex(hazardKey, hazKeys)
                grid[hazMask] = hazIndex
        
        # Create the grid showing the breakpoint areas
        weName = "ProposedTropWindWW"
        self.createGrid(self.mutableID(), weName, "DISCRETE", (grid, hazKeys), self._timeRange,
                        defaultColorTable="Hazards", discreteKeys=hazKeys,
                        discreteOverlap=1, discreteAuxDataLength=5)
        
        # This is commented out for now as it causes a crash from time to time 
#         self.setActiveElement(self.mutableID(), weName, "SFC", self._timeRange)
        return grid, hazKeys
    
    #  Calculates a difference grid (added versus removed)
    def calcDiffGrid(self, initialGrid, proposedGrid, diffName, timeRange, 
                     filterKeys=["HU.W", "HU.A", "TR.W", "TR.A", "TR.W^HU.A", "HU.A^TR.W"],
                     isWFO=False):
        """
        Calculate a temporary grid that shows the difference between the initial discrete
        grid and the the proposedGrid. -1=hazard removed, 0=no change, 1=hazard added,
        2=hazard changed. 
        """
        # Limit all changes to the areaMask
        areaMask = self.newGrid(True, np.bool)
        #  If this is a WFO
        if isWFO:            
            #  Filter the Hazards to only keep the Wind hazards        
            initialGrid = self.filterHazardGrid(initialGrid, filterKeys)
            proposedGrid = self.filterHazardGrid(proposedGrid, filterKeys)
            areaMask = self._cwaMask
        #  Split these grids into their components
        initGrid, initKeys = initialGrid
        propGrid, propKeys = proposedGrid

        #  Identify where there are no hazards in both grids
        initNone = self.getIndex("<None>", initKeys)
        propNone = self.getIndex("<None>", propKeys)

        #  Mask of these areas
        initNoneMask = (initGrid == initNone)
        propNoneMask = (propGrid == propNone)

        #  Make an empty grid to hold difference indicator
        diffGrid = self.empty(np.float32)
        
        # Calculate hazards that were removed
        diffGrid[propNoneMask & ~initNoneMask & areaMask] = -1

        # Calculate hazards that were added
        diffGrid[~propNoneMask & initNoneMask & areaMask] = 1
        
        # Find areas that had some hazard and it changed to another hazard
        for initKey in initKeys:
            for propKey in propKeys:
                if initKey == "<None>" or propKey == "<None>":   # ignore any <None> cases
                    continue
                if initKey == propKey: # ignore cases where the keys are the same
                    continue
                
                # Now we know the keys are different and neither is <None>
                initIndex = self.getIndex(initKey, initKeys)
                propIndex = self.getIndex(propKey, propKeys)

                initMask = (initGrid == initIndex)
                propMask = (propGrid == propIndex)
                
                # The intersection is where they changed
                diffGrid[initMask & propMask & areaMask] = 2

        #  Add this temporary grid to the grid manager so it can be seen
        self.createGrid(self.mutableID(), diffName, "SCALAR", diffGrid, timeRange,
                descriptiveName="Diff Between NHC and WFO",
                precision=0, minAllowedValue=-1.0, maxAllowedValue=2.0)
    
    def extractJSONText(self, textList):
        """
        Strip the header text from the text product.
        """
        startStr = "{\n"
        if startStr in textList:
            startIndex = textList.index(startStr)
            finalText = textList[startIndex:]
        else:
            self.statusBarMsg("Error parsing JSON text from text product.", "S")
            finalText = ""

        return finalText
    
    def fetchStormInfoFromTextProduct(self):
        """
        Fetch the text product from the text database,
        convert the text to a dictionary and return in 
        a dictionary indexed by bulletin.
        """
        
        stormInfoDict = {}
        
        for bulletin in self._bulletinList:
            productID = "MIAJSN" + bulletin
            
            text = self.getTextProductFromDB(productID)
            if text:
                # Make a single string out of lists of strings
                text = functools.reduce(operator.concat, text)
            else:
                continue
            
            text = self.extractJSONText(text)
          
            jsonDict = json.loads(text)
            if jsonDict is None:
                continue
            
            stormInfoDict[bulletin] = jsonDict
                    
        return stormInfoDict
    
    def removeOldPropGrids(self):
        """
        Removes all previous Proposed grids.
        """
        timeRange = TimeRange.allTimes()
        weList = ["ProposedTropWWGuidance", "ProposedTropWindWW", "WindHazardsDiff"]
        for weName in weList:
            self.deleteGrid(self.mutableID(), weName, "SFC", timeRange)
            
    def noHazardsFound(self, stormInfoDict, bulletinList):
        """
        Returns true if no hazards were found in list of bulletins.
        """
        for bulletin in bulletinList:
            if bulletin in stormInfoDict:
                if "Breakpoints" not in stormInfoDict[bulletin]:
                    return True

                for phenSig in stormInfoDict[bulletin]["Breakpoints"]:
                    if "Breakpoints" in stormInfoDict[bulletin]:
                        if len(stormInfoDict[bulletin]["Breakpoints"]) > 0:
                            return False
        return True

    # Main method that sets up the GUI and enters the event loop
    def execute(self, varDict):

        self._fullHazList =  ["<None>", "HU.A", "HU.W", "TR.A", "TR.W", "HU.A^TR.W"]
        
        self._bpTypes = ["land", "island", "water",
                         ]
        basinDict = self._WindWWUtils._basinBins
        
        siteID = self.getSiteID()

        if siteID in ["HFO"]:
            binInfo = [("Central Pacific Storm\nBin Number:", basinDict["Central Pacific"])]
        elif siteID in ["GUM", "PQE", "PQW"]:
            binInfo = [("Western Pacific Storm\nBin Number:", basinDict["Western Pacific"])]
        else:
            binInfo = [("Atlantic Storm\nBin Number:", basinDict["Atlantic"]),
                       ("Eastern Pacific Storm\nBin Number:", basinDict["Eastern Pacific"]),
                       ]

        variableList = []
        # Build the GUI
        for basinName, binList in binInfo:
            variableList.append((basinName, [], "check", binList))        
        # Display the GUI
        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList("Select Bins", variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            self.cancel()
        #Extract the selections
        self._bulletinList = []
        for basinName, binList in binInfo:
            self._bulletinList += varDict[basinName]
                
        if not self._bulletinList:
            self.statusBarMsg("Please select at least one bulletin.", "S")
            return

        self.removeOldPropGrids()

        # Get the WFO edit area and mask
        cwaEA = self.getEditArea(siteID)
        self._cwaMask = self.encodeEditArea(cwaEA)
                        
        # Make a timeRange used for displaying the grid, one day long starting now.
        start = int((self._gmtime().unixTime()) / 3600) * 3600
        end = start + 48 * 3600
        self._timeRange = TimeRange.TimeRange(AbsTime.AbsTime(start),
                                              AbsTime.AbsTime(end))

        
        self._stormInfoDict = self.fetchStormInfoFromTextProduct()
        
        if self.noHazardsFound(self._stormInfoDict, self._bulletinList):
            self.statusBarMsg("No Hazards found in the selected bulletin(s)", "S")
            return
         
        # Make the Proposed gird, display it and return it
        propGrid, propKeys = self.updateDisplay()
        
        # Fetch the Hazards grid
        trList = self.GM_getWEInventory("Hazards", self.mutableID())
        if trList:
            hazTR = trList[-1]
        else:   # No hazard grid found
            self.statusBarMsg("No Hazards grid was found. No difference grid made.", "S")
            return
        
        # Make the difference grid
        hazGrid, hazKeys = self.getGrids(self.mutableID(), "Hazards", "SFC", hazTR)
        
        self.calcDiffGrid((hazGrid, hazKeys), (propGrid, propKeys), "WindHazardsDiff", self._timeRange, self._fullHazList, isWFO=True)


