# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# SelectBreakpoints
#
# March 10, 2020 21020      tlefebvr    Original version
# March 13, 2020 21020      tlefebvr    Added dialog to save edits if cancel
# March 13, 2020 21020      tlefebvr    Added conflict checking with other storms
# March 13, 2020 21020      tlefebvr    Added Storm Number to GUI
# March 20, 2020 21020      tlefebvr    Many changes, primarily changed display to
#                                       paint breakpoints instead of coastal zones.
# March 23, 2020 21020      tlefebvr    Fixed code to paint all breakpoint segments.
# March 23, 2020 21020      tlefebvr    Refactored common code and put in WindWWUtils.
# March 24, 2020 21020      tlefebvr    Tool GUI and dialog now pop up near the cursor.
#                                       Added a bit of extra code to handle the special
#                                       case near the TX/MEXICO border.
# March 26, 2020 21020      tlefebvr    Added new lat/lon format for XML file. Fixed a
#                                       breakpoint segment sorting bug.
# March 27, 2020 21020      tlefebvr    Fixed a couple of bugs related to lat/lon output
# March 30, 2020 21020      tlefebvr    Added general way to connect circular BP sequences
#                                       and change the colors of the hazard buttons to match
#                                       the convention.
# April  7, 2020 21020      tlefebvr    Fixed selection of breakpoints algorithm
# April  8, 2020 21020      tlefebvr    Fixed so zones appear in one and only one hazard.
# April  9, 2020 21020      tlefebvr    Fixed hazard order in makeZoneDict.
# April 20, 2020 22033      tlefebvr    Added EastPac storm bins.
# May    5, 2020 22033      tlefebvr    Added CentralPac storm bins.
# May    6, 2020 22033      tlefebvr    Code clean up.
# May   13, 2020 22033      tlefebvr    Chance missing BP rank to zero instead of None.
# May   13, 2020 22033      tlefebvr    Fixed issue with BP outside domain.
# May   14, 2020 22033      tlefebvr    Modified to use ***Sites methods in WWUTils
# May   16, 2020 22033      tlefebvr    Changed location of breakpoint data files.
# May   18, 2020 22033      tlefebvr    Fixed bugs related to empty breakpoints list.
#                                       Tool now plots all advisories upon exit.
# May   20, 2020 22033      tlefebvr    Addressed code review comments.
# May   29, 2020 22033      tlefebvr    Addressed code review comment.
# June   3, 2020 22033      tlefebvr    Addressed several code review comments.
#
# Author: lefebvre
################################################################################

MenuItems = ["Populate"]

import numpy as np
import AbsTime, TimeRange
import TropicalUtility
import WindWWUtils
import copy
import sys

if sys.version_info.major == 2:
    import Tkinter as tk
else:
    import tkinter as tk

class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._dbss = dbss
        
        # Instantiate the WindWWUtils modules
        self._WindWWUtils = WindWWUtils.WindWWUtils(self._dbss)

    def makeZoneDict(self, bpDict):
        """ 
        Makes a dictionary of {haz : zoneList} based on the bpDict.
        This structure is stored in the JSON file.
        """        
        hazZoneDict = {}
        allZoneList = []
        reverseHazList = copy.copy(self._hazardOrder)
        reverseHazList.reverse() 
        for haz in reverseHazList:
            if haz == "<None>":
                continue
            if haz not in bpDict:
                hazZoneDict[haz] = []
                continue
            
            for bp in bpDict[haz]:
                zoneList = self._WindWWUtils.getBPZones(bpDict, haz)
                if haz not in hazZoneDict:
                    hazZoneDict[haz] = []
                for zone in zoneList:
                    if zone not in hazZoneDict[haz] and zone not in allZoneList:
                        hazZoneDict[haz].append(zone)
                        allZoneList.append(zone)
                        
        return hazZoneDict
    
    def getBPLatLon(self, bpName):
        """
        Fetches the lan/lon based on the breakpoint name
        """
        for bpType, bpDict in self._bpLatLonDict.items():
            for item in bpDict.items():
                gridCell, (name, lat, lon) = item
                if name == bpName:
                    return lat, lon
                       
        self.statusBarMsg(bpName + "not found in Breakpoint Lat/Lon dict.", "S")
        return None, None
    
    def dualBP(self, bpName):
        """
        Returns true in the bpName is a segment (two names)
        """
        return " - " in bpName

    def firstBP(self, bpName):
        """
        Returns the first BP in the specified dual.
        """
        return bpName.split(" - ")[0]

    def lastBP(self, bpName):
        """
        Returns the last BP in the specified dual.
        """
        return bpName.split(" - ")[-1]

    def groupBPSegments(self, segmentList):
        """
        Returns the specified segmentList as a list of segments lists
        grouped by adjacent breakpoint sets.
        """
        if not segmentList:
            return []
        
        segList = []
        groupList = []

        for i, segment in enumerate(segmentList):

            if not self.dualBP(segment):  # Singular BP
                if segList:
                    groupList.append(segList)
                    segList = []
                groupList.append([segment])
            else:  # DualBP
                if i == 0: # Special case for first time
                    segList.append(segment)
                    continue
                
                if self.lastBP(segmentList[i-1]) == self.firstBP(segmentList[i]):
                    segList.append(segment)
                else:
                    groupList.append(segList)
                    segList = [segment]

        if self.dualBP(segmentList[-1]):   # If the last segment is a dual
            groupList.append(segList)   # append the last segList

        return groupList

    def latLonsFromBreakpoints(self, bpDict, haz):
        """ 
        This method returns a list of lat/lon pairs based on the specified
        bpDict and hazard.
        """
        latLonList = []

        if haz not in bpDict:
            return latLonList

        bpList = bpDict[haz]

        singleBPList = []
        for bp in bpList:
            parts = bp.split(" - ")
            for p in parts:
                if p not in singleBPList:
                    singleBPList.append(p)
                
        sortedSegList = self.makeBPSegments(singleBPList)
        
        # Group the segments in lists of contiguous sets
        sortedSegments = self.groupBPSegments(sortedSegList)
                
        hazLatLons = []
        for segmentList in sortedSegments:
            segLatLons = []
            for segment in segmentList:
                bpList = segment.split(" - ")
                for bp in bpList:
                    lat, lon = self.getBPLatLon(bp)
                    if [lat, lon] not in segLatLons:
                        segLatLons.append([lat, lon])
            hazLatLons.append(segLatLons)
            
        return hazLatLons
    
    def makeLatLonDict(self, bpDict):
        """
        Makes a dictionary of lat/lon to store in the JSON file.
        """
        latLonDict = {}
        for haz in self._hazardOrder:
            if haz == "<None>":
                continue
            # Initialize for this hazard
            latLonDict[haz] = self.latLonsFromBreakpoints(bpDict, haz)
            
        return latLonDict

    def saveStormInfo(self, pil):
        """
        Saves the specified stormInfo to the JSON files under pil.
        """
        # Find the dict with the matching pil
        for (stormPil, stormInfo) in self._stormInfoDict.items():
            if stormPil == pil:
                # insert the bpDict                
                stormInfo["Breakpoints"] = self._stormInfoDict[pil]["Breakpoints"]
                zoneDict = self.makeZoneDict(stormInfo["Breakpoints"])
                stormInfo["zoneDict"] = zoneDict

                latLonDict = self.makeLatLonDict(stormInfo["Breakpoints"])
                
                stormInfo["latLonDict"] = latLonDict
                    
                # Use TropicalUtility to save advisories. 
                self._saveAdvisory(pil, stormInfo)
                break

        return
    
    def saveAllStormInfo(self):
        """
        Save all of the stormInfo dicts in the JSON files.
        """
        for adv in self._stormInfoDict:
            self.saveStormInfo(adv)
            
        self._savingNeeded = False
        
        return

    def saveHazards(self):
        """
        Save all edited stormInfo
        """
        self.saveAllStormInfo()
        self.cancelCommand()
        return
    
    def saveNotWanted(self):
        """
        Called when the user discards edits.
        """
        self._savingNeeded = False
        self.cancelCommand()
        return
    
    # Make a temporary dialog to see if the user wants to continue.        
    def dialogPrompt(self):
        """
        Pops a dialog and asks the user if they want to save before exiting.
        """
        self._dialogMaster = tk.Toplevel(self._tkmaster)
        self._dialogMaster.title("Save Edits?")
        self._dialogMaster.attributes("-topmost", True)

        dialogFrame = tk.Frame(self._dialogMaster)
        dialogFrame.grid()
        
        label = tk.Label(dialogFrame, text="Your edits have not been saved.")
        label.grid(row=0, column=0, columnspan=2)
        
        saveButton = tk.Button(dialogFrame, text="Save", command=self.saveHazards, bg="green")
        saveButton.grid(row=1, column=0, padx=20, pady=30)
        saveButton = tk.Button(dialogFrame, text="Discard Edits", command=self.saveNotWanted, bg="red")
        saveButton.grid(row=1, column=1, padx=20, pady=30)
        
        self._savingNeeded = False
        
        self.displayWindowOnCursor(self._dialogMaster)

        tk.mainloop()
        
        return 

    def cancelCommand(self):
        """
        Called when the cancel button is clicked
        """
        self.updateDisplay(plotAllStorms=True) # update with all storms
        if self._savingNeeded:
            self.dialogPrompt()
        try:
            self._tkmaster.destroy()
        except:
            pass
        
    def runCommand(self):
        """
        Called when run is selected. Just saves the stormInfo for the current pil.
        """
        self.saveAllStormInfo()

    def runDismissCommand(self):
        """
        Called when Run/Dismiss button is clicked.
        """
        self.saveAllStormInfo()
        self.cancelCommand()

    def makeBottomButtons(self, frame):
        """
        Create the Execute and Cancel buttons.
        """
        # Make the frame
        self._bottomButtonFrame = tk.Frame(frame, bg=self._bgColor)
        self._bottomButtonFrame.grid(row=5, column=0, pady=20, columnspan=2)

        # Cancel button
        saveColor = "green"
        self._saveButton = tk.Button(self._bottomButtonFrame, text="Save",
                                       command=self.runCommand, bg=saveColor)
        self._saveButton.grid(row=0, column=0, padx=20)
        # Cancel button
        runDismissColor = "lightgreen"
        self._saveDismissButton = tk.Button(self._bottomButtonFrame, text="Save/Dismiss",
                                       command=self.runDismissCommand, bg=runDismissColor)
        self._saveDismissButton.grid(row=0, column=1, padx=20)


        # Cancel button
        cancelColor = "red"
        self._cancelButton = tk.Button(self._bottomButtonFrame, text="Cancel",
                                       command=self.cancelCommand, bg=cancelColor)
        self._cancelButton.grid(row=0, column=2, padx=20)

        return
    
    def bpRank(self, breakpoint):
        """
        A ranking algorithm used to sort the breakpoints.
        """
        
        for bpType in self._bpLatLonDict:
            majorRank = self._bpTypes.index(bpType) + 1
            bpDict = self._bpLatLonDict[bpType]
            for i, item in enumerate(bpDict.items()):
                gridCell, (name, lat, lon) = item
                if name == breakpoint:
                    countryNum = int(self._countryDict[name])
                    return (majorRank * 1000000) + (countryNum) * 1000 + i
        # This should never happen
        print(breakpoint, "not found in bpRank.")
        return 0

    def calcBlobCoords(self, x, y):
        """
        Calculates the coordinates of the square used to indicate the BP location.
        """
        gridShape = self.getGridShape()
        if self._blobSize <= 2:
            return y, y+1, x, x+1
        else:
            inc = int(self._blobSize - 1) // 2
        y0 = y - inc
        y1 = y + inc + 1
        x0 = x - inc 
        x1 = x + inc + 1
        if y0 < 0:
            y0 = 0

        if y1 > gridShape[0] - 1:
            y1 = gridShape[0] - 1
        if x0 < 0:
            x0 = 0
        if x1 > gridShape[1] - 1:
            x1 = gridShape[1] - 1
        return y0, y1, x0, x1

    def getPathMask(self, x1, y1, x2=None, y2=None):
        """
        Calculates the mask made by a linear path from x1, y1 to x2, y2.
        Missing x2, y2 returns the blob for the first grid point.
        """
        mask = self.empty(np.bool)
        
        if not (x2 and y2):
            top, bottom,left, right = self.calcBlobCoords(y1, x1)
            mask[top:bottom, left:right] = True
            return mask
        # Calculate the path between the two points
        dx = x2 - x1
        dy = y2 - y1
        numSteps = max(abs(x2 - x1), abs(y2 - y1))
        if numSteps == 0:
            numSteps = 1
        dx = float((x2 - x1)) / numSteps
        dy = float((y2 - y1)) / numSteps
        for i in range(numSteps):
            x = int(x1 + (i * dx))
            y = int(y1 + (i * dy))
            top, bottom, left, right = self.calcBlobCoords(y, x)
            mask[top:bottom, left:right] = True
        return mask
    
    def getBPGridCell(self, bpName):
        """
        Returns the GFE grid cell corresponding to the specified breakpoint
        """
        for bpType in self._bpLatLonDict:
            for gridCell in self._bpLatLonDict[bpType]:
                name, lat, lon = self._bpLatLonDict[bpType][gridCell]
                if name == bpName:
                    return gridCell
        print("****** Grid cell not found for:", bpName)   
        return None
    
    def getGridCellSequence(self, bpList):
        """
        Returns a sequence of grid coordinates that match the format and 
        location of the specified sequence of breakpoints.
        """
        gridCellList = []
        delimiter = " - "
        
        parsedBPList = []
        for bp in bpList:
            if delimiter in bp:
                parts = bp.split(delimiter)
                for p in parts:
                    if p not in parsedBPList:
                        parsedBPList.append(p)
            else:
                if bp not in parsedBPList:
                    parsedBPList.append(bp)
                    
        segmentList = self.makeBPSegments(parsedBPList)
        
        for segment in segmentList:
            if delimiter in segment:  # two breakpoints
                parts = segment.split(delimiter)
                bp0 =  self.getBPGridCell(parts[0])
                bp1 =  self.getBPGridCell(parts[1])
                if not (bp0 and bp1):
                    continue
                cellTuple = (bp0, bp1)
            else:
                bp = self.getBPGridCell(segment)
                if not bp:
                    continue
                cellTuple = (bp)
            
            gridCellList.append(cellTuple)
            
        return gridCellList
        
    # Updates the GFE spatial display based on the specified list of BP names
    def updateDisplay(self, plotAllStorms = False):
        """
        Update the spatial GFE display based on the current state of the
        stormInfo data.
        """
        hazKeys = self._hazardOrder

        self._bpHazGrid = self.empty(np.int8)
        grid = self.empty(np.int8)
        
        # Define the advisory list based on specified flag
        if plotAllStorms:
            advisoryList = list(self._stormInfoDict.keys())
        else:
            advisoryList = [self._selectedAdvisory]

        for advisory in advisoryList:
            if advisory == "":
                continue
        
            if "Breakpoints" not in self._stormInfoDict[advisory]:
                continue
            
            stormInfoBPKeys = self._stormInfoDict[advisory]["Breakpoints"].keys()
                
            for hazard in self._hazardOrder:
                if hazard not in stormInfoBPKeys:
                    continue
                
                mask = self.empty(np.bool)
    
                bpList = self._stormInfoDict[advisory]["Breakpoints"][hazard]            
                
                gridCellList = self.getGridCellSequence(bpList)
                for points in gridCellList:
                    if isinstance(points[0], tuple):
                        mask |= self.getPathMask(points[0][0], points[0][1],
                                                 points[1][0], points[1][1])
                    else:
                        mask |= self.getPathMask(points[0], points[1])
                                    
                hazIndex = hazKeys.index(hazard)
                grid[mask] = hazIndex
        
        # Create the grid showing the breakpoint areas
        weName = "BreakpointHazards"
        self.createGrid(self.mutableID(), weName, "DISCRETE", (grid, hazKeys), self._timeRange,
                        defaultColorTable="Hazards", discreteKeys=hazKeys,
                        discreteOverlap=1, discreteAuxDataLength=5)
        
        if not self._selectedAdvisory:
            return
        # Plot the storm number label
        stormNum = self._stormInfoDict[self._selectedAdvisory]["stormNumber"]
        labelText = "Storm\nNumber\n" + str(stormNum)
        self._stormLabel.config(text=labelText)
        
        # This is commented out for now as it causes a crash from time to time 
        self.setActiveElement(self.mutableID(), weName, "SFC", self._timeRange)
            
    def makeStormButton(self, frame, label, row, column, active):
        """
        Makes a single storm button. This is implemented as a separate method
        so the lambda method works properly.
        """
        # Set the button state
        state = tk.NORMAL
        if not active:
            state = tk.DISABLED
        # Set the background color
        bgColor = self._unselectedColor
        if label == self._selectedAdvisory:
            bgColor = self._selectedColor
            
        button = tk.Button(frame, text=label,  command=lambda: self.stormButtonSelected(label),
                           font=self._font14Bold, state=state, bg=bgColor, activebackground=bgColor)
        button.grid(row=row, column=column, padx=20, pady=5)
        
        return button

    def makeHazardButton(self, frame, label, row):
        """
        Makes a single hazard button. This is implemented as a separate method
        so the lambda method works properly.
        """
        button = tk.Button(frame, text=label,  command=lambda: self.hazardButtonSelected(label),
                           font=self._font14Bold, bg=self._colors[label])
        button.grid(row=row, padx=20, pady=7)
        return button

    def stormButtonSelected(self, buttonLabel):
        """
        Called when any storm button (advisory) button is selected. Updates the
        internal selectedAdvisory variable and updates the display.
        """
        if self._selectedAdvisory:
            if buttonLabel != self._selectedAdvisory:
                self._advisoryButtons[self._selectedAdvisory].config(bg=self._unselectedColor)
                self._advisoryButtons[self._selectedAdvisory].config(activebackground=self._unselectedColor)
                
        self._advisoryButtons[buttonLabel].config(bg=self._selectedColor)
        self._advisoryButtons[buttonLabel].config(activebackground=self._selectedColor)
            
        self._selectedAdvisory = buttonLabel
        
        self.updateDisplay()
        
        return

    def dumpBPs(self):
        """
        Utility method to dump the contents of the internal stormInfo
        """
        print("-----------StormInfo DUMP-----------------------------------")

        adv = self._selectedAdvisory
        hazards = self._stormInfoDict[adv]["Breakpoints"]
        for haz in hazards:
            for bp in self._stormInfoDict[adv]["Breakpoints"][haz]:
                print(adv, haz, bp)
        print("------------------------------------------------------------")
        return
    
    def maskToBreakpoints(self, mask):
        """ 
        Calculates the set of breakpoint that lie inside the specified mask.
        """
        
        # Mask of breakpoints inside the specified mask
        selectedBPMask = mask & self._breakpointMask
        # The grid coordinates of the above mask
        selectedY, selectedX = np.nonzero(selectedBPMask)

        bpList = []
        for bpType in self._bpLatLonDict:
            for bp in zip(selectedY, selectedX):
                # See if this point is in this breakpoint type
                if bp in self._bpLatLonDict[bpType]:                # See if this point is in this breakpoint type
                    name, lat, lon = self._bpLatLonDict[bpType][bp]
                    bpList.append(name)

        return bpList

    def makeBPSegments(self, bpList):
        """
        Make breakpoint segments based on the specified bpList. Sorts the list and uses
        the ranking algorithm to determine where breakpoint sequences start and stop.
        """
        sortedBPs = sorted(bpList, key=lambda x: self.bpRank(x))
        rankDict = {}
        for bp in sortedBPs:
            rankDict[bp] = self.bpRank(bp)
            
        segmentList = []

        lastBP = ""
        if len(sortedBPs) == 1:
            return sortedBPs

        for bp in sortedBPs:
            # Get the type of BP. Water and island BPs stand on their own
            bpTypeNum = int(rankDict[bp] / 1000000)
            bpType = self._bpTypes[bpTypeNum - 1]

            if bpType in ["island", "water"]:
                segmentList.append(bp)
                lastBP = bp
                continue

            if lastBP == "":
                lastBP = bp
                continue
            
            # It's a land BP so group pairs that are consecutive
            if abs(rankDict[lastBP] - rankDict[bp]) == 1:  # they're consecutive
                segmentName = lastBP + " - "  + bp
                segmentList.append(segmentName)
           
            lastBP = bp
        
        # Places where gaps form because of the way the breakpoints are defined
        # in the *.tbl files.
        bpGaps = [("Mouth of the Rio Grande River", "Barra El Mezquital"),
                  ("Samana", "Cabo Engano"),
                  ("Cabo San Antonio", "Artemisa/Pinar del Rio"),
                  ]
        # Because of the way that the breakpoints are defined in the land.tbl file
        # there are gaps that occur when selecting particular breakpoints that
        # span across borders and end points of large islands.
        # So, check for this case and add a "phantom" segment so this gap is filled.
        for bp1, bp2 in bpGaps:
            if bp1 in bpList and bp2 in bpList:
                segmentList.append(bp1 + " - " + bp2)  # add the segment
                 
        return segmentList

    def hazardButtonSelected(self, hazard):
        """
        Called when the user selects a wind hazard button. Fetches the active area
        and figures out the breakpoints within. Converts those to breakpoint segments,
        removes them from the stormInfo first and then adds them back to ensure each
        segment is assigned to only on hazard. Finally it updates the display.
        """
        ea = self.getActiveEditArea()
        
        # Calculate the mask and 
        mask = self.encodeEditArea(ea)
        if not mask.any():
            self.statusBarMsg("Please select an edit area before assigning a Hazard.", "S")
            return
        bpList = self.maskToBreakpoints(mask)
        
        segmentList = self.makeBPSegments(bpList)
        
        # First check to see if any conflict with existing storms
        if hazard != "<None>":
            if self.anyBreakpointConflicts(segmentList):
                return 

        # Remove any BPs in the existing lists found in with any hazard 
        self.removeBreakpoints(segmentList)        

        # Add the new BPs to the list, unless it's None
        if hazard != "<None>":
            self.addBreakpoints(hazard, segmentList)
            
        self._savingNeeded = True

        # Update the display
        self.updateDisplay()
                
        return
    
    def makeStormNumLabel(self, frame):
        """
        Makes the label to be plotted in the GUI.
        """
        stormNumFrame = tk.Frame(frame)
        stormNumFrame.grid(row=6, column=0, columnspan=2) 
        self._stormLabel = tk.Label(stormNumFrame, text="", font=self._font12Normal)
        self._stormLabel.grid(row=0, column=0, pady=10)
        return
    
    def binButtons(self, siteID):
        
        basins = self._WindWWUtils.forecastBasins(siteID)
        return self._WindWWUtils.basinBins(basins)

    def makeStormButtons(self, frame):
        """
        Makes the advisory buttons on the GUI.
        """
        siteID = self.getSiteID()
        allButtonLabels = self.binButtons(siteID)
        if not allButtonLabels:
            return

        for column, buttonLabels in enumerate(allButtonLabels):
            for i, label in enumerate(buttonLabels):
                activeButton = label in self._advisoryNames
                row = i
                self._advisoryButtons[label] = self.makeStormButton(frame, label, row, column,
                                                                    activeButton)
        return 
    
    def makeWindHazardButtons(self, frame):
        """
        Makes the wind hazard buttons on the GUI.
        """
        buttonLabels = ["HU.W", "HU.A", "TR.W^HU.A", "TR.W", "TR.A", "<None>"]
        for i, label in enumerate(buttonLabels):
            self.makeHazardButton(frame, label, i)
        
        return
    
    def removeBreakpoints(self, bpList):
        """
        Removes the specified breakpoints from the stormInfoDict.
        """ 
        bpDict = self._stormInfoDict[self._selectedAdvisory]["Breakpoints"]

        for hazard in self._hazardOrder:
            if hazard == "<None>":
                continue
            for bp in bpList:
                if bp in bpDict[hazard]:
                    # Remove the Breakpoint segment
                    bpDict[hazard].remove(bp)
        return
    
    def addBreakpoints(self, hazard, bpList):
        """
        Adds the specified breakpoints to the stormInfoDict.
        """
        # Fetch the current set of BPs
        currentBPs = self._stormInfoDict[self._selectedAdvisory]["Breakpoints"].get(hazard, None)
        if currentBPs is None:
            currentBPs = []
        # Add the new BPs
        for bp in bpList:
            if bp in currentBPs:
                continue
            currentBPs.append(bp)
        
        # Replace the old list with this one.
        self._stormInfoDict[self._selectedAdvisory]["Breakpoints"][hazard] = currentBPs
        
        return
    
    def anyBreakpointConflicts(self, addedBPSegments):
        """
        Returns True if any of the specified added segments also exist in another storm.
        """
        
        for advisory in self._stormInfoDict:
            if advisory == self._selectedAdvisory:
                continue
            bpDict = self._stormInfoDict[advisory]["Breakpoints"]
            for hazard in bpDict:
                bpList = bpDict[hazard]
                for bp in addedBPSegments:
                    if bp in bpList:
                        self.statusBarMsg(bp + " conflicts with breakpoints in advisory: " + advisory + " for hazard: " + hazard, "S")
                        return True
        return False
        
    def displayWindowOnCursor(self, master):
        """
        Moves the specified window to the curso location.
        """
        master.update_idletasks()
        wh= master.winfo_height()
        ww= master.winfo_width()
        px, py = master.winfo_pointerxy()
        master.geometry("%dx%d+%d+%d" % (ww, wh, px - (ww //2 ),py - (wh // 2)))
        return

    def setUpUI(self):
        """
        Makes the tk calls to set up the GUI.
        """

        self._tkmaster = tk.Tk()
        self._master = tk.Toplevel(self._tkmaster)
        self._dialogMaster = None

        self._master.title("Select Breakpoint Hazards")
        self._master.attributes("-topmost", True)

        # Capture the "x" click to close the GUI
        self._master.protocol('WM_DELETE_WINDOW', self.cancelCommand)

        self._topFrame = tk.Frame(self._master)
        self._topFrame.grid()
        self._tkmaster.withdraw() # remove the master from the display
        
        self._advisoryButtons = {}
        stormFrame = tk.Frame(self._master, relief=tk.GROOVE, bd=3)
        stormFrame.grid(row=0, column=0, padx=20, pady=20)
        
        self.makeStormButtons(stormFrame)
        self.makeStormNumLabel(stormFrame)
        
        hazFrame = tk.Frame(self._master, relief=tk.GROOVE, bd=3)
        hazFrame.grid(row=0, column=1, padx=20, pady=20)
        self.makeWindHazardButtons(hazFrame)
        # Make the Run, Run/Dismiss, Cancel buttons
        bottomButtonFrame = tk.Frame(self._master, relief=tk.GROOVE, bd=3)
        bottomButtonFrame.grid(row=5, columnspan=2, pady=20)
        self.makeBottomButtons(bottomButtonFrame)
        
        self.updateDisplay()

        return
    
    def getAdvisoryNames(self):
        """
        Picks the first active advisory for this site.
        """
        binList = self.binButtons(self.getSiteID())
        
        nameList =[]
        for advisory in self._stormInfoDict:
            if advisory in binList:
                nameList.append(advisory)
        return nameList

    def execute(self):
        """
        Main method to start the tool.
        """
        # set up some constants for this tool
        self._font12Normal = "Helvetica 12 normal"
        self._font14Bold = "Helvetica 14 bold"
        self._bgColor = "#d9d9d9"
        self._selectedColor = "green"
        self._unselectedColor = "gray80"
        # List of tropical wind hazards in increasing order of severity
        self._hazardOrder =  ["<None>", "TR.A", "HU.A", "TR.W", "TR.W^HU.A", "HU.W"]
        
        self._colors = {"HU.W" : "red",
                        "HU.A" : "pink",
                        "TR.W^HU.A" : "purple",
                        "TR.W" : "#0092FF", # not too dark blue
                        "TR.A" : "yellow",
                        "<None>" : 'white'
                        }

        self._bpTypes = ["land", "island", "water",
                         ]
        
        #  Path for the breakpoint tables.
        path = "/localapps/runtime/RecommendWindWatchWarning/"

        self._filePaths = {
                           "land" : path + "tcabkpt_land.tbl",
                           "island" : path + "tcabkpt_island.tbl",
                           "water" : path + "tcabkpt_water.tbl",
                           }
        
        self._blobSize = 5

        # Make a timeRange used for displaying the grid, one day long starting now.
        start = int((self._gmtime().unixTime()) / 3600) * 3600 - (6 * 3600)
        end = start + 24 * 3600
        self._timeRange = TimeRange.TimeRange(AbsTime.AbsTime(start),
                                              AbsTime.AbsTime(end))
        
        # Fetch the storm information from the JOSN files.
        self._stormInfoDict = self._WindWWUtils.fetchStormInfo(self._hazardOrder)

        # Fetch the active advisory names from the JSON files.
        self._advisoryNames = self.getAdvisoryNames()
        if not self._advisoryNames:
            self.statusBarMsg("No Advisory files found. Please run StormInfo first.", "U")
            return
                
        # Set the default selectedAdvisory
        self._selectedAdvisory = self._advisoryNames[0]
            
        self._savingNeeded = False
               
        self._countryDict = {}
        self._bpLatLonDict, self._countryDict = \
            self._WindWWUtils.createBreakpointsDict(self._filePaths)
        
        # Create a mask consisting of the locations of the breakpoints.
        # This helps later when we need to find the breakpoints inside
        # the selected edit area.
        
        self._breakpointMask = self.empty(np.bool)
        for bpDict in self._bpLatLonDict.values():
            for gridCell in bpDict:        
                self._breakpointMask[gridCell] = True
        
        self.setUpUI()
        self.displayWindowOnCursor(self._master)
        tk.mainloop()

        return

