# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# DefineBreakpoints
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# May  9, 2019 21020      tlefebvr    Original version
# May 16, 2019 21020      tlefebvr    Code review changes
# May 28, 2019 21020      tlefebvr    Fixed issue when no advisories available
# May 31, 2019 21020      tlefebvr    Fixed Zone display issues and improved
#                                     performance when updating.
# Aug. 2, 2019 21020      tlefebvr    Clean-up based on code review.
# Aug. 14,2019 21020      tlefebvr    Implemented contiguous deselection.
#                                     Refactored updates to zone display for 
#                                     better performance. 
# Aug. 22, 2019 21020     tlefebvr    Code Review changes
# Sep. 18, 2019 21020     tlefebvr    Final Code review changes
################################################################################

MenuItems = ["Populate"]

VariableList = []


import time
import tkinter as tk

import numpy as np

import AbsTime
import TimeRange
import TropicalUtility
import WindWWUtils
import ZoneMap


class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._dbss = dbss

    # Saves the specified stormInfo to the JSON files under pil.
    def saveStormInfo(self, pil):
        # Find the dict with the matching pil
        for (stormPil, stormInfo) in self._stormInfoDicts.items():
            if stormPil == pil:
                # insert the bpDict
                stormInfo["Breakpoints"] = self.makeBPDict()
                zoneDict = self.makeZoneDict(stormInfo["Breakpoints"])
                stormInfo["zoneDict"] = zoneDict
                # Use TropicalUtility to save advisories. 
                self._saveAdvisory(pil, stormInfo)
                break

        return

    # Returns the list of zones for the specified bpDict and hazard.
    def getBPZones(self, bpDict, haz):
        bpList = bpDict[haz]
        zoneList = []
        for bp in bpList:
            zoneList = zoneList + self._bpDict[bp]

        return zoneList

    # Makes a dictionary of {haz : zoneList} based on the bpDict.
    def makeZoneDict(self, bpDict):
        hazZoneDict = {}
        for haz in self._hazList:
            bpList = bpDict[haz]
            for bp in bpList:
                hazZoneDict[haz] = self.getBPZones(bpDict, haz)
        return hazZoneDict

    # Makes a dictionary of {haz : breakpoint} list.
    def makeBPDict(self):
        hazBPDict = {}
        for haz in self._hazList:
            hazBPDict[haz] = self.makeBreakpointList(haz)
        return hazBPDict

    # Looks up the breakpoint name and returns the corresponding zoneList.
    def breakpointToZoneList(self, bpName):
        try:
            return self._bpDict[bpName]
        except KeyError:
            print ("Unknown breakpoint", bpName, "passed to breakpointToZoneList.")
        return []

    # Called when the cancel button is clicked
    def cancelCommand(self):
        self._tkmaster.destroy()

    # Called when run is selected. Just saves the stormInfo for the current pil.
    def runCommand(self):
        self.saveStormInfo(self._selectedPil)

    # Called when Run/Dismiss button is clicked.
    def runDismissCommand(self):
        self.saveStormInfo(self._selectedPil)
        self.cancelCommand()

    # Create the Execute and Cancel buttons
    def makeBottomButtons(self, frame):

        # Make the frame
        self._bottomButtonFrame = tk.Frame(frame, bg=self._bgColor)
        self._bottomButtonFrame.grid(row=2, column = 0, columnspan=6, pady=20)

        # Run button
        self._runButton = tk.Button(self._bottomButtonFrame, text="Save",
                                    command=self.runCommand)
        self._runButton.grid(row=0, column=0, padx=20)

        # Run/Dismiss button
        self._runDismissButton = tk.Button(self._bottomButtonFrame, text="Save/Dismiss",
                                           command=self.runDismissCommand)
        self._runDismissButton.grid(row=0, column=1, padx=20)
        # Cancel button
        self._cancelButton = tk.Button(self._bottomButtonFrame, text="Cancel",
                                       command=self.cancelCommand)
        self._cancelButton.grid(row=0, column=2, padx=20)

        return

    # Returns the starting breakpoint name, if it's a segment. For singles, just return
    # what is passed it.
    def startBP(self, bpName):
        bpName = bpName.strip()
        parts = bpName.split(" - ")

        bp = parts[0].strip()
        pos = bp.find("(")
        if pos > -1:
            bp = bp[0:pos]

        return bp 

    # Returns the ending breakpoint name, if it's a segment. For singles, just return
    # what is passed it.
    def endBP(self, bpName):
        bpName = bpName.strip()
        parts = bpName.split(" - ")
        if len(parts) < 2:
            return None
        bp = parts[-1].strip()
        pos = bp.find(")")
        if pos > -1:
            bp = bp[0:pos]

        return bp

    # Returns true if the specifed bp is a single (no " - ")
    def singleBP(self, bpName):

        return " - " not in bpName

    # Find the index where the startBP is the bpStartName
    def findStartBPIndex(self, bpList, bpStartName, availableIndexes):
        for i in range(len(bpList)):
            if not availableIndexes[i]:
                continue
            start = self.startBP(bpList[i])
            if start is None:
                return None
            elif start == bpStartName:
                return i
        return None

    # Sorts breakpoints into a list of linked breakpoints. Where
    # one BP ends and another starts is a link. This code will 
    # sort the BPs into a list of linked BPs and a list of single
    # BPs (orphans).
    def sortBPKeys(self, bpList):

        # Finds the BP index whose start matches the currentIndex end
        def findLinkedIndex(currentIndex, availableIndexes):
            target = self.endBP(bpList[currentIndex])
            for i in range(len(bpList)):
                if not availableIndexes[i]:
                    continue
                if self.singleBP(bpList[i]):
                    continue

                start = self.startBP(bpList[i])
                if start == target:
                    return i
            return None

        # Finds the next available index that has not yet been used.
        def findNextAvailableIndex(availableIndexes):
            availArray = np.argwhere(availableIndexes)
            if len(availArray) > 0:
                return availArray[0][0]

            return None

        availableIndexes = np.ones(len(bpList), np.bool) # Keeps track of used list entries

        finalList = []
        orphanList = []

        bpIndex = 0
        # Loop over the whole list
        innerList = [bpList[bpIndex]] # initialize with the first BP
        availableIndexes[bpIndex] = False

        while availableIndexes.any():

            nextIndex = findLinkedIndex(bpIndex, availableIndexes)
            if nextIndex is None:  # end of the sequence
                if len(innerList) == 1:
                    orphanList.append(bpList[bpIndex])
                else:  # longer than 1
                    finalList.append(innerList) # ordinary end of sequence

                availableIndexes[bpIndex] = False
                bpIndex = findNextAvailableIndex(availableIndexes)
                if bpIndex is None:
                    break
                innerList = [bpList[bpIndex]]  # re-init the innerList

            else:  # found a next index
                innerList.append(bpList[nextIndex])
                availableIndexes[bpIndex] = False
                bpIndex = nextIndex
                availableIndexes[bpIndex] = False

        return finalList, orphanList

    # Returns the listbox that corresponds to the specified event.
    def eventToListBox(self, event):

        for haz in self._hazList:
            if self._listBoxWidgets[haz] == event.widget:
                return haz, self._listBoxWidgets[haz]
        return None

    # Paints the listbox entries various colors depending on their state.
    def paintItems(self, hazard, item):
        
        for haz in self._hazList:
            # The selected item is already painted by Tk
            if haz == hazard:
                continue
            
            if self.allowedComboException(haz, hazard):
                if item in self._listBoxWidgets[haz].curselection():
                    # The other hazard is selected so, paint show both as selected
                    self._listBoxWidgets[haz].itemconfig(item, selectbackground="purple", foreground="black")
                    self._listBoxWidgets[hazard].itemconfig(item, selectbackground="purple", foreground="black")
                    continue
            
            backColor = self._fadedColors[hazard]
            foreColor = 'gray50'
            self._listBoxWidgets[haz].itemconfig(item, background=backColor, foreground=foreColor)

    # Clears the items in the listbox, depending on their state.
    def clearItems(self, hazard, item):
        backColor = "white"
        foreColor = "black"
        # First figure out what color the items should be if the hazard is one of the exceptions
        if hazard in self._allowedHazCombos:
            # See if the other hazard has the same selected item.
            otherHaz = self._allowedHazCombos[0]
            if hazard == self._allowedHazCombos[0]:
                otherHaz = self._allowedHazCombos[1]
            selItems = self._listBoxWidgets[otherHaz].curselection()
            # If it's selected, set the colors differently
            if item in selItems:
                backColor = self._fadedColors[otherHaz]
                foreColor = "gray50"
        for haz in self._hazList:

            if self.allowedComboException(haz, hazard):
                if item in self._listBoxWidgets[haz].curselection():
                    # different colors for this case
                    bColor = self._boldColors[haz]
                    self._listBoxWidgets[haz].itemconfig(item, selectbackground=bColor, foreground="black")
                    self._listBoxWidgets[hazard].itemconfig(item, background=backColor, foreground="black")

            self._listBoxWidgets[haz].itemconfig(item, background=backColor, foreground=foreColor)

    # Returns true if the combination of hazards is allowed to occur at the same time.
    def allowedComboException(self, haz1, haz2):

        return haz1 in self._allowedHazCombos and haz2 in self._allowedHazCombos

    # Returns the item that was selected based on the previous selections
    def selectedItem(self, previous, current):
        for c in current:
            if c not in previous:
                return c
        return None

    # Returns the item that was deselected based on the previous selections
    def deselectedItem(self, previous, current):
        for p in previous:
            if p not in current:
                return p
        return None

    def selectListBoxItem(self, listBox, haz, selectedItem):

        if self.isAvailable(haz, selectedItem):
            self.paintItems(haz, selectedItem)
            self._lastItemSelected = selectedItem
        else:
            # De-select the item since it's already taken
            listBox.selection_clear(selectedItem)
        return

    def deselectListBoxItem(self, listBox, haz, deselectedItem):
        listBox.selection_set(deselectedItem)
        self.clearItems(haz, deselectedItem)
        listBox.selection_clear(deselectedItem)
        self._lastItemDeselected = deselectedItem

        return

    # Determine if this hazards can be selected.
    def isAvailable(self, hazard, item):
        for haz in self._hazList:
            if haz == hazard:
                continue

            if self.allowedComboException(haz, hazard):
                continue

            hazSelections = self._listBoxWidgets[haz].curselection()
            if item in hazSelections:
                return False

        return True

    #Returns true if the shift button was down for the specified event.
    def shiftButtonDown(self, event):
        return event.state & 0x0001

    # A selection with a shift down is the contiguous selection event
    # where the tool automatically selects the items from the previous
    # selection to the current selection. The Tk EXTENDED mode only
    # allows one contiguous set at a time, which is a requirement,
    # so a bit of extra code to figure out what was clicked and then
    # select every item from start to end.
    # Figure out the start and end items and select those.
    def processContiguousSelection(self, event, listBox, haz):
        # A contiguous click must be in the same hazard as the previous click
        if self._lastHazSelected is None or haz != self._lastHazSelected:
            return

        if self._lastItemSelected is not None:
            lastItem = self._lastItemSelected
        elif self._lastItemDeselected is not None:
            lastItem = self._lastItemDeselected
        else:  # neither were previously selected
            return

        nearest = listBox.nearest(event.y)
        start = min(nearest, lastItem)
        end = max(nearest, lastItem) + 1
        # Disable the contiguous selection for islands
        if start >= self._islandSeparatorItem or end-1 >= self._islandSeparatorItem:
            return

        for i in range(start, end):
            if self._lastItemSelected is not None:
                if self.isAvailable(haz, i):
                    self.selectListBoxItem(listBox, haz, i)
                    listBox.selection_set(i)
                    self.updateZoneDisplay(haz, i, True) # add the hazard
            else:
                self.deselectListBoxItem(listBox, haz, i)
                self.updateZoneDisplay(haz, i, False) # remove the hazard
        self._lastHazSelected = None
        self._lastItemSelected = None

        # Update the breakpoint grid
        self.displayZones()

        return

    def listBoxSelected(self, event):

        haz, listBox = self.eventToListBox(event)

        selItems = listBox.curselection()
        
        # If a separator was selected, deselect and return 
        if self._islandSeparatorItem in selItems:
            listBox.selection_clear(self._islandSeparatorItem)
            return

        # Find the item that was selected or de-selected.
        deselected = self.deselectedItem(self._hazSelections[haz], selItems)
        selected = self.selectedItem(self._hazSelections[haz], selItems)

        if self.shiftButtonDown(event):
            self.processContiguousSelection(event, listBox, haz)
        else:
            if selected is not None:
                self.selectListBoxItem(listBox, haz, selected)
                self.updateZoneDisplay(haz, selected, True) # add the hazard
                self._lastItemSelected = selected
                self._lastItemDeselected = None
            elif deselected is not None:
                self.deselectListBoxItem(listBox, haz, deselected)
                self.updateZoneDisplay(haz, deselected, False) # add the hazard
                self._lastItemDeselected = deselected
                self._lastItemSelected = None

            # Update the breakpoint grid
            self.displayZones()

        self._lastHazSelected = haz
        self._hazSelections[haz] = listBox.curselection() # save the current selections

        self.updateBreakpointDisplay()
        return

    # Makes the listbox GUI for a hazard.
    def makeListBox(self, frame, bpList, orphanedBPs, label, mode):

        # Make the listbox. A strange interaction with X windows causes other listbox
        # selections to be unselected when any listbox selection is made. The
        # "exportselection = 0" flag fixes this annoyance.
        color = self._boldColors[label]
        self._listBoxWidgets[label] = tk.Listbox(frame, selectmode=mode, height=20, width=35,
                                                 selectbackground=color, exportselection=0)
        self._listBoxWidgets[label].grid(padx=5, pady=5)

        for bp in bpList:
            self._listBoxWidgets[label].insert(tk.END, bp)

        # Add in the separator before the list of islands
        self._listBoxWidgets[label].insert(tk.END, self._islandSeparator)
        self._islandSeparatorItem = self._listBoxWidgets[label].size() - 1

        # Add in the islands and other orphans
        for bp in orphanedBPs:
            self._listBoxWidgets[label].insert(tk.END, bp)
        self._listBoxWidgets[label].grid(row=0, column=0)

        self._listBoxWidgets[label].grid(row=0, column=0)
        self._listBoxWidgets[label].bind("<<ListboxSelect>>", self.listBoxSelected)
        self._listBoxWidgets[label].bind("<Shift-Button-1>", self.listBoxSelected)

        scrollbar = tk.Scrollbar(frame)
        scrollbar.grid(row= 0, column=1, sticky=tk.NS)
        scrollbar.config(command=self._listBoxWidgets[label].yview)

        self._listBoxWidgets[label].config(yscrollcommand=scrollbar.set)

        label = tk.Label(frame, text=self._listLabels[label], font=self._activeFont)
        label.grid(row=1)

        return

    # Returns the list of selected breakpoints.
    def makeBreakpointList(self, haz):
        selectedItems = self._listBoxWidgets[haz].curselection()
        bpList = []
        for s in selectedItems:
            bpList = bpList + [self._listBoxWidgets[haz].get(s)]

        return bpList

    # Returns a list of tuples that represent contiguous BP segments
    def getContiguousBPSegments(self, listBox):
        bpSegments = []

        selectedItems = listBox.curselection()
        if len(selectedItems) == 0:
            return bpSegments

        start = selectedItems[0]
        end = selectedItems[0]
        for item in range(1, len(selectedItems)):
            i = selectedItems[item]

            if i - end == 1:
                end = i
                continue
            else:
                bpSegments.append((start, end))
                start = i
                end = i

        bpSegments.append((start, end))

        return bpSegments

    # Makes the string that gets displayed in the GUI
    def makeBPString(self, haz):

        listBox = self._listBoxWidgets[haz]
        bpSegments = self.getContiguousBPSegments(listBox)
        bpList = []
        finalStr = ""
        for start, end in bpSegments:
            if start >= self._islandSeparatorItem:  # an island item
                for i in range(start, end+1):
                    bpList.append(listBox.get(i))
            else:  # A regular item
                startBP = self.startBP(listBox.get(start))
                endBP = self.endBP(listBox.get(end))
                if startBP is not None and endBP is not None:
                    bpList.append(startBP + "-" + endBP)

        finalStr = ", ".join(bpList)  # Concatenate the list into a single string

        return finalStr
    # Updates the breakpoints display
    def updateBreakpointDisplay(self):
        for haz, textBox in self._bpTextboxes.items():
            hazBPs = self.makeBPString(haz)

            # update the text box
            textBox.delete(0.0, tk.END)
            textBox.insert(0.0, hazBPs)

        return

    # Fetches the item number based on the label
    def getListBoxItem(self, listBox, textStr):
        for i in range(listBox.size()):
            if listBox.get(i) == textStr:
                return i
        return None

    # Deselects the specified pil button
    def deselectPilButton(self, pil):
        self._pilButtons[pil].configure(bg=self._deselectedColor)
        self._pilButtons[pil].configure(activebackground=self._deselectedColor)
        for haz in self._hazList:
            listBox = self._listBoxWidgets[haz]
            for item in listBox.curselection():
                self.clearItems(haz, item)
                listBox.selection_clear(item)

        return

    # Selects the list items that are defined in the stormInfoDicts.
    def selectPilButton(self, pil):
        self._pilButtons[pil].configure(bg=self._selectedColor)
        self._pilButtons[pil].configure(activebackground=self._selectedColor)

        self.fetchStormInfo()
        if pil in self._stormInfoDicts:
            bpDict = self._stormInfoDicts[pil]["Breakpoints"]
            for haz, bpList in bpDict.items():
                listBox = self._listBoxWidgets[haz]
                for bp in bpList:
                    item = self.getListBoxItem(listBox, bp)
                    self.selectListBoxItem(listBox, haz, item)
                    listBox.selection_set(item)
                self._hazSelections[haz] = listBox.curselection() # save the current selections
                
                # Scroll this list to the first item
                if len(self._hazSelections[haz]) > 0:
                    listBox.selection_set(self._hazSelections[haz][0])
                    
        # Update the entire zone display
        self.createZoneDisplay()

        return

    # Called when a pil button is selected
    def pilButtonSelected(self, pil):
        # Deselect the old pil button and list items
        if self._selectedPil != "":
            self.deselectPilButton(self._selectedPil)

        # Select the new button and restore list selections
        self._selectedPil = pil
        self.selectPilButton(self._selectedPil)

        self._lastHazSelected = None
        self._lastItemSelected = None

        self.updateBreakpointDisplay()

        return

    # Makes the pil button.
    def makePilButton(self, frame, pil):
        self._pilButtons[pil] = tk.Button(frame, text=pil, width=5, command=lambda: self.pilButtonSelected(pil))
        row = self._pilList.index(pil)
        self._pilButtons[pil].grid(row=row, column=0, sticky=tk.NW, pady=5)

        if pil == self._selectedPil:
            self.selectPilButton(pil)

    # Creates and plots the label
    def makeHazardLabel(self, frame, haz, row):

        label = tk.Label(frame, text=haz, font=self._activeFont)
        label.grid(row=row, column=1)

    # Selects the button that displays the zone areas
    def selectDisplayZonesButton(self):
        self._displayZonesButton.configure(bg=self._selectedColor)
        self._displayZonesButton.configure(activebackground=self._selectedColor)

    # Deselects the button that displays the zone areas
    def deselectDisplayZonesButton(self):
        self._displayZonesButton.configure(bg=self._deselectedColor)
        self._displayZonesButton.configure(activebackground=self._deselectedColor)

    # Called when the "Display Zones" button is clicked.
    def displayZonesSelected(self):

        if self._displayZones:
            self.resetZoneDisplay()
            self.displayZones()
            self._displayZones = False
            self.deselectDisplayZonesButton()
        else:
            self._displayZones = True
            self.selectDisplayZonesButton()
            self.pilButtonSelected(self._selectedPil)
            self.createZoneDisplay()

    # Creates the GUI areas that allows the user to display the selected zones.
    def makeHazardArea(self, frame):

        # Make buttons for the advisory pil
        self._pilFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3)
        self._pilFrame.grid(row=1, column=0, padx=10)
        self._pilButtons = {}

        for pil in self._pilList:
            self.makePilButton(self._pilFrame, pil)

        self._textboxFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3)
        self._textboxFrame.grid(row=1, column=2, padx=10)
        for haz in self._hazList:

            label = tk.Label(self._textboxFrame, text=haz, font=self._activeFont)
            row = self._hazList.index(haz)
            label.grid(row=row, column=1)

            # Make the text widget. Selected breakpoints will go there
            self._bpTextboxes[haz] = tk.Text(self._textboxFrame, height=2,
                                          width=150, wrap=tk.WORD, exportselection=0)
            self._bpTextboxes[haz].grid(row=row, column=2, columnspan=5, sticky=tk.W)

        self._displayZoneFrame = tk.Frame(frame)#, relief=tk.GROOVE, bd=3)
        self._displayZoneFrame.grid(row=2, column=2, sticky=tk.W, padx=10)
        label = "Display Zones"
        self._displayZonesButton = tk.Button(self._displayZoneFrame, text=label, width=len(label), command=lambda: self.displayZonesSelected())
        row = len(self._hazList)
        self._displayZonesButton.grid(row=row)
        label = tk.Label(self._displayZoneFrame, text="    Display 'BreakpointHazards' grid to see selected zones.")
        label.grid(row=row, column=3)
        
        if self._displayZones:
            self.selectDisplayZonesButton()
        else:
            self.selectDisplayZonesButton()

        return

    # Return the other of the allowed combo, if any
    def getOtherAllowedHazard(self, haz):
        for hazard in self._allowedHazCombos:
            if hazard != haz:
                return hazard
        return None

    # Return True if the other allowed combo is selected
    def hazardComboSelected(self, haz, item):
        if haz not in self._allowedHazCombos:
            return False

        otherHazard = self.getOtherAllowedHazard(haz)
        
        otherSelections = self._listBoxWidgets[otherHazard].curselection()
        if item in otherSelections:
            return True

        return False

    # Display the grid that allows the user to see the selected zones.
    def displayZones(self):

        weName = "BreakpointHazards"
        now = int(self._gmtime().unixTime() / (3600 * 6)) * 3600 * 6
        timeRange = TimeRange.TimeRange(AbsTime.AbsTime(now),
                                AbsTime.AbsTime(now + (3600 * 24)))
        self.createGrid("Fcst", weName, "DISCRETE", (self._bpHazGrid, self._fullHazList), timeRange,
                        discreteKeys=self._fullHazList, discreteOverlap=1, discreteAuxDataLength=5,
                        defaultColorTable="SITE/NHA/ProposedBPHazs")
        return

    # Resets the zones display grid
    def resetZoneDisplay(self):
        hazIndex = self.getIndex("<None>", self._fullHazList)
        self._bpHazGrid = self.newGrid(hazIndex, dtype=np.int8)
        self.displayZones()

    # Creates the zones display grid.
    def createZoneDisplay(self):
         
        if not self._displayZones:
            return

        weName = "BreakpointHazards"
        hazKeys = ["<None>"] + self._hazList + ["TR.W^HU.A"]

        self._bpHazGrid = self.empty(np.int8)

        bpDict = self.makeBPDict()   # haz : breakpointList
        zoneDict = self.makeZoneDict(bpDict)  # haz : zoneList

        maskDict = {}
        for hazKey in self._hazList:
            
            if hazKey in zoneDict:
                zoneList = zoneDict[hazKey]
                mask = self._zoneMap.maskFromZoneList(zoneList)
                maskDict[hazKey] = mask
            else:
                maskDict[hazKey] = self.empty(np.bool)

            hazIndex = self.getIndex(hazKey, self._fullHazList)
            self._bpHazGrid[maskDict[hazKey]] = hazIndex

        # Check for and set the allowed combination
        comboMask = maskDict["HU.A"] & maskDict["TR.W"]
        if comboMask.any():
            hazIndex = self.getIndex("TR.W^HU.A", hazKeys)
            self._bpHazGrid[comboMask] = hazIndex

        # Display the grid
        self.displayZones()
        
        return

    # Updates the zone display with the latest selection.
    def updateZoneDisplay(self, haz, item, addHazard):

        if not self._displayZones:
            return

        listBox = self._listBoxWidgets[haz]
        bpName = listBox.get(item)

        zoneList = self.breakpointToZoneList(bpName)
        mask = self._zoneMap.maskFromZoneList(zoneList)

        # See if this was part of an allowed combination
        comboSelected = self.hazardComboSelected(haz, item)
        hazardsToAdd = []
        if addHazard:
            if comboSelected:
                hazardsToAdd.append(self.getIndex("TR.W^HU.A", self._fullHazList))
            else:
                hazardsToAdd.append(self.getIndex(haz, self._fullHazList))
        else: # removing the hazard
            if comboSelected:
                otherHazard = self.getOtherAllowedHazard(haz)
                hazardsToAdd.append(self.getIndex(otherHazard, self._fullHazList))
            else:
                hazardsToAdd.append(self.getIndex("<None>", self._fullHazList))

        for hazIndex in hazardsToAdd:
            self._bpHazGrid[mask] = hazIndex 

        return

    # Returns the advisory names (AT1, ...) based on teh current set of
    # JSON files.
    def getAdvisoryNames(self):

        fileNames = self._getStormAdvisoryNames() # fetch the JSON fileNames
        # Strip the .json
        finalList = [fileName.replace(".json", "") for fileName in fileNames]
        return finalList

    # Returns all the storminfo objects in a dictionary. 
    def fetchStormInfo(self):
        # Fetch all the storm info dictionaries
        stormInfoDictList = self._WindWWUtils.getStormInfoDicts()
        self._stormInfoDicts = {}
        for stormInfo in stormInfoDictList:
            # Add the Breakpoints key if we don't have it.
            if "Breakpoints" not in stormInfo:
                stormInfo["Breakpoints"] = {}

            self._stormInfoDicts[stormInfo["pil"]] = stormInfo

    # Create the GUI upon startup.
    def setUpUI(self):

        self._tkmaster = tk.Tk()
        self._master = tk.Toplevel(self._tkmaster)

        self._master.title("Select Breakpoints")

        # Capture the "x" click to close the GUI
        self._master.protocol('WM_DELETE_WINDOW', self.cancelCommand)

        self._topFrame = tk.Frame(self._master)
        self._topFrame.grid()
        self._tkmaster.withdraw() # remove the master from the display

        self._listBoxWidgets = {}
        self._listboxFrames = {}
        self._bpFrame = tk.Frame(self._topFrame)
        self._bpFrame.grid(row=0, column=0)

        for haz in self._hazList:
            column = self._hazList.index(haz)
            self._listboxFrames[haz] = tk.Frame(self._bpFrame)
            self._listboxFrames[haz].grid(row=0, column=column)
            self.makeListBox(self._listboxFrames[haz], self._linkedBPs, self._orphanedBPs, haz, tk.MULTIPLE)

        self._hazardFrame = tk.Frame(self._topFrame)
        self._hazardFrame.grid(row=1, column=0, columnspan=3, sticky=tk.W, padx=20, pady=10)

        self._bpTextboxes = {}
        self.makeHazardArea(self._hazardFrame)

        # Make the Run, Run/Dismiss, Cancel buttons
        self.makeBottomButtons(self._topFrame)

        self.updateBreakpointDisplay()
        self.createZoneDisplay()

        return

    # Main method that sets up the GUI and enters the event loop
    def execute(self):

        # set up some constants for this tool
        self._activeFont = "Helvetica 12 bold"
        self._disabledFont = "Helvetica 12 normal"
        self._hazLabelFont =  "Helvetica 10 bold"
        self._bgColor = "#d9d9d9"

        self._selectedColor = "red"
        self._deselectedColor = "#d9d9d9"

        self._hazList =  ["HU.W", "HU.A", "TR.W", "TR.A"]
        self._fullHazList =  ["<None>", "HU.W", "HU.A", "TR.W", "TR.A", "TR.W^HU.A"]
        self._allowedHazCombos = ["TR.W", "HU.A"]

        self._listTypes = ["start", "end", "single"]
        self._islandSeparator = "***** ISLANDS *****"
        
        self._bpHazGrid = self.empty(np.bool)

        self._pilList = self.getAdvisoryNames()
        if len(self._pilList) == 0:
            self.statusBarMsg("No Advisory files found. Please run StormInfo first.", "U")
            return
        self._pilList.sort()

        self._boldColors = {
                           "HU.W" : "#FF0000",  #(255, 0, 0),  # red
                           "HU.A": "#FF0000", #(255, 125, 125), # light red
                           "TR.W" : "#8888FF", #(0, 0, 255), # blue
                           "TR.A" : "#8888FF", #(125, 125, 255), # light blue
                           "TR.W^HU.A" : "#FF00FF", #(255, 0, 255),
                           }
        self._fadedColors = {
                           "HU.W" : "#FFDDDD",  #(255, 0, 0),  # light red
                           "HU.A": "#FFDDDD", #(255, 125, 125), # light red
                           "TR.W" : "#DDDDFF", #(0, 0, 255), # light blue
                           "TR.A" : "#DDDDFF", #(125, 125, 255), # light blue
                           "TR.W^HU.A" : "#FF00FF", #(255, 0, 255), # light purple
                           }

        self._selectedPil = self._pilList[0]
        self._displayZones = True
        self._lastHazSelected = None
        self._lastItemSelected = None
        self._lastItemDeselected = None

        self._listLabels = {
                            "HU.W" : "Hurricane Warning",
                            "HU.A" : "Hurricane Watch",
                            "TR.W" : "Tropical Storm Warning",
                            "TR.A" : "Tropical Storm Watch",
                            }

        self._WindWWUtils = WindWWUtils.WindWWUtils(self._dbss)
        
        self._hazSelections = {}
        for haz in self._hazList:
            self._hazSelections[haz] = ()

        self._zoneMap = ZoneMap.ZoneMap(self._dbss)
        # initialize the selected breakpoints
        self._selectedBPs = {}
        for haz in self._hazList:
            self._selectedBPs[haz] = []

        self.fetchStormInfo()

        self._bpDict = WindWWUtils.bpDict
        bpList = list(self._bpDict.keys())

        self._linkedBPs, self._orphanedBPs = self.sortBPKeys(bpList)

        for l in self._linkedBPs:
            if len(l) <= 20:
                self._orphanedBPs = l + self._orphanedBPs

        self._linkedBPs = self._linkedBPs[0]

        self._bpListDict = {}
        for listType in self._listTypes:
            if listType in ["start", "end"]:
                self._bpListDict[listType] = self._linkedBPs
            elif listType in ["single"]:
                self._bpListDict[listType] = self._orphanedBPs

        self.setUpUI()
        tk.mainloop()

        return


