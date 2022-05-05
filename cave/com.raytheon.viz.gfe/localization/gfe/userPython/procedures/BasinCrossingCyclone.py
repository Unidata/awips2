# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# BasinCrossingCyclone
#
# April  17, 2020 21020      tlefebvr    Original Verison
# April  19, 2020 21020      tlefebvr    Mostly works. A few bugs left.
# April  20, 2020 21020      tlefebvr    Added more features. StormNum locks
#                                        for non-NHC domain. Changed GUI layout.
# April  20, 2020 21020      tlefebvr    Removed dead code and documented.
# April  20, 2020 21020      tlefebvr    Better error handling.
# April  21, 2020 21020      tlefebvr    Fixed a bug introduced with last clean
# April  21, 2020 21020      tlefebvr    Enforce that stormNumber mod 5 equals
#                                        bin number for NHC storms only.
# May    04, 2020 21020      tlefebvr    Added smarter default button settings.
# May    06, 2020 21020      tlefebvr    Code cleanup and Python3 mods.
#                                        Fixed issue when running on HPA.
# May    12, 2020 21020      tlefebvr    Added EP->CP basin buttons. Fixed a
#                                        couple of bugs. Fixed update issue
#                                        when changing basins.
# May    13, 2020 21020      tlefebvr    Fixed storm number scale that was not
#                                        updating when changing basins. Advisory
#                                        number clean up.
# May   14, 2020 22033      tlefebvr     Modified to use ***Sites methods in WWUTils
#                                        Fixed an issue with the bin buttons.
# May   21, 2020 22033      tlefebvr     Addressed code review comments.
# May   28, 2020 22033      tlefebvr     Bin button were not set to the proper state.
#                                        Cast .keys() to list for Python3.
# May   29, 2020 22033      tlefebvr     Removed StormNum slider for non NHC sites.
# Author: lefebvre
################################################################################

MenuItems = ["Populate"]

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

        self._WindWWUtils = WindWWUtils.WindWWUtils(self._dbss)
    
    def saveStormInfo(self, pil):
        """
        Saves the specified stormInfo to the JSON files under pil.
        """
        # Make a copy to ensure no funny business.
        stormInfo = copy.copy(self._stormInfoDict[self._sourceBin])
        stormInfo["pil"] = pil
        advisoryNumber = self._advisoryTextBox.get("1.0", tk.END)
        advisoryNumber = advisoryNumber.replace("\n", "")
        advisoryNumber = advisoryNumber.replace(" ", "")
        stormInfo["advisoryNumber"] = advisoryNumber
        # Save the changes locally
        self._stormInfoDict[pil] = stormInfo
        if self._NHCBasinRules:
            stormNumber = int(self._stormNumScale.get())
            stormInfo["stormNumber"] = stormNumber
            stormInfo["stormID"] = self._WindWWUtils.makeStormID(pil, stormNumber)
        
        # Use TropicalUtility to save advisories. 
        self._saveAdvisory(self._targetBin, stormInfo)
    
    def validStormNumber(self, bin):
        """
        See if the selected stormNumber is correct based on the bin.
        The stormNumber MOD 5 should match the number of the bin.
        """
        if not self._NHCBasinRules:
            return True
        
        stormNumber = int(self._stormNumScale.get())
        binDigit = int(bin[-1])
    
        return stormNumber % 5 == binDigit
    
    def saveStorm(self, bin):
        """
        Checks the selected stormNumber against the bin number and
        saves if they match, if NHC. If not, puts a message to the user and
        doesn't save. For other sited it just saves.
        """
        if self._NHCBasinRules:
            if self.validStormNumber(bin):
                self.saveStormInfo(bin)
                return True
            else:
                self.statusBarMsg("The Storm Number MOD 5 must match the bin number.", "S")
                return False
        else:
            self.saveStormInfo(bin)
            return True

    def cancelCommand(self):
        """
        Called when the cancel button is clicked
        """
        try:
            self._tkmaster.destroy()
        except:
            pass
        
    def runCommand(self):
        """ Called when run is selected. Just saves the stormInfo for the current pil.
        """
        return self.saveStorm(self._targetBin)

    def runDismissCommand(self):
        """
        Called when Run/Dismiss button is clicked.
        """
        if self.runCommand():
            self.cancelCommand()

    def makeBottomButtons(self, frame):
        """
        Create the Execute and Cancel buttons.
        """
        # Cancel button
        saveColor = "green"
        self._saveButton = tk.Button(frame, text="Save",
                                       command=self.runCommand, bg=saveColor)
        self._saveButton.grid(row=0, column=0, padx=20)
        # Cancel button
        runDismissColor = "lightgreen"
        self._saveDismissButton = tk.Button(frame, text="Save/Dismiss",
                                       command=self.runDismissCommand, bg=runDismissColor)
        self._saveDismissButton.grid(row=0, column=1, padx=20)

        # Cancel button
        cancelColor = "red"
        self._cancelButton = tk.Button(frame, text="Cancel",
                                       command=self.cancelCommand, bg=cancelColor)
        self._cancelButton.grid(row=0, column=2, padx=20)

    def binButtonSelected(self, buttonLabel):
        """
        Called when any bin button (advisory) button is selected. Updates the
        currently selected bin. If it was a source button update the info widgets.
        """
        buttonBasin = buttonLabel[0:2]
        if buttonBasin == self._sourceBasin:
            if buttonLabel != self._sourceBin:
                self._binButtons[self._sourceBin].config(bg=self._unselectedColor)
                self._binButtons[self._sourceBin].config(activebackground=self._unselectedColor)
                self._binButtons[buttonLabel].config(bg=self._selectedColor)
                self._binButtons[buttonLabel].config(activebackground=self._selectedColor)
                self._sourceBin = buttonLabel
                self.updateInfoWidgets()
        elif buttonBasin == self._targetBasin:
            if buttonLabel != self._targetBasin:
                if self._targetBin is not None:
                    self._binButtons[self._targetBin].config(bg=self._unselectedColor)
                    self._binButtons[self._targetBin].config(activebackground=self._unselectedColor)
                self._binButtons[buttonLabel].config(bg=self._selectedColor)
                self._binButtons[buttonLabel].config(activebackground=self._selectedColor)
                self._targetBin = buttonLabel

    def basinButtonSelected(self, basin):
        """ 
        Called when a basin button is selected. Updates the currently selected
        basin and makes new bin buttons.
        """
        if basin == self._currentBasinCombo:
            return
        # Show previous as unselected.
        self._basinButtons[self._currentBasinCombo].config(bg=self._unselectedColor,
                                         activebackground=self._unselectedColor)
        # Show current as selected.
        self._basinButtons[basin].config(bg=self._selectedColor, 
                                         activebackground=self._selectedColor)
        self._currentBasinCombo = basin
        # Redraw the bin buttons
        self.makeBinButtons()
        self.updateInfoWidgets()
    
    def setDefaultBin(self, basin):
        """
        Figures out the bin to select based on the basiList. Selects the
        first bin found for that basin.
        """
        filteredList = [bin for bin in self._stormInfoDict if bin.startswith(basin[0:2])]

        if len(filteredList) == 0:
            self.statusBarMsg("No active storms for basin:" + basin, "S")
            return None
        
        filteredList.sort()
        
        return filteredList[0]
    
    def makeBinButton(self, frame, label, row, column, select, disable):
        """
        Makes a single storm button. This is implemented as a separate method
        so the lambda method works properly.
        """
        # Set the button state
        state = tk.NORMAL
        if disable:
            state = tk.DISABLED
        # Set the background color
        bgColor = self._unselectedColor
        if select:
            bgColor = self._selectedColor
            
        button = tk.Button(frame, text=label,  command=lambda: self.binButtonSelected(label),
                           font=self._font14Bold, state=state, bg=bgColor, activebackground=bgColor)
        button.grid(row=row, column=column, padx=10, pady=5)
        
        return button

    def makeBinButtons(self):
        """
        Makes the bin buttons based on the currently selected basin.
        """
        # Make the frame or recycle the widgets within
        if self._binFrame:
            for child in self._binFrame.winfo_children():
                child.destroy()
                
        self._binFrame = tk.Frame(self._master, relief=tk.GROOVE, bd=3)
        self._binFrame.grid(row=1, column=0, padx=20, pady=5)
        
        self._binButtons = {}
        self._targetBin = None
        basinList = self._currentBasinCombo.split(" -> ")
        self._sourceBasin = basinList[0]
        self._targetBasin = basinList[1]
        self._defaultBin = self.setDefaultBin(self._sourceBasin)
        if self._defaultBin is None:
            self.statusBarMsg("No JSON files found. Please run StormInfo first.", "S")
            return

        self._sourceBin = self._defaultBin
        for column, basin in enumerate(basinList):
            basinName = self._basinDict[basin]
            buttonLabels = self._binDict.get(basinName, None)
            if not buttonLabels:
                self.statusBarMsg("Error, invalid basin: " + basin, "S")
                return
            
            for row, label in enumerate(buttonLabels):
                if basin == self._sourceBasin:
                    disable = label not in self._stormInfoDict
                    select = label == self._defaultBin
                else:
                    disable = label in self._stormInfoDict
                    select = False
                self._binButtons[label] = self.makeBinButton(self._binFrame, label,
                                             row, column, select, disable)
    
    def makeBasinButton(self, frame, label, row, select, disable):
        """
        Makes a single basin button. This is implemented as a separate method
        so the lambda method works properly.
        """
        # Set the button state
        state = tk.NORMAL
        if disable:
            state = tk.DISABLED
        # Set the background color
        bgColor = self._unselectedColor
        if select:
            bgColor = self._selectedColor

        button = tk.Button(frame, text=label, command=lambda: self.basinButtonSelected(label),
                           font=self._font14Bold, state=state, bg=bgColor)
        button.grid(row=row, padx=10, pady=10)

        return button

    def makeBasinButtons(self, frame):
        """
        Makes the basin buttons based on the keys in the stormInfo dict.
        """
        
        label = tk.Label(frame, text="From -> To", font=self._font14Bold, fg="blue")
        label.grid(row=0, column=0, pady=10)

        
        self._basinButtons = {}
        for i, label in enumerate(self._basinButtonList):
            
            # Filter out bins other basins
            basinBins = [bin for bin in self._stormInfoDict if label.startswith(bin[0:2])]
            disable = False
            active = False
            if len(basinBins) == 0: # There are no active storms for this basin.
                disable = True
            if label == self._currentBasinCombo:
                active = True
                self.makeBinButtons()
            row = i + 1
            button = self.makeBasinButton(frame, label, row, active, disable)
            self._basinButtons[label] = button
    
    def getStormNumLabel(self, stormNum):
        """
        Make a label for the stormNumber. This can vary depending on the
        particular office and its set of rules.
        """
        labelText = "     Storm     \n     Number     "
        if not self._NHCBasinRules:  # Add the stormNumber as it is immutable
            labelText += "\n" + str(stormNum)
        return labelText
    
    def getStormIDLabel(self, stormID):
        return "StormID:\n" + str(stormID)

    def updateInfoWidgets(self):
        """
        Updates the info widgets based on the currently selected source bin.
        Fetches the info out of the stormInfo and re-displays on the GUI.
        """
        stormName = self._stormInfoDict[self._sourceBin]["stormName"]
        advisoryNum = self._stormInfoDict[self._sourceBin]["advisoryNumber"]
        stormNum = self._stormInfoDict[self._sourceBin]["stormNumber"]
        stormID = self._stormInfoDict[self._sourceBin]["stormID"]
        self._stormNameLabel.config(text=stormName)
        
        labelText = self.getStormNumLabel(stormNum)   
        self._stormNumLabel.config(text=labelText)   
        if self._NHCBasinRules:
            self._stormNumScale.config(state=tk.NORMAL)
            self._stormNumScale.set(stormNum)
            
        labelText = self.getStormIDLabel(stormID)
        self._stormIDLabel.config(text=labelText)
           
        self._advisoryTextBox.delete("1.0", tk.END)
        self._advisoryTextBox.insert("1.0", advisoryNum)

    def makeInfoWidgets(self, frame):
        """
        Makes the info widgets on the GUI including the stormName,
        advisoryNumber and stormNumber.
        """

        # Fetch info from current bin
        stormName = self._stormInfoDict[self._sourceBin]["stormName"]
        advisoryNum = self._stormInfoDict[self._sourceBin]["advisoryNumber"]
        stormNum = self._stormInfoDict[self._sourceBin]["stormNumber"]
        stormID = self._stormInfoDict[self._sourceBin]["stormID"]
        labelFrame = tk.Frame(frame, relief=tk.GROOVE)
        labelFrame.grid(row=0, column=0, pady=5)

        self._stormNameLabel = tk.Label(labelFrame, text = stormName, font=self._font16Bold,
                                        fg="purple")
        self._stormNameLabel.grid(row=0, column=0, pady=5)
        # Make the text box for the advisory number
        textFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3)
        textFrame.grid(row=1, column=0, padx=20, pady=10)
        self._advisoryTextBox = tk.Text(textFrame, width=10, height=1, font=self._font14Bold)
        self._advisoryTextBox.grid(row=0, column=0, pady=10)
        self._advisoryTextBox.insert("1.0", advisoryNum)
        label = tk.Label(textFrame, text="     Advisory     \n    Number    ", font=self._font14Bold)
        label.grid(row=1, column=0, pady=20, sticky=tk.W+tk.E+tk.S+tk.N)
        
        # Make the scale for the storm number
        scaleFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3)
        scaleFrame.grid(row=2, column=0, padx=20, pady=10)
        if self._NHCBasinRules:
            self._stormNumScale = tk.Scale(scaleFrame, from_=0, to=self._maxStorms, resolution=1,
                                           orient=tk.HORIZONTAL, font=self._font14Normal)
            self._stormNumScale.grid(row=0, column=0, pady=10)
            self._stormNumScale.set(stormNum)
       
        labelText = self.getStormNumLabel(stormNum)
        self._stormNumLabel = tk.Label(scaleFrame, text=labelText, font=self._font14Bold)
        self._stormNumLabel.grid(row=1, column=0, pady=10)            
    
        labelText = self.getStormIDLabel(stormID)
        self._stormIDLabel = tk.Label(scaleFrame, text=labelText, font=self._font14Bold)
        self._stormIDLabel.grid(row=2, column=0, pady=10)            

    def setDefaultBasinCombo(self, basinButtonList, stormInfoDict):
        """
        Figures out a preferred default basin button based on the active storms in
        the stormInfoDict
        """
        for stormInfoKey in stormInfoDict:
            for basin in basinButtonList:
                if basin[0:2] in stormInfoKey:
                    return basin
        # Nothing found so return the first button
        return basinButtonList[0]
    
    def displayWindowOnCursor(self, master):
        """
        Moves the specified window to the cursor location.
        """
        master.update_idletasks()
        wh= master.winfo_height()
        ww= master.winfo_width()
        px, py = master.winfo_pointerxy()
        master.geometry("%dx%d+%d+%d" % (ww, wh, px - (ww //2 ),py - (wh // 2)))

    def setUpUI(self):
        """
        Makes the tk calls to set up the GUI.
        """
        self._tkmaster = tk.Tk()
        self._master = tk.Toplevel(self._tkmaster)
        self._dialogMaster = None

        self._master.title("Basin Crossing Cyclone")
        self._master.attributes("-topmost", True)

        # Capture the "x" click to close the GUI
        self._master.protocol('WM_DELETE_WINDOW', self.cancelCommand)
        self._topFrame = tk.Frame(self._master)
        self._topFrame.grid()
        self._tkmaster.withdraw() # remove the master from the display
        self._basinFrame = tk.Frame(self._master, relief=tk.GROOVE, bd=3)
        self._basinFrame.grid(row=0, column=0, padx=20, pady=5)
        self._binFrame = None
        self._binButtons = {}
        self.makeBasinButtons(self._basinFrame)
        self._infoFrame = tk.Frame(self._master, relief=tk.GROOVE, bd=3)
        self._infoFrame.grid(row=0, column=1, rowspan=2, padx=20, pady=20)
        self.makeInfoWidgets(self._infoFrame)
        # Make the Run, Run/Dismiss, Cancel buttons
        bottomButtonFrame = tk.Frame(self._master, relief=tk.GROOVE, bd=3)
        bottomButtonFrame.grid(row=2, columnspan=3, pady=20)
        self.makeBottomButtons(bottomButtonFrame)
     
    def execute(self, editArea, timeRange, varDict):

        # set up some constants for this tool
        self._font12Normal = "Helvetica 12 normal"
        self._font14Normal = "Helvetica 14 normal"
        self._font14Bold = "Helvetica 14 bold"
        self._font16Bold = "Helvetica 16 bold"
        self._bgColor = "#d9d9d9"
        self._selectedColor = "green"
        self._unselectedColor = "gray80"
        self._fullHazList =  ["<None>", "HU.W", "HU.A", "TR.W", "TR.A", "TR.W^HU.A"]
        
        # Used to translate basin nick names (AT) to basinNames
        self._basinDict = {
            "AT" : "Atlantic",
            "EP" : "Eastern Pacific",
            "CP" : "Central Pacific",
            "WP" : "Western Pacific",
            }
        
        self._binDict = self._WindWWUtils._basinBins

        # Fetch the storm information from the JSON files.        
        self._stormInfoDict = self._WindWWUtils.fetchStormInfo(self._fullHazList)
        advisoryNames = list(self._stormInfoDict.keys())
        if not advisoryNames:
            self.statusBarMsg("No Advisory files found. Please run StormInfo first.", "U")
            return

        siteID = self.getSiteID()
        
        self._NHCBasinRules = False
        if siteID in self._WindWWUtils.NHCSites():
            self._NHCBasinRules = True
            self._basinButtonList = ["AT -> EP", "EP -> AT"]
        elif siteID in self._WindWWUtils.HFOSites():
            self._basinButtonList = ["EP -> CP", "WP -> CP"]
        elif siteID in self._WindWWUtils.GUMSites():
            self._basinButtonList = ["CP -> WP"]
        else:
            self.statusBarMsg("This tool is not configured for this site.", "S")
            return

        self._maxStorms = self._WindWWUtils.maxStorms()

        self._defaultBasinCombo = self.setDefaultBasinCombo(self._basinButtonList,
                                                            self._stormInfoDict)
        self._currentBasinCombo = self._defaultBasinCombo
        self._sourceBin = advisoryNames[0]
        
        self.setUpUI()
        self.displayWindowOnCursor(self._master)
        tk.mainloop()
