# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Aviation_Populate
#
# Author: lefebvre
#
# Aviation_Populate - Version 20181114
#
# Last Modified: 14 November 2018
# 
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Populate"]

VariableList = []


import time
import SmartScript
from math import *
import numpy as np
import tkinter as tk
import AbsTime, TimeRange
import Aviation_EDASConfig as PopulateConfig


class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        self._dbss = dbss

    # Called when the cancel button is clicked
    def cancelCommand(self):
        self._tkmaster.destroy()
        return

    def runCommand(self):
        self.populateAviationGrids()
        return

    def runDismissCommand(self):
        self.populateAviationGrids()
        self.cancelCommand()
        return

    # Copied from the original CR version of Aviation_Populate.
    # We should replace this with a version that's vectorized for better performance.
    def calcCloudBaseRH(self, modelName, version, topo, timeRange):
        self.levels = ["MB1000","MB975", "MB950", "MB925", "MB900",
                        "MB875", "MB850", "MB825", "MB800", "MB775",
                        "MB750", "MB725", "MB700", "MB675", "MB650",
                        "MB625", "MB600", "MB575", "MB550", "MB525",
                        "MB500", "MB450", "MB400", "MB350", "MB300"
                       ]
        self.cloudRH = [98.0, 96.0, 94.0, 92.0, 90.0,
                        88.0, 85.0, 83.0, 80.0, 78.0,
                        75.0, 73.0, 70.0, 68.0, 65.0,
                        63.0, 60.0, 58.0, 55.0, 53.0,
                        50.0, 45.0, 40.0, 35.0, 30.0,
                        ]

        # Fetch the proper D2D databaseID
        if version == "Previous":
            versionNum = -1
        else:
            versionNum = 0
        databaseID = self.findDatabase("D2D_" + modelName, versionNum)

        trList = self.getWEInventory(databaseID, "rh", "MB500", timeRange)
        gridTRList = []
        MaxCloudBase=250
        
        for tr in trList:
            CloudBaseRH=self.newGrid(MaxCloudBase)
            sounding = self.makeNumericSounding(databaseID, "rh", self.levels, tr, noDataError=0)
            print("Making Sounding at ", tr)

            if sounding is not None:
               gh_c, rh_c = sounding 
               for i in range(rh_c.shape[0]):
                   rh = rh_c[i]
                   height = gh_c[i]/0.3048
                   height = height - topo
                   
                   height = np.where(np.less(height, 1.0), 0.0, height)
                   rh = np.where(np.less(height, 1.0), 0.0, rh)
                   CloudBaseRH = np.where(np.logical_and(np.equal(CloudBaseRH, MaxCloudBase),
                                          np.greater_equal(rh, self.cloudRH[i])), height/100.0, CloudBaseRH)
            else:
                print("Got None sounding back at:", tr)
                continue

            grid = np.clip(CloudBaseRH, 1, 250)
            gridTRList.append((grid, tr))

        return gridTRList
    # This method will fetch the fromElement and copy it to the toElement
    # for the specified database.
    def copyGridsFromOtherElement(self, dbid, fromElement, toElement, timeRange):
        fromInv = self.getWEInventory(dbid, fromElement, "SFC", timeRange)
        for tr in fromInv:
            grid = self.getGrids(dbid, fromElement, "SFC", tr)
            self.createGrid("Fcst", toElement, "SCALAR", grid, tr)
        return len(fromInv)

    # The main mthod that does the grid copying and calculations
    def populateAviationGrids(self):

        # Calculate the timeRange for the generic elements
        start = self._baseTime + (self._genStartTime * 3600)
        end = self._baseTime + (self._genEndTime * 3600)

        # Make the timeRanges for the generic and convective populate steps
        if self._timeButtonSelected.get() == "Choose -->":
            genTimeRange = TimeRange.TimeRange(AbsTime.AbsTime(start),
                                               AbsTime.AbsTime(end))
        elif self._timeButtonSelected.get() == "GFE Selected\nTime":
            genTimeRange = self._timeRange

        if self._convState == "Off":
            # Populate the generic elements first
            if self._selectedPrevCur.get() == "Previous":
                selectedDBID = self._dbIDDict[self._selectedGenSource][1]
            elif self._selectedPrevCur.get() == "Current":
                selectedDBID = self._dbIDDict[self._selectedGenSource][0]
            else:
                self.statusBarMsg("Unknown model version defined. Aborting", "S")
                return

            # Copy and interpolate the generic elements over the selected timeRange
            supportedWEs = []
            for we in self._genElementsSelected:
                if we in self._modelWEDict[selectedDBID.modelName()]:
                    supportedWEs.append(we)
            self.copyCmd(supportedWEs, selectedDBID, genTimeRange)

            self.interpolateCmd(self._genElementsSelected, genTimeRange, "GAPS", "SYNC", interval=1, duration=1)

        else: # convective state on
            # Populate the convective elements if needed.
            selectedDBID = self._dbIDDict[self._selectedConSource][0]  # Fetch the dbid from the dict
            if (self._selectedConSource == "CONSShort") & (self._selectedConAlgorithm == "CloudBaseRH"):
                self.statusBarMsg("Error!!!! CONNShort does not currently support the CloudBaseRH algorithm.", "S")
                return

            if self._selectedConAlgorithm == "CloudBaseRH":
                self._activeLabel.config(text="Calculating", fg="red")
                self._master.update()
                cloudBaseGrids = self.calcCloudBaseRH(self._selectedConSource, self._selectedPrevCur.get(), self.getTopo(), genTimeRange)
                self.deleteCmd([self._conElementSelected], genTimeRange)  # remove the grids first
                for grid, tr in cloudBaseGrids:
                    self.createGrid("Fcst", self._conElementSelected, "SCALAR", grid, tr)
                self._activeLabel.config(text="           ")
            else:
                self.deleteCmd([self._conElementSelected], genTimeRange)  # remove the grids first
                copyFromWE = self._selectedConAlgorithm
                copyCount = self.copyGridsFromOtherElement(selectedDBID, copyFromWE, self._conElementSelected, genTimeRange)
                if copyCount == 0:
                    self.statusBarMsg("No grids found for " + selectedDBID.modelName() + " " + copyFromWE, "S")
                                     

            self.interpolateCmd([self._conElementSelected], genTimeRange, "GAPS", "SYNC", interval=1, duration=1)

        # Finally Populate with Obs grids to back fill and make a gap for interpolation
        obsDB = self.findDatabase(self._obsSource)
        obsTR = TimeRange.TimeRange(AbsTime.AbsTime(self._baseTime - (self._backfillHours * 3600)),
                                    AbsTime.AbsTime(self._baseTime))

        # Copy the Obs into the Fcst
        if self._convState == "Off":
            elementsSelected = self._genElementsSelected
        else:
            elementsSelected = [self._conElementSelected]

        # Back fill grids from the Obs database
        for weName in elementsSelected:
            # simple case just copy the grids
            if weName in self._modelWEDict[self._obsSource]:
                self.copyCmd([weName], obsDB, obsTR)

        return 

    # Create the Execute and Cancel buttons
    def makeBottomButtons(self, frame):

        # Make the frame
        self._bottomButtonFrame = tk.Frame(frame, bg=self._bgColor)
        self._bottomButtonFrame.grid(row=4, column = 0, columnspan=6, pady=20)

        # Run button
        self._runButton = tk.Button(self._bottomButtonFrame, text="Run",
                                            command=self.runCommand)
        self._runButton.grid(row=0, column=0, padx=20)

        # Run/Dismiss buttom
        self._runDismissButton = tk.Button(self._bottomButtonFrame, text="Run/Dismiss",
                                            command=self.runDismissCommand)
        self._runDismissButton.grid(row=0, column=1, padx=20)
        # Cancel button
        self._cancelButton = tk.Button(self._bottomButtonFrame, text="Cancel",
                                            command=self.cancelCommand)
        self._cancelButton.grid(row=0, column=2, padx=20)

        return

    # Calls a procedure to fetch the currently selected timeRange. This allows the tool
    # to be run multiple times without closing.
    def getSelectedTimeRange(self):

        varDict = {}
        self.callProcedure("ReturnGFETimeRange", varDict=varDict)

        if "SelectedTimeRange" in varDict:
            start, end = varDict["SelectedTimeRange"]
            timeRange = TimeRange.TimeRange(AbsTime.AbsTime(start), AbsTime.AbsTime(end))
            return timeRange
        else:
            self.statusBarMsg("Error retrieving Selected TimeRange. Make sure ReturnGFETimeRange tool is installed.", "S")
            return None

    # Called when the cursor enter the GUI. Just fetches the current timeRange
    def enterEvent(self, event=None):
        # Reset the baseTime so the GUI stays current
        self.calcBaseTime()

        timeRange = self.getSelectedTimeRange()
        if timeRange is None:
            return

        if self._timeRange != timeRange:
            self._timeRange = timeRange
            
        # If we're using the GFE time, figure out the start and end time
        # in hours and update the sliders.
        if self._timeButtonSelected.get() == "GFE Selected\nTime":
            self.toggleTimeScaleState(True)
            now = int(time.time() / 3600) * 3600
            gfeStart = self._timeRange.startTime().unixTime()
            gfeEnd = self._timeRange.endTime().unixTime()
            self._genStartTime = int((gfeStart - now) / 3600)
            self._genEndTime = int((gfeEnd - now) / 3600)
            
            self._genSTScale.set(self._genStartTime)
            self._genETScale.set(self._genEndTime)
            
            self.updateStartEndTimeLabels()
            self.toggleTimeScaleState(False)

        return

    # Called when a element button is selected.  
    def genElementSelected(self, event, elementName):

        # Make a list of the elements that are supported
        supportedWEs = self._modelWEDict[self._selectedGenSource]

        if elementName not in supportedWEs:
            return

        # toggle on
        if elementName not in self._genElementsSelected and \
            self._genElementDict[elementName].cget("state") == tk.ACTIVE:
            self._genElementsSelected.append(elementName)
            self._genElementDict[elementName].select()
        elif elementName in self._genElementsSelected and \
            self._genElementDict[elementName].cget("state") == tk.ACTIVE:
            if elementName in self._genElementsSelected:
                self._genElementsSelected.remove(elementName)
                self._genElementDict[elementName].deselect()

        return

    # Make the element buttons.  The frame parameter is the topLevel frame.
    def makeGenElementButtons(self, frame):
        # Make the element frame for selecting the source
        elementFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3, bg=self._bgColor)
        elementFrame.grid(row=0, column=2, sticky=tk.N+tk.E+tk.S+tk.W)
        
        # make a label
        label = tk.Label(elementFrame, text="Select Element")
        label.grid(row=0, column=0)
        # Make each source button
        row = 1
        for i in range(len(self._config["genElementList"])):
            weName = self._config["genElementList"][i]
            def cbHandler(event, self=self, buttonName=weName):
                return self.genElementSelected(event, buttonName)
            self._genElementDict[weName] = tk.Checkbutton(elementFrame,
                text=weName)
            
            self._genElementDict[weName].grid(row=row, sticky=tk.W)
            self._genElementDict[weName].bind("<ButtonRelease-1>", cbHandler)
            self._genElementDict[weName].bind("<ButtonRelease-2>", cbHandler)
            if weName in self._config["genElementDefaultList"]:
                self._genElementDict[weName].select()
                
            self._genWidgetList.append((self._genElementDict[weName], tk.NORMAL))

            row = row + 1

        return

    # Called when the Previous or Current version buttons are selected.
    def prevCurSelected(self):
        # Tk variables are used to track the state of this button. Nothing to do here.
        return

    # Make the Current/Previous version buttons.  The frame parameter is the topLevel frame.
    def makePrevCurrentButtons(self, frame):
        # Make the Current/Previous frame for selecting the source
        prevCurFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3, bg=self._bgColor)
        prevCurFrame.grid(row=0, column=1, sticky=tk.N+tk.E+tk.S+tk.W)
        # Make each source button

        label = tk.Label(prevCurFrame, text="Model Run")
        label.grid(row=0, column=0)

        buttonList = ["Previous", "Current"]
        self._prevCurButtonDict = {}
        
        self._selectedPrevCur = tk.StringVar()
        self._selectedPrevCur.set(self._config["prevCurDefault"])

        row = 1
        for buttonName in buttonList:
            self._prevCurButtonDict[buttonName] = tk.Radiobutton(prevCurFrame, text=buttonName,
                                                           padx=10, pady=2, command=self.prevCurSelected,
                                                           variable=self._selectedPrevCur, value = buttonName)
            self._prevCurButtonDict[buttonName].grid(row=row, sticky=tk.W)
            
            if buttonName == self._config["prevCurDefault"]:
                self._prevCurButtonDict[buttonName].select()
                self._selectedPrevCur.set(buttonName)
                
            tkState = tk.NORMAL
            self._prevCurButtonDict[buttonName].config(state=tkState)

            self._genWidgetList.append((self._prevCurButtonDict[buttonName], tkState))
            
            row = row + 1

        return

    def genSourceButtonClicked(self):
        self._selectedGenSource = self._genSourceButtonSelected.get()

        # Enable or disable the element buttons based on the elements the selected model supports
        for weName, widget in self._genElementDict.items():
            
            if weName in self._modelWEDict[self._selectedGenSource]:
                widget.config(state=tk.NORMAL)
            else: # not available for this model so deselect and remove from selected list
                widget.deselect()
                widget.config(state=tk.DISABLED)
                if weName in self._genElementsSelected:
                    self._genElementsSelected.remove(weName)
        return

    # Creates the button for the generic sources. It will disable buttons when that
    # source is not found.
    def makeGenSourceRadioButtons(self, frame):
        # Make the source frame for selecting the source
        genFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3, bg = self._bgColor)
        genFrame.grid(row=0, column=0, sticky=tk.N+tk.E+tk.S+tk.W)

        # Add a label at the top
        label = tk.Label(genFrame, text=" Select Model")
        label.grid(row=0, column=0)

        self._genSourceButtonSelected = tk.StringVar()
        self._genSourceButtonSelected.set(self._config["genSources"][0])
        self._genSourceButtonDict = {}
        row = 1
        for buttonName in self._config["genSources"]:
            self._genSourceButtonDict[buttonName] = tk.Radiobutton(genFrame, text=buttonName,
                                                               padx=10, pady=2, command=self.genSourceButtonClicked,
                                                               variable=self._genSourceButtonSelected,
                                                               value = buttonName)
            self._genSourceButtonDict[buttonName].grid(row=row, sticky=tk.W)
            if buttonName == self._config["defaultGenSource"]:
                self._genSourceButtonDict[buttonName].invoke()

            if buttonName in self._genModelsMissing:
                tkState = tk.DISABLED
            else:
                tkState = tk.NORMAL

            self._genSourceButtonDict[buttonName].config(state=tkState)

            self._genWidgetList.append((self._genSourceButtonDict[buttonName], tkState))

            row = row + 1
        return

    # Formats the time string for display.
    def makeTimeStr(self, intTime):

        dayStr = str(time.gmtime(intTime).tm_mday).zfill(2)
        hourStr = str(time.gmtime(intTime).tm_hour).zfill(2)
        return "\n " + dayStr + "." + hourStr + "Z "

    # Updates the start end tne time labels in the GUI
    def updateStartEndTimeLabels(self):
        
        # self._baseTime is truncated to 00Z
        start = self._baseTime + self._genStartTime * 3600 
        end = self._baseTime + self._genEndTime * 3600

        startStr = self.makeTimeStr(start)
        endStr = self.makeTimeStr(end)

        self._startTimeLabel.config(text=startStr)
        self._endTimeLabel.config(text=endStr)

        return
    
    # Called when the generic start time is moved.
    def genStartChanged(self, event):

        self._genStartTime = self._genSTScale.get()

        if self._genStartTime > self._startHours:
            self._genStartTime = self._startHours
            self._genSTScale.set(self._genStartTime)

        # reset the endTime if needed
        if self._genStartTime >= self._genEndTime:
            self._genEndTime = self._genStartTime + 1
            if self._genEndTime > self._endHours:
                self._genEndTime = self._endHours
                
            self._genETScale.set(self._genEndTime)

        self.updateStartEndTimeLabels()

        return

    # Called when the generic end time is moved.
    def genEndChanged(self, event):

        self._genEndTime = self._genETScale.get()

        if self._genEndTime > self._endHours:
            self._genEndTime = self._endHours
        if self._genEndTime <= 0:
            self._genEndTime = 1
        self._genETScale.set(self._genEndTime)

        #reset startTime if needed
        if self._genEndTime <= self._genStartTime:
            self._genStartTime = self._genEndTime - 1
            if self._genStartTime <= 0:
                self._genStartTime = 0

            self._genSTScale.set(self._genStartTime)

        self.updateStartEndTimeLabels()
        return

    # Called when either time selector is selected. 
    def timeButtonSelected(self):

        if self._timeButtonSelected.get() == "Choose -->":
            self.toggleTimeScaleState(True)
        else:
            self.enterEvent()
            self.toggleTimeScaleState(False)
                  
        return
    
    # Toggles the time scales to active (True) or disabled (False)
    def toggleTimeScaleState(self, activate):

        if activate:
            tkState = tk.NORMAL
        else:
            tkState = tk.DISABLED

        for w in self._timeWidgetList:
            w.config(state=tkState)

        return

    # Creates the generic time scales on the GUI
    def makeGenTimeScales(self, frame):
        self._genTimeFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3, bg=self._bgColor)
        self._genTimeFrame.grid(row=1, column=0, columnspan=3, sticky=tk.E+tk.W)

        # Selector frame
        selFrame = tk.Frame(self._genTimeFrame, relief=tk.GROOVE, bd=3, bg=self._bgColor)
        selFrame.grid(row=0, column=0, sticky=tk.N+tk.S, rowspan=2)

        # Create Selector buttons
        buttonList = ["GFE Selected\nTime", "Choose -->"]
        self._timeButtonDict = {}
        self._timeButtonSelected = tk.StringVar()
        self._timeButtonSelected.set(self._timeButtonDefault)
        row = 0
        for buttonName in buttonList:
            self._timeButtonDict[buttonName] = tk.Radiobutton(selFrame,
                text=buttonName, padx=10, pady=2, command=self.timeButtonSelected,
                variable=self._timeButtonSelected, value=buttonName)
                        
            self._timeButtonDict[buttonName].grid(row=row)
            row = row + 1

        # Make the time frame, time scales and time labels
        timeScaleFrame = tk.Frame(self._genTimeFrame, relief=tk.GROOVE, bd=3, bg=self._bgColor)
        timeScaleFrame.grid(row=1, column=1, sticky=tk.E+tk.W, columnspan=2)
                
        # Create start and end time scales with labels
        self._startLabel = tk.Label(timeScaleFrame, text="\nStart")
        self._startLabel.grid(row=1, column=0)
        self._endLabel = tk.Label(timeScaleFrame, text="\nEnd")
        self._endLabel.grid(row=2, column=0)
        self._timeWidgetList.append(self._startLabel)
        self._timeWidgetList.append(self._endLabel)

        self._genSTScale = tk.Scale(timeScaleFrame, from_= 0, 
                                       to=self._startHours, orient=tk.HORIZONTAL,
                                       command=self.genStartChanged,
                                       resolution=1, length=190, sliderlength=20)
        self._genSTScale.grid(row=1, column=1, sticky=tk.E+tk.W, columnspan=2)
        self._genSTScale.set(self._defaultStartHour)
        self._genStartTime = self._defaultStartHour
        self._timeWidgetList.append(self._genSTScale)

        self._genETScale = tk.Scale(timeScaleFrame, from_= 0, 
                                       to=self._endHours, orient=tk.HORIZONTAL,
                                       command=self.genEndChanged,
                                       resolution=1, length=190, sliderlength=20)
        self._genETScale.grid(row=2, column=1, sticky=tk.E+tk.W, columnspan=2)
        self._genEndTime = self._genETScale.set(self._defaultEndHour)
        self._genEndTime = self._defaultEndHour
        self._timeWidgetList.append(self._genETScale)

        # And finally the start and end time labels
        self._startTimeLabel = tk.Label(timeScaleFrame, text=" DD.HH Z", font=self._activeFont)
        self._startTimeLabel.grid(row=1, column=3, sticky=tk.E)
        self._endTimeLabel = tk.Label(timeScaleFrame, text=" DD.HH Z", font=self._activeFont)
        self._endTimeLabel.grid(row=2, column=3, sticky=tk.E)

        self.updateStartEndTimeLabels()

        return
    # Toggles the widgets in the generic frame 
    def toggleGenFrame(self, active):
        if active:
            for w, tkState in self._genWidgetList:
                w.config(state=tkState, bg=self._bgColor)
        else:
            for w, tkState in self._genWidgetList:
                w.config(state=tk.DISABLED)
        return

    # Toggles on/off all the convective widgets based on the value of active
    def toggleConvectiveFrame(self, active):
        if active:
            self.toggleGenFrame(False)
            for w, tkState in self._conWidgetList:
                w.config(state=tkState, bg=self._bgColor)
        else:
            self.toggleGenFrame(True)
            for w, tkState in self._conWidgetList:
                w.config(state=tk.DISABLED)
        return

    # Called when the convective button is selected
    def convButtonSelected(self, event, buttonName):

        # If it's on, turn it off
        if self._convState == "On":
            self._convState = "Off"
            self._convButton.config(fg='gray20')
            self._convButton.config(activeforeground='gray20')
            self._convButton.config(font=self._disabledFont)
            self.toggleConvectiveFrame(False)
        # If it's off, turn it on
        elif self._convState == "Off":
            self._convState = "On"
            self._convButton.config(fg='black')
            self._convButton.config(activeforeground='black')
            self._convButton.config(font=self._activeFont)
            self.toggleConvectiveFrame(True)
            # Reset the algorithm button state
            self.setAlgorithmButtonState()

        return

    # Creates the convective button in the GUI
    def makeConvectiveButton(self, frame):

        buttonName = "Alternative Cloud Base"
        def cbHandler(event, self=self, buttonName=buttonName):
            return self.convButtonSelected(event, buttonName)

        if self._convState == "On":
            font = self._activeFont
        else:
            font = self._disabledFont

        self._convButton = tk.Button(frame, text=buttonName, font=font,
                                     height=1, width=len(buttonName), relief=tk.RAISED, bd=3,
                                     justify=tk.CENTER)
        self._convButton.grid(row=0, column=0, columnspan=4)

        self._convButton.bind("<ButtonRelease-1>", cbHandler)
        self._convButton.bind("<ButtonRelease-2>", cbHandler)

        return

    # Resets the state of the algorithm buttons based on the selected convective source
    def setAlgorithmButtonState(self):
        if self._algButtonDict:
            if self._selectedConSource == "CONSShort":  # disable the CloudBaseRH button
                if self._selectedConAlgorithm == "CloudBaseRH":
                    self._algButtonDict["CloudBaseCCL"].invoke()
                self._algButtonDict["CloudBaseRH"].config(state=tk.DISABLED)
            else:
                for alg in self._config["algChoices"]:
                    self._algButtonDict[alg].config(state=tk.NORMAL)
        return

    def conSourceButtonClicked(self):
        self._selectedConSource = self._conSourceButtonSelected.get()

        # Reset the algorithm state
        self.setAlgorithmButtonState()

        return

    # Creates the button for the convective sources. It will disable buttons when that
    # source is not found.
    def makeConSourceRadioButtons(self, frame):
        # Make the source frame for selecting the source
        conFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3, bg = self._bgColor)
        conFrame.grid(row=3, column=0, sticky=tk.N+tk.E+tk.S+tk.W)
        
        # Add a label at the top
        label = tk.Label(conFrame, text=" Select One Model")
        label.grid(row=0, column=0)


        self._conSourceButtonSelected = tk.StringVar()
        self._conSourceButtonSelected.set(self._config["conSources"][0])
        row = 1
        for buttonName in self._config["conSources"]:
            self._conSourceButtonDict[buttonName] = tk.Radiobutton(conFrame, text=buttonName,
                                                               padx=10, pady=2, command=self.conSourceButtonClicked,
                                                               variable=self._conSourceButtonSelected,
                                                               value = buttonName)
            self._conSourceButtonDict[buttonName].grid(row=row, sticky=tk.W)
            if buttonName == self._config["defaultConSource"]:
                self._conSourceButtonDict[buttonName].invoke()
                self._selectedConSource = self._config["defaultConSource"]

            if buttonName in self._conModelsMissing:
                tkState = tk.DISABLED
            else:
                tkState = tk.NORMAL

            self._conSourceButtonDict[buttonName].config(state=tkState)

            self._conWidgetList.append((self._conSourceButtonDict[buttonName], tkState))

            row = row + 1
        return

    # Called when a convective element button is selected.  
    def conElementSelected(self):
        self._conElementSelected = self._conElementStringVar.get()

        return

    # Make the element buttons.  The frame parameter is the topLevel frame.
    def makeConElementButtons(self, frame):
        # Make the element frame for selecting the source
        conElementFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3, bg=self._bgColor)
        conElementFrame.grid(row=3, column=1, sticky=tk.E+tk.W+tk.N)

        # make a label
        label = tk.Label(conElementFrame, text="Select Element")
        label.grid(row=0, column=0)

        # set up defaults for this menu
        self._conElementStringVar = tk.StringVar()
        self._conElementStringVar.set(self._config["conElementDefault"])
        self._conElementSelected = self._config["conElementDefault"]
        self._conElementDict = {}

        row = 1
        # Make each element button
        for buttonName in self._config["conElementList"]:
            self._conElementDict[buttonName] = tk.Radiobutton(conElementFrame, text=buttonName,
                                                               padx=10, pady=2, command=self.conElementSelected,
                                                               variable=self._conElementStringVar,
                                                               value = buttonName)
            self._conElementDict[buttonName].grid(row=row, sticky=tk.W)
            
            # Select the default value
            if buttonName == self._config["conElementDefault"]:
                self._conElementDict[buttonName].invoke()

            tkState = tk.NORMAL
            self._conElementDict[buttonName].config(state=tkState)

            self._conWidgetList.append((self._conElementDict[buttonName], tkState))
            
            row = row + 1

        # make a label that indicates the tool is calculating
        labelFrame = tk.Frame(conElementFrame, relief=tk.GROOVE, bd=3, bg=self._bgColor)
        labelFrame.grid(row=row+1, sticky=tk.N+tk.S+tk.E+tk.W)
        self._activeLabel = tk.Label(labelFrame, text="           ", padx=50)
        self._activeLabel.grid(row=1, column=0)

        return


    def algButtonClicked(self):
        self._selectedConAlgorithm = self._algButtonSelected.get()

        return

    # Creates the button for the algorithm choices.
    def makeAlgRadioButtons(self, frame):
        # Make the alg frame for selecting the algorithm
        algFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3, bg = self._bgColor)
        algFrame.grid(row=3, column=2, sticky=tk.N+tk.E+tk.S+tk.W)

        # Add a label at the top
        label = tk.Label(algFrame, text="Select One Algorithm")
        label.grid(row=0, column=0)

        self._algButtonSelected = tk.StringVar()
        self._algButtonSelected.set(self._config["algChoices"][0])
        self._algButtonDict = {}
        row = 1
        for buttonName in self._config["algChoices"]:
            self._algButtonDict[buttonName] = tk.Radiobutton(algFrame, text=buttonName,
                                                             padx=10, pady=2, command=self.algButtonClicked,
                                                             variable=self._algButtonSelected,
                                                             value = buttonName)
            self._algButtonDict[buttonName].grid(row=row, sticky=tk.W)
                            
            if buttonName == self._config["defaultAlg"]:
                self._algButtonDict[buttonName].invoke()
                self._selectedConAlgorithm = self._config["defaultAlg"]

            if buttonName == "CloudBaseRH" and self._selectedConSource == "CONSShort":
                tkState = tk.DISABLED
            else:
                tkState = tk.NORMAL

            self._conWidgetList.append((self._algButtonDict[buttonName], tkState))

            row = row + 1
        return

    # High-level method that calls other methods to build the GUI a widget at a time
    def setUpUI(self):

        self._tkmaster = tk.Tk()
        self._master = tk.Toplevel(self._tkmaster)

        self._master.title("Aviation_Populate")

        # Capture the "x" click to close the GUI
        self._master.protocol('WM_DELETE_WINDOW', self.cancelCommand)

#         self._topFrame = tk.Frame(self._master, bg=self._bgColor)
        self._topFrame = tk.Frame(self._master)
        self._topFrame.grid()
        self._tkmaster.withdraw() # remove the master from the display
        self._topFrame.bind("<Enter>", self.enterEvent)


        # Make the widgets for the generic sources
        self._genWidgetList = []
        self._timeWidgetList = []
        self._genElementDict = {}
        self.makeGenSourceRadioButtons(self._topFrame)
        self.makePrevCurrentButtons(self._topFrame)
        self.makeGenElementButtons(self._topFrame)
        self.makeGenTimeScales(self._topFrame)

        # Put the next set of widgets in a frame
        self._conFrame = tk.Frame(self._topFrame, relief=tk.GROOVE, bd=3, bg=self._bgColor)
        self._conFrame.grid(row=2, column=0, columnspan=3)

        # Initialize Alternative CloudBase state data
        self._conWidgetList = []
        self._conSourceButtonDict = {}
        self._conElementDict = {}
        self._algButtonDict = {}

        # Make the Alternative CloudBase widgets
        self.makeConvectiveButton(self._conFrame)
        self.makeConSourceRadioButtons(self._conFrame)
        self.makeConElementButtons(self._conFrame)
        self.makeAlgRadioButtons(self._conFrame)
        #self.makeConTimeScales(self._conFrame)
        
        if self._convState == "Off":
            self.toggleConvectiveFrame(False)

        self.makeBottomButtons(self._topFrame)

        return

    #####################################################################################

    # Returns a dictionary modelName : list of databaseIDs for the specified modelList. 
    def makeDatabaseIDDict(self, modelList):
        
        dbIDDict = {}
        allDBs = self.availableDatabases()
        for dbid in allDBs:
            # Skip all D2D databases
            if "_D2D_" in dbid.modelIdentifier():
                continue
            modelName = dbid.modelName()
            if modelName in modelList:
                if modelName in dbIDDict:
                    dbIDDict[modelName].append(dbid)
                else:
                    dbIDDict[modelName] = [dbid]

        # Sort the dbids in place newest to oldest.
        for dbids in dbIDDict.values():
            dbids.sort(key=lambda x: x.modelTime(), reverse=True)

        return dbIDDict

    # Fetches the inventory in the form of a timeRange list
    def getWEInventory(self, modelName, weName, level, timeRange=None):
       
        if timeRange is None:
            timeRange = TimeRange.allTimes()

        trList = []
        # getGridInfo will just die if the modelName or weName is not valid
        # so wrap it in a try block and return [] if it fails
        try:
            gridInfo = self.getGridInfo(modelName, weName, level, timeRange)
        except:
            return trList

        for g in gridInfo:
            if timeRange.overlaps(g.gridTime()):
                trList.append(g.gridTime())

        return trList

    def makeModelWEDict(self, modelList, weList):
        
        modelWEDict = {}
        allDBs = self.availableParms()
        for weName, level, dbid in allDBs:
            modelName = dbid.modelName()
            if modelName not in modelList or "_D2D_" in dbid.modelIdentifier():
                continue
            if weName not in weList:
                continue
            if modelName in modelWEDict and weName not in modelWEDict[modelName]:
                modelWEDict[modelName].append(weName)
            else:
                modelWEDict[modelName] = [weName]

        return modelWEDict

    def calcBaseTime(self):
        self._baseTime = int(time.time() / 3600) * 3600
        return

    # Make a temporary dialog to see if the user wants to continue.        
    def dialogPrompt(self):
        root = tk.Tk()
        # root.withdraw() # This removes the toot window, but causes the dialog
        # to appear on other screen. Both windows are removed by destroy
        import tkinter.messagebox as msg
        result = msg.askyesno("Aviation_Populate","Aviation_Populate may be already running. Continue?")
        root.destroy()
        return result

    # Called when the tool first starts, helps ensure the tool can't be started again.
    # Starting twice causes CAVE to hang
    def lockProcedure(self):
        # Since CAVE hangs when procedures like these are started twice.
        # The presence of this object indicates the tool is already running.
        try:
            # getObject throws an exception if the object is not found
            self.getObject("Aviation_Populate", "Aviation_Populate")
            keepGoing = self.dialogPrompt()
            return keepGoing
        except: # not found
            # Didn't find the object so save it now to prevent another execution of this procedure
            self.saveObject("Aviation_Populate", "StringObject", "Aviation_Populate")
            return True

    def unlockProcedure(self):
        
        # Remove the object now that we're done.
        self.deleteObject("Aviation_Populate", "Aviation_Populate")
        return

    # Main method that sets up the GUI and enters the event loop
    def execute(self, timeRange):

        self._config = PopulateConfig.config["Populate"]

        # Lock so procedure can't be run again without closing first.
        if not self.lockProcedure():
            return

        # Save the selected timeRange
        self._timeRange = timeRange

        # set up some constants for this tool
        self._activeFont = "Helvetica 12 bold"
        self._disabledFont = "Helvetica 12 normal"
        self._bgColor = "#d9d9d9"
                
        self._genElementsSelected = self._config["genElementDefaultList"]

        self._obsSource = "Obs"
        self._backfillHours = 6
        
        self._timeButtonDefault = "Choose -->" # "GFE Selected"
        
        self._startHours = 35
        self._endHours = 36
        self._defaultStartHour = 0
        self._defaultEndHour = 36
        self._interpGap = 1
        
        self.calcBaseTime()

        # Convective source, element and algorithm configuration
        self._convState = "Off"
        self._selectedConvState = self._convState
                
        self._conElementDefault = self._config["conElementDefault"]
        self._conElementSelected = self._conElementDefault

        # Make a dictionary of dbids
        self._dbIDDict = self.makeDatabaseIDDict(self._config["genSources"] + self._config["conSources"])

        # Identify any model we didn't find.
        self._genModelsMissing = []
        for modelName in self._config["genSources"]:
            if modelName not in self._dbIDDict:
                self._genModelsMissing.append(modelName)
        self._conModelsMissing = []
        for modelName in self._config["conSources"]:
            if modelName not in self._dbIDDict:
                self._conModelsMissing.append(modelName)
                        
        weList = self._config["genElementList"] + self._config["conElementList"]
        self._modelWEDict = self.makeModelWEDict(self._config["genSources"] + self._config["conSources"] + [self._obsSource], weList)

        # Call the main method to set up all the GUI widgets
        self.setUpUI()
        tk.mainloop()
        # Clean up the lock
        self.unlockProcedure()
        return


