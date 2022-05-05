# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
# ----------------------------------------------------------------------------
##
# Author: lefebvre
#
# HSF
#
# SOFTWARE HISTORY
#  Date        Ticket#    Engineer    Description
#  ----------- ---------- ----------- --------------------------
#  03/09/2015   -         tlefebvre   modified formatPolygonEditArea to add
#                                     leading zero when lat < 10
#  07/29/2016   -         tlefebvre   Changed edit area retrieval and storage to
#                                     work outside CAVE so edit areas could be shared.
#  12/20/2017  DCS17686   tlefebvre   Initial baseline version.
#  05/22/2018  DR20724    tlefebvre   Remove from menus by default since most 
#                                     sites will not use these tools
#
##
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = []

import SmartScript
import tkinter
import tkinter.messagebox
import time
import math
import MetLib
import numpy as np
import pickle, os, copy, stat
import TimeRange
import AbsTime
import EditAreaUtilities

import sys
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    #  The following section reduces a polygon to just the essential points

    # Calculate the angle formed by the center and y, x
    def calcAngle(self, centerY, centerX, y, x):

        lat1 = centerY * self._degToRad
        lon1 = centerX * self._degToRad
        lat2 = y * self._degToRad
        lon2 = x * self._degToRad

        yDiff = (lat2 - lat1) * self._degToRad
        xDiff = (lon2 - lon1) * self._degToRad
       
        angle = math.atan2(yDiff, xDiff) / self._degToRad
        if angle < 0:
            angle = angle + 360.0
        return angle

    # calculate the great circle distance between the two specified points
    # WARNING: resulting distances can be negative
    def distance(self, lat1, lon1, lat2, lon2):

        lat1 = lat1 * self._degToRad
        lon1 = lon1 * self._degToRad
        lat2 = lat2 * self._degToRad
        lon2 = lon2 * self._degToRad


        # some issue with acos and large numbers throws an except
        if abs(lon2 - lon1) < 0.000001:
            return (lat2 - lat1) / self._degToRad

        dist = math.acos(np.sin(lat1) * np.sin(lat2) + np.cos(lat1) * np.cos(lat2) * \
                    np.cos(lon2 - lon1)) / self._degToRad


        return dist

    # Debugging method to print coordinates in an easy to read format
    def printCoords(self, i, x0, y0, x1, y1, x2, y2):

        print("%d---" % i, "%.2f" % y0, "%.2f" % x0, "%.2f" % y1, "%.2f" % x1, "%.2f" % y2, "%.2f" % x2)

        return

    # Returns a list of lat/lons that represent the specified edit area.
    # These points approximate the edit area with no more than maxPoints
    # points.
    def simplifyEditArea(self, editArea):
               
        polygons = editArea.getPolygons(ReferenceData.CoordinateType.LATLON)

        coords = polygons.getCoordinates()
            
        coordList = []
       
        for c in coords:
            coordList.append((c.x, c.y))
       
        # Algorithm configuration section.  These values will affect how
        # well (or poorly) the algorithm works.

        # Define the number of slices.  More gives better radial resolution
        # at the price of slightly slower execution.
        slices = 36

        # The resulting simplified area will contain no more than
        # this number of points
        maxPoints = 6

        # Any triangle whose area is less than this ratio, the middle
        # point gets tossed as being not significant.
        areaThreshold = 0.05

        ###  End configuration section

        # Make fixed size arrays of length pieSlices for each access
        maxDist = slices * [0]

        # Initialize this list to None so we know if no points were
        # found in each slice.
        rawLoc = slices * [None]

        # calculate the center of the polygon
        centroid = polygons.getCentroid()
       
        latCenter = centroid.getY()
        lonCenter = centroid.getX()
       
        originalArea = polygons.getArea()

        # reduce the number of points to a manageable number by
        # breaking the edit area up into pie slices and removing
        # all points in each slice except the outer most point in
        # each pie slice.

        for x, y in coordList:

            angle = self.calcAngle(latCenter, lonCenter, y, x)
            angleIndex = int(angle / (360.0 / slices))
            dist = self.distance(latCenter, lonCenter, y, x)
            if dist > maxDist[angleIndex]:
                maxDist[angleIndex] = dist
                rawLoc[angleIndex] = (y, x)

        # it's possible no points were found in some sices, so filter them out
        loc = []
        for i in rawLoc:
            if not i is None:
                loc.append(i)

        while True:   # loop until we can't remove any more points

            prevCount = len(loc)
            minArea = originalArea
            areaList = []
           
            for i in range(len(loc)):
                # figure out the index of the previous and next points
                last = i - 1
                next = i + 1
                if next >= len(loc):
                    next = 0

                x0 = loc[i][1]
                y0 = loc[i][0]
                x1 = loc[last][1]
                y1 = loc[last][0]
                x2 = loc[next][1]
                y2 = loc[next][0]

                # make sure the slopes are not zero or infinity
                if x2 == x1:
                    x2 = x2 + 0.000001
                if y2 == y1:
                    y2 = y2 + 0.000001

                # it's now safe to calculate the slope of the base of the triagle
                slope = (y2 - y1) / (x2 - x1)

                # calculate the point on a line connecting last and next
                # points that is perpendicular to the middle point
                x = (slope * x1 - y1 + (x0 / slope) + y0) / (slope + (1 / slope))
                y = ((x0 - x) / slope) + y0

                # calculate the area of the triangle formed by the three points:
                # next, current, and previous
                height = self.distance(y, x, y0, x0)
                base = self.distance(y1, x1, y2, x2)
                area = abs(base * height / 2.0)  # note area can be negative!
                areaList.append(area)

                # save the area if it's the smallest
                if area < minArea:
                    minArea = area
                    delIndex = i

                ########################   end looping through all points  ##############

            # calculate the ratio of the area and the original area of the edit area
            # Use this later to decide whether to remove the point
            ratio = minArea / originalArea

            if len(loc) > maxPoints:
                del loc[delIndex]
                continue
            else:
                if ratio < areaThreshold:
                    del loc[delIndex]
               
            # If we're down to 3 points: stop
            if len(loc) <= 3:
                break
           
            #If no new points have been removed, we're done
            if prevCount == len(loc):
                break

        return loc


    ###################  End polygon code  ##################################

    # Fetches the inventory in the form of a timeRange list
    def getWEInventory(self, modelName, weName, timeRange=None):
       
        if timeRange is None:
            timeRange = TimeRange.allTimes()

        trList = []
        # getGridInfo will just die if the modelName or weName is not valid
        # so wrap it in a try block and return [] if it fails
        try:
            gridInfo = self.getGridInfo(modelName, weName, "SFC", timeRange)
        except:
            return trList

        for g in gridInfo:
            if timeRange.overlaps(g.gridTime()):
                trList.append(g.gridTime())  

        return trList
   

    # Create the list box that holds the names of the scripts
    def makeListBox(self):

        labelList = self.getAllEditAreaNames()
        labelList.sort()

        label = tkinter.Label(self._listFrame, text="Edit Area List")

        label.grid(row=0, padx=15, pady=10)
       
        scrollbar = tkinter.Scrollbar(self._listFrame, orient=tkinter.VERTICAL)
        self._listbox = tkinter.Listbox(self._listFrame, yscrollcommand=scrollbar.set,
                                        selectmode=tkinter.MULTIPLE)
        self._listbox.bind("<ButtonRelease-1>", self.listBoxClick)
        #self._listbox.bind("<ButtonPress-1>", self.listBoxClick)

        for i in range(len(labelList)):
            self._listbox.insert(tkinter.END, labelList[i])

        self._listbox.grid(row=1, padx=5, pady=5)
           
        scrollbar.config(command=self._listbox.yview)
        scrollbar.grid(row=1, column=1, sticky=tkinter.N+tkinter.S)

        return

    # Make the time period buttons
    def makeTimeButtons(self, timeFrame):
        label = tkinter.Label(timeFrame, text="Select Time\nPeriod")
        label.grid(row=0, padx=15, pady=10)

        labelList = []
        for i in range(0, 49, 6):
            label = str(i)
            if len(label) < 2:
                label = "0" + label
            label = label + "h"
            labelList.append(label)

        buttonRow = 1
        for i in range(len(labelList)):
            b = tkinter.Radiobutton(timeFrame, text=labelList[i],
                                    value=labelList[i])
            if labelList[i] == self._defaultTime:
                b.select()
                self._currentTimePeriod = self._defaultTime

            b.grid(row=buttonRow, column=0, sticky=tkinter.N+tkinter.S, pady=2)
            buttonRow = buttonRow + 1
            b.bind("<ButtonRelease-1>", self.timeButtonClick)
        return

 
    # Callback that fires when a list selector is clicked.
    def listBoxClick(self, event=None):
        
        if self._textBoxCurSelection is not None:
            self.textBoxReturnPress()
       
        # Disable all but the Add New button
        if len(self._listbox.curselection()) == 0:
            self._clearSelButton.configure(state=tkinter.DISABLED)      
            self._replaceSelButton.configure(state=tkinter.DISABLED)      
            self._saveSelButton.configure(state=tkinter.DISABLED)
            self._killSelButton.configure(state=tkinter.DISABLED)
#             return
           
       
        # Display the edit area(s) on the GFE
        gridSize = (self._gridLoc.gridSize().y, self._gridLoc.gridSize().x)

        mask = np.zeros(gridSize, bool)
       
        for i in self._listbox.curselection():
            curIndex = int(i)
            editAreaName = self._listbox.get(curIndex)
            
            mask = self._eaUtils.fetchEditArea(editAreaName)
            if mask is None:
                self.statusBarMsg("Edit Area: " + editAreaName + " not found in HighSeas repository.", "S")
                return
       
        editArea = self.decodeEditArea(mask)
        self.setActiveEditArea(editArea)
       
        # Update the textBox
        if sum(sum(mask)) > 0:
            editAreaDesc = self.getEditAreaDesc(editAreaName)
            self._textBox.configure(font=self._boldFont)
            self.updateTextBoxSaveState("Saved")
   
            self.displayAreaDesc(editAreaDesc)
           
        if len(self._listbox.curselection()) != 1:
            # Disable the Replace and Save Buttons
            self._replaceSelButton.configure(state=tkinter.DISABLED)      
            self._saveSelButton.configure(state=tkinter.DISABLED)
            self._killSelButton.configure(state=tkinter.DISABLED)
        else:
            self._replaceSelButton.configure(state=tkinter.NORMAL)
            self._clearSelButton.configure(state=tkinter.NORMAL)         
            self._saveSelButton.configure(state=tkinter.NORMAL)
            self._killSelButton.configure(state=tkinter.NORMAL)
           
        return
   
    def enterEvent(self, event=None):
       
        sys.stdout.flush()
       
        self._enterCount = self._enterCount + 1
       
        if self._enterCount > 4:
            self.setActiveEditArea(self._initialEditArea)
            self._master.unbind("<Enter>")
       
        return
   
    def redrawListBox(self):

        self._listbox.delete(0, tkinter.END)

        listItems = []
        for fDict in self._hsDesc:
            for areaDict in fDict["areaList"]:
                listItems.append(areaDict["areaName"])

        listItems.sort()
       
        for i in range(len(listItems)):
            self._listbox.insert(tkinter.END, listItems[i])

        return

    def selectListBoxItem(self, item):
        listBoxItems = self._listbox.get(0, tkinter.END)

        listBoxItems = list(listBoxItems)

        selectedIndex = listBoxItems.index(item)

        self._listbox.selection_set(selectedIndex)

        return
       
    def setSelectButtonState(self, buttonState):
   
        self._clearSelButton.configure(state=buttonState)      
        self._replaceSelButton.configure(state=buttonState)      
        self._saveSelButton.configure(state=buttonState)      
        self._killSelButton.configure(state=buttonState)      

        return

    def timeButtonClick(self, event):

        # the event's widget value is a tuple and we want the last one
        buttonText = event.widget.config("value")[-1]

        # for some reason, inserting a \n makes that entry a tuple
        # so just concatenate the tuple elements into a single string
        if type(buttonText) is tuple:
            valueText = ""
            for t in buttonText:
                valueText = valueText + t + " "
            valueText = valueText[0:-1]
        else:
            valueText = buttonText
           
        self._currentTimePeriod = valueText

        return

    # Called when the pie is click upon by the mouse
    def pieClick(self, event):

        xCenter = self._circleRadius + (self._circleOffset / 2.0)
        yCenter = self._circleRadius + (self._circleOffset / 2.0)

        xDiff = event.x - xCenter
        yDiff = event.y - yCenter
      
        angle = -math.atan2(yDiff, xDiff) * 360 / (2 * math.pi)

        x1, y1, x2, y2 = self.getCircleCoords()

        # truncate the angle between the center and the click location
        angleStart = int(angle / 45.0) * 45.0
        angleExtent = 45.0

        if angle < 0:
            angleExtent = -45.0

        sliceAngle = angle
        if angle < 0:
            sliceAngle = angle + 360
        sliceNum = int(sliceAngle / 45.0)

        fillColor = self._pieDeselectColor

        ButtonDownEvent = "4"
        ButtonUpEvent = "5"

        if event.type == ButtonDownEvent and not self._pieState[sliceNum] :
            self._selectingPieSlices = True
            self._deselectingPieSlices = False
        elif event.type == ButtonDownEvent and self._pieState[sliceNum]:
            self._selectingPieSlices = False
            self._deselectingPieSlices = True
        elif event.type == ButtonUpEvent:
            self._selectingPieSlices = False
            self._deselectingPieSlices = False
            self.makePolarEditArea()
            return

        # select a pieSlice if appropriate
        if self._selectingPieSlices:
            self._pieState[sliceNum] = True
        elif self._deselectingPieSlices:
            self._pieState[sliceNum] = False

        # If we're not selecting or deselecting, do nothing
        if not self._selectingPieSlices and not self._deselectingPieSlices:
            return

        if self._selectingPieSlices:
            fillColor = self._pieSelectColor
        elif self._deselectingPieSlices:
            fillColor = self._pieDeselectColor

        self._canvas.create_arc(x1, y1, x2, y2, style=tkinter.PIESLICE,
                                start=angleStart, extent=angleExtent, fill=fillColor)
       
        return

    # Turns on one pieSlice if they are all off.  This is so it shows up on the
    # screen
    def activatePie(self):

        # Don't bother if any slices are already on
        for state in self._pieState:
            if state:
                return

        # Make a fake event so pieClick will be called.
        xCenter = self._circleRadius + (self._circleOffset / 2.0)
        yCenter = self._circleRadius + (self._circleOffset / 2.0)

        xPos = xCenter + 10
        yPos = yCenter + 3

        self._canvas.event_generate("<ButtonRelease-1>", x=xPos, y=yPos)

        return

    def prettyLatString(self, latValue):
        if latValue >= 0:
            return str(latValue) + "N"
        return str(latValue)[1:] + "S"
       
    def prettyLonString(self, lonValue):
        if lonValue >= 0:
            return str(lonValue) + "E"
        else:
            if lonValue < -180.0:
                lonValue = lonValue + 360.0
                return str(lonValue) + "E"
           
        return str(lonValue)[1:] + "W"
       
    def latScaleMotion(self, value):
               
        value = float(value)
        if value == self._latScaleValue:
            return

        self._latScaleValue = value

        latStr = self.prettyLatString(value)
        self._latEntry.delete(0, tkinter.END)
        self._latEntry.insert(0, latStr)

        self.activatePie()
        self.makePolarEditArea()
        return

    def lonScaleMotion(self, value):

        value = float(value)
        if value == self._lonScaleValue:
            return

        self._lonScaleValue = value
       
        lonStr = self.prettyLonString(value)
        self._lonEntry.delete(0, tkinter.END)
        self._lonEntry.insert(0, lonStr)

        self.activatePie()
        self.makePolarEditArea()

        return

    def getLatLonGrids(self):
        trList = self.getWEInventory("Fcst", "lat")
        if len(trList) == 0:
            gridLoc = self.getGridLoc()
            latGrid, lonGrid = MetLib.getLatLonGrids(gridLoc)
        else:
            latGrid = self.getGrids("Fcst", "lat", "SFC", trList[0])
            lonGrid = self.getGrids("Fcst", "lon", "SFC", trList[0])
           
        return latGrid, lonGrid

    def radiusScaleMotion(self, value):

        value = float(value)
        if value == self._radiusScaleValue:
            return
       
        self._radVar.set(str(value))
        self._radEntry.delete(0, tkinter.END)
        self._radEntry.insert(0, str(value))

        self._radiusScaleValue = value
        self.activatePie()
        self.makePolarEditArea()

        return

    # A single place where the circle coords can be had.
    def getCircleCoords(self):
        topLeftX = self._circleOffset
        topLeftY = self._circleOffset
        bottomRightX = self._circleRadius * 2
        bottomRightY = self._circleRadius * 2

        return topLeftX, topLeftY, bottomRightX, bottomRightY

    def filterTextEntry(self, entryStr, allowedChars):

        returnStr = ""
        for c in entryStr:
            if allowedChars.find(c) > -1:
                returnStr = returnStr + c

        return returnStr

    def parseLatitude(self):
        latStr = self._latEntry.get()

        sign = 1.0
        if latStr.find("S") > -1 or latStr.find("s") > -1:
            sign = -1.0
            latStr = latStr[0:-1]
        elif latStr.find("N") > -1 or latStr.find("n") > -1:
            latStr = latStr[0:-1]

        try:
            latValue = float(latStr) * sign
        except:
            self.statusBarMsg("Invalid latitude value: " + str(self._lonEntry.get()), "S")
            return

        return latValue

    def parseLongitude(self):
        lonStr = copy.copy(self._lonEntry.get())

        sign = 1.0
        if lonStr.find("W") > -1 or lonStr.find("w") > -1:
            sign = -1.0
            lonStr = lonStr[0:-1]
        elif lonStr.find("E") > -1 or lonStr.find("e") > -1:
            lonStr = lonStr[0:-1]

        try:
            lonValue = float(lonStr) * sign
            if lonValue > 0:
                lonValue = lonValue - 360
        except:
            self.statusBarMsg("Invalid longitude value: " + str(self._lonEntry.get()), "S")
            return

        return lonValue
   
    def latKeyPress(self, event):
        # Ignore certain special characters
        ignore = ["BackSpace"]
        if event.keysym in ignore:
            return

        if event.keysym == "Return":
            self._latScaleValue = self.parseLatitude()
            self.activatePie()
            self.makePolarEditArea()
            self._latScale.set(self._latScaleValue)
            return
           
        allowedChars = "0123456789-.NSns"
        entryStr = self._latEntry.get() + event.char
        filterStr = self.filterTextEntry(entryStr, allowedChars)

        # replace the string if not valid
        if filterStr != entryStr:
            self._latVar.set(str(filterStr))
            # Insert a backspace in the queue to remove the offending char
            self._latEntry.event_generate("<KeyPress>", keysym="BackSpace",
                                          when="tail")
        return
   
    def lonKeyPress(self, event):
        # Ignore certain special characters
        ignore = ["BackSpace"]
        if event.keysym in ignore:
            return

        if event.keysym == "Return":
            self._lonScaleValue = self.parseLongitude()
            self.activatePie()
            self.makePolarEditArea()
            self._lonScale.set(self._lonScaleValue)
            return
       
        allowedChars = "0123456789-.EWew"
        entryStr = self._lonEntry.get() + event.char
        filterStr = self.filterTextEntry(entryStr, allowedChars)

        # replace the string if not valid
        if filterStr != entryStr:
            self._lonVar.set(str(filterStr))
            # Insert a backspace in the queue to remove the offending char
            self._lonEntry.event_generate("<KeyPress>", keysym="BackSpace",
                                          when="tail")
        return
   
    def radKeyPress(self, event):
        # Ignore certain special characters
        ignore = ["BackSpace"]
        if event.keysym in ignore:
            return

        allowedChars = "0123456789."
        entryStr = self._radEntry.get() + event.char
        filterStr = self.filterTextEntry(entryStr, allowedChars)
       
        try:
            self._radiusScaleValue = float(filterStr)
        except:
            # replace the string if not valid
            self._radVar.set(str(filterStr))
            # Insert a backspace in the queue to remove the offending char
            self._radEntry.event_generate("<KeyPress>", keysym="BackSpace", when="tail")
            return
       
        if event.keysym == "Return":
            self._radiusScale.set(self._radiusScaleValue)
            self.makePolarEditArea()
           
        return

    def makePolarWidgets(self):

        # make the pie slices
        x1, y1, x2, y2 = self.getCircleCoords()
        canvasWidth = x2 + self._circleOffset
        canvasHeight = y2 + self._circleOffset
        self._canvas = tkinter.Canvas(self._polarFrame, width=canvasWidth,
                                      height=canvasHeight)
        self._canvas.grid(row=0, column=1, columnspan=2)
       
        for i in range(0, 8):
            self._canvas.create_arc(x1, y1, x2, y2, style=tkinter.PIESLICE,
                                    start=i*45, extent=45, fill=self._pieDeselectColor)

        self._canvas.bind("<Button-1>", self.pieClick)
        self._canvas.bind("<ButtonRelease-1>", self.pieClick)
        self._canvas.bind("<Motion>", self.pieClick)

        # change from/to to minmax lat/lon values

        self._latScale = tkinter.Scale(self._polarFrame, from_=self._maxLat, to=self._minLat,
                                       orient=tkinter.VERTICAL, showvalue=0,
                                       resolution=0.5, length=160,
                                       command=self.latScaleMotion)
        self._latScale.grid(row=0, column=0, sticky=tkinter.N)
        self._latScale.set(self._latScaleValue)

        self._latVar = tkinter.StringVar()
        self._latVar.set(str(self._latScaleValue))

        self._latEntry = tkinter.Entry(self._polarFrame, width=6, textvariable=self._latVar)

        self._latEntry.grid(row=1, column=0, padx=10, sticky=tkinter.N)

        self._latEntry.bind("<KeyPress>", self.latKeyPress)

        latLabel = tkinter.Label(self._polarFrame, text="Latitude")
        latLabel.grid(row=2, column=0, sticky=tkinter.N)

        self._lonScale = tkinter.Scale(self._polarFrame, from_=self._minLon, to=self._maxLon,
                                 orient=tkinter.HORIZONTAL, showvalue=0,
                                 resolution=0.5, length=160, command=self.lonScaleMotion)
        self._lonScale.grid(row=1, column=2, sticky=tkinter.N)
        self._lonScale.set(self._lonScaleValue)

        self._lonVar = tkinter.StringVar()
        self._lonEntry = tkinter.Entry(self._polarFrame, width=6, textvariable=self._lonVar)
        self._lonEntry.grid(row=1, column=1, sticky=tkinter.N)
        self._lonEntry.bind("<KeyPress>", self.lonKeyPress)
       
        lonLabel = tkinter.Label(self._polarFrame, text="Longitude")
        lonLabel.grid(row=2, column=1, sticky=tkinter.NW)
       
        self._radiusScale = tkinter.Scale(self._polarFrame, from_=30, to=900,
                                    orient=tkinter.HORIZONTAL, showvalue=0,
                                    resolution=10, length=160, command=self.radiusScaleMotion)
        self._radiusScale.grid(row=3, column=2, pady=10, sticky=tkinter.W)
        self._radiusScale.set(self._defaultRadius)

        self._radVar = tkinter.StringVar()
        self._radEntry = tkinter.Entry(self._polarFrame, width=6, textvariable=self._radVar)
        self._radEntry.grid(row=3, column=0, pady=10, columnspan=2)
        self._radEntry.bind("<Key>", self.radKeyPress)


        label = tkinter.Label(self._polarFrame, text="Radius (nm)")
        label.grid(row=4, column=0, columnspan=2, sticky=tkinter.N)
       

        return
       
    # Create the Execute and Cancel buttons
    def makeBottomButtons(self):

        button = tkinter.Button(self._bottomFrame, text="Cancel",
                                command=self.cancelCommand)
        button.grid(row=0, column=0, padx=30, pady=10, sticky=tkinter.E)

        button = tkinter.Button(self._bottomFrame, text="Save",
                                command=self.saveOnlyCommand)
        button.grid(row=0, column=1, padx=30, pady=10, sticky=tkinter.E)

        button = tkinter.Button(self._bottomFrame, text="Save and Exit",
                                command=self.saveExitCommand)
        button.grid(row=0, column=2, padx=30, pady=10, sticky=tkinter.E)

        self._bottomFrame.grid()

        return

    def saveOnlyCommand(self):
        self.saveHSDescToFile()

        return

    def saveExitCommand(self):
        self.saveHSDescToFile()
        print("saving desc to file")
        self.cancelCommand()

        return
       
    # Cancels the GUI and the tool.
    def cancelCommand(self):

        msg = "Your edits have not been saved.  Cancel Anyway?"
        if not self._editsMade:
            self._master.destroy()
            self._tkmaster.destroy()
            return
       
        if tkinter.messagebox.askokcancel("High Sea - Cancel", msg, parent=self._bottomFrame):
            self._master.destroy()
            self._tkmaster.destroy()

        return   

    def getPolarInfo(self, editArea):
        lat = None
        lon = None
        radius = None
        pieState = None
        
        try:
            eaMask = self.encodeEditArea(editArea)
        except:
            eaMask = editArea
        
        eaMask = eaMask & self._clipMask
        polarMask = self.encodeEditArea(self._polarEditArea) & self._clipMask   

        if sum(sum(eaMask)) == sum(sum(polarMask)):  # they are the same editArea
            lat = self._latScaleValue
            lon = self._lonScaleValue
            radius = self._radiusScaleValue
            pieState = self._pieState
        return lat, lon, radius, pieState
   
   
    # Return the max width and height of the edit area in lat/lon coordinates
    def editAreaExtent(self, editArea):
        polygons = editArea.getPolygons(ReferenceData.CoordinateType.LATLON)
       
        coords = polygons.getCoordinates()
               
        minX = 10000.0
        minY = 10000.0
        maxX = -10000.0
        maxY = -10000.0
        for c in coords:
            minX = min(c.x, minX)
            minY = min(c.y, minY)
            maxX = max(c.x, maxX)
            maxY = max(c.y, maxY)
           
        diffX = maxX - minX
        diffY = maxY - minY

        return diffX, diffY
       
    # Called when the save button is clicked.
    def saveSelCommand(self):
        editArea = self.getActiveEditArea()

        # Check for null area
        mask = self.encodeEditArea(editArea) & self._clipMask
        if sum(sum(mask)) == 0:
            self.statusBarMsg("Please select an edit area before saving.", "S")
            return

        # Check to ensure the edit area is large enough
        diffLon, diffLat = self.editAreaExtent(editArea)
       
        if diffLon < self._minEASize or diffLat < self._minEASize:
            self.statusBarMsg("Please select a larger edit area before saving", "S")
            return          
       
        editArea = self.decodeEditArea(mask)
        self.setActiveEditArea(editArea)

        # get the selected button
        curSel = self._listbox.curselection()
        if len(curSel) == 0:
            return
       
        listIndex = self._listbox.curselection()[0]
        buttonStr = self._listbox.get(listIndex)

        featureName = self.extractFeature(buttonStr)
        timePeriod = self._currentTimePeriod
        editAreaName = self.getNewAreaName(featureName, timePeriod)  

        # get the polar parameters
        polarInfo = self.getPolarInfo(editArea)

        if polarInfo is not None:
            lat, lon, radius, pieState = polarInfo
        else:
            lat = None
            lon = None
            radius = None
            pieState = None

        editAreaDesc = self.makeEditAreaDesc(editArea, editAreaName, lat, lon, radius, pieState)
        # save the area
        
        activeArea = self.getActiveEditArea()
        
        activeMask = self.encodeEditArea(activeArea)
        self._eaUtils.saveEditArea(editAreaName, activeMask)

        self.displayAreaDesc(editAreaDesc)
        self.addNewArea(featureName, timePeriod, editAreaName, editAreaDesc,
                        lat, lon, radius, pieState)
       

        self.redrawListBox()
        self.selectListBoxItem(editAreaName)

        self.setSelectButtonState(tkinter.NORMAL)
       
        self._editsMade = True

    def saveNewCommand(self):
       
        editArea = self.getActiveEditArea()
       
        # Check for null area
        mask = self.encodeEditArea(editArea) & self._clipMask  # clip to domain mask

        if sum(sum(mask)) == 0:
            self.statusBarMsg("Please select an edit area before saving.", "S")
            return

        # Check to ensure the edit area is large enough
        diffLon, diffLat = self.editAreaExtent(editArea)
       
        if diffLon < self._minEASize or diffLat < self._minEASize:
            self.statusBarMsg("Please select a larger edit area before saving", "S")
            return           

        editArea = self.decodeEditArea(mask)
        self.setActiveEditArea(editArea)

        timePeriod = self._currentTimePeriod
        featureName = self.getNewFeatureName()
        editAreaName = self.getNewAreaName(featureName, timePeriod)

        # get the polar parameters
        polarInfo = self.getPolarInfo(editArea)

        if polarInfo is not None:
            lat, lon, radius, pieState = polarInfo
        else:
            lat = None
            lon = None
            radius = None
            pieState = None
       
        editAreaDesc = self.makeEditAreaDesc(editArea, editAreaName, lat, lon, radius, pieState)
        # save the area
        activeArea = self.getActiveEditArea()
        
        activeMask = self.encodeEditArea(activeArea)
        self._eaUtils.saveEditArea(editAreaName, activeMask)
       
        self.addNewFeature(featureName, timePeriod, editAreaName, editAreaDesc,
                           lat, lon, radius, pieState)


        self.redrawListBox()

        self.selectListBoxItem(editAreaName)

        self.setSelectButtonState(tkinter.NORMAL)

        self.displayAreaDesc(editAreaDesc)

        self._editsMade = True
       
        return
   
    def replaceSelCommand(self):
       
        listIndex = self._listbox.curselection()[0]
        editAreaName = self._listbox.get(listIndex)
       
        editArea = self.getActiveEditArea()
       
        # Check to ensure the edit area is large enough
        diffLon, diffLat = self.editAreaExtent(editArea)
       
        if diffLon < self._minEASize or diffLat < self._minEASize:
            self.statusBarMsg("Please select a larger edit area before saving", "S")
            return

        mask = self.encodeEditArea(editArea) & self._clipMask  # clip to domain mask

        editArea = self.decodeEditArea(mask)
        self.setActiveEditArea(editArea)
        
        self._eaUtils.saveEditArea(editAreaName, mask)
               
        # get the polar parameters
        lat, lon, radius, pieState = self.getPolarInfo(editArea)

        editAreaDesc = self.makeEditAreaDesc(editArea, editAreaName, lat, lon, radius, pieState)

        self.replaceEditAreaDesc(editAreaName, editAreaDesc)

        self.displayAreaDesc(editAreaDesc)
        self._editsMade = True
       
        return

    def clearSelCommand(self):
        self.displayAreaDesc("")

        while True:
       
            if len(self._listbox.curselection()) == 0:
                break
       
            index = self._listbox.curselection()[0]   
   
            editAreaName = self._listbox.get(index)
       
            self.removeEditArea(editAreaName)
       
            self._listbox.delete(index)
       
            self.setSelectButtonState(tkinter.DISABLED)

        self._editsMade = True
       
        return

    def clearAllCommand(self):
       
        self.displayAreaDesc("")

        listSize = self._listbox.size()
        for i in range(listSize):
            editAreaName = self._listbox.get(i)
            self.removeEditArea(editAreaName)

        self._listbox.delete(0, listSize)
       
        self.setSelectButtonState(tkinter.DISABLED)

        self._editsMade = True
       
        return
    
    # Called when the Reasons window is closed. Just destroy the window.
    def closeReasonWindow(self):
        
        self._killWindow.destroy()
        
        self.addKilledArea()
 
        return

    # Called when any reason button is clicked. Updates the reason to the GUI
    def reasonButtonClick(self, event):
        
        self._killDesc = self._killVar.get()
                
        # Update the display on the GUI
        self.displayAreaDesc(self._killDesc)
 
        return
    
    def displayKillReasonsDialog(self, killReasons):
                
        self._killWindow = tkinter.Toplevel(self._master)
        self._killWindow.title("Reason")
        self._killFrame = tkinter.Frame(self._killWindow, width=500, height=500) # can't get this to resize
        self._killFrame.pack()
        self._killVar = tkinter.StringVar()
        self._killVar.set(self._killDesc)
        for i in range(len(killReasons)):
            b = tkinter.Radiobutton(self._killFrame, text=killReasons[i],
                                    variable=self._killVar, value=killReasons[i])
            if b == self._defaultKillReason:
                b.select()
                
            b.grid(row=i, column=0, sticky=tkinter.W, pady=2)
        
            b.bind("<ButtonRelease-1>", self.reasonButtonClick)
            
        self._doneButton = tkinter.Button(self._killFrame, text= "DONE", command=self.closeReasonWindow)
        self._doneButton.grid(row=len(killReasons), sticky=tkinter.E+tkinter.W)

        # Update the display on the GUI
        self.displayAreaDesc(self._killDesc)

        self._killFrame.grid()
        
        # make this dialog modal so no other buttons can be clicked.
        self._killWindow.transient(self._master)
        self._killWindow.grab_set()
        
        return

    def killSelCommand(self):
        # make sure something is selected (should never happen)
        if len(self._listbox.curselection()) == 0:
            return
       
        self.displayKillReasonsDialog(self._killReasons)
        
        return
        
    def addKilledArea(self):
        
        # Get the selected button names
        buttonStr = self._currentTimePeriod
        listIndex = self._listbox.curselection()[0]

        editAreaName = self._listbox.get(listIndex)
        # Create the editArea name from the button name
        pos = editAreaName.find("_")
        editAreaName = editAreaName[0:pos+1] + buttonStr + editAreaName[pos+4:]
                       
        activeArea = self.getActiveEditArea()
        
        activeMask = self.encodeEditArea(activeArea)
        self._eaUtils.saveEditArea(editAreaName, activeMask)
        
        featureName = self.extractFeature(editAreaName)
        timePeriod = self.extractTimePeriod(editAreaName)

        # get the polar parameters
        polarInfo = self.getPolarInfo(activeArea)

        if polarInfo is not None:
            lat, lon, radius, pieState = polarInfo
        else:
            lat = None
            lon = None
            radius = None
            pieState = None

        editAreaDesc = self._killDesc
        
        self.addNewFeature(featureName, timePeriod, editAreaName, editAreaDesc,
                          lat, lon, radius, pieState)

        for hs in self._hsDesc:
            if hs["featureName"] == featureName and hs["timePeriod"] == timePeriod:
                for area in hs["areaList"]:
                    if area["areaName"] == editAreaName:
                        area["areaDesc"] = editAreaDesc

        self.setSelectButtonState(tkinter.NORMAL)
       
        self._editsMade = True
        
        self.redrawListBox()
        return

    def makePolarEditArea(self):
        lat1 = self._latScaleValue * self._degToRad
        lon1 = self._lonScaleValue * self._degToRad
        radius = self._radiusScaleValue
       
        earthRadius = 6371.0 / 1.852  # Nautical miles
               
        distanceGrid = np.arccos(np.sin(lat1) * np.sin(self._latGrid) + \
                       np.cos(lat1) * np.cos(self._latGrid) * \
                       np.cos(self._lonGrid - lon1)) * earthRadius
      
        distanceMask = np.less(distanceGrid, radius)

        xDist = self._latGrid - lat1
        yDist = self._lonGrid - lon1

        tanGrid = np.arctan2(xDist, yDist) / self._degToRad
        tanGrid = np.where(np.less(tanGrid, 0.0), tanGrid + 360.0, tanGrid)
        tanGrid = tanGrid / 45.0  # convert to 8 "quardants
       
        # mask off all but the selected quadrants.
        circleMask = np.zeros(tanGrid.shape, dtype=np.int8)
        for i in range(len(self._pieState)):
            if self._pieState[i]:
                tanMask = np.logical_and(np.greater_equal(tanGrid, i), np.less(tanGrid, i+1))
                circleMask |= tanMask

        mask = np.logical_and(distanceMask, circleMask)
        editArea = self.decodeEditArea(mask)

        self.setActiveEditArea(editArea)

        self._polarEditArea = editArea
               
        return

    def dirSort(self, dirA, dirB):

        aDir, aCount = dirA
        bDir, bCount = dirB

        if aCount > bCount:
            return 1
        elif aCount == bCount:
            return 0
        else:
            return -1

    # Returns a string that describes a circular edit area
    def formatCircleEditArea(self, lat, lon, radius, pieState):
               
        dirList = ["E", "NE", "N", "NW", "W", "SW", "S", "SE"]

        # Check to see if they're all selected (the whole pie)
        if sum(pieState) == len(pieState):
            latStr = self.latLonStr(lat, "lat")
            lonStr = self.latLonStr(lon, "lon")

            return "WITHIN " + str(int(radius)) + " NM OF CENTER AT " + \
                   latStr + lonStr

        quad = [0] * len(pieState)
        semi = [0] * len(pieState)

        # Find out if we have six consecutive slices selected
        # This is because the rules are different if 3/4 of the
        # pie is selected.
        sixConsec = False
        if sum(pieState) >= 6:
            for i in range(-2, len(pieState)):
                if not pieState[i] and not pieState[i + 1]:
                    sixConsec = True
                    break
               
        for i in range(len(pieState)):
            if pieState[i] and pieState[i - 1]:
                quad[i] = 1

        # Begin with the first selected pieSlice
        start = 0
        while quad[start]:
            start = start - 1
           
        for i in range(start, start + len(quad)):
            last = i - 1
            this = i
            next = i + 1
            afterNext = next + 1
            if next >= len(quad):
                next = next - len(quad)
            if afterNext >= len(quad):
                afterNext = afterNext - len(quad)

            if quad[last] and quad[this] and quad[next]:
                semi[this] = 1
                quad[last] = 0
                quad[this] = 0
                quad[next] = 0
                # if 3/4 of the pie is selected then turn off on extra quad
                if sixConsec:
                    quad[afterNext] = 0

        # Format the slices into words
        finalDir = ""

        # Direction order must be clockwise starting from N
        # This list orders directions properly.
        pieOrderList = [2, 1, 0, 7, 6, 5, 4, 3]

##        for i in range(len(semi)):
        for i in pieOrderList:
            if semi[i]:
                finalDir = dirList[i] + " SEMICIRCLE"

        if sum(semi) > 0 and sum(quad) > 0:
            finalDir = finalDir + " AND "

##        for i in range(len(quad)):
        for i in pieOrderList:
            if quad[i]:
                finalDir = finalDir  + dirList[i] +  " AND "

        if sum(quad) > 0:
            finalDir = finalDir[0:-5]   # remove the last AND

        if sum(quad) > 0:
            finalDir = finalDir + " QUADRANT"
           
        if sum(quad) > 1:
            finalDir = finalDir + "S"

        latStr = self.latLonStr(lat, "lat")
        lonStr = self.latLonStr(lon, "lon")
                       
        desc = "WITHIN " + str(int(radius)) + " NM " + finalDir + \
               " OF CENTER AT " + latStr + lonStr
              
        return desc

    # Formats a lat lon string with appropriate direction labels
    def latLonStr(self, coord, latOrLon, precision=1.0):

        if latOrLon == "lat":
            if coord >= 0:
                dirStr = "N"
            else:
                dirStr = "S"
            
            # Exception for southern boundary of EP3 area to force 18.5S output
            # This code is needed only for NH1 domain
            if coord < -18.5:
                precision = 0.5
            
            # Exception for northern boundary of EP3 area to force 3.4S output
            # This code is needed only for NH1 domain
            if coord < -3.36 and coord > -3.38:
                coord = -3.4
                coordStr = "0" + "%.1f" % abs(coord)  + dirStr
                return coordStr
                
        elif latOrLon == "lon":
            if coord >= 0:
                dirStr = "E"
            else:
                dirStr = "W"
       
        coordStr = "%.0f" % abs(self.round(coord, "Nearest", 1)) + dirStr
        # if the precision is 0.5, round to the nearest 0.5 degrees
        if precision == 0.5:
            coord = int((abs(coord) + 0.25) * 2.0) / 2.0
            # Keep the 0.5 if we have it after rounding
            if coord != int(coord):
                if abs(coord) < 10:
                    coordStr = "0" + "%.1f" % abs(coord)  + dirStr
                else:
                    coordStr = "%.1f" % abs(coord)  + dirStr
  
        return coordStr
   
    # Determines the precision based on the max extent in both lat and lon
    # Uses self._marginalEASize to define the threshold for the precision.
    def determinePrecision(self, points):
       
        latMin = 10000.0
        latMax = -10000.0
        lonMin = 10000.0
        lonMax = -10000.0
       
        # Calculate the min/max extent of these points.
        for lat, lon in points:
            latMin = min(lat, latMin)
            latMax = max(lat, latMax)
            lonMin = min(lon, lonMin)
            lonMax = max(lon, lonMax)
       
        # Calculate the difference between the maxes and mins
        latDiff = latMax - latMin
        lonDiff = lonMax - lonMin
               
        # If we have a marginally small area, set the precision to 0.5 otherwise 1.0 degrees.
        precision= 1.0
        if latDiff < self._marginalEASize or lonDiff < self._marginalEASize:
            precision = 0.5
       
        return precision
   
    # Returns a string that describes a polygon edit area
    # modified to add leading "0" when lat < 10
    def formatPolygonEditArea(self, editArea):
       
        points = self.simplifyEditArea(editArea)
       
        precision = self.determinePrecision(points)

        coordList = []

        fullStr = ""
        for y, x in points:
            latStr = self.latLonStr(y, "lat", precision)
            lonStr = self.latLonStr(x, "lon", precision)

            coordList.append((latStr, lonStr))

        # close the polygon
        coordList.append(coordList[0])
       
        lastLat = ""
        lastLon = ""
        fullStr = "WITHIN AREA BOUNDED BY "
        for latStr, lonStr in coordList:
            if latStr == lastLat and lonStr == lastLon:
                continue
            if len(latStr) < 3:
                latStr = latStr.zfill(3)
            fullStr = fullStr + latStr + lonStr + " TO "
            lastLat = latStr
            lastLon = lonStr

        # remove the last " TO "
        fullStr = fullStr[0:-4]
       
        return fullStr
   
    def makeNamedAreaDesc(self, editArea):
       
        descList = []
       
        editAreaMask = self.encodeEditArea(editArea)
        for (eaName, mask) in self._namedAreaMasks.items():
            overlap = mask & editAreaMask
           
            if overlap.any():
                descList.append(self._namedAreaDescip[eaName])
#    Commented out below as was requiring that ALL namedAreas be overlapped in order to return (JL/TL 07/21/2016)
#             else:
#                 return ""
           
        if len(descList) == 0:
            return ""
       
        finalStr = "...INCLUDING "
       
        for desc in descList:
            finalStr = finalStr + desc
            if desc != descList[-1]:   # add an AND if we're not at the last one
                finalStr = finalStr + " AND "
            else:
                finalStr = finalStr + "..."

        return finalStr

    def makeEditAreaDesc(self, editArea, editAreaName, lat=None, lon=None, radius=None, pieState=None):
       
        namedAreaDesc = self.makeNamedAreaDesc(editArea)

        if lat is not None:
            desc = self.formatCircleEditArea(lat, lon, radius, pieState)
        else:
            desc = self.formatPolygonEditArea(editArea)
        
        # Add delimiters around the lat/lon descriptor so we can replace it when needed
        return self._descDelimStart + desc + namedAreaDesc + self._descDelimEnd 

    def displayAreaDesc(self, editAreaDesc):
               
        self._textBox.configure(state=tkinter.NORMAL)
        self._textBox.delete("1.0", tkinter.END)
        self._textBox.insert("1.0", editAreaDesc)
        self._textBox.configure(state=tkinter.DISABLED)
       
        return

    def textBoxClick(self, event=None):

        self._textBox.configure(font=self._normalFont)
        self._textBox.configure(state=tkinter.NORMAL)
        self._textBoxCurSelection = self._listbox.curselection()[0]

        return
   
    def textBoxReturnPress(self, event=None):

        #listIndex = int(self._listbox.curselection()[0])
        listIndex = self._textBoxCurSelection
        editAreaName = self._listbox.get(listIndex, None)
        editAreaDesc = self._textBox.get("1.0", tkinter.END)

        editAreaDesc = editAreaDesc.replace(chr(10), "")
        self.displayAreaDesc(editAreaDesc)
           
        self.replaceEditAreaDesc(editAreaName, editAreaDesc)
        self._textBox.configure(font=self._boldFont)
        self._textBox.configure(state=tkinter.DISABLED)
        self.updateTextBoxSaveState("Saved")
        self._textBoxCurSelection = None

    def textBoxKeyPress(self, event):

        if event.char.isprintable():
            self.updateTextBoxSaveState("NOT Saved")

        return

    def updateTextBoxSaveState(self, state):
        msg = "Edit Area Description..." + state

        if state == "NOT Saved":
            color = "red"
        else:
            color = "black"
        self._textLabel.configure(text=msg, foreground=color)
        return

    ##########################################################################

    ####   Data structure code

    ##########################################################################
   
    def getBasinName(self, editAreaName):
        # Figure out which basin this area is in
        basinName = ""
        basinList = []
        for basin in self._basinNames:
            if basin in self._allEditAreaNames:
                
                basinMask = self._eaUtils.fetchEditArea(basin)
            else:
                continue
            
            areaMask = self._eaUtils.fetchEditArea(editAreaName)
            if areaMask is None:
                self.statusBarMsg("Edit area: " + " not found in repository.", "S")
                continue

            if sum(sum(basinMask & areaMask)) > 0:
                basinList.append(basin)
            
        basinList.sort()
        for b in basinList:
            if basinName == "":
                basinName = b
            else:
                basinName = basinName + " AND " + b
           
        if basinName == "":
            self.statusBarMsg("No Basin identified for last edit area.", "S")
            
        return basinName
       

    def addNewFeature(self, featureName, timePeriod, editAreaName, editAreaDesc,
                      lat=None, lon=None, radius=None, pieState=None):

        if self._siteID in self._sitesWithBasins:
            basinName = self.getBasinName(editAreaName)
        else:
            basinName = None
        if basinName is not None:
            if basinName.find("_") > -1:
                basinName = basinName.replace("_", " ")
           
        # make the low level data structure
        areaDict = {"areaName" : editAreaName,
                    "areaDesc" : editAreaDesc,
                    "basin" : basinName,
                    "lat" : lat,
                    "lon" : lon,
                    "radius" : radius,
                    "pieState" : pieState,
                    }

        featureDict = {
            "featureName" : featureName,
            "timePeriod" : timePeriod,
            "areaList" : [areaDict],  # list of areaDict
            "basin" : basinName,
            }
       
        self._hsDesc.append(featureDict)
    
        return featureName

    def addNewArea(self, featureName, timePeriod, editAreaName, editAreaDesc,
                   lat=None, lon=None, radius=None, pieState=None):
              
        if self._siteID in self._sitesWithBasins:
            basinName = self.getBasinName(editAreaName)
        else:
            basinName = None
       
        if basinName is not None:
            if basinName.find("_") > -1:
                basinName = basinName.replace("_", " ")


        areaDict = {"areaName" : editAreaName,
                    "areaDesc" : editAreaDesc,
                    "basin" : basinName,
                    "lat" : lat,
                    "lon" : lon,
                    "radius" : radius,
                    "pieState" : pieState,
                    }

        # Find any feature/time
        for fDict in self._hsDesc:
            if fDict["featureName"] == featureName:
                if fDict["timePeriod"] == timePeriod:
                    # add it to the existing list
                    fDict["areaList"].append(areaDict)
                   
                    newAreaList = []
                    for d in fDict["areaList"]:
                        newAreaList.append(d['areaName'])
                    newAreaList.sort()
                    return

        # featureName and time not found so make a new one
        self.addNewFeature(featureName, timePeriod, editAreaName,
                           editAreaDesc, lat, lon, radius, pieState)
        
        return

    def removeEditArea(self, editAreaName):
        # Find the feature
        for fDict in self._hsDesc:
            # look for the areaName
            for areaDict in fDict["areaList"]:
                if areaDict["areaName"] == editAreaName:
                    fDict["areaList"].remove(areaDict)

                    if len(fDict["areaList"]) == 0:
                        self._hsDesc.remove(fDict)

                    return

        return
   
    def replaceEditAreaDesc(self, editAreaName, editAreaDesc):
        # Find the feature

        if self._siteID in self._sitesWithBasins:
            basinName = self.getBasinName(editAreaName)
        else:
            basinName = None
           
        if basinName is not None:
            if basinName.find("_") > -1:
                basinName = basinName.replace("_", " ")
               
        for fDict in self._hsDesc:
            # look for the areaName
            for areaDict in fDict["areaList"]:
                if areaDict["areaName"] == editAreaName:
                    areaDict["areaDesc"] = editAreaDesc
                    areaDict["basin"] = basinName
                    return

        # Didn't find areaName

        return

    def getEditAreaDesc(self, editAreaName):
        for fDict in self._hsDesc:
            # look for the areaName
            for areaDict in fDict["areaList"]:
                if areaDict["areaName"] == editAreaName:
                    return areaDict["areaDesc"]

        return

    def getAllEditAreaNames(self):
        areaNameList = []
        for fDict in self._hsDesc:
            for areaDict in fDict["areaList"]:
                areaNameList.append(areaDict["areaName"])

        return areaNameList

    def extractFeature(self, name):

        parts = name.split("_")

        return parts[0]
   
    def extractTimePeriod(self, name):

        parts = name.split("_")

        return parts[1]
        
    # Extracts the directory and removes the fileName
    def extractDirName(self, path):
        parts = path.split("/")
        dirName = ""
        for i in range(len(parts) - 1):
            dirName = dirName + parts[i] + "/"
       
        return dirName
   
    def getNonLatLonText(self, areaDesc):
        
        areaDescOld = copy.copy(areaDesc)
        openBrace = areaDescOld.find(self._descDelimStart)
        closeBrace = areaDescOld.find(self._descDelimEnd)
     
        preText = areaDescOld[0:openBrace]
        postText = areaDescOld[closeBrace+1:]
        
        return preText, postText

    def clipDescAreas(self, hsDesc, clipAreaName):
        # make a copy so we don't corrupt the original version of the descriptor
        descList = []
        
        if clipAreaName in self._allEditAreaNames:
            clipMask = self._eaUtils.fetchEditArea(clipAreaName)
        else:
            self.statusBarMsg("Edit area: " + clipAreaName + " not found in repository.", "S")
            return descList
           
        for desc in hsDesc:
            # Process the editAreas first
            areaList = desc["areaList"]
            newAreaList = []
           
            for area in areaList:
                # Get the original edit area and clip to the clipArea
                oldAreaMask = self._eaUtils.fetchEditArea(area["areaName"])
                if oldAreaMask is None:
                    self.statusBarMsg("Edit area: " + clipAreaName + " not found in repository.", "S")
                    continue
                
                clippedMask = oldAreaMask & clipMask                      

                if sum(sum(clippedMask)) == 0: # Don't process this area
                    continue
               
                newEditArea = self.decodeEditArea(clippedMask)
                # save this newly clipped area with a different name
                newAreaName = area["areaName"] + clipAreaName
                self._eaUtils.saveEditArea(newAreaName, clippedMask)
                
                lat = None
                lon = None
                radius = None
                pieState = None
                
                if area["lat"] is not None:
                    lat = area["lat"]
                    lon = area["lon"]
                    radius = area["radius"]
                    pieState = area["pieState"]
                
                if len(area["areaDesc"]) < 100:
                    freshAreaDesc = area["areaDesc"]
                else:
                    # Extract the positions of the text before and after the lat/lon text
                    oldAreaDesc = copy.copy(area["areaDesc"])
                    newAreaDesc = self.makeEditAreaDesc(newEditArea, newAreaName, lat, lon, radius, pieState)

                    if area["areaDesc"].find(self._descDelimStart) > -1:
                        # Extract the positions of the text before and after the lat/lon text
                        preText, postText = self.getNonLatLonText(oldAreaDesc)
                        # Restore the pre and post text
                        freshAreaDesc = preText + newAreaDesc + postText
                        
                newAreaDict = {
                               "areaName" : newAreaName,
                               "areaDesc" : freshAreaDesc,
                               "basin" : area["basin"],
                               "lat" : None,
                               "lon" : None,
                               "radius" : None,
                               "pieState" : None,
                               }
                newAreaList.append(newAreaDict)
           
            if len(newAreaList) > 0:
                descDict = {}
                descDict["timePeriod"] = desc["timePeriod"]
                descDict["featureName"] = desc["featureName"]
                descDict["basin"] = desc["basin"]
                descDict["areaList"] = newAreaList
           
                descList.append(descDict)

        return descList
    
    def removeTextBoxDelims(self, highSeasDescriptor):
        
        for descDict in highSeasDescriptor:
            for areaDict in descDict["areaList"]:
                areaDict['areaDesc'] = areaDict['areaDesc'].replace("{", "")
                areaDict['areaDesc'] = areaDict['areaDesc'].replace("}", "")
                
        return

    # This method does the work of writing the descriptor information to a
    # file. This may be called several times one for the tool info and again for
    # the formatter info. In addition some sites may create two descriptor files
    # one north of the equator and one south.
    def writeHSDescriptorToFile(self, highSeasDescriptor, highSeasFileName):

        if not os.path.exists(highSeasFileName):
            # See if the directory exists and if not make it
            dirOnly = self.extractDirName(highSeasFileName)
            # make the fill directory path
            os.makedirs(dirOnly, exist_ok=True)
       
        # open the file for write
        setPermissions = False
        if not os.path.exists(highSeasFileName):  # it's a new file set permissions
            setPermissions = True
        # open the file for write        
        
        try:
            with open(highSeasFileName, "wb") as f:
                pickle.dump(highSeasDescriptor, f)
            if setPermissions:
                os.chmod(highSeasFileName, self._permissions)
        except:
            msg = "Error opening " + highSeasFileName + " for write."
            self.statusBarMsg(msg, "U")

    # Contains the logic for saving the descriptor info to files.
    def saveHSDescToFile(self):
        
        print("in saveHSDescToFile................................................")
        # First save the info for the tool 
        highSeasFileName = self.descriptorFileName("Tool")
        self.writeHSDescriptorToFile(self._hsDesc, highSeasFileName)
       
        # Now save the info for the formatter.
        # Find out what we need to save
        saveAreas = []
        if self._siteID in self._multiProductSites:
            saveAreas = self._multiProductSites[self._siteID]
       
        # Otherwise we're making two files, but we need to make two new
        # descriptors with the areas clipped to the savedAreas

        for area in saveAreas:
            newHSDesc = self.clipDescAreas(self._hsDesc, area)
       
            # remove the delimiters from the descriptors for the formatter version
            self.removeTextBoxDelims(newHSDesc)
            highSeasFileName = self.descriptorFileName("Formatter", area)
            self.writeHSDescriptorToFile(newHSDesc, highSeasFileName)
           
        # Now sample the weather grids for interesting weather, save that in
        # a different list of dictionaries and save that to a file using the
        # same structure.
        print("interesting weather.............................................")
        self.getInterestingWeather()
       
        # reset the editsMade flag
        self._editsMade = False

        return
   
    def fetchWxGrids(self):
        startTime = int(time.time() / (6 * 3600)) * (6 * 3600)  # truncated to last 6 hour period
        endTime = startTime + (48 * 3600) + 1  # 60 hours later
       
        trList = self.getWEInventory("Fcst", "Wx")
       
        wxGridList = []
        for t in range(startTime, endTime, 24*3600):
            # Find the grid that overlaps the first hour of the period in which we're interested
            tr = TimeRange.TimeRange(AbsTime.AbsTime(t), AbsTime.AbsTime(t+ 3600))
            for invTR in trList:
                if tr.overlaps(invTR):
                    wxGrid = self.getGrids("Fcst", "Wx", "SFC",  tr)
                    timePeriod = (t - startTime) // 3600  # hours from start
                    wxGridList.append((wxGrid, timePeriod))
               
        return wxGridList
   
    def getToolWxTypeIntens(self, wxType):
       
        intens = wxType[-1]
        if intens == "+" or intens == "-":
            typeOnly = wxType[:-1]
        else:
            intens = "m"
            typeOnly = wxType

#JL Testing "T"; currently reporting based on Intensity and not Coverage for "T" 05/30/2016
        if typeOnly in ["T", "K", "VA", "F"] and intens == "m":
            intens = "<NoInten>"

        return typeOnly, intens
   
    # Returns a mask where all points with the specified wxType are set to 1
    def getMaskForWxType(self, wxGrid, wxType):
        byteGrid, wxKeys = wxGrid
        wxMask = np.zeros(byteGrid.shape, np.bool)
        for wxKey in wxKeys:
            # get subKeys
            subKeys = wxKey.split("^")
            for subKey in subKeys:
                wxParts = subKey.split(":")
                wxStr = wxParts[1] + wxParts[2]
                if wxStr == wxType:
                    wxIndex = self.getIndex(wxKey, wxKeys)
                    mask = np.equal(byteGrid, wxIndex)
                    wxMask = mask | wxMask

        return wxMask
   
   
    # Discovers the individual sub edit areas of the specified editArea and
    # saves them as individual edit areas.
    def getContiguousEditAreas(self, editArea):
       
       
        # Make a dummy empty editArea that we will reset
        emptyEditAreaName = "EmptyEditArea"
        bitGrid = editArea.getGrid()
        locs = bitGrid.getContiguousBitArrayLocations()
        editAreaList = []
        for i in range(len(locs)):
            newGrid = bitGrid.contiguousBitArray(locs[i])
           
            # Make a dummy empty editArea that we will reset
            emptyMask = np.zeros(self.getGridShape(), np.bool)
            newEditArea = self.decodeEditArea(emptyMask)
            self.saveEditArea(emptyEditAreaName, newEditArea)
            # Now use the edit area
            newEditArea = self.getEditArea(emptyEditAreaName)
            newEditArea.setGrid(newGrid)
#             eaMask = self.encodeEditArea(newEditArea)  # Why do this?
            editAreaList.append(newEditArea)
           
        return editAreaList
    
    # Determines if the specified WxKey matches the weather combinations defined in
    # wxTypeList. If a match is found, return the wx coverage, type, and intensity,
    # otherwise return None.
    def matchingWxKey(self, wxKey, wxTypeList):
        # Examine each sub key
        subKeys = wxKey.split("^")
        for subKey in subKeys:
            wxParts = subKey.split(":")
            # Extract the coverage, type and intensity
            if len(wxParts) >= 4:
                wxCov = wxParts[0] 
                wxType = wxParts[1] 
                wxIntens = wxParts[2] 
            # Check each allowed type for a match
            for wxTypeIntens in wxTypeList:
                allowedType, allowedIntens = self.getToolWxTypeIntens(wxTypeIntens)
                if wxType == allowedType and wxIntens == allowedIntens:
                    # We're only interested in T if coverage matches one in self._thunderCoverages EXCEPT
                    # for the case Iso T m (moderate) where we will convert to Sct T- to represent 
                    # Scattered thunderstorms.
                    if "T" in wxType and wxCov == "Iso":
                        return wxCov, allowedType, allowedIntens

                    if "T" in wxType and wxCov not in self._thunderCoverages:
                        continue
                    
                    return wxCov, allowedType, allowedIntens
        
        # If we get to here, no match was found
        return None
       
    def makePeriodList(self, wxGrid, timePeriodStr, area):
        
        # Dictionary used to convert GFE intensities since it's part of an edit area name
        intenDict = {
                     "+" : "h",
                     "m" : "m",
                     "<NoInten>" : "n",
                     "-" : "l",
                     }
       
        byteGrid, wxKeys = wxGrid

        # Figure out the Wx types and intensities allowed for the HSF
        # Filter out all other Wx type and intensities but these.
        wxTypeList = self._allPeriodWxTypes
        if timePeriodStr == "00h":
            wxTypeList = wxTypeList + self._firstPeriodWxTypes

        areaList = [] # There's a chance that we won't find any wx so define this here
        areaMask = self.encodeEditArea(area)
        for wxKey in wxKeys:
            # Returns coverage, type, and intensity of any matching sub key
            covTypeIntens = self.matchingWxKey(wxKey, wxTypeList)
            
            if covTypeIntens is None:
                continue
            
            wxCov, wxType, wxIntens = covTypeIntens  # extract the components
            
            # Change IsoTm to SctT- so we can report scattered thunderstorms
            if wxCov == "Iso" and wxType == "T" and wxIntens == "<NoInten>":
                wxCov = "Sct"
                wxIntens = "-"

            wxIndex = self.getIndex(wxKey, wxKeys)
            mask = (byteGrid == wxIndex)
            mask = mask & self._clipMask
            if np.sum(mask) == 0:  # no points found with this wxType
                continue 
            
            # Make an edit area from the mask
            wxEditArea = self.decodeEditArea(mask)
            # Get the individual contiguous areas from the edit area 
            wxEAList = self.getContiguousEditAreas(wxEditArea)

            for eaNum, ea in enumerate(wxEAList):
                # Only include edit areas that overlap the current domain
                eaMask = self.encodeEditArea(ea)
                overlap = eaMask & areaMask
               
                if not overlap.any():
                    print("No overlap for ", wxType)
                    continue
                
                intenStr = intenDict[wxIntens]
                eaName = "Z" + timePeriodStr + wxCov + wxType + intenStr + str(eaNum).zfill(2)
                
                self._eaUtils.saveEditArea(eaName, eaMask)

                eaDesc = self.makeEditAreaDesc(ea, eaName)
                areaDict = {}
                areaDict["timePeriod"] = timePeriodStr
                areaDict["areaName"] = eaName
                areaDict["areaDesc"] = eaDesc
                areaDict["wxType"] = wxType
                areaDict["wxCoverage"] = wxCov
                areaDict["intensity"] = wxIntens
                
                areaList.append(areaDict)
                     
        return areaList
               
    def getInterestingWeather(self):
        # Fetch the Wx grids we need
        wxGridList = self.fetchWxGrids()
       
        saveDomains = []
        if self._siteID in self._multiProductSites:
            saveDomains = self._multiProductSites[self._siteID]
        else:
            self.statusBarMsg("Domains not defined for site:" + siteID, "S")
            self.statusBarMsg("Grid based features will not be saved." "S")
            return
        for domain in saveDomains:
       
            gridBasedFeatures = []
            
            for wxGrid, timePeriod in wxGridList:
                timePeriodStr = str(timePeriod).zfill(2) + "h"
                periodList = self.makePeriodList(wxGrid, timePeriodStr, domain)
                gridBasedFeatures = gridBasedFeatures + periodList
            
            # Strip and leading and trailing braces        
            for gFeature in gridBasedFeatures:
                gFeature["areaDesc"] = gFeature["areaDesc"].replace("{", "")
                gFeature["areaDesc"] = gFeature["areaDesc"].replace("}", "")

            wxFeaturesFileName = self.gridBasedFileName(domain[-3:])
            
            self.writeFeatureList(gridBasedFeatures, wxFeaturesFileName)
       
        return
    
    def similarArea(self, area1, area2):
        
        eaName1 = area1["areaName"]
        eaName2 = area2["areaName"]
        
        mask1 = self._eaUtils.fetchEditArea(eaName1)
        mask2 = self._eaUtils.fetchEditArea(eaName2)
        
        overlap = mask1 & mask2
        
        if overlap.any():
            return True
        
        return False

    def findSimilarFeature(self, area):
        # Extract the part from the area
        wxType = area["wxType"]
        wxCov = area["wxCoverage"]
        wxIntens = area["intensity"]
        
        for group in self._sortedFeatures:
            # Check for the same weather
            if group[0]["wxType"] != wxType or group[0]["wxCoverage"] != wxCov or group[0]["intensity"] != wxIntens:
                continue
            # Check for similar area
            if self.similarArea(area, group[0]):
                return group

        return None
        
    def groupGridBasedFeatures(self, gridBasedFeatures):
        # Populate the sorted features with the first area if we have any features
        self._sortedFeatures = []
        if len(gridBasedFeatures) == 0:
            return
        
        self._sortedFeatures = [[copy.copy(gridBasedFeatures[0])]]
        
        for i in range(1, len(gridBasedFeatures)):
            simGroup = self.findSimilarFeature(gridBasedFeatures[i])
            if simGroup is not None: # make a new group
                simGroup.append(copy.copy(gridBasedFeatures[i]))
                continue
            else:
                self._sortedFeatures.append([copy.copy(gridBasedFeatures[i])])
        return
   
    def gridBasedFileName(self, subArea):
        dirPath = "/data/local/HighSeas/Formatter/" + self._siteID + "/HSF_" + subArea + "GridBasedFeatures.pic"
        
        return dirPath

    # Writes the gridBased features to a file.
    def writeFeatureList(self, gridBasedFeatures, gridBasedFileName):
                
        self._sortedFeatures = []
        self.groupGridBasedFeatures(gridBasedFeatures)
        
        if not os.path.exists(gridBasedFileName):
            # See if the directory exists and if not make it
            dirOnly = self.extractDirName(gridBasedFileName)
            # make the fill directory path
            os.makedirs(dirOnly, exist_ok=True)
        
        # open the file for write
        setPermissions = False
        if not os.path.exists(gridBasedFileName):  # it's a new file set set permissions
            setPermissions = True
        try:   
            with open(gridBasedFileName, "wb") as f:
                pickle.dump(self._sortedFeatures, f)
                if setPermissions:
                    os.chmod(gridBasedFileName, self._permissions)
        except:
            msg = "Error opening " + gridBasedFileName + " for write."
            self.statusBarMsg(msg, "U")
     
        return


    def dumpArea(self, areaName):
       
        for fDict in self._hsDesc:
            for areaDict in fDict["areaList"]:
                if areaDict["areaName"] == areaName:
                    print("-------------------------------------")
                    print("Feature Name:", fDict["featureName"])
                    print("Time Period:", fDict["timePeriod"])
                    print("AreaName:", areaDict["areaName"])
                    print("AreaDesc:", areaDict["areaDesc"])
                    print("Basin:", areaDict["basin"])
                    print("Lat:", areaDict["lat"])
                    print("Lon:", areaDict["lon"])
                    print("Radius:", areaDict["radius"])
                    print("PieState", areaDict["pieState"])
        return

    def dumpDatabase(self):

        areaList = []
        print()
        print("+++++++++++++++  START Dump of database   +++++++++++++++")
        for fDict in self._hsDesc:
            for areaDict in fDict["areaList"]:
                areaList.append(areaDict["areaName"])
        areaList.sort()
        for areaName in areaList:
            self.dumpArea(areaName)

        print("+++++++++++++++  END Dump of database   +++++++++++++++")
        return

    def getNewFeatureName(self):
        # make a list of possibleNames
        featureNameList = []
        for fDict in self._hsDesc:
            featureNameList.append(fDict["featureName"])

        num = 1
        while True:
            featureName = "Feature" + str(num)
            if featureName not in featureNameList:
                return featureName

            num = num + 1
            if num > 1000:  # ininite loop prevention
                break

        print("Error, no new feature for getNewFeatureName")
        return ""

    def getNewAreaName(self, featureName, timePeriod):
        count = 0
        for fDict in self._hsDesc:
            if fDict["featureName"] == featureName and fDict["timePeriod"] == timePeriod:
                count = count + len(fDict["areaList"])

        name =  featureName + "_" + str(timePeriod) + "_" + "EA" + str(count+1)

        return name
    
    # Reads the descriptor from the file.
    def initializeHSDescriptor(self):

        highSeasFileName = self.descriptorFileName("Tool")

        try:
            with open(highSeasFileName, "rb") as f:
                self._hsDesc = pickle.load(f)
        except:
            msg = "Descriptor file not found. Starting with an empty descriptor."
            self.statusBarMsg(msg, "S")
            self._hsDesc = []
       
        self._editsMade = False

   
    def makeClipMask(self, clipEditAreas):
       
        clipMask = None
        for editArea in clipEditAreas:
            if editArea in self._allEditAreaNames:
                mask = self._eaUtils.fetchEditArea(editArea)
                if clipMask is None:
                    clipMask = mask
                else:
                    clipMask = clipMask | mask
            else:
                self.statusBarMsg(editArea + " edit area not found for clip area", "S")
       
        return clipMask

    ##########################################################################

    ####   END -----Data structure code

    ##########################################################################


    # Main method that glues all of the GIU pieces together.  Creates
    # all the frames used by other widgets and calls other methods
    # to create buttonsn listboxes, and the status window.
    def setUpUI(self):

        # create the main objects
        self._tkmaster = tkinter.Tk()
           
        self._master = tkinter.Toplevel(self._tkmaster)
        self._tkmaster.withdraw()
       
        # Capture the "x" click to close the GUI
        self._master.protocol('WM_DELETE_WINDOW', self.cancelCommand)

        self._master.title('High Sea Edit Areas')

        self._topFrame = tkinter.Frame(self._master)
        self._topFrame.grid()

        self._timeFrame = tkinter.Frame(self._topFrame, bd=2, relief=tkinter.GROOVE)
        self._timeFrame.grid(row=0, column=0, sticky=tkinter.N)
        self._timeVar = tkinter.StringVar()
        self.makeTimeButtons(self._timeFrame)

        self._listFrame = tkinter.Frame(self._topFrame)
        self._listFrame.grid(row=0, column=1, padx=15)
        self.makeListBox()

        self._listButtonFrame = tkinter.Frame(self._listFrame, bd=2, relief=tkinter.GROOVE)
        self._listButtonFrame.grid(row=2, column=0, columnspan=2, pady=5)

        self._saveNewButton = tkinter.Button(self._listButtonFrame, text="ADD New Feature",
                                command=self.saveNewCommand, width=25)
        self._saveNewButton.grid(row=0, column=0)

        self._saveSelButton = tkinter.Button(self._listButtonFrame, text="SAVE To Selected Feature",
                              command=self.saveSelCommand, width=25,
                              state=tkinter.DISABLED)
        self._saveSelButton.grid(row=1, column=0)
       
        self._replaceSelButton = tkinter.Button(self._listButtonFrame,
                                 text="REPLACE Selected Area", command=self.replaceSelCommand,
                                 width=25, state=tkinter.DISABLED)
        self._replaceSelButton.grid(row=2, column=0)
       
        self._clearSelButton = tkinter.Button(self._listButtonFrame,
                               text="REMOVE Selected Area(s)",
                               command=self.clearSelCommand, width=25,
                               state=tkinter.DISABLED)
        self._clearSelButton.grid(row=3, column=0)

        self._clearAllButton = tkinter.Button(self._listButtonFrame,
                               text="REMOVE All Areas",
                               command=self.clearAllCommand, width=25)
       
        self._clearAllButton.grid(row=4, column=0)

        self._killSelButton = tkinter.Button(self._listButtonFrame,
                               text="KILL Selected Area",
                               command=self.killSelCommand, width=25,
                               state=tkinter.DISABLED)
        self._killSelButton.grid(row=5, column=0)


        self._circleRadius = 80
        self._circleOffset = 20
        self._polarFrame = tkinter.Frame(self._topFrame, bd=2, relief=tkinter.GROOVE)
        self._polarFrame.grid(row=0, column=2, padx=20)
        self._pieSelectColor = "red"
        self._pieDeselectColor = "gray80"
        if self._displayPolarWidgets:
            self.makePolarWidgets()

        self._textFrame = tkinter.Frame(self._topFrame)
        self._textFrame.grid(row=2, column=0, columnspan=3, pady=20)
        self._textBox = tkinter.Text(self._textFrame, height=3, width=60, wrap=tkinter.CHAR,
                                     foreground="black", font=self._boldFont)
        self._textBox.grid(row=0, column=0, padx=10)
        self._textBox.bind("<ButtonRelease-1>", self.textBoxClick)
        self._textBox.bind("<KeyRelease-Return>", self.textBoxReturnPress)
        self._textBox.bind("<KeyPress>", self.textBoxKeyPress)
        self._textLabel = tkinter.Label(self._textFrame, text="Edit Area Description")
        self._textLabel.grid(row=1, column=0)

        self._bottomFrame = tkinter.Frame(self._topFrame)
        self._bottomFrame.grid(row=3, column=0, columnspan=3)       
        self.makeBottomButtons()
       
        self._master.bind("<Enter>", self.enterEvent)

        return

    # Returns the name of the file used to store the edit area information.
    # The appType can be "Tool" or "Formatter" only
    # The subArea parameter will be used for sites creating more than one
    # High Seas product
    def descriptorFileName(self, appType, subArea = ""):

        dirPath = "/data/local/HighSeas/" + appType + "/"

        fileName = dirPath + self._siteID + "/" + subArea + "HighSeasDescriptors.pic"
                       
        return fileName
   
    def makeNamedAreaMasks(self, namedAreaDict):
       
        namedAreaMasks = {}
        for eaName in namedAreaDict:
            eaMask = self._eaUtils.fetchEditArea(eaName)

            if eaMask is None:
                self.statusBarMsg("Edit area: " +  eaName + " not found in repository.", "S")
                continue
           

            namedAreaMasks[eaName] = eaMask
           
        return namedAreaMasks
   
    # Defines the local effect edit areas. These will be added to the lat/lon descriptors
    # to better identify the area.
    # These are areas for which we look for local effects. Format: Edit area name : Description
    def defineNamedAreas(self):
        
        allAreas = {
                    "le_cabocorrientes" : "CABO CORRIENTES",
                    "CALIFORNIA" : "GULF OF CALIFORNIA",
                    "le_pmz011" : "SEBASTIAN VIZCAINO BAY",
                    "le_pmz013" : "WITHIN 60 NM OF SHORE",
                    "le_pmz015" : "WITHIN 60 NM OF SHORE",
                    "le_tehuantepec" : "THE GULF OF TEHUANTEPEC",
                    "le_panama" : "THE GULF OF PANAMA",
                    "le_pmz115" : "NEAR THE AZUERO PENINSULA",
                    "le_papagayo" : "THE GULF OF PAPAGAYO",
                    "le_pmz119" : "THE GULF OF GUAYAQUIL",
                    "le_pmz123" : "LEE OF GALAPAGOS ISLANDS",
                    "le_gmz021_straits_of_florida" : "STRAITS OF FLORIDA",
                    "le_gmz023_s_of_21n_w_of_95w" : "WITHIN 60 NM OF COAST OF VERACRUZ",
                    "le_gmz025_60nm_of_campeche" : "WITHIN 60 NM OF COAST OF CAMPECHE",
                    "le_amz011_yucatan_channel" : "IN YUCATAN CHANNEL",
                    "le_amz013_cuba_jamaica" : "BETWEEN CUBA AND JAMAICA",
                    "le_gulf_of_honduras" : "GULF OF HONDURAS",
                    "le_amz023_mona_swell" : "IN MONA PASSAGE",
                    "le_amz025_atlc_exposures_and_passages" : "IN ATLANTIC EXPOSURES AND PASSAGES",
                    "le_amz029_nicaraguan_coast" : "WITHIN 60 NM OF COAST OF NICARAGUA",
                    "le_amz031_colombian_coast" : "WITHIN 90 NM OF COAST OF COLOMBIA",
                    "le_amz033_gulf_of_venezuela" : "GULF OF VENEZUELA",
                    "le_amz035_atlantic" : "ATLANTIC EXPOSURES",
                    "le_amz117_atlc_exposures" :"ATLANTIC EXPOSURES",    
                    "le_windward_passage" :"APPROACH TO WINDWARD PASSAGE",
                    }
        # Make sure we hvae the edit area before including it in the returned list
        allEANames = self._eaUtils.allEditAreaNames()
        namedAreaDescrip = {}
        for (eaName, value) in allAreas.items():
            if eaName in allEANames:
                namedAreaDescrip[eaName] = value
        
        return namedAreaDescrip

    def execute(self, timeRange):

        self._timeRange = timeRange
        
        siteID = self.getSiteID()
        
        #editAreasPath = "/scratch/local/HighSeas/EditAreas/"  # for Boulder development       
        editAreasPath = "/data/local/HighSeas/" + siteID + "/EditAreas/"       

        self._eaUtils = EditAreaUtilities.EditAreaUtilities(editAreasPath)

        self._gridLoc = self.getGridLoc()

        self._latGrid, self._lonGrid = self.getLatLonGrids()
        self._minLat = int(min(self._latGrid.flat))
        self._minLon = int(min(self._lonGrid.flat))
        self._maxLat = int(max(self._latGrid.flat)) + 1.0
        self._maxLon = int(max(self._lonGrid.flat)) + 1.0
        self._degToRad = 2.0  * math.pi / 360.0
        self._latGrid = self._latGrid  * self._degToRad
        self._lonGrid = self._lonGrid  * self._degToRad
       
        self._defaultLat = int((self._minLat + self._maxLat) / 2.0)
        self._defaultLon = int((self._minLon + self._maxLon) / 2.0)

        self._defaultRadius = 200.0
        self._latScaleValue = self._defaultLat + 0.01
        self._lonScaleValue = self._defaultLon + 0.01  
        self._radiusScaleValue = self._defaultRadius + 0.01
        self._pieState = [False, False, False, False, False, False, False, False]
        self._defaultTime = "00h"
        self._normalFont = ("helvetica", "12", "normal")
        self._boldFont = ("helvetica", "12", "bold")
        self._textBoxCurSelection = None

        self._allEditAreaNames = self._eaUtils.allEditAreaNames()
        
        
        # Configurable section. Probably will get moved to separate config file.
        self._siteID = self.getSiteID()
        self._sitesWithBasins = ["NH2"]
        self._basinNames = ["ATLC", "GULF_OF_MEXICO", "CARIBBEAN", "HSF_NP", "HSF_SP", "HSF_AT1", "HSF_EP1", "HSF_EPi"]
        self._killReasons = ["INLAND.", "MOVED ... OF AREA.", "CONDITIONS MERGED.", "ABSORBED.", "CONDITIONS IMPROVE.",
                             "LITTLE CHANGE.", "NONE."]
        self._defaultKillReason = self._killReasons[-1]
        self._killDesc = self._defaultKillReason
       
        if self._siteID == "HPA":
            self._clipEditAreas = ["HSF_NP", "HSF_SP"]
        elif self._siteID == "NH1":
            self._clipEditAreas = ["HSF_EP2", "HSF_EP3"]
        elif self._siteID == "NH2":
            self._clipEditAreas = ["ATLC", "GOM", "CARIB"] 
        elif self._siteID == "ONA":
            self._clipEditAreas = ["HSF_AT1"] 
        elif self._siteID == "ONP":
            self._clipEditAreas = ["HSF_EP1"] 
        else:
            self._clipEditAreas = []

        self._clipMask = self.makeClipMask(self._clipEditAreas)
        if self._clipMask is None:
            self._clipMask = np.ones(self.getGridShape(), np.bool)
       
        # Define data for sites that make two descriptor files, one for the NH and another for the SH
        # These sites make two products from the same GFE domain. The identifiers for each define the
        # edit area or mask over which the product is valid. All defined features will be clipped to
        # these areas when the descriptors are generated and save to to the file.
        self._multiProductSites = {
                                   "HPA" : ["HSF_NP", "HSF_SP"],
                                   "NH1" : ["HSF_EP2", "HSF_EP3"],
                                   "NH2" : ["HSF_AT2"],
                                   "ONA" : ["HSF_AT1"],
                                   "ONP" : ["HSF_EP1"],
                                   }
       
        self._thunderCoverages = ["Sct", "Num", "Wide"]
        self._firstPeriodWxTypes = ["T", "T+", "VA"]
        self._allPeriodWxTypes = ["F+", "K", "ZY-", "ZY", "ZY+"]
        self._descDelimStart = "{" 
        self._descDelimEnd = "}" 
            
        self._selectingPieSlices = False
        self._deselectingPieSlices = False
       
        self._displayPolarWidgets = True

        self._editsMade = False

        self._selectedEditArea = ""
       
        self._minEASize = 0.75
        self._marginalEASize = 1.0

        self._initialEditArea = self.getActiveEditArea()
        self._enterCount = 0
      
        self._polarEditArea = self._initialEditArea
       
        self._namedAreaDescip = self.defineNamedAreas()
        self._namedAreaMasks = self.makeNamedAreaMasks(self._namedAreaDescip)
       
        self._permissions = stat.S_IRWXU + stat.S_IRWXG + stat.S_IROTH  # 775 permissions on dirs and files

        self.initializeHSDescriptor()
        self.setUpUI()
        tkinter.mainloop()

        return
