# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Aviation_Timer - Version 20180608
#
# Last Modified: 14 February 2018
#
# Author: Tom LeFebvre based on an earlier version.
# ----------------------------------------------------------------------------

MenuItems = ["Populate"]

import AbsTime
import TimeRange

import time
import SmartScript
import Aviation_EDASConfig as TimerConfig
import tkinter as tk
import numpy as np


class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Utility method to make a TimeRange object
    def makeTimeRange(self, start, end):
        
        return TimeRange.TimeRange(AbsTime.AbsTime(start),
                                   AbsTime.AbsTime(end))

    # Creates the timeScale GUI
    def makeTimeScale(self):
        
        wList = self._timeFrame.winfo_children()
        for w in wList:
            w.destroy()

        self._timeCanvas = tk.Canvas(self._timeFrame,width=self._xSize,
                                          height=self._timeCanvasHeight,
                                          bg='gray')
        self._timeCanvas.grid()

        self._timeCanvas.bind("<ButtonPress-" + self._mouseButton + ">", self.markerPress)
        self._timeCanvas.bind("<ButtonRelease-" + self._mouseButton + ">", self.markerRelease)
        #self._timeCanvas.bind("<Leave>", self.markerRelease)
        self._timeCanvas.bind("<Motion>", self.markerMotion)          


        # Make a horizontal line
        x0 = self.timeToX(self._startTime)
        x1 = self.timeToX(self._endTime)
        self._timeCanvas.create_line(x0, 1, x1, 1, fill='black')
           
        self.drawTimeMarker(self._editStartTime)
        self.drawTimeMarker(self._editEndTime)
        for t in range(self._startTime, self._endTime + 1, 3600):                
            tupleTime = time.gmtime(t)
            dayStr = str(tupleTime.tm_mday).zfill(2)
            hourStr = str(tupleTime.tm_hour).zfill(2)
            timeStr = dayStr + "." + hourStr
            x0 = self.timeToX(t)
            y0 = 0
            tickHeight = self._tickHeight
            if tupleTime.tm_hour % self._labelFreq == 0:
                tickHeight = tickHeight * 2 
            
            y1 = y0 + tickHeight
            self._timeCanvas.create_line(x0, y0, x0, y1, fill='black')


            if tupleTime.tm_hour % self._labelFreq == 0: 
                self._timeCanvas.create_text(x0, y1 + self._timeLabelOffset,
                                             text=timeStr, fill='black')
        return

    # Returns the coordinate of the marker associated with the specifiedTime
    def markerCoords(self, markerTime):
        
        startX = self.timeToX(markerTime)

        x1 = startX - self._markerWidth // 2
        y1 = self._markerBottomY + self._markerHeight
        x2 = startX + self._markerWidth // 2
        y2 = self._markerBottomY + self._markerHeight
        x3 = startX
        y3 = self._markerBottomY
        
        return x1, y1, x2, y2, x3, y3
        
    # Draws the time marker at the specified time
    def drawTimeMarker(self, markerTime):

        startX = self.timeToX(markerTime)

        if markerTime == self._editStartTime:
            color = "green"
        elif markerTime == self._editEndTime:
            color = "red"
        
        self._timeCanvas.create_line(startX, self._markerTopY,
                                     startX, self._markerBottomY, fill=color)

        x1, y1, x2, y2, x3, y3 = self.markerCoords(markerTime)

        self._timeCanvas.create_polygon(x1, y1, x2, y2, x3, y3, fill=color,
                                        outline="black")

        return
    
    # Called when any time marker is pressed
    def markerPress(self, event):
        # Figure out if the start marker was pressed
        self._pressTime = self.xToTime(event.x)

        closeEnough = 600
        if abs(self._editStartTime - self._pressTime) < closeEnough:
            self._adjStartMarker = True
        elif abs(self._editEndTime - self._pressTime) < closeEnough:
            self._adjEndMarker = True
        return

    # Called when any time marker is moved or draged
    def markerMotion(self, event):
        markerTime = self.xToTime(event.x)

        # Round time to nearest hour
        markerTime = int((markerTime + 1800) / 3600) * 3600

        if self._adjStartMarker:
            if markerTime != self._editStartTime:
                if markerTime < self._startTime:
                    markerTime = self._startTime
                # Can't go past end time    
                if markerTime > self._editEndTime - 3600:
                    markerTime = self._editEndTime - 3600
                else:
                    self._editStartTime = markerTime

                self.makeTimeScale()
                return
        elif self._adjEndMarker:
            if markerTime != self._editEndTime:
                if markerTime > self._endTime:
                    markerTime = self._endTime
                # Can't go past start time
                if markerTime < self._editStartTime + 3600:
                    markerTime = self._editStartTime + 3600
                else:
                    self._editEndTime = markerTime

                self.makeTimeScale()
                return

        for weName in self._supportedWEs:
            self.drawTimeSeries(weName)
        return

    # Called when any time marker is released
    def markerRelease(self, event):

        # If it's a click snap the closest time marker
        releaseTime = self.xToTime(event.x)

        # If it's a click, snap the closest time marker
        if releaseTime == self._pressTime:
            startDiff = abs(self._editStartTime - releaseTime)
            endDiff = abs(self._editEndTime - releaseTime)
            if releaseTime < self._editStartTime or startDiff < endDiff:
                self._adjStartMarker = True
                self.markerMotion(event)
            elif releaseTime > self._editEndTime or startDiff >= endDiff:
                self._adjEndMarker = True
                self.markerMotion(event)

        self._adjStartMarker = False
        self._adjEndMarker = False

        return

    def weButtonClicked(self, weName):
        
        if self._weButtons[weName].selected:
            self._weButtons[weName].selected = False
            self._weButtons[weName].config(fg="gray80")
        else:
            color = self._config["colors"][weName]
            self._weButtons[weName].selected = True
            self._weButtons[weName].config(fg=color)
            
        self.drawTimeSeries(weName)
        return
    
    def zoomButtonClicked(self, weName):
        if self._zoomButtons[weName].selected:
            self._zoomButtons[weName].selected = False
            self._zoomButtons[weName].config(text="Zoom In")
        else:
            self._zoomButtons[weName].selected = True
            self._zoomButtons[weName].config(text="Zoom Out")            
            
            # make the vertical scale for values
        vMin, vMax, vTick = self.getScaleValues(weName)
        self.makeValueScale(self._canvasDict[weName], vMin, vMax, vTick, weName)
        self.drawTimeSeries(weName)
        return
    
    

    # Creates the Run Run/Dismiss, and Cancel buttons at the bottom
    def makeBottomButtons(self, frame):
        # Create the Execute/Cancel buttons
        padx = 60
        self._executeButton = tk.Button(frame, text="Run",
                                command=self.executeCommand)
        self._executeButton.grid(row=0, column=0, padx=padx, pady=5)

        self._executeButton = tk.Button(frame, text="Run and Dismiss",
                                command=self.executeDismissCommand)
        self._executeButton.grid(row=0, column=1, padx=padx, pady=5)

        self._cancelButton = tk.Button(frame, text="Cancel",
                                command=self.cancelCommand)
        self._cancelButton.grid(row=0, column=2, padx=padx, pady=5, sticky=tk.W+tk.E)

        frame.grid(columnspan=3, sticky=tk.EW)

    # Converts a time in seconds to the corresponding x-coordinate
    def timeToX(self, timeInSec):

        timeOffset = float(timeInSec - self._startTime) / 3600

        #xCoord = self._xOffset + (timeOffset * self._pixPerHour) + 1
        xCoord = self._xOffset + (timeOffset * self._pixPerHour) + 3

        return xCoord
    
    # Converts the x-coordinate to  time in seconds
    def xToTime(self, xCoord):

        coordTime = self._startTime + (((xCoord - self._xOffset - 3) * 3600) // self._pixPerHour)
        
        return coordTime

#     # Returns the element value based on the y coordinate and the weName
#     def yToValue(self, yCoord, weName):
#         minY = self._yCanvasOffset
#         maxY = self._ySize - (2 * self._yCanvasOffset)
#         minValue, maxValue = self.getParmMinMax(weName)
#                 
#         yScale = (maxY - minY) / (maxValue - minValue)
#         
#         value = -(yCoord - 1 - maxY) / yScale
#         
#         return value
# 
#     # Returns the corresponding y coordinate based on the element value     
#     def valueToY(self, value, weName):
#         minY = self._yCanvasOffset
#         maxY = self._ySize - (2 * self._yCanvasOffset)
#         minValue, maxValue = self.getParmMinMax(weName)
#                 
#         yScale = (maxY - minY) / (maxValue - minValue)
#         
#         y = maxY - (value * yScale) + 1
#         
#         return y

    # Returns the element value based on the y coordinate and the weName
    def yToValue(self, yCoord, weName):
        minY = self._yCanvasOffset
        maxY = self._ySize - (2 * self._yCanvasOffset)
        minValue, maxValue, tick = self.getScaleValues(weName)
        yScale = (maxY - minY) / (maxValue - minValue)
        
        value = -(yCoord - 1 - maxY) / yScale + minValue
        
        return value

    # Returns the corresponding y coordinate based on the element value     
    def valueToY(self, value, weName):
        minY = self._yCanvasOffset
        maxY = self._ySize - (2 * self._yCanvasOffset)
        minValue, maxValue, tick = self.getScaleValues(weName)
                
        yScale = (maxY - minY) / (maxValue - minValue)
        
        y = maxY - ((value - minValue) * yScale) + 1
        
        return y
    # Returns a color string that is a fraction of the specified color's brightness
    def dimColor(self, canvas, colorName, fraction):
        
        rgb = canvas.winfo_rgb(colorName)  # break down into rgb
        r, g, b = rgb[0] // 256, rgb[1] // 256, rgb[2] // 256  # scale to 8-bit color

        # Apply the dim fraction to the rgb values
        color = "#%02x%02x%02x" % (r * fraction, g * fraction, b * fraction)

        return color

    # Make a list of timeRanges starting at the current hour to the configured
    # number of hours
    def makeTimeRangeList(self):
        trList = []
        startTime = int(time.time() / 3600) * 3600
        hours = self._config["timeDuration"]
        for i in range(hours):
            start = startTime + (i * 3600)
            end = start + 3600
            tr = self.makeTimeRange(start, end)
            trList.append(tr)

        return trList

    # Destroys allthe widgets for this GUI
    def exit(self):
        self._master.destroy()

    # Executes the editing process.
    def executeCommand(self):

        # Make the mask composed of all the edit areas
        mask = np.zeros(self.getGridShape(), np.bool)
        for editArea in self._editAreasSelected:
            # Check for selected and fetch the live edit area
            if editArea == "Selected":
                ea = self.getActiveEditArea()
            else:
                ea = self.getEditArea(editArea)
            mask = mask | self.encodeEditArea(ea)
        
        editTimeRange = self.makeTimeRange(self._editStartTime, self._editEndTime)
        for weName in self._supportedWEs:
            
            if not self._weButtons[weName].selected:
                continue
            
            weType = self.getWEType(weName)
            # Fetch the time series data
            timeSeries = self._weDict[weName]["timeSeries"]
            # Fetch the time ranges
            trList = list(timeSeries.keys())
            trList.sort()
            for tr in trList:
                if not editTimeRange.overlaps(tr): # skip TRs outside the edit timeRange
                    continue
                grid = self.getGrids("Fcst", weName, "SFC", tr)  # Fetch the grid
                
                # Only modify points that are "worse" in value than the existing value
                # For Sky this means less than for all others greater than.
                if weName == "Sky":
                    dataMask = mask & (grid < timeSeries[tr])
                else:
                    dataMask = mask & (grid > timeSeries[tr])
                    
                grid[dataMask] = timeSeries[tr]   # poke in the data
                
                
                
                self.createGrid("Fcst", weName, weType, grid, tr) # Save to Fcst database
                
        self.statusBarMsg("All grids updated.", "R")
        
        return
    
    # Called when the Run/Dismiss button is clicked.
    def executeDismissCommand(self):
        self.executeCommand()
        self.cancelCommand()
    
    # Called when the Cancel button is clicked
    def cancelCommand(self):

        self._tkmaster.destroy()
            
        return

    def mouseEvent(self, event):
        # determine if the event is a button down
        if event.type == "4":  # button down event
            self._buttonDown = True
        elif event.type == "5":  # button up event
            self._buttonDown = False
        
        # Check for button down and up events
        if event.num == int(self._mouseButton) and event.state in [16, 17]:
            self._buttonDown = True
        # Button 1 was released
        elif event.num == int(self._mouseButton) and event.state in [272, 1040]:
            self._buttonDown = False
            
        # Find the weather element that was clicked upon
        for weName in self._supportedWEs:
            if self._weDict[weName]["canvas"] == event.widget:
                self._activeWE = weName
                break
            
        # Get weather element limits
        minValue, maxValue = self.getParmMinMax(weName)

        # check for shift button state. The last bit indicates the shift state.
        if event.state & 0x0001:  # shift is down
            if not self._shiftButtonDown: # shift just pressed so read and set value
                self._grabValue = self.yToValue(event.y, self._activeWE)
                self._grabValue = np.clip(self._grabValue, minValue, maxValue)

            self._shiftButtonDown = True
        else: #shift is up
            self._shiftButtonDown = False
            self._grabValue = None
            
        # Only interested in doing anything when button is down
        if not self._buttonDown:
            self.removeSample(self._activeWE)
            return

        # Fetch the time that was clicked upon
        t = int(self.xToTime(event.x))
        if t < self._startTime or t > self._endTime:
            return
            
        if not self._weButtons[self._activeWE].selected:
            return
        
        # Get the y-value that was clicked or dragged upon
        # Use the grabbed value if its valid
        if self._grabValue is not None:
            value = self._grabValue
        else:  # shift button up - use the current value
            value = self.yToValue(event.y, self._activeWE)
            value = np.clip(value, minValue, maxValue)
                
        # Change the value in the time series data
        trList = sorted(self._weDict[weName]["timeSeries"].keys())
        # Find the click upon timeRange and set the value in the time series data
        for tr in trList:
            if tr.contains(AbsTime.AbsTime(t)):
                self._weDict[weName]["timeSeries"][tr] = value
                break

        # Update the time series display including the sampleI
        self.drawTimeSeries(weName)
        self.drawSample(weName, event.x, event.y, value)
        
        return
    
    # Draws the vertical value scale
    def makeValueScale(self, canvas, minValue, maxValue, tickInt, weName):
               
        minY = self._yCanvasOffset
        maxY = self._ySize - (2 * self._yCanvasOffset)
        minX = self.timeToX(self._startTime)
        maxX = self.timeToX(self._endTime)
        # Scaling for y
        yScale = (maxY - minY) / (maxValue - minValue)
        
        tagName = weName
        canvas.delete(tagName)
        
        # Box the time series
        canvas.create_line(minX, minY, minX, maxY, fill="black", tags=tagName) # left
        canvas.create_line(minX, maxY, maxX, maxY, fill="black", tags=tagName) # bottom
        canvas.create_line(maxX, minY, maxX, maxY, fill="black", tags=tagName) # right
        canvas.create_line(minX, minY, maxX, minY, fill="black", tags=tagName) # top

        # Draw the vertical scale with value labels
        for v in range(int(minValue), int(maxValue+1), int(tickInt)):
            y = maxY - ((v-minValue) * yScale)
            x1 = self.timeToX(self._startTime)
            x0 = x1 - 5
            canvas.create_line(x0, y, x1, y, fill="black", tags=tagName)
            labelStr = str(v)
            x = x0 -  4 * len(labelStr)
            canvas.create_text(x, y, text=labelStr, tags=tagName)
            # Draw a horizontal line for better y- value awareness
            canvas.create_line(minX, y, maxX, y, fill="gray", tags=tagName)
        
        # Draw shadow lines to indicate time
        for t in range(self._startTime, self._endTime, 3600):
            if time.gmtime(t).tm_hour % 6 == 0:
                color = "gray50"
            else:
                color = "gray"
            x = self.timeToX(t)
            canvas.create_line(x, maxY-1, x, minY+1, fill=color, tags=tagName)
        
        
        return
    
    def makeWEButton(self, frame, weName):
        # Plot the weather element name at the bottom
#         x = ((self._xSize) / 2)
#         y = self._ySize - 10
        color = self._config["colors"][weName]
        button = tk.Button(frame, text=weName, command = lambda: self.weButtonClicked(weName),
                           fg=color)
#        button.grid(row=1, column=1, sticky=tk.EW)
        button.grid(row=1, column=0)
        button.selected = True
        return button
    
    def makeZoomButton(self, frame, weName):
        # Check the zoom factor to see if we need one.
        zoomFactor = self._config["zoomFactor"][weName]
        if zoomFactor == 1.0:
            return None
        button = tk.Button(frame, text="Zoom In", command = lambda: self.zoomButtonClicked(weName),
                           fg='black')
        button.grid(row=1, column=0, sticky=tk.W)
        button.selected = False
        return button
    
    # Returns the weather element type
    def getWEType(self, weName):            
        parm = self.getParm("Fcst", weName, "SFC")
        parmType = str(parm.getGridInfo().getGridType())
        return parmType

    # Returns the weather element min and max allowed values
    def getParmMinMax(self, weName):
        minVal, maxVal, ticks = self.getScaleValues(weName)
        return minVal, maxVal
    
    # Returns contrived time series data for the specified element
    def initializeTimeSeries(self, weName):
        
        # Get this elements min/max
        minVal, maxVal = self.getParmMinMax(weName)
        tsDict = {}
        for tr in self._timeRanges:
            tsDict[tr] = (minVal + maxVal) / 2.0
            
        return tsDict
    
    # Draw/redraws the time series display
    def drawTimeSeries(self, weName):        
        # Fetch the data
        tsDict = self._weDict[weName]["timeSeries"]
        
        trList = sorted(tsDict.keys())
        
        # Fetch the canvas
        canvas = self._weDict[weName]["canvas"]
        
        # Remove the old time series lines
        canvas.delete("TimeSeries")
        
        # Set the lastX and lastY which will update as we draw
        lastX = self.timeToX(trList[0].startTime().unixTime()) + 1
        lastY = self.valueToY(tsDict[trList[0]], weName)
        # Make the editable timeRange
        editTimeRange = self.makeTimeRange(self._editStartTime, self._editEndTime)
        
        # Fetch the color for this time series plot from the configuration file
        baseColor = self._config["colors"][weName]

        # Draw the time series
        for tr in trList:
            if editTimeRange.contains(tr) and self._weButtons[weName].selected:
                color = baseColor
            else:
                color = "gray85" # same as background, so it's erased
            # Calculate the left (x0) and right (x1) edges 
            x0 = self.timeToX(tr.startTime().unixTime())
            x1 = self.timeToX(tr.endTime().unixTime())
            # Calculate the Y coordinate
            value = self.closestReportableValue(weName, tsDict[tr])
            y = self.valueToY(value, weName)
            # Draw
            canvas.create_line(lastX, lastY, x0, y, fill=color, width=2, tags="TimeSeries")
            canvas.create_line(x0, y, x1, y, fill=color, width=2, tags="TimeSeries")
            # Update the last coord so we know where to update next time
            lastX = x1
            lastY = y

        return
    
    def drawSample(self, weName, xCoord, yCoord, value):
        # draw the sample
        canvas = self._weDict[weName]["canvas"]
        canvas.delete(self._sample)
        valueStr = self.valueStr(weName, value)
        self._sample = canvas.create_text(xCoord, yCoord-15, text=valueStr, tag="Sample")
        return
    
    def removeSample(self, weName):
        canvas = self._weDict[weName]["canvas"]
        canvas.delete("Sample")
        return
    
    def valueStr(self, weName, rawValue):
        value = self.closestReportableValue(weName, rawValue)
        if weName == "Visibility":
            return str(self.round(value, "Nearest", 0.1)) + "mi"
        elif weName == "Sky":
            return str(int(value)) + "%"
        elif weName in ["CloudBasePrimary", "CloudBaseSecondary"]:
            return str(int(value)) + " ft"
        else:
            print("Unknown weName", weName, "in valueStr method.")

        return None
    
    def closestReportableValue(self, weName, value):
        # Make sure we have the reportable value info 
        if weName not in self._config["reportableValues"]:
            return value

        reportableValues = self._config["reportableValues"][weName]
        
        for start, end, inc in reportableValues:
            if value >= start and value <= end:
                # report the closest value
                return start + int((value - start + (inc / 2.0)) / inc) * inc
                
        print("Value:", value, "not in range of Reportable Values in configuration file.")
        return 

    # Called when any edit area is clicked on or off.
    def editAreaSelected(self, event, editArea):
        self._editAreaDict
        # toggle on
        if editArea not in self._editAreasSelected and \
            self._editAreaDict[editArea].cget("state") == tk.ACTIVE:
            self._editAreasSelected.append(editArea)
            self._editAreaDict[editArea].select()
        # toggle off
        elif editArea in self._editAreasSelected and \
            self._editAreaDict[editArea].cget("state") == tk.ACTIVE:
            if editArea in self._editAreasSelected:
                self._editAreasSelected.remove(editArea)
                self._editAreaDict[editArea].deselect()

        return
    
    # Makes the edit area buttons
    def makeEditAreaButtons(self, frame):
        
        # make a label
        label = tk.Label(frame, text="Select Edit Area")
        label.grid(row=0, column=0)
        # Make each source button
        editAreaList = self._config["editAreaList"]
        defaultEditAreas = self._config["defaultEditAreas"]
        row = 1 # start below the label (above)
        for ea in editAreaList:
            # Make a method on the fly that will be called when any edit area
            # button is clicked.
            def cbHandler(event, self=self, buttonName=ea):
                return self.editAreaSelected(event, buttonName)
            self._editAreaDict[ea] = tk.Checkbutton(frame, text=ea)
            self._editAreaDict[ea].grid(row=row, sticky=tk.W)
            self._editAreaDict[ea].bind("<ButtonRelease-" + self._mouseButton + ">", cbHandler)
            #self._editAreaDict[ea].bind("<ButtonRelease-2>", cbHandler)
            # Select the button if it's in the default list
            if ea in defaultEditAreas:
                self._editAreaDict[ea].select()
                
            row = row + 1
        
        return
    
    def getScaleValues(self, weName):
        vMin, vMax, vTick = self._config["valueDict"][weName]
        zoomFactor = self._config["zoomFactor"][weName]
        if self._zoomButtons[weName] is None:
            return vMin, vMax, vTick
        
        if self._zoomButtons[weName].selected:
            vMax = vMax / zoomFactor
        
        # Sky is a special case
        if weName == "Sky" and self._zoomButtons[weName].selected:
            vMin, vMax, vTick = self._config["valueDict"][weName]
            vMin = vMax - (vMax / zoomFactor)
            vTick = vTick / zoomFactor

        return vMin, vMax, vTick
        
    # Define the coordinates for all of the graphical objects
    def defineGeometry(self):

        # Time Series data geometry
        self._xSize = 600  # width of each canvas
        self._ySize = 150  # height of each canvas
        self._xOffset = 50 # horizontal room for y-scale on left
        
        # Define Geometries for the element frames
        self._yCanvasOffset = 5
        
        # Time Scale geometry
        self._markerTopY = 10
        self._markerBottomY = 25
        self._markerWidth = 14
        self._markerHeight = 14
        self._adjStartMarker = False
        self._adjEndMarker = False
        self._timeCanvasHeight = 40
        self._tickHeight = 7
        self._timeLabelOffset = 7
        self._labelFreq = 6 # hours
        self._deltaTime = self._endTime - self._startTime
        self._pixPerHour = (self._xSize - self._xOffset - 20) // (self._deltaTime // 3600)
        
        return 
        
    # This code displays makes the window display at the cursor location
    def displayWindowOnCursor(self):
        self._master.update_idletasks()
        wh= self._master.winfo_height()
        ww= self._master.winfo_width()
        px, py =self._master.winfo_pointerxy()
        self._master.geometry("%dx%d+%d+%d" % (ww, wh, px - (ww // 2), py - (wh // 2)))
        return

    def setUpUI(self):
        
        # Create the time series canvases
        for weName in self._supportedWEs:
            frame = tk.Frame(self._master, relief=tk.RIDGE, bd=2)
            canvas = tk.Canvas(frame, width=self._xSize, height=self._ySize)
            self._canvasDict[weName] = canvas
            self._weDict[weName]["frame"] = frame
            self._weDict[weName]["canvas"] = canvas
            row = self._supportedWEs.index(weName)
            frame.grid(row=row, column=0)
            canvas.grid(row=0)
            canvas.bind("<Button-" + self._mouseButton + ">", self.mouseEvent)
            canvas.bind("<ButtonRelease-" + self._mouseButton + ">", self.mouseEvent)
            canvas.bind("<Motion>", self.mouseEvent)
            # save interesting stuff in the geoDict
            
            
            self._zoomButtons[weName] = self.makeZoomButton(frame, weName)

            # make the vertical scale for values
            vMin, vMax, vTick = self.getScaleValues(weName)
            self.makeValueScale(canvas, vMin, vMax, vTick, weName)
            
            
            self._weDict[weName]["timeSeries"] = self.initializeTimeSeries(weName)
            self._weButtons[weName] = self.makeWEButton(frame, weName)
            
        # Make the time frame
        row = len(self._supportedWEs)
        self._timeFrame = tk.Frame(self._master)
        self._timeFrame.grid(row=row, sticky=tk.N)

        self._master.title('Aviation_Timer')
               
        self.makeTimeScale()
        
        # Make the edit area buttons
        eaFrame = tk.Frame(self._master, relief=tk.RIDGE, bd=2)
        eaFrame.grid(row = 0, column=1, rowspan=3)
        self.makeEditAreaButtons(eaFrame)
        
        self._bottomFrame = tk.Frame(self._master, relief=tk.RIDGE, bd=2)
        row = row + 1
        self._bottomFrame.grid(row=row)
        self.makeBottomButtons(self._bottomFrame)
        
        self.displayWindowOnCursor()

        return
    
    def execute(self, timeRange):
        # Get the config dictionary for this tool
        self._config = TimerConfig.config["Timer"]

        self._timeRanges = self.makeTimeRangeList()
        
        # Calculate the begin and end of the display
        self._startTime = int(self._timeRanges[0].startTime().unixTime() / 3600) * 3600
        self._endTime = int(self._timeRanges[-1].endTime().unixTime() / 3600) * 3600

        # Set the edit times to this for now
        self._editStartTime = self._startTime
        self._editEndTime = self._endTime
        
        gfeStart = int(timeRange.startTime().unixTime() / 3600) * 3600
        gfeEnd = int(timeRange.endTime().unixTime() / 3600) * 3600
        
        # Set the edit start and end times. These may be changed by the user.
        self._editStartTime = self._startTime
        self._editEndTime = self._endTime
        # Reset the start and end times to what is selected in the GFE, if reasonable
        if gfeStart >= self._editStartTime and gfeStart < self._editEndTime:
            self._editStartTime = gfeStart
        if gfeEnd > self._editStartTime and gfeEnd <= self._editEndTime:
            self._editEndTime = gfeEnd

        self._buttonDown = False
        self._activeWE = None
        self._shiftButtonDown = False
        self._grabValue = None
        self._editAreaDict = {}
        
        self._editAreasSelected = self._config["defaultEditAreas"]
        
        self._colorList = self._config["colors"]

        self._tkmaster = tk.Tk()
        self._master = tk.Toplevel(self._tkmaster)
        self._master.protocol('WM_DELETE_WINDOW', self.cancelCommand)

        self._tkmaster.withdraw()
        
        self._sample = None

        self._mouseButton = self._config["editButton"]

#         self._supportedWEs = ["Visibility", "Sky", "CloudBasePrimary", "CloudBaseSecondary"]
        self._supportedWEs = ["Visibility", "Sky", "CloudBasePrimary"]
        self._zoomButtons = {}
        self._weButtons = {}
        self._weSelected = {}
        self._canvasDict = {}
        # Define graphical constants
        self._weDict = {}  # dictionary where element geometries are stored
        # Make an empty dict for each element
        for weName in self._supportedWEs:
            self._weDict[weName] = {}
        # Fill the geoDict with info 
        self.defineGeometry()
        
        self.setUpUI()
        
        for weName in self._supportedWEs:
            self.drawTimeSeries(weName)

        self._master.mainloop()



