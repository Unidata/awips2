# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# A2TCWindThreatFinal_3
# This version is for use with MLB's Graphical HLS.
# This version modifies the gHLS WindThreat element.
#
# Author: lefebvre 9/11/06
# Modified by: Volkmer/MLB and Santos/MFL 5/23/07
# More modifications: by LeFebvre/Santos/Sharp 05/7/09
#                        LeFebvre/Santos 07/20/10
# Modified: by Santos 04/26/2011 to accomodate alternate logic for NE Coast and hide bars
# Modified: by Santos 04/20/2012 to get rid off Very Low and tweak GUI.
# Modified: by Shannon/Pablo 06/19/2012 to make A2 and DRT compatible
# Modified: by Pablo/Shannon on 05/21/2014 to fix bug introduced in 2012 when Very Low was eliminated
# Last Modified: By Santos/Lefebvre 09/17/2014 to make changes for official implementation in 2015.
# ----------------------------------------------------------------------------
#
# THIS CODE SHALL NOT BE CHANGED WITHOUT CONSULTING ORIGINAL DEVELOPERS.
#
# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Populate","Edit"]

# The ToolList is optional, but recommended, if you are calling
# Smart Tools from your Script.
# If present, it can be used to show which grids will be
# modified by the Script.


VariableList = [("Forecast Confidence?", "Typical (Combined; For ill defined or for most systems anytime within 48 hours of landfall or closest approach)",
                 "radio", ["Typical (Combined; For ill defined or for most systems anytime within 48 hours of landfall or closest approach)",
                           "High (Combined; for well-behaved systems within 12 hours of landfall or closest approach)",
                           "Higher (Combined; for very well-behaved systems within 6 hours of landfall or closest approach)",
                           "Highest (Deterministic-only; assumes a perfect forecast)"]),
                ("WindMax Source?", "WFO Mesoscale (default)", "radio", ["WFO Mesoscale (default)","NHC/TCM Synoptic"]),
                ]

import SmartScript
import string
import TimeRange
import AbsTime
import time
import MetLib
import sys

from numpy import *


class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Finds the TPC prob database, extracts all of the grids and returns
    # the last one of each type
    def getProbGrids(self):
        ap = self.availableParms()
        searchStr = self.getSiteID() + "_GRID_D2D_TPCProb"
        probModel = ""
        for elem, level, model in ap:
            if string.find(model.modelIdentifier(), searchStr) > -1:
                probModel = model.modelIdentifier()
                break

        if probModel == "":
            self.statusBarMsg("TPC Wind probability grids not found.", "S")
            return None, None, None

        # make a big timeRange
        timeRange = TimeRange.allTimes()


        # get the TPC prob grids, and keep the last one
        prob34List = self.getGrids(probModel, "prob34", "FHAG10", timeRange,
                                   mode = "List")
        prob34Grid = prob34List[-1]        

        prob50List = self.getGrids(probModel, "prob50", "FHAG10", timeRange,
                                   mode = "List")
        prob50Grid = prob50List[-1]        

        prob64List = self.getGrids(probModel, "prob64", "FHAG10", timeRange,
                                   mode = "List")
        prob64Grid = prob64List[-1]
            
        return prob34Grid, prob50Grid, prob64Grid

    def getWEInventory(self, modelName, WEName):
        dbTime = self.makeDBTimeRange()
        trList = []

        try:
            gridInfo = self.getGridInfo(modelName, WEName, "SFC", dbTime)
        except:
            return trList
        
        for g in gridInfo:
            start = g.gridTime().startTime().unixTime()
            end = g.gridTime().endTime().unixTime()
            startTime = AbsTime.AbsTime(start)
            endTime = AbsTime.AbsTime(end)
    
            tr = TimeRange.TimeRange(startTime, endTime)
            if dbTime.overlaps(tr):
                trList.append(tr)

        return trList


    # make a timeRange spanning from the current hour to 6 hours hence
    def make6hrTimeRange(self):
        cTime = int(self._gmtime().unixTime()/ 3600) * 3600
        startTime = AbsTime.AbsTime(cTime)
        endTime = AbsTime.AbsTime(cTime + (6 * 3600))   # 6 hours
        tr = TimeRange.TimeRange(startTime, endTime)

        return tr

    # make a timeRange spanning the usual -24 to 10-day db
    def makeDBTimeRange(self):
        cTime = int(self._gmtime().unixTime()/ 3600) * 3600
        startTime = AbsTime.AbsTime(cTime - (24*3600))
        endTime = AbsTime.AbsTime(cTime + (240 * 3600))   # 10 days
        tr = TimeRange.TimeRange(startTime, endTime)

        return tr
    

    # removes all grids from a selected set of temporary parms
    def removeTempGrids(self):
        elementList = ["Prob34", "Prob50", "Prob64", "WindMax"]
        ap = self.availableParms()
        tr = TimeRange.allTimes()
        
        for elem, level, model in ap:
            modelName = model.modelIdentifier()
            if elem in elementList and level == "SFC":
                self.deleteCmd([elem], tr)
        return

    # Smooths the specified grid by the specified factor
    # With factor == 3, 3x3 smooth, factor == 5 5x5 smooth, etc.
    # Even factors (4, 6, 8,...) round up to the next odd value
    # If factors <3 are specified, the unmodified grid is returned.
    def smoothGrid(self, grid, factor=3):
        # factors of less than 3 are useless or dangerous
        if factor < 3:
            return grid
        st = self._gmtime().unixTime()
        half = int(factor)/ 2
        sg = zeros(grid.shape,'f8')
        count = zeros(grid.shape,'f8')
        gridOfOnes = ones(grid.shape,'f8')
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

    # 
    def getStormCenter(self, windGrid, tr):
        u, v = self.MagDirToUV(windGrid[0], windGrid[1])
        vortGrid = MetLib.vorticity((u, v))
        vortGrid = self.smoothGrid(vortGrid, 3)
        vortGrid = clip(vortGrid, 0.0, 1000000)
##        self.createGrid("Fcst", "Vorticity", "SCALAR", vortGrid, tr,
##                         minAllowedValue = 0.0, maxAllowedValue=1000000.0)

        # find the max
        maxVort = max(vortGrid.flat)

        if maxVort < 10:
            return None, None

        # find the location of the max
        mask = equal(vortGrid, maxVort)

        maxPos = nonzero(mask.flat)[0]

        # get x. y position of the center
        row = maxPos / mask.shape[1]
        col = maxPos % mask.shape[1]

        return col, row

    # Using the center, a distance grid from that center and a mask
    # indictaing the area that includes the maxWind field this
    # method returns the radius of that area in gridpoint units.
    # Note this method adds 2 gridpoints since the area tends to
    # fall a little short of the actual max wind value.
    def calcRadius(self, center, distGrid, mask):
        maskPoints = nonzero(mask)
        if len(maskPoints) == 0:
            return 0

        histDict = {}
        yCoords, xCoords = maskPoints
        
        for i in range(len(yCoords)):
            dist = int(distGrid[yCoords[i], xCoords[i]])
        
            if not histDict.has_key(dist):
                histDict[dist] = 1
            else:
                histDict[dist] = histDict[dist] + 1

        histKeys = histDict.keys()
        histKeys.sort()
        lastKey = 0
        for key in histKeys:
            if key - lastKey >=2:
                return lastKey + 2
            lastKey = key
        
        return lastKey + 2
        
    # makes a grid of distance from the specified center
    # which must be expressed as a tuple (x, y)
    def makeDistGrid(self, center):
        iGrid = indices(self.getGridShape())
        yDist = center[1] - iGrid[0]
        xDist = center[0] - iGrid[1] 
        distGrid = sqrt(pow(xDist, 2)+ pow(yDist, 2))
        
        return distGrid

    # Given a specified wind speed and direction grid along with
    # a distance grid, this method computes a mask that covers
    # the area defined by the ring of max winds.
    def makeCoreMask(self, ws, wd, distGrid):

        # Find the area inside the max wind ring
        u, v = self.MagDirToUV(ws, wd)
        windGrad = MetLib.gradient(ws)
        distGrad = MetLib.gradient(distGrid)
        dotGrid = MetLib.dot(windGrad, distGrad)

        # smooth this grid to remove noise
        dotGrid = self.smoothGrid(dotGrid, 9)
        mask = greater(dotGrid, 0.0)
        
        return mask

    # This method returns the max wind speed value found in the
    # specified wind speed grid.  This method restricts the search
    # to an area just inside and just outside the specified radius.
    def getMaxWindValue(self, distGrid, radius, ws, tr):
        maxWS = 0.0
        lessMask = less(distGrid, radius + 2)
        moreMask = greater(distGrid, radius - 7)
        mask = lessMask & moreMask
##        self.createGrid("Fcst", "MaxMask", "SCALAR", mask, tr)
        ringGrid = ws * mask
        thisMax = max(ringGrid.flat)
        if thisMax > maxWS:
            maxWS = thisMax

        return maxWS
        
    # This method adjusts and returns the specified maxWindGrid by
    # temporally interpolating each wind grid found in the current
    # inventory.  A vorticity field is used to determine the storm
    # center.  Then the wind speed and direction grids are used to
    # calculate the area surrounded by the ring of max winds. This
    # area is used to calculate the radius of max wind and the all
    # of the attributes are used to calculate the max wind field
    # over the entire event by interpolating temporally.  This max
    # wind field is later used to calculate the WindThreat.
    def adjustWindMax(self, maxWindGrid):
        trList = self.getWEInventory("Fcst", "Wind")
        if len(trList) == 0:
            return

        adjustedWindMax = maxWindGrid
        stormMaxWindValue = -1

        modifiedMask = self.empty(bool)

        lastXPos = None
        lastYPos = None
        lastRadius = None
        lastWindMax = None
        for tr in trList:

            ws, wd = self.getGrids("Fcst", "Wind", "SFC", tr, mode="First")

            xPos, yPos  = self.getStormCenter((ws, wd), tr)

            # See if the storm os outside the GFE domain.  If it is, just update
            # the adjustedWindMax and the stormMaxWindValue
            if xPos is None or yPos is None:
                adjustedWindMax = maximum(ws, adjustedWindMax)
                gridWindMax = max(ws.flat)  # max value over the whole grid
                stormMaxWindValue = max(gridWindMax, stormMaxWindValue)  # update the overall max
                continue

            # On 09/17/2014 took off these lines and assked a few above to fix problem wint returned Max Wind when storm
            # center remained outside grid.
            # ignore positions outside the domain
            #if xPos < 0 or yPos < 0 or \
            #   xPos >= ws.shape[1] or yPos >= ws.shape[0]:
            #    continue
            
            # calc change in wind speed as a function of radius
            distGrid = self.makeDistGrid((xPos, yPos))

            coreMask = self.makeCoreMask(ws, wd, distGrid)

            # first time through just store the position
            if lastXPos == None or lastYPos == None:
                lastXPos = xPos
                lastYPos = yPos
                lastRadius = self.calcRadius((xPos, yPos), distGrid, coreMask)
                lastWindMax = self.getMaxWindValue(distGrid, lastRadius, ws, tr)
                continue
            
            # get the maxWindRadius in grid cell coordinates
            radius = self.calcRadius((xPos, yPos), distGrid, coreMask)
            if radius is None:
                continue

            # Get the max wind value, but restrict it to near the radius
            maxWindValue = self.getMaxWindValue(distGrid, radius, ws, tr)
            #print "unrestricted max Wind Value: ", maxWindValue

            dr = radius - lastRadius
            
            # calc distance from last point
            dx = xPos - lastXPos
            dy = yPos - lastYPos
            dWind = maxWindValue - lastWindMax
            # one iteration per grid point
            steps = abs(maximum(dx, dy))
            
            for i in range(steps):
                x = lastXPos + (float(dx) / steps) * i
                y = lastYPos + (float(dy) / steps) * i
                r = lastRadius + (float(dr) / steps) * i

                windValue = lastWindMax + (float(dWind) / steps) * i
                distGrid = self.makeDistGrid((x, y))

                # change grids at the intersection of a circle defined by
                # the center and radius and points that whose value is
                # less than the current maxWind value
                distMask = less_equal(distGrid, r)
                lessMask = less(adjustedWindMax, windValue)
                mask = distMask & lessMask # union of dist and less masks
                    
                adjustedWindMax[mask] = windValue

                modifiedMask = mask | modifiedMask
                
            lastXPos = xPos
            lastYPos = yPos
            lastRadius = radius
            lastWindMax = maxWindValue


        # calculate the maximum wind value over the modified area
        stormMaxWindValue = max((adjustedWindMax * modifiedMask).flat)

        #print "stormMaxWindValue: ", stormMaxWindValue
        # smooth out moderate values to remove "quadrant effect"
        adjustedWindMax = self.smoothGrid(adjustedWindMax, 5) 
            
        return adjustedWindMax, stormMaxWindValue

    # fetch a grid that represents the maxWind everywhere for the next 5 days
    def getWindMax(self, timeRange):

        cTime = int(self._gmtime().unixTime()/ 3600) * 3600
        startTime = AbsTime.AbsTime(cTime)
        endTime = startTime + (5 * 24 * 3600)  # 5 days from the startTime
        tr = TimeRange.TimeRange(startTime, endTime)
        
        try:
            windMax, dir = self.getGrids("Fcst", "Wind", "SFC", tr, mode = "Max")
        except:
            windMax = None
            self.statusBarMsg("No Wind grids found.  Please define Wind grids first.",
                              "S")
        return windMax

    def getMaxWindGrid(self):
        try:
            grid = self.getObject("TCMMaxWindGrid", "WindGrid")
            maxWindValue = max(grid.flat)
            return (grid, maxWindValue)
        except:
            self.statusBarMsg("No TCMMaxWindGrid found.",
                              "S")
            return (None, None)
        

    def execute(self, varDict):

        # Get confidence value from the dialog
               
        confidenceStr = varDict["Forecast Confidence?"]
        tcmwindmax = varDict["WindMax Source?"]

        # define a couple of boolean flags that will come in handy later
        # allProb = confidenceStr == "Low (Probability-only; 10% Default-05% NE Coast)"
        # allWind = confidenceStr == "Highest (Deterministic-only; MaxWind Composite)"

        #allProb = confidenceStr == "Low (Probability-only; for ill-behaved systems)"
        allWind = confidenceStr == "Highest (Deterministic-only; assumes a perfect forecast)"
        typical = confidenceStr == "Typical (Combined; For ill defined or for most systems anytime within 48 hours of landfall or closest approach)"
        high    = confidenceStr == "High (Combined; for well-behaved systems within 12 hours of landfall or closest approach)"
        higher  = confidenceStr == "Higher (Combined; for very well-behaved systems within 6 hours of landfall or closest approach)"

        #print "allProb and allWind are: ", allProb, allWind
        
        # extract the percent value from this string
        # pctPos = confidenceStr.find("% Default")
        # pctStr = confidenceStr[pctPos - 2:pctPos]
        pctStr="10"
        if high:
            pctStr="20"
        elif higher:
            pctStr="30"
                
        # Percent thresholds for each confidence category
        threatDict = {"10" : [10.0, 10.0, 10.0, 20.0, 30.0],
                      "20" : [20.0, 20.0, 20.0, 30.0, 40.0],
                      "30" : [30.0, 30.0, 30.0, 40.0, 50.0],
                     }
        # wind thresholds for each threat category
        windDict = {'None' : 0.0,
                   # 'Very Low' : 34.0,
                    #'Elevated' : 50.0,
                    'Elevated': 34.0,
                    'Mod' : 50.0,
                    'High1' : 64.0,
                    'High2' : 89.0,
                    'Extreme' : 96.0,
                   }    

        # Make sure the string is valid. If not then assign any value since the user
        # has indicted Highest confidence in the wind field and is not using the
        # probabilistic grids at all.
        
        #if not threatDict.has_key(pctStr):
        #    pctStr = "10"

        #print "pctStr is: ", pctStr

        # Extract the proper list and assign thresholds
        thresholdList = threatDict[pctStr]
#
        t34TS1 = thresholdList[0]
        t50TS2 = thresholdList[1]
        t64Cat1 = thresholdList[2]
        t64Cat2 = thresholdList[3]
        t64Cat3 = thresholdList[4]

        timeRange = self.make6hrTimeRange()
        
        # Remove previous version of grids.
        self.removeTempGrids()
        
        # set up the indices for the discrete keys
        keys = self.getDiscreteKeys("WindThreat")
##        print "WindThreat keys are:", keys
        noneIndex = self.getIndex("None", keys)
        
        lowIndex = self.getIndex("Elevated", keys)
        modIndex = self.getIndex("Mod", keys)
        highIndex = self.getIndex("High", keys)
        extremeIndex = self.getIndex("Extreme", keys)

        # initialize the threat grid
        threatGrid = self.empty(int8)  # a grid of zeros


        # Attempt to get the grid from the server.
        windMax, maxWindValue = self.getMaxWindGrid()

        #print "MAXWIND ACROSS DOMAIN IS: ", maxWindValue
        
        if windMax is None or tcmwindmax == "WFO Mesoscale (default)":   # use the old-fashioned method
            # Get and adjust a grid of maximum wind over the entire storm
            windMax = self.getWindMax(timeRange)

            windMax, maxWindValue = self.adjustWindMax(windMax)

        if windMax is not None:
            self.createGrid("Fcst", "WindMax", "SCALAR", windMax, timeRange,
                            minAllowedValue=0, maxAllowedValue=200)

        # assign values to the grid based on the probability grids

        if allWind:

            threatGrid[windMax < windDict["Elevated"]] = 0
            threatGrid[windMax >= windDict["Elevated"]] = lowIndex
            threatGrid[windMax >= windDict["Mod"]] = modIndex
            threatGrid[windMax >= windDict["High1"]] = highIndex
            threatGrid[windMax >= windDict["Extreme"]] = extremeIndex

        else:

            # high and extreme threats require maxWind to meet particular windMax criteria
            # Fetch the probabilistic grids
            prob34Grid, prob50Grid, prob64Grid = self.getProbGrids()

            #self.createGrid("Fcst", "Prob34", "SCALAR", prob34Grid, timeRange)
            #self.createGrid("Fcst", "Prob50", "SCALAR", prob50Grid, timeRange)
            #self.createGrid("Fcst", "Prob64", "SCALAR", prob64Grid, timeRange)
            #print "MAXWIND IS: ", maxWindValue
            threatGrid = self.empty(int8)
            threatGrid[prob34Grid < t34TS1] = 0
            threatGrid[prob34Grid >= t34TS1] = lowIndex
            threatGrid[prob50Grid >= t50TS2] = modIndex
            threatGrid[prob64Grid >= t64Cat1] = highIndex
            if maxWindValue >= windDict['High2']:
                threatGrid[prob64Grid >= t64Cat3] = extremeIndex
            if maxWindValue >= windDict['Extreme']:
                threatGrid[prob64Grid >= t64Cat2] = extremeIndex

            # Upgrade windThreat based on windMax grid
##                  
            # upgrade None to Elevated 
            windMask = (windMax >= windDict['Elevated']) & (windMax < windDict['Mod'])
            threatMask = threatGrid < lowIndex
            threatGrid[windMask & threatMask] = lowIndex

            # upgrade Elevated to Med
            windMask = (windMax >= windDict['Mod']) & (windMax < windDict['High1'])
            threatMask = threatGrid < modIndex
            threatGrid[windMask & threatMask] = modIndex

            # upgrade Med to High
            windMask = (windMax >= windDict['High1']) & (windMax < windDict['Extreme'])
            threatMask = threatGrid < highIndex
            threatGrid[windMask & threatMask] = highIndex

            # upgrade High to Extreme
            windMask = windMax >= windDict['Extreme']
            threatMask = threatGrid < extremeIndex
            threatGrid[windMask & threatMask] = extremeIndex

        # Remove previous version of grid.
        dbTimes = self.makeDBTimeRange()
        self.deleteCmd(['WindThreat'], dbTimes)

        # create the threat grid
        self.createGrid("Fcst", "WindThreat", "DISCRETE", (threatGrid, keys),
                        timeRange, discreteKeys=keys, discreteAuxDataLength=5,
                        discreteOverlap=0)
