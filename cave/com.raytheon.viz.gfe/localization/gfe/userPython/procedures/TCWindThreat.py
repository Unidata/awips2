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
# Modified: by Santos 04/26/2011 to accommodate alternate logic for NE Coast and hide bars
# Modified: by Santos 04/20/2012 to get rid off Very Low and tweak GUI.
# Modified: by Shannon/Pablo 06/19/2012 to make A2 and DRT compatible
# Modified: by Pablo/Shannon on 05/21/2014 to fix bug introduced in 2012 when Very Low was eliminated
# Modified: By Santos/Lefebvre 09/17/2014 to make changes for official implementation in 2015.
# Modified: By Belk 07/15/2016 to make efficiency improvements, and
#   refactor to make use of a utility containing common methods with other tools
# CHECKED IN FOR 17.1.1: By LeFebvre 09/23/2016 finish conversion to numpy conventions.
# Modified: By LeFebvre 09/26/16 - Removed commented out code to pass code review.
# Modified: By LeFebvre 10/31/16 - Added more code to ensure only one cyclone center point is calculated
# Modified: By LeFebvre 07/18/17 - Added option to populate based on Preliminary or Official prob guidance.
# Modified: By P. Santos 9/25/18 - Cleaned GUI options based on SPT approved wording.
# Modified: By P. Santos 1/31/2019 - Made adjustments requested by Raytheon.
# Modified: By P. Santos 10/15/2020 - Made adjustments found with wind swath by BRO During Laura. Gotr rid of AdjustWindMax method. 
# Looks at CUM WPS out to 72 hour instead of 60.
# Modified: By P. Santos 04/22/2021 - Fixed issue found during 20.2.3 beta testing.
#----------------------------------------------------------------------------
##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

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

VariableList = [("Probabilistic Wind Source?", "Preliminary", "radio", ["Official", "Preliminary"]),
                ("Confidence Level in the Deterministic Wind Forecast Used for the Wind Threat Composition:", "Typical Confidence (10% exceedance; reasonable worse case), Widest Safety Margin",
                 "radio", ["Typical Confidence (10% exceedance; reasonable worse case), Widest Safety Margin",
                           "Medium Confidence (20% exceedance; some confidence in the deterministic forecast), Wide Safety Margin - MUST COORDINATE USE",
                           "High Confidence (30% exceedance; more confidence in the deterministic forecast), Narrow Safety Margin - MUST COORDINATE USE",
                           "Highest Confidence (deterministic wind only; smoothing will be needed), No Safety Margin - MUST COORDINATE USE"]),
                ]

import TropicalUtility
import TimeRange
import AbsTime
import MetLib
import math
import numpy as np

class Procedure (TropicalUtility.TropicalUtility):
    
    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)


    # Finds the TPC prob database, extracts all of the grids and returns
    # the last one of each type
    def getProbGrids(self):
        ap = self.availableParms()
        searchStr = self.getSiteID() + "_GRID_D2D_" + self._probWindModelSource + "_"
        probModel = ""
        for elem, level, model in ap:
            if  searchStr in model.modelIdentifier():
                probModel = model.modelIdentifier()
                break
                
        if probModel == "":
            self.statusBarMsg("TPC Wind probability grids not found. Proceeding if using Deterministic Option Only.", "S")
            return None, None, None, None

        # make a big timeRange
        timeRange = TimeRange.allTimes()
        trList = self.GM_getWEInventory("prob34", probModel, "FHAG10")
        timeRangeStartTime = AbsTime.AbsTime(trList[0].startTime().unixTime())
        timeRangeEndTime = AbsTime.AbsTime(trList[12].startTime().unixTime())
        tr = TimeRange.TimeRange(timeRangeStartTime, timeRangeEndTime)

        # get the TPC prob grids, and keeps the 72th hour forecast one in the series. But first the inventory is complete. 
        
        prob34List = self.getGrids(probModel, "prob34", "FHAG10", timeRange, mode = "List")
        prob50List = self.getGrids(probModel, "prob50", "FHAG10", timeRange, mode = "List")
        prob64List = self.getGrids(probModel, "prob64", "FHAG10", timeRange, mode = "List")        
              
        if len(prob34List) == 21 and len(prob50List) == 21 and len(prob64List) == 21:
            prob34Grid = prob34List[12]
            prob50Grid = prob50List[12]
            prob64Grid = prob64List[12]
            
        else:
     
           self.statusBarMsg("Probabilistic Elements Missing in " + probModel + " Database. Proceeding only if using Deterministic Option. Otherwise, Check and Try Again.", "S")
           return None, None, None, None
             
        return prob34Grid, prob50Grid, prob64Grid, tr

    #  Make a timeRange spanning from the current hour to 6 hours hence
    def make6hrTimeRange(self):
        cTime = int(self._gmtime().unixTime()/ 3600) * 3600
        startTime = AbsTime.AbsTime(cTime)
        endTime = AbsTime.AbsTime(cTime + (6 * 3600))   # 6 hours
        tr = TimeRange.TimeRange(startTime, endTime)

        return tr


    #  Make a timeRange spanning the usual -24 to 10-day db
    def makeDBTimeRange(self):
        cTime = int(self._gmtime().unixTime()/ 3600) * 3600
        startTime = AbsTime.AbsTime(cTime - (24*3600))
        endTime = AbsTime.AbsTime(cTime + (240 * 3600))   # 10 days
        tr = TimeRange.TimeRange(startTime, endTime)

        return tr
    

    #  Removes all grids previous grids
    def removeGrids(self, elementList):
        #elementList = ["Prob34", "Prob50", "Prob64", "WindMax", "WindThreat"]
        ap = self.availableParms()
        #tr = TimeRange.allTimes()
        tr = self.makeDBTimeRange()
        
        for elem, level, model in ap:
            if elem in elementList and level == "SFC":
                self.deleteCmd([elem], tr)
        return

    # fetch a grid that represents the maxWind everywhere for the next 5 days
    def getWindMax(self, timeRange):

        cTime = int(self._gmtime().unixTime()/ 3600) * 3600
        startTime = AbsTime.AbsTime(cTime)
        endTime = startTime + (3 * 24 * 3600)  # 3 days from the startTime
        tr = TimeRange.TimeRange(startTime, endTime)

        if timeRange is not None:
            if timeRange.overlaps(tr):
                endTime = AbsTime.AbsTime(timeRange.endTime().unixTime())
                tr = TimeRange.TimeRange(startTime, endTime)
            else:
                self.statusBarMsg("Winds grids and WSP model do not overlap.  Check your database. Stopping", "S")
                return None,None,None 
                              
        try:
            windMax, dir = self.getGrids(self.mutableID(), "Wind", "SFC", tr, mode = "Max")
        except:
            #windMax = None
            self.statusBarMsg("No Wind grids found.  Please define Wind grids first.", "S")
            return None,None,None

        maxWindValue = np.amax(windMax)    #  Max value over the whole grid
        
        return windMax, maxWindValue, tr

    def execute(self, varDict):

        # Fetch the model source and define the model name
        sourceDB = varDict["Probabilistic Wind Source?"]
        if sourceDB == "Official":
            self._probWindModelSource = "TPCProb"
        elif sourceDB == "Preliminary":
            self._probWindModelSource = "TPCProbPrelim"
        else:
            self.statusBarMsg("Unknown model source selected. Aborting.", "U")
            return

        # Get confidence value from the dialog
        confidenceStr = varDict["Confidence Level in the Deterministic Wind Forecast Used for the Wind Threat Composition:"]

        allWind = confidenceStr == "Highest Confidence (deterministic wind only; smoothing will be needed), No Safety Margin - MUST COORDINATE USE"
        high    = confidenceStr == "Medium Confidence (20% exceedance; some confidence in the deterministic forecast), Wide Safety Margin - MUST COORDINATE USE"
        higher  = confidenceStr == "High Confidence (30% exceedance; more confidence in the deterministic forecast), Narrow Safety Margin - MUST COORDINATE USE"

        #print("allProb and allWind are: ", allProb, allWind)
        
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
                    'High2' : 83.0,
                    'Extreme' : 96.0,
                   }    

        # Make sure the string is valid. If not then assign any value since the user
        # has indicted Highest confidence in the wind field and is not using the
        # probabilistic grids at all.
        
        #if not threatDict.has_key(pctStr):
        #    pctStr = "10"

        #print("pctStr is: ", pctStr)

        # Extract the proper list and assign thresholds
        thresholdList = threatDict[pctStr]
#
        t34TS1 = thresholdList[0]
        t50TS2 = thresholdList[1]
        t64Cat1 = thresholdList[2]
        t64Cat2 = thresholdList[3]
        t64Cat3 = thresholdList[4]

        timeRange6 = self.make6hrTimeRange()
              
        # set up the indices for the discrete keys
        keys = self.getDiscreteKeys("WindThreat")
        
        lowIndex = self.getIndex("Elevated", keys)
        modIndex = self.getIndex("Mod", keys)
        highIndex = self.getIndex("High", keys)
        extremeIndex = self.getIndex("Extreme", keys)
        
        #  Initialize the threat grid
        threatGrid = self.empty(np.int8)  # a grid of zeros
        
        prob34Grid, prob50Grid, prob64Grid, timeRange = self.getProbGrids()
        #print("Time Range for WSP is: ", timeRange)
        self.removeGrids(["Prob34","Prob50","Prob64"])            
           
        #  Use the old-fashioned method                   
        #  Get and adjust a grid of maximum wind over the entire storm
        windMax, maxWindValue, tr = self.getWindMax(timeRange)
        #print("Time Range from WindMax is: ", tr)
        if windMax is None and maxWindValue is None:
            return
        timeRange = tr

        self.removeGrids(["WindMax"])
        self.createGrid(self.mutableID(), "WindMax", "SCALAR", windMax, timeRange,
                            minAllowedValue=0, maxAllowedValue=200, defaultColorTable="GFE/TCMWinds")

        #  Assign values to the grid based on the probability grids
        if allWind:
            
            threatGrid[windMax >= windDict["Elevated"]] = lowIndex
            threatGrid[windMax >= windDict["Mod"]] = modIndex
            threatGrid[windMax >= windDict["High1"]] = highIndex
            threatGrid[windMax >= windDict["Extreme"]] = extremeIndex

        else:

            # high and extreme threats require maxWind to meet particular windMax criteria
            # Fetch the probabilistic grids
          
            #Prevent code from proceeding and erroring out if db or elements missing.
            if prob34Grid is None or prob50Grid is None or prob64Grid is None:
                return

# Show actual grids being used in algorithm               
            self.createGrid(self.mutableID(), "Prob34", "SCALAR", prob34Grid, timeRange, minAllowedValue=0, maxAllowedValue=100, defaultColorTable="GFE/TPCprob")
            self.createGrid(self.mutableID(), "Prob50", "SCALAR", prob50Grid, timeRange, minAllowedValue=0, maxAllowedValue=100, defaultColorTable="GFE/TPCprob")
            self.createGrid(self.mutableID(), "Prob64", "SCALAR", prob64Grid, timeRange, minAllowedValue=0, maxAllowedValue=100, defaultColorTable="GFE/TPCprob")

            threatGrid[prob34Grid >= t34TS1] = lowIndex
            threatGrid[prob50Grid >= t50TS2] = modIndex
            threatGrid[prob64Grid >= t64Cat1] = highIndex
            
            if maxWindValue >= windDict['High2']:
                threatGrid[prob64Grid >= t64Cat3] = extremeIndex
            if maxWindValue >= windDict['Extreme']:
                threatGrid[prob64Grid >= t64Cat2] = extremeIndex
 
            #===================================================================
            #  Upgrade windThreat based on windMax grid
##                  
            #  Upgrade None to Elevated
            windMask = ((windMax >= windDict['Elevated']) & 
                        (windMax < windDict['Mod']))
            threatMask = threatGrid < lowIndex
            threatGrid[windMask & threatMask] = lowIndex

            #  Upgrade Elevated to Med
            windMask = ((windMax >= windDict['Mod']) & 
                        (windMax < windDict['High1']))
            threatMask = threatGrid < modIndex
            threatGrid[windMask & threatMask] = modIndex

            #  Upgrade Med to High
            windMask = ((windMax >= windDict['High1']) & 
                        (windMax < windDict['Extreme']))
            threatMask = threatGrid < highIndex
            threatGrid[windMask & threatMask] =highIndex

            #  Upgrade High to Extreme
            windMask = windMax >= windDict['Extreme']
            threatMask = threatGrid < extremeIndex
            threatGrid[windMask & threatMask] = extremeIndex

        # Remove previous version of grids.
        self.removeGrids(["WindThreat"])
        
        # create the threat grid
        self.createGrid(self.mutableID(), "WindThreat", "DISCRETE", (threatGrid, keys),
                        timeRange6, discreteKeys=keys, discreteAuxDataLength=5,
                        discreteOverlap=0)
