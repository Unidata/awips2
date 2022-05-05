##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
#-------------------------------------------------------------------------
# Description: This product creates a Multiple Element Table.
#  The possible elements are Temperature (MaxT, MinT), Humidity (MinRH, MaxRH), and PoP
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# MultipleElementTableTable, MultipleElementTable_Local, MultipleElementTable_Aux_Local
#-------------------------------------------------------------------------
# User Configurable Variables:
#-------------------------------------------------------------------------
# Weather Elements Needed:
#-------------------------------------------------------------------------
# Edit Areas Needed:
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file:
#-------------------------------------------------------------------------
# Component Products:
#-------------------------------------------------------------------------
# Programmers and Support including product team leader's email:
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
#-------------------------------------------------------------------------
# Additional Information:
#-------------------------------------------------------------------------

##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the
# GFE Training Guide->GFE Text Products User Guide section of the GFE Online
# Help for guidance on creating a new text product.
##

import TextRules
import SampleAnalysis


class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = [
             (("Forecast Product", "productIssuance") , "Morning", "radio",
              ["Morning","Afternoon"]),
            ]

    Definition =  {
        "type": "smart",
        "displayName": "None",
        "outputFile": "/awips/GFESuite/products/TEXT/MultipleElementTable.txt",
        "defaultEditAreas": [
            ("area1","AREA 1"),
            ("area2","AREA 2"),
            ("area3","AREA 3"),
            ],
        # Product-specific variables
        "regionList" : [
            ("/33",["AREA 1","AREA 2"]),
            ("/19",["AREA 3"])
            ],
        # Possible elements are:
        #   "Temp"  -- lists MaxT for daytime, MinT for nighttime
        #   "PoP"
        #   "Humidity"  -- lists MinRH for daytime, MaxRH for nighttime
        "elementList" : ["Temp", "PoP"],
        # If set to 1, only one value for each element is listed
        "singleValueFormat": 0,  # Default is 0
        "includeTitle": 1,
        "introLetters": "&&\n  ",
        }

    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)

    def generateForecast(self, argDict):
        # Generate formatted product for a list of edit areas

        # Get variables from varDict and Definition
        self._getVariables(argDict)

        # Get the areaList -- derived from defaultEditAreas and
        # may be solicited at run-time from the user if desired
        self._areaList = self.getAreaList(argDict)

        # Determine time ranges for which the data will be sampled
        self._determineTimeRanges(argDict)

        # Sample the data
        self._sampleData(argDict)

        # Initialize the output string
        fcst = ""
        fcst = self._preProcessProduct(fcst, argDict)

        # Generate the product for each edit area in the list
        for editArea, areaLabel in self._areaList:
            fcst = self._preProcessArea(fcst, editArea, areaLabel, argDict)
            fcst  = self._makeProduct(fcst, editArea, areaLabel, argDict)
            fcst = self._postProcessArea(fcst, editArea, areaLabel, argDict)

        fcst = self._postProcessProduct(fcst, argDict)
        return fcst

    def _getVariables(self, argDict):
        # Determine whether Morning or Afternoon product type
        varDict = argDict["varDict"]
        for (key, value) in varDict.items():
            if type(key) is tuple:
                label, variable = key
                setattr(self, f"_{variable}", value)

        # Make argDict accessible
        self.__argDict = argDict
        
        # Set up any other product-specific variables from the Definition
        self._definition = argDict["forecastDef"]
        for (key, value) in self._definition.items():
            setattr(self, f"_{key}", value)

        self._currentRegion = None

        # The analysisList tells which weather elements and statistics
        # are desired for the product.
        self._analysisList = self._getAnalysisList()
 
    def _determineTimeRanges(self, argDict):
        # Determine time ranges for product
        # Sets up self._timeRangeList

        try:
            byTimeRange = argDict["byTimeRange"]
        except:
            byTimeRange = 0

        if byTimeRange:
            timeRange = argDict["timeRange"]
            day = self.getPeriod(timeRange)
            if day == self.DAYTIME():
                self._productIssuance = "Morning"
            else:
                self._productIssuance = "Afternoon"
            # Force singleValueFormat
            self._singleValueFormat = 1
        else:
            timeRange = None
        self._timeRangeList = self.getMultipleElementTableRanges(
            self._productIssuance, self._singleValueFormat, timeRange)
        return

    def _sampleData(self, argDict):
        # Sample the data
        self._sampler = self.getSampler(argDict, 
          (self._analysisList, self._timeRangeList, self._areaList))
        return

    def _preProcessProduct(self, fcst, argDict):
        # Set up format spacing and title line spacing
        
        numElements = len(self._elementList)
        if numElements > 2:
            self._spaceStr = ""
        else:
            self._spaceStr = "   "
        if self._includeTitle == 0:
            return fcst
            
        if self._singleValueFormat == 1:
            self._titles = self._titleDict()["SingleValue"]
            self._headingLen = 5
        else:
            self._titles = self._titleDict()["MultipleValue"]
            if numElements > 2:
                if self._productIssuance == "Morning":
                    self._headingLen = 15
                else:
                    self._headingLen = 19
            else:
                if self._productIssuance == "Morning":
                    self._headingLen = 21
                else:
                    self._headingLen = 28
                    
        # Create title line
        title = self._introLetters + "        "
        if self._singleValueFormat == 1:
            title = title + " "
        index = 0
        for element in self._elementList:
            title += self._titles[element].center(self._headingLen)
            if index < len(self._elementList)-1:
                title = title + "/"
            index += 1
        return fcst + title + "\n"

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        # If we are in a new region, add region header
        for region, areaList in self._regionList:
            if areaLabel in areaList:
                break
        if region != self._currentRegion:
            if self._currentRegion is not None:
                # End the Region
                fcst = fcst + "\n$$\n\n"
            self._currentRegion = region
            fcst = fcst + region

        return fcst + "\n" + areaLabel.ljust(10)

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        # Get the Statistics
        statList = self.getStatList(self._sampler, self._analysisList,
                                     self._timeRangeList, editArea)

        numElements = len(self._elementList)
        index = 0
        for element in self._elementList:
            elementMethod = getattr(self, f"_get{element}Values")
            fcst += elementMethod(statList, argDict)
            if index < numElements-1 and self._singleValueFormat == 0:
                fcst = fcst + "  /"
            index +=1
        return fcst

    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        return fcst

    def _postProcessProduct(self, fcst, argDict):
        fcst = fcst + "\n"
        return fcst

    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################

    def _getAnalysisList(self):
      return [
          ("MinT", self.avg),
          ("MaxT", self.avg),
          ("MinRH", self.avg),
          ("MaxRH", self.avg),
          ("PoP", self.stdDevMaxAvg),
          ]

    def _titleDict(self):
        return {
            "MultipleValue": {
                "Temp":    "TEMPERATURE",
                "PoP":     "PRECIPITATION",
                "Humidity":"HUMIDITY",
                },
            "SingleValue":
                {
                "Temp":    "TEMP",
                "PoP":     "POP",
                "Humidity":"HUM",
                }
            }

    def _getTempValues(self, statList, argDict):
       # Return a string of Temperature values given statList
       stats1 = statList[0]
       if self._productIssuance == "Morning":
           if self._singleValueFormat == 0:
               stats2 = statList[1]
               stats3 = statList[2]
               t1 = self.getScalarVal(stats1["MaxT"])
               t2 = self.getScalarVal(stats2["MinT"])
               t3 = self.getScalarVal(stats3["MaxT"])
               str =  " " + t1+ self._spaceStr +t2+ self._spaceStr +t3
               return str
           else:
               return self.getScalarVal(stats1["MaxT"]) + "  "
       else:
           if self._singleValueFormat == 0:
               stats2 = statList[1]
               stats3 = statList[2]
               stats4 = statList[3]
               t1 = self.getScalarVal(stats1["MinT"])
               t2 = self.getScalarVal(stats2["MaxT"])
               t3 = self.getScalarVal(stats3["MinT"])
               t4 = self.getScalarVal(stats4["MaxT"])
               str = " " +t1+ self._spaceStr +t2+ self._spaceStr +t3+ self._spaceStr+t4
               return str
           else:
               return self.getScalarVal(stats1["MinT"]) + "  "
 
    def _getHumidityValues(self, statList, argDict):
       # Return a string of Humidity values given statList
       stats1 = statList[0]
       if self._productIssuance == "Morning":
           if self._singleValueFormat == 0:
               stats2 = statList[1]
               stats3 = statList[2]
               t1 = self.getScalarVal(stats1["MinRH"])
               t2 = self.getScalarVal(stats2["MaxRH"])
               t3 = self.getScalarVal(stats3["MinRH"])
               return " " +t1+ self._spaceStr +t2+ self._spaceStr+t3
           else:
               return self.getScalarVal(stats1["MinRH"]) + "  "
       else:
           if self._singleValueFormat == 0:
               stats2 = statList[1]
               stats3 = statList[2]
               stats4 = statList[3]
               t1 = self.getScalarVal(stats1["MaxRH"])
               t2 = self.getScalarVal(stats2["MinRH"])
               t3 = self.getScalarVal(stats3["MaxRH"])
               t4 = self.getScalarVal(stats4["MinRH"])
               return " " +t1+ self._spaceStr +t2+ self._spaceStr +t3+ self._spaceStr +t4
           else:
               return self.getScalarVal(stats1["MaxRH"]) + "  "

    def _getPoPValues(self, statList, argDict):
       # Return a string of PoP values in the statList
       popStr = ""
       if self._singleValueFormat == 0:
           index = 0
           for stats in statList:
               val = self._getPoPValue(stats)
               if index < len(statList)-1:
                   popStr = popStr  + val + self._spaceStr
               else:
                   popStr = popStr + val
               index += 1
           popStr = popStr + " "
       else:
           popStr = self._getPoPValue(statList[0]) + "  "
       return popStr 

    def _getPoPValue(self, stats):
       pop = self.getStats(stats,"PoP")
       if pop is None:
           val = "    "
       else:
           max = self.round(pop, "Nearest", 10)
           val = self.getScalarVal(max)
       return val
