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
# File Name: CCF.py
# Description: This product creates a Coded Cities Forecast table 
# containing 7-days of forecast data for PoP, Weather, Snow, and Max
# and Min Temps.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# CCF, CCF_<site>_<MultiPil>_Definition, CCF_<site>_Override
#-------------------------------------------------------------------------
# User Configurable Variables:
# Definition Section:
#   displayName      If not None, defines how product appears in GFE GUI
##
#   defaultEditAreas defines edit area names and station IDs for edit areas
#                    expected in the form of (editAreaName, 3letterStationID)
#
#   fullStationID    full station identifier (4letter, KSLC)
#
#   wmoID            WMO ID for product header, such as FOUS45
#
#   pil              Product pil, such as CCFBOX
#
#  debug                  If on, debug_print statements will appear.
#  database               Source database for product. Can be "Official", 
#                         "Fcst" or "ISC"
#  outputFile             Defines the output location of the finished product
#                         when saved from the Formatter Launcher.
#  textdbPil              Defines the awips product identifier 
#                         (e.g., DENCCFDEN) that is used to store the product 
#                         in the AWIPS text database. 
#                         This value is also used for the default GUI entry for 
#                         storage.
#  awipsWANPil            Defines the awips product identifier 
#                         (e.g., KBOUCCFDEN) that is used to transmit the 
#                         product to the AWIPS WAN. 
#                         This value is also used for the default GUI 
#                         entry for storage.
#   alwaysIncludeSnow  include snow in product (1=yes,0=no)
#
#   popStartZ_AM     start time for PoP for AM issuance in Zulu, (12 for 12z)
#                    Usually changed only for OCONUS sites.
#   wxStartLT_AM     start time for Wx for AM issuance in LT, (6 for 6am)
#   wxStopLT_AM      stop time for Wx for AM issuance in LT, (18 for 6pm)
#                    
#
#-------------------------------------------------------------------------
# Weather Elements Needed: MinT, MaxT, PoP, Wx,
# Sky, Wind, and SnowAmt (optional).  The Sky, Wx,
# and Max/MinT are used to determine the weather character code. All
# weather elements out to 14 periods (7 days) except for SnowAmt which 
# is needed for the first 3 periods only.
#-------------------------------------------------------------------------
# Edit Areas Needed: Typically point edit areas are required for each
# entry to you wish to generate the product for.
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file: None
#-------------------------------------------------------------------------
# Component Products: None
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
#  None
#
# To look up additional tasks and their status, see the Text Product User Guide
# Section on "Tkgnats: Task Reporting System".
#-------------------------------------------------------------------------
# Additional Information:
# The grids are sampled according to the following rules:
# MaxT/MinT: 14 periods, 12 hours apart, daily, set up to take the 
#   MaxT grid overlapping noon LT and MinT grid overlapping midnight LT
# PoP: 14 periods, 12 hours apart, 12z-00z, and 00z-12z. Periods can
#   be overridden using the popStartZ_AM field for OCONUS sites.
# Snow: same as PoP, but only 3 periods.
# Weather: 7 days, 12 hours, Daylight periods usually 6am-6pm LT, but
#   can be overridden using the wxStartLT_AM and wxStopLT_AM field.
#
# Missing data will be shown with MMM for temperatures and snow amounts, '?'
# for Wx, and '/' for PoPs.
#-------------------------------------------------------------------------
# Example Output:
#
##FOUS45 KSLC 091329
##CCFSLC
##
##SLC UU 071/057 078/062 088 99000 0000/0000/0000
##    UBBBB 061/075 059/087 071/079 058/075 0000000-00/
##PVU BU 068/061 077/064 088 99400 0000/0000/0000
##    UBEUU 066/071 061/084 075/085 065/078 010-150032/
########################################################################


import TextRules
import SampleAnalysis
import string, time, types


class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = [
             (("Product Issuance", "productIssuance"), "Morning", "radio",
              ["Morning","Afternoon"]),
             (("Forecaster Number", "forecasterNumber"), "99", "alphaNumeric"),
             ]
    Definition =  {
          "type": "smart",
          "displayName": "None", # for Product Generation Menu
          # Source database for product. Can be "Official", "Fcst" or "ISC"
          "database": "Official",
          # Defines output location of finished product.
          "outputFile": "{prddir}/TEXT/CCF_<MultiPil>.txt",
          "debug": 0,
        
          "defaultEditAreas": [("area1", "AREA1"),
                               ("area2", "AREA2"),
                              ],

          # product identifiers
          "fullStationID": "<fullStationID>",  # full station identifier (4letter, KSLC)
          "wmoID": "<wmoID>",        # WMO ID
          "pil": "<pil>",          # Product pil
          "textdbPil": "<textdbPil>",       # Product ID for storing to AWIPS text database.
          "awipsWANPil": "<awipsWANPil>",   # Product ID for transmitting to AWIPS WAN.
          "awipsTEXTDBhost": None,  # textdb cmd host, or None for local

          # options
          "alwaysIncludeSnow": 1,   # include snow always (1=yes,0=no)
          "popStartZ_AM": 12,       # start time for PoP for AM issuance in Zulu
          "wxStartLT_AM" : 6,        # start time for Wx for AM issuance in LT
          "wxStopLT_AM" : 18,         # stop time for Wx for AM issuance in LT
          "AMnumPeriods": 13,       # set to 14 if using the C-20 directive
          }
    
    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)

    def generateForecast(self, argDict):
        # Generate formatted product for a list of edit areas

        # Get variables from varDict and Definition
        self._getVariables(argDict)

        # Get the areaList -- derived from defaultEditAreas and
        # may be solicited at run-time from user if desired
        self._areaList = self.getAreaList(argDict)
        if len(self._areaList) == 0:
            return "WARNING -- No Edit Areas Specified to Generate Product."

        # Determine time ranges
        self._determineTimeRanges(argDict)

        # Sample the data
        self._sampleData(argDict)
        
        # Initialize the output string
        fcst = ""
        fcst = self._preProcessProduct(fcst, argDict)

        # Generate the product for each edit area in the list
        fraction = 0
        fractionOne = 1.0/float(len(self._areaList))
        percent = 50.0
        for editArea, areaLabel in self._areaList:
            self.progressMessage(fraction, percent, 
              "Making Product for " + areaLabel)
            fcst = self._preProcessArea(fcst, editArea, areaLabel, argDict)
            fcst  = self._makeProduct(fcst, editArea, areaLabel, argDict)
            fcst = self._postProcessArea(fcst, editArea, areaLabel, argDict)
            fraction = fractionOne

        fcst = self._postProcessProduct(fcst, argDict)
        return fcst

    def _getVariables(self, argDict):
        # Determine Forecaster Number and issuance time
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"
        self._forecasterNumber = self._getForecasterNumber(self._forecasterNumber)

        # Make argDict accessible
        self.__argDict = argDict
        
        # Get Definition variables
        self._definition = argDict["forecastDef"]
        for key in self._definition.keys():
            exec "self._" + key + "= self._definition[key]"

    def _determineTimeRanges(self, argDict):
        # Determine time ranges for product
        # Returns popPeriods, snowPeriods, tempPeriods, codePeriods which
        # are a list of tuples (timeRange, label).

        # Calculate ddhhmm string value
        self._currentTime = argDict['creationTime']  #ZULU
        self._ddhhmmTime = time.strftime("%d%H%M",time.gmtime(
            self._currentTime))

        # PoP Time ranges :
        #   13 or 14 12-hour periods
        #   If AM, begin at 12z of issue day (default), may be overridden
        #     by the popStartZ_AM flag.
        #   If PM, begin at 00z of next day (default), may be overridden
        #     by the popStartZ_AM flag.
        if self._productIssuance == "Morning":
            startT = self._popStartZ_AM
        else:
            startT = self._popStartZ_AM + 12      # account for PM start later
            
        # rollover  - different days from gmtime and local time
        # so we need to sample the PoP from "yesterday"
        # for MDT, rollover occurs from 5pm-midnight LST
        if time.gmtime(self._currentTime)[2] != \
             time.localtime(self._currentTime)[2]: 
            startT = startT - 24
            
        popStartTR = self.createTimeRange(startT, startT + 1, mode="Zulu")
        timePeriod = 12
        timeSpan = 12
        if self._productIssuance == "Morning":
           numPeriods = self._AMnumPeriods
        else:
           numPeriods = 14
        self._popPeriods = self.getPeriods(popStartTR, timePeriod,
                                     timeSpan, numPeriods)

        # Snow Time Ranges, same as PoP, but not as many
        self._snowPeriods = self._popPeriods[0:3]

        # Temp Time ranges : 13 or 14 periods, 12 hours apart, 5 hour span
        #   This is to catch the correct Max/Min temp grid
        #   If AM, begin with noon LT of issue day to catch MaxT
        #   If PM, begin with midnight LT of issue day to get MinT
        if self._productIssuance == "Morning":
            tempStartTR = self.createTimeRange(10, 15)
        else:
            tempStartTR = self.createTimeRange(22, 27)
        timePeriod = 12
        timeSpan = 5
        if self._productIssuance == "Morning":
           numPeriods = self._AMnumPeriods
        else:
           numPeriods = 14
        self._tempPeriods = self.getPeriods(tempStartTR, timePeriod, timeSpan,
                                  numPeriods)

        # Code Time ranges :
        #   7 non-consecutive DAYLIGHT 12 hour periods
        #   If AM, begin at "wxStartLT_AM" of issue day
        #   If PM, begin at "wxStartLT_AM" of next day
        if self._productIssuance == "Morning":
            codeStartTR = self.createTimeRange(self._wxStartLT_AM, 
              self._wxStartLT_AM + 1)
        else:
            codeStartTR = self.createTimeRange(self._wxStartLT_AM + 24, 
              self._wxStartLT_AM + 25)
        timePeriod = 24
        timeSpan = self._wxStopLT_AM - self._wxStartLT_AM
        numPeriods = 7
        self._codePeriods = self.getPeriods(codeStartTR, timePeriod, timeSpan,
                                      numPeriods)

        return 

    def _sampleData(self, argDict):
        # Sample the data. Returns the samplers for pop, snow, temp, and code
        sampleList = []
        sampleList.append((self._analysisListPoP(), self._popPeriods))
        sampleList.append((self._analysisListSnow(), self._snowPeriods))
        sampleList.append((self._analysisListTemp(), self._tempPeriods))
        sampleList.append((self._analysisListCode(), self._codePeriods))
        sampleInfo = []
        for analList, periods in sampleList:
            sampleInfo.append((analList, periods, self._areaList))
      
        self._sampler = self.getSampler(argDict, sampleInfo)
        return
    
    def _preProcessProduct(self, fcst, argDict):
        # Add product heading to fcst string
        return fcst + self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n"

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        return fcst + areaLabel + " "

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        # creates the product for each edit area

        ########################
        # first line of product
        ########################

        # wx codes
        for index in xrange(0, 2):
            timeRange, label = self._codePeriods[index]
            fcst = fcst + self._getCCFCode(self._analysisListCode(),
                editArea, timeRange)
        fcst = fcst + " "

        # max/min temp codes
        separators = ["/", " ", "/", " ", " "]
        for index in xrange(0, 5):
            timeRange, label = self._tempPeriods[index]
            fcst = fcst + self._getMinOrMax(self._analysisListTemp(),
                editArea, timeRange) + separators[index]

        # forecaster number
        fcst = fcst + self._forecasterNumber

        # Pop fields
        for index in xrange(0, 3):
            timeRange, label = self._popPeriods[index]
            fcst = fcst + self._getPoP(self._analysisListPoP(), editArea,
                              timeRange)

        # Snow fields
        if self._alwaysIncludeSnow:
            fcst = fcst + self._addSnowEntries(self._analysisListSnow(),
                                               self._snowPeriods, editArea)
        fcst = fcst + "\n"

        ########################
        # second line of product
        ########################

        fcst = fcst + "    "    # ident 4 spaces on the 2nd line

        # wx codes
        startIndex = 2
        for index in xrange(startIndex, len(self._codePeriods)):
            timeRange, label = self._codePeriods[index]
            fcst = fcst + self._getCCFCode(self._analysisListCode(),
                editArea, timeRange)
        fcst = fcst + " "

        # max/min temp codes
        startIndex = 5
        separators = ["/", " ", "/", " ", "/", " ", "/", " ", " "]
        for index in xrange(startIndex, len(self._tempPeriods)):
            timeRange, label = self._tempPeriods[index]
            fcst = fcst + self._getMinOrMax(self._analysisListTemp(),
                editArea, timeRange) + separators[index-startIndex]

        # Pop fields
        startIndex = 3
        for index in xrange(startIndex, len(self._popPeriods)):
            timeRange, label = self._popPeriods[index]
            fcst = fcst + self._getPoP(self._analysisListPoP(), editArea,
                              timeRange)
        fcst = fcst + "\n"

        return fcst

    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        return fcst

    def _postProcessProduct(self, fcst, argDict):
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        #fcst = fcst + "\n$$\n\n"
        return fcst

    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################

    def _analysisListPoP(self):
      return [
          ("PoP", self.stdDevMaxAvg),
          ]

    def _analysisListSnow(self):
      return [
          ("SnowAmt", self.minMaxSum),
          ]

    def _analysisListTemp(self):
      return [
          ("MinT", self.avg),
          ("MaxT", self.avg),
          ]

    def _analysisListCode(self):
      return [
          ("MinT", self.avg),
          ("MaxT", self.avg),
          ("PoP", self.stdDevMaxAvg),
          ("Wx", self.dominantWx),
          ("Sky", self.avg),
          ("Wind", self.vectorAvg)
          ]

    def _getMinOrMax(self, analysisList, area, timeRange):
        # Return a Max or Min value depending on availability
        # Examples:  076 for positive, 915 for negative, MMM for missing
        statDict = self.getStatDict(self._sampler, analysisList,
                                    timeRange, area)
        dayNight = self.getPeriod(timeRange,shiftToLocal=1)
        if dayNight == self.DAYTIME():
            maxV = self.getStats(statDict, "MaxT")
            return self._temperatureFormat(maxV)
        else:
            minV = self.getStats(statDict, "MinT")
            return self._temperatureFormat(minV)

    def _temperatureFormat(self, value):
        # Return a 3-digit string with leading zeroes given the value
        # Examples: 076 for positive, 915 for negative, MMM for missing
        if value is None:
                return "MMM"   #for missing
        value = int(round(value))
        if value < 0:
            value = abs(value) + 900
        valStr = `value`
        while len(valStr) < 3:
            valStr = "0" + valStr
        return valStr

    def _getPoP(self,analysisList,area,timeRange):
        # Return a one-digit string value representing the 10's place of
        # rounded PoP value. Values are: 0-123456789+ and "/" for missing.
        statDict = self.getStatDict(
                self._sampler, analysisList, timeRange, area)
        pop = self.getStats(statDict, "PoP__stdDevMaxAvg")
        if pop is None:
            return "/"   #Missing symbol
        popMax5=int(self.round(pop,"Nearest",5))
        if popMax5 == 5:
            return "-"
        popMax10=int(self.round(pop,"Nearest",10))
        if popMax10 >90:
            return "+"
        val="%1.1d" % int(popMax10/10)
        return val

    def _getCCFCode(self, analysisList, area, timeRange):
        # Return the CCF code (single character) which depicts the sky,
        # wind, weather, obstructions. Example: "B".  "?" is returned
        # for missing data.
        statDict = self.getStatDict(
                self._sampler, analysisList, timeRange, area)
        code = self.getCode(statDict, timeRange)
        if code is None:
            return "?"
        else:
            return code

    def _getForecasterNumber(self, fnString):
        # Returns a string representing the specified forecaster number.
        # Reformats the string to ensure it is 2 digits.
        try:
            forecasterNumber = int(fnString)
            if forecasterNumber > 99 or forecasterNumber < 0:
                return "99"
            elif forecasterNumber < 10:
                return "0" + `forecasterNumber`
            else:
                return `forecasterNumber`
        except:
            return "99"

    def _addSnowEntries(self, analysisList, timePeriods, editArea):
        # Snow entry processing. Returns ranges of snow values in the
        # edit area for each of the periods.
        # Example:    0102/0202/0000.  Will return "MMMM" for missing periods.
        # This function will "calculate" a snow range if a single value
        # is provided to it, to make the output more realistic.
        returnString = " "
        for period, label in timePeriods:
            statDict = self.getStatDict(self._sampler, analysisList,
                                    period, editArea)
            stats = self.getStats(statDict, "SnowAmt__minMaxSum")

            if stats is None:
                returnString = returnString + "MMMM/"    #Missing Data
            else:
                minV, maxV, sumV = stats
                minAdj, maxAdj = self._adjustSnowAmounts(minV, maxV, sumV)
                minString = string.rjust(`int(round(minAdj))`, 2)
                maxString = string.rjust(`int(round(maxAdj))`, 2)
                if minString[0] == " ":    # fill in leading zero
                    minString = "0" + minString[1:]
                if maxString[0] == " ":    # fill in leading zero
                    maxString = "0" + maxString[1:]
                returnString = returnString + minString + maxString + "/"

        # strip off the final "/"
        if returnString[-1] == "/":
            returnString = returnString[:-1]

        return returnString

    def _adjustSnowAmounts(self, minV, maxV, sumV):
        # Snow Amount Adjustments. Since the CCF is typically a point
        # forecast, the minV and maxV are identical, but we would prefer
        # to report a "real" range.  This method compares the min and maxV
        # and if the range isn't sufficient, then puts a range into the
        # data.  

        # rangeThreshold - min/max must differ less than this in order
        # to use the snow table, otherwise, the min/max values are used.
        rangeThreshold = 2  
        if maxV - minV > rangeThreshold:
            return minV, maxV

        # use snow table, simply based on the average (sumV) found. However,
        # the maximum returned is never less than the actual maximum found,
        # and the minimum returned is never more than the actual minimum found.
        # the tuples are (snowLessThanValue, returnedMinVal, returnedMaxVal)
        table = [\
           (0.2, 0, 0),      #less than 0.2", then report no snow at all
           (1, 0, 1), 
           (2, 1, 2), 
           (3, 1, 3), 
           (4, 2, 4), 
           (5, 3, 5),
           (6, 4, 6),
           (7, 4, 8),
           (10, 6, 10),
           (10000, int(round(sumV)), int(round(sumV))+5)
          ]

        # calculate the entry in the table that is appropriate
        for threshold, tableMin, tableMax in table:
            if sumV < threshold:
                # found a match in the table
                if tableMin > minV:
                    reportedMin = minV    #actual min is less than table min
                else:
                    reportedMin = tableMin  # use the table min
                if tableMax < maxV:
                    reportedMax = maxV    #actual max is greater than table max
                else:
                    reportedMax = tableMax  # use the table max
                return reportedMin, reportedMax

        #should never get here since the table last entry is HUGE.
     
