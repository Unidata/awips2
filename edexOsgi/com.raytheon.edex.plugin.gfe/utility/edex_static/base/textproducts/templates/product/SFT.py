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
# Description: This product creates the Tabular State Forecast, which
# consists of a 7-day forecast over multiple areas, with the following
# elements: MaxT/MinT, daytime weather, PoP, QPF, and Snow.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# SFT, SFT_<site>_<MultiPil>_Definition, SFT_<site>_Overrides
#-------------------------------------------------------------------------
# User Configurable Variables:
# Definition Section:
#
# Required Configuration Items:
#
#  displayName      If not None, defines how product appears in GFE GUI
#
#  defaultEditAreas defines edit area names, region names, and city/area
#                   names in one of two formats, depending upon whether
#                   you are supporting regional headers.  Choose one and
#                   use it throughout the product.
#                      (editAreaName, "REGIONLABEL\nCITYLABEL")
#                      (editAreaName, "CITYLABEL")
#
#  productName      defines name of product e.g. "TABULAR STATE FORECAST"
#
#  fullStationID    Full station identifier, 4 letter, such as "KSLC".
#
#  wmoID            WMO ID code for product header, such as "FOUS45"
#
#  pil              Product pil, such as "SFTBOS"
#
#  zoneCode         ZONE code for product header, such as "NYZ001>025"
#
#  stateName        State name for product header, such as "WESTERN NEW YORK"
#
#  wfoCityState     WFO location, such as "BUFFALO NY"
#
# Optional Configuration Items
#  database               Source database for product. Can be "Official", 
#                         "Fcst" or "ISC"
#  outputFile             Defines the output location of the finished product
#                         when saved from the Formatter Launcher.
#  debug                  If on, debug_print statements will appear.
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
#-------------------------------------------------------------------------
# Weather Elements Needed:
# Out to 7 days: MinT, MaxT, PoP, Wx, Sky, Wind, QPF (opt.)
#-------------------------------------------------------------------------
# Edit Areas Needed: area1, area2, area3
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file: None
#-------------------------------------------------------------------------
# Component Products: None
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
#
# To look up additional tasks and their status, see the Text Product User Guide
# Section on "Tkgnats: Task Reporting System".
#-------------------------------------------------------------------------
# Additional Information:
#   Note that time sampling will not be correct if product is updated
#   after midnight for the PM issuance.
#-------------------------------------------------------------------------
# Example Output:
#
##    FOUS45 Kxxx 241925
##    SFTxxx
##    stZALL-251025-

##    TABULAR STATE FORECAST FOR STATENAME
##    NATIONAL WEATHER SERVICE WfoCity WfoState
##    125 PM MDT TUE JUN 24 2003

##    ROWS INCLUDE...
##       DAILY PREDOMINANT DAYTIME WEATHER 6AM-6PM
##       FORECAST TEMPERATURES...EARLY MORNING LOW/DAYTIME HIGH
##          PROBABILITY OF PRECIPITATION 6AM-6PM/DAYTIME 6AM-6PM
##          - INDICATES TEMPERATURES BELOW ZERO
##          MM INDICATES MISSING DATA


##       FCST    FCST    FCST    FCST    FCST    FCST    FCST    
##       TODAY   WED     THU     FRI     SAT     SUN     MON     
##       JUN 24  JUN 25  JUN 26  JUN 27  JUN 28  JUN 29  JUN 30  

##    ...REGION1...
##       CITY1
##       SUNNY   PTCLDY  PTCLDY  PTCLDY  PTCLDY  PTCLDY  PTCLDY  
##         /77   46/61   46/70   53/75   60/75   60/79   66/86   
##          /00   20/30   30/40   30/40   30/40   30/40   30/40  

##       CITY2
##       SUNNY   PTCLDY  PTCLDY  PTCLDY  PTCLDY  PTCLDY  PTCLDY  
##         /77   46/61   46/70   53/75   60/75   60/79   66/86   
##          /00   20/30   30/40   30/40   30/40   30/40   30/40  

##    ...REGION2...
##       CITY3
##       SUNNY   PTCLDY  PTCLDY  PTCLDY  PTCLDY  PTCLDY  PTCLDY  
##         /77   46/61   46/70   53/75   60/75   60/79   66/86   
##          /00   20/30   30/40   30/40   30/40   30/40   30/40  


##    $$
#   
########################################################################

import TextRules
import SampleAnalysis
import string, time, types

from WxMethods import *


class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = [
             (("Product Issuance", "productIssuance") , "Morning", "radio",
              ["Morning","Afternoon"]),
        ]
    Definition =  {
          "type": "smart",
          "displayName": "None", # for Product Generation Menu
          "database": "ISC",
          # Defines output location of finished product.
          "outputFile": "{prddir}/TEXT/SFT_<MultiPil>.txt",
          "debug": 0,
          
          "defaultEditAreas": [("area1", "REGION1\nCITY1"),
                               ("area2", "REGION1\nCITY2"),
                               ("area3", "REGION2\nCITY3"),
                              ],

          # product identifiers
          "productName": "TABULAR STATE FORECAST", # product name 
          "fullStationID": "<fullStationID>",  # full station identifier (4letter)
          "wmoID": "<wmoID>",        # WMO ID
          "pil": "<pil>",          # Product pil
          "zoneCode": "stZ000",      # Zone Code, such as "GAZ025-056"
          "stateName": "<state>",   # Name of state, such as "GEORGIA"
          "wfoCityState": "<wfoCityState>",  # Location of WFO - city state
          "textdbPil": "<textdbPil>",       # Product ID for storing to AWIPS text database.
          "awipsWANPil": "<awipsWANPil>",   # Product ID for transmitting to AWIPS WAN.

          # Product expiration/purge time
          "fixedExpire": 1, #ensure VTEC actions don't affect expiration time


          # options
          "alwaysIncludePoP": 1,    # include PoP, 0=no,1=yes
          "alwaysIncludeQPF": 0,    # include QFP, 0=no,1=yes
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
        # Determine whether AM or PM product type

        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"

        # Make argDict accessible
        self.__argDict = argDict
        
        # Get Definition variables
        self._definition = argDict["forecastDef"]
        for key in self._definition.keys():
            exec "self._" + key + "= self._definition[key]"
        
        # row alignments
        self._rowLabelWidth = 3
        self._columnWidth = 9

    def _determineTimeRanges(self, argDict):
        # Determine time ranges for product
        # Sets up self._popPeriods, self._snowPeriods,
        # self._tempPeriods, self._codePeriods which
        # are lists of tuples (timeRange, label).

        # Calculate ddhhmm string value
        self._currentTime = argDict['creationTime']  #ZULU
        self._ddhhmmTime = time.strftime("%d%H%M",time.gmtime(
            self._currentTime))

        # Determine expiration time
        self._expireTime, self._ddhhmmTimeExpire = \
          self.getExpireTimeFromLToffset(self._currentTime,
          self.expireOffset(), "")

        # timeLabel is the spelled-out version of the current time
        self._timeLabel = self.getCurrentTime(
           argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1)

        # Number of days to sample (7 for AM, 7 for PM).  Since PoP is
        # sampled every 12 hours, we have either 13 or 14 periods to sample.
        numPeriods = 7
        if self._productIssuance == "Morning":
            numPeriodsPoP = 13
        else:
            numPeriodsPoP = 14

        # PM issuances always start tomorrow, thus we offset by 24 hours for
        # most of the items, but only 12 for the PoP, since we want to catch
        # tonights PoP.
        if self._productIssuance == "Morning":
            pmOffset = 0
            pmPopOffset = 0
        else:
            pmOffset = 24
            pmPopOffset = 12

        # table header ranges: only used for the labels
        timePeriod = 24 
        timeSpan = 12
        tableStartTR = self.createTimeRange(6+pmOffset, 6+pmOffset+11) 
        self._tablePeriods = self.getPeriods(tableStartTR, timePeriod,
                                     timeSpan, numPeriods)

        # PoP Time ranges :
        #   7 days of 12-hour periods - day and night - sync'd to 6am/6pm
        #   If AM, begin at 6am of issue day, then every 12h
        #   If PM, begin at 6pm of issue day, then every 12h
        timePeriod = 12
        timeSpan = 12
        popStartTR = self.createTimeRange(6+pmPopOffset, 6+pmPopOffset+1) #6am
        self._popPeriods = self.getPeriods(popStartTR, timePeriod,
                                     timeSpan, numPeriodsPoP)


        # QPF and Snow Time Ranges, 24hr summaries, 7 periods
        # midnight to midnight, today for AM issuance, starting tmrw for PM
        timePeriod = 24
        timeSpan = 24
        accStartTR = self.createTimeRange(0+pmOffset, 1+pmOffset)   #midnight
        self._qpfPeriods = self.getPeriods(accStartTR, timePeriod, timeSpan,
                                     numPeriods)

        # Temp Time ranges : 7 or 8 periods, 24 hours apart, 12 hour span
        # to get both the Max and Min Temps
        #   This is to catch the correct Max/Min temp grid
        #   Always begin with 3am LT of issue day to catch MinT

        timePeriod = 24
        timeSpan = 12
        tempStartTR = self.createTimeRange(3+pmOffset, 4+pmOffset)  
        self._tempPeriods = self.getPeriods(tempStartTR, timePeriod, timeSpan,
                                  numPeriods)

        # Code Time ranges :
        #   7 or 8 non-consecutive DAYLIGHT 12 hour periods
        #   If AM, begin at 6amLT of issue day
        #   If PM, begin at 6amLT tomorrow
        timePeriod = 24
        timeSpan = 12
        codeStartTR = self.createTimeRange(6+pmOffset, 7+pmOffset)
        self._codePeriods = self.getPeriods(codeStartTR, timePeriod, timeSpan,
                                      numPeriods)

        return

    def _sampleData(self, argDict):
        # Sample the data.
        # Sets up self._sampler including sampling for
        #  pop, snow, temp, and code
        sampleList = []
        sampleList.append((self._analysisListPoP(), self._popPeriods))
        sampleList.append((self._analysisListQPF(), self._qpfPeriods))
        sampleList.append((self._analysisListTemp(), self._tempPeriods))
        sampleList.append((self._analysisListCode(), self._codePeriods))
        sampleInfo = []
        for analList, periods in sampleList:
            sampleInfo.append((analList, periods, self._areaList))
        self._sampler = self.getSampler(argDict, sampleInfo)
        return

    def _preProcessProduct(self, fcst, argDict):
        # initialize variable showing last region value in body
        self._lastRegion = None

        # Add product heading to fcst string
        issuedByString = self.getIssuedByString()            
    
        productName = self._productName + " FOR " + self._stateName 
        productName = self.checkTestMode(argDict, productName)
        return fcst + self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n" +\
               self._zoneCode + "-" + self._ddhhmmTimeExpire + "-\n\n" +\
               productName + "\n" +\
               "NATIONAL WEATHER SERVICE " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n\n" + \
               self._rowDescription() + "\n\n" + self._tableHeader() + "\n\n"

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        # determine the region and area names, separated by a new line
        # only print out the region if it is different from the last one
        regarea = string.split(areaLabel, '\n')
        if len(regarea) > 1:
            region = regarea[0]
            area = regarea[1]

            # region has changed, need to output it
            if self._lastRegion is None or region != self._lastRegion:
                fcst = fcst + "..." + region + "...\n   " + area + "\n"
                self._lastRegion = region
                return fcst
            else:
                fcst = fcst + "   " + area + "\n"
        else:
            fcst = fcst + regarea[0] + "\n"
        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):

        noRowLabel = ""

        # Weather Code
        statList = self.getStatList(self._sampler,
                self._analysisListCode(), self._codePeriods, editArea)
        fcst = fcst + self.makeRow(noRowLabel, self._columnWidth,
                                   self._codePeriods, statList,
                                   self._getCCFCode, [], self._rowLabelWidth,
                                   justify = 'l')

        # Max/Min Temperatures
        statList = self.getStatList(self._sampler,
                self._analysisListTemp(), self._tempPeriods, editArea)
        fcst = fcst + self.makeRow(
            noRowLabel, self._columnWidth, self._tempPeriods,
            statList, self._mxmnValue, [],
            self._rowLabelWidth, justify = 'l')

        # PoP
        if self._alwaysIncludePoP != 0:
            statList = self.getStatList(self._sampler,
                    self._analysisListPoP(), self._popPeriods, editArea)

            #need to make our own popColumnWidth for ###/### (###, /###xx)
            popColumnWidth = []
            justifyList = []
            slashPeriods = []
            # special case, AM and 1st period, no value so slide entries right
            if self._productIssuance == "Morning":
                offset = 1
                popRowLabelWidth = self._rowLabelWidth + 3
            else:
                offset = 0
                popRowLabelWidth = self._rowLabelWidth

            for x in xrange(len(self._popPeriods)):
                if x%2 == offset:
                    wid = 3
                    justifyMode = 'r'
                else:
                    wid = self._columnWidth - 3
                    justifyMode = 'l'
                    slashPeriods.append(self._popPeriods[x])
                popColumnWidth.append(wid)
                justifyList.append(justifyMode)

            fcst = fcst + self.makeRow(noRowLabel, popColumnWidth, 
              self._popPeriods, statList, self._popValue, [slashPeriods], 
              popRowLabelWidth, justify = justifyList)


        # QPF
        if self._alwaysIncludeQPF != 0:
            statList = self.getStatList(self._sampler,
                    self._analysisListQPF(), self._qpfPeriods, editArea)
            fcst = fcst + self.makeRow(
                noRowLabel, self._columnWidth, self._qpfPeriods, statList,
                self._qpfValue, [], self._rowLabelWidth, justify = 'l')

        return fcst

    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        return fcst + "\n"

    def _postProcessProduct(self, fcst, argDict):
        fcst = string.replace(fcst,"!"," ")
        fcst = fcst + "\n$$\n\n"
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst

    # provides expiration time offset from today's midnight based on issuance.
    def expireOffset(self):
        if self._productIssuance == "Morning":
            #4pm today
            return 17
        else:
            #4am tomorrow
            return 24+5

    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################

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

    def _analysisListQPF(self):
      return [
          ("QPF", self.minMaxSum),
          ]

    def _analysisListPoP(self):
      return [
          ("PoP", self.stdDevMaxAvg),
          ]

    def _popValue(self, statDict, timeRange, argList):
        #arglist[0] is a list of timeRanges for which a leading "/" is required.
        slashList = argList[0]
        # return a string for the pop, such as "80"
        # can return MM for no data

        val = self.getStats(statDict, "PoP__stdDevMaxAvg")
        if val is None:
            popString = "MM"
        else:
            popMax10=int(self.round(val,"Nearest",10))
            popString = `popMax10`
            if popString == "0":
                popString = "00"  #requires a leading zero
       
        # leading '/' required? 
        if (timeRange, "") in slashList:
            return "/" + popString
        else:
            return popString

    def _qpfValue(self, statDict, timeRange, argList):
        # Return a string for the QPF, such as 0, 0.05
        # can return "--" for missing data
        val = self.getStats(statDict, "QPF__minMaxSum")
        if val is None:
            return "MM"
        minV, maxV, sumV = val
        if maxV < 0.01:
            return "0.00"
        else:
            return string.strip("%5.2f" %sumV)

    def _getCCFCode(self, statDict, timeRange, argList):
        translateCodes = [("P","BLZZRD"),("T","TSTRMS"),("O","RNSNOW"),
            ("R","RAIN"),("S","SNOW"),("W","SHWRS"),("J","SNOSHWR"),
            ("L","DRZL"),("X","SLEET"),("Y","FZRAIN"),("Z","FZDRZL"),
            ("M","FLRRYS"),("Q","BLGSNO"),("N","WINDY"),("F","FOGGY"),
            ("G","VRYHOT"),("I","VRYCLD"),("D","DUST"),("H","HAZE"),
            ("K","SMOKE"),("C","CLOUDY"),("E","MOCLDY"),("B","PTCLDY"),
            ("U","SUNNY"),("A","FAIR"),("V","CLEAR")
            ]
        # Return the SFT weather code
        code = self.getCode(statDict, timeRange)
        if code is None:
            return "MM"
        else:
            for symbol, word in translateCodes:
                if code == symbol:
                    return word
            return "MM"


    def _mxmnValue(self, statDict, timeRange, argList):
        # Return a string for the max or min temperatures.
        # String may be a single value, such as /75, or 54/75  (Min/Max)
        # Can return MM for missing data
        # Get the numbers
        maxval = self.getStats(statDict, "MaxT")
        if maxval == None:
            maxString = "MM"
        else:
            maxString = `int(round(maxval))`
        minval = self.getStats(statDict, "MinT")
        if minval == None:
            minString = "MM"
        else:
            minString = `int(round(minval))`

        # special case, AM and 1st period, only 1 number (MaxT) goes here
        if timeRange == self._tempPeriods[0][0] and self._productIssuance == "Morning":
            return "!!/" + maxString

        # normal cases, two numbers (Min/Max)
        else:
            return minString + "/" + maxString


    def _tableHeader(self):
        # Put in the table header with the dates
        # qpfPeriods are midnight to midnight - convenient for labeling
        fcstString, dayNumString, dateString = self._calcPeriodLabels(
            self._tablePeriods, self._columnWidth, self._rowLabelWidth)
        return fcstString + "\n" + dayNumString + "\n" + dateString

    def _popTimeLabel(self):
        # Returns the valid time for the daily POP field
        return " NIGHTTIME 6PM-6AM/DAYTIME 6AM-6PM"

    def _qpfTimeLabel(self):
        # Returns the valid time for the daily qpf field
        return " 12AM-12AM"

    def _rowDescription(self):
        # Returns the row description between the product title and
        # table header.  Override in your local file as needed.

        ident = "   "

        # s is the built-up string containing the description
        s =  "ROWS INCLUDE...\n"


        # Weather
        s = s + ident + "DAILY PREDOMINANT DAYTIME WEATHER 6AM-6PM"

        # Temps
        s = s + "\n" + ident + \
           "FORECAST TEMPERATURES...EARLY MORNING LOW/DAYTIME HIGH" 

        # PoP
        if self._alwaysIncludePoP:
            s = s + "\n" + ident + ident + ident + \
              "PROBABILITY OF PRECIPITATION" + self._popTimeLabel()

        # other stuff
        s = s + "\n" + \
           ident + ident + ident + " - INDICATES TEMPERATURES BELOW ZERO\n" + \
           ident + ident + ident + "MM INDICATES MISSING DATA"

        #QPF
        if self._alwaysIncludeQPF:
            s = s + "\n" + ident + \
              "QUANTITATIVE PRECIPITATION - INCHES -" + \
                self._qpfTimeLabel()


        return s + "\n"

    def _calcPeriodLabels(self, periods, colWidth, startPoint):
        fcstString = string.ljust(" ", startPoint)
        dayNumString = string.ljust(" ", startPoint)
        dateString = string.ljust(" ", startPoint)

        for index in xrange(len(periods)):
            fcstString = fcstString + string.ljust("FCST", colWidth)
            dayNumString = dayNumString + string.ljust(
                self._periodNumString(index, periods[index][0]), colWidth)
            timeRange, label = periods[index]
            dateString = dateString + string.ljust(
                self._formatDate(timeRange), colWidth)

        return (fcstString, dayNumString, dateString)

    def _formatDate(self, timeRange):
        # calculate the local time
        startTime = timeRange.startTime() + self.determineShift()
        label = startTime.stringFmt("%b %d")
        return label

    def _periodNumString(self, num, period):
        #AM: Today, Tmrw, NextDay, Day 4, 5, 6, 7
        #PM: Tngt, Tmrw, NextDay, Day 3, 4, 5, 6
        if num == 0 and self._productIssuance == "Morning":
            return "Today"
        else:
            # calculate the day of the week based on the period
            periodTime = period.startTime().unixTime()
            return time.strftime("%a", time.localtime(periodTime))

