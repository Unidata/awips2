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
# Description: The produces a Marine Verification Forecast.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# MVF.py, MVF_<site>_<MultiPil>_Definition, MVF_<site>_Overrides
#-------------------------------------------------------------------------
# Customization Points in Local File:
#
# DEFINITION SECTION
#
# Required Configuration Items:
#
#  displayName      If not None, defines how product appears in GFE GUI
#  defaultEditAreas defines edit areas, default is Combinations
#
#  productName      defines name of product e.g. "MARINE VERIFICATION FORECAST"
#  fullStationID    Full station identifier, 4 letter, such as "KSLC".
#  wmoID            WMO ID code for product header, such as "FOUS45"
#  pil              Product pil, such as "MVFBOS"
#  zoneCode         ZONE code for product header, such as "NYZ001>025"
#  stateName        State name for product header, such as "WESTERN NEW YORK"
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
#      Wind
#      WaveHeight
#-------------------------------------------------------------------------
# Edit Areas Needed: area1, area2
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file:
# None
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
#
# To look up additional tasks and their status, see the Text Product User Guide
# Section on "Tkgnats: Task Reporting System".
#-------------------------------------------------------------------------
# Additional Information:
#-------------------------------------------------------------------------
# Example Output:
#  Refer to the NWS Directives for Marine Services.
#######################################################################

import TextRules
import SampleAnalysis
import string, time, types
import math

class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = [
             (("Forecaster Number", "forecasterNumber"), 99, "alphaNumeric"),
    #         (("Tropical Storm Winds", "tropicalStorm"), "no", "radio", ["NO", "YES"]),
            ]
    Definition =  {
        "type": "smart",
        "displayName": "None", # for Product Generation Menu
        "database": "Official",
        # Defines output location of finished product.
        "outputFile": "{prddir}/TEXT/MVF_<MultiPil>.txt",
        "debug": 0,

        # Label is Buoy/C-MAN identifier
        "defaultEditAreas": [("area1", "45004"),
                               ("area2", "45005"),
                            ],
        # product identifiers
        "productName": "MARINE VERIFICATION FORECAST", # product name 
        "fullStationID": "<fullStationID>",    # full station identifier (4letter)
        "wmoID": "<wmoID>",          # WMO ID
        "pil": "<pil>",            # Product pil
        "zoneCode": "stZALL",       # Zone Code, such as "GAZ025-056"
        "stateName": "<state>",   # Name of state, such as "GEORGIA"
        "wfoCityState": "<wfoCityState>", # Location of WFO - city state
        
        "textdbPil": "<textdbPil>",       # Product ID for storing to AWIPS text database.
        "awipsWANPil": "<awipsWANPil>",   # Product ID for transmitting to AWIPS WAN.
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
        self.setProgressPercentage(percent)
        for editArea, areaLabel in self._areaList:
            self.progressMessage(fraction, percent, "Making Product for " + areaLabel)
            fcst = self._preProcessArea(fcst, editArea, areaLabel, argDict)
            fcst  = self._makeProduct(fcst, editArea, areaLabel, argDict)
            fcst = self._postProcessArea(fcst, editArea, areaLabel, argDict)
            fraction = fractionOne
        fcst = self._postProcessProduct(fcst, argDict)
        return fcst

    def _getVariables(self, argDict):
        # Get variables from VariableList
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"

        try:
            if self._tropicalStorm == "YES":
                self._tropicalStorm = 1
            else:
                self._tropicalStorm = 0
        except:
            self._tropicalStorm = 0

        try:
            forecasterNumber = int(self._forecasterNumber)
            if forecasterNumber < 10:
                self._forecasterNumber = "0" + `forecasterNumber`
            else:
                self._forecasterNumber = `forecasterNumber`
        except:
            pass

        # Make argDict accessible
        self.__argDict = argDict
        
        # Get Definition variables
        self._definition = argDict["forecastDef"]
        for key in self._definition.keys():
            exec "self._" + key + "= self._definition[key]"
        self._expirationTimeOffset = 12  #hours
        return None

##        ## OLD ALGORITHM
##        # Determine time ranges for product
##        # Set up self._timeRangeList
##        # Based on currentLocalTime time:
##        # if prior to 12Z, use 18Z today and 6Z tomorrow
##        # else use 6Z tomorrow and 18Z tomorrow
##        currentLocalTime, shift = self.determineTimeShift()
##        if currentLocalTime.hour() < 12:
##            startHour = 18
##            label1 = "18"
##            label2 = "06"
##        else:
##            startHour = 24+6
##            label1 = "06"
##            label2 = "18"
##        tr1 = self.createTimeRange(startHour, 12)
##        tr2 = self.createTimeRange(startHour + 12, 12)
##        self._timeRangeList = [(tr1, label1), (tr2,label2)]

    def _determineTimeRanges(self, argDict):
        # Determine time ranges for product
        # Set up self._timeRangeList
        # based on current time.
        
        # Assume that: Verification is done for a 5 hour window
        # centered around 06Z and 18Z. (Belk)
        # So the time ranges are 04z up to 09Z and 16Z up to 21Z.
        # 09Z and 21Z are not included.

        # If current time is prior to 12Z,
        # use 18Z today and 6Z tomorrow
        # else use 6Z tomorrow and 18Z tomorrow
        gmTime = time.gmtime(argDict['creationTime'])
        gmHour = gmTime[3]
        if gmHour < 12:
            startHour = 16
            label1 = "18"
            label2 = "06"
        else:
            startHour = 24+4
            label1 = "06"
            label2 = "18"
        tr1 = self.createTimeRange(startHour, startHour + 5, mode="Zulu")
        tr2 = self.createTimeRange(startHour + 12, startHour + 12 + 5, mode="Zulu")
        self._timeRangeList = [(tr1, label1), (tr2,label2)]

        #print self._timeRangeList
        
        # Calculate current times
        self._ddhhmmTime = self.getCurrentTime(
            argDict, "%d%H%M", shiftToLocal=0, stripLeading=0)
        self._timeLabel = self.getCurrentTime(
            argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1)
 
        # Determine expiration time
        expireTime = argDict['creationTime'] + self._expirationTimeOffset*3600
        self._expireTime = time.strftime("%d%H%M",time.gmtime(expireTime)) 
        
        return
 
    def _sampleData(self, argDict):
        # Sample the data
        self._sampler = self.getSampler(argDict, 
          (self._getAnalysisList(), self._timeRangeList, self._areaList))

    def _preProcessProduct(self, fcst, argDict):
        fcst =  fcst + self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n"                     
        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        #self._issueTime = self.getCurrentTime(argDict)
        #fcst = fcst + areaLabel + "\n" + self._issueTime + "\n\n"
        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        statList = self.getStatList(self._sampler, self._getAnalysisList(),\
                                    self._timeRangeList, editArea)

        fcst = fcst + "%%F"
        fcst = fcst + self._forecasterNumber
        fcst = fcst + " " + areaLabel + " "

        index = 0
        for statDict in statList:
            fcst = fcst + self._timeRangeList[index][1] + "/"

            # Warning Status
            str = self._warningStatus(statDict, argDict)
            fcst = fcst + str + "/"

            # Wind direction and speed : ddff
            str = self._windDirSpeed(statDict, argDict)
            fcst = fcst + str + "/"

            # Wave Height
            str = self._sigWaveHeight(statDict, argDict)
            fcst = fcst + str + "/"

            index += 1

        return fcst

    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        fcst = fcst + "\n"
        return fcst

    def _postProcessProduct(self, fcst, argDict):
        fcst = fcst + "\n\n$$"
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst

    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################

    def _getCurrentTimeString(self):
        return time.strftime("%d%M%H", time.gmtime())

    def _getAnalysisList(self):
      return [
          ("Wind", self.vectorAvg),
          ("WaveHeight", self.avg),
          ]

    def _warningStatus(self, statDict, argDict):
        # Return a warning status
        wind = self.getStats(statDict, "Wind")
 
        # Need to use total or 'combined' seas
        waves = self.getStats(statDict, "WaveHeight")  # fixed

        if wind is None:
            return "NO"
        mag, dir = wind

        #non-tropical
        if self._tropicalStorm == 0:
            if mag < 25 and waves < 5:
                return "NO"
            elif mag < 34:                     # gales start at 34 kts
                return "SC"
            elif mag < 48:                     # storms start at 48 kts
                return "GL"
            elif mag < 63:
                return "ST"
            else:
                return "HF"
        #tropical
        else:
            if mag < 25 and waves < 5:
                return "NO"
            elif mag < 34:                     # gales start at 34 kts
                return "SC"
            elif mag < 63:                     # TS/HR winds
                return "TS"
            else:
                return "HR"

         
    def _windDirSpeed(self, statDict, argDict):
        # Return a wind direction and speed

        wind = self.getStats(statDict, "Wind")
        if wind is None:
            return "9999"
        mag, dir = wind
        #print "wind mag:", mag, " dir:", dir

        # initial direction
        if self._variableWinds(mag, dir) == 1:  #variable winds
            dirTens = 99
        else: 
            dir = self.round(dir, "Nearest", 10)
            dirTens = int(dir/10.0)
            if dirTens == 0:
                dirTens = 36   #want 36 for N, not 0
        mag = int(self.round(mag, "Nearest", 1))

        intvalue = dirTens*100 + mag%100   #output value as int

        # check to see if mag >= 100, a special case
        # add 50 to the direction value
        while mag >= 100:
            mag = mag - 100
            intvalue = intvalue + 5000

        #format as string
        return "%04i" % intvalue

    def _variableWinds(self, mag, dir):
        #  This method can be easily overridden with
        #  criteria for variable winds
        if mag < 5:
            return 1
        else:
            return 0

    def _sigWaveHeight(self, statDict, argDict):
        # Return a significant WaveHeight
        wave = self.getStats(statDict, "WaveHeight")
        if wave is None:
            return "99"
        if wave < 10:
            return "0" + `int(wave)`
        else:
            return `int(wave)`
