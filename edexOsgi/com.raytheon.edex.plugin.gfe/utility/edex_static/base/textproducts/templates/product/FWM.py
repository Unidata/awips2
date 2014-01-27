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
# Description: This product creates a table of Fire Weather values for a list of edit areas.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# FWM, FWM_<site>_<MultiPil>_Definition, FWM_<site>_Overrides
#-------------------------------------------------------------------------
# Customization Points:
#
# DEFINITION SECTION
#
# Required Configuration Items:
#
#  displayName      If not None, defines how product appears in GFE GUI
#  defaultEditAreas defines edit areas, default is Combinations
#
#  fullStationID    Full station identifier, 4 letter, such as "KSLC".
#  wmoID            WMO ID code for product header, such as "FOUS45"
#  pil              Product pil, such as "FWMBOS"
#
#  Optional Configuration Items
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
#  reportAsTrendsForZONE    If 1, Report data values as trends rather 
#                           than actual values for ZONE.
#  reportAsTrendsForFCST    If 1, Report data values as trends rather 
#                           than actual values for FCST.
#  reportTandRHforZONE      If 1, Report T and RH values for ZONE
#  reportTandRHforFCST      If 1, Report T and RH values for FCST
#  reportWindDirectionForZONE  If 1, Inserts comma placeholder for wind 
#                              direction for ZONE.
#  reportWindDirectionForFCST  If 1, Reports wind direction for FCST. 
#                              If reporting as trends, inserts placeholder 
#                              comma.
#  textDirection            If 1, the wind direction (if included) 
#                           is 16-point compass instead of numeric.
#  fuelMoisturePlaceHolder  String to hold the place for fuel moisture
#  windAdjustmentFactor     Winds are reported from the Wind20ft grid if 
#                           available.Otherwise, the Wind grid is used 
#                           with the magnitude multiplied by this wind 
#                           adjustment factor. Winds reported by RAWS sites 
#                           are frequently lower than ASOS winds due to the 
#                           fact that use a 10-min average.  A common 
#                           adjustment factor is 80% (0.80).  If you want 
#                           no adjustment to the winds then set this variable 
#                           to 1.00.
#  wxDurPopThreshold        Pop threshold for reporting wx duration
#-------------------------------------------------------------------------
# Weather Elements Needed:
#        Sky
#        Wx
#        T
#        Wind
#        LAL
#        RH
#        PoP
#        Wetflag
#        Ttrend (optional -- if not provided prior day's data is used)
#        RHtrend (optinal -- if not provided prior day's data is used)
#-------------------------------------------------------------------------
# Edit Areas Needed: area1, area2
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file:
#-------------------------------------------------------------------------
# Component Products: None
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
#
# To look up tasks and their status, see the Text Product User Guide
# Section on "Tkgnats: Task Reporting System".
#-------------------------------------------------------------------------
# Additional Information:
#-------------------------------------------------------------------------
# Example Output:
#  Refer to the NWS Directives for Fire Weather Services.
#######################################################################


import TextRules
import SampleAnalysis
import string, time, types
import math

class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    Definition =  {
          "type": "smart",
          "displayName": "None", # for Product Generation Menu
          # Source database for product. Can be "Official", "Fcst" or "ISC"
          "database": "Official",
          # Defines output location of finished product.
          "outputFile": "{prddir}/TEXT/FWM_<MultiPil>.txt",
          "debug": 0,
          
          "defaultEditAreas": [("area1", "086401"),
                               ("area2", "086402"),
                               ("area3", "668"),
                              ],
          # product identifiers
          "fullStationID": "<fullStationID>",    # full station identifier (4letter)
          "wmoID": "<wmoID>",          # WMO ID
          "pil": "<pil>",            # Product pil
          "textdbPil": "<textdbPil>",       # Product ID for storing to AWIPS text database.
          "awipsWANPil": "<awipsWANPil>",   # Product ID for transmitting to AWIPS WAN.
          
          # optional variables
          "reportAsTrendsForZONE": 1, # Report data values as trends for ZONE
          "reportAsTrendsForFCST": 1, # Report data values as trends for FCST
          "reportTandRHforZONE": 1,   # Report T and RH values for ZONE
          "reportTandRHforFCST": 1,   # Report T and RH values for FCST
          # wind direction
          "reportWindDirectionForZONE": 0, # If 1, Inserts comma placeholder 
                                           # for wind direction for ZONE
          "reportWindDirectionForFCST": 1, # Reports wind direction for FCST.
                                           # If reporting as trends, inserts 
                                           # comma.
          "textDirection": 1,   # Report wind direction as 16-point compass
          "fuelMoisturePlaceHolder": "",  # String to hold the place for 
                                          # fuel moisture.
          "windAdjustmentFactor": 0.80,   # Adjustment for Wind if 
                                          # no Wind20ft grid.
          "wxDurPopThreshold": 70,        # Pop threshold for reporting wx duration
          }

    ########################################################################
    # PRODUCT-SPECIFIC THRESHOLDS AND VARIABLES
    ########################################################################           
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
        # Make argDict accessible
        self.__argDict = argDict
        
        # Get Definition variables
        self._definition = argDict["forecastDef"]
        for key in self._definition.keys():
            exec "self._" + key + "= self._definition[key]"

        self._issueTime = self.getCurrentTime(argDict, "%H%M%S", upperCase=1)
        self._WIMSTime = self._getWIMSTime(argDict['creationTime'])
        self._definition = argDict["forecastDef"]

        # Get variables from VariableList
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"             

        # Calculate current times
        self._ddhhmmTime = self.getCurrentTime(
            argDict, "%d%H%M", shiftToLocal=0, stripLeading=0)
        self._timeLabel = self.getCurrentTime(
            argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1)
        return None
            

    def _determineTimeRanges(self, argDict):
        # Set up self._timeRangeList
        self._timeRangeList = []
        ranges = [
            (37,38,"sky_range"),             # Tomorrow 13L_14L
            (37,38,"temp_range"),            # Tomorrow 13L_14L
            (37,38,"rh_range"),              # Tomorrow 13L_14L
            (14,23,"lal1_range"),            # Today 13L-23L
            (23,47,"lal2_range"),            # Today 23L-Tomorrow 23L
            (37,38,"wind_range"),            # Tomorrow 13L_14L
            (13,38,"maxT_range"),            # Today 13L Tomorrow 13L
            (13,38,"minT_range"),            # Today_13L_Tomorrow_13L
            (13,38,"maxRH_range"),           # Today_13L_Tomorrow_13L
            (13,38,"minRH_range"),           # Today_13L_Tomorrow_13L 
            (13,29,"wxDur1_range"),          # Today 13L Tomorrow_05L
            (29,37,"wxDur2_range"),          # Tomorrow_05L_13L
            (37,38,"wetflag_range"),         # Tomorrow 13L_14L 
            (13,14,"prior_range"),           # Today 13L_14L 
            ]
        daylight = self.daylight()
        # If daylight savings in effect, adjust so that times are
        # local standard times (LST)
        for startHour, endHour, name in ranges:
            if daylight:
                startHour = startHour+1
                endHour = endHour+1
            range = self.createTimeRange(startHour, endHour)
            self._timeRangeList.append((range, name))
        return

    def _sampleData(self, argDict):
        # Sample the data
        self._sampler = self.getSampler(argDict, 
          (self._getAnalysisList(), self._timeRangeList, self._areaList))
        return

    def _preProcessProduct(self, fcst, argDict):
        # This is the header for the overall product
        fcst =  fcst + self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n"
        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        # This is the header for an edit area
        if len(areaLabel) == 6:
            label = "FCST,"
            self._productType = "FCST"
        else:
            label = "ZONE,"
            self._productType = "ZONE"
        fcst = fcst + label + areaLabel + "," + self._WIMSTime + ",13,"
        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict): 
        self._statList = self.getStatList(
            self._sampler, self._getAnalysisList(), self._timeRangeList, editArea) 
        #print "\n\n", areaLabel, self._statList 

        # State of Sky 
        str = self._sky("sky_range") 
        fcst = fcst + str + "," 

        # Temp 
        str = self._getStrValue("T", "temp_range",
                                trendElement="Ttrend", priorRangeName="prior_range") 
        fcst = fcst + str + "," 

        # RH 
        str = self._getStrValue("RH", "rh_range",
                                trendElement="RHtrend", priorRangeName="prior_range") 
        fcst = fcst + str + "," 

        # LAL1 
        str = self._getStrValue("LAL", "lal1_range", stats="max") 
        fcst = fcst + str + "," 

        # LAL2 
        str = self._getStrValue("LAL", "lal2_range", stats="max") 
        fcst = fcst + str + "," 

        # Wind Dir and Speed 
        str = self._getWind("wind_range", priorRangeName="prior_range") 
        fcst = fcst + str + "," 

        # Fuel Moisture - Place holder
        fcst = fcst + self._fuelMoisturePlaceHolder + ","

        # T and RH
        reportTandRH = 0
        if self._productType == "ZONE" and self._reportTandRHforZONE == 1:
            reportTandRH = 1
        if self._productType == "FCST" and self._reportTandRHforFCST == 1:
            reportTandRH = 1
            
        if reportTandRH == 0:
            fcst = fcst + ",,,," 
        else: 
            # MaxT 
            str = self._getStrValue("T", "maxT_range", stats="max") 
            fcst = fcst + str + "," 

            # MinT 
            str = self._getStrValue("T", "minT_range", stats="min") 
            fcst = fcst + str + "," 

            # MaxRH 
            str = self._getStrValue("RH", "maxRH_range", stats="max") 
            fcst = fcst + str + "," 

            # MinRH 
            str = self._getStrValue("RH", "minRH_range", stats="min") 
            fcst = fcst + str + "," 

        # WxDur1 
        percent = self._getValue("PoP", "wxDur1_range") 
        if percent is None or percent <= self._wxDurPopThreshold: 
            str = "0" 
        else: 
            str = self._getWxDur("wxDur1_range") 
        fcst = fcst + str + "," 

        # WxDur2 
        percent = self._getValue("PoP", "wxDur2_range") 
        if percent is None or percent <= self._wxDurPopThreshold: 
            str = "0" 
        else: 
            str = self._getWxDur("wxDur2_range") 
        fcst = fcst + str + "," 

        # Wet Flag 
        str = self._getWetflag("wetflag_range") 
        fcst = fcst + str 

        return fcst 
 
    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        # This is the footer for an edit area
        return fcst + "\n"

    def _postProcessProduct(self, fcst, argDict):
        fcst = string.replace(
            fcst,"%IssueTime",
            self.getCurrentTime(argDict,"%d%H%M", shiftToLocal=0, upperCase=1))
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst
    
    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################
    
    def _getAnalysisList(self):
        return [
          ("Sky", self.avg),
          ("Wx", self.dominantWx),
          ("Wx", self.dominantWx, [0]),
          ("T", self.avg),
          ("T", self.minMax),
          ("Wind", self.vectorAvg),
          ("Wind20ft", self.vectorAvg),
          ("LAL", self.minMax),
          ("RH", self.avg),
          ("RH", self.minMax),
          ("PoP", self.stdDevMaxAvg),
          ("Wetflag", self.avg),
          ("Ttrend", self.avg),  # for trends
          ("RHtrend", self.avg),  # for trends
          ]  
     
    def _getWIMSTime(self, currentTime):
        t_shift = 24 # Number of hours from current time
        return time.strftime("%y%m%d",time.localtime(currentTime+t_shift*60*60))

    def _sky(self, rangeName):
        # Return a sky value
        sky = self._getValue("Sky", rangeName)
        pop = self._getValue("PoP", rangeName)
        if sky is None or pop is None:
            return ""
        if pop < 70:
            if sky <= 10:
                value = "0"
            elif sky <= 50:
                value = "1"
            elif sky <= 90:
                value = "2"
            elif sky > 90:
                value = "3"
            return value
        else:
            value = self._getWxCode(rangeName)
            return value

    def _getWxCode(self, rangeName):
        i=0
        for period, label in self._timeRangeList:
            if label == rangeName:
                break
            i=i+1
        subKeyList =  self.getStats(self._statList[i], "Wx")
        if subKeyList is None or len(subKeyList) == 0:
            return ""
        # Take the first weather key of a combination
        # There is a possible problem if RW shows up before T
        # So we want to catch T if it occurs?
        wxKey = subKeyList[0]
        wxType = wxKey.wxType()

##        if wxType == "F":
##            return "4"
##        if wxType ==  "L":
##            return "5"
        if wxType == "R":
            return "6"
        if wxType == "S" or wxType == "IP":
            return "7"
        if wxType == "RW":
            return "8"
        if wxType == "T":
            return "9"
        return ""

    def _getWind(self, rangeName, priorRangeName=None):
        # Use Wind20ft if available, otherwise adjust Wind
        adjust = 0
        value = self._getValue("Wind20ft", rangeName)
        if value is None:
            value = self._getValue("Wind", rangeName)
            if value is None:
                return ""
            adjust = 1
        mag, dir = value
        mag_mph = self.ktToMph(mag)
        if adjust:
            mag_mph = mag_mph * self._windAdjustmentFactor
        mag_mph = int(mag_mph)
        # Report as trend OR by value
        if self._reportAsTrends() == 1 and priorRangeName is not None:
            adjustPrior = 0
            priorValue = self._getValue("Wind20ft", priorRangeName)
            if priorValue is None:
                priorValue = self._getValue("Wind", priorRangeName)
                if priorValue is None:
                    pass
                else:
                    adjust = 1
            if priorValue is None:
                diff = 0
            else:
                priorMag, dir = priorValue
                priorMag_mph = int(self.ktToMph(priorMag))
                if adjust:
                    priorMag_mph = priorMag_mph * self._windAdjustmentFactor
                diff = int(mag_mph - priorMag_mph)
            if diff >= 0:
                magStr =  "+"+`diff`
            else:
                magStr =  `diff`
            dir = None
        # By Value
        else:
            mag_mph = int(mag_mph)
            if mag_mph < 10:
                magStr =  "0" +`mag_mph`
            else:
                magStr =  `mag_mph`
                
        # Handle direction
        dirStr = ""
        if self._productType == "ZONE" and self._reportWindDirectionForZONE == 1:
            dirStr = ","
        elif self._productType == "FCST" and self._reportWindDirectionForFCST == 1:
            if dir is None:
                dirStr = ","
            elif self._textDirection == 1:
                dirStr = self.dirTo16PtText(dir) + ","
            else:
                dirStr = `int(dir)` + ","
        return dirStr + magStr
            
    def _getWxDur(self, rangeName):
        statsByRange = self._getValue("Wx__dominantWx_0", rangeName)
        if statsByRange is None:
            return ""
        for range, label in self._timeRangeList:
            if label == rangeName:
                timeRange = range
                break
        return self.wxDuration(statsByRange, timeRange)
    
    def _getStrValue(self, element, rangeName, trendElement=None, priorRangeName=None, stats="avg"):
        # Return a scalar text string value
        if stats == "min" or stats == "max":
            element = element + "__minMax"
        value = self._getValue(element, rangeName, stats)
        if value is None:
            return ""
        value = int(value)
        if self._reportAsTrends() == 1:
            if trendElement is not None:
                # Try to get data from trendElement
                trendValue = self._getValue(trendElement, rangeName, stats)
                if trendValue is not None:
                    return `int(trendValue)`                
            if priorRangeName is not None:
                # Get data from prior day's data
                priorValue = self._getValue(element, priorRangeName, stats)
                if priorValue is not None:
                    priorValue = int(priorValue)
                    diff = value - priorValue
                    if diff >= 0:
                        return "+"+`diff`
                    else:
                        return `diff`
        return `value`

    def _getValue(self, element, rangeName, stats="avg"): 
        # Return a scalar value 
        i=0 
        for period, label in self._timeRangeList: 
            if label == rangeName: 
                break 
            i=i+1 
        if stats == "avg": 
            return self.getStats(self._statList[i], element) 
        if stats == "max": 
            value = self.getStats(self._statList[i], element) 
            if value is not None: 
                value = value[1] 
            return value 
        if stats == "min": 
            value = self.getStats(self._statList[i], element) 
            if value is not None: 
                value = value[0] 
            return value

    def _getWetflag(self, rangeName):
        value = self._getValue("Wetflag", rangeName)
        if value is None:
            return "N"
        if value == 0:
            return "N"
        else:
            return "Y"

    def _reportAsTrends(self):
        if self._productType == "FCST" and self._reportAsTrendsForFCST == 1:
            return 1
        if self._productType == "ZONE" and self._reportAsTrendsForZONE == 1:
            return 1
        return 0

            

