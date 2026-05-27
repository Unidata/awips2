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
#
#
# SOFTWARE HISTORY
#
# Date            Ticket#       Engineer       Description
# ------------    ----------    -----------    --------------------------
# 03/15/2020      DCS21216      J. Lamb        Enable 7-day NFDRS Forecast.
#
##
##
# This is a base file that is not intended to be overridden.
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
#  enableSevenDayFwm        If 1, FWM outputs a forecast for each of the next 7 days.
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
import time

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
          "enableSevenDayFwm": 1, # Toggle 7-day or 1-day FWM
          "reportAsTrendsForZONE": 0, # Report data values as trends for ZONE
          "reportAsTrendsForFCST": 0, # Report data values as trends for FCST
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
        for (key, value) in self._definition.items():
            setattr(self, f"_{key}", value)

        self._issueTime = self.getCurrentTime(argDict, "%H%M%S", upperCase=1)
        self._WIMSTime = self._getWIMSTime(argDict['creationTime'])
        self._definition = argDict["forecastDef"]

        # Get variables from VariableList
        varDict = argDict["varDict"]
        for (key, value) in varDict.items():
            if type(key) is tuple:
                label, variable = key
                setattr(self, f"_{variable}", value)     

        # Calculate current times
        self._ddhhmmTime = self.getCurrentTime(
            argDict, "%d%H%M", shiftToLocal=0, stripLeading=0)
        self._timeLabel = self.getCurrentTime(
            argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1)
        return None
            
    def _determineTimeRanges(self, argDict):
        # Set up self._timeRangeList
        self._timeRangeList = []

        if self._enableSevenDayFwm:
            dayRange = list(range(0, 7))
        else:
            dayRange = [0]

        ranges = []
        for dy in dayRange:

            # SkyTempRHWindWetFlag Range
            ranges.append(
                (37 + (dy * 24), 38 + (dy * 24), "SkyTempRHWindWetFlag_range{}".format(dy))
            )
            # LAL1 Range
            ranges.append(
                (14 + (dy * 24), 23 + (dy * 24), "LAL1_range{}".format(dy))
            )
            # LAL2 Range
            ranges.append(
                (23 + (dy * 24), 47 + (dy * 24), "LAL2_range{}".format(dy))
            )
            # MaxMinTRH Range
            ranges.append(
                (13 + (dy * 24), 38 + (dy * 24), "MaxMinTRH_range{}".format(dy))
            )
            # WxDur1 Range
            ranges.append(
                (13 + (dy * 24), 29 + (dy * 24), "WxDur1_range{}".format(dy))
            )
            # WxDur2 Range
            ranges.append(
                (29 + (dy * 24), 37 + (dy * 24), "WxDur2_range{}".format(dy))
            )
            # Prior Range
            ranges.append(
                (13 + (dy * 24), 14 + (dy * 24), "Prior_range{}".format(dy))
            )

        daylight = self.daylight()
        # If daylight savings in effect, adjust so that times are in LST
        for startHour, endHour, name in ranges:
            if daylight:
                startHour += 1
                endHour += 1
            tr = self.createTimeRange(startHour, endHour)
            self._timeRangeList.append((tr, name))
        return

    def _sampleData(self, argDict):
        # Sample the data
        self._sampler = self.getSampler(argDict, 
          (self._getAnalysisList(), self._timeRangeList, self._areaList))
        return

    def _preProcessProduct(self, fcst, argDict):
        # This is the header for the overall product
        s = self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n"
        fcst = fcst + s.upper()
        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        # Determine product type. No formatting done here.
        if len(areaLabel) == 6:
            self._productType = "FCST"
        else:
            self._productType = "ZONE"
        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict): 
        self._statList = self.getStatList(
            self._sampler, self._getAnalysisList(), self._timeRangeList, editArea)
        # print("\n\n", areaLabel, self._statList)

        if self._enableSevenDayFwm:
            dayRange = list(range(0, 7))
        else:
            dayRange = [0]

        for dy in dayRange:

            # Initialize this line with a blank string
            fcstString = ""

            # Simplify time range names
            SkyTempRHWindWetFlag_TR = "SkyTempRHWindWetFlag_range{}".format(dy)
            LAL1_TR = "LAL1_range{}".format(dy)
            LAL2_TR = "LAL2_range{}".format(dy)
            Prior_TR = "Prior_range{}".format(dy)
            MaxMinTRH_TR = "MaxMinTRH_range{}".format(dy)
            WxDur1_TR = "WxDur1_range{}".format(dy)
            WxDur2_TR = "WxDur2_range{}".format(dy)

            # Get timestamp for this entry
            currentT = argDict['creationTime']
            WIMSTime = time.strftime("%y%m%d",
                                     time.localtime(currentT + (24 * (dy + 1)) * 3600))

            # Setup station name
            fcstString += self._productType + "," + areaLabel + "," + WIMSTime + ",13,"

            # State of Sky
            data = self._sky(SkyTempRHWindWetFlag_TR)
            fcstString += data + ","

            # Temp
            data = self._getStrValue("T", SkyTempRHWindWetFlag_TR, trendElement="Ttrend",
                                     priorRangeName=Prior_TR)
            fcstString += data + ","

            # RH
            data = self._getStrValue("RH", SkyTempRHWindWetFlag_TR, trendElement="RHtrend",
                                     priorRangeName=Prior_TR)
            fcstString += data + ","

            # LAL1
            data = self._getStrValue("LAL", LAL1_TR, stats="max")
            if not data:
                data = "1"
            fcstString += data + ","

            # LAL2
            data = self._getStrValue("LAL", LAL2_TR, stats="max")
            if not data:
                data = "1"
            fcstString += data + ","

            # Wind Dir and Speed
            data = self._getWind(SkyTempRHWindWetFlag_TR, priorRangeName=Prior_TR)
            fcstString += data + ","

            # Fuel Moisture - Place holder
            fcstString += self._fuelMoisturePlaceHolder + ","

            # T and RH
            reportTandRH = 0
            if (
                (self._productType == "ZONE" and self._reportTandRHforZONE == 1) or
                (self._productType == "FCST" and self._reportTandRHforFCST == 1)
            ):
                reportTandRH = 1

            if reportTandRH == 0:
                fcstString += ",,,,"
            else:
                # MaxT
                data = self._getStrValue("T", MaxMinTRH_TR, stats="max")
                fcstString += data + ","

                # MinT
                data = self._getStrValue("T", MaxMinTRH_TR, stats="min")
                fcstString += data + ","

                # MaxRH
                data = self._getStrValue("RH", MaxMinTRH_TR, stats="max")
                fcstString += data + ","

                # MinRH
                data = self._getStrValue("RH", MaxMinTRH_TR, stats="min")
                fcstString += data + ","

            # WxDur1
            percent = self._getValue("PoP", WxDur1_TR)
            if percent is None or percent <= self._wxDurPopThreshold:
                data = "0"
            else:
                data = self._getWxDur(WxDur1_TR)
            fcstString += data + ","

            # WxDur2
            percent = self._getValue("PoP", WxDur2_TR)
            if percent is None or percent <= self._wxDurPopThreshold:
                data = "0"
            else:
                data = self._getWxDur(WxDur2_TR)
            fcstString += data + ","

            # Wet Flag
            data = self._getWetflag(SkyTempRHWindWetFlag_TR)
            fcstString += data + "\n"
            fcst += fcstString

        return fcst
 
    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        # All formatting done in _makeProduct
        return fcst

    def _postProcessProduct(self, fcst, argDict):
        fcst = fcst.replace("%IssueTime",
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
                magStr =  "+"+repr(diff)
            else:
                magStr =  repr(diff)
            dir = None
        # By Value
        else:
            mag_mph = int(mag_mph)
            if mag_mph < 10:
                magStr =  "0" +repr(mag_mph)
            else:
                magStr =  repr(mag_mph)
                
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
                dirStr = repr(int(dir)) + ","
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
                    return repr(int(trendValue))                
            if priorRangeName is not None:
                # Get data from prior day's data
                priorValue = self._getValue(element, priorRangeName, stats)
                if priorValue is not None:
                    priorValue = int(priorValue)
                    diff = value - priorValue
                    if diff >= 0:
                        return "+"+repr(diff)
                    else:
                        return repr(diff)
        return repr(value)

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

            

