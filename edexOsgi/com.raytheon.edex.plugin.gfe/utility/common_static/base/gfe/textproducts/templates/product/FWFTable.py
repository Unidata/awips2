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
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Feb 22, 2018  7044     randerso  Fix units in _dispersion method
# May 16, 2018  7292     randerso  Fix letter casing issues
#
##

##
# This is a base file that is not intended to be overridden.
##

#-------------------------------------------------------------------------
# Description: This produces a Fire Weather Forecast in tabular format.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# FWFTable, FWFTable_<site>_<MultiPil>_Definition, FWFTable_<site>_Overrides
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
#  productName      defines name of product e.g. "Fire Weather Table"
#  fullStationID    Full station identifier, 4 letter, such as "KSLC".
#  wmoID            WMO ID code for product header, such as "FOUS45"
#  pil              Product pil, such as "FWFBOS"
#  areaName (opt.)  Area name for product header, such as "Western New York"
#  wfoCityState     WFO location, such as "Buffalo NY"
#
# Optional Configuration Items
#  editAreaSuffix      default None. Allows for generating the body of the product for
#                      an edit area that is a subset (e.g. population areas) of the
#                      edit areas specified in the defaultEditAreas.  So given the edit area,
#                      "COZ035" and the editAreaSuffix is "_pt", then the edit area that
#                      will be sampled and reported for the body of the product will be
#                      "COZ035_pt".  If no such edit area exists, the system will simply
#                      use the original edit area.
#                      Note that Hazards will always be generated for the entire edit area.
#  mapNameForCombinations Name of the map background that is used for
#                         creating/editing the combinations file.  This must
#                         be defined or the GFE zone combiner
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
#  hazardSamplingThreshold  Defines the percentage coverage or number of
#                    grid points in a zone that must contain the hazard
#                    in order for it to be considered. Tuple (percent, points)
#
#  periodCombining     If 1, an attempt will be made to combine components
#                      or time periods into one.  Otherwise no period
#                      combining will be done.
#  columnJustification   # Left (l) or right (r) justification for columns
#  areaDictionary      Modify the AreaDictionary utility with UGC information
#                      about zones.
#  useRH               If 1, use RH grids instead of MaxRH, MinRH
#  summaryExtended
#  summaryArea      If summaryExtended == 1, then a summary extended forecast will
#                    be generated for the given summaryArea
#  individualExtended  If individualExtended == 1, an extended forecast will be
#                       generated for each individual area
#  extendedLabel    If extendedLabel== 1, a label will be included for each
#                    individual extended
#  mixingParmsDayAndNight   Set this to 1 if you want Mixing Height,
#                           Transport Wind and Vent Index reported in
#                           night periods.
#  mixHgtMethod             Can be "Max" or "Avg" for mixHgt analysis method
#  lightningPhrases         Set this to 1 if you want Lightning Activity
#                           reported with phrases like "1-8 strikes",
#                           "9-15 strikes", etc.
#  windAdjustmentFactor     Winds are reported from the Wind20ft grid
#                           if available. Otherwise, the Wind grid is used
#                           with the magnitude multiplied by this wind
#                           adjustment factor. Winds reported by RAWS sites
#                           are frequently lower than ASOS winds
#                           due to the fact that use a 10-min average.
#                           A common adjustment factor is 80% (0.80).  If
#                           you want no adjustment to the winds then set
#                           this variable to 1.00.
#                           NOTE: This value can optionally be specified as an
#                           nlValue dictionary.
#
#  tableLightWindThreshold  Threshold for a light wind string in the table
#  tableLightWindPhrase     Light wind string in the table
#  tableLightWindThreshold  Threshold for a light wind string in the table
#  minGustMph               Gusts will not be reported below this value
#  windGustDiffMph          Gusts will be reported only if the difference between
#                           gust and max wind exceeds this amount.
#
#  humidityRecovery_percentage  If max humidity is above this percentage,
#                               humidity recovery will be Excellent.
#  rhPhraseThreshold        The MinRH phrase will be included in the extended, IF
#                           the MinRH is less than this threshold.
#                           The default (-1) will not produce a MinRH phrase.
#  includeOutlooks          Set this to 1 to include long-range outlook
#                           placeholders at the end of the product.  These
#                           are appended by _postProcessProduct.
#  useHolidays              Set to 1 to use holidays in the time period labels
#
#  Weather-related flags
#       hoursSChcEnds        - specifies hours past the beginning of the
#                              first period of the product to stop including 'Slight
#                              Chance' or 'Isolated' weather types
#       popWxThreshold       -Affects the values in the following rows:
#
##          * CHANCE PRECIP
##          * Precip amount
##          * Precip duration
##          * Precip begin
##          * Precip end

##        We will put values in these rows according to the following assumptions:
##        --If there is no weather, then all the above fields will be blank, zero or None.
##        --If the PoP falls below a the popWxThreshold, value (default 1),
##          then all of the above fields are blank, zero, or None.
##        --If QPF is 0.00, then CHANCE PRECIP, Precip duration, Precip begin and
##          Precip end will represent no precip.
##        --Thus, if Wx is non-accumulating i.e. drizzle or snow flurries, the Precip Type
##          could be non-empty, but since QPF would be 0.0,
##          then no CHANCE PRECIP, Precip duration, Precip begin and Precip end will be reported.

#  areaDictionary    Modify the AreaDictionary utility with UGC
#                    information about zones.
#
#-------------------------------------------------------------------------
# Weather Elements Needed:
#   Sky, PoP, Wx, MaxT, MinT, T, Wind, Wind20ft, QPF, MaxRH, MinRH,
#   MixHgt, TransWind, VentRate, HrsOfSun, CWR, Haines, LAL
#   RH (optional -- can be used in place of MinRH, MaxRH. Set the "useRH" flag)
#   Ttrend (optional -- if not included, prior day's data is used)
#   RHtrend (optional -- if not included, prior day's data is used)
#-------------------------------------------------------------------------
# Edit Areas Needed: area1, area2
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file:
#-------------------------------------------------------------------------
# Component Products:
#   Extended
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
# To look up tasks and their status, see the Text Product User Guide
# Section on "Tkgnats: Task Reporting System".
#-------------------------------------------------------------------------
# Additional Information:
#
#  COMMON OVERRIDES
#     from FWF_Table
#       _rowList
#     from WxPhrases:
#       wxCoverageDescriptors
#       wxTypeDescriptors
#       wxAttributeDescriptors
#       wxIntensityDescriptors
#       wxCombinations
#       combine_T_RW
#
#-------------------------------------------------------------------------
# Example Output:
#  Refer to the NWS Directives for Fire Weather Services.
#######################################################################

import TextRules
import SampleAnalysis
import time
import TimeRange, AbsTime

class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = [
             (("Product Issuance", "productIssuance") , "Morning", "radio",
              ["Morning", "Afternoon"]),
         ]
    Definition = {
        "type": "smart",
        "displayName": "None",
        "database": "Official",
        # Defines output location of finished product.
        "outputFile": "{prddir}/TEXT/FWFTable_<MultiPil>.txt",
        "debug": 0,
        # Name of map background for creating Combinations
        "mapNameForCombinations": "FireWxZones_<site>",

        "lineLength": 66,
        ## Edit Areas:
        "showZoneCombiner" : 1, # 1 to cause zone combiner to display
        "defaultEditAreas" : "Combinations_FWFTable_<site>_<MultiPil>",

        # product identifiers
        "productName": "Fire Weather Planning Forecast",       # product name
        "fullStationID": "<fullStationID>",    # full station identifier (4letter)
        "wmoID": "<wmoID>",          # WMO ID
        "pil": "<pil>",            # Product pil
        "areaName": "<state>",             # Name of state, such as "Georgia" -- optional
        "wfoCityState": "<wfoCityState>", # Location of WFO - city state

        "textdbPil": "<textdbPil>",       # Product ID for storing to AWIPS text database.
        "awipsWANPil": "<awipsWANPil>",   # Product ID for transmitting to AWIPS WAN.
        "periodCombining" : 0,      # If 1, combine periods, if possible

        "fixedExpire": 1, #ensure VTEC actions don't affect segment time


        "hazardSamplingThreshold": (10, None),  #(%cov, #points)

        # Optional Configuration Items
        "columnJustification": "l", # Left (l) or right (r) justification for columns
        "useRH": 0,                 # Use RH grids instead of MaxRH, MinRH
        # Area Dictionary -- Descriptive information about zones
        "areaDictionary": "AreaDictionary",

        # If summaryExtended == 1, then a summary extended forecast will
        # be generated for the given summaryArea
        "summaryExtended": 1,
        "summaryArea":"FireWxAOR_<site>_<MultiPil>",
        # If individualExtended == 1, an extended forecast will be
        # generated for each individual area
        # If extendedLabel == 1, a label will be included for each
        #  individual extended
        "individualExtended": 0,
        "extendedLabel": 0,
        # Set the following variable to 1 if you want Mixing Height,
        # Transport Wind and Vent Index reported in night periods.
        "mixingParmsDayAndNight": 0,
        "mixHgtMethod": "Max",  # Can be Max of Avg
        # Set the following variable to 1 if you want Lightning Activity
        # reported with phrases like "1-8 strikes", "9-15 strikes", etc.
        "lightningPhrases": 0,
        # Winds are reported from the Wind20ft grid if available.
        # Otherwise, the Wind grid is used with the magnitude multiplied
        # by this wind adjustment factor.
        # Winds reported by RAWS sites are frequently lower than ASOS winds
        # due to the fact that use a 10-min average.  A common adjustment
        # factor is 80% (0.80).  If you want no adjust ment to the winds
        # then set this variable to 1.00
        "windAdjustmentFactor": 0.80,
        # Threshold for a light wind string in the table
        "tableLightWindThreshold" : 5,
        # Light wind string in the table
        "tableLightWindPhrase" : "Lgt/Var",
        # Use a range for the winds in the table 1=yes
        "tableWindRanges" : 0,
        # Gusts will not be reported below this value.
        "minGustMph": 17,
        # Gust - wind must exceed this threshold to be reported.
        "windGustDiffMph": 7,
        # If max humidity is above this percentage, humidity recovery
        # will be Excellent.
        "humidityRecovery_percentage": 50,
        # Set to MinRH value below which you would like a MinRH phrase in the Extended.
        # Default (-1) is no MinRH phrase.
        "rhPhraseThreshold":-1,
        # Set the following variable to 1 to include long-range outlook
        # placeholders at the end of the product.  These are appended by
        # _postProcessProduct
        "includeOutlooks": 0,

        # Weather-related flags
        "hoursSChcEnds": 36,
        "popWxThreshold": 1,

        "language": "english",

        # Set to 1 to use holidays in the time period labels
        "useHolidays": 0,

        }

    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)

    def _rowList(self):
        # The rowList is controls what parameters go into the table.
        # The list is a set of (label:method) pairs.
        # You may change the label if you like.
        # The order of the list determines the order of the rows in the table
        # so you may re-arrange the order if you like.
        return [
            # Directive requirements
            ("Cloud Cover", self._cloudCover_row),
            ("Precip Type", self._precipType_row),
            ("Chance Precip (%)", self._chancePrecip_row),
            ("Temp (24h trend)", self._tempWithTrend_row),
            ("RH % (24h trend)", self._rhWithTrend_row),
            # Use these if you do not want trends
            #("Temp", self._temp_row),
            #("RH %", self._rh_row),
            ("20ftWnd-Val/AM(mph)", self._windValleyMph_row),
            ("20ftWnd-Rdg/PM(mph)", self._windRidgeMph_row),
            # Directive optional products
            ("Precip Amount", self._precipAmount_row),
            ("Precip Duration", self._precipDuration_row),
            ("Precip Begin", self._precipBegin_row),
            ("Precip End", self._precipEnd_row),
            ("Mixing Hgt(m-AGL/MSL)", self._mixHgtM_row),
            ("Mixing Hgt(ft-AGL/MSL)", self._mixHgtFt_row),
            ("Transport Wnd (kts)", self._transWindKts_row),
            ("Transport Wnd (m/s)", self._transWindMS_row),
            ("Transport Wnd (mph)", self._transWindMph_row),
            ("Vent Rate (kt-ft)", self._ventRateKtFt_row),
            ("Vent Rate (m/s-m)", self._ventRate_row),
            ("Vent Rate (mph-ft)", self._ventRateMphFt_row),
            ("Dispersion", self._dispersion_row),
            ("DSI", self._dsi_row),
            ("Sunshine Hours", self._sunHours_row),
##            # If you need Ceiling, uncomment the Ceiling line in _getAnalysisList
##            #("Ceiling", self._ceiling_row),
            ("CWR", self._cwr_row),
            ("LAL", self._lal_row),
            ("Haines Index", self._haines_row),
            ("RH Recovery", self._rhRecovery_row),
##            # If you need 500m Mix Hgt Temp, uncomment the MixHgt500
##            # line in _getAnalysisList
##            #("Mix Hgt 500", self._mixHgt500_row),
            ("Stability Class", self._stability_row),
            ]

    ########################################################################
    # COMPONENT PRODUCTS
    ########################################################################

    def ExtendedNarrative(self):
        # check for period combining first
        if self._periodCombining:
            methodList = [self.combineComponentStats, self.assembleChildWords]
        else:
            methodList = [self.assembleChildWords]

        return {
          "type": "narrative",
          "displayName": None,
          "timePeriodMethod ": self.timeRangeLabel,
         ## Components
          "methodList": methodList,
          "narrativeDef": [
                       ("Extended", 24), ("Extended", 24), ("Extended", 24),
                       ("Extended", 24), ("Extended", 24)],
          }

    def Extended(self):
         return {
             "type": "component",
             "methodList": [self.orderPhrases, self.consolidateSubPhrases,
                            self.assemblePhrases, self.wordWrap],
             "analysisList": [
                        ("MinT", self.firstAvg),
                        ("MaxT", self.avg),
                        ("MinRH", self.avg, [0]),
                        ("T", self.hourlyTemp),
                        ("T", self.minMax),
                        ("PoP", self.stdDevMaxAvg, [12]),
                        ("PoP", self.binnedPercent, [12]),
                        ("Sky", self.median, [12]),
                        #("Wind", self.vectorMedian),
                        ("Wind", self.vectorMinMax, [6]),
                        ("Wind20ft", self.vectorMedian),
                        ("Wind20ft", self.vectorMinMax),
                        ("Wx", self.rankedWx, [12]),
                        ("Wx", self.rankedWx, [0]),
                       ],
             "phraseList":[
                    self.wind_summary,
                    self.sky_phrase,
                    self.skyPopWx_phrase,
                    self.weather_phrase,
                    self.reportTrends,
                    self.lows_phrase,
                    self.highs_phrase,
                    self.rh_phrase,
                    self.wind_withGusts_phrase,
                    ],
             }


    #############################
    # Overrides to take care of Wind in the Extended forecast
    # Use Wind20ft if available, else use adjusted Wind
    #
    def rounding_method_dict(self, tree, node):
        # Special rounding methods
        #
        return {
            "Wind": self._adjustWind,
            }
    def _adjustWind(self, value, mode, increment, maxFlag):
        # Rounding for marine winds
        factor = self.nlValue(self._windAdjustmentFactor, value)
        value = value * factor
        return self.round(value, mode, increment)

    def wind_summary_words(self, tree, node):
        # Uses vectorAvg, vectorMedian, vectorMinMax
        # See if there's data for Wind20ft
        elementName = self.chooseElement(tree, node, ["Wind20ft", "Wind"])
        words = self.vector_summary(tree, node, elementName)
        return self.setWords(node, words)

    def wind_setUp(self, tree, node, gustFlag=0, element="Wind", connectorMethod=None):
        # See if there's data for Wind20ft
        elementName = self.chooseElement(tree, node, ["Wind20ft", "Wind"])
        wind = self.ElementInfo(elementName, "List", self.VECTOR())
        elementInfoList = [wind]
        if gustFlag:
            windGust = self.ElementInfo(
                "WindGust", "Max", phraseDef=self.gust_phrase)
            elementInfoList.append(windGust)
            node.set("gustFlag", 1)
        if connectorMethod is None:
            connectorMethod = self.vectorConnector
        self.subPhraseSetUp(tree, node, elementInfoList, connectorMethod)
        return self.DONE()

    def nextDay24HourLabel_flag(self, tree, node):
        # Return 1 to have the TimeDescriptor module label 24 hour periods starting
        # after 1600 as the next day.
        # This is needed for the Fire Weather Extended product,
        # but not for other products when period combining.
        return 1

    ########################################################################
    # OVERRIDING THRESHOLDS AND VARIABLES
    ########################################################################

    # Uncomment any combinations you wish to collapse.
    # For example, if the first entry is uncommented,
    #  the phrase: scattered rain showers and widespread rain
    #  will collapse to: scattered rain showers.
    def wxCombinations(self):
        return [
            ("RW", "R"),
            ("SW", "S"),
            ## ("T","RW"),
            ]

    def temporalCoverage_hours_dict(self, parmHisto, timeRange, componentName):
        # This is the hours of overlap of a grid with the TIMERANGE
        #    in order to include it in the analysis.  In addition, if a grid
        #    is completely contained within the time range, it will be included.
        # Used by temporalCoverage_flag
        return {
                "MinRH": 5,
                "MaxRH": 5,
                "MinT":  4,
                "MaxT":  4,
                #"Haines":0,
                #"PoP" :  0,
                }

    def minMax_std_deviation(self, parmHisto, timeRange, componentName):
        # Replaces MINMAX_STD_DEVIATION
        # Number of standard deviations to compute around the weighted
        # average for a stdDev_MinMax
        return 1.4

    def element_outUnits_dict(self, tree, node):
        unitDict = TextRules.TextRules.element_outUnits_dict(self, tree, node)
        unitDict["Wind"] = "mph"
        unitDict["Wind20ft"] = "mph"
        unitDict["WindGust"] = "mph"
        return unitDict

    def generateForecast(self, argDict):
        # Generate formatted product for a list of edit areas

        # Get variables from varDict and Definition
        error = self._getVariables(argDict)
        if error is not None:
            return error

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
        fractionOne = 1.0 / float(len(self._areaList))
        percent = 50.0
        for editArea, areaLabel in self._areaList:
            self.progressMessage(fraction, percent, "Making Product for " + areaLabel)
            fcst = self._preProcessArea(fcst, editArea, areaLabel, argDict)
            fcst = self._makeProduct(fcst, editArea, areaLabel, argDict)
            fcst = self._postProcessArea(fcst, editArea, areaLabel, argDict)
            fraction = fractionOne

        fcst = self._postProcessProduct(fcst, argDict)
        return fcst

    def _getVariables(self, argDict):
        # Make argDict accessible
        self.__argDict = argDict

        # Get variables from Definition and VariableList
        self._definition = argDict["forecastDef"]
        for (key, value) in self._definition.items():
            setattr(self, f"_{key}", value)

        varDict = argDict["varDict"]
        for (key, value) in varDict.items():
            if type(key) is tuple:
                label, variable = key
                setattr(self, f"_{variable}", value)

        # Set up product-specific variables
        self._colWidth = 13
        if self._columnJustification == "l":
            self._rowLabelWidth = 22
        else:
            self._rowLabelWidth = 24
        self._fixedValueWidth = 13
        self._analysisList = self._getAnalysisList()

        return None

    def _determineTimeRanges(self, argDict):
        # Determine the time ranges which need to be samplePM
        # Set up self._timeRangeList, self._extendedRange
        # Create a list (or lists) of tuples:  (timeRange, timeRangeLabel)
        self._currentTime = argDict['creationTime']
        self._isDST = time.localtime(self._currentTime)[8]
        self._currentHour = time.gmtime(self._currentTime)[3]

        if self._productIssuance == "Morning":
            rangeNames = ["Today", "Tonight", "Tomorrow"]

        else:
            dayTime3 = self.createTimeRange(54, 66, "LT")
            rangeNames = ["Tonight", "Tomorrow", "Tomorrow Night", dayTime3]

        self._timeRangeList = self.getTimeRangeList(
            argDict, rangeNames, self._getLabel)

        # Determine time range to BEGIN the extended forecast
        length = len(self._timeRangeList)
        lastPeriod = self._timeRangeList[length - 1][0]

        self._extendedRange = TimeRange.TimeRange(
            lastPeriod.endTime(), lastPeriod.endTime() + 3600)

        # Determine prior time range
        firstPeriod, label = self._timeRangeList[0]
        self._priorTimeRange = TimeRange.TimeRange(
            firstPeriod.startTime() - 24 * 3600, firstPeriod.startTime())

        # Get entire timeRange of table for Headlines
        # Tom says: I'm very unsure about removing this line...........
        self._timeRange = TimeRange.TimeRange(
            firstPeriod.startTime(), lastPeriod.endTime())
        argDict["productTimeRange"] = self._timeRange

        # Determine issue time
        self._issueTime = AbsTime.current()

        # Sets up the expiration time
        self._expireTime, self._ddhhmmTimeExpire = \
          self.getExpireTimeFromLToffset(self._currentTime,
          self.expireOffset(), "")

        # Calculate current times
        self._ddhhmmTime = time.strftime(
            "%d%H%M", time.gmtime(self._currentTime))
        self._timeLabel = self.getCurrentTime(
           argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1)

        return

    def _sampleData(self, argDict):
        # Get a sampler (or set of samplers)
        samplerList = []
        samplerList.append((self._getAnalysisList(), self._timeRangeList))
        samplerList.append((self._priorAnalysisList(),
          [(self._priorTimeRange, "")]))

        sampleInfo = []
        for analysisList, periods in samplerList:
            sampleInfo.append((analysisList, periods, self._areaList))
        self._sampler = self.getSampler(argDict, sampleInfo)
        # Get sampler for first 12 hours of extended period
        extTimeRange = self._extendedRange
        self._extTR = TimeRange.TimeRange(extTimeRange.startTime(),
                                     extTimeRange.startTime() + 43200)
        extTrTuple = [(self._extTR, "Extended")]
        self._extAnalList = self._getExtAnalysisList()
        extSampleInfo = (self._extAnalList, extTrTuple, self._areaList)
        self._extSampler = self.getSampler(argDict, extSampleInfo)
        return self._sampler

    def _preProcessProduct(self, fcst, argDict):
        # Add product heading to fcst string
        if self._areaName != "":
             productName = self._productName.strip() + " for " + \
                           self._areaName.strip()
        else:
             productName = self._productName.strip()

        issuedByString = self.getIssuedByString()
        productName = self.checkTestMode(argDict, productName)

        s = self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n"
        fcst = fcst + s.upper()

        s = productName + "\n" + \
               "National Weather Service " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n\n"
        fcst = fcst + s

        # Put in a place holder for the headlines to be substituted in
        # "postProcessProduct"
        fcst = fcst + "<HEADLINES>"
        self._prodHeadlines = []

        fcst = fcst + ".DISCUSSION..." + "\n\n\n\n\n"
        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        areaHeader = self.makeAreaHeader(
            argDict, areaLabel, self._issueTime, self._expireTime,
            self._areaDictionary, self._defaultEditAreas)
        fcst = fcst + areaHeader

        # get the hazards text
        self._hazards = argDict['hazards']
        self._combinations = argDict["combinations"]

        headlines = self.generateProduct("Hazards", argDict, area=editArea,
                                         areaLabel=areaLabel,
                                         timeRange=self._timeRange)
        self._addHeadlines(headlines)
        fcst = fcst + headlines

        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        # Return a text string representing the product
        # for the given edit area
        # Column headings
        fcst += ' ' * self._rowLabelWidth
        for period, label in self._timeRangeList:
            if label == "Rest of Today":
                label = "Today"
            if label == "Rest of Tonight":
                label = "Tonight"
            if self._columnJustification == "l":
                fcst += label.ljust(self._colWidth)
            else:
                fcst += label.rjust(self._colWidth)
        fcst = fcst + "\n\n"

        # Get the statistics for this edit area and all time ranges
        statList = self.getStatList(
            self._sampler, self._analysisList, self._timeRangeList, editArea)

        # Get the prior statistics for this edit area and all time ranges
        priorStatDict = self.getStatDict(
          self._sampler, self._priorAnalysisList(), self._priorTimeRange,
          editArea)

        # Get a statDict for the first 12 hours of the extended
        self._extStatDict = self.getStatDict(self._extSampler,
                                             self._extAnalList, self._extTR,
                                             editArea)

        # Format each row of table
        for label, method in self._rowList():
            fcst = method(fcst, label, statList, priorStatDict)
        fcst = fcst + "\n"
        fcst = fcst + "Remarks...None.\n\n"

        # Produce Individual Extended Forecast
        if self._individualExtended == 1:
            if self._extendedLabel == 1:
                fcst = fcst + ".FORECAST FOR DAYS 3 THROUGH 7...\n\n"
            extended = self.generateProduct("ExtendedNarrative", argDict,
                area=editArea, timeRange=self._extendedRange)
            fcst = fcst + extended
        return fcst

    def _cloudCover_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._sky, None, self._rowLabelWidth, self._fixedValueWidth,
            self._columnJustification)
        return fcst

    def _precipType_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self.wxVal, ["Wx"], self._rowLabelWidth, self._fixedValueWidth,
            self._columnJustification)
        return fcst

    def _chancePrecip_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._popVal, None, self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _tempWithTrend_row(self, fcst, label, statList, priorStatDict):
        dayElement = "MaxT"
        nightElement = "MinT"
        dayMinMax = "Max"
        nightMinMax = "Min"
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self.dayOrNightVal, [dayElement, nightElement, dayMinMax,
            nightMinMax, "Ttrend", priorStatDict, statList,
            self._timeRangeList], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _temp_row(self, fcst, label, statList, priorStatDict):
        dayElement = "MaxT"
        nightElement = "MinT"
        dayMinMax = "Max"
        nightMinMax = "Min"
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self.dayOrNightVal, [dayElement, nightElement, dayMinMax,
            nightMinMax, None, priorStatDict, statList,
            self._timeRangeList], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _rhWithTrend_row(self, fcst, label, statList, priorStatDict):
        if self._useRH == 1:
            dayElement = "RH"
            nightElement = "RH"
        else:
            dayElement = "MinRH"
            nightElement = "MaxRH"
        dayMinMax = "Min"
        nightMinMax = "Max"
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList, self.dayOrNightVal,
            [dayElement, nightElement, dayMinMax, nightMinMax, "RHtrend",
            priorStatDict, statList, self._timeRangeList],
            self._rowLabelWidth, self._fixedValueWidth, self._columnJustification)
        return fcst

    def _rh_row(self, fcst, label, statList, priorStatDict):
        if self._useRH == 1:
            dayElement = "RH"
            nightElement = "RH"
        else:
            dayElement = "MinRH"
            nightElement = "MaxRH"
        dayMinMax = "Min"
        nightMinMax = "Max"
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList, self.dayOrNightVal,
            [dayElement, nightElement, dayMinMax, nightMinMax, None,
             priorStatDict, statList, self._timeRangeList],
            self._rowLabelWidth, self._fixedValueWidth, self._columnJustification)
        return fcst

    def _windValleyMph_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._wind, ["AM"], self._rowLabelWidth, self._fixedValueWidth,
            self._columnJustification)
        return fcst

    def _windRidgeMph_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._wind, ["PM"], self._rowLabelWidth, self._fixedValueWidth,
            self._columnJustification)
        return fcst

    def _precipAmount_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._qpfVal, None, self._rowLabelWidth, self._fixedValueWidth,
            self._columnJustification)
        return fcst

    def _precipDuration_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._duration, ["Wx"], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _precipBegin_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._begin, ["Wx__dominantWx_0", self._timeRangeList, statList],
            self._rowLabelWidth, self._fixedValueWidth,
            self._columnJustification)
        return fcst

    def _precipEnd_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._end, ["Wx__dominantWx_0", self._timeRangeList, statList],
            self._rowLabelWidth, self._fixedValueWidth,
            self._columnJustification)
        return fcst

    def _mixHgtFt_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._mixHgt, ["ft"], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _mixHgtM_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._mixHgt, ["m"], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _transWindKts_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._transWind, ["kts"], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _transWindMS_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._transWind, ["ms"], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _transWindMph_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._transWind, ["mph"], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _ventRate_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._ventRate, ["m/s-m"], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _ventRateKtFt_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._ventRate, ["kt-ft"], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _ventRateMphFt_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._ventRate, ["mph-ft"], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _dispersion_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._dispersion, None, self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _dsi_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._dsi, None, self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _sunHours_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._sunHours, ["HrsOfSun"], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _ceiling_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._cigHeight, None, self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _cwr_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._cwr, None, self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _lal_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._getLightning, ["LAL"], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _haines_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._haines, ["Haines"], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _rhRecovery_row(self, fcst, label, statList, priorStatDict):
        if self._useRH == 1:
            element = "RH"
            priorElement = "RH"
        else:
            element = "MaxRH"
            priorElement = "MinRH"
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._rhRecovery, [element, priorElement, priorStatDict], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _mixHgt500_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._mixHgt500, None, self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _stability_row(self, fcst, label, statList, priorStatDict):
        fcst = fcst + self.makeRow(
            label, self._colWidth, self._timeRangeList, statList,
            self._stability, ["Stability"], self._rowLabelWidth,
            self._fixedValueWidth, self._columnJustification)
        return fcst

    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        fcst = fcst + "\n$$\n\n"
        return fcst

    def _postProcessProduct(self, fcst, argDict):
        if self._summaryExtended == 1:
            fcst = fcst + "\n.FORECAST FOR DAYS 3 THROUGH 7...\n\n"
            extended = self.generateProduct("ExtendedNarrative",
                argDict, area=self._summaryArea,
                timeRange=self._extendedRange)
            fcst = fcst + extended
        if self._includeOutlooks:
            fcst = fcst + "\n.OUTLOOK 6 TO 10 DAYS... \n\n.OUTLOOK 8 TO 14 DAYS...\n\n\n"
            fcst = fcst + "\n.OUTLOOK\n\n"

        # Make summary headline string and substitute for "<HEADLINE>" placeholder
        headlineStr = ""
        for h in self._prodHeadlines:
            headlineStr = headlineStr + "..." + h + "...\n"
        if len(self._prodHeadlines):
            headlineStr = headlineStr + "\n"
        fcst = fcst.replace("<HEADLINES>", headlineStr)

        # Uncomment to include = sign
        #fcst = fcst + "=\n"
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst

    # provides expiration time offset from today's midnight based on issuance.
    def expireOffset(self):
        if self._productIssuance == "Morning":
            # 6pm today
            return 16
        else:
            # 6am tomorrow
            return 24 + 4

     ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################

    def _getAnalysisList(self):
        return[
          ("Sky", self.avg),
          ("PoP", self.stdDevMaxAvg),
          ("Wx", self.dominantWx, [12]),
          ("Wx", self.dominantWx, [0]),
          ("MaxT", self.stdDevMinMax),
          ("MinT", self.stdDevMinMax),
          ("T", self.minMax),
          ("Wind", self.vectorAvg, [6]),
          ("Wind20ft", self.vectorAvg, [6]),
          ("WindGust", self.stdDevMax, [6]),
          ("QPF", self.minMaxSum),
          ("MaxRH", self.stdDevMinMax),
          ("MinRH", self.stdDevMinMax),
          ("RH", self.minMax),
          ("MixHgt", self.minMax, [0]),
          ("MixHgt", self.avg, [0]),
          ("TransWind", self.vectorAvg, [0]),
          ("VentRate", self.minMax, [0]), # aka "Dispersion" prior to RPP20
          ("DSI", self.avg),
          ("HrsOfSun", self.avg),
          # Uncomment the next line if you're carrying Cig Height
          #("Ceiling", self.minMax),
          ("CWR", self.stdDevMaxAvg),
          ("Haines", self.minMaxAvg),
          ("LAL", self.maximum),
          ("Ttrend", self.minMax),
          ("RHtrend", self.minMax),
          ("Stability", self.avg),
          # Uncomment the next line if you're carrying 500m mix height temp
          #("MixHgt500", self.avg),
          ]

    def _priorAnalysisList(self):
        return[
          ("MaxT", self.avg),
          ("MinT", self.avg),
          ("MaxRH", self.avg),
          ("MinRH", self.avg),
          ("RH", self.minMax),
          ("T", self.minMax),
          ]

    def _getExtAnalysisList(self):
        # Need to get the same items in the table's regular analysis
        # list for "Wx", "PoP", "QPF" in the extended analysis list
        # to work with _checkPrecip
        analList = []
        for item in self._getAnalysisList():
            if item[0] in ["Wx", "PoP", "QPF"]:
                analList.append(item)
        #print analList
        return analList

    def _getLabel(self, timeRange):
        "Produce a column label given the timeRange"
        return self.getWeekday(
            timeRange, holidays=self._useHolidays, shiftToLocal=1, labelType="CapsAbbreviated",
            today=1, tomorrow=0)

    def _sky(self, statDict, timeRange, argList):
        # Return a sky value
        sky = self.getStats(statDict, "Sky")
        if sky is None:
            value = ""
        elif  sky <= 6:
            value = "Clear"
        elif sky <= 31:
            value = "Mclear"
        elif sky <= 69:
            value = "Pcldy"
        elif sky <= 94:
            value = "Mcldy"
        else:
            value = "Cloudy"
        return value

    def _wind(self, statDict, timeRange, argList):
        # Use Wind20ft if available,
        #   otherwise adjust Wind
        # Stats vectorAvg

        # Get Wind stats
        adjust = 0
        windStats = self.getStats(statDict, "Wind20ft")
        if windStats is None:
            windStats = self.getStats(statDict, "Wind")
            if windStats is None:
                return ""
            adjust = 1

        (mag1, dir1), subRange = windStats[0]
        if len(windStats) > 1:
            (mag2, dir2), subRange = windStats[1]
        else:
            (mag2, dir2), subRange = windStats[0]
        # Convert to mph
        mag1 = self.ktToMph(mag1)
        mag2 = self.ktToMph(mag2)
        # Adjust if using Wind
        if adjust:
            mag1 = mag1 * self._windAdjustmentFactor
            mag2 = mag2 * self._windAdjustmentFactor

        # Get Gust Stats
        gustStats = self.getStats(statDict, "WindGust")
        if gustStats is None:
            gust1 = 0
            gust2 = 0
        else:
            gust1, subRange = gustStats[0]
            if len(gustStats) > 1:
                gust2, subRange = gustStats[1]
            else:
                gust2 = gust1
        gust1 = self.ktToMph(gust1) * self._windAdjustmentFactor
        gust2 = self.ktToMph(gust2) * self._windAdjustmentFactor

        # This method is called twice for each time period,
        #   once for AM winds and once for PM winds
        # For the AM winds:
        #     If the time period is daytime, report the morning winds
        # For the PM winds:
        #     If the time period is daytime, report the afternoon winds
        #     Otherwise, report the evening winds
        amPm = argList[0]
        day = self.getPeriod(timeRange, 1)
        if amPm == "AM":
            if day == 1:
                windMag = mag1
                windDir = dir1
                windGustVal = gust1
            else:
                windMag = None
                windGustVal = 0
        else:
            if day == 1:
                windMag = mag2
                windDir = dir2
                windGustVal = gust2
            else:
                windMag = mag1
                windDir = dir1
                windGustVal = gust1

        # Determine wind string
        windString = ""
        if windMag is not None:
            if windMag < self._tableLightWindThreshold:
                windString = self._tableLightWindPhrase
            elif self._tableWindRanges:
                windString = self._getVectorRange(((windMag - 2, windMag + 2), windDir))
            else:
                windString = self.getVectorVal((windMag, windDir))

         # Determine gust string
        gustString = ''
        if windString != "" and windString != self._tableLightWindPhrase:
            if windGustVal >= self._minGustMph and (windGustVal - windMag) >= self._windGustDiffMph:
                    gustString = ' G' + repr(int(windGustVal))
        return windString + gustString

    def _qpfVal(self, statDict, timeRange, argList):
        qpf = self.getStats(statDict, "QPF")
        if qpf is None:
            return ""
        precipFlag, wx, qpf, pop = \
                    self._checkPrecip(statDict, timeRange, argList)
        if precipFlag:
            return  self.fformat(qpf, .01)
        else:
            return "0.00"

    def _duration(self, statDict, timeRange, argList):
        precipFlag, wx, qpf, pop = \
                    self._checkPrecip(statDict, timeRange, argList)
        if wx.upper() == "NONE":
            return ""
        if not precipFlag:
            return ""
        statsByRange = self.getStats(statDict, "Wx__dominantWx_0")
        if statsByRange is None:
            return ""
        # Found in TableBuilder:
        return self.wxDuration(statsByRange, timeRange)

    def _begin(self, statDict, timeRange, argList):
        # Check if this period should have precip based on Wx, QPF, PoP
        precipFlag, wx, qpf, pop = \
                    self._checkPrecip(statDict, timeRange, argList)
        #print "_begin:",timeRange,precipFlag, wx, qpf, pop
        if not precipFlag:
            return ""

        durationRange = self._getTR(statDict, timeRange, argList)
        if durationRange is None:
            return ""
        durStart = durationRange.startTime()
        if durStart < timeRange.startTime():
            return "Continuing"
        value = self.localHourLabel(durStart).strip()
        return value

    def _end(self, statDict, timeRange, argList):
        # Check if this period should have precip based on Wx, QPF, PoP
        precipFlag, wx, qpf, pop = \
                    self._checkPrecip(statDict, timeRange, argList)
        #print "_end:",timeRange,precipFlag, wx, qpf, pop
        if not precipFlag:
            return ""

        durationRange = self._getTR(statDict, timeRange, argList, ending=1)
        if durationRange is None:
            return ""
        durEnd = durationRange.endTime()
        if durEnd > timeRange.endTime():
            return "Continuing"
        value = self.localHourLabel(durEnd).strip()
        return value

    def _getTR(self, statDict, timeRange, argList, ending=0):
        # Get a beginning or ending timeRange for weather occurring.
        #print "_getTR:",timeRange,ending

        # Parse the argList
        element = argList[0]
        trList = argList[1]
        statList = argList[2]

        # Get the length of the statList, so we know how many periods we have
        statLength = len(statList)

        # Get index for current time noWxTimeRange
        currentIndex = self._getIndex(timeRange, trList)
        #print "_getTR: currentIndex = ", currentIndex, "tr = ", timeRange, "Ending = ", ending
        # Use the index to access the previous and next time noWxTimeRange in the
        # statList.
        if currentIndex is None:
            #print "_getTR: no currentIndex returning none"
            return None
        nextIndex = currentIndex + 1

        # Set prevIndex to one less than the current, unless this is the first
        # period, then just use the current index.
        if currentIndex > 0:
            prevIndex = currentIndex - 1
        else:
            prevIndex = currentIndex
        prevTR = trList[prevIndex]
        # If we're on the last period of the table, we need to access the stats
        # from the extended portion.
        if currentIndex < statLength - 1:
            nextStatDict = statList[nextIndex]
            nextTR = trList[nextIndex]
        else:
            nextStatDict = self._extStatDict
            nextTR = self._extTR

        if prevIndex != currentIndex:
            prevStatDict = statList[prevIndex]
            prevStats = self.getStats(prevStatDict, element)
        nextStats = self.getStats(nextStatDict, element)
        eStats = self.getStats(statDict, element)
        #print "_getTR: element, estats=",element,eStats
        if eStats is None:
            #print "_getTR: no eStats returning none"
            return None

        # if looking for ending time, reverse so last time range is first
        if ending == 1:
            eStats.reverse()

        # "noWxTimeRange" will be first time range found where there is "<NoWx>"
        noWxTimeRange = None

        for values, tr in eStats:
            #print "_getTR: values, tr=",values, tr
            for subKey in values:
                #print "_getTR: subKey=",subKey
                if self.precip_related(subKey):
                    noWxTimeRange = tr
                    break
            if noWxTimeRange is not None:
                break

        if currentIndex > 0 and not ending:
            # If the precip startTime found in the previous for-loop equals the
            # startTime for the current timeRange, then we need to look at the
            # previous timeRange to see if precip is "Continuing".
            if noWxTimeRange is not None and noWxTimeRange.startTime() == timeRange.startTime():
                #PJ Make sure previous period has Wx/QPF/PoP
                precipFlag, wx, qpf, pop = \
                    self._checkPrecip(prevStatDict, prevTR, argList)
                #print "getTR beg _checkPrecip:",prevTR,precipFlag, wx, qpf, pop
                if precipFlag:
                    prevRange = noWxTimeRange
                    prevLength = len(prevStats)
                    lastIndex = prevLength - 1
                    val, tr = prevStats[lastIndex]
                    for subKey in val:
                        if self.precip_related(subKey):
                            prevRange = tr
                            break
                    noWxTimeRange = prevRange

        if ending == 1:
            # If noWxTimeRange has not been set OR the precip endTime equals the
            # endTime for the current timeRange, then we need to look at the
            # next timeRange to determine if precip is "Continuing".
            if noWxTimeRange is not None and noWxTimeRange.endTime() == timeRange.endTime():
                #PJ Make sure next period has Wx/QPF/PoP
                precipFlag, wx, qpf, pop = \
                    self._checkPrecip(nextStatDict, nextTR, argList)
                #print "getTR end _checkPrecip:",nextTR,precipFlag, wx, qpf, pop
                if precipFlag:
                    nextRange = None
                    val, tr = nextStats[0]
                    if len(val) == 0:
                        return noWxTimeRange
                    for subKey in val:
                        if self.precip_related(subKey):
                            nextRange = tr
                            break

                    if nextRange is None:
                        return noWxTimeRange

                    noWxTimeRange = nextRange
        #print "_getTR: returning:",noWxTimeRange
        return noWxTimeRange

    def _getIndex(self, timeRange, trList):
        index = 0
        for tr, label in trList:
            if timeRange == tr:
                return index
            index = index + 1
        return

    def _mixHgt(self, statDict, timeRange, argList):
        # Report MixHgt
        mixHgt = self._mixHgtValue(statDict, timeRange, argList)
        if mixHgt is None:
            return ""
        else:
            mixHgt = self.round(mixHgt, "Nearest", 1)
            return self.getScalarVal(mixHgt)

    def _mixHgtValue(self, statDict, timeRange, argList):
        # Report MixHgt
        units = argList[0]
        day = self.getPeriod(timeRange, 1)
        method = self._mixHgtMethod
        if day:
            minMax = "Max"
        else:
            minMax = "Min"
        if self._mixingParmsDayAndNight:
            day = 1
        if day == 1:
            mixHgt = self._getMixHgt(statDict, minMax, method)
            if mixHgt is not None:
                if units == "m":
                    mixHgt = mixHgt / 3.2808
            return mixHgt
        else:
            return None

    def _getMixHgt(self, statDict, minMax, method):
        # Returns minimum and maximum MixHgt with associated time ranges
        avgMixStats = self.getStats(statDict, "MixHgt__avg")
        minMaxMixStats = self.getStats(statDict, "MixHgt")
        if avgMixStats is None or minMaxMixStats is None:
            return None
        #print "mixStats = ", mixStats, "\n\n"
        mixMax = -1
        mixAvg = -1
        mixMin = 200000
        # Find the highest average mix height and subRange
        method = self._mixHgtMethod
        for mixStats, subRange in avgMixStats:
            avg = mixStats
            if avg > mixAvg:
                mixAvg = avg
                avgRange = subRange
        # Find the absolute minimum and maximum mixHgt and subRanges
        for mixStats, subRange in minMaxMixStats:
            minimum, maximum = self.getValue(mixStats, "MinMax")
            if maximum > mixMax:
                mixMax = maximum
                maxRange = subRange
            if minimum < mixMin:
                mixMin = minimum
                minRange = subRange
        if minMax == "Min":
            return mixMin
        elif minMax == "Max":
            if method == "Avg":
                return mixAvg
            else:
                return mixMax
        elif minMax == "MinMax":
            if method == "Avg":
                return mixMin, mixAvg
            else:
                return mixMin, mixMax
        else:
            return (mixMin, minRange), (mixMax, maxRange), (mixAvg, avgRange)

    def _getCorrelatedStats(self, statDict, element, minMax, dataType=None):
        # Return the value (min or max) for the given element
        # that correlates with the min or max Mixing Height
        statsByRange = self.getStats(statDict, element)
        method = self._mixHgtMethod
        if statsByRange is None:
            return None
        # Find the subRange that has the min or max mixing height'
        mixHgt = self._getMixHgt(statDict, "All", method)
        if mixHgt is None:
            return None
        mixMin, mixMax, mixAvg = mixHgt
        if minMax == "Min":
            mix, subRange = mixMin
        else:
            if method == "Avg":
                mix, subRange = mixAvg
            else:
                mix, subRange = mixMax
        # Find the first stats that overlap the mixing height subRange
        for stats, statRange in statsByRange:
            if statRange.overlaps(subRange):
                break
        stats = self.getValue(stats, minMax, dataType)
        return stats


    def _transWind(self, statDict, timeRange, argList):
        # Return the transport wind as a string
        day = self.getPeriod(timeRange, 1)
        if self._mixingParmsDayAndNight:
            day = 1
        if day == 1:
            transWind = self._transWindValue(statDict, timeRange, argList)
            if transWind is None:
                return "N/A"
            mag, direction = transWind
            mag = self.round(mag, "Nearest", 1)
            return self.getVectorVal((mag, direction))
        else:
            return ""

    def _transWindValue(self, statDict, timeRange, argList):
        # Return the transport wind as a tuple of magnitude and direction

        units = argList[0]
        day = self.getPeriod(timeRange, 1)
        if day:
            minMax = "Max"
        else:
            minMax = "Min"
        if self._mixingParmsDayAndNight:
            day = 1
        if day == 1:
            transWind = self._getCorrelatedStats(
                statDict, "TransWind", minMax, self.VECTOR())
            if transWind is not None:
                mag, direction = transWind
                if units == "ms":
                    mag = mag / 1.94
                elif units == "mph":
                    mag = mag * 1.15
                return mag, direction
        else:
            return None

    def _getVectorRange(self, value):
        # Return  text representation of vector value
        # Value is a tuple of magnitude and direction
        # E.g. returned value:  SW 19

        # Check for no data
        if value == () or value is None:
            return "    "
        else:
            mag1, mag2 = value[0]
            direction = value[1]
            mag1 = repr(int(mag1))
            mag2 = repr(int(mag2))
            magStr = mag1 + "-" + mag2
            magStrLen = len(magStr)
            rjustLen = magStrLen + 1
            magStr = magStr.rjust(rjustLen)
            if type(direction) is not str:
                direction = self.dirToText(direction)
            dirStr = direction.rjust(2)
            return dirStr + magStr

    def wxVal(self, stats, timeRange, argList):
        # Return a weather text string value
        # The desired element name must be the first element of argList
        # e.g., SNOW
        element = argList[0]
        wxStats = self.getStats(stats, element)
        if wxStats is None:
            return ""
        value = ""
        #print "\nIn wxVal"
        for wxValue, timeRange in wxStats:
            #print wxValue, timeRange
            val = self.short_weather_phrase(element, wxValue)
            val = val.replace("|", " ")
            val = val.replace("THUNDER STORMS", "THUNDERSTORMS")
            val = val.replace("THUNDERSTORMS", "Tstms")
            val = val.replace("FREEZING RAIN", "Frz Rain")
            val = val.replace("FREEZING DRIZZLE", "Frz Drzl")
            val = val.replace("RAIN SHOWERS", "Showers")
            val = val.replace("thunder storms", "thunderstorms")
            val = val.replace("thunderstorms", "Tstms")
            val = val.replace("freezing rain", "Frz Rain")
            val = val.replace("freezing drizzle", "Frz Drzl ")
            val = val.replace("rain showers ", "Showers")
            if self.wxOrder(val) < self.wxOrder(value) or value == "":
                value = val
            #print "value", value
        if value == "":
            value = "None"
        #print "Returning ", value
        return value

    def short_weather_phrase(self, element, stats):
        "Develop short phrase for weather in a table"
        # Weather Stats:
        #    SubKey List: list of all subkeys mentioned in time period
        if stats is None:
            return ""
        subkeyList = self.makeSubkeyList(stats)
        #print "subkeyList = ", subkeyList, "\n"
        if len(subkeyList) == 0:
            return ""
        value = ""
        #print "In short-weather-phrase"
        isFreezing = 0
        isLiquid = 0
        isSnow = 0
        isSleet = 0
        for subKey in subkeyList:
            if not self.precip_related(subKey):
                continue
            val, cov = self.weather_value(None, None, subKey, typeOnly=1)
            # print "subKey = ", subKey, "Val = ", val, "\n"
            if "freezing" in val:
                isFreezing = 1
            if val == "rain":
                isLiquid = 1
            if val == "snow":
                isSnow = 1
            if val == "sleet":
                isSleet = 1
            if self.wxOrder(val) < self.wxOrder(value) or value == "":
                value = val
            # print "value", value
        if isLiquid and isSnow:
            value = "snow/rain"
        if isLiquid and isSleet:
            value = "sleet/rain"
        if isFreezing and isSnow:
            value = "snow/fz ra"
        if isLiquid and isSleet and isSnow:
            value = "snow/rain"
        value = value.replace(" ", "|")
        value = value.replace("thunderstorm", "thunder|storm")
        # print "returning", value
        return value

    def precip_related(self, subkey):
        # These are weather types that are precip versus non-precip
        # and could be separated into different phrases from the non-precip weather types.
        if subkey.wxType() in  ["ZR", "R", "RW", "S", "SW", "T", "ZL", "L", "IP", "SA"]:
            return 1
        else:
            return 0

    def wxOrder(self, value):
        value = value.lower()
        if value == "thunderstorms" or value == "tstms":
            return 0
        elif value == "freezing rain" or value == "frz rain":
            return 1
        elif value == "freezing drizzle" or value == "frz drzl":
            return 2
        elif value == "sleet":
            return 3
        elif value == "snow":
            return 4
        elif value == "rain showers" or value == "showers":
            return 5
        elif value == "rain":
            return 6
        else:
            return 7

    def _stability(self, statDict, timeRange, argList):
        stab = self.getStats(statDict, "Stability")
        if stab is None:
            return ""
        stab = self.round(stab, "Nearest", 1)
        if stab == 1:
            value = "A"
        if stab == 2:
            value = "B"
        if stab == 3:
            value = "C"
        if stab == 4:
            value = "D"
        if stab == 5:
            value = "E"
        if stab == 6:
            value = "F"
        return value

    def _getLightning(self, statDict, timeRange, argList):
        # Return a LAL value
        lal = self.getStats(statDict, "LAL")
        if self._lightningPhrases:
            if lal is None:
                value = "Missing LAL"
            else:
                value = "No Tstms"
                if lal > 1:
                    value = "1-8 Strikes"
                if lal > 2:
                    value = "9-15 Strikes"
                if lal > 3:
                    value = "16-25 Strikes"
                if lal > 4:
                    value = ">25 Strikes"
                if lal > 5:
                    value = "Dry Lightning"
        else:
            if lal is None:
                value = "N/A"
            else:
                value = self.getScalarVal(lal)
        return value

    def _haines(self, statDict, timeRange, argList):
        haines = self.getStats(statDict, "Haines")
        if haines is None:
            return ""
        minimum, maximum, avg = haines
        return repr(int(avg)).strip()

    def _rhRecovery(self, statDict, timeRange, argList):
        element = argList[0]
        priorElement = argList[1]
        priorStatDict = argList[2]
        rh = self.getStats(statDict, element)
        if rh is None:
            return ""
        rh = self.getValue(rh, "Max")
        if rh > self._humidityRecovery_percentage:
            return "Excellent"
        priorRH = self.getStats(priorStatDict, priorElement)
        if priorRH is None:
            return ""
        priorRH = self.getValue(priorRH, "Max")
        diff = rh - priorRH
        words = ""
        for threshold, label in self._humidityRecovery_valueList():
            if diff <= threshold:
                words = label
                break
        return words

    def _humidityRecovery_valueList(self):
        "Used to convert percent difference to corresponding category"
        return [
            (25, "Poor"),
            (55, "Moderate"),
            (70, "Good"),
            (100, "Excellent"),
            ]

    def _calcVentRate(self, statDict, timeRange, argList):
        units = argList[0]
        if units == "kt-ft":
          mixHgtArgList = ["ft"]
          transWindArgList = ["kts"]
        elif units == "mph-ft":
          mixHgtArgList = ["ft"]
          transWindArgList = ["mph"]
        elif units == "m/s-m":
          mixHgtArgList = ["m"]
          transWindArgList = ["ms"]
        else:
            # Unknown configuration
            return None

        mixHgt = self._mixHgtValue(statDict, timeRange, mixHgtArgList)
        if mixHgt is None:
            return None
        transWind = self._transWindValue(statDict, timeRange, transWindArgList)
        if transWind is None:
            return None
        mag, direction = transWind
        return  mixHgt * mag

    def _ventRate(self, statDict, timeRange, argList):
        day = self.getPeriod(timeRange, 1)
        if day:
            minMax = "Max"
        else:
            minMax = "Min"
        if self._mixingParmsDayAndNight:
            day = 1
        if day == 1:
            vr = self._getCorrelatedStats(statDict, "VentRate", minMax)
            if vr is None:
                vr = self._calcVentRate(statDict, timeRange, argList)
                if vr is None:
                    return ""
            else:
                units = argList[0]
                if units == "m/s-m":
                    vr = vr / 1.94 / 3.2808
                elif units == "mph-ft":
                    vr = vr * 1.15
            return  repr(int(self.round(vr, "Nearest", 1)))
        else:
            return ""

    def _dispersion(self, statDict, timeRange, argList):
        day = self.getPeriod(timeRange, 1)
        if day:
            minMax = "Max"
        else:
            minMax = "Min"
        if self._mixingParmsDayAndNight:
            day = 1
        if day == 1:
            vr = self._getCorrelatedStats(statDict, "VentRate", minMax)
            if vr is None:
                argList = ["m/s-m"]
                vr = self._calcVentRate(statDict, timeRange, argList)
            if vr is None:
                return ""
            elif vr < 2000:
                return "1"
            elif vr < 4000:
                return "2"
            elif vr < 8000:
                return "3"
            elif vr < 16000:
                return "4"
            else:
                return "5"
        else:
            return ""

    def _dsi(self, statDict, timeRange, argList):
        # Return dsi
        dsi = self.getStats(statDict, "DSI")
        if dsi is None:
            return "N/A"
        else:
            return self.getScalarVal(dsi)

    def _sunHours(self, statDict, timeRange, argList):
        # Return sunshine hours...in day periods only!
        sunStats = self.getStats(statDict, "HrsOfSun")
        if sunStats is None:
            return "N/A"
        else:
            day = self.getPeriod(timeRange, 1)
            if day == 1:
                return self.getScalarVal(sunStats)
            else:
                return ""

    def _popVal(self, statDict, timeRange, argList):
        # Return the max PoP if weather in this period
        pop = self.getStats(statDict, "PoP")
        if pop is None:
            return " "
        precipFlag, wx, qpf, pop = \
                    self._checkPrecip(statDict, timeRange, argList)
        if precipFlag:
            return self.getScalarVal(pop)
        else:
            return "0"

    def _checkPrecip(self, statDict, timeRange, argList):
        """This sets a flag to indicate precip or no precip
        in the time range. Checks Wx, PoP and QPF to allow
        different rows with precip related info to be consistent.
        Also provides the value for the Wx, QPF and PoP rows. This is
        so the same rounded value is used for all checks. So if you
        need to change rounding for PoP or QPF, it is all in one
        place. Finally, new Definition['popWxThreshold'] is required
        (Default value should be 1). PoP < popWxThreshold indicates
        no precip."""
        precipFlag = 1
        weather = self.wxVal(statDict, timeRange, ["Wx"])
        if weather.upper() == "NONE":
            precipFlag = 0
        # Weather could be non-precipitating so next check QPF
        qpfStats = self.getStats(statDict, "QPF")
        if qpfStats is None:
            precipFlag = 0
            qpf = None
        else:
            minimum, maximum, total = qpfStats
            qpf = self.round(total, "Nearest", .01)
            if qpf <= 0.0:
                precipFlag = 0
        # Next check pop:
        pop = self.getStats(statDict, "PoP")
        if pop is None:
            precipFlag = 0
        else:
            pop = self.round(pop, "Nearest", 10)
            if pop < self._popWxThreshold:
                precipFlag = 0
        return precipFlag, weather, qpf, pop

    def _cwr(self, statDict, timeRange, argList):
        # Return the max CWR.  Make sure that weather is not "None"
        weather = self.wxVal(statDict, timeRange, ["Wx"])
        if weather.upper() == "NONE":
            return "0"
        else:
            val = self.getStats(statDict, "CWR")
            if val is None:
                return " "
            val = self.round(val, "Nearest", 10)
            return self.getScalarVal(val)

    def _mixHgt500(self, statDict, timeRange, argList):
        # Return 500m MixHgt for daytime only
        mixTempStats = self.getStats(statDict, "MixHgt500")
        day = self.getPeriod(timeRange, 1)
        if day == 1:
            if mixTempStats is None:
                return "N/A"
            else:
                return self.getScalarVal(mixTempStats)
        else:
            return ""

    def _cigHeight(self, statDict, timeRange, argList):
        # Return ceiling height in feet
        cigStats = self.getStats(statDict, "Ceiling")
        if cigStats is None:
            return "N/A"
        else:
            minimum, maximum = cigStats
            minimum = self.round(minimum, "Nearest", 1)
            if minimum <= 3000:
                value = self.round(minimum, "Nearest", 100)
            elif minimum <= 6500:
                value = self.round(minimum, "Nearest", 500)
            elif minimum <= 12000:
                value = self.round(minimum, "Nearest", 1000)
            else:
                return "No Cig"
        return repr(value)

    def _addHeadlines(self, headlines):
        # Add the headlines to the list of product headlines
        headlines = headlines.split("...")
        for headline in headlines:
            if len(headline) == 0 or headline[0] == '\n':
                continue
            if headline not in self._prodHeadlines:
                self._prodHeadlines.append(headline)


    # Returns a list of the Hazards allowed for this product in VTEC format.
    # These are sorted in priority order - most important first.
    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
            ('FW.W', allActions, 'FireWx'),  # RED FLAG WARNING
            ('FW.A', allActions, 'FireWx'),  # FIRE WEATHER WATCH
            ]

