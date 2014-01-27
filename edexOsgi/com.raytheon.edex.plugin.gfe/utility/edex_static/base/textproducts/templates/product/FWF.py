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
# Description: This produces a narrative Fire Weather Forecast.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# FWF, FWF_<site>_<MultiPil>_Definition, FWF_<site>_Overrides
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
#  productName      defines name of product e.g. "ZONE FORECAST PRODUCT"
#  fullStationID    Full station identifier, 4 letter, such as "KSLC".
#  wmoID            WMO ID code for product header, such as "FOUS45"
#  pil              Product pil, such as "SFTBOS"
#  areaName (opt.)  Area name for product header, such as "WESTERN NEW YORK"
#  wfoCityState     WFO location, such as "BUFFALO NY"
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
#                         be defined for the GFE zone combiner
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
#  periodCombining     If 1, an attempt will be made to combine components
#                      or time periods into one.  Otherwise no period 
#                      combining will will be done.
#   defaultEditAreas   defines edit areas, default is Combinations
#   areaDictionary     Modify the AreaDictionary utility with UGC information 
#                      about zones
#   useRH              If 1, use RH grids instead of MaxRH, MinRH
#   summaryExtended
#   summaryArea      If summaryExtended == 1, then a summary extended forecast will
#                    be generated for the given summaryArea
#   individualExtended  If individualExtended == 1, an extended forecast will be
#                       generated for each individual area
#   extendedLabel    If extendedLabel== 1, a label will be included for each
#                    individual extended
#   lightningPhrases    Set this to 1 if you want Lightning Activity
#                       reported with phrases like "1-8 STRIKES", 
#                       "9-15 STRIKES", etc.
#   windAdjustmentFactor    Winds are reported from the Wind20ft grid 
#                           if available. Otherwise, the Wind grid is 
#                           used with the magnitude multiplied
#                           by this wind adjustment factor. Winds reported 
#                           by RAWS sites are frequently lower than ASOS winds
#                           due to the fact that they use a 10-min average.  
#                           A common adjustment factor is 80% (0.80).  
#                           If you want no adjustment to the winds
#                           then set this variable to 1.00.
#                           NOTE: This value can optionally be specified as an
#                           nlValue dictionary.
#
#  tempLocalEffects         Set to 1 to after defining edit areas AboveElev
#                           and BelowElev to enable local effects for temperature
#                           and RH
#  windLocalEffects         Set to 1 after defining edit areas Ridges and Valleys
#                           to enable local effects for wind
#
#  lineLength               defaults to 66, specifies length of lines in output
#
#  hazardSamplingThreshold  Defines the percentage coverage or number of
#                    grid points in a zone that must contain the hazard
#                    in order for it to be considered. Tuple (percent, points)
#
#  Multiple Element Table
#     To include a MultipleElementTable (e.g. Temp Pop Values)
#     for each area in the current Combination:
#         Set "includeMultipleElementTable" to 1
#         Set the "elementList" and "singleValueFormat" flag if desired
#         "elementList" may include "Temp", "PoP", and/or "Humidity"
#         "singleValueFormat" lists only one value per element
#         Make sure you are using a Combinations file
#         Modify the CityDictionary TextUtility file or create a new one
#     To include a MultipleElementTable (e.g. Temp Pop Values)
#     for each period for each area in the current Combination:
#         Set "includeMultipleElementTable_perPeriod" to 1
#         Set the "elementList" and
#         Set the "singleValueFormat" flag to 1
#         "elementList" may include "Temp", "PoP", and/or "Humidity"
#         Make sure you are using a Combinations file
#         Modify the CityDictionary TextUtility file or create a new one
#
#   includeMultipleElementTable   
#   includeMultipleElementTable_perPeriod   
#   elementList
#   singleValueFormat
#   cityDictionary
#
#  Weather-related flags
#       hoursSChcEnds        - specifies hours past the beginning of the first
#                              first period of the product to stop including 'Slight
#                               Chance' or 'Isolated' weather types (ERH policy
#                               allows values of 1-5 * 12 hour periods)               
#
#  useHolidays              Set to 1 to use holidays in the time period labels
#  includeTrends            Set to 1 to include Temp and RH trends
#
#  Trouble-shooting items
#    passLimit -- Limit on passes allowed through Narrative Tree
#    trace     -- Set to 1 to turn on trace through Narrative Tree   
#
# NARRATIVE CUSTOMIZATION POINTS
#   The phrases in this product can be customized in many ways by overriding
#   infrastructure methods in the Local file.
#   You will see common overrides in the Local file and you may change them
#   in that there.
#   For further customization, you can determine  which phrases your product is
#   using by examining the Component Product Definitions below.
#   Then, you can look up the phrase in the Text Product User Guide which will
#   describe the all the relevant override methods associated with the phrase.
#   Refer to the Customization section of the Text Product User Guide
#   for step-by-step information.
#-------------------------------------------------------------------------
# Weather Elements Needed:
#   Hazards (optional):  If provided, headlines will be generated.
#   Sky, LAL, Wind (6 hourly), MaxRH, MinRH  (out 2 days), PoP, MaxT,
#   MinT, T, Wx (out to 7 days)
#  Optional:
#   Ttrend, RHtrend, Haines, TransWind, MixHgt, VentRate, CWR
#   MarineLayer:  If used, uncomment MarineLayer lines in:
#        getFirePeriod_analysisList
#        getFirePeriod_phraseList     
#-------------------------------------------------------------------------
# Edit Areas Needed:
#   Optional:
#     For local effects: AboveElev, BelowElev
#     NOTE: Set the Definition["tempLocalEffects"] = 1 in the Site Definition File
#       Define edit areas with an appropriate elevation
#         (e.g. in CO, it is 11000 for timberline).
#       This will be used to report local effects for temperature and RH.
#     For Valley/Ridge Winds: Valleys, Ridges
#     NOTE: Set the Definition["windLocalEffects"] = 1 in the Site Definition File
#       Define edit areas for Valleys and Ridges to be intersected with the current
#       area for reporting as separate Valley and Ridge winds.
#       Be sure and set up the list of areas for which you want separate
#       Valley and Ridge winds in the "ridgeValleyAreas" list in the Local file.
#       IF you do NOT want to distinguish between Valley and Ridge winds,
#       you can comment out these edit area entries in the "intersectArea" section
#       of the Local file.
#     Fire Area for the extended summary.  This defaults to your CWA, but you
#       can change it in the Definition overrides if you like.
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file:
#   Combinations
#-------------------------------------------------------------------------
# Component Products:
#      FirePeriod (component)
#      ExtendedLabel(component)
#      FireExtendedShortTerm (component)
#      FireExtended (component)
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
# To look up tasks and their status, see the Text Product User Guide
# Section on "Tkgnats: Task Reporting System".
#-------------------------------------------------------------------------
# Additional Information:
#   Modify the _tempLocalEffects and _rhLocalEffects " above timberline "
#   descriptor (in the FWF_Local file) to appropriate wording for your area.
#
#  COMMON OVERRIDES
#     from FWF
#       getFirePeriod_intersectAreas
#       _tempLocalEffects_list
#       _rhLocalEffects_list
#     from ConfigVariables:
#       units_descriptor_dict
#       untilPhrasing_flag_dict
#     from FirePhrases:
#       includeSkyRanges_flag
#       ridgeValleyAreas
#     from WxPhrases:
#       pop_wx_lower_threshold
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
#-------------------------------------------------------------------------
 

import TextRules
import SampleAnalysis
import ForecastNarrative
import time, string, types
import TimeRange

class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = []
    Definition =  {
        "type": "smart",
        "displayName": "None",
        # Source database for product. Can be "Official", "Fcst" or "ISC"
        "database": "Official",
        # Defines output location of finished product.
        "outputFile": "{prddir}/TEXT/FWF_<MultiPil>.txt",
        "debug": 0,
        # Name of map background for creating Combinations
        "mapNameForCombinations": "FireWxZones_<site>",
        
        ## Edit Areas: Create Combinations file with edit area combinations.
        "showZoneCombiner" : 1, # 1 to cause zone combiner to display
        "defaultEditAreas" : "Combinations_FWF_<site>_<MultiPil>",
        "editAreaSuffix": None,
        
        # product identifiers
        "productName": "FIRE WEATHER PLANNING FORECAST", # product name 
        "fullStationID": "<fullStationID>",    # full station identifier (4letter)
        "wmoID": "<wmoID>",          # WMO ID
        "pil": "<pil>",            # Product pil
        "areaName": "<state>",             # Name of state, such as "GEORGIA" -- optional
        "wfoCityState": "<wfoCityState>",  # Location of WFO - city state
        
        "textdbPil": "<textdbPil>",       # Product ID for storing to AWIPS text database.
        "awipsWANPil": "<awipsWANPil>",   # Product ID for transmitting to AWIPS WAN.
        "periodCombining" : 0,       # If 1, combine periods, if possible
        "lineLength" : 66,           # line length

        "hazardSamplingThreshold": (10, None),  #(%cov, #points)

        # Product-specific variables:
        "lightningPhrases": 0,         # Set to 1 to report lightning as phrases (e.g. 1-8 STRIKES)
        "windAdjustmentFactor": 0.80,  # Adjustment for Wind if no Wind20ft grid available
        
        # To include a MultipleElementTable (e.g. Temp Pop Table)
        # for each area in the current Combination:
        #   Set "includeMultipleElement" to 1
        #   Set the "elementList" and "singleValueFormat" flag if desired
        #   "elementList" may include "Temp", "PoP", and/or "Humidity"
        #   "singleValueFormat" lists only one value per element
        #   Make sure you are using a Combinations file
        #   Modify the CityDictionary TextUtility file or create a new one
        "includeMultipleElementTable": 0,
        "includeMultipleElementTable_perPeriod": 0,
        "elementList" : ["Temp", "Humidity", "PoP"],
        "singleValueFormat": 0,
        "cityDictionary": "CityDictionary",
        # Area Dictionary -- Descriptive information about zones
        "areaDictionary": "AreaDictionary", 
        # Language
        "language": "english",

        "useRH": 0,                 # Use RH grids instead of MaxRH, MinRH
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

        # Weather-related flags
        "hoursSChcEnds": 24,                
        
        "useHolidays": 1,            # Set to 1 to use holidays in the time period labels
        "includeTrends": 1,          # Set to 1 to include Temp and RH trends
        "tempLocalEffects": 0,       # Set to 1 to enable Temp and RH local effects AFTER
                                     # creating AboveElev and BelowElev edit areas
        "windLocalEffects": 0,       # Set to 1 to enable wind local effects AFTER
                                     # creating Ridges and Valleys edit areas
        "fixedExpire": 1,            #ensure VTEC actions don't affect expiration time


        # Trouble-shooting items
        "passLimit": 20,             # Limit on passes allowed through
                                     # Narrative Tree
        "trace": 0,                  # Set to 1 to turn on trace through
                                     # Narrative Tree for trouble-shooting
        }

    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)

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

    def minMax_std_deviation(self, parmHisto, timeRange, componentName):
        # Replaces MINMAX_STD_DEVIATION
        # Number of standard deviations to compute around the weighted
        # average for a stdDev_MinMax
        return 1.4

    def element_outUnits_dict(self, tree, node):
        dict = TextRules.TextRules.element_outUnits_dict(self, tree, node)
        dict["Wind"] = "mph"
        dict["Wind20ft"] = "mph"
        dict["TransWind"] = "mph"
        dict["FreeWind"] = "mph"
        dict["WindGust"] = "mph"
        return dict

    #############################
    # Overrides to take care of Wind in the Extended forecast
    # Use Wind20ft if available, else use adjusted Wind
    #
    def adjust_method_dict(self, tree, node):
        # Special adjustment methods
        #
        return {
            "Wind": self._adjustWind,
            }

    def _adjustWind(self, value):
        # adjustment for winds
        factor = self.nlValue(self._windAdjustmentFactor, value)
        value = value * factor
        return value

    def wind_summary_words(self, tree, node):
        # See if there's data for Wind20ft
        elementName = "Wind"
        args = node.get("args")
        if args is not None:
            elementName = args[0]
        if elementName == "Wind":
            elementName = self.chooseElement(tree, node, ["Wind20ft", "Wind"])
        words = self.vector_summary(tree, node, elementName)
        return self.setWords(node, words)

    def wind_setUp(self, tree, node, gustFlag=0, element="Wind", connectorMethod=None):
        args = node.get("args")
        if args is not None:
            element = args[0]
        if element == "Wind":
            # See if there's data for Wind20ft
            element = self.chooseElement(tree, node, ["Wind20ft", "Wind"])
        wind = self.ElementInfo(element, "List", self.VECTOR())
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
    
    def phrase_descriptor_dict(self, tree, node):
        # Descriptors for phrases
        dict = TextRules.TextRules.phrase_descriptor_dict(self, tree, node)
        # If not extended, make descriptor empty
        componentName = node.getComponent().get("name")
        if componentName == "FirePeriod":
            dict["Wind"] = ""
            dict["Wind20ft"] = ""
        return dict

    def nextDay24HourLabel_flag(self, tree, node):
        # Return 1 to have the TimeDescriptor module label 24 hour periods starting
        # after 1600 as the next day.
        # This is needed for the Fire Weather Extended product,
        # but not for other products when period combining.
        return 1

    def untilPhrasing_flag_dict(self, tree, node):
        # If set to 1, "until" time descriptor phrasing will be used.
        # E.g. "NORTH WINDS 20 MPH UNTIL 10 AM...THEN 35 MPH"
        #
        # NOTE: Be sure to increase the temporal resolution by 
        # overriding "getFirePeriod_analysisList" from the FWF standard file.
        # E.g.  ("MixHgt", self.minMax, [0]),

        dict = TextRules.TextRules.untilPhrasing_flag_dict(self, tree, node)
        dict["LAL"] = 1
        componentName = node.getComponent().get("name")
        if componentName == "FirePeriod":
            dict["Sky"] = 1
            dict["Wx"]  = 1
        return dict

    ########################################################################
    # COMPONENT PRODUCT DEFINITIONS
    ########################################################################

    def _PoP_analysisMethod(self, componentName):
        # Alternative PoP analysis methods for consistency between PoP and Wx
        #return self.maxMode
        #return self.maximum
        return self.stdDevMaxAvg 

    def FirePeriod(self):
        phraseList = self.getFirePeriod_phraseList()
        analysisList = self.getFirePeriod_analysisList()
        intersectAreas = self.getFirePeriod_intersectAreas()
        return {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assembleIndentedPhrases,            
                          ],
            "analysisList": analysisList,
            "phraseList": phraseList,
            "intersectAreas": intersectAreas,
            }
    
    def ExtraSampling(self):
        analysisList = [
                   ("MaxT", self.mode, [0]),
                   ("MinT", self.mode, [0]),
                  ]
        if self._useRH:
            analysisList += [("RH", self.mode, [0])]
        else:
            analysisList += [("MaxRH", self.mode, [0]),
                             ("MinRH", self.mode, [0])]
        return {
            "type": "component",
            "methodList": [self.noWords],
            "analysisList": analysisList,
            "phraseList": [],
            "intersectAreas": self.getFirePeriod_intersectAreas(),
            }

    def getFirePeriod_analysisList(self):
         analysisList = [
           ("Sky", self.minMax, [0]),
           ("PoP", self._PoP_analysisMethod("FirePeriod"), [3]),
           ("PoP", self.binnedPercent, [3]),
           ("Wx", self.rankedWx, [0]),
           ("LAL", self.maximum, [0]),
           ("LAL", self.binnedPercent, [0]),
           ("MaxT", self.minMax),
           ("MinT", self.minMax),
           ("T", self.minMax),
           ("MaxRH", self.minMax),
           ("MinRH", self.minMax),
           ("RH", self.minMax),
           ("MaxT", self.mode),   # for trends
           ("MinT", self.mode),   # for trends
           ("MaxRH", self.mode),  # for trends
           ("MinRH", self.mode),  # for trends
           ("RH", self.mode),     # for trends
           ("Ttrend", self.avg),  # for trends
           ("RHtrend", self.avg), # for trends
           ("Wind", self.vectorAvg, [6]),
           ("Wind20ft", self.vectorAvg, [6]),
           #("RidgeWind", self.vectorAvg, [6]),
           ("Haines", self.avg),
           ("TransWind", self.vectorAvg, [6]),
           ("FreeWind", self.vectorAvg, [6]),
           ("MixHgt", self.minMax),
           ("VentRate", self.minMax),
           ("CWR", self.avg),
           #("MarineLayer", self.minMax),
          ]
         return analysisList
         
    def getFirePeriod_phraseList(self):
        if self._useRH:
            dayRH = "RH"
            nightRH = "RH"
        else:
            dayRH = "MinRH"
            nightRH = "MaxRH"
        phraseList =  [
            self.skyWeather_byTimeRange_compoundPhrase,
            self.lal_phrase,
            (self.dayOrNight_phrase, ["MaxT", "MinT", 1, 1],
             self._tempLocalEffects_list()),
            (self.trend_DayOrNight_phrase, ["MaxT", "MinT", "Ttrend", 1, 1],
             self._tempTrendLocalEffects_list()),
            (self.dayOrNight_phrase, [dayRH, nightRH, 1, 1], self._rhLocalEffects_list()),
            (self.trend_DayOrNight_phrase, [dayRH, nightRH, "RHtrend", 1, 1],
             self._rhTrendLocalEffects_list()),
            self.humidityRecovery_phrase,
            self.fireWind_compoundPhrase,
            self.fireWind_label_phrase,  
            self.fireValleyWind_compoundPhrase,  
            self.fireRidgeWind_compoundPhrase,  
            self.haines_phrase,
            self.smokeDispersal_phrase,
            self.mixingHgt_phrase,
            self.transportWind_phrase,
            #self.freeWind_phrase,  
            self.cwr_phrase,
            #self.marineLayer_phrase,
            ]
        # Remove trend methods
        if self._includeTrends != 1:
            newList = []
            for phrase in phraseList:
                if type(phrase) is types.TupleType:
                    phraseMethod = phrase[0]
                    if phraseMethod == self.trend_DayOrNight_phrase:
                       continue
                newList.append(phrase)
            phraseList = newList
        # Add multipleElementTable
        if self._includeMultipleElementTable_perPeriod:
            phraseList.append(self.multipleElementTable_perPeriod_phrase)
        return phraseList
                            
    def getFirePeriod_intersectAreas(self):
        tempList = []
        windList = []
        if self._tempLocalEffects:
            tempList = [
                ("MinT", ["BelowElev", "AboveElev"]),
                ("MaxT", ["BelowElev", "AboveElev"]),
                ("MinRH", ["BelowElev", "AboveElev"]),
                ("MaxRH", ["BelowElev", "AboveElev"]),
                ("RH", ["BelowElev", "AboveElev"]),
                ("Ttrend", ["BelowElev", "AboveElev"]),
                ("RHtrend", ["BelowElev", "AboveElev"]),
                ]
        if self._windLocalEffects:
            windList = [
                ("Wind", ["Valleys", "Ridges"]),
                ("Wind20ft", ["Valleys", "Ridges"]),
                ("WindGust", ["Valleys", "Ridges"]),
                ]
        return tempList + windList
    
    def _tempLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("BelowElev", "")
        leArea2 = self.LocalEffectArea("AboveElev", " above timberline")
        return [self.LocalEffect([leArea1, leArea2], 8, "...except ")]
    
    def _rhLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("BelowElev", "")
        leArea2 = self.LocalEffectArea("AboveElev", " above timberline")
        return [self.LocalEffect([leArea1, leArea2], self._rhTrigger, "...except ")]

    def _tempTrendLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("BelowElev", "")
        leArea2 = self.LocalEffectArea("AboveElev", "above timberline")
        return [self.LocalEffect([leArea1, leArea2], self._trendTrigger, "...except ")]

    def _rhTrendLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("BelowElev", "")
        leArea2 = self.LocalEffectArea("AboveElev", "above timberline")
        return [self.LocalEffect([leArea1, leArea2], self._trendTrigger, "...except ")]    

    def _rhTrigger(self, tree, node, localEffect, leArea1Label, leArea2Label):
        first = node.getAncestor("firstElement")
        element = first.name
        dataType = first.dataType
        timeRange = node.getTimeRange()
        mergeMethod="MinMax"
        if element == "RH":
            day = self.getPeriod(timeRange, 1)
            if day:
                mergeMethod="Min"
            else:
                mergeMethod="Max"
                
        area1Stats = tree.stats.get(element, timeRange, leArea1Label,
                                    mergeMethod=mergeMethod)
        area2Stats = tree.stats.get(element, timeRange, leArea2Label,
                                    mergeMethod=mergeMethod)
        #print "\nLocal effects", element, timeRange
        #print leArea1Label, area1Stats
        #print leArea2Label, area2Stats
        if area1Stats is None or area2Stats is None:
            return 0
        flag = self.checkLocalEffectDifference(
            tree, node, dataType, 8, area1Stats, area2Stats,
            leArea1Label, leArea2Label)
        return flag

    def _trendTrigger(self, tree, node, localEffect, leArea1Label, leArea2Label):
        #print "*** Inside _trendTrigger ***"
        first = node.getAncestor("firstElement")
        element = first.name
        #print "element", element
        dataType = first.dataType
        timeRange = node.getTimeRange()

        if element.find("T") >= 0:
            trendElement = "Ttrend"
        else:
            trendElement="RHtrend"
                                
        # trend stats
        area1AbsDiff, area1RawDiff = self.getTrendStats(
            tree, node, element, timeRange, leArea1Label, trendElement)
        area2AbsDiff, area2RawDiff = self.getTrendStats(
            tree, node, element, timeRange, leArea2Label, trendElement)

        #print "\nTrend Local effects", element, timeRange
        #print "\t\tBelow", area1RawDiff
        #print "\t\tAbove", area2RawDiff

        if area1AbsDiff is None or area2AbsDiff is None:
            return 0
        
        # Use rawDiff because sign is important e.g. warmer vs. cooler
        flag = self.checkLocalEffectDifference(
            tree, node, dataType, 4, area1RawDiff, area2RawDiff,
            leArea1Label, leArea2Label)
        return flag                
        
    def skyPopWx_excludePoP_flag(self, tree, node):
        # If set to 1, PoP will not be included in the skyPopWx_phrase
        return 1
    
    def ExtendedLabel(self):
        return {
            "type": "component",
            "methodList": [self.setLabel],
            "analysisList": [],
            "phraseList":[],
            }
    def setLabel(self, tree, component):
        component.set("words", "\n.FORECAST DAYS 3 THROUGH 7......\n")
        return self.DONE()

    def FireExtendedShortTerm(self):
        return {
            "type": "component",
            "methodList": [
                          self.orderPhrases,
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            "analysisList": [
                       ("MinT", self.firstAvg),
                       ("MaxT", self.avg),
                       ("T", self.hourlyTemp),
                       ("Sky", self.avg, [12]),
                       ("Wind", self.vectorAvg),
                       ("Wind20ft", self.vectorAvg),
                       ("Wx", self.rankedWx, [6]),
                       ("PoP", self._PoP_analysisMethod("FireExtendedShortTerm"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                      ],
            "phraseList":[
                   self.reportTrends,
                   self.wind_summary,
                   self.sky_phrase,
                   self.skyPopWx_phrase,
                   self.weather_phrase,
                   self.lows_phrase,
                   self.highs_phrase,
                   self.wind_phrase,
                 ],
            }

    def FireExtended(self):
        return {
            "type": "component",
            "methodList": [
                          self.orderPhrases,
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            "analysisList": [
                       ("MinT", self.firstAvg),
                       ("MaxT", self.avg),
                       ("T", self.hourlyTemp),
                       ("Sky", self.avg, [12]),
                       ("Wx", self.rankedWx, [6]),
                       ("PoP", self._PoP_analysisMethod("FireExtended"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                      ],
            "phraseList":[
                   self.reportTrends,
                   self.sky_phrase,
                   self.skyPopWx_phrase,
                   self.weather_phrase,
                   self.lows_phrase,
                   self.highs_phrase,
                 ],
            }
    
    def ExtendedNarrative(self):
        return {
          "type": "narrative",
          "methodList": [self.assembleChildWords],
          ## Components
          "narrativeDef": [
                       ("FireExtendedShortTerm",24),("FireExtendedShortTerm",24),
                       ("FireExtendedShortTerm",24),
                       ("FireExtended",24),("FireExtended",24)
                       ],
          }

    def generateForecast(self, argDict):
        # Generate Text Phrases for a list of edit areas

        # Get variables
        error = self._getVariables(argDict)
        if error is not None:
            return error

        # Get the areaList -- derived from defaultEditAreas and
        # may be solicited at run-time from user if desired
        self._areaList = self.getAreaList(argDict)
        if len(self._areaList) == 0:
            return "WARNING -- No Edit Areas Specified to Generate Product."

        # Determine time ranges
        error = self._determineTimeRanges(argDict)
        if error is not None:
            return error

        # Sample the data
        error = self._sampleData(argDict)
        if error is not None:
            return error

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

        # Get VariableList and _issuance_list variables
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"

        self._language = argDict["language"]
        return None

    def _determineTimeRanges(self, argDict):
        # Set up the Narrative Definition and initial Time Range
        self._issuanceInfo = self.getIssuanceInfo(
            self._productIssuance, self._issuance_list(argDict), argDict["creationTime"])
        self._timeRange = self._issuanceInfo.timeRange()
        argDict["productTimeRange"] = self._timeRange
        self._expireTime = self._issuanceInfo.expireTime()
        self._issueTime = self._issuanceInfo.issueTime()
        self._definition["narrativeDef"] = self._issuanceInfo.narrativeDef()
        if self._periodCombining:
            self._definition["methodList"] = \
               [self.combineComponentStats, self.assembleChildWords]
        else:
            self._definition["methodList"] = [self.assembleChildWords]
        # Determine the extended range
        if self._individualExtended == 1:
            self._extendedStart = self._timeRange.endTime() - 24*5*3600
        else:
            self._extendedStart = self._timeRange.endTime()
        self._extendedRange = TimeRange.TimeRange(
            self._extendedStart, self._extendedStart + 3600)

        # Calculate current times
        self._ddhhmmTime = self.getCurrentTime(
            argDict, "%d%H%M", shiftToLocal=0, stripLeading=0)
        self._timeLabel = self.getCurrentTime(
            argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1)
        return None

    def _sampleData(self, argDict):
        # Sample and analyze the data for the narrative
        self._narrativeProcessor = ForecastNarrative.ForecastNarrative()
        error = self._narrativeProcessor.getNarrativeData(
            argDict, self._definition, self._timeRange, self._areaList, self._issuanceInfo)
        if error is not None:
            return error
        return None

    def _preProcessProduct(self, fcst, argDict):
        # Product header
        if self._areaName != "":
             productName = self._productName.strip() + " FOR " + \
                           self._areaName.strip()
        else:
             productName = self._productName.strip()

        issuedByString = self.getIssuedByString()
        productName = self.checkTestMode(argDict, productName)

        fcst =  fcst + self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n" +\
               productName + "\n" +\
               "NATIONAL WEATHER SERVICE " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n\n"
        
        # Put in a place holder for the headlines to be substituted in
        # "postProcessProduct"
        fcst = fcst + "<HEADLINES>"
        self._prodHeadlines = []
        
        fcst = fcst + ".DISCUSSION..." + "\n\n\n\n\n"
        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        # This is the header for an edit area combination
        areaHeader = self.makeAreaHeader(
            argDict, areaLabel, self._issueTime, self._expireTime,
            self._areaDictionary, self._defaultEditAreas)
        fcst = fcst + areaHeader

        # get the hazards text
        self._hazards = argDict['hazards']
        self._combinations = argDict["combinations"]

        headlines = self.generateProduct("Hazards", argDict, area = editArea,
                                         areaLabel=areaLabel,
                                         timeRange = self._timeRange)

        self._addHeadlines(headlines)
        fcst = fcst + headlines

        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        argDict["language"] = self._language
        # Generate Narrative Forecast for Edit Area
        fcst = fcst + self._narrativeProcessor.generateForecast(
            argDict, editArea, areaLabel)
        if self._includeMultipleElementTable == 1:
            fcst = fcst + self.makeMultipleElementTable(areaLabel, self._timeRange, argDict)
        return fcst

    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        fcst = fcst + "\n$$\n\n"
        return fcst

    def _postProcessProduct(self, fcst, argDict):
        # Add one extended
        if self._summaryExtended == 1:
            fcst = fcst + "\n.FORECAST DAYS 3 THROUGH 7...\n\n"
            extended = self.generateProduct("ExtendedNarrative",
                argDict, area=self._summaryArea,
                timeRange=self._extendedRange)
            fcst = fcst + extended
        fcst = fcst + "\n.OUTLOOK\n\n\n$$\n"

        # Make summary headline string and substitute for "<HEADLINE>" placeholder
        headlineStr = ""
        for h in self._prodHeadlines:
            headlineStr = headlineStr + "..." + h + "...\n"
        if len(self._prodHeadlines):
            headlineStr = headlineStr + "\n"
        fcst = fcst.replace("<HEADLINES>", headlineStr)        
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst

    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################

    def _addHeadlines(self, headlines):
        # Add the headlines to the list of product headlines
        headlines = headlines.split("...")
        for headline in headlines:
            if len(headline) == 0 or headline[0] == '\n':
                continue
            if headline not in self._prodHeadlines:
                self._prodHeadlines.append(headline)

    def _issuance_list(self, argDict):
        narrativeDefAM = [
            ("FirePeriod", "period1"),
            ("FirePeriod", 12), ("FirePeriod", 12),
            ("Custom", ("ExtraSampling", (-24, 12))),
           ]
        narrativeDefPM = [
            ("FirePeriod", "period1"),
            ("FirePeriod", 12), ("FirePeriod", 12), ("FirePeriod", 12),
            ("Custom", ("ExtraSampling", (-24, 24))),
            ]
        extended = [
            ("FireExtendedShortTerm", 24), ("FireExtendedShortTerm", 24),
            ("FireExtendedShortTerm", 24), 
            ("FireExtended", 24), ("FireExtended", 24),  
            ]
        try:
            if self._individualExtended == 1:
                if self._extendedLabel == 1:
                    narrativeDefAM.append(("ExtendedLabel",0))
                    narrativeDefPM.append(("ExtendedLabel",0))
                narrativeDefAM = narrativeDefAM + extended
                narrativeDefPM = narrativeDefPM + extended
        except:
            pass
        
        return [
            ("Morning", self.DAY(), self.NIGHT(), 16,
             ".Today...", "early in the morning", "late in the afternoon",
             1, narrativeDefAM), 
            ("Morning Update", "issuanceHour", self.NIGHT(), 16,
             ".Rest of Today...", "early in the morning", "late in the afternoon",
             1, narrativeDefAM), 
            ("Afternoon Update", "issuanceHour", self.NIGHT(), 16,
             ".Rest of Today...", "early in the morning","late in the afternoon",
             1, narrativeDefAM), 
            #  End times are tomorrow:
            ("Afternoon", self.NIGHT(), 24 + self.DAY(), 24 + 4,
             ".Tonight...", "late in the night", "early in the evening",
             1, narrativeDefPM), 
            ("Evening Update", "issuanceHour", 24 + self.DAY(), 24 + 4,
             ".Rest of Tonight...", "late in the night","early in the evening",
             1, narrativeDefPM),
            # For the early morning update, this produces:
            # Rest of Tonight:
            # MONDAY
            # MONDAY NIGHT
            ("Early Morning Update", "issuanceHour", self.DAY(), 4,
             ".Rest of Tonight...", "early in the morning","late in the afternoon",
             0, narrativeDefPM), 
            # Alternative
            # For the early morning update, this produces:
            # EARLY THIS MORNING:
            # Today
            # Tonight
            #("Evening Update", "issuanceHour", 24 + self.DAY(), 24 + 4,
            # ".Rest of Tonight...", "late in the night", "early in the evening",
            # 1, narrativeDefPM), 
            #("Early Morning Update", "issuanceHour", self.DAY(), 4,
            # ".EARLY THIS MORNING...", "early in the morning", "late in the afternoon",
            # 1, narrativeDefPM), 
            ]

    def lateDay_descriptor(self, statDict, argDict, timeRange):
        # If time range is in the first period, return period1 descriptor for
        #  late day -- default 3pm-6pm
        if self._issuanceInfo.period1TimeRange().contains(timeRange):
            return self._issuanceInfo.period1LateDayPhrase()
        else:
            return "late in the afternoon"
        
    def lateNight_descriptor(self, statDict, argDict, timeRange):
        # If time range is in the first period, return period1 descriptor for
        #  late night -- default 3am-6am
        if self._issuanceInfo.period1TimeRange().contains(timeRange):
            return self._issuanceInfo.period1LateNightPhrase()
        else:
            return "early in the morning"

    # Returns a list of the Hazards allowed for this product in VTEC format.
    # These are sorted in priority order - most important first.
    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
            ('FW.W', allActions, 'FireWx'),  # RED FLAG WARNING
            ('FW.A', allActions, 'FireWx'),  # FIRE WEATHER WATCH
            ('HW.W', allActions, 'Wind'),    # HIGH WIND WARNING
            ('WI.Y', allActions, 'Wind'),    # WIND ADVISORY
            ('HW.A', allActions, 'Wind'),    # HIGH WIND WATCH
            ('EH.W', allActions, 'Heat'),    # EXCESSIVE HEAT WARNING
            ('HT.Y', allActions, 'Heat'),    # HEAT ADVISORY
            ]

    

