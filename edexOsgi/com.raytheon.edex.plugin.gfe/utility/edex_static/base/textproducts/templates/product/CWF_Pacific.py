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
# Description: CWF_Pacific (Coastal Waters Forecast)
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# CWF_Pacific.py, CWF_Pacific_<site>_<MultiPil>_Definition, 
#   CWF_Pacific_<site>_Overrides
#-------------------------------------------------------------------------
# Customization Points in Local File:
#
# DEFINITION SECTION
#
# Required Configuration Items:
#
#  displayName      If not None, defines how product appears in GFE GUI
#  defaultEditAreas defines edit areas, default is Combinations
#  productName      defines name of product e.g. "COASTAL WATERS FORECAST"
#  fullStationID    Full station identifier, 4 letter, such as "KSLC".
#  wmoID            WMO ID code for product header, such as "FOUS45"
#  pil              Product pil, such as "CWFBOS"
#  areaName (opt.)  Area name for product header, such as "WESTERN NEW YORK"
#  wfoCityState     City,state that the WFO is located in, such as "BUFFALO, NY"
#
# Optional Configuration Items
#
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
#                      combining will will be done.
#  includeEveningPeriod   Include a 6 hour Evening period on the 3rd day
#  useAbbreviations
#      If 1, use marine abbreviations e.g. TSTM instead of THUNDERSTORM, 
#      NW instead of NORTHWEST
#      (See marine_abbreviateText in the TextRules module)
#
#  Weather-related flags
#       hoursSChcEnds        - specifies hours past the beginning of the first
#                              first period of the product to stop including 'Slight
#                               Chance' or 'Isolated' weather types (ERH policy
#                               allows values of 1-5 * 12 hour periods)
#  River Bar Zones
#       riverBarZones        - list to designate any river bars in the forecast
#
#
#  areaDictionary    Modify the AreaDictionary utility with UGC 
#                    information about zones
#
#  useHolidays              Set to 1 to use holidays in the time period labels
#
#  Trouble-shooting items
#    passLimit -- Limit on passes allowed through Narrative Tree
#    trace     -- Set to 1 to turn on trace through Narrative Tree   
#
# OVERRIDES
#
# Required Overrides
#
#  _Text1(), _Text2()  Descriptive text for header
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
#
#-------------------------------------------------------------------------
# Weather Elements Needed:
#    Wind (every 3 hours to 3 days, then every 6 hours to 7 days)
#    WaveHeight and/or WindWaveHgt
#         (every 6 hours to 3 days, then every 12 hours to 7 days)
#    Wx (every 6 hours to 3 days, then every 12 hours to 7 days)
#    Optional:
#       WindGust (every 3 hours to 7 days)
#       Swell, Swell2, Period, Period2 (every 6 hours to 7 days)
#-------------------------------------------------------------------------
# Edit Areas Needed: None
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file:
# Combinations
#-------------------------------------------------------------------------
# Component Products:
#      CWFPeriod (component)
#      CWFPeriodMid (component)
#      ExtendedLabel(component)
#      CWFExtended (component)
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
#
# To look up tasks and their status, see the Text Product User Guide
# Section on "Tkgnats: Task Reporting System".
#-------------------------------------------------------------------------
# Additional Information:
#
#   COMMON OVERRIDES
#     from CWF_Pacific:
#       _Text1
#       _Text2
#       _issuance_list
#     from MarinePhrases
#       inlandWatersAreas
#       inlandWatersWave_element
#       seasWaveHeight_element
#       seasWindWave_element
#       waveHeight_wind_threshold
#       marine_wind_flag
#       marine_wind_combining_flag
#       marine_wind_verbose_flag
#     from ConfigVariables
#       phrase_descriptor_dict
#       phrase_connector_dict
#       null_nlValue_dict
#       first_null_phrase_dict
#       null_phrase_dict
#       maximum_range_nlValue_dict
#       combine_singleValues_flag_dict
#     from WxPhrases:
#       embedded_visibility_flag
#       visibility_wx_threshold
#       significant_wx_visibility_subkeys
#       wxCoverageDescriptors
#       wxTypeDescriptors
#       wxAttributeDescriptors
#       wxIntensityDescriptors
#       wxCombinations
#       combine_T_RW
#     from SampleAnalysis
#       moderated_dict
#-------------------------------------------------------------------------
# Example Output:
#  Refer to the NWS Directives for Marine Services.
#-------------------------------------------------------------------------

import TextRules
import SampleAnalysis
import ForecastNarrative
import ModuleAccessor
import time, string, re, types
import TimeRange


class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = [
        #(("Include Tropical?", "includeTropical") , "No", "radio", ["Yes","No"]),
        ]  
    Definition =  {
        "type": "smart",
        "displayName": "None",
        "database": "Official",
        # Defines output location of finished product.
        "outputFile": "{prddir}/TEXT/CWF_<MultiPil>.txt",
        "debug": 0,
        # Name of map background for creating Combinations
        "mapNameForCombinations": "Marine_Zones_<site>", 

        "lineLength": 66,
        ## Edit Areas: Create Combinations file with edit area combinations.
        "showZoneCombiner" : 1, # 1 to cause zone combiner to display
        "defaultEditAreas" : "Combinations_CWF_<site>_<MultiPil>",
        "editAreaSuffix": None,
        # product identifiers
        "productName": "COASTAL WATERS FORECAST", # product name 
        "fullStationID": "<fullStationID>",    # full station identifier (4letter)
        "wmoID": "<wmoID>",          # WMO ID
        "pil": "<pil>",            # Product pil
        "areaName": "<state>",             # Name of state, such as "GEORGIA" -- optional
        "wfoCityState": "<wfoCityState>",   # Location of WFO - city state
                
        "textdbPil": "<textdbPil>",       # Product ID for storing to AWIPS text database.
        "previousProductID": "<textdbPil>", # Product ID for river bar forecast.
        "awipsWANPil": "<awipsWANPil>",   # Product ID for transmitting to AWIPS WAN.

        "fixedExpire": 1,       #ensure VTEC actions don't affect segment expiration time

        "hazardSamplingThreshold": (10, None),  #(%cov, #points)

        "periodCombining" : 0,       # If 1, combine periods, if possible
        # Product-specific variables:
        # Set to one if you want a 6-hour evening period instead of
        # 18-hour period without lows
        "includeEveningPeriod": 1,
        "useAbbreviations": 1,

        # Weather-related flags
        "hoursSChcEnds": 2,        
        
        # riverBarZones[] list to designate any river bars in the forecast 
        "riverBarZones" : [],

        # Area Dictionary -- Descriptive information about zones
        "areaDictionary": "AreaDictionary", 
        "useHolidays": 0,            # Set to 1 to use holidays in the time period labels
        # Language
        "language": "english",

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
    # OVERRIDING METHODS, THRESHOLDS AND VARIABLES
    ########################################################################
    # MUST BE OVERRIDDEN IN LOCAL FILE
    def _Text1(self):
        return ""
    
    def _Text2(self, argDict):
        synopsis = ""
        
        #  Try to get Synopsis from previous CWF
        #productID = "BOSCWFBOS"
        #synopsis = self.getPreviousProduct(productID, "SYNOPSIS")
        #  Clean up the previous synopsis
        #synopsis = re.sub(r'\n', r' ', synopsis)
        #synopsis = self.endline(synopsis, linelength=66, breakStr=" ")

        #  Convert absolute time pointer to a tuple of values like that
        #  returned by time.gmtime()
        #expTuple = time.strptime('%s' % (self._expireTime),
        #                         '%b %d %y %H:%M:%S GMT')
        expTuple = self._expireTime.utctimetuple()
        
        #  Format expiration time for inclusion in synopsis header
        expTime = time.strftime('%d%H%M', expTuple)
        siteID = self.getSiteID(argDict)

        if len("_<MultiPil>") == 0:
            ugc = self.synopsisUGC(siteID)
            heading = self.synopsisHeading(siteID)
        else:
            ugc = self.synopsisUGC(siteID, self._pil[-3:])
            heading = self.synopsisHeading(siteID, self._pil[-3:])

        return "%s-%s-\n" % (ugc, expTime) + self._timeLabel + "\n\n" + \
               heading + "\n" + synopsis + "\n$$\n\n"

    
    ########################################################################       
    def _getBarForecast(self, editArea, areaLabel, argDict):
        # IF current area contains a river bar zone
        # return the riverBarForecast 
        for barZone in self._riverBarZones:
            if self.currentAreaContains(argDict["tree"], [barZone]):
                barName = self._getAreaLabel(barZone)
                return " " + self.getPreviousProduct(self._previousProductID, barName)
        return ""

    def _getAreaLabel(self, areaName):
        # Get label from AreaDictionary
        accessor = ModuleAccessor.ModuleAccessor()
        areaDict = accessor.variable(self._areaDictionary, "AreaDictionary")
        if areaName in areaDict.keys():
            entry = areaDict[areaName]
            if entry.has_key("ugcName"):
                return entry["ugcName"]
        return ""
        
    ########################################################################

    def pop_wx_lower_threshold(self, tree, node):
        # Always report weather
        return 0

    def rounding_method_dict(self, tree, node):
        # Special rounding methods
        #
        return {
            "Wind": self.marineRounding,
            }
                
    def element_outUnits_dict(self, tree, node):
        dict = TextRules.TextRules.element_outUnits_dict(self, tree, node)
        dict["Visibility"] = "NM"
        return dict

    def gust_wind_difference_nlValue(self, tree, node):
        # Difference between gust and maxWind below which gusts are not
        # mentioned. Units are MPH
        if self._includeTropical:
            return 5
        else:
            return 10
        


    # SampleAnalysis overrides
    def temporalCoverage_hours(self, parmHisto, timeRange, componentName):
        # COMMENT: At WFO MFL we use 3 hrly wind grids. If you use 1 hrly wind grids
        # and this parameter is 2 or higher, tropical cyclone winds affecting the very
        # early or latter part of a forecast period might be neglected. 1 assures
        # maximum sensitivity.
        if self._includeTropical:
            return 1
        else:
            return 0

    def moderated_dict(self, parmHisto, timeRange, componentName):
        # This dictionary defines the low and high limit at which
        # outliers will be removed when calculating moderated stats.
        # By convention the first value listed is the percentage
        # allowed for low values and second the percentage allowed
        # for high values.
        dict = SampleAnalysis.SampleAnalysis.moderated_dict(self, parmHisto, timeRange, componentName)
        dict["Wind"] =  (0, 20)
        if self._includeTropical:
            dict["Wind"] = (0, 15)
            dict["WindGust"] = (0, 15)
            dict["WaveHeight"] = (0, 15)
            dict["Swell"] = (0, 15)
        return dict

    # Text Rules overrides
    def _skipAreas(self, argDict):
        # These are edit areas that the formatter will skip
        return []

    def inlandWatersAreas(self, tree, node):
        # List of edit area names that are inland or bay waters
        #  as opposed to "seas"
        # The phrasing for these areas will be treated differently
        #  (see the waveRange_phrase)
        #
        # e.g.
        # return ["TampaBayWaters"]
        return ["area3"]
    
    def inlandWatersWave_element(self, tree, node):
        # Weather element first and second choice to use for reporting inland waters waves
        # "WAVES 1 TO 2 FEET."
        # If there is incomplete or no data for the first element, the second will be used.
        return ("WindWaveHgt", "WaveHeight")
    
    def seasWaveHeight_element(self, tree, node):
        # Weather element to use for reporting seas
        # "COMBINED SEAS 10 TO 15 FEET."
        # IF above wind or swell thresholds
        return "WaveHeight"

    def seasWindWave_element(self, tree, node):
        # Weather element to use for reporting seas waves
        # "WIND WAVES 3 TO 4 FEET."
        # IF above wind or swell thresholds
        return "WindWaveHgt"

    def waveHeight_wind_threshold(self, tree, node):
        # Wind value above which waveHeight (combined seas)
        # is reported vs. wind waves.
        # Also, the Swell phrase is omitted if this threshold is exceeded.
        # Unit is knots
        return 34

    def combinedSeas_threshold(self, tree, node):
        # See wave_phrase
        # If waves and swells are above this threshold,
        # combined seas will be reported AND no Swell phrase will be reported.
        # Units: feet
        return 7

    def marine_wind_flag(self, tree, node):
        # If 1, Wind wording will reflect the
        # crossing of significant thresholds such as gales.
        # E.g. "West gales to 35 knots." instead of "West winds 35 knots."
        return 0

    def marine_wind_combining_flag(self, tree, node):
        # If 1, Wind combining will reflect the
        # crossing of significant thresholds such as gales.
        # E.g. "NORTH HURRICANE FORCE WINDS TO 100 KNOTS." instead of
        # "NORTH HURRICANE FORCE WINDS TO 100 KNOTS EASING TO
        #  TO 80 KNOTS IN THE AFTERNOON."
        return 0

    def phrase_descriptor_dict(self, tree, node):
        # Descriptors for phrases
        dict = TextRules.TextRules.phrase_descriptor_dict(self, tree, node)
        dict["Wind"] = "wind"
        dict["seas"] = "combined seas"
        dict["inland waters"] = "bay and inland waters"
        dict["chop"] = "bay and inland waters"
        dict["mixed swell"] = "mixed swell"
        dict["waves"] = "wind waves"
        dict["dominant period"] = "dominant period"
        # Apply only if marine_wind_flag (see above) is set to 1:
        dict["hurricane force winds to"] = "hurricane force winds to"
        dict["storm force winds to"] = "storm force winds to"
        dict["gales to"] =  "gales to"
        dict["up to"] =  "up to"
        dict["around"] =  ""
        return dict
    
    def phrase_connector_dict(self, tree, node):
        # Dictionary of connecting phrases for various
        # weather element phrases
        # The value for an element may be a phrase or a method
        # If a method, it will be called with arguments:
        #   tree, node
        dict = TextRules.TextRules.phrase_connector_dict(self, tree, node)
        dict["rising to"] =  {
                                "Wind": "...rising to ",
                                "Swell": "...building to ",
                                "Swell2": "...building to ",
                                "WaveHeight": "...building to ",
                                "WindWaveHgt": "...building to ",
                         }
            
        dict["easing to"] =  {
                                "Wind": "...easing to ",
                                "Swell": "...subsiding to ",
                                "Swell2": "...subsiding to ",
                                "WaveHeight": "...subsiding to ",
                                "WindWaveHgt": "...subsiding to ",
                         }                 
        dict["backing"] =  {
                                "Wind": "...backing to ",
                                "Swell": "...becoming ",
                                "Swell2": "...becoming ",
                                "WaveHeight": "...becoming ",
                                "WindWaveHgt": "...becoming ",
                         }
                
        dict["veering"] =  {
                                "Wind": "...veering to ",
                                "Swell": "...becoming ",
                                "Swell2": "...becoming ",
                                "WaveHeight": "...becoming ",
                                "WindWaveHgt": "...becoming ",
                         }

        dict["becoming"] =  "...becoming "
        dict["increasing to"] =  {
                                "Wind":  "...rising to ",
                                "Swell": "...building to ",
                                "Swell2": "...building to ",
                                "WaveHeight": "...building to ",
                                "WindWaveHgt": "...building to ",
                             }
        dict["decreasing to"] =  {
                                "Wind":  "...easing to ",
                                "Swell": "...subsiding to ",
                                "Swell2": "...subsiding to ",
                                "WaveHeight": "...subsiding to ",
                                "WindWaveHgt": "...subsiding to ",
                             }
        dict["shifting to the"] =  "...shifting to the "
        dict["becoming onshore"] =  " becoming onshore "
        dict["then"] =  {"Wx": ". ",
                         "Vector": "...becoming ",
                         "Scalar": "...becoming ",
                         "otherwise": "...becoming ",
                         }  
        return dict

    def null_nlValue_dict(self, tree, node):
        # Threshold below which values are considered "null" and  not reported.
        # Units depend on the element and product
        dict = TextRules.TextRules.null_nlValue_dict(self, tree, node)
        dict["WaveHeight"] =  3  
        dict["WindWaveHgt"] =  3 
        dict["Wind"] =  5  
        dict["WindGust"] = 33 
        dict["Swell"] =  5  
        dict["Visibility"] = 3 # in nautical miles. Report if less than this value.        
        return dict
       
    def first_null_phrase_dict(self, tree, node):
        # Phrase to use if values THROUGHOUT the period or
        # in the first period are Null (i.e. below threshold OR NoWx)
        # E.g.  LIGHT WINDS.    or    LIGHT WINDS BECOMING N 5 MPH.
        dict = TextRules.TextRules.first_null_phrase_dict(self, tree, node)
        dict["WaveHeight"] =  "waves 2 feet or less"  
        dict["WindWaveHgt"] =  "wind waves 2 feet or less"  
        dict["Wind"] =  "wind variable less than 5 knots"  
        dict["Swell"] =  ""  
        return dict

    def null_phrase_dict(self, tree, node):
        # Phrase to use for null values in subPhrases other than the first
        # Can be an empty string
        #  E.g.  "NORTH WINDS 20 to 25 KNOTS BECOMING LIGHT"
        dict = TextRules.TextRules.null_phrase_dict(self, tree, node)
        dict["WaveHeight"] =  "2 feet or less"  
        dict["WindWaveHgt"] =  "2 feet or less"  
        dict["Wind"] =  "variable less than 5 knots"  
        dict["Wx"] =  ""  
        dict["Swell"] =  "light"  
        return dict

    def maximum_range_nlValue_dict(self, tree, node):
        # Maximum range to be reported within a phrase
        #   e.g. 5 to 10 mph
        # Units depend on the product
        dict = TextRules.TextRules.maximum_range_nlValue_dict(self, tree, node)
        if self._includeTropical:
            dict["Wind"] = {'default': 5,
                            (0.0, 4.0): 0,
                            (4.0, 33.0): 5,
                            (33.0, 52.0): 10,
                            (52.0, 200.0): 20,
                            }
        else:
            dict["Wind"] = 10
        dict["Swell"] = 5  
        dict["Swell2"] = 5  
        dict["WaveHeight"] = 2
        dict["WindWaveHgt"] = 2
        return dict 

    def combine_singleValues_flag_dict(self, tree, node):
        # Dictionary of weather elements to combine using single values
        # rather than ranges.  If you are using single value statistics
        # for a weather element, you will want to set this flag to 1.
        # If there is no entry for an element, min/max combining will
        # be done.
        # The value for an element may be a phrase or a method
        # If a method, it will be called with arguments:
        #   tree, node
        dict = TextRules.TextRules.increment_nlValue_dict(self, tree, node)
        dict["Wind"] = 1
        dict["WindGust"] = 1
        dict["Swell"] = 1
        dict["Swell2"] = 1
        dict["WindWaveHgt"] = 1
        dict["WaveHeight"] = 1        
        return dict

    ########################################################################
    # COMPONENT PRODUCT DEFINITIONS
    ########################################################################

    def _PoP_analysisMethod(self, componentName):
        # Alternative PoP analysis methods for consistency between PoP and Wx
        return self.stdDevMaxAvg
        #return self.maxMode
        #return self.maximum


    def addTropical(self, analysisList, phraseList, includeHazards=True):
        newAnalysisList = []
        for entry in analysisList:
            #  Sampling defined as a tuple (field, statistic, temporal rate)
            #  If this is NOT a Wind or WindGust statistic
            if entry[0] not in ["Hazards", "Wind", "WindGust", "WaveHeight", "Swell"]:
                #  Add this statistic to the new analysisList
                newAnalysisList.append(entry)
        newAnalysisList += [
                ("Wind", self.vectorModeratedMinMax, [6]),
                ("WindGust", self.moderatedMinMax, [6]),
                ("WaveHeight", self.moderatedMax, [6]),
                ("Swell", self.vectorModeratedMinMax, [6]),
                ("pws34", self.maximum),
                ("pws64", self.maximum),
                ("pwsN34", self.maximum),
                ("pwsN64", self.maximum),
                ("pwsD34", self.maximum),
                ("pwsD64", self.maximum),
                ]
        if includeHazards:
            newAnalysisList.append(("Hazards", self.discreteTimeRangesByKey))
        phraseList.insert(0, self.pws_phrase)
        return newAnalysisList, phraseList

    def CWFPeriod(self):
        analysisList = [      
                      # Use the following if you want moderated
                      # single values (e.g. N WIND 20 KT).   
                      # Set the moderating percentage in the "moderated_dict"
                      # dictionary module.
                      # Set the combine_singleValues_flag_dict in the Local file.
                          ("Wind", self.vectorModeratedMax, [6]),
                          ("Wind", self.vectorMinMax, [12]),
                          ("WindGust", self.moderatedMax, [6]),
                          ("WaveHeight", self.moderatedMax, [6]),
                          ("WindWaveHgt", self.moderatedMax, [6]),
                          ("Swell", self.vectorModeratedMax, [6]),
                          ("Swell2", self.vectorModeratedMax, [6]),
                          ("Period", self.moderatedMax, [6]),
                          ("Period2", self.moderatedMax, [6]),
                          ("Wx", self.rankedWx, [6]),
                          ("T", self.minMax),
                          ("PoP", self._PoP_analysisMethod("CWFPeriod"), [6]),
                          ("PoP", self.binnedPercent, [6]),
                          ]
        phraseList = [
                           # WINDS
                           self.marine_wind_withGusts_phrase,
                           # Alternative:
                           #self.marine_wind_phrase,
                           #self.gust_phrase,
                           # WAVES
                           self.wave_withPeriods_phrase,
                           # Alternative:
                           #self.wave_phrase,
                           # Optional:
                           self.chop_phrase,
                           # SWELLS AND PERIODS
                           self.swell_withPeriods_phrase,
                           # Alternative:
                           #self.swell_phrase,
                           #self.period_phrase,
                           # WEATHER
                           self.weather_phrase,
                           self.visibility_phrase,
                           ]
        if self._includeTropical:
            analysisList, phraseList = self.addTropical(analysisList, phraseList)        
        return {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            "analysisList": analysisList,
            "phraseList": phraseList,
            }

    def CWFPeriodMid(self):
        return {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            
            "analysisList": [            
                      # Use the following if you want moderated
                      # single values (e.g. N WIND 20 KT).   
                      # Set the moderating percentage in the "moderated_dict"
                      # dictionary module.
                          ("Wind", self.vectorModeratedMax, [6]),
                          ("Wind", self.vectorMinMax, [12]),
                          ("WindGust", self.moderatedMax, [6]),
                          ("WaveHeight", self.moderatedMax, [6]),
                          ("WindWaveHgt", self.moderatedMax, [6]),
                          ("Swell", self.vectorModeratedMax, [6]),
                          ("Swell2", self.vectorModeratedMax, [6]),
                          ("Wx", self.rankedWx, [6]),
                          ("PoP", self._PoP_analysisMethod("CWFPeriodMid"), [6]),
                          ("PoP", self.binnedPercent, [6]),
                        ],

             "phraseList":[
                           # WINDS
                           self.marine_wind_withGusts_phrase,
                           # Alternative:
                           #self.marine_wind_phrase,
                           #self.gust_phrase,
                           # WAVES
                           #self.wave_withPeriods_phrase,
                           # Alternative:
                           self.wave_phrase,
                           # Optional:
                           self.chop_phrase,
                           # SWELLS AND PERIODS
                           self.swell_withPeriods_phrase,
                           # Alternative:
                           #self.swell_phrase,
                           #self.period_phrase,
                           # WEATHER
                           self.weather_phrase,
                           self.visibility_phrase,
                           ],
            }

    def CWFExtended(self):
        return { "type": "component",
                 "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,
                          self.wordWrap,          
                          ],
                 "analysisList": [
                          ("Wind", self.vectorModeratedMax, [6]),                         
                          ("WindGust", self.moderatedMax, [12]),
                          ("WaveHeight", self.moderatedMax, [12]),
                          ("WindWaveHgt", self.moderatedMax, [12]),
                          ("Wx", self.rankedWx),
                          ("T", self.minMax),
                          ("PoP", self._PoP_analysisMethod("CWFExtended")),
                          ("PoP", self.binnedPercent),
                          ("Swell", self.vectorModeratedMax, [12]),
                          ("Swell2", self.vectorModeratedMax, [12]),
                      ],
                 "phraseList":[ 
                               # WIND
                               self.marine_wind_phrase,
                               # WAVEHEIGHT
                               #self.wave_withPeriods_phrase,
                               # Alternative:
                               self.wave_phrase,
                               # Optional:
                               self.chop_phrase,
                               # SWELLS AND PERIODS
                               #self.swell_withPeriods_phrase,
                               # Alternative:
                               self.swell_phrase,
                               #self.period_phrase,
                               # WEATHER
                               #self.weather_phrase,
                               #self.visibility_phrase,
                               ],
                }

##     def _issuance_list(self, argDict):
##         #  This method sets up configurable issuance times with associated
##         #  narrative definitions.  See the Text Product User Guide for documentation.
##         if self._definition["includeEveningPeriod"] == 1:
##             narrativeDefAM = [
##                 ("CWFPeriod", "period1"), ("CWFPeriod", 12), ("CWFPeriod", 12),
##                 ("CWFPeriodMid", 12), ("CWFPeriodMid", 12), ("CWFPeriodMid", 12),
##                 ("CWFExtended", 24), ("CWFExtended", 24)
##                 ]
##             narrativeDefPM = [
##                 ("CWFPeriod", "period1"),("CWFPeriod", 12), ("CWFPeriod", 12),
##                 ("CWFPeriodMid", 12), ("CWFPeriodMid", 12), ("CWFPeriodMid", 12),
##                 ("CWFPeriodMid", 12),
##                 ("CWFExtended", 24), ("CWFExtended", 24)
##                 ]
##         else:
##             narrativeDefAM = [
##                 ("CWFPeriod", "period1"), ("CWFPeriod", 12), ("CWFPeriod", 12),
##                 ("CWFPeriodMid", 12), ("CWFPeriodMid", 24),
##                 ("CWFExtended", 24), ("CWFExtended", 24)
##                 ]
##             narrativeDefPM = [
##                 ("CWFPeriod", "period1"), ("CWFPeriod", 12), ("CWFPeriod", 12),
##                 ("CWFPeriodMid", 12), ("CWFPeriodMid", 12), ("CWFExtended", 24),
##                 ("CWFExtended", 24), ("CWFExtended", 24)
##                 ]
        
##         return [
##             # WRS modified the "label" and issuance starthour and expiration hours
##             # early phrases
##             # note:  the start, end times and expiration times are local time
##             #
##             # note: self.DAY = 0600 Local time and self.NIGHT = 1800 Local time
##             #
##             # description -- text to appear in the startUp dialog for the product (e.g. 330 AM). 
##             # startHour -- start hour (in local time) for the first period.
##             #              These times are relative to self.DAY() and
##             #              self.NIGHT() which default to 6 and 18, respectively.  
##             # endHour -- end hour (in local time) for the first period.
##             #              These times are relative to self.DAY() and
##             #              self.NIGHT() which default to 6 and 18, respectively. The start 
##             # expirationHour -- hour when the product expires (in local time)
##             #                   This is relitive to midnight local time of the
##             #                   current day.
##             # period1 Label  -- the label for the first period. e.g. ".TODAY...", ".REST OF TODAY..." 
##             # period1 lateNight phrase -- phrase to use if the hours of 3am to 6am must be qualified
##             #                e.g. "Partly cloudy in the early morning." 
##             # period1 lateDay phrase -- phrase to use if the hours of 3pm to 6pm must be qualified
##             #                e.g. "Partly cloudy in the early evening." 
##             # todayFlag -- if 1, "TODAY" and "Tonight" phrasing will be used in subsequent periods,
##             #                otherwise, weekday wording will apply. 
##             # narrative definition -- component and time period pairs
            
##             # 330 AM Early morning issuance starts at 1200Z or when product is actually
##             # is actually issued. Ends   
##             ("230 AM", self.DAY()-4, self.NIGHT(), self.NIGHT(),  # WR 8.5
##              ".Today...", "before sunrise", "late afternoon",
##              1, narrativeDefAM),
##             ("830 AM", self.DAY()+2, self.NIGHT(), self.NIGHT(),  # WR 14.5,
##              ".Today...", "early this morning", "late afternoon",
##              1, narrativeDefAM), 
##             #  End times are tomorrow:
##             ("230 PM", self.DAY()+8, self.NIGHT()+12, self.NIGHT()+12, # WR 20.5,
##              ".Tonight...", "late tonight", "before dark",
##              1, narrativeDefPM),
##             ("830 PM", self.NIGHT()+2, 24 + self.DAY(), 24 + self.DAY(), # WR 26.5,
##              ".Tonight...", "late tonight", "before dark",
##              1, narrativeDefPM),
##             ]

    def _issuance_list(self, argDict):
        #  This method sets up configurable issuance times with associated
        #  narrative definitions.  See the Text Product User Guide for documentation.
        if self._definition["includeEveningPeriod"] == 1:
            narrativeDefAM = [
                ("CWFPeriod", "period1"),
                ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12),
                ("CWFPeriod", 12),
                ("CWFExtended", 24), ("CWFExtended", 24)
                ]
            narrativeDefPM = [
                ("CWFPeriod", "period1"),
                ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12),
                ("CWFPeriod", 12),
                ("CWFExtended", 24), ("CWFExtended", 24)
                ]
        else:
            narrativeDefAM = [
                ("CWFPeriod", "period1"),
                ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 24),
                ("CWFExtended", 24), ("CWFExtended", 24)
                ]
            narrativeDefPM = [
                ("CWFPeriod", "period1"),
                ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 24),
                ("CWFExtended", 24), ("CWFExtended", 24)
                ]

        return [        
            ("Morning", self.DAY(), self.NIGHT(), "issuanceHour + 13",
             ".TODAY...", "early", "late", 1, narrativeDefAM), 
            ("Morning with Pre-1st Period", "issuanceHour", self.NIGHT(),
             "issuanceHour + 13", ".TODAY...", "early", "late", 1,
             narrativeDefAM),
            ("Morning Update", "issuanceHour", self.NIGHT(),
             "issuanceHour + 13", ".Rest of Today...", "early in the morning",
             "late in the afternoon", 1, narrativeDefAM), 
            ("Afternoon Update", "issuanceHour", self.NIGHT(), "issuanceHour + 13",
             ".Rest of Today...", "early in the morning", "late in the afternoon",
             1, narrativeDefAM), 
            #  End times are tomorrow:
            ("Afternoon", self.NIGHT(), 24 + self.DAY(), "issuanceHour + 13",
             ".Tonight...", "late in the night", "early in the evening", 1, narrativeDefPM), 
            ("Afternoon with Pre-1st Period", "issuanceHour", 24 + self.DAY(),
             "issuanceHour + 13", ".Tonight...", "late in the night", "early in the evening", 1,
             narrativeDefPM),
            ("Evening Update", "issuanceHour", 24 + self.DAY(), "issuanceHour + 13",
             ".Rest of Tonight...", "early in the morning", "early in the evening", 1,
             narrativeDefPM),
            # For the early morning update, this produces:
            # REST OF TONIGHT:
            # MONDAY
            # MONDAY NIGHT
            ("Early Morning Update", "issuanceHour", self.DAY(), "issuanceHour + 13",
             ".Rest of Tonight...", "early in the morning", "late in the afternoon",
             0, narrativeDefPM),
            # Alternative
            # For the early morning update, this produces:
            # EARLY THIS MORNING:
            # TODAY
            # TONIGHT
            #("Evening Update", "issuanceHour", 24 + self.DAY(), "issuanceHour + 13",
            # ".REST OF TONIGHT...", "late in the night", "early in the evening",
            # 1, narrativeDefPM), 
            #("Early Morning Update", "issuanceHour", self.DAY(), "issuanceHour + 13",
            # ".EARLY THIS MORNING...", "early in the morning", "late in the afternoon",
            # 1, narrativeDefPM), 
            ]


    
    
    # Handling visibility within the weather phrase
    def visibility_wx_threshold(self, tree, node):
        # Weather will be reported if the visibility is below
        # this threshold (in NM) OR if it includes a
        # significant_wx_visibility_subkey (see below)
        return None

    def significant_wx_visibility_subkeys(self, tree, node):
        # Weather values that constitute significant weather to
        # be reported regardless of visibility.
        # If your visibility_wx_threshold is None, you do not need
        # to set up these subkeys since weather will always be
        # reported.
        # Set of tuples of weather key search tuples in the form:
        #  (cov type inten)
        # Wildcards are permitted.
        return [("* T"), ("* FS")]

    # Configurable Weather Values
    def wxCoverageDescriptors(self):
         list = TextRules.TextRules.wxCoverageDescriptors(self)
         #list.append(("Chc", "*", "*", "*", "a chance"))
         return list

    def wxTypeDescriptors(self):
         list = TextRules.TextRules.wxTypeDescriptors(self)
         #list.append( ("*", "T", "*", "Dry", "dry thunderstorms") )
         #list.append( ("*", "RW", "*", "*", "rain showers") )
         return list

    def wxAttributeDescriptors(self):
         list = TextRules.TextRules.wxAttributeDescriptors(self)
         #list.append( ("*", "T", "*", "Dry", "") )
         return list

    def wxIntensityDescriptors(self):
         list = TextRules.TextRules.wxIntensityDescriptors(self)
         #list.append(("*", "RW", "--", "*", "light"))
         return list
       
    def wxCombinations(self):
        # This is the list of which wxTypes should be combined into one.
        # For example, if ("RW", "R") appears, then wxTypes of "RW" and "R" will
        # be combined into one key and the key with the dominant coverage will
        # be used as the combined key.
        # You may also specify a method which will be
        #  -- given arguments subkey1 and subkey2 and
        #  -- should return
        #     -- a flag = 1 if they are to be combined, 0 otherwise
        #     -- the combined key to be used
        #  Note: The method will be called twice, once with (subkey1, subkey2)
        #  and once with (subkey2, subkey1) so you can assume one ordering.
        #  See the example below, "combine_T_RW"
        # 
        return [
                ("RW", "R"),
                ("SW", "S"),
                self.combine_T_RW,
            ]
    
    def combine_T_RW(self, subkey1, subkey2):
        # Combine T and RW only if the coverage of T
        # is dominant over the coverage of RW
        wxType1 = subkey1.wxType()
        wxType2 = subkey2.wxType()
        if wxType1 == "T" and wxType2 == "RW":
            order = self.dominantCoverageOrder(subkey1, subkey2)
            if order == -1 or order == 0:
                return 1, subkey1
        return 0, None

    def ExtendedLabel(self):
        return {
            "type": "component",
            "methodList": [self.setLabel],
            "analysisList": [],
            "phraseList":[],
            }
    def setLabel(self, tree, component):
        component.set("words", "\n.EXTENDED FORECAST...\n")
        return self.DONE()

    def generateForecast(self, argDict):
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
            skipAreas = self._skipAreas(argDict)
            argDict["editArea"] = (editArea, areaLabel)
            if self.currentAreaContains(argDict, skipAreas):
                continue
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
        
        # Tropical exceptions
        try:
            self._includeTropical = self._includeTropical == "Yes"
        except:
            self._includeTropical = False
        if self._includeTropical:
            self._periodCombining = 0     
            if self._productIssuance == "Morning with Pre-1st Period":
                self._productIssuance = "Morning"
            if self._productIssuance == "Afternoon with Pre-1st Period":
                self._productIssuance = "Afternoon"

        return None

    def _determineTimeRanges(self, argDict):
        # Set up the Narrative Definition and initial Time Range
        self._issuanceInfo = self.getIssuanceInfo(
            self._productIssuance, self._issuance_list(argDict))
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
        
        # Calculate current times
        self._ddhhmmTime = self.getCurrentTime(
            argDict, "%d%H%M", shiftToLocal=0, stripLeading=0)
        staticIssueTime=re.sub(r'(\d{3,4} [AP]M).*',r'\1',self._productIssuance)
        self._timeLabel =  self.getCurrentTime(
            argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1)
        # Re-calculate issueTime
        #self._issueTime = self.strToGMT(staticIssueTime)
        expireTimeRange = TimeRange.TimeRange(self._expireTime, self._expireTime + 3600)
        self._expireTimeStr = self.timeDisplay(expireTimeRange, "", "", "%d%H%M", "")
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
        fcst = fcst + self._Text1()
        try:
            text2 = self._Text2(argDict)
        except:
            text2 = self._Text2()
        fcst = fcst + text2
        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        # This is the header for an edit area combination
        print "Generating Forecast for", areaLabel
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
        fcst = fcst + headlines

        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):

        # Handle River BarForecast
        barFcst = self._getBarForecast(editArea, areaLabel, argDict)
        if barFcst != "":
            fcst = fcst + barFcst
            
        # Handle regular zone forecast
        else:
            argDict["language"] = self._language
            # Generate Narrative Forecast for Edit Area
            fcstSegment = self._narrativeProcessor.generateForecast(
                argDict, editArea, areaLabel)

            # Handle abbreviations
            if self._useAbbreviations == 1:
                fcstSegment = self.marine_abbreviateText(fcstSegment)
                fcstSegment = re.sub(r'\n', r' ',fcstSegment)
                fcstSegment = re.sub(r' (\.[A-Za-z])', r'\n\1',fcstSegment)     
                fcstSegment = self.endline(fcstSegment, linelength=self._lineLength)
            fcst = fcst + fcstSegment
            
        return fcst

    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        return fcst + "\n$$\n\n"
    
    def _postProcessProduct(self, fcst, argDict):
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst

    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################    
##     def _issuance_list(self, argDict):
##         #  This method sets up configurable issuance times with associated
##         #  narrative definitions.  See the Text Product User Guide for documentation.
##         if self._definition["includeEveningPeriod"] == 1:
##             narrativeDefAM = [
##                 ("CWFPeriod", "period1"),
##                 ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12),
##                 ("CWFPeriod", 12),
##                 ("CWFExtended", 24), ("CWFExtended", 24)
##                 ]
##             narrativeDefPM = [
##                 ("CWFPeriod", "period1"),
##                 ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12),
##                 ("CWFPeriod", 12),
##                 ("CWFExtended", 24), ("CWFExtended", 24)
##                 ]
##         else:
##             narrativeDefAM = [
##                 ("CWFPeriod", "period1"),
##                 ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 24),
##                 ("CWFExtended", 24), ("CWFExtended", 24)
##                 ]
##             narrativeDefPM = [
##                 ("CWFPeriod", "period1"),
##                 ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 12), ("CWFPeriod", 24),
##                 ("CWFExtended", 24), ("CWFExtended", 24)
##                 ]
        
##         return [
##             ("430 AM", self.DAY(), self.NIGHT(), 17,
##              ".Today...", "early in the morning", "late in the afternoon",
##              1, narrativeDefAM), 
##             ("1030 AM", "issuanceHour", self.NIGHT(), 17,
##              ".Today...", "early in the morning", "late in the afternoon",
##              1, narrativeDefAM),
##             #  End times are tomorrow:
##             ("430 PM", self.NIGHT(), 24 + self.DAY(), 24 + 5,
##              ".Tonight...", "late in the night", "early in the evening",
##              1, narrativeDefPM), 
##             ("1030 PM", "issuanceHour", 24 + self.DAY(), 24 + 5,
##              ".Tonight...", "late in the night", "early in the evening",
##              1, narrativeDefPM),
##             ]

    # Alternative issuance list using CWFPeriodMid
##    def _issuance_list(self, argDict):
##        #  This method sets up configurable issuance times with associated
##        #  narrative definitions.  See the Text Product User Guide for documentation.
##        if self._definition["includeEveningPeriod"] == 1:
##            narrativeDefAM = [
##                ("CWFPeriod", "period1"), ("CWFPeriod", 12), ("CWFPeriod", 12),
##                ("CWFPeriodMid", 12), ("CWFPeriodMid", 12),
##                ("CWFPeriodMid", 6),
##                ("CWFExtended", 24), ("CWFExtended", 24)
##                ]
##            narrativeDefPM = [
##                ("CWFPeriod", "period1"),("CWFPeriod", 12), ("CWFPeriod", 12),
##                ("CWFPeriodMid", 12), ("CWFPeriodMid", 12), ("CWFPeriodMid", 12),
##                ("CWFPeriodMid", 6),
##                ("CWFExtended", 24), ("CWFExtended", 24)
##                ]
##        else:
##            narrativeDefAM = [
##                ("CWFPeriod", "period1"), ("CWFPeriod", 12), ("CWFPeriod", 12),
##                ("CWFPeriodMid", 12), ("CWFPeriodMid", 18),
##                ("CWFExtended", 24), ("CWFExtended", 24)
##                ]
##            narrativeDefPM = [
##                ("CWFPeriod", "period1"), ("CWFPeriod", 12), ("CWFPeriod", 12),
##                ("CWFPeriodMid", 12), ("CWFPeriodMid", 12), ("CWFPeriodMid", 18),
##                ("CWFExtended", 24), ("CWFExtended", 24)
##                ]
        
##        return [
##            # WRS modified the "label" and issuance starthour and expiration hours
##            # early phrases
##            # note:  the start, end times and expiration times are local time
##            #
##            # note: self.DAY = 0600 Local time and self.NIGHT = 1800 Local time
##            #
##            # description -- text to appear in the startUp dialog for the product (e.g. 330 AM). 
##            # startHour -- start hour (in local time) for the first period.
##            #              These times are relative to self.DAY() and
##            #              self.NIGHT() which default to 6 and 18, respectively.  
##            # endHour -- end hour (in local time) for the first period.
##            #              These times are relative to self.DAY() and
##            #              self.NIGHT() which default to 6 and 18, respectively. The start 
##            # expirationHour -- hour when the product expires (in local time)
##            #                   This is relitive to midnight local time of the
##            #                   current day.
##            # period1 Label  -- the label for the first period. e.g. ".Today...", ".REST OF Today..." 
##            # period1 lateNight phrase -- phrase to use if the hours of 3am to 6am must be qualified
##            #                e.g. "Partly cloudy in the early morning." 
##            # period1 lateDay phrase -- phrase to use if the hours of 3pm to 6pm must be qualified
##            #                e.g. "Partly cloudy in the early evening." 
##            # todayFlag -- if 1, "Today" and "Tonight" phrasing will be used in subsequent periods,
##            #                otherwise, weekday wording will apply. 
##            # narrative definition -- component and time period pairs
            
##            # 330 AM Early morning issuance starts at 1200Z or when product is actually
##            # is actually issued. Ends   
##            ("230 AM", self.DAY()-4, self.NIGHT(), 17,
##             ".Today...", "before sunrise", "late afternoon",
##             1, narrativeDefAM),
##            ("830 AM", self.DAY()+2, self.NIGHT(), 17,
##             ".Today...", "early this morning", "late afternoon",
##             1, narrativeDefAM),
##            #  End times are tomorrow:
##            ("230 PM", self.DAY()+8, self.NIGHT()+12, 24+5,
##             ".Tonight...", "late tonight", "before dark",
##             1, narrativeDefPM), 
##            ("830 PM", self.NIGHT()+2, 24 + self.DAY(), 24+5,
##             ".Tonight...", "late tonight", "before dark",
##             1, narrativeDefPM),
##            ]

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
        
    def splitDay24HourLabel_flag(self, tree, node):
        # Return 0 to have the TimeDescriptor module label 24 hour periods
        # with simply the weekday name (e.g. SATURDAY)
        # instead of including the day and night periods
        # (e.g. SATURDAY AND SATURDAY NIGHT)
        # NOTE: If you set this flag to 1, make sure the "nextDay24HourLabel_flag"
        # is set to zero.
        # NOTE: This applied only to periods that are exactly 24-hours in length.
        # Periods longer than that will always be split into day and night labels
        # (e.g. SUNDAY THROUGH MONDAY NIGHT)
        compName = node.getComponentName()
        if compName == "CWFExtended":
              return 0
        else:
              return 1

    def _skipAreas(self, argDict):
        # These are edit areas that the formatter will skip
        return []
     
    # Returns a list of the Hazards allowed for this product in VTEC format.
    # These are sorted in priority order - most important first.
    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        marineActions = ["NEW", "EXA", "EXB", "EXT", "CON"]
        tropicalActions = ["NEW", "EXA", "EXB", "EXT", "UPG", "CON", 'CAN', 
          'EXP']
        return [
            ('HF.A',  marineActions, 'Marine'),  # HURRICANE FORCE WIND WATCH
            ('SR.A',  marineActions, 'Marine'),  # STORM WATCH
            ('GL.A',  marineActions, 'Marine2'),  # GALE WATCH
            ('SE.A',  marineActions, 'Marine3'),  # HAZARDOUS SEAS
            ('UP.A', marineActions, 'IceAccr'),     # HEAVY FREEZING SPRAY WATCH
            ('HF.W', marineActions, 'Marine'),      # HURRICANE FORCE WIND WARNING
            ('SR.W', marineActions, 'Marine'),      # STORM WARNING
            ('GL.W', marineActions, 'Marine2'),      # GALE WARNING
            ('SE.W', marineActions, 'Marine3'),      # HAZARDOUS SEAS
            ('UP.W', marineActions, 'IceAccr'),  # HEAVY FREEZING SPRAY WARNING
            ('HU.W', allActions, 'Hurricane'),  # HURRICANE WARNING
            ('TY.W', allActions, 'Typhoon'), # TYPHOON WARNING
            ('TR.W', allActions, 'Tropical'), # TROPICAL STORM WARNING
            ('HU.A', allActions, 'Hurricane'),  # HURRICANE WATCH
            ('TY.A', allActions, 'Typhoon'), # TYPHOON WATCH
            ('TR.A', allActions, 'Tropical'), # TROPICAL STORM WATCH
            ('UP.Y', marineActions, 'IceAccr'),  # HEAVY FREEZING SPRAY ADVISORY
            ('MF.Y', marineActions, 'Fog'),        # MARINE DENSE FOG ADVISORY
            ('MS.Y', marineActions, 'Smoke'),      # MARINE DENSE SMOKE ADVISORY
            ('MH.W', marineActions, 'Ashfall'),    # MARINE VOLCANIC ASHFALL WARNING
            ('MH.Y', marineActions, 'Ashfall'),    # MARINE VOLCANIC ASHFALL ADVISORY
            ('RB.Y', marineActions, 'Marine'),   # ROUGH BAR
            ('SI.Y', marineActions, 'Marine1'),   # SMALL CRAFT ADVISORY
            ('SC.Y', marineActions, 'Marine2'),       # SMALL CRAFT ADVISORY
            ('SW.Y', marineActions, 'Marine3'),       # SMALL CRAFT ADVISORY
            ('BW.Y', marineActions, 'Marine4'),       # BRISK WIND ADVISORY
            ('TO.A', marineActions, 'Convective'), # TORNADO WATCH
            ('SV.A', marineActions, 'Convective'), # SEVERE THUNDERSTORM WATCH
            ('LO.Y', marineActions, 'LowWater'),   # LOW WATER ADVISORY
            ]
