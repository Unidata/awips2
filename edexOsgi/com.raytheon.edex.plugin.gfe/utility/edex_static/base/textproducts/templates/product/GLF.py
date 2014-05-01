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
# Description: This product creates a Great Lakes Forecast product.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# GLF, GLF_<site>_<MultiPil>_Definition, GLF_<site>_Overrides
#-------------------------------------------------------------------------
# Customization Points:
#
# DEFINITION SECTION
#
# Required Configuration Items:
#
#  displayName      If not None, defines how product appears in GFE GUI
#  defaultEditAreas defines edit areas...see "Edit Areas Needed" section below
#                   for all edit areas needed. Just uncomment the lines below the
#                   lake the formatter will be run for...and comment
#                   out the lines below the lakes not used.
#  productName      defines name of product e.g. "OPEN LAKES FORECAST"
#  fullStationID    Full station identifier, 4 letter, such as "KSLC".
#  wmoID            WMO ID code for product header, such as "FOUS45"
#  pil              Product pil, such as "GLFLS"
#  areaName (opt.)  Area name for product header, such as "LAKE SUPERIOR"
#  wfoCityState     WFO location, such as "BUFFALO NY"
#  lake_name        Name of lake...not including the word "LAKE" e.g "SUPERIOR"
#  lakezone         Zone code for the text portion of the forecast e.g. "LSZ260"
#  maforzone        Zone code for the mafor portion of the forecast e.g. "LSZ261"
#  headerphrase     Phrase for the header portion of forecast immediately above
#                   the SYNOPSIS section.
# Optional Configuration Items
#  database               Source database for product. Can be "Official", 
#                         "Fcst" or "ISC"
#  outputFile             Defines the output location of the finished product
#                         when saved from Formatter Launcher.
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
#  useAbbreviations (default == 1)
#      If 1, use marine abbreviations e.g. TSTM instead of THUNDERSTORM, NW instead of NORTHWEST
#      (See marine_abbreviateText in the TextRules module)
#  processMafor (default == 1)
#      1 --> The MAFOR code will be processed
#      0 --> The MAFOR code will not be processed
#  useHolidays              Set to 1 to use holidays in the time period labels
#
#  Weather-related flags
#       hoursSChcEnds        - specifies hours past the beginning of the first
#                              first period of the product to stop including 'Slight
#                               Chance' or 'Isolated' weather types (ERH policy
#                               allows values of 1-5 * 12 hour periods)               
#
#  Trouble-shooting items
#    passLimit -- Limit on passes allowed through Narrative Tree
#    trace     -- Set to 1 to turn on trace through Narrative Tree   
##
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
#  To 5 days:
#  Grids need to be continuous:
#    Wind (5 days)
#    WaveHeight (5 days)
#    Wx (5 days)
#    T (36 hours)
#    Optional:
#       WindGust (5 days)
#-------------------------------------------------------------------------
# Edit Areas Needed:  left side for E-W oriented lakes: right side for
#                     N-S oriented lakes: "whole lake" edit area needed
#                     for all lakes.
#   west_half               (north_half)
#   east_half               (south_half)
#   east_one_third          (south_one_third)
#   west_one_third          (north_one_third)
#   east_two_thirds         (south_two_thirds)
#   west_two_thirds         (north_two_thirds)
#   east_one_quarter        (south_one_quarter)
#   west_one_quarter        (north_one_quarter)
#   east_three_quarters     (south_three_quarters)
#   west_three_quarters     (north_three_quarters)
#   "whole lake" -- name the edit area the name of the entire lake (e.g. SUPERIOR, ST_CLAIR, HURON, etc)
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Component Products:
#   GLFFcstFirst
#   GLFFcstShort
#   GLFFcstExt
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
#
# To look up tasks and their status, see the Text Product User Guide
# Section on "Tkgnats: Task Reporting System".
#-------------------------------------------------------------------------
# Component Products:
#   GLFFcstShort
#   GLFFcstExt
#-------------------------------------------------------------------------
# Additional Information:
#
#     from ConfigVariables:
#       maximum_range_nlValue_dict
#       minimum_range_nlValue_dict
#       phrase_descriptor_dict
#       scalar_difference_nlValue_dict
#-------------------------------------------------------------------------
# Example Output:
#
##    FOUS43 KMQT 301825
##    GLFLS
##    LSZ260-310300

##    OPEN LAKE FORECAST FOR LAKE SUPERIOR
##    NATIONAL WEATHER SERVICE MARQUETTE MI
##    125 PM EST WED OCT 30 2002

##    LAKE SUPERIOR FORECAST BEYOND FIVE NM FROM SHORE

##    .SYNOPSIS...

##    WEST HALF

##    ...GALE WARNING IN EFFECT...

##    .TONIGHT...NW GALES TO 45 KT EASING TO GALES TO 40
##    KT LATE IN THE NIGHT. CHANCE OF SNOW SHOWERS. WAVES 7 TO 10 FT
##    SUBSIDING TO 6 TO 9 FT.
##    .THU...W WIND 15 TO 25 KT. SNOW SHOWERS LIKELY. WAVES 4
##    TO 7 FT SUBSIDING TO 4 TO 6 FT.
##    .THU NIGHT...W WIND 15 TO 25 KT VEERING NW WIND TO
##    30 KT AFTER MIDNIGHT. SNOW SHOWERS LIKELY. WAVES 4 TO 6 FT.
##    .FRI...NW WIND TO 30 KT. RAIN AND SNOW LIKELY.
##    WAVES 4 TO 7 FT.
##    .FRI NIGHT...NW WIND 15 TO 25 KT BACKING W WIND TO
##    30 KT IN THE LATE EVENING AND OVERNIGHT. RAIN AND SNOW LIKELY.
##    WAVES 5 TO 8 FT.
##    .SAT...W WIND TO 30 KT VEERING NW. WAVES 4 TO
##    7 FT SUBSIDING TO 3 TO 5 FT.
##    .SUN...NW WIND 10 TO 20 KT BACKING W 5 TO 15 KT
##    IN THE EVENING. WAVES 3 TO 5 FT.
##    .MON...W WIND 5 TO 15 KT AFTER MIDNIGHT VEERING NW
##    BACKING W UP TO 10 KT IN THE LATE MORNING AND EARLY AFTERNOON
##    BACKING SW BACKING S 5 TO 15 KT IN THE EVENING. WAVES
##    2 TO 4 FT BUILDING TO 3 TO 5 FT.

##    EAST HALF

##    ...GALE WARNING IN EFFECT...

##    .TONIGHT...NW GALES TO 45 KT EASING TO GALES TO 40
##    KT LATE IN THE NIGHT. CHANCE OF SNOW SHOWERS. WAVES 7 TO 10 FT
##    SUBSIDING TO 6 TO 9 FT.
##    .THU...W WIND 15 TO 25 KT. SNOW SHOWERS LIKELY. WAVES 4
##    TO 7 FT SUBSIDING TO 4 TO 6 FT.
##    .THU NIGHT...W WIND 10 TO 20 KT VEERING NW 15 TO
##    25 KT IN THE LATE EVENING AND OVERNIGHT. SNOW SHOWERS LIKELY.
##    WAVES 4 TO 6 FT BUILDING TO 6 TO 9 FT.
##    .FRI...NW WIND TO 30 KT. RAIN AND SNOW LIKELY.
##    WAVES 6 TO 9 FT BUILDING TO 7 TO 10 FT.
##    .FRI NIGHT...NW WIND 15 TO 25 KT BACKING W WIND TO
##    30 KT AFTER MIDNIGHT. RAIN AND SNOW LIKELY. WAVES 7 TO 10 FT
##    SUBSIDING TO 6 TO 9 FT.
##    .SAT...W WIND TO 30 KT VEERING NW. WAVES 6 TO
##    9 FT SUBSIDING TO 5 TO 8 FT.
##    .SUN...N WIND 15 TO 25 KT BACKING NW 10 TO 20
##    KT. WAVES 4 TO 7 FT.
##    .MON...NW WIND 10 TO 20 KT EASING TO UP TO 10 KT
##    EARLY IN THE AFTERNOON BACKING SE VEERING S 5 TO 15 KT
##    LATE IN THE EVENING. WAVES 4 TO 7 FT.

##    $$

##    LSZ261-310300-
##    MAFOR 3022/
##    SUPERIOR WEST 1/2...GALE WARNING IN EFFECT...13760 11640 11620 13630
##    220610 

##    SUPERIOR EAST 1/2...GALE WARNING IN EFFECT...13760 11740 12620 12630
##    220610 

##    $$

#

import TextRules
import SampleAnalysis
import ForecastNarrative
import time, string
import os, re, types
import TimeRange, AbsTime

class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = [
             # W-E orientation
             (("Groupings", "groupings"), "West 1/2:East 1/2", "radio",
                          ["West 1/2:East 1/2", "West 1/3:East 2/3", "West 2/3:East 1/3",
                           "West 1/4:East 3/4", "West 3/4:East 1/4", "Entire Lake"]),
             # N-S orientation
             #(("Groupings", "groupings") , "North 1/2:South 1/2", "radio",
             #             ["North 1/2:South 1/2", "North 1/3:South 2/3", "North 2/3:South 1/3",
             #              "North 1/4:South 3/4", "North 3/4:South 1/4", "Entire Lake"]),
             ]

    Definition =  {
        "type": "smart",
        "displayName": "None",
        # Source database for product. Can be "Official", "Fcst" or "ISC"
        "database": "Official",
        # Defines output location of finished product.
        "outputFile": "{prddir}/TEXT/GLF_<MultiPil>.txt",
        "debug": 0,
        
        "lineLength": 66,
        ## Edit Areas
        "defaultEditAreas" : [("west_half", "WEST HALF\n\n"),
                              ("east_half", "EAST HALF\n\n")],
        # product identifiers
        "lake_name": "SUPERIOR", # use -- SUPERIOR, HURON, ERIE, ONTARIO, MICHIGAN, ST_CLAIR
        "productName": "OPEN LAKES FORECAST", # product name 
        "fullStationID": "<fullStationID>",    # full station identifier (4letter)
        "wmoID": "<wmoID>",          # WMO ID
        "pil": "<pil>",            # Product pil
        "areaName": "STATENAME",   # Name of state, such as "GEORGIA"
        "wfoCityState": "<wfoCityState>",  # Location of WFO - city state
        "textdbPil": "<textdbPil>",       # Product ID for storing to AWIPS text database.
        "awipsWANPil": "<awipsWANPil>",   # Product ID for transmitting to AWIPS WAN.

        "fixedExpire": 1,        #ensure VTEC actions don't affect segment expiration time

        "hazardSamplingThreshold": (10, None),  #(%cov, #points)
        
        "headerphrase": "LAKE SUPERIOR FORECAST BEYOND FIVE NAUTICAL MILES FROM SHORE", # header phrase
        "lakezone": "LSZ260",       # Zone code for the Lake
        "maforzone": "LSZ261",       # Mafor zone code
        "processmafor" : 1, # process mafor data: 1=yes, 0=no

        "periodCombining" : 0,      # If 1, combine periods, if possible

        "useAbbreviations": 1,      # Use marine abbreviations

        # Area Dictionary -- Descriptive information about zones
        "areaDictionary": "AreaDictionary", 
        "useHolidays": 0,            # Set to 1 to use holidays in the time period labels

        # Weather-related flags
        "hoursSChcEnds": 24,
        
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
    # OVERRIDING THRESHOLDS AND VARIABLES
    ########################################################################

    ### THRESHOLDS AND VARIABLES
    ### Analysis Class
    ### To override, override the associated method in your text product class.
    def temporalCoverage_threshold(self, parmHisto, timeRange, componentName):
        # Replaces IN_RANGE_THRESHOLD -- Note that this threshold is now used
        #    differently i.e. it is the percentage of the TIMERANGE covered by the
        #    grid in order to include it in the analysis
        # Percentage of temporal coverage default value (if not found in temporalCoverage_dict)
        # Used by temporalCoverage_flag
        return  5.0

    def temporalCoverage_dict(self, parmHisto, timeRange, componentName):
        # Replaces IN_RANGE_DICT -- Note that this these thresholds are now used
        return {
            "LAL": 0,
            "MinRH": 0,
            "MaxRH": 0,
            "MinT": 50,
            "MaxT": 10,
            "Haines": 0,
            "Wx": 15,
            "PoP" : 50,
            }

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
    
    def vector_mag_difference_nlValue_dict(self, tree, node):
        # Replaces WIND_THRESHOLD
        # Magnitude difference.  If the difference between magnitudes
        # for sub-ranges is greater than this value,
        # the different magnitudes will be noted in the phrase.
        # Units can vary depending on the element and product
        return  {
            "Wind": 10,   
            "Wind20ft": 10,   
            "TransWind": 10,   
            "FreeWind": 10,   
            "Swell": 1,  # ft
            "Swell2": 1,  # ft
            }

    
    def vector_dir_difference_dict(self, tree, node):
        # Direction difference.  If the difference between directions
        # for 2 sub-periods is greater than this value,
        # the different directions will be noted in the phrase.
        # Units are degrees
        return {
            "Wind": 50, # degrees
            "TransWind": 60,  # mph
            "FreeWind": 60,  # mph
            "Swell":60, # degrees
            "Swell2":60, # degrees
            }    

    def maxReported_threshold_dict(self, tree, node):
        # Winds will not be reported above this value:
        # For example, if set to 30, all winds above 30 will
        # be reported as:
        #   "Winds up to 30 knots."
        return {
            "Wind": 200, # knots or mph depending on product
            }

    def null_nlValue_dict(self, tree, node):
        # Threshold below which values are considered "null" and  not reported.
        # Units depend on the element and product
        dict = TextRules.TextRules.null_nlValue_dict(self, tree, node)
        dict["Wind"] =  7   
        return dict
       
    def first_null_phrase_dict(self, tree, node):
        # Phrase to use if values THROUGHOUT the period or
        # in the first period are Null (i.e. below threshold OR NoWx)
        # E.g.  LIGHT WINDS.    or    LIGHT WINDS BECOMING N 5 MPH.
        dict = TextRules.TextRules.first_null_phrase_dict(self, tree, node)
        dict["Wave"] =  "variable winds less than 10 knots"  
        return dict

    def null_phrase_dict(self, tree, node):
        # Phrase to use for null values in subPhrases other than the first
        # Can be an empty string
        #  E.g.  "NORTH WINDS 20 to 25 KNOTS BECOMING LIGHT"
        dict = TextRules.TextRules.null_phrase_dict(self, tree, node)
        dict["Wind"] =  "variable less than 10 knots"  
        dict["Wx"] =  ""  
        return dict

    def gust_wind_difference_nlValue(self, tree, node):
        # Difference between gust and maxWind below which gusts are not mentioned
        # Units are mph
        return 15

    def maximum_range_nlValue_dict(self, tree, node):
        # Maximum range to be reported within a phrase
        #   e.g. 5 to 10 mph
        # Units depend on the product
#        return {}
        return {
            "Wind": 10,
            }
 
    def marine_wind_flag(self, tree, node):
        # If 1, Wind combining and wording will reflect the
        # crossing of significant thresholds such as gales 
        return 1

    def marine_wind_combining_flag(self, tree, node):
        # If 1, Wind combining will reflect the
        # crossing of significant thresholds such as gales.
        # E.g. "HURRICANE FORCE WINDS TO 100 KNOTS." instead of
        # "NORTH HURRICANE FORCE WINDS TO 100 KNOTS EASING TO
        #  HURRICANE FORCE WINDS TO 80 KNOTS IN THE AFTERNOON."
        return 1

    def marine_abbreviateText(self, fcst):
        fcst = " " + string.upper(fcst)
        fcst = re.sub(r'\n', r' ',fcst)        
        fcst = re.sub(r'NORTH(?!WARD|ERN|WESTWARD|EASTWARD|WESTERN|EASTERN)',
                      r'N', fcst)
        fcst = re.sub(r'SOUTH(?!WARD|ERN|WESTWARD|EASTWARD|WESTERN|EASTERN)',
                      r'S', fcst)
        fcst = re.sub(r'EAST(?!WARD|ERN)', r'E', fcst)
        fcst = re.sub(r'WEST(?!WARD|ERN)', r'W', fcst)
        fcst = re.sub(r'KNOTS?', r'KT', fcst)
        fcst = re.sub(r'MILLIBARS?', r'MB', fcst)
        fcst = re.sub(r'FATHOMS?', r'FM', fcst)
        fcst = re.sub(r'NAUTICAL MILES?', r'NM', fcst)
        fcst = re.sub(r'FOOT|FEET', r'FT', fcst)
        fcst = string.replace(fcst, 'POSITION', 'PSN') 
        fcst = string.replace(fcst, 'VISIBILITY', 'VSBY') 
        fcst = string.replace(fcst, 'THUNDERSTORM', 'TSTM') 
        fcst = string.replace(fcst, 'AVERAGE', 'AVG') 
        fcst = string.replace(fcst, 'ATLANTIC', 'ATLC') 
        fcst = string.replace(fcst, 'LONGITUDE', 'LONG') 
        fcst = string.replace(fcst, 'PACIFIC', 'PAC') 
        fcst = string.replace(fcst, 'DEGREE', 'DEG') 
        fcst = string.replace(fcst, 'PRESSURE', 'PRES')
        fcst = string.replace(fcst, 'SUNDAY', 'SUN')
        fcst = string.replace(fcst, 'MONDAY', 'MON')
        fcst = string.replace(fcst, 'TUESDAY', 'TUE')
        fcst = string.replace(fcst, 'WEDNESDAY', 'WED')
        fcst = string.replace(fcst, 'THURSDAY', 'THU')
        fcst = string.replace(fcst, 'FRIDAY', 'FRI')
        fcst = string.replace(fcst, 'SATURDAY', 'SAT')
        fcst = string.replace(fcst, 'W HALF', 'WEST HALF')
        fcst = string.replace(fcst, 'E HALF', 'EAST HALF')
        fcst = string.replace(fcst, 'N HALF', 'NORTH HALF')
        fcst = string.replace(fcst, 'S HALF', 'SOUTH HALF')
        fcst = string.replace(fcst, 'W THIRD', 'WEST THIRD')
        fcst = string.replace(fcst, 'E THIRD', 'EAST THIRD')
        fcst = string.replace(fcst, 'N THIRD', 'NORTH THIRD')
        fcst = string.replace(fcst, 'S THIRD', 'SOUTH THIRD')
        fcst = string.replace(fcst, 'W TWO', 'WEST TWO')
        fcst = string.replace(fcst, 'E TWO', 'EAST TWO')
        fcst = string.replace(fcst, 'N TWO', 'NORTH TWO')
        fcst = string.replace(fcst, 'S TWO', 'SOUTH TWO')
        fcst = re.sub(r'^ ', r'',fcst)        
        return fcst
  
    def rounding_method_dict(self, tree, node):
        # Special rounding methods
        #
        return {
            "Wind": self.marineRounding,
            }

    def waveHeight_words(self, tree, node):
        "Create phrase for waves"
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "WaveHeight")
        if stats is None:
            nodataPhrase = self.noWaveHeight_phrase(
                tree, node, "WaveHeight", "WaveHeight")
            return self.setWords(node.parent, nodataPhrase)

        min, max = self.getValue(stats, "MinMax")
        #avg = (min + max)/2
        words = self.wave_range(max)            
        return self.setWords(node, words)

    def wave_range(self, avg):
        # Make wave ranges based off the average wave value
        table = ((2, "2 feet or less"), (3, "1 to 3 feet"),
                 (4, "2 to 4 feet"), (5, "3 to 5 feet"),
                 (6, "4 to 6 feet"), (8, "5 to 8 feet"), 
                 (9, "6 to 9 feet"), (10, "7 to 10 feet"),
                 (11, "8 to 11 feet"), (12, "9 to 12 feet"),
                 (14, "10 to 14 feet"), (17, "12 to 17 feet"),
                 (20, "15 to 20 feet"), (25, "20 to 25 feet"),
                 (30, "25 to 30 feet"), (100, "over 30 feet"))
        range = ""
        for max, str in table:
            if avg <= max:
                range = str
                break
        return range

    ########################################################################
    # COMPONENT PRODUCT DEFINITIONS
    ########################################################################

    def _PoP_analysisMethod(self, componentName):
        # Alternative PoP analysis methods for consistency between PoP and Wx
        return self.stdDevMaxAvg
        #return self.maxMode
        #return self.maximum

    def GLFFcstFirst(self):

        return {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            "analysisList": [
                       ("Wind", self.vectorMinMax, [3]),
                       ("WindGust", self.minMax, [3]),
                       ("Wx", self.rankedWx, [6]),
                       ("T", self.minMax),
                       ("WaveHeight", self.minMax, [3]),
                       ("PoP", self._PoP_analysisMethod("GLFFcstFirst"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                      ],
            "phraseList":[
                   self.marine_wind_withGusts_phrase,
                   self.weather_orSky_phrase,
                   self.visibility_phrase,
                   self.severeWeather_phrase,
                   self.waveHeight_phrase,
                  ],
            }


    def GLFFcstShort(self):
        return {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            "analysisList": [
                       ("Wind", self.vectorMinMax, [3]),
                       ("WindGust", self.minMax, [3]),
                       ("Wx", self.rankedWx, [6]),
                       ("T", self.minMax),
                       ("WaveHeight", self.minMax, [3]),
                       ("PoP", self._PoP_analysisMethod("GLFFcstShort"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                      ],
            "phraseList":[
                   self.marine_wind_withGusts_phrase,
                   self.weather_orSky_phrase,
                   self.visibility_phrase,
                   self.severeWeather_phrase,
                   self.waveHeight_phrase,
                   self._warnOutlook_phrase,
                  ],
            }

    def GLFFcstExt(self):
        return {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            "analysisList": [
                       ("Wind", self.vectorMinMax, [3]),
                       ("Wx", self.rankedWx, [6]),
                       ("T", self.minMax),
                       ("WaveHeight", self.minMax, [6]),
                       ("PoP", self._PoP_analysisMethod("GLFFcstExt"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                      ],
            "phraseList":[
                   self.marine_wind_phrase,
                   self.weather_phrase,
                   self.visibility_phrase,
                   self.waveHeight_phrase,
                  ],
            }

    def generateForecast(self, argDict):
        # Get variables
        error = self._getVariables(argDict)
        if error is not None:
            return error

        # Get the edit areas
        try:
            if self._groupings == "West 1/3:East 2/3":
                self._areaList = [("west_one_third", "WEST THIRD"),
                            ("east_two_thirds", "EAST TWO THIRDS")]
            elif self._groupings == "West 2/3:East 1/3":
                self._areaList = [("west_two_thirds", "WEST TWO THIRDS"),
                            ("east_one_third", "EAST ONE THIRD")]
            elif self._groupings == "West 1/4:East 3/4":
                self._areaList = [("west_one_quarter", "WEST QUARTER"),
                            ("east_three_quarters", "EAST THREE QUARTERS")]
            elif self._groupings == "West 3/4:East 1/4":
                self._areaList = [("west_three_quarters", "WEST THREE QUARTERS"),
                            ("east_one_quarter", "EAST ONE QUARTER")]
            elif self._groupings == "Entire Lake":
                self._areaList = [(self._lake_name, "")]
            elif self._groupings == "West 1/2:East 1/2":
                self._areaList = [("west_half", "WEST HALF"), ("east_half", "EAST HALF")]
            elif self._groupings == "North 1/3:South 2/3":
                self._areaList = [("north_one_third", "NORTH THIRD"),
                            ("south_two_thirds", "SOUTH TWO THIRDS")]
            elif self._groupings == "North 2/3:South 1/3":
                self._areaList = [("north_two_thirds", "NORTH TWO THIRDS"),
                            ("south_one_third", "SOUTH ONE THIRD")]
            elif self._groupings == "North 1/4:South 3/4":
                self._areaList = [("north_one_quarter", "NORTH QUARTER"),
                            ("south_three_quarters", "SOUTH THREE QUARTERS")]
            elif self._groupings == "North 3/4:South 1/4":
                self._areaList = [("north_three_quarters", "NORTH THREE QUARTERS"),
                            ("south_one_quarter", "SOUTH ONE QUARTER")]
            elif self._groupings == "Entire Lake":
                self._areaList = [(self._lake_name, "")]
            elif self._groupings == "North 1/2:South 1/2":
                self._areaList = [("north_half", "NORTH HALF"), ("south_half", "SOUTH HALF")]
            else:
                self._areaList = [(self._lake_name, "")]
        except:
            self._areaList = [(self._lake_name, "")]            
        if len(self._areaList) == 0:
            return "WARNING -- No Edit Areas Specified to Generate Product."

        # determine time ranges for MAFOR
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

            # make sure outlook flag is set to 0
            self._outlookflag = 0

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

        # Get VariableList
        self._groupings = None
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"

        self._language = argDict["language"]

        # Initialize mafor list
        self._mafors = []
        return None

    def _determineTimeRanges(self, argDict):
        # Set up the Narrative Definition and initial Time Range
        self._issuanceInfo = self.getIssuanceInfo(
            self._productIssuance, self._issuance_list(argDict))
        self._timeRange = self._issuanceInfo.timeRange()
        argDict["productTimeRange"] = self._timeRange
        self._issueTime = self._issuanceInfo.issueTime()
        self._expireTime = self._issuanceInfo.expireTime()
        self._expireTimeDDHHMM = time.strftime("%d%H%M", 
          time.gmtime(self._expireTime.unixTime()))
        self._definition["narrativeDef"] = self._issuanceInfo.narrativeDef()
        if self._periodCombining:
            self._definition["methodList"] = \
               [self.combineComponentStats, self.assembleChildWords]
        else:
            self._definition["methodList"] = [self.assembleChildWords]
        self._definition["priorPeriod"] = 24

        # Set up self._headlineRange
        self._headlineRange =  TimeRange.TimeRange(self._timeRange.startTime(),
                                      self._timeRange.startTime() + 24*3600)


        trDict = {}
        trList = []
        currentLocalTime, shift = self.determineTimeShift()
        day = currentLocalTime.day
        month = currentLocalTime.month
        year = currentLocalTime.year
        startTime = AbsTime.absTimeYMD(year, month, day)

        if self._productIssuance == "400 AM":
            start = self.localTime(startTime, 8, shift)
            wxstart = self.localTime(startTime, 6, shift)
            wxend = self.localTime(startTime, 18, shift)
        if self._productIssuance == "1000 AM":
            start = self.localTime(startTime, 14, shift)
            wxstart = self.localTime(startTime, 10, shift)
            wxend = self.localTime(startTime, 18, shift)
        if self._productIssuance == "400 PM":
            start = self.localTime(startTime, 20, shift)
            wxstart = self.localTime(startTime, 18, shift)
            wxend = self.localTime(startTime, 30, shift)
        if self._productIssuance == "1000 PM":
            start = self.localTime(startTime, 26, shift)
            wxstart = self.localTime(startTime, 22, shift)
            wxend = self.localTime(startTime, 30, shift)
        # MAFOR config
        timeRange = TimeRange.TimeRange(start, start + 3600)
        periods = self.getPeriods(timeRange, 3, 1, 8)
        # coded winds and waves
        for i in range(0,8):
           trList.append(periods[i])
           trDict["MAFOR"+`i`] = periods[i][0]

        self._trDict = trDict
        self._trList = trList

        # worded weather times in mafor
        self._wxtimerange1 = TimeRange.TimeRange(wxstart, wxend)
        self._wxtimerange2 = TimeRange.TimeRange(wxend, wxend+(3600*12))

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
            argDict, self._definition, self._timeRange, 
            self._areaList, self._issuanceInfo)
        if error is not None:
            return error
        print "AREA LIST: ", self._areaList
        self._sampler = self.getSampler(argDict, 
          (self._MAFORAnalysisList(), self._trList, self._areaList))
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
               self._ddhhmmTime + "\n" + self._pil + '\n' + \
               self._lakezone + "-" + self._expireTimeDDHHMM + "-\n\n" +\
               productName +  "\n" +\
               "NATIONAL WEATHER SERVICE " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n\n" + \
               self._headerphrase + "\n\n" + ".SYNOPSIS..." + "\n\n"

        # Set up hazards
        self.getHazards(argDict, self._areaList)
        return fcst
 
    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        # This is the header for an edit area combination
        fcst = fcst + areaLabel+ "\n\n"       

        # Headlines
        # get the hazards text
        self._hazards = argDict['hazards']
        self._combinations = argDict["combinations"]

        headlines = self.generateProduct("Hazards", argDict, area = editArea,
                                         areaLabel=areaLabel,
                                         timeRange = self._timeRange)
        fcst = fcst + headlines


        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):

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

        # mafor stuff

        # grab headline in mafor
        maforheadline = self.generateProduct("MaforHeadline", argDict,
                                             area = editArea, areaLabel=areaLabel,
                                             timeRange = self._headlineRange)

        maforheadline = string.replace(maforheadline, "\n", "")

        if self._processmafor == 1:
            mafor = self._makeMAFOR(editArea, areaLabel, self._trDict,
                       self._trList, self._MAFORAnalysisList(), maforheadline, argDict, areaLabel)
            self._mafors.append(mafor)
        ##
        return fcst

    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):

        # Adjust some phrases to local requirments
        # ========================================
        fcst = string.replace(fcst,"WIDESPREAD RAIN", "OCCASIONAL RAIN")
        fcst = string.replace(fcst,"WIDESPREAD SHOWERS", "SHOWERS")
        fcst = string.replace(fcst,"WIDESPREAD THUNDERSTORMS", "THUNDERSTORMS")

        fcst = string.replace(fcst, "RAIN SHOWERS", "SHOWERS")
        fcst = string.replace(fcst, "THUNDERSTORMS AND SHOWERS", "SHOWERS AND THUNDERSTORMS")
        #phrase = string.replace(phrase, "widespread", "")

        # This is the footer for an edit area combination
        return fcst + "\n"

    def _postProcessProduct(self, fcst, argDict):

        fcst = fcst + "$$\n\n"

        if string.find(fcst, "STORM FORCE") > 0 or\
           string.find(fcst, "STORM WARNING") > 0 or\
           string.find(fcst, "HURRICANE") > 0:
            fcst = fcst + "&&STORM\n\n"

        if self._processmafor == 1:
            maforzone = self._maforzone+"-"+ self._expireTimeDDHHMM + "-\n"
            maforissue = "MAFOR " + self._getMaforTime(argDict) + "/" + "\n"

            fcst = fcst + maforzone + maforissue
            for mafor in self._mafors:
                fcst = fcst + mafor + "\n"
            fcst = fcst + "$$\n\n"

        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")        
        return fcst

    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################

    def _warnOutlook_phrase(self):
        return {
            "phraseMethods": [
                self._warnOutlook_words, # phrase.words
            ],
            }    
    def _warnOutlook_words(self, tree, phrase):
        # will put an outlook phrase in the text

        timeRange = phrase.getTimeRange()
        windStats = tree.stats.get("Wind", timeRange, mergeMethod="Max")
        if windStats is None:
            return self.setWords(phrase, "")
        
        max, dir = windStats
        words = ""
        if max >= 34 and (self._outlookflag == 0):
            words = "a gale warning may be needed"
            self._outlookflag = 1
        if max >= 48 and (self._outlookflag == 0 or self._outlookflag == 1):
            words = "a storm warning may be needed"
            self._outlookflag = 2
        if max < 34:
            words  = ""
            self._outlookflag = 0
        return self.setWords(phrase, words)
    
    def _issuance_list(self, argDict):
        #  This method sets up configurable issuance times with associated
        #  narrative definitions.  See the Text Product User Guide for documentation.
        narrativeDefAM = [
            ("GLFFcstFirst", "period1"), ("GLFFcstFirst", 12), ("GLFFcstShort", 12), ("GLFFcstShort", 12), 
            ("GLFFcstExt", 18), ("GLFFcstExt", 24), ("GLFFcstExt", 24), 
            ]
        narrativeDefPM = [
            ("GLFFcstFirst", "period1"), ("GLFFcstFirst", 12), ("GLFFcstShort", 12), ("GLFFcstShort", 12), 
            ("GLFFcstShort", 12),
            ("GLFFcstExt", 18), ("GLFFcstExt", 24), ("GLFFcstExt", 24), 
            ]        
        return [
            ("400 AM", self.DAY(), self.NIGHT(), 16,
             ".Today...", "early in the morning", "late in the afternoon",
             1, narrativeDefAM),
            ("1000 AM", "issuanceHour", self.NIGHT(), 16,
             ".Rest of Today...", "early in the morning", "late in the afternoon",
             1, narrativeDefAM),
            #  End times are tomorrow:
            ("400 PM", self.NIGHT(), 24 + self.DAY(), 24 + 4,
             ".Tonight...", "late in the night", "early in the evening",
             1, narrativeDefPM),
            ("1000 PM", "issuanceHour", 24 + self.DAY(), 24 + 4,
             ".Rest of Tonight...", "late in the night", "early in the evening",
             1, narrativeDefPM),
            ]

    def lateDay_descriptor(self, tree, node, timeRange):
        # If time range is in the first period, return period1 descriptor for
        #  late day -- default 3pm-6pm
        if self._issuanceInfo.period1TimeRange().contains(timeRange):
            return self._issuanceInfo.period1LateDayPhrase()
        else:
            return "late in the afternoon"
        
    def lateNight_descriptor(self, tree, node, timeRange):
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
        if compName == "GLFFcstExt":
              return 0
        else:
              return 1

#    def _getExpTime(self, argDict):
#        # get exp time for EST
#        currentTime = argDict['creationTime']
#        curday = time.strftime("%d", time.localtime(currentTime))
#        nextday = time.strftime("%d",time.localtime(currentTime + 86400))
#        timezone = time.strftime("%Z", time.localtime(currentTime))
#
#        if self._productIssuance == "400 AM":
#            if timezone == "EST" or timezone == "CST":
#                return curday + "1500"
#            else:
#                return curday + "1400"
#        elif self._productIssuance == "1000 AM":
#            if timezone == "EST" or timezone == "CST":
#                return curday + "2100"
#            else:
#                return curday + "2000"
#        elif self._productIssuance == "400 PM":
#            if timezone == "EST" or timezone == "CST":
#                return nextday + "0300"
#            else:
#                return nextday + "0200"
#        elif self._productIssuance == "1000 PM":
#            if timezone == "EST" or timezone == "CST":
#                return nextday + "0900"
#            else:
#                return nextday + "0800"
#
    def significant_wx_visibility_subkeys(self, tree, node):
        # Weather values that constitute significant weather to
        # be reported regardless of visibility.
        # If your visibility_wx_threshold is None, you do not need
        # to set up these subkeys since weather will always be
        # reported.
        # Set of tuples of weather key search tuples in the form:
        #  (cov type inten)
        # Wildcards are permitted.
        return [("* *")]

    #################################
    #  MAFOR code        
              
    def WxMAFOR(self):
        return {
            "type":"component",
            "methodList": [
                          self.assemblemaforPhrases,   
                          self.wordWrap,
                          ],
            "analysisList": [
                       #("Wind", self.vectorMinMax),
                       #("WaveHeight", self.minMax),
                       ("Wx", self.rankedWx, [6]),
                       ("T", self.minMax),
                       ("PoP", self._PoP_analysisMethod("WxMAFOR"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                      ],
            "phraseList":[
                     self.weather_phrase,
                     ],
            }

    def WaveMAFOR(self):
        return {
            "type":"component",
            "methodList": [
                          self.assemblemaforPhrases,   
                          self.wordWrap,
                          ],
            "analysisList": [
                       #("Wind", self.vectorMinMax),
                       #("WaveHeight", self.minMax),
                       ("WaveHeight", self.minMax, [6]),
                      ],
            "phraseList":[
                     self.waveHeight_phrase,
                     ],
            }

    def assemblemaforPhrases(self, tree, component):
        # Assemble component phrases and add Label
        # Qualify the phrases with local effect qualifiers
        #  if present.
        #   e.g. "near the coast"
        for phrase in component.get("childList"):
            words = phrase.get("words")
            if words is None:
                return
        fcst = ""
        lastQualifier = None
        lastPhrase = None
        for phrase in component.get("childList"):
            words = phrase.get("words")
            if words is None:
                return
            words, lastQualifier = self.qualifyWords(
                phrase, words, "conjunctiveQualifier", lastQualifier, lastPhrase)
            lastPhrase = phrase
            fcst = fcst + words
        # Add label
        curLocalTime, shift = self.determineTimeShift()
        issuanceInfo = tree.get("issuanceInfo")
        index = component.getIndex()
        label = self.createmaforLabel(component.get("timeRange"),
                                      issuanceInfo, curLocalTime, shift, index)
        if fcst == "":
            label = ""
        return self.setWords(component, fcst + label)

    def createmaforLabel(self, timeRange, issuanceInfo, currentLocalTime, shift, index=0):
        # Make a label given the timeRange in GMT and the shift to
        # convert it to local time. currentLocalTime can be used to
        # compare to current day.

        if timeRange.duration() <= 3600:
                return ""
        if index == 0:
            try:
                label =  issuanceInfo.period1Label()
                if label != "":
                    return label
            except:
                pass
        try:
            today =  issuanceInfo.todayFlag()
        except:
            today = 1
        label =  self.getWeekday(timeRange, holidays=0, shiftToLocal=1,
                                labelType="Combo", today=today,
                                tomorrow=0)        
        return label

    def maforheadline_phrase(self):
        return {
            "phraseMethods": [
                self.maforheadline_words, # phrase.words
            ],
            }
    def maforheadline_words(self, tree, phrase):
        timeRange = phrase.getTimeRange()
        waveHeightStats = tree.stats.get("WaveHeight", timeRange, mergeMethod="Max") 
        windStats = tree.stats.get("Wind", timeRange, mergeMethod="Max") 
        if waveHeightStats is None or windStats is None:
            return self.setWords(phrase,"@")

        # Look at max waveHeight
        waveheightMax = int(waveHeightStats)

        # Look at the max wind for the period
        windMax, dir = windStats
        #print "wind", windStats, windMax, argDict["timeRange"]
       
        words = ""
        if windMax >= 64:
            words = "...hurricane force wind warning in effect..."
        elif windMax >= 48:
            words = "...storm warning in effect..."
        elif windMax >= 34:
            words = "...gale warning in effect..."
#        elif windMax >= 21:
#            words = "\n\n...small craft advisory in effect.."
#        elif waveheightMax >= 5:
#            words = "\n\n...small craft advisory in effect.."
        else:
            words = ""
        return self.setWords(phrase, words)

    def _MAFORAnalysisList(self):
        return [
            ("Wind", self.vectorMinMax),
            ("WindGust", self.minMax),
            ("WaveHeight", self.minMax),
            ("Wx", self.dominantWx),
            ("T", self.minMax),
            ]

    def MaforHeadline(self):
        return {
            "type":"component",
            "methodList": [
                          self.assembleChildWords,   
                          self.wordWrap,
                          ],
            "analysisList": [
                       ("Wind", self.vectorMinMax),
                       ("WaveHeight", self.minMax),
                      ],
            "phraseList":[
                     self.maforheadline_phrase
                     ],
            }

    def _makeMAFOR(self, editArea, areaList, trDict, trList, analysis, maforheadline, argDict, areaLabel):
        
        MAFOR = []

        for i in range (0,8):
            statDict = self.getStatDict(self._sampler, analysis,
                                        trDict["MAFOR"+ `i`], editArea)
            windstats = self.getStats(statDict, "Wind")

            statDict2 = self.getStatDict(self._sampler, analysis,
                                        trDict["MAFOR"+ `i`], editArea)
            wxstats = self.getStats(statDict2, "Wx")
            
            statDict3 = self.getStatDict(self._sampler, analysis,
                                        trDict["MAFOR"+ `i`], editArea)
            Tstats = self.getStats(statDict3, "T")

            statDict4 = self.getStatDict(self._sampler, analysis,
                                        trDict["MAFOR"+ `i`], editArea)
            Guststats = self.getStats(statDict4, "WindGust")

            if Guststats is None:
                Gustmax = 0
            else:
                Gustmax = self.getValue(Guststats, "Max")
            
            if Tstats is None:
                Tmin = 32
            else:
                Tmin, Tmax = Tstats

            # wind mafor code
            
            if windstats == "None" or windstats is None:
                windcode = "MMMM"
                maxspd = 0
            else:
                windspd, winddir = windstats
                minspd, maxspd = windspd
                windcode = self._findwind(winddir, maxspd)

            # weather mafor code #

            if wxstats is None or wxstats == []:
                wxcode = "M" 
            else:
                subkeyList = wxstats
                wxcode = self._wxcode(maxspd, Gustmax, Tmin, subkeyList)

            code = windcode + wxcode
            MAFOR.append(code)
            
        code1 = code2 = code3 = code4 = code5 = code6 = code7 = code8 = ''

        first = 0
        next = 1
        time = 1
        mafor = ""
        ok = 1
        # sort the code #
        while first < 8:
            while ok == 1:
                if next > 7:
                    final = first
                    first = next
                    ok = 0
                    break
                if MAFOR[first] == MAFOR[next]:
                    first = first
                    next = next + 1
                    time = time + 1
                    ok = 1
                else:
                    final = first
                    first = next
                    next = next + 1
                    ok = 0
                    break

            mafor = self._makeCode(mafor, time, MAFOR[final])
            ok = 1
            time = 1
            
        mafor = mafor + "."
        mafor = string.replace(mafor, " .", ". ")

        # wx wording #

        wxmafor1 = self.generateProduct("WxMAFOR", argDict,
                                        area = editArea, areaLabel=areaLabel,
                                        timeRange = self._wxtimerange1)
        wxmafor1 = string.replace(wxmafor1, "\n", "")
        wxmafor1 = string.replace(wxmafor1, ". .", " ")
        wxmafor1 = string.replace(wxmafor1, "..", " ")
        if wxmafor1 != "":
            wxmafor1 = wxmafor1 + ". "

        wxmafor2 = self.generateProduct("WxMAFOR", argDict,
                                        area = editArea, areaLabel=areaLabel,
                                        timeRange = self._wxtimerange2)
        wxmafor2 = string.replace(wxmafor2, "\n", "")
        wxmafor2 = string.replace(wxmafor2, ". .", " ")
        wxmafor2 = string.replace(wxmafor2, "..", " ")
        if wxmafor2 != "":
            wxmafor2 = wxmafor2 + ". "

        wxmafor = wxmafor1 + wxmafor2
        
        mafor = mafor + wxmafor

        # wave wording #

        wavemafor1 = self.generateProduct("WaveMAFOR", argDict,
                                        area = editArea, areaLabel=areaLabel,
                                        timeRange = self._wxtimerange1)
        wavemafor1 = string.replace(wavemafor1, "\n", "")
        wavemafor1 = string.replace(wavemafor1, "..", " ")
        wavemafor1 = string.replace(wavemafor1, ". .", " ")
        wavemafor1 = wavemafor1 + ". "

        wavemafor2 = self.generateProduct("WaveMAFOR", argDict,
                                        area = editArea, areaLabel=areaLabel,
                                        timeRange = self._wxtimerange2)
        wavemafor2 = string.replace(wavemafor2, "\n", "")
        wavemafor2 = string.replace(wavemafor2, ". .", " ")
        wavemafor2 = string.replace(wavemafor2, "..", " ")
        wavemafor2 = wavemafor2 + ". "

        wavemafor = wavemafor1 + wavemafor2
        
        mafor = mafor + wavemafor

        # waves mafor code #
        waveMAFOR = []
        for j in range (0,2):
            statDict2 = self.getStatDict(self._sampler, analysis,
                                         trDict["MAFOR"+ `j`], editArea)
            wavestats = self.getStats(statDict2, "WaveHeight")
            if wavestats is "None" or wavestats is None:
                #mafor = mafor + " " + "MMMMM"
                wavecode = "MMMM"
                waveMAFOR.append(wavecode)
            else:
                min, max = wavestats
                wavecode, minimum, maximum = self._findwave(max)
                waveMAFOR.append(wavecode)

        if waveMAFOR[0] == waveMAFOR[1]:
            mafor = mafor + "22" + waveMAFOR[0] + ". "
        else:
            mafor = mafor + "21" + waveMAFOR[0] + " " +\
                "21" + waveMAFOR[1] + ". "

        if maforheadline == "":
            connector = " "
        else:
            connector = ""

        mafor = self._lake_name + " " + areaList + connector +  maforheadline + mafor
        mafor = string.replace(mafor, "_", " ")
        mafor = string.replace(mafor, "HALF", "1/2")
        mafor = string.replace(mafor, "THREE QUARTERS", "3/4")
        mafor = string.replace(mafor, "QUARTER", "1/4")
        mafor = string.replace(mafor, "TWO THIRDS", "2/3")
        mafor = string.replace(mafor, "THIRD", "1/3")
        mafor = string.replace(mafor, "LAKE SUPERIOR", "")

        mafor = self.linebreak(mafor, 69)

        return mafor
    def _findwind(self, winddir, windspd):

        winddir = self.dirToText(winddir)
        minspd = 7 #min wind speed that direction is included in mafor

        if windspd<=minspd:
            dir = `9`

        if winddir == "N":
            dir = `8`
        elif winddir == "NE":
            dir = `1`
        elif winddir == "E":
            dir = `2`
        elif winddir == "SE":
            dir = `3`
        elif winddir == "S":
            dir = `4`
        elif winddir == "SW":
            dir = `5`
        elif winddir == "W":
            dir = `6`
        elif winddir == "NW":
            dir = `7`
        else:
            dir = `9`

        if windspd > 60:
            spd = `9`
        if windspd <= 60:
            spd = `8`
        if windspd <= 55:
            spd = `7`
        if windspd <= 48:
            spd = `6`
        if windspd <= 35:
            spd = `5`
        if windspd <= 33:
            spd = `4`
        if windspd <= 25:
            spd = `3`
        if windspd <= 20:
            spd = `2`
        if windspd <= 15:
            spd = `1`
        if windspd <= 10:
            spd = `0`

        return dir + spd
 
############
#  weather mafor code
############

    def _wxcode(self, windspd, Gust, T, subkeyList):

        index = 0

        length = len(subkeyList)
        for wxKey in subkeyList:
            wxType = wxKey.wxType()
            cov = wxKey.coverage()
            vis = wxKey.visibility()

            number = "0"

            if wxType == "ZY" and T >= 23 and T < 32:
                number = "1"

            if wxType == "ZY" and T < 23:
                number = "2"

            if wxType == "WG":
                if cov == "Def" or cov == "Wide" or cov == "Areas":
                    number = "3"

            if wxType == "WG":
                if vis == "1/4SM" or vis == "0SM":
                    if cov == "Def" or cov == "Wide" or cov == "Areas":
                        number = "4"

            if wxType == "L":
                if cov == "Like" or cov == "Ocnl" or cov == "Def" or cov == "Wide" or cov == "Num" or cov == "Areas":
                    number = "5"

            if wxType == "R" or wxType == "RW":
                if cov == "Like" or cov == "Ocnl" or cov == "Def" or cov == "Wide" or cov == "Num" or cov == "Areas":
                    number = "6"

            if wxType == "S" or wxType == "SW":
                if cov == "Like" or cov == "Ocnl" or cov == "Def" or cov == "Wide" or cov == "Num" or cov == "Areas":
                    number = "7"

            if (Gust - windspd) > 16:
                if wxType == "SW" or wxType == "RW" or wxType == "T":
                    number = "8"            

            if wxType == "T":
                if cov == "Like" or cov == "Ocnl" or cov == "Def" or cov == "Wide" or cov == "Num" or cov == "Areas":
                    number = "9"            
                        
            index = index + 1
            if index <= length:
                break

        return number


    def _makeCode(self, mafor, time, wind):

        if time <=4:
            code = "1" + `time` + wind
        if time == 5:
            code = "14" + wind + " 11" + wind
        if time == 6:
            code = "15" + wind
        if time == 7:
            code = "15" + wind + " 11" + wind
        if time == 8:
            code = "16" + wind

#        mafor = mafor + " " + code
        mafor = mafor + code + " "

        return mafor

############
#  wave mafor code
############

    def _findwave(self, avg):

        if avg == 0:
            range = "0002"
            min=0
            max=2
        elif avg <= 2:
            range = "0002"
            min=0
            max=2
        elif avg > 2 and avg <= 3:
            range = "0103"
            min=1
            max=3
        elif avg > 3 and avg <= 4:
            range = "0204"
            min=2
            max=4
        elif avg > 4 and avg <= 5:
            range = "0305"
            min=3
            max=5
        elif avg > 5 and avg <= 6:
            range = "0306"
            min=3
            max=6
        elif avg > 6 and avg <= 7:
            range = "0407"
            min=4
            max=7
        elif avg > 7 and avg <= 8:
            range = "0508"
            min=5
            max=8
        elif avg > 8 and avg <= 10:
            range = "0610"
            min=6
            max=10
        elif avg > 10 and avg <= 12:
            range = "0812"
            min=8
            max=12
        elif avg > 12 and avg <= 14:
            range = "1014"
            min=10
            max=14
        elif avg > 14 and avg <= 16:
            range = "1216"
            min=12
            max=16
        elif avg > 16 and avg <= 18:
            range = "1418"
            min=14
            max=18
        elif avg > 18 and avg <= 20:
            range = "1520"
            min=15
            max=20
        elif avg > 20 and avg <= 23:
            range = "1823"
            min=18
            max=23
        elif avg > 23 and avg <= 25:
            range = "2025"
            min=20
            max=25
        elif avg > 25:
            range = "2530"
            min=25
            max=30
        else:
            range = "MMMM"
            min=0
            max=0

        return range, min, max
       

    def _getMaforTime(self, argDict):
        # get mafor time
        currentTime = argDict['creationTime']
        curday = time.strftime("%d", time.localtime(currentTime))
        nextday = time.strftime("%d",time.localtime(currentTime + 86400))
        timezone = time.strftime("%Z", time.localtime(currentTime))

        if self._productIssuance == "400 AM":
            if timezone == "EST" or timezone == "CST":
                return curday + "10"
            else:
                return curday + "09"
        elif self._productIssuance == "1000 AM":
            if timezone == "EST" or timezone == "CST":
                return curday + "16"
            else:
                return curday + "15"
        elif self._productIssuance == "400 PM":
            if timezone == "EST" or timezone == "CST":
                return curday + "22"
            else:
                return curday + "21"
        elif self._productIssuance == "1000 PM":
            if timezone == "EST" or timezone == "CST":
                return curday + "14"
            else:
                return nextday + "03"
                
    def timePeriod_descriptorTable(self, tree, node):
        # Contains definition for localtime start/end times and phrase
        # Tuples, 0=startHrLT, 1=endHrLT, 2=phrase
        day = self.DAY()
        return [
                (day, (day+3)%24, "early in the morning"),    # 6-9
                (day, (day+6)%24, "in the morning"),          # 6-12
                (day, (day+9)%24, "until late afternoon"),    # 6-15
                (day, (day+12)%24, ""),                       # 6-18
                ((day+3)%24, (day+6)%24, "late in the morning"), # 9-12
                ((day+3)%24, (day+9)%24, "around midday"), # 9-15
                ((day+3)%24, (day+12)%24, "by noon"),      # 9-18
                ((day+6)%24, (day+9)%24,  "early in the afternoon"),  # 12-15
                ((day+6)%24, (day+12)%24, "in the afternoon"),        # 12-18
                ((day+9)%24, (day+12)%24, "late in the afternoon"),   # 15-18
                ((day+12)%24, (day+15)%24, "early in the evening"),   # 18-21
                ((day+12)%24, (day+18)%24, "in the evening"),         # 18-0
                ((day+12)%24, (day+21)%24, "through early morning"),    # 18-3
                ((day+12)%24, day, ""),                               # 18-6
                ((day+15)%24, (day+18)%24, "late in the evening"),                     # 21-0
                ((day+15)%24, (day+21)%24, "around midnight"),   # 21-3
                ((day+15)%24, day, "overnight"),               # 21-6
                ((day+18)%24, (day+21)%24, "after midnight"),               # 0-3
                ((day+18)%24, day, "after midnight"),                       # 0-6
                ((day+18)%24, (day+6)%24, ""),                              # 0-12
                ((day+21)%24, day, "early in the morning"),                 # 3-6
                ]

    def phrase_descriptor_dict(self, tree, node):
        # Dictionary of descriptors for various weather elements in phrases
        # The value for an element may be a phrase or a method
        # If a method, it will be called with arguments:
        #   tree, node, key, element
        dict = TextRules.TextRules.phrase_descriptor_dict(self, tree, node)
        dict["Wind"] = "winds"
        dict["WindGust"] = "gusts up to"
        dict["WaveHeight"] = "waves"  
        dict["hurricane force winds to"]= "hurricane force winds to"
        dict["storm force winds to"] = "storm force winds to"
        dict["gales to"] = "gales to"
        dict["up to"] = ""
        dict["around"] = ""
        #  Used for Headlines
        dict["EXPECTED"] = "EXPECTED"
        dict["IN EFFECT"] = "IN EFFECT"
        return dict
             
    def scalar_difference_nlValue_dict(self, tree, node):
        # Scalar difference.  If the difference between scalar values
        # for 2 sub-periods is greater than or equal to this value,
        # the different values will be noted in the phrase.
        dict = TextRules.TextRules.scalar_difference_nlValue_dict(self, tree, node)
        dict["WaveHeight"] = {
            (0, 6) : 1,
            (6, 20) : 5,
            'default': 10,
            }
        return dict

    def minimum_range_nlValue_dict(self, tree, node):
        # This threshold is the "smallest" min/max difference allowed between values reported.
        # For example, if threshold is set to 5 for "MaxT", and the min value is 45
        # and the max value is 46, the range will be adjusted to at least a 5 degree
        # range e.g. 43-48.  These are the values that are then submitted for phrasing
        # such as:
        #   HIGHS IN THE MID 40S 
        dict = TextRules.TextRules.minimum_range_nlValue_dict(self, tree, node)
        dict["Wind"] = {
            (0, 5) : 0,   # will be reported as "null"
            (5, 8) : 5,
            "default" : 10,
            }
        return dict

    # Returns a list of the Hazards allowed for this product in VTEC format.
    # These are sorted in priority order - most important first.
    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        marineActions = ["NEW", "EXA", "EXB", "EXT", "CON"]
        return [
            ('HF.A', marineActions, 'Marine'), # HURRICANE FORCE WIND WATCH
            ('SR.A', marineActions, 'Marine'), # STORM WATCH
            ('GL.A', marineActions, 'Marine'), # GALE WATCH
            ('SE.A', marineActions, 'Marine'), # HAZARDOUS SEAS WATCH
            ('UP.A', allActions, 'IceAccr'), # HEAVY FREEZING SPRAY WATCH
            ('HF.W', marineActions, 'Marine'), # HURRICANE FORCE WIND WARNING
            ('SR.W', marineActions, 'Marine'), # STORM WARNING
            ('GL.W', marineActions, 'Marine'), # GALE WARNING
            ('SE.W', marineActions, 'Marine'), # HAZARDOUS SEAS WARNING
            ('UP.W', allActions, 'IceAccr'), # HEAVY FREEZING SPRAY WARNING
            ('TO.A', allActions, 'Convective'), # TORNADO WATCH
            ('SV.A', allActions, 'Convective'), # SEVERE THUNDERSTORM WATCH
            ('MH.W', allActions, 'Ashfall'), # VOLCANIC ASHFALL WARNING
            ('MH.Y', allActions, 'Ashfall'), # VOLCANIC ASHFALL ADVISORY
            ('MF.Y', allActions, 'Fog'), # DENSE FOG ADVISORY
            ('MS.Y', allActions, 'Smoke'), # DENSE SMOKE ADVISORY
            ('LO.Y', allActions, 'LowWater'), # LOW WATER ADVISORY
            ]
