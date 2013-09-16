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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Phrase_Test_Local
#  Local customizations for AreaFcst as Base class for testing Narrative Phrases
#  Refer to Test Cases for Text Products: tp003
# Author:
# ----------------------------------------------------------------------------

import AreaFcst
import string
import TextRules
import string, time, re, os, types, copy

class TextProduct(AreaFcst.TextProduct):
    Definition = copy.deepcopy(AreaFcst.TextProduct.Definition)
 
    # REQUIRED CONFIGURATION ITEMS
    Definition['displayName'] = "TEST_PhraseTest"
    #Definition["outputFile"] = "/awips/GFESuite/products/TEXT/ZFP.txt"

    # Header configuration items
    #Definition["productName"] = "ZONE FORECAST PRODUCT"  # name of product
    #Definition["fullStationID"] = "Kxxx"  # full station identifier (4letter)
    #Definition["wmoID"] = "FOUS45"        # WMO ID
    #Definition["pil"] = "ZFPxxx"          # product pil
    #Definition["areaName"] = "STATENAME"  # Name of state, such as "GEORGIA"
    #Definition["wfoCity"] = "WfoCity"     # Location of WFO - city name
    #Definition["wfoState"] = "WfoState"   # Location of WFO - state name

    # OPTIONAL CONFIGURATION ITEMS
    Definition["defaultEditAreas"] = [
                                      ("area3", "Area 1"),
    #                                  ("area2", "Area 2"),
                                     ]

    Definition["Period_1_version"] = 1
 
    #Definition["directiveType"] = "C11"
    #Definition["directiveType"] = "10-503"     # Can be "C11"
    #Definition["includeFloodingQuestion"] = 1  # Set to 1 to include flooding question

    #Definition["includeMultipleElementTable"] = 1       # Will include a TempPoPTable
    #Definition["cityDictionary"] = "CityDictionary"     # For TempPoPTable
 
    #Definition["areaDictionary"] = "AreaDictionary"     # For product headers
    #Definition["language"] = "english"       
    #Definition["hoursSChcEnds"] = 0   
 
    # Apply to C11 only:
    #Definition["includeExtended"] = 1        # To include extended forecast
    #Definition["extendedLabel"] = 1          # To include extended label
    #Definition["includeEveningPeriod"] = 0   # To turn off evening period

    #Definition["includeMultipleElementTable"] = 1       # Will include a MultipleElementTable
    # Uncomment just one elementList below
    #Definition["elementList"] = ["Temp", "PoP"]         # Default
    #Definition["elementList"] = ["Temp", "Humidity", "PoP"]
    #Definition["singleValueFormat"] = 1                 # Default is 0
 
    # Sampling Performance
    #Definition["sampleFromServer"] = 1       # If 1, sample directly from server
    # Trouble-shooting items
    #Definition["passLimit"] = 20             # Limit on passes allowed through
                                              # Narrative Tree
    #Definition["trace"] = 1                   # Set to 1 to turn on trace through
                                              # Narrative Tree for trouble-shooting
    #Definition["debug"] = 1

    Definition["arealSkyAnalysis"] = 1
 
    def __init__(self):
        AreaFcst.TextProduct.__init__(self)

    # OPTIONAL OVERRIDES
    #def DAY(self):
    #    return  6
    #def NIGHT(self):
    #    return 18

    # The thresholds and variables included here were selected because
    # they are commonly overridden for your product.
    # See the Text Product User Guide for other thresholds and variables
    # that may be relevant to your product and for more information
    # about the ones included here.
 
    def phrase_descriptor_dict(self, tree, node):
        # Descriptors for phrases
        dict = TextRules.TextRules.phrase_descriptor_dict(self, tree, node)
        # This is the default. Triggers if ALL coverage terms are areal
        #dict["PoP"] = self.allAreal_or_chance_pop_descriptor,
        # Uncomment this line for invoking areal or chance pop descriptor
        #    Triggers if ANY coverage terms are areal
        #dict["PoP"] = self.areal_or_chance_pop_descriptor,
        # Uncomment this line to use "chance" descriptor in all cases
        #dict["PoP"] = "chance of"
        return dict

    def pop_snow_lower_threshold(self, tree, node):
        # Snow accumulation will not be reported if Pop is below this threshold
        return 60

    def pop_snowLevel_upper_threshold(self, tree, node):
        # Snow level will be reported if Pop is above this threshold
        return 60

    def snowLevel_maximum_phrase(self, tree, node):
        # This returns the maximum snow level value to be reported and the
        # the corresponding snow level phrase. It can be set up by
        # edit area as follows:
        # editAreaList = [
        #    ("area1", 8000, "above 8000 feet"),
        #    ("area2", 6000, "above 6000 feet"),
        #    # Don't mention snow level at all in area3:
        #    ("area3", 0, ""),
        #    ]
        #maxElev = 0
        #phrase = ""
        #for area, elev, elevPhrase in editAreaList:
        #    if self.currentAreaContains(tree, [area]):
        #        if elev > maxElev:
        #            maxElev = elev
        #            phrase = elevPhrase
        #return (maxElev, phrase)
        return (8000, "above 8000 feet")
   
    def vector_mag_difference_dict(self, tree, node):
        # Replaces WIND_THRESHOLD
        # Magnitude difference.  If the difference between magnitudes
        # for the first and second half of a period is greater than this value,
        # the different magnitudes will be noted in the phrase.
        # Units can vary depending on the element
        dict = TextRules.TextRules.vector_mag_difference_dict(self, tree, node)
        #dict["Wind"] = 20
        return dict

    def scalar_difference_nlValue_dict(self, tree, node):
        # Scalar difference.  If the difference between scalar values
        # for 2 sub-periods is greater than this value,
        # the different values will be noted in the phrase.
        dict = TextRules.TextRules.scalar_difference_nlValue_dict(self, tree, node)
        #dict["WindGust"] = 20
        return dict

    def lake_wind_areaNames(self, tree, node):
        # Return list of edit area names for which the lake_wind_phrase
        # should be generated
        # If you want the phrase potentially generated for all zones, use:
        # return ["ALL"]
        return []

    def useWindsForGusts_flag(self, tree, node):
        # Turn this on if you want to use the maximum Wind
        # for reporting Gusts if a WindGust grid is not found
        return 0
 
    def range_threshold_nlValue_dict(self, tree, node):
        # Range for reporting temperature ranges in temp_range_phrase
        #   e.g HIGHS 80 TO 85
        dict = TextRules.TextRules.range_threshold_nlValue_dict(self, tree, node)
        dict["MaxT"] = 5
        dict["MinT"] =  5
        dict["MinRH"] =  5
        dict["MaxRH"] =  5
        return dict
 
    def temp_trend_nlValue(self, tree, node):
        # THRESHOLD FOR REPORTING TEMPERATURE TRENDS
        return 20.0

    def stdDev_dict(self, parmHisto, timeRange, componentName):
        # This dictionary defines the low and high limit at which
        # outliers will be removed when calculating stdDev stats.
        # These tuples represent the (low, high) number of standard
        # deviations.  Any values falling outside this range will
        # not be included in the calculated statistic.
        return {
                "LAL": (1.0, 1.0),
                "MinRH":  (1.0, 1.0),
                "MaxRH":  (1.0, 1.0),
                "MinT": (1.0, 1.0),
                "MaxT": (1.0, 1.0),
                "Haines": (1.0, 1.0),
                "PoP" : (1.0, 1.0),
                "T" : (1.0, 1.0),
                "Wind" : (1.0, 1.0),
                }

    def value_connector_dict(self, tree, node):
        dict = TextRules.TextRules.value_connector_dict(self, tree, node)
        dict["MaxT"] = " to "
        dict["MinT"] = " to "
        return dict
 
    def windChillTemp_difference(self, tree, node):
        # Difference between wind chill and temperature
        # for reporting wind chill
        return 5

    def heatIndexTemp_difference(self, tree, node):
        # Difference between heat index and temperature
        # for reporting heat index
        return 5

    def Period_1(self):
        exec "value = self.Period_1_version" + `self._Period_1_version` + "()"
        return value
 
    def Period_1_version1(self):
        component =  {
            "type": "component",
            "methodList": [
                          self.orderPhrases,
                          self.consolidateSubPhrases,
                          self.assemblePhrases,
                          self.wordWrap,
                          ],
            "analysisList": [
                       ("MinT", self.stdDevMinMax),
                       ("MaxT", self.stdDevMinMax),
                       ("T", self.hourlyTemp),
                       ("T", self.minMax),
                       ("Sky", self.median, [3]),
                       ("PoP", self._PoP_analysisMethod("Period_1"), [3]),
                       ("PoP", self.binnedPercent, [3]),
                       ("Wind", self.vectorModeratedMinMax, [0]),
                       ("Wind", self.vectorMinMax, [0]),
                       ("WindGust", self.maximum, [0]),
                       ("Wx", self.rankedWx, [3]),
                       ("WindChill", self.minMax),
                       ("HeatIndex", self.minMax),
                       ("SnowAmt", self.accumMinMax),
                       ],
            "phraseList":[
                   self.sky_phrase,
                   self.skyPopWx_phrase,
##                   (self.skyPopWx_phrase, self._wxLocalEffects_list()),
                   self.wind_summary,
                   self.reportTrends,
                   self.weather_phrase,
##                   (self.weather_phrase,self._wxLocalEffects_list()),
                   self.heavyPrecip_phrase,
                   self.severeWeather_phrase,
                   self.visibility_phrase,
                   self.snow_phrase,
                   self.extremeTemps_phrase,
                   self.steady_temp_trends,
                   self.highs_phrase,
                   self.lows_phrase,
                   self.temp_trends,
                   self.wind_withGusts_phrase,
                   self.popMax_phrase,
                   self.windChill_phrase,
                   self.heatIndex_phrase,
                   ],
##            "additionalAreas": [
##                   # Areas listed by weather element that will be
##                   # intersected with the current area then
##                   # sampled and analysed.
##                   # E.g. used in local effects methods.
##                   ("Sky", ["Rush_Valley"]),
##                   ("Wx",  ["Rush_Valley"]),
##                   ("PoP", ["Rush_Valley"]),
##             ],
           }
        if self._arealSkyAnalysis:
            component["analysisList"].append(("Sky", self.binnedPercent, [3]))
        if self._useStormTotalSnow:
            phraseList = component["phraseList"]
            index = phraseList.index(self.total_snow_phrase)
            phraseList[index] = self.stormTotalSnow_phrase
            component["phraseList"] = phraseList
        return component
 
    def _wxLocalEffects_addlArea_list(self):
        leArea1 = self.LocalEffectArea("__Current__", "", intersectFlag=0)
        leArea2 = self.LocalEffectArea("Rush_Valley", "in the Rush Valley",
                                       intersectFlag=0)
        return [self.LocalEffect([leArea1, leArea2], 10, "...except ")]
 
    def _10_503_issuance_list(self, argDict):
        seriesDefAM = [
            ("Period_1", "period1"), #("Phantom", 12),
##            ("Period_2_3", 12), ("Period_2_3", 12),
##            ("Period_4_5", 12), ("Period_4_5", 12),
##            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12),
##            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12),
            ]
        seriesDefPM = [
            ("Period_1", "period1"),
            ("Period_2_3", 12), ("Period_2_3", 12),
            ("Period_4_5", 12), ("Period_4_5", 12),
            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12),
            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12),
            ("Period_6_14", 12),
            ]
 
        return [
            ("Morning", self.DAY(), self.NIGHT(), self.NIGHT(),
             ".TODAY...", "early in the morning", "late in the afternoon",
             1, seriesDefAM),
            ("Morning with Pre-1st Period", self.DAY()-2, self.NIGHT(), self.NIGHT(),
             ".TODAY...", "early in the morning", "late in the afternoon",
             1, seriesDefAM),
            ("Morning Update", "issuanceHour", self.NIGHT(), self.NIGHT(),
             ".REST OF TODAY...", "early in the morning", "late in the afternoon",
             1, seriesDefAM),
            ("Afternoon Update", "issuanceHour", self.NIGHT(), self.NIGHT(),
             ".REST OF TODAY...", "early in the morning","late in the afternoon",
             1, seriesDefAM),
            #  End times are tomorrow:
            ("Afternoon", self.NIGHT(), 24 + self.DAY(), 24 + self.DAY(),
             ".TONIGHT...", "late in the night", "early in the evening",
             1, seriesDefPM),
            ("Afternoon with Pre-1st Period", self.NIGHT()-2, 24 + self.DAY(), 24 + self.DAY(),
             ".TONIGHT...", "late in the night", "early in the evening",
             1, seriesDefPM),
            ("Evening Update", "issuanceHour", 24 + self.DAY(), 24 + self.DAY(),
             ".REST OF TONIGHT...", "early in the morning","early in the evening",
             1, seriesDefPM),
            # For the early morning update, this produces:
            # REST OF TONIGHT:
            # MONDAY
            # MONDAY NIGHT
            ("Early Morning Update", "issuanceHour", self.DAY(), self.DAY(),
             ".REST OF TONIGHT...", "early in the morning","late in the afternoon",
             0, seriesDefPM),
            # Alternative
            # For the early morning update, this produces:
            # EARLY THIS MORNING:
            # TODAY
            # TONIGHT
            #("Evening Update", "issuanceHour", 24 + self.DAY(), 24 + self.DAY(),
            # ".REST OF TONIGHT...", "late in the night", "early in the evening",
            # 1, seriesDefPM),
            #("Early Morning Update", "issuanceHour", self.DAY(), self.DAY(),
            # ".EARLY THIS MORNING...", "early in the morning", "late in the afternoon",
            # 1, seriesDefPM),
            ]



######################################
## Section F:
##
##  Period_1 For Rush_Valley skyPopWx Local Effects
##
##    def Period_1(self):
##        return {
##            "type": "component",
##            "methodList": [
##                          self.orderPhrases,
##                          self.consolidateSubPhrases,
##                          self.assemblePhrases,
##                          self.wordWrap,
##                          ],
##            "analysisList": [
##                       ("MinT", self.stdDevMinMax),
##                       ("MaxT", self.stdDevMinMax),
##                       ("T", self.hourlyTemp),
##                       ("T", self.minMax),
##                       ("Sky", self.median, [3]),
##                       ("PoP", self._PoP_analysisMethod("Period_1"), [3]),
##                       ("PoP", self.binnedPercent, [3]),
##                       #("Wind", self.vectorMedianRange, [0]),
##                       ("Wind", self.vectorModeratedMinMax, [0]),
##                       ("Wind", self.vectorMinMax, [0]),
##                       ("WindGust", self.maximum, [0]),
##                       ("Wx", self.rankedWx, [3]),
##                       ("WindChill", self.minMax),
##                       ("HeatIndex", self.minMax),
##                       ],
##            "phraseList":[
##                   self.sky_phrase,
####                   self.skyPopWx_phrase,
##                   (self.skyPopWx_phrase, self._wxLocalEffects_addlArea_list()),
##                   self.wind_summary,
##                   self.reportTrends,
####                   self.weather_phrase,
##                   (self.weather_phrase,self._wxLocalEffects_addlArea_list()),
##                   self.heavyPrecip_phrase,
##                   self.severeWeather_phrase,
##                   self.highs_phrase,
##                   self.lows_phrase,
##                   self.temp_trends,
##                   self.wind_withGusts_phrase,
##                   self.popMax_phrase,
##                   self.windChill_phrase,
##                   self.heatIndex_phrase,
##                   ],
##            "additionalAreas": [
##                   # Areas listed by weather element that will be
##                   # intersected with the current area then
##                   # sampled and analysed.
##                   # E.g. used in local effects methods.
##                   ("Sky", ["Rush_Valley"]),
##                   ("Wx",  ["Rush_Valley"]),
##                   ("PoP", ["Rush_Valley"]),
##             ],
##        }
##
##  Period_1 For skyPopWx Local Effects
##


 
    def Period_1_version2(self):
        return {
            "type": "component",
            "methodList": [
                          self.orderPhrases,
                          self.consolidateSubPhrases,
                          self.assemblePhrases,
                          self.wordWrap,
                          ],
            "analysisList": [
                       ("Sky", self.median, [3]),
                       ("PoP", self._PoP_analysisMethod("Period_1"), [3]),
                       ("PoP", self.binnedPercent, [3]),
                       ("Wx", self.rankedWx, [3]),
                       ],
            "phraseList":[
                   (self.sky_phrase, self._skyLocalEffects_list()),
                   (self.skyPopWx_phrase, self._skyPopWxLocalEffects_list()),
                   (self.weather_phrase,self._wxLocalEffects_list()),
                   (self.popMax_phrase, self._popLocalEffects_list()),
                   ],
            "intersectAreas": [
                   # Areas listed by weather element that will be
                   # intersected with the current area then
                   # sampled and analysed.
                   # E.g. used in local effects methods.
                   ("Sky", ["AboveElev", "BelowElev"]),
                   ("Wx",  ["AboveElev", "BelowElev"]),
                   ("PoP", ["AboveElev", "BelowElev"]),
             ],
        }

    def _skyLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "windward")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        return [self.LocalEffect([leArea1, leArea2], self.checkSkyDifference, "...")]
 
    def _wxLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "windward")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        return [self.LocalEffect([leArea1, leArea2], 0, "...")]

    def _popLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "windward")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        return [self.LocalEffect([leArea1, leArea2], 20, "...")]

    def _skyPopWxLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "windward")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        # Set threshold to be used by checkSkyWxDifference
        self._skyLocalEffectThreshold = 38
        return [self.LocalEffect([leArea1, leArea2],
                                 self.checkSkyWxDifference, "...")]
 
 
######################################


##  Section G: Unit Conversion
 
    def element_outUnits_dict(self, tree, node):
        dict = AreaFcst.TextProduct.element_outUnits_dict(self, tree, node)
        # Default is mph
        #dict["Wind"] = "kts"
        return dict

##  Section G: Standard and Non-Standard Rounding
 
    def increment_nlValue_dict(self, tree, node):
        # Increment for rounding values
        # Units depend on the product
        dict = TextRules.TextRules.increment_nlValue_dict(self, tree, node)
        # Default is 5
        #dict["Wind"] = 10
        #dict["Wind"] = {
        #    'default': 15,
        #    (0, 21):  5,
        #    (21, 40): 10,
        #    }
        return dict
 
##  Section G: Range Adjustment

    def minimum_range_nlValue_dict(self, tree, node):
        # This threshold is the "smallest" min/max difference allowed between values reported.
        # For example, if threshold is set to 5 for "MaxT", and the min value is 45
        # and the max value is 46, the range will be adjusted to at least a 5 degree
        # range e.g. 43-48.  These are the values that are then submitted for phrasing
        # such as:
        #   HIGHS IN THE MID 40S
        dict = TextRules.TextRules.minimum_range_nlValue_dict(self, tree, node)
        # Default is 0
        #dict["Wind"] = 10
        return dict

    def minimum_range_bias_nlValue_dict(self, tree, node):
        # "Min", "Average", "Max"
        #  Should the minimum_range be taken from the "min" "average" or "max"
        #  value of the current range?
        dict = TextRules.TextRules.minimum_range_bias_nlValue_dict(self, tree, node)
        # Default is Max
        #dict["Wind"] = "Average"
        return dict
 
    def maximum_range_nlValue_dict(self, tree, node):
        # Maximum range to be reported within a vector phrase
        #   e.g. 5 to 10 mph
        # Units depend on the product
        dict = TextRules.TextRules.maximum_range_nlValue_dict(self, tree, node)
        #dict["Wind"] = 10
        return dict
 
##  Section G: Null Values

    def null_nlValue_dict(self, tree, node):
        # Threshold for reporting null values
        # Units depend on the element and product
        dict = TextRules.TextRules.null_nlValue_dict(self, tree, node)
        dict["Wind"] =  5
        return dict

    def first_null_phrase_dict(self, tree, node):
        # Phrase to use if values THROUGHOUT the period or
        # in the first period are Null (i.e. below threshold OR NoWx)
        # E.g.  LIGHT WINDS.    or    LIGHT WINDS BECOMING N 5 MPH.
        dict = TextRules.TextRules.first_null_phrase_dict(self, tree, node)
        #dict["Wind"] =  "light winds"
        #dict["Wind"] =  ""
        return dict

    def null_phrase_dict(self, tree, node):
        # Phrase to use for null values in subPhrases other than the first
        # Can be an empty string
        #  E.g.  "NORTH WINDS 20 to 25 KNOTS BECOMING LIGHT"
        dict = TextRules.TextRules.null_phrase_dict(self, tree, node)
        #dict["Wind"] =  "light"
        #dict["Wind"] =  ""
        dict["Wx"] =  ""
        return dict

##  Section G: Until Phrasing

    def untilPhrasing_flag_dict(self, tree, node):
        # If set to 1, "until" time descriptor phrasing will be used.
        # E.g. "NORTH WINDS 20 MPH UNTIL 10 AM...THEN 35 MPH"
        return {
            "otherwise": 0,
            #"Wind" : 1,
            }

    def onTheFly_untilPhrasing_flag_dict(self, tree, node):
        # If set to 1, "until" time descriptor phrasing will be used.
        # E.g. "NORTH WINDS 20 MPH UNTIL 10 AM...THEN 35 MPH"
        return {
            "otherwise": 0,
            #"Wind" : 1,
            }
 
    def untilPhrasing_format_dict(self, tree, node):
        # Format for "until" time descriptors.
        # If "military": UNTIL 1000
        # If "standard": UNTIL 10 AM
        return {
            "otherwise": "military",
            #"Wind": "standard",
            }

######################################
## Section G:  Period_1 For Visibility Tests
##    def Period_1(self):
##        return {
##            "type": "component",
##            "methodList": [
##                          self.orderPhrases,
##                          self.consolidateSubPhrases,
##                          self.assemblePhrases,
##                          self.wordWrap,
##                          ],
##            "analysisList": [
##                       ("MinT", self.stdDevMinMax),
##                       ("MaxT", self.stdDevMinMax),
##                       ("T", self.hourlyTemp),
##                       ("T", self.minMax),
##                       ("Sky", self.median, [3]),
##                       ("PoP", self._PoP_analysisMethod("Period_1"), [3]),
##                       ("PoP", self.binnedPercent, [3]),
##                       #("Wind", self.vectorMedianRange, [0]),
##                       ("Wind", self.vectorModeratedMinMax, [0]),
##                       ("Wind", self.vectorMinMax, [0]),
##                       ("WindGust", self.maximum, [0]),
##                       ("Wx", self.rankedWx, [3]),
##                       ("WindChill", self.minMax),
##                       ("HeatIndex", self.minMax),
##                       ],
##            "phraseList":[
##                   self.sky_phrase,
####                   self.skyPopWx_phrase,
####                   (self.skyPopWx_phrase, self._wxLocalEffects_list()),
##                   self.wind_summary,
##                   self.reportTrends,
##                   self.weather_phrase,
####                   (self.weather_phrase,self._wxLocalEffects_list()),
##                   self.heavyPrecip_phrase,
##                   self.severeWeather_phrase,
##                   self.highs_phrase,
##                   self.lows_phrase,
##                   self.temp_trends,
##                   self.wind_withGusts_phrase,
##                   self.popMax_phrase,
##                   self.windChill_phrase,
##                   self.heatIndex_phrase,
##                   ],
####            "additionalAreas": [
####                   # Areas listed by weather element that will be
####                   # intersected with the current area then
####                   # sampled and analysed.
####                   # E.g. used in local effects methods.
####                   ("Sky", ["Rush_Valley"]),
####                   ("Wx",  ["Rush_Valley"]),
####                   ("PoP", ["Rush_Valley"]),
####             ],
##        }

##    # Handling visibility within the weather phrase
##    def embedded_visibility_flag(self, tree, node):
##        # If 1, report visibility embedded with the
##        # weather phrase. Set this to 0 if you are using the
##        # visibility_phrase.
##        return 0
 
######################################


##  Section G: Weather Key Filtering

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
        # is dominant over the coverage of RW and
        # RW does not have + intensity
        wxType1 = subkey1.wxType()
        wxType2 = subkey2.wxType()
        if wxType1 == "T" and wxType2 == "RW":
            if subkey2.intensity() != "+":
                order = self.dominantCoverageOrder(subkey1, subkey2)
                if order == -1 or order == 0:
                    return 1, subkey1
        return 0, None
 
    def useSkyPopWx_consolidation(self, tree, node):
        # If set to 1, the skyPopWx phrase will consolidate weather keys that
        # span all time ranges to produce:
        #   PARTLY CLOUDY WITH A CHANCE OF RAIN.
        #   SNOW IN THE MORNING...THEN SLEET IN THE AFTERNOON.
        #
        # instead of:
        #    PARTLY CLOUDY. CHANCE OF RAIN AND SNOW IN THE MORNING
        #  ...THEN A CHANCE OF RAIN AND SLEET IN THE AFTERNOON.
 
        #return 1
        return 0
 
    def areal_sky_flag(self, tree, node):
        # Set to 1 if you want to use areal (e.g. patchy clouds, areas of clouds)
        # vs. traditional sky wording when appropriate.
        # BE SURE AND SET THE "arealSkyAnalysis" flag to 1 in the Definition section!
        # You may want to base this decision on the current edit area and/or
        # component e.g. "Period_1"
        return 0

    def matchToWxInfo_dict(self, tree, node):
        # The system will automatically match the following elements to
        # the highest ranking weather subkey coverage.
        # Each entry is a tuple of (increment, algorithm, noPrecipValue) where
 
        #  increment: This is the increment from the low "bin" value
        #    to be added.  For example, PoP has a bin of 55-65, so
        #    its increment is 5 to end up with values as multiples of 10.
 
        #  algorithm: Can be
        #    Max:  The MAXIMUM value that falls within the coverage range
        #          for the highest ranking subkey will be chosen.
        #    Mode: The MOST FREQUENT (over space and time) value that
        #          falls within the coverage range for the highest ranking
        #          subkey will be chosen.
        #    MaxMode: This is the MAXIMUM value over time of the MOST
        #         FREQUENT values over area for each of the grids in the timeRange.
        #         In other words, for each grid, we find the Mode i.e. MOST FREQUENT
        #         value that falls within the coverage range for the highest
        #         ranking subkey.  Then we find the MAXIMUM of these values
        #         over the grids again falling within the coverage values.
        #    AnalysisMethod: This will simply use whatever analysis method
        #         is specified as the first entry in the product component
        #         for the element. For example, if you have
        #
        #         ("PoP", self.stdDevMaxAvg, [3]),
        #         ("PoP", self.binnedPercent, [3]),
        #
        #         the "stdDevMaxAvg" method will be used.
        #  noPrecipValue: The value that should be returned if there is
        #         no precipitating weather.  Can be:
        #     None
        #     Max: The maximum value found that has a greater > 0% occurrence.
        #     AnalysisMethod: As above, will return the result of the product
        #         component analysis method e.g. stdDevMaxAvg or maximum.
        #
        #  EXAMPLE 1:  Suppose we have:

        #       Wx  Hours 1-12:  Chc R  (coverage range is 30-60)
        #       PoP Hours 1-3:   40% (over 70% of area), 50% (over 30% of area)
        #           Hours 4-12:  30

        # For the 12-hour PoP,
        #    If set to Max, we will get PoP:      50
        #    If set to Mode, we will get PoP:     30
        #    If set to MaxMode, we will get PoP:  40
 
        # For the Hours 1-3 PoP:
        #    If set to Max, we will get PoP:      50
        #    If set to Mode, we will get PoP:     40
        #    If set to MaxMode, we will get PoP:  40
 
        #  NOTE: IF you add a new element to this list, you MUST include
        #  a coverage table named "coverage<elementName>_value".  Follow
        #  the example for "coveragePoP_value" in CommonUtils.  You can
        #  then access the element value by calling "matchToWx" (WxPhrases).
        #
        # Test case 5_1 PopWx1
        return {
            "PoP": (5, "Max", None),     # 50
            #"PoP": (5, "Mode", None),    # 30
            #"PoP": (5, "MaxMode", None), # 40
            #"PoP": (5, "AnalysisMethod", None),  # 40
            "LAL": (0, "Max", "Max"),
            }


