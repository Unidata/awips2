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
# Local_Effects_Test_Local
#  Local customizations for AreaFcst as Base class to test Local Effects
#
# Author: hansen
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

import AreaFcst
import string
import TextRules
import types
import copy

class TextProduct(AreaFcst.TextProduct):
    Definition = copy.deepcopy(AreaFcst.TextProduct.Definition)
    Definition['displayName'] = "None"
    
    # REQUIRED CONFIGURATION ITEMS 
    #Definition['displayName'] = "TEST_LocalEffectsTest"
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
                                      ("area3", "Area 3"),
    #                                  ("area2", "Area 2"),
                                     ]

    Definition["windLE_list"] = 1
    Definition["tempLE_list"] = 1
    Definition["Period_1_version"] = 1
    Definition["tempLE_method"] = 1
    
    #Definition["directiveType"] = "C11"       
    #Definition["directiveType"] = "10-503"     # Can be "C11"
    #Definition["includeFloodingQuestion"] = 1  # Set to 1 to include flooding question

    #Definition["includeMultipleElementTable"] = 1       # Will include a TempPoPTable
    #Definition["cityDictionary"] = "CityDictionary"     # For TempPoPTable
    
    #Definition["areaDictionary"] = "AreaDictionary"     # For product headers
    #Definition["language"] = "english"                 
    
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
        # Uncomment this line for invoking areal or chance pop descriptor
        #dict["PoP"] = self.areal_or_chance_pop_descriptor,
        return dict

    def pop_lower_threshold(self, tree, node):
        # Pop values below this amount will not be reported
        return 20

    def pop_upper_threshold(self, tree, node):
        # Pop values above this amount will not be reported
        return 70

    def pop_wx_lower_threshold(self, tree, node):
        # Pop-related Wx will not be reported if Pop is below this threshold
        return 20

    def pop_sky_lower_threshold(self, tree, node):
        # Sky condition will not be reported if Pop is above this threshold
        return 60

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
        dict["Wind"] =  "light winds"  
        return dict

    def null_phrase_dict(self, tree, node):
        # Phrase to use for null values in subPhrases other than the first
        # Can be an empty string
        #  E.g.  "NORTH WINDS 20 to 25 KNOTS BECOMING LIGHT"
        dict = TextRules.TextRules.null_phrase_dict(self, tree, node)
        dict["Wind"] =  "light"  
        dict["Wx"] =  ""  
        return dict
 
    def increment_nlValue_dict(self, tree, node):
        # Increment for rounding values
        # Units depend on the product
        dict = TextRules.TextRules.increment_nlValue_dict(self, tree, node)
        dict["Wind"] = 5
        return dict
     
    def vector_mag_difference_dict(self, tree, node):
        # Replaces WIND_THRESHOLD
        # Magnitude difference.  If the difference between magnitudes
        # for the first and second half of a period is greater than this value,
        # the different magnitudes will be noted in the phrase.
        # Units can vary depending on the element
        dict = TextRules.TextRules.vector_mag_difference_dict(self, tree, node)
        dict["Wind"] = 10
        return dict   

    def scalar_difference_nlValue_dict(self, tree, node):
        # Scalar difference.  If the difference between scalar values
        # for 2 sub-periods is greater than this value,
        # the different values will be noted in the phrase.
        return {
            "WindGust": 10, # knots or mph depending on product
            "Period": 5, # seconds
            "PoP": 10, # percentage
            }        

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
    
    def maximum_range_nlValue_dict(self, tree, node):
        # Maximum range to be reported within a vector phrase
        #   e.g. 5 to 10 mph
        # Units depend on the product
        dict = TextRules.TextRules.maximum_range_nlValue_dict(self, tree, node)
        #dict["MaxT"] = 15
        #dict["MinT"] = 15
        return dict   

    def minimum_range_nlValue_dict(self, tree, node):
        # This threshold is the "smallest" min/max difference allowed between values reported.
        # For example, if threshold is set to 5 for "MaxT", and the min value is 45
        # and the max value is 46, the range will be adjusted to at least a 5 degree
        # range e.g. 43-48.  These are the values that are then submitted for phrasing
        # such as:
        #   HIGHS IN THE MID 40S 
        dict = TextRules.TextRules.minimum_range_nlValue_dict(self, tree, node)
        #dict["MaxT"] = 5
        #dict["MinT"] = 5
        return dict             
           
    def range_threshold_nlValue_dict(self, tree, node):
        # Range for reporting temperature ranges in temp_range_phrase
        #   e.g HIGHS 80 TO 85
        dict = TextRules.TextRules.range_threshold_nlValue_dict(self, tree, node)
        dict["MaxT"] = 5
        dict["MinT"] =  5
        dict["MinRH"] =  5
        dict["MaxRH"] =  5
        dict["WindChill"] = 5
        dict["HeatIndex"] = 5
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
        method = getattr(self, f"Period_1_version{self._Period_1_version!r}")
        return method()
       
    def Period_1_version1(self):
        return { 
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            "analysisList": [
                       #("MinT", self.avg),
                       #("MaxT", self.avg),
                       ("MaxT", self.stdDevMinMax),
                       ("T", self.hourlyTemp),
                       ("T", self.minMax),
                       ("Sky", self.median, [3]),
                       ("Sky", self.binnedPercent, [6]),
                       ("PoP", self._PoP_analysisMethod("Period_1"), [3]),
                       ("PoP", self.binnedPercent, [3]),
                       ("SnowAmt", self.accumMinMax),
                       ("IceAccum", self.accumMinMax),
                       ("Wind", self.vectorMedianRange, [6]),
                       ("Wind", self.vectorMinMax, [6]),
                       ("WindGust", self.maximum, [6]),
                       ("Wx", self.rankedWx, [3]),
                       ],
            "phraseList":[
##                   self.skyPopWx_phrase,
##                   (self.skyPopWx_phrase, self._wxLocalEffects_list()),
##                   self.sky_phrase,
##                   self.wind_summary,
##                   self.reportTrends,
##                   self.weather_phrase,
##                   (self.weather_phrase, self._wxLE_list),
            
##                   (self.weather_phrase,self._wxLocalEffects_list()),             
##                   (self.snow_phrase,self._snowAmtLocalEffects_list()),
##                   (self.total_snow_phrase,self._totalSnowAmtLocalEffects_list()),
            
                   (self.highs_phrase, self._tempLocalEffects_list()),
##                   (self.highs_phrase, self._tempLocalEffects_method),
                    
                   (self.wind_withGusts_phrase, self._windLocalEffects_list()),
                   
##                   self.popMax_phrase,
                   ],
##            "additionalAreas": [ 
##                   # Areas listed by weather element that will be
##                   # intersected with the current area then
##                   # sampled and analysed.  
##                   # E.g. used in local effects methods.
##                   ("MaxT", ["area2", "area1"]),
##             ],
            "intersectAreas": [ 
                   # Areas listed by weather element that will be
                   # intersected with the current area then
                   # sampled and analysed.  
                   # E.g. used in local effects methods.
                   ("MaxT", ["AboveElev", "BelowElev"]),
                   ("Wind", ["AboveElev", "BelowElev"]),
                   ("WindGust", ["AboveElev", "BelowElev"]),
                   ("SnowAmt", ["AboveElev", "BelowElev"]),
                   ("Wx", ["AboveElev", "BelowElev"]),
                   ("PoP", ["AboveElev", "BelowElev"]),
             ],
        }

    def TotalSnowSampling(self):
        return { 
            "type": "component",
            "methodList": [self.noWords],
            "analysisList": [
                       ("SnowAmt", self.accumMinMax),
                       ],
            "phraseList":[],
            "intersectAreas": [ 
                   ("SnowAmt", ["AboveElev", "BelowElev"]),
             ],
        }

    def _wxLE_list(self, tree, node):
        leArea5 = self.LocalEffectArea("BelowElev", "")
        leArea6 = self.LocalEffectArea("AboveElev", "along major rivers", intersectFlag=1)
        return [self.LocalEffect([leArea5, leArea6], 0, ", except ")]

    def _tempLocalEffects_list(self):
        method = getattr(self, f"_tempLocalEffects_list{self._tempLE_list!r}")
        return method()

    def _tempLocalEffects_list1(self):
        leArea1 = self.LocalEffectArea("BelowElev", "")
        leArea2 = self.LocalEffectArea("AboveElev", "in the mountains")
        return [self.LocalEffect([leArea1, leArea2], 8, ", except ")]
    
    def _windLocalEffects_list(self):
        method = getattr(self, f"_windLocalEffects_list{self._windLE_list!r}")
        return method()
        
    def _windLocalEffects_list1(self):
        leArea1 = self.LocalEffectArea("BelowElev", "")
        leArea2 = self.LocalEffectArea("AboveElev", "in the mountains")
        return [self.LocalEffect([leArea1, leArea2], 10, ", except ")]

    def Period_2_3(self):
        # No Lake Wind phrase
        return {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,         
                          ],
            "intersectAreas": [ 
                   # Areas listed by weather element that will be
                   # intersected with the current area then
                   # sampled and analysed.  
                   # E.g. used in local effects methods.
                   ("SnowAmt", ["AboveElev", "BelowElev"]),
                   ("Wx", ["AboveElev", "BelowElev"]),
                   ("PoP", ["AboveElev", "BelowElev"]),
                   ],
            "analysisList": [
                       #("MinT", self.avg),
                       #("MaxT", self.avg),
                       ("MinT", self.stdDevMinMax),
                       ("MaxT", self.stdDevMinMax),
                       ("T", self.hourlyTemp),
                       ("T", self.minMax),
                       ("Sky", self.median, [6]),
                       ("PoP", self._PoP_analysisMethod("Period_2_3"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                       ("SnowAmt", self.accumMinMax),
                       ("IceAccum", self.accumMinMax),
                       ("SnowLevel", self.avg),
                       ("Wind", self.vectorMedianRange, [6]),
                       ("Wind", self.vectorMinMax, [6]),
                       ("WindGust", self.maximum, [6]),
                       ("Wx", self.rankedWx, [6]),
                       ("WindChill", self.minMax),
                       ("HeatIndex", self.minMax),
                      ],
            "phraseList":[
                   self.sky_phrase,
                   self.wind_summary,
                   self.reportTrends,
                   (self.weather_phrase,self._wxLocalEffects_list()),             
                   #self.weather_phrase,
                   self.severeWeather_phrase,
                   (self.snow_phrase,self._snowAmtLocalEffects_list()),
                   #self.snow_phrase,
                   (self.total_snow_phrase,self._totalSnowAmtLocalEffects_list()),
                   self.snowLevel_phrase,
                   self.highs_phrase,
                   self.lows_phrase,
                   #self.highs_range_phrase,
                   #self.lows_range_phrase,
                   self.temp_trends,
                   self.wind_withGusts_phrase,
#                   self.lake_wind_phrase,
                   self.popMax_phrase,
                   self.windChill_phrase,
                   self.heatIndex_phrase,
                  ],
            }

    def Period_4_5(self):
        # Descriptive snow phrase
        return {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,  
                          self.wordWrap,         
                          ],
            "analysisList": [
                       #("MinT", self.avg),
                       #("MaxT", self.avg),
                       ("MinT", self.stdDevMinMax),
                       ("MaxT", self.stdDevMinMax),
                       ("T", self.hourlyTemp),
                       ("T", self.minMax),
                       ("Sky", self.median, [6]),
                       ("PoP", self._PoP_analysisMethod("Period_4_5"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                       ("SnowAmt", self.accumMinMax),
                       ("IceAccum", self.accumMinMax),
                       ("SnowLevel", self.avg),
                       ("Wind", self.vectorMedianRange, [6]),
                       ("Wind", self.vectorMinMax, [6]),
                       ("Wx", self.rankedWx, [6]),
                       ("WindChill", self.minMax),
                       ("HeatIndex", self.minMax),
                      ],
            "phraseList":[
                   self.sky_phrase,
                   self.wind_summary,
                   self.reportTrends,
                   (self.weather_phrase,self._wxLocalEffects_list()),             
                   #self.weather_phrase,
                   self.severeWeather_phrase,
                   self.snow_phrase,
                   self.total_snow_phrase,
                   self.snowLevel_phrase,
                   self.highs_phrase,
                   self.lows_phrase,
                   #self.highs_range_phrase,
                   #self.lows_range_phrase,
                   self.temp_trends,
                   self.wind_withGusts_phrase,
                   self.popMax_phrase,
                   self.windChill_phrase,
                   self.heatIndex_phrase,
                  ],
            "intersectAreas": [ 
                   # Areas listed by weather element that will be
                   # intersected with the current area then
                   # sampled and analysed.  
                   # E.g. used in local effects methods.
                   ("SnowAmt", ["AboveElev", "BelowElev"]),
                   ("Wx", ["AboveElev", "BelowElev"]),
                   ("PoP", ["AboveElev", "BelowElev"]),
                   ],
            }    

    def _snowAmtLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("BelowElev", "")
        leArea2 = self.LocalEffectArea("AboveElev", "above timberline")
        return [self.LocalEffect([leArea1, leArea2], 2, ", except ")]

    def _totalSnowAmtLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("BelowElev", "")
        leArea2 = self.LocalEffectArea("AboveElev", "above timberline")
        return [self.LocalEffect(
            [leArea1, leArea2], self._checkTotalSnow, ", except ")]

    def _checkTotalSnow(self, tree, node, localEffect, leArea1Label, leArea2Label):
        totalSnow1 = self.getTotalSnow(tree, node, leArea1Label)
        totalSnow2 = self.getTotalSnow(tree, node, leArea2Label)
        if totalSnow1 is None or totalSnow2 is None:
            return 0
        if type(totalSnow1) is tuple:
            min,totalSnow1 = totalSnow1
        if type(totalSnow2) is tuple:
            min,totalSnow2 = totalSnow2
        if abs(totalSnow1 - totalSnow2) > 3:
            return 1
        return 0
    
    def _wxLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("BelowElev", "")
        leArea2 = self.LocalEffectArea("AboveElev", "above timberline")
        return [self.LocalEffect([leArea1, leArea2], 10, ", except ")]

    def _10_503_issuance_list(self, argDict):
        seriesDefAM = [
            ("Period_1", "period1"),    
##            ("Period_2_3", 12), ("Period_2_3", 12), ("Period_4_5", 12), ("Period_4_5", 12), 
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

##    def checkThreshold(self, tree, node, triggerMethod, leArea1, leArea2):
##        # Return 1 if the difference between leArea1 and leArea2 stats is
##        # greater than the threshold
##        # Handles stats that are a min/max or a singleValue
##        leArea1Label = self.getLeAreaLabel(tree, node, leArea1)
##        leArea2Label = self.getLeAreaLabel(tree, node, leArea2)
##        if type(triggerMethod) is types.MethodType:
##            flag =  triggerMethod(tree, node, node.get("localEffect"), leArea1Label, leArea2Label)
##        else:
##            first = node.getAncestor("firstElement")
##            element = first.name
##            dataType = first.dataType
##            if dataType == self.WEATHER():
##                mergeMethod = "Average"
##            else:
##                mergeMethod = "MinMax"
##            timeRange = node.getTimeRange()
##            area1Stats = tree.stats.get(element, timeRange, leArea1Label,
##                                        mergeMethod=mergeMethod)
##            area2Stats = tree.stats.get(element, timeRange, leArea2Label,
##                                        mergeMethod=mergeMethod)
##            print "\nLocal effects", element, timeRange
##            print leArea1Label, area1Stats
##            print leArea2Label, area2Stats
##            if area1Stats is None or area2Stats is None:
##                return 0
##            flag = self.checkLocalEffectDifference(
##                tree, node, dataType, triggerMethod, area1Stats, area2Stats,
##                leArea1Label, leArea2Label)
##            print "returning", flag
##        return flag



###################################################
####  TEST OVERRIDE SECTION

    # EXCEPT VS OTHERWISE WORDING:
    
    def _windLocalEffects_list2(self):
        leArea1 = self.LocalEffectArea("BelowElev", "", "in the valleys")
        leArea2 = self.LocalEffectArea("AboveElev", "in the mountains")
        return [self.LocalEffect([leArea2, leArea1], 10, ", otherwise ")]

    # USING A METHOD TO SPECIFY LOCAL EFFECT AREAS:
    
    def _tempLocalEffects_list2(self):
        return [self.LocalEffect(self._getTempLeAreas, 8, ", except ")]
    
    def _getTempLeAreas(self, tree, node):
        leArea1 = self.LocalEffectArea("BelowElev", "")
        leArea2 = self.LocalEffectArea("AboveElev", "in the mountains")
        return [leArea1, leArea2]

    # LOCAL EFFECT AREAS THAT DO NOT INTERSECT THE CURRENT AREA
    
    def Period_1_version2(self):
        return { 
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            "analysisList": [
                       #("MinT", self.avg),
                       #("MaxT", self.avg),
                       ("MaxT", self.stdDevMinMax),
                       ("T", self.hourlyTemp),
                       ("T", self.minMax),
                       ("Sky", self.median, [3]),
                       ("Sky", self.binnedPercent, [6]),
                       ("PoP", self._PoP_analysisMethod("Period_1"), [3]),
                       ("PoP", self.binnedPercent, [3]),
                       ("SnowAmt", self.minMax),
                       ("IceAmt", self.minMax),
                       ("Wind", self.vectorMedianRange, [6]),
                       ("Wind", self.vectorMinMax, [6]),
                       ("WindGust", self.maximum, [6]),
                       ("Wx", self.rankedWx, [3]),
                       ],
            "phraseList":[
##                   self.skyPopWx_phrase,
##                   (self.skyPopWx_phrase, self._wxLocalEffects_list()),
##                   self.sky_phrase,
##                   self.wind_summary,
##                   self.reportTrends,
##                   self.weather_phrase, 
##                   (self.weather_phrase,self._wxLocalEffects_list()), 
##                   self.severeWeather_phrase,
##            
##                   (self.snow_phrase,self._snowAmtLocalEffects_list()),
##                   (self.total_snow_phrase,self._totalSnowAmtLocalEffects_list()),
            
##                   (self.highs_phrase, self._tempLocalEffects_list()),
                   (self.highs_phrase, self._tempLocalEffects_method),
                    
##                   (self.wind_withGusts_phrase, self._windLocalEffects_list()),
                   
##                   self.popMax_phrase,
                   ],
            "additionalAreas": [ 
                   # Areas listed by weather element that will be
                   # intersected with the current area then
                   # sampled and analysed.  
                   # E.g. used in local effects methods.
                   ("MaxT", ["area2", "area1"]),
             ],
            "intersectAreas": [ 
                   # Areas listed by weather element that will be
                   # intersected with the current area then
                   # sampled and analysed.  
                   # E.g. used in local effects methods.
                   ("MaxT", ["AboveElev", "BelowElev"]),
                   ("Wind", ["AboveElev", "BelowElev"]),
                   ("WindGust", ["AboveElev", "BelowElev"]),
                   ("SnowAmt", ["AboveElev", "BelowElev"]),
                   ("Wx", ["AboveElev", "BelowElev"]),
                   ("PoP", ["AboveElev", "BelowElev"]),
             ],
        }

    def _tempLocalEffects_method(self, tree, node):
        method = getattr(self, f"_tempLocalEffects_method{self._tempLE_method!r}")
        return method(tree, node)

    def _tempLocalEffects_method1(self, tree, node):
        if self.currentAreaContains(tree, ["area3"]):
           leArea1 = self.LocalEffectArea(
                    "__Current__","",intersectFlag=0)
           leArea2 = self.LocalEffectArea("area1",
                    "in the rush valley", intersectFlag=0)
           leArea3 = self.LocalEffectArea(
                    "area2", "in the benches",intersectFlag=0)
           return [
               self.LocalEffect([leArea1, leArea2], 5, ", except "),
               self.LocalEffect([leArea1, leArea3], 5, ", except "),
               ]
        else:
           return []

    def _tempLocalEffects_method2(self, tree, node):
        if self.currentAreaContains(tree, ["area3"]):
           leArea1 = self.LocalEffectArea(
                    "__Current__","in the city",intersectFlag=0)
           leArea2 = self.LocalEffectArea("area1",
                    "in the rush valley", intersectFlag=0)
           leArea3 = self.LocalEffectArea(
                    "area2", "in the benches",intersectFlag=0)
           return [self.LocalEffect([leArea1, leArea2, leArea3], 5, ", and ")]
           #return [
           #    self.LocalEffect([leArea1,leArea2],5,", except "),
           #    self.LocalEffect([leArea1,leArea3],5,", except "),
           #    ]
        else:
           return []


    def Period_1_version3(self):
        return { 
            "type": "component",
            "methodList": [
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
            "additionalAreas": [ 
                   # Areas listed by weather element that will be
                   # intersected with the current area then
                   # sampled and analysed.  
                   # E.g. used in local effects methods.
                   ("Sky", ["area2", "area1"]),
                   ("Wx", ["area2", "area1"]),
                   ("PoP", ["area2", "area1"]),
             ],
        }

    def _skyLocalEffects_list(self):
        leArea1 = self.LocalEffectArea(
            "__Current__","in the city",intersectFlag=0)
        leArea2 = self.LocalEffectArea(
            "area1","in the rush valley", intersectFlag=0)
        leArea3 = self.LocalEffectArea(
            "area2", "in the benches",intersectFlag=0)
        return [
               self.LocalEffect([leArea1, leArea2, leArea3], self.checkSkyDifference, ", "),
               ]
    
    def _wxLocalEffects_list(self):
        leArea1 = self.LocalEffectArea(
            "__Current__","in the city",intersectFlag=0)
        leArea2 = self.LocalEffectArea(
            "area1","in the rush valley", intersectFlag=0)
        leArea3 = self.LocalEffectArea(
            "area2", "in the benches",intersectFlag=0)
        return [
               self.LocalEffect([leArea1, leArea2, leArea3], 0, ", "),
               ]

    def _popLocalEffects_list(self):
        leArea1 = self.LocalEffectArea(
            "__Current__","in the city",intersectFlag=0)
        leArea2 = self.LocalEffectArea(
            "area1","in the rush valley", intersectFlag=0)
        leArea3 = self.LocalEffectArea(
            "area2", "in the benches",intersectFlag=0)
        return [
               self.LocalEffect([leArea1, leArea2, leArea3], 20, ", "),
               ]
    
    def _skyPopWxLocalEffects_list(self):
        leArea1 = self.LocalEffectArea(
            "__Current__","in the city",intersectFlag=0)
        leArea2 = self.LocalEffectArea(
            "area1","in the rush valley", intersectFlag=0)
        leArea3 = self.LocalEffectArea(
            "area2", "in the benches",intersectFlag=0)
        return [
               self.LocalEffect([leArea1, leArea2, leArea3], self.checkSkyWxDifference, ", "),
               ]
