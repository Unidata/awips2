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
# Description: This product creates a ZFP-type series of text phrases 
# for consecutive time periods for a list of edit areas. It can be 
# used to create a ZFP or an SAF.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# AreaFcst, ZFP_<site>_<MultiPil>_Definition, ZFP_<site>_Overrides,
# SAF_Overrides, SAF_<site>_<MultiPil>_Definition, SAF_<site>_Overrides
# Optional: MultipleElement_Aux_Local (to create a MultipleElementTable)
#-------------------------------------------------------------------------
# Customization Points:
#
# DEFINITION SECTION
#
# Required Configuration Items:
#
#  displayName      If not None, defines how product appears in GFE GUI
#
#  You must set the following:
#
#  productName      defines name of product e.g. "ZONE FORECAST PRODUCT"
#  fullStationID    Full station identifier, 4 letter, such as "KSLC".
#  wmoID            WMO ID code for product header, such as "FOUS45"
#  pil              Product pil, such as "SFTBOS"
#  areaName (opt.)  Area name for product header, such as "WESTERN NEW YORK"
#  wfoCityState     City,state that the WFO is located in, such as "BUFFALO NY"
#
# Optional Configuration Items
#
#  mapNameForCombinations Name of the map background that is used for 
#                         creating/editing the combinations file.  This must 
#                         be defined or the GFE zone combiner
#  database               Source database for product. Can be "Official", 
#                         "Fcst" or "ISC"
#  outputFile             Defines the output location of the finished product
#                         when saved from FormatterLauncher.
#  debug                  If on, debug_print statements will appear.
#  textdbPil              Defines the awips product identifier 
#                         (e.g., DENCCFDEN) that is used to store the product 
#                         in the AWIPS text database. This value is also used for
#                         the default GUI entry for storage.
#  awipsWANPil            Defines the awips product identifier 
#                         (e.g., KBOUCCFDEN) that is used to transmit the 
#                         product to the AWIPS WAN.  
#                         This value is also used for the default GUI 
#                         entry for storage.
#  lineLength          Desired maximum length of each line.
#
#  hazardSamplingThreshold  Defines the percentage coverage or number of
#                    grid points in a zone that must contain the hazard
#                    in order for it to be considered. Tuple (percent, points)

#  periodCombining     If 1, an attempt will be made to combine components
#                      or time periods into one.  Otherwise no period 
#                      combining will be done.
#  defaultEditAreas    defines edit areas, default is Combinations
#  editAreaSuffix      default None. Allows for generating the body of the product for
#                      an edit area that is a subset (e.g. population areas) of the
#                      edit areas specified in the defaultEditAreas.  So given the edit area,
#                      "COZ035" and the editAreaSuffix is "_pt", then the edit area that
#                      will be sampled and reported for the body of the product will be
#                      "COZ035_pt".  If no such edit area exists, the system will simply
#                      use the original edit area.
#                      Note that Hazards will always be generated for the entire edit area.
#  directiveType       10-503 or C11
#  arealSkyAnalysis    Set to 1 to include analysis for Sky for 
#                      areal vs. traditional sky phrasing.  This is made 
#                      optional since there is a performance cost to 
#                      include this analysis.
#  useStormTotalSnow   Set to 1 to use StormTotalSnow grids for reporting total snow
#  includeExtended (applies to C11 only)      Include the extended forecast
#  extendedLabel (applies to C11 only)        Includes label before extended forecast    
#  includeEveningPeriod (applies to C11 only) Include a 6 hour Evening period on the 3rd day
#
#  includeMultipleElementTable
#  cityDictionary
#         To include a MultipleElementTable (e.g. Temp Pop Table)
#         for each area in the current Combination:
#         Set "includeMultipleElement" to 1
#         Set the "elementList" and "singleValueFormat" flag if desired
#         "elementList" may include "Temp", "PoP", and/or "Humidity"
#         "singleValueFormat" lists only one value per element
#         Make sure you are using a Combinations file
#         Modify the CityDictionary TextUtility file or create a new one
#  areaDictionary   Modify the AreaDictionary utility with UGC information about zones.
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
#    Out to 7 days:
#        MinT, MaxT, T,
#        Sky, PoP, Wind, Wx -- every 6 hours
#        Optional: SnowAmt, IceAccum, SnowLevel, WindChill, HeatIndex,
#                  StormTotalSnow
#    Out to 3 days:
#        WindGust (opt.) (every 6 hours)
# 
#   NOTE: If you choose to include a "pre-1st period" for the
#   Morning or Afternoon issuance, you must have grids in the
#   pre-1st period or you will not see any data for that element.
#-------------------------------------------------------------------------
# Edit Areas Needed: None 
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file:
#   Combinations file
#-------------------------------------------------------------------------
# Component Products:
#   10-503:
#      Period_1
#      Period_2_3
#      Period_4_5
#      Period_6_14
#
#   C11:
#      FirstFcstPeriod
#      AreaFcstPeriod
#      Evening
#      LaterPeriod
#      LaterPeriodWithoutLows
#      C11Extended
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
#     from ConfigVariables:
#       maximum_range_nlValue_dict
#       minimum_range_nlValue_dict
#       range_nlValue_dict
#       phrase_descriptor_dict
#       pop_lower_threshold
#       pop_upper_threshold
#       null_nlValue_dict
#       first_null_phrase_dict
#       null_phrase_dict
#       increment_nlValue_dict
#       vector_mag_difference_nlValue_dict
#       scalar_difference_nlValue_dict
#       value_connector_dict
#     from WxPhrases:
#       pop_wx_lower_threshold
#       wxCoverageDescriptors
#       wxTypeDescriptors
#       wxAttributeDescriptors
#       wxIntensityDescriptors
#       wxCombinations
#       combine_T_RW
#     from ScalarPhrases:
#       pop_sky_lower_threshold
#       pop_snow_lower_threshold
#       pop_snowLevel_upper_threshold
#       snowLevel_maximum_phrase
#       temp_trend_nlValue
#       temp_trends_addToPhrase_flag
#       windChillTemp_difference
#       heatIndexTemp_difference
#       areal_sky_flag       
#     from CombinedPhrases:
#       useCombinedSkyPopWx
#     from VectorRelatedPhrases:
#       lake_wind_areaNames
#       useWindsForGusts_flag
#     from SampleAnalysis:
#       stdDev_dict
#       
#-------------------------------------------------------------------------
# Example Output:
#  Refer to the NWS C11 and 10-503 Directives for Public Weather Services.
#-------------------------------------------------------------------------

import TextRules
import SampleAnalysis
import ForecastNarrative
import time, string, types, copy
import TimeRange
            
class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = [
             #  If Flooding is "Yes", the system will insert a statement in the
             #  product when FFA, FLW, or FLS is in effect.
             #(("Flooding?", "flooding") , "No", "radio", ["Yes","No"]), 
             #(("Include Tropical?", "includeTropical") , "No", "radio", ["Yes","No"]),
             ]

    Definition = {
        "type": "smart",
        "displayName": "None",

        # Source database for product. Can be "Official", "Fcst" or "ISC"
        "database": "Official",
        # Defines output location of finished product.
        "outputFile": "{prddir}/TEXT/ZFP_<MultiPil>.txt",
        "debug": 0,
        # Name of map background for creating Combinations
        "mapNameForCombinations": "Zones_<site>", 
        
        ## Edit Areas: Create Combinations file with edit area combinations.
        "showZoneCombiner" : 1, # 1 to cause zone combiner to display
        "defaultEditAreas" : "Combinations_ZFP_<site>_<MultiPil>",
        "editAreaSuffix": None,

        # product identifiers
        "productName": "ZONE FORECAST PRODUCT", # product name 
        "fullStationID": "<fullStationID>",    # full station identifier (4letter)
        "wmoID": "<wmoID>",                # WMO ID
        "pil": "<pil>",                    # Product pil
        "areaName": "<state>",             # Name of state, such as "GEORGIA" -- optional
        "wfoCityState": "<wfoCityState>",  # Location of WFO - city,state

        "textdbPil": "<textdbPil>",       # Product ID for storing to AWIPS text database.
        "awipsWANPil": "<awipsWANPil>",   # Product ID for transmitting to AWIPS WAN.
        "periodCombining" : 0,      # If 1, combine periods, if possible        
        "lineLength": 66,  #product line length
        "fixedExpire": 1, #ensure VTEC actions don't affect segment time

        # Choices are "10-503" or "C11"
        "directiveType": "10-503",
        # Set to 1 to include analysis for Sky for areal vs. traditional
        # sky phrasing.
        "arealSkyAnalysis":0,
        # Set to 1 to use StormTotalSnow grid
        "useStormTotalSnow": 0,
        #"directiveType": "C11",        
        # Applies to C11 directive only:
        "includeExtended": 1,
        # Set to one if you want an extended label
        "extendedLabel": 1,
        # Set to one if you want a 6-hour evening period instead of
        # 18-hour period without lows
        "includeEveningPeriod": 1,
        
        # Product-specific variables:
        # To include a MultipleElementTable (e.g. Temp Pop Table)
        # for each area in the current Combination:
        #   Set "includeMultipleElement" to 1
        #   Set the "elementList" and "singleValueFormat" flag if desired
        #   "elementList" may include "Temp", "PoP", and/or "Humidity"
        #   "singleValueFormat" lists only one value per element
        #   Make sure you are using a Combinations file
        #   Modify the CityDictionary TextUtility file or create a new one
        "includeMultipleElementTable": 0,
        "elementList" : ["Temp", "PoP"],
        "singleValueFormat": 0,
        "cityDictionary": "CityDictionary",

        # Weather-related flags
        "hoursSChcEnds": 24,
        
        # Area Dictionary -- Descriptive information about zones
        "areaDictionary": "AreaDictionary", 
        # Language
        "language": "english",

        # Trouble-shooting items
        "passLimit": 20,             # Limit on passes allowed through
                                     # Narrative Tree
        "trace": 0,                  # Set to 1 to turn on trace through
                                     # Narrative Tree for trouble-shooting



        "hazardSamplingThreshold": (10, None),  #(%cov, #points)
        }
    
    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)

    ########################################################################
    # OVERRIDING THRESHOLDS AND VARIABLES
    ########################################################################

    def lake_wind_areaNames(self, tree, node):
        # Return list of edit area names for which the lake_wind_phrase
        # should be generated
        # If you want the phrase potentially generated for all zones, use:
        # return ["ALL"]
        return []

    def lake_wind_thresholds(self, tree, node):
        # Return upper and lower lake_wind thresholds in mph.
        # Only apply phrase for max wind speed of 25 to 35 mph.  At 35 mph
        # and higher, an advisory of some sort will be in effect and phrase
        # will not be needed.
        return 25, 35

    def element_outUnits_dict(self, tree, node):
        dict = TextRules.TextRules.element_outUnits_dict(self, tree, node)
        dict["Wind"] = "mph"
        dict["WindGust"] = "mph"
        return dict

    def range_nlValue_dict(self, tree, node):
        # If the range of values is less than this threshold,
        # the data will be reported as a single value
        #   e.g HIGHS AROUND 80
        dict = TextRules.TextRules.range_nlValue_dict(self, tree, node)
        dict["MaxT"] = 5
        dict["MinT"] =  5
        dict["MinRH"] =  5
        dict["MaxRH"] =  5
        return dict

    # If you are using the C11 format with periodCombining on,
    # set this variable to zero for proper labeling.
    def splitDay24HourLabel_flag(self, tree, node):
        # Return 0 to have the TimeDescriptor module label 24 hour periods
        # with simply the weekday name (e.g. SATURDAY)
        # instead of including the day and night periods
        # (e.g. SATURDAY AND SATURDAY NIGHT)
        return 1

    def gust_wind_difference_nlValue(self, tree, node):
        # Difference between gust and maxWind below which gusts are not
        # mentioned. Units are MPH
        if self._includeTropical:
            return 5
        else:
            return 10

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
        """
           Modifed to lower the high end filter threshold from 20 MPH to
           15 MPH for Tropical.
        """
        # COMMENT: This dictionary defines the low and high limit at which
        # outliers will be removed when calculating moderated stats.
        # By convention the first value listed is the percentage
        # allowed for low values and second the percentage allowed
        # for high values. The thresholds chosen below gave best results
        # during testing with 2004 and 2005 tropical cyclones. This dict
        # is used with the moderatedMinMax analysis method specified in the
        # TropicalPeriod definitions specified further down for use with
        # tropical cyclones with wind parameters.

        # Get Baseline thresholds
        dict = SampleAnalysis.SampleAnalysis.moderated_dict(self, parmHisto,
                                                    timeRange, componentName)

        #  Change thresholds for Wind, WindGust, WaveHeight and Swell
        if self._includeTropical:
            dict["Wind"] = (0, 15)
            dict["WindGust"] = (0, 15)
            dict["WaveHeight"] = (0, 15)
            dict["Swell"] = (0, 15)
        return dict

    def maximum_range_nlValue_dict(self, tree, node):
        # Maximum range to be reported within a phrase
        #   e.g. 5 to 10 mph
        # Units depend on the product
        dict = TextRules.TextRules.maximum_range_nlValue_dict(self, tree, node)
        #-----------------------------------------------------------------------
        # COMMENT: Override max ranges for certain fields
        # This dict specifications allows for wind speed ranges of up to 20 mph
        # during tropical cyclone situations allowing for far better wind speed
        # phrases.
        #-----------------------------------------------------------------------
        if self._includeTropical:
            dict["Wind"] = {'default': 5,
                            (0.0, 4.0): 0,
                            (4.0, 33.0): 5,
                            (33.0, 52.0): 10,
                            (52.0, 200.0): 20,
                            }
        return dict
    
    ########################################################################
    # COMPONENT PRODUCT DEFINITIONS
    ########################################################################

    def _PoP_analysisMethod(self, componentName):
        # Alternative PoP analysis methods for consistency between PoP and Wx
        return self.stdDevMaxAvg
        #return self.maxMode
        #return self.maximum
        
    # These Components are named to indicate which period of the forecast 
    # they define.

    # Special alterations to analysisList and phraseList for Tropical formatting
    def addTropical(self, analysisList, phraseList, includeHazards=True):
        newAnalysisList = []
        for entry in analysisList:
            #  Sampling defined as a tuple (field, statistic, temporal rate)
            #  If this is NOT a Wind or WindGust statistic
            if entry[0] not in ["Hazards", "Wind", "WindGust"]:
                #  Add this statistic to the new analysisList
                newAnalysisList.append(entry)
        newAnalysisList += [
                ("Wind", self.vectorModeratedMinMax, [6]),
                ("WindGust", self.moderatedMinMax, [6]),
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

    def Period_1(self):
        analysisList = [
            #("MinT", self.avg),
            #("MaxT", self.avg),
            ("MinT", self.stdDevMinMax),
            ("MaxT", self.stdDevMinMax),
            ("T", self.hourlyTemp),
            ("T", self.minMax),
            ("Sky", self.median, [3]),
            ("PoP", self._PoP_analysisMethod("Period_1"), [3]),
            ("PoP", self.binnedPercent, [3]),
            ("SnowAmt", self.accumMinMax),
            ("StormTotalSnow", self.accumMinMax),
            ("IceAccum", self.accumMinMax),
            ("SnowLevel", self.avg),
            ("Wind", self.vectorMedianRange, [6]),
            ("Wind", self.vectorMinMax, [6]),
            ("WindGust", self.maximum, [6]),
            ("Wx", self.rankedWx, [3]),
            ("WindChill", self.minMax, [6]),
            ("HeatIndex", self.minMax, [6]),
            ]
        phraseList = [
            self.wind_summary,
            self.reportTrends,
            self.sky_phrase,
            self.skyPopWx_phrase,
            self.weather_phrase,
            self.severeWeather_phrase,
            self.heavyPrecip_phrase,
            self.visibility_phrase,
            self.snow_phrase,
            self.total_snow_phrase,
            self.snowLevel_phrase,
            self.iceAccumulation_phrase,
            self.highs_phrase,
            self.lows_phrase,
            #self.highs_range_phrase,
            #self.lows_range_phrase,
            self.steady_temp_trends,
            self.temp_trends,
            self.wind_withGusts_phrase,
            self.lake_wind_phrase,
            self.popMax_phrase,
            self.windChill_phrase,
            # Alternative
            #self.windBased_windChill_phrase,
            self.heatIndex_phrase,
            ]
        
        if self._includeTropical:
            analysisList, phraseList = self.addTropical(analysisList, phraseList)
            
        component =  { 
            "type": "component",
            "methodList": [
                          self.orderPhrases,
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            "analysisList": analysisList,
            "phraseList": phraseList,
##            "additionalAreas": [ 
##                   # Areas listed by weather element that will be
##                   # sampled and analysed.
##                   # E.g. used for reporting population centers for temperatures. 
##                   ("MaxT", ["City1", "City2"]),
##                   ("MinT", ["City1", "City2"]),
##                   ],
##            "intersectAreas": [ 
##                   # Areas listed by weather element that will be
##                   # intersected with the current area then
##                   # sampled and analysed.  
##                   # E.g. used in local effects methods.
##                   ("MaxT", ["Mountains"]),
##                   ("MinT", ["Valleys"]),
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
                        
    def Period_2_3(self):
        # No Lake Wind phrase
        analysisList =  [
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
                       ("StormTotalSnow", self.accumMinMax),
                       ("IceAccum", self.accumMinMax),
                       ("SnowLevel", self.avg),
                       ("Wind", self.vectorMedianRange, [6]),
                       ("Wind", self.vectorMinMax, [6]),
                       ("WindGust", self.maximum, [6]),
                       ("Wx", self.rankedWx, [6]),
                       ("WindChill", self.minMax, [6]),
                       ("HeatIndex", self.minMax, [6]),
                      ]
        phraseList = [
                   self.wind_summary,
                   self.reportTrends,
                   self.sky_phrase,
                   self.skyPopWx_phrase,
                   self.weather_phrase,
                   self.severeWeather_phrase,
                   self.heavyPrecip_phrase,
                   self.visibility_phrase,
                   self.snow_phrase,
                   self.total_snow_phrase,
                   self.snowLevel_phrase,
                   self.iceAccumulation_phrase,
                   self.highs_phrase,
                   self.lows_phrase,
                   #self.highs_range_phrase,
                   #self.lows_range_phrase,
                   self.steady_temp_trends,
                   self.temp_trends,
                   self.wind_withGusts_phrase,
#                   self.lake_wind_phrase,
                   self.popMax_phrase,
                   self.windChill_phrase,
                   self.heatIndex_phrase,
                  ]
        
        if self._includeTropical:
            analysisList, phraseList = self.addTropical(analysisList, phraseList)

        component = {
            "type": "component",
            "methodList": [
                          self.orderPhrases,
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,         
                          ],
            "analysisList": analysisList,
            "phraseList": phraseList,
            }
        if self._arealSkyAnalysis:
            component["analysisList"].append(("Sky", self.binnedPercent, [6]))
        if self._useStormTotalSnow:
            phraseList = component["phraseList"]
            index = phraseList.index(self.total_snow_phrase)
            phraseList[index] = self.stormTotalSnow_phrase
            component["phraseList"] = phraseList
        return component
        
    def Period_4_5(self):
        # Descriptive snow phrase

        analysisList =  [
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
                       ("WindGust", self.maximum, [6]),
                       ("Wx", self.rankedWx, [6]),
                       ("WindChill", self.minMax, [6]),
                       ("HeatIndex", self.minMax, [6]),
                      ]
        phraseList = [
                   self.wind_summary,
                   self.reportTrends,
                   self.sky_phrase,
                   self.skyPopWx_phrase,
                   self.weather_phrase,
                   self.severeWeather_phrase,
                   self.heavyPrecip_phrase,
                   self.visibility_phrase,
                   self.descriptive_snow_phrase,
                   self.snowLevel_phrase,
                   self.iceAccumulation_phrase,
                   self.highs_phrase,
                   self.lows_phrase,
                   #self.highs_range_phrase,
                   #self.lows_range_phrase,
                   self.steady_temp_trends,
                   self.temp_trends,
                   self.wind_withGusts_phrase,
                   self.popMax_phrase,
                   self.windChill_phrase,
                   self.heatIndex_phrase,
                  ]
        
        if self._includeTropical:
            analysisList, phraseList = self.addTropical(analysisList, phraseList)

        component = {
            "type": "component",
            "methodList": [
                          self.orderPhrases,
                          self.consolidateSubPhrases,
                          self.assemblePhrases,  
                          self.wordWrap,         
                          ],
            "analysisList": analysisList,
            "phraseList": phraseList,
            }
        if self._arealSkyAnalysis:
            component["analysisList"].append(("Sky", self.binnedPercent, [6]))
        return component

    def Period_6_14(self):
        analysisList = [
                       #("MinT", self.avg),
                       #("MaxT", self.avg),
                       ("MinT", self.stdDevMinMax),
                       ("MaxT", self.stdDevMinMax),
                       ("T", self.hourlyTemp),
                       ("T", self.minMax),
                       ("Sky", self.median, [6]),
                       ("PoP", self._PoP_analysisMethod("Period_6_14"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                       ("Wind", self.vectorMedianRange, [12]),
                       ("SnowAmt", self.accumMinMax),
                       ("Wx", self.rankedWx, [6]),
                       ("WindChill", self.minMax, [6]),
                       ("HeatIndex", self.minMax, [6]),
                      ]
        phraseList = [
                   self.wind_summary,
                   self.reportTrends,
                   self.sky_phrase,
                   self.skyPopWx_phrase,
                   self.weather_phrase,
                   self.severeWeather_phrase,
                   self.heavyPrecip_phrase,
                   self.visibility_phrase,
                   self.descriptive_snow_phrase,
                   self.highs_phrase,
                   self.lows_phrase,
                   #self.highs_range_phrase,
                   #self.lows_range_phrase,
                   self.steady_temp_trends,
                   self.temp_trends,
                   self.popMax_phrase,
                   self.windChill_phrase,
                   self.heatIndex_phrase,
                  ]

        if self._includeTropical:
            analysisList, phraseList = self.addTropical(
                analysisList, phraseList, includeHazards=False)

        component =  {
            "type": "component",
            "methodList": [
                          self.orderPhrases,
                          self.consolidateSubPhrases,
                          self.assemblePhrases,  
                          self.wordWrap,         
                          ],
            "analysisList": analysisList,
            "phraseList": phraseList,
            }
        if self._arealSkyAnalysis:
            component["analysisList"].append(("Sky", self.binnedPercent, [6]))
        return component

    def ExtraSampling(self):
        # sampling for temp trends
        return {
            "type": "component",
            "methodList": [self.noWords],
            "analysisList": [
                   ("MaxT", self.stdDevMinMax, [0]),
                   ("MinT", self.stdDevMinMax, [0]),
                   ],
            "phraseList": [],
            "intersectAreas": [],
            }

    def TotalSnowSampling(self):
        return { 
            "type": "component",
            "methodList": [self.noWords],
            "analysisList": [
                       ("SnowAmt", self.moderatedMinMax),
                      ],
            "phraseList":[],
        }

    ###########################################################################
    ### C11 Definitions
    ###########################################################################
    
    def FirstFcstPeriod(self):
        component = {
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
                       ("PoP", self._PoP_analysisMethod("FirstFcstPeriod"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                       ("SnowAmt", self.accumMinMax),
                       ("StormTotalSnow", self.accumMinMax),
                       ("IceAccum", self.accumMinMax),
                       ("SnowLevel", self.avg),
                       ("Wind", self.vectorMedianRange, [6]),
                       ("Wind", self.vectorMinMax, [6]),
                       ("WindGust", self.maximum, [6]),
                       ("Wx", self.rankedWx, [6]),
                       ("WindChill", self.minMax, [6]),
                       ("HeatIndex", self.minMax, [6]),
                      ],
            "phraseList":[
                   self.wind_summary,
                   self.reportTrends,
                   self.sky_phrase,
                   self.skyPopWx_phrase,
                   self.weather_phrase,
                   self.severeWeather_phrase,
                   self.snow_phrase,
                   self.total_snow_phrase,
                   self.snowLevel_phrase,
                   self.highs_phrase,
                   self.lows_phrase,
                   #self.highs_range_phrase,
                   #self.lows_range_phrase,
                   self.steady_temp_trends,
                   self.temp_trends,
                   self.wind_withGusts_phrase,
                   self.lake_wind_phrase,
                   self.popMax_phrase,
                   self.windChill_phrase,
                   self.heatIndex_phrase,
                  ],
            }
        if self._arealSkyAnalysis:
            component["analysisList"].append(("Sky", self.binnedPercent, [6]))
        if self._useStormTotalSnow:
            phraseList = component["phraseList"]
            index = phraseList.index(self.total_snow_phrase)
            phraseList[index] = self.stormTotalSnow_phrase
            component["phraseList"] = phraseList
        return component

    def AreaFcstPeriod(self):
        component = {
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
                       ("PoP", self._PoP_analysisMethod("AreaFcstPeriod"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                       ("SnowAmt", self.accumMinMax),
                       ("StormTotalSnow", self.accumMinMax),
                       ("IceAccum", self.accumMinMax),
                       ("SnowLevel", self.avg),
                       ("Wind", self.vectorMedianRange, [6]),
                       ("Wind", self.vectorMinMax, [6]),
                       ("Wx", self.rankedWx, [6]),
                       ("WindChill", self.minMax, [6]),
                       ("HeatIndex", self.minMax, [6]),
                      ],
            "phraseList":[
                   self.wind_summary,
                   self.reportTrends,
                   self.sky_phrase,
                   self.skyPopWx_phrase,
                   self.weather_phrase,
                   self.severeWeather_phrase,
                   self.snow_phrase,
                   self.total_snow_phrase,
                   self.snowLevel_phrase,
                   self.highs_phrase,
                   self.lows_phrase,
                   #self.highs_range_phrase,
                   #self.lows_range_phrase,
                   self.steady_temp_trends,
                   self.temp_trends,
                   self.wind_withGusts_phrase,
                   self.popMax_phrase,
                   self.windChill_phrase,
                   self.heatIndex_phrase,
                  ],
            }
        if self._arealSkyAnalysis:
            component["analysisList"].append(("Sky", self.binnedPercent, [6]))
        if self._useStormTotalSnow:
            phraseList = component["phraseList"]
            index = phraseList.index(self.total_snow_phrase)
            phraseList[index] = self.stormTotalSnow_phrase
            component["phraseList"] = phraseList
        return component

    def Evening(self):
        component =  {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            "analysisList": [
                       ("Sky", self.median, [6]),
                       ("PoP", self._PoP_analysisMethod("Evening"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                       ("Wind", self.vectorMedianRange),
                       ("Wind", self.vectorMinMax),
                       ("Wx", self.rankedWx, [6])
                      ],
            "phraseList":[
                   self.wind_summary,
                   self.sky_phrase,
                   self.weather_phrase,
                   #self.popMax_phrase
                  ],
            }
        if self._arealSkyAnalysis:
            component["analysisList"].append(("Sky", self.binnedPercent, [6]))
        return component

    def LaterPeriod(self):
        component =  {
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
                       ("Sky", self.median, [6]),
                       ("PoP", self._PoP_analysisMethod("LaterPeriod"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                       ("Wind", self.vectorMedianRange, [6]),
                       ("Wind", self.vectorMinMax, [6]),
                       ("Wx", self.rankedWx, [6])
                      ],
            "phraseList":[
                   self.wind_summary,
                   self.reportTrends,
                   self.sky_phrase,
                   self.skyPopWx_phrase,
                   self.weather_phrase,
                   # Uncomment if you want detailed winds included:
                   #self.wind_withGusts_phrase,
                   self.highs_phrase,
                   self.lows_phrase,
                   self.temp_trends,
                   self.popMax_phrase
                  ],
            }
        if self._arealSkyAnalysis:
            component["analysisList"].append(("Sky", self.binnedPercent, [6]))
        return component

    def LaterPeriodWithoutLows(self):
        component =  {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            "analysisList": [
                       #("MaxT", self.avg),
                       ("MaxT", self.stdDevMinMax),
                       ("T", self.hourlyTemp),
                       ("Sky", self.median, [6]),
                       ("PoP", self._PoP_analysisMethod("LaterPeriodWithoutLows"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                       ("Wind", self.vectorMedianRange, [6]),
                       ("Wind", self.vectorMinMax, [6]),
                       ("Wx", self.rankedWx, [6])
                      ],
            "phraseList":[
                   self.wind_summary,
                   self.reportTrends,
                   self.sky_phrase,
                   self.skyPopWx_phrase,
                   self.weather_phrase,
                   self.highs_phrase,
                   self.popMax_phrase
                  ],
            }
        if self._arealSkyAnalysis:
            component["analysisList"].append(("Sky", self.binnedPercent, [6]))
        return component

    def C11Extended(self):
        component =  {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            "analysisList": [
                       ("MinT", self.firstAvg),
                       ("MaxT", self.avg),
                       ("T", self.hourlyTemp),
                       ("Sky", self.median, [12]),
                       ("PoP", self._PoP_analysisMethod("C11Extended"), [12]),
                       ("PoP", self.binnedPercent, [12]),
                       ("Wind", self.vectorMedianRange),
                       ("Wind", self.vectorMinMax),
                       ("Wx", self.rankedWx, [12]),
                      ],
            "phraseList":[
                   self.wind_summary,
                   #self.reportTrends,
                   self.sky_phrase,
                   self.weather_phrase,
                   self.extended_lows_phrase,
                   self.extended_highs_phrase,
                 ],
            }
        if self._arealSkyAnalysis:
            component["analysisList"].append(("Sky", self.binnedPercent, [6]))
        return component

    def ExtendedLabel(self):
        return {
            "type": "component",
            "methodList": [
                          self.setLabel,           
                          ],
            "analysisList": [],
            "phraseList":[],
            }
    def setLabel(self, tree, component):
        component.set("words", "\n.EXTENDED...\n")
        return self.DONE()

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
        self.setProgressPercentage(50)
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

        #Tropical exceptions
        try:
            self._includeTropical = self._includeTropical == "Yes"
        except:
            self._includeTropical = False
        # VERY IMP: Period combination will break the Tropical Formatters.
        # So regardless of what the user enters through the Definition file
        # the variable is set to 0 here if running the Tropical Formatters.
        # The same comment applies to the specification of the directive type.
        # Tropical Formatters will only work with that specified to be 10-503
        # so it is forced like so here.
        if self._includeTropical:
            self._periodCombining = 0
            self._directiveType = "10-503"
            if self._productIssuance == "Morning with Pre-1st Period":
                self._productIssuance = "Morning"
            if self._productIssuance == "Afternoon with Pre-1st Period":
                self._productIssuance = "Afternoon"

        self._language = argDict["language"]
        return None

    def _determineTimeRanges(self, argDict):
        # Set up the Narrative Definition and initial Time Range
        self._issuanceInfo = self.getIssuanceInfo(
            self._productIssuance, self._issuance_list(argDict))

        if not self._useStormTotalSnow:
            # Add a "custom" component to sample SnowAmt for 12 hours
            # prior to product start
            productStart = self._issuanceInfo.timeRange().startTime()
            tr = TimeRange.TimeRange(productStart - 12*3600, productStart)
            self._issuanceInfo.narrativeDef().append(\
                ("Custom", ("TotalSnowSampling", tr)))
                
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
        self._timeLabel = self.getCurrentTime(
            argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1)
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
        # Product header
        if self._areaName != "":
             productName = self._productName.strip() + " FOR " + \
                           self._areaName.strip()
        else:
             productName = self._productName.strip()

        productName = self.checkTestMode(argDict, productName)
            
        issuedByString = self.getIssuedByString()

        fcst =  fcst + self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n" +\
               productName + "\n" +\
               "NATIONAL WEATHER SERVICE " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n\n"

        # The following lines insert a statement
        # at the top of the forecast that describes the time periods
        # of the temp/pop table.  Comment out the lines if you
        # do not want this statement.
        if self._includeMultipleElementTable == 1:
            fcst = self._temp_pop_statement(fcst, argDict)

        # Set the "includeFloodingQuestion" flag if you want the possibility of
        # a flood statement.
        fcst = self._flood_statement(fcst, argDict)
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
        return fcst + "\n$$\n\n"

    def _postProcessProduct(self, fcst, argDict):
        fcst = string.replace(fcst, "%expireTime", self._expireTimeStr)
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst

    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################

    def _issuance_list(self, argDict):
        #  This method sets up configurable issuance times with associated
        #  narrative definitions.  See the Text Product User Guide for documentation.
        if self._definition["directiveType"] == "C11":
            return self._C11_issuance_list(argDict)
        else:
            return self._10_503_issuance_list(argDict)

    def _C11_issuance_list(self, argDict):
        try:
            if self._definition["includeEveningPeriod"] == 1:
                narrativeDefAM = [
                    ("FirstFcstPeriod", "period1"),
                    ("AreaFcstPeriod", 12), ("AreaFcstPeriod", 12), 
                    ("LaterPeriod", 12), ("LaterPeriod", 12), 
                    ("Evening", 6), 
                    ("Custom", ("ExtraSampling", (-24, 12))),
                    ]
                narrativeDefPM = [
                    ("FirstFcstPeriod", "period1"),
                    ("AreaFcstPeriod", 12), ("AreaFcstPeriod", 12), 
                    ("LaterPeriod", 12), ("LaterPeriod", 12), ("LaterPeriod", 12), 
                    ("Evening", 6), 
                    ("Custom", ("ExtraSampling", (-24, 24))),
                    ]
            else:
                narrativeDefAM = [
                    ("FirstFcstPeriod", "period1"),
                    ("AreaFcstPeriod", 12), ("AreaFcstPeriod", 12), 
                    ("LaterPeriod", 12), ("LaterPeriodWithoutLows", 18),
                    ("Custom", ("ExtraSampling", (-24, 12))),
                    ]            
                narrativeDefPM = [
                    ("FirstFcstPeriod", "period1"),
                    ("AreaFcstPeriod", 12), ("AreaFcstPeriod", 12), 
                    ("LaterPeriod", 12), ("LaterPeriod", 12), ("LaterPeriodWithoutLows", 18),
                    ("Custom", ("ExtraSampling", (-24, 24))),
                    ]
            extended = [("C11Extended", 24),("C11Extended", 24), ("C11Extended", 24),("C11Extended", 24)]
            if self._includeExtended == 1:
                if self._extendedLabel == 1:
                    narrativeDefAM.append(("ExtendedLabel",0))
                    narrativeDefPM.append(("ExtendedLabel",0))
                narrativeDefAM = narrativeDefAM + extended
                narrativeDefPM = narrativeDefPM + extended
        except:
            narrativeDefAM = None
            narrativeDefPM = None

        #(description, startHour, endHour, expireHour,
        # period1 label, period1 lateNight lateDay phrase, todayFlag, narrative definition)
        return [
            ("Morning", self.DAY(), self.NIGHT(), 16,
             ".Today...", "early in the morning", "late in the afternoon",
             1, narrativeDefAM), 
            ("Morning with Pre-1st Period", self.DAY()-2, self.NIGHT(), 16,
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
            ("Afternoon with Pre-1st Period", self.NIGHT()-2, 24 + self.DAY(), 24 + 4,
             ".Tonight...", "late in the night", "early in the evening",
             1, narrativeDefPM), 
            ("Evening Update", "issuanceHour", 24 + self.DAY(), 24 + 4,
             ".Rest of Tonight...", "late in the night","early in the evening",
             1, narrativeDefPM),
            ("Early Morning Update", "issuanceHour", self.DAY(), 4,
             ".Rest of Tonight...", "early in the morning","late in the afternoon",
             0, narrativeDefPM), 
            ]

    def _10_503_issuance_list(self, argDict):
        narrativeDefAM = [
            ("Period_1", "period1"),            
            ("Period_2_3", 12), ("Period_2_3", 12), 
            ("Period_4_5", 12), ("Period_4_5", 12), 
            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), 
            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), 
            ("Custom", ("ExtraSampling", (-24, 12))),
            ]
        narrativeDefPM = [
            ("Period_1", "period1"),
            ("Period_2_3", 12), ("Period_2_3", 12), 
            ("Period_4_5", 12), ("Period_4_5", 12), 
            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), 
            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), 
            ("Period_6_14", 12),
            ("Custom", ("ExtraSampling", (-24, 24))),
            ]
        
        return [
            ("Morning", self.DAY(), self.NIGHT(), 16,
             ".Today...", "early in the morning", "late in the afternoon",
             1, narrativeDefAM), 
            ("Morning with Pre-1st Period", self.DAY()-2, self.NIGHT(), 16,
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
            ("Afternoon with Pre-1st Period", self.NIGHT()-2, 24 + self.DAY(), 24 + 4,
             ".Tonight...", "late in the night", "early in the evening",
             1, narrativeDefPM), 
            ("Evening Update", "issuanceHour", 24 + self.DAY(), 24 + 4,
             ".Rest of Tonight...", "early in the morning","early in the evening",
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
            #("Evening Update", "issuanceHour", 24 + self.DAY(), 4,
            # ".Rest of Tonight...", "late in the night", "early in the evening",
            # 1, narrativeDefPM), 
            #("Early Morning Update", "issuanceHour", self.DAY(), 4,
            # ".EARLY THIS MORNING...", "early in the morning", "late in the afternoon",
            # 1, narrativeDefPM), 
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
                
    # Some Western Region offices insert a special statement
    # at the top of the ZFP for the temp/PoP table time periods.
    # This function creates the statement.  Usually this is called
    # by preProcessProduct. This code was contributed by Ron Miller,
    # NWS Spokane
    def _temp_pop_statement(self, fcst, argDict):
        #  Determine the number of periods based on what package we're issuing.
        myTimeRanges = self.getMultipleElementTableRanges(
            self._productIssuance, self._singleValueFormat)
        
        header = self._temp_pop_statement_header(argDict)

        #  Loop through each period
        periodNum = 1
        numPeriods = len(myTimeRanges)
        for myTimeRange, label in myTimeRanges:
            #  Add it on to the header
            if periodNum == numPeriods:
                header = header + "AND "
            header = header + label
            periodNum = periodNum + 1
            
        header = self.endline(header,argDict["lineLength"])
        fcst = fcst + header[0:-3] + "\n\n"
        return fcst
    
    def _temp_pop_statement_header(self, argDict):
        return  "SPOT TEMPERATURES AND PROBABILITIES OF MEASURABLE PRECIPITATION\nARE FOR "

    # Western Region offices insert a special statement
    # at the top of the ZFP if there are any active flood watches or
    # warnings.  Usually this is called by preProcessProduct.
    # This code was contributed by Ron Miller, NWS Spokane
    def _flood_statement(self, fcst, argDict):
        #  Now add in a hydro statement if flooding is occuring
        varDict = argDict["varDict"]
        try:
            self._flooding = varDict["Flooding?"]
        except:
            self._flooding = "No"
        if self._flooding == "Yes":
            fcst = fcst + self._flood_statement_wording(argDict)                        
        return fcst

    def _flood_statement_wording(self, argDict):
        return "...FLOOD WATCHES AND/OR WARNINGS HAVE BEEN ISSUED FOR PORTIONS OF\n" \
                        + "THE ZONE FORECAST AREA. PLEASE REFER TO THE LATEST FLOOD BULLETIN\n" \
                        + "FOR DETAILS...\n\n"
    
    # Returns a list of the Hazards allowed for this product in VTEC format.
    # These are sorted in priority order - most important first.
    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        tropicalActions = ["NEW", "EXA", "EXB", "EXT", "UPG", "CAN", "CON", 
          "EXP"]
        return [
            ('HU.W', tropicalActions, 'Tropical'),     # HURRICANE WARNING
            ('TY.W', tropicalActions, 'Tropical'),     # TYPHOON WARNING
            ('TR.W', tropicalActions, 'Tropical1'),     # TROPICAL STORM WARNING
            ('HU.A', tropicalActions, 'Tropical'),     # HURRICANE WATCH
            ('TY.A', tropicalActions, 'Tropical'),     # TYPHOON WATCH
            ('TR.A', tropicalActions, 'Tropical1'),     # TROPICAL STORM WATCH
            ('HF.W', allActions, 'Marine'),       # HURRICANE FORCE WIND WARNING
            ('BZ.W', allActions, 'WinterWx'),     # BLIZZARD WARNING
            ('IS.W', allActions, 'WinterWx'),     # ICE STORM WARNING
            ('LE.W', allActions, 'WinterWx'),     # LAKE EFFECT SNOW WARNING
            ('WS.W', allActions, 'WinterWx'),     # WINTER STORM WARNING
            ('ZR.Y', allActions, 'WinterWx'),     # FREEZING RAIN ADVISORY
            ('LE.Y', allActions, 'WinterWx'),     # LAKE EFFECT SNOW ADVISORY
            ('WW.Y', allActions, 'WinterWx'),     # WINTER WEATHER ADVISORY
            ('BZ.A', allActions, 'WinterWx'),     # BLIZZARD WATCH
            ('LE.A', allActions, 'WinterWx'),     # LAKE EFFECT SNOW WATCH
            ('WS.A', allActions, 'WinterWx'),     # WINTER STORM WATCH
            ('WC.W', allActions, 'WindChill'),    # WIND CHILL WARNING
            ('WC.Y', allActions, 'WindChill'),    # WIND CHILL ADVISORY
            ('WC.A', allActions, 'WindChill'),    # WIND CHILL WATCH
            ('DS.W', allActions, 'Dust'),         # DUST STORM WARNING
            ('DU.Y', allActions, 'Dust'),         # BLOWING DUST ADVISORY
            ('EC.W', allActions, 'Cold'),         # EXTREME COLD WARNING
            ('EC.A', allActions, 'Cold'),         # EXTREME COLD WATCH
            ('EH.W', allActions, 'Heat'),         # EXCESSIVE HEAT WARNING
            ('EH.A', allActions, 'Heat'),         # EXCESSIVE HEAT WATCH
            ('HT.Y', allActions, 'Heat'),         # HEAT ADVISORY
            ('FG.Y', allActions, 'Fog'),          # DENSE FOG ADVISORY
            ('HZ.W', allActions, 'FrostFreeze'),  # HARD FREEZE WARNING
            ('FZ.W', allActions, 'FrostFreeze'),  # FREEZE WARNING
            ('FR.Y', allActions, 'FrostFreeze'),  # FROST ADVISORY
            ('HZ.A', allActions, 'FrostFreeze'),  # HARD FREEZE WATCH
            ('FZ.A', allActions, 'FrostFreeze'),  # FREEZE WATCH
            ('HW.W', allActions, 'Wind'),         # HIGH WIND WARNING
            ('WI.Y', allActions, 'Wind'),         # WIND ADVISORY
            ('LW.Y', allActions, 'Wind'),         # LAKE WIND ADVISORY
            ('HW.A', allActions, 'Wind'),         # HIGH WIND WATCH
            ('SM.Y', allActions, 'Smoke'),        # DENSE SMOKE ADVISORY
            ('ZF.Y', allActions, 'FreezeFog'),    # FREEZING FOG ADVISORY
            ('FF.A', allActions, 'Flood'),        # FLASH FLOOD WATCH
            ('FA.A', allActions, 'Flood'),        # FLOOD WATCH
            ('FA.W', allActions, 'Flood'),        # FLOOD WARNING
            ('FA.Y', allActions, 'Flood'),        # FLOOD ADVISORY    
            ('CF.W', allActions, 'CoastalFlood'), # COASTAL FLOOD WARNING
            ('LS.W', allActions, 'CoastalFlood'), # LAKESHORE FLOOD WARNING
            ('CF.Y', allActions, 'CoastalFlood'), # COASTAL FLOOD ADVISORY
            ('LS.Y', allActions, 'CoastalFlood'), # LAKESHORE FLOOD ADVISORY
            ('CF.A', allActions, 'CoastalFlood'), # COASTAL FLOOD WATCH
            ('LS.A', allActions, 'CoastalFlood'), # LAKESHORE FLOOD WATCH
            ('UP.W', allActions, 'IceAccr'),      # ICE ACCRETION WARNING
            ('UP.Y', allActions, 'IceAccr'),      # ICE ACCRETION ADVISORY
            ('AS.Y', allActions, 'AirStag'),      # AIR STAGNATION ADVISORY
            ('AS.O', allActions, 'AirStag'),      # AIR STAGNATION OUTLOOK
            ('SU.W', allActions, 'HighSurf'),     # HIGH SURF WARNING
            ('SU.Y', allActions, 'HighSurf'),     # HIGH SURF ADVISORY
            ('RP.S', allActions, 'Rip'),          # HIGH RIP CURRENT RISK
            ('AF.W', allActions, 'Ashfall'),      # VOLCANIC ASHFALL WARNING
            ('AF.Y', allActions, 'Ashfall'),      # VOLCANIC ASHFALL ADVISORY
            ('LO.Y', allActions, 'Drought'),      # LOW WATER ADVISORY
            ('TO.A', allActions, 'Convective'),   # TORNADO WATCH
            ('SV.A', allActions, 'Convective'),   # SEVERE THUNDERSTORM WATCH
             ]
