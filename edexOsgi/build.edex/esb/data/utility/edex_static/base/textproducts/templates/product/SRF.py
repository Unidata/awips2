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
# Description: This produces a Surf Zone Forecast.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# SRF, SRF_TBW_Definition, SRF_TBW_Overrides
#-------------------------------------------------------------------------
# Customization Points:
#
# DEFINITION SECTION
#
# Required Configuration Items:
#
#  displayName        If not None, defines how product appears in GFE GUI
#  
#  areaDictionary     Create a SurfAreaDictionary with entries as described below.
#                     The SurfAreaDictionary appears in the TextUtilities window
#                     of the DefineTextProducts dialog.  You can use it as a
#                     starting point to create your own.
#
#       The SRF formatter is unique in that the various phrases are based on
#       different edit areas.  The SurfAreaDictionary specifies which areas
#       to use for each type of phrase.
#
#       For example, the sky, weather, and wind information
#       typically comes from a coastal land/sea edit area which you
#       will need to create while the swell information comes from a
#       coastal waters area. Meanwhile, each segment of the product is
#       based on a public zone.
#
#       The "additionalAreasDict", which can be overridden, lists
#       the weather elements, analysis, and phrases for each type of edit area.
#
#       The SurfAreaDictionary has an entry for each public zone
#       for which you want a SRF product segment.
#       You can copy the information from the AreaDictionary as a starting point.
#       Then add the following information for each zone:
#
#       "landSeaArea": An edit area you need to create which contians grid
#                      points along the coast, including both land and sea. 
#       "marineArea": Typically, the coastal waters area.
#       "surfAreas": The surfAreas entry is an optional list of edit areas and labels
#                    for which to create a surf (waveHeight) phrase.
#                    For example, If you have:
#
#           surfAreas: [           
#               ("WestCoast",  "SURF ALONG WEST FACING REEFS.............."),
#               ("NorthCoast", "SURF ALONG NORTH FACING REEFS............."),
#               ("EastCoast",  "SURF ALONG EAST FACING REEFS.............."),
#               ("SouthCoast", "SURF ALONG SOUTH FACING REEFS............."),
#               ]
#
#            You would get a surfHeight report for each surfArea listed:
#                
#               SURF ALONG WEST FACING REEFS................10 TO 12 FEET.
#               SURF ALONG NORTH FACING REEFS...............4 TO 6 FEET.
#               SURF ALONG EAST FACING REEFS................2 TO 3 FEET.
#               SURF ALONG SOUTH FACING REEFS...............4 TO 6 FEET.
#
#             If the list is empty, you will simply get surfHeight reported
#             for the current value of the WaveHeight Grid sampled from the
#             landSea edit area:
#
#               SURF................1 TO 2 FEET.
#
#        "uviCity": The UVI index is take from the previous UVI product (pil is UVI<siteID>).
#               This specifies the city name from which to report the uvi index.
#        "tideTables": A list of the tide tables from which you want tides reported
#               for each public zone.
#               Tide information is taken from files that you must set-up locally.
#               Make a file for each tide table that you want reported.
#               You can include a year's worth of tide information and update the files once
#               a year.
#               Then list the tide tables and corresponding files in the "tideFiles"
#               Definition entry (see below).
#               Tide tables should be in the format found at the website:
#                    http://co-ops.nos.noaa.gov/tides05/
#               
#  defaultEditAreas defines edit areas, default is Combinations.
#                   Note that zones can be combined for the SRF product.
#                   If so, the corresponding landSeaAreas, marineAreas
#                   will be combined, and the surfAreas and tideTables
#                   handled correctly.
#                   Note that if you always have the same combinations,
#                   you need only list this additional information for
#                   one of the zones in each combination.
#  productName      defines name of product e.g. "ZONE FORECAST PRODUCT"
#  fullStationID    Full station identifier, 4 letter, such as "KSLC".
#  wmoID            WMO ID code for product header, such as "FOUS45"
#  pil              Product pil, such as "SFTBOS"
#  areaName (opt.)  Area name for product header, such as "WESTERN NEW YORK"
#  wfoCityState     WFO location, such as "BUFFALO NY"
#
# Optional Configuration Items
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
#   periodCombining    If 1, an attempt will be made to combine components
#                      or time periods into one.  Otherwise no period
#                      combining will will be done.
#   individualExtended  If individualExtended == 1, an extended forecast will be
#                       generated for each individual area
#   extendedLabel    If extendedLabel== 1, a label will be included for each
#                    individual extended
#   tempLocalEffects    Set to 1 to after defining edit areas Inland
#                       and Coastal to enable local effects for temperature
#   windLocalEffects    Set to 1 after defining edit areas Inland and Coastal
#                       to enable local effects for wind.
#                       If you change these edit area names,
#                       make sure that the names in the "getIntersectAreas"
#                       and "valleyRidgeAreaNames" are set to the new names.
#   surfGrid         Name of grid to use for the waveHeight_phrase. Default is SurfHeight,
#                    but can be set to WaveHeight for sites not creating a SurfHeight grid.
#   ripGrid          If set to the name of a rip current grid, this will be used
#                    for the rip_phrase instead of using Wind Grid and SurfHeight/WaveHeight
#                    Grid values to calculate a rip_phrase value.
#   waterSpoutGrid   If you want a waterSpout phrase, set this to your Water Spout grid
#                    name.  Also, make sure to add this to the analysisList for
#                    "marineArea" in the "_additionalAreasDict".
#   includeOutlook   If 1, OUTLOOK section will be included. The OUTLOOK section is
#                    is a hand-editied narrative outlook.
#   tideFiles        Make a file for each tide table that you want reported.
#                    Tide tables should be in the format given at the website:
#                    http://co-ops.nos.noaa.gov/tides05/
#   includeTideHeights  If 1, tide heights will be included in the tide output.
#   extraAreaPrefix  Prefix for extra temporary edit areas that must be created to run
#                    this product e.g. combinations of landSeaAreas
#   surfDebug        If 1, produces information showing the extra edit areas created
#                    and which areas are used for which phrases.
#
# Other Important Configuration Items:
#
#    --To Add or Remove phrases from the SRF, override the getSurfPeriod_phraseList method
#
#    --To Change the analysisList OR to specify which areas to use for which phrases,
#      override the _additionalAreasDict.
#
#    --If you want to get the previous synopsis, and _uvi statement,
#       set Definition section "statePil"
#
#    --Override "seaBreeze_thresholds" (ConfigVariables)
#      to set up offshore wind values for the chop_phrase
#
#-------------------------------------------------------------------------
# Weather Elements Needed:
#   Hazards (optional):  If provided, headlines will be generated.
#   Sky, Wind (6 hourly), PoP, MaxT, MinT, Sky, Wind, T, HeatIndex, LAL
#   Wx, WaveHeight OR SurfHeight, Swell, Period, Swell2, Period2
#   Optional:
#   rip current grid (scalar values 1-3 for LOW, MODERATE, HIGH)
#   water spout grid (scalar values 1-5)
#
#-------------------------------------------------------------------------
# OTHER INPUTS
#   Tides from file, UVI from Awips
#-------------------------------------------------------------------------
# Component Products:
#      SurfPeriod (component)
#      ExtendedLabel(component)
#      SurfExtended (component)
#-------------------------------------------------------------------------
# Programmers and Support including product team leader's email:
# Product Team POC: Charlie Paxton <Charlie.Paxton@noaa.gov>
# FSL POC: Tracy Hansen <hansen@fsl.noaa.gov>
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
# To look up tasks and their status, see the Text Product User Guide
# Section on "Tkgnats: Task Reporting System".
#-------------------------------------------------------------------------
# Example Output:
#  Refer to the NWS Directives for Marine Services.
#-------------------------------------------------------------------------
# Implementation Notes
#
#   The _sampleData method must first call _getCombinedAdditionalAreas
#    to create combinations of additional edit areas based on the
#    combinations the user selected for the public zones for which the
#    segments of the product are reported.
#    These combinations are stored in the _combinedAreaDict:
#      {combo: {areaType: areaLabel}
#   Then _sampleData calls ForecastNarrative.getNarrativeData to interpret
#    the narrative definition, the components of which can now use
#    the _combinedAreaDict in creating their "additionalAreas",
#    "additionalAreasAnalysisList", intersectAreas", intersectWithAreas",
#    and "intersectAnalysisList" entries.
#   Finally, each phrase setUp method will call _setSurfAreaLabel to look
#    up and set the areaLabel for the phrase using the configurable
#    _additionaAreasDict:
#        {areaType: {analysisList: [], phraseList: []}}
#    and the _combinedAreaDict.
#    
#-------------------------------------------------------------------------
 
import TextRules
import SampleAnalysis
import ForecastNarrative
import time, string, types, os, re, copy

class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):

##    VariableList = [
##             (("Issuance Type", "issuanceType") , "ROUTINE", "radio",
##              ["ROUTINE", "UPDATE", "CORRECTION"]),
##             ]
   
    Definition =  {
        "type": "smart",
        "displayName": "None",
        "statePil": "MIA",
        "outputFile": "{prddir}/TEXT/SRF_<MultiPil>.txt",

##########################################################################       
##### Edit Areas: Create Combinations file with edit area combinations.
        # Name of map background for creating Combinations
        "mapNameForCombinations": "Zones_TBW",
        "showZoneCombiner" : 1, # 1 to cause zone combiner to display
        "defaultEditAreas" : "Combinations_SRF_<site>_<MultiPil>",

##########################################################################       
##### Product identifiers
        # Source database for product. Can be "Official", "Fcst" or "ISC"
        "database": "Official",
        "debug": 0,
        "productName": "SURFZONE FORECAST", # product name
        "fullStationID": "<fullStationID>",    # full station identifier (4letter)
        "wmoID": "<wmoID>",          # WMO ID
        "pil": "<pil>",            # Product pil
        "areaName": "<state>",     # Name of state, such as "GEORGIA" -- optional
        "wfoCityState": "<wfoCityState>", # Location of WFO - city and state name
       
        "textdbPil": "<textdbPil>",       # Product ID for storing to AWIPS text database.
        "awipsWANPil":"<awipsWANPil>" ,   # Product ID for transmitting to AWIPS WAN.
        "srfPil": "SRFTBW",
        "uviPil": "UVITBW",
                                 
        "periodCombining" : 1,       # If 1, combine periods, if possible
        "tempLocalEffects": 0,       # Set to 1 to enable Temp local effects AFTER
                                     # creating Inland and Coastal edit areas
        "windLocalEffects": 0,       # Set to 1 to enable wind local effects AFTER
                                     # creating Inland and Coastal edit areas
        #"surfGrid": "SurfHeight",   # Use grid for waveHeight_phrase
        "surfGrid": "WaveHeight",    # Use grid for waveHeight_phrase
        "ripGrid": "",               # Use grid for rip_phrase
        "waterSpoutGrid": "",        # Use grid for waterSpout_phrase
        "includeOutlook": 0,         # If 1, OUTLOOK section included
        "outLookText": "\n.OUTLOOK...",# Text for OUTLOOK section
        "tideFiles": {               # For each tide table, list the file where it can
                                     # be found
             "Venice Inlet": "/data/local/localapps/tides/VeniceInlet.txt",
             "Saint Petersburg": "/data/local/localapps/tides/SaintPetersburg.txt",
             "Fort Myers": "/data/local/localapps/tides/FortMyers.txt",
             "Cedar Key": "/data/local/localapps/tides/CedarKey.txt",
             },
        "includeTideHeights": 0,
        "extraAreaPrefix": "__ExtraSurfArea",
        "surfDebug": 1,
                 
        #"purgeTime": 15,
       
##########################################################################       
##### Product variables
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
        "areaDictionary": "SurfAreaDictionary",
        "language": "english",
        "synopsisUGC": "",                # UGC code for synopsis
        "synopsisHeading": ".SYNOPSIS...",# Heading for synopsis
        # If individualExtended == 1, an extended forecast will be
        # generated for each individual area
        # If extendedLabel == 1, a label will be included for each
        #  individual extended
        "individualExtended": 1,
        "extendedLabel": 0,
        "useHolidays": 1,            # Set to 1 to use holidays in the time period labels
        "sampleFromServer": 0,       # If 1, sample directly from server
        # Trouble-shooting items
        # Trouble-shooting items
        "passLimit": 20,             # Limit on passes allowed through
                                     # Narrative Tree
        "trace": 0,                  # Set to 1 to turn on trace through
        "lineLength": 70,                             # Narrative Tree for trouble-shooting
        }

    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)

############################################################################
##### OVERRIDING THRESHOLDS AND VARIABLES

    def minMax_std_deviation(self, parmHisto, timeRange, componentName):
        # Replaces MINMAX_STD_DEVIATION
        # Number of standard deviations to compute around the weighted
        # average for a stdDev_MinMax
        return 1.4

    def element_outUnits_dict(self, tree, node):
        dict = TextRules.TextRules.element_outUnits_dict(self, tree, node)
        dict["Wind"] = "mph"
        dict["WindGust"] = "mph"
        return dict

    def phrase_descriptor_dict(self, tree, node):
        # Dictionary of descriptors for various weather elements in phrases
        # The value for an element may be a phrase or a method
        # If a method, it will be called with arguments:
        #   tree, node, key, element
        return {
            "WaveHeight" : "surf................",
            "Swell":  "swell",
            "Swell2": "swell",
            "LabelSwell":  "swell...............",
            "LabelSwell2": "secondary swell.....",
            "Period": "period..............",
            "Period2":"secondary period....",
            "chop" : "water condition.....",
            "rip" :  "rip current risk....",
            "HeatIndex": "heat index..........",
            "20-FOOT WINDS......." : "Beach WINDS.........",
            "MaxT_FireWx":"MAX TEMPERATURE.....",
            "SKY/WEATHER.........":  "SKY/WEATHER.........",
            "sst" : "WATER TEMPERATURE...",
            "uvi" : "UVI INDEX...........",
            "LAL" : "LIGHTNING THREAT....",
            "WaterSpout" : "WATERSPOUT THREAT...",
            "PoP" : "CHANCE OF...........",           
            "MinT":"lows",
            "MaxT":"highs",
            "Wind": "winds",           
            #  Used for Headlines
            "EXPECTED" : "EXPECTED",
            "IN EFFECT" : "IN EFFECT",
            # Used for single values
            "around": "around ",
            "    VALLEYS/LWR SLOPES...": "    INLAND...............",
            "    RIDGES/UPR SLOPES....": "    COASTAL..............",
            }

############################################################################
###### COMPONENT PRODUCT DEFINITIONS
######   
############################################################################
##### SURF PERIOD AND ISSUANCE LISTS

    def _getSurfPeriod_phraseList(self):
        # Override this to add or remove phrases from the SRF product.
        phraseList =  [
            self.skyWeather_byTimeRange_compoundPhrase,
            self.popMax_phrase,
            (self.dayOrNight_phrase, ["MaxT", "MinT", 1, 1], self._tempLocalEffects_list()),
            self.severeWeather_phrase,
            self.fireWind_compoundPhrase,
            self.fireWind_label_phrase,
            self.fireValleyWind_compoundPhrase,  
            self.fireRidgeWind_compoundPhrase,  
            self.waveHeight_phrase,
            self.swell_compoundPhrase,
            self.period_phrase,
            self.swell2_compoundPhrase,
            self.period2_phrase,
            self.chop_phrase,
            self._sst_phrase,
            self._uvi_phrase,
            self.rip_phrase,
            self.heatIndex_phrase,
            self._lal_phrase,
            ]
        if self._waterSpoutGrid != "":
            phraseList.append(self._waterSpout_phrase)
        # Add multipleElementTable
        if self._includeMultipleElementTable_perPeriod:
            phraseList.append(self.multipleElementTable_perPeriod_phrase)
        return phraseList

    def _additionalAreasDict(self, component):
        # This dictionary is used to build the SurfPeriod.
        # Override this to
        #    --specify which areas to use for which phrases,
        #    --change the analysisLists
        # {areaType: {analysisList: [], phraseList: []}}
        if component == "SurfPeriod":
            return self._surfPeriodAdditionalAreasDict()
        elif component == "SurfExtended":
            return self._surfExtendedAdditionalAreasDict()
        elif component == "ExtraSampling":
            return self._extraSamplingAdditionalAreasDict()

    def _surfPeriodAdditionalAreasDict(self):
        return {
            "landSeaArea": {
                "analysisList": [
                   ("Sky", self.minMax, [0]),
                   ("PoP", self._PoP_analysisMethod("SurfPeriod")),
                   ("Wx", self.rankedWx),
                   ("WindGust", self.moderatedMax, [3]),
                   ("WaveHeight", self.maximum, [6]),
                   ("SurfHeight", self.maximum, [6]),
                   ("WindWaveHgt", self.maximum, [6]),
                   ("Wind", self.vectorAvg, [6]),          
                   ("MaxT", self.minMax),
                   ("MinT", self.minMax),
                   ("HeatIndex", self.minMax), 
                   ("T", self.minMax),
                   ("LAL", self.minMax),
                  ],
                "phraseList": [
                    self.skyWeather_byTimeRange_compoundPhrase,
                    self.popMax_phrase,
                    self.dayOrNight_phrase,
                    self.severeWeather_phrase,
                    self.fireWind_compoundPhrase,
                    self.fireWind_label_phrase,
                    self.fireValleyWind_compoundPhrase,  
                    self.fireRidgeWind_compoundPhrase,  
                    self.waveHeight_phrase,
                    self.chop_phrase,
                    self._sst_phrase,
                    self._uvi_phrase,
                    self.rip_phrase,
                    self.heatIndex_phrase,
                    self._lal_phrase,
                  ],
                },        
            "marineArea": {
                "analysisList": [
                   ("Swell", self.vectorModeratedMinMax, [6]),
                   ("Swell2", self.vectorModeratedMinMax, [6]),
                   ("Period", self.moderatedMinMax, [6]),
                   ("Period2", self.moderatedMinMax, [6]),
                  ],
                "phraseList": [
                    self.swell_compoundPhrase,
                    self.period_phrase,
                    self.swell2_compoundPhrase,
                    self.period2_phrase,
                    self._waterSpout_phrase,
                   ],
                },
             }
        
    def _surfExtendedAdditionalAreasDict(self):
        return {
            "landSeaArea": {
                "analysisList": [
                   ("MinT", self.firstAvg),
                   ("MaxT", self.avg),
                   ("T", self.hourlyTemp),
                   ("Sky", self.avg, [12]),
                   ("Wind", self.vectorAvg, [12]),
                   ("Wx", self.rankedWx, [12]),
                   ("PoP", self._PoP_analysisMethod("SurfExtended")),
                  ],
                "phraseList": [
                   self.wind_summary,
                   self.sky_phrase,
                   self.weather_phrase,
                   self.reportTrends,
                   self.lows_phrase,
                   self.highs_phrase,
                   self.wind_phrase,
                  ],
                },
            }

    def _extraSamplingAdditionalAreasDict(self):
        return {
            "landSeaArea": {
                "analysisList": [
                   ("MinT", self.firstAvg),
                   ("MaxT", self.avg),
                  ],
                "phraseList": [],
                },
            }

    def _PoP_analysisMethod(self, componentName):
        # Alternative PoP analysis methods for consistency between PoP and Wx
        return self.stdDevMaxAvg
        #return self.maximum
        # Use "mode" if you have non-continuous PoP values
        #return self.mode

    def SurfPeriod(self):
        return {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assembleIndentedPhrases,           
                          ],
            "analysisList": [],   # Public area is used only for header
            "phraseList": self._getSurfPeriod_phraseList(),
            "additionalAreas": self._getSurfPeriod_additionalAreas(),
            "additionalAnalysisList": self._getSurfPeriod_additionalAnalysisList(),
            "intersectAreas": self._getSurfPeriod_intersectAreas(),
            "intersectWithAreas": self._getSurfPeriod_intersectWithAreas(),
            "intersectAnalysisList": self._getSurfPeriod_intersectAnalysisList(),
            }

    def _getSurfPeriod_additionalAreas(self):
        # Return a list of (element, list of areas)
        # representing the areas in addition to the public zone
        # to be analyzed and added to the StatisticsDictionary
        # for the generation of phrases.
        additionalAreas = self._getAdditionalAreas("SurfPeriod")
        # Add in the surfAreas
        surfList = []
        for combo in self._combinedAreaDict.keys():
            surfList += self._combinedAreaDict[combo]["surfAreas"]
        areas = []
        for surfArea, label in surfList:
            areas.append(surfArea)
        additionalAreas += [(self._surfGrid, areas)]
        #print "\nadditionalAreas", additionalAreas
        return additionalAreas
    
    def _getAdditionalAreas(self, component):
        # List (element, list of areas) 
        additionalAreas = []
        areaDict = self._additionalAreasDict(component)
        # Create dictionary of {element: [areaTypes]}
        #   e.g. {"MaxT": ['landSeaArea']}
        elementDict = {}
        for areaType in areaDict.keys():
            for entry in areaDict[areaType]["analysisList"]:
                element = entry[0]
                if elementDict.has_key(element):
                    elementDict[element].append(areaType)
                else:
                    elementDict[element] = [areaType]
        # For each element, get the areas listed in the SurfAreaDictionary
        #  for the given areaTypes and then make an additionalAreas entry
        #  e.g. ("MaxT", ["SRF_586"])
        for element in elementDict.keys():
            areaTypes = elementDict[element]
            addAreaList = []
            for areaType in areaTypes:
                for combo in self._combinedAreaDict.keys():
                    addAreaList += [self._combinedAreaDict[combo][areaType]]
            additionalAreas.append((element, addAreaList))
        #print "\nadditionalAreas", additionalAreas
        return additionalAreas

    def _getSurfPeriod_additionalAnalysisList(self):
        # List all possible analysis here
        analysisList = self._getAdditionalAnalysisList("SurfPeriod")
        # Add in extra elements
        if self._ripGrid != "":
            analysisList.append((self._ripGrid, self.maximum))
        if self._waterSpoutGrid != "":
            analysisList.append((self._waterSpoutGrid, self.maximum))             
        return analysisList

    def _getAdditionalAnalysisList(self, component):
        # Return the concatenation of analysisLists from the
        # additionalAreasDict.
        # NOTE:  This is not the most efficient implementation
        # since all additional areas will be sampled and analyzed
        # for all weather elements in the additionalAreasDict.
        analysisList = []
        areaDict = self._additionalAreasDict(component)
        for key in areaDict.keys():
            analysisList += areaDict[key]["analysisList"]
        return analysisList
 
    def _getSurfPeriod_intersectAreas(self):
        # This is for local effects.
        tempList = []
        windList = []
        if self._tempLocalEffects == 1:
            tempList =  [
                ("MinT", ["Inland", "Coastal"]),
                ("MaxT", ["Inland", "Coastal"]),
                ]
        if self._windLocalEffects == 1:
            windList =  [("Wind", ["Inland", "Coastal"])]
        return tempList + windList 

    def _getSurfPeriod_intersectWithAreas(self):
        # Return all the landSeaArea combinations from the _combinedAreaDict
        intersectWithAreas = []
        for key in self._combinedAreaDict.keys():
            if self._combinedAreaDict[key].has_key('landSeaArea'):
                intersectWithAreas.append(
                    self._combinedAreaDict[key]['landSeaArea'])
        #print "returning INTERSECT WITH AREAS", intersectWithAreas
        return intersectWithAreas             

    def _getSurfPeriod_intersectAnalysisList(self):
         analysisList = [
           ("WindGust", self.moderatedMax, [3]),
           ("Wind", self.vectorAvg, [6]),          
           ("MaxT", self.minMax),
           ("MinT", self.minMax),
           ("T", self.minMax),
          ]
         return analysisList

    def _tempLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("Inland", "")
        leArea2 = self.LocalEffectArea("Coastal", " near the coast")
        return [self.LocalEffect([leArea1, leArea2], 8, "...except ")]

    def ExtendedLabel(self):
        return {
            "type": "component",
            "methodList": [self.setLabel],
            "analysisList": [],
            "phraseList":[],
            }
    def setLabel(self, tree, component):
        component.set("words", "\n.EXTENDED...\n")
        return self.DONE()

    def SurfExtended(self):
        return {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,  
                          self.wordWrap,         
                          ],
            "analysisList": [],
            "phraseList":[                  
                   self.wind_summary,
                   self.sky_phrase,
                   self.weather_phrase,
                   self.reportTrends,
                   self.lows_phrase,
                   self.highs_phrase,
                   self.wind_phrase,
                 ],
            "additionalAreas": self._getAdditionalAreas("SurfExtended"),
            "additionalAnalysisList": self._getAdditionalAnalysisList("SurfExtended"),
            }

    def ExtraSampling(self):
        return {
            "type": "component",
            "methodList": [self.noWords],
            "analysisList": [
                   ("MaxT", self.avg, [0]),
                   ("MinT", self.firstAvg, [0]),
                   ],
            "additionalAreas": self._getAdditionalAreas("ExtraSampling"),
            "additionalAnalysisList": self._getAdditionalAnalysisList("ExtraSampling"),
            }

    def ExtendedNarrative(self):
        return {
          "type": "narrative",
          "methodList": [self.assembleChildWords],
          ## Components
          "narrativeDef": [
                       ("SurfExtended",12),("SurfExtended",24),
                       ("SurfExtended",24), ("SurfExtended",24),
                       ],
          }

    def removeEmptyPhrase(self, tree, node):
        # If an indented phrase is empty, do not include the entry for it
        if node.getAncestor('name') in ["fireWind_label_phrase"]:
            return 0
        return 1

    def _setSurfAreaLabel(self, tree, node, phrase):
        # Look up the area label for the phrase and set the node's label
        # This is to handle landSeaArea and marineArea phrases
        # Use the additionalAreasDict: {areaType: {analysisList: [], phraseList: []}}
        # and the combinedAreaDict {combo: {areaType: areaLabel}
        #
        # First, find the phrase in the additionalAreasDict and determine the
        # areaType that we should use for this phrase.
        # E.g. is this a "landSeaArea" or "marineArea" phrase?
        areasDict = self._additionalAreasDict(node.getComponentName())
        useAreaType = None
        for areaType in areasDict.keys():
            phraseList = areasDict[areaType]["phraseList"]
            if type(phrase) is types.ListType:
                for p in phrase:
                    if p in phraseList:
                        useAreaType = areaType
                        break
            else:
                if phrase in phraseList:
                    useAreaType = areaType
                    break
        if useAreaType is None:
            print "Warning!! Trying to set areaLabel for ", phrase
            print " Entry not found in _additionalAreasDict"
            # Leave areaLabel alone and use current land zone
            return
        # Now, set the areaLabel for the node to the area for the areaType
        try:
            areaLabel = self._combinedAreaDict[node.getAreaLabel()][useAreaType]
            if self._surfDebug:
                print "Setting label", useAreaType, areaLabel, \
                      node.getComponentName(), node.getAncestor("name")
            node.set("areaLabel", areaLabel)
        except:
            if self._surfDebug:
                print "Leaving area alone", node.getAreaLabel(), node.getComponentName(),\
                      node.getAncestor("name")

#########################################################################################
### Generate Product
       
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
            self._productIssuance, self._issuance_list(argDict))
        self._timeRange = self._issuanceInfo.timeRange()
        argDict["productTimeRange"] = self._timeRange
        self._expireTime = self._issuanceInfo.expireTime()
        self._issueTime = self._issuanceInfo.issueTime()
        self._definition["narrativeDef"] = self._issuanceInfo.narrativeDef()
        #expireTime = time.time() + self._purgeTime*3600
        #self._expireTime = expireTime
        #self._ddhhmmTimeExpire = time.strftime("%d%H%M",
                       #                        time.gmtime(expireTime))

        if self._periodCombining:
            self._definition["methodList"] = \
               [self.combineComponentStats, self.assembleChildWords]
        else:
            self._definition["methodList"] = [self.assembleChildWords]
        self._definition["priorPeriod"] = 24

        # Calculate current times
        self._ddhhmmTime = self.getCurrentTime(
            argDict, "%d%H%M", shiftToLocal=0, stripLeading=0)
        self._timeLabel = self.getCurrentTime(
            argDict, "600 AM %Z %a %b %e %Y", stripLeading=1)
        return None

    def _sampleData(self, argDict):
        # Sample and analyze the data for the narrative
        components = ["SurfPeriod", "SurfExtended"]
        self._combinedAreaDict = self._getCombinedAdditionalAreas(argDict, components)
        self._narrativeProcessor = ForecastNarrative.ForecastNarrative()
        error = self._narrativeProcessor.getNarrativeData(
            argDict, self._definition, self._timeRange, self._areaList, self._issuanceInfo)
        if error is not None:
            return error
        return None

    def _preProcessProduct(self, fcst, argDict,):
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
       
        #  Try to get Synopsis from previous SRF
        srfPil = self._statePil + self._srfPil
        synopsis = self.getPreviousProduct(srfPil, "SYNOPSIS")
        discussion = self._synopsisHeading +  synopsis + "\n$$\n\n"       
        fcst = fcst + discussion
        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        # This is the header for an edit area combination
        areaHeader = self.makeAreaHeader(
            argDict, areaLabel, self._issueTime, self._expireTime,
            self._areaDictionary, self._defaultEditAreas,
            cityDescriptor = "INCLUDING THE BEACHES OF")
      
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
        fcst = fcst + self._narrativeProcessor.generateForecast(
            argDict, editArea, areaLabel)
        if self._includeMultipleElementTable == 1:
            fcst = fcst + self.makeMultipleElementTable(areaLabel, self._timeRange, argDict)
        return fcst
   
    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        if self._includeOutlook:
            fcst = fcst + self._outLookText + "\n"
        # Add Tide information
        fcst = self._getTideInfo(fcst, editArea, areaLabel, argDict)
        fcst = fcst + "\n$$\n"                
        return fcst
       
    def _postProcessProduct(self, fcst, argDict):
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        # Clean out extra areas that had to be saved
        try:
            if self._savedAreas:
                self.cleanOutEditAreas(self._extraAreaPrefix)
        except:
            pass
        return fcst

    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################
    def _getCombinedAdditionalAreas(self, argDict, components):
        # Called before sampling data to create appropriate combinations
        # for the additional areas based on the user's combinations
        # for the public zones.
        #
        # Create a dictionary with an entry for each areaList combination
        #   to let us know the combined landSea, marine, surf and tide areas
        # e.g.  {"Combo0": {
        #           "landSeaArea": "Extra0",
        #           "marineArea": "Extra1",
        #           "surfAreas": [
        #                ('NorthCoast', 'SURF ALONG NORTH FACING REEFS.............'),
        #                ('SouthCoast', 'SURF ALONG SOUTH FACING REEFS.............')
        #                ],
        #           "tideTables": ["Cedar Key", "Venice Inlet"],
        #           },
        #       }
        # For example, if GUZ001 has landSeaArea of SRF_001
        #   and it is combined with GUZ002 with landSeaArea of SRF_002,
        #   we need to create a union of SRF_001 and SRF_002 and list
        #   it as the landSeaArea for the combination of GUZ001 and GUZ002.
        #
        #   This works similarly for marineAreas.
        #   Any surfAreas that have the same label need to be unioned.
        #   The tideTables are not edit areas and thus do not need to be combined.
        #    However, all must be listed.
        #
        import ModuleAccessor
        accessor = ModuleAccessor.ModuleAccessor()
        self._surfAreaDict = accessor.variable(self._areaDictionary, "AreaDictionary")    
        combinations = argDict["combinations"]
        if combinations is None:
            # Use the areaList to create a combinations entry
            combinations = []
            for editArea, label in self._areaList:
                combinations.append(([label], label))
        extraAreas = {}
        areaTypes = []
        for comp in components:
            areaTypes += self._additionalAreasDict(comp).keys()
        areaTypes = self.removeDups(areaTypes)
        for comboList, label in combinations:
            extraAreas[label] = {}
            for areaType in areaTypes:
                allAreas = self.getAreaDictEntry(
                    comboList, self._surfAreaDict, areaType)
                if self._surfDebug:
                    print "\nGetting Areas", areaType, comboList
                    print "     ", allAreas
                if len(allAreas) > 1:
                    area = self.getUnion(argDict, allAreas, self._extraAreaPrefix)
                    if self._surfDebug:
                        print "  Saving Union", area.getId().getName()
                    self.saveEditAreas([area])
                    self._savedAreas = 1
                    areaName = area.getId().getName()
                elif len(allAreas) == 1:
                    areaName = allAreas[0]
                else:
                    areaName = label # Use combined area itself
                extraAreas[label][areaType] = areaName
            # Handle surf areas
            extraAreas[label]["surfAreas"] = self._getCombinedSurfAreas(
                argDict, components, comboList, label)
            # Handle tide areas         
            extraAreas[label]["tideTables"] = self.getAreaDictEntry(
                comboList, self._surfAreaDict, "tideTables")
        #print "extraAreas", extraAreas
        return extraAreas

    def _getCombinedSurfAreas(self, argDict, components, comboList, comboLabel):
        # Get the surfAreas for the combination.
        # Combine surfAreas that have the same label.
        # If a surfArea has a different label in different entries,
        # just use the first label.
        surfAreasList = self.getAreaDictEntry(
            comboList, self._surfAreaDict, "surfAreas")
        if self._surfDebug:
            print "\nGetting surfAreas"
            print "  Original areas:"
        # If any surfAreas have the same labels, combine them
        # First, make a dictionary of surfLabel: surfAreas
        labelDict = {}
        for surfArea, surfLabel in surfAreasList:
            if self._surfDebug:
                print "    surfArea, surfLabel", surfArea, surfLabel
            self.addToDictionary(labelDict, surfLabel, surfArea)
        #print "\nLabelDict", labelDict
        # Make combinations
        for surfLabel in labelDict.keys():
            surfAreas = labelDict[surfLabel]
            if len(surfAreas) > 1:
                area = self.getUnion(argDict, surfAreas, self._extraAreaPrefix)
                if self._surfDebug:
                    print "  Saving Union", area.getId().getName()
                self.saveEditAreas([area])
                self._savedAreas = 1
                areaName = area.getId().getName()
            else:
                areaName = surfAreas[0]
            labelDict[surfLabel] = areaName
        newList = []
        if self._surfDebug:
            print "  New surfAreas"
        for surfArea, surfLabel in surfAreasList:
            if labelDict.has_key(surfLabel):
                surfArea = labelDict[surfLabel]
                # Remove this entry so we don't get duplicate entries
                del labelDict[surfLabel]
            else:
                # We already handled this label
                continue
            if self._surfDebug:
                print "  ", surfArea, surfLabel
            newList.append((surfArea, surfLabel))
        return newList

    def _issuance_list(self, argDict):
        narrativeDefAM = [
            ("SurfPeriod", "period1"),
            ]
        narrativeDefPM = [
            ("SurfPeriod", "period1"),
            ]
        extended = [
            ("SurfExtended", 12),
            ("SurfExtended", 24),
            ("SurfExtended", 24),
            ("SurfExtended", 24),
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
       
        narrativeDefAM += [("Custom", ("ExtraSampling", (-12, 12)))]
        narrativeDefPM += [("Custom", ("ExtraSampling", (12, 36)))]
       
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
            ("Evening (for tomorrow)", self.DAY()+24, self.NIGHT()+24, 16+24,
             ".Tomorrow...", "early in the morning","late in the afternoon",
             0, narrativeDefPM),           
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
   
#########################################################################

    def valleyRidgeAreaNames(self, tree, node):
        # This was taken from the FWF, so the areas are named
        # Valleys and Ridges.
        # For the SRF, the default names are Inland and Coastal.
        # These areas are to be intersected with the current edit area for
        # reporting inland and coastal winds, respectively.
        # NOTE: If you change these area names, you will also
        # need to change the names in the SurfPeriod "intersectAreas"
        # section.
        return "Inland", "Coastal"

    def untilPhrasing_flag_dict(self, tree, node):
        # If set to 1, "until" time descriptor phrasing will be used.
        # E.g. "NORTH WINDS 20 MPH UNTIL 10 AM...THEN 35 MPH"
        dict = TextRules.TextRules.untilPhrasing_flag_dict(self, tree, node)
        componentName = node.getComponent().get("name")
        if componentName == "SurfPeriod":
            dict["Sky"] = 1
            dict["Wx"]  = 1
        return dict   
       
    def untilPhrasing_format_dict(self, tree, node):
        # Format for "until" time descriptors.
        # If "military": UNTIL 1000
        # If "standard": UNTIL 10 AM
        return {
            "otherwise": "standard",
            #"Wind": "standard",
            }

###########################################################################
#   PHRASES  SET-UP needs to use correct edit area  
###########################################################################

    def fireSky_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.skyWeather_byTimeRange_compoundPhrase)
        elementInfoList = [self.ElementInfo("Sky", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)  
        return self.DONE()
    
    def weather_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node,
                               [self.skyWeather_byTimeRange_compoundPhrase,
                                self.weather_phrase])
        resolution = node.get("resolution")
        if resolution is not None:
            mergeMethod = "Average"
        else:
            mergeMethod = "List"
        elementInfoList = [self.ElementInfo("Wx", mergeMethod, self.WEATHER())]
        self.subPhraseSetUp(tree, node, elementInfoList, self.wxConnector,
                            resolution)
        node.set("allTimeDescriptors", 1)
        if self.areal_sky_flag(tree, node):
            self.disableSkyRelatedWx(tree, node)
        return self.DONE()
 
    def popMax_setUp(self, tree, node):
        # NOTE:  The method is set to "Average" instead of "List" so
        # that the PoP phrase will always cover the full period.
        # It doesn't matter what method (other than List) we choose
        # since the popMax_words method gets its PoP value directly from
        # the "matchToWx" method.
        self._setSurfAreaLabel(tree, node, self.popMax_phrase)
        elementInfoList = [self.ElementInfo("PoP", "Average")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)      
        return self.DONE()

    def dayOrNight_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.dayOrNight_phrase)
        dayElement, nightElement, indent, endWithPeriod = node.get("args")
        elementName = self.dayOrNight_element(tree, node, dayElement, nightElement)
        indentName = elementName+"_FireWx"
        method = "MinMax"
        if elementName == "RH":
            dayNight = self.getPeriod(node.getTimeRange(), 1)
            if dayNight == self.DAYTIME():
                indentName = "MinRH_FireWx"
                method = "Min"
            else:
                indentName = "MaxRH_FireWx"
                method = "Max"
        elementInfoList = [self.ElementInfo(elementName, method)]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", indentName)
        return self.DONE()

    def severeWeather_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.severeWeather_phrase)
        elementInfoList = [self.ElementInfo("Wx", "List", self.WEATHER())]
        self.subPhraseSetUp(tree, node, elementInfoList, self.wxConnector)
        # Set this flag used by the "checkWeatherSimilarity" method
        node.set("noIntensityCombining", 1)
        self.determineSevereTimeDescriptors(tree, node)
        return self.DONE()

    def wind_setUp(self, tree, node, gustFlag=0, element="Wind", connectorMethod=None):
        self._setSurfAreaLabel(tree, node, [self.fireWind_compoundPhrase, self.wind_phrase])
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
    
    def wind_summary_setUp(self, tree, node):
        self._setSurfAreaLabel(
            tree, node, [self.fireWind_compoundPhrase, self.wind_summary])
        elementInfoList = []
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        return self.DONE()

    def fireWind_label_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.fireWind_label_phrase)
        if self.currentAreaContains(
            tree, self.ridgeValleyAreas(tree, node)) == 0:
            return self.setWords(node, "")
        self.setWords(node, "")
        node.set("descriptor", "")
        node.set("indentLabel", "20-FOOT WINDS.......")
        return self.DONE()      
    
    def fireRidgeValleyWind_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node,
                               [self.fireValleyWind_compoundPhrase,
                                self.fireRidgeWind_compoundPhrase])
        # Used for set-up of fireRidgeWind_compoundPhrase as well.
        if self.currentAreaContains(
            tree, self.ridgeValleyAreas(tree, node)) == 0:
            return self.setWords(node, "")
        # Set up intersect area to be used for the node
        areaName = node.getAreaLabel()
        phraseName = node.get("name")
        valleys, ridges = self.valleyRidgeAreaNames(tree, node)
        if phraseName.find("Valley") >= 0:
            area = valleys
        else:
            area = ridges
        intersectName = self.getIntersectName(areaName, area)
        #print "setting intersect", intersectName
        node.set("areaLabel", intersectName)
        return self.DONE()        

    ## WaveHeight Phrase Overrides (MarinePhrases)
    #  Need override to set up as indented phrase               
    def waveHeight_setUp(self, tree, node):
        # Need to determine if we will have one simple phrase OR
        # spawn separate phrases for each surfArea for the current combination

        #print "\nWaveHeight phrase"
        # Are we dealing with a spawned phrase?
        args = node.get("args")
        if args is not None:
            # Set the label for the spawned phrase
            #print "Has Args", args
            surfArea, label = args
            node.set("areaLabel", surfArea)
            indentLabel = label
        # If not, check to see if we need to spawn phrases
        else:
            #print "Does not have args"
            indentLabel = "WaveHeight"
            try:
                surfAreas = self._combinedAreaDict[node.getAreaLabel()]["surfAreas"]
            except:
                surfAreas = []
            #print "surfAreas", surfAreas
            if surfAreas == []:
                # Set label for simple phrase
                #print "Getting label"
                self._setSurfAreaLabel(tree, node, self.waveHeight_phrase)
            else:
                #print "Spawning"
                # Spawn separate phrases
                # We need to reverse the list since we are adding phrases
                # just after the current node and we want the resulting
                # spawned phrases to be in the order of surfAreas
                surfAreas.reverse()
                for surfArea, label in surfAreas:
                    newPhrase = tree.addPhraseDef(node, self.waveHeight_phrase)
                    newPhrase.set("args", (surfArea, label))
                return self.setWords(node, "")
            
        # Proceed with phrase
        if self._surfDebug:
            print "Proceeding with wave height phrase", node.getAreaLabel(), self._surfGrid
        elementInfoList = [self.ElementInfo(self._surfGrid, "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        # See if we are dealing with a surfArea
        node.set("descriptor", "") 
        node.set("indentLabel", indentLabel)
        return self.DONE()

    def waveHeight_words(self, tree, node):
        "Create phrase for waves"
        statDict = node.getStatDict()
        stats = self.getStats(statDict, self._surfGrid)
        if stats is None:
            nodataPhrase = self.noWaveHeight_phrase(
                tree, node, "WaveHeight", "WaveHeight")
            return self.setWords(node.parent, nodataPhrase)

        min, max = self.getValue(stats, "MinMax")
        avg = (min + max)/2
        words = self.wave_range(avg)            
        return self.setWords(node, words)
    
    def sky_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.sky_phrase)
        sky = self.ElementInfo("Sky", "List")
        elementInfoList = [sky]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)        
        return self.DONE()

    def reportTrends_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.reportTrends)
        timeRange = node.getTimeRange()
        dayNight = self.getPeriod(timeRange, 1)
        if dayNight == self.NIGHTTIME():
            eleInfo = self.ElementInfo("MinT", "Min")
        else:
            eleInfo = self.ElementInfo("MaxT", "Max")
            elementName = "MaxT"
        elementInfoList = [eleInfo]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        return self.DONE()

    def lows_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.lows_phrase)
        elementInfoList = [self.ElementInfo("MinT", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)    
        return self.DONE()

    def highs_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.highs_phrase)
        elementInfoList = [self.ElementInfo("MaxT", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)    
        return self.DONE()
    
#######################################################################
##### LAL Phrase Methods 

    def _lal_phrase(self):
        return {
            "setUpMethod": self.lal_setUp,
            "wordMethod": self.lal_words,
            "phraseMethods": self.standard_phraseMethods()
            }
   
    def lal_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self._lal_phrase)
        lal = self.ElementInfo("LAL", "Max", self.SCALAR())

        # comment the following line if you do not want chop
        # subPhrases e.g. "A light chop in morning."
        lal = self.ElementInfo("LAL", "List", self.SCALAR())
        elementInfoList = [lal]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "LAL")
        return self.DONE()

    def lal_words(self, tree, node):
        "Create phrase for lal"
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "LAL")
        if stats is None:
            return self.setWords(node, "")
        lal = self.getValue(stats, "Max", self.SCALAR())
        if lal == 1:
            value = "NO LIGHTNING"
        elif lal == 2:
            value = "VERY INFREQUENT DEADLY LIGHTNING"      
        elif lal == 3:
            value = "INFREQUENT DEADLY LIGHTNING"
        elif lal == 4:
           value = "FREQUENT DEADLY LIGHTNING"
        elif lal == 5:
            value = "EXTREME DEADLY LIGHTNING"
        else:
            value = "!!!LAL phrase problem!!!"
        return self.setWords(node, value)

#######################################################################

#######################################################################
##### Waterspout Phrase Methods 

    def _waterSpout_phrase(self):
        return {
            "setUpMethod": self._waterSpout_setUp,
            "wordMethod": self._waterSpout_words,
            "phraseMethods": self.standard_phraseMethods()
            }
   
    def _waterSpout_setUp(self, tree, node):
        if self._waterSpoutGrid == "":
            return self.setWords(node, "")
        self._setSurfAreaLabel(tree, node, self._waterSpout_phrase)
        waterSpout = self.ElementInfo(self._waterSpoutGrid, "Max", self.SCALAR())
        # comment the following line if you do not want chop
        # subPhrases e.g. "A light chop in morning."
        waterSpout = self.ElementInfo(self._waterSpoutGrid, "List", self.SCALAR())
        elementInfoList = [waterSpout]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "WaterSpout")
        return self.DONE()

    def _waterSpout_words(self, tree, node):
        "Create phrase for waterSpout"
        statDict = node.getStatDict()
        stats = self.getStats(statDict, self._waterSpoutGrid)
        if stats is None:
            return self.setWords(node, "")
        waterSpout = self.getValue(stats, "Max", self.SCALAR())
        #print "LLLLLLLLLLLLLLLL", waterSpout
        if waterSpout == 1:
            value = "NO WATERSPOUTS"
        elif waterSpout == 2:
            value = "VERY SLIGHT CHANCE OF WATERSPOUTS"      
        elif waterSpout == 3:
            value = "VERY SLIGHT CHANCE OF WATERSPOUTS"
        elif waterSpout == 4:
           value = "MODERATE CHANCE OF WATERSPOUTS"
        elif waterSpout == 5:
            value = "VERY GOOD CHANCE OF WATERSPOUTS"
        else:
            value = "!!!WATERSPOUT phrase problem!!!"
        return self.setWords(node, value)

##########################################################################
###### Wx Phrase Methods
    def pop_lower_threshold(self, tree, node):
        # Pop values below this amount will not be reported
        return 0
   
    def pop_wx_lower_threshold(self, tree, node):
        # Pop-related Wx will not be reported if Pop is below this threshold
        return 0
   
    def pop_upper_threshold(self, tree, node):
        # Pop values above this amount will not be reported
        return 100
           
#######################################################################
##### CHOP Phrase Methods 
   
    def chop_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.chop_phrase)
        chop = self.ElementInfo("Wind", "Max", self.VECTOR())
        # comment the following line if you do not want chop
        # subPhrases e.g. "A light chop in morning."
        chop = self.ElementInfo("Wind", "List", self.VECTOR())
        elementInfoList = [chop]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "chop")
        return self.DONE()

    def chop_words(self, tree, node):
        "Create phrase for chop"
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "Wind")
        if stats is None:
            return self.setWords(node, "")
        maxWind, dir = self.getValue(stats, "Max", self.VECTOR())
        offshore1, offshore2, onshore1, onshore2 = self.seaBreeze_thresholds(tree, node)
        if dir > offshore1 and dir < offshore2:
        # For offshore winds
            value = "smooth"
        else:
            if maxWind <= 7:
                value = "smooth"
            elif maxWind > 7 and maxWind <= 12:
                value = "a light chop"      
            elif maxWind > 12 and maxWind <= 17:
                value = "a moderate chop"
            elif maxWind > 17 and maxWind <= 22:
               value = "choppy"
            elif maxWind > 22 and maxWind <= 27:
                value = "rough"
            elif maxWind > 27 and maxWind <= 32:
                value = "very rough"       
            elif maxWind > 32:
                value = "extremely rough"
            else:
                value = "!!!Chop phrase problem!!!"
        return self.setWords(node, value)

#######################################################################
############################################################################
######### UVI    These are read from a daily tide file with various
#########        tide locations and tides.   
    def _uvi_phrase(self):
        return {
            "setUpMethod": self._uvi_setUp,
            "wordMethod": self._uvi_words,
            "phraseMethods": self.standard_phraseMethods()
            }
    def _uvi_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self._uvi_phrase)
        sst = self.ElementInfo("Wind", "Max", self.VECTOR())
        elementInfoList = [sst]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "uvi")
        # Get the uvi product
        productID = self._statePil + self._uviPil
        uviProduct = self.getPreviousProduct(productID, "")
        self._uviLines = string.split(uviProduct, "\n")
        return self.DONE()
   
    def _uvi_words(self, tree, node):
        # Get Synopsis from previous forecast
        # Product has lines such as:
        # ALBUQUERQUE          NM     7       LITTLE ROCK          AR     6
        uviCity = self.getAreaDictEntry(tree, self._surfAreaDict, "uviCity", firstOnly=1)
        uviState = self.getAreaDictEntry(tree, self._surfAreaDict, "stateAbbr", firstOnly=1)
        if uviCity == []:
            msg = self._areaDictErrorMsg(node.getAreaLabel(), "uviCity")
            return self.setWords(node, msg)
        #print "uvicity, state", uviCity, uviState
        uviValue = None
        for line in self._uviLines:
            tokens = line.split()
            # Look for the city
            for token in tokens:
                if token == uviCity:
                    index = tokens.index(token)
                    state = tokens[index + 1]
                    if state == uviState:
                        # The value is after the next token
                        uviValue = int(tokens[index + 2])
                        break
        print "uviValue", uviValue
        if uviValue is not None:
            uviWords = self._uvi_value(tree, node, uviValue)
        else:
            uviWords = ""
        return self.setWords(node, uviWords)

    def _uvi_value(self, tree, node, uviValue):
        if uviValue >= 0 and uviValue <= 2:
            uviWords = "Low"
        elif uviValue >= 3 and uviValue <= 5:
            uviWords = "Moderate"
        elif uviValue >= 6 and uviValue <= 7:
            uviWords = "High"
        elif uviValue >= 8 and uviValue <= 10:
            uviWords = "Very High"
        else:
            uviWords = "Extreme"
        return uviWords        

##########################################################################
    ### HeatIndex (ScalarPhrases)
    
    def heatIndex_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.heatIndex_phrase)
        elementInfoList = [self.ElementInfo("HeatIndex", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)      
        node.set("descriptor", "")
        node.set("indentLabel", "HeatIndex")
        return self.DONE()

##########################################################################
####### Tides    These are read from a daily tide file with various
#######          tide locations and tides.
    
    def _getTideInfo(self, fcst, editArea, areaLabel, argDict):
        # Get a list of tideTables lists from the current edit area combination
        labels = self._combinedAreaDict[areaLabel]["tideTables"]
        if labels == []:
            return fcst
            
        fcst = fcst + "\nTIDE INFORMATION...\n"
        # Get day/month/year
        creationTime = time.localtime(argDict["creationTime"])
        currentDate = time.strftime("%m/%d/%Y", creationTime)
        #print "currentDate", currentDate, type(currentDate)        
        for label in labels:
            success, tideTable = self._getTideTable(label)
            if not success:
                # Add error message to fcst
                fcst = fcst + tideTable
                continue
            fcst = fcst + "\nAT " + label + "...\n\n"
            for line in tideTable:
                if line.find(currentDate) == 0:
                    # Get the tide info 
                    # Line format has currentDate then series of low/high tide times:
                    #    05/02/2005 Mon 07:04AM LDT -0.0 L  02:18PM LDT 0.9  H  06:31PM LDT 0.5  L  
                    #    date       day, time, timeDescriptor, height, low/high
                    tokens = line.split()
                    tideList = []
                    index = 2
                    while index < len(tokens)-1:
                        tideList.append(tuple(tokens[index:index+4]))
                        index += 4
                    #print "tideList", tideList
                    for timeStr, timeDesc, height, lowHigh in tideList:
                        if lowHigh == "L":
                            tideType = "LOW"
                        else:
                            tideType = "HIGH"
                        hrMin = timeStr[0:5].lstrip("0")
                        amPm = timeStr[5:]
                        if self._includeTideHeights:
                            height = " " + height + " FEET"
                        else:
                            height = ""
                        fcst = fcst + tideType+" TIDE"+height+" AT "+hrMin+" "+amPm+ "\n"
                            
        return fcst

    def _getTideTable(self, label):
        fn = ""
        try:
            fn = self._tideFiles[label]
            tideTable = open(fn, "r").readlines()
        except:
            msg = "\nWARNING:Cannot find Tide File for " + label + " " + fn\
                  + " \nPlease check the tideFiles set up in the Site Definition file!!!\n"
            return 0, msg
        return 1, tideTable

#######################################################################
####### SST Phrase This is mainly bogus framework to add an input line
#######            for the SST.                 
    def _sst_phrase(self):
        return {
            "setUpMethod": self._sst_setUp,
            "wordMethod": self._sst_words,
            "phraseMethods": self.standard_phraseMethods()
            }
    def _sst_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self._sst_phrase)
        sst = self.ElementInfo("Wind", "Max", self.VECTOR())
        elementInfoList = [sst]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        descriptor = self.phrase_descriptor(tree, node, "sst", "sst")
        node.set("descriptor", descriptor)
        return self.DONE()
   
    def _sst_words(self, tree, node):
        sstphrase = "|* !!!!!insert water temperature here!!!!!!!!!! *|"
        return self.setWords(node,sstphrase)

    def _areaDictErrorMsg(self, entryName, areaLabel):
        msg = "WARNING: Cannot find " + entryName + " for areas in " + areaLabel \
              + " Please check the SurfAreaDictionary for this information!!!"
        return msg
    
######################################################################
### MarinePhrases TO BE PUT IN BASELINE  -- EXCEPT KEEP SET-UP METHODS
######################################################################
##### Swell methods: compound phrase and summary phrase

    def swell_compoundPhrase(self):
        return {
            "phraseList": [
                self.swell_summary,
                self.simple_swell_phrase,
                ],
            "phraseMethods": [
                self.consolidateSubPhrases,
                self.assembleSentences,
                self.swell_finishUp
                ],
            }   
    def swell2_compoundPhrase(self):
        return {
            "phraseList": [
                self.swell2_summary,
                self.simple_swell2_phrase,
                ],
            "phraseMethods": [
                self.consolidateSubPhrases,
                self.assembleSentences,
                self.swell2_finishUp
                ],
            }
    def swell_finishUp(self, tree, node):
        return self.get_swell_finishUp(tree, node, "Swell")
    
    def swell2_finishUp(self, tree, node):
        return self.get_swell_finishUp(tree, node, "Swell2")
    
    def get_swell_finishUp(self, tree, node, elementName):
        words = node.get("words")
        if words is None:
            return
        if words == "":
            words = "MISSING"
        node.set("descriptor", "")
        node.set("indentLabel", "Label"+elementName)
        node.set("compound", 1)
        return self.setWords(node, words)

    ### Swell Summary methods for Swell and Swell2
    def swell_summary(self):
        return {
            "setUpMethod": self.swell_summary_setUp,
            "wordMethod": self.swell_summary_words,
            "phraseMethods": self.standard_phraseMethods(),
            }    
    def swell2_summary(self):
        return {
            "setUpMethod": self.swell_summary_setUp,
            "wordMethod": self.swell2_summary_words,
            "phraseMethods": self.standard_phraseMethods(),
            }    
    def swell_summary_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, [self.swell_compoundPhrase,
                                            self.swell2_compoundPhrase])
        elementInfoList = []
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        return self.DONE()
    
    def swell_summary_words(self, tree, node):
        # Uses vectorAvg, vectorMedian, vectorMinMax
        words = self.vector_summary(tree, node, "Swell")
        return self.setWords(node, words)
    
    def swell2_summary_words(self, tree, node):
        # Uses vectorAvg, vectorMedian, vectorMinMax
        words = self.vector_summary(tree, node, "Swell2")
        return self.setWords(node, words)

    ## Simple Swell Phrases 
   
    def simple_swell_phrase(self):
        return {
            "setUpMethod": self.simple_swell_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
            }
    def simple_swell2_phrase(self):
        return {
            "setUpMethod": self.simple_swell2_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
            }
    
    def simple_swell_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.swell_compoundPhrase)
        return self.get_swell_setUp(tree, node, "Swell")
    
    def simple_swell2_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.swell2_compoundPhrase)
        return self.get_swell_setUp(tree, node, "Swell2")
    
    def get_swell_setUp(self, tree, node, element="Swell", connectorMethod=None):
        swell = self.ElementInfo(element, "List", self.VECTOR())
        elementInfoList = [swell]
        if connectorMethod is None:
            connectorMethod = self.vectorConnector
        self.subPhraseSetUp(tree, node, elementInfoList, connectorMethod)
        return self.DONE()

    def vector_summary_valueStr(self, value, elementName):
        # Thresholds and corresponding phrases
        # Defaults are for Winds converted to  mph
        words = ""
        if elementName in ["Swell", "Swell2"]:
            return self.swell_summary_valueStr(value, elementName)
        else:
            return self.wind_summary_valueStr(value, elementName)

    def swell_summary_valueStr(self, value, elementName):
        if value < 10:
            words = ""
        elif value < 20:
            words = "moderate"
        else:
            words = "large swells"
        return words
        
    def wind_summary_valueStr(self, value, elementName):
        if value < 25:
            words = ""
        elif value < 30:            
            words = "breezy"
        elif value < 40:
            words = "windy"
        elif value < 50:
            words = "very windy"
        elif value < 74:
            words = "strong winds"
        else:
            words = "hurricane force winds"
        return words

########################################################################
####### Period Phrases
    
    def period_phrase(self):
        return {
            "setUpMethod": self.period_setUp,
            "wordMethod": self.period_words,
            "phraseMethods": self.standard_phraseMethods()  
            }    
    def period_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.period_phrase)
        return self.get_period_setUp(tree, node, "Period")
        
    def period2_phrase(self):
        return {
            "setUpMethod": self.period2_setUp,
            "wordMethod": self.period_words,
            "phraseMethods": self.standard_phraseMethods()  
            }    
    def period2_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.period2_phrase)
        return self.get_period_setUp(tree, node, "Period2")

    def get_period_setUp(self, tree, node, element):
        elementInfoList = [self.ElementInfo(element, "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", element)
        return self.DONE()

    def period_words(self, tree, node):
        element = node.getAncestor("elementName")
        statDict = node.getStatDict()
        stats = self.getStats(statDict, element)
        if stats is None:
            return self.setWords(node, "")
        periodValue = int(self.getValue(stats))
        outUnits = self.element_outUnits(tree, node, element, element)
        units = self.units_descriptor(tree, node, "units", outUnits)
        unit = self.units_descriptor(tree, node, "unit", outUnits)
        if periodValue == 1:
            units = unit
        return self.setWords(node, `periodValue` + " " + units)
    
########################################################################
####### Rip Phrase
   
    def rip_phrase(self):
        return {
            "setUpMethod": self.rip_setUp,
            "wordMethod": self.rip_words,
            "phraseMethods": self.standard_phraseMethods()
            }
    def rip_setUp(self, tree, node):
        self._setSurfAreaLabel(tree, node, self.rip_phrase)
        if self._ripGrid != "":
            rip = self.ElementInfo(self._ripGrid, "Max")
        else:
            rip = self.ElementInfo("Wind", "Max", self.VECTOR())
        elementInfoList = [rip]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "rip")
        return self.DONE()
   
    def rip_words(self, tree, phrase):
        timeRange = phrase.getTimeRange()
        areaLabel = phrase.getAreaLabel()
        if self._ripGrid != "":
            stats = tree.stats.get(self._ripGrid, timeRange, areaLabel, mergeMethod="Max")
            if stats is None:
                return self.setWords(phrase, "")
            else:
                return self.setWords(phrase, self.ripGrid_value(stats))
        else:
            maxWave = tree.stats.get(self._surfGrid, timeRange, areaLabel, mergeMethod="Max")
            windStats = tree.stats.get("Wind", timeRange, areaLabel, mergeMethod="Max")

            ###This will provide average winds
            #windStats = tree.stats.get("Wind", timeRange, areaLabel, mergeMethod="Avg")
            #print "avg", windStats

            if maxWave is None or windStats is None:
                return self.setWords(phrase, "")

            maxWind, dir = windStats
            words =  self.rip_value(maxWave, maxWind, dir)
            if len(words) > 0:
                words = words
            return self.setWords(phrase, words)

    def ripPhrases(self):
        return {
            "lowPhrase" : "LOW...HOWEVER...STRONG CURRENTS CAN OCCUR NEAR PIERS AND JETTIES. ALWAYS HAVE A FLOTATION DEVICE WITH YOU IN THE WATER",
            "modPhrase" : "MODERATE. A MODERATE RISK OF RIP CURRENTS MEANS WIND AND OR WAVE CONDITIONS SUPPORT STRONGER OR MORE FREQUENT RIP CURRENTS. ALWAYS HAVE A FLOTATION DEVICE WITH YOU IN THE WATER",
            "highPhrase" : "HIGH. HIGH SURF AND LARGE SWELLS WILL PRODUCE DANGEROUS POUNDING SURF AND RIP CURRENTS AT THE BEACHES. PEOPLE VISITING THE BEACHES SHOULD STAY OUT OF THE HIGH SURF",
            }
    
    def ripGrid_value(self, value):
        phrase = ""
        ripPhrases = self.ripPhrases()
        if value == 1:
            words = ripPhrases["lowPhrase"]
        elif value == 2:
            words = ripPhrases["modPhrase"]
        elif value == 3:
            words = ripPhrases["highPhrase"]
        else:
            words = "RIP phrase problem!!!"
        return words

    def rip_value(self, maxWave, maxWind, dir):
        words = ""
        ripPhrases = self.ripPhrases()
        if dir > 150:
            if maxWind < 15:
                words = ripPhrases["lowPhrase"]
            if maxWind >= 15 and maxWave >= 3:
                words = ripPhrases["modPhrase"]
            elif maxWind >= 15 and maxWave < 3:
                words = ripPhrases["lowPhrase"]
            if maxWind >= 20 and maxWave >= 6:
                words = ripPhrases["highPhrase"]

            elif maxWind >= 20 and maxWave < 6:
                words = modPhrase
           
        else:
            if maxWave < 6:
                words = ripPhrases["lowPhrase"]
            if maxWave >= 6:
                words = ripPhrases["modPhrase"]
            if  maxWave > 8:
                words = ripPhrases["highPhrase"]
        return words

    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
            ('SU.W', allActions, 'Marine'),      # HIGH SURF WARNING
            ('SU.Y', allActions, 'Marine'),      # HIGH SURF ADVISORY
            ('RP.S', allActions, 'Rip'),         # High Rip Threat
            ]

