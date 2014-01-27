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
# Description: This product creates a Near Shore Marine product.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# NSH, NSH_<site>_<MultiPil>_Definition, NSH_<site>_Overrides
#-------------------------------------------------------------------------
# Customization Points:
#
# REQUIRED OVERRIDE:
#  _lakeStmt -- override with correct lake name(s) for your site
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
#  periodCombining         If 1, compnents an attempt will be made to combine components
#                          or time periods into one.  Otherwise no period combining will
#                          will be done.
#  useAbbreviations
#      If 1, use marine abbreviations e.g. TSTM instead of THUNDERSTORM, NW instead of NORTHWEST
#      (See marine_abbreviateText in the TextRules module)
#  areaDictionary   Modify the AreaDictionary utility with UGC information about zones
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
#  To 2 days:
#    Wind (every 3 hours)
#    WaveHeight and/or WindWaveHgt (every 6 hours)
#    Wx (every 6 hours)
#    Sky (every 6 hours)
#    Optional:
#       WindGust (every 3 hours)
#-------------------------------------------------------------------------
# Edit Areas Needed:
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file:
#  Combinations
#-------------------------------------------------------------------------
# Component Products:
#  Hazards (optional):  If Discrete grid provided, headlines will be generated.
#  NSHPeriod
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
#     from CWF:
#       _Text1
#       _Text2
#       _Text3
#       _issuance_list
#     from ConfigVariables:
#       maximum_range_nlValue_dict
#       minimum_range_nlValue_dict
#       phrase_descriptor_dict
#       scalar_difference_nlValue_dict
#
#-------------------------------------------------------------------------
# Example Output:
#  Refer to the NWS Directives for Marine Services.

import TextRules
import SampleAnalysis
import ForecastNarrative
import time, string, re, types

### adding import of os for the MWW turnkey code at the end of the file
import os

class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = []
    Definition =  {
        "type": "smart",
        "displayName": "None",
        "database": "Official",
        # Defines output location of finished product.
        "outputFile": "{prddir}/TEXT/NSH_<MultiPil>.txt",
        "debug": 0,
        # Name of map background for creating Combinations
        "mapNameForCombinations": "Marine_Zones_<site>", 

        ## Edit Areas: Create Combinations file with edit area combinations.
        "showZoneCombiner" : 1, # 1 to cause zone combiner to display
        "defaultEditAreas" : "Combinations_NSH_<site>_<MultiPil>",
        "editAreaSuffix": None,

        # product identifiers
        "productName": "NEARSHORE MARINE FORECAST", # product name 
        "fullStationID": "<fullStationID>",    # full station identifier (4letter)
        "wmoID": "<wmoID>",          # WMO ID
        "pil": "<pil>",            # Product pil
        "areaName": "<state>",             # Name of state, such as "GEORGIA"
        "wfoCityState": "<wfoCityState>",   # Location of WFO - city state
        
        "textdbPil": "<textdbPil>",       # Product ID for storing to AWIPS text database.
        "awipsWANPil": "<awipsWANPil>",   # Product ID for transmitting to AWIPS WAN.

        "hazardSamplingThreshold": (10, None),  #(%cov, #points)

        "fixedExpire": 1,       #ensure VTEC actions don't affect segment expiration time

        "periodCombining" : 0,       # If 1, combine periods, if possible


        "lineLength": 66,           # product line length
        "useAbbreviations": 0,      # Use marine abbreviations

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
        self._outlookflag = 0

    # TO BE OVERRIDDEN IN LOCAL FILE
    def _Text1(self):
        return ""

    def _Text2(self):
        return ""

    def _Text3(self):
        return ""

    def _lakeStmt(self, argDict):
        return "FOR WATERS WITHIN FIVE NAUTICAL MILES OF SHORE ON LAKE (NAME)"

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
    
    def vector_dir_difference_dict(self, tree, node):
        # Direction difference.  If the difference between directions
        # for 2 sub-periods is greater than this value,
        # the different directions will be noted in the phrase.
        # Units are degrees
        return {
            "Wind": 40, # degrees
            "TransWind": 60,  # mph
            "FreeWind": 60,  # mph
            "Swell":60, # degrees
            "Swell2":60, # degrees
            }    
    
    def phrase_descriptor_dict(self, tree, node):
        dict = TextRules.TextRules.phrase_descriptor_dict(self, tree, node)
        dict["Wind"] = "wind"  
        dict["around"] = ""
        return dict
    
    def null_nlValue_dict(self, tree, node):
        # Threshold below which values are considered "null" and  not reported.
        # Units depend on the element and product
        dict = TextRules.TextRules.null_nlValue_dict(self, tree, node)
        dict["Wind"] =  5   
        return dict
       
    def first_null_phrase_dict(self, tree, node):
        # Phrase to use if values THROUGHOUT the period or
        # in the first period are Null (i.e. below threshold OR NoWx)
        # E.g.  LIGHT WINDS.    or    LIGHT WINDS BECOMING N 5 MPH.
        dict = TextRules.TextRules.first_null_phrase_dict(self, tree, node)
        dict["Wind"] =  "variable winds 10 knots or less" 
        return dict

    def null_phrase_dict(self, tree, node):
        # Phrase to use for null values in subPhrases other than the first
        # Can be an empty string
        #  E.g.  "NORTH 20 to 25 KNOTS BECOMING LIGHT"
        dict = TextRules.TextRules.null_phrase_dict(self, tree, node)
        dict["Wind"] =  "variable 10 knots or less" 
        dict["Wx"] =  ""  
        return dict

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

    def postProcessPhrases(self, tree, node): 
        words = node.get("words") 
        if words is not None: 
            words = string.lower(words) 
            words = string.replace(words, "thunderstorms and rain showers", 
                        "showers and thunderstorms") 
            words = string.replace(words, "snow showers and rain showers", "rain and snow showers") 
            words = string.replace(words, "rain showers and snow showers", "rain and snow showers") 
            #print "words = ", words 
            words = string.replace(words, "light rain showers", "rain showers") 
            words = string.replace(words, "rain showers", "showers") 
            #print "words 2= ", words 
            words = string.replace(words, "winds hurricane", "hurricane") 
            words = string.replace(words, "winds gales", "gales") 
            words = string.replace(words, "winds storm", "storm") 
            words = string.replace(words, "to to", "to") 
            words = string.replace(words, "winds 10 knots", "winds around 10 knots") 
            words = string.replace(words, "winds 5 knots", "winds around 5 knots") 
            words = string.replace(words, "and chance of", "and a chance of")   
            return self.setWords(node, words) 
  
    def rounding_method_dict(self, tree, node):
        # Special rounding methods
        #
        return {
            "Wind": self.marineRounding,
            }

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
            (0,5) : 0,   # will be reported as "null"
            (5, 8) : 5,
            "default" : 10,
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

    def NSHFirstPeriod(self):
        return {
            "type": "phrase",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,
                          self.postProcessPhrases,
                          self.wordWrap,          
                          ],
            "analysisList": [
                  #("Wind", self.vectorMinMax, [3]),
                  ("Wind", self.vectorMinMax, [4]),
                  #("WindGust", self.maximum, [3]),
                  ("WindGust", self.maximum, [6]),
                  ("WaveHeight", self.minMax, [3]),
                  ("Wx", self.rankedWx, [6]),                                  
                  ("T", self.minMax),
                  ("Sky", self.avg, [6]),
                  ("PoP", self._PoP_analysisMethod("NSHFirstPeriod"), [6]),
                  ("PoP", self.binnedPercent, [6]),
                  ],
            "phraseList":[
                   # WINDS
                   self.marine_wind_withGusts_phrase,
                   # Alternative:
                   #self.marine_wind_phrase,
                   #self.gust_phrase,
                   # WEATHER
                   self.weather_orSky_phrase,
                   self.visibility_phrase,
                   # WAVES
                   self.waveHeight_phrase,
                   # Optional:
                   #self.chop_phrase,
                  ],
            "lineLength": 66,
            "runTimeEditArea": "yes",
            }


    def NSHPeriod(self):
        return {
            "type": "phrase",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.postProcessPhrases,
                          self.wordWrap,          
                          ],
            "analysisList": [
                  #("Wind", self.vectorMinMax, [3]),
                  ("Wind", self.vectorMinMax, [4]),
                  #("WindGust", self.maximum, [3]),
                  ("WindGust", self.maximum, [6]),
                  ("WaveHeight", self.minMax, [3]),
                  ("Wx", self.rankedWx, [6]),                                  
                  ("T", self.minMax),
                  ("Sky", self.avg, [6]),
                  ("PoP", self._PoP_analysisMethod("NSHPeriod"), [6]),
                  ("PoP", self.binnedPercent, [6]),
                  ],
            "phraseList":[
                   # WINDS
                   self.marine_wind_withGusts_phrase,
                   # Alternative:
                   #self.marine_wind_phrase,
                   #self.gust_phrase,
                   # WEATHER
                   self.weather_orSky_phrase,
                   self.visibility_phrase,
                   # WAVES
                   self.waveHeight_phrase,
                   # Optional:
                   #self.chop_phrase,
                   #outlook phrase
                   self._warnOutlook_phrase,
                  ],
            "lineLength": 66,
            "runTimeEditArea": "yes",
            }
    
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
            self.progressMessage(fraction, percent, "Making Product for " + areaLabel)
            fcst = self._preProcessArea(fcst, editArea, areaLabel, argDict)
            fcst  = self._makeProduct(fcst, editArea, areaLabel, argDict)
            fcst = self._postProcessArea(fcst, editArea, areaLabel, argDict)
            fraction = fractionOne

        fcst = self._postProcessProduct(fcst, argDict)
        return fcst

    def _getVariables(self, argDict):
        #part below is to eliminate time prompt added by Meade
        # Get Definition variables
        self._definition = argDict["forecastDef"]
        for key in self._definition.keys():
            exec "self._" + key + "= self._definition[key]"

        localtime = time.localtime(argDict['creationTime'])
        localHour = localtime[3]
        self._setProductIssuance(localHour)

        # Get VariableList and _issuance_list variables
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"

        self._format = "Standard"
        self._extended = "Without Extended"
        self._language = argDict["language"]

        # Make argDict accessible
        self.__argDict = argDict
        
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
        self._definition["priorPeriod"] = 24
        
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
               "\n" + issuedByString + self._timeLabel + "\n\n" + \
               self._lakeStmt(argDict) + "\n\n"
        fcst = fcst + self._Text1()
        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
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
        # Produce Headline product
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
        fcst = fcst + self._Text2()
        fcst = fcst + self._Text3()
        return fcst + "\n$$\n\n"

    def _postProcessProduct(self, fcst, argDict):
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst
     
    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################

    def _setProductIssuance(self, localHour):
        if localHour >= 0 and localHour <= 8:
            self._productIssuance = "430 AM"
        elif localHour > 8 and localHour <= 13:
            self._productIssuance = "Morning Update"
        elif localHour > 13 and localHour <= 18:
            self._productIssuance = "430 PM"        
        else:
            self._productIssuance = "Evening Update"

    def _issuance_list(self, argDict):
        #  This method sets up configurable issuance times with associated
        #  narrative definitions.  See the Text Product User Guide for documentation.
        narrativeDefAM = [
            ("NSHFirstPeriod", "period1"), ("NSHPeriod", 12), ("NSHPeriod", 12), ("NSHPeriod", 12), 
#            ("NSHExtended", 24), ("NSHExtended", 24), ("NSHExtended", 24), 
            ]
        narrativeDefPM = [
            ("NSHFirstPeriod", "period1"), ("NSHPeriod", 12), ("NSHPeriod", 12), ("NSHPeriod", 12), 
#            ("NSHExtended", 24), ("NSHExtended", 24), ("NSHExtended", 24), 
            ]        
        return [
            ("430 AM", self.DAY(), self.NIGHT(), 11,
             ".Today...", "early in the morning", "late in the afternoon",
             1, narrativeDefAM),
            ("Morning Update", "issuanceHour", self.NIGHT(), 17,
             ".This Afternoon...", "early", "late",
             1, narrativeDefAM),
            #  End times are tomorrow:
            ("430 PM", self.NIGHT(), 24 + self.DAY(), 23,
             ".Tonight...", "late in the night", "early in the evening",
             1, narrativeDefPM),
            ("Evening Update", "issuanceHour", 24 + self.DAY(), 24+5,
             ".Overnight...", "toward daybreak", "early in the evening",
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
        if compName == "NSHExtended":
              return 0
        else:
              return 1

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

    ########################################################################
    # OVERRIDING METHODS
    ########################################################################
     
    def _warnOutlook_phrase(self):
        return {
            "phraseMethods": [
                self._warnOutlook_words, # phrase.words
            ],
            }    
    def _warnOutlook_words(self, tree, phrase):
        # will put an outlook phrase in the text

        windStats = tree.stats.get("Wind", phrase.getTimeRange(), mergeMethod="Max")
        if windStats is None:
            return self.setWords(phrase, "")
        
        max, dir = windStats
        words = ""
        if max >= 23 and (self._outlookflag != 1):
            words = "a small craft advisory may be needed"
            self._outlookflag = 1
        if max >= 34 and (self._outlookflag != 2):
            words = "a gale warning may be needed"
            self._outlookflag = 2
        if max >= 48 and (self._outlookflag != 3):
            words = "a storm warning may be needed"
            self._outlookflag = 3
        if max >= 64 and (self._outlookflag != 4):
            self._outlookflag = 4
            words = "a hurricane force wind warning may be needed"
        if max < 23:
            words  = ""
            self._outlookflag = 0
        return self.setWords(phrase, words)
                           
    # Returns a list of the Hazards allowed for this product in VTEC format.
    # These are sorted in priority order - most important first.
    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        marineActions = ["NEW", "EXA", "EXB", "EXT", "CON"]


        return [
                ('HF.A',  marineActions, 'Marine'),  # HURRICANE FORCE WIND WATCH
                ('SR.A',  marineActions, 'Marine'),  # STORM WATCH
                ('GL.A',  marineActions, 'Marine'),  # GALE WATCH
                ('SE.A',  marineActions, 'Marine'),  # HAZARDOUS SEAS
                ('UP.A', allActions, 'IceAccr'),     # HEAVY FREEZING SPRAY WATCH
                ('HF.W',  marineActions, 'Marine'),  # HURRICANE FORCE WIND WARNING
                ('SR.W',  marineActions, 'Marine'),  # STORM WARNING
                ('GL.W',  marineActions, 'Marine'),  # GALE WARNING
                ('SE.W',  marineActions, 'Marine'),  # HAZARDOUS SEAS
                ('UP.W', allActions, 'IceAccr'),                    # HEAVY FREEZING SPRAY WARNING
                ('RB.Y', allActions, 'Marine'),      #ROUGH BAR
                ('SI.Y', allActions, 'Marine'),      #SMALL CRAFT ADVISORY
                ('SC.Y', allActions, 'Marine'),                     # SMALL CRAFT ADVISORY
                ('SW.Y', allActions, 'Marine'),                     # SMALL CRAFT ADVISORY
                ('BW.Y', allActions, 'Marine'),                     # BRISK WIND ADVISORY
                ('MF.Y', allActions, 'Fog'),                        # DENSE FOG ADVISORY
                ('MS.Y', allActions, 'Smoke'),                      # DENSE SMOKE ADVISORY
                ('UP.Y', allActions, 'IceAccr'),                    # HEAVY FREEZING SPRAY ADVISORY
                ('MH.W', allActions, 'Ashfall'),                    # VOLCANIC ASHFALL WARNING
                ('MH.Y', allActions, 'Ashfall'),                    # VOLCANIC ASHFALL ADVISORY
                ('LO.Y', allActions, 'LowWater'),                   # LOW WATER ADVISORY
                ('TO.A', allActions, 'Convective'),                 # TORNADO WATCH
                ('SV.A', allActions, 'Convective'),                 # SEVERE THUNDERSTORM WATCH
                ]
