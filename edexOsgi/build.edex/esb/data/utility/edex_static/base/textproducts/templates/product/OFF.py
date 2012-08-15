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
# Description: OFF (Offshore Forecast)
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# OFF.py, OFF _<site>_<MultiPil>_Definition, OFF_<site>_Override
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
#  productName      defines name of product e.g. "COASTAL WATERS FORECAST"
#  fullStationID    Full station identifier, 4 letter, such as "KSLC".
#  wmoID            WMO ID code for product header, such as "FOUS45"
#  pil              Product pil, such as "OFFBOS"
#  areaName (opt.)  Area name for product header, such as "WESTERN NEW YORK"
#  wfoCityState     City,state that the WFO is located in, such as "BUFFALO, NY"
#
#  synopsisUGC      UGC code for Synopsis
#  synopsisHeading  Heading for Synopsis
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
#      OFFPeriod (component)
#      OFFPeriodMid (component)
#      OFFExtended (component)
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
#     from OFF:
#       _Text1
#       _Text2
#       _issuance_list
#       riverBarForecast_dict
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
import time, string, re, types
import TimeRange

class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = []
    Definition =  {
        "type": "smart",
        "displayName": "None",
        "database": "Official",
        # Defines output location of finished product.
        "outputFile": "{prddir}/TEXT/OFF_<MultiPil>.txt",
        "debug": 0,
        # Name of map background for creating Combinations
        "mapNameForCombinations": "Marine_Zones_<site>", 

        "lineLength": 66,
        ## Edit Areas: Create Combinations file with edit area combinations.
        "showZoneCombiner" : 1, # 1 to cause zone combiner to display
        "defaultEditAreas" : "Combinations_OFF_<site>_<MultiPil>",
        "editAreaSuffix": None,
        # product identifiers
        "productName": "OFFSHORE FORECAST", # product name 
        "fullStationID": "<fullStationID>",    # full station identifier (4letter)
        "wmoID": "<wmoID>",          # WMO ID
        "pil": "<pil>",            # Product pil
        "areaName": "<state>",             # Name of state, such as "GEORGIA" -- optional
        "wfoCityState": "<wfoCityState>",   # Location of WFO - city state

        "synopsisUGC": "",                # UGC code for synopsis
        "synopsisHeading": ".SYNOPSIS...",# Heading for synopsis
        
        "textdbPil": "<textdbPil>",       # Product ID for storing to AWIPS text database.
        "awipsWANPil": "<awipsWANPil>",   # Product ID for transmitting to AWIPS WAN.

        "hazardSamplingThreshold": (10, None),  #(%cov, #points)

        "fixedExpire": 1,       #ensure VTEC actions don't affect segment expiration time

        "periodCombining" : 0,       # If 1, combine periods, if possible
        # Product-specific variables:
        # Set to one if you want a 6-hour evening period instead of
        # 18-hour period without lows
        "includeEveningPeriod": 1,
        "useAbbreviations": 0,

        # Weather-related flags
        "hoursSChcEnds": 24,        
        
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

    def _Text2(self):
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
        #                        '%b %d %y %H:%M:%S GMT')
        expTuple = self._expireTime.utctimetuple()
        
        #  Format expiration time for inclusion in synopsis header
        expTime = time.strftime('%d%H%M', expTuple)

        return "%s-%s-\n" % (self._synopsisUGC, expTime) + \
               self._timeLabel + "\n\n" + \
               self._synopsisHeading + "\n"  + \
               synopsis + "\n$$\n\n"
    
    ########################################################################

    # SampleAnalysis overrides
    def moderated_dict(self, parmHisto, timeRange, componentName):
        # This dictionary defines the low and high limit at which
        # outliers will be removed when calculating moderated stats.
        # By convention the first value listed is the percentage
        # allowed for low values and second the percentage allowed
        # for high values.
        dict = SampleAnalysis.SampleAnalysis.moderated_dict(self, parmHisto, timeRange, componentName)
        dict["Wind"] =  (0, 20)
        dict["WaveHeight"] = (5,5)
        return dict

    def null_nlValue_dict(self, tree, node):
        # Threshold below which values are considered "null" and  not reported.
        # Units depend on the element and product
        dict = TextRules.TextRules.null_nlValue_dict(self, tree, node)
        dict["WaveHeight"] =  6  
        dict["WindWaveHgt"] =  6 
        dict["Wind"] =  15  
        dict["WindGust"] = 120 
        dict["Swell"] =  5
        dict["Visibility"] = 5 # in nautical miles. Report if less than this value.        
        return dict

    # ConfigVariables Overrides
    def phrase_descriptor_dict(self, tree, node):
        # Descriptors for phrases
        dict = TextRules.TextRules.phrase_descriptor_dict(self, tree, node)
        dict["Wind"] = "winds"
        dict["WaveHeight"] = "seas"
        dict["seas"] = "seas"
        dict["mixed swell"] = "mixed swell"
        dict["waves"] = "seas"
        dict["dominant period"] = "dominant period"
        # Apply only if marine_wind_flag (see above) is set to 1:
        dict["hurricane force winds to"] =  "hurricane force winds to"
        dict["storm force winds to"] = "storm force winds to"
        dict["gales to"] =  "gales to"
        dict["up to"] =  "up to"
        dict["around"] = ""
        return dict

    def first_null_phrase_dict(self, tree, node):
        # Phrase to use if values THROUGHOUT the period or
        # in the first period are Null (i.e. below threshold OR NoWx)
        # E.g.  LIGHT WINDS.    or    LIGHT WINDS BECOMING N 5 MPH.
        dict = TextRules.TextRules.first_null_phrase_dict(self, tree, node)
        dict["WaveHeight"] =  "seas 6 feet or less"
        dict["WindWaveHgt"] =  "seas 6 feet or less"
        dict["Wind"] =  "winds 15 knots or less"
        dict["Swell"] =  ""
        return dict

    def null_phrase_dict(self, tree, node):
        # Phrase to use for null values in subPhrases other than the first
        # Can be an empty string
        #  E.g.  "NORTH WINDS 20 to 25 KNOTS BECOMING LIGHT"
        dict = TextRules.TextRules.null_phrase_dict(self, tree, node)
        dict["WaveHeight"] =  "6 feet or less"  
        dict["WindWaveHgt"] =  "6 feet or less"  
        dict["Wind"] =  "less than 15 knots"  
        dict["Wx"] =  ""  
        dict["Swell"] =  "light"  
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
                                "Wind": "...diminishing to ",
                                "Swell": "...subsiding to ",
                                "Swell2": "...subsiding to ",
                                "WaveHeight": "...subsiding to ",
                                "WindWaveHgt": "...subsiding to ",
                         }                 
        dict["backing"] =  {
                                "Wind": "...becoming ",
                                "Swell": "...becoming ",
                                "Swell2": "...becoming ",
                                "WaveHeight": "...becoming ",
                                "WindWaveHgt": "...becoming ",
                         }
                
        dict["veering"] =  {
                                "Wind": "...becoming to ",
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
                                "Wind":  "...diminishing to ",
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

    def maximum_range_nlValue_dict(self, tree, node):
        # Maximum range to be reported within a phrase
        #   e.g. 5 to 10 mph
        # Units depend on the product
        dict = TextRules.TextRules.maximum_range_nlValue_dict(self, tree, node)
        dict["Wind"] = 10  
        dict["Swell"] = 5  
        dict["Swell2"] = 5  
        dict["WaveHeight"] = 4
        dict["WindWaveHgt"] = 2
        return dict 

    def rounding_method_dict(self, tree, node):
        # Special rounding methods
        #
        return {
            "Wind": self.marineRounding,
            }
                
    def vector_mag_difference_nlValue_dict(self, tree, node):
        # Replaces WIND_THRESHOLD
        # Magnitude difference.  If the difference between magnitudes
        # for sub-ranges is greater than or equal to this value,
        # the different magnitudes will be noted in the phrase.
        # Units can vary depending on the element and product
        return  {
            "Wind": 10,  
            "Swell": 5,  # ft
            "Swell2": 5,  # ft
            }

    def vector_dir_difference_dict(self, tree, node):
        # Replaces WIND_DIR_DIFFERENCE
        # Direction difference.  If the difference between directions
        # for sub-ranges is greater than or equal to this value,
        # the different directions will be noted in the phrase.
        # Units are degrees
        return {
            "Wind": 90, # degrees
            "Swell":60, # degrees
            "Swell2":60, # degrees
            }

    def element_outUnits_dict(self, tree, node):
        dict = TextRules.TextRules.element_outUnits_dict(self, tree, node)
        dict["Visibility"] = "NM"
        return dict

    def scalar_difference_nlValue_dict(self, tree, node):
        # Scalar difference.  If the difference between scalar values
        # for 2 sub-periods is greater than or equal to this value,
        # the different values will be noted in the phrase.
        return {
            "WindGust": 20, # knots or mph depending on product
            "Period": 5, # seconds
            "WaveHeight":  self.waveht_scalar_value ,#0, # in feet
            "WindWaveHgt": 5, # feet
            }
   
    def waveht_scalar_value(self,tree,node,elementName,elementName1):
        # calculating the scalar value for changes based on wave height
        wave = tree.stats.get("WaveHeight", node.getTimeRange(), node.getAreaLabel(),
                                             mergeMethod="Max")
#        print wave, "Wave!"
        if wave is None:
            return 10
        if wave <= 6:
            rtnval = 6
        else:
            val = wave * .25
            rtnval = int(val+0.5)

    def periodCombining_elementList(self, tree, node):
        # Weather Elements to determine whether to combine periods
        #return ["Sky", "Wind", "Wx", "PoP", "MaxT", "MinT"]
        # Marine
        return ["WaveHeight", "Wind", "Wx"]
    
    # WxPhrases Overrides
    def pop_wx_lower_threshold(self, tree, node):
        # Always report weather
        return 0

    # MarinePhrases Overrides
    def seasWaveHeight_element(self, tree, node):
        # Weather element to use for reporting seas
        # "COMBINED SEAS 10 TO 15 FEET."
        # IF above wind or swell thresholds
        return "WaveHeight"

    def waveHeight_wind_threshold(self, tree, node):
        # wind value above which waveHeight is reported vs. wind waves
        # Unit is knots
        return 0

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
        if compName == "OFFExtended":
              return 0
        else:
              return 1

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

    ########################################################################
    # COMPONENT PRODUCT DEFINITIONS
    ########################################################################

    def _PoP_analysisMethod(self, componentName):
        # Alternative PoP analysis methods for consistency between PoP and Wx
        #return self.maxMode
        #return self.maximum
        return self.stdDevMaxAvg

    def OFFPeriod(self):
        return {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            
            "analysisList": [            
                      # NOTE: Choose from the following analysis options.
                      # Do not remove the "vectorMinMax" analysis for
                      # "Wind". This is necessary to get an absolute max if
                      # the useWindsForGusts flag is on.
            
                      # Use the following if you want moderated ranges 
                      # (e.g. N WIND 10 to 20 KT)
                      # Set the moderating percentage in the "moderated_dict"
                      # dictionary module.
                      # Set the maximum range values in the "maximum_range_nlValue_dict"
                      # dictionary module.
                          ("Wind", self.vectorModeratedMinMax, [3]),
                          ("Wind", self.vectorMinMax, [12]),
                          ("WindGust", self.moderatedMax, [3]),
                          ("WaveHeight", self.moderatedMinMax, [6]),
                          ("WindWaveHgt", self.moderatedMinMax, [6]),
                          ("Swell", self.vectorModeratedMinMax, [6]),
                          ("Swell2", self.vectorModeratedMinMax, [6]),
                          ("Period", self.moderatedMinMax, [6]),
                          ("Period2", self.moderatedMinMax, [6]),
                          ("Wx", self.rankedWx, [6]),
                          ("T", self.minMax),
                          ("PoP", self._PoP_analysisMethod("OFFPeriod"), [6]),
                          ("PoP", self.binnedPercent, [6]),

                      # Use the following if you want moderated
                      # single values (e.g. N WIND 20 KT).   
                      # Set the moderating percentage in the "moderated_dict"
                      # dictionary module.
                      # NOTE:  If you use these methods, include and uncomment
                      # the "combine_singleValues_flag_dict" in your Local file (see below)
                         #("Wind", self.vectorModeratedMax, [3]),
                          #("Wind", self.vectorMinMax, [12]),
                          #("WindGust", self.moderatedMax, [3]),
                          #("WaveHeight", self.moderatedMax, [6]),
                          #("WindWaveHgt", self.moderatedMax, [6]),
                          #("Swell", self.vectorModeratedMax, [6]),
                          #("Swell2", self.vectorModeratedMax, [6]),
                          #("Period", self.moderatedMax, [6]),
                          #("Period2", self.moderatedMax, [6]),
                          #("Wx", self.rankedWx, [6]),                                  
                          #("T", self.minMax),
                          #("PoP", self._PoP_analysisMethod("OFFPeriod")),
                          #("PoP", self.binnedPercent, [6]),

                      # Use the following if you want absolute ranges.   
                      # Set the maximum range values in the "maximum_range_nlValue_dict"
                      # dictionary module.
                          # Split time range in quarters for Wind and WindGust
                          #("Wind", self.vectorMinMax, [3]),
                          #("Wind", self.vectorMinMax, [12]),
                          #("WindGust", self.maximum, [3]),
                          #("WaveHeight", self.minMax, [6]),
                          #("WindWaveHgt", self.minMax, [6]),
                          # Split time range in half for Wx and Swell
                          #("Swell", self.vectorMinMax, [6]),
                          #("Swell2", self.vectorMinMax, [6]),
                          #("Period", self.avg, [6]),
                          #("Period2", self.avg, [6]),
                          #("Wx", self.rankedWx, [6]),                                  
                          #("T", self.minMax),
                          #("PoP", self._PoP_analysisMethod("OFFPeriod")),
                          #("PoP", self.binnedPercent, [6]),
                        ],

             "phraseList":[
                           # WINDS
                           self.marine_wind_withGusts_phrase,
                           # Alternative:
                           #self.marine_wind_phrase,
                           #self.gust_phrase,
                           # WAVES
                           self.wave_withPeriods_phrase,
                           # Alternative:
                           #self.wave_phrase,
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

    def OFFPeriodMid(self):
        return {
            "type": "component",
            "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
            
            "analysisList": [            
                      # NOTE: Choose from the following analysis options.
                      # Do not remove the "vectorMinMax" analysis for
                      # "Wind". This is necessary to get an absolute max if
                      # the useWindsForGusts flag is on.
            
                      # Use the following if you want moderated ranges 
                      # (e.g. N WIND 10 to 20 KT)
                      # Set the moderating percentage in the "moderated_dict"
                      # dictionary module.
                      # Set the maximum range values in the "maximum_range_nlValue_dict"
                      # dictionary module.
                          ("Wind", self.vectorModeratedMinMax, [12]),
                          ("Wind", self.vectorMinMax, [12]),
                          ("WindGust", self.moderatedMax, [12]),
                          ("WaveHeight", self.moderatedMinMax, [12]),
                          ("WindWaveHgt", self.moderatedMinMax, [12]),
                          ("Swell", self.vectorModeratedMinMax, [12]),
                          ("Swell2", self.vectorModeratedMinMax, [12]),

                      # Use the following if you want moderated
                      # single values (e.g. N WIND 20 KT).   
                      # Set the moderating percentage in the "moderated_dict"
                      # dictionary module.
                      # NOTE:  If you use these methods, include and uncomment
                      # the "combine_singleValues_flag_dict" in your Local file (see below)
                          #("Wind", self.vectorModeratedMax, [12]),
                          #("Wind", self.vectorMinMax, [12]),
                          #("WindGust", self.moderatedMax, [12]),
                          #("WaveHeight", self.moderatedMax, [12]),
                          #("WindWaveHgt", self.moderatedMax, [12]),
                          #("Swell", self.vectorModeratedMax, [12]),
                          #("Swell2", self.vectorModeratedMax, [12]),

                      # Use the following if you want absolute ranges.   
                      # Set the maximum range values in the "maximum_range_nlValue_dict"
                      # dictionary module.
                          # Split time range in quarters for Wind and WindGust
                          #("Wind", self.vectorMinMax, [12]),
                          #("Wind", self.vectorMinMax, [12]),
                          #("WindGust", self.maximum, [3]),
                          #("WaveHeight", self.minMax, [12]),
                          #("WindWaveHgt", self.minMax, [12]),
                          # Split time range in half for Wx and Swell
                          #("Swell", self.vectorMinMax, [12]),
                          #("Swell2", self.vectorMinMax, [12]),
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
                           # SWELLS AND PERIODS
                           self.swell_phrase,
                           ],
            }

    def combine_singleValues_flag_dict(self, tree, node):
        # Dictionary of weather elements to combine using single values
        # rather than ranges.  If you are using single value statistics
        # for a weather element, you will want to set this flag to 1.
        # If there is no entry for an element, min/max combining will
        # be done.
        # The value for an element may be a phrase or a method
        # If a method, it will be called with arguments:
        #   tree, node
        dict = TextRules.TextRules.combine_singleValues_flag_dict(self, tree, node)
        #dict["Wind"] = 1
        #dict["WindGust"] = 1
        #dict["Swell"] = 1
        #dict["Swell2"] = 1
        #dict["WindWaveHgt"] = 1
        #dict["WaveHeight"] = 1        
        return dict

    def OFFExtended(self):
        return { "type": "component",
                 "methodList": [
                          self.consolidateSubPhrases,
                          self.assemblePhrases,
                          self.wordWrap,          
                          ],
                 "analysisList": [
                      # NOTE: Choose from the following analysis options.
                      # Do not remove the "vectorMinMax" analysis for
                      # "Wind". This is necessary to get an absolute max if
                      # the useWindsForGusts flag is on.
            
                      # Use the following if you want moderated ranges 
                      # (e.g. N WIND 10 to 20 KT)
                      # Set the moderating percentage in the "moderated_dict"
                      # dictionary module.
                      # Set the maximum range values in the "maximum_range_nlValue_dict"
                      # dictionary module.
                          ("Wind", self.vectorModeratedMinMax, [24]),                         
                          ("WindGust", self.moderatedMinMax, [24]),
                          ("WaveHeight", self.moderatedMinMax, [24]),
                          ("WindWaveHgt", self.moderatedMinMax, [24]),
                          #("Wx", self.rankedWx),
                          #("T", self.minMax),  # needed for weather_phrase
                          #("PoP", self._PoP_analysisMethod("OFFExtended")),
                          #("PoP", self.binnedPercent),
                          #("Swell", self.vectorModeratedMinMax, [12]),
                          #("Swell2", self.vectorModeratedMinMax, [12]),
                          
                      # Use the following if you want moderated
                      # single values (e.g. N WIND 20 KT).   
                      # Set the moderating percentage in the "moderated_dict"
                      # dictionary module.
                      # NOTE:  If you use these methods, include and uncomment
                      # the "combine_singleValues_flag_dict" in your Local file (see below)
                          #("Wind", self.vectorModeratedMax, [6]),                         
                          #("WindGust", self.moderatedMax, [12]),
                          #("WaveHeight", self.moderatedMax, [12]),
                          #("WindWaveHgt", self.moderatedMax, [12]),
                          #("Wx", self.rankedWx),
                          #("T", self.minMax),
                          #("PoP", self._PoP_analysisMethod("OFFExtended")),
                          #("PoP", self.binnedPercent),
                          #("Swell", self.vectorModeratedMax, [12]),
                          #("Swell2", self.vectorModeratedMax, [12]),

                      # Use the following if you want absolute ranges.   
                      # Set the maximum range values in the "maximum_range_nlValue_dict"
                      # dictionary module.
                      # dictionary module.
                          #("Wind", self.vectorMinMax, [6]),                         
                          #("WindGust", self.minMax, [12]),
                          #("WaveHeight", self.minMax, [12]),
                          #("WindWaveHgt", self.minMax, [12]),
                          #("Wx", self.rankedWx),     
                          #("T", self.minMax),
                          #("PoP", self._PoP_analysisMethod("OFFExtended")),
                          #("PoP", self.binnedPercent),
                          #("Swell", self.vectorMinMax, [12]),
                          #("Swell2", self.vectorMinMax, [12]),
                      ],
                 "phraseList":[ 
                               # WIND
                               self.marine_wind_phrase,
                               # WAVEHEIGHT
                               #self.wave_withPeriods_phrase,
                               # Alternative:
                               self.wave_phrase,
                               # SWELLS AND PERIODS
                               #self.swell_withPeriods_phrase,
                               # Alternative:
                               #self.swell_phrase,
                               #self.period_phrase,
                               # WEATHER
                               #self.weather_phrase,
                               #self.visibility_phrase,
                               ],
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
        self._timeLabel =  staticIssueTime + " " + self.getCurrentTime(
            argDict, " %Z %a %b %e %Y", stripLeading=1)
        # Re-calculate issueTime
        self._issueTime = self.strToGMT(staticIssueTime)
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
   
        fcst =  fcst + self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n" +\
               productName + "\n" +\
               "NATIONAL WEATHER SERVICE " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n\n"
        fcst = fcst + self._Text1()
        try:
            text2 = self._Text2(argDict["host"])
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
        #fcst = fcst + """NNNN   """
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst

    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################    
    def _issuance_list(self, argDict):
        #  This method sets up configurable issuance times with associated
        #  narrative definitions.  See the Text Product User Guide for documentation.
        if self._definition["includeEveningPeriod"] == 1:
            narrativeDefAM = [
                ("OFFPeriod", "period1"),
                ("OFFPeriod", 12), ("OFFPeriodMid", 12), ("OFFPeriodMid", 12), ("OFFPeriodMid", 12),
                ("OFFPeriodMid", 12),
                ("OFFExtended", 24), ("OFFExtended", 24)
                ]
            narrativeDefPM = [
                ("OFFPeriod", "period1"),
                ("OFFPeriod", 12), ("OFFPeriodMid", 12), ("OFFPeriodMid", 12), ("OFFPeriodMid", 12), ("OFFPeriodMid", 12),
                ("OFFPeriodMid", 12),
                ("OFFExtended", 24), ("OFFExtended", 24)
                ]
        else:
            narrativeDefAM = [
                ("OFFPeriod", "period1"),
                ("OFFPeriod", 12), ("OFFPeriodMid", 12), ("OFFPeriodMid", 12), ("OFFPeriodMid", 24),
                ("OFFExtended", 24), ("OFFExtended", 24)
                ]
            narrativeDefPM = [
                ("OFFPeriod", "period1"),
                ("OFFPeriod", 12), ("OFFPeriodMid", 12), ("OFFPeriodMid", 12), ("OFFPeriodMid", 12), ("OFFPeriodMid", 24),
                ("OFFExtended", 24), ("OFFExtended", 24)
                ]

        return [
            ("400 AM", self.DAY(), self.NIGHT(), 16,
             ".Today...", "early in the morning", "late in the afternoon",
             1, narrativeDefAM),
            #("1030 AM", "issuanceHour", self.NIGHT(), 16,
            # ".Today...", "early in the morning", "late in the afternoon",
            # 1, narrativeDefAM),
            #  End times are tomorrow:
            ("400 PM", self.NIGHT(), 24 + self.DAY(), 24 + 4,
             ".Tonight...", "late in the night", "early in the evening",
             1, narrativeDefPM),
            #("1030 PM", "issuanceHour", 24 + self.DAY(), 24 + 4,
            # ".Tonight...", "late in the night", "early in the evening",
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

    # Returns a list of the Hazards allowed for this product in VTEC format.
    # These are sorted in priority order - most important first.
    def allowedHazards(self):

        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        tropicalActions = ["NEW", "EXA", "EXB", "EXT", "UPG", "CAN", "CON", 
          "EXP"]
        marineActions = ["NEW", "EXA", "EXB", "EXT", "CON"]
        return [
            ('HU.W', tropicalActions, 'Tropical'),     # HURRICANE WARNING
            ('TY.W', tropicalActions, 'Tropical'),     # TYPHOON WARNING
            ('TR.W', tropicalActions, 'Tropical'),     # TROPICAL STORM WARNING
            ('HF.W', marineActions, 'Marine'),       # HURRICANE FORCE WIND WARNING
            ('SR.W', marineActions, 'Marine'),       # STORM WARNING
            ('GL.W', marineActions, 'Marine'),       # GALE WARNING
            ('SE.W', marineActions, 'Marine'),       # HAZARDOUS SEAS
            ('UP.W', allActions, 'IceAccr'),                        # HEAVY FREEZING SPRAY WARNING
            ('FG.Y', allActions, 'Fog'),                            # DENSE FOG ADVISORY
            ('SM.Y', allActions, 'Smoke'),                          # DENSE SMOKE ADVISORY
            ('UP.Y', allActions, 'IceAccr'),                        # HEAVY FREEZING SPRAY ADVISORY
            ('AF.W', allActions, 'Ashfall'),                        # VOLCANIC ASHFALL WARNING
            ('AF.Y', allActions, 'Ashfall'),                        # VOLCANIC ASHFALL ADVISORY
            ]
