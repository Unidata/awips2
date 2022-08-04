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

##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the
# GFE Training Guide->GFE Text Products User Guide section of the GFE Online
# Help for guidance on creating a new text product.
##

#-------------------------------------------------------------------------
# Description: This produces a Spot Weather Forecast.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# FWS, FWS_<site>_<MultiPil>_Definition, FWS_<site>_Overrides
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
#  productName      defines name of product e.g. "Zone Forecast Product"
#  fullStationID    Full station identifier, 4 letter, such as "KSLC".
#  wmoID            WMO ID code for product header, such as "FOUS45"
#  pil              Product pil, such as "SFTBOS"
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
#                       reported with phrases like "1-8 strikes",
#                       "9-15 strikes", etc.
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
#  Refer to the NWS Directives for Spot Weather Forecasts.
#-------------------------------------------------------------------------
# Editing History:
# 06/02/2016 - Version 1.0.6  - Smoke Dispersal bug. vtm
# 08/24/2016 - Version 1.0.7  - Bug Fixes and phrase additions. vtm
# 08/25/2016 - Version 1.0.8  - OR OR bug in Haines_words. vtm
# 08/30/2016 -                - Mid Haines addition and bug fix. vtm
#                             - Mix Height, Transport Wind, Smoke Dispersal bug fix. vtm
# 09/06/2016 - Version 1.0.9  - Mixed Case work. Added decoding for Precipitation Amount and
#                               Pressure (in). Freezing Spray bug fix attempt. vtm
# 09/08/2016 - Version 1.0.10 - Manual input _extendedDefaultFlag crash. vtm
#                               Update to windWave_words and waveHeight_words to add foot wording. vtm
# 09/28/2016 - Version 1.0.11 - Big fixes with mixing height/transport wind related elements. vtm
#                               Added wind 1000/2000/3000/4000/5000 ft phrases. vtm
# 09/30/2016 - Version 1.0.12 - Added HiOneTenth, Water Temperature, and River Temperature phrases. vtm
# 10/26/2016 - Version 1.0.13 - Added Snow Level phrase. Fixed Wave Height and Visibility narrative labels (upper case)
#                               Fixed narrative Smoke Dispersal issues (Excellent to Excellent)
#                               Tabular 20 foot winds had adjustment applied twice.
#                               Problems with scraping emails from STQ product. vtm
# 10/27/2016 - Version 1.0.14 - Some Definitions duplicated in Spot_Overridies. vtm
# 11/15/2016                    Removed 2 period Afternoon, Evening Update, and Early Morning Update issuances.
#                               Renamed 4 period issuances to Afternoon, Evening Update, and Early Morning Update. vtm
#                               Fixed Bug with Metric Mixing Height. vtm
#                               Fixed Bug with tabular Icing. vtm
# 03/10/2017                    Removed Next Day Issuance. vtm
#                               Added the Auto Product Issuance. vtm
#                               Added the ability to edit the .TAG code. vtm
#                               Added manual entry by incident type. Weather element lists specific to type. vtm
# 03/13/2017 - Version 1.0.15 - mixingHgtMsl_words puts in AGL instead of MSL. (#1952) vtm
#                               Generate Message if no weather elements are selected in 2nd GUI. vtm
# 04/05/2017 - Version 1.0.16 - haines_words puts a blank between the haines value and the period if haines_dict
#                                 is configured with blanks for all values.
# 04/20/2017 -                  Bug fix - Missing Rest of Today label for 5 pm start times and missing Rest of
#                                         Tonight label for 5 am start times. createLabel method.
# 05/23/2017 -                  "PROFILE in remarks causes request to no be in list in 1st GUI. Bug fixed. vtm
# 08/17/2017 -                  Winds for Marine requests are now in knots. Implemented better conversions between
#                               surface, 20 foot, and eye level winds. vtm
# 01/23/2018 - version 1.0.17 - Re-written to handle Python 2.7 valid time structures and new naive vs. aware datetime objects. Bookbinder
# 03/12/2018                  - Fixed forecast start date/time decoding issues related to timezone changes. vtm
# 03/29/2018                  - Leaving default timezone (like MDT) in manual entry causes formatter to crash. Fixed. vtm
#                             - Added .DELDT line to bottom of forecast to help with statistical logging at Billings. vtm
# 04/12/2018 - version 1.0.18 - Added code to generate a blank forecast for 100 ft wind. vtm
# 04/12/2018 -                  Worked on narrative surface, 20 foot and eye level wind phrases (both narrative and tabular).
#                               If all of these are in one forecast, they will have the correct reductions. vtm
# 05/02/2018 -                  Manual entry did not use defaults for forecast format and intervals set in Definitions file. vtm
# 08/29/2018 -                  Added code to decode new Surface Wind (Kts) element on website. vtm
# 10/22/2018 - version 1.0.19 - Ventilation Rate in table now two rows...value and category. vtm
# 10/26/2018 -                - Worked on mixingHgt_words and scalar_difference_nlValue_dict to work better with hourly MixHgt grids. vtm
#                             - Fixed issue with narrative period labels for forecast start times of 5 am and 5 pm. i.e. two tonight periods. vtm
# 01/17/2019 -                - Fixed horrible plume wording for Haines for offices that create hourly Haines grids. vtm
# 01/17/2019 -                - Added sampling and analysis of SST grid in addition to SSTAK grid. vtm
# 01/24/2019 - version 1.0.20 - Worked on Smoke Dispersal narrative phrases to work better with hourly grids.
#                             - Put in different smoke dispersal category thresholds for 5 WFOs.
#                             - Defaulted Forecast Delivery Date, wfoid, and tag better for manual runs in _getProductInfo. vtm
# 01/31/2019 - version 1.0.21 - Bug: 2000 FT WIND was decoded as WIND 20 FT. Fixed issue in _weInfoList. vtm
#                             - Bug: Max Humidity listed twice in forecast when HUMIDITY and MAX/MIN HUMIDITY both on STQ.
#                                    Modified _getProductInfo. vtm
#                             - Change: Wnd1000Ft grid changed to Wind1000Ft grid. Ditto for 2000, 3000, 4000, and 5000 foot levels. vtm
#                             - Bug: Added Wind1000Ft, etc to increment_nlValue_dict, element_inUnits_dict, and element_outUnits_dict. vtm
# 02/06/2019 - version 1.0.22 - Bug: manual entry of STQ information results in no default Forecast Type in 2nd gui. vtm
#                             - Bug: No STQs in database causes default element lists to be ignored for manual STQ entry. vtm
# 02/07/2019 - version 1.0.23 - Bug: Tablular wind gusts too high, because conversion from wind 20 ft to wind 10 m was applied
#                                    when not needed. vtm
#                             - Added formatter version to FWS product for tracking. vtm
# 02/08/2019 - version 1.0.24 - Bug: version 1.0.22 introduced crashes when running the manually enter options. vtm
# 02/12/2019 - version 1.0.25 - Bug: Type in three places. NinMax instead of MinMax. vtm
# 03/06/2019 - version 1.0.26 - Removed &#39; string from STQ products (single quote). vtm
# 04/10/2019 - version 1.0.27 - Worked on narrative wording for ADI. vtm
# 03/08/2020 - version 2.0.0  - Coding changes to formatter for DCS 20845
# 12/16/2020 - version 2.0.1  - Removed the 'Narrative Only' default forecast type assignment from _displayFireInfo
#                               module which was preventing the defaultForecastType override (DR 22397)
#
# ------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer    Description
# ------------- -------- ----------- -------------------------------------------
# Mar 11, 2020  20845    vmiddendorf Upgrade GFE FWS formatter
# May 21, 2020  21986    dhaines     Overriding PhraseBuilder.combineVectors to
#                                    better handle wind direction changes in 1 hr
#                                    grids
# Dec 16, 2020  22397    vmiddendorf FWS Formatter defaulting to 'Narrative Only'
#                                    even when overridden
# Apr 26, 2021  22617    ryu         Check in fix from Virgil.
# May 05, 2021  22617    ryu         Modified solution to fix bleeding into new
#                                    period.
# Feb 22, 2022  8783     randerso    Fix formatting of rounded values.
#                                    Code cleanup.
#-------------------------------------------------------------------------

from datetime import datetime
import math
import os
import pwd
import re
import time

import pytz

import AbsTime
import ForecastNarrative
import HazardsTable
import Holidays
import ProcessVariableList
import SampleAnalysis
import TextRules
import TimeRange


class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = []
    Definition = {
        "version": "2.0.0",
        "type": "smart",
        "displayName": None,
        # Source database for product. Can be "Official", "Fcst" or "ISC"
        "database": "Official",
        # Defines output location of finished product.
        "outputFile": "{prddir}/TEXT/FWS.txt",
        "debug": 0,
        # Name of map background for creating Combinations
        "mapNameForCombinations": None,

        # Edit Areas: Create Combinations file with edit area combinations.
        "showZoneCombiner": 0,  # 1 to cause zone combiner to display
        "defaultEditAreas": [],
        "editAreaSuffix": None,

        # product identifiers
        "productName": "Spot Forecast",  # product name
        "fullStationID": "<fullStationID>",  # full station identifier (4letter)
        "wmoID": "<wmoID>",  # WMO ID
        "pil": "<pil>",  # Product pil
        # Name of state, such as "Georgia" -- optional
        "areaName": "<state>",
        "wfoCityState": "<wfoCityState>",  # Location of WFO - city state

        # Product ID for storing to AWIPS text database.
        "textdbPil": "<textdbPil>",
        # Product ID for transmitting to AWIPS WAN.
        "awipsWANPil": "<awipsWANPil>",
        "periodCombining": 0,  # If 1, combine periods, if possible
        "lineLength": 66,  # line length

        "hazardSamplingThreshold": (10, None),  # (%cov, #points)

        # Product-specific variables:
        # Set to 1 to report lightning as phrases (e.g. 1-8 STRIKES)
        "lightningPhrases": 0,
        # Adjustment for Wind if no Wind20ft grid available
        "windAdjustmentFactor": 1.00,
        # Adjustment for Wind if no Wind20ft grid available
        "eyeWindAdjustmentFactor": 0.60,

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
        "elementList": ["Temp", "Humidity", "PoP"],
        "singleValueFormat": 0,
        "cityDictionary": "CityDictionary",
        # Area Dictionary -- Descriptive information about zones
        "areaDictionary": "AreaDictionary",
        # Language
        "language": "english",
        "lowerCase": 1,

        "useRH": 0,  # Use RH grids instead of MaxRH, MinRH
        # If summaryExtended == 1, then a summary extended forecast will
        # be generated for the given summaryArea
        "summaryExtended": 0,
        "summaryArea":"FireWxAOR_<site>_<MultiPil>",
        # If individualExtended == 1, an extended forecast will be
        # generated for each individual area
        # If extendedLabel == 1, a label will be included for each
        #  individual extended
        "individualExtended": 1,
        "extendedLabel": 1,

        # Weather-related flags
        "hoursSChcEnds": 24,

        # Set to 1 to use holidays in the time period labels
        "useHolidays": 1,
        # Set to 1 to include Temp and RH trends
        "includeTrends": 1,
        # Set to 1 to enable Temp and RH local effects AFTER
        # creating AboveElev and BelowElev edit areas
        "tempLocalEffects": 0,
        # Set to 1 to enable wind local effects AFTER
        # creating Ridges and Valleys edit areas
        "windLocalEffects": 0,
        # ensure VTEC actions don't affect expiration time
        "fixedExpire": 1,

        # Trouble-shooting items
        "passLimit": 20,  # Limit on passes allowed through
                                     # Narrative Tree
        "trace": 0,  # Set to 1 to turn on trace through
                                     # Narrative Tree for trouble-shooting
        "agencyList": [],
        "forecasterList": [(1, "forecastera", "Forecaster A"),
                           (2, "forecasterb", "Forecaster B"),
                           (3, "forecasterc", "Forecaster C"),
                           (4, "forecasterd", "Forecaster D"),
                           (5, "forecastere", "Forecaster E")],
        "stqNumberVersions": 50,
        "stqWmoID": "BMBB91 K",
        "stqPil": "STQ<site>",  # STQ pil
        "fwfPil": "FWF<site>",
        "typeList": ["WILDFIRE", "PRESCRIBED", "HAZMAT", "SAR", "MARINE", "OTHER"],
        "defaultElementDict": {
            "WILDFIRE": ["SKY/WEATHER",
                         "TEMPERATURE",
                         "HUMIDITY",
                         "20 FOOT WINDS",
                         "RIDGE TOP WIND",
                         "CWR",
                         "LIGHTNING ACTIVITY LEVEL"],
            "PRESCRIBED": ["SKY/WEATHER",
                           "TEMPERATURE",
                           "HUMIDITY",
                           "20 FOOT WINDS",
                           "RIDGE TOP WIND",
                           "CWR",
                           "LIGHTNING ACTIVITY LEVEL",
                           "MIXING HEIGHT",
                           "TRANSPORT WINDS"],
            "HAZMAT Land": ["SKY/WEATHER",
                            "CHANCE OF PRECIPITATION",
                            "TEMPERATURE",
                            "HUMIDITY",
                            "DEWPOINT",
                            "SURFACE WINDS",
                            "MIXING HEIGHT",
                            "TRANSPORT WINDS"],
            "HAZMAT Inland Waterway": ["SKY/WEATHER",
                                       "CHANCE OF PRECIPITATION",
                                       "TEMPERATURE",
                                       "HUMIDITY",
                                       "DEWPOINT",
                                       "SURFACE WINDS",
                                       "MIXING HEIGHT",
                                       "TRANSPORT WINDS"],
            "SAR Land": ["SKY/WEATHER",
                         "CHANCE OF PRECIPITATION",
                         "TEMPERATURE",
                         "HUMIDITY",
                         "DEWPOINT",
                         "SURFACE WINDS"],
            "SAR Water": ["SKY/WEATHER",
                          "CHANCE OF PRECIPITATION",
                          "TEMPERATURE",
                          "HUMIDITY",
                          "DEWPOINT",
                          "SURFACE WINDS"],
            "MARINE": ["SKY/WEATHER",
                       "TEMPERATURE",
                       "SURFACE WINDS"],
            "OTHER": ["SKY/WEATHER",
                      "TEMPERATURE",
                      "HUMIDITY",
                      "SURFACE WINDS"],
        },
        "shortTermOnly": 1,
        "insertUnrepresentStatement": 1,
        "unrepresentStatement": "If conditions become unrepresentative...contact the National Weather\nService.",
        "contactStatement": "Please add and configure contactStatement definition to your FWS_XXX_Definition file.",
        "includeEmails": 1,
        "insertDiscussionFromFile": 0,
        "discussionFile": "/home/local_apps/xnow/temp/DISFWFBYZ",
        "insertOutlookFromFile": 0,
        "outlookFile": "/home/local_apps/xnow/temp/OLKFWFBYZ",
        "wildfireElementList": ["SKY/WEATHER",
                                "TEMPERATURE",
                                "HUMIDITY",
                                "20 FOOT WINDS",
                                "EYE LEVEL WINDS"],
        "forecastTypeList": [
            # Label                Forecast Type       Code
            ("Narrative Only", "Narrative Only", "N"),
            ("Tabular/Narrative", "Tabular/Narrative", "C"),
            ("Tabular Only", "Tabular Only", "T"),
            # If your _rowList specifies an all Tabular product,
            # you may want to change this entry to:
            #("Tabular",              "Tabular/Narrative", "T"   ),
        ],
        "defaultForecastType": "Tabular/Narrative",
        "withIgnitionTimes": "no",
        "includeIgnitionOptionOnGUI": 1,
        "tabularResolutionDict": {
            "Period 1": [1, 2, 3, 4, "None"],
            "Period 2": [1, 2, 3, 4, "None"],
            "Period 3": [1, 2, 3, 4, "None"],
            "Period 4": [1, 2, 3, 4, "None"]
        },
        "defaultTabularResolution": {
            "Period 1": 2,
            "Period 2": 2,
            "Period 3": 2,
            "Period 4": 2
        },
        "tabularAllPeriods": "yes",
        "tabularForWildfire": "no",
        "tableStartTimeMode": "ignitionTime",
        "tableStartTimeOffset": 0,
        "ignitionForecastTypeList": [],
        "elementFormatDict": {
            "Sky": "numeric",
            "Wind": "alpha",
            "Wind20ft": "alpha",
            "EyeWind": "alpha",
            "RidgeWind": "alpha",
            "TransWind": "alpha",
            "TransMetWind": "alpha",
        },
        "bothAlphaNumericDict": {
            "Sky": "No",
            "Wind": "No",
            "Wind20ft": "No",
            "EyeWind": "No",
            "SfcWind": "No",
            "RidgeWind": "No",
            "TransWind": "No",
        },
        # Will only be used for 2,3, or 4 time resolutions.
        "tabularMixingHeightUnits": "ft",
        # Will only be used for 2,3, or 4 time resolutions.
        "tabularMixingHeightMetricUnits": "m",
        "transportWindLabel": "tran",
        "includeMetricDispersion": "no",
        "20ftWindParm": "Wind",
        "wind20ftHeader": 0,  # Use 1 for yes, 0 for no
        "tableWindElementSplit": "no",
        "tableEyeWindElementSplit": "no",
        "tableRidgeElementSplit": "no",
        "tableTransElementSplit": "no",
        "tableSwellElementSplit": "no",
        "tableSfcWindElementSplit": "no",
        "cwrParm": "PoP",
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
        self.debug_print("Debug: wxCombinations in FWS.py")
        return [
            ("RW", "R"),
            ("SW", "S"),
            # ("T","RW"),
        ]

    def minMax_std_deviation(self, parmHisto, timeRange, componentName):
        self.debug_print("Debug: minMax_std_deviation in FWS.py")
        # Replaces MINMAX_STD_DEVIATION
        # Number of standard deviations to compute around the weighted
        # average for a stdDev_MinMax
        return 1.4

    def element_outUnits_dict(self, tree, node):
        self.debug_print("Debug: element_outUnits_dict in FWS.py")
        outUnitsDict = TextRules.TextRules.element_outUnits_dict(
            self, tree, node)
        outUnitsDict["Wind"] = "mph"
        outUnitsDict["Wind20ft"] = "mph"
        outUnitsDict["TransWind"] = "mph"
        outUnitsDict["Wind1000Ft"] = "mph"
        outUnitsDict["Wind2000Ft"] = "mph"
        outUnitsDict["Wind3000Ft"] = "mph"
        outUnitsDict["Wind4000Ft"] = "mph"
        outUnitsDict["Wind5000Ft"] = "mph"
        outUnitsDict["FreeWind"] = "mph"
        outUnitsDict["WindGust"] = "mph"
        outUnitsDict["Visibility"] = "SM"
        return outUnitsDict

    #############################
    # Overrides to take care of Wind in the Extended forecast
    # Use Wind20ft if available, else use adjusted Wind
    #
    def adjust_method_dict(self, tree, node):
        self.debug_print("Debug: adjust_method_dict in FWS.py")
        # Special adjustment methods
        #
        return {
            "Wind": self._adjustWind,
        }

    def _adjustWind(self, value):
        self.debug_print("Debug: adjustWind in FWS.py")
        # adjustment for winds
        factor = self.nlValue(self._windAdjustmentFactor, value)
        value = value * factor
        return value

    def wind_summary_words(self, tree, node):
        self.debug_print("Debug: wind_summary_words in FWS.py")
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
        self.debug_print("Debug: wind_setUp in FWS.py")
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
        self.debug_print("Debug: phrase_descriptor_dict in FWS.py")
        # Descriptors for phrases
        phraseDesDict = TextRules.TextRules.phrase_descriptor_dict(
            self, tree, node)
        # If not extended, make descriptor empty
        componentName = node.getComponent().get("name")
        if componentName == "FirePeriod":
            phraseDesDict["Wind"] = ""
            phraseDesDict["Wind20ft"] = ""
        phraseDesDict["SKY/WEATHER........."] = "Sky/weather........."
        phraseDesDict["   24 HR TREND......"] = "   24 hr trend......"
        phraseDesDict["MinT_FireWx"] = "Min temperature....."
        phraseDesDict["MaxT_FireWx"] = "Max temperature....."
        phraseDesDict["MinRH_FireWx"] = "Min humidity........"
        phraseDesDict["MaxRH_FireWx"] = "Max humidity........"
        if self._wind20ftHeader:
            phraseDesDict["WIND.(20 FT)........"] = "Wind (20 ft)........"
            phraseDesDict["20-FOOT WINDS......."] = " Slope/valley......."
            phraseDesDict["SLOPE/VALLEY WINDS.."] = " Slope/valley......."
            phraseDesDict["LOWER SLOPE WINDS..."] = " Valley/lwr slopes.."
            phraseDesDict["FREE WINDS.........."] = " Ridgetop..........."
            phraseDesDict["SURROUNDING RIDGE..."] = " Surrounding ridge.."
        else:
            phraseDesDict["20-FOOT WINDS......."] = "Wind (20 ft)........"
            phraseDesDict["SLOPE/VALLEY WINDS.."] = "Slope/valley winds.."
            phraseDesDict["LOWER SLOPE WINDS..."] = "Valley/lwr slp wind."
            phraseDesDict["FREE WINDS.........."] = "Ridgetop wind......."
            phraseDesDict["SURROUNDING RIDGE..."] = "Surrounding ridge..."
        phraseDesDict["EYE LEVEL WINDS....."] = "Eye level winds....."
        phraseDesDict["SURFACE WINDS......."] = "Surface winds (mph)."
        phraseDesDict["SURFACE WINDS KTS..."] = "Surface winds (kts)."
        phraseDesDict["WIND SHIFT.........."] = "Wind shift.........."
#        if self._transportWindLabel == "mix":
#            phraseDesDict["TRANSPORT WINDS....."] = "Mixing winds........"
#        else:
#            phraseDesDict["TRANSPORT WINDS....."] = "Transport winds....."
        phraseDesDict["MIXING WINDS........"] = "Mixing winds........"
        phraseDesDict["TRANSPORT WINDS....."] = "Transport winds....."
        phraseDesDict["MIXING WINDS (M/S).."] = "Mixing winds (m/s).."
        phraseDesDict["TRANSPORT WINDS M/S."] = "Transport winds m/s."
        phraseDesDict["1000 FT WINDS......."] = "1000 ft winds......."
        phraseDesDict["2000 FT WINDS......."] = "2000 ft winds......."
        phraseDesDict["3000 FT WINDS......."] = "3000 ft winds......."
        phraseDesDict["4000 FT WINDS......."] = "4000 ft winds......."
        phraseDesDict["5000 FT WINDS......."] = "5000 ft winds......."
        phraseDesDict["MIXING HEIGHT......."] = "Mixing height......."
        phraseDesDict["MIXING HEIGHT (M)..."] = "Mixing height (m)..."
        phraseDesDict["MIXING HEIGHT (MSL)."] = "Mixing height (MSL)."
        phraseDesDict["CWR................."] = "CWR................."
        phraseDesDict["DSI................."] = "Dispersion.........."
        phraseDesDict["LDSI................"] = "Dispersion index...."
        phraseDesDict["LVORI..............."] = "LVORI..............."
        phraseDesDict["ADI................."] = "ADI................."
        phraseDesDict["GFDI................"] = "GFDI................"
        phraseDesDict["SMOKE DISPERSAL....."] = "Smoke dispersal....."
        phraseDesDict["POP................."] = "Chance of pcpn......"
        phraseDesDict["CHANCE OF RAIN......"] = "Chance of rain......"
        phraseDesDict["CHANCE OF THUNDER..."] = "Chance of thunder..."
        phraseDesDict["CHANCE OF LIGHTNING."] = "Chance of lightning."
        phraseDesDict["DEWPOINT............"] = "Dewpoint............"
        phraseDesDict["BEGIN/END OF PCPN..."] = "Begin/end of pcpn..."
        phraseDesDict["STABILITY CLASS....."] = "Stability class....."
        phraseDesDict["WIND WAVE..........."] = "Wind wave..........."
        phraseDesDict["WAVE HEIGHT........."] = "Wave height........."
        phraseDesDict["HI ONE TENTH........"] = "Hi 1/10th height...."
        phraseDesDict["RAINFALL AMOUNT....."] = "Rainfall amount....."
        phraseDesDict["PCPN AMOUNT........."] = "Pcpn amount........."
        phraseDesDict["SNOWFALL AMOUNT....."] = "Snowfall amount....."
        phraseDesDict["SWELL PERIOD........"] = "Swell period........"
        phraseDesDict["WAVE PERIOD........."] = "Wave period........."
        phraseDesDict["SWELL HEIGHT........"] = "Swell height........"
        phraseDesDict["FREEZING SPRAY......"] = "Freezing spray......"
        phraseDesDict["SEA ICE COVERAGE...."] = "Sea ice coverage...."
        phraseDesDict["SEA SURFACE TEMP...."] = "Sea surface temp...."
        phraseDesDict["FREEZING LEVEL......"] = "Freezing level......"
        phraseDesDict["SNOW LEVEL.........."] = "Snow level.........."
        phraseDesDict["MARINE LAYER........"] = "Marine layer........"
        phraseDesDict["CEILING............."] = "Ceiling............."
        phraseDesDict["VISIBILITY (SM)....."] = "Visibility.........."
        phraseDesDict["SFC PRESSURE (IN)..."] = "Sfc pressure (in)..."
        phraseDesDict["ICING..............."] = "Icing..............."
        phraseDesDict["RIVER LEVEL........."] = "River level........."
        phraseDesDict["RIVER TEMPERATURE..."] = "River temperature..."
        phraseDesDict["WATER TEMPERATURE..."] = "Water temperature..."
        phraseDesDict["HEAT INDEX.........."] = "Heat index.........."
        phraseDesDict["WIND CHILL.........."] = "Wind chill.........."
        phraseDesDict["APPARENT TEMP......."] = "Apparent temp......."
        phraseDesDict["MIN APPARENT TEMP..."] = "Min apparent temp..."
        phraseDesDict["MAX APPARENT TEMP..."] = "Max apparent temp..."
        phraseDesDict["HAINES INDEX........"] = "Haines index........"
        phraseDesDict["HAINES MID LEVEL...."] = "Haines mid level...."
        phraseDesDict[
            "erraticWind"] = "gusty and erratic winds expected near thunderstorms"
        if self._withIgnitionTimes == "yes":
            phraseDesDict["MinT_FireWx"] = "Temperature........."
            phraseDesDict["MaxT_FireWx"] = "Temperature........."
            phraseDesDict["MinRH_FireWx"] = "RH.................."
            phraseDesDict["MaxRH_FireWx"] = "RH.................."
        return phraseDesDict

    def nextDay24HourLabel_flag(self, tree, node):
        self.debug_print("Debug: nextDay24HourLabel in FWS.py")
        # Return 1 to have the TimeDescriptor module label 24 hour periods starting
        # after 1600 as the next day.
        # This is needed for the Fire Weather Extended product,
        # but not for other products when period combining.
        return 1

    def untilPhrasing_flag_dict(self, tree, node):
        self.debug_print("Debug: untilPhrasing_flag_dict in FWS.py")
        # If set to 1, "until" time descriptor phrasing will be used.
        # E.g. "North winds 20 MPH until 10 AM, then 35 MPH"
        #
        # NOTE: Be sure to increase the temporal resolution by
        # overriding "getFirePeriod_analysisList" from the FWF standard file.
        # E.g.  ("MixHgt", self.minMax, [0]),

        untilDict = TextRules.TextRules.untilPhrasing_flag_dict(
            self, tree, node)
        untilDict["LAL"] = 1
        componentName = node.getComponent().get("name")
        if componentName == "FirePeriod":
            untilDict["Sky"] = 1
            untilDict["Wx"] = 1
        return untilDict

    ########################################################################
    # COMPONENT PRODUCT DEFINITIONS
    ########################################################################

    def _PoP_analysisMethod(self, componentName):
        self.debug_print("Debug: _PoP_analysisMethod in FWS.py")
        # Alternative PoP analysis methods for consistency between PoP and Wx
        # return self.maxMode
        # return self.maximum
        return self.stdDevMaxAvg

    def FirePeriod(self):
        self.debug_print("Debug: FirePeriod in FWS.py")
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
        self.debug_print("Debug: ExtraSampling in FWS.py")
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
        self.debug_print("Debug: getFirePeriod_analysisList in FWS.py")
        # Note: Some weather elements are commented out because they generate red-banners
        #       for some offices. If you need any of the commented elements, then move
        #       this method into your Spot_???_Overrides file and uncoment the elements
        #       you need.
        if self._forecastType in ["Tabular/Narrative", "Tabular Only"] or \
                self._withIgnitionTimes == "yes":
            analysisList = [
                ("Sky", self.median, [1]),
                ("Wx", self.rankedWx, [1]),
                ("PoP", self.stdDevMaxAvg, [1]),
                ("PoP", self.binnedPercent, [1]),
                ("LAL", self.maximum, [1]),
                ("LAL", self.binnedPercent, [1]),
                ##("PotRain", self.stdDevMaxAvg, [1]),
                ##("PotRain", self.binnedPercent, [1]),
                ##("PotThunder", self.stdDevMaxAvg, [1]),
                ##("PotThunder", self.binnedPercent, [1]),
                ("MaxT", self.moderatedMinMax),
                ("MinT", self.moderatedMinMax),
                ("MaxRH", self.moderatedMinMax),
                ("MinRH", self.moderatedMinMax),
                ("RH", self.avg, [1]),
                ("RH", self.moderatedMinMax),
                ("MaxT", self.avg),  # for trends
                ("MinT", self.avg),  # for trends
                ("MaxRH", self.avg),  # for trends
                ("MinRH", self.avg),  # for trends
                ("RH", self.avg),  # for trends
                ("T", self.avg, [1]),
                ("T", self.hourlyTemp),
                ("T", self.minMax),
                ("Td", self.avg, [1]),
                ("Td", self.hourlyTemp),
                ("Td", self.minMax),
                ("Wind", self.vectorMinMax, [1]),
                ("WindGust", self.maximum, [1]),
                ("Wind20ft", self.vectorMinMax, [1]),
                ("Haines", self.maximum, [1]),
                ("HainesMid", self.maximum, [1]),
                ("TransWind", self.vectorAvg, [1]),
                ("FreeWind", self.vectorAvg, [1]),
                ##("Wind1000Ft", self.vectorAvg, [1]),
                ##("Wind2000Ft", self.vectorAvg, [1]),
                ##("Wind3000Ft", self.vectorAvg, [1]),
                ##("Wind4000Ft", self.vectorAvg, [1]),
                ##("Wind5000Ft", self.vectorAvg, [1]),
                ("MixHgt", self.moderatedMin, [1]),
                ("VentRate", self.minMax, [1]),
                ("DSI", self.maximum, [1]),
                ##("LDSI", self.maximum, [1]),
                ##("LVORI", self.maximum, [1]),
                ##("ADI", self.maximum, [1]),
                ##("GFDI", self.maximum, [1]),
                ("CWR", self.maximum, [1]),
                ("Stability", self.maximum, [1]),
                ("MarineLayer", self.maximum, [1]),
                ("Swell", self.vectorMinMax, [1]),
                ("Period", self.maximum, [1]),
                ("WindWaveHgt", self.maximum, [1]),
                ("WaveHeight", self.maximum, [1]),
                ##("HiOneTenth", self.maximum, [1]),
                ##("IceC", self.maximum, [1]),
                ##("SSTAK", self.maximum, [1]),
                ##("SST", self.maximum, [1]),
                ##("FrzngSpry",self.dominantDiscreteValue, [1]),
                ("QPF", self.accumSum, [6]),
                ("SnowAmt", self.accumSum, [6]),
                ("FzLevel", self.median, [1]),
                ("SnowLevel", self.median, [1]),
                ("Hazards", self.dominantDiscreteValue, [1]),
                #("Vsby", self.minimum, [1]),
                #("PredHgt", self.minimum, [1]),
                ("Visibility", self.minimum, [1]),
                ("CloudBasePrimary", self.minimum, [1]),
                ("Pres", self.minimum, [1]),
                ("HeatIndex", self.maximum, [1]),
                ("WindChill", self.minimum, [1]),
                ("ApparentT", self.minMax, [1]),
            ]
        else:
            analysisList = [
                ("Sky", self.median, [6]),
                ("PoP", self.stdDevMaxAvg, [6]),
                ("PoP", self.binnedPercent, [6]),
                ##("PotRain", self.stdDevMaxAvg, [6]),
                ##("PotRain", self.binnedPercent, [6]),
                ##("PotThunder", self.stdDevMaxAvg, [6]),
                ##("PotThunder", self.binnedPercent, [6]),
                ("Wx", self.rankedWx, [6]),
                ("LAL", self.maximum, [12]),
                ("LAL", self.binnedPercent, [0]),
                ("MaxT", self.moderatedMinMax),
                ("MinT", self.moderatedMinMax),
                ("MaxRH", self.moderatedMinMax),
                ("MinRH", self.moderatedMinMax),
                ("RH", self.avg, [1]),
                ("RH", self.moderatedMinMax),
                ("MaxT", self.avg),  # for trends
                ("MinT", self.avg),  # for trends
                ("MaxRH", self.avg),  # for trends
                ("MinRH", self.avg),  # for trends
                ("RH", self.avg),  # for trends
                ("T", self.avg, [1]),
                ("T", self.hourlyTemp),
                ("T", self.minMax),
                ("Td", self.avg, [1]),
                ("Td", self.hourlyTemp),
                ("Td", self.minMax),
                ("Wind", self.vectorMinMax, [6]),
                ("WindGust", self.maximum, [6]),
                ("Wind20ft", self.vectorMinMax, [6]),
                ("Haines", self.maximum),
                ("HainesMid", self.maximum),
                ("TransWind", self.vectorAvg, [12]),
                ("FreeWind", self.vectorAvg, [12]),
                ##("Wind1000Ft", self.vectorAvg, [12]),
                ##("Wind2000Ft", self.vectorAvg, [12]),
                ##("Wind3000Ft", self.vectorAvg, [12]),
                ##("Wind4000Ft", self.vectorAvg, [12]),
                ##("Wind5000Ft", self.vectorAvg, [12]),
                ("MixHgt", self.moderatedMin, [1]),
                ("VentRate", self.minMax),
                ("CWR", self.maximum),
                ("DSI", self.maximum, [12]),
                ##("LDSI", self.maximum, [12]),
                ##("LVORI", self.maximum, [12]),
                ##("ADI", self.maximum, [12]),
                ##("GFDI", self.maximum, [12]),
                ("Stability", self.maximum),
                ("MarineLayer", self.maximum),
                ("Swell", self.vectorMinMax, [6]),
                ("Period", self.maximum, [6]),
                ("WindWaveHgt", self.maximum, [6]),
                ("WaveHeight", self.maximum, [6]),
                ##("HiOneTenth", self.maximum, [6]),
                ##("IceC", self.maximum, [6]),
                ##("SSTAK", self.maximum, [6]),
                ##("SST", self.maximum, [6]),
                ##("FrzngSpry",self.dominantDiscreteValue, [6]),
                ("QPF", self.accumMinMax, [6]),
                ("SnowAmt", self.accumMinMax, [6]),
                ("FzLevel", self.median, [6]),
                ("SnowLevel", self.median, [6]),
                ("Hazards", self.dominantDiscreteValue),
                #("Vsby", self.minimum, [6]),
                #("PredHgt", self.minimum, [6]),
                ("Visibility", self.minimum, [6]),
                ("CloudBasePrimary", self.minimum, [6]),
                ("Pres", self.minimum, [6]),
                ("HeatIndex", self.maximum, [6]),
                ("WindChill", self.minimum, [6]),
                ("ApparentT", self.minMax, [6]),
            ]
        return analysisList

    def getFirePeriod_phraseList(self, periodElements):
        self.debug_print("Debug: getFirePeriod_phraseList in FWS.py")
        phraseList = []
        if self._forecastType in ["Tabular/Narrative"]:
            # Figure out which narrative phrases should be included
            narratives = []
            tableElements = []
            for rowElement, narrativeToo, tableRows in self._rowList():
                tableElements.append(rowElement)
                if narrativeToo:
                    narratives.append(rowElement)
        if self._forecastType in ["Narrative Only", "Tabular/Narrative"]:
            for elementId in periodElements:
                for element, default, phrases, searchStrings in self._weInfoList():
                    if elementId == element:
                        if self._forecastType == "Tabular/Narrative":
                            if elementId in tableElements and elementId not in narratives:
                                break
                        if not isinstance(phrases, list):
                            phrases = [phrases]
                        phraseList += phrases
        if self._forecastType in ["Tabular/Narrative", "Tabular Only"]:
            phraseList.append(self._fwsTable_phrase)
        return phraseList

    def getFirePeriod_intersectAreas(self, periodNum):
        self.debug_print("Debug: getFirePeriod_intersectAreas in FWS.py")
        return []

    def _tempLocalEffects_list(self):
        self.debug_print("Debug: _tempLocalEffects in FWS.py")
        leArea1 = self.LocalEffectArea("BelowElev", "")
        leArea2 = self.LocalEffectArea("AboveElev", " above timberline")
        return [self.LocalEffect([leArea1, leArea2], 8, ", except ")]

    def _rhLocalEffects_list(self):
        self.debug_print("Debug: _rhLocalEffects in FWS.py")
        leArea1 = self.LocalEffectArea("BelowElev", "")
        leArea2 = self.LocalEffectArea("AboveElev", " above timberline")
        return [self.LocalEffect([leArea1, leArea2], self._rhTrigger, ", except ")]

    def _tempTrendLocalEffects_list(self):
        self.debug_print("Debug: _tempTrendLocalEffects in FWS.py")
        leArea1 = self.LocalEffectArea("BelowElev", "")
        leArea2 = self.LocalEffectArea("AboveElev", "above timberline")
        return [self.LocalEffect([leArea1, leArea2], self._trendTrigger, ", except ")]

    def _rhTrendLocalEffects_list(self):
        self.debug_print("Debug: _rhTrendLocalEffects in FWS.py")
        leArea1 = self.LocalEffectArea("BelowElev", "")
        leArea2 = self.LocalEffectArea("AboveElev", "above timberline")
        return [self.LocalEffect([leArea1, leArea2], self._trendTrigger, ", except ")]

    def _rhTrigger(self, tree, node, localEffect, leArea1Label, leArea2Label):
        self.debug_print("Debug: _rhTrigger in FWS.py")
        first = node.getAncestor("firstElement")
        element = first.name
        dataType = first.dataType
        timeRange = node.getTimeRange()
        mergeMethod = "MinMax"
        if element == "RH":
            day = self.getPeriod(timeRange, 1)
            if day:
                mergeMethod = "Min"
            else:
                mergeMethod = "Max"

        area1Stats = tree.stats.get(element, timeRange, leArea1Label,
                                    mergeMethod=mergeMethod)
        area2Stats = tree.stats.get(element, timeRange, leArea2Label,
                                    mergeMethod=mergeMethod)
        if area1Stats is None or area2Stats is None:
            return 0
        flag = self.checkLocalEffectDifference(
            tree, node, dataType, 8, area1Stats, area2Stats,
            leArea1Label, leArea2Label)
        return flag

    def _trendTrigger(self, tree, node, localEffect, leArea1Label, leArea2Label):
        self.debug_print("Debug: _trendTrigger in FWS.py")
        first = node.getAncestor("firstElement")
        element = first.name
        dataType = first.dataType
        timeRange = node.getTimeRange()

        if "T" in element:
            trendElement = "Ttrend"
        else:
            trendElement = "RHtrend"

        # trend stats
        area1AbsDiff, area1RawDiff = self.getTrendStats(
            tree, node, element, timeRange, leArea1Label, trendElement)
        area2AbsDiff, area2RawDiff = self.getTrendStats(
            tree, node, element, timeRange, leArea2Label, trendElement)

        if area1AbsDiff is None or area2AbsDiff is None:
            return 0

        # Use rawDiff because sign is important e.g. warmer vs. cooler
        flag = self.checkLocalEffectDifference(
            tree, node, dataType, 4, area1RawDiff, area2RawDiff,
            leArea1Label, leArea2Label)
        return flag

    def skyPopWx_excludePoP_flag(self, tree, node):
        self.debug_print("Debug: skyPopWx in FWS.py")
        # If set to 1, PoP will not be included in the skyPopWx_phrase
        return 1

    def ExtendedLabel(self):
        self.debug_print("Debug: ExtendedLabel in FWS.py")
        return {
            "type": "component",
            "methodList": [self.setLabel],
            "analysisList": [],
            "phraseList": [],
        }

    def setLabel(self, tree, component):
        self.debug_print("Debug: setLabel in FWS.py")
        if self._includeExtended:
            if "Include Day 3-5 Extended?" not in self._extendedQuestions:
                component.set("words", ".FORECAST DAYS 6 THROUGH 7...\n")
            else:
                component.set("words", ".FORECAST DAYS 3 THROUGH 7...\n")
        else:
            component.set("words", ".FORECAST DAYS 3 THROUGH 5...\n")
        return self.DONE()

    def FireExtendedShortTerm(self):
        self.debug_print("Debug: FireExtendedShortTerm in FWS.py")
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
                ("PoP", self._PoP_analysisMethod(
                    "FireExtendedShortTerm"), [6]),
                ("PoP", self.binnedPercent, [6]),
            ],
            "phraseList": [
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
        self.debug_print("Debug: FireExtended in FWS.py")
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
            "phraseList": [
                self.reportTrends,
                self.sky_phrase,
                self.skyPopWx_phrase,
                self.weather_phrase,
                self.lows_phrase,
                self.highs_phrase,
            ],
        }

    def ExtendedNarrative(self):
        self.debug_print("Debug: ExtendedNarrative in FWS.py")
        return {
            "type": "narrative",
            "methodList": [self.assembleChildWords],
            # Components
            "narrativeDef": [
                ("FireExtendedShortTerm", 24), ("FireExtendedShortTerm", 24),
                ("FireExtendedShortTerm", 24),
                ("FireExtended", 24), ("FireExtended", 24)
            ],
        }

    def generateForecast(self, argDict):
        self.debug_print("Debug: generateForecast in FWS.py")
        # Generate Text Phrases for a list of edit areas

        # Get variables
        error = self._getVariables(argDict)
        if error is not None:
            return error

        # Quality Control Gui data
        error = self._qualityControlFormData()
        if error is not None:
            return error

        # Get the areaList -- derived from the lat, lon, size of incident (acres),
        # and the name of the incident.
        error = self._determineAreaList(argDict)
        if error is not None:
            return error

        # Set the extended forecast configuration based on what was
        # requested by the user.
        error = self._setExtendedConfig()
        if error is not None:
            return error

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

        # Generate the unrepresentative statement for the Product
        fcst = self._makeUnrepresentStatement(fcst, argDict)

        # Generate the Contact statement for the Product
        fcst = self._makeContactStatement(fcst, argDict)

        # Generate the Headlines for the Product
        for editArea, areaLabel in self._areaList:
            fcst = self._makeHeadline(fcst, editArea, areaLabel, argDict)

        # Generate the Discussion section
        fcst = self._makeDiscussion(fcst, argDict)

        # Generate the product for each edit area in the list
        fraction = 0
        fractionOne = 1.0 / float(len(self._areaList))
        percent = 50.0
        self.setProgressPercentage(percent)
        for editArea, areaLabel in self._areaList:
            self.progressMessage(
                fraction, percent, "Making Product for " + areaLabel)
            fcst = self._preProcessArea(fcst, editArea, areaLabel, argDict)
            fcst = self._makeProduct(fcst, editArea, areaLabel, argDict)
            fcst = self._postProcessArea(fcst, editArea, areaLabel, argDict)
            fraction = fractionOne

        # Generate the summary extended section (if wanted)
        fcst = self._makeSummaryExtended(fcst, argDict)

        # Generate the 8 to 14 Day Outlook section
        error = self._generateOutlookLabels(argDict)
        if error is not None:
            return error
        fcst = self._make8to14DayOutlook(fcst, argDict)

        fcst = self._postProcessProduct(fcst, argDict)

        return fcst

    def _getVariables(self, argDict):
        self.debug_print("Debug: _getVariables in FWS.py")
        # Make argDict accessible
        self.__argDict = argDict

        self._period1TableRes = "None"
        self._period2TableRes = "None"
        self._period3TableRes = "None"
        self._period4TableRes = "None"
        self._period1Elements = []
        self._period2Elements = []
        self._period3Elements = []
        self._period4Elements = []

        # Get Definition variables
        self._definition = argDict["forecastDef"]
        for key, value in self._definition.items():
            setattr(self, '_' + key, value)

        # Get VariableList and _issuance_list variables
        varDict = argDict["varDict"]
        for key, value in varDict.items():
            if isinstance(key, tuple):
                label, variable = key
                setattr(self, '_' + variable, value)

        self._language = argDict["language"]

        # Adjust creationTime if user-supplied creation date and time
        if self._forecastStartDate != "":
            argDict["creationTime"] = self._getTime(
                self._forecastStartDate, self._forecastStartTime)
        return None

    def _determineTimeRanges(self, argDict):
        self.debug_print("Debug: _determineTimeRanges in FWS.py")
        # Set up the Narrative Definition and initial Time Range
        self._issuanceInfo = self.getIssuanceInfo(
            self._productIssuance, self._issuance_list(argDict), argDict["creationTime"])

        if self._tableStartTimeMode == "current":
            # Add a "custom" component to sample data from current time
            # to product start time
            ct = self._issuanceInfo.issueTime()
            currentTime = AbsTime.absTimeYMD(ct.year, ct.month, ct.day,
                                             ct.hour)
            productStart = self._issuanceInfo.timeRange().startTime()
            tr = TimeRange.TimeRange(currentTime, productStart)
            if tr.duration() > 0:
                self._issuanceInfo.narrativeDef().append(
                    ("Custom", ("PreFirePeriod1", tr)))

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
            self._extendedStart = self._timeRange.endTime() - 24 * 5 * 3600
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
        self.debug_print("Debug: _sampleData in FWS.py")
        # Sample and analyze the data for the narrative
        self._narrativeProcessor = ForecastNarrative.ForecastNarrative()
        error = self._narrativeProcessor.getNarrativeData(
            argDict, self._definition, self._timeRange, self._areaList, self._issuanceInfo)
        if error is not None:
            return error
        return None

    def _preProcessProduct(self, fcst, argDict):
        self.debug_print("Debug: _preProcessProduct in FWS.py")

        if self._requestingAgency == "Unlisted":
            newFireName = self._incidentName + "..." + self._otherAgencyName
        else:
            newFireName = self._incidentName + "..." + self._requestingAgency
        productLabel = self._productName + " for " + newFireName

        productLabel = self.checkTestMode(argDict, productLabel)

        issuedByString = self.getIssuedByString()

        # Product header
        s = self._wmoID + " " + self._fullStationID + " " + \
            self._ddhhmmTime + "\n" + self._pil + "\n\n"
        fcst = fcst + s.upper()

        s = productLabel + \
            "\nNational Weather Service " + self._wfoCityState + \
            "\n" + issuedByString + self._timeLabel + "\n\n"
        fcst = fcst + s

        # Add time disclaimer
        self._fireTR = None
        if self._withIgnitionTimes == "yes" or self._tableStartTimeMode == "ignitionTime":
            fcst = self._makeFcstTimeStatement(fcst, argDict)
        try:
            timeTup = time.strptime(self._timeLabel, '%I%M %p %Z %a %b %d %Y')
            issueTime = time.mktime(timeTup)
        except:
            issueTime = time.time()
        now = time.time()
        if ((issueTime - now) < -24 * 3600) or ((issueTime - now) > 9 * 24 * 3600):
            message = \
                '''|* The start time for this product is %s.
This is either more than a day in the past or more than 9 days
in the future. *|''' % self._timeLabel
            fcst = '%s\n%s\n\n' % (fcst, message)
        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        self.debug_print("Debug: _preProcessArea in FWS.py")
        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        self.debug_print("Debug: _makeProduct in FWS.py")
        argDict["language"] = self._language
        # Generate Narrative Forecast for Edit Area
        fcst = fcst + self._narrativeProcessor.generateForecast(
            argDict, editArea, areaLabel)
        if self._includeMultipleElementTable == 1:
            fcst = fcst + \
                self.makeMultipleElementTable(
                    areaLabel, self._timeRange, argDict)
        return fcst

    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        self.debug_print("Debug: _postProcessArea in FWS.py")
        if self._individualExtended == 1:
            fcst = fcst + "\n"
        return fcst

    def _postProcessProduct(self, fcst, argDict):
        self.debug_print("Debug: _postProcessProduct in FWS.py")
        fcst = fcst.replace("\n\n\n", "\n")
        forecasterString = "/".join(self._forecaster)
        if self._wfoID == "":
            self._wfoID = self._pil[3:]
        if self._webSiteTag == "" or self._webSiteTag == "YYCCCCC.U":
            tagLineString = "|* .TAG YYCCCCC.U/" + self._wfoID + " *|\n"
        else:
            tagLineString = ".TAG " + self._webSiteTag + \
                "/" + self._wfoID + "\n"
        if self._forecastDeliverDate == "" or self._forecastDeliverDate == "MM/DD/YY":
            deldtString = "|* .DELDT MM/DD/YY *|\n"
        else:
            deldtString = ".DELDT " + self._forecastDeliverDate + "\n"
        versionString = ".FormatterVersion " + self._version + "\n"
        if self._emailSend == "" or not self._includeEmails:
            emailString = ""
        else:
            emailString = ".EMAIL " + self._emailSend + "\n"
        fcst = fcst + "$$\nForecaster..." + forecasterString + "\n" + \
            "Requested by..." + self._agencyContact + "\n" + \
            "Type of request..." + self._incidentType + "\n" + \
            tagLineString + deldtString + versionString + emailString
        #self.storeAWIPS(fcst, self._awipsProductID)
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst

    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################

    def _addHeadlines(self, headlines):
        self.debug_print("Debug: _addHeadlines in FWS.py")
        # Add the headlines to the list of product headlines
        headlines = headlines.split("...")
        for headline in headlines:
            if len(headline) == 0 or headline[0] == '\n':
                continue
            if headline not in self._prodHeadlines:
                self._prodHeadlines.append(headline)

    def _issuance_list(self, argDict):
        self.debug_print("Debug: _issuance_list in FWS.py")
        narrativeDef = []
        if self._tabularAllPeriods == "yes":
            phantom = "Phantom"
        else:
            # If we are generating a 12-hour table
            # in the first period, need to have an empty
            # narrative so that the sampling will get done.
            phantom = "EmptyFirePeriod"
        if self._productIssuance in ["Morning", "Morning Update", "Afternoon Update"]:
            # Add the first period
            if len(self._period1Elements) == 0:
                period = (phantom, "period1")
            else:
                period = ("FirePeriod1", "period1")
            narrativeDef.append(period)

            if len(self._period2Elements) == 0:
                period = (phantom, 12)
            else:
                period = ("FirePeriod2", 12)
            narrativeDef.append(period)

            # Add the third period
            if len(self._period3Elements) == 0:
                period = (phantom, 12)
            else:
                period = ("FirePeriod3", 12)
            narrativeDef.append(period)
        else:
            # Add the first period.
            if len(self._period1Elements) == 0:
                period = (phantom, "period1")
            else:
                period = ("FirePeriod1", "period1")
            narrativeDef.append(period)

            # Add the second period
            if len(self._period2Elements) == 0:
                period = (phantom, 12)
            else:
                period = ("FirePeriod2", 12)
            narrativeDef.append(period)

            # Add the third period
            if len(self._period3Elements) == 0:
                period = (phantom, 12)
            else:
                period = ("FirePeriod3", 12)
            narrativeDef.append(period)

            # Add the fourth period
            if len(self._period4Elements) == 0:
                period = (phantom, 12)
            else:
                period = ("FirePeriod4", 12)
            narrativeDef.append(period)

        # Add extended if configured to appear
        if "Include Day 3-5 Extended?" in self._extendedQuestions:
            if self._productIssuance in ["Morning", "Morning Update", "Afternoon Update"]:
                extendedShortTerm = [
                    ("FireExtendedShortTerm", 24),
                    ("FireExtendedShortTerm", 24),
                    ("FireExtendedShortTerm", 24),
                ]
            else:
                extendedShortTerm = [
                    ("FireExtendedShortTerm", 24),
                    ("FireExtendedShortTerm", 24),
                    ("FireExtendedShortTerm", 24),
                ]
        else:
            if self._productIssuance in ["Morning", "Morning Update", "Afternoon Update"]:
                extendedShortTerm = [
                    ("Phantom", 24),
                    ("Phantom", 24),
                    ("Phantom", 24),
                ]
            else:
                extendedShortTerm = [
                    ("Phantom", 24),
                    ("Phantom", 24),
                    ("Phantom", 24),
                ]
        if "Include Day 6-7 Extended?" in self._extendedQuestions:
            extended = [
                ("FireExtended", 24),
                ("FireExtended", 24),
            ]
        else:
            extended = []

        # Combine sections
        try:
            if self._individualExtended == 1:
                if self._extendedLabel == 1:
                    narrativeDef.append(("ExtendedLabel", 0))
                if self._includeExtendedShortTerm or self._includeExtended:
                    narrativeDef = narrativeDef + extendedShortTerm
                if self._includeExtended:
                    narrativeDef = narrativeDef + extended
        except:
            pass
        return [
            ("Morning", self.DAY(), self.NIGHT(), self.NIGHT(),
             ".TODAY...", "early in the morning", "late in the afternoon",
             1, narrativeDef),
            ("Morning Update", "issuanceHour", self.NIGHT(), self.NIGHT(),
             ".REST OF TODAY...", "early in the morning", "late in the afternoon",
             1, narrativeDef),
            ("Afternoon Update", "issuanceHour", self.NIGHT(), self.NIGHT(),
             ".REST OF TODAY...", "early in the morning", "late in the afternoon",
             1, narrativeDef),
            #  End times are tomorrow:
            ("Afternoon", self.NIGHT(), 24 + self.DAY(), 24 + self.DAY(),
             ".TONIGHT...", "late in the night", "early in the evening",
             1, narrativeDef),
            ("Evening Update", "issuanceHour", 24 + self.DAY(), 24 + self.DAY(),
             ".REST OF TONIGHT...", "late in the night", "early in the evening",
             1, narrativeDef),
            # For the early morning update, this produces:
            # Rest of Tonight:
            # Monday
            # Monday Night
            ("Early Morning Update", "issuanceHour", self.DAY(), self.DAY(),
             ".REST OF TONIGHT...", "early in the morning", "late in the afternoon",
             1, narrativeDef),
        ]

    def lateDay_descriptor(self, statDict, argDict, timeRange):
        self.debug_print("Debug: lateDay_descriptor in FWS.py")
        # If time range is in the first period, return period1 descriptor for
        #  late day -- default 3pm-6pm
        if self._issuanceInfo.period1TimeRange().contains(timeRange):
            return self._issuanceInfo.period1LateDayPhrase()
        else:
            return "late in the afternoon"

    def lateNight_descriptor(self, statDict, argDict, timeRange):
        self.debug_print("Debug: lateNight_descriptor in FWS.py")
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
            ('HW.W', allActions, 'Wind'),  # HIGH WIND WARNING
            ('WI.Y', allActions, 'Wind'),  # WIND ADVISORY
            ('HW.A', allActions, 'Wind'),  # HIGH WIND WATCH
            ('EH.W', allActions, 'Heat'),  # EXCESSIVE HEAT WARNING
            ('HT.Y', allActions, 'Heat'),  # HEAT ADVISORY
        ]

    def DAY(self):
        return 6

    def NIGHT(self):
        return 18

    def _processVariableList(self, definition):
        self.debug_print("Debug: _processVariableList in FWS.py")

        # Get Definition variables
        for key, value in definition.items():
            setattr(self, '_' + key, value)

        # Load in a user specified number of STQ products into the formatter.
        products = self._getStqProducts()

        # Get the information for the specific incident.
        #  IF there are STQ products in the directory,
        #    selection GUI will be displayed
        cancel = self._getFireInfo(products)
        if cancel:
            # User cancelled
            return None

        # Get the user information for the specific incident
        # and return the resulting varDict
        varDict = self._displayFireInfo()
        # Set the Product Issuance
        if not self._forceIssuance:
            forecastStartTime = varDict[
                ('Forecast Start Time ................................', 'forecastStartTime')]
            if forecastStartTime == "0600":
                issuance = "Morning"
            elif forecastStartTime in ["0700", "0800", "0900", "1000", "1100"]:
                issuance = "Morning Update"
            elif forecastStartTime in ["1200", "1300", "1400", "1500", "1600", "1700"]:
                issuance = "Afternoon Update"
            elif forecastStartTime == "1800":
                issuance = "Afternoon"
            elif forecastStartTime in ["1900", "2000", "2100", "2200", "2300"]:
                issuance = "Evening Update"
            elif forecastStartTime in ["0000", "0100", "0200", "0300", "0400", "0500"]:
                issuance = "Early Morning Update"
            else:
                issuance = "Morning Update"
            varDict[('Product Issuance:', 'productIssuance')] = issuance
        return varDict

    def _getStqProducts(self):
        self.debug_print("Debug: _getStqProducts in FWS.py")
        # Load in a user specified number of STQ products into the formatter.
        # If no products found, return empty list
        products = []
        deletedIds = []
        version = 0
        stqPil = self._textdbPil[0:3] + self._stqPil
        searchString = ""
        for version in range(self._stqNumberVersions):
            product = self.getPreviousProduct(
                stqPil, searchString, version=version)
            if product is None or product == "":
                break

            # Let's filter the product just in case single quote is put into
            # the request.
            product = product.replace("\'", "")

            # Clean out single quote html code
            product = product.replace("&#39;", "")

            # Put the lines of the STQ product into an Array
            product = product.split("\n")

            # loop through lines of STQ to find needed information and set
            # flags
            missingFlag = 1
            feedbackFlag = 0
            deleteFlag = 0
            tagFlag = 1
            for line in product:
                line = line.replace("\n", "")
                if "PROJECT NAME" in line:
                    missingFlag = 0
                if "Feedback was just received for project" in line:
                    feedbackFlag = 1
                if "REQUEST TYPE:  DELETED" in line:
                    deleteFlag = 1
                if "OFILE:" in line:
                    tag = line[7:].upper()
                    tagTest = tag[7:8]
                    incidentId = tag[:7]
                    if tagTest != '.':
                        tagFlag = 0

            # incidentId found test
            try:
                self.debug_print(incidentId)
            except:
                continue

            # added incident id to list if deleted
            if tagFlag and deleteFlag and incidentId not in deletedIds:
                deletedIds.append(incidentId)

            # Do not let incidentId in deleted list to be appended to products
            if tagFlag and incidentId in deletedIds:
                missingFlag = 1

            # Do not let old format STQs to be appeded to products
            if not tagFlag:
                missingFlag = 1

            # Only append STQ products from new website and not deleted.
            if not missingFlag and not feedbackFlag:
                products.append(product)
        return products

    def _getFireInfo(self, products):
        self.debug_print("Debug: _getFireInfo in FWS.py")
        # If there were STQ products, display their names for user to select
        # Return 1 if user cancels
        product, issuance, forecasters = self._getFireProduct(products)
        if issuance is None:
            return 1  # User cancelled
        if len(products) > 0:
            self._noStqProduct = 0
        else:
            product = None
            self._noStqProduct = 1
        self.debug_print(product)
        self._getProductInfo(product, issuance, forecasters)

    def _getFireProduct(self, products):
        self.debug_print("Debug: _getFireProduct in FWS.py")
        # Create the incidentNameList used for the spot selection menu.
        incidentNameList = []
        ofileList = []
        validProductFound = 0
        productNumber = 0
        masterProductList = []
        masterIncidentIdList = []
        for product in products:
            self.debug_print(product)
            incidentName = "NAME MISSING"
            timeStamp = "DDHHMM"
            tag = "YYNNNNN.UUUU"
            tagFlag = 0
            feedbackFlag = 0
            deleteFlag = 0
            for line in product:
                line = line.replace("\n", "")
                if "PROJECT NAME" in line:
                    incidentName = line[22:]
                if self._stqWmoID in line:
                    timeStamp = line[12:]
                if "OFILE" in line:
                    tag = line[7:].upper()
                    tagTest = tag[7:8]
                    incidentId = tag[:7]
                    if tag not in ofileList and tagTest == '.' and incidentId not in masterIncidentIdList:
                        ofileList.append(tag)
                        masterIncidentIdList.append(incidentId)
                        tagFlag = 1
                        productNumber = productNumber + 1
            if tagFlag:
                incidentNameList.append(str(productNumber) + ") " + incidentName +
                                        " -- " + timeStamp + " -- " + tag)
                masterProductList.append(product)
                validProductFound = 1

        varList = []

        incidentNameList.append("Manually Enter in Request - WILDFIRE")
        incidentNameList.append("Manually Enter in Request - PRESCRIBED")
        incidentNameList.append("Manually Enter in Request - MARINE")
        incidentNameList.append("Manually Enter in Request - HAZMAT Land")
        incidentNameList.append(
            "Manually Enter in Request - HAZMAT Inland Waterway")
        incidentNameList.append("Manually Enter in Request - SAR Land")
        incidentNameList.append("Manually Enter in Request - SAR Water")
        incidentNameList.append("Manually Enter in Request - OTHER")
        desFireName = "Please Choose a Fire", "incidentName"
        varList.append(
            (desFireName, incidentNameList[0], "radio", incidentNameList))

        # Product Issuance Processing
        issuanceList = [
            "Auto", "Morning", "Morning Update", "Afternoon Update",
            "Afternoon", "Evening Update", "Early Morning Update"
        ]
        desIssuanceList = "Product Issuance:", "productIssuance"
        varList.append(
            (desIssuanceList, issuanceList[0], "radio", issuanceList))

        # Forecaster List Section of the GUI
        forecasterNameList = []
        defaultForecasterNameList = []
        awipsLogin = pwd.getpwuid(os.getuid()).pw_name
        for forecaster in self._forecasterList:
            id, awipsName, name = forecaster
            forecasterNameList.append(name)
            if awipsLogin == awipsName:
                defaultForecasterNameList.append(name)
        desForecasterNameList = "Forecaster:", "forecaster"
        varList.append(
            (desForecasterNameList, defaultForecasterNameList, "check", forecasterNameList))

        # Launch the Spot Request selection GUI.
        varDict = self._callProcessVariableList(
            "Select Spot Request", varList, varDict={})
        if varDict is None:
            return None, None, None

        productIssuance = varDict[desIssuanceList]
        self._productIssuance = productIssuance
        if productIssuance == "Auto":
            self._forceIssuance = 0
        else:
            self._forceIssuance = 1
            self._productIssuance = productIssuance
        forecasters = varDict[desForecasterNameList]

        if "Manually Enter in Request" in varDict[desFireName]:
            validProductFound = 0

        if validProductFound:
            self.debug_print("Product Found")
            stqIndex = incidentNameList.index(varDict[desFireName])
            return masterProductList[stqIndex], productIssuance, forecasters
        else:
            self.debug_print("varDict")
            self.debug_print(varDict[desFireName])
            if varDict[desFireName] == "Manually Enter in Request - WILDFIRE":
                return "manual - WILDFIRE", productIssuance, forecasters
            elif varDict[desFireName] == "Manually Enter in Request - PRESCRIBED":
                return "manual - PRESCRIBED", productIssuance, forecasters
            elif varDict[desFireName] == "Manually Enter in Request - MARINE":
                return "manual - MARINE", productIssuance, forecasters
            elif varDict[desFireName] == "Manually Enter in Request - HAZMAT Land":
                return "manual - HAZMAT Land", productIssuance, forecasters
            elif varDict[desFireName] == "Manually Enter in Request - HAZMAT Inland Waterway":
                return "manual - HAZMAT Inland Waterway", productIssuance, forecasters
            elif varDict[desFireName] == "Manually Enter in Request - SAR Land":
                return "manual - SAR Land", productIssuance, forecasters
            elif varDict[desFireName] == "Manually Enter in Request - SAR Water":
                return "manual - SAR Water", productIssuance, forecasters
            elif varDict[desFireName] == "Manually Enter in Request - OTHER":
                return "manual - OTHER", productIssuance, forecasters
            else:
                return None, productIssuance, forecasters

    def _callProcessVariableList(self, title, varList, varDict):
        self.debug_print("Debug: _getFireProduct in FWS.py")
        processVarList = ProcessVariableList.ProcessVariableList(
            title, varList, varDict={})
        self._selectionStatus = processVarList.status()
        if not self._selectionStatus == "OK":
            return None  # User Cancelled
        return processVarList.varDict()

    def _weInfoList(self):
        self.debug_print("Debug: _weInfoList in FWS.py")
        # This is the list of possible weather parameters listed under the
        # ...WEATHER PARAMETERS REQUESTED... section in your STQ Product.
        # These are listed in the order they will appear in the product.
        #
        # Weather Elements: If you have a weather element to add,
        # then send an email to Virgil.Middendorf@noaa.gov with your addition.
        # I will baseline it.
        #
        # Phrases: You can override this method and edit the phrase method if you
        # don't like the one used in baseline.

        # For each element, we list:
        #     --an identifier
        #     --flag to indicate if this is a default element
        #     --the FWF phrase (or list of phrases) to include in the product
        #     --a list of search strings that must appear in
        #       the STQ product to specify the element.
        #       Each search string in the list may be a tuple in which case any of
        #       the entries in the tuple will satsify the search.

        if self._useRH:
            dayRH = "RH"
            nightRH = "RH"
        else:
            dayRH = "MinRH"
            nightRH = "MaxRH"
        if self._wind20ftHeader:
            wind = [self.fireWind_label_phrase, self.fireWind_compoundPhrase]
        else:
            wind = [self.fireWind_compoundPhrase]
        return [

            # Weather Related Phrases
            ("SKY/WEATHER", 1, self.skyWeather_byTimeRange_compoundPhrase,
             [("SKY", "CLOUDS"), "WEATHER"]),
            ("CWR", 0, self.cwr_phrase,
             [("CWR", "WETTING RAIN")]),
            ("POP", 0, self.pop_phrase,
             [("CHANCE OF PRECIPITATION", "CHANCE OF PCPN", "POP")]),
            ("CHANCE OF RAIN", 0, self.chanceOfRain_phrase,
             ["CHANCE OF RAIN"]),
            ("CHANCE OF THUNDER", 0, self.chanceOfThunder_phrase,
             ["CHANCE OF THUNDER"]),
            ("CHANCE OF LIGHTNING", 0, self.chanceOfLightning_phrase,
             ["CHANCE OF LIGHTNING"]),
            ("LIGHTNING ACTIVITY LEVEL", 0, self.lal_phrase,
             [("LAL", "LIGHTNING")]),
            ("BEGIN/END OF PCPN", 0, self.pcpnTiming_phrase,
             ["BEGIN", "END", "PRECIPITATION"]),

            ("TEMPERATURE", 1, (self.dayOrNight_phrase, ["MaxT", "MinT", 1, 1]),
             ["TEMPERATURE"]),
            ("HUMIDITY", 1, (self.dayOrNight_phrase, [dayRH, nightRH, 1, 1]),
             [("RH", "HUMIDITY")]),
            ("DEWPOINT", 0, self.td_phrase,
             ["DEWPOINT"]),
            ("HEAT INDEX", 0, self.heatIndex_phrase,
             ["HEAT", "INDEX"]),
            ("WIND CHILL", 0, self.windChill_phrase,
             ["WIND", "CHILL"]),
            ("APPARENT TEMPERATURE", 0, self.apparentTemperature_phrase,
             ["APPARENT TEMPERATURE"]),
            ("EYE LEVEL WINDS", 0, self.fireEyeWind_compoundPhrase,
             ["EYE", "WIND"]),
            ("SURFACE WINDS", 0, self.sfcWind_compoundPhrase,
             ["SURFACE", "WIND"]),
            ("SURFACE WINDS KTS", 0, self.sfcKtsWind_compoundPhrase,
             ["SURFACE", "WIND", "(KTS)"]),
            ("WIND SHIFT", 0, self.fireWindShift_label_phrase,
             ["WIND", "SHIFT"]),

            # Very Important that the next 5 wind entries remain together and
            # remain in same order.
            ("SLOPE/VALLEY WINDS", 0, wind,
             ["SLOPE/VALLEY WINDS"]),
            ("VALLEY/LOWER SLOPE WINDS", 0, wind,
             ["VALLEY/LOWER SLOPE WINDS"]),
            ("20 FOOT WINDS", 0, wind,
             ["WIND (20 FT)"]),
            ("RIDGE/UPPER SLOPE WINDS", 0, self.upperSlopeWind_phrase,
             ["RIDGE/UPPER SLOPE WINDS"]),
            ("RIDGE TOP WIND", 0, self.freeWind_phrase,
             ["RIDGE TOP WINDS"]),

            ("SURROUNDING RIDGE", 0, self.surroundingRidgeWind_phrase,
             ["SURROUNDING", "RIDGE", "WIND"]),
            ("SMOKE DISPERSION", 1, [self.mixingHgt_phrase, self.transportWind_phrase],
             ["SMOKE", "DISPERSION"]),
            ("MIXING HEIGHT", 0, self.mixingHgt_phrase,
             ["MIXING HEIGHT"]),
            ("MIXING HEIGHT MSL", 0, self.mixingHgtMsl_phrase,
             ["MIXING HEIGHT (MSL)"]),
            ("MIXING HEIGHT METRIC", 0, self.mixingHgtMetric_phrase,
             ["MIXING HEIGHT (KM OR M)"]),
            ("TRANSPORT WINDS", 0, self.transportWind_phrase,
             ["TRANSPORT WINDS"]),
            ("TRANSPORT WINDS METRIC", 0, self.transportWindMetric_phrase,
             ["TRANSPORT WINDS (M/S)"]),
            ("MIXING WINDS", 0, self.mixingWind_phrase,
             ["MIXING WINDS"]),
            ("MIXING WINDS METRIC", 0, self.mixingWindMetric_phrase,
             ["MIXING WINDS (M/S)"]),
            ("100 FT WIND", 0, self.wind100ft_phrase,
             ["100 FT WINDS"]),
            ("1000 FT WIND", 0, self.wind1000ft_phrase,
             ["1000 FT WIND"]),
            ("2000 FT WIND", 0, self.wind2000ft_phrase,
             ["2000 FT WIND"]),
            ("3000 FT WIND", 0, self.wind3000ft_phrase,
             ["3000 FT WIND"]),
            ("4000 FT WIND", 0, self.wind4000ft_phrase,
             ["4000 FT WIND"]),
            ("5000 FT WIND", 0, self.wind5000ft_phrase,
             ["5000 FT WIND"]),
            ("SMOKE DISPERSAL", 1, self.smokeDispersal_phrase,
             ["SMOKE", "DISPERSAL"]),
            ("CLEARING INDEX", 0, self.smokeDispersal_phrase,
             ["CLEARING", "INDEX"]),
            ("VENTILATION RATE", 0, self.smokeDispersal_phrase,
             ["VENTILATION", "RATE"]),
            ("LDSI", 0, self.ldsi_phrase,
             ["LDSI"]),
            ("LVORI", 0, self.lvori_phrase,
             ["LVORI"]),
            ("ADI", 0, self.adi_phrase,
             ["ADI"]),
            ("GFDI", 0, self.gfdi_phrase,
             ["GFDI (GRASS FIRE DANGER INDEX)"]),
            ("DISPERSION INDEX", 0, self.dsi_phrase,
             ["DISPERSION", "INDEX"]),
            ("STABILITY CLASS", 0, self.stabilityClass_phrase,
             ["STABILITY"]),
            ("MARINE LAYER", 0, self.marineLayer_phrase,
             ["MARINE", "LAYER"]),
            ("HAINES INDEX", 0, self.haines_phrase,
             ["HAINES", "INDEX"]),
            ("MID LEVEL HAINES INDEX", 0, self.hainesMid_phrase,
             ["HAINES INDEX (MID-LEVEL)"]),
            ("SWELL HEIGHT", 0, self.swell_phrase,
             ["SWELL", "HEIGHT"]),
            ("WAVE HEIGHT", 0, self.waveHeight_phrase,
             ["WAVE", "HEIGHT"]),
            ("HIONETENTH", 0, self.hiOneTenth_phrase,
             ["HIONETENTH"]),
            ("SWELL PERIOD", 0, self.period_phrase,
             ["SWELL", "PERIOD"]),
            ("WAVE PERIOD", 0, self.wavePeriod_phrase,
             ["WAVE", "PERIOD"]),
            ("WIND WAVE", 0, self.windWave_phrase,
             ["WIND", "WAVE"]),
            ("FREEZING SPRAY", 0, self.freezingSpray_phrase,
             ["FREEZING SPRAY"]),
            ("SEA ICE CONCENTRATION", 0, self.seaIceConcentration_phrase,
             ["SEA ICE CONCENTRATION"]),
            ("SEA SURFACE TEMPERATURE", 0, self.seaSurfaceTemperature_phrase,
             ["SEA SURFACE TEMPERATURE"]),

            ("RAINFALL AMOUNT", 0, self.qpf_phrase,
             ["RAINFALL", "AMOUNT"]),
            ("PRECIPITATION AMOUNT", 0, self.precipitationAmount_phrase,
             ["PRECIPITATION AMOUNT"]),
            ("SNOWFALL AMOUNT", 0, self.snow_phrase,
             ["SNOWFALL", "AMOUNT"]),
            ("FREEZING LEVEL", 0, self.freezingLevel_phrase,
             ["FREEZING", "LEVEL"]),
            ("SNOW LEVEL", 0, self.snowLevel_phrase,
             ["SNOW LEVEL"]),
            ("CEILING", 0, self.ceiling_phrase,
             ["CEILING"]),
            ("VISIBILITY", 0, self.visibility_phrase,
             ["VISIBILITY"]),
            ("PRESSURE", 0, self.pressure_phrase,
             ["PRESSURE (IN)"]),
            ("ICING", 0, self.icing_phrase,
             ["ICING"]),
            ("RIVER LEVEL", 0, self.riverLevel_phrase,
             ["RIVER LEVEL"]),
            ("RIVER TEMPERATURE", 0, self.riverTemperature_phrase,
             ["RIVER TEMPERATURE"]),
            ("WATER TEMPERATURE", 0, self.waterTemperature_phrase,
             ["WATER TEMPERATURE"]),
            ("HAZARDS", 0, self.ceiling_phrase,
             ["HAZARDS"]),
            ("SUNRISE/SUNSET", 0, self.sunriseSunset_label_phrase,
             ["SUNRISE", "SUNSET"]),
            ("MOONLIGHT", 0, self.moonlight_label_phrase,
             ["MOONLIGHT"]),
            ("INVERSION SETUP/BURNOFF", 0, self.inversionSetupBurnoff_label_phrase,
             ["INVERSION", "SETUP", "BURNOFF"]),
            ("TIDES", 0, self.tides_label_phrase,
             ["TIDES"]),
        ]

    def _weInfoHiddenList(self):
        self.debug_print("Debug: _weInfoHiddenList in Spot_Overrides.py")
        # This is the list of possible weather parameters that are NOT listed
        # under the ...WEATHER PARAMETERS REQUESTED... section in your STQ
        # Product. There are times when a WFO will want to format certain
        # weather elements in the spot forecast, but do NOT want those elements
        # to be listed in the website.

        # These elements will be appended below the "weather parameters requested"
        # elements and will be in the order as specified in _weInfoHiddenList
        #
        # All weather elements will be commented out and they are the same
        # weather elements listed in _weInfoList. If you have a weather element
        # to add, then send an email to Virgil.Middendorf@noaa.gov for baselining.
        #
        # Phrases: Phrases associated with each element listed in this method
        # is still configured in the _weInfoList method

        # For each element, we list:
        #     --an identifier
        #     --flag to indicate if this is a default element

        return [
##            ("SKY/WEATHER",              0),
##            ("BEGIN/END OF PCPN",        0),
##            ("TEMPERATURE",              0),
##            ("HUMIDITY",                 0),
##            ("DEWPOINT",                 0),
##            ("20 FOOT WINDS",            0),
##            ("EYE LEVEL WINDS",          0),
##            ("SURFACE WINDS",            0),
##            ("SURFACE WINDS KTS",        0),
##            ("WIND SHIFT",               0),
##            ("VALLEY/LOWER SLOPE WINDS", 0),
##            ("SLOPE/VALLEY WINDS",       0),
##            ("RIDGE TOP WIND",           0),
##            ("RIDGE/UPPER SLOPE WINDS",  0),
##            ("SURROUNDING RIDGE",        0),
##            ("CWR",                      0),
##            ("POP",                      0),
##            ("CHANCE OF RAIN",           0),
##            ("CHANCE OF THUNDER",        0),
##            ("CHANCE OF LIGHTNING",      0),
##            ("LIGHTNING ACTIVITY LEVEL", 0),
##            ("SMOKE DISPERSION",         0),
##            ("MIXING HEIGHT",            0),
##            ("MIXING HEIGHT MSL",        0),
##            ("MIXING HEIGHT METRIC",     0),
##            ("TRANSPORT WINDS",          0),
##            ("TRANSPORT WINDS METRIC",   0),
##            ("MIXING WINDS",             0),
##            ("MIXING WINDS METRIC",      0),
##            ("100 FT WINDS",             0),
##            ("1000 FT WINDS",            0),
##            ("2000 FT WINDS",            0),
##            ("3000 FT WINDS",            0),
##            ("4000 FT WINDS",            0),
##            ("5000 FT WINDS",            0),
##            ("SMOKE DISPERSAL",          0),
##            ("CLEARING INDEX",           0),
##            ("VENTILATION RATE",         0),
##            ("DISPERSION INDEX",         0),
##            ("LDSI",                     0),
##            ("LVORI",                    0),
##            ("ADI",                      0),
##            ("GFDI",                     0),
##            ("STABILITY CLASS",          0),

##            ("MARINE LAYER",             0),
##            ("HAINES INDEX",             0),
##            ("HAINES INDEX MID LEVEL",   0),
##            ("SWELL HEIGHT",             0),
##            ("WAVE HEIGHT",              0),
##            ("HIONETENTH",               0),
##            ("SWELL PERIOD",             0),
##            ("WAVE PERIOD",              0),
##            ("WIND WAVE",                0),
##            ("FREEZING SPRAY",           0),
##            ("SEA ICE CONCENTRATION",    0),
##            ("SEA SURFACE TEMPERATURE",  0),
##            ("RAINFALL AMOUNT",          0),
##            ("PRECIPITATION AMOUNT",     0),
##            ("SNOWFALL AMOUNT",          0),
##            ("FREEZING LEVEL",           0),
##            ("SNOW LEVEL",               0),
##            ("CEILING",                  0),
##            ("VISIBILITY",               0),
##            ("PRESSURE",                 0),
##            ("ICING",                    0),
##            ("RIVER LEVEL",              0),
##            ("RIVER TEMPERATURE",        0),
##            ("WATER TEMPERATURE",        0),
##            ("HAZARDS",                  0),
##            ("HEAT INDEX",               0),
##            ("WIND CHILL",               0),
##            ("APPARENT TEMPERATURE",     0),
##            ("SUNRISE/SUNSET",           0),
##            ("MOONLIGHT",                0),
##            ("INVERSION SETUP/BURNOFF",  0),
##            ("TIDES",                    0),
        ]

    def _rowList(self, colWidth=1):
        self.debug_print("Debug: _rowList in FWS.py")

        ### 20 foot wind tabular phrase configuration ###

        if self._tableWindElementSplit == "no" and colWidth == 7:  # 2 hourly
            if self._bothAlphaNumericDict.get(self._20ftWindParm, "No") == "No":
                wind = [("20 ft wind......", self._wind_value),
                        ("20 ft wind gust.", self._windGust_value)]
            else:
                wind = [("20 FT wind......", self._wind_value),
                        ("20 FT wind gust.", self._windGust_value),
                        ("20 FT wind dir..", self._windNumDir_value)]
        # 3-4 hourly
        elif self._tableWindElementSplit == "no" and colWidth > 7:
            if self._bothAlphaNumericDict.get(self._20ftWindParm, "No") == "No":
                wind = [("20 FT wind......", self._windWithGust_value)]
            else:
                wind = [("20 FT wind......", self._windWithGust_value),
                        ("20 FT wind dir..", self._windNumDir_value)]
        else:
            if self._bothAlphaNumericDict.get(self._20ftWindParm, "No") == "No":
                wind = [("20 FT wind dir..", self._windDir_value),  # 1 hourly
                        ("20 FT wind spd..", self._windSpd_value),
                        ("20 FT wind gust.", self._windGust_value)]
            else:
                wind = [("20 FT wind dir..", self._windDir_value),  # 1 hourly
                        ("20 FT wind dir..", self._windNumDir_value),
                        ("20 FT wind spd..", self._windSpd_value),
                        ("20 FT wind gust.", self._windGust_value)]

        ### Slope Valley wind tabular phrase configuration ###

        if self._tableWindElementSplit == "no" and colWidth == 7:  # 2 hourly
            if self._bothAlphaNumericDict.get(self._20ftWindParm, "No") == "No":
                windsv = [("SL/VL wind......", self._wind_value),
                          ("SL/VL wind gust.", self._windGust_value)]
            else:
                windsv = [("SL/VL wind......", self._wind_value),
                          ("SL/VL wind gust.", self._windGust_value),
                          ("SL/VL wind dir..", self._windNumDir_value)]
        # 3-4 hourly
        elif self._tableWindElementSplit == "no" and colWidth > 7:
            if self._bothAlphaNumericDict.get(self._20ftWindParm, "No") == "No":
                windsv = [("SL/VL wind......", self._windWithGust_value)]
            else:
                windsv = [("SL/VL wind......", self._windWithGust_value),
                          ("SL/VL wind dir..", self._windNumDir_value)]
        else:
            if self._bothAlphaNumericDict.get(self._20ftWindParm, "No") == "No":
                windsv = [("SL/VL wind dir..", self._windDir_value),  # 1 hourly
                          ("SL/VL wind spd..", self._windSpd_value),
                          ("SL/VL wind gust.", self._windGust_value)]
            else:
                windsv = [("SL/VL wind dir..", self._windDir_value),  # 1 hourly
                          ("SL/VL wind dir..", self._windNumDir_value),
                          ("SL/VL wind spd..", self._windSpd_value),
                          ("SL/VL wind gust.", self._windGust_value)]

        ### Valley / Lower Slope wind tabular phrase configuration ###

        if self._tableWindElementSplit == "no" and colWidth == 7:  # 2 hourly
            if self._bothAlphaNumericDict.get(self._20ftWindParm, "No") == "No":
                windvl = [("VL/LS wind......", self._wind_value),
                          ("VL/LS wind gust.", self._windGust_value)]
            else:
                windvl = [("VL/LS wind......", self._wind_value),
                          ("VL/LS wind gust.", self._windGust_value),
                          ("VL/LS wind dir..", self._windNumDir_value)]
        # 3-4 hourly
        elif self._tableWindElementSplit == "no" and colWidth > 7:
            if self._bothAlphaNumericDict.get(self._20ftWindParm, "No") == "No":
                windvl = [("VL/LS wind......", self._windWithGust_value)]
            else:
                windvl = [("VL/LS wind......", self._windWithGust_value),
                          ("VL/LS wind dir..", self._windNumDir_value)]
        else:
            if self._bothAlphaNumericDict.get(self._20ftWindParm, "No") == "No":
                windvl = [("VL/LS wind dir..", self._windDir_value),  # 1 hourly
                          ("VL/LS wind spd..", self._windSpd_value),
                          ("VL/LS wind gust.", self._windGust_value)]
            else:
                windvl = [("VL/LS wind dir..", self._windDir_value),  # 1 hourly
                          ("VL/LS wind dir..", self._windNumDir_value),
                          ("VL/LS wind spd..", self._windSpd_value),
                          ("VL/LS wind gust.", self._windGust_value)]

        ### eye level wind tabular phrase configuration ###

        if self._tableEyeWindElementSplit == "no" and colWidth == 7:
            if self._bothAlphaNumericDict.get("EyeWind", "No") == "No":
                eyewind = [("Eye level wind..", self._eyewind_value),
                           ("Eye lvl wnd gst.", self._eyewindGust_value)]
            else:
                eyewind = [("Eye level wind..", self._eyewind_value),
                           ("Eye lvl wnd gst.", self._eyewindGust_value),
                           ("Eye lvl wnd dir.", self._eyewindNumDir_value)]
        elif self._tableEyeWindElementSplit == "no" and colWidth > 7:
            if self._bothAlphaNumericDict.get("EyeWind", "No") == "No":
                eyewind = [("Eye level wind..", self._eyewindWithGust_value)]
            else:
                eyewind = [("Eye level wind..", self._eyewindWithGust_value),
                           ("Eye lvl wnd dir.", self._eyewindNumDir_value)]
        else:
            if self._bothAlphaNumericDict.get("EyeWind", "No") == "No":
                eyewind = [("Eye lvl wnd dir.", self._eyewindDir_value),
                           ("Eye lvl wnd spd.", self._eyewindSpd_value),
                           ("Eye lvl wnd gst.", self._eyewindGust_value)]
            else:
                eyewind = [("Eye lvl wnd dir.", self._eyewindDir_value),
                           ("Eye lvl wnd dir.", self._eyewindNumDir_value),
                           ("Eye lvl wnd spd.", self._eyewindSpd_value),
                           ("Eye lvl wnd gst.", self._eyewindGust_value)]

        ### surface wind (10m) tabular phrase configuration ###

        if self._tableSfcWindElementSplit == "no" and colWidth == 7:

            if self._bothAlphaNumericDict.get("SfcWind", "No") == "No":
                sfcwind = [("Surface wind....", self._sfcwind_value),
                           ("Surface wnd gst.", self._sfcwindGust_value)]
            else:
                sfcwind = [("Surface wind....", self._sfcwind_value),
                           ("Surface wnd gst.", self._sfcwindGust_value),
                           ("Surface wnd dir.", self._sfcwindNumDir_value)]

        elif self._tableSfcWindElementSplit == "no" and colWidth > 7:

            if self._bothAlphaNumericDict.get("SfcWind", "No") == "No":
                sfcwind = [("Surface wind....", self._sfcwindWithGust_value)]
            else:
                sfcwind = [("Surface wind....", self._sfcwindWithGust_value),
                           ("Surface wnd dir.", self._sfcwindNumDir_value)]

        else:

            if self._bothAlphaNumericDict.get("SfcWind", "No") == "No":
                sfcwind = [("Surface wnd dir.", self._sfcwindDir_value),
                           ("Surface wnd spd.", self._sfcwindSpd_value),
                           ("Surface wnd gst.", self._sfcwindGust_value)]
            else:
                sfcwind = [("Surface wnd dir.", self._sfcwindDir_value),
                           ("Surface wnd dir.", self._sfcwindNumDir_value),
                           ("Surface wnd spd.", self._sfcwindSpd_value),
                           ("Surface wnd gst.", self._sfcwindGust_value)]

        ### surface wind in knots (10m) tabular phrase configuration ###

        if self._tableSfcWindElementSplit == "no" and colWidth == 7:

            if self._bothAlphaNumericDict.get("SfcWind", "No") == "No":
                sfcktswind = [("SFC wind (kts)..", self._sfcktswind_value),
                              ("SFC wnd gst ....", self._sfcktswindGust_value)]
            else:
                sfcktswind = [("SFC wind (kts)..", self._sfcktswind_value),
                              ("SFC wnd gst.....", self._sfcktswindGust_value),
                              ("SFC wnd dir.....", self._sfcktswindNumDir_value)]

        elif self._tableSfcWindElementSplit == "no" and colWidth > 7:

            if self._bothAlphaNumericDict.get("SfcWind", "No") == "No":
                sfcktswind = [
                    ("SFC wind (kts)..", self._sfcktswindWithGust_value)]
            else:
                sfcktswind = [("SFC wind (kts)..", self._sfcktswindWithGust_value),
                              ("SFS wnd dir.....", self._sfcktswindNumDir_value)]

        else:

            if self._bothAlphaNumericDict.get("SfcWind", "No") == "No":
                sfcktswind = [("SFC wnd dir.....", self._sfcktswindDir_value),
                              ("SFC wnd spd kts.", self._sfcktswindSpd_value),
                              ("SFC wnd gst.....", self._sfcwindGust_value)]
            else:
                sfcktswind = [("SFC wnd dir.....", self._sfcktswindDir_value),
                              ("SFC wnd dir.....",
                               self._sfcktswindNumDir_value),
                              ("SFC wnd spd kts.", self._sfcktswindSpd_value),
                              ("SFC wnd gst.....", self._sfcktswindGust_value)]

        ### ridge top wind tabular phrase configuration ###

        if self._tableRidgeElementSplit == "no" and colWidth >= 7:
            if self._bothAlphaNumericDict.get("RidgeWind", "No") == "No":
                ridge = [("Ridgetop wind...", self._ridge_value)]

            else:
                ridge = [("Ridgetop wind...", self._ridge_value),
                         ("Ridge wind dir..", self._ridgeNumDir_value)]
        else:
            if self._bothAlphaNumericDict.get("RidgeWind", "No") == "No":
                ridge = [("Ridge wind dir..", self._ridgeDir_value),
                         ("Ridge wind spd..", self._ridgeSpd_value)]
            else:
                ridge = [("Ridge wind dir..", self._ridgeDir_value),
                         ("Ridge wind dir..", self._ridgeNumDir_value),
                         ("Ridge wind spd..", self._ridgeSpd_value)]

        ### ridge/upper slope wind tabular phrase configuration ###

        if self._tableRidgeElementSplit == "no" and colWidth >= 7:
            if self._bothAlphaNumericDict.get("RidgeWind", "No") == "No":
                ridgeus = [("Ridge/upslp spd.", self._ridge_value)]
            else:
                ridgeus = [("Ridge/upslp spd.", self._ridge_value),
                           ("Ridge/upslp dir.", self._ridgeNumDir_value)]
        else:
            if self._bothAlphaNumericDict.get("RidgeWind", "No") == "No":
                ridgeus = [("Ridge/upslp dir.", self._ridgeDir_value),
                           ("Ridge/upslp spd.", self._ridgeSpd_value)]
            else:
                ridgeus = [("Ridge/upslp dir.", self._ridgeDir_value),
                           ("Ridge/upslp dir.", self._ridgeNumDir_value),
                           ("Ridge/upslp spd.", self._ridgeSpd_value)]

        ### swell tabular phrase configuration ###

        if self._tableSwellElementSplit == "no" and colWidth >= 7:
            swell = [("Swell hgt (ft)..", self._swell_value)]
        else:
            swell = [("Swell direction.", self._swellDir_value),
                     ("Swell hgt (ft)..", self._swellHgt_value)]

        # Mixing Height

        if self._tabularMixingHeightUnits == "ft" and colWidth > 4:
            mixLabel = "Mix hgt (ft)...."
        else:
            mixLabel = "Mix hgt (kft)..."
        mix = [(mixLabel, self._mixingHeight_value)]

        # Mixing Height MSL

        if self._tabularMixingHeightUnits == "ft" and colWidth > 4:
            mixLabel = "Mx ht msl (ft).."
        else:
            mixLabel = "Mx ht msl (kft)."
        mixmsl = [(mixLabel, self._mixingHeightMsl_value)]

        # Mixing Height Metric

        if self._tabularMixingHeightMetricUnits == "m" and colWidth > 4:
            mixMetricLabel = "Mix hgt (m)....."
        else:
            mixMetricLabel = "Mix hgt (km)...."
        mixMetric = [(mixMetricLabel, self._mixingHeightMetric_value)]

        # Transport Wind Metric

        transMetLabel = "Tran wind (m/s)."
        transMetDirLabel = "Trans wind dir.."
        transMetSpdLabel = "Trans spd (m/s)."

        if self._tableTransElementSplit == "no" and colWidth >= 7:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                transMet = [(transMetLabel, self._transMetric_value)]
            else:
                transMet = [(transMetDirLabel, self._transNumDir_value),
                            (transMetLabel, self._transMetric_value)]
        else:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                transMet = [(transMetDirLabel, self._transDir_value),
                            (transMetSpdLabel, self._transSpdMetric_value)]
            else:
                transMet = [(transMetDirLabel, self._transDir_value),
                            (transMetDirLabel, self._transNumDir_value),
                            (transMetSpdLabel, self._transSpdMetric_value)]

        # Mixing Wind Metric

        mixWindMetLabel = "Mix wind (m/s).."
        mixWindMetDirLabel = "Mix wind dir...."
        mixWindMetSpdLabel = "Mix spd (m/s)..."

        if self._tableTransElementSplit == "no" and colWidth >= 7:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                mixWindMet = [(mixWindMetLabel, self._transMetric_value)]
            else:
                mixWindMet = [(mixWindMetDirLabel, self._transNumDir_value),
                              (mixWindMetLabel, self._transMetric_value)]
        else:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                mixWindMet = [(mixWindMetDirLabel, self._transDir_value),
                              (mixWindMetSpdLabel, self._transSpdMetric_value)]
            else:
                mixWindMet = [(mixWindMetDirLabel, self._transDir_value),
                              (mixWindMetDirLabel, self._transNumDir_value),
                              (mixWindMetSpdLabel, self._transSpdMetric_value)]

        # Mixing Wind

        mixWindLabel = "Mixing wind....."
        mixWindDirLabel = "Mixing wind dir."
        mixWindSpdLabel = "Mixing wind spd."

        if self._tableTransElementSplit == "no" and colWidth >= 7:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                mixWind = [(mixWindLabel, self._trans_value)]
            else:
                mixWind = [(mixWindDirLabel, self._transNumDir_value),
                           (mixWindLabel, self._trans_value)]
        else:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                mixWind = [(mixWindDirLabel, self._transDir_value),
                           (mixWindSpdLabel, self._transSpd_value)]
            else:
                mixWind = [(mixWindDirLabel, self._transDir_value),
                           (mixWindDirLabel, self._transNumDir_value),
                           (mixWindSpdLabel, self._transSpd_value)]

        # 1000 FT Wind

        Wind1000Label = "1000 ft wind...."
        Wind1000DirLabel = "1000 ft wnd dir."
        Wind1000SpdLabel = "1000 ft wnd spd."

        if self._tableTransElementSplit == "no" and colWidth >= 7:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                wind1000ft = [(Wind1000Label, self._wind1000ft_value)]
            else:
                wind1000ft = [(Wind1000DirLabel, self._wind1000ftNumDir_value),
                              (Wind1000Label, self._wind1000ft_value)]
        else:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                wind1000ft = [(Wind1000DirLabel, self._wind1000ftDir_value),
                              (Wind1000SpdLabel, self._wind1000ftSpd_value)]
            else:
                wind1000ft = [(Wind1000DirLabel, self._wind1000ftDir_value),
                              (Wind1000DirLabel, self._wind1000ftNumDir_value),
                              (Wind1000SpdLabel, self._wind1000ftSpd_value)]

        # 2000 FT Wind

        Wind2000Label = "2000 ft wind...."
        Wind2000DirLabel = "2000 ft wnd dir."
        Wind2000SpdLabel = "2000 ft wnd spd."

        if self._tableTransElementSplit == "no" and colWidth >= 7:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                wind2000ft = [(Wind2000Label, self._wind2000ft_value)]
            else:
                wind2000ft = [(Wind2000DirLabel, self._wind2000ftNumDir_value),
                              (Wind2000Label, self._wind2000ft_value)]
        else:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                wind2000ft = [(Wind2000DirLabel, self._wind2000ftDir_value),
                              (Wind2000SpdLabel, self._wind2000ftSpd_value)]
            else:
                wind2000ft = [(Wind2000DirLabel, self._wind2000ftDir_value),
                              (Wind2000DirLabel, self._wind2000ftNumDir_value),
                              (Wind2000SpdLabel, self._wind2000ftSpd_value)]

        # 3000 FT Wind

        Wind3000Label = "3000 ft wind...."
        Wind3000DirLabel = "3000 ft wnd dir."
        Wind3000SpdLabel = "3000 ft wnd spd."

        if self._tableTransElementSplit == "no" and colWidth >= 7:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                wind3000ft = [(Wind3000Label, self._wind3000ft_value)]
            else:
                wind3000ft = [(Wind3000DirLabel, self._wind3000ftNumDir_value),
                              (Wind3000Label, self._wind3000ft_value)]
        else:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                wind3000ft = [(Wind3000DirLabel, self._wind3000ftDir_value),
                              (Wind3000SpdLabel, self._wind3000ftSpd_value)]
            else:
                wind3000ft = [(Wind3000DirLabel, self._wind3000ftDir_value),
                              (Wind3000DirLabel, self._wind3000ftNumDir_value),
                              (Wind3000SpdLabel, self._wind3000ftSpd_value)]

        # 4000 FT Wind

        Wind4000Label = "4000 ft wind...."
        Wind4000DirLabel = "4000 ft wnd dir."
        Wind4000SpdLabel = "4000 ft wnd spd."

        if self._tableTransElementSplit == "no" and colWidth >= 7:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                wind4000ft = [(Wind4000Label, self._wind4000ft_value)]
            else:
                wind4000ft = [(Wind4000DirLabel, self._wind4000ftNumDir_value),
                              (Wind4000Label, self._wind4000ft_value)]
        else:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                wind4000ft = [(Wind4000DirLabel, self._wind4000ftDir_value),
                              (Wind4000SpdLabel, self._wind4000ftSpd_value)]
            else:
                wind4000ft = [(Wind4000DirLabel, self._wind4000ftDir_value),
                              (Wind4000DirLabel, self._wind4000ftNumDir_value),
                              (Wind4000SpdLabel, self._wind4000ftSpd_value)]

        # 5000 FT Wind

        Wind5000Label = "5000 ft wind...."
        Wind5000DirLabel = "5000 ft wnd dir."
        Wind5000SpdLabel = "5000 ft wnd spd."

        if self._tableTransElementSplit == "no" and colWidth >= 7:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                wind5000ft = [(Wind5000Label, self._wind5000ft_value)]
            else:
                wind5000ft = [(Wind5000DirLabel, self._wind5000ftNumDir_value),
                              (Wind5000Label, self._wind5000ft_value)]
        else:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                wind5000ft = [(Wind5000DirLabel, self._wind5000ftDir_value),
                              (Wind5000SpdLabel, self._wind5000ftSpd_value)]
            else:
                wind5000ft = [(Wind5000DirLabel, self._wind5000ftDir_value),
                              (Wind5000DirLabel, self._wind5000ftNumDir_value),
                              (Wind5000SpdLabel, self._wind5000ftSpd_value)]

        # Transport Wind

        transWindLabel = "Transport wind.."
        transDirLabel = "Transp wind dir."
        transSpdLabel = "Transp wind spd."

        if self._tableTransElementSplit == "no" and colWidth >= 7:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                trans = [(transWindLabel, self._trans_value)]
            else:
                trans = [(transDirLabel, self._transNumDir_value),
                         (transWindLabel, self._trans_value)]
        else:
            if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                trans = [(transDirLabel, self._transDir_value),
                         (transSpdLabel, self._transSpd_value)]
            else:
                trans = [(transDirLabel, self._transDir_value),
                         (transDirLabel, self._transNumDir_value),
                         (transSpdLabel, self._transSpd_value)]

        ### sky/wx/hazard tabular phrase configuration ###

        if self._elementFormatDict.get("Sky", "alpha") == "alpha":
            if self._bothAlphaNumericDict.get("Sky", "No") == "No":
                skywx = [("Sky cover.......", self._sky_value),
                         ("Weather cov.....", self._weatherCov_value),
                         ("Weather type....", self._weatherType_value),
                         ("Tstm cov........", self._tstmCov_value)]
            else:
                skywx = [("Sky cover.......", self._sky_value),
                         ("Sky (%).........", self._numSky_value),
                         ("Weather cov.....", self._weatherCov_value),
                         ("Weather type....", self._weatherType_value),
                         ("Tstm cov........", self._tstmCov_value)]
        else:
            skywx = [("Sky (%).........", self._sky_value),
                     ("Weather cov.....", self._weatherCov_value),
                     ("Weather type....", self._weatherType_value),
                     ("Tstm cov........", self._tstmCov_value)]

        hazard = [("Hazard vtec 1...", self._wwa_value),
                  ("Hazard vtec 2...", self._wwa2_value),
                  ("Hazard vtec 3...", self._wwa3_value)]

        vent = [("Vrate kt-ft/1K..", self._ventrate_value),
                ("Ventrate Cat....", self._ventrate_cat)]

        return [
            # Set to Directive requirements
            # Each entry is a tuple:
            #  (Narrative Element, narrativeToo, tableRows)
            #
            #  If narrativeToo is 1, then the narrative phrase will be included
            #     in the narrative portion of the product as well.
            #  tableRows is a list of (label:method) pairs.
            #
            ("SKY/WEATHER"             , 1, skywx),
            ("CWR"                     , 1, [("CWR.............",
                                              self._cwr_value)]),
            ("POP"                     , 1, [("Chc of pcpn (%).",
                                              self._pop_value)]),
            ("CHANCE OF RAIN"          , 1, [("Chc of rain (%).",
                                              self._chanceOfRain_value)]),
            ("CHANCE OF THUNDER"       , 1, [("Chc of thdr (%).",
                                              self._chanceOfThunder_value)]),
            ("CHANCE OF LIGHTNING"     , 1, [("Chc of ltng (%).",
                                              self._chanceOfLightning_value)]),
            ("LIGHTNING ACTIVITY LEVEL", 1, [("LAL.............",
                                              self._lal_value)]),
            ("TEMPERATURE"             , 1, [("Temp............",
                                              self._temp_value)]),
            ("DEWPOINT"                , 1, [("Dewpoint........",
                                              self._td_value)]),
            ("HUMIDITY"                , 1, [("RH..............",
                                              self._rh_value)]),
            ("HEAT INDEX"              , 1, [("Heat index (F)..",
                                              self._heatIndex_value)]),
            ("WIND CHILL"              , 1, [("Wind chill (F)..",
                                              self._windChill_value)]),
            ("APPARENT TEMPERATURE"    , 1, [("Aparnt tmp (F)..",
                                              self._apparentTemperature_value)]),
            ("20 FOOT WINDS"           , 1, wind),
            ("VALLEY/LOWER SLOPE WINDS", 1, windvl),
            ("SLOPE/VALLEY WINDS"      , 1, windsv),
            ("EYE LEVEL WINDS"         , 1, eyewind),
            ("SURFACE WINDS"           , 1, sfcwind),
            ("SURFACE WINDS KTS"       , 1, sfcktswind),
            ("RIDGE TOP WIND"          , 1, ridge),
            ("RIDGE/UPPER SLOPE WINDS" , 1, ridgeus),
            ("MIXING HEIGHT"           , 1, mix),
            ("MIXING HEIGHT MSL"       , 1, mixmsl),
            ("MIXING HEIGHT METRIC"    , 1, mixMetric),
            ("TRANSPORT WINDS"         , 1, trans),
            ("MIXING WINDS"            , 1, mixWind),
            ("TRANSPORT WINDS METRIC"  , 1, transMet),
            ("MIXING WINDS METRIC"     , 1, mixWindMet),
            ("100 FT WIND"             , 1, [("100 ft wind.....",
                                              self._wind100ft_value)]),
            ("1000 FT WIND"            , 1, wind1000ft),
            ("2000 FT WIND"            , 1, wind2000ft),
            ("3000 FT WIND"            , 1, wind3000ft),
            ("4000 FT WIND"            , 1, wind4000ft),
            ("5000 FT WIND"            , 1, wind5000ft),
            ("DISPERSION INDEX"        , 1, [("Dispersion......",
                                              self._dsi_value)]),
            ("LDSI"                    , 1, [("Dispersion idx..",
                                              self._ldsi_value)]),
            ("LVORI"                   , 1, [("LVORI...........",
                                              self._lvori_value)]),
            ("ADI"                     , 1, [("ADI.............",
                                              self._adi_value)]),
            ("GFDI"                    , 1, [("GFDI............",
                                              self._gfdi_value)]),
            ("HAINES INDEX"            , 1, [("Haines index....",
                                              self._haines_value)]),
            ("MID LEVEL HAINES INDEX"  , 1, [("Haines mid lvl..",
                                              self._hainesMid_value)]),
            ("VENTILATION RATE"        , 1, vent),
            ("SMOKE DISPERSAL"         , 1, [("SDspl kt-ft/1000",
                                              self._ventrate_value)]),
            ("SWELL HEIGHT"            , 1, swell),
            ("SWELL PERIOD"            , 1, [("Swell period (s)",
                                              self._swellPeriod_value)]),
            ("WAVE PERIOD"             , 1, [("Wave period (s).",
                                              self._wavePeriod_value)]),
            ("WIND WAVE"               , 1, [("Wind wave (ft)..",
                                              self._windWave_value)]),
            ("WAVE HEIGHT"             , 1, [("Wave height (ft)",
                                              self._waveHeight_value)]),
            ("HIONETENTH"              , 1, [("Hi 1/10th (ft)..",
                                              self._hiOneTenth_value)]),
            ("FREEZING SPRAY"          , 1, [("Freezing spray..",
                                              self._freezingSpray_value)]),
            ("SEA ICE CONCENTRATION"   , 1, [("Sea ice cov (%).",
                                              self._seaIceConcentration_value)]),
            ("SEA SURFACE TEMPERATURE" , 1, [("SST (F).........",
                                              self._seaSurfaceTemperature_value)]),
            ("FREEZING LEVEL"          , 1, [("Fz level (kft)..",
                                              self._freezingLevel_value)]),
            ("SNOW LEVEL"              , 1, [("Snow lvl (kft)..",
                                              self._snowLevel_value)]),
            ("CEILING"                 , 1, [("Ceiling (kft)...",
                                              self._ceiling_value)]),
            ("VISIBILITY"              , 1, [("Visibility (sm).",
                                              self._visibility_value)]),
            ("PRESSURE"                , 1, [("Sfc pres (in)...",
                                              self._pressure_value)]),
            ("ICING"                   , 1, [("Icing...........",
                                              self._icing_value)]),
            ("RIVER LEVEL"             , 1, [("River level.....",
                                              self._riverLevel_value)]),
            ("RIVER TEMPERATURE"       , 1, [("River temp......",
                                              self._riverTemperature_value)]),
            ("WATER TEMPERATURE"       , 1, [("Water temp......",
                                              self._waterTemperature_value)]),
            ("HAZARDS"                 , 0, hazard),
            ]

    def _getProductInfo(self, product, issuance, forecasters):
        self.debug_print("Debug: _getProductInfo in Spot_Overrides.py")
        # Parse the spot request information selected and
        # return the FireInfo for display.
        localTime = time.localtime(time.time())
        nowDate = time.strftime('%m/%d/%y', localTime)
        nowTime = time.strftime('%H%M', localTime)
        timezone = time.strftime('%Z', localTime)
        wfoid = self._stqPil[3:6]
        tag = time.strftime('%y00000.0', localTime)
        spotRequestInfo = [
            ("PROJECT NAME:", "incidentName", "xxxx"),
            ("PROJECT TYPE:", "incidentType", "WILDFIRE"),
            ("REQUESTING AGENCY:", "requestingAgency", "xxxx"),
            ("REQUESTING OFFICIAL:", "agencyContact", "yyyy"),
            ("DLAT:", "incidentLatitude", 28.27),
            ("CLON:", "incidentLongitude", -82.19),
            ("SIZE (ACRES):", "incidentSize", 1),
            ("SITE:", "wfoID", wfoid),
            ("OFILE:", "webSiteTag", tag),
            ("TIMEZONE:", "webTimeZone", timezone),
            ("DATE:", "forecastStartDate", nowDate),
            ("TIME:", "forecastStartTime", nowTime),
            ("FORMAT:", "forecastFormat", "X"),
            ("INTERVAL:", "forecastInterval", "0,0,0,0"),
            ("DELIVER DATE:", "forecastDeliverDate", nowDate)
        ]

        obs = []
        self._spotList = ["This is a New Incident"]
        remarksFlag = 0
        extendedDefaultFlag = 0
        outlookDefaultFlag = 0
        remarks = ""

        self._periodElementDict = {
            "Period 1": [], "Period 2": [], "Period 3": [], "Period 4": []
        }

        self._periodAllElementDict = {
            "Period 1": [], "Period 2": [], "Period 3": [], "Period 4": []
        }

        # Set default values
        for field, variable, default in spotRequestInfo:
            setattr(self, '_' + variable, default)

        # If no issuance to use, we are done.
        if issuance is None:
            return

        # If no forecasters included, we are done.
        if forecasters is None:
            return
        self._forecasters = forecasters

        # initialize email
        self._email = ""

        # initialize manualType
        self._manualType = ""

        # If no product to parse, we are done
        self.debug_print("test in _getProductInfo")
        self.debug_print(product)
        if product is None:
            # Use default list of weather elements
            for element, defaultFlag, phrases, searchStrings in self._weInfoList():
                if defaultFlag:
                    self._periodAllElementDict["Period 1"].append(element)
                    self._periodAllElementDict["Period 2"].append(element)
                    self._periodAllElementDict["Period 3"].append(element)
                    self._periodAllElementDict["Period 4"].append(element)
                    self._periodElementDict["Period 1"].append(element)
                    self._periodElementDict["Period 2"].append(element)
                    self._periodElementDict["Period 3"].append(element)
                    self._periodElementDict["Period 4"].append(element)
            for element, defaultFlag in self._weInfoHiddenList():
                if defaultFlag:
                    self._periodElementDict["Period 1"].append(element)
                    self._periodElementDict["Period 2"].append(element)
                    self._periodElementDict["Period 3"].append(element)
                    self._periodElementDict["Period 4"].append(element)
                self._periodAllElementDict["Period 1"].append(element)
                self._periodAllElementDict["Period 2"].append(element)
                self._periodAllElementDict["Period 3"].append(element)
                self._periodAllElementDict["Period 4"].append(element)
            if not self._forceIssuance:
                self._productIssuance = "Morning Update"
            return

        # If Manually Entered Request, then include weather elements based on
        # type.
        self.debug_print("manual case in _getProductInfo")
        self.debug_print(product)
        if "manual - " in product:
            self._manualType = product.split(" - ", 1)[1]
            # Use default list of weather elements
            self.debug_print("manualType")
            self.debug_print(self._manualType)
            self.debug_print(self._defaultElementDict[self._manualType])
            for element in self._defaultElementDict[self._manualType]:
                self._periodAllElementDict["Period 1"].append(element)
                self._periodAllElementDict["Period 2"].append(element)
                self._periodAllElementDict["Period 3"].append(element)
                self._periodAllElementDict["Period 4"].append(element)
                self._periodElementDict["Period 1"].append(element)
                self._periodElementDict["Period 2"].append(element)
                self._periodElementDict["Period 3"].append(element)
                self._periodElementDict["Period 4"].append(element)
            if not self._forceIssuance:
                self._productIssuance = "Morning Update"
            return

        # Parse product
        emailList = []
        wxParmFlag = 0
        wxConditionFlag = 0
        stqElementList = []
        self.debug_print(product)
        for line in product:
            self.debug_print("--- " + line + " ---")

            # search for emails in request
            emailMatch = re.findall(r'[\w.-]+@[\w.-]+', line)
            if emailMatch and not wxConditionFlag:
                for em in emailMatch:
                    emailList.append(em)

            # turn wxConditionFlag on
            if "WEATHER CONDITION AT" in line:
                wxConditionFlag = 1

            # turn wxConditionFlag off
            if "...REMARKS..." in line:
                wxConditionFlag = 0

            # turn wxParmFlag on
            if "...WEATHER PARAMETERS REQUESTED..." in line:
                wxParmFlag = 1

            # turn wxParmFlag off
            if "SITE:" in line:
                wxParmFlag = 0

            # If the line has a colon, split it into fieldName/value
            cleanLine = line.replace("\n", "")
            cleanLine = cleanLine.strip()
            index = cleanLine.find(":")
            if index >= 0:
                # Handle STQ fields (lines with a colon)
                fieldName = cleanLine[:index].strip()
                value = cleanLine[index + 1:].strip()
                self.debug_print("|" + fieldName + "|")

                for field, variable, default in spotRequestInfo:
                    if field in cleanLine and cleanLine.find(field) == 0:
                        # Assign to variable
                        setattr(self, '_' + variable, value)

                if wxParmFlag:
                    for element, defaultFlag, phrases, searchStrings in self._weInfoList():
                        if fieldName == "TEMPERATURE" and element == "TEMPERATURE":
                            flags = value.split(",")
                            self.debug_print("appending ex |" + element + "|")
                            stqElementList.append((element, flags))
                            break
                        elif fieldName == "HUMIDITY" and element == "HUMIDITY":
                            flags = value.split(",")
                            self.debug_print("appending ex |" + element + "|")
                            stqElementList.append((element, flags))
                            break
                        elif fieldName == "HAINES INDEX" and element == "HAINES INDEX":
                            flags = value.split(",")
                            self.debug_print("appending ex |" + element + "|")
                            stqElementList.append((element, flags))
                            break
                        elif fieldName == "MIXING WINDS" and element == "MIXING WINDS":
                            flags = value.split(",")
                            self.debug_print("appending ex |" + element + "|")
                            stqElementList.append((element, flags))
                            break
                        elif fieldName == "TRANSPORT WINDS" and element == "TRANSPORT WINDS":
                            flags = value.split(",")
                            self.debug_print("appending ex |" + element + "|")
                            stqElementList.append((element, flags))
                            break
                        elif fieldName == "MIXING HEIGHT" and element == "MIXING HEIGHT":
                            flags = value.split(",")
                            self.debug_print("appending ex |" + element + "|")
                            stqElementList.append((element, flags))
                            break
                        elif fieldName == "SURFACE WIND" and element == "SURFACE WINDS":
                            flags = value.split(",")
                            self.debug_print("appending ex |" + element + "|")
                            stqElementList.append((element, flags))
                            break
                        elif self._checkStrs(searchStrings, fieldName) == 1 and element != "TEMPERATURE" and element != "HAINES INDEX" \
                                and element != "MIXING WINDS" and element != "TRANSPORT WINDS" and element != "MIXING HEIGHT" \
                                and element != "SURFACE WINDS" and element != "HUMIDITY":
                            # Enter flags in dictionary e.g. 1,1,1 for Today,
                            # Tonight, Tomorrow
                            flags = value.split(",")
                            self.debug_print("appending ex |" + element + "|")
                            stqElementList.append((element, flags))
                            break

            if "ELEV=" in line and "TIME=" in line:
                ob = line.replace("\n", "")
                if "ELEV= TIME=" not in ob:
                    obs.append(ob)
            if remarksFlag and "FORECAST ELEMENTS" not in line:
                remarks = remarks + line
                if "EXTENDED" in line.upper():
                    extendedDefaultFlag = 1
                if "OUTLOOK" in line.upper():
                    outlookDefaultFlag = 1
            if "...REMARKS..." in line:
                remarksFlag = 1
            if "...WEATHER PARAMETERS REQUESTED..." in line:
                remarksFlag = 0
                remarks = remarks.replace("\n\n", "\n")
                remarks = remarks.replace("\n\n", "\n")

        slopeValleyFlag = 0
        lowerSlopeFlag = 0
        upperSlopeFlag = 0
        for element, defaultFlag, phrases, searchStrings in self._weInfoList():
            appendTest = 0
            for stqElement, flags in stqElementList:
                if stqElement == element:
                    self.debug_print("    Match!")
                    # Enter flags in dictionary e.g. 1,1,1,1 for Period 1,
                    # Period 2, Period 3, and maybe Period 4
                    if flags[0] == "1":
                        if element == "SLOPE/VALLEY WINDS":
                            slopeValleyFlag = 1
                        if element == "VALLEY/LOWER SLOPE WINDS":
                            lowerSlopeFlag = 1
                        if element == "RIDGE/UPPER SLOPE WINDS":
                            upperSlopeFlag = 1
                        if element == "20 FOOT WINDS" and (lowerSlopeFlag or slopeValleyFlag):
                            continue
                        if element == "VALLEY/LOWER SLOPE WINDS" and slopeValleyFlag:
                            continue
                        if element == "RIDGE TOP WIND" and upperSlopeFlag:
                            continue
                        self._periodElementDict["Period 1"].append(element)
                        appendTest = 1
                    if flags[1] == "1":
                        if element == "SLOPE/VALLEY WINDS":
                            slopeValleyFlag = 1
                        if element == "VALLEY/LOWER SLOPE WINDS":
                            lowerSlopeFlag = 1
                        if element == "RIDGE/UPPER SLOPE WINDS":
                            upperSlopeFlag = 1
                        if element == "20 FOOT WINDS" and (lowerSlopeFlag or slopeValleyFlag):
                            continue
                        if element == "VALLEY/LOWER SLOPE WINDS" and slopeValleyFlag:
                            continue
                        if element == "RIDGE TOP WIND" and upperSlopeFlag:
                            continue
                        self._periodElementDict["Period 2"].append(element)
                        appendTest = 1
                    if flags[2] == "1":
                        if element == "SLOPE/VALLEY WINDS":
                            slopeValleyFlag = 1
                        if element == "VALLEY/LOWER SLOPE WINDS":
                            lowerSlopeFlag = 1
                        if element == "RIDGE/UPPER SLOPE WINDS":
                            upperSlopeFlag = 1
                        if element == "20 FOOT WINDS" and (lowerSlopeFlag or slopeValleyFlag):
                            continue
                        if element == "VALLEY/LOWER SLOPE WINDS" and slopeValleyFlag:
                            continue
                        if element == "RIDGE TOP WIND" and upperSlopeFlag:
                            continue
                        self._periodElementDict["Period 3"].append(element)
                        appendTest = 1
                    try:
                        if flags[3] == "1":
                            if element == "SLOPE/VALLEY WINDS":
                                slopeValleyFlag = 1
                            if element == "VALLEY/LOWER SLOPE WINDS":
                                lowerSlopeFlag = 1
                            if element == "RIDGE/UPPER SLOPE WINDS":
                                upperSlopeFlag = 1
                                if element == "20 FOOT WINDS" and (lowerSlopeFlag or slopeValleyFlag):
                                    continue
                                if element == "VALLEY/LOWER SLOPE WINDS" and slopeValleyFlag:
                                    continue
                            if element == "RIDGE TOP WIND" and upperSlopeFlag:
                                continue
                            self._periodElementDict["Period 4"].append(element)
                            appendTest = 1
                    except:
                        self.debug_print(
                            "No period 4 flags in STQ product to decode.")
                    self._periodAllElementDict["Period 1"].append(element)
                    self._periodAllElementDict["Period 2"].append(element)
                    self._periodAllElementDict["Period 3"].append(element)
                    self._periodAllElementDict["Period 4"].append(element)
                    continue

            # if no stq elements, then put in defaults for weather elements
            if appendTest == 0 and len(stqElementList) == 0 and element in self._defaultElementDict[self._incidentType]:
                self._periodAllElementDict["Period 1"].append(element)
                self._periodAllElementDict["Period 2"].append(element)
                self._periodAllElementDict["Period 3"].append(element)
                self._periodAllElementDict["Period 4"].append(element)

        self._lowerSlopeFlag = lowerSlopeFlag
        self._slopeValleyFlag = slopeValleyFlag
        for element, defaultFlag in self._weInfoHiddenList():
            if element in self._periodAllElementDict["Period 1"]:
                continue
            if defaultFlag:
                if len(self._periodElementDict["Period 1"]) != 0:
                    self._periodElementDict["Period 1"].append(element)
                if len(self._periodElementDict["Period 2"]) != 0:
                    self._periodElementDict["Period 2"].append(element)
                if len(self._periodElementDict["Period 3"]) != 0:
                    self._periodElementDict["Period 3"].append(element)
                try:
                    if len(self._periodElementDict["Period 4"]) != 0:
                        self._periodElementDict["Period 4"].append(element)
                except:
                    return "test"
            self._periodAllElementDict["Period 1"].append(element)
            self._periodAllElementDict["Period 2"].append(element)
            self._periodAllElementDict["Period 3"].append(element)
            self._periodAllElementDict["Period 4"].append(element)

        # Remove duplicates from emailList
        emailList = set(emailList)

        # put together the email list.
        self._email = ",".join(emailList)

        # Pass extended and outlook default flags
        self._extendedDefaultFlag = extendedDefaultFlag
        self._outlookDefaultFlag = outlookDefaultFlag

        # Set the Product Issuance
        if issuance == "Auto" and not self._forceIssuance:
            if self._forecastStartTime == "0600":
                issuance = "Morning"
            elif self._forecastStartTime in ["0700", "0800", "0900", "1000", "1100"]:
                issuance = "Morning Update"
            elif self._forecastStartTime in ["1200", "1300", "1400", "1500", "1600", "1700"]:
                issuance = "Afternoon Update"
            elif self._forecastStartTime == "1800":
                issuance = "Afternoon"
            elif self._forecastStartTime in ["1900", "2000", "2100", "2200", "2300"]:
                issuance = "Evening Update"
            elif self._forecastStartTime in ["0000", "0100", "0200", "0300", "0400", "0500"]:
                issuance = "Early Morning Update"
            else:
                issuance = "Morning Update"

        self._productIssuance = issuance

        # Default the forecast interval if blank on STQ.
        if self._forecastInterval == "":
            self._forecastInterval = "0,0,0,0"

    def _displayFireInfo(self):
        self.debug_print("Debug: _displayFireInfo in FWS.py")

        # Build and display GUI using the fireInfo
        varList = []

        # Fire Type Section of the GUI
        desTypeList = "Incident Type:", "incidentType"
        if self._manualType != "":
            self._incidentType = self._manualType.split(" ", 1)[0]
        varList.append(
            (desTypeList, self._incidentType, "radio", self._typeList))

        # requesting Agency Section of the GUI
        desAgencyNameList = "Agency:", "requestingAgency"
        agencyNameList = []
        findAgencyFlag = 0
        for agency in self._agencyList:
            id, name = agency
            agencyNameList.append(name)
            if self._requestingAgency == name:
                findAgencyFlag = 1
                requestingAgencyDefault = self._requestingAgency
        if not findAgencyFlag:
            agencyNameList.append("Unlisted")
            requestingAgencyDefault = "Unlisted"
        varList.append(
            (desAgencyNameList, requestingAgencyDefault, "radio", agencyNameList))

        # Include Extendeds/Outlook Section of the GUI
        if not self._shortTermOnly:
            questionList = ["Include Day 3-5 Extended?",
                            "Include Day 6-7 Extended?",
                            "Include Day 8-14 Outlook?"]
            try:
                if self._extendedDefaultFlag and self._outlookDefaultFlag:
                    defaultQuestionList = questionList
                elif self._extendedDefaultFlag:
                    defaultQuestionList = ["Include Day 3-5 Extended?",
                                           "Include Day 6-7 Extended?"]
                elif self._outlookDefaultFlag:
                    defaultQuestionList = ["Include Day 8-14 Outlook?"]
                else:
                    defaultQuestionList = []
            except:
                defaultQuestionList = []
            desExtendedQuestions = "Check Items to Include:", "extendedQuestions"
            varList.append(
                (desExtendedQuestions, defaultQuestionList, "check", questionList))

        # Forecast Type
        desFcstType = "What Type of Forecast?", "forecastType"
        labelList = []
        for label, forecastType, code in self._forecastTypeList:
            if self._forecastFormat == code:
                self._defaultForecastType = forecastType
            labelList.append(label)
        varList.append(
            (desFcstType, self._defaultForecastType, "radio", labelList))

        # Include Ignition Time Forecast Section of the GUI
        if self._includeIgnitionOptionOnGUI:
            desIT = ("Include Ignition Times?", "withIgnitionTimes")
            varList.append(
                (desIT, self._withIgnitionTimes, "radio", ["yes", "no"]))

        # Unlisted Agency Name Section of the GUI
        if not findAgencyFlag:
            desOtherAgencyName = "Name of Agency if not listed....", "otherAgencyName"
            varList.append(
                (desOtherAgencyName, self._requestingAgency, "alphaNumeric"))

        # Incident Name Section of the GUI
        desFireName = "Name of Incident ...................................", "incidentName"
        varList.append((desFireName, self._incidentName, "alphaNumeric"))

        # Incident Tag Section of the GUI
        desWebTag = ".TAG Code of Incident ...................................", "webSiteTag"
        varList.append((desWebTag, self._webSiteTag, "alphaNumeric"))

        # Forecast Deliver Date Section of the GUI
        desDeliverDate = "Forecast Delivery Date...................................", "forecastDeliverDate"
        varList.append(
            (desDeliverDate, self._forecastDeliverDate, "alphaNumeric"))

        # Fire Time Section of the GUI
        desFireTime = "Forecast Start Time ................................", "forecastStartTime"
        varList.append((desFireTime, self._forecastStartTime, "alphaNumeric"))

        # Fire Date Section of the GUI
        desFireDate = "Forecast Start Date ................................", "forecastStartDate"
        varList.append((desFireDate, self._forecastStartDate, "alphaNumeric"))

        # Fire Date Section of the GUI
        desIncidentTimezone = "Forecast Start Time Zone........................", "incidentTZ"
        varList.append(
            (desIncidentTimezone, self._webTimeZone, "alphaNumeric"))

        # Agency Contact Section of the GUI
        desAgencyContact = "Name of Agency Contact..........", "agencyContact"
        varList.append((desAgencyContact, self._agencyContact, "alphaNumeric"))

        # Email Section of the GUI
        desEmail = "Emails to send forecast to..........", "emailSend"
        varList.append((desEmail, self._email, "alphaNumeric"))

        # Fire Latitude Section of the GUI
        desFireLatitude = "Incident Latitude (Deg).......................", "incidentLatitude"
        varList.append(
            (desFireLatitude, self._incidentLatitude, "alphaNumeric"))

        # Fire Longitude Section of the GUI
        desFireLongitude = "Incident Longitude (Deg)...................", "incidentLongitude"
        varList.append(
            (desFireLongitude, self._incidentLongitude, "alphaNumeric"))

        # Fire Size Section of the GUI
        desFireSize = "Incident Size (Acres) .........................", "incidentSize"
        varList.append((desFireSize, self._incidentSize, "alphaNumeric"))

        # Forecast Elements Section of the GUI
        stqResArray = self._forecastInterval.split(',')
        tableHoursDesc = "Tab Hrs"

        ### Period 1 ###

        desElementList = "Period 1 Elements", "period1Elements"
        varList.append((desElementList, self._periodElementDict["Period 1"],
                        "check", self._periodAllElementDict["Period 1"]))
        if len(stqResArray) >= 1 and int(stqResArray[0]) in self._tabularResolutionDict["Period 1"]:
            self._defaultTabularResolution["Period 1"] = int(stqResArray[0])
        desTableRes = tableHoursDesc, "period1TableRes"
        varList.append((desTableRes, self._defaultTabularResolution["Period 1"], "radio",
                        self._tabularResolutionDict["Period 1"]))

        ### Period 2 ###

        desElementList = "Period 2 Elements", "period2Elements"
        varList.append((desElementList, self._periodElementDict["Period 2"],
                        "check", self._periodAllElementDict["Period 2"]))
        if self._tabularAllPeriods == "yes":
            if len(stqResArray) >= 2 and int(stqResArray[1]) in self._tabularResolutionDict["Period 2"]:
                self._defaultTabularResolution[
                    "Period 2"] = int(stqResArray[1])
            desTableRes = tableHoursDesc, "period2TableRes"
            varList.append((desTableRes, self._defaultTabularResolution["Period 2"], "radio",
                            self._tabularResolutionDict["Period 2"]))

        ### Period 3 ###

        desElementList = "Period 3 Elements", "period3Elements"
        varList.append((desElementList, self._periodElementDict["Period 3"],
                        "check", self._periodAllElementDict["Period 3"]))
        if self._tabularAllPeriods == "yes":
            if len(stqResArray) >= 3 and int(stqResArray[2]) in self._tabularResolutionDict["Period 3"]:
                self._defaultTabularResolution[
                    "Period 3"] = int(stqResArray[2])
            desTableRes = tableHoursDesc, "period3TableRes"
            varList.append((desTableRes, self._defaultTabularResolution["Period 3"], "radio",
                            self._tabularResolutionDict["Period 3"]))

        ### Period 4 ###

        if self._productIssuance in ["Afternoon", "Evening Update", "Early Morning Update"]:
            desElementList = "Period 4 Elements", "period4Elements"
            varList.append((desElementList, self._periodElementDict["Period 4"],
                            "check", self._periodAllElementDict["Period 4"]))
            if self._tabularAllPeriods == "yes":
                if len(stqResArray) >= 4 and int(stqResArray[3]) in self._tabularResolutionDict["Period 4"]:
                    self._defaultTabularResolution[
                        "Period 4"] = int(stqResArray[3])
                desTableRes = tableHoursDesc, "period4TableRes"
                varList.append((desTableRes, self._defaultTabularResolution["Period 4"], "radio",
                                self._tabularResolutionDict["Period 4"]))
        else:
            desElementList = "Period 4 Elements*", "period4Elements"
            varList.append((desElementList, self._periodElementDict["Period 4"],
                            "check", self._periodAllElementDict["Period 4"]))
            if self._tabularAllPeriods == "yes":
                if len(stqResArray) >= 4 and int(stqResArray[3]) in self._tabularResolutionDict["Period 4"]:
                    self._defaultTabularResolution[
                        "Period 4"] = int(stqResArray[3])
                desTableRes = tableHoursDesc, "period4TableRes"
                varList.append((desTableRes, self._defaultTabularResolution["Period 4"], "radio",
                                self._tabularResolutionDict["Period 4"]))
            desTableNote = "*Note: Period 4 will only be included in forecast if Forecast Start Time is changed above to be 1800 or higher OR 0500 or lower", "tableNote"
            varList.append((desTableNote, "", "label"))

        # Launch the Spot Request Quality Control GUI.
        varDict = self._callProcessVariableList(
            "Input Info", varList, varDict={})
        if varDict is None:
            return None

        # Set up varDict for forecastType using labels
        value = varDict[desFcstType]
        for label, forecastType, code in self._forecastTypeList:
            if label == value:
                varDict[desFcstType] = forecastType
                break

        # This section of code filters the forecaster entries to ensure that
        # single quotes are not included.
        if not findAgencyFlag:
            try:
                varDict[desOtherAgencyName] = varDict[
                    desOtherAgencyName].replace("\'", "")
            except AttributeError as e:
                self.debug_print("Other Agency Name is not a string.")
                self.debug_print(e)
        try:
            varDict[desFireName] = varDict[desFireName].replace("\'", "")
        except AttributeError as e:
            self.debug_print("Fire Name is not a string.")
            self.debug_print(e)
        try:
            varDict[desWebTag] = varDict[desWebTag].replace("\'", "")
        except AttributeError as e:
            self.debug_print(".TAG Code is not a string.")
            self.debug_print(e)
        try:
            varDict[desAgencyContact] = varDict[
                desAgencyContact].replace("\'", "")
        except AttributeError as e:
            self.debug_print("Agency Contact is not a string.")
            self.debug_print(e)
        try:
            varDict[desFireSize] = str(varDict[desFireSize]).replace("\'", "")
        except AttributeError as e:
            self.debug_print("Fire Size is not a string.")
            self.debug_print(e)
        try:
            varDict[desFireLatitude] = str(
                varDict[desFireLatitude]).replace("\'", "")
        except AttributeError as e:
            self.debug_print("Latitude is not a string.")
            self.debug_print(e)
        try:
            varDict[desFireLongitude] = str(
                varDict[desFireLongitude]).replace("\'", "")
        except AttributeError as e:
            self.debug_print("Longitude is not a string.")
            self.debug_print(e)
        try:
            varDict[desFireTime] = str(varDict[desFireTime]).replace("\'", "")
        except AttributeError as e:
            self.debug_print("Ignition Time is not a string.")
            self.debug_print(e)
        try:
            varDict[desFireDate] = str(varDict[desFireDate]).replace("\'", "")
        except AttributeError as e:
            self.debug_print("Ignition Date is not a string.")
            self.debug_print(e)

        # This section of code filters the forecaster entries to ensure that
        # double quotes are not included.
        if not findAgencyFlag:
            try:
                varDict[desOtherAgencyName] = varDict[
                    desOtherAgencyName].replace("\"", "")
            except AttributeError as e:
                self.debug_print("Other Agency Name is not a string.")
                self.debug_print(e)
        try:
            varDict[desFireName] = varDict[desFireName].replace("\"", "")
        except AttributeError as e:
            self.debug_print("Fire Name is not a string.")
        try:
            varDict[desAgencyContact] = varDict[
                desAgencyContact].replace("\"", "")
        except AttributeError as e:
            self.debug_print("Fire Size is not a string.")
            self.debug_print(e)
        try:
            varDict[desFireSize] = varDict[desFireSize].replace("\"", "")
        except AttributeError as e:
            self.debug_print("Fire Size is not a string.")
            self.debug_print(e)
        try:
            varDict[desFireLatitude] = varDict[
                desFireLatitude].replace("\"", "")
        except AttributeError as e:
            self.debug_print("Latitude is not a string.")
            self.debug_print(e)
        try:
            varDict[desFireLongitude] = varDict[
                desFireLongitude].replace("\"", "")
        except AttributeError as e:
            self.debug_print("Longitude is not a string.")
            self.debug_print(e)
        try:
            varDict[desFireTime] = varDict[desFireTime].replace("\"", "")
        except AttributeError as e:
            self.debug_print("Ignition Time is not a string.")
            self.debug_print(e)
        try:
            varDict[desFireDate] = varDict[desFireDate].replace("\"", "")
        except AttributeError as e:
            self.debug_print("Ignition Date is not a string.")
            self.debug_print(e)

        # convert lat/lon to floats
        try:
            varDict[desFireLatitude] = float(varDict[desFireLatitude])
        except ValueError as e:
            self.debug_print("Latitude is not a float.")
            self.debug_print(e)
        try:
            varDict[desFireLongitude] = float(varDict[desFireLongitude])
        except ValueError as e:
            self.debug_print("Longitude is not a float.")
            self.debug_print(e)

        # Convert forecastStartTime
        forecastStartTime = varDict[desFireTime]
        forecastStartTime = "000" + str(int(forecastStartTime))
        forecastStartTime = forecastStartTime[-4:]
        varDict[desFireTime] = forecastStartTime

        # Here are more varDict settings that need to be set before we launch
        # the formatter.
        varDict[
            ("Product Issuance:", "productIssuance")] = self._productIssuance
        varDict[("Forecaster:", "forecaster")] = self._forecasters
        varDict[("WFOid:", "wfoID")] = self._wfoID
        try:
            varDict[
                ("Lower Slope Flag:", "lowerSlopeFlag")] = self._lowerSlopeFlag
        except:
            varDict[("Lower Slope Flag:", "lowerSlopeFlag")] = 0
        try:
            varDict[
                ("Slope Valley Flag:", "slopeValleyFlag")] = self._slopeValleyFlag
        except:
            varDict[("Slope Valley Flag:", "slopeValleyFlag")] = 0

        if self._shortTermOnly:
            varDict[("Check Items to Include:", "extendedQuestions")] = []

        return varDict

#   Quality Control Form Information from user dialog to ensure completeness.

    # 04/24/07:  Tabular/Narrative is okay for wildfires. Changed code to make
    #            Tabular Only into Tabular/Narrative for wildfires.

    # From Spot_Overrides. Fixed a bug that causes the formatter to crash
    # when a number was entered for the agency or the contact. Added a
    # method called _convertToStr to do this cleanly.
    def _qualityControlFormData(self):
        self.debug_print("Debug: _qualityControlFormData in FWS.py")

        # If incidentSize is not an integer, then I default the size to 1 acre.
        # This will allow the formatter to run even if the user puts invalid
        # characters into the size field (like 10-20).
        try:
            self._incidentSize = int(float(self._incidentSize) + 0.5)
        except ValueError:
            self._incidentSize = 1
        if self._incidentSize <= 0:
            self._incidentSize = 1

        try:
            lat = float(self._incidentLatitude)
        except ValueError:
            return "Invalid latitude value."
        if lat < -90.0 or lat > 90.0:
            return "Invalid latitude value. Must be within -90 and 90"

        try:
            lon = float(self._incidentLongitude)
        except ValueError:
            return "Invalid longitude value."
        if lon < -180.0 or lon > 180.0:
            return "Invalid longitude value. Must be within -180 and 180"

        if len(self._forecaster) == 0:
            return "You must select at least one forecaster in the list."

        if self._productIssuance in ["Morning", "Morning Update", "Afternoon Update"]:
            elementLists = [
                self._period1Elements, self._period2Elements, self._period3Elements]
            if len(self._period1Elements) == 0 and len(self._period2Elements) == 0 and len(self._period3Elements) == 0:
                return "No Weather Elements were selected. Either rerun Spot Formatter and select weather elements\nor run a \"Change Request\" on the incident on the weather.gov/spot website."
        else:
            elementLists = [self._period1Elements, self._period2Elements,
                            self._period3Elements, self._period4Elements]
            if len(self._period1Elements) == 0 and len(self._period2Elements) == 0 and len(self._period3Elements) == 0 and len(self._period4Elements) == 0:
                return "No Weather Elements were selected. Either rerun Spot Formatter and select weather elements\n or run a \"Change Request\" on the incident on the weather.gov/spot website."

        # Check to make sure at least weather element is requested.
        elementsFound = 0
        for elementList in elementLists:
            if len(elementList) > 0:
                elementsFound = 1
                break
        if not elementsFound and \
           "Include Day 3-5 Extended?" not in self._extendedQuestions and \
           "Include Day 6-7 Extended?" not in self._extendedQuestions and \
           "Include Day 8-14 Outlook?" not in self._extendedQuestions:
            return "You must select at least one weather element to " + \
                   "forecast in the gui."

        # Code to ensure the wildfireElementList parameters are included in
        # the Spot product (if this is a wildfire incident) was added to this
        # method.
        if self._incidentType.upper() == "WILDFIRE":
            for element in self._wildfireElementList:
                for elementList in elementLists:
                    if element not in elementList and len(elementList) != 0:
                        elementList.append(element)
            if self._tabularForWildfire == "no" and \
               self._forecastType == "Tabular Only":
                self._forecastType = "Tabular/Narrative"

        self._incidentName = self._convertToStr(self._incidentName)
        if len(self._incidentName) == 0:
            return "You must enter the Name of Fire."

        self._agencyContact = self._convertToStr(self._agencyContact)
        if len(self._agencyContact) == 0:
            return "You must enter the Name of Agency Contact."

        self._requestingAgency = self._convertToStr(self._requestingAgency)
        if len(self._requestingAgency) == 0:
            return "You must choose a requesting agency."

        if self._requestingAgency == "Unlisted":
            self._otherAgencyName = self._convertToStr(self._otherAgencyName)
            if len(self._otherAgencyName) == 0:
                return "You must enter a requesting agency."

        validTimezones = ["BST11", "HST10", "AKST9AKDT", "PST8PDT", "MST7MDT", "MST7", "CST6CDT", "EST5EDT",
                          "EST", "EST5", "AST4ADT", "EET-10", "NZST-12", "ADT", "AST", "EDT", "CST", "CDT",
                          "MST", "MDT", "PST", "PDT", "AKST", "AKDT", "HST", "BST", "ChST", "CHST", "NZST"]
        if self._incidentTZ not in validTimezones:
            return "-" + str(self._incidentTZ) + "- is not a valid timezone for the incident."

    def _convertToStr(self, var):
        self.debug_print("Debug: _convertToStr in FWS.py")
        try:
            stringSize = len(var)
            return var
        except TypeError:
            try:
                var = str(int(var + 0.5))
            except TypeError:
                var = str(var)
            return var

    def _convertTimezone(self, tz):
        self.debug_print("Debug: _convertTimezone in Spot_Overrides.py")
        # Time zones listed in the STQ product usually do not match a time zone that GFE recognizes.
        # This method makes the conmversion.
        tzDict = {
            "MST7": "MST",
            "MDT": "MST7MDT",
            "MST": "MST7MDT",
            "EST5": "EST",
            "EDT": "EST5EDT",
            "EST": "EST5EDT",
            "CDT": "CST6CDT",
            "CST": "CST6CDT",
            "PDT": "PST8PDT",
            "PST": "PST8PDT",
            "AST4ADT": "America/Puerto_Rico",
            "AST": "America/Puerto_Rico",
            "ADT": "America/Puerto_Rico",
            "AKST9AKDT": "US/Alaska",
            "AKST": "US/Alaska",
            "AKDT": "US/Alaska",
            "HST10": "HST",
            "HST": "HST",
            "BST11": "Pacific/Pago_Pago",
            "BST": "Pacific/Pago_Pago",
            "EET-10": "Pacific/Guam",
            "ChST": "Pacific/Guam",
            "CHST": "Pacific/Guam",
            "NZST-12": "Pacific/Wake",
            "NZST": "Pacific/Wake"
        }
        return tzDict.get(tz, tz)

    def _getOfficeTimezone(self):
        self.debug_print("Debug: _getOfficeTimezone in FWS.py")
        stz = time.tzname[0]
        dtz = time.tzname[1]
        if stz == "MST" and dtz == "MST":
            return "MST"
        elif stz == "MST" and dtz == "MDT":
            return "MST7MDT"
        elif stz == "EST" and dtz == "EST":
            return "EST"
        elif stz == "EST" and dtz == "EDT":
            return "EST5EDT"
        elif stz == "CST" and dtz == "CDT":
            return "CST6CDT"
        elif stz == "PST" and dtz == "PDT":
            return "PST8PDT"
        elif stz == "AST" and dtz == "AST":
            return"America/Puerto_Rico"
        elif stz == "AKST" and dtz == "AKDT":
            return "US/Alaska"
        elif stz in ["HST10", "BST11", "EET-10", "NZST-12"]:
            return self._convertTimezone(stz)
        elif stz == "UTC":
            return "UTC"
        else:
            return "UTC"

    def _getTime(self, date, t):
        self.debug_print("Debug: _getTime in FWS.py")
        # Make a unix time integer from the given date and time strings
        if t == "":
            t = "0000"
        else:
            t = "000" + str(int(t))
            t = t[-4:]

        # Build the WFO Time Zone
        self._officeTimezone = self._getOfficeTimezone()

        # Set the time zone of the incident
        incidentTimezone = self._convertTimezone(self._incidentTZ)

        # Put Forecast Start Date/Time into datetime structure using product
        # time zone
        self.debug_print("*** Forecast Start Date/Time ***")
        self.debug_print(t + ' ' + date)
        cTime = datetime.strptime(t + ' ' + date, '%H%M %m/%d/%y')
        timezone = pytz.timezone(incidentTimezone)
        cTime = timezone.localize(cTime)

        # Now convert to local WFO time zone if incident has in a different
        # time zone
        if self._incidentTZ != self._officeTimezone:
            cTime = cTime.astimezone(pytz.timezone(self._officeTimezone))

        self.debug_print("Forecast Creation Time:")
        self.debug_print(cTime.strftime('%H%M %m/%d/%y %Z'))

        return time.mktime(cTime.timetuple())

    def _makeFcstTimeStatement(self, fcst, argDict):
        self.debug_print("Debug: _makeFcstTimeStatement in FWS.py")
        requestWords = self._getRequestWords()

        # Build the WFO Time Zone
        self._officeTimezone = self._getOfficeTimezone()

        # Set the time zone of the incident
        incidentTimezone = self._convertTimezone(self._incidentTZ)

        # Create a datetime structure in the requested product's timezone
        tempdatetime = datetime.strptime(
            self._forecastStartTime + ' ' + self._forecastStartDate,
            '%H%M %m/%d/%y')
        timezone = pytz.timezone(incidentTimezone)
        tempdatetime = timezone.localize(tempdatetime)

        # NOW CONVERT TO LOCAL WFO TIME ZONE
        tempdatetime = tempdatetime.astimezone(pytz.timezone(incidentTimezone))
        self._forecastStartDateTime = tempdatetime.timetuple()

        fcst = fcst + time.strftime(
            'Forecast is based on ' + requestWords + ' time of %H%M ' +
            tempdatetime.tzname() + ' on %B %d. ',
            self._forecastStartDateTime) + "\n"
        self._makeFireTimeRange()
        return fcst

    def _makeFireTimeRange(self):
        self.debug_print("Debug: _makeFireTimeRange in FWS.py")
        # Make a 1-hour fire time range for the forecastStartTime
        if self._withIgnitionTimes == "no":
            return None
        forecastStartDateTime = time.mktime(self._forecastStartDateTime)
        self._fireTR = self.makeTimeRange(
            forecastStartDateTime, forecastStartDateTime + 3600)
        self.debug_print("Fire Time Range:", self._fireTR)

    def _checkFireTR(self, tr):
        self.debug_print("Debug: _checkFireTR in FWS.py")
        if self._fireTR is None:
            return 0
        return self._fireTR.overlaps(tr)

    # This is a new method that Matt Davis wrote. Figures out whether or not
    # we are using a ignition time, request time, or incident time.
    def _getRequestWords(self):
        self.debug_print("Debug: _getRequestWords in FWS.py")
        if self._incidentType.upper() == "WILDFIRE":
            return "forecast start"
        elif self._incidentType.upper() == "PRESCRIBED":
            return "ignition"
        else:
            return "forecast start"

    # Import the discussion from a previously edited discussion file.
    def _makeDiscussion(self, fcst, argDict):
        self.debug_print("Debug: _makeDiscussion in FWS.py")
        discussionHeader = ""
        discussionHeader = ".DISCUSSION...\n"

        if self._insertDiscussionFromFile == 1:
            discussion = ""
            if os.path.isfile(self._discussionFile):
                with open(self._discussionFile, 'r') as input:
                    text = input.readlines()
                    for line in text:
                        discussion = discussion + line
                discussion = discussion.replace("\n\n", "\n")
                discussion = discussion.replace("\n\n", "\n")
                return fcst + discussionHeader + discussion + "\n"
            else:
                discussion = "...Put discussion text here..."
                return fcst + discussionHeader + discussion + "\n\n"
        elif self._insertDiscussionFromFile == 2:
            version = 0
            fwfPil = self._textdbPil[0:3] + self._fwfPil
            searchString = ""
            product = self.getPreviousProduct(
                fwfPil, searchString, version=version)
            product = product.split("\n")
            discussion = ""
            disFlag = 0
            foundDiscussion = 0
            for line in product:
                if "discussion..." in line.lower():
                    disFlag = 1
                    foundDiscussion = 1
                try:
                    if line[2] == "Z" and line[-1] == "-" and \
                       (line[6] == "-" or line[6] == ">"):
                        disFlag = 0
                except IndexError:
                    #self.debug_print("Discussion Index Error",line)
                    a = 0
                if line[:2] == "$$":
                    disFlag = 0
                if disFlag:
                    discussion = discussion + line + "\n"
            if foundDiscussion:
                return fcst + discussion + "\n\n"
            else:
                discussion = "...Put discussion text here..."
                return fcst + discussionHeader + discussion + "\n\n"
        else:
            return fcst + discussionHeader + "\n\n\n"

#   Create areaList based on lat/lon/size/firename.
    def _determineAreaList(self, argDict):
        self.debug_print("Debug: _determineAreaList in FWS.py")
        # Size of the fire is entered as acres.
        # Convert this area into square kilometers.
        # createLatLonArea only needs the length of the side of a square.
        size_out = int(math.sqrt(float(self._incidentSize) / 247.0) + 0.5)
        area = self.createLatLonArea(float(self._incidentLatitude),
                                     float(self._incidentLongitude),
                                     size_out)
        # SET UP FOR HAZARDS
        # Save to server
        self.saveEditAreas([area])
        # Create Hazards Table for this area
        hazards = HazardsTable.HazardsTable(
            argDict["ifpClient"], [[area.getId().getName()]], "FWS",
            self.filterMethod, argDict["databaseID"],
            self._fullStationID,
            activeTableName=argDict['vtecActiveTable'],
            vtecMode=argDict['vtecMode'],
            dataMgr=argDict['dataMgr'])
        argDict["hazards"] = hazards
        # Remove from server
        self.deleteEditAreas([area])

        self._areaList = [(area, self._incidentName)]

#   Set the extended configuration based on user input.
    def _setExtendedConfig(self):
        self.debug_print("Debug: _setExtendedConfig in FWS.py")
        # Include extended forecast if wanted and allowed.
        if "Include Day 3-5 Extended?" not in self._extendedQuestions and \
           "Include Day 6-7 Extended?" not in self._extendedQuestions:
            if self._individualExtended == 1:
                self._individualExtended = 0
            if self._summaryExtended == 1:
                self._summaryExtended = 0
            self._extendedLabel = 0
        else:
            if self._individualExtended == 1:
                self._individualExtended = 1
            if self._summaryExtended == 1:
                self._summaryExtended = 1
            self._extendedLabel = 1

        if "Include Day 3-5 Extended?" in self._extendedQuestions:
            self._includeExtendedShortTerm = 1
        else:
            self._includeExtendedShortTerm = 0

        if "Include Day 6-7 Extended?" in self._extendedQuestions:
            self._includeExtended = 1
        else:
            self._includeExtended = 0

    # Add the "if conditions become unrep..." statement.
    def _makeUnrepresentStatement(self, fcst, argDict):
        self.debug_print("Debug: _makeUnrepresentStatement in FWS.py")
        if self._insertUnrepresentStatement == 1:
            return fcst + self._unrepresentStatement + "\n\n"
        else:
            return fcst

    # Add the contact statement.
    def _makeContactStatement(self, fcst, argDict):
        self.debug_print("Debug: _makeContactStatement in FWS.py")
        if self._contactStatement != "":
            return fcst + self._contactStatement + "\n\n"
        else:
            return fcst

    # Place the headlines above the discussion.
    def _makeHeadline(self, fcst, editArea, areaLabel, argDict):
        self.debug_print("Debug: _makeHeadline in FWS.py")
        # get the hazards text
        self._hazards = argDict['hazards']
        self._combinations = argDict["combinations"]
        headlines = self.generateProduct("Hazards", argDict,
                                         area=editArea, areaLabel=areaLabel,
                                         timeRange=self._timeRange)
        fcst = fcst + headlines
        return fcst

    # Deal with the summary extended more cleanly.
    def _makeSummaryExtended(self, fcst, argDict):
        self.debug_print("Debug: _makeSummaryExtended in FWS.py")
        # Add one extended
        if self._summaryExtended == 1:
            extended = self.generateProduct("ExtendedNarrative",
                                            argDict, area=self._summaryArea,
                                            timeRange=self._extendedRange)
            fcst = fcst + extended
            fcst = fcst + "\n"
        return fcst

    def _generateOutlookLabels(self, argDict):
        self.debug_print("Debug: _generateOutlookLabels in FWS.py")
        today = argDict["creationTime"]
        if self._productIssuance in ["Morning", "Morning Update", "Afternoon Update"]:
            day8 = today + 7 * 24 * 3600
            day14 = today + 13 * 24 * 3600
            dow = time.gmtime(today)[6]
            if dow == 0 or dow == 2 or dow == 4:
                self._insertOutlookFlag = 1
            else:
                self._insertOutlookFlag = 0
            self._insertOutlookFlag = 1
        else:
            currentHour = time.gmtime(today)[3]
            if currentHour < 16:
                day8 = today + 7 * 24 * 3600
                day14 = today + 13 * 24 * 3600
                dow = time.gmtime(today)[6]
                if dow == 0 or dow == 2 or dow == 4:
                    self._insertOutlookFlag = 1
                else:
                    self._insertOutlookFlag = 0
                self._insertOutlookFlag = 1
            else:
                day8 = today + 8 * 24 * 3600
                day14 = today + 14 * 24 * 3600
                dow = time.gmtime(today + 24 * 3600)[6]
                if dow == 1 or dow == 3 or dow == 6:
                    self._insertOutlookFlag = 1
                else:
                    self._insertOutlookFlag = 0
                self._insertOutlookFlag = 1

        self._outlookDay8Label = time.strftime("%A %B %d", time.gmtime(day8))
        self._outlookDay14Label = time.strftime("%A %B %d", time.gmtime(day14))

        return None

    # Import the 8 to 14 day outlook into the product
    # if the user requests it for the spot forecast.
    def _make8to14DayOutlook(self, fcst, argDict):
        self.debug_print("Debug: _make8to14DayOutlook in FWS.py")
        if "Include Day 8-14 Outlook?" not in self._extendedQuestions:
            return fcst

        outlookHeader = ".Outlook for " + self._outlookDay8Label + " through " \
                        +self._outlookDay14Label + "...\n"
        outlookHeader = outlookHeader.upper()

        if self._insertOutlookFromFile == 1:
            outlook = ""
            if os.path.isfile(self._outlookFile):
                with open(self._outlookFile) as input:
                    text = input.readlines()
                    for line in text:
                        outlook = outlook + line
                outlook = outlook.replace("\n\n", "\n")
                outlook = outlook.replace("\n\n", "\n")
                return fcst + outlookHeader + outlook + "\n"
            else:
                outlook = "...Put 8 to 14 day outlook text here..."
                return fcst + outlookHeader + outlook + "\n\n"
        elif self._insertDiscussionFromFile == 2:
            version = 0
            fwfPil = self._textdbPil[0:3] + self._fwfPil
            searchString = ""
            product = self.getPreviousProduct(
                fwfPil, searchString, version=version)
            product = product.split("\n")
            outlook = ""
            outFlag = 0
            foundOutlook = 0
            for line in product:
                if line[:2] == "$$":
                    outFlag = 0
                if outFlag:
                    outlook = outlook + line + "\n"
                if line.find(".OUTLOOK") != -1:
                    outFlag = 1
                    foundOutlook = 1
            if foundOutlook:
                return fcst + outlookHeader + outlook + "\n\n"
            else:
                outlook = "...Put 8 to 14 day outlook text here..."
                return fcst + outlookHeader + outlook + "\n\n"
        else:
            return fcst + outlookHeader + "\n\n\n"

    def FirePeriod1(self):
        self.debug_print("Debug: _FirePeriod1 in FWS.py")
        phraseList = self.getFirePeriod_phraseList(self._period1Elements)
        analysisList = self.getFirePeriod_analysisList()
        intersectAreas = self.getFirePeriod_intersectAreas(1)
        return {
            "type": "component",
            "methodList": [
                self.assembleIndentedPhrases,
            ],
            "analysisList": analysisList,
            "phraseList": phraseList,
            "intersectAreas": intersectAreas,
        }

    def FirePeriod2(self):
        self.debug_print("Debug: _FirePeriod2 in FWS.py")
        phraseList = self.getFirePeriod_phraseList(self._period2Elements)
        analysisList = self.getFirePeriod_analysisList()
        intersectAreas = self.getFirePeriod_intersectAreas(2)
        return {
            "type": "component",
            "methodList": [
                self.assembleIndentedPhrases,
            ],
            "analysisList": analysisList,
            "phraseList": phraseList,
            "intersectAreas": intersectAreas,
        }

    def FirePeriod3(self):
        self.debug_print("Debug: _FirePeriod3 in FWS.py")
        phraseList = self.getFirePeriod_phraseList(self._period3Elements)
        analysisList = self.getFirePeriod_analysisList()
        intersectAreas = self.getFirePeriod_intersectAreas(3)
        return {
            "type": "component",
            "methodList": [
                self.assembleIndentedPhrases,
            ],
            "analysisList": analysisList,
            "phraseList": phraseList,
            "intersectAreas": intersectAreas,
        }

    def FirePeriod4(self):
        self.debug_print("Debug: _FirePeriod4 in FWS.py")
        phraseList = self.getFirePeriod_phraseList(self._period4Elements)
        analysisList = self.getFirePeriod_analysisList()
        intersectAreas = self.getFirePeriod_intersectAreas(4)
        return {
            "type": "component",
            "methodList": [
                self.assembleIndentedPhrases,
            ],
            "analysisList": analysisList,
            "phraseList": phraseList,
            "intersectAreas": intersectAreas,
        }

    def EmptyFirePeriod(self):
        self.debug_print("Debug: _EmptyFirePeriod in FWS.py")
        phraseList = []
        analysisList = self.getFirePeriod_analysisList()
        intersectAreas = self.getFirePeriod_intersectAreas(1)
        return {
            "type": "component",
            "methodList": [
                self.assembleIndentedPhrases,
            ],
            "analysisList": analysisList,
            "phraseList": phraseList,
            "intersectAreas": intersectAreas,
        }

    def PreFirePeriod1(self):
        self.debug_print("Debug: _PreFirePeriod1 in FWS.py")
        analysisList = self.getFirePeriod_analysisList()
        intersectAreas = self.getFirePeriod_intersectAreas(1)
        return {
            "type": "component",
            "methodList": [self.noWords],
            "analysisList": analysisList,
            "phraseList": [],
            "intersectAreas": intersectAreas,
        }

    def createLabel(self, tree, node, timeRange, issuanceInfo, currentLocalTime, shift, index=0):
        self.debug_print("Debug: createLabel in FWS.py")
        # Make a label given the timeRange in GMT and the shift to
        # convert it to local time. currentLocalTime can be used to
        # compare to current day.

        curLocal, shift = self.determineTimeShift()
        if currentLocalTime.hour == 0:
            if index == 0 and self._equalDates(currentLocalTime - 3600, curLocal):
                try:
                    label = issuanceInfo.period1Label()
                    if label != "":
                        return label + "\n"
                except:
                    pass
        else:
            if index == 0 and self._equalDates(currentLocalTime, curLocal):
                try:
                    label = issuanceInfo.period1Label()
                    if label != "":
                        return label + "\n"
                except:
                    pass
        if index == 0 and self._productIssuance == "Early Morning Update" and self._equalDates(currentLocalTime - 18 * 3600, curLocal):
            try:
                label = issuanceInfo.period1Label()
                if label != "":
                    return label + "\n"
            except:
                pass
        try:
            today = issuanceInfo.todayFlag()
        except:
            today = 1
        try:
            useHolidays = self._useHolidays
        except:
            useHolidays = 1
        nextDay24HourLabel = self.nextDay24HourLabel_flag(tree, node)
        splitDay24HourLabel = self.splitDay24HourLabel_flag(tree, node)
        label = self.getWeekday(timeRange, holidays=useHolidays, shiftToLocal=1,
                                labelType="CapitalWithPeriod", today=today,
                                tomorrow=0, nextDay24HourLabel=nextDay24HourLabel,
                                splitDay24HourLabel=splitDay24HourLabel)
        return label + "\n"

    def _equalDates(self, t1, t2):
        self.debug_print("Debug: _equalDates in FWS.py")
        # If AbsTimes t1 and t2 represent the same day, month, year
        # return 1 else 0
        d1 = t1.day
        d2 = t2.day
        m1 = t1.month
        m2 = t2.month
        y1 = t1.year
        y2 = t2.year
        if d1 == d2 and m1 == m2 and y1 == y2:
            return 1
        else:
            return 0

    def getWeekday_descriptor(self, timeRange, holidays=0, shiftToLocal=0,
                              labelType="Worded", today=0, tomorrow=0,
                              holidayModule="Holidays", nextDay24HourLabel=0,
                              splitDay24HourLabel=0):
        self.debug_print("Debug: getWeekday_descriptor in FWS.py")
        # Return a weekday text string
        # Arguments:
        #  timeRange
        #  holidays : if 1, the holiday file will be consulted
        #  shiftToLocal : if 1, will shift the given time range to local time
        #  labelType : See Labels dictionary below for types
        #  today : if 1, Today, Tonight,
        #    will be returned instead of corresponding weekday name
        #  tomorrow: if 1, Tomorrow, Tomorrow Night
        #    will be returned instead of corresponding weekday name
        #  holidayModule: file containing holiday dates
        #  nextDay24HourLabel: if 1, a 24-hour time period starting
        #    after 1600, will be labeled as the next day.
        #    This is to accommodate 24 extended periods that go from
        #    6pm-6pm.
        #  splitDay24HourLabel: if 0, a 24-hour period will be labeled with
        #    simply the weekday name (e.g. Saturday)
        #    instead of including the day and night periods
        #    (e.g. Saturday and Saturday night)
        #
        # If the time range is for today AND is less than 12 hours,
        # we must accurately describe the period.
        # At some point, this may need to be coordinated with TextRules
        # timePeriod descriptors.

        currentLocalTime, shift = self.determineTimeShift()
        if timeRange.startTime().hour == 0 or timeRange.startTime().hour == 12:
            shift = shift - 3600
        if shiftToLocal == 1:
            timeRange = TimeRange.TimeRange(timeRange.startTime() + shift,
                                            timeRange.endTime() + shift)
        labels = self.Labels()[labelType]
        pre = labels["PrePunctuation"]
        post = labels["PostPunctuation"]
        todayFlag = currentLocalTime.day == timeRange.startTime().day
        try:
            if self._productIssuance == "Next Day":
                todayFlag = currentLocalTime.day + \
                    1 == timeRange.startTime().day
        except:
            pass
        startHour = timeRange.startTime().hour
        dayNight = self.getPeriod(timeRange)
        durationHours = int(timeRange.duration() / 3600.0 + 0.5)
        if nextDay24HourLabel:
            nextDay24Hour = durationHours >= 24 and timeRange.startTime(
            ).hour > 16
        else:
            nextDay24Hour = 0
        splitDay24Hour = splitDay24HourLabel and durationHours == 24
        # Do not say "Night" if:
        #    startHour is between midnight self.DAY (e.g. 6 am)
        nightTimeFlag = durationHours <= 12 and dayNight == self.NIGHTTIME() \
            and startHour > self.DAY()

        # Check for holiday
        if not splitDay24Hour and not todayFlag and holidays == 1 and \
           (dayNight == self.DAYTIME() or dayNight == self.DAYNIGHT()):

            if nextDay24Hour == 1:
                label = Holidays.getHolidayLabel(timeRange.endTime())
            else:
                label = Holidays.getHolidayLabel(timeRange.startTime())

            if label != "":
                if labelType == "CapitalWithPeriod":
                    label = label.upper()

                return pre + label + post

        # Check for today or tonight
        if today == 1:
            if todayFlag:
                if dayNight == self.DAYNIGHT():
                    # Key off of end time
                    keyRange = TimeRange.TimeRange(
                        timeRange.endTime() - 3600, timeRange.endTime())
                    dayNight = self.getPeriod(keyRange)
                # if durationHours == 1:
                #    label =  labels["Now"]
                if durationHours < 12 and durationHours > 1:
                    if dayNight == self.DAYTIME():
                        label = labels["Rest of Today"]
                    else:
                        label = labels["Rest of Tonight"]
                elif dayNight == self.NIGHTTIME():
                    label = labels["Tonight"]
                else:
                    label = labels["Today"]
                return pre + label + post

        # Check for tomorrow or tomorrow night
        if tomorrow == 1:
            startTime = timeRange.startTime() - 24 * 3600
            if startTime.day == currentLocalTime.day:
                if durationHours == 1:
                    label = timeRange.startTime().string() + ": "
                elif nightTimeFlag:
                    label = labels["Tomorrow"] + " " + labels["Night"]
                else:
                    label = labels["Tomorrow"]
                try:
                    if self._productIssuance == "Next Day":
                        label = "Tonight"
                except:
                    pass
                return pre + label + post

        # Week day
        weekdayName = labels["Weekday"][timeRange.startTime().weekday()]
        if durationHours == 1:
            label = self.timeDisplay(timeRange, "Zulu", "", "", "%I %p")
            if label[0] == "0":
                label = label[1:]
            if label == "5 PM":
                endLabel = " TO 6 PM"
            elif label == "5 AM":
                endLabel = " TO 6 AM"
            else:
                endLabel = ""
            label = label + endLabel + " " + weekdayName
        # Check for Night and Evening
        elif nightTimeFlag:
            if durationHours <= 6:
                label = weekdayName + " " + labels["Evening"]
            else:
                label = weekdayName + " " + labels["Night"]
        elif nextDay24Hour == 1:
            # If we have a 24 hour time period starting late in the day,
            # use the next day as the label.
            label = labels["Weekday"][timeRange.endTime().weekday()]
        elif splitDay24Hour:
            # See if we are starting with a night or day period
            weekNight = weekdayName + " " + labels["Night"]
            if weekdayName[-1].isupper():
                connector = " AND "
            else:
                connector = " and "

            if startHour < self.NIGHT():
                # Monday and Monday Night OR
                # Labor Day and Monday Night
                weekDayHoliday = self.getHolidayLabel(
                    timeRange.startTime(), holidays)
                if weekDayHoliday != "":
                    weekdayName = weekDayHoliday
                label = weekdayName + connector + weekNight
            else:
                # Sunday Night and Monday OR
                # Sunday Night and Labor Day
                nextWeekdayName = self.getHolidayLabel(
                    timeRange.endTime(), holidays)
                if nextWeekdayName == "":
                    nextWeekdayName = labels["Weekday"][
                        timeRange.endTime().weekday()]
                label = weekNight + connector + nextWeekdayName
        else:
            label = weekdayName
        # Check for Evening

        return pre + label + post

    def increment_nlValue_dict(self, tree, node):
        self.debug_print("Debug: increment_nlValue_dict in FWS.py")
        # Increment for rounding values
        # Units depend on the product
        incValDict = TextRules.TextRules.increment_nlValue_dict(
            self, tree, node)
        incValDict["Wind"] = 1
        incValDict["WindGust"] = 1
        incValDict["Wind20ft"] = 1
        incValDict["TransWind"] = 1
        incValDict["Wind1000Ft"] = 1
        incValDict["Wind2000Ft"] = 1
        incValDict["Wind3000Ft"] = 1
        incValDict["Wind4000Ft"] = 1
        incValDict["Wind5000Ft"] = 1
        incValDict["CWR"] = 1
        incValDict["QPF"] = .0001
        #incValDict["Vsby"] = .01
        incValDict["Visibility"] = .01
        incValDict["Pres"] = .01
        return incValDict

    def element_inUnits_dict(self, tree, node):
        self.debug_print("Debug: element_inUnits_dict in FWS.py")
        # Increment for rounding values
        # Units depend on the product
        inUnitsDict = TextRules.TextRules.element_inUnits_dict(
            self, tree, node)
        inUnitsDict["Visibility"] = "SM"
        inUnitsDict["Wind1000Ft"] = "kts"
        inUnitsDict["Wind2000Ft"] = "kts"
        inUnitsDict["Wind3000Ft"] = "kts"
        inUnitsDict["Wind4000Ft"] = "kts"
        inUnitsDict["Wind5000Ft"] = "kts"
        return inUnitsDict

    def scalar_difference_nlValue_dict(self, tree, node):
        self.debug_print("Debug: scalar_difference_nlValue_dict in FWS.py")
        # Scalar difference.  If the difference between scalar values
        # for 2 sub-periods is greater than this value,
        # the different values will be noted in the phrase.
        scaleDiffDict = TextRules.TextRules.scalar_difference_nlValue_dict(
            self, tree, node)
        # scaleDiffDict["Vsby"] = {
        #    (0.00,1.00) : 0.25,
        #    (1.00,3.00) : 0.50,
        #    (3.00,5.00) : 1.00,
        #    "default" : 2.00,
        #    }
        # scaleDiffDict["PredHgt"] = {
        #    (0,10) : 1,
        #    (10,30) : 5,
        #    (30,100) : 10,
        #    "default" : 25,
        #    }
        scaleDiffDict["Visibility"] = {
            (0.00, 1.00): 0.25,
            (1.00, 3.00): 0.50,
            (3.00, 5.00): 1.00,
            "default": 2.00,
        }
        scaleDiffDict["CloudBasePrimary"] = {
            (0, 10): 1,
            (10, 30): 5,
            (30, 100): 10,
            "default": 25,
        }
        scaleDiffDict["VentRate"] = {
            (0, 40000): 10000,
            (40000, 60000): 20000,
            (60000, 100000): 40000,
            (100000, 150000): 50000,
            (150000, 1000001): 50000,
            "default": 50000,
        }
        scaleDiffDict["Td"] = 5
        scaleDiffDict["PoP"] = 10
        scaleDiffDict["MixHgt"] = 10000
        scaleDiffDict["VentRate"] = 100000
        scaleDiffDict["ADI"] = 500
        return scaleDiffDict

    # From FirePhrases. Changed to eliminate the area test. Thus,
    # this label will appear even though there is no ridgetop wind.
    def fireWind_label_setUp(self, tree, node):
        self.debug_print("Debug: fireWind_label_setUp in FWS.py")
        self.setWords(node, "")
        node.set("descriptor", "")
        node.set("indentLabel", "WIND.(20 FT)........")
        return self.DONE()

    # The methods below this line override baseline
    # methods to accomodate ignition times.
    # They were derived by Tracy Hansen from code originally
    # from Matt Davis and renamed by Virgil Middendorf.
    # The last two methods were created by Matt Davis to
    # check and make time ranges for the ignition time forecasts.

    def fire_dayOrNight_words(self, tree, node):
        self.debug_print("Debug: fire_dayOrNight_words in FWS.py")
        # Temp or RH elements
        elementName = node.getAncestor("elementName")
        statDict = node.getStatDict()
        if elementName == "MaxT" or elementName == "MinT":
            stats = self.getTempStats(tree, node)
            if stats is None:
                return self.setWords(node.parent, "MISSING")
            connector = self.value_connector(
                tree, node, elementName, elementName)
            igWords = str(int(self.getValue(stats, "avg")))
            words = self.getTempRangePhrase(tree, node, stats, elementName)
        else:  # MinRH, MaxRH or RH
            stats = self.getStats(statDict, elementName)
            if stats is None:
                return self.setWords(node.parent, "MISSING")
            connector = self.value_connector(
                tree, node, elementName, elementName)
            igWords = str(int(self.getValue(stats, "avg")))
            min, max = self.getValue(stats, "MinMax")
            if min > 100:
                min = 100
            if max > 100:
                max = 100
            if min == max:
                words = str(int(min))
            else:
                words = str(int(min)) + connector + str(int(max))
        outUnits = self.element_outUnits(tree, node, elementName, elementName)
        units = self.units_descriptor(tree, node, "units", outUnits)
        words = words + units
        igWords = igWords + units

        # Add ignition element if applicable
        if self._withIgnitionTimes == "yes":
            dayNight = self.getPeriod(node.getTimeRange(), 1)
            if dayNight == self.DAYTIME():
                tempElement = "Max"
                rhElement = "Min"
            else:
                tempElement = "Min"
                rhElement = "Max"
            if elementName == "MaxT" or elementName == "MinT":
                ignitionElement = "T"
                elementType = tempElement
            else:
                ignitionElement = "RH"
                elementType = rhElement
            if self._checkFireTR(node.getTimeRange()):
                ignitionStats = tree.stats.get(
                    ignitionElement, self._fireTR, node.getAreaLabel(), mergeMethod="Max")
                if ignitionStats is not None:
                    ignitionPhrase = str(int(self.getValue(ignitionStats)))
                    reqType = self._getRequestWords()
                    words = ignitionPhrase + units + " at " + \
                        reqType + "..." + elementType + " " + igWords
                else:
                    words = elementType + " " + igWords
            else:
                words = elementType + " " + igWords
        return self.setWords(node, words)

    def fireWind_compoundPhrase(self):
        self.debug_print("Debug: fireWind_compoundPhrase in FWS.py")
        return {
            "phraseList": [
                self.wind_summary,
                # self.wind_phrase,
                self.wind_withGusts_phrase,
                self.erraticWind_phrase
            ],
            "phraseMethods": [
                self.assembleSentences,
                self.fireWind_finishUp
            ],
        }

    def fireWind_finishUp(self, tree, node):
        "Create a phrase for Winds"
        self.debug_print("Debug: fireWind_finishUp in FWS.py")
        if self.currentAreaContains(
                tree, self.ridgeValleyAreas(tree, node)) == 1:
            return self.setWords(node, "")
        words = node.get("words")
        if words is None:
            return
        if words == "":
            words = "MISSING"

        # Add ignitionTime if appropriate
        igWords = ""
        if self._checkFireTR(node.getTimeRange()):
            ignitionWindStats = tree.stats.get(
                "Wind", self._fireTR, node.getAreaLabel(), mergeMethod="Max")
            if ignitionWindStats is not None:
                igMagStr = str(int(ignitionWindStats[0]))
                igDirStr = self.vector_dir(int(ignitionWindStats[1]))
                reqType = self._getRequestWords()
                igWords = "Winds " + igDirStr + " at " + \
                    igMagStr + " mph at " + reqType + "...otherwise "

        words = igWords + words
        node.set("descriptor", "")
        if self._slopeValleyFlag:
            node.set("indentLabel", "SLOPE/VALLEY WINDS..")
        elif self._lowerSlopeFlag:
            node.set("indentLabel", "LOWER SLOPE WINDS...")
        else:
            node.set("indentLabel", "20-FOOT WINDS.......")
        node.set("compound", 1)
        return self.setWords(node, words)

    def sfcWind_compoundPhrase(self):
        self.debug_print("Debug: sfcWind_compoundPhrase in FWS.py")
        return {
            "phraseList": [
                self.wind_summary,
                self.sfcWind_withGusts_phrase,
            ],
            "phraseMethods": [
                self.consolidateSubPhrases,
                self.assembleSentences,
                self.sfcWind_finishUp
            ],
        }

    def sfcWind_withGusts_phrase(self):
        self.debug_print("Debug: sfcWind_withGusts_phrase in FWS.py")
        return {
            "setUpMethod": self.sfcWind_withGusts_setUp,
            "wordMethod": self.sfcVector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def sfcWind_withGusts_setUp(self, tree, node):
        self.debug_print("Debug: sfcWind_withGusts_setUp in FWS.py")
        node.set("indentLabel", "SURFACE WINDS.......")
        return self.wind_setUp(tree, node, gustFlag=1)

    def sfcWind_finishUp(self, tree, node):
        "Create a phrase for Winds"
        self.debug_print("Debug: sfcWind_finishUp in FWS.py")
        # Empty phrase if doing ridge/valley winds
        if self.currentAreaContains(
                tree, self.ridgeValleyAreas(tree, node)) == 1:
            return self.setWords(node, "")
        words = node.get("words")
        if words is None:
            return
        if words == "":
            words = "MISSING"
        node.set("descriptor", "")
        node.set("indentLabel", "SURFACE WINDS.......")
        node.set("compound", 1)
        return self.setWords(node, words)

    def sfcVector_words(self, tree, node):
        self.debug_print("Debug: sfcVector_words in FWS.py")
        # Create a words for a vector element
        elementInfo = node.getAncestor("firstElement")
        if elementInfo is None:
            return self.setWords(node, "")
        words = self.simple_sfcVector_phrase(tree, node, elementInfo)
        if words == "null":
            return self.setWords(node, "null")
        gustPhrase = ""
        if words != "":
            # Add gusts
            gustFlag = node.getAncestor("gustFlag")
            if gustFlag == 1:
                windStats = tree.stats.get("Wind", node.getTimeRange(), node.getAreaLabel(),
                                           mergeMethod="Max")
                if windStats is not None:
                    maxMag, dir = windStats
                    statDict = node.getStatDict()
                    gustStats = self.getStats(statDict, "WindGust")
                    subRange = node.get("timeRange")
                    gustPhrase = self.sfcEmbedded_gust_phrase(
                        tree, node, gustStats, maxMag, subRange)
        return self.setWords(node, words + gustPhrase)

    def simple_sfcVector_phrase(self, tree, node, elementInfo, checkRepeating=1):
        self.debug_print("Debug: simple_sfcVector_phrase in FWS.py")
        # Create a vector subPhrase
        # Do not repeat mag, dir if same as previous phrase
        elementName = elementInfo.name
        statDict = node.getStatDict()
        stats = self.getStats(statDict, elementName)
        if stats is None:
            return ""
        mag, dir = stats
        minMag, maxMag = self.getValue(mag, "MinMax")

        if elementName == "Wind20ft":
            windStats = tree.stats.get("Wind", node.getTimeRange(), node.getAreaLabel(),
                                       mergeMethod="MinMax")
            if windStats is not None:
                wndMag, dir = windStats
                minMag, maxMag = self.getValue(wndMag, "MinMax")

        # Save maxMag at component level for other methods to use.
        # THIS IS PARTICULARLY IMPORTANT FOR USE IN THE includeOnlyPhrases_list def
        # below to eliminate certainly wx elements during tropical cyclone
        # situations when certain conditions are met.
        component = node.getComponent()
        maxMagList = component.get("maxMagList")
        if maxMagList is None:
            maxMagList = [maxMag]
        else:
            maxMagList.append(maxMag)
        component.set("maxMagList", maxMagList)

        words = self.sfcVector_mag(tree, node, minMag, maxMag,
                                   elementInfo.outUnits, elementName)
        if words == "null":
            return words
        magStr = words
        dirStr = self.vector_dir(dir)

        if checkRepeating:
            # Set for future reference
            node.set("dirStr", dirStr)
            node.set("magStr", magStr)
            node.set("minMag", minMag)
            node.set("maxMag", maxMag)
            if minMag == 0.0:
                minMag = maxMag
            # Check for repeating mag or dir
            prevNode = node.getPrev()
            if prevNode is not None:
                prevDirStr = prevNode.get("dirStr")
                prevMagStr = prevNode.get("magStr")
                prevMin = prevNode.get("minMag")
                prevMax = prevNode.get("maxMag")
                if prevMin == 0.0:
                    prevMin = prevMax
                if prevMin is None or prevMax is None or \
                   prevDirStr is None or prevMagStr is None:
                    pass
                elif prevDirStr == dirStr and prevMagStr == magStr:
                    pass
                elif prevDirStr == dirStr:
                    dirStr = ""
                elif prevMagStr == magStr:
                    magStr = ""
                # Prevent "around 10 becoming 5 to 10"
                #         "around 10 becoming 10 to 15"
                elif prevMin == prevMax:
                    if (minMag == prevMax - 5.0) or (maxMag == prevMax + 5.0):
                        magStr = ""
                # Prevent "5 to 10 becoming around 10"
                #         "10 to 15 becoming around 10"
                elif minMag == maxMag:
                    if (prevMin == maxMag - 5.0) or (prevMax == maxMag + 5.0):
                        magStr = ""
        words = dirStr + self.format(magStr)
        return words.lstrip()

    def sfcVector_mag(self, tree, node, minMag, maxMag, units,
                      elementName="Wind"):
        "Create a phrase for a Range of magnitudes"
        self.debug_print("Debug: sfcVector_mag in FWS.py")

        # Check for "null" value (below threshold)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, elementName, elementName), maxMag)
        if maxMag < threshold:
            return "null"

        # Apply max reported threshold
        maxReportedMag = self.maxReported_threshold(
            tree, node, elementName, elementName)
        if maxMag >= maxReportedMag:
            maxMag = maxReportedMag
            #minMag = 0

        units = self.units_descriptor(tree, node, "units", units)

        if elementName == "Wind" or elementName == "Wind20ft":
            if self._incidentType == "MARINE":
                units = "knots"
                # Must undo some conversions on the winds to get them in knots.
                minMag = int(minMag / self._windAdjustmentFactor / 1.15 + 0.5)
                maxMag = int(maxMag / self._windAdjustmentFactor / 1.15 + 0.5)
                return self.marine_wind_mag(tree, node, minMag, maxMag, units, elementName)
            else:
                minMag = int(minMag / self._windAdjustmentFactor + 0.5)
                maxMag = int(maxMag / self._windAdjustmentFactor + 0.5)

        # Check for SingleValue
        if maxMag == minMag:  # or minMag == 0:
            around = self.addSpace(
                self.phrase_descriptor(tree, node, "around", elementName))
            words = around + str(int(maxMag)) + " " + units
        else:
            if int(minMag) < threshold:
                upTo = self.addSpace(
                    self.phrase_descriptor(tree, node, "up to", elementName))
                words = upTo + str(int(maxMag)) + " " + units
            else:
                valueConnector = self.value_connector(
                    tree, node, elementName, elementName)
                words = str(int(minMag)) + valueConnector + \
                    str(int(maxMag)) + " " + units

        # This is an additional hook for customizing the magnitude wording
        words = self.vector_mag_hook(
            tree, node, minMag, maxMag, units, elementName, words)
        return words

    def sfcEmbedded_gust_phrase(self, tree, node, gustStats, maxWind, subRange):
        self.debug_print("Debug: sfcEmbedded_gust_phrase in FWS.py")
        # Determine what type of gust phrase to add. Day and night are treated
        # differently with gusts phrases toned down a bit for night.
        self.debug_print("indentLabel in embedded_gust_phrase")
        self.debug_print(node.get("indentLabel"))
        self.debug_print("name in embedded_gust_phrase")
        self.debug_print(node.get("name"))
        try:
            includeTropical = self._includeTropical
        except:
            includeTropical = False
        if includeTropical:
            # Use the moderatedMinMax from the Tropical components
            statLabel = ""
        else:
            statLabel = "vectorMinMax"
        gusts = None
        if gustStats is None:
            # If useWindForGusts_flag is set, use max Wind for reporting gusts
            if self.useWindsForGusts_flag(tree, node) == 1:
                windStats = tree.stats.get(
                    "Wind", subRange, node.getAreaLabel(), statLabel=statLabel,
                    mergeMethod="Max")
                if windStats is None:
                    return ""
                else:
                    gusts, dir = windStats
        else:
            gusts = self.getValue(gustStats, "Max")
        if gusts is None:
            return ""

        if includeTropical:
            # Round gusts and maxWind to the nearest 5 kt regardless of users'
            # overrides
            gusts = self.round(gusts, 'Nearest', 5.0)
            maxWind = self.round(maxWind, 'Nearest', 5.0)

        threshold = self.nlValue(
            self.null_nlValue(tree, node, "WindGust", "WindGust"), gusts)
        if gusts < threshold:
            return ""
        gustPhrase = ""
        outUnits = self.element_outUnits(tree, node, "WindGust", "WindGust")
        units = self.units_descriptor(tree, node, "units", outUnits)
        windDifference = self.nlValue(
            self.gust_wind_difference_nlValue(tree, node), maxWind)
        if gusts - maxWind > windDifference:
            if self._incidentType == "MARINE":
                units = "knots"
                gustPhrase = " with gusts to around " + \
                    str(int(gusts / 1.15 + 0.5)) + " " + units
            else:
                gustPhrase = " with gusts to around " + \
                    str(int(gusts)) + " " + units
        return gustPhrase

    def sfcKtsWind_compoundPhrase(self):
        self.debug_print("Debug: sfcKtsWind_compoundPhrase in FWS.py")
        return {
            "phraseList": [
                self.wind_summary,
                self.sfcKtsWind_withGusts_phrase,
            ],
            "phraseMethods": [
                self.consolidateSubPhrases,
                self.assembleSentences,
                self.sfcKtsWind_finishUp
            ],
        }

    def sfcKtsWind_withGusts_phrase(self):
        self.debug_print("Debug: sfcKtsWind_withGusts_phrase in FWS.py")
        return {
            "setUpMethod": self.sfcKtsWind_withGusts_setUp,
            "wordMethod": self.sfcKtsVector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def sfcKtsWind_withGusts_setUp(self, tree, node):
        self.debug_print("Debug: sfcKtsWind_withGusts_setUp in FWS.py")
        node.set("indentLabel", "SURFACE WINDS KTS...")
        return self.wind_setUp(tree, node, gustFlag=1)

    def sfcKtsWind_finishUp(self, tree, node):
        "Create a phrase for Winds"
        self.debug_print("Debug: sfcKtsWind_finishUp in FWS.py")
        # Empty phrase if doing ridge/valley winds
        if self.currentAreaContains(
                tree, self.ridgeValleyAreas(tree, node)) == 1:
            return self.setWords(node, "")
        words = node.get("words")
        if words is None:
            return
        if words == "":
            words = "MISSING"
        node.set("descriptor", "")
        node.set("indentLabel", "SURFACE WINDS KTS...")
        node.set("compound", 1)
        return self.setWords(node, words)

    def sfcKtsVector_words(self, tree, node):
        self.debug_print("Debug: sfcKtsVector_words in FWS.py")
        # Create a words for a vector element
        elementInfo = node.getAncestor("firstElement")
        if elementInfo is None:
            return self.setWords(node, "")
        words = self.simple_sfcKtsVector_phrase(tree, node, elementInfo)
        self.debug_print("sfcKtsVector_words words")
        self.debug_print(words)
        if words == "null":
            return self.setWords(node, "null")
        gustPhrase = ""
        if words != "":
            # Add gusts
            gustFlag = node.getAncestor("gustFlag")
            self.debug_print("gustFlag")
            self.debug_print(gustFlag)
            if gustFlag == 1:
                windStats = tree.stats.get("Wind", node.getTimeRange(), node.getAreaLabel(),
                                           mergeMethod="Max")
                if windStats is not None:
                    maxMag, dir = windStats
                    statDict = node.getStatDict()
                    gustStats = self.getStats(statDict, "WindGust")
                    # gustStats = tree.stats.get("WindGust", node.getTimeRange(), node.getAreaLabel(),
                    #                         mergeMethod="Max")
                    subRange = node.get("timeRange")
                    self.debug_print("a")
                    self.debug_print(gustStats)
                    self.debug_print(maxMag)
                    self.debug_print(subRange)
                    self.debug_print("b")
                    gustPhrase = self.sfcKtsEmbedded_gust_phrase(
                        tree, node, gustStats, maxMag, subRange)
                    self.debug_print(gustPhrase)
        return self.setWords(node, words + gustPhrase)

    def simple_sfcKtsVector_phrase(self, tree, node, elementInfo, checkRepeating=1):
        self.debug_print("Debug: sfcKtsVector_phrase in FWS.py")
        # Create a vector subPhrase
        # Do not repeat mag, dir if same as previous phrase
        elementName = elementInfo.name
        statDict = node.getStatDict()
        stats = self.getStats(statDict, elementName)
        if stats is None:
            return ""
        mag, dir = stats
        minMag, maxMag = self.getValue(mag, "MinMax")

        if elementName == "Wind20ft":
            windStats = tree.stats.get("Wind", node.getTimeRange(), node.getAreaLabel(),
                                       mergeMethod="MinMax")
            if windStats is not None:
                wndMag, dir = windStats
                minMag, maxMag = self.getValue(wndMag, "MinMax")

        # Save maxMag at component level for other methods to use.
        # THIS IS PARTICULARLY IMPORTANT FOR USE IN THE includeOnlyPhrases_list def
        # below to eliminate certainly wx elements during tropical cyclone
        # situations when certain conditions are met.
        component = node.getComponent()
        maxMagList = component.get("maxMagList")
        if maxMagList is None:
            maxMagList = [maxMag]
        else:
            maxMagList.append(maxMag)
        component.set("maxMagList", maxMagList)

        words = self.sfcKtsVector_mag(tree, node, minMag, maxMag,
                                      elementInfo.outUnits, elementName)
        if words == "null":
            return words
        magStr = words
        dirStr = self.vector_dir(dir)

        if checkRepeating:
            # Set for future reference
            node.set("dirStr", dirStr)
            node.set("magStr", magStr)
            node.set("minMag", minMag)
            node.set("maxMag", maxMag)
            if minMag == 0.0:
                minMag = maxMag
            # Check for repeating mag or dir
            prevNode = node.getPrev()
            if prevNode is not None:
                prevDirStr = prevNode.get("dirStr")
                prevMagStr = prevNode.get("magStr")
                prevMin = prevNode.get("minMag")
                prevMax = prevNode.get("maxMag")
                if prevMin == 0.0:
                    prevMin = prevMax
                if prevMin is None or prevMax is None or \
                   prevDirStr is None or prevMagStr is None:
                    pass
                elif prevDirStr == dirStr and prevMagStr == magStr:
                    pass
                elif prevDirStr == dirStr:
                    dirStr = ""
                elif prevMagStr == magStr:
                    magStr = ""
                # Prevent "around 10 becoming 5 to 10"
                #         "around 10 becoming 10 to 15"
                elif prevMin == prevMax:
                    if (minMag == prevMax - 5.0) or (maxMag == prevMax + 5.0):
                        magStr = ""
                # Prevent "5 to 10 becoming around 10"
                #         "10 to 15 becoming around 10"
                elif minMag == maxMag:
                    if (prevMin == maxMag - 5.0) or (prevMax == maxMag + 5.0):
                        magStr = ""
        words = dirStr + self.format(magStr)
        return words.lstrip()

    def sfcKtsVector_mag(self, tree, node, minMag, maxMag, units,
                         elementName="Wind"):
        "Create a phrase for a Range of magnitudes"
        self.debug_print("Debug: sfcKtsVector_mag in FWS.py")

        # Check for "null" value (below threshold)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, elementName, elementName), maxMag)
        if maxMag < threshold:
            return "null"

        # Apply max reported threshold
        maxReportedMag = self.maxReported_threshold(
            tree, node, elementName, elementName)
        if maxMag >= maxReportedMag:
            maxMag = maxReportedMag
            #minMag = 0

        units = self.units_descriptor(tree, node, "units", units)

        self.debug_print("*** wind element name ***")
        self.debug_print(elementName)

        units = "knots"
        # Must undo some conversions on the winds to get them in knots.
        minMag = int(minMag / self._windAdjustmentFactor / 1.15 + 0.5)
        maxMag = int(maxMag / self._windAdjustmentFactor / 1.15 + 0.5)

        # Check for SingleValue
        if maxMag == minMag:  # or minMag == 0:
            around = self.addSpace(
                self.phrase_descriptor(tree, node, "around", elementName))
            words = around + str(int(maxMag)) + " " + units
        else:
            if int(minMag) < threshold:
                upTo = self.addSpace(
                    self.phrase_descriptor(tree, node, "up to", elementName))
                words = upTo + str(int(maxMag)) + " " + units
            else:
                valueConnector = self.value_connector(
                    tree, node, elementName, elementName)
                words = str(int(minMag)) + valueConnector + \
                    str(int(maxMag)) + " " + units

        # This is an additional hook for customizing the magnitude wording
        words = self.vector_mag_hook(
            tree, node, minMag, maxMag, units, elementName, words)
        return words

    def sfcKtsEmbedded_gust_phrase(self, tree, node, gustStats, maxWind, subRange):
        self.debug_print("Debug: sfcKtsEmbedded_gust_phrase in FWS.py")
        # Determine what type of gust phrase to add. Day and night are treated
        # differently with gusts phrases toned down a bit for night.
        try:
            includeTropical = self._includeTropical
        except:
            includeTropical = False
        if includeTropical:
            # Use the moderatedMinMax from the Tropical components
            statLabel = ""
        else:
            statLabel = "vectorMinMax"
        gusts = None
        if gustStats is None:
            # If useWindForGusts_flag is set, use max Wind for reporting gusts
            if self.useWindsForGusts_flag(tree, node) == 1:
                windStats = tree.stats.get(
                    "Wind", subRange, node.getAreaLabel(), statLabel=statLabel,
                    mergeMethod="Max")
                if windStats is None:
                    return ""
                else:
                    gusts, dir = windStats
        else:
            gusts = self.getValue(gustStats, "Max")
        if gusts is None:
            return ""

        if includeTropical:
            # Round gusts and maxWind to the nearest 5 kt regardless of users'
            # overrides
            gusts = self.round(gusts, 'Nearest', 5.0)
            maxWind = self.round(maxWind, 'Nearest', 5.0)

        threshold = self.nlValue(
            self.null_nlValue(tree, node, "WindGust", "WindGust"), gusts)
        if gusts < threshold:
            return ""
        gustPhrase = ""
        outUnits = self.element_outUnits(tree, node, "WindGust", "WindGust")
        units = self.units_descriptor(tree, node, "units", outUnits)
        windDifference = self.nlValue(
            self.gust_wind_difference_nlValue(tree, node), maxWind)
        if gusts - maxWind > windDifference:
            units = "knots"
            gustPhrase = " with gusts to around " + \
                str(int(gusts / 1.15 + 0.5)) + " " + units
        return gustPhrase

    # For EYE LEVEL WINDS
    def fireEyeWind_compoundPhrase(self):
        self.debug_print("Debug: fireEyeWind_compoundPhrase in FWS.py")
        return {
            "phraseList": [
                self.wind_summary,
                self.eyeWind_withGusts_phrase,
            ],
            "phraseMethods": [
                self.consolidateSubPhrases,
                self.assembleSentences,
                self.eyeWind_finishUp
            ],
        }

    def eyeWind_withGusts_phrase(self):
        self.debug_print("Debug: eyeWind_withGusts_phrase in FWS.py")
        return {
            "setUpMethod": self.eyeWind_withGusts_setUp,
            "wordMethod": self.eyeVector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def eyeWind_withGusts_setUp(self, tree, node):
        self.debug_print("Debug: eyeWind_withGusts_setUp in FWS.py")
        node.set("indentLabel", "EYE LEVEL WINDS.....")
        return self.wind_setUp(tree, node, gustFlag=1)

    def eyeWind_finishUp(self, tree, node):
        "Create a phrase for Winds"
        self.debug_print("Debug: eyeWind_withGusts_finishUp in FWS.py")
        # Empty phrase if doing ridge/valley winds
        if self.currentAreaContains(
                tree, self.ridgeValleyAreas(tree, node)) == 1:
            return self.setWords(node, "")
        words = node.get("words")
        if words is None:
            return
        if words == "":
            words = "MISSING"
        node.set("descriptor", "")
        node.set("indentLabel", "EYE LEVEL WINDS.....")
        node.set("compound", 1)
        return self.setWords(node, words)

    def eyeVector_words(self, tree, node):
        self.debug_print("Debug: eyeVector_words in FWS.py")
        # Create a words for a vector element
        elementInfo = node.getAncestor("firstElement")
        if elementInfo is None:
            return self.setWords(node, "")
        words = self.simple_eyeVector_phrase(tree, node, elementInfo)
        if words == "null":
            return self.setWords(node, "null")
        gustPhrase = ""
        if words != "":
            # Add gusts
            gustFlag = node.getAncestor("gustFlag")
            if gustFlag == 1:
                windStats = tree.stats.get("Wind", node.getTimeRange(), node.getAreaLabel(),
                                           mergeMethod="Max")
                if windStats is not None:
                    maxMag, dir = windStats
                    statDict = node.getStatDict()
                    gustStats = self.getStats(statDict, "WindGust")
                    subRange = node.get("timeRange")
                    gustPhrase = self.eyeEmbedded_gust_phrase(
                        tree, node, gustStats, maxMag, subRange)
        return self.setWords(node, words + gustPhrase)

    def simple_eyeVector_phrase(self, tree, node, elementInfo, checkRepeating=1):
        self.debug_print("Debug: simple_eyeVector_phrase in FWS.py")
        # Create a vector subPhrase
        # Do not repeat mag, dir if same as previous phrase
        elementName = elementInfo.name
        statDict = node.getStatDict()
        stats = self.getStats(statDict, elementName)
        if stats is None:
            return ""
        mag, dir = stats
        minMag, maxMag = self.getValue(mag, "MinMax")

        if elementName == "Wind20ft":
            windStats = tree.stats.get("Wind", node.getTimeRange(), node.getAreaLabel(),
                                       mergeMethod="MinMax")
            if windStats is not None:
                wndMag, dir = windStats
                minMag, maxMag = self.getValue(wndMag, "MinMax")

        # Save maxMag at component level for other methods to use.
        # THIS IS PARTICULARLY IMPORTANT FOR USE IN THE includeOnlyPhrases_list def
        # below to eliminate certainly wx elements during tropical cyclone
        # situations when certain conditions are met.
        component = node.getComponent()
        maxMagList = component.get("maxMagList")
        if maxMagList is None:
            maxMagList = [maxMag]
        else:
            maxMagList.append(maxMag)
        component.set("maxMagList", maxMagList)

        words = self.eyeVector_mag(tree, node, minMag, maxMag,
                                   elementInfo.outUnits, elementName)
        if words == "null":
            return words
        magStr = words
        dirStr = self.vector_dir(dir)

        if checkRepeating:
            # Set for future reference
            node.set("dirStr", dirStr)
            node.set("magStr", magStr)
            node.set("minMag", minMag)
            node.set("maxMag", maxMag)
            if minMag == 0.0:
                minMag = maxMag
            # Check for repeating mag or dir
            prevNode = node.getPrev()
            if prevNode is not None:
                prevDirStr = prevNode.get("dirStr")
                prevMagStr = prevNode.get("magStr")
                prevMin = prevNode.get("minMag")
                prevMax = prevNode.get("maxMag")
                if prevMin == 0.0:
                    prevMin = prevMax
                if prevMin is None or prevMax is None or \
                   prevDirStr is None or prevMagStr is None:
                    pass
                elif prevDirStr == dirStr and prevMagStr == magStr:
                    pass
                elif prevDirStr == dirStr:
                    dirStr = ""
                elif prevMagStr == magStr:
                    magStr = ""
                # Prevent "around 10 becoming 5 to 10"
                #         "around 10 becoming 10 to 15"
                elif prevMin == prevMax:
                    if (minMag == prevMax - 5.0) or (maxMag == prevMax + 5.0):
                        magStr = ""
                # Prevent "5 to 10 becoming around 10"
                #         "10 to 15 becoming around 10"
                elif minMag == maxMag:
                    if (prevMin == maxMag - 5.0) or (prevMax == maxMag + 5.0):
                        magStr = ""
        words = dirStr + self.format(magStr)
        return words.lstrip()

    def eyeVector_mag(self, tree, node, minMag, maxMag, units,
                      elementName="Wind"):
        "Create a phrase for a Range of magnitudes"
        self.debug_print("Debug: eyeVector_mag in FWS.py")

        # Check for "null" value (below threshold)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, elementName, elementName), maxMag)
        if maxMag < threshold:
            return "null"

        # Apply max reported threshold
        maxReportedMag = self.maxReported_threshold(
            tree, node, elementName, elementName)
        if maxMag >= maxReportedMag:
            maxMag = maxReportedMag
            #minMag = 0

        units = self.units_descriptor(tree, node, "units", units)

        if elementName == "Wind" or elementName == "Wind20ft":
            if self._incidentType == "MARINE":
                units = "knots"
                # Must undo some conversions on the winds to get them in knots.
                minMag = int(
                    minMag * self._eyeWindAdjustmentFactor / self._windAdjustmentFactor / 1.15 + 0.5)
                maxMag = int(
                    maxMag * self._eyeWindAdjustmentFactor / self._windAdjustmentFactor / 1.15 + 0.5)
                return self.marine_wind_mag(tree, node, minMag, maxMag, units, elementName)
            else:
                minMag = int(
                    minMag * self._eyeWindAdjustmentFactor / self._windAdjustmentFactor + 0.5)
                maxMag = int(
                    maxMag * self._eyeWindAdjustmentFactor / self._windAdjustmentFactor + 0.5)

        # Check for SingleValue
        if maxMag == minMag:  # or minMag == 0:
            around = self.addSpace(
                self.phrase_descriptor(tree, node, "around", elementName))
            words = around + str(int(maxMag)) + " " + units
        else:
            if int(minMag) < threshold:
                upTo = self.addSpace(
                    self.phrase_descriptor(tree, node, "up to", elementName))
                words = upTo + str(int(maxMag)) + " " + units
            else:
                valueConnector = self.value_connector(
                    tree, node, elementName, elementName)
                words = str(int(minMag)) + valueConnector + \
                    str(int(maxMag)) + " " + units

        # This is an additional hook for customizing the magnitude wording
        words = self.vector_mag_hook(
            tree, node, minMag, maxMag, units, elementName, words)
        return words

    def marine_wind_mag(self, tree, node, minMag, maxMag, units, elementName):
        self.debug_print("Debug: marine_wind_mag in FWS.py")
        # Produce marine descriptor wording such as "storm force", "gales"
        specialDescriptor = 0
        prevSpecial = None
        if node.getIndex() > 0 and self.marine_wind_verbose_flag(tree, node) == 0:
            # Check for previous descriptor
            prevSpecial = node.getPrev().get("specialDescriptor")
        # Check for special descriptors
        windWordList = [(64, "hurricane force winds to"),
                        (45, "storm force winds to"),
                        (34, "gales to"),
                        ]
        for threshold, windWords in windWordList:
            if maxMag >= threshold:
                descriptor = self.addSpace(
                    self.phrase_descriptor(tree, node, windWords, elementName))
                if descriptor == prevSpecial:
                    descriptor = ""
                words = descriptor + str(int(maxMag)) + " " + units
                specialDescriptor = 1
                break

        if not specialDescriptor:
            if maxMag > 25:
                descriptor = self.addSpace(
                    self.phrase_descriptor(tree, node, "up to", elementName))
                words = descriptor + str(int(maxMag)) + " " + units
            else:
                if minMag == maxMag or minMag == 0:
                    around = self.addSpace(
                        self.phrase_descriptor(tree, node, "around", elementName))
                    words = around + str(int(maxMag)) + " " + units
                else:
                    valueConnector = self.value_connector(
                        tree, node, elementName, elementName)
                    words = str(int(minMag)) + valueConnector + \
                        str(int(maxMag)) + " " + units
        else:
            # If special marine descriptor is included in the resulting
            # words for the first subPhrase, turn off the phrase descriptor
            if node.getIndex() == 0:
                node.getParent().set("descriptor", "")
            node.set("specialDescriptor", descriptor)
        self.debug_print("Marine Phrase")
        self.debug_print(words)
        return words

    def eyeEmbedded_gust_phrase(self, tree, node, gustStats, maxWind, subRange):
        self.debug_print("Debug: eyeEmbedded_gust_phrase in FWS.py")
        # Determine what type of gust phrase to add. Day and night are treated
        # differently with gusts phrases toned down a bit for night.
        try:
            includeTropical = self._includeTropical
        except:
            includeTropical = False
        if includeTropical:
            # Use the moderatedMinMax from the Tropical components
            statLabel = ""
        else:
            statLabel = "vectorMinMax"
        gusts = None
        if gustStats is None:
            # If useWindForGusts_flag is set, use max Wind for reporting gusts
            if self.useWindsForGusts_flag(tree, node) == 1:
                windStats = tree.stats.get(
                    "Wind", subRange, node.getAreaLabel(), statLabel=statLabel,
                    mergeMethod="Max")
                if windStats is None:
                    return ""
                else:
                    gusts, dir = windStats
        else:
            gusts = self.getValue(gustStats, "Max")
        if gusts is None:
            return ""

        if includeTropical:
            # Round gusts and maxWind to the nearest 5 kt regardless of users'
            # overrides
            gusts = self.round(gusts, 'Nearest', 5.0)
            maxWind = self.round(maxWind, 'Nearest', 5.0)

        threshold = self.nlValue(
            self.null_nlValue(tree, node, "WindGust", "WindGust"), gusts)
        if gusts < threshold:
            return ""
        gustPhrase = ""
        outUnits = self.element_outUnits(tree, node, "WindGust", "WindGust")
        units = self.units_descriptor(tree, node, "units", outUnits)
        windDifference = self.nlValue(
            self.gust_wind_difference_nlValue(tree, node), maxWind)
        if gusts - maxWind > windDifference:
            if self._incidentType == "MARINE":
                units = "knots"
                gustPhrase = " with gusts to around " + \
                    str(int(
                        gusts * self._eyeWindAdjustmentFactor / 1.15 + 0.5)) + " " + units
            else:
                gustPhrase = " with gusts to around " + \
                    str(int(gusts * self._eyeWindAdjustmentFactor + 0.5)) + \
                    " " + units
        return gustPhrase

    def vector_mag(self, tree, node, minMag, maxMag, units,
                   elementName="Wind"):
        "Create a phrase for a Range of magnitudes"
        self.debug_print("Debug: vector_mag in FWS.py")

        # Check for "null" value (below threshold)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, elementName, elementName), maxMag)
        if maxMag < threshold:
            return "null"

        # Apply max reported threshold
        maxReportedMag = self.maxReported_threshold(
            tree, node, elementName, elementName)
        if maxMag >= maxReportedMag:
            maxMag = maxReportedMag
            #minMag = 0

        units = self.units_descriptor(tree, node, "units", units)

        if elementName == "Wind" or elementName == "Wind20ft":
            if self._incidentType == "MARINE":
                units = "knots"
                # Must undo some conversions on the winds to get them in knots.
                minMag = int(minMag / 1.15 + 0.5)
                maxMag = int(maxMag / 1.15 + 0.5)
                return self.marine_wind_mag(tree, node, minMag, maxMag, units, elementName)

        # Check for SingleValue
        if maxMag == minMag:  # or minMag == 0:
            around = self.addSpace(
                self.phrase_descriptor(tree, node, "around", elementName))
            words = around + str(int(maxMag)) + " " + units
        else:
            if int(minMag) < threshold:
                upTo = self.addSpace(
                    self.phrase_descriptor(tree, node, "up to", elementName))
                words = upTo + str(int(maxMag)) + " " + units
            else:
                valueConnector = self.value_connector(
                    tree, node, elementName, elementName)
                words = str(int(minMag)) + valueConnector + \
                    str(int(maxMag)) + " " + units

        # This is an additional hook for customizing the magnitude wording
        words = self.vector_mag_hook(
            tree, node, minMag, maxMag, units, elementName, words)
        return words

    def embedded_gust_phrase(self, tree, node, gustStats, maxWind, subRange):
        self.debug_print("Debug: embedded_gust_phrase in FWS.py")
        # Determine what type of gust phrase to add. Day and night are treated
        # differently with gusts phrases toned down a bit for night.
        self.debug_print("indentLabel in embedded_gust_phrase")
        self.debug_print(node.get("indentLabel"))
        self.debug_print("name in embedded_gust_phrase")
        self.debug_print(node.get("name"))
        try:
            includeTropical = self._includeTropical
        except:
            includeTropical = False
        if includeTropical:
            # Use the moderatedMinMax from the Tropical components
            statLabel = ""
        else:
            statLabel = "vectorMinMax"
        gusts = None
        if gustStats is None:
            # If useWindForGusts_flag is set, use max Wind for reporting gusts
            if self.useWindsForGusts_flag(tree, node) == 1:
                windStats = tree.stats.get(
                    "Wind", subRange, node.getAreaLabel(), statLabel=statLabel,
                    mergeMethod="Max")
                if windStats is None:
                    return ""
                else:
                    gusts, dir = windStats
        else:
            gusts = self.getValue(gustStats, "Max")
        if gusts is None:
            return ""

        if includeTropical:
            # Round gusts and maxWind to the nearest 5 kt regardless of users'
            # overrides
            gusts = self.round(gusts, 'Nearest', 5.0)
            maxWind = self.round(maxWind, 'Nearest', 5.0)

        threshold = self.nlValue(
            self.null_nlValue(tree, node, "WindGust", "WindGust"), gusts)
        if gusts < threshold:
            return ""
        gustPhrase = ""
        outUnits = self.element_outUnits(tree, node, "WindGust", "WindGust")
        units = self.units_descriptor(tree, node, "units", outUnits)
        windDifference = self.nlValue(
            self.gust_wind_difference_nlValue(tree, node), maxWind)
        if gusts - maxWind > windDifference:
            if self._incidentType == "MARINE":
                units = "knots"
                gustPhrase = " with gusts to around " + \
                    str(int(gusts * self._windAdjustmentFactor / 1.15 + 0.5)
                        ) + " " + units
            else:
                gustPhrase = " with gusts to around " + \
                    str(int(gusts * self._windAdjustmentFactor + 0.5)) + \
                    " " + units
        return gustPhrase

    def erraticWind_phrase(self):
        self.debug_print("Debug: erraticWind_phrase in FWS.py")
        return {
            "setUpMethod": self.erraticWind_setUp,
            "wordMethod": self.erraticWind_words,
            "phraseMethods": [
                self.preProcessWx,
                self.combineWords,
                self.fillNulls,
                self.timeDescriptorModeration,
                self.assembleSubPhrases,
                self.postProcessPhrase,
            ],
        }

    def erraticWind_setUp(self, tree, node):
        self.debug_print("Debug: erraticWind_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("Wx", "List", self.WEATHER())]
        self.subPhraseSetUp(tree, node, elementInfoList, self.wxConnector)
        # Set this flag used by the "checkWeatherSimilarity" method
        node.set("noIntensityCombining", 1)
        self.determineSevereTimeDescriptors(tree, node)
        return self.DONE()

    def erraticWind_words(self, tree, node):
        self.debug_print("Debug: erraticWind_words in FWS.py")
        # If T is in the Wx grids, then produce phrase.
        # Wx Statistics: rankedWx

        statDict = node.getStatDict()
        rankList = self.getStats(statDict, "Wx")
        if rankList is None or len(rankList) == 0:
            return self.setWords(node, "")
        # Check against PoP
        #rankList = self.checkPoP(tree, node, rankList)
        subkeyList = self.getSubkeys(rankList)

        severe = 0
        thunder = 0
        attrTextList = []
        for subkey in subkeyList:
            wxType = subkey.wxType()
            if wxType == "T":
                thunder = 1
                intensity = subkey.intensity()
                if intensity == "+":
                    severe = 1
                wxDef = subkey.wxDef()
                for attr in subkey.attributes():
                    if attr in ["Primary", "Mention", "Dry"]:
                        continue
                    attrText = wxDef.attributeDesc(
                        subkey.wxType(), attr).lower()
                    if attrText not in attrTextList:
                        attrTextList.append(attrText)

        if thunder == 0:
            return self.setWords(node, "")
        words = self.phrase_descriptor(tree, node, "erraticWind", "Wx")

        return self.setWords(node, words)

    def smokeDispersal_phrase(self):
        self.debug_print("Debug: smokeDispersal_phrase in FWS.py")
        return {
            "setUpMethod": self.smokeDispersal_setUp,
            "wordMethod": self.smokeDispersal_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def smokeDispersal_setUp(self, tree, node):
        self.debug_print("Debug: smokeDispersal_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("VentRate", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "SMOKE DISPERSAL.....")
        return self.DONE()

    def smokeDispersal_words(self, tree, node):
        "Create phrase for Smoke Dispersal"
        self.debug_print("Debug: smokeDispersal_words in FWS.py")
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "VentRate")
        if stats is None:
            return self.setWords(node.parent, "MISSING")

        dayNight = self.getPeriod(node.getTimeRange(), 1)
        if self._checkFireTR(node.getTimeRange()):
            # Handle phrase if including ignition time
            minVal, maxVal = self.getValue(stats, "MinMax")
            if dayNight == self.DAYTIME():
                vr = int(maxVal)
                ventType = "Max"
                mergeMethod = "Max"
            else:
                vr = int(minVal)
                ventType = "Min"
                mergeMethod = "Min"
            vrCat = self.smokeDispersal_valueStr(vr)
            words = ventType + "..." + vrCat + \
                " " + " /" + str(vr) + " knot-ft/"
            reqType = self._getRequestWords()
            ignitionDispersal = tree.stats.get(
                "VentRate", self._fireTR, node.getAreaLabel(), mergeMethod=mergeMethod)
            vrCat = self.smokeDispersal_valueStr(ignitionDispersal)
            igWords = vrCat + " /" + \
                str(int(ignitionDispersal)) + \
                " knot-ft/ at " + reqType + ". \n"
            words = igWords + " " + words
        else:
            # Handle phrase with range if not including ignition time
            vr1, vr2 = self.getValue(stats, "MinMax")
            vr1 = int(vr1)
            vr2 = int(vr2)
            vrCat1 = self.smokeDispersal_valueStr(vr1)
            vrCat2 = self.smokeDispersal_valueStr(vr2)
            # Single Value input
            if vrCat1 == vrCat2:
                words = vrCat1 + " (" + str(vr1) + " knot-ft)"
            # Range
            else:
                if dayNight == self.DAYTIME():
                    words = vrCat1 + " (" + str(vr1) + " knot-ft) early in the morning increasing to " + \
                        vrCat2 + " (" + str(vr2) + " knot-ft) in the afternoon"
                else:
                    words = vrCat2 + " (" + str(vr2) + " knot-ft) early in the evening decreasing to " + \
                        vrCat1 + \
                        " (" + str(vr1) + " knot-ft) late in the night"
        return self.setWords(node, words)

    #  SMOKE DISPERSAL CATEGORIES
    def smokeDispersal_valueStr(self, value):
        "Convert smoke dispersal value to corresponding category"
        self.debug_print("Debug: smokeDispersal_valueStr in FWS.py")

        self.debug_print("self._wfoID: " + self._wfoID)
        if self._wfoID in ["LMK", "JKL", "ILM", "MHX", "RAH"]:
            self.debug_print("Running new conversion.")
            if value < 29000:
                return "poor"
            elif value >= 29000 and value < 38000:
                return "marginal"
            elif value >= 38000 and value < 50000:
                return "fair"
            elif value >= 50000 and value < 95000:
                return "good"
            elif value >= 95000:
                return "excellent"
        else:
            if value < 13000:
                return "poor"
            elif value >= 13000 and value < 30000:
                return "fair"
            elif value >= 30000 and value < 60000:
                return "good"
            elif value >= 60000:
                return "excellent"

    def transportWind_phrase(self):
        self.debug_print("Debug: transportWind_phrase in FWS.py")
        return {
            "setUpMethod": self.transportWind_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def transportWind_setUp(self, tree, node):
        self.debug_print("Debug: transportWind_setUp in FWS.py")
        self.wind_setUp(tree, node, gustFlag=0, element="TransWind")
        node.set("descriptor", "")
        node.set("indentLabel", "TRANSPORT WINDS.....")
        return self.DONE()

    def transportWindMetric_phrase(self):
        self.debug_print("Debug: transportWindMetric_phrase in FWS.py")
        return {
            "setUpMethod": self.transportWindMetric_setUp,
            "wordMethod": self.vectorMetric_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def transportWindMetric_setUp(self, tree, node):
        self.debug_print("Debug: transportWindMetric_setUp in FWS.py")
        self.windMetric_setUp(tree, node, gustFlag=0, element="TransWind")
        node.set("descriptor", "")
        node.set("indentLabel", "TRANSPORT WINDS M/S.")
        return self.DONE()

    def mixingWind_phrase(self):
        self.debug_print("Debug: mixingWind_phrase in FWS.py")
        return {
            "setUpMethod": self.mixingWind_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def mixingWind_setUp(self, tree, node):
        self.debug_print("Debug: mixingWind_setUp in FWS.py")
        self.wind_setUp(tree, node, gustFlag=0, element="TransWind")
        node.set("descriptor", "")
        node.set("indentLabel", "MIXING WINDS........")
        return self.DONE()

    def mixingWindMetric_phrase(self):
        self.debug_print("Debug: mixingWindMetric_phrase in FWS.py")
        return {
            "setUpMethod": self.mixingWindMetric_setUp,
            "wordMethod": self.vectorMetric_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def mixingWindMetric_setUp(self, tree, node):
        self.debug_print("Debug: mixingWindMetric_setUp in FWS.py")
        self.windMetric_setUp(tree, node, gustFlag=0, element="TransWind")
        node.set("descriptor", "")
        node.set("indentLabel", "MIXING WINDS (M/S)..")
        return self.DONE()

    def windMetric_phrase(self):
        self.debug_print("Debug: windMetric_phrase in FWS.py")
        return {
            "setUpMethod": self.windMetric_setUp,
            "wordMethod": self.vectorMetric_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def windMetric_setUp(self, tree, node, gustFlag=0, element="Wind", connectorMethod=None):
        self.debug_print("Debug: windMetric_setUp in FWS.py")
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

    def vectorMetric_words(self, tree, node):
        self.debug_print("Debug: vectorMetric_words in FWS.py")
        # Create a words for a vector element
        elementInfo = node.getAncestor("firstElement")
        if elementInfo is None:
            return self.setWords(node, "")
        words = self.simple_vectorMetric_phrase(tree, node, elementInfo)
        if words == "null":
            return self.setWords(node, "null")
        gustPhrase = ""
        if words != "":
            # Add gusts
            gustFlag = node.getAncestor("gustFlag")
            if gustFlag == 1:
                windStats = tree.stats.get("Wind", node.getTimeRange(), node.getAreaLabel(),
                                           mergeMethod="Max")
                if windStats is not None:
                    maxMag, dir = windStats
                    statDict = node.getStatDict()
                    gustStats = self.getStats(statDict, "WindGust")
                    subRange = node.get("timeRange")
                    gustPhrase = self.embedded_gust_phrase(
                        tree, node, gustStats, maxMag, subRange)
        return self.setWords(node, words + gustPhrase)

    def simple_vectorMetric_phrase(self, tree, node, elementInfo, checkRepeating=1):
        self.debug_print("Debug: simple_vectorMetric_phrase in FWS.py")
        # Create a vector subPhrase
        # Do not repeat mag, dir if same as previous phrase
        elementName = elementInfo.name
        statDict = node.getStatDict()
        stats = self.getStats(statDict, elementName)
        if stats is None:
            return ""
        mag, dir = stats
        minMag, maxMag = self.getValue(mag, "MinMax")

        # Save maxMag at component level for other methods to use.
        # THIS IS PARTICULARLY IMPORTANT FOR USE IN THE includeOnlyPhrases_list def
        # below to eliminate certainly wx elements during tropical cyclone
        # situations when certain conditions are met.
        component = node.getComponent()
        maxMagList = component.get("maxMagList")
        if maxMagList is None:
            maxMagList = [maxMag]
        else:
            maxMagList.append(maxMag)
        component.set("maxMagList", maxMagList)

#        words = self.vector_mag(tree, node, minMag, maxMag,
#                                elementInfo.outUnits, elementName)
        minMag = int(float(minMag) * .44704 + 0.5)
        maxMag = int(float(maxMag) * .44704 + 0.5)
        words = self.vectorMetric_mag(tree, node, minMag, maxMag,
                                      "m/s", elementName)
        if words == "null":
            return words
        magStr = words
        dirStr = self.vector_dir(dir)

        if checkRepeating:
            # Set for future reference
            node.set("dirStr", dirStr)
            node.set("magStr", magStr)
            node.set("minMag", minMag)
            node.set("maxMag", maxMag)
            if minMag == 0.0:
                minMag = maxMag
            # Check for repeating mag or dir
            prevNode = node.getPrev()
            if prevNode is not None:
                prevDirStr = prevNode.get("dirStr")
                prevMagStr = prevNode.get("magStr")
                prevMin = prevNode.get("minMag")
                prevMax = prevNode.get("maxMag")
                if prevMin == 0.0:
                    prevMin = prevMax
                if prevMin is None or prevMax is None or \
                   prevDirStr is None or prevMagStr is None:
                    pass
                elif prevDirStr == dirStr and prevMagStr == magStr:
                    pass
                elif prevDirStr == dirStr:
                    dirStr = ""
                elif prevMagStr == magStr:
                    magStr = ""
                # Prevent "around 10 becoming 5 to 10"
                #         "around 10 becoming 10 to 15"
                elif prevMin == prevMax:
                    if (minMag == prevMax - 5.0) or (maxMag == prevMax + 5.0):
                        magStr = ""
                # Prevent "5 to 10 becoming around 10"
                #         "10 to 15 becoming around 10"
                elif minMag == maxMag:
                    if (prevMin == maxMag - 5.0) or (prevMax == maxMag + 5.0):
                        magStr = ""
        words = dirStr + self.format(magStr)
        return words.lstrip()

    def vectorMetric_mag(self, tree, node, minMag, maxMag, units,
                         elementName="Wind"):
        "Create a phrase for a Range of magnitudes"
        self.debug_print("Debug: vectorMetric_mag in FWS.py")

        # Check for "null" value (below threshold)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, elementName, elementName), maxMag)
        if maxMag < threshold:
            return "null"
        self.debug_print(
            "vector metric maxMag " + str(maxMag) + " " + str(threshold) + " " + units)

        # Apply max reported threshold
        maxReportedMag = self.maxReported_threshold(
            tree, node, elementName, elementName)
        if maxMag >= maxReportedMag:
            maxMag = maxReportedMag
            #minMag = 0

        units = self.units_descriptor(tree, node, "units", units)

        if elementName == "Wind":
            if self.marine_wind_flag(tree, node):
                return self.marine_wind_mag(tree, node, minMag, maxMag, units, elementName)

        # Check for SingleValue
        if maxMag == minMag:  # or minMag == 0:
            around = self.addSpace(
                self.phrase_descriptor(tree, node, "around", elementName))
            words = around + str(int(maxMag)) + " " + units
        else:
            if int(minMag) < threshold:
                upTo = self.addSpace(
                    self.phrase_descriptor(tree, node, "up to", elementName))
                words = upTo + str(int(maxMag)) + " " + units
            else:
                valueConnector = self.value_connector(
                    tree, node, elementName, elementName)
                words = str(int(minMag)) + valueConnector + \
                    str(int(maxMag)) + " " + units

        # This is an additional hook for customizing the magnitude wording
        words = self.vector_mag_hook(
            tree, node, minMag, maxMag, units, elementName, words)
        return words

    def marine_wind_flag(self, tree, node):
        self.debug_print("Debug: marine_wind_flag in FWS.py")
        # If 1, Wind wording will reflect the
        # crossing of significant thresholds such as gales.
        # E.g. "West gales to 35 knots." instead of "West winds 35 knots."
        self.debug_print("marine_wind_flag")
        self.debug_print(self._incidentType)
        if self._incidentType == "MARINE":
            return 1
        else:
            return 0

    def freeWind_phrase(self):
        self.debug_print("Debug: freeWind_phrase in FWS.py")
        return {
            "setUpMethod": self.freeWind_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def freeWind_setUp(self, tree, node):
        self.debug_print("Debug: freeWind_setUp in FWS.py")
        self.wind_setUp(tree, node, gustFlag=0, element="FreeWind")
        node.set("descriptor", "")
        node.set("indentLabel", "FREE WINDS..........")
        return self.DONE()

    # 1000 FT Winds
    def wind1000ft_phrase(self):
        self.debug_print("Debug: wind1000ft_phrase in FWS.py")
        return {
            "setUpMethod": self.wind1000ft_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def wind1000ft_setUp(self, tree, node):
        self.debug_print("Debug: wind1000ft_setUp in FWS.py")
        self.wind_setUp(tree, node, gustFlag=0, element="Wind1000Ft")
        node.set("descriptor", "")
        node.set("indentLabel", "1000 FT WINDS.......")
        return self.DONE()

    # 2000 FT Winds
    def wind2000ft_phrase(self):
        self.debug_print("Debug: wind2000ft_phrase in FWS.py")
        return {
            "setUpMethod": self.wind2000ft_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def wind2000ft_setUp(self, tree, node):
        self.debug_print("Debug: wind2000ft_setUp in FWS.py")
        self.wind_setUp(tree, node, gustFlag=0, element="Wind2000Ft")
        node.set("descriptor", "")
        node.set("indentLabel", "2000 FT WINDS.......")
        return self.DONE()

    # 3000 FT Winds
    def wind3000ft_phrase(self):
        self.debug_print("Debug: wind3000ft_phrase in FWS.py")
        return {
            "setUpMethod": self.wind3000ft_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def wind3000ft_setUp(self, tree, node):
        self.debug_print("Debug: wind3000ft_setUp in FWS.py")
        self.wind_setUp(tree, node, gustFlag=0, element="Wind3000Ft")
        node.set("descriptor", "")
        node.set("indentLabel", "3000 FT WINDS.......")
        return self.DONE()

    # 4000 FT Winds
    def wind4000ft_phrase(self):
        self.debug_print("Debug: wind4000ft_phrase in FWS.py")
        return {
            "setUpMethod": self.wind4000ft_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def wind4000ft_setUp(self, tree, node):
        self.debug_print("Debug: wind4000ft_setUp in FWS.py")
        self.wind_setUp(tree, node, gustFlag=0, element="Wind4000Ft")
        node.set("descriptor", "")
        node.set("indentLabel", "4000 FT WINDS.......")
        return self.DONE()

    # 5000 FT Winds
    def wind5000ft_phrase(self):
        self.debug_print("Debug: wind5000ft_phrase in FWS.py")
        return {
            "setUpMethod": self.wind5000ft_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def wind5000ft_setUp(self, tree, node):
        self.debug_print("Debug: wind5000ft_setUp in FWS.py")
        self.wind_setUp(tree, node, gustFlag=0, element="Wind5000Ft")
        node.set("descriptor", "")
        node.set("indentLabel", "5000 FT WINDS.......")
        return self.DONE()

    # MixHgt
    def mixingHgt_phrase(self):
        self.debug_print("Debug: mixingHgt_phrase in FWS.py")
        return {
            "setUpMethod": self.mixingHgt_setUp,
            "wordMethod": self.mixingHgt_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def mixingHgt_setUp(self, tree, node):
        self.debug_print("Debug: mixingHgt_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("MixHgt", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "MIXING HEIGHT.......")
        return self.DONE()

    def mixingHgt_words(self, tree, node):
        "Create phrase for Mixing Height"
        self.debug_print("Debug: mixingHgt_words in FWS.py")

        statDict = node.getStatDict()
        stats = self.getStats(statDict, "MixHgt")
        if stats is None:
            return self.setWords(node.parent, "MISSING")

        mix1, mix2 = self.getValue(stats, "MinMax")
        outUnits = self.element_outUnits(tree, node, "MixHgt", "MixHgt")
        mix1 = int(mix1)
        mix2 = int(mix2)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, "MixHgt", "MixHgt"), max)
        if int(mix1) < threshold and int(mix2) < threshold:
            return self.setWords(node, "null")

        # Single Value input
        if mix1 == mix2:
            words = str(mix1) + " " + outUnits + " AGL"
        # Range
        else:
            dayNight = self.getPeriod(node.getTimeRange(), 1)
            if dayNight == self.DAYTIME():
                words = str(mix2) + " " + outUnits + " AGL"
            else:
                words = str(mix1) + " " + outUnits + " AGL"

        # Handle ignition time
        if self._checkFireTR(node.getTimeRange()):
            reqType = self._getRequestWords()
            ignitionMixStats = tree.stats.get(
                "MixHgt", self._fireTR, node.getAreaLabel(), mergeMethod="Max")
            igWords = str(int(ignitionMixStats)) + " " + \
                outUnits + " AGL at " + reqType + "...otherwise "
            words = igWords + words

        return self.setWords(node, words)

    # MixHgt
    def mixingHgtMsl_phrase(self):
        self.debug_print("Debug: mixingHgtMsl_phrase in FWS.py")
        return {
            "setUpMethod": self.mixingHgtMsl_setUp,
            "wordMethod": self.mixingHgtMsl_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def mixingHgtMsl_setUp(self, tree, node):
        self.debug_print("Debug: mixingHgtMsl_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("MixHgt", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "MIXING HEIGHT (MSL).")
        return self.DONE()

    def mixingHgtMsl_words(self, tree, node):
        "Create phrase for Mixing Height"
        self.debug_print("Debug: mixingHgtMsl_words in FWS.py")

        statDict = node.getStatDict()
        stats = self.getStats(statDict, "MixHgt")
        if stats is None:
            return self.setWords(node.parent, "MISSING")

        mix1, mix2 = self.getValue(stats, "MinMax")
        outUnits = self.element_outUnits(tree, node, "MixHgt", "MixHgt")
        mix1 = int(mix1)
        mix2 = int(mix2)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, "MixHgt", "MixHgt"), max)
        if int(mix1) < threshold and int(mix2) < threshold:
            return self.setWords(node, "null")

        # Single Value input
        if mix1 == mix2:
            words = str(mix1) + " " + outUnits + " MSL"
        # Range
        else:
            words = str(mix1) + "-" + str(mix2) + " " + outUnits + " MSL"

        # Handle ignition time
        if self._checkFireTR(node.getTimeRange()):
            reqType = self._getRequestWords()
            ignitionMixStats = tree.stats.get(
                "MixHgt", self._fireTR, node.getAreaLabel(), mergeMethod="Max")
            igWords = str(int(ignitionMixStats)) + " " + \
                outUnits + " MSL at " + reqType + "...otherwise "
            words = igWords + words

        return self.setWords(node, words)

    def mixingHgtMetric_phrase(self):
        self.debug_print("Debug: mixingHgtMetric_phrase in FWS.py")
        return {
            "setUpMethod": self.mixingHgtMetric_setUp,
            "wordMethod": self.mixingHgtMetric_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def mixingHgtMetric_setUp(self, tree, node):
        self.debug_print("Debug: mixingHgtMetric_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("MixHgt", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "MIXING HEIGHT (M)...")
        return self.DONE()

    def mixingHgtMetric_words(self, tree, node):
        self.debug_print("Debug: mixingHgtMetric_words in FWS.py")
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "MixHgt")
        if stats is None:
            return self.setWords(node.parent, "MISSING")

        mixFt1, mixFt2 = self.getValue(stats, "MinMax")
        #outUnits = self.element_outUnits(tree, node, "MixHgt", "MixHgt")
        outUnits = "meters"
        mix1 = int(float(mixFt1) * 0.3048 + 0.5)
        mix2 = int(float(mixFt2) * 0.3048 + 0.5)
        thresholdFt = self.nlValue(self.null_nlValue(
            tree, node, "MixHgt", "MixHgt"), max)
        threshold = int(float(thresholdFt) * 0.3048 + 0.5)
        if mix1 < threshold and mix2 < threshold:
            return self.setWords(node, "null")

        # Single Value input
        if mix1 == mix2:
            words = str(mix1) + " " + outUnits + " AGL"
        # Range
        else:
            words = str(mix1) + "-" + str(mix2) + " " + outUnits + " AGL"

        # Handle ignition time
        if self._checkFireTR(node.getTimeRange()):
            reqType = self._getRequestWords()
            ignitionMixStats = tree.stats.get(
                "MixHgt", self._fireTR, node.getAreaLabel(), mergeMethod="Max")
            igWords = str(int(ignitionMixStats)) + " " + \
                outUnits + " AGL at " + reqType + "...otherwise "
            words = igWords + words

        return self.setWords(node, words)

    def adi_phrase(self):
        self.debug_print("Debug: adi_phrase in FWS.py")
        return {
            "setUpMethod": self.adi_setUp,
            "wordMethod": self.adi_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def adi_setUp(self, tree, node):
        self.debug_print("Debug: adi_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("ADI", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "ADI.................")
        return self.DONE()

    def adi_words(self, tree, node):
        self.debug_print("Debug: adi_words in FWS.py")
        statDict = node.getStatDict()
        adi = self.getStats(statDict, "ADI")
        if adi is None:
            return self.setWords(node.parent, "MISSING")
        minAdi, maxAdi = self.getValue(adi, "MinMax")
        if maxAdi - minAdi <= 40:
            if maxAdi != minAdi:
                words = str(int(minAdi + 0.5)) + " to " + \
                    str(int(maxAdi + 0.5))
            else:
                words = str(int(minAdi + 0.5))
        else:
            dayNight = self.getPeriod(node.getTimeRange(), 1)
            if dayNight == self.DAYTIME():
                words = str(int(minAdi + 0.5)) + " early in the morning increasing to " + \
                    str(int(maxAdi + 0.5)) + " in the afternoon"
            else:
                words = str(int(maxAdi + 0.5)) + " early in the evening decreasing to " + \
                    str(int(minAdi + 0.5)) + " late in the night"
        return self.setWords(node, words)

    def gfdi_phrase(self):
        self.debug_print("Debug: gfdi_phrase in FWS.py")
        return {
            "setUpMethod": self.gfdi_setUp,
            "wordMethod": self.gfdi_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def gfdi_setUp(self, tree, node):
        self.debug_print("Debug: gfdi_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("GFDI", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "GFDI................")
        return self.DONE()

    def gfdi_words(self, tree, node):
        self.debug_print("Debug: gfdi_words in FWS.py")
        statDict = node.getStatDict()
        gfdi = self.getStats(statDict, "GFDI")
        if gfdi is None:
            return self.setWords(node.parent, "MISSING")
        gfdi = self.getValue(gfdi)
        words = str(int(gfdi + 0.5))
        return self.setWords(node, words)

    def hainesDict(self):
        self.debug_print("Debug: hainesDict in FWS.py")
        return {
            0: "or very low potential for large plume dominated fire growth",
            1: "or very low potential for large plume dominated fire growth",
            2: "or very low potential for large plume dominated fire growth",
            3: "or very low potential for large plume dominated fire growth",
            4: "or low potential for large plume dominated fire growth",
            5: "or moderate potential for large plume dominated fire growth",
            6: "or high potential for large plume dominated fire growth",
            7: "or high potential for large plume dominated fire growth",
            8: "or high potential for large plume dominated fire growth",
            9: "or high potential for large plume dominated fire growth",
            10: "or high potential for large plume dominated fire growth"
        }

    def haines_words(self, tree, node):
        "Create phrase for Haines Index"
        self.debug_print("Debug: haines_words in FWS.py")
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "Haines")
        if stats is None:
            return self.setWords(node.parent, "MISSING")

        # Handle ignition time
        ignitionFlag = 0
        if self._checkFireTR(node.getTimeRange()):
            haines1 = int(self.getValue(stats, "Max"))
            ignitionStats = tree.stats.get("Haines", self._fireTR, node.getAreaLabel(),
                                           mergeMethod="Max")
            if ignitionStats is not None:
                ignitionPhrase = str(int(self.getValue(ignitionStats)))
                reqType = self._getRequestWords()
                hainesDict = self.hainesDict()
                words = ignitionPhrase + " " + hainesDict[int(ignitionPhrase)] + \
                    " at " + reqType + "...max " + str(haines1)
                ignitionFlag = 1
        if not ignitionFlag:
            haines1, haines2 = self.getValue(stats, "MinMax")
            hainesDict = self.hainesDict()
            haines1 = int(haines1)
            haines2 = int(haines2)
            words1 = hainesDict[haines1]
            words2 = hainesDict[haines2]

            if words1 == "" and words2 == "":
                # Single Value input
                if haines1 == haines2:
                    words = str(haines1)
                # Range
                else:
                    words = str(haines1) + " to " + str(haines2)
            else:
                # Single Value input
                if haines1 == haines2:
                    words = str(haines1) + "   " + words1
                # Range
                else:
                    if words1 == words2:
                        words = words1[3:]
                    else:
                        words = words1[3:].replace(" potential for large plume dominated fire growth", "") + " to " + \
                            words2[3:].replace(" potential for large plume dominated fire growth", "") + \
                            " potential for large plume dominated fire growth"
                    words = str(haines1) + " to " + \
                        str(haines2) + " or " + words
        return self.setWords(node, words)

    def hainesMid_phrase(self):
        self.debug_print("Debug: hainesMid_phrase in FWS.py")
        return {
            "setUpMethod": self.hainesMid_setUp,
            "wordMethod": self.hainesMid_words,
            "phraseMethods": [
                self.consolidatePhrase,
                self.checkLocalEffects,
                self.combinePhraseStats,
                self.combineWords,
                self.fillNulls,
                self.timeDescriptorModeration,
                self.assembleSubPhrases,
                self.postProcessPhrase,
            ]
        }

    def hainesMid_setUp(self, tree, node):
        self.debug_print("Debug: hainesMid_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("HainesMid", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "HAINES INDEX........")
        node.set("indentLabel", "HAINES MID LEVEL....")
        return self.DONE()

    def hainesMid_words(self, tree, node):
        "Create phrase for Haines Index"
        self.debug_print("Debug: hainesMid_words in FWS.py")
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "HainesMid")
        if stats is None:
            return self.setWords(node.parent, "MISSING")

        # Handle ignition time
        ignitionFlag = 0
        if self._checkFireTR(node.getTimeRange()):
            haines1 = int(self.getValue(stats, "Max"))
            ignitionStats = tree.stats.get("HainesMid", self._fireTR, node.getAreaLabel(),
                                           mergeMethod="Max")
            if ignitionStats is not None:
                ignitionPhrase = str(int(self.getValue(ignitionStats)))
                reqType = self._getRequestWords()
                hainesDict = self.hainesDict()
                words = ignitionPhrase + " " + hainesDict[int(ignitionPhrase)] + \
                    " at " + reqType + "...max " + str(haines1)
                ignitionFlag = 1
        if not ignitionFlag:
            haines1, haines2 = self.getValue(stats, "MinMax")
            hainesDict = self.hainesDict()
            haines1 = int(haines1)
            haines2 = int(haines2)
            words1 = hainesDict[haines1]
            words2 = hainesDict[haines2]

            # Single Value input
            if haines1 == haines2:
                words = str(haines1) + "   " + words1
            # Range
            else:
                if words1 == words2:
                    words = words1[3:]
                else:
                    words = words1[3:].replace(" potential for large plume dominated fire growth", "") + " to " + \
                        words2[3:].replace(" potential for large plume dominated fire growth", "") + \
                        " potential for large plume dominated fire growth"
                words = str(haines1) + " to " + str(haines2) + " or " + words
        return self.setWords(node, words)

    def cwr_words(self, tree, node):
        self.debug_print("Debug: cwr_words in FWS.py")
        # Handle ignition time
        if self._checkFireTR(node.getTimeRange()):
            cwr = tree.stats.get(
                self._cwrParm, self._fireTR, node.getAreaLabel(), mergeMethod="Max")
        else:
            cwr = tree.stats.get(
                self._cwrParm, node.getTimeRange(), node.getAreaLabel(), mergeMethod="Max")
        if cwr is None:
            return self.setWords(node.parent, "MISSING")
        cwr = self.getValue(cwr)
        threshold = self.nlValue(
            self.null_nlValue(tree, node, "CWR", "CWR"), cwr)
        if int(cwr) < threshold:
            return self.setWords(node, "null")
        else:
            words = str(int(cwr)) + " percent"
        return self.setWords(node, words)

    def windWave_phrase(self):
        self.debug_print("Debug: windWave_phrase in FWS.py")
        return {
            "setUpMethod": self.windWave_setUp,
            "wordMethod": self.windWave_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def windWave_setUp(self, tree, node):
        self.debug_print("Debug: windWave_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("WindWaveHgt", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "WIND WAVE...........")
        return self.DONE()

    def windWave_words(self, tree, node):
        "Create phrase Wind Wave"
        self.debug_print("Debug: windWave_words in FWS.py")
        statDict = node.getStatDict()
        height = self.getValue(self.getStats(statDict, "WindWaveHgt"), "Max")
        if height is None:
            return self.setWords(node.parent, "MISSING")
        heightInt = int(height + 0.5)
        if heightInt < 1:
            words = "Less than 1 foot"
        elif heightInt == 1:
            words = "1 foot"
        else:
            words = str(heightInt) + " feet"
        return self.setWords(node, words)

    def waveHeight_phrase(self):
        self.debug_print("Debug: windHeight_phrase in FWS.py")
        return {
            "setUpMethod": self.waveHeight_setUp,
            "wordMethod": self.waveHeight_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def waveHeight_setUp(self, tree, node):
        self.debug_print("Debug: windHeight_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("WaveHeight", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "WAVE HEIGHT.........")
        return self.DONE()

    def waveHeight_words(self, tree, node):
        "Create phrase Wind Wave"
        self.debug_print("Debug: windHeight_words in FWS.py")
        statDict = node.getStatDict()
        height = self.getValue(self.getStats(statDict, "WaveHeight"), "Max")
        if height is None:
            return self.setWords(node.parent, "MISSING")
        heightInt = int(height + 0.5)
        if heightInt < 1:
            words = "Less than 1 foot"
        elif heightInt == 1:
            words = "1 foot"
        else:
            words = str(heightInt) + " feet"
        return self.setWords(node, words)

    def hiOneTenth_phrase(self):
        self.debug_print("Debug: hiOneTenth_phrase in FWS.py")
        return {
            "setUpMethod": self.hiOneTenth_setUp,
            "wordMethod": self.hiOneTenth_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def hiOneTenth_setUp(self, tree, node):
        self.debug_print("Debug: hiOneTenth_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("HiOneTenth", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "HI ONE TENTH........")
        return self.DONE()

    def hiOneTenth_words(self, tree, node):
        self.debug_print("Debug: hiOneTenth_words in FWS.py")
        statDict = node.getStatDict()
        height = self.getValue(self.getStats(statDict, "HiOneTenth"), "Max")
        if height is None:
            return self.setWords(node.parent, "MISSING")
        heightInt = int(height + 0.5)
        if heightInt < 1:
            words = "Less than 1 foot"
        elif heightInt == 1:
            words = "1 foot"
        else:
            words = str(heightInt) + " feet"
        return self.setWords(node, words)

    def qpf_phrase(self):
        self.debug_print("Debug: qpf_phrase in FWS.py")
        return {
            "setUpMethod": self.qpf_setUp,
            "wordMethod": self.qpf_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def qpf_setUp(self, tree, node):
        self.debug_print("Debug: qpf_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("QPF", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "RAINFALL AMOUNT.....")
        return self.DONE()

    def qpf_words(self, tree, node):
        "Create phrase QPF"
        self.debug_print("Debug: qpf_words in FWS.py")
        statDict = node.getStatDict()
        qpf = self.getValue(self.getStats(statDict, "QPF"), "Max")
        if qpf is None:
            return self.setWords(node.parent, "MISSING")
        if qpf == 0.0:
            qpfWords = "0.00"
        else:
            qpf = qpf + 0.005
            qpfWords = "%5.2f" % qpf
            qpfWords = qpfWords.strip()
        words = qpfWords + " inches"
        return self.setWords(node, words)

    def precipitationAmount_phrase(self):
        self.debug_print("Debug: precipitationAmount_phrase in FWS.py")
        return {
            "setUpMethod": self.precipitationAmount_setUp,
            "wordMethod": self.precipitationAmount_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def precipitationAmount_setUp(self, tree, node):
        self.debug_print("Debug: precipitationAmount_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("QPF", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "PCPN AMOUNT.........")
        return self.DONE()

    def precipitationAmount_words(self, tree, node):
        self.debug_print("Debug: precipitationAmount_words in FWS.py")
        statDict = node.getStatDict()
        qpf = self.getValue(self.getStats(statDict, "QPF"), "Max")
        if qpf is None:
            return self.setWords(node.parent, "MISSING")
        if qpf == 0.0:
            qpfWords = "0.00"
        else:
            qpf = qpf + 0.005
            qpfWords = "%5.2f" % qpf
            qpfWords = qpfWords.strip()
        words = qpfWords + " inches"
        return self.setWords(node, words)

    def period_phrase(self):
        self.debug_print("Debug: period_phrase in FWS.py")
        return {
            "setUpMethod": self.period_setUp,
            "wordMethod": self.period_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def period_setUp(self, tree, node):
        self.debug_print("Debug: period_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("Period", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "SWELL PERIOD........")
        return self.DONE()

    def period_words(self, tree, node):
        self.debug_print("Debug: period_words in FWS.py")
        statDict = node.getStatDict()
        period = self.getValue(self.getStats(statDict, "Period"), "Max")
        if period is None:
            return self.setWords(node.parent, "MISSING")
        words = str(int(period + 0.5)) + " Seconds"
        return self.setWords(node, words)

    def wavePeriod_phrase(self):
        self.debug_print("Debug: wavePeriod_phrase in FWS.py")
        return {
            "setUpMethod": self.wavePeriod_setUp,
            "wordMethod": self.wavePeriod_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def wavePeriod_setUp(self, tree, node):
        self.debug_print("Debug: wavePeriod_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("Period", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "WAVE PERIOD.........")
        return self.DONE()

    def wavePeriod_words(self, tree, node):
        self.debug_print("Debug: wavePeriod_words in FWS.py")
        statDict = node.getStatDict()
        period = self.getValue(self.getStats(statDict, "Period"), "Max")
        if period is None:
            return self.setWords(node.parent, "MISSING")
        words = str(int(period + 0.5)) + " SECONDS"
        return self.setWords(node, words)

    def swell_phrase(self):
        self.debug_print("Debug: swell_phrase in FWS.py")
        return {
            "setUpMethod": self.swell_setUp,
            "wordMethod": self.swell_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def swell_setUp(self, tree, node):
        self.debug_print("Debug: swell_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("Swell", self.VECTOR())]
        self.subPhraseSetUp(tree, node, elementInfoList, self.vectorConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "SWELL HEIGHT........")
        return self.DONE()

    def swell_words(self, tree, node):
        "Create phrase Swell Height"
        self.debug_print("Debug: swell_words in FWS.py")
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "Swell")
        if stats is None:
            return self.setWords(node, "")
        height, dir = self.getValue(stats, "Max", self.VECTOR())
        if height is None:
            return self.setWords(node.parent, "MISSING")
        if dir >= 22.5 and dir < 67.5:
            dirWords = "northeast"
        elif dir >= 67.5 and dir < 112.5:
            dirWords = "east"
        elif dir >= 112.5 and dir < 157.5:
            dirWords = "southeast"
        elif dir >= 157.5 and dir < 202.5:
            dirWords = "south"
        elif dir >= 202.5 and dir < 247.5:
            dirWords = "southwest"
        elif dir >= 247.5 and dir < 292.5:
            dirWords = "west"
        elif dir >= 292.5 and dir < 337.5:
            dirWords = "northwest"
        else:
            dirWords = "north"
        heightWords = str(int(height + 0.5))
        words = dirWords + " swell " + heightWords + " feet"
        return self.setWords(node, words)

    def freezingSpray_phrase(self):
        self.debug_print("Debug: freezingSpray_phrase in FWS.py")
        return {
            "setUpMethod": self.freezingSpray_setUp,
            "wordMethod": self.freezingSpray_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def freezingSpray_setUp(self, tree, node):
        self.debug_print("Debug: freezingSpray_setUp in FWS.py")
        self.subPhraseSetUp(tree, node, [], self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "FREEZING SPRAY......")
        return self.DONE()

    def freezingSpray_words(self, tree, node):
        self.debug_print("Debug: freezingSpray_words in FWS.py")
        words = ""
        areaLabel = tree.getAreaLabel()
        stats = tree.stats.get("FrzngSpry", tree.getTimeRange(),
                               areaLabel, mergeMethod="List")
        if stats is None:
            return self.setWords(node, "None")

        # Sort the headlines by startTime
        temp = []
        heavyFlag = 0
        lightFlag = 0
        for h, tr in stats:
            if h == "Heavy":
                heavyFlag = 1
            elif h == "Light":
                lightFlag = 1
        if heavyFlag:
            return self.setWords(node, "Heavy")
        elif lightFlag:
            return self.setWords(node, "Light")
        else:
            return self.setWords(node, "None")

    def seaIceConcentration_phrase(self):
        self.debug_print("Debug: seaIceConcentration_phrase in FWS.py")
        return {
            "setUpMethod": self.seaIceConcentration_setUp,
            "wordMethod": self.seaIceConcentration_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def seaIceConcentration_setUp(self, tree, node):
        self.debug_print("Debug: seaIceConcentration_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("IceC", "Max")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "SEA ICE COVERAGE....")
        return self.DONE()

    def seaIceConcentration_words(self, tree, node):
        self.debug_print("Debug: seaIceConcentration_words in FWS.py")
        statDict = node.getStatDict()
        concentration = self.getValue(self.getStats(statDict, "IceC"), "Max")
        if concentration is None:
            return self.setWords(node.parent, "MISSING")
        icon = int(concentration * 10.0 + 0.5) * 10
        if icon > 100:
            icon = 100
        words = str(icon) + " Percent"
        return self.setWords(node, words)

    def seaSurfaceTemperature_phrase(self):
        self.debug_print("Debug: seaSurfaceTemperature_phrase in FWS.py")
        return {
            "setUpMethod": self.seaSurfaceTemperature_setUp,
            "wordMethod": self.seaSurfaceTemperature_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def seaSurfaceTemperature_setUp(self, tree, node):
        self.debug_print("Debug: seaSurfaceTemperature_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("SSTAK", "Min")]
        #elementInfoList = [self.ElementInfo("SST", "Min")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "SEA SURFACE TEMP....")
        return self.DONE()

    def seaSurfaceTemperature_words(self, tree, node):
        self.debug_print("Debug: seaSurfaceTemperature_words in FWS.py")
        statDict = node.getStatDict()
        sst = self.getValue(self.getStats(statDict, "SSTAK"), "Min")
        #sst = self.getValue(self.getStats(statDict, "SST"), "Min")
        if sst is None:
            return self.setWords(node.parent, "MISSING")
        isst = int(sst + 0.5)
        words = str(isst) + " Degrees Fahrenheit"
        return self.setWords(node, words)

    def ceiling_phrase(self):
        self.debug_print("Debug: ceiling_phrase in FWS.py")
        return {
            "setUpMethod": self.ceiling_setUp,
            "wordMethod": self.ceiling_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def ceiling_setUp(self, tree, node):
        self.debug_print("Debug: ceiling_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("CloudBasePrimary", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "CEILING (KFT).......")
        return self.DONE()

    def ceiling_words(self, tree, node):
        self.debug_print("Debug: ceiling_words in FWS.py")
        statDict = node.getStatDict()
        hgt = self.getValue(self.getStats(statDict, "CloudBasePrimary"), "Min")
        if hgt is None:
            return self.setWords(node.parent, "MISSING")
        hgt = hgt / 10.0
        if hgt == 0.0:
            hgtWords = "less than 0.1"
        else:
            if hgt < 10:
                hgtWords = "%5.1f" % hgt
                hgtWords = hgtWords.strip()
            else:
                hgtWords = str(int(hgt + 0.5))
        words = hgtWords
        return self.setWords(node, words)

    def visibility_phrase(self):
        self.debug_print("Debug: visibility_phrase in FWS.py")
        return {
            "setUpMethod": self.visibility_setUp,
            "wordMethod": self.visibility_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def visibility_setUp(self, tree, node):
        self.debug_print("Debug: visibility_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("Visibility", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "VISIBILITY (SM).....")
        return self.DONE()

    def visibility_words(self, tree, node):
        self.debug_print("Debug: visibility_words in FWS.py")
        statDict = node.getStatDict()
        vis = self.getValue(self.getStats(statDict, "Visibility"), "Min")
        if vis is None:
            return self.setWords(node.parent, "MISSING")
        if vis == 0.0:
            visWords = "0.0"
        else:
            if vis < 3:
                visWords = "%5.2f" % vis
                visWords = visWords.strip()
            else:
                visWords = str(int(vis + 0.5))
        words = visWords
        return self.setWords(node, words)

    def pressure_phrase(self):
        self.debug_print("Debug: pressure_phrase in FWS.py")
        return {
            "setUpMethod": self.pressure_setUp,
            "wordMethod": self.pressure_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def pressure_setUp(self, tree, node):
        self.debug_print("Debug: pressure_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("Pres", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "Sfc pressure (in)...")
        return self.DONE()

    def pressure_words(self, tree, node):
        self.debug_print("Debug: pressure_words in FWS.py")
        statDict = node.getStatDict()
        pressure = self.getValue(self.getStats(statDict, "Pres"), "Min")
        if pressure is None:
            return self.setWords(node.parent, "MISSING")
        if pressure < 40.0:
            inHg = float(int(float(pressure) * 100.0 + 0.5)) / 100.0
        else:
            inHg = float(int(float(pressure) * 2.953 + 0.5)) / 100.0
        words = str(inHg)
        return self.setWords(node, words)

    def icing_phrase(self):
        self.debug_print("Debug: icing_phrase in FWS.py")
        return {
            "setUpMethod": self.icing_setUp,
            "phraseMethods": [self.postProcessPhrase],
        }

    def icing_setUp(self, tree, node):
        self.debug_print("Debug: icing_setUp in FWS.py")
        self.setWords(node, "")
        node.set("descriptor", "")
        node.set("indentLabel", "ICING...............")
        return self.DONE()

    def wind100ft_phrase(self):
        self.debug_print("Debug: wind100ft_phrase in FWS.py")
        return {
            "setUpMethod": self.wind100ft_setUp,
            "phraseMethods": [self.postProcessPhrase],
        }

    def wind100ft_setUp(self, tree, node):
        self.debug_print("Debug: wind100ft_setUp in FWS.py")
        self.setWords(node, "")
        node.set("descriptor", "")
        node.set("indentLabel", "100 FT WIND.........")
        return self.DONE()

    def riverLevel_phrase(self):
        self.debug_print("Debug: riverLevel_phrase in FWS.py")
        return {
            "setUpMethod": self.riverLevel_setUp,
            "phraseMethods": [self.postProcessPhrase],
        }

    def riverLevel_setUp(self, tree, node):
        self.debug_print("Debug: riverLevel_setUp in FWS.py")
        self.setWords(node, "")
        node.set("descriptor", "")
        node.set("indentLabel", "RIVER LEVEL.........")
        return self.DONE()

    def riverTemperature_phrase(self):
        self.debug_print("Debug: riverTemperature_phrase in FWS.py")
        return {
            "setUpMethod": self.riverTemperature_setUp,
            "phraseMethods": [self.postProcessPhrase],
        }

    def riverTemperature_setUp(self, tree, node):
        self.debug_print("Debug: riverTemperature_setUp in FWS.py")
        self.setWords(node, "")
        node.set("descriptor", "")
        node.set("indentLabel", "RIVER TEMPERATURE...")
        return self.DONE()

    def waterTemperature_phrase(self):
        self.debug_print("Debug: waterTemperature_phrase in FWS.py")
        return {
            "setUpMethod": self.waterTemperature_setUp,
            "phraseMethods": [self.postProcessPhrase],
        }

    def waterTemperature_setUp(self, tree, node):
        self.debug_print("Debug: waterTemperature_setUp in FWS.py")
        self.setWords(node, "")
        node.set("descriptor", "")
        node.set("indentLabel", "WATER TEMPERATURE...")
        return self.DONE()

    def freezingLevel_phrase(self):
        self.debug_print("Debug: freezingLevel_phrase in FWS.py")
        return {
            "setUpMethod": self.freezingLevel_setUp,
            "wordMethod": self.freezingLevel_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def freezingLevel_setUp(self, tree, node):
        self.debug_print("Debug: freezingLevel_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("FzLevel", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "FREEZING LEVEL......")
        return self.DONE()

    def freezingLevel_words(self, tree, node):
        "Create phrase for Freezing Level"
        self.debug_print("Debug: freezingLevel_words in FWS.py")

        statDict = node.getStatDict()
        stats = self.getStats(statDict, "FzLevel")
        if stats is None:
            return self.setWords(node.parent, "MISSING")

        mix1, mix2 = self.getValue(stats, "MinMax")
        outUnits = self.element_outUnits(tree, node, "FzLevel", "FzLevel")
        mix1 = int(mix1)
        mix2 = int(mix2)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, "FzLevel", "FzLevel"), max)
        if int(mix1) < threshold and int(mix2) < threshold:
            return self.setWords(node, "null")

        # Single Value input
        if mix1 == mix2:
            words = str(mix1) + " " + outUnits
        # Range
        else:
            words = str(mix1) + "-" + str(mix2) + " " + outUnits

        # Handle ignition time
        if self._checkFireTR(node.getTimeRange()):
            reqType = self._getRequestWords()
            ignitionMixStats = tree.stats.get(
                "FzLevel", self._fireTR, node.getAreaLabel(), mergeMethod="Max")
            igWords = str(int(ignitionMixStats)) + " " + \
                outUnits + " at " + reqType + "...otherwise "
            words = igWords + words

        return self.setWords(node, words)

    def snowLevel_phrase(self):
        self.debug_print("Debug: snowLevel_phrase in FWS.py")
        return {
            "setUpMethod": self.snowLevel_setUp,
            "wordMethod": self.snowLevel_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def snowLevel_setUp(self, tree, node):
        self.debug_print("Debug: snowLevel_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("SnowLevel", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "SNOW LEVEL..........")
        return self.DONE()

    def snowLevel_words(self, tree, node):
        self.debug_print("Debug: snowLevel_words in FWS.py")
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "SnowLevel")
        if stats is None:
            return self.setWords(node.parent, "MISSING")

        mix1, mix2 = self.getValue(stats, "MinMax")
        outUnits = self.element_outUnits(tree, node, "SnowLevel", "SnowLevel")
        mix1 = int(mix1)
        mix2 = int(mix2)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, "SnowLevel", "SnowLevel"), max)
        if int(mix1) < threshold and int(mix2) < threshold:
            return self.setWords(node, "null")

        # Single Value input
        if mix1 == mix2:
            words = str(mix1) + " " + outUnits
        # Range
        else:
            words = str(mix1) + "-" + str(mix2) + " " + outUnits

        # Handle ignition time
        if self._checkFireTR(node.getTimeRange()):
            reqType = self._getRequestWords()
            ignitionMixStats = tree.stats.get(
                "SnowLevel", self._fireTR, node.getAreaLabel(), mergeMethod="Max")
            igWords = str(int(ignitionMixStats)) + " " + \
                outUnits + " at " + reqType + "...otherwise "
            words = igWords + words

        return self.setWords(node, words)

    def snow_phrase(self):
        self.debug_print("Debug: snow_phrase in FWS.py")
        return {
            "setUpMethod": self.snow_setUp,
            "wordMethod": self.snow_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def snow_setUp(self, tree, node):
        self.debug_print("Debug: snow_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("SnowAmt", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "SNOWFALL AMOUNT.....")
        return self.DONE()

    def snow_words(self, tree, node):
        "Create phrase Snow"
        self.debug_print("Debug: snow_words in FWS.py")
        statDict = node.getStatDict()
        snow = self.getValue(self.getStats(statDict, "SnowAmt"), "Max")
        if snow is None:
            return self.setWords(node.parent, "MISSING")
        if snow == 0.0:
            snowWords = "0.0"
        else:
            snow = snow + 0.05
            snowWords = "%5.1f" % snow
            snowWords = snowWords.strip()
        words = snowWords + " inches"
        return self.setWords(node, words)

    def heatIndex_phrase(self):
        self.debug_print("Debug: heatIndex_phrase in FWS.py")
        return {
            "setUpMethod": self.heatIndex_setUp,
            "wordMethod": self.heatIndex_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def heatIndex_setUp(self, tree, node):
        self.debug_print("Debug: heatIndex_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("HeatIndex", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "HEAT INDEX..........")
        return self.DONE()

    def heatIndex_words(self, tree, node):
        self.debug_print("Debug: heatIndex_words in FWS.py")
        statDict = node.getStatDict()
        hi = self.getValue(self.getStats(statDict, "HeatIndex"), "Max")
        if hi is None:
            return self.setWords(node.parent, "MISSING")
        words = str(int(hi))
        return self.setWords(node, words)

    def windChill_phrase(self):
        self.debug_print("Debug: windChill_phrase in FWS.py")
        return {
            "setUpMethod": self.windChill_setUp,
            "wordMethod": self.windChill_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def windChill_setUp(self, tree, node):
        self.debug_print("Debug: windChill_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("WindChill", "Min")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "WIND CHILL..........")
        return self.DONE()

    def windChill_words(self, tree, node):
        self.debug_print("Debug: windChill_words in FWS.py")
        statDict = node.getStatDict()
        self.debug_print(statDict)
        wc = self.getValue(self.getStats(statDict, "WindChill"), "Min")
        self.debug_print(wc)
        if wc is None:
            return self.setWords(node.parent, "MISSING")
        words = str(int(wc))
        return self.setWords(node, words)

    def apparentTemperature_phrase(self):
        self.debug_print("Debug: apparentTemperature_phrase in FWS.py")
        return {
            "setUpMethod": self.apparentTemperature_setUp,
            "wordMethod": self.apparentTemperature_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def apparentTemperature_setUp(self, tree, node):
        self.debug_print("Debug: apparentTemperature_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("ApparentT", "MinMax")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        #node.set("indentLabel", "APPARENT TEMP.......")
        return self.DONE()

    def apparentTemperature_words(self, tree, node):
        self.debug_print("Debug: apparentTemperature_words in FWS.py")
        statDict = node.getStatDict()
        self.debug_print(statDict)
        atMin, atMax = self.getValue(
            self.getStats(statDict, "ApparentT"), "MinMax")
        self.debug_print("Apparent Temperature")
        self.debug_print(atMin, atMax)
        if atMin is None or atMax is None:
            return self.setWords(node.parent, "MISSING")
        if atMin < 40:
            words = str(int(atMin))
            words = self.labelIndent(words, "Min apparent temp...")
        else:
            words = str(int(atMax))
            words = self.labelIndent(words, "Max apparent temp...")
        return self.setWords(node, words)

    ### Methods for Spot Table ###

    def _fwsTable_phrase(self):
        self.debug_print("Debug: fwsTable_phrase in FWS.py")
        return {
            "setUpMethod": self._fwsTable_setUp,
            "wordMethod": self._fwsTable_words,
            "phraseMethods": [
                self.assembleChildWords,
            ],
        }

    def _fwsTable_setUp(self, tree, node):
        self.debug_print("Debug: fwsTable_setUp in FWS.py")
        elementInfoList = []
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        return self.DONE()

    def _fwsTable_words(self, tree, node):
        self.debug_print("Debug: fwsTable_words in FWS.py")
        # See if we're doing a table for this time period
        tableVars = self._determineTableVars(tree, node)
        if tableVars is None:
            return self.setWords(node, "")
        timeRangeList, statList, colWidth, header, elements = tableVars
        words = header
        argList = [tree, node, colWidth]
        for rowElement, narrativeToo, tableRows in self._rowList(colWidth):
            if rowElement not in elements:
                continue
            for label, method in tableRows:
                # Call makeRow adding to words
                words += self.makeRow(
                    label, colWidth, timeRangeList, statList,
                    method, argList, justify="l")
        return self.setWords(node, words)

    def _determineTableVars(self, tree, node):
        self.debug_print("Debug: _determineTableVars in FWS.py")
        # Make timeRangeList, empty statList, colWidth, and header

        # Get table resolution (period) based on today, tonight, tomorrow
        componentName = node.getComponentName()
        period = None

        if self._productIssuance in ["Afternoon", "Evening Update", "Early Morning Update"]:
            tablePeriodList = [
                ("FirePeriod1", self._period1TableRes, self._period1Elements),
                ("FirePeriod2", self._period2TableRes, self._period2Elements),
                ("FirePeriod3", self._period3TableRes, self._period3Elements),
                ("FirePeriod4", self._period4TableRes, self._period4Elements),
            ]
        else:
            tablePeriodList = [
                ("FirePeriod1", self._period1TableRes, self._period1Elements),
                ("FirePeriod2", self._period2TableRes, self._period2Elements),
                ("FirePeriod3", self._period3TableRes, self._period3Elements),
            ]

        for name, variable, elements in tablePeriodList:
            if componentName == name:
                period = variable
                tableElements = elements
        if period is None or period == "None":
            # No table for this component
            return None

        # Determine colWidth given the period
        colWidth = 4
        for hrs, colWidth in self._colWidths():
            if period == hrs:
                break

        # Build the WFO Time Zone
        self._officeTimezone = self._getOfficeTimezone()

        # Set the time zone of the incident
        incidentTimezone = self._convertTimezone(self._incidentTZ)

        # Create a datetime structure in the requested product's timezone
        tempdatetime = datetime.strptime(
            self._forecastStartTime + ' ' + self._forecastStartDate,
            '%H%M %m/%d/%y')
        timezone = pytz.timezone(incidentTimezone)
        tempdatetime = timezone.localize(tempdatetime)

        # NOW CONVERT TO LOCAL WFO TIME ZONE
        tempdatetime = tempdatetime.astimezone(pytz.timezone(incidentTimezone))

        self._forecastStartDateTime = tempdatetime.timetuple()

        # Determine Time Ranges over which to create table
        forecastStartTimeZone = time.strftime(
            '%Z', self._forecastStartDateTime)
        timeRange = self._determineTableTimeRange(
            tree, node, forecastStartTimeZone)
        timeRangeList = self.getPeriods(timeRange, period, 1, None)

        # Make header
        header = "TIME (" + tempdatetime.tzname() + ")      "
        for tr, label in timeRangeList:
            label = self._makeTableLabel(tree, tr, colWidth)
            header += label.ljust(colWidth)
        header += "\n"

        # Make empty statList (dummy for calling "makeRow")
        statList = []
        for tr in timeRangeList:
            statList.append({})
        return timeRangeList, statList, colWidth, header, tableElements

    def _colWidths(self):
        self.debug_print("Debug: _colWidths in FWS.py")
        # Lists table resolutions hours, corresponding column width
        return [
            (1, 4),
            (2, 7),
            (3, 10),
            (4, 13),
        ]

    def _determineTableTimeRange(self, tree, node, forecastStartTimeZone):
        self.debug_print("Debug: _determineTableTimeRange in FWS.py")
        tr = node.getTimeRange()
        # See if this is first period of product
        prev = node.getComponent().getPrev()
        if prev is None:
            # Adjust timeRange if necessary
            if self._tableStartTimeMode == "current":
                currentTime = tree.get('argDict').get('creationTime')
                currentTime = int(currentTime / 3600.0) * 3600.0
                tr = self.makeTimeRange(currentTime, tr.endTime().unixTime())
            elif self._tableStartTimeMode == "ignitionTime":
                forecastStartDateTime = time.mktime(
                    self._forecastStartDateTime)
                forecastStartDateTime = int(
                    forecastStartDateTime / 3600.0) * 3600.0
                forecastStartTime = forecastStartDateTime - \
                    (self._tableStartTimeOffset * 3600)
                if forecastStartTime >= tr.startTime().unixTime() and \
                        forecastStartTime < tr.endTime().unixTime():
                    tr = self.makeTimeRange(
                        forecastStartTime, tr.endTime().unixTime())
        if self._tabularAllPeriods == "yes":
            timeRange = tr
        else:
            # One 12-hour period
            timeRange = self.makeTimeRange(tr.startTime(),
                                           tr.startTime() + 12 * 3600)
        return timeRange

    def _makeTableLabel(self, tree, timeRange, colWidth):
        self.debug_print("Debug: _makeTableLabel in FWS.py")

        # Build the WFO Time Zone
        self._officeTimezone = self._getOfficeTimezone()

        # Set the time zone of the incident
        incidentTimezone = self._convertTimezone(self._incidentTZ)

        # Create a datetime structure in the requested product's timezone
        tempdatetime = datetime.strptime(
            self._forecastStartTime + ' ' + self._forecastStartDate,
            '%H%M %m/%d/%y')
        timezone = pytz.timezone(incidentTimezone)
        tempdatetime = timezone.localize(tempdatetime)

        officedatetime = tempdatetime
        tempdatetime = tempdatetime.astimezone(pytz.timezone(incidentTimezone))
        self._forecastStartDateTime = tempdatetime.timetuple()

        officedatetime = officedatetime.astimezone(
            pytz.timezone(self._officeTimezone))

        localTime, shift = self.determineTimeShift()
        offset = int((tempdatetime.utcoffset().total_seconds() -
                      officedatetime.utcoffset().total_seconds()) / 3600)
        start = timeRange.startTime() + shift + offset * 3600
        militaryHour = start.hour
        hour, ampm = self.hourAmPm(militaryHour)
        for low, hi, shortVal, longVal in self._tableLabels():
            if militaryHour >= low and militaryHour <= hi:
                if colWidth > 4:
                    val = longVal
                else:
                    val = shortVal
                val = val.replace("hour", str(hour))
                break
        return val

    def _tableLabels(self):
        self.debug_print("Debug: _tableLabels in FWS.py")
        return [
            (0, 0, "MID", "MIDNGT"),
            (1, 9, "hourAM", "hour AM"),
            (10, 11, "hourA", "hour AM"),
            (12, 12, "12P", "NOON"),
            (13, 21, "hourPM", "hour PM"),
            (22, 23, "hourP", "hour PM"),
        ]

    def assembleIndentedPhrases(self, tree, component):
        self.debug_print("Debug: assembleIndentedPhrases in FWS.py")
        # Assemble and indent component phrases and add Label
        # Qualify the phrases with local effect qualifiers
        #  if present.
        #   e.g. "near the coast"
        for phrase in component.get("childList"):
            words = phrase.get("words")
            if words is None:
                return
        self.consolidateLocalEffectPhrases(tree, component)
        self.combineConjunctivePhrases(tree, component)
        fcst = ""
        lastQualifier = None
        lastPhrase = None
        self.orderWxPhrases(tree, component)
        for phrase in component.get("childList"):
            words = phrase.get("words")
            if words is None:
                return
            if words == "":
                if self.removeEmptyPhrase(tree, phrase):
                    continue

            # Handle multiple element table phrase
            # that appears per period
            # No need to indent or qualify
            name = phrase.get("name")
            if name == "multipleElementTable_perPeriod_phrase":
                fcst = fcst + words
                continue
            if name == "_fwsTable_phrase":
                if words != "":
                    fcst = fcst + "\n" + words
                continue

            if phrase.get("compound"):
                makeSentence = 0
            else:
                makeSentence = 1
            words, lastQualifier = self.qualifyWords(
                phrase, words, "conjunctiveQualifier", lastQualifier, lastPhrase,
                makeSentence=makeSentence)
            lastPhrase = phrase
            indentLabel = phrase.get("indentLabel")
            label = self.phrase_descriptor(
                tree, phrase, indentLabel, indentLabel)
            if indentLabel is not None and label == "":
                label = indentLabel
            if words == "":
                words = " "
            words = self.labelIndent(words, label)
            fcst = fcst + words
        if fcst == "":
            return self.setWords(component, "")
        # Add label
        issuanceInfo = tree.get("issuanceInfo")
        index = component.getIndex()
        curLocalTime, shift = self.determineTimeShift()
        creationTime = tree.get('argDict').get('creationTime')
        curLocalTime = AbsTime.AbsTime(creationTime)
        label = self.createLabel(tree, component, component.get("timeRange"),
                                 issuanceInfo, curLocalTime, shift, index)
        return self.setWords(component, label + "\n" + fcst + "\n")

    def _getTableStats(self, tree, element, tr, area, mergeMethod="Max", getValueMethod="Average"):
        self.debug_print("Debug: _getTableStats in FWS.py")
        stats = tree.stats.get(element, tr, area, mergeMethod=mergeMethod)
        if stats is None:
            return None
        return self.getValue(stats, getValueMethod)

    def _sky_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _sky_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        sky = self._getTableStats(tree, "Sky", timeRange, node.getAreaLabel())
        if sky is None:
            value = "M"
        elif self._elementFormatDict.get("Sky", "numeric") == "numeric":
            value = str(int(sky + 0.5))
        else:
            for threshold, shortVal, longVal in self._skyTableValues():
                if sky <= threshold:
                    if colWidth <= 4:
                        value = shortVal
                    else:
                        value = longVal
                    break
        return value

    def _numSky_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _numSky_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        sky = self._getTableStats(tree, "Sky", timeRange, node.getAreaLabel())
        if sky is None:
            value = "M"
        else:
            value = str(int(sky + 0.5))
        return value

    def _skyTableValues(self):
        self.debug_print("Debug: _skyTableValues in FWS.py")
        return [
            (5, "CLR", "CLEAR"),
            (25, "MCR", "MCLEAR"),
            (50, "PC", "PCLDY"),
            (69, "MC", "MCLDY"),
            (87, "MC", "MCLDY"),
            (100, "CDY", "CLOUDY"),
        ]

    def _weatherType_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _weatherType_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        areaLabel = node.getAreaLabel()
        wxStats = tree.stats.get("Wx", timeRange, areaLabel, mergeMethod="Max")
        if wxStats is None or len(wxStats) == 0:
            return "M"
        # If there are 2 subkeys tied for the highest rank,
        #   search for a weather mix table.
        # Otherwise, use the weather code table to find the weather value
        coRank = None
        hiRank = -1
        for subkey, rank in wxStats:
            # Find top 2 ranked subkeys to look for mixtures
            if rank > hiRank and subkey.wxType() != "T":
                hiKey = subkey
                hiRank = rank
            elif rank == hiRank and subkey.wxType() != "T":
                coKey = subkey
                coRank = rank
        if hiRank == -1:
            return ""
        keyAttrs = hiKey.attributes()
        keyType = hiKey.wxType()
        if coRank == hiRank:
            keyAttrs, keyType = self._matchMix(hiKey, coKey, colWidth)
        value = self._matchType(keyAttrs, keyType, colWidth)
        return value

    def _matchType(self, keyAttrs, keyType, colWidth):
        self.debug_print("Debug: _matchType in FWS.py")
        # Try to match the weatherCodeTable to the subkey
        # If no match found, return None
        value = None
        for wxAttr, wxType, shortVal, longVal in self._weatherCodeTable():
            if wxAttr == "" or wxAttr in keyAttrs:
                if wxType == keyType:
                    if colWidth == 4:
                        value = shortVal
                    else:
                        value = longVal
                    break
        if value is None:
            if colWidth == 4:
                value = "???"
            else:
                value = "??????"
        return value

    def _matchMix(self, hiKey, coKey, colWidth):
        self.debug_print("Debug: _matchMix in FWS.py")
        # Try to match the weather mix
        # Return the attribute and wxType
        # If not found, return the hiKey attributes and wxType
        for attr1, type1, attr2, type2, keyAttr, keyType in self._weatherMixTable():
            for key1, key2 in [(hiKey, coKey), (coKey, hiKey)]:
                if type1 == key1.wxType() and type2 == key2.wxType():
                    if len(key1.attributes()) == 0 and \
                       len(key2.attributes()) == 0 and \
                       attr1 == "" and attr2 == "":
                        # Match found
                        return [keyAttr], keyType
                    elif len(key1.attributes()) == 0 and \
                            len(key2.attributes()) != 0 and \
                            attr1 == "" and attr2 in key2.attributes():
                        # Match found
                        return [keyAttr], keyType
                    elif len(key1.attributes()) != 0 and \
                            len(key2.attributes()) == 0 and \
                            attr1 in key1.attributes() and attr2 == "":
                        # Match found
                        return [keyAttr], keyType
                    elif len(key1.attributes()) != 0 and \
                            len(key2.attributes()) != 0 and \
                            attr1 in key1.attributes() and \
                            attr2 in key2.attributes():
                        # Match found
                        return [keyAttr], keyType

        # No match found
        return hiKey.attributes(), hiKey.wxType()

    def _weatherCodeTable(self):
        self.debug_print("Debug: _weatherCodeTable in FWS.py")
        return [
            ("", "<NoWx>", "", "NONE"),
            ("Dry", "T", "DYT", "DRYTSM"),
            ("", "T", "TSM", "TSTORM"),
            ("GW", "T", "TSM", "TSTORM"),
            ("SmA", "T", "TSM", "TSTORM"),
            ("", "S", "SN", "SNOW"),
            ("", "R", "RN", "RAIN"),
            ("", "SW", "SW", "SNSHWR"),
            ("", "RW", "RW", "RNSHWR"),
            ("", "L", "DZL", "DRZL"),
            ("", "ZR", "FZR", "FZRAIN"),
            ("", "ZL", "FZD", "FZDRZL"),
            ("", "IP", "SLT", "SLEET"),
            ("", "F", "FOG", "FOG"),
            ("", "ZF", "FZF", "FZFOG"),
            ("", "IF", "IFG", "ICEFOG"),
            ("", "IC", "ICR", "ICECRL"),
            ("", "H", "HAZ", "HAZE"),
            ("", "BS", "BSN", "BLSNOW"),
            ("", "BN", "BSD", "BLSAND"),
            ("", "BD", "BDT", "BLDUST"),
            ("", "K", "SMK", "SMOKE"),
            ("", "FR", "FST", "FROST"),
            ("", "ZY", "FZS", "FZSPRY"),
            ("", "VA", "ASH", "VOLASH"),
            # Mixed Weather Types
            ("", "RS", "RS", "RNSN"),
            ("", "LF", "DZF", "DZL/FG"),
            ("", "SF", "SNF", "SN/FG "),
            ("", "RF", "RNF", "RN/FG "),
            ("", "ZRS", "ZRS", "ZRN/SN"),
            # Unknown Mixed Weather Type
            ("", "XX", "???", "??????"),
        ]

    def _weatherMixTable(self):
        self.debug_print("Debug: _weatherMixTable in FWS.py")
        return [
            ("", "S", "", "R", "", "RS"),
            ("", "SW", "", "RW", "", "RS"),
            ("", "RW", "", "T", "", "T"),
            ("Dry", "T", "", "RW", "Dry", "T"),
            ("", "L", "", "F", "", "LF"),
            ("", "S", "", "F", "", "SF"),
            ("", "R", "", "F", "", "RF"),
            ("", "SW", "", "F", "", "SF"),
            ("", "RW", "", "F", "", "RF"),
            ("", "ZR", "", "S", "", "ZRS"),
            ("", "ZR", "", "SW", "", "ZRS"),
        ]

    def _tstmCov_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _tstmCov_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        areaLabel = node.getAreaLabel()
        wxStats = tree.stats.get("Wx", timeRange, areaLabel, mergeMethod="Max")
        if wxStats is None or len(wxStats) == 0:
            return "M"
        hiRank = -1
        for subkey, rank in wxStats:
            if rank > hiRank and subkey.wxType() == "T":
                hiKey = subkey
                hiRank = rank
        if hiRank == -1:
            return ""
        value = None
        for cov, shortVal, longVal in self._coverageCodeTable():
            if hiKey.coverage() == cov:
                if colWidth == 4:
                    value = shortVal
                else:
                    value = longVal
                break
        if value is None:
            if colWidth == 4:
                value = "???"
            else:
                value = "??????"
        return value

    def _weatherCov_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _weatherCov_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        areaLabel = node.getAreaLabel()
        wxStats = tree.stats.get("Wx", timeRange, areaLabel, mergeMethod="Max")
        if wxStats is None or len(wxStats) == 0:
            return "M"
        hiRank = -1
        for subkey, rank in wxStats:
            if rank > hiRank and subkey.wxType() != "T":
                hiKey = subkey
                hiRank = rank
        if hiRank == -1:
            return ""
        value = None
        for cov, shortVal, longVal in self._coverageCodeTable():
            if hiKey.coverage() == cov:
                if colWidth == 4:
                    value = shortVal
                else:
                    value = longVal
                break
        if value is None:
            if colWidth == 4:
                value = "???"
            else:
                value = "??????"
        return value

    def _coverageCodeTable(self):
        self.debug_print("Debug: _coverageCodeTable in FWS.py")
        return [
            ("<NoCov>", "", ""),
            ("SChc", "SCH", "S CHC"),
            ("Iso", "ISO", "ISOLTD"),
            ("Chc", "CHC", "CHANCE"),
            ("Sct", "SCT", "SCTTRD"),
            ("Lkly", "LKY", "LIKELY"),
            ("Num", "NUM", "NUMRUS"),
            ("Def", "DEF", "DEFNTE"),
            ("Wide", "WID", "WIDSPD"),
            ("Ocnl", "OCL", "OCNL"),
            ("Frq", "FRQ", "FRQNT"),
            ("Brf", "BRF", "BRIEF"),
            ("Pds", "PDS", "PERIOD"),
            ("Inter", "ITR", "ITRMT"),
            ("Areas", "ARS", "AREAS"),
            ("Patchy", "PTY", "PATCHY")
        ]

    def _temp_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _temp_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        temp = self._getTableStats(tree, "T", timeRange, node.getAreaLabel())
        if temp is None:
            return "M"
        if temp >= 0:
            temp = int(temp + 0.5)
        else:
            temp = int(temp - 0.5)
        return str(temp)

    def _seaSurfaceTemperature_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _seaSurfaceTemperature_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        sst = self._getTableStats(
            tree, "SSTAK", timeRange, node.getAreaLabel())
        if sst is None:
            return "M"
        if sst >= 0:
            isst = int(sst + 0.5)
        else:
            isst = int(sst - 0.5)
        return str(isst)

    def _rh_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _rh_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        rh = self._getTableStats(tree, "RH", timeRange, node.getAreaLabel())
        if rh is None:
            return "M"
        rh = int(rh + 0.5)
        return str(rh)

    # Wind Methods
    # Utility for Wind Methods

    # Tabular Transport Wind bug found by John DeBlock and Stephen Miller.
    # tree.stats.get was using self._20ftWindParm instead of element.
    def _getWindDirSpdStr(self, tree, node, timeRange, element, formatElement=None, units=None):
        self.debug_print("Debug: _getWindDirSpdStr in FWS.py")
        if element == "Wind20ft" and self._20ftWindParm == "Wind20ft":
            windStats = tree.stats.get("Wind20ft", timeRange, node.getAreaLabel(),
                                       mergeMethod="Max")
        else:
            windStats = tree.stats.get(element, timeRange, node.getAreaLabel(),
                                       mergeMethod="Max")
        if windStats is None:
            return None
        wspd, wdir = windStats
        if formatElement is None:
            formatElement = element
        if self._elementFormatDict.get(formatElement, "alpha") == "alpha":
            wdir = int(wdir + 0.5)
            dirString = self._dirConvert(wdir)
        else:
            dir = int(wdir / 10.0 + 0.5) * 10
            if dir < 10:
                dirString = "00" + str(dir)
            elif dir < 100:
                dirString = "0" + str(dir)
            else:
                dirString = str(dir)
        if units == "Metric":
            wspd = int(wspd * .44704 + 0.5)
        else:
            if self._incidentType == "MARINE" and (element == "Wind" or element == "Wind20ft"):
                wspd = int(wspd / 1.15 + 0.5)
            else:
                wspd = int(wspd + 0.5)
        spdString = str(wspd)
        return dirString, spdString

    def _getWindNumDirSpdStr(self, tree, node, timeRange, element, formatElement=None, units=None):
        self.debug_print("Debug: _getWindNumDirSpdStr in FWS.py")
        if element == "Wind" and self._20ftWindParm == "Wind20ft":
            windStats = tree.stats.get("Wind20ft", timeRange, node.getAreaLabel(),
                                       mergeMethod="Max")
        else:
            windStats = tree.stats.get(element, timeRange, node.getAreaLabel(),
                                       mergeMethod="Max")
        if windStats is None:
            return None
        wspd, wdir = windStats
        if formatElement is None:
            formatElement = element
        dir = int(wdir / 10.0 + 0.5) * 10
        if dir < 10:
            dirString = "00" + str(dir)
        elif dir < 100:
            dirString = "0" + str(dir)
        else:
            dirString = str(dir)
        if units == "Metric":
            wspd = int(wspd * .44704 + 0.5)
        else:
            if self._incidentType == "MARINE" and element == "Wind":
                wspd = int(wspd / self._windAdjustmentFactor / 1.15 + 0.5)
            else:
                wspd = int(wspd + 0.5)
        spdString = str(wspd)
        return dirString, spdString

    def _getEyeWindDirSpdStr(self, tree, node, timeRange, element, formatElement=None, units=None):
        self.debug_print("Debug: _getEyeWindDirSpdStr in FWS.py")
        windStats = tree.stats.get(element, timeRange, node.getAreaLabel(),
                                   mergeMethod="Max")
        if windStats is None:
            return None
        wspd, wdir = windStats
        if formatElement is None:
            formatElement = element
        if self._elementFormatDict.get(formatElement, "alpha") == "alpha":
            wdir = int(wdir + 0.5)
            dirString = self._dirConvert(wdir)
        else:
            dir = int(wdir / 10.0 + 0.5) * 10
            if dir < 10:
                dirString = "00" + str(dir)
            elif dir < 100:
                dirString = "0" + str(dir)
            else:
                dirString = str(dir)
        if element == "Wind":
            if self._incidentType == "MARINE":
                wspd = wspd * self._eyeWindAdjustmentFactor / \
                    self._windAdjustmentFactor / 1.15
            else:
                wspd = wspd * self._eyeWindAdjustmentFactor / \
                    self._windAdjustmentFactor
        if units == "Metric":
            wspd = int(wspd * .44704 + 0.5)
        else:
            wspd = int(wspd + 0.5)
        spdString = str(wspd)
        return dirString, spdString

    def _getSfcWindDirSpdStr(self, tree, node, timeRange, element, formatElement=None, units=None):
        self.debug_print("Debug: _getSfcWindDirSpdStr in FWS.py")
        windStats = tree.stats.get(element, timeRange, node.getAreaLabel(),
                                   mergeMethod="Max")
        if windStats is None:
            return None
        wspd, wdir = windStats
        if formatElement is None:
            formatElement = element
        if self._elementFormatDict.get(formatElement, "alpha") == "alpha":
            wdir = int(wdir + 0.5)
            dirString = self._dirConvert(wdir)
        else:
            dir = int(wdir / 10.0 + 0.5) * 10
            if dir < 10:
                dirString = "00" + str(dir)
            elif dir < 100:
                dirString = "0" + str(dir)
            else:
                dirString = str(dir)
        if units == "Metric":
            wspd = int(wspd * .44704 + 0.5)
        else:
            if self._incidentType == "MARINE" and element == "Wind":
                wspd = int(wspd / self._windAdjustmentFactor / 1.15 + 0.5)
            elif formatElement == "SfcKtsWind":
                wspd = int(wspd / self._windAdjustmentFactor / 1.15 + 0.5)
            else:
                wspd = int(wspd / self._windAdjustmentFactor + 0.5)
        spdString = str(wspd)
        return dirString, spdString

    def _dirConvert(self, wdir):
        self.debug_print("Debug: _dirConvert in FWS.py")
        dirString = ""
        if wdir >= 338 or wdir <= 22:
            dirString = "N"
        elif wdir >= 23 and wdir <= 67:
            dirString = "NE"
        elif wdir >= 68 and wdir <= 112:
            dirString = "E"
        elif wdir >= 113 and wdir <= 157:
            dirString = "SE"
        elif wdir >= 158 and wdir <= 202:
            dirString = "S"
        elif wdir >= 203 and wdir <= 247:
            dirString = "SW"
        elif wdir >= 248 and wdir <= 292:
            dirString = "W"
        elif wdir >= 293 and wdir <= 337:
            dirString = "NW"
        return dirString

    def _adjustEyeWind(self, value):
        self.debug_print("Debug: _adjustEyeWind in FWS.py")
        # adjustment for winds
        factor = self.nlValue(self._eyeWindAdjustmentFactor, value)
        value = value * factor
        return value

    def _wind_value(self, statDict, timeRange, argList, element=None, formatElement=None):
        self.debug_print("Debug: _wind_value in FWS.py")
        if element is None:
            element = self._20ftWindParm
        if formatElement is None:
            formatElement = self._20ftWindParm
        tree, node, colWidth = tuple(argList)
        if formatElement == "TransMetWind":
            windString = self._getWindDirSpdStr(
                tree, node, timeRange, element, formatElement, "Metric")
        elif formatElement == "EyeWind":
            windString = self._getEyeWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        elif formatElement == "SfcWind":
            windString = self._getSfcWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        elif formatElement == "SfcKtsWind":
            windString = self._getSfcWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        else:
            windString = self._getWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        if windString is None:
            return "M"
        dirString, spdString = windString
        if self._elementFormatDict.get(formatElement, "alpha") == "alpha":
            value = dirString + " " + spdString
        else:
            value = dirString + "/" + spdString
        return value

    def _windWithGust_value(self, statDict, timeRange, argList, element=None, formatElement=None):
        self.debug_print("Debug: _windWithGust_value in FWS.py")
        if element is None:
            element = self._20ftWindParm
        if formatElement is None:
            formatElement = self._20ftWindParm
        tree, node, colWidth = tuple(argList)
        if formatElement == "EyeWind":
            windString = self._getEyeWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        elif formatElement == "SfcWind":
            windString = self._getSfcWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        elif formatElement == "SfcKtsWind":
            windString = self._getSfcWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        else:
            windString = self._getWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        if windString is None:
            return "M"
        dirString, spdString = windString
        gust = self._getTableStats(tree, "WindGust", timeRange, node.getAreaLabel(),
                                         getValueMethod="Max")
        if gust is None:
            gstString = "GMM"
        gstString = ""
        if self._incidentType == "MARINE":
            if formatElement == "SfcWind" or formatElement == "SfcKtsWind":
                gust = int(self.getValue(gust) / 1.15 + 0.5)
            elif formatElement == "EyeWind":
                gust = int(
                    self.getValue(gust) * self._eyeWindAdjustmentFactor / 1.15 + 0.5)
            else:
                gust = int(
                    self.getValue(gust) * self._windAdjustmentFactor / 1.15 + 0.5)
        else:
            if formatElement == "SfcWind":
                gust = int(self.getValue(gust) + 0.5)
            elif formatElement == "SfcKtsWind":
                gust = int(self.getValue(gust) / 1.15 + 0.5)
            elif formatElement == "EyeWind":
                gust = int(
                    self.getValue(gust) * self._eyeWindAdjustmentFactor + 0.5)
            else:
                gust = int(
                    self.getValue(gust) * self._windAdjustmentFactor + 0.5)
        if gust > int(spdString):
            gstString = "G" + str(gust)
        if self._elementFormatDict.get(formatElement, "alpha") == "alpha":
            value = dirString + " " + spdString + gstString
        else:
            value = dirString + "/" + spdString + gstString
        return value

    def _windDir_value(self, statDict, timeRange, argList, element=None, formatElement=None):
        self.debug_print("Debug: _windDir_value in FWS.py")
        if element is None:
            element = self._20ftWindParm
        if formatElement is None:
            formatElement = self._20ftWindParm
        tree, node, colWidth = tuple(argList)
        windString = self._getWindDirSpdStr(
            tree, node, timeRange, element, formatElement)
        if windString is None:
            return "M"
        dirString, spdString = windString
        return dirString

    def _windNumDir_value(self, statDict, timeRange, argList, element=None, formatElement=None):
        self.debug_print("Debug: _windNumDir_value in FWS.py")
        if element is None:
            element = self._20ftWindParm
        if formatElement is None:
            formatElement = self._20ftWindParm
        tree, node, colWidth = tuple(argList)
        windString = self._getWindNumDirSpdStr(
            tree, node, timeRange, element, formatElement)
        if windString is None:
            return "M"
        dirString, spdString = windString
        return dirString

    def _eyewindNumDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _eyewindNumDir_value in FWS.py")
        return self._windNumDir_value(statDict, timeRange, argList, "Wind", "EyeWind")

    def _sfcwind_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _sfcwind_value in FWS.py")
        return self._wind_value(statDict, timeRange, argList, "Wind", "SfcWind")

    def _sfcktswind_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _sfcktswind_value in FWS.py")
        return self._wind_value(statDict, timeRange, argList, "Wind", "SfcKtsWind")

    def _sfcwindWithGust_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _sfcwindWithGust_value in FWS.py")
        return self._windWithGust_value(statDict, timeRange, argList, "Wind", "SfcWind")

    def _sfcktswindWithGust_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _sfcktswindWithGust_value in FWS.py")
        return self._windWithGust_value(statDict, timeRange, argList, "Wind", "SfcKtsWind")

    def _sfcwindDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _sfcwindDir_value in FWS.py")
        return self._windDir_value(statDict, timeRange, argList, "Wind", "SfcWind")

    def _sfcktswindDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _sfcktswindDir_value in FWS.py")
        return self._windDir_value(statDict, timeRange, argList, "Wind", "SfcWind")

    def _sfcwindSpd_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _sfcwindSpd_value in FWS.py")
        return self._windSpd_value(statDict, timeRange, argList, "Wind", "SfcWind")

    def _sfcktswindSpd_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _sfcktswindSpd_value in FWS.py")
        return self._windSpd_value(statDict, timeRange, argList, "Wind", "SfcKtsWind")

    def _sfcwindGust_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _sfcwindGust_value in FWS.py")
        return self._windGust_value(statDict, timeRange, argList, "Wind", "SfcWind")

    def _sfcktswindGust_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _sfcktswindGust_value in FWS.py")
        return self._windGust_value(statDict, timeRange, argList, "Wind", "SfcKtsWind")

    def _sfcwindNumDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _sfcwindNumDir_value in FWS.py")
        return self._windNumDir_value(statDict, timeRange, argList, "Wind", "SfcWind")

    def _sfcktswindNumDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _sfcktswindNumDir_value in FWS.py")
        return self._windNumDir_value(statDict, timeRange, argList, "Wind", "SfcWind")

    def _ridgeNumDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _ridgeNumDir_value in FWS.py")
        return self._windNumDir_value(statDict, timeRange, argList, "FreeWind", "RidgeWind")

    def _windSpd_value(self, statDict, timeRange, argList, element=None, formatElement=None):
        self.debug_print("Debug: _windSpd_value in FWS.py")
        if element is None:
            element = self._20ftWindParm
        if formatElement is None:
            formatElement = self._20ftWindParm
        tree, node, colWidth = tuple(argList)
        if formatElement == "TransMetWind":
            windString = self._getWindDirSpdStr(
                tree, node, timeRange, element, formatElement, "Metric")
        elif formatElement == "EyeWind":
            windString = self._getEyeWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        elif formatElement == "SfcWind":
            windString = self._getSfcWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        elif formatElement == "SfcKtsWind":
            windString = self._getSfcWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        else:
            windString = self._getWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        if windString is None:
            return "M"
        dirString, spdString = windString
        return spdString

    def _windGust_value(self, statDict, timeRange, argList, element=None, formatElement=None):
        self.debug_print("Debug: _windGust_value in FWS.py")
        if element is None:
            element = self._20ftWindParm
        if formatElement is None:
            formatElement = self._20ftWindParm
        tree, node, colWidth = tuple(argList)
        if formatElement == "SfcWind":
            windString = self._getSfcWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        elif formatElement == "SfcKtsWind":
            windString = self._getSfcWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        elif formatElement == "EyeWind":
            windString = self._getEyeWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        else:
            windString = self._getWindDirSpdStr(
                tree, node, timeRange, element, formatElement)
        if windString is None:
            spdString = '0'
        else:
            dirString, spdString = windString
        gust = self._getTableStats(tree, "WindGust", timeRange, node.getAreaLabel(),
                                         getValueMethod="Max")
        if gust is None:
            return "M"
        gstString = " "
        if self._incidentType == "MARINE":
            if formatElement == "SfcWind" or formatElement == "SfcKtsWind":
                gust = int(self.getValue(gust) / 1.15 + 0.5)
            elif formatElement == "EyeWind":
                gust = int(
                    self.getValue(gust) * self._eyeWindAdjustmentFactor / 1.15 + 0.5)
            else:
                gust = int(
                    self.getValue(gust) * self._windAdjustmentFactor / 1.15 + 0.5)
        else:
            if formatElement == "SfcWind":
                gust = int(self.getValue(gust) + 0.5)
            elif formatElement == "SfcKtsWind":
                gust = int(self.getValue(gust) / 1.15 + 0.5)
            elif formatElement == "EyeWind":
                gust = int(
                    self.getValue(gust) * self._eyeWindAdjustmentFactor + 0.5)
            else:
                gust = int(
                    self.getValue(gust) * self._windAdjustmentFactor + 0.5)
        if gust > int(spdString):
            gstString = str(gust)
        return gstString

    def _eyewind_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _eyewind_value in FWS.py")
        return self._wind_value(statDict, timeRange, argList, "Wind", "EyeWind")

    def _eyewindWithGust_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _eyewindWithGust_value in FWS.py")
        return self._windWithGust_value(statDict, timeRange, argList, "Wind", "EyeWind")

    def _eyewindDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _eyewindDir_value in FWS.py")
        return self._windDir_value(statDict, timeRange, argList, "Wind", "EyeWind")

    def _eyewindSpd_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _eyewindSpd_value in FWS.py")
        return self._windSpd_value(statDict, timeRange, argList, "Wind", "EyeWind")

    def _eyewindGust_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _eyewindGust_value in FWS.py")
        return self._windGust_value(statDict, timeRange, argList, "Wind", "EyeWind")

    def _ridge_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _ridge_value in FWS.py")
        return self._wind_value(statDict, timeRange, argList, "FreeWind", "RidgeWind")

    def _ridgeDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _ridgeDir_value in FWS.py")
        return self._windDir_value(statDict, timeRange, argList, "FreeWind", "RidgeWind")

    def _ridgeSpd_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _ridgeSpd_value in FWS.py")
        return self._windSpd_value(statDict, timeRange, argList, "FreeWind", "RidgeWind")

    def _trans_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _trans_value in FWS.py")
        return self._wind_value(statDict, timeRange, argList, "TransWind", "TransWind")

    def _transDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _transDir_value in FWS.py")
        return self._windDir_value(statDict, timeRange, argList, "TransWind", "TransWind")

    def _transNumDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _transNumDir_value in FWS.py")
        return self._windNumDir_value(statDict, timeRange, argList, "TransWind", "TransWind")

    def _transSpd_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _transSpd_value in FWS.py")
        return self._windSpd_value(statDict, timeRange, argList, "TransWind", "TransWind")

    def _transMetric_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _transMetric_value in FWS.py")
        return self._wind_value(statDict, timeRange, argList, "TransWind", "TransMetWind")

    def _transSpdMetric_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _transSpdMetric_value in FWS.py")
        return self._windSpd_value(statDict, timeRange, argList, "TransWind", "TransMetWind")

    def _wind1000ft_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind1000ft_value in FWS.py")
        return self._wind_value(statDict, timeRange, argList, "Wind1000Ft", "Wind1000Ft")

    def _wind1000ftDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind1000ftDir_value in FWS.py")
        return self._windDir_value(statDict, timeRange, argList, "Wind1000Ft", "Wind1000Ft")

    def _wind1000ftNumDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind1000ftNumDir_value in FWS.py")
        return self._windNumDir_value(statDict, timeRange, argList, "Wind1000Ft", "Wind1000Ft")

    def _wind1000ftSpd_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind1000ftSpd_value in FWS.py")
        return self._windSpd_value(statDict, timeRange, argList, "Wind1000Ft", "Wind1000Ft")

    def _wind2000ft_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind2000ft_value in FWS.py")
        return self._wind_value(statDict, timeRange, argList, "Wind2000Ft", "Wind2000Ft")

    def _wind2000ftDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind2000ftDir_value in FWS.py")
        return self._windDir_value(statDict, timeRange, argList, "Wind2000Ft", "Wind2000Ft")

    def _wind2000ftNumDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind2000ftNumDir_value in FWS.py")
        return self._windNumDir_value(statDict, timeRange, argList, "Wind2000Ft", "Wind2000Ft")

    def _wind2000ftSpd_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind2000ftSpd_value in FWS.py")
        return self._windSpd_value(statDict, timeRange, argList, "Wind2000Ft", "Wind2000Ft")

    def _wind3000ft_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind3000ft_value in FWS.py")
        return self._wind_value(statDict, timeRange, argList, "Wind3000Ft", "Wind3000Ft")

    def _wind3000ftDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind3000ftDir_value in FWS.py")
        return self._windDir_value(statDict, timeRange, argList, "Wind3000Ft", "Wind3000Ft")

    def _wind3000ftNumDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind3000ftNumDir_value in FWS.py")
        return self._windNumDir_value(statDict, timeRange, argList, "Wind3000Ft", "Wind3000Ft")

    def _wind3000ftSpd_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind3000ftSpd_value in FWS.py")
        return self._windSpd_value(statDict, timeRange, argList, "Wind3000Ft", "Wind3000Ft")

    def _wind4000ft_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind4000ft_value in FWS.py")
        return self._wind_value(statDict, timeRange, argList, "Wind4000Ft", "Wind4000Ft")

    def _wind4000ftDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind4000ftDir_value in FWS.py")
        return self._windDir_value(statDict, timeRange, argList, "Wind4000Ft", "Wind4000Ft")

    def _wind4000ftNumDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind4000ftNumDir_value in FWS.py")
        return self._windNumDir_value(statDict, timeRange, argList, "Wind4000Ft", "Wind4000Ft")

    def _wind4000ftSpd_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind4000ftSpd_value in FWS.py")
        return self._windSpd_value(statDict, timeRange, argList, "Wind4000Ft", "Wind4000Ft")

    def _wind5000ft_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind5000ft_value in FWS.py")
        return self._wind_value(statDict, timeRange, argList, "Wind5000Ft", "Wind5000Ft")

    def _wind5000ftDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind5000ftDir_value in FWS.py")
        return self._windDir_value(statDict, timeRange, argList, "Wind5000Ft", "Wind5000Ft")

    def _wind5000ftNumDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind5000ftNumDir_value in FWS.py")
        return self._windNumDir_value(statDict, timeRange, argList, "Wind5000Ft", "Wind5000Ft")

    def _wind5000ftSpd_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind5000ftSpd_value in FWS.py")
        return self._windSpd_value(statDict, timeRange, argList, "Wind5000Ft", "Wind5000Ft")

    def _mixingHeight_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _mixingHeight_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        mix = self._getTableStats(
            tree, "MixHgt", timeRange, node.getAreaLabel())
        if mix is None:
            return "M"
        if self._tabularMixingHeightUnits == "ft" and colWidth != 4:
            mixft = int(mix / 100.0 + 0.5) * 100
            if mixft < 100:
                value = "BLW100"
            else:
                value = str(mixft)
        else:
            if mix < 50:
                mix = 100.0
            kmix = mix / 1000.0
            if kmix < 10:
                kmix = self.round(kmix, "Nearest", 0.1)
                value = f"{kmix:.1f}"
            else:
                kmix = int(kmix + 0.5)
                value = str(kmix)
        return value

    def _mixingHeightMsl_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _mixingHeightMsl_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        mix = self._getTableStats(
            tree, "MixHgt", timeRange, node.getAreaLabel())
        if mix is None:
            return "M"
        if self._tabularMixingHeightUnits == "ft" and colWidth != 4:
            mixft = int(mix / 100.0 + 0.5) * 100
            if mixft < 100:
                value = "BLW100"
            else:
                value = str(mixft)
        else:
            if mix < 50:
                mix = 100.0
            kmix = mix / 1000.0
            if kmix < 10:
                kmix = self.round(kmix, "Nearest", 0.1)
                value = f"{kmix:.1f}"
            else:
                kmix = int(kmix + 0.5)
                value = str(kmix)
        return value

    def _mixingHeightMetric_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _mixingHeightMetric_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        mix = self._getTableStats(
            tree, "MixHgt", timeRange, node.getAreaLabel())
        if mix is None:
            return "M"
        if self._tabularMixingHeightMetricUnits == "m" and colWidth != 4:
            mixMetric = mix * 0.3048
            mixRounded = int(mixMetric / 10.0 + 0.5) * 10
            if mixRounded < 10:
                value = "BLW10M"
            else:
                value = str(mixRounded)
        else:
            if mix < 330:
                mix = 330.0
            mixMetric = mix * 0.3048 / 1000.0
            if kmix < 10:
                kmix = self.round(mixMetric, "Nearest", 0.1)
                value = f"{kmix:.1f}"
            else:
                kmix = int(kmix + 0.5)
                value = str(kmix)
        return value

    def _cwr_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _cwr_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        cwr = self._getTableStats(
            tree, self._cwrParm, timeRange, node.getAreaLabel())
        if cwr is None:
            return "M"
        return str(int(cwr / 10 + 0.5) * 10)

    def _pop_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _pop_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        pop = self._getTableStats(tree, "PoP", timeRange, node.getAreaLabel())
        if pop is None:
            return "M"
        return str(int(pop / 10 + 0.5) * 10)

    def _chanceOfRain_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _chanceOfRain_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        pop = self._getTableStats(
            tree, "PotRain", timeRange, node.getAreaLabel())
        if pop is None:
            return "M"
        return str(int(pop / 10 + 0.5) * 10)

    def _chanceOfThunder_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _chanceOfThunder_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        pop = self._getTableStats(
            tree, "PotThunder", timeRange, node.getAreaLabel())
        if pop is None:
            return "M"
        return str(int(pop / 10 + 0.5) * 10)

    def _chanceOfLightning_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _chanceOfLightning_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        CoL = self._getTableStats(
            tree, "PotThunder", timeRange, node.getAreaLabel())
        if CoL is None:
            return "M"
        return str(int(CoL / 10 + 0.5) * 10)

    def _seaIceConcentration_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _seaIceConcentration_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        sic = self._getTableStats(tree, "IceC", timeRange, node.getAreaLabel())
        if sic is None:
            return "M"
        isic = int(sic * 100 + 0.5)
        if isic > 100:
            isic = 100
        elif isic < 0:
            isic = 0
        return str(isic)

    def _lal_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _lal_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        lal = self._getTableStats(tree, "LAL", timeRange, node.getAreaLabel())
        if lal is None:
            return "M"
        return str(int(lal + 0.5))

    def _dsi_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _dsi_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        dsi = self._getTableStats(tree, "DSI", timeRange, node.getAreaLabel())
        if dsi is None:
            return "M"
        return str(int(dsi + 0.5))

    def _ldsi_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _ldsi_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        dsi = self._getTableStats(tree, "LDSI", timeRange, node.getAreaLabel())
        if dsi is None:
            return "M"
        return str(int(dsi + 0.5))

    def _lvori_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _lvori_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        lvori = self._getTableStats(
            tree, "LVORI", timeRange, node.getAreaLabel())
        if lvori is None:
            return "M"
        return str(int(lvori + 0.5))

    def _adi_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _adi_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        adi = self._getTableStats(tree, "ADI", timeRange, node.getAreaLabel())
        if adi is None:
            return "M"
        return str(int(adi + 0.5))

    def _gfdi_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _gfdi_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        gfdi = self._getTableStats(
            tree, "GFDI", timeRange, node.getAreaLabel())
        if gfdi is None:
            return "M"
        return str(int(gfdi + 0.5))

    def _haines_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _haines_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        stats = self._getTableStats(
            tree, "Haines", timeRange, node.getAreaLabel())
        if stats is None:
            return "M"
        return str(int(stats + 0.5))

    def _hainesMid_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _hainesMid_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        stats = self._getTableStats(
            tree, "HainesMid", timeRange, node.getAreaLabel())
        if stats is None:
            return "M"
        return str(int(stats + 0.5))

    def _ventrate_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _ventrate_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        ventrate = self._getTableStats(
            tree, "VentRate", timeRange, node.getAreaLabel())
        if ventrate is None:
            return "M"
        return str(int(ventrate / 1000.0 + 0.5))

    def _ventrate_cat(self, statDict, timeRange, argList):
        self.debug_print("Debug: _ventrate_cat in FWS.py")
        tree, node, colWidth = tuple(argList)
        ventrate = self._getTableStats(
            tree, "VentRate", timeRange, node.getAreaLabel())
        if ventrate is None:
            return "M"
        else:
            if colWidth > 4:
                if ventrate < 40000:
                    ventrate = "POOR"
                elif ventrate >= 40000 and ventrate < 60000:
                    ventrate = "FAIR"
                elif ventrate >= 60000 and ventrate < 100000:
                    ventrate = "GOOD"
                elif ventrate >= 100000 and ventrate < 150000:
                    ventrate = "VYGD"
                elif ventrate >= 150000:
                    ventrate = "EXNT"
            else:
                if ventrate < 40000:
                    ventrate = "PR"
                elif ventrate >= 40000 and ventrate < 60000:
                    ventrate = "FR"
                elif ventrate >= 60000 and ventrate < 100000:
                    ventrate = "GD"
                elif ventrate >= 100000 and ventrate < 150000:
                    ventrate = "VGD"
                elif ventrate >= 150000:
                    ventrate = "EXL"

        return ventrate

    def _windWave_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _windWave_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        stats = self._getTableStats(
            tree, "WindWaveHgt", timeRange, node.getAreaLabel())
        if stats is None:
            return "M"
        return str(int(stats + 0.5))

    def _waveHeight_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _windHeight_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        stats = self._getTableStats(
            tree, "WaveHeight", timeRange, node.getAreaLabel())
        if stats is None:
            return "M"
        return str(int(stats + 0.5))

    def _hiOneTenth_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _hiOneTenth_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        stats = self._getTableStats(
            tree, "HiOneTenth", timeRange, node.getAreaLabel())
        if stats is None:
            return "M"
        return str(int(stats + 0.5))

    def _swellPeriod_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _swellPeriod_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        stats = self._getTableStats(
            tree, "Period", timeRange, node.getAreaLabel())
        if stats is None:
            return "M"
        return str(int(stats + 0.5))

    def _wavePeriod_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wavePeriod_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        stats = self._getTableStats(
            tree, "Period", timeRange, node.getAreaLabel())
        if stats is None:
            return "M"
        return str(int(stats + 0.5))

    def _swell_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _swell_value in FWS.py")
        return self._wind_value(statDict, timeRange, argList, "Swell", "RidgeWind")

    def _swellDir_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _swellDir_value in FWS.py")
        return self._windDir_value(statDict, timeRange, argList, "Swell", "RidgeWind")

    def _swellHgt_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _swellHgt_value in FWS.py")
        return self._windSpd_value(statDict, timeRange, argList, "Swell", "RidgeWind")

    def _freezingLevel_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _freezingLevel_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        mix = self._getTableStats(
            tree, "FzLevel", timeRange, node.getAreaLabel())
        if mix is None:
            return "M"
        if mix < 50:
            mix = 100.0
        kmix = mix / 1000.0
        if kmix < 10:
            kmix = self.round(kmix, "Nearest", 0.1)
            value = f"{kmix:.1f}"
        else:
            kmix = int(kmix + 0.5)
            value = str(kmix)
        return value

    def _snowLevel_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _snowLevel_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        mix = self._getTableStats(
            tree, "SnowLevel", timeRange, node.getAreaLabel())
        if mix is None:
            return "M"
        if mix < 50:
            mix = 100.0
        kmix = mix / 1000.0
        if kmix < 10:
            kmix = self.round(kmix, "Nearest", 0.1)
            value = f"{kmix:.1f}"
        else:
            kmix = int(kmix + 0.5)
            value = str(kmix)
        return value

    def _ceiling_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _ceiling_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        temp = self._getTableStats(
            tree, "CloudBasePrimary", timeRange, node.getAreaLabel())
        if temp is None:
            return " "
        temp = temp / 10.0
        if temp < 10:
            tempWords = "%4.1f" % temp
            tempWords = tempWords.strip()
        else:
            tempWords = str(int(temp + 0.5))
        return tempWords

    def _visibility_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _visibility_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        temp = self._getTableStats(
            tree, "Visibility", timeRange, node.getAreaLabel())
        if temp is None:
            return " "
        if colWidth > 4:
            if temp < 1.0:
                tempWords = "%4.2f" % temp
                tempWords = tempWords.strip()
            elif temp >= 1.0 and temp < 3.0:
                tempWords = "%4.1f" % temp
                tempWords = tempWords.strip()
            else:
                tempWords = str(int(temp + 0.5))
        else:
            if temp < 1.0:
                tempWords = "%3.2f" % temp
                tempWords = tempWords.strip()
                tempWords = tempWords[1:]
            elif temp >= 1.0 and temp < 3.0:
                tempWords = "%3.1f" % temp
                tempWords = tempWords.strip()
            else:
                tempWords = str(int(temp + 0.5))
        return tempWords

    def _pressure_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _pressure_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        pressure = self._getTableStats(
            tree, "Pres", timeRange, node.getAreaLabel())
        if pressure is None:
            return " "
        if pressure < 40.0:
            inHg = float(int(float(pressure) * 100.0 + 0.5)) / 100.0
        else:
            inHg = float(int(float(pressure) * 2.953 + 0.5)) / 100.0
        if colWidth > 4:
            pressureWords = "%5.2f" % inHg
            pressureWords = pressureWords.strip()
        else:
            if inHg < 30.0:
                inHgCode = int(inHg * 100 + 0.5) - 2000
            else:
                inHgCode = int(inHg * 100 + 0.5) - 3000
            pressureWords = "%3.3i" % inHgCode
            pressureWords = pressureWords.strip()
        return pressureWords

    def _freezingSpray_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _freezingSpray_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        fsRaw = self._getTableStats(
            tree, "FrzngSpry", timeRange, node.getAreaLabel())
        if fsRaw is None:
            return " "
        if isinstance(fsRaw, list):
            fs = fsRaw[0]
        else:
            fs = fsRaw
        if colWidth > 4:
            fsWords = fs
        else:
            if fs == "Heavy":
                fsWords = "HVY"
            elif fs == "Light":
                fsWords = "LGT"
            else:
                fsWords = " "
        return fsWords

    def _icing_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _icing_value in FWS.py")
        return " "

    def _wind100ft_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wind100ft_value in FWS.py")
        return " "

    def _riverLevel_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _riverLevel_value in FWS.py")
        return " "

    def _riverTemperature_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _riverTemperature_value in FWS.py")
        return " "

    def _waterTemperature_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _waterTemperature_value in FWS.py")
        return " "

    def _td_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _td_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        temp = self._getTableStats(tree, "Td", timeRange, node.getAreaLabel())
        if temp is None:
            return "M"
        if temp >= 0:
            temp = int(temp + 0.5)
        else:
            temp = int(temp - 0.5)
        return str(temp)

    def _heatIndex_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _heatIndex_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        temp = self._getTableStats(
            tree, "HeatIndex", timeRange, node.getAreaLabel())
        if temp is None:
            return "M"
        if temp >= 0:
            temp = int(temp + 0.5)
        else:
            temp = int(temp - 0.5)
        return str(temp)

    def _windChill_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _windChill_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        temp = self._getTableStats(
            tree, "WindChill", timeRange, node.getAreaLabel())
        if temp is None:
            return "M"
        if temp >= 0:
            temp = int(temp + 0.5)
        else:
            temp = int(temp - 0.5)
        return str(temp)

    def _apparentTemperature_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _apparentTempeature_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        temp = self._getTableStats(
            tree, "ApparentT", timeRange, node.getAreaLabel())
        if temp is None:
            return "M"
        if temp >= 0:
            temp = int(temp + 0.5)
        else:
            temp = int(temp - 0.5)
        return str(temp)

    def _wwa_exclude(self, stats):
        self.debug_print("Debug: _wwa_exclude in FWS.py")
        list = []
        index = 0
        newstats = []
        while index < len(stats):
            eidx = 0
            flag = 1
            while eidx < len(list):
                if stats[index] == list[eidx]:
                    flag = 0
                eidx = eidx + 1
            if flag:
                newstats.append(stats[index])
            index = index + 1
        return newstats

    def _wwa_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wwa_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        stats = self._getTableStats(
            tree, "Hazards", timeRange, node.getAreaLabel())
        if stats is None:
            return " "
        if stats[0] == "<None>":
            return " "
        stats = self._wwa_exclude(stats)
        return stats[0][0:2] + stats[0][3:4]

    def _wwa2_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wwa2_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        stats = self._getTableStats(
            tree, "Hazards", timeRange, node.getAreaLabel())
        if stats is None:
            return " "
        stats = self._wwa_exclude(stats)
        if len(stats) < 2:
            return " "
        return stats[1][0:2] + stats[1][3:4]

    def _wwa3_value(self, statDict, timeRange, argList):
        self.debug_print("Debug: _wwa3_value in FWS.py")
        tree, node, colWidth = tuple(argList)
        stats = self._getTableStats(
            tree, "Hazards", timeRange, node.getAreaLabel())
        if stats is None:
            return " "
        stats = self._wwa_exclude(stats)
        if len(stats) < 3:
            return " "
        return stats[2][0:2] + stats[2][3:4]

    ### NEW NARRATIVE PHRASES ###

    def dsi_phrase(self):
        self.debug_print("Debug: dsi_phrase in FWS.py")
        return {
            "setUpMethod": self.dsi_setUp,
            "wordMethod": self.dsi_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def dsi_setUp(self, tree, node):
        self.debug_print("Debug: dsi_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("DSI", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "DSI.................")
        return self.DONE()

    def dsi_words(self, tree, node):
        "Create phrase Probability of Precipitation"
        self.debug_print("Debug: dsi_words in FWS.py")
        statDict = node.getStatDict()
        dsi = self.getStats(statDict, "DSI")
        if dsi is None:
            return self.setWords(node.parent, "MISSING")
        dsi = self.getValue(dsi)
        words = str(int(dsi + 0.5))
        return self.setWords(node, words)

    def ldsi_phrase(self):
        self.debug_print("Debug: ldsi_phrase in FWS.py")
        return {
            "setUpMethod": self.ldsi_setUp,
            "wordMethod": self.ldsi_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def ldsi_setUp(self, tree, node):
        self.debug_print("Debug: ldsi_setUp in FWS.py")
        #elementInfoList = [self.ElementInfo("DSI", "List")]
        elementInfoList = [self.ElementInfo("LDSI", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "LDSI................")
        return self.DONE()

    def ldsi_words(self, tree, node):
        "Create phrase Probability of Precipitation"
        self.debug_print("Debug: ldsi_words in FWS.py")
        statDict = node.getStatDict()
        ldsi = self.getStats(statDict, "LDSI")
        if ldsi is None:
            return self.setWords(node.parent, "MISSING")
        ldsi = self.getValue(ldsi)
        words = str(int(ldsi + 0.5))
        return self.setWords(node, words)

    def lvori_phrase(self):
        self.debug_print("Debug: lvori_phrase in FWS.py")
        return {
            "setUpMethod": self.lvori_setUp,
            "wordMethod": self.lvori_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def lvori_setUp(self, tree, node):
        self.debug_print("Debug: lvori_setUp in FWS.py")
        #elementInfoList = [self.ElementInfo("DSI", "List")]
        elementInfoList = [self.ElementInfo("LVORI", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "LVORI...............")
        return self.DONE()

    def lvori_words(self, tree, node):
        self.debug_print("Debug: lvori_words in FWS.py")
        statDict = node.getStatDict()
        lvori = self.getStats(statDict, "LVORI")
        if lvori is None:
            return self.setWords(node.parent, "MISSING")
        lvori = self.getValue(lvori)
        words = str(int(lvori + 0.5))
        return self.setWords(node, words)

    ### *** END TABULAR TEST SECTION HERE *** ###

# I had to create these phrases or labels so the Spot formatter will work
# for any WFO out of the baseline. I created labels for elements that
# grids are not created for (that I know of). If offices do have grids
# for these elements, then they can create the phrase to get it into
# the Spot product.

    # For WIND SHIFT. Just need the label since there is not phrase.
    def fireWindShift_label_phrase(self):
        self.debug_print("Debug: fireWindShift_label_phrase in FWS.py")
        return {
            "setUpMethod": self.fireWindShift_label_setUp,
            "phraseMethods": [self.postProcessPhrase],
        }

    def fireWindShift_label_setUp(self, tree, node):
        self.debug_print("Debug: fireWindShift_label_setUp in FWS.py")
        self.setWords(node, "")
        node.set("descriptor", "")
        node.set("indentLabel", "WIND SHIFT..........")
        return self.DONE()

    # For Sunrise and Sunset lines. Just need the label since there is not
    # phrase.
    def sunriseSunset_label_phrase(self):
        self.debug_print("Debug: sunriseSunset_label_phrase in FWS.py")
        return {
            "setUpMethod": self.sunriseSunset_label_setUp,
            "phraseMethods": [self.postProcessPhrase],
        }

    def sunriseSunset_label_setUp(self, tree, node):
        self.debug_print("Debug: sunriseSunset_label_setup in FWS.py")
        self.setWords(node, "")
        node.set("descriptor", "")
        dayNight = self.getPeriod(node.getTimeRange(), 1)
        if dayNight == self.DAYTIME():
            node.set("indentLabel", "SUNSET..............")
        else:
            node.set("indentLabel", "SUNRISE.............")
        return self.DONE()

    # For Moonlight lines. Just need the label since there is not phrase.
    def moonlight_label_phrase(self):
        self.debug_print("Debug: moonlight_label_phrase in FWS.py")
        return {
            "setUpMethod": self.moonlight_label_setUp,
            "phraseMethods": [self.postProcessPhrase],
        }

    def moonlight_label_setUp(self, tree, node):
        self.debug_print("Debug: moonlight_label_setUp in FWS.py")
        self.setWords(node, "")
        node.set("descriptor", "")
        dayNight = self.getPeriod(node.getTimeRange(), 1)
        node.set("indentLabel", "MOONLIGHT...........")
        return self.DONE()

    # For Tides lines. Just need the label since there is not phrase.
    def tides_label_phrase(self):
        self.debug_print("Debug: tides_label_phrase in FWS.py")
        return {
            "setUpMethod": self.tides_label_setUp,
            "phraseMethods": [self.postProcessPhrase],
        }

    def tides_label_setUp(self, tree, node):
        self.debug_print("Debug: tides_label_setUp in FWS.py")
        self.setWords(node, "")
        node.set("descriptor", "")
        dayNight = self.getPeriod(node.getTimeRange(), 1)
        node.set("indentLabel", "TIDES...............")
        return self.DONE()

    # For Inversion Setup and Burnoff lines. Just need the label since there
    # is not phrase.
    def inversionSetupBurnoff_label_phrase(self):
        self.debug_print("Debug: inversionSetupBurnoff_label_phrase in FWS.py")
        return {
            "setUpMethod": self.inversionSetupBurnoff_label_setUp,
            "phraseMethods": [self.postProcessPhrase],
        }

    def inversionSetupBurnoff_label_setUp(self, tree, node):
        self.debug_print("Debug: inversionSetupBurnoff_label_setup in FWS.py")
        self.setWords(node, "")
        node.set("descriptor", "")
        dayNight = self.getPeriod(node.getTimeRange(), 1)
        if dayNight == self.DAYTIME():
            node.set("indentLabel", "INVERSION BURNOFF...")
        else:
            node.set("indentLabel", "INVERSION SETUP.....")
        return self.DONE()

    # For Surrounding Ridge Wind.
    def surroundingRidgeWind_phrase(self):
        self.debug_print("Debug: surroundingRidgeWind_label_phrase in FWS.py")
        return {
            "setUpMethod": self.surroundingRidgeWind_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def surroundingRidgeWind_setUp(self, tree, node):
        self.debug_print("Debug: surroundingRidgeWind_label_setUp in FWS.py")
        self.wind_setUp(tree, node, gustFlag=0, element="FreeWind")
        node.set("descriptor", "")
        node.set("indentLabel", "SURROUNDING RIDGE...")
        return self.DONE()

    # For Chance of Preciptiation.
    def pop_phrase(self):
        self.debug_print("Debug: pop_phrase in FWS.py")
        return {
            "setUpMethod": self.pop_setUp,
            "wordMethod": self.pop_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def pop_setUp(self, tree, node):
        self.debug_print("Debug: pop_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("PoP", "Max")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "POP.................")
        return self.DONE()

    def pop_words(self, tree, node):
        self.debug_print("Debug: pop_words in FWS.py")
        statDict = node.getStatDict()
        popStats = self.getStats(statDict, "PoP")
        if popStats is None:
            return self.setWords(node.parent, "MISSING")
        pop = self.getValue(popStats)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, "PoP", "PoP"), pop)
        if int(pop) < threshold:
            return self.setWords(node, "null")
        else:
            words = str(int(pop)) + " percent"
        return self.setWords(node, words)

    def chanceOfRain_phrase(self):
        self.debug_print("Debug: chanceOfRain_phrase in FWS.py")
        return {
            "setUpMethod": self.chanceOfRain_setUp,
            "wordMethod": self.chanceOfRain_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def chanceOfRain_setUp(self, tree, node):
        self.debug_print("Debug: chanceOfRain_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("PotRain", "Max")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "CHANCE OF RAIN......")
        return self.DONE()

    def chanceOfRain_words(self, tree, node):
        self.debug_print("Debug: chanceOfRain_words in FWS.py")
        statDict = node.getStatDict()
        popStats = self.getStats(statDict, "PotRain")
        if popStats is None:
            return self.setWords(node.parent, "MISSING")
        pop = self.getValue(popStats)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, "PoP", "PoP"), pop)
        if int(pop) < threshold:
            return self.setWords(node, "null")
        else:
            words = str(int(pop)) + " percent"
        return self.setWords(node, words)

    def chanceOfThunder_phrase(self):
        self.debug_print("Debug: chanceOfThunder_phrase in FWS.py")
        return {
            "setUpMethod": self.chanceOfThunder_setUp,
            "wordMethod": self.chanceOfThunder_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def chanceOfThunder_setUp(self, tree, node):
        self.debug_print("Debug: chanceOfThunder_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("PotThunder", "Max")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "CHANCE OF THUNDER...")
        return self.DONE()

    def chanceOfThunder_words(self, tree, node):
        self.debug_print("Debug: chanceOfThunder_words in FWS.py")
        statDict = node.getStatDict()
        popStats = self.getStats(statDict, "PotThunder")
        if popStats is None:
            return self.setWords(node.parent, "MISSING")
        pop = self.getValue(popStats)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, "PoP", "PoP"), pop)
        if int(pop) < threshold:
            return self.setWords(node, "null")
        else:
            words = str(int(pop)) + " percent"
        return self.setWords(node, words)

    def chanceOfLightning_phrase(self):
        self.debug_print("Debug: chanceOfLightning_phrase in FWS.py")
        return {
            "setUpMethod": self.chanceOfLightning_setUp,
            "wordMethod": self.chanceOfLightning_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def chanceOfLightning_setUp(self, tree, node):
        self.debug_print("Debug: chanceOfLightning_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("PotThunder", "Max")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "CHANCE OF LIGHTNING.")
        return self.DONE()

    def chanceOfLightning_words(self, tree, node):
        self.debug_print("Debug: chanceOfLightning_words in FWS.py")
        statDict = node.getStatDict()
        popStats = self.getStats(statDict, "PotThunder")
        if popStats is None:
            return self.setWords(node.parent, "MISSING")
        pop = self.getValue(popStats)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, "PoP", "PoP"), pop)
        if int(pop) < threshold:
            return self.setWords(node, "null")
        else:
            words = str(int(pop)) + " percent"
        return self.setWords(node, words)

    # For Stability Class.
    def stabilityClass_phrase(self):
        self.debug_print("Debug: stabilityClass_phrase in FWS.py")
        return {
            "setUpMethod": self.stabilityClass_setUp,
            "wordMethod": self.stabilityClass_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def stabilityClass_setUp(self, tree, node):
        self.debug_print("Debug: stabilityClass_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("Stability", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "STABILITY CLASS.....")
        return self.DONE()

    def stabilityClass_words(self, tree, node):
        "Create phrase Stability Class"
        self.debug_print("Debug: stabilityClass_words in FWS.py")
        statDict = node.getStatDict()
        stability = self.getStats(statDict, "Stability")
        if stability is None:
            return self.setWords(node.parent, "MISSING")
        words = str(int(self.getValue(stability)))
        return self.setWords(node, words)

    # For Marine Layer.
    def marineLayer_phrase(self):
        self.debug_print("Debug: marineLayer_phrase in FWS.py")
        return {
            "setUpMethod": self.marineLayer_setUp,
            "wordMethod": self.marineLayer_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def marineLayer_setUp(self, tree, node):
        self.debug_print("Debug: marineLayer_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("MarineLayer", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "MARINE LAYER........")
        return self.DONE()

    def marineLayer_words(self, tree, node):
        "Create phrase MarineLayer"
        self.debug_print("Debug: marineLayer_words in FWS.py")
        statDict = node.getStatDict()
        marineLayer = self.getStats(statDict, "MarineLayer")
        if marineLayer is None:
            return self.setWords(node.parent, "MISSING")
        elif marineLayer == 0:
            return self.setWords(node.parent, "none")
        words = str(int(self.getValue(marineLayer))) + "ft"
        return self.setWords(node, words)

    def td_phrase(self):
        self.debug_print("Debug: td_phrase in FWS.py")
        return {
            "setUpMethod": self.td_setUp,
            "wordMethod": self.td_words,
            "phraseMethods": self.standard_phraseMethods(),
        }

    def td_setUp(self, tree, node):
        self.debug_print("Debug: td_setUp in FWS.py")
        elementInfoList = [self.ElementInfo("Td", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "DEWPOINT............")
        return self.DONE()

    def td_words(self, tree, node):
        "Create phrase Td"
        self.debug_print("Debug: td_words in FWS.py")
        statDict = node.getStatDict()
        td = self.getValue(self.getStats(statDict, "Td"), "Avg")
        if td is None:
            return self.setWords(node.parent, "MISSING")
        words = str(int(td))
        return self.setWords(node, words)

    def upperSlopeWind_phrase(self):
        self.debug_print("Debug: upperSlopeWind_phrase in FWS.py")
        return {
            "setUpMethod": self.upperSlopeWind_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
        }

    def upperSlopeWind_setUp(self, tree, node):
        self.debug_print("Debug: upperSlopeWind_setUp in FWS.py")
        self.wind_setUp(tree, node, gustFlag=0, element="FreeWind")
        node.set("descriptor", "")
        node.set("indentLabel", " RIDGE/UPPER SLOPE..")
        return self.DONE()

    # For Begin/End of Preciptiation.
    def pcpnTiming_phrase(self):
        self.debug_print("Debug: pcpnTiming_phrase in FWS.py")
        return {
            "setUpMethod": self.pcpnTiming_setUp,
            "phraseMethods": [self.postProcessPhrase],
        }

    def pcpnTiming_setUp(self, tree, node):
        self.debug_print("Debug: pcpnTiming_setUp in FWS.py")
        self.setWords(node, "    ")
        node.set("descriptor", "")
        node.set("indentLabel", "BEGIN/END OF PCPN...")
        return self.DONE()

    def _checkStrs(self, checkStrings, inputStr, orderStrings=0, checkMode=1):
        self.debug_print("Debug: _checkStrs in FWS.py")
        # Check the inputStr for the list of checkStrings.
        # If a checkString is a tuple, at least one of the
        # given tuple strings must be found in the inputStr
        # If orderStrings == 1, the strings must occur in order in the inputStr
        # If checkMode == 0, the strings should NOT be found in the inputStr
        # Returns 1 if successful, the failed checkString if not.
        curIndex = -1
        for cStr in checkStrings:
            if isinstance(cStr, tuple):
                # Will pass if ANY of these strings are found
                # Not valid with checkMode of zero
                if not checkMode:
                    continue
                found = 0
                for subStr in cStr:
                    strIndex = inputStr.find(subStr)
                    if strIndex >= 0:
                        found = 1
                        break
                    else:
                        found = 0
                if not found:
                    return subStr
            else:
                # Must find exact string
                strIndex = inputStr.find(cStr)
                if strIndex < 0:
                    if checkMode:
                        return cStr
                else:
                    if not checkMode:
                        return cStr
            # Check the ordering
            if orderStrings:
                inputStr = inputStr[strIndex:]
        return 1

    # From TimeDescriptor. Eliminated until afternoon and added overnight
    # wording.
    def timePeriod_descriptor_list(self, tree, node):
        self.debug_print("Debug: timePeriod_descriptor_list in FWS.py")
        # Contains definition for localtime start/end times and phrase
        # Tuples, 0=startHrLT, 1=endHrLT, 2=phrase
        day = self.DAY()
        return [
            (day, (day + 3) % 24, "early in the morning"),  # 6a-9a
            (day, (day + 6) % 24, "in the morning"),  # 6a-noon
            (day, (day + 9) %
             24, "in the morning and early afternoon"),  # 6a-3p
            (day, (day + 12) % 24, ""),  # 6a-6p
            (day, (day + 15) % 24, "until early evening"),  # 6a-9p
            (day, (day + 18) % 24, "through the evening"),  # 6a-midnite

            ((day + 2) % 24, (day + 3) % 24, "early in the morning"),  # 8a-9a

            ((day + 3) % 24, (day + 6) % 24, "late in the morning"),  # 9a-noon
            ((day + 3) % 24, (day + 9) %
             24, "in the late morning and early afternoon"),  # 9a-3p
            ((day + 3) % 24, (day + 12) %
             24, "in the late morning and afternoon"),  # 9a-6p
            ((day + 3) % 24, (day + 15) %
             24, "until early evening"),  # 9a-9p
            ((day + 3) % 24, (day + 18) %
             24, "through the evening"),  # 9a-midnite

            ((day + 5) % 24, (day + 6) %
             24, "late in the morning"),  # 11a-noon

            ((day + 6) % 24, (day + 9) %
             24, "early in the afternoon"),  # noon-3p
            ((day + 6) % 24, (day + 12) %
             24, "in the afternoon"),  # noon-6p
            ((day + 6) % 24, (day + 15) %
             24, "in the afternoon and evening"),  # noon-9p
            ((day + 6) % 24, (day + 18) %
             24, "in the afternoon and evening"),  # noon-midnite

            ((day + 8) % 24, (day + 9) %
             24, "early in the afternoon"),  # 2pm-3pm

            #((day+8)%24, (day+11)%24, self.lateDay_descriptor),   # 2p-5p
            ((day + 8) % 24, (day + 11) %
             24, "late in the afternoon"),  # 2p-5p
            ((day + 8) % 24, (day + 14) %
             24, "early in the evening"),  # 2p-8p
            ((day + 8) % 24, (day + 17) %
             24, "in the evening"),  # 2p-11p
            ((day + 8) % 24, (day + 20) %
             24, "until early morning"),  # 2p-2a
            # 2p-5a
            ((day + 8) % 24, day, ""),

            #((day+9)%24, (day+12)%24, self.lateDay_descriptor),   # 3p-6p
            ((day + 9) % 24, (day + 12) %
             24, "late in the afternoon"),  # 3p-6p
            ((day + 9) % 24, (day + 15) %
             24, "early in the evening"),  # 3p-9p
            ((day + 9) % 24, (day + 18) %
             24, "in the evening"),  # 3p-midnite
            ((day + 9) % 24, (day + 21) %
             24, "until early morning"),  # 3p-3a
            # 3p-6a
            ((day + 9) % 24, day, ""),

            ((day + 10) % 24, (day + 12) %
             24, "late in the afternoon"),  # 4p-6p
            #((day+11)%24, (day+12)%24, self.lateDay_descriptor), # 5p-6p
            ((day + 11) % 24, (day + 12) %
             24, "late in the afternoon"),  # 5p-6p

            ((day + 12) % 24, (day + 15) %
             24, "early in the evening"),  # 6p-9p
            ((day + 12) % 24, (day + 18) %
             24, "in the evening"),  # 6p-midnite
            ((day + 12) % 24, (day + 21) %
             24, "in the evening and overnight"),  # 6p-3a
            # 6p-6a
            ((day + 12) % 24, day, ""),

            ((day + 14) % 24, (day + 15) %
             24, "early in the evening"),  # 8p-9p

            # 9p-midnite
            ((day + 15) % 24, (day + 18) % 24, "late in the evening"),
            ((day + 15) % 24, (day + 21) %
             24, "in the late evening and overnight"),  # 9p-3a
            # 9p-6a
            ((day + 15) % 24, day, "in the late evening and overnight"),

            ((day + 17) % 24, (day + 21) %
             24, "late in the evening"),  # 11p-midnight

            ((day + 18) % 24, (day + 21) %
             24, "overnight"),  # midnite-3a
            # midnite-6a
            ((day + 18) % 24, day, "overnight"),
            # midnite-noon
            ((day + 18) % 24, (day + 6) % 24, ""),

            ((day + 20) % 24, (day + 3) % 24, "overnight"),  # 2a-3a

            # 3a-6a
            ((day + 21) % 24, day, "after 3 am"),
            ((day + 21) % 24, (day + 3) %
             24, "early in the morning"),  # 3a-9a
            ((day + 21) % 24, (day + 6) %
             24, "early in the morning"),  # 3a-noon
            ((day + 21) % 24, (day + 9) %
             24, "until afternoon"),  # 3a-3p
            # 3a-6p
            ((day + 21) % 24, (day + 12) % 24, ""),
        ]

# For Testing
    # def getPreviousProduct(self, stqPil, searchString, version=0):
        # f = open("/home/eagle6/hansen/ui/middendorf/GTFSTQBYZ"+`version`, "r")
        # product = f.read()
        # f.close()
        # self.debug_print("returning", product)
        # return product

    def vector_dir_difference_dict(self, tree, node):
        self.debug_print("Debug: vector_dir_difference_dict in FWS.py")
        # Replaces WIND_DIR_DIFFERENCE
        # Direction difference.  If the difference between directions
        # for sub-ranges is greater than or equal to this value,
        # the different directions will be noted in the phrase.
        # Units are degrees
        return {
            "Wind": 45,  # degrees
            "Wind20ft": 60,  # degrees
            "TransWind": 60,  # mph
            "FreeWind": 60,  # mph
            "Swell": 60,  # degrees
            "Swell2": 60,  # degrees
            "otherwise": 60,
        }

    # Overriding PhraseBuilder.combineVectors to better handle wind direction changes in 1 hr
    # grids. When combining values, takes the average of all values from either the first value
    # or the last value that differed greatly enough to not trigger combining, to the current
    # one. The threshold used to determine whether a combination should occur is the "Wind"
    # value defined in vector_dir_difference_dict.

    def combineVectors(self, tree, phrase, subPhrase1, subPhrase2, elementName):
        mag1, dir1, dirStr1 = self.getVectorData(tree, subPhrase1, elementName, "MinMax")
        mag2, dir2, dirStr2 = self.getVectorData(tree, subPhrase2, elementName, "MinMax")
        if mag1 is None or mag2 is None or dir1 is None or dir2 is None or dirStr1 is None or dirStr2 is None:
            return 0, None

        min1, max1 = mag1
        min2, max2 = mag2

        try:
            if phrase != self.cvPhrase:
                self.vectorDataArr = []
                self.cvPhrase = phrase
        except:
            self.vectorDataArr = []
            self.cvPhrase = phrase

        if len(self.vectorDataArr) == 0:
            self.vectorDataArr.append((mag1, dir1, dirStr1))
            self.vectorDataArr.append((mag2, dir2, dirStr2))
            self.startingDir = dir1
        else:
            self.vectorDataArr.append((mag2, dir2, dirStr2))
        differenceFlag = self.checkVectorDifference(
            tree, subPhrase1, elementName, min1, max1, self.startingDir, min2, max2, dir2)
        if differenceFlag == 0:
            minArr = []
            maxArr = []
            dirsToAvg = []
            idx = 0
            while idx < len(self.vectorDataArr):
                minArr.append(self.vectorDataArr[idx][0][0])
                maxArr.append(self.vectorDataArr[idx][0][1])
                dirsToAvg.append(self.vectorDataArr[idx][1])
                idx += 1

            # if combine_singleValues=1, then wind speed ranges will be averaged into a single value
            # ("10-14mph" becomes "around 12mph")
            combine_singleValues = self.combine_singleValues_flag(
                tree, subPhrase1, elementName, elementName)
            if combine_singleValues == 1:
                newMag, newDir = self.vectorAverage((min(min1, min2), dir1), (max(max1, max2), dir2))
                newMag = self.roundStatistic(tree, subPhrase1, newMag, elementName)
                newValue = (newMag, newDir)
            else:
                # Combine using mins and maxs to catch slow trends
                newMin = min(minArr)
                newMax = max(maxArr)
                newMin, newMax = self.applyRanges(tree, phrase, newMin, newMax, elementName)
                sinsum = 0
                cossum = 0
                for direction in dirsToAvg:
                    sinsum += math.sin(math.radians(direction))
                    cossum += math.cos(math.radians(direction))
                newDir = math.degrees(math.atan2(sinsum, cossum))
                if newDir < 0:
                    newDir += 360
                if newDir > 360:
                    newDir -= 360
                newValue = ((newMin, newMax), newDir)
            return 1, newValue
        else:
            self.vectorDataArr = []
        return 0, None

