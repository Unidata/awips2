"""########################################################################
# Experimental TAF  -  TAF_XXX_Definition.TextUtility
#
#-------------------------------------------------------------------------
# Description:
#  This file sets up the site Product Definitions for the TAF formatter.
#-------------------------------------------------------------------------
# Copying:
#  This software is in the public domain, furnished "as is", without technical
#  support, and with no warranty, express or implied, as to its usefulness for
#  any purpose.
#-------------------------------------------------------------------------
# Version: 20180608
#-------------------------------------------------------------------------
# Authors:  GSD Digital Aviation Services Group
#
# Support Email: nws.digital.aviation.services@noaa.gov
#-------------------------------------------------------------------------
# Customization Points:
#
# REQUIRED CONFIGURATION ITEMS
#
#  Go to the "REQUIRED CONFIGURATION ITEMS" section below and review and modify
#  all items in that section.
########################################################################"""

#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum content of this file is the following Definition statement

Definition = {}

# End MAKE NO CHANGES HERE
#**********************************************************************

#----- WFO XXX TAF Definition -----
# Definition Statements must start in column 1.

#------------------------------------------------------------------------------
#------------------------REQUIRED CONFIGURATION ITEMS--------------------------
###############################################################################

# displayName -----------------------------------------------------------------
#   Set to whatever label (ie. "TAF") you would like displayed in the
#   formatter launcher.
Definition["displayName"] = "TAF"

# defaultEditAreas ------------------------------------------------------------
#   Each TAF site requires a default edit area in GFE. Specify this via a 16 km
#   edit area (10 statute miles) centered on the terminal latitude/longitude.
#   This is the list and location of your TAF stations. You will need to modify
#   this section and add your TAF sites. 
#   
#Definition["defaultEditAreas"] = [((42.36, -71.01, 16), "KBOS"),
#                                  ((42.47, -71.29, 16), "KBED"),
#                                  ((41.94, -72.68, 16), "KBDL"),
#                                 ]

# fullStationID ---------------------------------------------------------------
#   Specify the 4 letter full station identifier (ie. "KBOX" where BOX is
#   replaced with your site id).
Definition["fullStationID"] = "<fullStationID>"

# wmoID -----------------------------------------------------------------------
#   Specify the WMO ID code for the TAF collective (ie. "FTUS41").
Definition["wmoID"] = "<wmoID>"

# pil -------------------------------------------------------------------------
#   Specify the PIL identifier (ie. "TAFBOX" where BOX is replaced with your
#   site id).
Definition["pil"] = "TAF<site>"

# textdbPil -------------------------------------------------------------------
#   Specify the product ID for storing to the AWIPS text database (ie.
#   "cccTAFBOX" where BOX is replaced with your site id).
Definition["textdbPil"] = "<textdbPil>"[:3] + "TAF<site>"

# awipsWANPil -----------------------------------------------------------------
#   Specify the product ID for transmitting to the AWIPS WAN (ie. "KBOXTAFBOX"
#   where both BOX are replaced with your site id).
Definition["awipsWANPil"] = "<fullStationID>TAF<site>"

# tafLength -------------------------------------------------------------------
#   Define the length of each individual TAF issued by your office, in hours.
Definition["tafLength"] = {"KBOS":30,
                           "KBED":24,
                           "KBDL":30,
                           "KBAF":24,
                           "KORH":24,
                           "KMHT":24,
                           "KPVD":24,
                           "KFMH":24,
                           "KHYA":24,
                           "KACK":24,
                          }

# CACThresholds ---------------------------------------------------------------
#   Define the flight category thresholds for each airport - 
#   (CEILING, VISIBILITY).  CEILING values are in hundreds of feet AGL
#   (i.e. 2 = 200 ft) and VISIBILITY values are in decimal statute miles
#   (i.e. 0.50 = 1/2SM).  Each category, besides Cat F, is permitted
#   one tuple.  Cat F is defined as a list of
#   (CEILING, VISIBILITY, (START WIND DIR, END WIND DIR), COMMENT) items
#   for each local criterion.  The (START WIND DIR, END WIND DIR) tuple
#   is used to determine when this local criterion may be in force.  Wind
#   directions are always checked in a clockwise direction.  So if you
#   want to check an arc of 40 degrees around North, specify (340, 20)
#   instead of (20, 340) which would check the 320 degree arc passing
#   through South. The COMMENT is a string which can be used by the
#   formatter to provide additional feedback to the forecaster.  The
#   COMMENT may be omitted if desired.
#
#   Cat A - Airport Minimum
#   Cat B - Alternate Minimum
#   Cat C - IFR
#   Cat D - MVFR
#   Cat E - Fuel Alternate
#   Cat F - Other Thresholds
#
#   Here is an example for Logan International Airport in Boston, MA:
#
#   "KBOS": {"A":(2, 0.50),
#            "B":(7, 2),
#            "C":(10, 3),
#            "D":(30, 5),
#            "E":(20, 1),
#            "F":[(25, 3, (340, 80), "Loss of Visual Approach"),
#                 (14, 3, (340, 80), "Loss of Circling Approach"),
#                 (8, 2, (340, 80), "ILS Hold Points Assigned")]},
#   NOTE: These are examples. You must modify these base on criteria for your
#   TAF sites.
Definition["CACThresholds"] = {
    "KBOS": {"A":(2, 0.50), "B":(7, 2), "C":(10, 3), "D":(30, 5), "E":(20, 1),
             "F":[(25, 3, (340, 80), "Loss of Visual Approach"),
                  (14, 3, (340, 80), "Loss of Circling Approach"),
                  (8, 2, (340, 80), "ILS Hold Points Assigned")]},

    "KBED": {"A":(3, 1), "B":(7, 2), "C":(10, 3), "D":(30, 5), "E":(20, 3),
             "F":[]},
}

# weatherRules ----------------------------------------------------------------
#   These are the rules to determine how weather is reported in the TAF
#   output. There are three levels to the rules.
#
#   The first level of the rules specifies the specific weather types
#   that have special rules. The weather types are specified using GFE
#   types instead of TAF types (ie. R for rain instead of RA). The
#   "default" key will be used for all weather types that don't
#   explicitly have rules created for them. You can specify multiple
#   types at once (ie. "RW, SW") but "default" can't be combined with
#   anything.
#
#   The second level of the rules is the offset (in hours) from the
#   issuance time of the TAF to the time when the weather is projected to
#   occur. So if the TAF is being issued for 06Z and there is a
#   thunderstorm at 08Z, the offset would be 2. The thunderstorm might
#   occur during other hours as well and at each of those hours, the
#   offset is determined and the appropriate rule is selected. This means
#   that a thunderstorm (or any other weather event) that spans multiple
#   hours might be handled differently at different times during the
#   event. At this level of the rules, tuples are used as keys to specify
#   a range where the first hour in the range is inclusive and the second
#   hour in the range is exclusive. So if you have a key of (0, 6), it
#   will be used when the offset is 0, 1, 2, 3, 4, or 5 (but not 6). If
#   the offset is not in any of the tuple ranges specified, the "default"
#   key will be used instead.
#
#   The third level of the rules is the probability/coverage (with an
#   optional intensity specified) of the weather occurring. If both a
#   probability/coverage and an intensity are specified (ie. "Sct--"),
#   then the weather has to have both of those conditions in order for
#   the rule to be applied. If just probability/coverage is specified,
#   then that covers all of the intensities that don't have explicit
#   rules defined for them. So if you have a "Sct--" and "Sct", then the
#   "Sct--" would be used for light scattered weather and the "Sct" would
#   be used for scattered weather with no intensity, -, m or + intensity.
#   If the weather doesn't meet any of the specified
#   probabilities/coverages (with optional intensity), then the "default"
#   key will be used instead. It's important to note that the "default"
#   key is not allowed to have an intensity added to it. You can specify
#   multiple probabilities/coverages (with optional intensities) at once
#   (ie. "Chc, Sct+") but "default" can't be combined with anything else.
#
#   Basically what you are doing with these rules is creating a pairing
#   between conditions (time to weather and probability/coverage +
#   optional intensity) and classifier words to use when those conditions
#   are met. You can customize the weather types that have specific
#   rules, time frames, probabilities/coverages + optional intensities
#   and the classifier words in any way that makes sense for your office.
#
#   There are six ways to control the output for each weather type.
#   They are the weather classifier words:
#     1) "PREVAIL" - TAF weather type will be reported in the FM group.
#     2) "TEMPO"   - TAF weather type will be reported in a TEMPO group.
#     3) "PROB30"  - TAF weather type will be reported in a PROB30 group.
#     4) "VCTS"    - Vicinity thunderstorms will be reported in the
#                    FM group as VCTS (the actual weather type will not
#                    be shown).
#     5) "VCSH"    - Vicinity showers will be reported in the FM group
#                    as VCSH (the actual weather type will not be shown).
#     6) ""        - The weather type will not be reported at all.
#
#   When modifying and extending these rules, it's important to always
#   make sure that all levels of the dictionary have a 'default' key.
#
#   Examples using the default rules
#   ================================
#   TAF issuance time of 06Z with scattered thunderstorms at 08Z:
#       The offset would be 2 hours which is in the range specified by
#       (0, 3) and the probability/coverage is 'Sct', so the
#       thunderstorm would be classified as VCTS in the TAF output.
#
#   TAF issuance time of 18Z with likely thunderstorms at 02Z (next day):
#       The offset would be 8 hours which is in the range specified by
#       (3, 9) and the probability/coverage is 'Lkly' which isn't
#       a probability/coverage that has been specified so the 'default'
#       key would be used and the thunderstorm would be classified as
#       TEMPO in the TAF output.
#
#   Example of completely changing the rules
#   ========================================
#   This isn't meant to be a realistic example; it is just meant to show
#   that everything (weather types, offset times, probabilities/coverages
#   + optional intensities, classifier words) can be modified:
#
#   Definition["weatherRules"] = {
#       "R, S" : {
#           # Number of hours since Issuance time
#           (0, 3): {
#               # Probability/Coverage (Intensity optional)
#               'Sct, Chc--':  "PROB30",
#               'default':     ""},
#           (3, 4): {
#               'Wide':        "TEMPO",
#               'default':     "VCSH"},
#           'default': {
#               'default':     ""},
#       }
#
#       # The default key is always required
#       "default" : {
#           # Number of hours since Issuance time
#           'default': {
#               # Probability/Coverage (Intensity optional)
#               'default':     "PREVAIL"},
#       }
#   }
#Definition["weatherRules"] = {
#    #  Handle Thunderstorms specifically
#    "T": {
#        # Number of hours since Issuance time
#        (0, 3): {
#            # Probability/Coverage (Intensity optional)
#            'SChc, Iso':  "", # This isn't probable enough to show
#            'Chc,  Sct':  "VCTS",
#            'default':    "PREVAIL"},
#        (3, 9): {
#            'SChc, Iso':  "",
#            'Chc,  Sct':  "VCTS",
#            'default':    "TEMPO"},
#        'default': {
#            'SChc, Iso':  "", # Too far out, not probable enough to show
#            'Chc,  Sct':  "PROB30",
#            'default':    "VCTS"},
#    },
#
#    # Handle Fog specifically
#    "F": {
#        # Number of hours since Issuance time
#        'default': {
#            # Probability/Coverage (Intensity optional)
#            'default':    "PREVAIL"},
#    },
#
#    # Handle all other precipitation types
#    "default" : {
#        # Number of hours since Issuance time
#        (0, 3): {
#            # Probability/Coverage (Intensity optional)
#            'SChc, Iso':  "", # This isn't probable enough to show
#            'Chc,  Sct':  "VCSH",
#            'default':    "PREVAIL"},
#        (3, 9): {
#            'SChc, Iso':  "",
#            'Chc,  Sct':  "VCSH",
#            'default':    "TEMPO"},
#        'default': {
#            'SChc, Iso':  "", # Too far out, not probable enough to show
#            'Chc,  Sct':  "PROB30",
#            'default':    "PREVAIL"},
#    },
#}

# maxNonSignificantValue ------------------------------------------------------
#   A threshold for specifying the maximum significance rating that is
#   considered non-significant. Any significance rating of this value or less
#   is considered non-significant. This can be used by shortening algorithms
#   when trying to determine if it's safe to remove a line from the output. The
#   rating scale is from 0 (least significant) to 100 (most significant).
Definition["maxNonSignificantValue"] = 60


#------------------------------------------------------------------------------
#------------------------OPTIONAL CONFIGURATION ITEMS--------------------------
###############################################################################

#--------------------- == General Formatter Settings == -----------------------

# database --------------------------------------------------------------------
#   Specify the source database containing the grids to be used: ("Official",
#   "Fcst" or "ISC").
#Definition["database"] = "Official"

# outputFile ------------------------------------------------------------------
#   Specify the path for formatter output.
Definition["outputFile"] =  "/localapps/data/products/gridTAF.txt"

# autoSend --------------------------------------------------------------------
#   Set whether to automatically transmit product (1 = yes, 0 = no).
#Definition["autoSend"] = 1

# autoSendAddress -------------------------------------------------------------
#   Specify the transmission address.
#Definition["autoSendAddress"] = "000"

# autoStore -------------------------------------------------------------------
#   Set whether to store the product in the textDB (1 = yes, 0 = no).
#Definition["autoStore"] = 1

# autoWrite -------------------------------------------------------------------
#   Set whether to write the product to a file (1 = yes, 0 = no).
Definition["autoWrite"] = 1

# aggregateCov_arealThreshold -------------------------------------------------
#   Define percentage of area which must be covered by a weather key before
#   it's considered dominant for the whole area. (Baseline = 90.0)
Definition["aggregateCov_arealThreshold"] = 75.0

# debug -----------------------------------------------------------------------
#   Set whether to debug - there are three options for debugging:
#
#   1)  Method specific debugging
#           Set the dictionary value to '1' for each method key you want to
#           display debug info for
#
#   2)  Display ALL debug information
#           Replace the dictionary by uncommenting the line that hardcodes
#           the debug flag to be '1'
#
#   3)  Display NO debug information
#           Replace the dictionary by uncommenting the line that hardcodes
#           the debug flag to be '0'
#
#   Method-specific debugging
#   (contains only methods found in TAF and TAF_RR_Overrides)
#Definition["debug"] = {# To only show debug output for certain methods,
#                       # add the particular method names here. For each
#                       # method, list just the name of the method (not
#                       # the class) in quotes followed by a colon (:),
#                       # the number 1 (to turn debugging on) and then
#                       # a comma (,). So for instance, to turn on
#                       # debugging for just generateForecast (in
#                       # TextProduct) and comparePeriods (in
#                       # TextProduct.Consolidator), it would look like
#                       # this:
#                       #
#                       # "generateForecast":1,
#                       # "comparePeriods":1,
#                       #
#                       # Sites should avoid this feature and should
#                       # instead leave full debug on so that if issues
#                       # occur, the output log will have the information
#                       # needed to track down and fix the problem.
#                      }
#
#   Display ALL debug information
#Definition["debug"] = 1
#
#   Display NO debug information
#Definition["debug"] = 0


#----------------------- == TAF Specific Settings == --------------------------

# maxFmGroups -----------------------------------------------------------------
#   Defines the maximum number of FM groups allowed per TAF (this isn't a hard
#   limit, it's more of a goal for the formatter).
#Definition["maxFmGroups"] = 8

# disclaimer ------------------------------------------------------------------
#   Routine statements for specific airports to indicate when observations are
#   ending and resuming. The values in the dictionary are dictionaries where
#   ranges are specified (inclusive, exclusive) of the hours (issuance times)
#   during which to show a specific disclaimer.
#   
#   For instance, (3, 10): "disclaimer1" would show "disclaimer1" when the
#   issuance hour is between 3Z (inclusive) and 10Z (exclusive). All whitespace
#   before and after disclaimer text is ignored.
#   
#   Example:
#   Definition["disclaimer"] = {"KBOS":
#                                  {( 3, 10): "AMD NOT SKED",
#                                   (17, 19): "AMD NOT SKED",
#                                   "default": ""}
#                             }
#Definition["disclaimer"] = {}

# reportVC --------------------------------------------------------------------
#   Set whether to report "vicinity" weather (1 = yes, 0 = no).
#Definition["reportVC"] = 1

# minP6smVisibility -----------------------------------------------------------
#   The minimum visibility value considered to be P6SM.
#Definition["minP6smVisibility"] = 7

# minWindToReportGusts --------------------------------------------------------
#   Minimum sustained wind speed to allow reporting of wind gusts.
#Definition["minWindToReportGusts"] = 10

# minGustSpeedDifference ------------------------------------------------------
#   Wind gusts must be at least this much higher than sustained wind speed to
#   be reported.
#Definition["minGustSpeedDifference"] = 8

# maxSimilarWindGustChange ----------------------------------------------------
#   Maximum wind gust change amount still considered to be similar enough.
#Definition["maxSimilarWindGustChange"] = 7

# variableWindSpeed -----------------------------------------------------------
#   The maximum wind speed (knots) allowed to be considered variable.
#Definition["variableWindSpeed"] = 3

# maxSimilarWindDirChange -----------------------------------------------------
#   Maximum non-light wind/llws direction change amount still considered to be
#   similar enough. 
#Definition["maxSimilarWindDirChange"] = 39

# maxSimilarLightWindDirChange ------------------------------------------------
#   Maximum light wind/llws direction change amount still considered to be
#   similar enough.
#Definition["maxSimilarLightWindDirChange"] = 59

# maxLightWindSpeed -----------------------------------------------------------
#   Maximum wind speed considered to be light wind.
#Definition["maxLightWindSpeed"] = 5

# calmWindSpeed ---------------------------------------------------------------
#   The maximum speed (knots) allowed to be considered "calm".
#Definition["calmWindSpeed"] = 2

# thresholdsNLValues ----------------------------------------------------------
#   This allows use of non-linear thresholds for defined fields. The thresholds
#   are for determining if certain fields are 'similar' enough to each other.
#   "WIND_MAG" and "LLWS_MAG" thresholds must be defined here. The values of
#   the map are the thresholds and changes to the field must be less than the
#   threshold to be considered 'similar'. For example, for wind magnitude, for
#   winds that are >= 50 kts, changes in wind magnitude less than 20 kts will
#   be considered 'similar'.
#Definition["thresholdsNLValues"] = {# Thresholds for surface wind magnitude
#                       "WIND_MAG":
#                        {"default": 20,      # for max speed >= 50 kt
#                         (0.0, 10.0): 4,     # for max speed < 10 kt
#                         (10.0, 20.0): 8,    # for max speed 10-19 kt
#                         (20.0, 50.0): 10,   # for max speed 20-49 kt
#                        },
#                       
#                       # Thresholds for low-level wind shear magnitude
#                       "LLWS_MAG":
#                        {"default": 10,      # for all speeds
#                        },
#                       },

# maxSimilarNonCeilingHeightChange --------------------------------------------
#   Maximum non-ceiling height change amount (100s ft) still considered
#   to be similar enough
#Definition["maxSignificantNonCeilingHeight"] = 99

# maxSignificantNonCeilingHeight ----------------------------------------------
#   Maximum operationally significant non-ceiling height (100s ft)
#Definition["maxSignificantNonCeilingHeight"] = 50

# maxSignificantNonCeilingHeight ----------------------------------------------
#   Maximum operationally significant ceiling height (100s ft AGL) Used for sky
#   cover comparison.
#Definition["maxSignificantCeilingHeight"] = 119

# useDetailedCloudHeights -----------------------------------------------------
#   If set to 1, will report cloud heights to the nearest reportable value.
#   Otherwise, IFR cloud heights will be reported using representative
#   categorical values.
#Definition["useDetailedCloudHeights"] = 0

# tempoProbDefaultHeight ------------------------------------------------------
#   Defines cloud base height to use when weatherRules forces a TEMPO/PROB
#   group but CloudBaseConditional isn't available and the cloud base in the FM
#   group is SKC so we need to guess at a height. This must be a number with a
#   correct reportable value in hundreds of feet.
#Definition["tempoProbDefaultHeight"] = 40

# fmWeatherTypesToRepeat ------------------------------------------------------
#   A list of TAF weather type codes that can be repeated/copied into a TEMPO
#   or PROB30 group under certain circumstances; whenever a FM group has a TAF
#   weather type code that is in this list and a TEMPO or PROB30 group exists,
#   the TAF weather type code will shown in the TEMPO or PROB30 group as well.
#   For instance, if "BR" is in this list and a FM group has a "BR" as part of
#   it's weather, any TEMPO or PROB30 group associated with that FM group will
#   also have "BR" as part of its weather as well.
#
#   NOTE: Weather types can have attributes and so if you just want
#   thunderstorms with no attributes to be repeated, you would put "TS" here
#   and if "TSGR" (thunderstorms with large hail) occurs, it would not be
#   repeated; to repeat "TSGR", it needs to be specified explicitly.
#Definition["fmWeatherTypesToRepeat"] = ["BR",]

# minVfrVisibility ------------------------------------------------------------
#   Minimum VFR visibility (in statute miles).
#Definition["minVfrVisibility"] = 6

# maxNonSignificantRating -----------------------------------------------------
#   Maximum significance rating that is considered to be operationally
#   non-significant.
#Definition["maxNonSignificantRating"] = 60

# verificationHeaders ---------------------------------------------------------
#   Flag to toggle production of "verification" TAF headers (1 = yes, 0 = no).
#   MUST BE SET TO 0 FOR OPERATIONAL TAFS!
#Definition["verificationHeaders"] = 0

# areaOverrides ---------------------------------------------------------------
#   Area overrides allow you to modify behavior for particular airports.
#   In the case of the TAF, these edit areas are airports that are specified
#   by the "defaultEditAreas" in the Definition.
# 
#   When a WFO has multiple airports, those airports might have different
#   rules to follow. The area overrides allow you to decrease the amount of
#   changes that need to be made and also groups the changes together in a
#   single location so that you can quickly see how one airport differs
#   from another airport.
# 
#   Both variables (of any type of data) and functions can be overriden in
#   area overrides.
# 
#   The format is a dictionary where the edit area name (in this case it's
#   the airport) is the key and the value is a list of tuples. The first
#   element of each tuple is the name of the variable/function you want to
#   override and the second element of the tuple is the new value of the
#   variable/function.
# 
#   The overrides will only be in effect for that particular edit area and
#   will have no impact whatsoever for different edit areas.
# 
#   When overriding functions, it's important to specify the new function
#   in quotes so that you don't get a python error. You can place your
#   new function in site overrides like you would typically do.
# 
#   Example
#   =======
#   If the airport KBOS, wants to not report vicinity types, have different
#   weather rules and make the TAF product differently, the
#   areaOverrides could look like below. Note that 'self' isn't needed
#   anywhere and when overriding a Definition key, you prepend a
#   '_' to the name because that's what its name will be when it
#   becomes a variable of the TextProduct class. Also note that overrides
#   here completely replace the previous definition (so for this example,
#   if the baseline weatherRules have "F" (fog) rules but they aren't
#   specified here, then KBOS won't have any fog rules).
# 
#   Definition["areaOverrides"] = {
#       "KBOS" : [
#           ("_reportVC", 0),      # This is an integer variable
#           ("_weatherRules", {    # This is a dictionary variable
#               "T": {
#                   # Number of hours since Issuance time
#                   (0, 9): {
#                       # Probability/Coverage (Intensity optional)
#                       'Chc, Sct': "TEMPO",
#                       'default':  "VCTS"},
#                   'default': {
#                       'Chc, Sct': "PROB30",
#                       'default':  "PREVAIL"},
#               },
#               
#               "default": {
#                   'default': {
#                       'default':  "PREVAIL"},
#               },
#            }),
#           ("_makeProduct", "_makeProductKBOS"),  # This is a function whose
#                                                  # definition can be in the
#                                                  # site overrides file
#       ],
#   }
Definition["areaOverrides"] = {
}
