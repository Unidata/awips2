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
# ---------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without
# technical  support, and with no warranty, express or implied, as to
# its usefulness for any purpose.
#
# FWS_Overrides
#
#  This file provides any product specific overrides for the
#  FWS product.  This file is part of the baseline.
#
# Definition Section:
#   Overrides:
#   Additions:
#
# Methods:
#   Overrides:
#   Additions:
#
# ---------------------------------------------------------------------

import string, time, re, os, types, copy
import TextRules
import ProcessVariableList
import math
import HazardsTable, TimeRange, AbsTime

# Define overrides of Product Definition settings and
# default values of additional Definition settings
#  ( This Definition section must be before the Class definition)

#***** THIS NEXT LINE IS REQUIRED *****
Definition = {}
#
# FWS Definitions:
# Definition statements must start in column 1

# REQUIRED CONFIGURATION ITEMS
#Definition['displayName'] = "FWS"
Definition["statePil"] = "GTF"       # State Pil ID

Definition["productName"] = "SPOT FORECAST"  # name of product
Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
Definition["wmoID"] = "<wmoID>"      # WMO ID
Definition["pil"] = "<pil>"
Definition["stqPil"] = "STQ<site>"   # STQ pil
Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.

Definition["summaryExtended"] = 0
Definition["individualExtended"] = 1
Definition["extendedLabel"] = 1

### FWS settings of baseline options: ###
Definition["mapNameForCombinations"] = None
Definition["defaultEditAreas"] = []

# agencyList - This is a list of agency abbreviations as you want them to
#              appear in the product header. For Example...
#              SPOT FORECAST FOR WILLIE FIRE...USFS GNF
#              where "USFS GNF" is an example of agency abbreviation.
#              The FWS formatter will read the STQ spot request product
#              and will try to first guess the agency abbreviation from the
#              "REQUESTING AGENCY" line of the STQ product. If the first guess
#              is found in your agencyList list, then the Quality Control GUI
#              with have that agency pre-selected. If list is left empty,
#              then the formatter will always use what the user submitted 
#              for the agency.

#Definition["agencyList"] = [
#    (1,"AGENCY 1"),
#    (2,"AGENCY 2"),
#    (3,"AGENCY 3"),
#    (4,"AGENCY 4"),
#    (5,"AGENCY 5"),
#    ]
Definition["agencyList"] = []

# forecasterList - This is a list of forecaster numbers, forecaster awips login name,
#                  and forecaster last names. The Quality Control GUI will
#                  list the forecaster's last name and the forecaster will
#                  check all of the forecaster's names that were involved
#                  in that forecast.

Definition["forecasterList"] = [
    (1,"forecastera","FORECASTER A"),
    (2,"forecasterb","FORECASTER B"),
    (3,"forecasterc","FORECASTER C"),
    (4,"forecasterd","FORECASTER D"),
    (5,"forecastere","FORECASTER E"),
    ]

# stqNumberVersions - When you launch the FWS formatter, you will get a GUI
#                     that asks you to select which spot request you want to
#                     format a spot forecast for. This variable specifies
#                     how many spots you want to list in the GUI. If you do
#                     increase the number, then make sure you increase the
#                     number of versions stored in awips.
Definition["stqNumberVersions"] = 10

# stqWmoID - helps find the timestamp line in the STQ product. Only change if
#            WSH changes the WMO id of the STQ product.

Definition["stqWmoID"] = "BMBB91 K"

# wind20ftHeader:  This definition set to "1" allows offices to
# format winds in this format...
#
# WIND (20 FT)........
#  SLOPE/VALLEY.......WEST 10-20 MPH
#  RIDGETOP...........NORTHWEST 20 MPH
#
# By setting this definition to "0", you will get...
#
# WIND (20 FT)........WEST 10-20 MPH
# RIDGETOP WIND.......NORTHWEST 20 MPH
Definition["wind20ftHeader"] = 1  # Use 1 for yes, 0 for no

# typeList - This is a list of project types and are formatted in the "REASON FOR
#            REQUEST" line of the FWS forecast. Do not edit this list unless WSH
#            directs you to do so.

Definition["typeList"] = ["WILDFIRE", "PRESCRIBED", "HAZMAT", "SAR", "TEST"]

# Set shortTermOnly to 1 if you don't want to give your forecasters an option
# include extended forecasts and/or outlooks with their spot forecasts.
Definition["shortTermOnly"] = 1
#Definition["shortTermOnly"] = 0

Definition["outputFile"] = "{prddir}/TEXT/FWS.txt"

# Definitions to insert unrepresentativeness of the forecast
# instructions for the user.
Definition["insertUnrepresentStatement"] = 1  # Use 1 for yes, 0 for no
Definition["unrepresentStatement"] = "IF CONDITIONS BECOME UNREPRESENTATIVE..." + \
                                     "CONTACT THE NATIONAL WEATHER\nSERVICE."
# Definitions to insert the FWF discussion from a separate file.
# Discussion is edited separately in XNOW for the FWF forecast.
# Advantage of this is to have a first guess for the discussion in
# the Spot forecast...saving some composition time.
Definition["insertDiscussionFromFile"] = 0  # Use 1 for yes, 0 for no
Definition["discussionFile"] = "/home/local_apps/xnow/temp/DISFWF<site>"

# Definitions to insert the FWF 8 to 14 day outlook from a separate
# file if the user requests that information in their request. (Not
# very likely). Outlook is edited separately in XNOW for the FWF
# Forecast with the advantage of saving time in the composition of
# the Spot Forecast.
Definition["insertOutlookFromFile"] = 0     # Use 1 for yes, 0 for no
Definition["outlookFile"]  =   "/home/local_apps/xnow/temp/OLKFWF<site>"

 
# wildfireElementList is a subset list of the requestedElementList list.
# The directive states that Sky/Weather, Temp, RH, and Winds are required
# for wildfire spot forecasts. Even if the user doesn't select these elements,
# the formatter will put them in anyway because of the directive requirements.

# You may add weather elements corresponding to the entries you see in your STQ product.

Definition["wildfireElementList"] = [
    "SKY/WEATHER",
    "TEMPERATURE",
    "HUMIDITY",
    "20 FOOT WINDS",
    "EYE LEVEL WINDS",       
    ]

Definition["stqPil"] = "STQ<site>"   # STQ pil
 
# Definitions to insert unrepresentativeness of the forecast
# instructions for the user.
#Definition["insertUnrepresentStatement"] = 0  # Use 1 for yes, 0 for no
#Definition["unrepresentStatement"] = "IF CONDITIONS BECOME UNREPRESENTATIVE..." + \
#                                     "CONTACT THE NATIONAL WEATHER\nSERVICE."

# wind20ftHeader:  This definition set to "1" allows offices to
# format winds in this format...
#
# WIND (20 FT)........
#  SLOPE/VALLEY.......WEST 10-20 MPH
#  RIDGETOP...........NORTHWEST 20 MPH
#
# By setting this definition to "0", you will get...
#
# WIND (20 FT)........WEST 10-20 MPH
# RIDGETOP WIND.......NORTHWEST 20 MPH
#Definition["wind20ftHeader"] = 0  # Use 1 for yes (default), 0 for no

# Definitions to insert the FWF discussion from a separate file.
# Discussion is edited separately in XNOW for the FWF forecast.
# Advantage of this is to have a first guess for the discussion in
# the Spot forecast...saving some composition time.
#Definition["insertDiscussionFromFile"] = 1  # Use 1 for yes, 0 for no
#Definition["discussionFile"] = "/home/local_apps/xnow/temp/DISFWFBYZ"

# Definitions to insert the FWF 8 to 14 day outlook from a separate
# file if the user requests that information in their request. (Not
# very likely). Outlook is edited separately in XNOW for the FWF
# Forecast with the advantage of saving time in the composition of
# the Spot Forecast.
#Definition["insertOutlookFromFile"] = 1     # Use 1 for yes, 0 for no
#Definition["outlookFile"]  =   "/home/local_apps/xnow/temp/OLKFWFBYZ"

#Definition["tempLocalEffects"] = 1   # Set to 1 to enable Temp and RH local effects AFTER
                                      # creating AboveElev and BelowElev edit areas
#Definition["windLocalEffects"] = 1   # Set to 1 to enable wind local effects AFTER
                                      # creating Ridges and Valleys edit areas
# OPTIONAL CONFIGURATION ITEMS
#Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
#Definition["debug"] = 1
#Definition["lineLength"] = 66   #Product line length

# Set the following variable to 1 if you want Lightning Activity
# reported with phrases like "1-8 STRIKES", "9-15 STRIKES", etc.
#Definition["lightningPhrases"] = 1

# The following variable sets a wind adjustment factor for surface
# (20 ft) winds.  Wind speeds will be multiplied by this factor.
# Winds reported by RAWS sites are frequently lower than ASOS winds
# due to the fact that they measure wind speeds at lower heights.
# A common adjustment factor is 80% (0.80).  If you want no adjustment
# to the winds then set this variable to 1.00
#Definition["windAdjustmentFactor"] = 1.00

# The following variable sets a wind adjustment factor for eye level
# winds.  Wind speeds will be multiplied by this factor. Eye level
# winds are frequently lower than ASOS winds due to the fact that
# winds are slower when measured closer to the ground.  A common
# adjustment factor is 60% (0.60).  If you want no adjustment to
# the winds then set this variable to 1.00
Definition["eyeWindAdjustmentFactor"] = 0.60

#Definition["language"] = "english"                 

# Trouble-shooting items
#Definition["passLimit"] = 20              # Limit on passes allowed through
                                          # Narrative Tree
#Definition["trace"] = 1                   # Set to 1 to turn on trace 
#   useRH              If 1, use RH grids instead of MaxRH, MinRH
Definition["useRH"] = 0

### *** START TABULAR TEST SECTION *** ###

Definition["fwfPil"] = "FWFBYZ"   # FWF pil
 
# Definitions to insert the FWF discussion from a separate file or
# from the last FWF your office has issued.

# Discussion is edited separately in GFE for the FWF forecast.
# Advantage of this is to have a first guess for the discussion in
# the Spot forecast...saving some composition time.

# Use 1 to get Discussion from a file
# Use 2 to get Discussion from your last FWF product
# Use 0 to use a blank Discussion template
Definition["insertDiscussionFromFile"] = 2 
Definition["discussionFile"] = "/data/local/DISFWFBYZ"

# Definitions to insert the FWF 8 to 14 day outlook from a separate
# file if the user requests that information in their request. (Not
# very likely). Outlook is edited separately in XNOW for the FWF
# Forecast with the advantage of saving time in the composition of
# the Spot Forecast.

# Use 1 to get Outlook from a file
# Use 2 to get Outlook from your last FWF product
# Use 0 to use a blank Outlook template
Definition["insertOutlookFromFile"] = 2     # Use 1 for yes, 0 for no
Definition["outlookFile"]  =   "/data/local/OLKFWFBYZ"

# If set to 1, the user can enter a creation date/time
# for product generation. It will be as if the product was run
# at the creation time specified by the user at run-time.
Definition["includeCreationTimeOnGUI"] = 1
#Definition["includeCreationTimeOnGUI"] = 0

# forecastTypeList - This definition contains a list of spot forecast formats that a
#                    forecaster can select via the formatter gui. The formats are:
#
#   Narrative Only:     The spot forecast is in a narrative format.
#   Tabular/Narrative:  This format is a tabular/narrative mix as specified
#                       in the "_rowList" (see FWS_Overrides).
#
#   For each forecastType, you can specify a label that will appear in the GUI.
#
Definition["forecastTypeList"] = [
    #  Label                  Forecast Type
    ("Narrative Only",       "Narrative Only"),
    ("Tabular/Narrative",    "Tabular/Narrative"),
    ("Tabular Only",         "Tabular Only"),
    
    # If your _rowList specifies an all Tabular product,
    # you may want to change this entry to:
    #("Tabular",                     "Tabular/Narrative"),
    ]

# defaultForecastType - This defintion sets the default setting for which spot forecast
#                       format your WFO wants to use. Value for definition must be included
#                       in the forecastTypeList definition and must be either "Narrative",
#                       "Tabular", or "With Ignition Forecast".
#Definition["defaultForecastType"] = "Narrative Only"
Definition["defaultForecastType"] = "Tabular/Narrative"

#   withIgnitionTimes: If "yes", ertain weather elements can be configured to include
#                    an ignition time forecast within the narrative.
Definition["withIgnitionTimes"] = "no"

#  includeIgnitionOptionOnGUI: If 1, the GUI will include this option at run-time.
Definition["includeIgnitionOptionOnGUI"] = 1
#Definition["includeIgnitionOptionOnGUI"] = 0

# tabularResolutionDict - This definition contains the list of table time resolutions
#                         (per period) that you want to appear in the GUI to the forecaster.
#                         Some WFOs may not want to give forecasters an option to generate
#                         a table with an 1 hour resolution (for example), so you can
#                         delete "1" from this list and it will not appear on the gui.
#                         Possible values are 1, 2, 3, 4 hours and 123 in which case,
#                         hourly resolution will be 1 hour in the 1st period,
#                         2 hours in the 2nd period, and 3 hours in the third period
#                         (if needed)..
Definition["tabularResolutionDict"] = {
    "Today": [1, 2, 3, 4, "None"],
    "Tonight": [1, 2, 3, 4, "None"],
    "Tomorrow": [1, 2, 3, 4, "None"],
    }

# defaultTabularResolution - This definition must be set to one of values listed in the
#                            tabularResolutionList definition. This will be the value
#                            that the gui will use for a default for each period.
#                            Values are limited to 1, 2, 3, and 4 and must be included in
#                            the tabularResolutionList definition.
Definition["defaultTabularResolution"] = {
    "Today": 2,
    "Tonight": 2,
    "Tomorrow": 2
    }

# tabularAllPeriods - Setting this definition to "no" will generate a 12
#                     hour table only in the first period.
#                     The table will start at either the current time or
#                     the ignition time depending on the setting of
#                     tableStartTimeMode (see below).
#                     Setting this definition to "yes" will allow tables in
#                     all periods with snapshot values covering the
#                     time range of each period.

Definition["tabularAllPeriods"] = "yes"
#Definition["tabularAllPeriods"] = "no"

# tabularForWildfire - This is a nationally mandated setting which requires
#                      a narrative forecast for wildfire spot requests. When
#                      set to "no", a narrative will be produced, even if the
#                      tabular option is selected. Your office must issue a
#                      PDD to switch this definition to "yes".

Definition["tabularForWildfire"] = "no"
#Definition["tabularForWildfire"] = "yes"
                                       
# tableStartTimeMode - The setting of this definition will tell the formatter how to
#                      determine the start time for the table.
#                      If "productStart" is used, then the table will start at the
#                         beginning of the product issuance.
#                      If "ignitionTime" is used, then the formatter will use the ignition time
#                         if it is within the first period of the product.  
#                         Otherwise the formatter will use the productStart time.
#                      If "current" is used, then the table will start at the time the
#                         formatter was launched.

#Definition["tableStartTimeMode"] = "current"
#Definition["tableStartTimeMode"] = "productStart"
Definition["tableStartTimeMode"] = "ignitionTime"

# tableStartTimeOffset - When the ignition time is used for the table start time,
#                        you can start the table a set number of hours before the
#                        ignition time. You can configure the tableStartTimeOffset
#                        definition for this purpose. Value is the number of hours
#                        before the ignition time desired. Note, if this new time
#                        is before the product start time, then the product start
#                        time will be used.
Definition["tableStartTimeOffset"] = 0

# ignitionForecastTypeList - The formatter can produce Ignition Time/Request Time
#                            forecasts for certain weather elements, like T and RH.
#                            This list will produce this forecast as a default for
#                            certain types of spot requests. List elements must be
#                            a subset of the typeList definition.

#Definition["ignitionForecastTypeList"] = ["PRESCRIBED"]
Definition["ignitionForecastTypeList"] = []

# elementFormatDict - This defines the format as "alpha" or "numeric" for various
#                     tabular weather elements.

# Sky - This definition allows a WFO to use a text description of the
#       Sky Cover or use numeric values. Examples are as follows...
# alpha 1hr  :  SKY.............MC  MC  MC  MC  MC  MC  MC  MC  MC  MC  MC  MC  PC
# alpha 2hr  :  SKY.............MCLDY  MCLDY  MCLDY  MCLDY  MCLDY  MCLDY  PCLDY
# numeric 2hr:  SKY (%).........90     90     90     83     83     83     69

# Tabular Wind Format Definitions
# "alpha" will cause the direction to be formatted in the alphabetic characters
# of N, NW, W, SW, S, SE, E, and NE.
# "numeric" will return the wind direction in tens of degrees. i.e. 000, 010,
# etc. When a numeric wind direction is combined with wind speed it will look
# something like this...030/10

Definition["elementFormatDict"] = {
    "Sky" : "numeric",
    "Wind": "alpha",
    "Wind20ft": "alpha",
    "EyeWind": "alpha",
    "RidgeWind": "alpha",
    "TransWind": "alpha",
    "TransMetWind": "alpha",
    }
   
# bothAlphaNumericDict - For certain elements both alpha and numeric values
#                        are needed. In particular, sky cover and wind direction.
#                        Only possible values are "Yes" or "No".
#                        If you do configure a wind element to "Yes", then
#                        ensure the corresponding setting for elementFormatDict
#                        is set to "alpha". Otherwise, you will get two lines
#                        of numeric values.
   
Definition["bothAlphaNumericDict"] = {
    "Sky" : "No",
    "Wind": "No",
    "Wind20ft": "No",
    "EyeWind": "No",
    "SfcWind": "No",
    "RidgeWind": "No",
    "TransWind": "No",
    }

# tabularMixingHeightUnits - This definition allows the WFO to specify their preferance
#                            on how mixing height is expressed. In thousands of feet or
#                            in just feet? The definition can only be set to "kft" or
#                            "ft". Note: the 1 hour resolution table is limited to a
#                            three character space, so mixing height will always be
#                            expressed in kft when the 1 hour resolution is selected
#                            regardless to what this definition is set to. Examples...
# KFT 2hr:  MIX HGT (KFT)...0.3    0.3    0.3    0.3    0.3    7.9    11
# FT 2hr :  MIX HGT (FT)....300    300    300    300    300    7900   11100

#Definition["tabularMixingHeightUnits"] = "kft"      # So we can fit a number in a 3 character space.
Definition["tabularMixingHeightUnits"] = "ft"       # Will only be used for 2,3, or 4 time resolutions.

# transportWindLabel - Some WFOs use "Transport Winds", while others use "Mixing Winds".
#                      They are one in the same in terms of the forecast. This definition
#                      allows the WFO to configure their preference for the tabular section.
#Definition["transportWindLabel"] = "mix"
Definition["transportWindLabel"] = "tran"

# includeMetricDispersion - Some users need mixing height and transport winds
#                           in metric units. If you want to include the metric
#                           in addition to english values, then set definition
#                           to "yes". Otherwise "no".

#Definition["includeMetricDispersion"] = "yes"
Definition["includeMetricDispersion"] = "no"

# 20ftWindParm - Some WFOs actually produce a Wind20ft grid, so the 20 FOOT WIND
#                phrase can be configured to sample that grid (the "Wind20ft"
#                setting). Other WFOs just use a conversion factor (windAdjustmentFactor)
#                of what they have in the Wind grid (the "Wind" setting).

Definition["20ftWindParm"] = "Wind"
#Definition["20ftWindParm"] = "Wind20ft"

# wind20ftHeader:  This definition set to "1" allows offices to
# format winds in this format...
#
# WIND (20 FT)........
#  SLOPE/VALLEY.......WEST 10-20 MPH
#  RIDGETOP...........NORTHWEST 20 MPH
#
# By setting this definition to "0", you will get...
#
# WIND (20 FT)........WEST 10-20 MPH
# RIDGETOP WIND.......NORTHWEST 20 MPH
Definition["wind20ftHeader"] = 0  # Use 1 for yes, 0 for no
#Definition["wind20ftHeader"] = 1  # Use 1 for yes, 0 for no

# tableWindElementSplit - When the time resolution of the table is 1 hour, then
#                         I am forced to create separate lines for wind direction,
#                         wind speed, and wind gust speed. When the resolution
#                         is 2 hours or more, then a WFO has a choice of formats.
#                         They can set tableWindElementSplit to "yes" and wind
#                         direction, speed, and gusts will remain in their
#                         separate lines. Or the WFO can set tableWindElementSplit
#                         to "no". For the two hour resolution, direction and
#                         speed will be combined. For three and four hour
#                         resolution, direction, speed, and gusts will be
#                         combined. Examples follow...

# yes 2hr:  20 FT WIND DIR..SW     W      W      W      W      W      W
#        :  20 FT WIND SPD..26     26     18     18     18     14     14
#        :  20 FT WIND GUST.40     40

# no 2 hr:  20 FT WIND......W 26   W 26   W 18   W 18   W 18   W 14   W 14
#        :  20 FT WIND GUST.40     40

# yes 3hr:  20 FT WIND DIR..W         W         W         W         W
#        :  20 FT WIND SPD..26        25        13        14        13
#        :  20 FT WIND GUST.40        40

# no 3 hr:  20 FT WIND......W 26G40   W 25G40   W 13      W 14      W 13

#Definition["tableWindElementSplit"] = "yes"
Definition["tableWindElementSplit"] = "no"

# tableEyeWindElementSplit - When the time resolution of the table is 1 hour, then
#                            I am forced to create separate lines for wind direction,
#                            wind speed, and wind gust speed. When the resolution
#                            is 2 hours or more, then a WFO has a choice of formats.
#                            They can set tableEyeWindElementSplit to "yes" and wind
#                            direction, speed, and gusts will remain in their
#                            separate lines. Or the WFO can set tableEyeWindElementSplit
#                            to "no". For the two hour resolution, direction and
#                            speed will be combined. For three and four hour
#                            resolution, direction, speed, and gusts will be
#                            combined. Examples follow...

# yes 2hr:  EYE LVL WND DIR.SW     W      W      W      W      W      W
#        :  EYE LVL WND SPD.26     26     18     18     18     14     14
#        :  EYE LVL WND GST.40     40

# no 2 hr:  EYE LEVEL WIND..W 26   W 26   W 18   W 18   W 18   W 14   W 14
#        :  EYE LVL WND GST.40     40

# yes 3hr:  EYE LVL WND DIR.W         W         W         W         W
#        :  EYE LVL WND SPD.26        25        13        14        13
#        :  EYE LVL WND GST.40        40

# no 3 hr:  EYE LEVEL WIND..W 26G40   W 25G40   W 13      W 14      W 13

#Definition["tableEyeWindElementSplit"] = "yes"
Definition["tableEyeWindElementSplit"] = "no"

# tableRidgeElementSplit - When the time resolution of the table is 1 hour, then
#                          I am forced to create separate lines for wind direction
#                          and wind speed. When the resolution is 2 hours or more,
#                          then a WFO has a choice of formats. They can set
#                          tableRidgeElementSplit to "yes" and wind direction and
#                          speed will remain in their separate lines. Or the WFO
#                          can set tableRidgeElementSplit to "no" and the wind
#                          direction and speed will be combined into one line.
#                          Examples follow...

# yes 2hr:  RIDGE WIND DIR..W      W      W      W      W      W      W
#        :  RIDGE WIND SPD..36     36     36     36     36     36     16

# no 2 hr:  RIDGETOP WIND...W 36   W 36   W 36   W 36   W 36   W 36   W 16

#Definition["tableRidgeElementSplit"] = "yes"
Definition["tableRidgeElementSplit"] = "no"

# tableTransElementSplit - When the time resolution of the table is 1 hour, then
#                          I am forced to create separate lines for wind direction
#                          and wind speed. When the resolution is 2 hours or more,
#                          then a WFO has a choice of formats. They can set
#                          tableTransElementSplit to "yes" and wind direction and
#                          speed will remain in their separate lines. Or the WFO
#                          can set tableTransElementSplit to "no" and the wind
#                          direction and speed will be combined into one line.
#                          Examples follow...

# yes 2hr:  TRANSP WIND DIR.W      W      W      W      W      W      W
#        :  TRANSP WIND SPD.8      8      8      8      8      8      20

# no 2 hr:  TRANSPORT WIND..W 8    W 8    W 8    W 8    W 8    W 8    W 20

#Definition["tableTransElementSplit"] = "yes"
Definition["tableTransElementSplit"] = "no"

# tableSwellElementSplit - When the time resolution of the table is 1 hour, then
#                          I am forced to create separate lines for swell direction
#                          and swell height. When the resolution is 2 hours or more,
#                          then a WFO has a choice of formats. They can set
#                          tableSwellElementSplit to "yes" and swell direction and
#                          height will remain in their separate lines. Or the WFO
#                          can set tableSwellElementSplit to "no" and the swell
#                          direction and height will be combined into one line.
#                          Examples follow...

# yes 2hr:  SWELL DIRECTION.W      W      W      W      W      W      W
#        :  SWELL HGT (FT)..36     36     36     36     36     36     16

# no 2 hr:  SWELL HGT (FT)..W 36   W 36   W 36   W 36   W 36   W 36   W 16

#Definition["tableSwellElementSplit"] = "yes"
Definition["tableSwellElementSplit"] = "no"

# tableSfcWindElementSplit - When the time resolution of the table is 1 hour, then
#                            I am forced to create separate lines for wind direction,
#                            wind speed, and wind gust speed. When the resolution
#                            is 2 hours or more, then a WFO has a choice of formats.
#                            They can set tableSfcWindElementSplit to "yes" and wind
#                            direction, speed, and gusts will remain in their
#                            separate lines. Or the WFO can set tableSfcWindElementSplit
#                            to "no". For the two hour resolution, direction and
#                            speed will be combined. For three and four hour
#                            resolution, direction, speed, and gusts will be
#                            combined. Examples follow...

# yes 2hr:  SURFACE WND DIR.SW     W      W      W      W      W      W
#        :  SURFACE WND SPD.26     26     18     18     18     14     14
#        :  SURFACE WND GST.40     40

# no 2 hr:  SURFACE WIND....W 26   W 26   W 18   W 18   W 18   W 14   W 14
#        :  SURFACE WND GST.40     40

# yes 3hr:  SURFACE WND DIR.W         W         W         W         W
#        :  SURFACE WND SPD.26        25        13        14        13
#        :  SURFACE WND GST.40        40

# no 3 hr:  SURFACE WIND....W 26G40   W 25G40   W 13      W 14      W 13

#Definition["tableSfcWindElementSplit"] = "yes"
Definition["tableSfcWindElementSplit"] = "no"

# cwrParm - Some WFOs (especially in wetter climates) use the PoP grid for
#           chance of wetting rain, whereas offices in dry climates create a
#           CWR grid that has values lower than the PoP grid. Value values
#           for this definition is either "CWR" or "PoP".

Definition["cwrParm"] = "PoP"


### *** END TABULAR TEST SECTION *** ###

# END  definitions
############################################################

#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum contents of this file are the above Definition = {} line
# plus following class definition and the __init__ method with only
# the "pass" line in it.

class FWS_Overrides:
    """Class NNN_FILETYPE - Version: IFPS"""

    def __init__(self):
        pass

# End MAKE NO CHANGES HERE
#**********************************************************************
    # Add methods here making sure to indent inside the class statement
    # FWS Overrides ------------------------

    # It is helpful to put a debug statement at the beginning of each
    # method to help with trouble-shooting.
    #def _method(self):
        #self.debug_print("Debug: _method in FWS_Overrides")

    def _processVariableList(self, definition):

        # Get Definition variables
        for key in definition.keys():
            exec "self._" + key + "= definition[key]"

        # Load in a user specified number of STQ products into the formatter.
        products = self._getStqProducts()

        # Get the information for the specific fire.
        #  IF there are STQ products in the directory,
        #    selection GUI will be displayed
        cancel = self._getFireInfo(products)
        if cancel:
            # User cancelled
            return None

        # Get the user information for the specific fire
        # and return the resulting varDict
        return self._displayFireInfo()

    def _getStqProducts(self):
        # Load in a user specified number of STQ products into the formatter.
        # If no products found, return empty list
        products = []
        version = 0
        stqPil = self._statePil + self._stqPil
        searchString=""
        for version in range(self._stqNumberVersions):
            product = self.getPreviousProduct(stqPil, searchString, version=version)
            if product is None or product == "":
                break
            
            # Let's filter the product just in case single quote is put
            # into the the request.
            product = string.replace(product,"\'","")
            
            product = string.split(product, "\n")
            missingFlag=1
            feedbackFlag=0
            deleteFlag=0
            for line in product:
                line = string.replace(line, "\n", "")
                if "PROJECT NAME" in line:
                    missingFlag=0
                if "Feedback was just received for project" in line:
                    feedbackFlag=1
                if "The Spot Forecast Request for project" in line:
                    deleteFlag=1
            if not missingFlag and not feedbackFlag and not deleteFlag:
                products.append(product)
        return products

    def _getFireInfo(self, products):
        # If there were STQ products, display their names for user to select
        # Return 1 if user cancels
        product, issuance, forecasters = self._getFireProduct(products)
        if issuance is None:
            return 1 # User cancelled
        if len(products) > 0:
            self._noStqProduct = 0
        else:
            product = None
            self._noStqProduct = 1
        self._getProductInfo(product, issuance, forecasters)

    def _getFireProduct(self, products):
        # Create the fireNameList used for the spot selection menu.
        fireNameList = []
        ofileList = []
        validProductFound = 0
        productNumber = 0
        masterProductList = []
        for product in products:
            fireName = "NAME MISSING"
            timeStamp = "DDHHMM"
            tag = "YYYYMMDD.XXXXX.NN"
            tagFlag = 0
            feedbackFlag=0
            deleteFlag=0
            for line in product:
                line = string.replace(line, "\n", "")
                if "PROJECT NAME" in line:
                    fireName = string.upper(line[22:])
                if self._stqWmoID in line:
                    timeStamp = line[12:]
                if "OFILE" in line:
                    tag = string.upper(line[8:])
                    if tag not in ofileList:
                        ofileList.append(tag)
                        tagFlag = 1
                        productNumber = productNumber + 1
            if tagFlag:
                fireNameList.append(`productNumber` + ") " + fireName + \
                                    " -- " + timeStamp + " -- " + tag)
                masterProductList.append(product)
            validProductFound = 1

        varList = []
      
        if validProductFound:
            fireNameList.append("Manually Enter in Request Info")
            desFireName = "Please Choose a Fire", "fireName"
            varList.append((desFireName, fireNameList[0], "radio", fireNameList))
      
        # Product Issuance Processing
        issuanceList = [
            "Morning", "Morning Update", "Afternoon Update",
            "Afternoon", "Afternoon with 4 periods", "Evening Update",          
            "Evening Update with 4 periods", "Early Morning Update",
            "Early Morning Update with 4 periods", "Next Day"
            ]
        desIssuanceList = "Product Issuance:", "productIssuance"
        varList.append((desIssuanceList, issuanceList[0], "radio", issuanceList))

        # Forecaster List Section of the GUI
        forecasterNameList = []
        defaultForecasterNameList = []
        cmd = "whoami"
        db = os.popen(cmd,'r')
        awipsLogin = db.read()
        db.close()
        awipsLogin = string.replace(awipsLogin, "\n", "")
        for forecaster in self._forecasterList:
            id, awipsName, name = forecaster
            forecasterNameList.append(name)
            if awipsLogin == awipsName:
                defaultForecasterNameList.append(name)
        desForecasterNameList = "Forecaster:", "forecaster"
        varList.append((desForecasterNameList, defaultForecasterNameList, "check", forecasterNameList))

        if self._includeCreationTimeOnGUI:
            # Get start date and time from user
            desCreationDate = "Forecast Start Date (ex. 5/25/06)", "creationDate"
            varList.append((desCreationDate, "", "alphaNumeric"))
            desCreationTime = "Forecast Start Time in LT (ex 0900)", "creationTime"
            varList.append((desCreationTime, "", "alphaNumeric"))

        # Launch the Spot Request selection GUI.
        varDict = self._callProcessVariableList("Select Spot Request", varList, varDict={})
        if varDict is None:
            return None, None, None

        productIssuance = varDict[desIssuanceList]
        forecasters = varDict[desForecasterNameList]
        if self._includeCreationTimeOnGUI:
            self._creationDate = varDict[desCreationDate]
            self._creationTime = varDict[desCreationTime]        

        if validProductFound:
            if varDict[desFireName] == "Manually Enter in Request Info":
                return None, productIssuance, forecasters
            else:
                stqIndex = fireNameList.index(varDict[desFireName])
                return masterProductList[stqIndex], productIssuance, forecasters
        else:
            return None, productIssuance, forecasters

    def _callProcessVariableList(self, title, varList, varDict):
        processVarList = ProcessVariableList.ProcessVariableList(
            title, varList, varDict={})
        self._selectionStatus = processVarList.status()
        if not self._selectionStatus == "OK":
            return None   # User Cancelled
        return processVarList.varDict()


    def _weInfoList(self):
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
            ("SKY/WEATHER",     1, self.skyWeather_byTimeRange_compoundPhrase,
             [("SKY", "CLOUDS"), "WEATHER"]),
            ("BEGIN/END OF PCPN", 0, self.pcpnTiming_phrase,
             ["BEGIN", "END", "PCPN"]),
            ("TEMPERATURE",      1, (self.dayOrNight_phrase, ["MaxT", "MinT", 1, 1]),
             [("TEMPERATURE", "TEMP")]),  
            ("HUMIDITY",         1, (self.dayOrNight_phrase, [dayRH, nightRH, 1, 1]),
             [("RH", "HUMIDITY")]),
            ("DEWPOINT",        0, self.td_phrase,  
             ["DEWPOINT"]),
            ("20 FOOT WINDS",   0, wind,            
             ["20", "WIND", ("FT", "FOOT")]),
            ("EYE LEVEL WINDS",  1, self.fireEyeWind_compoundPhrase,
             [("EYE","10"), "WIND"]),
            ("SURFACE WINDS", 0, self.fireSfcWind_compoundPhrase,
             ["SURFACE", "WIND"]),
            ("WIND SHIFT",      0, self.fireWindShift_label_phrase,  
             ["WIND", "SHIFT"]),
            ("RIDGE TOP WIND",  0, self.freeWind_phrase,  
             ["WIND", "RIDGE", "TOP"]),
            ("SURROUNDING RIDGE", 0,  self.surroundingRidgeWind_phrase,  
             ["SURROUNDING", "RIDGE", "WIND"]),
            ("CWR",              0, self.cwr_phrase,  
             [("CWR", "WETTING RAIN")]),
            ("POP", 0, self.pop_phrase,  
             [("PRECIPITATION", "CHANCE OF PCPN", "POP")]),
            ("LIGHTNING ACTIVITY LEVEL", 0, self.lal_phrase,
             [("LAL", "LIGHTNING")]),
            ("SMOKE DISPERSION", 1, [self.mixingHgt_phrase, self.transportWind_phrase],
             [("SMOKE", "DISPERSION")]),              
            ("MIXING HEIGHT",   0, self.mixingHgt_phrase,
             ["MIXING"]),
            ("TRANSPORT WINDS", 0, self.transportWind_phrase,
             ["TRANSPORT", "WIND"]),
            ("LDSI", 0, self.ldsi_phrase,
             ["LDSI"]),
            ("LVORI", 0, self.lvori_phrase,
             ["LVORI"]),
            ("ADI",0, self.adi_phrase,
             ["ADI"]),
            ("DISPERSION INDEX",  0, self.dsi_phrase,
             ["DISPERSION", "INDEX"]),
            ("CLEARING INDEX",  0, self.smokeDispersal_phrase,
             ["CLEARING", "INDEX"]),
            ("STABILITY CLASS", 0, self.stabilityClass_phrase,
             ["STABILITY"]),
            ("MARINE LAYER",    0, self.marineLayer_phrase,
             ["MARINE", "LAYER"]),
            ("HAINES INDEX",    0, self.haines_phrase,
             ["HAINES", "INDEX"]),
            ("VENTILATION RATE", 0, self.smokeDispersal_phrase,
             ["VENTILATION", "RATE"]),
            ("SWELL HEIGHT",    0, self.swell_phrase,
             ["SWELL", "HEIGHT"]),
            ("WAVE HEIGHT",     0, self.waveHeight_phrase,
             ["WAVE","HEIGHT"]),
            ("SWELL PERIOD",    0, self.period_phrase,
             ["SWELL", "PERIOD"]),
            ("WIND WAVE",    0, self.windWave_phrase,
             ["WIND", "WAVE"]),
            ("RAINFALL AMOUNT",    0, self.qpf_phrase,
             ["RAINFALL", "AMOUNT"]),
            ("SNOWFALL AMOUNT",    0, self.snow_phrase,
             ["SNOWFALL", "AMOUNT"]),
            ("FREEZING LEVEL", 0, self.freezingLevel_phrase,
             ["FREEZING", "LEVEL"]),
            ("CEILING", 0, self.ceiling_phrase,
             ["CEILING"]),
            ("VISIBILITY", 0, self.visibility_phrase,
             ["VISIBILITY"]),
            ("ICING", 0, self.icing_phrase,
             ["ICING"]),
            ("HAZARDS", 0, self.ceiling_phrase,
             ["HAZARDS"]),
            ("HEAT INDEX", 0, self.heatIndex_phrase,
             ["HEAT", "INDEX"]),
            ]

    def _weInfoHiddenList(self):
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
##            ("WIND SHIFT",               0),
##            ("RIDGE TOP WIND",           0),
##            ("SURROUNDING RIDGE",        0),
##            ("CWR",                      0),
##            ("POP",                      0),
##            ("LIGHTNING ACTIVITY LEVEL", 0),
##            ("SMOKE DISPERSION",         0),
##            ("MIXING HEIGHT",            0),
##            ("TRANSPORT WINDS",          0),
##            ("DISPERSION INDEX",         0),
##            ("LDSI",                     0),
##            ("LVORI",                    0),
##            ("ADI",                      0),
##            ("CLEARING INDEX",           0),
##            ("STABILITY CLASS",          0),
##            ("MARINE LAYER",             0),
##            ("HAINES INDEX",             0),
##            ("VENTILATION RATE",         0),
##            ("SWELL HEIGHT",             0),
##            ("WAVE HEIGHT",              0),
##            ("SWELL PERIOD",             0),
##            ("WIND WAVE",                0),
##            ("RAINFALL AMOUNT",          0),
##            ("SNOWFALL AMOUNT",          0),
##            ("FREEZING LEVEL",           0),
##            ("CEILING",                  0),
##            ("VISIBILITY",               0),
##            ("ICING",                    0),
##            ("HAZARDS",                  0),
##            ("HEAT INDEX",               0),
            ]

    def _rowList(self, colWidth=1):

        ### 20 foot wind tabular phrase configuration ###
        
        if self._tableWindElementSplit == "no" and colWidth == 7: # 2 hourly
            if self._bothAlphaNumericDict.get(self._20ftWindParm, "No") == "No":
                wind = [("20 FT WIND......", self._wind_value),
                        ("20 FT WIND GUST.", self._windGust_value)]
            else:
                wind = [("20 FT WIND......", self._wind_value),
                        ("20 FT WIND GUST.", self._windGust_value),
                        ("20 FT WIND DIR..", self._windNumDir_value)]
        elif self._tableWindElementSplit == "no" and colWidth > 7: # 3-4 hourly
            if self._bothAlphaNumericDict.get(self._20ftWindParm, "No") == "No":
                wind = [("20 FT WIND......", self._windWithGust_value)]
            else:
                wind = [("20 FT WIND......", self._windWithGust_value),
                        ("20 FT WIND DIR..", self._windNumDir_value)]
        else:
            if self._bothAlphaNumericDict.get(self._20ftWindParm, "No") == "No":
                wind = [("20 FT WIND DIR..", self._windDir_value),  # 1 hourly
                        ("20 FT WIND SPD..", self._windSpd_value),
                        ("20 FT WIND GUST.", self._windGust_value)]
            else:
                wind = [("20 FT WIND DIR..", self._windDir_value),  # 1 hourly
                        ("20 FT WIND DIR..", self._windNumDir_value),
                        ("20 FT WIND SPD..", self._windSpd_value),
                        ("20 FT WIND GUST.", self._windGust_value)]

        ### eye level wind tabular phrase configuration ###
                
        if self._tableEyeWindElementSplit =="no" and colWidth == 7:
            if self._bothAlphaNumericDict.get("EyeWind", "No") == "No":
                eyewind = [("EYE LEVEL WIND..", self._eyewind_value),
                           ("EYE LVL WND GST.", self._eyewindGust_value)]
            else:
                eyewind = [("EYE LEVEL WIND..", self._eyewind_value),
                           ("EYE LVL WND GST.", self._eyewindGust_value),
                           ("EYE LVL WND DIR.", self._eyewindNumDir_value)]
        elif self._tableEyeWindElementSplit == "no" and colWidth > 7:
            if self._bothAlphaNumericDict.get("EyeWind", "No") == "No":
                eyewind = [("EYE LEVEL WIND..", self._eyewindWithGust_value)]
            else:
                eyewind = [("EYE LEVEL WIND..", self._eyewindWithGust_value),
                           ("EYE LVL WND DIR.", self._eyewindNumDir_value)]
        else:
            if self._bothAlphaNumericDict.get("EyeWind", "No") == "No":
                eyewind = [("EYE LVL WND DIR.", self._eyewindDir_value),
                           ("EYE LVL WND SPD.", self._eyewindSpd_value),
                           ("EYE LVL WND GST.", self._eyewindGust_value)]
            else:
                eyewind = [("EYE LVL WND DIR.", self._eyewindDir_value),
                           ("EYE LVL WND DIR.", self._eyewindNumDir_value),
                           ("EYE LVL WND SPD.", self._eyewindSpd_value),
                           ("EYE LVL WND GST.", self._eyewindGust_value)]
                
        ### surface wind (10m) tabular phrase configuration ###
                
        if self._tableSfcWindElementSplit =="no" and colWidth == 7:

            if self._bothAlphaNumericDict.get("SfcWind", "No") == "No":
                sfcwind = [("SURFACE WIND....", self._sfcwind_value),
                           ("SURFACE WND GST.", self._sfcwindGust_value)]
            else:
                sfcwind = [("SURFACE WIND....", self._sfcwind_value),
                           ("SURFACE WND GST.", self._sfcwindGust_value),
                           ("SURFACE WND DIR.", self._sfcwindNumDir_value)]
            
        elif self._tableSfcWindElementSplit == "no" and colWidth > 7:

            if self._bothAlphaNumericDict.get("SfcWind", "No") == "No":
                sfcwind = [("SURFACE WIND....", self._sfcwindWithGust_value)]
            else:
                sfcwind = [("SURFACE WIND....", self._sfcwindWithGust_value),
                           ("SURFACE WND DIR.", self._sfcwindNumDir_value)]
            
        else:

            if self._bothAlphaNumericDict.get("SfcWind", "No") == "No":
                sfcwind = [("SURFACE WND DIR.", self._sfcwindDir_value),
                           ("SURFACE WND SPD.", self._sfcwindSpd_value),
                           ("SURFACE WND GST.", self._sfcwindGust_value)]
            else:
                sfcwind = [("SURFACE WND DIR.", self._sfcwindDir_value),
                           ("SURFACE WND DIR.", self._sfcwindNumDir_value),
                           ("SURFACE WND SPD.", self._sfcwindSpd_value),
                           ("SURFACE WND GST.", self._sfcwindGust_value)]
                
        ### ridge top wind tabular phrase configuration ###
                
        if self._tableRidgeElementSplit == "no" and colWidth >=7:
            if self._bothAlphaNumericDict.get("RidgeWind", "No") == "No":
                ridge = [("RIDGETOP WIND...", self._ridge_value)]
            else:
                ridge = [("RIDGETOP WIND...", self._ridge_value),
                         ("RIDGE WIND DIR..", self._ridgeNumDir_value)]
        else:
            if self._bothAlphaNumericDict.get("RidgeWind", "No") == "No":
                ridge = [("RIDGE WIND DIR..", self._ridgeDir_value),
                         ("RIDGE WIND SPD..", self._ridgeSpd_value)]
            else:
                ridge = [("RIDGE WIND DIR..", self._ridgeDir_value),
                         ("RIDGE WIND DIR..", self._ridgeNumDir_value),
                         ("RIDGE WIND SPD..", self._ridgeSpd_value)]

        ### swell tabular phrase configuration ###
            
        if self._tableSwellElementSplit == "no" and colWidth >=7:
            swell = [("SWELL HGT (FT)..", self._swell_value)]
        else:
            swell = [("SWELL DIRECTION.", self._swellDir_value),
                     ("SWELL HGT (FT)..", self._swellHgt_value)]
            
        ### Mixing Height and Transport wind label configuration ###

        if self._tabularMixingHeightUnits == "ft" and colWidth > 4:
            mixLabel = "MIX HGT (FT)...."
            mixMetricLabel = "MIX HGT (M)....."                
        else:
            mixLabel = "MIX HGT (KFT)..."
            mixMetricLabel = "MIX HGT (KM)...."
            
        if self._transportWindLabel == "mix":
            transLabel = "MIXNG WIND......"
            transMetricLabel = "MIX WIND (M/S).."
            transDirLabel = "MIXNG WIND DIR.."
            transSpdLabel = "MIXNG WIND SPD.."
            transSpdMetricLabel = "MIX WND SPD M/S."
        else:
            transLabel = "TRANSPORT WIND.."
            transMetricLabel = "TRAN WIND (M/S)."
            transDirLabel = "TRANSP WIND DIR."
            transSpdLabel = "TRANSP WIND SPD."
            transSpdMetricLabel = "TRANS SPD (M/S)."
                            
        if self._tableTransElementSplit == "no" and colWidth >=7:
            # Baseline
            if self._includeMetricDispersion == "yes":
                if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                    smoke = [(mixLabel, self._mixingHeight_value),
                             (mixMetricLabel, self._mixingHeightMetric_value),
                             (transLabel, self._trans_value),
                             (transMetricLabel, self._transMetric_value)]
                    trans = [(transLabel, self._trans_value),
                             (transMetricLabel, self._transMetric_value)]
                else:
                    smoke = [(mixLabel, self._mixingHeight_value),
                             (mixMetricLabel, self._mixingHeightMetric_value),
                             (transLabel, self._trans_value),
                             (transDirLabel, self._transNumDir_value),
                             (transMetricLabel, self._transMetric_value)]
                    trans = [(transLabel, self._trans_value),
                             (transDirLabel, self._transNumDir_value),
                             (transMetricLabel, self._transMetric_value)]
            else:
                if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                    smoke = [(mixLabel, self._mixingHeight_value),
                             (transLabel, self._trans_value)]
                    trans = [(transLabel, self._trans_value)]
                else:
                    smoke = [(mixLabel, self._mixingHeight_value),
                             (transLabel, self._trans_value),
                             (transDirLabel, self._transNumDir_value)]
                    trans = [(transLabel, self._trans_value),
                             (transDirLabel, self._transNumDir_value)]
        else:
            # Baseline
            if self._includeMetricDispersion == "yes":
                if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                    smoke = [(mixLabel, self._mixingHeight_value),
                             (mixMetricLabel, self._mixingHeightMetric_value),
                             (transDirLabel, self._transDir_value),
                             (transSpdLabel, self._transSpd_value),
                             (transSpdMetricLabel, self._transSpdMetric_value)]
                    trans = [(transDirLabel, self._transDir_value),
                             (transSpdLabel, self._transSpd_value),
                             (transSpdMetricLabel, self._transSpdMetric_value)]
                else:
                    smoke = [(mixLabel, self._mixingHeight_value),
                             (mixMetricLabel, self._mixingHeightMetric_value),
                             (transDirLabel, self._transDir_value),
                             (transDirLabel, self._transNumDir_value),
                             (transSpdLabel, self._transSpd_value),
                             (transSpdMetricLabel, self._transSpdMetric_value)]
                    trans = [(transDirLabel, self._transDir_value),
                             (transDirLabel, self._transNumDir_value),
                             (transSpdLabel, self._transSpd_value),
                             (transSpdMetricLabel, self._transSpdMetric_value)]
            else:
                if self._bothAlphaNumericDict.get("TransWind", "No") == "No":
                    smoke = [(mixLabel, self._mixingHeight_value),
                             (transDirLabel, self._transDir_value),
                             (transSpdLabel, self._transSpd_value)]
                    trans = [(transDirLabel, self._transDir_value),
                             (transSpdLabel, self._transSpd_value)]
                else:
                    smoke = [(mixLabel, self._mixingHeight_value),
                             (transDirLabel, self._transDir_value),
                             (transDirLabel, self._transNumDir_value),
                             (transSpdLabel, self._transSpd_value)]
                    trans = [(transDirLabel, self._transDir_value),
                             (transDirLabel, self._transNumDir_value),
                             (transSpdLabel, self._transSpd_value)]
        if self._includeMetricDispersion == "yes":
            mix = [(mixLabel, self._mixingHeight_value),
                   (mixMetricLabel, self._mixingHeightMetric_value)]
        else:
            mix = [(mixLabel, self._mixingHeight_value)]
            
        ### sky/wx/hazard tabular phrase configuration ###

        if self._elementFormatDict.get("Sky", "alpha") == "alpha":
            if self._bothAlphaNumericDict.get("Sky", "No") == "No":
                skywx = [("SKY COVER.......", self._sky_value),
                         ("WEATHER COV.....", self._weatherCov_value),
                         ("WEATHER TYPE....", self._weatherType_value),
                         ("TSTM COV........", self._tstmCov_value)]
            else:
                skywx = [("SKY COVER.......", self._sky_value),
                         ("SKY (%).........", self._numSky_value),
                         ("WEATHER COV.....", self._weatherCov_value),
                         ("WEATHER TYPE....", self._weatherType_value),
                         ("TSTM COV........", self._tstmCov_value)]
        else:
            skywx = [("SKY (%).........", self._sky_value),
                     ("WEATHER COV.....", self._weatherCov_value),
                     ("WEATHER TYPE....", self._weatherType_value),
                     ("TSTM COV........", self._tstmCov_value)]
            
        hazard = [("HAZARD VTEC 1...", self._wwa_value),
                  ("HAZARD VTEC 2...", self._wwa2_value),
                  ("HAZARD VTEC 3...", self._wwa3_value)]
        
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
            ("TEMPERATURE"             , 1,[("TEMP............", self._temp_value)]),
            ("DEWPOINT"                , 1,[("DEWPOINT........", self._td_value)]),
            ("HUMIDITY"                , 1,[("RH..............", self._rh_value)]),
            ("20 FOOT WINDS"           , 1, wind),
            ("EYE LEVEL WINDS"         , 1, eyewind),
            ("SURFACE WINDS"           , 1, sfcwind),
            #("RIDGE TOP WIND"          , 1, ridge),
            #("SMOKE DISPERSION"        , 1, smoke),
            #("MIXING HEIGHT"           , 1, mix),
            #("TRANSPORT WINDS"         , 1, trans),
            ("DISPERSION INDEX"        , 1,[("DISPERSION......", self._dsi_value)]),
            ("LDSI"                    , 1,[("DISPERSION IDX..", self._ldsi_value)]),
            ("LVORI"                   , 1,[("LVORI...........", self._lvori_value)]),
            ("ADI"                     , 1,[("ADI.............", self._adi_value)]),
            #("CWR"                     , 1,[("CWR.............", self._cwr_value)]),
            ("POP"                     , 1,[("CHC OF PCPN (%).", self._pop_value)]),
            #("LIGHTNING ACTIVITY LEVEL", 1,[("LAL.............", self._lal_value)]),
            ("HAINES INDEX"            , 1,[("HAINES INDEX....", self._haines_value)]),
            ("VENTILATION RATE"        , 1,[("VRATE KT-FT/1000", self._ventrate_value)]),
            ("SWELL HEIGHT"            , 1, swell),
            ("SWELL PERIOD"            , 1,[("SWELL PERIOD (S)", self._swellPeriod_value)]),
            ("WIND WAVE"               , 1,[("WIND WAVE (FT)..", self._windWave_value)]),
            ("WAVE HEIGHT"             , 1,[("WAVE HEIGHT (FT)", self._waveHeight_value)]),
            ("FREEZING LEVEL"          , 1,[("FZ LEVEL (KFT)..", self._freezingLevel_value)]),
            ("CEILING"                 , 1,[("CEILING (KFT)...", self._ceiling_value)]),
            ("VISIBILITY"              , 1,[("VISIBILITY (SM).", self._visibility_value)]),
            ("ICING"                   , 1,[("ICING...........", self._ceiling_value)]),
            ("HAZARDS"                 , 0, hazard),
            ("HEAT INDEX"              , 1,[("HEAT INDEX (F)..", self._heatIndex_value)]),
            ]
    
    def _getProductInfo(self, product, issuance, forecasters):
        # Parse the spot request information selected and
        # return the FireInfo for display.
        timezone =  os.environ["TZ"]
        spotRequestInfo = [
            ("PROJECT NAME:", "fireName", "'xxxx'"),
            ("PROJECT TYPE:", "fireType", "'WILDFIRE'"),
            ("REQUESTING AGENCY:", "requestingAgency", "'xxxx'"),
            ("REQUESTING OFFICIAL:", "agencyContact", "'yyyy'"),
            ("DLAT:", "fireLatitude", "28.27"),
            ("DLON:", "fireLongitude", "82.19"),
            ("SIZE (ACRES):", "fireSize", "1"),
            ("SITE:", "wfoID", "''"),
            ("OFILE:", "webSiteTag", "''"),
            ("TIMEZONE:", "webTimeZone", "timezone"),
            ("DATE:", "fireDate", "'1/1/01'"),
            ("TIME:", "fireTime", "'1300'"),
            ]

        obs = []
        self._spotList = ["This is a New Incident"]
        remarksFlag = 0
        remarks = ""

        self._periodElementDict = {
            "Today": [], "Tonight": [], "Tomorrow": []
            }

        self._periodAllElementDict = {
            "Today": [], "Tonight": [], "Tomorrow": []
            }
      
        # Set default values
        for field, variable, default in spotRequestInfo:
            exec "self._"+variable + " = " + default

        # If no issuance to use, we are done.
        if issuance is None:
            return
        self._productIssuance = issuance
      
        # If no forecasters included, we are done.
        if forecasters is None:
            return
        self._forecasters = forecasters
      
        # If no product to parse, we are done
        if product is None:
            # Use default list of weather elements
            for element, defaultFlag, phrases, searchStrings in self._weInfoList():
                if defaultFlag:
                    self._periodAllElementDict["Today"].append(element)
                    self._periodAllElementDict["Tonight"].append(element)
                    self._periodAllElementDict["Tomorrow"].append(element)
                    self._periodElementDict["Today"].append(element)
                    self._periodElementDict["Tonight"].append(element)
                    self._periodElementDict["Tomorrow"].append(element)
                #self._allPeriodElementDict["Today"].append(element)
                #self._allPeriodElementDict["Tonight"].append(element)
                #self._allPeriodElementDict["Tomorrow"].append(element)
                #if defaultFlag:
                #    self._periodElementDict["Today"].append(element)
                #    self._periodElementDict["Tonight"].append(element)
                #    self._periodElementDict["Tomorrow"].append(element)
            for element, defaultFlag in self._weInfoHiddenList():
                if defaultFlag:
                    self._periodElementDict["Today"].append(element)
                    self._periodElementDict["Tonight"].append(element)
                    self._periodElementDict["Tomorrow"].append(element)
                self._periodAllElementDict["Today"].append(element)
                self._periodAllElementDict["Tonight"].append(element)
                self._periodAllElementDict["Tomorrow"].append(element)
            return
     
        # Parse product
        wxParmFlag = 0
        for line in product:
            print line
            if line.find("...WEATHER PARAMETERS REQUESTED...") >= 0:
                wxParmFlag = 1
            if line.find("SITE:") >= 0:
                wxParmFlag = 0
            # If the line has a colon, split it into fieldName/value
            cleanLine = string.replace(string.upper(line),"\n", "")
            cleanLine = cleanLine.strip()
            index = cleanLine.find(":")
            if index >= 0:
                # Handle STQ fields (lines with a colon)
                fieldName = cleanLine[:index].strip()
                value = cleanLine[index+1:].strip()
          
                for field, variable, default in spotRequestInfo:
                    if field in cleanLine and cleanLine.find(field) == 0:
                        # Assign to variable
                        exec "self._"+variable + " = value"

                if wxParmFlag:
                    for element, defaultFlag, phrases, searchStrings in self._weInfoList():
                        if self._checkStrs(searchStrings, fieldName) == 1:
                            #Enter flags in dictionary e.g. 1,1,1 for Today, Tonight, Tomorrow
                            flags = value.split(",")
                            if flags[0] == "1":
                                self._periodElementDict["Today"].append(element)
                            if flags[1] == "1":
                                self._periodElementDict["Tonight"].append(element)
                            if flags[2] == "1":
                                self._periodElementDict["Tomorrow"].append(element)
                            self._periodAllElementDict["Today"].append(element)
                            self._periodAllElementDict["Tonight"].append(element)
                            self._periodAllElementDict["Tomorrow"].append(element)
                  
            if "ELEV=" in line and "TIME=" in line:
                ob = string.replace(string.upper(line),"\n","")
                if "ELEV= TIME=" not in ob:
                    obs.append(ob)
            if remarksFlag and "FORECAST ELEMENTS" not in line:
                remarks = remarks + line
            if "...REMARKS..." in line:
                remarksFlag = 1
            if "...WEATHER PARAMETERS REQUESTED..." in line:
                remarksFlag = 0
                remarks = string.replace(remarks,"\n\n","\n")
                remarks = string.replace(remarks,"\n\n","\n")

        for element, defaultFlag in self._weInfoHiddenList():
            if defaultFlag:
                if len(self._periodElementDict["Today"]) != 0:
                    self._periodElementDict["Today"].append(element)
                if len(self._periodElementDict["Tonight"]) != 0:
                    self._periodElementDict["Tonight"].append(element)
                if len(self._periodElementDict["Tomorrow"]) != 0:
                    self._periodElementDict["Tomorrow"].append(element)
            self._periodAllElementDict["Today"].append(element)
            self._periodAllElementDict["Tonight"].append(element)
            self._periodAllElementDict["Tomorrow"].append(element)

    def _displayFireInfo(self):
      
        # Build and display GUI using the fireInfo
        varList = []

        # Fire Type Section of the GUI
        desTypeList = "Type of Fire:", "fireType"
        varList.append((desTypeList, self._fireType, "radio", self._typeList))

        # requesting Agency Section of the GUI
        desAgencyNameList = "Agency:", "requestingAgency"
        agencyNameList = []
        findAgencyFlag = 0
        for agency in self._agencyList:
            id,name = agency
            agencyNameList.append(name)
            if self._requestingAgency == name:
                findAgencyFlag = 1
                requestingAgencyDefault = self._requestingAgency
        if not findAgencyFlag:
            agencyNameList.append("Unlisted")
            requestingAgencyDefault = "Unlisted"
        varList.append((desAgencyNameList, requestingAgencyDefault, "radio", agencyNameList))

        # Include Extendeds/Outlook Section of the GUI
        if not self._shortTermOnly:
            questionList = ["Include Day 3-5 Extended?",
                            "Include Day 6-7 Extended?",
                            "Include Day 8-14 Outlook?"]
            desExtendedQuestions = "Check Items to Include:","extendedQuestions"
            varList.append((desExtendedQuestions, [], "check", questionList))

        # Forecast Type
        desFcstType = "What Type of Forecast?", "forecastType"
        labelList = []
        for label, forecastType in self._forecastTypeList:
            labelList.append(label)
        varList.append((desFcstType, self._defaultForecastType, "radio", labelList))

        # Include Ignition Time Forecast Section of the GUI
        if self._includeIgnitionOptionOnGUI:
            desIT = ("Include Ignition Times?", "withIgnitionTimes")
            varList.append((desIT, self._withIgnitionTimes, "radio", ["yes", "no"]))
            
        # Unlisted Agency Name Section of the GUI
        if not findAgencyFlag:
            desOtherAgencyName = "Name of Agency if not listed....", "otherAgencyName"
            varList.append((desOtherAgencyName, self._requestingAgency, "alphaNumeric"))

        # Fire Name Section of the GUI
        desFireName = "Name of Fire ...................................", "fireName"
        varList.append((desFireName, self._fireName, "alphaNumeric"))

        # Fire Time Section of the GUI
        desFireTime = "Time of Fire .....................................", "fireTime"
        varList.append((desFireTime, self._fireTime, "alphaNumeric"))

        # Fire Date Section of the GUI
        desFireDate = "Date of Fire .....................................", "fireDate"
        varList.append((desFireDate, self._fireDate, "alphaNumeric"))

        # Agency Contact Section of the GUI
        desAgencyContact = "Name of Agency Contact..........", "agencyContact"
        varList.append((desAgencyContact, self._agencyContact, "alphaNumeric"))

        # Fire Latitude Section of the GUI
        desFireLatitude = "Fire Latitude (Deg).......................", "fireLatitude"
        varList.append((desFireLatitude, self._fireLatitude, "alphaNumeric"))

        # Fire Longitude Section of the GUI
        desFireLongitude = "Fire Longitude (Deg)...................", "fireLongitude"
        varList.append((desFireLongitude, self._fireLongitude, "alphaNumeric"))
      
        # Fire Size Section of the GUI
        desFireSize = "Fire Size (Acres) .........................", "fireSize"
        varList.append((desFireSize, self._fireSize, "alphaNumeric"))          
      
        # Forecast Elements Section of the GUI
        tableHoursDesc = "Tab Hrs"
        if self._productIssuance in ["Next Day", "Morning", "Morning Update", "Afternoon Update"]:
            desElementList = "Today Elements", "todayElements"
            varList.append((desElementList, self._periodElementDict["Today"],
                            "check", self._periodAllElementDict["Today"]))
            desTableRes = tableHoursDesc,"todayTableRes"
            varList.append((desTableRes, self._defaultTabularResolution["Today"],"radio",
                        self._tabularResolutionDict["Today"]))
        desElementList = "Tonight Elements", "tonightElements"
        varList.append((desElementList, self._periodElementDict["Tonight"] ,
                        "check", self._periodAllElementDict["Tonight"] ))
        if self._tabularAllPeriods == "yes":
            desTableRes = tableHoursDesc,"tonightTableRes"
            varList.append((desTableRes, self._defaultTabularResolution["Tonight"],"radio",
                        self._tabularResolutionDict["Tonight"]))
        desElementList = "Tomorrow Elements", "tomorrowElements"
        varList.append((desElementList, self._periodElementDict["Tomorrow"],
                        "check", self._periodAllElementDict["Tomorrow"] ))
        if self._tabularAllPeriods == "yes":
            desTableRes = tableHoursDesc,"tomorrowTableRes"
            varList.append((desTableRes, self._defaultTabularResolution["Tomorrow"],"radio",
                        self._tabularResolutionDict["Tomorrow"]))

        if self._productIssuance in ["Afternoon with 4 periods", "Evening Update with 4 periods",
                                     "Early Morning Update with 4 periods"]:
            desElementList = "Tomorrow Night Elements", "tomorrowNightElements"
            varList.append((desElementList, self._periodElementDict["Tomorrow"],
                            "check", self._periodAllElementDict["Tomorrow"] ))
            if self._tabularAllPeriods == "yes":
                desTableRes = tableHoursDesc,"tomorrowNightTableRes"
                varList.append((desTableRes, self._defaultTabularResolution["Tomorrow"],"radio",
                            self._tabularResolutionDict["Tomorrow"]))
            desElementList = "Next Day Elements", "nextDayElements"
            varList.append((desElementList, self._periodElementDict["Tomorrow"],
                            "check", self._periodAllElementDict["Tomorrow"] ))
            if self._tabularAllPeriods == "yes":
                desTableRes = tableHoursDesc,"nextDayTableRes"
                varList.append((desTableRes, self._defaultTabularResolution["Tomorrow"],"radio",
                            self._tabularResolutionDict["Tomorrow"]))
                
        # Launch the Spot Request Quality Control GUI.
        varDict = self._callProcessVariableList("Input Info", varList, varDict={})
        if varDict is None:
            return None

        # Set up varDict for forecastType using labels
        value = varDict[desFcstType]
        for label, forecastType in self._forecastTypeList:
            if label == value:
                varDict[desFcstType] = forecastType
                break

        # This section of code filters the forecaster entries to ensure that
        # single quotes are not included.
        if not findAgencyFlag:
            try:
                varDict[desOtherAgencyName] = string.replace(varDict[desOtherAgencyName],"\'","")
            except AttributeError:
                print "Other Agency Name is not a string."
        try:
            varDict[desFireName] = string.replace(varDict[desFireName],"\'","")
        except AttributeError:
            print "Fire Name is not a string."
        try:
            varDict[desAgencyContact] = string.replace(varDict[desAgencyContact],"\'","")
        except AttributeError:
            print "Fire Size is not a string."
        try:
            varDict[desFireSize] = string.replace(str(varDict[desFireSize]),"\'","")
        except AttributeError:
            print "Fire Size is not a string."
        try:
            varDict[desFireLatitude] = string.replace(str(varDict[desFireLatitude]),"\'","")
        except AttributeError:
            print "Latitude is not a string."
        try:
            varDict[desFireLongitude] = string.replace(str(varDict[desFireLongitude]),"\'","")
        except AttributeError:
            print "Longitude is not a string."
        try:
            varDict[desFireTime] = string.replace(str(varDict[desFireTime]),"\'","")
        except AttributeError:
            print "Ignition Time is not a string."
        try:
            varDict[desFireDate] = string.replace(str(varDict[desFireDate]),"\'","")
        except AttributeError:
            print "Ignition Date is not a string."
                           
        # This section of code filters the forecaster entries to ensure that
        # double quotes are not included.
        if not findAgencyFlag:
            try:
                varDict[desOtherAgencyName] = string.replace(varDict[desOtherAgencyName],"\"","")
            except AttributeError:
                print "Other Agency Name is not a string."
        try:
            varDict[desFireName] = string.replace(varDict[desFireName],"\"","")
        except AttributeError:
            print "Fire Name is not a string."
        try:
            varDict[desAgencyContact] = string.replace(varDict[desAgencyContact],"\"","")
        except AttributeError:
            print "Fire Size is not a string."
        try:
            varDict[desFireSize] = string.replace(varDict[desFireSize],"\"","")
        except AttributeError:
            print "Fire Size is not a string."
        try:
            varDict[desFireLatitude] = string.replace(varDict[desFireLatitude],"\"","")
        except AttributeError:
            print "Latitude is not a string."
        try:
            varDict[desFireLongitude] = string.replace(varDict[desFireLongitude],"\"","")
        except AttributeError:
            print "Longitude is not a string."
        try:
            varDict[desFireTime] = string.replace(varDict[desFireTime],"\"","")
        except AttributeError:
            print "Ignition Time is not a string."
        try:
            varDict[desFireDate] = string.replace(varDict[desFireDate],"\"","")
        except AttributeError:
            print "Ignition Date is not a string."
                           
        # convert lat/lon to floats
        try:
            varDict[desFireLatitude] = string.atof(varDict[desFireLatitude])
        except ValueError:
            print "Latitude is not a float."
        try:
            varDict[desFireLongitude] = string.atof(varDict[desFireLongitude])
        except ValueError:
            print "Longitude is not a float."

        # Convert fireTime
        fireTime = varDict[desFireTime]
        fireTime = "000" + str(int(float(fireTime)))
        fireTime = fireTime[-4:]
        varDict[desFireTime] = fireTime
                           
        # Here are more varDict settings that need to be set before we launch
        # the formatter.
        varDict[("Product Issuance:", "productIssuance")] = self._productIssuance
        varDict[("Forecaster:", "forecaster")] = self._forecasters
        if self._includeCreationTimeOnGUI:
            varDict[("Creation Date", "creationDate")] = self._creationDate
            varDict[("Creation Time", "creationTime")] = self._creationTime                
        varDict[("WebSiteTag:", "webSiteTag")] = self._webSiteTag
        varDict[("WFOid:", "wfoID")] = self._wfoID
        varDict[("TimeZone:", "fireTZ")] = self._webTimeZone
        
        if self._shortTermOnly:
            varDict[("Check Items to Include:","extendedQuestions")] = []

        return varDict

    # From FWF. Needed to change .EXTENDED... to .FORECAST DAYS 3 THROUGH 7
    def setLabel(self, tree, component):
        if self._includeExtended:
            if "Include Day 3-5 Extended?" not in self._extendedQuestions:
               component.set("words", ".FORECAST DAYS 6 THROUGH 7...\n")
            else:
               component.set("words", ".FORECAST DAYS 3 THROUGH 7...\n")
        else:
            component.set("words", ".FORECAST DAYS 3 THROUGH 5...\n")
        return self.DONE()

    # From FWF.  Modifed to write the output to a file in a user specified
    # directory on the local lx machine. In addition, added sections to
    # insert headlines, discussion, and 8-14 day outlook.
    def generateForecast(self, argDict):
        # Generate Text Phrases for a list of edit areas

        # Get variables
        error = self._getVariables(argDict)
        if error is not None:
            return error

        # Quality Control Gui data
        error = self._qualityControlFormData()
        if error is not None:
            return error

        # Get the areaList -- derived from the lat, lon, size of fire (acres),
        # and the name of the fire.
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

        # Generate the Headlines for the Product
        for editArea, areaLabel in self._areaList:
            fcst = self._makeHeadline(fcst, editArea, areaLabel, argDict)

        # Generate the Discussion section
        fcst = self._makeDiscussion(fcst, argDict)

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
          
        # Generate the summary extended section (if wanted)
        fcst = self._makeSummaryExtended(fcst, argDict)

        # Generate the 8 to 14 Day Outlook section
        error = self._generateOutlookLabels(argDict)
        if error is not None:
            return error
        fcst = self._make8to14DayOutlook(fcst, argDict)

        fcst = self._postProcessProduct(fcst, argDict)

        return fcst

    def _determineTimeRanges(self, argDict):
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
                self._issuanceInfo.narrativeDef().append(\
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
            self._extendedStart = self._timeRange.endTime() - 24*5*3600
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

#   Quality Control Form Information from user dialog to ensure completeness.


    # 04/24/07:  Tabular/Narrative is okay for wildfires. Changed code to make
    #            Tabular Only into Tabular/Narrative for wildfires.

    # From FWS_Overrides. Fixed a bug that causes the formatter to crash
    # when a number was entered for the agency or the contact. Added a
    # method called _convertToStr to do this cleanly.
    def _qualityControlFormData(self):

        # If fireSize is not an integer, then I default the size to 1 acre.
        # This will allow the formatter to run even if the user puts invalid
        # characters into the size field (like 10-20).
        try:                          
            self._fireSize = int(float(self._fireSize)+0.5)
        except ValueError:                  
            self._fireSize = 1              
        if self._fireSize <= 0:                  
            self._fireSize = 1
          
        try:                      
            lat = float(self._fireLatitude)
        except ValueError:              
            return "Invalid latitude value."      
        if lat < 0.0 or lat > 90:              
            return "Invalid latitude value."          

        try:                      
            lon = float(self._fireLongitude)
        except ValueError:                  
             return "Invalid longitude value."             
        if lon < 0.0 or lon > 180.0:
            return "Invalid longitude value. Must be positive."

        if len(self._forecaster) == 0:
            return "You must select at least one forecaster in the list."

        if self._productIssuance in ["Next Day", "Morning", "Morning Update", "Afternoon Update"]:
            elementLists = [self._todayElements, self._tonightElements, self._tomorrowElements]
        elif self._productIssuance in ["Afternoon with 4 periods", "Evening Update with 4 periods",
                                       "Early Morning Update with 4 periods"]:
            elementLists = [self._tonightElements, self._tomorrowElements,
                            self._tomorrowNightElements, self._nextDayElements]
        else:
            elementLists = [self._tonightElements, self._tomorrowElements]
          
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
        # the FWS product (if this is a wildfire incident) was added to this
        # method.
        if self._fireType == "WILDFIRE":      
            for element in self._wildfireElementList:
                for elementList in elementLists:
                    if element not in elementList and len(elementList) != 0:
                        elementList.append(element)
            if self._tabularForWildfire == "no" and \
               self._forecastType == "Tabular Only":
                self._forecastType = "Tabular/Narrative"

        self._fireName = self._convertToStr(self._fireName)
        if len(self._fireName) == 0:              
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

    def _convertToStr(self, var):
        try:
            stringSize = len(var)
            return var
        except TypeError:
            try:
                var = `int(var+0.5)`
            except TypeError:
                var = `var`
            return var

    def _getVariables(self, argDict):
        # Make argDict accessible
        self.__argDict = argDict

        self._todayTableRes = "None"
        self._tonightTableRes = "None"
        self._tomorrowTableRes = "None"
        self._tomorrowNightTableRes = "None"
        self._nextDayTableRes = "None"
        self._todayElements = []
        self._tonightElements = []
        self._tomorrowElements = []
        self._tomorrowNightElements = []
        self._nextDayElements = []

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

        # Adjust creationTime if user-supplied creation date and time
        if self._includeCreationTimeOnGUI:
            if self._creationDate != "":
                argDict["creationTime"] = self._getTime(self._creationDate, self._creationTime)
        return None

    def _getTime(self, date, t):
        # Make a unix time integer from the given date and time strings
        if t == "":
            t = "0000"
        else:
            t = "000" + `int(t)`
            t = t[-4:]
        rtz = self._getActualTimeZone()
        stz = time.tzname[0]
        dtz = time.tzname[1]
        otz = stz[0:1]
        ptz = rtz[0:1]
        offset = 0
        if otz == ptz:
            cTime = time.strptime(t + ' ' + date + ' ' + rtz, '%H%M %m/%d/%y %Z')
        else:
            if ptz == "E":
                if otz == "E":
                    offset = 0
                elif otz == "C":
                    offset = -1
                elif otz == "M":
                    offset = -2
                elif otz == "P":
                    offset = -3
            elif ptz == "C":
                if otz == "E":
                    offset = 1
                elif otz == "C":
                    offset = 0
                elif otz == "M":
                    offset = -1
                elif otz == "P":
                    offset = -2
            elif ptz == "M":
                if otz == "E":
                    offset = 2
                elif otz == "C":
                    offset = 1
                elif otz == "M":
                    offset = 0
                elif otz == "P":
                    offset = -1
            elif ptz == "P":
                if otz == "E":
                    offset = 3
                elif otz == "C":
                    offset = 2
                elif otz == "M":
                    offset = 1
                elif otz == "P":
                    offset = 0
            if stz[1:3] == rtz[1:3]:
                cTime = time.strptime(t + ' ' + date + ' ' + stz, '%H%M %m/%d/%y %Z')
            else:
                cTime = time.strptime(t + ' ' + date + ' ' + dtz, '%H%M %m/%d/%y %Z')

        return time.mktime(cTime) + offset*3600

    def _getActualTimeZone(self):
        # Return the correct time zone based on DST and fireTZ variable
        if self._fireTZ.find('/') >= 0:
            standardTimeZone, daylightTimeZone = time.tzname
        elif len(self._fireTZ) == 9:
            standardTimeZone = self._fireTZ[:4]
            daylightTimeZone = self._fireTZ[5:]
        else:
            standardTimeZone = self._fireTZ[:3]
            daylightTimeZone = self._fireTZ[4:]

        if self.daylight() == 1:
            actualTimeZone = daylightTimeZone
        else:
            actualTimeZone = standardTimeZone
        return actualTimeZone    

    def _makeFcstTimeStatement(self, fcst, argDict):
        requestWords = self._getRequestWords()
        rtz = self._getActualTimeZone()
        stz = time.tzname[0]
        dtz = time.tzname[1]
        otz = stz[0:1]
        ptz = rtz[0:1]
        if otz == ptz:
            self._fireDateTime = time.strptime(
                self._fireTime + ' ' + self._fireDate + ' ' + rtz,
                '%H%M %m/%d/%y %Z')
            fcst = fcst + time.strftime(
                'FORECAST IS BASED ON ' + requestWords + ' TIME OF %H%M %Z ON %B %d. ',
                self._fireDateTime)
        else:
            offset = 0
            if ptz == "E":
                if otz == "E":
                    offset = 0
                elif otz == "C":
                    offset = -1
                elif otz == "M":
                    offset = -2
                elif otz == "P":
                    offset = -3
            elif ptz == "C":
                if otz == "E":
                    offset = 1
                elif otz == "C":
                    offset = 0
                elif otz == "M":
                    offset = -1
                elif otz == "P":
                    offset = -2
            elif ptz == "M":
                if otz == "E":
                    offset = 2
                elif otz == "C":
                    offset = 1
                elif otz == "M":
                    offset = 0
                elif otz == "P":
                    offset = -1
            elif ptz == "P":
                if otz == "E":
                    offset = 3
                elif otz == "C":
                    offset = 2
                elif otz == "M":
                    offset = 1
                elif otz == "P":
                    offset = 0
            if stz[1:3] == rtz[1:3]:
                self._fireDateTime = time.strptime(
                    self._fireTime + ' ' + self._fireDate + ' ' + stz,
                    '%H%M %m/%d/%y %Z')
                tempTime = time.mktime(self._fireDateTime) + offset*3600
                self._fireDateTime = time.localtime(tempTime)
            else:
                self._fireDateTime = time.strptime(
                    self._fireTime + ' ' + self._fireDate + ' ' + dtz,
                    '%H%M %m/%d/%y %Z')
                tempTime = time.mktime(self._fireDateTime) + offset*3600
                self._fireDateTime = time.localtime(tempTime)
            fireDateTime = time.strptime(
                self._fireTime + ' ' + self._fireDate, '%H%M %m/%d/%y')
            fcst = fcst + time.strftime(
                'FORECAST IS BASED ON ' + requestWords + ' TIME OF %H%M ' + rtz + ' ON %B %d. ',
                fireDateTime)
        fcst = fcst + "\n"
        self._makeFireTimeRange()
        return fcst
    
    def _makeFireTimeRange(self):
        # Make a 1-hour fire time range for the fireTime
        if self._withIgnitionTimes == "no":
            return None
        fireDateTime = time.mktime(self._fireDateTime)
        self._fireTR = self.makeTimeRange(fireDateTime, fireDateTime+3600)
        print "Fire Time Range:", self._fireTR

    def _checkFireTR(self, tr):
        if self._fireTR is None:
            return 0
        return self._fireTR.overlaps(tr)

    # This is a new method that Matt Davis wrote. Figures out whether or not
    # we are using a ignition time, request time, or incident time.
    def _getRequestWords(self):
        if self._fireType == "WILDFIRE":
            return "REQUEST"
        elif self._fireType == "PRESCRIBED":
            return "IGNITION"
        else:
            return "INCIDENT"
        
    # Import the discussion from a previously edited discussion file.
    def _makeDiscussion(self, fcst, argDict):

        discussionHeader = ""
        discussionHeader = ".DISCUSSION...\n"

        if self._insertDiscussionFromFile == 1:      
            discussion = ""
            if os.path.isfile(self._discussionFile):
                input = open(self._discussionFile)
                text = input.readlines()
                for line in text:
                    discussion = discussion + line
                discussion = string.join(string.split(discussion,"\n\n"),"\n")
                discussion = string.join(string.split(discussion,"\n\n"),"\n")
                return fcst + discussionHeader + discussion + "\n"
            else:
                discussion = "...PUT DISCUSSION TEXT HERE..."
                return fcst + discussionHeader + discussion + "\n\n"
        elif self._insertDiscussionFromFile == 2:
            version = 0
            fwfPil = self._statePil + self._fwfPil
            searchString=""
            product = self.getPreviousProduct(fwfPil, searchString, version=version)
            product = string.split(product, "\n")
            discussion = ""
            disFlag = 0
            foundDiscussion = 0
            for line in product:
                if string.find(line,"DISCUSSION...") != -1:
                    disFlag = 1
                    foundDiscussion = 1
                try:
                    if line[2] == "Z" and line[-1] == "-" and \
                       (line[6] == "-" or line[6] == ">"):
                        disFlag = 0
                except IndexError:
                    #print "Discussion Index Error",line
                    a = 0
                if line[:2] == "$$":
                    disFlag = 0
                if disFlag:
                    discussion = discussion + line + "\n"
            if foundDiscussion:
                return fcst + discussion + "\n\n"
            else:
                discussion = "...PUT DISCUSSION TEXT HERE..."
                return fcst + discussionHeader + discussion + "\n\n"
        else:
            return fcst + discussionHeader + "\n\n\n"
    
#   Create areaList based on lat/lon/size/firename.
    def _determineAreaList(self, argDict):

        # Size of the fire is entered as acres.
        # Convert this area into square kilometers.
        # createLatLonArea only needs the length of the side of a square. 
        size_out = int(math.sqrt(float(self._fireSize)/247.0) + 0.5)
        area = self.createLatLonArea(float(self._fireLatitude),
                                     float(0.0 - self._fireLongitude),
                                     size_out)
        # SET UP FOR HAZARDS
        # Save to server
        self.saveEditAreas([area])
        # Create Hazards Table for this area
        hazards = HazardsTable.HazardsTable(
                  argDict["ifpClient"], [[area.getId().getName()]], "FWS",
                  self.filterMethod, argDict["databaseID"],
                  self._fullStationID,
                  activeTableName = argDict['vtecActiveTable'],
                  vtecMode = argDict['vtecMode'],
                  dataMgr=argDict['dataMgr'])
        argDict["hazards"] = hazards
        # Remove from server
        self.deleteEditAreas([area])

        self._areaList = [(area, self._fireName)]

#   Set the extended configuration based on user input.
    def _setExtendedConfig(self):
      
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

        if self._insertUnrepresentStatement == 1:      
            return fcst + self._unrepresentStatement + "\n\n"
        else:
            return fcst
             
    # Place the headlines above the discussion.
    def _makeHeadline(self, fcst, editArea, areaLabel, argDict):

        # get the hazards text
        self._hazards = argDict['hazards']
        self._combinations = argDict["combinations"]
        #hlList =  tree.stats.get("Hazards", self._timeRange, areaLabel)
        #print "hlList = ",hlList
        headlines = self.generateProduct("Hazards", argDict,
                                    area = editArea, areaLabel=areaLabel,
                                    timeRange = self._timeRange)
        fcst = fcst + headlines
        return fcst
             
    # From FWF. Modified to eliminate the UGC header not needed in the FWS.
    # Since Headlines are placed above the discussion...that eliminated too.
    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        return fcst

    # From FWF. Modified to eliminate everything.
    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        if self._individualExtended == 1:
            fcst = fcst + "\n"
        return fcst

    # Deal with the summary extended more cleanly.
    def _makeSummaryExtended(self, fcst, argDict):

        # Add one extended
        if self._summaryExtended == 1:
            extended = self.generateProduct("ExtendedNarrative",
                argDict, area=self._summaryArea,
                timeRange=self._extendedRange)
            fcst = fcst + extended
            fcst = fcst + "\n"
        return fcst
             
# From FWS_Overrides. changes commented on the right margin. (just two lines)

    def _generateOutlookLabels(self, argDict):

        today = argDict["creationTime"]
        if self._productIssuance in ["Morning", "Morning Update", "Afternoon Update", "Next Day"]:
            day8 = today + 7*24*3600
            day14 = today + 13*24*3600
            dow = time.gmtime(today)[6]
            if dow == 0 or dow == 2 or dow == 4:
                self._insertOutlookFlag = 1
            else:
                self._insertOutlookFlag = 0
            self._insertOutlookFlag = 1
        else:
            currentHour = time.gmtime(today)[3]
            if currentHour < 16:
                day8 = today + 7*24*3600
                day14 = today + 13*24*3600
                dow = time.gmtime(today)[6]
                if dow == 0 or dow == 2 or dow == 4:
                    self._insertOutlookFlag = 1
                else:
                    self._insertOutlookFlag = 0
                self._insertOutlookFlag = 1
            else:
                day8 = today + 8*24*3600
                day14 = today + 14*24*3600
                dow = time.gmtime(today + 24*3600)[6]
                if dow == 1 or dow == 3 or dow == 6:
                    self._insertOutlookFlag = 1
                else:
                    self._insertOutlookFlag = 0
                self._insertOutlookFlag = 1

        self._outlookDay8Label = time.strftime("%A %B %d",time.gmtime(day8))
        self._outlookDay14Label = time.strftime("%A %B %d",time.gmtime(day14))
      
        return None
      
    # Import the 8 to 14 day outlook into the product
    # if the user requests it for the spot forecast.
    def _make8to14DayOutlook(self, fcst, argDict):

        if "Include Day 8-14 Outlook?" not in self._extendedQuestions:
            return fcst
      
        outlookHeader = ".OUTLOOK FOR " + self._outlookDay8Label + " THROUGH " \
                        + self._outlookDay14Label + "...\n"
        outlookHeader = string.upper(outlookHeader)

        if self._insertOutlookFromFile == 1:
            outlook = ""
            if os.path.isfile(self._outlookFile):
                input = open(self._outlookFile)
                text = input.readlines()
                for line in text:
                    outlook = outlook + line
                outlook = string.join(string.split(outlook,"\n\n"),"\n")
                outlook = string.join(string.split(outlook,"\n\n"),"\n")
                return fcst + outlookHeader + outlook + "\n"
            else:
                outlook = "...PUT 8 TO 14 DAY OUTLOOK TEXT HERE..."
                return fcst + outlookHeader + outlook + "\n\n"
        elif self._insertDiscussionFromFile == 2:
            version = 0
            fwfPil = self._statePil + self._fwfPil
            searchString=""
            product = self.getPreviousProduct(fwfPil, searchString, version=version)
            product = string.split(product, "\n")
            outlook = ""
            outFlag = 0
            foundOutlook = 0
            for line in product:
                if line[:2] == "$$":
                    outFlag = 0
                if outFlag:
                    outlook = outlook + line + "\n"
                if string.find(line,".OUTLOOK") != -1:
                    outFlag = 1
                    foundOutlook = 1
            if foundOutlook:
                return fcst + outlookHeader + outlook + "\n\n"
            else:
                outlook = "...PUT 8 TO 14 DAY OUTLOOK TEXT HERE..."
                return fcst + outlookHeader + outlook + "\n\n"
        else:
            return fcst + outlookHeader + "\n\n\n"
             
   # From FWF. Modified to append the fire name and agency name to the
    # product name. Modified to eliminate the discussion from method.
    # Modified to include Matt Davis' enhancement (unlisted agency)
    def _preProcessProduct(self, fcst, argDict):

        if self._requestingAgency == "Unlisted":
            newFireName = self._fireName + "..." + self._otherAgencyName
        else:
            newFireName = self._fireName + "..." + self._requestingAgency
        productLabel = self._productName + " FOR " + newFireName

        productLabel = self.checkTestMode(argDict, productLabel)

        issuedByString = self.getIssuedByString()

        # Product header
        fcst =  fcst + self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n" + productLabel + \
               "\nNATIONAL WEATHER SERVICE " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n\n"
        
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
        if ((issueTime - now) < -24*3600) or ((issueTime - now) > 9*24*3600):
            message = \
'''|* The start time for this product is %s.
This is either more than a day in the past or more than 9 days
in the future. *|''' % self._timeLabel
            fcst = '%s\n%s\n\n' % (fcst, message)
        return fcst
    
    def _postProcessProduct(self, fcst, argDict):
        fcst = string.join(string.split(fcst, "\n\n\n"), "\n")
        forecasterString = string.join(self._forecaster,"/")
        if self._webSiteTag == "":
            tagLineString = ""
        else:
            tagLineString = ".TAG " + self._webSiteTag + "/" + self._wfoID + "\n"
        fcst = fcst + "$$\nFORECASTER..." + forecasterString + "\n" + \
               "REQUESTED BY..." + self._agencyContact + "\n" + \
               "TYPE OF REQUEST..." + self._fireType + "\n" + tagLineString
        #self.storeAWIPS(fcst, self._awipsProductID)
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst
  
#############################################################################
#  Weather Element Sampling and Phrase Configuration                              #
#############################################################################

    def _issuance_list(self, argDict):
        narrativeDef = []
        if self._tabularAllPeriods == "yes":
            phantom = "Phantom"
        else:
            # If we are generating a 12-hour table
            # in the first period, need to have an empty
            # narrative so that the sampling will get done.
            phantom = "EmptyFirePeriod"
        if self._productIssuance in ["Next Day", "Morning", "Morning Update", "Afternoon Update"]:
            # Add the first period
            if len(self._todayElements) == 0:
                period = (phantom, "period1")
            else:
                period = ("FirePeriod1", "period1")
            narrativeDef.append(period)

            if len(self._tonightElements) == 0:
                period = (phantom, 12)
            else:
                period = ("FirePeriod2", 12)
            narrativeDef.append(period)

            # Add the third period
            if len(self._tomorrowElements) == 0:
               period = (phantom, 12)
            else:
                period = ("FirePeriod3", 12)
            narrativeDef.append(period)
        else:
            # Add the first period.
            if len(self._tonightElements) == 0:
                period = (phantom, "period1")
            else:
                period = ("FirePeriod2", "period1")
            narrativeDef.append(period)

            # Add the second period
            if len(self._tomorrowElements) == 0:
                period = (phantom, 12)
            else:
                period = ("FirePeriod3", 12)
            narrativeDef.append(period)
            if self._productIssuance in ["Afternoon with 4 periods",
                                         "Evening Update with 4 periods",
                                         "Early Morning Update with 4 periods"]:
                # Add the third period
                if len(self._tomorrowNightElements) == 0:
                    period = (phantom, 12)
                else:
                    period = ("FirePeriod4", 12)
                narrativeDef.append(period)

                # Add the fourth period
                if len(self._nextDayElements) == 0:
                    period = (phantom, 12)
                else:
                    period = ("FirePeriod5", 12)
                narrativeDef.append(period)

        # Add extended if configured to appear
        if "Include Day 3-5 Extended?" in self._extendedQuestions:
            if self._productIssuance in ["Next Day", "Morning", "Morning Update", "Afternoon Update"]: 
                extendedShortTerm = [
                    ("FireExtendedShortTerm", 24),
                    ("FireExtendedShortTerm", 24),
                    ("FireExtendedShortTerm", 24),
                    ]
            elif self._productIssuance in ["Afternoon with 4 periods",
                                         "Evening Update with 4 periods",
                                         "Early Morning Update with 4 periods"]:
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
                    ("FireExtendedShortTerm", 24),
                    ]
        else:
            if self._productIssuance in ["Next Day", "Morning", "Morning Update", "Afternoon Update"]:
                extendedShortTerm = [
                    ("Phantom", 24),
                    ("Phantom", 24),
                    ("Phantom", 24),
                    ]
            elif self._productIssuance in ["Afternoon with 4 periods",
                                         "Evening Update with 4 periods",
                                         "Early Morning Update with 4 periods"]:
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
                    narrativeDef.append(("ExtendedLabel",0))
                if self._includeExtendedShortTerm or self._includeExtended:
                    narrativeDef = narrativeDef + extendedShortTerm
                if self._includeExtended:
                    narrativeDef = narrativeDef + extended
        except:
            pass
        return [
            ("Next Day", 24 + self.DAY(), 24 + self.NIGHT(), 24 + self.NIGHT(),  
             ".TODAY...", "early in the morning", "late in the afternoon",  
             1, narrativeDef),                       
            ("Morning", self.DAY(), self.NIGHT(), self.NIGHT(),
             ".TODAY...", "early in the morning", "late in the afternoon",
             1, narrativeDef),
            ("Morning Update", "issuanceHour", self.NIGHT(), self.NIGHT(),
             ".REST OF TODAY...", "early in the morning", "late in the afternoon",
             1, narrativeDef),
            ("Afternoon Update", "issuanceHour", self.NIGHT(), self.NIGHT(),
             ".REST OF TODAY...", "early in the morning","late in the afternoon",
             1, narrativeDef),
            #  End times are tomorrow:
            ("Afternoon", self.NIGHT(), 24 + self.DAY(), 24 + self.DAY(),
             ".TONIGHT...", "late in the night", "early in the evening",
             1, narrativeDef),
            ("Afternoon with 4 periods", self.NIGHT(), 24 + self.DAY(), 24 + self.DAY(),
             ".TONIGHT...", "late in the night", "early in the evening",
             1, narrativeDef),
            ("Evening Update", "issuanceHour", 24 + self.DAY(), 24 + self.DAY(),
             ".REST OF TONIGHT...", "late in the night","early in the evening",
             1, narrativeDef),
            ("Evening Update with 4 periods", "issuanceHour", 24 + self.DAY(), 24 + self.DAY(),
             ".REST OF TONIGHT...", "late in the night","early in the evening",
             1, narrativeDef),
            # For the early morning update, this produces:
            # REST OF TONIGHT:
            # MONDAY
            # MONDAY NIGHT
            ("Early Morning Update", "issuanceHour", self.DAY(), self.DAY(),
             ".REST OF TONIGHT...", "early in the morning","late in the afternoon",
             0, narrativeDef),
            ("Early Morning Update with 4 periods", "issuanceHour", self.DAY(), self.DAY(),
             ".REST OF TONIGHT...", "early in the morning","late in the afternoon",
             0, narrativeDef),
            ]

    def FirePeriod1(self):
        phraseList = self.getFirePeriod_phraseList(self._todayElements)
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
        phraseList = self.getFirePeriod_phraseList(self._tonightElements)
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
        phraseList = self.getFirePeriod_phraseList(self._tomorrowElements)
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
        phraseList = self.getFirePeriod_phraseList(self._tomorrowNightElements)
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
    
    def FirePeriod5(self):
        phraseList = self.getFirePeriod_phraseList(self._nextDayElements)
        analysisList = self.getFirePeriod_analysisList()
        intersectAreas = self.getFirePeriod_intersectAreas(5)
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
        # Make a label given the timeRange in GMT and the shift to
        # convert it to local time. currentLocalTime can be used to
        # compare to current day.            
        if timeRange.duration() <= 3600:
                return ""

        curLocal, shift = self.determineTimeShift()
        if index == 0 and self._equalDates(currentLocalTime, curLocal):
            try:
                label =  issuanceInfo.period1Label()
                if label != "":
                    return label + "\n"
            except:
                pass
        try:
            today =  issuanceInfo.todayFlag()
        except:
            today = 1
        try:
            useHolidays = self._useHolidays
        except:
            useHolidays = 1
        nextDay24HourLabel = self.nextDay24HourLabel_flag(tree, node)
        splitDay24HourLabel = self.splitDay24HourLabel_flag(tree, node)
        label =  self.getWeekday(timeRange, holidays=useHolidays, shiftToLocal=1,
                                labelType="CapitalWithPeriod", today=today,
                                tomorrow=0, nextDay24HourLabel=nextDay24HourLabel,
                                 splitDay24HourLabel=splitDay24HourLabel)
        return label + "\n"

    def _equalDates(self, t1, t2):
        # If AbsTimes t1 and t2 represent the same day, month, year
        # return 1 else 0
        d1 = t1.day
        d2 = t2.day
        m1 = t1.month
        m2 = t2.month
        y1 = t1.year
        y2 = t2.year
        if d1==d2 and m1==m2 and y1==y2:
            return 1
        else:
            return 0
        
    def increment_nlValue_dict(self, tree, node):
        # Increment for rounding values
        # Units depend on the product
        dict = TextRules.TextRules.increment_nlValue_dict(self, tree, node)
        dict["Wind"] = 1
        dict["Wind20ft"] = 1
        dict["TransWind"] = 1
        dict["CWR"] = 1
        dict["QPF"] = .0001
        dict["Vsby"] = .01
        return dict
     
    def scalar_difference_nlValue_dict(self, tree, node):
        # Scalar difference.  If the difference between scalar values
        # for 2 sub-periods is greater than this value,
        # the different values will be noted in the phrase.
        dict = TextRules.TextRules.scalar_difference_nlValue_dict(self, tree, node)
        dict["Vsby"] = {
            (0.00,1.00) : 0.25,
            (1.00,3.00) : 0.50,
            (3.00,5.00) : 1.00,
            "default" : 2.00,
            }
        dict["PredHgt"] = {
            (0,10) : 1,
            (10,30) : 5,
            (30,100) : 10,
            "default" : 25,
            }
        dict["Td"] = 5
        dict["PoP"] = 10
        return dict       

    def getFirePeriod_phraseList(self, periodElements):
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
                        if type(phrases) is not types.ListType:
                            phrases = [phrases]
                        phraseList += phrases
        if self._forecastType in ["Tabular/Narrative", "Tabular Only"]:
            phraseList.append(self._fwsTable_phrase)
        return phraseList

    # From FWS_Overrides. Added one hourly sampling for T and RH.
    # This sampling is used for the ignition time forecasts.
    def getFirePeriod_analysisList(self):
        if self._forecastType in ["Tabular/Narrative", "Tabular Only"] or \
               self._withIgnitionTimes == "yes":
            analysisList = [
                ("Sky", self.median, [1]),            
                ("Wx", self.rankedWx, [1]),           
                ("PoP", self.stdDevMaxAvg, [1]),
                ("PoP", self.binnedPercent, [1]),
                ("LAL", self.maximum, [1]),               
                ("LAL", self.binnedPercent, [1]),
                ("MaxT", self.moderatedMinMax),
                ("MinT", self.moderatedMinMax),
                ("MaxRH", self.moderatedMinMax),
                ("MinRH", self.moderatedMinMax),
                ("RH", self.avg, [1]),
                ("RH", self.moderatedMinMax),
                ("MaxT", self.avg),   # for trends
                ("MinT", self.avg),   # for trends
                ("MaxRH", self.avg),  # for trends
                ("MinRH", self.avg),  # for trends
                ("RH", self.avg),     # for trends
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
                ("TransWind", self.vectorAvg, [1]),
                ("FreeWind", self.vectorAvg, [1]),
                ("MixHgt", self.moderatedMin, [1]),          
                ("VentRate", self.minMax, [1]),
                ("DSI", self.maximum,[1]),
                ("LDSI", self.maximum,[1]),
                ("LVORI", self.maximum,[1]),
                ("ADI", self.maximum,[1]),
                ("CWR", self.maximum, [1]),
                ("Stability", self.maximum, [1]),
                ("MarineLayer", self.maximum, [1]),
                ("Swell", self.vectorMinMax, [1]),
                ("Period", self.maximum, [1]),
                ("WindWaveHgt", self.maximum, [1]),
                ("WaveHeight", self.maximum, [1]),
                ("QPF", self.accumSum, [6]),
                ("SnowAmt", self.accumSum, [6]),
                ("FzLevel", self.median, [1]),
                ("Hazards", self.dominantDiscreteValue, [1]),
                ("Vsby", self.minimum, [1]),
                ("PredHgt", self.minimum, [1]),
                ("HeatIndex", self.maximum, [1]),
                ("ApparentT", self.maximum, [1]),
                ]
        else:
            analysisList = [
                ("Sky", self.median, [6]),            
                ("PoP", self.stdDevMaxAvg, [6]),
                ("PoP", self.binnedPercent, [6]),
                ("Wx", self.rankedWx, [6]),           
                ("LAL", self.maximum, [12]),               
                ("LAL", self.binnedPercent, [0]),
                ("MaxT", self.moderatedMinMax),
                ("MinT", self.moderatedMinMax),
                ("MaxRH", self.moderatedMinMax),
                ("MinRH", self.moderatedMinMax),
                ("RH", self.avg, [1]),
                ("RH", self.moderatedMinMax),
                ("MaxT", self.avg),   # for trends
                ("MinT", self.avg),   # for trends
                ("MaxRH", self.avg),  # for trends
                ("MinRH", self.avg),  # for trends
                ("RH", self.avg),     # for trends
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
                ("TransWind", self.vectorAvg, [12]),
                ("FreeWind", self.vectorAvg, [12]),
                ("MixHgt", self.moderatedMin, [1]),          
                ("VentRate", self.minMax),
                ("CWR", self.maximum),
                ("DSI", self.maximum,[12]),
                ("LDSI", self.maximum,[12]),
                ("LVORI", self.maximum,[12]),
                ("ADI", self.maximum,[12]),
                ("Stability", self.maximum),
                ("MarineLayer", self.maximum),
                ("Swell", self.vectorMinMax, [6]),
                ("Period", self.maximum, [6]),
                ("WindWaveHgt", self.maximum, [6]),
                ("WaveHeight", self.maximum, [6]),
                ("QPF", self.accumMinMax, [6]),
                ("SnowAmt", self.accumMinMax, [6]),
                ("FzLevel", self.median, [6]),
                ("Hazards", self.dominantDiscreteValue),
                ("Vsby", self.minimum, [6]),
                ("PredHgt", self.minimum, [6]),
                ("HeatIndex", self.maximum, [6]),
                ("ApparentT", self.maximum, [6]),
                ]
        return analysisList

    def getFirePeriod_intersectAreas(self, periodNum):
        return []  

    # From ConfigVariables.
  
    def phrase_descriptor_dict(self, tree, node):
        dict = TextRules.TextRules.phrase_descriptor_dict(self, tree, node)
        if self._wind20ftHeader:
            dict["WIND.(20 FT)........"]="WIND (20 FT)........"
            dict["20-FOOT WINDS......."]=" SLOPE/VALLEY......."
            dict["FREE WINDS.........."]=" RIDGETOP..........."
            dict["SURROUNDING RIDGE..."]=" SURROUNDING RIDGE.."
        else:
            dict["20-FOOT WINDS......."]="WIND (20 FT)........"
            dict["FREE WINDS.........."]="RIDGETOP WIND......."
            dict["SURROUNDING RIDGE..."]="SURROUNDING RIDGE..."
        dict["EYE LEVEL WINDS....."]="EYE LEVEL WINDS....."
        dict["SURFACE WINDS......."]="SURFACE WINDS......."
        dict["WIND SHIFT.........."]="WIND SHIFT.........."
        if self._transportWindLabel == "mix":
            dict["TRANSPORT WINDS....."]="MIXING WINDS........"
        else:
            dict["TRANSPORT WINDS....."]="TRANSPORT WINDS....."
        dict["CWR................."]="CWR................."
        dict["DSI................."]="DISPERSION.........."
        dict["LDSI................"]="DISPERSION INDEX...."
        dict["LVORI..............."]="LVORI..............."
        dict["ADI................."]="ADI................."
        dict["POP................."]="CHANCE OF PCPN......"
        dict["DEWPOINT............"]="DEWPOINT............"
        dict["BEGIN/END OF PCPN..."]="BEGIN/END OF PCPN..."
        dict["STABILITY CLASS....."]="STABILITY CLASS....."
        dict["WIND WAVE..........."]="WIND WAVE..........."
        dict["RAINFALL AMOUNT....."]="RAINFALL AMOUNT....."
        dict["SNOWFALL AMOUNT....."]="SNOWFALL AMOUNT....."
        dict["SWELL PERIOD........"]="SWELL PERIOD........"
        dict["SWELL HEIGHT........"]="SWELL HEIGHT........"
        dict["FREEZING LEVEL......"]="FREEZING LEVEL......"
        dict["CEILING............."]="CEILING............."
        dict["VISIBILITY.........."]="VISIBILITY.........."
        dict["ICING..............."]="ICING..............."
        dict["HEAT INDEX.........."]="HEAT INDEX.........."
        dict["erraticWind"]="gusty and erratic winds expected near thunderstorms"
        if self._withIgnitionTimes == "yes":
            dict["MinT_FireWx"]="TEMPERATURE........."
            dict["MaxT_FireWx"]="TEMPERATURE........."
            dict["MinRH_FireWx"]="RH.................."
            dict["MaxRH_FireWx"]="RH.................."
        return dict 
      
    # From FirePhrases. Changed to eliminate the area test. Thus,
    # this label will appear even though there is no ridgetop wind.
    def fireWind_label_setUp(self, tree, node):
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
        # Temp or RH elements
        elementName = node.getAncestor("elementName")
        statDict = node.getStatDict()        
        if elementName == "MaxT" or elementName == "MinT":
            stats = self.getTempStats(tree, node)
            if stats is None:
                return self.setWords(node.parent, "MISSING")
            connector = self.value_connector(tree, node, elementName, elementName)
            igWords =  `int(self.getValue(stats, "avg"))`
            words =  self.getTempRangePhrase(tree, node, stats, elementName)        
        else: # MinRH, MaxRH or RH
            stats = self.getStats(statDict, elementName)
            if stats is None:
                return self.setWords(node.parent, "MISSING")
            connector = self.value_connector(tree, node, elementName, elementName)
            igWords = `int(self.getValue(stats, "avg"))`                
            min, max = self.getValue(stats, "MinMax")
            if min > 100:
                min = 100
            if max > 100:
                max = 100
            if min == max:
                words = `int(min)`
            else:
                words = `int(min)` + connector + `int(max)`
        outUnits = self.element_outUnits(tree, node, elementName, elementName)
        units = self.units_descriptor(tree, node,"units", outUnits)
        words = words + units
        igWords = igWords + units
        
        # Add ignition element if applicable
        if self._withIgnitionTimes == "yes":
            dayNight = self.getPeriod(node.getTimeRange(), 1)
            if dayNight == self.DAYTIME():
                tempElement = "MAX"
                rhElement = "MIN"
            else:
                tempElement = "MIN"
                rhElement = "MAX"                
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
                    ignitionPhrase = `int(self.getValue(ignitionStats))`
                    reqType = self._getRequestWords()    
                    words = ignitionPhrase + units + " AT " + reqType + "..." + elementType + " " + igWords
                else:
                    words = elementType + " " + igWords
            else:
                words = elementType + " " + igWords
        return self.setWords(node, words)

    def fireWind_compoundPhrase(self):
        return {
            "phraseList": [
                self.wind_summary,
                #self.wind_phrase,
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
                igMagStr = `int(ignitionWindStats[0])`
                igDirStr = self.vector_dir(int(ignitionWindStats[1]))
                reqType = self._getRequestWords()
                igWords = "WINDS " + igDirStr + " AT " + igMagStr + " MPH AT " + reqType + "...OTHERWISE "
            
        words = igWords + words
        node.set("descriptor", "")
        node.set("indentLabel", "20-FOOT WINDS.......")
        node.set("compound", 1)
        return self.setWords(node, words)
    
    def fireSfcWind_compoundPhrase(self):
        return {
            "phraseList": [
                self.wind_summary,
                self.wind_phrase,
                ],
            "phraseMethods": [
                self.consolidateSubPhrases,
                self.assembleSentences,
                self.fireSfcWind_finishUp
                ],
            }
    
    def fireSfcWind_finishUp(self, tree, node):
        "Create a phrase for Winds"
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

    def erraticWind_phrase(self):
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
        elementInfoList = [self.ElementInfo("Wx", "List", self.WEATHER())]
        self.subPhraseSetUp(tree, node, elementInfoList, self.wxConnector)
        # Set this flag used by the "checkWeatherSimilarity" method
        node.set("noIntensityCombining", 1)
        self.determineSevereTimeDescriptors(tree, node)
        return self.DONE()
    
    def erraticWind_words(self, tree, node):
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
                    attrText = wxDef.attributeDesc(subkey.wxType(), attr).lower()
                    if attrText not in attrTextList:
                        attrTextList.append(attrText)
                        
        if thunder == 0:
            return self.setWords(node, "")
        words = self.phrase_descriptor(tree, node, "erraticWind", "Wx")
                       
        return self.setWords(node, words)
    
    def smokeDispersal_words(self, tree, node):
        "Create phrase for Smoke Dispersal"
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "VentRate")
        if stats is None:
           return self.setWords(node.parent, "MISSING")

        if self._checkFireTR(node.getTimeRange()):
            # Handle phrase if including ignition time
            minVal, maxVal = self.getValue(stats, "MinMax")
            dayNight = self.getPeriod(node.getTimeRange(), 1)
            if dayNight == self.DAYTIME():
                vr = int(maxVal)
                ventType = "MAX"
                mergeMethod = "Max"
            else:
                vr = int(minVal)
                ventType = "MIN"
                mergeMethod = "Min"
            vrCat = self.smokeDispersal_valueStr(vr)
            words =  ventType + "..." + vrCat + " " + " /" + `vr` + " knot-ft/"
            reqType = self._getRequestWords()                
            ignitionDispersal = tree.stats.get(
                "VentRate", self._fireTR, node.getAreaLabel(), mergeMethod=mergeMethod)
            vrCat = self.smokeDispersal_valueStr(ignitionDispersal)
            igWords =  vrCat + " /" + `int(ignitionDispersal)` + " KNOT-FT/ AT " + reqType + ". \n"
            words = igWords + " " + words
        else:
            # Handle phrase with range if not including ignition time
            vr1, vr2 = self.getValue(stats, "MinMax")
            vr1 = int(vr1)
            vr2 = int(vr2)
            vrCat1 = self.smokeDispersal_valueStr(vr1)
            vrCat2 = self.smokeDispersal_valueStr(vr2)
            # Single Value input
            if  vr1 == vr2:
                words =  vrCat1 + " (" + `vr1` + " knot-ft)"
            # Range
            else:
                words =  vrCat1 + " to " + vrCat2 + " (" + `vr1` + "-" + \
                   `vr2` + " knot-ft)"            
        return self.setWords(node, words)
    
    #  SMOKE DISPERSAL CATEGORIES
    def smokeDispersal_valueStr(self, value):
        "Convert smoke dispersal value to corresponding category"

        if value < 13000 :
            return "poor"

        if value >= 13000 and value < 30000:
            return "fair"

        if value >= 30000 and value < 60000 :
            return "good"

        if value >= 60000 :
            return "excellent"

    ### MixHgt
    def mixingHgt_words(self, tree, node):
        "Create phrase for Mixing Height"

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
        if  mix1 == mix2:
                words =  `mix1` + " " + outUnits + " agl"
        # Range
        else:
            words =  `mix1`+ "-" + `mix2` + " " + outUnits + " agl"

        # Handle ignition time
        if self._checkFireTR(node.getTimeRange()):
            reqType = self._getRequestWords()
            ignitionMixStats = tree.stats.get(
                "MixHgt", self._fireTR, node.getAreaLabel(), mergeMethod="Max")
            igWords =  `int(ignitionMixStats)` + " " + outUnits + " agl at " + reqType +"...otherwise "
            words = igWords + words
        
        return self.setWords(node, words)

    def adi_phrase(self):
        return {
            "setUpMethod": self.adi_setUp,
            "wordMethod": self.adi_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }
    
    def adi_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("ADI", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "ADI.................")
        return self.DONE()      

    def adi_words(self, tree, node):
        statDict = node.getStatDict()
        adi = self.getStats(statDict, "ADI")
        if adi is None:
            return self.setWords(node.parent, "MISSING")
        adi = self.getValue(adi)
        words =  `int(adi + 0.5)` 
        return self.setWords(node, words)
            
    def haines_words(self, tree, node):
        "Create phrase for Haines Index"
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
                ignitionPhrase = `int(self.getValue(ignitionStats))`
                #print "Haines ignitionStats", ignitionStats
                reqType = self._getRequestWords()
                hainesDict = self.hainesDict()
                words = ignitionPhrase + " " + hainesDict[int(ignitionPhrase)] + \
                    " AT " + reqType + "...MAX " + `haines1`
                ignitionFlag = 1
        if not ignitionFlag:
            haines1, haines2 = self.getValue(stats, "MinMax")
            hainesDict = self.hainesDict()
            haines1 = int(haines1)
            haines2 = int(haines2)
            words1 = hainesDict[haines1]
            words2 = hainesDict[haines2]

            # Single Value input
            if  haines1 == haines2:
                words = `haines1` + "   " + words1
            # Range
            else:
                if words1 == words2:
                    words = words1
                else:
                    words = words1 + " to " + words2
                words =  `haines1` + " to " + `haines2` + " OR " + words           
        return self.setWords(node, words)
        
    def cwr_words(self, tree, node):
        # Handle ignition time
        if self._checkFireTR(node.getTimeRange()):
            cwr = tree.stats.get(self._cwrParm, self._fireTR, node.getAreaLabel(), mergeMethod="Max")
        else:
            cwr = tree.stats.get(self._cwrParm, node.getTimeRange(), node.getAreaLabel(), mergeMethod="Max")
        if cwr is None:
            return self.setWords(node.parent, "MISSING")
        cwr = self.getValue(cwr)
        threshold = self.nlValue(self.null_nlValue(tree, node, "CWR", "CWR"), cwr)
        if int(cwr) < threshold:
            return self.setWords(node, "null")
        else:
            words =  `int(cwr)` + " percent"
        return self.setWords(node, words)

    def windWave_phrase(self):
        return {
            "setUpMethod": self.windWave_setUp,
            "wordMethod": self.windWave_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }
    
    def windWave_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("WindWaveHgt", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "WIND WAVE...........")
        return self.DONE()
    
    def windWave_words(self, tree, node):
        "Create phrase Wind Wave"
        statDict = node.getStatDict()
        height = self.getValue(self.getStats(statDict, "WindWaveHgt"), "Max")
        if height is None:
            return self.setWords(node.parent, "MISSING")
        words =  `int(height + 0.5)` + " FEET"
        return self.setWords(node, words)

    def waveHeight_phrase(self):
        return {
            "setUpMethod": self.waveHeight_setUp,
            "wordMethod": self.waveHeight_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }
    
    def waveHeight_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("WaveHeight", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "WAVE HEIGHT.........")
        return self.DONE()
    
    def waveHeight_words(self, tree, node):
        "Create phrase Wind Wave"
        statDict = node.getStatDict()
        height = self.getValue(self.getStats(statDict, "WaveHeight"), "Max")
        if height is None:
            return self.setWords(node.parent, "MISSING")
        words =  `int(height + 0.5)` + " FEET"
        return self.setWords(node, words)
    
    def qpf_phrase(self):
        return {
            "setUpMethod": self.qpf_setUp,
            "wordMethod": self.qpf_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }
    
    def qpf_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("QPF", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "RAINFALL AMOUNT.....")
        return self.DONE()
    
    def qpf_words(self, tree, node):
        "Create phrase QPF"
        statDict = node.getStatDict()
        qpf = self.getValue(self.getStats(statDict, "QPF"), "Max")
        if qpf is None:
            return self.setWords(node.parent, "MISSING")
        if qpf == 0.0:
            qpfWords = "0.00"
        else:
            qpf = qpf + 0.005
            qpfWords = string.strip("%5.2f" % qpf)
        words = qpfWords + " INCHES"
        return self.setWords(node, words)
    
    def period_phrase(self):
        return {
            "setUpMethod": self.period_setUp,
            "wordMethod": self.period_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }
    
    def period_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("Period", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "SWELL PERIOD........")
        return self.DONE()
    
    def period_words(self, tree, node):
        "Create phrase Swell Period"
        statDict = node.getStatDict()
        period = self.getValue(self.getStats(statDict, "Period"), "Max")
        if period is None:
            return self.setWords(node.parent, "MISSING")
        words =  `int(period + 0.5)` + " SECONDS"
        return self.setWords(node, words)
    
    def swell_phrase(self):
        return {
            "setUpMethod": self.swell_setUp,
            "wordMethod": self.swell_words,
            "phraseMethods": self.standard_vector_phraseMethods(), 
            }
    
    def swell_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("Swell", self.VECTOR())]
        self.subPhraseSetUp(tree, node, elementInfoList, self.vectorConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "SWELL HEIGHT........")
        return self.DONE()
    
    def swell_words(self, tree, node):
        "Create phrase Swell Height"
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "Swell")
        if stats is None:
            return self.setWords(node, "")
        height, dir = self.getValue(stats, "Max", self.VECTOR())
        if height is None:
            return self.setWords(node.parent, "MISSING")
        if dir >= 22.5 and dir < 67.5:
            dirWords = "NORTHEAST"
        elif dir >= 67.5 and dir < 112.5:
            dirWords = "EAST"
        elif dir >= 112.5 and dir < 157.5:
            dirWords = "SOUTHEAST"
        elif dir >= 157.5 and dir < 202.5:
            dirWords = "SOUTH"
        elif dir >= 202.5 and dir < 247.5:
            dirWords = "SOUTHWEST"
        elif dir >= 247.5 and dir < 292.5:
            dirWords = "WEST"
        elif dir >= 292.5 and dir < 337.5:
            dirWords = "NORTHWEST"
        else:
            dirWords = "NORTH"
        heightWords = `int(height + 0.5)`
        words =  dirWords + " SWELL " + heightWords + " FEET"
        return self.setWords(node, words)
    
    def ceiling_phrase(self):
        return {
            "setUpMethod": self.ceiling_setUp,
            "wordMethod": self.ceiling_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }
    
    def ceiling_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("PredHgt", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "CEILING (KFT).......")
        return self.DONE()
    
    def ceiling_words(self, tree, node):
        "Create phrase Visibility"
        statDict = node.getStatDict()
        hgt = self.getValue(self.getStats(statDict, "PredHgt"), "Min")
        if hgt is None:
            return self.setWords(node.parent, "MISSING")
        hgt = hgt / 10.0
        if hgt == 0.0:
            hgtWords = "less than 0.1"
        else:
            if hgt < 10:
                hgtWords = string.strip("%5.1f" % hgt)
            else:
                hgtWords = `int(hgt + 0.5)`
        words = hgtWords
        return self.setWords(node, words)
    
    def visibility_phrase(self):
        return {
            "setUpMethod": self.visibility_setUp,
            "wordMethod": self.visibility_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }
    
    def visibility_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("Vsby", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "VISIBILITY (SM).....")
        return self.DONE()
    
    def visibility_words(self, tree, node):
        "Create phrase Visibility"
        statDict = node.getStatDict()
        vis = self.getValue(self.getStats(statDict, "Vsby"), "Min")
        if vis is None:
            return self.setWords(node.parent, "MISSING")
        if vis == 0.0:
            visWords = "0.0"
        else:
            if vis < 3:
                visWords = string.strip("%5.2f" % vis)
            else:
                visWords = `int(vis + 0.5)`
        words = visWords
        return self.setWords(node, words)
    
    def icing_phrase(self):
        return {
            "setUpMethod": self.icing_setUp,
            "phraseMethods": [self.postProcessPhrase],
            }
    
    def icing_setUp(self, tree, node):
        self.setWords(node, "")
        node.set("descriptor", "")
        node.set("indentLabel", "ICING...............")
        return self.DONE()
    
    def freezingLevel_phrase(self):
        return {
            "setUpMethod": self.freezingLevel_setUp,
            "wordMethod": self.freezingLevel_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }
    
    def freezingLevel_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("FzLevel", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "FREEZING LEVEL......")
        return self.DONE()
    
    def freezingLevel_words(self, tree, node):
        "Create phrase for Freezing Level"

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
        if  mix1 == mix2:
                words =  `mix1` + " " + outUnits
        # Range
        else:
            words =  `mix1`+ "-" + `mix2` + " " + outUnits

        # Handle ignition time
        if self._checkFireTR(node.getTimeRange()):
            reqType = self._getRequestWords()
            ignitionMixStats = tree.stats.get(
                "FzLevel", self._fireTR, node.getAreaLabel(), mergeMethod="Max")
            igWords =  `int(ignitionMixStats)` + " " + outUnits + " at " + reqType +"...otherwise "
            words = igWords + words
        
        return self.setWords(node, words)
    
    def snow_phrase(self):
        return {
            "setUpMethod": self.snow_setUp,
            "wordMethod": self.snow_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }

    def snow_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("SnowAmt", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "SNOWFALL AMOUNT.....")
        return self.DONE()
    
    def snow_words(self, tree, node):
        "Create phrase Snow"
        statDict = node.getStatDict()
        snow = self.getValue(self.getStats(statDict, "SnowAmt"), "Max")
        if snow is None:
            return self.setWords(node.parent, "MISSING")
        if snow == 0.0:
            snowWords = "0.0"
        else:
            snow = snow + 0.05
            snowWords = string.strip("%5.1f" % snow)
        words = snowWords + " INCHES"
        return self.setWords(node, words)
    
    def heatIndex_phrase(self):
        return {
            "setUpMethod": self.heatIndex_setUp,
            "wordMethod": self.heatIndex_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }
    
    def heatIndex_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("HeatIndex", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "HEAT INDEX..........")
        return self.DONE()
    
    def heatIndex_words(self, tree, node):
        "Create phrase Td"
        statDict = node.getStatDict()
        hi = self.getValue(self.getStats(statDict, "HeatIndex"), "Max")
        if hi is None:
            return self.setWords(node.parent, "MISSING")
        words =  `int(hi)`
        return self.setWords(node, words)
    
    ### Methods for Spot Table ###

    def _fwsTable_phrase(self):
        return {
            "setUpMethod": self._fwsTable_setUp,
            "wordMethod": self._fwsTable_words,
            "phraseMethods": [
                self.assembleChildWords,
                ], 
            }

    def _fwsTable_setUp(self, tree, node):
        elementInfoList = []
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        return self.DONE()        

    def _fwsTable_words(self, tree, node):
        # See if we're doing a table for this time period
        tableVars = self._determineTableVars(tree, node)
        if tableVars is None:
            return self.setWords(node, "")
        timeRangeList, statList, colWidth, header, elements = tableVars
        #print "header", header
        #print "colWidth", colWidth
        #print "timeRangeList"
        #for tr, label in timeRangeList:
        #    print tr, label
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
        # Make timeRangeList, empty statList, colWidth, and header
        
        # Get table resolution (period) based on today, tonight, tomorrow
        componentName = node.getComponentName()
        period = None
        
        if self._productIssuance in ["Afternoon with 4 periods",
                                     "Evening Update with 4 periods",
                                     "Early Morning Update with 4 periods"]:
            tablePeriodList = [
                ("FirePeriod1", self._todayTableRes, self._todayElements),
                ("FirePeriod2", self._tonightTableRes, self._tonightElements),
                ("FirePeriod3", self._tomorrowTableRes, self._tomorrowElements),
                ("FirePeriod4", self._tomorrowNightTableRes, self._tomorrowNightElements),
                ("FirePeriod5", self._nextDayTableRes, self._nextDayElements),
                ]
        else:
            tablePeriodList = [
                ("FirePeriod1", self._todayTableRes, self._todayElements),
                ("FirePeriod2", self._tonightTableRes, self._tonightElements),
                ("FirePeriod3", self._tomorrowTableRes, self._tomorrowElements),
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
            
        # Determine Time Ranges over which to create table
        fireTimeZone = self._getActualTimeZone()
        timeRange = self._determineTableTimeRange(tree, node, fireTimeZone)
        timeRangeList = self.getPeriods(timeRange, period, 1, None)
        
        # Make header
        header = "TIME ("+fireTimeZone+")      "
        for tr, label in timeRangeList:
            label = self._makeTableLabel(tree, tr, colWidth)
            header += string.ljust(label, colWidth)
        header += "\n"
        
        # Make empty statList (dummy for calling "makeRow")
        statList = []
        for i in range(len(timeRangeList)):
            statList.append({})
        return timeRangeList, statList, colWidth, header, tableElements   

    def _colWidths(self):
        # Lists table resolutions hours, corresponding column width
        return [
            (1, 4),
            (2, 7),
            (3, 10),
            (4, 13),
            ]

    def _determineTableTimeRange(self, tree, node, fireTimeZone):
        tr = node.getTimeRange()
        # See if this is first period of product
        prev = node.getComponent().getPrev()
        if prev is None:
            # Adjust timeRange if necessary
            if self._tableStartTimeMode == "current":
                currentTime = tree.get('argDict').get('creationTime')
                currentTime = int(currentTime/3600.0)*3600.0
                tr = self.makeTimeRange(currentTime, tr.endTime().unixTime())
            elif self._tableStartTimeMode == "ignitionTime":
                fireDateTime = time.mktime(self._fireDateTime)
                fireDateTime = int(fireDateTime/3600.0)*3600.0
                fireTime = fireDateTime - (self._tableStartTimeOffset * 3600)
                if fireTime >= tr.startTime().unixTime() and \
                       fireTime < tr.endTime().unixTime():
                    tr = self.makeTimeRange(fireTime, tr.endTime().unixTime())
        if self._tabularAllPeriods == "yes":
            timeRange = tr
        else:
            # One 12-hour period
            timeRange = self.makeTimeRange(tr.startTime(),
                                           tr.startTime()+12*3600)
        #print "Table time range", timeRange, node.getTimeRange()
        return timeRange

    def _makeTableLabel(self, tree, timeRange, colWidth):
        localTime, shift = self.determineTimeShift()
        rtz = self._getActualTimeZone()
        stz = time.tzname[0]
        dtz = time.tzname[1]
        otz = stz[0:1]
        ptz = rtz[0:1]
        if otz == ptz:
            start = timeRange.startTime() + shift
        else:
            offset = 0
            if ptz == "E":
                if otz == "E":
                    offset = 0
                elif otz == "C":
                    offset = 1
                elif otz == "M":
                    offset = 2
                elif otz == "P":
                    offset = 3
            elif ptz == "C":
                if otz == "E":
                    offset = -1
                elif otz == "C":
                    offset = 0
                elif otz == "M":
                    offset = 1
                elif otz == "P":
                    offset = 2
            elif ptz == "M":
                if otz == "E":
                    offset = -2
                elif otz == "C":
                    offset = -1
                elif otz == "M":
                    offset = 0
                elif otz == "P":
                    offset = 1
            elif ptz == "P":
                if otz == "E":
                    offset = -3
                elif otz == "C":
                    offset = -2
                elif otz == "M":
                    offset = -1
                elif otz == "P":
                    offset = 0
            if stz[1:3] == rtz[1:3]:
                start = timeRange.startTime() + shift + offset*3600
            else:
                start = timeRange.startTime() + shift + offset*3600
        militaryHour = start.hour
        hour, ampm = self.hourAmPm(militaryHour)
        for low, hi, shortVal, longVal in self._tableLabels():
            if militaryHour >= low and militaryHour <= hi:
                if colWidth > 4:
                    val = longVal
                else:
                    val = shortVal
                val = val.replace("hour", `hour`)
                break
        return val

    def _tableLabels(self):
        return [
            (0,0, "MID", "MIDNGT"),
            (1,9, "hourAM", "hour AM"),
            (10,11, "hourA", "hour AM"),
            (12,12, "12P", "NOON"),
            (13,21, "hourPM", "hour PM"),
            (22,23, "hourP", "hour PM"),
            ]
    
    def assembleIndentedPhrases(self, tree, component):
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
                    fcst = fcst +  "\n" + words 
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
            #print "indentLabel, label", indentLabel, label
            if indentLabel is not None and label == "":
                label = indentLabel
            if words == "":
                words = " "
            words = self.labelIndent(words, label)
            fcst = fcst + words
        if fcst == "":
            return self.setWords(component,"")
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
        stats = tree.stats.get(element, tr, area, mergeMethod=mergeMethod)
        if stats is None:
            return None
        return self.getValue(stats, getValueMethod)
    
    def _sky_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)
        sky = self._getTableStats(tree, "Sky", timeRange, node.getAreaLabel())
        if sky is None:
            value = "M"
        elif self._elementFormatDict.get("Sky", "numeric") == "numeric":
            value = `int(sky + 0.5)`
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
        tree, node, colWidth = tuple(argList)
        sky = self._getTableStats(tree, "Sky", timeRange, node.getAreaLabel())
        if sky is None:
            value = "M"
        else:
            value = `int(sky + 0.5)`
        return value

    def _skyTableValues(self):
        return [
            (5, "CLR", "CLEAR"),
            (25,"MCR", "MCLEAR"),
            (50,"PC",  "PCLDY"),
            (69,"MC",  "MCLDY"),
            (87,"MC",  "MCLDY"),
            (100,"CDY", "CLOUDY"),
            ]
    
    def _weatherType_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        areaLabel = node.getAreaLabel()
        wxStats = tree.stats.get("Wx", timeRange, areaLabel, mergeMethod="Max")
        #print "wxStats = ", wxStats,tr
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
        return [
            ("",    "<NoWx>", "",   "NONE"  ),
            ("Dry", "T",      "DYT","DRYTSM"),
            ("",    "T",      "TSM","TSTORM"),
            ("GW",  "T",      "TSM","TSTORM"),
            ("SmA", "T",      "TSM","TSTORM"),
            ("",    "S",      "SN", "SNOW"  ),
            ("",    "R",      "RN", "RAIN"  ),
            ("",    "SW",     "SW", "SNSHWR"),
            ("",    "RW",     "RW", "RNSHWR"),
            ("",    "L",      "DZL","DRZL"  ),
            ("",    "ZR",     "FZR","FZRAIN"),
            ("",    "ZL",     "FZD","FZDRZL"),
            ("",    "IP",     "SLT","SLEET" ),
            ("",    "F",      "FOG","FOG"   ),
            ("",    "ZF",     "FZF","FZFOG" ),
            ("",    "IF",     "IFG","ICEFOG"),
            ("",    "IC",     "ICR","ICECRL"),
            ("",    "H",      "HAZ","HAZE"  ),
            ("",    "BS",     "BSN","BLSNOW"),
            ("",    "BN",     "BSD","BLSAND"),
            ("",    "BD",     "BDT","BLDUST"),
            ("",    "K",      "SMK","SMOKE" ),
            ("",    "FR",     "FST","FROST" ),
            ("",    "ZY",     "FZS","FZSPRY"),
            ("",    "VA",     "ASH","VOLASH"),
           # Mixed Weather Types
            ("",    "RS",     "RS", "RNSN"  ),
            ("",    "LF",     "DZF","DZL/FG"),
            ("",    "SF",     "SNF","SN/FG "),
            ("",    "RF",     "RNF","RN/FG "),
            ("",    "ZRS",    "ZRS","ZRN/SN"),
           # Unknown Mixed Weather Type
            ("",    "XX",     "???","??????"),
            ]

    def _weatherMixTable(self):
        return [
            ("",   "S", "","R", "",   "RS"),
            ("",   "SW","","RW","",   "RS"),
            ("",   "RW","","T", "",   "T"),
            ("Dry","T", "","RW","Dry","T"),
            ("",   "L", "","F", "",   "LF"),
            ("",   "S", "","F", "",   "SF"),
            ("",   "R", "","F", "",   "RF"),
            ("",   "SW","","F", "",   "SF"),
            ("",   "RW","","F", "",   "RF"),
            ("",   "ZR","","S", "",   "ZRS"),
            ("",   "ZR","","SW","",   "ZRS"),
            ]            

    def _tstmCov_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)       
        areaLabel = node.getAreaLabel()
        wxStats = tree.stats.get("Wx", timeRange, areaLabel, mergeMethod="Max")
        if wxStats is None or len(wxStats) == 0:
            return "M"
        hiRank = -1
        for subkey, rank in wxStats:
            print "*** vtm ***"
            print subkey, rank
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
        return [
            ("<NoCov>","",   ""),
            ("SChc",   "SCH","S CHC"),
            ("Iso",    "ISO","ISOLTD"),
            ("Chc",    "CHC","CHANCE"),
            ("Sct",    "SCT","SCTTRD"),
            ("Lkly",   "LKY","LIKELY"),
            ("Num",    "NUM","NUMRUS"),
            ("Def",    "DEF","DEFNTE"),
            ("Wide",   "WID","WIDSPD"),
            ("Ocnl",   "OCL","OCNL"),
            ("Frq",    "FRQ","FRQNT"),
            ("Brf",    "BRF","BRIEF"),
            ("Pds",    "PDS","PERIOD"),
            ("Inter",  "ITR","ITRMT"),
            ("Areas",  "ARS","AREAS"),
            ("Patchy", "PTY","PATCHY")
            ]

    def _temp_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        temp = self._getTableStats(tree, "T", timeRange, node.getAreaLabel())
        if temp is None:
            return "M"
        if temp >= 0:
            temp = int(temp + 0.5)
        else:
            temp = int(temp - 0.5)
        return `temp`
     
    def _rh_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)
        rh =self._getTableStats(tree, "RH", timeRange, node.getAreaLabel())
        if rh is None:
            return "M"
        rh = int(rh + 0.5)
        return `rh`

    # Wind Methods
    # Utility for Wind Methods

    # Tabular Transport Wind bug found by John DeBlock and Stephen Miller.
    # tree.stats.get was using self._20ftWindParm instead of element.
    def _getWindDirSpdStr(self, tree, node, timeRange, element, formatElement=None, units=None):
        windStats = tree.stats.get(element, timeRange, node.getAreaLabel(),
                                   mergeMethod="Max")
        if windStats is None:
            return None
        wspd,wdir = windStats
        if formatElement is None:
            formatElement = element
        if self._elementFormatDict.get(formatElement, "alpha") == "alpha":
            wdir = int(wdir + 0.5)
            dirString = self._dirConvert(wdir)
        else:
            dir = int(wdir/10.0 + 0.5) * 10
            if dir < 10:
                dirString = "00" + `dir`
            elif dir < 100:
                dirString = "0" + `dir`
            else:
                dirString = `dir`
        if element == "Wind":
            wspd = wspd * self._windAdjustmentFactor
        if units == "Metric":
            wspd = int(wspd*.44704 + 0.5)
        else:
            wspd = int(wspd + 0.5)
        spdString = `wspd`
        return dirString, spdString

    def _getWindNumDirSpdStr(self, tree, node, timeRange, element, formatElement=None, units=None):
        windStats = tree.stats.get(element, timeRange, node.getAreaLabel(),
                                   mergeMethod="Max")
        if windStats is None:
            return None
        wspd,wdir = windStats
        if formatElement is None:
            formatElement = element
        dir = int(wdir/10.0 + 0.5) * 10
        if dir < 10:
            dirString = "00" + `dir`
        elif dir < 100:
            dirString = "0" + `dir`
        else:
            dirString = `dir`
        if element == "Wind":
            wspd = wspd * self._windAdjustmentFactor
        if units == "Metric":
            wspd = int(wspd*.44704 + 0.5)
        else:
            wspd = int(wspd + 0.5)
        spdString = `wspd`
        return dirString, spdString

    def _getEyeWindDirSpdStr(self, tree, node, timeRange, element, formatElement=None, units=None):
        windStats = tree.stats.get(element, timeRange, node.getAreaLabel(),
                                   mergeMethod="Max")
        if windStats is None:
            return None
        wspd,wdir = windStats
        if formatElement is None:
            formatElement = element
        if self._elementFormatDict.get(formatElement, "alpha") == "alpha":
            wdir = int(wdir + 0.5)
            dirString = self._dirConvert(wdir)
        else:
            dir = int(wdir/10.0 + 0.5) * 10
            if dir < 10:
                dirString = "00" + `dir`
            elif dir < 100:
                dirString = "0" + `dir`
            else:
                dirString = `dir`
        if element == "Wind":
            wspd = wspd * self._eyeWindAdjustmentFactor
        if units == "Metric":
            wspd = int(wspd*.44704 + 0.5)
        else:
            wspd = int(wspd + 0.5)
        spdString = `wspd`
        return dirString, spdString

    def _getSfcWindDirSpdStr(self, tree, node, timeRange, element, formatElement=None, units=None):
        windStats = tree.stats.get(element, timeRange, node.getAreaLabel(),
                                   mergeMethod="Max")
        if windStats is None:
            return None
        wspd,wdir = windStats
        if formatElement is None:
            formatElement = element
        if self._elementFormatDict.get(formatElement, "alpha") == "alpha":
            wdir = int(wdir + 0.5)
            dirString = self._dirConvert(wdir)
        else:
            dir = int(wdir/10.0 + 0.5) * 10
            if dir < 10:
                dirString = "00" + `dir`
            elif dir < 100:
                dirString = "0" + `dir`
            else:
                dirString = `dir`
        if units == "Metric":
            wspd = int(wspd*.44704 + 0.5)
        else:
            wspd = int(wspd + 0.5)
        spdString = `wspd`
        return dirString, spdString

    def _dirConvert(self, wdir):
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
        # adjustment for winds
        factor = self.nlValue(self._eyeWindAdjustmentFactor, value)
        value = value * factor
        return value
    
    def _wind_value(self, statDict, timeRange, argList, element=None, formatElement=None):
        if element is None:
            element = self._20ftWindParm
        if formatElement is None:
            formatElement = self._20ftWindParm
        tree, node, colWidth = tuple(argList)
        if formatElement == "TransMetWind":
            windString = self._getWindDirSpdStr(tree, node, timeRange, element, formatElement, "Metric")
        elif formatElement == "EyeWind":
            windString = self._getEyeWindDirSpdStr(tree, node, timeRange, element, formatElement)
        elif formatElement == "SfcWind":
            windString = self._getSfcWindDirSpdStr(tree, node, timeRange, element, formatElement)
        else:
            windString = self._getWindDirSpdStr(tree, node, timeRange, element, formatElement)
        if windString is None:
            return "M"
        dirString, spdString = windString
        if self._elementFormatDict.get(formatElement, "alpha") == "alpha":
            value = dirString + " " + spdString
        else:
            value = dirString + "/" + spdString                
        return value

    def _windWithGust_value(self, statDict, timeRange, argList, element=None, formatElement=None):
        if element is None:
            element = self._20ftWindParm
        if formatElement is None:
            formatElement = self._20ftWindParm
        tree, node, colWidth = tuple(argList)
        if formatElement == "EyeWind":
            windString = self._getEyeWindDirSpdStr(tree, node, timeRange, element, formatElement)
        elif formatElement == "SfcWind":
            windString = self._getSfcWindDirSpdStr(tree, node, timeRange, element, formatElement)
        else:
            windString = self._getWindDirSpdStr(tree, node, timeRange, element, formatElement)
        if windString is None:
            return "M"
        dirString, spdString = windString
        gust = self._getTableStats(tree, "WindGust", timeRange, node.getAreaLabel(),
                                         getValueMethod="Max")
        if gust is None:
            gstString = "GMM"
        gstString = ""
        gust = int(self.getValue(gust) + 0.5)
        if gust > string.atoi(spdString):
            gstString = "G" + `gust`
        if self._elementFormatDict.get(formatElement, "alpha") == "alpha":
            value = dirString + " " + spdString + gstString
        else:
            value = dirString + "/" + spdString + gstString
        return value

    def _windDir_value(self, statDict, timeRange, argList, element=None, formatElement=None):
        if element is None:
            element = self._20ftWindParm
        if formatElement is None:
            formatElement = self._20ftWindParm
        tree, node, colWidth = tuple(argList)   
        windString = self._getWindDirSpdStr(tree, node, timeRange, element, formatElement)
        if windString is None:
            return "M"
        dirString, spdString = windString    
        return dirString

    def _windNumDir_value(self, statDict, timeRange, argList, element=None, formatElement=None):
        if element is None:
            element = self._20ftWindParm
        if formatElement is None:
            formatElement = self._20ftWindParm
        tree, node, colWidth = tuple(argList)   
        windString = self._getWindNumDirSpdStr(tree, node, timeRange, element, formatElement)
        if windString is None:
            return "M"
        dirString, spdString = windString    
        return dirString
    
    def _eyewindNumDir_value(self, statDict, timeRange, argList):
        return self._windNumDir_value(statDict, timeRange, argList, "Wind", "EyeWind")

    def _sfcwind_value(self, statDict, timeRange, argList):
        return self._wind_value(statDict, timeRange, argList, "Wind", "SfcWind")

    def _sfcwindWithGust_value(self, statDict, timeRange, argList):
        return self._windWithGust_value(statDict, timeRange, argList, "Wind", "SfcWind")

    def _sfcwindDir_value(self, statDict, timeRange, argList):
        return self._windDir_value(statDict, timeRange, argList, "Wind", "SfcWind")

    def _sfcwindSpd_value(self, statDict, timeRange, argList):
        return self._windSpd_value(statDict, timeRange, argList, "Wind", "SfcWind")

    def _sfcwindGust_value(self, statDict, timeRange, argList):
        return self._windGust_value(statDict, timeRange, argList, "Wind", "SfcWind")

    def _sfcwindNumDir_value(self, statDict, timeRange, argList):
        return self._windNumDir_value(statDict, timeRange, argList, "Wind", "SfcWind")

    def _ridgeNumDir_value(self, statDict, timeRange, argList):
        return self._windNumDir_value(statDict, timeRange, argList, "FreeWind", "RidgeWind")

    def _transNumDir_value(self, statDict, timeRange, argList):
        return self._windNumDir_value(statDict, timeRange, argList, "TransWind", "TransWind")

    def _windSpd_value(self, statDict, timeRange, argList, element=None, formatElement=None):
        if element is None:
            element = self._20ftWindParm
        if formatElement is None:
            formatElement = self._20ftWindParm
        tree, node, colWidth = tuple(argList)
        if formatElement == "TransMetWind":
            windString = self._getWindDirSpdStr(tree, node, timeRange, element, formatElement, "Metric")
        elif formatElement == "EyeWind":
            windString = self._getEyeWindDirSpdStr(tree, node, timeRange, element, formatElement)
        elif formatElement == "SfcWind":
            windString = self._getSfcWindDirSpdStr(tree, node, timeRange, element, formatElement)
        else:
            windString = self._getWindDirSpdStr(tree, node, timeRange, element, formatElement)
        if windString is None:
            return "M"
        dirString, spdString = windString    
        return spdString

    def _windGust_value(self, statDict, timeRange, argList, element=None, formatElement=None ):
        if element is None:
            element = self._20ftWindParm
        if formatElement is None:
            formatElement = self._20ftWindParm
        tree, node, colWidth = tuple(argList)
        windString = self._getWindDirSpdStr(tree, node, timeRange, element, formatElement)
        if windString is None:
            spdString = '0'
        else:
            dirString, spdString = windString
        gust = self._getTableStats(tree, "WindGust", timeRange, node.getAreaLabel(),
                                         getValueMethod="Max")
        if gust is None:
            return "M"
        gstString = " "
        gust = int(gust + 0.5)
        if gust > string.atoi(spdString):
            gstString = `gust`
        return gstString        

    def _eyewind_value(self, statDict, timeRange, argList):
        return self._wind_value(statDict, timeRange, argList, "Wind", "EyeWind")

    def _eyewindWithGust_value(self, statDict, timeRange, argList):
        return self._windWithGust_value(statDict, timeRange, argList, "Wind", "EyeWind")

    def _eyewindDir_value(self, statDict, timeRange, argList):
        return self._windDir_value(statDict, timeRange, argList, "Wind", "EyeWind")
        
    def _eyewindSpd_value(self, statDict, timeRange, argList):
        return self._windSpd_value(statDict, timeRange, argList, "Wind", "EyeWind")
        
    def _eyewindGust_value(self, statDict, timeRange, argList):
        return self._windGust_value(statDict, timeRange, argList, "Wind", "EyeWind")

    def _ridge_value(self, statDict, timeRange, argList):
        return self._wind_value(statDict, timeRange, argList,"FreeWind", "RidgeWind" )

    def _ridgeDir_value(self, statDict, timeRange, argList):
        return self._windDir_value(statDict, timeRange, argList, "FreeWind", "RidgeWind")

    def _ridgeSpd_value(self,  statDict, timeRange, argList):
        return self._windSpd_value(statDict, timeRange, argList, "FreeWind", "RidgeWind")

    def _trans_value(self, statDict, timeRange, argList):
        return self._wind_value(statDict, timeRange, argList, "TransWind", "TransWind")

    def _transDir_value(self, statDict, timeRange, argList):
        return self._windDir_value(statDict, timeRange, argList, "TransWind", "TransWind")

    def _transSpd_value(self, statDict, timeRange, argList):
        return self._windSpd_value(statDict, timeRange, argList, "TransWind", "TransWind")

    def _transMetric_value(self, statDict, timeRange, argList):
        return self._wind_value(statDict, timeRange, argList, "TransWind", "TransMetWind")

    def _transSpdMetric_value(self, statDict, timeRange, argList):
        return self._windSpd_value(statDict, timeRange, argList, "TransWind", "TransMetWind")
    
    def _mixingHeight_value(self, statDict, timeRange, argList):      
        tree, node, colWidth = tuple(argList)        
        mix = self._getTableStats(tree, "MixHgt", timeRange, node.getAreaLabel())
        if mix is None:
            return "M"
        if self._tabularMixingHeightUnits == "ft" and colWidth != 4:
            mixft = int(mix/100.0+0.5) * 100
            if mixft < 100:
                value = "BLW100"
            else:
                value = `mixft`
        else:
            if mix < 50:
                mix = 100.0
            kmix = mix / 1000.0
            kmix = round(kmix,1)
            if kmix < 10:
                value = str(round(kmix,1))
            else:
                kmix = mix / 1000.0
                kmix = int(kmix + 0.5)
                value = `kmix`
        return value
 
    def _mixingHeightMetric_value(self, statDict, timeRange, argList):      
        tree, node, colWidth = tuple(argList)        
        mix = self._getTableStats(tree, "MixHgt", timeRange, node.getAreaLabel())
        if mix is None:
            return "M"
        if self._tabularMixingHeightUnits == "ft" and colWidth != 4:
            mixMetric = mix * 0.3048
            mixRounded = int(mixMetric/10.0+0.5) * 10
            if mixRounded < 10:
                value = "BLW10M"
            else:
                value = `mixRounded`
        else:
            if mix < 330:
                mix = 330.0
            mixMetric = mix * 0.3048 / 1000.0
            kmix = round(mixMetric,1)
            if kmix < 10:
                value = str(round(kmix,1))
            else:
                value = `kmix`
        return value

    def _cwr_value(self, statDict, timeRange, argList):      
        tree, node, colWidth = tuple(argList)        
        cwr = self._getTableStats(tree, self._cwrParm, timeRange, node.getAreaLabel())
        if cwr is None:
            return "M"
        return `int(cwr/10 + 0.5)*10`

    def _pop_value(self, statDict, timeRange, argList):      
        tree, node, colWidth = tuple(argList)        
        pop = self._getTableStats(tree, "PoP", timeRange, node.getAreaLabel())
        if pop is None:
            return "M"        
        return `int(pop/10 + 0.5)*10`

    def _lal_value(self, statDict, timeRange, argList):      
        tree, node, colWidth = tuple(argList)        
        lal = self._getTableStats(tree, "LAL", timeRange, node.getAreaLabel())
        if lal is None:
            return "M"      
        return `int(lal+0.5)`                
    
    def _dsi_value(self, statDict, timeRange, argList):      
        tree, node, colWidth = tuple(argList)        
        dsi = self._getTableStats(tree, "DSI", timeRange, node.getAreaLabel())
        if dsi is None:
            return "M"        
        return `int(dsi + 0.5)`

    
    def _ldsi_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        #dsi = self._getTableStats(tree, "DSI", timeRange, node.getAreaLabel())
        dsi = self._getTableStats(tree, "LDSI", timeRange, node.getAreaLabel())
        if dsi is None:
            return "M"        
        return `int(dsi + 0.5)`

    def _lvori_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        #lvori = self._getTableStats(tree, "DSI", timeRange, node.getAreaLabel())
        lvori = self._getTableStats(tree, "LVORI", timeRange, node.getAreaLabel())
        if lvori is None:
            return "M"        
        return `int(lvori + 0.5)`
        
    def _adi_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        adi = self._getTableStats(tree, "ADI", timeRange, node.getAreaLabel())
        if adi is None:
            return "M"        
        return `int(adi + 0.5)`

    def _haines_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        stats = self._getTableStats(tree, "Haines", timeRange, node.getAreaLabel())
        if stats is None:
            return "M"        
        return `int(stats + 0.5)`
        
    def _ventrate_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        ventrate = self._getTableStats(tree, "VentRate", timeRange, node.getAreaLabel())
        if ventrate is None:
            return "M"        
        return `int(ventrate/1000.0 + 0.5)`
        
    def _windWave_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        stats = self._getTableStats(tree, "WindWaveHgt", timeRange, node.getAreaLabel())
        if stats is None:
            return "M"        
        return `int(stats + 0.5)`
    
    def _waveHeight_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        stats = self._getTableStats(tree, "WaveHeight", timeRange, node.getAreaLabel())
        if stats is None:
            return "M"        
        return `int(stats + 0.5)`
    
    def _swellPeriod_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        stats = self._getTableStats(tree, "Period", timeRange, node.getAreaLabel())
        if stats is None:
            return "M"        
        return `int(stats + 0.5)`
    
    def _swell_value(self, statDict, timeRange, argList):
        return self._wind_value(statDict, timeRange, argList,"Swell", "RidgeWind" )
    
    def _swellDir_value(self, statDict, timeRange, argList):
        return self._windDir_value(statDict, timeRange, argList, "Swell", "RidgeWind")
    
    def _swellHgt_value(self,  statDict, timeRange, argList):
        return self._windSpd_value(statDict, timeRange, argList, "Swell", "RidgeWind")
    
    def _freezingLevel_value(self, statDict, timeRange, argList):      
        tree, node, colWidth = tuple(argList)        
        mix = self._getTableStats(tree, "FzLevel", timeRange, node.getAreaLabel())
        if mix is None:
            return "M"
        if mix < 50:
            mix = 100.0
        kmix = mix / 1000.0
        kmix = round(kmix,1)
        if kmix < 10:
            value = str(round(kmix,1))
        else:
            kmix = mix / 1000.0
            kmix = int(kmix + 0.5)
            value = `kmix`
        return value
 
    def _ceiling_value(self, statDict, timeRange, argList):      
        tree, node, colWidth = tuple(argList)        
        temp = self._getTableStats(tree, "PredHgt", timeRange, node.getAreaLabel())
        if temp is None:
            return " "
        temp = temp / 10.0
        if temp < 10:
            tempWords = string.strip("%4.1f" % temp)
        else:
            tempWords = `int(temp + 0.5)`
        return tempWords
    
    def _visibility_value(self, statDict, timeRange, argList):      
        tree, node, colWidth = tuple(argList)        
        temp = self._getTableStats(tree, "Vsby", timeRange, node.getAreaLabel())
        if temp is None:
            return " "
        print "colWidth =", colWidth
        if colWidth > 4:
            if temp < 1.0:
                tempWords = string.strip("%4.2f" % temp)
            elif temp >= 1.0 and temp < 3.0:
                tempWords = string.strip("%4.1f" % temp)
            else:
                tempWords = `int(temp + 0.5)`
        else:
            if temp < 1.0:
                tempWords = string.strip("%3.2f" % temp)
                tempWords = tempWords[1:]
            elif temp >= 1.0 and temp < 3.0:
                tempWords = string.strip("%3.1f" % temp)
            else:
                tempWords = `int(temp + 0.5)`
        return tempWords

    def _icing_value(self, statDict, timeRange, argList):      
       return " "

    def _td_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        temp = self._getTableStats(tree, "Td", timeRange, node.getAreaLabel())
        if temp is None:
            return "M"
        if temp >= 0:
            temp = int(temp + 0.5)
        else:
            temp = int(temp - 0.5)
        return `temp`

    def _heatIndex_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        temp = self._getTableStats(tree, "HeatIndex", timeRange, node.getAreaLabel())
        if temp is None:
            return "M"
        if temp >= 0:
            temp = int(temp + 0.5)
        else:
            temp = int(temp - 0.5)
        return `temp`

    def _wwa_exclude(self,stats):
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
        tree, node, colWidth = tuple(argList)        
        stats = self._getTableStats(tree, "Hazards", timeRange, node.getAreaLabel())
        if stats is None:
            return " "
        if stats[0] == "<None>":
            return " "
        stats = self._wwa_exclude(stats)
        return stats[0][0:2] + stats[0][3:4]

    def _wwa2_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        stats = self._getTableStats(tree, "Hazards", timeRange, node.getAreaLabel())
        if stats is None:
            return " "
        stats = self._wwa_exclude(stats)
        if len(stats) < 2:
            return " "
        return stats[1][0:2] + stats[1][3:4]

    def _wwa3_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        stats = self._getTableStats(tree, "Hazards", timeRange, node.getAreaLabel())
        if stats is None:
            return " "
        stats = self._wwa_exclude(stats)
        if len(stats) < 3:
            return " "
        return stats[2][0:2] + stats[2][3:4]

    ### NEW NARRATIVE PHRASES ###

    def dsi_phrase(self):
        return {
            "setUpMethod": self.dsi_setUp,
            "wordMethod": self.dsi_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }
    
    def dsi_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("DSI", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "DSI.................")
        return self.DONE()

    def dsi_words(self, tree, node) :
        "Create phrase Probability of Precipitation"
        statDict = node.getStatDict()
        dsi = self.getStats(statDict, "DSI")
        if dsi is None:
            return self.setWords(node.parent, "MISSING")
        dsi = self.getValue(dsi)
        words =  `int(dsi + 0.5)` 
        return self.setWords(node, words)
            
    def ldsi_phrase(self):
        return {
            "setUpMethod": self.ldsi_setUp,
            "wordMethod": self.ldsi_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }
    
    def ldsi_setUp(self, tree, node):
        #elementInfoList = [self.ElementInfo("DSI", "List")]
        elementInfoList = [self.ElementInfo("LDSI", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "LDSI................")
        return self.DONE()

    def ldsi_words(self, tree, node):
        "Create phrase Probability of Precipitation"
        statDict = node.getStatDict()
        #ldsi = self.getStats(statDict, "DSI")
        ldsi = self.getStats(statDict, "LDSI")
        if ldsi is None:
            return self.setWords(node.parent, "MISSING")
        ldsi = self.getValue(ldsi)
        words =  `int(ldsi + 0.5)` 
        return self.setWords(node, words)
            
    def lvori_phrase(self):
        return {
            "setUpMethod": self.lvori_setUp,
            "wordMethod": self.lvori_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }
    
    def lvori_setUp(self, tree, node):
        #elementInfoList = [self.ElementInfo("DSI", "List")]
        elementInfoList = [self.ElementInfo("LVORI", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "LVORI...............")
        return self.DONE()      

    def lvori_words(self, tree, node):
        statDict = node.getStatDict()
        lvori = self.getStats(statDict, "LVORI")
        #lvori = self.getStats(statDict, "DSI")
        if lvori is None:
            return self.setWords(node.parent, "MISSING")
        lvori = self.getValue(lvori)
        words =  `int(lvori + 0.5)` 
        return self.setWords(node, words)
            
    def pop_phrase(self):
        return {
            "setUpMethod": self.pop_setUp,
            "wordMethod": self.pop_words,
            "phraseMethods": self.standard_phraseMethods(),
            }  
    def pop_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("PoP", "Max")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "POP.................")
        return self.DONE()

    def pop_words(self, tree, node) :
        "Create phrase Probability of Precipitation"
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
            words =  `int(pop)` + " percent"
        return self.setWords(node, words)
          
    ### *** END TABULAR TEST SECTION HERE *** ###

# I had to create these phrases or labels so the FWS formatter will work
# for any WFO out of the baseline. I created labels for elements that
# grids are not created for (that I know of). If offices do have grids
# for these elements, then they can create the phrase to get it into
# the FWS product.

    # For EYE LEVEL WINDS
    def fireEyeWind_compoundPhrase(self):
        return {
            "phraseList": [
                self.wind_summary,
                self.wind_phrase,
                ],
            "phraseMethods": [
                self.consolidateSubPhrases,
                self.assembleSentences,
                self.fireEyeWind_finishUp
                ],
            }  
    def fireEyeWind_finishUp(self, tree, node):
        "Create a phrase for Winds"
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

    # For WIND SHIFT. Just need the label since there is not phrase.
    def fireWindShift_label_phrase(self):
        return {
            "setUpMethod": self.fireWindShift_label_setUp,
            "phraseMethods": [self.postProcessPhrase],
            }
  
    def fireWindShift_label_setUp(self, tree, node):
        self.setWords(node, "")
        node.set("descriptor", "")
        node.set("indentLabel", "WIND SHIFT..........")
        return self.DONE()

    # For Surrounding Ridge Wind.
    def surroundingRidgeWind_phrase(self):
        return {
            "setUpMethod": self.surroundingRidgeWind_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),
            }
    def surroundingRidgeWind_setUp(self, tree, node):
        self.wind_setUp(tree, node, gustFlag=0, element="FreeWind")
        node.set("descriptor", "")
        node.set("indentLabel","SURROUNDING RIDGE...")  
        return self.DONE()

    # For Chance of Preciptiation.
    def pop_phrase(self):
        return {
            "setUpMethod": self.pop_setUp,
            "wordMethod": self.pop_words,
            "phraseMethods": self.standard_phraseMethods(),
            }  
    def pop_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("PoP", "Average")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", "POP.................")
        return self.DONE()

    def pop_words(self, tree, node) :
        "Create phrase Probability of Precipitation"
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
            words =  `int(pop)` + " percent"
        return self.setWords(node, words)
          
    # For Stability Class.
    def stabilityClass_phrase(self):
        return {
            "setUpMethod": self.stabilityClass_setUp,
            "wordMethod": self.stabilityClass_words,
            "phraseMethods": self.standard_phraseMethods(),
            }
  
    def stabilityClass_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("Stability", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "STABILITY CLASS.....")
        return self.DONE()

    def stabilityClass_words(self, tree, node) :
        "Create phrase Stability Class"
        statDict = node.getStatDict()
        stability = self.getStats(statDict, "Stability")
        if stability is None:
            return self.setWords(node.parent, "MISSING")
        words =  `int(self.getValue(stability))`
        return self.setWords(node, words)
    
    # For Marine Layer.
    def marineLayer_phrase(self):
        return {
            "setUpMethod": self.marineLayer_setUp,
            "wordMethod": self.marineLayer_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }
  
    def marineLayer_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("MarineLayer", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "MARINE LAYER........")
        return self.DONE()

    def marineLayer_words(self, tree, node) :
        "Create phrase MarineLayer"
        statDict = node.getStatDict()
        marineLayer = self.getStats(statDict, "MarineLayer")
        if marineLayer is None:
            return self.setWords(node.parent, "MISSING")
        words =  `int(self.getValue(marineLayer))`
        return self.setWords(node, words)

    def td_phrase(self):
        return {
            "setUpMethod": self.td_setUp,
            "wordMethod": self.td_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }
  
    def td_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("Td", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "DEWPOINT............")
        return self.DONE()
    
    def td_words(self, tree, node):
        "Create phrase Td"
        statDict = node.getStatDict()
        td = self.getValue(self.getStats(statDict, "Td"), "Avg")
        if td is None:
            return self.setWords(node.parent, "MISSING")
        words =  `int(td)`
        return self.setWords(node, words)
    
    # For Begin/End of Preciptiation.
    def pcpnTiming_phrase(self):
        return {
            "setUpMethod": self.pcpnTiming_setUp,
            "phraseMethods": [self.postProcessPhrase],
            }
  
    def pcpnTiming_setUp(self, tree, node):
        self.setWords(node, "    ")
        node.set("descriptor", "")
        node.set("indentLabel", "BEGIN/END OF PCPN...")
        return self.DONE()

    def _checkStrs(self, checkStrings, inputStr, orderStrings=0, checkMode=1):
        # Check the inputStr for the list of checkStrings.
        # If a checkString is a tuple, at least one of the
        # given tuple strings must be found in the inputStr
        # If orderStrings == 1, the strings must occur in order in the inputStr
        # If checkMode == 0, the strings should NOT be found in the inputStr
        # Returns 1 if successful, the failed checkString if not.
        curIndex = -1
        for cStr in checkStrings:
            if type(cStr) == types.TupleType:
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



### For Testing
##    def getPreviousProduct(self, stqPil, searchString, version=0):
##        f = open("/home/eagle6/hansen/ui/middendorf/GTFSTQBYZ"+`version`, "r")
##        product = f.read()
##        f.close()
##        #print "returning", product
##        return product

