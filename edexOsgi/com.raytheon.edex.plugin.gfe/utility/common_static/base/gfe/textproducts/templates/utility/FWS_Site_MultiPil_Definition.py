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
##

# ---------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without
# technical  support, and with no warranty, express or implied, as to
# its usefulness for any purpose.
#
# FWS_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the
#  FWS formatter for a site.
#
# ---------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer     Description
# ------------- -------- ---------    ---------------------------------
# 03/11/2020    DCS20845 vmiddendorf  Upgrade GFE FWS formatter
#
# ---------------------------------------------------------------------
#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum content of this file is the following Definition statement

Definition = {}

# End MAKE NO CHANGES HERE
#**********************************************************************

#----- WFO <site> FWS Definition -----
# Definition Statements must start in column 1.

# REQUIRED CONFIGURATION ITEMS
#Definition['displayName'] = None
Definition['displayName'] = "FWS_<MultiPil>"
#Definition["productName"] = "Spot Forecast"  # name of product
Definition["stqPil"] = "STQ<site>"   # STQ pil
Definition["fwfPil"] = "FWF<site>"      # FWF pil

# Header configuration items
Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
Definition["wmoID"] = "<wmoID>"        # WMO ID
Definition["pil"] = "<pil>"          # product pil
Definition["areaName"] = "<state>" # Name of state, such as "Georgia"
Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city st
Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
Definition["outputFile"] =  "{prddir}/TEXT/FWS_<MultiPil>.txt"

# agencyList - This is a list of agency abbreviations as you want them to
#              appear in the product header. For Example...
#              SPOT FORECAST FOR WILLIE FIRE...USFS GNF
#              where "USFS GNF" is an example of agency abbreviation.
#              The FWS formatter will read the STQ spot request product
#              and will try to first guess the agency abbreviation from the
#              "REQUESTING AGENCY" line of the STQ product. If the first guess
#              is found in your agencyList list, then the Quality Control GUI
#              with have that agency pre-selected.

Definition["agencyList"] = []
#    [
#    (1,"AGENCY 1"),
#    (2,"AGENCY 2"),
#    (3,"AGENCY 3"),
#    (4,"AGENCY 4"),
#    (5,"AGENCY 5"),
#    ]

# forecasterList - This is a list of forecaster numbers, forecaster initials,
#                  and forecaster last names. The Quality Control GUI will
#                  list the forecaster's last name and the forecaster will
#                  check all of the forecaster's names that were involved
#                  in that forecast.

#Definition["forecasterList"] = [
#    (1,"FRA","FORECASTER A"),
#    (2,"FRB","FORECASTER B"),
#    (3,"FRC","FORECASTER C"),
#    (4,"FRD","FORECASTER D"),
#    (5,"FRE","FORECASTER E"),
#    ]

# stqNumberVersions - When you launch the FWS formatter, you will get a GUI
#                     that asks you to select which spot request you want to
#                     format a spot forecast for. This variable specifies
#                     how many spots you want to list in the GUI. If you do
#                     increase the number, then make sure you increase the
#                     number of versions stored in awips.

#Definition["stqNumberVersions"] = 50

# stqWmoID - helps find the timestamp line in the STQ product. Only change if
#            WSH changes the WMO id of the STQ product.

#Definition["stqWmoID"] = "BMBB91 K"

# typeList - This is a list of project types and are formatted in the "REASON FOR
#            REQUEST" line of the FWS forecast. Do not edit this list unless WSH
#            directs you to do so.

#Definition["typeList"] = ["WILDFIRE", "PRESCRIBED", "HAZMAT", "SAR", "MARINE", "OTHER"]

# defaultElementDict - In event Request Data is manually entered, these are the elements that will appear as
#                      default in the 2nd GUI.
#Definition["defaultElementDict"] =  {
#                                     "WILDFIRE":["SKY/WEATHER","TEMPERATURE","HUMIDITY","20 FOOT WINDS","RIDGE TOP WIND","CWR","LIGHTNING ACTIVITY LEVEL"],
#                                     "PRESCRIBED":["SKY/WEATHER","TEMPERATURE","HUMIDITY","20 FOOT WINDS","RIDGE TOP WIND","CWR","LIGHTNING ACTIVITY LEVEL","MIXING HEIGHT","TRANSPORT WINDS"],
#                                     "HAZMAT Land":["SKY/WEATHER","CHANCE OF PRECIPITATION","TEMPERATURE","HUMIDITY","DEWPOINT","SURFACE WINDS","MIXING HEIGHT","TRANSPORT WINDS"],
#                                     "HAZMAT Inland Waterway":["SKY/WEATHER","CHANCE OF PRECIPITATION","TEMPERATURE","HUMIDITY","DEWPOINT","SURFACE WINDS","MIXING HEIGHT","TRANSPORT WINDS"],
#                                     "SAR Land":["SKY/WEATHER","CHANCE OF PRECIPITATION","TEMPERATURE","HUMIDITY","DEWPOINT","SURFACE WINDS"],
#                                     "SAR Water":["SKY/WEATHER","CHANCE OF PRECIPITATION","TEMPERATURE","HUMIDITY","DEWPOINT","SURFACE WINDS"],
#                                     "MARINE":["SKY/WEATHER","TEMPERATURE","SURFACE WINDS"],
#                                     "OTHER":["SKY/WEATHER","TEMPERATURE","HUMIDITY","SURFACE WINDS"],
#                                    },

# wildfireElementList is a subset list of the requestedElementList list.
# The directive states that Sky/Weather, Temp, RH, and Winds are required
# for wildfire spot forecasts. Even if the user doesn't select these elements,
# the formatter will put them in anyway because of the directive requirements.

# You may add weather elements corresponding to the entries you see in your STQ product.

#Definition["wildfireElementList"] = [
#    "SKY/WEATHER",
#    "TEMPERATURE",
#    "HUMIDITY",
#    "20 FOOT WINDS",
#    "SLOPE/VALLEY WINDS"
#    "VALLEY/LOWER SLOPE WINDS"
#    "EYE LEVEL WINDS",
#    "TRANSPORT WINDS",
#    ]

# Set shortTermOnly to 1 if you don't want to give your forecasters an option
# include extended forecasts and/or outlooks with their spot forecasts.
#Definition["shortTermOnly"] = 0

# Definitions to insert unrepresentativeness of the forecast
# instructions for the user.
#Definition["insertUnrepresentStatement"] = 0  # Use 1 for yes, 0 for no
#Definition["unrepresentStatement"] = "If conditions become unrepresentative, " + \
#                                     "contact the National Weather\nService."

# If you want to add a phone number of your office for your customer to contact you
# in event of a problem or question, then use this statement. Set to "" if you do not
# want to use this feature.
#Definition["contactStatement"] = ""
#Definition["contactStatement"] = "Please contact our office if you have questions\nor concerns with this forecast."
#Definition["contactStatement"] = "Please add and configure contactStatement definition to your FWS_XXX_Definition file."

# There is a local app that emails forecasts to customers. The app looks for an .EMAIL line on the bottom of the forecast.
Definition["includeEmails"] = 1   # to added .EMAIL line
#Definition["includeEmails"] = 0   # to keep .EMAIL line off of FWS product

#Definition["tempLocalEffects"] = 1   # Set to 1 to enable Temp and RH local effects AFTER
                                      # creating AboveElev and BelowElev edit areas
#Definition["windLocalEffects"] = 1   # Set to 1 to enable wind local effects AFTER
                                      # creating Ridges and Valleys edit areas
# Weather-related flags
Definition["hoursSChcEnds"] = 240

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
# due to the fact that they use a 10-min average.  A common adjustment
# factor is 80% (0.80).  If you want no adjustment to the winds
# then set this variable to 1.00
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
#Definition["useRH"] = 1

### *** START TABULAR TEST SECTION *** ###

 
# Definitions to insert the FWF discussion from a separate file or
# from the last FWF your office has issued.

# Discussion is edited separately in GFE for the FWF forecast.
# Advantage of this is to have a first guess for the discussion in
# the Spot forecast...saving some composition time.

# Use 1 to get Discussion from a file
# Use 2 to get Discussion from your last FWF product
# Use 0 to use a blank Discussion template
#Definition["insertDiscussionFromFile"] = 2 
#Definition["discussionFile"] = "/data/local/DISFWFBYZ"

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
#                       in the "_rowList" (see FWS).
#   Tabular Only:       The spot forecast is in a table only format.
#
#   For each forecastType, you can specify a label that will appear in the GUI.
#
Definition["forecastTypeList"] = [
    #  Label                  Forecast Type        Code
    ("Narrative Only",       "Narrative Only",     "N"  ),
    ("Tabular/Narrative",    "Tabular/Narrative",  "C"  ),
    ("Tabular Only",         "Tabular Only",       "T"  ),

    # If your _rowList specifies an all Tabular product,
    # you may want to change this entry to:
    #("Tabular",             "Tabular/Narrative",  "C"  ),
    ]

# defaultForecastType - This definition sets the default setting for which spot forecast
#                       format your WFO wants to use FOR MANUAL Entry. Value for the definition
#                       must be included in the forecastTypeList definition. Otherwise, formatter
#                       will default to what is in the STQ.
#Definition["defaultForecastType"] = "Narrative Only"
Definition["defaultForecastType"] = "Tabular/Narrative"
#Definition["defaultForecastType"] = "Tabular Only"

#   withIgnitionTimes: If "yes", certain weather elements can be configured to include
#                    an ignition time forecast within the narrative.
Definition["withIgnitionTimes"] = "no"
#Definition["withIgnitionTimes"] = "yes"

#  includeIgnitionOptionOnGUI: If 1, the GUI will include this option at run-time.
Definition["includeIgnitionOptionOnGUI"] = 1
#Definition["includeIgnitionOptionOnGUI"] = 0

# tabularResolutionDict - This definition contains the list of table time resolutions
#                         (per period) that you want to appear in the GUI to the forecaster.
#                         Some WFOs may not want to give forecasters an option to generate
#                         a table with a 1 hour resolution (for example), so you can
#                         delete "1" from this list and it will not appear on the gui.
#                         Possible values are 1, 2, 3, 4 hours or "None" for no table.
Definition["tabularResolutionDict"] = {
    "Period 1": [1, 2, 3, 4, "None"],
    "Period 2": [1, 2, 3, 4, "None"],
    "Period 3": [1, 2, 3, 4, "None"],
    "Period 4": [1, 2, 3, 4, "None"]
    }

# defaultTabularResolution - This definition must be set to one of values listed in the
#                            tabularResolutionList definition. This will be the value
#                            that the gui will use for a default for each period.
#                            Values are limited to 1, 2, 3, and 4 and must be included in
#                            the tabularResolutionList definition.
#                            Note: these defaults only works for MANUAL entry. If an STQ
#                            is used, then the data in the STQ will be used as the default.
Definition["defaultTabularResolution"] = {
    "Period 1": 2,
    "Period 2": 2,
    "Period 3": 2,
    "Period 4": 2
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
# numeric 2hr:  Sky (%).........90     90     90     83     83     83     69

# Tabular Wind Format Definitions
# "alpha" will cause the direction to be formatted in the alphabetic characters
# of N, NW, W, SW, S, SE, E, and NE.
# "numeric" will return the wind direction in tens of degrees. i.e. 000, 010,
# etc. When a numeric wind direction is combined with wind speed it will look
# something like this...030/10

Definition["elementFormatDict"] = {
    "Sky" : "numeric",
    "Wind": "alpha",
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
# KFT 2hr:  Mix hgt (kft)...0.3    0.3    0.3    0.3    0.3    7.9    11
# FT 2hr :  Mix hgt (ft)....300    300    300    300    300    7900   11100

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
# Wind (20 ft)........
#  Slope/valley.......WEST 10-20 MPH
#  Ridgetop...........NORTHWEST 20 MPH
#
# By setting this definition to "0", you will get...
#
# Wind (20 ft)........WEST 10-20 MPH
# Ridgetop wind.......NORTHWEST 20 MPH
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

# yes 2hr:  20 ft wind dir..SW     W      W      W      W      W      W
#        :  20 ft wind spd..26     26     18     18     18     14     14
#        :  20 ft wind gust.40     40

# no 2 hr:  20 ft wind......W 26   W 26   W 18   W 18   W 18   W 14   W 14
#        :  20 ft wind gust.40     40

# yes 3hr:  20 ft wind dir..W         W         W         W         W
#        :  20 ft wind spd..26        25        13        14        13
#        :  20 ft wind gust.40        40

# no 3 hr:  20 ft wind......W 26G40   W 25G40   W 13      W 14      W 13

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

# yes 2hr:  Eye lvl wnd dir.SW     W      W      W      W      W      W
#        :  Eye lvl wnd spd.26     26     18     18     18     14     14
#        :  Eye lvl wnd gst.40     40

# no 2 hr:  Eye level wind..W 26   W 26   W 18   W 18   W 18   W 14   W 14
#        :  Eye lvl wnd gst.40     40

# yes 3hr:  Eye lvl wnd dir.W         W         W         W         W
#        :  Eye lvl wnd spd.26        25        13        14        13
#        :  Eye lvl wnd gst.40        40

# no 3 hr:  Eye level wind..W 26G40   W 25G40   W 13      W 14      W 13

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

# yes 2hr:  Ridge wnd dir..W      W      W      W      W      W      W
#        :  Ridge wnd spd..36     36     36     36     36     36     16

# no 2 hr:  Ridgetop wind...W 36   W 36   W 36   W 36   W 36   W 36   W 16

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

# yes 2hr:  Transp wind dir.W      W      W      W      W      W      W
#        :  Transp wind spd.8      8      8      8      8      8      20

# no 2 hr:  Transport wind..W 8    W 8    W 8    W 8    W 8    W 8    W 20

#Definition["tableTransElementSplit"] = "yes"
Definition["tableTransElementSplit"] = "no"

# cwrParm - Some WFOs (especially in wetter climates) use the PoP grid for
#           chance of wetting rain, whereas offices in dry climates create a
#           CWR grid that has values lower than the PoP grid. Value values
#           for this definition is either "CWR" or "PoP".

Definition["cwrParm"] = "PoP"

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

### *** END TABULAR TEST SECTION *** ###



