"""########################################################################
# Experimental TAF (TAC and IWXXM)
#
#-------------------------------------------------------------------------
# Description:  This product will generate an experimantal version of the
#               Terminal Aerodrome Forecast (TAF) for a list of airports and
#               outputs the TAF in TAC (ASCII) format as specified in the
#               directives and can optionally save the airport TAFs in
#               IWXXM-US 2.0 (XML) format to a user specified directory. The
#               TAC (ASCII) format is what will be returned and displayed in
#               the Formatter Launcher when the formatter is run.
#
#               Tweak the grids and configuration settings until satisfied
#               with the output and then run AvnFPS to transmit the TAF.
#-------------------------------------------------------------------------
#
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
#  TAF, TAF_RR_Overrides, TAF_XXX_Overrides,
#  TAF_XXX_Definition
#-------------------------------------------------------------------------
# Version: 20181114
#
# Modified: 14 November 2018
#-------------------------------------------------------------------------
# Author:  GSD Digital Aviation Services Group
#
# Developers: Sarah Pontius and Tom LeFebvre
#
# Support Email: nws.digital.aviation.services@noaa.gov
#-------------------------------------------------------------------------
# Customization Points:
#
#  See the Customization information in the
#  "Digital TAF Tools and Formatter Installation" document (located at
#  https://docs.google.com/document/d/1Qdz7d0x6uTv90w8l6WKkOQ7Vu17vZO1Ey0jjSoeE8J0/edit
#  or locally in /tags/latest_stable/doc) and also the TAF_XXX_Definition
#  and TAF_XXX_Overrides files for information about how to customize the
#  formatter.
#
#-------------------------------------------------------------------------
# Weather Elements Needed:  Sky, PoP, Wind, WindGust, Wx, CloudBasePrimary,
#                           CloudBaseSecondary, CloudBaseConditional,
#                           Ceiling, Visibility, VisibilityConditional, LLWS
#                           and LLWSHgt
#
# Optional Weather Elements: SkyPrimary, SkySecondary (will be used if present)
#
#-------------------------------------------------------------------------
# Edit Areas Needed:        One edit area for each airport in the TAF
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
#
#  The shortening algorithms and especially the significance rating rules
#  are all still experimental and prototypes. Forecasters are needed to
#  help improve the rating rules and shortening algorithms.
#
#-------------------------------------------------------------------------
# Additional Information:
#
#  High level flow of the formatter:
#    When running the TAF, it first brings up a GUI (defined in
#    _processVariableList) with the options pre-selected (based off of best
#    guesses for the most appropriate options) and the forecaster can modify
#    the selections if desired/needed.
#
#    The main entry point into the formatter is generateForecast. The TAF uses
#    a product parts design so it works by getting information (or creating it
#    when it's not available via grids) and storing that information (aka.
#    product parts) in product dictionaries. Once it has all the needed product
#    parts, it passes those product dictionaries to output formatters which
#    take the information contained in the dictionaries and constructs the
#    output in a particular format; currently TAC (ASCII) and IWXXM-US 2.0
#    (XML) formats are supported. This design allows for much cleaner, easier
#    to understand code and makes it easier to add new ways of presenting the
#    information in the TAF to the customers.
#
#  Classes Overview:
#    TextProduct               - This is the main class that all GFE formatters
#                                have. It sets everything up, creates the
#                                product dictionaries (with helper classes) and
#                                calls the appropriate output formatter(s) to
#                                construct the output.
#
#      GenericGroup            - This is the base class for all the various
#                                groups (FM, TEMPO and PROB30). It contains
#                                things needed by all the various groups like
#                                methods for getting data from the grids and
#                                generic/common product parts not specific to
#                                a particular group.
#
#      FmGroup                 - This stores a product dictionary containing
#                                the product parts needed for a FM group and
#                                contains everything needed to generate all
#                                those product parts.
#
#      ConditionalGroup        - This stores a product dictionary containing
#                                the product parts needed for a conditional
#                                (TEMPO/PROB30) group and contains everything
#                                needed to generate all those product parts.
#
#      WeatherUglyStringInfo   - This breaks down, modifies (if necessary) and
#                                stores information contained in "weather ugly
#                                strings" like "Sct:T:+:1/4SM:SmA,HvyRn" that
#                                are read in from the grids.
#
#      GroupWeatherInfo        - Since a significant portion of the formatter
#                                code is for dealing with weather, this class
#                                gathers, creates and stores all the needed
#                                weather information for a particular group.
#
#      SignificanceRatingRules - This is used for determining the significance
#                                of each of the product parts in a particular
#                                group so that the group's overall significance
#                                compared to other groups can be determined.
#
#      ShorteningAlgorithms    - This is used for determining which groups are
#                                safest to remove so that the TAF can try to be
#                                as concise as possible while still being as
#                                meteorologically accurate as possible and
#                                conveying all the operationally significant
#                                information needed by airports.
#
#    TAF_DataException         - This is an exception used when failing to get
#                                data from the grids and indicates information
#                                like if that data is truly needed or if it can
#                                be ignored or if the grid did have data but it
#                                was a null (aka. does not exist) value.
#
#    Output_Formatter          - The base class for the output formatters.
#
#    TAC_Formatter             - The output formatter for constructing the TAF
#                                in the TAC (ASCII) format as specified in the
#                                directives.
#
#    IWXXM_Formatter           - The output formatter for constructing the TAF
#                                in the experimental IWXXM-US 2.0 (XML) format.
#
#      tag                     - A helper class used for specifying information
#                                about tags/elements in the XML and how to
#                                create them.
#
#-------------------------------------------------------------------------
#  Example TAC (ASCII) Output:
#
#    FTUS41 KBOX 081003
#
#    TAF
#    KBOS 081003Z 0812/0918 23010KT P6SM SCT090 BKN250
#         FM090300 26005KT P6SM FEW250
#         FM091600 20010KT P6SM SCT250=
#
#    TAF
#    KPVD 081003Z 0812/0912 23006KT P6SM FEW090 SCT250
#         FM090400 00000KT P6SM FEW250=
#
#    TAF
#    KBDL 081003Z 0812/0918 21004KT P6SM SCT050 BKN250
#         FM091600 20009KT P6SM SCT250=
#
#    TAF
#    KBAF 081003Z 0812/0912 00000KT P6SM SCT050 BKN250
#         FM081600 29005KT P6SM FEW250
#         FM082300 00000KT P6SM FEW250=
#
#    TAF
#    KORH 081003Z 0812/0912 25011KT P6SM SCT050 BKN250
#         FM081600 27009KT P6SM FEW250=
#
#    TAF
#    KMHT 081003Z 0812/0912 22004KT P6SM SCT050 BKN090
#         FM081300 23004KT P6SM BKN050 BKN090=
#
#    TAF
#    KFMH 081003Z 0812/0912 22011KT P6SM FEW090 SCT250
#         FM081500 23010G19KT P6SM SCT250
#         FM081700 25009KT P6SM FEW250
#         FM082200 27004KT P6SM FEW250=
#
#    TAF
#    KHYA 081003Z 0812/0912 24008KT P6SM FEW090 SCT250
#         FM081800 27011KT P6SM FEW250
#         FM081900 28009KT P6SM FEW250
#         FM082300 VRB03KT P6SM FEW250=
#
#    TAF
#    KACK 081003Z 0812/0912 23011KT P6SM FEW090 SCT250
#         FM082000 27009KT P6SM SCT250
#         FM090200 32004KT P6SM FEW250=
########################################################################"""

import TextRules
import SampleAnalysis
import ProcessVariableList
import TimeRange
import time
import re
import copy
import os
import pprint
import inspect
import traceback
import sys
from collections import OrderedDict
from operator import itemgetter
import LocalizationSupport
import functools

class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    Definition = {
        #--- ======
        #--- GENERIC
        #--- ======

        #--- type -------------------------------------------------------------
        "type": "smart",

        #--- displayName ------------------------------------------------------
        "displayName": None,

        #--- defaultEditAreas -------------------------------------------------
        # The edit areas surrounding the airports to be sampled for the TAF.
        # The edit areas are listed by indicating the latitude and longitude
        # (in decimal degrees) of the center point of the edit area (where the
        # airport is), the distance from the center point to the edge of the
        # edit area (typically 16 kilometers aka. 10 statute miles), and the
        # ICAO identifier code.
        #
        # Template:
        # ((latitude, longitude, edit area radius), ICAO identifier)
        "defaultEditAreas": [
                             # ((42.36, -71.04, 16), "KBOS"),
                            ],

        #--- fullStationID ----------------------------------------------------
        "fullStationID": "<fullStationID>",  # full station id (e.g., KBOX)

        #--- wmoID ------------------------------------------------------------
        "wmoID": "<wmoID>",  # product WMO ID (e.g., FTUS41)

        #--- pil --------------------------------------------------------------
        "pil": "TAF<site>",  # product pil (e.g., TAFBOX)

        #--- debug ------------------------------------------------------------
        # Define a flag to turn on/off ALL (or certain) debug messages
        "debug": 1,  # 1 = Full debug
                       # 0 = No debug
                       # map = debug for certain methods only

        #--- outputFile -------------------------------------------------------
        # Path for formatter output so that it may be stored in the text database.
        # No need to change this as it is only used for the text database.
        # The output is redundantly stored (without the header) in:
        # /caveData/etc/site/[siteID]/aviation/tmp/gridTAF.txt
        # for loading into AvnFPS.
        "outputFile": "/localapps/data/products/gridTAF.txt",

        #--- autoWrite --------------------------------------------------------
        # Specify whether or not to write the product to file
        # TODO: This doesn't appear to be used currently??
        "autoWrite": 1,  # 1 = Write product to file
                           # 0 = Don't write

    #==========================================================================
    # Product-specific variables:

        #--- =========
        #--- TAF SPECIFIC
        #--- =========

        #--- tafLength --------------------------------------------------------
        # The maximum length (in hours) of each TAF issued by your office
        "tafLength": {
                      # "KBOS": 30,
                     },

        #--- CACThresholds ----------------------------------------------------
        # Define the flight category thresholds for each airport
        "CACThresholds" : {
            # "KBOS": {"A":(2, 0.50), "B":(7, 2), "C":(10, 3), "D":(30, 5),
            #         "E":(20, 1),
            #         "F":[(25, 3, (340, 80), "Loss of Visual Approach"),
            #              (14, 3, (340, 80), "Loss of Circling Approach"),
            #              ( 8, 2, (340, 80), "ILS Hold Points Assigned")]},
        },

        #--- weatherRules -----------------------------------------------------
        # Define how weather is reported in the TAF output
        # TODO: Would inverting the rules help any?
        "weatherRules" : {

            #  Handle Thunderstorms specifically
            "T": {
                # Number of hours since Issuance time
                (0, 3): {
                    # Probability/Coverage (Intensity optional)
                    "SChc, Iso":  "",  # This isn't probable enough to show
                    "Chc,  Sct":  "VCTS",
                    "default":    "PREVAIL"},
                (3, 9): {
                    "SChc, Iso":  "",
                    "Chc,  Sct":  "VCTS",
                    "default":    "TEMPO"},
                "default": {
                    "SChc, Iso":  "",  # Too far out, not probable enough
                    "Chc,  Sct":  "PROB30",
                    "default":    "VCTS"},
            },

            # Handle Fog specifically
            "F": {
                # Number of hours since Issuance time
                "default": {
                    # Probability/Coverage (Intensity optional)
                    "default":    "PREVAIL"},
            },

            # Handle all other precipitation types
            "default" : {
                # Number of hours since Issuance time
                (0, 3): {
                    # Probability/Coverage (Intensity optional)
                    "SChc, Iso":  "",  # This isn't probable enough to show
                    "Chc,  Sct":  "VCSH",
                    "default":    "PREVAIL"},
                (3, 9): {
                    "SChc, Iso":  "",
                    "Chc,  Sct":  "VCSH",
#                     "default":    "TEMPO"},
                    "default":    "PREVAIL"},
                "default": {
                    "SChc, Iso":  "",  # Too far out, not probable enough
                    "Chc,  Sct":  "PROB30",
                    "default":    "PREVAIL"},
            },
        },

        #--- fmWeatherTypesToRepeat -------------------------------------------
        # A list of TAF weather type codes that can be repeated/copied into a
        # TEMPO or PROB30 group under certain circumstances; whenever a FM
        # group has a TAF weather type code that is in this list and a TEMPO or
        # PROB30 group exists, the TAF weather type code will shown in the
        # TEMPO or PROB30 group as well. For instance, if "BR" is in this list
        # and a FM group has a "BR" as part of it's weather, any TEMPO or
        # PROB30 group associated with that FM group will also have "BR" as
        # part of its weather as well.
        #
        # NOTE: Weather types can have attributes and so if you just want
        # thunderstorms with no attributes to be repeated, you would put "TS"
        # here and if "TSGR" (thunderstorms with large hail) occurs, it would
        # not be repeated; to repeat "TSGR", it needs to be specified
        # explicitly.
        # TODO: Add other fog types to the default list?
        "fmWeatherTypesToRepeat" : ["BR", ],

        #--- minP6smVisibility ------------------------------------------------
        # The minimum visibility value considered to be P6SM
        "minP6smVisibility": 7,

        #--- tempoProbDefaultHeight -------------------------------------------
        # Cloud base height to use when weatherRules forces a TEMPO/PROB group
        # but CloudBaseConditional isn't available and the cloud base in the FM
        # group is SKC so we need to guess at a height. This must be a number
        # with a correct reportable value in hundreds of feet.
        "tempoProbDefaultHeight" : 40,

        #--- reportVC ---------------------------------------------------------
        # Report "vicinity" weather
        "reportVC": 1,  # 1 = Yes
                          # 0 = No

        #--- calmWindSpeed ----------------------------------------------------
        # The maximum speed (knots) allowed to be considered "calm"
        "calmWindSpeed": 2,

        #--- variablesWindSpeed -----------------------------------------------
        # Wind speed (knots) below which winds may be considered variable
        "variableWindSpeed": 4,

        #--- minWindToReportGusts ---------------------------------------------
        # Minimum sustained wind speed to allow reporting of wind gusts
        "minWindToReportGusts": 10,

        #--- minGustSpeedDifference -------------------------------------------
        # Wind gusts must be at least this much higher than sustained wind
        # speed to be reported.
        "minGustSpeedDifference": 8,

        #--- useDetailedCloudHeights ------------------------------------------
        # Toggle detailed cloud reporting
        "useDetailedCloudHeights": 1,  # 0 = No
                                         # 1 = Yes

        #--- disclaimer -------------------------------------------------------
        # Routine statements for specific airports to indicate when
        # observations are ending and resuming.
        # TODO: Determine if placeholders are supported by AvnFPS in AWIPS 2
        "disclaimer": {},

        #--- verificationHeaders ----------------------------------------------
        # Toggle production of verification headers
        "verificationHeaders": 0,  # 1 = Yes
                                     # 0 = No (routine)

        #--- areaOverrides ----------------------------------------------------
        # Area overrides allow you to modify behavior for particular airports
        "areaOverrides": {},

        #--- _______________
        #--- SHORTENING

        #--- maxFmGroups ------------------------------------------------------
        # Mimimum number of FM groups for each TAF. 
        "minFmGroups" : 4,

        #--- maxFmGroups ------------------------------------------------------
        # Maximum number of FM groups for each TAF (this isn't a hard limit,
        # it's more of a goal for the formatter)
        "maxFmGroups" : 8,

        #--- maxLightWindSpeed ------------------------------------------------
        # Maximum wind speed considered to be light wind
        "maxLightWindSpeed": 5,

        #--- maxSimilarLightWindDirChange -------------------------------------
        # Maximum light wind/llws direction change amount still considered to
        # be similar enough
        "maxSimilarLightWindDirChange": 59,

        #--- maxSimilarWindDirChange ------------------------------------------
        # Maximum non-light wind/llws direction change amount still considered
        # to be similar enough
        "maxSimilarWindDirChange": 39,

        #--- maxSimilarWindGustChange -----------------------------------------
        # Maximum wind gust change amount still considered to be similar enough
        "maxSimilarWindGustChange": 7,

        #--- minVfrVisibility -------------------------------------------------
        # Minimum VFR visibility (in statute miles)
        "minVfrVisibility" : 6,

        #--- maxSimilarNonCeilingHeightChange ---------------------------------
        # Maximum non-ceiling height change amount (100s ft) still considered
        # to be similar enough
        "maxSimilarNonCeilingHeightChange": 99,

        #--- maxSignificantNonCeilingHeight -----------------------------------
        # Maximum operationally significant non-ceiling height (100s ft)
        "maxSignificantNonCeilingHeight": 50,

        #--- maxSignificantNonCeilingHeight -----------------------------------
        # Maximum operationally significant ceiling height (100s ft)
        "maxSignificantCeilingHeight": 119,

        #--- thresholdsNLValues -----------------------------------------------
        # This allows use of non-linear thresholds for defined fields. The
        # thresholds are for determining if certain fields are 'similar' enough
        # to each other. "WIND_MAG" and "LLWS_MAG" thresholds must be defined
        # here. The values of the map are the thresholds and changes to the
        # field must be less than the threshold to be considered "similar". For
        # example, for wind magnitude, for winds that are >= 50 kts, changes in
        # wind magnitude less than 20 kts will be considered "similar".
        "thresholdsNLValues": {  # Thresholds for surface wind magnitude
                               "WIND_MAG":
                                {(0.0, 10.0):  4,  # for min speed < 10 kt
                                 (10.0, 20.0): 8,  # for min speed 10-19 kt
                                 (20.0, 50.0): 10,  # for min speed 20-49 kt
                                 "default":    20,  # for min speed >= 50 kt
                                },
                               
                               # Thresholds for low-level wind shear magnitude
                               "LLWS_MAG":
#                                 {"default": 10,      # for all speeds   # changed this to 20 - tl
                                # New change for bug 37459
                                {"default": 20,  # for all speeds
                                },
                               },

        #--- maxNonSignificantRating ------------------------------------------
        # Maximum significance rating that is considered to be operationally
        # non-significant.
        "maxNonSignificantRating": 60,
        
        # Set this to True if the site is using SkyPrimary and/or SkySecondary
        # as additional Sky coverage elements. If this is set to True and those
        # elements are not configured in your GFE database, AlertViz errors will
        # display, but the resulting text product will not be affected.
        "useAdditionalSkyElements" : False,
        
        # This variable was moved from the GUI as it was determined most sites
        # would want this most of the time.
        # Set to "Yes" if you want multi-hour TEMPO/PROB groups, otherwise no.
        "allowMultiHourTempoProbs" : "Yes",
        
        # Weighting values to de-emphasize the latter periods.
        # Format: [(startHour, weight), (startHour, weight)...],
        # where weight is out of 100. 
        "timeWeighting" : [(0, 100), (24, 50)],
                
        # Wind speed threshold below which wind changes are all but ignored when 
        # ranking FMGroups.
        "windSpeedRankLow" : 7,
        
        # Moderate Wind speed threshold used to assigning wind ranking
        "windSpeedRankModerate" : 12,
        # Wind magnitude temporal difference at which a new FMGroups will be automatically
        # generated 
        "windMagDiff" : 10,
        # Wind direction threshold below which wind changes are all but ignored
        # when ranking FMGroups.
        "windDirRankLarge" : 30,
        # Any hourly wind gust change of this magnitude will generate a new FMGroup
        "windGustDiff" : 7,
        # Translates GFE Wx type to TAF Wx type.  DO NOT MODIFY.
        "gfeCodeToTafCodeMap" : {
            "<NoWx>": "",  # No weather
            "T" : "TS",  # Thunderstorm
            "R" : "RA",  # Rain
            "RW": "SHRA",  # Rain shower
            "L" : "DZ",  # Drizzle
            "ZR": "FZRA",  # Freezing rain
            "ZL": "FZDZ",  # Freezing drizzle
            "S" : "SN",  # Snow
            "SW": "SHSN",  # Snow shower
            "IP": "GS",  # Ice pellets/sleet
#             "IP": "PL",    # Ice pellets/sleet   # changed to above for 10/31/18 version
            "F" : "BR",  # Mist/fog (visibility > 1/2SM)
            "ZF": "FZFG",  # Freezing fog (visibility <= 1/2SM)
            "IF": "BR",  # Ice fog (visibility > 1/2SM)
            "IC": "IC",  # Ice crystals
            "H" : "HZ",  # Haze
            "BS": "BLSN",  # Blowing snow
            "BN": "BLSA",  # Blowing sand
            "BD": "BLDU",  # Blowing dust
            "K" : "FU",  # Smoke
            "ZY": "",  # Freezing spray (FZPY) - never report it
            "VA": "VA",  # Volcanic ash
            # GFE attributes
            "SmA": "GR",  # Small hail - must have thunderstorm
            "LgA": "GR",  # Large hail - must have thunderstorm
            },

        }

#     gfeCodeToTafCodeMap = {
#         "<NoWx>": "",  # No weather
#         "T" : "TS",  # Thunderstorm
#         "R" : "RA",  # Rain
#         "RW": "SHRA",  # Rain shower
#         "L" : "DZ",  # Drizzle
#         "ZR": "FZRA",  # Freezing rain
#         "ZL": "FZDZ",  # Freezing drizzle
#         "S" : "SN",  # Snow
#         "SW": "SHSN",  # Snow shower
#         "IP": "GS",  # Ice pellets/sleet
# #             "IP": "PL",    # Ice pellets/sleet   # changed to above for 10/31/18 version
#         "F" : "BR",  # Mist/fog (visibility > 1/2SM)
#         "ZF": "FZFG",  # Freezing fog (visibility <= 1/2SM)
#         "IF": "BR",  # Ice fog (visibility > 1/2SM)
#         "IC": "IC",  # Ice crystals
#         "H" : "HZ",  # Haze
#         "BS": "BLSN",  # Blowing snow
#         "BN": "BLSA",  # Blowing sand
#         "BD": "BLDU",  # Blowing dust
#         "K" : "FU",  # Smoke
#         "ZY": "",  # Freezing spray (FZPY) - never report it
#         "VA": "VA",  # Volcanic ash
#         # GFE attributes
#         "SmA": "GR",  # Small hail - must have thunderstorm
#         "LgA": "GR",  # Large hail - must have thunderstorm
#         }


    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)

    def generateForecast(self, argDict):       
        
        """
        This generates a product dictionary containing product parts and then
        calls various formatter classes that will use the product dictionary to
        create and save the TAF product output in various different formats.
        """
        
        # Get variables
        error = self._getVariables(argDict)
        if error is not None:
            return error

        # Get the areaList -- derived from defaultEditAreas and
        # may be solicited at run-time from user if desired
        self._areaList = self.getAreaList(argDict)
               
        if len(self._areaList) == 0:
            return "WARNING -- No Edit Areas Specified to Generate Product."

        # Create the product parts that aren't airport specific and add them
        # to the product dictionary.
        self._createSharedProductParts()

        # Generate the product for each edit area in the list
        for editArea, airportIcaoId in self._areaList:
            #==================================================================
            # Let's get started on this TAF site
            self.debug_print("\n" + "#" * 80 + 
                             "\nWorking on -> %s" % airportIcaoId, 1)

            self._preProcessAirport(argDict, editArea, airportIcaoId)

            # Create the product parts specific to this airport and add them
            # to the product dictionary.
            self._createAirportProductParts(airportIcaoId)

            self._postProcessAirport(argDict, airportIcaoId)

        try:
            # Create the TAF output in IWXXM-US 2.0 (XML) format
            formatter = IWXXM_Formatter(self)
            formatter.createOutput(self._productDict, self._airportDicts)
        except Exception as e:
            self.debug_print("Failed to create IWXXM files: %s" % e, 1)

        # Create and return the TAF in TAC (ASCII) format
        formatter = TAC_Formatter(self)
        fcst = ""
        fcst = self._preProcessProduct(fcst, argDict)
        
        fcst = formatter.createOutput(fcst, self._productDict, self._airportDicts)
        
        fcst = self._postProcessProduct(fcst, argDict)

        return fcst

    def _getVariables(self, argDict):
        """
        Gets and makes available TAF specific variables, Definition dictionary
        variables, and variables based on forecaster-selected GUI options.
        """

        # Set up support for debugging
        self._debug = argDict["forecastDef"]["debug"]
        self._pp = pprint.PrettyPrinter()
        self._pformat = self._pp.pformat

        # Get current time
        self._currentTimeSeconds = argDict["creationTime"]
        self._currentTimeLocal = time.localtime(self._currentTimeSeconds)
        self._currentTimeUTC = time.gmtime(self._currentTimeSeconds)

        #----------------------------------------------------------------------

        # Display TAF Version and current time
        self.debug_print("\n" + "#" * 80, 1)
        self.debug_print("TAF %s" % self._getVersion() + "\n", 1)
        self.debug_print("current time = %s (seconds)"
                         % self._currentTimeSeconds, 1)
        self.debug_print("current time = %s (UTC)"
                         % time.asctime(self._currentTimeUTC), 1)
        self.debug_print("current time = %s (local)"
                         % time.asctime(self._currentTimeLocal), 1)

        #----------------------------------------------------------------------

        self._getConfigurationSettings(argDict)
        self._getGuiSelections(argDict)

        #----------------------------------------------------------------------

        # Defines sky cover thresholds for sky categories. They were decided
        # upon by the DAS group and placed here (instead of the Definition
        # dictionary) to discourage offices from overriding them so that sites
        # are consistent with one another. They are percentages representing
        # how much of the sky is covered by clouds. The end percentage of a
        # category is inclusive; so for example, according to these thresholds,
        # clouds will be considered broken (BKN) when 57% - 87% of the sky is
        # covered.
        self._maxSkyPercentages = OrderedDict()
        self._maxSkyPercentages["SKC"] = 6
        self._maxSkyPercentages["FEW"] = 31
        self._maxSkyPercentages["SCT"] = 56
        self._maxSkyPercentages["BKN"] = 87
        self._maxSkyPercentages["OVC"] = 100

        # Determine the maximum TAF length for this office
        self._maxTafLength = 0
        for length in self._tafLength.values():
            if length > self._maxTafLength:
                self._maxTafLength = length

        # Create the product dictionaries that will hold the product parts used
        # by the formatters to create the TAF output in various formats.
        self._productDict = OrderedDict()
        self._airportDicts = OrderedDict()
        
    def _determineTimeRanges(self, argDict, airportIcaoId):
        """
        Determines the time range which needs to be sampled for an airport.
        This time range starts at the chosen base forecast time and lasts for
        the maximum TAF duration defined for the airport. Data will be sampled
        in one hour increments spanning the time range.
        """

        self.debug_print("\n" + "="*80 + "\n%s Time Information:"
                         % airportIcaoId, 1)

        #----------------------------------------------------------------------
        # Determine the number of one hour periods we need to sample

        (self._startTimeSeconds, self._endTimeSeconds) = \
            self._getAirportStartEndTimes(airportIcaoId)

        startTimeUTC = time.gmtime(self._startTimeSeconds)
        endTimeUTC = time.gmtime(self._endTimeSeconds)

        numPeriods = \
            int((self._endTimeSeconds - self._startTimeSeconds) / 3600.0)

        self.debug_print(
            "\nstart time = %s (UTC)\nend time = %s (UTC)\nnumPeriods = %s" % 
            (time.asctime(startTimeUTC), time.asctime(endTimeUTC), numPeriods),
            1)

        #----------------------------------------------------------------------
        # Make a time range for this airport TAF

        # Zero out the minutes and seconds (Sampling done at 1 hour boundaries)
        startTimeSeconds = self._startTimeSeconds \
                           - startTimeUTC.tm_min * 60 - startTimeUTC.tm_sec

        endTimeSeconds = self._endTimeSeconds \
                           - endTimeUTC.tm_min * 60 - endTimeUTC.tm_sec

        self._timeRange = self.makeTimeRange(startTimeSeconds, endTimeSeconds)

        self.debug_print("final time range = %s" % (repr(self._timeRange)), 1)

        #----------------------------------------------------------------------
        # Make the periods we need to sample - one hour blocks with no gaps

        # Define a label template for each time period
        # Format is of form: (LT_OR_Zulu, durationFmt, startFmt, endFmt)
        tafLabel = ("Zulu", "", "FM%d%H00", "")

        # Create the number of periods requested.
        self._samplePeriods = self.getPeriods(self._timeRange,
                                              period=1, span=1,
                                              numPeriods=numPeriods,
                                              labelFormat=tafLabel)
        
        self.debug_print("sample periods:", 1)
        for index, (tr, fmGroupLabel) in enumerate(self._samplePeriods):
            self.debug_print("%02d) %s:    %s"
                             % (index + 1, fmGroupLabel, tr), 1)

        #----------------------------------------------------------------------
        # Set aside the list of start and end times (in seconds) for all
        # periods in this airport TAF.

        self._allPeriodStartEndTimes = []

        # Go through each of the periods and save the start and end times (in
        # seconds) for each one so that we can determine the duration of TEMPO
        # and PROB30 groups later.
        nextStartTimeSeconds = startTimeSeconds
        for periodIndex in range(numPeriods):
            periodStartTimeSeconds = nextStartTimeSeconds
            # Periods are one hour long
            periodEndTimeSeconds = periodStartTimeSeconds + 60 * 60

            self._allPeriodStartEndTimes.append((periodStartTimeSeconds,
                                                 periodEndTimeSeconds))

            # There are no gaps in the time, so the start of the next period is
            # the end of this period.
            nextStartTimeSeconds = periodEndTimeSeconds

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        # Return a text string representing the product
        # for the given edit area
        return fcst

    def _getAnalysisList(self):
        """
        This method defines which sampling techniques and weather elements to
        use.
        """

        return [
            ("Sky", self.avg),
            ("PoP", self.avg),
            ("Wind", self.vectorAvg),
            ("WindGust", self.maximum),
            ("Wx", self.dominantWx),  # do NOT use rankedWx
            ("CloudBasePrimary", self.mode),
            ("CloudBaseSecondary", self.mode),
            ("CloudBaseConditional", self.mode),
            ("Ceiling", self.mode),
            ("Visibility", self.mode),
            ("VisibilityConditional", self.mode),
            ("LLWS", self.vectorMax),
            ("LLWSHgt", self.maximum),
        ]

    def _getOptionalAnalysisList(self):
        """
        This method defines which sampling techniques and weather elements to
        use for the optional weather elements (which may or may not exist).
        """
        print("Getting optional analysis list.")
        return [
            ("SkyPrimary", self.avg),
            ("SkySecondary", self.avg),
            ("SkyTertiary", self.avg),
            ("CloudBaseTertiary", self.avg),
        ]


    def _processVariableList(self, definition):
        """
        This method is called prior to running the product and displays the
        GUI. It tries to automatically determine a forecast base time from
        the current UTC time. Once this is known, a guess is made to set the
        TAF type. The GUI presented to the forecaster has these options
        pre-selected. Finally, the forecaster-approved options are returned
        and become accessible to the formatter via argDict["varDict"].
        """

        # Get what the current UTC time
        gmt = time.gmtime()

        # Define initial GUI selection for base time
        baseTime = self._getInitialBaseTimeForGui(gmt)

        # Define initial GUI selection for TAF type
        tafType = self._getInitialTafTypeForGui(gmt)

        # Define formatter GUI options
        varList = [
            (("TAF base time", "productIssuance"),
                baseTime, "radio",
                ["00Z", "06Z", "12Z", "18Z"]),
            (("TAF Type", "tafType"),
                tafType, "radio",
                ["Routine", "Amendment", "Delayed", "Corrected"]),
            (("Only allow one\nTEMPO per TAF?", "limitOneTempoPerTAF"),
                "No", "radio",
                ["Yes", "No"]),
            (("Detail Level:", "detail"), 5, "scale", [1, 10], 1),
            # Moved to Definitions section
#             (("Allow Multi-hour\nTEMPO/PROBs?", "allowMultiHourTempoProbs"),
#                 "Yes", "radio",
#                 ["Yes", "No"]),
            # Removed entirely. Now the formatter write IWXXM format always
#             (("Save IWXXM-US 2.0\n(XML) to files?", "createIWXXM"),
#                 "No", "radio",
#                 ["Yes", "No"]),
        ]

        self._tafType = "Routine"

        # Process the GUI selections
        processVarList = ProcessVariableList.ProcessVariableList(
            "Terminal Aerodrome Forecast", varList, varDict={})

        # See if we made GUI selections
        selectionStatus = processVarList.status()

        # If the user cancelled the formatter
        if selectionStatus.lower() != "ok":
            return None  # User Cancelled - don't run the formatter

        # If we made it this far - return the GUI selections
        return processVarList.varDict()
    
    def _preProcessProduct(self, fcst, argDict):
        return fcst

    
    def _postProcessProduct(self, fcst, argDict):

        return fcst
    
    #--------------------------------------------------------------------------
    #----------------------------================------------------------------
    #--------------------------TAF-SPECIFIC METHODS----------------------------
    #----------------------------================------------------------------

    def _getInitialBaseTimeForGui(self, currentUtcTime):
        """Determines the TAF base time relative to current UTC hour."""

        if currentUtcTime.tm_hour in [23, 0, 1, 2, 3, 4]:
            baseTime = "00Z"
        elif currentUtcTime.tm_hour in [5, 6, 7, 8, 9, 10]:
            baseTime = "06Z"
        elif currentUtcTime.tm_hour in [11, 12, 13, 14, 15, 16]:
            baseTime = "12Z"
        else:
            baseTime = "18Z"

        return baseTime

    def _getInitialTafTypeForGui(self, currentUtcTime):
        """Determines the TAF Type based on the current UTC hour and minute."""

        # Assume "routine" TAFs to start
        tafType = "Routine"

        # If this is not the normal hour for "routine" TAF issuances
        if currentUtcTime.tm_hour not in [23, 5, 11, 17]:
            # Assume it is an amendment
            tafType = "Amendment"
        # Otherwise, check to see if these TAFs are "delayed"
        elif currentUtcTime.tm_min >= 40:
            # Assume these TAFs are delayed
            tafType = "Delayed"

        return tafType

    def _getVersion(self):
        """
        Gets the version by inspecting the documentation at the top of this
        file. This prevents things getting out of sync by having the version
        in a single location.
        """

        # lambda:0 is used instead of self so that we get the TAF
        # module (this file) instead of TAF_XXX. Even though
        # TAF_XXX is what is run, we want information about this file
        # because this is where the documentation containing the version is.
        module = inspect.getmodule(lambda:0)

        # Get the documentation at the top of this file
        tafDoc = inspect.getdoc(module)

        # Find where the version information is specified
        versionStart = tafDoc.find("Version:")
        versionEnd = tafDoc[versionStart:].find("\n")

        return tafDoc[versionStart:][:versionEnd]

    def _getConfigurationSettings(self, argDict):
        """Get Definition variables."""
        
        self.debug_print("\n" + "#" * 80 + "\n" + "Definition Dictionary:", 1)
        self._definition = argDict["forecastDef"]
        for (key, value) in self._definition.items():
            setattr(self, f"_{key}", value)

            if key in ["CACThresholds", "weatherRules", "areaOverrides"]:
                continue  # These will be displayed later for each airport
            else:
                self.debug_print("\n%s:" % key, 1)
                self.debug_print("\n\t%s"
                                 % (self._pformat(eval("self._" + key))), 1)

    def _getGuiSelections(self, argDict):
        """Make the GUI selected options available as variables."""

        self.debug_print("\n" + "#" * 80 + "\n" + "GUI Selections:", 1)
        varDict = argDict["varDict"]
        for (key, value) in varDict.items():
            if isinstance(key, tuple):
                label, variable = key
                setattr(self, f"_{variable}", value)

                self.debug_print("\n%s:" % repr(key), 1)
                self.debug_print("\n\t%s"
                                 % (self._pformat(eval("varDict[key]"))), 1)

    #--- ________________________________
    #--- AIRPORT-SPECIFIC METHODS

    def _getAirportStartEndTimes(self, airportIcaoId):
        """
        Determine the start and end time (in seconds) for this airport's TAF.
        """

        # Product issuance is "00Z", "06Z", "12Z", or "18Z" so start hour will
        # be 0, 6, 12, 18
        startHour = int(self._productIssuance[:2])

        # Determine the offset between the start hour and current hour
        # in seconds.

        # If the current UTC hour is after 06Z but the start hour is 0Z
        if (self._currentTimeUTC.tm_hour > 6) and (startHour == 0):
                # Start at 00Z from the next meteorological day
                secondsOffset = (24 - self._currentTimeUTC.tm_hour) * 3600
        else:
            # Adjust to the desired start hour
            secondsOffset = (startHour - self._currentTimeUTC.tm_hour) * 3600

        # Set the start time in seconds
        startTimeSeconds = self._currentTimeSeconds + secondsOffset

        # Define end time in seconds from base time of TAFs
        endTimeSeconds = startTimeSeconds + \
                           self._tafLength[airportIcaoId] * 3600

        # If this is an amendment, adjust the start time to the current time.
        # End hour is not adjusted because it only changes when the next
        # routine TAF is issued.
        if self._tafType == "Amendment":
            startTimeSeconds = self._currentTimeSeconds
            # If we are in the bottom half of the hour
            if self._currentTimeUTC.tm_min >= 29:
                # Start with the next hour instead
                startTimeSeconds += 3600

        return (startTimeSeconds, endTimeSeconds)

    def _preProcessAirport(self, argDict, editArea, airportIcaoId):
        """Gets and makes available airport TAF specific variables."""

        # Create the dictionary for storing airport specific product parts
        self._airportDicts[airportIcaoId] = OrderedDict()

        self._setAreaOverrides(airportIcaoId)

        self._determineTimeRanges(argDict, airportIcaoId)

        self._finalizeAnalysisList(argDict, editArea, airportIcaoId)

        self._createSampler(argDict, editArea, airportIcaoId)

        self._setFlightCategoryOrder(airportIcaoId)

        self._displayWeatherRulesInfo(airportIcaoId)

    def _setAreaOverrides(self, airportIcaoId):
        """
        This method goes through all the defined overrides for a particular
        airport and enables them. They will stay in effect until
        _revertAreaOverrides is called.
        """

        self.debug_print("\n" + "="*80 + "\n%s Area Overrides:"
                         % airportIcaoId, 1)

        self._oldAttributes = []
        areaOverrides = self._areaOverrides.get(airportIcaoId, [])

        for (attributeName, newValue) in areaOverrides:
            self.debug_print("\n%s:" % attributeName, 1)
            self.debug_print("\n\t%s" % (self._pformat(newValue)), 1)

            oldValue = getattr(self, attributeName)
            self._oldAttributes.append((attributeName, oldValue))

            # When overriding methods, the new method will be a string to
            # prevent errors. For example:
            # ("_createSampler", "_createSamplerKMIA")
            if type(newValue) is str and hasattr(self, newValue):
                # This is a method override
                newValue = eval("self." + newValue)

            setattr(self, attributeName, newValue)

    def _finalizeAnalysisList(self, argDict, editArea, airportIcaoId):
        """Finalize the analysis list to use for sampling the grids."""

#         self.debug_print("\n" + "="*80 + "\n%s Analysis List:"
#                          % airportIcaoId, 1)
        # Start off with the standard weather elements
        self._analysisList = self._getAnalysisList()

        if self._useAdditionalSkyElements:
            self._addOptionalToAnalysisList(argDict, editArea, airportIcaoId)

#         self.debug_print("\nFinal Analysis List:", 1)
#         for weatherElementInfo in self._analysisList:
#             self.debug_print("%s" % repr(weatherElementInfo), 1)

    def _addOptionalToAnalysisList(self, argDict, editArea, airportIcaoId):
        """ Get optional weather elements if they exist """
#         self.debug_print("\nChecking for existence of Optional weather elements", 1)

        # Save our existing debug settings because we want to temporarily turn
        # debug off when trying to check existence of weather elements because
        # if they don't exist, massive amounts of unhelpful debug is printed
        oldDebug = self._debug
        self._debug = 0
        oldStdOut = sys.stdout
        oldStdErr = sys.stderr

        # See if any of the optional weather elements are supported
        self._optionalWeatherElements = []
        for weatherElementInfo in self._getOptionalAnalysisList():
            weatherElement, samplingMethod = weatherElementInfo
            singleElementAnalysisList = [weatherElementInfo]
            sampleInfo = [(singleElementAnalysisList,
                              self._samplePeriods,
                              self._areaList)]
            try:
                # Temporarily turn off debug and print statements
                self._debug = 0
                # Try creating a sampler (getSampler) and sampling the weather
                # element (getStatList). If it fails, the weather element is
                # not defined                
                sampler = self.getSampler(argDict, sampleInfo)
                
                # Sample only this weather element and no others
                self.getStatList(sampler, singleElementAnalysisList,
                                 self._samplePeriods, editArea)

                # We didn't fail, so add this weather element sampling
                # information to the main analysis list
                self._analysisList.append(weatherElementInfo)
                self._optionalWeatherElements.append(weatherElement)
            except Exception as e:
                # Temporarily turn debug back on and print debug information
                self._debug = 1
                sys.stdout = oldStdOut
                sys.stderr = oldStdErr
                print(e.__class__, ":", e)

        # Restore debug settings
        self._debug = oldDebug
        sys.stdout = oldStdOut
        sys.stderr = oldStdErr

    def _createSampler(self, argDict, editArea, airportIcaoId):
        """Create a sampler to sample the data for this airport."""

        # The information needed to sample the data for this airport
        sampleInfo = [(self._analysisList,
                       self._samplePeriods,
                       self._areaList)]

        # Create the sampler for this product
        self._sampler = self.getSampler(argDict, sampleInfo)
        
        # Save off the edit area for when we actually sample the data
        self._editArea = editArea

    def _setFlightCategoryOrder(self, airportIcaoId):
        """Determines the order of the flight categories for this airport."""

        self.debug_print("\n" + "="*80 + "\n%s Flight Category Information:"
                         % airportIcaoId, 1)
        self.debug_print(" ", 1)

        #----------------------------------------------------------------------
        # Store information about each of the categories in order from the
        # worst category to the best category.
        self._orderedCategoryInfo = []

        # Store all the category information
        self._addFlightCategoryInfo(self._CACThresholds[airportIcaoId])

        # Sort first by visibility (index 2) then by ceiling (index 1)
        self._orderedCategoryInfo = sorted(self._orderedCategoryInfo,
                                           key=itemgetter(2, 1))

        # Always end with VFR
        self._orderedCategoryInfo.append(("V", None, None))

        self._displayOrderedFlightCategories()

    def _addFlightCategoryInfo(self, flightCategoryThresholds):
        """Store all the flight category information."""

        # Categories A - E are required
        for category in ["A", "B", "C", "D", "E"]:
            thresholds = flightCategoryThresholds[category]
            self.debug_print("processing category %s thresholds: %s"
                             % (category, thresholds), 1)

            (ceiling, visibility) = thresholds

            self._orderedCategoryInfo.append((category, ceiling, visibility))

        # F Categories are optional
        fCategoryThresholds = flightCategoryThresholds.get("F", [])
        for (index, thresholds) in enumerate(fCategoryThresholds):
            # Append the index to make each F threshold unique
            category = "F" + str(index)
            self.debug_print("processing category %s thresholds: %s"
                             % (category, thresholds), 1)

            (ceiling, visibility, windDirections) = thresholds[:3]

            self._orderedCategoryInfo.append((category, ceiling, visibility,
                                              windDirections))

    def _displayOrderedFlightCategories(self):
        self.debug_print(" ", 1)
        self.debug_print("Final Order:", 1)
        for categoryInfo in self._orderedCategoryInfo:
            if len(categoryInfo) == 3:
                self.debug_print("%s:\tvisibility: %sceiling: %s"
                                 % (categoryInfo[0],
                                    str(categoryInfo[2]).ljust(8),
                                    categoryInfo[1]), 1)
            else:
                self.debug_print(
                    "%s:\tvisibility: %sceiling: %swind directions: %s"
                    % (categoryInfo[0],
                       str(categoryInfo[2]).ljust(8),
                       str(categoryInfo[1]).ljust(8),
                       categoryInfo[3]), 1)

    def _displayWeatherRulesInfo(self, airportIcaoId):
        """Display the weather rules in a nice format."""

        self.debug_print("\n" + "="*80 + "\n%s Weather Rules:"
                         % airportIcaoId, 1)

        def sortKeys(keys):
            keys = list(keys)

            # Remove "default" so collection is homogeneous
            contains_default = "default" in keys
            if contains_default:
                keys.remove("default")

            keys = sorted(keys)

            # Make sure default is always listed last
            if contains_default:
                keys.append("default")

            return keys

        gfeWeatherTypes = sortKeys(self._weatherRules.keys())
        for weatherType in gfeWeatherTypes:
            self.debug_print("\n\t%s:" % repr(weatherType), 1)

            occurrenceTimeRanges = sortKeys(self._weatherRules[weatherType].keys())
            for occurrenceRange in occurrenceTimeRanges:
                self.debug_print("\t%s:" % repr(occurrenceRange), 1)

                probabilities = sortKeys(self._weatherRules[weatherType][occurrenceRange].keys())
                for probability in probabilities:
                    classifier = self._weatherRules[weatherType][occurrenceRange][probability]
                    self.debug_print("\t\t%s%s"
                                     % ((repr(probability) + ":").ljust(12),
                                        repr(classifier)), 1)

    def _postProcessAirport(self, argDict, airportIcaoId):

        self._revertAreaOverrides(airportIcaoId)
        
    # Fetch the indices where the keeper live
    def _calcKeeperIndices(self, keeperFMGroups, allFMGroups):
        
        keeperIndices = []  # Filled with lists
        
        for fm in keeperFMGroups:
            fmIndex = allFMGroups.index(fm)
            keeperIndices.append(fmIndex)
        
        return keeperIndices
    
    # Calculates the average of the scalar data list
    def _scalarAverage(self, valueList):
        sum = 0.0
        for v in valueList:
            sum = sum + v
        return sum / len(valueList)
    
    # Calculate the vector average using the (dir, speed) pairs in valueList.
    # In some cases the direction is "VRB" so that case is accounted for and
    # thus a separate count of direction values is needed. 
    def _vectorAverage(self, valueList):
        uSum = 0.0
        vSum = 0.0
        speedSum = 0.0
        dirCount = 0
        for windDir, windSpeed in valueList:
            speedSum = speedSum + windSpeed
            if windDir == "VRB":  # skip VRBs
                continue
            # get the u and v for the unit vector to remove influence from speed
            u, v = self.MagDirToUV(1.0, windDir) 
            uSum = uSum + u
            vSum = vSum + v
            dirCount = dirCount + 1  # keep a separate count of direction values
        speedAvg = speedSum / len(valueList)
        if speedAvg > self._maxLightWindSpeed:
            speedAvg = self.round(speedAvg, "Nearest", 5)
        if dirCount > 0:
            uAvg = uSum / dirCount
            vAvg = vSum / dirCount
            unitSpeed, dirAvg = self.UVToMagDir(uAvg, vAvg)
            dirAvg = self.round(dirAvg, "Nearest", 10)
        else:
            dirAvg = "VRB"
        
        if speedAvg < 1.0:
            speedAvg = 0
            dirAvg = 0
        elif speedAvg <= self._variableWindSpeed:
            dirAvg = "VRB"
        
        return dirAvg, speedAvg 
        
    def _averageKeeperGroups(self, keeperFMGroups, allFMGroups):
        
        elementList = ["wind", "windGust", "sky"]

        # FInd the indices where the keepers live.
        keeperIndices = self._calcKeeperIndices(keeperFMGroups, allFMGroups)
        
        for i in range(len(keeperIndices)):
            startIndex = keeperIndices[i]
            if i < len(keeperIndices) - 1:
                endIndex = keeperIndices[i + 1]
            else:
                endIndex = len(allFMGroups)
            
            if startIndex + 1 == endIndex:  # consecutive FMGroups. No averaging needed.
                continue
                        
            for we in elementList:
                valueList = []
                # Collect the values for this element
                for i in range(startIndex, endIndex):
                    value = allFMGroups[i].productDict[we]
                    if value is not None:
                        valueList.append(value)
                # All values could be None
                if len(valueList) == 0:
                    continue
                
                 # Calculate the average based on we type and poke in the average value
                if we == "wind":
                    allFMGroups[startIndex].productDict[we] = self._vectorAverage(valueList)
                    # print "+&... " + we + " Average: " + str(self._vectorAverage(valueList))
                else:
                    allFMGroups[startIndex].productDict[we] = self._scalarAverage(valueList)
#                     print "... " + we + " Average: " + str(self._scalarAverage(valueList))

            # Make sure windGust is significantly higher than windSpeed
            if allFMGroups[startIndex].productDict["windGust"] < \
              allFMGroups[startIndex].productDict["wind"][1] + self._minGustSpeedDifference:
                allFMGroups[startIndex].productDict["windGust"] = None
                
        return

    def _revertAreaOverrides(self, airportIcaoId):
        """
        This method goes through all the defined overrides for a particular
        airport and reverts them back to the default values.
        """
        if airportIcaoId in self._areaOverrides:
            for (attributeName, oldValue) in self._oldAttributes:
                setattr(self, attributeName, oldValue)

    #--------------------------------------------------------------------------
    #-------------------------------===========--------------------------------
    #------------------------------PRODUCT-PARTS-------------------------------
    #-------------------------------===========--------------------------------

    def _createSharedProductParts(self):
        self.debug_print("\n" + "#" * 80 + 
                         "\nShared (Not Airport Specific) Product Parts:", 1)
        for productPart in self._bulletinHeaderParts():
            self._productPart = productPart
            method = getattr(self, f"_{productPart}")
            method(self._productDict, productPart)

    def _createAirportProductParts(self, airportIcaoId):
        for productPart in self._airportTafParts():
            
#             print "::::Exec string for " + productPart, "self._" + productPart + \
#                  "(self._airportDicts[airportIcaoId]," + \
#                  " productPart, airportIcaoId)"
            method = getattr(self, f"_{productPart}")
            method(self._airportDicts[airportIcaoId], productPart, airportIcaoId)   

        # Before trying to shorten the TAF, save a copy of all the FM groups
        fmGroups = self._airportDicts[airportIcaoId]["fmGroups"]
        
        if len(fmGroups) == 0:
            print("No FMGroups found in self._airportDicts.")
            return
        
        
        allFmGroups = fmGroups[:]
        print("All FMGroups: " + str(len(fmGroups)))
        # Try to make the TAF have as few FM groups as possible while still
        # trying to have the TAF be as meteorologically accurate as possible
        # and showing all the operationally significant information
        shorteningAlgorithms = self.ShorteningAlgorithms(self, airportIcaoId)
        keeperFMGroups = shorteningAlgorithms.shortenFmGroups(fmGroups)

        # Average the values for each keeper using the subsequent fmGroups in time.
        # Updates are made to just the keeper FMGroups in place in the productDict.
        # Note this has been commented out for now as forecasters prefer a more
        # representative values for the FMGroup.
        # self._averageKeeperGroups(keeperFMGroups, allFmGroups)
                
        self._airportDicts[airportIcaoId]["fmGroups"] = keeperFMGroups

        self._finalizeTemposProbs(keeperFMGroups, allFmGroups, airportIcaoId)

        self._displayShortenedTafInfo(keeperFMGroups)
        
        return

    def _setProductPart(self, value):
        self._productDict[self._productPart] = value

        if isinstance(value, time.struct_time):
            value = time.asctime(value)  # Create a nicely formatted time string

        self.debug_print("\n%s = %s" % (self._productPart, repr(value)), 1)

    #--- ___________________________
    #--- BULLETIN HEADER PARTS
    def _bulletinHeaderParts(self):
        """Product Parts for header info (ie. FTUS42 KMFL 141100 AAA)."""
        return [
                "bulletinType",  # FT
                "bulletinLocation",  # US
                "bulletinDifferentiator",  # 42
                "icaoOfficeIndicator",  # KMFL
                "issuanceTime",  # 141100
                "tafTypeIdentifier",  # AAA
               ]

    def _bulletinType(self, productDict, productPart):
        # TODO: Couldn't this change in theory if valid period < 24 hours?
        self._setProductPart(self._wmoID[0:2])

    def _bulletinLocation(self, productDict, productPart):
        self._setProductPart(self._wmoID[2:4])

    def _bulletinDifferentiator(self, productDict, productPart):
        self._setProductPart(self._wmoID[4:6])

    def _icaoOfficeIndicator(self, productDict, productPart):
        self._setProductPart(self._fullStationID)

    def _issuanceTime(self, productDict, productPart):
        self._setProductPart(self._currentTimeUTC)

    def _tafTypeIdentifier(self, productDict, productPart):
        typeIdentifier = ""  # Default to routine
        
        if self._tafType == "Amendment":
            typeIdentifier = "AAX"
        elif self._tafType == "Delayed":
            typeIdentifier = "RRX"
        elif self._tafType == "Corrected":
            typeIdentifier = "CCX"

        # If we are producing "verification" TAFs
        if self._verificationHeaders:
            # Fix the type identifier so there is no "X"
            # TODO: Look at past issued TAF to determine A - Z?
            typeIdentifier = typeIdentifier.replace("X", "A")

        self._setProductPart(typeIdentifier)

    #--- _________________
    #--- AIRPORT PARTS
    def _airportTafParts(self):
        """
        Product Parts for creating a particular airport TAF.
        
        
        For Example (TAC format):
        TAFMIA                    <- Only for verification TAFs
        TAF AMD
        KMIA 122229Z 1223/1318 ...first FM group...
        ...other FM groups...
        AMD NOT SKED=
        """
        return [
                "airportHeader",  # 'TAFFLL' (Only for verification TAFs)
                "tafTypeHeader",  # 'TAF AMD'
                "icaoAirportIndicator",  # 'KFLL'
                "preparationTime",  # Preparation time (seconds since epoch)
                "validPeriodStart",  # Start time (seconds since epoch)
                "validPeriodEnd",  # End time (seconds since epoch)
                "fmGroups",  # List of FM groups for this airport
                "airportDisclaimer",  # 'AMD NOT SKED'
               ]

    def _airportHeader(self, productDict, productPart, airportIcaoId):
        productDict[productPart] = "TAF" + airportIcaoId[1:]

    def _tafTypeHeader(self, productDict, productPart, airportIcaoId):
        # If this is an amendment
        if self._tafType == "Amendment":
            productDict[productPart] = "TAF AMD"
        elif self._tafType == "Corrected":
            productDict[productPart] = "TAF COR"
        elif self._tafType == "Delayed":
            # TODO: Change to "RTD" when AvnFPS is updated
            productDict[productPart] = "TAF"
        else:
            productDict[productPart] = "TAF"

    def _icaoAirportIndicator(self, productDict, productPart, airportIcaoId):
        productDict[productPart] = airportIcaoId

    def _preparationTime(self, productDict, productPart, airportIcaoId):
        productDict[productPart] = self._currentTimeSeconds

    def _validPeriodStart(self, productDict, productPart, airportIcaoId):
        productDict[productPart] = self._startTimeSeconds

    def _validPeriodEnd(self, productDict, productPart, airportIcaoId):
        productDict[productPart] = self._endTimeSeconds

    def _fmGroups(self, productDict, productPart, airportIcaoId):
        """
        Create all the forecast periods for this site based off the
        data in the grids.
        """

        # Get the hourly statistics for this area
        self.debug_print("\n" + "="*80 + "\n%s Sample Analysis Information:"
                         % airportIcaoId, 1)
        self.debug_print("\n" + "ProductPart: " + productPart + " Airport:", airportIcaoId)
        statLists = self.getStatList(self._sampler, self._analysisList,
                                     self._samplePeriods, self._editArea)
#         self.debug_print("\n" + "?"*80 + "\n%s Analysis Lists:"
#                          % self._analysisList, 1)

        # Create the list of FM groups
        fmGroups = []
        for index, (timeRange, fmGroupLabel) in enumerate(self._samplePeriods):
            self._periodIndex = index
            self._period = index + 1
            statList = statLists[index]

            invalidPeriod = False
            problemsFound = False

            self.debug_print("\n" + "_"*80, 1)

            someDataPresent = any([stat is not None
                                   for stat in statList.values()])
            if someDataPresent:
                fmGroup = self.FmGroup(self, statList,
                                       self._productDict,
                                       self._airportDicts[airportIcaoId],
                                       fmGroupLabel,
                                       self._period)

                problemsFound = fmGroup.createProductParts()

                if not problemsFound:
                    self.debug_print("\nFinal %s (Period %s) product parts:"
                                     % (fmGroupLabel, self._period), 1)
                    for (part, value) in fmGroup.productDict.items():
                        self.debug_print("%s = %s" % (part, repr(value)), 1)

                    fmGroups.append(fmGroup)

            if not someDataPresent:
                invalidPeriod = True
                self.debug_print("\nNo statistics for %s (Period %s)"
                                 % (fmGroupLabel, self._period), 1)
            elif problemsFound:
                invalidPeriod = True
                self.debug_print("\nFailed to create %s (Period %s)"
                                 % (fmGroupLabel, self._period), 1)

            if invalidPeriod and self._period == 1:
                self.debug_print("\nInvalid airport TAF: " + 
                    "First period in valid period time range always required.",
                    1)
                productDict[productPart] = list()
                return

        productDict[productPart] = fmGroups

    def _airportDisclaimer(self, productDict, productPart, airportIcaoId):
        # Check if this airport has a disclaimer for this issuance time

        productDict[productPart] = None  # Start off assuming no disclaimer
        startHour = time.gmtime(self._startTimeSeconds).tm_hour

        if airportIcaoId in self._disclaimer:
            disclaimer = self.nlValue(self._disclaimer[airportIcaoId],
                                      startHour)

            productDict[productPart] = disclaimer.strip()

    #--- ___________________

    class GenericGroup():
        def __init__(self, textProduct, statList, sharedDict, airportDict):
            self._textProduct = textProduct
            self._statList = statList

            # The shared product parts (not specific to airports)
            self._sharedDict = sharedDict

            # The airport specific product parts
            self._airportDict = airportDict

            # The group specific product parts for this FM/TEMPO/PROB30 group
            self.productDict = OrderedDict()

        def __repr__(self):
            return "%s %s group %s" % \
                    (self._airportDict["icaoAirportIndicator"],
                     self.productDict["groupClassification"],
                     self.productDict["groupLabel"])

        def __str__(self):
            return self.__repr__()

        def _getData(self, statName, newType,
                     required=False, nullValue=None):
            self._textProduct.debug_print(
                "\n" + "_"*40 + "\n%s Grid Information%s:"
                % (statName, " (Required)" if required else ""), 1)

            # check to see if the stat exists first
            if statName in self._statList:
                data = self._statList[statName]
            else:  # the element was likely not configured
                data = None
            # self._textProduct.debug_print("\n " + statName + "????Data:" + str(data), 1)
            # Ignore the no data error if the element is not required
            if (data is None) or (data == nullValue):
                raise TAF_DataException("No data available for this period",
                                            required)
            else:
                # Return the data as the appropriate data type
                try:
                    if isinstance(data, tuple):
                        self._textProduct.debug_print("Value = (%s, %s):" % 
                                                      (data[0], data[1]), 1)
                        self._textProduct.debug_print(
                            "Converting (%s, %s) to (%s, %s)" % 
                            (type(data[0]), type(data[1]),
                             newType[0], newType[1]), 1)

                        data = (eval("newType[0](data[0])"),
                                eval("newType[1](data[1])"))

                        self._textProduct.debug_print("New value = (%s, %s)" % 
                                                      (data[0], data[1]), 1)
                    else:
                        self._textProduct.debug_print("Value = %s:" % data, 1)
                        self._textProduct.debug_print("Converting %s to %s" % 
                                                      (type(data), newType), 1)

                        data = eval("newType(data)")

                        self._textProduct.debug_print(
                            "New value = %s" % data, 1)
                except:
                    raise TAF_DataException("Failed to convert %s" % statName,
                                            required)

            return data

        def _getOptionalData(self, weatherElementName, newType, alternateWeatherElementName):
            optionalWeatherElements = \
                self._textProduct._optionalWeatherElements

            self._textProduct.debug_print(
                "Getting data for AWT weather element %s"
                % weatherElementName, 1)
            if weatherElementName in optionalWeatherElements:
                try:
                    return self._getData(weatherElementName, newType)
                except:
                    self._textProduct.debug_print(
                        "\tNo data available for %s. Using %s instead."
                        % (weatherElementName, alternateWeatherElementName), 1)
                    return self._getData(alternateWeatherElementName, int)
            else:
                self._textProduct.debug_print(
                    "\t%s does not exist. Using %s instead."
                    % (weatherElementName, alternateWeatherElementName), 1)
                return self._getData(alternateWeatherElementName, int)

        def _roundVisibilityToReportableValue(self, numericalVisibility):
            """
            Round visibility to a reportable value in a way that is consistent
            with the tools.
            """

            if numericalVisibility is None:
                return None

            # TODO: This logic, while consistent with tools, differs from the
            #       directive.

            # If visibility < 1, then round it to the nearest 0.25
            if numericalVisibility < 1:
                numericalVisibility = self._textProduct.round(numericalVisibility, "Nearest", 0.25)

            # If 1 <= visibility < 2.5, then round it to the nearest 0.5
            elif 1 <= numericalVisibility < 2.5:
                numericalVisibility = self._textProduct.round(numericalVisibility, "Nearest", 0.5)

            # Otherwise, round it to the nearest whole number
            else:
                numericalVisibility = self._textProduct.round(numericalVisibility, "Nearest", 1)

            return numericalVisibility

        def _determineWeather(self, groupType, numericalVisibility):
            groupWeather = self._textProduct.GroupWeatherInfo(
                self._textProduct, groupType, numericalVisibility)

            weatherSubKeys = self._getData("Wx", list)

            self._textProduct.debug_print(
                "\n--- Looking for %s group weather ---" % groupType, 1)
            self._textProduct.debug_print("%s grid = %s" % 
                ("Visibility" if groupType == "FM"
                 else "VisibilityConditional", numericalVisibility), 1)

            # A weatherSubKey is a WeatherSubKey object that represents
            # information about a single weather type in a Wx grid; a single
            # Wx grid may have multiple weather types present and therefore
            # multiple weather subkeys. Each weather subkey in a Wx grid will
            # be processed one at a time.
            for weatherSubKey in weatherSubKeys:
                # The WeatherSubKey can be converted to the corresponding
                # weather ugly string in a generic way (working on all AWIPS
                # versions) by calling repr on it.
                weatherUglyString = repr(weatherSubKey)

                # TODO: Display nice description of ugly string?
                self._textProduct.debug_print(
                    "\nWorking on %s" % weatherUglyString, 1)

                try:
                    weatherInfo = self._textProduct.WeatherUglyStringInfo(
                        self._textProduct, weatherUglyString,
                        numericalVisibility, groupType)

                except TAF_DataException as e:
                    self._textProduct.debug_print(e, 1)
                    self._textProduct.debug_print("Skipping this weather: " + str(weatherSubKey), 1)
                    continue

                self._textProduct.debug_print(
                    "\n\tTAF type = %s" % weatherInfo.tafWeatherType, 1)
                self._textProduct.debug_print(
                    "weatherRules classifier = %s"
                    % weatherInfo.classifier, 1)

#                 weatherIsForFmGroup = (groupType == "FM") and \
#                                       ("VC" in weatherInfo.classifier or
#                                        weatherInfo.classifier == "PREVAIL")
                weatherIsForFmGroup = (groupType == "FM") and \
                                      ("VC" in weatherInfo.classifier or
                                       weatherInfo.classifier == "PREVAIL")

                weatherIsForCurrentGroup = weatherIsForFmGroup or \
                    (groupType == weatherInfo.classifier)

                if not weatherIsForCurrentGroup:
                    self._textProduct.debug_print(
                        "Skipping weather: not for %s group" % groupType, 1)
                    continue
                # Add the weather information to this group
                groupWeather.updateWeather(weatherInfo)
                
            return groupWeather

        def _finalizeWeather(self, productPart, fmWeather):
            weather = self.productDict[productPart]

            # If this is a conditional group, repeat any necessary FM group
            # weather
            if weather.groupType == "TEMPO" or weather.groupType == "PROB30":
                weather.addAnyRepeatingFmWeather(fmWeather)

            # Consolidate and remove duplicates while preserving insertion
            # order (unless the directives specify to reorder certain weather)
            weather.postProcessWeatherTypes()

            self._textProduct.debug_print("\nGroup Weather Information:", 1)
            self._textProduct.debug_print("maxIntensity = %s"
                % weather.maxIntensity, 1)
            self._textProduct.debug_print("regularWeatherTypes = %s"
                % weather.regularWeatherTypes, 1)
            self._textProduct.debug_print("obstructionWeatherTypes = %s"
                % weather.obstructionWeatherTypes, 1)
            self._textProduct.debug_print("vicinityWeather = %s"
                % weather.vicinityWeather, 1)

            if weather.weatherExists():
                self.productDict[productPart] = weather
            else:
                self.productDict[productPart] = None

        def _roundCloudHeight(self, cloudHeight):
            """
            Ensures cloud heights (in 100s of feet) are reportable.
            """

            # Do not permit null or negative ceiling heights
            if (cloudHeight is None) or (cloudHeight < 0):
                return None

            # If we are using detailed cloud heights, use logic from directives
            if self._textProduct._useDetailedCloudHeights:
                if cloudHeight < 30:  # Clouds < 3000 feet
                    # Round to nearest 100 feet
                    return int(self._textProduct.round(cloudHeight,
                                                       "Nearest", 1))

                elif cloudHeight < 50:  # Clouds 3,000 - 5,000 feet
                    # Round to nearest 500 feet
                    return int(self._textProduct.round(cloudHeight,
                                                       "Nearest", 5))

                else:  # Clouds >= 5,000 feet
                    # Round to nearest 1000 feet
                    return int(self._textProduct.round(cloudHeight,
                                                       "Nearest", 10))

            # Otherwise, report categorical cloud heights
            # TODO: Where do categorical heights come from? Should they be
            #       configurable? Why are they the default instead of
            #       directives logic above?
            else:
                if cloudHeight < 2:  # Handle 100 feet clouds
                    return 1  # 100 feet

                elif 2 <= cloudHeight < 6:  # Clouds 200-500 feet
                    return 3  # 300 feet

                elif 6 <= cloudHeight < 10:  # Clouds 600-900 feet
                    return 8  # 800 feet

                elif cloudHeight <= 50:  # Clouds 1,000-5,000 feet
                    # Round to nearest 500 feet
                    return int(self._textProduct.round(cloudHeight,
                                                       "Nearest", 5))

                elif cloudHeight <= 150:  # Clouds 6,000-15,000 feet
                    # Round to nearest 1000 feet
                    return int(self._textProduct.round(cloudHeight,
                                                       "Nearest", 10))

                else:  # Clouds > 15,000 feet
                    # Round to nearest 5000 feet
                    return int(self._textProduct.round(cloudHeight,
                                                       "Nearest", 50))

        def _convertFeetTo100sFeet(self, value):
            """
            Convert a value in units of feet to units of 100s of feet while
            preserving the values type.
            """
            originalType = type(value)

            return originalType(value / 100.0)
        
        # Translates a Sky value (0-100) to coverage string, e.g., "BKN"
        def _skyPctToCov(self, skyValue):
            
            if skyValue is None:
                return None
            
            # Initialize to the lowest value
            firstKey = list(self._textProduct._maxSkyPercentages.keys())[0]
            finalCov = self._textProduct._maxSkyPercentages[firstKey]
            
            for cov in self._textProduct._maxSkyPercentages:
                if skyValue <= self._textProduct._maxSkyPercentages[cov]:
                    return cov
                else:
                    finalCov = cov
            
            return finalCov
        
        # Uses _skyPctToCov to return one category lower than the skyValue indicates
        def _oneSkyCatLowerThan(self, skyCat):
           
           if skyCat in ["OVC", "BKN"]:
               return "SCT"
           elif skyCat in ["SCT", "FEW"]:
               return "FEW"
           
           return None 
                       
        # Sort method for cloud layers. Only looks at the base height
        def _cloudLayerSort(self, a):
            _, height = a
            return height

    class FmGroup(GenericGroup):
        def __init__(self, textProduct, statList, sharedDict, airportDict,
                     label, period):
            TextProduct.GenericGroup.__init__(self,
                                              textProduct,
                                              statList,
                                              sharedDict, airportDict)

            # The label (ie. "FM121800" for a FM group on the 12th day of the
            # month at 18Z)
            self._label = label

            # The period (1-based) of this FM group. This never changes so if
            # there are a total of 24 periods (before shortening) and this is
            # the 5th period (4 hours after the issuance time) and periods 2-4
            # get removed during shortening, this is still the 5th period in
            # the valid period time range even though it will now be the 2nd
            # FM group in the output.
            self.period = period

            # The number of hours since the issuance time.
            self.hourOffset = period - 1

            # The significance (0 - 100) of each product part in this FM group.
            self.ratings = OrderedDict()

            # The number of changes to the fields that make up a FM group.
            self.numChanges = 0

            # Information for each field about whether it changed or not.
            self.fieldChangeInfo = OrderedDict()
            
            self.rankingValue = 0
            self.rankDict = {}

        def __repr__(self):
            # Add the period to the information displayed
            return TextProduct.GenericGroup.__repr__(self) + \
                   " (Period %s)" % self.productDict["fmPeriod"]

        @staticmethod
        def productParts():
            """Product Parts for creating a particular FM group."""
            return [
                    "groupClassification",
                    "groupType",
                    "groupLabel",
                    "fmPeriod",
                    "startTime",
                    "endTime",
                    "wind",
                    "windGust",
                    "visibility",
                    "weather",
                    "sky",
                    "skyPrimary",  # optional product part
                    "skySecondary",  # optional product part
                    "skyTertiary",  # optional product part
                    "cloudBasePrimary",
                    "cloudBaseSecondary",  # optional product part
                    "cloudBaseTertiary",  # optional product part
                    "ceiling",
                    "llws",
                    "pop",
                    "clouds",
                    "flightCategory",
                    "conditionalGroup",
                    "ratings",  # Rates the significance of each of
                                          # the product parts

                    # "numChanges",

                    # "similarChecks",
                   ]

        def createProductParts(self):
            self._textProduct.debug_print(
                "Gathering statistics for %s (%s Period %s)"
                 % (self._label, self._airportDict["icaoAirportIndicator"],
                    self.period), 1)
            self._textProduct.debug_print("____________________________________________________________________")
            
            problemsFound = False
            for productPart in self.productParts():
                print("FMGroups productPart: " + productPart)
                try:
                    method = getattr(self, f"_{productPart}")
                    method(productPart)
                except Exception as e:
                    self._textProduct.debug_print(
                        "\n" + traceback.format_exc().strip(), 1)

                    if isinstance(e, TAF_DataException):
                        if not e.requiredData:
                            # The issue is with data that isn't required so
                            # ignore it and just set the product part to None
                            self.productDict[productPart] = None
                            self._textProduct.debug_print("Ignoring issue: " + 
                                "%s is not required (setting to None)"
                                % productPart, 1)
                        else:
                            problemsFound = True
                    else:
                        problemsFound = True

            return problemsFound

        def _groupClassification(self, productPart):
            self.productDict[productPart] = "FROM"

        def _groupType(self, productPart):
            self.productDict[productPart] = "FM"

        def _groupLabel(self, productPart):
            self.productDict[productPart] = self._label

        def _fmPeriod(self, productPart):
            self.productDict[productPart] = self.period

        def _startTime(self, productPart):
            if self.hourOffset == 0:
                startTimeSeconds = self._airportDict["preparationTime"]
            else:
                startTimeSeconds = \
                    self._textProduct._allPeriodStartEndTimes[self.hourOffset][0]

            self.productDict[productPart] = startTimeSeconds

        def _endTime(self, productPart):
            endTimeSeconds = \
                self._textProduct._allPeriodStartEndTimes[self.hourOffset][1]

            self.productDict[productPart] = endTimeSeconds

        def _wind(self, productPart):
            """Get the prevailing wind direction and wind speed."""

            # Speed in knots.
            # Direction in degrees (rounded to nearest 10 degrees).
            (speed, direction) = self._getData("Wind", (int, int),
                                               required=True)
            direction = int(self._textProduct.round(direction, "Nearest", 10))

            # Fix true north winds if needed
            if direction == 0 and speed > self._textProduct._calmWindSpeed:
                direction = 360

            # If the winds are "calm", then show everything as 0
            if speed <= self._textProduct._calmWindSpeed:
                direction = 0
                speed = 0

            # All wind speeds that are not "calm" and <= maximum permissible
            # speed for "variable" directions
            elif speed <= self._textProduct._variableWindSpeed:
                direction = "VRB"

            self.productDict[productPart] = (direction, speed)

        def _windGust(self, productPart):
            (windDirection, windSpeed) = self.productDict["wind"]
            windGust = self._getData("WindGust", int)
            # If we should not report a gust - either because the wind gust
            # stats are missing or sustained wind is less than the defined
            # minimum threshold for reporting gusts or gust speed does not
            # exceed sustained wind by the minimum required amount
            if (windGust is None or
                windSpeed < self._textProduct._minWindToReportGusts or
                ((windGust - windSpeed) < self._textProduct._minGustSpeedDifference)):
                print("REJECTING WIND GUST VALUE:" + str(windGust) + " WindSpeed: " + str(windSpeed))
                self.productDict[productPart] = None
            else:
                self.productDict[productPart] = windGust

        def _visibility(self, productPart):
            """Get the prevailing numerical visibility."""
            # TODO: Add Vertical Visibility support?
            # TODO: Ensure FM and TEMPO/PROB visibilities never identical

            visibility = self._getData("Visibility", float, required=True)
            visibility = self._roundVisibilityToReportableValue(visibility)

            self.productDict[productPart] = visibility

        def _weather(self, productPart):
            self.productDict[productPart] = \
                self._determineWeather("FM", self.productDict["visibility"])

            self._finalizeWeather("weather", self.productDict["weather"])

        def _sky(self, productPart):
            # Cloud coverage (in percentage)
            self.productDict[productPart] = self._getData("Sky", int)

#         def _skyPrimary(self, productPart):
#             # skyPrimary product part
#             # Cloud coverage (in percentage)
#             self.productDict[productPart] = self._getOptionalData("SkyPrimary", int, "Sky")
# 
#         def _skySecondary(self, productPart):
#             # skySecondary product part
#             # Cloud coverage (in percentage)
#             data = self._getOptionalData("SkySecondary", int, "Sky")
#             
#             self.productDict[productPart] = self._getOptionalData("SkySecondary", int, "Sky")

        def _skyPrimary(self, productPart):
            # skyPrimary product part
            self.productDict[productPart] = self._getData("SkyPrimary", int)

        def _skySecondary(self, productPart):
            # skySecondary product part
            self.productDict[productPart] = self._getData("SkySecondary", int)

        def _skyTertiary(self, productPart):
            # skyTertiary product part
            self.productDict[productPart] = self._getData("SkyTertiary", int)

        def _cloudBasePrimary(self, productPart):
            # Predominant cloud height (in 100s feet)
            cloudBasePrimary = self._getData("CloudBasePrimary", int,
                                             nullValue=0)

            self.productDict[productPart] = \
                self._roundCloudHeight(cloudBasePrimary)

        def _cloudBaseSecondary(self, productPart):
            # Secondary predominant cloud height (in 100s feet)
            cloudBaseSecondary = self._getData("CloudBaseSecondary", int,
                                               nullValue=0)

            self.productDict[productPart] = \
                self._roundCloudHeight(cloudBaseSecondary)

        def _cloudBaseTertiary(self, productPart):
            # Secondary predominant cloud height (in 100s feet)
            cloudBaseTertiary = self._getData("CloudBaseTertiary", int,
                                               nullValue=0)

            self.productDict[productPart] = \
                self._roundCloudHeight(cloudBaseTertiary)

        def _ceiling(self, productPart):
            # Ceiling cloud height (in feet but converted to 100s feet)
            ceiling = self._getData("Ceiling", int)
            ceiling = self._convertFeetTo100sFeet(ceiling)

            # The Ceiling grid allows negative values that count as the null
            # value
            self.productDict[productPart] = self._roundCloudHeight(ceiling)

        def _llws(self, productPart):
            """
            Get the prevailing low-level wind shear direction, speed and
            height.
            """
            # Speed in knots (rounded to nearest 5 knots).
            # Direction in degrees (rounded to nearest 10 degrees).
            (speed, direction) = self._getData("LLWS", (int, int))
            outStr = productPart + " speed:: " + str(speed) + " direction:: " + str(direction)
            self._textProduct.debug_print("\n" + "?"*40 + outStr, 1)

            
            speed = int(self._textProduct.round(speed, "Nearest", 5))
            direction = int(self._textProduct.round(direction, "Nearest", 10))
            
            # Height in 100s of feet (No rounding done)
            height = self._getData("LLWSHgt", int)

            # A zero speed or zero height indicates no LLWS.
            if speed == 0 or height == 0:
                self.productDict[productPart] = None
            else:
                self.productDict[productPart] = (direction, speed, height)

        def _pop(self, productPart):
            """
            Get the prevailing probability of precipitation. This is not shown
            in the TAC output format but can be used to help determine the
            meteorological significance of this period.
            """

            # PoP in percentage (rounded to nearest 10 percent).
            pop = self._getData("PoP", int)
            pop = int(self._textProduct.round(pop, "Nearest", 10))

            self.productDict[productPart] = pop
        
        # Prevailing clouds
        def _clouds(self, productPart):
            
            # Returns True if all cloud coverages and bases are valid
            def allCloudsValid(cloudList):
                for cov, base in cloudList:
                    if cov is None or base is None:
                        return False
                    
                return True
            
            # Filters out None bases and SKC coverages and returns the remainder
            def filteredCloudList(cloudList):
                finalCloudList = []
                for cov, base in cloudList:
                    if base is not None:
                        if cov != "SKC":
                            finalCloudList.append((cov, base))
                            
                return finalCloudList
                
            # First check the ceiling for 0 value and set to "VV000" if so.
            ceiling = self.productDict["ceiling"]
            if ceiling == 0:
                self.productDict[productPart] = "VV000"
                return
            
            # Fetch the cloud base values
            cloudBase1 = self.productDict["cloudBasePrimary"]
            cloudBase2 = self.productDict["cloudBaseSecondary"]
            cloudBase3 = self.productDict["cloudBaseTertiary"]
                        
            # Fetch the sky coverage values
            sky = self.productDict["sky"]
            sky1 = self.productDict["skyPrimary"] 
            sky2 = self.productDict["skySecondary"]
            sky3 = self.productDict["skyTertiary"]
                            
            print("Dump-- Sky: " + str(sky) + " sky1: " + str(sky1) + " sky2: " + str(sky2) + " sky3: " + str(sky3))
            
            # Convert sky data to Coverage strings
            skyCov = self._skyPctToCov(sky)
            sky1Cov = self._skyPctToCov(sky1)
            sky2Cov = self._skyPctToCov(sky2)
            sky3Cov = self._skyPctToCov(sky3)
            
            # Make a draft of the cloud list 
            rawCloudList = [(sky1Cov, cloudBase1), (sky2Cov, cloudBase2), (sky3Cov, cloudBase3)]
            print("RawCloudList: " + str(rawCloudList))

            cloudList = filteredCloudList(rawCloudList) # Filter out None bases and SKC layers
            print("Filtered CloudList: " + str(cloudList))
            
            if len(cloudList) == 0:  # No valid clouds, so report clear skies
                self.productDict[productPart] = "SKC"
                print("Final cloud list: SKC")
                return
            
            # If all the clouds are valid (cov, base) we can set the product part and return
            if allCloudsValid(cloudList):
                self.productDict[productPart] = cloudList
                print("Final cloud list: " + str(cloudList))
                return
            
            # Distribute the cloud coverage over all layers            
            cloudList.sort(key=self._cloudLayerSort)
            cloudList.reverse()  # sort highest to lowest by cloud height
            print("Reversed CloudList: " + str(cloudList))
            
            # Initialize the coverage from Sky which will decrement as we move down in layers
            currentCov = skyCov
            finalCloudList = []
            for cov, base in cloudList:
                if currentCov == "SKC":  # Stop if coverage has decremented to SKC
                    break
                print("cov: " + str(cov) + " base: " + str(base) + " CurrentCov: " + str(currentCov))
                finalCloudList.append((currentCov, base))
                print("Assigned: " + str((currentCov, base)))
                currentCov = self._oneSkyCatLowerThan(currentCov)
            
            # No layers here means a clear sky 
            if len(finalCloudList) == 0:
                print("Empty cloudList...Assigning SKC")
                self.productDict[productPart] = "SKC"
                return
            
            # sort the list by base height low to high
            finalCloudList.sort(key=self._cloudLayerSort)
            finalCloudList = filteredCloudList(finalCloudList) # filter one last time
            print("Ultimate CloudList: " + str(finalCloudList))

            self.productDict[productPart] = finalCloudList

            return

        def _flightCategory(self, productPart):
            """
            Determine the most severe flight category based off of the ceiling
            and visibility (and possibly wind direction for F categories). This
            is not shown in the TAC output format but can be used to help
            determine the meteorological significance of this period.
            """

            self._textProduct.debug_print("\n" + "_"*40 + 
                "\nDetermining Flight Category:", 1)

            # Ceiling (in units of 100s of feet)
            ceilingHeight = self.productDict["ceiling"]

            # Visibility (in units of statute miles)
            numericalVisibility = self.productDict["visibility"]

            # Wind (direction in degrees, speed in knots)
            wind = self.productDict["wind"]

            self._textProduct.debug_print("ceilingHeight = %s"
                % ceilingHeight, 1)
            self._textProduct.debug_print("numericalVisibility = %s"
                % numericalVisibility, 1)
            self._textProduct.debug_print("wind = %s" % repr(wind), 1)
            self._textProduct.debug_print(" ", 1)

            # Assume VFR conditions
            flightCategory = "V"

            if (ceilingHeight is not None) and \
               (numericalVisibility is not None):

                flightCategory = self._findFlightCategory(ceilingHeight,
                                                          numericalVisibility,
                                                          wind)

            self._textProduct.debug_print(
                "\nFlight category = %s" % flightCategory, 1)
            self.productDict[productPart] = flightCategory

        def _conditionalGroup(self, productPart):
            conditionalGroup = self._textProduct.ConditionalGroup(
                self._textProduct, self._statList, self._sharedDict,
                self._airportDict, self.hourOffset)

            problemsFound = conditionalGroup.createProductParts(self)
            if problemsFound:
                self.productDict[productPart] = None
            else:
                self.productDict[productPart] = conditionalGroup

        def _ratings(self, productPart):
            self.rateFmGroup()

        #--- ____________________
        #--- HELPER METHODS
        def rateFmGroup(self):
            # Rate the product parts of the FM groups created
#             airportIcaoId = self._airportDict["icaoAirportIndicator"]

            ratingRules = self._textProduct.SignificanceRatingRules(
                self._textProduct, self)

#             self._textProduct.debug_print("\n" + "-" * 80 +
#                 "\nRating %s product parts for %s (Period %s)"
#                 % (self.productDict["groupLabel"], airportIcaoId,
#                    self.productDict["fmPeriod"]), 1)

            ratings = OrderedDict()

            for productPart in self.productParts():
                if productPart in ["groupClassification", "groupType",
                                   "groupLabel", "fmPeriod", "startTime",
                                   "endTime", "conditionalGroup", "ratings"]:
                    continue

                if self.productDict[productPart] is not None:
                    method = getattr(ratingRules, f"_{productPart}Significance")
                    ratings[productPart] = method(self.productDict[productPart])

                    self._textProduct.debug_print("%s (%s) rating = %s"
                        % (productPart, repr(self.productDict[productPart]),
                           ratings[productPart]), 1)

            self.productDict["ratings"] = ratings

            if self.productDict["conditionalGroup"] is not None:
                self._textProduct.debug_print(
                    "\n\tAverage FM group rating = %s"
                    % ratingRules.averageRating(ratings), 1)

                conditionalGroup = self.productDict["conditionalGroup"]
                conditionalRatings = conditionalGroup.productDict["ratings"]

                self._textProduct.debug_print(
                    "\nTotal average (%s and %s) rating = %s"
                    % (self.productDict["groupLabel"],
                       conditionalGroup.productDict["groupLabel"],
                       ratingRules.averageRating(ratings, conditionalRatings)),
                    1)
            else:
                self._textProduct.debug_print(
                    "\nTotal average FM group rating = %s"
                    % ratingRules.averageRating(ratings), 1)

        def mergeConditionalGroups(self, otherFmGroup):
            """
            Try to merge this FM group's conditional group with the other FM
            group's conditional group if they exist and can be merged.
            """
            if self.productDict["conditionalGroup"].canMergeWith(
               otherFmGroup.productDict["conditionalGroup"]):
                # Merge the other conditional group into this conditional group
                self.productDict["conditionalGroup"].mergeGroup(
                    otherFmGroup.productDict["conditionalGroup"])

                # Remove the old TEMPO/PROB
                otherFmGroup.removeConditionalGroup()

                return True
            else:
                return False

        def takeConditionalGroup(self, otherFmGroup):
            self.productDict["conditionalGroup"] = \
                otherFmGroup.productDict["conditionalGroup"]

            otherFmGroup.productDict["conditionalGroup"] = None

        def removeConditionalGroup(self):
            conditionalGroup = self.productDict["conditionalGroup"]

            if conditionalGroup is not None:
                self._textProduct.debug_print(
                    "\tRemoving %s from %s (Period %s)"
                    % (conditionalGroup.productDict["groupLabel"],
                       self.productDict["groupLabel"], self.period), 1)

                del self.productDict["conditionalGroup"]
                self.productDict["conditionalGroup"] = None

        def _getSkyCoverage(self, skyPercentage):
            maxSkyPercentages = self._textProduct._maxSkyPercentages

            for skyCoverage, maxPercentage in maxSkyPercentages.items():
                if skyPercentage <= maxPercentage:
                    return skyCoverage

        def _findFlightCategory(self, ceilingHeight, numericalVisibility, wind):
            """
            Go through each of the categories (from worst to best) and
            determine if either the ceiling or the visibility meet the
            requirements for that category. If so, that is our flight category.
            Note that "D" categories use a <= threshold check rather than
            a < threshold check and "F" categories have an additional wind
            direction requirement.
            """

            for categoryInfo in self._textProduct._orderedCategoryInfo:
                (category, testCeiling, testVisibility) = categoryInfo[0:3]
                self._textProduct.debug_print(
                    "processing %s:\tvisibility: %s\t\tceiling: %s"
                    % (category, testVisibility, testCeiling), 1)

                if category == "V":
                    # If we've gotten to the VFR category, we are done since
                    # it's always last.
                    return "V"
                else:
                    self._textProduct.debug_print(
                        "comparing against category %s" % category, 1)

                    if category == "D":
                        thresholdsCheck = \
                            (ceilingHeight <= testCeiling or
                             numericalVisibility <= testVisibility)
                    else:
                        thresholdsCheck = \
                            (ceilingHeight < testCeiling or
                             numericalVisibility < testVisibility)

                    if thresholdsCheck:
                        self._textProduct.debug_print(
                            "Met ceiling and/or visibility thresholds!", 1)

                        if "F" in category:
                            (windDirRangeStart, windDirRangeEnd) = \
                                categoryInfo[3]
                            self._textProduct.debug_print("\tF category: " + 
                                "ensure wind direction within range (%s, %s)"
                                % (windDirRangeStart, windDirRangeEnd), 1)

                            if self._isDirectionInRange(wind,
                                                        windDirRangeStart,
                                                        windDirRangeEnd):

                                self._textProduct.debug_print(
                                    "\t\tMet wind threshold!", 1)
                                return category
                        else:
                            return category

        def _isDirectionInRange(self, wind, dirRangeStart, dirRangeEnd):
            """
            Checks if a particular wind direction is within the specified
            range.
            """

            if wind is None:
                return False

            (direction, speed) = wind
            if direction == "VRB":
                return False

            if dirRangeEnd < dirRangeStart:
                # We've crossed the 0/360 mark
                if (direction <= dirRangeEnd) or (direction >= dirRangeStart):
                    return True
                else:
                    return False

            # else dirRangeEnd >= dirRangeStart
            if (dirRangeStart <= direction) and (direction <= dirRangeEnd):
                return True
            else:
                return False

    class ConditionalGroup(GenericGroup):
        def __init__(self, textProduct, statList, sharedDict, airportDict,
                     hourOffset):
            TextProduct.GenericGroup.__init__(self,
                                              textProduct,
                                              statList,
                                              sharedDict, airportDict)

            # The number of hours since the issuance time.
            self.hourOffset = hourOffset

        @staticmethod
        def productParts():
            """
            Product Parts for creating a particular TEMPO or PROB30 group.
            """
            return [
                    "groupClassification",
                    "groupType",
                    "startTime",
                    "endTime",
                    "wind",
                    "windGust",
                    "visibilityConditional",  # Conditional visibility grid
                    "weather",  # Weather and visibility have to be
                                  # constructed in steps since they are
                                  # dependent on each other
                    "visibility",  # Final computed visibility
                    "cloudBaseConditional",  # Conditional cloud base grid
                    "clouds",  # Final computed cloud bases
                    "groupLabel",  # groupLabel is created last because it
                                     # relies on other product parts
                    "ratings",  # Rates significance of the product parts
                   ]

        def createProductParts(self, fmGroup):
            self._textProduct.debug_print("\n" + "_"*80 + "\n" + 
                "Making TEMPO/PROB30 group for %s (%s Period %s)"
                % (fmGroup.productDict["groupLabel"],
                   self._airportDict["icaoAirportIndicator"],
                   fmGroup.period), 1)

            problemsFound = False
            for productPart in self.productParts():
                print("self._" + productPart + "(productPart, fmGroup)")
                try:
                    method = getattr(self, f"_{productPart}")
                    method(productPart, fmGroup)
                except Exception as e:
                    self._textProduct.debug_print(
                        "\n" + traceback.format_exc().strip(), 1)

                    if isinstance(e, TAF_DataException):
                        # The issue is with data that isn't required so ignore
                        # it and just set the product part to None
                        self.productDict[productPart] = None
                        self._textProduct.debug_print(
                            "Ignoring issue: %s is not required" % 
                            productPart, 1)

            if not self._isValidGroup():
                self._textProduct.debug_print(
                    "\nCould not create a conditional group.", 1)
                problemsFound = True
            else:
                self._textProduct.debug_print("\nCreated a %s group!"
                    % self.productDict["groupType"], 1)

            return problemsFound

        def _groupClassification(self, productPart, fmGroup):
            self.productDict[productPart] = "TEMPORARY_FLUCTUATIONS"

        def _groupType(self, productPart, fmGroup):
            # Only TEMPOs are allowed in the first 9 hours
            if self.hourOffset < 9:
                self.productDict[productPart] = "TEMPO"

            # Only PROBs are allowed after 9 hours
            else:
                self.productDict[productPart] = "PROB30"

        def _groupLabel(self, productPart, fmGroup):
            self._makeGroupLabel()

        def _startTime(self, productPart, fmGroup):
            startTimeSeconds = \
                self._textProduct._allPeriodStartEndTimes[self.hourOffset][0]
            self.productDict[productPart] = startTimeSeconds

        def _endTime(self, productPart, fmGroup):
            endTimeSeconds = \
                self._textProduct._allPeriodStartEndTimes[self.hourOffset][1]
            self.productDict[productPart] = endTimeSeconds

        def _wind(self, productPart, fmGroup):
            # TODO: Wind information currently not supported by the formatter
            self.productDict[productPart] = None
    
        def _windGust(self, productPart, fmGroup):
            # TODO: WindGust information currently not supported by the formatter
            self.productDict[productPart] = None

        def _visibilityConditional(self, productPart, fmGroup):
            """Get the conditional numerical visibility."""
    
            visibility = self._getData("VisibilityConditional", float,
                                       nullValue=0)
            visibility = self._roundVisibilityToReportableValue(visibility)
    
            self.productDict[productPart] = visibility

        def _weather(self, productPart, fmGroup):
#             print "::::: ConditonalGroup:_weather"
            self.productDict[productPart] = self._determineWeather(
                self.productDict["groupType"],
                self.productDict["visibilityConditional"])

        def _visibility(self, productPart, fmGroup):
            """Compute the visibility for this conditional group."""

            # FM group visibility (in statute miles)
            numericalVisibility = fmGroup.productDict["visibility"]
            # Conditional group visibility (in statute miles)
            numericalConditionalVisibility = \
                self.productDict["visibilityConditional"]
            visibilityConditionalExists = \
                numericalConditionalVisibility is not None

            # Assume there won't be conditional visibility
            self.productDict[productPart] = None

            # If this conditional group has weather
            if self.productDict["weather"].weatherExists():

                if visibilityConditionalExists:
                    # Only allow conditional visibility that is different from
                    # the FM group visibility.
                    # TODO: Why don't we want to ensure only worse visibility?
                    if numericalConditionalVisibility != numericalVisibility:
                        self.productDict[productPart] = \
                            numericalConditionalVisibility
                else:
                    # Guess the group visibility based off the visibility in
                    # the FM group.
                    if numericalVisibility < 1:
                        self.productDict[productPart] = numericalVisibility
                    elif 1 <= numericalVisibility < 2:
                        self.productDict[productPart] = 0.75
                    elif 2 <= numericalVisibility < 3:
                        self.productDict[productPart] = 1.0
                    elif 3 <= numericalVisibility <= 5:
                        self.productDict[productPart] = 2.0
                    elif numericalVisibility > 5:
                        self.productDict[productPart] = 4.0

            # If the conditional group doesn't have weather but we have a valid
            # VisibilityConditional value and this is a TEMPO group (PROB30
            # groups are only created when there is weather for them)
            elif visibilityConditionalExists and \
                 self.productDict["groupType"] == "TEMPO":

                # Conditional visibility must be worse than the FM group
                # visibility
                if numericalConditionalVisibility < numericalVisibility:
                    self.productDict[productPart] = \
                        numericalConditionalVisibility

            # Now that the final visibility is computed, finish the weather
            self._finalizeWeather("weather", fmGroup.productDict["weather"])

        def _cloudBaseConditional(self, productPart, fmGroup):
            # Conditional (TEMPO or PROB30) predominant cloud height
            # (in 100s feet)
            cloudBaseConditional = self._getData("CloudBaseConditional", int,
                                                 nullValue=0)
            self.productDict[productPart] = \
                self._roundCloudHeight(cloudBaseConditional)

        # Conditional clouds
        def _clouds(self, productPart, fmGroup):
            cloudBaseConditional = self.productDict["cloudBaseConditional"]
            cloudBaseConditionalExists = cloudBaseConditional is not None

            # The lowest cloud base in the FM group
            lowestFMCloudBase = self._getLowestFMGroupCloudBase(fmGroup)

            # Calculate a ceiling coverage based off of the sky cover of the
            # lowest cloud base in the FM group
            conditionalSkyCover = \
                self._calculateConditionalSkyCover(lowestFMCloudBase)

            # The height of the conditional cloud base that we will be
            # computing. Start off assuming we won't report anything by
            # initializing to None.
            conditionalCloudHeight = None

            # If this conditional group has weather
            if self.productDict["weather"] is not None:

                if not cloudBaseConditionalExists:
                    # Calculate a ceiling height based off of the lowest cloud
                    # base in the FM group.
                    conditionalCloudHeight = \
                        self._calculateConditionalCloudHeight(lowestFMCloudBase,
                                                              fmGroup)
                else:  # We have CloudBaseConditional available
                    conditionalCloudHeight = cloudBaseConditional

            # If the conditional group doesn't have weather but we have a valid
            # CloudBaseConditional value and this is a TEMPO group (PROB30
            # groups are only created when there is weather for them)
            elif cloudBaseConditionalExists and \
                 self.productDict["groupType"] == "TEMPO":

                # Only report a cloud base if the conditional height is <= the
                # lowest FM group cloud base height.
                if lowestFMCloudBase == "SKC" or \
                   cloudBaseConditional <= lowestFMCloudBase[1]:

                    conditionalCloudHeight = cloudBaseConditional

            # Make sure we have both parts of the cloud base
            if (conditionalSkyCover is not None) and \
               (conditionalCloudHeight is not None):

                # Make sure we don't duplicate what is in the FM group
                conditionalCloudBase = (conditionalSkyCover,
                                        conditionalCloudHeight)
                fmClouds = fmGroup.productDict["clouds"]

                if isinstance(fmClouds, list) and \
                   conditionalCloudBase in fmClouds:

                    self.productDict[productPart] = None
                else:
                    self.productDict[productPart] = [conditionalCloudBase]
            else:
                # We couldn't create a cloud base, but we have thunderstorms in
                # this conditional group, so try using the associated FM
                # group's first cloud base as this cloud base
                if self.productDict["weather"] is not None and \
                   self.productDict["weather"].thunderstormsPresent() and \
                   isinstance(fmGroup.productDict["clouds"], list) and \
                   len(fmGroup.productDict["clouds"]) > 0:
                        # Copy the prevailing clouds into the conditional clouds
                        self.productDict[productPart] = \
                            [fmGroup.productDict["clouds"][0]]
                else:
                    self.productDict[productPart] = None

        def _ratings(self, productPart, fmGroup):
            self.rateConditionalGroup()

        #--- ____________________
        #--- HELPER METHODS

        def rateConditionalGroup(self):
            # Rate the product parts of this conditional group
            if not self._isValidGroup():
                self.productDict["ratings"] = OrderedDict()
                return

            ratingRules = self._textProduct.SignificanceRatingRules(
                self._textProduct, self)

            self._textProduct.debug_print("\n" + "-" * 80 + "\nRating %s:" % 
                (self.productDict["groupLabel"]), 1)

            self.productDict["ratings"] = OrderedDict()

            for (productPart, ruleMethod) in \
                [("visibility", ratingRules._visibilitySignificance),
                 ("weather", ratingRules._weatherSignificance),
                 ("clouds", ratingRules._cloudsSignificance)]:

                if self.productDict[productPart] is not None:
                    self.productDict["ratings"][productPart] = \
                        ruleMethod(self.productDict[productPart])

                    self._textProduct.debug_print("%s (%s) rating = %s"
                        % (productPart, repr(self.productDict[productPart]),
                           self.productDict["ratings"][productPart]), 1)

            self._textProduct.debug_print(
                "\n\tAverage %s rating = %s"
                % (self.productDict["groupLabel"],
                   ratingRules.averageRating(self.productDict["ratings"])), 1)

        def canMergeWith(self, otherGroup):
            groupType = self.productDict["groupType"]
            otherGroupType = otherGroup.productDict["groupType"]
            isSameGroupType = (groupType == otherGroupType)

            groupEnd = self.productDict["endTime"]
            otherGroupStart = otherGroup.productDict["startTime"]
            isConsecutive = (groupEnd == otherGroupStart)

            groupStart = self.productDict["startTime"]
            currentDuration = time.gmtime(groupEnd - groupStart).tm_hour
            # TEMPOs can be at most 4 hours in length, PROB30s can be at
            # most 6 hours
            isValidLength = ((groupType == "TEMPO" and currentDuration < 4)
                             or
                             (groupType == "PROB30" and currentDuration < 6))

            if isSameGroupType and isConsecutive and isValidLength:
                return True
            else:
                return False

        def mergeGroup(self, otherGroup):
            """
            Merge the passed in conditional group.
            """
            self._textProduct.debug_print("\n\tMerging %s with %s" % 
                (self.productDict["groupLabel"],
                 otherGroup.productDict["groupLabel"]), 1)

            # Update the times (start time stays the same) and update the label
            self.productDict["endTime"] = otherGroup.productDict["endTime"]
            self._makeGroupLabel()

            self._mergeVisibility(otherGroup.productDict["visibility"])

            self._mergeWeather(otherGroup.productDict["weather"])

            self._mergeClouds(otherGroup.productDict["clouds"])

            # Rate the product parts of the merged conditional group
            self.rateConditionalGroup()

        def _isValidGroup(self):
            return (self.productDict["visibility"] is not None or
                    self.productDict["weather"] is not None or
                    self.productDict["clouds"] is not None)

        def _makeGroupLabel(self):
            startTimeUTC = time.gmtime(self.productDict["startTime"])
            endTimeUTC = time.gmtime(self.productDict["endTime"])
            
            # Added this code to fix bug 37515
            # Needed to use raw time from productDict so math would work
            if endTimeUTC.tm_hour == 0:
                endDay = time.gmtime(self.productDict["endTime"] - (24 * 3600)).tm_mday
                endHour = 24
            else:
                endDay = endTimeUTC.tm_mday
                endHour = endTimeUTC.tm_hour
                
            # New change for bug 37515
#             label = "%s %02d%02d/%02d%02d" % \
#                     (self.productDict["groupType"],
#                      startTimeUTC.tm_mday, startTimeUTC.tm_hour,
#                      endTimeUTC.tm_mday, endTimeUTC.tm_hour)

            # And commented out above and replaced with this code
            label = "%s %02d%02d/%02d%02d" % \
                    (self.productDict["groupType"],
                     startTimeUTC.tm_mday, startTimeUTC.tm_hour,
                     endDay, endHour)
            self.productDict["groupLabel"] = label

        def _getLowestFMGroupCloudBase(self, fmGroup):
            # Returns the lowest cloud base reported in the FM group.

            clouds = fmGroup.productDict["clouds"]

            # The first cloud base in a FM group is always the lowest.
            if isinstance(clouds, list):
                return clouds[0]
            else:
                return "SKC"

        def _calculateConditionalSkyCover(self, lowestFMCloudBase):
            # Force a ceiling in the TEMPO/PROB30 group
            if (lowestFMCloudBase == "SKC") or \
               (lowestFMCloudBase[0] in ["FEW", "SCT"]):

                return "BKN"
            else:
                return "OVC"

        def _calculateConditionalCloudHeight(self, lowestFMCloudBase, fmGroup):
            if lowestFMCloudBase == "SKC":
                # Make a guess about the height
                return self._textProduct._tempoProbDefaultHeight

            elif lowestFMCloudBase[0] in ["FEW", "SCT"]:
                # Use the same height as in the FM group
                return lowestFMCloudBase[1]

            else:
                # Get list of ordered categories
                orderedCategories = \
                    [categoryInfo[0] for categoryInfo in self._textProduct._orderedCategoryInfo]

                # Get the index of the current FM group flight category
                flightCategory = fmGroup.productDict["flightCategory"]
                categoryIndex = orderedCategories.index(flightCategory)

                # Go down one flight category and choose a height in the middle
                # of that worse category
                if categoryIndex >= 1:
                    oneLowerCategoryInfo = \
                        self._textProduct._orderedCategoryInfo[categoryIndex - 1]
                    # Second entry of category info is ceiling threshold
                    oneLowerCeiling = oneLowerCategoryInfo[1]

                    if categoryIndex == 1:
                        twoLowerCeiling = 0
                    else:
                        twoLowerCategoryInfo = \
                            self._textProduct._orderedCategoryInfo[categoryIndex - 2]
                        twoLowerCeiling = twoLowerCategoryInfo[1]

                    # Calculate the height
                    return int(self._textProduct.round((oneLowerCeiling + twoLowerCeiling) / 2.0, "Nearest", 1))
                else:
                    # Already at the worst flight category
                    lowestCategoryInfo = self._textProduct._orderedCategoryInfo[0]
                    if lowestCategoryInfo[1] > 0:  # If the ceiling isn't 0
                        # Calculate the height
                        return int(self._textProduct.round(lowestCategoryInfo[1] / 2.0, "Nearest", 1))
                    else:
                        return None  # Don't report a cloud base

        def _mergeVisibility(self, otherVisibility):
            # Merge the visibility information
            if ((self.productDict["visibility"] is None)
                or
                (otherVisibility is not None
                 and
                 otherVisibility < self.productDict["visibility"])):

                self.productDict["visibility"] = otherVisibility

        def _mergeWeather(self, otherWeather):
            # Merge the weather information
            if self.productDict["weather"] is None:

                self.productDict["weather"] = otherWeather

            elif otherWeather is not None:

                self.productDict["weather"].mergeWeather(
                    otherWeather, self.productDict["visibility"])
                
            return
        
        # Run through he layers and copy until we see an OVC layer 
        def filterClouds(self, sortedBases):
            newClouds = []
            for cover, height in sortedBases:
                if cover == "OVC":
                    newClouds.append((cover, height))
                    return newClouds
                else:
                    newClouds.append((cover, height))
            
            return newClouds

        def _mergeClouds(self, otherClouds):
            # Merge cloud bases
            clouds = self.productDict["clouds"]

            if (clouds is None
                or
                (clouds == "SKC" and isinstance(otherClouds, list))):

                self.productDict["clouds"] = otherClouds

            elif (isinstance(clouds, list) and isinstance(otherClouds, list)):

                allClouds = clouds + otherClouds
                self.productDict["clouds"] = []

                # Sort the cloud bases according to height
                sortedBases = sorted(allClouds, key=itemgetter(1))
                
                # Filter out any cloud bases above the lowest OVC layer
                sortedBases = self.filterClouds(sortedBases)

                skyCoverOrder = [None, "FEW", "SCT", "BKN", "OVC"]

                for (skyCover, height) in allClouds:
                    if len(self.productDict["clouds"]) == 0:
                        self._addCloudBase(skyCover, height)

                    else:
                        (prevSkyCover, prevHeight) = self.productDict["clouds"][-1]
                        prevCoverIndex = skyCoverOrder.index(prevSkyCover)
                        currCoverIndex = skyCoverOrder.index(skyCover)
                        # if heights are the same, pick the layer with the most clouds
                        if (height == prevHeight and currCoverIndex > prevCoverIndex):
                            self.productDict["clouds"].pop()
                            self._addCloudBase(skyCover, height)

                        elif (height > prevHeight and currCoverIndex >= prevCoverIndex):
                            self._addCloudBase(skyCover, height)
                            
            return

        def _addCloudBase(self, skyCover, height):
            numOvercastClouds = functools.reduce(lambda count, base:
                                           (count + 1) if base[0] == "OVC"
                                           else count,
                                       self.productDict["clouds"],
                                       0)

            # Don't allow more than one overcast cloud base (when looking
            # up from the ground, only the first overcast cloud base is
            # visible)
            # The directives only allow at most 3 cloud bases     
            print("number of OvercastClouds:", numOvercastClouds)   
            if numOvercastClouds == 0 and len(self.productDict["clouds"]) < 3:
                self.productDict["clouds"].append((skyCover, height))
                print("Added cloud layer to Conditional group", (skyCover, height))
                print("Conditional clouds now:", self.productDict["clouds"])

            return
    #--- HELPER METHODS

    def _displayShortenedTafInfo(self, fmGroups):
        self.debug_print("\n" + "*" * 80, 1)
        self.debug_print("Shortened TAF:", 1)
        self.debug_print("----------------", 1)

        self.debug_print("Number of periods = %s" % len(fmGroups), 1)
        for fmGroup in fmGroups:
            self.debug_print("\n%s:" % fmGroup, 1)

            fmGroupDict = fmGroup.productDict
            for (productPart, value) in fmGroupDict.items():
                self.debug_print("%s = %s" % (productPart,
                    repr(value)), 1)

            if fmGroupDict["conditionalGroup"] is not None:
                conditionalGroup = fmGroupDict["conditionalGroup"]

                self.debug_print("\n\t\t%s:" % conditionalGroup, 1)

                conditionalGroupDict = conditionalGroup.productDict
                for (productPart, value) in conditionalGroupDict.items():
                    self.debug_print("\t%s = %s" % (productPart,
                           repr(value)), 1)

        self.debug_print("\n" + "*" * 80, 1)

    def _finalizeTemposProbs(self, shortenedFmGroups, allFmGroups, airportIcaoId):
        self.debug_print("\nFinalizing conditional groups", 1)

        # Determine which FM groups were removed
        removedFmGroups = allFmGroups[:]
        self.debug_print("Removed FM groups:", 1)
        for fmGroup in shortenedFmGroups:
            # Take away all the kept FM groups to determine which were removed
            removedFmGroups.remove(fmGroup)
            self.debug_print("\t%s" % fmGroup, 1)

        # Get the list of consecutive removed periods
        consecutiveRemovedPeriodsList = self._getConsecutiveRemovedPeriodsList(
            removedFmGroups, allFmGroups)

        # May want to consider looking at all fmGroups not just removed groups.
        for consecutiveRemovedPeriods in consecutiveRemovedPeriodsList:
            consecutiveRemovedFmGroups = []
            for fmPeriod in consecutiveRemovedPeriods:
                for fmGroup in allFmGroups:
                    if fmGroup.productDict["fmPeriod"] == fmPeriod:
                        consecutiveRemovedFmGroups.append(fmGroup)
                        break

            # Try to create multi-hour TEMPO and multi-hour PROB groups by
            # merging together consecutive TEMPOS and consecutive PROBs
            if self._allowMultiHourTempoProbs == "Yes":
                self._mergeTemposProbs(consecutiveRemovedFmGroups)

            # For each list of consecutive FM groups, find the most significant
            # TEMPO/PROB30 and attach it to the first FM group (which was kept)
            # in the list and remove the TEMPO/PROB30 groups from all other
            # FM groups in the list (which were removed by shortening).
            self._keepMostSignificantTempoProb(consecutiveRemovedFmGroups,
                                               attachToFirstGroup=True)

        if self._limitOneTempoPerTAF == "Yes":
            # If the user selects the GUI option to limit TAFs to have at most
            # one TEMPO, look at all the TEMPOs and only keep the most
            # significant one and remove the rest
            self.debug_print("\n\tLimiting TAF to at most 1 TEMPO group", 1)

            self._keepMostSignificantTempoProb(shortenedFmGroups, "TEMPO")

    def _mergeTemposProbs(self, fmGroups):
        # Find all FM groups that have conditional groups associated with them
        fmGroupsWithConditionals = self._findFmGroupsWithTemposProbs(fmGroups)

        workingFmGroup = None
        index = 0
        numGroups = len(fmGroupsWithConditionals)

        while index < numGroups:
            if workingFmGroup is None:
                # This FM group has a TEMPO/PROB30 we are working on that
                # might become a multi-hour TEMPO/PROB30 group possibly
                workingFmGroup = fmGroupsWithConditionals[index]

                # Go to the next group to see if it can be merged with this
                index = index + 1

            else:
                currentFmGroup = fmGroupsWithConditionals[index]

                # Try merging the conditional groups associated with these
                # FM groups
                mergedSuccessfully = \
                    workingFmGroup.mergeConditionalGroups(currentFmGroup)

                if mergedSuccessfully:
                    # Go to the next group
                    index = index + 1
                else:
                    # Trigger the start of a new working group
                    workingFmGroup = None

    def _getConsecutiveRemovedPeriodsList(self, removedFmGroups, allFmGroups):
        # Build up lists of consecutive removed periods and include the period
        # before the first period in each list. For example, if the periods
        # removed were: 02Z, 03Z, 04Z, 07Z, 08Z, 13Z, 19Z, 20Z then create this
        # list of lists:
        # [[01Z, 02Z, 03Z, 04Z], [06Z, 07Z, 08Z], [12Z, 13Z], [18Z, 19Z, 20Z]]
        consecutivePeriodsList = [[]]
        # Since we never remove the first period (unless there's a
        # bug in the code), it's safe to assume every removed period has a
        # previous period before it

        self.debug_print("\n\tBuilding lists of consecutive FM groups", 1)

        numRemoved = len(removedFmGroups)
        index = 0
        while index < numRemoved:
            removedFmGroup = removedFmGroups[index]
            consecutivePeriods = consecutivePeriodsList[-1]
            previousFmGroupIndex = allFmGroups.index(removedFmGroup) - 1
            previousFmGroup = allFmGroups[previousFmGroupIndex]

            if len(consecutivePeriods) == 0:
                # This is a new list of consecutive periods.
                # Add the previous (kept) period and the removed period.
                consecutivePeriods.append(previousFmGroup.period)
                consecutivePeriods.append(removedFmGroup.period)
                index = index + 1

                self.debug_print("\tStarted new list:", 1)
                self.debug_print("\t\tAdded Period %s\n\t\t\tAdded Period %s" % 
                    (previousFmGroup.period, removedFmGroup.period), 1)

            elif removedFmGroup.period - consecutivePeriods[-1] == 1:
                # This is the next consecutive period in this list
                consecutivePeriods.append(removedFmGroup.period)
                index = index + 1

                self.debug_print("\t\tAdded Period %s" % 
                    removedFmGroup.period, 1)

            else:
                # This is not consecutive, start a new list
                consecutivePeriodsList.append(list())

        self.debug_print("Finished building consecutive lists", 1)
        return consecutivePeriodsList

    def _keepMostSignificantTempoProb(self, fmGroups, groupType=None, attachToFirstGroup=False):
        self.debug_print("\tKeeping most significant conditional group", 1)

        # Find all FM groups that have conditional groups associated with them
        fmGroupsWithConditionals = self._findFmGroupsWithTemposProbs(fmGroups,
                                                                     groupType)

        if len(fmGroupsWithConditionals) == 0:
            self.debug_print("\tNo conditional groups found", 1)
            return

        # Find the most significant TEMPO/PROB30 and remove all the rest
        fmGroupWithMostSignificantTempoProb = \
            self._findMostSignificantTempoProb(fmGroupsWithConditionals)
        mostSignificantConditional = \
            fmGroupWithMostSignificantTempoProb.productDict["conditionalGroup"]

        for fmGroup in fmGroupsWithConditionals:
            if fmGroup.period != fmGroupWithMostSignificantTempoProb.period:
                # This FM group's TEMPO/PROB30 is not the most significant,
                # remove it
                fmGroup.removeConditionalGroup()

        self.debug_print("\tKept '%s' from '%s'"
            % (mostSignificantConditional,
               fmGroupWithMostSignificantTempoProb), 1)

        # If the most significant TEMPO/PROB needs to be attached to the first
        # FM group in the list, then attach it if necessary
        if attachToFirstGroup and \
           fmGroupWithMostSignificantTempoProb.period != fmGroups[0].period:

            self.debug_print("\t\tAttaching '%s' to '%s'"
                             % (mostSignificantConditional, fmGroups[0]), 1)

            fmGroups[0].takeConditionalGroup(
                fmGroupWithMostSignificantTempoProb)

    def _findFmGroupsWithTemposProbs(self, fmGroups, groupType=None):
        """
        Find all FM groups that have a conditional (TEMPO/PROB30) group
        associated with them. If a group type is specified, only find FM groups
        with that type of group.
        """
        fmGroupsWithConditionals = []
        for fmGroup in fmGroups:
            if fmGroup.productDict["conditionalGroup"] is not None:
                conditionalGroup = fmGroup.productDict["conditionalGroup"]

                if groupType is None or \
                   conditionalGroup.productDict["groupType"] == groupType:

                    fmGroupsWithConditionals.append(fmGroup)

        return fmGroupsWithConditionals

    def _findMostSignificantTempoProb(self, fmGroupsWithTempoProb):
        self.debug_print("\tLooking for most significant conditional group", 1)

        mostSignificantRating = None
        fmGroupWithMostSignficantTempoProb = None

        for fmGroup in fmGroupsWithTempoProb:
            conditionalGroup = fmGroup.productDict["conditionalGroup"]
            conditionalRatings = conditionalGroup.productDict["ratings"]

            averageConditionalRating = \
                self.SignificanceRatingRules.averageRating(conditionalRatings)

            if mostSignificantRating is None or \
               averageConditionalRating > mostSignificantRating:

                mostSignificantRating = averageConditionalRating
                fmGroupWithMostSignficantTempoProb = fmGroup

        if mostSignificantRating is not None:
            fmGroup = fmGroupWithMostSignficantTempoProb
            self.debug_print("\t%s has most significant conditional:\n\t\t\t%s"
                % (fmGroup, fmGroup.productDict["conditionalGroup"]), 1)

        return fmGroupWithMostSignficantTempoProb

    def _removeTempoProb(self, fmGroupDict):
        self.debug_print("\tRemoving %s from %s (Period %s)"
            % (fmGroupDict["conditionalGroup"].productDict["groupLabel"],
               fmGroupDict["groupLabel"], fmGroupDict["fmPeriod"]), 1)

        del fmGroupDict["conditionalGroup"]
        fmGroupDict["conditionalGroup"] = None

    #--------------------------------------------------------------------------

    # Holds information extracted from a weather ugly string
    # TODO: add __str__ and __repr__ to debug nicely
    class WeatherUglyStringInfo():
        """
        An example weather ugly string looks like: "Sct:T:+:1/4SM:SmA,HvyRn"
        which would indicate heavy scattered thunderstorms (with attributes of
        small hail and heavy rain) that cause a reduced visibility of 1/4
        statute miles.

        It's important to note that the type and attributes use GFE codes
        initially but are converted to TAF codes.
        """

        def __init__(self, textProduct, weatherUglyString, numericalVisibility, groupType):
            self._textProduct = textProduct
            self._setWeatherParts(weatherUglyString, numericalVisibility)
            # Set the classifier for this weather type to determine how (or if)
            # it should be reported in the output
            self._setWeatherClassifier()
            # Use the visibility from the grids, not the weather ugly string
            self._modifyWeatherIfNeeded(groupType, numericalVisibility)
            
        def __copy__(self):
            info = type(self)(self.__class__)
            info.__dict__.update(self.__dict__)

            return info

        def _setWeatherParts(self, weatherUglyString, visibility):
            """Extract the information contained in the weather ugly string."""
            weatherParts = weatherUglyString.split(":")

            self.probability = weatherParts[0]  # probability/coverage

            self.gfeWeatherType = weatherParts[1]  # wx type

            self.tafWeatherType = self._convertGfeCodeToTafCode(
                                    self.gfeWeatherType, visibility)

            self.intensity = weatherParts[2]  # wx intensity

            self.visibility = weatherParts[3]  # visibility

            self.gfeAttributes = weatherParts[4]  #  extra attributes
            self.tafAttributes = []
            attributeList = self.gfeAttributes.split(",")
            for gfeAttribute in attributeList:
                # Try converting it if possible, otherwise don't include it
                tafAttribute = self._convertGfeCodeToTafCode(gfeAttribute, visibility)
                # TODO: Are any other attributes convertable to a TAF code?
                if tafAttribute != "":
                    self.tafAttributes.append(tafAttribute)

        def _setWeatherClassifier(self):
            """
            Determine the weather classifier used to indicate how (or if) to
            report this weather type based upon the type, when it occurs and
            its probability/coverage (and possibly intensity too, if present). 
            """

            typeRules = self._findTypeRules()

            probabilityRules = self._findProbabilityRules(typeRules)

            self.classifier = self._findClassifier(probabilityRules)

        def _modifyWeatherIfNeeded(self, groupType, numericalVisibility):
            # If we will report this weather
            if self.classifier in ["PREVAIL", "TEMPO", "PROB30"]:
                
                print("+& ModifyWx: TafWxType: :" + self.tafWeatherType)
                print("+& ModifyWx: Vis:" + str(numericalVisibility))
                
                # Modify thunderstorms when attributes are present
                if self.tafWeatherType == "TS":

                    self._textProduct.debug_print("Adding %s attributes to TS"
                                                  % self.tafAttributes, 1)

                    # Add any attributes which may be present
                    for tafAttribute in self.tafAttributes:
                        self.tafWeatherType += tafAttribute

                # Modify fog codes based on visibility conditions
                elif self.tafWeatherType == "BR" and \
                     numericalVisibility < 0.625:

                    self._textProduct.debug_print("Visibility < 5/8SM: " + 
                                                  "using stronger fog code FG",
                                                  1)

                    # Fog with < 5/8SM visibility uses stronger fog code FG
                    self.tafWeatherType = "FG"

                elif self.tafWeatherType == "FZFG" and \
                    numericalVisibility >= 0.625:

                    self._textProduct.debug_print("Visibility >= 5/8SM: " + 
                                                  "using weaker fog code BR",
                                                  1)

                    # Freezing Fog is only allowed when visibility is < 5/8SM,
                    # other visibilities use the weaker BR fog code instead
                    self.tafWeatherType = "BR"

            # Handle vicinity weather (if we are going to report it). Only FM
            # groups can have vicinity weather.
            elif "VC" in self.classifier and \
                 self._textProduct._reportVC and groupType == "FM":

                self._textProduct.debug_print(
                    "Reporting %s as %s instead" % 
                    (self.tafWeatherType, self.classifier), 1)

                # Show this as vicinity weather instead of the usual type code.
                self.tafWeatherType = self.classifier
                # Ignore intensity with vicinity weather
                self.intensity = "<NoInten>"

        def _findTypeRules(self):
            """Try finding rules for this weather type."""

            # Start off assuming we'll use the default rules, if defined
            typeRules = self._textProduct._weatherRules.get("default", None)

            # Use the specific rules for this weather type, if defined
            for types in self._textProduct._weatherRules:
                # Multiple types can share the same rules
                typeList = types.split(',')
                typeList = list(map(str.strip, typeList))

                if self.gfeWeatherType in typeList:
                    typeRules = self._textProduct._weatherRules[types]
                    break

            if typeRules is None:
                raise TAF_DataException(
                    "Can't find rules to classify GFE type %s (TAF type %s)."
                    % (self.gfeWeatherType, self.tafWeatherType))

            return typeRules

        def _findProbabilityRules(self, typeRules):
            """Try finding rules for this weather type at this hour offset."""
            
            # Use this period's index (aka. hours since issuance time) to get
            # the dictionary of probability/coverage rules for this weather
            # type.
            try:
                probabilityRules = \
                    self._textProduct.nlValue(typeRules,
                                              self._textProduct._periodIndex)
            except:
                message = "Can't find rules to classify GFE type %s " \
                          + "(TAF type %s) at hour offset %s."
                raise TAF_DataException(message % 
                    (self.gfeWeatherType, self.tafWeatherType,
                     self._textProduct._periodIndex))

            return probabilityRules

        def _findClassifier(self, probabilityRules):
            """
            Use the probability/coverage (and possibly intensity) to determine
            how to display this weather in the output (the classifier).
            """
            classifier = None

            for probabilities in probabilityRules:
                print("Probabilities: " + str(probabilities))
                if probabilities.lower().strip() == "default":
                    # We'll deal with default at the end if we haven't found a
                    # better matching probability
                    continue

                # Multiple probabilities/coverages can share the same rule
                probabilityList = probabilities.split(',')
                probabilityList = list(map(str.strip, probabilityList))

                # Check for a more specialized rule that includes intensity
                # with the probability
                if self.intensity not in ["", "<NoInten>"]:
                    probabilityIntensity = self.probability + self.intensity

                    if probabilityIntensity in probabilityList:
                        classifier = probabilityRules[probabilities]
                        break

                # Check for a rule that only specifies the probability
                if self.probability in probabilityList:
                    classifier = probabilityRules[probabilities]
                    break

            # We haven't found a classifier yet, try using the default rule
            if classifier is None:
                try:
                    classifier = probabilityRules["default"]
                except:
                    message = "No default classifier rules for GFE type %s " \
                              + "(TAF type %s) at hour offset %s."
                    raise TAF_DataException(message % 
                        (self.gfeWeatherType, self.tafWeatherType,
                         self._textProduct._periodIndex))

            # It was determined to not show this weather, so throw this
            # exception and skip it.
            if classifier == "":
                raise TAF_DataException(
                    "GFE type %s (TAF type %s) classified as 'do not show'."
                    % (self.gfeWeatherType, self.tafWeatherType))
            print("returning classifier: " + str(classifier))
            return classifier

        def _convertGfeCodeToTafCode(self, gfeCode, visibility):
            """Convert a GFE code to a TAF code."""

            # Try converting it if possible, otherwise don't report it
            tafCode = self._textProduct._gfeCodeToTafCodeMap.get(gfeCode, "")

            return tafCode

    class GroupWeatherInfo():
        # Define a list of TAF weather type codes for precipitation weather

        # The set and order in which descriptor  types should appear
        descriptorTypes = ["TSRA", "TSSN", "FZRA", "FZDZ", "FZFG", "SHSN", "SHRA", "BLSN", "BLDU", "BLSA", ]
        descriptorPrecipTypes = ["TSRA", "TSSN", "FZRA", "FZDZ", "SHSN", "SHRA", ]

        # The set and order in which precipitation types should appear
        precipitationWeatherTypes = ["TSRA", "TSSN", "TS", "VCTS",
                                     "GR", "GS", "SN", "PL", "IC", "RA",
                                     "VCSH", "DZ",
                                     ]
        # The set and order in which obstruction types should appear        
        obstructionTypes = ["BLSN", "BLDU", "BLSA", "BR", "FG", "HZ", "FU",
                            "SS", "DS", "VA", "SA", "DU", "PY",
                            ]
        
        intensitySeverity = [None, "<NoInten>", "--", "-", "m", "+"]

        def __init__(self, textProduct, groupType, numericalVisibility):
            self._textProduct = textProduct

            self.groupType = groupType
            # The visibility from the Visibility grid (FM groups) or
            # VisibilityConditional grid (TEMPO/PROB30 groups)
            self.numericalVisibility = numericalVisibility

            self.maxIntensity = ""
            self.tafIntensityWx = ""
            self.regularWeatherTypes = []
            self.obstructionWeatherTypes = []
            self.vicinityWeather = None
            self.uglyStringInfos = []

        def __repr__(self):
            
            text = " Prob:"
            for type in self.uglyStringInfos:
                text += " %s" % type.probability
                
            text = text + " Int: %s" % self.maxIntensity

            text += " Type:"
            for type in self.regularWeatherTypes:
                text += " %s" % type

            text += " obscur:"
            for type in self.obstructionWeatherTypes:
                text += " %s" % type
            
            text += ", ViWx: %s" % self.vicinityWeather
                
            return text

        def __eq__(self, other):
            if other is None:
                return False
            else:
                return (self.maxIntensity == other.maxIntensity
                        and
                        (sorted(self.regularWeatherTypes) == 
                         sorted(other.regularWeatherTypes))
                        and
                        (sorted(self.obstructionWeatherTypes) == 
                         sorted(other.obstructionWeatherTypes))
                        and
                        self.vicinityWeather == other.vicinityWeather)

        @property
        def precipSort(self):
            def cmpfunc(a, b):
                if a not in self.precipitationWeatherTypes or b not in self.precipitationWeatherTypes:
                    return 0
                aIndex = self.precipitationWeatherTypes.index(a)
                bIndex = self.precipitationWeatherTypes.index(b)

                if aIndex < bIndex:
                    return -1
                if bIndex < aIndex:
                    return 1
                return 0
            return functools.cmp_to_key(cmpfunc)

        # A simple method to rank weather types in the order that they appear in the TAF
        def weatherTypeRanking(self, wx):
            
            if wx.find("TS") > -1:  # Thunder rules over all over types
                return 0
            for descWx in self.descriptorTypes:
                if wx.find(descWx) > -1:
                    return 1
            for precipWx in self.precipitationWeatherTypes:
                if wx.find(precipWx) > -1:
                    return 2
    
            return 3
        
        @property
        def weatherSort(self):
            def cmpfunc(a, b):
                # First sort by ranking - See above
                rankA = self.weatherTypeRanking(a)
                rankB = self.weatherTypeRanking(b)

                if rankA > rankB:
                    return 1
                if rankA < rankB:
                    return -1

                # Next sort by max intensity
                if a == self.tafIntensityWx:
                    return -1
                if b == self.tafIntensityWx:
                    return 1

                return 0
            return functools.cmp_to_key(cmpfunc)

        def updateWeather(self, weatherUglyStringInfo):
            uglyStringTafWeatherType = weatherUglyStringInfo.tafWeatherType
            uglyStringClassifier = weatherUglyStringInfo.classifier

            # Only FM groups can have vicinity weather
            if "VC" in uglyStringClassifier:
                if self.groupType == "FM":
                    self.updateVicinityWeather(uglyStringTafWeatherType)

            # Update maximum intensity if this is precipitation weather
            # Check for VCSH first and replace it if we're inserting VCTS
#             if uglyStringTafWeatherType in [self.precipitationWeatherTypes]:  #was
            intensityTypes = self.precipitationWeatherTypes + self.descriptorTypes
            if uglyStringTafWeatherType in intensityTypes:
                self.updateMaximumIntensity(weatherUglyStringInfo.intensity, uglyStringTafWeatherType)

                if "VCSH" in self.regularWeatherTypes and uglyStringTafWeatherType == "VCTS":
                    self.regularWeatherTypes.remove("VCSH")  # VCTS replaces VCSH
                self.regularWeatherTypes.append(uglyStringTafWeatherType)

            elif uglyStringTafWeatherType in self.obstructionTypes:
                self.obstructionWeatherTypes.append(uglyStringTafWeatherType)
                # This code commented out since it prevented the addition of obstruction wx  10.31.18 
#                 if uglyStringTafWeatherType == "VA" or \
#                    (self.numericalVisibility is not None and
#                     self.numericalVisibility <
#                     self._textProduct._minP6smVisibility):

                    # With the exception of volcanic ash, all obstruction types
                    # must have reduced visibility
            else:
                self.regularWeatherTypes.append(uglyStringTafWeatherType)

            self.uglyStringInfos.append(weatherUglyStringInfo)
            return 

        def mergeWeather(self, otherWeather, updatedVisibility):
            # Merge the weather information
            self.uglyStringInfos += otherWeather.uglyStringInfos
            self.uglyStringInfos = list(set(self.uglyStringInfos))

            self.regularWeatherTypes += otherWeather.regularWeatherTypes
            self.regularWeatherTypes = list(set(self.regularWeatherTypes))

            self.obstructionWeatherTypes += \
                otherWeather.obstructionWeatherTypes
            self.obstructionWeatherTypes = \
                list(set(self.obstructionWeatherTypes))

            self.updateVicinityWeather(otherWeather.vicinityWeather)
                            
            self.updateMaximumIntensity(otherWeather.maxIntensity, otherWeather.uglyStringInfos)

            self.numericalVisibility = updatedVisibility

            self.postProcessWeatherTypes()

        def addAnyRepeatingFmWeather(self, fmWeather):
            fmWeatherExists = fmWeather is not None
            minP6smVisibility = self._textProduct._minP6smVisibility

            # Make sure the conditional weather doesn't consist of only
            # repeating FM weather except when reduced visibility exists, then
            # copy all FM weather into the conditional weather.
            if not self.weatherExists():
                if self.numericalVisibility is not None and \
                   self.numericalVisibility < minP6smVisibility and \
                   fmWeatherExists:

                    self.maxIntensity = fmWeather.maxIntensity
                    self.regularWeatherTypes = \
                        copy.copy(fmWeather.regularWeatherTypes)
                    self.obstructionWeatherTypes = \
                        copy.copy(fmWeather.obstructionWeatherTypes)
                    self.vicinityWeather = fmWeather.vicinityWeather
                    self.uglyStringInfos = \
                        copy.copy(fmWeather.uglyStringInfos)

                # If we don't already have weather, any weather added here
                # would consist of only repeated FM weather
                return

            # If there is any reportable weather in the FM group
            if fmWeatherExists:
                # Check if there is any FM weather we need to repeat in this
                # group
                fmWeatherTypesToRepeat = \
                    self._textProduct._fmWeatherTypesToRepeat

                for tafWeatherType in fmWeatherTypesToRepeat:
                    if tafWeatherType in fmWeather.regularWeatherTypes:
                        self.regularWeatherTypes.append(tafWeatherType)

                    elif tafWeatherType in fmWeather.obstructionWeatherTypes:
                        self.obstructionWeatherTypes.append(tafWeatherType)

        def postProcessWeatherTypes(self):
            # Remove all duplicate weather while preserving order
            self.regularWeatherTypes = \
                self._removeDuplicateWeather(self.regularWeatherTypes)
            self.obstructionWeatherTypes = \
                self._removeDuplicateWeather(self.obstructionWeatherTypes)

            # Apply various logic rules to ensure the main weather is displayed
            # properly when the weather text is constructed
            self.regularWeatherTypes = \
                self._consolidateRegularWeather(self.regularWeatherTypes)
            self.regularWeatherTypes = \
                self._orderRegularWeather(self.regularWeatherTypes)

            # Apply various logic rules to ensure the obstruction weather is
            # displayed properly when the weather text is constructed
            print("IN postprocess....")
            self.obstructionWeatherTypes = self._consolidateObstructionWeather(
                self.obstructionWeatherTypes)

        def weatherExists(self):
            
            allWx = self.getAllWeather()
            if len(allWx) > 0:
                if allWx[0] != "":
                    return True
             
            return False


#             replaced this code with above as there were cases where it didn't work
#             return (len(self.regularWeatherTypes) > 0 or
#                     len(self.obstructionWeatherTypes) > 0 or
#                     self.vicinityWeather is not None)

        def getAllWeather(self):
            return self.regularWeatherTypes + \
                   self.obstructionWeatherTypes + \
                   ([] if self.vicinityWeather is None
                    else [self.vicinityWeather])
                   
        def getMaxIntensity(self):
            return self.maxIntensity

        def updateVicinityWeather(self, newVicinityType):
            vicinitySeverity = [None, "VCSH", "VCTS"]
            currentSeverityLevel = vicinitySeverity.index(self.vicinityWeather)
            newSeverityLevel = vicinitySeverity.index(newVicinityType)

            if newSeverityLevel > currentSeverityLevel:
                self.vicinityWeather = newVicinityType
                
                
        # Updates maximum intensity and the weather type that caused it.
        def updateMaximumIntensity(self, newIntensity, tafWxType):
        
            # Turns out the directive says that the intensity goes with the 
            # Wx type that contains a descriptor, if present.
            # This code updates the intensity based on these rules.
            # If we have a descriptor type, that trumps no descriptor type
            if self.tafIntensityWx in self.descriptorTypes and tafWxType not in self.descriptorTypes:
                return
            
            # Automatic update if the new one is a descriptor type and the current one is not
            elif self.tafIntensityWx not in self.descriptorTypes and tafWxType in self.descriptorTypes:
                self.tafIntensityWx = tafWxType
                self.maxIntensity = newIntensity
                return
                
            # At this point either both are or both are not descriptor types, so check severity (intensity).
            currentSeverityLevel = 0
            newSeverityLevel = 0
            if self.maxIntensity in self.intensitySeverity:
                currentSeverityLevel = self.intensitySeverity.index(self.maxIntensity)
            if newIntensity in self.intensitySeverity:
                newSeverityLevel = self.intensitySeverity.index(newIntensity)

            # If the intensities are the same, update the tafIntensityWx if the weather type
            # ranks higher in the list.
            
            if newSeverityLevel == currentSeverityLevel:
                if tafWxType in self.precipitationWeatherTypes and \
                  self.tafIntensityWx in self.precipitationWeatherTypes:
                    if self.precipitationWeatherTypes.index(tafWxType) < \
                      self.precipitationWeatherTypes.index(self.tafIntensityWx):
                        self.maxIntensity = newIntensity
                        self.tafIntensityWx = tafWxType
                        
            
            if newSeverityLevel > currentSeverityLevel:
                self.maxIntensity = newIntensity
                self.tafIntensityWx = tafWxType
                
            return
        
        def thunderstormsPresent(self):
            allWeatherTypes = \
                (self.regularWeatherTypes + self.obstructionWeatherTypes)
            # Only check for vicinity thunderstorms in FM groups (they aren't
            # allowed in TEMPO/PROB30 groups)
            
            
            if self.vicinityWeather is not None:
                allWeatherTypes += [self.vicinityWeather]
#             print "All Weather Types: " + str(allWeatherTypes)
#             print "Vicinity Weather Types: " + str(self.vicinityWeather)
              # Removed this code to fix VCTS/VCSH bug
#             if self.groupType == "FM":
#                 allWeatherTypes += ([self.vicinityWeather]
#                                     if self.vicinityWeather is not None
#                                     else [])

            for weatherType in allWeatherTypes:
                # We check like this because thunderstorms can have attributes
                # added to them to become new types (ie. TSGR) so we can't
                # simply look for TS in allWeatherTypes.
                if "TS" in weatherType:
                    return True

            return False

        def _removeDuplicateWeather(self, weatherTypes):
            """Remove duplicate weather types while preserving order."""

            weatherTypes = list(OrderedDict.fromkeys(weatherTypes))

            return weatherTypes

        def _consolidateRegularWeather(self, weatherTypes):
            # TODO: change & move this logic elsewhere so the weather doesn't
            #       even get added in the first place.
            # Remove SHRA if RA present
            if "SHRA" in weatherTypes and "RA" in weatherTypes:
                weatherTypes.remove("SHRA")
            
            # Some thunder cases have other precip. types already combined
            # so we must test for each case.
            if "SHRA" in weatherTypes and "TSGR" in weatherTypes:
                weatherTypes.remove("SHRA")
                weatherTypes.remove("TSGR")
                weatherTypes.insert(0, "TSRAGR")
                
            if "SHRA" in weatherTypes and "TSGR" in weatherTypes:
                weatherTypes.remove("SHRA")
                weatherTypes.remove("TSGS")
                weatherTypes.insert(0, "TSRAGS")

            if "SHRA" in weatherTypes and "TSPL" in weatherTypes:
                weatherTypes.remove("SHRA")
                weatherTypes.remove("TSPL")
                weatherTypes.insert(0, "TSRAPL")

            # Remove SHSN if SN present
            if "SHSN" in weatherTypes and "SN" in weatherTypes:
                weatherTypes.remove("SHSN")

            # If both SHRA and SHSN are present
            if "SHRA" in weatherTypes and "SHSN" in weatherTypes:

                # Determine the order of appearance for each type
                rainIndex = weatherTypes.index("SHRA")
                snowIndex = weatherTypes.index("SHSN")

                # If SHRA is more dominant
                if rainIndex < snowIndex:

                    # Replace SHSN with SN so we get SHRASN, not SHRASHSN
                    weatherTypes.insert(snowIndex, "SN")
                    weatherTypes.remove("SHSN")

                # Otherwise, if SHSN is more dominant
                else:

                    # Replace SHRA with RA so we get SHSNRA, not SHSNSHRA
                    weatherTypes.insert(rainIndex, "RA")
                    weatherTypes.remove("SHRA")

            # Remove RA if FZRA present
            if "RA" in weatherTypes and "FZRA" in weatherTypes:
                weatherTypes.remove("RA")

            # Remove SHRA if FZRA present
            if "SHRA" in weatherTypes and "FZRA" in weatherTypes:
                weatherTypes.remove("SHRA")

            # Remove DZ if FZDZ or FZRA present
            if "DZ" in weatherTypes and ("FZRA" in weatherTypes or
                                         "FZDZ" in weatherTypes):
                weatherTypes.remove("DZ")

            # Remove FZDZ if FZRA present
            if "FZDZ" in weatherTypes and "FZRA" in weatherTypes:
                weatherTypes.remove("FZDZ")

            return weatherTypes

        def _consolidateObstructionWeather(self, weatherTypes):
            # TODO: change & move this logic elsewhere so the weather doesn't
            #       even get added in the first place.
            # Remove BR if FG or FZFG present
            
            if "BR" in weatherTypes and ("FG" in weatherTypes or
                                         "FZFG" in weatherTypes):
                weatherTypes.remove("BR")

            # Remove FG if FZFG present
            if "FG" in weatherTypes and "FZFG" in weatherTypes:
                weatherTypes.remove("FG")
                
#             if self.numericalVisibility >= 0.625 and self.numericalVisibility <= 6.0:
#                 if "BR" not in weatherTypes and len(weatherTypes) == 0:
#                     weatherTypes.append("BR")
#                     print "appended BR...wxTypes: " + str(weatherTypes)

            return weatherTypes

        def _orderRegularWeather(self, weatherTypes):
            # TODO: Investigate this method, it seems off somehow
            newWeatherTypes = []

            # If we have thunderstorms as a prevailing weather type
            if "TS" in weatherTypes:

                # List it as the first weather type
                newWeatherTypes.append("TS")
                weatherTypes.remove("TS")

                # Now handle the precipitation side of things
                if "SHRA" in weatherTypes:

                    # Add just "rain" to the weather types
                    newWeatherTypes.append("RA")
                    weatherTypes.remove("SHRA")

                if "SHSN" in weatherTypes:

                    # Add just "snow" to the weather types
                    newWeatherTypes.append("SN")
                    weatherTypes.remove("SHSN")

            # If we have freezing precipitation as a prevailing weather type
            if "FZRA" in weatherTypes:

                # List it as the first weather type
                newWeatherTypes.append("FZRA")
                weatherTypes.remove("FZRA")

            # If we have freezing precipitation as a prevailing weather type
            if "FZDZ" in weatherTypes:

                # List it as the first weather type
                newWeatherTypes.append("FZDZ")
                weatherTypes.remove("FZDZ")

            # Look through all of the precipitation weather types
            for currType in weatherTypes:

                # If this is a weather type associated with a descriptor
                if len(currType) > 2:

                    # Add it first before weather without descriptors
                    newWeatherTypes.append(currType)
                    weatherTypes.remove(currType)

            # Now add the remaining kept weather types in the order we found it
            for currType in weatherTypes:
                newWeatherTypes.append(currType)

            return newWeatherTypes

    class SignificanceRatingRules():
        def __init__(self, textProduct, group):
            self._textProduct = textProduct
            self._group = group

        @staticmethod
        def averageRating(ratingDict1, ratingDict2=dict()):
            ratings = list(ratingDict1.values()) + list(ratingDict2.values())
            # Don't let non-existent ratings mess up the average rating
            ratings = [value for value in ratings if value is not None]

            return functools.reduce(int.__add__, ratings, 0) / float(len(ratings))

        def _windSignificance(self, wind):
            # direction is either a number in units of degrees or "VRB"
            # speed is a number in units of knots
            (direction, speed) = wind

            # rate speed
            speedRating = 0

            if speed < 20:
                speedRating = 0
            elif speed < 30:
                speedRating = 20
            elif speed < 40:
                speedRating = 40
            elif speed < 50:
                speedRating = 60
            else:
                speedRating = 100

            # rate direction
            directionRating = 0

            normal = 0
            affected = 25
            restricted = 80

            if direction == "VRB":
                directionRating = normal
            elif direction <= 40:
                directionRating = normal
            elif direction <= 80:
                directionRating = affected
            elif direction <= 120:
                directionRating = restricted
            elif direction <= 140:
                directionRating = affected
            elif direction <= 240:
                directionRating = normal
            else:
                directionRating = affected

            return (speedRating + directionRating) // 2

        def _windGustSignificance(self, windGust):
            # windGust is a number in units of knots

            # the higher the wind gust, the more significant it is
            if windGust < 15:
                return 0
            elif windGust < 25:
                return 15
            elif windGust < 30:
                return 25
            elif windGust < 40:
                return 40
            elif windGust < 50:
                return 60
            elif windGust < 63:
                return 80
            else:
                return 100

        def _visibilitySignificance(self, visibility):
            # visibility is a number with units of SM (statute miles)

            # the lower the visibility, the more significant it is
            if 0 <= visibility <= 0.5:
                return 100
            elif 0.5 < visibility <= 1:
                return 75
            elif 1 < visibility <= 2:
                return 50
            elif 2 < visibility <= 5:
                return 25
            else:
                return 0

        def _weatherSignificance(self, weather):
            # weather is a GroupWeatherInfo object for a group that gives all
            # sorts of information like the maximum intensity, list of regular
            # weather types, list of obstruction weather types, vicinity
            # weather, associated weather ugly string information and more.

            maxRating = 0
            for uglyStringInfo in weather.uglyStringInfos:
                rating = 0
                tafWeatherType = uglyStringInfo.tafWeatherType
                intensity = uglyStringInfo.intensity  # <NoInten>, --, -, m, +

                # Note that tafWeatherType can have qualifiers and/or
                # attributes. To prevent accidentally capturing a type not
                # wanted, put all the exact types (with desired qualifiers
                # and/or attributes attached) in the list.
                if tafWeatherType in ["RA", "SHRA", "SN", "SHSN", "PL"]:
                    if intensity == "--":
                        rating = 10
                    elif intensity == "-":
                        rating = 20
                    elif intensity == "m":
                        rating = 60
                    elif intensity == "+":
                        rating = 90

                elif tafWeatherType == "DZ":
                    if intensity == "--":
                        rating = 5
                    elif intensity == "-":
                        rating = 10
                    elif intensity == "m":
                        rating = 30
                    elif intensity == "+":
                        rating = 50

                elif tafWeatherType == "FZRA":
                    if intensity == "--":
                        rating = 30
                    elif intensity == "-":
                        rating = 50
                    elif intensity == "m":
                        rating = 75
                    elif intensity == "+":
                        rating = 100

                elif tafWeatherType == "FZDZ":
                    if intensity == "--":
                        rating = 20
                    elif intensity == "-":
                        rating = 30
                    elif intensity == "m":
                        rating = 60
                    elif intensity == "+":
                        rating = 90

                elif tafWeatherType == "FZFG":
                    if intensity == "<NoInten>":
                        rating = 30
                    elif intensity == "+":
                        rating = 80

                elif tafWeatherType in ["BLSN", "BLSA", "BLDU", "FU"]:
                    rating = 50

                elif tafWeatherType == "VA":
                    rating = 100

                elif tafWeatherType == "BR":
                    if intensity == "<NoInten>":
                        rating = 50
                    elif intensity == "+":
                        rating = 90

                elif tafWeatherType == "HZ":
                    rating = 20

                if rating > maxRating:
                    maxRating = rating

            return maxRating

        def _skySignificance(self, sky):
            # sky is a number in units of percent

            maxSkyPercentages = self._textProduct._maxSkyPercentages

            # the higher the sky, the more significant it is
            if sky <= maxSkyPercentages["SKC"]:
                return 0
            elif sky <= maxSkyPercentages["FEW"]:
                return 20
            elif sky <= maxSkyPercentages["SCT"]:
                return 40
            elif sky <= maxSkyPercentages["BKN"]:
                return 60
            else:
                return 80

        def _skyPrimarySignificance(self, sky):
            # Only try rating if this optional weather element is actually defined
            if "SkyPrimary" in self._textProduct._optionalWeatherElements:
                return self._skySignificance(sky)
            else:
                return None

        def _skySecondarySignificance(self, sky):
            # Only try rating if this AWT weather element is actually defined
            if "SkySecondary" in self._textProduct._optionalWeatherElements:
                return self._skySignificance(sky)
            else:
                return None

        def _skyTertiarySignificance(self, sky):
            # Only try rating if this AWT weather element is actually defined
            if "SkySecondary" in self._textProduct._optionalWeatherElements:
                return self._skySignificance(sky)
            else:
                return None

        def _cloudBasePrimarySignificance(self, cloudBasePrimary):
            # cloudBasePrimary is a number in units of 100s of feet
            return 0

        def _cloudBaseSecondarySignificance(self, cloudBaseSecondary):
            # cloudBaseSecondary is a number in units of 100s of feet
            return 0

        def _cloudBaseTertiarySignificance(self, cloudBaseTertiary):
            # cloudBaseTertiary is a number in units of 100s of feet
            return 0
        
        def _ceilingSignificance(self, ceiling):
            # ceiling is a number in units of 100s of feet

            # The lower the ceiling, the more significant it is
            if ceiling < 5:
                return 80
            elif ceiling < 10:
                return 60
            elif ceiling < 30:
                return 20
            else:
                return 0

        def _llwsSignificance(self, llws):
            # direction is a number in units of degrees
            # speed is a number in units of knots
            # height is a number in units of 100s of feet
            (direction, speed, height) = llws

            return 100

        def _popSignificance(self, pop):
            # pop is a number in units of percent

            # the higher the pop, the more significant it is
            if pop <= 44:
                return 0
            elif pop <= 54:
                return 25
            else:
                return 75

        def _cloudsSignificance(self, clouds):
            # clouds is either "SKC" or a list of cloud bases where each cloud
            # base is a tuple of the form: (coverage, height):
            # (string "FEW", "SCT", "BKN", or "OVC", number in 100s of feet)
            if isinstance(clouds, str):
                return 0

            for (coverage, height) in clouds:
                if coverage in ["FEW", "SCT"]:
                    return self._cloudBasePrimarySignificance(height)
                else:
                    return self._ceilingSignificance(height)

        def _flightCategorySignificance(self, flightCategory):
            # flightCategory is a string of either "A", "B", "C", "D", "E",
            # "F#" (where # is a 0-based index to differentiate F categories),
            # or "V" for VFR conditions
    
            return 0
        

    class ShorteningAlgorithms():
        def __init__(self, textProduct, airportIcaoId):
            self._textProduct = textProduct
            self._airportIcaoId = airportIcaoId
            
        # This class ranks all of the weather element changes from one time period to
        # another. Algorithms for each element are independent from one another.
        # THis ranking of forecast changes determines which FMGroups we select for
        # final TAF product.
        class Ranking():
        
            def __init__(self, textProduct):
                
                self._textProduct = textProduct
                
                self._windspeedCategories = [0, 6, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55]
                self._windDirCategories = [0, 10, 15, 20, 25, 30]
                self._windGustCategories = [0, 6, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55]
                self._skyCategories = [0, 6, 31, 56, 87]
                self._cloudBaseCategories = [0, 2, 5, 10, 30, 50, 70, 100]
                self._visibilityCategories = [0.0, 0.25, 0.50, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0]
                self._wxTypes = ["TS", "RA", "SHRA", "DZ", "FZRA", "FZDZ", "SN", "SHSN",
                   "PL", "BR", "FZFG", "BR", "IC", "HZ", "BLSN", "BLSA",
                   "BLDU", "FU", "VA", "GS", "GR",
                       ]
                self._wxIntensities = ["<NoInten>", "--", "-", "m", "+"]
                self._flightCategories = ["A", "B", "C", "D", "E", "F"]
                
            # Generic code to calculate the category
            def _calcCategory(self, value, catList):
        
                for i, cat in enumerate(catList):
                    if value <= cat:
                        return i
                        
                return len(catList)
                
            def calcWindspeedRanking(self, speed1, dir1, speed2, dir2):
                        
                # If one is VRB and the other not: new FMGroup
#                 if (dir1 == "VRB" and dir2 != "VRB") or \
#                    (dir1 != "VRB" and dir2 == "VRB"):
#                     return 100
                
                # Calculate the direction difference
                if type(dir1) is int and type(dir2) is int:
                    dirDiff = abs(dir1 - dir2)
                    if dirDiff > 180:
                        dirDiff = abs(dirDiff - 360)
                else:
                    dirDiff = 0
                    
                # Fetch some stuff we'll need
                lowSpeed = self._textProduct._windSpeedRankLow
                modSpeed = self._textProduct._windSpeedRankModerate
                speedDiff = abs(speed1 - speed2)
                # Calc min and max of the speeds for later use
                minSpeed = min(speed1, speed2)
                maxSpeed = max(speed1, speed2)
                
                # CASE 1: A high enough magnitude difference gets a new FMGroup always
                # Scale the points on the high end for higher wind events.
                if speedDiff >= self._textProduct._windMagDiff:
                    if minSpeed < 50:
                        return 200
                    elif minSpeed >= 50 and minSpeed < 100:
                        return 200 - (speedDiff - self._textProduct._windMagDiff * 1.5) * 10  # 150-200+ points
                    elif minSpeed >= 100:
                        return 200 - (speedDiff - self._textProduct._windMagDiff * 2.0) * 10  # 100-200+ points

                # CASE 2: If both speeds are low, 0 points
                # May need to reread the original value to deal with VRB cases.          
                if maxSpeed <= self._textProduct._variableWindSpeed:
                    return 0
                
                # CASE 2.5: If one speed is at or below VRB speed and the other
                # is above VRB speed, assign a few points based on the speed difference
                if minSpeed <= self._textProduct._variableWindSpeed and \
                    maxSpeed > self._textProduct._variableWindSpeed:
                    return speedDiff * 5  # assign a few points 
                
                # CASE 3: lowest speed is in the low range
                if minSpeed < lowSpeed:
                    if maxSpeed < modSpeed:
                        # If direction difference is big -> 100
                        if dirDiff >= self._textProduct._windDirRankLarge:
                            return 100
                        elif dirDiff == 20:
                            return speedDiff * 5  # (5-45 points)
                        else:  # dirDiff == 10, not significant
                            return 0
                    # CASE 4: One is low the other high
                    if maxSpeed >= modSpeed:
                        return 100

                # Note the next three cases have identical logic but are
                # left here for possible refinement.
                # CASE 5: Lowest speed is moderate
                if minSpeed < modSpeed:
                    if maxSpeed < modSpeed:  # other is moderate too
                        if dirDiff >= self._textProduct._windDirRankLarge:
                            return 100
                        elif dirDiff == 20:
                            return speedDiff * 10  # (10-90 points)
                        else:  # dirDiff == 10, not significant
                            return 0
                    # CASE 6: one speed moderate the other high
                    if maxSpeed >= modSpeed:
                        if dirDiff >= self._textProduct._windDirRankLarge:
                            return 100    
                        elif dirDiff == 20:
                            return speedDiff * 10  # (10-90 points)
                        else:  # dirDiff == 10, not significant
                            return 0
                # CASE 7: both speeds above moderate threshold
                if minSpeed >= modSpeed and maxSpeed >= modSpeed:
                    if dirDiff >= self._textProduct._windDirRankLarge:
                        return 100
                    elif dirDiff == 20:
                        return speedDiff * 10  # (10-90 points)
                    else:  # dirDiff == 10, not significant
                        return 0

            
            def calcWindGustRanking(self, gust1, gust2):
                
                minGustDiff = self._textProduct._windGustDiff

                # Check for None cases
                # Both None
                if gust1 is None and gust2 is None:
                    return 0
                # One or the other is None - return 100
                if ((gust1 is None and gust2 is not None) or \
                   (gust1 is not None and gust2 is None)):
                    return 200
                # Both valid gusts - check for difference
                if abs(gust1 - gust2) >= minGustDiff:
                    return 100
                else:
                    return abs(gust1 - gust2) * 5
        
            def calcSkyRanking(self, sky1, sky2, cloudBase1, cloudBase2):
            
                nonCeiling = [0, 1, 2, 3]
                ceiling = [4, 5]
            
                cat1 = self._calcCategory(sky1, self._skyCategories)
                cat2 = self._calcCategory(sky2, self._skyCategories)
                
                # No interest in Sky changes below BKN
                maxCat = max(cat1, cat2)
                if maxCat in nonCeiling:
                    return 0

                if cat1 == cat2:
                    return 0
                # ceiling cases. If it flips from one to the other, big points
                elif cat1 in nonCeiling and cat2 in ceiling or \
                    cat2 in nonCeiling and cat1 in ceiling:
                    return 200
                # High cloud bases are not as important, so a different algorithm for those
                elif cloudBase1 > 70 and cloudBase2 > 70:
                    return abs(cat1 - cat2) * 5
                
                else:
                    return abs(cat1 - cat2) * 20

                
            def calcCloudBaseRanking(self, cbp1, cbp2, cbs1, cbs2, cbc1, cbc2):
                
#                 print "CBP1: " + str(cbp1) + " CBP2: " + str(cbp2)
#                 print "CBS1: " + str(cbs1) + " CBS2: " + str(cbs2)
#                 print "CBC1: " + str(cbc1) + " CBC2: " + str(cbc2)
                
                # Internal method to rank the change of a single cloud base in time
                # Cloud bases are assumed to have real values (not None).
                def rankCloudChange(cb1, cb2):
                    
                    if cb1 >= 70 and cb2 >= 70:
                        return abs(cb1 - cb2) // 20  # 0-10 points
                    
                    # Calculate the major categories of each base
                    cat1 = self._calcCategory(cb1, self._cloudBaseCategories)
                    cat2 = self._calcCategory(cb2, self._cloudBaseCategories)
                    baseCatDiff = abs(cat1 - cat2)
                
                    if baseCatDiff == 0:
                        return abs(cb1 - cb2) // ((cb1 + cb2) // 2) * 20  # 0 - 20 points
                    elif baseCatDiff == 1:
                        minBase = min(cb1, cb2)
                        if minBase > 30:
                            return 25
                        elif minBase > 10 and minBase <= 30:  # below 3000 ft
                            return abs(cb1 - cb2) * 2  # 40 points max
                        else:  # <= 1000 ft
                            return abs(cb1 - cb2) * 5
                    else:  # baseCatDiff >= 2:
                        return baseCatDiff * 25
            
                    # Should never get here
                    print("ERROR!!! Unknown case in rankCloudChange!!!!!!!!!!!!!!!!!!!!!!!")
                    return 0
                
                # ---- End rankCloudChange
                
                rankSum = 0
                
                if cbp1 is not None and cbp2 is not None:
                    rankSum = rankSum + rankCloudChange(cbp1, cbp2)
                
                if cbs1 is not None and cbs2 is not None:
                    rankSum = rankSum + rankCloudChange(cbs1, cbs2)
                    
                if cbc1 is None and cbc2 is None:
                    return rankSum
                
                # If the Conditional grid starts or stops return a large value to force an FMGroup.
                if (cbc1 is not None and cbc2 is None) or (cbc1 is None and cbc2 is not None):
                    return 200
                
                rankSum = rankSum + rankCloudChange(cbc1, cbc2)
                
                return rankSum
        
            def calcVisibilityRanking(self, vis1, vis2):
                
                if vis1 is None or vis2 is None:
                    return 100
                    
                # Calculate the major categories of each base
                cat1 = self._calcCategory(vis1, self._visibilityCategories)
                cat2 = self._calcCategory(vis2, self._visibilityCategories)
                catDiff = abs(cat1 - cat2)
                
                def calcVisScaling(visValue):
                    visScaling = [(1.0, 50), (3.0, 30), (5.0, 15)]
                    for vis, scale in visScaling:
                        if visValue < vis:
                            return scale
                    return 5
                    
                scale1 = calcVisScaling(vis1)
                scale2 = calcVisScaling(vis2)
                scale = (scale1 + scale2) / 2.0
                
                rank = catDiff * scale
                
                return rank
                
            def calcWxRanking(self, wx1, wx2):
                
                # print "Wx1:" + str(wx1) + "  Wx2: " + str(wx2)

#                 if wx1 is not None:
#                     print "All Wx: " + str(wx1.getAllWeather())
#                     print "All Wx weatherExists: " + str(wx1.weatherExists())
#                     print "Reg wx types: " + str(wx1.regularWeatherTypes)
#                     print "Obs wx types: " + str(wx1.obstructionWeatherTypes)
#                     print "wx1 intensity: " + str(wx1.getMaxIntensity())

                wxIntenTypes = [None, "--", "-", "m", "+"]
                
                if wx1 is None and wx2 is None:
                    return 0
                
                # If no Wx exists that's as good as None
                if wx1 is not None and not wx1.weatherExists():
                    wx1 = None
                if wx2 is not None and not wx2.weatherExists():
                    wx2 = None
                    
                # If one is None and the other not return a big number
                if wx1 is None and wx2 is not None or \
                   wx1 is not None and wx2 is None:
                    return 200

                # Analyze the weather types and intensities
                allWx1 = wx1.getAllWeather()
                allWx2 = wx2.getAllWeather()
                
                # If the number of weather types are different there must be
                # some new or missing weather
                if len(allWx1) != len(allWx2):  # must be some different weather
                    return 200
                
                # Sort so each type can be compared
                allWx1.sort()
                allWx2.sort()
                
#                 print "Sorted Wx: " + str(allWx1) + " " + str(allWx2) 
#                 print "Wx1 inten: " + str(wx1.getMaxIntensity())
#                 print "Wx2 inten: " + str(wx2.getMaxIntensity())
                
                # Chech each weather type one at a time
                for i in range(len(allWx1)):
                    if allWx1[i] == allWx2[i]:
                        # At this point the wx types are identical, check intensities
                        wx1Inten = wx1.getMaxIntensity()
                        wx2Inten = wx2.getMaxIntensity()
                        if wx1Inten == wx2Inten:
                            continue  # same intensity, keep going
                        if wx1Inten in wxIntenTypes and wx2Inten in wxIntenTypes:
                            index1 = wxIntenTypes.index(wx1Inten)
                            index2 = wxIntenTypes.index(wx2Inten)
                            if index1 > index2:
                                factor = 10  # decreasing intensity
                            else:
                                factor = 25  # increasing intensity
                            points = factor * abs(index1 - index2)
                            return points
                    else:
                        # wx types are different
                        return 200
                
                return 0
                
            def calcFlightCategoryRanking(self, flightCat1, flightCat2):

                # oth are None - no points                
                if flightCat1 is None and flightCat2 is None:
                    return 0
                
                # At least one is not None
                if flightCat1 is None or flightCat2 is None:
                    return 200
            
                # Both valid flight cats    
                if flightCat1 == flightCat2:
                    return 0
                
                return 200
                
            def calcLLWSRanking(self, llws1, llws2):
                        
                # Check if both are None
                if llws1 is None and llws2 is None:
                    return 0
                
                # At least one of them is valid (not None)
                # A valid/invalid pair triggers the mandatory value
                if llws1 is None:
                    return 200
                
                if llws2 is None:
                    return 200
                
                # Both are valid.  Extract the parts
                d1, s1, h1 = llws1
                d2, s2, h2 = llws2
                
                # Add points for each component: height, speed, direction
                totalPoints = 0
                
                speedDiff = abs(s1 - s2)
                totalPoints = totalPoints + (speedDiff / 15 * 50)  # 50 points per 15 kts.
        
                dirDiff = abs(d1 - d2)
                if dirDiff > 180:
                    dirDiff = abs(dirDiff - 360)
                totalPoints = totalPoints + (dirDiff / 30.0 * 50)  # 50 pts per 30 deg.
                
                htDiff = abs(h1 - h2)
                totalPoints = totalPoints + (htDiff / 500.0 * 50)  # 50 pts per 500 ft.
                
                return int(totalPoints)
        
        ##############  End - Class Ranking

        
        #######################################################################################
        def getWxDumpStr(self, groupWx):
            wxStr = "None"
            if groupWx is not None:
                wxStr = ""
                for info in groupWx.uglyStringInfos:
                    wxType = info.gfeWeatherType
                    prob = info.probability
                    inten = info.intensity
                    if inten == "<NoInten>":
                        inten = ""
                    wxStr = wxStr + prob + wxType + inten + "." + str(info.tafWeatherType)

            return wxStr
                    
        # Temporary method to format a single string contain the values of all elements
        def makeDumpString(self, fmGroup):
            
            weList = [("Wind:", "wind"), ("Gust:", "windGust"),
                      ("Wx:", "weather"), ("Sky:", "sky"), ("CBP^S^C:", "cloudBase"),
                      ("FC:", "flightCategory"), ("Vis:", "visibility"), ("LLWS:", "llws")
                       ]
#             rankDict = fmGroup.rankDict
            
            dumpStr = "+& " + fmGroup._airportDict["icaoAirportIndicator"] 
            dumpStr = dumpStr + " FcstHr:" + fmGroup.productDict["groupLabel"] + " "
            for label, weName in weList:
                if weName in fmGroup.rankDict:
                    pointStr = ">" + str(int(fmGroup.rankDict[weName]))
                else:
                    pointStr = ""
                if weName == "weather":
                    dumpStr = dumpStr + " " + label + self.getWxDumpStr(fmGroup.productDict[weName]) + pointStr
                elif weName == "cloudBase":
                    dumpStr = dumpStr + " " + label + str(fmGroup.productDict["cloudBasePrimary"]) + "^" + \
                              str(fmGroup.productDict["cloudBaseSecondary"]) + pointStr
                else:
                    dumpStr = dumpStr + " " + label + str(fmGroup.productDict[weName]) + pointStr
#                 if rankDict is not None:
#                     dumpStr = dumpStr + ">" + str(rankDict[weName]) + " "
            dumpStr = dumpStr + " Total:", str(fmGroup.rankingValue)
            
            return dumpStr
        
        # Figure out the time weighting factor based on the time period and the 
        # configured timeWeighting list. Return as a number between 0 and 1 for
        # easier calculations.
        def determineTimeWeighting(self, periodHour):
            
            finalWeight = 100  # initialize
            for hour, weight in self._textProduct._timeWeighting:
                if hour <= periodHour:
                    finalWeight = weight
                    continue
                else:
                    break
                
            return finalWeight / 100.0
            
        def rankElementChange(self, rankingObj, fmGroups, we, prev, curr):
            
            if we == "wind":
                dir1, speed1 = fmGroups[prev].productDict[we]
                dir2, speed2 = fmGroups[curr].productDict[we]
                windRank = rankingObj.calcWindspeedRanking(speed1, dir1, speed2, dir2)
                # windRank = windRank * self.determineTimeWeighting(curr)
                return windRank 
            elif we == "windGust":
                gust1 = fmGroups[prev].productDict[we]
                gust2 = fmGroups[curr].productDict[we]
                gustRank = rankingObj.calcWindGustRanking(gust1, gust2)
                # gustRank = gustRank * self.determineTimeWeighting(curr)
                return gustRank
            elif we == "sky":
                sky1 = fmGroups[prev].productDict[we]
                sky2 = fmGroups[curr].productDict[we]
                cloudBase1 = fmGroups[prev].productDict["cloudBasePrimary"]
                cloudBase2 = fmGroups[curr].productDict["cloudBasePrimary"]
                return rankingObj.calcSkyRanking(sky1, sky2, cloudBase1, cloudBase2)
            
            elif we == "weather":
                wx1 = fmGroups[prev].productDict[we]
                wx2 = fmGroups[curr].productDict[we]
                  
                return rankingObj.calcWxRanking(wx1, wx2)
            
            elif we == "cloudBase":
                # Fetch primary and secondary cloud groups
                cbp1 = fmGroups[prev].productDict["cloudBasePrimary"]
                cbp2 = fmGroups[curr].productDict["cloudBasePrimary"]
                cbs1 = fmGroups[prev].productDict["cloudBaseSecondary"]
                cbs2 = fmGroups[curr].productDict["cloudBaseSecondary"]
                # Conditional clouds are harbored in the conditionalGroup
                cbc1 = None
                condGroup1 = fmGroups[prev].productDict["conditionalGroup"]
                if condGroup1 is not None:
                    cbc1 = condGroup1.productDict["cloudBaseConditional"]
                cbc2 = None
                condGroup2 = fmGroups[curr].productDict["conditionalGroup"]
                if condGroup2 is not None:
                    cbc2 = condGroup2.productDict["cloudBaseConditional"]
                
                return rankingObj.calcCloudBaseRanking(cbp1, cbp2, cbs1, cbs2, cbc1, cbc2)
            
            elif we == "flightCategory":
                flightCat1 = fmGroups[prev].productDict[we]
                flightCat2 = fmGroups[curr].productDict[we]
                return rankingObj.calcFlightCategoryRanking(flightCat1, flightCat2)
            elif we == "visibility":
                return rankingObj.calcVisibilityRanking(fmGroups[prev].productDict[we],
                                                        fmGroups[curr].productDict[we])
            elif we == "llws":
                return rankingObj.calcLLWSRanking(fmGroups[prev].productDict[we],
                                                  fmGroups[curr].productDict[we])
            else:
                print("Unknown weather element", we, "in rankElementChange.")

            return 0

        @property
        def rankingSort(self):
            def cmpfunc(a, b):
                periodA, rankA = a
                periodB, rankB = b
                if rankA < rankB:
                    return 1
                elif rankB < rankA:
                    return -1
                return 0
            return functools.cmp_to_key(cmpfunc)

        def calculateRank(self, fmGroups, index1, index2):
            
            rankingObj = self.Ranking(self._textProduct)  # the object that will rank the elements

            elementList = ["wind", "windGust", "sky", "weather", "cloudBase",
                           "flightCategory", "visibility", "llws"]
            rankSum = 0
            
            # Loop through each element and sum the rank over all of them.
            for we in elementList:
                rank = self.rankElementChange(rankingObj, fmGroups, we, index1, index2)
                fmGroups[index2].rankDict[we] = rank
                rankSum = rankSum + rank
                
            fmGroups[index2].rankingValue = rankSum
            
            return rankSum
        
        # This method finds appropriate FMGroups by ranking them and comparing the rank to
        # the specified threshold. Once a new FMGroup meets that criteria, future FMGroups
        # are compared to the last found FMGroups
        def findKeepers(self, fmGroups, threshold):
            
            def fcstHour(i):
                startTime = fmGroups[i].productDict["startTime"]
                return str(time.gmtime(startTime).tm_mday) + "/" + str(time.gmtime(startTime).tm_hour) + "Z "

            # The first FMGroup is always included, so initialize with that.
            # This format of each item is (position, rankScore).
            keepers = [(0, 0)]  # Assign a fake 
            
            baseIndex = 0
            fmIndex = 1
            timeFactor = 0.0
            while fmIndex < len(fmGroups):
                rank = self.calculateRank(fmGroups, baseIndex, fmIndex)
                # Increase the rank slightly as the time interval gets longer.
                # This will make it harder to get long time gaps in the TAF and
                # has the side effect of filtering out some FMGroups during long trends
                rank = rank + (fmIndex - baseIndex - 1) * timeFactor
                print("Comparing " + str(fcstHour(baseIndex)) + "to " + str(fcstHour(fmIndex)) + "...Rank:" + str(rank)) 
                if rank >= threshold and rank > 0:
                    keepers.append((fmIndex, rank))
                    baseIndex = fmIndex
                
                fmIndex = fmIndex + 1
                
            return keepers
                
        #######################################################################################
        
        def makeFMGroupList(self, fmGroups, keepers):
            
            # Extract the FMGroups from the full list and the keepers
            keepers.sort()
            fmGroupList = []
            
            for position, rank in keepers:
                fmGroupList.append(fmGroups[position])
            
            # Dump groups to log file....for now.
            print("+&         Final KEEPERS..............................")
            for fm in fmGroupList:
                print(self.makeDumpString(fm))

            return fmGroupList
                
        def shortenFmGroups(self, fmGroups):
            """
            This will try to shorten the TAF output so that it hopefully does
            not exceed the desired number of periods (specified using the
            'maxFmGroups' configuration setting) while still trying to be as
            meteorologically correct as possible and conveying operationally
            significant information.
            """
            maxFmGroups = self._textProduct._maxFmGroups
            self._textProduct.debug_print("\n" + "=" * 80 + 
                "\nShortening FM groups for " + 
                "%s (number of periods = %s, max allowed = %s)"
                % (self._airportIcaoId, len(fmGroups), maxFmGroups), 1)
            
            # Dump all the groups for post mortem diagnostics.
            print("ALL FMGROUPS:")
            for fm in fmGroups:
                print(self.makeDumpString(fm))
            
            # Define a set of thresholds in decreasing order. We may use 
            # all of them if not enough FMGroups are found.
            mandatoryThreshold = 200
            thresholds = list(range(mandatoryThreshold, 10, -5))
            print("Thresholds:" + str(thresholds))
            print("GUI Detail Level: " + str(self._textProduct._detail))
            for threshold in thresholds:
                keepers = self.findKeepers(fmGroups, threshold)

                print(str(len(keepers)) + " with threshold: " + str(threshold))
                
                if len(keepers) >= self._textProduct._detail:
                     if threshold == mandatoryThreshold:  # Must haves
                         # print "Returning MANDATORY keepers........................"
                         print("Returning keepers matching MANDATORY THRESHOLD......")
                         return self.makeFMGroupList(fmGroups, keepers)
                     else:  # Too many, so cull the herd
                         keepers.sort(key=self.rankingSort)
                         print("Untrimmed Ranked Keepers:" + str(keepers))
                         keepers = keepers[0:self._textProduct._detail - 1]
                         keepers.insert(0, (0, 0))  # Prepend the first group 
                         print("Returning Trimmed Ranked Keepers:" + str(keepers))
                         return self.makeFMGroupList(fmGroups, keepers)

#                 elif len(keepers) >= self._textProduct._minFmGroups:  # We're in range
#                     print "Returning keepers with threshold:" + str(threshold)
#                     return self.makeFMGroupList(fmGroups, keepers)

            print("Still not enough FMGroups after all thresholds.")
            print("Keepers:", str(keepers))
        
            return self.makeFMGroupList(fmGroups, keepers)
            
            ##########################################################################
            

        def _orderedShorteningAlgorithms(self):
            """
            List of ordered shortening algorithms to try to attempt to minimize
            the number of FM groups in the output.
            """
            return [  # If a period is similar enough to the previous period,
                    # throw it away.
                    self._removeSimilarPeriods,

                    # Check if periods show increasing or decreasing trends
                    # (all changes have to be due to trends) and if so, get rid
                    # of the middle periods.
                    self._shortenTrends,

                    # If each consecutive hour contains a single change, merge
                    # all 3 changes into the middle hour (current FM group) and
                    # throw away the previous and next FM groups. Some of the
                    # changed fields require the same product parts and so if
                    # two FM groups need the same product part, the
                    # significance ratings are used to determine which FM
                    # group's data to use.
                    self._combineSingleChangeConsecutiveHours,

                    # Keep periods with the most number of changes and
                    # discard the rest.
                    self._eliminateFewestChanges,

                    # Going through in reverse order, if you have 3 consecutive
                    # FM groups and they all have the same changed fields and
                    # the product parts associated with each changed field are
                    # all non-significant (i.e. signficance rating <=
                    # maxNonSignificantRating), remove the middle FM group.
                    self._eliminateConsecutiveChanges,

                    # Going through in reverse order, if you have 3 consecutive
                    # FM groups, arbitrarily remove the middle FM group.
                    self._eliminateMiddlePeriods,
            ]

        def _removeSimilarPeriods(self, fmGroups, maxFmGroups):
            self._textProduct.debug_print(
                "\nRemoving similar periods for %s" % self._airportIcaoId, 1)

            keptFmGroups = []

            # See how many FM groups we have so far
            numFmGroups = len(fmGroups)

            # Always keep the first period
            if numFmGroups > 0:
                similarChecks = OrderedDict()
                similarChecks["Wind"] = False
                similarChecks["WindGust"] = False
                similarChecks["Weather"] = False
                similarChecks["Sky"] = False
                similarChecks["LLWS"] = False
                similarChecks["FlightCategory"] = False

                fmGroups[0].productDict["similarChecks"] = similarChecks
                fmGroups[0].productDict["numChanges"] = \
                    len(list(similarChecks.keys()))

                keptFmGroups.append(fmGroups[0])

            # Start at the 2nd FM group (index 1) so that there is always a
            # previous period
            for index in range(1, numFmGroups, 1):
                self._periodIndex = index
                fmGroup = fmGroups[self._periodIndex]

                self._textProduct.debug_print("\n" + "_" * 80 + 
                    "\nWorking on %s" % fmGroup, 1)

                similarChecks = OrderedDict()

                similarChecks["Wind"] = self._isWindSimilar(fmGroups)
                similarChecks["WindGust"] = self._isWindGustSimilar(fmGroups)
                similarChecks["Weather"] = self._isWeatherSimilar(fmGroups)
                similarChecks["Sky"] = self._isSkySimilar(fmGroups)
                similarChecks["LLWS"] = self._isLlwsSimilar(fmGroups)
                similarChecks["FlightCategory"] = \
                    self._isFlightCategorySimilar(fmGroups)

                # Save how many changes this FM group had so the information
                # can be used in other shortening algorithms
                fmGroup.productDict["similarChecks"] = similarChecks
                fmGroup.productDict["numChanges"] = \
                    list(similarChecks.values()).count(False)

                if not all(similarChecks.values()):
                    self._textProduct.debug_print("\nKeeping %s" % fmGroup, 1)
                    keptFmGroups.append(fmGroup)
                else:
                    self._textProduct.debug_print("\nRemoving %s" % fmGroup, 1)

            # Let user know what has happened
            if numFmGroups != len(keptFmGroups):
                self._textProduct.debug_print(
                    "\nShortened TAF by eliminating identical periods.  " + 
                    "(%d periods left, %d allowed)" % 
                    (len(keptFmGroups), maxFmGroups), 1)

            return keptFmGroups

        def _shortenTrends(self, fmGroups, maxFmGroups):
            """
            Shorten the TAF output by examining trends within data. The first
            hour of the forecast is always kept and becomes the basis for
            comparison. Each subsequent forecast group is searched in ascending
            order and compared to the current basis to determine trends. The
            last forecast group is also always kept to indicate the 'end state'
            of the forecast trends. Note that while we iterate forward, it's
            the later periods that get removed first (it's better to have more
            detail in the beginning hours than the later hours).
            """
            self._textProduct.debug_print(
                "\nShortening trends for %s" % self._airportIcaoId, 1)

            # Make a copy of the FM groups we are working with
            newFmGroups = fmGroups[:]

            # Save off the FM groups to remove because at the end, we will
            # remove the later FM groups first
            fmGroupsToRemove = []

            # See how many FM groups we have so far
            numFmGroups = len(fmGroups)

            indices = list(range(1, numFmGroups - 1, 1))
            self._textProduct.debug_print("%s groups; looping over indices %s"
                                          % (numFmGroups, repr(indices)), 1)

            # Now look at each potential FM group we could eliminate, starting
            # from the beginning of the forecast. Remember we are already
            # keeping the first and last FM group.
            #
            # FM groups marked for removal will still be looked at while
            # iterating so that if there is a block of increasing or decreasing
            # periods, it will be reduced.
            trendCheckers = {
                "Wind":           self._isWindTrend,
                "WindGust":       self._isWindGustTrend,
                "Weather":        self._isWeatherTrend,
                "Sky":            self._isSkyTrend,
                "LLWS":           self._isLLWSTrend,
                "FlightCategory": self._isFlightCategoryTrend, }

            for index in indices:
                prevFmGroup = fmGroups[index - 1]
                currFmGroup = fmGroups[index]
                nextFmGroup = fmGroups[index + 1]

                similarChecks = currFmGroup.productDict["similarChecks"]

                self._textProduct.debug_print("\n\tindex = %s" % index, 1)
                self._textProduct.debug_print("\tprevious = %s"
                    % prevFmGroup, 1)
                self._textProduct.debug_print("\tcurrent = %s"
                    % currFmGroup, 1)
                self._textProduct.debug_print("\tnext = %s" % nextFmGroup, 1)

                self._textProduct.debug_print("\n\t\tsimilarity checks = %s\n"
                    % repr(similarChecks), 1)

                #--------------------------------------------------------------
                # Check if every change is due to a trend
                allChangesDueToTrends = True

                nonSimilarFields = self._getNonSimilarFields(similarChecks)

                for field in nonSimilarFields:
                    trendChecker = trendCheckers[field]

                    isTrend = trendChecker(prevFmGroup.productDict,
                                           currFmGroup.productDict,
                                           nextFmGroup.productDict)

                    # If this is not a trend then force a FM group
                    if not isTrend:
                        allChangesDueToTrends = False
                        break

                if not allChangesDueToTrends:
                    self._textProduct.debug_print("\n\t\tMoving on: " + 
                        "Not all changes were due to trends", 1)

                # All changes are due to trends
                else:
                    self._textProduct.debug_print(
                        "\n\t\t'%s' able to be removed: " % currFmGroup + 
                        "All changes were due to trends!", 1)

                    # A clear trend has been established - eliminate the
                    # current FM group (save it off in reverse order so that
                    # later periods get removed first)
                    fmGroupsToRemove.insert(0, currFmGroup)

            for fmGroup in fmGroupsToRemove:
                self._textProduct.debug_print(
                    "\t\tEliminating current period: %s" % fmGroup, 1)

                newFmGroups.remove(fmGroup)

                # If we are now at the desired length - stop looking
                if len(newFmGroups) == maxFmGroups:
                    break

            # Let user know what has happened
            if numFmGroups != len(newFmGroups):
                self._textProduct.debug_print(
                    "\nShortened TAF by eliminating intermediate groups of " + 
                    "established trends.  (%d periods left, %d allowed)" % 
                    (len(newFmGroups), maxFmGroups), 1)

            # Return the FM groups we have left
            return newFmGroups

        def _combineSingleChangeConsecutiveHours(self, fmGroups, maxFmGroups):
            # Count the number of FM groups we have in this TAF right now
            numFmGroups = len(fmGroups)

            # Get the index of the second to last FM group - remember Python
            # starts counting at zero so we need to subtract 1 more
            index = numFmGroups - 2

            #------------------------------------------------------------------
            # Examine these FM groups working backwards while there are still
            # FM groups to examine and the TAF is still too long. Use
            # index >= 2 check to ensure we never remove the first FM group.
            while index >= 2 and len(fmGroups) > maxFmGroups:
                prevFmGroup = fmGroups[index - 1]
                currFmGroup = fmGroups[index]
                nextFmGroup = fmGroups[index + 1]

                hourDifference = nextFmGroup.period - prevFmGroup.period

                prevSimilarChecks = prevFmGroup.productDict["similarChecks"]
                currSimilarChecks = currFmGroup.productDict["similarChecks"]
                nextSimilarChecks = nextFmGroup.productDict["similarChecks"]

                self._textProduct.debug_print("Looking at %s" % currFmGroup, 1)
                self._textProduct.debug_print("Hour difference between " + 
                    "next (%s) and previous (%s) = %s"
                    % (nextFmGroup, prevFmGroup, hourDifference), 1)

                # If we have three consecutive groups (indicated by a two hour
                # difference)
                if hourDifference == 2:
                    prevChanges = self._getNonSimilarFields(prevSimilarChecks)
                    currChanges = self._getNonSimilarFields(currSimilarChecks)
                    nextChanges = self._getNonSimilarFields(nextSimilarChecks)

                    self._textProduct.debug_print("\tprev changes: %s"
                                                  % prevChanges, 1)
                    self._textProduct.debug_print("\tcurr changes: %s"
                                                  % currChanges, 1)
                    self._textProduct.debug_print("\tnext changes: %s"
                                                  % nextChanges, 1)

                    # Gather all changed fields and eliminate duplicates
                    allChanges = set(prevChanges + currChanges + nextChanges)

                    # If each FM group had only 1 changed field and they are
                    # all different (if set length is 3 because sets can't
                    # contain duplicates)
                    if len(prevChanges) == 1 and len(currChanges) == 1 and \
                       len(nextChanges) == 1 and len(allChanges) == 3:

                        self._textProduct.debug_print(
                            "\n\t\tCombining previous and next periods into " + 
                            "current period '%s' then removing them"
                            % currFmGroup, 1)

                        currFmGroup = \
                            self._combineFmGroupDicts(currFmGroup, prevFmGroup)
                        currFmGroup = \
                            self._combineFmGroupDicts(currFmGroup, nextFmGroup)

                        self._textProduct.debug_print(
                            "\tEliminating previous period '%s'"
                            % prevFmGroup, 1)

                        fmGroups.remove(prevFmGroup)

                        self._textProduct.debug_print(
                            "\tEliminating next period '%s'" % nextFmGroup, 1)

                        fmGroups.remove(nextFmGroup)

                        # Update the search index - go back 3 groups since
                        # the previous FM group wasn't kept
                        index = index - 3
                    else:
                        index = index - 1

                # Otherwise, move backward one group
                else:
                    index = index - 1
                
                self._textProduct.debug_print(" ", 1)

            # Let user know what has happened
            if numFmGroups != len(fmGroups):
                self._textProduct.debug_print("Shortened TAF by " + 
                    "combining single change consecutive FM groups.  " + 
                    "(%d periods left, %d allowed)" % 
                    (len(fmGroups), maxFmGroups), 1)

            # Return the (hopefully) shortened list of FM groups
            return fmGroups

        def _eliminateFewestChanges(self, fmGroups, maxFmGroups):
            """
            Shorten the TAF output by examining the number of changed fields in
            each FM group and preferentially keeping those with the greatest
            number of changes. Since there can be many FM groups with the same
            number of field changes, the TAF could still be too long after this
            process. 
            """

            #------------------------------------------------------------------
            # Track the number of changes we have each hour
            changesDict = {}
            for index, fmGroup in enumerate(fmGroups):
                numChanges = fmGroup.productDict["numChanges"]

                # Save the number of changes and index of this FM group
                if numChanges in changesDict:
                    changesDict[numChanges].append(index)  # add it
                else:
                    changesDict[numChanges] = [index]  # create a new key

            #------------------------------------------------------------------
            # Keep adding FM groups while the TAF is still short enough and we
            # still have more FM groups to add
            newIndices = []

            # Start with the most field changes in an hour and work backwards
            numChangesList = list(changesDict.keys())
            numChangesList.sort(reverse=True)

            for numChanges in numChangesList:
                if len(newIndices) < maxFmGroups:
                    # Add the indices for these FM groups to our new list
                    newIndices += changesDict[numChanges]

                    action = "Kept"
                else:
                    action = "Removed"

                self._textProduct.debug_print("%s periods with %s changes:"
                    % (action, numChanges), 1)
                for index in changesDict[numChanges]:
                    self._textProduct.debug_print("\t%s" % fmGroups[index], 1)

            #------------------------------------------------------------------
            # Create a new (hopefully shorter) list of FM groups using the list
            # of indices we built up
            newFmGroups = []

            # Put the FM group indices in ascending/chronological order
            newIndices.sort()

            # Add the FM groups for these indices to our new list
            for index in newIndices:
                newFmGroups.append(fmGroups[index])

            # Let user know what has happened
            if len(newFmGroups) != len(fmGroups):
                self._textProduct.debug_print(
                    "\nShortened TAF as much as possible " + 
                    "based on the number of field changes.  " + 
                    "(%d periods left, %d allowed)" % 
                    (len(newFmGroups), maxFmGroups), 1)

            # Return the (hopefully) shortened list of FM groups
            return newFmGroups

        def _eliminateConsecutiveChanges(self, fmGroups, maxFmGroups):
            # Count the number of FM groups we have in this TAF right now
            numFmGroups = len(fmGroups)

            # Get the index of the second to last FM group - remember Python
            # starts counting at zero so we need to subtract 1 more
            index = numFmGroups - 2

            #------------------------------------------------------------------
            # Examine these FM groups working backwards while there are still
            # FM groups to examine and the TAF is still too long
            while index > 0 and len(fmGroups) > maxFmGroups:
                prevFmGroup = fmGroups[index - 1]
                currFmGroup = fmGroups[index]
                nextFmGroup = fmGroups[index + 1]

                hourDifference = nextFmGroup.period - prevFmGroup.period

                prevSimilarChecks = prevFmGroup.productDict["similarChecks"]
                currSimilarChecks = currFmGroup.productDict["similarChecks"]
                nextSimilarChecks = nextFmGroup.productDict["similarChecks"]

                self._textProduct.debug_print("Looking at %s" % currFmGroup, 1)
                self._textProduct.debug_print("Hour difference between " + 
                    "next (%s) and previous (%s) = %s"
                    % (nextFmGroup, prevFmGroup, hourDifference), 1)

                safeToRemoveMiddlePeriod = False

                # If we have three consecutive FM groups (indicated by a 2 hour
                # difference)
                if hourDifference == 2:
                    prevChanges = self._getNonSimilarFields(prevSimilarChecks)
                    currChanges = self._getNonSimilarFields(currSimilarChecks)
                    nextChanges = self._getNonSimilarFields(nextSimilarChecks)

                    self._textProduct.debug_print("\tprev changes: %s"
                                                  % prevChanges, 1)
                    self._textProduct.debug_print("\tcurr changes: %s"
                                                  % currChanges, 1)
                    self._textProduct.debug_print("\tnext changes: %s"
                                                  % nextChanges, 1)

                    currRatings = currFmGroup.productDict["ratings"]

                    self._textProduct.debug_print(
                        "\n\t\tmaximum non-significant rating threshold: %s"
                        % self._textProduct._maxNonSignificantRating, 1)
                    self._textProduct.debug_print("\tcurrent ratings:", 1)
                    for (productPart, rating) in currRatings.items():
                        self._textProduct.debug_print("\t\t%s = %s"
                            % (productPart, rating), 1)

                    safeToRemoveMiddlePeriod = True
                    for field in currChanges:
                        self._textProduct.debug_print("\n\t\tChecking %s"
                                                      % field, 1)

                        # Make sure each changed field was changed in all 3
                        # periods
                        if field in prevChanges and field in nextChanges:
                            # Make sure each product part needed for this
                            # changed field is not significant
                            partsNeeded = \
                                self._partsNeededForChangedField(field)

                            self._textProduct.debug_print(
                                "\t\tLooking at significance ratings for: %s"
                                % partsNeeded, 1)

                            for productPart in partsNeeded:
                                if (productPart in currRatings and
                                    currRatings[productPart] > 
                                    self._textProduct._maxNonSignificantRating):

                                    safeToRemoveMiddlePeriod = False
                                    break
                        else:  # Not all 3 periods had the changed field
                            safeToRemoveMiddlePeriod = False
                            break

                self._textProduct.debug_print(
                    "\n\t\tSafe to remove middle period? %s"
                    % safeToRemoveMiddlePeriod, 1)

                if safeToRemoveMiddlePeriod:
                    self._textProduct.debug_print(
                        "\n\t\tRemoving middle period '%s'\n" % currFmGroup, 1)

                    # Eliminate the middle period
                    del fmGroups[index]

                    # Update the search index - go back 2 groups since
                    # the current FM group wasn't kept
                    index = index - 2
                else:
                    self._textProduct.debug_print(" ", 1)
                    index = index - 1

            # Let user know what has happened
            if numFmGroups != len(fmGroups):
                self._textProduct.debug_print(
                    "\nShortened TAF by eliminating middle groups of " + 
                    "consecutive FM groups with the same changed fields.  " + 
                    "(%d periods left, %d allowed)" % 
                    (len(fmGroups), maxFmGroups), 1)

            # Return the (hopefully) shortened list of FM groups
            return fmGroups

        def _eliminateMiddlePeriods(self, fmGroups, maxFmGroups):
            """
            Shorten the TAF output by examining the amount of time spanned by
            blocks of 3 FM groups. If 3 FM groups are consecutive (for example:
            FM0500Z, FM0600Z and FM0700Z), then the middle FM group will be
            arbitrarily removed (e.g. FM0500Z and FM0700Z are kept). The TAF
            could still be too long after this process.
            """

            # Count the number of FM groups we have in this TAF right now
            numFmGroups = len(fmGroups)

            # Get the index of the second to last FM group - remember Python
            # starts counting at zero so we need to subtract 1 more
            index = numFmGroups - 2

            #------------------------------------------------------------------
            # Examine these FM groups working backwards while there are still
            # FM groups to examine and the TAF is still too long
            while index > 0 and len(fmGroups) > maxFmGroups:
                prevFmGroup = fmGroups[index - 1]
                currFmGroup = fmGroups[index]
                nextFmGroup = fmGroups[index + 1]

                hourDifference = nextFmGroup.period - prevFmGroup.period

                self._textProduct.debug_print("Looking at %s" % currFmGroup, 1)
                self._textProduct.debug_print("Hour difference between " + 
                    "next (%s) and previous (%s) = %s"
                    % (nextFmGroup, prevFmGroup, hourDifference), 1)

                # If we have three consecutive FM groups (indicated by a 2 hour
                # difference)
                if hourDifference == 2:
                    self._textProduct.debug_print(
                        "\n\t\tRemoving middle period '%s'\n" % currFmGroup, 1)

                    # Eliminate the middle group
                    del fmGroups[index]

                    # Update the search index - go back 2 groups since
                    # the current FM group wasn't kept
                    index = index - 2

                # Otherwise, move backward one group
                else:
                    self._textProduct.debug_print(" ", 1)
                    index = index - 1

            # Let user know what has happened
            if numFmGroups != len(fmGroups):
                self._textProduct.debug_print(
                    "\nShortened TAF by eliminating middle FM group of " + 
                    "consecutive FM groups.  (%d periods left, %d allowed)" % 
                    (len(fmGroups), maxFmGroups), 1)

            # Return the (hopefully) shortened list of FM groups
            return fmGroups

        def _getNonSimilarFields(self, similarChecks):
            """Get the list of non similar fields for a period."""

            return [fieldName for fieldName in similarChecks.keys() if not similarChecks[fieldName]]

        def _isWindTrend(self, prevFmGroupDict, currFmGroupDict, nextFmGroupDict):
            self._textProduct.debug_print("\tChecking for Wind trend", 1)

            # TODO: Handle trends where prev or next is None but the others
            # aren't
            if prevFmGroupDict["wind"] is None or \
               currFmGroupDict["wind"] is None or \
               nextFmGroupDict["wind"] is None:

                return False

            (prevDirection, prevSpeed) = prevFmGroupDict["wind"]
            (currDirection, currSpeed) = currFmGroupDict["wind"]
            (nextDirection, nextSpeed) = nextFmGroupDict["wind"]

            # Direction change is a trend and speeds are similar
            # TODO: Handle VRB cases??
            isDirectionTrend = (
                ("VRB" not in [prevDirection, currDirection, nextDirection])
                and
                self._isNumericalTrend(
                    prevDirection, currDirection, nextDirection,
                    "Wind (Direction)")
                and
                self._allWindSpeedsSimilar(
                    prevFmGroupDict["wind"], currFmGroupDict["wind"],
                    nextFmGroupDict["wind"], "WIND_MAG"))

            # Speed change is a trend and directions are similar
            isSpeedTrend = (
                self._isNumericalTrend(
                    prevSpeed, currSpeed, nextSpeed, "Wind (Speed)")
                and
                self._allWindDirectionsSimilar(
                    prevFmGroupDict["wind"], currFmGroupDict["wind"],
                    nextFmGroupDict["wind"], "WIND_DIR"))

            # TODO: Handle case where both direction and speed are trends???

            isWindTrend = (isDirectionTrend or isSpeedTrend)

            self._textProduct.debug_print(
                "\t\tFound a trend? %s" % isWindTrend, 1)

            return isWindTrend

        def _isWindGustTrend(self, prevFmGroupDict, currFmGroupDict, nextFmGroupDict):
            return self._isNumericalTrend(prevFmGroupDict["windGust"],
                                          currFmGroupDict["windGust"],
                                          nextFmGroupDict["windGust"],
                                          "WindGust")

        def _isWeatherTrend(self, prevFmGroupDict, currFmGroupDict, nextFmGroupDict):
            self._textProduct.debug_print("\tChecking for Weather trend", 1)

            prevWeather = prevFmGroupDict["weather"]
            currWeather = currFmGroupDict["weather"]
            nextWeather = nextFmGroupDict["weather"]
            self._textProduct.debug_print(
                "\t\tprev = %s\n\t\t\tcurr = %s\n\t\t\tnext = %s"
                % (repr(prevWeather), repr(currWeather), repr(nextWeather)), 1)

            if (prevWeather is not None) and \
               (currWeather is not None) and \
               (nextWeather is not None):

                prevRegularWeather = set(prevWeather.regularWeatherTypes)
                nextRegularWeather = set(nextWeather.regularWeatherTypes)
                allPrecipitationTypes = set(self._textProduct.
                                            GroupWeatherInfo.
                                            precipitationWeatherTypes)
                #--------------------------------------------------------------
                # Check for VCSH ramping up to precipitation next period:
                # no precip -> VCSH -> precip
                if (len(prevRegularWeather.
                        intersection(allPrecipitationTypes)) == 0
                    and
                    currWeather.vicinityWeather == "VCSH"
                    and
                    len(nextRegularWeather.
                        intersection(allPrecipitationTypes)) > 0):

                    self._textProduct.debug_print(
                        "\t\tFound a trend! VCSH ramping up next period", 1)
                    return True

                #--------------------------------------------------------------
                # Check for VCSH ramping down from precipitation next period:
                # precip -> VCSH -> no precip
                if (len(prevRegularWeather.
                        intersection(allPrecipitationTypes)) > 0
                    and
                    currWeather.vicinityWeather == "VCSH"
                    and
                    len(nextRegularWeather.
                        intersection(allPrecipitationTypes)) == 0):

                    self._textProduct.debug_print(
                        "\t\tFound a trend! VCSH ramping down next period", 1)
                    return True

            return False

        def _isSkyTrend(self, prevFmGroupDict, currFmGroupDict, nextFmGroupDict):
            # TODO: Make this better now that we have the clouds product part
            return self._isNumericalTrend(prevFmGroupDict["sky"],
                                          currFmGroupDict["sky"],
                                          nextFmGroupDict["sky"],
                                          "Sky")

        def _isLLWSTrend(self, prevFmGroupDict, currFmGroupDict, nextFmGroupDict):
            self._textProduct.debug_print("\tChecking for LLWS trend", 1)

            # TODO: Handle trends where prev or next is None but the others
            # aren't
            if prevFmGroupDict["llws"] is None or \
               currFmGroupDict["llws"] is None or \
               nextFmGroupDict["llws"] is None:

                return False

            (prevDirection, prevSpeed, prevHeight) = prevFmGroupDict["llws"]
            (currDirection, currSpeed, currHeight) = currFmGroupDict["llws"]
            (nextDirection, nextSpeed, nextHeight) = nextFmGroupDict["llws"]

            allDirectionsSimilar = self._allWindDirectionsSimilar(
                                    (prevDirection, prevSpeed),
                                    (currDirection, currSpeed),
                                    (nextDirection, nextSpeed),
                                    "LLWS_DIR")

            allSpeedsSimilar = self._allWindSpeedsSimilar(
                                (prevDirection, prevSpeed),
                                (currDirection, currSpeed),
                                (nextDirection, nextSpeed),
                                "LLWS_MAG")

            allHeightsSimilar = (prevHeight == currHeight == nextHeight)

            # Direction change is a trend and speeds and heights are similar
            isDirectionTrend = (self._isNumericalTrend(prevDirection,
                                                       currDirection,
                                                       nextDirection,
                                                       "LLWS (Direction)")
                                and allSpeedsSimilar
                                and allHeightsSimilar)

            # Speed change is a trend and directions and heights are similar
            isSpeedTrend = (self._isNumericalTrend(prevSpeed,
                                                   currSpeed,
                                                   nextSpeed,
                                                   "LLWS (Speed)")
                            and allDirectionsSimilar
                            and allHeightsSimilar)

            # Height change is a trend and directions and speeds are similar
            isHeightTrend = (self._isNumericalTrend(prevHeight,
                                                    currHeight,
                                                    nextHeight,
                                                    "LLWS (Height)")
                             and allDirectionsSimilar
                             and allSpeedsSimilar)

            isLlwsTrend = (isDirectionTrend or isSpeedTrend or isHeightTrend)

            self._textProduct.debug_print(
                "\t\tFound a trend? %s" % isLlwsTrend, 1)

            return isLlwsTrend

        def _isFlightCategoryTrend(self, prevFmGroupDict, currFmGroupDict, nextFmGroupDict):
            self._textProduct.debug_print(
                "\tChecking for Flight Category trend", 1)

            # Get list of ordered categories
            orderedCategories = [categoryInfo[0] for categoryInfo in self._textProduct._orderedCategoryInfo]

            prevCategory = prevFmGroupDict["flightCategory"]
            prevIndex = -1
            if prevCategory in orderedCategories:
                prevIndex = orderedCategories.index(prevCategory)

            currCategory = currFmGroupDict["flightCategory"]
            currIndex = -1
            if currCategory in orderedCategories:
                currIndex = orderedCategories.index(currCategory)

            nextCategory = nextFmGroupDict["flightCategory"]
            nextIndex = -1
            if nextCategory in orderedCategories:
                nextIndex = orderedCategories.index(nextCategory)

            self._textProduct.debug_print(
                "\t\tprev = %s\tcurr = %s\tnext = %s"
                % (prevCategory, currCategory, nextCategory), 1)

            if ((prevIndex < currIndex < nextIndex) or
                (prevIndex > currIndex > nextIndex)):

                self._textProduct.debug_print("\t\tFound a trend!", 1)
                return True

            return False

        def _isNumericalTrend(self, prevValue, currValue, nextValue, fieldName):
            """
            Checks for a simple increasing or decreasing trend for numerical
            data.
            """
            self._textProduct.debug_print(
                "\tChecking for numerical trend for %s:" % fieldName, 1)
            self._textProduct.debug_print("\t\tprev = %s\tcurr = %s\tnext = %s"
                % (prevValue, currValue, nextValue), 1)

            if ((prevValue is not None) and
                (currValue is not None) and
                (nextValue is not None) and
                ((prevValue < currValue < nextValue) or
                 (prevValue > currValue > nextValue))):

                self._textProduct.debug_print("\t\tFound a trend!", 1)
                return True

            return False

        def _isWindSimilar(self, fmGroups):
            """Determine if wind is similar enough."""

            prevFmGroup = fmGroups[self._periodIndex - 1]
            prevWind = prevFmGroup.productDict["wind"]

            currFmGroup = fmGroups[self._periodIndex]
            currWind = currFmGroup.productDict["wind"]

            self._textProduct.debug_print("\n" + "_" * 40 + 
                "\nDetermining wind similarity between:" + 
                "\n\t%s: wind = %s" % (prevFmGroup, repr(prevWind)) + 
                "\n\t%s: wind = %s" % (currFmGroup, repr(currWind)), 1)

            # Check the wind direction and speed
            if prevWind is None and currWind is None:
                self._textProduct.debug_print(
                    "\nSimilar: Neither wind exists", 1)
                isWindDirSpeedSimilar = True

            elif [prevWind, currWind].count(None) == 1:
                self._textProduct.debug_print(
                    "\nNot similar: One wind exists, one doesn't", 1)
                isWindDirSpeedSimilar = False

            else:
                isDirectionSimilar = self._checkWindDirectionSimilarity(
                    prevWind, currWind, "WIND_DIR")

                isSpeedSimilar = self._checkWindSpeedSimilarity(
                    prevWind, currWind, "WIND_MAG")

                isWindDirSpeedSimilar = isDirectionSimilar and isSpeedSimilar

            self._textProduct.debug_print("\nWind similar? %s"
                                          % isWindDirSpeedSimilar, 1)

            return isWindDirSpeedSimilar

        def _isWindGustSimilar(self, fmGroups):
            """Determine if wind gust is similar enough."""

            prevFmGroup = fmGroups[self._periodIndex - 1]
            prevWindGust = prevFmGroup.productDict["windGust"]

            currFmGroup = fmGroups[self._periodIndex]
            currWindGust = currFmGroup.productDict["windGust"]

            self._textProduct.debug_print("\n" + "_" * 40 + 
                "\nDetermining wind gust similarity between:" + 
                "\n\t%s: gust = %s" % (prevFmGroup, prevWindGust) + 
                "\n\t%s: gust = %s" % (currFmGroup, currWindGust), 1)

            # Check the wind gust
            if prevWindGust is None and currWindGust is None:
                self._textProduct.debug_print("\nSimilar: Neither gust exists"
                                              , 1)
                isGustSimilar = True

            elif [prevWindGust, currWindGust].count(None) == 1:
                self._textProduct.debug_print(
                    "\nNot similar: One gust exists, one doesn't", 1)
                isGustSimilar = False

            else:
                isGustSimilar = self._checkWindGustSimilarity(prevWindGust,
                                                              currWindGust)

            self._textProduct.debug_print(
                "\nWind gust similar? %s" % isGustSimilar, 1)

            return isGustSimilar

        def _isWeatherSimilar(self, fmGroups):
            """Determine if weather is similar enough."""

            prevFmGroup = fmGroups[self._periodIndex - 1]
            prevWeather = prevFmGroup.productDict["weather"]

            currFmGroup = fmGroups[self._periodIndex]
            currWeather = currFmGroup.productDict["weather"]

            self._textProduct.debug_print("\n" + "_" * 40 + 
                "\nDetermining weather similarity between:" + 
                "\n\t%s: \n\t\tweather = %s" % 
                (prevFmGroup, repr(prevWeather)) + 
                "\n\t%s: \n\t\tweather = %s" % 
                (currFmGroup, repr(currWeather)), 1)

            if prevWeather is None and currWeather is None:
                self._textProduct.debug_print(
                    "\nSimilar: Neither weather exists", 1)
                return True

            if [prevWeather, currWeather].count(None) == 1:
                self._textProduct.debug_print(
                    "\nNot similar: One weather exists, one doesn't", 1)
                return False

            if prevWeather == currWeather:
                self._textProduct.debug_print(
                    "\nSimilar: Identical weather", 1)
                return True

            # Do not allow VFR fog or haze weather alone to force a FM group
            if (prevWeather.getAllWeather() in [["BR"], ["HZ"]] and
                currWeather.getAllWeather() in [["BR"], ["HZ"]]):

                prevVisibility = prevFmGroup.productDict["visibility"]
                currVisibility = currFmGroup.productDict["visibility"]
                self._textProduct.debug_print(
                    "\tPrevious visibility = %s" % prevVisibility, 1)
                self._textProduct.debug_print(
                    "\tCurrent visibility = %s" % currVisibility, 1)
                self._textProduct.debug_print(
                    "\tMinimum VFR visibility (in statute miles) = %s"
                        % self._textProduct._minVfrVisibility, 1)

                if prevVisibility >= self._textProduct._minVfrVisibility and \
                   currVisibility >= self._textProduct._minVfrVisibility:
                    
                    self._textProduct.debug_print("\nSimilar: " + 
                        "Only fog/haze with VFR visibilities", 1)
                    return True

            prevPrecipitation = set(prevWeather.regularWeatherTypes)
            currPrecipitation = set(currWeather.regularWeatherTypes)
            commonPrecipitation = \
                currPrecipitation.intersection(prevPrecipitation)
            self._textProduct.debug_print(
                "\tCommon precipitation = %s" % repr(commonPrecipitation), 1)

            prevObstructions = set(prevWeather.obstructionWeatherTypes)
            currObstructions = set(currWeather.obstructionWeatherTypes)
            commonObstructions = \
                currObstructions.intersection(prevObstructions)
            self._textProduct.debug_print(
                "\tCommon obstructions = %s" % repr(commonObstructions), 1)

            # If the weather is partially the same:
            #  previous precip only consists of common precip or
            #  current precip only consists of common precip or
            #  previous obstructions only consist of common obstructions or
            #  current obstructions only consist of common obstructions
            if (len(prevPrecipitation.difference(commonPrecipitation)) == 0
                or
                len(currPrecipitation.difference(commonPrecipitation)) == 0
                or
                len(prevObstructions.difference(commonObstructions)) == 0
                or
                len(currObstructions.difference(commonObstructions)) == 0):

                prevAllWeatherSet = set(prevWeather.getAllWeather())
                currAllWeatherSet = set(currWeather.getAllWeather())
                # Get all differences between previous and current weather
                differentWeather = \
                    currAllWeatherSet.symmetric_difference(prevAllWeatherSet)
                self._textProduct.debug_print(
                    "\tDifferent weather = %s" % repr(differentWeather), 1)

                # If the only difference is fog or haze
                if (differentWeather == set(["BR"]) or
                    differentWeather == set(["FG"]) or
                    differentWeather == set(["FZFG"]) or
                    differentWeather == set(["HZ"])):

                    self._textProduct.debug_print(
                        "\nSimilar: Partially the same except fog/haze", 1)
                    return True

            # Stratiform v. convective with no obstructions to visibility
            # TODO: THIS LOGIC SEEMS WRONG
            if ((prevObstructions == set() and currObstructions == set())
                and
                (prevPrecipitation in [set(["RA"]), set(["SHRA"])] and
                 currPrecipitation in [set(["RA"]), set(["SHRA"])])
                or
                (prevPrecipitation in [set(["SN"]), set(["SHSN"])] and
                 currPrecipitation in [set(["SN"]), set(["SHSN"])])):

                self._textProduct.debug_print("\nSimilar: " + 
                    "Stratiform v. convective and " + 
                    "no obstructions to visibility", 1)
                return True

            self._textProduct.debug_print("\nNot similar: no checks succeeded"
                                          , 1)
            return False

        def _isSkySimilar(self, fmGroups):
            """Determine if sky is similar enough."""

            prevFmGroup = fmGroups[self._periodIndex - 1]
            prevClouds = prevFmGroup.productDict["clouds"]

            currFmGroup = fmGroups[self._periodIndex]
            currClouds = currFmGroup.productDict["clouds"]

            self._textProduct.debug_print("\n" + "_" * 40 + 
                "\nDetermining sky similarity between:" + 
                "\n\t%s: clouds = %s" % (prevFmGroup, repr(prevClouds)) + 
                "\n\t%s: clouds = %s" % (currFmGroup, repr(currClouds)), 1)

            if prevClouds == "SKC" and currClouds == "SKC":
                self._textProduct.debug_print("Similar: both SKC", 1)
                return True

            if [prevClouds, currClouds].count("SKC") == 1:
                self._textProduct.debug_print("Not similar: one SKC, one not"
                                              , 1)
                return False

            # Both clouds must be lists
            if len(prevClouds) != len(currClouds):
                self._textProduct.debug_print(
                    "Not similar: different number of clouds", 1)
                return False

            for index in range(len(currClouds)):

                prevCloud = prevClouds[index]
                (prevCoverage, prevHeight) = prevCloud

                currCloud = currClouds[index]
                (currCoverage, currHeight) = currCloud

                self._textProduct.debug_print("Comparing clouds %s and %s"
                    % (repr(prevCloud), repr(currCloud)), 1)

                ceilings = ["BKN", "OVC"]
                if prevCoverage in ceilings and currCoverage in ceilings:
                    self._textProduct.debug_print("\tSimilar: both ceilings"
                                                  , 1)
                    continue

                nonCeilings = ["FEW", "SCT"]
                if prevCoverage in nonCeilings and currCoverage in nonCeilings:
                    # See how close the heights are
                    changeAmount = abs(currHeight - prevHeight)

                    self._textProduct.debug_print(
                        "\tChange in height = %s" % changeAmount, 1)
                    self._textProduct.debug_print(
                        "\tMaximum similar non-ceiling height change (100s ft)"
                        + " = %d" % 
                        self._textProduct._maxSimilarNonCeilingHeightChange, 1)
                    self._textProduct.debug_print(
                        "\tMaximum significant non-ceiling height (100s ft)"
                        + " = %d" % 
                        self._textProduct._maxSignificantNonCeilingHeight, 1)

                    # If the height has changed too much and one of the heights
                    # is significant
                    if ((changeAmount > 
                         self._textProduct._maxSimilarNonCeilingHeightChange)
                        and
                        (currHeight <= 
                         self._textProduct._maxSignificantNonCeilingHeight or
                         prevHeight <= 
                         self._textProduct._maxSignificantNonCeilingHeight)):

                        self._textProduct.debug_print(
                            "\tNot similar: both non-ceilings with at least " + 
                            "one significant height and the height changed " + 
                            "too much", 1)
                        return False
                    else:
                        self._textProduct.debug_print(
                            "\tSimilar: both non-ceilings have " + 
                            "non-significant heights and/or the height " + 
                            "didn't change enough", 1)
                        continue

                self._textProduct.debug_print(
                    "\tMaximum significant ceiling height (100s ft) = %s"
                    % self._textProduct._maxSignificantCeilingHeight, 1)

                if (((prevCoverage in nonCeilings and currCoverage in ceilings)
                     or
                     (prevCoverage in ceilings and currCoverage in nonCeilings))
                    and
                    (prevHeight > 
                     self._textProduct._maxSignificantCeilingHeight)
                    and
                    (currHeight > 
                     self._textProduct._maxSignificantCeilingHeight)):

                    # TODO: Should the non-ceiling use non-ceiling threshold??
                    self._textProduct.debug_print(
                        "\tSimilar: one ceiling, one not, both too high to " + 
                        "be operationally significant", 1)
                    continue

                self._textProduct.debug_print(
                    "\tNot similar: no checks succeeded", 1)
                return False

            # Made it through all the checks
            self._textProduct.debug_print("Similar: all clouds similar", 1)
            return True

        def _isLlwsSimilar(self, fmGroups):
            """Determine if llws is similar enough."""

            prevFmGroup = fmGroups[self._periodIndex - 1]
            prevLLWS = prevFmGroup.productDict["llws"]

            currFmGroup = fmGroups[self._periodIndex]
            currLLWS = currFmGroup.productDict["llws"]

            self._textProduct.debug_print("\n" + "_" * 40 + 
                "\nDetermining llws similarity between:" + 
                "\n\t%s: llws = %s" % (prevFmGroup, repr(prevLLWS)) + 
                "\n\t%s: llws = %s" % (currFmGroup, repr(currLLWS)), 1)

            if prevLLWS is None and currLLWS is None:
                self._textProduct.debug_print("\nSimilar: Neither llws exists"
                                              , 1)
                return True

            if [prevLLWS, currLLWS].count(None) == 1:
                self._textProduct.debug_print(
                    "\nNot similar: One llws exists, one doesn't", 1)
                return False

            (prevDirection, prevSpeed, prevHeight) = prevLLWS
            (currDirection, currSpeed, currHeight) = currLLWS

            if prevHeight != currHeight:
                self._textProduct.debug_print(
                    "\nNot similar: llws heights are different", 1)
                return False

            isDirectionSimilar = self._checkWindDirectionSimilarity(
                (prevDirection, prevSpeed), (currDirection, currSpeed),
                "LLWS_DIR")

            isSpeedSimilar = self._checkWindSpeedSimilarity(
                (prevDirection, prevSpeed), (currDirection, currSpeed),
                "LLWS_MAG")

            isLlwsSimilar = isDirectionSimilar and isSpeedSimilar
            self._textProduct.debug_print("\nLLWS similar? %s" % isLlwsSimilar
                                          , 1)

            return isLlwsSimilar

        def _isFlightCategorySimilar(self, fmGroups):
            """Determine if flight category is similar enough."""

            prevFmGroup = fmGroups[self._periodIndex - 1]
            prevFlightCategory = prevFmGroup.productDict["flightCategory"]

            currFmGroup = fmGroups[self._periodIndex]
            currFlightCategory = currFmGroup.productDict["flightCategory"]

            flightCategoryInfo = \
                self._textProduct._CACThresholds[self._airportIcaoId]
            self._textProduct.debug_print("\n" + "_" * 40 + 
                "\nDetermining flight category similarity between:" + 
                "\n\t%s: flight category = %s  %s" % 
                (prevFmGroup, prevFlightCategory,
                 repr(flightCategoryInfo.get(prevFlightCategory, "VFR"))) + 
                "\n\t%s: flight category = %s  %s" % 
                (currFmGroup, currFlightCategory,
                 repr(flightCategoryInfo.get(currFlightCategory, "VFR"))), 1)

            if prevFlightCategory == currFlightCategory:
                self._textProduct.debug_print(
                    "Similar: flight categories the same", 1)
                return True
            else:
                self._textProduct.debug_print(
                    "Not similar: flight categories differ", 1)
                return False

        def _checkWindDirectionSimilarity(self, wind1, wind2, dirName):
            """Determine if wind directions are similar enough."""

            self._textProduct.debug_print(
                "\nDetermining wind direction similarity between %s and %s"
                % (repr(wind1), repr(wind2)), 1)

            (direction1, speed1) = wind1
            (direction2, speed2) = wind2
            
            # New change for bug 39127
            # Tom: Added this bit of code for bug 39127
            # If both speeds are light assume a similar direction 
            if speed1 <= self._textProduct._maxLightWindSpeed and \
                speed2 <= self._textProduct._maxLightWindSpeed:
                return True
                

            if direction1 == direction2:
                self._textProduct.debug_print("Similar: directions the same"
                                              , 1)
                return True

            if direction1 == "VRB" and isinstance(direction2, int) or \
               isinstance(direction1, int) and direction2 == "VRB":

                self._textProduct.debug_print(
                    "Not similar: one variable direction, one not", 1)
                return False

            if (direction1 == 0 and direction2 != 0) or \
               (direction1 != 0 and direction2 == 0):

                self._textProduct.debug_print(
                    "Not similar: one calm direction, one not", 1)
                return False

            #==================================================================
            # Determine if these wind directions are considered similar enough

            # See how close the wind directions are
            changeAmount = abs(direction1 - direction2)

            # If directions are unreasonably far apart
            if changeAmount > 180:
                # Ensure we take the shortest arc
                changeAmount = abs(360 - changeAmount)

            self._textProduct.debug_print(
                "Change in direction = %s" % changeAmount, 1)
            self._textProduct.debug_print(
                "Maximum light wind speed = %d"
                % self._textProduct._maxLightWindSpeed, 1)
            self._textProduct.debug_print(
                "Maximum similar light wind/llws direction change = %d"
                % self._textProduct._maxSimilarLightWindDirChange, 1)
            self._textProduct.debug_print(
                "Maximum similar non-light wind/llws direction change " + 
                "(Always used for LLWS regardless of wind speed) = %d"
                % self._textProduct._maxSimilarWindDirChange, 1)

            if dirName == "WIND_DIR":
                #--------------------------------------------------------------
                # If directions are similar and both wind speeds are not
                # "light"
                if ((changeAmount <= 
                     self._textProduct._maxSimilarWindDirChange) and
                    speed1 > self._textProduct._maxLightWindSpeed and
                    speed2 > self._textProduct._maxLightWindSpeed):

                    self._textProduct.debug_print(
                        "Similar: No light winds, directions are similar", 1)

                    return True

                #--------------------------------------------------------------
                # If directions are similar and at least one wind speed is
                # "light"
                elif ((changeAmount <= 
                       self._textProduct._maxSimilarLightWindDirChange)
                      and
                      (speed1 <= self._textProduct._maxLightWindSpeed or
                       speed2 <= self._textProduct._maxLightWindSpeed)):

                    self._textProduct.debug_print("Similar: " + 
                        "At least one light wind, directions are similar", 1)

                    return True

            elif dirName == "LLWS_DIR":
                #--------------------------------------------------------------
                # If directions are similar
                if changeAmount <= self._textProduct._maxSimilarWindDirChange:

                    self._textProduct.debug_print(
                        "Similar: Directions are similar", 1)

                    return True

            self._textProduct.debug_print("Not similar: no checks succeeded"
                                          , 1)
            return False

        def _checkWindSpeedSimilarity(self, wind1, wind2, speedName):
            """Determine if wind speeds are similar enough."""

            self._textProduct.debug_print(
                "\nDetermining wind speed similarity between %s and %s"
                % (repr(wind1), repr(wind2)), 1)

            (direction1, speed1) = wind1
            (direction2, speed2) = wind2

            self._textProduct.debug_print("Maximum light wind speed = %d"
                % self._textProduct._maxLightWindSpeed, 1)

            #------------------------------------------------------------------
            # If both wind speeds are considered "light"
            if speed1 < self._textProduct._maxLightWindSpeed and \
               speed2 < self._textProduct._maxLightWindSpeed:

                self._textProduct.debug_print("Similar: Both wind speeds light"
                                              , 1)
                return True

            #------------------------------------------------------------------
            # Determine the difference in wind speeds as well as which
            # threshold we should use during comparison

            # See how close the wind speeds are
            changeAmount = abs(speed1 - speed2)

            # Get the threshold to use for wind speed comparison
            # Note: speedName is either "WIND_MAG" or "LLWS_MAG"
            speedThreshold = self._determineThreshold(min(speed1, speed2),
                                                      speedName)

            self._textProduct.debug_print(
                "Change in speed = %s" % changeAmount, 1)
            self._textProduct.debug_print(
                "Speed change threshold = %s" % speedThreshold, 1)

            if speedThreshold is not None and changeAmount < speedThreshold:

                self._textProduct.debug_print("Similar: Speeds close enough"
                                              , 1)
                return True

            self._textProduct.debug_print("Not similar: no checks succeeded"
                                          , 1)
            return False

        def _checkWindGustSimilarity(self, windGust1, windGust2):
            """Determine if wind gusts are similar enough."""

            self._textProduct.debug_print(
                "\nDetermining wind gust similarity between %s and %s"
                % (repr(windGust1), repr(windGust2)), 1)

            # See how close the wind gusts are
            changeAmount = abs(windGust1 - windGust2)

            self._textProduct.debug_print(
                "Change in gust = %s" % changeAmount, 1)
            self._textProduct.debug_print(
                "Maximum similar wind gust change = %s"
                % self._textProduct._maxSimilarWindGustChange, 1)

            if changeAmount <= self._textProduct._maxSimilarWindGustChange:
                self._textProduct.debug_print("Similar: gusts are close enough"
                                              , 1)
                return True

            self._textProduct.debug_print("Not similar: no checks succeeded"
                                          , 1)
            return False

        def _allWindDirectionsSimilar(self, prevWind, currWind, nextWind, dirName):
            return (self._checkWindDirectionSimilarity(prevWind, currWind,
                                                       dirName)
                    and
                    self._checkWindDirectionSimilarity(prevWind, nextWind,
                                                       dirName)
                    and
                    self._checkWindDirectionSimilarity(currWind, nextWind,
                                                       dirName))

        def _allWindSpeedsSimilar(self, prevWind, currWind, nextWind, speedName):
            return (self._checkWindSpeedSimilarity(prevWind, currWind,
                                                   speedName)
                    and
                    self._checkWindSpeedSimilarity(prevWind, nextWind,
                                                   speedName)
                    and
                    self._checkWindSpeedSimilarity(currWind, nextWind,
                                                   speedName))

        def _determineThreshold(self, value, field):
            """
            Determine the non-linear threshold to use for the specified field.
            """
            # See if we have a defined non-linear threshold for this field
            if field in self._textProduct._thresholdsNLValues:
                print("_thresholdsNLValues item: " + str(field))
                try:
                    # Return the threshold to use based on the specified value
                    return self._textProduct.nlValue(
                            self._textProduct._thresholdsNLValues[field],
                            value)
                except:
                    # Couldn't find the value in the NL thresholds dictionary
                    return None

            #------------------------------------------------------------------
            # We could not determine a threshold value to use
            return None

        def _combineFmGroupDicts(self, fmGroup1, fmGroup2):
            changedFields = self._getNonSimilarFields(
                fmGroup2.productDict["similarChecks"])

            for field in changedFields:
                for productPart in self._partsNeededForChangedField(field):
                    mostSignificantFmGroupDict = \
                        self._getMostSignificantFmGroupDict(productPart,
                            fmGroup1.productDict, fmGroup2.productDict)

                    if mostSignificantFmGroupDict[productPart] is not None:
                        fmGroup1.productDict[productPart] = \
                            mostSignificantFmGroupDict[productPart]
                        fmGroup1.productDict["ratings"][productPart] = \
                            mostSignificantFmGroupDict["ratings"][productPart]

            return fmGroup1

        def _partsNeededForChangedField(self, changedField):
            """
            A dictionary indicating what product parts are required for each
            field that changes.
            """
            partsNeeded = {"Wind":           ["wind", "windGust"],

                           "WindGust":       ["wind", "windGust"],

                           "Weather":        ["weather", "visibility"],

                           "Sky":            ["sky", "skyPrimary", "skySecondary", "skyTertiary"
                                              "cloudBasePrimary", "cloudBaseSecondary",
                                              "cloudBaseTertiary" "ceiling",
                                              "clouds", "weather"],

                           "LLWS":           ["llws"],

                           "FlightCategory": ["flightCategory", "ceiling",
                                              "visibility", "wind"], }

            return partsNeeded.get(changedField, [])

        def _getMostSignificantFmGroupDict(self, productPart, fmGroupDict1, fmGroupDict2):
            if (fmGroupDict1["ratings"].get(productPart, 0) > 
                fmGroupDict2["ratings"].get(productPart, 0)):
                return fmGroupDict1
            else:
                return fmGroupDict2

    #--------------------------------------------------------------------------

    #--------------------------------------------------------------------------

    ########################################################################
    # OVERRIDING THRESHOLDS, VARIABLES, and METHODS
    ########################################################################
    # Examine the Forecast, TextRules and SampleAnalysis Text Utilities
    # for the definitions and documentation of the thresholds and variables
    # which can be overridden.

    def getAverageDirection(self, parmHisto, timeRange, componentName):
        """TAF version of SampleAnalysis.getAverageDirection.

        Returns the dominant direction calculated by always assuming a
        magnitude of 1.

        This method has been modified to ignore calm winds when computing the
        average direction for the LLWS element.  Including these "missing" wind
        values results in a returned direction that is unrepresentative of what
        was actually in the grids (northerly bias). Modified logic supplied by
        John Rozbicki (WFO BUF).
        """

        uSum = 0.0
        vSum = 0.0
        totCount = 0
        weight = 0
        totWeight = 0

        # Get the name of the field for this histogram
        elementName = parmHisto.parmID().getParmName()

        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName,
                                          histSample) == 0:
                continue

            histPairs = histSample.histogram()
            for histPair in histPairs:

                # JJR 10/04/13 - Begin modified code.
                # JJR - added this if block so that if we are working with the
                # LLWS element, we ignore any calm winds when calculating the
                # average wind direction. This results in a returned wind
                # direction that is much more representative of what is
                # actually in the LLWS grids.
                includeHistPair = True
                if elementName == "LLWS":

                    # Get the components for this histPair...
                    magnitude = int(histPair.value().magnitude())
                    direction = int(histPair.value().direction())
                    count = histPair.count()
                    self.debug_print("LLWS histogram:", 1)
                    self.debug_print("\ttime range: %s" % (timeRange), 1)
                    self.debug_print(
                        "\tcount: %s, value: (magnitude %s, direction %s)"
                        % (count, magnitude, direction), 1)

                    # Only incorporate this histPair in our calculations if its
                    # magnitude and direction are both greater than 0...
#                     if magnitude <= 0 or direction <= 0:
#                         includeHistPair = False
                    if magnitude <= 0 or direction < 0:
                        includeHistPair = False

                if includeHistPair:
                    validTime = histSample.validTime()
                    validTime = TimeRange.TimeRange(validTime)
                    weight = validTime.intersection(timeRange).duration()
                    totWeight = totWeight + weight
                    count = histPair.count() 
                    totCount = totCount + count
                    # Sum u and v components, always assigning a magnitude of 1
                    uw, vw = self.MagDirToUV(1.0, histPair.value().direction())
                    uSum = uSum + (uw * count) * weight
                    vSum = vSum + (vw * count) * weight
                # JJR 10/04/13 - End modified code.

        # Calculate the average normalized wind vector
        if totCount > 0:
            u = uSum / (float(totCount) * totWeight)
            v = vSum / (float(totCount) * totWeight)
            mag, dir = self.UVToMagDir(u, v)
            return dir
        else:
            return None


# An exception used when data is missing or invalid
class TAF_DataException(Exception):
    def __init__(self, message, required=False):
        if required:
            Exception.__init__(self, message)
        self.requiredData = required


class Output_Formatter():
    def __init__(self, textProduct):
        self._textProduct = textProduct


class TAC_Formatter(Output_Formatter):
    def __init__(self, textProduct):
        Output_Formatter.__init__(self, textProduct)

    def createOutput(self, fcst, productDict, airportDicts):
        """
        Create output in the TAC (ASCII) format for the airport TAF represented
        by the product parts in the productDict and airportDict.
        """
        
        fcst = fcst + self._makeProductHeader(productDict)

        for airportDict in airportDicts.values():
            fcst += self._makeAirportTafHeader(productDict, airportDict)

            airportIcaoId = airportDict["icaoAirportIndicator"]
            self._textProduct.debug_print(
                "\n" + "="*80 + "\nForecast Output for %s:" % airportIcaoId, 1)
            for fmGroup in airportDict["fmGroups"]:
                self._textProduct.debug_print("\n" + "_"*80 + 
                                              "\n%s:" % fmGroup, 1)

                fcst += self._makeAirportTafFmGroup(productDict,
                                                    airportDict,
                                                    fmGroup.productDict)
            self._textProduct.debug_print(" ", 1)

            # Clean up the initial FM group to not have a FM group label
            fcst = re.sub("\*\n +FM\d{6} ", "", fcst)

            fcst += self._makeAirportTafFooter(productDict, airportDict)

        # Write out the generated output
        # Figure out the siteID from the icaoOfficeIndicator
        siteID = productDict["icaoOfficeIndicator"][1:]
        self._saveOutput(fcst, siteID)

        return fcst

    def _saveOutput(self, fcst, siteID):
        
        # Write out the TAFs - minus the WMO header
        try:
            tacOutputFile = self._textProduct._outputFile
            tacOutputDirectory = os.path.dirname(tacOutputFile)

            if not os.path.exists(tacOutputDirectory):
                os.makedirs(tacOutputDirectory)

            with open(tacOutputFile, "w") as tacFile:
                # Find the first 'TAF' line in the text and write it out and
                # all subsequent lines
                index = fcst.index("TAF")
                tacFile.writelines(fcst[index:])

            # Now store the TAFs in the AWIPS2 text database
            os.system("/awips2/fxa/bin/textdb -w WRKTAF < " + tacOutputFile)

            #  Determine a relative path for the EDEX utility tree
            relativeFilePath = "aviation/tmp/gridTAF.txt"
            
            #  Write this file into the Localization store - needs to be generalized for all sites
            LocalizationSupport.writeFile(LocalizationSupport.COMMON_STATIC, LocalizationSupport.SITE,
                                          siteID, relativeFilePath, fcst[index:])

        except Exception as e:
            self._textProduct.debug_print(e, 1)

    def _makeProductHeader(self, productDict):
        """
        Create the text for the bulletin header.
        
        For Example (TAC format):
        FTUS42 KMFL 141100 AAA
        """

        if self._textProduct._verificationHeaders:
            # Format header for these verification TAFs so Stats-On-Demand
            # will ingest them
            return "000\n%s%s%s %s %02d%02d00 %s\n" % \
                   (productDict["bulletinType"],
                    productDict["bulletinLocation"],
                    productDict["bulletinDifferentiator"],
                    productDict["icaoOfficeIndicator"],
                    productDict["issuanceTime"].tm_mday,
                    productDict["issuanceTime"].tm_hour,
                    productDict["tafTypeIdentifier"])
        else:
            # Format header for these TAFs so AvnFPS will ingest them
            return "%s%s%s %s %02d%02d%02d %s\n" % \
                   (productDict["bulletinType"],
                    productDict["bulletinLocation"],
                    productDict["bulletinDifferentiator"],
                    productDict["icaoOfficeIndicator"],
                    productDict["issuanceTime"].tm_mday,
                    productDict["issuanceTime"].tm_hour,
                    productDict["issuanceTime"].tm_min,
                    productDict["tafTypeIdentifier"])

    def _makeAirportTafHeader(self, productDict, airportDict):
        """
        Create the airport specific header.
        
        For Example (TAC format):
        TAFMIA                    <- Only for verification TAFs
        TAF AMD
        KMIA 122229Z 1223/1318
        """

        fcst = ""

        if self._textProduct._verificationHeaders:
            # Format header for these verification TAFs so Stats-On-Demand
            # will ingest them
            fcst = "%s\n" % airportDict["airportHeader"]

        preparationTimeUTC = time.gmtime(airportDict["preparationTime"])
        # Format header for these TAFs so AvnFPS will ingest them
        if len(airportDict["fmGroups"]) == 0:
            fcst += "%s\n%s %02d%02d%02dZ NIL" % \
                    (airportDict["tafTypeHeader"],
                     airportDict["icaoAirportIndicator"],
                     preparationTimeUTC.tm_mday,
                     preparationTimeUTC.tm_hour,
                     preparationTimeUTC.tm_min)
        else:
            validPeriod = \
                self._formatValidPeriod(airportDict["validPeriodStart"],
                                        airportDict["validPeriodEnd"])

            fcst += "%s\n%s %02d%02d%02dZ %s *" % \
                    (airportDict["tafTypeHeader"],
                     airportDict["icaoAirportIndicator"],
                     preparationTimeUTC.tm_mday,
                     preparationTimeUTC.tm_hour,
                     preparationTimeUTC.tm_min,
                     validPeriod)

        return fcst

    def _formatValidPeriod(self, startTimeSeconds, endTimeSeconds):
        validPeriodStartUTC = time.gmtime(startTimeSeconds)
        startDay = validPeriodStartUTC.tm_mday
        startHour = validPeriodStartUTC.tm_hour

        validPeriodEndUTC = time.gmtime(endTimeSeconds)
        # Fix the end time if necessary so that 00Z displays as 24Z of the
        # previous day
        # TODO: Need to change 23Z to 24Z like old code? I think that was just
        #       because it wasn't using this approach I use now with seconds
        #       since epoch to ensure day, month, year changes are handled
        #       properly.
        if validPeriodEndUTC.tm_hour == 0:
            endDay = time.gmtime(endTimeSeconds - 24 * 3600).tm_mday
            endHour = 24
        else:
            endDay = validPeriodEndUTC.tm_mday
            endHour = validPeriodEndUTC.tm_hour

        return "%02d%02d/%02d%02d" % (startDay, startHour, endDay, endHour)

    def _makeAirportTafFmGroup(self, productDict, airportDict, fmGroupDict):
        windText = self._formatWind(fmGroupDict["wind"],
                                    fmGroupDict["windGust"])
        self._textProduct.debug_print("windText = %s" % windText, 1)

        visibilityWeatherText = \
            self._formatVisibilityWeather(fmGroupDict["visibility"],
                                          fmGroupDict["weather"])
        self._textProduct.debug_print("visibilityWeatherText = %s"
                                      % visibilityWeatherText, 1)

        cloudsText = self._formatClouds(fmGroupDict["clouds"],
                                        fmGroupDict["weather"])
        self._textProduct.debug_print("cloudsText = %s" % cloudsText, 1)

        llwsText = self._formatLLWS(fmGroupDict["llws"])
        self._textProduct.debug_print("llwsText = %s" % llwsText, 1)

        conditionalGroupText = self._formatConditionalGroup(
            fmGroupDict["conditionalGroup"])

        # New change for bug 37007
        # commented out this code to address bug 37007 - tl
#         if "CB" in conditionalGroupText:
#             # The TEMPO/PROB has a thunderstorm listed so the CB belongs there
#             # and not in the FM group
#             cloudsText = re.sub("CB", "", cloudsText)

        fcst = "\n     %s %s %s %s" % (fmGroupDict["groupLabel"],
                          windText, visibilityWeatherText, cloudsText)

        if llwsText != "":
            fcst += " %s" % llwsText

        if conditionalGroupText != "":
            fcst += conditionalGroupText

        self._textProduct.debug_print("\n%s" % fcst, 1)

        return fcst

    def _formatWind(self, wind, windGust):
        """
        This method will format the wind and wind gust text for a TAF
        forecast hour.
        """

        (windDirection, windSpeed) = wind
        # print "+&FormatWind: dir: " + str(windDirection) + " speed: " + str(windSpeed) 

        # If we should not report a gust
        if windGust is None:
            # If we have a "variable" wind
            if windDirection == "VRB":
                # Format a variable wind forecast
                windText = "VRB%02dKT" % (windSpeed)
                # print "+&VRB reporting: " + windText

            # Otherwise, format a sustained wind forecast
            else:
                windText = "%03d%02dKT" % (windDirection, windSpeed)

        # Otherwise report a wind and wind gust forecast
        else:
            # If we have a "variable" wind
            if windDirection == "VRB":
                windText = "VRB%02dG%dKT" % (windSpeed, windGust)
            else:
                windText = "%03d%02dG%dKT" % (windDirection, windSpeed,
                                              windGust)


        # print "+& finally reporting: " + windText

        #  Return the text we have
        return windText

    def _formatVisibilityWeather(self, numericalVisibility, weather):
        
        # Commented out for now. Weather should be based in vis, not Vis based on Wx
#         if weather is None:
#             if numericalVisibility is not None:
#                 # Always have perfect visibility if there is no weather
#                 numericalVisibility = self._textProduct._minP6smVisibility
# 
#             return self._convertVisibilityToText(numericalVisibility)
#         else:
        visibilityText = self._convertVisibilityToText(numericalVisibility)
        visibilityWeather = visibilityText

        # Add in weather
        weatherText = self._formatWeather(weather)
        inBRRange = numericalVisibility >= 0.625 and numericalVisibility <= 6.0
        if weatherText == "" and inBRRange:
            weatherText = "BR" 

        # If we have visibility and weather, add a space to separate them
        if len(visibilityText) > 0 and len(weatherText) > 0:
            visibilityWeather += " "

        visibilityWeather += weatherText

        return visibilityWeather

    def _wxTypeOnly(self, wx):
        if wx == "":
            return wx
        
        if wx[0] in ["-", "+"]:
            return wx[1:]
        
        return wx

    def _formatWeather(self, weather):
        
        if weather is None:
            return ""
        
        weather.regularWeatherTypes.sort(key=weather.precipSort)
                
        allWxTypes = weather.regularWeatherTypes + weather.obstructionWeatherTypes
        
        allWxTypes.sort(key=weather.weatherSort)
        # Add in the intensity to the appropriate wxType
        for i in range(len(allWxTypes)):
            if allWxTypes[i] == weather.tafIntensityWx:
                if weather.maxIntensity not in ["<NoInten>", "M", "m"]:  # Matt's fix
                    allWxTypes[i] = weather.maxIntensity + allWxTypes[i]
               
        weatherText = ""
        lastWx = ""
        rank = weather.weatherTypeRanking(allWxTypes[0])
        for wx in allWxTypes:
            if weather.weatherTypeRanking(wx) != rank:
                # Directive has an odd exception when combining particular wx types
                # This detects the exception and if so, no space.
                if self._wxTypeOnly(lastWx) in weather.descriptorPrecipTypes and \
                   self._wxTypeOnly(wx) in weather.precipitationWeatherTypes:
                    space = ""
                else:
                    space = " "
                    
                weatherText = weatherText + space  # add a space, maybe
                rank = weather.weatherTypeRanking(wx)
                print(wx + " new Rank: " + str(rank))
            weatherText = weatherText + wx
            lastWx = wx
            print("Building wx str: " + weatherText)
            
        print("Prelim weatherText:", weatherText)
                
#         # Add maximum intensity if there is weather to report
#         if weather.maxIntensity is not None:
#             # Clean up certain weather intensities in the text
#             weather.maxIntensity = \
#                 weather.maxIntensity.replace("<NoInten>", "")
#             weather.maxIntensity = weather.maxIntensity.replace("--", "-")
#             weather.maxIntensity = weather.maxIntensity.replace("m", "")

#         # Loop thru all the weather types and make the weather text
#         for weatherType in weather.regularWeatherTypes:
#             if weatherType == weather.tafIntensityWx and weather.maxIntensity is not None:
#                 weatherText += weather.maxIntensity + weatherType
#             else:
#                 weatherText += weatherType
#             
#             print "Appending precip wx: " + str(weatherText)
#             
#         print "Final precip weather: " + str(weatherText) 
#                      
#         # Add any obstruction weather
#         for weatherType in weather.obstructionWeatherTypes:
#             weatherText += " %s" % weatherType
#         
#         # Add any vicinity weather last
#         if weather.vicinityWeather is not None:
#             # TODO: Move this content logic over to the product parts section
#             # Only allow vicinity thunderstorms if we don't have prevailing
#             # thunderstorms
# 
#             #This code causes both TSRA and VCTS to appear as present weather 
#             if weather.vicinityWeather == "VCTS" and "TS" not in weatherText:
#                 weatherText = " VCTS" + weatherText
# 
#             # Only allow vicinity showers if we don't have prevailing showers
#             # or anything more severe than showers
#             if weather.vicinityWeather == "VCSH":
#                 allowVCSH = True
#                 for weatherPart in ["SH", "RA", "SN", "FZRA", "GS", "GR"]:
#                     if weatherPart in weatherText:
#                         allowVCSH = False
#                         break
# 
#                 if allowVCSH:
#                     weatherText = " VCSH" + weatherText

        # Clean up any added extra spaces
        weatherText = weatherText.strip()

        print("returning wxText: " + weatherText)
        return weatherText

    def _convertVisibilityToText(self, numericalVisibility):
        # Visibility can only be None with TEMPOs and PROBs, not FM groups
        if numericalVisibility is None:
            return ""
        elif 0 <= numericalVisibility < 0.25:
            return "0SM"
        elif 0.25 <= numericalVisibility < 0.50:
            return "1/4SM"
        elif 0.50 <= numericalVisibility < 0.75:
            return "1/2SM"
        elif 0.75 <= numericalVisibility < 1:
            return "3/4SM"
        elif 1 <= numericalVisibility < 1.50:
            return "1SM"
        elif 1.50 <= numericalVisibility < 2:
            return "1 1/2SM"
        elif 2 <= numericalVisibility < 3:
            return "2SM"
        elif 3 <= numericalVisibility < 4:
            return "3SM"
        elif 4 <= numericalVisibility < 5:
            return "4SM"
        elif 5 <= numericalVisibility < 6:
            return "5SM"
        elif 6 <= numericalVisibility < self._textProduct._minP6smVisibility:
            return "6SM"
        else:
            return "P6SM"

    def _formatClouds(self, clouds, weather):
        
        if isinstance(clouds, str):
            return clouds
        elif isinstance(clouds, list):
            cloudText = ""
            for (index, cloud) in enumerate(clouds):
                coverage, height = cloud
                
                if coverage == "SKC" or height is None:
                    continue

                # Add "CB" to the first cloud base if there are thunderstorms
                if index == 0 and \
                   weather is not None and weather.thunderstormsPresent():

                    cloudText += " %s%03dCB" % (coverage, height)
                else:
                    cloudText += " %s%03d" % (coverage, height)
                    
            print("CloudText: |" + str(cloudText) + "|")
            cloudText = cloudText.strip()
            if cloudText == "":
                cloudText = "SKC"
                
            return cloudText
        else:
            return ""

    def _formatLLWS(self, llws):
        """Format the low-level wind shear text for a TAF forecast hour."""

        llwsText = ""

        # If we have both a wind and a height to report
        if llws is not None:
            (direction, speed, height) = llws

            # Format the wind shear group. Prepend a space since it's optional
            # and if it doesn't exist, we don't want extra spaces in the text.
            llwsText = "WS%03d/%03d%02dKT" % (height, direction, speed)

        return llwsText

    def _formatConditionalGroup(self, conditionalGroup):
        if conditionalGroup is None:
            return ""
        else:
            groupDict = conditionalGroup.productDict
            groupType = groupDict["groupType"]
            groupLabel = groupDict["groupLabel"]

            numericalVisibility = groupDict["visibility"]
            weather = groupDict["weather"]
            clouds = groupDict["clouds"]

            visibilityWeatherText = \
                self._formatVisibilityWeather(numericalVisibility, weather)

            cloudText = self._formatClouds(clouds, weather)

            self._textProduct.debug_print("Conditional group information:", 1)
            self._textProduct.debug_print("\tgroupType = %s" % groupType, 1)
            self._textProduct.debug_print("\tgroupLabel = %s" % groupLabel, 1)
            self._textProduct.debug_print("\tvisibilityWeatherText = %s"
                                          % visibilityWeatherText, 1)
            self._textProduct.debug_print("\tcloudText = %s" % cloudText, 1)

            # Don't create a TEMPO/PROB if all it's going to show is P6SM
            if visibilityWeatherText == "P6SM" and cloudText == "":
                return ""
            else:
                if groupType == "TEMPO":
                    groupText = "\n      %s" % groupLabel
                else:
                    groupText = " %s" % groupLabel

                if visibilityWeatherText != "":
                    groupText += " %s" % visibilityWeatherText

                if cloudText != "":
                    groupText += " %s" % cloudText

                return groupText

    def _makeAirportTafFooter(self, productDict, airportDict):
        fcst = ""

        if airportDict["airportDisclaimer"] is not None:
            # Add the disclaimer to this airport TAF (indented 6 spaces)
            fcst += "\n      " + airportDict["airportDisclaimer"]

        # Place a blank line between each airport TAF forecast
        fcst += "=\n\n"

        return fcst


from xml.etree.ElementTree import Element, SubElement, tostring
from uuid import uuid4
from xml.dom.minidom import parseString

class IWXXM_Formatter(Output_Formatter):
    def __init__(self, textProduct):
        Output_Formatter.__init__(self, textProduct)

        self._weatherUriToDescription = {
            "http://codes.wmo.int/306/4678/BLDU": "Blowing dust",
            "http://codes.wmo.int/306/4678/BLSA": "Blowing sand",
            "http://codes.wmo.int/306/4678/BLSN": "Blowing snow",
            "http://codes.wmo.int/306/4678/BR": "Mist",
            "http://codes.wmo.int/306/4678/-DZ": "Light precipitation of drizzle",
            "http://codes.wmo.int/306/4678/DZ": "Precipitation of drizzle",
            "http://codes.wmo.int/306/4678/+DZ": "Heavy precipitation of drizzle",
            "http://codes.wmo.int/306/4678/FG": "Fog",
            "http://codes.wmo.int/306/4678/FU": "Smoke",
            "http://codes.wmo.int/306/4678/-FZDZ": "Light precipitation of freezing drizzle",
            "http://codes.wmo.int/306/4678/FZDZ": "Precipitation of freezing drizzle",
            "http://codes.wmo.int/306/4678/+FZDZ": "Heavy precipitation of freezing drizzle",
            "http://codes.wmo.int/306/4678/FZFG": "Freezing fog",
            "http://codes.wmo.int/306/4678/-FZRA": "Light precipitation of freezng rain",
            "http://codes.wmo.int/306/4678/FZRA": "Precipitation of freezng rain",
            "http://codes.wmo.int/306/4678/+FZRA": "Heavy precipitation of freezng rain",
            "http://codes.wmo.int/306/4678/HZ": "Haze",
            "http://codes.wmo.int/306/4678/-PL": "Light precipitation of ice pellets",
            "http://codes.wmo.int/306/4678/PL": "Precipitation of ice pellets",
            "http://codes.wmo.int/306/4678/+PL": "Heavy precipitation of ice pellets",
            "http://codes.wmo.int/306/4678/-RA": "Light precipitation of rain",
            "http://codes.wmo.int/306/4678/RA": "Precipitation of rain",
            "http://codes.wmo.int/306/4678/+RA": "Heavy precipitation of rain",
            "http://codes.wmo.int/306/4678/-SHRA": "Light showery precipitation of rain",
            "http://codes.wmo.int/306/4678/SHRA": "Showery precipitation of rain",
            "http://codes.wmo.int/306/4678/+SHRA": "Heavy showery precipitation of rain",
            "http://codes.wmo.int/306/4678/-SHSN": "Light showery precipitation of snow",
            "http://codes.wmo.int/306/4678/SHSN": "Showery precipitation of snow",
            "http://codes.wmo.int/306/4678/+SHSN": "Heavy showery precipitation of snow",
            "http://codes.wmo.int/306/4678/-SN": "Light precipitation of snow",
            "http://codes.wmo.int/306/4678/SN": "Precipitation of snow",
            "http://codes.wmo.int/306/4678/+SN": "Heavy precipitation of snow",
            "http://codes.wmo.int/306/4678/TS": "Thunderstorm",
            "http://codes.wmo.int/306/4678/-TSGS": "Thunderstorm with light precipitation of snow pellets/small hail",
            "http://codes.wmo.int/306/4678/TSGS": "Thunderstorm with precipitation of snow pellets/small hail",
            "http://codes.wmo.int/306/4678/+TSGS": "Thunderstorm with heavy precipitation of snow pellets/small hail",
            "http://codes.wmo.int/306/4678/-TSGR": "Thunderstorm with light precipitation of hail",
            "http://codes.wmo.int/306/4678/TSGR": "Thunderstorm with precipitation of hail",
            "http://codes.wmo.int/306/4678/+TSGR": "Thunderstorm with heavy precipitation of hail",
            "http://codes.wmo.int/306/4678/VA": "Volcanic ash",
            "http://codes.wmo.int/306/4678/VCSH": "Shower(s) in the vicinity",
            "http://codes.wmo.int/306/4678/VCTS": "Thunderstorm in the vicinity"}

        self._coverageHref = {
            "FEW": "http://codes.wmo.int/bufr4/codeflag/0-20-008/1",
            "SCT": "http://codes.wmo.int/bufr4/codeflag/0-20-008/2",
            "BKN": "http://codes.wmo.int/bufr4/codeflag/0-20-008/3",
            "OVC": "http://codes.wmo.int/bufr4/codeflag/0-20-008/4"}

        self._coverageTitle = {
            "FEW": "Few",
            "SCT": "Scattered",
            "BKN": "Broken",
            "OVC": "Overcast"}

    def _tafRoot(self):
        attributes = OrderedDict()
        attributes["xmlns"] = "http://icao.int/iwxxm/2.0"
        attributes["xmlns:aixm"] = "http://www.aixm.aero/schema/5.1.1"
        attributes["xmlns:gml"] = "http://www.opengis.net/gml/3.2"
        attributes["xmlns:metce"] = "http://def.wmo.int/metce/2013"
        attributes["xmlns:om"] = "http://www.opengis.net/om/2.0"
        attributes["xmlns:sams"] = "http://www.opengis.net/samplingSpatial/2.0"
        attributes["xmlns:sf"] = "http://www.opengis.net/sampling/2.0"
        attributes["xmlns:xlink"] = "http://www.w3.org/1999/xlink"
        attributes["xmlns:xsi"] = "http://www.w3.org/2001/XMLSchema-instance"
        attributes["gml:id"] = self._randomUuidString()
        attributes["permissibleUsage"] = "OPERATIONAL"
        attributes["status"] = "NORMAL"
        attributes["xsi:schemaLocation"] = \
            "http://icao.int/iwxxm/2.0 " + \
            "http://schemas.wmo.int/iwxxm/2.0/iwxxm.xsd"

        return Element("TAF", attributes)

    def _iwxxmTags(self):
        """Specifies the main XML tags needed for the IWXXM XML format."""

        return [self.tag("issueTime", method=self._iwxxmIssueTime),
                self.tag("validTime", method=self._iwxxmValidTime),
               ]

    def _iwxxmIssueTime(self, tag, productDict, airportDict):
        self._addChild("issueTime")

        timeInstant = self._addChild("gml:TimeInstant", "issueTime")
        self._issueTimeUuidString = self._randomUuidString()
        timeInstant.set("gml:id", self._issueTimeUuidString)

        timePosition = self._addChild("gml:timePosition",
                                      "issueTime/gml:TimeInstant")

        timeUTC = time.gmtime(airportDict["preparationTime"])
        timePosition.text = time.strftime("%Y-%m-%dT%H:%M:%SZ", timeUTC)

    def _iwxxmValidTime(self, tag, productDict, airportDict):
        self._addChild("validTime")

        timePeriod = self._addChild("gml:TimePeriod", "validTime")
        self._validTimeUuidString = self._randomUuidString()
        timePeriod.set("gml:id", self._validTimeUuidString)

        beginPosition = self._addChild("gml:beginPosition",
                                       "validTime/gml:TimePeriod")

        timeUTC = time.gmtime(airportDict["validPeriodStart"])
        beginPosition.text = time.strftime("%Y-%m-%dT%H:00:00Z", timeUTC)

        endPosition = self._addChild("gml:endPosition",
                                     "validTime/gml:TimePeriod")

        timeUTC = time.gmtime(airportDict["validPeriodEnd"])
        endPosition.text = time.strftime("%Y-%m-%dT%H:00:00Z", timeUTC)

    def _forecastTags(self):
        """
        Specifies the XML tags used for each (FM/TEMPO/PROB30) forecast.
        Tags starting with a lowercase "x" will have that "x" replaced with
        either "base" (first forecast) or "change" (all remaining forecasts).
        So for example, the first tag ("xForecast") will become "baseForecast"
        for the first forecast described and it will become "changeForecast"
        for all remaining forecasts described.
        """

        return [self.tag("xForecast"),
                  self.tag("om:OM_Observation", parentTag="xForecast",
                           method=self._OM_Observation),
                    self.tag("om:type",
                             parentTag="xForecast/om:OM_Observation",
                             method=self._type),
                    self.tag("om:phenomenonTime",
                             parentTag="xForecast/om:OM_Observation",
                             method=self._phenomenonTime),
                    self.tag("om:resultTime",
                             parentTag="xForecast/om:OM_Observation",
                             method=self._resultTime),
                    self.tag("om:validTime",
                             parentTag="xForecast/om:OM_Observation",
                             method=self._validTime),
                    self.tag("om:procedure",
                             parentTag="xForecast/om:OM_Observation",
                             method=self._procedure),
                    self.tag("om:observedProperty",
                             parentTag="xForecast/om:OM_Observation",
                             method=self._observedProperty),
                    self.tag("om:featureOfInterest",
                             parentTag="xForecast/om:OM_Observation",
                             method=self._featureOfInterest),
                    self.tag("om:result",
                             parentTag="xForecast/om:OM_Observation",
                             method=self._result),
               ]

    def _OM_Observation(self, tag, productDict, airportDict, groupDict):
        observation = self._addChild("om:OM_Observation", tag.parentTag)
        observation.set("gml:id", self._randomUuidString())

    def _type(self, tag, productDict, airportDict, groupDict):
        type = self._addChild("om:type", tag.parentTag)
        type.set("xlink:href",
                 "http://codes.wmo.int/49-2/observation-type/IWXXM/1.0/" + \
                 "MeteorologicalAerodromeForecast")

    def _phenomenonTime(self, tag, productDict, airportDict, groupDict):
        self._addChild("om:phenomenonTime", tag.parentTag)

        timePeriod = self._addChild("gml:TimePeriod",
                                    tag.parentTag + "/om:phenomenonTime")
        timePeriod.set("gml:id", self._randomUuidString())

        beginPosition = self._addChild("gml:beginPosition",
                                       tag.parentTag + 
                                       "/om:phenomenonTime/gml:TimePeriod")

        startTimeUTC = time.gmtime(groupDict["startTime"])
        beginPosition.text = time.strftime("%Y-%m-%dT%H:%M:%SZ", startTimeUTC)

        endPosition = self._addChild("gml:endPosition",
                                     tag.parentTag + 
                                     "/om:phenomenonTime/gml:TimePeriod")

        endTimeUTC = time.gmtime(groupDict["endTime"])
        endPosition.text = time.strftime("%Y-%m-%dT%H:%M:%SZ", endTimeUTC)

    def _resultTime(self, tag, productDict, airportDict, groupDict):
        resultTime = self._addChild("om:resultTime", tag.parentTag)
        resultTime.set("xlink:href", "#" + self._issueTimeUuidString)

    def _validTime(self, tag, productDict, airportDict, groupDict):
        validTime = self._addChild("om:validTime", tag.parentTag)
        validTime.set("xlink:href", "#" + self._validTimeUuidString)

    def _procedure(self, tag, productDict, airportDict, groupDict):
        if self._forecastIndex > 0:
            procedure = self._addChild("om:procedure", tag.parentTag)
            procedure.set("xlink:href",
                          "#" + self._processUuidString)
            return

        self._addChild("om:procedure", tag.parentTag)

        process = self._addChild("metce:Process",
                                 tag.parentTag + "/om:procedure")
        self._processUuidString = self._randomUuidString()
        process.set("gml:id", self._processUuidString)

        description = self._addChild("gml:description",
                                     tag.parentTag + 
                                     "/om:procedure/metce:Process")
        description.text = "United States National Weather Service " + \
                           "Instruction 10-813 Terminal Aerodrome Forecasts"

    def _observedProperty(self, tag, productDict, airportDict, groupDict):
        observedProperty = self._addChild("om:observedProperty", tag.parentTag)
        observedProperty.set("xlink:href",
                             "http://codes.wmo.int/49-2/" + 
                             "observables-property/" + 
                             "MeteorologicalAerodromeForecast")

    def _featureOfInterest(self, tag, productDict, airportDict, groupDict):
        if self._forecastIndex > 0:
            feature = self._addChild("om:featureOfInterest", tag.parentTag)
            feature.set("xlink:href",
                        "#" + self._samplingFeatureUuidString)
            return

        parentTag = tag.parentTag

        self._addChild("om:featureOfInterest", parentTag)
        parentTag += "/om:featureOfInterest"

        samplingFeature = self._addChild("sams:SF_SpatialSamplingFeature",
                                         parentTag)
        self._samplingFeatureUuidString = self._randomUuidString()
        samplingFeature.set("gml:id", self._samplingFeatureUuidString)
        parentTag += "/sams:SF_SpatialSamplingFeature"

        type = self._addChild("sf:type", parentTag)
        type.set("xlink:href",
                 "http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0" + 
                 "/SF_SamplingPoint")

        self._addChild("sf:sampledFeature", parentTag)
        parentTag += "/sf:sampledFeature"

        airportHeliport = self._addChild("aixm:AirportHeliport", parentTag)
        airportHeliport.set("gml:id", self._randomUuidString())
        parentTag += "/aixm:AirportHeliport"

        self._addChild("aixm:timeSlice", parentTag)
        parentTag += "/aixm:timeSlice"

        airportHeliportTimeSlice = self._addChild(
            "aixm:AirportHeliportTimeSlice", parentTag)
        airportHeliportTimeSlice.set("gml:id", self._randomUuidString())
        parentTag += "/aixm:AirportHeliportTimeSlice"

        validTime = self._addChild("gml:validTime", parentTag)
        validTime.set("xlink:href", "#" + self._issueTimeUuidString)

        interpretation = self._addChild("aixm:interpretation", parentTag)
        interpretation.text = "SNAPSHOT"

        designator = self._addChild("aixm:designator", parentTag)
        designator.text = airportDict["icaoAirportIndicator"]

        # TODO: name

        location = self._addChild("aixm:locationIndicatorICAO", parentTag)
        location.text = airportDict["icaoAirportIndicator"]

        # Done with the sampled feature element
        parentTag = re.sub("/sf:sampledFeature/aixm:AirportHeliport" + 
                           "/aixm:timeSlice/aixm:AirportHeliportTimeSlice",
                           "", parentTag)

        self._addChild("sams:shape", parentTag)
        parentTag += "/sams:shape"

        point = self._addChild("gml:Point", parentTag)
        point.set("axisLabels", "lat lon altitude")
        point.set("gml:id", self._randomUuidString())
        point.set("srsName", "urn:ogc:crs:EPSG::4979")
        point.set("uomLabels", "deg deg m")
        parentTag += "/gml:Point"

        self._addChild("gml:pos", parentTag)
        # TODO: add lat lon altitude data as the text field

    def _result(self, tag, productDict, airportDict, groupDict):
        parentTag = tag.parentTag

        self._addChild("om:result", parentTag)
        parentTag += "/om:result"

        wind = groupDict.get("wind", None)
        windGust = groupDict.get("windGust", None)
        visibility = groupDict.get("visibility", None)
        weather = groupDict.get("weather", None)
        clouds = groupDict.get("clouds", None)
        llws = groupDict.get("llws", None)

        record = self._addChild("MeteorologicalAerodromeForecastRecord",
                                parentTag)
        record.set("changeIndicator", groupDict["groupClassification"])
        record.set("gml:id", self._randomUuidString())
        if ((visibility is not None and weather is not None
             and
             visibility < self._textProduct._minP6smVisibility)
            or
            (isinstance(clouds, list) and len(clouds) > 0)):

            record.set("cloudAndVisibilityOK", "false")
        else:
            record.set("cloudAndVisibilityOK", "true")
        parentTag += "/MeteorologicalAerodromeForecastRecord"

        if visibility is not None:
            self._resultVisibility(tag, parentTag, visibility, weather)

        if wind is not None:
            self._resultWind(tag, parentTag, wind, windGust)

        if weather is not None:
            self._resultWeather(tag, parentTag, weather)

        if clouds is not None:
            self._resultClouds(tag, parentTag, clouds, weather)

        if llws is not None:
            self._resultLLWS(tag, parentTag, llws)

    def _resultVisibility(self, tag, parentTag, visibility, weather):
        prevailingVisibility = self._addChild("prevailingVisibility",
                                              parentTag)
        prevailingVisibility.set("uom", "m")
        if (visibility >= self._textProduct._minP6smVisibility or
            weather is None):

            prevailingVisibility.text = "10000"
            operator = self._addChild("prevailingVisibilityOperator",
                                      parentTag)
            operator.text = "ABOVE"
        else:
            milesToMeters = 1609.344
            prevailingVisibility.text = \
                str(int(self.round(visibility * milesToMeters, "Nearest", 1)))

    def _resultWind(self, tag, parentTag, wind, windGust):
        self._addChild("surfaceWind", parentTag)
        windForecast = self._addChild("AerodromeSurfaceWindForecast",
                                      parentTag + "/surfaceWind")
        (direction, speed) = wind
        if direction == "VRB":
            windForecast.set("variableWindDirection", "true")
        else:
            windForecast.set("variableWindDirection", "false")
            windDirection = self._addChild("meanWindDirection",
                parentTag + "/surfaceWind/AerodromeSurfaceWindForecast")
            windDirection.set("uom", "deg")
            windDirection.text = str(direction)
        windSpeed = self._addChild("meanWindSpeed",
            parentTag + "/surfaceWind/AerodromeSurfaceWindForecast")
        windSpeed.set("uom", "[kn_i]")
        windSpeed.text = str(speed)

        if windGust is not None:
            windGustSpeed = self._addChild("windGustSpeed",
                parentTag + "/surfaceWind/AerodromeSurfaceWindForecast")
            windGustSpeed.set("uom", "[kn_i]")
            windGustSpeed.text = str(windGust)

    def _resultWeather(self, tag, parentTag, weather):
        nonVicinityTypes = weather.regularWeatherTypes + \
                           weather.obstructionWeatherTypes
        for (index, tafWeatherType) in enumerate(nonVicinityTypes):

            if index == 0:
                if weather.maxIntensity is not None:
                    # Clean up certain weather intensities in the text
                    weather.maxIntensity = \
                        weather.maxIntensity.replace("<NoInten>", "")
                    weather.maxIntensity = weather.maxIntensity.replace("--", "-")
                    weather.maxIntensity = weather.maxIntensity.replace("m", "")

                    tafWeatherType = weather.maxIntensity + tafWeatherType
                    print("Assigned wxIntens: " + str(tafWeatherType))

            weatherUri = "http://codes.wmo.int/306/4678/" + tafWeatherType

            description = self._weatherUriToDescription.get(weatherUri, None)
            if description is not None:
                weatherElement = self._addChild("weather", parentTag)
                weatherElement.set("xlink:href", weatherUri)
                weatherElement.set("xlink:title", description)

        if weather.vicinityWeather is not None:
            weatherUri = \
                "http://codes.wmo.int/306/4678/" + weather.vicinityWeather
            description = self._weatherUriToDescription.get(weatherUri, None)

            weatherElement = self._addChild("weather", parentTag)
            weatherElement.set("xlink:href",
                               "http://codes.wmo.int/306/4678/" + 
                               weather.vicinityWeather)
            weatherElement.set("xlink:title", description)

    def _resultClouds(self, tag, parentTag, clouds, weather):
        self._addChild("cloud", parentTag)
        cloudForecast = self._addChild("AerodromeCloudForecast",
                                       parentTag + "/cloud")
        cloudForecast.set("gml:id", self._randomUuidString())

        if clouds == "SKC":
            self._addChild("layer", parentTag + 
                           "/cloud/AerodromeCloudForecast")
            self._addChild("CloudLayer", parentTag + 
                           "/cloud/AerodromeCloudForecast/layer")
            amount = self._addChild("amount", parentTag + 
                        "/cloud/AerodromeCloudForecast/layer/CloudLayer")
            amount.set("xlink:href",
                       "http://codes.wmo.int/bufr4/codeflag/0-20-008/0")
            amount.set("xlink:title", "Clear")
            base = self._addChild("base", parentTag + 
                        "/cloud/AerodromeCloudForecast/layer/CloudLayer")
            base.set("nilReason", "inapplicable")
            base.set("uom", "N/A")
            base.set("xsi:nil", "true")
        else:
            for (index, (coverage, height)) in enumerate(clouds):
                self._addChild("layer", parentTag + 
                               "/cloud/AerodromeCloudForecast")
                self._addChild("CloudLayer", parentTag + 
                               "/cloud/AerodromeCloudForecast/layer")
                amount = self._addChild("amount", parentTag + 
                            "/cloud/AerodromeCloudForecast/layer/CloudLayer")
                amount.set("xlink:href", self._coverageHref[coverage])
                amount.set("xlink:title", self._coverageTitle[coverage])
                base = self._addChild("base", parentTag + 
                            "/cloud/AerodromeCloudForecast/layer/CloudLayer")
                base.set("uom", "[ft_i]")
                base.text = str(height * 100)  # height was in 100s feet units
                if index == 0 and \
                   weather is not None and weather.thunderstormsPresent():

                    type = self._addChild("cloudType", parentTag + 
                                "/cloud/AerodromeCloudForecast/layer/CloudLayer")
                    type.set("xlink:href",
                             "http://codes.wmo.int/bufr4/codeflag/0-20-012/9")
                    type.set("xlink:title", "Cumulonimbus")

    def _resultLLWS(self, tag, parentTag, llws):
        (direction, speed, height) = llws

        self._addChild("extension", parentTag)
        recordExtension = self._addChild(
            "MeteorologicalAerodromeForecastRecordExtension",
            parentTag + "/extension")
        recordExtension.set("xmlns", "http://nws.weather.gov/iwxxm-us/2.0")
        recordExtension.set("xsi:schemaLocation",
            "http://nws.weather.gov/iwxxm-us/2.0 " + 
            "http://nws.weather.gov/schemas/IWXXM-US/2.0/Release/schemas/usTaf.xsd")
        self._addChild("nonConvectiveLLWS", parentTag + "/extension" + 
            "/MeteorologicalAerodromeForecastRecordExtension")
        windDirection = self._addChild("lowLevelWindShearWindDirection",
            parentTag + "/extension" + 
            "/MeteorologicalAerodromeForecastRecordExtension" + 
            "/nonConvectiveLLWS")
        windDirection.set("uom", "deg")
        windDirection.text = str(direction)
        windSpeed = self._addChild("lowLevelWindShearWindSpeed",
            parentTag + "/extension" + 
            "/MeteorologicalAerodromeForecastRecordExtension" + 
            "/nonConvectiveLLWS")
        windSpeed.set("uom", "[kn_i]")
        windSpeed.text = str(speed)
        self._addChild("layerAboveAerodrome",
            parentTag + "/extension" + 
            "/MeteorologicalAerodromeForecastRecordExtension" + 
            "/nonConvectiveLLWS")
        lowerLimit = self._addChild("lowerLimit",
            parentTag + "/extension" + 
            "/MeteorologicalAerodromeForecastRecordExtension" + 
            "/nonConvectiveLLWS/layerAboveAerodrome")
        lowerLimit.set("uom", "[ft_i]")
        lowerLimit.text = "0"
        upperLimit = self._addChild("upperLimit",
            parentTag + "/extension" + 
            "/MeteorologicalAerodromeForecastRecordExtension" + 
            "/nonConvectiveLLWS/layerAboveAerodrome")
        upperLimit.set("uom", "[ft_i]")
        upperLimit.text = str(height * 100)  # height was in 100s feet units

    class tag:
        def __init__(self, tagName, parentTag=None, namespace=None,
                     method=None, productKey=None, value=None):
            self.tagName = tagName
            self.parentTag = parentTag
            self.namespace = namespace
            self.method = method
            self.productKey = productKey
            self.value = value

    def _addChild(self, tagName, parentTagName=None):
        # ElementTree crashes when finding parents with namespaces so the
        # separators are changed to # and changed back when done creating
        # the entire xml document
        tagName = re.sub(":", "#", tagName)

        parent = self._taf
        if parentTagName is not None:
            parentTagName = re.sub(":", "#", parentTagName)
            parents = self._taf.findall(parentTagName)
            # Choose the last created element with this tag name as the parent
            parent = parents[-1]

        return SubElement(parent, tagName)

    def _createTagXML(self, tag, productDict, airportDict, groupDict=None):
        """Create the specified tag and add it to the XML document."""

        if self._forecastIndex == 0:
            tag.tagName = re.sub("xForecast", "baseForecast", tag.tagName)

            if tag.parentTag is not None:
                tag.parentTag = re.sub("xForecast", "baseForecast",
                                       tag.parentTag)
        elif self._forecastIndex > 0:
            tag.tagName = re.sub("xForecast", "changeForecast", tag.tagName)

            if tag.parentTag is not None:
                tag.parentTag = re.sub("xForecast", "changeForecast",
                                       tag.parentTag)

        if tag.method is not None:
            if groupDict is not None:
                tag.method(tag, productDict, airportDict, groupDict)
            else:
                tag.method(tag, productDict, airportDict)

        else:
            subelement = self._addChild(tag.tagName, tag.parentTag)

            value = None
            if tag.productKey is not None:
                value = self._findDictValue(
                    productDict, airportDict, groupDict, tag.productKey)

            elif tag.value is not None:
                value = tag.value

            if value is not None:
                subelement.text = value

    def _findDictValue(self, productDict, airportDict, groupDict, key):
        if key in productDict:
            return productDict[key]
        elif key in airportDict:
            return airportDict[key]
        elif key in groupDict:
            return groupDict[key]
        else:
            return None

    # Creates IWXXM output in XML format
    def createOutput(self, productDict, airportDicts):
        """
        Create an IWXXM-US 2.0 XML document for the airport TAF represented by
        the product parts in the productDict and airportDict.
        """

        airportXMLs = OrderedDict()

        for airportDict in airportDicts.values():
            # Create the root node
            self._taf = self._tafRoot()

            self._forecastIndex = None

            # Create the main IWXXM tags
            for tag in self._iwxxmTags():
                self._createTagXML(tag, productDict, airportDict)

            # Get the FM groups for this airport TAF
            fmGroups = airportDict["fmGroups"]

            if len(fmGroups) == 0:
                # Create a NIL forecast to indicate there are no FM groups
                # TODO: reorder things because not all _iwxxmTags used for this
                self._createNilBaseForecast(productDict, airportDict)

            # Create the forecasts
            self._forecastIndex = 0
            for fmGroup in fmGroups:
                fmGroupDict = fmGroup.productDict

                for tag in self._forecastTags():
                    self._createTagXML(tag,
                                       productDict, airportDict, fmGroupDict)

                self._forecastIndex += 1

                if fmGroupDict["conditionalGroup"] is not None:
                    groupDict = fmGroupDict["conditionalGroup"].productDict

                    for tag in self._forecastTags():
                        self._createTagXML(tag,
                                           productDict, airportDict, groupDict)

                    self._forecastIndex += 1

            # Turn the XML into a pretty formatted string
            tafXmlText = tostring(self._taf)
            tafXmlText = re.sub(r"([a-zA-Z])#", r"\1:", tafXmlText)
            # print str(parseString(tafXmlText).toprettyxml())

            tafXmlText = str(parseString(tafXmlText).toprettyxml())

            airportXMLs[airportDict["icaoAirportIndicator"]] = \
                tafXmlText

        # Figure out the siteID from the icaoOfficeIndicator
        siteID = productDict["icaoOfficeIndicator"][1:]

        # Write out the generated output
        self._saveOutput(airportXMLs, siteID)
        
        return airportXMLs[list(airportXMLs.keys())[0]]

    def _saveOutput(self, fcsts, siteID):
        # Write out the TAFs - fcsts is a dictionary mapping airport ICAO ids
        # to the IWXXM XML for that airport


        tacOutputFile = self._textProduct._outputFile
        tacOutputDirectory = os.path.dirname(tacOutputFile)

        for (airportIcaoId, xmlOutput) in fcsts.items():
            try:
                xmlOutputDirectory = os.path.join(tacOutputDirectory, "IWXXM")
                xmlOutputFile = os.path.join(xmlOutputDirectory, airportIcaoId) + ".xml"

                if not os.path.exists(xmlOutputDirectory):
                    os.makedirs(xmlOutputDirectory)

                with open(xmlOutputFile, "w") as xmlFile:
                    xmlFile.writelines(xmlOutput)

            except Exception as e:
                self._textProduct.debug_print(e, 1)

        for (airportIcaoId, xmlOutput) in fcsts.items():

            # Write the file using the LocalizationSupport
            relativeFilePath = "aviation/tmp/" + airportIcaoId + ".xml"
            
            #  Write this file into the Localization store - needs to be generalized for all sites
            LocalizationSupport.writeFile(LocalizationSupport.COMMON_STATIC, LocalizationSupport.SITE,
                                          siteID, relativeFilePath, xmlOutput)
        return
    
    def _createNilBaseForecast(self, productDict, airportDict):
        # TODO: Add Support for NIL XML TAFS (Example 4)
        pass

    def _randomUuidString(self):
        return "uuid." + str(uuid4())

