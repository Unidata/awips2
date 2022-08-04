"""########################################################################
# Experimental TAF (TAC and IWXXM)
#
# -------------------------------------------------------------------------
# Description:  This product will generate an experimental version of the
#               Terminal Aerodrome Forecast (TAF) for a list of airports and
#               outputs the TAF in TAC (ASCII) format as specified in the
#               directives and can optionally save the airport TAFs in
#               IWXXM-US 2.0 (XML) format to a user specified directory. The
#               TAC (ASCII) format is what will be returned and displayed in
#               the Formatter Launcher when the formatter is run.
#
#               Tweak the grids and configuration settings until satisfied
#               with the output and then run AvnFPS to transmit the TAF.
# -------------------------------------------------------------------------
#
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
# -------------------------------------------------------------------------
# Standard and Local file names and Locations:
# TAF, TAF_RR_Overrides, TAF_XXX_Overrides,
# TAF_XXX_Definition
# -------------------------------------------------------------------------
# Version: 20220505
#
# Modified: 05 May 2022 - experimental version for Python3
# -------------------------------------------------------------------------
# Author:  GSD Digital Aviation Services Group
#
# Developers: Sarah Pontius and Tom LeFebvre
#
# Support Email: nws.digital.aviation.services@noaa.gov
# -------------------------------------------------------------------------
# Customization Points:
#
# See the Customization information in the
#  "Digital TAF Tools and Formatter Installation" document (located at
# https://docs.google.com/document/d/1Qdz7d0x6uTv90w8l6WKkOQ7Vu17vZO1Ey0jjSoeE8J0/edit
# or locally in /tags/latest_stable/doc) and also the TAF_XXX_Definition
# and TAF_XXX_Overrides files for information about how to customize the
# formatter.
#
# -------------------------------------------------------------------------
# Weather Elements Needed:  Sky, PoP, Wind, WindGust, Wx, CloudBasePrimary,
#                           CloudBaseSecondary, CloudBaseConditional,
#                           Ceiling, Visibility, VisibilityConditional, LLWS
#                           and LLWSHgt
#
# Optional Weather Elements: SkyPrimary, SkySecondary (will be used if present)
#
# -------------------------------------------------------------------------
# Edit Areas Needed:        One edit area for each airport in the TAF
# -------------------------------------------------------------------------
# Development tasks that are identified and in progress:
#
# The shortening algorithms and especially the significance rating rules
# are all still experimental and prototypes. Forecasters are needed to
# help improve the rating rules and shortening algorithms.
#
# -------------------------------------------------------------------------
# Additional Information:
#
# High level flow of the formatter:
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
# Classes Overview:
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
# -------------------------------------------------------------------------
# Example TAC (ASCII) Output:
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
######################################################################## """

from collections import OrderedDict
from functools import reduce, cmp_to_key
from operator import itemgetter
from uuid import uuid4
from xml.dom.minidom import parseString
from xml.etree.ElementTree import Element, SubElement, tostring
import LocalizationSupport
import ProcessVariableList
import SampleAnalysis
import TextRules
import TimeRange
import copy
import inspect
import numpy as np
import os
import pprint
import re
import sys
import time
import traceback


class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    Definition = {
        # --- ======
        # --- GENERIC
        # --- ======
        # --- type -------------------------------------------------------------
        "type": "smart",
        # --- displayName ------------------------------------------------------
        "displayName": None,
        # --- defaultEditAreas -------------------------------------------------
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
        # --- fullStationID ----------------------------------------------------
        "fullStationID": "<fullStationID>",  # full station id (e.g., KBOX)
        # --- wmoID ------------------------------------------------------------
        "wmoID": "<wmoID>",  # product WMO ID (e.g., FTUS41)
        # --- pil --------------------------------------------------------------
        "pil": "TAF<site>",  # product pil (e.g., TAFBOX)
        # --- debug ------------------------------------------------------------
        # Define a flag to turn on/off ALL (or certain) debug messages
        "debug": 1,  # 1 = Full debug, 0 = No debug,  map = debug for certain methods only
        # --- outputFile -------------------------------------------------------
        # Path for formatter output so that it may be stored in the text database.
        # No need to change this as it is only used for the text database.
        # The output is redundantly stored (without the header) in:
        # /caveData/etc/site/[siteID]/aviation/tmp/gridTAF.txt
        # for loading into AvnFPS.
        "outputFile": "/localapps/data/products/gridTAF.txt",
        # --- autoWrite --------------------------------------------------------
        # Specify whether or not to write the product to file
        "autoWrite": 1,  # 1 = Write product to file, 0 = Don't write
        # ==========================================================================
        # Product-specific variables:
        # --- =========
        # --- TAF SPECIFIC
        # --- =========
        # --- tafLength --------------------------------------------------------
        # The maximum length (in hours) of each TAF issued by your office
        "tafLength": {
            # "KBOS": 30,
        },
        # --- CACThresholds ----------------------------------------------------
        # Define the flight category thresholds for each airport
        "CACThresholds": {
            # "KBOS": {"A":(2, 0.50), "B":(7, 2), "C":(10, 3), "D":(30, 5),
            #         "E":(20, 1),
            #         "F":[(25, 3, (340, 80), "Loss of Visual Approach"),
            #              (14, 3, (340, 80), "Loss of Circling Approach"),
            #              ( 8, 2, (340, 80), "ILS Hold Points Assigned")]},
        },
        # --- weatherRules -----------------------------------------------------
        # Define how weather is reported in the TAF output
        "weatherRules": {
            # Handle Thunderstorms specifically
            "T": {
                # Number of hours since Issuance time
                (0, 3): {
                    # Probability/Coverage (Intensity optional)
                    "SChc, Iso": "",  # This isn't probable enough to show
                    "Chc,  Sct": "VCTS",
                    "default": "PREVAIL",
                },
                (3, 9): {
                    "SChc, Iso": "",
                    "Chc,  Sct": "VCTS",
                    "default": "TEMPO",
                },
                "default": {
                    "SChc, Iso": "",  # Too far out, not probable enough
                    "Chc,  Sct": "PROB30",
                    "default": "VCTS",
                },
            },
            # Handle Fog specifically
            "F": {
                # Number of hours since Issuance time
                "default": {
                    # Probability/Coverage (Intensity optional)
                    "default": "PREVAIL"
                },
            },
            # Handle all other precipitation types
            "default": {
                # Number of hours since Issuance time
                (0, 3): {
                    # Probability/Coverage (Intensity optional)
                    "SChc, Iso": "",  # This isn't probable enough to show
                    "Chc,  Sct": "VCSH",
                    "default": "PREVAIL",
                },
                (3, 9): {
                    "SChc, Iso": "",
                    "Chc,  Sct": "VCSH",
                    "default": "PREVAIL",
                },
                "default": {
                    "SChc, Iso": "",  # Too far out, not probable enough
                    "Chc,  Sct": "PROB30",
                    "default": "PREVAIL",
                },
            },
        },
        # --- fmWeatherTypesToRepeat -------------------------------------------
        # A list of TAF weather type codes that can be repeated/copied into a
        # TEMPO or PROB30 group under certain circumstances; whenever a FM
        # group has a TAF weather type code that is in this list and a TEMPO or
        # PROB30 group exists, the TAF weather type code will be shown in the
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
        "fmWeatherTypesToRepeat": ["BR", "RA", "SN", "SHRA"],
        # --- minP6smVisibility ------------------------------------------------
        # The minimum visibility value considered to be P6SM
        "minP6smVisibility": 7,
        # --- tempoProbDefaultHeight -------------------------------------------
        # Cloud base height to use when weatherRules forces a TEMPO/PROB group
        # but CloudBaseConditional isn't available and the cloud base in the FM
        # group is SKC, so we need to guess at a height. This must be a number
        # with a correct reportable value in hundreds of feet.
        "tempoProbDefaultHeight": 40,
        # --- reportVC ---------------------------------------------------------
        # Report "vicinity" weather
        "reportVC": True,
        # --- calmWindSpeed ----------------------------------------------------
        # The maximum speed (knots) allowed to be considered "calm"
        "calmWindSpeed": 2,
        # --- variablesWindSpeed -----------------------------------------------
        # Wind speed (knots) below which winds may be considered variable
        "variableWindSpeed": 4,
        # --- minWindToReportGusts ---------------------------------------------
        # Minimum sustained wind speed to allow reporting of wind gusts
        "minWindToReportGusts": 10,
        # --- minGustSpeedDifference -------------------------------------------
        # Wind gusts must be at least this much higher than sustained wind
        # speed to be reported.
        "minGustSpeedDifference": 8,
        # --- useDetailedCloudHeights ------------------------------------------
        # Toggle detailed cloud reporting
        "useDetailedCloudHeights": 1,  # 0 = No
        # 1 = Yes
        # --- disclaimer -------------------------------------------------------
        # Routine statements for specific airports to indicate when
        # observations are ending and resuming.
        # TODO: Determine if placeholders are supported by AvnFPS in AWIPS 2
        "disclaimer": {},
        # --- verificationHeaders ----------------------------------------------
        # Toggle production of verification headers
        "verificationHeaders": 0,  # 1 = Yes
        # 0 = No (routine)
        # --- areaOverrides ----------------------------------------------------
        # Area overrides allow you to modify behavior for particular airports
        "areaOverrides": {},
        # --- _______________
        # --- SHORTENING
        # --- maxFmGroups ------------------------------------------------------
        # Minimum number of FM groups for each TAF.
        "minFmGroups": 4,
        # --- maxFmGroups ------------------------------------------------------
        # Maximum number of FM groups for each TAF (this isn't a hard limit,
        # it's more of a goal for the formatter)
        "maxFmGroups": 8,
        # --- maxLightWindSpeed ------------------------------------------------
        # Maximum wind speed considered to be light wind
        "maxLightWindSpeed": 5,
        # --- maxSimilarLightWindDirChange -------------------------------------
        # Maximum light wind/llws direction change amount still considered to
        # be similar enough
        "maxSimilarLightWindDirChange": 59,
        # --- maxSimilarWindDirChange ------------------------------------------
        # Maximum non-light wind/llws direction change amount still considered
        # to be similar enough
        "maxSimilarWindDirChange": 39,
        # --- maxSimilarWindGustChange -----------------------------------------
        # Maximum wind gust change amount still considered to be similar enough
        "maxSimilarWindGustChange": 7,
        # --- minVfrVisibility -------------------------------------------------
        # Minimum VFR visibility (in statute miles)
        "minVfrVisibility": 6,
        # --- maxSimilarNonCeilingHeightChange ---------------------------------
        # Maximum non-ceiling height change amount (100s ft) still considered
        # to be similar enough
        "maxSimilarNonCeilingHeightChange": 99,  # found only in _isSkySimilar()
        # --- maxSignificantNonCeilingHeight -----------------------------------
        # Maximum operationally significant non-ceiling height (100s ft)
        "maxSignificantNonCeilingHeight": 50,
        # --- maxSignificantNonCeilingHeight -----------------------------------
        # Maximum operationally significant ceiling height (100s ft)
        "maxSignificantCeilingHeight": 119,  # found only in _skyIsSimilar()
        # --- thresholdsNLValues -----------------------------------------------
        # This allows use of non-linear thresholds for defined fields. The
        # thresholds are for determining if certain fields are 'similar' enough
        # to each other. "WIND_MAG" and "LLWS_MAG" thresholds must be defined
        # here. The values of the map are the thresholds and changes to the
        # field must be less than the threshold to be considered "similar". For
        # example, for wind magnitude, for winds that are >= 50 kts, changes in
        # wind magnitude less than 20 kts will be considered "similar".
        "thresholdsNLValues": {  # Thresholds for surface wind magnitude
            "WIND_MAG": {
                (0.0, 10.0): 4,  # for min speed < 10 kt
                (10.0, 20.0): 8,  # for min speed 10-19 kt
                (20.0, 50.0): 10,  # for min speed 20-49 kt
                "default": 20,  # for min speed >= 50 kt
            },
            # Thresholds for low-level wind shear magnitude
            "LLWS_MAG":
            # {"default": 10,  # for all speeds - changed this to 20 - tl
            # New change for bug 37459
            {
                "default": 20,  # for all speeds
            },
        },
        # --- maxNonSignificantRating ------------------------------------------
        # Maximum significance rating that is considered to be operationally
        # non-significant.
        "maxNonSignificantRating": 60,
        # Set this to True if the site is using SkyPrimary and/or SkySecondary
        # as additional Sky coverage elements. If this is set to True and those
        # elements are not configured in your GFE database, AlertViz errors will
        # display, but the resulting text product will not be affected.
        "useAdditionalSkyElements": False,
        # This variable was moved from the GUI as it was determined most sites
        # would want this most of the time.
        # Set to "Yes" if you want multi-hour TEMPO/PROB groups, otherwise no.
        "allowMultiHourTempoProbs": True,
        # Wind speed threshold below which wind changes are all but ignored when
        # ranking FMGroups.
        "windSpeedRankLow": 7,
        # Moderate Wind speed threshold used to assigning wind ranking
        "windSpeedRankModerate": 12,
        # Wind magnitude temporal difference at which a new FMGroups will be automatically
        # generated
        "windMagDiff": 10,
        # Wind direction threshold below which wind changes are all but ignored
        # when ranking FMGroups.
        "windDirRankLarge": 30,
        # Any hourly wind gust change of this magnitude will generate a new FMGroup
        "windGustDiff": 7,
        # Threshold at or above which LLWS will be reported
        "llwsThreshold": 30,
        # Default number of FMGroups
        "numberOfFMGroups": 6,
        # Time weighting values will emphasize come periods over others.
        # Format (hours, weight). In example below, first 6 hours is 1.2,
        # the NEXT 12 hours is 1.0, and the LAST 18 hours is 0.9.
        "timeWeights": [(6, 1.2), (12, 1.0), (18, 0.9)],
        # Define the rank value below which we do not keep a FMGroup
        # This weeds out the FMGroups that don't add much to the TAF
        # If no reduction of FMGroups is desired, set this to 0.
        "minimumRankValue": 25,
        # Translates GFE Wx type to TAF Wx type.  DO NOT MODIFY.
        "gfeCodeToTafCodeMap": {
            "<NoWx>": "",  # No weather
            "T": "TS",  # Thunderstorm
            "R": "RA",  # Rain
            "RW": "SHRA",  # Rain shower
            "L": "DZ",  # Drizzle
            "ZR": "FZRA",  # Freezing rain
            "ZL": "FZDZ",  # Freezing drizzle
            "S": "SN",  # Snow
            "SW": "SHSN",  # Snow shower
            "IP": "PL",  # Ice pellets/sleet
            "F": "BR",  # Mist/fog (visibility > 1/2SM)
            "ZF": "FZFG",  # Freezing fog (visibility <= 1/2SM)
            "IF": "BR",  # Ice fog (visibility > 1/2SM)
            "IC": "IC",  # Ice crystals
            "H": "HZ",  # Haze
            "BS": "BLSN",  # Blowing snow
            "BN": "BLSA",  # Blowing sand
            "BD": "BLDU",  # Blowing dust
            "K": "FU",  # Smoke
            "ZY": "",  # Freezing spray (FZPY) - never report it
            "FR": "",  # Frost (FR) - never report it
            "VA": "VA",  # Volcanic ash
            # GFE attributes
            "SmA": "GR",  # Small hail - must have thunderstorm
            "LgA": "GR",  # Large hail - must have thunderstorm
        },
        # LLWS wind magnitude at or above this value will appear in the TAF
        # if that FMGroup is ranked high enough.
        "runwayInfo": {},
    }

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

        if not self._areaList:
            return "WARNING -- No Edit Areas Specified to Generate Product."

        # Create the product parts that aren't airport specific and add them
        # to the product dictionary.
        self._createSharedProductParts()

        # Generate the product for each edit area in the list
        for editArea, airportIcaoId in self._areaList:
            # ==================================================================
            # Let's get started on this TAF site
            self.debug_print(f"\n{'#' * 80}\nWorking on -> {airportIcaoId}", 1)

            self._resetDefinitions(argDict["forecastDef"], airportIcaoId)

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
            self.debug_print(f"Failed to create IWXXM files: {e}", 1)

        # Create and return the TAF in TAC (ASCII) format
        formatter = TAC_Formatter(self)
        fcst = ""
        fcst = self._preProcessProduct(fcst, argDict)

        fcst = formatter.createOutput(fcst, self._productDict, self._airportDicts)

        fcst = self._postProcessProduct(fcst, argDict)

        return fcst

    def _resetDefinitions(self, definitions, airportID):
        """
        Refreshes the set of configuration variables. Users can define
        overrides for specific airports and here is where these get
        defined for the life of the TAF for the specified airport.
        """

        # First restore to initial "global" setting
        for varName, value in definitions.items():
            if varName == "airportOverrides":
                continue
            setattr(self, f"_{varName}", value)

        # Now find the airport-specific overrides and replace
        if "airportOverrides" not in definitions:
            return
        if airportID not in definitions["airportOverrides"]:
            return

        airportDefs = definitions["airportOverrides"][airportID]
        for varName, value in airportDefs.items():
            setattr(self, f"_{varName}", value)

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

        # ----------------------------------------------------------------------

        # Display TAF Version and current time
        self.debug_print(f"\n{'#' * 80}", 1)
        self.debug_print(f"TAF {self._getVersion()}\n", 1)
        self.debug_print(f"current time = {self._currentTimeSeconds} (seconds)", 1)
        self.debug_print(f"current time = {time.asctime(self._currentTimeUTC)} (UTC)", 1)
        self.debug_print(f"current time = {time.asctime(self._currentTimeLocal)} (local)", 1)

        # ----------------------------------------------------------------------

        self._getConfigurationSettings(argDict)
        self._getGuiSelections(argDict)

        # ----------------------------------------------------------------------

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
        self._maxTafLength = max(self._tafLength.values())

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

        self.debug_print(f"\n{'=' * 80}\n{airportIcaoId} Time Information:", 1)

        # ----------------------------------------------------------------------
        # Determine the number of one hour periods we need to sample

        (self._startTimeSeconds, self._endTimeSeconds) = self._getAirportStartEndTimes(
            airportIcaoId
        )

        startTimeUTC = time.gmtime(self._startTimeSeconds)
        endTimeUTC = time.gmtime(self._endTimeSeconds)

        numPeriods = int((self._endTimeSeconds - self._startTimeSeconds) // 3600)

        self.debug_print(
            f"\nstart time = {time.asctime(startTimeUTC)} (UTC)\n"
            f"end time = {time.asctime(endTimeUTC)} (UTC)\nnumPeriods = {numPeriods}",
            1,
        )

        # ----------------------------------------------------------------------
        # Make a time range for this airport TAF

        # Zero out the minutes and seconds (Sampling done at 1 hour boundaries)
        startTimeSeconds = self._startTimeSeconds - startTimeUTC.tm_min * 60 - startTimeUTC.tm_sec

        endTimeSeconds = self._endTimeSeconds - endTimeUTC.tm_min * 60 - endTimeUTC.tm_sec

        self._timeRange = self.makeTimeRange(startTimeSeconds, endTimeSeconds)

        self.debug_print(f"final time range = {self._timeRange}", 1)

        # ----------------------------------------------------------------------
        # Make the periods we need to sample - one hour blocks with no gaps

        # Define a label template for each time period
        # Format is of form: (LT_OR_Zulu, durationFmt, startFmt, endFmt)
        tafLabel = ("Zulu", "", "FM%d%H00", "")

        # Create the number of periods requested.
        self._samplePeriods = self.getPeriods(
            self._timeRange, period=1, span=1, numPeriods=numPeriods, labelFormat=tafLabel
        )

        self.debug_print("sample periods:", 1)
        for index, (tr, fmGroupLabel) in enumerate(self._samplePeriods):
            self.debug_print(f"{index + 1:02d}) {fmGroupLabel}:    {tr}", 1)

        # ----------------------------------------------------------------------
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

            self._allPeriodStartEndTimes.append((periodStartTimeSeconds, periodEndTimeSeconds))

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

        # Fetch the default number of FMGroups
        numberOfFMGroups = definition.get("numberOfFMGroups", 6)

        # Define formatter GUI options
        varList = [
            (
                ("TAF base time", "productIssuance"),
                baseTime,
                "radio",
                ["00Z", "06Z", "12Z", "18Z"],
            ),
            (
                ("TAF Type", "tafType"),
                tafType,
                "radio",
                ["Routine", "Amendment", "Delayed", "Corrected"],
            ),
            (
                ("Only allow one\nTEMPO per TAF?", "limitOneTempoPerTAF"),
                "No",
                "radio",
                ["Yes", "No"],
            ),
            (
                ("Suggested Number of FMGroups:", "detail"),
                numberOfFMGroups,
                "scale",
                [1, 10],
                1,
            ),
        ]

        self._tafType = "Routine"

        # Process the GUI selections
        processVarList = ProcessVariableList.ProcessVariableList(
            "Terminal Aerodrome Forecast", varList, varDict={}
        )

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

    # --------------------------------------------------------------------------
    # ----------------------------================------------------------------
    # --------------------------TAF-SPECIFIC METHODS----------------------------
    # ----------------------------================------------------------------

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
        module = inspect.getmodule(lambda: 0)

        # Get the documentation at the top of this file
        tafDoc = inspect.getdoc(module)

        # Find where the version information is specified
        versionStart = tafDoc.find("Version:")
        versionEnd = tafDoc[versionStart:].find("\n")

        return tafDoc[versionStart:][:versionEnd]

    def _getConfigurationSettings(self, argDict):
        """Get Definition variables."""

        self.debug_print(f"\n{'#' * 80}\nDefinition Dictionary:", 1)
        self._definition = argDict["forecastDef"]
        for (key, value) in self._definition.items():
            setattr(self, f"_{key}", value)

            if key in ["CACThresholds", "weatherRules", "areaOverrides"]:
                continue  # These will be displayed later for each airport
            else:
                self.debug_print(f"\n{key}:", 1)
                self.debug_print(f"\n\t{self._pformat(eval(f'self._{key}'))}", 1)

    def _getGuiSelections(self, argDict):
        """Make the GUI selected options available as variables."""

        self.debug_print(f"\n{'#' * 80}\nGUI Selections:", 1)
        varDict = argDict["varDict"]
        for (key, value) in varDict.items():
            if isinstance(key, tuple):
                label, variable = key
                setattr(self, f"_{variable}", value)

                self.debug_print(f"\n{key}:", 1)
                self.debug_print(f"\n\t{self._pformat(eval('varDict[key]'))}", 1)

    # --- ________________________________
    # --- AIRPORT-SPECIFIC METHODS

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
        endTimeSeconds = startTimeSeconds + self._tafLength[airportIcaoId] * 3600

        # If this is an amendment, adjust the start time to the current time.
        # End hour is not adjusted because it only changes when the next
        # routine TAF is issued.
        if self._tafType == "Amendment":
            startTimeSeconds = self._currentTimeSeconds
            # If we are in the bottom half of the hour
            if self._currentTimeUTC.tm_min >= 29:
                # Start with the next hour instead
                startTimeSeconds += 3600

        return startTimeSeconds, endTimeSeconds

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

        self.debug_print(f"\n{'=' * 80}\n{airportIcaoId} Area Overrides:", 1)

        self._oldAttributes = []
        areaOverrides = self._areaOverrides.get(airportIcaoId, [])

        for (attributeName, newValue) in areaOverrides:
            self.debug_print(f"\n{attributeName}:", 1)
            self.debug_print(f"\n\t{self._pformat(newValue)}", 1)

            oldValue = getattr(self, attributeName)
            self._oldAttributes.append((attributeName, oldValue))

            # When overriding methods, the new method will be a string to
            # prevent errors. For example:
            # ("_createSampler", "_createSamplerKMIA")
            if isinstance(newValue, str) and hasattr(self, newValue):
                # This is a method override
                newValue = eval(f"self.{newValue}")

            setattr(self, attributeName, newValue)

    def _finalizeAnalysisList(self, argDict, editArea, airportIcaoId):
        """Finalize the analysis list to use for sampling the grids."""

        # self.debug_print(f"\n{'=' * 80}\n{airportIcaoId} Analysis List:", 1)
        # Start off with the standard weather elements
        self._analysisList = self._getAnalysisList()

        if self._useAdditionalSkyElements:
            self._addOptionalToAnalysisList(argDict, editArea, airportIcaoId)

        # self.debug_print("\nFinal Analysis List:", 1)
        # for weatherElementInfo in self._analysisList:
        #     self.debug_print(f"{weatherElementInfo}", 1)

    def _addOptionalToAnalysisList(self, argDict, editArea, airportIcaoId):
        """Get optional weather elements if they exist"""
        # self.debug_print("\nChecking for existence of Optional weather elements", 1)

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
            sampleInfo = [(singleElementAnalysisList, self._samplePeriods, self._areaList)]
            try:
                # Temporarily turn off debug and print statements
                self._debug = 0
                # Try creating a sampler (getSampler) and sampling the weather
                # element (getStatList). If it fails, the weather element is
                # not defined
                sampler = self.getSampler(argDict, sampleInfo)

                # Sample only this weather element and no others
                self.getStatList(sampler, singleElementAnalysisList, self._samplePeriods, editArea)

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
        sampleInfo = [(self._analysisList, self._samplePeriods, self._areaList)]

        # Create the sampler for this product
        self._sampler = self.getSampler(argDict, sampleInfo)

        # Save off the edit area for when we actually sample the data
        self._editArea = editArea

    def _setFlightCategoryOrder(self, airportIcaoId):
        """Determines the order of the flight categories for this airport."""

        self.debug_print(f"\n{'=' * 80}\n{airportIcaoId} Flight Category Information:", 1)
        self.debug_print(" ", 1)

        # ----------------------------------------------------------------------
        # Store information about each of the categories in order from the
        # worst category to the best category.
        self._orderedCategoryInfo = []

        # Store all the category information
        self._addFlightCategoryInfo(self._CACThresholds[airportIcaoId])

        # Sort first by visibility (index 2) then by ceiling (index 1)
        self._orderedCategoryInfo = sorted(self._orderedCategoryInfo, key=itemgetter(2, 1))

        # Always end with VFR
        self._orderedCategoryInfo.append(("V", None, None))

        self._displayOrderedFlightCategories()

    def _addFlightCategoryInfo(self, flightCategoryThresholds):
        """Store all the flight category information."""

        # Categories A - E are required
        for category in ["A", "B", "C", "D", "E"]:
            thresholds = flightCategoryThresholds[category]
            # self.debug_print(
            #     f"processing category {category} thresholds: {thresholds}", 1
            # )

            (ceiling, visibility) = thresholds

            self._orderedCategoryInfo.append((category, ceiling, visibility))

        # F Categories are optional
        fCategoryThresholds = flightCategoryThresholds.get("F", [])
        for (index, thresholds) in enumerate(fCategoryThresholds):
            # Append the index to make each F threshold unique
            category = f"F{index}"
            # self.debug_print(
            #     f"processing category {category} thresholds: {thresholds}", 1
            # )

            (ceiling, visibility, windDirections) = thresholds[:3]

            self._orderedCategoryInfo.append((category, ceiling, visibility, windDirections))

    def _displayOrderedFlightCategories(self):
        self.debug_print(" ", 1)
        self.debug_print("Final Order:", 1)
        for categoryInfo in self._orderedCategoryInfo:
            if len(categoryInfo) == 3:
                self.debug_print(
                    f"{categoryInfo[0]}:\tvisibility: {categoryInfo[1]}ceiling: {categoryInfo[2]}",
                   
                    1,
                )
            else:
                self.debug_print(
                    f"{categoryInfo[0]}:\tvisibility: {categoryInfo[2]}"
                    f"ceiling: {categoryInfo[1]}wind directions: {categoryInfo[3]}",
                    1,
                )

    def _displayWeatherRulesInfo(self, airportIcaoId):
        """Display the weather rules in a nice format."""

        self.debug_print(f"\n{'=' * 80}\n{airportIcaoId} Weather Rules:", 1)

        def sortKeys(weatherRules):

            keys = list(weatherRules)

            defaultPresent = False
            if "default" in keys:
                keys.remove("default")
                defaultPresent = True

            keys = sorted(keys)

            # Make sure default is always listed last
            if defaultPresent:
                keys.append("default")

            return keys

        gfeWeatherTypes = sortKeys(self._weatherRules)
        for weatherType in gfeWeatherTypes:
            self.debug_print(f"\n\t{weatherType}:", 1)

            occurrenceTimeRanges = sortKeys(self._weatherRules[weatherType])
            for occurrenceRange in occurrenceTimeRanges:
                self.debug_print(f"\t{occurrenceRange}:", 1)

                probabilities = sortKeys(self._weatherRules[weatherType][occurrenceRange])
                for probability in probabilities:
                    classifier = self._weatherRules[weatherType][occurrenceRange][probability]
                    self.debug_print(f"\t\t{f'{probability}:':12}{classifier}", 1)

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
        return sum(valueList) / len(valueList)

    # Calculate the vector average using the (dir, speed) pairs in valueList.
    # In some cases the direction is "VRB" so that case is accounted for and
    # thus a separate count of direction values is needed.
    def _vectorAverage(self, valueList):
        uSum = 0.0
        vSum = 0.0
        speedSum = 0.0
        dirCount = 0
        for windDir, windSpeed in valueList:
            speedSum += windSpeed
            if windDir == "VRB":  # skip VRBs
                continue
            # get the u and v for the unit vector to remove influence from speed
            u, v = self.MagDirToUV(1.0, windDir)
            uSum += u
            vSum += v
            dirCount += 1  # keep a separate count of direction values
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

    def _revertAreaOverrides(self, airportIcaoId):
        """
        This method goes through all the defined overrides for a particular
        airport and reverts them back to the default values.
        """
        if airportIcaoId in self._areaOverrides:
            for (attributeName, oldValue) in self._oldAttributes:
                setattr(self, attributeName, oldValue)

    # --------------------------------------------------------------------------
    # -------------------------------===========--------------------------------
    # ------------------------------PRODUCT-PARTS-------------------------------
    # -------------------------------===========--------------------------------

    def _createSharedProductParts(self):
        self.debug_print(f"\n{'#' * 80}\nShared (Not Airport Specific) Product Parts:", 1)
        for productPart in self._bulletinHeaderParts():
            self._productPart = productPart
            method = getattr(self, f"_{productPart}")
            if method:
                method(self._productDict, productPart)

    def _createAirportProductParts(self, airportIcaoId):
        for productPart in self._airportTafParts():
            method = getattr(self, f"_{productPart}")
            if method:
                method(self._airportDicts[airportIcaoId], productPart, airportIcaoId)

        # Before trying to shorten the TAF, save a copy of all the FM groups
        fmGroups = self._airportDicts[airportIcaoId]["fmGroups"]

        if not fmGroups:
            self.debug_print("No FMGroups found in self._airportDicts.")
            return

        allFmGroups = fmGroups[:]
        # Try to make the TAF have as few FM groups as possible while still
        # trying to have the TAF be as meteorologically accurate as possible
        # and showing all the operationally significant information
        shorteningAlgorithms = self.ShorteningAlgorithms(self, airportIcaoId)

        keeperFMGroups = shorteningAlgorithms.shortenFmGroups(fmGroups)

        self._airportDicts[airportIcaoId]["fmGroups"] = keeperFMGroups

        self._finalizeTemposProbs(keeperFMGroups, allFmGroups, airportIcaoId)

        self._displayShortenedTafInfo(keeperFMGroups)

        return

    def _setProductPart(self, value):
        self._productDict[self._productPart] = value

        if isinstance(value, time.struct_time):
            value = time.asctime(value)  # Create a nicely formatted time string

        self.debug_print(f"\n{self._productPart} = {value}", 1)

    # --- ___________________________
    # --- BULLETIN HEADER PARTS
    def _bulletinHeaderParts(self):
        """Product Parts for header info (i.e. FTUS42 KMFL 141100 AAA)."""
        return [
            "bulletinType",  # FT
            "bulletinLocation",  # US
            "bulletinDifferentiator",  # 42
            "icaoOfficeIndicator",  # KMFL
            "issuanceTime",  # 141100
            "tafTypeIdentifier",  # AAA
        ]

    def _bulletinType(self, productDict, productPart):
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
            typeIdentifier = typeIdentifier.replace("X", "A")

        self._setProductPart(typeIdentifier)

    # --- _________________
    # --- AIRPORT PARTS
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
        productDict[productPart] = f"TAF{airportIcaoId[1:]}"

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

    def determineAMDType(self, start, end, currentHour):
        """
        Helper method to determine which of the three types should be reported
        based on the start and end of the outage and the current hour.
        """
        # Ensure the start is less than the end
        startHour = start
        endHour = end
        if start > end:
            startHour = end
            endHour = start

        amdType = ""
        startDiff = startHour - currentHour
        endDiff = endHour - currentHour
        if currentHour < startHour:
            if startDiff in range(7):
                if endDiff in range(7):
                    amdType = "FULL"
                else:
                    amdType = "AFT"
            else:
                amdType = ""
        elif currentHour >= startHour:
            if currentHour < endHour:
                endDiff = endHour - currentHour
                if endDiff in range(6):
                    amdType = "TIL"
                elif currentHour > endHour:
                    amdType = ""
                else:
                    amdType = "FULL"

        # See if the start and end hours were out of order and if so, reverse the type
        # based on the following dictionary.
        flipDict = {
            "AFT": "TIL",
            "TIL": "AFT",
            "FULL": "",
            "": "FULL",
        }
        if start > end:
            return flipDict[amdType]
        else:
            return amdType

    def getIssuanceTime(self, issuanceHour):
        """
        Method to figure out the issuance time based on the clock time and issuanceHour.
        """
        nowHour = time.gmtime().tm_hour
        if int(issuanceHour) > nowHour:
            diff = (int(issuanceHour) - nowHour) * 3600
        else:
            diff = (int(issuanceHour) - nowHour) + (24 * 3600)
        # Calculate the true issuance time with the 40-minute offset
        issuanceTime = int((time.time() + (40 * 60) + diff) / (6 * 3600)) * 6 * 3600

        return issuanceTime

    def getStartEndDay(self, startHour, endHour, issuanceHour):
        """
        Returns the appropriate start and end day, given the start and
        end hour. Assumes the next six-hour period starts 40 minutes
        before 00, 06, 12, and 18Z.
        """
        # Get the time of the product that will be issued
        issuanceTime = self.getIssuanceTime(issuanceHour)

        now = int((time.time() + (40 * 60)) / (6 * 3600)) * 6 * 3600
        currentHour = time.gmtime(issuanceTime).tm_hour
        start = end = issuanceTime
        oneDay = 24 * 3600
        if startHour < endHour:
            if startHour < currentHour:
                start += oneDay
                end += oneDay
        else:
            end += oneDay
            if endHour < startHour < currentHour:
                start += oneDay
        startDay = time.gmtime(start).tm_mday
        endDay = time.gmtime(end).tm_mday
        return startDay, endDay

    def _disclaimerStr(self, airportIcaoId):
        """
        Generates and returns the disclaimer string based on configuration settings.
        """
        disDict = self._disclaimer.get(airportIcaoId, None)
        if disDict is None:
            return ""

        issuanceHour = int(self._productIssuance[0:2])
        disStr = ""
        disKeys = []
        for disKey in disDict:
            if isinstance(disKey, tuple):
                disKeys.append(disKey)

        if not disKeys:
            return disStr

        allowedHours = [0, 6, 12, 18]
        if "allowedHours" in disDict:
            allowedHours = disDict["allowedHours"]
        if int(issuanceHour) not in allowedHours:
            return disStr

        for key in disKeys:
            startHour, endHour = key
            startDay, endDay = self.getStartEndDay(startHour, endHour, issuanceHour)
            startDayStr = str(startDay).zfill(2)
            endDayStr = str(endDay).zfill(2)

            amdType = self.determineAMDType(startHour, endHour, issuanceHour)
            startHourStr = str(startHour).zfill(2)
            endHourStr = str(endHour).zfill(2)
            if not amdType:
                return ""
            elif amdType == "AFT":
                disStr = f"{disDict[key]} AFT {startDayStr}{startHourStr}00"
            elif amdType == "TIL":
                disStr = f"{disDict[key]} TIL {endDayStr}{endHourStr}00"
            elif amdType == "FULL":
                disStr = f"{disDict[key]} {startDayStr}{startHourStr}/{endDayStr}{endHourStr}"
            else:
                disStr = disDict[key]

        return disStr

    def _fmGroups(self, productDict, productPart, airportIcaoId):
        """
        Create all the forecast periods for this site based off the
        data in the grids.
        """
        # Get the hourly statistics for this area
        self.debug_print(f"\n{'=' * 80}\n{airportIcaoId} Sample Analysis Information:", 1)
        self.debug_print(f"\nProductPart: {productPart} Airport: {airportIcaoId}")
        statLists = self.getStatList(
            self._sampler, self._analysisList, self._samplePeriods, self._editArea
        )

        # Create the list of FM groups
        fmGroups = []
        for index, (timeRange, fmGroupLabel) in enumerate(self._samplePeriods):
            self._periodIndex = index
            self._period = index + 1
            statList = statLists[index]

            invalidPeriod = False
            problemsFound = False

            self.debug_print(f"\n{'_' * 80}", 1)

            someDataPresent = any([stat is not None for stat in statList.values()])
            if someDataPresent:
                fmGroup = self.FmGroup(
                    self,
                    statList,
                    self._productDict,
                    self._airportDicts[airportIcaoId],
                    fmGroupLabel,
                    self._period,
                )

                problemsFound = fmGroup.createProductParts()

                if not problemsFound:
                    self.debug_print(
                        f"\nFinal {fmGroupLabel} (Period {self._period}) product parts:",
                        1,
                    )
                    for (part, value) in fmGroup.productDict.items():
                        self.debug_print(f"{part} = {value}", 1)

                    fmGroups.append(fmGroup)

            if not someDataPresent:
                invalidPeriod = True
                self.debug_print(
                    f"\nNo statistics for {fmGroupLabel} (Period {self._period})",
                    1,
                )
            elif problemsFound:
                invalidPeriod = True
                self.debug_print(
                    f"\nFailed to create {fmGroupLabel} (Period {self._period})",
                    1,
                )

            if invalidPeriod and self._period == 1:
                self.debug_print(
                    f"\nInvalid airport TAF: First period in valid period time range always required.",
                    1,
                )
                productDict[productPart] = list()
                return

        productDict[productPart] = fmGroups

    def _airportDisclaimer(self, productDict, productPart, airportIcaoId):
        # Check if this airport has a disclaimer for this issuance time
        productDict[productPart] = None  # Start off assuming no disclaimer
        startHour = time.gmtime(self._startTimeSeconds).tm_hour
        if isinstance(self._disclaimer, tuple):
            self._disclaimer = self._disclaimer[0]  # just keep the text portion
        if airportIcaoId in self._disclaimer:
            disclaimerStr = self._disclaimerStr(airportIcaoId)
            productDict[productPart] = disclaimerStr

    class GenericGroup:
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
            return (
                f"{self._airportDict['icaoAirportIndicator']} "
                f"{self.productDict['groupClassification']} group {self.productDict['groupLabel']}"
            )

        def __str__(self):
            return self.__repr__()

        def _getData(self, statName, newType, required=False, nullValue=None):
            self._textProduct.debug_print(
                f"\n{'_' * 40}\n{statName} Grid Information{' (Required)' if required else ''}:",
                1,
            )

            # check to see if the stat exists first
            # if statName in self._statList:
            #     data = self._statList[statName]
            # else:  # the element was likely not configured
            #     data = None

            data = self._statList.get(statName, None)

            # Ignore the no data error if the element is not required
            if data is None or data == nullValue:
                raise TAF_DataException("No data available for this period", required)
                return data

            # Return the data as the appropriate data type
            try:
                if isinstance(data, tuple):
                    if data[0] is None or data[1] is None:
                        return None

                    self._textProduct.debug_print(f"Value = ({data[0]}, {data[1]}):", 1)
                    self._textProduct.debug_print(f"Value = ({data[0]}, {data[1]}):", 1)
                    self._textProduct.debug_print(
                        f"Converting ({type(data[0])}, {type(data[1])}) to "
                        f"({newType[0]}, {newType[1]})",
                        1,
                    )

                    data = (eval("newType[0](data[0])"), eval("newType[1](data[1])"))

                    self._textProduct.debug_print(f"New value = ({data[0]}, {data[1]})", 1)

                else:
                    self._textProduct.debug_print(f"Value = {data}:", 1)
                    self._textProduct.debug_print(f"Converting {type(data)} to {newType}", 1)

                    data = eval("newType(data)")

                    self._textProduct.debug_print(f"New value = {data}", 1)
            except:
                raise TAF_DataException(f"Failed to convert {statName}", required)

            return data

        def _getOptionalData(self, weatherElementName, newType, alternateWeatherElementName):
            optionalWeatherElements = self._textProduct._optionalWeatherElements

            self._textProduct.debug_print(
                f"Getting data for AWT weather element {weatherElementName}", 1
            )
            if weatherElementName in optionalWeatherElements:
                try:
                    return self._getData(weatherElementName, newType)
                except:
                    self._textProduct.debug_print(
                        f"\tNo data available for {weatherElementName}. "
                        f"Using {alternateWeatherElementName} instead.",
                        1,
                    )
                    return self._getData(alternateWeatherElementName, int)
            else:
                self._textProduct.debug_print(
                    f"\t{weatherElementName} does not exist. "
                    f"Using {alternateWeatherElementName} instead.",
                    1,
                )
                return self._getData(alternateWeatherElementName, int)

        def _roundVisibilityToReportableValue(self, numericalVisibility):
            """
            Round visibility to a reportable value in a way that is consistent
            with the tools.
            """

            if numericalVisibility is None:
                return None

            # TODO: This logic, while consistent with tools, differs from the directive.

            # If visibility < 1, then round it to the nearest 0.25
            if numericalVisibility < 1:
                numericalVisibility = round(4.0 * numericalVisibility) / 4.0

            # If 1 <= visibility < 2.5, then round it to the nearest 0.5
            elif 1 <= numericalVisibility < 2.5:
                numericalVisibility = round(2.0 * numericalVisibility) / 2.0

            # Otherwise, round it to the nearest whole number
            else:
                numericalVisibility = round(numericalVisibility)

            return numericalVisibility

        def _determineWeather(self, groupType, numericalVisibility):
            groupWeather = self._textProduct.GroupWeatherInfo(
                self._textProduct, groupType, numericalVisibility
            )

            weatherSubKeys = self._getData("Wx", list)
            print(f"_determineWeather-wxSubKeys: {str(weatherSubKeys)}")
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
                self._textProduct.debug_print(f"\nWorking on {weatherUglyString}", 1)

                try:
                    weatherInfo = self._textProduct.WeatherUglyStringInfo(
                        self._textProduct, weatherUglyString, numericalVisibility, groupType
                    )

                except TAF_DataException as e:
                    self._textProduct.debug_print(e, 1)
                    self._textProduct.debug_print(f"Skipping this weather: {weatherSubKey}", 1)
                    continue

                self._textProduct.debug_print(f"\n\tTAF type = {weatherInfo.tafWeatherType}", 1)
                self._textProduct.debug_print(
                    f"weatherRules classifier = {weatherInfo.classifier}", 1
                )

                weatherIsForFmGroup = (groupType == "FM") and (
                    "VC" in weatherInfo.classifier or weatherInfo.classifier == "PREVAIL"
                )

                weatherIsForCurrentGroup = weatherIsForFmGroup or (
                    groupType == weatherInfo.classifier
                )

                if not weatherIsForCurrentGroup:
                    self._textProduct.debug_print(f"Skipping weather: not for {groupType} group", 1)
                    continue
                # Add the weather information to this group
                groupWeather.updateWeather(weatherInfo)

            return groupWeather

        def _finalizeWeather(self, productPart, fmWeather):
            weather = self.productDict.get(productPart, None)
            if weather is None:
                self.productDict[productPart] = None
                return

            # If this is a conditional group, repeat any necessary FM group
            # weather
            if weather.groupType == "TEMPO" or weather.groupType == "PROB30":
                weather.addAnyRepeatingFmWeather(fmWeather)

            # Consolidate and remove duplicates while preserving insertion
            # order (unless the directives specify to reorder certain weather)
            weather.postProcessWeatherTypes()

            self._textProduct.debug_print("\nGroup Weather Information:", 1)
            self._textProduct.debug_print(f"maxIntensity = {weather.maxIntensity}", 1)
            self._textProduct.debug_print(f"regularWeatherTypes = {weather.regularWeatherTypes}", 1)
            self._textProduct.debug_print(
                f"obstructionWeatherTypes = {weather.obstructionWeatherTypes}", 1
            )
            self._textProduct.debug_print(f"vicinityWeather = {weather.vicinityWeather}", 1)

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
                    return int(self._textProduct.round(cloudHeight, "Nearest", 1))

                elif cloudHeight < 50:  # Clouds 3,000 - 5,000 feet
                    # Round to nearest 500 feet
                    return int(self._textProduct.round(cloudHeight, "Nearest", 5))

                else:  # Clouds >= 5,000 feet
                    # Round to nearest 1000 feet
                    return int(self._textProduct.round(cloudHeight, "Nearest", 10))

            # Otherwise, report categorical cloud heights
            else:
                if cloudHeight < 2:  # Handle 100 feet clouds
                    return 1  # 100 feet

                elif 2 <= cloudHeight < 6:  # Clouds 200-500 feet
                    return 3  # 300 feet

                elif 6 <= cloudHeight < 10:  # Clouds 600-900 feet
                    return 8  # 800 feet

                elif cloudHeight <= 50:  # Clouds 1,000-5,000 feet
                    # Round to nearest 500 feet
                    return int(self._textProduct.round(cloudHeight, "Nearest", 5))

                elif cloudHeight <= 150:  # Clouds 6,000-15,000 feet
                    # Round to nearest 1000 feet
                    return int(self._textProduct.round(cloudHeight, "Nearest", 10))

                else:  # Clouds > 15,000 feet
                    # Round to nearest 5000 feet
                    return int(self._textProduct.round(cloudHeight, "Nearest", 50))

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
        def __init__(self, textProduct, statList, sharedDict, airportDict, label, period):
            TextProduct.GenericGroup.__init__(self, textProduct, statList, sharedDict, airportDict)

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
            return (
                f"{TextProduct.GenericGroup.__repr__(self)} (Period {self.productDict['fmPeriod']})"
            )

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
                "ratings",  # Rates the significance of each of the product parts
                # "numChanges",
                # "similarChecks",
            ]

        def createProductParts(self):
            self._textProduct.debug_print(
                f"Gathering statistics for {self._label} "
                f"({self._airportDict['icaoAirportIndicator']} Period {self.period})",
                1,
            )
            self._textProduct.debug_print(
                "____________________________________________________________________"
            )

            problemsFound = False
            for productPart in self.productParts():
                try:
                    method = getattr(self, f"_{productPart}")
                    if method:
                        method(productPart)

                except Exception as e:
                    self._textProduct.debug_print(f"\n{traceback.format_exc().strip()}", 1)

                    if isinstance(e, TAF_DataException):
                        if not e.requiredData:
                            # The issue is with data that isn't required so
                            # ignore it and just set the product part to None
                            self.productDict[productPart] = None
                            self._textProduct.debug_print(
                                f"Ignoring issue: {productPart} is not required (setting to None)",
                                1,
                            )
                        else:
                            print("Data not required, but still an Exception. Part:", productPart)
                            print("productDict:", self.productDict)
                            problemsFound = True
                    else:
                        problemsFound = True
                        print("Not a data exception:", e, "Part:", productPart)
                        print("productDict:", self.productDict)
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
                startTimeSeconds = self._textProduct._allPeriodStartEndTimes[self.hourOffset][0]

            self.productDict[productPart] = startTimeSeconds

        def _endTime(self, productPart):
            endTimeSeconds = self._textProduct._allPeriodStartEndTimes[self.hourOffset][1]

            self.productDict[productPart] = endTimeSeconds

        def _wind(self, productPart):
            """Get the prevailing wind direction and wind speed."""

            # Speed in knots.
            # Direction in degrees (rounded to nearest 10 degrees).
            windData = self._getData("Wind", (int, int), required=True)
            if not windData:
                self.productDict[productPart] = None
                print("Failed to get Wind data.")
                return
            speed, direction = windData
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
            windData = self.productDict["wind"]
            if not windData:
                self.productDict[productPart] = None
                print("Failed to get WindGust data.")
                return

            windDirection, windSpeed = windData
            windGust = self._getData("WindGust", int)
            # If we should not report a gust - either because the wind gust
            # stats are missing or sustained wind is less than the defined
            # minimum threshold for reporting gusts or gust speed does not
            # exceed sustained wind by the minimum required amount
            self.productDict[productPart] = windGust

        def _visibility(self, productPart):
            """Get the prevailing numerical visibility."""
            # TODO: Add Vertical Visibility support?
            # TODO: Ensure FM and TEMPO/PROB visibilities never identical

            visibility = self._getData("Visibility", float, required=True)
            visibility = self._roundVisibilityToReportableValue(visibility)

            self.productDict[productPart] = visibility

        def _weather(self, productPart):
            self.productDict[productPart] = self._determineWeather(
                "FM", self.productDict["visibility"]
            )

            self._finalizeWeather("weather", self.productDict["weather"])

        def _sky(self, productPart):
            # Cloud coverage (in percentage)
            self.productDict[productPart] = self._getData("Sky", int)

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
            cloudBasePrimary = self._getData("CloudBasePrimary", int, nullValue=0)

            self.productDict[productPart] = self._roundCloudHeight(cloudBasePrimary)

        def _cloudBaseSecondary(self, productPart):
            # Secondary predominant cloud height (in 100s feet)
            cloudBaseSecondary = self._getData("CloudBaseSecondary", int, nullValue=0)

            self.productDict[productPart] = self._roundCloudHeight(cloudBaseSecondary)

        def _cloudBaseTertiary(self, productPart):
            # Secondary predominant cloud height (in 100s feet)
            cloudBaseTertiary = self._getData("CloudBaseTertiary", int, nullValue=0)

            self.productDict[productPart] = self._roundCloudHeight(cloudBaseTertiary)

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
            llwsData = self._getData("LLWS", (int, int))
            if not llwsData:
                self.productDict[productPart] = None
                return
            speed, direction = llwsData
            outStr = f"{productPart} speed:: {str(speed)} direction:: {str(direction)}"
            self._textProduct.debug_print(f"\n{'?' * 40}{outStr}", 1)

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
            def filteredCloudList(rawCloudList):

                finalCloudList = []
                previousBases = []
                # Remove any None cloud layers first
                cloudList = [(cov, base) for (cov, base) in rawCloudList if cov and base]
                # Sort by base height
                cloudList = sorted(cloudList, key=itemgetter(1), reverse=True)
                for cov, base in cloudList:
                    if cov == "SKC":
                        continue
                    # Ensure no base is a duplicate.
                    if base is not None and base not in previousBases:
                        finalCloudList.append((cov, base))
                        previousBases.append(base)

                return finalCloudList

            def calcCovDiff(layerHt, ceilingLayerHt, maxCov):
                """
                This method calculates the coverage for the specified layer given
                the ceilingLayer and the maximum coverage. There are a fair number
                or strange rule that govern how to assign coverage to other layers
                given the ceiling. These are isolated here.
                """
                covList = ["OVC", "BKN", "SCT", "FEW"]

                if maxCov not in covList:
                    return None
                # Case where the layer is at above the ceiling, just return the max.
                if layerHt == ceilingLayerHt:
                    return maxCov

                if layerHt < ceilingLayerHt:
                    if maxCov == "OVC":
                        return None
                    return maxCov
                # Otherwise, the layer is below the ceiling and can be assigned
                # a lower coverage than the maxCov.
                maxCovIndex = covList.index(maxCov)
                layerIndex = maxCovIndex - (ceilingLayerHt - layerHt)
                # FEW FEW is allowed
                if maxCov == "FEW":
                    return "FEW"
                if 0 <= layerIndex < len(covList):
                    return covList[layerIndex]
                return None

            def cloudBase(covBase):
                return covBase[1]

            def removeLayersAboveOVC(cloudList):
                sortedCloudList = sorted(cloudList, key=cloudBase)
                finalClouds = []
                for cov, base in sortedCloudList:
                    finalClouds.append((cov, base))
                    if cov == "OVC":
                        return finalClouds
                return finalClouds

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

            # Convert sky data to Coverage strings
            skyCov = self._skyPctToCov(sky)
            sky1Cov = self._skyPctToCov(sky1)
            sky2Cov = self._skyPctToCov(sky2)
            sky3Cov = self._skyPctToCov(sky3)

            if sky:
                rawCloudList = [(skyCov, cloudBase1)]
            else:
                rawCloudList = [(sky1Cov, cloudBase1), (sky2Cov, cloudBase2), (sky3Cov, cloudBase3)]

            cloudList = filteredCloudList(rawCloudList)  # Filter out None bases and SKC layers
            if not cloudList:  # No valid clouds, so report clear skies
                self.productDict[productPart] = "SKC"
                return

            # If all the clouds are valid (cov, base) we can set the product part and return
            if allCloudsValid(cloudList):
                self.productDict[productPart] = cloudList
                return
            # Distribute the cloud coverage over all layers
            # sort highest to the lowest by cloud height
            # cloudList.sort(self._cloudLayerSort)
            # cloudList.reverse()
            cloudList = sorted(cloudList, key=itemgetter(1), reverse=True)

            # Initialize the coverage from Sky which will decrement as we move down in layers
            finalCloudList = []
            # Assign coverage to each layer based on the total Sky coverage.
            # Initialize to the highest layer
            ceilingLayerNum = 0
            # See if there is a real ceiling and adjust ceilingNum
            if ceiling is not None and ceiling > 0:
                # Find the layer that matches the ceiling
                for (i, (cov, base)) in enumerate(cloudList):
                    if base == ceiling:
                        ceilingLayerNum = i
                        break
            # Assign coverage to each layer.
            for (i, (cov, base)) in enumerate(cloudList):
                coverage = cov
                if cov is None:
                    coverage = calcCovDiff(i, ceilingLayerNum, skyCov)
                    # Yet another odd rule: If the ceiling is OVC the layer below must be SCT
                    if skyCov == "OVC" and i != ceilingLayerNum:
                        coverage = "SCT"
                    if coverage is not None:
                        finalCloudList.append((coverage, base))

            # No layers here means a clear sky
            if not finalCloudList:
                self.productDict[productPart] = "SKC"
                return

            # sort the list by base height low to high
            finalCloudList = filteredCloudList(finalCloudList)  # filter one last time
            # Remove any layers above the lowest OVC
            finalCloudList = removeLayersAboveOVC(finalCloudList)
            self.productDict[productPart] = finalCloudList

            return

        def _flightCategory(self, productPart):
            """
            Determine the most severe flight category based off of the ceiling
            and visibility (and possibly wind direction for F categories). This
            is not shown in the TAC output format but can be used to help
            determine the meteorological significance of this period.
            """

            self._textProduct.debug_print(f"\n{'_' * 40}\nDetermining Flight Category:", 1)

            # Ceiling (in units of 100s of feet)
            ceilingHeight = self.productDict["ceiling"]

            # Visibility (in units of statute miles)
            numericalVisibility = self.productDict["visibility"]

            # Wind (direction in degrees, speed in knots)
            wind = self.productDict["wind"]

            self._textProduct.debug_print(f"ceilingHeight = {ceilingHeight}", 1)
            self._textProduct.debug_print(f"numericalVisibility = {numericalVisibility}", 1)
            self._textProduct.debug_print(f"wind = {wind}", 1)
            self._textProduct.debug_print(" ", 1)

            # Assume VFR conditions
            flightCategory = "V"

            if ceilingHeight is not None and numericalVisibility is not None:

                flightCategory = self._findFlightCategory(ceilingHeight, numericalVisibility, wind)

            self._textProduct.debug_print(f"\nFlight category = {flightCategory}", 1)
            self.productDict[productPart] = flightCategory

        def _conditionalGroup(self, productPart):
            conditionalGroup = self._textProduct.ConditionalGroup(
                self._textProduct,
                self._statList,
                self._sharedDict,
                self._airportDict,
                self.hourOffset,
            )

            problemsFound = conditionalGroup.createProductParts(self)
            if problemsFound:
                self.productDict[productPart] = None
            else:
                self.productDict[productPart] = conditionalGroup

        def _ratings(self, productPart):
            self.rateFmGroup()

        # --- ____________________
        # --- HELPER METHODS
        def rateFmGroup(self):
            # Rate the product parts of the FM groups created
            airportIcaoId = self._airportDict["icaoAirportIndicator"]

            ratingRules = self._textProduct.SignificanceRatingRules(self._textProduct, self)

            ratings = OrderedDict()

            for productPart in self.productParts():
                if productPart in [
                    "groupClassification",
                    "groupType",
                    "groupLabel",
                    "fmPeriod",
                    "startTime",
                    "endTime",
                    "conditionalGroup",
                    "ratings",
                ]:
                    continue

                if productPart not in self.productDict:
                    self.productDict[productPart] = None
                    continue
                if self.productDict[productPart] is not None:
                    method = getattr(ratingRules, f"_{productPart}Significance")
                    if method:
                        ratings[productPart] = method(self.productDict[productPart])

                    self._textProduct.debug_print(
                        f"{productPart} ({self.productDict[productPart]!r}) "
                        f"rating = {ratings[productPart]}",
                        1,
                    )

            self.productDict["ratings"] = ratings

            if self.productDict["conditionalGroup"] is not None:
                self._textProduct.debug_print(
                    f"\n\tAverage FM group rating = {ratingRules.averageRating(ratings)}",
                    1,
                )

                conditionalGroup = self.productDict["conditionalGroup"]
                conditionalRatings = conditionalGroup.productDict["ratings"]

                self._textProduct.debug_print(
                    f"\nTotal average ({self.productDict['groupLabel']} and "
                    f"{conditionalGroup.productDict['groupLabel']}) "
                    f"rating = {ratingRules.averageRating(ratings, conditionalRatings)}",
                    1,
                )
            else:
                self._textProduct.debug_print(
                    f"\nTotal average FM group rating = {ratingRules.averageRating(ratings)}", 1
                )

        def mergeConditionalGroups(self, otherFmGroup):
            """
            Try to merge this FM group's conditional group with the other FM
            group's conditional group if they exist and can be merged.
            """
            if self.productDict["conditionalGroup"].canMergeWith(
                otherFmGroup.productDict["conditionalGroup"]
            ):
                # Merge the other conditional group into this conditional group
                self.productDict["conditionalGroup"].mergeGroup(
                    otherFmGroup.productDict["conditionalGroup"]
                )

                # Remove the old TEMPO/PROB
                otherFmGroup.removeConditionalGroup()

                return True
            else:
                return False

        def takeConditionalGroup(self, otherFmGroup):
            self.productDict["conditionalGroup"] = otherFmGroup.productDict["conditionalGroup"]

            otherFmGroup.productDict["conditionalGroup"] = None

        def removeConditionalGroup(self):
            conditionalGroup = self.productDict["conditionalGroup"]

            if conditionalGroup is not None:
                self._textProduct.debug_print(
                    f"\tRemoving {conditionalGroup.productDict['groupLabel']} from "
                    f"{self.productDict['groupLabel']} (Period {self.period})",
                    1,
                )

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
                # self._textProduct.debug_print(
                #     f"processing {category}:\tvisibility: {testVisibility}\t\tceiling: {testCeiling}",
                #     1,
                # )

                if category == "V":
                    # If we've gotten to the VFR category, we are done since it's always last.
                    return "V"
                else:
                    self._textProduct.debug_print(f"comparing against category {category}", 1)

                    if category == "D":
                        thresholdsCheck = (
                            ceilingHeight <= testCeiling or numericalVisibility <= testVisibility
                        )
                    else:
                        thresholdsCheck = (
                            ceilingHeight < testCeiling or numericalVisibility < testVisibility
                        )

                    if thresholdsCheck:
                        self._textProduct.debug_print(
                            "Met ceiling and/or visibility thresholds!", 1
                        )

                        if "F" in category:
                            (windDirRangeStart, windDirRangeEnd) = categoryInfo[3]
                            self._textProduct.debug_print(
                                f"\tF category: ensure wind direction within range "
                                f"({windDirRangeStart}, {windDirRangeEnd})",
                                1,
                            )

                            if self._isDirectionInRange(wind, windDirRangeStart, windDirRangeEnd):

                                self._textProduct.debug_print("\t\tMet wind threshold!", 1)
                                return category
                        else:
                            return category

        def _isDirectionInRange(self, wind, dirRangeStart, dirRangeEnd):
            """
            Checks if a particular wind direction is within the specified range.
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
        def __init__(self, textProduct, statList, sharedDict, airportDict, hourOffset):
            TextProduct.GenericGroup.__init__(self, textProduct, statList, sharedDict, airportDict)

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
            self._textProduct.debug_print(
                f"\n{'_' * 80}\nMaking TEMPO/PROB30 group for {fmGroup.productDict['groupLabel']} "
                f"({self._airportDict['icaoAirportIndicator']} Period {fmGroup.period})",
                1,
            )

            problemsFound = False
            for productPart in self.productParts():
                print(f"self._{productPart}(productPart, fmGroup)")
                try:
                    method = getattr(self, f"_{productPart}")
                    if method:
                        method(productPart, fmGroup)

                except Exception as e:
                    self._textProduct.debug_print(f"\n{traceback.format_exc().strip()}", 1)

                    if isinstance(e, TAF_DataException):
                        # The issue is with data that isn't required so ignore
                        # it and just set the product part to None
                        self.productDict[productPart] = None
                        self._textProduct.debug_print(
                            f"Ignoring issue: {productPart} is not required", 1
                        )

            if not self._isValidGroup():
                self._textProduct.debug_print("\nCould not create a conditional group.", 1)
                problemsFound = True
            else:
                self._textProduct.debug_print(
                    f"\nCreated a {self.productDict['groupType']} group!", 1
                )

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
            startTimeSeconds = self._textProduct._allPeriodStartEndTimes[self.hourOffset][0]
            self.productDict[productPart] = startTimeSeconds

        def _endTime(self, productPart, fmGroup):
            endTimeSeconds = self._textProduct._allPeriodStartEndTimes[self.hourOffset][1]
            self.productDict[productPart] = endTimeSeconds

        def _wind(self, productPart, fmGroup):
            # TODO: Wind information currently not supported by the formatter
            self.productDict[productPart] = None

        def _windGust(self, productPart, fmGroup):
            # TODO: WindGust information currently not supported by the formatter
            self.productDict[productPart] = None

        def _visibilityConditional(self, productPart, fmGroup):
            """Get the conditional numerical visibility."""

            visibility = self._getData("VisibilityConditional", float, nullValue=0)
            visibility = self._roundVisibilityToReportableValue(visibility)

            self.productDict[productPart] = visibility

        def _weather(self, productPart, fmGroup):
            self.productDict[productPart] = self._determineWeather(
                self.productDict["groupType"], self.productDict["visibilityConditional"]
            )

        def _visibility(self, productPart, fmGroup):
            """Compute the visibility for this conditional group."""

            # FM group visibility (in statute miles)
            numericalVisibility = fmGroup.productDict["visibility"]
            # Conditional group visibility (in statute miles)
            numericalConditionalVisibility = self.productDict["visibilityConditional"]
            visibilityConditionalExists = numericalConditionalVisibility is not None

            # Assume there won't be conditional visibility
            self.productDict[productPart] = None

            # If this conditional group has weather
            if self.productDict["weather"].weatherExists():

                if visibilityConditionalExists:
                    # Only allow conditional visibility that is different from
                    # the FM group visibility.
                    if numericalConditionalVisibility != numericalVisibility:
                        self.productDict[productPart] = numericalConditionalVisibility
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

            # If the conditional group doesn't have weather, but we have a valid
            # VisibilityConditional value and this is a TEMPO group (PROB30
            # groups are only created when there is weather for them)
            elif visibilityConditionalExists and self.productDict["groupType"] == "TEMPO":

                # Conditional visibility must be worse than the FM group
                # visibility
                if numericalConditionalVisibility < numericalVisibility:
                    self.productDict[productPart] = numericalConditionalVisibility

            # Now that the final visibility is computed, finish the weather
            self._finalizeWeather("weather", fmGroup.productDict["weather"])

        def _cloudBaseConditional(self, productPart, fmGroup):
            # Conditional (TEMPO or PROB30) predominant cloud height
            # (in 100s feet)
            cloudBaseConditional = self._getData("CloudBaseConditional", int, nullValue=0)
            self.productDict[productPart] = self._roundCloudHeight(cloudBaseConditional)

        # Conditional clouds
        def _clouds(self, productPart, fmGroup):
            cloudBaseConditional = self.productDict["cloudBaseConditional"]
            cloudBaseConditionalExists = cloudBaseConditional is not None

            # The lowest cloud base in the FM group
            lowestFMCloudBase = self._getLowestFMGroupCloudBase(fmGroup)

            # Calculate a ceiling coverage based off of the sky cover of the
            # lowest cloud base in the FM group
            conditionalSkyCover = self._calculateConditionalSkyCover(lowestFMCloudBase)

            # The height of the conditional cloud base that we will be
            # computing. Start off assuming we won't report anything by
            # initializing to None.
            conditionalCloudHeight = None

            # If this conditional group has weather
            if self.productDict["weather"] is not None:

                if not cloudBaseConditionalExists:
                    # Calculate a ceiling height based off of the lowest cloud
                    # base in the FM group.
                    conditionalCloudHeight = self._calculateConditionalCloudHeight(
                        lowestFMCloudBase, fmGroup
                    )
                else:  # We have CloudBaseConditional available
                    conditionalCloudHeight = cloudBaseConditional

            # If the conditional group doesn't have weather, but we have a valid
            # CloudBaseConditional value and this is a TEMPO group (PROB30
            # groups are only created when there is weather for them)
            elif cloudBaseConditionalExists and self.productDict["groupType"] == "TEMPO":

                # Only report a cloud base if the conditional height is <= the
                # lowest FM group cloud base height.
                if lowestFMCloudBase == "SKC" or cloudBaseConditional <= lowestFMCloudBase[1]:

                    conditionalCloudHeight = cloudBaseConditional

            # Make sure we have both parts of the cloud base
            if conditionalSkyCover is not None and conditionalCloudHeight is not None:

                # Make sure we don't duplicate what is in the FM group
                conditionalCloudBase = (conditionalSkyCover, conditionalCloudHeight)
                fmClouds = fmGroup.productDict["clouds"]

                if isinstance(fmClouds, list) and conditionalCloudBase in fmClouds:

                    self.productDict[productPart] = None
                else:
                    self.productDict[productPart] = [conditionalCloudBase]
            else:
                # We couldn't create a cloud base, but we have thunderstorms in
                # this conditional group, so try using the associated FM
                # group's first cloud base as this cloud base
                if (
                    self.productDict["weather"] is not None
                    and self.productDict["weather"].thunderstormsPresent()
                    and isinstance(fmGroup.productDict["clouds"], list)
                    and fmGroup.productDict["clouds"]
                ):
                    # Copy the prevailing clouds into the conditional clouds
                    self.productDict[productPart] = [fmGroup.productDict["clouds"][0]]
                else:
                    self.productDict[productPart] = None

        def _ratings(self, productPart, fmGroup):
            self.rateConditionalGroup()

        # --- ____________________
        # --- HELPER METHODS

        def rateConditionalGroup(self):
            # Rate the product parts of this conditional group
            if not self._isValidGroup():
                self.productDict["ratings"] = OrderedDict()
                return

            ratingRules = self._textProduct.SignificanceRatingRules(self._textProduct, self)

            self._textProduct.debug_print(
                f"\n{'-' * 80}\nRating {self.productDict['groupLabel']}:", 1
            )

            self.productDict["ratings"] = OrderedDict()

            for (productPart, ruleMethod) in [
                ("visibility", ratingRules._visibilitySignificance),
                ("weather", ratingRules._weatherSignificance),
                ("clouds", ratingRules._cloudsSignificance),
            ]:

                if self.productDict[productPart] is not None:
                    self.productDict["ratings"][productPart] = ruleMethod(
                        self.productDict[productPart]
                    )

                    self._textProduct.debug_print(
                        f"{productPart} ({self.productDict[productPart]}) "
                        f"rating = {self.productDict['ratings'][productPart]}",
                        1,
                    )

            self._textProduct.debug_print(
                f"\n\tAverage {self.productDict['groupLabel']} "
                f"rating = {ratingRules.averageRating(self.productDict['ratings'])}",
                1,
            )

        def canMergeWith(self, otherGroup):
            groupType = self.productDict["groupType"]
            otherGroupType = otherGroup.productDict["groupType"]
            isSameGroupType = groupType == otherGroupType

            groupEnd = self.productDict["endTime"]
            otherGroupStart = otherGroup.productDict["startTime"]
            isConsecutive = groupEnd == otherGroupStart

            groupStart = self.productDict["startTime"]
            currentDuration = time.gmtime(groupEnd - groupStart).tm_hour
            # TEMPOs can be at most 4 hours in length, PROB30s can be at
            # most 6 hours
            isValidLength = (groupType == "TEMPO" and currentDuration < 4) or (
                groupType == "PROB30" and currentDuration < 6
            )

            if isSameGroupType and isConsecutive and isValidLength:
                return True
            else:
                return False

        def mergeGroup(self, otherGroup):
            """
            Merge the passed in conditional group.
            """
            self._textProduct.debug_print(
                f"\n\tMerging {self.productDict['groupLabel']} with "
                f"{otherGroup.productDict['groupLabel']}",
                1,
            )

            # Update the times (start time stays the same) and update the label
            self.productDict["endTime"] = otherGroup.productDict["endTime"]
            self._makeGroupLabel()

            self._mergeVisibility(otherGroup.productDict["visibility"])

            self._mergeWeather(otherGroup.productDict["weather"])

            self._mergeClouds(otherGroup.productDict["clouds"])

            # Rate the product parts of the merged conditional group
            self.rateConditionalGroup()

        def _isValidGroup(self):
            return (
                self.productDict["visibility"] is not None
                or self.productDict["weather"] is not None
                or self.productDict["clouds"] is not None
            )

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

            # And commented out above and replaced with this code
            label = (
                f"{self.productDict['groupType']} "
                f"{startTimeUTC.tm_mday:02d}{startTimeUTC.tm_hour:02d}/"
                f"{endDay:02d}{endHour:02d}"
            )
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
            if (lowestFMCloudBase == "SKC") or (lowestFMCloudBase[0] in ["FEW", "SCT"]):
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
                # Get list of ordered categories (First entry of categoryInfo is the category)
                orderedCategories = [
                    categoryInfo[0] for categoryInfo in self._textProduct._orderedCategoryInfo
                ]

                # Get the index of the current FM group flight category
                flightCategory = fmGroup.productDict["flightCategory"]
                categoryIndex = orderedCategories.index(flightCategory)

                # Go down one flight category and choose a height in the middle
                # of that worse category
                if categoryIndex >= 1:
                    oneLowerCategoryInfo = self._textProduct._orderedCategoryInfo[categoryIndex - 1]
                    # Second entry of category info is ceiling threshold
                    oneLowerCeiling = oneLowerCategoryInfo[1]

                    if categoryIndex == 1:
                        twoLowerCeiling = 0
                    else:
                        twoLowerCategoryInfo = self._textProduct._orderedCategoryInfo[
                            categoryIndex - 2
                        ]
                        twoLowerCeiling = twoLowerCategoryInfo[1]

                    # Calculate the height
                    return int(round((oneLowerCeiling + twoLowerCeiling) / 2.0))
                else:
                    # Already at the worst flight category
                    lowestCategoryInfo = self._textProduct._orderedCategoryInfo[0]
                    if lowestCategoryInfo[1] > 0:  # If the ceiling isn't 0
                        # Calculate the height
                        return int(round(lowestCategoryInfo[1] / 2.0))
                    else:
                        return None  # Don't report a cloud base

        def _mergeVisibility(self, otherVisibility):
            # Merge the visibility information
            if self.productDict["visibility"] is None or (
                otherVisibility is not None and otherVisibility < self.productDict["visibility"]
            ):

                self.productDict["visibility"] = otherVisibility

        def _mergeWeather(self, otherWeather):
            # Merge the weather information
            if self.productDict["weather"] is None:

                self.productDict["weather"] = otherWeather

            elif otherWeather is not None:

                self.productDict["weather"].mergeWeather(
                    otherWeather, self.productDict["visibility"]
                )

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

            if clouds is None or (clouds == "SKC" and isinstance(otherClouds, list)):

                self.productDict["clouds"] = otherClouds

            elif isinstance(clouds, list) and isinstance(otherClouds, list):

                allClouds = clouds + otherClouds
                self.productDict["clouds"] = []

                # Sort the cloud bases according to height
                sortedBases = sorted(allClouds, key=itemgetter(1))

                # Filter out any cloud bases above the lowest OVC layer
                sortedBases = self.filterClouds(sortedBases)

                skyCoverOrder = [None, "FEW", "SCT", "BKN", "OVC"]

                for (skyCover, height) in allClouds:
                    if not self.productDict["clouds"]:
                        self._addCloudBase(skyCover, height)

                    else:
                        (prevSkyCover, prevHeight) = self.productDict["clouds"][-1]
                        prevCoverIndex = skyCoverOrder.index(prevSkyCover)
                        currCoverIndex = skyCoverOrder.index(skyCover)
                        # if heights are the same, pick the layer with the most clouds
                        if height == prevHeight and currCoverIndex > prevCoverIndex:
                            self.productDict["clouds"].pop()
                            self._addCloudBase(skyCover, height)

                        elif height > prevHeight and currCoverIndex >= prevCoverIndex:
                            self._addCloudBase(skyCover, height)

            return

        def _addCloudBase(self, skyCover, height):
            numOvercastClouds = reduce(
                lambda count, base: (count + 1) if base[0] == "OVC" else count,
                self.productDict["clouds"],
                0,
            )

            # Don't allow more than one overcast cloud base (when looking
            # up from the ground, only the first overcast cloud base is
            # visible)
            # The directives only allow at most 3 cloud bases
            if numOvercastClouds == 0 and len(self.productDict["clouds"]) < 3:
                self.productDict["clouds"].append((skyCover, height))

            return

    # --- HELPER METHODS

    def _displayShortenedTafInfo(self, fmGroups):
        self.debug_print(f"\n{'*' * 80}", 1)
        self.debug_print("Shortened TAF:", 1)
        self.debug_print("----------------", 1)

        self.debug_print(f"Number of periods = {len(fmGroups)}", 1)
        for fmGroup in fmGroups:
            self.debug_print(f"\n{fmGroup}:", 1)

            fmGroupDict = fmGroup.productDict
            for (productPart, value) in fmGroupDict.items():
                self.debug_print(f"{productPart} = {value}", 1)

            if fmGroupDict["conditionalGroup"] is not None:
                conditionalGroup = fmGroupDict["conditionalGroup"]

                self.debug_print(f"\n\t\t{conditionalGroup}:", 1)

                conditionalGroupDict = conditionalGroup.productDict
                for (productPart, value) in conditionalGroupDict.items():
                    self.debug_print(f"\t{productPart} = {value}", 1)

        self.debug_print(f"\n{'*' * 80}", 1)

    def _finalizeTemposProbs(self, shortenedFmGroups, allFmGroups, airportIcaoId):
        self.debug_print("\nFinalizing conditional groups", 1)

        # Determine which FM groups were removed
        removedFmGroups = allFmGroups[:]
        self.debug_print("Removed FM groups:", 1)
        for fmGroup in shortenedFmGroups:
            # Take away all the kept FM groups to determine which were removed
            removedFmGroups.remove(fmGroup)
            self.debug_print(f"\t{fmGroup}", 1)

        # Get the list of consecutive removed periods
        consecutiveRemovedPeriodsList = self._getConsecutiveRemovedPeriodsList(
            removedFmGroups, allFmGroups
        )

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
            if self._allowMultiHourTempoProbs:
                self._mergeTemposProbs(consecutiveRemovedFmGroups)

            # For each list of consecutive FM groups, find the most significant
            # TEMPO/PROB30 and attach it to the first FM group (which was kept)
            # in the list and remove the TEMPO/PROB30 groups from all other
            # FM groups in the list (which were removed by shortening).
            self._keepMostSignificantTempoProb(consecutiveRemovedFmGroups, attachToFirstGroup=True)

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
                index += 1

            else:
                currentFmGroup = fmGroupsWithConditionals[index]

                # Try merging the conditional groups associated with these
                # FM groups
                mergedSuccessfully = workingFmGroup.mergeConditionalGroups(currentFmGroup)

                if mergedSuccessfully:
                    # Go to the next group
                    index += 1
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

            if not consecutivePeriods:
                # This is a new list of consecutive periods.
                # Add the previous (kept) period and the removed period.
                consecutivePeriods.append(previousFmGroup.period)
                consecutivePeriods.append(removedFmGroup.period)
                index += 1

                self.debug_print("\tStarted new list:", 1)
                self.debug_print(
                    f"\t\tAdded Period {previousFmGroup.period}"
                    f"\n\t\t\tAdded Period {removedFmGroup.period}",
                    1,
                )

            elif removedFmGroup.period - consecutivePeriods[-1] == 1:
                # This is the next consecutive period in this list
                consecutivePeriods.append(removedFmGroup.period)
                index += 1

                self.debug_print(f"\t\tAdded Period {removedFmGroup.period}", 1)

            else:
                # This is not consecutive, start a new list
                consecutivePeriodsList.append(list())

        self.debug_print("Finished building consecutive lists", 1)
        return consecutivePeriodsList

    def _keepMostSignificantTempoProb(self, fmGroups, groupType=None, attachToFirstGroup=False):
        self.debug_print("\tKeeping most significant conditional group", 1)

        # Find all FM groups that have conditional groups associated with them
        fmGroupsWithConditionals = self._findFmGroupsWithTemposProbs(fmGroups, groupType)

        if not fmGroupsWithConditionals:
            self.debug_print("\tNo conditional groups found", 1)
            return

        # Find the most significant TEMPO/PROB30 and remove all the rest
        fmGroupWithMostSignificantTempoProb = self._findMostSignificantTempoProb(
            fmGroupsWithConditionals
        )
        mostSignificantConditional = fmGroupWithMostSignificantTempoProb.productDict[
            "conditionalGroup"
        ]

        for fmGroup in fmGroupsWithConditionals:
            if fmGroup.period != fmGroupWithMostSignificantTempoProb.period:
                # This FM group's TEMPO/PROB30 is not the most significant,
                # remove it
                fmGroup.removeConditionalGroup()

        self.debug_print(
            f"\tKept '{mostSignificantConditional}' from '{fmGroupWithMostSignificantTempoProb}'",
            1,
        )

        # If the most significant TEMPO/PROB needs to be attached to the first
        # FM group in the list, then attach it if necessary
        if attachToFirstGroup and fmGroupWithMostSignificantTempoProb.period != fmGroups[0].period:

            self.debug_print(
                f"\t\tAttaching '{mostSignificantConditional}' to '{fmGroups[0]}'",
                1,
            )

            fmGroups[0].takeConditionalGroup(fmGroupWithMostSignificantTempoProb)

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

                if groupType is None or conditionalGroup.productDict["groupType"] == groupType:
                    fmGroupsWithConditionals.append(fmGroup)

        return fmGroupsWithConditionals

    def _findMostSignificantTempoProb(self, fmGroupsWithTempoProb):
        self.debug_print("\tLooking for most significant conditional group", 1)

        mostSignificantRating = None
        fmGroupWithMostSignificantTempoProb = None

        for fmGroup in fmGroupsWithTempoProb:
            conditionalGroup = fmGroup.productDict["conditionalGroup"]
            conditionalRatings = conditionalGroup.productDict["ratings"]

            averageConditionalRating = self.SignificanceRatingRules.averageRating(
                conditionalRatings
            )

            if mostSignificantRating is None or averageConditionalRating > mostSignificantRating:

                mostSignificantRating = averageConditionalRating
                fmGroupWithMostSignificantTempoProb = fmGroup

        if mostSignificantRating is not None:
            fmGroup = fmGroupWithMostSignificantTempoProb
            self.debug_print(
                f"\t{fmGroup} has most significant conditional:"
                f"\n\t\t\t{fmGroup.productDict['conditionalGroup']}",
                1,
            )

        return fmGroupWithMostSignificantTempoProb

    def _removeTempoProb(self, fmGroupDict):
        self.debug_print(
            f"\tRemoving {fmGroupDict['conditionalGroup'].productDict['groupLabel']} "
            f"from {fmGroupDict['groupLabel']} (Period {fmGroupDict['fmPeriod']})",
            1,
        )

        del fmGroupDict["conditionalGroup"]
        fmGroupDict["conditionalGroup"] = None

    # --------------------------------------------------------------------------

    # Holds information extracted from a weather ugly string
    # TODO: add __str__ and __repr__ to debug nicely
    class WeatherUglyStringInfo:
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
            print(f"WxUglyStrInfo-classifier:{str(self.classifier)}:")
            # Use the visibility from the grids, not the weather ugly string
            self._modifyWeatherIfNeeded(groupType, numericalVisibility)

            self._uglyStr = weatherUglyString

        def __copy__(self):
            info = type(self)(self.__class__)
            info.__dict__.update(self.__dict__)

            return info

        def _setWeatherParts(self, weatherUglyString, visibility):
            """Extract the information contained in the weather ugly string."""
            weatherParts = weatherUglyString.split(":")

            self.probability = weatherParts[0]  # probability/coverage

            self.gfeWeatherType = weatherParts[1]  # wx type

            self.tafWeatherType = self._convertGfeCodeToTafCode(self.gfeWeatherType, visibility)

            self.intensity = weatherParts[2]  # wx intensity

            self.visibility = weatherParts[3]  # visibility

            self.gfeAttributes = weatherParts[4]  # extra attributes
            self.tafAttributes = []
            attributeList = self.gfeAttributes.split(",")
            for gfeAttribute in attributeList:
                # Try converting it if possible, otherwise don't include it
                tafAttribute = self._convertGfeCodeToTafCode(gfeAttribute, visibility)
                # TODO: Are any other attributes convertible to a TAF code?
                if tafAttribute:
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

                # Modify thunderstorms when attributes are present
                if self.tafWeatherType == "TS":

                    self._textProduct.debug_print(
                        f"Adding {self.tafAttributes} attributes to TS", 1
                    )

                    # Add any attributes which may be present
                    for tafAttribute in self.tafAttributes:
                        self.tafWeatherType += tafAttribute

                # Modify fog codes based on visibility conditions
                elif (
                    self.tafWeatherType == "BR"
                    and numericalVisibility is not None
                    and numericalVisibility < 0.625
                ):

                    self._textProduct.debug_print(
                        "Visibility < 5/8SM: using stronger fog code FG", 1
                    )

                    # Fog with < 5/8SM visibility uses stronger fog code FG
                    self.tafWeatherType = "FG"

                elif (
                    self.tafWeatherType == "FZFG"
                    and numericalVisibility is not None
                    and 0.625 <= numericalVisibility < 6.0
                ):

                    self._textProduct.debug_print(
                        "Visibility >= 5/8SM: using weaker fog code BR", 1
                    )

                    # Freezing Fog is only allowed when visibility is < 5/8SM,
                    # other visibilities use the weaker BR fog code instead
                    self.tafWeatherType = "BR"

            # Handle vicinity weather (if we are going to report it). Only FM
            # groups can have vicinity weather.
            elif "VC" in self.classifier and self._textProduct._reportVC and groupType == "FM":

                self._textProduct.debug_print(
                    f"Reporting {self.tafWeatherType} as {self.classifier} instead",
                    1,
                )

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
                typeList = types.split(",")
                typeList = list(map(str.strip, typeList))

                if self.gfeWeatherType in typeList:
                    typeRules = self._textProduct._weatherRules[types]
                    break

            if typeRules is None:
                raise TAF_DataException(
                    f"Can't find rules to classify GFE type {self.gfeWeatherType} "
                    f"(TAF type {self.tafWeatherType})."
                )

            return typeRules

        def _findProbabilityRules(self, typeRules):
            """Try finding rules for this weather type at this hour offset."""

            # Use this period's index (aka. hours since issuance time) to get
            # the dictionary of probability/coverage rules for this weather type.
            try:
                probabilityRules = self._textProduct.nlValue(
                    typeRules, self._textProduct._periodIndex
                )
            except:
                message = (
                    f"Can't find rules to classify GFE type {self.gfeWeatherType} "
                    f"(TAF type {self.tafWeatherType}) at hour offset "
                    f"{self._textProduct._periodIndex}."
                )
                raise TAF_DataException(message)

            return probabilityRules

        def _findClassifier(self, probabilityRules):
            """
            Use the probability/coverage (and possibly intensity) to determine
            how to display this weather in the output (the classifier).
            """
            classifier = None

            for probabilities in probabilityRules:
                if probabilities.lower().strip() == "default":
                    # We'll deal with default at the end if we haven't found a
                    # better matching probability
                    continue

                # Multiple probabilities/coverages can share the same rule
                probabilityList = probabilities.split(",")
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
            print(f"Found Classifier:{str(classifier)}:")
            # We haven't found a classifier yet, try using the default rule
            if classifier is None:
                try:
                    classifier = probabilityRules["default"]
                except:
                    message = (
                        f"No default classifier rules for GFE type {self.gfeWeatherType} "
                        f"(TAF type {self.tafWeatherType}) at hour offset "
                        f"{self._textProduct._periodIndex}."
                    )
                    raise TAF_DataException(message)

            # It was determined to not show this weather, so throw this
            # exception and skip it.
            if not classifier:
                raise TAF_DataException(
                    f"GFE type {self.gfeWeatherType} (TAF type {self.tafWeatherType}) "
                    f"classified as 'do not show'."
                )
            return classifier

        def _convertGfeCodeToTafCode(self, gfeCode, visibility):
            """Convert a GFE code to a TAF code."""

            # Try converting it if possible, otherwise don't report it
            tafCode = self._textProduct._gfeCodeToTafCodeMap.get(gfeCode, "")

            if tafCode == "BR" and visibility is not None and visibility >= 6.0:
                tafCode = ""

            return tafCode

    class GroupWeatherInfo:
        # Define a list of TAF weather type codes for precipitation weather

        # The set and order in which descriptor  types should appear
        descriptorTypes = [
            "TS",
            "TSRA",
            "TSSN",
            "FZRA",
            "FZDZ",
            "SHSN",
            "SHRA",
        ]

        # The set and order in which precipitation types should appear
        precipitationWeatherTypes = [
            "TSRA",
            "TSSN",
            "TS",
            "RA",
            "SN",
            "GR",
            "GS",
            "PL",
            "IC",
            "DZ",
        ]
        vicinityWxTypes = [
            "VCTS",
            "VCSH",
        ]
        # The set and order in which obstruction types should appear
        obstructionTypes = [
            "FZFG",
            "BLSN",
            "BLDU",
            "BLSA",
            "BR",
            "FG",
            "HZ",
            "FU",
            "SS",
            "DS",
            "VA",
            "SA",
            "DU",
            "PY",
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
                text += f" {type.probability}"

            text += f" Int: {self.maxIntensity}"

            text += " Type:"
            for type in self.regularWeatherTypes:
                text += f" {type}"

            text += " obscur:"
            for type in self.obstructionWeatherTypes:
                text += f" {type}"

            text += f", ViWx: {self.vicinityWeather}"

            return text

        def __eq__(self, other):
            if other is None:
                return False
            else:
                return (
                    self.maxIntensity == other.maxIntensity
                    and (sorted(self.regularWeatherTypes) == sorted(other.regularWeatherTypes))
                    and (
                        sorted(self.obstructionWeatherTypes)
                        == sorted(other.obstructionWeatherTypes)
                    )
                    and self.vicinityWeather == other.vicinityWeather
                )

        @property
        def precipSort(self):
            def cmpfunc(a, b):
                if (
                    a not in self.precipitationWeatherTypes
                    or b not in self.precipitationWeatherTypes
                ):
                    return 0
                aIndex = self.precipitationWeatherTypes.index(a)
                bIndex = self.precipitationWeatherTypes.index(b)

                if aIndex < bIndex:
                    return -1
                if bIndex < aIndex:
                    return 1
                return 0

            return cmp_to_key(cmpfunc)

        def precipSortKey(self, wx):
            # if not part or precip types, make it last.
            if wx not in self.precipitationWeatherTypes:
                return 999
            return self.precipitationWeatherTypes.index(wx)

        # A simple method to rank weather types in the order that they appear in the TAF
        def weatherTypeRanking(self, wx):

            if "TS" in wx:  # Thunder rules over all over types
                return 0
            for descWx in self.descriptorTypes:
                if descWx in wx:
                    return 1
            for precipWx in self.precipitationWeatherTypes:
                if precipWx in wx:
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

            return cmp_to_key(cmpfunc)

        def weatherSortKey(self, a):

            # First sort by ranking - See above
            rankA = self.weatherTypeRanking(a)

            # Next sort by max intensity
            intenA = 1
            if a == self.tafIntensityWx:
                intenA = 0

            return f"{rankA}_{intenA}"

        def hasPrecipWeather(self, weatherList):
            for wx in weatherList:
                if wx in self.precipitationWeatherTypes:
                    return True
            return False

        def updateWeather(self, weatherUglyStringInfo):
            uglyStringTafWeatherType = weatherUglyStringInfo.tafWeatherType
            uglyStringClassifier = weatherUglyStringInfo.classifier

            if uglyStringTafWeatherType == "FR":
                return

            if "VC" in uglyStringClassifier:
                if self.hasPrecipWeather(self.regularWeatherTypes):
                    return  # Don't add VCSH to precip weather

                # Only FM groups can have vicinity weather
                if self.groupType == "FM":
                    self.updateVicinityWeather(uglyStringTafWeatherType)

            # Update maximum intensity if this is precipitation weather
            # Check for VCSH first and replace it if we're inserting VCTS
            # if uglyStringTafWeatherType in [self.precipitationWeatherTypes]:  # was
            intensityTypes = self.precipitationWeatherTypes + self.descriptorTypes
            if uglyStringTafWeatherType in intensityTypes:
                self.updateMaximumIntensity(
                    weatherUglyStringInfo.intensity, uglyStringTafWeatherType
                )

                if "VCSH" in self.regularWeatherTypes and uglyStringTafWeatherType == "VCTS":
                    self.regularWeatherTypes.remove("VCSH")  # VCTS replaces VCSH
                self.regularWeatherTypes.append(uglyStringTafWeatherType)

            elif uglyStringTafWeatherType in self.obstructionTypes:
                self.obstructionWeatherTypes.append(uglyStringTafWeatherType)

            # Except for volcanic ash, all obstruction types must have reduced visibility
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

            self.obstructionWeatherTypes += otherWeather.obstructionWeatherTypes
            self.obstructionWeatherTypes = list(set(self.obstructionWeatherTypes))

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
                if (
                    self.numericalVisibility is not None
                    and self.numericalVisibility < minP6smVisibility
                    and fmWeatherExists
                ):

                    self.maxIntensity = fmWeather.maxIntensity
                    self.regularWeatherTypes = copy.copy(fmWeather.regularWeatherTypes)
                    self.obstructionWeatherTypes = copy.copy(fmWeather.obstructionWeatherTypes)
                    self.vicinityWeather = fmWeather.vicinityWeather
                    self.uglyStringInfos = copy.copy(fmWeather.uglyStringInfos)

                # If we don't already have weather, any weather added here
                # would consist of only repeated FM weather
                return

            # If there is any reportable weather in the FM group
            if fmWeatherExists:
                # Check if there is any FM weather we need to repeat in this
                # group
                fmWeatherTypesToRepeat = self._textProduct._fmWeatherTypesToRepeat

                for tafWeatherType in fmWeatherTypesToRepeat:
                    if tafWeatherType in fmWeather.regularWeatherTypes:
                        self.regularWeatherTypes.append(tafWeatherType)

                    elif tafWeatherType in fmWeather.obstructionWeatherTypes:
                        self.obstructionWeatherTypes.append(tafWeatherType)

        def postProcessWeatherTypes(self):
            # Remove all duplicate weather while preserving order
            self.regularWeatherTypes = self._removeDuplicateWeather(self.regularWeatherTypes)
            self.obstructionWeatherTypes = self._removeDuplicateWeather(
                self.obstructionWeatherTypes
            )

            # Apply various logic rules to ensure the main weather is displayed
            # properly when the weather text is constructed
            self.regularWeatherTypes = self._consolidateRegularWeather(self.regularWeatherTypes)
            self.regularWeatherTypes = self._orderRegularWeather(self.regularWeatherTypes)

            # Apply various logic rules to ensure the obstruction weather is
            # displayed properly when the weather text is constructed
            self.obstructionWeatherTypes = self._consolidateObstructionWeather(
                self.obstructionWeatherTypes
            )

        def weatherExists(self):

            allWx = self.getAllWeather()
            if allWx:
                if allWx[0]:
                    self._textProduct.debug_print("** allWx = {} - returning True", 1)
                    return True
            self._textProduct.debug_print("** allWx = {} - returning False", 1)
            return False

        def getAllWeather(self):
            return (
                self.regularWeatherTypes
                + self.obstructionWeatherTypes
                + ([] if self.vicinityWeather is None else [self.vicinityWeather])
            )

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
            if (
                self.tafIntensityWx in self.descriptorTypes
                and tafWxType not in self.descriptorTypes
            ):
                return

            # Automatic update if the new one is a descriptor type and the current one is not
            elif (
                self.tafIntensityWx not in self.descriptorTypes
                and tafWxType in self.descriptorTypes
            ):
                self.tafIntensityWx = tafWxType
                self.maxIntensity = newIntensity
                return

            # At this point either both are or both are not descriptor types, so check
            # severity (intensity).
            currentSeverityLevel = 0
            newSeverityLevel = 0
            if self.maxIntensity in self.intensitySeverity:
                currentSeverityLevel = self.intensitySeverity.index(self.maxIntensity)
            if newIntensity in self.intensitySeverity:
                newSeverityLevel = self.intensitySeverity.index(newIntensity)

            # If the intensities are the same, update the tafIntensityWx if the weather type
            # ranks higher in the list.

            if newSeverityLevel == currentSeverityLevel:
                if (
                    tafWxType in self.precipitationWeatherTypes
                    and self.tafIntensityWx in self.precipitationWeatherTypes
                ):
                    if self.precipitationWeatherTypes.index(
                        tafWxType
                    ) < self.precipitationWeatherTypes.index(self.tafIntensityWx):
                        self.maxIntensity = newIntensity
                        self.tafIntensityWx = tafWxType

            if newSeverityLevel > currentSeverityLevel:
                self.maxIntensity = newIntensity
                self.tafIntensityWx = tafWxType

            return

        def thunderstormsPresent(self):
            allWeatherTypes = self.regularWeatherTypes + self.obstructionWeatherTypes
            # Only check for vicinity thunderstorms in FM groups (they aren't
            # allowed in TEMPO/PROB30 groups)

            if self.vicinityWeather is not None:
                allWeatherTypes += [self.vicinityWeather]

            for weatherType in allWeatherTypes:
                # We check like this because thunderstorms can have attributes
                # added to them to become new types (i.e. TSGR) so we can't
                # simply look for TS in allWeatherTypes.
                if "TS" in weatherType:
                    return True

            return False

        def _removeDuplicateWeather(self, weatherTypes):
            """Remove duplicate weather types while preserving order."""

            weatherTypes = list(OrderedDict.fromkeys(weatherTypes))

            return weatherTypes

        def _consolidateRegularWeather(self, weatherTypes):
            # Remove SHRA if RA present
            if "SHRA" in weatherTypes and "RA" in weatherTypes:
                weatherTypes.remove("SHRA")

            # Some thunder cases have other precipitation types already combined.
            # So, we must test for each case.
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

                    # Replace SHSN with SN, so we get SHRASN, not SHRASHSN
                    weatherTypes.insert(snowIndex, "SN")
                    weatherTypes.remove("SHSN")

                # Otherwise, if SHSN is more dominant
                else:

                    # Replace SHRA with RA, so we get SHSNRA, not SHSNSHRA
                    weatherTypes.insert(rainIndex, "RA")
                    weatherTypes.remove("SHRA")

            # Remove RA if FZRA present
            if "RA" in weatherTypes and "FZRA" in weatherTypes:
                weatherTypes.remove("RA")

            # Remove SHRA if FZRA present
            if "SHRA" in weatherTypes and "FZRA" in weatherTypes:
                weatherTypes.remove("SHRA")

            # Remove DZ if FZDZ or FZRA present
            if "DZ" in weatherTypes and ("FZRA" in weatherTypes or "FZDZ" in weatherTypes):
                weatherTypes.remove("DZ")

            # Remove FZDZ if FZRA present
            if "FZDZ" in weatherTypes and "FZRA" in weatherTypes:
                weatherTypes.remove("FZDZ")

            return weatherTypes

        def _consolidateObstructionWeather(self, weatherTypes):
            # Remove BR if FG or FZFG present
            if "BR" in weatherTypes and ("FG" in weatherTypes or "FZFG" in weatherTypes):
                weatherTypes.remove("BR")

            # Remove FG if FZFG present
            if "FG" in weatherTypes and "FZFG" in weatherTypes:
                weatherTypes.remove("FG")

            return weatherTypes

        def _orderRegularWeather(self, weatherTypes):
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

            # Look through all the precipitation weather types
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

    class SignificanceRatingRules:
        def __init__(self, textProduct, group):
            self._textProduct = textProduct
            self._group = group

        @staticmethod
        def averageRating(ratingDict1, ratingDict2=dict()):
            ratings = list(ratingDict1.values()) + list(ratingDict2.values())
            # Don't let non-existent ratings mess up the average rating
            ratings = [value for value in ratings if value is not None]

            return reduce(int.__add__, ratings, 0) / float(len(ratings))

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

    class ShorteningAlgorithms:
        def __init__(self, textProduct, airportIcaoId):
            self._textProduct = textProduct
            self._airportIcaoId = airportIcaoId

        # This class ranks all the weather element changes from one time period to
        # another. Algorithms for each element are independent of one another.
        # THis ranking of forecast changes determines which FMGroups we select for
        # final TAF product.
        class Ranking:
            def __init__(self, textProduct, airportIcaoId):

                self._textProduct = textProduct
                self._airportIcaoId = airportIcaoId
                self._windspeedCategories = [0, 6, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55]
                self._windDirCategories = [0, 10, 15, 20, 25, 30]
                self._windGustCategories = [0, 6, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55]
                self._skyCategories = [0, 6, 31, 56, 87]
                self._cloudBaseCategories = [0, 2, 5, 10, 30, 50, 70, 100]
                self._visibilityCategories = [
                    0.0,
                    0.25,
                    0.50,
                    0.75,
                    1.0,
                    1.5,
                    2.0,
                    3.0,
                    4.0,
                    5.0,
                    6.0,
                ]
                self._wxTypes = [
                    "TS",
                    "RA",
                    "SHRA",
                    "DZ",
                    "FZRA",
                    "FZDZ",
                    "SN",
                    "SHSN",
                    "PL",
                    "FZFG",
                    "BR",
                    "IC",
                    "HZ",
                    "BLSN",
                    "BLSA",
                    "BLDU",
                    "FU",
                    "VA",
                    "GS",
                    "GR",
                ]
                self._wxIntensities = ["<NoInten>", "--", "-", "m", "+"]
                self._flightCategories = ["A", "B", "C", "D", "E", "F"]

            # Generic code to calculate the category
            def _calcCategory(self, value, catList):
                if not value:
                    return 0
                for i, cat in enumerate(catList):
                    if value <= cat:
                        return i

                return len(catList)

            def minRunwayAngle(self, runwayInfo, windSpeed, windDirection):
                """
                Given the runwayInfo, calculate the difference between
                the runway direction and the wind direction, if the
                speed and angle criteria are met.
                """

                def directionDiff(dirA, dirB):
                    """Helper method that gets the difference between directions"""
                    diff = abs(dirA - dirB)
                    if diff > 180:
                        diff = 360 - diff
                    return diff

                minRWInfo = None
                if not runwayInfo:
                    return None

                minAngle = 360
                for rwDir, speed, deltaAngle in runwayInfo:
                    if windSpeed < speed:
                        continue
                    angle = directionDiff(rwDir, windDirection)
                    if angle < minAngle:
                        minAngle = angle
                        minRWInfo = (rwDir, speed, deltaAngle, minAngle)
                return minRWInfo

            def calcWindspeedRanking(self, runwayInfo, speed1, dir1, speed2, dir2):
                # Calculate the direction difference
                if isinstance(dir1, int) and isinstance(dir2, int):
                    dirDiff = abs(dir1 - dir2)
                    if dirDiff > 180:
                        dirDiff = abs(dirDiff - 360)
                else:
                    dirDiff = 0

                # Fetch some stuff we'll need
                lowSpeed = self._textProduct._windSpeedRankLow
                speedDiff = abs(speed1 - speed2)
                # Calc min and max of the speeds for later use
                minSpeed = min(speed1, speed2)
                halfLowSpeed = lowSpeed / 2

                # Speed component
                speedComp = speedDiff * speedDiff
                # Add more if speeds are higher
                if speedDiff >= halfLowSpeed:
                    speedComp += (speedDiff - halfLowSpeed) ** 2

                # Check for lowSpeed to higher speed
                if speed1 < lowSpeed <= speed2:
                    speedComp += 50 + (speedDiff * 5)

                # Direction component
                if minSpeed > lowSpeed:
                    dirComp = (minSpeed - lowSpeed) * (dirDiff * dirDiff / 25)
                else:
                    # Fewer points for low speeds
                    dirComp = int(np.sqrt(dirDiff)) + speedDiff * dirDiff / 5

                points = speedComp + dirComp
                points = min(points, 200)
                # Check for cross runway wind
                if runwayInfo is None:
                    return points

                beforeInfo = self.minRunwayAngle(runwayInfo, speed1, dir1)
                afterInfo = self.minRunwayAngle(runwayInfo, speed2, dir2)

                if beforeInfo is None and afterInfo is None:
                    return points
                # Check for case where wind is along runway and then not or vice-versa
                if (beforeInfo is None and afterInfo is not None) or (
                    beforeInfo is not None and afterInfo is None
                ):
                    return points + 100
                # Check for case where winds are along two different runways
                if beforeInfo is not None and afterInfo is not None:
                    beforeRunwayDir, speed, delta, angle = beforeInfo
                    afterRunwayDir, speed, delta, angle = afterInfo
                    if beforeRunwayDir != afterRunwayDir:
                        return points + 100
                return points

            def calcWindGustRanking(self, gust1, gust2):

                minGustDiff = self._textProduct._windGustDiff

                # Check for None cases
                # Both None
                if gust1 is None and gust2 is None:
                    return 0
                # If we go from no gust to a gust: 200
                if gust1 is None and gust2 is not None:
                    return 200
                # If we go from a gust to no gust: 100
                if gust1 is not None and gust2 is None:
                    return 100
                # Both valid gusts - check for difference
                if abs(gust1 - gust2) >= minGustDiff:
                    return 100
                else:
                    return abs(gust1 - gust2) * 5

            def calcSkyRanking(self, sky1, sky2, cloudBase1, cloudBase2):

                # Try to fetch the maxSigCeiling from the dictionary, otherwise use
                # the default.
                try:
                    maxCeiling = self._textProduct._maxSigCeilingByAirport[self._airportIcaoId]
                except AttributeError:
                    maxCeiling = self._textProduct._maxSignificantNonCeilingHeight

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
                # ceiling cases. If it flips from one to the other,
                # big points but only if the ceiling is low enough
                # High cloud bases are not as important, so a different
                # algorithm for those.
                # If both ceilings are above the max, small points
                elif cloudBase1 > maxCeiling and cloudBase2 > maxCeiling:
                    return abs(cat1 - cat2) * 5
                # If we change from ceiling to non-ceiling, potentially big
                # points. Points are dependent on the ceiling height difference.
                elif (cat1 in nonCeiling and cat2 in ceiling) or (
                    cat2 in nonCeiling and cat1 in ceiling
                ):
                    catMax = self._calcCategory(maxCeiling, self._cloudBaseCategories)
                    catLow = self._calcCategory(
                        min(cloudBase1, cloudBase2), self._cloudBaseCategories
                    )
                    return (catMax - catLow) * 50  # 0-200 points
                else:
                    return abs(cat1 - cat2) * 20  # 20-120 points

            def calcCloudBaseRanking(self, cbp1, cbp2, cbs1, cbs2, cbc1, cbc2):
                # Internal method to rank the change of a single cloud base in time
                # Cloud bases are assumed to have real values (not None).
                def rankCloudChange(cb1, cb2):

                    if (
                        cb1 >= self._textProduct._maxSignificantNonCeilingHeight
                        and cb2 >= self._textProduct._maxSignificantNonCeilingHeight
                    ):
                        return abs(cb1 - cb2) / 20  # 0-10 points

                    # Calculate the major categories of each base
                    cat1 = self._calcCategory(cb1, self._cloudBaseCategories)
                    cat2 = self._calcCategory(cb2, self._cloudBaseCategories)
                    baseCatDiff = abs(cat1 - cat2)

                    if baseCatDiff == 0:
                        return abs(cb1 - cb2) / ((cb1 + cb2) / 2) * 20  # 0 - 20 points
                    elif baseCatDiff == 1:
                        minBase = min(cb1, cb2)
                        if minBase > 30:
                            return 25
                        elif 10 < minBase <= 30:  # below 3000 ft
                            return abs(cb1 - cb2) * 2  # 40 points max
                        else:  # <= 1000 ft
                            return abs(cb1 - cb2) * 5
                    else:  # baseCatDiff >= 2:
                        return baseCatDiff * 25  # 25 point per category diff

                    # Should never get here
                    return 0

                # End rankCloudChange method

                rankSum = 0

                if cbp1 is not None and cbp2 is not None:
                    rankSum += rankCloudChange(cbp1, cbp2)

                if cbs1 is not None and cbs2 is not None:
                    rankSum += rankCloudChange(cbs1, cbs2)

                if cbc1 is not None and cbc2 is not None:
                    rankSum += rankCloudChange(cbc1, cbc2)

                # Case where primary or secondary cloud grid starts or stops
                if (cbp1 is None and cbp2 is not None) or (cbp1 is not None and cbp2 is None):
                    rankSum = +100

                if (cbs1 is None and cbs2 is not None) or (cbs1 is not None and cbs2 is None):
                    rankSum = +100

                if (cbc1 is None and cbc2 is not None) or (cbc1 is not None and cbc2 is None):
                    rankSum = +100

                # If the Conditional grid starts or stops return a large value to force an FMGroup.
                if cbc1 is None and cbc2 is not None:  # new appearance of cbc
                    rankSum += 50 + rankCloudChange(cbp1, cbc2) * 3  # assign lots of points
                elif cbc1 is not None and cbc2 is None:  # cbc disappeared
                    rankSum += 50 + rankCloudChange(cbp1, cbc1) * 2

                if cbc1 is not None and cbc2 is not None:
                    rankSum += rankCloudChange(cbc1, cbc2)

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

                wxIntenTypes = [None, "--", "-", "m", "+"]

                if wx1 is None and wx2 is None:
                    return 0

                # If no Wx exists that's as good as None
                if wx1 is not None and not wx1.weatherExists():
                    wx1 = None
                if wx2 is not None and not wx2.weatherExists():
                    wx2 = None

                # If one is None and the other not, return a big number
                if wx1 is None and wx2 is not None or wx1 is not None and wx2 is None:
                    return 200

                # Analyze the weather types and intensities
                allWx1 = wx1.getAllWeather()
                allWx2 = wx2.getAllWeather()

                # If the number of weather types are different there must be
                # some new or missing weather
                if len(allWx1) != len(allWx2):  # must be some different weather
                    vcshIn1 = "VCSH" in allWx1
                    vcshIn2 = "VCSH" in allWx2
                    if vcshIn1 ^ vcshIn2:
                        return 100
                    return 200

                # Sort so each type can be compared
                allWx1.sort()
                allWx2.sort()

                # Check each weather type one at a time
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

                # Small utility to calculate importance of LLWS change
                def calcLLWSPoints(speed1, speed2):

                    diff = abs(speed1 - speed2)
                    if diff < 10:
                        return diff

                    points = (diff - 10) * 10  # diff 20 = 100 pts, diff 30 = 200 pts

                    if points > 200:  # clip at 200 points
                        points = 200

                    return points

                # Check if both are None
                if llws1 is None and llws2 is None:
                    return 0

                if llws1 is None or llws2 is None:  # only one is none
                    if llws1 is None:
                        d2, s2, h2 = llws2
                        return calcLLWSPoints(s2, 0)
                    else:  # llws2 is None
                        d1, s1, h1 = llws1
                        return calcLLWSPoints(s1, 0)

                # Both are valid.  Extract the parts
                d1, s1, h1 = llws1
                d2, s2, h2 = llws2

                return calcLLWSPoints(s1, s2)

        ############## End - Class Ranking

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
                    wxStr = f"{wxStr + prob + wxType + inten}.{info.tafWeatherType}"

            return wxStr

        # Temporary method to format a single string contain the values of all elements
        def makeDumpString(self, fmGroup):

            weList = [
                ("Wind:", "wind"),
                ("Gust:", "windGust"),
                ("Wx:", "weather"),
                ("Sky:", "sky"),
                ("CBP^S^C:", "cloudBase"),
                ("FC:", "flightCategory"),
                ("Vis:", "visibility"),
                ("LLWS:", "llws"),
            ]
            rankDict = fmGroup.rankDict

            dumpStr = f"+& {fmGroup._airportDict['icaoAirportIndicator']}"
            dumpStr = f"{dumpStr} FcstHr:{fmGroup.productDict['groupLabel']} "
            for label, weName in weList:
                if weName in fmGroup.rankDict:
                    pointStr = f">{str(int(fmGroup.rankDict[weName]))}"
                else:
                    pointStr = ""
                if weName == "weather":
                    dumpStr += f" {label}{self.getWxDumpStr(fmGroup.productDict[weName])}{pointStr}"
                elif weName == "cloudBase":
                    dumpStr += (
                        f" {label}{fmGroup.productDict['cloudBasePrimary']}^"
                        f"{fmGroup.productDict['cloudBaseSecondary']}{pointStr}"
                    )
                else:
                    dumpStr += f" {label}{fmGroup.productDict[weName]}{pointStr}"
                # if rankDict is not None:
                #     dumpStr += f">{rankDict[weName]} "
            dumpStr += f" Total:{fmGroup.rankingValue}"

            return dumpStr

        def rankElementChange(self, rankingObj, fmGroups, we, prev, curr):

            if we == "wind":
                dir1, speed1 = fmGroups[prev].productDict[we]
                dir2, speed2 = fmGroups[curr].productDict[we]

                runwayInfo = self._textProduct._runwayInfo.get(self._airportIcaoId, None)
                windRank = rankingObj.calcWindspeedRanking(runwayInfo, speed1, dir1, speed2, dir2)
                return windRank
            elif we == "windGust":
                gust1 = fmGroups[prev].productDict[we]
                gust2 = fmGroups[curr].productDict[we]
                gustRank = rankingObj.calcWindGustRanking(gust1, gust2)
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
                return rankingObj.calcVisibilityRanking(
                    fmGroups[prev].productDict[we], fmGroups[curr].productDict[we]
                )
            elif we == "llws":
                return rankingObj.calcLLWSRanking(
                    fmGroups[prev].productDict[we], fmGroups[curr].productDict[we]
                )

            return 0

        def rankingSort(self, a, b):

            periodA, rankA = a
            periodB, rankB = b

            if rankA < rankB:
                return 1
            elif rankB < rankA:
                return -1

            return 0

        # Calculates the rank or importance of the two FMGroups.
        # Ultimately this is a measure of difference between the two groups
        def calculateRank(self, fmGroups, index1, index2):

            # the object that will rank the elements
            rankingObj = self.Ranking(self._textProduct, self._airportIcaoId)

            elementList = [
                "wind",
                "windGust",
                "sky",
                "weather",
                "cloudBase",
                "flightCategory",
                "visibility",
                "llws",
            ]
            rankSum = 0

            # Loop through each element and sum the rank over all of them.
            for we in elementList:
                # Make sure the element is in both productDicts
                prodDict1 = fmGroups[index1].productDict.get(we, None)
                prodDict2 = fmGroups[index2].productDict.get(we, None)
                if not (prodDict1 and prodDict2):
                    continue

                rank = self.rankElementChange(rankingObj, fmGroups, we, index1, index2)
                fmGroups[index2].rankDict[we] = rank
                rankSum += rank

            fmGroups[index2].rankingValue = rankSum

            return rankSum

        # This method finds appropriate FMGroups by ranking them and comparing the rank to
        # the specified threshold. Once a new FMGroup meets that criteria, future FMGroups
        # are compared to the last found FMGroups.
        # Applies weighting to each group based on the time-weighting definition.
        def findKeepers(self, fmGroups, threshold):

            # The first FMGroup is always included, so initialize with that.
            # This format of each item is (position, rankScore).
            keepers = [(0, 0)]

            baseIndex = 0
            fmIndex = 1

            while fmIndex < len(fmGroups):
                rank = self.calculateRank(fmGroups, baseIndex, fmIndex)

                # Weight the rank based on the configured timeWeighting function
                if fmIndex < len(self._FMTimeWeights):
                    rank *= self._FMTimeWeights[fmIndex - 1]

                if rank >= threshold and rank > 0:
                    keepers.append((fmIndex, rank))
                    baseIndex = fmIndex

                fmIndex += 1

            if len(keepers) <= 1:
                return keepers

            # Remove keepers that are not worthy based on the minimumRankValue
            reducedKeepers = [
                (fmIndex, rankValue)
                for fmIndex, rankValue in keepers
                if fmIndex == 0 or rankValue > self._textProduct._minimumRankValue
            ]

            return reducedKeepers

        def makeFMGroupList(self, fmGroups, keepers):

            # Extract the FMGroups from the full list and the keepers
            keepers.sort()
            fmGroupList = []

            for position, rank in keepers:
                fmGroupList.append(fmGroups[position])

            return fmGroupList

        def makeTimeWeights(self):

            # Make a simple time weighting array based on the configuration
            timeWeights = []
            for hours, weight in self._textProduct._timeWeights:
                for i in range(hours):
                    timeWeights.append(weight)

            return timeWeights

        def shortenFmGroups(self, fmGroups):
            """
            This will try to shorten the TAF output so that it hopefully does
            not exceed the desired number of periods (specified using the
            'maxFmGroups' configuration setting) while still trying to be as
            meteorologically correct as possible and conveying operationally
            significant information.
            """
            minFmGroups = self._textProduct._minFmGroups
            maxFmGroups = self._textProduct._maxFmGroups

            self._textProduct.debug_print(
                f"\n{'=' * 80}\nShortening FM groups for {self._airportIcaoId} "
                f"(number of periods = {len(fmGroups)}, max allowed = {maxFmGroups})",
                1,
            )

            # Calculate the timeWeights values here, as they will be reused extensively
            # when finding the keepers
            self._FMTimeWeights = self.makeTimeWeights()

            # Define a set of thresholds in decreasing order. Start at the
            # mandatory threshold and increase the resolution as we get
            # closer to zero
            mandatoryThreshold = 800
            thresholds = list(range(mandatoryThreshold, 200, -20))  # increments of 10
            thresholds += list(range(200, 50, -5))  # increments of 5
            thresholds += list(range(50, 2, -2))  # increments of 2
            for threshold in thresholds:
                keepers = self.findKeepers(fmGroups, threshold)

                if len(keepers) >= self._textProduct._detail:
                    if threshold == mandatoryThreshold:  # Must haves
                        return self.makeFMGroupList(fmGroups, keepers)
                    else:  # Too many, so cull the herd
                        # keepers.sort(self.rankingSort)
                        keepers.sort(key=itemgetter(1), reverse=True)
                        keepers = keepers[0 : self._textProduct._detail - 1]
                        keepers.insert(0, (0, 0))  # Prepend the first group
                        return self.makeFMGroupList(fmGroups, keepers)

            return self.makeFMGroupList(fmGroups, keepers)

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
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName, histSample) == 0:
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
                    self.debug_print(f"\ttime range: {timeRange}", 1)
                    self.debug_print(
                        f"\tcount: {count}, value: (magnitude {magnitude}, direction {direction})",
                        1,
                    )

                    # Only incorporate this histPair in our calculations if its
                    # magnitude and direction are both greater than 0...
                    # if magnitude <= 0 or direction <= 0:
                    #     includeHistPair = False
                    if magnitude <= 0 or direction < 0:
                        includeHistPair = False

                if includeHistPair:
                    validTime = histSample.validTime()
                    validTime = TimeRange.TimeRange(validTime)
                    weight = validTime.intersection(timeRange).duration()
                    totWeight += weight
                    count = histPair.count()
                    totCount += count
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


class Output_Formatter:
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

        fcst += self._makeProductHeader(productDict)

        for airportDict in airportDicts.values():
            fcst += self._makeAirportTafHeader(productDict, airportDict)

            airportIcaoId = airportDict["icaoAirportIndicator"]
            self._textProduct.debug_print(f"\n{'=' * 80}\nForecast Output for {airportIcaoId}:", 1)
            for fmGroup in airportDict["fmGroups"]:
                self._textProduct.debug_print(f"\n{'_' * 80}\n{fmGroup}:", 1)

                fcst += self._makeAirportTafFmGroup(productDict, airportDict, fmGroup.productDict)
            self._textProduct.debug_print(" ", 1)

            # Clean up the initial FM group to not have a FM group label
            fcst = re.sub(r"\*\n +FM\d{6} ", "", fcst)

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
            os.system(f"/awips2/fxa/bin/textdb -w WRKTAF < {tacOutputFile}")

            # Determine a relative path for the EDEX utility tree
            relativeFilePath = "aviation/tmp/gridTAF.txt"

            # Write this file into the Localization store - needs to be generalized for all sites
            LocalizationSupport.writeFile(
                LocalizationSupport.COMMON_STATIC,
                LocalizationSupport.SITE,
                siteID,
                relativeFilePath,
                fcst[index:],
            )

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
            return f"000\n{productDict['bulletinType']}{productDict['bulletinLocation']}{productDict['bulletinDifferentiator']} {productDict['icaoOfficeIndicator']} {productDict['issuanceTime'].tm_mday:02d}{productDict['issuanceTime'].tm_hour:02d}00 {productDict['tafTypeIdentifier']}\n"
        else:
            # Format header for these TAFs so AvnFPS will ingest them
            return (
                f"{productDict['bulletinType']}{productDict['bulletinLocation']}"
                f"{productDict['bulletinDifferentiator']} "
                f"{productDict['icaoOfficeIndicator']} "
                f"{productDict['issuanceTime'].tm_mday:02d}"
                f"{productDict['issuanceTime'].tm_hour:02d}"
                f"{productDict['issuanceTime'].tm_min:02d} "
                f"{productDict['tafTypeIdentifier']}\n"
            )

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
            fcst = f"{airportDict['airportHeader']}\n"

        preparationTimeUTC = time.gmtime(airportDict["preparationTime"])
        # Format header for these TAFs so AvnFPS will ingest them
        if not airportDict["fmGroups"]:
            fcst += (
                f"{airportDict['tafTypeHeader']}\n{airportDict['icaoAirportIndicator']} "
                f"{preparationTimeUTC.tm_mday:02d}{preparationTimeUTC.tm_hour:02d}"
                f"{preparationTimeUTC.tm_min:02d}Z NIL"
            )
        else:
            validPeriod = self._formatValidPeriod(
                airportDict["validPeriodStart"], airportDict["validPeriodEnd"]
            )

            fcst += (
                f"{airportDict['tafTypeHeader']}\n{airportDict['icaoAirportIndicator']} "
                f"{preparationTimeUTC.tm_mday:02d}{preparationTimeUTC.tm_hour:02d}"
                f"{preparationTimeUTC.tm_min:02d}Z {validPeriod} *"
            )

        return fcst

    def _formatValidPeriod(self, startTimeSeconds, endTimeSeconds):
        validPeriodStartUTC = time.gmtime(startTimeSeconds)
        startDay = validPeriodStartUTC.tm_mday
        startHour = validPeriodStartUTC.tm_hour

        validPeriodEndUTC = time.gmtime(endTimeSeconds)
        # Fix the end time if necessary so that 00Z displays as 24Z of the
        # previous day
        if validPeriodEndUTC.tm_hour == 0:
            endDay = time.gmtime(endTimeSeconds - 24 * 3600).tm_mday
            endHour = 24
        else:
            endDay = validPeriodEndUTC.tm_mday
            endHour = validPeriodEndUTC.tm_hour

        return f"{startDay:02d}{startHour:02d}/{endDay:02d}{endHour:02d}"

    def _makeAirportTafFmGroup(self, productDict, airportDict, fmGroupDict):
        windText = self._formatWind(fmGroupDict["wind"], fmGroupDict["windGust"])
        self._textProduct.debug_print(f"windText = {windText}", 1)

        visibilityWeatherText = self._formatVisibilityWeather(
            fmGroupDict["visibility"], fmGroupDict["weather"]
        )
        self._textProduct.debug_print(f"visibilityWeatherText = {visibilityWeatherText}", 1)

        cloudsText = self._formatClouds(fmGroupDict["clouds"], fmGroupDict["weather"])
        self._textProduct.debug_print(f"cloudsText = {cloudsText}", 1)

        llwsText = self._formatLLWS(fmGroupDict["llws"])
        self._textProduct.debug_print(f"llwsText = {llwsText}", 1)

        conditionalGroupText = self._formatConditionalGroup(fmGroupDict["conditionalGroup"])

        fcst = f"\n     {fmGroupDict['groupLabel']} {windText} {visibilityWeatherText} {cloudsText}"

        if llwsText:
            fcst += f" {llwsText}"

        if conditionalGroupText:
            fcst += conditionalGroupText

        self._textProduct.debug_print(f"\n{fcst}", 1)

        return fcst

    def _formatWind(self, wind, windGust):
        """
        This method will format the wind and wind gust text for a TAF
        forecast hour.
        """
        if not wind:
            return ""
        windDirection, windSpeed = wind

        # Criteria for reporting windGust move to the formatting stage since
        # windGust data is needed to properly rank each FMgroup.
        if windGust is not None:
            if (
                windSpeed < self._textProduct._minWindToReportGusts
                or windGust - windSpeed < self._textProduct._minGustSpeedDifference
            ):
                windGust = None

        # If we should not report a gust
        if windGust is None:
            # If we have a "variable" wind
            if windDirection == "VRB":
                # Format a variable wind forecast
                windText = f"VRB{windSpeed:02d}KT"

            # Otherwise, format a sustained wind forecast
            else:
                windText = f"{windDirection:03d}{windSpeed:02d}KT"

        # Otherwise, report a wind and wind gust forecast
        else:
            # If we have a "variable" wind
            if windDirection == "VRB":
                windText = f"VRB{windSpeed:02d}G{windGust:d}KT"
            else:
                windText = f"{windDirection:03d}{windSpeed:02d}G{windGust:d}KT"

        # Return the text we have
        return windText

    def _formatVisibilityWeather(self, numericalVisibility, weather):

        visibilityText = self._convertVisibilityToText(numericalVisibility)
        visibilityWeather = visibilityText

        # Add in weather
        weatherText = self._formatWeather(weather, numericalVisibility)

        # If we have visibility and weather, add a space to separate them
        if visibilityText and weatherText:
            visibilityWeather += " "

        visibilityWeather += weatherText

        return visibilityWeather

    def _wxTypeOnly(self, wx):
        if not wx:
            return wx

        if wx[0] in ["-", "+"]:
            return wx[1:]

        return wx

    # Tests whether conditions are good for adding BR to obstruction weather
    def _BRCanBeAdded(self, weather, visibility):
        if 0.625 <= visibility <= 6.0:
            if weather is None:
                return True

            for obsWx in weather.obstructionWeatherTypes:
                if obsWx in weather.obstructionTypes:
                    return False

            return True

        return False

    # Tests whether conditions are good for adding FG to obstruction weather
    def _FGCanBeAdded(self, weather, visibility):

        if visibility < 0.625:
            if not weather.weatherExists():
                return True

        return False

    def _formatWeather(self, weather, numericalVisibility):

        if not weather:
            weather = self._textProduct.GroupWeatherInfo(
                self._textProduct, "FM", numericalVisibility
            )

        # See if we can add BR if the visibility is within a particular range
        if numericalVisibility and self._BRCanBeAdded(weather, numericalVisibility):
            weather.obstructionWeatherTypes.append("BR")

        # weather.regularWeatherTypes.sort(weather.precipSort)
        weather.regularWeatherTypes = sorted(weather.regularWeatherTypes, key=weather.precipSortKey)

        allWxTypes = weather.regularWeatherTypes

        # allWxTypes.sort(weather.weatherSort)
        allWxTypes.sort(key=weather.weatherSortKey)
        # Add in the intensity to the appropriate wxType
        for i in range(len(allWxTypes)):
            if allWxTypes[i] == weather.tafIntensityWx:
                if weather.maxIntensity not in ["<NoInten>", "M", "m"]:  # Matt's fix
                    allWxTypes[i] = weather.maxIntensity + allWxTypes[i]

        weatherText = ""
        if not allWxTypes and not weather.obstructionWeatherTypes:
            return ""

        for wx in allWxTypes:

            if wx in weather.vicinityWxTypes:
                continue

            weatherText += wx

        # Append the obstructions to the weatherText
        obsWx = ""

        for wx in weather.obstructionWeatherTypes:
            if not obsWx:
                obsWx = wx
            else:
                obsWx += f" {wx}"

        if obsWx:
            if weatherText:
                weatherText += f" {obsWx}"
            else:
                weatherText = obsWx

        if weather.vicinityWeather is not None:
            weatherText += f" {weather.vicinityWeather}"

        # Clean up any added extra spaces
        weatherText = weatherText.strip()

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
                if index == 0 and weather and weather.thunderstormsPresent():
                    cloudText += f" {coverage}{height:03d}CB"
                else:
                    cloudText += f" {coverage}{height:03d}"

            cloudText = cloudText.strip()
            if not cloudText:
                cloudText = "SKC"

            return cloudText
        else:
            return ""

    def _formatLLWS(self, llws):
        """Format the low-level wind shear text for a TAF forecast hour."""

        llwsText = ""

        # None case
        if llws is None:
            return llwsText

        # If we have both a wind and a height to report
        direction, speed, height = llws
        #Direction is rounded to the nearest 10 degrees.  0 is expressed as 360.
        #direction //= 10
        direction=round(direction/10)*10
        if direction == 0:
            direction = 360
        if speed >= self._textProduct._llwsThreshold:
            # Format the wind shear group. Prepend a space since it's optional
            # and if it doesn't exist, we don't want extra spaces in the text.
            llwsText = f"WS{height:03d}/{direction:03d}{speed:02d}KT"

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

            visibilityWeatherText = self._formatVisibilityWeather(numericalVisibility, weather)

            cloudText = self._formatClouds(clouds, weather)

            self._textProduct.debug_print("Conditional group information:", 1)
            self._textProduct.debug_print(f"\tgroupType = {groupType}", 1)
            self._textProduct.debug_print(f"\tgroupLabel = {groupLabel}", 1)
            self._textProduct.debug_print(f"\tvisibilityWeatherText = {visibilityWeatherText}", 1)
            self._textProduct.debug_print(f"\tcloudText = {cloudText}", 1)

            # Don't create a TEMPO/PROB if all it's going to show is P6SM
            if visibilityWeatherText == "P6SM" and not cloudText:
                return ""
            else:
                if groupType == "TEMPO":
                    groupText = f"\n      {groupLabel}"
                else:
                    groupText = f" {groupLabel}"

                if visibilityWeatherText:
                    groupText += f" {visibilityWeatherText}"

                if cloudText:
                    groupText += f" {cloudText}"

                return groupText

    def _makeAirportTafFooter(self, productDict, airportDict):
        fcst = ""

        if airportDict["airportDisclaimer"] is not None:
            # Add the disclaimer to this airport TAF (indented 6 spaces)
            fcst += f"\n      {airportDict['airportDisclaimer']}"
        # Place a blank line between each airport TAF forecast
        fcst += "=\n\n"

        return fcst


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
            "http://codes.wmo.int/306/4678/VCTS": "Thunderstorm in the vicinity",
        }

        self._coverageHref = {
            "FEW": "http://codes.wmo.int/bufr4/codeflag/0-20-008/1",
            "SCT": "http://codes.wmo.int/bufr4/codeflag/0-20-008/2",
            "BKN": "http://codes.wmo.int/bufr4/codeflag/0-20-008/3",
            "OVC": "http://codes.wmo.int/bufr4/codeflag/0-20-008/4",
        }

        self._coverageTitle = {
            "FEW": "Few",
            "SCT": "Scattered",
            "BKN": "Broken",
            "OVC": "Overcast",
        }

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
        attributes[
            "xsi:schemaLocation"
        ] = "http://icao.int/iwxxm/2.0 http://schemas.wmo.int/iwxxm/2.0/iwxxm.xsd"

        return Element("TAF", attributes)

    def _iwxxmTags(self):
        """Specifies the main XML tags needed for the IWXXM XML format."""

        return [
            self.tag("issueTime", method=self._iwxxmIssueTime),
            self.tag("validTime", method=self._iwxxmValidTime),
        ]

    def _iwxxmIssueTime(self, tag, productDict, airportDict):
        self._addChild("issueTime")

        timeInstant = self._addChild("gml:TimeInstant", "issueTime")
        self._issueTimeUuidString = self._randomUuidString()
        timeInstant.set("gml:id", self._issueTimeUuidString)

        timePosition = self._addChild("gml:timePosition", "issueTime/gml:TimeInstant")

        timeUTC = time.gmtime(airportDict["preparationTime"])
        timePosition.text = time.strftime("%Y-%m-%dT%H:%M:%SZ", timeUTC)

    def _iwxxmValidTime(self, tag, productDict, airportDict):
        self._addChild("validTime")

        timePeriod = self._addChild("gml:TimePeriod", "validTime")
        self._validTimeUuidString = self._randomUuidString()
        timePeriod.set("gml:id", self._validTimeUuidString)

        beginPosition = self._addChild("gml:beginPosition", "validTime/gml:TimePeriod")

        timeUTC = time.gmtime(airportDict["validPeriodStart"])
        beginPosition.text = time.strftime("%Y-%m-%dT%H:00:00Z", timeUTC)

        endPosition = self._addChild("gml:endPosition", "validTime/gml:TimePeriod")

        timeUTC = time.gmtime(airportDict["validPeriodEnd"])
        endPosition.text = time.strftime("%Y-%m-%dT%H:00:00Z", timeUTC)

    def _forecastTags(self):
        """
        Specifies the XML tags used for each (FM/TEMPO/PROB30) forecast.
        Tags starting with a lowercase "x" will have that "x" replaced with
        either "base" (first forecast) or "change" (all remaining forecasts).
        So for example, the first tag ("xForecast") will become "baseForecast"
        for the first forecast described, and it will become "changeForecast"
        for all remaining forecasts described.
        """

        return [
            self.tag("xForecast"),
            self.tag("om:OM_Observation", parentTag="xForecast", method=self._OM_Observation),
            self.tag("om:type", parentTag="xForecast/om:OM_Observation", method=self._type),
            self.tag(
                "om:phenomenonTime",
                parentTag="xForecast/om:OM_Observation",
                method=self._phenomenonTime,
            ),
            self.tag(
                "om:resultTime", parentTag="xForecast/om:OM_Observation", method=self._resultTime
            ),
            self.tag(
                "om:validTime", parentTag="xForecast/om:OM_Observation", method=self._validTime
            ),
            self.tag(
                "om:procedure", parentTag="xForecast/om:OM_Observation", method=self._procedure
            ),
            self.tag(
                "om:observedProperty",
                parentTag="xForecast/om:OM_Observation",
                method=self._observedProperty,
            ),
            self.tag(
                "om:featureOfInterest",
                parentTag="xForecast/om:OM_Observation",
                method=self._featureOfInterest,
            ),
            self.tag("om:result", parentTag="xForecast/om:OM_Observation", method=self._result),
        ]

    def _OM_Observation(self, tag, productDict, airportDict, groupDict):
        observation = self._addChild("om:OM_Observation", tag.parentTag)
        observation.set("gml:id", self._randomUuidString())

    def _type(self, tag, productDict, airportDict, groupDict):
        type = self._addChild("om:type", tag.parentTag)
        type.set(
            "xlink:href",
            "http://codes.wmo.int/49-2/observation-type/IWXXM/1.0/MeteorologicalAerodromeForecast",
        )

    def _phenomenonTime(self, tag, productDict, airportDict, groupDict):
        self._addChild("om:phenomenonTime", tag.parentTag)

        timePeriod = self._addChild("gml:TimePeriod", f"{tag.parentTag}/om:phenomenonTime")
        timePeriod.set("gml:id", self._randomUuidString())

        beginPosition = self._addChild(
            "gml:beginPosition", f"{tag.parentTag}/om:phenomenonTime/gml:TimePeriod"
        )

        startTimeUTC = time.gmtime(groupDict["startTime"])
        beginPosition.text = time.strftime("%Y-%m-%dT%H:%M:%SZ", startTimeUTC)

        endPosition = self._addChild(
            "gml:endPosition", f"{tag.parentTag}/om:phenomenonTime/gml:TimePeriod"
        )

        endTimeUTC = time.gmtime(groupDict["endTime"])
        endPosition.text = time.strftime("%Y-%m-%dT%H:%M:%SZ", endTimeUTC)

    def _resultTime(self, tag, productDict, airportDict, groupDict):
        resultTime = self._addChild("om:resultTime", tag.parentTag)
        resultTime.set("xlink:href", f"#{self._issueTimeUuidString}")

    def _validTime(self, tag, productDict, airportDict, groupDict):
        validTime = self._addChild("om:validTime", tag.parentTag)
        validTime.set("xlink:href", f"#{self._validTimeUuidString}")

    def _procedure(self, tag, productDict, airportDict, groupDict):
        if self._forecastIndex > 0:
            procedure = self._addChild("om:procedure", tag.parentTag)
            procedure.set("xlink:href", f"#{self._processUuidString}")
            return

        self._addChild("om:procedure", tag.parentTag)

        process = self._addChild("metce:Process", f"{tag.parentTag}/om:procedure")
        self._processUuidString = self._randomUuidString()
        process.set("gml:id", self._processUuidString)

        description = self._addChild(
            "gml:description", f"{tag.parentTag}/om:procedure/metce:Process"
        )
        description.text = (
            "United States National Weather Service "
            "Instruction 10-813 Terminal Aerodrome Forecasts"
        )

    def _observedProperty(self, tag, productDict, airportDict, groupDict):
        observedProperty = self._addChild("om:observedProperty", tag.parentTag)
        observedProperty.set(
            "xlink:href",
            "http://codes.wmo.int/49-2/observables-property/MeteorologicalAerodromeForecast",
        )

    def _featureOfInterest(self, tag, productDict, airportDict, groupDict):
        if self._forecastIndex > 0:
            feature = self._addChild("om:featureOfInterest", tag.parentTag)
            feature.set("xlink:href", f"#{self._samplingFeatureUuidString}")
            return

        parentTag = tag.parentTag

        self._addChild("om:featureOfInterest", parentTag)
        parentTag += "/om:featureOfInterest"

        samplingFeature = self._addChild("sams:SF_SpatialSamplingFeature", parentTag)
        self._samplingFeatureUuidString = self._randomUuidString()
        samplingFeature.set("gml:id", self._samplingFeatureUuidString)
        parentTag += "/sams:SF_SpatialSamplingFeature"

        type = self._addChild("sf:type", parentTag)
        type.set(
            "xlink:href",
            "http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint",
        )

        self._addChild("sf:sampledFeature", parentTag)
        parentTag += "/sf:sampledFeature"

        airportHeliport = self._addChild("aixm:AirportHeliport", parentTag)
        airportHeliport.set("gml:id", self._randomUuidString())
        parentTag += "/aixm:AirportHeliport"

        self._addChild("aixm:timeSlice", parentTag)
        parentTag += "/aixm:timeSlice"

        airportHeliportTimeSlice = self._addChild("aixm:AirportHeliportTimeSlice", parentTag)
        airportHeliportTimeSlice.set("gml:id", self._randomUuidString())
        parentTag += "/aixm:AirportHeliportTimeSlice"

        validTime = self._addChild("gml:validTime", parentTag)
        validTime.set("xlink:href", f"#{self._issueTimeUuidString}")

        interpretation = self._addChild("aixm:interpretation", parentTag)
        interpretation.text = "SNAPSHOT"

        designator = self._addChild("aixm:designator", parentTag)
        designator.text = airportDict["icaoAirportIndicator"]

        # TODO: name

        location = self._addChild("aixm:locationIndicatorICAO", parentTag)
        location.text = airportDict["icaoAirportIndicator"]

        # Done with the sampled feature element
        parentTag = re.sub(
            "/sf:sampledFeature/aixm:AirportHeliport/aixm:timeSlice/aixm:AirportHeliportTimeSlice",
            "",
            parentTag,
        )

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

        record = self._addChild("MeteorologicalAerodromeForecastRecord", parentTag)
        record.set("changeIndicator", groupDict["groupClassification"])
        record.set("gml:id", self._randomUuidString())
        if (
            visibility is not None
            and weather is not None
            and visibility < self._textProduct._minP6smVisibility
        ) or (isinstance(clouds, list) and clouds):

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
        prevailingVisibility = self._addChild("prevailingVisibility", parentTag)
        prevailingVisibility.set("uom", "m")
        if visibility >= self._textProduct._minP6smVisibility or weather is None:

            prevailingVisibility.text = "10000"
            operator = self._addChild("prevailingVisibilityOperator", parentTag)
            operator.text = "ABOVE"
        else:
            milesToMeters = 1609.344
            prevailingVisibility.text = str(int(round(visibility * milesToMeters)))

    def _resultWind(self, tag, parentTag, wind, windGust):
        self._addChild("surfaceWind", parentTag)
        windForecast = self._addChild("AerodromeSurfaceWindForecast", f"{parentTag}/surfaceWind")
        (direction, speed) = wind
        if direction == "VRB":
            windForecast.set("variableWindDirection", "true")
        else:
            windForecast.set("variableWindDirection", "false")
            windDirection = self._addChild(
                "meanWindDirection", f"{parentTag}/surfaceWind/AerodromeSurfaceWindForecast"
            )
            windDirection.set("uom", "deg")
            windDirection.text = str(direction)
        windSpeed = self._addChild(
            "meanWindSpeed", f"{parentTag}/surfaceWind/AerodromeSurfaceWindForecast"
        )
        windSpeed.set("uom", "[kn_i]")
        windSpeed.text = str(speed)

        if windGust is not None:
            windGustSpeed = self._addChild(
                "windGustSpeed", f"{parentTag}/surfaceWind/AerodromeSurfaceWindForecast"
            )
            windGustSpeed.set("uom", "[kn_i]")
            windGustSpeed.text = str(windGust)

    def _resultWeather(self, tag, parentTag, weather):
        nonVicinityTypes = weather.regularWeatherTypes + weather.obstructionWeatherTypes
        for (index, tafWeatherType) in enumerate(nonVicinityTypes):

            if index == 0:
                if weather.maxIntensity is not None:
                    # Clean up certain weather intensities in the text
                    weather.maxIntensity = weather.maxIntensity.replace("<NoInten>", "")
                    weather.maxIntensity = weather.maxIntensity.replace("--", "-")
                    weather.maxIntensity = weather.maxIntensity.replace("m", "")

                    tafWeatherType = weather.maxIntensity + tafWeatherType

            weatherUri = f"http://codes.wmo.int/306/4678/{tafWeatherType}"

            description = self._weatherUriToDescription.get(weatherUri, None)
            if description is not None:
                weatherElement = self._addChild("weather", parentTag)
                weatherElement.set("xlink:href", weatherUri)
                weatherElement.set("xlink:title", description)

        if weather.vicinityWeather is not None:
            weatherUri = f"http://codes.wmo.int/306/4678/{weather.vicinityWeather}"
            description = self._weatherUriToDescription.get(weatherUri, None)

            weatherElement = self._addChild("weather", parentTag)
            weatherElement.set(
                "xlink:href", f"http://codes.wmo.int/306/4678/{weather.vicinityWeather}"
            )
            weatherElement.set("xlink:title", description)

    def _resultClouds(self, tag, parentTag, clouds, weather):
        self._addChild("cloud", parentTag)
        cloudForecast = self._addChild("AerodromeCloudForecast", f"{parentTag}/cloud")
        cloudForecast.set("gml:id", self._randomUuidString())

        if clouds == "SKC":
            self._addChild("layer", f"{parentTag}/cloud/AerodromeCloudForecast")
            self._addChild("CloudLayer", f"{parentTag}/cloud/AerodromeCloudForecast/layer")
            amount = self._addChild(
                "amount", f"{parentTag}/cloud/AerodromeCloudForecast/layer/CloudLayer"
            )
            amount.set("xlink:href", "http://codes.wmo.int/bufr4/codeflag/0-20-008/0")
            amount.set("xlink:title", "Clear")
            base = self._addChild(
                "base", f"{parentTag}/cloud/AerodromeCloudForecast/layer/CloudLayer"
            )
            base.set("nilReason", "inapplicable")
            base.set("uom", "N/A")
            base.set("xsi:nil", "true")
        else:
            for (index, (coverage, height)) in enumerate(clouds):
                self._addChild("layer", f"{parentTag}/cloud/AerodromeCloudForecast")
                self._addChild("CloudLayer", f"{parentTag}/cloud/AerodromeCloudForecast/layer")
                amount = self._addChild(
                    "amount", f"{parentTag}/cloud/AerodromeCloudForecast/layer/CloudLayer"
                )
                amount.set("xlink:href", self._coverageHref[coverage])
                amount.set("xlink:title", self._coverageTitle[coverage])
                base = self._addChild(
                    "base", f"{parentTag}/cloud/AerodromeCloudForecast/layer/CloudLayer"
                )
                base.set("uom", "[ft_i]")
                base.text = str(height * 100)  # height was in 100s feet units
                if index == 0 and weather is not None and weather.thunderstormsPresent():

                    type = self._addChild(
                        "cloudType", f"{parentTag}/cloud/AerodromeCloudForecast/layer/CloudLayer"
                    )
                    type.set("xlink:href", "http://codes.wmo.int/bufr4/codeflag/0-20-012/9")
                    type.set("xlink:title", "Cumulonimbus")

    def _resultLLWS(self, tag, parentTag, llws):
        (direction, speed, height) = llws

        self._addChild("extension", parentTag)
        recordExtension = self._addChild(
            "MeteorologicalAerodromeForecastRecordExtension", f"{parentTag}/extension"
        )
        recordExtension.set("xmlns", "http://nws.weather.gov/iwxxm-us/2.0")
        recordExtension.set(
            "xsi:schemaLocation",
            f"http://nws.weather.gov/iwxxm-us/2.0 "
            f"http://nws.weather.gov/schemas/IWXXM-US/2.0/Release/schemas/usTaf.xsd",
        )
        self._addChild(
            "nonConvectiveLLWS",
            f"{parentTag}/extension/MeteorologicalAerodromeForecastRecordExtension",
        )
        windDirection = self._addChild(
            "lowLevelWindShearWindDirection",
            f"{parentTag}/extension/MeteorologicalAerodromeForecastRecordExtension/nonConvectiveLLWS",
        )
        windDirection.set("uom", "deg")
        windDirection.text = str(direction)
        windSpeed = self._addChild(
            "lowLevelWindShearWindSpeed",
            f"{parentTag}/extension/MeteorologicalAerodromeForecastRecordExtension/nonConvectiveLLWS",
        )
        windSpeed.set("uom", "[kn_i]")
        windSpeed.text = str(speed)
        self._addChild(
            "layerAboveAerodrome",
            f"{parentTag}/extension/MeteorologicalAerodromeForecastRecordExtension/nonConvectiveLLWS",
        )
        lowerLimit = self._addChild(
            "lowerLimit",
            f"{parentTag}/extension/MeteorologicalAerodromeForecastRecordExtension/"
            f"nonConvectiveLLWS/layerAboveAerodrome",
        )
        lowerLimit.set("uom", "[ft_i]")
        lowerLimit.text = "0"
        upperLimit = self._addChild(
            "upperLimit",
            f"{parentTag}/extension/MeteorologicalAerodromeForecastRecordExtension/"
            f"nonConvectiveLLWS/layerAboveAerodrome",
        )
        upperLimit.set("uom", "[ft_i]")
        upperLimit.text = str(height * 100)  # height was in 100s feet units

    class tag:
        def __init__(
            self, tagName, parentTag=None, namespace=None, method=None, productKey=None, value=None
        ):
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
                tag.parentTag = re.sub("xForecast", "baseForecast", tag.parentTag)
        elif self._forecastIndex > 0:
            tag.tagName = re.sub("xForecast", "changeForecast", tag.tagName)

            if tag.parentTag is not None:
                tag.parentTag = re.sub("xForecast", "changeForecast", tag.parentTag)

        if tag.method is not None:
            if groupDict is not None:
                tag.method(tag, productDict, airportDict, groupDict)
            else:
                tag.method(tag, productDict, airportDict)

        else:
            subelement = self._addChild(tag.tagName, tag.parentTag)

            value = None
            if tag.productKey is not None:
                value = self._findDictValue(productDict, airportDict, groupDict, tag.productKey)

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

        for airportDict in list(airportDicts.values()):
            # Create the root node
            self._taf = self._tafRoot()

            self._forecastIndex = None

            # Create the main IWXXM tags
            for tag in self._iwxxmTags():
                self._createTagXML(tag, productDict, airportDict)

            # Get the FM groups for this airport TAF
            fmGroups = airportDict["fmGroups"]

            if not fmGroups:
                # Create a NIL forecast to indicate there are no FM groups
                # TODO: reorder things because not all _iwxxmTags used for this
                self._createNilBaseForecast(productDict, airportDict)

            # Create the forecasts
            self._forecastIndex = 0
            for fmGroup in fmGroups:
                fmGroupDict = fmGroup.productDict

                for tag in self._forecastTags():
                    self._createTagXML(tag, productDict, airportDict, fmGroupDict)

                self._forecastIndex += 1

                if fmGroupDict["conditionalGroup"] is not None:
                    groupDict = fmGroupDict["conditionalGroup"].productDict

                    for tag in self._forecastTags():
                        self._createTagXML(tag, productDict, airportDict, groupDict)

                    self._forecastIndex += 1

            # Turn the XML into a pretty formatted string
            tafXmlText = tostring(self._taf)
            tafXmlText = re.sub(r"([a-zA-Z])# ", r"\1:", tafXmlText)

            tafXmlText = str(parseString(tafXmlText).toprettyxml())

            airportXMLs[airportDict["icaoAirportIndicator"]] = tafXmlText

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
                xmlOutputFile = f"{os.path.join(xmlOutputDirectory, airportIcaoId)}.xml"

                if not os.path.exists(xmlOutputDirectory):
                    os.makedirs(xmlOutputDirectory)

                with open(xmlOutputFile, "w") as xmlFile:
                    xmlFile.writelines(xmlOutput)

            except Exception as e:
                self._textProduct.debug_print(e, 1)

        for (airportIcaoId, xmlOutput) in fcsts.items():

            # Write the file using the LocalizationSupport
            relativeFilePath = f"aviation/tmp/{airportIcaoId}.xml"

            # Write this file into the Localization store - needs to be generalized for all sites
            LocalizationSupport.writeFile(
                LocalizationSupport.COMMON_STATIC,
                LocalizationSupport.SITE,
                siteID,
                relativeFilePath,
                xmlOutput,
            )
        return

    def _createNilBaseForecast(self, productDict, airportDict):
        # TODO: Add Support for NIL XML TAFS (Example 4)
        pass

    def _randomUuidString(self):
        return f"uuid.{uuid4()}"