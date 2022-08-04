##
# This is a base file that is not intended to be overridden.
##
# Creates Hurricane Local Statement (HLS) Product.
#
# SOFTWARE HISTORY
#  Date       Ticket#    Engineer        Description
#  ---------- ---------- -----------     --------------------------
#  07/13/2018 DCS20374   mgamazaychikov  Set the default to manual entry for the stormInfoOptions.
#  12/21/2018 DR20994    smoorthy        Added Shannon White's regex fix to mndSearch
#  02/20/2020 DCS20651   mbelk           Added ability to use previous HLS text, when possible
#  02/28/2020 DCS20651   mbelk           Reworked methods to load and use HLS advisory JSON
#  03/09/2020 DCS20651   mbelk           Bug fixes working with HLS advisory JSON
#  03/11/2020 DCS20651   mbelk           Modifications as part of code review
#  03/13/2020 DCS20651   mbelk           Bug fixes to determine if previous text can be used
#  03/16/2020 DCS20651   mbelk           Bug fixes to remove extraneous line feeds.
#                                        Minor code cleanup.
#  10/08/2020 DR 22279   mbelk           Fix for Dispel Rumors bug
#  10/21/2020 DR22212    bhurley         Added "issued by" line for when in service backup 
#  11/03/2020 DR22201    bhurley         Addressed problem in DR "GFE - TCV formatter fails due to TCP parsing when header is broken into two lines"
#                                        due to TCP parsing when header is broken into two lines"
#  12/01/2020 8293       randerso        Fix refLimit check that never worked correctly in Python2
#                                        and now throws exceptions in Python3
#  12/01/2020 8293       randerso        Fix handling of Nones in decidingField logic
#  06/28/2021 DR22698    mbelk           Fixed problem with HLS failing for intermediate advisories 
#
# Version 2020.10.08-0

from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DBit as JavaGrid2DBit
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData, ReferenceID
import EditAreaUtils
import GenericHazards
import HLSTCV_Common
import JsonSupport
import LocalizationSupport
import LogStream
import ModuleAccessor
import VTECTable
import copy
import functools
import math
import os
import re
import time
import tkinter



class TextProduct(HLSTCV_Common.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition["displayName"] = "None"
    Definition["outputFile"] = "{prddir}/TEXT/HLS.txt"
    Definition["database"] = "Official"  # Source database
    Definition["mapNameForCombinations"] = "Zones_<site>"
    Definition["defaultEditAreas"] = "Combinations_HLS_<site>"
    Definition["showZoneCombiner"] = 1  # 1 to cause zone combiner to display

    Definition["productName"] = "Local Statement"

    Definition["fullStationID"] = "<fullStationID>"
    Definition["wmoID"]         = "<wmoID>"
    Definition["wfoCityState"]  = "<wfoCityState>"
    Definition["pil"]           = "<pil>"
    Definition["textdbPil"]     = "<textdbPil>"
    Definition["awipsWANPil"]   = "<awipsWANPil>"
    Definition["site"]          = "<site>"
    Definition["wfoCity"]       = "<wfoCity>"

    Definition["areaName"]       = ""  # optional area name for product
    Definition["areaDictionary"] = "AreaDictionary"
    Definition["language"]       = "english"
    Definition["lineLength"]     = 71  # Maximum line length
    Definition["tabLength"]      = 4

    Definition["purgeTime"]        = 8  # Default Expiration in hours if
    Definition["includeZoneNames"] = 1  # Zone names will be included in the area header
    Definition["includeIssueTime"] = 0  # Issue Time will be included in the area header
    # Optional EAS phrase to be include in product header
    Definition["easPhrase"] = "URGENT - IMMEDIATE BROADCAST REQUESTED"
    Definition["callToAction"] = 1

    # Add options for debugging
    Definition["debug"] = {
        # TextProduct
        "__init__": 0,
        "_inlandAreas": 0,
        "_coastalAreas": 0,
        "_cwa": 0,
        "_cwa_descriptor": 0,
        "_localReferencePoints": 0,
        "_localReferencePoints_defaults": 0,
        "_referencePointLimit": 0,
        "_productParts_HLS": 0,
        "_analysisList_HLS": 0,
        "_analysisList_HLS_WholeDomain": 0,
        "_intersectAnalysisList_HLS": 0,
        "generateForecast": 0,
        "_initializeVariables": 0,
        "_initializeHeadlines": 0,
        "_initializeSamplingDict": 0,
        "_noOpParts": 0,
        "_areaList": 0,
        "_summaryHeadlines": 0,
        "_changesHazards": 0,
        "_currentHazards": 0,
        "_stormInformation": 0,
        "_situationOverview": 0,
        "_windSection": 0,
        "_surgeSection": 0,
        "_floodingRainSection": 0,
        "_tornadoSection": 0,
        "_coastalHazardsSection": 0,
        "_preparednessSection": 0,
        "_evacuationStatements": 0,
        "_otherPreparednessActions": 0,
        "_additionalSourcesInfo": 0,
        "_nextUpdate": 0,
        "_impactsKeyFunction": 0,
        "_getPotentialImpactsStatements": 0,
        "_impactCategoryToThreatLevel": 0,
        "_determineHazardStates": 0,
        "_sampleHLSData": 0,
        "_sampleTCVAdvisory": 0,
        "_sampleRankedDiscreteValue": 0,
        "_sampleMostSignificantDiscreteValue": 0,
        "_getDominantThreatLevel": 0,
        "_getHighestThreat": 0,
        "_getLowestThreat": 0,
        "_setHazardImpactCategories": 0,
        "_createWholeDomainEditArea": 0,
        "_determineHazards": 0,
        "_formatLocalTime": 0,
        "_getTimeZoneList": 0,
        "_grabHeadline": 0,
        "_getStormInfo": 0,
        "_grabStormInfo": 0,
        "_decodeStormInfo": 0,
        "_expandBearings": 0,
        "_removeKM": 0,
        "_cleanText": 0,
        "_calcLocalReferences": 0,
        "_calcReference": 0,
        "_distanceFromLatLon": 0,
        "_bearing": 0,
        "_dirInEnglish": 0,
        "_overview_list": 0,
        "_displayGUI": 0,
        "_frame": 0,
        "_getPreviousAdvisories": 0,
        "_getPreviousHlsAdvisories": 0,
        "_applyPreviousText": 0,
        "_determinePreviousTextUse": 0,
        "_getPreviousHlsText": 0,
        "_cleanupAdvisories": 0,

        # HLSTCV_Common
        "allowedHazards": 0,
        "allowedHeadlines": 0,
        "_initializeVariables": 0,
        "moderated_dict": 0,
        "_wmoHeader": 0,
        "_productHeader": 0,
        "_ugcHeader": 0,
        "_getPartMethod": 0,
        "_processProductParts": 0,
        "_createProductDictionary": 0,
        "_initializeProductDictionary": 0,
        "_formatProductDictionary": 0,
        "_getStatValue": 0,
        "_allAreas": 0,
        "_groupSegments": 0,
        "_getSegmentVTECRecordsTuples": 0,
        "_computeIntersectAreas": 0,
        "_initializeHazardsTable": 0,
        "_getHazardsTable": 0,
        "_ignoreActions": 0,
        "_setVTECActiveTable": 0,
        "_getVtecRecords": 0,
        "_getAllowedHazardList": 0,
        "_altFilterMethod": 0,
        "_filterHazards": 0,
        "_getAdditionalHazards": 0,
        "_checkHazard": 0,
        "_initializeTimeVariables": 0,
        "_determineTimeRanges": 0,
        "_createPeriodList": 0,
        "_calculateStartTime": 0,
        "_formatPeriod": 0,
        "_getTimeDesc": 0,
        "_getPartOfDay": 0,
        "_initializeStormInformation": 0,
        "_parseTCP": 0,
        "_getStormTypeFromTCP": 0,
        "_getStormNameFromTCP": 0,
        "_getAdvisoryTypeFromTCP": 0,
        "_getAdvisoryNumberStringFromTCP": 0,
        "_getStormNumberStringFromTCP": 0,
        "_getStormIDStringFromTCP": 0,
        "_useTestTCP": 0,
        "_testTCP": 0,
        "_initializeAdvisories": 0,
        "_synchronizeAdvisories": 0,
        "_getLocalAdvisoryDirectoryPath": 0,
        "_getStormAdvisoryNames": 0,
        "_loadLastTwoAdvisories": 0,
        "_loadAdvisory": 0,
        "_getAdvisoryPath": 0,
        "_getAdvisoryFilename": 0,
        "_processVariableList": 0,
        "_GUI_sizing_dict": 0,
        "_GUI1_configDict": 0,
        "_font_GUI_dict": 0,

        # Overview_Dialog
        "body": 0,
        "_makeStep3": 0,
        "_makeButtons": 0,
        "okCB": 0,

        # Common_Dialog
        "getVarDict": 0,
        "_makeRadioOrCheckList": 0,
        "_makeEntry": 0,
        "cancelCB": 0,
        "_entryName": 0,
        "_makeTuple": 0,
        "_setVarDict": 0,
        "status": 0,
        "buttonbox": 0,

        # LegacyFormatter
        "execute": 0,
        "_processProductParts": 0,
        "processWmoHeader": 0,
        "processProductHeader": 0,
        "processSummaryHeadlines": 0,
        "processHazards": 1,
        "_addToGroupedHazards": 1,
        "_sortHazardsType": 0,
        "_consolidateGroupedHazards": 1,
        "_createHazardTextParts": 0,
        "_areaWords": 0,
        "processStormInformation": 0,
        "processSituationOverview": 0,
        "processHazardsSection": 0,
        "processSubParts": 0,

        # TextProductCommon
        "setUp": 0,
        "hazardTimeZones": 0,
        "getExpireTime": 0,
        "getHeadlinesAndSections": 0,
        "formatUGCs": 0,
        "getFormattedTime": 0,
        "formatUGC_names": 0,
        "formatNameString": 0,
        "getVal": 0,
        "formatDatetime": 0,
        "flush": 0,
        "makeUGCString": 0,
        "checkLastArrow": 0,
    }

#     Definition["debug"] = 1  # turn on ALL debug messages
    Definition["debug"] = 0  # turn off ALL debug messages

    def __init__(self):
        HLSTCV_Common.TextProduct.__init__(self)

    #####################################################################
    #####################################################################
    ### Organization of Formatter Code

    ###############################################################
    ###  MUST OVERRIDE DEFINITIONS !!!
    ###    _inlandAreas, _coastalAreas, _cwa, _cwa_descriptor,
    ###    _localReferencePoints, _localReferencePoints_defaults
    ###############################################################

    ###############################################################
    ###  Optional Overrides
    ###    _referencePointLimit
    ###############################################################

    ###############################################################
    ### HLS Product and Segment Parts Definition
    ###############################################################

    ###############################################################
    ### Analysis Lists, SampleAnalysis Overrides and other
    ###   analysis related methods
    ###############################################################

    ###############################################################
    ###  CODE
    ###############################################################
    ###  High level flow of formatter
    ###    generateForecast, _initializeVariables,
    ###    _loadLastTwoAdvisories, _determineTimeRanges,
    ###    _initializeSamplingDict, _sampleTCVAdvisory,
    ###    _sampleHLSData, _determineHazardStates,
    ###    _setHazardImpactCategories, _createProductDictionary,
    ###    _formatProductDictionary
    ###############################################################

    ###############################################################
    ### Product Parts Implementation
    ###############################################################

    ###############################################################
    ### Sampling and Statistics related methods
    ###############################################################

    ###############################################################
    ### Area, Zone and Segment related methods
    ###############################################################

    ###############################################################
    ### Hazards related methods
    ###############################################################

    ###############################################################
    ### Time related methods
    ###############################################################

    ###############################################################
    ### Storm Information and TCP related methods
    ###############################################################

    ###############################################################
    ### GUI related methods
    ###############################################################

    ###############################################################
    ###  MUST OVERRIDE DEFINITIONS !!!

    def _inlandAreas(self):
        return [
            # "FLZ063", "FLZ066", "FLZ067", "FLZ068", "FLZ070",
            # "FLZ071", "FLZ072", "FLZ073", "FLZ074",
        ]

    def _coastalAreas(self):
        return [
            # "FLZ069", "FLZ075", "FLZ168", "FLZ172", "FLZ173", "FLZ174",
        ]

    def _cwa(self):
        return ""  # "MFL"

    def _cwa_descriptor(self):
        return ""  # "South Florida"

    def _localReferencePoints(self):
        # Give the name and lat/lon for each local reference point
        return [
            # ("West Palm Beach, FL", (26.71, -80.06)),
            # ("Fort Lauderdale, FL", (26.12, -80.15)),
            # ("Miami, FL", (25.77, -80.20)),
            # ("Miami Beach, FL", (25.81, -80.13)),
            # ("Naples, FL", (26.14, -81.80)),
            # ("Marco Island, FL", (25.94, -81.73)),
        ]

    def _localReferencePoints_defaults(self):
        # Give a list of the local reference point names to be
        # turned on by default
        return []  # ["Miami, FL", "Naples, FL"]

    ###############################################################
    ###  Optional Overrides

    def _referencePointLimit(self):
        # Give the number of reference points allowed to be chosen
        # Also give a label (e.g. "two") for the GUI
        return (2, "two")

    ###############################################################
    ### HLS Product and Segment Parts Definition

    def _productParts_HLS(self, segment_vtecRecords_tuples):
        partsList = [
            "wmoHeader",
            "ugcHeader",
            "productHeader",
            "areaList",
            "summaryHeadlines",
            "newInformationHeader",
            "changesHazards",
            "currentHazards",
            "stormInformation",
            "situationOverview",
            "sigPotentialImpacts",
        ]

        if self._ImpactsAnticipated:
            includedImpacts = sorted(self._IncludedImpacts, key=self._impactsKeyFunction)
            for ((_, sectionName), _) in includedImpacts:
                self.debug_print("adding section = '%s'" % (sectionName), 1)
                partsList.append(sectionName)

        partsList.append("preparednessSection")

        if self._ImpactsAnticipated:
            partsList.append("evacuationStatements")
            partsList.append("otherPreparednessActions")
            partsList.append("additionalSourcesInfo")

        partsList.append("nextUpdate")
        partsList.append("endProduct")

        self.debug_print("Product Parts partsList =\n\n%s\n" %
                         (self._pp.pformat(partsList)), 1)

        return {"partsList": partsList}

    ###############################################################
    ### Analysis Lists, SampleAnalysis Overrides and other
    ###   analysis related methods

    def _analysisList_HLS(self):
        # Sample over 120 hours beginning at current time
        analysisList = [
            # Wind Section
            ("WindThreat", self.rankedDiscreteValue),
            ("WindThreat", self.mostSignificantDiscreteValue),

            # Flooding Rain Section
            ("QPFtoFFGRatio", self.moderatedMax, [6]),
            ("FloodingRainThreat", self.rankedDiscreteValue),
            ("FloodingRainThreat", self.mostSignificantDiscreteValue),

            # Tornado Section
            ("TornadoThreat", self.rankedDiscreteValue),
            ("TornadoThreat", self.mostSignificantDiscreteValue),
        ]

        return analysisList

    def _analysisList_HLS_WholeDomain(self):
        # Sample over 120 hours beginning at current time
        analysisList = [
            # Wind Section
            ("Wind", self.vectorModeratedMax, [6]),
        ]

        return analysisList

    def _intersectAnalysisList_HLS(self):
        # The grids for the Surge Section will be intersected with a special edit area
        analysisList = [
            ("InundationMax", self.moderatedMax, [6]),
            ("StormSurgeThreat", self.rankedDiscreteValue),
            ("StormSurgeThreat", self.mostSignificantDiscreteValue),
        ]

        return analysisList

    ###############################################################
    ###  High level flow of formatter

    def generateForecast(self, argDict):
        # Generate Text Phrases for a list of edit areas

        error = self._initializeVariables(argDict)
        if error is not None:
            return error

        if self._stormName is None or self._stormName == "":
            return "Could not determine the storm name"

        if self._ImpactsAnticipated:
            self._loadLastTwoAdvisories()
            if not self._previousAdvisory or not self._previousAdvisoryMatchesNumber:
                return "A TCV must be transmitted before an HLS can be run"

        if not self._IncludedImpacts:
            return "At least one potential impact section needs to be included."

        # Determine time ranges
        self._determineTimeRanges(argDict)

        if self._ImpactsAnticipated:

            # Sample the data
            self._initializeSamplingDict()
            self._sampleTCVAdvisory(self._previousAdvisory)
            self._sampleHLSData(argDict)

            self._determineHazardStates()

            for threatName in [
                "WindThreat",
                "StormSurgeThreat",
                "FloodingRainThreat",
                "TornadoThreat",
            ]:
                self._setHazardImpactCategories(threatName)

            self._archiveCurrentAdvisory()

        # Create the product dictionary and format it to create the output
        productDict = self._createProductDictionary(self._productParts_HLS,
                                                    self._allAreas(),
                                                    areProductPartsSegmented=False)

        # See if we should get the previous text
        if self._PreviousText:
            productDict = self._applyPreviousText(productDict)
            self.debug_print("productDict updated with previous text, where possible")

        productOutput = self._formatProductDictionary(LegacyFormatter, productDict)

        return productOutput

    def _initializeVariables(self, argDict):
        error = HLSTCV_Common.TextProduct._initializeVariables(self, argDict)
        if error is not None:
            return error

        self._getStormInfo(argDict)

        self._initializeHeadlines()

        #=======================================================================
        # Now produce a UGC header using only the WFO selected zones
        #=======================================================================

        # Get the Combinations file for the HLS
        accessor = ModuleAccessor.ModuleAccessor()
        self.debug_print("self._defaultEditAreas = %s" %
                         (self._pp.pformat(self._defaultEditAreas)), 1)

        # combos is a list of tuples. Each tuple is a grouping of zones
        # (a list of zones, combo name).
        combos = accessor.variable(self._defaultEditAreas, "Combinations")

        # If we could not find a Combinations file for the HLS
        if combos is None:
            LogStream.logVerbose(
                "Combination file not found: " + self._pp.pformat(self._defaultEditAreas)
            )

            # Default to using the entire CWA
            self._ugcs = sorted(self._allAreas())

        # Otherwise, construct the final list of WFO selected zones
        else:
            self.debug_print(
                "Segments from Zone Combiner = %s" % (self._pp.pformat(combos)), 1)

            # Create a list containing all zones from all combination groups
            selectedZones = functools.reduce(lambda zones, combo: zones + combo[0],
                                   combos,
                                   [])

            # Use the selected zones for the UGC header
            self._ugcs = sorted(selectedZones)

        self.debug_print("Final Zones for UGC header = %s" % (self._pp.pformat(self._ugcs)), 1)

        return None

    def _initializeHeadlines(self):
        if self._MainHeadline == "Enter":
            self._headlines = [self._MainHeadline_entry]
        elif self._MainHeadline == "UsePrev":
            self._prevHLS = self.getPreviousProduct(self._textdbPil)
            self._headlines = [self._grabHeadline(self._prevHLS)]
        elif self._MainHeadline == "UseTCP":
            try:
                self._headlines = [self._grabHeadline(self._TCP)]
            except BaseException:
                self._headlines = []

    def _initializeSamplingDict(self):
        self._samplingDict = dict()
        statsDict = dict()
        statsDict["catastrophicThreshold"] = None
        statsDict["decidingField"] = None
        statsDict["inputThreatLow"] = None
        statsDict["inputThreatHigh"] = None
        statsDict["inputThreatDominant"] = None
        statsDict["impactMin"] = None
        statsDict["impactMax"] = None
        statsDict["impactRange"] = None
        statsDict["impactRangeMax"] = None

        self._samplingDict["WindThreat"] = copy.copy(statsDict)
        self._samplingDict["StormSurgeThreat"] = copy.copy(statsDict)
        self._samplingDict["FloodingRainThreat"] = copy.copy(statsDict)
        self._samplingDict["TornadoThreat"] = copy.copy(statsDict)

        self._samplingDict["WindThreat"]["catastrophicThreshold"] = 137  # knots
        self._samplingDict["StormSurgeThreat"]["catastrophicThreshold"] = 14  # feet
        self._samplingDict["FloodingRainThreat"]["catastrophicThreshold"] = 3  # percent

    ###############################################################
    ### Product Parts Implementation

    def _noOpParts(self):
        """
        These represent product parts that should be skipped when calling product part methods.
        They will be handled automatically by the formatters.
        """
        return [
            "CR",
            "endProduct",
            "endSegment",
            "doubleAmpersand",
            "newInformationHeader",
            "sigPotentialImpacts",
        ]

    ################# Product Level

    def _areaList(self, productDict, productSegmentGroup, productSegment):
        productDict["areaList"] = "This product covers " + self._cwa_descriptor()

    def _summaryHeadlines(self, productDict, productSegmentGroup, productSegment):
        productDict["summaryHeadlines"] = self._headlines

    def _changesHazards(self, productDict, productSegmentGroup, productSegment):
        if (not self._ImpactsAnticipated) or \
           (self._ImpactsAnticipated and self._GeneralOnsetTime == "recovery"):
            productDict["changesHazards"] = []
        else:
            productDict["changesHazards"] = self._changesHazardsList

    def _currentHazards(self, productDict, productSegmentGroup, productSegment):
        if (not self._ImpactsAnticipated) or \
           (self._ImpactsAnticipated and self._GeneralOnsetTime == "recovery"):
            productDict["currentHazards"] = []
        else:
            productDict["currentHazards"] = self._currentHazardsList

    def _stormInformation(self, productDict, productSegmentGroup, productSegment):
        stormInfoDict = dict()
        if self._ImpactsAnticipated:
            stormInfoDict["references"] = self._stormLocalReferences
            stormInfoDict["location"] = self._stormLocation
            stormInfoDict["intensity"] = self._stormIntensityTrend
            stormInfoDict["movement"] = self._stormMovementTrend
        productDict["stormInformation"] = stormInfoDict

    def _situationOverview(self, productDict, productSegmentGroup, productSegment):
        # Use generic text for the situation overview
        productDict["situationOverview"] = self._frame(
            "Succinctly describe the expected evolution of the event for the CWA; "
            + "which hazards are of greater (or lesser) concern, forecast focus, etc."
        )

        # Get the WRKHLS product minus header that has the situation overview we want
        wrkhlsProduct = self.getPreviousProduct("WRKHLS")[40:]

        # If we found the overview
        if len(wrkhlsProduct) > 0:
            # Clean and frame the imported overview and use it instead of the generic text
            productDict["situationOverview"] = self._frame(
                self._cleanText(wrkhlsProduct.strip())
            )

    def _windSection(self, productDict, productSegmentGroup, productSegment):
        sectionDict = dict()
        sectionDict["title"] = "Wind"
        sectionDict["impactRange"] = ""
        sectionDict["impactLib"] = []
        sectionDict["additionalImpactRange"] = []

        impactMin = self._samplingDict["WindThreat"]["impactMin"]
        impactMax = self._samplingDict["WindThreat"]["impactMax"]
        impactRange = self._samplingDict["WindThreat"]["impactRange"]
        impactRangeMax = self._samplingDict["WindThreat"]["impactRangeMax"]
        inputThreatDominant = self._samplingDict["WindThreat"]["inputThreatDominant"]

        # Test the simplest case first
        if impactMin == "none" and impactMax == "none":
            sectionDict["impactRange"] = impactRange
            productDict["windSection"] = sectionDict
            return

        qualifier = self._getImpactsQualifier(impactMax)

        # If there is only one impact across the entire CWA, and it is the max
        if impactMax != "none" and impactMin == impactMax and inputThreatDominant != "None":
            if self._GeneralOnsetTime == "check plans":
                sectionDict["impactRange"] = (
                    "Prepare for "
                    + qualifier
                    + "wind having possible "
                    + impactMax
                    + " impacts across "
                    + self._cwa_descriptor()
                    + ". Potential impacts include:"
                )
            elif self._GeneralOnsetTime == "complete preparations":
                sectionDict["impactRange"] = (
                    "Protect against "
                    + qualifier
                    + "wind having possible "
                    + impactMax
                    + " impacts across "
                    + self._cwa_descriptor()
                    + ". Potential impacts include:"
                )
            elif self._GeneralOnsetTime == "hunker down":
                sectionDict["impactRange"] = (
                    "Potential impacts from the main wind event are now unfolding across "
                    + self._cwa_descriptor()
                    + ". Remain well sheltered from "
                    + qualifier
                    + "wind having "
                    + self._frame("possible | additional")
                    + " "
                    + impactMax
                    + " impacts. If realized, these impacts include:"
                )
            else:
                sectionDict["impactRange"] = "Little to no additional wind impacts expected."
        # Handle the case where the impacts are not the same across the entire CWA
        else:
            if self._GeneralOnsetTime == "check plans":
                sectionDict["impactRange"] = (
                    "Prepare for "
                    + qualifier
                    + "wind having possible "
                    + impactMax
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + ". Potential impacts in this area include:"
                )
            elif self._GeneralOnsetTime == "complete preparations":
                sectionDict["impactRange"] = (
                    "Protect against "
                    + qualifier
                    + "wind having possible "
                    + impactMax
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + ". Potential impacts in this area include:"
                )
            elif self._GeneralOnsetTime == "hunker down":
                sectionDict["impactRange"] = (
                    "Potential impacts from the main wind event are now unfolding across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + ". Remain well sheltered from "
                    + qualifier
                    + "wind having "
                    + self._frame("possible | additional")
                    + " "
                    + impactMax
                    + " impacts. If realized, these impacts include:"
                )
            else:
                sectionDict["impactRange"] = "Little to no additional wind impacts expected."

        if self._GeneralOnsetTime != "recovery":
            sectionDict["impactLib"] = self._getPotentialImpactsStatements(
                "Wind", self._impactCategoryToThreatLevel(impactMax)
            )
        else:
            sectionDict["impactLib"] = [
                "Community officials are now assessing the extent of actual wind impacts accordingly.",
                "Emergency response teams are attending to casualty situations as needed.",
                "Emergency work crews are restoring essential community infrastructure as necessary.",
                "If you have an emergency dial 9 1 1.",
            ]

        # If there are additional areas
        if impactRange != impactMax:
            qualifier = self._getImpactsQualifier(impactRangeMax)

            if self._GeneralOnsetTime == "check plans":
                curPhrase = (
                    "Also, prepare for "
                    + qualifier
                    + "wind having possible "
                    + impactRange
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + "."
                )
            elif self._GeneralOnsetTime == "complete preparations":
                curPhrase = (
                    "Also, protect against "
                    + qualifier
                    + "wind having possible "
                    + impactRange
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + "."
                )
            elif self._GeneralOnsetTime == "hunker down":
                curPhrase = (
                    "Potential impacts from the main wind event are also now unfolding across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + ". Remain well sheltered from "
                    + qualifier
                    + "wind having "
                    + self._frame("possible | additional")
                    + " "
                    + impactRange
                    + " impacts."
                )
            else:
                curPhrase = "Little to no additional wind impacts expected."

            # If this phrase is not already part of the additional impacts
            if curPhrase not in sectionDict["additionalImpactRange"]:

                # Add it now
                sectionDict["additionalImpactRange"].append(curPhrase)

        # If there is no impact across more than one half the area, include a
        # statement for that as well
        if inputThreatDominant == "None":

            curPhrase = (
                "Elsewhere across "
                + self._cwa_descriptor()
                + ", little to no impact is anticipated."
            )

            # If this phrase is not already part of the additional impacts
            if curPhrase not in sectionDict["additionalImpactRange"]:

                # Add it now
                sectionDict["additionalImpactRange"].append(curPhrase)

        productDict["windSection"] = sectionDict

    def _surgeSection(self, productDict, productSegmentGroup, productSegment):
        sectionDict = dict()
        sectionDict["title"] = "Surge"
        sectionDict["impactRange"] = ""
        sectionDict["impactLib"] = []
        sectionDict["additionalImpactRange"] = []
        sectionDict["variedImpacts"] = True

        impactMin = self._samplingDict["StormSurgeThreat"]["impactMin"]
        impactMax = self._samplingDict["StormSurgeThreat"]["impactMax"]
        impactRange = self._samplingDict["StormSurgeThreat"]["impactRange"]
        impactRangeMax = self._samplingDict["StormSurgeThreat"]["impactRangeMax"]
        inputThreatDominant = self._samplingDict["StormSurgeThreat"]["inputThreatDominant"]

        self.debug_print("DEBUG: B4 %s" %
                         (self._pp.pformat(self._samplingDict["StormSurgeThreat"])), 1)

        # Test the simplest case first
        if impactMin == "none" and impactMax == "none":
            sectionDict["impactRange"] = impactRange
            productDict["surgeSection"] = sectionDict
            return

        # See if we need to include the term "life-threatening" surge
        # This corresponds to threat levels of Moderate, High and Extreme
        lifeThreatening = ""

        if impactMax in ["significant", "extensive", "devastating", "catastrophic"]:
            lifeThreatening = "life-threatening "
        elif impactMax == "limited":
            lifeThreatening = "locally hazardous "

        if self._GeneralOnsetTime == "check plans":
            sectionDict["impactRange"] = (
                "Prepare for "
                + lifeThreatening
                + "surge having possible "
                + impactMax
                + " impacts across "
                + self._frame("ENTER AREA DESCRIPTION")
                + ". Potential impacts in this area include:"
            )
        elif self._GeneralOnsetTime == "complete preparations":
            sectionDict["impactRange"] = (
                "Protect against "
                + lifeThreatening
                + "surge having possible "
                + impactMax
                + " impacts across "
                + self._frame("ENTER AREA DESCRIPTION")
                + ". Potential impacts in this area include:"
            )
        elif self._GeneralOnsetTime == "hunker down":
            sectionDict["impactRange"] = (
                "Potential impacts from the main surge event are now unfolding across "
                + self._frame("ENTER AREA DESCRIPTION")
                + ". Remain well away from "
                + lifeThreatening
                + "surge having "
                + self._frame("possible | additional")
                + " "
                + impactMax
                + " impacts. If realized, these impacts include:"
            )
        else:
            sectionDict["impactRange"] = "Little to no additional surge impacts expected."

        if self._GeneralOnsetTime != "recovery":
            sectionDict["impactLib"] = self._getPotentialImpactsStatements(
                "Storm Surge", self._impactCategoryToThreatLevel(impactMax)
            )
        else:
            sectionDict["impactLib"] = [
                "Community officials are now assessing the extent of actual surge impacts accordingly.",
                "Emergency response teams are attending to casualty situations as needed.",
                "Emergency work crews are restoring essential community infrastructure as necessary.",
                "If you have an emergency dial 9 1 1.",
            ]

        # Reexamine the impact range - we need to separate out "life-threatening"
        # surge categories into a separate statement
        impactParts = impactRange.split(" ")

        # Initialize a variable to keep the proper scope.  This will hold any
        # leftover surge categories
        impactRangeRest = ""

        # Look at the high end of the range
        if len(impactParts) == 3 and impactParts[2] in [
                "significant", "extensive", "devastating", "catastrophic"]:
            # We have some "life-threatening" categories we need to split out -
            # check the low end
            if impactParts[0] in ["limited", "none"]:
                # Make a new range to report
                impactRange = "significant"
                impactRangeMax = impactRange

                if impactParts[2] != "significant":
                    impactRange += " to " + impactParts[2]
                    impactRangeMax = impactParts[2]

                impactRangeRest = impactParts[0]

        # Ensure the leftover impact range is set - just in case we need it
        # This should only ever be "limited" in the case of surge under current policy
        elif len(impactParts) == 1:
            impactRangeRest = impactParts[0]

        self.debug_print("DEBUG: impactRange = '%s'  impactMax = '%s'   impactMin = '%s'" %
                         (impactRange, impactMax, impactMin), 1)
        # If there are additional life-threatening surge areas
        if impactRange != impactMax and impactRange != impactMin:

            lifeThreatening = ""

            if impactRangeMax in ["significant", "extensive", "devastating", "catastrophic"]:
                lifeThreatening = "life-threatening "
            elif impactRangeMax == "limited":
                lifeThreatening = "locally hazardous "

            if self._GeneralOnsetTime == "check plans":
                curPhrase = (
                    "Also, prepare for "
                    + lifeThreatening
                    + "surge having possible "
                    + impactRange
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + "."
                )
            elif self._GeneralOnsetTime == "complete preparations":
                curPhrase = (
                    "Also, protect against "
                    + lifeThreatening
                    + "surge having possible "
                    + impactRange
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + "."
                )
            elif self._GeneralOnsetTime == "hunker down":
                curPhrase = (
                    "Potential impacts from the main surge event are also now unfolding across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + ". Remain well away from "
                    + lifeThreatening
                    + "surge having "
                    + self._frame("possible | additional")
                    + " "
                    + impactRange
                    + " impacts."
                )
            else:
                curPhrase = "Little to no additional surge impacts expected."

            self.debug_print("DEBUG: curPhrase = '%s'" % (curPhrase), 1)
            self.debug_print("DEBUG: sectionDict['additionalImpactRange'] = \n'%s'" %
                             (sectionDict["additionalImpactRange"]), 1)
            # If this phrase is not already part of the additional impacts
            if curPhrase not in sectionDict["additionalImpactRange"]:

                # Add it now
                sectionDict["additionalImpactRange"].append(curPhrase)

        # If there are additional areas
        if impactRangeRest != impactMax:

            lifeThreatening = "locally hazardous "

            if self._GeneralOnsetTime == "check plans":
                curPhrase = (
                    "Also, prepare for "
                    + lifeThreatening
                    + "surge having possible "
                    + impactRangeRest
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + "."
                )
            elif self._GeneralOnsetTime == "complete preparations":
                curPhrase = (
                    "Also, protect against "
                    + lifeThreatening
                    + "surge having possible "
                    + impactRangeRest
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + "."
                )
            elif self._GeneralOnsetTime == "hunker down":
                curPhrase = (
                    "Potential impacts from the main surge event are also now unfolding across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + ". Remain well away from "
                    + lifeThreatening
                    + "surge having "
                    + self._frame("possible | additional")
                    + " "
                    + impactRangeRest
                    + " impacts."
                )
            else:
                curPhrase = "Little to no additional surge impacts expected."

            # If this phrase is not already part of the additional impacts
            if curPhrase not in sectionDict["additionalImpactRange"]:

                # Add it now
                sectionDict["additionalImpactRange"].append(curPhrase)

        # If there is no impact across more than one half the area, include a
        # statement for that as well
        if inputThreatDominant == "None":

            curPhrase = (
                "Elsewhere across "
                + self._cwa_descriptor()
                + ", little to no impact is anticipated."
            )

            # If this phrase is not already part of the additional impacts
            if curPhrase not in sectionDict["additionalImpactRange"]:

                # Add it now
                sectionDict["additionalImpactRange"].append(curPhrase)

        self.debug_print("Final Surge sectionDict['additionalImpactRange'] = '%s'" %
                         (sectionDict["additionalImpactRange"]), 1)
        productDict["surgeSection"] = sectionDict

    def _floodingRainSection(self, productDict, productSegmentGroup, productSegment):
        sectionDict = dict()
        sectionDict["title"] = "Flooding Rain"
        sectionDict["impactRange"] = ""
        sectionDict["impactLib"] = []
        sectionDict["additionalImpactRange"] = []
        sectionDict["variedImpacts"] = False

        impactMin = self._samplingDict["FloodingRainThreat"]["impactMin"]
        impactMax = self._samplingDict["FloodingRainThreat"]["impactMax"]
        impactRange = self._samplingDict["FloodingRainThreat"]["impactRange"]
        impactRangeMax = self._samplingDict["FloodingRainThreat"]["impactRangeMax"]
        inputThreatDominant = self._samplingDict["FloodingRainThreat"]["inputThreatDominant"]

        self.debug_print("In _floodingRainSection", 1)
        self.debug_print("_samplingDict = \n\n%s\n" %
                         (self._pp.pformat(self._samplingDict["FloodingRainThreat"])), 1)

        # Test the simplest case first
        if impactMin == "none" and impactMax == "none":
            sectionDict["impactRange"] = impactRange
            productDict["floodingRainSection"] = sectionDict
            return

        qualifier = ""
        if impactMax in ["extensive", "devastating", "catastrophic"]:
            qualifier = "life-threatening "
        elif impactMax == "significant":
            qualifier = "dangerous "
        elif impactMax == "limited":
            qualifier = "locally hazardous "

        # If there is only one impact across the entire CWA, and it is the max
        if impactMax != "none" and impactMin == impactMax and inputThreatDominant != "None":
            if self._GeneralOnsetTime == "check plans":
                sectionDict["impactRange"] = (
                    "Prepare for "
                    + qualifier
                    + "rainfall flooding having possible "
                    + impactMax
                    + " impacts across "
                    + self._cwa_descriptor()
                    + ". Potential impacts include:"
                )
            elif self._GeneralOnsetTime == "complete preparations":
                sectionDict["impactRange"] = (
                    "Protect against "
                    + qualifier
                    + "rainfall flooding having possible "
                    + impactMax
                    + " impacts across "
                    + self._cwa_descriptor()
                    + ". Potential impacts include:"
                )
            elif self._GeneralOnsetTime == "hunker down":
                sectionDict["impactRange"] = (
                    "Potential impacts from the flooding rain are still unfolding across "
                    + self._cwa_descriptor()
                    + ". Remain well guarded against "
                    + qualifier
                    + "flood waters having "
                    + self._frame("possible | additional")
                    + " "
                    + impactMax
                    + " impacts. If realized, these impacts include:"
                )
            else:
                sectionDict["impactRange"] = (
                    "Additional impacts from flooding rain are still a concern across "
                    + self._cwa_descriptor()
                    + ". Remain well guarded against "
                    + qualifier
                    + "flood waters having further impacts of "
                    + impactMax
                    + " potential."
                )
        # Handle the case where the impacts are not the same across the entire CWA
        else:
            if self._GeneralOnsetTime == "check plans":
                sectionDict["impactRange"] = (
                    "Prepare for "
                    + qualifier
                    + "rainfall flooding having possible "
                    + impactMax
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + ". Potential impacts include:"
                )
            elif self._GeneralOnsetTime == "complete preparations":
                sectionDict["impactRange"] = (
                    "Protect against "
                    + qualifier
                    + "rainfall flooding having possible "
                    + impactMax
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + ". Potential impacts include:"
                )
            elif self._GeneralOnsetTime == "hunker down":
                sectionDict["impactRange"] = (
                    "Potential impacts from the flooding rain are still unfolding across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + ". Remain well guarded against "
                    + qualifier
                    + "flood waters having "
                    + self._frame("possible | additional")
                    + " "
                    + impactMax
                    + " impacts. If realized, these impacts include:"
                )
            else:
                if impactMax != "none":
                    sectionDict["impactRange"] = (
                        "Additional impacts from flooding rain are still a concern across "
                        + self._frame("ENTER AREA DESCRIPTION")
                        + ". Remain well guarded against "
                        + qualifier
                        + "flood waters having further impacts of "
                        + impactMax
                        + " potential."
                    )
                else:
                    sectionDict[
                        "impactRange"
                    ] = "Little to no additional impacts expected from flooding rain."

        if self._GeneralOnsetTime != "recovery":
            sectionDict["impactLib"] = self._getPotentialImpactsStatements(
                "Flooding Rain", self._impactCategoryToThreatLevel(impactMax)
            )
        else:
            sectionDict["impactLib"] = []

        # If there are additional areas
        if impactRange != impactMax:

            qualifier = ""
            if impactRangeMax in ["extensive", "devastating", "catastrophic"]:
                qualifier = "life-threatening "
            elif impactRangeMax == "significant":
                qualifier = "dangerous "
            elif impactRangeMax == "limited":
                qualifier = "locally hazardous "

            if self._GeneralOnsetTime == "check plans":
                curPhrase = (
                    "Prepare for "
                    + qualifier
                    + "rainfall flooding having possible "
                    + impactRange
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + "."
                )
            elif self._GeneralOnsetTime == "complete preparations":
                curPhrase = (
                    "Protect against "
                    + qualifier
                    + "rainfall flooding having possible "
                    + impactRange
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + "."
                )
            elif self._GeneralOnsetTime == "hunker down":
                curPhrase = (
                    "Potential impacts from the flooding rain are still unfolding across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + ". Remain well guarded against "
                    + qualifier
                    + "flood waters having "
                    + self._frame("possible | additional")
                    + " "
                    + impactRange
                    + " impacts."
                )
            else:
                if impactMax != "none":
                    curPhrase = (
                        "Additional impacts from flooding rain are still a concern across "
                        + self._frame("ENTER AREA DESCRIPTION")
                        + ". Remain well guarded against "
                        + qualifier
                        + "flood waters having further impacts of "
                        + impactRange
                        + " potential."
                    )
                else:
                    curPhrase = "Little to no additional impacts expected from flooding rain."

            # If this phrase is not already part of the additional impacts
            if curPhrase not in sectionDict["additionalImpactRange"]:

                # Add it now
                sectionDict["additionalImpactRange"].append(curPhrase)

        # If there is no impact across more than one half the area, include a
        # statement for that as well
        if inputThreatDominant == "None":

            curPhrase = (
                "Elsewhere across "
                + self._cwa_descriptor()
                + ", little to no impact is anticipated."
            )

            # If this phrase is not already part of the additional impacts
            if curPhrase not in sectionDict["additionalImpactRange"]:

                # Add it now
                sectionDict["additionalImpactRange"].append(curPhrase)

        productDict["floodingRainSection"] = sectionDict

    def _tornadoSection(self, productDict, productSegmentGroup, productSegment):
        sectionDict = dict()
        sectionDict["title"] = "Tornadoes"
        sectionDict["impactRange"] = ""
        sectionDict["impactLib"] = []
        sectionDict["additionalImpactRange"] = []
        sectionDict["variedImpacts"] = False

        impactMin = self._samplingDict["TornadoThreat"]["impactMin"]
        impactMax = self._samplingDict["TornadoThreat"]["impactMax"]
        impactRange = self._samplingDict["TornadoThreat"]["impactRange"]
        impactRangeMax = self._samplingDict["TornadoThreat"]["impactRangeMax"]
        inputThreatDominant = self._samplingDict["TornadoThreat"]["inputThreatDominant"]

        # Test the simplest case first
        if impactMin == "none" and impactMax == "none":
            sectionDict["impactRange"] = impactRange
            productDict["tornadoSection"] = sectionDict
            return

        # For tornadoes only, Cap at devastating
        if impactMax in ["devastating", "catastrophic"]:
            impactMax = "devastating"
        if impactMin in ["devastating", "catastrophic"]:
            impactMin = "devastating"
        if impactRange in ["devastating", "catastrophic"]:
            impactRange = "devastating"
            impactRangeMax = impactRange

        # If the max impact category is "catastrophic", and we lumped "devastating" in
        # with it, ensure "devastating" is not leftover as the high end of the range
        impactParts = impactRange.split(" ")  # split up the impact range

        # If "devastating" is the high end of the range
        if len(impactParts) == 3 and impactParts[2] == "devastating":
            # If the first part is not "extensive"
            if impactParts[0] != "extensive":
                # Force the upper end to be 1 category lower
                impactRange.replace("devastating", "extensive")
                impactRangeMax = "extensive"
            # Otherwise, the impact is just "extensive"
            else:
                impactRange = "extensive"
                impactRangeMax = "extensive"

        qualifier = ""
        if impactMax in ["extensive", "devastating"]:
            qualifier = "particularly dangerous "
        elif impactMax == "significant":
            qualifier = "dangerous "

        # If there is only one impact across the entire CWA, and it is the max
        if impactMax != "none" and impactMin == impactMax and inputThreatDominant != "None":
            if self._GeneralOnsetTime == "check plans":
                sectionDict["impactRange"] = (
                    "Prepare for a "
                    + qualifier
                    + "tornado event having possible "
                    + impactMax
                    + " impacts across "
                    + self._cwa_descriptor()
                    + ". Potential impacts include:"
                )
            elif self._GeneralOnsetTime == "complete preparations":
                sectionDict["impactRange"] = (
                    "Protect against a "
                    + qualifier
                    + "tornado event having possible "
                    + impactMax
                    + " impacts across "
                    + self._cwa_descriptor()
                    + ". Potential impacts include:"
                )
            elif self._GeneralOnsetTime == "hunker down":
                sectionDict["impactRange"] = (
                    "Potential impacts from tornadoes are still unfolding across "
                    + self._cwa_descriptor()
                    + ". Remain well braced against a "
                    + qualifier
                    + "tornado event having "
                    + self._frame("possible | additional")
                    + " "
                    + impactMax
                    + " impacts. If realized, these impacts include:"
                )
            else:
                sectionDict["impactRange"] = (
                    "Additional impacts from tornadoes are still a concern across "
                    + self._cwa_descriptor()
                    + ". Remain well braced against "
                    + qualifier
                    + "tornado event having further "
                    + impactMax
                    + " impact potential."
                )
        # Handle the case where the impacts are not the same across the entire CWA
        else:
            if self._GeneralOnsetTime == "check plans":
                sectionDict["impactRange"] = (
                    "Prepare for a "
                    + qualifier
                    + "tornado event having possible "
                    + impactMax
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + ". Potential impacts include:"
                )
            elif self._GeneralOnsetTime == "complete preparations":
                sectionDict["impactRange"] = (
                    "Protect against a "
                    + qualifier
                    + "tornado event having possible "
                    + impactMax
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + ". Potential impacts include:"
                )
            elif self._GeneralOnsetTime == "hunker down":
                sectionDict["impactRange"] = (
                    "Potential impacts from tornadoes are still unfolding across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + ". Remain well braced against a "
                    + qualifier
                    + "tornado event having "
                    + self._frame("possible | additional")
                    + " "
                    + impactMax
                    + " impacts. If realized, these impacts include:"
                )
            else:
                if impactMax != "none":
                    sectionDict["impactRange"] = (
                        "Additional impacts from tornadoes are still a concern across "
                        + self._frame("ENTER AREA DESCRIPTION")
                        + ". Remain well braced against "
                        + qualifier
                        + "tornado event having further "
                        + impactMax
                        + " impact potential."
                    )
                else:
                    sectionDict[
                        "impactRange"
                    ] = "Little to no additional impacts expected from tornadoes."

        if self._GeneralOnsetTime != "recovery":
            sectionDict["impactLib"] = self._getPotentialImpactsStatements(
                "Tornado", self._impactCategoryToThreatLevel(impactMax))
        else:
            sectionDict["impactLib"] = []

        # If there are additional areas
        if impactRange != impactMax:

            qualifier = ""
            if impactRangeMax in ["extensive", "devastating"]:
                qualifier = "particularly dangerous "
            elif impactRangeMax == "significant":
                qualifier = "dangerous "

            if self._GeneralOnsetTime == "check plans":
                curPhrase = (
                    "Prepare for a "
                    + qualifier
                    + "tornado event having possible "
                    + impactRange
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + "."
                )
            elif self._GeneralOnsetTime == "complete preparations":
                curPhrase = (
                    "Protect against a "
                    + qualifier
                    + "tornado event having possible "
                    + impactRange
                    + " impacts across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + "."
                )
            elif self._GeneralOnsetTime == "hunker down":
                curPhrase = (
                    "Potential impacts from tornadoes are still unfolding across "
                    + self._frame("ENTER AREA DESCRIPTION")
                    + ". Remain well braced against a "
                    + qualifier
                    + "tornado event having "
                    + self._frame("possible | additional")
                    + " "
                    + impactRange
                    + " impacts."
                )
            else:
                if impactMax != "none":
                    curPhrase = (
                        "Additional impacts from tornadoes are still a concern across "
                        + self._frame("ENTER AREA DESCRIPTION")
                        + ". Remain well braced against "
                        + qualifier
                        + "tornado event having further "
                        + impactRange
                        + " impact potential."
                    )
                else:
                    curPhrase = "Little to no additional impacts expected from tornadoes."

            # If this phrase is not already part of the additional impacts
            if curPhrase not in sectionDict["additionalImpactRange"]:

                # Add it now
                sectionDict["additionalImpactRange"].append(curPhrase)

        # If there is no impact across more than one half the area, include a
        # statement for that as well
        if inputThreatDominant == "None":

            curPhrase = (
                "Elsewhere across "
                + self._cwa_descriptor()
                + ", little to no impact is anticipated."
            )

            # If this phrase is not already part of the additional impacts
            if curPhrase not in sectionDict["additionalImpactRange"]:

                # Add it now
                sectionDict["additionalImpactRange"].append(curPhrase)

        productDict["tornadoSection"] = sectionDict

    def _getImpactsQualifier(self, impact):
        qualifier = ""
        if impact in ["extensive", "devastating", "catastrophic"]:
            qualifier = "life-threatening "
        elif impact == "significant":
            qualifier = "dangerous "
        elif impact == "limited":
            qualifier = "hazardous "

        return qualifier

    def _coastalHazardsSection(self, productDict, productSegmentGroup, productSegment):
        productDict["coastalHazardsSection"] = self._frame(
            "Enter here a statement of any additional hazards of concern along the coast "
            "such as rip currents, high waves, concerns for beach erosion etc etc if not "
            "already done in the surge section."
        )

    def _preparednessSection(self, productDict, productSegmentGroup, productSegment):
        sectionDict = dict()
        sectionDict["title"] = "PRECAUTIONARY/PREPAREDNESS ACTIONS"

        sectionDict["genericAction"] = None
        if not self._ImpactsAnticipated:
            sectionDict["genericAction"] = (
                "It is always a good idea to check your preparedness plans so when "
                "and if the time comes during hurricane season, you are ready to "
                "execute them. A good resource is ready.gov."
            )

        productDict["preparednessSection"] = sectionDict

    def _evacuationStatements(self, productDict, productSegmentGroup, productSegment):
        evacuationDict = dict()
        evacuationDict["title"] = "Evacuations"

        import TCVDictionary

        evacuationDict["statements"] = TCVDictionary.EvacuationStatements

        productDict["evacuationStatements"] = evacuationDict

    def _otherPreparednessActions(self, productDict, productSegmentGroup, productSegment):
        actionsDict = dict()
        actionsDict["title"] = "Other Preparedness Information"

        import TCVDictionary

        actionsDict["actions"] = TCVDictionary.OtherPreparednessActions[self._GeneralOnsetTime]

        productDict["otherPreparednessActions"] = actionsDict

    def _additionalSourcesInfo(self, productDict, productSegmentGroup, productSegment):
        infoDict = dict()
        infoDict["title"] = "Additional Sources of Information"

        import TCVDictionary

        infoDict["sources"] = TCVDictionary.AdditionalSources

        productDict["additionalSourcesInfo"] = infoDict

    def _applyPreviousText(self, productDict=None):
        """_applyPreviousText - copies any sections of previous text which can be used
        into the productParts dictionary of this text product.
        """

        # If we did not get a valid productParts dictionary - there is nothing to do
        if productDict is None:
            return productDict

        # Display keys of current productParts dictionary
        self.debug_print("*" * 100, 1)
        for key in sorted(productDict):
            self.debug_print(key, 1)
        self.debug_print("*" * 100, 1)

        # See which sections of previous text could be used
        (usePreviousText, usePreviousTextDict, messages) = self._determinePreviousTextUse()

        # If we can use any previous text
        if usePreviousText:

            # Try to get the previous HLS text
            (prevHLSdict, hlsMessages) = self._getPreviousHlsText(
                usePreviousTextDict["advisoryNum"]
            )

            # Add any error messages retrieving the previous text
            if hlsMessages:
                if messages:
                    messages += "\n\n"

                messages += hlsMessages

            # If the previous text dictionary was created correctly
            if prevHLSdict:

                missingSections = []

                # Update the sections which are valid and permitted to use previous text
                for key in usePreviousTextDict:
                    self.debug_print("*" * 125 + "\n\n{}\n".format(key), 1)

                    if usePreviousTextDict.get(key, None):
                        prevText = prevHLSdict.get(key, None)

                        if prevText:
                            if key in productDict:
                                for item in productDict[key]:
                                    self.debug_print(
                                        "=" * 80 + "\nOriginal:\n{}:\n\t{}".format(
                                            item, productDict[key][item]), 1
                                    )

                                    if item in prevText and prevText[item]:
                                        productDict[key][item] = prevText[item]
                                        self.debug_print(
                                            (
                                                "\n\n" + "=" * 80 +
                                                "\nUpdated to:"
                                                "\n{}:\n\t{}\n\n" + "=" * 80
                                            ).format(item, productDict[key][item]),
                                            1,
                                        )
                        elif productDict.get(key, None):
                            missingSections.append(key)

                if missingSections:
                    if messages:
                        messages += "\n\n"

                    messages = (
                        "Could not find previous text for these sections, "
                        "although it would have been used:\n\t"
                        "{0} and {1}"
                    ).format(", ".join(missingSections[:-1]), missingSections[-1])

        if messages:
#             os.system("sendGfeMessage -u -c GFE -m '" + messages + "'")
            LogStream.logProblem(messages)

        for item in productDict:
            self.debug_print("\n\n{}: {}".format(item, productDict[item]), 1)

        return productDict

    def _getAdvisoryNumberParts(self, advisoryNum):
        """_getAdvisoryNumberParts - Splits a tropical public advisory number into
        its numeric and letter parts (if any).
        """

        # Split off any letter parts to this advisory number
        parts = [part for part in re.split(r"\d+", advisoryNum) if part]
        letter = ""

        # See if there is a letter part to this advisory, there should only be one,
        # if any. Ignore any empty parts
        if parts:
            letter = parts[0].upper()

        # Now get the numeric part
        numeric = int(advisoryNum.replace(letter, ""))

        self.debug_print("***  numeric, letter = {}, {}  ***".format(numeric, letter), 1)
        return (numeric, letter)

    def _determinePreviousTextUse(self):
        """_determinePreviousTextUse - Compares last two HLS advisories to determine
        which sections may use text from a previous product.
        """

        # Assume we cannot use previous text for any section
        usePreviousText = False

        usePreviousTextDict = {
            "advisoryNum": None,
            "windSection": False,
            "surgeSection": False,
            "floodingRainSection": False,
            "tornadoSection": False,
            "coastalHazardsSection": False,
            "evacuationStatements": False,
            "otherPreparednessActions": False,
            "additionalSourcesInfo": False,
        }

        # Set up to process any error messages to be passed on to the user
        messages = ""
        threatSections = []

        # Retrieve the latest HLS advisory, which should be the one we're running now
        curAdvisory = getattr(self, "_currentAdvisory", None)

        if not curAdvisory:
            messages = (
                "Something is very wrong! Could not find any HLS advisory information "
                "for the latest NHC advisory. No previous text will be used."
            )

            return (usePreviousText, usePreviousTextDict, messages)

        # If we found the latest HLS advisory information
        prevAdvisory = None
        (curAdvNum, curAdvLetter) = self._getAdvisoryNumberParts(
            curAdvisory["AdvisoryNumber"]
        )
        self.debug_print("***  advisoryInfo: curNum = {}   curLetter = '{}'".format(
            curAdvNum, curAdvLetter), 1)

        # Look for the latest advisory which immediately preceded it. We need to
        # be careful here, because advisories may be "lettered" (e.g. "7B" follows
        # "7A" or "8" follows "7A")
        for adv in self._previousHlsAdvisories:
            (advNum, advLetter) = self._getAdvisoryNumberParts(adv["AdvisoryNumber"])
            if advNum + 1 == curAdvNum or (
                advNum == curAdvNum
                and (
                    (advLetter == "" and curAdvLetter == "A")
                    or (curAdvLetter and advLetter
                        and (ord(curAdvLetter) - ord(advLetter) == 1)
                        )
                )
            ):
                prevAdvisory = adv.copy()
                usePreviousTextDict["advisoryNum"] = (advNum, advLetter)
                break

        # If we found the previous HLS advisory information
        if prevAdvisory:

            # See if we can use the text from individual sections
            if (
                curAdvisory["WindImpactDominant"] == prevAdvisory["WindImpactDominant"]
                and curAdvisory["WindImpactMax"] == prevAdvisory["WindImpactMax"]
                and curAdvisory["WindImpactMin"] == prevAdvisory["WindImpactMin"]
            ):
                usePreviousTextDict["windSection"] = True
                usePreviousText = True
            else:
                threatSections.append("Wind")

            if (curAdvisory["StormSurgeImpactDominant"] == prevAdvisory["StormSurgeImpactDominant"]
                and curAdvisory["StormSurgeImpactMax"] == prevAdvisory["StormSurgeImpactMax"]
                and curAdvisory["StormSurgeImpactMin"] == prevAdvisory["StormSurgeImpactMin"]
            ):
                usePreviousTextDict["surgeSection"] = True
                usePreviousText = True
            else:
                threatSections.append("Surge")

            if (curAdvisory["FloodingRainImpactDominant"] == prevAdvisory["FloodingRainImpactDominant"]
                and curAdvisory["FloodingRainImpactMax"] == prevAdvisory["FloodingRainImpactMax"]
                and curAdvisory["FloodingRainImpactMin"] == prevAdvisory["FloodingRainImpactMin"]
            ):
                usePreviousTextDict["floodingRainSection"] = True
                usePreviousText = True
            else:
                threatSections.append("Flooding Rain")

            if (curAdvisory["TornadoImpactDominant"] == prevAdvisory["TornadoImpactDominant"]
                and curAdvisory["TornadoImpactMax"] == prevAdvisory["TornadoImpactMax"]
                and curAdvisory["TornadoImpactMin"] == prevAdvisory["TornadoImpactMin"]
            ):
                usePreviousTextDict["tornadoSection"] = True
                usePreviousText = True
            else:
                threatSections.append("Tornadoes")

            # See if any sections could not be used
            if threatSections:
                if messages:
                    messages += "\n"

                messages += (
                    "Threat information changed from the previous advisory. "
                    "Impact info regenerated for "
                )

                if len(threatSections) > 1:
                    messages += "these sections: {0} and {1}\n\n".format(
                        ", ".join(threatSections[:-1]), threatSections[-1])
                else:
                    messages += "this section: {0}\n\n".format(threatSections[0])

            # Check precautionary/preparedness sections
            if curAdvisory["EventContext"] == prevAdvisory["EventContext"]:
                usePreviousTextDict["evacuationStatements"] = True
                usePreviousTextDict["otherPreparednessActions"] = True
                usePreviousTextDict["additionalSourcesInfo"] = True
                usePreviousText = True
            else:
                if messages:
                    messages += "\n"

                messages += (
                    "Event context changed. Precautionary/Preparedness info "
                    "was regenerated.\n"
                )
        else:
            if messages:
                messages += "\n"

            if curAdvNum == 1:
                prevAdvNum = "from a previous advisory"
            else:
                prevAdvNum = "for advisory number {}".format(curAdvNum - 1)

            messages += (
                "Could not find HLS information {}. No previous text will be used."
            ).format(prevAdvNum)

        self.debug_print("final messages = '{}'".format(messages), 1)

        # Return a flag to indicate if any previous text may be used, as well as which
        # specific sections can be used, as well as any error messages so far
        return (usePreviousText, usePreviousTextDict, messages)

    def _getPreviousHlsText(self, desiredAdv):
        """_getPreviousHlsText - Retrieves text from a previous HLS product, but only
        if the advisory number associated with that text is deemed to be close enough.
        """

        messages = ""

        # Define a record, at least the parts we are interested in keeping
        record = {
            "additionalImpactRange": [],
            "impactLib": [],
            "impactRange": "",
        }

        # Initialize a dictionary to track various parts of the HLS
        prevHLSdict = {
            "advisoryNum": None,
            "windSection": record.copy(),
            "surgeSection": record.copy(),
            "floodingRainSection": record.copy(),
            "tornadoSection": record.copy(),
            "coastalHazardsSection": record.copy(),
            "evacuationStatements": record.copy(),
            "otherPreparednessActions": {"statements": []},
            "additionalSourcesInfo": {"actions": []},
        }

        previousText = self.getPreviousProduct(self._textdbPil)

        self.debug_print("=" * 100, 1)
        self.debug_print("Previous Text", 1)
        self.debug_print(previousText, 1)
        self.debug_print("=" * 100, 1)

        if previousText:

            # Ensure this advisory is close enough to be used safely
            desiredAdvNum, desiredAdvLetter = desiredAdv
            hlsAdvNum = None
            hlsAdvLetter = None

            # Get the advisory number from the latest HLS product
            advNumMatch = re.search("(?i)Local Statement Advisory Number (\w+)", previousText)
            if advNumMatch:
                prevHLSdict["advisoryNum"] = self._getAdvisoryNumberParts(
                    advNumMatch.group(1).strip()
                )

                (hlsAdvNum, hlsAdvLetter) = prevHLSdict["advisoryNum"]

                self.debug_print("\n\tdesiredAdv = {}   hlsAdv = {}".format(
                    desiredAdv, prevHLSdict["advisoryNum"]), 1
                )

            if (hlsAdvNum and hlsAdvNum != desiredAdvNum) or (
                hlsAdvLetter != desiredAdvLetter
                and (
                    hlsAdvLetter
                    and desiredAdvLetter
                    and abs(ord(hlsAdvLetter) - ord(desiredAdvLetter)) > 1
                )
            ):
                messages = (
                    "Previous HLS text is from the wrong advisory. "
                    "No previous text will be used."
                )

                # No point going further, return the error messages
                return (None, messages)

            #==========================================================================
            # If we made it this far, parse out the sections we might be able to use

            # Look for impact sections - can be in any order
            sectionHeaders = (
                r"^\* ((WIND|SURGE|FLOODING RAIN|TORNADOES|OTHER COASTAL HAZARDS)\:\n)"
            )
            sectionMatches = re.findall(
                (
                    "(?ism){0}(^.+?)(^ +\-.+?\n\n)*(?=({0}|"
                    "^PRECAUTIONARY/PREPAREDNESS ACTIONS))"
                ).format(sectionHeaders),
                previousText,
            )

            self.debug_print("*" * 100, 1)
            self.debug_print(sectionMatches, 1)
            self.debug_print("*" * 100, 1)

            for sectionMatch in sectionMatches:

                self.debug_print("\n" + "=" * 75, 1)
                self.debug_print("sectionMatch", 1)
                self.debug_print("{}".format(repr(sectionMatch)), 1)
                for index, item in enumerate(sectionMatch):
                    self.debug_print("\n{}  {}".format(index, item), 1)
                self.debug_print("=" * 75, 1)

                if sectionMatch:

                    # See which section we got
                    if "WIND:" in sectionMatch[0]:
                        sectionKey = "windSection"
                    elif "SURGE:" in sectionMatch[0]:
                        sectionKey = "surgeSection"
                    elif "FLOODING RAIN:" in sectionMatch[0]:
                        sectionKey = "floodingRainSection"
                    elif "TORNADOES:" in sectionMatch[0]:
                        sectionKey = "tornadoSection"
                    elif "OTHER COASTAL HAZARDS:" in sectionMatch[0]:
                        sectionKey = "coastalHazardsSection"

                    prevHLSdict[sectionKey]["impactRange"] = re.sub(
                        " +", " ", sectionMatch[2].replace("\n", " ")
                    ).strip()

                    if sectionMatch[3].strip():
                        parts = [
                            re.sub(" +", " ", part.replace("\n", " ").strip())
                            for part in sectionMatch[3].split("\n\n")
                        ]

                        prevHLSdict[sectionKey]["impactLib"] = [
                            item.strip() for item in parts[0].split("- ") if item.strip()
                        ]

                        self.debug_print("impactLib = {}".format(
                            repr(prevHLSdict[sectionKey]["impactLib"])), 1)

                        if len(parts) > 1 and parts[1].strip():
                            prevHLSdict[sectionKey]["additionalImpactRange"] = [
                                parts[1].strip()
                            ]

            # Look for Precautionary/PreparednessActions evacuations
            sectionMatch = re.search(
                r"(?ism)^\* EVACUATIONS:\n(.+?)^\* OTHER PREPAREDNESS INFORMATION:",
                previousText,
            )

            if sectionMatch:

                # Cleanup input text
                sectionText = sectionMatch.group(1).strip().replace("\n\n", "<CR>")
                self.debug_print("\n\t***\n\t{}\n\t***".format(sectionText), 1)
                sectionText = re.sub(" +", " ", sectionText.replace("\n", " "))

                parts = [part.strip()
                         for part in sectionText.split("<CR>") if part.strip()]

                prevHLSdict["evacuationStatements"]["statements"] = parts

            # Look for Precautionary/PreparednessActions other info
            sectionMatch = re.search(
                r"(?ism)^\* OTHER PREPAREDNESS INFORMATION:\n(.+?)"
                r"^\* ADDITIONAL SOURCES OF INFORMATION:(.+?)",
                previousText,
            )

            if sectionMatch:

                # Cleanup input text
                sectionText = sectionMatch.group(1).strip().replace("\n\n", "<CR>")
                self.debug_print("\n\t***\n\t{}\n\t***".format(sectionText))
                sectionText = re.sub(" +", " ", sectionText.replace("\n", " "))

                parts = [part.strip()
                         for part in sectionText.split("<CR>") if part.strip()]
                self.debug_print(parts)

                prevHLSdict["otherPreparednessActions"]["actions"] = parts

        self.debug_print("=" * 100 + "Previous Text Dictionary", 1)
        for key in prevHLSdict:
            curDict = prevHLSdict[key]

            if isinstance(curDict, dict):
                self.debug_print("\n\n***  {}  ***\n".format(key), 1)
                for item in curDict:
                    self.debug_print("{}: {}".format(item, repr(curDict[item])), 1)
            else:
                self.debug_print("\n\n***  {}  ***\n\t{}".format(key, curDict), 1)

        self.debug_print("=" * 100, 1)

        return (prevHLSdict, messages)

    def _nextUpdate(self, productDict, productSegmentGroup, productSegment):

        if not self._ImpactsAnticipated:
            productDict[
                "nextUpdate"
            ] = "At this time...additional local statements are not anticipated unless conditions warrant."
        # or not self._ImpactsAnticipated:
        elif self._NextUpdate == "LastIssuance":
            productDict["nextUpdate"] = (
                "As it pertains to this event...this will be the last local statement "
                "issued by the National Weather Service in "
                + self._wfoCityState
                + " regarding the effects of tropical cyclone hazards upon the area."
            )
        elif self._NextUpdate == "Conditions":
            productDict["nextUpdate"] = (
                "The next local statement will be issued by the National Weather Service in "
                + self._wfoCityState
                + " as conditions warrant."
            )
        elif self._NextUpdate == "Enter":
            productDict["nextUpdate"] = (
                "The next local statement will be issued by the National Weather Service in "
                + self._wfoCityState
                + " around "
                + self._NextUpdate_entry.strip()
                + ", or sooner if conditions warrant."
            )

    ################# Product Parts Helper Methods

    def _impactsKeyFunction(self, optionIndexTuple):
        ((_, _), indexStr) = optionIndexTuple
        indexStr = indexStr.strip()
        if len(indexStr) == 0:
            return 9999
        else:
            return int(indexStr)

    def _getPotentialImpactsStatements(self, elementName, maxThreat):
        import TCVDictionary

        potentialImpactStatements = TCVDictionary.PotentialImpactStatements
        statements = potentialImpactStatements[elementName][maxThreat]

        return statements

    def _impactCategoryToThreatLevel(self, impactCategory):
        if impactCategory == "catastrophic" or impactCategory == "devastating":
            return "Extreme"
        elif impactCategory == "extensive":
            return "High"
        elif impactCategory == "significant":
            return "Mod"
        elif impactCategory == "limited":
            return "Elevated"
        else:
            return "None"

    def _determineHazardStates(self):
        self._currentHazardsList = []
        self._changesHazardsList = []

        self.debug_print("*" * 80)

        for key in sorted(self._previousAdvisory.keys()):
            self.debug_print("%s : %s" % (key, self._previousAdvisory[key]), 1)
        for hazard in self._previousAdvisory["HazardsForHLS"]:
            self.debug_print("DEBUG Hazard: %s" % (self._pp.pformat(hazard)), 1)
            if hazard["act"] != "CON":
                self._changesHazardsList.append(hazard)
            if hazard["act"] not in ["CAN", "UPG"]:
                self._currentHazardsList.append(hazard)

        self.debug_print("-" * 80, 1)
        self.debug_print("self._changesHazardsList = %s" % (self._changesHazardsList), 1)
        self.debug_print("self._currentHazardsList = %s" % (self._currentHazardsList), 1)

    ###############################################################
    ### Sampling and Statistics related methods

    def _sampleHLSData(self, argDict):
        self._initializeHLSstatus()
        self._previousHlsAdvisories = self._getPreviousHlsAdvisories()

        editAreas = [(self._cwa(), self._cwa())]

        cwaSampler = self.getSampler(
            argDict, (self._analysisList_HLS(), self._timeRangeList3Hour, editAreas))

        statList = self.getStatList(cwaSampler,
                                    self._analysisList_HLS(),
                                    self._timeRangeList3Hour,
                                    self._cwa())

        for period in range(len(statList)):

            self.debug_print("=" * 100, 1)
            self.debug_print("In _sampleHLSData for period %s (%s)" %
                             (period, self._timeRangeList3Hour[period][0]), 1)

            statDict = statList[period]
            for threatName in ["WindThreat", "FloodingRainThreat", "TornadoThreat"]:
                self._sampleRankedDiscreteValue(threatName, statDict)
                # TODO: Investigate if this sampling method is still really needed. The
                #       JSON files may have all the needed information now
                self._sampleMostSignificantDiscreteValue(threatName, statDict)

            qpfToFfgRatio = self._getStatValue(statDict, "QPFtoFFGRatio", "Max")
            decidingField = self._samplingDict["FloodingRainThreat"]["decidingField"]
            if decidingField is None or (qpfToFfgRatio is not None and qpfToFfgRatio > decidingField):
                self._samplingDict["FloodingRainThreat"]["decidingField"] = qpfToFfgRatio

        self.debug_print(
            "WindThreat = %s" % (self._samplingDict["WindThreat"]["inputThreatDominant"]), 1)
        self.debug_print("FloodingRainThreat = %s" %
                         (self._samplingDict["FloodingRainThreat"]["inputThreatDominant"]), 1)
        self.debug_print("TornadoThreat = %s" %
                         (self._samplingDict["TornadoThreat"]["inputThreatDominant"]), 1)

        self._createWholeDomainEditArea(argDict)
        editAreas = [("WholeDomain", "WholeDomain")]
        wholeDomainSampler = self.getSampler(
            argDict, (self._analysisList_HLS_WholeDomain(), self._timeRangeList3Hour, editAreas))

        statList = self.getStatList(wholeDomainSampler,
                                    self._analysisList_HLS_WholeDomain(),
                                    self._timeRangeList3Hour,
                                    "WholeDomain")

        for period in range(len(statList)):
            statDict = statList[period]
            maxWind = self._getStatValue(statDict, "Wind", "Max", self.VECTOR())
            decidingField = self._samplingDict["WindThreat"]["decidingField"]
            if decidingField is None or (maxWind is not None and maxWind > decidingField):
                self._samplingDict["WindThreat"]["decidingField"] = maxWind

        editAreas = [(self._cwa(), self._cwa())]
        intersectAreas = self._computeIntersectAreas(editAreas, argDict)
        if len(intersectAreas) != 0:
            self.debug_print("Sampling StormSurgeThreat, now")
            intersectSampler = self.getSampler(
                argDict,
                (self._intersectAnalysisList_HLS(), self._timeRangeList3Hour, intersectAreas),
            )

            statList = self.getStatList(
                intersectSampler,
                self._intersectAnalysisList_HLS(),
                self._timeRangeList3Hour,
                "intersect_" + self._cwa(),
            )

            for period in range(len(statList)):
                statDict = statList[period]
                self.debug_print(
                    "current stormSurge statDict = %s" % (self._pp.pformat(statDict)), 1
                )
                self._sampleRankedDiscreteValue("StormSurgeThreat", statDict)

                inundationMax = self._getStatValue(statDict, "InundationMax", "Max")
                decidingField = self._samplingDict["StormSurgeThreat"]["decidingField"]
                if decidingField is None or (inundationMax is not None and inundationMax > decidingField):
                    self._samplingDict["StormSurgeThreat"]["decidingField"] = inundationMax

            self.debug_print("StormSurgeThreat = %s" %
                             (self._samplingDict["StormSurgeThreat"]["inputThreatDominant"]), 1)

    def _sampleTCVAdvisory(self, advisory):
        self.debug_print("sampling TCV advisory!", 1)
        seenValidThreatLevel = {}
        for zone in advisory["ZoneData"]:
            self.debug_print("-" * 60, 1)
            self.debug_print("Looking at zone %s" % (zone), 1)
            for key in advisory["ZoneData"][zone]:
                if "Threat" not in key or "highestHunkerDown" in key:
                    continue

                if key not in seenValidThreatLevel:
                    seenValidThreatLevel[key] = False

                self.debug_print("Looking at key '%s'" % (key), 1)

                threatLevel = advisory["ZoneData"][zone][key]
                self.debug_print("   Threat level = %s" % (threatLevel), 1)

                if (self._samplingDict[key]["inputThreatLow"] is None) and (
                    not seenValidThreatLevel[key]
                ):
                    self._samplingDict[key]["inputThreatLow"] = threatLevel
                if (self._samplingDict[key]["inputThreatHigh"] is None) and (
                    not seenValidThreatLevel[key]
                ):
                    self._samplingDict[key]["inputThreatHigh"] = threatLevel

                if threatLevel is not None:
                    seenValidThreatLevel[key] = True

                lowThreat = self._samplingDict[key]["inputThreatLow"]
                highThreat = self._samplingDict[key]["inputThreatHigh"]
                threatOrder = self.mostSignificantDiscrete_keyOrder_dict(None, None, None)[key]

                self.debug_print("***** threatOrder = %s" % (repr(threatOrder)), 1)

                if threatOrder.index(threatLevel) < threatOrder.index(lowThreat):
                    lowThreat = threatLevel
                if threatOrder.index(threatLevel) > threatOrder.index(highThreat):
                    highThreat = threatLevel

                if lowThreat is None:
                    self.debug_print("   low threat = Python None", 1)
                else:
                    self.debug_print("   low threat = %s" % (lowThreat), 1)
                self.debug_print("   high threat = %s" % (highThreat), 1)

                self._samplingDict[key]["inputThreatLow"] = lowThreat
                self._samplingDict[key]["inputThreatHigh"] = highThreat

        self.debug_print("Sampling dict =\n\n%s\n" % (self._pp.pformat(self._samplingDict)), 1)

    def _sampleRankedDiscreteValue(self, threatName, statDict):
        self.debug_print("-" * 60, 1)
        self.debug_print(
            "_sampleRankedDiscreteValue statDict =\n\n%s\n" %
            (self._pp.pformat(statDict)), 1)
        rankedThreatLevels = self.getStats(statDict, threatName + "__rankedDiscreteValue")
        self.debug_print("sampling %s" % (threatName), 1)
        self.debug_print("sampleData: rankedThreatLevels =\n\n%s\n" %
                         (self._pp.pformat(rankedThreatLevels)), 1)
        if rankedThreatLevels is not None:
            dominantThreatLevel = self._getDominantThreatLevel(threatName, rankedThreatLevels)
            self.debug_print("dominantThreatLevel = %s" % (dominantThreatLevel), 1)

            currentDominantThreatLevel = self._samplingDict[threatName]["inputThreatDominant"]
            self.debug_print("currentDominantThreatLevel = %s" % (currentDominantThreatLevel), 1)
            self._samplingDict[threatName]["inputThreatDominant"] = self._getHighestThreat(
                threatName, dominantThreatLevel, currentDominantThreatLevel)
            self.debug_print("new dominant = %s" %
                             (self._samplingDict[threatName]["inputThreatDominant"]), 1)

    def _sampleMostSignificantDiscreteValue(self, threatName, statDict):
        self.debug_print("_sampleMostSignificantDiscreteValue for %s" % (threatName), 1)
        threatLevel = self.getStats(statDict, threatName + "__mostSignificantDiscreteValue")
        self.debug_print("threatLevel = %s" % (threatLevel), 1)
        if threatLevel is not None:
            inputThreatLow = self._samplingDict[threatName]["inputThreatLow"]
            self.debug_print("current inputThreatLow = %s" % (inputThreatLow), 1)
            if inputThreatLow is None:
                self._samplingDict[threatName]["inputThreatLow"] = threatLevel
            else:
                self._samplingDict[threatName]["inputThreatLow"] = self._getLowestThreat(
                    threatName, threatLevel, inputThreatLow)
            self.debug_print("new inputThreatLow = %s" %
                             (self._samplingDict[threatName]["inputThreatLow"]), 1)

            inputThreatHigh = self._samplingDict[threatName]["inputThreatHigh"]
            self.debug_print("current inputThreatHigh = %s" % (inputThreatHigh), 1)
            self._samplingDict[threatName]["inputThreatHigh"] = self._getHighestThreat(
                threatName, threatLevel, inputThreatHigh)
            self.debug_print("new inputThreatHigh = %s" %
                             (self._samplingDict[threatName]["inputThreatHigh"]), 1)

    def _getDominantThreatLevel(self, threatName, rankedThreatLevels):
        dominantLevelWithHighestRank = None
        highestRank = None

        for (level, rank) in rankedThreatLevels:
            if highestRank is None or rank > highestRank:
                highestRank = rank
                dominantLevelWithHighestRank = level
            elif rank == highestRank:
                dominantLevelWithHighestRank = self._getHighestThreat(
                    threatName, dominantLevelWithHighestRank, level
                )

        return dominantLevelWithHighestRank

    def _getHighestThreat(self, threatName, threatLevel1, threatLevel2):
        keyOrderDict = self.mostSignificantDiscrete_keyOrder_dict(None, None, None)
        keyOrder = keyOrderDict[threatName]

        level1Index = keyOrder.index(threatLevel1)
        level2Index = keyOrder.index(threatLevel2)

        if level1Index < level2Index:
            return threatLevel2
        elif level1Index == level2Index:
            return threatLevel1
        else:
            return threatLevel1

    def _getLowestThreat(self, threatName, threatLevel1, threatLevel2):
        keyOrderDict = self.mostSignificantDiscrete_keyOrder_dict(None, None, None)
        keyOrder = keyOrderDict[threatName]

        level1Index = keyOrder.index(threatLevel1)
        level2Index = keyOrder.index(threatLevel2)

        if level1Index < level2Index:
            return threatLevel1
        elif level1Index == level2Index:
            return threatLevel1
        else:
            return threatLevel2

    def _setHazardImpactCategories(self, threatName):
        inputThreatLow = self._samplingDict[threatName]["inputThreatLow"]
        inputThreatHigh = self._samplingDict[threatName]["inputThreatHigh"]
        decidingField = self._samplingDict[threatName]["decidingField"]
        catastrophicThreshold = self._samplingDict[threatName]["catastrophicThreshold"]

        self.debug_print("-" * 60, 1)
        self.debug_print("DEBUG: _setHazardImpactCategories for %s" % (threatName), 1)

        impactMin = None
        impactMax = None
        impactRange = None
        impactRangeMax = None

        # Determine lowest impact category
        if inputThreatLow == "Extreme":
            if threatName != "TornadoThreat" and decidingField is not None and decidingField >= catastrophicThreshold:
                impactMin = "catastrophic"
            else:
                impactMin = "devastating"
        elif inputThreatLow == "High":
            impactMin = "extensive"
        elif inputThreatLow == "Mod":
            impactMin = "significant"
        elif inputThreatLow == "Elevated":
            impactMin = "limited"
        else:
            impactMin = "none"

        # Determine highest impact category
        if inputThreatHigh == "Extreme":
            if threatName != "TornadoThreat" and decidingField is not None and decidingField >= catastrophicThreshold:
                impactMax = "catastrophic"
                impactRangeMax = "devastating"
            else:
                impactMax = "devastating"
                impactRangeMax = "extensive"
        elif inputThreatHigh == "High":
            impactMax = "extensive"
            impactRangeMax = "significant"
        elif inputThreatHigh == "Mod":
            impactMax = "significant"
            impactRangeMax = "limited"
        elif inputThreatHigh == "Elevated":
            impactMax = "limited"
            impactRangeMax = "none"
        else:
            impactMax = "none"
            impactRangeMax = "none"

        self.debug_print(
            "DEBUG: impactMin = '%s'  impactMax = '%s' impactRangeMax = '%s'" %
            (impactMin, impactMax, impactRangeMax), 1)

        # Determine dominant impact category for rest of CWA - No impact
        if impactMin == "none" and impactMax == "none":
            impactRange = (
                "Little to no "
                + self._frame("additional")
                + " impacts are anticipated at this time across "
                + self._cwa_descriptor()
                + "."
            )
        # Otherwise, at least some impact will be experienced across the CWA
        else:
            # Do not permit the lowest category to be "None", if the highest category
            # is also not "None". This is to avoid poor impact range wording in
            # situations of tight gradients across a CWA (e.g. "None to High")
            if impactMin == "none" and impactMax != "none":
                impactMin = "limited"

            if impactMin == impactMax:
                impactRange = impactMax
                impactRangeMax = impactMax
            elif impactMin == impactRangeMax:
                impactRange = impactRangeMax
            else:
                impactRange = impactMin + " to " + impactRangeMax

        self._samplingDict[threatName]["impactMin"] = impactMin
        self._samplingDict[threatName]["impactMax"] = impactMax
        self._samplingDict[threatName]["impactRange"] = impactRange
        self._samplingDict[threatName]["impactRangeMax"] = impactRangeMax

    ###############################################################
    ### Area, Zone and Segment related methods

    def _createWholeDomainEditArea(self, argDict):
        editAreaUtils = EditAreaUtils.EditAreaUtils()
        editAreaUtils.setUp(None, argDict)

        gridLoc = editAreaUtils.getGridLoc()
        grid2Dbit = JavaGrid2DBit(gridLoc.gridSize().x, gridLoc.gridSize().y)
        grid2Dbit.setAllValues(1)

        refID = ReferenceID("WholeDomain")
        refData = ReferenceData(gridLoc, refID, grid2Dbit)
        editAreaUtils.saveEditAreas([refData])

    ###############################################################
    ### Hazards related methods

    def _determineHazards(self, segments):
        # Return a list of hazards from the given segments in the form:
        #    (key, landList, marineList, coastalList, inlandList)
        #  where key is (hdln, act, phen, sig) and the lists show which areas
        #    contain the hazard separated by category
        hazAreaList = []
        for segment in segments:
            hazardTable = self._argDict["hazards"]
            hazards = hazardTable.getHazardList(segment)
            for hazard in hazards:
                hazAreaList.append((hazard, segment))
        # Consolidate hazards (there could be multiple segments with the same phen/sig/act)
        hazardDict = {}
        hazardList = []
        for hazard, segment in hazAreaList:
            key = (hazard["hdln"], hazard["act"], hazard["phen"], hazard["sig"])
            if key not in hazardDict:
                hazardDict[key] = segment
                hazardList.append(key)
            else:
                hazardDict[key] = hazardDict[key] + segment

        self.debug_print("hazardList =\n\n%s\n" % (self._pp.pformat(hazardList)), 1)

        return hazardList

    ###############################################################
    ### Time related methods

    def _formatLocalTime(self, para, areas):
        # Create a time string in local time
        #  e.g.  2 AM EDT
        # Get the Z time hour
        timeSearch = re.compile("...([0-9]+) *(Z|UTC)...")
        timeStr = timeSearch.search(para)

#         gmtStr = para[timeStr.start():timeStr.end()]
#         gmt = gmtStr.strip("...").replace("Z","")
#         gmtHour = int(gmt/100)

        # This code could bomb in the unlikely event we don't find a UTC
        # time.  We should probably add some kind of default hour here,
        # keyed off the current hour, to prevent this.  (MHB)
        try:
            # Convert the hour portion of the time string to an integer
            gmtHour = int(timeStr.group(1)[:2])
        except BaseException:
            gmtHour = time.gmtime().tm_hour

        gmtTR = self.createTimeRange(gmtHour, gmtHour + 1, "Zulu")
        gmtTime = gmtTR.startTime().unixTime()

        # Now make a string for each time zone
        zoneList = self._getTimeZoneList(areas)
        timeStrs = []
        timeDesc = ""
        for timeZone in zoneList:
            timeStr = self.formatTimeString(gmtTime, "%I %p %Z ", timeZone)
            timeStr = timeStr.replace("  ", " ").strip().lstrip("0")
            if timeStr not in timeStrs:
                if len(timeStrs) > 0:
                    timeDesc += "...OR "
                timeStrs.append(timeStr)
                timeDesc += timeStr
        return timeDesc

    def _getTimeZoneList(self, areaList):
        # NOTE -- this code was taken from the middle of getAreaHeader
        # in Header.py -- it really should be put back in and used
        # in Header.py, but to avoid confusion, I'm repeating it here
        # get this time zone
        thisTimeZone = os.environ["TZ"]
        zoneList = []
        # check to see if we have any areas outside our time zone
        for areaName in areaList:
            if areaName in self._areaDict:
                entry = self._areaDict[areaName]
                if "ugcTimeZone" not in entry:  # add your site tz
                    if thisTimeZone not in zoneList:
                        zoneList.append(thisTimeZone)
                    continue  # skip this entry
                timeZoneList = entry["ugcTimeZone"]
                if isinstance(timeZoneList, str):  # a single value
                    timeZoneList = [timeZoneList]  # make it into a list
                for timeZone in timeZoneList:
                    if timeZone not in zoneList:
                        zoneList.append(timeZone)
        # if the resulting zoneList is empty, put in our time zone
        if len(zoneList) == 0:
            zoneList.append(thisTimeZone)
        # if the resulting zoneList has our time zone in it, be sure it
        # is the first one in the list
        try:
            index = zoneList.index(thisTimeZone)
            if index != 0:
                del zoneList[index]
                zoneList.insert(0, thisTimeZone)
        except BaseException:
            pass
        return zoneList

    ###############################################################
    ### Storm Information and TCP related methods

    def _grabHeadline(self, text=""):
        # Get first headline found in text and return it as a string

        self.debug_print("_grabHeadline text = '%s'" % (text))

        # Fixed pattern to grab headline (MHB 04/08/2009)
        # See if there is a headline in this text
        headlineSearch = re.findall(r"(?ism)^(\.{3}.+?\.{3}) *\n", text)

        self.debug_print("old headlineSearch = %s" % (headlineSearch), 1)

        # If we could not find original headlines, try to use 'new' HLS style
        if headlineSearch is None or headlineSearch == []:
            headlineSearch = re.findall(r"(?ism)^\*\*.+?\*\* *\n", text)

        self.debug_print("now headlineSearch = %s" % (headlineSearch), 1)

        # If we found a headline
        if len(headlineSearch) > 0:

            # Remove the first and last ellipses - if they exist
            headlineSearch[0] = re.sub(r"^\.\.\.", "", headlineSearch[0])
            headlineSearch[0] = re.sub(r"\.\.\.$", "", headlineSearch[0])

            # Remove the first and last '**' - if they exist
            headlineSearch[0] = headlineSearch[0].replace("**", "").strip()

            # Return the first cleaned-up headline string we found
            return self._cleanText(headlineSearch[0])

        # Otherwise, return an indicator there is no headline in this text
        else:
            return ""  # Changed to an null string instead of None
            # (MHB  04/08/2009)

    def _getStormInfo(self, argDict):
        # Get the Storm information
        self._stormType = "Tropical"
        self._stormName = "Cyclone"
        self._stormTypeName = self._stormType + " " + self._stormName

        stormDict = self._grabStormInfo(self._TCP)
        self._stormName = stormDict.get("StormName", "")
        self._stormType = stormDict.get("StormType", "")
        self._stormTypeName = self._stormType + " " + self._stormName
        self._decodeStormInfo(stormDict)
        # Storm movement in mph and the stated movement trend
        self._stormMovementTrend = self._expandBearings(
            "Movement " + stormDict.get("StormMotion", "")
        )
        # Storm intensity in mph and the stated intensity trend.
        self._stormIntensityTrend = "Storm Intensity " + stormDict.get("StormIntensity", "")

        self.debug_print("Begin storm information", 1)
        self.debug_print("storm dict = %s" % (stormDict), 1)
        self.debug_print("storm name = %s" % (self._stormName), 1)
        self.debug_print("type = %s" % (self._stormType), 1)
        self.debug_print("type name = %s" % (self._stormTypeName), 1)
        self.debug_print("time = %s" % (self._stormTime), 1)
        self.debug_print("lat = %s" % (self._stormLat), 1)
        self.debug_print("lon = %s" % (self._stormLon), 1)
        self.debug_print("location = %s" % (str(self._stormLocation)), 1)
        self.debug_print("reference = %s" % (self._stormReference), 1)
        self.debug_print("references = %s" % (self._stormLocalReferences), 1)
        self.debug_print("movement trend = %s" % (self._stormMovementTrend), 1)
        self.debug_print("intensity trend = %s" % (self._stormIntensityTrend), 1)
        self.debug_print("End storm information", 1)

    def _grabStormInfo(self, tcp):
        # Get the storm information from the selected TCP
        # return a dictionary
        # Initialize a dictionary to hold the information we want
        dict = {
            "StormType": "|* fill in storm type here *|",
            "StormName": "|* fill in storm name here *|",
            "StormTime": "|* Enter storm time *| ",
            "StormLat": "",
            "StormLon": "",
            "StormReference": "",
            "StormIntensity": "",
            "StormMotion": "",
            "StormInfo": "",
            "StormCenter": "",
        }
        #======================================================================
        # If we got the latest public advisory

        if tcp is not None and len(tcp) > 0:

            #==================================================================
            # Try to determine the storm type and name automatically

            # Updated version to handle WFO GUM advisories.  This pattern will
            # handle multiple word names (including certain special characters)
            # This is for the NHC format.
            mndSearch = re.search(
                "(?im)^.*?(HURRICANE|(POTENTIAL\s|SUB|POST.?)"
                + "?TROPICAL\s(STORM|DEPRESSION|CYCLONE)|"
                + "(SUPER\s)?TYPHOON|REMNANTS\sOF)\s"
                + r"([A-Z0-9\-\(\)\s]+?)"
                + "(SPECIAL\s|INTERMEDIATE\s)?ADVISORY",
                tcp,
            )

            # Display some debug info - if flag is set
            self.debug_print("mndSearch = '%s'" % (mndSearch), 1)

            # If we found the storm type and name in the MND header
            if mndSearch is not None:

                # Pick off the storm type and name
                dict["StormType"] = mndSearch.group(1).strip()
                dict["StormName"] = mndSearch.group(5).strip()

            ###################################################################
            ###################################################################
            # 12/15/2010 (MHB) - we should not need this anymore, but will
            # leave it for the 2011 season as a fail-safe.

            # Look for the HPC format instead
            else:

                mndSearch = re.search(
                    "(?im)^PUBLIC ADVISORY.+?FOR REMNANTS OF ([A-Z0-9\-\(\) ]+)", tcp
                )

                # If we found the storm type and name in the MND header
                if mndSearch is not None:

                    # Pick off the storm type and name
                    dict["StormType"] = "Remnants of"
                    dict["StormName"] = mndSearch.group(1).strip()

            # end possible removal - 12/15/2010 (MHB)
            ###################################################################
            ###################################################################

            #==================================================================
            # Clean up the product for easier parsing

            tcp = self._cleanText(tcp)

            #==================================================================
            # Now try to grab the latest storm information

            # Look for the new NHC format first
            summarySearch = re.search(
                "(?is)SUMMARY OF (.+?)\.{3}.+?"
                + r"LOCATION\.{3}(.+?[NS]) +(.+?[WE]).+?"
                + "(ABOUT .+?)MAXIMUM SUSTAINED WIND.+?"
                + "(\d+ MPH).+?",
                tcp,
            )

            #------------------------------------------------------------------
            # If we found the NHC summary section

            if summarySearch is not None:

                # Set aside some information we'll need later on
                dict["StormTime"] = summarySearch.group(1).strip()
                dict["StormLat"] = summarySearch.group(2).strip()
                dict["StormLon"] = summarySearch.group(3).strip()
                dict["StormReference"] = summarySearch.group(4).strip()
                dict["StormIntensity"] = summarySearch.group(5).strip().lower()

                haveStormMotion = True
                if tcp.find("PRESENT MOVEMENT...STATIONARY") != -1:
                    dict["StormMotion"] = "Stationary"
                else:
                    summarySearch = re.search("PRESENT MOVEMENT\.{3}(.+?)\.{3}", tcp)

                    if summarySearch is not None:
                        dict["StormMotion"] = summarySearch.group(1).strip().lower()
                    else:
                        haveStormMotion = False

                #==============================================================
                # Use the remaining summary groups to contruct a paragraph
                # similar to the "old" TCP format, and save that for later use

                # Start the paragraph with the advisory time
                dict["StormCenter"] = "AT %s...THE CENTER OF " % (dict["StormTime"])

                # Now add some phrasing to maintain proper grammar, if needed
                if dict["StormType"] == "Remnants of":
                    dict["StormCenter"] = "%s THE" % (dict["StormCenter"])

                # Now add the storm type and storm name
                dict["StormCenter"] = "%s %s %s " % (dict["StormCenter"],
                                                     dict["StormType"],
                                                     dict["StormName"])

                # Now add the storm position
                dict["StormCenter"] = "%s WAS LOCATED AT LATITUDE %s...LONGITUDE %s." % (
                    dict["StormCenter"],
                    dict["StormLat"],
                    dict["StormLon"],
                )

                #--------------------------------------------------------------
                # Now add the primary NHC geographic reference

                # Get all the NHC references - starting with the word 'About'
                # after the first one
                referenceIndex = dict["StormReference"][4:].find("About")

                # Assume we only have one NHC reference point by default
                nhcReference = dict["StormReference"]

                self.debug_print("referenceIndex = %s" % (referenceIndex), 1)

                # If we have more than one NHC reference point
                if referenceIndex != -1:

                    # Adjust this index to account for the first 'About'
                    referenceIndex += 4

                    # Only keep the first NHC reference location
                    nhcReference = dict["StormReference"][:referenceIndex]

                # Convert any abbreviated bearings to full words
                nhcReference = self._expandBearings(nhcReference)

                # Add only first one to the summary paragraph for brevity
                dict["StormCenter"] = "%s THIS WAS %s. " % (
                    dict["StormCenter"],
                    self._removeKM(nhcReference.strip()),
                )

                #--------------------------------------------------------------
                # Add the maximum sustained wind speed phrase

                dict["StormCenter"] = "%s MAXIMUM SUSTAINED WINDS WERE %s." % (
                    dict["StormCenter"],
                    self._removeKM(dict["StormIntensity"]),
                )

                #--------------------------------------------------------------
                # Now add the storm motion

                if haveStormMotion:
                    dict["StormCenter"] = "%s THE STORM MOTION WAS %s." % (
                        dict["StormCenter"],
                        self._removeKM(dict["StormMotion"]),
                    )

            ###################################################################
            ###################################################################
            # 12/15/2010 (MHB) - we should not need this anymore, but will
            # leave it for the 2011 season as a fail-safe.
            #------------------------------------------------------------------
            # Search the product for the legacy storm info section - in case
            # the new NHC style was not found

            stormInfoSearch = re.search(
                "(?is)(AT +(\d+ +[AP]M [AECMPH][DS]T)"
                + r"\.{3}\d+ *(Z|UTC)\.{3}THE (CENTER|REMNANTS|EYE) .+)",
                tcp,
            )

            # Display some debug info - if flag is set
            self.debug_print("storminfoSearch = '%s'" % (stormInfoSearch))
            if stormInfoSearch is not None:
                self.debug_print(
                    "\n\n%s\n" % (self._pp.pformat(stormInfoSearch.groups())), 1)

            # If we found the storm info section of the product
            if stormInfoSearch is not None:
                for group in stormInfoSearch.groups():
                    self.debug_print("-" * 50, 1)
                    self.debug_print("%s\n" % (group), 1)

                # Clean this section up a bit.  Keep each paragraph separate
                # by a single <CR>, but remove all others as well as extra
                # spaces.  Then store this text in the TCP dictionary
                dict["StormInfo"] = stormInfoSearch.group(1).strip()

                # Set aside the first paragraph of the storm info since it
                # contains the TPC-provided reference point - if we haven't
                # already found this information
                if len(dict["StormCenter"].strip()) == 0:
                    dict["StormCenter"] = dict["StormInfo"].split("\n")[0]

                # If we have not already found the advisory time - get it from
                # the legacy format
                if dict["StormTime"] == "|* Enter storm time *| ":
                    dict["StormTime"] = stormInfoSearch.group(2).strip()

                # Set aside the first paragraph of the storm info since it
                # contains the TPC-provided reference point - if we haven't
                # already found this information
                if len(dict["StormCenter"].strip()) == 0:
                    dict["StormCenter"] = dict["StormInfo"].split("\n")[0]

            #==================================================================
            # Now try to grab the repeated storm information summary

            repeatInfo = re.search("(?is)(\.{3}SUMMARY.+?\.)\n *\n", tcp)
            # If we cannot find the summary, try to find a "repeating" section
            if repeatInfo is None:
                repeatInfo = re.search(r"(?is)(REPEATING.+?\.)\n *\n", tcp)
            self.debug_print(self._pp.pformat(repeatInfo), 1)

            # If we found the repeated storm information summary
            if repeatInfo is not None:

                # Clean up this paragraph
                summary = repeatInfo.group(1).strip()

                #==============================================================
                # Now try to grab the latest storm location - if we need it

                if dict["StormLat"] == "" or dict["StormLon"] == "":

                    # Search the product for the storm location section
                    locationSearch = re.search(
                        "(?is).+LOCATION.*?(\d+\.\d+ *N).+?(\d+\.\d+ *[EW])", summary
                    )

                    # Display some debug info - if flag is set
                    self.debug_print("locationSearch = '%s'" % (locationSearch), 1)
                    if locationSearch is not None:
                        self.debug_print("\n\n%s\n" %
                                         (self._pp.pformat(locationSearch.groups())), 1)

                    # If we found the storm location section of the product
                    if locationSearch is not None:

                        # Pick off the storm latitude and longitude
                        dict["StormLat"] = locationSearch.group(1).strip()
                        dict["StormLon"] = locationSearch.group(2).strip()

                #==============================================================
                # Now try to grab the latest storm intensity - if we need it

                if dict["StormIntensity"] == "":

                    # Search the product for the storm intensity section
                    intensitySearch = re.search(r"(?i).+MAXIMUM SUST.+?(\d+ *MPH)", summary)

                    # Display some debug info - if flag is set
                    self.debug_print("intensitySearch = '%s'" % (intensitySearch), 1)

                    # If we found the storm intensity section of the product
                    if intensitySearch is not None:

                        # Pick off the storm intensity
                        dict["StormIntensity"] = intensitySearch.group(1).strip()

                #==============================================================
                # Now try to grab the latest storm motion - if we need it

                if dict["StormMotion"] == "":

                    # Search the product for the storm motion section
                    motionSearch = re.search("(?i).+MOVEMENT\.{3}(.+?\d+ MPH)", summary)
                    if motionSearch is None:
                        motionSearch = re.search("(?i).+MOVEMENT(.+?\d+.+?)\.", summary)

                    # Display some debug info - if flag is set
                    self.debug_print("motionSearch = '%s'" % (motionSearch), 1)

                    # If we found the storm motion section of the product
                    if motionSearch is not None:

                        # Pick off the storm motion
                        motion = motionSearch.group(1).strip()

                        # Fix the motion (i.e no "...")
                        dict["StormMotion"] = re.sub(r"(?i)\.{3}", " the ", motion)

            # end possible removal - 12/15/2010 (MHB)
            ###################################################################
            ###################################################################

        #======================================================================
        # Display final decoded information from TCP

        self.debug_print("*" * 80, 1)
        self.debug_print("Final TCP Info...\n", 1)
        self.debug_print('dict["StormType"] = %s' % (dict["StormType"]), 1)
        self.debug_print('dict["StormName"] =  %s' % (dict["StormName"]), 1)
        self.debug_print('dict["StormTime"] =  %s' % (dict["StormTime"]), 1)
        self.debug_print('dict["StormLat"] =  %s' % (dict["StormLat"]), 1)
        self.debug_print('dict["StormLon"] =  %s' % (dict["StormLon"]), 1)
        self.debug_print('dict["StormReference"] =  %s' % (dict["StormReference"]), 1)
        self.debug_print('dict["StormIntensity"] =  %s' % (dict["StormIntensity"]), 1)
        self.debug_print('dict["StormMotion"] =  %s' % (dict["StormMotion"]), 1)
        self.debug_print('dict["StormInfo"] =  %s' % (dict["StormInfo"]), 1)
        self.debug_print('dict["StormCenter"] =  %s' % (dict["StormCenter"]), 1)

        # Return the dictionary will all the information we found in the TCP
        return dict

    def _decodeStormInfo(self, stormDict):
        self._stormTime = "|* Enter Storm Time *| "
        self._stormLat = "|* Enter Storm Lat *| "
        self._stormLon = "|* Enter Storm Lon *| "
        self._stormLocation = "|* Enter Storm Location *| "
        self._stormReference = ""
        self._stormLocalReferences = ""
        para = stormDict.get("StormCenter", "")
        self.debug_print("para %d %s" % (len(para), para), 1)
        if len(para) <= 0:
            return

        # Create the time string
        self._stormTime = self._formatLocalTime(para, self._allAreas())

        # Make a pattern to find the latest storm location
        coordPtn = re.compile(
            "(?i)(LATITUDE ([\d\.]+) ?((N|S)(O[RU]TH)?))..."
            + "(AND )?(LONGITUDE ([\d\.]+) ?((W|E)([AE]ST)?)).+?"
        )
#         + "OR ((ABOUT )?.+)")

        # Make a pattern to find the NHC reference location
        refPtn = re.compile(
            "(?i)(WAS|OR) ((ABOUT )?\d+ MILES.+?(NORTH|SOUTH|EAST|WEST).+?)\."
        )

        # Try to find these patterns in the text
        coordPtnMatch = coordPtn.search(para)
        self.debug_print("+" * 90, 1)
        self.debug_print("coordinate search...", 1)
        if coordPtnMatch is not None:
            self.debug_print("\n\n%s|n" % (self._pp.pformat(coordPtnMatch.groups())), 1)

        refPtnMatch = refPtn.search(para)
        self.debug_print("reference search...", 1)
        if refPtnMatch is not None:
            self.debug_print("\n\n%s|n" % (self._pp.pformat(refPtnMatch.groups())), 1)

        # If we found the coordinates we were after
        if coordPtnMatch is not None:

            # If we have the correct paragraph, set aside the latitude and
            # longitude info as numbers
            self._stormLat = float(coordPtnMatch.group(2))
            self._stormLon = float(coordPtnMatch.group(8))  # was 7

            # Adjust latitude and longitude as need for "other" hemispheres
            if coordPtnMatch.group(4) in ["S", "s"]:
                self._stormLat *= -1.0

            if coordPtnMatch.group(10) in ["W", "w"]:
                self._stormLon *= -1.0

            # Construct the storm location pair and remove the "Latitude " and
            # "Longitude " text
            self._stormLocation = (
                coordPtnMatch.group(1)[9:], coordPtnMatch.group(7)[10:])

        # If we found the primary NHC reference we were after
        if refPtnMatch is not None:

            # Set aside all the geographic reference text
            stormReference = refPtnMatch.group(2)

            # Watch out for some grammar gotchas with this reference
            stormReference = re.sub("(?i)^(WAS|OR) ", "", stormReference)

            # See if there are multiple geographic references
            if re.search("(?i) and ", stormReference) is not None:

                # Yes there are multiple references, so only keep the
                # first one
                stormReference = re.sub("(?i) AND .+", "", stormReference)

            # Also remove any metric distances
            self._stormReference = self._removeKM(stormReference)

        # Miles/km from chosen local reference
        self._stormLocalReferences = self._calcLocalReferences(self._stormLat, self._stormLon)

        self.debug_print("stormLocalRefs = %s" % (self._stormLocalReferences), 1)

        # Compare the NHC reference to the local references
        for localRef in self._stormLocalReferences:

            self.debug_print(
                "self._stormReference = '%s', localRef = '%s'" %
                (self._stormReference, localRef), 1)

            # Get the locations from these statements
            nhcRef = re.search("(?i)(north|south|east|west) of (.+)", self._stormReference)
            testRef = re.search("(?i)(north|south|east|west) of (.+)", localRef)

            if nhcRef is not None:
                self.debug_print("nhcRef = '%s'" % (nhcRef.group(2)), 1)

            if testRef is not None:
                self.debug_print("testRef = '%s'" % (testRef.group(2)), 1)

            # If we have a local reference that matches the national
            # center reference
            if (
                testRef is not None
                and nhcRef is not None
                and re.search("(?i)%s" % (testRef.group(2).strip()), nhcRef.group(2))
                is not None
            ):

                # Do not include the national reference
                self._stormReference = ""

    def _expandBearings(self, text):
        # Convert any abbreviated bearings to full words
        text = text.replace(" n ", " North ")
        text = text.replace(" nne ", " North-northeast ")
        text = text.replace(" ne ", " Northeast ")
        text = text.replace(" ene ", " East-northeast ")
        text = text.replace(" e ", " East ")
        text = text.replace(" ese ", " East-southeast ")
        text = text.replace(" se ", " Southeast ")
        text = text.replace(" sse ", " South-southeast ")
        text = text.replace(" s ", " South ")
        text = text.replace(" ssw ", " South-southwest ")
        text = text.replace(" sw ", " Southwest ")
        text = text.replace(" wsw ", " West-southwest ")
        text = text.replace(" w ", " West ")
        text = text.replace(" wnw ", " West-northwest ")
        text = text.replace(" nw ", " Northwest ")
        text = text.replace(" nnw ", " North-northwest ")

        return text

    # Modified 12/15/2010 (MHB) - modified to recognize the new way NHC will
    # present metric speeds.  Will continue to recognize the "old" way for
    # testing purposes as well.
    def _removeKM(self, words):
        # Remove references to KM (e.g. 420 KM... 100 KM/HR...)

        self.debug_print("words = '%s'" % (words), 1)

        kmSearch = re.compile(r"\.\.\. *[0-9]+ +(KM|KM/HR?) *\.?\.?\.?")

        # Replace metric reference with a space to keep words from mashing
        # together.
        words = kmSearch.sub(" ", words)

        # Make sure we don't have any double space issues with this text
        doubleSpaces = re.findall("  +", words)
        for doubleSpace in doubleSpaces:
            words = re.sub(doubleSpace, " ", words)

        self.debug_print("\tfinal words = '%s'" % (words), 1)
        return words

    def _cleanText(self, text=""):
        # Cleans up text for easier string searches, but retains paragraphs

        # Replace all single <CR> characters with a space
        text = re.sub("\n(?! *\n)", " ", text)

        # Ensure all text is only single-spaced
        text = re.sub(" +", " ", text)

        # Remove all spaces at the start of a new paragraph
        text = re.sub("(?m)^ +", "", text)

        # Do not allow any spaces after an ellipsis
        text = re.sub(r"\.{3} +", "...", text)

        # Finally, ensure the paragraphs are put back
        text = re.sub("\n", "\n\n", text)

        # Return the cleaned-up text
        return text

    def _calcLocalReferences(self, lat0, lon0):
        localRefs = []
        refList = self._LocalReferencePoints
#         refList.append(("Grand Cayman", (19.2, -81.4)))
        # Limit reference points
        refLimit = self._referencePointLimit()[0]
        if len(refList) > refLimit:
            refList = refList[0:refLimit]
        for label, latLon in refList:
            lat, lon = latLon
            localRef = self._calcReference(lat0, lon0, lat, lon)
            localRef += " of " + label
            localRef = localRef.replace(",", "")
            localRefs.append(localRef)
        return localRefs

    def _calcReference(self, lat0, lon0, lat1, lon1):
        # return self._oldCalcReference(lat0, lon0, lat1, lon1)
        distKm = self._distanceFromLatLon(lat0, lon0, lat1, lon1)
        distMph = distKm * 0.62
        # Round to nearest 10
        distMph = self.round(distMph, "Nearest", 10)
        distMph_str = str(int(distMph))
        direction = self._bearing(lat1, lon1, lat0, lon0)
        direction = self._dirInEnglish(direction)
        localRef = "About " + distMph_str + " miles " + direction
        self.debug_print("localRef = %s" % (localRef), 1)
        return localRef

    # Returns the distance from lat0, lon0 to lat1, lon1 in kilometers
    def _distanceFromLatLon(self, lat0, lon0, lat1, lon1):
        R = 6371.0
        lat0 = math.radians(lat0)
        lon0 = math.radians(lon0)
        lat1 = math.radians(lat1)
        lon1 = math.radians(lon1)
        dist = math.acos(math.sin(lat0) * math.sin(lat1) +
                         math.cos(lat0) * math.cos(lat1) * math.cos(lon1 - lon0)) * R
        return dist

    def _bearing(self, lat0, lon0, lat1, lon1):

        dlon = math.radians((lon0 - lon1))

        y = math.sin(dlon) * math.cos(math.radians(lat1))
        x = math.cos(math.radians(lat0)) * math.sin(math.radians(lat1)) - \
            (math.sin(math.radians(lat0)) * math.cos(math.radians(lat1)) * math.cos(dlon))

        direction = math.degrees(math.atan2(x, y)) - 90.0
        if direction < 0.0:
            direction += 360.0
        direction %= 360

        return direction

    def _dirInEnglish(self, direction):
        dirList = [
            "north",
            "north-northeast",
            "northeast",
            "east-northeast",
            "east",
            "east-southeast",
            "southeast",
            "south-southeast",
            "south",
            "south-southwest",
            "southwest",
            "west-southwest",
            "west",
            "west-northwest",
            "northwest",
            "north-northwest",
        ]
        dirIndex = int((direction + 11.25) / 22.5)
        if dirIndex > 15:
            dirIndex -= 16
        return dirList[dirIndex]

    ###############################################################
    ### Advisory related methods

    def _initializeHLSstatus(self):
        # The current advisory will be populated when getting a section's stats
        self._currentAdvisory = {
            "WindImpactMax": None,
            "WindImpactMin": None,
            "WindImpactDominant": None,
            "StormSurgeImpactMax": None,
            "StormSurgeImpactMin": None,
            "StormSurgeImpactDominant": None,
            "TornadoImpactMax": None,
            "TornadoImpactMin": None,
            "TornadoImpactDominant": None,
            "FloodingRainImpactMax": None,
            "FloodingRainImpactMin": None,
            "FloodingRainImpactDominant": None,
        }

    def _getPreviousHlsAdvisories(self):
        stormAdvisories = sorted(self._getStormAdvisoryNames(wantHLS=True), reverse=True)

        self.debug_print("DEBUG: stormAdvisories = %s" %
                         (self._pp.pformat(stormAdvisories)), 1)

        previousAdvisories = []

        # Get the current storm number from the TCP (ie. AL092016)
        curStormNumber = self._getStormNumberStringFromTCP()
        self.debug_print("DEBUG: curStormNumber = %s" % (curStormNumber), 1)

        # Filter out the advisories we wish to process further
        for advisory in stormAdvisories:

            # If this was an advisory for the current storm
            if advisory.startswith(curStormNumber) and "HLS" in advisory:

                # Load this advisory for this storm
                curAdvisory = self._loadAdvisory(advisory)
                self.debug_print("*****\n{}\n*****".format(curAdvisory))

                if curAdvisory is not None:
                    self.debug_print("DEBUG: storing advisory", 1)
                    previousAdvisories.append(curAdvisory)

        self.debug_print(
            "DEBUG: previous advisories = %s" % (self._pp.pformat(previousAdvisories)), 1
        )

        return previousAdvisories

    def _archiveCurrentAdvisory(self):
        # Determine if all actions are canceled
#         allCAN = False
#         for vtecRecord in self._getAllVTECRecords():
#             action = vtecRecord["act"]
#             if action != "CAN":
#                 allCAN = False
#                 break

#         self._currentAdvisory["AllCAN"] = allCAN
        self._currentAdvisory["CreationTime"] = self._issueTime_secs
        self._currentAdvisory["Transmitted"] = True
        self._currentAdvisory["StormName"] = self._getStormNameFromTCP()
        self._currentAdvisory["StormNumber"] = self._getStormNumberStringFromTCP()
        self._currentAdvisory["StormID"] = self._getStormIDStringFromTCP()
        self._currentAdvisory["AdvisoryNumber"] = self._getAdvisoryNumberStringFromTCP()
        # self._currentAdvisory["HazardsForHLS"] = self._getHazardsForHLS()
        self._currentAdvisory["EventContext"] = [self._GeneralOnsetTime]

        # Set the threat levels for each section
        self._currentAdvisory["WindImpactMax"] = \
            self._samplingDict["WindThreat"]["impactMax"]
        self._currentAdvisory["WindImpactMin"] = \
            self._samplingDict["WindThreat"]["impactMin"]
        self._currentAdvisory["WindImpactDominant"] = \
            self._samplingDict["WindThreat"]["inputThreatDominant"]

        self._currentAdvisory["StormSurgeImpactMax"] = \
            self._samplingDict["StormSurgeThreat"]["impactMax"]
        self._currentAdvisory["StormSurgeImpactMin"] = \
            self._samplingDict["StormSurgeThreat"]["impactMin"]
        self._currentAdvisory["StormSurgeImpactDominant"] = \
            self._samplingDict["StormSurgeThreat"]["inputThreatDominant"]

        self._currentAdvisory["FloodingRainImpactMax"] = \
            self._samplingDict["FloodingRainThreat"]["impactMax"]
        self._currentAdvisory["FloodingRainImpactMin"] = \
            self._samplingDict["FloodingRainThreat"]["impactMin"]
        self._currentAdvisory["FloodingRainImpactDominant"] = \
            self._samplingDict["FloodingRainThreat"]["inputThreatDominant"]

        self._currentAdvisory["TornadoImpactMax"] = \
            self._samplingDict["TornadoThreat"]["impactMax"]
        self._currentAdvisory["TornadoImpactMin"] = \
            self._samplingDict["TornadoThreat"]["impactMin"]
        self._currentAdvisory["TornadoImpactDominant"] = \
            self._samplingDict["TornadoThreat"]["inputThreatDominant"]

        # Intermediate advisories are indicated by a letter at the end of the advisory number.
        # The HLS advisory filename will need to be constructed differently in the case of an
        # intermediate or special advisory

        # If this is not an intermediate advisory
        if not self._currentAdvisory["AdvisoryNumber"][-1].isalpha():
            hlsadv_fname = "{}_{:02d}HLS".format(
                self._currentAdvisory["StormNumber"],
                int(self._currentAdvisory["AdvisoryNumber"]),
            )
        # Handle the case of an intermediate or special advisory
        else:
            hlsadv_fname = "{}_{:02d}{}HLS".format(
                self._currentAdvisory["StormNumber"],
                int(self._currentAdvisory["AdvisoryNumber"][:-1]),
                self._currentAdvisory["AdvisoryNumber"][-1]
            )

        self._saveAdvisory(hlsadv_fname, self._currentAdvisory
        )

    def _saveAdvisory(self, advisoryName, advisoryDict):
        self._synchronizeAdvisories()
        fileName = self._getAdvisoryFilename(advisoryName)

        self.debug_print("Saving %s to %s" % (advisoryName, fileName), 1)
        self.debug_print("advisoryDict: %s" % (self._pp.pformat(advisoryDict)), 1)

        try:
            JsonSupport.saveToJson(
                LocalizationSupport.CAVE_STATIC, self._site, fileName, advisoryDict
            )
        except Exception:
            LogStream.logProblem("Exception saving %s: %s" % (fileName, LogStream.exc()))
        else:  # No exceptions occurred
            self.debug_print("Wrote file contents for: %s" % (fileName), 1)

            # Purposely allow this to throw
            self._synchronizeAdvisories()

            self._cleanupAdvisories()

    def _cleanupAdvisories(self):
        self._synchronizeAdvisories()

        # Remove any advisories we no longer need
        advPath = self._getLocalAdvisoryDirectoryPath()

        # Get a list of all the advisories - both TCV and HLS
        filenames = os.listdir(advPath)
        filenames = [os.path.join(advPath, adv) for adv in filenames]

        self.debug_print("**  filenames = {}".format(filenames), 1)

        # Only cleanup the HLS advisories to start
        cleanupAdvisories = sorted(
            [adv.replace(".json", "") for adv in filenames if "HLS" in adv], reverse=True
        )[2:]

        # Remove any advisory more than 7 days old from creation time of this product
        # This could include TCV and HLS advisories
        now = self._argDict.get("creationTime", time.time())
        oldAdvisories = [
            adv
            for adv in filenames
            if os.path.isfile(adv) and os.stat(adv).st_mtime < (now - (7 * 86400))
        ]

        self.debug_print("cleanupAdvisories = {}   oldAdvisories = {}".format(
            cleanupAdvisories, oldAdvisories), 1
        )

        # Remove the files from the EDEX utility tree
        for adv in cleanupAdvisories + oldAdvisories:
            fname = self._getAdvisoryFilename(adv.replace(advPath + "/", ""))
            self.debug_print("Trying to delete -> {}".format(fname), 1)

            try:
                LocalizationSupport.deleteFile(
                    LocalizationSupport.CAVE_STATIC,
                    LocalizationSupport.SITE,
                    self._site,
                    fname
                )
            except BaseException as exception:
                self.debug_print("Could not delete! - {}".format(fname))
                self.debug_print(exception)

        self._synchronizeAdvisories()

    ###############################################################
    ### GUI related methods

    def _overview_list(self):
        if self._site == "HFO":
            stormInfoOptions = ["TCPCP1", "TCPCP2", "TCPCP3", "TCPCP4", "TCPCP5"]
        else:
            stormInfoOptions = ["TCPAT1", "TCPAT2", "TCPAT3", "TCPAT4", "TCPAT5"]

        stormInfoOptions.append("Enter PIL below (e.g. WRKTCP):")

        # setting the default to manual entry for the stormInfoOptions
        # ensures that the forecaster gets notified if the PIL is omitted.

        return [
            {
                "name": "ImpactsAnticipated",
                "label": "Step 1. Potential Impacts Anticipated?",
                "options": [
                    (
                        "Yes (NOTE: Any case other than dispel rumors must\n"
                        "have current TCP for storm in question)",
                        True,
                    ),
                    ("No (Dispel Rumors)", False),
                ],
                "default": "Yes (NOTE: Any case other than dispel rumors must\n"
                "have current TCP for storm in question)",
            },
            {
                "name": "StormInfo",
                "label": "Step 2. Obtain Storm Type/Name/Info",
                "options": stormInfoOptions,
                "entryField": "     ",
                "default": "Enter PIL below (e.g. WRKTCP):",
            },
            {
                "name": "IncludedImpacts",
                "label": "Step 3. Potential Impacts to Include and Order",
                "optionType": "check",
                "options": [
                    ("Wind", "windSection"),
                    ("Surge", "surgeSection"),
                    ("Flooding Rain", "floodingRainSection"),
                    ("Tornadoes", "tornadoSection"),
                    ("Other Coastal Hazards", "coastalHazardsSection"),
                ],
                "default": ["Wind", "Surge", "Flooding Rain", "Tornadoes"],
            },
            {
                "name": "LocalReferencePoints",
                "label": "Step 4. Locate Storm Relative to Local Reference Points\n(choose at most "
                + self._referencePointLimit()[1]
                + ")",
                "optionType": "check",
                "options": self._localReferencePoints(),
                "default": self._localReferencePoints_defaults(),
            },
            {
                "name": "GeneralOnsetTime",
                "label": "Step 5. General Time to Onset",
                "options": [
                    ("Watch", "check plans"),
                    ("Warning", "complete preparations"),
                    ("Conditions/Ongoing", "hunker down"),
                    ("Recovery (After last TCV)", "recovery"),
                ],
            },
            {
                "name": "PreviousText",
                "label": "Step 6. Use Previous Text?",
                "options": [
                    ("Yes (Previous Text Used Where/When Possible)", True),
                    ("No (All Text Regenerated)", False),
                ],
                "default": "No (All Text Regenerated)",
            },
            {
                "name": "NextUpdate",
                "label": "Step 7. Indicate Next Update Time",
                "options": [
                    ("As Conditions Warrant", "Conditions"),
                    ("Last Issuance", "LastIssuance"),
                    ("Enter Approximate Time (below)", "Enter"),
                ],
                "default": "Enter Approximate Time (below)",
                "entryField": "     e.g. 6 AM EDT",
            },
            {
                "name": "MainHeadline",
                "label": "Step 8. Input Main Headline (required)",
                "options": [
                    ("Enter Unique Headline (to right)", "Enter"),
                    ("Use Previous HLS Headline", "UsePrev"),
                    ("Use Latest TCP Headline", "UseTCP"),
                ],
                "entryField": "     ",
            },
        ]

    def _displayGUI(self, infoDict=None):
        dialog = Overview_Dialog(self, "HLS", infoDict)
        status = dialog.status()
        LogStream.logVerbose("status=" + status)
        if status == "Cancel":
            return None
        else:
            return dialog.getVarDict()

    def _frame(self, text):
        return "|* " + text + " *|"


class Overview_Dialog(HLSTCV_Common.Common_Dialog):
    def __init__(self, parent, title, infoDict=None):
        HLSTCV_Common.Common_Dialog.__init__(self, parent, title, infoDict)

    def body(self, master):
        # build the main display dialog
        tkObject_dict = self._tkObject_dict
        overviewList = self._parent._overview_list()
        fontDict = self._parent._font_GUI_dict()

        # OVERVIEW header
        headerFG, headerFont = fontDict["headers"]
        frame = tkinter.Frame(master, relief=tkinter.GROOVE, borderwidth=1)
        frame.pack(side=tkinter.TOP, fill=tkinter.X, expand=tkinter.NO)

        numBoxes = 3

        boxes = []
        for i in range(numBoxes):
            newBox = tkinter.Frame(master)
            newBox.pack(side=tkinter.TOP, expand=tkinter.NO,
              fill=tkinter.Y, anchor=tkinter.W)
            boxes.append(newBox)

        for infoDict in overviewList:
            name = infoDict["name"]
            label = infoDict["label"]
            options = infoDict.get("options", [])
            entryField = infoDict.get("entryField", None)
            default = infoDict.get("default", None)
            optionType = infoDict.get("optionType", "radio")

            index = overviewList.index(infoDict)
            if index in [0, 1, 2]:
                boxNum = 0
                buttonSide = tkinter.TOP
                frameSide = tkinter.LEFT
            elif index in [3, 4, 5]:
                boxNum = 1
#                 buttonSide = tkinter.LEFT
#                 frameSide = tkinter.TOP
                buttonSide = tkinter.TOP
                frameSide = tkinter.LEFT
            else:
                boxNum = 2
                buttonSide = tkinter.TOP
                frameSide = tkinter.LEFT

            box = boxes[boxNum]

            if name == "MainHeadline":
                entryField = None

            if name == "IncludedImpacts":
                tkObject_dict[name] = self._makeStep3(
                    box,
                    label,
                    options,
                    default,
                    buttonSide=buttonSide,
                    frameSide=frameSide,
                    entryField=entryField,
                    headerFG=headerFG,
                    headerFont=headerFont,
                )
            else:
                tkObject_dict[name], entryObject = self._makeRadioOrCheckList(
                    box,
                    label,
                    options,
                    default,
                    buttonSide=buttonSide,
                    frameSide=frameSide,
                    entryField=entryField,
                    headerFG=headerFG,
                    headerFont=headerFont,
                    boxType=optionType,
                )
                if entryObject is not None:
                    tkObject_dict[self._entryName(name)] = entryObject

            if name == "MainHeadline":
                frame = tkinter.Frame(box, relief=tkinter.GROOVE, borderwidth=1)
                tkObject_dict[self._entryName(name)] = self._makeEntry(frame, "", 80)
                frame.pack(fill=tkinter.X, expand=tkinter.YES)

        # Buttons
        frame = tkinter.Frame(master, relief=tkinter.GROOVE, borderwidth=1)
        self._makeButtons(frame)
        frame.pack(side=tkinter.TOP, fill=tkinter.X, expand=tkinter.NO)

    def _makeStep3(
        self,
        master,
        label,
        elementList,
        default=None,
        buttonSide=tkinter.TOP,
        frameSide=tkinter.LEFT,
        entryField=None,
        headerFG=None,
        headerFont=None,
        listFrameRelief=tkinter.GROOVE,
    ):
        listFrame = tkinter.Frame(master, relief=listFrameRelief, borderwidth=1)

        if label != "":
            listLabel = tkinter.Label(listFrame, text=label, fg=headerFG, font=headerFont)
            listLabel.pack(side=tkinter.TOP, fill=tkinter.X, expand=tkinter.NO, padx=10)

        ivar = tkinter.IntVar()
        ivarEntryPairList = []
        for element in elementList:
            if isinstance(element, tuple):
                element, key = element

            ivar = tkinter.IntVar()
            if default is not None and element in default:
                ivar.set(1)
            else:
                ivar.set(0)

            buttonFrame = tkinter.Frame(listFrame)

            button = tkinter.Checkbutton(buttonFrame, variable=ivar, text=element)
            button.grid(row=0, column=0, sticky=tkinter.W + tkinter.E)
            button.grid_columnconfigure(0, weight=1)

            svar = tkinter.StringVar()
            entry = tkinter.Entry(buttonFrame, textvariable=svar, relief=tkinter.SUNKEN, width=3)
            entry.grid(row=0, column=1, sticky=tkinter.E)

            ivarEntryPairList.append((ivar, svar))

            buttonFrame.pack(side=buttonSide, fill=tkinter.X, expand=tkinter.YES, padx=4)

        noteLabel = tkinter.Label(
            listFrame, text="Note: Check Hazards to include (left) and order number (right)"
        )
        noteLabel.pack(side=tkinter.TOP, fill=tkinter.X, expand=tkinter.NO, padx=10)

        # packing
        listFrame.pack(
            side=frameSide, expand=tkinter.NO, fill=tkinter.Y
        )  # , anchor=tkinter.N)

        return ivarEntryPairList

    def _makeButtons(self, master):
        frame = tkinter.Frame(master)
        buttonList = self._parent._GUI1_configDict().get("buttonList", [])
        for button, label in buttonList:
            if button == "Run":
                command = self.okCB
            else:  # Cancel
                command = self.cancelCB
            tkinter.Button(
                frame, text=label, command=command, width=10, state=tkinter.NORMAL
            ).pack(side=tkinter.LEFT, pady=5, padx=10)
        frame.pack()

    def okCB(self):
        # pull the data from the tkObject_dict before they get toasted
        tkObject_dict = self._tkObject_dict
        overviewList = self._parent._overview_list()
        print("in okCB!")
        for infoDict in overviewList:
            name = infoDict["name"]
            options = infoDict.get("options", [])
            entryField = infoDict.get("entryField", None)
            optionType = infoDict.get("optionType", "radio")

            if optionType == "check":
                checkList = []
                ivarList = tkObject_dict[name]
                for i in range(len(options)):
                    if name == "IncludedImpacts":
                        ivar, svar = ivarList[i]
                        if ivar.get():
                            checkList.append((options[i], svar.get()))
                    else:
                        if ivarList[i].get():
                            print("adding option = %s" % (self._pp.pformat(options[i])))
                            checkList.append(options[i])
                value = checkList
                self._setVarDict(name, value)
            else:
                value = tkObject_dict[name].get()
                self._setVarDict(name, value, options)

            if entryField is not None:
                entryName = self._entryName(name)
                self._setVarDict(entryName, tkObject_dict[entryName].get())
        # close window and set status "Ok"
        self._status = "Ok"
        self.withdraw()
        self.ok()


class LegacyFormatter:

    def __init__(self, textProduct):
        self._textProduct = textProduct
        self.TAB = " " * self._textProduct._tabLength
        self._tpc = HLSTCV_Common.TextProductCommon()

    def execute(self, productDict):
        self.productDict = productDict
        productParts = self._tpc.getVal(productDict, "productParts", [])
        text = self._processProductParts(productDict, productParts.get("partsList"))
        return text

    def _processProductParts(self, productDict, productParts, skipParts=[]):
        """
        Adds the product parts to the product
        @param productDict -- dictionary of information -- could be the product dictionary or a sub-part such as a segment
        @param skipParts -- necessary to avoid repetition when calling this method recursively
        @param productParts -- list of instances of the ProductPart class with information about how to format each product part
        @return text -- product string
        """
        text = ""
        self._textProduct.debug_print("productParts = %s" % (productParts))
        for part in productParts:
            valtype = type(part)
            if valtype is str:
                name = part
            elif valtype is tuple:
                name = part[0]
                infoDicts = part[1]
                self._textProduct.debug_print("name = %s" % (name), 1)
                self._textProduct.debug_print(
                    "infoDicts =\n\n%s\n" % (self._pp.pformat(infoDicts)), 1
                )
                newtext = self.processSubParts(productDict.get(name), infoDicts)
                self._textProduct.debug_print("newtext type = %s" % (type(newtext)), 1)
                self._textProduct.debug_print(
                    "newtext =\n\n%s\b" % (self._pp.pformat(newtext)), 1
                )
                text += newtext
                continue
            elif valtype is list:
                self._textProduct.debug_print("GOT HERE -- found list", 1)
                self._tpc.flush()
                # TODO THIS SHOULD BE REMOVED AFTER THE REFACTOR OF
                # HazardServicesProductGenerationHandler.JAVA
                tup = (part[0], part[1])
                part = tup
                name = part[0]

            if name == "wmoHeader":
                text += self.processWmoHeader(productDict["wmoHeader"])
            elif name == "ugcHeader":
                text += productDict["ugcHeader"] + "\n\n"
            elif name == "productHeader":
                text += self.processProductHeader(productDict["productHeader"])
            elif name == "vtecRecords":
                for vtecString in productDict["vtecRecords"]:
                    text += vtecString + "\n"
            elif name == "areaList":
                text += productDict["areaList"] + "\n\n"
            elif name == "issuanceTimeDate":
                text += productDict["issuanceTimeDate"] + "\n\n"
            elif name == "summaryHeadlines":
                text += self.processSummaryHeadlines(productDict["summaryHeadlines"])
            elif name == "newInformationHeader":
                header = "NEW INFORMATION"
                text += header + "\n" + "-" * len(header) + "\n\n"
            elif name == "changesHazards":
                text += "* CHANGES TO WATCHES AND WARNINGS:\n" + \
                        self.processHazards(productDict["changesHazards"], isChangesHazards=True)
            elif name == "currentHazards":
                text += "* CURRENT WATCHES AND WARNINGS:\n" + \
                        self.processHazards(productDict["currentHazards"], isChangesHazards=False)
            elif name == "stormInformation":
                text += self.processStormInformation(productDict["stormInformation"])
            elif name == "situationOverview":
                text += self.processSituationOverview(productDict["situationOverview"])
            elif name == "sigPotentialImpacts":
                header = "POTENTIAL IMPACTS"
                text += header + "\n" + "-" * len(header) + "\n\n"
                if not self._textProduct._ImpactsAnticipated:
                    text += "None\n\n"
            elif name in [
                "windSection",
                "surgeSection",
                "floodingRainSection",
                "tornadoSection",
            ]:
                text += self.processHazardsSection(productDict[name])
            elif name == "coastalHazardsSection":
                text += "* OTHER COASTAL HAZARDS:\n"
                text += self._textProduct.indentText(
                    productDict[name], maxWidth=self._textProduct._lineLength) + "\n"
            elif name == "preparednessSection":
                header = productDict[name]["title"]
                text += header + "\n" + "-" * len(header) + "\n\n"
                if productDict[name]["genericAction"] is not None:
                    text += self._textProduct.indentText(
                        productDict[name]["genericAction"],
                        maxWidth=self._textProduct._lineLength) + "\n"
            elif name == "evacuationStatements":
                text += "* " + productDict[name]["title"].upper() + ":\n|* "
                for statement in productDict[name]["statements"]:
                    text += self._textProduct.indentText(
                        statement, maxWidth=self._textProduct._lineLength) + "\n"
                text += "*|\n"
            elif name == "otherPreparednessActions":
                text += "* " + productDict[name]["title"].upper() + ":\n|* "
                for action in productDict[name]["actions"]:
                    text += self._textProduct.indentText(
                        action, maxWidth=self._textProduct._lineLength) + "\n"
                text += "*|\n"
            elif name == "additionalSourcesInfo":
                text += "* " + productDict[name]["title"].upper() + ":\n"
                for source in productDict[name]["sources"]:
                    text += self._textProduct.indentText(
                        source, maxWidth=self._textProduct._lineLength)
                text += "\n"
            elif name == "nextUpdate":
                header = "NEXT UPDATE"
                text += header + "\n" + "-" * len(header) + "\n\n"
                text += self._textProduct.indentText(
                    productDict[name], maxWidth=self._textProduct._lineLength) + "\n"
            elif 'sectionHeader' in name:
                text += "* " + productDict[name].upper() + "\n"
            elif "Subsection" in name:
                text += self.processSubsection(productDict[name])
            elif name == "infoSection":
                text += self.processInfoSection(productDict["infoSection"])
            elif name == "endProduct":
                text += "$$\n"
            elif name == "CR":
                text += "\n"
            elif name == "doubleAmpersand":
                text += "&&\n"
            elif name not in self._noOpParts():
                textStr = productDict.get(name)
                self._textProduct.debug_print("name = %s" % (name), 1)
                self._textProduct.debug_print("textStr = '%s'" % (textStr), 1)
                if textStr:
                    text += textStr + "\n"
        return text

    def _noOpParts(self):
        """
        These represent product parts that should be skipped when calling product part methods.
        They will be handled automatically by the formatters.
        """
        return [
            "setup_segment"
        ]  # ["CR", "endProduct", "endSegment", "issuanceDateTime", "doubleAmpersand"]

    def processWmoHeader(self, wmoHeader):
        text = (
            "{} {} {}\n"
            "{}{}\n".format(
                wmoHeader["TTAAii"], wmoHeader["fullStationID"], wmoHeader["ddhhmmTime"],
                wmoHeader["productID"], wmoHeader["siteID"]
            )
        )
        return text

    def processProductHeader(self, headerDict):
        if not self._textProduct._ImpactsAnticipated:
            text = "Tropical Local Statement\n"
            text += "National Weather Service " + headerDict["cityState"] + "\n"
            text += headerDict["issuanceTimeDate"] + "\n\n"

        else:
            text = (
                headerDict["stormType"]
                + " "
                + headerDict["stormName"]
                + " "
                + headerDict["productName"]
            )

            advisoryText = ""
            if headerDict["advisoryType"] is not None and \
               headerDict["advisoryType"].lower() in ["intermediate", "special"]:
                advisoryText = headerDict["advisoryType"] + " "

            if headerDict["advisoryNumber"] is not None:
                advisoryText += "Advisory Number " + headerDict["advisoryNumber"]

            if len(advisoryText) > 0:
                if len(text + " " + advisoryText) > self._textProduct._lineLength:
                    text += "\n"
                else:
                    text += " "

                text += advisoryText + "\n"
            else:
                text += "\n"

            text += (
                "National Weather Service "
                + headerDict["cityState"]
                + "  "
                + headerDict["stormNumber"]
                + "\n"
            )
            text += headerDict["issuedByString"] + headerDict["issuanceTimeDate"] + "\n\n"

        return text

    def processSummaryHeadlines(self, headlinesList):
        if headlinesList in [[], [""]]:
            text = "**" + self._textProduct._frame("Enter headline here") + "**\n\n"
        else:
            text = ""
            for headline in headlinesList:
                text += self._textProduct.indentText(
                    "**" + headline + "**  ", maxWidth=self._textProduct._lineLength
                )

            text = self._textProduct._frame(text) + "\n\n"

        return text

    def processHazards(self, hazardsList, isChangesHazards):
        text = ""

        if len(hazardsList) == 0:
            if (
                isChangesHazards
                and self._textProduct._ImpactsAnticipated
                and self._textProduct._GeneralOnsetTime == "recovery"
            ):
                text = self.TAB + "- All watches and warnings have been canceled\n"
            else:
                text = self.TAB + "- None\n"
        else:
            # Group the hazards together by status, areas and headlines
            groupedHazards = dict()

            # Grab the appropriate headlines for this section
            actions = ["NEW", "EXA"]  # always want these
            if isChangesHazards:
                actions.append("CAN")  # only want these for 'changes'
            else:
                actions.append("CON")  # only want these for 'current'

            for hazard in hazardsList:
                # If this is an action we want, update the active headline
                if hazard["act"] in actions:
                    headline = hazard["phensig"][:4]

                # Group areas together which have identical hazards
                groupedHazards = self._addToGroupedHazards(hazard, headline, groupedHazards)

            self._textProduct.debug_print("groupedHazards = %s"
                                          % self._textProduct._pp.pformat(groupedHazards), 1)

            groupedHazards = self._consolidateGroupedHazards(groupedHazards, isChangesHazards)

            self._textProduct.debug_print("consolidated groupedHazards = %s"
                                          % self._textProduct._pp.pformat(groupedHazards), 1)

            self._textProduct.debug_print("\n\nCreating text bullets for %s section..."
                                          % ("CHANGES" if isChangesHazards else "CURRENT"), 1)

            # Create the hazard text using the grouped hazards
            for (sortedAreas, hazards) in groupedHazards.items():
                self._textProduct.debug_print("Creating text for hazards covering %s (%s)"
                                              % (sortedAreas, self._areaWords(sortedAreas)), 1)

                if isChangesHazards:
                    hazardTextParts = self._createChangesTextParts(hazards)
                else:
                    hazardTextParts = self._createCurrentTextParts(hazards)

                self._textProduct.debug_print("hazardTextParts = %s"
                                              % self._textProduct._pp.pformat(hazardTextParts), 1)

                groupedHazardText = self._textProduct.punctuateList(hazardTextParts)
                groupedHazardText = groupedHazardText.replace(" and The ", " and the ")
                groupedHazardText = groupedHazardText.replace(" and A ", " and a ")
                groupedHazardText = groupedHazardText.replace(", The ", ", the ")
                groupedHazardText = groupedHazardText.replace(", A ", ", a ")

                groupedHazardText += " for " + self._areaWords(sortedAreas)

                self._textProduct.debug_print("groupedHazardText = '%s'" % groupedHazardText, 1)

                text += self._textProduct.indentText(
                    groupedHazardText,
                    indentFirstString=self.TAB + "- ",
                    indentNextString=self.TAB + "  ",
                    maxWidth=self._textProduct._lineLength,
                )

                self._textProduct.debug_print("text = '%s'" % text, 1)

        text += "\n"

        return text

    def _addToGroupedHazards(self, hazard, headline, groupedHazards):
        # Only consider certain hazard statuses
        status = hazard["act"]
        if status not in ["CON", "NEW", "EXA", "CAN", "UPG"]:
            return groupedHazards

        self._textProduct.debug_print("\n\nIn _addToGroupedHazardsList, adding a '%s' hazard"
                                      % status, 1)

        areas = hazard["id"]
        sortedAreas = tuple(sorted(areas))
        previousPhenSig = None
        upgrades = dict()

        if status in ["NEW", "EXA"] and hazard.get("upgradeFrom", None) is not None:

            # The phensig of the hazard that got upgraded to this headline
            previousPhenSig = hazard["upgradeFrom"]["phensig"]
            upgrades[headline] = set([previousPhenSig])

        self._textProduct.debug_print("Areas affected %s (%s)"
                                      % (sortedAreas, self._areaWords(sortedAreas)), 1)
        self._textProduct.debug_print("Headline = %s" % headline, 1)
        self._textProduct.debug_print("previousPhenSig = %s" % previousPhenSig, 1)
        self._textProduct.debug_print("upgrades = %s" % upgrades, 1)

        self._textProduct.debug_print("Trying to find where to put the hazard info...", 1)

        #======================================================================

        if sortedAreas not in groupedHazards:

            self._textProduct.debug_print("Creating a new areas entry...", 1)

            groupedHazards[sortedAreas] = {
                status: ([headline], upgrades),
            }

        # If we already have a record for this area
        else:

            self._textProduct.debug_print("Adding to an existing areas entry...", 1)

            if status == "UPG":

                # Reset the "previous" phensig
                previousPhenSig = hazard["phensig"][:4]
                self._textProduct.debug_print("Now previousPhenSig = %s" %
                                              previousPhenSig, 1)

                # Determine the upgrade
                for action in groupedHazards[sortedAreas]:
                    if action not in ["NEW", "EXA"]:
                        continue

                    sortedHeadlines, upgrades = groupedHazards[sortedAreas][action]

                    foundUpg = False
                    for hl in sortedHeadlines:
                        if (
                            hl in VTECTable.upgradeHazardsDict
                            and previousPhenSig in VTECTable.upgradeHazardsDict[hl]
                        ):
                            headline = hl
                            foundUpg = True
                            break

                    if foundUpg:
                        # add to upgrades and we are done
                        upgrades.setdefault(headline, set()).add(previousPhenSig)
                        break

            else:
                (sortedHeadlines, upgrades) = groupedHazards[sortedAreas].setdefault(
                    status, ([], {})
                )

                sortedHeadlines.append(headline)
                sortedHeadlines.sort(key=self._sortHazardsType)

                if (
                    previousPhenSig is not None
                    and headline in VTECTable.upgradeHazardsDict
                    and previousPhenSig in VTECTable.upgradeHazardsDict[headline]
                ):

                    # add to upgrades
                    upgrades.setdefault(headline, set()).add(previousPhenSig)

        return groupedHazards

    # Method to sort tropical headlines by priority, then type
    @property
    def _sortHazardsType(self):

        def cmpfunc(a, b):
            #  Warnings always go first
            if ".W" in a and ".W" not in b:
                return -1
            elif ".W" not in a and ".W" in b:
                return 1
            else:

                #  Both have the same priority of hazard, now sort by type

                #  Storm Surge headlines first
                #  just need to look for Warning vs. Watch)
                if "SS." in a and "SS." not in b:
                    return -1
                elif "SS." not in a and "SS." in b:
                    return 1
                elif "SS." in a and "SS." in b:
                    return 0

                #  Hurricane headlines next
                #  (Surge already accounted for, just need to check for Tropical)
                if "HU." in a and "TR." in b:
                    return -1
                elif "TR." in a and "HU." in b:
                    return 1
                elif "HU." in a and "HU." in b:
                    return 0

                #  Tropical Storm headlines last
                if "TR." not in a and "TR." in b:
                    return -1
                elif "TR." in a and "TR." not in b:
                    return 1
                elif "TR." in a and "TR." in b:
                    return 0

        return functools.cmp_to_key(cmpfunc)
    def _consolidateGroupedHazards(self, groupedHazards, isChangesHazards):
        """Combine areas that share the same headlines and status"""

        self._textProduct.debug_print("Trying to consolidate grouped hazards...", 1)

        newGroupedHazards = dict()

        for sortedAreas1, hazards1 in groupedHazards.items():
            self._textProduct.debug_print("Working on areas %s..."
                                          % self._textProduct._pp.pformat(sortedAreas1), 1)

            # Clean up use of EXA
            if not isChangesHazards:
                newHazards1 = dict()

                # Current section, we don't care about upgrades
                newUpgrades = dict()

                for (action, (hazard, upgrades)) in hazards1.items():

                    if action in ["NEW", "EXA"]:
                        action = "CON"

                    if action in newHazards1:
                        newHazards1[action][0].extend(hazard)
                        newHazards1[action][0].sort(key=self._sortHazardsType)

#                         newHazards1[action] = (newHazards1[action][0], dict())

#                         for phensig, upgrade in upgrades.items():
#
#                             curUpgrade = newHazard1[action][1].get(phensig, set())
#                             newHazard1[action][1][phensig] = curUpgrade | upgrade
#                         newHazards1[action][1] = newUpgrades
                    else:
                        newHazards1[action] = (hazard, newUpgrades)

                # Reset the dictionary
                hazards1 = newHazards1

            # print("Done with merging EXA and CON")

            foundMatchingInfo = False
            for sortedAreas2, hazards2 in newGroupedHazards.items():

                if hazards1 == hazards2:
                    self._textProduct.debug_print("Combining areas %s and %s..."
                                                  % (self._textProduct._pp.pformat(sortedAreas1),
                                                     self._textProduct._pp.pformat(sortedAreas2)),
                                                  1)

                    # Both areas contain the same information, combine the areas and remove
                    # duplicates
                    combinedAreas = tuple(sorted(set(sortedAreas1 + sortedAreas2)))
                    newGroupedHazards[combinedAreas] = hazards1

                    # Remove the old, uncombined areas
                    del newGroupedHazards[sortedAreas2]

                    foundMatchingInfo = True
                    break

            if not foundMatchingInfo:
                self._textProduct.debug_print("Adding new area entry...", 1)
                newGroupedHazards[sortedAreas1] = hazards1

        return newGroupedHazards

    def _createChangesTextParts(self, hazards):
        self._textProduct.debug_print("hazards = %s"
                                      % self._textProduct._pp.pformat(hazards), 1)

        hazardTextParts = []
        for status, (sortedPhensigs, upgrades) in hazards.items():

            self._textProduct.debug_print("status = %s" % status, 1)
            self._textProduct.debug_print("upgrades = %s" % upgrades, 1)
            self._textProduct.debug_print("sortedPhensigs = %s" % sortedPhensigs, 1)

            # Convert the headlines from VTEC into text
            sortedHeadlines = []
            for phensig in sortedPhensigs:
                if VTECTable.VTECTable[phensig]["hdln"] not in sortedHeadlines:
                    sortedHeadlines.append(VTECTable.VTECTable[phensig]["hdln"])

            self._textProduct.debug_print("sortedHeadlines = %s" % sortedHeadlines, 1)

            hasText = " has "
            # If there is more than one hazard
            if len(sortedHeadlines) > 1:
                hasText = " have "

            if status == "CAN":

                canHeadlines = self._textProduct.punctuateList(sortedHeadlines)
                self._textProduct.debug_print("canHeadlines = '%s'" % canHeadlines, 1)
                print(("canHeadlines = '%s'" % canHeadlines))

                canText = "The " + canHeadlines + hasText + "been cancelled"
                self._textProduct.debug_print("Result: '%s'" % canText, 1)
                hazardTextParts.append(canText)

            elif status in ["NEW", "EXA"]:
                if len(upgrades) > 0:
                    upgradeTextParts = []
                    for phensig, previousPhenSig in upgrades.items():
                        headline = VTECTable.VTECTable[phensig]["hdln"]
                        upgradeList = []

                        # Convert the list of VTEC codes to a headline
                        for prevPhenSig in previousPhenSig:
                            curHeadline = VTECTable.VTECTable[prevPhenSig]["hdln"]
                            if curHeadline not in upgradeList:
                                upgradeList.append(curHeadline)

                        upgradedHeadlines = self._textProduct.punctuateList(upgradeList)

                        upgHasText = " has "
                        # If there are no 'and's
                        if len(upgradeList) > 1:
                            upgHasText = " have "

                        upgradeTextParts.append(
                            "The "
                            + upgradedHeadlines
                            + upgHasText
                            + "been upgraded to a "
                            + headline
                        )

                        # Make sure we don't repeat information multiple times
                        # in the same section
                        try:
                            sortedHeadlines.remove(headline)
                        except BaseException:
                            print(
                                '*** Warning: attempt to remove "%s" from %s'
                                % (headline, sortedHeadlines)
                            )

                    upgradesText = self._textProduct.punctuateList(upgradeTextParts)

                    self._textProduct.debug_print("Changes Result: '%s'" % upgradesText, 1)
                    hazardTextParts.append(upgradesText)

                # NEW and EXA hazards can have both upgrades and headlines associated with them
                if len(sortedHeadlines) > 0:
                    headlines = self._textProduct.punctuateList(sortedHeadlines)
                    self._textProduct.debug_print("headlines = '%s'" % headlines, 1)

                    newExaChangesText = "A " + headlines + hasText + "been issued"
                    self._textProduct.debug_print("Changes Result: '%s'" % newExaChangesText, 1)
                    hazardTextParts.append(newExaChangesText)

        return hazardTextParts

    def _createCurrentTextParts(self, hazards):
        self._textProduct.debug_print("hazards = %s"
                                      % self._textProduct._pp.pformat(hazards), 1)

        hazardTextParts = []
        sortedHeadlines = []

        for status, (sortedPhensigs, upgrades) in hazards.items():

            self._textProduct.debug_print("status = %s" % status, 1)
            self._textProduct.debug_print("upgrades = %s" % upgrades, 1)
            self._textProduct.debug_print("sortedPhensigs = %s" % sortedPhensigs, 1)

            # Convert the headlines from VTEC into text
            for phensig in sortedPhensigs:
                if VTECTable.VTECTable[phensig]["hdln"] not in sortedHeadlines:
                    sortedHeadlines.append(VTECTable.VTECTable[phensig]["hdln"])

            self._textProduct.debug_print("sortedHeadlines = %s" % sortedHeadlines, 1)

            headlines = self._textProduct.punctuateList(sortedHeadlines)
            self._textProduct.debug_print("headlines = '%s'" % headlines, 1)

            isText = " is "
            # If there is more than one hazard
            if len(sortedHeadlines) > 1:
                isText = " are "

            if status in ["NEW", "EXA", "CON"]:
                # NEW and EXA hazards can have both upgrades and headlines associated with them
                if len(sortedHeadlines) > 0:

                    # If we are all done processing this group
                    numKeys = len(hazards)
                    numHeadlines = len(sortedHeadlines)
                    numPhensigs = len(sortedPhensigs)
#                     print("Current test keys = %d  headlines = %d  phensigs = %d" % (numKeys, numHeadlines, numPhensigs))

                    if (numKeys == numHeadlines) or (
                        numKeys == 1 and numHeadlines == numPhensigs
                    ):
                        newExaCurrentText = "A " + headlines + isText + "in effect"
                        self._textProduct.debug_print(
                            "Current Result: '%s'" % newExaCurrentText, 1
                        )
                        hazardTextParts.append(newExaCurrentText)

        return hazardTextParts

    def _areaWords(self, sortedAreas):
        if not sortedAreas:
            return ""
        names = []
        areaDict = self._textProduct._areaDict
        for area in sortedAreas:
            name = areaDict[area].get("altName", areaDict[area].get("ugcName", ""))
            names.append(name)
        names.sort()
        areaWords = self._textProduct.formatCountyString("", names)[1:]
        return areaWords

    def processStormInformation(self, stormInfoDict):
        text = "* STORM INFORMATION:\n"

        if len(stormInfoDict) == 0:
            text += self.TAB + "- None\n\n"
        else:
            referenceText = " or ".join(stormInfoDict["references"]) + "\n"
            referenceText = referenceText.replace(" or About", " or about")

            text += self._textProduct.indentText(referenceText,
                                                 indentFirstString=self.TAB + "- ",
                                                 indentNextString=self.TAB + "  ",
                                                 maxWidth=self._textProduct._lineLength)

            (lat, lon) = stormInfoDict["location"]
            text += self.TAB + "- " + lat + " " + lon + "\n"

            text += self.TAB + "- " + stormInfoDict["intensity"] + "\n"

            text += self.TAB + "- " + stormInfoDict["movement"] + "\n\n"

        return text

    def processSituationOverview(self, overviewText):
        title = "SITUATION OVERVIEW"
        text = title + "\n" + "-" * len(title) + "\n\n"

        text += self._textProduct.endline(
            overviewText, linelength=self._textProduct._lineLength
        )
        text += "\n"

        return text

    def processHazardsSection(self, sectionDict):
        text = "* " + sectionDict["title"].upper() + ":\n"

        impactRangeText = sectionDict["impactRange"]
        text += self._textProduct.indentText(
            impactRangeText, maxWidth=self._textProduct._lineLength
        )

        if self._textProduct._GeneralOnsetTime == "recovery" and sectionDict["impactLib"]:
            text += "|*\n"

        for impact in sectionDict["impactLib"]:
            text += self._textProduct.indentText(impact,
                                                 indentFirstString=self.TAB + "- ",
                                                 indentNextString=self.TAB + "  ",
                                                 maxWidth=self._textProduct._lineLength)

        if self._textProduct._GeneralOnsetTime == "recovery" and sectionDict["impactLib"]:
            text += "*|\n"

        if sectionDict["additionalImpactRange"]:
            text += "\n"

            additionalImpactRangeText = ""
            curAdditionalImpactText = ""
            count = 1

            self._textProduct.debug_print(
                "DEBUG: %d sectionDict['additionalImpactRange'] = '%s'"
                % (
                    len(sectionDict["additionalImpactRange"]),
                    sectionDict["additionalImpactRange"],
                )
            )
            for additionalImpact in sectionDict["additionalImpactRange"]:

                self._textProduct.debug_print("additionalImpact = '%s'" % (additionalImpact))
                self._textProduct.debug_print("count = %d" % (count))

                curAdditionalImpactText += self._textProduct.indentText(
                    additionalImpact.strip(), maxWidth=self._textProduct._lineLength
                )

                if (
                    count != len(sectionDict["additionalImpactRange"])
                    and curAdditionalImpactText
                ):
                    curAdditionalImpactText += "\n"

                self._textProduct.debug_print(
                    "DEBUG: curAdditionalImpactText ='%s'" % (curAdditionalImpactText)
                )

                count += 1

            # If this additional impact is not already included in the output
            if curAdditionalImpactText not in additionalImpactRangeText:

                # Add this additional impact text
                self._textProduct.debug_print(
                    "Adding current impact. '%s'" % (curAdditionalImpactText))
                additionalImpactRangeText += curAdditionalImpactText

            text += additionalImpactRangeText

        text += "\n"
        return text

    def processSubParts(self, subParts, infoDicts):
        """
        Generates Legacy text from a list of subParts e.g. segments or sections
        @param subParts: a list of dictionaries for each subPart
        @param partsLists: a list of Product Parts for each segment
        @return: Returns the legacy text of the subParts
        """
        text = ""
        for i in range(len(subParts)):
            self._textProduct.debug_print("subpart subParts[i] = %s" % (subParts[i]))
            self._textProduct.debug_print("subpart infoDicts[i] = %s" % (infoDicts[i]))
            newtext = self._processProductParts(subParts[i], infoDicts[i].get("partsList"))
            self._textProduct.debug_print("subpart newtext type = %s" % (type(newtext)))
            self._textProduct.debug_print("subpart newtext = '%s'" % (self._pp.pformat(newtext)))
            text += newtext

        return text
