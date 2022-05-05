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
#
# SOFTWARE HISTORY
#  Date       Ticket#    Engineer        Description
#  ---------- ---------- -----------     --------------------------
#  07/13/2018 DCS20374   mgamazaychikov  Updated return message in _initializeStormInformation
#  02/20/2020 DCS20651   mbelk           Added ability to use previous HLS text, when possible
#  02/28/2020 DCS20651   mbelk           Reworked methods to load either TCV or HLS advisory JSON
#  03/09/2020 DCS20651   mbelk           Bug fixes working with HLS advisory JSON
#  03/10/2020 DCS20651   mbelk           Modifications as part of code review
#  03/13/2020 DCS20651   mbelk           Added _getPartMethod to handle getting method
#                                        objects for certain parts
#  10/21/2020 DR22201    bhurley         Addressed problem in DR "GFE - TCV formatter fails due to TCP parsing when header is broken into two lines"
#  11/30/2020 8287       randerso        Fixed Python3 issue
#  01/29/2021 DR22438    mbelk           Updated makeUGCString method to prevent
#                                        UGC repeating the first zone number
#  02/05/2021 DR22440    ryu             Added elapsed time logging.
#
# Version 2021.01.29-0

from __future__ import print_function
from StartupDialog import IFPDialog as Dialog
from com.raytheon.viz.core.mode import CAVEMode
import AbsTime
import DiscretePhrases
import EditAreaUtils
import GenericHazards
import JsonSupport
import LocalizationSupport
import ModuleAccessor
import SampleAnalysis
import collections
import copy
import errno
import os
import pprint
import re
import time
import datetime

AWIPS_ENVIRON = "AWIPS2"


class TextProduct(GenericHazards.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    def __init__(self):
        GenericHazards.TextProduct.__init__(self)
        self._pp = pprint.PrettyPrinter()

    ###############################################################
    # Hazards and Additional Hazards
    # allowedHazards is used for VTEC records and summary
    # headlines
    # allowedHeadlines are additional hazards reported in
    # certain sections
    ###############################################################

    ###############################################################
    # Initialization
    ###############################################################

    ###############################################################
    # Analysis Lists, SampleAnalysis Overrides and other
    # analysis related methods
    ###############################################################

    ###############################################################
    # Product Parts Implementation
    ###############################################################

    ###############################################################
    # Product Dictionary methods for creating, populating and
    # formatting the product dictionary
    ###############################################################

    ###############################################################
    # Sampling and Statistics related methods
    ###############################################################

    ###############################################################
    # Area, Zone and Segment related methods
    ###############################################################

    ###############################################################
    # Hazards related methods
    ###############################################################

    ###############################################################
    # Time related methods
    ###############################################################

    ###############################################################
    # Storm Information and TCP related methods
    ###############################################################

    ###############################################################
    # Advisory related methods
    ###############################################################

    ###############################################################
    # GUI related methods
    ###############################################################

    ###############################################################
    # Hazards and Additional Hazards

    def allowedHazards(self):
        tropicalActions = ["NEW", "EXA", "CAN", "CON"]
        return [
            ("HU.W", tropicalActions, "Hurricane"),
            ("HU.A", tropicalActions, "Hurricane"),
            ("SS.W", tropicalActions, "Surge"),
            ("SS.A", tropicalActions, "Surge"),
            ("TR.W", tropicalActions, "Tropical"),
            ("TR.A", tropicalActions, "Tropical"),
        ]

    def allowedHeadlines(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
            ("FF.A", allActions, "Flood"),        # FLASH FLOOD WATCH
            ("FA.A", allActions, "Flood"),        # FLOOD WATCH
            ("TO.A", allActions, "Convective"),   # TORNADO WATCH
        ]

    ###############################################################
    # Initialization

    def _initializeVariables(self, argDict):
        # Get variables
        error = self._getVariables(argDict)
        if error is not None:
            return error

        self._argDict = argDict
        self._productID = self._pil[0:3].upper()

        argDict["definition"] = self._definition

        self._initializeTimeVariables(argDict)

        self._initializeHazardsTable(argDict)

        error = self._initializeStormInformation()
        if error is not None:
            return error

        # Set up the areaDictionary for all to use
        accessor = ModuleAccessor.ModuleAccessor()
        self._areaDict = accessor.variable(self._areaDictionary, "AreaDictionary")

        self._tpc = TextProductCommon()
        self._tpc.setUp(self._areaDict)

        return None

    ###############################################################
    # Analysis Lists, SampleAnalysis Overrides and other
    # analysis related methods

    def moderated_dict(self, parmHisto, timeRange, componentName):
        """
           Specifies the lower percentages and upper percentages of
           data to be thrown out for moderated stats.
        """
        # COMMENT: This dictionary defines the low and high limit at which
        # outliers will be removed when calculating moderated stats.
        # By convention the first value listed is the percentage
        # allowed for low values and second the percentage allowed
        # for high values.

        # Get Baseline thresholds
        dict = SampleAnalysis.SampleAnalysis.moderated_dict(
            self, parmHisto, timeRange, componentName
        )

        # Change thresholds
        dict["Wind"] = (0, 15)
        dict["WindGust"] = (0, 15)
        dict["pws34int"] = (0, 5)
        dict["pws64int"] = (0, 5)
        dict["pwsD34"] = (0, 5)
        dict["pwsN34"] = (0, 5)
        dict["pwsD64"] = (0, 5)
        dict["pwsN64"] = (0, 5)
        dict["InundationMax"] = (0, 3)
        dict["InundationTiming"] = (0, 3)
        return dict

    ###############################################################
    # Product Parts Implementation

    # Product Level

    def _wmoHeader(self, productDict, productSegmentGroup, arguments=None):
        headerDict = collections.OrderedDict()
        headerDict["TTAAii"] = self._wmoID
        headerDict["originatingOffice"] = self._fullStationID
        headerDict["productID"] = self._productID
        headerDict["siteID"] = self._site
        headerDict["fullStationID"] = self._fullStationID
        headerDict["ddhhmmTime"] = self._ddhhmmTime
        productDict["wmoHeader"] = headerDict

    def _productHeader(self, productDict, productSegmentGroup, arguments=None):
        headerDict = dict()
        headerDict["disclaimer"] = (
            "This XML wrapped text product should be considered COMPLETELY EXPERIMENTAL. "
            "The National Weather Service currently makes NO GUARANTEE WHATSOEVER that "
            "this product will continue to be supplied without interruption. The format "
            "of this product MAY CHANGE AT ANY TIME without notice."
        )
        headerDict["cityState"] = self._wfoCityState
        headerDict["stormNumber"] = self._getStormNumberStringFromTCP()
        # Modify the product name to indicate test or experimental mode if necessary
        self._productName = self.checkTestMode(
            self._argDict, productSegmentGroup.get("productName") + self._areaName
        )
        headerDict["productName"] = self._productName
        headerDict["stormType"] = self._getStormTypeFromTCP()
        headerDict["stormName"] = self._getStormNameFromTCP()
        headerDict["advisoryType"] = self._getAdvisoryTypeFromTCP()
        headerDict["advisoryNumber"] = self._getAdvisoryNumberStringFromTCP()
        headerDict["issuedByString"] = self.getIssuedByString()
        headerDict["issuanceTimeDate"] = self._timeLabel
        productDict["productHeader"] = headerDict

    # Mixed Level

    def _ugcHeader(self, productDict, productSegmentGroup, productSegment):
        # The UGC header is the formatted list of UGCs along with an expire time
        # For example: "FLZ066>068-071-072-063-069-073>075-168-172>174-070-230515-"
        ugcHeader = self._tpc.formatUGCs(self._ugcs, self._expireTime)
        productDict["ugcHeader"] = ugcHeader

    # Product Parts Processing

    def _getPartMethod(self, productGenerator, part):
        """This method will identify and return the method object for an associated
        product part. Some of these method objects are nested within a dictionary
        structure, making them inaccessible to the direct use of getattr.
        """

        # Initialize some variables to help identify what we have
        firstPart = part[:]
        secondPart = ""
        thirdPart = ""
        partMethod = None

        # If this part looks like a dictionary - split up the parts at start of
        # dictionary key
        if "[" in part:
            newParts = part.split("[")
            firstPart = newParts[0]
            secondPart = newParts[1].replace("]", "").replace("'", "")

        self.debug_print("after step 1 -> firstPart = '{}'   secondPart = '{}'".format(
            firstPart, secondPart), 1)

        # If we have two parts already defined, separate the dictionary key from the
        # method name
        if secondPart and "." in secondPart:
            newParts = secondPart.split(".")
            secondPart = newParts[0]
            thirdPart = newParts[1]

        self.debug_print("after step 2 -> secondPart = '{}'   thirdPart = '{}'".format(
            secondPart, thirdPart), 1)

        # Get the object associated with the first part. This could be either a direct
        # method object, or a segment dictionary
        part1 = getattr(productGenerator, "_{}".format(firstPart), None)
        self.debug_print(part1, 1)

        # If there are more parts, and we got the initial object
        if secondPart and part1:

            # Access the final method object via the dictionary entry
            self.debug_print("\tdigging deeper", 1)
            part2 = part1[secondPart]
            self.debug_print(part2)
            partMethod = getattr(part2, thirdPart, None)
        else:
            partMethod = part1

        self.debug_print(partMethod, 1)
        return partMethod

    def _processProductParts(
        self, productGenerator, productDict, productSegmentGroup, productParts
    ):
        """
        @param productDict
        @param productSegmentGroup
        @param productParts
        @return product dictionary created from the product parts

        Note that this method is called recursively such that a product part is allowed to be
        a set of subParts specified as follows:
          (subPartLabel, list of productParts for each subPart)
        For example, we have
          ("segments", [list of [segment product parts]])

        # Product Dictionary
        #   Contains information for all formats e.g.
        #   partner XML, CAP, and Legacy text
        """
        etlog = ETLogger("_processProductParts")

        if isinstance(productParts, dict):
            arguments = productParts.get("arguments")
            partsList = productParts.get("partsList")
        else:
            partsList = productParts

        removedParts = []
        for part in partsList:
            if isinstance(part, tuple):
                # e.g. subPart == "segments", subPartsLists == list of parts for each segment
                subPart, subPartsLists = part
                etlog.logElapsedTime("Before processing subpart %s" % subPart)
                subParts = []
                for subPartsList in subPartsLists:
                    subDict = collections.OrderedDict()
                    self._processProductParts(
                        productGenerator, subDict, productSegmentGroup, subPartsList
                    )
                    subParts.append(subDict)
                # e.g. productDict["segments"] = segment dictionaries
                productDict[subPart] = subParts
                etlog.logElapsedTime("Completed processing subpart %s" % subPart)
            else:
                if part not in self._noOpParts():
                    method = eval(f"productGenerator._{part}")
                    method(productDict, productSegmentGroup, arguments)
                    if part not in productDict:
                        removedParts.append(part)

        for part in removedParts:
            self.debug_print(
                "in _processProductParts - Removing product part = %s" % (part), 1
            )
            partsList.remove(part)

    ###############################################################
    # Product Dictionary methods for creating, populating and
    # formatting the product dictionary

    def _createProductDictionary(
        self, productPartsGenerator, segments, areProductPartsSegmented
    ):
        etlog = ETLogger("_createProductDictionary")

        # Create the product dictionary
        productSegmentGroup = self._groupSegments(
            productPartsGenerator, segments, areProductPartsSegmented
        )
        etlog.logElapsedTime("Completed _groupSegments")

        productParts = productSegmentGroup.get("productParts")
        productDict = self._initializeProductDictionary(productParts)
        self._processProductParts(self, productDict, productSegmentGroup, productParts)
        etlog.logElapsedTime("Completed _processProductParts")

        return productDict

    def _initializeProductDictionary(self, productParts):
        """
        Set up the Product Dictionary for the given Product consisting of a
        group of segments.

        Fill in the dictionary information for the product header.

        @param productSegmentGroup: holds meta information about the product
        @return initialized product dictionary

        ***********
        Example segmented product:

           WGUS63 KBOU 080400
           FFABOU

           URGENT - IMMEDIATE BROADCAST REQUESTED
           FLOOD WATCH
           NATIONAL WEATHER SERVICE DENVER CO
           400 AM GMT TUE FEB 8 2011

           Overview Headline
           Overview

        ***********
        Example non-segmented product:
           WGUS63 KBOU 080400
           FFWBOU

        """
        if self._areaName:
            self._areaName = " for " + self._areaName + "\n"

        # Fill in product dictionary information
        productDict = collections.OrderedDict()
        productDict["productID"] = self._productID
        productDict["productParts"] = productParts
        return productDict

    def _formatProductDictionary(self, formatterClass, productDict):
        formatter = formatterClass(self)
        product = formatter.execute(productDict)

        return product

    ###############################################################
    # Sampling and Statistics related methods

    def _getStatValue(self, statDict, element, method=None, dataType=None):
        self.debug_print("In _getStatValue looking for '%s'" % (element), 1)
        self.debug_print("method = %s" % (pprint.pformat(method)), 1)
        self.debug_print("dataType = %s" % (pprint.pformat(dataType)), 1)

        stats = statDict.get(element, None)
        self.debug_print("stats = %s" % (pprint.pformat(stats)), 1)

        if stats is None:
            return None
        if isinstance(stats, list):
            stats = stats[0]
            stats, tr = stats
        if dataType == self.VECTOR():
            stats, dir = stats

        value = self.getValue(stats, method)
        self.debug_print("value = %s" % (pprint.pformat(value)), 1)
        return value

    # Define a class to handle missing statistics
    class StatisticsException(Exception):
        pass

    ###############################################################
    # Area, Zone and Segment related methods

    def _allAreas(self):
        return self._inlandAreas() + self._coastalAreas()

    def _groupSegments(self, productPartsGenerator, segments, areProductPartsSegmented):
        """
         Group the segments into the products. The TCV and HLS product generators
         only create a single product each so there is only one product segment group.
        """

        segment_vtecRecords_tuples = self._getSegmentVTECRecordsTuples(segments)

        productSegmentGroup = {
            "productID": self._productID,
            "productName": self._productName,
            "geoType": "area",
            "vtecEngine": self._hazardsTable,
            "mapType": "publicZones",
            "segmented": areProductPartsSegmented,
            "productParts": productPartsGenerator(segment_vtecRecords_tuples),
        }

        return productSegmentGroup

    def _getSegmentVTECRecordsTuples(self, segments):
        segment_vtecRecords_tuples = []
        for segment in segments:
            vtecRecords = self._getVtecRecords(segment)
            self.debug_print(
                "vtecRecords for %s =\n\n%s\n" % (segment, self._pp.pformat(vtecRecords))
            )
            segment_vtecRecords_tuples.append((segment, vtecRecords))

        return segment_vtecRecords_tuples

    def _computeIntersectAreas(self, editAreas, argDict):
        editAreaUtils = EditAreaUtils.EditAreaUtils()
        editAreaUtils.setUp(None, argDict)
        surgeEditArea = editAreaUtils.getEditArea("StormSurgeWW_EditArea", argDict)
        intersectAreas = []
        for (_, editAreaLabel) in editAreas:
            editArea = editAreaUtils.getEditArea(editAreaLabel, argDict)
            intersectAreaLabel = "intersect_" + editAreaLabel
            intersectArea = editAreaUtils.intersectAreas(
                intersectAreaLabel, editArea, surgeEditArea
            )
            grid = intersectArea.getGrid()
            if grid.isAnyBitsSet():  # Make sure the intersection isn't empty
                # Register the new edit area with the system
                editAreaUtils.saveEditAreas([intersectArea])
                intersectAreas.append((intersectAreaLabel, intersectAreaLabel))

        return intersectAreas

    ###############################################################
    # Hazards related methods

    def _initializeHazardsTable(self, argDict):
        import VTECMessageType

        vtecMode = VTECMessageType.getVTECMessageType(self._productID)
        argDict["vtecMode"] = vtecMode

        self._setVTECActiveTable(argDict)

        # Need to check hazards against all edit areas in the CWA MAOR
        argDict["combinations"] = [(self._allAreas(), "Region1")]

        self._hazardsTable = self._getHazardsTable(argDict, self.filterMethod)
        argDict["hazards"] = self._hazardsTable

    def _getHazardsTable(self, argDict, filterMethod):
        # Set up edit areas as list of lists
        dfEditAreas = argDict["combinations"]
        editAreas = []
        for area, label in dfEditAreas:
            if isinstance(area, list):
                editAreas.append(area)
            elif isinstance(area, tuple):  # LatLon
                editAreas.append([self.__getLatLonAreaName(area)])
            else:
                editAreas.append([area])
        # Get Product ID and other info for HazardsTable
        stationID4 = self._fullStationID
        productCategory = self._productID
        definition = argDict["definition"]
        sampleThreshold = definition.get("hazardSamplingThreshold", (10, None))
        # Process the hazards
        accurateCities = definition.get("accurateCities", 0)
        import HazardsTable

        hazards = HazardsTable.HazardsTable(
            argDict["ifpClient"],
            editAreas,
            productCategory,
            filterMethod,
            argDict["databaseID"],
            stationID4,
            argDict["vtecActiveTable"],
            argDict["vtecMode"],
            sampleThreshold,
            creationTime=argDict["creationTime"],
            accurateCities=accurateCities,
            cityEditAreas=[],
            dataMgr=argDict["dataMgr"],
        )
        return hazards

    def _ignoreActions(self):
        # Ignore hazards with these action codes in the overview headlines
        # NOTE: the VTEC and segments will still include them correctly.
        return ["CAN", "UPG"]

    def _setVTECActiveTable(self, argDict):
        gfeMode = CAVEMode.getMode().name()

        self.debug_print("*" * 100, 1)
        self.debug_print("gfeMode = '%s'" % (gfeMode), 1)
        self.debug_print("*" * 100, 1)

        if gfeMode == "PRACTICE":
            argDict["vtecActiveTable"] = "PRACTICE"
        else:
            argDict["vtecActiveTable"] = "active"

    def _getVtecRecords(self, segment):
        vtecRecords = self._hazardsTable.getHazardList(segment)
        # Tropical hazards shouldn't ever have EXT and EXB actions since
        # they are "until further notice"
        for record in vtecRecords:
            if record["act"] == "EXT":
                record["act"] = "CON"
            elif record["act"] == "EXB":
                record["act"] = "EXA"

        return vtecRecords

    def _getAllowedHazardList(self, allowedHazardList=None):
        # Get the list of allowed phenSigs (ie. "HU.W")
        if allowedHazardList is None:
            allowedHazardList = self.allowedHazards()
        hazardList = []
        for h in allowedHazardList:
            if isinstance(h, tuple):
                hazardList.append(h[0])
            else:
                hazardList.append(h)
        return hazardList

    def _altFilterMethod(self, hazardTable, allowedHazardsOnly=False):
        # Remove hazards not in allowedHeadlines list
        allowedHazardList = self._getAllowedHazardList(self.allowedHeadlines())
        return self._filterHazards(hazardTable, allowedHazardList, allowedHazardsOnly)

    def _filterHazards(self, hazardTable, allowedHazardList, allowedHazardsOnly=False):
        newTable = []
        hazStr = ""
        for i in range(len(hazardTable)):
            if hazardTable[i]["sig"]:  # VTEC
                hazStr = hazardTable[i]["phen"] + "." + hazardTable[i]["sig"]
            else:  # non-VTEC
                hazStr = hazardTable[i]["phen"]

            if hazStr in allowedHazardList:
                newTable.append(hazardTable[i])
        if allowedHazardsOnly:
            return newTable
        # get a raw list of unique edit areas
        zoneList = []
        for t in newTable:
            if t["id"] not in zoneList:
                zoneList.append(t["id"])
        for zone in zoneList:
            # Remove lower priority hazards of the same type
            self.filterZoneHazards(zone, newTable)
        return newTable

    def _getAdditionalHazards(self):
        argDict = self._argDict
        argDict["definition"] = self._definition
        altHazards = self._getHazardsTable(argDict, self._altFilterMethod)
        conTable = altHazards.consolidatedTableByID()

        # Consolidate across action codes
        hazDict = {}
        for hazard in conTable:
            hdln = hazard["hdln"]
            phen = hazard["phen"]
            sig = hazard["sig"]
            act = hazard["act"]
            if act in self._ignoreActions():
                continue
            for area in hazard["id"]:
                hazDict.setdefault((hdln, phen, sig), []).append(area)

        self.debug_print("hazDict = %s" % (self._pp.pformat(hazDict)), 1)
        hazardHdlns = []
        huAreas = []
        self.debug_print("Additional Hazard Headlines", 1)
        for (key, value) in hazDict.items():
            hdln, phen, sig = key
            huAreas += value
            hazardHdln = ((hdln, "NEW", phen,sig), value, [], [], [])
            self.debug_print("   %s" % (self._pp.pformat(hazardHdln)), 1)
            self.debug_print("       %s" % (self._pp.pformat(value)), 1)
            hazardHdlns.append(hazardHdln)
        return hazardHdlns, huAreas

    def _checkHazard(
        self,
        hazardHdlns,
        phenSigList,
        checkAreaTypes=None,
        checkAreas=None,
        returnList=False,
        mode="any",
        includeCAN=False,
    ):
        # Given a list of hazards in the form
        #    (key, landList, marineList, coastalList, inlandList)
        #  where key is (hdln, act, phen, sig) and the lists show which areas
        #    contain the hazard
        # If mode == "any":
        #     Check to see if any of the given phenSigList = [(phen, sig), (phen, sig)]
        #          are found
        # If mode == "all":
        #     Check to see if all of the given phenSigList are found
        # IF checkAreaTypes is given, then check against that particular area type(s) i.e.
        #   "land", "marine", etc.
        # IF checkAreas is given, only return areas that are in that list
        # IF returnList=True, returns a list of (key, areas) that meet the criteria
        # IF includeCAN is True then CAN hazards will be included as well.
        #     Otherwise, they are ignored.
        #
        # E.g. hdlnList = self._checkHazard(hazardHdlns, [("FA","W")], returnList=True)
        self.debug_print("_checkHazard hazardHdlns is %s" % (self._pp.pformat(hazardHdlns)), 1)
        self.debug_print("_checkHazard phenSigList is %s" % (self._pp.pformat(phenSigList)), 1)
        chosen = []
        for key, landList, marineList, coastalList, inlandList in hazardHdlns:

            # We do not want to consider marine hazards in this product
            hazAreas = landList
            hazValue = (key, hazAreas)
            self.debug_print("hazValue is %s" % (repr(hazValue)), 1)
            hdln, act, phen, sig = key
            if not includeCAN and act == "CAN":
                continue
            for checkPhen, checkSig in phenSigList:
                self.debug_print("checkPhen is %s" % (checkPhen), 1)
                self.debug_print("checkSig is %s" % (checkSig), 1)
                if phen == checkPhen and sig == checkSig:
                    if checkAreaTypes is not None:
                        # Check for land, marine, etc.
                        for checkAreaType in checkAreaTypes:
                            testList = eval(f"{checkAreaType}List")
                            self.debug_print("testList is %s" % (testList), 1)
                            if testList:
                                chosen.append(hazValue)
                    elif checkAreas is not None:
                        acceptedAreas = []
                        for hazArea in hazAreas:
                            if hazArea in checkAreas:
                                acceptedAreas.append(hazArea)
                        if acceptedAreas:
                            chosen.append((key, acceptedAreas))
                    else:
                        chosen.append(hazValue)
                    if not returnList and chosen:
                        break

        self.debug_print("In _checkHazard chosen = %s" % (self._pp.pformat(chosen)), 1)
        if not returnList:
            return chosen != []
        return chosen

    ###############################################################
    # Time related methods

    def _initializeTimeVariables(self, argDict):
        argDict["creationTime"] = int(time.time() / 60) * 60
        self._issueTime_secs = argDict["creationTime"]
        self._issueTime_ms = self._issueTime_secs * 1000  # in milliseconds

        self._ddhhmmTime = self.getCurrentTime(
            argDict, "%d%H%M", shiftToLocal=0, stripLeading=0
        )
        self._purgeHours = self._purgeTime
        self._expireTime = self._issueTime_secs + self._purgeHours * 3600
        self._timeLabel = self.getCurrentTime(
            argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1
        )

    def _determineTimeRanges(self, argDict):
        # Create a 120 hour time range starting from the issuanceHour and broken into 1-hour chunks
        # Used for Wind Section
        startTime1Hour = self._calculateStartTime(self._issueTime_secs, resolution=1)
        self._timeRange1Hour = self.makeTimeRange(startTime1Hour, startTime1Hour + 120 * 3600)
        self._timeRangeList1Hour = self._createTimeRangeList(self._timeRange1Hour, hours=1)

        # Create a 120 hour time range starting from the issuanceHour and broken into 3-hour chunks
        # Used for Flooding Rain and Tornado Sections
        startTime3Hour = self._calculateStartTime(self._issueTime_secs, resolution=3)
        timeRange3Hour = self.makeTimeRange(startTime3Hour, startTime3Hour + 120 * 3600)
        self._timeRangeList3Hour = self._createTimeRangeList(timeRange3Hour, hours=3)

        # Create a time range to look from the current time back 12 hours.
        # We will use this to determine if we need to use "additional"
        # wording with rainfall for the TCV
        self._extraSampleTimeRange = self.makeTimeRange(
            startTime3Hour - 12 * 3600, startTime3Hour
        )

        # Create a 120 hour time range starting from the issuanceHour and broken into 6-hour chunks
        # Used for Storm Surge Section
        startTime6Hour = self._calculateStartTime(self._issueTime_secs, resolution=6)
        timeRange6Hour = self.makeTimeRange(startTime6Hour, startTime6Hour + 120 * 3600)
        self._timeRangeList6Hour = self._createTimeRangeList(timeRange6Hour, hours=6)
        # Create a list of 10 periods (GMT time) corresponding to 6AM and 6PM
        # times (local time)
        self._createPeriodList(startTime1Hour)

    def _createTimeRangeList(self, timeRange, hours):
        subRanges = self.divideRange(timeRange, hours)
        trList = []
        for index, tr in enumerate(subRanges):
            self.debug_print(
                "In _createTimeRangeList (%s hour chunks) -> tr = %s"
                % (hours, self._pp.pformat(tr)),
                1,
            )
            trList.append((tr, "Label"))

        return trList

    def _createPeriodList(self, startTime):
        # Create the 10 periods
        self._periodList = []

        localtime = time.localtime(startTime.unixTime())

        # Determine the number of hours to the next 6AM or 6PM period
        if localtime.tm_hour < 6:
            periodLength = 6 - localtime.tm_hour
        elif localtime.tm_hour >= 6 and localtime.tm_hour < 18:
            periodLength = 18 - localtime.tm_hour
        else:
            periodLength = 30 - localtime.tm_hour

        # Don't allow the first period to be less than 3 hours long;
        # instead just start with the next period
        if periodLength < 3:
            periodStart = startTime + periodLength * 3600
            period = self.makeTimeRange(periodStart, periodStart + 12 * 3600)
        else:
            period = self.makeTimeRange(startTime, startTime + periodLength * 3600)

        self._periodList.append(period)

        for i in range(1, 10):
            startTime = period.endTime()  # Start where the last period leaves off
            period = self.makeTimeRange(startTime, startTime + 12 * 3600)
            self._periodList.append(period)

        self.debug_print(
            "final periodList =\n\n%s\n" % (self._pp.pformat(self._periodList)), 1
        )

    def _calculateStartTime(self, localCreationTime, resolution):
        resolution *= 3600

        self.debug_print(
            "In _calculateStartTime incoming res %d   localCreationTime = %d"
            % (resolution, localCreationTime),
            1,
        )

        # Determine how far along we are in a given time block
        adjust = localCreationTime % resolution

        # If we are less than halfway though a block we would want.
        # If this is for surge (6 hour resolution aka 21600 seconds) and we
        # aren't within 30 minutes (aka 1800 seconds) of the next block, go
        # back to the start of the current block
        if adjust < resolution // 2 or (resolution == 21600 and (resolution - adjust) > 1800):
            adjust *= -1  # to move back to beginning of the block
        else:
            adjust = resolution - adjust  # go to next block

        self.debug_print("In _calculateStartTime %d   adjust = %d" % (resolution, adjust), 1)

        startTime = AbsTime.AbsTime(localCreationTime + adjust)

        return startTime

    def _formatPeriod(
        self, period, wholePeriod=False, shiftToLocal=True, useEndTime=False, resolution=6
    ):
        # Format period (a timeRange) resulting in
        # DAY + MORNING / AFTERNOON / EVENING / OVERNIGHT.
        # If wholePeriod, format FROM ... TO...

        self.debug_print(
            "Format period wholePeriod = %s, period = %s, useEndTime =%s"
            % (str(wholePeriod), str(period), str(useEndTime)),
            1,
        )
        if period is None:
            return ""
        if useEndTime:
            startTime = period.endTime()
        else:
            startTime = period.startTime()
        result = self._getTimeDesc(startTime, resolution, shiftToLocal)
        self.debug_print("_getTimeDesc result = '%s'" % (result), 1)
        if wholePeriod:
            endResult = self._getTimeDesc(period.endTime(), resolution, shiftToLocal)
            self.debug_print("_getTimeDesc endResult = '%s'" % (endResult), 1)
            if result != endResult:
                result += " TO " + endResult
        return result

    def _getTimeDesc(self, startTime, resolution=3, shiftToLocal=True):
        # Create phrase such as Tuesday morning
        # Handle today/tonight and "this" morning/afternoon/etc..
        #
        self.debug_print(
            "\n\n**************Formatting Period for GMT startTime %s" % (repr(startTime)), 1
        )
        labels = self.Labels()["SimpleWorded"]
        currentTime = self._timeRange1Hour.startTime()
        self.debug_print("   currentTime = %s" % (repr(currentTime)), 1)
        if shiftToLocal:
            currentLocalTime, shift = self.determineTimeShift()
            startTime += shift
            currentTime += shift
            self.debug_print(
                "shift = %s   shifted start = %s   current = %s"
                % (int(shift / 3600), startTime, currentTime),
                1,
            )
        hour = startTime.hour
        prevDay = False
        prevDay, partOfDay = self._getPartOfDay(hour, resolution)
#         if prevDay:
#             startTime = startTime - 24*3600
        todayFlag = currentTime.day == startTime.day
        if todayFlag:
            if partOfDay.lower().find("midnight") > 0:
                todayWord = "tonight"
            else:
                todayWord = "this"
            weekday = todayWord
        else:
            weekday = labels["Weekday"][startTime.weekday()]
        if "<weekday>" in partOfDay:
            result = partOfDay.replace("<weekday>", weekday)
        else:
            result = weekday + " " + partOfDay
        self.debug_print("Result = '%s'" % (result), 1)
        return result

    def _getPartOfDay(self, hour, resolution):
        prevDay = False
        if resolution == 3:
            if hour < 3:
                prevDay = True
                partOfDay = "early <weekday> morning"
#                 partOfDay = "after midnight"
            elif hour < 6:
                partOfDay = "early <weekday> morning"
            elif hour < 9:
                partOfDay = "morning"
            elif hour < 12:
                partOfDay = "late <weekday> morning"
            elif hour < 15:
                partOfDay = "early <weekday> afternoon"
            elif hour < 18:
                partOfDay = "late <weekday> afternoon"
            elif hour < 21:
                partOfDay = "early <weekday> evening"
            else:
                partOfDay = "late <weekday> evening"
        else:
            if hour < 6:
                prevDay = True
#                 partOfDay = "after midnight"
                partOfDay = "early <weekday> morning"
            elif hour < 12:
                partOfDay = "morning"
            elif hour < 18:
                partOfDay = "afternoon"
            else:
                partOfDay = "evening"
        return prevDay, partOfDay

    ###############################################################
    # Storm Information and TCP related methods

    # These variables were previously all set to None
    def _initializeStormInformation(self):
        self._stormType = None
        self._stormName = None
        self._advisoryType = None
        self._advisoryNumber = None
        self._stormNumber = None  # This is an 8-digit string like "AL092016"
        # This is a 2-digit string embedded in the storm number ("09" in "AL092016"
        self._stormID = None

        if self._useTestTCP():
            self._TCP = self._testTCP()
        elif "Enter PIL below" in self._StormInfo:
            if not self._StormInfo_entry.strip():
                return "You need to select the TCP PIL for this storm."
            else:
                # Ensure PIL is in UPPERCASE
                self._TCP = self.getPreviousProduct(self._StormInfo_entry.strip().upper())
        else:
            self._TCP = self.getPreviousProduct(self._StormInfo)

        self._parseTCP(self._TCP)

        return None

    def _parseTCP(self, tcp):
        # This pattern will handle multiple word names
        # (including certain special characters).
        # This is for the NHC format.
        mndSearch = re.search(
            "(?im)^.*?(?P<stormType>HURRICANE|"
            + "(POTENTIAL|SUB|POST.?)"
            + "?TROPICAL\s(STORM|DEPRESSION|CYCLONE)|"
            + "(SUPER\s)?TYPHOON|REMNANTS\sOF)\s"
            + r"(?P<stormName>[A-Z0-9\-\(\)\s]+?)"
            + "(?P<advisoryType>SPECIAL\s|INTERMEDIATE\s)"
            + "?ADVISORY\sNUMBER[\s]+"
            + "(?P<advisoryNumber>[A-Z0-9]+)[ ]*",
            tcp,
        )

        if mndSearch is not None:
            self._stormType = mndSearch.group("stormType").strip()
            self._stormName = mndSearch.group("stormName").strip()
            advisoryType = mndSearch.group("advisoryType")
            if advisoryType is not None:
                self._advisoryType = advisoryType.strip()
            self._advisoryNumber = mndSearch.group("advisoryNumber").strip()

        senderSearch = re.search(
            "(?im)^(?P<sender>(NWS (National |Central Pacific )?Hurricane Center|"
            + "National Weather Service).*?)$",
            tcp,
        )

        if senderSearch is not None:
            sender = senderSearch.group("sender")
            senderParts = sender.split(" ")
            # If the storm number is mentioned, it will be the last "word" of the line
            stormNumber = senderParts[-1].strip()
            if (
                len(stormNumber) == 8
                and stormNumber[0:2].isalpha()
                and stormNumber[2:].isdigit()
            ):
                self._stormNumber = stormNumber
                self._stormID = stormNumber[2:4]

    def _getStormTypeFromTCP(self):
        return self._stormType

    def _getStormNameFromTCP(self):
        return self._stormName

    def _getAdvisoryTypeFromTCP(self):
        return self._advisoryType

    def _getAdvisoryNumberStringFromTCP(self):
        return self._advisoryNumber

    def _getStormNumberStringFromTCP(self):
        return self._stormNumber

    def _getStormIDStringFromTCP(self):
        return self._stormID

    # Used for testing and debugging
    def _useTestTCP(self):
        # If True, will use the canned TCP text provided by the _testTCP method below.
        # If False, will use TCP text from the live text database (normal operations).
        return False

    def _testTCP(self):
        return """337
WTNT34 KNHC 250256
TCPAT4

BULLETIN
TROPICAL STORM ISAAC ADVISORY NUMBER  16
NWS NATIONAL HURRICANE CENTER MIAMI FL       AL092012
1100 PM EDT FRI AUG 24 2012

...ISAAC GETTING BETTER ORGANIZED AS IT MOVES NORTHWESTWARD TOWARD
HAITI...


SUMMARY OF 1100 PM EDT...0300 UTC...INFORMATION
-----------------------------------------------
LOCATION...17.7N 72.5W
ABOUT 65 MI...100 KM SSW OF PORT AU PRINCE HAITI
ABOUT 245 MI...395 KM SE OF GUANTANAMO CUBA
MAXIMUM SUSTAINED WINDS...70 MPH...110 KM/H
PRESENT MOVEMENT...NW OR 310 DEGREES AT 14 MPH...22 KM/H
MINIMUM CENTRAL PRESSURE...990 MB...29.23 INCHES


WATCHES AND WARNINGS
--------------------
CHANGES WITH THIS ADVISORY...

A HURRICANE WATCH AND A TROPICAL STORM WARNING HAVE BEEN ISSUED FOR
ALL OF THE FLORIDA KEYS...INCLUDING FLORIDA BAY...AND FOR THE COAST
OF THE SOUTHERN FLORIDA PENINSULA FROM OCEAN REEF ON THE EAST COAST
WESTWARD TO BONITA BEACH ON THE WEST COAST.

A TROPICAL STORM WARNING HAS BEEN ISSUED FOR THE SOUTHEAST FLORIDA
COAST FROM NORTH OF OCEAN REEF NORTHWARD TO JUPITER INLET...AND FOR
LAKE OKEECHOBEE.

THE GOVERNMENT OF THE BAHAMAS HAS ISSUED A TROPICAL STORM WARNING
FOR ALL OF THE NORTHWESTERN BAHAMAS.

A TROPICAL STORM WATCH HAS BEEN ISSUED FOR THE EAST-CENTRAL FLORIDA
COAST FROM NORTH OF JUPITER INLET TO SEBASTIAN INLET.

THE CAYMAN ISLANDS METEOROLOGICAL SERVICE HAS ISSUED A TROPICAL
STORM WATCH FOR THE CAYMAN ISLANDS.

SUMMARY OF WATCHES AND WARNINGS IN EFFECT...

A HURRICANE WATCH IS IN EFFECT FOR...
* HAITI
* FLORIDA KEYS INCLUDING THE DRY TORTUGAS
* FLORIDA BAY
* THE FLORIDA EAST COAST FROM OCEAN REEF SOUTHWARD
* THE FLORIDA WEST COAST FROM BONITA BEACH SOUTHWARD

A TROPICAL STORM WARNING IS IN EFFECT FOR...
* DOMINICAN REPUBLIC
* HAITI
* CUBAN PROVINCES OF CIEGO DE AVILA...SANCTI SPIRITUS...VILLA
CLARA...CAMAGUEY...LAS TUNAS...GRANMA...HOLGUIN...SANTIAGO DE
CUBA...AND GUANTANAMO
* THE BAHAMAS
* TURKS AND CAICOS ISLANDS
* THE FLORIDA KEYS INCLUDING THE DRY TORTUGAS
* THE FLORIDA EAST COAST FROM JUPITER INLET SOUTHWARD
* THE FLORIDA WEST COAST FROM BONITA BEACH SOUTHWARD
* FLORIDA BAY AND LAKE OKEECHOBEE

A TROPICAL STORM WATCH IS IN EFFECT FOR...
* CUBAN PROVINCES OF MATANZAS AND CIENFUEGOS
* JAMAICA
* THE FLORIDA EAST COAST NORTH OF JUPITER INLET TO SEBASTIAN INLET

A HURRICANE WATCH MEANS THAT HURRICANE CONDITIONS ARE POSSIBLE
WITHIN THE WATCH AREA...IN THIS CASE WITHIN THE NEXT 24 TO 36 HOURS.

A TROPICAL STORM WARNING MEANS THAT TROPICAL STORM CONDITIONS ARE
EXPECTED SOMEWHERE WITHIN THE WARNING AREA WITHIN 36 HOURS.

A TROPICAL STORM WATCH MEANS THAT TROPICAL STORM CONDITIONS ARE
POSSIBLE WITHIN THE WATCH AREA...GENERALLY WITHIN 48 HOURS.

INTERESTS IN THE REMAINDER OF CUBA AND THE REMAINDER OF THE FLORIDA
PENINSULA SHOULD MONITOR THE PROGRESS OF ISAAC.

FOR STORM INFORMATION SPECIFIC TO YOUR AREA IN THE UNITED
STATES...INCLUDING POSSIBLE INLAND WATCHES AND WARNINGS...PLEASE
MONITOR PRODUCTS ISSUED BY YOUR LOCAL NATIONAL WEATHER SERVICE
FORECAST OFFICE. FOR STORM INFORMATION SPECIFIC TO YOUR AREA OUTSIDE
THE UNITED STATES...PLEASE MONITOR PRODUCTS ISSUED BY YOUR NATIONAL
METEOROLOGICAL SERVICE.


DISCUSSION AND 48-HOUR OUTLOOK
------------------------------
AT 1100 PM EDT...0300 UTC...THE CENTER OF TROPICAL STORM ISAAC WAS
LOCATED NEAR LATITUDE 17.7 NORTH...LONGITUDE 72.5 WEST. ISAAC IS
MOVING TOWARD THE NORTHWEST NEAR 14 MPH...22 KM/H...BUT IS EXPECTED
TO RESUME A FASTER FORWARD SPEED TOWARD THE NORTHWEST TONIGHT
THROUGH SUNDAY.  ON THE FORECAST TRACK...THE CENTER OF ISAAC SHOULD
MAKE LANDFALL IN HAITI TONIGHT...MOVE NEAR OR OVER SOUTHEASTERN
CUBA ON SATURDAY...MOVE NEAR OR OVER CENTRAL CUBA SATURDAY NIGHT...
AND APPROACH THE FLORIDA KEYS ON SUNDAY.

MAXIMUM SUSTAINED WINDS ARE NEAR 70 MPH...110 KM/H...WITH HIGHER
GUSTS. LITTLE CHANGE IN STRENGTH IS LIKELY BEFORE LANDFALL...
FOLLOWED BY SOME WEAKENING AS THE CENTER CROSSES HAITI AND
SOUTHEASTERN CUBA.

TROPICAL-STORM-FORCE WINDS EXTEND OUTWARD UP TO 230 MILES...
370 KM...MAINLY NORTHWEST AND NORTHEAST OF THE CENTER.

ESTIMATED MINIMUM CENTRAL PRESSURE IS 990 MB...29.23 INCHES.


HAZARDS AFFECTING LAND
----------------------
RAINFALL...TOTAL RAINFALL ACCUMULATIONS OF 8 TO 12 INCHES...WITH
MAXIMUM AMOUNTS OF 20 INCHES...ARE POSSIBLE OVER HISPANIOLA. THESE
RAINS COULD CAUSE LIFE-THREATENING FLASH FLOODS AND MUD SLIDES.
TOTAL RAIN ACCUMULATIONS OF 4 TO 8 INCHES...WITH MAXIMUM AMOUNTS OF
12 INCHES...ARE POSSIBLE ACROSS JAMAICA...THE CENTRAL AND EASTERN
PORTIONS OF CUBA...THE FLORIDA KEYS AND THE SOUTHERN PENINSULA OF
FLORIDA.  TOTAL RAIN ACCUMULATIONS OF 2 TO 4 INCHES ARE POSSIBLE
OVER THE CENTRAL AND SOUTHEASTERN BAHAMAS.

WIND...TROPICAL STORM CONDITIONS ARE SPREADING OVER PORTIONS OF THE
DOMINICAN REPUBLIC AND HAITI...WITH HURRICANE CONDITIONS POSSIBLE IN
HAITI. TROPICAL STORM CONDITIONS ARE EXPECTED OVER THE SOUTHEASTERN
BAHAMAS AND THE TURKS AND CAICOS ISLANDS TONIGHT...ARE EXPECTED
OVER THE CENTRAL BAHAMAS BY SATURDAY OR SATURDAY NIGHT...AND ARE
EXPECTED OVER THE NORTHWESTERN BAHAMAS BY SUNDAY. TROPICAL STORM
CONDITIONS ARE EXPECTED OVER EASTERN CUBA BY TONIGHT AND OVER
CENTRAL CUBA BY SATURDAY OR SATURDAY NIGHT. TROPICAL STORM
CONDITIONS ARE EXPECTED TO REACH NORTHWESTERN CUBA AND THE
NORTHWESTERN BAHAMAS BY SATURDAY NIGHT OR SUNDAY...AND SOUTH
FLORIDA AND THE FLORIDA KEYS ON SUNDAY. HURRICANE CONDITIONS ARE
POSSIBLE OVER THE FLORIDA KEYS...FLORIDA BAY...AND THE SOUTHERNMOST
FLORIDA PENINSULA BY SUNDAY EVENING.

STORM SURGE...THE COMBINATION OF A STORM SURGE AND THE TIDE WILL
CAUSE NORMALLY DRY AREAS NEAR THE COAST TO BE FLOODED BY RISING
WATERS. THE WATER COULD REACH THE FOLLOWING DEPTHS ABOVE GROUND
IF THE PEAK SURGE OCCURS AT THE TIME OF HIGH TIDE...

SOUTH FLORIDA INCLUDING THE FLORIDA KEYS...2 TO 4 FT
HISPANIOLA AND EASTERN CUBA...1 TO 3 FT
THE BAHAMAS AND TURKS AND CAICOS...1 TO 3 FT

THE DEEPEST WATER WILL OCCUR ALONG THE IMMEDIATE COAST IN AREAS OF
ONSHORE FLOW. SURGE-RELATED FLOODING DEPENDS ON THE RELATIVE TIMING
OF THE SURGE AND THE TIDAL CYCLE...AND CAN VARY GREATLY OVER SHORT
DISTANCES. FOR INFORMATION SPECIFIC TO YOUR AREA...PLEASE SEE
PRODUCTS ISSUED BY YOUR LOCAL WEATHER SERVICE OFFICE. NEAR THE
COAST...THE SURGE WILL BE ACCOMPANIED BY DANGEROUS WAVES.

SURF...DANGEROUS SURF AND RIP CURRENT CONDITIONS WILL AFFECT PUERTO
RICO...HISPANIOLA...THE BAHAMAS...THE TURKS AND CAICOS...EASTERN
AND CENTRAL CUBA...AND THE EAST COAST OF FLORIDA AND THE FLORIDA
KEYS DURING THE NEXT COUPLE OF DAYS. PLEASE CONSULT PRODUCTS FROM
YOUR LOCAL WEATHER OFFICE FOR MORE INFORMATION.


NEXT ADVISORY
-------------
NEXT INTERMEDIATE ADVISORY...200 AM EDT.
NEXT COMPLETE ADVISORY...500 AM EDT.

$$
FORECASTER STEWART"""

    ###############################################################
    # Advisory related methods

    def _initializeAdvisories(self):
        self._currentAdvisory = dict()
        self._currentAdvisory["ZoneData"] = dict()
        self._loadLastTwoAdvisories()

    def _synchronizeAdvisories(self):
        # Retrieving a directory causes synching to occur.
        # This code can throw an exception but don't catch it
        # so that forecasters can be made aware of the issue.
        file = LocalizationSupport.getLocalizationFile(
            LocalizationSupport.CAVE_STATIC,
            LocalizationSupport.SITE,
            self._site,
            self._getAdvisoryPath(),
        ).getFile()

        return file

    def _getLocalAdvisoryDirectoryPath(self):
        file = self._synchronizeAdvisories()
        path = file.getPath()

        try:
            os.makedirs(path)
        except OSError as exception:
            if exception.errno != errno.EEXIST:
                raise

        return path

    def _getStormAdvisoryNames(self, wantHLS=False):
        self._synchronizeAdvisories()
        advisoryDirectoryPath = self._getLocalAdvisoryDirectoryPath()
        filenames = os.listdir(advisoryDirectoryPath)
        allAdvisories = [fname for fname in filenames if fname[-5:] == ".json"]

        self.debug_print("allAdvisories = %s" % (self._pp.pformat(allAdvisories)))

        stormAdvisories = [
            fname[:-5]
            for fname in allAdvisories
            if self._getStormNumberStringFromTCP() in fname
        ]

        # If we want the HLS advisories
        if wantHLS:
            productType = "HLS"
            stormAdvisories = [adv for adv in stormAdvisories if "HLS" in adv]

        # Otherwise, ignore the HLS advisories
        else:
            productType = "TCV"
            stormAdvisories = [adv for adv in stormAdvisories if "HLS" not in adv]

        self.debug_print(
            "{} stormAdvisories = {}".format(productType, self._pp.pformat(stormAdvisories)), 1
        )

        return stormAdvisories

    def _loadLastTwoAdvisories(self, wantHLS=False):

        # We need to reverse the order of the advisories so the latest
        # advisories come first in this list
        stormAdvisories = sorted(self._getStormAdvisoryNames(wantHLS), reverse=True)

        lastTwoAdvisories = []
        productType = "TCV"

        # Get the current advisory number string from the TCP
        curAdvisoryString = self._getAdvisoryNumberStringFromTCP()

        if "TCV" in self._awipsWANPil:

            # Get the file names of the last several TCV advisory files
            # (HLS advisory files should already be ignored)
            lastTwoAdvisories = [
                adv for adv in stormAdvisories if not adv.endswith(curAdvisoryString)
            ]

        # Must be the HLS
        else:
            lastTwoAdvisories = stormAdvisories[:2]
            productType = "HLS"

            # Only make this check if loading TCV advisory files
            if len(lastTwoAdvisories) > 0 and not wantHLS:
                self._previousAdvisoryMatchesNumber = lastTwoAdvisories[0].endswith(
                    curAdvisoryString
                )

        self.debug_print(
            "DEBUG: {} last two advisories = {}".format(
                productType, self._pp.pformat(lastTwoAdvisories)
            ),
            1,
        )
        self._previousAdvisory = None
        if len(lastTwoAdvisories) >= 1:
            self._previousAdvisory = self._loadAdvisory(lastTwoAdvisories[0])

        self._previousPreviousAdvisory = None
        if len(lastTwoAdvisories) >= 2:
            self._previousPreviousAdvisory = self._loadAdvisory(lastTwoAdvisories[1])

    def _loadAdvisory(self, advisoryName):
        self._synchronizeAdvisories()
        fileName = self._getAdvisoryFilename(advisoryName)

        try:
            pythonDict = JsonSupport.loadFromJson(
                LocalizationSupport.CAVE_STATIC, self._site, fileName
            )

            self.debug_print("File contents for %s:" % (fileName), 1)
            self.debug_print(self._pp.pformat(pythonDict), 1)

            # Only use transmitted advisories
            if pythonDict["Transmitted"] == False and advisoryName != "pending":
                return None
            else:
                return pythonDict
        except Exception as e:
            self.debug_print("Load Exception for %s : %s" % (fileName, e), 1)
            return None

    def _getAdvisoryPath(self):
        gfeMode = CAVEMode.getMode().name()
        if gfeMode == "PRACTICE":
            return os.path.join("gfe", "tcvAdvisories", "practice")
        else:
            return os.path.join("gfe", "tcvAdvisories")

    def _getAdvisoryFilename(self, advisoryName):
        advisoryFilename = os.path.join(self._getAdvisoryPath(), advisoryName + ".json")
        return advisoryFilename

    ###############################################################
    # GUI related methods

    def _processVariableList(self, definition, parent):
        # Get Definition variables
        for (key, value) in definition.items():
            setattr(self, f"_{key}", value)

        # Overview GUI
        while True:
            overviewDict = self._displayGUI()
            if overviewDict is None:
                return None
            break

        # Consolidate information from GUI's
        varDict = overviewDict
        return varDict

    def _GUI_sizing_dict(self):
        # This contains values that adjust the GUI sizing.
        return {
            "GUI_height_limit": 900,  # limit to GUI height in canvas pixels
            "charSize": 9,
        }

    def _GUI1_configDict(self):
        return {
            # Order and inclusion of GUI1 buttons
            # Each entry is (name of button in GUI code, desired label on GUI)
            "buttonList": [("Run", "Run"), ("Cancel", "Cancel"), ],
        }

    def _font_GUI_dict(self):
        return {
            "headers": ("blue", ("Helvetica", 14, "bold")),
            "instructions": (None, ("Helvetica", 12, "italic")),
        }


import tkinter
class Common_Dialog(Dialog):

    def __init__(self, parent, title, infoDict=None):
        self._status = "Cancel"    # exception, or user-cancels
        self._tkObject_dict = {}   # place to store reference to tk objects
        self._varDict = {}         # all end results must be saved here
        self._infoDict = infoDict
        self._parent = parent
        self._pp = pprint.PrettyPrinter()
        Dialog.__init__(self, parent=None, title=title)

    def getVarDict(self):
        return self._varDict

    def _makeRadioOrCheckList(
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
        boxType="radio",
        listFrameRelief=tkinter.GROOVE,
    ):
        listFrame = tkinter.Frame(master, relief=listFrameRelief, borderwidth=1)

        if label:
            listLabel = tkinter.Label(listFrame, text=label, fg=headerFG, font=headerFont)
            listLabel.pack(side=tkinter.TOP, fill=tkinter.X, expand=tkinter.NO, padx=10)

        ivar = tkinter.IntVar()
        defaultIndex = 0
        ivarList = []
        for element in elementList:
            index = elementList.index(element)
            if isinstance(element, tuple):
                element, key = element
            if boxType == "radio":
                button = tkinter.Radiobutton(
                    listFrame, variable=ivar, text=element, value=index
                )
            else:
                ivar = tkinter.IntVar()
                if default is not None and element in default:
                    ivar.set(1)
                else:
                    ivar.set(0)
                button = tkinter.Checkbutton(listFrame, variable=ivar, text=element)
                ivarList.append(ivar)
            button.pack(side=buttonSide, anchor=tkinter.W, expand=tkinter.YES, padx=4)
            # Look for default
            if element == default:
                defaultIndex = index

        entryObject = None
        if entryField is not None:
            entryObject = self._makeEntry(listFrame, entryField)
        # packing
        listFrame.pack(
            side=frameSide, expand=tkinter.NO, fill=tkinter.Y
        )  # , anchor=tkinter.N)

        if boxType == "radio":
            ivar.set(defaultIndex)  # set the default
        if boxType == "check":
            ivar = ivarList
        return ivar, entryObject

    def _makeEntry(self, frame, text, width=20):
        label = tkinter.Label(frame, text=text)
        label.pack(side=tkinter.LEFT, fill=tkinter.X, expand=tkinter.NO)
        entry = tkinter.Entry(frame, relief=tkinter.SUNKEN, width=width)
        entry.pack(side=tkinter.LEFT, fill=tkinter.X, expand=tkinter.NO)
        return entry

    def cancelCB(self):
        self._status = "Cancel"
        self.cancel()

    def _entryName(self, name):
        return name + "_entry"

    def _makeTuple(self, str):
        str = re.sub("(?im)[^_a-z]", "", str)
        return (str + ":", str)

    def _setVarDict(self, key, value, options=None):
        if options is not None:
            value = options[value]
            if isinstance(value, tuple):
                value = value[1]
        self._varDict[self._makeTuple(key)] = value

    def status(self):
        return self._status

    def buttonbox(self):
        # override the existing ok/cancel button box, removing it.
        # we do this so that we can attach our own hooks into the functions.
        pass


#########################################################
# The following defintions are from TextProductCommon.  #
# This is just bringing over the minimum amount needed. #
#########################################################


class TextProductCommon(DiscretePhrases.DiscretePhrases):

    def __init__(self):
        DiscretePhrases.DiscretePhrases.__init__(self)

    def setUp(self, areaDict):
        self._areaDictionary = areaDict

    def hazardTimeZones(self, areaList):
        """
        Returns list of time zones for the starting time
        and list of time zones for the ending time.

        The areaList provides a complete list of areas for this headline.
        startT, endT are the hazard times.
        """

        # get this time zone
        thisTimeZone = os.environ.get("TZ")
        if thisTimeZone is None:
            thisTimeZone = "GMT"

        zoneList = []
        areaDict = self._areaDictionary

        # check to see if we have any areas outside our time zone
        for areaName in areaList:
            if areaName in areaDict:
                entry = areaDict[areaName]
                if "ugcTimeZone" not in entry:  # add your site id
                    if thisTimeZone not in zoneList:
                        zoneList.append(thisTimeZone)
                    continue  # skip it
                timeZoneList = entry["ugcTimeZone"]
                if not isinstance(timeZoneList, list):  # a single value
                    timeZoneList = [str(timeZoneList)]  # make it into a list
                for timeZone in timeZoneList:
                    if timeZone not in zoneList:
                        zoneList.append(timeZone)

        # if the resulting zoneList is empty, put in our time zone
        if not zoneList:
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

    def getExpireTime(
        self, issueTime, purgeHours, vtecRecords, roundMinutes=15, fixedExpire=0
    ):
        """
        Given the issuance time, purgeHours, and the vtecRecords (with times converted to ms),
        returns the appropriate expiration time.

        Expiration time is the earliest of the specified expiration time, 1 hr if a CAN code
        is detected, or the ending time of ongoing events (CON, EXT, EXB, NEW).
        The issueTime and expireTime are ints in milliseconds.

        @param issueTime in ms
        @param purgeHours -- set time past issuance time.
               The default for this is set by policy e.g. an FFA expires by default
               in 8 hours. However, if there is a hazard end time earlier, then that
               is used.
               if -1, then hazard end time is to be used
        @param vtecRecords in the segment with times converted to ms
        @param roundMinutes
        @param fixedExpire -- indicates to ignore the VTEC actions when computing the
               expiration time

        """
        if purgeHours > 0:
            expireTime = issueTime + purgeHours * 3600 * 1000
        else:
            expireTime = None
            # Pick the earliest end time of the vtecRecords in the segment
            for vtecRecord in vtecRecords:
                if expireTime is None or vtecRecord.get("endTime") < expireTime:
                    expireTime = vtecRecord.get("endTime")

        if not fixedExpire:
            canExpFound = 0
            activeFound = 0
            laterActive = None  # later end time of all active events
            for vtecRecord in vtecRecords:
                action = vtecRecord.get("act")
                if action in ["CAN", "EXP"]:
                    canExpFound = 1
                elif action in ["NEW", "CON", "EXT", "EXB", "EXA"]:
                    activeFound = 1
                    endTime = vtecRecord.get("endTime")
                    if endTime != 0:
                        if laterActive is not None:
                            laterActive = max(laterActive, endTime)
                        else:
                            laterActive = endTime
            if laterActive is not None:
                expireTime = min(expireTime, laterActive)
            elif canExpFound and not activeFound:
                expireTime = min(expireTime, issueTime + 3600 * 1000)  # 1hr from now

        # ensure expireTime is not before issueTime, and is at least 1 hour
        if expireTime - issueTime < 3600 * 1000:
            expireTime = issueTime + 3600 * 1000

        # Round expiration time to next "roundMinutes" (default is 15 minutes)
        roundValue = roundMinutes * 60 * 1000  # in milliseconds
        delta = expireTime % roundValue  # in milliseconds
        baseTime = int(expireTime / roundValue) * roundValue

        # If we are at least 1 minute past this rounded expiration time
        if int(delta / 60 / 1000) >= 1:

            # add the next increment to the product expiration time
            expireTime = baseTime + roundValue
        else:  # within 1 minute, don't add the next increment
            expireTime = baseTime

        return expireTime

    def getHeadlinesAndSections(self, vtecRecords, productID, issueTime):
        """
        Order vtec records and create the sections for the segment

        @param vtecRecords:  vtecRecords for a segment
        @param metaDataList: list of (metaData, hazardEvent) for the segment
        @param productID: product ID e.g. FFA, CWF, etc.
        @param issueTime: in seconds so that it compares to the vtec records
        """

        headlines = []
        headlineStr = ""
        hList = copy.deepcopy(vtecRecords)
        if len(hList):
            if productID in ["CWF", "NSH", "OFF", "GLF"]:
                hList.sort(key=self.marineSortHazardAlg)
            else:
                hList.sort(key=self.regularSortHazardAlg)

        while len(hList) > 0:
            vtecRecord = hList[0]

            # Can't make phrases with vtecRecords with no 'hdln' entry
            if not vtecRecord["hdln"]:
                hList.remove(vtecRecord)
                continue

            # make sure the vtecRecord is still in effect or within EXP critiera
            if (vtecRecord["act"] != "EXP" and issueTime >= vtecRecord["endTime"]) or (
                vtecRecord["act"] == "EXP" and issueTime > 30 * 60 + vtecRecord["endTime"]
            ):
                hList.remove(vtecRecord)
                continue  # no headline for expired vtecRecords

            # assemble the vtecRecord type
            hazStr = vtecRecord["hdln"]
            headlines.append(hazStr)
            # hazStr = self.convertToLower(hazStr)

            # if the vtecRecord is a convective watch, tack on the etn
            phenSig = vtecRecord["phen"] + "." + vtecRecord["sig"]
            if phenSig in ["TO.A", "SV.A"]:
                hazStr += " " + str(vtecRecord["etn"])

            # add on the action
            actionWords = self.actionControlWord(vtecRecord, issueTime)
            hazStr += " " + actionWords

            if len(hazStr):
                # Call user hook
                localStr = self.hazard_hook(
                    None,
                    None,
                    vtecRecord["phen"],
                    vtecRecord["sig"],
                    vtecRecord["act"],
                    vtecRecord["startTime"],
                    vtecRecord["endTime"],
                )  # May need to add leading space if non-null
                headlineStr += "..." + hazStr + localStr + "...\n"

            # always remove the main vtecRecord from the list
            hList.remove(vtecRecord)

        return headlineStr, headlines

    def formatUGCs(self, ugcs, expireTime):
        """
        Create ugc header with expire time
        Examples:
        "COC123-112330-"
        "FLZ066>068-071-072-063-069-073>075-168-172>174-070-230515-"
        """
        ugcStr = self.makeUGCString(ugcs)
        ddhhmmTime = self.getFormattedTime(
              expireTime // 1000, "%d%H%M", shiftToLocal=0, stripLeading=0).upper()
        ugcStr += "-" + ddhhmmTime + "-"
        return ugcStr

    def getFormattedTime(
        self,
        time_secs,
        format="%I%M %p %Z %a %b %d %Y",
        shiftToLocal=1,
        upperCase=0,
        stripLeading=1,
    ):
        """
         Return a text string of the given time in seconds in the given format
         This method is used for product headers.
        """
        if time_secs == 0:
            time_secs = time.time()
        if shiftToLocal:
            curTime = time.localtime(time_secs)
        else:
            curTime = time.gmtime(time_secs)
            localTime = time.localtime(time_secs)
            zoneName = time.strftime("%Z", localTime)
        timeStr = time.strftime(format, curTime)
        if not shiftToLocal:
            timeStr = timeStr.replace(zoneName, "GMT")
        if stripLeading and timeStr[0] in ["0", " "]:
            timeStr = timeStr[1:]
        if upperCase:
            timeStr = timeStr.upper()
        timeStr = timeStr.replace("  ", " ")
        return timeStr

    def formatUGC_names(self, ugcs, alphabetize=False, separator="-"):
        """
        For example: Saunders-Douglas-Sarpy-Lancaster-Cass-Otoe-
        """
        nameList = []
        for ugc in ugcs:
            entry = self._areaDictionary.get(ugc)
            nameList.append(entry.get("ugcName", ugc))
        if alphabetize:
            nameList.sort()
        return self.formatNameString(nameList, separator)

    def formatNameString(self, nameList, separator, state=None):
        nameString = ""
        for name in nameList:
            nameString += name + separator
        if state:
            nameString = nameString.rstrip(separator) + " (" + state + ") "
        return nameString

    def getVal(self, dictionary, key, default=None, altDict=None):
        """
        Convenience method to access dictionary keys and account for :skip and :editable suffixes

        @param dictionary
        @param key, potentially without a suffix e.g. "info"
        @return the key value accounting for suffixes e.g. "info:skip"
        """
        for dictKey in [key, key + ":skip", key + ":editable"]:
            if dictionary.get(dictKey):
                return dictionary.get(dictKey)
            if altDict and altDict.get(dictKey):
                return altDict.get(dictKey)
        return default

    def formatDatetime(self, dt, format="ISO", timeZone=None):
        """
        @param dt: datetime object
        @param format: format string e.g. "%H%M %p %Z %a %e %b %Y"
        @param zone: time zone e.g."CST7CDT".   If None use UTC
        @return datetime formatted with time zone e.g. "1400 PM CST Mon 12 Feb 2011"
        """
        import datetime
        from dateutil import tz

        # TODO REMOVE THIS BLOCK AS PART OF THE JSON REFACTOR.
        if isinstance(dt, float):
            dt = datetime.fromtimestamp(dt / 1000)

        from_zone = tz.tzutc()
        new_time = dt.replace(tzinfo=from_zone)
        if timeZone is not None:
            to_zone = tz.gettz(timeZone)
            new_time = new_time.astimezone(to_zone)
        if format == "ISO":
            return new_time.isoformat()
        else:
            return new_time.strftime(format)

    def flush(self):
        """ Flush the print buffer """
        os.sys.__stdout__.flush()

    def makeUGCString(self, ugcs):
        """
        Create the UGC string for product / segment headers.

        Examples:
        FLZ173-
        FLZ066>068-071-072-063-069-073>075-168-172>174-070-
        """
        # if nothing in the list, return empty string
        if not ugcs:
            return ""

        # Remove any blank UGC lines and duplicates from the list
        ugcList = [ugc.strip() for ugc in sorted(set(ugcs)) if ugc.strip()]

        # Set up state variables and process initialize ugcStr with first ugc
        # in ugcList
        inSeq = 0
        ugcStr = ugcList[0]
        curState = ugcStr[0:3]
        lastNum = int(ugcList[0][3:])

        for ugc in ugcList[1:]:
            ugcState = ugc[:3]
            ugcNumStr = ugc[3:]
            num = int(ugcNumStr)
            if ugcState == curState:
                if num == lastNum + 1:
                    if inSeq > 0:
                        # Replace the last ugcNumStr in sequence with the
                        # current ugcNumStr
                        # e.g.   062>063  becomes 062>064
                        ugcStr = ugcStr[: len(ugcStr) - 3] + ugcNumStr
                        inSeq += 1
                    else:
                        ugcStr += ">" + ugcNumStr
                        inSeq = 1
                else:  # num != lastNum + 1
                    ugcStr = self.checkLastArrow(inSeq, ugcStr)
                    inSeq = 0  # reset sequence when number not in sequence
                    ugcStr += "-" + ugcNumStr
            else:
                ugcStr = self.checkLastArrow(inSeq, ugcStr)
                ugcStr += "-" + ugc
                curState = ugcState
                inSeq = 0  # reset sequence when switching states
            lastNum = num

        # May have to clean up last arrow at the end
        ugcStr = self.checkLastArrow(inSeq, ugcStr)
        return ugcStr

    def checkLastArrow(self, inSeq, ugcStr):
        """
        Part of formatUGCs
        """
        if inSeq == 1:
            # Change the last arrow to - since
            # we only had 2 in the sequence e.g.
            # 062>063  should be   062-063
            arrowIndex = ugcStr.rfind(">")
            if arrowIndex >= 0:
                ugcStr = ugcStr[:arrowIndex] + "-" + ugcStr[arrowIndex + 1:]
        return ugcStr


class ETLogger():

    def __init__(self, method):
        self.__method = method
        now = datetime.datetime.now()
        self.__initTime = now
        print ("%s  In %s. Elapsed time=%s" % \
               (now, method, datetime.timedelta(0)))
        self.__lastLogTime = now

    def logElapsedTime(self, signature):
        now = datetime.datetime.now()
        print ("%s  In %s/%s. Elapsed time=%s  Since last log=%s" % \
               (now, self.__method, signature, now-self.__initTime, now-self.__lastLogTime))
        self.__lastLogTime = now

