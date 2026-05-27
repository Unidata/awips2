# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# WindWWUtils
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# May  9, 2019 21020      tlefebvr    Original version
# May 16, 2019 21020      tlefebvr    Code review changes
# Aug  3, 2019 21020      tlefebvr    Added breakpointZoneList
# Aug 16, 2019 21020      tlefebvr    Code clean up.
# Aug 22, 2019 21020      tlefebvr    Code Review changes
# Apr  3, 2020 21020      tlefebvr/psantos  Spring 2020 Sprint
# Apr 29, 2020 22033      tlefebvr    Added HFO breakpoints to bpZoneDict
# May  6, 2020 22033      tlefebvr    Code clean-up. Commented out bad BP
# May 11, 2020 22033      tlefebvr    Added new methods to track stormIDs
#                                     to prevent duplicates.
# May 12, 2020 22033      tlefebvr    Code comments.
# May 13, 2020 22033      tlefebvr    Fixed bpDict -> bpZoneDict issue.
# May 14, 2020 22033      tlefebvr    Added ***Sites methods for all to user.
# May 21, 2020 22033      tlefebvr    Added more support for basins and bins
#                                     Addressed code review comments.
# May 26, 2020 22033      tlefebvr    StormID history includes gfe operating mode.
# May 29, 2020 22033      tlefebvr    Added makeStormID for refactoring.
# Jun  3, 2020 22033      tlefebvr    Addressed code review comments.
# Oct 21, 2020 22033      tlefebvr    Refactoring: added a few methods common to tools.
# Oct 22, 2020 22033      tlefebvr    Fixed so that TR.W^HU.A and HU.A^TRW are treated
#                                     the same.
# Oct 21, 2020 22033      tlefebvr    Moved getAllTropicalHazards as it is now common code.
# Feb  3, 2021            mbrennan    Updated for breakpoint and zone changes for 2021.
# Feb 11, 2020 22033      tlefebvr    Added new method getNationalCenterIDs.
# Feb 16, 2020 22033      tlefebvr    Changed maxStorms to 99.
# May 10, 2021 22033      tlefebvr    Added new method to fetch data files path.
# May 13, 2021 22033      tlefebvr    Changed hazList in fetchStormInfo to be optional.
# May 13, 2021 22033      tlefebvr    Added common method to saveAdvisories and implemneted
#                                     conversion from HU to TY and back based on siteID.
# Jul 29, 2021 22531      tlefebvr    Final code clean-up before check-in.
# Aug  5, 2021 22531      tlefebvr    Added new method breakpointInASequence to support
#                                     SelectBreakPoints feature.
# Aug 10, 2021 22531      pSantos     Updated a few breakpoint zones.
# Aug 24, 2021 22531      tlefebvr    Updated decodeUnicode for Py2 and Py3 compatibility.
# Sep 13, 2021  8657      randerso    Moved a number of tropical configuration
#                                     items up to TropicalUtility
# Jan  6, 2022 22531      tlefebvr    Moved getTextProdFromDB to here from SmartScript.
# Jan 19, 2022 22531      tlefebvr    Fixed Names descriptions for HI zone names in dict to match BP tables.
# Jan 21, 2022 22531      tlefebvr    Fixed Names descriptions for CA zone names in dict to match BP tables.
# Feb  1, 2022 22531      tlefebvr    p3 fix
# Mar  2, 2022 22531      tlefebvr    Added support to display JSON Files content.
# Mar  7, 2022 22531      tlefebvr    Fixed issue with inlandZones in displayJSONHazards.
# Sep 15, 2022 22531      santos/scamp/white Fixed post 21.4.1-13 testing.
# Feb 28, 2023 2036298    scamposano  Updating breakpoints for 2023
################################################################################
from __future__ import print_function
import SmartScript
from collections import OrderedDict
import TropicalUtility
import operator
import numpy as np
import copy
import AbsTime, TimeRange
import os
from functools import reduce

# This version derived from Breakpoints spreadsheet
bpZoneDict = OrderedDict([
    ("Mouth of the Rio Grande River - Arroyo City", ['TXZ355', 'TXZ455']),
    ("Arroyo City - North of Port Mansfield", ['TXZ354', 'TXZ454']),
    ("North of Port Mansfield - Baffin Bay", ['TXZ351', 'TXZ451']),
    ("Baffin Bay - N Entrance Padre Island NS", ['TXZ342', 'TXZ442']),
    ("N Entrance Padre Island NS - Port Aransas", ['TXZ343', 'TXZ344', 'TXZ443']),
    ("Port Aransas - Mesquite Bay", ['TXZ245', 'TXZ345', 'TXZ346']),
    ("Mesquite Bay - Port OConnor", ['TXZ347', 'TXZ447']),
    ("Port OConnor - Matagorda", ['TXZ335', 'TXZ336', 'TXZ436']),
    ("Matagorda - Sargent", ['TXZ336', 'TXZ436']),
    ("Sargent - Freeport", ['TXZ337', 'TXZ437']),
    ("Freeport - San Luis Pass", ['TXZ337', 'TXZ437']),
    ("San Luis Pass - Port Bolivar", ['TXZ313', 'TXZ338', 'TXZ438']),
    ("Port Bolivar - High Island", ['TXZ214', 'TXZ300', 'TXZ439']),
    ("High Island - Sabine Pass", ['TXZ615']),
    ("Sabine Pass - Cameron", ['LAZ073']),
    ("Cameron - Vermillion Cameron Parish Line", ['LAZ074']),
    ("Vermillion Cameron Parish Line - Intracoastal City", ['LAZ252']),
    ("Intracoastal City - Cypremort Point", ['LAZ252', 'LAZ253']),
    ("Cypremort Point - Morgan City", ['LAZ254']),
    ("Lake Maurepas", ['LAZ057', 'LAZ084', 'LAZ086']),
    ("Lake Pontchartrain", ['LAZ058', 'LAZ060', 'LAZ076', 'LAZ077', 'LAZ078', 'LAZ080', 'LAZ082', 'LAZ087']),
    ("Morgan City - Grand Isle", ['LAZ066', 'LAZ067', 'LAZ068']),
    ("Grand Isle - Mouth Mississippi River", ['LAZ068', 'LAZ069']),
    ("Mouth Mississippi River - Mouth Pearl River", ['LAZ069', 'LAZ070']),
    ("Mouth Pearl River - Bay St Louis", ['MSZ086']),
    ("Bay St Louis - Ocean Springs", ['MSZ087']),
    ("Ocean Springs - MS/AL border", ['MSZ088']),
    ("MS/AL border - AL/FL border", ['ALZ263', 'ALZ264', 'ALZ265', 'ALZ266']),
    ("AL/FL border - Santa Rosa Okaloosa County Line", ['FLZ202', 'FLZ204']),
    ("Santa Rosa Okaloosa County Line - Okaloosa Walton County Line", ['FLZ206']),
    ("Okaloosa Walton County Line - Walton Bay County Line", ['FLZ108']),
    ("Walton Bay County Line - Mexico Beach", ['FLZ112']),
    ("Mexico Beach - Indian Pass", ['FLZ114']),
    ("Indian Pass - Ochlockonee River", ['FLZ115']),
    ("Ochlockonee River - Wakulla Jefferson County Line", ['FLZ127']),
    ("Wakulla Jefferson County Line - Aucilla River", ['FLZ118']),
    ("Aucilla River - Steinhatchee River", ['FLZ128']),
    ("Steinhatchee River - Suwannee River", ['FLZ134']),
    ("Suwannee River - Yankeetown", ['FLZ139']),
    ("Yankeetown - Chassahowitzka", ['FLZ142']),
    ("Chassahowitzka - Aripeka", ['FLZ148']),
    ("Aripeka - Anclote River", ['FLZ149']),
    ("Anclote River - Egmont Key", ['FLZ050', 'FLZ151']),
    ("Egmont Key - Anna Maria Island", ['FLZ155']),
    ("Anna Maria Island - Middle of Longboat Key", ['FLZ155']),
    ("Middle of Longboat Key - Englewood", ['FLZ160']),
    ("Englewood - Boca Grande", ['FLZ162']),
    ("Boca Grande - Bonita Beach", ['FLZ165']),
    ("Dry Tortugas Island", []),
    ("Key West - Seven Mile Bridge", ['FLZ078']),
    ("Seven Mile Bridge - Channel 5 Bridge", ['FLZ077']),
    ("Channel 5 Bridge - Ocean Reef", ['FLZ076']),
    ("St Thomas and St John", ['VIZ001']),
    ("St Croix", ['VIZ002']),
    ("Puerto Rico",
     ['PRZ001', 'PRZ002', 'PRZ003', 'PRZ004', 'PRZ005', 'PRZ006', 'PRZ007', 'PRZ008', 'PRZ009', 'PRZ010', 'PRZ011']),
    ("Culebra", ['PRZ012']),
    ("Vieques", ['PRZ013']),
    ("Bonita Beach - Chokoloskee", ['FLZ069']),
    ("Chokoloskee - East Cape Sable", ['FLZ075']),
    ("East Cape Sable - Flamingo", ['FLZ075']),
    ("Flamingo - Card Sound Bridge", ['FLZ174']),
    ("Card Sound Bridge - Ocean Reef Coastal", ['FLZ173']),
    ("Ocean Reef Coastal - MiamiDade Broward County Line", ['FLZ173']),
    ("Ocean Reef - MiamiDade Broward County Line", ['FLZ173']),
    ("MiamiDade Broward County Line - Broward Palm Bch Cnty Line", ['FLZ172']),
    ("Broward Palm Bch Cnty Line - Palm Bch Martin Cnty Line", ['FLZ168']),
    ("Palm Bch Martin Cnty Line - Stuart", ["FLZ164"]),
    ("Stuart - Fort Pierce", ['FLZ159']),
    ("Fort Pierce - Vero Beach", ['FLZ154', 'FLZ159']),
    ("Vero Beach - Sebastian Inlet", ['FLZ154']),
    ("Sebastian Inlet - Port Canaveral", ['FLZ747']),
    ("Port Canaveral - Volusia Brevard County Line", ['FLZ447']),
    ("Volusia Brevard County Line - New Smyrna Beach", ['FLZ141']),
    ("New Smyrna Beach - Flagler Volusia County Line", ['FLZ141']),
    ("Flagler Volusia County Line - Marineland", ['FLZ138']),
    ("Marineland - Crescent Beach", ['FLZ133']),
    ("Crescent Beach - St Augustine", ['FLZ133']),
    ("St Augustine - Ponte Vedra Beach", ['FLZ133']),
    ("Ponte Vedra Beach - Nassau Sound", ['FLZ125']),
    ("Nassau Sound - Mouth of St Marys River", ['FLZ124']),
    ("Mouth of St Marys River - St Andrews Sound", ['GAZ166']),
    ("St Andrews Sound - Altamaha Sound", ['GAZ154']),
    ("Altamaha Sound - Savannah River",
     ['GAZ116', 'GAZ117', 'GAZ118', 'GAZ119', 'GAZ138', 'GAZ139', 'GAZ140', 'GAZ141']),
    ("Savannah River - Edisto Beach", ['SCZ047', 'SCZ048', 'SCZ049', 'SCZ051']),
    ("Edisto Beach - South Santee River", ['SCZ045', 'SCZ050', 'SCZ052']),
    ("South Santee River - Murrells Inlet", ['SCZ056']),
    ("Murrells Inlet - Little River Inlet", ['SCZ054']),
    ("Little River Inlet - Cape Fear", ['NCZ110']),
    ("Cape Fear - Surf City NC", ['NCZ106', 'NCZ108']),
    ("Surf City NC - Bogue Inlet", ['NCZ199']),
    ("Bogue Inlet - Beaufort Inlet", ['NCZ195']),
    ("Beaufort Inlet - Cape Lookout", ['NCZ196']),
    ("Cape Lookout - Ocracoke Inlet", ['NCZ196']),
    ("Ocracoke Inlet - Hatteras Inlet", ['NCZ204']),
    ("Hatteras Inlet - Cape Hatteras", ['NCZ205']),
    ("Cape Hatteras - Oregon Inlet", ['NCZ205']),
    ("Oregon Inlet - Duck", ['NCZ203']),
    ("Pamlico Sound", []),
    ("Albemarle Sound", []),
    ("Duck - NC/VA border", ['NCZ102']),
    ("NC/VA border - Cape Charles Light", ['VAZ098']),
    ("Cape Charles Light - Parramore Island", ['VAZ100']),
    ("Parramore Island - Chincoteague", ['VAZ099']),
    ("Chincoteague - Fenwick Island", ['MDZ024', 'MDZ025']),
    ("Chesapeake Bay New Point Comfort", ['VAZ095', 'VAZ523', 'VAZ525']),
    ("Chesapeake Bay Windmill Point", ['VAZ084', 'VAZ085', 'VAZ086']),
    ("Chesapeake Bay Smith Point", ['VAZ077', 'VAZ078']),
    ("Chesapeake Bay Drum Point", ['MDZ023']),
    ("Tidal Potomac Cobb Island", ['VAZ075', 'VAZ077', 'MDZ017']),
    ("Tidal Potomac Indian Head", ['MDZ016', 'VAZ055', 'VAZ057', 'VAZ527']),
    ("Chesapeake Bay North Beach", ['MDZ018', 'MDZ021']),
    ("Chesapeake Bay Sandy Point", ['MDZ014', 'MDZ015', 'MDZ019']),
    ("Chesapeake Bay Pooles Island", ['MDZ011', 'MDZ012']),
    ("Chesa Bay North of Pooles Islnd", ['MDZ508', 'MDZ008']),
    ("Tidal Potomac Key Bridge", ['DCZ001', 'MDZ013', 'VAZ053', 'VAZ054']),
    ("Fenwick Island - Cape Henlopen", ['DEZ003', 'DEZ004']),
    ("Cape Henlopen - Cape May", ['DEZ003', 'DEZ004', 'NJZ023', 'NJZ024']),
    ("Cape May - Great Egg Inlet", ['NJZ023', 'NJZ024']),
    ("Great Egg Inlet - Little Egg Inlet", ['NJZ022', 'NJZ025']),
    ("Little Egg Inlet - Manasquan Inlet", ['NJZ020', 'NJZ026', 'NJZ027']),
    ("Manasquan Inlet - Sandy Hook", ['NJZ013', 'NJZ014']),
    ("Sandy Hook - East Rockaway Inlet",
     ['NJZ012', 'NJZ013', 'NJZ006', 'NJZ106', 'NJZ108', 'NYZ072', 'NYZ073', 'NYZ074', 'NYZ075', 'NYZ176', 'NYZ178']),
    ("Delaware Bay South", ['DEZ003', 'NJZ023']),
    ("Delaware Bay North", ['DEZ002', 'NJZ021']),
    ("East Rockaway Inlet - Fire Island Inlet", ['NYZ179']),
    ("Fire Island Inlet - Moriches Inlet", ['NYZ080']),
    ("Moriches Inlet - Montauk Point", ['NYZ081']),
    ("Montauk Point - Port Jefferson Harbor", ['NYZ078', 'NYZ079']),
    ("Port Jefferson Harbor - Kings Point", ['NYZ177']),
    ("Kings Point - Greenwich", ['NYZ071']),
    ("Greenwich - New Haven", ['CTZ009', 'CTZ010']),
    ("New Haven - Watch Hill", ['CTZ010', 'CTZ011', 'CTZ012']),
    ("Block Island", ['RIZ008']),
    ("Watch Hill - Point Judith", ['RIZ006']),
    ("Point Judith - Westport", ['RIZ006', 'RIZ007']),
    ("Nantucket", ['MAZ024']),
    ("Marthas Vineyard", ['MAZ023']),
    ("Westport - Woods Hole", ['MAZ020', 'MAZ021']),
    ("Woods Hole - Chatham", ['MAZ022']),
    ("Chatham - Provincetown", ['MAZ022']),
    ("Provincetown - Sagamore Beach", ['MAZ022']),
    ("Sagamore Beach - Hull", ['MAZ019']),
    ("Hull - Gloucester", ['MAZ007', 'MAZ015', 'MAZ016']),
    ("Gloucester - Merrimack River", ['MAZ007']),
    ("Merrimack River - Portsmouth", ['NHZ014']),
    ("Portsmouth - Cape Elizabeth", ['MEZ023', 'MEZ024']),
    ("Cape Elizabeth - Waldoboro", ['MEZ024', 'MEZ025', 'MEZ026']),
    ("Waldoboro - Stonington ME", ['MEZ027', 'MEZ028']),
    ("Stonington ME - Petit Manan Point", ['MEZ029']),
    ("Petit Manan Point - Eastport", ['MEZ030']),
    ("Eastport - US Canadian Border", ['MEZ030']),
    ("Point Piedras Blancas - Point Sal", ['CAZ340']),
    ("Point Sal - Point Arguello", ['CAZ346']),
    ("Point Arguello - Point Mugu", ['CAZ349', 'CAZ350', 'CAZ354', 'CAZ549', 'CAZ550']),
    ("Point Mugu - Orange Los Angeles Co Line", ['CAZ362', 'CAZ366']),
    ("Catalina Island", ['CAZ087']),
    ("Orange Los Angeles Co Line - San Diego Orange Co Line", ['CAZ552']),
    ("San Diego Orange Co Line - US Mexico Border", ['CAZ043']),

    # HFO Breakpoints / Zones
    ("Hawaii", ['HIZ023', 'HIZ026', 'HIZ027', 'HIZ028', 'HIZ051', 'HIZ052', 'HIZ053', 'HIZ054']),
    ("Maui Molokai Lanai Kahoolawe",
     ['HIZ015', 'HIZ016', 'HIZ017', 'HIZ018', 'HIZ022', 'HIZ037', 'HIZ038', 'HIZ039', 'HIZ040', 'HIZ041', 'HIZ042',
      'HIZ043', 'HIZ044', 'HIZ045', 'HIZ046', 'HIZ047', 'HIZ048', 'HIZ049', 'HIZ050']),
    ("Oahu", ['HIZ006', 'HIZ007', 'HIZ009', 'HIZ010', 'HIZ011', 'HIZ032', 'HIZ033', 'HIZ034', 'HIZ035', 'HIZ036']),
    ("Kauai Niihau", ['HIZ001', 'HIZ003', 'HIZ004', 'HIZ029', 'HIZ030', 'HIZ031']),
    ("Johnston Atol", []),
    ("Midway Island", []),
    ("Kure Atoll", []),
    ("Palmyra Atoll", []),
    ("Teraina Atoll (Washington)", []),
    ("Tabuaeran Atoll (Fanning)", []),
    ("Kiritimati Island (Christmas)", []),
    ("Nihoa to French Frigate Shoals", []),
    ("French Frigate Shoals to Maro Reef", []),
    ("Maro Reef to Lisianski Island", []),
    ("Lisianski Island to Pearl Hermes", []),
])


class WindWWUtils(TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        mode = self.gfeOperatingMode()
        self._historyObjName = "StormIDHistory" + "_" + mode
        self._historyCategory = "WindWWUtils"
        self._rankedWindKeys = ["<None>", "TR.A", "HU.A", "TR.W", "HU.A^TR.W", "TR.W^HU.A", "HU.W", ]
        siteID = self.getSiteID()
        self._NHCZonesEAName = "WindWWNHCZones" + siteID

    def fetchNHCZonesEditArea(self):
        """
        Creates the NHC zones edit area.
        """
        nhcEA = self.getEditArea(self._NHCZonesEAName)
        nhcZoneMask = self.newGrid(False, np.bool)
        if nhcEA is None:
            self.statusBarMsg("Making NHCZone mask. This may take a few minutes. Please stand by.", "R")
            nhcZoneList = self.breakpointZoneList()
            # Make the mask
            for zone in nhcZoneList:
                try:
                    zoneEA = self.getEditArea(zone)
                except:
                    self.statusBarMsg(zone, "edit area not found!")
                    zoneEA = None
                if zoneEA is None:
                    continue
                zoneMask = self.encodeEditArea(zoneEA)
                nhcZoneMask |= zoneMask
            # Convert to edit area and save
            nhcEA = self.decodeEditArea(nhcZoneMask)
            self.saveEditArea(self._NHCZonesEAName, nhcEA)

        return nhcEA

    def getDataFilePath(self):
        """
        Returns the file path of the data tables.
        """
        return os.path.join(os.sep, "awips2", "edex", "data", "share", "RecommendWindWW")

    def getArchiveFilePath(self, practiceMode, stormName):
        """
        Returns the file path where an archive file can be stored
        """

        return os.path.join(self.getDataFilePath(), "archives", "JSON",
                            "Practice" if practiceMode else "", stormName)

    def getStormInfoDicts(self):
        """
        Retrieves the storm info from the JSON file. Additionally, converts all
        the data from unicode to ordinary strings so it can be used an ordinary
        dictionary.
        """
        return self.extractStormInfo(filterATOnly=False)

    def breakpointZoneList(self):
        """
        Return a list of all the zones included in the breakpoint dictionary (bpDict)
        """
        bpValues = bpZoneDict.values()
        # Flatten to a simple list
        bpList = reduce(operator.add, bpValues)
        # Reduce to unique values
        bpList = list(set(bpList))

        # Make sure each ea is valid
        allEAs = self.editAreaList()
        bpList = [ea for ea in bpList if ea in allEAs]

        return bpList

    def createBreakpointsDict(self, filePaths):
        """
        Reads the files specified in filePaths and a dictionary for easy lookup.
        Returns a dictionary of the form (y, x) : (breakpointName, (lat, lon))
        where (y, x) is the corresponding GFE grid point.
        """
        bpDict = {}
        bpLocations = []
        currentCountryID = 0
        countryDict = {}
        countryNMDict = {}

        for bpType in filePaths:
            bpList = []
            path = filePaths[bpType]  # path for this type of BP
            # Accumulate the file into one giant string
            with open(path, 'r') as f:
                text = ""
                while f:
                    s = f.read()
                    if s == "":
                        break
                    else:
                        text = s

            lines = text.split("\n")  # split the one big string into one string per line

            # Extract the fields for the bpName, latitude and longitude
            for l in lines:
                # Skip lines that don't have what we're looking for
                if len(l) == 0:  # Empty line
                    continue
                if l[0] == "!":  # Commented out line
                    continue
                if len(l) > 72 and l[72] != "0":  # A 0 in column 75 is an official breakpoint
                    continue

                # Extract fields we want
                bpName = l[16:48].strip()
                bpName = bpName.replace("_", " ")
                if bpName == "":
                    continue

                # Extract lat/lon
                latStr = l[56:60]
                lonStr = l[60:67]
                # Convert from integer to degrees
                lat = int(latStr) / 100.0
                lon = int(lonStr) / 100.0

                x, y = self.getGridCell(lat, lon)
                if x is None or y is None:
                    continue

                # READ - May 18, 2023 - These breakpoints are on fictitious grid points on the map as the actual break points are
                # off the NHZ grid domain, This code exception replaces the lat/lon derived from the interactive map for these points with
                # the real geo-physical coordinates before writing them on to the latlon dictionary in the json file.

                if "Cape Verde (South)" in bpName:
                    lat = 15.06
                    lon = -23.95
                    print("*** ", bpName, lat, lon)
                elif "Cape Verde (North)" in bpName:
                    lat = 16.53
                    lon = -23.60
                    print("*** ", bpName, lat, lon)
                elif "Azores (Northwest)" in bpName:
                    lat = 39.50
                    lon = -31.19
                    print("*** ", bpName, lat, lon)
                elif "Azores (Southeast)" in bpName:
                    lat = 38.45
                    lon = -27.62
                    print("*** ", bpName, lat, lon)

                countryID = l[74:76]  # used to separate BP sequences
                countryNM = l[52:54]  # used to determine country for conflict checking
                bpInfo = ((y, x), (bpName, lat, lon))

                if (y, x) not in bpLocations:  # Add it to the list
                    bpLocations.append((y, x))
                    bpList.append(bpInfo)
                    # See if it's a new coastline segment and if so, save the last
                    countryDict[bpName] = countryID
                    countryNMDict[bpName] = countryNM
                    if bpType == "land":
                        # Save the last point for each segment so we can add separators
                        if countryID != currentCountryID:  # new coastline segment
                            currentCountryID = countryID
                else:  # Duplicate breakpoint
                    print(bpName, lat, lon, "is a duplicate.")
            bpDict[bpType] = bpList

        latLonDict = {}
        for bpType, bpList in filePaths.items():
            latLonDict[bpType] = OrderedDict(bpDict[bpType])

        return latLonDict, countryDict, countryNMDict

    def saveAdvisory(self, pil, stormInfo, siteID):
        """
        Check to see if we need to convert TY to HU upon saving.
        """

        self._saveAdvisory(pil, stormInfo)

    # Returns all the storminfo objects in a dictionary. Adds an
    # empty breakpoint entry if it's not there already
    def fetchStormInfo(self, hazList=None):
        """
        Fetch all the storm info dictionaries.
        """
        stormInfoDictList = self.getStormInfoDicts()
        stormInfoDicts = {}
        if not hazList:
            hazList = self._rankedWindKeys

        for stormInfo in stormInfoDictList:
            # Add the Breakpoints key if we don't have it.
            if "Breakpoints" not in stormInfo:
                stormInfo["Breakpoints"] = {}
            for hazard in hazList:
                if hazard == "<None>":
                    continue
                if hazard not in stormInfo["Breakpoints"]:
                    stormInfo["Breakpoints"][hazard] = []

            stormInfoDicts[stormInfo["pil"]] = stormInfo

        return stormInfoDicts

    def makeEditAreaName(self, bpName):
        """
        Converts a breakpoint name into a string that can be used as an editArea.
        """
        eaName = "WW_"
        # Remove special characters
        for c in bpName:
            if c.isalpha() or c.isdigit() or c == "_":
                eaName += c
            else:
                eaName += "_"

        return eaName

    def getBPZones(self, bpDict, haz):
        """
        Returns the list of zones for the specified bpDict and hazard.
        """
        bpList = bpDict[haz]
        zoneList = []
        for bp in bpList:
            bpZoneList = bpZoneDict.get(bp, None)
            if bpZoneList is None:
                continue
            for zone in bpZoneList:
                if zone not in zoneList:
                    zoneList.append(zone)

        return zoneList

    def getAdvisoryNames(self):
        """
        Fetches all of the advisory names.
        """
        fileNames = self._getStormAdvisoryNames()  # fetch the JSON fileNames
        # Strip the .json
        finalList = [fileName.replace(".json", "") for fileName in fileNames]
        return finalList

    def stormIDHistory(self):
        """
        Fetches the list of stormIDs that have been used in the past.
        """
        stormIDHistoryList = []
        try:
            stormIDHistoryList = self.getObject(self._historyObjName, self._historyCategory)
        except IOError:  # Object was not found so make an empty one.
            self.saveObject(self._historyObjName, stormIDHistoryList, self._historyCategory)

        return stormIDHistoryList

    def updateStormIDHistory(self, stormID):
        """
        Updates the storm history with the specified stormID.
        """
        stormIDHistoryList = self.stormIDHistory()
        if stormID not in stormIDHistoryList:
            stormIDHistoryList.append(stormID)
            # Save the list
            self.saveObject(self._historyObjName, stormIDHistoryList, self._historyCategory)

        return

    def getAllTropicalHazards(self, needETN=False):
        """
        Retrieves all tropical hazard for all timeRanges and returns the composite of all
        hazards for the entire time period.
        """

        trList = self.GM_getWEInventory("Hazards")
        if not trList:
            return None, None

        newGrid = self.empty(np.int8)
        newKeys = self._rankedWindKeys
        # newKeys = ["<None>"]
        for tr in trList:
            # Only look at Hazards that end in the future
            if tr.endTime() < self._gmtime():
                continue
            hazGrid, hazKeys = self.getGrids(self.mutableID(), "Hazards", "SFC", tr)
            for hazKey in hazKeys:
                if hazKey == "<None>":
                    continue
                hazIndex = self.getIndex(hazKey, hazKeys)
                mask = hazGrid == hazIndex
                if needETN:
                    tropKey = self.extractWindHazardwETN(hazKey)
                else:
                    tropKey = self.extractWindHazard(hazKey)
                if tropKey == "<None>":
                    continue
                newIndex = self.getIndex(tropKey, newKeys)
                zeroMask = newGrid == 0
                newGrid[mask & zeroMask] = newIndex

        return newGrid, newKeys

    def stripETN(self, hazKey):
        """
        Remove the ETN from the hazKey and return the result.
        """
        if ":" not in hazKey:
            return hazKey
        newKey = ""
        subKeys = hazKey.split("^")
        for subKey in subKeys:
            while ":" in subKey:
                pos = subKey.find(":")
                if pos >= 0:
                    delStr = subKey[pos:pos + 5]
                    subKey = subKey.replace(delStr, "")
                    newKey += subKey + "^"
        if newKey != "":
            if newKey[-1] == "^":
                return newKey[0:-1]

        return newKey

    def extractWindHazard(self, hazKey, windKeys=None):
        """
        Extract any hazards found in HazKey that matches any found in windKeys.
        Defaults to standard wind hazards if windKeys is None.
        """
        # Default to standard wind hazards
        if windKeys is None:
            windKeys = self._rankedWindKeys
        hazKey = self.stripETN(hazKey)
        newKey = ""
        subKeys = hazKey.split("^")
        for subKey in subKeys:
            if subKey in windKeys:
                newKey += subKey + "^"
        if newKey != "":
            if newKey[-1] == "^":
                return newKey[0:-1]
            else:
                return newKey

        return "<None>"

    def extractWindHazardwETN(self, hazKey, windKeys=None):  # This is the one that works for conflict checking.
        """
        Extract any hazards found in HazKey that matches any found in windKeys.
        Defaults to standard wind hazards if windKeys is None.
        """
        # Default to standard wind hazards
        if windKeys is None:
            windKeys = self._rankedWindKeys

        newKey = ""
        subKeys = hazKey.split("^")
        for subKey in subKeys:
            for windKey in windKeys:
                if windKey in subKey:
                    newKey += subKey + "^"
                    # print("extractWindHazard newKey building is: ", newKey)
        if newKey != "":
            if newKey[-1] == "^":
                return newKey[0:-1]
            else:
                return newKey

        return "<None>"

    # Extract just the wind hazards from the specified hazard grid.

    def calcRankGrid(self, hazardGrid, rankList=None):
        """
        Calculates a scalar grid based on the hazard phen/sig to compare hazard grids.
        """
        if rankList is None:
            rankList = self._rankedWindKeys
        rankGrid = self.empty()
        hazGrid, hazKeys = hazardGrid
        for hazKey in hazKeys:
            pureKey = self.extractWindHazard(hazKey, rankList)
            if pureKey in rankList:
                rankIndex = rankList.index(pureKey)
                hazIndex = self.getIndex(hazKey, hazKeys)
                mask = hazGrid == hazIndex
                rankGrid[mask] = rankIndex

        # Reset any "HU.A^TR.W ranking to "TR.W^HU.A" ranking so they are treated the identically.
        huatrwIndex = rankList.index("HU.A^TR.W")
        trwhuaIndex = rankList.index("TR.W^HU.A")
        rankGrid[rankGrid == huatrwIndex] = trwhuaIndex
        return rankGrid

    def getTextProductFromDB(self, productID, mode=None):
        """
        Retrieves a text product from the text database.
            Parameters:
                productID: ID of product to be retrieved
                mode: (optional) Should be one of "OPERATIONAL", "PRACTICE", or "TEST".
                       If omitted defaults to current CAVE mode.
        Returns:
            Full text of the requested product as a list of lines.
        """
        from com.raytheon.viz.gfe.product import TextDBUtil
        if not mode:
            mode = self.gfeOperatingMode()

        validModes = ("OPERATIONAL", "PRACTICE", "TEST")
        if mode.upper() not in validModes:
            raise ValueError("Invalid mode: '{}'. Expected one of {}".format(mode, validModes))

        opMode = mode.upper() in ("OPERATIONAL", "TEST")
        fullText = TextDBUtil.retrieveProduct(productID, opMode)
        textList = fullText.splitlines(True)
        return textList

    def getETNByStormName(self, stormName):
        """
        Lookup and return the ETN from the stormName and stormInfo
        """
        etnDict = self._etnDict

        for bin in self._stormInfoDict:
            stormInfo = self._stormInfoDict[bin]
            if stormInfo["stormName"] == stormName:
                stormNumber = stormInfo["stormNumber"]
                stormID = stormInfo["stormID"]
                etnValue = etnDict[stormID[0:2]]
                break

        return str(stormNumber + etnValue)

    def displayJSONHazards(self, weName, zoneMap):
        hazardOrder = ["<None>", "TR.A", "HU.A", "TR.W", "TR.W^HU.A", "HU.W"]
        self._stormInfoDict = self.fetchStormInfo(hazardOrder)
        # print self._stormInfoDict.keys()
        hazGrid = self.empty(np.int8)
        hazKeys = self.getDiscreteKeys("ProposedTropWindWW")
        for bin in self._stormInfoDict:
            stormName = self._stormInfoDict[bin]["stormName"]
            etn = self.getETNByStormName(stormName)
            if "zoneDict" in self._stormInfoDict[bin]:
                for hazard in self._stormInfoDict[bin]["zoneDict"]:
                    zoneList = self._stormInfoDict[bin]["zoneDict"][hazard]
                    mask = zoneMap.maskFromZoneList(zoneList)
                    hazETN = hazard + ":" + etn
                    hazIndex = self.getIndex(hazETN, hazKeys)
                    hazGrid[mask] = hazIndex
            if "inlandZoneDict" in self._stormInfoDict[bin]:
                for hazard in self._stormInfoDict[bin]["inlandZoneDict"]:
                    zoneList = self._stormInfoDict[bin]["inlandZoneDict"][hazard]
                    mask = zoneMap.maskFromZoneList(zoneList)
                    hazETN = hazard + ":" + etn
                    hazIndex = self.getIndex(hazETN, hazKeys)
                    hazGrid[mask] = hazIndex

        # Make a timeRange used for displaying the grid, one day long starting now.
        start = int((self._gmtime().unixTime()) / 3600) * 3600 - (6 * 3600)
        end = start + 24 * 3600
        self._timeRange = TimeRange.TimeRange(AbsTime.AbsTime(start),
                                              AbsTime.AbsTime(end))
        self.createGrid(self.mutableID(), weName, "DISCRETE", (hazGrid, hazKeys), self._timeRange,
                        discreteKeys=hazKeys, discreteOverlap=1, discreteAuxDataLength=5,
                        defaultColorTable="GFE/ProposedWind")

        return
