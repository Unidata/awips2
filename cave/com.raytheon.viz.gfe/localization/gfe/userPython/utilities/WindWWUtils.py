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
# Aug. 3, 2019 21020      tlefebvr    Added breakpointZoneList
# Aug. 16,2019 21020      tlefebvr    Code clean up.
# Aug 22, 2019 21020      tlefebvr    Code Review changes
# Apr 03, 2020 21020      tlefebvr/psantos    Spring 2020 Sprint
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
# June 3, 2020 22033      tlefebvr    Addressed code review comments.
# 
################################################################################

from collections import OrderedDict
import TropicalUtility
import operator
import functools

# This version derived from Breakpoints spreadsheet
bpZoneDict = OrderedDict([
    ("Mouth of the Rio Grande River - Port Mansfield" , ['TXZ257', 'TXZ256']),
    ("Port Mansfield - Baffin Bay" , ['TXZ351']),
    ("Baffin Bay - N Entrance Padre Island NS" , ['TXZ342', 'TXZ442']),
    ("N Entrance Padre Island NS - Port Aransas" , ['TXZ343', 'TXZ344', 'TXZ443']),
    ("Port Aransas - Mesquite Bay" , ['TXZ245', 'TXZ345', 'TXZ346']),
    ("Mesquite Bay - Port OConnor" , ['TXZ347', 'TXZ447']),
    ("Port OConnor - Matagorda" , ['TXZ335', 'TXZ336', 'TXZ436']),
    ("Matagorda - Sargent" , ['TXZ336', 'TXZ436']),
    ("Sargent - Freeport" , ['TXZ337', 'TXZ437']),
    ("Freeport - San Luis Pass" , ['TXZ337', 'TXZ437']),
    ("San Luis Pass - Port Bolivar" , ['TXZ313', 'TXZ338', 'TXZ438']),
    ("Port Bolivar - High Island" , ['TXZ214', 'TXZ300', 'TXZ438']),
    ("High Island - Sabine Pass" , ['TXZ215']),
    ("Sabine Pass - Cameron" , ['LAZ073']),
    ("Cameron - Intracoastal City" , ['LAZ052', 'LAZ074']),
    ("Intracoastal City - Morgan City" , ['LAZ052', 'LAZ053', 'LAZ054']),
    ("Lake Maurepas" , ['LAZ049', 'LAZ050', 'LAZ057']),
    ("Lake Pontchartrain" , ['LAZ040', 'LAZ058', 'LAZ060', 'LAZ061', 'LAZ062', 'LAZ063', 'LAZ064', 'LAZ072']),
    ("Morgan City - Grand Isle" , ['LAZ056', 'LAZ059', 'LAZ065', 'LAZ066', 'LAZ067', 'LAZ068']),
    ("Grand Isle - Mouth Mississippi River" , ['LAZ068', 'LAZ069']),
    ("Mouth Mississippi River - Mouth Pearl River" , ['LAZ069', 'LAZ070']),
    ("Mouth Pearl River - Bay St Louis" , ['MSZ080']),
    ("Bay St Louis - Ocean Springs" , ['MSZ081']),
    ("Ocean Springs - MS/AL border" , ['MSZ082']),
    ("MS/AL border - AL/FL border" , ['ALZ263', 'ALZ264', 'ALZ265', 'ALZ266']),
    ("AL/FL border - Navarre" , ['FLZ202', 'FLZ204']),
    ("Navarre - Okaloosa Walton County Line" , ['FLZ206']),
    ("Okaloosa Walton County Line - Walton Bay County Line" , ['FLZ108']),
    ("Walton Bay County Line - Panama City" , ['FLZ112']),
    ("Panama City - Mexico Beach" , ['FLZ112']),
    ("Mexico Beach - Indian Pass" , ['FLZ114']),
    ("Indian Pass - Apalachicola" , ['FLZ115']),
    ("Apalachicola - Ochlockonee River" , ['FLZ115']),
    ("Ochlockonee River - St Marks" , ['FLZ127']),
    ("St Marks - Aucilla River" , ['FLZ118', 'FLZ127']),
    ("Aucilla River - Keaton Beach" , ['FLZ128']),
    ("Keaton Beach - Steinhatchee River" , ['FLZ128']),
    ("Steinhatchee River - Suwannee River" , ['FLZ134']),
    ("Suwannee River - Yankeetown" , ['FLZ139']),
    ("Yankeetown - Chassahowitzka" , ['FLZ142']),
    ("Chassahowitzka - Aripeka" , ['FLZ148']),
    ("Aripeka - Anclote River" , ['FLZ149']),
    ("Anclote River - Egmont Key" , ['FLZ050', 'FLZ151']),
    ("Egmont Key - Anna Maria Island" , ['FLZ155']),
    ("Anna Maria Island - Middle of Longboat Key" , ['FLZ155']),
    ("Middle of Longboat Key - Englewood" , ['FLZ160']),
    ("Englewood - Boca Grande" , ['FLZ162']),
    ("Boca Grande - Bonita Beach" , ['FLZ165']),
    ("Dry Tortugas" , []),
    ("Key West - Seven Mile Bridge" , ['FLZ078']),
    ("Seven Mile Bridge - Craig Key" , ['FLZ077']),
    ("Craig Key - Key Largo" , ['FLZ076']),
    ("Key Largo - Ocean Reef" , ['FLZ076']),
    ("St Thomas and St John" , ['VIZ001']),
    ("St Croix" , ['VIZ002']),
    ("Puerto Rico" , ['PRZ001', 'PRZ002', 'PRZ003', 'PRZ004', 'PRZ005', 'PRZ006', 'PRZ007', 'PRZ008', 'PRZ009', 'PRZ010', 'PRZ011']),
    ("Culebra" , ['PRZ012']),
    ("Vieques" , ['PRZ013']),
    ("Bonita Beach - Chokoloskee" , ['FLZ069']),
    ("Chokoloskee - East Cape Sable" , ['FLZ075']),
    ("East Cape Sable - Flamingo" , ['FLZ075']),
    ("Flamingo - Card Sound Bridge" , ['FLZ174']),
    ("Card Sound Bridge - Ocean Reef Coastal" , ['FLZ173']),
    ("Ocean Reef Coastal - Golden Beach" , ['FLZ173']),
    ("Golden Beach - Hallandale Beach" , ['FLZ172']),
    ("Hallandale Beach - Deerfield Beach" , ['FLZ172']),
    ("Deerfield Beach - Boca Raton" , ['FLZ168']),
    ("Boca Raton - Jupiter Inlet" , ['FLZ168']),
    ("Jupiter Inlet - Stuart" , ['FLZ064']),
    ("Stuart - Fort Pierce" , ['FLZ059']),
    ("Fort Pierce - Vero Beach" , ['FLZ054', 'FLZ059']),
    ("Vero Beach - Sebastian Inlet" , ['FLZ054']),
    ("Sebastian Inlet - Cocoa Beach" , ['FLZ047']),
    ("Cocoa Beach - Volusia Brevard County Line" , ['FLZ147']),
    ("Volusia Brevard County Line - New Smyrna Beach" , ['FLZ141']),
    ("New Smyrna Beach - Flagler Volusia County Line" , ['FLZ141']),
    ("Flagler Volusia County Line - Marineland" , ['FLZ138']),
    ("Marineland - Crescent Beach" , ['FLZ133']),
    ("Crescent Beach - St Augustine" , ['FLZ133']),
    ("St Augustine - Ponte Vedra Beach" , ['FLZ133']),
    ("Ponte Vedra Beach - Nassau Sound" , ['FLZ125']),
    ("Nassau Sound - Mouth of St Marys River" , ['FLZ124']),
    ("Mouth of St Marys River - St Andrews Sound" , ['GAZ166']),
    ("St Andrews Sound - Altamaha Sound" , ['GAZ154']),
    ("Altamaha Sound - Savannah River" , ['GAZ116', 'GAZ117', 'GAZ118', 'GAZ119', 'GAZ138', 'GAZ139', 'GAZ140', 'GAZ141']),
    ("Savannah River - Edisto Beach" , ['SCZ047', 'SCZ048', 'SCZ049', 'SCZ051']),
    ("Edisto Beach - South Santee River" , ['SCZ045', 'SCZ050', 'SCZ052']),
    ("South Santee River - Murrells Inlet" , ['SCZ055', 'SCZ056']),
    ("Murrells Inlet - Little River Inlet" , ['SCZ054']),
    ("Little River Inlet - Cape Fear" , ['NCZ109', 'NCZ110']),
    ("Cape Fear - Surf City NC" , ['NCZ105', 'NCZ106', 'NCZ107', 'NCZ108']),
    ("Surf City NC - New River Inlet" , ['NCZ199']),
    ("New River Inlet - Bogue Inlet" , ['NCZ199']),
    ("Bogue Inlet - Cape Lookout" , ['NCZ195', 'NCZ196']),
    ("Cape Lookout - Ocracoke Inlet" , ['NCZ196']),
    ("Ocracoke Inlet - Cape Hatteras" , ['NCZ204', 'NCZ205']),
    ("Cape Hatteras - Oregon Inlet" , ['NCZ205']),
    ("Oregon Inlet - Duck" , ['NCZ203']),
    ("Pamlico Sound" , []),
    ("Albemarle Sound" , []),
    ("Duck - NC/VA border" , ['NCZ102']),
    ("NC/VA border - Cape Charles Light" , ['VAZ098']),
    ("Cape Charles Light - Parramore Island" , ['VAZ100']),
    ("Parramore Island - Chincoteague" , ['VAZ099']),
    ("Chincoteague - Fenwick Island" , ['MDZ024', 'MDZ025']),
    ("Chesapeake Bay New Point Comfort" , ['VAZ095', 'VAZ523', 'VAZ525']),
    ("Chesapeake Bay Windmill Point" , ['VAZ084', 'VAZ085', 'VAZ086']),
    ("Chesapeake Bay Smith Point" , ['VAZ077', 'VAZ078']),
    ("Chesapeake Bay Drum Point" , ['MDZ021', 'MDZ022', 'MDZ023']),
    ("Tidal Potomac Cobb Island" , ['VAZ075', 'VAZ077', 'MDZ017']),
    ("Tidal Potomac Indian Head" , ['VAZ075', 'MDZ016', 'VAZ052', 'VAZ055', 'VAZ057']),
    ("Chesapeake Bay North Beach" , ['MDZ018', 'MDZ015', 'MDZ019', 'MDZ020']),
    ("Chesapeake Bay Sandy Point" , ['MDZ014', 'MDZ012', 'MDZ015']),
    ("Chesapeake Bay Pooles Island" , ['MDZ011', 'MDZ008', 'MDZ012']),
    ("Chesa Bay North of Pooles Islnd" , ['MDZ508', 'MDZ008', 'MDZ012']),
    ("Tidal Potomac Key Bridge" , ['DCZ001','MDZ013','VAZ053', 'VAZ054']),
    ("Fenwick Island - Cape Henlopen" , ['DEZ003', 'DEZ004']),
    ("Cape Henlopen - Cape May" , ['DEZ003','DEZ004', 'NJZ023', 'NJZ024']),
    ("Cape May - Great Egg Inlet" , ['NJZ023', 'NJZ024']),
    ("Great Egg Inlet - Little Egg Inlet" , ['NJZ022', 'NJZ025']),
    ("Little Egg Inlet - Manasquan Inlet" , ['NJZ020', 'NJZ026', 'NJZ027']),
    ("Manasquan Inlet - Sandy Hook" , ['NJZ013', 'NJZ014']),
    ("Sandy Hook - East Rockaway Inlet" , ['NJZ012', 'NJZ013', 'NJZ006', 'NJZ106', 'NJZ108', 'NYZ072', 'NYZ073', 'NYZ074', 'NYZ075', 'NYZ176', 'NYZ178']),
    ("Delaware Bay South" , ['DEZ003','NJZ023']),
    ("Delaware Bay North" , ['DEZ002', 'NJZ021']),
    ("East Rockaway Inlet - Fire Island Inlet" , ['NYZ179']),
    ("Fire Island Inlet - Moriches Inlet" , ['NYZ080']),
    ("Moriches Inlet - Montauk Point" , ['NYZ081']),
    ("Montauk Point - Port Jefferson Harbor" , ['NYZ078', 'NYZ079']),
    ("Port Jefferson Harbor - Kings Point" , ['NYZ177']),
    ("Kings Point - Greenwich" , ['NYZ071']),
    ("Greenwich - New Haven" , ['CTZ009', 'CTZ010']),
    ("New Haven - Watch Hill" , ['CTZ010', 'CTZ011', 'CTZ012']),
    ("Block Island" , ['RIZ008']),
    ("Watch Hill - Point Judith" , ['RIZ006']),
    ("Point Judith - Westport" , ['RIZ006', 'RIZ007']),
    ("Nantucket" , ['MAZ024']),
    ("Marthas Vineyard" , ['MAZ023']),
    ("Westport - Woods Hole" , ['MAZ020', 'MAZ021']),
    ("Woods Hole - Chatham" , ['MAZ022']),
    ("Chatham - Provincetown" , ['MAZ022']),
    ("Provincetown - Sagamore Beach" , ['MAZ022']),
    ("Sagamore Beach - Hull" , ['MAZ019']),
    ("Hull - Gloucester" , ['MAZ007', 'MAZ015', 'MAZ016']),
    ("Gloucester - Merrimack River" , ['MAZ007']),
    ("Merrimack River - Portsmouth" , ['NHZ014']),
    ("Portsmouth - Cape Elizabeth" , ['MEZ023', 'MEZ024']),
    ("Cape Elizabeth - Port Clyde" , ['MEZ024', 'MEZ025', 'MEZ026', 'MEZ027']),
    ("Port Clyde - Stonington ME" , ['MEZ027', 'MEZ028']),
    ("Stonington ME - Petit Manan Point" , ['MEZ029']),
    ("Petit Manan Point - Eastport" , ['MEZ030']),
    ("Eastport - US Canadian Border" , ['MEZ030']),
    ("Point Piedras Blancas - Point Sal" , ['CAZ034']),    
    ("Point Sal - Point Conception" , ['CAZ035']),
    ("Point Conception - Point Mugu" , ['CAZ039', 'CAZ040', 'CAZ549', 'CAZ550']),
    ("Point Mogu - Orange Los_Angeles Co_Line" , ['CAZ041']),
    ("Orange Los Angeles Co Line - San Diego Orange Co Line" , ['CAZ087','CAZ552']),
    ("San Diego Orange Co Line - US Mexico Border", ['CAZ043']),
    
    # HFO Breakpoints / Zones
    ("Hawaii County",  ['HIZ023','HIZ024','HIZ025','HIZ026','HIZ027','HIZ028']),
    ("Maui County, including the islands of Maui, Lanai, Molokai and Kahoolawe", 
           ['HIZ012','HIZ013','HIZ014','HIZ015','HIZ016','HIZ017','HIZ018','HIZ019','HIZ020','HIZ021','HIZ022']),
    ("Oahu", ['HIZ005','HIZ006','HIZ007','HIZ008','HIZ009','HIZ010','HIZ011']),
    ("Kauai County, including the islands of Kauai and Niihau", ['HIZ001','HIZ002','HIZ003','HIZ004']),
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

        # Defines which sites are responsible for which basins
        self._basinDomains = {
            "Atlantic" : ["NHA", "NHZ", "NHP"],
            "Eastern Pacific" :  ["NHA", "NHZ", "NHP"],
            "Central Pacific" : ["HPA"],
            "Western Pacific" : ["GUM"],
            }
        
        # Defines the "bins" that are used for each basin
        self._basinBins = {
            "Atlantic" : ["AT1", "AT2", "AT3", "AT4", "AT5"],
            "Eastern Pacific" : ["EP1", "EP2", "EP3", "EP4", "EP5"],
            "Central Pacific" : ["CP1", "CP2", "CP3", "CP4", "CP5"],
            "Western Pacific" : ["WP1", "WP2", "WP3", "WP4", "WP5"],
            }
        
        self._maxStorms = 30
    
    def basinNames(self):
        """
        Returns the list of basin names.
        """
        return self._basinDomains.keys()
    
    def maxStorms(self):
        return self._maxStorms
        
    def forecastBasins(self, siteID):
        """
        Returns the list of basins for which the specified siteID is responsible.
        """
        basinList = []
        for basinName, siteList in self._basinDomains.items():
            if siteID in siteList:
                basinList.append(basinName)
        return basinList
    
    def basinBins(self, basinList):
        """
        Returns the list of bins defined for the given basin
        """
        binList = []
        for basinName in basinList:
            binList.append(self._basinBins.get(basinName, None))
            
        return binList
    
    def etnDict(self):
        return {
            "AT" : 1000,
            "EP" : 2000,
            "CP" : 3000,
            "WP" : 4000,
            }

    def NHCSites(self):
        """
        Returns the list of NHC sites.
        """
        return ["NHA", "NHZ", "NHP"]
    
    def HFOSites(self):
        """
        Returns the list of HFO sites.
        """
        return ["HPA"]
    
    def GUMSites(self):
        """
        Returns the list of GUM sites.
        """
        return ["GUM", "PQE", "PQW"]

    def getStormInfoDicts(self):
        """
        Retrieves the storm info from the JSON file. Additionally, converts all
        the data from unicode to ordinary strings so it can be used an ordinary
        dictionary.
        """
        # Recursive method to convert unicode strings to ordinary strings
        def decodeUnicode(object):


            if isinstance(object, list):
                newList = []
                for item in object:
                    newList.append(decodeUnicode(item))
                return newList

            elif isinstance(object, tuple):
                return tuple(decodeUnicode(list(object)))

            elif isinstance(object, dict):
                newDict = {}
                for key in object:
                    newKey = decodeUnicode(key)
                    newDict[newKey] = decodeUnicode(object[key])
                return newDict

            return object

        stormInfoList = self.extractStormInfo(filterATOnly=False)
        
        return decodeUnicode(stormInfoList)

    def breakpointZoneList(self):
        """
        Return a list of all the zones included in the breakpoint dictionary (bpDict)
        """
        bpValues = bpZoneDict.values()
        # Flatten to a simple list
        bpList = functools.reduce(operator.add, bpValues)
        # Reduce to unique values
        bpList = list(set(bpList))

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
                if len(l) == 0:   # Empty line
                    continue
                if l[0] == "!":   # Commented out line
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
                
                countryID = l[74:76]  # used to separate BP sequences
                bpInfo = ((y, x), (bpName, lat, lon))
                
                if (y, x) not in bpLocations:  # Add it to the list
                    bpLocations.append((y, x))
            
                    bpList.append(bpInfo)
                    # See if it's a new coastline segment and if so, save the last
                    countryDict[bpName] = countryID
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

        return latLonDict, countryDict

    # Returns all the storminfo objects in a dictionary. Adds an
    # empty breakpoint entry if it's not there already
    def fetchStormInfo(self, hazList):
        """
        Fetch all the storm info dictionaries.
        """
        stormInfoDictList = self.getStormInfoDicts()
        stormInfoDicts = {}
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
        allowedChars = string.ascii_letters + string.digits + "_"
        for c in bpName:
            if c in allowedChars:
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
        fileNames = self._getStormAdvisoryNames() # fetch the JSON fileNames
        # Strip the .json
        finalList = [fileName.replace(".json", "") for fileName in fileNames]
        return finalList
    
    def makeStormID(self, pil, stormNumber):
        """
        Makes a stormID from the specified pil and stormNumber.
        """
        now = self._gmtime().unixTime()
        yearStr = self._gmtime().strftime("%Y")
        basinID = pil[0:2]
        if basinID == "AT":
            basinID = "AL"

        stormID = basinID + str(stormNumber).zfill(2) + yearStr

        return stormID

    def stormIDHistory(self):
        """
        Fetches the list of stormIDs that have been used in the past.
        """
        stormIDHistoryList = []
        try:
            stormIDHistoryList = self.getObject(self._historyObjName, self._historyCategory)
        except IOError:   # Object was not found so make an empty one.
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
