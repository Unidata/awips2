
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TropicalHazards
#
# Author:  Matthew H. Belk WFO BOX
# ----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Jun 22, 2013           mbelk     Initial creation
# Jul 14, 2016           mbelk     Changes for 2017 season
# Sep 19, 2016  19293    randerso  Initial baseline check in
#
########################################################################

import time, string, os, re, errno

import Header
import JsonSupport, LocalizationSupport
import LogStream
import ModuleAccessor
import pprint as pp


class TropicalHazards(Header.Header):

    def __init__(self):

        Header.Header.__init__(self)

        #-----------------------------------------------------------------------
        #  Make a link to the TropicalAreaDictionary

        # Access the information for the breakpoint area(s) if available
        self._tropicalAreaDict = \
            ModuleAccessor.ModuleAccessor().variable("AreaDictionary",
                                                     "AreaDictionary")

        #  Then make a cache of sorted hash keys to this dictionary
        self._tropicalAreaDictKeys = self._tropicalAreaDict.keys()
        self._tropicalAreaDictKeys.sort(self._sortBreakpoints)


################################################################################
#  New common utility methods for tropical hazard formatters
################################################################################

    def allowedHazards(self):
        """TropicalHazards version of GenericHazards.allowedHazards.

        This method defines the allowed hazards for tropical hazard products.
        """
        tropicalActions = ["NEW", "EXA", "CAN", "CON"]
        return [
            ('HU.W',tropicalActions,'Hurricane'),   
            ('HU.A',tropicalActions,'Hurricane'),        
            ('SS.W',tropicalActions,'Surge'),       
            ('SS.A',tropicalActions,'Surge'),       
            ('TR.W',tropicalActions,'Tropical'),   
            ('TR.A',tropicalActions,'Tropical'),    
            ]


################################################################################
#  New common utility methods for tropical hazard formatters
################################################################################

    #===========================================================================
    #  Define a method to determine which WFOs are impacted based on a list of
    #  affected edit area ID's

    def _getAffectedWFOs(self, idList):
        """TropicalHazards addition of _getAffectedWFOs.

        This method will produce a list of NWS WFOs impacted by tropical
        hazards with the specified identifiers.

        Arguments:
            idList -> list TropicalAreaDictionary keys
        """

        #  Look at each breakpoint segment in this list
        for id in idList:

            #  Get the AreaDictionary entry for this segment
            if self._tropicalAreaDict.has_key(id):
                entry = self._tropicalAreaDict[id]
            else:
                LogStream.logProblem(\
                  "AreaDictionary missing definition for [" + id + "].")
                continue

            #  Get the WFO responsible for this segment - always use the WFO of the
            #  starting point to avoid including WFOs unnecessarily
            if 'wfo' in entry:
                wfo = entry['wfo'].strip()
            else:
                LogStream.logProblem(\
                  "AreaDictionary missing WFO definition for [" + id + "].")
                continue  

            #  If we have a valid WFO identifier, and it is not already noted in the
            #  impacted WFO list
            if len(wfo) > 0 and wfo not in self._wfoList:

                #  Add this WFO to the list of impacted WFOs
                self._wfoList.append(wfo)


    #===========================================================================
    #  Define a method to format a breakpoint text line within a TCV segment

    def _formatTCVline(self, entry, type="start"):
        """TropicalHazards addition of _formatTCVline.

        This method will produce a list of NWS WFOs impacted by tropical
        hazards with the specified identifiers.

        Arguments:
            entry -> TropicalAreaDictionary entry for an edit area
            type -> type of breakpoint to produce (optional, defaults to start)
        """

        #  If this is not an ending point
        if type != "end":

            #  Get the information we need
            point = entry["startBreakpoint"].strip()
            lat = entry["startLat"].strip()
            lon = entry["startLon"].strip()
            state = entry["startState"].strip()

        #  Otherwise - get the end point info for this segment
        else:

            #  Get the information we need
            point = entry["endBreakpoint"].strip()
            lat = entry["endLat"].strip()
            lon = entry["endLon"].strip()
            state = entry["endState"].strip()


        #  Clean up the state so there are no spaces or dashes
        state = re.sub("[ _-]+", "", state)

        #-----------------------------------------------------------------------
        #  If this is not the border of a state or country

        if re.search("(?i)border", point) is None:

            #  Add the state/country
            point += "-" + state

        #-----------------------------------------------------------------------
        #  Append the appropriate hemisphere of the latitude

        if lat.find("-") != -1:
            lat += "S"
            lat = lat.replace("-", "")
        else:
            lat += "N"

        #-----------------------------------------------------------------------
        #  Append the appropriate hemisphere of the longitude

        if lon.find("-") != -1:
            lon += "W"
            lon = lon.replace("-", "")
        else:
            lon += "E"

        #-----------------------------------------------------------------------
        # Now construct the final formatted line

        text = "%-36s%6s%7s\n" % (re.sub("[ _]+", "-", point) + " ", lat, lon)

        #  Return the text
        return text


    #===========================================================================
    #  Define a method to sort breakpoint record keys

    def _sortBreakpoints(self, a, b):
        """TropicalHazards addition of _sortBreakpoints.

        This method will produce a sorted list of breakpoint segments.  This
        ensures the order of contiguous breakpoint segments
        """

        #  Make a list of valid string identifier parts
        validTypes = [
            "LN",   #  mainland segments - any country
            "KEY",  #  Florida Keys
            "ISL", 	#  islands
            "CUBA", #  Cuba
            "HISP", #  Hispaniola
            "NAI", 	#  North Atlantic islands
            "WTDE", #  Deleware Bay
            "WTTP",	#  Tidal Potomac
            "WTCP", #  Chesapeake Bay
            "WTPT", #  Generic water points

            #  Zones used by VTEC
            "GYC",	#  Guyana
            "VEC",	#  Venezuela
            "COC",	#  Colombia
            "PAC",	#  Panama
            "CRC",	#  Costa Rica
            "NIC",	#  Nicaragua
            "HNC",	#  Honduras
            "GTC",	#  Guatemala
            "BZC",	#  Belize
            "MXC",	#  Mexico
            "USC",  #  United States
            "CNC",  #  Canada
            "KEC",  #  Dry Tortugas
            "AWC",	#  Aruba
            "CWC",	#  Curacao
            "TTC",	#  Trinidad and Tobago
            "BBC",	#  Barbados
            "LCC",	#  St. Lucia
            "MQC",	#  France - Caribbean
            "AGC",	#  Antigua and Barbuda
            "BSC",	#  Bahamas
            "BMC",	#  Bermuda
            "JMC",	#  Jamaica
            "KYC",	#  Cayman Islands
            "CUC",	#  Cuba
            "DOC",	#  Dominican Republic
            "HTC",	#  Haiti
            "PMC",	#  France - North Atlantic
            "LOC",	#  Lake_Okeechobee
            "FBC",	#  Florida Bay
            "PSC",	#  Pamlico Sound
            "ASC",	#  Albemarle Sound
            "TXZ",    #  Texas
            "LAZ",    #  Louisiana
            "MSZ",    #  Mississippi
            "ALZ",    #  Alabama
            "FLZ",    #  Florida
            "GAZ",    #  Georgia
            "SCZ",    #  South Carolina
            "NCZ",    #  North Carolina
            "VAZ",    #  Virginia
            "MDZ",    #  Maryland
            "DCZ",    #  District of Columbia
            "DEZ",    #  Deleware
            "NJZ",    #  New Jersey
            "NYZ",    #  New York
            "CTZ",    #  Connecticut
            "RIZ",    #  Rhode Island
            "MAZ",    #  Massachusetts
            "NHZ",    #  New Hampshire
            "MEZ",    #  Maine
            "NMZ",    #  New Mexico
            "ARZ",    #  Arkansas
            "OKZ",    #  Oklahoma
            "MOZ",    #  Missouri
            "TNZ",    #  Tennessee
            "WVZ",    #  West Virginia
            "PAZ",    #  Pennsylvania
            "VTZ",    #  Vermont
            "PRZ",    #  Puerto Rico
            "VIZ",    #  U.S. Virgin Islands
            "RE",     #  General edit area collection
        ]

        #  Get the first part of each identifier
        aSeg = a.split("_")[0]
        bSeg = b.split("_")[0]

        #  Get ready to split these identifiers into alpha and numeric parts
        aSegType = ""
        bSegType = ""
        aSegNum = ""
        bSegNum = ""

        #  Start with the alpha components
        for c in aSeg:
            if c in string.letters:
                aSegType = aSegType + c

        for c in bSeg:
            if c in string.letters:
                bSegType = bSegType + c

        #  Now get the numeric components
        for c in aSeg:
            if c in string.digits:
                aSegNum = aSegNum + c

        for c in bSeg:
            if c in string.digits:
                bSegNum = bSegNum + c

        #  Determine the order of these areas based on segment type first
        aTypeIndex = validTypes.index(aSegType)
        try:
            bTypeIndex = validTypes.index(bSegType)
        except:
            bTypeIndex = aTypeIndex
            bSegNum = aSegNum

        #  Try to determine order based on segment type
        if aTypeIndex < bTypeIndex:
            return -1
        elif bTypeIndex < aTypeIndex:
            return 1

        #  If the segment types are the same, use the numeric component last
        if int(aSegNum) < int(bSegNum):
            return -1
        elif int(bSegNum) < int(aSegNum):
            return 1
        else:
##            print "ERROR!!!!!!! Segment names are equal!!!!!!!"
            return 0


    #===========================================================================
    #  Define a method to organize breakpoint segment zones by government

    def _organizeZonesByGovt(self):
        """TropicalHazards addition of _organizeZonesByGovt.

        This method will produce a dictionary of sorted lists where each list
        contains all the zone identifiers for which that government is the
        responsible entity.
        """

        #  Create a dictionary to hold all zones managed by a government
        zoneDict = {}

        #  Also make a list to hold governments in the order we find them
        govtList = []

        #  Initialize some lists for handling zone codes
        ugcList = []
        ugcListUsa = []

        #-----------------------------------------------------------------------
        #  Look at each edit area we have

        for key in self._tropicalAreaDictKeys:

            #  Get the type of this zone
            zoneId = key.split("_")[0]

            zoneType = ""

            #  Get all the letters from this zone ID
            for c in zoneId:
                if not c.isdigit():
                    zoneType += c

            #  If this is primary representation of segment zones
            #  (this is to avoid duplication with the individual zones)
            #  Make an exception for US zones though
            if zoneType in ["LN", "KEY", "CUBA", "HISP", "ISL", "NAI", "WTDE",
                            "WTTP", "WTCP", "WTPT", "USC"]:

                #  Get the government responsible for this zone
                zoneGovt = self._tropicalAreaDict[key]["hazardIssuer"].strip()

                #  Handle the case of the USA, which receives no attribution
                if zoneGovt.strip() == "":
                    zoneGovt = "US"

                #  If we do not already know about this government
                if zoneGovt not in govtList:

                    #  Add it now
                    govtList.append(zoneGovt)

                #---------------------------------------------------------------
                #  Get the impacted zones for this segment

                ugcCode = self._tropicalAreaDict[key]["ugcCode"].strip()

                #---------------------------------------------------------------
                #  Expand UGC code if there is more than one zone represented

                if len(ugcCode.split("-")) > 1 or ugcCode.find(">") != -1:
                    ugcList = self.expandComplexUgc(ugcCode)

                #  Otherwise, just use this single zone
                else:
                    ugcList = [ugcCode]

                #---------------------------------------------------------------
                #  Expand UGC code if there is more than one zone represented

                try:
                    ugcCodeUsa = self._tropicalAreaDict[key]["ugcCodeUsa"].strip()

                    if len(ugcCodeUsa.split("-")) > 1 or ugcCodeUsa.find(">") != -1:
                        ugcListUsa = self.expandComplexUgc(ugcCodeUsa)

                    #  Otherwise, just use this single zone
                    else:
                        ugcListUsa = [ugcCodeUsa]

                except:
                    ugcListUsa = []

                #---------------------------------------------------------------
                #  If we already have an entry for this government

                if zoneDict.has_key(zoneGovt):

                    #  Get the zones already associated
                    curZoneList = zoneDict[zoneGovt]

                #  Otherwise make a new list for this governement
                else:
                    curZoneList = []

                #---------------------------------------------------------------
                #  Now add all the new zones

                for ugc in ugcList + ugcListUsa:

                    #  If we don't already have this ugc
                    if ugc not in curZoneList and len(ugc.strip()) > 0:

                        #  Add it
                        curZoneList.append(ugc)

##                print curZoneList

                #  Sort the UGC list
                curZoneList.sort()

                #  Store the list of zones for this government
                zoneDict[zoneGovt] = curZoneList

        #  Always ensure the USA comes first
        if "US" in govtList:
            govtList.remove("US")
        finalGovtList = ["US"] + govtList

        #  Return the completed dictionary
        return zoneDict, finalGovtList


    #===========================================================================
    #  Define a method to filter a segment list by government

    def _filterAreaListByGovernment(self, govtList, areaList):
        """TropicalHazards addition of _filterAreaListByGovernment.

        This method will produce a list of all zones managed by a particular
        government contained within the specified area list.

        Arguments:
            govtList -> list of identifiers managed by a government
            areaList -> list of edit area identifiers to process
        """

        #  Initialize a new list
        newList = []

        #  Look through each edit area
        for area in areaList:

            #  If this edit area is managed by this government
            if area in govtList:

                #  Add it to the filtered list
                newList.append(area)

        #  Return the filtered list
        return newList


    #===========================================================================
    #  Define a method to filter a segment list by government

    def _organizeAreasByType(self, areaList):
        """TropicalHazards addition of _organizeAreasByType.

        This method will separate a list of areas into one of four types:
        mainland segments, UGC zones, zones and islands.  These will be stored
        in the processed hazard dictionary for easier access later.

        Arguments:
            areaList -> list of edit area identifiers to process
        """

        #  Initialize both lists
        segmentList = []
        ugcZoneList = []
        zoneList = []
        islandList = []
        waterList = []

        #  Look through each edit area
        for area in areaList:

            #  Assume this is a "land" area
            areaType = "land"

            #  If the TropicalAreaDictionary has a record for this area
            if self._tropicalAreaDict.has_key(area):

                #  Get the type of this area - if we can
                if self._tropicalAreaDict[area].has_key("segmentType"):
                    areaType = self._tropicalAreaDict[area]["segmentType"]

                #  Get the type of this area
                try:
                    usaZoneList = self.expandComplexUgc(
                                    self._tropicalAreaDict[area]["ugcCodeUsa"])
                except:
                    usaZoneList = []


                #---------------------------------------------------------------
                #  If this is an island

                if areaType == "island":

                    #  If we do not already have a record for this area
                    if area not in islandList:

                        #  Add it to the list of islands
                        islandList.append(area)

                #---------------------------------------------------------------
                #  Otherwise, if this is a water area

                elif areaType == "water":

                    #  If we do not already have a record for this area
                    if area not in waterList:

                        #  Add it to the list of islands
                        waterList.append(area)

                #---------------------------------------------------------------
                #  Otherwise, organize the land-based areas

                else:

                    #  If this is a zone-based identifier
                    if len(area) == 6 and area[2] in ["Z", "C"]:

                        #  Place this zone into the proper list
                        if area[2] == "Z":

                            #  If this area has not already been recorded
                            if area not in ugcZoneList:

                                #  Add it to the UGC zone list
                                ugcZoneList.append(area)
                        else:

                            #  If this area has not already been recorded
                            if area not in zoneList:

                                #  Add it to the zone list
                                zoneList.append(area)

                            #  If there any zones associated with this segment
                            if len(usaZoneList) > 0:

                                for usZone in usaZoneList:

                                    if usZone not in ugcZoneList:

                                        #  Add it to the UGC zone list
                                        ugcZoneList.append(usZone)

                    #  Otherwise, this is a breakpoint segment
                    elif area not in segmentList:
                        segmentList.append(area)

                        #  Get any UGC codes associated with this segment
                        areaUgc = self._tropicalAreaDict[area]["ugcCode"]

                        #  If there is more than 1 zone associated with segment
                        if len(areaUgc) > 7:

                            #  Expand the UGC codes
                            ugcList = self.expandComplexUgc(areaUgc)

                        #  Otherwise, make a simpler list so we can proceed
                        else:
                            ugcList = [areaUgc]

                        #-------------------------------------------------------
                        #  Add each zone code into the list as needed

                        for ugcCode in ugcList:

                            #  Clean up any extra characters
                            ugcCode = ugcCode.replace("-", "")

                            #  If this is a zone-based identifier
                            if len(ugcCode) >= 6 and ugcCode[2] in ["Z", "C"]:

                                #  Place this zone into the proper list
                                if ugcCode[2] == "Z":

                                    #  If this area has not already been recorded
                                    if ugcCode not in ugcZoneList:

                                        #  Add it to the UGC zone list
                                        ugcZoneList.append(ugcCode)
                                else:

                                    #  If this area has not already been recorded
                                    if ugcCode not in zoneList:

                                        #  Add it to the zone list
                                        zoneList.append(ugcCode)


        #-----------------------------------------------------------------------
        #  Sort all lists to keep them ordered - as needed

        if len(segmentList) > 1:
            segmentList.sort(self._sortBreakpoints)

        if len(ugcZoneList) > 1:
            ugcZoneList.sort(self._sortBreakpoints)

        if len(zoneList) > 1:
            zoneList.sort(self._sortBreakpoints)

        if len(islandList) > 1:
            islandList.sort(self._sortBreakpoints)

        if len(waterList) > 1:
            waterList.sort(self._sortBreakpoints)

        #  Return the compiled lists
        return (segmentList, ugcZoneList, zoneList, islandList, waterList)


    #===========================================================================
    #  Define a method to construct a processed hazard dictionary

    def _constructHazardDict(self, hazardPhenSig, filterEtn):
        """TropicalHazards addition of _constructHazardDict.

        This method will produce a processed dictionary of tropical hazards
        for easier use later on.

        Arguments:
            hazardPhenSig -> dictionary of hazards keyed by phenomenon and
                             significance. Values are a list of all hazards
                             which share that same phenomenon and significance.
            filterEtn -> Event Tracking Number of interest which will be used
                         to filter hazards for a particular product.
        """

        #-----------------------------------------------------------------------
        #  Get ready to populate the hazard dictionary for this storm

        hazardAreaDict = {}
        hazardAreaDictKeyList = []

        #  Assume this is going to be the last product we issue for this storm
        self._allCAN = True

        #=======================================================================
        #  Look for each of the tropical hazards in order

        for phenSig in [("SS","W"), ("HU","W"), ("SS","A"), ("HU","A"),
                        ("TR","W"), ("TR","A")]:

            if hazardPhenSig.has_key(phenSig):
                print "="*90
                print "\n\tConstructing -> %s" % (repr(phenSig))
                print len(hazardPhenSig[phenSig]), hazardPhenSig[phenSig]

                #  Look through all the sampled hazards
                for phen in hazardPhenSig[phenSig]:
                    print "-"*90
                    print "phen = %s" % (phen)

                    #  Set aside the headline for each action in this area
                    NEW = []
                    CAN = []
                    UPG = []
                    EXA = []
                    CON = []

                    #-----------------------------------------------------------
                    #  If we have items for this particular phen.sig
                    #  combination, and this is the storm we are after

                    if phen["etn"] != filterEtn:

                        print "\tWrong storm!", phen

                        #  Move on to the next one
                        continue


                    #  Get the full VTEC code for this phenomena
#                     curHazardKey = (phen["act"], phen["key"])
                    curHazardKey = (phen["act"], phen["phensig"])
                    print "+++++  %s" % (repr(curHazardKey))

                    #  If this action is anything other than "CAN", indicate it
                    #  so we don't delete the JSON file for this storm at end
                    if phen["act"] != "CAN":
                        self._allCAN = False

                    #  If we do not have the ETN of this hazard
                    if re.search(":\d{4}$", curHazardKey[1]) is None:

                        newHazardType = curHazardKey[1]  + ":%d" % \
                                        (phen["etn"])
                    else:
                        newHazardType = ""

                    #  If we need to adjust the hazard key
                    if len(newHazardType) > 0:

                        #  Make the changes
                        newCurHazardKey = (curHazardKey[0], newHazardType)
                        curHazardKey = newCurHazardKey

                    #  See if there are upgrades or replacements for this area
                    areaHazardList = self._hazards.getHazardList(phen["id"])

                    #-----------------------------------------------------------
                    #  Construct a hazard key which incorporates all hazards
                    #  and actions for this area

                    tempHazardList = [curHazardKey]
                    for areaHazard in areaHazardList:

                        #-------------------------------------------------------
                        #  Record headline for each action we find

                        if areaHazard["act"] == "NEW" and \
                           areaHazard["hdln"] not in NEW:
                            NEW.append(areaHazard["hdln"])

                        elif areaHazard["act"] == "CAN" and \
                           areaHazard["hdln"] not in CAN:
                            CAN.append(areaHazard["hdln"])

                        elif areaHazard["act"] == "UPG" and \
                           areaHazard["hdln"] not in UPG:
                            UPG.append(areaHazard["hdln"])

                        elif areaHazard["act"] == "CON" and \
                           areaHazard["hdln"] not in CON:
                            CON.append(areaHazard["hdln"])

                        elif areaHazard["act"] == "EXA" and \
                           areaHazard["hdln"] not in EXA:
                            EXA.append(areaHazard["hdln"])

                        #-------------------------------------------------------
                        #  Make a key for this particular hazard/action combo

                        tempHazardKey = (areaHazard["act"], areaHazard["phensig"])

                        #  If we do not have the ETN of this hazard
                        if re.search(":\d{4}$", tempHazardKey[1]) is None:

                            newHazardType = tempHazardKey[1]  + ":%d" % \
                                            (areaHazard["etn"])
                        else:
                            newHazardType = ""

                        #  If we need to adjust the hazard key
                        if len(newHazardType) > 0:

                            #  Make the changes
                            newTempHazardKey = (tempHazardKey[0], newHazardType)
                            tempHazardKey = newTempHazardKey

                        #  If this is not already part of the hazard key
                        if tempHazardKey != curHazardKey:

                            #  Add this hazard/action combo to the list
                            tempHazardList.append(tempHazardKey)

                    #  Sort the keys so we have some consistency in ordering
                    tempHazardList.sort()

                    #  Convert the list of hazards for this area into a tuple
                    #  so we can use it as a dictionary key
                    hazardKey = tuple(tempHazardList)

                    #-----------------------------------------------------------
                    #  Ensure we only group areas associated with same storm
                    #  and hazard/action combos

                    #  If we already have an entry for this storm and hazard
                    if hazardAreaDict.has_key(hazardKey):

                        #  Add to what is already there
                        tempList = hazardAreaDict[hazardKey]["AREAS"]
                        tempList.append(phen["id"])
                        hazardAreaDict[hazardKey]["AREAS"] = tempList

                    #  Otherwise, make a new entry
                    else:
##                            "AREAS":[phen["id"]], "HDLN":phen["hdln"],
                        hazardAreaDict[hazardKey] = {
                            "AREAS":[phen["id"]], "NEW":NEW, "CAN":CAN,
                            "CON":CON, "EXA":EXA, "UPG":UPG
                        }

                    #  Add this key the list
                    if hazardKey not in hazardAreaDictKeyList:
                        hazardAreaDictKeyList.append(hazardKey)
                        
        print "\n\n", "+"*100
        print "in the middle"
        print pp.pformat(hazardAreaDictKeyList), "\n"
        print pp.pformat(hazardAreaDict)

        #-----------------------------------------------------------------------
        #  Keep track of segments and zones.  We will need segments to
        #  ensure proper grouping and breakpoints.  We will need UGC zones
        #  to get the proper VTEC action

        segments = []
        ugcZones = []
        zones = []
        islands = []
        water = []

        #=======================================================================
        #  Organize all the impacted areas by type

        for key in hazardAreaDictKeyList:

            #-------------------------------------------------------------------
            #  Organize various areas associated with this hazard by type

            (segments, ugcZones, zones, islands, water) = \
                       self._organizeAreasByType(hazardAreaDict[key]["AREAS"])

            #  Add these organized zones to the dictionary for this hazard
            hazardAreaDict[key]["SEGMENTS"] = segments
            hazardAreaDict[key]["UGCZONES"] = ugcZones
            hazardAreaDict[key]["ZONES"] = zones
            hazardAreaDict[key]["ISLANDS"] = islands
            hazardAreaDict[key]["WATER"] = water

        print "+"*90 + "\nFinally!"
        print "found -> %s" % (repr(hazardAreaDictKeyList))
        print "allCAN = %s" % (self._allCAN)
        for key in hazardAreaDictKeyList:
            print "-"*60
            print "%s\n\t%s" % (key, pp.pformat(hazardAreaDict[key]))
        
        #  Return the completed hazard dictionary and sorted keys
        return (hazardAreaDict, hazardAreaDictKeyList)


    #===========================================================================
    #  Process the shortcut hazard dictionary and organize a list of segments
    #  which are organized by similar type.

    def _constructSegmentList(self, hazardAreaDict, hazardAreaDictKeyList,
                              UStcv=0):
        """TropicalHazards addition of _constructSegmentList.

        This method will produce create all the appropriate segments which
        should go into a tropical hazard product, particularly a TCV.

        Arguments:
            hazardAreaDict -> processed dictionary of hazards
            hazardAreaDictKeyList -> list of keys of active hazards to process.
            UStcv -> (optional) toggle to group islands within a single segment
                     {1 = Yes (default - USA TCV) / 0 = No}
        """

        #  Initialize a list to hold the final group of segments
        finalSegmentList = []

        segmentDict = {"CUC": [], "HTC":[], "DOC":[], "USC":[]}

        #-----------------------------------------------------------------------
        #  Look at every hazard we found

        for hazardKey in hazardAreaDictKeyList:

            segmentList = []
            zoneList = []

            #-------------------------------------------------------------------
            #  Double check to be sure Cuba and Hispaniola are not part of
            #  the mainland segment lists

            for curId in hazardAreaDict[hazardKey]["SEGMENTS"]:

                #  If this is a Cuba or Hispaniola code
                if curId[:3] in segmentDict.keys():

##                    print "Found segment -> curId =", curId
                    #  Add this identifier to the dictionary
                    tempList = segmentDict[curId[:3]]

                    #  If this identifier is not already in the list
                    if curId not in tempList:
                        tempList.append(curId)

                    #  Store the updated list
                    segmentDict[curId[:3]] = tempList

                #  Otherwise,
                elif curId not in segmentList:

                    segmentList.append(curId)

            #-------------------------------------------------------------------
            #  Double check to be sure Cuba and Hispaniola are not part of
            #  the mainland zone lists

            for curId in hazardAreaDict[hazardKey]["ZONES"]:

                #  If this is a Cuba or Hispaniola code
                if curId[:3] in segmentDict.keys():

##                    print "Found zone -> curId =", curId
                    #  Add this identifier to the dictionary
                    tempList = segmentDict[curId[:3]]

                    #  If this identifier is not already in the list
                    if curId not in tempList:
                        tempList.append(curId)

                    #  Store the updated list
                    segmentDict[curId[:3]] = tempList

                #  Otherwise,
                elif curId not in segmentList:

                    zoneList.append(curId)

            #-------------------------------------------------------------------
            #  Make a separate segment for Cuba

            if len(segmentDict["CUC"]) > 0:

                sortedZones = segmentDict["CUC"]
                sortedZones.sort()

                record = ([],                           #  Segments
                          [],                           #  UGC zones
                          sortedZones,                  #  Zones
                          [],                           #  Islands
                          [])                           #  Water

                #  If we do not already have a record for these particular zones
                if record not in finalSegmentList:

                    print "\n\nCuba record", record

                    #  Add it now
                    finalSegmentList.append(record)

            #-------------------------------------------------------------------
            #  Make a separate segment for Hispaniola

            if len(segmentDict["HTC"]) > 0 or len(segmentDict["DOC"]) > 0:

                #  Get a compined list of zones
                combinedZones = segmentDict["HTC"] + segmentDict["DOC"]

                #  Sort these zones
                combinedZones.sort(self._sortBreakpoints)

                record = ([],                           #  Segments
                          [],                           #  UGC zones
                          combinedZones,                #  Zones
                          [],                           #  Islands
                          [])                           #  Water

                #  If we do not already have a record for these particular zones
                if record not in finalSegmentList:

                    print "\n\nHispaniola record", record

                    #  Add it now
                    finalSegmentList.append(record)

##            #-------------------------------------------------------------------
##            #  Make a separate segment for the United States
##
##            if len(segmentDict["USC"]) > 0:
##
##                sortedZones = segmentDict["USC"]
##                sortedZones.sort()
##
##                record = ([],                           #  Segments
##                          [],                           #  UGC zones
##                          sortedZones,                  #  Zones
##                          [],                           #  Islands
##                          [])                           #  Water
##
##                #  If we do not already have a record for these particular zones
##                if record not in finalSegmentList:
##
##                    print "\n\nCuba record", record
##
##                    #  Add it now
##                    finalSegmentList.append(record)

            #-------------------------------------------------------------------
            #  If we have filtered segments or zones

            if len(segmentList) != 0 and \
               len(segmentList) != len(hazardAreaDict[hazardKey]["SEGMENTS"]):

                #  Update the record for this segment
                curSegments = segmentList

            #  Otherwise, keep segments we already have
            else:
                curSegments = hazardAreaDict[hazardKey]["SEGMENTS"]

            if len(zoneList) != 0 and \
               len(zoneList) != len(hazardAreaDict[hazardKey]["ZONES"]):

                #  Update the record for this segment
                curZones = zoneList

            #  Otherwise, keep segments we already have
            else:
                curZones = hazardAreaDict[hazardKey]["ZONES"]


            #-------------------------------------------------------------------
            #  Create a new segment for each island which was found - if we
            #  need to

            islandList = []

            #  USA TCV
            if UStcv:

                #  Make a new record for this island
                record = ([],                                     #  Segments
                          [hazardAreaDict[hazardKey]["ISLANDS"]], #  UGC zones
                          [],                                     #  Zones
                          [hazardAreaDict[hazardKey]["ISLANDS"]], #  Islands
                          [])                                     #  Water

                #  If we do not already have a record for this island
                if record not in finalSegmentList:

                    print "\n\nUSA Island record -> %s" % (record)

                    #  Add it now
                    finalSegmentList.append(record)

            # International TCV
            else:

                for island in hazardAreaDict[hazardKey]["ISLANDS"]:

                    #  Make a new record for this island
                    record = ([],                           #  Segments
                              [],                           #  UGC zones
                              [island],                     #  Zones
                              [island],                     #  Islands
                              [])                           #  Water

                    #  If we do not already have a record for this island
                    if record not in finalSegmentList:

#                         print "\n\nIntl Island record -> %s" % (record)

                        #  Add it now
                        finalSegmentList.append(record)

            #===================================================================
            #  Make a record to group all the various areas associated with a
            #  particular hazard

            record = (curSegments,
                      hazardAreaDict[hazardKey]["UGCZONES"],
                      curZones,
                      hazardAreaDict[hazardKey]["ISLANDS"],
                      hazardAreaDict[hazardKey]["WATER"])

            #  If we do not already have a record for these particular zones
            if record not in finalSegmentList:

                #  Add it now
                finalSegmentList.append(record)

        print "\n\n", "*"*90
        print "finalSegmentList =", pp.pformat(finalSegmentList)

        #  Return the final organized list
        return finalSegmentList


    #===========================================================================
    #  Define a method to find missing zone codes for breakpoint segments,
    #  islands and water.

    def _findZoneCodes(self, areaList):

        #  Get ready to find the zone codes
        ugcZones = []
        zones = []

        #  Look at each area in the list
        for areaId in areaList:

            #  If there is an entry in the TropicalAreaDictionary for this area
            if self._tropicalAreaDict.has_key(areaId):

                #  Get the 'ugcCode' for this area
                ugcCode = self._tropicalAreaDict[areaId]["ugcCode"]

                #  If this is a generic zone code
                if len(ugcCode) > 3 and ugcCode[2] == "C":

                    #  If we do not already have it in the zone list
                    if ugcCode not in zones:

                        #  Add it now
                        zones.append(ugcCode)

                #  Otherwise, this must be a UGC code
                else:

                    #  See if we need to expand it
                    if len(ugcCode) > 7:

                        #  Expand the UGC code into individual zones
                        expandUgcList = self.expandComplexUgc(ugcCode)

                        #  Add each UGC code to the list - if not already there
                        for ugc in expandUgcList:

                            if ugc not in ugcZones:
                                ugcZones.append(ugc)

                    #  Otherwise, just add this UGC zone if it is not already there
                    elif ugcCode not in ugcZones:
                        ugcZones.append(ugc)

        #  Sort these lists as needed
        if len(ugcZones) > 1:
            ugcZones.sort(self._sortBreakpoints)

        if len(zones) > 1:
            zones.sort(self._sortBreakpoints)
            
        if len(ugcZones) == 0:
            return "There are no areas in this TCV"

        #  Return the zones we found
        return (ugcZones, zones)


#===============================================================================
#  Code to process StormInfo files -

    # same as HLSTCV_Common
    def _synchronizeAdvisories(self):
        # Retrieving a directory causes synching to occur.
        # This code can throw an exception but don't catch it
        # so that forecasters can be made aware of the issue.
        file = LocalizationSupport.getLocalizationFile(
                                    LocalizationSupport.CAVE_STATIC,
                                    LocalizationSupport.SITE, self._site,
                                    self._getAdvisoryPath()).getFile()

        return file

    # same as HLSTCV_Common
    def _getLocalAdvisoryDirectoryPath(self):
        file = self._synchronizeAdvisories()
        path = file.getPath()
        print "\n\nLooking for JSON files in '%s'" % (path)

        try:
             os.makedirs(path)
        except OSError as exception:
            if exception.errno != errno.EEXIST:
                raise

        return path

    def _getStormAdvisoryNames(self):
        advisoryDirectoryPath = self._getLocalAdvisoryDirectoryPath()
        filenames = os.listdir(advisoryDirectoryPath)
        allAdvisories = filter(lambda filename: filename[-5:] == ".json",
                               filenames)

        stormAdvisories = filter(lambda filename: filename[:2] == "AT",
                                 allAdvisories)

        return stormAdvisories

    def _loadAdvisory(self, advisoryName):
        self._synchronizeAdvisories()
        fileName = self._getAdvisoryFilename(advisoryName)

        try:
            pythonDict = JsonSupport.loadFromJson(LocalizationSupport.CAVE_STATIC,
                                             self._site, fileName)

            statFileName = os.path.join(os.environ["HOME"], "caveData", "etc",
                                        "site", self._site, fileName)
            lastModified = os.stat(statFileName).st_mtime
            pythonDict["lastModified"] = lastModified

            print "File contents for %s:" % (fileName)
            print pp.pformat(pythonDict)

            return pythonDict

        except Exception, e:
            print "Load Exception for %s : %s" % (fileName, e)
            return None

    def _saveAdvisory(self, advisoryName, advisoryDict):
        self._synchronizeAdvisories()
        fileName = self._getAdvisoryFilename(advisoryName)

        print "Saving %s to %s" % (advisoryName, fileName)
        print "advisoryDict: %s" % (pp.pformat(advisoryDict))

        try:
            JsonSupport.saveToJson(LocalizationSupport.CAVE_STATIC,
                                   self._site, fileName, advisoryDict)
#             os.system('chmod 664 %s' % (fileName))
        except Exception as e:
            print "Save Exception for %s : %s" % (fileName, e)
        else: # No exceptions occurred
            print "Wrote file contents for: %s" % (fileName)

            # Purposely allow this to throw
            self._synchronizeAdvisories()

    def _deleteAdvisory(self):

        #  Sync the CAVE localization store
        self._synchronizeAdvisories()
        fileName = self._getAdvisoryFilename(self._advisoryFileName)

        print "\n\nDeleting -> " + fileName

        try:
            LocalizationSupport.deleteFile(LocalizationSupport.CAVE_STATIC,
                                           LocalizationSupport.SITE, self._site,
                                           fileName)

        except Exception, e:
            print "Delete Exception for %s : %s" % (fileName, e)
            return None


    # same as HLSTCV_Common
    def _getAdvisoryPath(self):
        dataMgr = self._argDict["dataMgr"]
        gfeMode = dataMgr.getOpMode().name()

        if gfeMode == "PRACTICE":
            return os.path.join("gfe", "tcvAdvisories", "practice")
        else:
            return os.path.join("gfe", "tcvAdvisories")


    def _getAdvisoryFilename(self, advisoryName):
        advisoryFilename = os.path.join(self._getAdvisoryPath(), advisoryName)

        if not advisoryFilename.endswith(".json"):
            advisoryFilename += ".json"
        
        return advisoryFilename
