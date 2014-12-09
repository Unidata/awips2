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
#
# Create Area Dictionary 
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/05/14        4953          randerso       Changed to use LocalizationSupport
# 
##

import os, string, copy
import tempfile, stat
import LogStream, pprint

from fips2cities import *
from zones2cities import *

from LockingFile import File
import LocalizationSupport

#
#  Creates area dictionary specific to a site.  Somewhat ported from AWIPS-I.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/08/10             #1209    randerso       Initial Creation.
#    10/19/12             #1091    dgilling       Support localMaps.py.
#    
# 
#

CityLocationDict = {}

# obtain real time zone string from shapefile TIME_ZONE variable
def getRealTimeZone(tzstring):
    d = {'M':"MST7MDT",'m':"MST7",'V':'America/Puerto_Rico',
         'E':"EST5EDT",'e':"EST5",
         'C':"CST6CDT",'P':"PST8PDT",'A':"America/Anchorage",
         'H':"Pacific/Honolulu",'G':"Pacific/Guam", 
         'J':"Pacific/Palu", 'K': "Pacific/Wake", 'F': "Pacific/Ponape"}
    tzones = []
    for tz in tzstring:
        if d.has_key(tz):
            tzones.append(d[tz])

    if len(tzones) > 1:
        return tzones
    elif len(tzones) == 1:
        return tzones[0]
    else:
        LogStream.logProblem("No time zone information decodable: ", tzstring)
        return None

# sorts the cities by decending population
def citysort(c1, c2):
    if c1[1] is None and c2[1] is None:
        return 0
    elif c1[1] is None:
        return 1
    elif c2[1] is None:
        return -1
    elif c1[1] < c2[1]:
        return 1
    elif c1[1] > c2[1]:
        return -1
    else:
        return 0
     
# Creates the city string part of the area dictionary, based on population
def makeCityString(dictRecord):
    if dictRecord.has_key("cities"):
        cities = copy.deepcopy(dictRecord["cities"])
        if len(cities) == 0:
            return None
        cities.sort(citysort)
        locs = {}
        s = ""
        count = 0
        maxPop = cities[0][1] #population of largest city
        for x in xrange(len(cities)):

            #limit small cities to 25% of the large city population
            if maxPop is not None and cities[x][1] is not None and \
              cities[x][1] * 4 < maxPop:
                break
            elif maxPop is not None and cities[x][1] is None:
                break

            s = s + "..." + cities[x][0]

            # save data to cifyLocation dictionary
            locs[cities[x][0]] = tuple(map(float, cities[x][2:4]))

            #max of 6 cities in the list
            count = count + 1
            if count > 6:
                break

        return s, locs

# handle marine states
def checkMarineState(ugcCode):        
    #returns None if unknown, description if known
    areas = {'AM': 'ATLANTIC COASTAL WATERS', 'GM': 'GULF OF MEXICO',
      'LE': 'LAKE ERIE', 'LO': 'LAKE ONTARIO', 'LH': 'LAKE HURON',
      'SC': 'LAKE ST CLAIR', 'LM': 'LAKE MICHIGAN', 'LS': 'LAKE SUPERIOR',
      'PZ': 'PACIFIC COASTAL WATERS', 'PK': 'ALASKAN COASTAL WATERS',
      'PH': 'HAWAIIAN COASTAL WATERS', 'PM': 'MARIANAS WATERS',
      'AN': 'ATLANTIC COASTAL WATERS', 'PS': 'AMERICAN SAMOA COASTAL WATERS',
      'SL': 'ST LAWRENCE RIVER'}
    area = ugcCode[0:2]
    return areas.get(area, None)
    
        
# Utility to create the area dictionary, based on the map background data
def createAreaDictionary(outputDir, mapDict):
    LogStream.logEvent("Generating AreaDictionary")
    areadict = {}
    mapIter = mapDict.entrySet().iterator()
    while mapIter.hasNext():
        entry = mapIter.next() 
        ean = str(entry.getKey())
        att = entry.getValue()
        if len(ean):
            try:
                d = {}
                if att.containsKey('zone') and att.containsKey('state'):
                    d['ugcCode'] = str(att.get('state')) + "Z" + str(att.get('zone'))
                elif att.containsKey('id'):
                    d['ugcCode'] = str(att.get('id'))
                elif att.containsKey('fips') and att.containsKey('state') and \
                  att.containsKey('countyname'):
                    d['ugcCode'] = str(att.get('state')) + "C" + str(att.get('fips'))[-3:]
                    d['ugcName'] = string.strip(str(att.get('countyname')))
                else:
                    continue

                if att.containsKey('state'):
                    d["stateAbbr"] = str(att.get('state'))

                if att.containsKey('name'):
                    d["ugcName"] = string.strip(str(att.get('name')))

                if att.containsKey('time_zone'):
                    tzvalue = getRealTimeZone(str(att.get('time_zone')))
                    if tzvalue is not None:
                        d["ugcTimeZone"] = tzvalue

                if zonedata.has_key(d['ugcCode']):
                    cityDict = zonedata[d['ugcCode']]
                elif fipsdata.has_key(d['ugcCode']):
                    cityDict = fipsdata[d['ugcCode']]
                else:
                    cityDict = None

                if cityDict:
                    cityString = makeCityString(cityDict)
                    if cityString is not None:
                        cityString, locs = cityString
                        if len(cityString): 
                            d["ugcCityString"] = cityString
                            CityLocationDict[ean] = locs

                # partOfState codes
                if zonedata.has_key(d['ugcCode']):
                    if zonedata[d['ugcCode']].has_key('partOfState'):
                        d["partOfState"] = \
                          zonedata[d['ugcCode']]['partOfState']
                elif fipsdata.has_key(d['ugcCode']):
                    if fipsdata[d['ugcCode']].has_key('partOfState'):
                        d["partOfState"] = \
                          fipsdata[d['ugcCode']]['partOfState']
                      
                # full state name
                if zonedata.has_key(d['ugcCode']):
                    if zonedata[d['ugcCode']].has_key('fullStateName'):
                        d["fullStateName"] = \
                          zonedata[d['ugcCode']]['fullStateName']
                elif fipsdata.has_key(d['ugcCode']):
                    if fipsdata[d['ugcCode']].has_key('fullStateName'):
                        d["fullStateName"] = \
                          fipsdata[d['ugcCode']]['fullStateName']
                else: 
                    marineState = checkMarineState(d['ugcCode'])
                    if marineState is not None:
                        d['fullStateName'] = marineState
                
                      
                if areadict.has_key(ean) and d != areadict[ean]:
                    LogStream.logDiag("Mismatch of definitions in " +\
                      "AreaDictionary creation. EditAreaName=",  ean,
                      "AreaDict=\n", areadict[ean], "\nIgnored=\n", d)
                else:
                    areadict[ean] = d
            except:
                LogStream.logProblem("Problem with ", ean, LogStream.exc())

    s = """
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# AreaDictionary
#   AreaDictionary file
#
# Author: GFE Installation Script
# ----------------------------------------------------------------------------

# Format:
# AreaDictionary = {
#    "editArea" : {
#             "ugcCode": "STZxxx",
#             "ugcName": "EditAreaName",
#             "ugcCityString": "...CITY1...CITY2",
#             "ugcTimeZone": "MST7MDT",
#             "fullStateName": "COLORADO",
#             "partOfState": "NORTHEAST",
#             "stateAbbr": "CO",
#             "independentCity": 0,
#             "locationName": "GeneralAreaName",
#             }
#  ...
#   }
#  ugcTimeZone:  This field should be replace with the correct time zone
#                for that zone.  If it is different from the time zone of
#                the local WFO, it's time zone will appear in the header of
#                some products in parentheses.  
#                Using any other strings to define
#                the time zone may produce undesirable results.
#                The time zone may also be a list of time zones in case
#                a forecast zone happens to cover an area that contains
#                two time zones.
#                e.g.   "ugcTimeZone" : ["MST7MDT", "PST8PDT"]
#
# ugcCode: This field contains the ugc coding for this area, such as COZ023
#
# ugcName: This field contains the descriptive name for this area.  It
#          is used in various products, including Hazard products.  This is
#          the official county or zone name.
#
# locationName: This field is optional, but provides an alternate name that
#      is used in the text of some products to describe the area.  The
#      FFA product uses this value if available.
#
# ugcCityString: This field contains the list of cities as a string for 
#          hazard and routine products.  
#
# fullStateName: This field is used in hazard products to fully describe
#          the state in which this edit area resides.
#
# partOfState: This field describes the location within a state (such as
#         NORTHEAST) for this area. It is used in hazard products.
#
# stateAbbr:  State Abbreviation for the fullStateName.
#
# independentCity:  Set to 0 or 1.  Some counties (FIPS coding) are actually
#        cities.  Setting the flag to 1 will instruct those formatters
#        to mention independent cities, rather than include this "county"
#        in the county list of the product.

#  


AreaDictionary = \
"""
    pp = pprint.PrettyPrinter()
    s = s + pp.pformat(areadict)

    if not os.path.isdir(outputDir):
        os.makedirs(outputDir)

    outName = os.path.join(outputDir, "AreaDictionary.py")
    
    fh = None
    try:
        fh, fpath = tempfile.mkstemp(dir=outputDir, suffix=".py")
        os.write(fh, s)
        os.chmod(fpath, stat.S_IRUSR | stat.S_IWUSR |
                        stat.S_IRGRP | stat.S_IWGRP | 
                        stat.S_IROTH)
        os.close(fh)
        fh = None
        os.rename(fpath, outName)
    except:
        LogStream.logProblem("Error writing area dictionary", LogStream.exc())
    finally:
        if fh is not None:
            os.close(fh)


def createTCVAreaDictionary(outputDir, mapDict, siteID):
    tcvAreaDictionaryContents = \
"""
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TCV_AreaDictionary
#   TCV_AreaDictionary file
#
# Author: GFE Installation Script
# ----------------------------------------------------------------------------

# Here is an example TCVAreaDictionary for just a single zone and with comments
# to talk about the structure of the dictionary.
#
# TCV_AreaDictionary = {
#     # Zone
#     'FLZ173': {
#         # A list of location names.
#         'locationsAffected': [
#             "Miami Beach",
#             "Downtown Miami",
#         ],
#         
#         # Potential impacts statements can be overriden here; anything not
#         # overriden here will use the generic potential impacts statements
#         'potentialImpactsStatements': {
#             # Section name: "Wind", "Storm Surge", "Flooding Rain" or "Tornado"
#             "Wind": {
#                 # Threat level: "None", "Low", "Mod", "High" or "Extreme"
#                 "Extreme": [
#                     # Each string will be on its own line
#                     "Widespread power outages with some areas experiencing long-term outages",
#                     "Many bridges and access routes connecting barrier islands impassable",
#                     "Structural category to sturdy buildings with some having complete wall and roof failures",
#                     "Complete destruction of mobile homes",
#                     "Numerous roads impassable from large debris",
#                     
#                 ],
#             },
#         },
#         
#         # Additional information that will be displayed at the end of the segment
#         # The structure is a list containing strings and/or lists. Strings in the
#         # same list will be idented the same amount. Introducing a list, idents the
#         # text until it ends. For example:
#         #
#         # 'infoSection': [
#         #     "This will be at tab level 0",
#         #     [
#         #         "A new list was introduced so this is at tab level 1",
#         #         [
#         #             "Yet another list so this is tab level 2",
#         #             "Still at tab level 2 here",
#         #         ],
#         #         "We are back at tab level 1 because we ended the list",
#         #     ],
#         #     "We ended the other list and are back at tab level 0 now",
#         # ]
#         'infoSection': [
#             "LOCAL EVACUATION AND SHELTERING: MIAMI-DADE COUNTY EMERGENCY MANAGEMENT",
#             [
#                 "HTTP://WWW.MIAMIDADE.GOV/EMERGENCY/",
#             ],
#             "FAMILY EMERGENCY PLANS: FEDERAL EMERGENCY MANAGEMENT AGENCY",
#             [
#                 "HTTP://READY.GOV/",
#             ],
#             "LOCAL WEATHER CONDITIONS AND FORECASTS: NWS MIAMI FLORIDA",
#             [
#                 "HTTP://WWW.SRH.NOAA.GOV/MFL/",
#             ],
#         ],
#     },
# }

TCV_AreaDictionary = {
"""
    
    zoneSkeletonContents = {
            'locationsAffected' : [],
            'potentialImpactsStatements' : {},
            'infoSection' : [],
        }
    
    existingTCVAreaDictionary = {}
    try:
        with open(outputDir + "/TCVAreaDictionary.py", "r") as existingFile:
            contents = existingFile.read()
            exec(contents)
        
        # TCV_AreaDictionary comes from the existing TCVAreaDictionary when it is exec'ed
        existingTCVAreaDictionary = TCV_AreaDictionary
    except Exception:
        pass
    
    for zone in _getZones(siteID):
        tcvAreaDictionaryContents += "    '" + zone + "': {\n"
        
        # Don't clobber existing dictionary entries
        if zone in existingTCVAreaDictionary:
            # Add new entries
            for key in zoneSkeletonContents:
                if key not in existingTCVAreaDictionary[zone]:
                    existingTCVAreaDictionary[zone][key] = zoneSkeletonContents[key]
            
            # Remove entries that are no longer needed
            existingKeys = existingTCVAreaDictionary[zone].keys()
            for key in existingKeys:
                if key not in zoneSkeletonContents:
                    existingTCVAreaDictionary[zone].pop(key)
            
            tcvAreaDictionaryContents += _formatDictionary(existingTCVAreaDictionary[zone], tabLevel = 2)
        else:
            tcvAreaDictionaryContents += _formatDictionary(zoneSkeletonContents, tabLevel = 2)
        
        tcvAreaDictionaryContents += "    },\n\n"
    
    tcvAreaDictionaryContents += "}\n"
    
    with open(outputDir + "/TCVAreaDictionary.py", "w") as file:
        file.write(tcvAreaDictionaryContents)
    
def _getZones(siteID):
    editAreasFilename = "gfe/combinations/EditAreas_PublicZones_" + \
                        siteID + ".py"
    zonesKey = "Zones_" + siteID
    
    editAreasFileContents = LocalizationSupport.readFile(LocalizationSupport.CAVE_STATIC,
                                                         LocalizationSupport.CONFIGURED,
                                                         siteID,
                                                         editAreasFilename)
    
    
    exec(editAreasFileContents)
    
    # EASourceMap comes from the EditAreas file
    return EASourceMap[zonesKey]
    
def _formatDictionary(dictionary, tabLevel, output=""):
    TAB = " " * 4
    
    for key in dictionary:
        output += TAB*tabLevel + repr(key) + ": "
        
        value = dictionary[key]
        if type(value) is dict:
            output += "{\n"
            output = _formatDictionary(value, tabLevel+1, output)
            output += TAB*tabLevel + "},\n"
        elif type(value) is list:
            output += "[\n"
            output = _formatList(value, tabLevel+1, output)
            output += TAB*tabLevel + "],\n"
        else:
            output += repr(value) + ",\n"
    
    return output
    
def _formatList(theList, tabLevel, output=""):
    TAB = " " * 4
    
    for value in theList:
        if type(value) is dict:
            output += TAB*tabLevel + "{\n"
            output = _formatDictionary(value, tabLevel+1, output)
            output += TAB*tabLevel + "},\n"
        elif type(value) is list:
            output += TAB*tabLevel + "[\n"
            output = _formatList(value, tabLevel+1, output)
            output += TAB*tabLevel + "],\n"
        else:
            output += TAB*tabLevel + repr(value) + ",\n"
    
    return output


# Utility to create the city location dictionary
def createCityLocation(outputDir, mapDict):
    LogStream.logEvent("Generating CityLocation")

    citydict = CityLocationDict
    
    if dict != type(mapDict):
        import JUtil
        mapDict = JUtil.javaObjToPyVal(mapDict)

    for mapname in mapDict.keys():
        if mapname.find("Cities") == -1:
            continue
         
        attList = mapDict[mapname]
        for att in attList:
            #LogStream.logProblem("att:", att)
            ean = att['name']
            state = att['st']
            county_FIP = att['county_fip']

            if len(ean) and len(state) and len(county_FIP):
                fip = state + 'C' + county_FIP
                if not citydict.has_key(fip):
                    citydict[fip] = {}
                try:
                    latitude = float(string.strip(att['lat']))
                    longitude = float(string.strip(att['lon']))
                    citydict[fip][ean.upper()] = (latitude, longitude)
                except:
                    LogStream.logProblem("Problem creating city location ",
                                         ean, att, LogStream.exc())

    s = """
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CityLocation
#   CityLocation file
#
# Author: GFE Installation Script 
# ----------------------------------------------------------------------------

# Format:
# CityLocation = {
#    "editArea": {"cityName1" : (latitude, longitude),
#                 "cityName2" : (latitude, longitude),
#                 ...
#                }
#  ...
#   }
#
# editArea: name of edit area as in AreaDictionary
#
# cityName: name of the city - should be the same as in AreaDictionary.
#
# latitude/longitude: city's lat/lon location.
#  


CityLocation = \
"""
    pp = pprint.PrettyPrinter()
    s = s + pp.pformat(citydict)

    if not os.path.isdir(outputDir):
        os.makedirs(outputDir)

    outName = os.path.join(outputDir, "CityLocation.py")
    
    fh = None
    try:
        fh, fpath = tempfile.mkstemp(dir=outputDir, suffix=".py")
        os.write(fh, s)
        os.chmod(fpath, stat.S_IRUSR | stat.S_IWUSR |
                        stat.S_IRGRP | stat.S_IWGRP | 
                        stat.S_IROTH)
        os.close(fh)
        fh = None
        os.rename(fpath, outName)
    except:
        LogStream.logProblem("Error writing city location", LogStream.exc())
    finally:
        if fh is not None:
            os.close(fh)

