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
#
#  Creates area dictionary specific to a site.  Somewhat ported from AWIPS-I.
#  
#    
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Jan 08, 2010  1209     randerso  Initial Creation.
# Oct 19, 2012  1091     dgilling  Support localMaps.py.
# Oct 20, 2014  3685     randerso  Converted text to mixed case
#                                  Fixed mapDict to keep zones from different 
#                                  maps separate
# Dec 05, 2014  4953     randerso  Changed to use LocalizationSupport
# Mar 10, 2015  4129     randerso  Fixed error logging
# Jul 01, 2016  18114    ryu       Changed timezone designation from 'MST7' to
#                                  'US/Arizona'.
# Jul 15, 2016  5749     randerso  Changed preformatted ugcCityString to
#                                  ugcCities list
#                                  Added wfo field to support generation of
#                                  national TCV
# Sep 28, 2016  19293    randerso  Added exception handling for
#                                   createTCVAreaDictionary
# Nov 21, 2016  5959     njensen   Remove unused imports and made more
#                                  pythonic
# Nov 18, 2020  8287     randerso  Fix logging when TCV_AreaDictionary is not
#                                  present.
##

##
# This is a base file that is not intended to be overridden.
##



import os, copy
import tempfile, stat
import LogStream, pprint

from fips2cities import *
from zones2cities import *

import LocalizationSupport


CityLocationDict = {}

# obtain real time zone string from shapefile TIME_ZONE variable
def getRealTimeZone(tzstring):
    d = {'M':"MST7MDT",'m':"US/Arizona",'V':'America/Puerto_Rico',
         'E':"EST5EDT",'e':"EST5",
         'C':"CST6CDT",'P':"PST8PDT",'A':"America/Anchorage",
         'H':"Pacific/Honolulu",'G':"Pacific/Guam", 
         'J':"Pacific/Palu", 'K': "Pacific/Wake", 'F': "Pacific/Ponape"}
    tzones = []
    for tz in tzstring:
        if tz in d:
            tzones.append(d[tz])

    if len(tzones) > 1:
        return tzones
    elif len(tzones) == 1:
        return tzones[0]
    else:
        LogStream.logProblem("No time zone information decodable: ", tzstring)
        return None
     
# Creates the city list part of the area dictionary, based on population
def makeCityList(dictRecord):
    if "cities" in dictRecord:
        cities = copy.deepcopy(dictRecord["cities"])
        if len(cities) == 0:
            return None
        cities.sort(key=lambda c: c[1] if c[1] is not None else float('-inf'), reverse=True)
        locs = {}
        cityList = []
        count = 0
        maxPop = cities[0][1] #population of largest city
        for x in range(len(cities)):

            #limit small cities to 25% of the large city population
            if maxPop is not None and cities[x][1] is not None and \
              cities[x][1] * 4 < maxPop:
                break
            elif maxPop is not None and cities[x][1] is None:
                break

            cityList.append(cities[x][0])

            # save data to cifyLocation dictionary
            locs[cities[x][0]] = tuple(map(float, cities[x][2:4]))

            #max of 6 cities in the list
            count = count + 1
            if count > 6:
                break

        return cityList, locs

# handle marine states
def checkMarineState(ugcCode):        
    #returns None if unknown, description if known
    areas = {
      'AM': 'Atlantic coastal waters',
      'GM': 'Gulf of Mexico',
      'LE': 'Lake Erie',
      'LO': 'Lake Ontario', 
      'LH': 'Lake Huron',
      'SC': 'Lake St Clair', 
      'LM': 'Lake Michigan',
      'LS': 'Lake Superior',
      'PZ': 'Pacific coastal waters', 
      'PK': 'Alaskan coastal waters',
      'PH': 'Hawaiian coastal waters', 
      'PM': 'Marianas waters',
      'AN': 'Atlantic coastal waters', 
      'PS': 'American Samoa coastal waters',
      'SL': 'St Lawrence River',
    }
    area = ugcCode[0:2]
    return areas.get(area, None)
    
        
# Utility to create the area dictionary, based on the map background data
def createAreaDictionary(outputDir, mapDict):
    LogStream.logEvent("Generating AreaDictionary")
    areadict = {}
    mapIter = mapDict.entrySet().iterator()
    while mapIter.hasNext():
        mapEntry = next(mapIter)
        attList = mapEntry.getValue()
        attIter = attList.iterator()
        while attIter.hasNext():
            att = next(attIter)
            ean = str(att.get("editarea"))
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
                        d['ugcName'] = str(att.get('countyname')).strip()
                    else:
                        continue
    
                    if att.containsKey('state'):
                        d["stateAbbr"] = str(att.get('state'))
    
                    if att.containsKey('name'):
                        d["ugcName"] = str(att.get('name')).strip()
    
                    if att.containsKey('time_zone'):
                        tzvalue = getRealTimeZone(str(att.get('time_zone')))
                        if tzvalue is not None:
                            d["ugcTimeZone"] = tzvalue
    
                    if d['ugcCode'] in zonedata:
                        cityDict = zonedata[d['ugcCode']]
                    elif d['ugcCode'] in fipsdata:
                        cityDict = fipsdata[d['ugcCode']]
                    else:
                        cityDict = None
    
                    if cityDict:
                        cityList = makeCityList(cityDict)
                        if cityList is not None:
                            cityList, locs = cityList
                            if len(cityList): 
                                d["ugcCities"] = cityList
                                CityLocationDict[ean] = locs
    
                    # partOfState codes
                    if d['ugcCode'] in zonedata:
                        if 'partOfState' in zonedata[d['ugcCode']]:
                            d["partOfState"] = \
                              zonedata[d['ugcCode']]['partOfState']
                    elif d['ugcCode'] in fipsdata:
                        if 'partOfState' in fipsdata[d['ugcCode']]:
                            d["partOfState"] = \
                              fipsdata[d['ugcCode']]['partOfState']
                          
                    # wfo
                    if d['ugcCode'] in zonedata:
                        if 'wfo' in zonedata[d['ugcCode']]:
                            d["wfo"] = \
                              zonedata[d['ugcCode']]['wfo']
                    elif d['ugcCode'] in fipsdata:
                        if 'wfo' in fipsdata[d['ugcCode']]:
                            d["wfo"] = \
                              fipsdata[d['ugcCode']]['wfo']
                          
                    # full state name
                    if d['ugcCode'] in zonedata:
                        if 'fullStateName' in zonedata[d['ugcCode']]:
                            d["fullStateName"] = \
                              zonedata[d['ugcCode']]['fullStateName']
                    elif d['ugcCode'] in fipsdata:
                        if 'fullStateName' in fipsdata[d['ugcCode']]:
                            d["fullStateName"] = \
                              fipsdata[d['ugcCode']]['fullStateName']
                    else: 
                        marineState = checkMarineState(d['ugcCode'])
                        if marineState is not None:
                            d['fullStateName'] = marineState
                    
                          
                    if ean in areadict and d != areadict[ean]:
                        LogStream.logProblem("Mismatch of definitions in " +\
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
# DefaultAreaDictionary
#   AreaDictionary file
#
# Author: GFE Installation Script
# ----------------------------------------------------------------------------

##
# This is a base file that is not intended to be overridden.
##

# Format:
# AreaDictionary = {
#    "editArea" : {
#             "ugcCode": "STZxxx",
#             "ugcName": "EditAreaName",
#             "ugcCities": ['City1', 'City2'],
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
# ugcCities: This field contains the list of cities for hazard and routine 
#          products.  
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
# wfo: The wfo(s) with responsibility for the area
#
#  


AreaDictionary = \
"""
    pp = pprint.PrettyPrinter()
    s = s + pp.pformat(areadict)

    if not os.path.isdir(outputDir):
        os.makedirs(outputDir)

    outName = os.path.join(outputDir, "DefaultAreaDictionary.py")
    
    try:
        with tempfile.NamedTemporaryFile('w', suffix='.py', dir=outputDir, delete=False) as fh:
            fpath = fh.name
            fh.write(s)
        
        os.chmod(fpath, stat.S_IRUSR | stat.S_IWUSR |
                        stat.S_IRGRP | stat.S_IWGRP | 
                        stat.S_IROTH)
        os.rename(fpath, outName)
    except:
        LogStream.logProblem("Error writing area dictionary", LogStream.exc())

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
#             "Local evacuation and sheltering: Miami-Dade County Emergency Management",
#             [
#                 "http://www.miamidade.gov/emergency/",
#             ],
#             "Family emergency plans: Federal Emergency Management Agency",
#             [
#                 "http://ready.gov/",
#             ],
#             "Local weather conditions and forecasts: NWS Miami Florida",
#             [
#                 "http://www.srh.noaa.gov/mfl/",
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
        with open(outputDir + "/TCVAreaDictionary.py", "rb") as existingFile:
            contents = existingFile.read()
            namespace = {}
            exec(contents, namespace)
        
        # TCV_AreaDictionary comes from the existing TCVAreaDictionary when it is exec'ed
        existingTCVAreaDictionary = namespace["TCV_AreaDictionary"]
    except FileNotFoundError:
        LogStream.logVerbose("No TCV_AreaDictionary found in file [{}/TCVAreaDictionary.py]".format(outputDir))
    except Exception:
        LogStream.logProblem("Failed to read TCVAreaDictionary.py", LogStream.exc())
    
    for zone in _getZones(siteID):
        tcvAreaDictionaryContents += "    '" + zone + "': {\n"
        
        # Don't clobber existing dictionary entries
        if zone in existingTCVAreaDictionary:
            # Add new entries
            for key in zoneSkeletonContents:
                if key not in existingTCVAreaDictionary[zone]:
                    existingTCVAreaDictionary[zone][key] = zoneSkeletonContents[key]
            
            # Remove entries that are no longer needed
            existingKeys = list(existingTCVAreaDictionary[zone].keys())
            for key in existingKeys:
                if key not in zoneSkeletonContents:
                    existingTCVAreaDictionary[zone].pop(key)
            
            tcvAreaDictionaryContents += _formatDictionary(existingTCVAreaDictionary[zone], tabLevel = 2)
        else:
            tcvAreaDictionaryContents += _formatDictionary(zoneSkeletonContents, tabLevel = 2)
        
        tcvAreaDictionaryContents += "    },\n\n"
    
    tcvAreaDictionaryContents += "}\n"
    
    with open(outputDir + "/TCVAreaDictionary.py", "w") as f:
        f.write(tcvAreaDictionaryContents)
    
def _getZones(siteID):
    editAreasFilename = "gfe/combinations/EditAreas_PublicZones_" + \
                        siteID + ".py"
    zonesKey = "Zones_" + siteID
    
    try:
        editAreasFileContents = LocalizationSupport.readFile(LocalizationSupport.CAVE_STATIC,
                                                             LocalizationSupport.CONFIGURED,
                                                             siteID,
                                                             editAreasFilename)
        namespace = {}
        exec(editAreasFileContents, namespace)
        
        # EASourceMap comes from the EditAreas file
        return namespace['EASourceMap'][zonesKey]
    except:
        LogStream.logProblem("Error getting zones for %s: " % siteID, LogStream.exc())
        return []
    
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

    for mapname in mapDict:
        if 'Cities' not in mapname:
            continue
         
        attList = mapDict[mapname]
        for att in attList:
            #LogStream.logProblem("att:", att)
            ean = att['name']
            state = att['st']
            county_FIP = att['county_fip']

            if len(ean) and len(state) and len(county_FIP):
                fip = state + 'C' + county_FIP
                if fip not in citydict:
                    citydict[fip] = {}
                try:
                    latitude = float(str(att['lat']).strip())
                    longitude = float(str(att['lon']).strip())
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
# DefaultCityLocation
#   CityLocation file
#
# Author: GFE Installation Script 
# ----------------------------------------------------------------------------

##
# This is a base file that is not intended to be overridden.
##

# Format:
# CityLocation = {
#    "editArea": {'cityName1' : (latitude, longitude),
#                 'cityName2' : (latitude, longitude),
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

    outName = os.path.join(outputDir, "DefaultCityLocation.py")
    
    try:
        with tempfile.NamedTemporaryFile('w', suffix='.py', dir=outputDir, delete=False) as fh:
            fpath = fh.name
            fh.write(s)
        
        os.chmod(fpath, stat.S_IRUSR | stat.S_IWUSR |
                        stat.S_IRGRP | stat.S_IWGRP | 
                        stat.S_IROTH)
        os.rename(fpath, outName)
    except:
        LogStream.logProblem("Error writing city location", LogStream.exc())
