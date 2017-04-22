#!/usr/bin/env python
#
# Delta script to convert the ugcCitiesString to ugcCities list
# the wfo field is not added as it is only required for NHA and they already have it
#

import imp, glob, os, pprint, re, sys

AREA_DICTIONARY_PATH = "/awips2/edex/data/utility/cave_static/site/*/gfe/userPython/textUtilities/regular/*AreaDictionary.py"
SUFFIX = ".orig_ugcCity" 

HEADER_PATTERN = re.compile("(.*\n)\s*AreaDictionary\s*=\s*{", re.MULTILINE | re.DOTALL)

SURF_HEADER = """
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
# Example using site TBW
#
#  Additional Edit Areas that needed to be created:
#
#  landSeaAreas:
#     SRF_850, SRF_853, SRF_856
#
#  surfAreas:
#     NorthCoast1 (listed with FLZ139), SouthCoast  (along GMZ850)
#     NorthCoast2 (listed with FLZ142), SouthCoast  (along GMZ850)
#
# TBW always runs with the same combinations:
#   FLZ139-FLZ142-FLZ148-FLZ149
#   FLZ050-FLZ151-FLZ155-FLZ160
#   FLZ162-FLZ165
# Thus, additional entries need only be listed for at least one zone
# in each combination.
#
# If you want to combine zones on-the-fly, you should list additional areas
# for each zone and the system will take care to combine them appropriately.
# For example, note that the "surfAreas" are listed for both FLZ039 and FLZ042.
# When they are combined, the surfAreas are reported just once, as desired.
# ----------------------------------------------------------------------------
#
"""

STANDARD_HEADER = """
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
"""

pp = pprint.PrettyPrinter()

def writeAreaDictionary(header, areaDict, path):
    
    # if header is None choose the appropriate default header
    if header is None:
        if os.path.basename(path) == "SurfAreaDictionary.py":
            header = SURF_HEADER
        else:
            header = STANDARD_HEADER
    
    s = header + "AreaDictionary = "+ pp.pformat(areaDict)
    
    # if the backup file doesn't already exist create it
    if not os.path.exists(path + SUFFIX):
        os.rename(path, path + SUFFIX)
    
    # write out the new file
    with open(path, "w") as out:
        out.write(s)

def main():
    for path in glob.iglob(AREA_DICTIONARY_PATH):
        # only process AreaDictionary and SurfAreaDictionary
        if os.path.basename(path) not in ["AreaDictionary.py", "SurfAreaDictionary.py"]:
            print "Skipping %s..." % path
            continue
        
        print "Processing %s..." % path
        
        # attempt to read file contents and extract header
        header = None        
        try:
            with open(path, 'r') as file:
                contents = file.read()
    
            m = HEADER_PATTERN.match(contents)
            if m is not None:
                header = m.group(1)
        except:
            print "Unable to read header, default header will be used"
        
        module = imp.load_source('module', path)
        if not hasattr(module, "AreaDictionary"):
            print "No AreaDictionary found, skipping!"
            continue
        
        areaDict = module.AreaDictionary
        
        for key in areaDict:
            entry = areaDict[key]
            try:
                if "ugcCityString" in entry:
                    # remove any leading or trailing whitespace
                    cityString = entry["ugcCityString"].strip()
                    
                    # remove leading ...
                    if cityString.startswith("..."):
                        cityString = cityString[3:]
                    
                    # remove trailing ...
                    if cityString.endswith("..."):
                        cityString = cityString[:-3]
                        
                    # remove any leading or trailing whitespace again
                    cityString = cityString.strip()
                    
                    # if any cities then create the ugcCities list
                    if len(cityString) > 0:
                        cityList = cityString.split("...")
                        entry["ugcCities"] = cityList
                    
                    # remove the old ugcCityString entry
                    del entry["ugcCityString"]
            except:
                print "Bad ugcCityString in %s: '%s'" % (path, cityString)
                

        writeAreaDictionary(header, areaDict, path)
        
        # delete the module and anything in it's namespace
        for x in dir(module):
            exec "del module."+x
        del module
    
if __name__ == "__main__":
    main()
