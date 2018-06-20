#!/awips2/python/bin/python2

##
# DR 6346 - This script will compare the site overrides for AreaDictionary.py
# and CityLocation.py to the CONFIGURED level versions and create incremental
# overrides of these files.
##

import logging
import glob
import imp
import os.path
import pprint
import re
import shutil


logging.basicConfig(format='%(asctime)-15s %(levelname)s:  %(message)s', 
                    datefmt='%Y-%m-%d %H:%M:%S',
                    level=logging.DEBUG)
log = logging.getLogger("createIncrementalAreaDictionaryAndCityLocation.py")

TEXT_UTILS_BASE_PATH = "/awips2/edex/data/utility/cave_static/site/*/gfe/userPython/textUtilities/regular/"
AREA_DICT_GLOB_PATH = os.path.join(TEXT_UTILS_BASE_PATH, "AreaDictionary.py")
CITY_LOC_GLOB_PATH = os.path.join(TEXT_UTILS_BASE_PATH, "CityLocation.py")

AREA_DICT_HEADER = """
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

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
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

from DefaultAreaDictionary import AreaDictionary


"""
CITY_LOCATION_HEADER = """ 
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

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
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

from DefaultCityLocation import CityLocation


"""


def create_incremental_area_dictionary():
    for file in glob.iglob(AREA_DICT_GLOB_PATH):
        log.info("Generating incremental override file [%s]...", file)
        
        base_file = file.replace("site", "configured", 1)
        if not os.path.isfile(base_file):
            log.error("Could not find CONFIGURED level file [%s].", base_file)
            log.error("Skipping to next file.")
        
        log.debug("Using configured file [%s]...", base_file)
        log.debug("Using site file [%s]...", file)
        
        configured_module = imp.load_source('base', base_file)
        site_module = imp.load_source('override', file)    
        configured_dict = configured_module.AreaDictionary
        site_dict = site_module.AreaDictionary
        
        diffs = diff_dicts(configured_dict, site_dict)
        log.debug("AreaDictionary Differences: %r", diffs)
        write_override_file(file, 'AreaDictionary', diffs, AREA_DICT_HEADER)

def create_incremental_city_location():
    for file in glob.iglob(CITY_LOC_GLOB_PATH):
        log.info("Generating incremental override file [%s]...", file)
        
        base_file = file.replace("site", "configured", 1)
        if not os.path.isfile(base_file):
            log.error("Could not find CONFIGURED level file [%s].", base_file)
            log.error("Skipping to next file..")
        
        log.debug("Using configured file [%s]...", base_file)
        log.debug("Using site file [%s]...", file)
        
        configured_module = imp.load_source('base', base_file)
        site_module = imp.load_source('override', file)    
        configured_dict = configured_module.CityLocation
        site_dict = site_module.CityLocation
        
        diffs = diff_dicts(configured_dict, site_dict)
        log.debug("CityLocation Differences: %r", diffs)
        write_override_file(file, 'CityLocation', diffs, CITY_LOCATION_HEADER)

def diff_dicts(base, override, level=0):
    differences = {}
    
    keys = set().union(base.keys(), override.keys())
    # log.debug("Combined keys: %s", keys)
    
    for key in sorted(keys):
        if key not in base:
            log.debug("Key [%s] in override, but not base.", key)
            differences[key] = override[key].copy()
        elif key not in override:
            log.debug("Key [%s] in base, but not override.", key)
        else:
            if level != 1:
                sub_diffs = diff_dicts(base[key], override[key], level+1)
                if sub_diffs:
                    log.debug("Differences for key [%s]: %r", key, sub_diffs)
                    differences[key] = sub_diffs
            else:
                if base[key] != override[key]:
                    differences[key] = override[key]
    
    return differences

def write_override_file(file, object_name, object_value, header):
    backup_file = file + ".bak.dr_6346"
    log.info("Writing backup file [%s]", backup_file)
    try:
        shutil.copy(file, backup_file)
    except:
        log.exception("Unable to write backup file [%s]", backup_file)
        log.error("Skipping file [%s]", file)
        return
    
    log.info("Writing override file [%s]", file)
    try:
        with open(file, 'w') as out_file:
            printer = pprint.PrettyPrinter()
            
            out_file.write(header)
            for key in sorted(object_value.keys()):
                for sub_key in sorted(object_value[key].keys()):
                    out_file.write("{}[{}][{}] = {}".format(object_name, key, sub_key, printer.pformat(object_value[key][sub_key])))
                    out_file.write('\n')
                out_file.write('\n')
    except:
        log.exception("Unable to write incremental override file [%s]", file)
        log.critical("Restore backup file [%s] to [%s] before restarting EDEX.", backup_file, file)

def main():
    log.info("Starting delta script for DR #6346: creating incremental overrides for AreaDictionary.py and CityLocation.py...")
    
    create_incremental_area_dictionary()
    
    create_incremental_city_location()
    
    log.info("Delta script complete.")



if __name__ == '__main__':
    main()
