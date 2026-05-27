#!/awips2/python/bin/python2

##
# DR 6346 - This script will compare the site overrides for AreaDictionary.py
# and CityLocation.py to the CONFIGURED level versions and create incremental
# overrides of these files.
##

import copy
import logging
import glob
import imp
import os
import os.path
import pprint
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
    for site_file in glob.iglob(AREA_DICT_GLOB_PATH):
        log.info("Generating incremental override file [%s]...", site_file)
        
        base_file = site_file.replace("site", "configured", 1)
        if not os.path.isfile(base_file):
            log.error("Could not find CONFIGURED level file [%s].", base_file)
            log.error("Skipping to next file.")
            continue

        with open(site_file, 'r') as f:
            contents = f.read()
            if "from DefaultAreaDictionary import AreaDictionary" in contents:
                log.info("Site AreaDictionary file [%s] has already been upgraded.", site_file)
                continue
        
        log.info("Using configured file [%s]...", base_file)
        log.info("Using site file [%s]...", site_file)
        
        configured_module = imp.load_source('base', base_file)
        site_module = imp.load_source('override', site_file)    
        configured_dict = configured_module.AreaDictionary
        site_dict = site_module.AreaDictionary
        
        diffs = diff_dicts(configured_dict, site_dict)
        log.debug("AreaDictionary Differences: %r", diffs)
        write_override_file(site_file, 'AreaDictionary', diffs, AREA_DICT_HEADER)

        delete_files(base_file + '*')

def create_incremental_city_location():
    for site_file in glob.iglob(CITY_LOC_GLOB_PATH):
        log.info("Generating incremental override file [%s]...", site_file)
        
        base_file = site_file.replace("site", "configured", 1)
        if not os.path.isfile(base_file):
            log.error("Could not find CONFIGURED level file [%s].", base_file)
            log.error("Skipping to next file.")
            continue

        with open(site_file, 'r') as f:
            contents = f.read()
            if "from DefaultCityLocation import CityLocation" in contents:
                log.info("Site CityLocation file [%s] has already been upgraded.", site_file)
                continue
        
        log.info("Using configured file [%s]...", base_file)
        log.info("Using site file [%s]...", site_file)
        
        configured_module = imp.load_source('base', base_file)
        site_module = imp.load_source('override', site_file)    
        configured_dict = configured_module.CityLocation
        site_dict = site_module.CityLocation
        
        diffs = diff_dicts(configured_dict, site_dict)
        log.debug("CityLocation Differences: %r", diffs)
        write_override_file(site_file, 'CityLocation', diffs, CITY_LOCATION_HEADER)

        delete_files(base_file + '*')

def diff_dicts(base, override):
    differences = []
    
    keys = set().union(base.keys(), override.keys())
    # log.debug("Combined keys: %s", keys)
    
    for key in sorted(keys):
        if key not in base:
            log.debug("Key [%s] in override, but not base.", key)
            differences.append((key, copy.copy(override[key]), True))
        elif key not in override:
            log.debug("Key [%s] in base, but not override.", key)
        else:
            sub_diffs = sub_diff_dicts(base[key], override[key])
            if sub_diffs:
                log.debug("Differences for key [%s]: %r", key, sub_diffs)
                differences.append((key, sub_diffs, False))
    
    return differences

def sub_diff_dicts(base, override, level=0):
    differences = {}
    
    keys = set().union(base.keys(), override.keys())
    # log.debug("Combined keys: %s", keys)
    
    for key in sorted(keys):
        if key not in base:
            log.debug("Key [%s] in override, but not base.", key)
            differences[key] = copy.copy(override[key])
        elif key not in override:
            log.debug("Key [%s] in base, but not override.", key)
        else:
            if base[key] != override[key]:
                    differences[key] = override[key]
    
    return differences

def write_override_file(file_name, object_name, object_value, header):
    backup_file = file_name + ".bak.dr_6346"
    log.info("Writing backup file [%s]", backup_file)
    try:
        shutil.copy(file_name, backup_file)
    except:
        log.exception("Unable to write backup file [%s]", backup_file)
        log.error("Skipping file [%s]", file_name)
        return
    
    log.info("Writing override file [%s]", file_name)
    try:
        with open(file_name, 'w') as out_file:
            printer = pprint.PrettyPrinter()
            
            out_file.write(header)
            for (key, value, added) in sorted(object_value, key=lambda i: i[0]):
                if added:
                    out_file.write("{}[{!r}] = {}".format(object_name, key, printer.pformat(value)))
                    out_file.write('\n')
                else:
                    for sub_key in sorted(value.keys()):
                        out_file.write("{}[{!r}][{!r}] = {}".format(object_name, key, sub_key, printer.pformat(value[sub_key])))
                        out_file.write('\n')
                out_file.write('\n')
    except:
        log.exception("Unable to write incremental override file [%s]", file_name)
        log.critical("Restore backup file [%s] to [%s] before restarting EDEX.", backup_file, file_name)

def delete_files(file_pattern):
    for f in glob.iglob(file_pattern):
        try:
            os.remove(f)
        except:
            log.exception("Unable to delete file [%s].", f)

def main():
    log.info("Starting delta script for DR #6346: creating incremental overrides for AreaDictionary.py and CityLocation.py...")
    
    create_incremental_area_dictionary()
    
    create_incremental_city_location()
    
    log.info("Delta script complete.")



if __name__ == '__main__':
    main()
