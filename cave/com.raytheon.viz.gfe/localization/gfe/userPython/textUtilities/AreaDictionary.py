
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


