##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CityDictionary
#   Example CityDictionary file
#
# Author:
# ----------------------------------------------------------------------------

# Format:
# CityDictionary = {
#    "editArea" : [<list of (city edit area, city label) in the editArea>],
#  ...
#   }
#
# If there are no cities within an area, enter an empty list:
#
#    "editArea:" [],
#

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

CityDictionary = {
    "area1" : [("city1", "City 1"), ("city2", "City 2")],
    "area2" : [("city3", "City 3")],
    "area3" : [("city4", "City 4")],
   }
