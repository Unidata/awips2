##
##

##
# This is a base file that is not intended to be overridden.
##

# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# NOW_Local
#  Produces NOW product.
#
# Author: davis
# ----------------------------------------------------------------------------

import GenericReport
import TextRules
import string, time, re, os, types, copy

class TextProduct(GenericReport.TextProduct):
    Definition = copy.deepcopy(GenericReport.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "NOW_<MultiPil>"

    Definition["outputFile"] = "{prddir}/TEXT/NOW_<MultiPil>.txt"
    Definition["database"] =  "Official"  # Source database

    Definition["debug"] = 0
   
    # Name of map background for creating Combinations
    Definition["mapNameForCombinations"] = ["Zones_<site>",
      "Marine_Zones_<site>"]
        
    ## Edit Areas: Create Combinations file with edit area combinations.
    Definition["defaultEditAreas"] = "Combinations_NOW_<site>_<MultiPil>"
    Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display
 
    # product identifiers
    Definition["productName"] = "Short Term Forecast" # product name 
    Definition["fullStationID" ] = "<fullStationID>"    # 4 letter station ID
    Definition["wmoID" ] = "<wmoID>"                    # WMO code
    Definition["wfoCityState" ] = "<wfoCityState>"      # Location of WFO
    Definition["pil" ] = "<pil>"                        # product pil
    Definition["textdbPil" ] = "<textdbPil>"   # pil: storing to AWIPS textdb
    Definition["awipsWANPil" ] = "<awipsWANPil>" # pil: transmitting to WAN.
    Definition["wfoSiteID"] = "<site>"
    Definition["areaName"] = ""  #optional area name for product

    # Area Dictionary -- Descriptive information about zones
    Definition["areaDictionary"] = "AreaDictionary" 

    # Language
    Definition["language"] = "english"
    Definition["lineLength"] = 66   #Maximum line length

    def __init__(self):
        GenericReport.TextProduct.__init__(self)

   # REQUIRED OVERRIDES
    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        return fcst + ".NOW...\n\n\n$$\n\n"

   # OPTIONAL OVERRIDES

   # PATCHES:  To be removed with each new release




