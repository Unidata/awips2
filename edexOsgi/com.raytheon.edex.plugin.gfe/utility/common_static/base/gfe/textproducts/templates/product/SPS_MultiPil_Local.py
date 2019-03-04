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
# SPS_Local
# Produces SPS product.
#
# Author: Matt Davis
# ----------------------------------------------------------------------------

import GenericReport
import TextRules
import string, time, re, os, types, copy

class TextProduct(GenericReport.TextProduct):
    Definition = copy.deepcopy(GenericReport.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "SPS_<MultiPil>"

    Definition["outputFile"] = "{prddir}/TEXT/SPS_<MultiPil>.txt"
    Definition["database"] =  "Official"  # Source database

    Definition["debug"] = 0
   
    # Name of map background for creating Combinations
    Definition["mapNameForCombinations"] = "Zones_<site>" 
        
    ## Edit Areas: Create Combinations file with edit area combinations.
    Definition["defaultEditAreas"] = "Combinations_SPS_<site>_<MultiPil>"
    Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display
 
    #Special multiple product domains for certain sites:
    if "<site>" == "AFG":
        if "_<MultiPil>" == "_AFG":
            Definition["subDomainUGCs"] = ["AKZ218","AKZ219","AKZ220","AKZ221",
                                           "AKZ222","AKZ223","AKZ224","AKZ225",
                                           "AKZ226"]
        elif "_<MultiPil>" == "_NSB":
            Definition["subDomainUGCs"] = ["AKZ201","AKZ202","AKZ203","AKZ204",
                                           "AKZ205","AKZ206"]
        elif "_<MultiPil>" == "_WCZ":
            Definition["subDomainUGCs"] = ["AKZ207","AKZ208","AKZ209","AKZ210",
                                           "AKZ211","AKZ212","AKZ213","AKZ214",
                                           "AKZ215","AKZ216","AKZ217","AKZ227"]
                                                                                
    # product identifiers
    Definition["productName"] = "Special Weather Statement" # product name 
    Definition["fullStationID" ] = "<fullStationID>"    # 4 letter station ID
    Definition["wmoID" ] = "<wmoID>"                    # WMO code
    Definition["wfoCityState" ] = "<wfoCityState>"      # Location of WFO
    Definition["pil" ] = "<pil>"                        # product pil
    Definition["textdbPil" ] = "<textdbPil>"            # pil: storing to AWIPS textdb
    Definition["awipsWANPil" ] = "<awipsWANPil>" # pil: transmitting to WAN.
    Definition["wfoSiteID"] = "<site>"
    Definition["areaName"] = ""  #optional area name for product

    # Area Dictionary -- Descriptive information about zones
    Definition["areaDictionary"] = "AreaDictionary" 

    # Language
    Definition["language"] = "english"
    Definition["lineLength"] = 66   #Maximum line length
    
    # Expiration
 
    def __init__(self):
        GenericReport.TextProduct.__init__(self)

   # REQUIRED OVERRIDES
    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        return fcst + "|* Statement text *|\n\n$$\n\n"

   # OPTIONAL OVERRIDES

   # PATCHES:  To be removed with each new release
