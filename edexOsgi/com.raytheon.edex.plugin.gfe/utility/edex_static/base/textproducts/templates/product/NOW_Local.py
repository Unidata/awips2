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
    Definition["productName"] = "SHORT TERM FORECAST" # product name 
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




