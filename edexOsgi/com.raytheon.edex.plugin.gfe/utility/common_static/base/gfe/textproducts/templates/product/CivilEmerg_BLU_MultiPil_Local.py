##
# This software was developed and / or modified by NOAA/NWS/OCP/ASDT
##

##
# This is a base file that is not intended to be overridden.
##

# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CivilEmerg_BLU_<MultiPil>_Local
# This product produces a Blue Alert message
#
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# Example Output:
# Refer to the NWS 10-518 Directive for further information.
#-----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#  Date         Ticket#    Engineer       Description
#  ------------ ---------- -----------    ------------------------------------
#  05/17/2019    20782     mgamazaychikov Initial creation.
#
##

import CivilEmerg
import TextRules
import string, time, re, os, types, copy

class TextProduct(CivilEmerg.TextProduct):
    Definition = copy.deepcopy(CivilEmerg.TextProduct.Definition)
    Definition["displayName"] = "CivilEmergency_BLU_<MultiPil> (Law Enforcement Warning)"
             # for Product Generation Menu
    Definition["outputFile"] = "{prddir}/TEXT/BLU_<MultiPil>.txt"
    ## Edit Areas: Create Combinations file with edit area combinations.
    Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display
    Definition["defaultEditAreas"] = "Combinations_BLU_<site>"
    Definition["productName"] = "Blue Alert" # product name
    Definition["wmoID"] = "<wmoID>"                      # WMO code
    Definition["pil"] = "<pil>"                      # product pil
    Definition["textdbPil"] = "<textdbPil>"          # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"      # Product ID for transmitting to AWIPS WAN.

    def __init__(self):
        CivilEmerg.TextProduct.__init__(self)


    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        fcst = fcst + "The following message is transmitted" + \
               " at the request of the " + self._source + "."
        return fcst