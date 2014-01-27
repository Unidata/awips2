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
# CivilEmerg_SPW_<MultiPil>_Local
# This product produces a Shelter In Place Warning
#
# Author: Matt Davis
# ----------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Example Output:
# Refer to the NWS 10-518 Directive for further information.
#-------------------------------------------------------------------------

import CivilEmerg
import TextRules
import string, time, re, os, types, copy

class TextProduct(CivilEmerg.TextProduct):
    Definition = copy.deepcopy(CivilEmerg.TextProduct.Definition)
    Definition["displayName"] = "CivilEmergency_SPW_<MultiPil> (Shelter In Place Warning)"
             # for Product Generation Menu               
    Definition["outputFile"] = "{prddir}/TEXT/SPW_<MultiPil>.txt"
    ## Edit Areas: Create Combinations file with edit area combinations.
    Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display
    Definition["defaultEditAreas"] = "Combinations_SPW_<site>"
    Definition["productName"] = "SHELTER IN PLACE WARNING" # product name
    Definition["wmoID"] = "<wmoID>"                      # WMO code
    Definition["pil"] = "<pil>"                      # product pil
    Definition["textdbPil"] = "<textdbPil>"          # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"      # Product ID for transmitting to AWIPS WAN.

    def __init__(self):
        CivilEmerg.TextProduct.__init__(self)        

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        fcst = fcst + "THE FOLLOWING MESSAGE IS TRANSMITTED" + \
               " AT THE REQUEST OF THE " + self._source + "."                 
        return fcst

