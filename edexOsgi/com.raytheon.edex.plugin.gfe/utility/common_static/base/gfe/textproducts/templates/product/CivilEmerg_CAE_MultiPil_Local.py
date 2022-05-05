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

##
# This is a base file that is not intended to be overridden.
##

# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CivilEmerg_CAE_<MultiPil>_Local
# This product produces a Child Abduction Emergency
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
    Definition["displayName"] = "CivilEmergency_CAE_<MultiPil> (Child Abduction Emergency)"
             # for Product Generation Menu                
    Definition["outputFile"] = "{prddir}/TEXT/CAE_<MultiPil>.txt"
    ## Edit Areas: Create Combinations file with edit area combinations. 
    Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display
    Definition["defaultEditAreas"] = "Combinations_CAE_<site>"
    Definition["productName"] = "Child Abduction Emergency" # product name
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

