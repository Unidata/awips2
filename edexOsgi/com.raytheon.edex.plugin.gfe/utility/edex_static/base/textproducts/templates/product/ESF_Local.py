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
# ESF_Local
# Hydrologic Outlook
#
# Author: Matt Davis
# ----------------------------------------------------------------------------
##
#
# SOFTWARE HISTORY
# Date            Ticket#        Engineer    Description
# ------------    ----------     ----------- --------------------------
# Oct 20, 2014    #3685          randerso    Changed to support mixed case
#
##

#-------------------------------------------------------------------------
# Example Output:
# Refer to the NWS 10-922 Directive for further information.
#-------------------------------------------------------------------------

import GenericReport
import TextRules
import string, time, re, os, types, copy

class TextProduct(GenericReport.TextProduct):


    Definition =  {
        "type": "smart",
        "displayName": "ESF",      # for Product Generation Menu
        "database" : "Official",  # Source database. "Official", "Fcst", or "ISC"

        "outputFile": "{prddir}/TEXT/ESF.txt",
        "debug": 0,

        "areaType": 'ZONES',    #default UGC type (FIPS, ZONES)
       
        ## Edit Areas: Create Combinations file with edit area combinations.
        "defaultEditAreas": "Combinations_ESF_<site>",
        "showZoneCombiner" : 1,

        # product identifiers
        "productName": "Hydrologic Outlook", # product name 
        "fullStationID" : "<fullStationID>",    # 4 letter station ID
        "wmoID" : "<wmoID>",                    # WMO code
        "wfoCityState" : "<wfoCityState>",      # Location of WFO
        "pil" : "<pil>",                        # product pil
        "textdbPil" : "<textdbPil>",            # Product ID for storing to AWIPS text database.
        "awipsWANPil" : "<awipsWANPil>",        # Product ID for transmitting to AWIPS WAN.
        "wfoSiteID": "<site>",
 
        # Area Dictionary -- Descriptive information about zones
        "areaDictionary": "AreaDictionary", 
        # Language
        "language": "english",
        "lineLength": 66,   #Maximum line length
        "includeCities" : 0,    # Cities included in area header
        "cityDescriptor" : "Including the cities of",
        "includeZoneNames" : 0, # Zone names will be included in the area header
        "includeIssueTime" : 0,   # This should be set to zero
        "singleComboOnly" : 1, # Used for non-segmented products
        "purgeTime" : 12,             # Expiration in hours
        }

    # DO NOT OVERRIDE THE FOLLOWING CODE BLOCK
    # It is necessary to properly set for zones or counties
    if Definition['areaType'] == 'FIPS':
        # Name of map background for creating Combinations
        Definition["mapNameForCombinations"] =  "FIPS_<site>" 
    else:
        # Name of map background for creating Combinations
        Definition["mapNameForCombinations"] =  "Zones_<site>" 

    def __init__(self):
        GenericReport.TextProduct.__init__(self)        

    def _preProcessProduct(self, fcst, argDict):
        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):

        #
        # First, generate WMO lines
        #

        fcst = self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n"

        #
        # Next, add the non-segmented UGC data
        #
        
        areaHeader = self.makeAreaHeader(
            argDict, areaLabel, self._issueTime, self._expireTime,
            self._areaDictionary, self._defaultEditAreas, cityDescriptor=self._cityDescriptor,
            includeCities=self._includeCities, includeZoneNames = self._includeZoneNames,
            includeIssueTime = self._includeIssueTime)
       
        fcst = fcst + areaHeader + "\n"

        #
        # Last, add the product name/time lines
        #


        issuedByString = self.getIssuedByString()
        productName = self.checkTestMode(argDict, self._productName)
        s = productName + "\n" + \
               "National Weather Service " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n\n"
        fcst = fcst + s.upper()
        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        fcst = fcst + "|* ...Headline concerning hydrologic conditions *|\n\n"
        fcst = fcst + "|* Insert forecast and narrative hydrologic information here *|"
                
        return fcst

    def _postProcessProduct(self, fcst, argDict):
        #
        # Clean up multiple line feeds
        #

        fixMultiLF = re.compile(r'(\n\n)\n*', re.DOTALL)
        fcst = fixMultiLF.sub(r'\1', fcst)
        
        
        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "...", "-"])
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst
