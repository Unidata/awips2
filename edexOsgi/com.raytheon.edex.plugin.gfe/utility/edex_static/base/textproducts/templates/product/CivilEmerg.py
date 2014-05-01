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
# CivilEmerg.py
# Standard File for Civil Emergencies
#
# Author: Matt Davis
# ----------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Example Output:
# Refer to the NWS 10-518 Directive for further information.
#-------------------------------------------------------------------------

import GenericReport
import TextRules
import string, time, re, os, types, copy


class TextProduct(GenericReport.TextProduct):

    VariableList = [

             #
             # Local Agency configuration
             #

             (("Source", "source") , 
              "<state> EMERGENCY MANAGEMENT AGENCY <wfoCity> <state>",
              "alphaNumeric"),

             #
             # Do not change these
             #
        
             (("EAS Level", "eas") , "NONE", "radio",
              ["NONE",
               "URGENT - IMMEDIATE BROADCAST REQUESTED",
               "BULLETIN - IMMEDIATE BROADCAST REQUESTED",
               "BULLETIN - EAS ACTIVATION REQUESTED"
               ])
             ]
             

    Definition =  {
        "type": "smart",
        "displayName": None,      # for Product Generation Menu
        "database" : "Official",  # Source database. "Official", "Fcst", or "ISC"

        "outputFile": "{prddir}/TEXT/ADR_<MultiPil>.txt",
        "debug": 0,
       
        # Name of map background for creating Combinations
        "mapNameForCombinations": "FIPS_<site>", 
        
        ## Edit Areas: Create Combinations file with edit area combinations.
        "showZoneCombiner" : 1, # 1 to cause zone combiner to display
        "defaultEditAreas" : "Combinations_ADR_<site>",

        # product identifiers
        "productName": "ADMINISTRATIVE MESSAGE", # product name 
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
        "cityDescriptor" : "INCLUDING THE CITIES OF",
        "includeZoneNames" : 0, # Zone names will be included in the area header
        "includeIssueTime" : 0,   # This should be set to zero
        "singleComboOnly" : 1, # Used for non-segmented products
        "purgeTime" : 3,             # Expiration in hours
        "callToAction" : 0, # disable call to action markers
        }

    #Use Zone codes for PR and AR regions
    if "<region>" in ['PR','AR']:
        Definition["mapNameForCombinations"] =  "Zones_<site>" 

    def __init__(self):
        GenericReport.TextProduct.__init__(self)        

    def _preProcessProduct(self, fcst, argDict):
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"
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

        try:
            if self._eas == "NONE":
                self._eas = ""
            else:
                self._eas = self._eas + "\n"
        except:
            self._eas = ""

        try:
            source = self._source + '\n'
        except:
            source = ""

        issuedByString = self.getIssuedByString()
        productName = self.checkTestMode(argDict, self._productName)
        fcst = fcst + self._eas + productName + "\n" +\
               source +\
               "RELAYED BY NATIONAL WEATHER SERVICE " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n\n"
        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        fcst = fcst + "...ADMINISTRATIVE MESSAGE...\n\n"
        fcst = fcst + "THE FOLLOWING MESSAGE IS TRANSMITTED" + \
               " AT THE REQUEST OF THE " + self._source + "." 
       
        if self._callToAction:
            fcst = self._makeCallToAction(fcst, editArea, areaLabel, argDict)

        return fcst

    def _makeCallToAction(self, fcst, editArea, areaLabel, argDict):
        ctaBodyPhrase = ""
        if self._callToAction:
            ctaBodyPhrase = "\n\nPRECAUTIONARY/PREPAREDNESS ACTIONS...\n\n" + \
                        ctaBodyPhrase + \
                        "|* CALL TO ACTION GOES HERE *|\n\n" + \
                        "\n\n&&\n\n"
        fcst = fcst + ctaBodyPhrase
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
