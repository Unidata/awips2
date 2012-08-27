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
#-------------------------------------------------------------------------
# Description: This is a generic product template.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.

# Customization Points:
#
# DEFINITION SECTION
#
# Required Configuration Items:
#
#  displayName      If not None, defines how product appears in GFE GUI
#
#  You must set the following:
#
#  productName      defines name of product e.g. "ZONE FORECAST PRODUCT"
#  fullStationID    Full station identifier, 4 letter, such as "KSLC".
#  wmoID            WMO ID code for product header, such as "FOUS45"
#  pil              Product pil, such as "SFTBOS"
#  areaName (opt.)  Area name for product header, such as "WESTERN NEW YORK"
#  wfoCityState     City,state that the WFO is located in, such as "BUFFALO NY"
#
# Optional Configuration Items
#
#  mapNameForCombinations Name of the map background that is used for 
#                         creating/editing the combinations file.  This must 
#                         be defined or the GFE zone combiner
#  database               Source database for product. Can be "Official", 
#                         "Fcst" or "ISC"
#  outputFile             Defines the output location of the finished product
#                         when saved from the Formatter Launcher.
#  debug                  If on, debug_print statements will appear.
#  textdbPil              Defines the awips product identifier 
#                         (e.g., DENCCFDEN) that is used to store the product 
#                         in the AWIPS text database. 
#                         This value is also used for the default GUI entry for 
#                         storage.
#  awipsWANPil            Defines the awips product identifier 
#                         (e.g., KBOUCCFDEN) that is used to transmit the 
#                         product to the AWIPS WAN. 
#                         This value is also used for the default GUI 
#                         entry for storage.
#  lineLength             Desired maximum length of each line.
#
#  defaultEditAreas       Defines edit areas, default is Combinations
#  includeCities          If 1, cities will be included in the area header
#  cityDescriptor         "INCLUDING THE CITIES OF" phrase used when including
#                         cities
#  includeZoneNames       If 1, zone names will be included in the area header
#
#  areaDictionary         Modify the AreaDictionary utility with UGC information about zones.
#
#  singleComboOnly        If set to 1, this indicates that the zone combiner should only use one
#                         combination. This is used for non-segmented products.
#
#  purgeTime              Default expiration in hours 
#
#  
#  Trouble-shooting items
#    passLimit -- Limit on passes allowed through Narrative Tree
#    trace     -- Set to 1 to turn on trace through Narrative Tree   
#
#
#-------------------------------------------------------------------------
# Weather Elements Needed:
#-------------------------------------------------------------------------
# Edit Areas Needed: None 
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file:
#   Combinations file
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
#
# To look up tasks and their status, see the Text Product User Guide
# Section on "Tkgnats: Task Reporting System".
#-------------------------------------------------------------------------
# Additional Information:
#
#       
#-------------------------------------------------------------------------
# Example Output:
#  Refer to the NWS C11 and 10-503 Directives for Public Weather Services.
#-------------------------------------------------------------------------

import TextRules
import SampleAnalysis
import ForecastNarrative
import time, string, types, copy
import ProcessVariableList, AbsTime          

class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    
    Definition =  {
        "type": "smart",
        "displayName": None, # for Product Generation Menu
        "database" : "Official",  # Source database. "Official", "Fcst", or "ISC"

        "outputFile": "",
        "debug": 0,
       
        # Name of map background for creating Combinations
        "mapNameForCombinations": "", 
        
        ## Edit Areas: Create Combinations file with edit area combinations.
        "defaultEditAreas" : "",
        "includeCities": 1 ,  # Cities included in area header
        "cityDescriptor":"INCLUDING THE CITIES OF",
        "includeZoneNames":1, # Zone names will be included in the area header

        # product identifiers
        "productName": "",                      # product name 
        "fullStationID" : "<fullStationID>",    # 4 letter station ID
        "wmoID" : "<wmoID>",                    # WMO code
        "wfoCityState" : "<wfoCityState>",      # Location of WFO
        "pil" : "<pil>",                        # product pil
        "textdbPil" : "<textdbPil>",            # Product ID for storing to AWIPS text database.
        "awipsWANPil" : "<awipsWANPil>",        # Product ID for transmitting to AWIPS WAN.
        "wfoSiteID": "<site>",
        "areaName": "",                         #optional area name for header

        # Area Dictionary -- Descriptive information about zones
        "areaDictionary": "AreaDictionary",

        # Use only a single zone combination (1 for non-segmented product, 0 - segmented)
        "singleComboOnly": 0,

        # Language
        "language": "english",
        "lineLength": 66,            # Maximum line length

        # Expiration
        "purgeTime" : 2,             # Default Expiration in hours 
        }

    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)

    def generateForecast(self, argDict):
        # Generate Text Phrases for a list of edit areas
        
        # Get variables
        error = self._getVariables(argDict)
        if error is not None:
            return error
        
        # Get the areaList -- derived from defaultEditAreas and
        # may be solicited at run-time from user if desired
        self._areaList = self.getAreaList(argDict)
        if len(self._areaList) == 0:
            return "WARNING -- No Edit Areas Specified to Generate Product."
        
        # Determine time ranges
        error = self._determineTimeRanges(argDict)
        if error is not None:
            return error

        # Sample the data
        error = self._sampleData(argDict)
        if error is not None:
            return error

        # Initialize the output string
        fcst = ""
        fcst = self._preProcessProduct(fcst, argDict)

        # Generate the product for each edit area in the list
        fraction = 0
        fractionOne = 1.0/float(len(self._areaList))
        percent = 50.0
        self.setProgressPercentage(50)
        for editArea, areaLabel in self._areaList:
            self.progressMessage(fraction, percent, "Making Product for " + areaLabel)
            fcst = self._preProcessArea(fcst, editArea, areaLabel, argDict)
            fcst  = self._makeProduct(fcst, editArea, areaLabel, argDict)
            fcst = self._postProcessArea(fcst, editArea, areaLabel, argDict)
            fraction = fractionOne
        fcst = self._postProcessProduct(fcst, argDict)
        fcst = string.upper(fcst)
        return fcst

    def _getVariables(self, argDict):

        # Make argDict accessible
        self.__argDict = argDict
        
        # Get Definition variables
        self._definition = argDict["forecastDef"]
        for key in self._definition.keys():
            exec "self._" + key + "= self._definition[key]"

        # Get VariableList and _issuance_list variables
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"

        self._language = argDict["language"]
        return None

    def _determineTimeRanges(self, argDict):
        # Set up the Narrative Definition and initial Time Range
        # Calculate ddhhmm string value:
        self._timeRange = self.createTimeRange(0, 240)
        self._currentTime = argDict['creationTime']  #ZULU
        self._ddhhmmTime = time.strftime("%d%H%M",time.gmtime(
            self._currentTime))
        self._timeLabel = self.getCurrentTime(
           argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1).upper()
        self._issueTime = AbsTime.current()

        self._expireTime = self._issueTime + self._purgeTime*3600
            
        return None

    def _sampleData(self, argDict):
        # Sample and analyze the data for the narrative
        return None

    def _preProcessProduct(self, fcst, argDict):
        # Product header
        if self._areaName != "":
             productName = self._productName.strip() + " FOR " + \
                           self._areaName.strip()
        else:
             productName = self._productName.strip()
        issuedByString = self.getIssuedByString()
        productName = self.checkTestMode(argDict, productName)

        fcst =  fcst + self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n" +\
               productName + "\n" +\
               "NATIONAL WEATHER SERVICE " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n\n"

        fcst = string.upper(fcst)
        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        # This is the header for an edit area combination
        areaHeader = self.makeAreaHeader(
            argDict, areaLabel, self._issueTime, self._expireTime,
            self._areaDictionary, self._defaultEditAreas,
            cityDescriptor=self._cityDescriptor, includeCities=self._includeCities,
            includeZoneNames = self._includeZoneNames)
        fcst = fcst + areaHeader
        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        return fcst

    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        return fcst + "\n\n$$\n"

    def _postProcessProduct(self, fcst, argDict):
        fcst = string.upper(fcst)
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst

    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################




