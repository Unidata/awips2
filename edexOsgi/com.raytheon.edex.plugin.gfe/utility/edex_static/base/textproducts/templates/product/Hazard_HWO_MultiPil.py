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
# Hazard_HWO
# Produces HWO product.
#
# Author: Matt Davis/ARX
# ----------------------------------------------------------------------------

import GenericReport
import TextRules
import string, time, re, os, types, copy

class TextProduct(GenericReport.TextProduct):
    Definition = copy.deepcopy(GenericReport.TextProduct.Definition)

    #Definition['displayName'] = None
    Definition['displayName'] = "BaselineHazard_HWO_<MultiPil> (Hazardous Weather Outlook)"

    Definition["outputFile"] = "{prddir}/TEXT/HWO_<MultiPil>.txt"
    Definition["database"] =  "Official"  # Source database

    Definition["debug"] = 0
   
    # Name of map background for creating Combinations
    Definition["mapNameForCombinations"] = ["Zones_<site>",
      "Marine_Zones_<site>"]
        
    ## Edit Areas: Create Combinations file with edit area combinations.
    Definition["defaultEditAreas"] = "Combinations_HWO_<site>_<MultiPil>"
    Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display
 
    # product identifiers
    Definition["productName"] = "Hazardous Weather Outlook" # product name 
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

    # Expirations
    Definition["purgeTime"] = 24          # Default Expiration in hours if 

    # Header format
    Definition["includeCities"] = 0  # Cities not included in area header
    Definition ["cityDescriptor"] = "Including the cities of"
    Definition["includeZoneNames"] = 1 # Zone names will be included in the area header

    #
    # The below is used for NIL HWOS
    #
    
    VariableList = [
             (("HWO Type", "hwoType") , "No Active Weather", "radio",
              ["No Active Weather", "Active Weather"])
             ] 
    
    def __init__(self):
        GenericReport.TextProduct.__init__(self)

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):

        #
        # Get combinations to be used in the header
        #
        
        combinations = argDict["combinations"]
        if combinations is not None:
            headerAreaList = self.getCurrentAreaNames(argDict, areaLabel)
            usingCombo = 1
        else:
            for editArea, label in self._defaultEditAreas:
                if label == areaLabel:
                    headerAreaList = [editArea]
                    
        # This is the header for an edit area combination
        areaHeader = self.makeAreaHeader(
            argDict, areaLabel, self._issueTime, self._expireTime,
            self._areaDictionary, self._defaultEditAreas,
            cityDescriptor=self._cityDescriptor, areaList=headerAreaList,
            includeCities=self._includeCities,
            includeZoneNames = self._includeZoneNames)
        fcst = fcst + areaHeader
        return fcst


    def _makeProduct(self, fcst, editArea, areaLabel, argDict):

        #
        # Get combinations to be used for the segment
        #
        
        combinations = argDict["combinations"]
        if combinations is not None:
            areaList = self.getCurrentAreaNames(argDict, areaLabel)
            usingCombo = 1
        else:
            for editArea, label in self._defaultEditAreas:
                if label == areaLabel:
                    areaList = [editArea]

        #
        # Make the general area Phrase
        #
        
        generalAreas = self.getGeneralAreaList(areaList, areaDictName=self._areaDictionary)
        areaPhrase = "This Hazardous Weather Outlook is for portions of "
        
        #  Make a list of all general areas we found
        #parts of the states
        areaList = []
        for state, partOfState, names in generalAreas:
            if partOfState == '' or partOfState == ' ':
                areaList.append(state)
            else:
                areaList.append(partOfState + " " + state)

        #  Add this general area to the text
        areaPhrase += self.punctuateList(areaList)
        fcst = fcst + areaPhrase + ".\n\n"   

        #
        # Make the time period headers
        #

        currentTime = argDict['creationTime']
        currentHour = time.strftime("%H", time.localtime(currentTime))
        currentHour = int(currentHour)

      
        if currentHour < 3:
            baseTime = currentTime - 10800
        else:
            baseTime = currentTime

        tommorow = time.strftime("%A", time.localtime(baseTime + 86400))
        daySeven = time.strftime("%A", time.localtime(baseTime + 518400))


        if currentHour >= 16 or currentHour < 3:
            dayOnePhrase = ".DAY ONE...Tonight"
        elif currentHour >= 3 and currentHour < 11:
            dayOnePhrase = ".DAY ONE...Today and Tonight"
        elif currentHour >= 11 and currentHour < 16:
            dayOnePhrase = ".DAY ONE...This Afternoon and Tonight"

        dayTwoSevenPhrase = ".DAYS TWO THROUGH SEVEN..." + tommorow +\
                            " through " + daySeven

        #
        # Check for the NIL HWO
        #
        
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"

        dayOneText = ""
        dayTwoSevenText = ""
        spotterText = ""

        if self._hwoType == "No Active Weather":
            dayOneText = self.getDayOneText()
            dayTwoSevenText = self.getDayTwoSevenText()
            spotterText = self.getSpotterText()

        spotterInfoStmt = self.getSpotterInfoStmt()
            
        fcst = fcst + dayOnePhrase + "\n\n" + dayOneText + "\n\n" +\
               dayTwoSevenPhrase + "\n\n" + dayTwoSevenText + "\n\n" +\
               spotterInfoStmt + "\n\n" +\
               spotterText + "\n\n"
        
        return fcst

    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        return fcst + "\n$$\n\n"

    def _postProcessProduct(self, fcst, argDict):

        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "-", "..."])

        #
        # Clean up multiple line feeds
        #

        fixMultiLF = re.compile(r'(\n\n)\n*', re.DOTALL)
        fcst = fixMultiLF.sub(r'\1', fcst)

        #
        # Finish Progress Meter
        #

        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst

    # REQUIRED OVERRIDES
 

    # OPTIONAL OVERRIDES
    def getDayOneText(self):
        return "No hazardous weather is expected at this time."

    def getDayTwoSevenText(self):
        return "No hazardous weather is expected at this time."

    def getSpotterText(self):
        return "Spotter activation will not be needed."

    def getSpotterInfoStmt(self):
        return ".SPOTTER INFORMATION STATEMENT..."
   
   # PATCHES:  To be removed with each new release




