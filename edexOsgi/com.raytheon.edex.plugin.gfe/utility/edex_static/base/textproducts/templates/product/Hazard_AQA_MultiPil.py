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
########################################################################
# Hazard_AQA.py
#
# SOFTWARE HISTORY
# Date            Ticket#        Engineer    Description
# ------------    ----------     ----------- --------------------------
# Oct 20, 2014    #3685          randerso    Changed to support mixed case
# Apr 28, 2015    #4027          randerso    Additional changes for mixed case
# Jul 15, 2016    #5749          randerso    Replaced ellipses with commas
#
##

import GenericHazards
import string, time, re, os, types, copy
import ProcessVariableList

class TextProduct(GenericHazards.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)
    
    Definition['displayName'] = None
    Definition['displayName'] = "BaselineHazard_AQA_<MultiPil> (Air Quality Alert)"

    Definition["defaultEditAreas"] = "EditAreas_PublicZones_<site>_<MultiPil>"
    Definition["mapNameForCombinations"] = "Zones_<site>" # Map background for creating Combinations

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

    # Header configuration items
    Definition["productName"] = "Air Quality Alert"  # name of product
    Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
    Definition["wmoID"] = "<wmoID>"        # WMO ID
    Definition["pil"] = "<pil>"          # product pil
    #Definition["areaName"] = "Statename"  # Name of state, such as "Georgia"
    Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
    Definition["wfoCity"] = "<wfoCity>"       # WFO Name as it should appear in a text product
    Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
    Definition["outputFile"] =  "{prddir}/TEXT/AQA_<MultiPil>.txt"

    # OPTIONAL CONFIGURATION ITEMS
    #Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
    #Definition["displayOutputDialog"] = 0  # If 1 will display results when finished
    #Definition["debug"] = 1
    Definition["headlineEditAreaGroup"] = "Zones" # Name of EditAreaGroup for sampling headlines

    Definition["purgeTime"] = 24       # Maximum hours for expireTime from issueTime
    Definition["includeCities"] = 1    # Cities included in area header
    Definition["cityDescriptor"] = "Including the cities of"
    Definition["includeZoneNames"] = 1 # Zone names will be included in the area header
    #Definition["easPhrase"] = ""       # Optional EAS phrase to be include in product header
    Definition["lineLength"] = 69
    Definition["includeOverviewHeadline"] = 1   #If 1, the overview header is templated
    Definition["includeOverview"] = 1   #If 1, the overview section is templated
    #########################################################
    # Be sure to override the agencyDict                    #
    # in your Definition file                               #
    #########################################################
    Definition["agencyDict"] = {
        
        "Forsyth": {
        "name": "Forsyth County Environmental Affairs Department Winston-Salem NC",
        "declaration": "The Forsyth County Environmental Affairs Department has issued an Air Quality Action Day, ",
        "zones": ["FLZ039"],
        "text": "A Code @ALERTCODE Air Quality Alert for Ozone has been issued. Ground level ozone concentrations within the region may approach or exceed unhealthy standards. @ALERTCTA For additional information, please visit the Forsyth County Environmental Affairs Department web site at http://www.co.forsyth.nc.us/envaffairs.",
        },
        
        "NC": {
        "name": "North Carolina Department of Environmental and Natural Resources Raleigh NC",
        "declaration": "The North Carolina Department of Environmental and Natural Resources has issued an Air Quality Action Day, ",
        "zones" : ["FLZ042", "FLZ043","FLZ048"],
        "text": "A Code @ALERTCODE Air Quality Alert for Ozone has been issued. Ground level ozone concentrations within the region may approach or exceed unhealthy standards. @ALERTCTA For additional information, please visit the North Carolina Division of Air Quality web site at http://daq.state.nc.us/airaware/forecast/.",
        },        
        }
    
    ############################################################
    # Override the alertCodes and alertCTAsDict for your Site. #
    # If you do not want to use alertCodes,                    #
    #      set alertCodes to []                                #
    # If you want alertCodes and/or alertCTA messages to       #
    #   appear in your product, put @ALERTCODE and @ALERTCTA   #
    #   in the "text" for each agency in the agencyDict        #
    ############################################################
    Definition["alertCodes"] = ["Orange", "Red", "Purple"]
    Definition["alertCTAsDict"] = {
        "Orange": "Members of sensitive groups may experience health effects. The general public is not likely to be affected.",
        "Red" : "Everyone may experience health effects. Members of sensitive groups May experience more serious health effects.",
        "Purple" : "Health alert: everyone may experience serious health effects.",
        }


    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)

    def __init__(self):
        GenericHazards.TextProduct.__init__(self)

    def _processVariableList(self, definition):
        # Get Definition variables
        for key in definition.keys():
            exec "self._" + key + "= definition[key]"
        alertCodes = self._alertCodes
        if alertCodes != []:
            varList = [(("Alert Level", "alertCode"), alertCodes[0],
                        "radio", alertCodes)]
            return self._callProcessVariableList("Input Info", varList, varDict={})
        else:
            return {}

    def _callProcessVariableList(self, title, varList, varDict):
        processVarList = ProcessVariableList.ProcessVariableList(
            title, varList, varDict={})
        self._selectionStatus = processVarList.status()
        if not self._selectionStatus.upper() == "OK":
            return None   # User Cancelled
        return processVarList.varDict()       

    def _preProcessProduct(self, fcst, argDict):
        """
        Modified to allow a multiple MND list of agencies and to insert
        Relayed By wording.
        """

        # Product header
        if self._areaName != "":
             productName = self._productName.strip() + " for " + \
                           self._areaName.strip()
        else:
             productName = self._productName.strip()
        issuedByString = self.getIssuedByString()
        productName = self.checkTestMode(argDict, productName)

        s = self._wmoID + " " + self._fullStationID + " " +\
               self._ddhhmmTime + "\n" + self._pil + "\n\n"
        fcst = fcst + s.upper()
               
        s = productName + "\n"

        # Placeholder for Agency Names to be filled in in _postProcessProduct
        #s = s + "@AGENCYNAMES" + "\n"
        s = s + "Relayed by National Weather Service " + self._wfoCityState + "\n" +\
               issuedByString + self._timeLabel + "\n\n"
        fcst = fcst + s

        return fcst

    def headlinesTiming(self, tree, node, key, timeRange, areaLabel, issuanceTime):
        """
        Modified to change start and end PhraseType to EXPLICIT.
        This will provide exact start and stop times for the AQA hazard."""
        
        # Return
        #  "startPhraseType" and "endPhraseType"
        #   Each can be one of these phraseTypes: 
        #      "EXPLICIT" will return words such as "5 PM"
        #      "FUZZY4" will return words such as "THIS EVENING"
        #      "DAY_NIGHT_ONLY" use only weekday or weekday "Night" e.g.
        #         "Sunday" or "Sunday night" or "Today" or "Tonight"
        #         Note: You will probably want to set both the
        #         startPhraseType and endPhraseType to DAY_NIGHT_ONLY to
        #         have this work correctly.
        #      "None" will result in no words
        #   OR a method which takes arguments:
        #        issueTime, eventTime, timeZone, and timeType
        #     and returns:
        #        phraseType, (hourStr, hourTZstr, description)
        #     You can use "timingWordTableFUZZY8" as an example to
        #     write your own method.
        # 
        # If you simply return None, no timing words will be used.

        # Note that you can use the information given to determine which
        # timing phrases to use. In particular, the "key" is the Hazard
        # key so different local headlines can use different timing.
        #  
        startPhraseType = "EXPLICIT"
        endPhraseType = "EXPLICIT"

        #Example code
        #startTime = timeRange.startTime().unixTime()
        #if startTime <= issuanceTime + 12 * 3600:   # 12 hours past issuance
            #startPhraseType = "EXPLICIT"
        #endTime = timeRange.endTime().unixTime()
        #if endTime <= issuanceTime + 12 * 3600:   # 12 hours past issuance
            #endPhraseType = "EXPLICIT"

        return startPhraseType, endPhraseType
        
    # Returns a formatted string announcing the hazards that are valid with
    # timing phrases
    def getHazardString(self, tree, node, fcstArea):
        if len(fcstArea) <= 0:
            return ""
        hazardTable = self._hazards.getHazardList(fcstArea)
        returnStr = ""
        issuanceTime = self._issueTime.unixTime()

        returnStr = self.makeHeadlinePhrases(tree, node, hazardTable,
                                             issuanceTime)
        #Test mode?
        returnStr = self.headlinePhraseTESTcheck(tree.get("argDict"),
          returnStr)

        return returnStr

    def _makeProduct(self, fcst, segmentAreas, argDict):
        """
        Modified to allow headlines.  Segments will be automatically
        generated based on hazards grid, if not already broken up."""
        editArea = segmentAreas[0]
        areaLabel = editArea

        # Get combinations to be used for the segment

        combinations = argDict["combinations"]
        if combinations is not None:
            areaList = self.getCurrentAreaNames(argDict, areaLabel)
            print "using combinations, areaList=", areaList
            usingCombo = 1
        else:
            for editArea, label in defaultEditAreas:
                if label == areaLabel:
                    areaList = [editArea]
            print "not using combinations, areaList=", areaList
        
        # Generate the standard headline for the segment
        self._hazards = argDict['hazards']
        self._combinations = argDict["combinations"]
        self._hazardTR = self.createTimeRange(0,240)
      
        headlines = self.generateProduct("Hazards", argDict, area = editArea,
                                         areaLabel=areaLabel,
                                         timeRange = self._hazardTR)

        self._agencyNames = ""
                                
        # If no valid AQA hazard grid, just return a placeholder
        if headlines == "":
            return fcst + "|* Statement text *|"

        # If valid hazard grid, insert headline, agency attribution, and any default text
        else:

            # Make sure main headline is in upper case.
            upperhead = string.upper(headlines)
            fcst = fcst + upperhead
            #strip out the line feed within headlines
            headlines = string.split(headlines,'\n')
            headlines = string.join(headlines)

            #create an attribution phrase containing headline info
            HeadIssue1 = ""
            HeadIssue2 = ""
            HeadIssue = ""

            # Determine the list of agencies associated with the segmentAreas
            agencies = []
            for areaLabel in segmentAreas:
                # Find the agency for this areaLabel
                for agency in self._agencyDict.keys():
                    if agency not in agencies:                        
                        zones = self._agencyDict[agency]['zones']
                        if areaLabel in zones:
                            agencies.append(agency)
                            name = self._agencyDict[agency]['name']
                            self._agencyNames = self._agencyNames + "\n" + name
            
            # Make the headline using the first agency only
            if agencies == []:
                print "\n\nCheck set up of agencyDict!! -- no agencyDict entry for "+`segmentAreas`+"\n\n"
            agency = agencies[0]
            HeadIssue1 = self._agencyDict[agency]['declaration']
            HeadIssue2 = headlines
            if "remains" in headlines:  # This is an update
                HeadIssue2 = Headissue2[29:len(HeadIssue2)-4]
            else:  # This is the first issuance
                HeadIssue2 = HeadIssue2[21:len(HeadIssue2)-4]
            HeadIssue = HeadIssue1 + HeadIssue2 + "\n\n" + self._agencyDict[agency]['text']
            fcst = fcst + HeadIssue + "\n\n"
      
            return fcst
    
    def _postProcessProduct(self, fcst, argDict):
        """
        Handles word-wrapping, line feeds, color-code text replacements
        and lower case."""

        # Replace the string '@AGENCYNAMES' with the agency names
        #fcst = fcst.replace('@AGENCYNAMES', self._agencyNames)
        if self._alertCodes != []:
            # Replace the string '@ALERTCODE' with the appropriate alertcode 
            fcst = fcst.replace('@ALERTCODE', self._alertCode)
        
        # Insert specific CTA based on alertCode.
        if re.search('@ALERTCTA',fcst) != None:
            fcst = re.sub('@ALERTCTA', self._alertCTAsDict[self._alertCode],fcst)

        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "-", "..."])

        fixMultiLF = re.compile(r'(\n\n)\n*', re.DOTALL)
        fcst = fixMultiLF.sub(r'\1', fcst)
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst
 
    def allowedHazards(self):        
        allActions = ["NEW", "EXA", "EXB", "EXT", "UPG", "CAN", "CON", "EXP"]
        return [
            ('AQ.Y', allActions, 'AirQual'),      # AIR QUALITY ALERT
            ]
    

