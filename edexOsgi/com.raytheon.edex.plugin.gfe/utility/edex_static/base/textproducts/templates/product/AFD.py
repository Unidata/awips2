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
# File Name: AFD.py
# Description: This product creates a Area Forecast Discussion product.
#   Contributed by Eastern Region (Jim Noel, Rob Radzanowski) and
#   Southern Region (Brian Curran)
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# AFD, AFD_<site>_<MultiPil>_Definition, AFD_<site>_Overrides
#-------------------------------------------------------------------------
# Weather Elements Needed: MinT, MaxT, PoP
#-------------------------------------------------------------------------
# Edit Areas Needed: Individual edit areas are required, one for each
# preliminary temp/PoP forecast point.
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file: None
#-------------------------------------------------------------------------
# Component Products: None
#-------------------------------------------------------------------------
# User Configurable Variables:
# Definition Section:
#   displayName      If not None, defines how product appears in GFE GUI
#
#   defaultEditAreas defines edit area names and station IDs for edit areas
#                    expected in the form of (editAreaName, 3letterStationID)
#   editAreaSuffix     default None. Allows for generating the body of the product for
#                      an edit area that is a subset (e.g. population areas) of the
#                      edit areas specified in the defaultEditAreas.  So given the edit area,
#                      "COZ035" and the editAreaSuffix is "_pt", then the edit area that
#                      will be sampled and reported for the body of the product will be
#                      "COZ035_pt".  If no such edit area exists, the system will simply
#                      use the original edit area.
#                      Note that Hazards will always be generated for the entire edit area.
#   productName      defines name of product e.g. "ZONE FORECAST PRODUCT"
#   fullStationID    full station identifier (4letter, KSLC)
#
#   wmoID            WMO ID for product header, such as FOUS45
#
#   pil              Product pil, such as CCFBOX
#
#  debug                  If on, debug_print statements will appear.
#  database               Source database for product. Can be "Official",
#                         "Fcst" or "ISC"
#  outputFile             Defines the output location of the finished product
#                         when saved from FormatterLauncher.
#  textdbPil              Defines the awips product identifier
#                         (e.g., DENCCFDEN) that is used to store the product
#                         in the AWIPS text database. 
#                         This value is also used for the default GUI entry for
#                         storage.
#   awipsWANPil           Defines the awips product identifier
#                         (e.g., KBOUCCFDEN) that is used to transmit the
#                         product to the AWIPS WAN. 
#                         This value is also used for the default GUI
#                         entry for storage.

#   topicDividers       List of tuples describing the various topic dividers:
#                       (topicName, topicDivider, alwaysInclude, includeInGUI) where
#                         --topicName is "Synopsis", "Update", etc.
#                         --topicDivider will appear in the product ".SYNOPSIS..."
#                         --alwaysInclude: if 1, the topic divider will always
#                           appear in the product.
#                           Otherwise, the user can choose at run-time whether
#                           to include the topic divider.
#                         --If 1, and alwaysInclude == 0, this item will appear
#                           in the GUI to be selected at run time.
#                           Some options, like PrevDisc should not appear in the
#                           GUI since they are tied to other user input e.g.
#                           getPreviousAFD.
#   state_IDs           The state_ID definitions below are for the W/W/A portion of
#                       your AFD.  Multiple state IDs (including MARINE) are separated
#                       by commas.
#   tieUpdateToPreviousAFD  If 1, then when "Include Previous AFD" is chosen in the GUI,
#                       the UPDATE topic divider will automatically be included.
#   fcstrNumberFormat   The fcstrNumberFormat can take three values:
#                          Brief - short term forecaster and long term forecaster
#                             numbers separated by a slash.
#                          Verbose - "SHORT TERM...xx" and "LONG TERM...yy".
#                          None - no forecaster numbers added to the end of the AFD.
#                       NOTE: names or numbers may be used.
#   shortTermForecasters : List of identifiers (number strings or names) for the
#                       short term forecasters
#   longTerm Forecasters : List of identifiers (number strings or names) for the
#                       long term forecasters
#   aviationForecasters : List of identifiers (number strings or names) for the
#                       aviation forecasters
#   pointEditAreas   If non-empty list, a point temp/pop table will be produced.
#   popStartZ_AM     start time for PoP for AM issuance in Zulu, (12 for 12z)
#                    Usually changed only for OCONUS sites.
#   useZoneNames     If 1, will use zone names instead of ugc codes in the W/W/A portion.
#   abbreviateUGCs   If 1, will abbreviate ugc string. Instead of: FLZ042-FLZ043-FLZ044
#                    produce: FLZ042>FLZ044
#   WWA_Nil          The WWA_Nil definition will be used for nil watches, warnings,
#                    or advisories.
#   hazardSamplingThreshold  Defines the percentage coverage or number of
#                    grid points in a zone that must contain the hazard
#                    in order for it to be considered. Tuple (percent, points)
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
#  None
#
# To look up additional tasks and their status, see the Text Product User Guide
# Section on "Tkgnats: Task Reporting System".
#-------------------------------------------------------------------------
# Additional Information:
# The grids are sampled according to the following rules:
# MaxT/MinT: Four periods, 12 hours apart, daily, set up to take the
#   MaxT grid overlapping noon LT and MinT grid overlapping midnight LT
# PoP: Four periods, 12 hours apart, 12z-00z, and 00z-12z. Periods can
#   be overridden using the popStartZ_AM field for OCONUS sites.
#
# Missing data will be shown with MMM for temperatures and PoPs.
#-------------------------------------------------------------------------
# Example Output:
#
##
##FXUS64 KMAF 041309
##AFDMAF
##
##AREA FORECAST DISCUSSION
##NATIONAL WEATHER SERVICE MIDLAND/ODESSA TX
##809 AM CDT SAT OCT 4 2003
##
##.SHORT TERM...
##
##
##&&
##
##.LONG TERM...
##
##
##&&
##
##.PRELIMINARY POINT TEMPS/POPS...
##MAF  84  60  84  60 / 100  30  20 10
##CNM  83  56  85  57 / 100  20  10  0
##MRF  79  53  79  53 / 100  30  10  0
##FST  86  62  87  62 / 100  30  10 10
##
##&&
##
##.MAF WATCHES/WARNINGS/ADVISORIES...
##TX...NONE.
##NM...NONE.
##
##&&
##
##$$
##
##99/99
########################################################################

import TextRules
import SampleAnalysis
import string, time, types, os, re, copy
import ModuleAccessor, ProcessVariableList
import AbsTime

class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):

    Definition =  {
        "type": "smart",

        "displayName": "None", # for Product Generation Menu
        "database" : "Official",  # Source database. "Official", "Fcst", or "ISC"

        "defaultEditAreas" : "EditAreas_PublicMarineFireWx_<site>_<MultiPil>",
        "editAreaSuffix": None,

        # Edit Areas for creating the optional Preliminary Point Temp/PoPs
        "pointEditAreas": [],
        "outputFile": "{prddir}/TEXT/AFD_<MultiPil>.txt",
        "debug": 0,

        "productName": "AREA FORECAST DISCUSSION", 
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

        "state_IDs": ["ST"],
        "tieUpdateToPreviousAFD": 0,
        "fcstrNumberFormat" : "Brief",  # Brief, Verbose, or None
        "shortTermForecasters": ["99","01","02","03"],
        "longTermForecasters":  ["99","01","02","03"],
        "aviationForecasters":  ["99","01","02","03"],
        "useZoneNames": 0,
        "abbreviateUGCs": 1,           

        "topicDividers" : [
               # topicName,   topicDivider,   alwaysInclude, includeInGUI
        
               ("Update",     ".UPDATE...",          0,          1),
               ("Synopsis",   ".SYNOPSIS...",        0,          1),
               
               # EITHER Discussion OR ShortTerm/LongTerm should always be included.
               ("Discussion", ".DISCUSSION...",      0,          0),
               ("ShortTerm",  ".SHORT TERM...",      1,          0),
               ("LongTerm",   ".LONG TERM...",       1,          0),
               
               # Optional dividers
               ("Aviation",   ".AVIATION...",        0,          1),
               ("Marine",     ".MARINE...",          0,          1),
               ("FireWeather",".FIRE WEATHER...",    0,          1),
               ("Hydro",      ".HYDROLOGY...",       0,          1),
               ("Climate",    ".CLIMATE...",         0,          1),
                              
               # Controlled by "includePreviousAFD"
               ("PrevDisc",   ".PREV DISCUSSION...", 0,          0),

               # Controlled by "pointEditAreas"
               ("Prelim", ".PRELIMINARY POINT TEMPS/POPS...", 0, 0),
               ],

        "popStartZ_AM": 12,                 #hour UTC
        "WWA_Nil" : "NONE.",

        "hazardSamplingThreshold": (10, None),  #(%cov, #points)
          }


    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)

####################################################################
#   generateForecast:
#   AFD formatter engine.
####################################################################
    def generateForecast(self, argDict):

        # Get variables from varDict and Definition:
        error = self._getVariables(argDict)
        if error is not None:
            return error

        # Get the segments
        hazardsC = argDict['hazards']
        self._segmentList = self.organizeHazards(hazardsC.rawAnalyzedTable())

        # Determine time ranges:
        error = self._determineTimeRanges(argDict)
        if error is not None:
            return error

        # Initialize the output string:
        fcst = ""
        fcst = self._preProcessProduct(fcst, argDict)

        fcst = self._makeProduct(fcst, argDict)

        # Append the $$ delimiter and the forecaster numbers:
        fcst = self._postProcessProduct(fcst, argDict)
        return fcst

####################################################################
#   _processVariableList
#   Displays user dialog.
####################################################################

    def _processVariableList(self, definition):
        # Get Definition variables
        for key in definition.keys():
            exec "self._" + key + "= definition[key]"

        # Create the list of optional topic dividers to appear in GUI
        self._options = []
        for topicName, topicDivider, alwaysInclude, includeInGUI in self._topicDividers:
            if topicName == "Update" and self._tieUpdateToPreviousAFD:
                continue
            if alwaysInclude:
                continue
            if includeInGUI:
                self._options.append(topicDivider)
            
        varList = []
        varList.append((("Product Issuance", "productIssuance"), "Morning", "radio",
         ["Morning","Afternoon"]))
        varList.append((("Optional\nTopics", "optionalTopics"), [], "check",
                        self._options))
        varList.append((("Include\nPrevious AFD?", "includePreviousAFD"), "NO", "radio",
         ["NO", "YES"]))
        varList.append((("Short Term\nForecaster", "shortTermFcstrNumber") , "99", "radio",
         self._shortTermForecasters))
        varList.append((("Long Term\nForecaster", "longTermFcstrNumber"), "99", "radio",
         self._longTermForecasters))
        varList.append((("Aviation\nForecaster","aviationFcstrNumber"), "", "radio",
         self._aviationForecasters))
        return self._callProcessVariableList("AFD Values", varList, varDict={})
            
    def _callProcessVariableList(self, title, varList, varDict):
        processVarList = ProcessVariableList.ProcessVariableList(
            title, varList, varDict={})
        self._selectionStatus = processVarList.status()
        if not self._selectionStatus == "OK":
            return None   # User Cancelled
        return processVarList.varDict()
    
####################################################################
#   _getVariables:
#   Retrieves variables and definitions.
####################################################################
    def _getVariables(self, argDict):
        # Make variable assignments
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"
        self._longTermFcstrNumber = self._getForecasterNumber(self._longTermFcstrNumber)
        self._shortTermFcstrNumber = self._getForecasterNumber(self._shortTermFcstrNumber)
        self._aviationFcstrNumber = self._getForecasterNumber(self._aviationFcstrNumber)

        # Make argDict accessible
        self.__argDict = argDict

        # Get Definition variables
        self._definition = argDict["forecastDef"]
        for key in self._definition.keys():
            exec "self._" + key + "= self._definition[key]"

        # Set up information for Hazards product
        # TODO uncomment following line?
        self._hazards = argDict['hazards']
        self._combinations = argDict["combinations"]
        
        # Check for state id: ST indicating that the user needs to
        # set up the list of state id's
        if len(self._state_IDs) == 1 and self._state_IDs[0] == "ST":
            return "WARNING:You must set up 'state_IDs' in Definition section before running the AFD."
        return

####################################################################
#   _determineTimeRanges:
#   Determine time ranges for product.  Returns popPeriods and
#   tempPeriods which are a list of tuples (timeRange, label).
#   Also determines the timeLabel string for the MND header.
#   Adapted from previous _determineTimeRanges found in CCF
#   formatter.
####################################################################
    def _determineTimeRanges(self, argDict):

        # Calculate ddhhmm string value:
        self._timeRange = self.createTimeRange(0, 240)
        self._currentTime = argDict['creationTime']  #ZULU
        self._ddhhmmTime = time.strftime("%d%H%M",time.gmtime(
            self._currentTime))
        self._timeLabel = self.getCurrentTime(
           argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1)
        self._issueTime = AbsTime.current()
        
        # If generating temp/pop table, determine time ranges
        if len(self._pointEditAreas) > 0:
            # numPeriods is the number of forecast periods used for the
            # preliminary temp/PoP block:
            numPeriods = 4

            # PoP Time ranges: four periods
            #   If AM, begin at 12z of issue day (default), may be overridden
            #     by the popStartZ_AM flag.
            #   If PM, begin at 00z of next day (default), may be overridden
            #     by the popStartZ_AM flag.
            if self._productIssuance == "Morning":
                startT = self._popStartZ_AM
            else:
                startT = self._popStartZ_AM + 12      # account for PM start later

            # rollover  - different days from gmtime and local time
            # so we need to sample the PoP from "yesterday"
            # for MDT, rollover occurs from 5pm-midnight LST
            if time.gmtime(self._currentTime)[2] != \
                 time.localtime(self._currentTime)[2]:
                startT = startT - 24

            popStartTR = self.createTimeRange(startT, startT + 1, mode="Zulu")
            timePeriod = 12
            timeSpan = 12
            self._popPeriods = self.getPeriods(popStartTR, timePeriod,
                                         timeSpan, numPeriods)

            # Temp Time ranges:  four periods, 12 hours apart, 5 hour span
            #   This is to catch the correct Max/Min temp grid
            #   If AM, begin with noon LT of issue day to catch MaxT
            #   If PM, begin with midnight LT of issue day to get MinT
            if self._productIssuance == "Morning":
                tempStartTR = self.createTimeRange(10, 15)
            else:
                tempStartTR = self.createTimeRange(22, 27)
            timePeriod = 12
            timeSpan = 5
            self._tempPeriods = self.getPeriods(tempStartTR, timePeriod, timeSpan,
                                      numPeriods)
        return

####################################################################
#   _addTopicDividers:
#   Puts in the required and optional topic dividers per NWSI 10-503.
#   Check for Update tied to getPreviousAFD.
#   Get the previous discussion if requested.
####################################################################

    def _addTopicDividers(self, fcst, argDict):
        # Flag for adding the aviation forecaster number later
        self._addedAviation = 0  
        pad = "\n\n\n&&\n\n"
        #print "\naddTopicDividers: user options", self._optionalTopics
        
        for topicName, topicDivider, alwaysInclude, includeInGUI in self._topicDividers:
            
            # Handle PrevDisc and Prelim in the order specified in the topicDividers list.
            if topicName == "PrevDisc" and self._includePreviousAFD == "YES":
                fcst = self._getPreviousAFD(fcst, argDict, divider=1) + "&&\n\n"
                continue
            if topicName == "Prelim":
                fcst = self._makePrelimNumberBlock(fcst, argDict)
                continue
            
            # See if we need to add this divider
            addDivider = 0
            # If alwaysInclude OR the user chose this divider from the GUI
            #   add the topic divider
            if alwaysInclude or topicDivider in self._optionalTopics:
                addDivider = 1
            # Handle Update if it's tied to previous AFD and the
            # user chose to include the previous AFD
            if topicName == "Update" and self._tieUpdateToPreviousAFD \
                   and self._includePreviousAFD == "YES":
                addDivider = 1
            if not addDivider:
                continue
            
            # Add padding
            #print "Adding divider", topicName, topicDivider
            # Check for Aviation so we can later add the aviationFcstrNumber
            if topicName == "Aviation":
                self._addedAviation = 1
            if topicDivider == "":
                continue
            if topicName == "ShortTerm":
                fcst += topicDivider + "\n\n"
            else:
                fcst += topicDivider + pad
        return fcst

    def _getTopicDivider(self, topic):
        for topicName, topicDivider, alwaysInclude, includeInGUI in self._topicDividers:
            if topicName == topic:
                return topicDivider
        return ""
        
####################################################################
#   _getPreviousAFD:
#   Gets the previous AFD.  Strips the leading MND header and the
#   trailing W/W/A block.  Also lops off the prelim number block if
#   present..
####################################################################
    def _getPreviousAFD(self, fcst, argDict, divider=0):
        # Initialize strings and lists:
        WWABlockString = "." + self._wfoSiteID + " WATCHES/W"
        newAFD = []

        # Retrieve the previous AFD and store the list in AFD_old:
        prevAFD = self.getPreviousProduct(self._textdbPil)
        prevAFD = string.split(prevAFD, "\n") # ADDED newline delimeter 12/7/04 bc

        # Initialize starting and ending indices:
        start_index = 0
        end_index = len(prevAFD)
        if end_index == 0:
            print "WARNING -- Previous AFD has zero length."

        # Place newlines back at the end of each element in list prevAFD:
        # ADDED 12/7/04 bc
        for index in xrange(start_index, end_index): 
            prevAFD[index] = prevAFD[index] + "\n" 

        # Make a copy of prevAFD to modify
        oldAFD = prevAFD
        # Loop through the list to find the first dot delimeter.  Once
        # found, then set start_index to the index in AFDlist.  This will
        # effectively strip off the MND header.  Will also handle headlines
        # too!
        body_start_index = start_index
        for index in xrange(start_index, end_index):
            if oldAFD[index][:1] == ".":    # first dot
                body_start_index = index
                break

        # Loop through the list to find the beginning of the W/W/A block.
        # Once found, then set end_index to the index in AFDlist.  This will
        # strip off everything below the W/W/A block including this block:
        body_end_index = end_index
        for index in xrange(body_start_index, end_index):
            if re.match(WWABlockString, oldAFD[index]) != None:
                body_end_index = index
                break

        # Make another pass to lop off the preliminary number block if it
        # is present and reset end_index.
        prelim_divider = self._getTopicDivider("Prelim")
        if prelim_divider:
            for index in xrange(body_start_index, body_end_index):
                if re.match(prelim_divider, oldAFD[index]) != None:
                    body_end_index = index
                    break

        # Suggested by Rob R. @ CTP and from ER supplement to 10-503...
        # Strip out the ampersands and the leading dot on the
        # topic divider and place in newAFD.
        for index in xrange(body_start_index, body_end_index):
            if (oldAFD[index][:1] == "."):
                newAFD.append(oldAFD[index][1:])
            elif (oldAFD[index][:2] == "&&"):
                index = index + 1
            else:
                newAFD.append(oldAFD[index])

        if divider:
            # If previous issuance time is desired, append it to the
            # _PrevDisc_Divider string:
            issuance_dateTime = ""
            # Loop through the list to find the issuance time string.
            for index in xrange(start_index, end_index-1):
                if prevAFD[index][:8] == "NATIONAL":   # next line has date time stamp
                    issuance_dateTime = str(prevAFD[index+1])
                    break
            # Build issuance_DateTime string:
            # Strip off trailing newline...
            issuance_dateTime = " /ISSUED " + issuance_dateTime[:-1] + "/ \n"
            # Eliminate double whitespace characters if present:
            issuance_dateTime = re.sub(r"  ", r" ", issuance_dateTime) # PATCH 12/7/04 bc
            fcst = fcst + self._getTopicDivider("PrevDisc") + issuance_dateTime + "\n" # PATCH 12/7/04 bc

        # Now test for multiple newlines.  If this isn't a newline, write to fcst.
        # If it is a newline, test the next one down.  If it's also a newline,
        # write the first newline and skip to the next line.
        for index in xrange(0, len(newAFD)-1):
            if newAFD[index] != "\n":
                fcst = fcst + newAFD[index]
            else:
                if newAFD[index+1] != "\n":
                    fcst = fcst + newAFD[index]
                    index = index + 1
        fcst = fcst + "\n"
        return fcst

####################################################################
#   _makePrelimNumberBlock:
#   Creates the prelim number block for the AFD.
####################################################################
    def _makePrelimNumberBlock(self, fcst, argDict):
        # Get the areaList:
        if len(self._pointEditAreas) == 0:
            return fcst

        # Convert (lat, lon, dim) entries to editAreas
        self._areaList = []
        for editArea, areaLabel in self._pointEditAreas:
            if type(editArea) is types.TupleType:
                lat, lon, dim = editArea
                editArea = self.createLatLonArea(lat, lon, dim)
            self._areaList.append((editArea, areaLabel))                        

        # Append the prelim numbers divider:
        fcst = fcst + self._getTopicDivider("Prelim") + "\n"

        # Sample the temps and PoPs:
        self._sampleData(argDict)

        # Generate the preliminary numbers:
        for editArea, areaLabel in self._areaList:
            fcst = fcst + areaLabel + " "
            fcst = self._makeFirstGuess(fcst, editArea, areaLabel, argDict)
        fcst = fcst + "\n&&\n\n"
        return fcst
    
####################################################################
#   _preProcessProduct:
#   Creates the MND header for the AFD.  Checks to see if this is
#   a routine, corrected, or updated issuance and appends this
#   to the MND.
####################################################################
    def _preProcessProduct(self, fcst, argDict):
        # Add product heading to fcst string
        fcst = fcst + self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n"

        issuedByString = self.getIssuedByString()

        productName = self.checkTestMode(argDict, self._productName) 

        fcst = fcst + self._pil + "\n\n"
        fcst = fcst + productName + "\n"
        fcst = fcst + "NATIONAL WEATHER SERVICE "
        fcst = fcst + self._wfoCityState +"\n"
        fcst = fcst + issuedByString
        fcst = fcst + self._timeLabel + "\n\n"
        return fcst

####################################################################
#   _makeProduct:
#   Formats the product
####################################################################
    def _makeProduct(self, fcst, argDict):

        # Insert topic dividers:
        fcst = self._addTopicDividers(fcst, argDict)

        # Make the Hazard block:
        fcst = self._makeHazardBlock(fcst,argDict)
        return fcst

####################################################################
#   _makeFirstGuess:
#   Creates the "first guess" temp and PoP forecasts for each edit
#   area.  Note the format is not strictly NWSI 10-503 compliant
#   as the directive makes no allowances for temperatures above 100
#   or below zero, nor does it allow for 100% PoPs.  But I got
#   permission to do it this way from SRH, so...
####################################################################
    def _makeFirstGuess(self, fcst, editArea, areaLabel, argDict):
        # Produce temp forecast substring:
        separators = [" ", " ", " ", " / ",]
        for index in xrange(0, 4):
            timeRange, label = self._tempPeriods[index]
            fcst = fcst + self._getMinOrMax(self._analysisListTemp(),
                editArea, timeRange) + separators[index]

        # Produce PoP forecast substring
        separators = [" ", " ", " ", " "]
        for index in xrange(0, 4):
            timeRange, label = self._popPeriods[index]
            fcst = fcst + self._getPoP(self._analysisListPoP(), editArea,
                              timeRange) + separators[index]
        fcst = fcst + "\n"
        return fcst


####################################################################
#   marineNameDict
#   Used in the makeHazardBlock to determine the defined names
#   for the marine zones.  This function can be overridden to change
#   the names of the marine areas.  
####################################################################
    def marineNameDict(self):
        # dictionary for marine zone identifiers
        return {}    #use the two-letter ids for the marine areas

        # if you want descriptive names for the marine areas
        #return {'AM': 'ATLANTIC COASTAL WATERS', 'GM': 'GULF OF MEXICO',
        #  'LE': 'LAKE ERIE', 'LO': 'LAKE ONTARIO', 'LH': 'LAKE HURON',
        #  'LC': 'LAKE ST CLAIR', 'LM': 'LAKE MICHIGAN', 'LS': 'LAKE SUPERIOR',
        #  'PZ': 'PACIFIC COASTAL WATERS', 'PK': 'ALASKAN COASTAL WATERS',
        #  'PH': 'HAWAIIAN COASTAL WATERS', 'PM': 'MARIANAS WATERS',
        #  'AN': 'ATLANTIC COASTAL WATERS', 
        #  'PS': 'AMERICAN SAMOA COASTAL WATERS', 'SL': 'ST LAWRENCE RIVER'}

####################################################################
#   _makeHazardBlock:
#   Cycles through the list of segments and reports the Hazards
####################################################################

    def _makeHazardBlock(self, fcst, argDict):

        fcst = fcst + "." + self._wfoSiteID + \
               " WATCHES/WARNINGS/ADVISORIES...\n"

        accessor = ModuleAccessor.ModuleAccessor()
        areaDict = accessor.variable(self._areaDictionary, "AreaDictionary")

        # get combinations file used, which contains extra info which will
        # tell us which zones are marine, firewx and public    
        combo = self._defaultEditAreas
        fireWxPhenSig = [("FW","W"), ("FW","A")]
        fireWxZones = []
        otherZones = []
        if type(combo) is types.StringType:
            try:
                m = __import__(combo)                
                for map in m.EASourceMap.keys():
                    if map.find("FireWx") != -1:
                        fireWxZones = m.EASourceMap[map]
                    else:
                        for idz in m.EASourceMap[map]:
                            if idz not in otherZones:
                                otherZones.append(idz)
            except:
                otherZones = None


        marine = self.marineNameDict()
        #
        # Get every hazard and hazard combination in effect, separate them
        # into records by state
        #
        hazardsRaw = argDict['hazards'].rawAnalyzedTable()
        hazards = self._combineHazardRecords(hazardsRaw, argDict)

        stateDict = {}
        for h in hazards:
            #determine the states in this record
            sd = {}
            ids = h['id']
            for id in ids:
                stateid = id[0:2]
                if sd.has_key(stateid):
                    locs = sd[stateid]
                    locs.append(id)
                    sd[stateid] = locs
                else:
                    sd[stateid] = [id]
            #add the record to the appropriate "state" in stateDict
            for state in sd.keys():
                hcopy = copy.deepcopy(h)
                if stateDict.has_key(state):
                    recs = stateDict[state]
                    hcopy['id'] = sd[state]
                    recs.append(hcopy)
                    stateDict[state] = recs
                else:
                    hcopy['id'] = sd[state]
                    stateDict[state] = [hcopy]

                
        
        #
        # For every state we are responsible for, check for hazards
        #

        for eachState in self._state_IDs:
            if stateDict.has_key(eachState):
                stateHazardList = stateDict[eachState]
            else:
                stateHazardList = []
            
            # add the state identifier (only if multiple states)
            if len(self._state_IDs) > 1:
                #marine zone
                if eachState in marine.keys():     
                    fcst = fcst + marine[eachState] + "..."
                else:
                    fcst = fcst + eachState + "..."

            # If no hazards are found, append the null phrase

            if len(stateHazardList) == 0:
                fcst = fcst + "NONE.\n"
                continue

            # If hazards are found, then build the hazard phrases
            for i in xrange(len(stateHazardList)):
                eachHazard  = stateHazardList[i]

                # special check code for firewx
                if (eachHazard['phen'],eachHazard['sig']) in fireWxPhenSig:
                    firezones = []
                    for id in eachHazard['id']:
                        if id in fireWxZones and id not in firezones:
                            firezones.append(id)
                    eachHazard['id'] = firezones   #eliminated public
                    stateHazardList[i] = eachHazard
                else:
                    otherzones = []
                    for id in eachHazard['id']:
                        if (otherZones is None or id in otherZones) and id not in otherzones:
                            otherzones.append(id)
                    eachHazard['id'] = otherzones   #eliminated firewx
                    stateHazardList[i] = eachHazard

                         

                # hazard name
                hazName = self.hazardName(eachHazard['hdln'], argDict, False)

                # timing phrase
                timing = self.getTimingPhrase(eachHazard, argDict['creationTime'])

                # ids
                ids = eachHazard['id']
                if len(ids) == 0:
                    continue   #skip hazard string if no zones
                if self._useZoneNames == 1:
                    zoneNames = []
                    for id in ids:
                        zoneNames.append(areaDict[id]['ugcName'])
                    ids = zoneNames
                ids.sort()
                idString = "-".join(ids)
                if self._useZoneNames == 0 and self._abbreviateUGCs == 1:
                    idString = self.makeUGCString(ids)                    

                # hazard phrase
                phrase = hazName + ' ' + timing + ' FOR ' + idString + '.'

                # Indent if there is a state list associated
                if len(self._state_IDs) > 1:
                    phrase = self.indentText(phrase, indentFirstString = '',
                                             indentNextString = '     ',
                                             maxWidth=self._lineLength,
                                             breakStrings=[" ", "-"])
                else:
                   phrase = self.indentText(phrase, indentFirstString = '',
                                             indentNextString = '',
                                             maxWidth=self._lineLength,
                                             breakStrings=[" ", "-"])

                # Apply the hazard phrases
                if len(self._state_IDs) > 1:
                    #don't indent 1st one
                    if i == 0:
                        fcst = fcst + phrase + '\n'
                    #ident the remainder
                    else:
                        fcst = fcst + "     " + phrase + '\n'
                else:
                    fcst = fcst + phrase + '\n'   #never ident - only 1 state
            
        fcst = fcst + "&&\n\n"
        return fcst

####################################################################
#   _combineHazardRecords
#   Consolidate the hazard records for the hazard block.  Combines
#   "like" records by "id".  Like records are those with the
#   same phen, sig, start, and ending times.
#   by the fcstrNumberFormat variable.
####################################################################
    def _combineHazardRecords(self, hazrecs, argDict):
        ptable = copy.deepcopy(hazrecs)
        import VTECTableUtil
        vtu = VTECTableUtil.VTECTableUtil(activeTableFileName = None)
        compare = ['phen','sig','startTime','endTime']
        acts = ['NEW','CON','EXT','EXB','EXA']   #live event
        ctable = []
        for a in ptable:
            if a['act'] not in acts:
                continue    #ignore non-live events
            #ensure we combine records currently active, but may have diff
            #start times
            if a['startTime'] < argDict['creationTime']:
                a['startTime'] = argDict['creationTime']
            found = 0
            for c in ctable:
                if vtu.hazardCompare(a, c, compare):
                    found = 1
                    zones = [a['id']]

                    allzones = c['id']
                    for z in zones:
                        allzones.append(z)
                    c['id'] = allzones
                    break
            if found == 0:
                newc = copy.deepcopy(a)
                if newc['id'] is not list:
                    newc['id'] = [newc['id']]
                ctable.append(newc)

        return ctable


####################################################################
#   _postProcessProduct:
#   Appends the $$ delimeter followed by the short and long term
#   forecaster numbers.  Display of forecaster numbers is governed
#   by the fcstrNumberFormat variable.
####################################################################
    def _postProcessProduct(self, fcst, argDict):
        # Put in the $$ delimeter:
        fcst = fcst + "$$\n\n"
        # Add the forecaster numbers to the fcst string:
        if self._fcstrNumberFormat == "Brief":
            fcst = fcst + self._shortTermFcstrNumber
            if self._longTermFcstrNumber != "":
                fcst = fcst + "/" + self._longTermFcstrNumber
            if self._addedAviation:
                fcst = fcst + "/" +self._aviationFcstrNumber

        elif self._fcstrNumberFormat == "Verbose":
            fcst = fcst + "SHORT TERM..." + self._shortTermFcstrNumber
            if self._longTermFcstrNumber != "":
                fcst = fcst + "\nLONG TERM...." + self._longTermFcstrNumber
            if self._addedAviation:
                fcst = fcst + "\nAVIATION..." + self._aviationFcstrNumber

        return fcst

#####################################################################
#   _sampleData, _analysisListPop, _analysisListTemp:
#   Sample the temp and PoP grids.  Returns the samplers for temp and
#   PoP.
#####################################################################
    def _sampleData(self, argDict):
        # Sample the data. Returns the samplers for pop and temp
        sampleList = []
        sampleList.append((self._analysisListPoP(), self._popPeriods))
        sampleList.append((self._analysisListTemp(), self._tempPeriods))
        sampleInfo = []
        for analList, periods in sampleList:
            sampleInfo.append((analList, periods, self._areaList))

        self._sampler = self.getSampler(argDict, sampleInfo)
        return

    def _analysisListPoP(self):
        return [
            ("PoP", self.stdDevMaxAvg),
            ]

    def _analysisListTemp(self):
        return [
            ("MinT", self.avg),
            ("MaxT", self.avg),
            ]

####################################################################
#   _getMinOrMax:
#   Returns a Max or Min value depending on availability.
####################################################################
    def _getMinOrMax(self, analysisList, area, timeRange):

        statDict = self.getStatDict(self._sampler, analysisList,
                                    timeRange, area)
        dayNight = self.getPeriod(timeRange,shiftToLocal=1)
        if dayNight == self.DAYTIME():
            maxV = self.getStats(statDict, "MaxT")
            return self._getTemp(maxV)
        else:
            minV = self.getStats(statDict, "MinT")
            return self._getTemp(minV)

####################################################################
#   _getTemp:
#   Returns a three character string containing the temperature.
#   For positive values less than 100, the leading 0 is replaced by
#   a space.  If no grid is found, "MMM" is returned.
####################################################################
    def _getTemp(self, value):
        if value is None:
            return "MMM"   #for missing
        value = int(round(value))
        valStr = string.rjust(`value`, 3)
        return valStr

####################################################################
#   _getPoP:
#   Returns a three character string containing the PoP to the
#   nearest 10 percent.
####################################################################
    def _getPoP(self,analysisList,area,timeRange):
        statDict = self.getStatDict(
                self._sampler, analysisList, timeRange, area)
        pop = self.getStats(statDict, "PoP")
        if pop is None:
            return "MMM"
        value = int(self.round(pop,"Nearest",10))
        valStr = string.rjust(`value`, 3)
        return valStr

####################################################################
#   _getForecasterNumber:
#   Takes a number string or name and returns a string.
#   Removes leading zeros from numbers.
####################################################################
    def _getForecasterNumber(self, numberString):
        try:
            result = "" # set result to null
            num = int(numberString) # convert numberString to integer
            if num > 99 or num < 0: # if outside [0,99] assign 99 string
                result = `99`
            elif num < 10: # if less than 10 pad leading 0:
                result = string.zfill(`num`, 2)
            else: # convert back to string
                result = `num`
            return result
        except:
            return numberString

####################################################################

### Removed inland tropical hazards in OB9.3
    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        tropicalActions = ["NEW", "EXA", "EXB", "EXT", "UPG", "CAN", 
          "CON", "EXP"]
        marineActions = ["NEW", "EXA", "EXB", "EXT", "CON"]
        return [
            ('HU.W', tropicalActions, 'Tropical'),     # HURRICANE WARNING
            ('TY.W', tropicalActions, 'Tropical1'),    # TYPHOON WARNING
            ('TR.W', tropicalActions, 'Tropical2'),    # TROPICAL STORM WARNING
            ('HU.A', tropicalActions, 'Tropical3'),     # HURRICANE WATCH
            ('TY.A', tropicalActions, 'Tropical4'),    # TYPHOON WATCH
            ('TR.A', tropicalActions, 'Tropical5'),    # TROPICAL STORM WATCH
            ('HF.A', allActions, 'Marine'),       # HURRICANE FORCE WIND WATCH
            ('HF.W', allActions, 'Marine1'),       # HURRICANE FORCE WIND WARNING
            ('SR.A', allActions, 'Marine2'),       # STORM WATCH
            ('SR.W', allActions, 'Marine3'),       # STORM WARNING
            ('GL.A', allActions, 'Marine4'),       # GALE WATCH
            ('GL.W', allActions, 'Marine5'),       # GALE WARNING
            ('SE.A', allActions, 'Marine6'),       # HAZARDOUS SEAS WATCH
            ('SE.W', allActions, 'Marine7'),       # HAZARDOUS SEAS WARNING
            ('UP.A', allActions, 'IceAccr'),    # HEAVY FREEZING SPRAY WATCH
            ('UP.W', allActions, 'IceAccr1'),    # HEAVY FREEZING SPRAY WARNING
            ('UP.Y', allActions, 'IceAccr2'),    # FREEZING SPRAY ADVISORY
            ('SC.Y', allActions, 'Marine8'),       # SMALL CRAFT ADVISORY
            ('SW.Y', allActions, 'Marine9'),       # SMALL CRAFT ADVISORY
            ('RB.Y', allActions, 'Marine10'),       # SMALL CRAFT ADVISORY
            ('SI.Y', allActions, 'Marine11'),       # SMALL CRAFT ADVISORY
            ('BW.Y', allActions, 'Marine12'),       # BRISK WIND ADVISORY
            ('MH.W', allActions, 'Marine16'),       # VOLCANIC ASHFALL WARNING            
            ('MF.Y', allActions, 'Marine13'),       # DENSE FOG ADVISORY
            ('MS.Y', allActions, 'Marine14'),       # DENSE SMOKE ADVISORY
            ('MH.Y', allActions, 'Marine15'),       # VOLCANIC ASHFALL ADVISORY
            ('BZ.W', allActions, 'WinterWx'),     # BLIZZARD WARNING
            ('IS.W', allActions, 'WinterWx1'),     # ICE STORM WARNING
            ('LE.W', allActions, 'WinterWx2'),     # LAKE EFFECT SNOW WARNING
            ('WS.W', allActions, 'WinterWx3'),     # WINTER STORM WARNING
            ('LE.Y', allActions, 'WinterWx4'),     # LAKE EFFECT SNOW ADVISORY
            ('WW.Y', allActions, 'WinterWx5'),     # WINTER WEATHER ADVISORY
            ('ZR.Y', allActions, 'WinterWx9'),     # FREEZING RAIN ADVISORY
            ('BZ.A', allActions, 'WinterWx6'),     # BLIZZARD WATCH
            ('LE.A', allActions, 'WinterWx7'),     # LAKE EFFECT SNOW WATCH
            ('WS.A', allActions, 'WinterWx8'),     # WINTER STORM WATCH
            ('WC.W', allActions, 'WindChill'),    # WIND CHILL WARNING
            ('WC.Y', allActions, 'WindChill1'),    # WIND CHILL ADVISORY
            ('WC.A', allActions, 'WindChill2'),    # WIND CHILL WATCH
            ('DS.W', allActions, 'Dust'),         # DUST STORM WARNING
            ('DU.Y', allActions, 'Dust1'),         # BLOWING DUST ADVISORY
            ('EC.W', allActions, 'Cold'),         # EXTREME COLD WARNING
            ('EC.A', allActions, 'Cold2'),         # EXTREME COLD WATCH
            ('EH.W', allActions, 'Heat'),         # EXCESSIVE HEAT WARNING
            ('EH.A', allActions, 'Heat1'),         # EXCESSIVE HEAT WATCH
            ('HT.Y', allActions, 'Heat2'),         # HEAT ADVISORY
            ('FG.Y', allActions, 'Fog'),          # DENSE FOG ADVISORY
            ('ZF.Y', allActions, 'Fog2'),          # FREEZING FOG ADVISORY
            ('HZ.W', allActions, 'FrostFreeze'),  # HARD FREEZE WARNING
            ('FZ.W', allActions, 'FrostFreeze1'),  # FREEZE WARNING
            ('FR.Y', allActions, 'FrostFreeze2'),  # FROST ADVISORY
            ('HZ.A', allActions, 'FrostFreeze3'),  # HARD FREEZE WATCH
            ('FZ.A', allActions, 'FrostFreeze4'),  # FREEZE WATCH
            ('HW.W', allActions, 'Wind'),         # HIGH WIND WARNING
            ('WI.Y', allActions, 'Wind1'),         # WIND ADVISORY
            ('LW.Y', allActions, 'Wind2'),         # LAKE WIND ADVISORY
            ('HW.A', allActions, 'Wind3'),         # HIGH WIND WATCH
            ('SM.Y', allActions, 'Smoke'),        # DENSE SMOKE ADVISORY
            ('FF.A', allActions, 'Flood'),        # FLASH FLOOD WATCH
            ('FA.A', allActions, 'Flood1'),        # FLOOD WATCH
            ('CF.W', allActions, 'CoastalFlood'), # COASTAL FLOOD WARNING
            ('CF.Y', allActions, 'CoastalFlood3'), # COASTAL FLOOD ADVISORY
            ('CF.A', allActions, 'CoastalFlood1'), # COASTAL FLOOD WATCH
            ('LS.W', allActions, 'CoastalFlood5'), # LAKESHORE FLOOD WARNING
            ('LS.A', allActions, 'CoastalFlood2'), # LAKESHORE FLOOD WATCH
            ('LS.Y', allActions, 'CoastalFlood4'), # LAKESHORE FLOOD ADVISORY
            ('AS.Y', allActions, 'AirStag'),      # AIR STAGNATION ADVISORY
            ('AS.O', allActions, 'AirStag1'),      # AIR STAGNATION OUTLOOK
            ('SU.W', allActions, 'HighSurf'),     # HIGH SURF WARNING
            ('SU.Y', allActions, 'HighSurf1'),     # HIGH SURF ADVISORY
            ('RP.S', allActions, 'Rip'),           # HIGH RIP CURRENT RISK
            ('AF.W', allActions, 'Ashfall2'),      # VOLCANIC ASHFALL WARNING
            ('AF.Y', allActions, 'Ashfall'),      # VOLCANIC ASHFALL ADVISORY
            ('TS.W', allActions, 'Tsunami'),      # TSUNAMI WARNING
            ('TS.A', allActions, 'Tsunami1'),      # TSUNAMI WATCH
            ('FW.W', allActions, 'FireWx'),       # RED FLAG WARNING
            ('FW.A', allActions, 'FireWx1'),       # FIRE WEATHER WATCH           
            ('LO.Y', marineActions, 'LowWater'),   # LOW WATER ADVISORY
            ]
