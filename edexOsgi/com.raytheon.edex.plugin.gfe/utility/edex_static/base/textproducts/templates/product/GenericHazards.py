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
# Description: This product is a template for creating Hazard Products.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# GenericHazards
#-------------------------------------------------------------------------
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
#  outputFile             Defines the output location of the finished product.
#                         Product is saved if autoWrite is 1.
#  debug                  If on, debug_print statements will appear.
#  textdbPil              Defines the awips product identifier 
#                         (e.g., DENCCFDEN) that is used to store the product 
#                         in the AWIPS text database. The product is not 
#                         automatically stored unless autoStore is 1.  This 
#                         value is also used for the default GUI entry for 
#                         storage.
#  awipsWANPil            Defines the awips product identifier 
#                         (e.g., KBOUCCFDEN) that is used to transmit the 
#                         product to the AWIPS WAN. The product is not 
#                         automatically transmitted unless autoSend is 1.  
#                         This value is also used for the default GUI 
#                         entry for storage.
#  autoSend               If set to 1, then the product will be automatically
#                         sent on the AWIPS WAN to the "autoSendAddress" with 
#                         the "awipsWANPil after product creation.
#  autoStore              If set to 1, then the product will be automatically
#                         stored into the text database using the "textdbPil"
#                         after product creation.
#  autoWrite              If set to 1, then the product will be automatically
#                         written to the "output" named disk file after 
#                         product creation.
#
#  lineLength          max length of each line
#
#  defaultEditAreas    defines edit areas, default is Combinations
#
#  purgeTime        Maximum number of hours past issuance time for the
#                   expire time.
#  includeCities    If 1, cities will be included in the area header
#  accurateCities   If 1, cities are determined from grids
#  citiesPhrase     "INCLUDING THE CITIES OF" phrase used when including
#                   cities
#  includeZoneNames If 1, zone names will be included in the area header
#  easPhrase        Optional EAS phrase to be include in product header
#
#  hazardSamplingThreshold  Defines the percentage coverage or number of
#                    grid points in a zone that must contain the hazard
#                    in order for it to be considered. Tuple (percent, points)
#  includeOverviewHeadline   If 1, the overview header is templated
#  includeOverview    If 1, the overview section is templated
#  bulletProd         If 1, the product will use a bullet format
#-------------------------------------------------------------------------
# Weather Elements Needed:
#    Hazards
#-------------------------------------------------------------------------
# Edit Areas Needed: None 
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file:
#   Combinations file
#-------------------------------------------------------------------------
# Component Products:
#   Hazards
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
#
# To look up tasks and their status, see the Text Product User Guide
# Section on "Tkgnats: Task Reporting System".
#-------------------------------------------------------------------------
# Additional Information:
#-------------------------------------------------------------------------
# Example Output:
#-------------------------------------------------------------------------

import LogStream
import TextRules
import SampleAnalysis
import time, string, types, copy, re
import CallToActions
import AbsTime
            
class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis,
  CallToActions.CallToActions):
    Definition = {
        "type": "smart",
        "displayName": None,

        # Source database for product. Can be "Official", "Fcst" or "ISC"
        "database": "Official",
        # Defines output location of finished product.
        "outputFile": "{prddir}/TEXT/genHaz.txt",
        "debug": 0,
        # Name of map background for creating Combinations
        # Can be:
        #   Zones_BOU
        #   FireWxZones_BOU
        #   Counties
        #   Marine_Zones_BOU
        "mapNameForCombinations": "Zones_<site>", 
        
        ## Edit Areas: Create Combinations file with edit area combinations.
        ## Can be:
        ##    EditAreas_PublicZones_BOU
        ##    EditAreas_FireWx_BOU
        ##    EditAreas_FIPS_BOU
        ##    EditAreas_MarineZones_BOU
        "defaultEditAreas" : "EditAreas_PublicZones_<site>_<MultiPil>",

        # product identifiers
        "productName": "GENERIC HAZARD PRODUCT", # product name 
        "fullStationID": "<fullStationID>", # full station identifier (4letter)
        "wmoID": "<wmoID>",          # WMO ID
        "pil": "<pil>",            # Product pil
        "areaName": "",             # Name of state, such as "GEORGIA" -- optional
        "wfoCityState": "<wfoCityState>",  # Location of WFO - city,state

        "textdbPil": "<textdbPil>", # Product ID for storing to AWIPS text database.
        "awipsWANPil": "<awipsWANPil>",   # Product ID for transmitting to AWIPS WAN.
        "periodCombining" : 0,      # If 1, combine periods, if possible
        
        # automatic functions
        "autoSend": 0,   #set to 1 to automatically transmit product
        "autoSendAddress": "000",   #transmission address
        "autoStore": 0,   #set to 1 to automatically store product in textDB
        "autoWrite": 0,   #set to 1 to automatically write product to file
        
        # Area Dictionary -- Descriptive information about zones
        "areaDictionary": "AreaDictionary", 
        # Language
        "language": "english",

        "lineLength": 66,   #Maximum line length

        "purgeTime": 8,       # Maximum hours for expireTime
        "includeCities": 1 ,  # Cities included in area header
        "accurateCities": 0,  # Include all cities in area header
        "cityLocation": "CityLocation", # City lat/lon dictionary to use
        "cityDescriptor":"INCLUDING THE CITIES OF",
        "includeZoneNames":1, # Zone names will be included in the area header
        "easPhrase" :"",      # Optional EAS phrase to be include in product header        

        "includeOverviewHeadline": 1,  #include overview header
        "includeOverview": 1, #include overview section
        "bulletProd": 0, # do not default to bullets
        "hazardSamplingThreshold": (10, None),  #(%cov, #points)
        "callToAction": 1,
        }
    
    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)
        self.__overviewText = ""
        self.__procCTA = None

    def generateForecast(self, argDict):
        # Generate Text Phrases for a list of edit areas

        # Get variables
        error = self._getVariables(argDict)
        if error is not None:
            return error

        # Get the segments
        hazardsC = argDict['hazards']
        segmentList = self.organizeHazards(hazardsC.rawAnalyzedTable())
        if len(segmentList) == 0:
            return "NO HAZARDS TO REPORT"

        # Determine time ranges
        error = self._determineTimeRanges(argDict)
        if error is not None:
            return error

        # Initialize the output string
        fcst = ""
        fcst = self._preProcessProduct(fcst, argDict)

        # Generate the product for each segment in the segmentList
        fraction = 0
        fractionOne = 1.0/float(len(segmentList))
        percent = 50.0
        self.setProgressPercentage(50)
        for segmentAreas in segmentList:
            self.progressMessage(fraction, percent, "Making Product for Segment")
            fcst = self._preProcessArea(fcst, segmentAreas, self._expireTime, argDict)
            fcst  = self._makeProduct(fcst, segmentAreas, argDict)
            fcst = self._postProcessArea(fcst, segmentAreas, argDict)
            fraction = fractionOne
        fcst = self._postProcessProduct(fcst, argDict)
        return fcst

    def _getVariables(self, argDict):
        # Make argDict accessible
        self.__argDict = argDict
        
        # Get Definition variables
        self._definition = argDict["forecastDef"]
        for key in self._definition.keys():
            exec "self._" + key + "= self._definition[key]"

        # Get VariableList 
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"

        self._language = argDict["language"]

        # Set up information for Hazards product
        self._hazards = argDict['hazards']
        self._combinations = argDict["combinations"]
        return None

    def _determineTimeRanges(self, argDict):
        # Set up the time range for 0-240 hours
        self._timeRange = self.createTimeRange(0, 240)
        self._ddhhmmTime = self.getCurrentTime(
            argDict, "%d%H%M", shiftToLocal=0, stripLeading=0)
        self._issueTime = AbsTime.AbsTime(argDict['creationTime'])
        self._currentTime = argDict['creationTime']
        self._expireTime = self._issueTime + self._purgeTime*3600
        self._timeLabel = self.getCurrentTime(
            argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1)
        return None

    def _preProcessProduct(self, fcst, argDict):
        # Product header
        if self._areaName != "":
            self._areaName = " FOR " + self._areaName
        issuedByString = self.getIssuedByString()
        productName = self.checkTestMode(argDict, 
          self._productName + self._areaName) 

        if len(self._easPhrase) != 0:
            eas = self._easPhrase + '\n'
        else:
            eas = ''

        fcst =  fcst + self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n" +\
               eas + productName + "\n" +\
               "NATIONAL WEATHER SERVICE " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n\n" 

        fcst = fcst + "DEFAULT OVERVIEW SECTION\n"
        return fcst

    def _preProcessArea(self, fcst, segmentAreas, expireTime, argDict):
        # This is the header for an edit area combination
        areaHeader = self.makeAreaHeader(
            argDict, "", self._issueTime, expireTime,
            self._areaDictionary, None, cityDescriptor=self._cityDescriptor,
            areaList=segmentAreas, includeCities=self._includeCities,
            includeZoneNames = self._includeZoneNames,
            accurateCities = self._accurateCities)
        fcst = fcst + areaHeader      
        return fcst

    def _makeProduct(self, fcst, segmentAreas, argDict):
        argDict["language"] = self._language
        # Generate Narrative Forecast for Edit Area
        # get the hazards text

        # We only need to get headlines for the first edit area
        # in the segment since all areas in the segment have
        # the same headlines        
        editArea = segmentAreas[0]
        areaLabel = editArea
        
        headlines = self.generateProduct("Hazards", argDict, area = editArea,
                                         areaLabel=areaLabel,
                                         timeRange = self._timeRange)
        fcst = fcst + headlines
        return fcst

    def _postProcessArea(self, fcst, segmentAreas, argDict):
        return fcst + "\n\n$$\n\n"

    def _postProcessProduct(self, fcst, argDict):
        #
        # If an overview exists for this product, insert it
        #
        overview = self.finalOverviewText()
        overviewSearch = re.compile(r'DEFAULT OVERVIEW SECTION', re.DOTALL)
        fcst = overviewSearch.sub(overview, fcst)
        #
        # Added to place line feeds in the CAP tags to keep separate from CTAs

        fcst = string.replace(fcst, \
                              r"PRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.", \
                              r"\nPRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.\n")
        fcst = string.replace(fcst, "\n ","\n")
        fcst = string.replace(fcst, "&&", "\n&&\n")

        # Prevent empty Call to Action Tags
        fcst = re.sub(r'\nPRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.\s*&&\n', \
                      "", fcst)
        
        fcst = self._indentBulletText(fcst)
        
        #
        # Clean up multiple line feeds
        #

        fixMultiLF = re.compile(r'(\n\n)\n*', re.DOTALL)
        fcst = fixMultiLF.sub(r'\1', fcst)

        # finish progress meter
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst	

    def allowedHazards(self):
        return []

    # Added for DR 21194
    def _bulletDict(self):
        return []

    # Added for DR 21309
    def _bulletOrder(self):
        return []

## Replaced by 21309 code
##    def _getBullets(self, newBulletList, argDict):
##
##        ### get the bullet dictionary and split the bullets
##        bDict = self._bulletDict()
##        bLine = bDict.get(eachHazard['phen'])
##        print 20* "*" + (eachHazard['phen'])
##        bList = newBulletList.split(",")
##
##        ### initialize the bullet output
##        bullets = ""
##
##        ### loop through the bullets and format the output
##        for b in bList:
##            bullets = bullets + "* " + b + "...|* ENTER BULLET TEXT *|\n\n"
##       # bullets = bullets + "\n"
##        return bullets

    def _indentBulletText(self, prevText):

        print prevText
        ### if previous text is empty, return nothing
        if prevText is None:
            return prevText

        ###
        ### split the text
        ###
        bullets = []
        bullets = string.split(prevText, '\n\n')
        if len(bullets) <= 1:
            return prevText

        ###
        ### process the text
        ###
        outText = ""
        for b in bullets:
            ### if first character is a * we found a bullet
            if re.match("\*", b):
                ### remove line feeds
                removeLF = re.compile(r'(s*[^\n])\n([^\n])', re.DOTALL)
                bullet = removeLF.sub(r'\1 \2',b)
                ### indent code
                bullet = self.indentText(bullet, indentFirstString = '',
                                     indentNextString = '  ', maxWidth=self._lineLength,
                                     breakStrings=[" ", "..."])
                ###
                ### the "-" in the breakStrings line above is causing issues with
                ### offices that use "-20 degrees" in the text.
                ###
                outText = outText + bullet + "\n\n"
            else:  ### not a bullet, CTA text
                outText = outText + b + "\n\n"
                
        ### that's it
        print outText
        return outText
    
    # The _hazardTimePhrases method is passed a hazard key, and returns
    # time phrase wording consistent with that generated by the headline
    # algorithms in DiscretePhrases.
    #
    def hazardTimePhrases(self, hazard, argDict, prefixSpace=True):
        timeWords = self.getTimingPhrase(hazard, argDict['creationTime'])
        if prefixSpace and len(timeWords):
            timeWords = " " + timeWords   #add a leading space
        return timeWords

    #
    # The method hazardBodyText creates an attribution phrase
    #

    def hazardBodyText(self, hazardList, argDict):

        bulletProd = self._bulletProd
        hazardBodyPhrase = ''

        #
        # First, sort the hazards for this segment by importance
        #
        
        sortedHazardList = []
        for each in ['W', 'Y', 'A', 'O', 'S']:
            for eachHazard in hazardList:
                if eachHazard['sig'] == each:
                   if eachHazard not in sortedHazardList:
                       sortedHazardList.append(eachHazard)
 
      #
      # Next, break them into individual lists based on action 
      #

        newList = []
        canList = []
        expList = []
        extList = []
        conList = []
        upgList = []
        statementList = []

        for eachHazard in sortedHazardList:
            if eachHazard['sig'] in ['S']and eachHazard['phen'] in ['CF', 'LS']:
                statementList.append(eachHazard)
            elif eachHazard['act'] in ['NEW', 'EXA', 'EXB']:
                newList.append(eachHazard)
            elif eachHazard['act'] in ['CAN']:
                canList.append(eachHazard)
            elif eachHazard['act'] in ['EXP']:
                expList.append(eachHazard)
            elif eachHazard['act'] in ['EXT']:
                extList.append(eachHazard)
            elif eachHazard['act'] in ['UPG']:
                upgList.append(eachHazard)
            else:
                conList.append(eachHazard)

        #
        # Now, go through each list and build the phrases
        #

        nwsIntroUsed = 0

        #
        # This is for the new hazards
        #
        
        phraseCount = 0
        lastHdln = None
        for eachHazard in newList:
            hdln = eachHazard['hdln']
            if len(eachHazard['hdln']) == 0:
                continue   #no defined headline, skip phrase
            endTimePhrase = self.hazardTimePhrases(eachHazard, argDict)
            hazNameA = self.hazardName(eachHazard['hdln'], argDict, True)
            hazName = self.hazardName(eachHazard['hdln'], argDict, False)

            if hazName == "WINTER WEATHER ADVISORY" or hazName == "WINTER STORM WARNING":
                forPhrase = " FOR |* ENTER HAZARD TYPE *|"
            else:
                forPhrase =""

            if nwsIntroUsed == 0:
                hazardBodyPhrase = "THE NATIONAL WEATHER SERVICE IN " + self._wfoCity
                nwsIntroUsed = 1
            if phraseCount == 0:
                phraseCount = 1
                hazardBodyPhrase = hazardBodyPhrase + " HAS ISSUED " + \
                  hazNameA + forPhrase + \
                  "...WHICH IS IN EFFECT" + endTimePhrase + ". "
            elif phraseCount == 1:
                phraseCount = 2
                if hdln != lastHdln:
                    hazardBodyPhrase = hazardBodyPhrase + hazNameA + \
                      " HAS ALSO BEEN ISSUED. THIS " + hazName + forPhrase + \
                      " IS IN EFFECT" + endTimePhrase + ". "
                else:
                    hazardBodyPhrase = hazardBodyPhrase + hazNameA + \
                      " HAS ALSO BEEN ISSUED" + endTimePhrase + ". "
            else:
                hazardBodyPhrase = hazardBodyPhrase + "IN ADDITION..." + \
                  hazNameA + forPhrase + " HAS BEEN ISSUED. THIS " + hazName + \
                  " IS IN EFFECT" + endTimePhrase + ". "
            lastHdln = hdln                                         
            
        #
        # This is for the can hazards
        #
        
        for eachHazard in canList:
            if len(eachHazard['hdln']) == 0:
                continue   #no defined headline, skip phrase
            hazName = self.hazardName(eachHazard['hdln'], argDict, False)
            if nwsIntroUsed == 0:
                hazardBodyPhrase = "THE NATIONAL WEATHER SERVICE IN " +\
                  self._wfoCity
                nwsIntroUsed = 1
                hazardBodyPhrase = hazardBodyPhrase + \
                 " HAS CANCELLED THE " + hazName + ". "
            else:
                hazardBodyPhrase = hazardBodyPhrase + "THE " + hazName + \
                  " HAS BEEN CANCELLED. "

        #
        # This is for the exp hazards
        #
        
        phraseCount = 0
        for eachHazard in expList:
            if len(eachHazard['hdln']) == 0:
                continue   #no defined headline, skip phrase
            if self._bulletProd:
                continue   # No attribution for this case if it is a bullet product
            hazName = self.hazardName(eachHazard['hdln'], argDict, False)
            if eachHazard['endTime'] <= argDict['creationTime']:
                hazardBodyPhrase = hazardBodyPhrase + "THE " + hazName + \
                  " IS NO LONGER IN EFFECT. "
            else:
               expTimeCurrent = argDict['creationTime']
               timeWords = self.getTimingPhrase(eachHazard, expTimeCurrent)
                                         
               hazardBodyPhrase = hazardBodyPhrase + "THE " + hazName + \
                 " WILL EXPIRE " + timeWords + ". "

        #
        # This is for ext hazards
        #
        
        for eachHazard in extList:
            if len(eachHazard['hdln']) == 0:
                continue   #no defined headline, skip phrase
            if self._bulletProd:
                continue   # No attribution for this case if it is a bullet product
            endTimePhrase = self.hazardTimePhrases(eachHazard, argDict)
            hazName = self.hazardName(eachHazard['hdln'], argDict, False)
            
            hazardBodyPhrase = hazardBodyPhrase + "THE " + hazName + \
              " IS NOW IN EFFECT" + endTimePhrase + ". "

        #
        # This is for upgrade hazards
        #

        for eachHazard in upgList:
            if len(eachHazard['hdln']) == 0:
                continue   #no defined headline, skip phrase
            hazName = self.hazardName(eachHazard['hdln'], argDict, False)
            hazardBodyPhrase = hazardBodyPhrase + "THE " + hazName + \
              " IS NO LONGER IN EFFECT. "

        #
        # This is for con hazards
        #

        for eachHazard in conList:
            if len(eachHazard['hdln']) == 0:
                continue   #no defined headline, skip phrase
            if self._bulletProd:
                continue   # No attribution for this case if it is a bullet product
            endTimePhrase = self.hazardTimePhrases(eachHazard, argDict)
            hazNameA = self.hazardName(eachHazard['hdln'], argDict, True)
            hazardBodyPhrase = hazardBodyPhrase + hazNameA + \
              " REMAINS IN EFFECT" + endTimePhrase + ". "

        #
        # This is for statement hazards
        #

        for eachHazard in statementList:
            hazardBodyPhrase = "...|* ADD STATEMENT HEADLINE *|...\n\n"
                        
        #
        # This adds segment text
        #

        segmentText = ''
        
        #
        # Check that this segment codes to determine capture or not,
        # and frame captured text or not
        #
        incTextFlag, incFramingCodes, skipCTAs, forceCTAList = \
          self.useCaptureText(sortedHazardList)

        #
        #
        # Check that the previous text exists
        #

        
        foundCTAs = []
        for eachHazard in sortedHazardList:
            if eachHazard.has_key('prevText'):
                prevText = eachHazard['prevText']
                if eachHazard['pil'] == 'MWS':
                    startPara = 0
                else:
                    startPara = 1
                    segmentText, foundCTAs = self.cleanCapturedText(prevText,
                      startPara, addFramingCodes = False,
                      skipCTAs = skipCTAs)
                    tester = segmentText[0]
                    if tester == '*':
                        startPara = 1
                    else: 
                        startPara = 2
                segmentText, foundCTAs = self.cleanCapturedText(prevText,
                  startPara, addFramingCodes = False,
                  skipCTAs = skipCTAs)

        #
        # Check that the segment text isn't very short or blank
        #

        if len(segmentText) < 6:
            incTextFlag = 0

        # DR 21309 code addition from Middendorf (BYZ)
        #
        # Now if there is a new hazard and previous segment Text, then
        # we may have to add bullets.
        #
        if incTextFlag and bulletProd:
            for eachHazard in sortedHazardList:
                if not eachHazard.has_key('prevText'):
                    newBullets = string.split(self._bulletDict().get(eachHazard['phen']),",")
                    print "newBullets = ", newBullets
                    print "segment text is: ", segmentText
                    for bullet in newBullets:
                        if not "* " + bullet + "..." in segmentText:
                            print bullet + " not in segmentText"
                            start = self._bulletOrder().index(bullet) + 1
                            end = len(self._bulletOrder())
                            bulletFlag = 1
                            for i in range(start,end):
                                if "* " + self._bulletOrder()[i] + "..." in segmentText and bulletFlag:
                                    print "* " + self._bulletOrder()[i] + "... found!"
                                    segmentTextSplit = string.split(segmentText,"* " + self._bulletOrder()[i] + "...")
                                    segmentText = string.join(segmentTextSplit,"* " + bullet + \
                                                              "...|* ENTER BULLET TEXT *|\n\n* " + self._bulletOrder()[i] + "...")
                                    bulletFlag = 0
                            if bulletFlag:
                                print "appending to bottom list of bullets!"
                                segmentTextSplit = string.split(segmentText,"PRECAUTIONARY/PREPAREDNESS ACTIONS...")
                                segmentText = "\n" + string.join(segmentTextSplit,"* " + bullet + \
                                                                   "...|* ENTER BULLET TEXT *|\n\nPRECAUTIONARY/PREPAREDNESS ACTIONS...")
                                bulletFlag = 0
        #
        # Now if there is a can/exp hazard and previous segment Text, then
        # we may have to remove bullets.
        #

        if incTextFlag and bulletProd:
            # First make list of bullets that we need to keep.
            keepBulletList = []
            for eachHazard in sortedHazardList:
                if eachHazard['act'] not in ["CAN","EXP"]:
                    saveBullets = string.split(self._bulletDict().get(eachHazard['phen']),",")
                    for saveBullet in saveBullets:
                        if saveBullet not in keepBulletList:
                            keepBulletList.append(saveBullet)
            # Now determine which bullets we have to remove.
            removeBulletList = []
            for eachHazard in sortedHazardList:
                if eachHazard['act'] in ["CAN","EXP"]:
                    canBullets = string.split(self._bulletDict().get(eachHazard['phen']),",")
                    for canBullet in canBullets:
                        if canBullet not in keepBulletList and canBullet not in removeBulletList:
                            removeBulletList.append(canBullet)
            print "hazardBodyText info: keepBulletList: ",keepBulletList
            print "hazardBodyText info: removeBulletList: ",removeBulletList
            # Finally remove the bullets no longer needed.
            PRECAUTION = "PRECAUTIONARY/PREPAREDNESS ACTIONS..."
            for bullet in removeBulletList:
                segmentTextSplit = string.split(segmentText,"* " + bullet + "...")
                print "segmentTextSplit is ", segmentTextSplit 
                if len(segmentTextSplit) > 1:
                    segmentTextSplit2 = string.split(segmentTextSplit[1],"*",1)
                    if len(segmentTextSplit2) == 2:
                        segmentTextSplit[1] = "*" + segmentTextSplit2[1]
                    else:
                        segmentTextSplit2 = string.split(segmentTextSplit[1], \
                                                         PRECAUTION, 1)
                        if len(segmentTextSplit2) == 2:
                            segmentTextSplit[1] = PRECAUTION + segmentTextSplit2[1]
                    segmentText = string.join(segmentTextSplit,"")

            if removeBulletList != []:
                segmentText = "|*\n" + segmentText + "*|"

        #
        # If segment passes the above checks, add the text
        #

        print "hazardBodyText info: incTextFlag: ",incTextFlag
        if incTextFlag:
            print "hazardBodyText info: segmentText: ",segmentText
            hazardBodyPhrase = hazardBodyPhrase + "\n\n" + \
              segmentText + '\n\n'

        elif bulletProd:
            bulletFlag = 0
            if eachHazard['act'] == 'CAN':
                hazardBodyPhrase = hazardBodyPhrase + \
                  "\n\n|* WRAP-UP TEXT GOES HERE *|.\n"
            elif eachHazard['act'] == 'EXP':
                hazardBodyPhrase = hazardBodyPhrase + \
                  "\n\n|* WRAP-UP TEXT GOES HERE *|.\n"
            else:
                bulletFlag = 1
##            print "bulletFlag is: ",bulletFlag
            if bulletFlag:
                newBulletList = []
                bullets = ""
                for eachHazard in sortedHazardList:
                ### get the default bullets for all hazards from the bullet diction
                    newBullets = string.split(self._bulletDict().get(eachHazard['phen']),",")
                    for newBullet in newBullets:
                        if newBullet not in newBulletList:
                            newBulletList.append(newBullet)
                print "my bullets are: ", newBulletList
         ###   Determine the correct order for all bullets       
                bulletOrder = self._bulletOrder()
                staticBulletOrder = self._bulletOrder()
                for bullet in staticBulletOrder:
                    print "correct bullet order should be: ", bulletOrder
                    if bullet not in newBulletList:
                        bulletOrder.remove(bullet)
                print "reordered bullets are: ", bulletOrder
                for b in bulletOrder:
                    bullets = bullets + "* " + b + "...|* ENTER BULLET TEXT *|\n\n"

                hazardBodyPhrase = hazardBodyPhrase + "\n\n" + bullets

        # If segment doesn't pass the checks, put in framing codes
        else:
            hazardBodyPhrase = hazardBodyPhrase + \
                "\n\n|* STATEMENT TEXT GOES HERE *|.\n\n"

        # End code for DR 21310

        #
        # This adds the call to action statements. This is only performed
        # if the segment is 'NEW' or if the previous text has been discarded
        # due to a CAN/EXP/UPG segment
        #

        # remove items from forceCTAList if they exist in foundCTAs. Note
        # that the formats of these lists are different, thus this code
        # is more complicated
        for ent in foundCTAs:
            #only process CTAs that are vtec phen/sig based
            if ent.find('.') == 2:
                phensig = (ent[0:2], ent[3])   #phen.sig
                if phensig in forceCTAList:
                    del forceCTAList[forceCTAList.index(phensig)]

        hazardBodyPhrase = hazardBodyPhrase + '\n\n'
        ctas = []
        for (phen,sig) in forceCTAList:
            hazardPhenSig = phen + "." + sig
            cta = self.defaultCTA(hazardPhenSig)
            if cta not in ctas:
                ctas.append(cta)

        if len(ctas) > 0:
            hazardBodyPhrase = hazardBodyPhrase + \
                               'PRECAUTIONARY/PREPAREDNESS ACTIONS...\n\n'
            for c in ctas:
                hazardBodyPhrase = hazardBodyPhrase +  c + '\n\n'
            hazardBodyPhrase = hazardBodyPhrase + '&&\n\n'

        # Make sure there is only one CAP tag pairs
        hazardBodyPhrase = re.sub(r'&&\s*PRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.\n', \
                                  "", hazardBodyPhrase)

        return hazardBodyPhrase 

    def finalOverviewText(self):
        #if didn't calculate any, use the default
        if len(self.__overviewText) == 0:

            if self._includeOverviewHeadline:
                overviewHeadline = "...|*OVERVIEW HEADLINE " + \
                  "(must edit)*|...\n\n"
            else:
                overviewHeadline = ""

            if self._includeOverview:
                overviewBody = ".|*OVERVIEW (must edit)*|.\n\n"
            else:
                overviewBody = ""

            #assemble the lines
            overview = overviewHeadline + overviewBody
            return overview

        else:
            return self.__overviewText

    def overviewText(self, hazardList, pil):

        #
        # This method finds an overview in the previous product
        #
        
        overview = ""
        for each in hazardList:
            if (each.has_key('prevOverviewText') and
                each.has_key('pil') and
                each.has_key('endTime') and
                each.has_key('act')):
                if (each['pil'] == pil and
                  each['endTime'] > self._currentTime and
                  each['act'] not in ['CAN', 'EXP']):
                    overview = each['prevOverviewText']
                    self.__overviewText, dummy = self.cleanCapturedText(
                      overview, 0)
                    break

    def useCaptureText(self, hazardList):
        #Based on the hazardlist, returns a tuple indicating:
        # (inc capture text, inc framing codes, skip CTAs, forceCTAList)
        # 
        # For the values to be considered, the 'hdln' value must be 
        # present in the list, or it needs to be a Statement (sig="S")
        cans = ['CAN','UPG','EXP']
        acts = ['NEW','EXT','EXA','EXB','CON']
        foundACTS = 0
        foundCANS = 0
        foundSig = []
        for eh in hazardList:
            if eh['act'] in acts and (len(eh['hdln']) or eh['sig'] == 'S'):
                foundACTS = 1
            if eh['act'] in cans and (len(eh['hdln']) or eh['sig'] == 'S'):
                foundCANS = 1
            if eh['sig'] not in foundSig:
                foundSig.append(eh['sig'])
       
        includeFrameCodes = 0
        includeText = 1
        skipCTAs = 0
        forceCTAList = []

        # all actions are in CAN, UPG, EXP only (don't include text)
        if foundCANS and not foundACTS:
            if 'S' in foundSig and len(foundSig) == 1:   #only S
                includeFrameCodes = 1  #capture text, but frame it
            else: 
                includeText = 0  #end of non statement

        # something in CANS and something in acts (frame it, include text)
        elif foundCANS and foundACTS:
            includeFrameCodes = 1
            skipCTAs = 1
            for eh in hazardList:
                if eh['act'] in acts and \
                  (eh['phen'], eh['sig']) not in forceCTAList and \
                  len(eh['hdln']):
                    forceCTAList.append((eh['phen'], eh['sig']))

        #everything in active entries, captured text is used, but still
        # need to handle the "NEW" entries.
        else:
            for eh in hazardList:
                if eh['act'] in ['NEW'] and len(eh['hdln']):
                    forceCTAList.append((eh['phen'], eh['sig']))

        return (includeText, includeFrameCodes, skipCTAs, forceCTAList)

             

    def cleanCapturedText(self, text, paragraphs, addFramingCodes = False,
      skipCTAs = False):
        #
        # This method takes a block of text, wraps it preserving blank lines,
        # then returns the part after 'paragaphs'. So, if paragraphs is 0, it
        # returns the whole thing, if it's 2, it retunrs paragraphs 2 -> end, etc.
        # Headlines are always removed.
        # Framing codes are added if specified.
        #
        paras = self.convertSingleParas(text)  #single paragraphs

        # keep track of any call to actions found
        foundCTAs = []

        # Process the paragraphs, keep only the interested ones
        paraCount = 0
        processedText = ''
        for eachPara in paras:
            if paraCount >= paragraphs:
               found = self.ctasFound(eachPara)  #get list of ctas found
               if skipCTAs and len(found):
                   pass
               else:
                   processedText = processedText + eachPara + '\n\n'
                   #keep track of remaining CTAs in processed text
                   for f in found:
                       if f not in foundCTAs:
                           foundCTAs.append(f)
            if eachPara.find('...') == 0:
               pass   #ignore headlines
            paraCount = paraCount + 1

        # Add framing codes
        if addFramingCodes:
            processedText = processedText.rstrip()
            processedText = "|*\n" + processedText + "*|\n"

        # Wrap
        processedText = self.endline(processedText, 
          linelength=self._lineLength, breakStr=[" ", "-", "..."])


        return processedText, foundCTAs

    def decodeBulletedText(self, prevText):
        # returns the bullet paragraph text or None, returns the
        # regular text after the bullets.  The afterText is text up to
        # the next bullet or up to "THE NATIONAL WEATHER SERVICE". Note
        # that this only correctly handles the 1st set of entries in 
        # a segment, thus double events will only decode the first set
        # of bullets and text. The multipleRecords is set to 1 in the
        # event that there are multiple sets of bullets. In this case
        # only the 1st set was captured/decoded.
        # (hazard, time, basis, impact, afterText, multipleRecords)
        if prevText is None:
            return (None, None, None, None, None, None)

        # find the bullets
        bullets = []
        buf = prevText.split('\n\n* ')
        if len(buf) <= 1:
            return (None, None, None, None, None, None)

        multRecords = 0  #indicator of multiple sets of bullets

        for x in xrange(len(buf)):
            if x == 0:
                continue   #headlines and text before the bullets
            bullets.append(buf[x])

        # find only the bulleted text, defined by the double line feed term.
        # of the text
        regText = ""   #regular text after bullets
        for x in xrange(1, len(bullets)):
            index = bullets[x].find('\n\n')
            if index != -1:
                regText = bullets[x][index+2:]
                bullets[x] = bullets[x][0:index]  #eliminate after bullet text
                if len(bullets) > x+2:   #more bullets are present
                    multRecords = 1
                bullets = bullets[0:x+1]  #only interested in these bullets
                break
                
        # regular text is the remainder of the text.  However we only
        # want text from the last in the series of bullets to the 
        # beginning of any next NWS phrase. 
        lines = regText.split('\n')
        for x in xrange(len(lines)):
            if lines[x].find('THE NATIONAL WEATHER SERVICE') == 0:
                lines = lines[0:x]  #eliminate following lines
                break
        regText = ("\n").join(lines)

        # now clean up the text
        for x in xrange(len(bullets)):
            bullets[x] = string.replace(bullets[x],'\n',' ')
        removeLF = re.compile(r'(s*[^\n])\n([^\n])', re.DOTALL)
        regText = removeLF.sub(r'\1 \2',regText)

        # extract out each section for returning the values
        if len(bullets) >= 1:
            hazard = bullets[0]
        else:
            hazard = None
        if len(bullets) >= 2:
            time = bullets[1]
        else:
            time = None
        if len(bullets) >= 3:
            basis = bullets[2]
        else:
            basis = None
        if len(bullets) >= 4:
            impact = bullets[3]
        else:
            impact = None
        if len(regText) == 0:
            regText = None  #no regular text after bullets

        return (hazard, time, basis, impact, regText, multRecords)

    def substituteBulletedText(self, capText, defaultText, frameit="Never"):
        #returns a properly formatted bulleted text based on
        #the capText variable.  If capText is None or 0 length, then
        #the default text is used.  frameit can be "Never", in which
        #nothing is wrapped in framing codes, "Always" in which the 
        #text (default or cap) is wrapped in framing codes, or 
        #DefaultOnly" in which just the default text is wrapped.
        if capText is not None and len(capText):
            textToUse = capText
            if frameit == "Always":
                textToUse = "|* " + textToUse + " *|"
        else:
            textToUse = defaultText
            if frameit == "Always" or frameit == "DefaultOnly":
                textToUse = "|* " + textToUse + " *|"

        # add bullet codes
        textToUse = "* " + textToUse

        # format it
        return self.indentText(textToUse, indentFirstString = '',
          indentNextString = '  ', maxWidth=self._lineLength,
          breakStrings=[" ", "-", "..."])

        
    def convertSingleParas(self, text):
        #returns a list of paragraphs based on the input text.
        lf = re.compile(r'(s*[^\n])\n([^\n])', re.DOTALL)
        ptext = lf.sub(r'\1 \2', text)
        ptext = ptext.replace('\n\n', '\n')
        paragraphs = ptext.split('\n')
        return paragraphs

    def ctasFound(self, text):
        #returns types of ctas found. The identifier is the pil (e.g., ZFP), 
        #phen/sig (e.g., DU.Y), or GENERIC. Uses the CallToAction definitions.

        #convert text to single paragraphs
        paragraphs = self.convertSingleParas(text)
        for x in xrange(len(paragraphs)):
            paragraphs[x] = string.replace(paragraphs[x],' ','')

        #make list of call to actions   (type, cta text)
        if self.__procCTA is None:
            self.__procCTA = []
            ctao = CallToActions.CallToActions()
            d = ctao.ctaDict()
            for k in d.keys():
                func = d[k]
                items = func()
                for it in items:
                    if type(it) == types.TupleType:
                        it = it[1]  #get second string which is the CTA
                    ctaParas = self.convertSingleParas(it)
                    for cta in ctaParas:
                        self.__procCTA.append((k,string.replace(cta,' ','')))
            d = ctao.ctaPilDict()
            for k in d.keys():
                func = d[k]
                items = func()
                for it in items:
                    if type(it) == types.TupleType:
                        it = it[1]  #get second string which is the CTA
                    ctaParas = self.convertSingleParas(it)
                    for cta in ctaParas:
                        self.__procCTA.append((k,string.replace(cta,' ','')))
           
            ctas = ctao.genericCTAs()
            for it in ctas:
                if type(it) == types.TupleType:
                    it = it[1]  #get second string which is the CTA
                ctaParas = self.convertSingleParas(it)
                for cta in ctaParas:
                    self.__procCTA.append(("GENERIC",
                      string.replace(cta,' ','')))

        #compare
        found = []
        for para in paragraphs:
            for (ctaType, cta) in self.__procCTA:
                ## Added following line to account for framing code issues in CTA
                cta = re.sub("\|\*.*\*\|","",cta)
                if para == cta and ctaType not in found:
                    found.append(ctaType)
        return found

        


