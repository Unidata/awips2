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
# Hazard_RFW.py
# Updated for OB9.3 by
## Brian Brong/REV, Ron Miller/OTX, Jeff Zeltwagner/NWSTC, Shannon White/FDTB
##########################################################################
import LogStream
import GenericHazards
import string, time, re, os, types, copy, sets
import CallToActions
import ModuleAccessor, StringUtils

class TextProduct(GenericHazards.TextProduct):
    VariableList = [
        (("Select RFW Type", "rfwType"), [], "check", []),
        (("Source for Headline and \nAffected Area Bullet", "elevationSource"),
              "Grids", "radio", ["Grids", "Previous Text"]),
            ]
    
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "BaselineHazard_RFW_<MultiPil> (FireWx Watch/Warning)"  

    Definition["defaultEditAreas"] = "EditAreas_FireWx_<site>_<MultiPil>"
    Definition["mapNameForCombinations"] = "FireWxZones_<site>" # Map background for creating Combinations

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
                                           "AKZ215","AKZ216","AKZ217"]

    # Header configuration items
    Definition["productName"] = "URGENT - FIRE WEATHER MESSAGE"  # name of product
    Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
    Definition["wmoID"] = "<wmoID>"        # WMO ID
    Definition["pil"] = "<pil>"          # product pil
    #Definition["areaName"] = "STATENAME"  # Name of state, such as "GEORGIA"
    Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
    Definition["wfoCity"] = "<wfoCity>"       # WFO Name as it should appear in a text product
    Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
    Definition["outputFile"] =  "{prddir}/TEXT/RFW_<MultiPil>.txt"

    # OPTIONAL CONFIGURATION ITEMS
    #Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
    #Definition["displayOutputDialog"] = 0  # If 1 will display results when finished
    #Definition["debug"] = 1
    #Definition["headlineEditAreaGroup"] = "Zones" # Name of EditAreaGroup for sampling headlines
    
    Definition["purgeTime"] = 8        # Maximum hours for expireTime from issueTime
    Definition["includeCities"] = 0    # Cities included in area header
    Definition["accurateCities"] = 0  # If 1, cities are based on grids;
                                      # otherwise full list is included
    Definition["cityLocation"] = "CityLocation" # City lat/lon dictionary to use
    Definition["cityDescriptor"] = "INCLUDING THE CITIES OF"
    Definition["includeZoneNames"] = 1 # Zone names will be included in the area header
    Definition["includeIssueTime"] = 1   # This should be set to zero for products
                                       # that do not include a time lime below the UGC
    #Definition["easPhrase"] = ""      # Optional EAS phrase to be include in product header        
    Definition["lineLength"] = 66
    Definition["includeOverviewHeadline"] = 1   #If 1, the overview header is templated
    Definition["includeOverview"] = 1   #If 1, the overview section is templated
    Definition["bulletProd"] = 1   #If 1, the product is bulletted

    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)
    
    Definition["numInHeadline"] = 1
    Definition["GenericBullets"] = ["AFFECTED AREA", "WIND", "HUMIDITY", "THUNDERSTORMS", "HIGHEST THREAT", "IMPACTS"] 
    Definition["locationsBullet"] = "AFFECTED AREA"
    Definition["noNameInBullet"] = 1
    Definition["includeStateName"] = 0
    Definition["urlText"] = ""
    
    def __init__(self):
        GenericHazards.TextProduct.__init__(self)

    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
            ('FW.W', allActions, 'FireWx'),
            ('FW.A', allActions, 'FireWx')
            ]

    def _preProcessArea(self, fcst, segmentAreas, expireTime, argDict):

        # This is the header for an edit area combination
        areaHeader = self.makeAreaHeader(
            argDict, "", self._issueTime, expireTime,
            self._areaDictionary, None, cityDescriptor=self._cityDescriptor,
            areaList=segmentAreas, includeCities=self._includeCities,
            includeZoneNames = self._includeZoneNames,
            includeIssueTime = self._includeIssueTime,
            accurateCities = self._accurateCities)

        fcst = fcst + areaHeader + "\n"
        return fcst

    #
    # Overridden to allow for attribution statement
    #
    def hazardBodyText(self, hazardList, argDict): 
         
        hazardBodyPhrase = '' 

        # 
        # First, sort the hazards for this segment by importance
        # 
         
        sortedHazardList = [] 
        for each in ['W', 'Y', 'A', 'S']: 
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
            if eachHazard['act'] in ['NEW', 'EXA', 'EXB'] and eachHazard['sig'] != 'S': 
                newList.append(eachHazard) 
            elif eachHazard['act'] in ['CAN'] and eachHazard['sig'] != 'S':
                canList.append(eachHazard) 
            elif eachHazard['act'] in ['EXP'] and eachHazard['sig'] != 'S':
                expList.append(eachHazard) 
            elif eachHazard['act'] in ['EXT'] and eachHazard['sig'] != 'S':
                extList.append(eachHazard)
            elif eachHazard['act'] in ['UPG'] and eachHazard['sig'] != 'S':
                upgList.append(eachHazard) 
            elif eachHazard['sig'] != 'S': 
                conList.append(eachHazard) 
            elif eachHazard['sig'] == 'S': 
                statementList.append(eachHazard) 
 
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

            if nwsIntroUsed == 0:
                hazardBodyPhrase = "THE NATIONAL WEATHER SERVICE IN " + self._wfoCity
                nwsIntroUsed = 1
            if phraseCount == 0:
                phraseCount = 1
                hazardBodyPhrase = hazardBodyPhrase + " HAS ISSUED " + \
                  hazNameA + "...WHICH IS IN EFFECT" + endTimePhrase + ". "
            elif phraseCount == 1:
                phraseCount = 2
                if hdln != lastHdln:
                    hazardBodyPhrase = hazardBodyPhrase + hazNameA + \
                      " HAS ALSO BEEN ISSUED. THIS " + hazName + \
                      " IS IN EFFECT" + endTimePhrase + ". "
                else:
                    hazardBodyPhrase = hazardBodyPhrase + hazNameA + \
                      " HAS ALSO BEEN ISSUED" + endTimePhrase + ". "
            else:
                hazardBodyPhrase = hazardBodyPhrase + "IN ADDITION..." + \
                  hazNameA + " HAS BEEN ISSUED. THIS " + hazName + \
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
 
        #we will add in text later either by text capture or  
        #framing codes as needed
        #for eachHazard in statementList: 
        #    hazardBodyPhrase = hazardBodyPhrase + "|* STATEMENT TEXT *|."
         
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
 
                if eachHazard['sig'] == 'S': 
                    startPara = 0 
                else: 
                    startPara = 1
                    segmentText, foundCTAs = self.cleanCapturedText(prevText,
                      startPara, addFramingCodes = incFramingCodes,
                      skipCTAs = skipCTAs)
                    tester = segmentText[0]
                    if tester == '*':
                        startPara = 1
                    else: 
                        startPara = 2 
 
                segmentText, foundCTAs = self.cleanCapturedText(prevText,
                  startPara, addFramingCodes = incFramingCodes,
                  skipCTAs = skipCTAs) 
 
                # split the current bullets 
                split_bullets = segmentText.split("\n\n") 
                # reset the segmentText variable 
                segmentText = "" 
                for current_bullet in split_bullets: 
 
 
                    # if the user wants to use the gridded data for the elevation and location info...
                    if self._elevationSource == "Grids": 
                        # check to see if this is the ELEVATION bullet. If so, replace it with
                        # the elevation from the grid. 
 
                        ### Modification of Ron Miller's code to remove the check for the elevations bullet
                        ### since it could be the same as the locationsBullet.
                        ### Caused issues during testing at SLC. Thanks Linda!
                        ### bsb 5-9-10.
                        ###
                        ### locationsBullet is defined in the configuration section
                        locBullet = "* " + self._locationsBullet + "..."
                        # check to see if this is the LOCATIONS bullet.  If so, replace it with
                        # the locations from the grid.
                        if current_bullet.find(locBullet) >= 0:
                            new_locations = self._getLocationsList(self._areaDictionary,argDict,eachHazard['seg'])
                            current_bullet = self.indentText(self._locationsBullet + "..." + new_locations, \
                                indentFirstString="* ", indentNextString="  ", \
                                maxWidth=65,breakStrings=[" ","..."])

                    # for all bullets, ensure that it's indented
                    if current_bullet.find("* ") == 0:
                        current_bullet = self.indentText(current_bullet.replace("* ",""), \
                                indentFirstString="* ", indentNextString="  ", \
                                maxWidth=65,breakStrings=[" ","..."])

                    # now add the bullet back to the list.  check to make sure that it's
                    # not just a blank line.
                    if len(current_bullet) > 1:
                        segmentText = segmentText + current_bullet + "\n\n"


                #################################################################################
                #  End Ron Miller's added code
                #################################################################################

        #
        # Check that the segment text isn't very short or blank
        #

        if len(segmentText) < 6:
            incTextFlag = 0
        #
        # If segment passes the above checks, add the text 
        # 
 
        if incTextFlag: 
            hazardBodyPhrase = hazardBodyPhrase + "\n\n" + \
              segmentText + '\n\n'
# added below for DR21194 
##        else: 
##            if eachHazard['act'] != 'CAN': 
##                ### get the default bullets from the bullet dictionary
##                bullets = self._getBullets(eachHazard, argDict, self._areaDictionary)
##                hazardBodyPhrase = hazardBodyPhrase + "\n\n" + bullets
##            else: 
##                hazardBodyPhrase = hazardBodyPhrase + \
##                  "\n\n|* CANCELLATION TEXT GOES HERE *|.\n"  
        elif self._bulletProd:
            forceList = ['HW','DS','EH','EC','BZ','WS','IS']
            for h in newList:
                if h['phen'] in forceList:
                    eachHazard = h 
            if eachHazard['act'] == 'CAN':
                hazardBodyPhrase = hazardBodyPhrase + \
                  "\n\n|* WRAP-UP TEXT GOES HERE *|.\n"
            elif eachHazard['act'] == 'EXP':
                hazardBodyPhrase = hazardBodyPhrase + \
                  "\n\n|* WRAP-UP TEXT GOES HERE *|.\n"
            else:
                ### get the default bullets from the bullet dictionary
                bullets = self._getBullets(eachHazard, argDict, self._areaDictionary)
                hazardBodyPhrase = hazardBodyPhrase + "\n\n" + bullets
        else:
            hazardBodyPhrase = hazardBodyPhrase + \
                  "\n\n|* SEGMENT TEXT GOES HERE *|.\n\n"
# end addition
 
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
     
    #######################################################################
    ###
    ### Modified to create additive data for the RFW headlines.
    ### For now returns the headline from the previous product for CON,
    ### and EXT issuances
    ###
    #######################################################################

    def hazard_hook(self, tree, node, hazard, hazardPhen, hazardSig, hazardAct,
                    hazardStart, hazardEnd, hazardSeg=""):
        ### create the additive data for the fire headlines

        ### check for the source
        if self._elevationSource == "Grids":

            ### initialize the phrase
            le = " FOR "
            ### Set the phrase from the forecaster selections
            if len(self._rfwType) > 0:
                ### add the event type
                phraseDict = self._bulletDict()  ### get the phrase/bullet dictionary
                for t in self._rfwType:
                    le = le + phraseDict.get(t)[0]
                ### add zone numbers or generic location description to headline
                if self._numInHeadline == 0:
                    le = le + "FOR |* LOCATION DESCRIPTION *|"
                else:
                    le = le + self._headlineNumbers(hazard['id'])
            else:
                ### if no event type selected make a generic phrase
                if self._numInHeadline == 0:
                    le = le + "|* EVENT TYPE *| FOR |* LOCATION DESCRIPTION *|"
                else:
                    le = le + "|* EVENT TYPE *| " + self._headlineNumbers(hazard['id'])
        else:
            ### get the additive data from the previous product
            le = self._buildForPhrase(hazard)
        return le


    ###########################################################################
    ###
    ### From DiscretePhrases
    ### RJM modified this routine to pass the hazard segment number to the
    ### hazard_hook routine. Makes multiple headlines based on the hazards list
    ### and returns the lot.
    ###
    ###########################################################################

    def makeHeadlinePhrases(self, tree, node, hazardList, issuanceTime, testMode=0):
        returnStr = ""
        # make a deepcopy since we plan to mess with it.
        hList = copy.deepcopy(hazardList)

        # sort headlines in appropriate order
        if len(hList):
            if hList[0]['pil'] in ['CWF','NSH','OFF','GLF']:
                hList.sort(self.marineSortHazardAlg)
            else:
                hList.sort(self.regularSortHazardAlg)

        while len(hList) > 0:
            hazard = hList[0]

            # Can't make phrases with hazards with no 'hdln' entry
            if hazard['hdln'] == "":
                hList.remove(hazard)
                continue

            phenSig = hazard['phen'] + "." + hazard['sig']
            actionCodeList = self.getAllowedActionCodes(phenSig)

            # if the action is not in the actionCodeList, skip it
            if hazard['sig'] != "":   # it's not locally defined
                if not hazard['act'] in actionCodeList:
                    hList.remove(hazard)
                    continue

            # get the headline phrase
            hazStr = self.makeStandardPhrase(hazard, issuanceTime)
            if len(hazStr):
                # Call user hook
                localStr = self.hazard_hook(tree, node, hazard, hazard['phen'], hazard['sig'], hazard['act'],
                ########################################################################
                # modified by RJM to pass the hazard_segment to the hazard_hook routine.
                # hazard['start'], hazard['end']), "leading")
                  hazard['startTime'], hazard['endTime'], hazard['seg'])
                ########################################################################
                returnStr = returnStr + "..." + hazStr + localStr + "...\n"

            # always remove the main hazard from the list
            hList.remove(hazard)

        ###  get rid of any spaces in the ellipses
        returnStr = returnStr.replace(" ...","...")

        return returnStr

    ###########################################################################
    ###
    ### to insert zone numbers into the headlines
    ###
    ###########################################################################

    def _headlineNumbers(self, idList):
        numList = []
        ### get rid of the state ids (NVZ, CAZ) in idlist
        for i in range (len(idList)):
            numList.append(idList[i].replace(idList[i][:3], ''))
        ### sort for increasing order    
        numList.sort()
        ### initialize the zone number list
        if len(numList) > 1:
            numStr = "FOR FIRE WEATHER ZONES "
        else:
            numStr = "FOR FIRE WEATHER ZONE "

        i = 0
        for i in range (len(numList)):
            if (len(numList) - i) == 1: ### one entry or last entry in list
                return numStr + numList[i]
            elif (len(numList) - i) > 2: ### more than three zones, and/or last zone in list
                numStr = numStr + numList[i] + "..."
            elif (len(numList) - i) > 1: ### next to last zone in list
                numStr = numStr + numList[i] + " AND "

        return numStr


    ###########################################################################
    ###
    ### inserted to grab the zone name and number and insert into the locations
    ### bullet
    ###
    ###########################################################################

    def _getLocationsList(self, areaDictionary, argDict, seg):

        # Access the UGC information for the area(s) if available
        accessor = ModuleAccessor.ModuleAccessor()
        areaDict = accessor.variable(areaDictionary, "AreaDictionary")
        areaList = argDict['segmentAreas']
        ugcList = []
        zoneNameList = []
        stateList = []
        nameString = ""

        #  Cycle through each zone in this segment
        for areaName in areaList:
            if areaName in areaDict.keys():
                if areaDict.has_key(areaName):
                    entry = areaDict[areaName]
                else:
                    entry = {}
                    LogStream.logProblem(\
                      "AreaDictionary missing definition for [" + \
                      areaName + "].")
                if entry.has_key('ugcName'):
                    ugcName = entry['ugcName']
                else:
                    ugcName = areaName  #missing UGCname
                    LogStream.logProblem(\
                      "AreaDictionary missing ugcName definition for [" + \
                      areaName + "].")
                if entry.has_key('ugcCode'):
                    ugcCode = entry['ugcCode']
                else:
                    ugcCode = areaName  #missing UGCcode
                    LogStream.logProblem(\
                      "AreaDictionary missing ugcCode definition for [" + \
                      areaName + "].")
                if entry.has_key('fullStateName'):
                    ugcState = entry['fullStateName']
                else:
                    ugcState = areaName  #missing fullStateName
                    LogStream.logEvent(\
                      "AreaDictionary missing fullStateName definition for [" + \
                      areaName + "].")
                if ugcName not in ugcList:
                    ugcList.append((ugcState, ugcName, ugcCode[3:]))
                if ugcState not in stateList:
                    stateList.append(ugcState)


        ### sort ugclist by state
        ugcList.sort()
        stateList.sort()

        ### check the length of stateList for multiple states
        if len(stateList) <= 1:  ### only one state

            ### include state name
            if self._includeStateName == 1:
                nameString = nameString + "IN " + stateList[0] + "..."

            ### sort based on zone number
            ugcList = sorted(ugcList, key=lambda ugc: ugc[2])

            if self._noNameInBullet == 0:  ### include zone names and numbers
                for i in range (len(ugcList)):
                    if (len(ugcList) - i) > 1:
                        nameString = nameString + "FIRE WEATHER ZONE " + ugcList[i][2] + " " + ugcList[i][1] + "..."    
                    else:
                        nameString = nameString + "FIRE WEATHER ZONE " + ugcList[i][2] + " " + ugcList[i][1] + "."
            else: ### include zone numbers 
                if len(ugcList) > 1:
                    nameString = nameString + "FIRE WEATHER ZONES "
                else:
                    nameString = nameString + "FIRE WEATHER ZONE "

                for i in range (len(ugcList)):
                    if (len(ugcList) - i) == 1: ### one entry or last entry in list
                        nameString = nameString + ugcList[i][2] + "."
                    elif (len(ugcList) - i) > 2: ### more than three zones, and/or last zone in list
                        nameString = nameString + ugcList[i][2] + " " + "..."
                    elif (len(ugcList) - i) == 2: ### next to last zone in list
                        nameString = nameString + ugcList[i][2] + " " + " AND "
        else: ### more than one state

            for state in stateList:

                ### include state name
                if self._includeStateName == 1:
                    nameString = nameString + "IN " + state + "..."

                newList = []  ### split up ugcList for each state.
                for st, name, num in ugcList:
                    if st == state:
                        newList.append((num, name))

                ### sort for zone numbers
                newList.sort()

                if self._noNameInBullet == 0:  ### include zone names
                    for i in range (len(newList)):
                        if (len(newList) - i) > 1:
                            nameString = nameString + "FIRE WEATHER ZONE " + newList[i][0] + " " + newList[i][1] + "..."
                        else:
                            nameString = nameString + "FIRE WEATHER ZONE " + newList[i][0] + " " + newList[i][1] + ". "
                else: ### don't include zone names
                    if len(newList) > 1:
                        nameString = nameString + "FIRE WEATHER ZONES "
                    else:
                        nameString = nameString + "FIRE WEATHER ZONE "

                    for i in range (len(newList)):
                        if (len(newList) - i) == 1: ### one entry or last entry in list
                            nameString = nameString + newList[i][0] + ". "
                        elif (len(newList) - i) > 2: ### more than three zones, and/or last zone in list
                            nameString = nameString+ newList[i][0] + " " + "..."
                        elif (len(newList) - i) == 2: ### next to last zone in list
                            nameString = nameString + newList[i][0] + " " + " AND "

        ###  get rid of any spaces in the ellipses
        nameString = nameString.replace("... ","...")
        nameString = nameString.replace(" ...","...")

        return nameString

    #######################################################################
    ###
    ### Override the GenericHazards method for special RFW case
    ###
    #######################################################################

    def _getBullets(self, eachHazard, argDict, areaDictionary):

        ###
        ### set up the bullet list
        ###
        bList = []

        ### get the list from the GUI if the forecaster entered anything
        if len(self._rfwType) > 0:
            for b in self._rfwType:
                dict = self._bulletDict()
                bList = bList + dict.get(b)[1]

            bList.append("IMPACTS")

        ### get the default configured list
        else:
            ### Use GenericBullets defined locally to throw in some generic bullets
            bList = self._GenericBullets

        ### remove any duplicate entries in the bList
        ### removeDups is in CommonUtils
        bList = self.removeDups(bList)

        ### initialize the bullet output
        bullets = ""

        ### loop through the bullets and format the output
        for b in bList:
            if b == self._locationsBullet:
                locations = self._getLocationsList(areaDictionary, argDict, eachHazard['seg'])
                bullets = bullets + StringUtils.StringUtils().indentText(b+"..."+locations, \
                        indentFirstString="* ", indentNextString="  ", \
                        maxWidth=65,breakStrings=[" ","..."]) + "\n\n"
            elif b == "EXTREME GRASSLAND FIRE DANGER":
                bullets = bullets + "* " + b + "...IS FORECAST.\n\n"
 
            elif b == "HIGHEST THREAT": 
                bullets = bullets + "|* * " + b + "...IS LOCATED (optional bullet)*|\n\n"
 
            elif b == "IMPACTS":
                bullets = bullets + "* " + b + "...ANY FIRES THAT DEVELOP WILL LIKELY SPREAD RAPIDLY."
                bullets = bullets + " OUTDOOR BURNING IS NOT RECOMMENDED.\n\n"
 
 
            else:
                bullets = bullets + "* " + b + "...|* ENTER BULLET TEXT *|\n\n"

        return bullets
 
    ############################################################################################
    ###
    ### code from PQR to return additive data from previous RFW headlines
    ###
    ############################################################################################
 
    def _buildForPhrase (self, eHazard):
        forPhrase = " FOR |* ENTER REASON FOR RFW *|"
 
        if eHazard.has_key('prevText'):
            prevProduct = eHazard['prevText']

             #Get the fire wx info from the initial issuance product
             # Find the start of the text using the beginSearchString

            previousProduct = ""
            previousProduct = re.sub(r'\n([^\n])', r' \1', prevProduct)
            beginSearchString = "FOR (.*)"
            endSearchString = "\.\.\.\n"
            matchObjstart = re.search(beginSearchString, previousProduct, re.DOTALL)
            ###
            ### added the next two lines as a trap since if the search found nothing
            ### it caused the RFW formatter to fail
            if matchObjstart is None:
                return ""
            matchObjend = re.search(endSearchString, matchObjstart.group(1),re.DOTALL)
            if matchObjend != None:
                hazardTypeForWeather = matchObjstart.group(1)[:matchObjend.start(0)]
 
            else:
                    hazardTypeForWeather = ""
            forPhrase = " FOR " + hazardTypeForWeather
        return forPhrase


    def _makeProduct(self, fcst, segmentAreas, argDict):
        argDict["language"] = self._language

        #
        # This section generates the headline on the segment
        #

        # stuff argDict with the segmentAreas for DiscretePhrases
        argDict['segmentAreas'] = segmentAreas

        editArea = segmentAreas[0]
        areaLabel = editArea
        headlines = self.generateProduct("Hazards", argDict, area = editArea,
                                         areaLabel=areaLabel,
                                         timeRange = self._timeRange)
        fcst = fcst + headlines

        #
        # This section generates the attribution statements and calls-to-action
        #

        hazardsC = argDict['hazards']
        listOfHazards = hazardsC.getHazardList(segmentAreas)
        fcst = fcst + self.hazardBodyText(listOfHazards, argDict)

        #
        # If an overview exists for this product, calculate it
        #
        
        self.overviewText(listOfHazards, "RFW")

        #
        # Clean up and return
        #
        
        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "-", "..."])
        return fcst

    ###########################################################################
    ###
    ## Added code to add URL to end of product
    ###
    ###########################################################################
    
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
        # Commented out following line to prevent it from changing bullet indentation
        #fcst = string.replace(fcst, "\n ","\n")
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
        ### add the url text from the configuration section
        fcst = fcst + "\n" + self._urlText

        # finish progress meter
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst

    def cleanCapturedText(self, text, paragraphs, addFramingCodes = False, skipCTAs = False):
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
            ###
            ### added the \n to get the framing code on its own line
            ###
            processedText = "|*\n\n" + processedText + "\n*|\n\n"

        # Wrap
        processedText = self.endline(processedText, 
          linelength=self._lineLength, breakStr=[" ", "-", "..."])


        return processedText, foundCTAs
