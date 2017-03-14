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
# Hazard_FFA.py
#
##
##########################################################################
import GenericHazards

import string, time, re, os, types, copy

class TextProduct(GenericHazards.TextProduct):
    VariableList = [
             (("Flood Reason (HVTEC)", "floodReason"), 
              "ER (Excessive Rainfall)", "radio",
              ["ER (Excessive Rainfall)", 
               "SM (Snow Melt)", 
               "RS (Rain and Snow Melt)", 
               "DM (Dam or Levee Failure)",
               "DR (Upstream Dam Release)",
               "GO (Glacier-Dammed Lake Outburst)",
               "IJ (Ice Jam)", 
               "IC (Rain and/or Snow melt and/or Ice Jam)",
               "FS (Upstream Flooding plus Storm Surge)", 
               "FT (Upstream Flooding plus Tidal Effects)",
               "ET (Elevated Upstream Flow plus Tidal Effects)",
               "WT (Wind and/or Tidal Effects)",
               "OT (Other Effects)",
               "UU (Unknown)"
             ]),
             ]

    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "BaselineHazard_FFA_<MultiPil> (Flood Watch)"  # Flood Watch

    Definition["areaType"] = "ZONES"  # OR "FIPS"
    if Definition["areaType"] == "FIPS":
        Definition["defaultEditAreas"] = "EditAreas_FIPS_<site>_<MultiPil>"   #Where XXX = site id
        Definition["mapNameForCombinations"] = "FIPS_<site>"
    else:
        Definition["defaultEditAreas"] = "EditAreas_PublicZones_<site>_<MultiPil>"   #Where XXX = site id
        Definition["mapNameForCombinations"] = "Zones_<site>"

    # Header configuration items
    Definition["productName"] = "Flood Watch"  # name of product
    Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
    Definition["wmoID"] = "<wmoID>"        # WMO ID
    Definition["pil"] = "<pil>"          # product pil
    #Definition["areaName"] = "Statename"  # Name of state, such as "Georgia"
    Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
    Definition["wfoCity"] = "<wfoCity>"       # WFO Name as it should appear in a text product
    Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
    Definition["outputFile"] =  "{prddir}/TEXT/FFA_<MultiPil>.txt"

    # OPTIONAL CONFIGURATION ITEMS
    #Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
    #Definition["displayOutputDialog"] = 0  # If 1 will display results when finished
    #Definition["debug"] = 1
    #Definition["headlineEditAreaGroup"] = "Zones" # Name of EditAreaGroup for sampling headlines

    Definition["purgeTime"] = 8        # Maximum hours for expireTime from issueTime
    Definition["includeCities"] = 1    # Cities included in area header
    Definition["accurateCities"] = 0  # If 1, cities are based on grids;
                                      # otherwise full list is included
    Definition["cityLocation"] = "CityLocation" # City lat/lon dictionary to use
    Definition["cityDescriptor"] = "Including the cities of"
    Definition["includeZoneNames"] = 1 # Zone names will be included in the area header
    Definition["includeIssueTime"] = 1   # This should be set to zero for products
                                       # that do not include a time lime below the UGC
    #Definition["easPhrase"] = ""      # Optional EAS phrase to be include in product header
    Definition["lineLength"] = 66

    Definition["includeOverviewHeadline"] = 1   #If 1, the overview header is templated
    Definition["includeOverview"] = 1   #If 1, the overview section is templated
    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)
    Definition["callToAction"] = 1

    def __init__(self):
        GenericHazards.TextProduct.__init__(self)

    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
            ('FF.A', allActions, 'FlashFlood'),
            ('FA.A', allActions, 'Flood'),
               ]

    def _preProcessProduct(self, fcst, argDict):

        #
        # The code below determines if a NEW, EXA, or EXB is being created.
        # This will determine if the EAS phrase is needed.
        #

        hazards = argDict['hazards']
        segmentList = self.organizeHazards(hazards.rawAnalyzedTable())
        timeRange = self.createTimeRange(0, 240)
        listOfHazards = []
        useEAS = 0
        for each_watch_area in segmentList:
            for each_area in each_watch_area:
                areaWatchList = self._hazards.getHazardList(each_area)
                for eachHazard in areaWatchList:
                    if (eachHazard['act'] in ['NEW', 'EXA', 'EXB', 'EXT'] and
                      (eachHazard['phen'] == 'FF' or eachHazard['phen'] == 'FA')):
                        useEAS = 1
                        break



        #
        # Special Product header code to add EAS Phrase if needed
        #

        if self._areaName != "":
            self._areaName = " for " + self._areaName
        if useEAS == 1:
            easPhrase = "URGENT - IMMEDIATE BROADCAST REQUESTED\n"
        else:
            easPhrase = ""

        issuedByString = self.getIssuedByString()
        productName = self.checkTestMode(argDict, self._productName)

        s = self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n" 
        fcst =  fcst + s.upper()
               
        s = easPhrase +\
               productName + "\n" +\
               "National Weather Service " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n" + \
               self._easPhrase + "\n\n"
        fcst =  fcst + s
        
        fcst = fcst + "Default overview section\n"
        return fcst


    def _preProcessArea(self, fcst, segmentAreas, expireTime, argDict):

        #
        # This is the header for an edit area combination
        #

        editArea = segmentAreas[0]
        areaLabel = editArea
        HVTEC = "/00000.0." + self._floodReason[0:2] + \
          ".000000T0000Z.000000T0000Z.000000T0000Z.OO/"

        areaHeader = self.makeAreaHeader(
            argDict, "", self._issueTime, expireTime,
            self._areaDictionary, None, cityDescriptor=self._cityDescriptor,
            areaList=segmentAreas, includeCities=self._includeCities,
            includeZoneNames = self._includeZoneNames, includeIssueTime = self._includeIssueTime,
            hVTECString=HVTEC,
            accurateCities = self._accurateCities)

        fcst = fcst + areaHeader + '\n' 
        return fcst


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

        hazards = argDict['hazards']
        timeRange = self.createTimeRange(0, 240)
        listOfHazards = hazards.getHazardList(segmentAreas)

        attrPhrase = ""

        #
        # Check for special case where a CAN/EXP is paired with a
        # NEW/EXA/EXB/EXT
        #
        includeText, includeFrameCodes, skipCTAs, forceCTAList = \
          self.useCaptureText(listOfHazards)

        # sort the sections within the product
        listOfHazards.sort(self.sortSection)

        # find any "CAN" with non-CAN for reasons of text capture
        canHazard = None
        for eh in listOfHazards:
            if eh['act'] in ['CAN','EXP','UPG']:
                canHazard = eh
                break  #take the first one

        # Make Area Phrase
        areas = self.getGeneralAreaList(listOfHazards[0]['id'], 
          areaDictName = self._areaDictionary)
        areas = self.simplifyAreas(areas)
        areaPhrase = self.makeAreaPhrase(areas, listOfHazards[0]['id'])
        areaPhraseShort = self.makeAreaPhrase(areas, listOfHazards[0]['id'],
          True)

        #process each part of the section
        for hazard in listOfHazards:
            if hazard['act'] in ['CAN','EXP','UPG']:
                phrase = self.makeSection(hazard, None, areaPhraseShort, 
                  argDict)
            else:
                phrase = self.makeSection(hazard, canHazard, areaPhrase, 
                  argDict)
            fcst = fcst + phrase + "\n\n"


        self.overviewText(listOfHazards, "FFA")

        fcst = self.endline(fcst, linelength=self._lineLength, 
          breakStr=[" ", "...", "-"])
        return fcst


    def simplifyAreas(self, areas):
        #simplifies the area phrases by combining subareas, returns the
        #areas.

        # rules: 1) multiple states and multiple direction terms in a state,
        # only mention the state.  2) Multiple states but single directional
        # term in a state, include the directional term.  3) Single state,
        # include the directional terms.

        #determine how many states, and how many areas within each state
        stateDict = {}   #key is state, value is count of portions of state
        for state, partOfState, names in areas:
            if stateDict.has_key(state):
                stateDict[state] = stateDict[state] + 1
            else:
                stateDict[state] = 1
   
        # if single state, include all directional terms
        if len(stateDict.keys()) < 2:
            return areas   #unchanged

        # multiple states - multiple direction terms in a state
        # keep states sorted in same order as present. 
        out = []
        for state, partOfState, names in areas:
            if stateDict[state] == 1:
                names.sort()
                out.append((state, partOfState, names))
            elif len(out) == 0 or state != out[-1][0]:  #new state 
                out.append((state, "", names))   #leave out partOfState
            else:    #same state as before
                nmeList = out[-1][2]
                for n in names:
                    nmeList.append(n)
                nmeList.sort() 
              
        return out                


                        
        
    def makeAreaPhrase(self, areaGroups, areas, generalOnly=False):
        #creates the area phrase based on the groups of areas (areaGroups, 
        #such as NE Pennsylvania), and the areas (areas), individual zones.
        #returns the area phrase.  Area phrase does not have a terminating
        #period.
        areaGroupLen = len(areaGroups)
        if areaGroupLen == 1:
            areaPhrase = "a portion of "
        else:
            areaPhrase = "portions of "

        #parts of the states
        areaList = []
        for state, partOfState, names in areaGroups:
            if partOfState == '' or partOfState == ' ':
                areaList.append(state)
            else:
                areaList.append(partOfState + " " + state)
        
        areaPhrase += self.punctuateList(areaList)

        #including phrase, have to count what we have
        d = {'Independent city': ("Independent city", "Independent cities"),
             'Parish': ("Parish", "Parishes"),
             'County': ("County", "Counties"),
             'Zone':   ("Area", "Areas")  }
        icCnt = 0
        parishCnt = 0
        zoneCnt = 0
        countyCnt = 0
        for state, partOfState, names in areaGroups:
            for name,nameType in names:
                if nameType == "zone":
                    zoneCnt = zoneCnt + 1
                elif nameType == "county":
                    countyCnt = countyCnt + 1
                elif nameType == "independent city":
                    icCnt = icCnt + 1
                elif nameType == "parish":
                    parishCnt = parishCnt + 1

        incPhrases = []
        if zoneCnt == 1:
            incPhrases.append("area")
        elif zoneCnt > 1:
            incPhrases.append("areas")
        if countyCnt == 1:
            incPhrases.append("county")
        elif countyCnt > 1:
            incPhrases.append("counties")
        if icCnt == 1:
            incPhrases.append("independent city")
        elif icCnt > 1:
            incPhrases.append("independent cities")
        if parishCnt == 1:
            incPhrases.append("parish")
        elif parishCnt > 1:
            incPhrases.append("parishes")
        incPhrase = self.punctuateList(incPhrases)

        if generalOnly:
            return areaPhrase

             
        areaPhrase += ", including the following " + incPhrase + ", "

        #list of the specific areas
        for i in xrange(len(areaGroups)):
            state, partOfState, names = areaGroups[i]
            if state == "The District of Columbia":
                areaPhrase += state
            else:
                # extract out the names
                snames = []
                for name,nameType in names:
                    snames.append(name)

                # single (don't mention state, partOfState again)
                if len(areaGroups) == 1:
                    phrase = ""
                # complex phrasing (state, partOfState, and names)
                else:
                    if i == 0:
                        phrase = "in "
                    else:
                        phrase = "In "
                    if partOfState != '' and partOfState != ' ':
                        phrase += partOfState + ' '
                    phrase += state + ", "

                phrase += self.punctuateList(snames)
                
                areaPhrase += phrase
            if i != len(areaGroups) - 1:
                areaPhrase += '. '  #another one coming, add period
                
        return areaPhrase

    def sortSection(self, r1, r2):
        #sorts the hazards in a particular order for the sections within
        #each segment.  We try to keep this in the same order as the
        #headlines order for clarity.
        return self.regularSortHazardAlg(r1, r2)

    def makeSection(self, hazard, canHazard, areaPhrase, argDict):
        #creates a section of the FFA product.  The hazard record is passed
        #in.  canHazard is any associated CAN/EXP/UPG hazard, areaPhrase is
        #the area description for the segment.

        nwsPhrase = "The National Weather Service in " + self._wfoCity + " has "

        #
        # Attribution and 1st bullet (headPhrase)
        #
        headPhrase = None
        attribution = ''

        hazName = self.hazardName(hazard['hdln'], argDict, False)
        
        if hazard['act'] == 'NEW' and len(hazard['hdln']):
            attribution = nwsPhrase + "issued a"
            headPhrase =  self.substituteBulletedText(hazName + " for " + areaPhrase + ".", None, "Never")

        elif hazard['act'] == 'CON' and len(hazard['hdln']):
            attribution = "The " + hazName + " continues for"
            headPhrase = self.substituteBulletedText(areaPhrase + ".", None, "Never")

        elif hazard['act'] == 'EXA' and len(hazard['hdln']):
            attribution = nwsPhrase + "expanded the"
            headPhrase = self.substituteBulletedText(hazName + " to include " + areaPhrase + ".", None, "Never")

        elif hazard['act'] == 'EXT' and len(hazard['hdln']):
            attribution = 'The ' + hazName + " is now in effect for" 
            headPhrase = self.substituteBulletedText(areaPhrase + ".", None, "Never")
                
        elif hazard['act'] == 'EXB' and len(hazard['hdln']):
            attribution = nwsPhrase + "expanded the"
            headPhrase = self.substituteBulletedText(hazName + " to include " + areaPhrase + ".", None, "Never")

        elif hazard['act'] == 'CAN' and len(hazard['hdln']):
            attribution = "The " + hazName + \
               " for " + areaPhrase + " has been cancelled. " + \
               "|* brief post-synopsis/summary of hydromet activity *|\n\n"

        elif hazard['act'] == 'EXP' and len(hazard['hdln']):
            expTimeCurrent = argDict['creationTime']
            if hazard['endTime'] <= expTimeCurrent:
                attribution = "The " + hazName + \
                  " for " + areaPhrase + " has expired. " + \
                  "|* brief post-synopsis/summary of hydromet activity *|"
            else:
               timeWords = self.getTimingPhrase(hazard, expTimeCurrent)
               attribution = "The " + hazName + \
                  " for " + areaPhrase + " will expire " + timeWords + \
                  ". " + \
                  "|* brief post-synopsis/summary of hydromet activity *|"
          
        #wrap it, if headPhrase, then we have bullets
        if headPhrase is not None:
#             headPhrase = self.indentText(headPhrase, indentFirstString = '',
#               indentNextString = '  ', maxWidth=self._lineLength,
#               breakStrings=[" ", "-", "..."])

            endTimePhrase = self.hazardTimePhrases(hazard, argDict, 
              prefixSpace=False)
            endTimePhrase = self.substituteBulletedText(endTimePhrase,
                  "Time is missing", "DefaultOnly")

            # 3rd bullet (basis), 4th bullet (impacts)
            if hazard['act'] == "NEW" and canHazard:
                capText = canHazard.get('prevText', None)
            else:
                capText = hazard.get('prevText', None)
            (haz, timeB, basis, impact, remainder, multRecords) = \
              self.decodeBulletedText(capText)

            defaultBasis = {
              'NEW': ("Basis for the watch", "Always"),
              'CON': ("Describe current situation", "DefaultOnly"),
              'EXT': ("Basis for extending the watch", "DefaultOnly"),
              'EXB': ("Basis for expansion of the watch", "DefaultOnly"),
              'EXA': ("Basis for expansion of the watch", "DefaultOnly"),
              }
            b = defaultBasis[hazard['act']]
            if multRecords == 0:
                basisPhrase = self.substituteBulletedText(basis, b[0], b[1])
            else:
                basisPhrase = self.substituteBulletedText(basis, b[0], "Always")

            if (hazard['act'] == "NEW" and canHazard) or multRecords:
                framing = "Always"
            else:
                framing = "DefaultOnly"
            impactsPhrase = self.substituteBulletedText(impact,
              "(optional) potential impacts of flooding", framing)

            #remainder of text
            general = ''
            addCTA = False
            ctaBodyPhrase = ''
            if remainder is not None and \
              (canHazard or hazard['act'] != "NEW"):
                if canHazard is None:
                    general = remainder  #use all
                else:
                    #frame the text, without the ctas
                    addCTA = True
                    paras = self.convertSingleParas(remainder)
                    for p in paras:
                        found = self.ctasFound(p)
                        if len(found) == 0:
                            general = general + p + '\n\n'
                    if len(general):
                        general = "|* " + general[:-2]  + " *|\n\n"    
            else:
                addCTA = True

            # add in call to actions
            if addCTA:
                key = hazard['phen'] + '.' + hazard['sig']
                cta = self.defaultCTA(key)
            else:
                cta = ''

            if len(cta) > 1:
              ctaBodyPhrase ="\n\nPRECAUTIONARY/PREPAREDNESS ACTIONS...\n\n" + \
                cta + \
                "\n\n&&\n\n"
            else:
              ctaBodyPhrase = cta

            if ctaBodyPhrase.find('PRECAUTIONARY/PREPAREDNESS ACTIONS...') != -1 and  \
               attribution.find('&&') != -1:
                 attribution = attribution.replace('&&','')
                 ctaBodyPhrase = ctaBodyPhrase.replace('PRECAUTIONARY/PREPAREDNESS ACTIONS...','')

            attrPhrase = attribution + '\n\n' + headPhrase + '\n' + \
              endTimePhrase + '\n' + basisPhrase + '\n' + impactsPhrase + \
              '\n' + general + ctaBodyPhrase + '\n'

        #no headPhrase (EXP or CAN alone)
        else:
            attrPhrase = attribution

        return attrPhrase



