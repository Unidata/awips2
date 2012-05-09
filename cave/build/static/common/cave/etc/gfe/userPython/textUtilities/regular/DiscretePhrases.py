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
# DiscretePhrases.py
# Methods for producing text forecast from SampleAnalysis statistics.
#
# Author: hansen
# ----------------------------------------------------------------------------

import PhraseBuilder
import ModuleAccessor
import types, copy, time, string, sets, os
import SampleAnalysis
import TimeRange, AbsTime

class DiscretePhrases(PhraseBuilder.PhraseBuilder):
    def __init__(self):    
        PhraseBuilder.PhraseBuilder.__init__(self)

    ### Local non-VTEC headlines.
    # To sample the Hazards grid and produce locally generated headlines
    # independent of the VTEC Headlines structure, follow these steps:
    #
    # 1. Put an "allowedHeadlines" method into your product with the
    #    same format as the "allowedHazards" method.
    #
    # 2. Generate the headlines using "generateProduct" in, for example, the
    #    _preProcessArea method:

    #    headlines = self.generateProduct("Headlines", argDict, area = editArea,
    #                                     areaLabel=areaLabel,
    #                                     timeRange = self._timeRange)
    #    fcst = fcst + headlines
    #
    # 3. If desired, override "headlinesTiming" to adjust or remove the time descriptors
    #    for the headline.
    #
    
    def headlinesTiming(self, tree, node, key, timeRange, areaLabel, issuanceTime):
        # Return
        #  "startPhraseType" and "endPhraseType"
        #   Each can be one of these phraseTypes: 
        #      "EXPLICIT" will return words such as "5 PM"
        #      "FUZZY4" will return words such as "THIS EVENING"
        #      "DAY_NIGHT_ONLY" use only weekday or weekday "NIGHT" e.g.
        #         "SUNDAY" or "SUNDAY NIGHT" or "TODAY" or "TONIGHT"
        #         Note: You will probably want to set both the
        #         startPhraseType and endPhraseType to DAY_NIGHT_ONLY to
        #         have this work correctly.
        #      "NONE" will result in no words
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
        startPhraseType = "FUZZY"
        endPhraseType = "FUZZY"

        #Example code
        #startTime = timeRange.startTime().unixTime()
        #if startTime <= issuanceTime + 12 * 3600:   # 12 hours past issuance
            #startPhraseType = "EXPLICIT"
        #endTime = timeRange.endTime().unixTime()
        #if endTime <= issuanceTime + 12 * 3600:   # 12 hours past issuance
            #endPhraseType = "EXPLICIT"

        #return startPhraseType, endPhraseType
        return None, None
    
    def Headlines(self): 
        return {
            "type": "component",
            "lineLength": 69,
            "methodList": [
                          self.assembleChildWords,   
                          self.wordWrap,          
                          ],
            "analysisList":[
                          ("Hazards",
                           SampleAnalysis.SampleAnalysis().discreteTimeRangesByKey),
                         ],

            "phraseList":[
                       self.headlines_phrase,
                       ],
            "autoSentence": 0,
        }
    
    def headlines_phrase(self):
        return {
            "setUpMethod": self.headlines_setUp,
            "wordMethod": self.headlines_words,
            "phraseMethods": [self.assembleSubPhrases,
                              self.postProcessPhrase,
                              ]
            }
    
    def headlines_setUp(self, tree, node):
        self.subPhraseSetUp(tree, node, [], self.scalarConnector)                           
        return self.DONE()

    def headlines_words(self, tree, node):
        "Create the phrase for local headlines from the Hazards grids"

        words = ""
        areaLabel = tree.getAreaLabel()
        headlines = tree.stats.get("Hazards", tree.getTimeRange(),
                                   areaLabel, mergeMethod = "List")
        if headlines is None:
            return self.setWords(node, "")

        # Sort the headlines by startTime
        temp = []
        for h, tr in headlines:
            temp.append((tr.startTime(), (h, tr)))
        temp.sort()
        newList = []
        for t in temp:
            newList.append(t[1])
        headlines = newList
        
        # Fetch the set of local headlines allowed for this product
        allowedHeadlines = []
        for key, allActions, cat in self.allowedHeadlines():
            allowedHeadlines.append(key)
        issuanceTime = self._issueTime.unixTime()
        
        from com.raytheon.uf.viz.core.localization import LocalizationManager
        siteId = LocalizationManager.getInstance().getSite()
        for key, tr in headlines:  # value == list of subkeys
            if key not in allowedHeadlines:
                continue

            timeDescriptor = self.headlinesTimeRange_descriptor(
                tree, node, key, tr, areaLabel, issuanceTime)
            from com.raytheon.uf.common.dataplugin.gfe.discrete import DiscreteKey
            headlineWords = DiscreteKey.discreteDefinition(siteId).keyDesc(
                "Hazards" + "_SFC", key)
            if headlineWords == "":    #  Don't process the "<None>" key
                continue
            hookWords = self.hazard_hook(tree, node, key, "", "",tr.startTime(), tr.endTime())
            headlineWords = self.convertToLower(headlineWords)
            headlinePhrase = "..." + headlineWords + timeDescriptor +hookWords + "...\n"
            words = words + headlinePhrase

        words = self.convertToUpper(words)
        return self.setWords(node, words)

    def headlinesTimeRange_descriptor(self, tree, node, key, tr, areaLabel, issuanceTime):
        # Return a time range descriptor for the headline
        # This method can be overridden to customize timing descriptors for
        # non-VTEC local headlines

        headlinesTiming = self.headlinesTiming(tree, node, key, tr,
          areaLabel, issuanceTime)
        if headlinesTiming is None:
            return ""
        try:
            startPhraseType, endPhraseType = headlinesTiming
        except:
            # For backward compatibility -- the startBoundary argument
            # was formerly part of the headlinesTiming method
            startPhraseType, endPhraseType, startBoundary = headlinesTiming
        startTime = tr.startTime().unixTime()
        endTime = tr.endTime().unixTime()
        tree.combinations = self._combinations
        areaList = self.getCurrentAreaNames(tree, areaLabel)
        hazRec = {
            'id': areaList,
            'startTime': startTime,
            'endTime': endTime,
            'act': "NEW",
            }
        if startPhraseType == "FUZZY":
            startPhraseType = "FUZZY4"
        if endPhraseType == "FUZZY":
            endPhraseType = "FUZZY4"
       
        phrase = self.getTimingPhrase(
            hazRec, issuanceTime, startPhraseType, endPhraseType)
        return " " + phrase


############################################################################################
    ### WARNING!!!!  VTEC CODE  -- DO NOT OVERRIDE ANY CODE BELOW THIS POINT!!!!!
    
    ### IF YOU USE A METHOD BELOW THIS POINT AND WANT TO ALTER IT,
    ### COPY IT TO YOUR LOCAL FILE AND RE-NAME IT. THEN OVERRIDE ANY
    ### METHODS THAT CALL IT AND USE THE NEW NAME.

    def getHazards(self, argDict, areaList):
        # This is for setting up the argDict hazards entry AFTER the TextFormatter
        # has created the Hazards Table.
        # This is necessary for products that allow the user to specify through
        # the GUI which edit areas will be sampled.
        # Set up edit areas
        editAreas = []
        for area, label in areaList:
            editAreas.append([area])       
        # Process the hazards
        import HazardsTable
        hazards = HazardsTable.HazardsTable(
                  argDict["ifpClient"], editAreas, self._pil[0:3],
                  self.filterMethod, argDict["databaseID"],
                  self._fullStationID, 
                  activeTableName = argDict['vtecActiveTable'],
                  vtecMode=argDict['vtecMode'],
                  dataMgr=argDict['dataMgr'])
        # Store hazards object for later use
        argDict["hazards"] = hazards
    
    def Hazards(self): 
        return {
            "type": "component",
            "lineLength": 66,
            "methodList": [
                          self.assembleChildWords,   
                          self.wordWrap,          
                          ],
            "analysisList":[],

            "phraseList":[
                       self.hazards_phrase,
                       ],
            "autoSentence": 0,
        }

    def hazards_phrase(self):
        return {
            "setUpMethod": self.hazards_setUp,
            "wordMethod": self.hazards_words,
            "phraseMethods": self.standard_phraseMethods(),  
            }
    
    def hazards_setUp(self, tree, node):
        self.subPhraseSetUp(tree, node, [], self.scalarConnector)

        return self.DONE()

    def hazards_words(self, tree, node):
        "Create the phrase for any watches, warnings or advisories"
        hazardsTable = self._hazards
        tree.combinations = self._combinations
        if self._combinations is None:
            areaLabel = None
        else:
            areaLabel = tree.getAreaLabel()
        
        editAreas = self.getCurrentAreaNames(tree, areaLabel)
        try:
            # Remove suffixes if necessary
            if self._editAreaSuffix is not None:
                editAreas = self.removeSuffixes(editAreas, self._editAreaSuffix)
        except:
            pass
            

        # Check for a particular entry in argDict that is inserted when
        # we're formatting hazards type products like WSW, NPW.
        argDict = tree.get("argDict")
        # look for segmentAreas in the argDict and override editAreas
        if argDict.has_key("segmentAreas"):
            editAreas = argDict['segmentAreas']  # override editAreas
            
        words = self.getHazardString(tree, node, editAreas)

        words = self.convertToUpper(words)  # convert to upper case

        return self.setWords(node, words)

    #####   VTEC methods  #####
    
    # Return just a simple list of hazards in the form phen.sig (WS.W)
    def getAllowedHazardList(self):
        allowedHazardList = self.allowedHazards()

        hazardList = []
        for h in allowedHazardList:
            if type(h) is types.TupleType:
                hazardList.append(h[0])
            else:
                hazardList.append(h)

        return hazardList

    # Return the list of action codes given the hazard, if hazard not found
    # or actions not specified, return "ALL codes"
    
    def getAllowedActionCodes(self, hazard):
        allowedHazardList = self.allowedHazards()

        for h in allowedHazardList:
            if type(h) is types.TupleType:
                if h[0] == hazard:
                    return h[1]
        return ["NEW", "EXA", "EXB", "EXT", "UPG", "CAN", "CON", "EXP"]

    # Returns the words to be used in the headline for 'act' field in the
    # specified hazard.
    def actionControlWord(self, hazard, issuanceTime):
        if not hazard.has_key('act'):
            print "Error!  No field act in hazard record."
            return "<noaction>"
            
        actionCode = hazard['act']
        if actionCode in ["NEW", "EXA", "EXB"]:
            return "in effect"
        elif actionCode == "CON":
            return "remains in effect"
        elif actionCode == "CAN":
            return "is cancelled"
        elif actionCode == "EXT":
            return "now in effect"
        elif actionCode == "EXP":
            deltaTime = issuanceTime - hazard['endTime']
            if deltaTime >= 0:
                return "has expired"
            else:
                return "will expire"
        elif actionCode == "UPG":
            return "no longer in effect"
        else:
            print actionCode, "not recognized in actionControlWord."
            return "<actionControlWord>"

    #
    # Determine the category for Hazard overrides
    #    
    def getHazardCategory(self, hazard):
        allowedHazardList = self.allowedHazards()
         
        for h in allowedHazardList:
            if h[0] == hazard:
                if len(h) == 3:
                   if type(h[2]) is types.StringType:
                       return h[2]
                elif len(h) == 4:
                   if type(h[3]) is types.StringType:
                       return h[3]

        return None
    #
    # Determine the priority of a Hazard (lower count = higher priority)
    #
    
    def getHazardImportance(self, hazard):
        allowedHazardList = self.allowedHazards()
        count = 0
        for h in allowedHazardList:
            count = count + 1
            if h[0] == hazard:
                return count

        return 1000 # no priority


    # This method uses the allowedHazards() list to determine which
    # hazardTable entry has the most important priority and removes
    # the entry or piece thereof in place.  Returns 1 if something was
    # modified and 0 otherwise
    def fixHazardConflict(self, index1, index2, hazardTable):
       
        allowedHazardList = self.getAllowedHazardList()
        phen1 = hazardTable[index1]['phen']
        phen2 = hazardTable[index2]['phen']
        sig1 = hazardTable[index1]['sig']
        sig2 = hazardTable[index2]['sig']
        act1 =  hazardTable[index1]['act']
        act2 =  hazardTable[index2]['act']
        haz1 = phen1 + "." + sig1
        haz2 = phen2 + "." + sig2
        ignoreList = ['CAN', 'EXP', 'UPG']
        if haz1 in allowedHazardList and haz2 in allowedHazardList and \
               act1 not in ignoreList and act2 not in ignoreList:
                                                            
                                               
            if (self.getHazardCategory(haz1) != self.getHazardCategory(haz2)) or \
                self.getHazardCategory(haz1) is None or \
                self.getHazardCategory(haz2) is None:
                return 0
                                                                                
        else:
            return 0  # no changes were made
                                                                   
        if self.getHazardImportance(haz1) < self.getHazardImportance(haz2):
            lowIndex = index2
            highIndex = index1
        else:
            lowIndex = index1
            highIndex = index2
        
        #
        # Added to prevent a current lower TO.A from overiding a higher SV.A
        #
        
        if hazardTable[lowIndex]['phen'] == 'SV' and \
           hazardTable[lowIndex]['sig'] == 'A' and \
           hazardTable[highIndex]['phen'] == 'TO' and \
           hazardTable[highIndex]['sig'] == 'A':
               if (int(hazardTable[lowIndex]['etn']) > int(hazardTable[highIndex]['etn']) and
                  (int(hazardTable[highIndex]['etn']) - int(hazardTable[lowIndex]['etn'])) > 50):
                   lowIndexTemp = lowIndex
                   lowIndex = highIndex
                   highIndex = lowIndexTemp
                           
        lowStart = hazardTable[lowIndex]['startTime']
        lowEnd = hazardTable[lowIndex]['endTime']
        highStart = hazardTable[highIndex]['startTime']
        highEnd = hazardTable[highIndex]['endTime']
                                                                                
        # first check to see if high pri completely covers low pri
        if highStart <= lowStart and highEnd >= lowEnd:  # remove low priority
            del hazardTable[lowIndex]
                                                                                
        # next check to see if high pri lies within low pri
        elif lowStart <= highStart and lowEnd >= highEnd:  # high pri in middle
            if lowStart < highStart:
                h = copy.deepcopy(hazardTable[lowIndex])
                # trim the early piece
                hazardTable[lowIndex]['endTime'] = highStart
                if lowEnd > highEnd:
                    # make a new end piece
                    h['startTime'] = highEnd
                    hazardTable.append(h)
            elif lowStart == highStart:
                hazardTable[lowIndex]['startTime'] = highEnd
                                                                                
        elif highEnd >= lowStart:
            hazardTable[lowIndex]['startTime'] = highEnd  # change low start
                                                                                
        elif highStart <= lowEnd:
            hazardTable[lowIndex]['endTime'] = highStart  # change low end

        return 1
    

    # This method removes all entries of the specified hazardTable that
    # are not in the allowedHazards list.
    def filterAllowedHazards(self, hazardTable):

        newTable = []
        allowedHazardList = self.getAllowedHazardList()
        
        hazStr = ""
        for i in range(len(hazardTable)):
            if hazardTable[i]['sig'] != "":   # VTEC
                hazStr = hazardTable[i]['phen'] + "." + hazardTable[i]['sig']
            else:   #non-VTEC
                hazStr = hazardTable[i]['phen']

            if hazStr in allowedHazardList:
                newTable.append(hazardTable[i])
        return newTable
                                        
    # This method searches all entries of the specified hazardTable for
    # entries matching the specified zone.  Then for each entry it finds
    # it looks for a conflicting entry in time.  If it finds one, it calls
    # fixHazardsConflict, which fixes the table and then calls itself again
    # recursively with the fixed table.  If it doesn't find one it returns
    # None.
    def filterZoneHazards(self, zone, hazardTable):
        for i in range(len(hazardTable)):
            if hazardTable[i]['id'] == zone:
                for j in range(len(hazardTable)):
                    if hazardTable[j]['id'] == zone and i != j:
                        tr1 = TimeRange.TimeRange(
                            AbsTime.AbsTime(int(hazardTable[i]['startTime'])),
                            AbsTime.AbsTime(int(hazardTable[i]['endTime'])))
                        tr2 = TimeRange.TimeRange(
                            AbsTime.AbsTime(int(hazardTable[j]['startTime'])),
                            AbsTime.AbsTime(int(hazardTable[j]['endTime'])))
                        if tr1.overlaps(tr2):
                            if self.fixHazardConflict(i, j, hazardTable):
                                self.filterZoneHazards(zone, hazardTable)
                                return None
        return None

    # Main method that drives the code to filter hazards that conflict in time.
    # Only one hazard of the same phenomenon is allowed per zone per time.
    # This method processes the table, removing any time conflicts, so the one
    # hazard per zone, time rule is adhered to.
    def filterMethod(self, hazardTable, allowedHazardsOnly=False):
        # Remove hazards not in allowedHazards list
        newTable = self.filterAllowedHazards(hazardTable)
        if allowedHazardsOnly:
            return newTable

        # get a raw list of unique edit areas
        zoneList = []
        for t in newTable:
            if t['id'] not in zoneList:
                zoneList.append(t['id'])

        for zone in zoneList:
            # Remove lower priority hazards of the same type
            self.filterZoneHazards(zone, newTable)

        return newTable


    # function returns the timing phrase to use for the area, hazard,
    # and issuance time.  Can force the type of timing phrase given the
    # stype and etype. The stype/etype may be: NONE, EXPLICIT, FUZZY4,
    # FUZZY8, or DAY_NIGHT_ONLY. Returns phrase like:
    #  FROM 4 PM MST THIS AFTERNOON THROUGH TUESDAY EVENING
    def getTimingPhrase(self, hazRec, issueTime, stype=None, etype=None):
        #Returns the timing phrase to use

        # Get the timing type
        if stype is None or etype is None:
            stype, etype = self.getTimingType(hazRec, issueTime)

        # Get the time zones for the areas
        timeZones = self.hazardTimeZones(hazRec['id'])

        # Get the starting time
        stext = []
        if type(stype) is types.MethodType:
            for tz in timeZones:
                newType, info = stype(
                    issueTime, hazRec['startTime'], tz, "start")
                if info is not None and info not in stext:
                    stext.append(info)
            stype = newType
        elif stype == "EXPLICIT":
            for tz in timeZones:
                info = self.timingWordTableEXPLICIT(issueTime, 
                  hazRec['startTime'], tz, "start")
                if info not in stext:
                    stext.append(info)
        elif stype == "FUZZY4":
            for tz in timeZones:
                info = self.timingWordTableFUZZY4(issueTime, 
                  hazRec['startTime'], tz, "start")
                if info not in stext:
                    stext.append(info)
        elif stype == "FUZZY8":
            for tz in timeZones:
                info = self.timingWordTableFUZZY8(issueTime, 
                  hazRec['startTime'], tz, "start")
                if info not in stext:
                    stext.append(info)
        elif stype == "DAY_NIGHT_ONLY":
            for tz in timeZones:
                info = self.timingWordTableDAYNIGHT(issueTime, 
                  hazRec['startTime'], tz, "start")
                if info not in stext:
                    stext.append(info)

        # Get the ending time
        etext = []
        if type(etype) is types.MethodType:
            for tz in timeZones:
                newType, info = etype(
                    issueTime, hazRec['endTime'], tz, "end")
                if info is not None and info not in etext:
                    etext.append(info)
            etype = newType
        elif etype == "EXPLICIT":
            for tz in timeZones:
                info = self.timingWordTableEXPLICIT(issueTime, 
                  hazRec['endTime'], tz, "end")
                if info not in etext:
                    etext.append(info)
        elif etype == "FUZZY4":
            for tz in timeZones:
                info = self.timingWordTableFUZZY4(issueTime, 
                  hazRec['endTime'], tz, "end")
                if info not in etext:
                    etext.append(info)
        elif etype == "FUZZY8":
            for tz in timeZones:
                info = self.timingWordTableFUZZY8(issueTime, 
                  hazRec['endTime'], tz, "end")
                if info not in etext:
                    etext.append(info)
        elif etype == "DAY_NIGHT_ONLY":
            for tz in timeZones:
                info = self.timingWordTableDAYNIGHT(issueTime, 
                  hazRec['endTime'], tz, "end")
                if info not in etext:
                    etext.append(info)

        # timing connection types
        startPrefix, endPrefix = self.getTimingConnectorType((stype, etype),
          hazRec['act'])

        # get the timing phrase
        phrase = self.calculateTimingPhrase(stype, etype, stext, etext,
          startPrefix, endPrefix)

        return phrase

    # calculates the timing phrase based on the timing type, the calculated
    # timing words, and the prefixes
    def calculateTimingPhrase(self, stype, etype, stext, etext, startPrefix,
      endPrefix):

        if (stype, etype) == ("NONE", "NONE"):
            return ""  #no timing phrase

        elif (stype, etype) in [("NONE", "EXPLICIT")]:
            return self.ctp_NONE_EXPLICIT(stext,etext,startPrefix,endPrefix)

        elif (stype, etype) in [("NONE", "FUZZY4"), ("NONE", "FUZZY8")]:
            return self.ctp_NONE_FUZZY(stext,etext,startPrefix,endPrefix)

        elif (stype, etype) in [("EXPLICIT", "EXPLICIT")]:
            return self.ctp_EXPLICIT_EXPLICIT(stext,etext,startPrefix,
              endPrefix)

        elif (stype, etype) in [("EXPLICIT", "FUZZY4"), ("EXPLICIT", "FUZZY8")]:
            return self.ctp_EXPLICIT_FUZZY(stext,etext,startPrefix,endPrefix)

        elif (stype, etype) in [("FUZZY4", "FUZZY4"), ("FUZZY8", "FUZZY4"),
          ("FUZZY4", "FUZZY8"), ("FUZZY8", "FUZZY8")]:
            return self.ctp_FUZZY_FUZZY(stext,etext,startPrefix,endPrefix)

        elif (stype, etype) in [("NONE", "DAY_NIGHT_ONLY")]:
            return self.ctp_NONE_DAYNIGHT(stext,etext,startPrefix,endPrefix)

        elif (stype, etype) in [("EXPLICIT", "DAY_NIGHT_ONLY")]:
            return self.ctp_EXPLICIT_DAYNIGHT(stext,etext,startPrefix,
              endPrefix)

        elif (stype, etype) in [("FUZZY4", "DAY_NIGHT_ONLY"), 
          ("FUZZY8", "DAY_NIGHT_ONLY")]:
            return self.ctp_FUZZY_DAYNIGHT(stext,etext,startPrefix,endPrefix)

        elif (stype, etype) in [("DAY_NIGHT_ONLY", "DAY_NIGHT_ONLY")]:
            return self.ctp_DAYNIGHT_DAYNIGHT(stext,etext,startPrefix,
              endPrefix)

        elif (stype, etype) in [("DAY_NIGHT_ONLY", "NONE")]:
            return self.ctp_DAYNIGHT_NONE(stext,etext,startPrefix,endPrefix)

        elif (stype, etype) in [("DAY_NIGHT_ONLY", "EXPLICIT")]:
            return self.ctp_DAYNIGHT_EXPLICIT(stext,etext,startPrefix,
              endPrefix)

        elif (stype, etype) in [("DAY_NIGHT_ONLY", "FUZZY4"),
          ("DAY_NIGHT_ONLY", "FUZZY8")]:
            return self.ctp_DAYNIGHT_FUZZY(stext,etext,startPrefix,endPrefix)

        else:
            return "<UnknownPhraseType-" + stype + "/" + etype + ">"


    #calculates the NONE/EXPLICIT timing phrase
    def ctp_NONE_EXPLICIT(self, stext, etext, startPrefix, endPrefix):
        #single time zone
        if len(etext) == 1:
            hourStr, hourTZstr, description = etext[0]
            #special cases NOON
            if hourStr == "12 PM":
               hourStr = "Noon"
            return endPrefix + ' ' + hourStr + ' ' + hourTZstr + ' ' + \
              description
        
        #multiple time zones            
        elif len(etext) > 1:
            hourStr, hourTZstr, description = etext[0]
            #special cases NOON
            if hourStr == "12 PM":
               hourStr = "Noon"
            s = endPrefix + ' ' + hourStr + ' ' + hourTZstr + ' '
            for x in xrange(1, len(etext)):
                hourStr, hourTZstr, othDescription = etext[x]
                #special cases NOON
                if hourStr == "12 PM":
                   hourStr = "Noon"
                s = s + "/" + hourStr + ' ' + hourTZstr + "/ "
            s = s + description
            return s
    
    #calculates the NONE/FUZZY timing phrase
    def ctp_NONE_FUZZY(self, stext, etext, startPrefix, endPrefix):
        #returns phrase like:  THROUGH THIS EVENING
        hourStr, hourTZstr, description = etext[0]  #ending text
        s = endPrefix + ' ' + description
        return s
    
    #calculates the NONE/EXPLICIT timing phrase
    def ctp_EXPLICIT_EXPLICIT(self, stext, etext, startPrefix, endPrefix):
        #return phrases like:  
        #  FROM 2 AM WEDNESDAY TO 2 AM CST THURSDAY
        #  FROM 2 AM TO 5 AM CST THURSDAY
        #  FROM 2 AM CST /1 AM MST/ WEDNESDAY TO 2 AM CST /1 AM MST/ THURSDAY
        #  FROM 2 AM CST /1 AM MST/ TO 6 AM CST /5AM MST/ THURSDAY

        shourStr, shourTZstr, sdescription = stext[0]  #starting text
        ehourStr, ehourTZstr, edescription = etext[0]  #ending text

        #special cases NOON
        if shourStr == "12 PM":
           shourStr = "Noon"

        #special cases NOON
        if ehourStr == "12 PM":
           ehourStr = "Noon"

        # special case EARLY THIS MORNING and THIS MORNING, replace with
        # just THIS MORNING
        if sdescription == "early this morning" and \
          edescription == "this morning":
            sdescription = "this morning"  #combine two phrases
 

        # single time zone, same time zone for start/end times - same day
        if len(stext) == 1 and len(etext) == 1 and \
          shourTZstr == ehourTZstr and sdescription == edescription:
            return startPrefix + ' ' + shourStr + ' ' + endPrefix + ' ' +\
              ehourStr + ' ' + ehourTZstr + ' ' + edescription

        # single time zone, same time zone for start/end times - diff day
        if len(stext) == 1 and len(etext) == 1 and \
          shourTZstr == ehourTZstr and sdescription != edescription:
            return startPrefix + ' ' + shourStr + ' ' + sdescription + \
              ' ' + endPrefix + ' ' + ehourStr + ' ' + ehourTZstr + \
              ' ' + edescription

        # mult time zones, same day for start/end times
        if sdescription == edescription:
            s = startPrefix + ' ' + shourStr + ' ' + shourTZstr + ' '
            for x in xrange(1, len(stext)):
                hourStr, hourTZstr, description = stext[x]
                #special cases NOON
                if hourStr == "12 PM":
                   hourStr = "Noon"
                s = s + "/" + hourStr + ' ' + hourTZstr + "/ "
            s = s + endPrefix + ' ' + ehourStr + ' ' + ehourTZstr + ' '
            for x in xrange(1, len(etext)):
                hourStr, hourTZstr, description = etext[x]
                #special cases NOON
                if hourStr == "12 PM":
                   hourStr = "Noon"
                s = s + "/" + hourStr + ' ' + hourTZstr + "/ "
            s = s + edescription
            return s

        # mult time zones, different day for start/end times
        else:
            s = startPrefix + ' ' + shourStr + ' ' + shourTZstr + ' '
            for x in xrange(1, len(stext)):
                hourStr, hourTZstr, description = stext[x]
                #special cases NOON
                if hourStr == "12 PM":
                   hourStr = "Noon"
                s = s + "/" + hourStr + ' ' + hourTZstr + "/ "
            s = s + sdescription + ' ' + endPrefix + ' ' + ehourStr + \
              ' ' + ehourTZstr + ' '
            for x in xrange(1, len(etext)):
                hourStr, hourTZstr, description = etext[x]
                #special cases NOON
                if hourStr == "12 PM":
                   hourStr = "Noon"
                s = s + "/" + hourStr + ' ' + hourTZstr + "/ "
            s = s + edescription
            return s
    
    #calculates the NONE/EXPLICIT timing phrase
    def ctp_EXPLICIT_FUZZY(self, stext, etext, startPrefix, endPrefix):
        #returns phrase like:
        #    FROM 2 AM CST WEDNESDAY THROUGH LATE WEDNESDAY NIGHT
        #    FROM 2 AM CST /1 AM MST/ WEDNESDAY THROUGH LATE WEDNESDAY NIGHT

        #start phrase
        hourStr, hourTZstr, description0 = stext[0]
        #special cases NOON
        if hourStr == "12 PM":
           hourStr = "Noon"
        s = startPrefix + ' ' + hourStr + ' ' + hourTZstr + ' ' 
        for x in xrange(1, len(stext)):
            hourStr, hourTZstr, description = stext[x]
            #special cases NOON
            if hourStr == "12 PM":
               hourStr = "Noon"
            s = s + "/" + hourStr + ' ' + hourTZstr + "/ "
        s = s + description0 + ' '

        #end phrase
        hourStr, hourTZstr, description = etext[0]
        s = s + endPrefix + ' ' + description
        
        return s
    
    #calculates the FUZZY/FUZZY timing phrase
    def ctp_FUZZY_FUZZY(self, stext, etext, startPrefix, endPrefix):
        #return phrases like FROM THIS EVENING THROUGH LATE WEDNESDAY NIGHT
        #return phrases like LATE WEDNESDAY NIGHT

        hourStr, hourTZstr, s_description = stext[0]  #starting text
        hourStr, hourTZstr, e_description = etext[0]  #ending text

        #special case of description the same
        if s_description == e_description:
            return s_description

        #normal case of different descriptions
        s = startPrefix + ' ' + s_description + ' ' + endPrefix + ' ' +\
          e_description

        return s
 
    def ctp_NONE_DAYNIGHT(self,stext,etext,startPrefix,endPrefix):
        #return phrases like THROUGH WEDNESDAY

        hourStr, hourTZstr, e_description = etext[0]  #ending text

        s = endPrefix + ' ' + e_description

        return s

    def ctp_EXPLICIT_DAYNIGHT(self, stext, etext, startPrefix, endPrefix):
        #returns phrase like:
        #    FROM 2 AM CST WEDNESDAY THROUGH WEDNESDAY
        #    FROM 2 AM CST /1 AM MST/ WEDNESDAY THROUGH WEDNESDAY

        #start phrase
        hourStr, hourTZstr, description0 = stext[0]
        #special cases NOON
        if hourStr == "12 PM":
           hourStr = "Noon"
        s = startPrefix + ' ' + hourStr + ' ' + hourTZstr + ' ' 
        for x in xrange(1, len(stext)):
            hourStr, hourTZstr, description = stext[x]
            #special cases NOON
            if hourStr == "12 PM":
               hourStr = "Noon"
            s = s + "/" + hourStr + ' ' + hourTZstr + "/ "
        s = s + description0 + ' '

        #end phrase
        hourStr, hourTZstr, description = etext[0]
        s = s + endPrefix + ' ' + description
        
        return s

    def ctp_FUZZY_DAYNIGHT(self, stext,etext,startPrefix,endPrefix):
        #return phrases like FROM THIS EVENING THROUGH WEDNESDAY NIGHT

        hourStr, hourTZstr, s_description = stext[0]  #starting text
        hourStr, hourTZstr, e_description = etext[0]  #ending text

        #special case of description the same
        if s_description == e_description:
            return s_description

        #normal case of different descriptions
        s = startPrefix + ' ' + s_description + ' ' + endPrefix + ' ' +\
          e_description

        return s

    def ctp_DAYNIGHT_DAYNIGHT(self,stext,etext,startPrefix,endPrefix):
        #return phrases like FROM TONIGHT THROUGH WEDNESDAY

        hourStr, hourTZstr, s_description = stext[0]  #starting text
        hourStr, hourTZstr, e_description = etext[0]  #ending text

        #special case of description the same
        if s_description == e_description:
            return s_description

        #normal case of different descriptions
        s = startPrefix + ' ' + s_description + ' ' + endPrefix + ' ' +\
          e_description

        return s

    def ctp_DAYNIGHT_EXPLICIT(self, stext,etext,startPrefix,endPrefix):
        #returns phrase like:
        #    FROM TUESDAY UNTIL 2 AM CST WEDNESDAY
        #    FROM TUESDAY UNTIL 2 AM CST /1 AM MST/ WEDNESDAY

        #start phrase
        hourStr, hourTZstr, description = stext[0]
        s = startPrefix + ' ' + description + ' '

        #end phrase
        hourStr, hourTZstr, description0 = etext[0]
        #special cases NOON
        if hourStr == "12 PM":
           hourStr = "Noon"
        s = s + endPrefix + ' ' + hourStr + ' ' + hourTZstr + ' ' 
        for x in xrange(1, len(etext)):
            hourStr, hourTZstr, description = etext[x]
            #special cases NOON
            if hourStr == "12 PM":
               hourStr = "Noon"
            s = s + "/" + hourStr + ' ' + hourTZstr + "/ "
        s = s + description0 + ' '

        return s

    def ctp_DAYNIGHT_NONE(self, stext,etext,startPrefix,endPrefix):
        #return phrases like FROM TONIGHT

        hourStr, hourTZstr, s_description = stext[0]  #starting text

        s = startPrefix + ' ' + s_description

        return s

    def ctp_DAYNIGHT_FUZZY(self,stext,etext,startPrefix,endPrefix):
        #return phrases like FROM TONIGHT THROUGH WEDNESDAY NIGHT

        hourStr, hourTZstr, s_description = stext[0]  #starting text
        hourStr, hourTZstr, e_description = etext[0]  #ending text

        #special case of description the same
        if s_description == e_description:
            return s_description

        #normal case of different descriptions
        s = startPrefix + ' ' + s_description + ' ' + endPrefix + ' ' +\
          e_description

        return s


    def getTimingConnectorType(self, timingType, action):
    # Returns the start and end prefix for the given start and end phrase
    # type and action code.
        d = {("NONE", "NONE"):           (None, None),
             ("NONE", "EXPLICIT"):       (None, "until"),
             ("NONE", "FUZZY4"):         (None, "through"),
             ("NONE", "FUZZY8"):         (None, "through"),
             ("EXPLICIT", "EXPLICIT"):   ("from", "to"),
             ("EXPLICIT", "FUZZY4"):     ("from", "through"),
             ("EXPLICIT", "FUZZY8"):     ("from", "through"),
             ("FUZZY4", "FUZZY4"):       ("from", "through"),
             ("FUZZY4", "FUZZY8"):       ("from", "through"),
             ("FUZZY8", "FUZZY4"):       ("from", "through"),
             ("FUZZY8", "FUZZY8"):       ("from", "through"),
             ("NONE", "DAY_NIGHT_ONLY"):          (None, "through"),
             ("EXPLICIT", "DAY_NIGHT_ONLY"):      ("from", "through"),
             ("FUZZY4", "DAY_NIGHT_ONLY"):        ("from", "through"),
             ("FUZZY8", "DAY_NIGHT_ONLY"):        ("from", "through"),
             ("DAY_NIGHT_ONLY", "DAY_NIGHT_ONLY"): ("from", "through"),
             ("DAY_NIGHT_ONLY", "NONE"):          ("from", None),
             ("DAY_NIGHT_ONLY", "EXPLICIT"):      ("from", "to"),
             ("DAY_NIGHT_ONLY", "FUZZY4"):        ("from", "through"),
             ("DAY_NIGHT_ONLY", "FUZZY8"):        ("from", "through"),
            }

        # special case for expirations.
        if action == 'EXP':
            return (None, "AT")

        return d.get(timingType, ("<startPrefix?>", "<endPrefix?>"))
         
    def getTimingType(self, hazRec, issueTime):
        #Returns the timing type based on the issuanceTime and hazard record
        #Returns (startType, endType), which is NONE, EXPLICIT, FUZZY4, FUZZY8

        # Get the local headlines customizable timing
        tr = self.makeTimeRange(hazRec['startTime'], hazRec['endTime'])
        locStart, locEnd = self.getLocalHeadlinesTiming(
            None, None, hazRec['phen'], tr, hazRec['id'], issueTime)
                
        #time from issuanceTime
        deltaTstart = hazRec['startTime'] - issueTime  #seconds past now
        deltaTend = hazRec['endTime'] - issueTime  #seconds past now
    
        HR=3600  #convenience constants
        MIN=60   #convenience constants
    
        # record in the past, ignore
        if deltaTend <= 0:
            return ("NONE", "NONE")
    
        # upgrades and cancels
        if hazRec['act'] in ['UPG', 'CAN']:
            return ("NONE", "NONE")   #upgrades/cancels never get timing phrases
    
        # expirations EXP codes are always expressed explictly, only end time
        if hazRec['act'] == 'EXP':
            return ('NONE', 'EXPLICIT')
    
        phensig = hazRec['phen'] + '.' + hazRec['sig']
    
        # SPC Watches always get explicit times, 3 hour start mention
        spcWatches = ['TO.A', 'SV.A']
        if phensig in spcWatches:
            if deltaTstart < 3*HR:
                return ('NONE', 'EXPLICIT')
            else:
                return ('EXPLICIT', 'EXPLICIT')

        # Tropical events never get times at all
        tpcEvents = ['TY.A','TY.W','HU.A','HU.S','HU.W','TR.A','TR.W']
        if phensig in tpcEvents:
            return ('NONE', 'NONE')
    
        # special marine case?
        marineHazList = ["SC.Y", "SW.Y", "GL.W", "SR.W", 'HF.W', 'BW.Y',
          'UP.W', 'UP.Y', 'RB.Y', 'SE.W', 'SI.Y']  #treat like watches
        marinePils = ['CWF', 'OFF', 'NSH', 'GLF']  #specific marine pils
        oconusSites = ['PGUM','PHFO','PAFC','PAJK','PAFG']
    
        # regular products - not marine
        if hazRec['pil'] not in marinePils:
            #advisories/warnings
            if hazRec['sig'] in ['Y','W']:   #advisories/warnings - explicit
                if deltaTstart < 3*HR:    #no start time in first 3 hours
                    start = 'NONE'
                else:
                    start = 'EXPLICIT'    #explicit start time after 3 hours
                end = 'EXPLICIT'          #end time always explicit
    
            #watches
            elif hazRec['sig'] in ['A']:  #watches - mix of explicit/fuzzy
                if deltaTstart < 3*HR:    #no start time in first 3 hours
                    start = 'NONE'
                elif deltaTstart < 12*HR:
                    start = 'EXPLICIT'    #explicit start time 3-12 hours
                else:
                    start = 'FUZZY4'      #fuzzy times after 12 (4/day)
                if deltaTend < 12*HR:     #explicit end time 0-12 hours
                    end = 'EXPLICIT'
                else:
                    end = 'FUZZY4'        #fuzzy times after 12 (4/day)
            
            #local hazards
            elif locStart is not None and locEnd is not None:
                start = locStart
                end = locEnd
            else: 
                if deltaTstart < 3*HR:    #no start time in first 3 hours
                    start = 'NONE'
                elif deltaTstart < 12*HR:
                    start = 'EXPLICIT'    #explicit start time 3-12 hours
                else:
                    start = 'FUZZY4'      #fuzzy times after 12 (4/day)
                if deltaTend < 12*HR:     #explicit end time 0-12 hours
                    end = 'EXPLICIT'
                else:
                    end = 'FUZZY4'        #fuzzy times after 12 (4/day)
    
    
        # marine - CONUS
        elif hazRec['officeid'] not in oconusSites:
    
            # njensen: i changed the < to <= below because the automated tests
            # were failing with a race condition where issueTime would be the
            # exact same time as startTime and therefore the tests would sometimes
            # fall into the wrong if/else block
    
            #advisories/warnings - explicit, but not some phensigs
            if hazRec['sig'] in ['Y','W'] and phensig not in marineHazList:
                if deltaTstart <= 3*HR:    #no start time in first 3 hours
                    start = 'NONE'
                else:
                    start = 'EXPLICIT'    #explicit start time after 3 hours
                end = 'EXPLICIT'          #end time always explicit
    
            #watches - mix of explicit/fuzzy, some phensig treated as watches
            elif hazRec['sig'] in ['A'] or phensig in marineHazList:
                if deltaTstart <= 3*HR:    #no start time in first 3 hours
                    start = 'NONE'
                elif deltaTstart <= 12*HR:
                    start = 'EXPLICIT'    #explicit start time 3-12 hours
                else:
                    start = 'FUZZY4'      #fuzzy times after 12 (4/day)
                if deltaTend <= 12*HR:     #explicit end time 0-12 hours
                    end = 'EXPLICIT'
                else:
                    end = 'FUZZY4'        #fuzzy times after 12 (4/day)
            
            #local hazards - treat as watches
            elif locStart is not None and locEnd is not None:
                start = locStart
                end = locEnd
            else:
                if deltaTstart < 3*HR:    #no start time in first 3 hours
                    start = 'NONE'
                elif deltaTstart < 12*HR:
                    start = 'EXPLICIT'    #explicit start time 3-12 hours
                else:
                    start = 'FUZZY4'      #fuzzy times after 12 (4/day)
                if deltaTend < 12*HR:     #explicit end time 0-12 hours
                    end = 'EXPLICIT'
                else:
                    end = 'FUZZY4'        #fuzzy times after 12 (4/day)
    
        # marine - OCONUS
        else:
    
            #advisories/warnings - explicit, but not some phensigs
            if hazRec['sig'] in ['Y','W'] and phensig not in marineHazList:
                if deltaTstart < 3*HR:    #no start time in first 3 hours
                    start = 'NONE'
                else:
                    start = 'EXPLICIT'    #explicit start time after 3 hours
                end = 'EXPLICIT'          #end time always explicit

            #special marine phensigs - treat as watches, with fuzzy8
            elif phensig in marineHazList:
                if deltaTstart < 3*HR:    #no start time in first 3 hours
                    start = 'NONE'
                else:
                    start = 'FUZZY8'      #fuzzy start times
                end = 'FUZZY8'            #always fuzzy end times

    
            #regular watches - fuzzy4
            elif hazRec['sig'] in ['A']:
                if deltaTstart < 3*HR:    #no start time in first 3 hours
                    start = 'NONE'
                elif deltaTstart < 12*HR:
                    start = 'EXPLICIT'    #explicit start time 3-12 hours
                else:
                    start = 'FUZZY4'      #fuzzy times after 12 (4/day)
                if deltaTend < 12*HR:     #explicit end time 0-12 hours
                    end = 'EXPLICIT'
                else:
                    end = 'FUZZY4'        #fuzzy times after 12 (4/day)

            #local hazards - treat as watches
            elif locStart is not None and locEnd is not None:
                start = locStart
                end = locEnd
            else:
                if deltaTstart < 3*HR:    #no start time in first 3 hours
                    start = 'NONE'
                elif deltaTstart < 12*HR:
                    start = 'EXPLICIT'    #explicit start time 3-12 hours
                else:
                    start = 'FUZZY4'      #fuzzy times after 12 (4/day)
                if deltaTend < 12*HR:     #explicit end time 0-12 hours
                    end = 'EXPLICIT'
                else:
                    end = 'FUZZY4'        #fuzzy times after 12 (4/day)
    
        return (start, end)

    def getLocalHeadlinesTiming(self,tree, node, key, tr,
                                areaLabel, issuanceTime):
        headlinesTiming = self.headlinesTiming(tree, node, key, tr,
          areaLabel, issuanceTime)
        if headlinesTiming is None:
            locStart = None
            locEnd = None
        else:
            locStart, locEnd = headlinesTiming
            if locStart == "FUZZY":
                locStart = "FUZZY4"
            if locEnd == "FUZZY":
                locEnd = "FUZZY4"
        return locStart, locEnd

    def hazardTimeZones(self, areaList):
        #returns list of time zones for the starting time
        #and list of time zones for the ending time.  The areaList provides
        #a complete list of areas for this headline. startT, endT are the
        #hazard times.

        # get this time zone
        thisTimeZone = os.environ["TZ"]
        zoneList = []

        # get the AreaDictionary that contains time zones per edit area
        areaDictName = self._areaDictionary
        accessor = ModuleAccessor.ModuleAccessor()
        areaDict = accessor.variable(areaDictName, "AreaDictionary")

        # check to see if we have any areas outside our time zone
        for areaName in areaList:
            if areaName in areaDict.keys():
                entry = areaDict[areaName]
                if not entry.has_key("ugcTimeZone"): #add your site id
                    if thisTimeZone not in zoneList:
                        zoneList.append(thisTimeZone)
                    continue  # skip it
                timeZoneList = entry["ugcTimeZone"]
                if type(timeZoneList) == types.StringType:  # a single value
                    timeZoneList = [timeZoneList]   # make it into a list
                for timeZone in timeZoneList:
                    if timeZone not in zoneList:
                        zoneList.append(timeZone)

        # if the resulting zoneList is empty, put in our time zone
        if len(zoneList) == 0:
            zoneList.append(thisTimeZone)

        # if the resulting zoneList has our time zone in it, be sure it
        # is the first one in the list
        try:
            index = zoneList.index(thisTimeZone)
            if index != 0:
                del zoneList[index]
                zoneList.insert(0, thisTimeZone)
        except:
            pass

        return zoneList
        
    def timingWordTableEXPLICIT(self, issueTime, eventTime, timezone, 
      timeType='start'):
        #returns (timeValue, timeZone, descriptiveWord).  
        #eventTime is either the starting or ending time, based on 
        #the timeType flag. timezone is the time zone for the hazard area

        HR=3600
        sameDay = [
          (0*HR,       6*HR,     "early this morning"), #midnght-559am
          (6*HR,      12*HR-1,   "this morning"),       #600am-1159am
          (12*HR,     12*HR+1,   "today"),               #noon
          (12*HR+1,   18*HR-1,   "this afternoon"),     #1201pm-559pm
          (18*HR,     24*HR,     "this evening")]       #6pm-1159pm

        nextDay = [
          (0*HR,       0*HR+1,   "tonight"),            #midnght
          (0*HR,      24*HR,     "<dayOfWeek>"),]       #midnght-1159pm

        subsequentDay = [
          (0*HR,       0*HR+1,   "<dayOfWeek-1> Night"),  #midnght
          (0*HR,      24*HR,     "<dayOfWeek>"),]         #midnght-1159pm


        #determine local time
        myTimeZone = os.environ["TZ"]  # save the defined time zone
        os.environ["TZ"] = timezone    # set the new time zone
        ltissue = time.localtime(issueTime) # issuance local time
        ltevent = time.localtime(eventTime) # event local time
        #get the hour string (e.g., 8 PM)
        hourStr = time.strftime("%I %p", ltevent)
        if hourStr[0] == '0':
            hourStr = hourStr[1:]  #eliminate leading zero

        #get the time zone (e.g., MDT)
        hourTZstr = time.strftime("%Z", ltevent)

        #determine the delta days from issuance to event
        diffDays = ltevent[7] - ltissue[7]  #julian day
        if diffDays < 0:   #year wrap around, assume Dec/Jan
            diffDays = ltevent[2] + 31 - ltissue[2]  #day of month

        #get description time phrase
        description = "<day>"
        hourmin = ltevent[3]*3600 + ltevent[4]*60   #hour, minute
        if diffDays == 0:
            for (startT, endT, desc) in sameDay:
                if hourmin >= startT and hourmin < endT and timeType=='start':
                    description = desc
                    break
                elif hourmin <= endT and timeType=='end':
                    description = desc
                    break

        else:
            #choose proper table
            if diffDays == 1:
                table = nextDay
            else:
                table = subsequentDay
            for (startT, endT, desc) in table:
                hourmin = ltevent[3]*3600 + ltevent[4]*60   #hour, minute
                if hourmin >= startT and hourmin < endT and timeType=='start':
                    description = desc
                    break
                elif hourmin <= endT and timeType=='end':
                    description = desc
                    break
            dow = ltevent[6]  #day of week
            dowMinusOne = ltevent[6] - 1
            if dowMinusOne < 0:
                dowMinusOne = 6   #week wraparound
            description = string.replace(description, "<dayOfWeek>",
              self.asciiDayOfWeek(dow))   #day of week
            description = string.replace(description, "<dayOfWeek-1>",
              self.asciiDayOfWeek(dowMinusOne))   #day of week

        #special cases NOON
        if hourStr == "12 PM" and description == "today":
            hourStr = "Noon"

        #special cases MIDNIGHT
        if hourStr == "12 AM":
            hourStr = "Midnight"

        os.environ["TZ"] = myTimeZone  # reset the defined time zone

        return (hourStr, hourTZstr, description)


    def timingWordTableFUZZY4(self, issueTime, eventTime, timeZone,
      timeType='start'):
        #returns (timeValue, timeZone, descriptiveWord).  
        #eventTime is either the starting or ending time, based on 
        #the timeType flag. timezone is the time zone for the hazard area
        #table is local time, start, end, descriptive phrase
        HR=3600
        sameDay = [
          (0*HR,       6*HR,     "early this morning"), #midnght-559am
          (6*HR,      12*HR,     "this morning"),       #600am-noon
          (12*HR,     18*HR,     "this afternoon"),     #1200pm-559pm
          (18*HR,     24*HR,     "this evening")]       #6pm-1159pm

        nextDay = [
          (0*HR,       0*HR,     "this evening"),              #midnght tonight
          (0*HR,       6*HR,     "late tonight"),              #midnght-559am
          (6*HR,      12*HR,     "<dayOfWeek> morning"),       #600am-noon
          (12*HR,     18*HR,     "<dayOfWeek> afternoon"),     #1200pm-559pm
          (18*HR,     24*HR,     "<dayOfWeek> evening")]       #6pm-1159pm

        subsequentDay = [
          (0*HR,       0*HR,     "<dayOfWeek-1> evening"),     #midnght ystdy 
          (0*HR,       6*HR,     "late <dayOfWeek-1> night"),  #midnght-559am
          (6*HR,      12*HR,     "<dayOfWeek> morning"),       #600am-noon
          (12*HR,     18*HR,     "<dayOfWeek> afternoon"),     #1200pm-559pm
          (18*HR,     24*HR,     "<dayOfWeek> evening")]       #6pm-1159pm


        #determine local time
        myTimeZone = os.environ["TZ"]  # save the defined time zone
        os.environ["TZ"] = timeZone    # set the new time zone
        ltissue = time.localtime(issueTime) # issuance local time
        ltevent = time.localtime(eventTime) # event local time

        #determine the delta days from issuance to event
        diffDays = ltevent[7] - ltissue[7]  #julian day
        if diffDays < 0:   #year wrap around, assume Dec/Jan
            diffDays = ltevent[2] + 31 - ltissue[2]  #day of month

        #get description time phrase
        description = "<day>"
        hourmin = ltevent[3]*3600 + ltevent[4]*60   #hour, minute
        if diffDays == 0:
            for (startT, endT, desc) in sameDay:
                if hourmin >= startT and hourmin < endT and timeType=='start':  
                    description = desc
                    break
                elif hourmin <= endT and timeType=='end':
                    description = desc
                    break

        else:
            #choose proper table
            if diffDays == 1:
                table = nextDay
            else:
                table = subsequentDay
            for (startT, endT, desc) in table:
                hourmin = ltevent[3]*3600 + ltevent[4]*60   #hour, minute
                if hourmin >= startT and hourmin < endT and timeType=='start':
                    description = desc
                    break
                elif hourmin <= endT and timeType=='end':
                    description = desc
                    break
            dow = ltevent[6]  #day of week
            dowMinusOne = ltevent[6] - 1
            if dowMinusOne < 0:
                dowMinusOne = 6   #week wraparound
            description = string.replace(description, "<dayOfWeek>",
              self.asciiDayOfWeek(dow))   #day of week
            description = string.replace(description, "<dayOfWeek-1>",
              self.asciiDayOfWeek(dowMinusOne))   #day of week

        os.environ["TZ"] = myTimeZone  # reset the defined time zone

        hourStr = None
        hourTZstr = None
        return (hourStr, hourTZstr, description)


    def timingWordTableFUZZY8(self, issueTime, eventTime, timeZone,
      timeType='start'):
        #returns the descriptive word for the event.  eventTime is either
        #the starting or ending time, based on the timeType flag.
        #table is local time, start, end, descriptive phrase-A
  
        HR=3600
        sameDay = [
          (0*HR,       3*HR,     "late <dayOfWeek-1> night"), #midnght-259am
          (3*HR,       6*HR,     "early this morning"),    #300am-559am
          (6*HR,       9*HR,     "this morning"),          #600am-859am
          (9*HR,      12*HR,     "late this morning"),     #900am-1159am
          (12*HR,     15*HR,     "early this afternoon"),  #noon-259pm
          (15*HR,     18*HR,     "late this afternoon"),   #300pm-559pm
          (18*HR,     21*HR,     "this evening"),          #600pm-859pm
          (21*HR,     24*HR,     "tonight")]               #900pm-1159pm

        nextDayStart = [
          (0*HR,       3*HR,     "late <dayOfWeek-1> night"),  #midnght-259am
          (3*HR,       6*HR,     "early <dayOfWeek> morning"), #300am-559am
          (6*HR,      12*HR,     "<dayOfWeek> morning"),       #600am-noon
          (12*HR,     18*HR,     "<dayOfWeek> afternoon"),     #1200pm-559pm
          (18*HR,     24*HR,     "<dayOfWeek> evening")]       #6pm-1159pm

        nextDayEnd = [
          (0*HR,       0*HR,     "tonight"),                   #midnght tonight
          (0*HR,       3*HR,     "late <dayOfWeek-1> night"), #midnght-259am
          (3*HR,       6*HR,     "early <dayOfWeek> morning"), #300am-559am
          (6*HR,      12*HR,     "<dayOfWeek> morning"),       #600am-noon
          (12*HR,     18*HR,     "<dayOfWeek> afternoon"),     #1200pm-559pm
          (18*HR,     24*HR,     "<dayOfWeek> night")]         #6pm-1159pm

        subsequentDayStart =  [
          (0*HR,       6*HR,     "late <dayOfWeek-1> night"),  #midnght-559am
          (6*HR,      12*HR,     "<dayOfWeek> morning"),       #600am-noon
          (12*HR,     18*HR,     "<dayOfWeek> afternoon"),     #1200pm-559pm
          (18*HR,     24*HR,     "<dayOfWeek> evening")]       #6pm-1159pm

        subsequentDayEnd = [
          (0*HR,       0*HR,     "<dayOfWeek-1> night"),       #midnght tonight
          (0*HR,       6*HR,     "early <dayOfWeek> morning"), #midnght-559am
          (6*HR,      12*HR,     "<dayOfWeek> morning"),       #600am-noon
          (12*HR,     18*HR,     "<dayOfWeek> afternoon"),     #1200pm-559pm
          (18*HR,     24*HR,     "<dayOfWeek> night")]         #6pm-1159pm


        #determine local time
        myTimeZone = os.environ["TZ"]  # save the defined time zone
        os.environ["TZ"] = timeZone    # set the new time zone
        ltissue = time.localtime(issueTime) # issuance local time
        ltevent = time.localtime(eventTime) # event local time

        #determine the delta days from issuance to event
        diffDays = ltevent[7] - ltissue[7]  #julian day
        if diffDays < 0:   #year wrap around, assume Dec/Jan
            diffDays = ltevent[2] + 31 - ltissue[2]  #day of month

        #get description time phrase
        description = "<day>"
        hourmin = ltevent[3]*3600 + ltevent[4]*60   #hour, minute
        if diffDays == 0:
            for (startT, endT, desc) in sameDay:
                if hourmin >= startT and hourmin < endT and timeType=='start':  
                    description = desc
                    break
                elif hourmin <= endT and timeType=='end':
                    description = desc
                    break

        else:
            #choose proper table
            if timeType == 'start':
                if diffDays == 1:
                    table = nextDayStart
                else:
                    table = subsequentDayStart
            else:
                if diffDays == 1:
                    table = nextDayEnd
                else:
                    table = subsequentDayEnd
            for (startT, endT, desc) in table:
                hourmin = ltevent[3]*3600 + ltevent[4]*60   #hour, minute
                if hourmin >= startT and hourmin < endT and timeType=='start':
                    description = desc
                    break
                elif hourmin <= endT and timeType=='end':
                    description = desc
                    break

       #do substitution
        dow = ltevent[6]  #day of week
        dowMinusOne = ltevent[6] - 1
        if dowMinusOne < 0:
            dowMinusOne = 6   #week wraparound
        description = string.replace(description, "<dayOfWeek>",
          self.asciiDayOfWeek(dow))   #day of week
        description = string.replace(description, "<dayOfWeek-1>",
          self.asciiDayOfWeek(dowMinusOne))   #day of week

        os.environ["TZ"] = myTimeZone  # reset the defined time zone

        hourStr = None
        hourTZstr = None
        return (hourStr, hourTZstr, description)

    def timingWordTableDAYNIGHT(self, issueTime, eventTime, timeZone,
      timeType='start'):
        #returns (timeValue, timeZone, descriptiveWord).  
        #eventTime is either the starting or ending time, based on 
        #the timeType flag. timezone is the time zone for the hazard area
        #table is local time, start, end, descriptive phrase
        HR=3600
        sameDay = [
          (0*HR,         self.DAY()*HR,   "early today"), #midnght-559am
          (self.DAY()*HR,   self.NIGHT()*HR, "today"),       #600am-6pm
          (self.NIGHT()*HR, 24*HR,        "tonight")]     #6pm-midnight

        nextDay = [
          (0*HR,         self.DAY()*HR,   "tonight"),           #midnght-559am
          (self.DAY()*HR,   self.NIGHT()*HR, "<dayOfWeek>"),       #600am-6pm
          (self.NIGHT()*HR, 24*HR,        "<dayOfWeek> night")] #6pm-midnight

        subsequentDay = [
          (0*HR,         self.DAY()*HR,   "<dayOfWeek-1> night"), #midnght-559am
          (self.DAY()*HR,   self.NIGHT()*HR, "<dayOfWeek>"),         #600am-6pm
          (self.NIGHT()*HR, 24*HR,        "<dayOfWeek> night")]   #6pm-midnight

        #determine local time
        myTimeZone = os.environ["TZ"]  # save the defined time zone
        os.environ["TZ"] = timeZone    # set the new time zone
        ltissue = time.localtime(issueTime) # issuance local time
        ltevent = time.localtime(eventTime) # event local time

        #determine the delta days from issuance to event
        diffDays = ltevent[7] - ltissue[7]  #julian day
        if diffDays < 0:   #year wrap around, assume Dec/Jan
            diffDays = ltevent[2] + 31 - ltissue[2]  #day of month

        #get description time phrase
        description = "<day>"
        hourmin = ltevent[3]*3600 + ltevent[4]*60   #hour, minute
        if diffDays == 0:
            for (startT, endT, desc) in sameDay:
                if hourmin >= startT and hourmin < endT and timeType=='start':  
                    description = desc
                    break
                elif hourmin <= endT and timeType=='end':
                    description = desc
                    break

        else:
            #choose proper table
            if diffDays == 1:
                table = nextDay
            else:
                table = subsequentDay
            for (startT, endT, desc) in table:
                hourmin = ltevent[3]*3600 + ltevent[4]*60   #hour, minute
                if hourmin >= startT and hourmin < endT and timeType=='start':
                    description = desc
                    break
                elif hourmin <= endT and timeType=='end':
                    description = desc
                    break
            dow = ltevent[6]  #day of week
            dowMinusOne = ltevent[6] - 1
            if dowMinusOne < 0:
                dowMinusOne = 6   #week wraparound
            description = string.replace(description, "<dayOfWeek>",
              self.asciiDayOfWeek(dow))   #day of week
            description = string.replace(description, "<dayOfWeek-1>",
              self.asciiDayOfWeek(dowMinusOne))   #day of week

        os.environ["TZ"] = myTimeZone  # reset the defined time zone

        hourStr = None
        hourTZstr = None
        return (hourStr, hourTZstr, description)


    def asciiDayOfWeek(self, number):
        #converts number (0-Monday) to day of week
        days = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
          'Saturday', 'Sunday']
        if number >= 0 and number < 7:
            return days[number]
        else:
            return "?" + `number` + "?"


    # Returns the headline phrase based on the specified hazard.
    # The hazard record contains all geoIDs in the hazard['id'] field, 
    # not just a single one.  Doesn't add the dots.
    def makeStandardPhrase(self, hazard, issuanceTime):

        # hdln field present?
        if not hazard.has_key('hdln'):
            return ""

        # make sure the hazard is still in effect or within EXP critiera
        if (hazard['act'] != 'EXP' and issuanceTime >= hazard['endTime']) or \
          (hazard['act'] == 'EXP' and issuanceTime > 30*60 + hazard['endTime']):
            return ""   # no headline for expired hazards

        #assemble the hazard type
        hazStr = hazard['hdln']
        hazStr = self.convertToLower(hazStr)

        # if the hazard is a convective watch, tack on the etn
        phenSig = hazard['phen'] + "." + hazard['sig']
        if phenSig in ["TO.A", "SV.A"]:
            hazStr = hazStr + " " + str(hazard["etn"])

        # add on the action
        actionWords = self.actionControlWord(hazard, issuanceTime)
        hazStr = hazStr + ' ' + actionWords

        #get the timing words
        timeWords = self.getTimingPhrase(hazard, issuanceTime)
        if len(timeWords):
            hazStr = hazStr + ' ' + timeWords

        return hazStr

    def timeCompare(self, haz1, haz2):
        if haz1['startTime'] < haz2['startTime']:
            return -1
        elif haz1['startTime'] == haz2['startTime']:
            return 0
        else:
            return 1

    # Sorts headlines for marine products. sort algorithm
    # cronological ordering by start time, then action, 
    # then significance, then phen alphabetically.
    def marineSortHazardAlg(self, r1, r2):
        #1st by start time
        if r1['startTime'] < r2['startTime']:
            return -1
        elif r1['startTime'] > r2['startTime']:
            return 1
        
        #2nd by action
        actionCodeOrder = ["CAN", "EXP", "UPG", "NEW", "EXB", "EXA",
                           "EXT", "ROU", "CON"]
        try:
            aIndex = actionCodeOrder.index(r1['act'])
        except:
            aIndex = 99
        try:
            bIndex = actionCodeOrder.index(r2['act'])
        except:
            bIndex = 99
        if aIndex < bIndex:
            return -1
        elif aIndex > bIndex:
            return 1

        #3rd by significance
        sig = ['W','Y','A']
        try:
            index1 = sig.index(r1['sig'])
        except:
            index1 = 99
        try:
            index2 = sig.index(r2['sig'])
        except:
            index2 = 99
        if index1 < index2:
            return -1
        elif index1 > index2:
            return 1

        #4th by phen (alphabetically)
        if r1['phen'] < r2['phen']:
            return -1
        elif r1['phen'] > r2['phen']:
            return 1

        #equal
        return 0


    # Sorts headlines for regular products. 
    def regularSortHazardAlg(self, r1, r2):
        actActions = ["NEW", "EXB", "EXA", "EXT", "ROU", "CON"]
        inactActions = ["CAN", "EXP", "UPG"]
        actionCodeOrder = actActions + inactActions

        # 1st by general action category
        if r1['act'] in actActions and r2['act'] in inactActions:
            return -1
        elif r1['act'] in inactActions and r2['act'] in actActions:
            return 1

        # 2nd by chronological event starting time
        if r1['startTime'] < r2['startTime']:
            return -1
        elif r1['startTime'] > r2['startTime']:
            return 1

        # 3rd by action code order
        try:
            aIndex = actionCodeOrder.index(r1['act'])
        except:
            aIndex = 99
        try:
            bIndex = actionCodeOrder.index(r2['act'])
        except:
            bIndex = 99
        if aIndex < bIndex:
            return -1
        elif aIndex > bIndex:
            return 1
        
        #4th by significance
        sig = ['W','Y','A']
        try:
            index1 = sig.index(r1['sig'])
        except:
            index1 = 99
        try:
            index2 = sig.index(r2['sig'])
        except:
            index2 = 99
        if index1 < index2:
            return -1
        elif index1 > index2:
            return 1

        #5th by phen (alphabetically)
        if r1['phen'] < r2['phen']:
            return -1
        elif r1['phen'] > r2['phen']:
            return 1

        #equal
        return 0

        
    # Makes multiple headlines based on the hazards list and returns the lot.
    def makeHeadlinePhrases(self, tree, node, hazardList, issuanceTime,
      testMode=0):
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
                    print "...Ignoring action code:", hazard['act'], \
                      hazard['hdln']
                    hList.remove(hazard)
                    continue

            # get the headline phrase
            hazStr = self.makeStandardPhrase(hazard, issuanceTime)
            if len(hazStr):
                # Call user hook
                localStr = self.addSpace(self.hazard_hook(
                  tree, node, hazard['phen'], hazard['sig'], hazard['act'],
                  hazard['startTime'], hazard['endTime']), "leading")
                returnStr = returnStr + "..." + hazStr + localStr + "...\n"

            # always remove the main hazard from the list
            hList.remove(hazard)

        return returnStr
                    
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


    # The organizeHazard method brings in the raw analyzed table, 
    # then organizes it by edit area, returing a list of 
    # editArea lists. The first element of the list must the the first 
    # segment in a hazard based product.  Ensures that a group of edit areas
    # does not contain both zones and FIPS code - per 10-1702.
    def organizeHazards(self, rawATable):
                                                                                
        # Initialize data structures to be used.
        byIdDict = {}
        byHazardDict = {}
        masterEditAreaList = []
                                                                                
        # Loop over the activeTable, and organize by editArea
                                                                                
        #
        # Added code to discard segment identifer when cancelling a product.
        # this was creating bogus segments.
        #
                                                                                
        for eachHazard in rawATable:
            if byIdDict.has_key(eachHazard['id']):
                byIdDict[eachHazard['id']].append(\
                 (eachHazard['phen'], eachHazard['sig'], eachHazard['seg'],
                 eachHazard['act'], eachHazard['startTime'], eachHazard['endTime'],
                 eachHazard['etn']))
            else:
                byIdDict[eachHazard['id']] = [(eachHazard['phen'],
                  eachHazard['sig'], eachHazard['seg'],
                  eachHazard['act'], eachHazard['startTime'], eachHazard['endTime'],
                  eachHazard['etn'])]
                                                                                
        #
        # Go through the sorted dictionary, organize into combos
        #
                                                                                
        idsList = byIdDict.keys()
        unsortedHazards = byIdDict.values()
        sortedHazards = []
        for eachHazard in unsortedHazards:
            if not self.__sortedContains(eachHazard, sortedHazards):
                sortedHazards.append(eachHazard)
                                                                                
        #
        # The following section determines the VTEC/segment ordering
        #
                                                                                
        weightedList = []
                                                                                
        #
        # this list ranks by 'sig' and 'act' from least [0] to most 
        # importance [n]. All CANs go at the end, because cancel is the 
        # most important action.
        #
                                                                                
        segmentVTECOrderList = [
          # Place holder for local hazards
          'LocalHazard',            
          'F.ROU', 'F.CON', 'F.EXT', 'F.EXA', 'F.EXB', 'F.NEW',
          'F.UPG', 'S.ROU', 'S.CON', 'S.EXT', 'S.EXA', 'S.EXB',
          'S.NEW', 'S.UPG', 'A.ROU', 'A.CON', 'A.EXT', 'A.EXA',
          'A.EXB', 'A.NEW', 'A.UPG', 'Y.ROU', 'Y.CON', 'Y.EXT',
          'Y.EXA', 'Y.EXB', 'Y.NEW', 'Y.UPG', 'W.ROU', 'W.CON',
          'W.EXT', 'W.EXA', 'W.EXB', 'W.NEW', 'W.UPG', 'F.EXP',
          'F.CAN', 'S.EXP', 'S.CAN', 'A.EXP', 'A.CAN', 'Y.EXP',
          'Y.CAN', 'W.EXP', 'W.CAN']
                                                                                
        for eachHazard in sortedHazards:
            tempEditAreaList = []
            tempElementWeight = -1.0
            tempElementWeightCheck = -1.0
            secondaryWeight = 0.0
            segmentWeight = 0.0
            timeWeight = 0.0
                                                                                
            #
            # Figure out the maximum weight based on each
            # element in the hazard combination.
            #
                                                                                
            for eachElement in eachHazard:
                                                                                
                #
                # This section checks of the hazard's index in 
                # segmentVTECOrderList
                #
                                                                                
                if eachElement[1] is not None and eachElement[3] is not None:
                    sigAction = eachElement[1] + '.' + eachElement[3]
                    if sigAction in segmentVTECOrderList:
                        tempElementWeightCheck = float(segmentVTECOrderList.index(sigAction))
                    else:
                        # Local hazards are not in list so
                        # assign it least importance
                        tempElementWeightCheck = 0.0
                                                                                
                #
                # secondaryWeight is a cumulative value << 1 that allows 
                # combinations of actions and sigs to take precedence over 
                # single actions or sigs of the same primary importance. For 
                # instance, a BZ.W^WC.Y will come before BZ.W by itself, 
                # even though they are the same priority. It also takes
                # into account the hazards position in the allowedHazardTable, 
                # so that a a blizzard warning will trump a winter storm 
                # warning
                #
                                                                                
                #
                # from 1 (important) to 1001 (undefined). 1 is added to 
                # prevent division errors.
                #
                                                                                
                allowedHazardValue = float(self.getHazardImportance(\
                  eachElement[0] + '.' + eachElement[1])) + 1.0
                #
                #  Ensure that secondary weight never approaches 1 (ten thousandths...)
                #
                                                                                
                secondaryWeight = secondaryWeight + 0.0001/allowedHazardValue
                                                                                
                #
                # Check the tempElementWeightCheck against the 
                # tempElementWeight. If it's more, then the current hazard 
                # is the higher priority of the combo, and set 
                # tempElementWeight to it's index value. If it's less, then 
                # this hazard is of lower priority, but do give it a little 
                # weight (<< 1) so that for instance  a warn + advisory 
                # segment will come before just a warn segment.
                #
                                                                                
                if tempElementWeightCheck > tempElementWeight:
                   # This hazard is more important
                   tempElementWeight = tempElementWeightCheck
                                                                                
                #
                # Add a factor for segment number. Lowest segments go first. Never
                # Approach one (millionths...)
                #                                                                
                
                segmentWeight = 1.0/(10000000.0 + float(eachElement[2]))
                
                #
                # Add a factor for time. Earliest start times go first. Never
                # approach one (e-10)
                #
                
                timeWeight = 1.0/float(eachElement[4] + 100.0)
                
            #
            # Assign the sum of weights before adding 
            # list for sorting
            #
                                                                                
            tempElementWeight = tempElementWeight + secondaryWeight +\
                                segmentWeight + timeWeight
                                
            secondaryWeight = 0.0
            segmentWeight = 0.0
            timeWeight = 0.0
                                                                                
            for eachID in idsList:
                if sets.Set(byIdDict[eachID]) == sets.Set(eachHazard):
                    tempEditAreaList.append(eachID)
                                                                                
            weightedList.append((tempElementWeight, tempEditAreaList))
                                                                                
        # Sort the list by weight
        weightedList.sort(self._wtListSort)
                                                                                
        # Make the list of geoareas
        finalList = []
        for w in weightedList:
            finalList.append(w[1])

        # Seperate out the zones and FIPS into separate UGC blocks
        s = []
        for s1 in finalList:
            fips = []
            zones = []
            for s2 in s1:
                if s2[2] == 'Z':
                    zones.append(s2)
                elif s2[2] == 'C':
                    fips.append(s2)
            if len(fips):
                s.append(fips)
            if len(zones):
                s.append(zones)
        finalList = s
                                                                                
        return finalList
                      

    # Determines if hazard in sorted hazards. Hazard can be a list, thus we
    # need to compare all elements for their inclusion, rather than simply 
    # using the "in" operator.
    def __sortedContains(self, hazard, sorted_hazards):
        hazard.sort()
        for indSorted in sorted_hazards:
            indSorted.sort()
            if hazard == indSorted:
                return 1
        return 0

    # Sorts tuples of (weight, list, time), by weight
    def _wtListSort(self, a, b):
        if a[0] > b[0]:
            return -1
        elif a[0] == b[0]:
            return 0
        else:
            return 1


    # Modifies string to have ...TEST... if we are in TEST mode.  This
    # to the MND header.  Modifies string to have EXPERIMENTAL... if
    # we are in EXPERIMENTAL mode.
    def checkTestMode(self, argDict, str):
        # testMode is set, then we are in product test mode.
        # modify the str to have beginning and ending TEST indication.
        if argDict.get('testMode', 0):
            return "TEST..."+str+"...TEST"
        elif argDict.get('experimentalMode', 0):
            return "EXPERIMENTAL..." + str
        else:
            return str

    # Modifies headline string to have TEST if we are in TEST mode. 
    def headlinePhraseTESTcheck(self, argDict, str):
        if argDict.get('testMode', 0):
            lines = str.split('\n')
            str = "...THIS MESSAGE IS FOR TEST PURPOSES ONLY...\n"
            for x in xrange(len(lines)-1):   #-1 for trailing new line
                line = lines[x]

                #beginning of line
                if line.find("...") == 0:
                    line = line[0:3] + "TEST " + line[3:]
                #end of line
                index = line.rfind("...")
                if index != 0 and index == len(line)-3:
                    line = line[0:-3] + " TEST..." 

                lines[x] = line

            return str + string.join(lines,'\n')

        #normal mode (not test mode)
        else:
            return str
 
    # utility for attribution, takes hazard description ['hdln'] field and
    # adds TEST if appropriate in test mode, adds "A" or "AN" as appropriate
    # if desired. 
    def hazardName(self, name, argDict, addA=False):
 
        if len(name) == 0:
            return name

        # test mode
        if argDict.get('testMode', 0):
            phrase = 'TEST ' + name   #test mode, prepend "TEST"
        else:
            phrase = name

        # want A or AN?
        if addA:
            if phrase[0] in ['A','E','I','O','U','a','e','i','o','u']:
                phrase = "AN " + phrase
            else:
                phrase = "A " + phrase
        return phrase

                
            

