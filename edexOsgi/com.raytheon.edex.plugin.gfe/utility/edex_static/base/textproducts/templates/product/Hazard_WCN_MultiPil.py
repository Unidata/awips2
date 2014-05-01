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
#######################################################################
# Hazard_WCN.py
#
##
##########################################################################
import GenericHazards
import string, time, re, os, types, copy, sets
import ModuleAccessor, LogStream
import VTECTable

class TextProduct(GenericHazards.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "BaselineHazard_WCN_<MultiPil> (Convective Watch)"

    Definition["defaultEditAreas"] = "EditAreas_FIPS_<site>_<MultiPil>"
    Definition["mapNameForCombinations"] = ["FIPS_<site>", "Marine_Zones_<site>"]
       # Map background for creating Combinations

    # Header configuration items
    Definition["productName"] = "WATCH COUNTY NOTIFICATION"  # name of product
    Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
    Definition["wmoID"] = "<wmoID>"        # WMO ID
    Definition["pil"] = "<pil>"          # product pil
    #Definition["areaName"] = "STATENAME"  # Name of state, such as "GEORGIA"
    Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
    Definition["wfoCity"] = "<wfoCity>"       # WFO Name as it should appear in a text product
    Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
    Definition["outputFile"] =  "{prddir}/TEXT/WCN_<MultiPil>.txt"

    # OPTIONAL CONFIGURATION ITEMS
    #Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
    #Definition["displayOutputDialog"] = 0  # If 1 will display results when finished
    #Definition["debug"] = 1
    #Definition["headlineEditAreaGroup"] = "Zones" # Name of EditAreaGroup for sampling headlines

    Definition["purgeTime"] = 15        # Maximum hours for expireTime from issueTime
    Definition["includeCities"] = 0    # Cities included in area header
    Definition["cityDescriptor"] = "INCLUDING THE CITIES OF"
    Definition["includeZoneNames"] = 0 # Zone names will be included in the area header
    Definition["includeIssueTime"] = 0   # This should be set to zero for products
                                       # that do not include a time lime below the UGC
    #Definition["easPhrase"] = ""      # Optional EAS phrase to be include in product header        
    Definition["lineLength"] = 66

    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)

    Definition["statePartMode"] = "byState"  #"byState" or "byPart" formatting
                                             #options. byState summarizes
                                             #count by state.  "byPart" 
                                             #counts by part of state.
       

    def __init__(self):
        GenericHazards.TextProduct.__init__(self)

    def _preProcessProduct(self, fcst, argDict):

        #
        # The code below determines the set of ETNs for the header
        #
        
        self._hazards = argDict['hazards']
        hazards = self._hazards.rawAnalyzedTable()
        allWatchList = []
        for hazard in hazards:
            if hazard['etn'] not in allWatchList:
                allWatchList.append(hazard['etn'])

        if len(allWatchList) == 1:                        
            watchPhrase = " FOR WATCH " +  str(allWatchList[0])
        else:
            watchPhrase = " FOR WATCHES "
            allWatchList.sort()
            for x in xrange(len(allWatchList)):
                watchPhrase = watchPhrase + str(allWatchList[x])
                if x != len(allWatchList) - 1:
                    watchPhrase = watchPhrase + "/"
            
        #           
        # Special Product header code to add watch number determined above
        #
        
        if self._areaName != "":
            self._areaName = " FOR " + self._areaName
        issuedByString = self.getIssuedByString()
        productName = self.checkTestMode(argDict, 
          self._productName + watchPhrase) 
        
        fcst =  fcst + self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n" +\
               productName + "\n" +\
               "NATIONAL WEATHER SERVICE " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n" + self._easPhrase + "\n"
        fcst = string.upper(fcst)
        return fcst


    def _preProcessArea(self, fcst, segmentAreas, expireTime, argDict):

        #
        # This is the header for an edit area combination
        #
        
        editArea = segmentAreas[0]
        areaLabel = editArea
        areaHeader = self.makeAreaHeader(
            argDict, "", self._issueTime, expireTime,
            self._areaDictionary, None, cityDescriptor=self._cityDescriptor,
            areaList=segmentAreas, includeCities=self._includeCities,
            includeZoneNames = self._includeZoneNames, includeIssueTime = self._includeIssueTime)
        fcst = fcst + areaHeader + "\n"
        return fcst


    def _makeProduct(self, fcst, segmentAreas, argDict):
        argDict["language"] = self._language

        #issuance time
        issuanceTime = self._issueTime.unixTime()
        
        #
        # Set up the edit areas being dealt with
        #
        
        editArea = segmentAreas[0]
        areaLabel = editArea

        #
        # Build a list of the merged hazards being returned
        #
        
        listOfHazards = self._hazards.getHazardList(segmentAreas)

        # Ensure hdln is defined, since other products can reset this
        for h in listOfHazards:
            if len(h['hdln']) == 0:
                phensig = h['phen'] + '.' + h['sig']
                if VTECTable.VTECTable.has_key(phensig):
                    h['hdln'] = VTECTable.VTECTable[phensig]['hdln']

        #
        # Prepare to build phrases
        #

        attrPhrase = ""
        actionTest = []
        hazardListLength = len(listOfHazards)
        
        #
        # Start building phrases
        #

        phraseType = ""  #CANCEL, NEW, REPLACE, EXPIRE (zone listing wording)

        #
        # First check to see if this segment contains a CAN and a NEW
        #

        if hazardListLength == 2:

            phraseType = "REPLACE"

            activeActions = ['NEW','EXB','EXA','EXT','CON']

            #
            # Element 0 is is CAN, UPG.  Element 1 is active actions
            #

            if listOfHazards[1]['act'] in activeActions and \
              listOfHazards[0]['act'] in ['CAN', 'UPG']:
                 #change forces next block to execute, code savings
                 listOfHazards.reverse() 
                                    
            #
            # Element 0 is active actions,  Element 1 is CAN, UPG
            #
            if listOfHazards[1]['act'] in ['CAN','UPG'] and \
              listOfHazards[0]['act'] in activeActions:
                 newWatch = listOfHazards[0]
                 oldWatch = listOfHazards[1]
                 newWatchName = self.hazardName(newWatch['hdln'], argDict,
                   False) + " " + str(newWatch['etn'])
                 oldWatchName = self.hazardName(oldWatch['hdln'], argDict,
                   False) + " " + str(oldWatch['etn'])
                 validTime = self.getTimingPhrase(newWatch, issuanceTime)

                 attrPhrase =  "THE NATIONAL WEATHER SERVICE HAS ISSUED " + \
                   newWatchName + " " + validTime + \
                   " WHICH REPLACES A PORTION OF " + oldWatchName + '. ' + \
                   "THE NEW WATCH IS VALID FOR THE FOLLOWING AREAS"
                 

            #
            # Element 0 is EXP, Element 1 is active actions
            #
            
            if listOfHazards[1]['act'] in activeActions and \
               listOfHazards[0]['act'] == 'EXP':
                 #change forces next block to execute, code savings
                 listOfHazards.reverse() 

            #
            # Element 0 is is active actions.  Element 1 is EXP
            #

            if listOfHazards[1]['act'] == 'EXP' and \
               listOfHazards[0]['act'] in activeActions:
                 newWatch = listOfHazards[0]
                 oldWatch = listOfHazards[1]
                 newWatchName = self.hazardName(newWatch['hdln'], argDict,
                   False) + " " + str(newWatch['etn'])
                 oldWatchName = self.hazardName(oldWatch['hdln'], argDict,
                   False) + " " + str(oldWatch['etn'])
                 validTime = self.getTimingPhrase(newWatch, issuanceTime)

                 if oldWatch['endTime'] > argDict['creationTime']:
                     expirePhrase = "WILL BE ALLOWED TO EXPIRE."
                 else:
                     expirePhrase = "HAS EXPIRED."

                 attrPhrase = "THE NATIONAL WEATHER SERVICE HAS ISSUED " + \
                   newWatchName + ' ' + validTime + ". " + \
                   oldWatchName + " " + expirePhrase + \
                   " THE NEW WATCH IS VALID FOR THE FOLLOWING AREAS"

        #
        # Else if the hazardListLength isn't 2
        #

        else:
            for eachHazard in listOfHazards:
                etnString = str(eachHazard['etn'])
                watchName = self.hazardName(eachHazard['hdln'], argDict,
                  False) + " " + etnString   #complete name and etn
                validTime  = self.getTimingPhrase(eachHazard, issuanceTime)
        
                #
                # Phrase for NEW
                #
                
                if eachHazard['act'] == 'NEW':
                    attrPhrase = "THE NATIONAL WEATHER SERVICE HAS ISSUED " +\
                      watchName + " IN EFFECT " +\
                       validTime + " FOR THE FOLLOWING AREAS"
                    phraseType = "NEW"

                #
                # Phrase for CON
                #
                
                elif eachHazard['act'] == 'CON':
                    attrPhrase = watchName + " REMAINS VALID " + validTime + \
                      " FOR THE FOLLOWING AREAS"
                    phraseType = "NEW"

                #
                # Phrase for EXP
                #
                
                elif eachHazard['act'] == 'EXP':
                    if eachHazard['endTime'] > argDict['creationTime']:
                        attrPhrase = "THE NATIONAL WEATHER SERVICE " + \
                          "WILL ALLOW " + watchName + " TO EXPIRE " +\
                          validTime + " FOR THE FOLLOWING AREAS"
                    else:
                        attrPhrase = "THE NATIONAL WEATHER SERVICE " + \
                          "HAS ALLOWED " + watchName + " TO EXPIRE" +\
                          " FOR THE FOLLOWING AREAS"
                    phraseType = "EXPIRE"

                #
                # Phrase for CAN
                #

                elif eachHazard['act'] == 'CAN':
                    attrPhrase = "THE NATIONAL WEATHER SERVICE " +\
                      "HAS CANCELLED " + watchName + \
                      " FOR THE FOLLOWING AREAS"
                    phraseType = "CANCEL"

                #
                # Phrase for EXA and EXB
                #
                
                elif eachHazard['act'] in ['EXA', 'EXB']:
                    attrPhrase="THE NATIONAL WEATHER SERVICE HAS EXTENDED " +\
                      watchName + " TO INCLUDE THE FOLLOWING AREAS " + \
                      validTime
                    phraseType = "NEW"

                #
                # Phrase for EXT
                #
                
                elif eachHazard['act'] == 'EXT':
                    phraseType = "NEW"
                    #prevExpPhrase = self.getHourAMPMTimeZone(\
                    #  eachHazard['previousEnd'], eachHazard['id'])
                    prevRec = copy.deepcopy(eachHazard)
                    prevRec['endTime'] = eachHazard['previousEnd']
                    prevExpPhrase = self.getTimingPhrase(prevRec, issuanceTime)
                        
                    attrPhrase = watchName + "...PREVIOUSLY IN EFFECT " +\
                      prevExpPhrase + "...IS NOW IN EFFECT " + \
                      validTime + " FOR THE FOLLOWING AREAS"

                #
                # Generic Phrase...should never reach this point
                #
                
                else:
                    startingPhrase = "THE NATIONAL WEATHER SERVICE " + \
                      "HAS ISSUED |* WATCH TYPE *| |* WATCH NUMBER *| " + \
                      "UNTIL |* WATCH END TIME *| FOR THE FOLLOWING AREAS" 
                    attrPhrase = startingPhrase
                    phraseType = "NEW"

        #
        # Add phrase to forecast
        #
        
        fcst = fcst + attrPhrase + '\n\n'


        # Get the phrasing set up for the type of event 
        if phraseType == "NEW":
            county1 = "IN {area} THIS WATCH INCLUDES {number} {placeType}"
            county2 = "IN {area} THIS WATCH INCLUDES {number} {placeTypes}"
            indepCity1 = "IN {area} THIS WATCH INCLUDES {number} " + \
              "INDEPENDENT CITY"
            indepCity2 = "IN {area} THIS WATCH INCLUDES {number} " + \
              "INDEPENDENT CITIES"
            marine = "THIS WATCH INCLUDES THE FOLLOWING ADJACENT " +\
              "COASTAL WATERS"
            
        elif phraseType == "CANCEL":
            county1 = "IN {area} THIS CANCELS {number} {placeType}"
            county2 = "IN {area} THIS CANCELS {number} {placeTypes}"
            indepCity1 = "IN {area} THIS CANCELS {number} INDEPENDENT CITY"
            indepCity2 = "IN {area} THIS CANCELS {number} INDEPENDENT CITIES"
            marine = "THIS CANCELS THE FOLLOWING ADJACENT " +\
              "COASTAL WATERS"

        elif phraseType == "EXPIRE":
            county1 = "IN {area} THIS ALLOWS TO EXPIRE {number} {placeType}"
            county2 = "IN {area} THIS ALLOWS TO EXPIRE {number} {placeTypes}"
            indepCity1 = "IN {area} THIS ALLOWS TO EXPIRE {number} " +\
              "INDEPENDENT CITY"
            indepCity2 = "IN {area} THIS ALLOWS TO EXPIRE {number} " +\
              "INDEPENDENT CITIES"
            marine = "THIS ALLOWS TO EXPIRE THE FOLLOWING ADJACENT " +\
              "COASTAL WATERS"

        elif phraseType == "REPLACE":
            county1 = "IN {area} THE NEW WATCH INCLUDES {number} {placeType}"
            county2 = "IN {area} THE NEW WATCH INCLUDES {number} {placeTypes}"
            indepCity1 = "IN {area} THE NEW WATCH INCLUDES {number} " + \
              "INDEPENDENT CITY"
            indepCity2 = "IN {area} THE NEW WATCH INCLUDES {number} " + \
              "INDEPENDENT CITIES"
            marine = "THE NEW WATCH INCLUDES THE FOLLOWING ADJACENT " +\
              "COASTAL WATERS"

        else:
            raise Exception, "Illegal phraseType in WCN formatter. " +\
              "Expected NEW, CANCEL, EXPIRE, or REPLACE.  Got " + phraseType


        # Add the list of counties
        countyTuple = self._getFilteredAreaList(
            segmentAreas, mode="COUNTY", areaDictName=self._areaDictionary)
        fcst = fcst + self._makeTextFromCountyTuple(countyTuple, 
          mainFormatSingular = county1, mainFormatPlural = county2,
          mode=self._statePartMode)

        # Add the lists of independent cities
        countyTuple = self._getFilteredAreaList(
            segmentAreas, mode="CITY", areaDictName=self._areaDictionary)
        fcst = fcst + self._makeTextFromCountyTuple(countyTuple, 
          mainFormatSingular = indepCity1, mainFormatPlural = indepCity2,
          mode=self._statePartMode)
        
        # Add the lists of marine zones
        countyTuple = self._getFilteredAreaList(
            segmentAreas, mode="ZONE", areaDictName=self._areaDictionary)
        fcst = fcst + self._makeTextFromMarineTuple(countyTuple, 
          mainFormat = marine)
        
        # Add the lists of cities
        fcst = fcst + "\n\n" + self.getCityList(
            segmentAreas, areaDictName = self._areaDictionary, addPeriod=True,
            forceAlphaSort=True)

        #
        # Line Wrap
        #
        
        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "...", "-"])

        #
        # Finished
        #
        
        return fcst

    def _postProcessArea(self, fcst, segmentAreas, argDict):
        fcst = fcst + "$$\n\n"
        return fcst


    def _countFilteredAreaList(self, countyTuples, index):
        #Returns a dictionary. dictionary is based on the 'index' element 
        # of the tuple (key) and is a count of the number of those 
        # records found.
        dict = {}
        for values in countyTuples:
            key = values[index]
            count = dict.get(key, 0)
            count = count + 1
            dict[key] = count

        return dict


    def _getFilteredAreaList(self, areaList, areaDictName="AreaDictionary", 
      mode="COUNTY"):
        #returns list of sorted tuples:
        #  [(state, partOfState, partOfState State, zonename)]
        #mode='COUNTY','ZONE','CITY'
        
        # Access the UGC information for the area(s) if available
        areaDict = ModuleAccessor.ModuleAccessor().variable(areaDictName,
          "AreaDictionary")
        if areaDict is None:
            return []

        # sort by zone name
        if mode == "ZONE":
            areaList.sort()

        # Make a list of (state, partOfStateAndState, county) tuples
        countyList = []
        for areaName in areaList:
            if areaDict.has_key(areaName):
                entry = areaDict[areaName]
            else:
                entry = {}
                LogStream.logProblem(\
                  "AreaDictionary missing definition for [" + areaName + "].")

            if mode == "COUNTY":
                if len(areaName) == 6 and areaName[2] != "C":  #not ssCnnn
                    continue   #not a county fips
                if entry.has_key("independentCity") and \
                  entry["independentCity"] == 1:
                    continue   #independent city, when in county mode
            elif mode == "CITY":
                if len(areaName) == 6 and areaName[2] != "C":  #not ssCnnn
                    continue   #not a county/city fips
                if not entry.has_key("independentCity") or \
                  entry["independentCity"] == 0:
                    continue   #not independent city, when in city mode
            elif mode == "ZONE":
                if len(areaName) == 6 and areaName[2] != "Z":  #not ssZnnn
                    continue   #not a zone code
            else:
                raise Exception, "Illegal mode specified " + mode

            if entry.has_key("ugcName") and len(entry['ugcName']):
                # Get fullStateName
                state = areaName[0:2]
                if entry.has_key("fullStateName") and \
                  len(entry['fullStateName']):
                    state = entry["fullStateName"]
                else:
                    state = "<fullStateName for " + state + " missing>"
                    LogStream.logProblem("AreaDictionary does not contain " +\
                      'fullStateName definition for ', areaName)
                    

                # Get part-of-state information with state (not for Zones)
                if mode == "ZONE":   #marine
                    partOfState = ""
                else:
                    if entry.has_key("partOfState") and \
                      len(entry['partOfState']):
                        partOfState = entry["partOfState"] + ' ' + state
                    else:
                        partOfState = "<partOfState> " + state
                        LogStream.logProblem(\
                          "AreaDictionary does not contain " +\
                          'partOfState definition for ', areaName)

                # Get county name
                county = entry["ugcName"]

                # Eliminate the name County and others, if in the name
                if mode == "COUNTY":
                    val = ['County','Counties','Parish','Parishes'] 
                    for v in val:
                        county = county.replace(" " + v, "")
                countyList.append((state, partOfState, county))
            
            #missing ugcName
            else:
                countyList.append(("<ugcName>", "<ugcName>", areaName))
                LogStream.logProblem("AreaDictionary does not contain " +\
                    'ugcName definition for ', areaName)


        # Sort by state, part of state, then county
        if mode != "ZONE":
            countyList.sort()  #state, partOfState, county
        return countyList

    def _makeTextFromMarineTuple(self, countyTuple, lineLength=66, colWidth=22,
      mainFormat="THIS WATCH INCLUDES THE FOLLOWING ADJACENT COASTAL WATERS"):
    
      #countyTuple:  (state, partOfStateAndState, name)
      #extract out the marine zones
      mzones = []
      for state, partOfState, name in countyTuple:
          mzones.append(name)

      if len(mzones) == 0:
          return ""

      return mainFormat + "\n\n" + \
        self.formatCountyColumns(mzones, colWidth, lineLength) + '\n\n'


    def _makeTextFromCountyTuple(self, countyTuple, lineLength=66, colWidth=22,
      mainFormatSingular="IN {area} THIS WATCH INCLUDES {number} {placeType}",
      mainFormatPlural="IN {area} THIS WATCH INCLUDES {number} {placeTypes}",
      subFormat="IN {area}", mode="byState"):

        #countyTuple:  (state, partOfStateAndState, name)
        #The type of text depends upon the mode: "byState" or "byPart"
        # "byState" formatting:
        # mainFormatSingular/mainFormatPlural  (for each state)
        # subFormat (for each partOfState)
        # column formatting of names
        #
        # "byPart" formatting:
        # (subFormat not used):
        # mainFormatSingular/mainFormatPlural (for each partOfState State)
        # column formatting of names

        # Format
        if mode == "byState":
            return self._byStateTextFromCountyTuple(countyTuple, lineLength,
              colWidth, mainFormatSingular, mainFormatPlural, subFormat)
        elif mode == "byPart":
            return self._byPartTextFromCountyTuple(countyTuple, lineLength,
              colWidth, mainFormatSingular, mainFormatPlural)
        else:
            raise Exception, "Illegal mode in makeTextFromCountyTuple(): " +\
              `mode`

    def _byStateTextFromCountyTuple(self, countyTuple, lineLength,
      colWidth, mainFormatSingular, mainFormatPlural, subFormat):

        #Determine counts for each area
        counts = self._countFilteredAreaList(countyTuple, 0)

        # Convert countyTuple into format that follows the text format
        # byState:  [(state, [(partOfStateAndState, [names])]]
        geoList = []
        geoPList = []
        names = []
        curState = None
        curPart = None
        for state, partState, name in countyTuple:
            if curState == state:
                if curPart == partState:
                    names.append(name)
                else:
                    if len(names):
                        geoPList.append((curPart, names))
                    names = [name]
                    curPart = partState
            else:
                if len(names):
                    geoPList.append((curPart, names))
                if len(geoPList):
                    geoList.append((curState, geoPList))
                geoPList = []
                names = [name]
                curPart = partState
                curState = state

        if len(names):
            geoPList.append((curPart, names))
            geoList.append((curState, geoPList))

        # Now Format the text
        result = ''
        for state, partStateNames in geoList:

            #special District of Columbia, no parts of state descriptors
            if state == "DISTRICT OF COLUMBIA":
                result = result + "THE DISTRICT OF COLUMBIA\n\n"
                continue

            ccount = counts.get(state, 0)
            if ccount > 1:
                header = mainFormatPlural
            else:
                header = mainFormatSingular
            header = string.replace(header, '{area}', state)
            header = string.replace(header, '{number}', str(ccount))
            if state == "LOUISIANA":
                header = string.replace(header, '{placeType}', "PARISH")
                header = string.replace(header, '{placeTypes}', "PARISHES")
            else:
                header = string.replace(header, '{placeType}', "COUNTY")
                header = string.replace(header, '{placeTypes}', "COUNTIES")
 

            result = result + header + '\n\n'

            for partState, names in partStateNames:
                subheader = subFormat
                subheader = string.replace(subheader, '{area}', partState)
                result = result + subheader + '\n\n'

                counties = self.formatCountyColumns(names, colWidth, 
                  lineLength)
                result = result + counties + '\n\n'

        return result


    def _byPartTextFromCountyTuple(self, countyTuple, lineLength,
      colWidth, mainFormatSingular, mainFormatPlural):
    
        #Determine counts for each area
        counts = self._countFilteredAreaList(countyTuple, 1)

        # Convert countyTuple into format that follows the text format
        # byPart:   [(partOfStateAndState, [names])]
        geoList = []
        names = []
        curSection = None   #partState
        for state, partState, name in countyTuple:
            if partState == curSection:
                names.append(name)
            else:
                if len(names):
                    geoList.append((curSection, names))
                names = [name]
                curSection = partState
        if len(names):
            geoList.append((curSection, names))

        # Now Format the text
        result = ''
        for partState, names in geoList:

            #special District of Columbia
            if partState.find("DISTRICT OF COLUMBIA") != -1:
                result = result + "THE DISTRICT OF COLUMBIA\n\n"
                continue

            ccount = counts.get(partState, 0)
            if ccount > 1:
                header = mainFormatPlural
            else:
                header = mainFormatSingular
            header = string.replace(header, '{area}', partState)
            header = string.replace(header, '{number}', str(ccount))
            if partState.find("LOUISIANA") != -1:
                header = string.replace(header, '{placeType}', "PARISH")
                header = string.replace(header, '{placeTypes}', "PARISHES")
            else:
                header = string.replace(header, '{placeType}', "COUNTY")
                header = string.replace(header, '{placeTypes}', "COUNTIES")

            counties = self.formatCountyColumns(names, colWidth, lineLength)

            result = result + header + '\n\n' + counties + '\n\n'

        return result

    
    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
            ('TO.A', allActions, 'Convective'),
            ('SV.A', allActions, 'Convective')
               ]
