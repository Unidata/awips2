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
# Header.py
# Methods for producing headers.
#
# Author: hansen
# ----------------------------------------------------------------------------

import EditAreaUtils
import StringUtils
import ModuleAccessor
import string, os, time, types, re
import TimeRangeUtils
import TimeRange, AbsTime
import logging

class Header(EditAreaUtils.EditAreaUtils, StringUtils.StringUtils):
#             TimeRangeUtils):
    def __init__(self):
        EditAreaUtils.EditAreaUtils.__init__(self)
        StringUtils.StringUtils.__init__(self)
#        TimeRangeUtils.TimeRangeUtils.__init__(self)
        self.log = logging.getLogger("FormatterRunner.Header.Header")
    
    def makeAreaHeader(self, argDict, areaLabel, issueTime, expireTime,
                       areaDictName, defaultEditAreas,
                       cityDescriptor ="Including the cities of",
                       areaList=None, includeCities=1, includeZoneNames=1,
                       includeIssueTime=1, includeCodes=1, includeVTECString=1,
                       hVTECString=None, accurateCities=False):
        # Make a UGC area header for the given areaLabel
        # Determine list of areas (there could be more than one if we are using a combination)

        if areaDictName is None or areaDictName == "None":
            return areaLabel + "\n"

        # If we didn't supply an areaList,
        #     Use combinations file or defaultEditAreas
        if areaList is None:
            combinations = argDict["combinations"]
            if combinations is not None:
                areaList = self.getCurrentAreaNames(argDict, areaLabel)
            else:
                for editArea, label in defaultEditAreas:
                    if label == areaLabel:
                        areaList = [editArea]

        try:
            # Remove suffixes if necessary
            if self._editAreaSuffix is not None:
                areaList = self.removeSuffixes(areaList, self._editAreaSuffix)
        except:
            pass

        # Access the UGC information for the area(s) if available
        accessor = ModuleAccessor.ModuleAccessor()
        areaDict = accessor.variable(areaDictName, "AreaDictionary")
        ugcCityList = []
        if areaDict is None:  # create a dummy header
            codeString = "STZxxx-"
            nameString = areaLabel
            cityString = ""
        else:
            codeString = ""
            nameString = ""
            cityString = ""
            areaList, ugcList = self.makeUGCList(areaDict, areaList)
            codeString = self.makeUGCString(ugcList)
            ugcNameList = []
            for areaName in areaList:
                if areaName in areaDict.keys():
                    if areaDict.has_key(areaName):
                        entry = areaDict[areaName]
                    else:
                        entry = {}
                        log.error(\
                          "AreaDictionary missing definition for [" + \
                          areaName + "].")
                    if entry.has_key('ugcName'):
                        ugcName = entry['ugcName']
                    else:
                        ugcName = areaName  #missing UGCname
                        log.error(\
                          "AreaDictionary missing ugcName definition for [" + \
                          areaName + "].")
                    if ugcName not in ugcNameList:
                        ugcNameList.append(ugcName)
                    if entry.has_key("ugcCityString"):
                        cities = entry["ugcCityString"].split('...')
                        for c in cities:
                            if len(c) and c not in ugcCityList:
                                ugcCityList.append(c)
                else:
                    ugcNameList.append(areaName)
                    log.error("AreaDictionary does not contain " +\
                      'ugcName definition for ', areaName)

            if self.alphabetizeHeaders() == 1:
                # Alphabetize both lists.
                ugcNameList.sort()
                ugcCityList.sort()

            # Build nameString and cityString strings:
            for ugcName in ugcNameList:
                nameString = nameString + ugcName + "-"
            for ugcCity in ugcCityList:
                cityString = cityString + "..." + ugcCity

        # Compute accurate city list
        if accurateCities and \
           len(ugcCityList) > 0 and argDict.has_key("hazards"):
            ugcCityList, cityString = self.makeAccurateCityList(areaList, \
                                        ugcCityList, argDict)

        # get the VTEC string from the HazardsTable
        VTECString = ""
        VTECRecords = []
        if argDict.has_key("hazards") and includeVTECString:
            hazards = argDict["hazards"]
            VTECString = hazards.getVTECString(areaList)
            #must have VTECString, in order to have hVTEC string
            if hVTECString is not None and len(VTECString) and len(hVTECString):
                VTECString = VTECString + hVTECString + "\n"

        # expiration time is dependent upon the passed in expiration time
        # and the VTEC strings. expireT is seconds since epoch
        if type(expireTime) is types.IntType or\
          type(expireTime) is types.FloatType:
            expireTime = AbsTime.AbsTime(int(expireTime))
        try:
            if self._fixedExpire == 1:
                fixed = 1
            else:
                fixed = 0
        except:
            fixed = 0
        expireT = self.getExpireTime(issueTime.unixTime(),
          expireTime.unixTime(), VTECString, fixedExpire = fixed)

        # format the expiration time
        expireTimeRange = TimeRange.TimeRange(AbsTime.AbsTime(expireT),
          AbsTime.AbsTime(expireT+1))
        expireTime = self.timeDisplay(expireTimeRange, "", "","%d%H%M", "")
        codeString = self.endline(codeString + "-" + expireTime + "-",
          linelength=self._lineLength, breakStr=["-"])

        # get this time zone
        thisTimeZone = os.environ["TZ"]
        zoneList = []
        # check to see if we have any areas outside our time zone
        for areaName in areaList:
            if areaName in areaDict.keys():
                entry = areaDict[areaName]
                if not entry.has_key("ugcTimeZone"):   #add your site tz
                    if thisTimeZone not in zoneList:
                        zoneList.append(thisTimeZone)
                    continue  # skip this entry
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

        # now create the time string
        issueTimeStr = ''
        timeStrs = []
        for timeZone in zoneList:
            timeStr = self.formatTimeString(
              issueTime.unixTime(), "%l%M %p %Z %a %b %e %Y", timeZone)
            timeStr = string.replace(timeStr, "  ", " ")
            timeStr = string.strip(timeStr)
            if timeStr not in timeStrs:
                timeStrs.append(timeStr)
        if len(timeStrs) == 1:
            issueTimeStr = timeStrs[0]
        else:
            issueTimeStr = timeStrs[0]
            for i in xrange(1, len(timeStrs)):
                issueTimeStr = issueTimeStr + " /" + timeStrs[i] + "/"

        try:
            if self._useRegionLabel == 1:
                if (areaLabel != ""):
                    nameString = areaLabel
        except:
            pass


        nameString = self.endline(nameString, linelength=self._lineLength,breakStr=["-"])
        if cityString != "":
            numCities = len(string.split(cityString, "...")[1:])
            if numCities == 1:
                cityDescriptor = string.replace(cityDescriptor, "CITIES", "CITY")
            cityString = self.endline(cityDescriptor + cityString,
              linelength=self._lineLength, breakStr=["..."])
        issueTimeStr = issueTimeStr + "\n\n"
        try:
            if self._includeHeader == 0:
                issueTimeStr = "\n"
                codeString = ""
                cityString = ""
        except:
            pass
        if includeCities == 0:
            cityString = ""
        if includeZoneNames == 0:
            nameString = ""
        if includeIssueTime == 0:
            issueTimeStr = ""
        if includeCodes == 0:
            codeString = ""
        if includeVTECString == 0:
            VTECString = ""
        header = codeString + VTECString + nameString + cityString  + issueTimeStr
        return header

    # Make accurate city list based on the grids
    # In case of missing grid (CAN/EXP actions), check active table for cities
    # in the previous product
    # When cities cannot be determined with certainty, add framing code to city
    # list so it may be unlocked for editing

    def makeAccurateCityList(self, areaList, ugcCityList, argDict):
        hazards = argDict["hazards"]
        vtecS = hazards.getHazardList(areaList)

        # Separate hazards according to action
        canRecords = []
        upgRecords = []
        expRecords = []
        actRecords = []
        for vtec in vtecS:
            if vtec['act'] == 'CAN':
                canRecords.append(vtec)
            elif vtec['act'] == 'UPG':
                upgRecords.append(vtec)
            elif vtec['act'] == 'EXP':
                expRecords.append(vtec)
            else:
                actRecords.append(vtec)                

        # Now determine the cities corresponding to the active grids
        citylist = hazards.getCities(ugcCityList, actRecords)

        # See if we can determine cities for EXP records from grids
        unresolved = []
        if len(expRecords):
            for expRec in expRecords:
                cities = hazards.getCitiesForEXP(ugcCityList,
                  areaList[0], expRec['phen'], expRec['sig'], expRec['endTime'])
                if cities is None:
                    unresolved.append(expRec)
                else:
                    citylist += cities

        # check if the full list is used and if we need to check the
        # previous product

        fullListUsed = 1
        for city in ugcCityList:
            if city not in citylist:
                fullListUsed = 0
                break

        editable = 0
        if fullListUsed:
            citylist = ugcCityList[:]
            
        elif (unresolved + canRecords):
            
            # For VTEC not associated with a grid,
            # try to extract cities from previous product
            # If without absolute certainty, make city list editable

            cities, certain = hazards.getCitiesFromPrevious(areaList,
                                expRecords + canRecords,
                                ignoredVTEC = upgRecords)
            if cities is not None:
                citylist += cities
                editable = not certain
            else:
                # failed...use the full list and make it editable
                editable = 1
                citylist = ugcCityList[:]

        # filter and order
        newlist = []
        for city in ugcCityList:
            if city in citylist:
                newlist.append(city)
        citylist = newlist

        cityString = ''
        for city in citylist:
            cityString = cityString + "..." + city
                
        # add framing code so the city list will be editable
        if editable and len(cityString) > 0:
            cityString = '...|*' + cityString[3:] + '*|'

        return citylist, cityString


    # Return new areaList and associated ugcList both sorted by ugcCode.
    # Extracts ugcCode from the area dictionary for the each areaName in areaList.
    # Will accept complex UGC strings in the area dictionary such as:
    # ORZ001-004-WAZ021>023-029.
    # However, in this case, one areaName could correspond to multiple
    # ugcCodes and thus, the areaList is not guaranteed to follow
    # the sorted ugcCode list order.
    def makeUGCList(self, areaDict, areaList):
        # Make a list of (areaName, ugcCode) tuples
        areaUgcList = []
        for areaName in areaList:
            if areaName in areaDict.keys():
                ugc = areaDict[areaName]['ugcCode']
                if ugc.find("-") >= 0 or ugc.find(">") >= 0:
                    ugcs = self.expandComplexUgc(ugc)
                    for ugc in ugcs:
                        areaUgcList.append((areaName, ugc))
                else:
                    areaUgcList.append((areaName, ugc))
                
        # Sort this list in ugc order
        areaUgcList.sort(self.ugcSort)

        # Make new "parallel" lists of areaNames and ugcCodes
        ugcList = []
        newAreaList = []
        for areaName, ugcCode in areaUgcList:
            if areaName not in newAreaList:
                newAreaList.append(areaName)
            if ugcCode not in ugcList:
                ugcList.append(ugcCode)

        #print "newAreaList, ugcList", newAreaList, ugcList
        return newAreaList, ugcList

    
    def expandComplexUgc(self, complexUgc):
        # Given a complex ugc string e.g. ORZ001-004-WAZ021>023-029,
        # return a list of all ugcs represented
        ugcList = []
        curState = ""
        arrowFlag = 0
        lastNum = 0
        # While we still have a dash or arrow
        while len(complexUgc) > 0:
            dash = complexUgc.find("-")
            arrow = complexUgc.find(">")
            # Peel off the next ugc from the complexUgc string
            if dash < 0 and arrow < 0:
                sep = len(complexUgc)
            elif arrow >= 0 and arrow < dash:
                sep = arrow
            elif dash >= 0 and dash < arrow:
                sep = dash
            elif dash >= 0:
                sep = dash
            else:
                sep = arrow
            ugc = complexUgc[:sep]
            complexUgc = complexUgc[sep+1:]
            # Add this ugc to the list
            nextUgcs, curState, lastNum = self.expandUgc(ugc, curState, lastNum, arrowFlag)
            arrowFlag = 0
            if sep == arrow:
                arrowFlag = 1
            for nextUgc in nextUgcs:
                ugcList.append(nextUgc)
        return ugcList

    def expandUgc(self, ugc, curState, lastNum, arrowFlag):
        # If the ugc has a state identifier on it,
        # return it as is and return it's state as the curState.
        # Otherwise append the curState to the ugc and
        # return it and curState
        if curState == "":
            curState = ugc[:3]
            return [ugc], curState, int(ugc[3:])
        state = ugc[:3]
        try:
            # If simply a number, add the current state
            num = int(state)
            # Check for arrow
            ugcList = []
            if arrowFlag:
                for ugcNum in range(lastNum+1, num+1):
                    curNum = str(ugcNum)
                    curNum = curNum.zfill(3)
                    ugcList.append(curState + curNum)
            else:
                ugcList.append(curState + ugc)
            return ugcList, curState, num                       
        except:
            curState = state
            return [ugc], curState, int(ugc[3:])

    def ugcSort(self, val1, val2):
        name1, ugc1 = val1
        name2, ugc2 = val2
        if ugc1 > ugc2:
            return 1
        elif ugc1 == ugc2:
            return 0
        else:
            return -1
    
    ###  creates a UGCCode header string from the specified list of UGC codes.
    def makeUGCString(self, ugcList):
        # if nothing in the list, return empty string
        if len(ugcList) == 0:
            return ""

        # Remove any blank UGC lines from the list
        listsize=len(ugcList)
        j=0
        while j < listsize:
            if ugcList[j] == "":
                del ugcList[j]
            j=j+1

        # Set up state variables and process intialize ugcStr with first ugc
        # in ugcList
        inSeq = 0
        ugcStr = ugcList[0]
        curState = ugcStr[0:3]
        lastNum = int(ugcList[0][3:])
        firstNum = 0
        lastUgc = ugcList[0]
        #print "ugcList", ugcList

        # By initializing properly we don't need the first item
        ugcList.remove(ugcList[0])

        for ugc in ugcList:
            ugcState = ugc[:3]
            ugcNumStr = ugc[3:]
            num = int(ugcNumStr)
            if ugcState == curState:
                if num == lastNum + 1:
                    if inSeq > 0:
                        # Replace the last ugcNumStr in sequence with the
                        # current ugcNumStr
                        # e.g.   062>063  becomes 062>064
                        ugcStr = ugcStr[:len(ugcStr)-3] + ugcNumStr
                        inSeq += 1
                    else:
                        ugcStr += ">" + ugcNumStr
                        inSeq = 1
                else:  # num != lastNum + 1
                    ugcStr = self.checkLastArrow(inSeq, ugcStr)
                    inSeq = 0  # reset sequence when number not in sequence
                    ugcStr += "-" + ugcNumStr
            else:
                ugcStr = self.checkLastArrow(inSeq, ugcStr)
                ugcStr += "-" + ugc
                curState = ugcState
                inSeq = 0   #reset sequence when switching states
            lastNum = num
            lastUgc = ugc

        # May have to clean up last arrow at the end
        ugcStr = self.checkLastArrow(inSeq, ugcStr)
        #print "returning", ugcStr
        return ugcStr

    def checkLastArrow(self, inSeq, ugcStr):
        if inSeq == 1:
            # Change the last arrow to - since
            # we only had 2 in the sequence e.g.
            # 062>063  should be   062-063
            arrowIndex = ugcStr.rfind(">")
            if arrowIndex >= 0:
                ugcStr = ugcStr[:arrowIndex] + "-" + ugcStr[arrowIndex+1:]
        return ugcStr
        

    # Header support for Watch/Warning products
    def getCityList(self, areaList, label="THIS INCLUDES THE CITIES OF", 
      lineLength=66, areaDictName="AreaDictionary", addPeriod = False,
      forceAlphaSort=False):
        # Returns a list of cities (from the AreaDictionary)
        # Appends an " AND " instead of "..." for the last city mentioned.

        # Access the UGC information for the area(s) if available
        areaDict = ModuleAccessor.ModuleAccessor().variable(areaDictName, "AreaDictionary")
        if areaDict is None:
            return ""
        cities = []
        cityString = ""
        for areaName in areaList:
            entry = areaDict[areaName]
            if entry.has_key("ugcCityString"):
                ct = entry['ugcCityString'].split('...')
                for c in ct:
                    if len(c):
                        cities.append(c)
        if len(cities) and (self.alphabetizeHeaders() == 1 or forceAlphaSort):
            cities.sort()
        if len(cities):
            for c in cities:
                cityString = cityString + "..." + c  

        cityString = self.replaceLast(cityString, "...", " AND ")
        if len(cityString) == 0:
            return ""
        else:
            if addPeriod:
                cityString = cityString + '.'
            return self.endline(label + cityString, lineLength, breakStr=["..."]) + "\n"

    # Returns a list of strings that describe the "areaList", such as
    # Southwest Kansas, along with their county/zone names.  Format returned
    # is [(stateName, portionOfState, [(county/zone list,type)])].  The type
    # is PARISH, COUNTY, ZONE, INDEPENDENT CITY.  Duplicate names are
    # eliminated.
    def getGeneralAreaList(self, areaList, areaDictName="AreaDictionary"):

        # Access the UGC information for the area(s) if available
        areaDict = ModuleAccessor.ModuleAccessor().variable(areaDictName,
          "AreaDictionary")
        if areaDict is None:
            return []

        geoAreas = {}
        for areaName in areaList:
            entry = areaDict[areaName]
            if entry.has_key("ugcName"):
                # Get state
                state = areaName[0:2]
                if entry.has_key("fullStateName"):
                    state = entry["fullStateName"]
                    #Special District of Columbia case
                    if state == "DISTRICT OF COLUMBIA":
                        state = "THE DISTRICT OF COLUMBIA"
                # Get part-of-state information
                partOfState = ""
                if entry.has_key("partOfState"):
                    partOfState = entry["partOfState"]

                # get the county/zone name
                zoneName = entry["ugcName"]
                if entry.has_key("locationName"):
                    zoneName = entry["locationName"]  #alternative name
                if entry.has_key("ugcCode"):
                    codeType = entry["ugcCode"][2]
                    if codeType == "Z":
                        nameType = "ZONE"
                    elif codeType == "C":
                        indCty=entry.get("independentCity", 0)
                        if indCty == 1:
                            nameType = "INDEPENDENT CITY"
                        elif state == "LOUISIANA":
                            nameType = "PARISH"
                        else:
                            nameType = "COUNTY"
                    else:
                        codeType == "?"
                value = (state, partOfState)
                znt = (zoneName, nameType)
                 
                if geoAreas.has_key(value):
                    names = geoAreas[value]
                    if znt not in names:
                        names.append(znt)
                else:
                    geoAreas[value] = [znt]

        #now sort the zoneName or countyNames
        for state, partState in geoAreas.keys():
            names = geoAreas[(state,partState)]
            names.sort()

        #now separate them by land and water
        #Anything to do with WATERS or other related items go last
        waters = ['WATERS','LAKE','RIVER']
        gaLAND = []
        gaWATER = []
        for g,pg in geoAreas.keys():
            names = geoAreas[(g,pg)]
            words = g.split(' ')
            found = 0
            for w in waters:
                if w in words:
                    found = 1
                    break
            if found:
                gaWATER.append((g,pg,names))
            else:
                gaLAND.append((g,pg,names))

        #convert the output to a list with land first, then waters
        geoAreas = []
        for g in gaLAND:
            geoAreas.append(g)
        for g in gaWATER:
            geoAreas.append(g)

        geoAreas.sort()

        return geoAreas



    def formatCountyColumns(self, counties, colWidth, lineLength):
        result = ''
        curCol = 0
        for county in counties:
            #print "county", len(county), county
            # Need 2 spaces between county names
            columns = (len(county)+2)/(colWidth+1) + 1
            countyWidth = columns*colWidth
            if curCol > 0 and curCol + countyWidth > lineLength:
                # Need to start on new line
                result = result + "\n"
                curCol = 0
            result = result + county.ljust(countyWidth)
            curCol = curCol + countyWidth
        return result

    def formatCountyString(self, state, counties):
        # IN MINNESOTA...  (if state defined) plus list
        if len(state):
            result = "\n\nIN " + state + "...\n"
        else:
            result = "\n"
        first = 1
        for county in counties:
            if first:
                result = result + county
                first = 0
            else:
                result = result + "..." + county
        result = self.replaceLast(result, "...", " AND ")
        return result

    def replaceLast(self, str, str1, str2):
        # Replace the last occurrence of str1 with str2
        count = str.count(str1)
        if count > 1:
            str = str.replace(str1, str2)
            str = str.replace(str2, str1, count-1)
        return str

    def getIssuedByString(self, words = "ISSUED BY NATIONAL WEATHER SERVICE "):
        issuedByString = ""
        try:
            if self._issuedBy is not None:
                issuedByString = "ISSUED BY NATIONAL WEATHER SERVICE " + \
                        self.getSiteInfo("wfoCityState",self._issuedBy) + "\n"
        except:
            pass
        return issuedByString

    def timeFromDDHHMM(self, dtgString):
        # converts a DDHHMM string into a time value.
        #group1=day, group2=hour, group3=minute, returns seconds since epoch
        try:
            wmo_day = int(dtgString[0:2])
            wmo_hour = int(dtgString[2:4])
            wmo_min = int(dtgString[4:6])
        except:
            s = "timeFromDDHHMM(), input string not in DDHHMM format: " +\
              dtgString
            raise Exception, s

        #reset time zone to GMT for this function
        prevTZ = os.environ.get('TZ', None)
        os.environ['TZ'] = "GMT0"

        #assemble time tuple
        gmtuple = time.gmtime(time.time())
        wmo_year = gmtuple[0]  #based on current time
        wmo_month = gmtuple[1] #based on current time
        current_day = gmtuple[2]
        if current_day - wmo_day > 15:
            # next  month
            wmo_month = wmo_month + 1
            if wmo_month > 12:
                wmo_month = 1
                wmo_year = wmo_year + 1
        elif current_day - wmo_day < -15:
            # previous month
            wmo_month = wmo_month -1
            if wmo_month < 1:
                wmo_month = 12
                wmo_year = wmo_year - 1

        s = `wmo_year` + "%02i" % wmo_month + "%02i" % wmo_day + \
          "%02i" % wmo_hour + "%02i" % wmo_min + "UTC"
        timeTuple = time.strptime(s, "%Y%m%d%H%M%Z")
        wmoTime = time.mktime(timeTuple)   #TZ is GMT0, so this mktime works

        #reset the time zone
        if prevTZ is not None:
            os.environ['TZ'] = prevTZ
        else:
            del os.environ['TZ']
        return wmoTime

    # Given the issuance time, offset in hours from local time midnight,
    # vtecString, returns the expire time (time_t), and the expire time 
    # as ddhhmm.
    def getExpireTimeFromLToffset(self, issueTime, offsetLTHours,
      vtecString, roundMinutes=15, fixedExpire=0):

        # get issue time in local time, then today's midnight local time
        lclTime = time.localtime(issueTime)
        midnight = time.mktime((lclTime[0], lclTime[1], lclTime[2], 
          0, 0, 0, 0, 0, 0))  #midnight today local time

        # calculate expire time as offset into the future
        expireTime = offsetLTHours*3600 + midnight  #actual time for expire

        #use the other getExpireTime()
        expireTime = self.getExpireTime(issueTime, expireTime, vtecString,
          roundMinutes, fixedExpire)

        ddhhmm = time.strftime("%d%H%M", time.gmtime(expireTime))

        return (expireTime, ddhhmm)

    # Given the issuance time, expiration time (desired), and the VTEC codes,
    # returns the appropriate expiration time.  Expiration time is the
    # earliest of the specified expiration time, 1 hr if a CAN code
    # is detected, or the ending time of ongoing events (CON, EXT, EXB, NEW).
    # The issueTime and expireTime are ints, VTECCodes are a list of
    # records from the Hazard's analyzed table. The fixedExpire flag
    # indicates to ignore the VTEC actions when computing the expiration time.
    def getExpireTime(self, issueTime, expireTime, vtecString, roundMinutes=15,
        fixedExpire=0):


        #hazard product
        if not fixedExpire:
            #vtec regular expression
            vtecRE = '/[OTEX]\.([A-Z]{3})\.([A-Z]{4})\.([A-Z]{2})\.' + \
              '([WAYSOFN])\.([0-9]{4})\.([0-9]{6})T([0-9]{4})Z\-' + \
              '([0-9]{6})T([0-9]{4})Z/'

            #break up the VTEC strings, decode ending time
            vstrs = vtecString.split('\n')
            canExpFound = 0
            activeFound = 0
            laterActive = None  #later end time of all active events
            for vs in vstrs:
                search =  re.search(vtecRE, vs)
                if search:
                    action = search.group(1)
                    if action in ['CAN','EXP']:
                        canExpFound = 1
                    elif action in ['NEW','CON','EXT','EXB','EXA']:
                        activeFound = 1
                        endTime = self.timeFromYYYYMMDD_HHMM(search.group(8),
                          search.group(9))
                        if endTime != 0:
                            if laterActive is not None:
                                laterActive = max(laterActive, endTime)
                            else:
                                laterActive = endTime
            if laterActive is not None:
                expireTime = min(expireTime, laterActive)
            elif canExpFound and not activeFound:
                expireTime = min(expireTime, issueTime+3600)  #1hr from now

        #ensure expireTime is not before issueTime, and is at least 1 hour
        if expireTime - issueTime < 3600:
            expireTime = issueTime + 3600


        #round to next "roundMinutes"
        roundValue = roundMinutes*60  #in seconds
        delta = expireTime % roundValue  # in seconds
        baseTime = int(expireTime/roundValue)*roundValue
        if delta/60 >= 1:   #add the next increment
            expireTime = baseTime + roundValue
        else:   #within 1 minute, don't add the next increment
            expireTime = baseTime

        return expireTime


    def timeFromYYYYMMDD_HHMM(self, yyyymmdd, hhmm):
        #returns a value of seconds since epoch from a encoded YYYYMMDD
        #and hhmm string.

        #if all zeros, return 0
        if yyyymmdd == "000000" and hhmm == "0000":
            return 0
        else:
            #reset time zone to GMT for this function
            prevTZ = os.environ.get('TZ', None)
            os.environ['TZ'] = "GMT0"

            timeString = yyyymmdd + hhmm
            timeTuple = time.strptime(timeString, "%y%m%d%H%M")
            seconds =  time.mktime(timeTuple)   #TZ is GMT0, so mktime works

            #reset the time zone
            if prevTZ is not None:
                os.environ['TZ'] = prevTZ
            else:
                del os.environ['TZ']

            return seconds

#############################

    def getAreaDictEntry(self, accessArg, dict, entryName, firstOnly=0):
        # Access the given area dictionary for the given entryName
        # using the accessArg to find the area (or areas)
        # "accessArg" can be:
        #   tree or argDict: Gets entry from current area
        #   "all": Gets all dict entries and returns a list
        #   a list of area labels: Gets a list of dict entries for the given areaLabels
        # If firstOnly==1, return only the value for the first area found
        # Otherwise, return a list of entry values from which duplicates have been removed
        # If not found, return an empty list
        entryValues = []
        for areaLabel in dict.keys():
            if accessArg != "all":
                if type(accessArg) is types.DictType:
                    # tree or argDict
                    if not self.currentAreaContains(tree, [areaLabel]):
                        continue
                if type(accessArg) is types.ListType:
                    # list of areas
                    if areaLabel not in accessArg:
                        continue
            entry = dict[areaLabel]
            if entry.has_key(entryName):
                entryValue = entry[entryName]
                if firstOnly:
                    # Only the FIRST entryValue is taken if multiple ones exist
                    return entryValue
                else:
                    if type(entryValue) is types.ListType:
                        entryValues += entryValue
                    else:
                        entryValues.append(entryValue)
        return self.removeDups(entryValues)

#############################

    def synopsisUGC(self, siteID, pil=None):
        # Get the synopsis UGC for the CWF
        synopsisDict = self.synopsisCWF_dict()
        if pil is None:
            ugcInfo = synopsisDict.get(siteID)
        else:
            ugcInfo = synopsisDict.get((siteID, pil))
        if ugcInfo is None:
            return ""
        ugc = ugcInfo[0]
        if type(ugc) is types.ListType:
            # Add hyphens e.g. AMZ600-GMZ606
            ugc = "-".join(ugc)
        return ugc

    def synopsisHeading(self, siteID, pil=None):
        # Get the synopsis heading for the CWF
        synopsisDict = self.synopsisCWF_dict()
        if pil is None:
            ugcInfo = synopsisDict.get(siteID)
        else:
            ugcInfo = synopsisDict.get((siteID, pil))
        if ugcInfo is None:
            return ""
        return self.endline(ugcInfo[1], self._lineLength)
        
    def synopsisCWF_dict(self):
        #key is site id, or (site id, pil) for sites that produce multiple CWFs
        #value is ('ugcCode','ugcDescription')
        return {
         'CAR': ('ANZ005', 
            'Synopsis for Eastport ME to Stonington (Deer Isle) ME out 25 NM'),
         'GYX': ('ANZ100',
            'Synopsis for Stonington (Deer Isle) ME to Merrimack River MA out 25 NM'),
         'BOX': ('ANZ200', 
            'Synopsis for MA and RI waters'),
         'OKX': ('ANZ300',
            'Synopsis for Long Island waters and New York Harbor'),
         'PHI': ('ANZ400',
            'Synopsis for Sandy Hook NJ to Fenwick Island DE'),
         'LWX': ('ANZ500',
            'Synopsis for North Chesapeake Bay and the Tidal Potomac'),
         'AKQ': ('ANZ600',
            'Synopsis for Fenwick Island DE to Currituck Beach Light NC out ' +\
             '20 NM including Virginia portion of the Chesapeake Bay and ' +\
             'Currituck Sound'),
         'MHX': ('AMZ100',
            'Synopsis for Currituck Beach Light to Surf City NC out 20 NM ' +\
              'including Albemarle Sound and Pamlico Sound'),
         'ILM': ('AMZ200',
            'Synopsis for Surf City NC to South Santee River SC out 20 NM'),
         'CHS': ('AMZ300',
            'Synopsis for South Santee River SC to Savannah GA out 20 NM ' +\
             'and Savannah GA to Altamaha Sound GA out 60 NM...including ' +\
             'Charleston Harbor and Grays Reef National Marine Sanctuary'),
         'JAX': ('AMZ400',
            'Synopsis for Altamaha Sound GA to Flagler Beach FL out 60 NM'),
         'MLB': ('AMZ500',
            'Synopsis for Flagler Beach to Jupiter Inlet FL out 60 NM'),
         'MFL': (['AMZ600','GMZ606'],
            'Synopsis for Jupiter Inlet to Ocean Reef FL including Biscayne ' +\
             'Bay out 60 NM and for East Cape Sable to Bonita Beach FL out 60 NM'),
         'SJU': ('AMZ700',
            'Synopsis for PR and U.S. Virgin Islands coastal waters'),
         'KEY': ('GMZ005',
            'Synopsis for Florida Bay and the Keys'),
         'TBW': ('GMZ800',
            'Synopsis for Bonita Beach to Suwannee River FL out 60 NM'),
         'TAE': ('GMZ700',
            'Synopsis for Suwannee River to Destin FL out 60 NM'),
         'MOB': ('GMZ600',
            'Synopsis for Destin FL to Pascagoula MS out 60 NM including Mobile Bay'),
         'LIX': (['GMZ500','GMZ501'],
            'Synopsis for Pascagoula MS to Southwest Pass of the Mississippi River' +\
             ' and Southwest Pass of the Mississippi River to Atchafalaya River LA' +\
             ' out 60 NM'),
         'LCH': ('GMZ400',
            'Synopsis for Atchafalaya River LA to High Island TX out 60 NM'),
         'HGX': ('GMZ300',
            'Synopsis for High Island to Matagorda Ship Channel TX out 60 NM'),
         'CRP': ('GMZ200',
            'Synopsis for Baffin Bay to Matagorda Ship Channel out 60 NM'),
         'BRO': ('GMZ100',
            'Synopsis from Baffin Bary to Rio Grande River TX out 60 NM'),
         'SEW': ('PZZ100',
            'Synopsis for Northern Washington Coast and Puget Sound'),
         'PQR': ('PZZ200',
            'Synopsis for Southern Washington and Northern Oregon Coast'),
         'MFR': ('PZZ300',
            'Synopsis for the Southern Oregon Coastal Waters'),
         'EKA': ('PZZ400',
            'Synopsis for Northern California Waters'),
         'MTR': ('PZZ500',
            'Synopsis for Central California Coast and Bays'),
         'LOX': ('PZZ600',
            'Synopsis for Southern California Coast and Santa Barbara Channel'),
         'SGX': ('PZZ700',
            'Synopsis for Far Southern California Coast'),
         'AJK': ('PKZ098',
            'Synopsis for Inside Waters'),
         'AER': (['PKZ196','PKZ197'],
            'Synopsis for the North Gulf Coast and Valdez Port Narrows and Arm'),
         'ALU': (['PKZ198','PKZ199'],
            'Synopsis for Southwest Alaska and the Aluetians ' +\
             'including Chiniak and Marmot Bays'),
         ('AFG','WCZ'): ('PKZ299', 'Synopsis for Northwest Coast'),
         ('AFG','NSB'): ('PKZ298', 'Synopsis for Arctic Coast'),
         'HFO': ('PHZ100', 'Synopsis for Hawaiian Waters'),
         'GUM': ('PMZ150', 'Synopsis for Marianas Waters'),
          }



    
        



