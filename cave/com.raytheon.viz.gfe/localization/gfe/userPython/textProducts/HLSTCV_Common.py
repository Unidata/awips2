import GenericHazards
import JsonSupport
import string, time, os, re, types, copy, LogStream, collections
import ModuleAccessor, SampleAnalysis, EditAreaUtils
import math


try:  # See if this is the AWIPS I environment
    import AFPS
    from AFPS import AbsTime
    from IFPDialog import Dialog
    AWIPS_ENVIRON = "AWIPS1"
except:  # Must be the AWIPS II environment
    from AbsTime import *
    from StartupDialog import IFPDialog as Dialog
    from LockingFile import File
    from com.raytheon.uf.common.localization import PathManagerFactory
    from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType
    AWIPS_ENVIRON = "AWIPS2"

class TextProduct(GenericHazards.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    def __init__(self):
        GenericHazards.TextProduct.__init__(self)

    ######################################################
    #  Populate Product Parts for HLS and TCV
    ######################################################
        
    ################# Product Level
          
    def _wmoHeader(self, productDict, productSegmentGroup, arguments=None):
        headerDict = collections.OrderedDict()
        headerDict['TTAAii'] = self._wmoID
        headerDict['originatingOffice'] = self._backupFullStationID  # Will be siteID if not in backup mode
        headerDict['productID'] = self._productID
        headerDict['siteID'] = self._site
        headerDict['fullStationID'] = self._fullStationID
        headerDict['ddhhmmTime'] = self._ddhhmmTime
        productDict['wmoHeader'] = headerDict

    def _productHeader(self, productDict, productSegmentGroup, arguments=None):
        headerDict = dict()
        headerDict['disclaimer'] = 'This XML wrapped text product should be considered COMPLETELY EXPERIMENTAL. The National Weather Service currently makes NO GUARANTEE WHATSOEVER that this product will continue to be supplied without interruption. The format of this product MAY CHANGE AT ANY TIME without notice.'
        headerDict['cityState'] = self._wfoCityState
        headerDict['stormNumber'] = self._getStormNumberStringFromTCP()
        self._productName = self.checkTestMode(
                self._argDict, productSegmentGroup.get('productName') + self._areaName)
        headerDict['productName'] = self._productName
        headerDict['stormType'] = self._getStormTypeFromTCP()
        headerDict['stormName'] = self._getStormNameFromTCP()
        headerDict['advisoryType'] = self._getAdvisoryTypeFromTCP()
        headerDict['advisoryNumber'] = self._getAdvisoryNumberStringFromTCP()
        headerDict['issuedByString'] = self.getIssuedByString()
        headerDict['issuanceTimeDate'] = self._timeLabel
        productDict['productHeader'] = headerDict

    ###############################################################
    ### Hazards related methods
    
    def _initializeHazardsTable(self, argDict):
        import VTECMessageType
        productID = self._pil[0:3]
        vtecMode = VTECMessageType.getVTECMessageType(productID.upper())
        argDict["vtecMode"] = vtecMode
        
        self._setVTECActiveTable(argDict)
        
        # Need to check hazards against all edit areas in the CWA MAOR
        argDict["combinations"]= [(self._allAreas(),"Region1")]

        self._hazardsTable = self._getHazardsTable(argDict, self.filterMethod)
        argDict["hazards"] = self._hazardsTable

    def _setVTECActiveTable(self, argDict):
        dataMgr = argDict["dataMgr"]
        gfeMode = dataMgr.getOpMode().name()
            
        if gfeMode == "PRACTICE":
            argDict["vtecActiveTable"] = "PRACTICE"
        else:
            argDict["vtecActiveTable"] = "active"

    def _getAllVTECRecords(self):
        allRecords = []
        for segment in self._segmentList:
            vtecRecords = self._hazardsTable.getHazardList(segment)
            allRecords += vtecRecords
        
        return allRecords

    def _ignoreActions(self):
        # Ignore hazards with these action codes in the overview headlines
        # NOTE: the VTEC and segments will still include them correctly.
        return ['CAN', 'UPG']
    
    # In order to have the HazardsTable use the allowedHeadlines list,
    # we need to supply a filterMethod that uses allowedHeadlines instead of allowedHazards
    def _getAllowedHazardList(self, allowedHazardList=None):
        if allowedHazardList is None:
            allowedHazardList = self.allowedHazards()
        hazardList = []
        for h in allowedHazardList:
            if type(h) is types.TupleType:
                hazardList.append(h[0])
            else:
                hazardList.append(h)
        return hazardList

    def _altFilterMethod(self, hazardTable, allowedHazardsOnly=False):
        # Remove hazards not in allowedHeadlines list
        allowedHazardList = self._getAllowedHazardList(self.allowedHeadlines())
        return self._filterHazards(hazardTable, allowedHazardList,
                                   allowedHazardsOnly)
    
    def _filterHazards(self, hazardTable, allowedHazardList,
                       allowedHazardsOnly=False):
        newTable = []
        hazStr = ""
        for i in range(len(hazardTable)):
            if hazardTable[i]['sig'] != "":   # VTEC
                hazStr = hazardTable[i]['phen'] + "." + hazardTable[i]['sig']
            else:   #non-VTEC
                hazStr = hazardTable[i]['phen']

            if hazStr in allowedHazardList:
                newTable.append(hazardTable[i])
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
    
    def _getAdditionalHazards(self):
        argDict = self._argDict
        argDict['definition'] = self._definition
        altHazards = self._getHazardsTable(argDict, self._altFilterMethod)
        conTable = altHazards.consolidatedTableByID()

        # Consolidate across action codes
        hazDict = {}
        for hazard in conTable:
            hdln=hazard['hdln']
            phen=hazard['phen']
            sig=hazard['sig']
            act=hazard['act']
            if act in self._ignoreActions():
                continue
            for area in hazard['id']:
                hazDict.setdefault((hdln, phen, sig), []).append(area)

        #print "hazDict", hazDict
        hazardHdlns=[]
        huAreas = []
#        print "\nAdditional Hazard Headlines"
        for key in hazDict.keys():
            hdln, phen, sig = key
            huAreas = huAreas + hazDict[key]
            hazardHdln = ((hdln, "NEW", phen,sig), hazDict[key], [],[],[])
            #print "   ", hazardHdln, hazDict[key]
            hazardHdlns.append(hazardHdln)
        return hazardHdlns, huAreas
    
    def _checkHazard(self, hazardHdlns, phenSigList, checkAreaTypes=None,
                    checkAreas=None, returnList=False, mode="any", includeCAN=False):
        # Given a list of hazards in the form
        #    (key, landList, marineList, coastalList, inlandList)
        #  where key is (hdln, act, phen, sig) and the lists show which areas
        #    contain the hazard
        # If mode == "any":
        #     Check to see if any of the given phenSigList = [(phen, sig), (phen, sig)]
        #          are found
        # If mode == "all":
        #     Check to see if all of the given phenSigList are found
        # IF checkAreaTypes is given, then check against that particular area type(s) i.e.
        #   "land", "marine", etc.
        # IF checkAreas is given, only return areas that are in that list
        # IF returnList=True, returns a list of (key, areas) that meet the criteria
        # IF includeCAN is True then CAN hazards will be included as well.
        #     Otherwise, they are ignored.
        #
        # E.g. hdlnList = self._checkHazard(hazardHdlns, [("FA","W")], returnList=True)
        print "_checkHazard hazardHdlns is ", hazardHdlns
        print "_checkHazard phenSigList is ", phenSigList
        chosen = []
        for key, landList, marineList, coastalList, inlandList in hazardHdlns:
#            print "what is mode?", mode

            #  SARAH - we do not want to consider marine hazards in this product
#             hazAreas = landList+marineList
            hazAreas = landList
            hazValue = (key, hazAreas)
            print "hazValue is ", hazValue
            hdln, act, phen, sig = key
            if not includeCAN and act == "CAN":
                continue
            for checkPhen, checkSig in phenSigList:
                print "checkPhen is ", checkPhen
                print "checkSig is ", checkSig
                if phen == checkPhen and sig == checkSig:
                    if checkAreaTypes is not None:
                        # Check for land, marine, etc.
                        for checkAreaType in checkAreaTypes:
                            exec "testList = " + checkAreaType + "List"
#                            print "testList is", testList
                            if testList != []:
                                chosen.append(hazValue)
#                                print "chosen is ", chosen
                    elif checkAreas is not None:
                        acceptedAreas=[]
                        for hazArea in hazAreas:
                            if hazArea in checkAreas:
                                acceptedAreas.append(hazArea)
                        if acceptedAreas!=[]:
                            chosen.append((key, acceptedAreas))
                    else:
                        chosen.append(hazValue)
                    if not returnList and chosen!=[]: break
        
        print "MATT _checkHazard chosen = %s" % (repr(chosen))
        if not returnList:
            return chosen!=[]
        return chosen
    
    def getVtecRecords(self, segment, vtecEngine=None):
        vtecRecords = self._hazardsTable.getHazardList(segment)
        return vtecRecords
    
    def _getHazardsTable(self, argDict, filterMethod, editAreas=None):
        # Set up edit areas as list of lists
        # Need to check hazards against all edit areas in the CWA MAOR
        argDict["combinations"]= [(self._allAreas(),"Region1")]
        dfEditAreas = argDict["combinations"]
        editAreas = []
        for area, label in dfEditAreas:
            if type(area) is types.ListType:
                editAreas.append(area)
            elif type(area) is types.TupleType: #LatLon
                editAreas.append([self.__getLatLonAreaName(area)])
            else:
                editAreas.append([area])
        # Get Product ID and other info for HazardsTable
        pil = self._pil.upper()   #  Ensure PIL is in UPPERCASE
        stationID4 = self._fullStationID
        productCategory = pil[0:3]   #part of the pil
        definition = argDict['definition']
        sampleThreshold = definition.get("hazardSamplingThreshold", (10, None))
        # Process the hazards
        accurateCities = definition.get('accurateCities', 0)
        cityRefData = []
        import HazardsTable
        hazards = HazardsTable.HazardsTable(
          argDict["ifpClient"], editAreas, productCategory, filterMethod,
          argDict["databaseID"],
          stationID4, argDict["vtecActiveTable"], argDict["vtecMode"], sampleThreshold,
          creationTime=argDict["creationTime"], accurateCities=accurateCities,
          cityEditAreas=cityRefData, dataMgr=argDict['dataMgr'])
        return hazards

    ###############################################################
    ### Time related methods
    
    def _initializeTimeVariables(self, argDict):
        argDict['creationTime'] = int(time.time()/60)*60
        self._issueTime_secs = argDict['creationTime']
        self._issueTime = self._issueTime_secs * 1000 # in milliseconds
        
        self._ddhhmmTime = self.getCurrentTime(
            argDict, "%d%H%M", shiftToLocal=0, stripLeading=0)
        self._currentTime = self._issueTime_secs
        self._expireTime = self._issueTime_secs + self._purgeTime*3600
        self._timeLabel = self.getCurrentTime(
            argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1)

    def _determineTimeRanges(self, argDict):
        # Set up the time range for 0-120 hours

        # Create a time range from the issuanceHour out 120 hours
        startTime = self._calculateStartTime(time.gmtime(self._issueTime_secs))
        self._timeRange = self.makeTimeRange(startTime, startTime+120*3600)

        # Determine the time range list, making sure they are on hour boundaries
        #   w.r.t. midnight today according to the resolution
        subRanges = self.divideRange(self._timeRange, self._resolution())
        trList = []
        for tr in subRanges:
            # print tr
            trList.append((tr, "Label"))
        self._timeRangeList = trList
    
    def _calculateStartTime(self, localCreationTime):
        year = localCreationTime[0]
        month = localCreationTime[1]
        day = localCreationTime[2]
        hour = localCreationTime[3]
        
        #  If we are more than halfway though a 6 hr period
        if hour % 6 > 3:
            adjust = 6      #  move on to the next 6 hr block
        else:
            adjust = 0
#         print "MATT: _calculateStartTime %d   adjust = %d" % (hour % 6, adjust)
        
        #  Now "truncate" to a 6-hourly boundary and compute startTime in local Time.
        hour =  int( (hour/6) * 6) + adjust
        if hour > 23:
            hour -= 24
        elif hour < 0:
            hour += 24

        startTime = absTimeYMD(year, month, day, hour)
        
        return startTime
    
    def _resolution(self):
        return 3

    def _formatPeriod(self, period, wholePeriod=False, shiftToLocal=True, useEndTime=False,
                      resolution=3):
        # Format period (a timeRange) resulting in
        #     DAY + MORNING / AFTERNOON / EVENING / OVERNIGHT.
        # If wholePeriod, format FROM ... TO...

        print "\nMATT Format period wholePeriod = %s, period = %s, useEndTime =%s" % (str(wholePeriod), str(period), str(useEndTime))
        if period is None:  return ""
        if useEndTime:
            startTime = period.endTime()
        else:
            startTime = period.startTime()
        result = self._getTimeDesc(startTime, resolution, shiftToLocal)
        print "MATT result = '%s'" % (result)
        if wholePeriod:
            endResult = self._getTimeDesc(period.endTime(), resolution, shiftToLocal)
            print "MATT endResult = '%s'" % (endResult)
            if result != endResult:
                result=result + " TO "+ endResult 
        return result

    def _getTimeDesc(self, startTime, resolution=3, shiftToLocal=True):
        # Create phrase such as Tuesday morning
        # Handle today/tonight and "this" morning/afternoon/etc..
        #
        print "\n\n**************Formatting Period for GMT starttime ", startTime
        labels = self.Labels()["SimpleWorded"]
        currentTime = self._timeRange.startTime()
        print "   currentTime = %s" % (repr(currentTime))  
        if shiftToLocal:
            currentLocalTime, shift = self.determineTimeShift()
            startTime = startTime + shift
            currentTime = currentTime + shift
            print "  shift, shifted start, current", shift/3600, startTime, currentTime
        hour = startTime.hour
        prevDay = False
        prevDay, partOfDay = self._getPartOfDay(hour, resolution)
#         if prevDay:
#             startTime = startTime - 24*3600
        todayFlag = currentTime.day == startTime.day
        if todayFlag:
            if partOfDay.upper().find("MIDNIGHT")>0: todayWord = "tonight"
            else: todayWord = "THIS"
            weekday = todayWord
        else:
            weekday = labels["Weekday"][startTime.weekday()]
        if partOfDay.find("<weekday>") >= 0:
            result = partOfDay.replace('<weekday>', weekday)
        else:
            result =  weekday + " " + partOfDay
        print "Result = '%s'" % (result)
        return result

    def _getPartOfDay(self, hour, resolution):
        prevDay = False
        if resolution == 3:
            if hour < 3:
                prevDay = True
                partOfDay = "early <weekday> morning"
#                 partOfDay = "AFTER MIDNIGHT"
            elif hour < 6:
                partOfDay = "early <weekday> morning"
            elif hour < 9:
                partOfDay = "morning"
            elif hour < 12:
                partOfDay = "late <weekday> morning"
            elif hour < 15:
                partOfDay = "early <weekday> afternoon"
            elif hour < 18:
                partOfDay = "late <weekday> afternoon"
            elif hour < 21:
                partOfDay = "early <weekday> evening"
            else:
                partOfDay = "late <weekday> evening"    
        else:
            if hour < 6:
                prevDay = True
#                 partOfDay = "AFTER MIDNIGHT"
                partOfDay = "early <weekday> morning"
            elif hour < 12: partOfDay = "morning"
            elif hour < 18: partOfDay =  "afternoon"
            else: partOfDay = "evening"
        return prevDay, partOfDay
    
    ###############################################################
    ### Sampling and Statistics related methods
    
    def moderated_dict(self, parmHisto, timeRange, componentName):
        """
           Specifies the lower percentages and upper percentages of
           data to be thrown out for moderated stats.
        """
        # COMMENT: This dictionary defines the low and high limit at which
        # outliers will be removed when calculating moderated stats.
        # By convention the first value listed is the percentage
        # allowed for low values and second the percentage allowed
        # for high values.

        # Get Baseline thresholds
        dict = SampleAnalysis.SampleAnalysis.moderated_dict(
            self, parmHisto, timeRange, componentName)

        #  Change thresholds
        dict["Wind"] = (0, 15)
        dict["WindGust"] = (0, 15)
        dict["pws34int"] = (0, 5)
        dict["pws64int"] = (0, 5)
        dict["pwsD34"] = (0, 5)
        dict["pwsN34"] = (0, 5)
        dict["pwsD64"] = (0, 5)
        dict["pwsN64"] = (0, 5)
        dict["InundationMax"] = (0, 5)
        dict["InundationTiming"] = (0, 5)
        return dict
    
    def _getStatValue(self, statDict, element, method=None, dataType=None):
        stats = statDict.get(element, None)
        if stats is None: return None
        if type(stats) is types.ListType:
            stats = stats[0]
            stats, tr = stats
        if dataType==self.VECTOR():
            stats, dir = stats
        return self.getValue(stats, method)
    
    ###############################################################
    ### Area, Zone and Segment related methods
    
    def _allAreas(self):
        return self._inlandAreas() + self._coastalAreas()
    
    def _computeIntersectAreas(self, editAreas, argDict):
        editAreaUtils = EditAreaUtils.EditAreaUtils()
        editAreaUtils.setUp(None, argDict)
        surgeEditArea = editAreaUtils.getEditArea("StormSurgeWW_EditArea", argDict)
        intersectAreas =[]
        for (_, editAreaLabel) in editAreas:
            editArea = editAreaUtils.getEditArea(editAreaLabel, argDict)
            intersectAreaLabel = "intersect_"+editAreaLabel
            intersectArea = editAreaUtils.intersectAreas(intersectAreaLabel, editArea, surgeEditArea)
            grid = intersectArea.getGrid()
            if grid.isAnyBitsSet():
                editAreaUtils.saveEditAreas([intersectArea])
                intersectAreas.append((intersectAreaLabel, intersectAreaLabel))
                
        return intersectAreas

    ###############################################################
    ### Storm Information and TCP related methods
    
    def _initializeStormInformation(self):
        self._stormType = None
        self._stormName = None
        self._advisoryType = None
        self._advisoryNumber = None
        self._stormNumber = None
        
        if self._useTestTCP():
            self._TCP = self._testTCP()
        elif "Enter PIL below" in self._StormInfo:
            if len(self._StormInfo_entry.strip()) == 0:
                return "You need to enter the PIL"
            else:
                #  Ensure PIL is in UPPERCASE
                self._TCP = self.getPreviousProduct(self._StormInfo_entry.strip().upper())
        else:
            self._TCP = self.getPreviousProduct(self._StormInfo)
        
        self._parseTCP(self._TCP)
        
        return None

    def _parseTCP(self, tcp):
        #  This pattern will handle multiple word names
        #  (including certain special characters).
        #  This is for the NHC format.
        mndSearch = re.search("(?im)^.*?(?P<stormType>HURRICANE|(SUB|POST.?)?TROPICAL " +
                              "(STORM|DEPRESSION|CYCLONE)|(SUPER )?TYPHOON|" +
                              "REMNANTS OF) (?P<stormName>[A-Z0-9\-\(\) ]+?)" +
                              "(?P<advisoryType>SPECIAL |INTERMEDIATE )?ADVISORY " +
                              "NUMBER[ ]+(?P<advisoryNumber>[A-Z0-9]+)[ ]*", tcp)
        
        if mndSearch is not None:
            self._stormType = mndSearch.group("stormType").strip()
            self._stormName = mndSearch.group("stormName").strip()
            advisoryType = mndSearch.group("advisoryType")
            if advisoryType is not None:
                self._advisoryType = advisoryType.strip()
            self._advisoryNumber = mndSearch.group("advisoryNumber").strip()
        
        senderSearch = re.search("(?im)^(?P<sender>(NWS (TPC/)?NATIONAL HURRICANE CENTER|" +
                                 "NATIONAL WEATHER SERVICE).*?)$", tcp)
        
        if senderSearch is not None:
            sender = senderSearch.group("sender")
            senderParts = sender.split(" ")
            # If the storm number is mentioned, it will be the last "word" of the line
            stormNumber = senderParts[-1]
            if len(stormNumber) == 8 and \
               stormNumber[0:2].isalpha() and \
               stormNumber[2:].isdigit():
                self._stormNumber = stormNumber.strip()

    def _getStormTypeFromTCP(self):
        return self._stormType
    
    def _getStormNameFromTCP(self):
        return self._stormName
    
    def _getAdvisoryTypeFromTCP(self):
        return self._advisoryType
    
    def _getAdvisoryNumberStringFromTCP(self):
        return self._advisoryNumber
    
    def _getStormNumberStringFromTCP(self):
        if self._stormNumber is not None:
            return self._stormNumber
        else:
            return ""
    
    ## Used for testing and debugging
    def _useTestTCP(self):
        #return True
        return False
    
    def _testTCP(self):
        return \
"""337 
WTNT34 KNHC 250256
TCPAT4
 
BULLETIN
TROPICAL STORM ISAAC ADVISORY NUMBER  16
NWS NATIONAL HURRICANE CENTER MIAMI FL       AL092012
1100 PM EDT FRI AUG 24 2012
 
...ISAAC GETTING BETTER ORGANIZED AS IT MOVES NORTHWESTWARD TOWARD
HAITI...
 
 
SUMMARY OF 1100 PM EDT...0300 UTC...INFORMATION
-----------------------------------------------
LOCATION...17.7N 72.5W
ABOUT 65 MI...100 KM SSW OF PORT AU PRINCE HAITI
ABOUT 245 MI...395 KM SE OF GUANTANAMO CUBA
MAXIMUM SUSTAINED WINDS...70 MPH...110 KM/H
PRESENT MOVEMENT...NW OR 310 DEGREES AT 14 MPH...22 KM/H
MINIMUM CENTRAL PRESSURE...990 MB...29.23 INCHES
 
 
WATCHES AND WARNINGS
--------------------
CHANGES WITH THIS ADVISORY...
 
A HURRICANE WATCH AND A TROPICAL STORM WARNING HAVE BEEN ISSUED FOR
ALL OF THE FLORIDA KEYS...INCLUDING FLORIDA BAY...AND FOR THE COAST
OF THE SOUTHERN FLORIDA PENINSULA FROM OCEAN REEF ON THE EAST COAST
WESTWARD TO BONITA BEACH ON THE WEST COAST.
 
A TROPICAL STORM WARNING HAS BEEN ISSUED FOR THE SOUTHEAST FLORIDA
COAST FROM NORTH OF OCEAN REEF NORTHWARD TO JUPITER INLET...AND FOR
LAKE OKEECHOBEE.
 
THE GOVERNMENT OF THE BAHAMAS HAS ISSUED A TROPICAL STORM WARNING
FOR ALL OF THE NORTHWESTERN BAHAMAS.
 
A TROPICAL STORM WATCH HAS BEEN ISSUED FOR THE EAST-CENTRAL FLORIDA
COAST FROM NORTH OF JUPITER INLET TO SEBASTIAN INLET.
 
THE CAYMAN ISLANDS METEOROLOGICAL SERVICE HAS ISSUED A TROPICAL
STORM WATCH FOR THE CAYMAN ISLANDS.
 
SUMMARY OF WATCHES AND WARNINGS IN EFFECT...
 
A HURRICANE WATCH IS IN EFFECT FOR...
* HAITI
* FLORIDA KEYS INCLUDING THE DRY TORTUGAS
* FLORIDA BAY
* THE FLORIDA EAST COAST FROM OCEAN REEF SOUTHWARD
* THE FLORIDA WEST COAST FROM BONITA BEACH SOUTHWARD
 
A TROPICAL STORM WARNING IS IN EFFECT FOR...
* DOMINICAN REPUBLIC
* HAITI
* CUBAN PROVINCES OF CIEGO DE AVILA...SANCTI SPIRITUS...VILLA
CLARA...CAMAGUEY...LAS TUNAS...GRANMA...HOLGUIN...SANTIAGO DE
CUBA...AND GUANTANAMO
* THE BAHAMAS
* TURKS AND CAICOS ISLANDS
* THE FLORIDA KEYS INCLUDING THE DRY TORTUGAS
* THE FLORIDA EAST COAST FROM JUPITER INLET SOUTHWARD
* THE FLORIDA WEST COAST FROM BONITA BEACH SOUTHWARD
* FLORIDA BAY AND LAKE OKEECHOBEE
 
A TROPICAL STORM WATCH IS IN EFFECT FOR...
* CUBAN PROVINCES OF MATANZAS AND CIENFUEGOS
* JAMAICA
* THE FLORIDA EAST COAST NORTH OF JUPITER INLET TO SEBASTIAN INLET
 
A HURRICANE WATCH MEANS THAT HURRICANE CONDITIONS ARE POSSIBLE
WITHIN THE WATCH AREA...IN THIS CASE WITHIN THE NEXT 24 TO 36 HOURS.
 
A TROPICAL STORM WARNING MEANS THAT TROPICAL STORM CONDITIONS ARE
EXPECTED SOMEWHERE WITHIN THE WARNING AREA WITHIN 36 HOURS.
 
A TROPICAL STORM WATCH MEANS THAT TROPICAL STORM CONDITIONS ARE
POSSIBLE WITHIN THE WATCH AREA...GENERALLY WITHIN 48 HOURS.
 
INTERESTS IN THE REMAINDER OF CUBA AND THE REMAINDER OF THE FLORIDA
PENINSULA SHOULD MONITOR THE PROGRESS OF ISAAC.
 
FOR STORM INFORMATION SPECIFIC TO YOUR AREA IN THE UNITED
STATES...INCLUDING POSSIBLE INLAND WATCHES AND WARNINGS...PLEASE
MONITOR PRODUCTS ISSUED BY YOUR LOCAL NATIONAL WEATHER SERVICE
FORECAST OFFICE. FOR STORM INFORMATION SPECIFIC TO YOUR AREA OUTSIDE
THE UNITED STATES...PLEASE MONITOR PRODUCTS ISSUED BY YOUR NATIONAL
METEOROLOGICAL SERVICE.
 
 
DISCUSSION AND 48-HOUR OUTLOOK
------------------------------
AT 1100 PM EDT...0300 UTC...THE CENTER OF TROPICAL STORM ISAAC WAS
LOCATED NEAR LATITUDE 17.7 NORTH...LONGITUDE 72.5 WEST. ISAAC IS
MOVING TOWARD THE NORTHWEST NEAR 14 MPH...22 KM/H...BUT IS EXPECTED
TO RESUME A FASTER FORWARD SPEED TOWARD THE NORTHWEST TONIGHT
THROUGH SUNDAY.  ON THE FORECAST TRACK...THE CENTER OF ISAAC SHOULD
MAKE LANDFALL IN HAITI TONIGHT...MOVE NEAR OR OVER SOUTHEASTERN
CUBA ON SATURDAY...MOVE NEAR OR OVER CENTRAL CUBA SATURDAY NIGHT...
AND APPROACH THE FLORIDA KEYS ON SUNDAY.
 
MAXIMUM SUSTAINED WINDS ARE NEAR 70 MPH...110 KM/H...WITH HIGHER
GUSTS. LITTLE CHANGE IN STRENGTH IS LIKELY BEFORE LANDFALL...
FOLLOWED BY SOME WEAKENING AS THE CENTER CROSSES HAITI AND
SOUTHEASTERN CUBA.
 
TROPICAL-STORM-FORCE WINDS EXTEND OUTWARD UP TO 230 MILES...
370 KM...MAINLY NORTHWEST AND NORTHEAST OF THE CENTER.
 
ESTIMATED MINIMUM CENTRAL PRESSURE IS 990 MB...29.23 INCHES.
 
 
HAZARDS AFFECTING LAND
----------------------
RAINFALL...TOTAL RAINFALL ACCUMULATIONS OF 8 TO 12 INCHES...WITH
MAXIMUM AMOUNTS OF 20 INCHES...ARE POSSIBLE OVER HISPANIOLA. THESE
RAINS COULD CAUSE LIFE-THREATENING FLASH FLOODS AND MUD SLIDES.
TOTAL RAIN ACCUMULATIONS OF 4 TO 8 INCHES...WITH MAXIMUM AMOUNTS OF
12 INCHES...ARE POSSIBLE ACROSS JAMAICA...THE CENTRAL AND EASTERN
PORTIONS OF CUBA...THE FLORIDA KEYS AND THE SOUTHERN PENINSULA OF
FLORIDA.  TOTAL RAIN ACCUMULATIONS OF 2 TO 4 INCHES ARE POSSIBLE
OVER THE CENTRAL AND SOUTHEASTERN BAHAMAS.
 
WIND...TROPICAL STORM CONDITIONS ARE SPREADING OVER PORTIONS OF THE
DOMINICAN REPUBLIC AND HAITI...WITH HURRICANE CONDITIONS POSSIBLE IN
HAITI. TROPICAL STORM CONDITIONS ARE EXPECTED OVER THE SOUTHEASTERN
BAHAMAS AND THE TURKS AND CAICOS ISLANDS TONIGHT...ARE EXPECTED
OVER THE CENTRAL BAHAMAS BY SATURDAY OR SATURDAY NIGHT...AND ARE
EXPECTED OVER THE NORTHWESTERN BAHAMAS BY SUNDAY. TROPICAL STORM
CONDITIONS ARE EXPECTED OVER EASTERN CUBA BY TONIGHT AND OVER
CENTRAL CUBA BY SATURDAY OR SATURDAY NIGHT. TROPICAL STORM
CONDITIONS ARE EXPECTED TO REACH NORTHWESTERN CUBA AND THE
NORTHWESTERN BAHAMAS BY SATURDAY NIGHT OR SUNDAY...AND SOUTH
FLORIDA AND THE FLORIDA KEYS ON SUNDAY. HURRICANE CONDITIONS ARE
POSSIBLE OVER THE FLORIDA KEYS...FLORIDA BAY...AND THE SOUTHERNMOST
FLORIDA PENINSULA BY SUNDAY EVENING.
 
STORM SURGE...THE COMBINATION OF A STORM SURGE AND THE TIDE WILL
CAUSE NORMALLY DRY AREAS NEAR THE COAST TO BE FLOODED BY RISING
WATERS. THE WATER COULD REACH THE FOLLOWING DEPTHS ABOVE GROUND
IF THE PEAK SURGE OCCURS AT THE TIME OF HIGH TIDE...
 
SOUTH FLORIDA INCLUDING THE FLORIDA KEYS...2 TO 4 FT
HISPANIOLA AND EASTERN CUBA...1 TO 3 FT
THE BAHAMAS AND TURKS AND CAICOS...1 TO 3 FT
 
THE DEEPEST WATER WILL OCCUR ALONG THE IMMEDIATE COAST IN AREAS OF
ONSHORE FLOW. SURGE-RELATED FLOODING DEPENDS ON THE RELATIVE TIMING
OF THE SURGE AND THE TIDAL CYCLE...AND CAN VARY GREATLY OVER SHORT
DISTANCES. FOR INFORMATION SPECIFIC TO YOUR AREA...PLEASE SEE
PRODUCTS ISSUED BY YOUR LOCAL WEATHER SERVICE OFFICE. NEAR THE
COAST...THE SURGE WILL BE ACCOMPANIED BY DANGEROUS WAVES.
 
SURF...DANGEROUS SURF AND RIP CURRENT CONDITIONS WILL AFFECT PUERTO
RICO...HISPANIOLA...THE BAHAMAS...THE TURKS AND CAICOS...EASTERN
AND CENTRAL CUBA...AND THE EAST COAST OF FLORIDA AND THE FLORIDA
KEYS DURING THE NEXT COUPLE OF DAYS. PLEASE CONSULT PRODUCTS FROM
YOUR LOCAL WEATHER OFFICE FOR MORE INFORMATION.
 
 
NEXT ADVISORY
-------------
NEXT INTERMEDIATE ADVISORY...200 AM EDT.
NEXT COMPLETE ADVISORY...500 AM EDT.
 
$$
FORECASTER STEWART"""

    ###############################################################
    ### Advisory related methods

    def _synchronizeAdvisories(self):
        pathManager = PathManagerFactory.getPathManager()
        context = pathManager.getContextForSite(LocalizationType.CAVE_STATIC, self._site)
        
        # Retrieving a directory causes synching to occur
        file = pathManager.getLocalizationFile(context, self._getAdvisoryPath()).getFile()
        
        return file
    
    def _getLocalAdvisoryDirectoryPath(self):
        file = self._synchronizeAdvisories()

        path = file.getPath()
        return path
    
    def _loadLastTwoAdvisories(self):
        advisoryDirectoryPath = self._getLocalAdvisoryDirectoryPath()
        filenames = os.listdir(advisoryDirectoryPath)
        allAdvisories = filter(lambda filename: filename[-5:] == ".json", filenames)
        
        stormAdvisories = filter(lambda filename: self._getStormNameFromTCP() in filename,
                                 allAdvisories)
        stormAdvisories = map(lambda filename: filename[:-5], stormAdvisories)
        lastTwoAdvisories = sorted(stormAdvisories)[:2]
        
        self._previousAdvisory = None
        if len(lastTwoAdvisories) >= 1:
            self._previousAdvisory = self._loadAdvisory(lastTwoAdvisories[0])
        
        self._previousPreviousAdvisory = None
        if len(lastTwoAdvisories) >= 2:
            self._previousPreviousAdvisory = self._loadAdvisory(lastTwoAdvisories[1])
    
    def _loadAdvisory(self, advisoryName):
        self._synchronizeAdvisories()
         
        try:
            pythonDict = JsonSupport.loadFromJson(LocalizationType.CAVE_STATIC,
                                             self._site,
                                             self._getAdvisoryFilename(advisoryName))
            
            print "SARAH: File contents for", self._getAdvisoryFilename(advisoryName), ":"
            print pythonDict
             
            # Only use transmitted advisories
            if pythonDict["Transmitted"] == False and advisoryName != "pending":
                return None
            else:
                return pythonDict
        except Exception, e:
            print "SARAH Load Exception for %s : %s" % (self._getAdvisoryFilename(advisoryName), e)
            return None
        
    def _getAdvisoryPath(self):
        return "gfe/tcvAdvisories/"
    
    def _getLocalizationFile(self, loctype, siteID, filename):
        pathManager = PathManagerFactory.getPathManager()
        context = pathManager.getContextForSite(loctype, siteID)
        localizationFile = pathManager.getLocalizationFile(context, filename)
        
        return localizationFile
    
    def _getAdvisoryFilename(self, advisoryName):
        advisoryFilename = self._getAdvisoryPath() + \
                           advisoryName + \
                           ".json"
        return advisoryFilename

    ###############################################################
    ### GUI related methods

    def _processVariableList(self, definition, parent):
        # Get Definition variables
        for key in definition.keys():
            exec "self._" + key + "= definition[key]"

        # Overview GUI
        while True:
            overviewDict = self._displayGUI()
            if overviewDict is None:
                return None
            break

        # Consolidate information from GUI's
        varDict = overviewDict
        return varDict
    
    def _GUI_sizing_dict(self):
        # This contains values that adjust the GUI sizing.
        return {
            "GUI_height_limit": 900, # limit to GUI height in canvas pixels
            "charSize":       9,
            }
    
    def _GUI1_configDict(self):
        return {
            # Order and inclusion of GUI1 buttons
            # Each entry is (name of button in GUI code, desired label on GUI)
            "buttonList":[
                ("Run","Run"),
                ("Cancel","Cancel"),
                ],
            }

    def _font_GUI_dict(self):
        return {
            "headers": ("blue", ("Helvetica", 14, "bold")),
            "instructions": (None, ("Helvetica", 12, "italic")),
            }
    
    ###############################################################
    ### TCV Statistics
    
    def threatKeyOrder(self):
        return [None, "None", "Elevated", "Mod", "High", "Extreme"]
    
    def allowedHazards(self):
        tropicalActions = ["NEW", "EXA", "CAN", "CON"]
        return [
            ('HU.A',tropicalActions,'Hurricane'),
            ('HU.W',tropicalActions,'Hurricane'),
            ('SS.A',tropicalActions,'Surge'),
            ('SS.W',tropicalActions,'Surge'),
            ('TR.A',tropicalActions,'Tropical'),
            ('TR.W',tropicalActions,'Tropical'),
            ]
        
    def allowedHeadlines(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
            ('FF.A', allActions, 'Flood'),        # FLASH FLOOD WATCH
            ('FA.A', allActions, 'Flood'),        # FLOOD WATCH
            ('TO.A', allActions, 'Convective'),   # TORNADO WATCH
            ]
    
    def _initializeAdvisories(self):
        self._currentAdvisory = dict()
        self._currentAdvisory['ZoneData'] = dict()
        self._loadLastTwoAdvisories()
    
    def _initializeSegmentZoneData(self, segment):
        # The current advisory will be populated when setting a section's stats
        self._currentAdvisory['ZoneData'][segment] = {
            "WindThreat":            None,
            "WindForecast":          None,
            "StormSurgeThreat":      None,
            "StormSurgeForecast":    None,
            "FloodingRainThreat":    None,
            "FloodingRainForecast":  None,
            "TornadoThreat":         None,
        }
    
    def _makeSegmentEditAreas(self, argDict):
        areasList = self._segmentList
        #print "areaList", areasList
        editAreas = []
        self._editAreaDict = {}
        for area in areasList:
            self._editAreaDict[area] = area
            editAreas.append((area, area))
        return editAreas
    
    def _determineSegments(self):
        # Get the segments based on hazards "overlaid" with combinations file

        # Get the segments resulting from Hazards
        #print "\nRaw Analyzed", self._hazardsTable.rawAnalyzedTable()
        hazSegments = self.organizeHazards(self._hazardsTable.rawAnalyzedTable())
        #print "\nSegments from HazardsTable organizeHazards", hazSegments

        # Get the forecaster entered combinations
        accessor = ModuleAccessor.ModuleAccessor()
#        print "self._defaultEditAreas", self._defaultEditAreas
        combos = accessor.variable(self._defaultEditAreas, "Combinations")
        if combos is None:
            LogStream.logVerbose("COMBINATION FILE NOT FOUND: " + self._defaultEditAreas)
            return [], None
        #print "\nSegments from Zone Combiner", combos
        # "Overlay" the forecaster-entered combinations onto the segments
        segmentList = self._refineSegments(hazSegments, combos)
        #print "\nNew segments", segmentList
        
        # Instead of a segment being a group of zones, it will be just a single zone.
        # So collapse this list of lists down to a list of zones (aka. segments)
        segments = []
        for segment in segmentList:
            segments += segment

        return segments
    
    def _refineSegments(self, hazSegments, combos):
        """Break down each segment further according to combos given.
        Make sure the resulting segments follow the ordering of the combos.
        """
        if combos == []:
            return hazSegments
        newSegments = []  # list of lists
        newAreas = []
        for combo, label in combos:
            # Each combination will be tested to see if it can stay intact
            # i.e. if all areas in the combo are in the same segment
            # else split it into like segments
            #
            # segmentMapping is a list where each entry is
            #   the hazSegment in which the corresponding combo area appears.
            # (We need to define self._segmentList for the mapping function
            #   to use)
            self._segmentList = hazSegments
            #print "self._segmentList = %s" % (repr(self._segmentList))
            segmentMapping = map(self._findSegment, combo)
            #print "   segmentMapping", segmentMapping

            # segmentDict keys will be the hazSegments and
            #   we will gather all the areas of the combos that appear
            #   in each of these hazSegments
            segmentDict = {}
            keyList = []
            for areaName in combo:
                #print "       Adding", areaName
                key = tuple(segmentMapping[combo.index(areaName)])
                if key == ():  # If no hazard for area, do not include
                    continue
                if key not in keyList:
                    keyList.append(key)
                segmentDict.setdefault(key,[]).append(areaName)
            #print "   segmentDict", segmentDict

            # Keep track of the areas that we are including
            for key in keyList:
                segAreas = segmentDict[key]
                newAreas = newAreas + segAreas
                newSegments.append(segAreas)
        #print "   newSegments", newSegments
        # Now add in the hazAreas that have not been accounted for
        #   in the combinations
        for hazSegment in hazSegments:
            newSeg = []
            for hazArea in hazSegment:
                if hazArea not in newAreas:
                    newSeg.append(hazArea)
            if newSeg != []:
                newSegments.append(newSeg)
        return newSegments
    
    def _findSegment(self, areaName):
        for segment in self._segmentList:
            if areaName in segment:
                return segment
        return []

import Tkinter
class Common_Dialog(Dialog):
    def __init__(self, parent, title, infoDict=None):
        self._status = "Cancel"    # exception, or user-cancels
        self._tkObject_dict = {}   # place to store reference to tk objects
        self._varDict = {}         # all end results must be saved here
        self._infoDict = infoDict
        self._parent = parent
        Dialog.__init__(self, parent=None, title=title)
            
    def getVarDict(self):
        return self._varDict

    def _makeRadioOrCheckList(self, master, label, elementList, default=None,
                        buttonSide=Tkinter.TOP, frameSide=Tkinter.LEFT, entryField=None,
                        headerFG=None, headerFont=None, boxType="radio",
                              listFrameRelief=Tkinter.GROOVE):
        listFrame = Tkinter.Frame(master, relief=listFrameRelief, borderwidth=1)

        if label != "":
            listLabel = Tkinter.Label(listFrame, text=label, fg=headerFG, font=headerFont)
            listLabel.pack(side=Tkinter.TOP, fill=Tkinter.X, expand=Tkinter.NO, padx=10)

        ivar = Tkinter.IntVar()
        defaultIndex = 0
        ivarList = []
        for element in elementList:
            index = elementList.index(element)
            if type(element) is types.TupleType:
                element, key = element
            if boxType== "radio":
                button = Tkinter.Radiobutton(listFrame, variable=ivar, text=element, value=index)
            else:
                ivar = Tkinter.IntVar()
                if default is not None and element in default: ivar.set(1)
                else: ivar.set(0)
                button= Tkinter.Checkbutton(listFrame, variable=ivar, text=element)
                ivarList.append(ivar)
            button.pack(side=buttonSide, anchor=Tkinter.W, expand=Tkinter.YES, padx=4)
            # Look for default
            if element == default:
                defaultIndex = index

        entryObject = None
        if entryField is not None:
            entryObject = self._makeEntry(listFrame, entryField)
        # packing
        listFrame.pack(side=frameSide, expand=Tkinter.NO, fill=Tkinter.Y) #, anchor=Tkinter.N)        
        #listFrame.pack(side=frameSide, expand=Tkinter.YES, fill=Tkinter.Y, anchor=Tkinter.N)

        if boxType == "radio":
            ivar.set(defaultIndex) # set the default
        if boxType == "check":
            ivar = ivarList
        return ivar, entryObject

    def _makeEntry(self, frame, text, width=20):
        label = Tkinter.Label(frame, text=text)
        label.pack(side=Tkinter.LEFT, fill=Tkinter.X,  expand=Tkinter.NO)
        entry = Tkinter.Entry(frame, relief=Tkinter.SUNKEN, width=width)
        entry.pack(side=Tkinter.LEFT, fill=Tkinter.X, expand=Tkinter.NO)
        return entry

    def cancelCB(self):
        self._status = "Cancel"
        self.cancel()

    def _entryName(self, name):
        return name+"_entry"

    def _makeTuple(self,str):
        str = re.sub('(?im)[^_a-z]', '', str)
        return (str+":",str)

    def _setVarDict(self, key, value, options=None):
        if options is not None:
            value = options[value]
            if type(value) is types.TupleType:
                value = value[1]
        self._varDict[self._makeTuple(key)] = value

    def status(self):
        return self._status

    def buttonbox(self):
        # override the existing ok/cancel button box, removing it.
        # we do this so that we can attach our own hooks into the functions.
        pass


#########################################################
# The following defintions are from TextProductCommon.  #
# This is just bringing over the minimum amount needed. #
#########################################################
import DiscretePhrases
class TextProductCommon(DiscretePhrases.DiscretePhrases):
    def __init__(self):
        DiscretePhrases.DiscretePhrases.__init__(self)

    def setUp(self, areaDict):
        self._areaDictionary = areaDict

    def hazardTimeZones(self, areaList):
        '''
        Returns list of time zones for the starting time
        and list of time zones for the ending time.  
        
        The areaList provides a complete list of areas for this headline. 
        startT, endT are the hazard times.
        '''
        
        # get this time zone
        thisTimeZone = os.environ.get('TZ')
        if thisTimeZone is None:
            thisTimeZone = 'GMT'
            
        zoneList = []
        areaDict = self._areaDictionary

        # check to see if we have any areas outside our time zone
        for areaName in areaList:
            if areaName in areaDict.keys():
                entry = areaDict[areaName]
                if not entry.has_key('ugcTimeZone'): #add your site id
                    if thisTimeZone not in zoneList:
                        zoneList.append(thisTimeZone)
                    continue  # skip it
                timeZoneList = entry['ugcTimeZone']
                if type(timeZoneList) is not types.ListType:  # a single value
                    timeZoneList = [str(timeZoneList)]   # make it into a list
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

    def getExpireTime(self, issueTime, purgeHours, vtecRecords, roundMinutes=15,
        fixedExpire=0):
        '''
        Given the issuance time, purgeHours, and the vtecRecords (with times converted to ms),
        returns the appropriate expiration time.  
        
        Expiration time is the earliest of the specified expiration time, 1 hr if a CAN code
        is detected, or the ending time of ongoing events (CON, EXT, EXB, NEW).
        The issueTime and expireTime are ints in milliseconds. 
        
        @param issueTime in ms
        @param purgeHours -- set time past issuance time. 
               The default for this is set by policy e.g. an FFA expires by default
               in 8 hours. However, if there is a hazard end time earlier, then that
               is used.
               if -1, then hazard end time is to be used
        @param vtecRecords in the segment with times converted to ms
        @param roundMinutes
        @param fixedExpire -- indicates to ignore the VTEC actions when computing the 
               expiration time
        
        '''
        if purgeHours > 0:
            expireTime = issueTime + purgeHours * 3600 * 1000
        else:
            expireTime = None
            # Pick the earliest end time of the vtecRecords in the segment
            for vtecRecord in vtecRecords:
                if expireTime is None or vtecRecord.get('endTime') < expireTime:
                    expireTime = vtecRecord.get('endTime')

        if not fixedExpire:
            canExpFound = 0
            activeFound = 0
            laterActive = None  #later end time of all active events
            for vtecRecord in vtecRecords: 
                action = vtecRecord.get('act')
                if action in  ['CAN','EXP']:
                    canExpFound = 1
                elif action in ['NEW','CON','EXT','EXB','EXA']:
                    activeFound = 1
                    endTime = vtecRecord.get('endTime')
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
            expireTime = issueTime + 3600*1000

        #round to next 'roundMinutes'
        roundValue = roundMinutes*60*1000  #in milliseconds
        delta = expireTime % roundValue  # in milliseconds
        baseTime = int(expireTime/roundValue)*roundValue
        if delta/60*1000 >= 1:   #add the next increment
            expireTime = baseTime + roundValue
        else:   #within 1 minute, don't add the next increment
            expireTime = baseTime
                
        return expireTime

    def getHeadlinesAndSections(self, vtecRecords, productID, issueTime):
        '''
        Order vtec records and create the sections for the segment
        
        @param vtecRecords:  vtecRecords for a segment
        @param metaDataList: list of (metaData, hazardEvent) for the segment
        @param productID: product ID e.g. FFA, CWF, etc.
        @param issueTime: in seconds so that it compares to the vtec records
        '''
        sections = []
        headlines = []
        headlineStr = ''
        hList = copy.deepcopy(vtecRecords)
        if len(hList):
            if productID in ['CWF','NSH','OFF','GLF']:
                hList.sort(self.marineSortHazardAlg)
            else:
                hList.sort(self.regularSortHazardAlg)
                                           
        while len(hList) > 0:
            vtecRecord = hList[0]
            
            # Can't make phrases with vtecRecords with no 'hdln' entry 
            if vtecRecord['hdln'] == '':
                hList.remove(vtecRecord)
                continue

            # make sure the vtecRecord is still in effect or within EXP critiera
            if (vtecRecord['act'] != 'EXP' and issueTime >= vtecRecord['endTime']) or \
            (vtecRecord['act'] == 'EXP' and issueTime > 30*60 + vtecRecord['endTime']):
                hList.remove(vtecRecord)
                continue # no headline for expired vtecRecords
   
            #assemble the vtecRecord type
            hazStr = vtecRecord['hdln']
            headlines.append(hazStr)
            #hazStr = self.convertToLower(hazStr)

            # if the vtecRecord is a convective watch, tack on the etn
            phenSig = vtecRecord['phen'] + '.' + vtecRecord['sig']
            if phenSig in ['TO.A', 'SV.A']:
                hazStr = hazStr + ' ' + str(vtecRecord['etn'])

            # add on the action
            actionWords = self.actionControlWord(vtecRecord, issueTime)
            hazStr = hazStr + ' ' + actionWords
            
            if len(hazStr):
                # Call user hook
                localStr = self.hazard_hook(
                  None, None, vtecRecord['phen'], vtecRecord['sig'], vtecRecord['act'],
                  vtecRecord['startTime'], vtecRecord['endTime'])  # May need to add leading space if non-null 
                headlineStr = headlineStr + '...' + hazStr + localStr + '...\n'
                    
            # always remove the main vtecRecord from the list
            hList.remove(vtecRecord)
            
        return headlineStr, headlines

    def formatUGCs(self, ugcs, expireTime):
        '''
        Create ugc header with expire time
        'COC123-112330-'        
        '''
        ugcStr = self.makeUGCString(ugcs)
        ddhhmmTime = self.getFormattedTime(
              expireTime/1000, '%d%H%M', shiftToLocal=0, stripLeading=0).upper()
        ugcStr = ugcStr + '-' + ddhhmmTime + '-'
        return ugcStr

    def getFormattedTime(self, time_secs, format='%I%M %p %Z %a %b %d %Y',
                        shiftToLocal=1, upperCase=0, stripLeading=1):
        '''
         Return a text string of the given time in seconds in the given format
         This method is used for product headers.
        '''
        if time_secs == 0:
            time_secs = time.time()
        if shiftToLocal == 1:
            curTime = time.localtime(time_secs)
        else:
            curTime = time.gmtime(time_secs)
            localTime = time.localtime(time_secs)
            zoneName = time.strftime('%Z',localTime)
        timeStr = time.strftime(format, curTime)
        if shiftToLocal == 0:
            timeStr = string.replace(timeStr, zoneName, 'GMT')
        if stripLeading==1 and (timeStr[0] == '0' or timeStr[0] == ' '):
            timeStr = timeStr[1:]
        if upperCase == 1:
            timeStr = string.upper(timeStr)
        timeStr = string.replace(timeStr, '  ', ' ')
        return timeStr

    def formatUGC_names(self, ugcs, alphabetize=False, separator='-'):
        '''
        For example: Saunders-Douglas-Sarpy-Lancaster-Cass-Otoe-
        '''
        nameList = []
        for ugc in ugcs:
            entry = self._areaDictionary.get(ugc)
            nameList.append(entry.get('ugcName', ugc))
        if alphabetize:
            nameList.sort()
        return self.formatNameString(nameList, separator)

    def formatNameString(self, nameList, separator, state=None):
        nameString = ''
        for name in nameList:
            nameString+= name + separator
        if state:
            nameString = nameString.rstrip(separator) + ' ('+state+') '
        return nameString

    def getVal(self, dictionary, key, default=None, altDict=None):
        '''
        Convenience method to access dictionary keys and account for :skip and :editable suffixes
        
        @param dictionary 
        @param key, potentially without a suffix e.g. 'info'
        @return the key value accounting for suffixes e.g. 'info:skip'
        '''        
        for dictKey in [key, key+':skip', key+':editable']:
            if dictionary.get(dictKey): 
                return dictionary.get(dictKey)
            if altDict and altDict.get(dictKey):
                return altDict.get(dictKey)
        return default

    def formatDatetime(self, dt, format='ISO', timeZone=None):
        '''
        @param dt: datetime object
        @param format: format string e.g. '%H%M %p %Z %a %e %b %Y'
        @param zone: time zone e.g.'CST7CDT'.   If None use UTC 
        @return datetime formatted with time zone e.g. '1400 PM CST Mon 12 Feb 2011'
        '''
        import datetime
        from dateutil import tz
        # TODO REMOVE THIS BLOCK AS PART OF THE JSON REFACTOR.
        if type(dt) is float:
            dt = datetime.fromtimestamp(dt / 1000)
        
        from_zone = tz.tzutc()
        new_time = dt.replace(tzinfo=from_zone)
        if timeZone is not None:
            to_zone = tz.gettz(timeZone)
            new_time = new_time.astimezone(to_zone)
        if format == 'ISO':
            return new_time.isoformat()
        else:
            return new_time.strftime(format)

    def flush(self):
        ''' Flush the print buffer '''
        os.sys.__stdout__.flush()

    def makeUGCString(self, ugcs):
        '''
        Create the UGC string for product / segment headers.
        '''
        # if nothing in the list, return empty string
        if len(ugcs) == 0:
            return ''
        ugcList = copy.deepcopy(ugcs)
        # Remove any blank UGC lines from the list
        listsize=len(ugcList)
        j=0
        while j < listsize:
            if ugcList[j] == '':
                del ugcList[j]
            j=j+1

        # Set up state variables and process initialize ugcStr with first ugc
        # in ugcList
        inSeq = 0
        ugcStr = ugcList[0]
        curState = ugcStr[0:3]
        lastNum = int(ugcList[0][3:])
        firstNum = 0
        lastUgc = ugcList[0]

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
                        ugcStr += '>' + ugcNumStr
                        inSeq = 1
                else:  # num != lastNum + 1
                    ugcStr = self.checkLastArrow(inSeq, ugcStr)
                    inSeq = 0  # reset sequence when number not in sequence
                    ugcStr += '-' + ugcNumStr
            else:
                ugcStr = self.checkLastArrow(inSeq, ugcStr)
                ugcStr += '-' + ugc
                curState = ugcState
                inSeq = 0   #reset sequence when switching states
            lastNum = num
            lastUgc = ugc

        # May have to clean up last arrow at the end
        ugcStr = self.checkLastArrow(inSeq, ugcStr)
        return ugcStr

    def checkLastArrow(self, inSeq, ugcStr):
        '''
        Part of formatUGCs
        '''
        if inSeq == 1:
            # Change the last arrow to - since
            # we only had 2 in the sequence e.g.
            # 062>063  should be   062-063
            arrowIndex = ugcStr.rfind('>')
            if arrowIndex >= 0:
                ugcStr = ugcStr[:arrowIndex] + '-' + ugcStr[arrowIndex+1:]
        return ugcStr


