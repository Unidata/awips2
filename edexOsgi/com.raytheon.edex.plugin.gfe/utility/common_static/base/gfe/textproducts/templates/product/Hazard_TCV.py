# Version 2017.01.9-0

import GenericHazards
import JsonSupport
import LocalizationSupport
import time, types, copy, LogStream, collections
import ModuleAccessor
import math
import TimeRange
from com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID


from AbsTime import *
AWIPS_ENVIRON = "AWIPS2"

import HLSTCV_Common

class TextProduct(HLSTCV_Common.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition["displayName"]   = "None"
    Definition["outputFile"]    = "{prddir}/TEXT/TCV.txt"
    Definition["database"]      =  "Official"  # Source database
    Definition["mapNameForCombinations"] = "Zones_<site>"
    Definition["defaultEditAreas"] = "Combinations_TCV_<site>"
    Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display

    Definition["productName"]       = "Local Watch/Warning Statement"

    Definition["fullStationID" ]    = "<fullStationID>"
    Definition["wmoID" ]            = "<wmoID>"
    Definition["wfoCityState" ]     = "<wfoCityState>"
    Definition["pil" ]              = "<pil>"
    Definition["textdbPil" ]        = "<textdbPil>"
    Definition["awipsWANPil" ]      = "<awipsWANPil>"
    Definition["site"]              = "<site>"
    Definition["wfoCity"]           = "<wfoCity>"

    Definition["areaName"]          = ""  #optional area name for product
    Definition["areaDictionary"]    = "AreaDictionary"
    Definition["language"]          = "english"
    Definition["lineLength"]        = 71   #Maximum line length
    Definition["tabLength"]         = 4

    Definition["purgeTime"]         = 8 # Default Expiration in hours if
    Definition["includeZoneNames"]  = 1 # Zone names will be included in the area header
    Definition["includeIssueTime"]  = 0 # Issue Time will be included in the area header
    Definition["easPhrase"] = \
        "URGENT - IMMEDIATE BROADCAST REQUESTED" # Optional EAS phrase to be include in product header
    Definition["callToAction"] = 1
    Definition["hazardSamplingThreshold"] = (3, None)
        
    Definition["debug"] = {
                          #TextProduct
                          "__init__": 0,
                          "_inlandAreas": 0,
                          "_coastalAreas": 0,
                          "_cwa": 0,
                          "_productParts_TCV": 0,
                          "_segmentParts_TCV": 0,
                          "_analysisList": 0,
                          "_intersectAnalysisList": 0,
                          "_extraRainfallAnalysisList": 0,
                          "generateForecast": 0,
                          "_initializeVariables": 0,
                          "_performGridChecks": 0,
                          "_isCorrectNumGrids": 0,
                          "_checkContinuousDuration": 0,
                          "_noOpParts": 0,
                          "_easMessage": 0,
                          "_setup_segment": 0,
                          "_vtecRecords": 0,
                          "_areaList": 0,
                          "_issuanceTimeDate": 0,
                          "_summaryHeadlines": 0,
                          "_locationsAffected": 0,
                          "_fcstConfidence": 0,
                          "_infoSection": 0,
                          "_endSection": 0,
                          "_hazardDefinition": 0,
                          "_threatKeyOrder": 0,
                          "_sampleData": 0,
                          "_getStats": 0,
                          "_determineSegments": 0,
                          "_getRefinedHazardSegments": 0,
                          "_refineSegments": 0,
                          "_makeSegmentEditAreas": 0,
                          "_findSegment": 0,
                          "_getAllVTECRecords": 0,
                          "_getHazardsForHLS": 0,
                          "_convertToISO": 0,
                          "_convertToDatetime": 0,
                          "_initializeSegmentZoneData": 0,
                          "_archiveCurrentAdvisory": 0,
                          "_saveAdvisory": 0,
                          "_overview_list": 0,
                          "_displayGUI": 0,
                          
                          #HLSTCV_Common
                          "allowedHazards": 0,
                          "allowedHeadlines": 0,
                          "_initializeVariables": 0,
                          "moderated_dict": 0,
                          "_wmoHeader": 0,
                          "_productHeader": 0,
                          "_ugcHeader": 0,
                          "_processProductParts": 0,
                          "_createProductDictionary": 0,
                          "_initializeProductDictionary": 0,
                          "_formatProductDictionary": 0,
                          "_getStatValue": 0,
                          "_allAreas": 0,
                          "_groupSegments": 0,
                          "_getSegmentVTECRecordsTuples": 0,
                          "_computeIntersectAreas": 0,
                          "_initializeHazardsTable": 0,
                          "_getHazardsTable": 0,
                          "_ignoreActions": 0,
                          "_setVTECActiveTable": 0,
                          "_getVtecRecords": 0,
                          "_getAllowedHazardList": 0,
                          "_altFilterMethod": 0,
                          "_filterHazards": 0,
                          "_getAdditionalHazards": 0,
                          "_checkHazard": 0,
                          "_initializeTimeVariables": 0,
                          "_determineTimeRanges": 0,
                          "_createPeriodList": 0,
                          "_calculateStartTime": 0,
                          "_formatPeriod": 0,
                          "_getTimeDesc": 0,
                          "_getPartOfDay": 0,
                          "_initializeStormInformation": 0,
                          "_parseTCP": 0,
                          "_getStormTypeFromTCP": 0,
                          "_getStormNameFromTCP": 0,
                          "_getAdvisoryTypeFromTCP": 0,
                          "_getAdvisoryNumberStringFromTCP": 0,
                          "_getStormNumberStringFromTCP": 0,
                          "_getStormIDStringFromTCP": 0,
                          "_useTestTCP": 0,
                          "_testTCP": 0,
                          "_initializeAdvisories": 0,
                          "_synchronizeAdvisories": 0,
                          "_getLocalAdvisoryDirectoryPath": 0,
                          "_getStormAdvisoryNames": 0,
                          "_loadLastTwoAdvisories": 0,
                          "_loadAdvisory": 0,
                          "_getAdvisoryPath": 0,
                          "_getAdvisoryFilename": 0,
                          "_processVariableList": 0,
                          "_GUI_sizing_dict": 0,
                          "_GUI1_configDict": 0,
                          "_font_GUI_dict": 0,
                          
                          #Overview_Dialog
                          "body": 0,
                          "_makeButtons": 0,
                          "okCB": 0,
                          
                          #Common_Dialog
                          "getVarDict": 0,
                          "_makeRadioOrCheckList": 0,
                          "_makeEntry": 0,
                          "cancelCB": 0,
                          "_entryName": 0,
                          "_makeTuple": 0,
                          "_setVarDict": 0,
                          "status": 0,
                          "buttonbox": 0,
                          
                          #SectionCommon
                          "_setProductPartValue": 0,
                          "_finalSectionParts": 0,
                          "_sectionHeader": 0,
                          "_lifePropertyThreatSummary": 0,
                          "_getThreatTrendSentence": 0,
                          "_getThreatTrendValue": 0,
                          "_threatDifference": 0,
                          "_isThreatDecreasing": 0,
                          "_isThreatIncreasing": 0,
                          "_advisoryHasValidKey": 0,
                          "_isMagnitudeIncreasing": 0,
                          "_calculateThreatStatementTr": 0,
                          "_pastWindHazardWasCAN": 0,
                          "_pastThreatsNotNone": 0,
                          "_setThreatStatementsProductParts": 0,
                          "_getThreatStatements": 0,
                          "_potentialImpactsSummary": 0,
                          "_getPotentialImpactsSummaryText": 0,
                          "_potentialImpactsStatements": 0,
                          "_getPotentialImpactsStatements": 0,
                          "_preparationStatement": 0,
                          
                          #Unique to each section, but common method name
                          "sectionParts": 0,
                          "_forecastSubsection": 0,
                          "_latestForecastSummary": 0,
                          "_threatSubsection": 0,
                          "_threatTrend": 0,
                          "_threatStatements": 0,
                          "_impactsSubsection": 0,
                          "_setStats": 0,
                          
                          #WindSection
                          "_peakWind": 0,
                          "_windowTS": 0,
                          "_windowHU": 0,
                          "_moderatedMaxWindMph_categories": 0,
                          "_ktToMph": 0,
                          "_increment": 0,
                          
                          #StormSurgeSection
                          "_peakSurge": 0,
                          "_surgeWindow": 0,
                          
                          #FloodingRainSection
                          "_peakRain": 0,
                          "_rainRange": 0,
                          
                          #TornadoSection
                          "_tornadoSituation": 0,
                          
                          #SectionCommonStats
                          "_initializeSegmentAdvisories": 0,
                          "_updateThreatStats": 0,
                          "_calculateHourOffset": 0,
                          
                          #WindSectionStats
                          "_determineCurrentPeriod": 0,
                          "_updateStatsForPwsXXint": 0,
                          "_updateStatsForPwsTXX": 0,
                          "_updateStatsForWind": 0,
                          "_updateMaxWindGust": 0,
                          "_calculateProbOnset": 0,
                          "_calculateMaxPwsXXintTr": 0,
                          "_computeWindOnsetAndEnd": 0,
                          "_createWindowText": 0,
                          "_getConfiguredTime": 0,
                          "_calculateUTCandLocalHourOffset": 0,
                          "_isValidDayTime": 0,
                          
                          #Unique to each formatter, but common method name
                          "execute": 0,
                          
                          #XMLFormatter
                          "xmlKeys": 0,
                          "sectionKeys": 0,
                          "getSectionKey": 0,
                          "dictionary": 0,
                          "list": 0,
                          
                          #LegacyFormatter
                          "_processProductParts": 0,
                          "processWmoHeader": 0,
                          "processProductHeader": 0,
                          "processLocationsAffected": 0,
                          "processSubsection": 0,
                          "processThreatStatements": 0,
                          "processImpactsStatements": 0,
                          "processInfoSection": 0,
                          "_buildInfoSection": 0,
                          "processSummaryHeadlines": 0,
                          "processSubParts": 0,
                          
                          #TextProductCommon
                          "setUp": 0,
                          "hazardTimeZones": 0,
                          "getExpireTime": 0,
                          "getHeadlinesAndSections": 0,
                          "formatUGCs": 0,
                          "getFormattedTime": 0,
                          "formatUGC_names": 0,
                          "formatNameString": 0,
                          "getVal": 0,
                          "formatDatetime": 0,
                          "flush": 0,
                          "makeUGCString": 0,
                          "checkLastArrow": 0,
                          }
#    Definition["debug"] = 1         #  turn on ALL debug messages
    Definition["debug"] = 0         #  turn off ALL debug messages


    def __init__(self):
        HLSTCV_Common.TextProduct.__init__(self)

    #####################################################################
    #####################################################################
    ### Organization of Formatter Code
    
    ###############################################################
    ###  MUST OVERRIDE DEFINITIONS !!!
    ###    _inlandAreas, _coastalAreas, _cwa
    ###############################################################
    
    ###############################################################
    ### TCV Product and Segment Parts Definition
    ###############################################################
    
    ###############################################################
    ### Analysis Lists, SampleAnalysis Overrides and other
    ###   analysis related methods
    ###############################################################
    
    ###############################################################
    #  CODE
    ###############################################################
    ###  High level flow of formatter
    ###    generateForecast, _initializeVariables,
    ###    _determineSegments, _determineTimeRanges, _sampleData,
    ###    _createProductDictionary, _formatProductDictionary,
    ###    _archiveCurrentAdvisory
    ###############################################################
    
    ###############################################################
    ### Product Parts Implementation
    ###############################################################
    
    ###############################################################
    ### Sampling and Statistics related methods
    ###############################################################
    
    ###############################################################
    ### Area, Zone and Segment related methods
    ###############################################################
    
    ###############################################################
    ### Hazards related methods
    ###############################################################
    
    ###############################################################
    ### Time related methods
    ###############################################################
    
    ###############################################################
    ### Advisory related methods
    ###############################################################
    
    ###############################################################
    ### GUI related methods
    ###############################################################
    
    
    ###############################################################
    ###  MUST OVERRIDE DEFINITIONS !!!
    
    def _inlandAreas(self):
        return [
            #"FLZ052", "FLZ056", "FLZ057", "FLZ061", "FLZ043",
            ]

    def _coastalAreas(self):
        return [
            #"FLZ039", "FLZ042", "FLZ048", "FLZ049", "FLZ050", "FLZ051", "FLZ055", "FLZ060",
            #"FLZ062",
            ]

    def _cwa(self):
        return ""  #"MFL"
    
    ###############################################################
    ### TCV Product and Segment Parts Definition
    
    def _productParts_TCV(self, segment_vtecRecords_tuples):
        segmentParts = []
        for segment_vtecRecords_tuple in segment_vtecRecords_tuples:
            segmentParts.append(self._segmentParts_TCV(segment_vtecRecords_tuple))
        return {
            'partsList': [
                'wmoHeader',
                'easMessage',
                'productHeader',
                ('segments', segmentParts),
            ]
            }

    def _segmentParts_TCV(self, segment_vtecRecords_tuple): 
        segment, _ = segment_vtecRecords_tuple
        
        windSection = 'windSection[\'' + segment + '\']'
        stormSurgeSection = 'stormSurgeSection[\'' + segment + '\']'
        floodingRainSection = 'floodingRainSection[\'' + segment + '\']'
        tornadoSection = 'tornadoSection[\'' + segment + '\']'

        partsList = [
                'setup_segment',
                'ugcHeader',
                'vtecRecords',
                'areaList',
                'issuanceTimeDate',
                'summaryHeadlines',
                'locationsAffected',
                'fcstConfidence',
                (windSection, self._windSection[segment].sectionParts(segment_vtecRecords_tuple)),
                ]
        
        #  The storm surge section should never be inserted into 
        #  "inland" zones, since there will never be a surge impact.
        if segment not in self._inlandAreas():
            partsList.append(
                    (stormSurgeSection, self._stormSurgeSection[segment].sectionParts(segment_vtecRecords_tuple)))
        
        partsList.extend([
                (floodingRainSection, self._floodingRainSection[segment].sectionParts(segment_vtecRecords_tuple)),
                (tornadoSection, self._tornadoSection[segment].sectionParts(segment_vtecRecords_tuple)),
                'infoSection',
                'endSection'])

        return {
            'arguments': segment_vtecRecords_tuple,
            'partsList': partsList
            }
    
    ###############################################################
    ### Analysis Lists, SampleAnalysis Overrides and other
    ###   analysis related methods
    
    def _analysisList(self):
        # Sample over 120 hours beginning at current time
        analysisList = [
            # Wind Section
            ("Wind", self.vectorModeratedMax, [3]),
            ("WindGust", self.moderatedMax, [3]),
            ("WindThreat", self.mostSignificantDiscreteValue),
            ("pws34int", self.moderatedMax, [3]),
            ("pws64int", self.moderatedMax, [3]),
            ("pwsD34", self.moderatedMax),
            ("pwsN34", self.moderatedMax),
            ("pwsD64", self.moderatedMax),
            ("pwsN64", self.moderatedMax),
            
            # Flooding Rain Section
            ("QPF", self.accumSum, [72]),
            ("FloodingRainThreat", self.mostSignificantDiscreteValue),
            
            # Tornado Section
            ("TornadoThreat", self.mostSignificantDiscreteValue),
            ]

        return analysisList
    
    def _intersectAnalysisList(self):
        # The grids for the Surge Section will be intersected with a special edit area
        analysisList = [
            ("InundationMax", self.moderatedMax),
            ("InundationTiming", self.moderatedMax, [6]),
            ]

        return analysisList
    
    def _extraRainfallAnalysisList(self):
        analysisList = [
            ("QPF", self.accumSum),
            ]

        return analysisList
    
    ###############################################################
    ###  High level flow of formatter

    def generateForecast(self, argDict):
        # Generate Text Phrases for a list of edit areas
        
        self.debug_print("argDict = %s" % (self._pp.pformat(argDict)), 1)

        error = self._initializeVariables(argDict)
        if error is not None:
            return error
        
        if self._stormName is None or self._stormName == "":
            return "Could not determine the storm name"
        
        self._segmentList = self._determineSegments()
        self.debug_print("Segment Information: %s" % (self._pp.pformat(self._segmentList)), 1)
        if len(self._segmentList) == 0:
            return "No hazards to report"

        # Determine time ranges
        self._determineTimeRanges(argDict)

        # Make sure we have all of the necessary grids before continuing
        error = self._performGridChecks(argDict)
        if error is not None:
            return error

        # Sample the data
        self._sampleData(argDict)

        # Create the product dictionary and format it to create the output
        productDict = self._createProductDictionary(self._productParts_TCV,
                                                    self._segmentList,
                                                    areProductPartsSegmented=True)
        productOutput = self._formatProductDictionary(LegacyFormatter, productDict)

        self._archiveCurrentAdvisory()
        
        return productOutput
    
    def _initializeVariables(self, argDict):
        error = HLSTCV_Common.TextProduct._initializeVariables(self, argDict)
        if error is not None:
            return error
        
        self._windSection = dict()
        self._stormSurgeSection = dict()
        self._floodingRainSection = dict()
        self._tornadoSection = dict()
        
        self._initializeAdvisories()
        
        return None
    
    def _performGridChecks(self, argDict):
        gridChecks = [(self._isCorrectNumGrids, "FloodingRainThreat", 1, argDict),
                      (self._isCorrectNumGrids, "TornadoThreat", 1, argDict),
                      (self._isContinuousDuration, "QPF", 72, argDict),]
        
        if self._WSPGridsAvailable:
            gridChecks += [(self._isCorrectNumGrids, "WindThreat", 1, argDict),
                           (self._isContinuousDuration, "Wind", 120, argDict),
                           (self._isContinuousDuration, "WindGust", 120, argDict),
                           (self._isContinuousDuration, "pws34int", 114, argDict),
                           (self._isContinuousDuration, "pws64int", 114, argDict),
                           (self._isCombinedContinuousDuration, "pwsD34", "pwsN34", 102, argDict),
                           (self._isCombinedContinuousDuration, "pwsD64", "pwsN64", 102, argDict),]
        
        if self._PopulateSurge and len(self._coastalAreas()) != 0:
            gridChecks += [(self._isCorrectNumGrids, "InundationMax", 1, argDict),
                           (self._isCorrectNumGrids, "InundationTiming", 12, argDict),]
        
        missingGridErrors = []
        for gridCheck in gridChecks:
            # The first element is the grid check function to call and
            # the rest of the elements are the arguments to the function
            if not gridCheck[0](*gridCheck[1:]):
                error = ""
                if gridCheck[0] == self._isCorrectNumGrids:
                    if gridCheck[2] == 1:
                        error = "%s needs at least 1 grid" % (gridCheck[1])
                    else:
                        error = "%s needs at least %s grids" % (gridCheck[1], gridCheck[2])
                elif gridCheck[0] == self._isContinuousDuration:
                    error = "%s needs at least %s continuous hours worth of data" % \
                        (gridCheck[1], gridCheck[2])
                else:
                    error = "%s and %s combined need at least %s continuous hours worth of data" % \
                        (gridCheck[1], gridCheck[2], gridCheck[3])
                
                missingGridErrors.append(error)
        
        if len(missingGridErrors) != 0:
            error = "There were problems with the following weather elements:\n"

            for gridError in missingGridErrors:
                error += "\t" + gridError + "\n"
            
            return error
        
        return None
    
    def _isCorrectNumGrids(self, weatherElement, expectedNumGrids, argDict):
        ifpClient = argDict["ifpClient"]
        dbId = argDict["databaseID"]
        parmId = ParmID(weatherElement, dbId)
        times = ifpClient.getGridInventory(parmId)
    
        self.debug_print("_isCorrectNumGrids test for element: %s" % weatherElement, 1)
        self.debug_print("Expected number of grids: %s" % expectedNumGrids, 1)
    
        gridTimes = []
        for index in range(len(times)):
            gridTime = TimeRange.TimeRange(times[index])
    
            if (gridTime.endTime() <= self._timeRange1Hour.startTime() or
                gridTime.startTime() >= self._timeRange1Hour.endTime()):
    
#                prettyStartTime = self._pp.pformat(str(gridTime.startTime()))
#                prettyEndTime = self._pp.pformat(str(gridTime.endTime()))
#                self.debug_print("skipping grid %s (%s - %s): outside of time range"
#                                 % (index, prettyStartTime, prettyEndTime), 1)
                pass
            else:
                gridTimes.append(gridTime)
    
        self.debug_print("Actual number of grids: %s" % len(gridTimes), 1)

        retval = len(gridTimes) >= expectedNumGrids
        if not retval:
            self.debug_print("_isCorrectNumGrids test failed", 1)
            self.debug_print("self._timeRange1Hour: %s" % str(self._timeRange1Hour), 1)
            self.debug_print("times: %s" % str(times), 1)
        return retval

    def _isContinuousDuration(self, weatherElement, minimumNumHours, argDict):
        return self._checkContinuousDuration([weatherElement], minimumNumHours, argDict)
    
    def _isCombinedContinuousDuration(self, weatherElement1, weatherElement2, minimumNumHours, argDict):
        return self._checkContinuousDuration([weatherElement1, weatherElement2], minimumNumHours, argDict)
    
    def _checkContinuousDuration(self, weatherElementList, minimumNumHours, argDict):

        self.debug_print("_checkContinuousDuration for elements: %s" % \
                             self._pp.pformat(weatherElementList), 1)
        self.debug_print("Minimum Number of Hours: %s" % minimumNumHours, 1)
    
        ifpClient = argDict["ifpClient"]
        dbId = argDict["databaseID"]
    
        gridTimes = []
        inventoryDict = {}
        for weatherElement in weatherElementList:
            parmId = ParmID(weatherElement, dbId)
            times = ifpClient.getGridInventory(parmId)
            inventoryDict[weatherElement] = times
    
            for index in range(times.size()):
                gridTimes.append(TimeRange.TimeRange(times[index]))
    
        if len(gridTimes) == 0:
            # No grids
            self.debug_print("No grids found.", 1)
            return False
    
        gridTimes = sorted(gridTimes, key= lambda gridTime: gridTime.startTime())
    
        totalHours = 0
        previousEndTime = None
        for gridTime in gridTimes:
            if gridTime.endTime() <= self._timeRange1Hour.startTime():
#                prettyEndTime = self._pp.pformat(str(gridTime.endTime()))
#                prettyStartTime = self._pp.pformat(str(self._timeRange1Hour.startTime()))
#                self.debug_print("skipping: grid end time (%s) before time range start time (%s)"
#                                 % (prettyEndTime, prettyStartTime), 1)
                continue
    
            if gridTime.startTime() >= self._timeRange1Hour.endTime():
#                prettyStartTime = self._pp.pformat(str(gridTime.startTime()))
#                prettyEndTime = self._pp.pformat(str(self._timeRange1Hour.endTime()))
#                self.debug_print("done: grid start time (%s) after time range end time (%s)"
#                                 % (prettyStartTime, prettyEndTime), 1)
                break
    
            if previousEndTime is None:
                previousEndTime = gridTime.startTime()
    
            if previousEndTime != gridTime.startTime():
                break

            previousEndTime = gridTime.endTime()
            totalHours += gridTime.duration() / 3600 # Convert from seconds to hours
    
        self.debug_print("Total Hours of continuous grids: %s" % totalHours, 1)

        retval = totalHours >= minimumNumHours
        if not retval:
            self.debug_print("_checkContinuousDuration failed.", 1)
            self.debug_print("self._timeRange1Hour: %s" % self._pp.pformat(self._timeRange1Hour), 1)
            for we in inventoryDict: 
                self.debug_print("times for %s: %s" % (we, str(inventoryDict[we])), 1) 
            self.debug_print("Not continuous at: %s" % str(previousEndTime), 1)

        return retval
    
    ###############################################################
    ### Product Parts Implementation
    
    def _noOpParts(self):
        '''
        These represent product parts that should be skipped when calling product part methods.
        They will be handled automatically by the formatters.
        '''
        return ['CR', 'endProduct', 'endSegment', 'doubleAmpersand']
    
    ################# Product Level

    def _easMessage(self, productDict, productSegmentGroup, arguments=None):
        productDict['easMessage'] = self._easPhrase
    
    ################# Segment Level

    def _setup_segment(self, segmentDict, productSegmentGroup, productSegment):
        segment, vtecRecords = productSegment
        self.debug_print('setup_segment productSegment %s' % (self._pp.pformat(productSegment)), 1)
        # NOTE -- using _getVtecRecords to change to milliseconds
        segmentVtecRecords = self._getVtecRecords(segment)
            
        # UGCs and Expire Time
        # Assume that the geoType is the same for all hazard events in the segment i.e. area or point
        self._ugcs = [segment]
        self._timeZones = self._tpc.hazardTimeZones(self._ugcs)
        
        # In order to compute the expire time, the VTEC record times
        # need to be in milliseconds.
        recordsInMS = []
        for record in segmentVtecRecords:
            recordInMS = copy.copy(record)
            
            recordInMS["startTime"] = recordInMS["startTime"] * 1000
            recordInMS["endTime"] = recordInMS["endTime"] * 1000
            if recordInMS.has_key("purgeTime"):
                recordInMS["purgeTime"] = recordInMS["purgeTime"] * 1000
            if recordInMS.has_key("issueTime"):
                recordInMS["issueTime"] = recordInMS["issueTime"] * 1000
            
            recordsInMS.append(recordInMS)
        
        # Get the expire time in milliseconds since the epoch
        self._expireTime = self._tpc.getExpireTime(
                    self._issueTime_ms, self._purgeHours, recordsInMS)
        # Then convert it to a date
        segmentDict['expireTime'] = self._convertToISO(self._expireTime)
        
        # Don't show UPG headlines
        nonUPGrecords = []
        for record in segmentVtecRecords:
            if record['act'] != "UPG":
                nonUPGrecords.append(record)
        self._summaryHeadlines_value, _ = self._tpc.getHeadlinesAndSections(
                    nonUPGrecords, self._productID, self._issueTime_secs)
        
    def _vtecRecords(self, segmentDict, productSegmentGroup, productSegment):
        segment, vtecRecords = productSegment
        records = []
        for vtecRecord in vtecRecords:
            vstr = vtecRecord["vtecstr"]
            
            self.debug_print("vtecRecord = %s" % (self._pp.pformat(vtecRecord)), 1)
            
            self.debug_print("final vstr = %s" % vstr, 1)
            records.append(vstr)
        segmentDict['vtecRecords'] = records
        
    def _areaList(self, segmentDict, productSegmentGroup, productSegment):
        # Area String        
        segmentDict['areaList'] = self._tpc.formatUGC_names(self._ugcs)
    
    def _issuanceTimeDate(self, segmentDict, productSegmentGroup, productSegment):
        segmentDict['issuanceTimeDate'] = self._timeLabel
    
    def _summaryHeadlines(self, segmentDict, productSegmentGroup, productSegment):
        segment, vtecRecords = productSegment
        definitions = []
        hazardsFound = []
        
        for (phenSig, actions, name) in self.allowedHazards():
            for vtecRecord in vtecRecords:
                #  The 'phensig' in the VTEC record could contain an
                #  ETN. As such, we need to strip the ETN before doing a
                #  comparison with the allowedHazards.
                if vtecRecord["phensig"].split(":")[0] == phenSig and \
                   phenSig not in hazardsFound and \
                   vtecRecord["act"] in ["NEW", "EXA"]:
                    
                    hazardsFound.append(phenSig)
                    definition = self._hazardDefinition(phenSig)
                    if definition != "":
                        definitions.append(definition)
        
        summaryDict = collections.OrderedDict()
        headlines = self._summaryHeadlines_value.split("\n")
        headlinesInEffect = []
        for headline in headlines:
            if len(headline) != 0:
                headlinesInEffect.append(headline)
        summaryDict['headlinesInEffect'] = headlinesInEffect
        summaryDict['headlineDefinitions'] = definitions
        segmentDict['summaryHeadlines'] = summaryDict
    
    def _locationsAffected(self, segmentDict, productSegmentGroup, productSegment):
        segment, vtecRecords = productSegment
        import TCVAreaDictionary
        tcv_AreaDictionary = TCVAreaDictionary.TCV_AreaDictionary
        
        segmentDict['locationsAffected'] = []
        if segment in tcv_AreaDictionary:
            segmentDict['locationsAffected'] = tcv_AreaDictionary[segment]["locationsAffected"]
    
    def _fcstConfidence(self, segmentDict, productSegmentGroup, productSegment):
        # TODO - Get this from the TCM product potentially? Not included until provided from NHC
        return ""
    
    def _infoSection(self, segmentDict, productSegmentGroup, productSegment):
        segment, vtecRecords = productSegment
        import TCVAreaDictionary
        tcv_AreaDictionary = TCVAreaDictionary.TCV_AreaDictionary
    
        segment, vtecRecords = productSegment
        infoSection = []
        if segment in tcv_AreaDictionary:
            infoSection = tcv_AreaDictionary[segment]["infoSection"]
        
        segmentDict['infoSection'] = infoSection
        
    def _endSection(self, segmentDict, productSegmentGroup, productSegment):
        segmentDict['endSection'] = "\n$$"
    
    ################# Product Parts Helper Methods
    
    def _hazardDefinition(self, phenSig):
        import VTECTable
        
        phen, sig = phenSig.split('.')
        headline = VTECTable.VTECTable[phenSig]["hdln"]
        
        definition = "A " + headline + " means "
        
        if phen == "HU":
            definition += "Hurricane wind conditions"
        elif phen == "TR":
            definition += "Tropical storm wind conditions"
        elif phen == "SS":
            definition += "life-threatening inundation levels"
        else:
            return ""
        
        if sig == "W": # Warning
            definition += " are expected somewhere within this area and within the next 36 hours"
        elif sig == "A": # Watch
            definition += " are possible somewhere within this area and within the next 48 hours"
        
        return definition
    
    ###############################################################
    ### Sampling and Statistics related methods
    
    def _threatKeyOrder(self):
        return [None, "None", "Elevated", "Mod", "High", "Extreme"]
    
    def _sampleData(self, argDict):
        # Sample the data
        self._createSamplers(argDict)
        
        # We need to preserve the ordering of the zones based off the zone combiner ordering
        sortedAreas = sorted(self._allAreas(),
                             key=lambda x: self._segmentList.index(x) if x in self._segmentList else 9999)
        for segment in sortedAreas:
            self._initializeSegmentZoneData(segment)

            # We need stats for all zones to be saved in the advisory,
            # regardless of whether or not it has a hazard in it. Getting
            # the stats causes them to be added to the advisory.
            windStats, stormSurgeStats, floodingRainStats, tornadoStats = \
                self._getStats(self._argDict,
                               segment,
                               self._editAreaDict,
                               self._timeRangeList1Hour,
                               self._timeRangeList3Hour,
                               self._timeRangeList6Hour)
            
            # Only show zones with hazards in the output
            if segment in self._segmentList:
                # These segment sections will be added to the product parts
                self._windSection[segment] = WindSection(self, segment, windStats)
                self._stormSurgeSection[segment] = StormSurgeSection(self, segment, stormSurgeStats)
                self._floodingRainSection[segment] = FloodingRainSection(self, segment, floodingRainStats)
                self._tornadoSection[segment] = TornadoSection(self, segment, tornadoStats)

    def _createSamplers(self, argDict):
        # Create the samplers used for sampling the data
        editAreas = self._makeSegmentEditAreas(argDict)

        # The sampler used for Wind section related stats
        self._sampler1Hour = self.getSampler(argDict,
          (self._analysisList(), self._timeRangeList1Hour, editAreas))

        # The sampler used for Flooding Rain and Storm Surge section related stats
        self._sampler3Hour = self.getSampler(argDict,
          (self._analysisList(), self._timeRangeList3Hour, editAreas))
        
        # For storm surge, the edit areas are intersected with a special edit area.
        # If there aren't any coastal areas, they won't have the special edit area
        # though so don't execute this code in that case.
        if len(self._coastalAreas()) > 0:
            intersectAreas = self._computeIntersectAreas(editAreas, argDict)
            self._intersectSampler = self.getSampler(argDict,
              (self._intersectAnalysisList(), self._timeRangeList6Hour, intersectAreas))

        #  Make a sample period for the previous rainfall
        self._previousRainfallTR = [(self._extraSampleTimeRange, "PrevRainfall")]
        self._extraRainfallSampler = self.getSampler(argDict,
            (self._extraRainfallAnalysisList(),  self._previousRainfallTR, 
             editAreas))
    
    def _getStats(self, argDict, segment, editAreaDict, timeRangeList1Hour, timeRangeList3Hour, timeRangeList6Hour):
        # Get statistics for this segment
        
        editArea = editAreaDict[segment]
        
        statList1Hour = self.getStatList(self._sampler1Hour,
                                         self._analysisList(),
                                         timeRangeList1Hour,
                                         editArea)
        
        statList3Hour = self.getStatList(self._sampler3Hour,
                                         self._analysisList(),
                                         timeRangeList3Hour,
                                         editArea)
        
        self.debug_print("*"*80, 1)
        self.debug_print("editArea =" + editArea, 1)
        self.debug_print("timeRangeList1Hour = %s" % (self._pp.pformat(timeRangeList1Hour)), 1)
        self.debug_print("timeRangeList3Hour = %s" % (self._pp.pformat(timeRangeList3Hour)), 1)
        self.debug_print("timeRangeList6Hour = %s" % (self._pp.pformat(timeRangeList6Hour)), 1)
        self.debug_print("statList1Hour = %s" % (self._pp.pformat(statList1Hour)), 1)
        self.debug_print("statList3Hour = %s" % (self._pp.pformat(statList3Hour)), 1)
        self.debug_print("-"*40, 1)
        
        windStats = WindSectionStats(self, segment, statList1Hour, timeRangeList1Hour)
        
        # The surge section needs sampling done with an intersected edit area
        if editArea in self._coastalAreas():
            intersectEditArea = "intersect_"+editArea
            intersectStatList = self.getStatList(self._intersectSampler,
                                                 self._intersectAnalysisList(),
                                                 timeRangeList6Hour,
                                                 intersectEditArea)
        else:
            intersectStatList = "InlandArea"
        
        self.debug_print("intersectStatList = %s" % (self._pp.pformat(intersectStatList)), 1)
        self.debug_print("-"*40, 1)

        stormSurgeStats = StormSurgeSectionStats(self, segment, intersectStatList, timeRangeList6Hour)

        #  These stats are for handling the extra rainfall
        extraRainfallStatList = self.getStatList(self._extraRainfallSampler,
                                             self._extraRainfallAnalysisList(),
                                             self._previousRainfallTR,
                                             editArea)
         
        floodingRainStats = FloodingRainSectionStats(self, segment, 
                               statList3Hour, timeRangeList3Hour, 
                               extraRainfallStatList, self._previousRainfallTR)
        tornadoStats = TornadoSectionStats(self, segment, statList3Hour, timeRangeList3Hour)
                
        return (windStats, stormSurgeStats, floodingRainStats, tornadoStats)
    
    ###############################################################
    ### Area, Zone and Segment related methods
    
    def _determineSegments(self):
        # Get the segments based on hazards "overlaid" with combinations file

        # Get the forecaster entered combinations
        accessor = ModuleAccessor.ModuleAccessor()
        self.debug_print("self._defaultEditAreas = %s" % (self._pp.pformat(self._defaultEditAreas)), 1)
        combos = accessor.variable(self._defaultEditAreas, "Combinations")
        # combos is a list of tuples. Each tuple is a grouping of zones (a list of zones, combo name).
        if combos is None:
            LogStream.logVerbose("Combination file not found: " + self._defaultEditAreas)
            return []
        self.debug_print("Segments from Zone Combiner = %s" % (self._pp.pformat(combos)), 1)
        
        # "Overlay" the forecaster-entered combinations onto the segments
        # so that the zones are ordered and grouped (as much as possible)
        # as indicated in the zone combiner.
        refinedHazardSegments = self._getRefinedHazardSegments(combos)
        
        # Instead of a segment being a group of zones, it will be just a single zone.
        # So collapse this list of lists down to a list of zones (aka. segments)
        segments = []
        for segment in refinedHazardSegments:
            segments += segment

        return segments
    
    def _getRefinedHazardSegments(self, combos):
        # Get a list of list of zones that are ordered and grouped
        # based off of hazards and the provided zone combinations.
        
        # Get the raw analyzed table (a list of VTEC records) and organize the hazards
        # to get a list of lists of zones that have the same hazards
        self.debug_print("Raw Analyzed %s" % (self._pp.pformat(self._hazardsTable.rawAnalyzedTable())), 1)
        hazSegments = self.organizeHazards(self._hazardsTable.rawAnalyzedTable())
        self.debug_print("Segments from HazardsTable organizeHazards %s" % (self._pp.pformat(hazSegments)), 1)
        
        # "Overlay" the forecaster-entered combinations onto the segments
        # so that the zones are ordered and grouped (as much as possible)
        # as indicated in the zone combiner.
        refinedSegments = self._refineSegments(hazSegments, combos)
        self.debug_print("New segments = %s" % (self._pp.pformat(refinedSegments)), 1)
        
        return refinedSegments
    
    def _refineSegments(self, hazSegments, combos):
        """Reorder and regroup (as much as possible) the hazard segments
           based off of the ordering and grouping in combos. Zones will
           only be combined into groups if they share the same hazards
           (regardless of whether they are grouped together in combos).
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
            self.debug_print("self._segmentList = %s" % (self._pp.pformat(self._segmentList)), 1)
            self.debug_print("current combo = %s" % (self._pp.pformat(combo)), 1)
            segmentMapping = map(self._findSegment, combo)
            self.debug_print("   segmentMapping = %s" % (self._pp.pformat(segmentMapping)), 1)

            # segmentDict keys will be the hazSegments and
            #   we will gather all the areas of the combos that appear
            #   in each of these hazSegments
            segmentDict = {}
            keyList = []
            for areaName in combo:
                self.debug_print("       Adding %s" % (areaName), 1)
                key = tuple(segmentMapping[combo.index(areaName)])
                if key == ():  # If no hazard for area, do not include
                    continue
                if key not in keyList:
                    keyList.append(key)
                segmentDict.setdefault(key,[]).append(areaName)
            self.debug_print("   segmentDict = %s" % (self._pp.pformat(segmentDict)), 1)

            # Keep track of the areas that we are including
            for key in keyList:
                segAreas = segmentDict[key]
                newAreas = newAreas + segAreas
                newSegments.append(segAreas)
            self.debug_print("   newAreas = %s" % (self._pp.pformat(newAreas)), 1)
            self.debug_print("   newSegments = %s" % (self._pp.pformat(newSegments)), 1)
        self.debug_print("   newSegments = %s" % (self._pp.pformat(newSegments)), 1)
        # Now add in the hazAreas that have not been accounted for
        #   in the combinations
        for hazSegment in hazSegments:
            newSeg = []
            for hazArea in hazSegment:
                if hazArea not in newAreas:
                    newSeg.append(hazArea)
            if newSeg != []:
                newSegments.append(newSeg)
        self.debug_print("   final newSegments = %s" % (self._pp.pformat(newSegments)), 1)
        return newSegments
    
    def _makeSegmentEditAreas(self, argDict):
        # Create the edit areas that will be sampled
        areasList = self._allAreas()
        self.debug_print("areasList = %s" % (self._pp.pformat(areasList)), 1)
        editAreas = []
        self._editAreaDict = {}
        for area in areasList:
            self._editAreaDict[area] = area
            editAreas.append((area, area))
        return editAreas
    
    def _findSegment(self, areaName):
        # Determine which hazard group a zone belongs to
        for segment in self._segmentList:
            if areaName in segment:
                return segment
        return []
    
    ###############################################################
    ### Hazards related methods
    
    def _getAllVTECRecords(self):
        allRecords = []
        # Only the segments in _segmentList contain hazards so no
        # need to check everything in _allAreas()
        for segment in self._segmentList:
            allRecords += self._getVtecRecords(segment)
        
        return allRecords
    
    def _getHazardsForHLS(self):
        # Get all the hazards so that the HLS will have access to them.
        # Areas that share the same hazards are grouped together
        # into a single hazard.
        hazardTable = self._argDict["hazards"]
        
        # Create a single grouping of all zones. This will make it so that
        # the hazards are grouped together as much as possible so that we
        # don't repeat hazard information for zones in HLS.
        combos = [([self._allAreas()], "AllAreas")]
        
        # "Overlay" this group of all zones onto the segments
        # so that we get as few groups of zones as possible.
        refinedHazardSegments = self._getRefinedHazardSegments(combos)
        
        allHazards = []
        for segment in refinedHazardSegments:
            hazardsList = hazardTable.getHazardList(segment)
            for hazard in hazardsList:
                # If this is a correction, don't generate new hazards,
                # use the previous ones
                if hazard['act'] == 'COR':
                    return self._previousAdvisory["HazardsForHLS"]
                else:
                    # Tropical hazards shouldn't ever have EXT and EXB actions since
                    # they are "until further notice"
                    if hazard["act"] == "EXT":
                        hazard["act"] = "CON"
                    elif hazard["act"] == "EXB":
                        hazard["act"] = "EXA"
                    
                    allHazards.append(hazard)
        
        return allHazards
    
    ###############################################################
    ### Time related methods

    def _convertToISO(self, time_ms, local=None):
        # Convert milliseconds since the epoch to a date
        import datetime
        dt = datetime.datetime.fromtimestamp(time_ms / 1000)
        if local:
            timeZone = self._timeZones[0]
        else:
            timeZone = None
        return self._tpc.formatDatetime(dt, timeZone=timeZone)

    def _convertToDatetime(self, time_ms):
        import datetime
        return datetime.datetime.fromtimestamp(time_ms / 1000)
    
    ###############################################################
    ### Advisory related methods
    
    def _initializeSegmentZoneData(self, segment):
        # The current advisory will be populated when getting a section's stats
        self._currentAdvisory['ZoneData'][segment] = {
            "WindThreat":                    None,
            "WindForecast":                  None,
            "WindHighestPhaseReached":       None,
            "StormSurgeThreat":              None,
            "StormSurgeForecast":            None,
            "StormSurgeHighestPhaseReached": None,
            "FloodingRainThreat":            None,
            "FloodingRainForecast":          None,
            "TornadoThreat":                 None,
        }
    
    def _getPreviousAdvisories(self):
        stormAdvisories = self._getStormAdvisoryNames()
        
        self.debug_print("DEBUG: stormAdvisories = %s" % 
                         (self._pp.pformat(stormAdvisories)), 1)
        
        previousAdvisories = []
        
        #  Get the current storm number from the TCP (ie. AL092016)
        curStormNumber = self._getStormNumberStringFromTCP()
        self.debug_print("DEBUG: curStormNumber = %s" % 
                         (curStormNumber), 1)
     
        #  Filter out the advisories we wish to process further
        for advisory in stormAdvisories:
            
            #  If this was an advisory for the current storm
            if advisory.startswith(curStormNumber):

                #  Load this advisory for this storm
                curAdvisory = self._loadAdvisory(advisory)
                
                if curAdvisory is not None:
                    previousAdvisories.append(curAdvisory)

        
        self.debug_print("DEBUG: previous advisories = %s" % 
                         (self._pp.pformat(previousAdvisories)), 1)
        
        return previousAdvisories
    
    def _archiveCurrentAdvisory(self):
        ### Determine if all actions are canceled
        allCAN = True
        for vtecRecord in self._getAllVTECRecords():
            action = vtecRecord['act']
            if action != "CAN":
                allCAN = False
                break
        
        self._currentAdvisory["AllCAN"] = allCAN
        self._currentAdvisory["CreationTime"] = self._issueTime_secs
        self._currentAdvisory["Transmitted"] = False
        self._currentAdvisory["StormName"] = self._getStormNameFromTCP()
        self._currentAdvisory["StormNumber"] = self._getStormNumberStringFromTCP()
        self._currentAdvisory["StormID"] = self._getStormIDStringFromTCP()
        self._currentAdvisory["AdvisoryNumber"] = self._getAdvisoryNumberStringFromTCP()
        self._currentAdvisory["HazardsForHLS"] = self._getHazardsForHLS()
        
        self._saveAdvisory("pending", self._currentAdvisory)
    
    def _saveAdvisory(self, advisoryName, advisoryDict):
        self._synchronizeAdvisories()
        fileName = self._getAdvisoryFilename(advisoryName)

        self.debug_print("Saving %s to %s" % (advisoryName, fileName), 1)
        self.debug_print("advisoryDict: %s" % (self._pp.pformat(advisoryDict)), 1)
         
        try:
            JsonSupport.saveToJson(LocalizationSupport.CAVE_STATIC,
                                   self._site,
                                   fileName,
                                   advisoryDict)
        except Exception, e:
            self.debug_print("Save Exception for %s : %s" % (fileName, e), 1)
        else: # No exceptions occurred
            self.debug_print("Wrote file contents for: %s" % (fileName), 1)
            
            # Purposely allow this to throw
            self._synchronizeAdvisories()
    
    ###############################################################
    ### GUI related methods
    
    def _overview_list(self):
        if self._site == "HFO":
            stormInfoOptions = ["TCPCP1", "TCPCP2", "TCPCP3", "TCPCP4", "TCPCP5"]
        else:
            stormInfoOptions = ["TCPAT1", "TCPAT2", "TCPAT3", "TCPAT4", "TCPAT5"]
        
        stormInfoOptions.append("Enter PIL below (e.g. WRKTCP):")
        
        return [
            {
            "name": "StormInfo",
            "label": "Obtain Storm Type/Name/Info",
            "options": stormInfoOptions,
            "entryField": "     ",
            },
            {
            "name": "PopulateSurge",
            "label": "Populate Surge Section",
            "options": [
                ("Populate", True),
                ("Do not populate", False),
                ],
            "default": "Populate",
            },
            {
            "name": "WSPGridsAvailable",
            "label": "Are WSP grids available?",
            "options": [
                ("Yes", True),
                ("No", False),
                ],
            "default": "Yes",
            },
            ]
    
    def _displayGUI(self, infoDict=None):
        dialog = Overview_Dialog(self, "TCV", infoDict)
        status = dialog.status()
        LogStream.logVerbose("status="+status)
        if status == "Cancel":
            return None
        else:
            return dialog.getVarDict()

import Tkinter
class Overview_Dialog(HLSTCV_Common.Common_Dialog):
    def __init__(self, parent, title, infoDict=None):
        HLSTCV_Common.Common_Dialog.__init__(self, parent, title, infoDict)

    def body(self, master):
        # build the main display dialog
        tkObject_dict = self._tkObject_dict
        overviewList = self._parent._overview_list()
        fontDict = self._parent._font_GUI_dict()
            
        #  OVERVIEW header
        headerFG, headerFont = fontDict["headers"]
        frame = Tkinter.Frame(master, relief=Tkinter.GROOVE, borderwidth=1)
        frame.pack(side=Tkinter.TOP, fill=Tkinter.X, expand=Tkinter.NO)

        numBoxes = 2

        boxes = []
        for i in range(numBoxes):
            newBox = Tkinter.Frame(master)
            newBox.pack(side=Tkinter.TOP, expand=Tkinter.NO,
              fill=Tkinter.Y, anchor=Tkinter.W)
            boxes.append(newBox)

        for infoDict in overviewList:
            name = infoDict["name"]
            label = infoDict["label"]
            options = infoDict.get("options", [])
            entryField = infoDict.get("entryField", None)
            default = infoDict.get("default", None)
            optionType = infoDict.get("optionType", "radio")

            index = overviewList.index(infoDict)
            if index == 0:
                boxNum = 0
                buttonSide=Tkinter.TOP
                frameSide = Tkinter.LEFT
            else:
                boxNum = 1
                buttonSide=Tkinter.LEFT
                frameSide=Tkinter.TOP

            box = boxes[boxNum]
            
            tkObject_dict[name], entryObject = self._makeRadioOrCheckList(
                    box, label, options, default, buttonSide=buttonSide, frameSide=frameSide,
                    entryField=entryField, headerFG=headerFG,
                    headerFont=headerFont, boxType=optionType)
            if entryObject is not None:
                tkObject_dict[self._entryName(name)] = entryObject

        # End Instructions and Button
        frame = Tkinter.Frame(master, relief=Tkinter.GROOVE, borderwidth=1)
        self._makeButtons(frame)
        frame.pack(side=Tkinter.TOP, fill=Tkinter.X, expand=Tkinter.NO)        

    def _makeButtons(self, master):
        frame = Tkinter.Frame(master)
        buttonList = self._parent._GUI1_configDict().get("buttonList", [])
        for button, label in buttonList:
            if button == "Run":
                command = self.okCB
            else: # Cancel
                command = self.cancelCB
            Tkinter.Button(frame, text=label, command=command, width=10, 
                           state=Tkinter.NORMAL).pack(side=Tkinter.LEFT, pady=5, padx=10)
        frame.pack()

    def okCB(self):
        # pull the data from the tkObject_dict before they get toasted
        tkObject_dict  = self._tkObject_dict
        overviewList = self._parent._overview_list()
        for infoDict in overviewList:
            name = infoDict["name"]
            label = infoDict["label"]            
            options = infoDict.get("options", [])
            entryField = infoDict.get("entryField", None)
            default = infoDict.get("default", None)
            optionType = infoDict.get("optionType", "radio")

            if optionType == "check":
                checkList = []
                ivarList = tkObject_dict[name]
                for i in range(len(options)):
                    if ivarList[i].get():
                        checkList.append(options[i])
                value = checkList
                self._setVarDict(name, value)
            else:
                value = tkObject_dict[name].get()
                self._setVarDict(name, value, options)
                        
            if entryField is not None:
                entryName = self._entryName(name)
                self._setVarDict(entryName, tkObject_dict[entryName].get())
        # close window and set status "Ok"
        self._status = "Ok"
        self.withdraw()
        self.ok()


class SectionCommon():
    def __init__(self, textProduct, segment, sectionHeaderName):
        self._textProduct = textProduct
        self._sectionHeaderName = sectionHeaderName
        self._segment = segment
        self._tr = None
        self.isThreatNoneForEntireStorm = True
        
    def _isThreatNoneForEntireStorm(self, threatName):
        previousAdvisories = self._textProduct._getPreviousAdvisories()

        # For the first advisory, this needs to be false otherwise
        # potential impacts could be wrong
        if len(previousAdvisories) == 0:
            return False
        
        for advisory in previousAdvisories:
            if advisory["ZoneData"][self._segment][threatName] != "None":
                return False
        
        return True
    
    def _setProductPartValue(self, dictionary, productPartName, value):
        dictionary[self._sectionName + '._' + productPartName] = value
        
    def _finalSectionParts(self, segment_vtecRecords_tuple, parts):
        finalParts = []
        for partName in parts:
            if partName not in self._textProduct._noOpParts():
                finalParts.append(self._sectionName + '._' + partName)
            else:
                finalParts.append(partName)
        
        return [{
                'arguments': segment_vtecRecords_tuple,
                'partsList': finalParts
                }]
    
    def _sectionHeader(self, segmentDict, productSegmentGroup, productSegment):
        self._setProductPartValue(segmentDict, 'sectionHeader', self._sectionHeaderName)
    
    def _lifePropertyThreatSummary(self, segmentDict, productSegmentGroup, productSegment):
        if self._stats._maxThreat is not None:
            threatLevel = self._stats._maxThreat
            if threatLevel == "Mod":
                threatLevel = "Moderate"

            self._setProductPartValue(segmentDict, 'lifePropertyThreatSummary',
                                      "CURRENT THREAT TO LIFE AND PROPERTY: " + threatLevel)

    #  This new method will convert the single word threat trend into 
    #  an appropriate sentence 
    def _getThreatTrendSentence(self, section, threatTrendValue):
        
        if threatTrendValue.upper() == "INCREASING":
            text = "The %s threat has increased" % (section)
        elif threatTrendValue.upper() == "DECREASING":
            text = "The %s threat has decreased" % (section)
        elif threatTrendValue.upper() == "NEARLY STEADY":
            text = "The %s threat has remained nearly steady" % (section)
            
        return text + " from the previous assessment."

    def _getThreatTrendValue(self, elementName, magnitudeIncreaseThreshold):
        threatKey = elementName + "Threat"
        forecastKey = elementName + "Forecast"
        
        self._textProduct.debug_print("THREAT DEBUG for %s" % (elementName), 1)
        
        self._textProduct.debug_print("getThreatTrendValue _currentAdvisory =\n%s" % (self._textProduct._pp.pformat(self._stats._currentAdvisory)), 1)
        self._textProduct.debug_print("getThreatTrendValue _previousAdvisory =\n%s" % (self._textProduct._pp.pformat(self._stats._previousAdvisory)), 1)
        
        if (self._stats._currentAdvisory is None) or (self._stats._previousAdvisory is None):
            # Only compute a threat trend if we have 2 or more advisories
            return None
        
        currentThreat = self._stats._currentAdvisory[threatKey]
        previousThreat = self._stats._previousAdvisory[threatKey]
        shorterTermTrendDifference = self._threatDifference(currentThreat, previousThreat)
        
        self._textProduct.debug_print("currentThreat = %s" % (self._textProduct._pp.pformat(currentThreat)), 1)
        self._textProduct.debug_print("previousThreat = %s" % (self._textProduct._pp.pformat(previousThreat)), 1)
        self._textProduct.debug_print("shorterTermTrendDifference = %s" % (shorterTermTrendDifference), 1)
        
        previousPreviousThreat = None
        longerTermTrendDifference = None
        if self._stats._previousPreviousAdvisory is not None:
            self._textProduct.debug_print("_previousPreviousAdvisory is not None", 1)
            previousPreviousThreat = self._stats._previousPreviousAdvisory[threatKey]
            self._textProduct.debug_print("previousPreviousThreat = %s" % (self._textProduct._pp.pformat(previousPreviousThreat)), 1)
            longerTermTrendDifference = self._threatDifference(currentThreat, previousPreviousThreat)
            self._textProduct.debug_print("longerTermTrendDifference = %s" % (longerTermTrendDifference), 1)
        
        threatTrendValue = "NEARLY STEADY"
        self._textProduct.debug_print("magnitudeIncreaseThreshold = %s   forecastKey = '%s'" % (magnitudeIncreaseThreshold, forecastKey), 1)
        if self._isThreatDecreasing(shorterTermTrendDifference, longerTermTrendDifference):
            self._textProduct.debug_print("threat is decreasing", 1)
            threatTrendValue = "DECREASING"
        elif self._isThreatIncreasing(shorterTermTrendDifference, longerTermTrendDifference):
            self._textProduct.debug_print("threat is increasing", 1)
            threatTrendValue = "INCREASING"
        # NOTE: Modified so more threat levels can be classified as increasing when forecast has increased
        elif currentThreat in ["Mod", "High", "Extreme"] and \
             self._isMagnitudeIncreasing(forecastKey, magnitudeIncreaseThreshold):
            self._textProduct.debug_print("Increasing based on magnitude", 1)
            threatTrendValue = "INCREASING"

        return threatTrendValue
    
    def _threatDifference(self, threat1, threat2):
        threatLevels = self._textProduct._threatKeyOrder()
        self._textProduct.debug_print("threat1 index = %s" % (threatLevels.index(threat1)), 1)
        self._textProduct.debug_print("threat2 index = %s" % (threatLevels.index(threat2)), 1)
        return threatLevels.index(threat1) - threatLevels.index(threat2)
    
    def _isThreatDecreasing(self, shorterTermTrendDifference, longerTermTrendDifference):
        #If the current threat is at least 1 category lower than both previous advisories
        if (shorterTermTrendDifference < 0 and \
            longerTermTrendDifference is not None and \
            longerTermTrendDifference < 0):
            self._textProduct.debug_print("the current threat is at least 1 category lower than both previous advisories", 1)
            return True
        #Or if the current threat decreased by more than 1 category
        elif shorterTermTrendDifference < -1:
            self._textProduct.debug_print("the current threat decreased by more than 1 category", 1)
            return True
        else:
            self._textProduct.debug_print("the current threat is not decreasing", 1)
            return False
        
    def _isThreatIncreasing(self, shorterTermTrendDifference, longerTermTrendDifference):
        #If the current threat is at least 1 category higher than both previous advisories
        if (shorterTermTrendDifference > 0 and \
            longerTermTrendDifference is not None and \
            longerTermTrendDifference > 0):
            self._textProduct.debug_print("the current threat is at least 1 category higher than both previous advisories", 1)
            return True
        #Or if the current threat increased by more than 1 category
        elif shorterTermTrendDifference > 1:
            self._textProduct.debug_print("the current threat increased by more than 1 category", 1)
            return True
        else:
            self._textProduct.debug_print("the current threat is not increasing", 1)
            return False
    
    def _advisoryHasValidKey(self, advisory, key):
        return (advisory is not None) and \
               (advisory.has_key(key)) and \
               (advisory[key] is not None)

    def _isMagnitudeIncreasing(self, forecastKey, threshold):
#         currentValue, previousValue, previousPreviousValue
        self._textProduct.debug_print("_isMagnitudeIncreasing", 1)
        self._textProduct.debug_print("forecastKey = %s" % (forecastKey), 1)
        self._textProduct.debug_print("threshold = %s" % (threshold), 1)
        
        if self._advisoryHasValidKey(self._stats._currentAdvisory, forecastKey) and \
           self._advisoryHasValidKey(self._stats._previousAdvisory, forecastKey):
            currentValue = self._stats._currentAdvisory[forecastKey]
            previousValue = self._stats._previousAdvisory[forecastKey]
            self._textProduct.debug_print("currentValue = %s" % (currentValue), 1)
            self._textProduct.debug_print("previousValue = %s" % (previousValue), 1)
            
            if (currentValue - previousValue) >= threshold:
                self._textProduct.debug_print("the current magnitude has increased by more than the threshold since the last advisory", 1)
                return True
            elif self._advisoryHasValidKey(self._stats._previousPreviousAdvisory, forecastKey):
                previousPreviousValue = self._stats._previousPreviousAdvisory[forecastKey]
                self._textProduct.debug_print("previousPreviousValue = %s" % (previousPreviousValue), 1)
                
                if (currentValue - previousPreviousValue) >= threshold:
                    self._textProduct.debug_print("the current magnitude has increased by more than the threshold since the previous previous advisory", 1)
                    return True
                else:
                    self._textProduct.debug_print("the current magnitude does not meet the requirements to be considered increasing", 1)
                    return False
            else:
                self._textProduct.debug_print("the current magnitude did not increase past threshold and could not look at the previous previous advisory", 1)
                return False
        else:
            self._textProduct.debug_print("the current advisory and/or previous advisory did not have key: %s" % (forecastKey), 1)
            return False
        
    def _calculateThreatStatementTr(self, onsetHour, endHour, section):
        tr = "default"
        
        self._textProduct.debug_print("onset hour = %s" % (onsetHour), 1)
        self._textProduct.debug_print("end hour = %s" % (endHour), 1)
        
        if (onsetHour is not None):
            if onsetHour > 36:
                tr = "check plans"
            elif onsetHour > 6:
                tr = "complete preparations"
            elif (onsetHour <= 6) and (endHour is not None) and (endHour > 0):
                tr = "hunker down"
        
        self._textProduct.debug_print("Before default section. %s tr is currently -> %s for %s" % (section, tr, self._segment), 1)

        # Will need to redo this logic when SS hazards are used
        if section == "Wind":
            threatGrid = "WindThreat"
        elif section == "Surge":
            threatGrid = "StormSurgeThreat"
        
        # we are here because we had no onset time
        if tr == "default":
            if self._textProduct._currentAdvisory['ZoneData'][self._segment][threatGrid] in \
               ["Elevated", "Mod", "High", "Extreme"]:
                tr = "check plans"

            # Checking to see if we ever had a threat. If so, set to recovery
            elif self._pastThreatsNotNone(threatGrid):
                    tr = "recovery"
                                
            # If we are still default, that means we have no onset and never had any threat
            if tr == "default":
                tr = "check plans"
        
        self._textProduct.debug_print("After default section. %s tr is -> %s for %s" % (section, tr, self._segment), 1)
                           
        # ---------------------------------------------------------------------
        # Don't allow the event to regress to an earlier phase for this section
        
        # "default" isn't ordered because it can occur at multiple points before the recovery phase
        phaseOrder = [None, "check plans", "complete preparations", "hunker down", "recovery"]
        
        if self._sectionHeaderName == "Storm Surge":
            highestPhaseReachedField = "StormSurgeHighestPhaseReached"
        else: # Flooding Rain and Tornado are tied to Wind so that's why they use Wind's phase
            highestPhaseReachedField = "WindHighestPhaseReached"

        previousHighestPhaseReached = None
        if self._textProduct._previousAdvisory is not None:
            previousHighestPhaseReached = self._textProduct._previousAdvisory['ZoneData'][self._segment][highestPhaseReachedField]
              
        currentHighestPhaseReached = self._textProduct._currentAdvisory['ZoneData'][self._segment][highestPhaseReachedField]
        
        if phaseOrder.index(currentHighestPhaseReached) >= phaseOrder.index(previousHighestPhaseReached):
            highestPhaseReached = currentHighestPhaseReached
        else:
            highestPhaseReached = previousHighestPhaseReached
               
        if tr == "default":
            if highestPhaseReached == "recovery":
                tr = "recovery"
        else:
            highestPhaseIndex = phaseOrder.index(highestPhaseReached)
            
            self._textProduct.debug_print("highestPhaseReached so far for %s is -> '%s' for '%s'" \
                                          % (self._sectionHeaderName, highestPhaseReached, self._segment), 1)
            
            currentPhaseIndex = phaseOrder.index(tr)
            if currentPhaseIndex < highestPhaseIndex:
                tr = highestPhaseReached
                if currentHighestPhaseReached is None:
                    self._textProduct._currentAdvisory['ZoneData'][self._segment][highestPhaseReachedField] = tr
            elif currentPhaseIndex >= highestPhaseIndex:
                self._textProduct._currentAdvisory['ZoneData'][self._segment][highestPhaseReachedField] = tr
                
        dict = self._textProduct._currentAdvisory['ZoneData'][self._segment][highestPhaseReachedField]
        self._textProduct.debug_print("End of method. %s tr is -> %s for %s" % (section, tr, self._segment), 1)
        self._textProduct.debug_print("End of method. %s dict tr is -> %s for %s" % (section, dict, self._segment), 1)
        
        return tr
    
    def _pastWindHazardWasCAN(self):
        previousAdvisories = self._textProduct._getPreviousAdvisories()
        
        #  If there are NOT any advisories to process - no need to continue
        if len(previousAdvisories) == 0:
            return False
        
        #  Look at all past advisories for this storm
        for advisory in previousAdvisories:

            for hazard in advisory["HazardsForHLS"]:
                if self._segment in hazard["id"] and \
                   hazard["phen"] in ["TR", "HU"] and \
                   hazard["sig"] == "W" and \
                   hazard["act"] == "CAN":
                    return True
        
        return False
    
    def _pastThreatsNotNone(self, threatGrid):
        
        # Will need to modify this to be both Wind and Surge once SS codes are added
        previousAdvisories = self._textProduct._getPreviousAdvisories()
        
        #  If there are NOT any advisories to process - no need to continue
        if len(previousAdvisories) == 0:
            return False
        
        #  Look at all past advisories for this storm
        for advisory in previousAdvisories:

            #  We had a threat previously
            if advisory["ZoneData"][self._segment][threatGrid] in ["Elevated", "Mod", "High", "Extreme"]:
                return True
        
        return False
    
    def _setThreatStatementsProductParts(self, segmentDict, productSegment, tr):
        
        self._textProduct.debug_print("tr = %s   %s" % 
                          (self._textProduct._pp.pformat(tr), self._sectionHeaderName), 1)  
#        if tr is not None and self._stats._maxThreat is not None:
        if tr is not None:  
            (planning, action, preparation) = self._getThreatStatements(productSegment,
                                                                        self._sectionHeaderName,
                                                                        self._stats._maxThreat,
                                                                        tr)
    
            self._setProductPartValue(segmentDict, 'threatStatements',
                                      [planning, action, preparation])
        else:
            self._textProduct.debug_print("this is not a valid time range", 1)
            return
    
    def _getThreatStatements(self, productSegment, sectionName, maxThreat, tr):
#         import TCVDictionary
#         threatStatements = TCVDictionary.ThreatStatements
        
        with open("/awips2/cave/etc/gfe/userPython/utilities/TCVDictionary.py", 'r') as pythonFile:
            fileContents = pythonFile.read()
            exec(fileContents)
        
        # ThreatStatements comes from TCVDictionary.py when it is exec'ed
        threatStatements = ThreatStatements
        
        self._textProduct.debug_print(40*"-", 1)
        self._textProduct.debug_print("sectionName = %s, maxThreat = %s, tr = %s" % 
                         (sectionName, maxThreat, self._textProduct._pp.pformat(tr)), 1)

#         if maxThreat is None:
#             maxThreat = "None"

        statements = threatStatements[sectionName][maxThreat][tr]
        planning = statements["planning"]
        preparation = statements["preparation"]
        action = statements["action"]
    
        return (planning, preparation, action)
        
    def _potentialImpactsSummary(self, segmentDict, productSegmentGroup, productSegment):
        if self._stats._maxThreat is not None:
            summary = self._getPotentialImpactsSummaryText(self._stats._maxThreat)
            self._setProductPartValue(segmentDict, 'potentialImpactsSummary', summary)

    def _getPotentialImpactsSummaryText(self, maxThreat):
        if self.isThreatNoneForEntireStorm:
            return "POTENTIAL IMPACTS: Little to None"
        if self._tr is not None and self._sectionHeaderName in ["Wind", "Storm Surge"]:
            if self._tr == "hunker down":
                return "POTENTIAL IMPACTS: Unfolding"
            elif self._tr == "recovery":
                return "REALIZED IMPACTS: Being Assessed"
        
        if maxThreat == "Extreme":
            impactLevel = "Devastating to Catastrophic"
        elif maxThreat == "High":
            impactLevel = "Extensive"
        elif maxThreat == "Mod":
            impactLevel = "Significant"
        elif maxThreat == "Elevated":
            impactLevel = "Limited"
        else:
            impactLevel = "Little to None"

        return "POTENTIAL IMPACTS: " + impactLevel
    
    def _potentialImpactsStatements(self, segmentDict, productSegmentGroup, productSegment):
        self._textProduct.debug_print("segment = %s, elementName = %s, maxThreat = %s" % 
                         (productSegment[0], self._sectionHeaderName, self._stats._maxThreat), 1)        
        if self._stats._maxThreat is not None:
            statements = self._getPotentialImpactsStatements(productSegment, self._sectionHeaderName, self._stats._maxThreat)
            self._setProductPartValue(segmentDict, 'potentialImpactsStatements', statements)
    
    def _getPotentialImpactsStatements(self, productSegment, elementName, maxThreat):
        import TCVDictionary
        potentialImpactStatements = TCVDictionary.PotentialImpactStatements
        statements = potentialImpactStatements[elementName][maxThreat]
        
        import TCVAreaDictionary
        tcv_AreaDictionary = TCVAreaDictionary.TCV_AreaDictionary

        segment, vtecRecords = productSegment
        
        self._textProduct.debug_print("zone number = %s, elementName = %s, maxThreat = %s, tr = %s" % 
                         (segment, elementName, maxThreat, self._tr), 1)
                
        if segment in tcv_AreaDictionary:
            potentialImpactStatements = tcv_AreaDictionary[segment]["potentialImpactsStatements"]

        # Check for any overrides
        try:
            statements = potentialImpactStatements[elementName][maxThreat]
        except KeyError:
            pass

        if self.isThreatNoneForEntireStorm:
            return statements

        if self._tr is not None:
            specialStatements = self._specialImpactsStatements()
            if self._tr in specialStatements.keys():
                if self._tr in ["recovery", "hunker down"] and self.isThreatNoneForEntireStorm:
                    return statements
                else:
                    return specialStatements[self._tr]

        #  If this is the "default" case
        if self._tr == "default" and len(statements) > 0:
            if elementName in ["Wind", "Storm Surge"]:
                if statements[0].find("If realized, ") == -1:
                   statements[0] = "If realized, " + statements[0][0].lower() + statements[0][1:]

        return statements 
    
    # Specific hazard sections can override this to provide special impacts statements
    def _specialImpactsStatements(self):
        return {}
    
    def _preparationStatement(self, severityString):
        preparationStatement = ""
        if severityString == "Devastating" or severityString == "Extensive impacts":
            preparationStatement += "Aggressive "
        
        preparationStatement += "preparations should be made for chance of "
        
        if severityString == "Devastating":
            preparationStatement += "devastating to catastrophic"
        elif severityString == "Extensive impacts":
            preparationStatement += "extensive"
        elif severityString == "Significant":
            preparationStatement += "significant"
        elif severityString == "Limited":
            preparationStatement += "limited"
        
        preparationStatement += " impacts based on latest threat"
        
        return preparationStatement

class WindSection(SectionCommon):
    def __init__(self, textProduct, segment, stats):
        SectionCommon.__init__(self, textProduct, segment, "Wind")
        self._sectionName = 'windSection[\'' + segment + '\']'
        self._stats = stats
        self.isThreatNoneForEntireStorm = self._isThreatNoneForEntireStorm("WindThreat")
        
    def sectionParts(self, segment_vtecRecords_tuple):
        parts = [
            'sectionHeader',
            'forecastSubsection',
            'threatSubsection',
            'impactsSubsection',
            ]
        
        return self._finalSectionParts(segment_vtecRecords_tuple, parts)
    
    def _forecastSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._latestForecastSummary(subsectionDict, productSegmentGroup, productSegment)
        
        if self._textProduct._WSPGridsAvailable:
            self._peakWind(subsectionDict, productSegmentGroup, productSegment)
            self._windowTS(subsectionDict, productSegmentGroup, productSegment)
            self._windowHU(subsectionDict, productSegmentGroup, productSegment)
            
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'forecastSubsection', subsectionDict)
    
    def _latestForecastSummary(self, segmentDict, productSegmentGroup, productSegment):
        if not self._textProduct._WSPGridsAvailable:
            self._setProductPartValue(segmentDict, 'latestForecastSummary',
                                      "LATEST LOCAL FORECAST: Not available at this time. To be updated shortly.")
        elif self._stats._maxWind is None:
            self._setProductPartValue(segmentDict, 'latestForecastSummary',
                                      "No wind forecast")
        else:
            categoryLabel = None
            categories = self._moderatedMaxWindMph_categories()
            moderatedMaxWind = self._ktToMph(self._stats._maxWind, "Wind")
            for key in categories.keys():
                minVal, maxVal = categories[key]
                if minVal <= moderatedMaxWind and moderatedMaxWind < maxVal:
                    categoryLabel = key
                    break
    
            forecastText = "LATEST LOCAL FORECAST: "
            if categoryLabel is not None:
                forecastText += "Equivalent " + categoryLabel + " force wind"
            else:
                segment, vtecRecords = productSegment
                numRecords = len(vtecRecords)
                possibleHazardsFound = False
                
                for i in range(numRecords):
                    vtecRecord = vtecRecords[i]
                    if (vtecRecord["phensig"] in ["HU.A", "HU.W", "TR.A", "TR.W"] or \
                       self._stats._windowTS is not None) and \
                       vtecRecord["act"] != "CAN":
                        forecastText += "Tropical storm force winds remain possible"
                        possibleHazardsFound = True
                        break
                if not possibleHazardsFound:
                    forecastText += "Below tropical storm force wind"
                        
            self._setProductPartValue(segmentDict, 'latestForecastSummary', forecastText)

    def _peakWind(self, segmentDict, productSegmentGroup, productSegment):
        if self._stats._maxWind is not None:
            windText = "Peak Wind Forecast: "
            moderatedMaxWind = self._ktToMph(self._stats._maxWind, "Wind")
            if moderatedMaxWind >= 74:
                maxRange = 20
            elif moderatedMaxWind >= 58:
                maxRange = 15
            elif moderatedMaxWind >= 20:
                maxRange = 10
            else:
                maxRange = 5

            windText += str(int(moderatedMaxWind - maxRange)) + "-" + str(int(moderatedMaxWind)) + " mph"
            if self._stats._maxGust is not None:
                moderatedMaxWindGust = self._ktToMph(self._stats._maxGust, "WindGust")
                
#                 #  We want to round the wind gust to the nearest 5 kt
#                 moderatedMaxWindGust = \
#                     self._textProduct.round(moderatedMaxWindGust, "Nearest", 5)
                
                windText += " with gusts to " + str(int(moderatedMaxWindGust)) + " mph"

            self._setProductPartValue(segmentDict, 'peakWind', windText)

    def _windowTS(self, segmentDict, productSegmentGroup, productSegment):
        if self._stats._windowTS is not None:
            self._setProductPartValue(segmentDict, 'windowTS', self._stats._windowTS)
        
    def _windowHU(self, segmentDict, productSegmentGroup, productSegment):
        if self._stats._windowHU is not None:
            self._setProductPartValue(segmentDict, 'windowHU', self._stats._windowHU)

    def _threatSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._lifePropertyThreatSummary(subsectionDict, productSegmentGroup, productSegment)
        
        if self._textProduct._WSPGridsAvailable:
            self._threatTrend(subsectionDict, productSegmentGroup, productSegment)
            self._threatStatements(subsectionDict, productSegmentGroup, productSegment)
            
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'threatSubsection', subsectionDict)
    
    def _lifePropertyThreatSummary(self, segmentDict, productSegmentGroup, productSegment):
        if not self._textProduct._WSPGridsAvailable:
            self._setProductPartValue(segmentDict, 'lifePropertyThreatSummary',
                                      "Threat to Life and Property: Not available at this time. To be updated shortly.")
        else:
            SectionCommon._lifePropertyThreatSummary(self, segmentDict, productSegmentGroup, productSegment)
    
    def _threatTrend(self, segmentDict, productSegmentGroup, productSegment):
        threatTrendValue = \
            self._getThreatTrendValue("Wind",
                                      magnitudeIncreaseThreshold=self._textProduct.mphToKt(15))
        
        if threatTrendValue is not None:
            #  Convert the threat trend to a sentence
            threatTrendSentence = \
                self._getThreatTrendSentence("wind", threatTrendValue)

            self._setProductPartValue(segmentDict, 'threatTrend',
                                      threatTrendSentence)
    
    def _threatStatements(self, segmentDict, productSegmentGroup, productSegment):
        self._tr = self._calculateThreatStatementTr(self._stats._onset34Hour,
                                                self._stats._end34Hour, "Wind")
        self._textProduct.debug_print("in _threatStatements tr = %s" % 
                                      (self._textProduct._pp.pformat(self._tr)), 1)
        
        if not hasattr(self._textProduct, "_windThreatStatementsTr"):
            self._textProduct._windThreatStatementsTr = dict()
            
        self._textProduct._windThreatStatementsTr[self._segment] = self._tr
        
        self._setThreatStatementsProductParts(segmentDict, productSegment, 
                                              self._tr)

    def _impactsSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._potentialImpactsSummary(subsectionDict, productSegmentGroup, productSegment)
        
        if self._textProduct._WSPGridsAvailable:
            self._potentialImpactsStatements(subsectionDict, productSegmentGroup, productSegment)
            
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'impactsSubsection', subsectionDict)
    
    def _specialImpactsStatements(self):
        return {"hunker down": ["Potential impacts from the main wind event are unfolding.",
                               # "The extent of realized impacts will depend on the actual strength, duration, and exposure of the wind as experienced at particular locations.",
                                ],
                "recovery": ["Little to no additional wind impacts expected. Community officials are now assessing the extent of actual wind impacts accordingly.",
                             ],
                }
    
    def _potentialImpactsSummary(self, segmentDict, productSegmentGroup, productSegment):
        if not self._textProduct._WSPGridsAvailable:
            self._setProductPartValue(segmentDict, 'potentialImpactsSummary',
                                      "POTENTIAL IMPACTS: Not available at this time. To be updated shortly.")
        else:
            SectionCommon._potentialImpactsSummary(self, segmentDict, productSegmentGroup, productSegment)

    ### Supporting functions
    def _moderatedMaxWindMph_categories(self):
        # Dictionary representing wind thresholds in kts
        # for category 1, 2, 3, 4 or 5 hurricanes.
        return {
            'Cat 5 Hurricane':       (157, 999),
            'Cat 4 Hurricane':       (130, 157),
            'Cat 3 Hurricane':       (111, 130),
            'Cat 2 Hurricane':       ( 96, 111),
            'Cat 1 Hurricane':       ( 74,  96),
            'Strong Tropical Storm': ( 58,  73),
            'Tropical Storm':        ( 39,  58),
            }

    def _ktToMph(self, value, element):
        newVal = self._textProduct.ktToMph(value)
        newVal = self._textProduct.round(newVal, "Nearest", self._increment(element))
        return newVal

    # This is a very simple way to round values -- if we need
    # something more sophisticated, we'll add it later.
    def _increment(self, element):
        dict = {
            "Wind": 5,
            "WindGust": 5,
            "InundationMax": 0.1,
            }
        return dict.get(element, 0)

class StormSurgeSection(SectionCommon):
    def __init__(self, textProduct, segment, stats):
        SectionCommon.__init__(self, textProduct, segment, "Storm Surge")
        self._sectionName = 'stormSurgeSection[\'' + segment + '\']'
        self._stats = stats
        self.isThreatNoneForEntireStorm = self._isThreatNoneForEntireStorm("StormSurgeThreat")
        
    def sectionParts(self, segment_vtecRecords_tuple):
        parts = [
            'sectionHeader',
            'forecastSubsection',
            'threatSubsection',
            'impactsSubsection',
            ]
        
        return self._finalSectionParts(segment_vtecRecords_tuple, parts)
    
    def _forecastSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._latestForecastSummary(subsectionDict, productSegmentGroup, productSegment)
        
        if self._textProduct._PopulateSurge:
            self._peakSurge(subsectionDict, productSegmentGroup, productSegment)
            self._surgeWindow(subsectionDict, productSegmentGroup, productSegment)
            
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'forecastSubsection', subsectionDict)
    
    def _latestForecastSummary(self, segmentDict, productSegmentGroup, productSegment):
        if not self._textProduct._PopulateSurge:
            self._setProductPartValue(segmentDict, 'latestForecastSummary',
                                      "LATEST LOCAL FORECAST: Not available at this time. To be updated shortly.")
 
        elif "None" in self._stats._windowSurge or \
             self._stats._inundationMax is None or \
             self._stats._inundationMax <= 1:
            self._setProductPartValue(segmentDict, 'latestForecastSummary',
                                      "No storm surge inundation forecast")
        else:
            max = self._stats._inundationMax
            summary = "LATEST LOCAL FORECAST: "

            if 1 < max and max < 4:
                summary += "Localized"
            elif 4 <= max and max < 12:
                summary += "Life-threatening"
            else:
                summary += "Life-threatening and historic"
                
            self._setProductPartValue(segmentDict, 'latestForecastSummary', 
                                      summary + " storm surge possible")

    def _peakSurge(self, segmentDict, productSegmentGroup, productSegment):
        self._textProduct.debug_print("_peakSurge _inundationMax = %s" % (self._stats._inundationMax), 1)
        
        # DR 17727: To make the output consistent, max threat should be calculated here
        self._stats._maxThreat = "None"
        
        if self._stats._inundationMax is not None and self._stats._inundationMax > 1:
            max = self._stats._inundationMax
            if max > 10:
                maxRange = 4
                self._stats._maxThreat = "Extreme"
            elif max > 6:
                maxRange = 3
                if max > 9:
                    self._stats._maxThreat = "Extreme"
                else:
                    self._stats._maxThreat = "High"
            elif max >= 3:
                maxRange = 2
                if max > 3:
                    self._stats._maxThreat = "Mod"
                else:
                    self._stats._maxThreat = "Elevated"
            else:
                maxRange = None
                if max > 1:
                    self._stats._maxThreat = "Elevated"
            
            self._textProduct.debug_print("_peakSurge maxRange = %s" % (maxRange), 1)
            self._textProduct.debug_print("_peakSurge _maxThreat = %s" % (self._stats._maxThreat), 1)
            
            # Save off the surge threat to the advisory
            self._textProduct._currentAdvisory['ZoneData'][self._segment]["StormSurgeThreat"] = self._stats._maxThreat

            if maxRange is not None:
                words = str(int(max - maxRange)) + "-" + str(int(max)) + " feet above ground"
            elif max > 0:
                
                #  We were getting really weird values of peak surge
                #  (e.g. "up to 1.70000004768 feet").  This fix will round up 
                #  to the nearest integer value
#                 words = "up to " + str(max) + " feet above ground"
                words = "up to " + str(int(max + 0.5)) + " feet above ground"
            else:
                words = ""

            if len(words) > 0:
                self._setProductPartValue(segmentDict, 'peakSurge',
                                          "Peak Storm Surge Inundation: The potential for " + words + " somewhere within surge prone areas")
            else:
                self._setProductPartValue(segmentDict, 'peakSurge',
                                          "Peak Storm Surge Inundation: The potential for little to no storm surge inundation")
        
    def _surgeWindow(self, segmentDict, productSegmentGroup, productSegment):
        if "None" not in self._stats._windowSurge:
            self._setProductPartValue(segmentDict, 'surgeWindow', self._stats._windowSurge)
    
    def _threatSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._lifePropertyThreatSummary(subsectionDict, productSegmentGroup, productSegment)
        
        if self._textProduct._PopulateSurge:
            self._threatTrend(subsectionDict, productSegmentGroup, productSegment)
            self._threatStatements(subsectionDict, productSegmentGroup, productSegment)
            
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'threatSubsection', subsectionDict)
    
    def _lifePropertyThreatSummary(self, segmentDict, productSegmentGroup, productSegment):
        if not self._textProduct._PopulateSurge:
            self._setProductPartValue(segmentDict, 'lifePropertyThreatSummary',
                                      "Threat to Life and Property: Not available at this time. To be updated shortly.")
        else:
            SectionCommon._lifePropertyThreatSummary(self, segmentDict, productSegmentGroup, productSegment)
    
    def _threatTrend(self, segmentDict, productSegmentGroup, productSegment):
        threatTrendValue = self._getThreatTrendValue("StormSurge", magnitudeIncreaseThreshold=4)

        if threatTrendValue is not None:
            #  Convert the threat trend to a sentence
            threatTrendSentence = \
                self._getThreatTrendSentence("storm surge", threatTrendValue)

            self._setProductPartValue(segmentDict, 'threatTrend',
                                      threatTrendSentence)
    
    def _threatStatements(self, segmentDict, productSegmentGroup, productSegment):
        self._textProduct.debug_print("Surge Threat Statements", 1)
        self._tr = self._calculateThreatStatementTr(self._stats._onsetSurgeHour,
                                            self._stats._endSurgeHour, "Surge")
        
        self._setThreatStatementsProductParts(segmentDict, productSegment, 
                                              self._tr)
        
    def _impactsSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._potentialImpactsSummary(subsectionDict, productSegmentGroup, productSegment)
        
        if self._textProduct._PopulateSurge:
            self._potentialImpactsStatements(subsectionDict, productSegmentGroup, productSegment)
            
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'impactsSubsection', subsectionDict)
    
    def _specialImpactsStatements(self):
        return {"hunker down": ["Potential impacts from the main surge event are unfolding.",
                               # "The extent of realized impacts will depend on the actual height of storm surge moving onshore and the resulting depth of coastal flooding as experienced at particular locations.",
                                ],
                "recovery": ["Little to no additional surge impacts expected. Community officials are now assessing the extent of actual surge impacts accordingly.",
                             ],
                }
    
    def _potentialImpactsSummary(self, segmentDict, productSegmentGroup, productSegment):
        if not self._textProduct._PopulateSurge:
            self._setProductPartValue(segmentDict, 'potentialImpactsSummary',
                                      "POTENTIAL IMPACTS: Not available at this time. To be updated shortly.")
        else:
            SectionCommon._potentialImpactsSummary(self, segmentDict, productSegmentGroup, productSegment)


class FloodingRainSection(SectionCommon):
    def __init__(self, textProduct, segment, stats):
        SectionCommon.__init__(self, textProduct, segment, "Flooding Rain")
        self._sectionName = 'floodingRainSection[\'' + segment + '\']'
        self._stats = stats
        self.isThreatNoneForEntireStorm = self._isThreatNoneForEntireStorm("FloodingRainThreat")
        
    def sectionParts(self, segment_vtecRecords_tuple):
        parts = [
            'sectionHeader',
            'forecastSubsection',
            'threatSubsection',
            'impactsSubsection',
            ]
        
        return self._finalSectionParts(segment_vtecRecords_tuple, parts)
    
    def _forecastSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._latestForecastSummary(subsectionDict, productSegmentGroup, productSegment)
        
        if self._textProduct._WSPGridsAvailable:
            self._peakRain(subsectionDict, productSegmentGroup, productSegment)
            
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'forecastSubsection', subsectionDict)
    
    def _latestForecastSummary(self, segmentDict, productSegmentGroup, productSegment):
        if not self._textProduct._WSPGridsAvailable:
            self._setProductPartValue(segmentDict, 'latestForecastSummary',
                                      "LATEST LOCAL FORECAST: Not available at this time. To be updated shortly.")
        else:
            summary = ""    # was "No Flood Watch is in effect"
            segment, vtecRecords = productSegment
            
            headlines, _ = self._textProduct._getAdditionalHazards()
            headlineList = self._textProduct._checkHazard(headlines,
                                                          [("FA","A"),("FF","A")],
                                                          returnList = True)
            
            if len(headlineList) != 0:
                # Extract the first flood headline out (there will only be 1 in effect at a time)
                (key, areaList) = headlineList[0]
                (headline, _, _, _) = key
                
                # Make sure it is for our zone
                if self._segment in areaList:
                    summary = headline + " is in effect"
            
            self._setProductPartValue(segmentDict, 'latestForecastSummary',
                                      "LATEST LOCAL FORECAST: " + summary)
    
    def _peakRain(self, segmentDict, productSegmentGroup, productSegment):
        if self._stats._sumAccum is not None:
            words = self._rainRange(int(self._stats._sumAccum + 0.5))
            
            #  If we have previous rainfall
            if self._stats._prevAccum not in [0.0, None] and (int(self._stats._sumAccum + 0.5)) != 0:
                words = "Additional " + words
            self._setProductPartValue(segmentDict, 'peakRain', "Peak Rainfall Amounts: " + words)
        
    def _rainRange(self, sumAccum):
        minAccum = 0
        maxAccum = 0
        
        if sumAccum == 0 and self._stats._prevAccum not in [0.0, None]:
            return "No additional significant rainfall forecast"
        elif sumAccum == 0 and self._stats._prevAccum in [0.0, None]:
            return "No significant rainfall forecast"
        elif sumAccum == 1:
            return "around 1 inch"
        elif sumAccum == 2:
            minAccum, maxAccum = (1, 3)
        elif sumAccum == 3:
            minAccum, maxAccum = (2, 4)
        elif sumAccum == 4:
            minAccum, maxAccum = (3, 5)
        elif sumAccum in [5,6,7]:
            minAccum, maxAccum = (4, 8)
        elif sumAccum in [8,9]:
            minAccum, maxAccum = (6, 10)
        elif sumAccum in [10,11]:
            minAccum, maxAccum = (8, 12)
        elif sumAccum in [12,13]:
            minAccum, maxAccum = (10, 14)
        elif sumAccum in [14,15,16,17]:
            minAccum, maxAccum = (12, 18)
        elif 17 < sumAccum and sumAccum < 25:
            minAccum, maxAccum = (18, 24)
        else:
            return "More than two feet"
        
        return "%d-%d inches, with locally higher amounts" % (minAccum, maxAccum)
    
    def _threatSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._lifePropertyThreatSummary(subsectionDict, productSegmentGroup, productSegment)
        
        if self._textProduct._WSPGridsAvailable:
            self._threatTrend(subsectionDict, productSegmentGroup, productSegment)
            self._threatStatements(subsectionDict, productSegmentGroup, productSegment)
            
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'threatSubsection', subsectionDict)
    
    def _lifePropertyThreatSummary(self, segmentDict, productSegmentGroup, productSegment):
        if not self._textProduct._WSPGridsAvailable:
            self._setProductPartValue(segmentDict, 'lifePropertyThreatSummary',
                                      "Threat to Life and Property: Not available at this time. To be updated shortly.")
        else:
            SectionCommon._lifePropertyThreatSummary(self, segmentDict, productSegmentGroup, productSegment)
    
    def _threatTrend(self, segmentDict, productSegmentGroup, productSegment):
        threatTrendValue = self._getThreatTrendValue("FloodingRain", magnitudeIncreaseThreshold=4)

        if threatTrendValue is not None:
            #  Convert the threat trend to a sentence
            threatTrendSentence = \
                self._getThreatTrendSentence("flooding rain", threatTrendValue)

            self._setProductPartValue(segmentDict, 'threatTrend',
                                      threatTrendSentence)
    
    def _threatStatements(self, segmentDict, productSegmentGroup, productSegment):
        self._tr = self._textProduct._windThreatStatementsTr[self._segment]
        
        self._setThreatStatementsProductParts(segmentDict, productSegment, self._tr)
        
    def _impactsSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._potentialImpactsSummary(subsectionDict, productSegmentGroup, productSegment)
        
        if self._textProduct._WSPGridsAvailable:
            self._potentialImpactsStatements(subsectionDict, productSegmentGroup, productSegment)
            
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'impactsSubsection', subsectionDict)
    
    def _potentialImpactsSummary(self, segmentDict, productSegmentGroup, productSegment):
        if not self._textProduct._WSPGridsAvailable:
            self._setProductPartValue(segmentDict, 'potentialImpactsSummary',
                                      "POTENTIAL IMPACTS: Not available at this time. To be updated shortly.")
        else:
            SectionCommon._potentialImpactsSummary(self, segmentDict, productSegmentGroup, productSegment)

class TornadoSection(SectionCommon):
    def __init__(self, textProduct, segment, stats):
        SectionCommon.__init__(self, textProduct, segment, "Tornado")
        self._sectionName = 'tornadoSection[\'' + segment + '\']'
        self._stats = stats
        self.isThreatNoneForEntireStorm = self._isThreatNoneForEntireStorm("TornadoThreat")
        
    def sectionParts(self, segment_vtecRecords_tuple):
        parts = [
            'sectionHeader',
            'forecastSubsection',
            'threatSubsection',
            'impactsSubsection',
            ]
        
        return self._finalSectionParts(segment_vtecRecords_tuple, parts)
        
    def _forecastSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._latestForecastSummary(subsectionDict, productSegmentGroup, productSegment)
        
        if self._textProduct._WSPGridsAvailable:
            self._tornadoSituation(subsectionDict, productSegmentGroup, productSegment)
            
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'forecastSubsection', subsectionDict)
    
    def _latestForecastSummary(self, segmentDict, productSegmentGroup, productSegment):
        if not self._textProduct._WSPGridsAvailable:
            self._setProductPartValue(segmentDict, 'latestForecastSummary',
                                      "LATEST LOCAL FORECAST: Not available at this time. To be updated shortly.")
        else:
            summary = ""
            segment, vtecRecords = productSegment
            
            headlines, _ = self._textProduct._getAdditionalHazards()
            headlineList = self._textProduct._checkHazard(headlines,
                                                          [("TO","A")],
                                                          returnList = True)
            if len(headlineList) != 0:
                # Extract the first tornado headline out (there will only be 1 in effect at a time)
                (key, areaList) = headlineList[0]
                (headline, _, _, _) = key
                
                # Make sure it is for our zone
                if self._segment in areaList:
                    summary = "Tornado Watch is in effect"
    
            self._setProductPartValue(segmentDict, 'latestForecastSummary',
                                      "LATEST LOCAL FORECAST: " + summary)
 
    def _tornadoSituation(self, segmentDict, productSegmentGroup, productSegment):

        #  Now add the bullet about tornado situation
        if self._stats._maxThreat in ["Extreme", "High"]:
            qualifier = "very favorable"
        elif self._stats._maxThreat in ["Mod"]:
            qualifier = "favorable"
        elif self._stats._maxThreat in ["Elevated"]:
            qualifier = "somewhat favorable"
        else:
            qualifier = "unfavorable"
            
        words = "Situation is %s for tornadoes" % (qualifier) 
        
        self._setProductPartValue(segmentDict, 'tornadoSituation', words)
        

    def _threatSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._lifePropertyThreatSummary(subsectionDict, productSegmentGroup, productSegment)
        
        if self._textProduct._WSPGridsAvailable:
            self._threatTrend(subsectionDict, productSegmentGroup, productSegment)
            self._threatStatements(subsectionDict, productSegmentGroup, productSegment)
            
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'threatSubsection', subsectionDict)
    
    def _lifePropertyThreatSummary(self, segmentDict, productSegmentGroup, productSegment):
        if not self._textProduct._WSPGridsAvailable:
            self._setProductPartValue(segmentDict, 'lifePropertyThreatSummary',
                                      "Threat to Life and Property: Not available at this time. To be updated shortly.")
        else:
            SectionCommon._lifePropertyThreatSummary(self, segmentDict, productSegmentGroup, productSegment)
    
    def _threatTrend(self, segmentDict, productSegmentGroup, productSegment):
        threatTrendValue = self._getThreatTrendValue("Tornado", 
                                                magnitudeIncreaseThreshold=None)
        
        if threatTrendValue is not None:
            #  Convert the threat trend to a sentence
            threatTrendSentence = \
                self._getThreatTrendSentence("tornado", threatTrendValue)

            self._setProductPartValue(segmentDict, 'threatTrend',
                                      threatTrendSentence)
        
    def _threatStatements(self, segmentDict, productSegmentGroup, productSegment):
        self._tr = self._textProduct._windThreatStatementsTr[self._segment]

        self._setThreatStatementsProductParts(segmentDict, productSegment, self._tr)
    
    def _impactsSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._potentialImpactsSummary(subsectionDict, productSegmentGroup, productSegment)
        
        if self._textProduct._WSPGridsAvailable:
            self._potentialImpactsStatements(subsectionDict, productSegmentGroup, productSegment)
            
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'impactsSubsection', subsectionDict)
    
    def _potentialImpactsSummary(self, segmentDict, productSegmentGroup, productSegment):
        if not self._textProduct._WSPGridsAvailable:
            self._setProductPartValue(segmentDict, 'potentialImpactsSummary',
                                      "POTENTIAL IMPACTS: Not available at this time. To be updated shortly.")
        else:
            SectionCommon._potentialImpactsSummary(self, segmentDict, productSegmentGroup, productSegment)


###############################################################
### TCV Statistics Classes
    
class SectionCommonStats():
    def __init__(self, textProduct, segment):
        self._textProduct = textProduct
        self._segment = segment
        
        self._initializeSegmentAdvisories()
        
        # The maximum threat level during the entire advisory
        self._maxThreat = None

    
    def _initializeSegmentAdvisories(self):
        self._currentAdvisory = self._textProduct._currentAdvisory['ZoneData'][self._segment]
        
        self._previousAdvisory = None
        self._textProduct.debug_print("textProduct._previousAdvisory = '%s'" % (self._textProduct._previousAdvisory))
        if self._textProduct._previousAdvisory is not None:
            if self._textProduct._previousAdvisory['ZoneData'].has_key(self._segment):
                self._previousAdvisory = self._textProduct._previousAdvisory['ZoneData'][self._segment]
            
        self._textProduct.debug_print("textProduct._previousPreviousAdvisory = '%s'" % \
            (self._textProduct._previousPreviousAdvisory))
        self._previousPreviousAdvisory = None
        if self._textProduct._previousPreviousAdvisory is not None:
            self._previousPreviousAdvisory = self._textProduct._previousPreviousAdvisory['ZoneData'][self._segment]
    
    def _updateThreatStats(self, tr, statDict, threatGridName):
        self._textProduct.debug_print("In _updateThreatStats for %s" % (threatGridName), 1)
        self._textProduct.debug_print("maxThreat = %s" % (self._maxThreat), 1)
        
        threatLevel = self._textProduct._getStatValue(statDict, threatGridName)
        if threatLevel is not None:
            threatLevels = self._textProduct._threatKeyOrder()
            self._textProduct.debug_print("current threatLevel = %s" % (threatLevel), 1)
            if self._maxThreat is None or \
               threatLevels.index(threatLevel) > threatLevels.index(self._maxThreat):
                self._textProduct.debug_print("updating max threat to = %s" % (threatLevel), 1)
                self._maxThreat = threatLevel
    
    def _calculateHourOffset(self, targetTime):
        self._textProduct.debug_print("Calculating hours from issuance time for %s"
            % (self._textProduct._pp.pformat(targetTime)), 1)
        self._textProduct.debug_print("target unix time = %s"
            % (self._textProduct._pp.pformat(targetTime.unixTime())), 1)
        self._textProduct.debug_print("issuance unix time = %s"
            % (self._textProduct._pp.pformat(self._textProduct._issueTime_secs)), 1)
        
        seconds = targetTime.unixTime() - self._textProduct._issueTime_secs
        hour = int(round(seconds/60.0/60.0))
        self._textProduct.debug_print("hour offset = %s" % (hour), 1)
        if hour < 0:
            hour = 0
        
        self._textProduct.debug_print("final hour offset = %s" % (hour), 1)
        
        return hour

class WindSectionStats(SectionCommonStats):
    def __init__(self, textProduct, segment, statList, timeRangeList):
        SectionCommonStats.__init__(self, textProduct, segment)
        # The maximum wind speed that occurs during the entire advisory.
        self._maxWind = None
        
        # The maximum wind gust speed that occurs during the entire advisory.
        self._maxGust = None
        
        # The number of hours since the issuance time when the wind first becomes >= 34 kts.
        self._onset34Hour = None
        
        # The number of hours since the issuance time when the wind drops below 34 kts.
        self._end34Hour = None
        
        # Text describing when tropical storm force winds (>= 34 kts) start and end.
        self._windowTS = None
        
        # Text describing when hurricane force winds (>= 64 kts) start and end.
        self._windowHU = None
        
        # Only gather stats if we have the wind speed probability grids available
        if self._textProduct._WSPGridsAvailable:
            self._textProduct.debug_print("#"*90)
            self._textProduct.debug_print("Setting wind stats for %s" % (segment), 1)
            
            self._setStats(statList, timeRangeList)
            self._textProduct.debug_print("#"*90)
    
    # pws34int and pws64int grids give you the probability of 34/64 kt winds
    # occurring during the grid time range. The grids are 6 hours long so they
    # give you a more specific starting or ending time which allows for better
    # descriptions of when events start.
    class PwsXXintStats():
        def __init__(self):
            # The maximum value in pws34/64int grids across the entire advisory.
            self.max = None
            
            # The number of hours since the issuance time when this maximum value first occurs.
            self.onsetHour = None
    
    # pwsD34, pwsN34, pwsD64 and pwsN64 grids give you the probability of 34/64
    # kt winds occurring during the grid time range. They are 12 hour long day
    # and night grids that match ZPF periods. They give you a ball park idea of
    # when an event will start or end and if it's day or night time and then
    # the pwsXXint grids can be used to narrow down the time frame.
    class PwsTXXStats():
        def __init__(self):
            # Depending on when the issuance time is, there may be a day or night
            # grid that we need to drop at the beginning so that we start with the
            # grid that occurs during our issuance time so that our windows are
            # accurate.
            
            # We need to do special logic the first time around so record if this
            # is the first run through the loop or not.
            self.firstRun = True
            
            # Indicates if we need to possibly drop the first grid or not.
            self.dropFirstGridType = None
            
            # Indicates if we actually did drop the first grid. Sometimes we will
            # determine that we need to drop the grid if it exists but it doesn't
            # end up existing so we don't actually drop anything in some cases.
            self.droppedFirstGrid = False
            
            # Indicate the period (actually a 0-based index into a list of periods)
            # that contains the first correct grid.
            self.periodWithFirstCorrectGrid = None
            
            # The AbsTime of when the grids first met or exceeded the threshold.
            self.onsetTime = None
            
            # The AbsTime of when the grids last met or exceeded the threshold.
            self.endTime = None
    
    # Start and end hour information from the Wind grids.
    class WindStats():
        def __init__(self):
            # The number of hours since issuance time when the wind first gets >= 34/64 knots.
            self.onsetHour = None
            # The number of hours since issuance time when the wind is last >= 34/64 knots.
            self.endHour = None
    
    # Information needed for creating the wind window text.
    class WindowInfo():
        def __init__(self, eventType):
            # The type (as a string) of the event this window is for (Tropical Storm or Hurricane).
            self.eventType = eventType
            # The number of hours since issuance time when the tropical storm or hurricane starts.
            self.onsetHour = None
            # The number of hours since issuance time when the tropical storm or hurricane ends.
            self.endHour = None
            # The resolution to use when determining the wording for the end time of the window.
            self.endTimeResolution = None
            # Determines if we should create window text for this event (did wind exceed threshold?)
            self.shouldCreateWindowText = True
            # The constructed window text.
            self.windowText = None
      
    def _setStats(self, statList, timeRangeList):
        pws34intStats = self.PwsXXintStats()
        pws64intStats = self.PwsXXintStats()
        pwsT34Stats = self.PwsTXXStats()
        pwsT64Stats = self.PwsTXXStats()
        wind34timeInfo = self.WindStats()
        wind64timeInfo = self.WindStats()
        prob34Onset = None
        
        for index in range(len(statList)):
            tr, _ = timeRangeList[index]
            statDict = statList[index]
            
            self._textProduct.debug_print("="*90, 1)
            self._textProduct.debug_print("\n\ntr = %s" % (tr), 1)
            
            self._textProduct.debug_print("*"*90, 1)
            currentPeriod = self._determineCurrentPeriod(tr)

            self._textProduct.debug_print("*"*90, 1)
            self._updateStatsForPwsXXint(tr, statDict, "pws34int", pws34intStats)
            self._textProduct.debug_print("-"*45, 1)
            self._updateStatsForPwsXXint(tr, statDict, "pws64int", pws64intStats)
            
            self._textProduct.debug_print("*"*90, 1)
            self._updateStatsForPwsTXX(tr, statDict, "pwsD34", "pwsN34", pwsT34Stats, currentPeriod)
            self._textProduct.debug_print("-"*45, 1)
            self._updateStatsForPwsTXX(tr, statDict, "pwsD64", "pwsN64", pwsT64Stats, currentPeriod)
            
            # Calculate an additional probabilistic onset hour for scenarios where we weren't
            # able to calculate the onset the usual way. This is only done for tropical
            # storms to help determine the correct TR (check plans, etc.)
            if prob34Onset is None and pwsT34Stats.onsetTime is not None:
                self._textProduct.debug_print("*"*90, 1)
                self._textProduct.debug_print("Found pwsD/N34 onset time, calculating prob34Onset", 1)
                prob34Onset = self._calculateProbOnset(timeRangeList, statList, index, "pws34int")
            
            self._textProduct.debug_print("*"*90, 1)
            self._updateStatsForWind(tr, statDict, wind34timeInfo, speed=34)
            self._textProduct.debug_print("-"*45, 1)
            self._updateStatsForWind(tr, statDict, wind64timeInfo, speed=64)
            
            self._textProduct.debug_print("*"*90, 1)
            self._updateMaxWindGust(statDict)
            
            self._textProduct.debug_print("*"*90, 1)
            self._updateThreatStats(tr, statDict, "WindThreat")
         
        self._textProduct.debug_print("="*90, 1)
        
        #Tropical Storm
        self._textProduct.debug_print("Tropical Storm Window:", 1)
        tropicalStormWindow = self.WindowInfo("Tropical Storm")
        tropicalStormWindow = self._computeWindOnsetAndEnd(tropicalStormWindow,
                                                           wind34timeInfo,
                                                           pws34intStats,
                                                           pwsT34Stats,
                                                           prob34Onset)
        tropicalStormWindow = self._createWindowText(tropicalStormWindow)
        # The tropical storm onset and end hours will be used for calculating threat statements
        self._onset34Hour = tropicalStormWindow.onsetHour
        self._end34Hour = tropicalStormWindow.endHour
        self._windowTS = tropicalStormWindow.windowText
         
        #Hurricane
        self._textProduct.debug_print("-"*45, 1)
        self._textProduct.debug_print("Hurricane Window:", 1)
        hurricaneWindow = self.WindowInfo("Hurricane")
        hurricaneWindow = self._computeWindOnsetAndEnd(hurricaneWindow,
                                                       wind64timeInfo,
                                                       pws64intStats,
                                                       pwsT64Stats)
        
        # Make sure the hurricane window end time resolution is the same
        # resolution used for tropical storms so that hurricanes don't appear
        # to end after tropical storms
        hurricaneWindow.endTimeResolution = tropicalStormWindow.endTimeResolution
        
        hurricaneWindow = self._createWindowText(hurricaneWindow)
        self._windowHU = hurricaneWindow.windowText
        
        self._textProduct.debug_print("-"*45, 1) 
        self._currentAdvisory["WindThreat"] = self._maxThreat
        self._currentAdvisory["WindForecast"] = self._maxWind
 
        self._textProduct.debug_print("+"*60, 1)
        self._textProduct.debug_print("In WindSectionStats._setStats", 1)
        self._textProduct.debug_print("pws34intStats.max = %s" % (pws34intStats.max), 1)
        self._textProduct.debug_print("pws64intStats.max = %s" % (pws64intStats.max), 1)
        self._textProduct.debug_print("pwsT34Stats.periodWithFirstCorrectGrid = %s" % (pwsT34Stats.periodWithFirstCorrectGrid), 1)
        self._textProduct.debug_print("pwsT34Stats.endTime = '%s'" % (pwsT34Stats.endTime), 1)
        self._textProduct.debug_print("pwsT64Stats.periodWithFirstCorrectGrid = %s" % (pwsT64Stats.periodWithFirstCorrectGrid), 1)
        self._textProduct.debug_print("pwsT64Stats.endTime = '%s'" % (pwsT64Stats.endTime), 1)
        self._textProduct.debug_print("self._maxWind = %s" % (self._maxWind), 1)
        self._textProduct.debug_print("self._maxGust = %s" % (self._maxGust), 1)
        self._textProduct.debug_print("self._maxThreat = %s" % (self._maxThreat), 1)
    
    def _determineCurrentPeriod(self, tr):
        currentPeriod = None
        for periodIndex, periodTr in enumerate(self._textProduct._periodList):
            self._textProduct.debug_print("\n\nperiodIndex = %d    periodList tr = %s"
                                          % (periodIndex, repr(periodTr)), 1)
            
            if (periodIndex == 0) and (tr.startTime().unixTime() < periodTr.startTime().unixTime()):
                # If the tr is before the first period, use the first period
                currentPeriod = periodIndex
                break
            elif (periodIndex == len(self._textProduct._periodList) - 1) and \
                 (tr.startTime().unixTime() >= periodTr.endTime().unixTime()):
                # If the tr is after (or at the end of) the last period, use the last period
                currentPeriod = periodIndex
                break
            elif periodTr.contains(tr.startTime()):
                currentPeriod = periodIndex
                break
        
        self._textProduct.debug_print("\n\ncurrentPeriod index = %s" % (currentPeriod), 1)
        self._textProduct.debug_print("\n\ncurrentPeriod tr = %s"
                                      % (self._textProduct._periodList[currentPeriod]), 1)
        
        return currentPeriod
    
    def _updateStatsForPwsXXint(self, tr, statDict, gridName, pwsXXintStats):
        pwsXXint = self._textProduct._getStatValue(statDict, gridName, "Max")
        
        self._textProduct.debug_print("Wind Window Debug: pwsXXintStats gridName = %s" % (gridName), 1)
        self._textProduct.debug_print("Wind Window Debug: pwsXXintStats pwsXXint = %s" % (pwsXXint), 1)
        
        if pwsXXint is not None:
            if pwsXXintStats.max is None or pwsXXint > pwsXXintStats.max:
                pwsXXintStats.max = pwsXXint
                pwsXXintStats.onsetHour = self._calculateHourOffset(tr.startTime())
                
                self._textProduct.debug_print("Wind Window Debug: pwsXXintStats Found a new max value!", 1)
                self._textProduct.debug_print("Wind Window Debug: pwsXXintStats onsetHour = %s" % (pwsXXintStats.onsetHour), 1)
    
    def _updateStatsForPwsTXX(self, tr, statDict, dayGridName, nightGridName, pwsTXXStats, period):
        if pwsTXXStats.firstRun:
            self._textProduct.debug_print("first run for _updateStatsForPwsTXX!", 1)
            self._textProduct.debug_print("grids: %s %s" % (dayGridName, nightGridName), 1)
            pwsTXXStats.firstRun = False
            localtime = time.localtime(self._textProduct._issueTime_secs)
            self._textProduct.debug_print("localtime = %s" % (localtime), 1)

            if localtime.tm_hour >= 15: # 3PM to midnight
                self._textProduct.debug_print("between 3PM and midnight!", 1)
                pwsTXXStats.dropFirstGridType = "day"
                self._textProduct.debug_print("need to drop the day grid(s) if they come first", 1)
            elif localtime.tm_hour >= 3 and localtime.tm_hour < 12: # 3AM to noon
                self._textProduct.debug_print("between 3AM and noon!", 1)
                pwsTXXStats.dropFirstGridType = "night"
                self._textProduct.debug_print("need to drop the night grid(s) if they come first", 1)
            else:
                self._textProduct.debug_print("not dropping any grids!", 1)


        pwsDXX = self._textProduct._getStatValue(statDict, dayGridName, "Max")
        pwsNXX = self._textProduct._getStatValue(statDict, nightGridName, "Max")
        
        maxPws = None
        self._textProduct.debug_print("%s pwsDXX = %s    pwsNXX = %s " % 
                         (self._textProduct._pp.pformat(tr),pwsDXX, pwsNXX), 1)

        #  Determine coversion factor to get DAY and NIGHT in UTC
        utcHourOffset = self._calculateUTCandLocalHourOffset()

        #  See if this hour a valid DAYtime hour
        isValidDay = self._isValidDayTime(tr.startTime().hour,
                                          self._textProduct.DAY() + utcHourOffset,
                                          self._textProduct.NIGHT() + utcHourOffset)

        #  If we have pwsD data, and this is a time period it applies to
        if pwsDXX is not None and isValidDay:
            self._textProduct.debug_print("Wind Window Debug: pwsTXXStats DAY", 1)
            
            if pwsTXXStats.dropFirstGridType == "day":
                self._textProduct.debug_print("Wind Window Debug: dropping a day grid", 1)
                self._textProduct.debug_print("Wind Window Debug: tr = %s, period = %s" % (tr, period), 1)
                pwsTXXStats.droppedFirstGrid = True
                return
            elif pwsTXXStats.dropFirstGridType == "night":
                # We dropped all the necessary grids now that we found a day grid so stop dropping
                pwsTXXStats.dropFirstGridType = None
                pwsTXXStats.periodWithFirstCorrectGrid = period
                self._textProduct.debug_print("Wind Window Debug: found day grid; done dropping night grids", 1)
                self._textProduct.debug_print("Wind Window Debug: tr = %s, period = %s" % (tr, period), 1)
            
            maxPws = pwsDXX

        #  If we have pwsN data, and this is a time period it applies to
        elif pwsNXX is not None and not isValidDay:
            self._textProduct.debug_print("Wind Window Debug: pwsTXXStats NIGHT", 1)
           
            if pwsTXXStats.dropFirstGridType == "night":
                self._textProduct.debug_print("Wind Window Debug: dropping a night grid", 1)
                self._textProduct.debug_print("Wind Window Debug: tr = %s, period = %s" % (tr, period), 1)
                pwsTXXStats.droppedFirstGrid = True
                return
            elif pwsTXXStats.dropFirstGridType == "day":
                # We dropped all the necessary grids now that we found a night grid so stop dropping
                pwsTXXStats.dropFirstGridType = None
                pwsTXXStats.periodWithFirstCorrectGrid = period
                self._textProduct.debug_print("Wind Window Debug: found night grid; done dropping day grids", 1)
                self._textProduct.debug_print("Wind Window Debug: tr = %s, period = %s" % (tr, period), 1)

            maxPws = pwsNXX

        #  These two statements will need to be reevaluated when this product is 
        #  expanded to the Pacific basin (MHB - 02/03/2015)
        elif pwsDXX is not None and tr.startTime().hour in [21, 0, 3]:
            self._textProduct.debug_print("Wind Window Debug: pwsTXXStats DAY ignored", 1)

        elif pwsNXX is not None and tr.startTime().hour in [9, 12, 15]:
            self._textProduct.debug_print("Wind Window Debug: pwsTXXStats NIGHT ignored", 1)
        
        threshold34index = 0
        threshold64index = 1
        if maxPws is not None:
            # Don't shift if the period with the first correct grid is period 0
            if pwsTXXStats.droppedFirstGrid and pwsTXXStats.periodWithFirstCorrectGrid != 0:
                period = period - 1 # We dropped the first grid so we are off-by-one
                self._textProduct.debug_print("shifting period back 1...new period = %s" % 
                                 (period), 1)
                
            #  Just set the first correct period to period zero, if it hasn't
            #  been set yet, so the missing grid check will not fail
            if pwsTXXStats.periodWithFirstCorrectGrid is None:
                pwsTXXStats.periodWithFirstCorrectGrid = 0

            if "64" in dayGridName:
                index = threshold64index
            else: #if "34"
                index = threshold34index
            
            threshold = None
            thresholds = self._textProduct.windSpdProb_thresholds(threshold, threshold)
            if period == 0:
                (thresholdLow, thresholdHigh) = thresholds[period][index]
                threshold = thresholdLow
            else:
                threshold = thresholds[period][index]
            self._textProduct.debug_print("Probability threshold for period %s = %s"
                                          % (period, threshold), 1)
                
            if maxPws > threshold:
                if pwsTXXStats.onsetTime is None:
                    pwsTXXStats.onsetTime = tr.startTime()
                
                trEndTime = tr.endTime()
                periodEndTime = self._textProduct._periodList[period].endTime()
                
                # Don't go past the end of the period
                if trEndTime <= periodEndTime:
                    pwsTXXStats.endTime = trEndTime
                else:
                    pwsTXXStats.endTime = periodEndTime
                
                self._textProduct.debug_print("Wind Window Debug: probability threshold = %s (period index %s)" % (threshold, period), 1)
                self._textProduct.debug_print("Wind Window Debug: pwsTXXStats dayGridName = %s" % (dayGridName), 1)
                self._textProduct.debug_print("Wind Window Debug: pwsTXXStats nightGridName = %s" % (nightGridName), 1)
                self._textProduct.debug_print("Wind Window Debug: pwsTXXStats original tr = %s" % (self._textProduct._pp.pformat(tr)), 1)
                self._textProduct.debug_print("Wind Window Debug: pwsTXXStats maxPws = %s" %(self._textProduct._pp.pformat(maxPws)), 1)
                self._textProduct.debug_print("Wind Window Debug: pwsTXXStats onsetTime = %s" % (self._textProduct._pp.pformat(pwsTXXStats.onsetTime)), 1)
                self._textProduct.debug_print("Wind Window Debug: pwsTXXStats endTime = %s" % (self._textProduct._pp.pformat(pwsTXXStats.endTime)), 1)
                self._textProduct.debug_print("Wind Window Debug: period tr = %s" % (self._textProduct._pp.pformat(self._textProduct._periodList[period])), 1)
    
    def _updateStatsForWind(self, tr, statDict, timeInfo, speed):
        self._textProduct.debug_print("Wind Window Debug: In _updateStatsForWind", 1)
        self._textProduct.debug_print("Wind Window Debug: timeInfo tr = %s" % (self._textProduct._pp.pformat(tr)), 1)
        self._textProduct.debug_print("Wind Window Debug: timeInfo speed threshold = %s" % (speed), 1)
        self._textProduct.debug_print("Wind Window Debug: timeInfo maxWind = %s" % (self._maxWind), 1)
        
        wind = self._textProduct._getStatValue(statDict, "Wind", "Max", self._textProduct.VECTOR())
        self._textProduct.debug_print("Wind Window Debug: current wind value = %s" % (wind), 1)
        
        if wind is not None:
            if self._maxWind is None or wind > self._maxWind:
                self._textProduct.debug_print("Wind Window Debug: Found new max wind value!", 1)
                self._maxWind = wind
            
            if wind >= speed:
                self._textProduct.debug_print("Wind Window Debug: current wind >= speed!", 1)
                
                if timeInfo.onsetHour is None:
                    timeInfo.onsetHour = self._calculateHourOffset(tr.startTime())
                    
                    self._textProduct.debug_print("Wind Window Debug: onsetHour was None", 1)
                    self._textProduct.debug_print("Wind Window Debug: timeInfo onsetHour = %s" % (timeInfo.onsetHour), 1)
                
                # Always update the end time (it's the last time we exceeded the speed)
                timeInfo.endHour = self._calculateHourOffset(tr.endTime())
                self._textProduct.debug_print("Wind Window Debug: timeInfo endHour = %s" % (timeInfo.endHour), 1)
    
    def _updateMaxWindGust(self, statDict):
        windGust = self._textProduct._getStatValue(statDict, "WindGust", "Max")
        self._textProduct.debug_print("Wind Window Debug: current windGust value = %s" % (windGust), 1)
        
        if windGust is not None:
            if self._maxGust is None or windGust > self._maxGust:
                self._textProduct.debug_print("Wind Window Debug: Found new max windGust value!", 1)
                self._maxGust = windGust
    
    def _calculateProbOnset(self, timeRangeList, statList, index, pwsXXintGridName):
        self._textProduct.debug_print("Wind Window Debug: in _calculateProbOnset", 1)
        
        # Calculate corresponding maximum intersecting pwsXXint tr
        maxPwsXXintTr = self._calculateMaxPwsXXintTr(timeRangeList, statList, index,
                                                     pwsXXintGridName)
        self._textProduct.debug_print("Wind Window Debug: maxPwsXXintTr = %s" % (maxPwsXXintTr), 1)
        
        # Calculate hours since issuance time to start time
        probOnset = self._calculateHourOffset(maxPwsXXintTr.startTime())
        self._textProduct.debug_print("Wind Window Debug: probOnset = %s" % (probOnset), 1)
        
        return probOnset
    
    def _calculateMaxPwsXXintTr(self, timeRangeList, statList, index, gridName):
        self._textProduct.debug_print("Wind Window Debug: gridName = %s" % (gridName), 1)
        
        # The current tr is always the first pwsXXint grid that intersects the onset pwsTXX grid
        currTr, _ = timeRangeList[index]
        currStatDict = statList[index]
        currPwsXXint = self._textProduct._getStatValue(currStatDict, gridName, "Max")
        self._textProduct.debug_print("Wind Window Debug: currTr = %s" % (currTr), 1)
        self._textProduct.debug_print("Wind Window Debug: currPwsXXint = %s" % (currPwsXXint), 1)
        
        # Now try to find the next intersecting pwsXXint grid.
        # pwsXXint grids are 6-hours long with times: 00-06, 06-12, 12-18, 18-00 GMT
        if 0 <= currTr.startTime().hour < 6:
            nextTrStartHour = 6
        elif 6 <= currTr.startTime().hour < 12:
            nextTrStartHour = 12
        elif 12 <= currTr.startTime().hour < 18:
            nextTrStartHour = 18
        else:
            nextTrStartHour = 0
        
        nextTr = None
        nextPwsXXint = None
        
        for nextIndex in range(index + 1, len(statList)):
            nextTr, _ = timeRangeList[nextIndex]
            if nextTr.startTime().hour != nextTrStartHour:
                continue
            
            nextStatDict = statList[nextIndex]
            nextPwsXXint = self._textProduct._getStatValue(nextStatDict, gridName, "Max")
            self._textProduct.debug_print("Wind Window Debug: nextTr = %s" % (nextTr), 1)
            self._textProduct.debug_print("Wind Window Debug: nextPwsXXint = %s" % (nextPwsXXint), 1)
        
        if (nextPwsXXint is None) or (currPwsXXint >= nextPwsXXint):
            return currTr
        else:
            return nextTr
    
    def _computeWindOnsetAndEnd(self, windowInfo, windTimeInfo, pwsXXintStats, pwsTXXStats, probOnset=None):
        self._textProduct.debug_print("Wind Window Debug: In _computeWindOnsetAndEnd", 1)
        self._textProduct.debug_print("Wind Window Debug: windTimeInfo.onsetHour = %s" % (windTimeInfo.onsetHour), 1)
        self._textProduct.debug_print("Wind Window Debug: pwsXXintStats.onsetHour = %s" % (pwsXXintStats.onsetHour), 1)
        self._textProduct.debug_print("Wind Window Debug: windTimeInfo.endHour = %s" % (windTimeInfo.endHour), 1)
        self._textProduct.debug_print("Wind Window Debug: pwsTXXStats.endTime = %s" % (pwsTXXStats.endTime), 1)
        if pwsTXXStats.endTime is not None:
            self._textProduct.debug_print("Wind Window Debug: pwsTXXStats end hour = %s" % (self._calculateHourOffset(pwsTXXStats.endTime)), 1)
        self._textProduct.debug_print("Wind Window Debug: probOnset = %s" % (probOnset), 1)
        
        if windTimeInfo.onsetHour is not None:
            if windTimeInfo.onsetHour < 6:
                self._textProduct.debug_print("onsetHour for wind is < 6, using that as window onset hour", 1)
                windowInfo.onsetHour = windTimeInfo.onsetHour
                self._textProduct.debug_print("onsetHour = %s" % (windowInfo.onsetHour), 1)
            elif pwsXXintStats.onsetHour is not None:
                self._textProduct.debug_print("onsetHour for pwsXXintStats is not None", 1)
                self._textProduct.debug_print("using min onset hour betweeen wind and pwsXXintStats", 1)
                windowInfo.onsetHour = min(windTimeInfo.onsetHour, pwsXXintStats.onsetHour)
                self._textProduct.debug_print("onsetHour = %s" % (windowInfo.onsetHour), 1)
            else:
                self._textProduct.debug_print("ERROR: onsetHour for pwsXXintStats is None. Check the grids.", 1)
                return windowInfo
        else:
            self._textProduct.debug_print("windTimeInfo.onsetHour was None, using probOnset (%s) instead"
                                          % probOnset, 1)
            windowInfo.onsetHour = probOnset
            self._textProduct.debug_print("onsetHour = %s" % (windowInfo.onsetHour), 1)

            self._textProduct.debug_print("Since wind threshold not exceeded, will not create window text", 1)
            windowInfo.shouldCreateWindowText = False
        
        if windowInfo.onsetHour is None:
            # We won't have a timing window
            self._textProduct.debug_print("onsetHour for wind is None", 1)
            return windowInfo
        
        windEndHourExists = windTimeInfo.endHour is not None
        windEndHourOutOfRange = windTimeInfo.endHour > 114 or windTimeInfo.endHour < 6
        pwsTXXEndTimeExists = pwsTXXStats.endTime is not None
        
        if (not windEndHourExists or windEndHourOutOfRange) or \
           (windEndHourExists and not pwsTXXEndTimeExists):
            self._textProduct.debug_print("using Wind end hour for the window wind hour", 1)
            self._textProduct.debug_print("\twind end hour exists? %s" % windEndHourExists, 1)
            self._textProduct.debug_print("\twind end hour out of range? %s" % windEndHourOutOfRange, 1)
            self._textProduct.debug_print("\tpwsTXX end time exists? %s" % pwsTXXEndTimeExists, 1)
            windowInfo.endHour = windTimeInfo.endHour
            self._textProduct.debug_print("endHour = %s" % (windowInfo.endHour), 1)
        elif pwsTXXEndTimeExists:
            self._textProduct.debug_print("endTime for pwsTXXStats is not None", 1)
            self._textProduct.debug_print("converting endTime to a configured time", 1)
            configuredTime = self._getConfiguredTime(pwsTXXStats.endTime)

            probEndHour = self._calculateHourOffset(configuredTime)
            
            self._textProduct.debug_print("using rounded average betweeen wind end hour and configured pwsTXXStats end hour", 1)
            windowInfo.endHour = int(round(self._textProduct.average(windTimeInfo.endHour, probEndHour)))
            self._textProduct.debug_print("endHour = %s" % (windowInfo.endHour), 1)
        
        return windowInfo
    
    def _createWindowText(self, windowInfo):
        windowInfo.windowText = "Window for " + windowInfo.eventType + " force winds: "
        self._textProduct.debug_print("In _createWindowText", 1)
        self._textProduct.debug_print("window stats:", 1)
        self._textProduct.debug_print("onsetHour = %s" % (windowInfo.onsetHour), 1)
        self._textProduct.debug_print("endHour = %s" % (windowInfo.endHour), 1)
        self._textProduct.debug_print("endTimeResolution = %s" % (windowInfo.endTimeResolution), 1)
        self._textProduct.debug_print("shouldCreateWindowText = %s" % (windowInfo.shouldCreateWindowText), 1)
        
        if windowInfo.onsetHour is None or not windowInfo.shouldCreateWindowText:
            #  We do not want a statement of a non-existent window
            windowInfo.windowText = None
        else:
            startTime = AbsTime(self._textProduct._issueTime_secs + windowInfo.onsetHour*60*60)
            if windowInfo.endHour is not None:
                endTime = AbsTime(self._textProduct._issueTime_secs + windowInfo.endHour*60*60)
                windowPeriod = self._textProduct.makeTimeRange(startTime, endTime)
            else:
                windowPeriod = self._textProduct.makeTimeRange(startTime, startTime + 1)
            self._textProduct.debug_print("window period = %s" % (windowPeriod), 1)
            
            startTimeDescriptor = ""
            if windowInfo.onsetHour >= 18:
                startTimeDescriptor = self._textProduct._formatPeriod(windowPeriod, resolution = 6)
            elif 6 <= windowInfo.onsetHour < 18:
                startTimeDescriptor = self._textProduct._formatPeriod(windowPeriod, resolution = 3)
            
            if len(startTimeDescriptor) == 0 and windowInfo.endHour is None:
                windowInfo.windowText = None
            elif len(startTimeDescriptor) != 0 and windowInfo.endHour > 114:
                windowInfo.windowText += "Begins " + startTimeDescriptor
            else:
                connector = "through "
                endTimeDescriptor = "the next few hours"
                
                if windowInfo.endHour is not None:
                    if windowInfo.endTimeResolution is None:
                        if windowInfo.endHour >= 18:
                            windowInfo.endTimeResolution = 6
                        elif 6 <= windowInfo.endHour < 18:
                            windowInfo.endTimeResolution = 3
                    
                    if windowInfo.endTimeResolution is not None:
                        endTimeDescriptor = \
                            self._textProduct._formatPeriod(windowPeriod,
                                                            useEndTime = True,
                                                            resolution = windowInfo.endTimeResolution)
                
                #  If we are not talking about the next few hours
                if endTimeDescriptor != "the next few hours":
                    connector = "until "

                if len(startTimeDescriptor) != 0:
                    connector = " " + connector
                windowInfo.windowText += startTimeDescriptor + connector + endTimeDescriptor
                     
        return windowInfo
    
    def _getConfiguredTime(self, originalTime):
        self._textProduct.debug_print("original time = %s" % 
                                      (self._textProduct._pp.pformat(originalTime)), 1)
        
        unixTime = originalTime.unixTime()
        localTime = time.localtime(unixTime)
        self._textProduct.debug_print("original time in local time is %s" % 
                                      (self._textProduct._pp.pformat(localTime)), 1)
        utcHourOffset = self._calculateUTCandLocalHourOffset()
        self._textProduct.debug_print("utcHourOffset = %s" % (utcHourOffset), 1)
         
        #  Remember these times are in local time zone, so hour 0 is 
        #  midnight of the current calendar day.
        if localTime.tm_hour > 6 and localTime.tm_hour <= 18:
            #  It's daytime, so use the end of the daytime period (18 = 6PM).
            #  NIGHT returns the start of the nighttime period which is the
            #  end of the daytime period.
            configuredTime = absTimeYMD(originalTime.year,
                                        originalTime.month,
                                        originalTime.day,
                                        self._textProduct.NIGHT())
        else:
            #  It's nighttime, so use the end of the nighttime period (6 = 6AM).
            #  DAY returns the start of the daytime period which is the end of
            #  the nighttime period.
            configuredTime = absTimeYMD(originalTime.year,
                                        originalTime.month,
                                        originalTime.day,
                                        self._textProduct.DAY())
        self._textProduct.debug_print("configuredTime (local time) = %s" % 
                                      (self._textProduct._pp.pformat(configuredTime)), 1)
        
        # The configured time is local time so we need to add an offset to make the entire date UTC
        configuredUnixTime = configuredTime.unixTime() + (utcHourOffset * 3600)
        configuredTime = AbsTime(configuredUnixTime)
        self._textProduct.debug_print("configuredTime (UTC time) = %s" % 
                                      (self._textProduct._pp.pformat(configuredTime)), 1)
        
        return configuredTime
    
    def _calculateUTCandLocalHourOffset(self):
        if time.daylight:
            # This is daylight savings time so it needs to be handled differently
            return int(time.altzone // 3600)
        else:
            utc = time.gmtime()
            local = time.localtime()
            
            diffInSeconds = time.mktime(utc) - time.mktime(local)
            return int(diffInSeconds // 3600)

    def _isValidDayTime(self, trStartHour, utcDay, utcNight):

        #  Handle case where "night" starts at an "earlier" UTC hour than "day"
        #  (e.g. DAY = 18Z and NIGHT = 06Z)
        if (utcNight < utcDay) and \
           (trStartHour >= utcDay or trStartHour < utcNight):

            #  If we are toward the end of the daytime, and more than 1 hour
            #  from its end
            if (trStartHour < utcNight) and (utcNight - trStartHour) > 1:
                return True
            elif trStartHour >= utcDay:
                return True

        #  Handle "normal" case where "day" starts before "night" in UTC
        elif trStartHour >= utcDay and trStartHour < utcNight and \
             (utcNight - trStartHour) > 1:
            return True

        #  If we made it this far, this is not a valid "day" hour
        return False


class StormSurgeSectionStats(SectionCommonStats):
    def __init__(self, textProduct, segment, intersectStatList, timeRangeList):
        SectionCommonStats.__init__(self, textProduct, segment)
        self._inundationMax = None
        self._onsetSurgeHour = None
        self._endSurgeHour = None
        self._windowSurge = None
        
        # Only gather stats if we are populating the surge section
        if self._textProduct._PopulateSurge:
            self._setStats(intersectStatList, timeRangeList)
    
    def _setStats(self, statList, timeRangeList):
        windows = []
        phishStartTime = None
        phishEndTime = None
        
        #  If this is an inland area, just move on
        if statList == "InlandArea":
            return
        
        self._textProduct.debug_print("*"*100, 1)
        self._textProduct.debug_print("Setting Surge Section stats for %s" % self._segment, 1)
        
        statDict = statList[0]        
        self._textProduct.debug_print("StatDict %s" % statDict, 1)

        self._inundationMax = self._textProduct._getStatValue(statDict, "InundationMax", "Max")
        self._textProduct.debug_print("Raw self._inundationMax = %s" % (repr(self._inundationMax)), 1)
                                      
        if self._inundationMax is not None:
            self._inundationMax = round(self._inundationMax)
        self._textProduct.debug_print("self._inundationMax = %s" % (self._inundationMax), 1)
        
        self._textProduct.debug_print("length of statList = %s" % (len(statList)), 1)
        for period in range(len(statList)):
            tr, _ = timeRangeList[period]
            statDict = statList[period]
            self._textProduct.debug_print("-"*50, 1)
            self._textProduct.debug_print("tr = %s" % (self._textProduct._pp.pformat(tr)), 1)
            self._textProduct.debug_print("statDict = %s" % (self._textProduct._pp.pformat(statDict)), 1)
            
            
            curPhish = self._textProduct._getStatValue(statDict, "InundationTiming", "Max")
            self._textProduct.debug_print("curPhish = '%s'" % (str(curPhish)), 1)
            self._textProduct.debug_print("phishStartTime = %s   phishEndTime = %s" % 
                                          (str(phishStartTime), str(phishEndTime)), 1)
            
            if (curPhish is None) or (curPhish == 'None'):
                self._textProduct.debug_print("Done: Reached end of grids (curPhish was None)", 1)
                break
            

            # For start time: 
            #     If inundationMax > 3:
            #             Looking for 2 consecutive grids with a surge height > 1
            #             Start will be the start time of the FIRST of the 2 consecutive grids
            #     If 1 < inundationMax <= 3:
            #             Looking for 1 grid with a surge height > 1
            #             Start will be the start time of this grid
            #
            # For end time:
            #     Looking for 2 consecutive grids with a surge height <= 1
            #     End will be the start time of the FIRST of the 2 consecutive grids
            
            # If we have another period after this one, we may need to look at the two
            # consecutive periods for start and end time conditions
            isLastPeriod = True
            if period < len(statList) - 1:
                isLastPeriod = False
                nextTr, _ = timeRangeList[period+1]
                nextStatDict = statList[period+1]
                nextPhish = self._textProduct._getStatValue(nextStatDict, "InundationTiming", "Max")
                
                self._textProduct.debug_print("nextTr = %s" % (self._textProduct._pp.pformat(nextTr)), 1)
                self._textProduct.debug_print("nextStatDict = %s" % (self._textProduct._pp.pformat(nextStatDict)), 1)
                self._textProduct.debug_print("nextPhish = '%s'" % (str(nextPhish)), 1)
            
            # Set what the condition is for determining the start time
            if (self._inundationMax > 3) and (not isLastPeriod):
                startCondition = (curPhish > 1) and (nextPhish > 1)
                self._textProduct.debug_print("startCondition looking at 2 periods", 1)
            elif 1 < self._inundationMax <= 3:
                startCondition = curPhish > 1
                self._textProduct.debug_print("startCondition looking at 1 period", 1)
            else:
                startCondition = False
                self._textProduct.debug_print("no startCondition, done", 1)
                break
            
            # Set what the condition is for determining the end time
            if not isLastPeriod:
                endCondition = (curPhish <= 1) and (nextPhish <= 1)
                self._textProduct.debug_print("endCondition looking at 2 periods", 1)
            else:
                endCondition = False
                self._textProduct.debug_print("this is the last period, no endCondition possible", 1)
            
            if startCondition and (phishStartTime is None):
                phishStartTime = tr.startTime()
            elif endCondition and (phishStartTime is not None) and (phishEndTime is None):
                phishEndTime = tr.startTime()
                
                # We found a new window, save it, reset and look for any additional windows
                self._textProduct.debug_print("Found a new window:", 1)
                self._textProduct.debug_print("window phishStartTime = %s   window phishEndTime = %s" % 
                                              (str(phishStartTime), str(phishEndTime)), 1)
                
                windows.append((phishStartTime, phishEndTime))
                phishStartTime = None
                phishEndTime = None
                
                self._textProduct.debug_print("Looking for additional windows", 1)
            
            self._textProduct.debug_print("new phishStartTime = %s   new phishEndTime = %s" % 
                                          (str(phishStartTime), str(phishEndTime)), 1)
        
        # Check for the case where a window doesn't end
        if (phishStartTime is not None) and (phishEndTime is None):
            self._textProduct.debug_print("Found a never-ending window:", 1)
            self._textProduct.debug_print("window phishStartTime = %s   window phishEndTime = %s" % 
                                          (str(phishStartTime), str(phishEndTime)), 1)
            windows.append((phishStartTime, None))
        
        # Create the final window
        if len(windows) == 0:
            phishStartTime = None
            phishEndTime = None
        else:
            phishStartTime = windows[0][0] # Start time of first window
            phishEndTime = windows[-1][1] # End time of last window
        
        self._textProduct.debug_print("Constructed the final window:", 1)
        self._textProduct.debug_print("final phishStartTime = %s   final phishEndTime = %s" % 
                                      (str(phishStartTime), str(phishEndTime)), 1)
        
        self._windowSurge = "Window of concern: "
        
        if phishStartTime is None:
            if self._inundationMax is None or self._inundationMax <= 1:
                self._windowSurge += "None"
            else:
                self._windowSurge += "Around high tide"
        else:
            self._onsetSurgeHour = self._calculateHourOffset(phishStartTime)
            startTime = AbsTime(self._textProduct._issueTime_secs + self._onsetSurgeHour*60*60)
            
            self._textProduct.debug_print("surge startTime = %s   self._onsetSurgeHour = %s " %
                                          (self._textProduct._pp.pformat(startTime), self._onsetSurgeHour), 1)
            if phishEndTime is not None:
                self._endSurgeHour = self._calculateHourOffset(phishEndTime)
                endTime = AbsTime(self._textProduct._issueTime_secs + self._endSurgeHour*60*60)
                windowPeriod = self._textProduct.makeTimeRange(startTime, endTime)
            else:
                windowPeriod = self._textProduct.makeTimeRange(startTime, startTime + 1)
            self._textProduct.debug_print("surge window period = %s" % (windowPeriod), 1)
            
            startTimeDescriptor = self._textProduct._formatPeriod(windowPeriod)

            if phishEndTime is None:
                self._windowSurge += "Begins " + startTimeDescriptor
            else:
                endTimeDescriptor = self._textProduct._formatPeriod(windowPeriod, useEndTime = True)
            
                if self._onsetSurgeHour > 12:
                    self._windowSurge += startTimeDescriptor +\
                                         " until " +\
                                         endTimeDescriptor
                else:
                    self._windowSurge += "through " + endTimeDescriptor
        
        if self._inundationMax is not None:
            # inundationMax is already rounded but should be stored as an int and not a float
            self._currentAdvisory["StormSurgeForecast"] = int(self._inundationMax)

        self._textProduct.debug_print("+"*60, 1)
        self._textProduct.debug_print("Done in StormSurgeSectionStats._setStats:", 1)
        self._textProduct.debug_print("self._inundationMax = '%s'" % 
                                      (self._inundationMax), 1)
        self._textProduct.debug_print("self._onsetSurgeHour = '%s'" % 
                                      (self._onsetSurgeHour), 1)
        self._textProduct.debug_print("self._endSurgeHour = '%s'" % 
                                      (self._endSurgeHour), 1)
        self._textProduct.debug_print("self._windowSurge = '%s'" % 
                                      (self._windowSurge), 1)
        self._textProduct.debug_print("self._maxThreat = '%s'" % 
                                      (self._maxThreat), 1)
        self._textProduct.debug_print("+"*60, 1)


class FloodingRainSectionStats(SectionCommonStats):
    def __init__(self, textProduct, segment, statList, timeRangeList, 
                 extraRainfallStatList, previousRainfallTRlist):
        SectionCommonStats.__init__(self, textProduct, segment)
        self._sumAccum = None
        self._prevAccum = 0.00
        
        self._setStats(statList, timeRangeList, extraRainfallStatList, 
                       previousRainfallTRlist)
    
    def _setStats(self, statList, timeRangeList, extraRainfallStatList, 
                       previousRainfallTRlist):
        for period in range(len(statList)):
            tr, _ = timeRangeList[period]
            statDict = statList[period]
        
            value = self._textProduct._getStatValue(statDict, "QPF")
                     
            if value is not None:
                if self._sumAccum is None:
                    self._sumAccum = value
                else:
                    self._sumAccum += value
            
            self._updateThreatStats(tr, statDict, "FloodingRainThreat")
        
        self._currentAdvisory["FloodingRainThreat"] = self._maxThreat
        if self._sumAccum is not None:
            # Round so that we don't end up with stats like 4.03143835067749
            self._currentAdvisory["FloodingRainForecast"] = \
                    self._textProduct.round(self._sumAccum, "Nearest", 0.5)

        #  Now compute the previous rainfall
        for period in range(len(extraRainfallStatList)):
            tr, _ = timeRangeList[period]
            prevStatDict = extraRainfallStatList[period]
        
            prevStats = self._textProduct._getStatValue(prevStatDict, "QPF")
            self._textProduct.debug_print("prevStats = %s" % (prevStats), 1)
            if prevStats is not None:
                
                if self._prevAccum is not None:
                    self._prevAccum += prevStats
                else:
                    self._prevAccum = prevStats
            else:
                self._prevAccum = 0.00
                    
        if self._prevAccum is not None and self._prevAccum >= 0.10:
            # Round so that we don't end up with stats like 4.03143835067749
            self._currentAdvisory["PreviousRainfall"] = \
                    self._textProduct.round(self._prevAccum, "Nearest", 0.1)
        else:
            #  Otherwise, do not consider this sgnificant rainfall
            self._currentAdvisory["PreviousRainfall"] = 0.00

        self._textProduct.debug_print("+"*60, 1)
        self._textProduct.debug_print("In FloodingRainSectionStats._setStats", 1)
        self._textProduct.debug_print("self._sumAccum = '%s'" % (self._sumAccum), 1)
        self._textProduct.debug_print("self._maxThreat = '%s'" % (self._maxThreat), 1)


class TornadoSectionStats(SectionCommonStats):
    def __init__(self, textProduct, segment, statList, timeRangeList):
        SectionCommonStats.__init__(self, textProduct, segment)
        
        self._setStats(statList, timeRangeList)
    
    def _setStats(self, statList, timeRangeList):
        for period in range(len(statList)):
            tr, _ = timeRangeList[period]
            statDict = statList[period]
            
            self._updateThreatStats(tr, statDict, "TornadoThreat")
        
        self._currentAdvisory["TornadoThreat"] = self._maxThreat

        self._textProduct.debug_print("+"*60, 1)
        self._textProduct.debug_print("In TornadoSectionStats._setStats", 1)
        self._textProduct.debug_print("self._maxThreat = '%s'" % (self._maxThreat), 1)


from xml.etree.ElementTree import Element, SubElement, tostring, dump
import xml.dom.minidom as minidom
import re
class XMLFormatter():
    def __init__(self, textProduct):
        self._textProduct = textProduct
    
    def execute(self, productDict):
        xml = Element('product')
        self.dictionary(xml, productDict)
        self._textProduct.debug_print("XML = %s" % (xml), 1)
        self._textProduct.debug_print("XML dump = %s", dump(xml), 1)
        prettyXML = minidom.parseString(tostring(xml))
        return prettyXML.toprettyxml() #tostring(xml)
    
    def xmlKeys(self):
        return [
                'wmoHeader',
                    'TTAAii',
                    'originatingOffice',
                    'productID',
                    'siteID',
                    'fullStationID',
                    'ddhhmmTime',
                'easMessage',
                'productHeader',
                    'disclaimer',
                    'cityState',
                    'stormNumber',
                    'productName',
                    'stormName',
                    'advisoryType',
                    'advisoryNumber',
                    'issuedByString',
                    'issuanceTimeDate',
                    
                'segments',
                    'ugcHeader',
                    'vtecRecords',
                    'areaList',
                    'issuanceTimeDate',
                    'summaryHeadlines',
                        'headlinesInEffect',
                        'headlineDefinitions',
                    'locationsAffected',
                    'fcstConfidence',
                    #section keys will be inserted here (see sectionKeys)
                    'infoSection',
                
                'endProduct',
                ]
    
    def sectionKeys(self):
        return [
                'windSection',
                    'sectionHeader',
                    'forecastSubsection',
                        'latestForecastSummary',
                        'peakWind',
                        'windowTS',
                        'windowHU',
                    'threatSubsection',
                        'lifePropertyThreatSummary',
                        'threatTrend',
                        'threatStatements',
                    'impactsSubsection',
                        'potentialImpactsSummary',
                        'potentialImpactsStatements',
                        
                'stormSurgeSection',
                    'sectionHeader',
                    'forecastSubsection',
                        'latestForecastSummary',
                        'peakSurge',
                        'surgeWindow',
                    'threatSubsection',
                        'lifePropertyThreatSummary',
                        'threatTrend',
                        'threatStatements',
                    'impactsSubsection',
                        'potentialImpactsSummary',
                        'potentialImpactsStatements',
                        
                'floodingRainSection',
                    'sectionHeader',
                    'forecastSubsection',
                        'latestForecastSummary',
                        'peakRain',
                    'threatSubsection',
                        'lifePropertyThreatSummary',
                        'threatTrend',
                        'threatStatements',
                    'impactsSubsection',
                        'potentialImpactsSummary',
                        'potentialImpactsStatements',
                        
                'tornadoSection',
                    'sectionHeader',
                    'forecastSubsection',
                        'latestForecastSummary',
                        'tornadoSituation',
                    'threatSubsection',
                        'lifePropertyThreatSummary',
                        'threatStatements',
                    'impactsSubsection',
                        'potentialImpactsSummary',
                        'potentialImpactsStatements',
                ]
    
    def getSectionKey(self, key):
        sectionKey = re.sub("\['......'\]", "", key)
        
        if "._" in sectionKey:
            sectionKey = re.sub(".*\._", "", sectionKey)
        
        self._textProduct.debug_print("sectionKey = %s" % (sectionKey), 1)
        return sectionKey
    
    def dictionary(self, xml, productDict):
        '''
        Returns the dictionary in XML format.
        @param productDict: dictionary values
        @return: Returns the dictionary in XML format.
        '''   
        if productDict is not None:
            for key in productDict:
                value = productDict[key]
                editable = False
#                 if isinstance(key, KeyInfo):
#                     editable = key.isEditable()
#                     key = key.getName()
                
                if key not in self.xmlKeys():
                    sectionKey = self.getSectionKey(key)
                    if sectionKey not in self.sectionKeys():
                        self._textProduct.debug_print("skipping '%s' in XML" % (key), 1)
                        continue
                    else:
                        key = sectionKey
                if isinstance(value, dict):
                    subElement = SubElement(xml,key)
                    self.dictionary(subElement, value)
                elif isinstance(value, list):
                    if key == 'cityList':
                        subElement = SubElement(xml,'cityList')
                        if editable:
                            subElement.attrib['editable'] = 'true'
                        self.list(subElement, 'city', value)
#                     elif key == 'infoSection':
#                         subElement = SubElement(xml, key)
#                         legacyFormatter = LegacyFormatter(self._textProduct)
#                         legacyText = legacyFormatter.processInfoSection(value)
#                         legacyText = legacyText.encode('string-escape')
#                         subElement.text = legacyText
#                         if editable:
#                             subElement.attrib['editable'] = 'true'
                    else:
                        self.list(xml, key, value)
                else:
                    subElement = SubElement(xml,key)
                    subElement.text = value
                    if editable:
                        subElement.attrib['editable'] = 'true'
    
    def list(self, xml, key, data):
        '''
        Returns the list in XML format.
        @param data: list of values
        @return: Returns the list in XML format.
        '''
        editable = False
#         if isinstance(key, KeyInfo):
#             editable = key.isEditable()
#             key = key.getName()  
        if data is not None:
            if 'info' in key and 'Section' in key:
                subElement = SubElement(xml, key)
                self._textProduct.debug_print("info key = '%s'" % (key), 1)
                self._textProduct.debug_print("value = %s" % (data), 1)
                if isinstance(data, list):
                    subkey = 'info' + 'Sub' + key[4:]
                    for value in data:
                        self.list(subElement, subkey, value)
                else:
                    subElement.text = data
            else:
                for value in data:
    
                    subElement = SubElement(xml, key)
                    if editable:
                        subElement.attrib['editable'] = 'true'
                    
                    if isinstance(value, dict):
                        self.dictionary(subElement, value)
                    elif isinstance(value, list):
                        if key == 'cityList':
                            subElement = SubElement(xml,'cityList')
                            if editable:
                                subElement.attrib['editable'] = 'true'
                            self.list(subElement, 'city', value)
                        else:
                            self.list(xml, key, value)
                    else:          
                        subElement.text = value


class LegacyFormatter():
    def __init__(self, textProduct):
        self._textProduct = textProduct
        self.TAB = " "*self._textProduct._tabLength
        self._tpc = HLSTCV_Common.TextProductCommon()
    
    def execute(self, productDict):
        self.productDict = productDict
        productParts = self._tpc.getVal(productDict, 'productParts', [])
        text = self._processProductParts(productDict, productParts.get('partsList'))
        return text

    def _processProductParts(self, productDict, productParts, skipParts=[]):
        '''
        Adds the product parts to the product
        @param productDict -- dictionary of information -- could be the product dictionary or a sub-part such as a segment
        @param skipParts -- necessary to avoid repetition when calling this method recursively
        @param productParts -- list of instances of the ProductPart class with information about how to format each product part
        @return text -- product string
        '''
        text = ''
        self._textProduct.debug_print("productParts = %s" % (self._textProduct._pp.pformat(productParts)), 1)
        for part in productParts:             
            valtype = type(part)
            if valtype is str:
                name = part
            elif valtype is tuple:
                name = part[0]
                infoDicts = part[1]
                newtext = self.processSubParts(productDict.get(name), infoDicts)
                text += newtext
                continue
            elif valtype is list:
                self._tpc.flush()
                # TODO THIS SHOULD BE REMOVED AFTER THE REFACTOR OF HazardServicesProductGenerationHandler.JAVA
                tup = (part[0], part[1])
                part = tup
                name = part[0]
    
            
            if name == 'wmoHeader':
                text += self.processWmoHeader(productDict['wmoHeader']) + '\n'
            elif name == 'easMessage':
                text += productDict['easMessage'] + '\n'
            elif name == 'productHeader':
                text += self.processProductHeader(productDict['productHeader'])
            elif name == 'vtecRecords':
                for vtecString in productDict['vtecRecords']:
                    text += vtecString + '\n'
            elif name == 'areaList':
                text += self._textProduct.indentText(productDict['areaList'], '', '',
                                                     maxWidth=self._textProduct._lineLength)
            elif name == 'issuanceTimeDate':
                text += productDict['issuanceTimeDate'] + '\n\n'
            elif name == 'summaryHeadlines':
                text += self.processSummaryHeadlines(productDict['summaryHeadlines'])
            elif name == 'locationsAffected':
                text += self.processLocationsAffected(productDict['locationsAffected'])
            elif 'sectionHeader' in name:
                text += "* " + productDict[name].upper() + "\n"
            elif 'Subsection' in name:
                text += self.processSubsection(productDict[name])
            elif name == 'infoSection':
                text += self.processInfoSection(productDict['infoSection'])
            elif name in ['endProduct', 'endSection']:
                text += '$$\n' 
            elif name == 'CR':
                text += '\n'
            elif name == 'doubleAmpersand':
                text += '&&\n'
            elif name not in self._noOpParts():
                textStr = productDict.get(name)
                if textStr:
                    text += textStr + '\n'
                    
        #  Cleanup the case of the last segment which will wind up with two sets
        #  of '$$'
        text = re.sub("\$\$\n+\$\$", "$$\n", text)
        
        #  Return completed text
        return text

    def _noOpParts(self):
        '''
        These represent product parts that should be skipped when calling product part methods.
        They will be handled automatically by the formatters.
        '''
        return ["setup_segment"] #['CR', 'endProduct', 'endSegment', 'issuanceDateTime', 'doubleAmpersand']

    def processWmoHeader(self, wmoHeader):
        text = wmoHeader['TTAAii'] + ' ' + wmoHeader['fullStationID'] + ' ' + wmoHeader['ddhhmmTime'] + '\n'
        text += wmoHeader['productID'] + wmoHeader['siteID'] + '\n'
        return text

    def processProductHeader(self, headerDict):
        text = headerDict['stormName'] + ' ' + headerDict['productName']
               
        advisoryText = ''
        if headerDict['advisoryType'] is not None and \
           headerDict['advisoryType'].lower() in ["intermediate", "special"]:
            advisoryText = headerDict['advisoryType'] + ' '
        
        if headerDict['advisoryNumber'] is not None:
            advisoryText += 'Advisory Number ' + headerDict['advisoryNumber']
            
        if len(advisoryText) > 0:
            if len(text + "/" + advisoryText) > self._textProduct._lineLength:
                text += '\n'
            else:
                text += '/'
            
            text += advisoryText + '\n'
        else:
            text += '\n'
        
        text += "National Weather Service " + headerDict['cityState'] + "  " + headerDict['stormNumber'] + '\n'
        text += headerDict['issuanceTimeDate'] + '\n\n'
        
        return text
    
    def processLocationsAffected(self, locationsAffectedList):
        if len(locationsAffectedList) == 0:
            return ""
        
        text = "* LOCATIONS AFFECTED\n"
        for location in locationsAffectedList:
            text += self.TAB + "- " + location + "\n"
        return text + "\n"
    
    def processSubsection(self, subsectionOrderedDict):
        text = ""
        for partName in subsectionOrderedDict:
            if "Summary" in partName:
                firstIndentText = self.TAB + "- "
                nextIndentText  = self.TAB + "  "
                text += self._textProduct.indentText(subsectionOrderedDict[partName],
                                                     firstIndentText,
                                                     nextIndentText,
                                                     maxWidth = self._textProduct._lineLength)
            else:
                firstIndentText = self.TAB*2 + "- "
                nextIndentText  = self.TAB*2 + "  "
                if "threatStatements" in partName:
                    text += self.processThreatStatements(firstIndentText,
                                                         nextIndentText,
                                                         subsectionOrderedDict[partName])
                elif "potentialImpactsStatements" in partName:
                    text += self.processImpactsStatements(firstIndentText,
                                                          nextIndentText,
                                                          subsectionOrderedDict[partName])
                else:
                    text += self._textProduct.indentText(subsectionOrderedDict[partName],
                                                         firstIndentText,
                                                         nextIndentText,
                                                         maxWidth=self._textProduct._lineLength)
        
        return text + "\n"
    
    def processThreatStatements(self, firstIndentText, nextIndentText, threatStatements):
        planning = threatStatements[0]
        text = self._textProduct.indentText(planning,
                                            firstIndentText,
                                            nextIndentText,
                                            maxWidth=self._textProduct._lineLength)

        preparation = threatStatements[1]
        text += self._textProduct.indentText(preparation,
                                             firstIndentText,
                                             nextIndentText,
                                             maxWidth=self._textProduct._lineLength)        

        action = threatStatements[2]
        text += self._textProduct.indentText(action,
                                             firstIndentText,
                                             nextIndentText,
                                             maxWidth=self._textProduct._lineLength)
       
        return text
    
    def processImpactsStatements(self, firstIndentText, nextIndentText, statements):
        text = ""
        
        for statement in statements:
            text += self._textProduct.indentText(statement,
                                                 firstIndentText,
                                                 nextIndentText,
                                                 maxWidth=self._textProduct._lineLength)

        return text
    
    def processInfoSection(self, infoSection):
        if len(infoSection) == 0:
            return ""
        
        text = "* FOR MORE INFORMATION:\n"
        text += self._buildInfoSection(infoSection, tabLevel=1)
        return text + "\n$$\n\n"
    
    def _buildInfoSection(self, infoSection, tabLevel):
        text = ""
        for component in infoSection:
            if type(component) is str:
                text += self.TAB*tabLevel + "- " + component + "\n"
            elif type(component) is list:
                text += self._buildInfoSection(component, tabLevel+1)
        return text

    def processSummaryHeadlines(self, summaryDict):
        text = ""
        for headline in summaryDict['headlinesInEffect']:
            text += headline.upper() + "\n"
        
        text += "\n"
        
        for definition in summaryDict['headlineDefinitions']:
            text += self._textProduct.indentText(definition,
                                                 maxWidth=self._textProduct._lineLength) \
                    + "\n"
        return text

    def processSubParts(self, subParts, infoDicts):
        """
        Generates Legacy text from a list of subParts e.g. segments or sections
        @param subParts: a list of dictionaries for each subPart
        @param partsLists: a list of Product Parts for each segment
        @return: Returns the legacy text of the subParts
        """
        text = '' 
        for i in range(len(subParts)):
            newtext = self._processProductParts(subParts[i], infoDicts[i].get('partsList'))
            text += newtext
        return text

