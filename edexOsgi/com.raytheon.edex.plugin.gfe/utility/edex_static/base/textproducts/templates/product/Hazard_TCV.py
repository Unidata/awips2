# Version 2015.5.22-0

import GenericHazards
import JsonSupport
import LocalizationSupport
import time, types, copy, LogStream, collections
import ModuleAccessor
import math


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
                          "_calculateStartTime": 0,
                          "_resolution": 0,
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
                          "_pastSurgeThreatsNotNone": 0,
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
                          "_updateStatsForPwsXXint": 0,
                          "_updateStatsForPwsTXX": 0,
                          "_updateWindTimeInfo": 0,
                          "_computeWindOnsetAndEnd": 0,
                          "_createWindow": 0,
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
            ("InundationMax", self.moderatedMax, [6]),
            ("InundationTiming", self.moderatedMax, [6]),
            ("StormSurgeThreat", self.mostSignificantDiscreteValue),
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
            
            if vtecRecord["phen"] == "SS":
                # Temporary? Change the vtec mode for SS hazards to be experimental
                vstr = vstr[0] + 'X' + vstr[2:]
                
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
            definition += "Life threatening inundation levels"
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
                self._getStats(self._argDict, segment, self._editAreaDict, self._timeRangeList)
            
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

        self._sampler = self.getSampler(argDict,
          (self._analysisList(), self._timeRangeList, editAreas))
        
        # For storm surge, the edit areas are intersected with a special edit area
        intersectAreas = self._computeIntersectAreas(editAreas, argDict)
        self._intersectSampler = self.getSampler(argDict,
          (self._intersectAnalysisList(), self._timeRangeList, intersectAreas))

        #  Make a sample period for the previous rainfall
        self._previousRainfallTR = [(self._extraSampleTimeRange, "PrevRainfall")]
        self._extraRainfallSampler = self.getSampler(argDict,
            (self._extraRainfallAnalysisList(),  self._previousRainfallTR, 
             editAreas))
    
    def _getStats(self, argDict, segment, editAreaDict, timeRangeList):
        # Get statistics for this segment
        
        editArea = editAreaDict[segment]
        statList = self.getStatList(self._sampler,
                                    self._analysisList(),
                                    timeRangeList,
                                    editArea)
        
        self.debug_print("*"*80, 1)
#         for index in range(len(timeRangeList)):
        self.debug_print("editArea =" + editArea, 1)
        self.debug_print("timeRangeList = %s" % (self._pp.pformat(timeRangeList)), 1)
        self.debug_print("statList = %s" % (self._pp.pformat(statList)), 1)
        self.debug_print("-"*40, 1)
        
        windStats = WindSectionStats(self, segment, statList, timeRangeList)
        
        # The surge section needs sampling done with an intersected edit area
        if editArea in self._coastalAreas():
            intersectEditArea = "intersect_"+editArea
            intersectStatList = self.getStatList(self._intersectSampler,
                                                 self._intersectAnalysisList(),
                                                 timeRangeList,
                                                 intersectEditArea)
        else:
            intersectStatList = "InlandArea"
        
        self.debug_print("intersectStatList = %s" % (self._pp.pformat(intersectStatList)), 1)
        self.debug_print("-"*40, 1)

        stormSurgeStats = StormSurgeSectionStats(self, segment, intersectStatList, timeRangeList)

        #  These stats are for handling the extra rainfall
        extraRainfallStatList = self.getStatList(self._extraRainfallSampler,
                                             self._extraRainfallAnalysisList(),
                                             self._previousRainfallTR,
                                             editArea)
         
        floodingRainStats = FloodingRainSectionStats(self, segment, 
                               statList, timeRangeList, 
                               extraRainfallStatList, self._previousRainfallTR)
        tornadoStats = TornadoSectionStats(self, segment, statList, timeRangeList)
                
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
            "WindThreat":            None,
            "WindForecast":          None,
            "StormSurgeThreat":      None,
            "StormSurgeForecast":    None,
            "FloodingRainThreat":    None,
            "FloodingRainForecast":  None,
            "TornadoThreat":         None,
        }
    
    def _getPreviousAdvisories(self):
        stormAdvisories = self._getStormAdvisoryNames()
        
        self.debug_print("DEBUG: stormAdvisories = %s" % 
                         (self._pp.pformat(stormAdvisories)), 1)
        
        previousAdvisories = []
        
        #  Get the current storm name from the TCP
        curAdvisoryString = self._getStormNameFromTCP()
        self.debug_print("DEBUG: curAdvisoryString = %s" % 
                         (curAdvisoryString), 1)
     
        #  Filter out the advisories we wish to process further
        for advisory in stormAdvisories:
            
            #  If this was an advisory for the current storm
            if advisory.find(curAdvisoryString) != -1:

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
        return [
            {
            "name": "StormInfo",
            "label": "Obtain Storm Type/Name/Info",
            "options": [
                "TCPAT1", "TCPAT2", "TCPAT3", "TCPAT4", "TCPAT5",
                "Enter PIL below (e.g. TCPEP1):",
                ],
            "entryField": "     ",
            },
            {
            "name": "PopulateSurge",
            "label": "Populate Surge Section",
            "options": [
                ("Populate", True),
                ("Do not populate", False),
                ],
            "default": "None",
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
        self.isThreatInAllAdvisories = False
        
    def _isThreatInAllAdvisories(self, threatName):
        previousAdvisories = self._textProduct._getPreviousAdvisories()
        
        for advisory in previousAdvisories:
            if advisory["ZoneData"][self._segment][threatName] == "None":
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
                                      "Current Threat to Life and Property: " + threatLevel)

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
        elif currentThreat == "Extreme" and \
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
        
        self._textProduct.debug_print("tr is currently -> '%s'" % (tr), 1)

        if tr == "default":
            records = self._textProduct._getVtecRecords(self._segment)
            for record in records:
                if record["phen"] in ["HU", "TR"] and record["sig"] == "W":
                    if record["act"] == "CAN":
                        tr = "recovery"
                        break
                    # This is just for 2015
                    elif record["act"] == "CON" and \
                         section == "Surge" and \
                         self._textProduct._currentAdvisory['ZoneData'][self._segment]["StormSurgeThreat"] == "None" and \
                         self._pastSurgeThreatsNotNone():
                        tr = "recovery"
                        break
                    
            if tr == "default" and \
               section == "Wind" and \
               self._pastWindHazardWasCAN():
                tr = "recovery"

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
    
    def _pastSurgeThreatsNotNone(self):
        previousAdvisories = self._textProduct._getPreviousAdvisories()
        
        #  If there are NOT any advisories to process - no need to continue
        if len(previousAdvisories) == 0:
            return False
        
        #  Look at all past advisories for this storm
        for advisory in previousAdvisories:

            #  We had a threat previously
            if advisory["ZoneData"][self._segment]["StormSurgeThreat"] in ["Elevated", "Mod", "High", "Extreme"]:
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
        if self._tr is not None:
            if self._tr == "hunker down":
                return "Potential Impacts: Still Unfolding"
            elif self._tr == "recovery":
                return "Realized Impacts: Still Being Assessed"
        
        if maxThreat == "Extreme":
            impactLevel = "Devastating to Catastrophic"
        elif maxThreat == "High":
            impactLevel = "Extensive"
        elif maxThreat == "Mod":
            impactLevel = "Significant"
        elif maxThreat == "Elevated":
            impactLevel = "Limited"
        else:
            impactLevel = "None"

        return "Potential Impacts: " + impactLevel
    
    def _potentialImpactsStatements(self, segmentDict, productSegmentGroup, productSegment):
        self._textProduct.debug_print("segment = %s, elementName = %s, maxThreat = %s" % 
                         (productSegment[0], self._sectionHeaderName, self._stats._maxThreat), 1)        
        if self._stats._maxThreat is not None:
            statements = self._getPotentialImpactsStatements(productSegment, self._sectionHeaderName, self._stats._maxThreat)
            self._setProductPartValue(segmentDict, 'potentialImpactsStatements', statements)
    
    def _getPotentialImpactsStatements(self, productSegment, elementName, maxThreat):
        if self._tr is not None:
            specialStatements = self._specialImpactsStatements()
            if self._tr in specialStatements.keys():
                if self._tr == "recovery" and not self.isThreatInAllAdvisories:
                    return []
                else:
                    return specialStatements[self._tr]
        
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

        #  If this is the "default" case
        #if self._tr == "default" and len(statements) > 0:
        #
        #    if elementName in ["Wind", "Storm Surge"]:
        #        if statements[0].find("If realized, ") == -1:
        #           statements[0] = "If realized, " + statements[0][0].lower() + statements[0][1:]

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
        self.isThreatInAllAdvisories = self._isThreatInAllAdvisories("WindThreat")
        
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
        self._peakWind(subsectionDict, productSegmentGroup, productSegment)
        self._windowTS(subsectionDict, productSegmentGroup, productSegment)
        self._windowHU(subsectionDict, productSegmentGroup, productSegment)
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'forecastSubsection', subsectionDict)
    
    def _latestForecastSummary(self, segmentDict, productSegmentGroup, productSegment):
        if self._stats._maxWind is None:
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
    
            forecastText = "Latest Local Forecast: "
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
        self._threatTrend(subsectionDict, productSegmentGroup, productSegment)
        self._threatStatements(subsectionDict, productSegmentGroup, productSegment)
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'threatSubsection', subsectionDict)
    
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
        self._potentialImpactsStatements(subsectionDict, productSegmentGroup, productSegment)
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'impactsSubsection', subsectionDict)
    
    def _specialImpactsStatements(self):
        return {"hunker down": ["Potential impacts from the main wind event are still unfolding.",
                                "The extent of realized impacts will depend on the actual strength, duration, and exposure of the wind as experienced at particular locations.",
                                ],
                "recovery": ["Little to no additional wind impacts expected. Community officials are now assessing the extent of actual wind impacts accordingly.",
                             ],
                }

    ### Supporting functions
    def _moderatedMaxWindMph_categories(self):
        # Dictionary representing wind thresholds in kts
        # for category 1, 2, 3, 4 or 5 hurricanes.
        return {
            'CAT 5 Hurricane':       (157, 999),
            'CAT 4 Hurricane':       (130, 157),
            'CAT 3 Hurricane':       (111, 130),
            'CAT 2 Hurricane':       ( 96, 111),
            'CAT 1 Hurricane':       ( 74,  96),
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
        self.isThreatInAllAdvisories = self._isThreatInAllAdvisories("StormSurgeThreat")
        
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
                                      "Latest local forecast: Not available at this time. To be updated shortly.")
 
        elif "None" in self._stats._windowSurge or \
             self._stats._inundationMax is None or \
             self._stats._inundationMax < 1:
            self._setProductPartValue(segmentDict, 'latestForecastSummary',
                                      "No storm surge inundation forecast")
        else:
            max = self._stats._inundationMax
            summary = "Latest local forecast: "

            if 1 <= max and max < 4:
                summary += "Localized"
            elif 4 <= max and max < 12:
                summary += "Life-threatening"
            else:
                summary += "Life-threatening and historic"
                
            self._setProductPartValue(segmentDict, 'latestForecastSummary', 
                                      summary + " storm surge possible")

    def _peakSurge(self, segmentDict, productSegmentGroup, productSegment):
        if self._stats._inundationMax is not None and self._stats._inundationMax >= 1:
            max = self._stats._inundationMax
            if max > 10:
                maxRange = 4
            elif max > 6:
                maxRange = 3
            elif max > 2:
                maxRange = 2
            else:
                maxRange = None

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
        return {"hunker down": ["Potential impacts from the main surge event are still unfolding.",
                                "The extent of realized impacts will depend on the actual height of storm surge moving onshore and the resulting depth of coastal flooding as experienced at particular locations.",
                                ],
                "recovery": ["Little to no additional surge impacts expected. Community officials are now assessing the extent of actual surge impacts accordingly.",
                             ],
                }
    
    def _potentialImpactsSummary(self, segmentDict, productSegmentGroup, productSegment):
        if not self._textProduct._PopulateSurge:

        #  We do not want the '(For plausible worst case)' in the text
#             self._setProductPartValue(segmentDict, 'potentialImpactsSummary',
#                                       "Potential Impacts (For plausible worst case): Not available at this time. To be updated shortly.")
            self._setProductPartValue(segmentDict, 'potentialImpactsSummary',
                                      "Potential Impacts: Not available at this time. To be updated shortly.")
        else:
            SectionCommon._potentialImpactsSummary(self, segmentDict, productSegmentGroup, productSegment)


class FloodingRainSection(SectionCommon):
    def __init__(self, textProduct, segment, stats):
        SectionCommon.__init__(self, textProduct, segment, "Flooding Rain")
        self._sectionName = 'floodingRainSection[\'' + segment + '\']'
        self._stats = stats
        self.isThreatInAllAdvisories = self._isThreatInAllAdvisories("FloodingRainThreat")
        
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
        self._peakRain(subsectionDict, productSegmentGroup, productSegment)
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'forecastSubsection', subsectionDict)
    
    def _latestForecastSummary(self, segmentDict, productSegmentGroup, productSegment):
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
                                  "Latest Local Forecast: " + summary)
    
    def _peakRain(self, segmentDict, productSegmentGroup, productSegment):
        if self._stats._sumAccum is not None:
            words = self._rainRange(int(self._stats._sumAccum + 0.5))
            
            #  If we have previous rainfall
            if self._stats._prevAccum not in [0.0, None]:
                words = "Additional " + words
            self._setProductPartValue(segmentDict, 'peakRain', "Peak Rainfall Amounts: " + words)
        
    def _rainRange(self, sumAccum):
        minAccum = 0
        maxAccum = 0
        
        if sumAccum == 0:
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
        self._threatTrend(subsectionDict, productSegmentGroup, productSegment)
        self._threatStatements(subsectionDict, productSegmentGroup, productSegment)
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'threatSubsection', subsectionDict)
    
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
        self._potentialImpactsStatements(subsectionDict, productSegmentGroup, productSegment)
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'impactsSubsection', subsectionDict)
    
    def _specialImpactsStatements(self):
        return {"hunker down": ["Potential impacts from flooding rain are still unfolding.",
                                "The extent of realized impacts will depend on actual rainfall amounts as received at particular locations.",
                                ],
                "recovery": ["For additional information on impacts being caused by flooding rain, refer to the local hazardous weather outlook or hurricane local statement.",
                             ],
                }

class TornadoSection(SectionCommon):
    def __init__(self, textProduct, segment, stats):
        SectionCommon.__init__(self, textProduct, segment, "Tornado")
        self._sectionName = 'tornadoSection[\'' + segment + '\']'
        self._stats = stats
        self.isThreatInAllAdvisories = self._isThreatInAllAdvisories("TornadoThreat")
        
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
        self._tornadoSituation(subsectionDict, productSegmentGroup, productSegment)
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'forecastSubsection', subsectionDict)
    
    def _latestForecastSummary(self, segmentDict, productSegmentGroup, productSegment):
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
                                  "Latest Local Forecast: " + summary)
 
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
        self._threatTrend(subsectionDict, productSegmentGroup, productSegment)
        self._threatStatements(subsectionDict, productSegmentGroup, productSegment)
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'threatSubsection', subsectionDict)
    
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
        self._potentialImpactsStatements(subsectionDict, productSegmentGroup, productSegment)
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'impactsSubsection', subsectionDict)
    
    def _specialImpactsStatements(self):
        return {"hunker down": ["Potential impacts from tropical tornadoes are still unfolding.",
                                "The extent of realized impacts will depend on the severity of actual tornado occurrence as experienced at particular locations.",
                                ],
                "recovery": ["For additional information on impacts being caused by tropical tornadoes, refer to the local hazardous weather outlook or hurricane local statement.",
                             ],
                }


###############################################################
### TCV Statistics Classes
    
class SectionCommonStats():
    def __init__(self, textProduct, segment):
        self._textProduct = textProduct
        self._segment = segment
        
        self._initializeSegmentAdvisories()
        
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
        self._textProduct.debug_print("statDict = '%s'" % (self._textProduct._pp.pformat(statDict)), 1)
        
        threatLevel = self._textProduct._getStatValue(statDict, threatGridName)
        if threatLevel is not None:
            threatLevels = self._textProduct._threatKeyOrder()
            self._textProduct.debug_print("updateThreatStats for %s" % (threatGridName), 1)
            self._textProduct.debug_print("threatLevel = %s" % (threatLevel), 1)
            self._textProduct.debug_print("maxThreat = %s" % (self._maxThreat), 1)
            if self._maxThreat is None or \
               threatLevels.index(threatLevel) > threatLevels.index(self._maxThreat):
                self._textProduct.debug_print("updating max threat to = %s" % (threatLevel), 1)
                self._maxThreat = threatLevel
    
    def _calculateHourOffset(self, targetTime):
        seconds = targetTime.unixTime() - self._textProduct._issueTime_secs
        hour = int(round(seconds/60.0/60.0))
        if hour < 0:
            hour = 0
        
        return hour

class WindSectionStats(SectionCommonStats):
    def __init__(self, textProduct, segment, statList, timeRangeList):
        SectionCommonStats.__init__(self, textProduct, segment)
        self._maxWind = None
        self._maxGust = None
        self._onset34Hour = None
        self._end34Hour = None
        self._onset64Hour = None
        self._end64Hour = None
        self._windowTS = None
        self._windowHU = None

        self._textProduct.debug_print("*"*90)   
        self._textProduct.debug_print("Setting wind stats for %s" % (segment), 1)
        
        self._setStats(statList, timeRangeList)
    
    class PwsXXintStats():
        max = None
        onsetHour = None
    
    class PwsTXXStats():
        firstRun = True
        dropFirstGridType = None
        droppedFirstGrid = False
        periodWithFirstCorrectGrid = None
        endTime = None
    
    class TimeInfo():
        onsetHour = None
        endHour = None
        
    class EventsOccurring():
        pwsTXXEvent = False
        windXXEvent = False
      
    def _setStats(self, statList, timeRangeList):
        pws34intStats = self.PwsXXintStats()
        pws64intStats = self.PwsXXintStats()
        pwsT34Stats = self.PwsTXXStats()
        pwsT64Stats = self.PwsTXXStats()
        wind34timeInfo = self.TimeInfo()
        wind64timeInfo = self.TimeInfo()
        
        currentPeriod = None
        for index in range(len(statList)):
            tr, _ = timeRangeList[index]
            statDict = statList[index]
            
            self._textProduct.debug_print("\n\ntr = %s    statDict = %s" % (tr, statDict), 1)
            
            for periodIndex, periodTr in enumerate(self._textProduct._periodList):
                self._textProduct.debug_print("\n\nperiodIndex = %d    periodList tr = %s" % (periodIndex, repr(periodTr)), 1)
                if (periodIndex == 0) and (tr.startTime().unixTime() < periodTr.startTime().unixTime()):
                    # If the tr is before the first period, use the first period
                    currentPeriod = periodIndex
                    break
                elif periodTr.contains(tr.startTime()):
                    currentPeriod = periodIndex
                    break
            
            self._textProduct.debug_print("\n\ncurrentPeriod index = %s" % (currentPeriod), 1)
            self._textProduct.debug_print("\n\ncurrentPeriod tr = %s" % (self._textProduct._periodList[currentPeriod]), 1)

            self._updateStatsForPwsXXint(tr, statDict, "pws34int", pws34intStats)
            self._updateStatsForPwsXXint(tr, statDict, "pws64int", pws64intStats)
             
            self._updateStatsForPwsTXX(tr, statDict, "pwsD34", "pwsN34", pwsT34Stats, currentPeriod)
            self._updateStatsForPwsTXX(tr, statDict, "pwsD64", "pwsN64", pwsT64Stats, currentPeriod)
             
            wind = self._textProduct._getStatValue(statDict, "Wind", "Max", self._textProduct.VECTOR())
            if wind is not None:
                if self._maxWind is None or wind >= self._maxWind:
                    self._maxWind = wind
                     
                self._updateWindTimeInfo(tr, wind, wind34timeInfo, speed=34)
                self._updateWindTimeInfo(tr, wind, wind64timeInfo, speed=64)
             
            windGust = self._textProduct._getStatValue(statDict, "WindGust", "Max")
            if windGust is not None:
                if self._maxGust is None or windGust > self._maxGust:
                    self._maxGust = windGust
                     
            self._updateThreatStats(tr, statDict, "WindThreat")
         
        #Tropical Storm
        onsetEndInfo = self._computeWindOnsetAndEnd(wind34timeInfo, pws34intStats, pwsT34Stats)
        self._onset34Hour = onsetEndInfo.onsetHour
        self._end34Hour = onsetEndInfo.endHour
         
        self._textProduct.debug_print("Tropical Storm Window:", 1)
        self._windowTS = self._createWindow("Tropical Storm",
                                            self._onset34Hour,
                                            self._end34Hour)
         
        #Hurricane
        onsetEndInfo = self._computeWindOnsetAndEnd(wind64timeInfo, pws64intStats, pwsT64Stats)
        self._onset64Hour = onsetEndInfo.onsetHour
        self._end64Hour = onsetEndInfo.endHour
         
        self._textProduct.debug_print("Hurricane Window:", 1)
        self._windowHU = self._createWindow("Hurricane",
                                            self._onset64Hour,
                                            self._end64Hour)
         
        self._currentAdvisory["WindThreat"] = self._maxThreat
        self._currentAdvisory["WindForecast"] = self._maxWind
 
        #======================================================================
        #  Let operator know if any required stats are missing
        
        missingGridsList = []

        self._textProduct.debug_print("+"*60, 1)
        self._textProduct.debug_print("In WindSectionStats._setStats", 1)
        self._textProduct.debug_print("pws34intStats.max = %s" % (pws34intStats.max), 1)
        self._textProduct.debug_print("pws64intStats.max = %s" % (pws64intStats.max), 1)
        self._textProduct.debug_print("pwsT34Stats.periodWithFirstCorrectGrid = %s" % (pwsT34Stats.periodWithFirstCorrectGrid), 1)
#         self._textProduct.debug_print("pwsT34Stats.endTime = '%s'" % (pwsT34Stats.endTime), 1)
        self._textProduct.debug_print("pwsT64Stats.periodWithFirstCorrectGrid = %s" % (pwsT64Stats.periodWithFirstCorrectGrid), 1)
#         self._textProduct.debug_print("pwsT64Stats.endTime = '%s'" % (pwsT64Stats.endTime), 1)
        self._textProduct.debug_print("self._maxWind = %s" % (self._maxWind), 1)
        self._textProduct.debug_print("self._maxGust = %s" % (self._maxGust), 1)
        self._textProduct.debug_print("self._maxThreat = %s" % (self._maxThreat), 1)
        
        #  Interval wind speed probabilities
        if pws34intStats.max is None:
            missingGridsList.append("pws34int")
        
        if pws64intStats.max is None:
            missingGridsList.append("pws64int")
        
        #  Incremental wind speed probabilities
        if pwsT34Stats.periodWithFirstCorrectGrid is None:
            missingGridsList.append("PWSD34")
            missingGridsList.append("PWSN34")
        
        if pwsT64Stats.periodWithFirstCorrectGrid is None:
            missingGridsList.append("PWSD64")
            missingGridsList.append("PWSN64")
 
        #  Deterministic wind
        if self._maxWind is None:
            missingGridsList.append("Wind")
        
        #  Deterministic wind gust 
        if self._maxGust is None:
            missingGridsList.append("WindGust")
        
        #  Threat grid
        if self._maxThreat is None:
             missingGridsList.append("WindThreat")
           
        #  If there are any missing grids - let the user know
        if len(missingGridsList) > 0:
            msg = "\n\nSome grids are missing! Please check these grids " + \
                  "before trying to run the formatter again:\n"
            
            for item in missingGridsList:
                msg += "\n%s" % (item)
            
            msg += "\n\n"
       
            #  Throw a statistics exception
            #  (no point in continuing until the grids are fixed)
            raise self._textProduct.StatisticsException(msg)
       
    def _updateStatsForPwsXXint(self, tr, statDict, gridName, pwsXXintStats):
        pwsXXint = self._textProduct._getStatValue(statDict, gridName, "Max")
        
        if pwsXXint is not None:
            if pwsXXintStats.max is None or pwsXXint > pwsXXintStats.max:
                pwsXXintStats.max = pwsXXint
                pwsXXintStats.onsetHour = self._calculateHourOffset(tr.startTime())
                
                self._textProduct.debug_print("Wind Window Debug: pwsXXintStats gridName = %s" % (gridName), 1)
                self._textProduct.debug_print("Wind Window Debug: pwsXXintStats onsetHour = %s" % (pwsXXintStats.onsetHour), 1)
    
    def _updateStatsForPwsTXX(self, tr, statDict, dayGridName, nightGridName, pwsTXXStats, period):
        pwsDXX = self._textProduct._getStatValue(statDict, dayGridName, "Max")
        pwsNXX = self._textProduct._getStatValue(statDict, nightGridName, "Max")
        
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
            self._textProduct.debug_print("Getting probability threshold for period %s" % (period), 1)
            if period == 0:
                (thresholdLow, thresholdHigh) = thresholds[period][index]
                threshold = thresholdLow
            else:
                threshold = thresholds[period][index]
                
            if maxPws > threshold:
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
                self._textProduct.debug_print("Wind Window Debug: pwsTXXStats endTime = %s" % (self._textProduct._pp.pformat(pwsTXXStats.endTime)), 1)
                self._textProduct.debug_print("Wind Window Debug: period tr = %s" % (self._textProduct._pp.pformat(self._textProduct._periodList[period])), 1)
    
    def _updateWindTimeInfo(self, tr, wind, timeInfo, speed):
        if wind >= speed:
            timeInfo.endHour = self._calculateHourOffset(tr.endTime())
            
            self._textProduct.debug_print("Wind Window Debug: In _updateWindTimeInfo", 1)
            self._textProduct.debug_print("Wind Window Debug: timeInfo speed = %s" % (speed), 1)
            self._textProduct.debug_print("Wind Window Debug: timeInfo maxWind = %s" % (self._maxWind), 1)
            self._textProduct.debug_print("Wind Window Debug: timeInfo tr = %s" % (self._textProduct._pp.pformat(tr)), 1)
            self._textProduct.debug_print("Wind Window Debug: timeInfo endHour = %s" % (timeInfo.endHour), 1)
            
            if timeInfo.onsetHour is None:
                timeInfo.onsetHour = self._calculateHourOffset(tr.startTime())
                
                self._textProduct.debug_print("Wind Window Debug: onsetHour was None", 1)
                self._textProduct.debug_print("Wind Window Debug: timeInfo speed = %s" % (speed), 1)
                self._textProduct.debug_print("Wind Window Debug: timeInfo maxWind = %s" % (self._maxWind), 1)
                self._textProduct.debug_print("Wind Window Debug: timeInfo tr = %s" % (self._textProduct._pp.pformat(tr)), 1)
                self._textProduct.debug_print("Wind Window Debug: timeInfo onsetHour = %s" % (timeInfo.onsetHour), 1)
    
    def _computeWindOnsetAndEnd(self, windTimeInfo, pwsXXintStats, pwsTXXStats):
        onsetEndInfo = self.TimeInfo()
 
        self._textProduct.debug_print("Wind Window Debug: In _computeWindOnsetAndEnd", 1)
        self._textProduct.debug_print("Wind Window Debug: windTimeInfo.onsetHour = %s" % (windTimeInfo.onsetHour), 1)
        self._textProduct.debug_print("Wind Window Debug: pwsXXintStats.onsetHour = %s" % (pwsXXintStats.onsetHour), 1)
        self._textProduct.debug_print("Wind Window Debug: windTimeInfo.endHour = %s" % (windTimeInfo.endHour), 1)
        self._textProduct.debug_print("Wind Window Debug: pwsTXXStats.endTime = %s" % (pwsTXXStats.endTime), 1)
        if pwsTXXStats.endTime is not None:
            self._textProduct.debug_print("Wind Window Debug: pwsTXXStats end hour = %s" % (self._calculateHourOffset(pwsTXXStats.endTime)), 1)
        
        if windTimeInfo.onsetHour is None:
            # We won't have a timing window
            self._textProduct.debug_print("onsetHour for wind is None", 1)
            return onsetEndInfo
        
        if windTimeInfo.onsetHour < 6:
            self._textProduct.debug_print("onsetHour for wind is < 6, using that as window onset hour", 1)
            onsetEndInfo.onsetHour = windTimeInfo.onsetHour
            self._textProduct.debug_print("onsetHour = " + str(onsetEndInfo.onsetHour), 1)
        elif pwsXXintStats.onsetHour is not None:
            self._textProduct.debug_print("using min onset hour betweeen wind and pwsXXintStats", 1)
            onsetEndInfo.onsetHour = min(windTimeInfo.onsetHour, pwsXXintStats.onsetHour)
            self._textProduct.debug_print("onsetHour = " + str(onsetEndInfo.onsetHour), 1)
        else:
            self._textProduct.debug_print("ERROR: onsetHour for pwsXXintStats is None. Check the grids.", 1)
            return onsetEndInfo
        
        if windTimeInfo.endHour > 114 or windTimeInfo.endHour < 6:
            self._textProduct.debug_print("using endHour for wind as window end hour", 1)
            onsetEndInfo.endHour = windTimeInfo.endHour
            self._textProduct.debug_print("endHour = " + str(onsetEndInfo.endHour), 1)
        elif pwsTXXStats.endTime is not None:
            self._textProduct.debug_print("endTime for pwsTXXStats is not None", 1)
            endUnixTime = pwsTXXStats.endTime.unixTime()
            endLocalTime = time.localtime(endUnixTime)
            utcHourOffset = self._calculateUTCandLocalHourOffset()
            self._textProduct.debug_print("utcHourOffset = %s" % (utcHourOffset), 1)
            self._textProduct.debug_print("endTime for pwsTXXStats in local time is %s" % 
                                          (self._textProduct._pp.pformat(endLocalTime)), 1)
             
            #  Remember these times are in local time zone, so hour 0 is 
            #  midnight of the current calendar day.
            if endLocalTime.tm_hour > 6 and endLocalTime.tm_hour <= 18:
                configuredTime = absTimeYMD(pwsTXXStats.endTime.year,
                                            pwsTXXStats.endTime.month,
                                            pwsTXXStats.endTime.day,
                                            self._textProduct.NIGHT())
            else:
                configuredTime = absTimeYMD(pwsTXXStats.endTime.year,
                                            pwsTXXStats.endTime.month,
                                            pwsTXXStats.endTime.day,
                                            self._textProduct.DAY())
            self._textProduct.debug_print("configuredTime (local time) = %s" % 
                                          (self._textProduct._pp.pformat(configuredTime)), 1)
            
            # The configured hour is localtime so we need to add an offset to make the entire date UTC
            configuredUnixTime = configuredTime.unixTime() + (utcHourOffset * 3600)
            configuredTime = AbsTime(configuredUnixTime)
            self._textProduct.debug_print("configuredTime (UTC time) = %s" % 
                                          (self._textProduct._pp.pformat(configuredTime)), 1)


            probEndHour = self._calculateHourOffset(configuredTime)
            
            onsetEndInfo.endHour = int(round(self._textProduct.average(windTimeInfo.endHour, probEndHour)))
            self._textProduct.debug_print("endHour = " + str(onsetEndInfo.endHour), 1)
        else:
            self._textProduct.debug_print("ERROR: endHour for pwsTXXStats is None. Check the grids.", 1)
            return onsetEndInfo
        
        return onsetEndInfo
    
    def _createWindow(self, windowName, onsetHour, endHour):
        window = "Window for " + windowName + " force winds: "
        self._textProduct.debug_print("In _createWindow", 1)
        self._textProduct.debug_print("window stats:", 1)
        self._textProduct.debug_print("onsetHour = %s" % (onsetHour), 1)
        self._textProduct.debug_print("endHour = %s" % (endHour), 1)
        
        if onsetHour is None:
            #  We do not want a statement of a non-existent window
#             window += "None"
            window = None
        else:
            startTime = AbsTime(self._textProduct._issueTime_secs + onsetHour*60*60)
            if endHour is not None:
                endTime = AbsTime(self._textProduct._issueTime_secs + endHour*60*60)
                windowPeriod = self._textProduct.makeTimeRange(startTime, endTime)
            else:
                windowPeriod = self._textProduct.makeTimeRange(startTime, startTime + 1)
            self._textProduct.debug_print("window period = %s" % (windowPeriod), 1)
            
            startTimeDescriptor = ""
            if onsetHour >= 18:
                startTimeDescriptor = self._textProduct._formatPeriod(windowPeriod, resolution = 6)
            elif 6 <= onsetHour and onsetHour < 18:
                startTimeDescriptor = self._textProduct._formatPeriod(windowPeriod, resolution = 3)
            
            if len(startTimeDescriptor) == 0 and endHour is None:
                window = None
            elif len(startTimeDescriptor) != 0 and endHour > 114:
                window += "Begins " + startTimeDescriptor
            else:
                connector = "through "
                endTimeDescriptor = "the next few hours"
                
                if endHour >= 18:
                    endTimeDescriptor = self._textProduct._formatPeriod(windowPeriod,
                                                                        useEndTime = True,
                                                                        resolution = 6)
                elif 6 <= endHour and endHour < 18:
                    endTimeDescriptor = self._textProduct._formatPeriod(windowPeriod,
                                                                        useEndTime = True,
                                                                        resolution = 3)
                #  If we are not talking about the next few hours
                if endTimeDescriptor != "the next few hours":
                    connector = "until "

                if len(startTimeDescriptor) != 0:
                    connector = " " + connector
                window += startTimeDescriptor + connector + endTimeDescriptor
                     
        return window
    
    def _calculateUTCandLocalHourOffset(self):
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
        
        self._setStats(intersectStatList, timeRangeList)
    
    def _setStats(self, statList, timeRangeList):
        phishStartTime = None
        phishEndTime = None
        possibleStop = 0
        
        #  If this is an inland area, just move on
        if statList == "InlandArea":
            return
        
        self._textProduct.debug_print("*"*100, 1)
        self._textProduct.debug_print("phishStartTime = %s   phishEndTime  = %s   possibleStop = %d" % 
                                      (str(phishStartTime), str(phishEndTime), possibleStop), 1)
        
        self._textProduct.debug_print("%s" % (self._textProduct._pp.pformat(statList)), 1)
        for period in range(len(statList)):
            tr, _ = timeRangeList[period]
            statDict = statList[period]
            self._textProduct.debug_print("tr = %s" % (self._textProduct._pp.pformat(tr)), 1)
            self._textProduct.debug_print("statDict = %s" % (self._textProduct._pp.pformat(statDict)), 1)
        
            phishPeak = self._textProduct._getStatValue(statDict, "InundationMax", "Max")
            self._textProduct.debug_print("%s phishPeak = %s" % (repr(tr), phishPeak), 1)
            if phishPeak is not None:
                if self._inundationMax is None or phishPeak > self._inundationMax:
                    self._inundationMax = phishPeak
                    
            curPhish = self._textProduct._getStatValue(statDict, "InundationTiming", "Max")
            self._textProduct.debug_print("tr = %s" % (self._textProduct._pp.pformat(tr)), 1)
            self._textProduct.debug_print("curPhish = '%s'    possibleStop = %d" % 
                                          (str(curPhish), possibleStop), 1)
            self._textProduct.debug_print("phishStartTime = %s   phishEndTime  = %s" % 
                                          (str(phishStartTime), str(phishEndTime)), 1)
            
            if curPhish is not None and possibleStop != 2:
                if curPhish >= 0.5:
                    if phishStartTime is None:
                        phishStartTime = tr.startTime()
                        possibleStop = 0
                        phishEndTime = None
                elif phishStartTime is not None:
                    possibleStop += 1
                    
                    if phishEndTime is None:
                        phishEndTime = tr.startTime()
            
            self._updateThreatStats(tr, statDict, "StormSurgeThreat")
        
        self._windowSurge = "Window of concern: "
        
        if phishStartTime is None or self._inundationMax is None or self._inundationMax < 1:
            self._windowSurge += "None"
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
            elif phishStartTime == phishEndTime:
                self._windowSurge += startTimeDescriptor
            else:
                endTimeDescriptor = self._textProduct._formatPeriod(windowPeriod, useEndTime = True)
            
                if self._onsetSurgeHour > 12:
#                     self._windowSurge += startTimeDescriptor +\
#                                          " through " +\
#                                          endTimeDescriptor
                    self._windowSurge += startTimeDescriptor +\
                                         " until " +\
                                         endTimeDescriptor
                else:
                    self._windowSurge += "through " + endTimeDescriptor
        
        self._currentAdvisory["StormSurgeThreat"] = self._maxThreat
        if self._inundationMax is not None:
            # Round so we don't store values like 1.600000023841858
            self._currentAdvisory["StormSurgeForecast"] = \
                    int(self._inundationMax * 10.0) / 10.0

        #======================================================================
        #  Let operator know if any required stats are missing - only if we
        #  are populating the storm surge section
        
        if self._textProduct._PopulateSurge:

            missingGridsList = []
            
            self._textProduct.debug_print("+"*60, 1)
            self._textProduct.debug_print("In StormSurgeSectionStats._setStats", 1)
            self._textProduct.debug_print("self._inundationMax = '%s'" % 
                                          (self._inundationMax), 1)
            self._textProduct.debug_print("self._onsetSurgeHour = '%s'" % 
                                          (self._onsetSurgeHour), 1)
            self._textProduct.debug_print("self._maxThreat = '%s'" % 
                                          (self._maxThreat), 1)
            
            #  Max inundation
            if self._inundationMax is None:
                missingGridsList.append("InundationMax")
                missingGridsList.append("InundationTiming")
            
            #  Inundation timing - only applies if any inundation forecast
            if self._inundationMax >= 1 and self._onsetSurgeHour is None:
                missingGridsList.append("InundationTiming")
    
            #  Threat grid
            if self._maxThreat is None:
                 missingGridsList.append("StormSurgeThreat")
                       
            #  If there are any missing grids - let the user know
            if len(missingGridsList) > 0:
                msg = "\n\nSome grids are missing! Please check these grids " + \
                      "before trying to run the formatter again:\n"
                
                for item in missingGridsList:
                    msg += "\n%s" % (item)
                
                msg += "\n\n"
           
                self._textProduct.debug_print("%s" % (self._textProduct._pp.pformat(dir (self))), 1)
                #  Throw a statistics exception
                #  (no point in continuing until the grids are fixed)
                raise self._textProduct.StatisticsException(msg)


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

        #======================================================================
        #  Let operator know if any required stats are missing
        
        missingGridsList = []
        
        self._textProduct.debug_print("+"*60, 1)
        self._textProduct.debug_print("In FloodingRainSectionStats._setStats", 1)
        self._textProduct.debug_print("self._sumAccum = '%s'" % (self._sumAccum), 1)
        self._textProduct.debug_print("self._maxThreat = '%s'" % (self._maxThreat), 1)
        
        #  Rainfall forecast
        if self._sumAccum is None:
            missingGridsList.append("QPF")
        
        #  Threat grid
        if self._maxThreat is None:
             missingGridsList.append("FloodingRainThreat")
                   
        #  If there are any missing grids - let the user know
        if len(missingGridsList) > 0:
            msg = "\n\nSome grids are missing! Please check these grids " + \
                  "before trying to run the formatter again:\n"
            
            for item in missingGridsList:
                msg += "\n%s" % (item)
            
            msg += "\n\n"
       
            #  Throw a statistics exception
            #  (no point in continuing until the grids are fixed)
            raise self._textProduct.StatisticsException(msg)


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

        #======================================================================
        #  Let operator know if any required stats are missing
        
        missingGridsList = []
        
        self._textProduct.debug_print("+"*60, 1)
        self._textProduct.debug_print("In TornadoSectionStats._setStats", 1)
        self._textProduct.debug_print("self._maxThreat = '%s'" % (self._maxThreat), 1)

        #  Threat grid
        if self._maxThreat is None:
             missingGridsList.append("TornadoThreat")
                   
        #  If there are any missing grids - let the user know
        if len(missingGridsList) > 0:
            msg = "\n\nSome grids are missing! Please check these grids " + \
                  "before trying to run the formatter again:\n"
            
            for item in missingGridsList:
                msg += "\n%s"  % (item) 
       
            #  Throw a statistics exception
            #  (no point in continuing until the grids are fixed)
            raise self._textProduct.StatisticsException(msg)


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
                text += "* " + productDict[name] + "\n"
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
        
        text = "* Locations Affected\n"
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
        
        text = "* For more information:\n"
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
            text += headline + "\n"
        
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
