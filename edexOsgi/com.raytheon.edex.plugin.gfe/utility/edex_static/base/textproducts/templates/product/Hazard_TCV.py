# Version 2014.12.03-0
import GenericHazards
import JsonSupport
import time, types, copy, LogStream, collections
import ModuleAccessor
import math


from AbsTime import *
from StartupDialog import IFPDialog as Dialog
from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType
AWIPS_ENVIRON = "AWIPS2"

import HLSTCV_Common

class TextProduct(HLSTCV_Common.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition["displayName"]   = "None"
    Definition["outputFile"]    = "{prddir}/TEXT/TCV.txt"
    Definition["database"]      =  "Official"  # Source database
    Definition["debug"]         =  1
    Definition["mapNameForCombinations"] = "Zones_<site>"
    Definition["defaultEditAreas"] = "Combinations_TCV_<site>"
    Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display

    Definition["productName"]       = "LOCAL WATCH/WARNING STATEMENT"

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
                           # TextProduct class
                          "__init__": 0,
                          "_inlandAreas": 0,
                          "_coastalAreas": 0,
                          "_cwa": 0,
                          "_analysisList": 0,
                          "_intersectAnalysisList": 0,
                          "_productParts_TCV": 0,
                          "_segmentParts_TCV": 0,
                          "generateForecast": 0,
                          "_initializeVariables": 0,
                          "_createProductDictionary": 0,
                          "_formatProductDictionary": 0,
                          "_easMessage": 0,
                          "_setup_segment": 0,
                          "_ugcHeader": 0,
                          "_vtecRecords": 0,
                          "_areaList": 0,
                          "_locationsAffected": 0,
                          "_fcstConfidence": 0,
                          "_infoSection": 0,
                          "_endSection": 0,
                          "_issuanceTimeDate": 0,
                          "_summaryHeadlines": 0,
                          "_processProductParts": 0,
                          "_noOpParts": 0,
                          "_initializeProductDict": 0,
                          "_wrapUpProductDict": 0,
                          "_groupSegments": 0,
                          "_makeSegmentEditAreas": 0,
                          "_determineSegments": 0,
                          "_refineSegments": 0,
                          "_findSegment": 0,
                          "_formatUGC_entries": 0,
                          "_getUgcInfo": 0,
                          "_hazardDefinition": 0,
                          "_sampleData": 0,
                          "_getStats": 0,
                          "_formatPeriod": 0,
                          "_getTimeDesc": 0,
                          "_getPartOfDay": 0,
                          "_convertToISO": 0,
                          "_convertToDatetime": 0,
                          "_overview_list": 0,
                          "_displayGUI": 0,
                          "_archiveCurrentAdvisory": 0,
                          "_saveAdvisory": 0,
                          "_getHazardsForHLS": 0,
                          
                          # SectionCommon
                          "_setProductPartValue": 0,
                          "_finalSectionParts": 0,
                          "_sectionHeader": 0,
                          "_lifePropertyThreatSummary": 0,
                          "_getThreatTrendSentence": 0,
                          "_getThreatTrendValue": 1,
                          "_threatDifference": 1,
                          "_isThreatDecreasing": 1,
                          "_isThreatIncreasing": 1,
                          "_advisoryHasValidKey": 0,
                          "_isMagnitudeIncreasing": 1,
                          "_calculateThreatStatementTr": 0,
                          "_setThreatStatementsProductParts": 0,
                          "_getThreatStatements": 0,
                          "_potentialImpactsSummary": 0,
                          "_getPotentialImpactsSummaryText": 0,
                          "_potentialImpactsStatements": 0,
                          "_getPotentialImpactsStatements": 0,
                          "_preparationStatement": 0,

                          #  Unique to each section, but common method name
                          "_threatSubsection": 0,
                          "_threatTrend": 0,
                          "_threatStatements": 0,
                          "_impactsSubsection": 0,
                          
                          # WindSection
                          "sectionParts": 0,
                          "_forecastSubsection": 0,
                          "_latestForecastSummary": 0,
                          "_peakWind": 0,
                          "_windowTS": 0,
                          "_windowHU": 0,
                          "_moderatedMaxWindMph_categories": 0,
                          "_ktToMph": 0,
                          "_increment": 0,
                          
                          # StormSurgeSection
                          "sectionParts": 0,
                          "_forecastSubsection": 0,
                          "_latestForecastSummary": 1,
                          "_peakSurge": 0,
                          "_surgeWindow": 1,
                           "_lifePropertyThreatSummary": 0,
                          "_potentialImpactsSummary": 0,
                          
                          # FloodingRainSection
                          "sectionParts": 0,
                          "_forecastSubsection": 0,
                          "_latestForecastSummary": 0,
                          "_peakRain": 0,
                          "_rainRange": 0,

                          # TornadoSection
                          "sectionParts": 0,
                          "_forecastSubsection": 0,
                          "_latestForecastSummary": 0,

                          # SectionCommonStats
                          "_initializeAdvisories": 0,
                          "_updateThreatStats": 0,
                          "_calculateHourOffset": 0,

                          #  Common to all SectionStats classes 
                          "_setStats": 1,
                          
                          # WindsSectionStats
                          "_updateStatsForPwsXXint": 0,
                          "_updateStatsForPwsTXX": 1,
                          "windSpdProb_thresholds": 0,
                          "_updateWindTimeInfo": 0,
                          "_computeWindOnsetAndEnd": 1,
                          "_createWindow": 1,
                      
                          # XMLFormatter
                          "execute": 0,
                          "xmlKeys": 0,
                          "sectionKeys": 0,
                          "getSectionKey": 0,
                          "dictionary": 0,
                          "list": 0,
                          
                          # LegacyFormatter
                          "_processProductParts": 0,
                          "_noOpParts": 0,
                          "processWmoHeader": 0,
                          "processProductHeader": 0,
                          "processLocationsAffected": 0,
                          "processSubsection": 0,
                          "processThreatStatements": 0,
                          "processImpactsStatements": 0,
                          "processInfoSection": 0,
                          "_buildInfoSection": 0,
                          "formatIssueTime": 0,
                          "processSummaryHeadlines": 0,
                          "processSubParts": 0,
                          }
#     Definition["debug"] = 1         #  turn on ALL debug messages
#     Definition["debug"] = 0         #  turn off ALL debug messages


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
    ### Analysis Lists, SampleAnalysis Overrides and other
    ###   analysis related methods
    ###############################################################
    
    ###############################################################
    ### TCV Product and Segment Parts Definition
    ###############################################################
    
    ###############################################################
    ###  Hazards and Additional Hazards
    ###    allowedHazards is used for VTEC records and summary
    ###      headlines
    ###    allowedHeadlines are additional hazards reported in
    ###      certain sections
    ###############################################################

    ###############################################################
    #  CODE
    ###############################################################
    ###  High level flow of formatter
    ###    generateForecast, initializeVariables,
    ###    determineSegments, determineTimeRanges, sampleData,
    ###    createProductDictionary, formatProductDictionary,
    ###    archiveCurrentAdvisory...
    ###############################################################
    
    ###############################################################
    ### Product Dictionary methods for creating the dictionary,
    ###   formatting the dictionary and populating product parts
    ###############################################################
    
    ###############################################################
    ### Area, Zone and Segment related methods
    ###############################################################
    
    ###############################################################
    ### Hazards related methods
    ###############################################################
    
    ###############################################################
    ### Sampling and Statistics related methods
    ###############################################################
    
    ###############################################################
    ### Time related methods
    ###############################################################
    
    ###############################################################
    ### Storm Information and TCP related methods
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
    ### Analysis Lists, SampleAnalysis Overrides and other
    ###   analysis related methods
    
    def _analysisList(self):
        # Sample over 120 hours beginning at current time
        analysisList = [
            # Wind Section
            ("Wind", self.vectorModeratedMax, [6]),
            ("WindGust", self.moderatedMax, [6]),
            ("WindThreat", self.mostSignificantDiscreteValue),
            ("pws34int", self.moderatedMax, [6]),
            ("pws64int", self.moderatedMax, [6]),
            ("pwsD34", self.moderatedMax),
            ("pwsN34", self.moderatedMax),
            ("pwsD64", self.moderatedMax),
            ("pwsN64", self.moderatedMax),
#             ("pwsD34", self.moderatedMax, [0]),
#             ("pwsN34", self.moderatedMax, [0]),
#             ("pwsD64", self.moderatedMax, [0]),
#             ("pwsN64", self.moderatedMax, [0]),
            
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
        # The grids for the Surge Section will be intersected with a special edit area
        analysisList = [
            ("QPF", self.accumSum),
            ]

        return analysisList
    
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
                (floodingRainSection, self._floodingRainSection[segment].sectionParts(segment_vtecRecords_tuple)),
                (tornadoSection, self._tornadoSection[segment].sectionParts(segment_vtecRecords_tuple)),
                'infoSection',
                'endSection'
                ]

        #  The storm surge section should never be inserted into 
        #  "inland" zones, since there will never be a surge impact.
        if segment not in self._inlandAreas():
            partsList.insert(9, 
                    (stormSurgeSection, self._stormSurgeSection[segment].sectionParts(segment_vtecRecords_tuple)))

        return {
            'arguments': segment_vtecRecords_tuple,
            'partsList': partsList
            }

    ###############################################################
    ###  High level flow of formatter

    def generateForecast(self, argDict):
        import pprint 
        pp = pprint.PrettyPrinter()
        self.debug_print("argDict = %s" % (pp.pformat(argDict)), 1)
        
        # Generate Text Phrases for a list of edit areas

        error = self._initializeVariables(argDict)
        if error is not None:
            return error
        
        self._segmentList = self._determineSegments()
        self.debug_print("Segment Information: %s" % (repr(self._segmentList)), 1)
        if len(self._segmentList) == 0:
            return "NO HAZARDS TO REPORT"

        # Determine time ranges
        self._determineTimeRanges(argDict)

        # Sample the data
        self._sampleData(argDict)

        # Create the product dictionary and format it to create the output
        productDict = self._createProductDictionary()
        productOutput = self._formatProductDictionary(productDict)

        self._archiveCurrentAdvisory()
        
        return productOutput
    
    def _initializeVariables(self, argDict):
        # Get variables
        error = self._getVariables(argDict)
        if error is not None:
            return error
    
        self._backupFullStationID = self._fullStationID
        self._argDict = argDict
        
        argDict["definition"] = self._definition
        
        self._initializeTimeVariables(argDict)
        
        self._initializeHazardsTable(argDict)
        
        error = self._initializeStormInformation()
        if error is not None:
            return error
        
        if self._stormName is None or self._stormName.strip() == "":
            return "Could not determine the storm name"
        
        self._windSection = dict()
        self._stormSurgeSection = dict()
        self._floodingRainSection = dict()
        self._tornadoSection = dict()
        
        self._initializeAdvisories()
        
        # Set up the areaDictionary for all to use
        accessor = ModuleAccessor.ModuleAccessor()
        self._areaDict = accessor.variable(self._areaDictionary, "AreaDictionary")
        self._tpc = HLSTCV_Common.TextProductCommon()
        self._tpc.setUp(self._areaDict)
        
        return None
    
    ###############################################################
    ### Product Dictionary methods
    
    def _createProductDictionary(self):
        # Create the product dictionary
        productSegmentGroup = self._groupSegments(self._segmentList)

        productDict = self._initializeProductDict(productSegmentGroup)
        productParts = productSegmentGroup.get('productParts') 
        productDict['productParts'] = productParts
        self._processProductParts(self, productDict, productSegmentGroup, productParts)
        self._wrapUpProductDict(productDict)
        
        return productDict
    
    def _formatProductDictionary(self, productDict):
        legacyFormatter = LegacyFormatter(self)
        product = legacyFormatter.execute(productDict)
#         xmlFormatter = XMLFormatter(self)
#         product = xmlFormatter.execute(productDict)
        
        return product
        
    ######################################################
    #  Populate Product Parts for HLS and TCV
    ######################################################
        
    ################# Product Level

    def _easMessage(self, productDict, productSegmentGroup, arguments=None):
        productDict['easMessage'] = self._easPhrase
    
    ################# Segment Level

    def _setup_segment(self, segmentDict, productSegmentGroup, productSegment):
        segment, vtecRecords = productSegment
        self.debug_print('setup_segment productSegment %s' % (repr(productSegment)), 1)
        # NOTE -- using getVtecRecords to change to milliseconds
        self._segmentVtecRecords = self.getVtecRecords(segment)
            
        # UGCs and Expire Time
        # Assume that the geoType is the same for all hazard events in the segment i.e. area or point
        self._ugcs = [segment]
        self._timeZones = self._tpc.hazardTimeZones(self._ugcs)
        segmentDict['timeZones'] = self._timeZones  

        for tz in self._timeZones:
            if tz not in self._productTimeZones:
                self._productTimeZones.append(tz)
        self._purgeHours = self._purgeTime
        self._expireTime = self._tpc.getExpireTime(
                    self._issueTime_ms, self._purgeHours, self._segmentVtecRecords)
        segmentDict['expireTime'] = self._convertToISO(self._expireTime)

        # CAP Specific Fields        
        segmentDict['status'] = 'Actual'
        
        # Don't show UPG headlines
        nonUPGrecords = []
        for record in self._segmentVtecRecords:
            if record['act'] != "UPG":
                nonUPGrecords.append(record)
        self._summaryHeadlines_value, self._headlines = self._tpc.getHeadlinesAndSections(
                    nonUPGrecords, self._productID, self._issueTime_secs)
    
    def _ugcHeader(self, segmentDict, productSegmentGroup, productSegment):
        segmentDict['ugcCodes'] = self._formatUGC_entries()
        self._ugcHeader_value = self._tpc.formatUGCs(self._ugcs, self._expireTime)
        segmentDict['ugcHeader'] = self._ugcHeader_value
       
    def _vtecRecords(self, segmentDict, productSegmentGroup, productSegment):
       segment, vtecRecords = productSegment
       records = []
       for vtecRecord in vtecRecords:
           vstr = None
           vstr = vtecRecord["vtecstr"]
           
           self.debug_print("vtecRecord = %s" % (repr(vtecRecord)), 1)
           
           #  Post-process some VTEC codes which should not exist
           vstr = vstr.replace(".EXT.", ".CON.")
           vstr = vstr.replace(".EXB.", ".EXA.")
           
           if vtecRecord["phen"] == "SS":
               # SARAH: Temporary? Change the vtec mode for SS hazards to be experimental
               vstr = vstr[0] + 'X' + vstr[2:]
           records.append(vstr)
       segmentDict['vtecRecords'] = records
       
    def _areaList(self, segmentDict, productSegmentGroup, productSegment):
         # Area String        
        segmentDict['areaList'] = self._tpc.formatUGC_names(self._ugcs)

    def _locationsAffected(self, segmentDict, productSegmentGroup, productSegment):
        segment, vtecRecords = productSegment
        import TCVAreaDictionary
        tcv_AreaDictionary = TCVAreaDictionary.TCV_AreaDictionary
        
        segmentDict['locationsAffected'] = []
        if segment in tcv_AreaDictionary:
            segmentDict['locationsAffected'] += tcv_AreaDictionary[segment]["locationsAffected"]
        
    def _fcstConfidence(self, segmentDict, productSegmentGroup, productSegment):
        # SARAH: TODO - Get this from the TCM product potentially? Not included until provided from NHC
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
        
    def _issuanceTimeDate(self, segmentDict, productSegmentGroup, productSegment):
        segmentDict['issuanceTimeDate'] = self._timeLabel
        
    def _summaryHeadlines(self, segmentDict, productSegmentGroup, productSegment):
        segment, vtecRecords = productSegment
        numRecords = len(vtecRecords)
        definitions = []
        hazardsFound = []
        
        for (phenSig, actions, name) in self.allowedHazards():
            for i in range(numRecords):
                vtecRecord = vtecRecords[i]
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
        
    ######################################################
    #  Product Part processing         
    ######################################################
   
    def _processProductParts(self, productGenerator, productDict, productSegmentGroup, productParts):            
        '''
        @param productDict
        @param productSegmentGroup
        @param productParts
        @return product dictionary created from the product parts
        
        Note that this method is called recursively such that a product part is allowed to be
        a set of subParts specified as follows:
          (subPartLabel, list of productParts for each subPart)
        For example, we have
          ('segments', [list of [segment product parts]])

        # Product Dictionary
        #   Contains information for all formats e.g.
        #   partner XML, CAP, and Legacy text 
        '''
        
        
        if type(productParts) is types.DictType:
            arguments = productParts.get('arguments')
            partsList = productParts.get('partsList')
        else:
            partsList = productParts
        
        removedParts = []
        for part in partsList:
            if type(part) is types.TupleType:
                # e.g. subPart == 'segments', subPartsLists == list of parts for each segment
                subPart, subPartsLists = part
                subParts = []
                for subPartsList in subPartsLists:
                    subDict = collections.OrderedDict()
                    self._processProductParts(productGenerator, subDict, productSegmentGroup, subPartsList)
                    subParts.append(subDict)
                # e.g. productDict['segments'] = segment dictionaries
                productDict[subPart] = subParts
            else:
                if part not in self._noOpParts():
                    execString = 'productGenerator._'+part+'(productDict, productSegmentGroup, arguments)'
                    exec execString
                    if part not in productDict:
                        removedParts.append(part)
        
        for part in removedParts:
            partsList.remove(part)
                    
    def _noOpParts(self):
        '''
        These represent product parts that should be skipped when calling product part methods.
        They will be handled automatically by the formatters.
        '''
        return ['CR', 'endProduct', 'endSegment', 'doubleAmpersand']
    
    ######################################################
    #  Product Dictionary -- General product information        
    ######################################################

    def _initializeProductDict(self, productSegmentGroup):
        '''
        Set up the Product Dictionary for the given Product consisting of a 
        group of segments.
        
        Fill in the dictionary information for the product header.
        
        @param productSegmentGroup: holds meta information about the product
        @return initialized product dictionary
      
        ***********
        Example segmented product:
        
           WGUS63 KBOU 080400
           FFABOU

           URGENT - IMMEDIATE BROADCAST REQUESTED
           FLOOD WATCH
           NATIONAL WEATHER SERVICE DENVER CO
           400 AM GMT TUE FEB 8 2011

           Overview Headline
           Overview

        ***********
        Example non-segmented product:
           WGUS63 KBOU 080400
           FFWBOU
        
        '''        
        self._productID = productSegmentGroup.get('productID', 'NNN')
        if self._areaName != '':
            self._areaName = ' FOR ' + self._areaName + '\n'
        self._geoType = productSegmentGroup.get('geoType')
        self._mapType = productSegmentGroup.get('mapType')
        self._productTimeZones = []
        
        # Fill in product dictionary information
        productDict = collections.OrderedDict()
        productDict['productID'] = self._productID
        return productDict
        
    def _wrapUpProductDict(self, productDict):    
        productDict['sentTimeZ'] = self._convertToISO(self._issueTime_ms)
        productDict['sentTimeZ_datetime'] = self._convertToDatetime(self._issueTime_ms)
        productDict['sentTimeLocal'] = self._convertToISO(self._issueTime_ms, local=True)
        productDict['timeZones'] = self._productTimeZones
        return productDict

    ###############################################################
    ### Area, Zone and Segment related methods
    
    def _groupSegments(self, segmentsWithHazards):
        '''
         Group the segments into the products
            return a list of productSegmentGroup dictionaries
        '''
        
        segment_vtecRecords_tuples = []

        # We need to preserve the ordering of the zones based off the zone combiner ordering
        sortedAreas = sorted(self._allAreas(),
                             key=lambda x: segmentsWithHazards.index(x) if x in segmentsWithHazards else 9999)
        for segment in sortedAreas:
            self._initializeSegmentZoneData(segment)

            # We need stats for all zones to be saved in the advisory,
            # regardless of whether or not it has a hazard in it
            windStats, stormSurgeStats, floodingRainStats, tornadoStats = \
                self._getStats(self._argDict, segment, self._editAreaDict, self._timeRangeList)
            
            # Only show zones with hazards in the output
            if segment in segmentsWithHazards:
                vtecRecords = self.getVtecRecords(segment)
                segment_vtecRecords_tuples.append((segment, vtecRecords))
                
                self._windSection[segment] = WindSection(self, segment, windStats)
                self._stormSurgeSection[segment] = StormSurgeSection(self, segment, stormSurgeStats)
                self._floodingRainSection[segment] = FloodingRainSection(self, segment, floodingRainStats)
                self._tornadoSection[segment] = TornadoSection(self, segment, tornadoStats)
        
        productSegmentGroup = { 
                       'productID' : 'TCV',
                       'productName': self._productName,
                       'geoType': 'area',
                       'vtecEngine': self._hazardsTable,
                       'mapType': 'publicZones',
                       'segmented': True,
                       'productParts': self._productParts_TCV(segment_vtecRecords_tuples),
                       }

        return productSegmentGroup
    
    def _makeSegmentEditAreas(self, argDict):
        areasList = self._allAreas()
        #self.debug_print("areaList = %s" % (repr(areasList)), 1)
        editAreas = []
        self._editAreaDict = {}
        for area in areasList:
            self._editAreaDict[area] = area
            editAreas.append((area, area))
        return editAreas
    
    def _determineSegments(self):
        # Get the segments based on hazards "overlaid" with combinations file

        # Get the segments resulting from Hazards
        #self.debug_print("\nRaw Analyzed %s" % (repr(self._hazardsTable.rawAnalyzedTable())), 1)
        hazSegments = self.organizeHazards(self._hazardsTable.rawAnalyzedTable())
        #self.debug_print("\nSegments from HazardsTable organizeHazards %s" % (repr(hazSegments)), 1)

        # Get the forecaster entered combinations
        accessor = ModuleAccessor.ModuleAccessor()
#        self.debug_print("self._defaultEditAreas = %s" % (repr(self._defaultEditAreas)), 1)
        combos = accessor.variable(self._defaultEditAreas, "Combinations")
        if combos is None:
            LogStream.logVerbose("COMBINATION FILE NOT FOUND: " + self._defaultEditAreas)
            return [], None
        #self.debug_print("\nSegments from Zone Combiner = %s" % (repr(combos)), 1)
        # "Overlay" the forecaster-entered combinations onto the segments
        segmentList = self._refineSegments(hazSegments, combos)
        #self.debug_print("\nNew segments = %s" % (repr(segmentList)), 1)
        
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
            #self.debug_print("self._segmentList = %s" % (repr(self._segmentList)), 1)
            segmentMapping = map(self._findSegment, combo)
            #self.debug_print("   segmentMapping = %s" % (repr(segmentMapping)), 1)

            # segmentDict keys will be the hazSegments and
            #   we will gather all the areas of the combos that appear
            #   in each of these hazSegments
            segmentDict = {}
            keyList = []
            for areaName in combo:
                #self.debug_print("       Adding %s" % (areaName), 1)
                key = tuple(segmentMapping[combo.index(areaName)])
                if key == ():  # If no hazard for area, do not include
                    continue
                if key not in keyList:
                    keyList.append(key)
                segmentDict.setdefault(key,[]).append(areaName)
            #self.debug_print("   segmentDict = %s" % (repr(segmentDict)), 1)

            # Keep track of the areas that we are including
            for key in keyList:
                segAreas = segmentDict[key]
                newAreas = newAreas + segAreas
                newSegments.append(segAreas)
        #self.debug_print("   newSegments = %s" % (repr(newSegments)), 1)
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

    def _formatUGC_entries(self):
        ugcCodeList = []
        for ugc in self._ugcs:
            areaDictEntry = self._areaDict.get(ugc)
            if areaDictEntry is None:
                # We are not localized correctly for the hazard
                # So get the first dictionary entry
                self.logger.info('Not Localized for the hazard area -- ugc' + ugc)
                keys = self._areaDict.keys()
                areaDictEntry = self._areaDict.get(keys[0])
            ugcEntry = collections.OrderedDict()
            ugcEntry['state'] = areaDictEntry.get('stateAbbr')
            ugcEntry['type'] = self._getUgcInfo(ugc, 'type')
            ugcEntry['number'] = self._getUgcInfo(ugc, 'number')
            ugcEntry['text'] = ugc
            ugcEntry['subArea'] = ''
            ugcCodeList.append(ugcEntry)
        return ugcCodeList

    def _getUgcInfo(self, ugc, part='type'):
        if part == 'type':
            if ugc[2] == 'C': 
                return 'County'
            else: 
                return 'Zone'
        if part == 'number':
            return ugc[3:]

    ###############################################################
    ### Hazards related methods

    def _hazardDefinition(self, phenSig):
        if phenSig == "HU.W":
            return "A HURRICANE WARNING MEANS HURRICANE WIND CONDITIONS ARE " + \
                   "EXPECTED SOMEWHERE WITHIN THIS AREA AND WITHIN THE NEXT 36 HOURS"
        elif phenSig == "HU.A":
            return "A HURRICANE WATCH MEANS HURRICANE WIND CONDITIONS ARE " + \
                   "POSSIBLE SOMEWHERE WITHIN THIS AREA AND WITHIN THE NEXT 48 HOURS"
        if phenSig == "TR.W":
            return "A TROPICAL STORM WARNING MEANS TROPICAL STORM WIND CONDITIONS ARE " + \
                   "EXPECTED SOMEWHERE WITHIN THIS AREA AND WITHIN THE NEXT 36 HOURS"
        elif phenSig == "TR.A":
            return "A TROPICAL STORM WATCH MEANS TROPICAL STORM WIND CONDITIONS ARE " + \
                   "POSSIBLE SOMEWHERE WITHIN THIS AREA AND WITHIN THE NEXT 48 HOURS"
        elif phenSig == "SS.W":
            return "A STORM SURGE WARNING MEANS LIFE THREATENING INUNDATION LEVELS ARE " + \
                   "EXPECTED SOMEWHERE WITHIN THIS AREA AND WITHIN THE NEXT 36 HOURS"
        elif phenSig == "SS.A":
            return "A STORM SURGE WATCH MEANS LIFE THREATENING INUNDATION LEVELS ARE " + \
                   "POSSIBLE SOMEWHERE WITHIN THIS AREA AND WITHIN THE NEXT 48 HOURS"
        else:
            return ""
    
    ###############################################################
    ### Sampling and Statistics related methods
    
    def _sampleData(self, argDict):
        # Sample the data
        editAreas = self._makeSegmentEditAreas(argDict)
        cwa = self._cwa()
        editAreas.append((cwa, cwa))

        self._sampler = self.getSampler(argDict,
          (self._analysisList(), self._timeRangeList, editAreas))
        
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
        
        import pprint
        pp = pprint.PrettyPrinter()
        self.debug_print("*"*80)
        for index in range(len(timeRangeList)):
            self.debug_print("editArea =" + editArea, 1)
            self.debug_print("timeRange = %s" % (pp.pformat(timeRangeList[index])), 1)
            self.debug_print("statList = %s" % (pp.pformat(statList[index])), 1)
            self.debug_print("-"*40, 1)

        #  These stats are for handling the extra rainfall
        extraRainfallStatList = self.getStatList(self._extraRainfallSampler,
                                             self._extraRainfallAnalysisList(),
                                             self._previousRainfallTR,
                                             editArea)
         
        windStats = WindSectionStats(self, segment, statList, timeRangeList)
        floodingRainStats = FloodingRainSectionStats(self, segment, 
                               statList, timeRangeList, 
                               extraRainfallStatList, self._previousRainfallTR)
        tornadoStats = TornadoSectionStats(self, segment, statList, timeRangeList)
        
        # The surge section needs sampling done with an intersected edit area
        intersectEditArea = "intersect_"+editArea
        intersectStatList = self.getStatList(self._intersectSampler,
                                             self._intersectAnalysisList(),
                                             timeRangeList,
                                             intersectEditArea)
        
        stormSurgeStats = StormSurgeSectionStats(self, segment, intersectStatList, timeRangeList)
                
        return (windStats, stormSurgeStats, floodingRainStats, tornadoStats)
    
    ###############################################################
    ### Time related methods

    def _convertToISO(self, time_ms, local=None):
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
    
    ###############################################################
    ### Advisory related methods
    
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

#         print "*"*80
#         print "advisoryDict = ", advisoryDict
#         print "-"*80 + "\n"
#         print type(advisoryDict["HazardsForHLS"])
        newList = []
        for item in advisoryDict["HazardsForHLS"]:
#             print item, "\n"

            #  Now handle the action code we should not have
            if item["act"] == "EXT":
                item["act"] = "CON"
            elif item["act"] == "EXB":
                item["act"] = "EXA"
                
            newList.append(item)
        advisoryDict["HazardsForHLS"] = newList
         
        try:
            JsonSupport.saveToJson(LocalizationType.CAVE_STATIC,
                                   self._site,
                                   fileName,
                                   advisoryDict)
            
            self.debug_print("SARAH: Wrote file contents for: %s" % (fileName), 1)
            
            self._synchronizeAdvisories()
        except Exception, e:
            self.debug_print("SARAH Save Exception for %s : %s" % (fileName, e), 1)
    
    def _getHazardsForHLS(self):
        hazardTable = self._argDict["hazards"]
        
        
        hazSegments = self.organizeHazards(hazardTable.rawAnalyzedTable())
        self.debug_print("Segments from HazardsTable organizeHazards %s" % 
                         (repr(hazSegments)), 1)
        
        combos = [([self._allAreas()], "AllAreas")]
        
        self.debug_print("Segments from Zone Combiner %s" % (repr(combos)), 1)
        # "Overlay" the forecaster-entered combinations onto the segments
        segmentList = self._refineSegments(hazSegments, combos)
        self.debug_print("SegmentList from refineSegments = %s" % 
                         (repr(segmentList)), 1)
        
        allHazards = []
        for segment in segmentList:
            hazardsList = hazardTable.getHazardList(segment)
            for hazard in hazardsList:
                if hazard['act'] == 'COR':
                    return self._previousAdvisory["HazardsForHLS"]
                else:
                    if hazard['act'] == "UPG":
                        upgPhenSig = hazard['phen'] + "." + hazard['sig']
                        newRecord = self._findNEWAssociatedWithUPG(upgPhenSig, hazardsList)
                        hazard['new_record'] = newRecord
                    
                    allHazards.append(hazard)
        
        return allHazards
    
    def _findNEWAssociatedWithUPG(self, upgPhenSig, vtecRecords):
        import VTECTable
        
        possibleUpgrades = []
        for upgradedTo, upgradedFrom in VTECTable.upgradeHazardsDict:
            if upgPhenSig in upgradedFrom:
                possibleUpgrades.append(upgradedTo)
        
        for record in vtecRecords:
            if record['act'] == "NEW":
                newPhenSig = record['phen'] + "." + record['sig']
                if newPhenSig in possibleUpgrades:
                    return record
        
        return None

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
                                      "Threat to Life and Property: " + threatLevel)

    #  SARAH - this new method will convert the single word threat trend into 
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
        
        self._textProduct.debug_print("SARAH: THREAT DEBUG for %s" % elementName, 1)
        
        self._textProduct.debug_print("SARAH: getThreatTrendValue _currentAdvisory =\n%s" % (repr(self._stats._currentAdvisory)), 1)
        self._textProduct.debug_print("SARAH: getThreatTrendValue _previousAdvisory =\n%s" % (repr(self._stats._previousAdvisory)), 1)
        
        if (self._stats._currentAdvisory is None) or (self._stats._previousAdvisory is None):
            # Only compute a threat trend if we have 2 or more advisories
            return None
        
        currentThreat = self._stats._currentAdvisory[threatKey]
        previousThreat = self._stats._previousAdvisory[threatKey]
        shorterTermTrendDifference = self._threatDifference(currentThreat, previousThreat)
        
        self._textProduct.debug_print("SARAH: currentThreat = %s" % currentThreat, 1)
        self._textProduct.debug_print("SARAH: previousThreat = %s" % previousThreat, 1)
        self._textProduct.debug_print("SARAH: shorterTermTrendDifference = %s" % shorterTermTrendDifference, 1)
        
        previousPreviousThreat = None
        longerTermTrendDifference = None
        if self._stats._previousPreviousAdvisory is not None:
            self._textProduct.debug_print("SARAH: _previousPreviousAdvisory is not None", 1)
            previousPreviousThreat = self._stats._previousPreviousAdvisory[threatKey]
            self._textProduct.debug_print("SARAH: previousPreviousThreat = %s" % previousPreviousThreat, 1)
            longerTermTrendDifference = self._threatDifference(currentThreat, previousPreviousThreat)
            self._textProduct.debug_print("SARAH: longerTermTrendDifference = %s" % longerTermTrendDifference, 1)
        
        threatTrendValue = "NEARLY STEADY"
        self._textProduct.debug_print("magnitudeIncreaseThreshold = %s   forecastKey = '%s'" % (magnitudeIncreaseThreshold, forecastKey), 1)
        if self._isThreatDecreasing(shorterTermTrendDifference, longerTermTrendDifference):
            self._textProduct.debug_print("SARAH: threat is decreasing", 1)
            threatTrendValue = "DECREASING"
        elif self._isThreatIncreasing(shorterTermTrendDifference, longerTermTrendDifference):
            self._textProduct.debug_print("SARAH: threat is increasing", 1)
            threatTrendValue = "INCREASING"
        elif currentThreat == "Extreme" and \
             self._isMagnitudeIncreasing(forecastKey, magnitudeIncreaseThreshold):
            self._textProduct.debug_print("Increasing based on magnitude", 1)
            threatTrendValue = "INCREASING"

        return threatTrendValue
    
    def _threatDifference(self, threat1, threat2):
        threatLevels = self._textProduct.threatKeyOrder()
        self._textProduct.debug_print("SARAH: threat1 index = %s" % threatLevels.index(threat1), 1)
        self._textProduct.debug_print("SARAH: threat2 index = %s" % threatLevels.index(threat2), 1)
        return threatLevels.index(threat1) - threatLevels.index(threat2)
    
    def _isThreatDecreasing(self, shorterTermTrendDifference, longerTermTrendDifference):
        #If the current threat is at least 1 category lower than both previous advisories
        if (shorterTermTrendDifference < 0 and \
            longerTermTrendDifference is not None and \
            longerTermTrendDifference < 0):
            self._textProduct.debug_print("SARAH: the current threat is at least 1 category lower than both previous advisories", 1)
            return True
        #Or if the current threat decreased by more than 1 category
        elif shorterTermTrendDifference < -1:
            self._textProduct.debug_print("SARAH: the current threat decreased by more than 1 category", 1)
            return True
        else:
            self._textProduct.debug_print("SARAH: the current threat is not decreasing", 1)
            return False
        
    def _isThreatIncreasing(self, shorterTermTrendDifference, longerTermTrendDifference):
        #If the current threat is at least 1 category higher than both previous advisories
        if (shorterTermTrendDifference > 0 and \
            longerTermTrendDifference is not None and \
            longerTermTrendDifference > 0):
            self._textProduct.debug_print("SARAH: the current threat is at least 1 category higher than both previous advisories", 1)
            return True
        #Or if the current threat increased by more than 1 category
        elif shorterTermTrendDifference > 1:
            self._textProduct.debug_print("SARAH: the current threat increased by more than 1 category", 1)
            return True
        else:
            self._textProduct.debug_print("SARAH: the current threat is not increasing", 1)
            return False
    
    def _advisoryHasValidKey(self, advisory, key):
        return (advisory is not None) and \
               (advisory.has_key(key)) and \
               (advisory[key] is not None)

    def _isMagnitudeIncreasing(self, forecastKey, threshold):
#         currentValue, previousValue, previousPreviousValue
        self._textProduct.debug_print("SARAH: _isMagnitudeIncreasing", 1)
        self._textProduct.debug_print("SARAH: forecastKey = %s" % forecastKey, 1)
        self._textProduct.debug_print("SARAH: threshold = %s" % threshold, 1)
        
        if self._advisoryHasValidKey(self._stats._currentAdvisory, forecastKey) and \
           self._advisoryHasValidKey(self._stats._previousAdvisory, forecastKey):
            currentValue = self._stats._currentAdvisory[forecastKey]
            previousValue = self._stats._previousAdvisory[forecastKey]
            self._textProduct.debug_print("SARAH: currentValue = %s" % currentValue, 1)
            self._textProduct.debug_print("SARAH: previousValue = %s" % previousValue, 1)
            
            if (currentValue - previousValue) >= threshold:
                self._textProduct.debug_print("SARAH: the current magnitude has increased by more than the threshold since the last advisory", 1)
                return True
            elif self._advisoryHasValidKey(self._stats._previousPreviousAdvisory, forecastKey):
                previousPreviousValue = self._stats._previousPreviousAdvisory[forecastKey]
                self._textProduct.debug_print("SARAH: previousPreviousValue = %s" % previousPreviousValue, 1)
                
                if (currentValue - previousPreviousValue) >= threshold:
                    self._textProduct.debug_print("SARAH: the current magnitude has increased by more than the threshold since the previous previous advisory", 1)
                    return True
                else:
                    self._textProduct.debug_print("SARAH: the current magnitude does not meet the requirements to be considered increasing", 1)
                    return False
            else:
                self._textProduct.debug_print("SARAH: the current magnitude did not increase past threshold and could not look at the previous previous advisory", 1)
                return False
        else:
            self._textProduct.debug_print("SARAH: the current advisory and/or previous advisory did not have key: %s" % forecastKey, 1)
            return False
        
    def _calculateThreatStatementTr(self, onsetHour, endHour, threatTrendValue):
        tr = None
        
        self._textProduct.debug_print("SARAH: onset hour = %s" % (onsetHour), 1)
        self._textProduct.debug_print("SARAH: end hour = %s" % (endHour), 1)
        self._textProduct.debug_print("SHANNON: threatTrendValue = %s" % 
                         (threatTrendValue), 1)
        
        if (onsetHour is not None) and \
           (endHour is not None):
    
            if onsetHour > 36:
                tr = "check plans"
            elif onsetHour > 6:
                tr = "complete preparations"
            elif onsetHour <= 6 and endHour > 0:
                tr = "hunker down"
        elif (threatTrendValue is not None) and (threatTrendValue.upper() in ["DECREASING", "NEARLY STEADY"]):
            tr = "recovery"
        else:
            tr = "nothing to see here"
    
        return tr
    
    def _setThreatStatementsProductParts(self, segmentDict, productSegment, tr):
        
        self._textProduct.debug_print("SHANNON: tr = %s   %s" % 
                          (repr(tr), self._sectionHeaderName), 1)  
#        if tr is not None and self._stats._maxThreat is not None:
        if tr is not None:  
            (planning, action, preparation) = self._getThreatStatements(productSegment,
                                                                        self._sectionHeaderName,
                                                                        self._stats._maxThreat,
                                                                        tr)
    
            self._setProductPartValue(segmentDict, 'threatStatements',
                                      [planning, action, preparation])
        else:
            self._textProduct.debug_print("SHANNON messed up", 1)
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
                         (sectionName, maxThreat, repr(tr)), 1)

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
        self._textProduct.debug_print("SHANNON: segment = %s, elementName = %s, maxThreat = %s" % 
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
        
        self._textProduct.debug_print("SHANNON: zone number = %s, elementName = %s, maxThreat = %s" % 
                         (segment, elementName, maxThreat), 1)
                
        if segment in tcv_AreaDictionary:
            potentialImpactStatements = tcv_AreaDictionary[segment]["potentialImpactsStatements"]
                
        # Check for any overrides
        try:
            statements = potentialImpactStatements[elementName][maxThreat]
        except KeyError:
            pass
        
        return statements
    
    def _preparationStatement(self, severityString):
        preparationStatement = ""
        if severityString == "DEVASTATING" or severityString == "EXTENSIVE IMPACTS":
            preparationStatement += "AGGRESSIVE "
        
        preparationStatement += "PREPARATIONS SHOULD BE MADE FOR CHANCE OF "
        
        if severityString == "DEVASTATING":
            preparationStatement += "DEVASTATING TO CATASTROPHIC"
        elif severityString == "EXTENSIVE IMPACTS":
            preparationStatement += "EXTENSIVE"
        elif severityString == "SIGNIFICANT":
            preparationStatement += "SIGNIFICANT"
        elif severityString == "LIMITED":
            preparationStatement += "LIMITED"
        
        preparationStatement += " IMPACTS BASED ON LATEST THREAT"
        
        return preparationStatement

class WindSection(SectionCommon):
    def __init__(self, textProduct, segment, stats):
        SectionCommon.__init__(self, textProduct, segment, "Wind")
        self._sectionName = 'windSection[\'' + segment + '\']'
        self._stats = stats
        
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
            windText = "PEAK WIND FORECAST: "
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
                
#                 #  SARAH - we want to round the wind gust to the nearest 5 kt
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
        self._threatTrendValue = \
            self._getThreatTrendValue("Wind",
                                      magnitudeIncreaseThreshold=self._textProduct.mphToKt(15))
        
        if self._threatTrendValue is not None:
            #  Convert the threat trend to a sentence
            threatTrendSentence = \
                self._getThreatTrendSentence("wind", self._threatTrendValue)

            self._setProductPartValue(segmentDict, 'threatTrend',
                                      threatTrendSentence)
    
    def _threatStatements(self, segmentDict, productSegmentGroup, productSegment):
        windTr = self._calculateThreatStatementTr(self._stats._onset34Hour,
                                                  self._stats._end34Hour,
                                                  self._threatTrendValue)
#         self._textProduct.debug_print("MATT: in _threatStatements tr = %s" % (repr(windTr))
        
        if not hasattr(self._textProduct, "_windThreatStatementsTr"):
            self._textProduct._windThreatStatementsTr = dict()
            
        self._textProduct._windThreatStatementsTr[self._segment] = windTr
        
        self._setThreatStatementsProductParts(segmentDict, productSegment, 
                                              windTr)
        
#     def _impactsSubsection(self, segmentDict, productSegmentGroup, productSegment):
#         subsectionDict = collections.OrderedDict()
#         self._potentialImpactsSummary(subsectionDict, productSegmentGroup, productSegment)
#         self._potentialImpactsStatements(subsectionDict, productSegmentGroup, productSegment)
#         if len(subsectionDict) > 0:
#             self._setProductPartValue(segmentDict, 'impactsSubsection', subsectionDict)

    #  SARAH - modified to not include wind impacts during the "recovery" and 
    #  "nothing to see here" phases of the tropical cyclone event
    def _impactsSubsection(self, segmentDict, productSegmentGroup, productSegment):

        #  Compute time range to onset
        try:
            windTr = self._textProduct._windThreatStatementsTr[self._segment]
        except:
            windTr = self._calculateThreatStatementTr(self._stats._onset34Hour,
                                                      self._stats._end34Hour,
                                                      self._threatTrendValue)
        
        subsectionDict = collections.OrderedDict()
        self._potentialImpactsSummary(subsectionDict, productSegmentGroup, productSegment)
        self._potentialImpactsStatements(subsectionDict, productSegmentGroup, productSegment)
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'impactsSubsection', subsectionDict)

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
                
                #  SARAH - we were getting really weird values of peak surge
                #  (e.g. "UP TO 1.70000004768 FEET").  This fix will round up 
                #  to the nearest integer value
#                 words = "Up to " + str(max) + " feet above ground"
                words = "Up to " + str(int(max + 0.5)) + " feet above ground"
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
        self._threatTrendValue = self._getThreatTrendValue("StormSurge", magnitudeIncreaseThreshold=4)

        if self._threatTrendValue is not None:
            #  Convert the threat trend to a sentence
            threatTrendSentence = \
                self._getThreatTrendSentence("storm surge", self._threatTrendValue)

            self._setProductPartValue(segmentDict, 'threatTrend',
                                      threatTrendSentence)
    
    def _threatStatements(self, segmentDict, productSegmentGroup, productSegment):
        self._textProduct.debug_print("SARAH: Surge Threat Statements", 1)
        surgeTr = self._calculateThreatStatementTr(self._stats._onsetSurgeHour,
                                                   self._stats._endSurgeHour,
                                                   self._threatTrendValue)
        
        self._setThreatStatementsProductParts(segmentDict, productSegment, 
                                              surgeTr)
        
    def _impactsSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._potentialImpactsSummary(subsectionDict, productSegmentGroup, productSegment)
        if self._textProduct._PopulateSurge:
            self._potentialImpactsStatements(subsectionDict, productSegmentGroup, productSegment)
            
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'impactsSubsection', subsectionDict)
    
    def _potentialImpactsSummary(self, segmentDict, productSegmentGroup, productSegment):
        if not self._textProduct._PopulateSurge:

        #  SARAH - We do not want the '(For plausible worst case)' in the text
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
        summary = ""    # was "No flood watch is in effect"
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
            words = self._rainRange(int(math.ceil(self._stats._sumAccum)))
            
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
        self._threatTrendValue = self._getThreatTrendValue("FloodingRain", magnitudeIncreaseThreshold=4)

        if self._threatTrendValue is not None:
            #  Convert the threat trend to a sentence
            threatTrendSentence = \
                self._getThreatTrendSentence("flooding rain", self._threatTrendValue)

            self._setProductPartValue(segmentDict, 'threatTrend',
                                      threatTrendSentence)
    
    def _threatStatements(self, segmentDict, productSegmentGroup, productSegment):
        tr = self._textProduct._windThreatStatementsTr[self._segment]
        
        self._setThreatStatementsProductParts(segmentDict, productSegment, tr)
        
    def _impactsSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._potentialImpactsSummary(subsectionDict, productSegmentGroup, productSegment)
        self._potentialImpactsStatements(subsectionDict, productSegmentGroup, productSegment)
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'impactsSubsection', subsectionDict)

class TornadoSection(SectionCommon):
    def __init__(self, textProduct, segment, stats):
        SectionCommon.__init__(self, textProduct, segment, "Tornado")
        self._sectionName = 'tornadoSection[\'' + segment + '\']'
        self._stats = stats
        
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
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'forecastSubsection', subsectionDict)
    
    def _latestForecastSummary(self, segmentDict, productSegmentGroup, productSegment):
        summary = "There is no Tornado Watch in effect"
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

    def _threatSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._lifePropertyThreatSummary(subsectionDict, productSegmentGroup, productSegment)
        self._threatTrend(subsectionDict, productSegmentGroup, productSegment)
        self._threatStatements(subsectionDict, productSegmentGroup, productSegment)
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'threatSubsection', subsectionDict)
    
    def _threatTrend(self, segmentDict, productSegmentGroup, productSegment):
        self._threatTrendValue = self._getThreatTrendValue("Tornado", 
                                                magnitudeIncreaseThreshold=None)
        
        if self._threatTrendValue is not None:
            #  Convert the threat trend to a sentence
            threatTrendSentence = \
                self._getThreatTrendSentence("tornado", self._threatTrendValue)

            self._setProductPartValue(segmentDict, 'threatTrend',
                                      threatTrendSentence)
        
    def _threatStatements(self, segmentDict, productSegmentGroup, productSegment):
        tr = self._textProduct._windThreatStatementsTr[self._segment]

        self._setThreatStatementsProductParts(segmentDict, productSegment, tr)
    
    def _impactsSubsection(self, segmentDict, productSegmentGroup, productSegment):
        subsectionDict = collections.OrderedDict()
        self._potentialImpactsSummary(subsectionDict, productSegmentGroup, productSegment)
        self._potentialImpactsStatements(subsectionDict, productSegmentGroup, productSegment)
        if len(subsectionDict) > 0:
            self._setProductPartValue(segmentDict, 'impactsSubsection', subsectionDict)


###############################################################
### TCV Statistics Classes
    
class SectionCommonStats():
    def __init__(self, textProduct, segment):
        self._textProduct = textProduct
        self._segment = segment
        
        self._initializeAdvisories()
        
        self._maxThreat = None
    
    def _initializeAdvisories(self):
        self._currentAdvisory = self._textProduct._currentAdvisory['ZoneData'][self._segment]
        
        self._previousAdvisory = None
#         self._textProduct.debug_print("MATT textProduct._previousAdvisory = '%s'" % (textProduct._previousAdvisory))
        if self._textProduct._previousAdvisory is not None:
            if self._textProduct._previousAdvisory['ZoneData'].has_key(self._segment):
                self._previousAdvisory = self._textProduct._previousAdvisory['ZoneData'][self._segment]
            
#         self._textProduct.debug_print("MATT textProduct._previousPreviousAdvisory = '%s'" % \
#             (textProduct._previousPreviousAdvisory))
        self._previousPreviousAdvisory = None
        if self._textProduct._previousPreviousAdvisory is not None:
            self._previousPreviousAdvisory = self._textProduct._previousPreviousAdvisory['ZoneData'][self._segment]
    
    def _updateThreatStats(self, tr, statDict, threatGridName):
        self._textProduct.debug_print("statDict = '%s'" % (repr(statDict)), 1)
        threatLevel = self._textProduct.getStats(statDict, threatGridName)
        if threatLevel is not None:
            threatLevels = self._textProduct.threatKeyOrder()
            self._textProduct.debug_print("SARAH: updateThreatStats for %s" % (threatGridName), 1)
            self._textProduct.debug_print("SARAH: threatLevel = %s" % (threatLevel), 1)
            self._textProduct.debug_print("SARAH: maxThreat = %s" % (self._maxThreat), 1)
            if self._maxThreat is None or \
               threatLevels.index(threatLevel) > threatLevels.index(self._maxThreat):
                self._textProduct.debug_print("SARAH: updating max threat to = %s" % (threatLevel), 1)
                self._maxThreat = threatLevel
    
    def _calculateHourOffset(self, targetTime):
        seconds = targetTime.unixTime() - self._textProduct._issueTime_secs
        hour = int(round(seconds/60/60))
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

        print "*"*90        
        print "Setting wind stats for %s" % (segment)
        
        self._setStats(statList, timeRangeList)
    
    class PwsXXintStats():
        max = None
        onsetHour = None
    
    class PwsTXXStats():
        firstRun = True
        dropFirstGridType = None
        droppedFirstGrid = False
        periodWithFirstCorrectGrid = None
        onsetTime = None
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
            
            for periodIndex, periodTr in enumerate(self._textProduct._periodList):
                if (periodIndex == 0) and (tr.startTime().unixTime() < periodTr.startTime().unixTime()):
                    # If the tr is before the first period, use the first period
                    currentPeriod = periodIndex
                    break
                elif periodTr.contains(tr.startTime()):
                    currentPeriod = periodIndex
                    break

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
         
        self._textProduct.debug_print("SARAH: Tropical Storm Window:", 1)
        self._windowTS = self._createWindow("Tropical Storm",
                                            self._onset34Hour,
                                            self._end34Hour)
         
        #Hurricane
        onsetEndInfo = self._computeWindOnsetAndEnd(wind64timeInfo, pws64intStats, pwsT64Stats)
        self._onset64Hour = onsetEndInfo.onsetHour
        self._end64Hour = onsetEndInfo.endHour
         
        self._textProduct.debug_print("SARAH: Hurricane Window:", 1)
        self._windowHU = self._createWindow("Hurricane",
                                            self._onset64Hour,
                                            self._end64Hour)
         
        self._currentAdvisory["WindThreat"] = self._maxThreat
        self._currentAdvisory["WindForecast"] = self._maxWind
        
    def _updateStatsForPwsXXint(self, tr, statDict, gridName, pwsXXintStats):
        pwsXXint = self._textProduct._getStatValue(statDict, gridName, "Max")
        
        if pwsXXint is not None:
            if pwsXXintStats.max is None or pwsXXint > pwsXXintStats.max:
                pwsXXintStats.max = pwsXXint
                pwsXXintStats.onsetHour = self._calculateHourOffset(tr.startTime())
                
                self._textProduct.debug_print("SARAH: Window Debug: pwsXXintStats gridName = %s" % (gridName), 1)
                self._textProduct.debug_print("SARAH: Window Debug: pwsXXintStats onsetHour = %s" % (pwsXXintStats.onsetHour), 1)
    
    def _updateStatsForPwsTXX(self, tr, statDict, dayGridName, nightGridName, pwsTXXStats, period):
        pwsDXX = self._textProduct._getStatValue(statDict, dayGridName, "Max")
        pwsNXX = self._textProduct._getStatValue(statDict, nightGridName, "Max")
        
        if pwsTXXStats.firstRun:
            self._textProduct.debug_print("SARAH: first run for _updateStatsForPwsTXX!", 1)
            self._textProduct.debug_print("SARAH: grids: %s %s" % (dayGridName, nightGridName), 1)
            pwsTXXStats.firstRun = False
            localtime = time.localtime(self._textProduct._issueTime_secs)
            self._textProduct.debug_print("SARAH: localtime = %s" % (localtime), 1)

            if localtime.tm_hour >= 15: # 3PM to midnight
                self._textProduct.debug_print("SARAH: between 3PM and midnight!", 1)
                pwsTXXStats.dropFirstGridType = "DAY"
                self._textProduct.debug_print("SARAH: need to drop the day grid(s) if they come first", 1)
            elif localtime.tm_hour >= 3 and localtime.tm_hour < 12: # 3AM to noon
                self._textProduct.debug_print("SARAH: between 3AM and noon!", 1)
                pwsTXXStats.dropFirstGridType = "NIGHT"
                self._textProduct.debug_print("SARAH: need to drop the night grid(s) if they come first", 1)
            else:
                self._textProduct.debug_print("SARAH: not dropping any grids!", 1)

        maxPws = None
        self._textProduct.debug_print("MATT %s pwsDXX = %s    pwsNXX = %s " % 
                         (repr(tr),pwsDXX, pwsNXX), 1)

        if pwsDXX is not None:
            self._textProduct.debug_print("SARAH: Window Debug: pwsTXXStats DAY", 1)
            
            if pwsTXXStats.dropFirstGridType == "DAY":
                self._textProduct.debug_print("SARAH: Window Debug: dropping a day grid", 1)
                self._textProduct.debug_print("SARAH: Window Debug: tr = %s, period = %s" % (tr, period), 1)
                pwsTXXStats.droppedFirstGrid = True
                return
            elif pwsTXXStats.dropFirstGridType == "NIGHT":
                # We dropped all the necessary grids now that we found a day grid so stop dropping
                pwsTXXStats.dropFirstGridType = None
                pwsTXXStats.periodWithFirstCorrectGrid = period
                self._textProduct.debug_print("SARAH: Window Debug: found day grid; done dropping night grids", 1)
                self._textProduct.debug_print("SARAH: Window Debug: tr = %s, period = %s" % (tr, period), 1)
            
            maxPws = pwsDXX

        elif pwsNXX is not None:
            self._textProduct.debug_print("SARAH: Window Debug: pwsTXXStats NIGHT", 1)
           
            if pwsTXXStats.dropFirstGridType == "NIGHT":
                self._textProduct.debug_print("SARAH: Window Debug: dropping a night grid", 1)
                self._textProduct.debug_print("SARAH: Window Debug: tr = %s, period = %s" % (tr, period), 1)
                pwsTXXStats.droppedFirstGrid = True
                return
            elif pwsTXXStats.dropFirstGridType == "DAY":
                # We dropped all the necessary grids now that we found a night grid so stop dropping
                pwsTXXStats.dropFirstGridType = None
                pwsTXXStats.periodWithFirstCorrectGrid = period
                self._textProduct.debug_print("SARAH: Window Debug: found night grid; done dropping day grids", 1)
                self._textProduct.debug_print("SARAH: Window Debug: tr = %s, period = %s" % (tr, period), 1)

            maxPws = pwsNXX
        
        threshold34index = 0
        threshold64index = 1
        if maxPws is not None:
            # Don't shift if the period with the first correct grid is period 0
            if pwsTXXStats.droppedFirstGrid and pwsTXXStats.periodWithFirstCorrectGrid != 0:
                period = period - 1 # We dropped the first grid so we are off-by-one
                self._textProduct.debug_print("SARAH: shifting period back 1...new period = %s" % 
                                 (period), 1)

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
                
            if maxPws > threshold:
                pwsTXXStats.onsetTime = tr.startTime()
                pwsTXXStats.endTime = tr.endTime()
                
                self._textProduct.debug_print("SARAH: Window Debug: pwsTXXStats dayGridName = %s" % (dayGridName), 1)
                self._textProduct.debug_print("SARAH: Window Debug: pwsTXXStats nightGridName = %s" % (nightGridName), 1)
                self._textProduct.debug_print("SARAH: Window Debug: pwsTXXStats original tr = %s" % (repr(tr)), 1)
                self._textProduct.debug_print("SARAH: Window Debug: pwsTXXStats maxPws = %s" %(repr(maxPws)), 1)
                self._textProduct.debug_print("SARAH: Window Debug: pwsTXXStats onsetTime = %s" % (repr(pwsTXXStats.onsetTime)), 1)
                self._textProduct.debug_print("SARAH: Window Debug: pwsTXXStats endTime = %s" % (repr(pwsTXXStats.endTime)), 1)
    
    def _updateWindTimeInfo(self, tr, wind, timeInfo, speed):
        if wind >= speed:
            timeInfo.endHour = self._calculateHourOffset(tr.endTime())
            
            self._textProduct.debug_print("SARAH: Window Debug: In _updateWindTimeInfo", 1)
            self._textProduct.debug_print("SARAH: Window Debug: timeInfo speed = %s" % (speed), 1)
            self._textProduct.debug_print("SARAH: Window Debug: timeInfo maxWind = %s" % (self._maxWind), 1)
            self._textProduct.debug_print("SARAH: Window Debug: timeInfo tr = %s" % (repr(tr)), 1)
            self._textProduct.debug_print("SARAH: Window Debug: timeInfo endHour = %s" % (timeInfo.endHour), 1)
            
            if timeInfo.onsetHour is None:
                timeInfo.onsetHour = self._calculateHourOffset(tr.startTime())
                
                self._textProduct.debug_print("SARAH: Window Debug: onsetHour was None", 1)
                self._textProduct.debug_print("SARAH: Window Debug: timeInfo speed = %s" % (speed), 1)
                self._textProduct.debug_print("SARAH: Window Debug: timeInfo maxWind = %s" % (self._maxWind), 1)
                self._textProduct.debug_print("SARAH: Window Debug: timeInfo tr = %s" % (repr(tr)), 1)
                self._textProduct.debug_print("SARAH: Window Debug: timeInfo onsetHour = %s" % (timeInfo.onsetHour), 1)
    
    def _computeWindOnsetAndEnd(self, windTimeInfo, pwsXXintStats, pwsTXXStats):
        onsetEndInfo = self.TimeInfo()
 
        self._textProduct.debug_print("SARAH: Window Debug: In _computeWindOnsetAndEnd", 1)
        self._textProduct.debug_print("SARAH: Window Debug: windTimeInfo.onsetHour = %s" % (windTimeInfo.onsetHour), 1)
        self._textProduct.debug_print("SARAH: Window Debug: pwsXXintStats.onsetHour = %s" % (pwsXXintStats.onsetHour), 1)
        self._textProduct.debug_print("SARAH: Window Debug: windTimeInfo.endHour = %s" % (windTimeInfo.endHour), 1)
        self._textProduct.debug_print("SARAH: Window Debug: pwsTXXStats.endTime = %s" % (pwsTXXStats.endTime), 1)
        
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
            startUnixTime = pwsTXXStats.onsetTime.unixTime()
            startLocalTime = time.localtime(startUnixTime)
            
            if endLocalTime.tm_hour >= 6 and endLocalTime.tm_hour < 18:
                configuredTime = absTimeYMD(endLocalTime.tm_year,
                                            endLocalTime.tm_mon,
                                            endLocalTime.tm_mday,
                                            self._textProduct.DAY())
            elif endLocalTime.tm_hour < 6:
                # Use 6 PM of previous day
                configuredTime = absTimeYMD(startLocalTime.tm_year,
                                            startLocalTime.tm_mon,
                                            startLocalTime.tm_mday,
                                            self._textProduct.NIGHT())
            else:
                configuredTime = absTimeYMD(endLocalTime.tm_year,
                                            endLocalTime.tm_mon,
                                            endLocalTime.tm_mday,
                                            self._textProduct.NIGHT())
            
            probEndHour = self._calculateHourOffset(configuredTime)
            onsetEndInfo.endHour = int(round(self._textProduct.average(windTimeInfo.endHour, probEndHour)))
            self._textProduct.debug_print("endHour = " + str(onsetEndInfo.endHour), 1)
        else:
            self._textProduct.debug_print("ERROR: endHour for pwsTXXStats is None. Check the grids.", 1)
            return onsetEndInfo
        
        return onsetEndInfo
    
    def _createWindow(self, windowName, onsetHour, endHour):
        window = "Window for " + windowName + " force winds: "
        self._textProduct.debug_print("SARAH: In _createWindow", 1)
        self._textProduct.debug_print("SARAH: window stats:", 1)
        self._textProduct.debug_print("SARAH: onsetHour = %s" % (onsetHour), 1)
        self._textProduct.debug_print("SARAH: endHour = %s" % (endHour), 1)
        
        if onsetHour is None:
            # SARAH - we do not want a statement of a non-existent window
#             window += "None"
            window = None
        else:
            startTime = AbsTime(self._textProduct._issueTime_secs + onsetHour*60*60)
            if endHour is not None:
                endTime = AbsTime(self._textProduct._issueTime_secs + endHour*60*60)
                windowPeriod = self._textProduct.makeTimeRange(startTime, endTime)
            else:
                windowPeriod = self._textProduct.makeTimeRange(startTime, startTime + 1)
            self._textProduct.debug_print("SARAH: window period = %s" % (windowPeriod), 1)
            
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
        
        self._textProduct.debug_print("*"*100, 1)
        self._textProduct.debug_print("MATT phishStartTime = %s   phishEndTime  = %s   possibleStop = %d" % 
                                      (str(phishStartTime), str(phishEndTime), possibleStop), 1)
        
        for period in range(len(statList)):
            tr, _ = timeRangeList[period]
            statDict = statList[period]
        
            phishPeak = self._textProduct._getStatValue(statDict, "InundationMax", "Max")
            if phishPeak is not None:
                if self._inundationMax is None or phishPeak > self._inundationMax:
                    self._inundationMax = phishPeak
                    
            curPhish = self._textProduct._getStatValue(statDict, "InundationTiming", "Max")
            self._textProduct.debug_print("MATT tr = %s" % (repr(tr)), 1)
            self._textProduct.debug_print("MATT curPhish = '%s'    possibleStop = %d" % 
                                          (str(curPhish), possibleStop), 1)
            self._textProduct.debug_print("MATT phishStartTime = %s   phishEndTime  = %s" % 
                                          (str(phishStartTime), str(phishEndTime)), 1)
            
            if curPhish is not None and possibleStop != 2:
                if curPhish >= 1:
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
            
            self._textProduct.debug_print("MATT surge startTime = %s   self._onsetSurgeHour = %s " %
                                          (repr(startTime), self._onsetSurgeHour), 1)
            if phishEndTime is not None:
                self._endSurgeHour = self._calculateHourOffset(phishEndTime)
                endTime = AbsTime(self._textProduct._issueTime_secs + self._endSurgeHour*60*60)
                windowPeriod = self._textProduct.makeTimeRange(startTime, endTime)
            else:
                windowPeriod = self._textProduct.makeTimeRange(startTime, startTime + 1)
            self._textProduct.debug_print("SARAH: surge window period = %s" % (windowPeriod), 1)
            
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
        
            stats = self._textProduct.getStats(statDict, "QPF")
            if stats is not None:
                for (value, tr) in stats:
                    
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
        
            prevStats = self._textProduct.getStats(prevStatDict, "QPF")
            print "prevStats = ", prevStats
            if prevStats is not None:
                self._prevAccum += prevStats
            else:
                self._prevAccum = 0.00
                    
        if self._prevAccum is not None and self._prevAccum >= 0.10:
            # Round so that we don't end up with stats like 4.03143835067749
            self._currentAdvisory["PreviousRainfall"] = \
                    self._textProduct.round(self._prevAccum, "Nearest", 0.1)
        else:
            #  Otherwise, do not consider this sgnificant rainfall
            self._currentAdvisory["PreviousRainfall"] = 0.00


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



from xml.etree.ElementTree import Element, SubElement, tostring, dump
import xml.dom.minidom as minidom
import re
class XMLFormatter():
    def __init__(self, textProduct):
        self._textProduct = textProduct
    
    def execute(self, productDict):
        xml = Element('product')
        self.dictionary(xml, productDict)
        self._textProduct.debug_print("SARAH: XML = %s" % (xml), 1)
        self._textProduct.debug_print("SARAH: XML dump = %s", dump(xml), 1)
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
                    'ugcCodes',
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
        
        self._textProduct.debug_print("SARAH: sectionKey = %s" % (sectionKey), 1)
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
                        self._textProduct.debug_print("SARAH: skipping '%s' in XML" % (key), 1)
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
                self._textProduct.debug_print("SARAH: info key = '%s'" % (key), 1)
                self._textProduct.debug_print("SARAH: value = %s" % (data), 1)
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
        self._textProduct.debug_print("productParts = %s" % (repr(productParts)), 1)
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

    def formatIssueTime(self):  
        text = ''  
        sentTimeZ = self._tpc.getVal(self.productDict, 'sentTimeZ_datetime')
        timeZones = self._tpc.getVal(self.productDict, 'timeZones')
        for timeZone in timeZones:
            text += self._tpc.formatDatetime(sentTimeZ, '%I%M %p %Z %a %e %b %Y', timeZone) + '\n'
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


