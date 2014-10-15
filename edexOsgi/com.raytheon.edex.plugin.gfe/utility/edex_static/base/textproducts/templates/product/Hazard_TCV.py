import GenericHazards
import string, time, os, types, copy, LogStream, collections
import ModuleAccessor, SampleAnalysis, EditAreaUtils
import math


from AbsTime import *
from StartupDialog import IFPDialog as Dialog
from LockingFile import File
AWIPS_ENVIRON = "AWIPS2"

import Hazard_HLSTCV_Common

class TextProduct(Hazard_HLSTCV_Common.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition["displayName"]   = "None"
    Definition["outputFile"]    = "{prddir}/TEXT/TCV.txt"
    Definition["database"]      =  "Official"  # Source database
    Definition["debug"]         =  1
    Definition["mapNameForCombinations"] = "Zones_<site>"
    Definition["defaultEditAreas"] = "EditAreas_PublicMarine_<site>"
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

    def __init__(self):
        Hazard_HLSTCV_Common.TextProduct.__init__(self)

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
    ###  Hazards and Additional Hazards
    ###    allowedHazards is used for VTEC records and summary
    ###      headlines
    ###    allowedHeadlines are additional hazards reported in
    ###      certain sections
    ###############################################################
    
    ###############################################################
    ### TCV Product and Segment Parts Definition
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
    ### Product Dictionary methods
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
                'endProduct',
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
        # Generate Text Phrases for a list of edit areas

        error = self._initializeVariables(argDict)
        if error is not None:
            return error

        self._segmentList = self._determineSegments()
        print "Segment Information: ", self._segmentList, "\n\n"
        if len(self._segmentList) == 0:
            return "NO HAZARDS TO REPORT"

        # Determine time ranges
        self._determineTimeRanges(argDict)

        # Sample the data
        self._sampleTCVData(argDict)

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
        self._tpc = Hazard_HLSTCV_Common.TextProductCommon()
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
        print 'setup_segment productSegment', productSegment
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
                    self._issueTime, self._purgeHours, self._segmentVtecRecords)
        segmentDict['expireTime'] = self._convertToISO(self._expireTime)

        # CAP Specific Fields        
        segmentDict['status'] = 'Actual'
        self._summaryHeadlines_value, self._headlines = self._tpc.getHeadlinesAndSections(
                    self._segmentVtecRecords, self._productID, self._issueTime_secs)
    
    def _ugcHeader(self, segmentDict, productSegmentGroup, productSegment):
        segmentDict['ugcCodes'] = self._formatUGC_entries()
        self._ugcHeader_value = self._tpc.formatUGCs(self._ugcs, self._expireTime)
        segmentDict['ugcHeader'] = self._ugcHeader_value
       
    def _vtecRecords(self, segmentDict, productSegmentGroup, productSegment):
       segment, vtecRecords = productSegment
       records = []
       for vtecRecord in vtecRecords:
           print "SARAH: vtecRecord dict:", vtecRecord
           vstr = None
           vstr = vtecRecord["vtecstr"]
           
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
            print "SARAH: Removing part =", part
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
        productDict['sentTimeZ'] = self._convertToISO(self._issueTime)
        productDict['sentTimeZ_datetime'] = self._convertToDatetime(self._issueTime)
        productDict['sentTimeLocal'] = self._convertToISO(self._issueTime, local=True)
        productDict['timeZones'] = self._productTimeZones
        return productDict

    ###############################################################
    ### Area, Zone and Segment related methods
    
    def _groupSegments(self, segments):
        '''
         Group the segments into the products
            return a list of productSegmentGroup dictionaries
        '''
        
        segment_vtecRecords_tuples = []
        for segment in segments:
            vtecRecords = self.getVtecRecords(segment)
            segment_vtecRecords_tuples.append((segment, vtecRecords))

            self._initializeSegmentZoneData(segment)

            windStats, stormSurgeStats, floodingRainStats, tornadoStats = \
                self._getTCVStats(self._argDict, segment, self._editAreaDict, self._timeRangeList)
            
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
    ### Time related methods
    
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
        print "   currentTime", currentTime  
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
        print "Result", result
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

import Tkinter
class Overview_Dialog(Hazard_HLSTCV_Common.Common_Dialog):
    def __init__(self, parent, title, infoDict=None):
        Hazard_HLSTCV_Common.Common_Dialog.__init__(self, parent, title, infoDict)

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
            if button == "Next":
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
        
        print "SARAH: getThreatTrendValue _currentAdvisory =", self._stats._currentAdvisory
        print "SARAH: getThreatTrendValue _previousAdvisory =", self._stats._previousAdvisory
        
        if (self._stats._currentAdvisory is None) or (self._stats._previousAdvisory is None):
            # Only compute a threat trend if we have 2 or more advisories
            return None
        
        currentThreat = self._stats._currentAdvisory[threatKey]
        previousThreat = self._stats._previousAdvisory[threatKey]
        shorterTermTrendDifference = self._threatDifference(currentThreat, previousThreat)
        
        print "SARAH: shorterTermTrendDifference =", shorterTermTrendDifference
        
        previousPreviousThreat = None
        longerTermTrendDifference = None
        if self._stats._previousPreviousAdvisory is not None:
            previousPreviousThreat = self._stats._previousPreviousAdvisory[threatKey]
            longerTermTrendDifference = self._threatDifference(currentThreat, previousPreviousThreat)
        
        threatTrendValue = "NEARLY STEADY"
        if self._isThreatDecreasing(shorterTermTrendDifference, longerTermTrendDifference):
            threatTrendValue = "DECREASING"
        elif self._isThreatIncreasing(shorterTermTrendDifference, longerTermTrendDifference):
            threatTrendValue = "INCREASING"
        elif currentThreat == "Extreme" and \
             self._advisoriesHaveKey(forecastKey) and \
             self._isMagnitudeIncreasing(self._stats._currentAdvisory[forecastKey],
                                         self._stats._previousAdvisory[forecastKey],
                                         self._stats._previousPreviousAdvisory[forecastKey],
                                         magnitudeIncreaseThreshold):
            threatTrendValue = "INCREASING"

        return threatTrendValue
    
    def _threatDifference(self, threat1, threat2):
        threatLevels = self._textProduct.threatKeyOrder()
        return threatLevels.index(threat1) - threatLevels.index(threat2)
    
    def _isThreatDecreasing(self, shorterTermTrendDifference, longerTermTrendDifference):
        #If the current threat is at least 1 category lower than both previous advisories
        if (shorterTermTrendDifference < 0 and \
            longerTermTrendDifference is not None and \
            longerTermTrendDifference < 0):
            return True
        #Or if the current threat decreased by more than 1 category
        elif shorterTermTrendDifference < -1:
            return True
        else:
            return False
        
    def _isThreatIncreasing(self, shorterTermTrendDifference, longerTermTrendDifference):
        #If the current threat is at least 1 category higher than both previous advisories
        if (shorterTermTrendDifference > 0 and \
            longerTermTrendDifference is not None and \
            longerTermTrendDifference > 0):
            return True
        #Or if the current threat increased by more than 1 category
        elif shorterTermTrendDifference > 1:
            return True
        else:
            return False
    
    def _advisoriesHaveKey(self, key):
        return (self._stats._currentAdvisory.has_key(key) and
                self._stats._currentAdvisory[key] is not None) and \
               (self._stats._previousAdvisory is not None and 
                self._stats._previousAdvisory[key] is not None) and \
               (self._stats._previousPreviousAdvisory is not None and 
                self._stats._previousPreviousAdvisory[key] is not None)

    def _isMagnitudeIncreasing(self, currentValue, previousValue, previousPreviousValue, threshold):
        if (currentValue > previousValue and currentValue > previousPreviousValue) or \
           (currentValue - previousValue) >= threshold:
            return True
        else:
            return False
        
    def _calculateThreatStatementTr(self, onsetHour, endHour, threatTrendValue):
        tr = None
        
        print "SARAH: onset hour =", onsetHour
        print "SARAH: end hour =", endHour
        print "SARAH: threatTrendValue =", threatTrendValue
        
        if (onsetHour is not None) and \
           (endHour is not None):
    
            if onsetHour > 36:
                tr = "check plans"
            elif onsetHour > 6:
                tr = "complete preparations"
            elif onsetHour <= 6 and endHour > 0:
                tr = "hunker down"
            elif (threatTrendValue is not None) and (threatTrendValue.upper() == "DECREASING"):
                tr = "recovery"
            else:
                tr = "nothing to see here"
    
        return tr
    
    def _setThreatStatementsProductParts(self, segmentDict, productSegment, tr):
        
#         print "MATT: tr = %s    self._stats.maxThreat = %s" % (repr(tr),
#                                                                repr(self._stats.maxThreat))
        if tr is not None and self._stats._maxThreat is not None:
            (planning, action, preparation) = self._getThreatStatements(productSegment,
                                                                        self._sectionHeaderName,
                                                                        self._stats._maxThreat,
                                                                        tr)
    
            self._setProductPartValue(segmentDict, 'threatStatements',
                                      [planning, action, preparation])
    
    def _getThreatStatements(self, productSegment, sectionName, maxThreat, tr):
        import TCVDictionary
        threatStatements = TCVDictionary.ThreatStatements
        statements = threatStatements[sectionName][maxThreat][tr]
        planning = statements["planning"]
        preparation = statements["preparation"]
        action = statements["action"]
        
        # Check for any overrides
        try:
            planning = threatStatements[sectionName][maxThreat][tr]["planning"]
        except KeyError:
            pass
        
        try:
            preparation = threatStatements[sectionName][maxThreat][tr]["preparation"]
        except KeyError:
            pass
        
        try:
            action = threatStatements[sectionName][maxThreat][tr]["action"]
        except KeyError:
            pass
    
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
                    if vtecRecord["phensig"] in ["HU.A", "HU.W", "TR.A", "TR.W"] or \
                       self._stats._windowTS is not None:
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
                                      magnitudeIncreaseThreshold=self._textProduct.mphToKt(20))
        
        if self._threatTrendValue is not None:
            #  Convert the threat trend to a sentence
            threatTrendSentence = \
                self._getThreatTrendSentence("wind", self._threatTrendValue)

            self._setProductPartValue(segmentDict, 'threatTrend',
                                      threatTrendSentence)
    
    def _threatStatements(self, segmentDict, productSegmentGroup, productSegment):
        print "SARAH: Wind Threat Statements"
        windTr = self._calculateThreatStatementTr(self._stats._onset34Hour,
                                                  self._stats._end34Hour,
                                                  self._threatTrendValue)
#         print "MATT: in _threatStatements tr = %s" % (repr(windTr))
        
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
       
        #  If we are after the hunker down phase
        if windTr in ["recovery", "nothing to see here"]:
            #  Do not include this section at all
            return
 
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
        elif self._stats._inundationMax is None or self._stats._inundationMax < 1:
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
                summary += "Historic"
                
            self._setProductPartValue(segmentDict, 'latestForecastSummary', 
                                      summary + " storm surge flooding")

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

            self._setProductPartValue(segmentDict, 'peakSurge',
                                      "Peak Storm Surge Inundation: " + words + " somewhere within surge prone areas")
        
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
        print "SARAH: Surge Threat Statements"
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
        summary = "No flood watch is in effect"
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
        
        return "%d-%d inches...with locally higher amounts" % (minAccum, maxAccum)
    
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
        summary = "No Tornado Watch is in effect"
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


from xml.etree.ElementTree import Element, SubElement, tostring, dump
import xml.dom.minidom as minidom
import re
class XMLFormatter():
    def __init__(self, textProduct):
        self._textProduct = textProduct
    
    def execute(self, productDict):
        xml = Element('product')
        self.dictionary(xml, productDict)
        print "SARAH: XML =", xml
        print "SARAH: XML dump =", dump(xml)
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
        
        print "SARAH: sectionKey =", sectionKey
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
                        print "SARAH: skipping", key, "in XML"
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
                print "SARAH: info key =", key
                print "SARAH: value =", data
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
        self._tpc = Hazard_HLSTCV_Common.TextProductCommon()
    
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
        print "SARAH: productParts =", productParts
        for part in productParts:             
            valtype = type(part)
            if valtype is str:
                name = part
            elif valtype is tuple:
                name = part[0]
                infoDicts = part[1]
                print "SARAH: name =", str(name)
                print "SARAH: infoDicts =", infoDicts
                newtext = self.processSubParts(productDict.get(name), infoDicts)
                print "SARAH: newtext type =", type(newtext)
                print "SARAH: newtext =", repr(newtext)
                text += newtext
                continue
            elif valtype is list:
                print 'GOT HERE -- found list'
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
            elif name == 'endProduct':
                text += '$$\n' 
            elif name == 'CR':
                text += '\n'
            elif name == 'doubleAmpersand':
                text += '&&\n'
            elif name not in self._noOpParts():
                textStr = productDict.get(name)
                print "SARAH: name =", name
                print "SARAH: textStr =", textStr
                if textStr:
                    text += textStr + '\n'
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
        return text + "\n"
    
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
            print "SARAH: subpart subParts[i] =", subParts[i]
            print "SARAH: subpart infoDicts[i] =", infoDicts[i]
            newtext = self._processProductParts(subParts[i], infoDicts[i].get('partsList'))
            print "SARAH: subpart newtext type =", type(newtext)
            print "SARAH: subpart newtext =", repr(newtext)
            text += newtext
        return text

