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
# TextFormatter.py
# Main program and Control class for producing Forecasts
#
# Author: hansen
# ----------------------------------------------------------------------------
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    02/12/2014          #2591     randerso       Added retry when loading combinations fails

import string, getopt, sys, time, os, types, math
import ModuleAccessor
import Utility, logging, traceback
import AbsTime
from java.lang import ThreadDeath
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID, ReferenceData

GridLoc = None
LatLonIds = []

MAX_TRIES = 2

# If someone imports TextFormatter and needs this instance
# they should either be fixed to use the IFPImporter module
# or turn this line on (which is a kludge but should make
# the incorrect code run without fixing it).
#IFPImporter = IFPImporter.IFPImporter

class TextFormatter:
    def __init__(self, dataManager):
        # Variable for unique combinations
        self.__comboNumber = -1
        self.dataMgr = dataManager
        self.log = logging.getLogger("FormatterRunner.TextFormatter.TextFormatter")
        pass

#    def __del__(self):
#        for i in LatLonIds:
#            self.dataMgr.getRefManager().deleteRefSet(i, False)

    def getForecast(self, fcstName, argDict):
        " Create the forecast "

        ForecastNarrative = argDict["ForecastNarrative"]
        ForecastTable = argDict["ForecastTable"]
        Analysis = argDict["Analysis"]

        argDict["prevStats"] = ()
        argDict["productName"] = fcstName
        argDict["combinations"] = None
        # This allows the Interfaces class to later call this
        #  method for component forecasts.
        #  See: Interfaces.generateProduct
        argDict["getForecast"] = self.getForecast
        argDict["getFcstDef"] = self.getFcstDef
        argDict["dataMgr"] = self.dataMgr
        self.__ut = argDict["utility"]

        # Get the Forecast Definition and type from the server
        #print "finding", fcstName
        found, module = self.getFcstDef(fcstName, argDict)
        #print "found ", found
        if found == 0:
            text = "Text Product Definition Not Found: " + fcstName + " " + \
                traceback.format_exc()
            self.log.error("Text Product Definition Not Found: Caught Exception: " + fcstName, exc_info=True)
            raise Exception, text
        forecastDef = argDict["forecastDef"]
        fcstType = self.__ut.set(forecastDef, "type", None)
        argDict["fcstType"] = fcstType
        if fcstType is None:
            text = "Text Product Type Not Found: " + fcstName + " " + \
              traceback.format_exc()
            self.log.error("Text Product Type Not Found: Caught Exception: " + fcstName, exc_info=True)
            raise Exception, text

        argDict["varDict"] = argDict["cmdLineVarDict"]
        error = self.__getRunTimeVariables(fcstName, forecastDef, fcstType, module, argDict)
        if error is not None:
            return error

        # Sanity checks
        # Unless a "smart" product,
        # Must have at least one edit area and time range specified
        if fcstType != "smart" and fcstType != "component":
            if argDict["editAreas"] == []:
                text = "No Edit Areas Specified over which to generate product."
                text = text + '\nTry setting "defaultEditAreas" in the product Definition'
                text = text + '\nOr, if running from the command line, add a -r flag.'
                text = text + '\n' + string.join(traceback.format_exc())
                self.log.error("Caught Exception: " + text)
                raise Exception, text
            if argDict["rawRanges"] == []:
                text = "No Time Ranges Specified over which to generate product."
                text = text + '\nTry setting "defaultRanges" in the product Definition'
                text = text + '\nOr, if running from the command line, add a -w flag.'
                text = text + '\n' + string.join(traceback.format_exc())
                self.log.error("Caught Exception: " + text)
                raise Exception, text

        argDict["subType"] = fcstName

        # Component Phrase Forecasts
        # Create a narrative of one forecast
        if fcstType == "component":
            timeRange, label = argDict["rawRanges"][0]
            forecastDef = self.__createNarrativeDef(fcstName, timeRange)
            fcstType = "narrative"

        # Table Forecasts
        if fcstType == "table":
            forecast = ForecastTable.ForecastTable()
            forecast._debug = 0
            forecast._argDict = argDict
            try:
                text = forecast.generateForecast(argDict)
            except:
                self.log.error("Caught Exception: ", exc_info=True)
                raise Exception, string.join(traceback.format_exc())

        # Narrative Phrase Forecasts
        elif fcstType == "narrative":
            forecast = ForecastNarrative.ForecastNarrative()
            forecast._debug = 0
            forecast._argDict = argDict
            timeRange, label = argDict["rawRanges"][0]
            forecast.getNarrativeData(
                argDict, forecastDef, timeRange, argDict["editAreas"], None)
            text = self.__loop(argDict, forecast, forecastDef)

        # Smart Text
        elif fcstType == "smart":
            product = module.TextProduct()
            product._debug = 0
            argDict["self"] = product
            argDict["module"] = module
            product.setUp("T", argDict)
            product._argDict = argDict

            try:
                text = product.generateForecast(argDict)
            except RuntimeError, e:
                msg = e.message
                if msg.find('java.lang.ThreadDeath') > -1:
                    self.log.info("Formatter Canceled")
                else:
                    self.log.error("Caught Exception: ", exc_info=True)
                raise Exception

            # requirement for TEST phrasing for TEST products
            if argDict.get('testMode', 0):
                testMsg = "\nTHIS IS A TEST MESSAGE. DO NOT TAKE ACTION" + \
                  " BASED ON THIS TEST\nMESSAGE.\n"
                #split by "$$"
                segs = text.split('\n$$')
                for i in xrange(len(segs) - 1):  #never the last one
                    if text.find(testMsg) == -1:  #not found, add it in
                        segs[i] = segs[i] + testMsg
                text = '\n$$'.join(segs)   #put text back together again
                if text.find(testMsg) == -1:
                    text = text + '\n' + testMsg

            # Translator
            language = forecastDef.get('language', None)
            if language is not None:
                text = product.translateForecast(text, language)
            # Convert to Upper Case
            if not forecastDef.get('lowerCase', 0):
                text = text.upper()
        else:
            text = "Text Product Type Invalid " + \
                 "(must be 'table', 'component' or 'narrative'): ", fcstName, type
            text = text + '\n' + string.join(traceback.format_exc())
            self.log.error("Caught Exception: " + text)
            raise Exception, text

        return text

    def __createNarrativeDef(self, fcstName, timeRange):
        return {
            "methodList": [self.assembleChildWords],
            "narrativeDef": [(fcstName, timeRange.duration() / 3600)],
            }

    def __loop(self, argDict, forecast, forecastDef):
        # Loop through product by edit areas and time ranges

        begText = self.__ut.set(forecastDef, "beginningText", "")
        endText = self.__ut.set(forecastDef, "endingText", "")
        editAreaLoopBegText = self.__ut.set(forecastDef, "editAreaLoopBegText", "")
        timeRangeLoopBegText = self.__ut.set(forecastDef, "timeRangeLoopBegText", "")
        editAreaLoopEndText = self.__ut.set(forecastDef, "editAreaLoopEndText", "")
        timeRangeLoopEndText = self.__ut.set(forecastDef, "timeRangeLoopEndText", "")
        outerLoop = self.__ut.set(forecastDef, "outerLoop", "EditArea")

        editAreas = argDict["editAreas"]
        rawRanges = argDict["rawRanges"]
        # Loop through Edit Areas
        text = begText
        if outerLoop == "EditArea":
            for editArea in editAreas:
                argDict["editArea"] = editArea
                text = text + editAreaLoopBegText
                # Loop through time ranges
                if len(rawRanges) > 0:
                    argDict["issueRange"] = rawRanges[0]
                for rawRange, rangeName in rawRanges:
                    argDict["timeRange"] = rawRange
                    argDict["timeRangeName"] = rangeName
                    text = text + timeRangeLoopBegText
                    # forecastDef gets destroyed in narrative?, so must restore
                    argDict["forecastDef"] = forecastDef
                    try:
                        subText = forecast.generateForecast(argDict)
                    except:
                        self.log.error("Caught Exception: ", exc_info=True)
                        raise Exception, string.join(traceback.format_exc())
                    try:
                        subText, statDict, valueDict = subText
                    except:
                        pass
                    text = text + subText + timeRangeLoopEndText
                    text = forecast.fillSpecial(text, argDict)
                text = text + editAreaLoopEndText
                text = forecast.fillSpecial(text, argDict)
        else:
            for rawRange, rangeName in rawRanges:
                argDict["timeRange"] = rawRange
                argDict["timeRangeName"] = rangeName
                text = text + timeRangeLoopBegText
                for editArea in editAreas:
                    argDict["editArea"] = editArea
                    text = text + editAreaLoopBegText
                    # Loop through time ranges
                    # forecastDef gets  destroyed in narrative, so must restore
                    argDict["forecastDef"] = forecastDef
                    try:
                        subText, statDict, valueDict = \
                          forecast.generateForecast(argDict)
                    except:
                        self.log.error("Caught Exception: ", exc_info=True)
                        raise Exception, string.join(traceback.format_exc())
                    text = text + subText + editAreaLoopEndText
                    text = forecast.fillSpecial(text, argDict)
                text = text + timeRangeLoopEndText
                text = forecast.fillSpecial(text, argDict)
        return text

    def __getRunTimeVariables(self, fcstName, forecastDef, fcstType, module, argDict):
        # Input variables can come from various sources:
        #   varDict from command line
        #   command line switch e.g. -l language (will be in argDict)
        #   definition section of product
        # We must check all three in that order.

        varDict = argDict["varDict"]
        #print "varDict", varDict

        for item, default in [
            ("language", "english"),
            ("appendFile", None),
            ("lineLength", 69), # no command line option
            ("timePeriod", 3),
            ]:
            try: # Try the varDict
                #print "trying varDict", item
                argDict[item] = varDict[item]
                #print "got it in varDict"
            except:
                try: # Try argDict i.e. from command line switch
                    # If so, argDict is already set
                    #print "trying argDict", item
                    argValue = argDict[item]
                    #print "got it in argDict", argValue
                except:
                    argValue = None
                    # Try
                    #print "getting from definition", item
                if argValue is None:
                    argDict[item] = self.__ut.set(forecastDef, item, default)
                    #print "value from definition", argDict[item]
        # These need to be integers
        for item in ["lineLength", "timePeriod"]:
            if argDict[item] is not None:
                argDict[item] = int(argDict[item])

        # Edit Areas and Time Ranges
        #
        # Set up these argDict values:
        #      editAreas -- list of (refData, label) pairs
        #      timeRanges -- list of named ranges
        #      rawRanges -- list of (rawRange, rangeName) pairs
        #
        # As we eventually loop through the product, these values will be set:
        #      editArea -- current edit area pair
        #      timeRange -- current raw time range
        #      issueTime -- first raw time range
        #      timeRangeName -- current time range name (if available)
        #      combinations -- list of (editAreaList, comboLabel) tuples
        #         where editAreaList is the list of edit areas composing
        #         the combination

        # Set up Edit Areas
        # May be from these sources:
        #  "AreaLabel"
        #    varDict from command line -- list of names
        #    command line as named reference areas
        #    defaultEditAreas
        #       list of (name, label) pairs
        #  "LatLon"
        #    OR list of ((lat,lon,dimension), label)
        #  "Combinations"
        #    OR A Combinations file

        # We may have had edit areas entered from the command line
        #  or from the Interfaces "generateForecast" command
        # If so, argDict["editAreas"] will be a list of either
        #  (name, label) or (refData, label) pairs
        editAreaType = "AreaLabel"

        # We may have had edit areas entered from the command line
        #  If so, argDict["editAreas"] will be a list of either
        #  (name, label) or (refData, label) pairs
        if len(argDict["editAreas"]) == 0:
            dfEditAreas = self.__ut.set(forecastDef, "defaultEditAreas", [])
            try: # Try the varDict
                chosenAreas = varDict["Choose Edit Areas"]
                # Turn the list of chosen areas into (name, label) pairs
                # using the defaultEditAreas list
                dfEditAreas = self.__pairAreaWithLabel(chosenAreas, dfEditAreas)
            except:
                pass

            # Set up edit areas as ReferenceData's for AreaLabel and LatLon areas
            if type(dfEditAreas) == types.StringType:
                editAreaType = "Combinations"
                # Get edit areas from file with format:
                #   Combinations = [
                #         ([editArea1, editArea2,...],label)
                #         ...
                #         ]
                #  For example:
                #   Combinations = [
                #         (["Zones48", "Zones49", "Zones50"],"/48/49/50"),
                #         (["Zones37","Zones38"], "/37/38"),"/37/38"),
                #         (["Zones57","Zones58","Zones59"],"57/58/59")
                #        ]

                comboName = dfEditAreas
                for retryCount in xrange(MAX_TRIES):
                    accessor = ModuleAccessor.ModuleAccessor()
                    dfEditAreas = accessor.variable(comboName, "Combinations")
                    if dfEditAreas is None:
                        if sys.modules.has_key(comboName):
                            comboMod = sys.modules[comboName]
                            if comboMod.__file__.endswith(".pyo"):
                                os.remove(comboMod.__file__)
                            comboMod = None
                            del sys.modules[comboName]

                        # if not last try, log and try again
                        if retryCount < MAX_TRIES - 1:
                            # log but don't pop up
                            self.log.error("Error loading combinations file: %s, retrying", comboName)
                        else:
                            return "COMBINATION FILE NOT FOUND: " + \
                                   self.__ut.set(forecastDef, "defaultEditAreas", [])
                    else:
                        break

            elif len(dfEditAreas) > 0:
                refDataList = []
                tempRefData = []
                for area, label in dfEditAreas:
                    if type(area) is types.TupleType:
                        # Create a referenceData given lat, lon, dim
                        refData = self.__createArea(area, argDict)
                        tempRefData.append(refData)
                    else: # Get named Reference Data
                        id = ReferenceID(area)
                        refData = self.getEditArea(area, argDict)
                        if refData is None:
                            return "EDIT AREA NOT FOUND: " + str(id)
                    refDataList.append((refData, label))

                argDict["editAreas"] = refDataList
                storeReferenceData(self.dataMgr.getRefManager(), tempRefData)

        # Set up HazardsTable
        #    Product must be:
        #      --Type "smart"
        #      --Have an "filterMethod" method
        if fcstType == "smart":
            product = module.TextProduct()

            # Test Code: Uncomment to test
            #allowedHazards = product.allowedHazards()
            #filterMethod = product.filterMethod
            #print "allowedHazards", allowedHazards

            try:
                allowedHazards = product.allowedHazards()
                filterMethod = product.filterMethod
            except:
                allowedHazards = None

            if allowedHazards is not None and allowedHazards != []:
                # Set up editAreas as a list of combinations
                #   Cases:
                #    lat/lon or (area, label) pairs -- call HazardsTable,
                #            but the edit areas will not change
                #    Combinations -- call HazardsTable and check for changed combinations

                # Set up edit areas as list of lists
                editAreas = []

                for area, label in dfEditAreas:
                    if type(area) is types.ListType:
                        editAreas.append(area)
                    elif type(area) is types.TupleType: #LatLon
                        editAreas.append([self.__getLatLonAreaName(area)])
                    else:
                        editAreas.append([area])

                # if Definition['separateByTimeZone'] set to "effectiveTZ"
                # or "actualTZ", change the set of edit areas to ensure
                # that time zones are same in each grouping.
                separateByTZ = product.Definition.get('separateByTimeZone',
                  None)
                if separateByTZ is not None:
                    areaDictName = product.Definition.get('areaDictionary',
                      "AreaDictionary")
                    editAreas = self._separateByTimeZone(editAreas,
                      areaDictName, argDict['creationTime'],
                      effectiveTZ=separateByTZ)

                accurateCities = product.Definition.get('accurateCities', 0)
                cityRefData = []
                if accurateCities:
                    cityLocationName = product.Definition.get('cityLocation',
                                                              "CityLocation")
                    accessor = ModuleAccessor.ModuleAccessor()
                    citydict = accessor.variable(cityLocationName,
                                                 "CityLocation")

                    cityEAs = []
                    if citydict is None:
                        msg = "CityLocation dictionary module was not found for"\
                              " city location:"
                        self.log.error(msg + `cityLocationName`)
                    else:
                        for ea in editAreas:
                            for ean in ea:
                                if ean not in citydict:
                                    msg = "CityLocation dictionary does not "\
                                          "contain entry for edit area: "
                                    self.log.error(msg + `ean`)
                                    continue

                                for city, llrec in citydict[ean].iteritems():
                                    # Create a referenceData given lat, lon, dim
                                    area = (llrec[0], llrec[1], 0)
                                    refData = self.__createArea(area, argDict)
                                    cityEAs.append(refData)
                                    cityRefData.append((refData, city))

                    # Store temporary reference data in the server
                    #storeReferenceData(argDict['ifpClient'], cityEAs)
                    storeReferenceData(self.dataMgr.getRefManager(), cityEAs)

                # Get Product ID and other info for HazardsTable
                pil = self.__ut.set(forecastDef, "pil", None)
                stationID4 = product.Definition['fullStationID']
                productCategory = pil[0:3]   #part of the pil
                sampleThreshold = product.Definition.get(\
                  "hazardSamplingThreshold", (10, None))

                # Process the hazards
                import HazardsTable
                hazards = HazardsTable.HazardsTable(
                  argDict["ifpClient"], editAreas, productCategory,
                  filterMethod, argDict["databaseID"], stationID4,
                  argDict["vtecActiveTable"], argDict["vtecMode"],
                  sampleThreshold, creationTime=argDict["creationTime"], dataMgr=self.dataMgr,
                accurateCities=accurateCities,
                         cityEditAreas=cityRefData)

                # Store hazards object for later use
                argDict["hazards"] = hazards

                # Get revised combinations
                if editAreaType == "Combinations":
                    reorganizeCombos = product.Definition.get("reorganizeCombinations", 1)
                    if reorganizeCombos:
                        hazardAreas = hazards.getHazardAreaCombinations()
                        # Add a bogus label
                        newCombos = []
                        for combo in hazardAreas:
                            newCombos.append((combo, ""))
                        # Re-assign new combinations
                        dfEditAreas = newCombos

        # Set up Combinations as ReferenceDatas
        if editAreaType == "Combinations":
            argDict["editAreas"], dfEditAreas = self.getCombinations(dfEditAreas, argDict)
            argDict["combinations"] = dfEditAreas

        # Set up Time Ranges
        # May be from these sources:
        #   varDict from command line
        #   defaultTimeRanges
        #   command line as named time ranges
        #   command line as start and end times OR
        #   from argDict already set up by Interfaces::generateProduct
        #     In these cases "useRawTR" will be set to 1
        if len(argDict["timeRanges"]) > 0:
            # Use named time ranges from command line
            dfRanges = argDict["timeRanges"]
        else:
            try: # Try the varDict
                dfRanges = varDict["Choose Time Ranges"]
            except:
                dfRanges = self.__ut.set(forecastDef, "defaultRanges", [])
            argDict["timeRanges"] = dfRanges

        rawRanges = []
        argDict["rawRanges"] = rawRanges
        if argDict["useRawTR"] == 1:
            tr = argDict["timeRange"]
            try:
                trName = argDict["timeRangeName"]
            except:
                trName = ""
            if tr is not None:
                rawRanges.append((tr, trName))
        elif len(dfRanges) == 0:
            pass
        else:
            import TimeRangeUtils
            forecast = TimeRangeUtils.TimeRangeUtils()
            for rangeName in dfRanges:
                rawRange = forecast.getTimeRange(rangeName, argDict)
                rawRanges.append((rawRange, rangeName))
        argDict["rawRanges"] = rawRanges
        #print "rawRanges", rawRanges

        # Row Label
        areaType = self.__ut.set(forecastDef, "areaType", "")
        rowVariable = self.__ut.set(forecastDef, "rowVariable", "EditArea")
        if rowVariable == "EditArea":
            rowLabel = areaType
        elif rowVariable == "WeatherElement":
            rowLabel = "Weather Element"
        else:
            rowLabel = "Time Period"
        argDict["heading"] = rowLabel

    def __pairAreaWithLabel(self, chosenAreas, defaultEditAreas):
        # Pair the chosen edit areas with associated labels from
        # default list and return new list
        dfEditAreas = []
        for area in chosenAreas:
            for name, label in defaultEditAreas:
                if area == name:
                    dfEditAreas.append((name, label))
                elif area == label:
                    # Pair back with (lat,lon,dim) tuple
                    dfEditAreas.append((name, label))
        return dfEditAreas

    def __createArea(self, latLonTuple, argDict):
        # Return a ReferenceData created for the given lat,lon and dimension
        # If dim is zero, make edit area of the one grid
        #  point closest to the lat/lon value.
        lat, lon, dim = latLonTuple
        name = self.__getLatLonAreaName((lat, lon, dim))
        #print "\ncreateArea", lat, lon, dim, name
        if dim != 0:
            for x in range(100):
                points = makeSquare(lat, lon, dim)
                pointList = []
                for point in points:
                    pointList.append(makePoint(point))
                refData = makeArea(self.dataMgr.getClient().getDBGridLocation(), pointList, refname=name)
                # Make sure we have at least one grid point in
                # the edit area
                if refData.getGrid().isAnyBitsSet():
                    #print "returning", dim
                    return refData
                # Increment dim and try again
                #print "iterating", dim
                dim += 0.25
            msg = "\nWARNING!!! EMPTY EDIT AREA. INCREASE LAT/LON AREA DIMENSION!!\n"
            self.log.warning(msg)
            return None
        else:
            from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DBit
            # Get grid cell coordinates for lat/lon
            gridLoc = self.dataMgr.getClient().getDBGridLocation()
            nx = gridLoc.getNx()
            ny = gridLoc.getNy()
            cc2D = gridLoc.gridCell(float(lat), float(lon))
            # convert grid cell to Grid2DBit with single bit set
            grid2Dbit = Grid2DBit(nx.intValue(), ny.intValue())
            if (nx > cc2D.x >= 0 and ny > cc2D.y >= 0):
                grid2Dbit.set(int(cc2D.x), int(cc2D.y))
            #refData = GridLoc.convertToReferenceData(grid2Dbit)
            refID = ReferenceID(name)
            refData = ReferenceData(gridLoc, refID, grid2Dbit)
            return refData

    def __getLatLonAreaName(self, latLonTuple):
        lat, lon, dim = latLonTuple
        name = "Ref" + '%s%s%s' % (lat, lon, dim)
        name = name.replace(".", "")
        name = name.replace("-", "")
        return name

    def getCombinations(self, combinations, argDict):
        editAreas = []
        newCombinations = []
        for comboList, areaLabel in combinations:
            newComboList = []
            for editArea in comboList:
                #print "Get edit area TF: get edit area combo", editArea
                #print "TF2: Get Edit Area set up edit areas", editArea
                newArea = self.getEditArea(editArea, argDict)
                if comboList.index(editArea) == 0:
                    comboNumber = self.getComboNumber()
                    label = "Combo" + `comboNumber`
                    refId = ReferenceID(label)
                    #global GridLoc
                    #GridLoc = newArea.getGloc()
                    area = ReferenceData(newArea)
                    area.setId(refId)
                        #GridLoc, refId, newArea.getPolygons("LATLON"), "LATLON")
                    # randerso: I don't think this is necessary
                    # area.convertToAWIPS()
                newComboList.append(newArea.getId().getName())
                area = self.unionAreas(label, area, newArea)
            if argDict["fcstType"] == "table":
                # Allow user-supplied area labels to be used for simple tables
                editAreas.append((area, areaLabel))
            else:
                editAreas.append((area, label))
            newCombinations.append((newComboList, label))
        return editAreas, newCombinations

    def getComboNumber(self):
        self.__comboNumber = self.__comboNumber + 1
        return self.__comboNumber

    def getFcstDef(self, fcstName, argDict):
        # Find the given Forecast Definition.
        # Look for a method in the current forecast smart "TextProduct" class (if one exists)
        #        i.e. product.fcstName()
        # These can be removed eventually:
        #    Next look for Definition variable i.e. module.Definition
        #    Next look by name in modules already imported i.e. module.fcstName
        #    Next try to find a module with that name i.e. module.__name__ = fcstName
        # If found,
        #    set argDict["forecastDef"] to the definition and return 1 and the module
        # Else return 0 (not found)
        #
        # The Definition can be in various forms due to various product types and
        #  backward compatibility:
        #    Existing TextProduct class: product.fcstName()
        #    Find file with fcstName and look for Definition as variable in file
        #    Otherwise if TextProduct class, instantiate and look for Definition method
        #
        # Is the method in the current forecast smart TextProduct class?

        try:
            product = argDict["self"]
            exec "fcstDef = product." + fcstName + "()"
            module = argDict["module"]
        except:
            # See if fcstName is variable in imported modules e.g. MyTable = {}
            #  This can be removed eventually
            fcstDef, module = self.__ut.findInImported(fcstName)
            if fcstDef is None or type(fcstDef) is not dict:
                # Go get new module, fcstName
                module = self.__ut.module(fcstName, 0)
                if module is None:
                    return 0, module
                try:
                    # Look for Definition = {}
                    #  This can be removed eventually
                    exec "fcstDef = module.Definition"
                except:
                    try:
                        # Look for fcstName = {}
                        # This can be removed eventually
                        exec "fcstDef = module." + fcstName
                    except:
                        try:
                            # Try to instantiate smart text product class
                            #  and look for product.Definition() method
                            fcstDef = module.TextProduct.Definition
                            #product = module.TextProduct()
                            #fcstDef = product.Definition()
                        except:
                            return 0, module

        # Let the command line Variables override Definition variables
        # Handle cases of varDict key as single value or tuple
        # e.g.
        #   ('editAreaSuffix':'_pt')
        #     or
        #   ('Edit Area Suffix', 'editAreaSuffix'): '_pt'

        varDict = argDict["cmdLineVarDict"]
        for key in fcstDef.keys():
            for varKey in varDict.keys():
                if varKey == key:
                    fcstDef[key] = varDict[varKey]
                elif type(varKey) is types.TupleType:
                    if varKey[1] == key:
                        fcstDef[key] = varDict[varKey]

        argDict["forecastDef"] = fcstDef
        return 1, module

    def assembleChildWords(self, tree, node):
        fcst = ""
        for child in node.get("childList"):
            words = child.get("words")
            if words is None:
                return
            fcst = fcst + words
        node.set("words", fcst)
        return 1

    def unionAreas(self, name, area1, area2):
        # OR the areas (ReferenceData objects)
        # together and return a ReferenceData object
        refData = area1.orEquals(area2)
        #refData.convertToLatLon()
        refData.setId(ReferenceID(name))
        refData.getGrid()
        return refData

    def getEditArea(self, editAreaName, argDict):
        # Returns an AFPS.ReferenceData object given an edit area name
        # as defined in the GFE
        # Apply suffix if appropriate
        refID = ReferenceID(editAreaName)
        #print "Getting edit area"
        definition = argDict["forecastDef"]
        if definition.has_key("editAreaSuffix"):
            eaSuffix = definition["editAreaSuffix"]
            #print "eaSuffix", eaSuffix
            if eaSuffix is not None:
                #inv = argDict["ifpClient"].getReferenceInventory()
                inv = self.dataMgr.getRefManager().getAvailableSets()
                inventory = []
                sz = inv.size()
                for x in range(sz):
                    invID = inv.get(x)
                    inventory.append(str(invID.getName()))
                suffName = editAreaName + eaSuffix
                if suffName in inventory:
                    #print "  Setting suffix id", suffName
                    refID = ReferenceID(suffName)
        #print "  Adding editArea", refID
        from java.util import ArrayList
        refList = ArrayList()
        refList.add(refID)
        tmp = self.dataMgr.getRefManager().getReferenceData(refList).get(0)
        #tmp.getGrid()
        return tmp

    def _separateByTimeZone(self, editAreaGroups, areaDictName, creationTime,
      effectiveTZ="effectiveTZ"):
        #takes the list of areas, and based on the time zones breaks
        #them up to ensure that each grouping using the same time zone.
        #areaDictName is name of the area dictionary. creationTime is the
        #run time of the formatter.  EffectiveTZ organizes the groups by
        #the effective time zone, rather than the TZ environment variable.
        #Typically used for the PFM/AFM.

        #print "separateByTimeZone: ", editAreaGroups
        out = []  #list of editAreaGroups, with edit areas within each group

        import ModuleAccessor
        accessor = ModuleAccessor.ModuleAccessor()
        areaDict = accessor.variable(areaDictName, "AreaDictionary")
        localTZ = os.environ['TZ']   #current WFO time zone
        localTZid = time.strftime("%Z", time.localtime(creationTime))
        #print "Current WFO tz: ", localTZ
        for areas in editAreaGroups:
            tzDir = {}   #key is TZ var (EST5EDT), value is edit areas
            tzidDir = {}  #key is effective TZ (EST), value is edit areas
            #print "Areas in group: ", areas
            tzs = None
            for area in areas:
                #print "processing area: ", area
                try:
                    zoneTZ = areaDict[area]['ugcTimeZone']
                    prevTZ = os.environ['TZ']
                    os.environ['TZ'] = zoneTZ
                    time.tzset()
                    tzid = time.strftime("%Z", time.localtime(creationTime))
                    os.environ['TZ'] = prevTZ
                    time.tzset()

                    #print "areadict entry: ", zoneTZ
                except:
                    zoneTZ = localTZ
                    tzid = localTZid
                    #print "falling back to WFOtz: ", zoneTZ
                    self.log.warning("WARNING: Entry " + area +
                      " missing from AreaDictionary. Using default time zone.")

                zones = tzDir.get(zoneTZ, [])
                zones.append(area)
                tzDir[zoneTZ] = zones
                zones = tzidDir.get(tzid, [])
                zones.append(area)
                tzidDir[tzid] = zones
            #print "TZs for areas: ", tzDir
            #print "TZids for areas: ", tzidDir

            #organize the effective time zones
            if effectiveTZ == "effectiveTZ":
                dict = tzidDir
            elif effectiveTZ == "actualTZ":
                dict = tzDir
            else:
                self.log.error("Invalid effectiveTZ for separateByTZ() " +
                  effectiveTZ)
                return editAreaGroups
            keys = dict.keys()
            keys.sort()
            for key in keys:
                out.append(dict[key])
        #print "After TZ separate: ", out
        return out



#################################################################
def makeSquare(lat, lon, km):
    " Make a list of square of given km  around lat,lon"
    latinc = km / 222.0
    loninc = math.cos(lat / 57.17) * km / 222.0

    latTop = lat + latinc
    latBottom = lat - latinc
    lonLeft = lon - loninc
    lonRight = lon + loninc

    points = []
    points.append(`latTop` + "," + `lonRight`)
    points.append(`latTop` + "," + `lonLeft`)
    points.append(`latBottom` + "," + `lonLeft`)
    points.append(`latBottom` + "," + `lonRight`)
    return points

def makePoint(point):
    " Make a CartCoord2D from the point in format: x,y"
    from com.vividsolutions.jts.geom import Coordinate
    ind = string.find(point, ",")
    latStr = point[0:ind - 1]
    lonStr = point[ind + 1:len(point)]
    lat = float(latStr)
    lon = float(lonStr)
    return Coordinate(lon, lat)

def makeArea(gridLoc, pointList, refname=None):
    " Make a Reference Area with a unique ReferenceID"
    from com.vividsolutions.jts.geom import GeometryFactory, LinearRing, Coordinate, Polygon
    from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData_CoordinateType as CoordinateType
    geomFactory = GeometryFactory()
    import jep
    size = len(pointList)
    if pointList[0] != pointList[size - 1]: # closing the loop
        pointList.append(pointList[0])
    pointArray = jep.jarray(len(pointList), Coordinate)
    for i in range(len(pointList)):
        pointArray[i] = pointList[i]
    lr = geomFactory.createLinearRing(pointArray)
    poly = geomFactory.createPolygon(lr, jep.jarray(0, LinearRing))
    polyArray = jep.jarray(1, Polygon)
    polyArray[0] = poly
    region = geomFactory.createMultiPolygon(polyArray)
    if refname is None:
        refname = "Ref" + getTime()
    refId = ReferenceID(refname)
    refData = ReferenceData(gridLoc, refId, region, CoordinateType.LATLON)
    # randerso: I don't think this is necessary
    # refData.convertToAWIPS()
    return refData

def storeReferenceData(refSetMgr, refData, temp=True):
    if type(refData) is not list:
        refData = [refData]
    for ref in refData:
        refSetMgr.saveRefSet(ref)
    # Save it's name to delete later
    if temp:
        for r in refData:
            LatLonIds.append(r.getId())


def getTime():
    "Return an ascii string for the current time without spaces or :'s"
    timeStr = `time.time()`
    timeStr = string.replace(timeStr, ".", "_")
    return timeStr

def getAbsTime(timeStr):
    "Create an AbsTime from a string: YYYYMMDD_HHMM"

    year = string.atoi(timeStr[0:4])
    month = string.atoi(timeStr[4:6])
    day = string.atoi(timeStr[6:8])
    hour = string.atoi(timeStr[9:11])
    minute = string.atoi(timeStr[11:13])

    return AbsTime.absTimeYMD(year, month, day, hour, minute)

def usage():
    print """
Usage: python TextFormatter.py
              -d database
              -t forecastType
             [-o output file for text -- default None]
             [-O server output file for text -- default None]
             [-S server controlled output file -- default None]
             [-A append text to given file name]
             [-h host -- default orca.fsl.noaa.gov]
             [-p port -- default 98000000]
             [-l language -- english, french, spanish: default english]
             [-z displaced real time -- format YYYYMMDD_HHMM]
             [-T] Generates a "TEST" product.
             [-E] Generates a "EXPERIMENTAL" product.
             [-v vtecMode] Specifies vtec mode ('X','O','T','E')
             [-a vtecActiveTableName] Specifies alternate active table
             [-V vardict] use this option to provide a run-time VariableList
                  instead of displaying the user dialog.
                  The dictionary must be in the form of a Python
                  dictionary string, e.g.,
                '{("Forecast Product", "productType"):"Morning",
                  ("Issuance", "issuanceType"):"Routine"}'
                  The entries must be complete or the product will be cancelled.

             For Simple Table products:
             [-r Edit Area Name]
             [-w Time Range Name]  OR
             [-s startTime -e endTime]
             [-i Period for Tables with variable period (rows or columns)]
             """

def writeToFile(forecasts, outputFile, mode):
    if not outputFile is None and outputFile != "":
        outfile = open(outputFile, mode)
        os.chmod(outputFile, 0644)
        if outfile is None:
            return 0
        else:
            outfile.write(forecasts)
            outfile.close()
    return 1

def writeToServerFile(forecasts, outputFile, ifpClient):
    if not outputFile is None and outputFile != "":
        id = AFPS.TextFileID(outputFile, "PRODGEN")
        textFile = AFPS.TextFile(id, forecasts)
        ifpClient.saveTextData([textFile])
        return 1
    return 1
