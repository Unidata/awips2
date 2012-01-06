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
# RecreationFcst
#
# This product creates a combination of text phrases for consecutive
#   time periods for a list of edit areas.
#
#   Type: smart
#   Local product:
#      RecreationFcst_Local.py (type: smart)
#   Associated Utility files:
#     Combinations
#   To customize this product for your site:
#      Set up the Combinations file with Edit Areas and labels.
#      Set up RecreationFcst_Local to override variables, definitions, thresholds, and methods
#
#   Component Product Definitions included as methods in this file:
#      RecreationPhrases 
#      Extended 
#      Extended Label
##
##########################################################################
#  Example Output:
##    Recreation Statement

##    Area 1

##    .TODAY...
##    DEW POINTS...       IN THE UPPER TEENS.
##    MINIMUM HUMIDITY... 18.0 PERCENT.
##    WIND CHILL...       35.0 BECOMING 24.0 IN THE NIGHT..
##    WIND...             WEST WINDS 25 TO 35 MPH.
##    PRECIPITATION...    DRY.
##    LIGHTENING...       2.

##    .WEDNESDAY...
##    DEW POINTS...       IN THE MID TEENS.
##    MINIMUM HUMIDITY... 20.0 PERCENT.
##    WIND CHILL...       36.0 BECOMING 36.0 IN THE NIGHT..
##    WIND...             WEST WINDS 25 TO 35 MPH.
##    PRECIPITATION...    WIDESPREAD RAIN AND SNOW.
##    LIGHTENING...       2.


##    .THURSDAY...
##    VERY WINDY. SUNNY. WIDESPREAD SNOW. LOWS IN THE UPPER 30S. HIGHS IN THE MID
##    40S.
##    .FRIDAY...
##    SUNNY AND DRY. LOWS AROUND 40. HIGHS IN THE LOWER 40S.

##    Area 2

##    .TODAY...
##    DEW POINTS...       IN THE UPPER TEENS.
##    MINIMUM HUMIDITY... 18.0 PERCENT.
##    WIND CHILL...       35.0 BECOMING 24.0 IN THE NIGHT..
##    WIND...             WEST WINDS 25 TO 35 MPH.
##    PRECIPITATION...    DRY.
##    LIGHTENING...       2.

##    .WEDNESDAY...
##    DEW POINTS...       IN THE MID TEENS.
##    MINIMUM HUMIDITY... 20.0 PERCENT.
##    WIND CHILL...       36.0 BECOMING 36.0 IN THE NIGHT..
##    WIND...             WEST WINDS 25 TO 35 MPH.
##    PRECIPITATION...    WIDESPREAD RAIN AND SNOW.
##    LIGHTENING...       2.


##    .THURSDAY...
##    VERY WINDY. SUNNY. WIDESPREAD SNOW. LOWS IN THE UPPER 30S. HIGHS IN THE MID
##    40S.
##    .FRIDAY...
##    SUNNY AND DRY. LOWS AROUND 40. HIGHS IN THE LOWER 40S.

import TextRules
import SampleAnalysis
import ForecastNarrative
import time, string, types


class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = [
         (("Product Title","title"), "Recreation Statement", "alphaNumeric"),
         (("Choose Starting Time Range:", "timeRangeName"), "Tomorrow", "radio",
             ["Today", "Tomorrow"]),
         (("Number of days:", "numPeriods"), 2, "radio", [2, 3]),
         # Comment out the following line if you do not want to include Extended as an option:
         (("Extended", "extended"), "With Extended", "radio", ["Without Extended","With Extended"]),
         ]
    Definition =  {
        "type": "smart",
        "displayName": "None",
        # Name of map background for creating Combinations
        #"mapNameForCombinations": "Zones_<site>", 
        
        ## Edit Areas
        "defaultEditAreas" : [
              ("area1","Area 1"),
              ("area2","Area 2"),
              ("area3","Area 3"),
              ],      

        # Product-specific variables: Can be overridden in the Local file
        "extendedLabel": 1,
        "lineLimit": 45,
        "trace":0,
        }
    
    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)

    def generateForecast(self, argDict):
        # Generate Text Phrases for a list of edit areas

        # Get variables
        error = self._getVariables(argDict)
        if error is not None:
            return error

        # Get the areaList -- derived from defaultEditAreas and
        # may be solicited at run-time from user if desired
        self._areaList = self.getAreaList(argDict)
        if len(self._areaList) == 0:
            return "WARNING -- No Edit Areas Specified to Generate Product."

        # Determine time ranges
        error = self._determineTimeRanges(argDict)
        if error is not None:
            return error

        # Sample the data
        error = self._sampleData(argDict)
        if error is not None:
            return error

        # Initialize the output string
        fcst = ""
        fcst = self._preProcessProduct(fcst, argDict)

        # Generate the product for each edit area in the list
        for editArea, areaLabel in self._areaList:
            fcst = self._preProcessArea(fcst, editArea, areaLabel, argDict)
            fcst  = self._makeProduct(fcst, editArea, areaLabel, argDict)
            fcst = self._postProcessArea(fcst, editArea, areaLabel, argDict)

        fcst = self._postProcessProduct(fcst, argDict)
        return fcst

    def _getVariables(self, argDict):
        # Make argDict accessible
        self.__argDict = argDict

        # Get Definition variables
        self._definition = argDict["forecastDef"]
        for key in self._definition.keys():
            exec "self._" + key + "= self._definition[key]"

        # Get VariableList and _issuance_list variables
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"
                            
        self._language = argDict["language"]
        return None

    def _determineTimeRanges(self, argDict):
        # Set up the Narrative Definition and initial Time Range
        self._timeRange, self._narrativeDef = self._createNarrativeDef(argDict)
        self._definition["narrativeDef"] = self._narrativeDef
        self._definition["methodList"] = [self.assembleChildWords]
        return None

    def _sampleData(self, argDict):
        # Sample and analyze the data for the narrative
        # This data will be available in argDict["narrativeData"] for text rules
        self._narrativeProcessor = ForecastNarrative.ForecastNarrative()
        error = self._narrativeProcessor.getNarrativeData(
            argDict, self._definition, self._timeRange, self._areaList, None)
        if error is not None:
            return error
        return None

    def _preProcessProduct(self, fcst, argDict):
        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        # Generate Narrative Forecast for Edit Area
        fcst = fcst + self._narrativeProcessor.generateForecast(
            argDict, editArea, areaLabel)
        return fcst

    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        return fcst

    def _postProcessProduct(self, fcst, argDict):
        return string.upper(fcst)

    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################

    def _createNarrativeDef(self, argDict):
        # Determine the start time for the product and a Narrative Definition

        timeRange = self.getTimeRange(self._timeRangeName, argDict)

        if self._numPeriods == 2:
            recPhrases = [24, 24]
        else:
            recPhrases = [24,24,24]
        extendeds = [24, 24]

        # Create the NarrativeDef
        narrativeDef = []
        for recPhrase in recPhrases:
            narrativeDef.append(("RecreationPhrases",recPhrase))
        if self._extended == "With Extended":
            if self._extendedLabel == 1:
                narrativeDef.append(("ExtendedLabel",0))
            for extended in extendeds:
                narrativeDef.append(("Extended", extended))
        return timeRange, narrativeDef

    def _td_phrase(self):
        return {
            "setUpMethod": self._td_setUp,
            "wordMethod": self._td_words,
            "phraseMethods": [self.assembleSubPhrases,
                              self.postProcessPhrase],
            }
    def _td_setUp(self, tree, node):
        td = self.ElementInfo("Td", "List")
        elementInfoList = [td]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "DEW POINTS...       ")
        return self.DONE()

    def _td_words(self, tree, node):
        statDict = node.getStatDict()
        stats = self.getStats(statDict,"Td")
        if stats is None:
           return self.setWords(node, "")
        words = self.getTempPhrase(tree, node, stats,"Td")
        return self.setWords(node, words)

    def _rh_phrase(self):
        return {
            "setUpMethod": self._rh_setUp,
            "wordMethod": self._rh_words,
            "phraseMethods": [self.assembleSubPhrases,
                              self.postProcessPhrase],
            }
    def _rh_setUp(self, tree, node):
        rh = self.ElementInfo("RH", "List")
        elementInfoList = [rh]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "MINIMUM HUMIDITY... ")
        return self.DONE()

    def _rh_words(self, tree, node):
        statDict = node.getStatDict()
        stats = self.getStats(statDict,"RH")
        if stats is None:
           return self.setWords(node, "")
        min, max = stats
        ten = int(min / 10) * 10
        digit = min % 10
        if digit <= 3:
           RH1 = ten
        if digit > 3 or digit <= 9:
           RH1 = ten + 5
        RH2 = RH1 + 10
        words = `RH1` + " TO " + `RH2` + " PERCENT"
        return self.setWords(node, words)

    def _windChill_heatIndex_compoundPhrase(self):
        return {
            "phraseList": [
                self.windChill_phrase,
                self.heatIndex_phrase,
                ],
            "phraseMethods": [
                self.assembleSentences,
                self._windChill_heatIndex_finishUp,
                ],
            }
    def _windChill_heatIndex_finishUp(self, tree, node):
        words = node.get("words")
        if words is None:
            return
        if words == "":
            words = "NOT A FACTOR"
        node.set("descriptor", "")
        statsWC = tree.stats.get("WindChill", node.getTimeRange(),
                                 node.getAreaLabel(), mergeMethod="Min")
        if statsWC is not None and \
            statsWC < self.windChill_threshold(tree, node):
            node.set("indentLabel", "WIND CHILL...       ")
        else:
           statsHI = tree.stats.get("HeatIndex", node.getTimeRange(),
                                    node.getAreaLabel(), mergeMethod="Max")
           if statsHI is not None and \
              statsHI > self.heatIndex_threshold(tree, node):
              node.set("indentLabel", "HEAT INDEX...       ")
           else:
              node.set("indentLabel", "")
              words = ""
        node.set("compound", 1)
        return self.setWords(node, words)
 
    def _wind_phrase(self):
        return {
            "setUpMethod": self._wind_setUp,
            "wordMethod": self._wind_words,
            "phraseMethods": [self.assembleSubPhrases,
                              self.postProcessPhrase],
            }
    def _wind_setUp(self, tree, node):
        wind = self.ElementInfo("Wind", "List")
        elementInfoList = [wind]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "WIND...             ")
        return self.DONE()

    def _wind_words(self, tree, node):
        statDict = node.getStatDict()
        stats = self.getStats(statDict,"Wind")
        if stats is None:
           return self.setWords(node, "")
        elementInfo = node.getAncestor("firstElement")
        if elementInfo is None:
            return self.setWords(node, "")
        words = self.simple_vector_phrase(tree, node, elementInfo)
        if words == "null":
            return self.setWords(node, "null")
        maxWind, dir = self.getValue(stats, "Max", self.VECTOR())
        chopphrase = ""
        if maxWind >= 26.1:
           chopphrase = "HEAVY CHOP EXPECTED ON AREA RIVERS AND LAKES"
        elif maxWind >= 21.7:
           chopphrase = "MODERATE CHOP EXPECTED ON AREA RIVERS AND LAKES"
        elif maxWind >= 17.4:
           chopphrase = "LIGHT CHOP EXPECTED ON AREA RIVERS AND LAKES"
        if chopphrase != "":
           words = words + ".  " + chopphrase 
        return self.setWords(node, words)

    def _wx_phrase(self):
        return {
            "setUpMethod": self._wx_setUp,
            "wordMethod": self._wx_words,
            "phraseMethods": [self.assembleSubPhrases,
                              self.postProcessPhrase],
            }
    def _wx_setUp(self, tree, node):
        wx = self.ElementInfo("Wx", "List")
        qpf = self.ElementInfo("QPF", "MinMax")
        elementInfoList = [wx]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "PRECIPITATION...    ")
        return self.DONE()

    def _wx_words(self, tree, node):
        statDict = node.getStatDict()
        stats = self.getStats(statDict,"Wx")
        if stats is None:
           return self.setWords(node, "")
        self.weather_words(tree, node)
        WXwords = node.get("words")
        statsQ = self.getStats(statDict, "QPF")
        if statsQ is None:
           words = WXwords
        else:
           QPFrange0 = str(round(statsQ[0],2))
           QPFrange1 = str(round(statsQ[1],2))
           #print QPFrange1
           if ((QPFrange0 == "0.0" and QPFrange1 == "0.0") or (string.find(WXwords, "dry") != -1)):
              #print "Found dry weather"
              QPFwords = "\n"
              words =  WXwords
           elif (QPFrange0 == "0.0"):
              QPFwords = "AMOUNTS UP TO " + QPFrange1 + " OF AN INCH"
              words = WXwords + ".  " + QPFwords
           else:
              QPFwords = "AMOUNTS BETWEEN " + QPFrange0 + " AND " + QPFrange1 + " OF AN INCH"
              words = WXwords + ".  " + QPFwords
        return self.setWords(node, words)

    def _ltng_phrase(self):
        return {
            "setUpMethod": self._ltng_setUp,
            "wordMethod": self._ltng_words,
            "phraseMethods": [self.assembleSubPhrases,
                              self.postProcessPhrase],
            }
    def _ltng_setUp(self, tree, node):
        wx = self.ElementInfo("Wx", "List")
        elementInfoList = [wx]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "LIGHTNING...        ")
        return self.DONE()

    def _ltng_words(self, tree, node):
        statDict = node.getStatDict()
        stats = self.getStats(statDict,"Wx")
        if stats is None:
           return self.setWords(node, "NONE.")
        words = ""
        for subkey, rank in stats:
           wxType = subkey.wxType()
           if wxType == "T":
              cov = subkey.coverage()
              if cov in ["Num", "Wide", "Ocnl", "Brf", "Frq", "Pds", "Inter", "Lkly", "Def"]:
                 words = "LIKELY"
              elif cov in ["Sct", "Chc"] and words not in ["LIKELY"]:
                 words = "SCATTERED"
              elif cov in ["Iso", "SChc"] and words not in ["LIKELY", "SCATTERED"]:
                 words = "ISOLATED"
              elif words not in ["LIKELY", "SCATTERED", "ISOLATED"]:
                 words = "POSSIBLE"
           elif words not in ["LIKELY", "SCATTERED", "ISOLATED", "POSSIBLE"]:
              words = "NONE"
        #print words
        return self.setWords(node, words)

    ########################################################################
    # OVERRIDING THRESHOLDS AND VARIABLES
    ########################################################################

    # SampleAnalysis overrides
    def temporalCoverage_percentage(self, parmHisto, timeRange, componentName):
        return 15.0

    def temporalCoverage_dict(self, parmHisto, timeRange, componentName):
        return {
            "LAL": 0,
            "MinRH": 0,
            "MaxRH": 0,
            "MinT": 10,
            "MaxT": 10,
            "Haines": 0,
            "PoP" : 50,
            }

    # TextRules overrides
    def pop_wx_lower_threshold(self, tree, node):
        # Pop-related Wx will not be reported if Pop is below this threshold
        return 20

    def phrase_descriptor_dict(self, tree, node):
        # Descriptors for phrases
        dict = TextRules.TextRules.phrase_descriptor_dict(self, tree, node)
        dict["DEW POINTS...       "] = "DEW POINTS.........."
        dict["MINIMUM HUMIDITY... "] = "MINIMUM HUMIDITY...."
        dict["WIND CHILL...       "] = "WIND CHILL.........."
        dict["HEAT INDEX...       "] = "HEAT INDEX.........."
        dict["WIND...             "] = "WIND................"
        dict["PRECIPITATION...    "] = "PRECIPITATION......."
        dict["LIGHTNING...        "] = "LIGHTNING..........."
        dict["HeatIndex"] = ""
        dict["WindChill"] = ""
        return dict
    

    ########################################################################
    # OVERRIDING METHODS
    ########################################################################

    ########################################################################
    # COMPONENT PRODUCT DEFINITIONS
    ########################################################################

    def RecreationPhrases(self):
        return {
            "type": "component",
            "methodList": [
                          self.assembleIndentedPhrases,         
                          ], 
            "analysisList": [
                 ("Td", self.avg),
                 ("RH", self.minMax),
                 ("T", self.minMax),
                 ("WindChill", self.minMax, [12]),
                 ("HeatIndex", self.minMax, [12]),
                 ("Wind", self.vectorMinMax),
                 ("Wx", self.rankedWx),
                 ("QPF", self.accumMinMax),
                 ("PoP", self.binnedPercent),
                 ],
            "phraseList":[
                 self._td_phrase,
                 self._rh_phrase,
                 self._windChill_heatIndex_compoundPhrase,
                 self._wind_phrase,
                 self._wx_phrase,
                 self._ltng_phrase,
                 ],
            }

    def ExtendedLabel(self):
        return {
            "type": "component",
            "methodList": [self.setLabel],
            "analysisList": [],
            "phraseList":[],
            }
    def setLabel(self, tree, component):
        component.set("words", "\n.EXTENDED...\n")
        return self.DONE()

    def Extended(self):
        return {
            "type": "component",
            "methodList": [
                          self.orderPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ], 
            "analysisList": [
                       ("MinT", self.avg),
                       ("MaxT", self.avg),
                       ("T", self.hourlyTemp),
                       ("Sky", self.minMax),
                       ("Wind", self.vectorMinMax),
                       ("Wx", self.rankedWx),
                       ("PoP", self.binnedPercent),
                      ],
            "phraseList":[
                   self.reportTrends,
                   self.wind_summary,
                   self.sky_phrase,
                   self.weather_phrase,
                   self.lows_phrase,
                   self.highs_phrase,
                   self.temp_trends,
                 ],
            }

