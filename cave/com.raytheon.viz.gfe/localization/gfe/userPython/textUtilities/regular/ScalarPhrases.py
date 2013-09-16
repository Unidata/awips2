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
# ScalarPhrases.py
# Methods for producing text forecast from SampleAnalysis statistics.
#
# Author: hansen
# ----------------------------------------------------------------------------

import PhraseBuilder
import types

class ScalarPhrases(PhraseBuilder.PhraseBuilder):
    def __init__(self):    
        PhraseBuilder.PhraseBuilder.__init__(self)

    ############################################      
    ### PUBLIC SCALAR WEATHER ELEMENT PHRASES
    ### To override, override the associated method in your text product class.

    ### T
    def temp_trend_nlValue(self, tree, node):
        # THRESHOLD FOR REPORTING TEMPERATURE TRENDS
        return 20.0
    ### Td
    ### MaxT and MinT
    ### Sky

    def pop_sky_lower_threshold(self, tree, node):
        """Do not include an explicit Sky forecast when PoPs are
        >= 60% for the majority of the forecast period.
        """
        #  Get all the PoP stats for this component
        component = node.getComponent()
        compRange = component.getTimeRange()
        popStats = tree.stats.get('PoP', compRange, node.getAreaLabel(),
                                  mergeMethod="List")

        #  If the PoP stats are missing
        if popStats is None or popStats == []:
            return 100.0                #  keep sky cover as a precaution

        #  Initialize a counter to keep track of the number of subperiods
        #  where the PoP >= 55% (rounds to 60%)
        count = 0
        #  Look at each PoP value
        for (value, timeRange) in popStats:
            #  See if PoP is 'likely' or 'categorical'
            if value >= 55.0:
                count += 1              #  count this subphrase period

        #  Determine the percentage of the time PoP is 'likely' or 'categorical'
        percent = 100.0 * float(count)/float(len(popStats))       
        # If the majority of the period has 'likely' or 'categorical' PoPs
        if percent > 50.0:
            val = 59.0             # omit sky cover from the forecast
        else:
            val = 100.0            # sky cover required
        return val

    def clearing_threshold(self, tree, node):
        # Threshold for phrases such as:
        #    mostly cloudy in the morning then clearing
        # Used by sky_phrase
        return 31
    
    def sky_valueList(self, tree, node):
        # Phrases for sky given values.  Tuples consist of:
        #  (threshold, dayTime phrase, nightTime phrase)
        # Used by skyRange_phrase
        # NOTE: If you change these words, you MUST also
        # adjust the similarSkyWords_list and preferredSkyWords
        # used for sub-phrase combining and reporting sky trends.
        return [
            (5, "sunny", "clear"),
            (25, "sunny", "mostly clear"),
            (50, "mostly sunny", "partly cloudy"),
            (69, "partly sunny", "mostly cloudy"),
            (87, "mostly cloudy", "mostly cloudy"),
            (100, "cloudy", "cloudy"),
            ]
                    
    def similarSkyWords_list(self, tree, node):
        # The following pairs of sky words will be considered
        # "equal" when comparing for phrase combining
        # and redundancy
        #
        # For trends, (e.g. Sunny in the morning then partly cloudy in the afternoon.)
        # the following transitions are not allowed:
        #  Day time:
        #    Sunny <--> mostly sunny
        #    Mostly sunny <--> partly sunny
        #    Partly cloudy <--> mostly cloudy
        #  Night time:
        #    Clear <--> mostly clear
        #    Mostly clear <--> partly cloudy
        #    Mostly cloudy <--> cloudy
        #
        # In other words these transitions are allowed:
        #  Day time:
        #    sunny <--> partly sunny or above
        #    mostly sunny <--> mostly cloudy or above
        #    partly sunny <-->  sunny or cloudy
        #    mostly cloudy <--> mostly sunny
        #  Night time:
        #    clear can go to partly cloudy or above
        #    mostly clear <--> mostly cloudy or above
        #    partly cloudy <--> mostly cloudy or above
        #    mostly cloudy <--> partly cloudy or below
        
        dayNight = self.getPeriod(node.getTimeRange(), 1)
        if dayNight == self.DAYTIME():
            return [
                ("sunny", "mostly sunny"), 
                ("mostly sunny", "partly sunny"),
                ("partly sunny", "mostly cloudy"),
                ("mostly cloudy", "cloudy"),
                ]
        else:
            return [
                ("clear", "mostly clear"),
                ("mostly clear", "partly cloudy"),
                ("mostly cloudy", "cloudy"),
                ]
            
    def similarSkyWords_flag(self, tree, node, words1, words2):
        # Returns 1 if the pair of words is equal or similar
        # according to the "similarSkyWords_list"
        if words1 == words2:
            return 1
        # Check for similarity
        for value1, value2 in self.similarSkyWords_list(tree, node):
            if (words1 == value1 and words2 == value2) or \
               (words2 == value1 and words1 == value2):
                return 1
        return 0

    def preferredSkyWords(self, tree, node, words1, words2):
        # Returns the preferred words given the pair
        # of words1, words2
        preferredList = ["mostly sunny", "mostly clear", "cloudy"]
        if words1 in preferredList:
            return words1
        if words2 in preferredList:
            return words2
        return words1

    def reportIncreasingDecreasingSky_flag(self, tree, node):
        # If 1, will use "increasing clouds", "decreasing clouds"
        # wording instead of "mostly cloudy becoming sunny"

        # You have 3 options:
        #   return 0 -- do not use increasing/decreasing wording
        #   return 1 -- use increasing/decreasing wording if applicable
        #   Use the code shown below to use increasing/decreasing wording
        #    but avoid repetitive usage.
        return 0
        #return 1
   
        # Use the following code to avoid redundancy e.g.
        #  SUNDAY...Increasing clouds.
        #  SUNDAY NIGHT...Increasing clouds.
        #
        #If the previous period had increasing or decreasing wording, return 0
        # Otherwise, return 1

        # Check to see if previous period had increasing or decreasing wording
        component = node.getComponent()
        prevComp = component.getPrev()
        if prevComp is not None:
            # Look at the sky_phrase
            skyWords = self.findWords(
                tree, prevComp, "Sky", node.getAreaLabel(),
                phraseList=[node.getAncestor('name')], phraseLevel=1)
            if skyWords is not None:
                if skyWords.find("increasing") >= 0 or \
                   skyWords.find("decreasing") >= 0:
                    return 0                     
            return 1
        return 1    

    def reportClearSkyForExtendedPeriod_flag(self, tree, node):
        # If 1, will report clear/mostly clear wording for periods that
        # exceed 12 hours. Otherwise, will report sunny/mostly sunny.
        return 1
    
    def sky_value(self, tree, node, value, dayNight, returnIndex=0):
        # Check for areal coverage term
        # Otherwise, access the sky_valueList and return words corresponding to value
        if value is None:
            return ""
        words = self.areal_sky_value(tree, node, value, dayNight)
        if words is not None:
            # Set to use then connector only
            node.set("connector",  " then ")
            # Return areal wording
            if returnIndex:
                return words, 0
            else:
                return words
        sky_valueList = self.sky_valueList(tree, node)
        for i in range(len(sky_valueList)):
            threshold, dayWords, nightWords = sky_valueList[i]
            if value <= threshold:
                flag = self.reportClearSkyForExtendedPeriod_flag(tree, node)
                if flag == 1:
                    if dayNight == self.DAYTIME():
                        words = dayWords
                    else:
                        words = nightWords
                else:
                    if dayNight == self.NIGHTTIME():
                        words = nightWords
                    else:
                        words = dayWords
                if returnIndex:
                    return words, i
                else:
                    return words

    def areal_sky_flag(self, tree, node):
        # Set to 1 if you want to use areal (e.g. patchy clouds, areas of clouds)
        # vs. traditional sky wording when appropriate.
        # BE SURE AND SET THE "arealSkyAnalysis" flag to 1 in the Definition section!
        # You may want to base this decision on the current edit area and/or
        # component e.g. "Period_1"
        return 0
    
    def areal_sky_value(self, tree, node, value, dayNight):
        if not self.areal_sky_flag(tree, node):
            return None
        skyBins = tree.stats.get("Sky", node.getTimeRange(),
                                 node.getAreaLabel(),
                                 statLabel="binnedPercent",
                                 mergeMethod="MergeBins")
        #print "skyBins", skyBins, node.getTimeRange()
        if skyBins is None:
            return None
        
        # Determine percent in highest bin
        length = len(skyBins)
        highBin = skyBins[length-1]
        low, high, highBinPercent = highBin
        
        # Base wording on high bin percent
        words = None
        #print "highBinPercent", highBinPercent
        for skyPercent, skyWords in self.areal_skyPercentages(tree, node):
            #print "skyPercent", skyPercent
            if highBinPercent > skyPercent:
                words = skyWords
                break
        #print "words", words
        if words is None:
            return None # Revert to traditional coverage
        
        # Check for sky-related Wx
        wxStats = tree.stats.get("Wx", node.getTimeRange(), node.getAreaLabel(),
                                 mergeMethod="Average")
        if wxStats is None:
            return None
        # Keep track of skyRelatedWx that we have added to the wording already
        # so we don't end up with "Areas of low clouds and fog and fog." 
        foundWx = []
        for wx in self.areal_skyRelatedWx(tree, node):
            # Look for "dense" fog
            dense = ""
            if wx == "F":
                for subkey, rank in wxStats:
                    if subkey.wxType() == "F" and subkey.intensity() == "+":
                        dense = "dense "                    
            for subkey, rank in wxStats:
                if subkey.wxType() == wx and wx not in foundWx:
                    foundWx.append(wx)
                    # Add wording
                    words = words + " and " + dense + subkey.wxDef().typeDesc(wx).lower()
        return words
            
    def areal_skyPercentages(self, tree, node):
        # Used IF the areal_sky_flag is 1.
        # Each tuple is a (skyValue, words) pair such that if the
        # sky percentage with the highest areal coverage exceeds
        # the given skyValue, the associated words are used.
        return [
            (80, "low clouds"),
            (40, "areas of clouds"),
            (9,  "patchy clouds"),
            ]

    def areal_skyRelatedWx(self, tree, node):
        # Used IF the areal_sky_flag is 1.
        # Weather types that are related to sky cover and will be included in the
        # sky phrase if their areal coverage matches the sky areal coverage.
        # For example: AREAS OF LOW CLOUDS AND FOG IN THE MORNING...THEN MOSTLY SUNNY.
        return ["F", "L"]

    def disableSkyRelatedWx(self, tree, node):
        # Disable the areal_skyRelatedWx subkeys for the given node
        wxStats = tree.stats.get("Wx", node.getTimeRange(), node.getAreaLabel(),
                                 mergeMethod="Average")
        if wxStats is None:
            return
        disabled = node.getAncestor("disabledSubkeys")
        if disabled is None:
            disabled = []
        #print "wxStats", wxStats
        for wx in self.areal_skyRelatedWx(tree, node):
            for subkey, rank in wxStats:
                if subkey.wxType() == wx:
                    disabled.append(subkey)
        node.set("disabledSubkeys", disabled)
    
    def sky_phrase(self):
        return {
            "setUpMethod": self.sky_setUp,
            "wordMethod": self.sky_words,
            "phraseMethods": [
                self.checkLocalEffects,
                self.combineSky,
                self.skySpecialCases,
                self.combineWords,
                self.fillNulls,
                self.timeDescriptorModeration,
                self.sky_timeDescriptorModeration,
                self.assembleSubPhrases,
                self.postProcessPhrase,
             ]
            }
    def sky_setUp(self, tree, node):
        sky = self.ElementInfo("Sky", "List")
        elementInfoList = [sky]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)        
        return self.DONE()

    def combineSky(self, tree, node):
        return self.combineChildren(tree, node, self.combine_sky)    
    def combine_sky(self, tree, node, subPhrase1, subPhrase2):        
        skyValue1 = self.getScalarData(tree, subPhrase1, "Sky", "MinMax")
        skyValue2 = self.getScalarData(tree, subPhrase2, "Sky", "MinMax")
        if skyValue1 is None and skyValue2 is None:
            return 1, None
        if skyValue1 is None or skyValue2 is None:
            return 0, None
        timeRange = node.getTimeRange()
        if timeRange.duration() > 12*3600:
            dayNight = -1
        else:
            dayNight = self.getPeriod(timeRange, 1)
        words1 = self.sky_value(tree, subPhrase1, self.getValue(skyValue1), dayNight)
        words2 = self.sky_value(tree, subPhrase2, self.getValue(skyValue2), dayNight)
        if self.similarSkyWords_flag(tree, subPhrase1, words1, words2):
            min1, max1 = skyValue1
            min2, max2 = skyValue2
            newVal = (min(min1, min2), max(max1, max2))
            elementInfoList = node.get("elementInfoList")
            newSubPhrase = self.combine2SubPhrases(
                tree, node, subPhrase1, subPhrase2, elementInfoList, newVal)            
            return 1, newSubPhrase
        else:
            return 0,  None            
                        
    def skySpecialCases(self, tree, node):
        # If phrase has exactly 2 subphrases,
        # Look for clearing.
        # If not, then if reportIncreasingDecreasing,
        #  report increasing/decreasing wording.
        subPhrases = node.get("childList")
        if len(subPhrases) == 2:
            words = None
            skyValue1 = self.getScalarData(tree, subPhrases[0], "Sky", "Average")
            skyValue2 = self.getScalarData(tree, subPhrases[1], "Sky", "Average")
            # Look for clearing
            clearing_threshold = self.clearing_threshold(tree, node)
            if skyValue1 > skyValue2 and skyValue2 <= clearing_threshold and skyValue1 > clearing_threshold:
                period1Phrase = self.timePeriod_descriptor(tree, node, subPhrases[0].getTimeRange())
                period1Phrase = self.addSpace(period1Phrase, "leading")
                timeRange = node.getTimeRange()
                if timeRange.duration() > 12*3600:
                    dayNight = -1
                else:
                    dayNight = self.getPeriod(timeRange, 1)
                words1 = self.sky_value(tree, subPhrases[0], skyValue1, dayNight)
                words = words1 + period1Phrase + " then clearing"
            else:
                reportIncreasingDecreasing = self.reportIncreasingDecreasingSky_flag(tree, node)
                if reportIncreasingDecreasing:
                    if skyValue2 > skyValue1:
                        words = "increasing clouds"
                    else:
                        words = "decreasing clouds"
            if words is not None:
                # End processing of the phrase; we are done
                node.set("doneList", node.get("methodList"))
                return self.setWords(node, words)
        return self.DONE()
 
    def sky_timeDescriptorModeration(self, tree, node):
        # If only two subphrases, turn off second time descriptor
        # 
        childList = node.get("childList")
        length = len(childList)
        # Check for words
        if length > 0:
            words = childList[0].get("words")
            if words is None:
                return
        else:
            return self.DONE()
        if length == 2:
            words0 = childList[0].get("words")
            words1 = childList[1].get("words")
            if words0 != "" and words1 != "":
                # Neither is null
                flag0 = 1
                flag1 = 0
            else:  # One is null
                flag0 = 1
                flag1 = 1
                if words0 == "":  # First sub-phrase is null
                   childList[1].set("words", "becoming " + words1)
            childList[0].set("timeDescFlag", flag0)
            childList[1].set("timeDescFlag", flag1)
        return self.DONE()

    def sky_words(self, tree, node):
        # Create sky phrase.                
        statDict = node.getStatDict()
        sky = self.getStats(statDict, "Sky")
        if sky is None:
            return self.setWords(node, "")
         
        # Check Pop i.e. don't report sky if we can assume overcast
        threshold = self.pop_sky_lower_threshold(tree, node)
        if self.lowPop_flag(tree, node, threshold) == 0:
            return self.setWords(node, "")
                        
        sky = self.getValue(sky)
        timeRange = node.getTimeRange()
        if timeRange.duration() > 12*3600:
            words = self.getSkyDiurnalWords(tree, node)
            if words is not None:
                return self.setWords(node, words)
            dayNight = -1
        else:
            dayNight = self.getPeriod(timeRange, 1)
        words = self.sky_value(tree, node, sky, dayNight)
        return self.setWords(node, words)

    def getSkyDiurnalWords(self, tree, node):
        # Produce words such as
        #   xx in the night and morning otherwise yy
        # where xx is the sky value for the night and morning
        # and yy is the sky value otherwise
        #
        # If the night and morning words are the same as the
        # evening and afternoon, (no diurnal pattern),
        #    return None

        # If we have not tested for diurnal sky and wx, return
        if "DiurnalSkyWx" not in self.periodCombining_elementList(tree, node):
            return None

        wordList = []
        index = 0
        trList = self.divideRange(node.getTimeRange(), 6)
        dayNight = self.getPeriod(trList[0], 1)
        # Need to save timeRange so we can re-set it for determining
        # words for sub-ranges
        saveTR = node.getTimeRange()
        # Only need to use first 12 hours to check for similarity
        for tr in trList[0:2]:
            sky = tree.stats.get("Sky", tr, node.getAreaLabel(),
                                 mergeMethod="Average")
            sky = self.getValue(sky)
            node.timeRange = tr
            result = self.sky_value(tree, node, sky, dayNight)
            wordList.append(result)
            #print "\nsky, tr", sky, tr
            #print "words", result
            index += 1
        # Re-set timeRange
        node.timeRange = saveTR
        #print "\nwordList", wordList
        if wordList[0] == wordList[1]:
            return None
        if dayNight == self.DAYTIME():
            # First period is the morning
            words1 = wordList[0]
            words2 = wordList[1]
            descriptor = " in the morning and night"
        else:
            # First period is the evening
            words1 = wordList[1]
            words2 = wordList[0]
            descriptor = " in the night and morning"
        words2 = words2.replace("sunny", "clear")
        words = words1 + descriptor + "...otherwise " + words2
        #print "returning", words
        return words
                 
    def simple_sky_phrase(self):
        return {
            "phraseMethods": [
                self.simple_sky_words, # phrase.words
            ],
            }    
    def simple_sky_words(self, tree, phrase):
        # Create sky phrase.
                
        # If no information, do not report sky condition
        timeRange = phrase.getTimeRange()
        #print "Getting sky"
        skyStats = tree.stats.get("Sky", timeRange, phrase.getAreaLabel(), mergeMethod="List")
        #print "Sky ", skyStats
        statsByRange = self.makeRangeStats(tree, self.SCALAR(), skyStats, timeRange)
        #print "Sky ", statsByRange
        if statsByRange is None:
            return self.setWords(phrase, "")
         
        # Check Pop i.e. don't report sky if we can assume overcast
        threshold = self.pop_sky_lower_threshold(tree, phrase)
        if self.lowPop_flag(tree, phrase, threshold) == 0:
            return self.setWords(phrase, "")
        
        reportIncreasingDecreasing = self.reportIncreasingDecreasingSky_flag(tree, phrase)
        
        # Get values for each part of time range
        if len(statsByRange) == 1:
            skyTime1, period1 = statsByRange[0]
            skyTime2, period2 = statsByRange[0]
        else:
            skyTime1, period1 = statsByRange[0]
            skyTime2, period2 = statsByRange[1]
        
        skyTime1 = self.getValue(skyTime1)
        skyTime2 = self.getValue(skyTime2)

        dayNight1 = self.getPeriod(period1, 1)
        dayNight2 = self.getPeriod(period2, 1)
        
        # Determine category and phrase for skyTime1 and skyTime2
        index = 1
        for skyValue, dayNight in [(skyTime1, dayNight1), (skyTime2, dayNight2)]:
            skyPhrase, valueIndex = self.sky_value(tree, phrase, skyValue, dayNight, 1)
            exec "words"+`index`+"=skyPhrase"
            exec "index"+`index`+"=valueIndex"
            index = index+1
            
        period1Phrase = self.timePeriod_descriptor(tree, phrase, period1)
        period1Phrase = self.addSpace(period1Phrase, "leading")

        # Look for clearing
        clearing_threshold = self.clearing_threshold(tree, phrase)
        if skyTime1 > skyTime2 and skyTime2 <= clearing_threshold and skyTime1 > clearing_threshold:
            return self.setWords(phrase, words1 + period1Phrase + " then clearing")
             
        # See if skyTime1 is different from skyTime2 by more than
        # one category of sky values
        if abs(index1 - index2) > self.sky_index_difference(tree, phrase):
            if reportIncreasingDecreasing == 1:
                if skyTime2 > skyTime1:
                    return self.setWords(phrase, "increasing clouds")
                else:
                    return self.setWords(phrase, "decreasing clouds")
            else:
                return self.setWords(phrase, words1 + period1Phrase + " then becoming " + words2)
       # Report Average value
        else:
            skyValue = self.average(skyTime1, skyTime2)
            if timeRange.duration() > 12*3600:
                dayNight = -1
            else:
                dayNight = self.getPeriod(timeRange, 1)
            words = self.sky_value(tree, phrase, skyValue, dayNight)
            return self.setWords(phrase, words)
 
    # PoP
    def wxQualifiedPoP_flag(self, tree, node):
        # If 1, PoP phrases will be qualified with the weather type
        # E.g. "Chance of rain and snow 20 percent." instead of
        #      "Chance of precipitation 20 percent."
        return 1
    
    def popMax_phrase(self):
        return {
            "setUpMethod": self.popMax_setUp,
            "wordMethod": self.popMax_words,
            "phraseMethods": self.standard_phraseMethods()  
            }    
    def popMax_setUp(self, tree, node):
        # NOTE:  The method is set to "Average" instead of "List" so
        # that the PoP phrase will always cover the full period.
        # It doesn't matter what method (other than List) we choose
        # since the popMax_words method gets its PoP value directly from
        # the "matchToWx" method.
        elementInfoList = [self.ElementInfo("PoP", "Average")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)      
        return self.DONE()
    

    def popMax_words(self, tree, node) :
        "Create phrase Probability of Precipitation for maximum value"
        # Wait for weather phrase to complete
        wxWords = ""
        attrDict = {}
        if self.wxQualifiedPoP_flag(tree, node) == 1:
            compArea = node.getComponent().getAreaLabel()
            wxWords, attrDict = self.findWords(tree, node, "Wx", [node.getAreaLabel(), compArea],
                                               phraseList=["weather_phrase", "skyPopWx_phrase"],
                                               attributes=['reportedRankList'])
            if wxWords is None:
               return
            #print "wxWords", wxWords
            if wxWords == "":
                #print "setting popMax to Null"
                return self.setWords(node, "null")

        #print "PopMax", node.getAreaLabel(), wxWords
        pop = self.matchToWx(tree, node, "PoP")
        #print " Pop", pop
        if pop is None:
            return self.setWords(node, "")

        # Check pop thresholds
        pop = self.getValue(pop, "Max")
        if pop < self.pop_lower_threshold(tree, node) or \
           pop > self.pop_upper_threshold(tree, node):
            return self.setWords(node, "")

        popType = self.getPopType(tree, node, pop, wxWords, attrDict)
        node.set("popType", popType)
        result = self.checkRepeatingString(tree, node, popType, "popType",0)
        if result == -1:
            # Wait for previous phrase to finish
            return 
        popType = self.addSpace(result)

        unit = self.units_descriptor(tree, node, "unit", "percent")
        popStr = self.getPopStr(tree, node, pop)
        words = popType + popStr + " " + unit
 
        # Need to try and set phrase descriptor at this point since
        # weather phrase was not complete during phrase set-up
        phrase = node.parent
        if phrase.get("descriptor") is None:
            descriptor = self.phrase_descriptor(tree, phrase, "PoP", "PoP")
            phrase.set("descriptor", descriptor)
 
        return self.setWords(node, words)

    def getPopStr(self, tree, node, pop):
        pop = int(pop)
        if pop >= 100:
            popWords = "near 100"
        else:
            popWords = `pop`
        return popWords
            
    def getPopType(self, tree, node, pop, wxWords, attrDict):
        popType = "precipitation"
        if self.wxQualifiedPoP_flag(tree, node) == 1:
            #  Examine reported weather type(s) from phrase.
            #  If there is more than one descriptor for precipitating weather
            #   or if they are general weather types,
            #     return "precipitation"
            #  Otherwise, describe the weather type
            #     e.g. chance of rain, chance of snow
            wxTypes = []
            if attrDict.has_key("reportedRankList"):
                rankList = attrDict["reportedRankList"]
                for subkey, rank in rankList:
                    wxTypes.append(subkey.wxType())
            generalTypes = ["IP", "ZL", "ZR", "ZF", "ZY"]
            for general in generalTypes:
                if general in wxTypes:
                    return "precipitation"
            descriptors = {
                "R": "rain",
                "RW": "showers",
                "S": "snow",
                "SW": "snow",
                "T": "thunderstorms",
                }
            popTypes = []
            for wxType in wxTypes:
                if wxType in ["R", "S", "RW", "SW", "T"]:
                    desc = descriptors[wxType]
                    if desc not in popTypes:
                        popTypes.append(desc)
            if len(popTypes) > 1:
                popType = "precipitation"
            elif len(popTypes) == 1:
                popType = popTypes[0]
        return popType

    # This version will report only the weather types that
    # match the reported PoP
##    def getPopType(self, tree, node, pop, wxWords, attrDict):
##        popType = "precipitation"
##        if self.wxQualifiedPoP_flag(tree, node) == 1:
##            ## Need to find weather type(s) from phrase.
##            ## "wxWords" is the concatenation of all weather phrases
##            ## for this component.
##            ## Returns "popType" e.g. chance of rain, chance of rain and snow
##            wxTypes = []
##            if attrDict.has_key("reportedRankList"):
##                rankList = attrDict["reportedRankList"]
##                for subkey, rank in rankList:
##                    # Check the coverage against the reported PoP
##                    covLow, covHigh = self.coveragePoP_value(subkey.coverage())
##                    if covHigh >= pop:
##                        wxTypes.append(subkey.wxType())
##            popType = None
##            generalTypes = ["IP", "ZL", "ZR", "ZF", "ZY"]
##            for general in generalTypes:
##                if general in wxTypes:
##                    popType = "precipitation"
##            if popType is None:
##                rain = 0
##                snow = 0
##                thunder = 0
##                showers = 0
##                snowShowers = 0
##                rainShowers = 0
##                if "R" in wxTypes:
##                    rain = 1
##                if "S" in wxTypes:
##                    snow = 1
##                if "RW" in wxTypes:
##                    showers = 1
##                if "SW" in wxTypes:
##                    snowShowers = 1
##                if "T" in wxTypes:
##                    thunder = 1
##                if showers and not snowShowers:
##                    rainShowers = 1
##                if (rain or rainShowers or thunder) and snow:
##                    popType = "precipitation"
##                else:
##                    if snow or snowShowers:
##                        if rain or rainShowers:
##                            if wxWords.find(" or ") > -1:
##                                popType = "rain or snow"
##                            else:
##                                popType = "rain and snow"                    
##                        else:
##                            popType = "snow"
##                    elif rain and not rainShowers:
##                        popType = "rain" 
##                    elif showers:
##                        popType = "showers" 
##                        if thunder:
##                            popType = "showers and thunderstorms"
##                    elif thunder:
##                        popType = "thunderstorms"
##                    else:
##                        popType = "precipitation"
##                if popType is None:
##                    popType = "precipitation"
##        return popType
    
    def areal_or_chance_pop_descriptor(self, tree, node, key, elementName):
        # Stats: dominantWx 
        # Returns descriptor for a pop phrase based on Wx
        # Returns   areal coverage of precipitation OR
        #           chance of precipitation
        # Get weather.  Determine if ANY terms in the period are convective. If so,
        # change the phrase to "areal coverage".  This is an Amarillo WFO
        # preference.
        wxPhrase = self.findWords(tree, node, "Wx", node.getAreaLabel(),
                                  phraseList=["weather_phrase", "skyPopWx_phrase"])
        if wxPhrase is None:
            return None
        if wxPhrase == "":
            return "chance of"
        use_areal = 0

        if wxPhrase.find("isolated") >= 0:
            use_areal = 1
        if wxPhrase.find("scattered") >= 0:
            use_areal = 1
        if wxPhrase.find("numerous") >= 0:
            use_areal = 1
        if wxPhrase.find("widespread") >= 0:
            use_areal = 1

        if use_areal == 1:
            return "areal coverage of" 
        else:
            return "chance of"
        
    def allAreal_or_chance_pop_descriptor(self, tree, node, key, elementName):
        # Stats: rankedWx 
        # Returns descriptor for a pop phrase based on Wx
        # Returns   areal coverage of precipitation OR
        #           chance of precipitation
        # Get weather.  Determine if ALL terms in the period are convective. If so,
        # change the phrase to "areal coverage".  This is an Amarillo WFO
        # preference.
        statsByRange =  tree.stats.get(
            "Wx", node.getTimeRange(), node.getAreaLabel(), mergeMethod="List")
        if statsByRange is None:
            return "chance of"
        use_areal = 1

        for rankList, subRange in statsByRange:
            subkeys = self.getSubkeys(rankList)
            for subkey in subkeys:
                if self.precip_related_flag(tree, node, subkey):
                    cov = subkey.coverage()                                
                    if cov not in ["Iso", "Sct", "Num", "Wide", "<NoCov>"]:
                        use_areal = 0
                        break

        if use_areal == 1:
            return "areal coverage of" 
        else:
            return "chance of"

    # Temperature worded phrases:
    #     HIGHS IN THE MIDDLE 80S
    #     HIGHS IN THE MIDDLE 80S TO LOWER 90S
    #  using temp_phrase_threshold
 
    def highs_phrase(self):
        return {
            "setUpMethod": self.highs_setUp,
            "wordMethod": self.temp_words,
            "phraseMethods": self.standard_phraseMethods() 
            }    
    def highs_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("MaxT", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)    
        return self.DONE()
    
    def lows_phrase(self):
        return {
            "setUpMethod": self.lows_setUp,
            "wordMethod": self.temp_words,
            "phraseMethods": self.standard_phraseMethods(),  
            }    
    def lows_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("MinT", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)    
        return self.DONE()
        
    def temp_words(self, tree, node):
        stats = self.getTempStats(tree, node)
        if stats is None:
            return self.setWords(node, "")
        elementName = node.getAncestor("elementName")
        words =  self.getTempPhrase(tree, node, stats, elementName)
        return self.setWords(node, words)

    def tempDiff_threshold(self, tree, node):
        # If the difference between the minimum and maximum temperature values
        # exceeds this range, report the actual values e.g. 23 to 29.
        return 4

    def getTempPhrase(self, tree, node, temp, elementName):
        minVal, maxVal = self.getValue(temp, "MinMax")
        minVal = int(minVal)
        maxVal = int(maxVal)

        # Chris Gibson's version
##        # Handle teens
##        ave = int((maxVal + minVal)/2)
##        if ave < 20 and ave > 9:
##            if ave > 15:
##                return "15-20"
##            elif ave == 15 or ave == 14:
##                return "near 15"
##            elif ave == 10 or ave == 11:
##                return "near 10"
##            else:
##                return "10-15"
##        if ave < 10 and ave > 0:
##            if ave > 4:
##                return "5 to 10 above"
##            else:
##                return "zero to 5 above"
        
##        if minVal <=0 or maxVal <=0:
##            maxVal = int(self.round(maxVal, "Nearest", 5))
##            minVal = int(self.round(minVal, "Nearest", 5))

        # End Chris Gibson's version
                
        # Check for exceptions
        exceptions = self.tempPhrase_exceptions(tree, node)
        for minBoundaries, maxBoundaries, equalityPhrase, phrase in exceptions:
            if minVal >= minBoundaries[0] and minVal <= minBoundaries[1] and \
               maxVal >= maxBoundaries[0] and maxVal <= maxBoundaries[1]:
                if minVal == maxVal:
                    resultPhrase = equalityPhrase
                else:
                    resultPhrase = phrase
                return self.constructTempException(resultPhrase, minVal, maxVal)
            
        # Handle actual range values
        if abs(maxVal-minVal) > self.tempDiff_threshold(tree, node):
            return `minVal` + " to " + `maxVal`
        
        # set up for "lower," "mid," or "upper" wording
        # Modulus (%) gets tricky below zero so have to take
        # modulus of abs(temperature)
        decadeMaxStr = self.getDecadeStr(maxVal)
        decadeMinStr = self.getDecadeStr(minVal)        
        digitMax = abs(maxVal) % 10
        digitMin = abs(minVal) % 10
        boundaries = self.tempPhrase_boundary_dict(tree, node)
        digitMinStr = self.getDigitStr(digitMin, boundaries)
        digitMaxStr = self.getDigitStr(digitMax, boundaries)
        lowerMax = boundaries["lower"][1]
        upperMin = boundaries["upper"][0]
        if decadeMinStr == decadeMaxStr:
            # this solves the problem of returning "...IN THE LOWER 60s TO LOWER 60s..."
            if digitMinStr == digitMaxStr:
                return "in the " + digitMinStr + " " + decadeMinStr

            # shortens a return of "...lower to upper..." to "...in the xxS"
            elif digitMin <= lowerMax and digitMax >= upperMin:
                return "in the " + decadeMaxStr

            else:
                return "in the " + digitMinStr + " to " + digitMaxStr + " " + decadeMaxStr
        elif digitMinStr == digitMaxStr:
            # return 50s TO LOWER 60s (not LOWER 50s TO LOWER 60s)
            return "in the " + decadeMinStr + " to " + digitMaxStr + " " + decadeMaxStr
        else:  # different decade
            if maxVal >= 100 and minVal < 100:  # UPPER 80s to 102
                return digitMinStr + " " + decadeMinStr + " to " + str(maxVal)
            # return NEAR 60 (not UPPER 50s TO LOWER 60s)
            elif digitMin >= upperMin and digitMax <= lowerMax and maxVal - minVal <= 10:
                roundedMax = int(self.round(maxVal, "Nearest", 10))
                return self.constructTempException("near %max", minVal, roundedMax)
            # return 50s AND 60s (not LOWER 50s TO UPPER 60s)
            elif digitMin <= lowerMax and digitMax >= upperMin:
                return "in the " + decadeMinStr + " to " + decadeMaxStr
            digitMinPhrase = digitMinStr + " " + decadeMinStr
            digitMaxPhrase = digitMaxStr + " " + decadeMaxStr
            return "in the " + digitMinPhrase + " to " + digitMaxPhrase        

    def constructTempException(self, phrase, minVal, maxVal):
       phrase = phrase.replace("%min", `minVal`)
       phrase = phrase.replace("%max", `maxVal`)
       zeroPhraseMin = self.getZeroPhrase(minVal)
       zeroPhraseMax = self.getZeroPhrase(maxVal)
       phrase = phrase.replace("%zeroPhraseMin", zeroPhraseMin)
       phrase = phrase.replace("%zeroPhraseMax", zeroPhraseMax)
       return phrase

    def getDecade(self, value):
        decade = abs(int(value)) / 10 * 10
        if value < 0:
            decade = -decade
        return decade       
       
    def getDecadeStr(self, value):
        decade = self.getDecade(value)
        if decade == 0:
            return "single digits"
        elif decade == 10:
            return "teens"
        elif decade == -10:
            return "teens below zero"
        else:
            return `decade` + "s"
        
    def getDigitStr(self, value, boundaries):
        for key in boundaries.keys():
            lower, upper = boundaries[key]
            if value >= lower and value <= upper:
                return key        
  
    def tempPhrase_exceptions(self, tree, node):
       # These exceptions to the getTempPhrase are processed before trying to
       # generate a phrase such as "in the lower 20's to upper 30's".
       return [
           # Boundaries are inclusive
           # Min boundaries # Max boundaries # phrase if Min == Max # phrase if Min != Max
           #   %min will be replaced by the minimum temperature value
           #   %max will be replaced by the maximum temperature value
           #   %zeroPhraseMin will be replaced with a zero-based phrase for the min e.g.
           #        12 below
           #   %zeroPhraseMax will be replaced with a zero-based phrase for the min e.g.
           #        5 above             
           
           # Both 100 and above
           [(100,200), (100,200), "around %min", "%min to %max"],
           # Min in 90's, Max 100 and above
           [(90, 99), (100,200), "", "%min to %max"],

           # Handle lower temperatures
           [(1, 19), (1, 29),     "around %min", "%min to %max"],
           # Handle zero temperatures
           [(0, 0), (0, 29),     "near zero", "zero to %zeroPhraseMax"],
           [(-200, 0), (0, 0),     "near zero", "%zeroPhraseMin to zero"],
           
           # Min below zero, Max above zero
           [(-200,-1), (1,200),    "near zero", "%zeroPhraseMin to %zeroPhraseMax zero"],
           # Both below zero
           #[(-200,-1), (-200,-1),   "%zeroPhraseMin","%zeroPhraseMax to %zeroPhraseMin zero"],
           [(-200,-1), (-200,-1),   "around %zeroPhraseMin","%zeroPhraseMax to %zeroPhraseMin zero"],
           
##           # Chris Gibson's version Comment out the above exception and use this instead:
##           #[(-200,-1), (-200,-1),   "near %zeroPhraseMax","%zeroPhraseMax to %zeroPhraseMin zero"]

           # Around phrases fix from Steve Nelson
           [(20, 20), (20, 20), "around %min", "%min to %max"],
           [(30, 30), (30, 30), "around %min", "%min to %max"],
           [(40, 40), (40, 40), "around %min", "%min to %max"],
           [(50, 50), (50, 50), "around %min", "%min to %max"],
           [(60, 60), (60, 60), "around %min", "%min to %max"],
           [(70, 70), (70, 70), "around %min", "%min to %max"],
           [(80, 80), (80, 80), "around %min", "%min to %max"],
           [(90, 90), (90, 90), "around %min", "%min to %max"],           

           ]
         
    def tempPhrase_boundary_dict(self, tree, node):
       return {
           "lower": (0,3),
           "mid":   (4,6),
           "upper": (7,9),
           }

    # Temperature worded phrases:
    #     HIGHS 45 TO 50
    #  using range_nlValue for "MinT", "MaxT"
    
    def highs_range_phrase(self):
        return {
            "setUpMethod": self.highs_setUp,
            "wordMethod": self.tempRange_words,
            "phraseMethods": self.standard_phraseMethods(),
            }
    
    def lows_range_phrase(self):
        return {
            "setUpMethod": self.lows_setUp,
            "wordMethod": self.tempRange_words,
            "phraseMethods": self.standard_phraseMethods(),
            }    

    def tempRange_words(self, tree, node) :
        "Create phrase for Min or Max Temperature"
        stats = self.getTempStats(tree, node)
        if stats is None:
            return self.setWords(node, "")
        elementName = node.getAncestor("elementName")
        words =  self.getTempRangePhrase(tree, node, stats, elementName)
        return self.setWords(node, words)

    def getTempRangePhrase(self, tree, node, temp, elementName):        
        connector = self.value_connector(tree, node, elementName, elementName)
        min, max = self.getValue(temp, "MinMax")

        decadeMax  =  self.getDecade(max)
        digitMax = max % 10
        decadeMin  = self.getDecade(min) 
        digitMin = min % 10
        diff = abs(max - min)

        # "Around" phrases
        #  e.g. a range of 19-21 --> "highs around 20"
        around = self.addSpace(self.phrase_descriptor(tree, node, "around", elementName))
        if 0 < diff <= 3 and (digitMax == 0 or digitMax == 1):
            if decadeMax <= 10:
                decadeMax = self.getZeroPhrase(decadeMax)
            else:
                decadeMax = `decadeMax`
            return around + decadeMax

        # Report the range
        min = int(min)
        max = int(max)
        if min == max:
            # Adjust descriptor e.g. highs --> high
            descriptor = node.parent.get("descriptor")
            if descriptor is not None and around == "":
                descriptor = descriptor.replace("s", "")
                node.parent.set("descriptor", descriptor)
            if min <= 10: 
                min = self.getZeroPhrase(min) 
            else: 
                min = `min` 
            return around + min
        elif min > 0 and max > 0:
            return `min` + connector + `max`
        elif min <= 0 and max > 0:
            minval = self.getZeroPhrase(min)
            maxval = self.getZeroPhrase(max, 1)
            return minval + " to " + maxval
        else:
            if min < 0 and max < 0:
                firstVal = self.getZeroPhrase(max)
                secondVal = self.getZeroPhrase(min, 1)
            else:
                firstVal = self.getZeroPhrase(min)
                secondVal = self.getZeroPhrase(max, 1)
            return firstVal + connector + secondVal

    def getZeroPhrase(self, val, addZero=0):
        if val == 0:
            return "zero"
        if val < 0:
            phrase = `abs(val)` + " below"
        else:
            phrase = `val` + " above"
        if addZero == 1:
            phrase = phrase + " zero"
        return phrase

    # Extended Temperatures
    def extended_temp_range(self, tree, node):
        # Range for extended temperatures e.g.
        #    "Highs 45 to 55."
        # This value must be 10 or 5.
        # Other values are not supported for extended ranges.
        return 10
        #return 5

    def extended_highs_phrase(self):
        return {
            "setUpMethod": self.highs_setUp,
            "wordMethod": self.extended_temp_words,
            "phraseMethods": self.standard_phraseMethods(),  
            }    
    def extended_lows_phrase(self):
        return {
            "setUpMethod": self.lows_setUp,
            "wordMethod": self.extended_temp_words,
            "phraseMethods": self.standard_phraseMethods(),  
            }    
       
    def extended_temp_words(self, tree, node) :
        "Create phrase for Min or Max Temperature"
        stats = self.getTempStats(tree, node)
        if stats is None:
            return self.setWords(node, "")
        elementName = node.get("elementName")
        if elementName == "MaxT":
            mergeMethod = "Max"
        else:
            mergeMethod = "Min"
        temp = int(self.getValue(stats, mergeMethod))
        words = self.getExtendedTempPhrase(tree, node, temp)
        return self.setWords(node, words)

    def getExtendedTempPhrase(self, tree, node, temp):
        # Temperatures above 99
        # Give exact value
        if temp > 99:
            if node.getIndex() == 0:
                parent = node.getParent()
                descriptor = parent.get("descriptor")
                descriptor = descriptor.replace("s ", " ")
                parent.set("descriptor", descriptor)
            return `int(temp)`

        # Temperatures below 10 
        # Build and return special phrases
        if temp < -27:
            return "25 below to 35 below"
        elif temp < -22:
            return "20 below to 30 below"
        elif temp < -17:
            return "15 below to 25 below"
        elif temp < -12:
            return "10 below to 20 below"
        elif temp < -7:
            return "5 below to 15 below"
        elif temp < -2:
            return "zero to 10 below"
        elif temp < 3:
            return "5 below zero to 5 above"
        elif temp < 8:
            return "zero to 10 above"
        elif temp < 10:
            return "5 to 15"

        # Determine modifier for temperature: around, lower, mid, upper
        decade  =  self.getDecade(temp)
        digit = temp % 10

        range = self.extended_temp_range(tree, node)
        if range == 10:
            if digit >= 0 and digit <= 2:
                phrase = self.getExtTemp(decade-5, decade+5)
            elif digit >= 3 and digit <= 7:
                if decade == 10:
                    phrase = "in the " + "teens"
                elif decade <= 0 or decade >= 100:
                    phrase = self.getExtTemp(decade, decade+10)
                else:
                    phrase = "in the " + `decade` + "s"
            elif digit >= 8 and digit <=9:
                phrase = self.getExtTemp(decade+5, decade+15)
        else:  # Assume range of 5
            if digit >= 0 and digit <= 2:
                phrase = self.getExtTemp(decade, decade+5)
            elif digit >= 3 and digit <= 7:
                if decade == 10:
                    phrase = "in the " + "teens"
                elif decade <= 0 or decade >= 100:
                    phrase = self.getExtTemp(decade, decade+5)
                else:
                    phrase = "in the " + `decade` + "s"
            elif digit >= 8 and digit <=9:
                phrase = self.getExtTemp(decade+5, decade+10)

        return phrase

    def getExtTemp(self, val1, val2):
        v1 = `val1`
        if val1 < 0:
            v1 = v1 + " below"
        v2 = `val2`
        if val2 < 0:
            v2 = v2 + " below"
        return v1 + " to " + v2
    
    def getTempStats(self, tree, node):
        "Get correct Temperature stats (MaxT or MinT) to determine temperature phrase"
        elementName = node.getAncestor("elementName")
        timeRange = node.getTimeRange()
        areaLabel = node.getAreaLabel()
        day = self.getPeriod(timeRange, 1)        
        #  day is 1=DAYTIME or 0=NIGHTTIME or -1=DAYNIGHT (spans both day and night)
        #  In the normal case, MaxT is greater than MinT and:
        #     for highs, return MaxT
        #     for lows, return  MinT
        #  If, however, MaxT is less than MinT, then MaxT and MinT have to be switched

        # Don't do highs at night or lows in the day
        if elementName == "MaxT":
            dayValue = self.DAYTIME()
        else:
            dayValue = self.NIGHTTIME()
        if not day == self.DAYNIGHT() and not day == dayValue:
            return None

        if timeRange.duration() <= 12*3600:
            statDict = node.getStatDict()
            stats = self.getStats(statDict, elementName)
            return stats
        else:        
            # If the time period spans day and night,
            # get the conglomerate stats.
            maxT = tree.stats.get("MaxT", timeRange, areaLabel,
                                  mergeMethod="MinMax")
            minT = tree.stats.get("MinT", timeRange, areaLabel,
                                  mergeMethod="MinMax")
            if maxT is None and minT is None:
                return None
            if maxT is None:
                if dayValue == self.DAYTIME():
                    return None
                else:
                    return minT
            if minT is None:
                if dayValue == self.NIGHTTIME():
                    return None
                else:
                    return maxT
            # Check for case of MaxT < MinT
            max = self.getValue(maxT, "Max")
            min = self.getValue(minT, "Max")
            if max < min:
                temp = maxT
                maxT = minT
                minT = temp
            if dayValue == self.DAYTIME():
                return maxT
            else:
                return minT

    def temp_trends_addToPhrase_flag(self, tree, node):
        # If set to 0, will report:
        # "Temperatures falling in the afternoon."
        # If set to 1:
        # "Temperatures falling to the 50's in the afternoon."
        # If set to 2:
        # "Temperatures falling to the lower 50's in the afternoon."
        return 2
    
    def temp_trends(self):
        return {
            "setUpMethod": self.temp_trends_setUp,
            "wordMethod": self.temp_trends_words,
            "phraseMethods": self.standard_phraseMethods(),  
            }    
    def temp_trends_setUp(self, tree, node):
        duration = node.getTimeRange().duration()
        if duration > 12*3600:
           return self.setWords(node, "")
        timeRange = node.getTimeRange()
        dayNight = self.getPeriod(timeRange, 1)
        if dayNight == self.NIGHTTIME():
            eleInfo = self.ElementInfo("MinT", "Min")
        else:
            eleInfo = self.ElementInfo("MaxT", "Max")
        elementInfoList = [eleInfo]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        return self.DONE()
 
    def temp_trends_words(self, tree, node):
       "Look for sharp temperature increases or decreases"
       # Determine if temps rise or fall in a non-diurnal way.
       # MaxT/MinT temps -- min/max tuple for each
       # Hourly Temp Stats: list of hourly temperature tuples
       #    Each tuple has:
       #     -- average temperature value
       #     -- hour of occurrence
       # For a Daytime period, compare MaxT to T for the last grid 
       #   of the period and report "temperatures falling in the afternoon"
       #   if the difference exceeds the temp_trend_nlValue
       # For a Nighttime period, compare MinT to T for the last grid
       #   of the period and report "temperatures rising overnight"
       #   if the difference exceeds the temp_trend_threshold.

       statDict = node.getStatDict()
       timeRange = node.getTimeRange()
       tStats = tree.stats.get("T", timeRange, node.getAreaLabel(),
                               mergeMethod="List") 
       if tStats is None:
           return self.setWords(node, "")
       tStats, subRange = tStats[0]
       if tStats is None:
           return self.setWords(node, "")
       dayNight = self.getPeriod(timeRange,1)
       trend_nlValue = self.temp_trend_nlValue(tree, node)
       if dayNight == self.DAYTIME():
           maxT = self.getStats(statDict, "MaxT")
           if maxT is None:
               return self.setWords(node, "")
           maxT = self.getValue(maxT)
           threshold = self.nlValue(trend_nlValue, maxT)
       else:
           minT = self.getStats(statDict, "MinT")
           if minT is None:
               return self.setWords(node, "") 
           minT = self.getValue(minT)
           threshold = self.nlValue(trend_nlValue, minT)
       halfWay = len(tStats)/2
       
       index = len(tStats)-1
       while index >= halfWay:
           tempValue, curHour = tStats[index]
           if tempValue is None:
               index = index - 1
               continue
           
           if dayNight == self.DAYTIME():
               if tempValue <= (maxT - threshold):
                   toPhrase = self.getToPhrase(tree, node, tempValue)
                   words = "temperatures falling" + toPhrase + " in the afternoon"
                   return self.setWords(node, words)
           else:
               if tempValue >= (minT + threshold):
                   toPhrase = self.getToPhrase(tree, node, tempValue)
                   words = "temperatures rising" + toPhrase + " after midnight"
                   return self.setWords(node, words)
           break           
       return self.setWords(node, "")

    def getToPhrase(self, tree, node, tempValue):
       flag = self.temp_trends_addToPhrase_flag(tree, node)
       if flag > 0:
           if flag > 1:
               rangeStr = self.getDigitStr(
                   abs(tempValue)%10, self.tempPhrase_boundary_dict(tree, node))
               rangeStr += " "
           else:
               rangeStr = ""
           return " into the " + rangeStr + self.getDecadeStr(tempValue)
       else:
           return ""

##     def temp_trends_words(self, tree, node):
##        "Look for sharp temperature increases or decreases"
       
##        # Here is an alternative temp_trends method provided by Tom Spriggs.
##        # If a 12-hour period, it looks at the 12, 3, and 5 o'clock grids
##        # (both am/pm depending on time of day) and verifies the trend (either
##        # going down or up) and then looks at the difference between the 
##        # 5 o'clock grid and the MaxT/MinT grid.  It only needs to look at the
##        # 5 o'clock grid since that is the last one in the 12-hour period, 
##        # and if it is going to trip the threshold anywhere, it will be on that
##        # hour since if you have an unusual temperature trend, it will peak at
##        # that grid.  If less than a 12-hour period, then the 3 times that it
##        # checks will be adjusted accordingly inside the smaller time range.
##        statDict = node.getStatDict()
##        timeRange = node.getTimeRange()
##        tStats = tree.stats.get("T", timeRange, node.getAreaLabel(),
##                                mergeMethod="List") 
##        if tStats is None:
##            return self.setWords(node, "")
##        tStats, subRange = tStats[0]
##        if tStats is None:
##            return self.setWords(node, "")
##        dayNight = self.getPeriod(timeRange,1)
##        trend_nlValue = self.temp_trend_nlValue(tree, node)
##        if dayNight == self.DAYTIME():
##            maxT = self.getStats(statDict, "MaxT")
##            if maxT is None:
##                return self.setWords(node, "")
##            maxT = self.getValue(maxT)
##            threshold = self.nlValue(trend_nlValue, maxT)
##        else:
##            minT = self.getStats(statDict, "MinT")
##            if minT is None:
##                return self.setWords(node, "") 
##            minT = self.getValue(minT)
##            threshold = self.nlValue(trend_nlValue, minT)

##        if len(tStats) >= 6:
##            halfWay    = len(tStats) - 6
##            quarterWay = len(tStats) - 3
##            endPoint   = len(tStats) - 1
##        elif len(tStats) >= 4:
##            halfWay    = 0
##            quarterWay = len(tStats) - 3
##            endPoint   = len(tStats) - 1
##        elif len(tStats) == 1:
##            halfWay    = 0
##            quarterWay = 0
##            endPoint   = 0
##        else:
##            halfWay    = 0
##            quarterWay = 1
##            endPoint   = len(tStats) - 1
           
##        tempValue_halfWay, curHour1    = tStats[halfWay]
##        tempValue_quarterWay, curHour2 = tStats[quarterWay]
##        tempValue_endPoint, curHour3   = tStats[endPoint]
       
##        if tempValue_halfWay is None:
##            return self.setWords(node, "")
##        if tempValue_quarterWay is None:
##            return self.setWords(node, "")
##        if tempValue_endPoint is None:
##            return self.setWords(node, "")

##        words = ""
##        if dayNight == self.DAYTIME():
##            if tempValue_quarterWay < tempValue_halfWay:
##                if tempValue_endPoint <= tempValue_quarterWay:
##                    if tempValue_endPoint <= (maxT - threshold):
##                        # large temp fall (i.e. >= threshold)
##                        toPhrase = self.getToPhrase(tree, node, tempValue_endPoint)
##                        mxPhrase = self.getToPhrase(tree, node, maxT)
##                        if (toPhrase == mxPhrase):
##                            # avoid saying--"high in the upper 50s. temperature falling
##                            # into the 50s in the afternoon."
##                            # instead say--"high in the upper 50s. temperature falling
##                            # through the 50s in the afternoon."
##                            toPhrase = " through" + toPhrase[5:]
##                        if len(tStats) <= 6:   #assumes already in the afternoon
##                            words = "temperature falling" + toPhrase + " by late afternoon"
##                        else:                           
##                            words = "temperature falling" + toPhrase + " in the afternoon"
##                    elif tempValue_endPoint < maxT:
##                        # small temp fall (i.e. < threshold)
##                        if len(tStats) <= 6:   #assumes already in the afternoon
##                            words = "temperature steady or slowly falling through late afternoon"
##                        else:
##                            words = "temperature steady or slowly falling in the afternoon"
##        else:
##            if tempValue_quarterWay > tempValue_halfWay:
##                if tempValue_endPoint >= tempValue_quarterWay:
##                    if tempValue_endPoint >= (minT + threshold):
##                        # large temp rise (i.e. >= threshold)
##                        toPhrase = self.getToPhrase(tree, node, tempValue_endPoint)
##                        mnPhrase = self.getToPhrase(tree, node, minT)
##                        if (toPhrase == mnPhrase):
##                            # avoid saying--"low in the lower 30s. temperature rising
##                            # into the 30s after midnight."
##                            # instead say--"low in the lower 30s. temperature rising
##                            # through the 30s after midnight."
##                            toPhrase = " through" + toPhrase[5:]
##                        if len(tStats) <= 6:   #assumes already after midnight
##                            words = "temperature rising" + toPhrase + " through sunrise"
##                        else:
##                            words = "temperature rising" + toPhrase + " after midnight"
##                    elif tempValue_endPoint > minT:
##                        # small temp rise (i.e. < threshold)
##                        if len(tStats) <= 6:   #assumes already after midnight
##                            words = "temperature steady or slowly rising through sunrise"
##                        else:
##                            words = "temperature steady or slowly rising after midnight"

##        return self.setWords(node, words)



    def reportTrends(self):
        return {
            "setUpMethod": self.reportTrends_setUp,
            "wordMethod": self.reportTrends_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }    
    def reportTrends_setUp(self, tree, node):
        timeRange = node.getTimeRange()
        dayNight = self.getPeriod(timeRange, 1)
        if dayNight == self.NIGHTTIME():
            eleInfo = self.ElementInfo("MinT", "Min")
        else:
            eleInfo = self.ElementInfo("MaxT", "Max")
            elementName = "MaxT"
        elementInfoList = [eleInfo]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        return self.DONE()
    
    def reportTrends_words(self, tree, node):
        "Compare current analysis to previous analysis for trends"
        elementName = node.get("elementName")
        statDict = node.getStatDict()
        curStats =  self.getStats(statDict, elementName)
        if curStats is None:
            return self.setWords(node, "")
        timeRange = node.getTimeRange()
        areaLabel = node.getAreaLabel()
        prevTimeRange = self.adjustTimeRange(timeRange, -24)
        prevStats =  tree.stats.get(elementName, prevTimeRange, areaLabel,
                                    mergeMethod="Average")
        #print "Report trends", timeRange, elementName, curStats, prevStats
        if prevStats is None:
            return self.setWords(node, "")

        prevStats = self.getValue(prevStats)
        curStats = self.getValue(curStats)
        #print "stats", prevStats, curStats
        diff = curStats - prevStats
        value =  self.reportTrends_valueStr(tree, node, diff, curStats)
        #print "   returning ", value
        return self.setWords(node, value)

    def reportTrends_valueStr(self, tree, node, diff, temp):
        # Given a difference between current and 24-hour prior
        # MaxT or MinT grids, report a trend.

        var = self.colder_warmer_dict(tree, node)
        timeRange = node.getTimeRange()
        dayNight = self.getPeriod(timeRange, 1)
        if dayNight == self.DAYTIME():
            if diff > 10:
                return self.nlValue(var["HighWarmer"], temp)
            elif diff < -20:
                return self.nlValue(var["HighMuchColder"], temp)
            elif diff <= -10 and diff >= -20:
                return self.nlValue(var["HighColder"], temp)
            else:
                return ""

        else:
            if diff > 10:
                return self.nlValue(var["LowWarmer"], temp)
            elif diff < -20:
                return self.nlValue(var["LowMuchColder"], temp)
            elif diff <= -10 and diff >= -20:
                return self.nlValue(var["LowColder"], temp)
            else:
                return ""

        return ""

    # colder_warmer_Dict
    # Dictionary of non-linear dictionaries each with
    # phrases to use instead of colder/warmer
    # based on the temperature

    def colder_warmer_dict(self, tree, node):
        # This dictionary of non-linear dictionaries controls what phrase is returned
        # for cold/much colder warmer/much warmer.  It is based off
        # of the maxT or MinT
        dict =  {}
        dict["LowColder"] = {
           (-80,45): "colder",
           (45,70): "cooler",
           (70,150): "not as warm",
           "default": "",
           }
        dict["LowMuchColder"] = {
           (-80,45): "much colder",
           (45,70): "much cooler",
           (70,150): "not as warm",
           "default": "",
           }
        dict["LowWarmer"] = {
           (-80,35): "not as cold",
           (35,50): "not as cool",
           (50,150): "warmer",
           "default": "",
           }
        dict["HighColder"]= {
           (-80,45): "colder",
           (45,75): "cooler",
           (75,90): "not as warm",
           (90,150): "not as hot",
           "default": "",
           }
        dict["HighMuchColder"]= {
           (-80,45): "much colder",
           (45,75): "much cooler",
           (75,90): "not as warm",
           (90,150): "not as hot",
           "default": "",
           }
        dict["HighWarmer"]= {
           (-80,45): "not as cold",
           (45,65): "not as cool",
           (65,150): "warmer",
           "default": "",
           }
        return dict


##    def reportTrends_valueStr(self, tree, node, diff):
##        # Given a difference between current and 24-hour prior
##        # MaxT or MinT grids, report a trend.
##        if diff > 15 and diff < 25:
##            return "warmer"
##        elif diff >= 25:
##            return "much warmer"
##        elif diff < -15 and diff > -25:
##            return "cooler"
##        elif diff <= -25:
##            return "much colder"
##        else:
##            return ""

    def extremeTemps_phrase(self):
        ### NEW METHOD written by Tom Spriggs
        ### ZFP_Local
        return {
            "setUpMethod": self.extremeTemps_setUp,
            "wordMethod": self.extremeTemps_words,
            "phraseMethods": self.standard_phraseMethods(),  
            }
    
    def extremeTemps_setUp(self, tree, node):
        dayNight = self.getPeriod(node.getTimeRange(), 1)
        if dayNight == self.DAYTIME():
            elementInfoList = [
                self.ElementInfo("MaxT", "Max"),
                self.ElementInfo("MinT", "Min"),
                ]
        else:
            elementInfoList = [
                self.ElementInfo("MinT", "Min"),
                self.ElementInfo("MaxT", "Max"),
                ]
        elementInfoList.append(self.ElementInfo("HeatIndex", "Max"))
        elementInfoList.append(self.ElementInfo("WindChill", "Min"))
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        return self.DONE()    


    def extremeTemps_words(self, tree, node):
        "Compare current analysis to previous analysis for trends"

        tempPhrases = ["reportTrends"]
        words = self.findWords(tree, node, None, node.getAreaLabel(),
                                 phraseList=tempPhrases)

        if words is None:
            # If words have not yet been set, return
            # We need to wait for reportTrends to complete
            # before doing the extremeTemps_phrase
            return
 
        statDict = node.getStatDict()
        timeRange = node.getTimeRange()
        dayNight = self.getPeriod(timeRange, 1)
        if dayNight == self.DAYTIME():
            element = "MaxT"
        else:
            element = "MinT"
        tStats =  self.getStats(statDict, element)
        if tStats is None:
            return self.setWords(node, "")
        tStats = self.getValue(tStats)
 
        chillStats = self.getStats(statDict, "WindChill")
        chillStats = self.getValue(chillStats, "Min")
        heatStats = self.getStats(statDict, "HeatIndex")
        heatStats = self.getValue(heatStats, "Max")

        words = ""

        if dayNight == self.DAYTIME():
            if tStats > 99:
                if heatStats is None:
                    words = "very hot"
                elif (heatStats - tStats) > 7:
                    words = "very hot and humid"
                else:
                    words = "very hot"
            elif tStats > 95:
                if heatStats is None:
                    words = "hot"
                elif (heatStats - tStats) > 6:
                    words = "hot and humid"
                else:
                    words = "hot"
            elif tStats < 20:
                if chillStats is None:
                    words = "very cold"
                elif chillStats < -9:
                    words = "bitterly cold"
                else:
                    words = "very cold"
            elif heatStats is None:
                words = ""
            elif heatStats >= self.heatIndex_threshold(tree, node):
                words = "hot and humid"
            elif chillStats is None:
                words = ""
            elif chillStats <= self.windChill_threshold(tree, node):
                words = "bitterly cold"
        else:
            if tStats < 5:
                if chillStats is None:
                    words = "very cold"
                elif chillStats <= self.windChill_threshold(tree, node):
                    words = "bitterly cold"
                else:
                    words = "very cold"
            elif chillStats is None:
                words = ""
            elif chillStats <= self.windChill_threshold(tree, node):
                words = "bitterly cold"

        if words == "":
            return self.setWords(node, words)

        # Clear the words in reportTrends to
        # prevent extra temperature phrases
        component = node.getComponent()
        progeny = component.getProgeny()
        for child in progeny:
            phraseName = child.get("name")
            if phraseName in tempPhrases:
                child.set("words", "")
        return self.setWords(node, words)

    ## Submitted by Brian Walawender
    ## Reviewed by Tracy Hansen
    def steady_temp_threshold(self, tree, node):
        # Diurnal ranges less than this value will
        # be reported as steady temperatures
        return 4

    def steady_temp_trends(self):
        return {
            "setUpMethod": self.steady_temp_trends_setUp,
            "wordMethod": self.steady_temp_trends_words,
            "phraseMethods": self.standard_phraseMethods(),
            }
   
    def steady_temp_trends_setUp(self, tree, node):
        elementInfoList = []
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        return self.DONE()
 
    def steady_temp_trends_words(self, tree, node):
        "Look for small diurnal changes"
        # Check Diurnal range in T.  If range is
        # less than steady_temp_threshold report
        # a temperatures steady phrase
        # i.e. "temperature steady in the mid 20s"
        tempPhrases = ["highs_phrase", "lows_phrase",
                       "highs_range_phrase", "lows_range_phrase",
                       "temp_trends",
                       "extended_lows_phrase", "extended_highs_phrase"
                       ]
        words = self.findWords(tree, node, None, node.getAreaLabel(),
                                 phraseList=tempPhrases)
        if words is None:
            # If words have not yet been set, return
            # We need to wait for all highs_phrases to complete
            # before doing the steady_temp_phrase
            return
     
        timeRange = node.getTimeRange()
        tStats = tree.stats.get("T", timeRange, node.getAreaLabel(),
                               mergeMethod="List")
        if tStats is None:
            return self.setWords(node, "")
        # tStats is a list of (hourlyTemp, subRange) tuples
       
        max = -999
        min = 999
        words = ""
        sum = 0
        count = 0
        for hourlyTemps, subRange in tStats:
            if hourlyTemps is None:
                return self.setWords(node, "")
            for t, hr in hourlyTemps:
                if t is None:
                    return self.setWords(node, "")
                if t < min:
                    min = t
                if t > max:
                    max = t
                sum = sum + t
                count = count + 1
               
        diff = max - min

        if diff >= self.steady_temp_threshold(tree,node):
            return self.setWords(node, "")

        dayNight = self.getPeriod(timeRange, 1)
        if dayNight == self.DAYTIME():
            avg = int((sum/count)+0.5)
        else:
            avg = int(sum/count)
                   
        phrase = self.getTempPhrase(tree, node, avg, "")
        words = "near steady temperature " + phrase
           
        # Clear the words in high and lows phrase
        # prevent extra temperature phrases
        component = node.getComponent()
        progeny = component.getProgeny()
        for child in progeny:
            phraseName = child.get("name")
            if phraseName in tempPhrases:
                child.set("words", "")

                # Begin ER changes 
                # Not sure if this is used...but set anyway 
                child.set("emptyPhrase", 1) 

                # Now erase subphrase words. This is what seems to fix 
                # the problem of high/low phrases still appearing with 
                # the steady phrase - PJ 
                subphrases=child.get("childList") 

                if subphrases is not None:
                    for n in subphrases:
                        n.set("words", "") 
                
        return self.setWords(node, words)
    
    ### SnowAmt
    def pop_snow_lower_threshold(self, tree, node):
        # Snow accumulation will not be reported
        # if Pop is below this threshold
        return 60

    def getSnowReportEndDay(self, tree, node):
        # This is the first day we do not try to report total accumulation.
        return self.createTimeRange(96,108)
    
    def snow_phrase(self):
        return {
            "setUpMethod": self.snow_setUp,
            "wordMethod": self.snow_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }    

    def snow_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("SnowAmt", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        component = node.getComponent()
        index = component.getIndex()

        # Calculate past snow
        prodTR = tree.getTimeRange()
        pastSnowMin = 0
        pastSnowMax = 0
        pastSnowTimeRange = self.makeTimeRange(prodTR.startTime() - 12*3600,
                                               prodTR.startTime())
        stats = tree.stats.get("SnowAmt", pastSnowTimeRange,
                               node.getAreaLabel(), mergeMethod="MinMax")

        if stats is not None:
            # site is using a past SnowAmt grid
            pastSnowMin, pastSnowMax = self.getValue(stats, "MinMax")
            # If first period is less than 12 hours long, thus an "update"
            #  report as "new" snow accumulation ONLY IF
            #  there was some previous snow accumulation
            timeRange = node.getTimeRange()
            if index == 0 and timeRange.duration() < 12*3600 and \
                   pastSnowMax > 0.0:
                node.set("newFlag", 1)            
        else:
            # site is NOT using a past SnowAmt grid
            # If first period is less than 12 hours long, thus an "update"
            #  report as "new" snow accumulation
            timeRange = node.getTimeRange()
            if index == 0 and timeRange.duration() < 12*3600:
                node.set("newFlag", 1)
            
        return self.DONE()

    def snow_words(self, tree, node):
        # First check if the pop threshold has been met
        # If not, then do not generate phrase
        threshold = self.pop_snow_lower_threshold(tree, node)
        lowPopFlag = self.lowPop_flag(tree, node, threshold)
        if lowPopFlag == 1:
            return self.setWords(node, "")

        # Second, wait for weather phrase to complete and make sure there
        # is mention of accumulating weather
        wxWords, attrDict = self.findWords(tree, node, "Wx", node.getAreaLabel(),
                                 phraseList=["weather_phrase", "skyPopWx_phrase"],
                                 attributes=["reportedRankList"])
        if wxWords is None:
            return
        accumFlag, descriptor = self.checkAccumulatingWx(tree, node, wxWords, attrDict)
        if accumFlag == 0:
            return self.setWords(node, "null")
                        
        # Third, load in the SnowAmt statistics, check for low amounts, then round to nearest inch
        currentSnow = tree.stats.get("SnowAmt", node.getTimeRange(), node.getAreaLabel(), mergeMethod="MinMax")
        if currentSnow is None:
            return self.setWords(node, "")
        min, max = self.getValue(currentSnow, "MinMax")
        if min == 0 and max == 0:
            node.parent.set("descriptor", "")
            return self.setWords(node, "no " + descriptor)
        elif min < 0.5 and max < 0.5:
            node.parent.set("descriptor", "")
            return self.setWords(node, "little or no " + descriptor)
        min = int(min+0.5)
        max = int(max+0.5)

        # Finally, generate the snow accumulation phrase
        # Decide on singular or plural units
        if max == 1:
            units = self.units_descriptor(tree, node, "unit", "in")
        else:
            units = self.units_descriptor(tree, node, "units", "in")
        # Create worded phrase based on type of range
        if min == 0:
            upTo = self.addSpace(self.phrase_descriptor(tree, node, "up to", "SnowAmt"))
            snowPhrase = upTo + `max`
        elif min == max:
            around = self.addSpace(self.phrase_descriptor(tree, node, "around", "SnowAmt"))
            snowPhrase = around + `max`
        else:
            snowPhrase = "of " + `min` + " to " + `max`
        snowPhrase = snowPhrase + " " + units
        
        return self.setWords(node, snowPhrase)

    def checkAccumulatingWx(self, tree, node, wxWords, attrDict):
        accumulatingWx = [
            ('S', 'Snow'),
            ('SW', 'Snow'),
            ('IP', 'Sleet'),
            ('IC', 'IceCrystal'),
            ]
        desc = ""
        wxTypes = []
        if attrDict.has_key("reportedRankList"):
            rankList = attrDict["reportedRankList"]
            for subkey, rank in rankList:
                # DR_18506
                if subkey.wxType() in ['SW'] and subkey.intensity() == "--":
                    pass
                elif subkey.wxType() in ['IC']:
                    pass
                else:
                    wxTypes.append(subkey.wxType())
        for wxType, wxVar in accumulatingWx:
            if wxType in wxTypes:
                desc += wxVar
        if desc == "":
            return 0, ""
        # Determine the phrase descriptor
        descriptor = self.phrase_descriptor(tree, node, desc, "SnowAmt")
        if node.getAncestor('newFlag') == 1:
            new = self.addSpace(self.phrase_descriptor(tree, node, "New", "SnowAmt"))
            if new != "":
                descriptor = new + descriptor
        node.parent.set("descriptor", descriptor)
        # The handle the case of embedded local effects, set the parent's parent as well
        node.parent.parent.set("descriptor", descriptor)
        return 1, descriptor

    ##    Modifications submitted by Tom Spriggs LSX for accurately reporting total snow

    ##    Since the total_snow_phrase uses the SnowAmt element exclusively for tallying
    ##    up storm totals, you can use a SnowAmt grid that exists in the past 
    ##    (before the current hour) to tell us how much snow has already fallen from THIS STORM
    ##    (this will not include already existing snow pack from other storms)
    ##    and thus compensate for the diminishing/shrinking forecast totals.
    ##    The method simply samples and adds the already fallen snow to what is still forecast
    ##    to produce an accuate total snow amount for an ongoing event
    ##    (as well as an event still yet to happen).

    ##    This "past" SnowAmt grid will end at the current time and can go as far back as
    ##    the user wants--but it MUST be a single grid for previously fallen snow,
    ##    not a bunch of fragmented grids.  The actual values in the "past" SnowAmt grid
    ##    will then need to be filled with values based on already collected snow reports.
    ##    One  method to create this "old" SnowAmt grid would be through use of the CONTOUR tool.

    ##    Using this method, it is now possible to kick off a total snow phrase in the
    ##    first period when the total snow differs from the still yet to fall/forecasted
    ##    snow in the first period.

    ##    If you leave the "past" SnowAmt grid as zero, the formatter will know to treat
    ##    it as a non-ongoing event.
      
    def total_snow_phrase(self):
        return {
            "setUpMethod": self.total_snow_setUp,
            "wordMethod": self.total_snow_words,
            "phraseMethods": self.standard_phraseMethods()  
            }
    
    def total_snow_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("SnowAmt", "MinMax"),
                           self.ElementInfo("IceAccum", "MinMax", primary=0)]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        descriptor = self.phrase_descriptor(tree, node, "TotalSnow", "SnowAmt") 
        node.set("descriptor", descriptor)
        return self.DONE()

    def total_snow_words(self, tree, node):
        # Return a total accumulation phrase if appropriate
        # Example:
        #   TOTAL SNOW ACCUMULATION 7 INCHES.

        component = node.getComponent()
        index = component.getIndex()        
        totalSnow = ""

        # See if we are prior to the snow report end day
        timeRange = node.getParent().getTimeRange()
        snowReportEndDay = self.getSnowReportEndDay(tree, node)
        shiftedTimeRange = self.shiftedTimeRange(timeRange)
        if shiftedTimeRange.startTime() < snowReportEndDay.startTime():
            ### Round up stats--need current period snow, next period snow, and past snow
            # Obtain minimum PoP needed to report accumulations
            threshold = self.pop_snow_lower_threshold(tree, node)
            # Get snow stats for the current period
            currentSnow = tree.stats.get("SnowAmt", node.getTimeRange(), node.getAreaLabel(), mergeMethod="MinMax")
            if currentSnow is None:
                return self.setWords(node, "")
            currentMin, currentMax = self.getValue(currentSnow, "MinMax")
            currentMin = int(currentMin+0.5)
            currentMax = int(currentMax+0.5)
            # Check PoP threshold for the current period--zero out if below threshold PoP
            popStats = self.matchToWx(tree, node, "PoP", node.getTimeRange())
            if popStats < threshold:
                currentMin = 0
                currentMax = 0
            # Get snow stats for the next period--does the event come to an end?
            nextComp = component.getNext()
            if nextComp is None:
                return self.setWords(node, "")
            nextTimeRange = nextComp.getTimeRange()
            nextSnow = tree.stats.get("SnowAmt", nextTimeRange,
                                      node.getAreaLabel(), mergeMethod="Max")
            if nextSnow is None:
                return self.setWords(node, "")
            nextSnow = int(nextSnow+0.5)
            # Check PoP threshold for the next period--zero out if below threshold PoP
            threshold = self.pop_snow_lower_threshold(tree, node)
            popStats = self.matchToWx(tree, node, "PoP", nextTimeRange)
            if popStats < threshold:
                nextSnow = 0
            # Get snow stats for both already fallen snow AND preceding forecast periods
            minSum, maxSum = self.sumPrevStats(tree, component,
                                    node.getAreaLabel(), "SnowAmt", "MinMax")

            ### Generate total snow accumulation phrase if conditions met
            # We produce a total accumulation phrase for the current period IF
            # the next period's snow is 0--thus snow will cease by the end of the current period AND
            # there is snow accumulation expected in the current period AND
            # there is snow accumulation in one or more periods immediately preceding
            if nextSnow == 0 and currentMax > 0 and maxSum > 0:
                # Finalize total snow amount
                finalMinSum = int(currentMin + minSum)
                finalMaxSum = int(currentMax + maxSum)
                # Decide on singular or plural units
                if finalMaxSum == 1:
                    units = self.units_descriptor(tree, node, "unit", "in")
                else:
                    units = self.units_descriptor(tree, node, "units", "in")
                # Create worded phrase based on type of range
                if finalMinSum == 0:
                    upTo = self.addSpace(self.phrase_descriptor(tree, node,
                                                 "up to", "SnowAmt"))
                    totalSnowPhrase = upTo + `finalMaxSum`
                elif finalMinSum == finalMaxSum:
                    around = self.addSpace(self.phrase_descriptor(tree, node,
                                                   "around", "SnowAmt"))
                    totalSnowPhrase = around + `finalMaxSum`
                else:
                    totalSnowPhrase = `finalMinSum` + " to " + `finalMaxSum`
                totalSnow = totalSnowPhrase + " " + units
            else:
                return self.setWords(node, "")
                    
        return self.setWords(node, totalSnow)

    def getSnowValue(self, tree, node, areaLabel=None):
        # Return min and max snow values
        threshold = self.pop_snow_lower_threshold(tree, node)
        lowPopFlag = self.lowPop_flag(tree, node, threshold)
        if lowPopFlag == 1:
            return None
        if areaLabel is None:
            areaLabel = node.getAreaLabel()
        stats = tree.stats.get("SnowAmt", node.getTimeRange(),
                               areaLabel, mergeMethod="MinMax")
        if stats is None:
            return None
        min, max = self.getValue(stats, "MinMax")
        min = int(min+0.5)
        max = int(max+0.5)
        if min < 1 and max < 1:
            return None

        return min, max

    def getTotalSnow(self, tree, node, areaLabel=None, snowValue=None):
        component = node.getComponent()
        # Get sum of previous periods
        if areaLabel is None:
            areaLabel = node.getAreaLabel()
        if snowValue is None:
            snowValue = self.getSnowValue(tree, node, areaLabel)
        if snowValue is None:
            return None
        minSnowValue, maxSnowValue = snowValue
        minSum, maxSum = self.sumPrevStats(tree, component, areaLabel, "SnowAmt", "MinMax")
        # Add this period's value to the sum
        minSum = minSum + minSnowValue
        maxSum = maxSum + maxSnowValue
        minIncrement = self.nlValue(self.increment_nlValue(
            tree, node, "SnowAmt", "SnowAmt"), minSum)
        maxIncrement = self.nlValue(self.increment_nlValue(
            tree, node, "SnowAmt", "SnowAmt"), maxSum)
        minSum = self.round(minSum, "Nearest", minIncrement)
        maxSum = self.round(maxSum, "Nearest", maxIncrement)
        return minSum, maxSum

    ## TOTAL SNOW Phrase submitted by Virgil Mittendorf
    def stormTotalSnow_phrase(self):
        return {
            "setUpMethod": self.stormTotalSnow_setUp,
            "wordMethod": self.stormTotalSnow_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }    
    def stormTotalSnow_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("StormTotalSnow", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        return self.DONE()
    
    def stormTotalSnow_words(self, tree, node):
        "Create phrase for Storm Total Snow accumulation"
                
        # Load in the statistics for storm total snow
        elementName = "StormTotalSnow"
        statDict = node.getStatDict()
        stats = self.getStats(statDict, elementName)

        #print "storm total snow stats", stats

        # test...if no stats then don't create phrase (i.e. grid missing)
        if stats is None:
            return self.setWords(node, "")
        
        min, max = self.getValue(stats, "MinMax")
        threshold = 1
        incMin = 1
        incMax = 1
            
        if min%1 == 0:
            min = int(min)
            minStr = `min`
        else:
            minStr = `int(min+0.5)`
        if max%1 == 0:
            max = int(max)
            maxStr = `max`
        else:
            maxStr = `int(max+0.5)`

        #print "min, max", min, max, node.getTimeRange(), node.getAreaLabel(), "storm total accumulation"
            
        if min == 0 and max == 0:
            return self.setWords(node,"")
        elif min < 0.5 and max < 0.5:
            return self.setWords(node,"")

        outUnits = self.element_outUnits(tree, node, elementName, elementName)
        unit = self.units_descriptor(tree, node,"unit", outUnits)
        units = self.units_descriptor(tree, node,"units", outUnits)

        min = int(min+0.5)
        max = int(max+0.5)
                        
        # Single Value input
        if  min == max:
            # Handle case of 1 inch
            if min == 1:
                units = unit
            value = "around " + minStr

        # Range
        else:
            value = "of " + minStr + " to " + maxStr
            # Handle case when lower value is 0
            if min == 0:
                value = "up to " + maxStr
            if max == 1:
                units = unit
                
        snowPhrase = value + " " + units        
        return self.setWords(node, snowPhrase)

    # New def by Scott.  According to Directive 10-503, descriptive terms
    # should be used in period 4 and beyond.  This function returns
    # a descriptive snow phrase.
    def descriptive_snow_phrase(self):
        return {
            "setUpMethod": self.descriptive_snow_setUp,
            "wordMethod": self.descriptive_snow_words,
            "phraseMethods": self.standard_phraseMethods(),  
            }    
    def descriptive_snow_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("SnowAmt", "MinMax")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        # Do not want phrase descriptor
        node.set("descriptor", "")
        return self.DONE()
    
    def descriptive_snow_words(self, tree, node):
        "Create phrase for snow accumulation"
        # According to Directive 10-503, descriptive terms
        # should be used in period 4 and beyond.  This function returns
        # a descriptive snow phrase.
        #
        # According to Directive 10-503, snow accumulation
        # should not be mentioned if PoP is under 60%.
        threshold = self.pop_snow_lower_threshold(tree, node)
        lowPopFlag = self.lowPop_flag(tree, node, threshold)
        if lowPopFlag == 1:
            return self.setWords(node, "")

        statDict = node.getStatDict()
        stats = self.getStats(statDict, "SnowAmt")
        if stats is None:
            return self.setWords(node, "")

        max = int(self.getValue(stats, "Max"))

        if max < 1:
            words =  ""
        elif max >= 1 and max <= 2:
            words = "light snow accumulations"
        elif max > 2 and max <= 5:
            words =  "moderate snow accumulations"
        else:
            words =  "heavy snow accumulations"
        return self.setWords(node, words)

    ### SnowLevel
    def pop_snowLevel_upper_threshold(self, tree, node):
        # Snow level will be reported if Pop is above this threshold
        return 60
    
    def snowLevel_maximum_phrase(self, tree, node):
        # This returns the maximum snow level value to be reported and the
        # the corresponding snow level phrase. It can be set up by
        # edit area as follows:
        # editAreaList = [
        #    ("area1", 8000, "above 8000 feet"),
        #    ("area2", 6000, "above 6000 feet"),
        #    # Don't mention snow level at all in area3:
        #    ("area3", 0, ""),
        #    ]
        #maxElev = 0
        #phrase = ""
        #for area, elev, elevPhrase in editAreaList:
        #    if self.currentAreaContains(tree, [area]):
        #        if elev > maxElev:
        #            maxElev = elev
        #            phrase = elevPhrase
        #return (maxElev, phrase)
        return (8000, "above 8000 feet")

    def snowLevel_upper_topo_percentage(self, tree, node):
        # If this percentage of the edit area is above the snow level,
        # do not report snow level
        return 80
    
    def snowLevel_lower_topo_percentage(self, tree, node):
        # If this percentage of the edit area is below or equal to the snow level,
        # do not report snow level
        return 80        
    
    def snowLevel_phrase(self):
        return {
            "setUpMethod": self.snowLevel_setUp,
            "wordMethod": self.snowLevel_words,
            "phraseMethods": self.standard_phraseMethods(),  
            }    
    def snowLevel_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("SnowLevel", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)  
        return self.DONE()

    def snowLevel_words(self, tree, node):
        "Create phrase for reporting snow level"

        # Check for low pop
        threshold = self.pop_snowLevel_upper_threshold(tree, node)
        lowPopFlag = self.lowPop_flag(tree, node, threshold)

        if lowPopFlag == 1:
            return self.setWords(node, "")

        statDict = node.getStatDict()
        snowLevel = self.getStats(statDict, "SnowLevel")
        if snowLevel is None:
            return self.setWords(node, "")
        snowLevel = self.getValue(snowLevel)
        element = "SnowLevel"
        roundingMethod = self.rounding_method(tree, node, element, element)
        increment_nlValue = self.increment_nlValue(tree, node, element, element)
        snowLevel = self.roundValue(snowLevel, roundingMethod, "Nearest", increment_nlValue, 0)

        # Check Wx for R or RW
        stats = tree.stats.get("Wx", node.getTimeRange(), node.getAreaLabel(),
                               mergeMethod="List")
        if stats is None:
            return self.setWords(node, "")
        
        found = 0
        for rankList, subRange in stats:
            subkeys = self.getSubkeys(rankList)
            for subkey in subkeys:
                if subkey.wxType() == "R" or subkey.wxType() == "RW":
                    found = 1
                    break

        if found == 0:
            return self.setWords(node, "")
        
        # Check for upper and lower topo percentages
        percentage_above = self.calcTopoPercentage(tree, node, node.getAreaLabel(), snowLevel)
        percentage_below = 100 - percentage_above

        if percentage_above > self.snowLevel_upper_topo_percentage(tree, node):
            return self.setWords(node, "")
        if percentage_below > self.snowLevel_lower_topo_percentage(tree, node):
            return self.setWords(node, "")
                
        # Check for maximum snow level to be reported
        max, words = self.snowLevel_maximum_phrase(tree, node)

        if snowLevel < max:        
            units = self.units_descriptor(tree, node, "units", "ft")
            words = `int(snowLevel)` + " " + units
        return self.setWords(node, words)
    
    ### IceAccum
    def ice_accumulation_threshold(self, tree, node):
        # If maximum IceAccum is greater than this threshold, it will be
        # reported instead of SnowAmt in the snow_phrase
        return .10
    
    def iceAccumulation_phrase(self):
        return {
            "setUpMethod": self.iceAccumulation_setUp,
            "wordMethod": self.iceAccumulation_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }    
    def iceAccumulation_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("IceAccum", "MinMax", primary=0)]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        return self.DONE()
    
    def iceAccumulation_words(self, tree, node):
        "Create phrase for ice accumulation"
                
        threshold = self.pop_snow_lower_threshold(tree, node)
        lowPopFlag = self.lowPop_flag(tree, node, threshold)
        if lowPopFlag == 1:
            return self.setWords(node, "")
  
        # Check for IceAccum.  If it is significant, report it.
        statDict = node.getStatDict()

        stats = self.getStats(statDict, "IceAccum")        
        reportIceAccum = 0
        if stats is not None:
            threshold = self.ice_accumulation_threshold(tree, node)
            min, max = self.getValue(stats, "MinMax")
            if max >= threshold:
                reportIceAccum = 1
        if reportIceAccum == 1:
            component = node.getComponent()
            index = component.getIndex()
            timeRange = node.getTimeRange()
            if index == 0 and timeRange.duration() < 12*3600:
                descriptor = self.phrase_descriptor(
                    tree, node, "NewIceAccum", "IceAccum")
            else:
                descriptor = self.phrase_descriptor(tree, node, "IceAccum", "IceAccum")
            node.parent.set("descriptor", descriptor)
            elementName = "IceAccum"
        else:
            return self.setWords(node, "")
            
        if min < 0.2:
            minStr = "less than one quarter"
        elif min >= 0.2 and min < 0.4:
            minStr = "one quarter"
        elif min >= 0.4 and min < 0.7:
            minStr = "one half"
        elif min >= 0.7 and min < 0.9:
            minStr = "three quarters"
        elif min >= 0.9 and min < 1.3:
            minStr = "one"
        elif min >= 1.3 and min < 1.8:
            minStr = "one and a half"
        elif min >= 1.8:
            minStr = `int(min+0.5)`
        if max < 0.2:
            maxStr = "less than one quarter"
        elif max >= 0.2 and max < 0.4:
            maxStr = "one quarter"
        elif max >= 0.4 and max < 0.7:
            maxStr = "one half"
        elif max >= 0.7 and max < 0.9:
            maxStr = "three quarters"
        elif max >= 0.9 and max < 1.3:
            maxStr = "one"
        elif max >= 1.3 and max < 1.8:
            maxStr = "one and a half"
        elif max >= 1.8:
            maxStr = `int(max+0.5)`
            if min >= 0.9 and min < 1.3:
                minStr = `int(min+0.5)`

        #print "min, max", min, max, node.getTimeRange(), node.getAreaLabel()
            
        outUnits = self.element_outUnits(tree, node, elementName, elementName)
        unit = self.units_descriptor(tree, node,"unit", outUnits)
        units = self.units_descriptor(tree, node,"units", outUnits)
        
        # Single Value input
        if  minStr == maxStr:
            if min < 0.2:
                icePhrase = "of " + minStr + " of an " + unit
            elif min >= 0.2 and min < 0.9:
                icePhrase = "around " + minStr + " of an " + unit
            elif min >= 0.9 and min < 1.3:
                icePhrase = "around " + minStr + " " + unit
            elif min >= 1.3:
                icePhrase = "around " + minStr + " " + units
            else:
                return self.setWords(node, "")
 
        # Range
        else:
            if min < 0.2:
                if max < 0.9:
                    icePhrase = "of up to " + maxStr + " of an " + unit
                elif max >= 0.9 and max < 1.3:
                    icePhrase = "of up to " + maxStr + " " + unit
                elif max >= 1.3:
                    icePhrase = "of up to " + maxStr + " " + units
                else:
                    return self.setWords(node, "")
            elif min >= 0.2 and min < 0.9:
                if max < 0.9:
                    icePhrase = "of " + minStr + " to " + maxStr + " of an " + unit
                elif max >= 0.9 and max < 1.3:
                    icePhrase = "of " + minStr + " of an " + unit + " to " + maxStr + " " + unit
                elif max >= 1.3:
                    icePhrase = "of " + minStr + " of an " + unit + " to " + maxStr + " " + units
                else:
                    return self.setWords(node, "")
            elif min >= 0.9:
                if max >= 1.3:
                    icePhrase = "of " + minStr + " to " + maxStr + " " + units
                else:
                    return self.setWords(node, "")
        return self.setWords(node, icePhrase)

    ### FzLevel
    ### WindChill
    def windChill_threshold(self, tree, node):
        # THRESHOLD FOR REPORTING WIND CHILL
        return 0.0
    
    def windChillTemp_difference(self, tree, node):
        # Difference between wind chill and temperature
        # for reporting wind chill
        return 5

    def windChill_phrase(self):
        return {
            "setUpMethod": self.windChill_setUp,
            "wordMethod": self.windChill_words,
            "phraseMethods": self.standard_phraseMethods(),  
            }    
    def windChill_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("WindChill", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)               
        return self.DONE()

    def windChill_words(self, tree, node):
        "Create phrase for Wind Chill"

        statDict = node.getStatDict()
        stats = self.getStats(statDict, "WindChill")
        if stats is None:
            return self.setWords(node, "")
        t = tree.stats.get("T", node.getTimeRange(),
                           node.getAreaLabel(), statLabel="minMax",
                           mergeMethod="MinMax")
        if t is None:
            return self.setWords(node, "")
        
        min, max = self.getValue(stats, "MinMax")

        timeRange = node.getTimeRange()
        day = self.getPeriod(timeRange, 1)
        if day == self.DAYTIME():
            # Compare to max T
            t = self.getValue(t,"Max")
        else:
            # Compare to min T
            t = self.getValue(t,"Min")

        diff = self.windChillTemp_difference(tree, node)
        if min <= self.windChill_threshold(tree, node) and min <= t - diff:
            words = self.getTempRangePhrase(tree, node,  (min, max), "WindChill")
        else:
            words = ""
        return self.setWords(node, words)

    # Alternate phrase based on wind speed    
    def windChill_wind_threshold(self, tree, node):
        # Minimum wind speed (mph) required for reporting wind chill
        return 10
    
    def windBased_windChill_phrase(self):
        return {
            "setUpMethod": self.windChill_setUp,
            "wordMethod": self.windBased_windChill_words,
            "phraseMethods": self.standard_phraseMethods(),  
            }    

    def windBased_windChill_words(self, tree, node) :
        "Create phrase for Wind Chill"

        # Wait for wind phrase to complete
        windWords = self.findWords(tree, node, "Wind", node.getAreaLabel())
        if windWords is None:
            return
        
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "WindChill")
        if stats is None:
            return self.setWords(node, "")

        if windWords == "":
            return self.setWords(node, "")            
        
        min, max = self.getValue(stats, "MinMax")

        # Check wind speed
        # First try to re-use information from wind_phrase
        maxWind = node.getComponent().get("maxMag")
        if maxWind is None:
            # Have to access it from statistics dictionary
            timeRange = node.getTimeRange()
            wind = tree.stats.get("Wind", timeRange, node.getAreaLabel(), mergeMethod="Max")
            if wind is None:
                return self.setWords(node, "")            
            maxWind, dir = wind
            
        if maxWind < self.windChill_wind_threshold(tree, node):
            return self.setWords(node, "")
    
        # WC must be less or equal to threshold 
        if min <= self.windChill_threshold(tree, node):
            words = self.getTempRangePhrase(tree, node, (min, max), "WindChill")
        else:
            words = ""
        return self.setWords(node, words)

    ### HeatIndex
    def heatIndex_threshold(self, tree, node):
        # THRESHOLD FOR REPORTING HEAT INDEX
        return 108.0

    def heatIndexTemp_difference(self, tree, node):
        # Difference between heat index and temperature
        # for reporting heat index
        return 5

    def heatIndex_phrase(self):
        return {
            "setUpMethod": self.heatIndex_setUp,
            "wordMethod": self.heatIndex_words,
            "phraseMethods": self.standard_phraseMethods(),  
            }    
    def heatIndex_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("HeatIndex", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)      
        return self.DONE()

    def heatIndex_words(self, tree, node) :
        "Create phrase for Heat Index"
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "HeatIndex")
        if stats is None:
            return self.setWords(node, "")
        t = tree.stats.get("T", node.getTimeRange(),
                           node.getAreaLabel(), statLabel="minMax",
                           mergeMethod="MinMax")
        if t is None:
            return self.setWords(node, "")
        
        min, max = self.getValue(stats, "MinMax")

        timeRange = node.getTimeRange()
        day = self.getPeriod(timeRange,1)
        if day == self.DAYTIME():
            # Compare to max T
            t = self.getValue(t,"Max")
        else:
            # Compare to min T
            t = self.getValue(t,"Min")
        # HI must be greater or equal to threshold and at least
        # two degrees higher than the maximum T.
        diff = self.heatIndexTemp_difference(tree, node)
        if max >= self.heatIndex_threshold(tree, node) and max >= t + diff:
            words = self.getTempRangePhrase(tree, node, (min, max), "HeatIndex")
        else:
            words = ""
        return self.setWords(node, words)

    # RH -- Contributed by ER 8/04
    def rh_threshold(self, tree, node):
        # Threshold for reporting RH in extended narrative. If MinRH grid is
        # lower than this threshold, an RH phrase will be formatted.
        # To turn off phrase completely, set to -1.
        if self.__dict__.has_key("_rhPhraseThreshold"):
            # Use Definition setting if defined
            return self._rhPhraseThreshold
        else:
            # Default to no phrase
            return -1
        
    def rh_phrase(self):
        return {
            "setUpMethod": self.rh_setUp,
            "wordMethod": self.rh_words,
            "phraseMethods": self.standard_phraseMethods(),
            }
    def rh_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("MinRH", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        return self.DONE()
    
    def rh_words(self, tree, node):
        # Creates phrase for MinRH. Phrase will be generated if the MinRH
        # value is <= rh_threshold(tree, node).  Also uses only MinRH
        # grids during the day part of the extended period. Requires the
        # sample analysis method to use [0] for the time duration.
        minRH = None
        words = ""
        statDict = node.getStatDict()
        
        rhStats = tree.stats.get("MinRH", node.getTimeRange(), node.getAreaLabel(),
                               mergeMethod="List")

        if rhStats is None:
            return self.setWords(node, "")
        for rhValues, tr in rhStats:
            # Use data only from daytime timeranges
            if self.getPeriod(tr, 1):
                rh = self.getValue(rhValues, "Min")
                if minRH == None or rh < minRH:
                    minRH = rh
        if minRH is not None and minRH <= self.rh_threshold(tree, node):
           words = "MINIMUM RH " + `int(minRH)` + " PERCENT"
        return self.setWords(node, words)
    
    # MultipleElementTable calls
    def multipleElementTable_perPeriod_phrase(self):
        return {
            "setUpMethod": self.multipleElementTable_perPeriod_setUp,
            "wordMethod": self.multipleElementTable_perPeriod_words,
            "phraseMethods": [], 
            }    
    def multipleElementTable_perPeriod_setUp(self, tree, node):
        elementInfoList = []
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        return self.DONE()
    
    def multipleElementTable_perPeriod_words(self, tree, node):
        # Make a MultipleElementTable for this period
        words = self.makeMultipleElementTable(
            node.getAreaLabel(), node.getTimeRange(), tree,
            byTimeRange=1)
        return self.setWords(node.parent, words)
 
  
