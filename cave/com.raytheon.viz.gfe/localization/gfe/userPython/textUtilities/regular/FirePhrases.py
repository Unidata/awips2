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
# FirePhrases.py
# Methods for producing text forecast from SampleAnalysis statistics.
#
# Author: hansen
# ----------------------------------------------------------------------------

import ScalarPhrases
import VectorRelatedPhrases
import WxPhrases
import DiscretePhrases

class FirePhrases(ScalarPhrases.ScalarPhrases, VectorRelatedPhrases.VectorRelatedPhrases,
                  WxPhrases.WxPhrases, DiscretePhrases.DiscretePhrases):
    def __init__(self):    
        ScalarPhrases.ScalarPhrases.__init__(self)
        VectorRelatedPhrases.VectorRelatedPhrases.__init__(self)
        WxPhrases.WxPhrases.__init__(self)
        DiscretePhrases.DiscretePhrases.__init__(self)

    ############################################      
    ### FIRE WEATHER PHRASES
                
    ### Sky and Weather
    def includeSkyRanges_flag(self, tree, node):
        # Set to 0 if you do not want ranges reported with sky phrases:
        #   Partly cloudy (35-40 PERCENT)
        return 1
    
    def skyWeather_byTimeRange_compoundPhrase(self):
        return {
            "phraseList": [
                self.fireSky_phrase,
                self.weather_phrase,
                ],
            "phraseMethods": [
                self.consolidateSubPhrases,
                self.assembleSentences,
                self.skyWeather_finishUp, 
            ],
            }    
    def skyWeather_finishUp(self, tree, node):
        "Create a phrase for sky/weather"
        words = node.get("words")
        if words is None:
            return
        if words == "":
            words = "MISSING"
        node.set("descriptor", "")
        node.set("indentLabel", "SKY/WEATHER.........")
        node.set("compound", 1)
        return self.DONE()

    def fireSky_phrase(self):
        return {
            "setUpMethod": self.fireSky_setUp,
            "wordMethod": self.fireSky_words,
            "phraseMethods": [
                self.checkLocalEffects,
                self.combineSky,
                self.combineWords,
                self.fillNulls,
                self.assembleSubPhrases,
                self.postProcessPhrase,
                ],  
            }    
    def fireSky_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("Sky", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)  
        return self.DONE()
    
    def fireSky_words(self, tree, node):
        # Report average as a worded summary, also return average
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "Sky")
        if stats is None:
            return self.setWords(node, "")
        avg = self.getValue(stats)
        dayNight = self.getPeriod(node.getTimeRange(), 1)
        words = self.sky_value(tree, node, avg, dayNight)
        words = self.addSkyRange(tree, node, words, avg)
        return self.setWords(node, words)

    def addSkyRange(self, tree, node, words, avg):
        # Add range if desired
        if self.includeSkyRanges_flag(tree, node):
            roundAvg = int(self.round(avg, "Nearest", 5))
            if roundAvg < 5:
                min = 0
            else:
                min = roundAvg-5
            if roundAvg >= 100:
                max = 100
            else:
                max = roundAvg + 5
            units = self.units_descriptor(tree, node, "units", "%")            
            words = words +  " (" + `min` + "-" + `max` + units + ")"
        return words
       
    # Trends
    ### MinT, MaxT, MinRH, MaxRH, RH
    def trend_DayOrNight_phrase(self):
        return {
            "setUpMethod": self.trend_DayOrNight_setUp,
            "wordMethod": self.trend_DayOrNight_words,
            "phraseMethods": [
                    self.checkLocalEffects,
                    self.assembleSubPhrases,
                    self.postProcessPhrase,
                    ],
            }
    def trend_DayOrNight_setUp(self, tree, node):
        dayElement, nightElement, trendElement, indent, endWithPeriod = node.get("args")
        dayNight = self.getPeriod(node.getTimeRange(), 1)
        if dayNight == self.DAYTIME():
            elementName = dayElement
        else:
            elementName = nightElement             

        elementInfoList = [self.ElementInfo(elementName, "MinMax")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("trendElement", trendElement)
        node.set("descriptor", "")
        if indent == 1:
            node.set("indentLabel", "   24 HR TREND......")            
        return self.DONE()

    def trend_DayOrNight_words(self, tree, node):
        "Compare current analysis to previous analysis for trends"
        elementName = node.get("elementName")

        higher = self.phrase_descriptor(tree, node, "higher", elementName )
        lower = self.phrase_descriptor(tree, node, "lower", elementName )
        threshold = self.trend_threshold(tree, node, elementName, elementName)
        if elementName == "MinT" or elementName == "MaxT":
            units = self.units_descriptor(tree, node, "units", "degrees")
            unit = self.units_descriptor(tree, node, "unit", "degree")
        if elementName == "MaxRH" or elementName == "MinRH" or elementName == "RH":
            units = self.units_descriptor(tree, node, "units", "%")
            unit = self.units_descriptor(tree, node, "unit", "%")
        trendElement = node.getAncestor("trendElement")
        words, diff = self.getTrend(
            tree, node, elementName, trendElement, "", units, unit, threshold,
            higher, lower)
        if words is not None:
            if diff == 0:
                words = self.phrase_descriptor(tree, node, "unchanged", elementName )
        else:
            words = self.phrase_descriptor(tree, node, "missing", elementName )
        return self.setWords(node, words)
    
    def getTrend(self, tree, phrase, element, trendElement, introWords, units, unit, threshold,
                 positiveDescriptor, negativeDescriptor):

        timeRange = phrase.getTimeRange()
        areaLabel = phrase.getAreaLabel()
        absDiff, rawDiff = self.getTrendStats(
            tree, phrase, element, timeRange, areaLabel, trendElement)
        
        if absDiff is None:
            return absDiff, rawDiff
            
        diff = int(absDiff)
        if absDiff >= threshold:
            if rawDiff >=0:
                descriptor = positiveDescriptor
            else:
                descriptor = negativeDescriptor
            if diff == 1:
                units = unit
            introWords = self.addSpace(introWords)
            if units != "%":
                units = " " + units
            words =  introWords + `diff` + units + " " + descriptor
        else:
            words =  ""
        #print "returning", words, diff
        return words, diff

    def getTrendStats(self, tree, phrase, element, timeRange, areaLabel, trendElement):
        # NO CONVERSION DONE 
        # Try the trend element first
        stats = tree.stats.get(trendElement, timeRange, areaLabel,
                               mergeMethod="Average")
        #print "\ngetTrendStats"
        if stats is not None:
            rawDiff = int(self.getValue(stats))
            absDiff = abs(rawDiff)
        else:
            #Use Max/Min element
            curStats = tree.stats.get(element, timeRange, areaLabel,
                                      mergeMethod = "Average", statLabel="mode")
            #print "curstats", curStats
            if curStats is None:
                return None, 0

            prevTimeRange = self.adjustTimeRange(timeRange, -24)
            prevStats = tree.stats.get(element, prevTimeRange, areaLabel,
                                       mergeMethod = "Average", statLabel="mode")
            #print "prevstats", prevStats
            if prevStats is None:
                return None, 0
            
            prevStats = self.getValue(prevStats)
            curStats = self.getValue(curStats)
            #print "cur, prev", element, curStats, prevStats
            absDiff = self.absDiff(curStats, prevStats)
            rawDiff = curStats - prevStats
        #print "absDiff, rawDiff", absDiff, rawDiff
        return absDiff, rawDiff

    def dayOrNight_phrase(self):
        return {
            "setUpMethod": self.dayOrNight_setUp,
            "wordMethod": self.fire_dayOrNight_words,
            "phraseMethods": [
                    self.checkLocalEffects,
                    self.assembleSubPhrases,
                    self.postProcessPhrase,
                    ],
            }
    def dayOrNight_setUp(self, tree, node):
        dayElement, nightElement, indent, endWithPeriod = node.get("args")
        elementName = self.dayOrNight_element(tree, node, dayElement, nightElement)
        indentName = elementName+"_FireWx"
        method = "MinMax"
        if elementName == "RH":
            dayNight = self.getPeriod(node.getTimeRange(), 1)
            if dayNight == self.DAYTIME():
                indentName = "MinRH_FireWx"
                method = "Min"
            else:
                indentName = "MaxRH_FireWx"
                method = "Max"
        elementInfoList = [self.ElementInfo(elementName, method)]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        node.set("descriptor", "")
        node.set("indentLabel", indentName)
        return self.DONE()
    
    def fire_dayOrNight_words(self, tree, node):
        elementName = node.getAncestor("elementName")
        statDict = node.getStatDict()
        if elementName == "MaxT" or elementName == "MinT":
            stats = self.getTempStats(tree, node)
            if stats is None:
                return self.setWords(node.parent, "MISSING")
            words =  self.getTempRangePhrase(tree, node, stats, elementName)        
        else: # MinRH, MaxRH or RH
            stats = self.getStats(statDict, elementName)
            if stats is None:
                return self.setWords(node.parent, "MISSING")
            connector = self.value_connector(tree, node, elementName, elementName)
            min, max = self.getValue(stats, "MinMax")
            if min == max:
                words = `int(min)`
            else:
                words = `int(min)` + connector + `int(max)`
        outUnits = self.element_outUnits(tree, node, elementName, elementName)
        units = self.units_descriptor(tree, node,"units", outUnits)
        words = words + units
        return self.setWords(node, words)
    
    ### CWR
    def cwr_phrase(self):
        return {
            "setUpMethod": self.cwr_setUp,
            "wordMethod": self.cwr_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }    
    def cwr_setUp(self, tree, node):
        try:
            cwr = self._cwrParm
        except:
            cwr = "CWR"
        elementInfoList = [self.ElementInfo(cwr, "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "CWR.................")
        return self.DONE()

    def cwr_words(self, tree, node) :
        "Create phrase Probability of Precipitation"
        statDict = node.getStatDict()
        cwr = self.getStats(statDict, "CWR")
        if cwr is None:
            return self.setWords(node.parent, "MISSING")
        cwr = self.getValue(cwr)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, "CWR", "CWR"), cwr)
        if int(cwr) < threshold:
            return self.setWords(node, "null")
        else:
            words =  `int(cwr)` + " percent"
        return self.setWords(node, words)
            
    ### VentRate or smoke dispersal phrase
    def smokeDispersal_phrase(self):
        return {
            "setUpMethod": self.smokeDispersal_setUp,
            "wordMethod": self.smokeDispersal_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }    
    def smokeDispersal_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("VentRate", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)  
        node.set("descriptor", "")
        node.set("indentLabel","SMOKE DISPERSAL.....")
        return self.DONE()

    def smokeDispersal_words(self, tree, node):
        "Create phrase for Smoke Dispersal"

        statDict = node.getStatDict()
        stats = self.getStats(statDict, "VentRate")
        if stats is None:
            return self.setWords(node.parent, "MISSING")
        vr1, vr2 = self.getValue(stats, "MinMax")

        vr1 = int(vr1)
        vr2 = int(vr2)
        vrCat1 = self.smokeDispersal_valueStr(vr1)
        vrCat2 = self.smokeDispersal_valueStr(vr2)
        # Single Value input
        if  vr1 == vr2:
            words =  vrCat1 + " (" + `vr1` + " knot-ft)"
        # Range
        else:
            words =  vrCat1 + " to " + vrCat2 + " (" + `vr1` + "-" + \
                   `vr2` + " knot-ft)"
        return self.setWords(node, words)
   
    #  SMOKE DISPERSAL CATEGORIES
    def smokeDispersal_valueStr(self, value):
        "Convert smoke dispersal value to corresponding category"

        if value < 40000 :
            return "poor"

        if value >= 40000 and value < 60000:
            return "fair"

        if value >= 60000 and value < 100000 :
            return "good"

        if value >= 100000 and value < 150000 :
            return "very good"

        if value >= 150000 :
            return "excellent"

    ### Fire Winds
    def transportWind_phrase(self):
        return {
            "setUpMethod": self.transportWind_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),  
            }
    def transportWind_setUp(self, tree, node):
        self.wind_setUp(tree, node, gustFlag=0, element="TransWind")
        node.set("descriptor", "")
        node.set("indentLabel","TRANSPORT WINDS.....")
        return self.DONE()
    
    def freeWind_phrase(self):
        return {
            "setUpMethod": self.freeWind_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),  
            }
    def freeWind_setUp(self, tree, node):
        self.wind_setUp(tree, node, gustFlag=0, element="FreeWind")
        node.set("descriptor", "")
        node.set("indentLabel","FREE WINDS..........")
        return self.DONE()

    def ridgeValleyAreas(self, tree, node):
        # List of edit area names for which we want
        # ridge/valley winds reported:
        #
        # 20-FOOT WINDS...
        #     VALLEYS/LWR SLOPES...
        #     RIDGES/UPR SLOPES....       
        #
        # e.g.
        # return ["Area1"]
        #
        return []
    
    def valleyRidgeAreaNames(self, tree, node):
        # These are the areas for valleys and ridges, respectively,
        # to be intersected with the current edit area for
        # reporting valley winds and ridge winds, respectively.
        # NOTE: If you change these area names, you will also
        # need to change the names in the FirePeriod "intersectAreas"
        # section.
        return "Valleys", "Ridges"

    def fireWind_compoundPhrase(self):
        return {
            "phraseList": [
                self.wind_summary,
                self.wind_phrase,
                ],
            "phraseMethods": [
                self.consolidateSubPhrases,
                self.assembleSentences,
                self.fireWind_finishUp
                ],
            }    
    def fireWind_finishUp(self, tree, node):
        "Create a phrase for Winds"
        # Empty phrase if doing ridge/valley winds
        if self.currentAreaContains(
            tree, self.ridgeValleyAreas(tree, node)) == 1:
            return self.setWords(node, "")
        words = node.get("words")
        if words is None:
            return
        if words == "":
            words = "MISSING"
        node.set("descriptor", "")
        node.set("indentLabel", "20-FOOT WINDS.......")
        node.set("compound", 1)
        return self.setWords(node, words)

    def fireWind_label_phrase(self):
        return {
            "setUpMethod": self.fireWind_label_setUp,
            "phraseMethods": [self.postProcessPhrase], 
            }    
    def fireWind_label_setUp(self, tree, node):
        if self.currentAreaContains(
            tree, self.ridgeValleyAreas(tree, node)) == 0:
            return self.setWords(node, "")
        self.setWords(node, "")
        node.set("descriptor", "")
        node.set("indentLabel", "20-FOOT WINDS.......")
        return self.DONE()

    #  Valley/Ridge split set-up    
    def fireValleyWind_compoundPhrase(self):
        return {
            "phraseList": [
                self.wind_summary,
                self.wind_phrase,
                ],
            "phraseMethods": [
                self.fireRidgeValleyWind_setUp,
                self.consolidateSubPhrases,
                self.assembleSentences,
                self.fireValleyWind_finishUp
                ],
            }    
    def fireRidgeValleyWind_setUp(self, tree, node):
        # Used for set-up of fireRidgeWind_compoundPhrase as well.
        if self.currentAreaContains(
            tree, self.ridgeValleyAreas(tree, node)) == 0:
            return self.setWords(node, "")
        # Set up intersect area to be used for the node
        areaName = node.getAreaLabel()
        phraseName = node.get("name")
        valleys, ridges = self.valleyRidgeAreaNames(tree, node)
        if phraseName.find("Valley") >= 0:
            area = valleys
        else:
            area = ridges
        intersectName = self.getIntersectName(areaName, area)
        #print "setting intersect", intersectName
        node.set("areaLabel", intersectName)
        return self.DONE()        
        
    def fireValleyWind_finishUp(self, tree, node):
        "Create a phrase for Winds"
        words = node.get("words")
        if words is None:
            return
        if words == "":
            words = "MISSING"
        node.set("descriptor", "")
        node.set("indentLabel","    VALLEYS/LWR SLOPES...")
        node.set("compound", 1)
        return self.setWords(node, words)
  
    def fireRidgeWind_compoundPhrase(self):
        return {
            "phraseList": [
                self.wind_summary,
                self.wind_phrase,
                ],
            "phraseMethods": [
                self.fireRidgeValleyWind_setUp,
                self.consolidateSubPhrases,
                self.assembleSentences,
                self.fireRidgeWind_finishUp
                ],
            }    
    def fireRidgeWind_finishUp(self, tree, node):
        "Create a phrase for Winds"
        words = node.get("words")
        if words is None:
            return
        if words == "":
            words = "MISSING"
        node.set("descriptor", "")
        node.set("indentLabel", "    RIDGES/UPR SLOPES....")
        node.set("compound", 1)
        return self.setWords(node, words)

    ### Haines
    def hainesDict(self):
        return {
            0:"or very low potential for large plume dominated fire growth",
            1:"or very low potential for large plume dominated fire growth",
            2:"or very low potential for large plume dominated fire growth",
            3:"or very low potential for large plume dominated fire growth",
            4:"or low potential for large plume dominated fire growth",
            5:"or moderate potential for large plume dominated fire growth",
            6:"or high potential for large plume dominated fire growth",
            7:"or high potential for large plume dominated fire growth",
            8:"or high potential for large plume dominated fire growth",
            9:"or high potential for large plume dominated fire growth",
            10:"or high potential for large plume dominated fire growth"
            }

    def haines_phrase(self):
        return {
            "setUpMethod": self.haines_setUp,
            "wordMethod": self.haines_words,
            "phraseMethods": [
               self.consolidatePhrase,
               self.checkLocalEffects,
               self.combinePhraseStats,
               self.combineWords,
               self.fillNulls,
               self.timeDescriptorModeration,
               self.assembleSubPhrases,
               self.postProcessPhrase,
              ]        
            }
    def haines_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("Haines", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel","HAINES INDEX........")
        return self.DONE()

    def haines_words(self, tree, node):
        "Create phrase for Haines Index"
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "Haines")
        if stats is None:
            return self.setWords(node.parent, "MISSING")

        haines1, haines2 = self.getValue(stats, "MinMax")
        hainesDict = self.hainesDict()
        haines1 = int(haines1)
        haines2 = int(haines2)
        words1 = hainesDict[haines1]
        words2 = hainesDict[haines2]

        # Single Value input
        if  haines1 == haines2:
                words = `haines1` + "   " + words1
        # Range
        else:
            if words1 == words2:
                words = words1
            else:
                words = words1 + " to " + words2
            words =  `haines1` + " to " + `haines2` + " OR " + words
        return self.setWords(node, words)

    ### Humidity
    def humidityRecovery_percentage(self, tree, node):
        # If the maximum humidity is greater than this percentage,
        # humidity recovery will be EXCELLENT.
        return 50
    
    def humidityRecovery_phrase(self):
        return {
            "setUpMethod": self.humidityRecovery_setUp,
            "wordMethod": self.humidityRecovery_words,
            "phraseMethods": self.standard_phraseMethods(),  
            }
    def humidityRecovery_setUp(self, tree, node):
        timeRange = node.getTimeRange()
        dayNight = self.getPeriod(timeRange,1)
        if dayNight != self.NIGHTTIME():
            return self.setWords(node, "")
        if self._useRH:
            elementName = "RH"
        else:
            elementName = "MaxRH"
        elementInfoList = [self.ElementInfo(elementName, "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)  
        node.set("descriptor", "")
        node.set("indentLabel", "HUMIDITY RECOVERY...")
        return self.DONE()

    def humidityRecovery_words(self, tree, node):
        "Create phrase for Humidity recovery"

        if self._useRH:
            elementName = "RH"
        else:
            elementName = "MaxRH"
        statDict = node.getStatDict()
        curStats = self.getStats(statDict, elementName)
        if curStats is None:
            return self.setWords(node.parent, "MISSING")
        maxRH = self.getValue(curStats, "Max")
        if maxRH > self.humidityRecovery_percentage(tree, node):
            return self.setWords(node, "EXCELLENT")       
        timeRange = node.getTimeRange()
        prevTimeRange = self.adjustTimeRange(timeRange, -24)
        prevStats = tree.stats.get(elementName, prevTimeRange, node.getAreaLabel(),
                                   statLabel="mode", mergeMethod="Max")
        if prevStats is None:
            return self.setWords(node, "")        
        curStats = self.getValue(curStats)
        prevStats = self.getValue(prevStats)
                    
        diff = curStats - prevStats
        words = ""
        for threshold, label in self.humidityRecovery_valueList(tree, node):
            if diff <= threshold:
                words = label
                break
        return self.setWords(node, words)
            
    #  Humidity recovery values
    def humidityRecovery_valueList(self, tree, node):
        "Used to convert percent difference to corresponding category"
        # If you want to return different thresholds based on edit areas:
        #
        # editAreaNames = ["area1", "area2"]
        # if self.currentAreaContains(tree, editAreaNames):
        #    return [
        #      (15, "POOR"),
        #      (20, "FAIR"),
        #      (30, "GOOD"),
        #    ]
        return [
            (25, "POOR"),
            (55, "MODERATE"),
            (70, "GOOD"),
            (100,"EXCELLENT"),
            ]

    ### LAL
    ##    ###################################
    ##    # General LAL Definition          #  
    ##    # mainly defined for wrn 1/2 of US#
    ##    # but still different across the  #
    ##    # country   (from Dave Metze      #
    ##    ###################################

    ##    LALs.....(L)ightning (A)ctivity (L)evels numbered 1 through 6.

    ##        * LAL 1 - No thunderstorms.
    ##        * LAL 2 - isolated/slight chance thunderstorms..............PoP 5-14%
    ##        * LAL 3 - isolated/slight chance thunderstorms..............PoP 15-24%
    ##        * LAL 4 - scattered/chance thunderstorms....................PoP 25-54%
    ##        * LAL 5 - numerous/likely to wide/def thunderstorms.........PoP 55-100%
    ##        * LAL 6 - Dry lightning(thunderstorms) for LAL 3 through 5..PoP 15-100%

    ##    The number of lightning strikes is also a variable, but not sure if the
    ##    science of meteorology is in place to forecast the actual number of 
    ##    lightning strikes, so most offices use the potential/coverage of 
    ##    thunderstorms to determine the LAL value. 

    def lal_phrase(self):
        return {
            "setUpMethod": self.lal_setUp,
            "wordMethod": self.lal_words,
            "phraseMethods":  [
               self.consolidatePhrase,
               self.checkLocalEffects,
               self.combinePhraseStats,
               self.combineWords,
               self.fillNulls,
               self.timeDescriptorModeration,
               self.assembleSubPhrases,
               self.postProcessPhrase,
              ]       
            }

    def lal_setUp(self, tree, node):
        # Wait for all Wx (and other) phrases to complete
        # NOTE that we are sending the areaLabel so IF you have a local
        # effect set up for LAL, you must have the same local effect set up
        # for weather, AND LAL and weather should be consistent in the grids.
        phraseList = self.checkPhrasesDone(
            tree, node, areaLabel=node.getAreaLabel(),
            exceptions=[node.get('name')])
        if phraseList is None:
            return

        # Check to see if the weather phrase changed it's resolution.
        # If so, we want to match it in the LAL phrase
        nodeRes = None
        for phrase in phraseList:
            firstElement = phrase.get("firstElement")
            if firstElement is not None and firstElement.name=="Wx":
                resolution = phrase.get('resolution')
                if resolution is not None:
                    nodeRes = resolution
                    break
        
        # Use resolution of Wx phrase        
        elementInfoList = [self.ElementInfo("LAL", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector,
                            resolution=nodeRes)  
        node.set("descriptor", "")
        node.set("indentLabel", "LAL.................")
        return self.DONE()

    def lal_words(self, tree, node) :
        "Create phrase for Lightning Activity Level"
        #statDict = node.getStatDict()
        #stats = self.getStats(statDict, "LAL")
        # Check low pop.  If low, set LAL to 1.
        popThreshold = self.pop_wx_lower_threshold(tree, node)
        lowPopFlag = self.lowPop_flag(tree, node, popThreshold)
        if lowPopFlag == 1:
            lal = 1
        else:
            lal = self.matchToWx(tree, node, "LAL")
            if lal is None:
                return self.setWords(node, "null")
        if self._lightningPhrases:
            words = self.lal_value(tree, node, lal)
        else:            
            words = `int(lal)`
        return self.setWords(node, words)

    def lal_value(self, tree, node, lal):
        value = "NO TSTMS"
        if lal > 1:
            value = "1-8 STRIKES"
        if lal > 2:
            value = "9-15 STRIKES"
        if lal > 3:
            value = "16-25 STRIKES"
        if lal > 4:
            value = ">25 STRIKES"
        if lal > 5:
            value = "DRY LIGHTNING"
        return value
    
    def coverageLAL_value(self, coverage):
        # LAL ranges that correspond to each of the weather coverages
        lalValue = self.coverageLAL_table()
        return lalValue[coverage]
    
    def coverageLAL_table(self):
        # LAL ranges that correspond to each of the weather coverages
        return {
            "<NoCov>": (1,1),          
            "Iso":  self.lal_2_3,
            "SChc": self.lal_2_3,
            "Patchy": self.lal_wx_value,
            "Areas":self.lal_wx_value,
            "Chc":  self.lal_wx_value,
            "Sct":  self.lal_wx_value,
            "Lkly": self.lal_wx_value,
            "Num":  self.lal_wx_value,
            "Brf":  self.lal_wx_value,
            "Frq":  self.lal_wx_value,
            "Ocnl": self.lal_wx_value,
            "Pds":  self.lal_wx_value,
            "Inter":self.lal_wx_value,
            "Def":  self.lal_wx_value,
            "Wide": self.lal_wx_value,
            }

    def lal_2_3(self, tree, node, highKey):
        pop = tree.stats.get("PoP", node.getTimeRange(), node.getAreaLabel(),
                             mergeMethod="Max")
        if pop > 10:
            if highKey.wxType() == "T" and "Dry" in highKey.attributes():
                return (6,6)
            else:
                return (3,3)
        else:
            return (2,2)
        
    def lal_wx_value(self, tree, node, highKey):
        # Check for Dry Thunderstorms
        if highKey.wxType() == "T" and "Dry" in highKey.attributes():
            return (6,6)
        coverage = highKey.coverage()
        lal_dict = {
            "Patchy": (2,2),
            "Areas":(4,4),
            "Chc":  (4,4),
            "Sct":  (4,4),
            "Lkly": (5,5),
            "Num":  (5,5),
            "Brf":  (5,5),
            "Frq":  (5,5),
            "Ocnl": (5,5),
            "Pds":  (5,5),
            "Inter":(5,5),
            "Def":  (5,5),
            "Wide": (5,5),
            }
        return lal_dict[coverage]


    ### MixHgt
    def mixingHgt_phrase(self):
        return {
            "setUpMethod": self.mixingHgt_setUp,
            "wordMethod": self.mixingHgt_words,
            "phraseMethods": self.standard_phraseMethods(), 
            }    
    def mixingHgt_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("MixHgt", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "MIXING HEIGHT.......")
        return self.DONE()

    def mixingHgt_words(self, tree, node):
        "Create phrase for Mixing Height"

        statDict = node.getStatDict()
        stats = self.getStats(statDict, "MixHgt")
        if stats is None:
            return self.setWords(node.parent, "MISSING")

        mix1, mix2 = self.getValue(stats, "MinMax")
        outUnits = self.element_outUnits(tree, node, "MixHgt", "MixHgt")
        mix1 = int(mix1)
        mix2 = int(mix2)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, "MixHgt", "MixHgt"), max)
        if int(mix1) < threshold and int(mix2) < threshold:
            return self.setWords(node, "null")
        
        # Single Value input
        if  mix1 == mix2:
                words =  `mix1` + " " + outUnits + " agl"
        # Range
        else:
            words =  `mix1`+ "-" + `mix2` + " " + outUnits + " agl"
        return self.setWords(node, words)

    ###---------------------------------------------------------
    ### OPTIONAL Phrase: Contributed by Ben Moyer, LOX
    ### Marine Layer - taken from mixingHgt phrase methods
    ###---------------------------------------------------------

    def marineLayer_phrase(self):
        return {
            "setUpMethod": self.marineLayer_setUp,
            "wordMethod": self.marineLayer_words,
            "phraseMethods": self.standard_phraseMethods(),
            }

    def marineLayer_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("MarineLayer", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        
        # Do not generate for desert areas
        #desertArea = ["Desert"]
        #if self.currentAreaContains(tree, desertArea):
        #    return self.DONE()
        
        node.set("descriptor", "")
        node.set("indentLabel", "MARINE LAYER........")
        return self.DONE()
    
    def marineLayer_words(self, tree, node):
        "Create phrase for Marine Layer"

        statDict = node.getStatDict()
        stats = self.getStats(statDict, "MarineLayer")
        if stats is None:
            return self.setWords(node.parent, "MISSING")

        mix1, mix2 = self.getValue(stats, "MinMax")
        outUnits = self.element_outUnits(tree, node, "MixHgt", "MixHgt")
        mix1 = int(mix1)
        mix2 = int(mix2)
        
        # Single Value input
        # Makes "0 ft asl" be returned as "none"
        if mix1 == 0 and mix2 == 0:
            words = "none"
        elif mix1 == mix2:
            words =  `mix1` + " " + outUnits + " asl"
        # Makes phrases such as "0-800 ft asl" be simply "800 ft asl"
        elif mix1 == 0 and mix2 > 0:
            words = `mix2` + " " + outUnits + " asl"
        # Range
        else:
            words =  `mix1`+ "-" + `mix2` + " " + outUnits + " asl"        
        return self.setWords(node, words)
