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
# VectorRelatedPhrases.py
# Methods for producing text forecast from SampleAnalysis statistics.
#
# Author: hansen
# ----------------------------------------------------------------------------

import PhraseBuilder

class VectorRelatedPhrases(PhraseBuilder.PhraseBuilder):
    def __init__(self):    
        PhraseBuilder.PhraseBuilder.__init__(self)
        
    ############################################      
    ### VECTOR PHRASES
    
    def standard_vector_phraseMethods(self):
        return [
            self.consolidatePhrase,
            self.checkLocalEffects,
            self.combinePhraseStats,
            self.consolidateDirection,
            self.consolidateTrends,
            self.chooseMostImportant,
            self.combineWords,
            self.fillNulls,
            self.timeDescriptorModeration,
            self.embedDescriptor,
            self.assembleSubPhrases,
            self.postProcessPhrase,
            ]

    ### Wind        
    def lake_wind_thresholds(self, tree, node):
        # Return upper and lower lake_wind thresholds in mph.
        # Only apply phrase for max wind speed of 25 to 35 mph.  At 35 mph
        # and higher, an advisory of some sort will be in effect and phrase
        # will not be needed.
        return 25, 35

    def lake_wind_areaNames(self, tree, node):
        # Return list of edit area names for which the lake_wind_phrase
        # should be generated
        # If you want the phrase potentially generated for all zones, use:
        # return ["ALL"]
        return []

    def useWindsForGusts_flag(self, tree, node):
        # Turn this on if you want to use the maximum Wind
        # for reporting Gusts if a WindGust grid is not found.
        # Note that if the difference between the maximum wind speed
        # and the reported wind speed (e.g. using stdDevMinMax) is
        # not greater than the gust_wind_difference_nlValue,
        # no wind gust will be reported.
        return 0
    
    def lake_wind_phrase(self):
        return {
            "setUpMethod": self.lake_wind_setUp,
            "wordMethod": self.lake_wind_words,
            "phraseMethods": self.standard_phraseMethods()  
            }    
    def lake_wind_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("Wind", "Max", self.VECTOR())]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)                           
        return self.DONE()

    def lake_wind_words(self, tree, node):
        # Wind Statistics -- vectorAvg, vectorMinMax -- any temporal resolution
        # Customization Points:
        #    lake_wind_areaNames
        #    lake_wind_thresholds
        #    descriptor_phrase for lakeWinds

        timeRange = node.getTimeRange()        
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "Wind")
        if stats is None:
            return self.setWords(node, "")
        areaNames = self.getCurrentAreaNames(tree)
        include_phrase = 0
        lakeWindNames = self.lake_wind_areaNames(tree, node)
        if "ALL" in lakeWindNames:
            include_phrase = 1
        else:
            for areaName in areaNames:
                if areaName in self.lake_wind_areaNames(tree, node):
                    include_phrase = 1
                    break            
        if include_phrase == 0 or stats is None:
            return self.setWords(node, "")

        max, dir = self.getValue(stats, "Max", self.VECTOR())
        phrase = ""
        lower_threshold, upper_threshold = self.lake_wind_thresholds(tree, node)
        if max >= lower_threshold and max < upper_threshold:
            descriptor = self.phrase_descriptor(tree, node, "lakeWinds", "Wind")
            phrase = descriptor
            node.getParent().set("descriptor", "")
        return self.setWords(node, phrase)
 
    #  Wind Range methods 
    def wind_phrase(self):
        return {
            "setUpMethod": self.wind_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),  
            }
    
    def wind_withGusts_phrase(self):
        return {
            "setUpMethod": self.wind_withGusts_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),  
            }    
        
    def wind_withGusts_setUp(self, tree, node):
        return self.wind_setUp(tree, node, gustFlag=1)
        
    def wind_setUp(self, tree, node, gustFlag=0, element="Wind", connectorMethod=None):
        wind = self.ElementInfo(element, "List", self.VECTOR())
        elementInfoList = [wind]
        if gustFlag:
            windGust = self.ElementInfo(
                "WindGust", "Max", phraseDef=self.gust_phrase)
            elementInfoList.append(windGust)
            node.set("gustFlag", 1)
        if connectorMethod is None:
            connectorMethod = self.vectorConnector
        self.subPhraseSetUp(tree, node, elementInfoList, connectorMethod)
        return self.DONE()
        
    def vector_words(self, tree, node):
        # Create a words for a vector element
        elementInfo = node.getAncestor("firstElement")
        if elementInfo is None:
            return self.setWords(node, "")
        words = self.simple_vector_phrase(tree, node, elementInfo)
        if words == "null":
            return self.setWords(node, "null")
        gustPhrase = ""
        if words != "":
            # Add gusts
            gustFlag = node.getAncestor("gustFlag")
            if gustFlag == 1:
                windStats = tree.stats.get("Wind", node.getTimeRange(), node.getAreaLabel(),
                                             mergeMethod="Max")
                if windStats is not None:                    
                    maxMag, dir = windStats
                    statDict = node.getStatDict()
                    gustStats = self.getStats(statDict, "WindGust")
                    subRange = node.get("timeRange")
                    gustPhrase = self.embedded_gust_phrase(
                        tree, node, gustStats, maxMag, subRange)
        return self.setWords(node, words + gustPhrase)
                         
    def embedded_gust_phrase(self, tree, node, gustStats, maxWind, subRange):
        # Determine what type of gust phrase to add. Day and night are treated
        # differently with gusts phrases toned down a bit for night.
        try:
            includeTropical = self._includeTropical
        except:
            includeTropical = False
        if includeTropical:
            statLabel = "" # Use the moderatedMinMax from the Tropical components
        else:
            statLabel = "vectorMinMax"
        gusts = None
        if gustStats is None:
           # If useWindForGusts_flag is set, use max Wind for reporting gusts
           if self.useWindsForGusts_flag(tree,  node) == 1:
               windStats = tree.stats.get(
                       "Wind", subRange, node.getAreaLabel(), statLabel=statLabel,
                       mergeMethod="Max")
               if windStats is None:
                   return ""
               else:
                   gusts, dir = windStats
        else:
            gusts = self.getValue(gustStats,"Max")
        if gusts is None:
            return ""

        if includeTropical:
            #  Round gusts and maxWind to the nearest 5 kt regardless of users' overrides
            gusts = self.round(gusts, 'Nearest', 5.0)
            maxWind = self.round(maxWind, 'Nearest', 5.0)
        
        threshold = self.nlValue(self.null_nlValue(tree, node, "WindGust", "WindGust"), gusts)
        if gusts < threshold:
            return ""
        gustPhrase = ""
        outUnits = self.element_outUnits(tree, node, "WindGust", "WindGust")
        units = self.units_descriptor(tree, node, "units", outUnits)
        windDifference = self.nlValue(self.gust_wind_difference_nlValue(tree, node), maxWind)
        if gusts - maxWind > windDifference:
            gustPhrase = " with gusts to around " + `int(gusts)` + " " + units
        return gustPhrase
     
    def simple_vector_phrase(self, tree, node, elementInfo, checkRepeating=1):
        # Create a vector subPhrase
        # Do not repeat mag, dir if same as previous phrase
        elementName = elementInfo.name
        statDict = node.getStatDict()
        stats = self.getStats(statDict, elementName)
        if stats is None:
            return ""
        mag, dir = stats
        minMag, maxMag = self.getValue(mag, "MinMax")
        
        # Save maxMag at component level for other methods to use.
        # THIS IS PARTICULARLY IMPORTANT FOR USE IN THE includeOnlyPhrases_list def
        # below to eliminate certainly wx elements during tropical cyclone
        # situations when certain conditions are met.
        component = node.getComponent()
        maxMagList = component.get("maxMagList")
        if maxMagList is None:
            maxMagList = [maxMag]
        else:
            maxMagList.append(maxMag)
        component.set("maxMagList", maxMagList)

        words = self.vector_mag(tree, node, minMag, maxMag,
                                elementInfo.outUnits, elementName)
        if words == "null":
            return words
        magStr = words
        dirStr = self.vector_dir(dir)

        if checkRepeating:
            # Set for future reference
            node.set("dirStr", dirStr)
            node.set("magStr", magStr)        
            node.set("minMag", minMag)
            node.set("maxMag", maxMag)
            if minMag == 0.0:
                minMag = maxMag
            # Check for repeating mag or dir
            prevNode = node.getPrev()
            if prevNode is not None:
                prevDirStr = prevNode.get("dirStr")
                prevMagStr = prevNode.get("magStr")
                prevMin = prevNode.get("minMag")
                prevMax = prevNode.get("maxMag")
                if prevMin == 0.0:
                    prevMin = prevMax
                if prevMin is None or prevMax is None or \
                   prevDirStr is None or prevMagStr is None:
                    pass
                elif prevDirStr == dirStr and prevMagStr == magStr:
                    pass
                elif prevDirStr == dirStr:
                    dirStr = ""
                elif prevMagStr == magStr:
                    magStr = ""
                # Prevent "around 10 becoming 5 to 10"
                #         "around 10 becoming 10 to 15"
                elif prevMin == prevMax:
                    if (minMag == prevMax - 5.0) or (maxMag == prevMax + 5.0):
                        magStr = ""
                # Prevent "5 to 10 becoming around 10"
                #         "10 to 15 becoming around 10"
                elif minMag == maxMag:
                    if (prevMin == maxMag - 5.0) or (prevMax == maxMag + 5.0):
                        magStr = ""
        words = dirStr + self.format(magStr)
        return words.lstrip()
 
    def vector_mag(self, tree, node, minMag, maxMag, units,
                   elementName="Wind"):
        "Create a phrase for a Range of magnitudes"

        # Check for "null" value (below threshold)
        threshold = self.nlValue(self.null_nlValue(
            tree, node, elementName, elementName), maxMag)
        if maxMag < threshold:
            return "null"
            
        # Apply max reported threshold
        maxReportedMag = self.maxReported_threshold(tree, node, elementName, elementName)
        if maxMag >= maxReportedMag:
            maxMag = maxReportedMag
            #minMag = 0

        units = self.units_descriptor(tree, node, "units", units)

        if elementName == "Wind":
            if self.marine_wind_flag(tree, node):
                return self.marine_wind_mag(tree, node, minMag, maxMag, units, elementName)

        # Check for SingleValue
        if maxMag == minMag: #or minMag == 0:
            around = self.addSpace(
                self.phrase_descriptor(tree, node, "around", elementName))
            words =  around + `int(maxMag)` + " " + units    
        else:    
            if int(minMag) < threshold:
                upTo = self.addSpace(
                    self.phrase_descriptor(tree, node, "up to", elementName))
                words = upTo + `int(maxMag)` + " " + units
            else:
                valueConnector = self.value_connector(tree, node, elementName, elementName)
                words =  `int(minMag)` + valueConnector + `int(maxMag)` + " " + units

        # This is an additional hook for customizing the magnitude wording
        words = self.vector_mag_hook(tree, node, minMag, maxMag, units, elementName, words)
        return words
    
    def vector_mag_hook(self, tree, node, minMag, maxMag, units, elementName, words):
        # Further refinement and customization of the wind phrase can be done here
        return words
        
    def marine_wind_mag(self, tree, node, minMag, maxMag, units, elementName):
        # Produce marine descriptor wording such as "storm force", "gales"
        specialDescriptor = 0
        prevSpecial = None
        if node.getIndex() > 0 and self.marine_wind_verbose_flag(tree, node) == 0:
            # Check for previous descriptor
            prevSpecial = node.getPrev().get("specialDescriptor")
        # Check for special descriptors
        windWordList = [(64, "hurricane force winds to"),
                        (45, "storm force winds to"),
                        (34, "gales to"),
                        ]
        for threshold, windWords in windWordList:
            if maxMag >= threshold: 
                descriptor = self.addSpace(
                    self.phrase_descriptor(tree, node, windWords, elementName))
                if descriptor == prevSpecial:
                    descriptor = ""
                words = descriptor  + `int(maxMag)` + " " + units 
                specialDescriptor = 1
                break

        if not specialDescriptor:
            if maxMag > 25:
                descriptor = self.addSpace(
                    self.phrase_descriptor(tree, node, "up to", elementName))
                words = descriptor + `int(maxMag)` + " " + units 
            else:
                if minMag == maxMag or minMag == 0:
                    around = self.addSpace(
                        self.phrase_descriptor(tree, node, "around", elementName))
                    words = around + `int(maxMag)` + " " + units
                else:
                    valueConnector = self.value_connector(tree, node, elementName, elementName)
                    words =  `int(minMag)` + valueConnector + `int(maxMag)` + " " + units
        else:
            # If special marine descriptor is included in the resulting
            # words for the first subPhrase, turn off the phrase descriptor
            if node.getIndex() == 0:
                node.getParent().set("descriptor","")
            node.set("specialDescriptor", descriptor)
           
        return words     

   
    def embedDescriptor(self, tree, node):
        # See if ready to process
        if not self.phrase_trigger(tree, node):
            return
        # If appropriate, embed descriptor in first part of non-empty subPhrase
        elementInfoList = node.get("elementInfoList")
        if len(elementInfoList) < 1:
            return self.DONE()
        elementName = node.getAncestor("firstElement").name
        if self.embedded_vector_descriptor_flag(
                tree, node, elementName, elementName) == 0:
            return self.DONE()
        for node in node.get("childList"):
            words = node.get("words")
            if words is None:
                return
            # Find first non-empty phrase to embed descriptor
            if words != "":
                #if node.get("null"):
                if self.isNull(node):
                    # Do not embed descriptor into null-filled words
                    break
                dirStr = node.get("dirStr")
                if dirStr is not None:
                    # Embed only if there is a dirStr
                    phrase = node.getParent()
                    descriptor = phrase.get("descriptor")
                    phrase.set("embeddedDescriptor", descriptor)
                    descriptor = self.format(descriptor)
                    words = words.replace(dirStr, dirStr + descriptor, 1)
                    node.set("words", words)
                    phrase.set("descriptor", "")
                break
        return self.DONE()
  
    def wind_summary(self):
        return {
            "setUpMethod": self.wind_summary_setUp,
            "wordMethod": self.wind_summary_words,
            "phraseMethods": self.standard_phraseMethods(),  
            }    
    def wind_summary_setUp(self, tree, node):
        elementInfoList = []
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        return self.DONE()
                                                                  
    def wind_summary_words(self, tree, node):
        # Uses vectorAvg, vectorMedian, vectorMinMax
        elementName = "Wind"
        words = self.vector_summary(tree, node, elementName)
        return self.setWords(node, words)
     
    def vector_summary(self, tree, node, elementName):
        "Determine summary of given element"
        # Uses vectorAvg, vectorMedian, vectorMinMax
        stats = tree.stats.get(
            elementName, node.getTimeRange(), node.getAreaLabel(),
            mergeMethod="Max")
        if stats is None:
            return ""
        max, dir = stats
        return self.vector_summary_valueStr(max, elementName)
        

    def vector_summary_valueStr(self, value, elementName):
        # Thresholds and corresponding phrases
        # Defaults are for Winds converted to  mph
        words = ""
        if value < 25:
            words = ""
        elif value < 30:            
            words = "breezy"
        elif value < 40:
            words = "windy"
        elif value < 50:
            words = "very windy"
        elif value < 74:
            words = "strong winds"
        else:
            words = "hurricane force winds"
        return words

    ### WindGust
    def gust_wind_difference_nlValue(self, tree, node):
        # Difference between gust and maxWind below which gusts are not mentioned
        # Units are mph
        return 10
    
    #  WindGust
    def gust_phrase(self):
        return {
            "setUpMethod": self.gust_setUp,
            "wordMethod": self.gust_words,
            "phraseMethods": self.standard_phraseMethods(),  
            }    
    def gust_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("WindGust", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)   
        return self.DONE()
            
    def gust_words(self, tree, node):
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "WindGust")
        if stats is None:
            return self.setWords(node, "")
        gustValue = self.getValue(stats, "Max")
        threshold = self.nlValue(self.null_nlValue(tree, node, "WindGust", "WindGust"), gustValue)
        if gustValue < threshold:
            return self.setWords(node, "null")
        # Check WindGust against Wind
        maxWind, dir = tree.stats.get("Wind", node.getTimeRange(), node.getAreaLabel(),
                                      mergeMethod="Max")
        windDifference = self.nlValue(self.gust_wind_difference_nlValue(tree, node), maxWind)
        if gustValue - maxWind <= windDifference:
            return self.setWords(node, "null")
        outUnits = self.element_outUnits(tree, node, "WindGust", "WindGust")
        units = self.units_descriptor(tree, node, "units", outUnits)
        words =  `int(gustValue)` + " " + units        
        return self.setWords(node, words)


    #---------------------------------------------------------------------------
    # Tropical Phrasing - Updated for OB9.5
    #---------------------------------------------------------------------------

    def windSpdProb_thresholds(self, tree, node):
        return [
            ((45.0, 80.0), (25.0, 60.0)), # Per 1
            (35.0, 20.0),                 # Per 2
            (30.0, 15.0),                 # Per 3
            (25.0, 12.5),                 # Per 4
            (22.5, 10.0),                 # Per 5
            (20.0,  8.0),                 # Per 6
            (17.5,  7.0),                 # Per 7
            (15.0,  6.0),                 # Per 8
            (12.5,  5.0),                 # Per 9
            (10.0,  4.0),                 # Per 10
            ]
    
    def firstComponentPeriod(self, tree, node):
        # Define forecast period number for first component of this product.
        # This is for greater flexibility in production of a tropical SAF
        # valid values 1-14
        return 1

    def includeOnlyPhrases_list(self, tree, component):
        """
           Used for Tropical phrases.
           Determines which phrases to keep in each period of the product.
        """
        # Return list of phrases to include in the component
        # Return an empty list if all phrases should be included
        try:
            includeTropical = self._includeTropical
        except:
            includeTropical = False
        if not includeTropical:
            return []

        # See which period we are in
        compPeriod = int(component.getIndex() + self.firstComponentPeriod(tree, component))
        self.debug_print("Working in Period %d" % (compPeriod), 1)

        #  See which list of periods we may want to modify
        if self._pil.find("ZFP") == 0:
            productType = "ZFP"
            includeSomeList = [1]
        else:
            productType = "CWF"
            includeSomeList = [6,7,8,9,10]

        #  If this is not one of the periods we might want to remove phrases,
        #  then return
        if compPeriod not in includeSomeList:
            #  Ensure all phrases are used
            return []

        #  Grab thresholds for this period - handle the first period case
        windSpdProb_thresholds = self.windSpdProb_thresholds(tree, component)
        if compPeriod == 1:
            (thresh34low, thresh34high) = windSpdProb_thresholds[0][0]
            (thresh64low, thresh64high) = windSpdProb_thresholds[0][1]

            #  Display thresholds so we know what we're using
            self.debug_print("34 kt thresholds = (%.2f, %.2f)" %
                             (thresh34low, thresh34high), 1)
            self.debug_print("64 kt thresholds = (%.2f, %.2f)" %
                             (thresh64low, thresh64high), 1)

        #  Otherwise, handle all other periods
        else:
            index = int(component.getIndex())
            (thresh34, thresh64) = windSpdProb_thresholds[index]

            #  Display thresholds so we know what we're using
            self.debug_print("(34 kt threshold, 64 kt threshold) = (%.2f, %.2f)" %
                             (thresh34, thresh64), 1)

        #  Get some information about this forecast period
        dayNight = self.getPeriod(component.getTimeRange(), 1)
        timeRange = component.getTimeRange()
        areaLabel = component.getAreaLabel()
        self.debug_print("dayNight = %s\ttimeRange = %s" % (dayNight,
                                                            repr(timeRange)), 1)

        # Get pws64
        if dayNight == 1:
            pws64 = tree.stats.get("pwsD64", timeRange, areaLabel, mergeMethod="Max")
            self.debug_print("USING pwsD64", 1)
        else:
            pws64 = tree.stats.get("pwsN64", timeRange, areaLabel, mergeMethod="Max")
            self.debug_print("USING pwsN64", 1)

        self.debug_print("PWS64 = %s" % (pws64), 1)

        if pws64 is None:
            return []


        # Get pws34
        if dayNight == 1:
            pws34 = tree.stats.get("pwsD34", timeRange, areaLabel, mergeMethod="Max")
            self.debug_print("USING pwsD34", 1)
        else:
            pws34 = tree.stats.get("pwsN34", timeRange, areaLabel, mergeMethod="Max")
            self.debug_print("USING pwsN34", 1)

        self.debug_print("PWS34 = %s" % (pws34), 1)

        if pws34 is None:
            return []

        # COMMENT: Get the stored wind stats from the component level.
        # IF WE WERE TO LIMIT ELEMENTS IN THE ZFP BEYOND PERIOD 5,
        # THE WIND STAT LABEL ABOVE WOULD ALSO BE NEEDED.

        maxMagList = component.get("maxMagList")
        if maxMagList is None:
            return self.setWords(component, "") 

        self.debug_print("maxMag from includeOnlyPhrases_list: %s " % (maxMagList), 1)
        print "maxMagList from includeOnlyPhrases_list: ", maxMagList

        maxMag = 0.0
        for mag in maxMagList:
            if mag > maxMag:
                maxMag = mag

        ##  maxMag, dir = wind
        if productType == "ZFP":
            maxMag = maxMag*0.868976242
        self.debug_print("maxMag in includeOnlyPhrases_list: %s " % (maxMag), 1)
        print "maxMag in includeOnlyPhrases_list: ", maxMag

        if maxMag is None:
            maxMag = 0.0

        # Retrieve the headlineKeys stored at the component level
        headlineKeys = component.get("headlineKeys")
        if headlineKeys is None:
            headlineKeys = []

        #  If this is the first period, and in the list of periods we might
        #  want to modify
        if productType == "ZFP":
            if compPeriod == 1 and compPeriod in includeSomeList:
                if "HU.W" in headlineKeys or "HI.W" in headlineKeys:
                    if pws64 >= thresh64high and maxMag >= 64.0:
                        #  Limit the phrases we'll report
                        return ["pws_phrase", "wind_withGusts_phrase", "weather_phrase"]
                    elif pws64 >= thresh64low and maxMag >= 50.0:
                        #  Keep all phrases
                        return []
                    elif pws34 >= thresh34high and maxMag >= 34.0:
                        #  Limit the phrases we'll report
                        return ["pws_phrase", "wind_withGusts_phrase", "weather_phrase"]

                elif "TR.W" in headlineKeys or "TI.W" in headlineKeys: 
                    if pws34 >= thresh34high and maxMag >= 34.0:
                        #  Limit the phrases we'll report
                        return ["pws_phrase", "wind_withGusts_phrase", "weather_phrase"]
                    
                else:
                    return []           #  keep all phrases

        #  If this period is beyond the fifth period, and in the list of
        #  periods we might want to modify
        else:
            if compPeriod >= 6 and compPeriod in includeSomeList:
                if ((pws34 >= thresh34 or pws34+2.5 >= thresh34) and maxMag >= 20.0) \
                       or ((pws64 >= thresh64 or pws64+1.0 >= thresh64) and maxMag >= 20.0) \
                       or maxMag >= 34.0:
                    #  Limit the phrases we'll report
                    return ["pws_phrase", "weather_phrase"]
                else:
                    #  Return all phrases
                    return []


    #---------------------------------------------------------------------------
    #  Probabilistic Wind Phrase
    #---------------------------------------------------------------------------

    def pws_phrase(self):
        """
        Added to produce the tropical probabilistic wind phrase.
        """
        return {
            "setUpMethod": self.pws_setUp,
            "wordMethod": self.pws_words,
            "phraseMethods": [
                   self.combineWords,
                   self.fillNulls,
                   self.timeDescriptorModeration,
                   self.assembleSubPhrases,
                   ],
            }

    def pws_setUp(self, tree, node):
        """
        Setup method for the tropical probabilistic wind phrase.
        """
        elementInfoList = []
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        return self.DONE()

    def pws_words(self, tree, node):
        """
        Words method for the tropical probabilistic wind phrase.
        """
        # Get Wind
        self.debug_print("\nBegin period***********", 1)
        self.debug_print("\nNode time range -> %s" %
                         (repr(node.getTimeRange())), 1)
        self.debug_print("Parent time range -> %s" %
                         (repr(node.parent.getTimeRange())), 1)

        #  Get name and index of this node's component
        component = node.getComponent()
        compIndex = node.getComponent().getIndex()
        compPeriod = int(compIndex + self.firstComponentPeriod(tree, node))
        print "COMPONENT IN pws_words", compPeriod
        componentName = node.getComponentName()
        
        if self._pil.find("ZFP") == 0:
            productType = "ZFP"
        else:
            productType = "CWF"
            
        #  COMMENT: If this is one of the first 5 periods of the ZFP, or this is the CWF
        if not productType == "ZFP" or compPeriod <= 5:
            print "I AM IN: ", node.getTimeRange()
            #!!! Wait for wind phrase to complete
            #    We're assuming that all the wind phrases have completed (including
            #    local effect phrases) if one has.
            if productType == "ZFP":
                phraseList = ["wind_withGusts_phrase"]
            else:
                phraseList = ["marine_wind_withGusts_phrase"]
            windWords = self.findWords(tree, node, "Wind", phraseList = phraseList)
            self.debug_print("windWords = '%s'" % (windWords), 1)
            # Wait for Wind phrase
            if windWords is None:
                return

            # Get the stored wind stats from the component level
            maxMagList = component.get("maxMagList")
            if maxMagList is None:
                return self.setWords(node, "")

            self.debug_print("MaxMagList from pws_words %s %s" % (maxMagList,
                                                repr(node.getTimeRange())), 1)
            # print "MaxMagList from pws_words", maxMagList, node.getTimeRange()
            maxMag = 0.0
            for mag in maxMagList:
                if mag > maxMag:
                    maxMag = mag

            if productType == "ZFP":
                # print "PWS MAXMAG in MPH IS: ", maxMag
                maxMag = maxMag*0.868976242
            # print "PWS MAXMAG IN KNOTS: ", maxMag
        #
        #  COMMENT: Othwerwise Periods 6 and beyond in the ZFP.
        #  Although wind phrases are not included in extended ZFP you
        #  still need to do the analysis so tropical cyclone formatter
        #  logic can be carried out through the extended (day 5) periods.
        #
        else:
            print "I AM IN: ", node.getTimeRange()
            windStats = tree.stats.get(
                "Wind", node.getTimeRange(), node.getAreaLabel(),
                statLabel="vectorModeratedMinMax", mergeMethod="Max")
            ## print "WINDSTATS", windStats
            if windStats is None:
                return self.setWords(node, "")
            maxMag, dir = windStats
            maxMag = maxMag*0.868976242

        #  Display maximum wind speed in MPH and KTS
        self.debug_print("PWS MAXMAG in MPH IS: %s" % (maxMag), 1)
        self.debug_print("PWS MAXMAG in KTS IS: %s" % (maxMag), 1)

        dayNight = self.getPeriod(node.getTimeRange(), 1)
        self.debug_print("dayNight IS %s" % (dayNight), 1)

        #  See which grids to use for probability of 34 and 64 kts
        if dayNight == 1:
            prob34 = "pwsD34"
            prob64 = "pwsD64"
        else:
            prob34 = "pwsN34"
            prob64 = "pwsN64"
        self.debug_print("USING pws34 = "+prob34, 1)
        self.debug_print("USING pws64 = "+prob64, 1)
        pws64 = tree.stats.get(prob64, node.getTimeRange(),
                               node.getAreaLabel(), mergeMethod="Max")
        if pws64 is None:
            self.debug_print("pws64 NONE", 1)
            return self.setWords(node, "")
        pws34 = tree.stats.get(prob34, node.getTimeRange(),
                               node.getAreaLabel(), mergeMethod="Max")
        if pws34 is None:
            self.debug_print("pws34 NONE", 1)
            return self.setWords(node, "")

        #print "check   ", "check"
        ####################################################################
        #print "WORDS1", words
        words = ""
        areaLabel = tree.getAreaLabel()
        print "\nBegin period***********", node.getTimeRange()
        self.debug_print("\nNode time range -> %s" %
                         (repr(node.getTimeRange())), 1)
        self.debug_print("Parent time range -> %s" %
                         (repr(node.parent.getTimeRange())), 1)
        self.debug_print("MAXMAG IS -> %s KTS" % (maxMag), 1)
        self.debug_print("\nNode time and label -> %s %s" %
                         (repr(node.getTimeRange()),
                          repr(node.getAreaLabel())), 1)
        #tree.stats.printDictionary("Hazards")
        # Get Hazards
        headlines = tree.stats.get("Hazards", node.getTimeRange(),
                                   areaLabel, mergeMethod = "List")

        self.debug_print("maxMag = %s" % (maxMag), 1)
        self.debug_print("warningpws64 = %s" % (pws64), 1)
        self.debug_print("warningpws34 = %s" % (pws34), 1)
        self.debug_print("Headline stats for warning -> %s" %
                         (repr(headlines)), 1)
        print "maxMag = ", maxMag
        print "warningpws64 = ", pws64
        print "warningpws34 = ", pws34
        print "Headline stats for warning ", headlines

        if headlines is not None:
            # Sort the headlines by startTime
            temp = []
            for h, tr in headlines:
                temp.append((tr.startTime(), (h, tr)))
            temp.sort()
            newList = []
            for t in temp:
                newList.append(t[1])
            headlines = newList

            # Fetch the set of local headlines allowed for this product
            allowedHazards = []
            for key, allActions, cat in self.allowedHazards():
                allowedHazards.append(key)

            # Create a list of headline keys as strings e.g. HU.A
            headlineKeys = [] 
            for key, tr in headlines:  # value == list of subkeys
                if key not in allowedHazards:
                    continue
                # Don't call headlinesTimeRange_descriptor function due to
                # an exception which is caused - DR19483
                #timeDescriptor = self.headlinesTimeRange_descriptor(
                #    tree, node, key, tr, areaLabel, issuanceTime)
                if key == "<None>":
                    continue
                if key not in headlineKeys:
                    headlineKeys.append(key)
                self.debug_print("key: %s" % (key), 1)
  
            self.debug_print("headlineKeys: %s" % (repr(headlineKeys)), 1)
            words = self.getTropicalDescription(
                    tree, node, headlineKeys, maxMag, pws64, pws34)
            # Store the headlineKeys at the component node for later examination
            component = node.getComponent()
            component.set("headlineKeys", headlineKeys)

        elif headlines is None or headlines is NoData: 
            words = words + self.getTropicalDescription(
                tree, node, "", maxMag, pws64, pws34)

        #  COMMENT: If we have words from the pws_phrase during tropical cyclones
        #  the following lines of code will make sure wind_summary is
        #  not printed out.
        if words is not None and len(words.strip()) > 0:
            #  Remove the wind sumamry phrase from this component and any local
            #  effect areas - no need to replace undesirable phrases later on
            self.removeComponentPhrases(tree, node, "wind_summary",
                 areaLabels=[node.getAreaLabel(),
                             node.getComponent().getAreaLabel()
                            ])
        self.debug_print("\nSetting words '%s' for %s" %
                         (words, node.getAncestor('name')), 1)
        self.debug_print("%s %s\n" % (node.getComponentName(),
                                      repr(node.getTimeRange())), 1)
        return self.setWords(node, words)

    def getTropicalDescription(self, tree, node, headlineKeys, maxMag, pws64,
                               pws34):
        """
        Determines which tropical descriptions to use for current period.
        """
        self.debug_print("\tgetTropicalDescription")
        #  Get some information about the component of this node
        compName = node.getComponentName()
        compIndex = node.getComponent().getIndex()
        #  Convert convert component index to a forecast period number
        compPeriod = int(compIndex + self.firstComponentPeriod(tree, node))
        self.debug_print("-"*80, 1)
        self.debug_print("Component name = %s" % (compName) +
                         "\tForecast Period = %d" % (compPeriod) +
                         "\tmaxMag = %s" % (maxMag), 1)
        descMethod = None
        words = ""
        #  If this is one of the first 4 periods of the forecast
        if compPeriod <= 4:
            exec "descMethod = self.getPeriod_%d_Desc" % (compPeriod)
        #  Otherwise, If this is one of the fifth to ninth forecast periods
        elif 5 <= compPeriod <= 9:
            descMethod = self.getPeriod_5_9_Desc
        #  Otherwise
        else:
            descMethod = self.getPeriod_10_14_Desc

        #  Ensure the tropical boolean variables are set using current
        #  set of headlines
        self.tropicalBooleanConditions(headlineKeys)

        #  Get the description from this method
        if descMethod is not None:
            desc = descMethod(tree, node, maxMag, pws64, pws34)
            #  If we found the description - prepare it to be returned
            if desc != "":
                words =  " " + self.phrase_descriptor(tree, node, desc, desc)
        return words
    
    def tropicalBooleanConditions(self, headlineKeys):
        '''
        This method sets the conditions needed by subsequent methods
        (getPeriodX_Desc) to form the appropriate wording:
        Each entry in the conditionsList is
        the condition name (e.g. self._Hurricane_W) and
        the Hazard keys whose presence will trigger the condition.
        Note that we only need four conditions to cover all the
        logic for generating wording.

        '''
        conditionList = [
            ("Hurricane_W", ["HU.W", "HI.W"]),
            ("Hurricane_A", ["HU.A", "HI.A"]),
            ("TropStorm_W", ["TR.W", "TI.W"]),
            ("TropStorm_A", ["TR.A", "TI.A"]),
            ]

        for varName, hazardList in conditionList:
            found = False
            for key in hazardList:
                if key in headlineKeys:
                    found = True
                    break
            exec "self._"+varName+"= found"


##     def tropicalBooleanConditions(self, headlineKeys):
##         """
##         Sets various boolean variables used by the pws_phrase logic based
##         upon contents of current headlines.
##         """
##         self.debug_print("\ttropicalBooleanConditions")
##         #  COMMENT: All boolean variables are defined globally within the tropical
##         #  formatter, so there is nothing to 'return'
##         #  These are all the tropical headline combinations accounted for.
##         conditionLists = [
##             ["HI.W"],
##             ["HI.A"],
##             ["HU.W"],
##             ['HU.A'],
##             ["TI.W"],
##             ["TI.A"],
##             ["TR.W"],
##             ["TR.A"],
##             ["HU.A", "TR.W"],
##             ["HI.A", "TI.W"],
##             ["HU.W", "TI.W"],
##             ["HI.W", "TI.W"],
##             ["HI.W", "TR.W"],
##             ["HI.A", "TR.A"],
##             ["HU.A", "TI.A"],
##             ["HI.A", "TI.A"],
##             ["TI.W", "TR.A"],
##             ["TI.W", "HU.A"],
##             ["HI.W", "TR.A"],
##             ["HI.W", "HU.A"],
##             ["HI.W", "HU.W"],
##             ["HI.A", "HU.A"],
##             ["TI.A", "TR.A"],
##             ["TI.W", "TR.W"],
##             ["HI.A", "TI.W", "TR.W"],
##             ["HI.A","TI.W","HU.A","TR.W"],
##             ["HI.A", "TI.W", "HU.A", "TR.W"],       
##             ]

##         kLen = len(headlineKeys)
##         for keyList in conditionLists:
##             conditionName = ""
##             klLen = len(keyList)
##             if kLen == klLen: cond = True
##             else: cond = False
##             for keyStr in keyList:
##                 conditionName = conditionName + "_" + keyStr
##                 if kLen == klLen:
##                     if keyStr not in headlineKeys:
##                         cond = False
##             conditionName = conditionName.replace(".", "_")
##             exec "self." + conditionName + "= cond"  
            
##         self.debug_print("HU_A_TR_W %s" % (self._HU_A_TR_W), 1)
##         self.debug_print("HI_A_TI_W %s" % (self._HI_A_TI_W), 1)
##         self.debug_print("HI_A_TI_W_HU_A_TR_W %s" % (self._HI_A_TI_W_HU_A_TR_W), 1)
##         self.debug_print("HI_A_TI_W_TR_W %s" % (self._HI_A_TI_W_TR_W), 1)
##         self.debug_print("HI_A_TI_W_HU_A_TR_W %s" % (self._HI_A_TI_W_HU_A_TR_W), 1)
##         self.debug_print("HU_W_TI_W %s" % (self._HU_W_TI_W), 1)
##         self.debug_print("HI_W_TI_W %s" % (self._HI_W_TI_W), 1)
##         self.debug_print("HI_W_TR_W %s" % (self._HI_W_TR_W), 1)
##         self.debug_print("HI_A_TR_A %s" % (self._HI_A_TR_A), 1)
##         self.debug_print("HU_A_TI_A %s" % (self._HU_A_TI_A), 1)
##         self.debug_print("HI_A_TI_A %s" % (self._HI_A_TI_A), 1)
##         self.debug_print("TI_W_TR_A %s" % (self._TI_W_TR_A), 1)
##         self.debug_print("TI_W_HU_A %s" % (self._TI_W_HU_A), 1)
##         self.debug_print("HI_W_TR_A %s" % (self._HI_W_TR_A), 1)
##         self.debug_print("HI_W_HU_A %s" % (self._HI_W_HU_A), 1)
##         self.debug_print("HI_W %s" % (self._HI_W), 1)
##         self.debug_print("HI_A %s" % (self._HI_A), 1)
##         self.debug_print("HU_W %s" % (self._HU_W), 1)
##         self.debug_print("HU_A %s" % (self._HU_A), 1)
##         self.debug_print("HI_W_HU_W %s" % (self._HI_W_HU_W), 1)
##         self.debug_print("HI_A_HU_A %s" % (self._HI_A_HU_A), 1)
##         self.debug_print("TI_W %s" % (self._TI_W), 1)
##         self.debug_print("TI_A %s" % (self._TI_A), 1)
##         self.debug_print("TR_W %s" % (self._TR_W), 1)
##         self.debug_print("TR_A %s" % (self._TR_A), 1)
##         self.debug_print("TI_A_TR_A %s" % (self._TI_A_TR_A), 1)
##         self.debug_print("TI_W_TR_W %s" % (self._TI_W_TR_W), 1)        
            
#
# COMMENT: getPeriod_#_ definitions below contain the guts of the tropical
# cyclone formatter logic used to determine pws phrases or expressions of
# uncertainty.
#
    def getPeriod_1_Desc(self, tree, node, maxMag, pws64, pws34):
        """
        Determines contents of PWS phrase for a first period forecast.
        """
        self.debug_print("\tgetPeriod_1_Desc")

        desc = ""
        self.debug_print("Period time range = %s" %
                         (repr(node.getComponent().getTimeRange())), 1)
        self.debug_print("PWS34_wrng = %s" % (pws34), 1)
        self.debug_print("PWS64_wrng = %s" % (pws64), 1)

        #  Grab thresholds for this period - special case 2 for each
        component = node.getComponent()
        windSpdProb_thresholds = self.windSpdProb_thresholds(tree, component)
        (thresh34low, thresh34high) = windSpdProb_thresholds[0][0]
        (thresh64low, thresh64high) = windSpdProb_thresholds[0][1]

        #  Display thresholds so we know what we're using
        self.debug_print("34 kt thresholds = (%.2f, %.2f)" %
                         (thresh34low, thresh34high), 1)
        self.debug_print("64 kt thresholds = (%.2f, %.2f)" %
                         (thresh64low, thresh64high), 1)

        if self._Hurricane_A and self._TropStorm_W and not self._Hurricane_W:
        #if (self._Hurricane_W or self._Hurricane_A) and (self._TropStorm_W or self._TropStorm_A):
            if maxMag >= 34.0:
                if pws34 >= thresh34high:
                    desc = "iminTSposHR"
                else:
                    desc = "expTSposHR"
            elif pws34 >= thresh34low and maxMag >= 25.0:
                desc = "expTSposHR"
            elif pws64 >= thresh64low:
                desc = "posTSbcmgposHR"
            elif pws34 >= thresh34low or pws34+10.0 >= thresh34low or maxMag >= 25.0:
                desc = "posTS"
            else:
                desc = "" # or regular phrasing
            self.debug_print("ifelse1!!!  %s" % (maxMag))

        elif self._Hurricane_W or self._Hurricane_A:

            if maxMag >= 64.0:
                if pws64 >= thresh64high:
                    desc = "iminHR"
                else:
                    desc = "expHR"
            elif pws64 >= thresh64low and maxMag >= 50.0:
                desc = "expHR"
            elif maxMag >= 34.0:
                if pws34 >= thresh34high:
                    desc = "iminTSposHR"
                else:
                    desc = "expTSposHR"
            elif pws34 >= thresh34low and maxMag >= 25.0:
                desc = "expTSposHR"
            elif pws64 >= thresh64low:
                desc = "posHR"
            elif pws34 >= thresh34low or pws34+10.0 >= thresh34low or maxMag >= 25.0:
                desc = "posTSbcmgposHR"
            else:
                desc = "" # or regular phrasing
            self.debug_print("ifelse2!!!  %s" % (maxMag))

        elif self._TropStorm_W or self._TropStorm_A:
            if maxMag >= 34.0:
                if pws34 >= thresh34high:
                    desc = "iminTS"
                else:
                    desc = "expTS"
            elif pws34 >= thresh34low and maxMag >= 25.0:
                desc = "expTS"
            elif pws64 >= thresh64low:
                desc = "posTSbcmgposHR"
            elif pws34 >= thresh34low or pws34+10.0 >= thresh34low or maxMag >= 25.0:
                desc = "posTS"
            else:
                desc = "" # or regular phrasing
            self.debug_print("ifelse3!!!  %s" % (maxMag))

        else:
            print "check..........   ", "check"
            if maxMag >= 64.0:
                desc = "posHR"
            elif maxMag >= 34.0:
                desc = "posTS"
            elif pws64 >= thresh64low or pws64 +5.0 >= thresh64low:
                desc = "posHR"
            elif pws34 >= thresh34low or pws34+10.0 >= thresh34low:
                desc = "posTS"
            else:
                desc = ""

        return desc


    def getPeriod_2_Desc(self, tree, node, maxMag, pws64, pws34):
        """
        Determines contents of PWS phrase for a second period forecast.
        """
        self.debug_print("\tgetPeriod_2_Desc")

        desc = ""
        self.debug_print("Period time range = %s" %
                         (repr(node.getComponent().getTimeRange())), 1)
        self.debug_print("PWS34_wrng = %s" % (pws34), 1)
        self.debug_print("PWS64_wrng = %s" % (pws64), 1)

        #  Grab thresholds for this period
        component = node.getComponent()
        windSpdProb_thresholds = self.windSpdProb_thresholds(tree, component)
        (thresh34, thresh64) = windSpdProb_thresholds[1]

        #  Display thresholds so we know what we're using
        self.debug_print("(34 kt threshold, 64 kt threshold) = (%.2f, %.2f)" %
                         (thresh34, thresh64), 1)

        if self._Hurricane_A and self._TropStorm_W and not self._Hurricane_W:
        #if (self._Hurricane_W or self._Hurricane_A) and (self._TropStorm_W or self._TropStorm_A):
            if maxMag >= 34.0 or (pws34 >= thresh34 and maxMag >= 25.0):
                desc = "expTSposHR"
            elif pws64 >= thresh64:
                desc = "posTSbcmgposHR"
            elif pws34 >= thresh34 or pws34+10.0 >= thresh34 or maxMag >= 25.0:
                desc = "posTS"
            else:
                desc = "" # or regular phrasing
            self.debug_print("ifelse1!!!  %s" % (maxMag))

        elif self._Hurricane_W or self._Hurricane_A:
            if maxMag >= 64.0 or (pws64 >= thresh64 and maxMag >= 50.0):
                desc = "expHR"
            elif maxMag >= 34.0 or (pws34 >= thresh34 and maxMag >= 25.0):
                desc = "expTSposHR"
            elif pws64 >= thresh64:
                desc = "posHR"
            elif pws34 >= thresh34 or pws34+10.0 >= thresh34 or maxMag >= 25.0:
                desc = "posTSbcmgposHR"
            else:
                desc = "" # or regular phrasing
            self.debug_print("ifelse2!!!  %s" % (maxMag))

        elif self._TropStorm_W or self._TropStorm_A:
            if maxMag >= 34.0 or (pws34 >= thresh34 and maxMag >= 25.0):
                desc = "expTS"
            elif pws64 >= thresh64:
                desc = "posTSbcmgposHR"
            elif pws34 >= thresh34 or pws34+10.0 >= thresh34 or maxMag >= 25.0:
                desc = "posTS"
            else:
                desc = "" # or regular phrasing
            self.debug_print("ifelse3!!!  %s" % (maxMag))

        else:
            # print "check..........   ", "check"
            if maxMag >= 64.0:
                desc = "posHR"
            elif maxMag >= 34.0:
                desc = "posTS"
            elif pws64 >= thresh64 or pws64 +5.0 >= thresh64:
                desc = "posHR"
            elif pws34 >= thresh34 or pws34+10.0 >= thresh34:
                desc = "posTS"
            else:
                desc = ""

        return desc


    def getPeriod_3_Desc(self, tree, node, maxMag, pws64, pws34):
        """
        Determines contents of PWS phrase for a third period forecast.
        """
        self.debug_print("\tgetPeriod_3_Desc")

        desc = ""
        self.debug_print("Period time range = %s" %
                         (repr(node.getComponent().getTimeRange())), 1)
        self.debug_print("PWS34_wrng = %s" % (pws34), 1)
        self.debug_print("PWS64_wrng = %s" % (pws64), 1)

        #  Grab thresholds for this period
        component = node.getComponent()
        windSpdProb_thresholds = self.windSpdProb_thresholds(tree, component)
        (thresh34, thresh64) = windSpdProb_thresholds[2]

        #  Display thresholds so we know what we're using
        self.debug_print("(34 kt threshold, 64 kt threshold) = (%.2f, %.2f)" %
                         (thresh34, thresh64), 1)

        if self._Hurricane_A and self._TropStorm_W and not self._Hurricane_W:

            if maxMag >= 34.0 or (pws34 >= thresh34 and maxMag >= 25.0):
                desc = "expTSposHR"
            elif pws64 >= thresh64:
                desc = "posTSbcmgposHR"
            elif pws34 >= thresh34 or pws34+5.0 >= thresh34 or maxMag >= 25.0:
                desc = "posTS"
            else:
                desc = ""
            self.debug_print("ifelse1!!!  %s" % (maxMag))

        elif self._Hurricane_W:

            if maxMag >= 64.0 or (pws64 >= thresh64 and maxMag >= 50.0):
                desc = "expHR"
            elif maxMag >= 34.0 or (pws34 >= thresh34 and maxMag >= 25.0):
                desc = "expTSposHR"
            elif pws64 >= thresh64:
                desc = "posHR"
            elif pws34 >= thresh34 or pws34+5.0 >= thresh34 or maxMag >= 25.0:
                desc = "posTSbcmgposHR"
            else:
                desc = ""
            self.debug_print("ifelse2!!!  %s" % (maxMag))

        elif self._TropStorm_W:

            if maxMag >= 34.0 or (pws34 >= thresh34 and maxMag >= 25.0):
                desc = "expTS"
            elif pws64 >= thresh64:
                desc = "posTSbcmgposHR"
            elif pws34 >= thresh34 or pws34+5.0 >= thresh34 or maxMag >= 25.0:
                    desc = "posTS"
            else:
                desc = ""
            self.debug_print("ifelse3!!!  %s" % (maxMag))

        elif self._Hurricane_A:

            if maxMag >= 50.0 or pws64 >= thresh64:
                desc = "posHR"
            elif maxMag >= 25.0 or pws34 >= thresh34 or pws34+5.0 >= thresh34:
                desc = "posTSbcmgposHR"
            else:
                desc = ""
            self.debug_print("ifelse4!!!  %s" % (maxMag))

        elif self._TropStorm_A:

            if maxMag >= 34.0:
                if pws64 >= thresh64:
                    desc = "posTSbcmgposHR"
                else:
                    desc = "posTS"
            elif pws64 >= thresh64:
                desc = "posTSbcmgposHR"
            elif pws34 >= thresh34 or pws34+5.0 >= thresh34 or maxMag >= 25.0:
                desc = "posTS"
            else:
                desc = ""
            self.debug_print("ifelse5!!!  %s" % (maxMag))

        else:
            self.debug_print("HERE I AM")
            if pws64 >= thresh64 or pws64+2.5 >= thresh64:
                desc = "posHR"
            elif maxMag >= 64.0:
                desc = "posHR"
            elif pws34 >= thresh34 or pws34+5.0 >= thresh34:
                desc = "posTS"
            elif maxMag >= 34.0:
                desc = "posTS"
            else:
                desc = ""

        return desc


    def getPeriod_4_Desc(self, tree, node, maxMag, pws64, pws34):
        """
        Determines contents of PWS phrase for a fourth period forecast.
        """
        self.debug_print("\tgetPeriod_4_Desc")

        desc = ""
        self.debug_print("Period time range = %s" %
                         (repr(node.getComponent().getTimeRange())), 1)
        self.debug_print("PWS34_wrng = %s" % (pws34), 1)
        self.debug_print("PWS64_wrng = %s" % (pws64), 1)

        #  Grab thresholds for this period
        component = node.getComponent()
        windSpdProb_thresholds = self.windSpdProb_thresholds(tree, component)
        (thresh34, thresh64) = windSpdProb_thresholds[3]

        #  Display thresholds so we know what we're using
        self.debug_print("(34 kt threshold, 64 kt threshold) = (%.2f, %.2f)" %
                         (thresh34, thresh64), 1)

        if self._Hurricane_A and self._TropStorm_W and not self._Hurricane_W:

            if maxMag >= 34.0 or (pws34 >= thresh34 and maxMag >= 25.0):
                desc = "expTSposHR"
            elif pws64 >= thresh64:
                desc = "posTSbcmgposHR"
            elif pws34 >= thresh34 or pws34+5.0 >= thresh34 or maxMag >= 25.0:
                desc = "posTS"
            else:
                desc = ""
            self.debug_print("ifelse1!!!  %s" % (maxMag))

        elif self._Hurricane_W:

            if maxMag >= 64.0 or (pws64 >= thresh64 and maxMag >= 50.0):
                desc = "expHR"
            elif maxMag >= 34 or (pws34 >= thresh34 and maxMag >= 25.0):
                desc = "expTSposHR"
            elif pws64 >= thresh64:
                desc = "posHR"
            elif pws34 >= thresh34 or pws34+5.0 >= thresh34 or maxMag >= 25.0:
                desc = "posTSbcmgposHR"
            else:
                desc = ""
            self.debug_print("ifelse2!!!  %s" % (maxMag))

        elif self._TropStorm_W:

            if maxMag >= 34.0 or (pws34 >= thresh34 and maxMag >= 25.0):
                desc = "expTS"
            elif pws64 >= thresh64:
                desc = "posTSbcmgposHR"
            elif pws34 >= thresh34 or pws34+5.0 >= thresh34 or maxMag >= 25.0:
                    desc = "posTS"
            else:
                desc = ""
            self.debug_print("ifelse3!!!  %s" % (maxMag))

        elif self._Hurricane_A:

            if maxMag >= 50.0 or pws64 >= thresh64:
                desc = "posHR"
            elif maxMag >= 25.0 or pws34 >= thresh34 or pws34+5.0 >= thresh34:
                desc = "posTSbcmgposHR"
            else:
                desc = ""
            self.debug_print("ifelse4!!!  %s" % (maxMag))

        elif self._TropStorm_A:

            if maxMag >= 34.0:
                if pws64 >= thresh64:
                    desc = "posTSbcmgposHR"
                else:
                    desc = "posTS"
            elif pws64 >= thresh64:
                desc = "posTSbcmgposHR"
            elif pws34 >= thresh34 or pws34+5.0 >= thresh34 or maxMag >= 25.0:
                desc = "posTS"
            else:
                desc = ""
            self.debug_print("ifelse5!!!  %s" % (maxMag))

        else:
            self.debug_print("HERE I AM")
            if pws64 >= thresh64 or pws64+2.5 >= thresh64:
                desc = "posHR"
            elif maxMag >= 64.0:
                desc = "posHR"
            elif pws34 >= thresh34 or pws34+5.0 >= thresh34:
                desc = "posTS"
            elif maxMag >= 34.0:
                desc = "posTS"
            else:
                desc = ""

        return desc


    def getPeriod_5_9_Desc(self, tree, node, maxMag, pws64, pws34):
        """
        Determines contents of PWS phrase for a fifth to ninth period forecast.
        """
        self.debug_print("\tgetPeriod_5_9_Desc")

        desc = ""
        self.debug_print("Period time range = %s" %
                         (repr(node.getComponent().getTimeRange())), 1)
        self.debug_print("PWS34_wrng = %s" % (pws34), 1)
        self.debug_print("PWS64_wrng = %s" % (pws64), 1)

        #  Grab thresholds for this period
        component = node.getComponent()
        windSpdProb_thresholds = self.windSpdProb_thresholds(tree, component)
        (thresh34, thresh64) = \
                   windSpdProb_thresholds[node.getComponent().getIndex()]

        #  Display thresholds so we know what we're using
        self.debug_print("(34 kt threshold, 64 kt threshold) = (%.2f, %.2f)" %
                         (thresh34, thresh64), 1)

        if (pws64 >= thresh64 or (pws64 + 1.0) >= thresh64):
            desc = "posHR"
        elif maxMag >= 64.0:
            desc = "posHR"
        elif (self._Hurricane_A or self._Hurricane_W) and maxMag >= 50:
            desc = "posHR"
        elif (pws34 >= thresh34 or (pws34 + 2.5) >= thresh34):
            desc = "posTS"
        elif maxMag >= 34.0:
            desc = "posTS"
        elif (self._Hurricane_A or self._Hurricane_W or self._TropStorm_A or self._TropStorm_W) and maxMag >= 25:
            desc = "posTS"
        else:
            desc = ""

        return desc


    def getPeriod_10_14_Desc(self, tree, node, maxMag, pws64, pws34):
        """
        Determines contents of PWS phrase for a fourth period forecast.
        """
        self.debug_print("\tgetPeriod_4_Desc")

        desc = ""
        self.debug_print("Period time range = %s" %
                         (repr(node.getComponent().getTimeRange())), 1)
        self.debug_print("PWS34_wrng = %s" % (pws34), 1)
        self.debug_print("PWS64_wrng = %s" % (pws64), 1)

        #  Grab thresholds for this period
        component = node.getComponent()
        windSpdProb_thresholds = self.windSpdProb_thresholds(tree, component)  
        (thresh34, thresh64) = windSpdProb_thresholds[9]

        #  Display thresholds so we know what we're using
        self.debug_print("(34 kt threshold, 64 kt threshold) = (%.2f, %.2f)" %
                         (thresh34, thresh64), 1)

        if (pws64 >= thresh64 or (pws64 + 1.0) >= thresh64):
            desc = "posHR"
        elif maxMag >= 64.0:
            desc = "posHR"
        elif (self._Hurricane_A or self._Hurricane_W) and maxMag >= 50:
            desc = "posHR"
        elif (pws34 >= thresh34 or (pws34 + 2.5) >= thresh34):
            desc = "posTS"
        elif maxMag >= 34.0:
            desc = "posTS"
        elif (self._Hurricane_A or self._Hurricane_W or self._TropStorm_A or self._TropStorm_W) and maxMag >= 25:
            desc = "posTS"
        else:
            desc = ""

        return desc

