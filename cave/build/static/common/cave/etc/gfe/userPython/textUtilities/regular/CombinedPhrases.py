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
# CombinedPhrases.py
# Methods for producing text forecast from SampleAnalysis statistics.
#
# Author: hansen
# ----------------------------------------------------------------------------

import ScalarPhrases
import VectorRelatedPhrases
import WxPhrases
import DiscretePhrases
import string

class CombinedPhrases(ScalarPhrases.ScalarPhrases, VectorRelatedPhrases.VectorRelatedPhrases,
                  WxPhrases.WxPhrases, DiscretePhrases.DiscretePhrases):
    def __init__(self):    
        ScalarPhrases.ScalarPhrases.__init__(self)
        VectorRelatedPhrases.VectorRelatedPhrases.__init__(self)
        WxPhrases.WxPhrases.__init__(self)
        DiscretePhrases.DiscretePhrases.__init__(self)

    ############################################      
    ### COMBINED ELEMENT PHRASES

    # Weather OR Sky
    def weather_orSky_phrase(self):
        return {
            "phraseList": [
                  self.weather_phrase,
                  self.sky_phrase,
                  ],
            "phraseMethods": [
                  self.orSkyTest,
                  ],
            }    
    def orSkyTest(self, tree, node):
        " Develop phrase for weather Stats"
        # If there is not weather, report Sky
        # Check for empty words
        for child in node.get("childList"):
            words = child.get("words")
            if words is None:
                return
        # Order the weather phrases
        self.orderWxPhrases(tree, node)
        # Gather the words for the child phrases
        wordList = []
        for child in node.get("childList"):
            wordList.append((child.get("name"), child.get("words")))
        wxWords = ""
        skyWords = ""
        # If weather, use that. Else use sky.
        for name, words in wordList:
            if name == "weather_phrase":
                if wxWords != "":
                    wxWords = wxWords + ". "
                wxWords = wxWords + words
            if name == "sky_phrase":
                if skyWords != "":
                    skyWords = skyWords + ". "
                skyWords = skyWords + words                
        if wxWords != "":
           return self.setWords(node, wxWords)
        else:
           return self.setWords(node, skyWords)
   
    ### Sky, PoP, Wx
       
##       The skyPopWx_phrase will produce a combined Sky, Pop, Wx phrase IF appropriate.
       
##       For example:
##        "Partly cloudy with a 20 percent chance of showers and thunderstorms."
##        "Sunny then partly cloudy with a 20 percent chance of
##          showers and thunderstorms in the afternoon."
## 
##        NOTE: IF you are using this phrase, you must also include the
##        sky_phrase, weather_phrase and popMax_phrase in your phraseList to be
##        used if the combined phrase cannot be generated.
## 
##        Based on algorithms by Chris Gibson and Brian Walawender
##         
##        This phrase operates much like the weather_phrase according to these rules :
       
##         --Sub-phrases are split out initially based on the Wx parameter. In other words,
##          "Wx" is the primary element.
##           Sub-phrases are consolidated and combined based on "Wx".
##         --Only precip-related weather types will be included in the phrase.
##           Non-precip-related weather types will be reported in a separate weather
##           phrase.
##         --After combining, if there are more than 2 sub-phrases, this phrase
##           is removed and independent Sky, PoP, and Wx phrases are generated.
##           Otherwise, independent sky_phrase, weather_phrase, popMax_phrases
##           are removed IF the information is to be included in the skyPopWx_phrase:
##            --If there are ANY areal coverage terms in the weather phrase, the PoP will
##              be reported in a separate phrase.  However, the Sky and Wx can still be reported
##              in the combined phrase.       
##            --Only one PoP value can be reported in a combined phrase, so if there are
##              multiple sub-phrases with precip, an independent PoP phrase will be generated.
##              E.g.: Partly cloudy with a chance of rain in the morning...then a 
##              chance of thunderstorms in the afternoon. Probability of precipitation 50 percent.
       
##         --If the value for Sky is the same throughout the period it will not be repeated.
##           Instead of: Partly cloudy with a chance of rain showers in the morning
##           then partly cloudy with a slight chance of thunderstorms in the afternoon.
##           Produce: Partly cloudy. A chance of rain showers in the morning then a 
##           slight chance of thunderstorms in the afternoon.
##

##   IMPLEMENTATION: This phrase takes into consideration Sky PoP and Wx,
##   checking for required criteria ("checkSkyPopWx"), combining on a sub-phrase by
##   sub-phrase basis, special handling of non-precip
##   ("separateNonPrecip") and the word method ("skyPopWx_words").



    def useCombinedSkyPopWx(self, tree, node):
        # If set to 1, the combined skyPopWx_phrase will be used when appropriate
        # as long as it is included in the product component definition
        # If this is set to zero, the combined skyPopWx_phrase will not be used
        return 1
    
    def useSkyPopWx_consolidation(self, tree, node):
        # If set to 1, the skyPopWx phrase will consolidate weather keys that
        # span all time ranges to produce:
        #   PARTLY CLOUDY WITH A CHANCE OF RAIN.
        #   SNOW IN THE MORNING...THEN SLEET IN THE AFTERNOON.
        #
        # instead of:
        #    PARTLY CLOUDY. CHANCE OF RAIN AND SNOW IN THE MORNING
        #  ...THEN A CHANCE OF RAIN AND SLEET IN THE AFTERNOON. 
        return 0

    def skyPopWx_excludePoP_flag(self, tree, node):
        # If set to 1, PoP will not be included in the skyPopWx_phrase
        return 0
     
    def skyPopWx_phrase(self):
        return {
            "setUpMethod": self.skyPopWx_setUp,
            "wordMethod": self.skyPopWx_words,
            "phraseMethods": [
                self.skyPopWx_separateNonPrecip,
                self.skyPopWx_consolidateWx,
                self.checkLocalEffects,
                self.combinePhraseStats,
                self.checkSkyPopWx,
                self.combineWords,
                self.fillNulls,
                self.timeDescriptorModeration,
                self.assembleSubPhrases,
                self.postProcessPhrase,
                ]
            }
      
    def skyPopWx_setUp(self, tree, node):
        if self.useCombinedSkyPopWx(tree, node) == 0:
            return self.setWords(node, "")
        resolution = node.get("resolution")
        if resolution is not None:
            mergeMethod = "Average"
        else:
            mergeMethod = "List"
        elementInfoList = [self.ElementInfo("Wx", mergeMethod, self.WEATHER())]
        self.subPhraseSetUp(tree, node, elementInfoList, self.wxConnector,
                            resolution)
        if self.areal_sky_flag(tree, node):
            self.disableSkyRelatedWx(tree, node)
        spawnedWxPhrases = node.get("spawnedWxPhrases")
        if spawnedWxPhrases is None:
            node.set("spawnedWxPhrases", [])
        node.set("allTimeDescriptors", 1)
        return self.DONE()    

    def skyPopWx_separateNonPrecip(self, tree, node):
        # If > designated subPhrases, separate into precip/non-precip
        statList = self.getSubStats(node, "Wx")
        length = len(statList)
        if self.__dict__.get("_leDebug", 0):
            print "\n\nSPW separateNonPrecip", node.get('name'), node.getAreaLabel()
            print "   node", node
            print "   disabled", node.getAncestor("disabledSubkeys")
            print "   timerange", node.getTimeRange()
            print "   statList", statList
            #print "   doneList", node.doneList
            #print "   disabled", node.get('disabledSubkeys')
        if length > 0:
            precip = []
            nonPrecip = []
            for rankList in statList:
                subkeys = self.getSubkeys(rankList)
                for subkey in subkeys:
                    if subkey.wxType() == "<NoWx>":
                        continue
                    if self.precip_related_flag(tree, node, subkey):
                        precip.append(subkey)                        
                    else:
                        nonPrecip.append(subkey)
            if self.__dict__.get("_leDebug", 0): print "precip, nonPrecip", precip, nonPrecip
            if len(precip) >= 0 and len(nonPrecip) >= 1:
                # Save this information so we can remove this new phrase later if
                # we do not end up doing a combined sky, pop, weather phrase.
                #print "\nCalling splitWxPhrase for SPW"
                newPhrase = self.splitWxPhrase(
                    tree, node, nonPrecip, precip,
                    [self.separateNonPrecip, self.skyPopWx_separateNonPrecip,
                     self.consolidateWx],
                    newPhraseDef=self.weather_phrase)
                spawnedWxPhrases = node.get("spawnedWxPhrases")
                spawnedWxPhrases.append(newPhrase)
                node.set("spawnedWxPhrases", spawnedWxPhrases)
        return self.DONE()

    def skyPopWx_consolidateWx(self, tree, node):         
        # If any wxTypes span all subPhrases, separate into their own phrase
        if self.useSkyPopWx_consolidation(tree, node) == 0:
            return self.DONE()
        statList = self.getSubStats(node, "Wx")
        length = len(statList)
        subkeyDict = {}
        if self.__dict__.get("_leDebug", 0):
            print "\nSPW Consolidating ", node.get('name'), node.getAreaLabel()
            print "   node", node
            print "   disabled", node.getAncestor("disabledSubkeys")
            print "   timerange", node.getTimeRange()
            print "   statList", statList
            #print "   doneList", node.doneList
        if length > 1:
            # Count occurrences of each weather key
            for rankList in statList:
                subkeys = self.getSubkeys(rankList)
                for subkey in subkeys:
                    if subkey not in subkeyDict.keys():
                        subkeyDict[subkey] = 1
                    else:
                        subkeyDict[subkey] += 1
            # Find subkeys to disable in first phrase and second phrase,
            # respectively
            list1 = []
            list2 = []
            for subkey in subkeyDict.keys():
                count = subkeyDict[subkey]
                if count >= length:
                    list2.append(subkey)
                else:
                    list1.append(subkey)
            if self.__dict__.get("_leDebug", 0): print "list1, list2", list1, list2
            if len(list1) > 0 and len(list2) > 0:
                newPhrase = self.splitWxPhrase(
                    tree, node, list1, list2,
                    [self.consolidateWx, self.separateNonPrecip,
                     self.skyPopWx_consolidateWx])
                newPhrase.set("includeSky", 0)
                newPhrase.set("includePoP", 0)
                node.set("includePoP", 0)
        return self.DONE()

    # This method takes care of removing independent sky, pop and weather phrases if we are
    # going to report combined skyPopWx.
    #
    # For PoP:
    #   If PoP is included in the combined phrase (includePoP == 1),
    #     we remove all independent PoP phrases from the component.
    #     This means that if this is a local effect phrase,
    #     we remove the local effect Pop phrase in addition to the
    #     Pop phrase for the component area.
    #
    # For Sky:
    #   If Sky is to be included in the combined phrase (includeSky == 1),
    #     we remove the independent Sky phrase associated with this
    #     node area.
    #     
    # For Wx:
    #   We cannot simply remove all independent weather phrases because
    #     --They may have been spawned from consolidateWx or separateNonPrecip
    #       and need to remain.
    #     --If this is a local effect node, weather phrases may have been
    #       spawned by the component level area and need to remain.
    #   Thus, we remove non-spawned weather phrases for the node area.
    #   If this is a local effect area,
    #     we also remove weather phrases for the component area IFF
    #     they would be reporting the same subkeys as this skyPopWx phrase.
    #   
    def checkSkyPopWx(self, tree, node):
        # Check criteria to see if we can produce a combined phrase
        # Enhanced by Dave Zaff

        if self.__dict__.get("_leDebug", 0): print "\nCheckSPW", node.getTimeRange(), node.getAreaLabel()

        # Determine non-empty weather subPhrases
        wxSubPhrases = self.getNonEmptyWxSubPhrases(tree, node)
        length = len(wxSubPhrases)
        #print "length in skyPopWx", length
        if length > 2:
            return self.skyPopWx_cleanUp(tree, node)
        # PoP
        includePoP = self.checkIncludePoP(tree, node)
        if self.__dict__.get("_leDebug", 0):
            print "\nBefore removing independent phrases:"
            self.printCompPhrases(tree, node)
            print
            print "includePoP", includePoP
            print "If -1, clean-up. If 1, remove area and compArea popMax"
        if includePoP == -1:
            #print "cleaning up"
            return self.skyPopWx_cleanUp(tree, node)
            #return self.setWords(node, "")
        if includePoP:
            # Remove the independent PoP phrases
            # Note that we remove both the local effect phrase
            # and the component level phrase.
            self.removeComponentPhrases(
                tree, node, "popMax_phrase",
                areaLabels=[
                    node.getAreaLabel(),
                    node.getComponent().getAreaLabel()
                    ])
        node.set("includePoP", includePoP)

        # Sky
        includeSky = self.checkIncludeSky(tree, node, wxSubPhrases)
        if self.__dict__.get("_leDebug", 0):
            print "includeSky", includeSky
            print "If 1, remove sky_phrase for area"
        if includeSky:
            self.removeComponentPhrases(tree, node, "sky_phrase",
                                        areaLabels=[node.getAreaLabel()])
        node.set("includeSky", includeSky)

        # Wx
        # Don't remove the spawned phrases
        spawnedWxPhrases = node.get("spawnedWxPhrases")
        wxExceptionPhrases = self.getWxExceptions(tree, node, "weather_phrase")
        self.removeComponentPhrases(
            tree, node, "weather_phrase",
            spawnedWxPhrases + wxExceptionPhrases,
            areaLabels=[
                node.getAreaLabel(),
                node.getComponent().getAreaLabel()
                ])
        if self.__dict__.get("_leDebug", 0):
            print "Removed weather phrases", node.getAreaLabel()
            print "\nAfter removing independent phrases:"
            self.printCompPhrases(tree, node)
        return self.DONE()

    def getWxExceptions(self, tree, node, phraseName):
        # Return a list of the phrases with the given phraseName that
        #   --do not have the same areaLabel AND
        #   --do not have the same Wx stats as the given node.
        nodeStats = self.getWxStats(tree, node)
        nodeKeys = []
        for subkey, rank in nodeStats:
            nodeKeys.append(subkey)
        component = node.getComponent()
        progeny = component.getProgeny()
        wxExceptions = []
        if self.__dict__.get("_leDebug", 0): print "\nGetting exceptions for", nodeStats
        for child in progeny:
            if child.getAreaLabel() == node.getAreaLabel():
                continue
            name = child.get("name")
            if name == phraseName:
                # Check the stats
                wxStats = self.getWxStats(tree, child)
                if self.__dict__.get("_leDebug", 0): print "\nChecking", wxStats
                for subkey, rank in wxStats:
                    if subkey in nodeKeys or subkey.wxType() == "<NoWx>":
                        continue
                    if self.__dict__.get("_leDebug", 0): print "Appending"
                    wxExceptions.append(child)
        return wxExceptions

    def getWxStats(self, tree, node):
        wxStats = tree.stats.get("Wx", node.getTimeRange(), node.getAreaLabel(),
                                 mergeMethod="Average")
        wxStats = self.applyDisabled(tree, node, wxStats)
        #print "\ngetWxStats", node.getAreaLabel(), node.getTimeRange()
        #print "    wxStats", wxStats
        return wxStats

    def getNonEmptyWxSubPhrases(self, tree, node):
        wxSubPhrases = []
        for child in node.get("childList"):
            statDict = child.getStatDict()
            rankList = self.getStats(statDict, "Wx")
            if rankList is None or len(rankList) == 0:
                continue
            # See if all subkeys are NoWx
            for subkey, rank in rankList:
                if subkey.wxType() != "<NoWx>":
                    wxSubPhrases.append(child)
                    break            
        return wxSubPhrases

    def checkIncludePoP(self, tree, node):
        # Determine number of sub-phrases with precip-related Wx
        # Also, determine if there are any areal coverage terms
        # for a precip-related weather type
        # Return 1 if we are to include PoP
        #        0 if we are not to include PoP
        #       -1 if there is no precip weather and we are to abort the skyPopWx_phrase
        includePoP = 1
        withPrecip = 0
        arealCov = 0
        arealCovs = self.arealCoverages()
        pop = self.matchToWx(tree, node, "PoP", algorithm="Max")
        if pop is None:
            return -1
        
        covList=[]
        noWx = 1
        for subPhrase in node.childList:
            precipFound = 0
            statDict = subPhrase.getStatDict()
            if statDict is None:
                continue
            rankList = statDict["Wx"]
            subkeys = self.getSubkeys(rankList)
            #print "sub-phrase keys", subkeys
            for subkey in subkeys:
                if pop < self.pop_wx_lower_threshold(tree, node) and \
                   not self.pop_related_flag(tree, node, subkey):
                    noWx = 0
                elif pop >= self.pop_wx_lower_threshold(tree, node) and \
                   subkey.wxType() != "<NoWx>":
                    noWx = 0
                if noWx == 0 and self.precip_related_flag(tree, node, subkey):
                    precipFound = 1
                    cov = subkey.coverage()
                    if cov in arealCovs:
                        arealCov = 1
                    # Condition added by Dave Zaff:
                    # If any coverages are different,
                    # we will report PoP separately
                    else:
                        for getCov in covList:
                            if cov!=getCov:
                                includePoP=0
                        covList.append(cov)
            if precipFound:
                withPrecip = withPrecip + 1                    
               
        if withPrecip == 0 and noWx:
            # Do not use combined skyPopWx_phrase
            self.skyPopWx_cleanUp(tree, node)
            return -1  # Signal that we are Done

        # Check for excluding PoP
        if self.skyPopWx_excludePoP_flag(tree, node):
            return 0
        
        # If there is more than one sub-phrase with precip,  
        # report PoP independently.
        #print "withPrecip", withPrecip
        if withPrecip > 1:
            includePoP = 0

        # If there are ANY areal coverage terms for a precip-related
        # weather type, report PoP independently.
        if arealCov == 1:
            includePoP = 0
            
        # Check to see if includePoP has been set previously
        prevIncludePoP = node.get("includePoP")
        if prevIncludePoP == 0:
            includePoP = 0

        # If PoP >= 60, then report PoP independently
        if pop >= 60:
            includePoP = 0
        return includePoP
    
    def checkIncludeSky(self, tree, node, wxSubPhrases):
        #   Inclusion of Sky in the skyPopWx_phrase.
        #       The "includeSky" flag is checked in "checkSkyPopWx" and if set,
        #          the independent sky_phrase is removed.
        #       The general rules for including Sky are outlined below.
        #       In addition:
        #       The "includeSky" flag is set to zero when a new skyPopWx_phrase
        #          is spawned by "skyPopWx_consolideWx" since the sky condition
        #          will be addressed by the original skyPopWx_phrase.
        #       The "includeSky" flag may be set in "checkLocalEffects" (PhraseBuilder):
        #          If we find a local effect for the skyPopWx phrase (using the
        #          "checkSkyWxDifference" method in PhraseBuilder), then
        #          we will not includeSky in the new local effect phrases
        #          UNLESS there was also a sky local effect, in which case we
        #          will "checkIncludeSky" for the local effect nodes.
        #
        # Check to see if already set by "skyPopWx_consolidateWx" or by "checkLocalEffects"
        includeSky = node.get("includeSky")
        if includeSky is None:
            # We need to check for the following situations to set "includeSky":
            #   Number of sub-phrases (length)  Wx        Sky      includeSky
            #
            #            1                    similar    similar      1
            #    MOSTLY SUNNY WITH A 50 PERCENT CHANCE OF RAIN.
            #
            #            2                    different  similar      0
            #    MOSTLY SUNNY. A CHANCE OF RAIN THEN A SLIGHT CHANCE OF SNOW
            #    IN THE AFTERNOON.
            #
            #            1                    similar    different    0
            #    MOSTLY SUNNY IN THE MORNING THEN BECOMING PARTLY CLOUDY. A
            #    50 PERCENT CHANCE OF RAIN.
            #
            #            2                    different  different    1
            #    MOSTLY SUNNY WITH A CHANCE OF RAIN THEN PARTLY CLOUDY WITH
            #    A SLIGHT CHANCE OF SNOW IN THE AFTERNOON. A 50 PERCENT CHANCE
            #    OF RAIN AND SNOW.
            #
            # Compare sky for similarity in the 1st and 2nd half of the period.
            # Note: We can't count on Sky having a temporal resolution of [6],
            # but, since the skyPopWx_phrase bails after 2 sub-phrases,
            # looking at the 2 halves of the timeRange is sufficient.
            timeRange = node.getTimeRange()
            areaLabel = node.getAreaLabel()
            timeRange1, timeRange2 = self.splitRange(timeRange)
            skyStats1 = tree.stats.get("Sky", timeRange1, areaLabel,
                                       mergeMethod="Average")
            skyStats2 = tree.stats.get("Sky", timeRange2, areaLabel,
                                       mergeMethod="Average")
            sky1 = self.getSkyValue(tree, node, skyStats1, timeRange1)
            sky2 = self.getSkyValue(tree, node, skyStats2, timeRange2)
            #print "similarSky", self.similarSkyWords_flag(tree, node, sky1, sky2)
            # Determine if Wx is similar
            similarWx = 1
            wxSubPhraseLen = len(wxSubPhrases)
            if wxSubPhraseLen > 1:
                similarWx = 0        
            # There is one subphrase and we have to see if it covers the
            # entire phrase timeRange.  If not, we have empty and non-empty
            # weather subPhrases and similarWx is 0
            elif wxSubPhraseLen == 1 and wxSubPhrases[0].getTimeRange() != node.getTimeRange():
                similarWx = 0
            if self.similarSkyWords_flag(tree, node, sky1, sky2):
                if similarWx:
                    includeSky = 1
                else:
                    includeSky = 0
            else:
                if similarWx:
                    includeSky = 0
                else:
                    includeSky = 1
        return includeSky

    def skyPopWx_cleanUp(self, tree, node):
        # Clean up any non-precip node that we spawned
        spawnedWxPhrases =  node.get("spawnedWxPhrases")
        for phrase in spawnedWxPhrases:
            phrase.remove()
        node.set('childList', [])
        return self.setWords(node, "")

    def skyPopWx_words(self, tree, node):
        # Create a combined sky, pop, weather sub-phrase

        timeRange = node.getTimeRange()
        areaLabel = node.getAreaLabel()
        
        # Sky words
        includeSky = node.getAncestor("includeSky")
        if includeSky is None:
            includeSky = 1
        # Add sky to the statDict for this node
        if includeSky:
            skyWords = self.getSkyWords(tree, node)
            # If this is the second node, see if the sky words are similar to the first
            # If so, do not repeat
            index = node.getIndex()
            if index == 1:
                prevSkyWords = node.getPrev().get("words")
                if self.similarSkyWords_flag(tree, node, skyWords, prevSkyWords):
                    prevSkyWords = self.preferredSkyWords(tree, node, skyWords, prevSkyWords)
                    node.getPrev().set("words", prevSkyWords)
                    skyWords = ""
        else:
            skyWords = ""

        # Pop words
        includePoP = node.getAncestor("includePoP")
        if includePoP:
            pop = self.matchToWx(tree, node, "PoP")
            if pop < self.pop_lower_threshold(tree, node) or \
               pop > self.pop_upper_threshold(tree, node):
                popWords = ""
            else:
                popStr = self.getPopStr(tree, node, pop)
                popWords = "a " + popStr + " percent chance of"
        else:
            popWords = ""


        # Weather words
        self.weather_words(tree, node)
        weatherWords = node.get("words")
        if weatherWords == "null":
            weatherWords = ""
        if weatherWords == "":
            popWords = ""

##        print "\n\n", areaLabel, timeRange
##        print "includeSky", includeSky
##        print "includePoP", includePoP
##        print "skyWords", skyWords
##        print "popWords", popWords
##        print "weatherWords", weatherWords
            
        if skyWords == "" and weatherWords == "":
            return self.setWords(node, "")
        
        words = ""
        if includePoP:
            if popWords != "":
                weatherWords = string.replace(weatherWords, "a slight chance of", "")
                weatherWords = string.replace(weatherWords, "a chance of", "")
                weatherWords = string.replace(weatherWords, "slight chance of", "")
                weatherWords = string.replace(weatherWords, "chance of", "")
                weatherWords = string.replace(weatherWords, "likely", "")
                weatherWords = string.replace(weatherWords, "occasional", "")
            weatherWords = weatherWords.strip()
            
            # Do not repeat weather words from previous sub-phrase   
            node.set("weatherWords", weatherWords)
            if node.getIndex() > 0:
                weatherWords = self.checkRepeatingString(                
                    tree, node, weatherWords, "weatherWords")
                if weatherWords == "":
                    popWords = popWords.replace(" of", "")
            
            if popWords == "" and weatherWords == "":
                words = skyWords
            elif popWords == "" and skyWords == "":
                words = weatherWords
            elif skyWords == "" and weatherWords == "":
                words = popWords
            elif popWords == "":
                # There must be sky and weather
                words = skyWords + " with " + weatherWords
            elif skyWords == "":
                words = popWords + " " + weatherWords
            elif weatherWords == "":
                words = skyWords + " with " + popWords
            else:
                words =  skyWords + " with " + popWords + " " + weatherWords
        else:
            weatherWords = weatherWords.lstrip()
            if skyWords != "" and weatherWords != "":
                words = skyWords + " with " + weatherWords
            elif skyWords == "" and weatherWords != "":
                words =  weatherWords
            else:
                words =  skyWords
        return self.setWords(node, words)

    def getSkyWords(self, tree, node):
        timeRange = node.getTimeRange()
        areaLabel = node.getAreaLabel()
        sky = tree.stats.get("Sky", timeRange, areaLabel, mergeMethod="Average")
        statDict = node.getStatDict()
        statDict["Sky"] = sky
        node.set("statDict", statDict)
        self.sky_words(tree, node)
        skyWords = node.get("words")
        return skyWords

    def getSkyValue(self, tree, node, skyStats, timeRange):
        if timeRange.duration() > 12*3600:
            dayNight = -1
        else:
            dayNight = self.getPeriod(timeRange, 1)
        return self.sky_value(tree, node, skyStats, dayNight)
        
