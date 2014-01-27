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
# WxPhrases.py
# Methods for producing text forecast from SampleAnalysis statistics.
#
# Author: hansen
# ----------------------------------------------------------------------------

import PhraseBuilder
import types, re
import TimeRange
import logging

class WxPhrases(PhraseBuilder.PhraseBuilder):
    def __init__(self):    
        PhraseBuilder.PhraseBuilder.__init__(self)
        self.log = logging.getLogger("FormatterRunner.WxPhrases.WxPhrases")
        
    ############################################      
    ### WEATHER PHRASES
    def standard_weather_phraseMethods(self):
        return [
            self.preProcessWx,
            self.separateNonPrecip,
            self.consolidateVisibility,
            self.checkLocalEffects,
            self.combineWords,
            self.fillNulls,
            self.timeDescriptorModeration,
            self.checkResolution,
            self.assembleSubPhrases,
            self.postProcessPhrase,
            ]    

    ### WX
    def pop_wx_lower_threshold(self, tree, node):
        # Pop-related Wx will not be reported if Pop is below this threshold
        return self.popThreshold(tree, node, self._hoursSChcEnds, 15.0, 25.0)
    
    def pop_related_flag(self, tree, node, subkey):
        # These are weather subkeys that are related to PoP and
        # should be not be reported if pop is low
        wxType = subkey.wxType()
        if wxType in  ["ZR","R","RW","S","SW", "T", "IP"]:
            if wxType == "SW" or wxType == "RW":
                if subkey.intensity() == "--":
                    return 0
            if wxType == "T" and "Dry" in subkey.attributes():
                return 0
            return 1
        else:
            return 0
    
    def precip_related_flag(self, tree, node, subkey):
        # These are weather types that are precip versus non-precip
        # and could be separated into different phrases from the
        # non-precip weather types.
        if subkey.wxType() in  ["ZR", "R", "RW", "S", "SW", "T", "ZL", "L", "IP"]:
            return 1
        else:
            return 0

    def filter_subkeys_flag(self):
        # Filtering and condensing of weather subkeys.
        # If you do not want subkeys to be condensed and filtered, override this
        # variable and set it to 0.
        return  1
    
    def wxHierarchies(self):
    # This is the hierarchy of which coverage and intensity to choose if
    # wxTypes are the same and to be combined into one subkey.
        return {
        "wxType":["WP", "R", "RW", "T", "L", "ZR", "ZL", "S", "SW",
                  "IP", "F", "ZF", "IF", "IC", "H", "BS", "BN", "K", "BD",
                  "FR", "ZY", "BA", "<NoWx>","<Invalid>"],
        "coverage":["Def","Wide","Brf","Frq", "Ocnl","Pds", "Inter",
                    "Lkly","Num","Sct","Chc","Areas",
                    "SChc","WSct","Iso","Patchy","<NoCov>","<Invalid>"],
        "intensity":["+","m","-","--","<NoInten>","<Invalid>"],
        "visibility":["0SM", "1/4SM", "1/2SM", "3/4SM", "1SM", "11/2SM", "2SM",
                      "21/2SM", "3SM", "4SM", "5SM", "6SM", "P6SM", "<NoVis>","<Invalid>"],
        }

    def similarCoverageLists(self, tree, node, subkey1, subkey2):
        # Lists of coverages that should be combined or considered equal.
        # Each list should be ordered from weaker to stronger
        # and the stronger coverage will be kept when subkeys are
        # combined.
        #
        # These lists are examined when combining sub-phrases and when
        # determining if there is a local effect to report.
        # Called by PhraseBuilder:checkWeatherSimilarity
        #
        return [
            ['SChc', 'Iso'],
            ['Chc', 'Sct'],
            ['Lkly', 'Num'],
            ['Brf', 'Frq', 'Ocnl', 'Pds', 'Inter', 'Def', 'Wide'],
            ]

    def wxCombinations(self):
        # This is the list of which wxTypes should be combined into one
        # WITHIN a sub-phrase.
        # For example, if ("RW", "R") appears, then wxTypes of "RW" and "R" will
        # be combined into one key and the key with the dominant coverage will
        # be used as the combined key.
        # You may also specify a method which will be
        #  -- given arguments subkey1 and subkey2 and
        #  -- should return
        #     -- a flag = 1 if they are to be combined, 0 otherwise
        #     -- the combined key to be used
        #  Note: The method will be called twice, once with (subkey1, subkey2)
        #  and once with (subkey2, subkey1) so you can assume one ordering.
        #  See the example below, "combine_T_RW"
        # 
        return [
                ("RW", "R"),
                ("SW", "S"),
                self.combine_T_RW,
            ]
    
    def combine_T_RW(self, subkey1, subkey2):
        # Combine T and RW only if the coverage of T
        # is dominant over OR equal to the coverage of RW and
        # RW does not have + intensity
        wxType1 = subkey1.wxType()
        wxType2 = subkey2.wxType()
        if wxType1 == "T" and wxType2 == "RW":
            if subkey2.intensity() != "+":
                order = self.dominantCoverageOrder(subkey1, subkey2)
                if order == -1 or order == 0:
                    return 1, subkey1
        return 0, None

    # Customizing Weather Phrases
    def wxCoverageDescriptors(self):
        # This is the list of coverages, wxTypes, intensities, attributes for which special
        # weather coverage wording is desired.  Wildcards (*) can be used to match any value.
        # If a weather subkey is not found in this list, default wording
        # will be used from the Weather Definition in the server.
        # The format of each tuple is:
        #    (coverage, wxType, intensity, attribute, descriptor)
        # For example:
        #return [
        #    ("Chc", "*", "*", "*", "a chance of"),
        #    ]
        # NOTE: descriptor can be a method taking (tree, node, subkey) as arguments
        return []

    def wxTypeDescriptors(self):
        # This is the list of coverages, wxTypes, intensities, attributes for which special
        # weather type wording is desired.  Wildcards (*) can be used to match any value.
        # If a weather subkey is not found in this list, default wording
        # will be used from the Weather Definition in the server.
        # The format of each tuple is:
        #    (coverage, wxType, intensity, attribute, descriptor)
        # NOTE: descriptor can be a method taking (tree, node, subkey) as arguments
        return [
                ("*", "SW", "--", "*", "flurries"),
                ("*", "RW", "*", "*", self.rainShowersDescriptor),
                ("*", "T", "*", "Dry", "dry thunderstorms"),
                ]

    def rainShowersDescriptor(self, tree, node, subkey):
        if subkey.intensity() == "--":
            return "sprinkles"
        if tree is None:
            return "showers"
        t = tree.stats.get(
              "T", node.getTimeRange(),
              node.getAreaLabel(), statLabel="minMax",
              mergeMethod="Min")            
        if t is None:
            return "showers"
        if t < 60:
            return "rain showers"
        else:
            return "showers"

    def wxIntensityDescriptors(self):
        # This is the list of coverages, wxTypes, intensities, attribute for which special
        # weather intensity wording is desired.  Wildcards (*) can be used to match any value.
        # If a weather subkey is not found in this list, default wording
        # will be used from the Weather Definition in the server.
        # The format of each tuple is:
        #    (coverage, wxType, intensity, attribute, descriptor)
        # NOTE: descriptor can be a method taking (tree, node, subkey) as arguments
        return [
                ("*", "RW", "--", "*", ""),
                ("*", "RW", "-", "*", ""),
                ("*", "R", "--", "*", "light"),
                ("*", "R", "-", "*", ""),
                ("*", "R", "+", "*", ""),
                ("*", "RW", "+", "*", ""),
                ("*", "SW", "--", "*", ""),
                ("*", "SW", "-", "*", ""),
                ("*", "SW", "+", "*", ""),
                ("*", "S", "--", "*", "very light"),
                ("*", "S", "-", "*", ""),
                ("*", "S", "+", "*", ""),
                ("*", "T", "+", "*", ""),
                ("*", "ZR", "--", "*", "light"),
                ("*", "ZR", "+", "*", ""),
                ("*", "L", "*", "*", ""),
                ("*", "F", "+", "*", "dense"),
                ("*", "IP", "+", "*", ""),
            ]

    def wxAttributeDescriptors(self):
        # This is the list of coverages, wxTypes, intensities, attributes, for which special
        # weather attribute wording is desired.  Wildcards (*) can be used to match any value.
        # If a weather subkey is not found in this list, default wording
        # will be used from the Weather Definition in the server.
        # The format of each tuple is:
        #    (coverage, wxType, intensity, attribute, descriptor)
        # NOTE: descriptor can be a method taking (tree, node, subkey) as arguments
        return [
            ("*", "T", "*", "HvyRn", ""),
            ("*", "T", "*", "Dry", ""),
            ("*", "T", "*", "GW", ""),
            ("*", "T", "*", "DmgW", ""),
            ("*", "T", "*", "FL", ""),
            ("*", "T", "*", "LgA", ""),
            ("*", "T", "*", "SmA", ""),
            ("*", "T", "*", "TOR", ""),
            ]

    def weather_phrase(self):
        return {
            "setUpMethod": self.weather_setUp,
            "wordMethod": self.weather_words,
            "phraseMethods": self.standard_weather_phraseMethods()  
            }    
    def weather_setUp(self, tree, node):
        resolution = node.get("resolution")
        if resolution is not None:
            mergeMethod = "Average"
        else:
            mergeMethod = "List"
        elementInfoList = [self.ElementInfo("Wx", mergeMethod, self.WEATHER())]
        self.subPhraseSetUp(tree, node, elementInfoList, self.wxConnector,
                            resolution)
        node.set("allTimeDescriptors", 1)
        if self.areal_sky_flag(tree, node):
            self.disableSkyRelatedWx(tree, node)
        return self.DONE()
    
    def preProcessWx(self, tree, phrase):
        #print '---------------------------------------------------------------'
        #print 'in _preProcessWxStats -> ', phrase.get("name"),  phrase.getTimeRange()
        #time1 = time.time()

        #  Create a new list to hold all our subkeys and timeRanges
        newStats = []
        #  Create a variable to hold the subphrase methodList
        methodList = None
        #  Look through each subphrase of this phrase
        resolution = phrase.get("resolution")
        for subPhrase in phrase.childList:
            #  If we do not have a copy of the subPhrase methodList - make one
            if methodList is None:
                methodList = subPhrase.get("methodList")                
            #  Get stats for this subphrase
            if resolution is not None:
                tr = subPhrase.getTimeRange()
                rankList = tree.stats.get(
                    'Wx', tr, subPhrase.getAreaLabel(), mergeMethod="Average")
                statList = [(rankList, tr)]
            else:
                statList = tree.stats.get(
                    'Wx', subPhrase.getTimeRange(),subPhrase.getAreaLabel())
            if statList is None:
                return self.DONE()
            #  Gather the subkeys and ranks by time range
            for (rankList, timeRange) in statList:
                if rankList is None:
                    continue
                for subkey, rank in rankList:
                    newStats.append((subkey, rank, timeRange, 0))
                #  Remove this node from the tree
                #subPhrase.remove()

        #  Make a new list to hold combined stats
        combinedStats = []
        #  Define a variable to track number of changes made to new Wx stats
        changes = 1             #  We want at least one pass through the stats        
        #  Keep consolidating new statistics until no changes are made
        while len(newStats) > 0 and changes > 0:
            #  No changes made yet this pass
            changes = 0
            #  If we have already combined Wx statistics
            if len(combinedStats) > 0:
                #  Update the list of Wx stats we need to consolidate
                newStats = combinedStats
            #  Combine as many Wx stats as possible
            (changes, combinedStats) = self.combineWxStats(
                tree, subPhrase, newStats)

        #  Make a new dictionary to filter consolidated stats by time
        finalStatDict = {}
        finalKeys = []
        #  For each combined Wx statistic
        for (curKey, curRank, curTR, curCombined_flag) in combinedStats:
            #  If the dictionary does not have this time range currently
            if curTR not in finalStatDict.keys():
                #  Add this tuple to the dictionary
                finalStatDict[curTR] = [(curKey, curRank)]
                finalKeys.append(curTR)
            #  Otherwise
            else:
                tempList = finalStatDict[curTR]
                tempList.append((curKey, curRank))
                finalStatDict[curTR] = tempList

        #print "\nPhrase :", phrase.get("name")
        #print 'finalStatDict = ', finalStatDict
        #print "timeRanges", finalKeys

##        for su100bPhrase in phrase.childList:
##            print '------------------------------------------------------------'
##            print subPhrase.printNode(subPhrase)

        #  Make a list to hold all the new subphrases
        newSubPhraseList = []
        #  Sort consolidated time ranges
        #print "\nfinalKeys before sort", finalKeys
        finalKeys.sort(self.orderTimeRanges)
        #print "\nfinalKeys after sort", finalKeys
        #  Create new nodes for each of our consolidated subphrases
        for timeRange in finalKeys:
            #  Create a new subphrase for the consolidated Wx types and times
            newSubPhrase = tree.makeNode([], methodList, phrase)
            statDict = {}
            statDict['Wx'] = finalStatDict[timeRange]
            newSubPhrase.set("elementName", 'Wx')
            newSubPhrase.set("changeFlag", 0)            
            newSubPhrase.set("parent", phrase)
            newSubPhrase.set("statDict", statDict)
            newSubPhrase.set("timeRange", timeRange)
            newSubPhrase.set("childList", [])
            newSubPhrase.set("timeDescFlag", 1)     #  always include timers
            #  Keep track of this new node
            newSubPhraseList.append(newSubPhrase)

        #  Replace the old subphrases with the new subphrases
        phrase.set("childList", newSubPhraseList)
        #  Indicate Wx stats have been preprocessed\
        #print "  Time: ", time.time() - time1
        return self.DONE()   

    #  Define a method to combine Wx stats per my relaxed rules
    def combineWxStats(self, tree, subPhrase, statList):
        #  Define some variables to keep track of combined Wx subkeys
        combinedStats = []
        changes = 0
        combinedTypes = []

        #  Look through all our subkeys
        for index in range(len(statList)-1):

            #  Get the info about this key
            (curKey, curRank, curTR, curCombined_flag) = statList[index]
            
            #  Break this subkey into its components
            curCov = curKey.coverage()
            curType = curKey.wxType()
            curInten = curKey.intensity()
            curVis = curKey.visibility()
            curAttr = curKey.attributes()

            #  Shorten the list of keys we need to test
            searchStats = statList[index+1:]
##            print 'searchStats = ', searchStats

            #  See if we can combine this key with any of the other keys
            for testIndex in range(len(searchStats)):

                #  Get the infor about this key
                (testKey, testRank, testTR, testCombined_flag) = searchStats[testIndex]

                # We will only try combining if neighboring time ranges
                if curTR.endTime() != testTR.startTime():
                    continue

                #  See if there is a significant difference between these
                #  Wx subkeys
                # match is zero if there is a difference between the keys.
                #       is 1 or 2 if they can be combined.
                match = self.checkWeatherSimilarity(
                    tree, subPhrase, [(curKey, curRank)],[(testKey, testRank)],
                    tr1=curTR, tr2=testTR)

                #print 'combineWxStats %s <=> %s  ->  %d' % (curKey, testKey, match)

                #  If these keys could be combined, and this type has not been
                #  combined yet
                if match > 0 and curType not in combinedTypes:
                    newSubkey = self.makeAggregateSubkey(curKey, curRank, testKey, testRank)
                    #print "newSubkey", newSubkey
                    #  Mark both keys in current lists as being combined
                    statList[index] = (curKey, curRank, curTR, curCombined_flag + 1)
                    searchStats[testIndex] = (testKey, testTR,
                                              testCombined_flag + 1)
                    #  Make sure the search key is also marked as combined
                    #  in the main stats list
                    statList[index+testIndex+1] = (testKey, testRank, testTR,
                                                   testCombined_flag + 1)
                    #  Make a new time range for this combined key
                    newTR = TimeRange.TimeRange(curTR.startTime(), testTR.endTime())
                    #  Take highest rank of the two subkeys to be combined
                    if curRank > testRank:
                        newRank = curRank
                    else:
                        newRank = testRank
                    #  Add this new subkey to the consolidated stats
                    combinedStats.append((newSubkey, newRank, newTR, 0))
                    #  Do not try to combine this Wx subkey any more
                    changes = changes + 1
                    combinedTypes.append(curType)

        #  Make sure we did not miss any subkeys that were not combined
        for (curKey, curRank, curTR, curCombined_flag) in statList:

            #  If this key was not previously combined
            if curCombined_flag == 0:

                #  Add this key to the consolidated stats
                combinedStats.append((curKey, curRank, curTR, curCombined_flag))

##        print '\n', changes, combinedTypes
##        print 'combinedStats = '
##        print combinedStats
##        print '**********************************************************'

        #  Return number of changes and current combined Wx stats 
        return (changes, combinedStats)

    def useSimple(self, tree, node, rankList, subkeys):
        # Return 1 if we want to use simple weather phrasing
        # where the weather subkeys are simply connected by
        # a conjunction i.e. do not use "with", "mixedWith", "possiblyMixedWith",
        # "withPocketsOf".  For example:
        #
        # Simple wording:
        #    Chance of rain and snow and slight chance of sleet in the evening.
        #    Mostly cloudy with chance of rain and a slight chance of thunderstorms.
        #
        # Complex wording:
        #    Chance of rain and snow possibly mixed with sleet in the evening.
        #    Chance of rain with possible thunderstorms in the evening.
        #
        numSubkeys = len(subkeys)
        if numSubkeys <= 2:
            return 1
        elif numSubkeys <= 4:
            # Check for coverage groupings
            # If there are only 1 or 2 coverage groupings, use simple wording
            covList = []
            for subkey in subkeys:
                cov = subkey.coverage()
                if cov not in covList:
                    covList.append(cov)
            if len(covList) <= 2:
                return 1
            else:
                return 0
        else:
            return 0
        
    def rankWordingFuzzFactor(self, tree, node, subkey1, subkey2):
        # Used in weather wording to determine if
        # subkeys have significantly different ranks.
        # If so, then wording such as "possibly mixed with" or
        # "with pockets of" could be used.
        return 10

    def wxConjunction(self, tree, node, subkey1, rank1, subkey2, rank2):
        if subkey1 is not None:
            attr1 = subkey1.attributes()
        else:
            attr1 = []
        attr2 = subkey2.attributes()
        if "OR" in attr1 or "OR" in attr2:
            return " or "

        # If Def is followed by Lkly, return "with" to avoid
        # ambiguity
        cov1 = subkey1.coverage()
        cov2 = subkey2.coverage()
        if cov1 == "Def" and cov2 == "Lkly":
            return " with "
        else:
            return " and "

    def withPossible(self, tree, node, subkey1, rank1, subkey2, rank2):
        # Wording to use if subkey1 has higher rank or is dominant over subkey2
        # Handle "with little or no rain"
        wxType1 = subkey1.wxType()
        wxType2 = subkey2.wxType()
        if wxType1 in ["T"] and wxType2 in ["RW", "R"]:
            self.includeValue = 0 # i.e. do not add rain or rain showers wording
            return " with little or no rain"
        return " with possible "
    
    def withPhrase(self, tree, node, subkey1, rank1, subkey2, rank2):
        # Wording to use if subkey1 has similar rank and coverage to subkey2
        wxType1 = subkey1.wxType()
        wxType2 = subkey2.wxType()
        #print "wxType1, 2", wxType1, wxType2
        if wxType2 in ["T"]:
            return " and "
        else:
            return " with "
    
    def withPocketsOf(self, tree, node, subkey1, rank1, subkey2, rank2):
        # Wording to use if subkey2 has higher rank or is dominant over subkey1
        return " with pockets of "

    def possiblyMixedWith(self, tree, node, subkey1, rank1, subkey2, rank2):
        # Wording to use if subkey1 has higher rank or is dominant over subkey2
        wxType1 = subkey1.wxType()
        wxType2 = subkey2.wxType()
        # Handle "with possible thunderstorms"
        if wxType2 in ["T"]:
            return " with possible "
        # Handle "with little or no rain"
        if wxType1 in ["T"] and wxType2 in ["RW", "R"]:
            self.includeValue = 0 # i.e. do not add rain or rain showers wording
            return " with little or no rain"
        return " possibly mixed with "
    
    def mixedWith(self, tree, node, subkey1, rank1, subkey2, rank2):
        # Wording to use if subkey1 has similar rank and coverage to subkey2
        wxType1 = subkey1.wxType()
        wxType2 = subkey2.wxType()
        #print "wxType1, 2", wxType1, wxType2
        if wxType2 in ["T"]:
            return " and "
        else:
            return " mixed with "

    def weather_words(self, tree, node):
        # Create a phrase to describe a list of weather sub keys for one sub-period

        # Get rankList
        statDict = node.getStatDict()
        rankList = self.getStats(statDict, "Wx")
        if self._debug:
            print "\n SubKeys in weather_words", rankList
            print "   TimeRange", node.getTimeRange(), node.getAreaLabel()
            print "   Phrase name", node.getAncestor("name")
        if rankList is None or len(rankList) == 0:
            return self.setWords(node, "")

        # Check against PoP
        rankList = self.checkPoP(tree, node, rankList)

        # Check visibility
        subkeys = self.getSubkeys(rankList)
        if self.checkVisibility(tree, node, subkeys):
            return self.setWords(node, "null")

        # Get the weather words
        words = self.getWeatherWords(tree, node, rankList)
        node.set('reportedRankList', rankList)

        # Add embedded visibility
        words = self.addEmbeddedVisibility(tree, node, subkeys, words)
        if words == "":
            words = "null"
        if self._debug:
            print "   Setting words", words
            
        # To replace multiple "and's" with ellipses
        words = self.useEllipses(tree, node, words)
                
        return self.setWords(node, words)

    def checkPoP(self, tree, node, rankList):
        # Do not report pop_related subkeys if PoP is below
        # pop_wx_lower_threshold
        popThreshold = self.pop_wx_lower_threshold(tree, node)
        lowPopFlag = self.lowPop_flag(tree, node, popThreshold)
        #  Force a check of all weather subkeys
        newList = []
        for subkey, rank in rankList:
            #  If PoP threshold >= 25% and coverage is 'SChc' or 'Iso'
            #  don't mention the subkey
            if popThreshold >= 25 and subkey.coverage() in ['SChc', 'Iso']:
                continue
            #  If we have a low PoP, and this is not a precip-related subkey
            #  report it e.g. Fog
            if lowPopFlag == 1: 
               if not self.pop_related_flag(tree, node, subkey):
                    newList.append((subkey, rank))
            #  If there is no low PoP, report the subkey
            else:
                newList.append((subkey, rank))                
        return newList

    def getWeatherWords(self, tree, node, rankList):
        # For each WeatherSubKey, add it to the phrase
        # Use ranking of subkeys to form wording:
        #   If useSimple produce simple wording e.g.
        #       Chance of rain and snow and slight chance of sleet and freezing rain.
        #   Otherwise:
        #    Create a phrase of the form:
        #    <list1 of subkeys> <conjunction> <list2 of subkeys>
        #    where:
        #      list1 and list2 are lists of subkeys separated by
        #        '...' or 'and' e.g.  Snow...rain and sleet
        #      list1 subkeys have similar coverages and ranks
        #      list2 subkeys have coverages or ranks significantly
        #         different from those in list1
        #      conjunction connects the 2 lists appropriately, e.g.
        #         Snow and rain with possible sleet and freezing rain.
        #         Rain and drizzle with pockets of snow.

        rankList.sort(self.rankedSortOrder)
        length = len(rankList)
        words = ""
        index = 0
        # For non-simple phrasing, have we switched to the second list
        # using the conjunction yet?
        switchConjunction = 0
        # Begin by including coverage with weather value
        includeCovInten = 1
        # Handle "Likely" specially
        addLkly = 0
        subkeys = self.getSubkeys(rankList)
        useSimple = self.useSimple(tree, node, rankList, subkeys)
        prevCoverage = prevSubkey = prevRank = None
        prevConj = ""
            
        for index in range(len(rankList)):
            subkey, rank = rankList[index]
            # If not last one, determine nextCoverage
            if index < length-1:
                nextSubkey, nextRank = rankList[index+1]
            else:
                nextSubkey = None

            # Set so that value is included UNLESS re-set by one of the
            # sub-methods e.g. mixedWith, possiblyMixedWith, etc..
            self.includeValue = 1

            # Add conjunction for non-simple words
            if not useSimple:
                words, conj, switchConjunction, includeCovInten, addLkly = \
                       self.addWxConjunction(
                    tree, node, words, prevSubkey, prevRank, subkey, rank,
                    index, switchConjunction, includeCovInten, addLkly)
                                        
            # Get string for subkey checking previous and next coverage
            value, prevCoverage = self.weather_value(
                tree, node, subkey, prevCoverage, nextSubkey,
                includeCovInten=includeCovInten)
            if self.includeValue == 1:
                if value == "":
                    # If empty value string, remove the previous conjunction
                    # so we don't end up with something like "rain and"
                    words = self.removeLast(words, prevConj)
                else:
                    words = words + value

            # if last one, do not add conjunction
            if index == length - 1: break
            if useSimple:
                conj = self.wxConjunction(tree, node, subkey, rank, nextSubkey, nextRank)
                words = words + conj
                
            prevSubkey = subkey
            prevConj = conj
            prevRank = rank

        if addLkly:
            words = words + " likely"
        return words

    def addWxConjunction(self, tree, node, words, prevSubkey, prevRank, subkey, rank,
                         index, switchConjunction, includeCovInten, addLkly): 
        # Check to see if we can switch to "with" or "with pockets of"
        # OR just add the regular conjunction
        # NOTE:"mixed" weather will be implemented when the samplers can support it.
        conj = ""
        #print "\nin addWxConj", prevSubkey, prevRank, subkey, rank
        if not switchConjunction and index > 0:
            includeCovInten = 0
            similarCovs = self.similarCoverages(tree, node, prevSubkey, subkey)
            rankWordingFuzzFactor = self.rankWordingFuzzFactor(
                tree, node, prevSubkey, subkey)
            # If the current rank is significantly less than the previous one
            # or if the coverage is significantly different from the previous one
            #print "prevSubkey, subkey", prevSubkey, subkey
            #print "similarCovs", similarCovs
            if rank <= prevRank - rankWordingFuzzFactor or similarCovs == 0:
                if similarCovs == 1 or similarCovs == 0: 
                        # Prev subkey is dominant either by coverage or rank
                        method = self.withPossible
                else: # similarCovs == 2 i.e. Current subkey is dominant
                        method = self.withPocketsOf
                switchConjunction = 1
                conj =  method(tree, node, prevSubkey, prevRank, subkey, rank)
        if conj == "":
            if index > 0:
                if prevSubkey.coverage() == "Lkly":
                    addLkly = 1
                conj = self.wxConjunction(tree, node, prevSubkey, prevRank,
                                      subkey, rank)
        words = words + conj
        #print "returning", words+conj
        return words, conj, switchConjunction, includeCovInten, addLkly
    

    def weather_value(self, tree, node, subkey, prevCoverage=None,
                      nextSubkey=None, typeOnly=0,
                      includeCovInten=1):
        "Return a phrase for the WeatherSubkey"

        # If the prevCoverage is the same, then do not repeat it.
        #  e.g. "Widespread rain and snow" instead of
        #       "Widespread rain and widespread snow"
        # "Likely" is a special case because it follows the nouns,
        #  so we need to look at the nextCoverage to get:
        #  e.g. "Rain and snow likely" instead of
        #       "Rain likely and snow likely"
        # If typeOnly is set to one, only the Type phrase is returned.

        wxDef = subkey.wxDef()
        wxType = subkey.wxType()
        if wxType == '<NoWx>':
            wxType = ""
        else:
            wxType = wxDef.typeDesc(wxType).lower()

        inten = subkey.intensity()
        if inten == '<NoInten>':
            inten = ""
        else:
            inten = wxDef.intensityDesc(subkey.wxType(), inten).lower()

        if inten.find("moderate") != -1:
            inten = "" 

        attrList = subkey.attributes()
        attrList = self.removeDups(attrList)
        attrTextList = []
        for attr in attrList:
            # Ignore non-text attributes
            if attr == "MX" or attr == "OR" or attr == "Primary" or attr == "Mention":
                continue
            attrDesc = wxDef.attributeDesc(subkey.wxType(), attr).lower()
            # Use the wxAttributeDescriptors if provided
            attrDescs = self.call(self.wxAttributeDescriptors, tree, node)
            for des_cov, des_type, des_inten, des_attr, desc in attrDescs:
                if self.matchSubkey(subkey, des_cov, des_type, des_inten, des_attr, [attr]) == 1:
                    attrDesc = self.getWxDesc(tree, node, subkey, desc)           
            if attrDesc != "":
                attrTextList.append(attrDesc)

        # Determine coverage.  Check for repetition.
        covDescs = self.call(self.wxCoverageDescriptors, tree, node)
        cov = self.getCoverage(tree, node, subkey, covDescs, wxDef, attrList)
        # Make a copy of this coverage for later use
        copyCov = cov
        #print "cov, prev", cov, prevCoverage
        if cov == prevCoverage and not prevCoverage == "likely":
            cov = ""
        elif cov == "likely":
            nextCoverage = self.getCoverage(
                tree, node, nextSubkey, covDescs, wxDef, attrList)
            if cov == nextCoverage:
                cov = ""
        #print "result", cov

        # Use wxTypeDescriptors and wxIntensityDescriptors if provided
        typeDescs = self.call(self.wxTypeDescriptors, tree, node)
        intenDescs = self.call(self.wxIntensityDescriptors, tree, node)                             

        for des_cov, des_type, des_inten, des_attr, desc in typeDescs:
            if self.matchSubkey(subkey, des_cov, des_type, des_inten,des_attr, attrList) == 1:
                wxType = self.getWxDesc(tree, node, subkey, desc)
        for des_cov, des_type, des_inten, des_attr, desc in intenDescs:
            if self.matchSubkey(subkey, des_cov, des_type, des_inten,des_attr, attrList) == 1:
                inten = self.getWxDesc(tree, node, subkey, desc)
        
        # Handle special cases and clean up
        if cov == "definite":
            cov = ""

        # Hail -- "Large Hail" and "Small Hail" attributes
        # get converted to adjectives instead of attributes.
        if wxType == "hail":
            hailAttr = None
            if "large hail" in attrTextList:
                hailAttr = "large hail"
                adj = "large"
            if "small hail" in attrTextList:
                hailAttr = "small hail"
                adj = "small"
            if hailAttr is not None:
                wxType = adj + " hail"
                newAttrs = []
                for attr in attrTextList:
                    if attr != hailAttr:
                        newAttrs.append(attr)
                attrTextList = newAttrs

        # Arrange the order of the words
        if typeOnly == 1:
            return wxType, cov

        if includeCovInten == 0:
            cov = ""
            inten = ""
            
        word1 = cov
        word2 = inten
        word3 = wxType

        # Handle special case of "likely"
        if cov == "likely":
            word1 = inten
            word2 = wxType
            word3 = cov

        # Put coverage, intensity and wxType together
        if word2 == "":
            phrase = word1 + " " + word3
        else:
            phrase = word1 + " " + word2 + " " + word3
        phrase = phrase.strip()

        # Add attributes
        phrase = self.addTextList(phrase, attrTextList, " with ", " and ")
        phrase = phrase.replace("with in", "in")

        if cov == "":
            cov = copyCov
        return phrase, cov    

    def getIndex(self, hierarchy, value): 
        list = self.wxHierarchies()[hierarchy]
        return list.index(value)

    def getSubkeys(self, rankList):
        subkeys = []
        if rankList is None:
            return subkeys
        for subkey, rank in rankList:
            if subkey.wxType() == "<NoWx>":
                continue
            subkeys.append(subkey)
        return subkeys

    def checkVisibility(self, tree, node, subkeys):
        ##    If no visibility threshold is set, produce weather words as normal.
        ##    If a visibility threshold is set:
        ##      --If there are significant weather keys in the weather grid,
        ##        produce the weather words regardless of the visibility.
        ##      --If there is no visibility specified in the grids,
        ##        produce the weather words IF there are significant
        ##        weather keys in the grids.
        ##      --If there is a visibility specified in the grids, check to see
        ##        if it is less than the visibility threshold.
        ##        If it is, produce the weather words.
        ##      --If there is a visibility specified in the grids and it is
        ##        greater or equal to threshold and there are no significant weather
        ##        keys, produce "null" weather words.
        visThreshold = self.visibility_wx_threshold(tree, node)
        if visThreshold is not None:
            produceWords = 0
            # Check for significant keys
            significantKeys = self.significant_wx_visibility_subkeys(tree, node)
            sigFlag = self.findSubkeys(subkeys, significantKeys)
            # If significant weather keys, produce words regardless of vis
            if sigFlag:
                produceWords = 1
            else: # sigFlag is 0
                lowVisNM = self.getVis(subkeys)
                # If lowVisNM is None, we will not produce words
                if lowVisNM is not None:
                    # Produce words only if lowVisNM < visThreshold
                    if lowVisNM < visThreshold:
                        produceWords = 1
            if not produceWords:
                return 1
        return 0

    def addEmbeddedVisibility(self, tree, node, subkeys, words):
        # Add embedded visibility wording
        if self.embedded_visibility_flag(tree, node):
            # Check for visibility having consolidated to separate phrase
            visFlag = node.parent.get("reportVisibility")
            if visFlag != 0:
                # Find low visibility for subkeys
                lowVisNM = self.getVis(subkeys)
                if lowVisNM is not None:
                    lowVisNM = self.roundStatistic(tree, node, lowVisNM, "Visibility") 
                    # If below null_nlValue, report it
                    visThreshold = self.nlValue(self.null_nlValue(
                        tree, node, "Visibility", "Visibility"), lowVisNM)
                    if lowVisNM < visThreshold:
                        visWords = self.nlValue(
                            self.visibility_weather_phrase_nlValue(tree, node), lowVisNM)
                        significantKeys = self.significant_wx_visibility_subkeys(tree, node)
                        sigFlag = self.findSubkeys(subkeys, significantKeys)
                        #  If the weather words are not null and this is a Wx
                        #  obstruction for which we should report vsby
                        if words != "": #  and sigFlag != 0: DR_18894 change
                            visWords = " with " + visWords
                        words = words + visWords
        return words    
          
    def separateNonPrecip_threshold(self, tree, node):
        # Number of sub-phrases required to separate precip from
        # non-precip
        return 1
    
    def separateNonPrecip(self, tree, node):
        # See if ready to process
        if not self.phrase_trigger(tree, node, setUpOnly=1):
            return
        # If > designated subPhrases, separate into precip/non-precip
        statList = self.getSubStats(node, "Wx")
        length = len(statList)
        if self.__dict__.get('_leDebug', 0):
            print "\n\nseparateNonPrecip disabled", node, length, \
                      node.getAncestor("disabledSubkeys")
            print "   timerange", node.getTimeRange()
            print "   statList", statList
            #print "   doneList", node.doneList
        if length >= self.separateNonPrecip_threshold(tree, node):
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
            if self.__dict__.get('_leDebug', 0): print "precip, nonPrecip", precip, nonPrecip
            if len(precip) >= 1 and len(nonPrecip) >= 1:
                self.splitWxPhrase(tree, node, precip, nonPrecip, [self.separateNonPrecip])
        return self.DONE()
            
    def consolidateVisibility(self, tree, node):
        # If visibility is constant throughout subphrases and non-null,
        # separate out into its own phrase
        # See if ready to process
        if not self.phrase_trigger(tree, node, setUpOnly=1):
            return
        subPhrases = node.get("childList")
        if len(subPhrases) <= 1:
            return self.DONE()
        lowVis = None
        firstTime = 1
        #print "\nconsolidating"
        for subPhrase in node.childList:
            statDict = subPhrase.getStatDict()
            rankList = self.getStats(statDict, "Wx")
            subkeys = self.getSubkeys(rankList)
            subLowVis = self.getVis(subkeys)
            #print "low vis", subLowVis, subPhrase.getTimeRange()
            if firstTime:
                lowVis = subLowVis
                firstTime = 0
            elif subLowVis != lowVis:
                # Visibility for this subPhrase differs from previous
                # so we can't consolidate
                return self.DONE()
        if lowVis is None:
            return self.DONE()
        # Check to see if lowVis is non-null
        visThreshold = self.nlValue(self.null_nlValue(
            tree, node, "Visibility", "Visibility"), lowVis)
        if lowVis <= visThreshold:
            # Need to report as separate phrase
            newPhrase = tree.addPhraseDef(node, self.visibility_phrase)
            # Turn off visibility reporting for this weather phrase
            node.set("reportVisibility", 0)
        return self.DONE()

    def consolidateWx(self, tree, node):         
        # If any wxTypes span all subPhrases, separate into their own phrase
        statList = self.getSubStats(node, "Wx")
        length = len(statList)
        subkeyDict = {}
        if self.__dict__.get('leDebug', 0):
            print "\n\nConsolidating disabled", node.getAncestor("disabledSubkeys")
            print "   timerange", node.getTimeRange()
            print "   statList", statList
            #print "   doneList", node.doneList
        if length > 1:
            # Count occurrences of each weather key
            for subkeys in statList:
                for subkey in subkeys:
                    if subkey not in subkeyDict.keys():
                        subkeyDict[subkey] = 1
                    else:
                        subkeyDict[subkey] += 1
            # Find subkeys to disable in first phrase and second phrase, respectively
            list1 = []
            list2 = []
            for subkey in subkeyDict.keys():
                count = subkeyDict[subkey]
                if count >= length:
                    list2.append(subkey)
                else:
                    list1.append(subkey)
            if self.__dict__.get('_leDebug', 0): print "list1, list2", list1, list2
            if len(list1) > 0 and len(list2) > 0:
                self.splitWxPhrase(
                    tree, node, list1, list2,
                    [self.consolidateWx, self.separateNonPrecip,
                     self.skyPopWx_consolidateWx])
        return self.DONE()

    def subPhrase_limit(self, tree, node):
        # If the number of sub-phrases is greater than this limit, the weather
        # phrase will use 6-hour instead of the higher resolution to produce:
        #
        #    OCCASIONAL SNOW POSSIBLY MIXED WITH SLEET AND FREEZING
        #    DRIZZLE IN THE MORNING...THEN A CHANCE OF RAIN POSSIBLY MIXED WITH SNOW
        #    AND SLEET AND FREEZING DRIZZLE IN THE AFTERNOON. 
        #
        # instead of:
        #    OCCASIONAL SNOW IN THE MORNING. CHANCE OF LIGHT SLEET AND
        #    SLIGHT CHANCE OF LIGHT FREEZING DRIZZLE IN THE LATE MORNING AND
        #    EARLY AFTERNOON. CHANCE OF SNOW EARLY IN THE AFTERNOON. CHANCE OF
        #    RAIN IN THE AFTERNOON.
        return 3
    
                 
    def checkResolution(self, tree, node):
        # Check to see if there are too many sub-phrases and we need to re-do the
        # phrase in lower resolution. The limit is determined by "subPhrase_limit".
        # This currently assumes we have a 3 or greater resolution and want to go to
        # a 6-hour resolution.
        
        # See if ready to process
        if not self.phrase_trigger(tree, node):
            return
        
        # Count the number of non-empty phrases
        #print "\n In check resolution", node
        count = 0
        for subPhrase in node.get("childList"):
            words = subPhrase.get("words")
            if words == "":
                continue
            #print "words", subPhrase, words
            count += 1
        if count > self.subPhrase_limit(tree, node):
            #print "count", count
            # Create a new node in it's place with a new
            # resolution set
            exec "newPhraseDef = self." + node.getAncestor('name')
            newPhrase = tree.addPhraseDef(node, newPhraseDef)
            newPhrase.set("disabledSubkeys", node.get("disabledSubkeys"))
            curResolution = node.get("resolution")
            if curResolution is not None:
                # If we have already re-set the resolution and we are still over the
                # sub-phrase limit, we'll have to decrease the resolution some more
                # to try and reduce the number of sub-phrases.
                # This is necessary because of the way preProcessWx works:
                # For example, even if we have only 2 time periods sampled,
                # they can result in 3 or more sub-phrases depending on the
                # complexity of weather.
                # Example:  Hours 1-6  Chc RW Chc L
                #           Hours 7-12 Chc SW Chc L
                #   Results in 3 sub-phrases
                #           Hours 1-12 Chc L
                #           Hours 1-6  Chc RW
                #           Hours 7-12 Chc SW
                newResolution = curResolution * 2
            else:
                newResolution = 6
            newPhrase.set("resolution", newResolution)
            for key in ["spawnedWxPhrases", "conjunctiveQualifier",
                        "embeddedQualifier", "localEffect", "localEffectsList",
                        "firstElement", "elementName"]:
                newPhrase.set(key, node.get(key))
            #print "making newPhrase", newPhrase
            #print "parent should be", node.parent
            #tree.printNode(newPhrase)
            # Remove this node
            node.remove()
        return self.DONE()

    def severeWeather_phrase(self):
        return {
            "setUpMethod": self.severeWeather_setUp,
            "wordMethod": self.severeWeather_words,
            "phraseMethods": [
                   self.preProcessWx,
                   self.combineWords,
                   self.fillNulls,
                   self.timeDescriptorModeration,
                   self.assembleSubPhrases,
                   self.postProcessPhrase,
                   ],
            }    

    def severeWeather_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("Wx", "List", self.WEATHER())]
        self.subPhraseSetUp(tree, node, elementInfoList, self.wxConnector)
        # Set this flag used by the "checkWeatherSimilarity" method
        node.set("noIntensityCombining", 1)
        self.determineSevereTimeDescriptors(tree, node)
        return self.DONE()

    def determineSevereTimeDescriptors(self, tree, node):
        wxStats = tree.stats.get("Wx", node.getTimeRange(), node.getAreaLabel())
        thunderThru = 1  # T throughout the time period
        severeThru = 1   # T+ throughout the time period
        allThunderSevere = 1  # All T that appears in the period is +
        if wxStats is not None:
            for subkeys, tr in wxStats:
                thunderFound = 0
                severeFound = 0
                for subkey, rank in subkeys:
                    #print "   subkey", subkey
                    if subkey.wxType() == "T":
                        thunderFound = 1
                        if subkey.intensity() == "+":
                            severeFound = 1
                        else:
                            allThunderSevere = 0
                if not severeFound:
                    severeThru = 0
                if not thunderFound:
                    thunderThru = 0                    
        #print "thunderThru, severeThru, allThunderSevere", thunderThru, severeThru, allThunderSevere
        if thunderThru == 1:
            if severeThru == 1:
                noTD = 1
            else:
                noTD = 0
        elif allThunderSevere == 1:
            noTD = 1
        else:
            noTD = 0
        #print "noTD", noTD
        if noTD:
            node.set("noTimeDescriptors", 1)
            #print "setting", node
        return

    def severeWeather_words(self, tree, node):
        "If T +, produce phrase.  Report attributes of T."
        # Wx Statistics: rankedWx

        statDict = node.getStatDict()
        rankList = self.getStats(statDict, "Wx")
        if rankList is None or len(rankList) == 0:
            return self.setWords(node, "")
        # Check against PoP
        rankList = self.checkPoP(tree, node, rankList)
        subkeyList = self.getSubkeys(rankList)

        severe = 0
        thunder = 0
        attrTextList = []
        for subkey in subkeyList:
            wxType = subkey.wxType()
            if wxType == "T":
                thunder = 1
                intensity = subkey.intensity()
                if intensity == "+":
                    severe = 1
                wxDef = subkey.wxDef()
                for attr in subkey.attributes():
                    if attr in ["Primary", "Mention", "Dry"]:
                        continue
                    attrText = wxDef.attributeDesc(subkey.wxType(), attr).lower()
                    if attrText not in attrTextList:
                        attrTextList.append(attrText)
                        
        if thunder == 0:
            return self.setWords(node, "")
        if severe == 0 and attrTextList == []:
            return self.setWords(node, "")

        # Add attributes to phrase 
        if severe == 0:
            words = self.phrase_descriptor(tree, node, "thunderstorms", "Wx")
            words = self.addTextList(words, attrTextList, " ", " and ")
        else:
            words = self.phrase_descriptor(tree, node, "severeWeather", "Wx")
            words = self.addTextList(words, attrTextList, " with ", " and ")
                       
        return self.setWords(node, words)

    def heavyRainTypes(self, tree, node):
        # Rain weather types that will trigger the heavyPrecip_phrase
        return ["R", "RW"]

    def heavySnowTypes(self, tree, node):
        # Snow weather types that will trigger the heavyPrecip_phrase
        return ["S", "SW"]

    def heavyOtherTypes(self, tree, node):
        # Weather types other than snow that will trigger the heavyPrecip_phrase
        # R and RW are automatic triggers
        return ["IP", "ZR", "L", "ZL"]

    def heavyPrecip_phrase(self):
        ### NEW METHOD by Tom Spriggs/Steve Nelson/Tracy Hansen
        ### ZFP_Local
        return {
            "setUpMethod": self.heavyPrecip_setUp,
            "wordMethod": self.heavyPrecip_words,
            "phraseMethods": [
                   self.combineHeavyPrecip,
                   self.combineWords,
                   self.fillNulls,
                   self.timeDescriptorModeration,
                   self.assembleSubPhrases,
                   self.postProcessPhrase,
                   ],
            }    
    def heavyPrecip_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("Wx", "List", self.WEATHER())]
        self.subPhraseSetUp(tree, node, elementInfoList, self.wxConnector)
        # Set this flag used by the "checkWeatherSimilarity" method
        node.set("noIntensityCombining", 1)
        return self.DONE()
    
    def combineHeavyPrecip(self, tree, phrase):
        # See if ready to process
        if not self.phrase_trigger(tree, phrase, setUpOnly=1):
            return
        return self.combineChildren(tree, phrase, self.combineHeavy)
 
    def combineHeavy(self, tree, phrase, subPhrase1, subPhrase2):
        ### NEW METHOD TO prevent reporting redundant phrases in complex wx
        
        # If there is heavy precip in both subPhrase1 and subPhrase2, combine
        statDict1 = subPhrase1.getStatDict()
        stats1 = statDict1["Wx"]
        statDict2 = subPhrase2.getStatDict()
        stats2 = statDict2["Wx"]

        if stats1 is None and stats2 is None:
            return 1, None
        if stats1 is None or stats2 is None:
            return 0, None

        newStats = []
        heavy = [0,0]
        index = 0
        for wxStats in [stats1, stats2]:
            for subkey, rank in wxStats:
                wxType = subkey.wxType()
                if subkey.intensity() == "+":
                    if wxType in self.heavyRainTypes(tree, phrase) or \
                       wxType in self.heavySnowTypes(tree, phrase) or \
                       wxType in self.heavyOtherTypes(tree, phrase):
                        heavy[index] = 1
                newStats.append((subkey, rank))
            index += 1
        if heavy[0] and heavy[1]:
            elementInfoList = phrase.get("elementInfoList")
            newSubPhrase = self.combine2SubPhrases(tree, phrase, subPhrase1, subPhrase2,
                                                   elementInfoList, newStats)
            return 1, newSubPhrase
        else:
            return 0, None

    def heavyPrecip_words(self, tree, node):
        ### WxPhrases
        self._heavyPrecipFlag = 0
        self._rainfallFlag = 0
        self._rainFlag = 0
        self._snowFlag = 0
        self._otherFlag = 0
        statDict = node.getStatDict()
        rankList = self.getStats(statDict, "Wx")
        if rankList is None or len(rankList) == 0:
            return self.setWords(node, "")
        # Check against PoP
        rankList = self.checkPoP(tree, node, rankList)

        subkeyList = self.getSubkeys(rankList)

        checkSnowTypes = self.heavySnowTypes(tree, node) 
        checkOtherTypes = self.heavyOtherTypes(tree, node)
        
        words = ""
        for subkey in subkeyList:
            wxType = subkey.wxType()
            intensity = subkey.intensity()

            if intensity == "+":
                for type in checkOtherTypes:
                    if wxType == type:
                        self._heavyPrecipFlag = 1
                        self._otherFlag = 1

                for type in checkSnowTypes:
                    if wxType == type:
                        self._heavyPrecipFlag = 1
                        self._snowFlag = 1

                if wxType == "RW":
                    self._heavyPrecipFlag = 1
                    self._rainfallFlag = 1
                if wxType == "R":
                    self._heavyPrecipFlag = 1
                    self._rainFlag = 1

        if self._heavyPrecipFlag == 1:
            if self._otherFlag == 1:
                words = self.phrase_descriptor(tree, node, "heavyPrecip", "Wx")
            elif self._snowFlag == 1 and self._rainFlag == 0 and self._rainfallFlag == 0:
                words = self.phrase_descriptor(tree, node, "heavySnow", "Wx")
            elif self._snowFlag == 0 and self._rainFlag == 1 and self._rainfallFlag == 0:
                words = self.phrase_descriptor(tree, node, "heavyRain", "Wx")
            elif self._snowFlag == 0 and self._rainFlag == 0 and self._rainfallFlag == 1:
                words = self.phrase_descriptor(tree, node, "heavyRainfall", "Wx")
            else:
                words = self.phrase_descriptor(tree, node, "heavyPrecip", "Wx")

        return self.setWords(node, words)

    def filterSubkeys(self, tree, node, rankList):
        # Filter subkeys in rankList:
        #   Combine using wxCombinations
        
        if self.filter_subkeys_flag() == 0:
            return rankList
        #print "rankList in filter", rankList
        if rankList is None:
            return rankList
        if len(rankList) == 0:
            return rankList
        rankList, convertedFlag = self.convertToRankList(rankList)
        rankList = self.combineSubKeys(tree, node, rankList)
        if convertedFlag:
            rankList = self.convertFromRankList(rankList)
        return rankList

    def convertToRankList(self, rankList):
        # If the list is a simple list of subkeys,
        # add a dummy rank to each entry
        # and return convertedFlag = 1
        if rankList == []:
            return rankList, 0
        entry = rankList[0]
        if type(entry) is not types.TupleType:
            newList = []
            for subkey in rankList:
                newList.append((subkey,0))
            rankList = newList
            convertedFlag = 1
        else:
            convertedFlag = 0
        return rankList, convertedFlag

    def convertFromRankList(self, rankList):
        # Strip the dummy ranks off the rankList
        newList = []
        for subkey, rank in rankList:
            newList.append(subkey)
        return newList

    def combineSubKeys(self, tree, node, rankList):
        # Compare subkeys and condense if appropriate
        rankList, convertedFlag = self.convertToRankList(rankList)
        done = 0
        while done == 0:
            combinedKey, combinedRank, index1, index2 = self.combineKeys(tree, node, rankList)
            #print "combinedKey", combinedKey, index1, index2
            # If no more combinations possible, we are done
            if combinedKey is None:
                done = 1
            else:
                # Make a new list:
                # Set index1 to combinedKey
                # Delete index2
                newList = []
                ind = 0
                length = len(rankList)
                for subkey, rank in rankList:
                    if ind > length-1:
                        break
                    if ind == index1:
                        newList.append((combinedKey, combinedRank))
                    elif ind != index2:
                        newList.append((subkey, rank))
                    ind = ind + 1
                rankList = newList
        #print "Leaving combineSubKeys", rankList
        if convertedFlag:
            rankList = self.convertFromRankList(rankList)
        return rankList
    
    def combineKeys(self, tree, node, rankList):
        # See if any keys can be combined
        # Return when the first combination is found
        length = len(rankList)
        if length <= 1:
            return None, None, 0, 0
        for index1 in range(0, length):
            # March down remaining list, trying to combine
            for index2 in range(index1 + 1 , length):
                combinedKey, combinedRank = self.combineKey1Key2(tree, node, rankList[index1],
                                                   rankList[index2])
                if combinedKey is not None:
                    return combinedKey, combinedRank, index1, index2
        return None, None, 0, 0

    def combineKey1Key2(self, tree, node, entry1, entry2):
        # Combine duplicates, "near duplicates", and user-defined
        # combinations from wxCombinations
        subkey1, rank1 = entry1
        subkey2, rank2 = entry2
        wxType1 = subkey1.wxType()
        wxType2 = subkey2.wxType()
        cov1 = subkey1.coverage()
        inten1 = subkey1.intensity()
        cov2 = subkey2.coverage()
        inten2 = subkey2.intensity()

        combinedKey = None
        combinedRank = max(rank1, rank2)
        if subkey1 == subkey2:
            combinedKey = subkey1
        elif wxType1 == wxType2:
            # In this case, we must make an aggregate
            combinedKey = self.makeAggregateSubkey(subkey1, rank1, subkey2, rank2)     
        else:
            # Try to combine using configurable wxCombinations
            # May need to pick lowVis, preserve attrs here too
            combinations = self.call(self.wxCombinations, tree, node)
            for combination in combinations:
                match, combinedKey = self.matchWxCombination(
                    combination, subkey1, subkey2)
                if match:
                    break
        return combinedKey, combinedRank
                        
    def matchWxCombination(self, combination, subkey1, subkey2):
        # Given a combination i.e. pair (wx1, wx2) or method,
        #   determine if subkey1 and subkey2 should be combined.
        # Return flag (match) and combinedKey
        keyList = [(subkey1, subkey2), (subkey2, subkey1)]
        match = 0
        combinedKey = None
        for key1, key2 in keyList:
            if type(combination) is types.MethodType:
                match, combinedKey = combination(key1, key2)
                if match:
                    break
            else:
                wx1, wx2 = combination
                if wx1 == key1.wxType() and wx2 == key2.wxType():
                    order = self.dominantCoverageOrder(key1, key2)
                    if order == -1 or order == 0:
                        combinedKey = key1
                    else:
                        combinedKey = key2
                    match = 1
                    break
        return match, combinedKey
                
    def dominantCoverageOrder(self, val1, val2):
        # Order by dominant coverage -- lower indices are dominant
        # If val1 coverage is dominant over val2 coverage,
        #   return -1, if equal return 0, else return 1
        val1 = val1.coverage()
        val2 = val2.coverage()
        list = self.wxHierarchies()["coverage"]
        try:
            index1 = list.index(val1)
        except:
            index1 = len(list)-1
        try:
            index2 = list.index(val2)
        except:
            index2 = len(list)-1
        if index1 < index2:
            return -1
        if index1 == index2:
            return 0
        if index1 > index2:
            return 1

    def dominantTypeOrder(self, val1, val2):
        # If val1 wxType is dominant over val2 wxType
        #   Lower indices are dominant
        #   return -1, if equal return 0, else return 1
        val1 = val1.wxType()
        val2 = val2.wxType()
        list = self.wxHierarchies()["wxType"]
        try:
            index1 = list.index(val1)
        except:
            index1 = len(list)-1
        try:
            index2 = list.index(val2)
        except:
            index2 = len(list)-1        
        if index1 < index2:
            return -1
        if index1 == index2:
            return 0
        if index1 > index2:
            return 1
        
    def getDominant(self, hierarchy, val1, val2):
        # Return the value that appears first in the given hierarchy
        list = self.wxHierarchies()[hierarchy]

        index1 = list.index(val1)
        index2 = list.index(val2)
        if index1 < index2:
            return val1
        else:
            return val2


    def getCoverage(self, tree, node, subkey, covDescs, wxDef, attrList):
        if subkey is None:
            return None
        cov = subkey.coverage()
        if cov == "<NoCov>":
            return ""
        cov = wxDef.coverageDesc(subkey.wxType(), cov).lower()
        for des_cov, des_type, des_inten, des_attr, desc in covDescs:
            if self.matchSubkey(subkey, des_cov, des_type, des_inten, des_attr, attrList) == 1:
                cov = self.getWxDesc(tree, node, subkey, desc)
        return cov
        
    def matchSubkey(self, subkey, cov, wxType, inten, attr, attrList):
        if cov != "*":
            if subkey.coverage() != cov:
                return 0
        if wxType != "*":
            if subkey.wxType() != wxType:
                return 0
        if inten != "*":
            if subkey.intensity() != inten:
                return 0
        if attr != "*":
            if attr not in attrList:
                return 0
        return 1

    def getWxDesc(self, tree, node, subkey, desc):
        if type(desc) is types.MethodType:
            return desc(tree, node, subkey)
        else:
            return desc

    # VISIBILITY
    def visibility_phrase(self):
        return {
            "setUpMethod": self.visibility_setUp,
            "wordMethod": self.visibility_words,
            "phraseMethods": [
                  #self.preProcessWx,
                  self.combinePhraseStats,
                  self.combineWords,
                  self.fillNulls,
                  self.timeDescriptorModeration,
                  self.assembleSubPhrases,
                  self.postProcessPhrase,
                  ],            
            }    
    def visibility_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("Wx", "List", self.WEATHER())]
        self.subPhraseSetUp(tree, node, elementInfoList, self.visConnector)
        node.set("combineVisibility", 1)
        descriptor = self.phrase_descriptor(tree, node, "Visibility", "Visibility")
        node.set("descriptor", descriptor)
        return self.DONE()
    
    def visibility_words(self, tree, node):
        # Return a phrase for the given subPhrase
        
        # Create a phrase to describe a list of weather subkeys for one sub-period
        statDict = node.getStatDict()
        rankList = self.getStats(statDict, "Wx")
        if rankList is None or len(rankList) == 0:
            return self.setWords(node, "")
        # Filter rankList so we don't report visibility for weather subkeys
        # not reported in the text (e.g. SChc, Iso)
        rankList = self.checkPoP(tree, node, rankList)
        subkeyList = self.getSubkeys(rankList)

        lowVisNM = self.getVis(subkeyList)
        if lowVisNM is None:
            return self.setWords(node, "null")
        # If less than null_nlValue (in nautical miles) return "null"
        nullVisNM = self.null_nlValue(tree, node, "Visibility", "Visibility")
        lowVisNM = self.roundStatistic(tree, node, lowVisNM, "Visibility") 
        if lowVisNM >= self.nlValue(nullVisNM, lowVisNM):
            return self.setWords(node, "null")
        words = self.nlValue(self.visibility_phrase_nlValue(tree, node), lowVisNM)
        #  See if the Wx type is significant 
        significantKeys = self.significant_wx_visibility_subkeys(tree, node)
        sigFlag = self.findSubkeys(subkeyList, significantKeys)
        #  If there are no Wx obstructions for which we should report vsby
        if sigFlag == 0:
            words = ""
        return self.setWords(node, words)

    def visibility_phrase_nlValue(self, tree, node):
        # Visibility descriptions for visibility_phrase.
        #   "Visibility less than 1 nautical mile then 2 NM in the afternoon."
        # The numerical ranges are in nautical miles.
        outUnits = self.element_outUnits(tree, node, "Visibility", "Visibility")
        if outUnits == "NM":
            return {
                   (0, 1):   "1 NM or less",
                   (1.1, 2): "2 NM",              
                   (2.1, 3): "3 NM",              
                   (3.1, 4): "4 NM",              
                   (4,1, 5): "5 NM",              
                   (5.1, 6): "6 NM",
                   "default": "null",
                   }
        else:
            return {
                   (0, .3):   "one quarter mile or less at times",
                   "default": "null",
                   }
                    
    # Handling visibility within the weather phrase
    def embedded_visibility_flag(self, tree, node):
        # If 1, report visibility embedded with the
        # weather phrase. Set this to 0 if you are using the
        # visibility_phrase.
        return 0
    
    def visibility_wx_threshold(self, tree, node):
        # Weather will be reported if the visibility is below
        # this threshold (in NM) OR if it includes a
        # significant_wx_visibility_subkey (see below)
        return None

    def significant_wx_visibility_subkeys(self, tree, node):
        # Weather values that constitute significant weather to
        # be reported regardless of visibility.
        # If your visibility_wx_threshold is None, you do not need
        # to set up these subkeys since weather will always be
        # reported.
        # Set of weather key search tuples in the form:
        #  (cov type inten)
        # Wildcards are permitted.
        return [("* F "), ("* ZF "), ("* IF "), ("* H"), ("* K"), ("* BS"), ("* BD"), ("* VA")]
         
    def visibility_weather_phrase_nlValue(self, tree, node):
        # Visibility descriptions within the weather_phrase
        #   "Rain showers and fog with visibility less than 1 nautical mile in the morning."
        # The numerical ranges are in nautical miles.
        outUnits = self.element_outUnits(tree, node, "Visibility", "Visibility")
        if outUnits == "NM":
            return {
                   (0, 1):     "visibility 1 NM or less",
                   (1, 2.1):   "2 NM visibility",              
                   (2.1, 3.1): "3 NM visibility",              
                   (3.1, 4.1): "4 NM visibility",              
                   (4.1, 5.1): "5 NM visibility",              
                   (5.1, 6.1): "6 NM visibility",
                   "default": "null",
                   }
        else:
            return {
                   (0, .3):   "visibility one quarter mile or less at times",
                   "default": "null",
                   }


    def matchToWx(self, tree, node, element, timeRange=None, areaLabel=None,
                  algorithm=None, increment=None):
        if timeRange is None:
            timeRange = node.getTimeRange()
        if areaLabel is None:
            areaLabel = node.getAreaLabel()
        algorithm, increment, noPrecipValue, percentThreshold, wxTypes = \
                      self.getMatchToWxInfo(tree, node, element, algorithm, increment)
        #print "\nin matchToWx", element, timeRange, areaLabel
        #print "  ", algorithm, increment, noPrecipValue, wxTypes
        #print "  node", node.getAncestor("name")
        #import traceback
        #traceback.print_stack(limit=6)

        # Gather all data that might be necessary
        analysisMethodVal = tree.stats.get(
                element, timeRange, areaLabel, mergeMethod="Max")
        elementBins = tree.stats.get(
                    element, timeRange, areaLabel, statLabel="binnedPercent",
                    mergeMethod="MergeBins")

        # Compute result
        result = None
        if algorithm == "AnalysisMethod":
            #print "  Returning AnalysisMethod", elementVal
            result = analysisMethodVal
        else:        
            # Determine "highKey" -- key with the highest coverage
            highKey = self.getHighSubkey(tree, node, timeRange, areaLabel, wxTypes)
            #print "highKey", highKey
            # Handle case of no precipitating weather
            if highKey is None:
                result = self.getNoPrecipValue(
                    noPrecipValue, elementBins, analysisMethodVal)
            else:
                # Get the Element range of values corresponding to the
                # high key coverage.
                coverage = highKey.coverage()
                exec "elementRange = self.coverage"+element+"_value(coverage)"
                if type(elementRange) is types.MethodType:
                    covLowVal, covHighVal = elementRange(tree, node, highKey)
                else:
                    covLowVal, covHighVal = elementRange

                if algorithm == "Max" or algorithm == "Mode":
                    # Merge the binned values over space and time
                    if elementBins is None:
                        return None       
                    # Return the result
                    result = self.getBinnedResult(tree, node, elementBins, covLowVal,
                                                covHighVal, increment, algorithm,
                                                percentThreshold)
                elif algorithm == "MaxMode":
                    # Get the list of bins for each grid in the time range
                    elementBinsList = tree.stats.get(
                        element, timeRange, areaLabel, statLabel="binnedPercent",
                        mergeMethod="List")
                    if elementBinsList is None or elementBinsList == []:
                        result = None
                    else:
                        result = self.getMaxModeResult(
                            tree, node, elementBinsList, covLowVal, covHighVal,
                            increment, algorithm, percentThreshold)
                else:            
                    log.warning(
                        "WARNING -- Invalid matToWxInfo algorithm for " + element + \
                    " Must be 'Max', 'Mode', 'MaxMode', or 'AnalysisMethod'")   
        return result

    def getMatchToWxInfo(self, tree, node, element, algorithm, increment):
        matchingInfo = self.matchToWxInfo(tree, node, element, element)
        if matchingInfo == "":
            log.warning(
                "WARNING -- Cannot MatchToWx: Add " + element + \
                " to matchToWxInfo_dict!!")
            increment = 0
            algorithm = "Max"
            noPrecipValue = None
            percentThreshold = 0
            wxTypes = None
        else:
            # Get increment and algorithm if not set in arguments
            # Get other matchingInfo
            inc = matchingInfo[0]
            alg = matchingInfo[1]
            noPrecipValue = matchingInfo[2]
            try:
                percentThreshold = matchingInfo[3]
            except:
                percentThreshold = 0
            try:
                wxTypes = matchingInfo[4]
            except:
                wxTypes = None                
            if algorithm is None:
                algorithm = alg
            if increment is None:
                increment = inc
        return algorithm, increment, noPrecipValue, percentThreshold, wxTypes

    def getNoPrecipValue(self, noPrecipValue, elementBins, analysisMethodVal):
        if noPrecipValue is None:
            result = None
        elif noPrecipValue == "Max":
            # Return the maximum value with > 0% areal coverage
            if elementBins is None:
                result = None
            else:
                elementVal = None
                for lowVal, highVal, percent in elementBins:
                    if percent > 0:
                        elementVal = lowVal + increment
                result = elementVal
        elif noPrecipValue == "AnalysisMethod":
            result = analysisMethodVal
        else:
            result = noPrecipValue
        return result

    def getBinnedResult(self, tree, node, elementBins, binLowVal,
                       binHighVal, increment, algorithm, percentThreshold):
        # If algorithm == "Max":
        #   Return the MAXIMUM element value that falls within the element range 
        #   AND has a greater than zero percentage.  
        # Else: (Algorithm == "Mode")
        #   Return the MOST FREQUENT element value that falls within the element range 
        #   AND has a greater than zero percentage. 
        # We also calculate "resultValue" in case no element value falls within
        #   the range.
        # If the algorithm is "Max", resultValue will be the Maximum element value
        #   overall, otherwise, it will be the Most Frequent.
        elementVal = None
        resultValue = None
        # The Most Frequent value overall
        maxPercent = 0
        # The Most Frequent value within the lowVal/highVal range
        maxInRangePercent = 0
        #print "\nLooking for range", binLowVal, binHighVal
        for lowVal, highVal, percent in elementBins:
            #print "low, high, percent", lowVal, highVal, percent
            #print "   maxPercent, maxInRangePercent", maxPercent, maxInRangePercent
            #print "   elementVal, resultValue", elementVal, resultValue
            if percent > percentThreshold:
                # The element bins could be, for example, 55-65,
                # so we add the increment
                curValue = lowVal + increment
                if algorithm == "Max":
                    # Bins are in ascending order so the
                    # (Maximum) will just be the last one 
                    resultValue = curValue
                    if curValue >= binLowVal and curValue <= binHighVal:
                        elementVal = curValue
                else: # "Mode"
                    if percent > maxPercent:
                        resultValue = curValue
                        maxPercent = percent
                    if curValue >= binLowVal and curValue <= binHighVal:
                        if percent > maxInRangePercent:
                            elementVal = curValue
                            maxInRangePercent = percent
        #print "algorithm, elementVal, resultVal", algorithm, elementVal, resultValue
        # If no element meets this criteria
        #    If the resultValue value is greater than the binHighVal, use binHighVal
        #    Otherwise, use the resultValue value
        if elementVal is None:
            if resultValue > binHighVal:
                elementVal = binHighVal
            else:
                elementVal = resultValue
        return elementVal

    def getMaxModeResult(self, tree, node, elementBinsList, covLowVal,
                         covHighVal, increment, algorithm, percentThreshold):
        # For each grid, find the Mode i.e. highest percentage value
        # that falls within the coverage range
        valueList = []
        for elementBins in elementBinsList:
            elementBins, timeRange = elementBins
            elementVal = self.getBinnedResult(
                tree, node, elementBins, covLowVal, covHighVal,
                increment, "Mode", percentThreshold)
            valueList.append(elementVal)
        # Choose the maximum of these values that fall within the
        # range for high key coverage.
        #print "valueList", valueList
        maxVal = valueList[0]  # At least we'll have something ??
        for value in valueList:
            if value >= covLowVal and value <=covHighVal:
                if value > maxVal:
                    maxVal = value        
        return maxVal

    def getHighSubkey(self, tree, node, timeRange, areaLabel, wxTypes=None):
        # Find the highest precip subkey in the ranked list
        # If wxType is not None, consider only keys of that wxType
        highKey = None
        wxStats = tree.stats.get("Wx", timeRange, areaLabel)
        #print "\nWx in getHighSubkey", wxStats
        if wxStats is not None:
            highKey = None
            highCov = None
            for subkeys, tr in wxStats:
                #print "subkeys, rank", subkeys, tr
                if subkeys is None:
                    continue
                for subkey, rank in subkeys:
                    #print "subkey", subkey
                    if wxTypes is not None:
                        if subkey.wxType() not in wxTypes:
                            continue
                    else:
                        # Check for PoP-related
                        if self.pop_related_flag(tree, node, subkey):                        
                            #  Do not consider trace events for matching
                            if subkey.wxType() in ['L', 'ZL'] or \
                               (subkey.wxType() in ['RW', 'SW'] and
                                subkey.intensity() == '--'):
                                #  Move on to next subkey
                                continue
                        else:
                            # If non-precipitating, skip
                           continue
                    if highKey is None or \
                        self.dominantCoverageOrder(
                        subkey, highKey) == -1:
                        highKey = subkey
        return highKey
