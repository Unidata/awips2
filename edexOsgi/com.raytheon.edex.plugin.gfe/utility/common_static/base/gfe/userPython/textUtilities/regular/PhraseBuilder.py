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
# PhraseBuilder.py
# Methods for building phrases for Narrative products.
# 
# Author: hansen
# History
# Time          Ticket Number   Developer   Comments
# -----------------------------------------------------------------------------
# 12/28/2012    DR 15596        J.Zeng      Added checkWeatherSimilarity 
#                                           for two lists based on Virgil's 
#                                           suggestion
# ----------------------------------------------------------------------------

import types
import TimeDescriptor
import ConfigVariables
import StringUtils
import UnitConvertor
import TimeRange, WeatherSubKey

class PhraseBuilder(ConfigVariables.ConfigVariables,
                    StringUtils.StringUtils,
                    TimeDescriptor.TimeDescriptor,
                    UnitConvertor.UnitConvertor):

    def __init__(self):
        ConfigVariables.ConfigVariables.__init__(self)
        StringUtils.StringUtils.__init__(self)
        TimeDescriptor.TimeDescriptor.__init__(self)
        UnitConvertor.UnitConvertor.__init__(self)

    def SCALAR(self):
        return 0
    def MAGNITUDE(self):
        return 1
    def DIRECTION(self):
        return 2
    def VECTOR(self):
        return 3
    def VECTOR_TEXT(self):
        return 4
    def VECTOR_NUM(self):
        return 5
    def WEATHER(self):
        return 6
    def DISCRETE(self):
        return 7

    def DONE(self):
        return 1

    def call(self, method, tree, node):
       # Try to call with tree, node arguments
       try:
          return method(tree, node)
       except:
          return method()

    def setWords(self, node, words):
        node.set("words", words)
        # If the words are empty, we can close down node
        if words == "":
            #node.remove()
            node.doneList = node.methodList
        return 1

    def useEllipses(self, tree, node, words):
        # Instead of:
        # "Chance of rain and snow and freezing rain..."
        # use
        # "Chance of rain...snow and freezing rain..."
        if words.count(" and ") > 1:
            hiIndex = words.rfind(" and ")
            words = words[:hiIndex].replace(" and ", "...") + words[hiIndex:]
        return words

    def setDone(self, node):
        # Set the node doneList to the methodList
        # so that the node is defunct
        node.doneList = node.methodList

    def standard_phraseMethods(self):
        return [
            self.consolidatePhrase,
            self.checkLocalEffects,
            self.combinePhraseStats,
            self.consolidateTrends,
            self.chooseMostImportant,
            self.combineWords,
            self.fillNulls,
            self.timeDescriptorModeration,
            self.assembleSubPhrases,
            self.postProcessPhrase,
            ]

    def postProcessPhrase(self, tree, node):
        words = node.get("words")
        rval = None
        if words is not None:
            words =  words.replace("rain showers and thunderstorms", "showers and thunderstorms")
            # To handle snow amt local effects
            words = words.replace("except of", "except")
            # Translate phrase
            # This is necessary so that word-wrap works correctly
            try:
                words = self.translateForecast(words, self._language)
            except:
                words = self.translateForecast(words, "english")
            rval = self.setWords(node, words)
        return rval

    def roundStatistic(self, tree, node, value, elementName):
        roundingMethod = self.rounding_method(tree, node, elementName, elementName)
        nlIncrement = self.nlValue(self.increment_nlValue(
            tree, node, elementName, elementName), value)
        return self.roundValue(value, roundingMethod, "Nearest", nlIncrement)

    def findWords(self, tree, node, firstElementName, areaLabel=None, phraseList=None,
                  ignoreAreaIfLastChance=1, phraseLevel=0, attributes=None):
        # Return a text string which is the concatenation of the words for all
        # leaves of the tree in the same component as the given node
        # and have the given firstElement at their phrase level.
        # "Leaves" will be sub-phrases in most case unless a phrase node has
        # words set and has empty sub-phrases.
        # The method returns None unless all leaves that meet the qualifications
        # have words set.
        #
        # If an areaLabel is given, it is applied to the selection of leaves.
        # Otherwise all leaves meeting the firstElementName criteria are accepted.
        # "areaLabel" can optionally be a list of areaLabels.
        #
        # If a phraseList is given, only leaves belonging to a phrase
        # in that list are examined.
        # If phraseLevel is 1 and words are set at the phrase level, those
        # are concatenated to the words as well.
        #
        # if attributes is not None, return a dictionary:
        #     {attribute: [list of attribute values for leaves with words]}
        #

        leaves = self.getLeaves(tree, node)
        found = 0
        words = ""
        attrDict = {}
        #print "\nFind Words", firstElementName
        if areaLabel is not None:
            if type(areaLabel) is not types.ListType:
                areaLabel = [areaLabel]
        for child in leaves:
            firstElement = child.getAncestor("firstElement")
            if firstElement is None:
                continue
            if firstElement.name == firstElementName or firstElementName is None:
                if phraseList is not None:
                    if child.getAncestor("name") not in phraseList:
                        continue
                #print "firstElement", firstElement.name
                #print "child", child, child.get("childList")
                #print "words", child.get('words')

                # Check the area
                if ignoreAreaIfLastChance:
                    # If last time thru because no changes were made to tree,
                    #   ignore area
                    if tree.get("lastChance") == 1:
                        areaLabel = None
                if areaLabel is not None:
                    if child.getAreaLabel() not in areaLabel:
                        continue

                # This is a phrase for which we want words
                childWords = child.get('words')
                if childWords is not None:
                    found = 1
                    #print "Adding words", child.get('name'), child.getAreaLabel()
                    #print "       ", child.getTimeRange()
                    #print "       ", childWords
                    words = words + " " + childWords
                    if attributes is not None:
                        for attribute in attributes:
                            attrVal = child.getAncestor(attribute)
                            if attrVal is not None:
                                self.addToDictionary(attrDict, attribute, attrVal)
                    if phraseLevel:
                        parentWords = child.parent.get('words')
                        if parentWords is not None:
                            words = words + " " + parentWords
                else:
                    # wait for words to complete
                    words = None
                    break
        if not found:
            words = None
        if attributes is not None:
            return words, attrDict
        else:
            return words

    def addToDictionary(self, dictionary, key, value):
        # Add the given value to the dictionary where the key
        # entry is a list of values
        if dictionary.has_key(key):
            if type(value) is types.ListType:
                dictionary[key] += value
            else:
                dictionary[key].append(value)
        else:
            if type(value) is types.ListType:
                dictionary[key] = value
            else:
                dictionary[key] = [value]

    def getLeaves(self, tree, node):
        # Return a list of "leaves" for the component to which the node belongs.
        # "Leaves" will be nodes that have no children (sub-phrases)
        # EXCEPT if a node has words set AND it's children do not.
        # (Some phrases e.g. waveHeight_phrase, will sometimes by-pass
        # the sub-phrases and set the phrase words directly.)
        #
        component = node.getComponent()
        progeny = component.getProgeny()
        leaves = []
        for child in progeny:
            childList = child.get('childList')
            #print "child", child.getAncestor('name'), childList
            #print "    ", child.get('words'), child.getAreaLabel()
            if childList == [] or childList is None:
                # This is a leaf
                # Check to see if it has words
                leaf = child
                if child.get('words') is None:
                    # If not AND it's parent has words,
                    # take the parent as the leaf.
                    if child.parent.get('words') is not None:
                        leaf = child.parent
                leaves.append(leaf)
        return leaves

    def dayOrNight_element(self, tree, node, dayElement, nightElement):
        dayNight = self.getPeriod(node.getTimeRange(), 1)
        if dayNight == self.DAYTIME():
            element = dayElement
        else:
            element = nightElement
        return element

    def removeComponentPhrases(self, tree, node, phraseName, exceptions=[], areaLabels=[]):
        # Remove all phrases  with the given phraseName in the current component
        # If areaLabel is not None, check for a match there as well and only
        # remove the phrase if it is for that area.
        component = node.getComponent()
        progeny = component.getProgeny()
        #print "\nRemoving Phrases", phraseName, areaLabels, exceptions
        for child in progeny:
            name = child.get("name")
            if name == phraseName:
                #print "child", child, child.getAreaLabel()
                if child not in exceptions:
                    if areaLabels != []:
                        if child.getAreaLabel() in areaLabels:
                            #print "Removing 1", name, areaLabels, child
                            #import traceback
                            #traceback.print_stack(limit=3)
                            child.remove()
                    else:
                        child.remove()
                        #print "Removing 2", name, areaLabels
                        #import traceback
                        #traceback.print_stack(limit=3)

    class ElementInfo:
        def __init__(self, name, mergeMethod="Average", dataType=0,
                     statLabel="", primary=1, phraseDef=None):
            self.name = name
            self.mergeMethod = mergeMethod
            self.statLabel = statLabel
            self.primary=primary
            self.dataType = dataType
            self.phraseDef = phraseDef
##        def name(self):
##            return self.name
##        def mergeMethod(self):
##            return self.mergeMethod
##        def statLabel(self):
##            return self.statLabel
##        def primary(self):
##            return self.primary()
##        def dataType(self):
##            return self.dataType()
##        def phraseDef(self):
##            return self.phraseDef()

    class LocalEffectArea:
        def __init__(self, areaLabel, areaWords, conjAreaWords=None, intersectFlag=1):
            # AreaLabel can be "__Current__" which will use the
            # current edit area.
            self.areaLabel = areaLabel
            #  Words to describe this local effect area.
            # This can be a text string or a method
            #    arguments: (tree, node, LocalEffectArea)
            #    returns a text string
            self.areaWords = areaWords
            # This can be a text string or a method
            #    arguments: (tree, node, LocalEffectArea)
            #    returns a text string
            # If you want different area words for the conjunctive
            # phrase versus the embedded phrase, add them here
            if conjAreaWords is None:
                self.conjAreaWords = areaWords
            else:
                self.conjAreaWords = conjAreaWords
            # If 1, area is to be intersected with the current area to
            # look for local effects and the area must be listed as an
            # intersectArea in the current product component
            self.intersectFlag = intersectFlag

    class LocalEffect:
        def __init__(self, leAreaList, triggerMethod, exceptionWords):
            # List of LocalEffectAreas
            # Currently only two areas can be handled by the local effect methods
            self.leAreaList = leAreaList
            self.triggerMethod = triggerMethod
            self.exceptionWords = exceptionWords

    def makeRangeStats(self, tree, dataType, stats, timeRange):
        if stats is None:
            return None
        if tree.library.isStatsByRange(dataType, stats, timeRange):
            return stats
        else:
            return[(stats, timeRange)]

    def sumPrevStats(self, tree, node, areaLabel, elementName,
                     mergeMethod="Average", increment=1):        
        # Return a sum of stats going backward in time
        # until there is a zero value (when rounded to the nearest increment)
        # and NOT including the current value
        # For example:
        #   Period1 = 2 inches snow amt
        #   Period2 = 0 inches
        #   Period3 = 2 inches
        #   Period4 = 2 inches
        #   Period5 = 4 inches
        # If sumPrevStats is called during evaluation of
        # Period5, the sum will be 4 (sum of Period3 and Period4)

        # Calculate past snow
        prodTR = tree.getTimeRange()
        pastSnowMin = 0
        pastSnowMax = 0
        pastSnowTimeRange = self.makeTimeRange(prodTR.startTime() - 12*3600,
                                               prodTR.startTime())
        stats = tree.stats.get("SnowAmt", pastSnowTimeRange,
                               areaLabel, mergeMethod="MinMax")

        if stats is not None:
            pastSnowMin, pastSnowMax = self.getValue(stats, "MinMax")
            pastSnowMin = int(pastSnowMin+0.5)
            pastSnowMax = int(pastSnowMax+0.5)
        else:
            pastSnowMin = 0
            pastSnowMax = 0

        minSum = pastSnowMin
        maxSum = pastSnowMax

        # Calculate snow in forecast periods
        childList = node.getParent().get("childList")
        timeRange = node.getTimeRange()
        for child in childList:
            childTimeRange = child.getTimeRange()
            if childTimeRange == timeRange:
                break

            stats = tree.stats.get("SnowAmt", childTimeRange,
                                   areaLabel, mergeMethod="MinMax")
            if stats is None:
                continue
            min, max = self.getValue(stats, "MinMax")
            min = int(min+0.5)
            max = int(max+0.5)
            threshold = self.pop_snow_lower_threshold(tree, node)
            popStats = self.matchToWx(tree, node, "PoP", childTimeRange)
            if popStats < threshold:
                min = 0
                max = 0
            if max == 0:
                # Start over
                minSum = 0
                maxSum = 0
            else:
                minSum = minSum + min
                maxSum = maxSum + max
                
        return minSum, maxSum

    def getScalarRangeStr(self, tree, node,  element, min, max):
        min1=int(min)
        max1=int(max)
        if min1 == max1:
            return `min1`
        else:
            maxRange = self.maximum_range_nlValue(tree, node, element, element)
            maxRange = self.nlValue(maxRange, max1)
            if (maxRange == 0):
                return `max1`
            if abs(min1-max1) > maxRange:
                min1 = max1 - maxRange
            connector = self.value_connector(tree, node, element, element)
            return  `min1` + connector + `max1`

    def makeSentence(self, tree, node):
        "Make a sentence from the words at the node level"
        words = node.get("words")
        if words is None:
            return
        if words == "":
            words = "MISSING."
        else:
            words = self.sentence(words)
        return self.setWords(node, words)

    def chooseElement(self, tree, node, elementNameList):
        # Return first elementName for which there are stats for
        # this node.  If none, return last elementName.
        for elementName in elementNameList:
            stats = tree.stats.get(elementName, node.getTimeRange(),
                                  node.getAreaLabel(), mergeMethod="Avg")
            if stats is not None:
                return elementName
        return elementName

    # Narrative Level
    def assembleChildWords(self, tree, node):
        fcst = ""
        for child in node.get("childList"):
            words = child.get("words")
            if words is None:
                return
            fcst = fcst + words
        return self.setWords(node, fcst)

    # Component Level

    def noWords(self, tree, component):
        self.setWords(component, "")
        return self.DONE()

    def assembleSentences(self, tree, node):
        for phrase in node.get("childList"):
            words = phrase.get("words")
            if words is None:
                return
        fcst = ""
        lastQualifier = None
        lastPhrase = None
        self.orderWxPhrases(tree, node)
        self.consolidateLocalEffectPhrases(tree, node)
        for child in node.get("childList"):
            words = child.get("words")
            words, lastQualifier = self.qualifyWords(
                child, words, "conjunctiveQualifier", lastQualifier,
                lastPhrase)
            lastPhrase = child
            fcst = fcst + words
        return self.setWords(node, fcst)


    def assemblePhrases(self, tree, component):
        # Assemble component phrases and add Label
        # Qualify the phrases with local effect qualifiers
        # if present.
        #   e.g. "near the coast"
        phrases = []
        for phrase in component.get("childList"):
            words = phrase.get("words")
            if words is None:
                return
            if words != "":
                phrases.append(phrase)
        #print "\nAssemblePhrases"
        # Remove empty word phrases
        component.childList = phrases
        self.orderWxPhrases(tree, component)
        self.consolidateLocalEffectPhrases(tree, component)
        #print
        fcst = ""
        lastQualifier = None
        lastPhrase = None
        phraseList = []
        includeOnlyPhrases = self.includeOnlyPhrases_list(tree, component)

        for phrase in component.get("childList"):
            words = phrase.get("words")
            words = self.adjustWords(tree, phrase, words)
            #print phrase.get('name'), phrase.getAreaLabel()
            #print "     ", words
            if type(includeOnlyPhrases) is types.ListType and len(includeOnlyPhrases) > 0 and \
               phrase.get('name') not in includeOnlyPhrases:            
               # Do not include this phrase
               continue

            words, lastQualifier = self.qualifyWords(
                phrase, words, "conjunctiveQualifier", lastQualifier, lastPhrase)
            lastPhrase = phrase
            if words not in phraseList:
                phraseList.append(words)
                fcst = fcst + words
        # Add label
        curLocalTime, shift = self.determineTimeShift()
        issuanceInfo = tree.get("issuanceInfo")
        index = component.getIndex()
        label = self.createLabel(tree, component, component.get("timeRange"),
                                issuanceInfo, curLocalTime, shift, index)
        fcst = self.combineSentences(fcst)
        return self.setWords(component, label + fcst)

    def adjustWords(self, tree, phrase, words):
        # Make any special adjustments to phrases
        # This one is necessary for popMax since we have
        # removed repeating popType wording assuming
        # an embedded PoP phrase will result.
        # If it happens to end up as a conjunctive,
        # then we have to put the popType back.
        if phrase.get('name') in ["popMax_phrase"]:
            if not phrase.get('embedded'):
                popType = phrase.getDescendent('popType')
                if words.find(popType) == -1:
                    desc = phrase.get('descriptor')
                    words = words.replace(desc, desc + " " + popType)
        return words

    def assembleIndentedPhrases(self, tree, component):
        # Assemble and indent component phrases and add Label
        # Qualify the phrases with local effect qualifiers
        #  if present.
        #   e.g. "near the coast"
        for phrase in component.get("childList"):
            words = phrase.get("words")
            #print phrase, words
            if words is None:
                return

        # DR_18964
        self.consolidateLEPerPhraseInstance(tree, component)
        #self.consolidatePerPhraseNameGroup(tree, component)
        
        fcst = ""
        lastQualifier = None
        lastPhrase = None
        self.orderWxPhrases(tree, component)
        for phrase in component.get("childList"):
            words = phrase.get("words")
            if words is None:
                return
            if words == "":
                if self.removeEmptyPhrase(tree, phrase):
                    continue

            # Handle multiple element table phrase
            # that appears per period
            # No need to indent or qualify
            name = phrase.get("name")
            if name == "multipleElementTable_perPeriod_phrase":
                fcst = fcst + words
                continue

            if phrase.get("compound"):
                makeSentence = 0
            else:
                makeSentence = 1
            words, lastQualifier = self.qualifyWords(
                phrase, words, "conjunctiveQualifier", lastQualifier, lastPhrase,
                makeSentence=makeSentence)
            lastPhrase = phrase
            indentLabel = phrase.get("indentLabel")
            label = self.phrase_descriptor(
                tree, phrase, indentLabel, indentLabel)
            #print "indentLabel, label", indentLabel, label
            if indentLabel is not None and label == "":
                label = indentLabel
            if words == "":
                words = " "
            words = self.labelIndent(words, label)
            print phrase, words
            fcst = fcst + words
        # Add label
        curLocalTime, shift = self.determineTimeShift()
        issuanceInfo = tree.get("issuanceInfo")
        index = component.getIndex()
        label = self.createLabel(tree, component, component.get("timeRange"),
                                issuanceInfo, curLocalTime, shift, index)
        return self.setWords(component, label + "\n" + fcst + "\n")

    def consolidateLEPerPhraseInstance(self, tree, component):
	# Do the LE consolidation/combination for each (compound) phrase
        # create a pseudo component for the LE phrases to hang onto
        pseudo = component.copy()
        
        lePhraseNameGroups = []
        le_groups = {}
        for phrase in component.get("childList"):
            if phrase.get("localEffect"):
                lePhraseNameGroup, firstName = self.getLePhraseNameGroup(
                    tree, component, lePhraseNameGroups, phrase)
                le_groups.setdefault(lePhraseNameGroup, []).append(phrase)

        for name, nodes in le_groups.iteritems():
            #print name, nodes

            # put the nodes under the pseudo-component and do the
            # LE consolidation/combination
            pseudo.set("childList", nodes)
            self.consolidateLocalEffectPhrases(tree, pseudo)
            #self.combineConjunctivePhrases(tree, pseudo)
            
            # add the resultant nodes back under the component,
            # replacing the original ones
            newChildren = pseudo.get("childList")
            #print 'new nodes:', newChildren
            if newChildren == nodes:
                continue

            childList = []
            inserted = 0
            for child in component.get("childList"):
                if child in nodes:
                    if not inserted:
                        childList += newChildren
                        inserted = 1
                else:
                    childList.append(child)
            component.set("childList", childList)
        
	# delete the pseudo component
        pseudo.remove()
        pseudo.set("parent", None)
        pseudo.set("childList", [])


    def consolidatePerPhraseNameGroup(self, tree, component):
        # Do the LE consolidation/combination for each (compound) phrase
        # create a pseudo component for the LE phrases to hang onto
        pseudo = component.copy()

        lePhraseNameGroups = self.lePhraseNameGroups(tree, component)
        le_groups = {}
        for phrase in component.get("childList"):
            if phrase.get("localEffect"):
                lePhraseNameGroup, firstName = self.getLePhraseNameGroup(
                    tree, component, lePhraseNameGroups, phrase)
                le_groups.setdefault(lePhraseNameGroup, []).append(phrase)

        for name, nodes in le_groups.iteritems():
            #print name, nodes

            # put the nodes under the pseudo-component and do the
            # LE consolidation/combination
            pseudo.childList = nodes
            self.consolidateLocalEffectPhrases(tree, pseudo)
            self.combineConjunctivePhrases(tree, pseudo)

            # add the resultant nodes back under the component,
            # replacing the original ones
            newChildren = pseudo.childList
            childList = []
            inserted = 0
            for child in component.childList:
                if child in nodes:
                    if not inserted:
                        childList += newChildren
                        inserted = 1
                else:
                    childList.append(child)
            component.childList = childList

        # delete the pseudo component
        pseudo.remove()

    def weatherPhraseNames(self, tree, node):
        return ["weather_phrase", "skyPopWx_phrase"]

    def orderWxPhrases(self, tree, component):
        # Sort the weather phrases (weather_phrase, skyPopWx_phrase)
        # according to their time span for non-empty sub-phrases.
        # Then replace the weather phrases in the component childList.
        # We will assume that all the weather phrases for the
        # component are consecutive.
        wxPhraseNames = self.weatherPhraseNames(tree, component)
        phraseList = component.get("childList")
        wxPhrases = []
        for phrase in phraseList:
            if phrase.get("name") in wxPhraseNames:
                wxPhrases.append(phrase)
                #print "appending", phrase.get('name'), phrase.get('words')
        wxPhrases.sort(self.sortPhraseTimeSpans)
        #print "sorted list"
        #for phrase in wxPhrases:
        #        print phrase.get('name'), phrase.get('words')

        newPhraseList = []
        firstTime = 1
        for phrase in phraseList:
            if phrase.get("name") in wxPhraseNames:
                if firstTime:
                    # Add in the sorted wxPhrases
                    newPhraseList += wxPhrases
                    firstTime = 0
                continue
            # If not a wx phrase, append to the list
            newPhraseList.append(phrase)
        component.set("childList", newPhraseList)

    def sortPhraseTimeSpans(self, phrase1, phrase2):
        # Determine which phrase should come first according
        # to it's time span of non-empty sub-phrases
        # First, determine the timeSpan of each phrase:
        defaultTR = TimeRange.default()
        for phrase in [phrase1, phrase2]:
            # If time span already calculated, skip it
            timeSpan = phrase.get("timeSpan")
            if timeSpan is not None:
                continue
            startTime = None
            endTime = None
            for subPhrase in phrase.get("childList"):
                if subPhrase.get("words") != "":
                    tr = subPhrase.getTimeRange()
                    trStart = tr.startTime()
                    trEnd = tr.endTime()
                    if startTime is None:
                        startTime = trStart
                    elif startTime > trStart:
                        startTime = trStart
                    if endTime is None:
                        endTime = trEnd
                    elif endTime < trEnd:
                        endTime = trEnd
            if startTime is not None and  endTime is not None:
                # Make a time span for this phrase's words
                phraseTR = TimeRange.TimeRange(startTime, endTime)
                phrase.set("timeSpan", phraseTR)
            else:
                phrase.set("timeSpan", defaultTR)
        # Order the phrases according to their time spans.
        timeSpan1 = phrase1.get("timeSpan")
        timeSpan2 = phrase2.get("timeSpan")
        if timeSpan1 == defaultTR or timeSpan2 == defaultTR:
            return 0
        #print "\ntimeSpan1, timeSpan2", timeSpan2, timeSpan2
        #print " order", self.orderTimeRanges(timeSpan2, timeSpan2)
        return self.orderTimeRanges(timeSpan1, timeSpan2)

    def orderTimeRanges(self, tr1, tr2):
        # If tr1 should come before tr2, return -1
        # If equal, return 0, else return 1
        #print "\nin orderTimeRanges", tr1, tr2
        s1 = tr1.startTime()
        s2 = tr2.startTime()
        if s1 < s2:
            #print "return1 -1"
            return -1
        elif s2 < s1:
            #print "return2  1"
            return 1
        else:
            # They start at the same time
            e1 = tr1.endTime()
            e2 = tr2.endTime()
            # Put the one with the shortest
            # span first
            if e1 < e2:
                #print "return3  -1"
                return -1
            elif e1 > e2:
                #print "return4   1"
                return 1
            else:
                #print "return5   0"
                return 0

    def consolidateSubPhrases(self, tree, component):
        ##  Timing: This method runs at the component level
        ##  AFTER all sub-phrase words have been set and
        ##  BEFORE they have been assembled into phrases at the phrase level.
        ##
        ##  Purpose: Check for duplicate subPhrases and consolidate
        #            them into one.
        ##
        ##  For example:  (see Case 2 below)
        ##    Chance of thunderstorms in the morning (windward)
        ##    Chance of thunderstorms in the morning (leeward)
        ##    Chance of rain in the afternoon (windward)
        ##    Chance of snow in the afternoon (leeward)
        ##
        ##  becomes:
        ##    Chance of thunderstorms in the morning (unqualified)
        ##    Chance of rain in the afternoon (windward)
        ##    Chance of snow in the afternoon (leeward)

        # Set a flag to make sure we pass by this method the first time
        # so that the phrase set-up methods have a chance to run and
        # create sub-phrases before we try to consolidate them
        if component.get('first') is None:
            component.set('first', 1)
            return

        # Make sure all subPhrases have completed i.e. have words set
        subPhraseList = []
        leaves = self.getLeaves(tree, component)
        leFlag = 0
        for child in leaves:
            words = child.get("words")
            #print "Consolidate SubPhrases", child.getAncestor("name"), words
            if words is None:
                #print "Returning"
                return
            le = child.getAncestor('localEffect')
            if le is not None:
                leFlag = 1
            subPhraseList.append(child)

        # If no localEffects, skip this method
        if not leFlag:
            #print "In Consolidate SubPhrases: No local effects"
            return self.DONE()

        if self.__dict__.get("_leDebug", 0):
            print "\nConsolidateSubPhrases", tree.get('passes')

        # Create subPhraseDict =
        #    {(words, tr, lePhraseNameGroup):
        #       list of subPhrases with those words, tr, and lePhraseNameGroup}
        lePhraseNameGroups = self.lePhraseNameGroups(tree, component)
        subPhraseDict = {}
        for subPhrase in subPhraseList:
            tr = subPhrase.getTimeRange()
            words = subPhrase.get("words")
            lePhraseNameGroup, firstName = self.getLePhraseNameGroup(
                tree, component, lePhraseNameGroups, subPhrase.parent)
            if words == "":
                continue
            if self.__dict__.get("_leDebug", 0):
                print subPhrase.getAncestor("name")#, subPhrase.parent
                print "   ", subPhrase.getAreaLabel(), tr, words
                print "       local effect", subPhrase.getAncestor('localEffect')
            self.addToDictionary(subPhraseDict, (words,tr,lePhraseNameGroup), subPhrase)
        if self.__dict__.get('_leDebug', 0): print "subPhraseDict", subPhraseDict

        # Check for duplicate subPhrases and consolidate them into one.
        #  Case 1: If the duplicates are all for the same areaLabel,
        #     set the areaLabel for the consolidated subPhrase to that.
        #  Case 2: If the duplicates are for a local effect and
        #      cover all possible local effect areas for their phrase,
        #      create a new phrase for component.getAreaLabel()
        #      with this subPhrase wording. Remove the local effect subPhrases.
        #  Case 3: If the duplicates are for a local effect
        #      and they cover a subset of the local effect areas,
        #      leave them alone except for removing any component.getAreaLabel()
        #      duplicate subPhrases.
        compArea = component.getAreaLabel()
        if self.__dict__.get('_leDebug',0):
            print "\nDetermine Case for each set of duplicate phrases. compArea", compArea
        for key in subPhraseDict.keys():
            words, tr, lePhraseNameGroup = key
            subPhrases = subPhraseDict[key]
            if len(subPhrases) <= 1:
                continue
            # We have duplicate subPhrases to consolidate.
            # Gather the areaLabels for these duplicate subphrases
            # and the possible localEffect Area labels
            areaLabels, leAreas = self.gatherDupAreaLabels(
                tree, component, compArea, subPhrases)
            if self.__dict__.get('_leDebug',0):
                print "\n", words
                print "    ", tr, len(subPhrases)
                print "areaLabels, leAreas", areaLabels, leAreas
            # Determine the consolidated areaLabel
            if len(areaLabels) == 1:
                # Case 1
                if self.__dict__.get('_leDebug',0): print "CASE 1"
                # Remove all but the first subPhrase
                for subPhrase in subPhrases[1:]:
                    subPhrase.set('words', "")
            else:
                parent = subPhrases[0].parent
                localEffect = subPhrases[0].getAncestor('localEffect')
                if localEffect is None:
                    continue
                # See if all local effect areas are covered
                allAreasCovered = self.allLeAreasCovered(
                    tree, component, compArea, leAreas, areaLabels)
                if allAreasCovered:
                    # Case 2: Consolidate
                    if self.__dict__.get('_leDebug',0): print "CASE 2"
                    parent = subPhrases[0].parent
                    newNode = tree.copyPhrase(
                        parent, areaLabel=compArea,
                        copyAttrs=["doneList", "disabledSubkeys", "disabledElements",
                                   "firstElement", "elementName", "elementInfoList",
                                   "descriptor", "indentLabel"])
                    component.insertChild(parent, newNode)
                    newSubPhrase = subPhrases[0].copy()
                    newNode.set('childList', [newSubPhrase])
                    for subPhrase in subPhrases:
                        subPhrase.set('words', "")
                else:
                    # Case 3: Throw out any compArea subPhrase and
                    # leave local effect ones alone for now
                    if self.__dict__.get('_leDebug',0): print "CASE 3"
                    for subPhrase in subPhrases:
                        if subPhrase.getAreaLabel() == compArea:
                            subPhrase.set("words", "")
        return self.DONE()

    def gatherDupAreaLabels(self, tree, component, compArea, subPhrases):
        areaLabels = []
        leAreas = []
        for subPhrase in subPhrases:
            subArea = subPhrase.getAreaLabel()
            if subArea not in areaLabels:
                areaLabels.append(subArea)
            #print "subArea", subArea, subPhrase.getAncestor('name')
            if subArea != compArea:
                localEffect = subPhrase.getAncestor("localEffect")
                if localEffect is not None:
                    leAreas += self.getLeAreaList(tree, subPhrase, localEffect)
        return areaLabels, leAreas

    def allLeAreasCovered(self, tree, component, compArea, leAreas, areaLabels):
        allAreasCovered = 1
        if leAreas != []:
            # Determine if the subPhrases cover all possible local effect areas
            for leArea in leAreas:
                if leArea.intersectFlag:
                    areaName = self.getIntersectName(
                        compArea, leArea.areaLabel)
                elif leArea.areaLabel == "__Current__":
                    areaName = compArea
                else:
                    areaName = leArea.areaLabel
                #print "le Area Name", areaName
                if areaName not in areaLabels:
                    allAreasCovered = 0
                    break
        return allAreasCovered

    def consolidateSubPhrases_trigger(self, tree, node):
        # Checking to see if consolidateSubPhrases has been
        # completed (if it is on the component methodList)
        # This assumes that "node" is a phrase and its parent is
        # a component.
        parent = node.parent
        if self.consolidateSubPhrases in parent.methodList and \
           self.consolidateSubPhrases not in parent.doneList:
            return 0
        return 1


    def consolidateLocalEffectPhrases(self, tree, node):
        # Organize the local effect and non-local effect phrases.
        # "node" can be a component or a compound phrase.
        # Convert to embedded local effect phrases if appropriate.
        # Apply the Local Effect thresholds:
        #    repeatingEmbedded_localEffect_threshold
        #    repeatingPhrase_localEffect_threshold
        hasLE = 0
        for phrase in node.get('childList'):
            le = phrase.get('localEffect')
            if le is not None:
                hasLE = 1
                break
        if not hasLE:
            # No local effect phrases so no work to to be done
            return
        if not self.incorporateNonLocalEffectPhrases(tree, node):
            self.convertToEmbedded(tree, node)
        self.orderLocalEffectPhrases(tree, node)

    # Add later as an enhancement
    def incorporateNonLocalEffectPhrases(self, tree, node):
        #  Try to incorporate non-qualified phrases
        #    If there is exactly one leArea group in the set of phrases
        #      AND this group is composed of intersect areas
        #      AND there is more than one local effect phrase
        #      AND the number of non-local effect phrases
        #       < repeatingPhrase_localEffect_threshold:
        #         Convert them to conjunctive local effect phrases
        #         (one for each intersect local effect area)
        #        return 1
        #    Else:
        #        return 0

        #   EXAMPLE:
        #   Instead of:
        #     Chance of thunderstorms in the morning.
        #     Windward...Cloudy...Rain likely...Chance of precipitation 70 percent.
        #     Leeward...Partly cloudy...Scattered showers...Chance of precipitation 30
        #     percent. Highs in the 40s.  Winds 20 mph.
        #
        #   We will produce:
        #     Windward...Cloudy....Rain likely...Chance of thunderstorms in the morning...
        #     Chance of precipitation 70 percent.
        #     Leeward...Partly cloudy...Scattered showers...Chance of thunderstorms in
        #     the morning...Chance of precipitation 30 percent. Highs in the 40s.
        #     Winds 20 mph.
        return 0

    # Replaces combineConjunctiveLocalEffects
    def convertToEmbedded(self, tree, component):
        #
        #   Converts conjunctive local effects to embedded if possible.
        #   For each leGroup:
        #      If number of possible embedded phrases
        #              < repeatingEmbedded_localEffect_threshold
        #         AND there are NO mandatory conjunctives:
        #             Replace conjunctive phrases with an embedded phrase.
        #
        if self.__dict__.get('_leDebug',0): print "\nConvert to embedded"
        lePhraseNameGroups = self.lePhraseNameGroups(tree, component)
        lePhraseDict = self.createLePhraseDict(tree, component, lePhraseNameGroups)
        if self.__dict__.get('_leDebug',0): print "\nlePhraseDict", lePhraseDict

        repeatThreshold = self.repeatingEmbedded_localEffect_threshold(
            tree, component)
        qualifiersDict = self.createQualifiersDict(
            tree, component, lePhraseDict, repeatThreshold)
        if self.__dict__.get('_leDebug',0): print "\nqualifiersDict", qualifiersDict

        self.createEmbeddedPhrases(
            tree, component, lePhraseDict, qualifiersDict, repeatThreshold)
        if self.__dict__.get('_leDebug',0):print "\nlePhraseDict", lePhraseDict

        self.insertEmbeddedPhrases(
            tree, component, lePhraseDict, lePhraseNameGroups)

    def createLePhraseDict(self, tree, component, lePhraseNameGroups):
        # Organize phrases in the component by lePhraseNameGroups. lePhraseDict:
        #      lePhraseNameGroup: {
        #           qualifiers: e.g. ["leeward", "windward"]
        #           phrases: [phrases]
        #           firstElementName: firstElement.name
        #           }
        lePhraseDict = {}
        for phrase in component.get("childList"):
            if self.__dict__.get('_leDebug',0):
                print "phrase", phrase.get('name'), phrase.get('words')
                print "      ", phrase.getAreaLabel()
                print "      ", phrase.get('conjunctiveQualifier')
                print "      ", phrase.get('embeddedQualifier')
            localEffect = phrase.get('localEffect')
            if localEffect is None:
                continue
            lePhraseNameGroup, firstName = self.getLePhraseNameGroup(
                tree, component, lePhraseNameGroups, phrase)
            qualifier = phrase.get('embeddedQualifier')
            # Add the entry to the dictionary
            if lePhraseDict.has_key(lePhraseNameGroup):
                entry = lePhraseDict[lePhraseNameGroup]
                if qualifier not in entry["qualifiers"]:
                    entry["qualifiers"].append(qualifier)
                entry["phrases"].append(phrase)
            else:
                lePhraseDict[lePhraseNameGroup] = {
                    "qualifiers": [qualifier],
                    "phrases": [phrase],
                    "firstElementName": firstName,
                    }
        return lePhraseDict

    def createQualifiersDict(self, tree, component, lePhraseDict, repeatThreshold):
        # Find out how many potential embedded phrases there are for each
        #   unique set of qualifiers.
        # Create a qualifiersDict: qualifiers: count
        #      for lePhraseNameGroup in lePhraseDict:
        #          Can it be embedded i.e. are there NO mandatory conjunctives?
        #          If so, flag it as such and increase count for it's qualifier set.
        #
        qualifiersDict = {}
        for lePhraseNameGroup in lePhraseDict.keys():
            embedded = 1
            nameDict = lePhraseDict[lePhraseNameGroup]
            phrases = nameDict["phrases"]
            # For the phraseNameGroup to be embedded,
            # all phrases in the phrase name group must qualify
            # to be embedded i.e.the phrase must have just one subphrase
            # which is non-empty and covers the phrase time range.
            #
            # Also, create areaCountDict to keep track of number of
            # potentially embedded phrases per areaLabel:
            #        areaLabel:count
            # Unless the phraseNameGroup has multiple phrases (e.g. sky, pop, wx),
            # the count for each areaLabel will be 1.
            # If the number of embedded phrases for any area exceeds the
            # repeatThreshold, do not do an embedded phrase.
            #
            areaCountDict = {}
            for phrase in phrases:
                subPhrases = phrase.get('childList')
                if len(subPhrases) != 1:
                    embedded = 0
                    break
                subPhrase = subPhrases[0]
                if subPhrase.get("words") == "" or \
                       subPhrase.getTimeRange() != phrase.getTimeRange():
                    embedded = 0
                    break
                # Keep track of count for each area
                areaLabel = phrase.getAreaLabel()
                if areaCountDict.has_key(areaLabel):
                    areaCountDict[areaLabel] += 1
                else:
                    areaCountDict[areaLabel] = 1
                if areaCountDict[areaLabel] > repeatThreshold:
                    if self.__dict__.get('_leDebug',0):
                        print "areaCount exceeded", areaLabel, areaCountDict[areaLabel]
                    embedded = 0
                    break
            if embedded:
                qualifiers = nameDict["qualifiers"]
                # Sort, removeDups and re-store qualifiers as a tuple
                # We convert the qualifiers from a list to a tuple
                # so that the qualifiers can be dictionary keys in qualifiersDict
                qualifiers.sort()
                qualifiers = self.removeDups(qualifiers)
                tQualifiers = tuple(qualifiers)
                nameDict["qualifiers"] = tQualifiers
                if qualifiersDict.has_key(tQualifiers):
                    qualifiersDict[tQualifiers] += 1
                else:
                    qualifiersDict[tQualifiers] = 1
            nameDict["embedded"] = embedded
        return qualifiersDict

    def createEmbeddedPhrases(self, tree, component, lePhraseDict, qualifiersDict,
                              repeatThreshold):
        # Convert to embedded if repeatingEmbedded_localEffect_threshold is not exceeded.
        #      for lePhraseNameGroup in lePhraseDict:
        #          If it's leGroup count < repeatingEmbedded_localEffect_threshold:
        #             convert to embedded and add to lePhraseNameGroup entry
        #
        for lePhraseNameGroup in lePhraseDict:
            nameDict = lePhraseDict[lePhraseNameGroup]
            if not nameDict["embedded"]:
                continue
            qualifiers = nameDict["qualifiers"]
            count = qualifiersDict[qualifiers]
            if count > repeatThreshold:
                nameDict["embedded"] = 0
                continue
            # Create an embedded phrase
            phrases = nameDict["phrases"]
            nameDict["embeddedPhrase"] = self.makeEmbeddedFromConjunctiveLE(
                tree, component, phrases)

    def insertEmbeddedPhrases(self, tree, component, lePhraseDict, lePhraseNameGroups):
        #    Insert the embedded phrases at the proper places in the component
        #         phraseList and remove the associated conjunctive phrases.
        #         Embedded phrases are inserted at the site of the first
        #         associated conjunctive phrase.
        #
        newPhraseList = []
        # doneList keeps track of those lePhraseNameGroups for which we've already
        # inserted the embedded phrase
        doneList = []
        if self.__dict__.get('_leDebug',0):print "\nStep"
        for phrase in component.get("childList"):
            if self.__dict__.get('_leDebug',0):print "phrase", phrase.get('words')
            localEffect = phrase.get('localEffect')
            if localEffect is None:
                newPhraseList.append(phrase)
                continue
            # Determine lePhraseNameGroup for this phrase
            lePhraseNameGroup, firstName = self.getLePhraseNameGroup(
                tree, component, lePhraseNameGroups, phrase)
            if self.__dict__.get('_leDebug',0):
                print "   lePhraseNameGroup", lePhraseNameGroup
            nameDict = lePhraseDict[lePhraseNameGroup]
            if not nameDict["embedded"]:
                newPhraseList.append(phrase)
                continue
            if lePhraseNameGroup not in doneList:
                # insert the embedded phrase
                newPhraseList.append(nameDict["embeddedPhrase"])
            doneList.append(lePhraseNameGroup)
        component.set("childList", newPhraseList)

    def getLePhraseNameGroup(self, tree, node, lePhraseNameGroups, phrase):
        # Unless the group is an explicitly defined by lePhraseNameGroups,
        # the name returned will be the <phraseName_firstElementName>.
        #   Make sure all the phrases for each lePhraseNameGroup have the
        #       same firstElement UNLESS the lePhraseNameGroup is listed explicitly
        #       in self.lePhraseNameGroups.
        #       If not, make a separate dictionary entry name for each firstElement.
        #   For example:  In the FWF, the"dayOrNight_phrase" may have MaxT for some phrases
        #       and MinRH for others.  We want to keep them separate when converting
        #       to embedded local effect phrases.
        explicitGroup = 0
        phraseName = phrase.get('name')
        for group in lePhraseNameGroups:
            if phraseName in group:
                lePhraseNameGroup = group
                explicitGroup = 1
        # Check the firstElement
        firstElement = phrase.get('firstElement')
        if firstElement is None:
            firstName = "None"
        else:
            firstName = firstElement.name
        if not explicitGroup:
            lePhraseNameGroup = phraseName + "_" + firstName
        return lePhraseNameGroup, firstName

    def orderLocalEffectPhrases(self, tree, node):
        #
        #  Group all conjunctive local effect phrases
        #      for each local effect area together
        #      (at the location of the first occurrence).
        #
        #   EXAMPLE:
        #     LEEWARD...SUNNY IN THE MORNING THEN BECOMING PARTLY SUNNY...SCATTERED SHOWERS.
        #     WINDWARD...MOSTLY CLOUDY WITH SCATTERED SHOWERS
        #
        #   instead of:
        #     LEEWARD...SUNNY IN THE MORNING THEN BECOMING PARTLY SUNNY.
        #     WINDWARD...MOSTLY CLOUDY WITH SCATTERED SHOWERS.
        #     LEEWARD...SCATTERED SHOWERS.
        #
        phraseList = node.get("childList")
        newList = []
        doneAreas = []
        #print "\nOrder LE phrases"
        for phrase in phraseList:
            localEffect = phrase.get("localEffect")
            areaLabel = phrase.getAreaLabel()
            embedded = phrase.get("embedded")
            #print "PHRASE", phrase.get('words')
            if localEffect is None or embedded:
                newList.append(phrase)
            else:
                #print "   phrase", phrase.get("name")
                #print "   conjqualifier", phrase.get("conjunctiveQualifier")
                #print "   area", areaLabel
                if areaLabel in doneAreas:
                    # We already added this phrase to the newList
                    # as a Local Effect area
                    continue
                newList.append(phrase)
                # Gather the other phrases for this local effect area.
                index = phraseList.index(phrase)
                for p in phraseList[index+1:]:
                    p_localEffect = p.get("localEffect")
                    p_area = p.getAreaLabel()
                    if p_localEffect is not None and p_area == areaLabel:
                        newList.append(p)
                doneAreas.append(areaLabel)
        node.set("childList", newList)

    def combineConjunctivePhrases(self, tree, component):
        # Check for Conjunctive Local Effects and make sure
        # we do not repeat the indented label.
        #
        #   For example:
        #   LAL.................IN THE VALLEYS ...1.
        #   LAL.................IN THE MOUNTAINS...3 UNTIL 2400...THEN 1.
        #
        #   Should be:
        #   LAL.................IN THE VALLEYS ...1.
        #                       IN THE MOUNTAINS...3 UNTIL 2400...THEN 1.
        #
        for phrase in component.get("childList"):
            words = phrase.get("words")
            if words is None:
                return
        newChildList = []
        lastName = ""
        lastElement = ""
        lastPhrase = None
        for phrase in component.get("childList"):
            words = phrase.get("words")
            if words is None:
                return
            curName = phrase.get("name")
            curElement = phrase.get("elementName")
            if lastPhrase is None:
                lastPhrase = phrase
                lastName = curName
            else:
                # Look for a local effect phrase to be combined with
                # lastPhrase
                localEffect = phrase.get("localEffect")
                embedded = phrase.get('embedded')
                #print "phrase", curName, lastName, localEffect
                if localEffect is not None and curName == lastName and \
                   embedded != 1 and curElement == lastElement:
                    # Combine this phrase words into last one
                    # Add conjunctive qualifier
                    #print "combining"
                    phraseWords = phrase.get("words")
                    qualifier = phrase.get("conjunctiveQualifier")
                    if qualifier is not None and qualifier != "":
                        phraseWords = qualifier + " "+ phraseWords
                    newWords = lastPhrase.get("words") + "." + \
                               phraseWords
                    lastPhrase.set("words", newWords)
                else:
                    # Add phrase to new list
                    #print "switching"
                    newChildList.append(lastPhrase)
                    lastPhrase = phrase
                    lastName = curName
                    lastElement = curElement
        # Clean up lastPhrase
        if lastPhrase is not None:
            newChildList.append(lastPhrase)
        component.set("childList", newChildList)


    def makeEmbeddedFromConjunctiveLE(self, tree, component, conjList):
        # Make an embedded phrase from the list of conjunctive phrases
        #
        conjWords = ""
        embeddedPhrase = tree.copyPhrase(
            conjList[0], areaLabel = component.getAreaLabel(),
            copyAttrs=["doneList", "descriptor", "indentLabel",
                       "embeddedDescriptor", "localEffect"])
        descriptor = self.addSpace(embeddedPhrase.get("descriptor"))
        if descriptor == "":
            descriptor = self.addSpace(embeddedPhrase.get("embeddedDescriptor"))
        #print "\nIn makeEmbeddedFromConjunctiveLE", descriptor
        index = 0
        localEffect = embeddedPhrase.get("localEffect")
        fcst = ""
        # Gather words
        for phrase in conjList:
            words = phrase.get("words")
            if words == "":
                continue
            if not index == 0:
                # Get connector
                connector = localEffect.exceptionWords
                fcst = fcst + connector
                # Get rid of duplicate descriptor
                if descriptor != "":
                    words = words.replace(descriptor, "")
           # Local Effect Descriptor
            areaWords = phrase.getAncestor("embeddedQualifier")
            if areaWords is None:
                areaWords = ""
            areaWords = self.addSpace(areaWords, "leading")
            fcst = fcst + words + areaWords
            index = index + 1
        embeddedPhrase.set("words", fcst)
        embeddedPhrase.set('embedded', 1)
        self.postProcessPhrase(tree, embeddedPhrase)
        #print "embedded words", embeddedPhrase.get('words')
        return embeddedPhrase

    def qualifyWords(self, node, words, qualifierName, lastQualifier,
                     lastPhrase, makeSentence=1):
        # Qualifies words with local effect qualifiers
        # Also, if makeSentence==1, makes the words into a sentence
        #   when appropriate.
        # Returns the modified words and the qualifier (if any)
        #
        # Logic:
        #   If empty words, skip.
        #   If no qualifier:
        #      if makeSentence:
        #         makeSentence and return words and lastQualifier
        #   If there is a qualifier:
        #      Handle a new qualifier.
        #         If qualifier is new and non-empty:
        #           Add the qualifier and ellipses to beginning of words
        #      Handle a continuation: If the next phrase will be qualified
        #           with the same qualifier,
        #           Add ellipses to the end of the words. In this case,
        #             we will not add a period to the end of the words
        #             when making a sentence.
        #      if makeSentence, make the words into a sentence with or without
        #         a period at the end.
        #      return words and qualifier
        #
        qualifier = node.get(qualifierName)
        #print "\nQualify words: qualifier, lastQualifier, words", qualifier, lastQualifier, words
        if words == "":
            return words, lastQualifier
        addPeriod = 1
        if qualifier is not None:
            if qualifier != lastQualifier and qualifier != "":
                words = qualifier + "..." + words
            next = self.getNext_nonEmpty(node, "words")
            if next is not None:
                nextQualifier = next.get(qualifierName)
                #print "nextQualifier, qualifier", nextQualifier, "X", qualifier, "X", words
                if nextQualifier == qualifier:
                    addPeriod = 0
                    words = words + "..."
        if makeSentence:
            words = self.sentence(words, addPeriod)
        #print "returning", words
        return words, qualifier

    def getNext_nonEmpty(self, node, attrName):
        next = node.getNext()
        while 1:
            if next is None:
                break
            val = next.get(attrName)
            if val is not None and val != "":
                break
            next = next.getNext()
        return next

    def namesEqual(self, name1, name2):
        weatherPhrases = ["skyPopWx_phrase", "weather_phrase"]
        if name1 == name2 or \
           (name1 in weatherPhrases and name2 in weatherPhrases):
            namesEqual = 1
        else:
            namesEqual = 0
        return namesEqual

    def wordWrap(self, tree, component):
        # Wrap the component.words()
        compWords = component.get("words")
        if compWords is None:
            return
        compWords = self.endline(compWords, tree.get("lineLength"))
        return self.setWords(component, compWords)

    def createLabel(self, tree, node, timeRange, issuanceInfo, currentLocalTime, shift, index=0):
        # Make a label given the timeRange in GMT and the shift to
        # convert it to local time. currentLocalTime can be used to
        # compare to current day.

        # NOTE: If you make changes to this method, change the SAF_Overrides
        # file as it is overridden there.

        if timeRange.duration() <= 3600:
                return ""
        if index == 0:
            try:
                label =  issuanceInfo.period1Label()
                if label != "":
                    return label
            except:
                pass
        try:
            today =  issuanceInfo.todayFlag()
        except:
            today = 1
        try:
            useHolidays = self._useHolidays
        except:
            useHolidays = 1
        nextDay24HourLabel = self.nextDay24HourLabel_flag(tree, node)
        splitDay24HourLabel = self.splitDay24HourLabel_flag(tree, node)
        label =  self.getWeekday(timeRange, holidays=useHolidays, shiftToLocal=1,
                                labelType="CapitalWithPeriod", today=today,
                                tomorrow=0, nextDay24HourLabel=nextDay24HourLabel,
                                 splitDay24HourLabel=splitDay24HourLabel)
        return label


    # Ordering phrases
    def orderPhrases(self, tree, component):
        # Reorder highs and lows based on start period

        reorderList = []
        timeRange=component.getTimeRange()
        areaLabel= component.getAreaLabel()

        if timeRange.duration() >= 24 * 3600:
            startTR = TimeRange.TimeRange(timeRange.startTime(),
                                     timeRange.startTime() + (12 * 3600))
            dayNight = self.getPeriod(startTR, 1)
            if dayNight == self.NIGHTTIME():
                reorderList.append(("lows_phrase", "highs_phrase"))
                reorderList.append(("lows_range_phrase", "highs_range_phrase"))

        for phrase1, phrase2 in reorderList:
            self.moveAbove(tree, component, phrase1, phrase2)

        return self.DONE()

    def moveAbove(self, tree, component, phrase1, phrase2):
        # Move the phrase phrase1 above phrase2 in the
        # component list

        # Find Phrase to move
        savedPhrase = ""
        for phrase in component.childList:
            name = phrase.get("name")
            if name == phrase1:
                savedPhrase = phrase
                break
        # Create new phrase list inserting savedPhrase in
        # the new location
        newPhraseList = []
        for phrase in component.childList:
            name = phrase.get("name")
            if name == phrase1:
                continue
            if name == phrase2 and savedPhrase != "":
                newPhraseList.append(savedPhrase)
            newPhraseList.append(phrase)
        component.set("childList", newPhraseList)

        return self.DONE()

    # Phrase Level
    def subPhraseSetUp(self, tree, phrase, elementInfoList, connectorMethod, resolution=None):
        # Set up subPhrase nodes and "statDict" statistics for multiple elements.
        # The temporal resolution of the first element determines the number of subPhrases created.
        # If the elementInfoList is empty, one empty subPhrase is created with an empty "statDict".
        # If there is noData for the first element, one empty subPhrase is created.
        #
        # Sets up the following attributes:
        #    Phrase Level
        #       descriptor -- based on phrase_descriptor for the first element
        #       connectorMethod -- based on setUp arguments
        #       elementInfoList -- adds "outUnits" to each elementInfo in list
        #       firstElement    -- elementInfo for first in elementInfoList
        #       elementName     -- elementName for first in elementInfoList
        #
        #    SubPhrase Level
        #       elementName     -- elementName for first in elementInfoList
        #       timeRange
        #       statDict        -- entries for all elements in elementInfoList
        #
        timeRange = phrase.getTimeRange()
        areaLabel = phrase.getAreaLabel()
        statDictList = []

        if len(elementInfoList) < 1:
            # Make phrase with one empty subphrase
            self.makeEmptySubPhrase(tree, phrase, None)
            return self.DONE()

        # Make sub ranges based on first element
        first = elementInfoList[0]
        elementName = first.name
        first.outUnits = self.element_outUnits(tree, phrase, first.name, first.name)

        # Check to see if the timeRange is great enough to collapse sub-phrases
        # automatically
        hours = self.collapseSubPhrase_hours(tree, phrase, elementName, elementName)
        if timeRange.duration() > hours * 3600:
            first.mergeMethod = self.mergeMethod(tree, phrase, elementName, elementName)

        #print "Getting first", first.name, timeRange, areaLabel
        # Check to see if we are requesting a particular time resolution
        if resolution is not None:
            # Create sub-ranges with this resolution and provide list
            # of stats for each time range
            subRanges = self.divideRange(timeRange, resolution)
            stats = []
            for subRange in subRanges:
                subStats = tree.stats.get(first.name, subRange, areaLabel,
                               first.statLabel, first.mergeMethod)
                stats.append((subStats, subRange))
        else:
            stats = tree.stats.get(first.name, timeRange, areaLabel,
                               first.statLabel, first.mergeMethod)
        #print "stats", stats
        statsByRange = self.makeRangeStats(tree, first.dataType, stats, timeRange)
        #print "statsByRange", first.name, statsByRange

        # Case of no data for first element
        if statsByRange is None:
            self.makeEmptySubPhrase(tree, phrase, first)
            return self.setWords(phrase, "")

        phrase.set("emptyPhrase", 0)
        # Set up descriptor and connector
        if phrase.get("descriptor") is None:
            descriptor = self.phrase_descriptor(tree, phrase, first.name, first.name)
            phrase.set("descriptor", descriptor)
        phrase.set("connectorMethod", connectorMethod)

        # Create sub phrases based on first element
        # Create subPhrase List of (statDict, subRange) pairs
        # This list will be added to by each element
        subPhraseList = []
        for stats, subRange in statsByRange:
            #print "stats going into statDict", stats
            subPhraseList.append(({first.name:stats}, subRange))

        for subPhrase in subPhraseList:
            # Add each additional element to the sub range statDict
            statDict, subRange = subPhrase
            for elementInfo in elementInfoList[1:]:
                name = elementInfo.name
                elementInfo.outUnits = self.element_outUnits(tree, subPhrase, name, name)
                #print "Getting sub stats", elementInfo
                stats = tree.stats.get( elementInfo.name, subRange, areaLabel,
                                        elementInfo.statLabel, elementInfo.mergeMethod)
                # Add to subPhrase statDict for each subPhrase
                statDict[name] = stats

        # Make SubPhrase children
        subPhraseMethods = phrase.get("subPhraseMethods")
        childList = []
        #print "subPhraseList", subPhraseList
        for statDict, subRange in subPhraseList:
            subPhrase = tree.makeNode([], subPhraseMethods, phrase)
            #print "statDict", statDict
            subPhrase.set("statDict", statDict)
            subPhrase.set("timeRange", subRange)
            subPhrase.set("changeFlag", 0)
            subPhrase.set("elementName", elementName)
            childList.append(subPhrase)

        #print "Setting childList"
        phrase.set("firstElement", first)
        phrase.set("elementInfoList", elementInfoList)
        phrase.set("elementName", elementName)
        phrase.set("childList", childList)
        if childList == []:
            self.setWords(phrase, "")

        #print "AFTER SET-UP"
        #if elementName == "Wx":
        #    print phrase.printNode(phrase)
        return self.DONE()

    def makeEmptySubPhrase(self, tree, phrase, firstElement):
        phrase.set("emptyPhrase", 1)
        phrase.set("firstElement", firstElement)
        phrase.set("connectorMethod", None)
        subPhraseMethods = phrase.get("subPhraseMethods")
        subPhrase = tree.makeNode([], subPhraseMethods, phrase)
        if firstElement is None:
            phrase.set("elementInfoList", [])
            phrase.set("elementName", None)
            subPhrase.set("elementName", None)
            subPhrase.set("statDict", {})
        else:
            phrase.set("elementInfoList", [firstElement])
            phrase.set("elementName", firstElement.name)
            subPhrase.set("elementName", firstElement.name)
            subPhrase.set("statDict", {firstElement.name:None})
        subPhrase.set("timeRange", phrase.getTimeRange())
        subPhrase.set("changeFlag", 0)
        phrase.set("childList", [subPhrase])


    ### Checking for differences between sub-phrases
    def checkForDifferences(self, tree, node, elementInfo, magOnly=0, dirOnly=0):
        # Return 1 if there are differences among the subPhrase values
        # for the given element.
        # If VECTOR and magOnly==1, only the magnitude is checked.
        # If VECTOR and dirOnly==1, only the direction is checked.
        # If no data, return 1 as well
        elementName = elementInfo.name
        dataType = elementInfo.dataType
        statList = self.getSubStats(node, elementName)
        if len(statList) > 1:
            # Check each subphrase against the first
            # Return when a difference is found
            if dataType == self.SCALAR():
                if statList[0] is None:
                    return 1
                value = self.getValue(statList[0], "MinMax")
                min1, max1 = value
                for statVal in statList[1:]:
                    if statVal is None:
                        return 1
                    statVal = self.getValue(statVal, "MinMax")
                    min2, max2 = statVal
                    differenceFlag = self.checkScalarDifference(
                        tree, node, elementName, min1, max1, min2, max2)
                    if differenceFlag:
                        return 1
                return 0
            if dataType == self.VECTOR():
                if statList[0] is None:
                    return 1
                mag, dir1 = self.getValue(statList[0], "MinMax", self.VECTOR())
                min1, max1 = mag
                for stats in statList[1:]:
                    if stats is None:
                        return 1
                    statMag, dir2 = self.getValue(stats, "MinMax", self.VECTOR())
                    min2, max2 = statMag
                    differenceFlag = self.checkVectorDifference(
                        tree, node, elementName, min1, max1, dir1, min2, max2, dir2, magOnly, dirOnly)
                    if differenceFlag:
                        return 1
                return 0
            if dataType == self.WEATHER() or dataType == self.DISCRETE():
                wx = statList[0]
                for wxVal in statList[1:]:
                    if wxVal is None or wx is None:
                        return 1
                    if wxVal != wx:
                        return 1
        return 0

    def checkScalarDifference(self, tree, node, elementName, min1, max1, min2, max2):
        # Return 1 if the min/max pairs show a difference
        # First see if both are below null threshold
        threshold = self.null_nlValue(tree, node, elementName, elementName)
        threshold1 = self.nlValue(threshold, max1)
        threshold2 = self.nlValue(threshold, max2)
        if max1 < threshold1 and max2 < threshold2:
            return 0
        # See if only one is below null threshold
        if self.null_alwaysDistinct_flag(tree, node, elementName, elementName):
            if max1 < threshold1 or max2 < threshold2:
                return 1
        # If one set of min/max has only one value,
        # and that value matches the min or
        # max of the other set, show no difference.
        if min1 == max1 and (min1==min2 or max1==max2):
            return 0
        if min2 == max2 and (min1==min2 or max1==max2):
            return 0
        # Compare mins and compare maxs
        diff_nlValue = self.scalar_difference_nlValue(tree, node, elementName, elementName)
        diff_min = self.nlValue(diff_nlValue, min(min1, min2))
        diff_max = self.nlValue(diff_nlValue, max(max1, max2))
        if abs(min1-min2) < diff_min and abs(max1-max2) < diff_max:
            return 0
        return 1

    def checkVectorDifference(self, tree, node, elementName,
                              min1, max1, dir1, min2, max2, dir2, magOnly=0, dirOnly=0):
        # Return 1 if the min/max/dir pairs show a difference
        #print "Checking", elementName, min1, max2, dir1, min2, max2, dir2, magOnly
        if magOnly == 0 or dirOnly == 1:
            # DR_18632
#            if self.direction_difference(dir1, dir2) >= self.vector_dir_difference(
#                tree, node, elementName, elementName):
#                return 1
            diff = self.direction_difference(dir1, dir2)
            nlValue_dict = self.vector_dir_difference_nlValue(
                tree, node, elementName, elementName)
            threshold_min = self.nlValue(nlValue_dict, min(min1, min2))
            threshold_max = self.nlValue(nlValue_dict, max(max1, max2))
            if diff >= min(threshold_min, threshold_max):
                return 1
            if dirOnly == 1:
                return 0

        # Check magnitude
        # Compare mins and maxs

        # Add special check for marine wording:
        # This will prevent:
        #    NORTHWEST GALES TO 35 KNOTS RISING TO GALES TO 35 KNOTS AFTER MIDNIGHT.
        # And will facilitate:
        #    "N WINDS 30 KT IN THE MORNING INCREASING TO
        #     GALES TO 35 KT EARLY IN THE AFTERNOON...THEN
        #     EASING TO 30 KT LATE IN THE AFTERNOON."
        if elementName == "Wind":
            if self.marine_wind_combining_flag(tree, node):
                if max1 > 30 or max2 > 30:
                    # Check for both within the same warning thresholds
                    warnThreshold1 = self.getWarnThreshold(max1)
                    warnThreshold2 = self.getWarnThreshold(max2)
                    if warnThreshold1 == warnThreshold2:
                        return 0
                    else:
                        return 1

        # First see if both are below null threshold
        threshold = self.null_nlValue(tree, node, elementName, elementName)
        threshold1 = self.nlValue(threshold, max1)
        threshold2 = self.nlValue(threshold, max2)
        if max1 < threshold1 and max2 < threshold2:
            return 0
        # See if only one is below null threshold
        if self.null_alwaysDistinct_flag(tree, node, elementName, elementName):
            if max1 < threshold1 or max2 < threshold2:
                return 1
        # If one set of min/max has only one value,
        # and that value matches the min or
        # max of the other set, show no difference.
        if min1 == max1 and (min1==min2 or max1==max2):
            return 0
        if min2 == max2 and (min1==min2 or max1==max2):
            return 0
        # Check for magnitude differences
        mag_nlValue = self.vector_mag_difference_nlValue(
            tree, node, elementName, elementName)
        mag_diff_min = self.nlValue(mag_nlValue, min(min1, min2))
        mag_diff_max = self.nlValue(mag_nlValue, max(max1, max2))
        if abs(min1-min2) >= mag_diff_min or abs(max1-max2) >= mag_diff_max:
            return 1
        return 0

    def getWarnThreshold(self, max):
        if max >= 65:
            return 3
        elif max > 45:
            return 2
        elif max > 30:
            return 1
        else:
            return 0

    def maskSubkeys(self, subkeyList, intensity=None):
        # Make a new weather key masking the given intensity with the given value
        if intensity is not None:
            newkeyList = []
            for subkey in subkeyList:
               newSubkey = WeatherSubKey.weatherSubKey(self._argDict['dataMgr'], subkey.coverage(), subkey.wxType(), intensity,
                                              subkey.visibility(), subkey.attributes())
               newkeyList.append(newSubkey)
            subkeyList = newkeyList
        return subkeyList

    def checkWeatherSimilarity(self, tree, node, rankList1, rankList2,
                               node1=None, node2=None, tr1=None, tr2=None,
                               al1=None, al2=None):
        ### FIXES BUG BY FORCING ATTRIBUTES CHECK WHEN CHECKING FOR SIMILAR WX
        # Return 0 if the two sets of subkeys in the rankLists are significantly
        #   different
        # If the keys can be considered similar:
        #    Return 1 if the first set of keys presides
        #    Return 2 if the second set of keys presides
        #    Return a new aggregated rankList if there are multiple subkeys in the
        #    rankLists AND they are similar.
        #
        # Optional nodes and time ranges may be supplied.  These are used for
        # accessing PoP stats.  All are necessary since "similar_diurnal" does
        # comparisons for various time ranges and local effects does comparisons
        # for various areas.

        #print "\nCheckWxSimilarity"

        # The ranks are available, but not currently used
        stats1 = self.getSubkeys(rankList1)
        stats2 = self.getSubkeys(rankList2)
        # Sort for comparison
        stats1.sort(self.rankedSortOrder)
        stats2.sort(self.rankedSortOrder)

        diff = []
        for element in stats1:
            test = 1
            for el in stats2:
                if str(element) == str(el):
                    test = 0
            if test and str(element) not in diff:
                diff.append(str(element))
        for element in stats2:
            test = 1
            for el in stats1:
                if str(element) == str(el):
                    test = 0
            if test and str(element) not in diff:
                diff.append(str(element))
        if len(diff) == 0:
            return 1       
        
        if stats1 == stats2:
            #print 'checkWx return 1'
            return 1
   
        # Check for equal length of statistics
        if len(stats1) == len(stats2):
            # If there is only one subkey to worry about
            if len(stats1) == 1:
                #  If the types, intensities, and coverages are similar
                if self.similarWxTypes(tree, node, stats1[0], stats2[0]):
                    if self.similarIntensities(tree, node, stats1[0], stats2[0]):
                        if self.similarAttributes(tree, node, stats1[0], stats2[0]):
                            flag = self.similarCoverages(tree, node, stats1[0], stats2[0])
                            if flag > 0:
                                #print "returning flag", flag
                                return flag
                else:
                    # Different wxTypes are not similar
                    #print "returning diff wxTypes 0"
                    return 0
            # Node can turn off this check.
            # Some phrases (severeWeather_phrase, heavyPrecip_phrase, heavyRain_phrase)
            # are checking intensities, so we don't want to loose them
            if node.getAncestor("noIntensityCombining") != 1:
                # Make new subkeys that all have the same intensity
                stats1 = self.maskSubkeys(stats1, intensity="-")
                stats2 = self.maskSubkeys(stats2, intensity="-")
                if stats1 == stats2:
                    #print 'checkWx return 1'
                    return 1
            # Handle case of len(stats) > 1
            if len(stats1) > 1:
                return self.checkSubkeysSimilarity(
                    tree, node, rankList1, rankList2, node1, node2, tr1, tr2, al1, al2)

        # Check the PoP.
        #    If low for both time periods and areas
        #       AND there is no non-precip Wx
        #    then we can assume the Wx is the same
        if node1 is None:
            node1 = node
        if node2 is None:
            node2 = node
        if tr1 is None:
            tr1 = node1.getTimeRange()
        if tr2 is None:
            tr2 = node2.getTimeRange()
        if al1 is None:
            al1 = node1.getAreaLabel()
        if al2 is None:
            al2 = node2.getAreaLabel()
        popstats1 = self.matchToWx(tree, node1, "PoP", tr1, al1)
        popstats2 = self.matchToWx(tree, node2, "PoP", tr2, al2)
        #print "popstats", popstats1, popstats2
        if popstats1 < self.pop_wx_lower_threshold(tree, node1) and \
           popstats2 < self.pop_wx_lower_threshold(tree, node2):
            for subkey in stats1:
                if not self.pop_related_flag(tree, node1, subkey):
                    return 0
            for subkey in stats2:
                if not self.pop_related_flag(tree, node2, subkey):
                    return 0
            return 1
        #print 'checkWx return 0'
        return 0

    def checkSubkeysSimilarity(self, tree, node, rankList1, rankList2,
                               node1, node2, tr1, tr2, al1, al2):
        # Return 0 if the two sets of subkeys in the rankLists are significantly
        #   different
        # Otherwise, return a new rankList of the combined subkeys and ranks
        # sorted in rank order
        #
        # We combine if:
        #   The set of wxTypes in rankList1 is equal to the set of wxTypes in rankList2 AND
        #   Each wxType individually can be combined i.e. they have similar coverages
        #print "\nCheckSubkeysSimilarity"

        # Sort ranklists by wxType
        list1 = self.removeNoWx(rankList1)
        list2 = self.removeNoWx(rankList2)
        list1.sort(self.rankedWxTypeOrder)
        list2.sort(self.rankedWxTypeOrder)

        #print "rankList1, rankList2", rankList1, rankList2

        newRankList = []
        for i in range(len(list1)):
            subkey1, rank1 = list1[i]
            wxType1 = subkey1.wxType()
            subkey2, rank2 = list2[i]
            wxType2 = subkey2.wxType()
            if not wxType1 == wxType2:
                # We cannot combine
                return 0
            # See of the wxTypes have similar coverages
            similarFlag = self.checkWeatherSimilarity(
                tree, node, [list1[i]],[list2[i]], node1, node2, tr1, tr2, al1, al2)
            if similarFlag == 0:
                return 0
            newRank = int((rank1 + rank2)/2.0)
            newSubkey = self.makeAggregateSubkey(subkey1, rank1, subkey2, rank2)
            newRankList.append((newSubkey, newRank))
##            if similarFlag == 1:
##                newRankList.append((subkey1, newRank))
##            else:
##                newRankList.append((subkey2, newRank))
        # Sort newRankList
        newRankList.sort(self.rankedSortOrder)
        #print "returning", newRankList
        return newRankList

    def removeNoWx(self, rankList):
        newList = []
        for subkey, rank in rankList:
            if subkey.wxType() == "<NoWx>":
                continue
            newList.append((subkey, rank))
        return newList

    def similarWxTypes(self, tree, node, subkey1, subkey2):
        # If wxTypes should be similar, return 1
        # else return 0
        wxType1 = subkey1.wxType()
        wxType2 = subkey2.wxType()
        inten1 = subkey1.intensity()
        inten2 = subkey2.intensity()
        # Take care of sprinkles and flurries
        if wxType1 == wxType2 and wxType1 in ["RW", "SW"]:
            if inten1 != inten2 and (inten1 == "--" or inten2 == "--"):
                return 0
        if wxType1 == wxType2:
           return 1
        return 0

    def similarIntensities(self, tree, node, subkey1, subkey2):
        intenList = ['<NoInten>', '-', 'm']
        # If intensities are close enough
        inten1 = subkey1.intensity()
        inten2 = subkey2.intensity()
        if (inten1==inten2 or
            (inten1 in intenList and inten2 in intenList)):
            return 1
        return 0

    def similarCoverages(self, tree, node, subkey1, subkey2):
        # Return 0 if coverages of subkey1 and subkey2 are significantly
        # different
        # Return 1 if coverages are similar and the coverage of subkey1
        # is dominant.
        # Return 2 if coverages are similar and the coverage of subkey2
        # is dominant.
        cov1 = subkey1.coverage()
        cov2 = subkey2.coverage()
        for coverageList in self.similarCoverageLists(tree, node, subkey1, subkey2):
            if (cov1 in coverageList and cov2 in coverageList):
                index1 = coverageList.index(cov1)
                index2 = coverageList.index(cov2)
                if index1 >= index2:
                   #print 'checkWx return 1 - use subkey1'
                   return 1
                else:
                   #print 'checkWx return 2 - use subkey2'
                   return 2
        return 0

    def similarAttributeLists(self):
        # Lists weather attributes that can be combined or considered equal.
        # These lists are examined when producing rankLists,
        #    combining sub-phrases, and 
        #    determining if there is a local effect to report.
        # Used by
        #    PhraseBuilder:checkWeatherSimilarity
        #    SampleAnalysis: getDominantValues 
        return [
            ["DmgW", "GW"],
            ["LgA", "SmA"],
            ]
    
    def similarAttributes(self, tree, node, subkey1, subkey2):
        # If weather attributes are similar, return 1; otherwise return 0
        attrs1 = subkey1.attributes()
        attrs2 = subkey2.attributes()
        attrs1 = self.removeSpecialAttributes(attrs1)
        attrs2 = self.removeSpecialAttributes(attrs2)
        attrs1.sort()
        attrs2.sort()

        # If the lists are equal, they are similar.
        if attrs1 == attrs2:
            return True

        # Otherwise, check that each attribute for subkey1 matches
        # an attribute for subkey2 and vice versa
        if self.matchAttrs(attrs1, attrs2) and self.matchAttrs(attrs2, attrs1):
            return True
        else:
            return False

    def removeSpecialAttributes(self, attrs):
        rv = []
        for attr in attrs:
            if attr not in ["MX", "OR", "Mention", "Primary"]:
                rv.append(attr)
        return rv        

    def matchAttrs(self, attrs1, attrs2):
        for attr1 in attrs1:
            if not self.checkAttrs(attr1, attrs2):
                return False
        return True

    def checkAttrs(self, attr1, attrs2):
        # Check to see if there is a match for attr1 in attrs2
        # A "match" is equality OR there exists an attr2 in attrs2
        # such that both attr1 and attr2 are in one of the similarAttrsLists
        # E.g., "GW" is attr1, "DmgW" is attr2 and attrList is ["GW","DmgW"]
        if attr1 in attrs2:
            return True
        for attrList in self.similarAttributeLists():
            if attr1 in attrList:
                for attr2 in attrs2:
                    if attr2 in attrList:
                        return True
        return False


    # Consolidation
    def consolidatePhrase(self, tree, phrase):
        # See if ready to process
        if not self.phrase_trigger(tree, phrase, setUpOnly=1):
            return
        # Separate out primary elements that are constant throughout phrase
        elementInfoList = phrase.get("elementInfoList")
        if elementInfoList is None or len(elementInfoList) <= 1:
            return self.DONE()
        subPhrases = phrase.get("childList")
        if len(subPhrases) <= 1:
            return self.DONE()
        first = 1
        constants = []
        nonConstants = []
        constantFirst = 0
        disabled = phrase.get("disabledElements", [])
        if disabled is None:
            disabled = []
        for elementInfo in elementInfoList:
            if elementInfo.name in disabled:
                continue
            if elementInfo.primary:
                # Primary elements
                diffFlag = self.checkForDifferences(tree, phrase, elementInfo)
                #print "element", elementInfo.name, diffFlag
                if diffFlag == 0:
                    constants.append(elementInfo)
                    # If first element is in constant list,
                    # make that the first phrase when split
                    # else, make it the second phrase
                    if first:
                        constantFirst = 1
                else:
                    nonConstants.append(elementInfo)
            else:
                # Secondary elements remain with first element
                if constantFirst:
                    constants.append(elementInfo)
                else:
                    nonConstants.append(elementInfo)
            first = 0
        # Split off elements that are not in the same list as the first element
        #print "Constants", constantFirst
        #for eleInfo in constants:
        #    print eleInfo.name
        #print "NonConstants"
        #for eleInfo in nonConstants:
        #    print eleInfo.name
        if constantFirst:
            splitElements = nonConstants
            curElements = constants
        else:
            splitElements = constants
            curElements = nonConstants
        length = len(splitElements)
        if length > 0 and length < len(elementInfoList):
            self.splitPhrase(tree, phrase, curElements, splitElements)
        return self.DONE()

    def splitPhrase(self, tree, phrase, curElements, splitElements):
        # For each element in splitElements e.g. Swell2 or WindGust:
        #    set the current phrase disabledElements
        #     (to turn them off for the original phrase)
        #    add a new phrase for the split element
        # For each element left in the current phrase e.g. Swell or Wind,
        #    set the new phrase disabled elements
        #     (to turn them off in the new phrase)
        #
        disabledElements = []
        newDis = []
        for elementInfo in curElements:
            newDis.append(elementInfo.name)
        for elementInfo in splitElements:
            disabledElements.append(elementInfo.name)
            newPhrase = tree.addPhraseDef(phrase, elementInfo.phraseDef)
            newPhrase.set("disabledElements", newDis)
        currentNone = phrase.getAncestor("disabledElements")
        if currentNone is not None:
            phrase.set("disabledElements", disabledElements + currentNone)
        else:
            phrase.set("disabledElements", disabledElements)
        for key in ["spawnedWxPhrases", "conjunctiveQualifier",
                    "embeddedQualifier", "localEffect", "localEffectsList",
                    "firstElement", "elementName", "elementInfoList"]:
                    #"descriptor", "indentLabel"]:
            newPhrase.set(key, phrase.get(key))
        #print "\nSplitPhrase: New phrase", newPhrase, newDis
        #print "Current phrase disabled", phrase, phrase.get("disabledElements")

    def consolidateDirection(self, tree, phrase):
        # See if ready to process
        if not self.phrase_trigger(tree, phrase, setUpOnly=1):
            return
        # If vector direction is progressive and mags are similar,

        # use only first and last subPhrases
        elementInfoList = phrase.get("elementInfoList")
        if elementInfoList is None or len(elementInfoList) < 1:
            return self.DONE()
        subPhrases = phrase.get("childList")
        if len(subPhrases) <= 1:
            return self.DONE()
        firstElement = phrase.get("firstElement")
        diffFlag = self.checkForDifferences(tree, phrase, firstElement, magOnly=1)
        if diffFlag == 0:
            vectorStats = self.getSubStats(phrase, firstElement.name)
            dirList = []
            for mag, dir in vectorStats:
                dirList.append(dir)
            progression = self.checkProgression(dirList)
            if progression:
                childList = phrase.get("childList")
                new = []
                new.append(childList[0])
                new.append(childList[len(childList)-1])
                phrase.set("childList", new)
        return self.DONE()

    def consolidateTrends(self, tree, phrase):
        # See if we need to ignore this method
        if self.ignoreTrends(tree, phrase):
            return self.DONE()
        # See if ready to process
        if not self.phrase_trigger(tree, phrase, setUpOnly=1):
            return
        # If there is a progression of magnitudes,
        # use only first and last subPhrases with no time descriptor
        elementInfoList = phrase.get("elementInfoList")
        if elementInfoList is None or len(elementInfoList) < 1:
            return self.DONE()
        subPhrases = phrase.get("childList")
        if len(subPhrases) <= 2:
            return self.DONE()
        firstElement = phrase.get("firstElement")
        # If Vector, make sure directions are the same
        diffFlag = 0
        dataType = firstElement.dataType
        if dataType == self.VECTOR():
            diffFlag = self.checkForDifferences(tree, phrase, firstElement, dirOnly=1)
        # Check for an increasing or decreasing magnitude progression
        if diffFlag == 0:
            statList = self.getSubStats(phrase, firstElement.name)
            trend = self.checkTrend(statList, dataType)
            # If trend, take first and last children only
            if trend:
                childList = phrase.get("childList")
                new = []
                new.append(childList[0])
                new.append(childList[len(childList)-1])
                phrase.set("childList", new)
                # Turn off time descriptors
                phrase.set("noTimeDescriptors", 1)
        return self.DONE()

    def ignoreTrends(self, tree, node):
        if node.get('name') in ["windChill_phrase", "windBased_windChill_phrase",
                                "heatIndex_phrase", "apparentT_phrase"]:
            return 1
        else:
            return 0

    def checkProgression(self, dirList):
        # make a list of differences
        diffList = []
        for i in range(1, len(dirList)):
            diff = dirList[i] - dirList[i-1]  # calc difference
            # normalize the difference to remove the 359 -> 0 effect
            if diff > 180:
                diff = diff - 360
            elif diff < -180:
                diff = diff + 360
            diffList.append(diff)

        minVal = min(diffList)
        maxVal = max(diffList)

        # any diffs >= 90 not allowed
        if maxVal >= 90 or minVal <= -90:
            return 0

        # see if all the diff are of the same sign, if not return 0
        if minVal * maxVal < 0:
            return 0
        else:
            return 1

    def checkTrend(self, statList, dataType):
        # check to see if increasing/decreasing values
        lastMax = None
        trend = None
        for stats in statList:
            if dataType == self.VECTOR():
                stats, dir = stats
            min, max = self.getValue(stats, "MinMax")
            if lastMax is None:
                lastMax = max
            elif trend is None:
                trend = lastMax > max
                lastMax = max
            else:
                # Test for an decreasing trend
                if trend == 1 and lastMax > max:
                    lastMax = max
                    continue
                # Test for an increasing trend
                if trend == 0 and lastMax <= max:
                    lastMax = max
                    continue
                return 0
        return 1

    def chooseMostImportant(self, tree, phrase):
        # If there is more than 1 sub-phrase AND mostImportant_dict
        # is set, report only the "Min" or "Max" sub-phrase using
        # the "mostImportant_descriptor"
        if not self.phrase_trigger(tree, phrase, setUpOnly=1):
            return
        elementInfoList = phrase.get("elementInfoList")
        if elementInfoList is None or len(elementInfoList) < 1:
            return self.DONE()
        subPhrases = phrase.get("childList")
        if len(subPhrases) <= 1:
            return self.DONE()
        elementName = phrase.get("elementName")
        mostImportant = self.mostImportant(tree, phrase, elementName, elementName)
        if mostImportant is None:
            return self.DONE()

        # Find the index of the sub-phrase with the Min or Max value
        firstElement = phrase.get("firstElement")
        statList = self.getSubStats(phrase, elementName)
        dataType = firstElement.dataType
        for i in range(len(statList)):
            if dataType == self.VECTOR():
                stats, dir = statList[i]
            else:
                stats = statList[i]
            min, max = self.getValue(stats, "MinMax")
            if i == 0:
                if mostImportant == "Min":
                    importantVal = min
                else:
                    importantVal = max
                importantIndex = 0
            else:
                if mostImportant == "Min":
                    if min < importantVal:
                        importantVal = min
                        importantIndex = i
                else:
                    if max > importantVal:
                        importantVal = max
                        importantIndex = i
        # Null out the other sub-phrases
        for i in range(len(subPhrases)):
            if i != importantIndex:
                statDict = subPhrases[i].getStatDict()
                statDict[elementName] = None
                #print "subPhrase", subPhrases[i].getTimeRange()
        # Set up the mostImportant_descriptor
        descriptor = self.mostImportant_descriptor(
            tree, phrase, elementName, elementName)
        if descriptor is not None:
            phrase.set("descriptor", descriptor)
        return self.DONE()

    def getSubStats(self, phrase, elementName):
        # Return a list of stats for the subPhrases
        statList = []
        for subPhrase in phrase.childList:
            statDict = subPhrase.getStatDict()
            if statDict is None:
                continue
            statList.append(statDict[elementName])
        return statList

    def splitWxPhrase(self, tree, node, disabledSubkeys1, disabledSubkeys2, doneList,
                      newPhraseDef=None):
        # Set disableSubkeys1 for original node
        # Create a new node with disabledSubkeys2
        # Set new phrase doneList using "doneList"
        # Add to new phrase according to newPhraseDef if provided,
        #  otherwise, duplicate current node
        # Make sure to propagate "disabledSubkeys" and "spawnedWxPhrases"
        #  from the original node to the new node
        #print "\nSplit Wx Phrase: original node", node.get("name"), node.getAreaLabel()
        #print "   ", node
        #import traceback
        #traceback.print_stack(limit=3)
        #print "   localEffect", node.get('localEffect')
        #print "   parent", node.parent
        #tree.printNode(node)
        disabled = node.getAncestor("disabledSubkeys")
        if disabled is None:
            disabled = []
        disabledSubkeys1 = disabledSubkeys1 + disabled
        node.set("disabledSubkeys", disabledSubkeys1)
        if newPhraseDef is None:
            newPhrase = tree.addPhrase(node)
            disabledSubkeys2 = disabledSubkeys2 + disabled
        else:
            newPhrase = tree.addPhraseDef(node, newPhraseDef)
        newPhrase.set("disabledSubkeys", disabledSubkeys2)
        newPhrase.set("doneList", doneList)
        #print "   disabled", node.get("disabledSubkeys")
        #print "new node", newPhrase, newPhrase.get("name")
        #print "  parent", newPhrase.parent
        for key in ["spawnedWxPhrases", "conjunctiveQualifier",
                    "embeddedQualifier", "localEffect", "localEffectsList",
                    "firstElement", "elementName", "elementInfoList"]:
                    #"descriptor", "indentLabel"]:
            #print "    setting ", key, node.get(key)
            newPhrase.set(key, node.get(key))
        #print "   ", newPhrase.getAreaLabel()
        #print "   disabled", newPhrase.get("disabledSubkeys")
        #tree.printNode(newPhrase)
        return newPhrase

    ##    Combining
    ##      If you want to alter the combining criteria,
    ##      override starred (**) methods
    ##
    ##       combinePhraseStats     (phrase level)
    ##       combineWords           (phrase level)
    ##       combineComponentStats  (tree level)
    ##       combineWords           (any level)

    ##         combineChildren  -- loops through child nodes
    ##            (tree, node, combineMethod)

    ##           **combineScalars         (subPhrase1, subPhrase2)
    ##           **combineVectors         (subPhrase1, subPhrase2)
    ##           **combineWx              (subPhrase1, subPhrase2)
    ##           **combineComponents      (component1, component2)

    ##              combine2SubPhrases (tree, phrase, subPhrase1, subPhrase2)
    ##              combine2Components (tree, tree, component1, component2)

    ##           combineChildWords        (any node with children that has words)
    ##              combine2Children  (tree, node, child1, child2)

    def combinePhraseStats(self, tree, phrase):
        # See if ready to process
        if not self.phrase_trigger(tree, phrase, setUpOnly=1):
            return
        return self.combineChildren(tree, phrase, self.combineStats)
        #print "before combine ", phrase.get("elementName"), len(phrase.get("childList"))
        #result = self.combineChildren(tree, phrase, self.combineStats)
        #print "after combine ", phrase.get("elementName"), len(phrase.get("childList"))
        #return result

    def recallCombinePhraseStats(self, tree, phrase):
        # This is needed because we want to call combinePhraseStats twice in some phrases.
        # If we simply put in two calls to a method, it gets put on the "doneList" with
        # the first call and is not called again.
        # See if ready to process
        if not self.phrase_trigger(tree, phrase, setUpOnly=1):
            return
        return self.combinePhraseStats(tree, phrase)

    def combineComponentStats(self, tree, node):
        return self.combineChildren(tree, node, self.combineComponents)

    def combineWords(self, tree, node):
        # Check for data
        if not self.phrase_trigger(tree, node):
            return
        children = node.get("childList")
        if len(children) <= 1:
            return self.DONE()
        return self.combineChildren(tree, node, self.combineChildWords)

    def combineChildren(self, tree, node, combineMethod):
        # Combine similar nodes if possible
        length = len(node.childList)
        if length <= 1:
            return self.DONE()
        index = 1
        while index < len(node.childList):
            # Try to combine with previous subPhrase
            combineFlag, combinedChild  = combineMethod(
                tree, node, node.childList[index-1], node.childList[index])
            if combineFlag:
                # Reset childList for phrase
                node.childList[index-1] = combinedChild
                del node.childList[index]
            else:
                index = index + 1
        return self.DONE()

    def combineComponents(self, tree, node, component1, component2):
        # Criteria to set combine_flag

        #print "\nTrying to combine"
        # Don't combine components with different names since their
        # analysis lists could be different.
        comp1Name = component1.get("name")
        comp2Name = component2.get("name")
        if comp1Name != comp2Name:
            #print "Different component names", comp1Name, comp2Name
            return 0, None

        # Make sure we don't combine periods any earlier than we should
        noCombineUntil = self.periodCombining_startHour(tree, node)
        if self.hoursPastProductStart(tree, component1) <= noCombineUntil:
            return 0, None

        ## call the "similar" methods to see if each element is roughly
        ## the same.  Any element that is not similar will cause the
        ## combine flag to evaluate to 0 or false.
        elements = self.periodCombining_elementList(tree, component1)
        combine_flag = 1
        for element in elements:
            #print "trying to combine", element
            exec "combine_flag = combine_flag and self.similar"+element+\
                 "(tree, component1, component2)"
            #print "result", combine_flag

        if combine_flag:
            #print "combining"
            newComp = self.combine2Components(tree, tree, component1, component2)
            return 1, newComp
        return 0, None

    def similarWind(self, tree, comp1, comp2):
        # Returns true if the wind stats are similar
        # Also, return true (combine) if past the first 5 period since
        # wind is not reported in these periods

        # these numbers determine if components are close enough to combine
        magThreshold = 10
        dirThreshold = 45

        al1 = comp1.getAreaLabel()
        al2 = comp2.getAreaLabel()
        tr1 = comp1.getTimeRange()
        tr2 = comp2.getTimeRange()
        stats1 = tree.stats.get("Wind", tr1, al1, mergeMethod = "Average")
        stats2 = tree.stats.get("Wind", tr2, al2, mergeMethod = "Average")

        # If past the first 5 periods, return 1 (combine)
        hours = self.hoursPastProductStart(tree, comp1)
        if hours >= 5*12:
            return 1

        # check for none
        if stats1 is None or stats2 is None:
            return 0

        mag1 = stats1[0]
        mag2 = stats2[0]
        dir1 = stats1[1]
        dir2 = stats2[1]
        # calculate the differences, mag and dir
        magDiff = abs(mag1 - mag2)
        dirDiff = abs(dir1 - dir2)

        # account for the 360 to 0 problem
        if dirDiff > 180:
            dirDiff = abs(dirDiff - 360.0)

        if magDiff <= magThreshold and dirDiff <= dirThreshold:
            return 1

        return 0

    def hoursPastProductStart(self, tree, node):
        # Compute the hours past the product start time (prodTR)
        # that the current time range (curTR) starts.
        # If the prodTR is not a multiple of 12, then it is either
        #    --an update and the first period is less than 12 hours, or
        #    --a pre-first period issuance.
        # In these case, we return the hours past the product start
        # as if the first period was a full 12-hour period.
        # For example,
        # A morning update issuance starting at 10 am would
        # have an hoursPastProductStart for the first period
        # of 4 hours.
        # A pre-first period issuance starting at 4 am would
        # have an hoursPastProductStart for the first period
        # of -2 hours.
        prodTR = tree.getTimeRange()
        curTR = node.getTimeRange()
        prodHours = prodTR.duration()/3600
        prodMod = prodHours%12
        if prodMod > 0:
            try:
                # check for 'pre-first period issuances'
                period1Hours = self._issuanceInfo.period1TimeRange().duration()/3600
                if period1Hours > 12:
                    adjustHours = prodMod
                else:
                    adjustHours = -(12-prodMod)
            except:
                adjustHours = 0
        else:
            adjustHours = 0
        prodStart = prodTR.startTime() + adjustHours*3600
        return (curTR.startTime() - prodStart)/3600

    def similarSky(self, tree, comp1, comp2):
        # Returns true if sky stats are similar
        # Necessary because of the override to sky_valueList above
        al1 = comp1.getAreaLabel()
        al2 = comp2.getAreaLabel()
        tr1 = comp1.getTimeRange()
        tr2 = comp2.getTimeRange()
        return self.similarSkyLogic(tree, comp1, comp2, tr1, al1, tr2, al2)

    def similarWx(self, tree, comp1, comp2):
        # Returns true if wx stats are similar
        al1 = comp1.getAreaLabel()
        al2 = comp2.getAreaLabel()
        tr1 = comp1.getTimeRange()
        tr2 = comp2.getTimeRange()
        return self.similarWxLogic(tree, comp1, comp2, tr1, al1, tr2, al2)

    def similarPoP(self, tree, comp1, comp2):
        # returns true if PoP stats are similar
        stats1 = self.matchToWx(tree, comp1, "PoP")
        stats2 = self.matchToWx(tree, comp2, "PoP")

        if stats1 is None and stats2 is None:
            return 1

        # check for none
        #if stats1 is None or stats2 is None:
        #    return 0

        if stats1 == stats2:
            return 1

        if stats1 < self.pop_lower_threshold(tree, comp1) and \
               stats2 < self.pop_lower_threshold(tree, comp2):
            return 1

        if stats1 > self.pop_upper_threshold(tree, comp1) and \
               stats2 > self.pop_upper_threshold(tree, comp2):
            return 1

        return 0


    ##    Submitted by Brian Walawender  3/05
    ##    The problem with combining long time periods, is that the
    ##    combined period is growing 12 hours at a time.  If you get rid of the bleed
    ##    over grids for MinT and MaxT (SampleAnalysis temporalCoverage_hours_dict),
    ##    then you start returning None for either MaxT or MinT during these 12 hour periods.
    ##    To combat this, I check the duration of tr1 and tr2.
    ##    If it is 12 or less then I check to see if it is day or night.
    ##    For MaxT, it will return a combine if the period is 12 hours or less and it is a
    ##    nighttime period.
    ##    For MinT, it will return a combine if the period is 12 hours
    ##    or less and it is a daytime period.  This allowed long periods to be grouped
    ##    together without bleed over.

    def similarMaxT(self, tree, comp1, comp2):
        # returns true if temp stats are similar

        # this number determines if components are close enough to combine
        tempThreshold = 5  # degrees

        al1 = comp1.getAreaLabel()
        al2 = comp2.getAreaLabel()
        tr1 = comp1.getTimeRange()
        tr2 = comp2.getTimeRange()

        hours = (tr2.endTime()-tr1.startTime())/3600
        if hours <= 24:
            return  1

        if (tr1.duration()/3600) <= 12:
            dayNight = self.getPeriod(tr1, 1)
            if dayNight == self.NIGHTTIME():
                return 1

        if (tr2.duration()/3600) <= 12:
            dayNight = self.getPeriod(tr2, 1)
            if dayNight == self.NIGHTTIME():
                return 1

        stats1 = tree.stats.get("MaxT", tr1, al1, mergeMethod = "Average")
        stats2 = tree.stats.get("MaxT", tr2, al2, mergeMethod = "Average")
        # check for none
        if stats1 is None or stats2 is None:
            return 0

        if abs(stats1 - stats2) < tempThreshold:
            return 1

        return 0

    def similarMinT(self, tree, comp1, comp2):
        # returns true if temp stats are similar

        # this number determines if components are close enough to combine
        tempThreshold = 5  # degrees
        al1 = comp1.getAreaLabel()
        al2 = comp2.getAreaLabel()
        tr1 = comp1.getTimeRange()
        tr2 = comp2.getTimeRange()
        hours = (tr2.endTime()-tr1.startTime())/3600
        if hours <= 24:
            return  1

        if (tr1.duration()/3600) <= 12:
            dayNight = self.getPeriod(tr1, 1)
            if dayNight == self.DAYTIME():
                return 1

        if (tr2.duration()/3600) <= 12:
            dayNight = self.getPeriod(tr2, 1)
            if dayNight == self.DAYTIME():
                return 1

        # check for none
        stats1 = tree.stats.get("MinT", tr1, al1, mergeMethod = "Average")
        stats2 = tree.stats.get("MinT", tr2, al2, mergeMethod = "Average")

        if stats1 is None or stats2 is None:
            return 0

        if abs(stats1 - stats2) < tempThreshold:
            return 1

        return 0


    def similarWaveHeight(self, tree, comp1, comp2):
        # returns true if seas stats are similar

        # this number dtermines if components are close enough to combine
        seaThreshold = 4  # feet

        al1 = comp1.getAreaLabel()
        al2 = comp2.getAreaLabel()
        tr1 = comp1.getTimeRange()
        tr2 = comp2.getTimeRange()
        stats1 = tree.stats.get("WaveHeight", tr1, al1, mergeMethod ="Average")
        stats2 = tree.stats.get("WaveHeight", tr2, al2, mergeMethod ="Average")

        # check for none
        if stats1 is None or stats2 is None:
            return 0

        if stats1 == None or stats2 == None:
            return 0

        if abs(stats1 - stats2) < seaThreshold:
            return 1
        return 0

    def similarDiurnalSkyWx(self, tree, comp1, comp2):
        return self.similar_diurnal(tree, comp1, comp2, ["Sky", "Wx"])

    def similar_diurnal(self, tree, comp1, comp2, elementList):
        # Returns true if stats for the given elements are similar
        # in the night and morning AND the afternoon and evening.
        # NOTE: the night and morning MAY be similar to the afternoon
        # and evening, so word methods need to test for this case.
        #
        # Meant to handle the case of clouds and fog in the
        # night and morning clearing in the afternoon and
        # evening.
        # Assumes comp2 is a 12-hour period.

        #print "similar_diurnal"
        al1 = comp1.getAreaLabel()
        al2 = comp2.getAreaLabel()
        comp1TR = comp1.getTimeRange()
        comp2TR = comp2.getTimeRange()
        # comp2 morning, comp2 afternoon OR
        # comp2 evening, comp2 night
        c2tr1, c2tr2 = self.divideRange(comp2TR,6)
        comparisons = []
        if comp1TR.duration() == 12*3600:
            # Compare comp1 night to comp2 morning
            #     and comp1 evening to comp2 afternoon
            #  OR     comp1 afternoon to comp2 evening
            #     and comp1 morning to comp2 night
            c1tr1, c1tr2 = self.divideRange(comp1TR,6)
            comparisons.append((c1tr1, c2tr2))
            comparisons.append((c1tr2, c2tr1))
        else:
            # We have already combined at least once so
            # comp1 is at least 24 hours. Use the most
            # recent 24 hours for comparison.
            # if comp2 is daytime:
            #     compare comp1 morning to comp2 morning
            #         and comp1 afternoon to comp2 afternoon
            # else
            #     compare comp1 evening to comp2 evening
            #         and comp1 night to comp2 night
            subRanges = self.divideRange(comp1TR, 6)
            length = len(subRanges)-1
            c1tr1 = subRanges[length-3]
            c1tr2 = subRanges[length-2]
            comparisons.append((c1tr1, c2tr1))
            comparisons.append((c1tr2, c2tr2))

        # Do comparisons
        wordDict = {}
        for element in elementList:
            wordDict[element] = []
        #print "\nComparisons"
        for tr1, tr2 in comparisons:
            for element in elementList:
                #print "comparing", tr1, tr2
                #print "    ", element
                exec "flag = self.similar"+element+\
                 "Logic(tree, comp1, comp2, tr1, al1, tr2, al2)"
                #print "flag", flag
                if not flag:
                    #print "returning 0"
                    return 0
        #print "returning 1"
        return 1

    def similarSkyLogic(self, tree, comp1, comp2, tr1, al1, tr2, al2):
        stats1 = tree.stats.get("Sky", tr1, al1, mergeMethod ="Average")
        stats2 = tree.stats.get("Sky", tr2, al2, mergeMethod ="Average")
        # check for none
        #print "stats1", stats1
        #print "stats2", stats2
        if stats1 is None or stats2 is None:
            return 0
        if stats1 == None or stats2 == None:
            return 0
        saveTR1 = comp1.timeRange
        saveTR2 = comp2.timeRange
        comp1.timeRange = tr1
        comp2.timeRange = tr2
        words1 = self.sky_value(tree, comp1, self.getValue(stats1), -1)
        words2 = self.sky_value(tree, comp2, self.getValue(stats2), -1)
        comp1.timeRange = saveTR1
        comp2.timeRange = saveTR2
        #print "words1, words2", words1, words2
        if words1 == words2:
            return 1
        #if words1.find("partly") > -1 and words2.find("partly")> -1:
        #    return 1
        return 0

    def similarWxLogic(self, tree, comp1, comp2, tr1, al1, tr2, al2):
        # Returns true if wx stats are similar
        stats1 = tree.stats.get("Wx", tr1, al1, mergeMethod = "Average")
        stats2 = tree.stats.get("Wx", tr2, al2, mergeMethod = "Average")
        # check for none
        #print "stats1, stats2", stats1, stats2
        if stats1 is None or stats2 is None:
           return 0
        stats1 = self.cleanOutNoWx(stats1)
        stats2 = self.cleanOutNoWx(stats2)
        similarWx = self.checkWeatherSimilarity(
            tree, comp1, stats1, stats2, comp1, comp2, tr1, tr2, al1, al2)
        #print "similarWx", similarWx
        if similarWx == 0:
            return 0
        else:
            return 1

    def cleanOutNoWx(self, stats):
        # Cleans out NoWx from stats list
        if stats is None:
            return None
        newList = []
        for stat in stats:
            if type(stat) is types.TupleType:
                subkey, rank = stat
            else:
                subkey = stat
            if subkey.wxType() == "<NoWx>":
                continue
            newList.append(stat)
        return newList

    def combineStats(self, tree, phrase, subPhrase1, subPhrase2):
        firstElement = phrase.get("firstElement")
        elementName = firstElement.name
        dataType = firstElement.dataType
        if dataType == self.SCALAR():
            combineFlag, newVal = self.combineScalars(
                tree, phrase, subPhrase1, subPhrase2, elementName)
        elif dataType == self.VECTOR():
            combineFlag, newVal = self.combineVectors(
                tree, phrase, subPhrase1, subPhrase2, elementName)
        elif dataType == self.WEATHER():
            combineFlag, newVal = self.combineWeather(
                tree, phrase, subPhrase1, subPhrase2, elementName)
        elif dataType == self.DISCRETE():
            combineFlag, newVal = self.combineDiscrete(
                tree, phrase, subPhrase1, subPhrase2, elementName)
        if combineFlag:
            elementInfoList = phrase.get("elementInfoList")
            newSubPhrase = self.combine2SubPhrases(
                tree, phrase, subPhrase1, subPhrase2, elementInfoList, newVal)
            return 1, newSubPhrase
        else:
            return 0,  None

    def combineScalars(self, tree, node, subPhrase1, subPhrase2, elementName):
        min1, max1 = self.getScalarData(tree, subPhrase1, elementName, "MinMax")
        min2, max2 = self.getScalarData(tree, subPhrase2, elementName, "MinMax")
        #print "combining", min1, max1, min2, max2
        if min1 is None and max1 is None and min2 is None and max2 is None:
            return 1, None
        if min1 is None or max1 is None or min2 is None or max2 is None:
            return 0, None

        differenceFlag = self.checkScalarDifference(
            tree, subPhrase1, elementName, min1, max1, min2, max2)
        if differenceFlag == 0:
            combine_singleValues = self.combine_singleValues_flag(
                tree, subPhrase1, elementName, elementName)
            if combine_singleValues == 1:
                newValue = self.average(min(min1, min2), max(max1, max2))
                newValue = self.roundStatistic(tree, subPhrase1, newValue, elementName)
            else:
                # Combine using mins and maxs to catch slow trends
                min1 = self.roundStatistic(tree, subPhrase1, min(min1, min2), elementName)
                max1 = self.roundStatistic(tree, subPhrase1, max(max1, max2), elementName)
                min1, max1 = self.applyRanges(tree, node, min1, max1, elementName)
                newValue = (min1, max1)
            #print "combined"
            return 1, newValue
        #print "not combined"
        return 0,  None

    def combineVectors(self, tree, phrase, subPhrase1, subPhrase2, elementName):
        mag1, dir1, dirStr1 = self.getVectorData(tree, subPhrase1, elementName, "MinMax")
        mag2, dir2, dirStr2 = self.getVectorData(tree, subPhrase2, elementName, "MinMax")
        if mag1 is None and mag2 is None:
            return 1, (None, dir1)
        if mag1 is None or mag2 is None:
            return 0, (None, dir1)

        min1, max1 = mag1
        min2, max2 = mag2

        differenceFlag = self.checkVectorDifference(
            tree, subPhrase1, elementName, min1, max1, dir1, min2, max2, dir2)
        if differenceFlag == 0:
            combine_singleValues = self.combine_singleValues_flag(
                tree, subPhrase1, elementName, elementName)
            if combine_singleValues == 1:
                newMag, newDir = self.vectorAverage((min(min1, min2), dir1), (max(max1, max2), dir2))
                newMag = self.roundStatistic(tree, subPhrase1, newMag, elementName)
                newValue = (newMag, newDir)
            else:
                # Combine using mins and maxs to catch slow trends
                newMin = min(min1, min2)
                newMax = max(max1, max2)
                newMin, newMax = self.applyRanges(tree, phrase, newMin, newMax, elementName)
                magAvg, newDir = self.vectorAverage((newMin, dir1), (newMax, dir2))
                newValue = ((newMin, newMax), newDir)
            return 1, newValue
        return 0,  None

    def combineWeather(self, tree, phrase, subPhrase1, subPhrase2, elementName):
        # This method now only used for skyPopWx and visibility phrases
        statDict1 = subPhrase1.getStatDict()
        stats1 = statDict1[elementName]
        statDict2 = subPhrase2.getStatDict()
        stats2 = statDict2[elementName]
        if stats1 is None and stats2 is None:
            return 1, None
        if stats1 is None or stats2 is None:
            return 0, None

        subkeys1 = self.getSubkeys(stats1)
        subkeys2 = self.getSubkeys(stats2)
        # Special case of combining based only on Visibility
        combineVisibility = phrase.get("combineVisibility")
        if combineVisibility == 1:
            # Combine if low visibility is the same for each subPhrase
            lowVis1 = self.getVis(subkeys1)
            lowVis2 = self.getVis(subkeys2)
            if lowVis1 == lowVis2:
                return 1, stats1
            else:
                return 0, None

        # Check weather key differences
        similarResult = self.checkWeatherSimilarity(
            tree, phrase, stats1, stats2, subPhrase1, subPhrase2)
        if type(similarResult) is types.ListType:
            return 1, similarResult
        elif similarResult == 1:
            return 1, stats1
        elif similarResult == 2:
            return 1, stats2
        else:
            return 0,  None

    def combineDiscrete(self, tree, phrase, subPhrase1, subPhrase2, elementName):
        statDict1 = subPhrase1.getStatDict()
        stats1 = statDict1[elementName]
        statDict2 = subPhrase2.getStatDict()
        stats2 = statDict2[elementName]
        if stats1 is None and stats2 is None:
            return 1, None
        if stats1 is None or stats2 is None:
            return 0, None

        if stats1 == stats2:
            return 1, stats1
        return 0,  None

    def combineChildWords(self, tree, node, child1, child2):
        words1 = child1.get("words")
        if words1 is None:
            return 0, None
        words2 = child2.get("words")
        if words2 is None:
            return 0, None

        if words1 == words2:
            newChild = self.combine2Children(tree, node, child1, child2)
            return 1, newChild
        return 0,  None

    def combine2SubPhrases(self, tree, node, subPhrase1, subPhrase2, elementInfoList, newVal):
        # Combine time ranges
        subRange1 = subPhrase1.get("timeRange")
        subRange2 = subPhrase2.get("timeRange")
        newTimeRange = TimeRange.TimeRange(subRange1.startTime(), subRange2.endTime())

        # Make new Node so methods will be re-run
        # Preserve other elements in statDict
        newSubPhrase = tree.makeNode([], subPhrase1.methodList)
        # Make new statDict based on new time range
        first = elementInfoList[0]
        statDict = {}
        statDict[first.name] = newVal
        areaLabel = node.getAreaLabel()
        for elementInfo in elementInfoList[1:]:
            stats = tree.stats.get(
                elementInfo.name, newTimeRange, areaLabel, elementInfo.statLabel,
                elementInfo.mergeMethod)
            statDict[elementInfo.name] = stats
        newSubPhrase.set("statDict", statDict)
        newSubPhrase.set("timeRange", newTimeRange)
        newSubPhrase.parent = node
        return newSubPhrase

    def combine2Components(self, tree, node, comp1, comp2):
        # Combine time ranges
        timeRange1 = comp1.getTimeRange()
        timeRange2 = comp2.getTimeRange()
        newTimeRange = TimeRange.TimeRange(timeRange1.startTime(), timeRange2.endTime())
        # Get fresh component definition so methods will be re-run
        newComp = tree.makeComponent(comp1.get("name"), newTimeRange, comp1.get("definition"))
        newComp.parent = tree
        return newComp

    def combine2Children(self, tree, node, child1, child2):
        # Combine time ranges and take same values so methods will not be re-run
        # Used for combining words
        timeRange1 = child1.get("timeRange")
        timeRange2 = child2.get("timeRange")
        newTimeRange = TimeRange.TimeRange(timeRange1.startTime(), timeRange2.endTime())
        child1.set("timeRange", newTimeRange)
        return child1

    ####################################

    def fillNulls(self, tree, node):
        # Data Needed: subPhrase "words"
        # Fill in the subPhrases designated as "null" with configurable
        # null phrases (first_null_phrase, null_phrase)

        # See if ready to process
        if not self.phrase_trigger(tree, node):
            return

        index = 0
        #print "fillNulls node", node.get("firstElement"), node.get("elementName")
        try:
            elementName = node.get("firstElement").name
        except:
            return self.DONE()
        firstNullPhrase = self.first_null_phrase(tree, node, elementName, elementName)
        nullPhrase = self.null_phrase(tree, node, elementName, elementName)
        for subPhrase in node.get("childList"):
            words = subPhrase.get("words")
            if words is None:
                return
            if words == "null":
                if index == 0:
                    subPhrase.set("words", firstNullPhrase)
                else:
                    subPhrase.set("words", nullPhrase)
                subPhrase.set("null",1)
            index = index + 1
        # Collapse empty word sub-phrases
        self.collapsePhraseWords(tree, node)
        return self.DONE()

    def collapsePhraseWords(self, tree, phrase):
        # Collapse empty word sub-phrases
        childList = phrase.childList
        if len(childList) <= 1:
            return
        newList = []
        lastWords = None
        index = 0
        emptyIndex = None
        for subPhrase in phrase.childList:
            subWords = subPhrase.get("words")
            if subWords == "":
                if lastWords == "":
                    # Add to empty phrase
                    subRange = subPhrase.getTimeRange()
                    emptyRange = childList[emptyIndex].getTimeRange()
                    newRange = TimeRange.TimeRange(emptyRange.startTime(), subRange.endTime())
                    childList[emptyIndex].set("timeRange", newRange)
                else:
                    # Start an empty phrase
                    emptyIndex = index
            else:
                if lastWords == "":
                    newList.append(childList[emptyIndex])
                    emptyIndex = None
                newList.append(subPhrase)
            lastWords = subWords
            index = index + 1
        if emptyIndex is not None:
            newList.append(childList[emptyIndex])
        phrase.childList = newList


    def timeDescriptorModeration(self, tree, phrase):
        # Moderates the time descriptor
        # Needs subPhrase "words"
        # Looks at subPhrase "null" or empty
        # Sets subPhrase "timeDescFlag" indicating whether or not
        #  to generate a timeDescriptor for this subPhrase
        #
        # Algorithm:
        #  if last subPhrase is null, (make sure to flag last non-null)
        #     If odd number of subPhrases,
        #         flag even else flag odd
        #  elif the first even subPhrase is null, flag odd subPhrases
        #  else, flag even subPhrases
        #
        # See if ready to process
        if not self.phrase_trigger(tree, phrase):
            return
        childList = phrase.get("childList")
        length = len(childList)
        if length == 0:
            return self.DONE()
        # Set all subPhrases if time descriptors are always
        # to be on OR off
        flag = None
        if phrase.get("noTimeDescriptors") == 1:
            flag = 0
        elif phrase.get("allTimeDescriptors") == 1:
            flag = 1
        if flag is not None:
            for subPhrase in childList:
                subPhrase.set("timeDescFlag", flag)
            return self.DONE()
        # If one subPhrase, we need time descriptor IF
        # the subPhrase time range differs from the
        # phrase time range
        if length == 1:
            subPhrase = childList[0]
            if subPhrase.getTimeRange() == phrase.getTimeRange():
                flag = 0
            else:
                flag = 1
            subPhrase.set("timeDescFlag",flag)
            return self.DONE()
        odd = length%2
        lastNull = self.isNull(childList[length-1])
        if lastNull:
            if odd:
                flagOdd = 0
            else:
                flagOdd = 1
        else:
            firstEven = self.isNull(childList[1])
            if firstEven:
                flagOdd = 1
            else:
                flagOdd = 0

        index = 0
        for subPhrase in childList:
            #print "words", subPhrase.get("words")
            #print "null", subPhrase.get("null")
            flag = 0
            if index%2 == 0: # odd subPhrase
                if flagOdd == 1:
                    flag = 1
            else:            # even subPhrase
                if flagOdd == 0:
                    flag = 1

            # Uncomment the following line if you want ALL sub-phrases
            # to have a time descriptor.
            #
            #flag = 1
            #
            # Alternatively, you could test per weather element:
            #
            #if phrase.get("elementName") == "Wx":
            #    flag = 1

            subPhrase.set("timeDescFlag", flag)
            index = index + 1
        return self.DONE()

    def isNull(self, subPhrase):
        if subPhrase.get("null") == 1:
            return 1
        if subPhrase.get("words") == "":
            return 1
        return 0

    def checkPhrasesDone(self, tree, node, areaLabel=None, exceptions=[]):
        # Check that all phrases (except those with names listed in exceptions)
        #    are done for the component associated with node
        # If areaLabel is not None, check only those phrases that have
        #    the given areaLabel.
        # Return the list of phrases that are done.

        # We need to look at all progeny -- not just children
        # since phrases can have child phrases
        leaves = self.getLeaves(tree, node)
        phraseList = []
        for child in leaves:
            childWords = child.get("words")
            childName = child.getAncestor('name')
            #print " child", childName, childWords
            if childName is None or childName in exceptions:
                continue
            if areaLabel is not None:
                if child.getAreaLabel() != areaLabel:
                    continue
            if childWords is not None:
                phraseList.append(child)
            else:
                # If no words yet, return
                #print "returning to wait"
                return None
        if phraseList is []:
            return None
        else:
            return phraseList

    def phrase_trigger(self, tree, phrase, setUpOnly=0):
        # Return 1 if trigger is met, else 0
        # If setUpOnly == 1, trigger will be met if setUp method
        #   has been completed
        # Make sure set-up method was completed
        #if len(phrase.get("childList")) == 0 and phrase.get("words") is None:
        #    return 0
        if not phrase.setUpMethod in phrase.doneList:
            return 0
        if setUpOnly:
            return 1
        # Make sure sub-phrases have words
        for subPhrase in phrase.get("childList"):
            # Check to make sure we have words
            words = subPhrase.get("words")
            if words is None:
                return 0
        return 1

    def assembleSubPhrases(self, tree, phrase):
        # Assembles sub-phrases adding the time descriptor
        # Check for data

        # See if ready to process
        if not self.phrase_trigger(tree, phrase):
            return
        if not self.consolidateSubPhrases_trigger(tree, phrase):
            return

        #print "NODE", phrase.get("name"), phrase.getTimeRange()
        if self.useUntilPhrasing(tree, phrase):
            return self.assembleUntilSubPhrases(tree, phrase)

        fcst = ""
        index = 0

        #print "\nAssemble Subphrases", phrase.get('name'), phrase

        for subPhrase in phrase.get("childList"):
            # Check to make sure we have words
            words = subPhrase.get("words")
            if words is None:
                return
            #print "  words", words
            #print "     ", subPhrase.getTimeRange(), subPhrase
            #print "     ", subPhrase.getAncestor("conjunctiveQualifier")
            #print "     ", subPhrase.getAreaLabel()
            if words == "":
                continue

            if index == 0:
                #if not subPhrase.get("null"):
                if not self.isNull(subPhrase):
                    # Get descriptor
                    descriptor = phrase.get("descriptor")
                    if descriptor is not None and descriptor != "":
                        fcst = fcst + descriptor + " "
            else:
                # Get connector
                connectorMethod = phrase.get("connectorMethod")
                connector = connectorMethod(tree, subPhrase)

                if index == 2:
                    # Add conjunctive "THEN" to make 3+ subPhrase phrases
                    # flow better. e.g.
                    # "N WIND 10 TO 20 KT RISING TO 30 KT EARLY IN THE
                    # AFTERNOON...THEN RISING TO GALES TO 40 KT LATE
                    # IN THE AFTERNOON."
                    elementName = phrase.getAncestor("elementName")
                    useThenConnector = self.useThenConnector(
                        tree, phrase, elementName, elementName)
                    if useThenConnector:
                        thenConnector = self.thenConnector(
                            tree, phrase, elementName, elementName)
                        if thenConnector != "":
                            # Add another time descriptor
                            subPhrase.set("timeDescFlag", 1)
                        connector = thenConnector + connector

                fcst = fcst + connector

            # Time Descriptor
            timeDescriptor = self.format(
                self.subPhrase_timeDescriptor(tree, phrase, subPhrase))

            # Get words again in case they were changed by connector method
            fcst = fcst + subPhrase.get("words") + timeDescriptor
            index = index + 1
        #print "  words", fcst
        phrase.set("words", fcst)
        return self.DONE()

    def assembleUntilSubPhrases(self, tree, phrase):
        # Create a phrase that reports a list of (value, timeRange)
        # tuples. Optionally, an associated range may be added to the
        # phrase values.

        elementName = phrase.getAncestor("elementName")
        untilFormat = self.untilPhrasing_format(tree, phrase, elementName, elementName)
        timeRange = phrase.getTimeRange()

        # Make lists of consecutive subphrases
        phraseLists = []
        curList = []
        lastTR = None
        for subPhrase in phrase.get("childList"):
            tr = subPhrase.getTimeRange()
            # First time thru -- start curList
            if lastTR is None:
                curList.append(subPhrase)
                lastTR = tr
                continue
            # Check for consecutive sub ranges
            if tr.startTime() == lastTR.endTime():
                curList.append(subPhrase)
            # If not consecutive, clear out curList
            # and append current subPhrase
            else:
                if curList != []:
                    phraseLists.append(curList)
                    curList = []
                curList.append(subPhrase)
            lastTR = tr
        if curList != []:
            phraseLists.append(curList)

        #print "\nUNTIL NODE", phrase.getTimeRange()
        phrases = []
        for phraseList in phraseLists:
            words = ""
            index = 0
            subWords = ""
            firstWords = 1
            for subPhrase in phraseList:
                # Check to make sure we have words
                lastWords = subWords
                subWords = subPhrase.get("words")
                if subWords is None:
                    return
                #print "  words", subWords, subPhrase.getTimeRange()

                if index == 0:
                    #if not subPhrase.get("null"):
                    if not self.isNull(subPhrase):
                        # Get descriptor
                        descriptor = phrase.get("descriptor")
                        if descriptor is not None and descriptor != "":
                            words = words + descriptor + " "
                index += 1
                if subWords == "":
                    continue
                subRange = subPhrase.getTimeRange()
                # Add connector ...then if words came before this
                if not firstWords:
                    words = words + "...then "
                # Use after if lastWords were empty and subRange
                # starts after timeRange
                words = words + subWords
                if lastWords == "":
                    if subRange.startTime() != timeRange.startTime():
                        afterTime = self.getTimeStr(untilFormat, subRange, "begin")
                        if afterTime == "0000":
                            afterTime = "2400"
                        words = words + " after " + afterTime
                # Use until if the subRange ends before the time range
                if subRange.endTime() < timeRange.endTime():
                    untilTime = self.getTimeStr(untilFormat, subRange, "end")
                    if untilTime == "0000":
                        untilTime = "2400"
                    words = words +  " until " + untilTime
                firstWords = 0

            phrases.append(words)

        # String together phrases and insert periods
        index = 0
        words = ""
        for str in phrases:
            words = words + str
            if index < len(phrases)-1:
                if phrases[index+1] != "":
                    if not words == "":
                        words = words + ". "
            index += 1

        return self.setWords(phrase, words)

    def getTimeStr(self, format, timeRange, endBegin):
        if endBegin == "end":
            if format == "military":
                return self.timeDisplay(timeRange, "LT","","","%H")+"00"
            else:
                str =  self.timeDisplay(timeRange, "LT", "", "", "%I %p")
                if str[0] == "0":
                    str = str[1:]
                return str
        else:
            if format == "military":
                return self.timeDisplay(timeRange, "LT","","%H","")+"00"
            else:
                str =  self.timeDisplay(timeRange, "LT", "", "%I %p", "")
                if str[0] == "0":
                    str = str[1:]
                return str

    def useUntilPhrasing(self, tree, phrase):
        # Check to see if the subPhrases warrant "until" phrasing
        # Can be set for the phrase
        elementName = phrase.getAncestor("elementName")
        if self.untilPhrasing_flag(tree, phrase, elementName, elementName):
            return 1
        elif self.onTheFly_untilPhrasing_flag(
                tree, phrase, elementName, elementName) != 1:
            return 0
        # Examine sub-phrase time ranges
        tr = phrase.getTimeRange()
        timeStart = tr.startTime()
        timeEnd = tr.endTime()
        for subPhrase in phrase.get("childList"):
            #print "subTimeRange", subPhrase.getTimeRange()
            subTr = subPhrase.getTimeRange()
            subStart = subTr.startTime()
            subEnd = subTr.endTime()
            # See if subRange end time or start time is
            # not a multiple of 3 hours back from timeRange end time.
            # If start time is the same as phrase start time,
            # do not count as until phrasing.
            if timeEnd != subEnd:
                timeDiff = timeEnd - subEnd
                #print timeDiff, timeDiff % (3*3600)
                if timeDiff % (3*3600) != 0:
                    return 1
            if timeStart != subStart:
                timeDiff = timeEnd - subEnd
                if timeDiff % (3*3600) != 0:
                    return 1
        return 0

    def format(self, str):
        if str is None:
            str = ""
        str = self.addSpace(str, "leading")
        return str

    def subPhrase_timeDescriptor(self, tree, phrase, subPhrase):
        if subPhrase.get("timeDescFlag"):
            subRange = subPhrase.getTimeRange()
            phraseRange = phrase.getTimeRange()
            if phrase.get("name") in self.weatherPhraseNames(tree, phrase) and \
                   len(phrase.get("childList")) > 1 and subRange == phraseRange:
                dayNight = self.getPeriod(phraseRange, 1)
                elementName = phrase.get("elementName")
                if dayNight == self.DAYTIME():
                    return self.phrase_descriptor(
                        tree, phrase, "through the day", elementName)
                elif dayNight == self.NIGHTTIME():
                    return self.phrase_descriptor(
                        tree, phrase, "through the night", elementName)
            else:
                return self.timePeriod_descriptor(tree, phrase, subRange)
        else:
            return ""

    # Connectors
    def scalarConnector(self, tree, subPhrase):
        # return connector phrase to connect subPhrase and previous one
        elementName = subPhrase.getAncestor("firstElement").name
        then = self.phrase_connector(tree, subPhrase, "then", elementName)
        #if subPhrase.get("null") or subPhrase.getPrev().get("null"):
        prev = subPhrase.getPrev()
        if self.isNull(subPhrase) or self.isNull(prev):
            return then
        # Check for either subPhrase specifying only special connector
        connector = subPhrase.get("connector")
        if connector is not None:
            return connector
        connector = prev.get("connector")
        if connector is not None:
            return connector

        # Check for increasing/decreasing values
        subPhrase1 = subPhrase.getPrev()
        val1 = self.getScalarData(tree, subPhrase1, elementName, "Average")
        val2 = self.getScalarData(tree, subPhrase, elementName, "Average")
        if val1 > val2:
            connector = self.phrase_connector(
                tree, subPhrase, "decreasing to", elementName)
        elif val1 < val2:
            connector = self.phrase_connector(
                tree, subPhrase, "increasing to", elementName)
        else:
            connector = then
        return connector

    def wxConnector(self, tree, subPhrase):
        # Return connector string to connect subPhrase and previous one.
        # If subPhrases cover neighboring time ranges, connect them with "then"
        # Otherwise, connect them with ". "
        # Make sure that we do not connect more than two subPhrases in a row
        # with a "then" connector by setting and re-setting the "useThenConnector"
        # flag at the phrase level.
        thenConnector = self.phrase_connector(tree, subPhrase, "then", "Wx")
        connector = '. '
        prev = subPhrase.getPrev()
        if prev is None:
            return ""
        phrase = subPhrase.getParent()
        index = subPhrase.getIndex()
        if index == 1:
            # Initialize so that we are ready to use the thenConnector
            # if appropriate
            phrase.set("useThenConnector", 1)
        useThenConnector = phrase.get("useThenConnector")
        prevEnd = prev.getTimeRange().endTime()
        #  If the start time of this subPhrase is the same
        #  as the end time of the previous subphrase
        if useThenConnector and prevEnd == subPhrase.getTimeRange().startTime():
            #  use the then connector
            connector = '...then '
            # Re-set useThenConnector so we don't get
            # a long string of "...then" connected sub-phrases
            phrase.set("useThenConnector", 0)
        else:
            # Can re-set connector so we are ready to use the
            # then connector on the next subPhrase
            phrase.set("useThenConnector", 1)
        return connector

    def visConnector(self, tree, subPhrase):
        # return connector phrase to connect subPhrase and previous one
        elementName = subPhrase.getAncestor("firstElement").name
        then = self.phrase_connector(tree, subPhrase, "then", elementName)
        #if subPhrase.get("null") or subPhrase.getPrev().get("null"):
        prev = subPhrase.getPrev()
        if self.isNull(subPhrase) or self.isNull(prev):
            return then
        # Check for either subPhrase specifying only special connector
        connector = subPhrase.get("connector")
        if connector is not None:
            return connector
        connector = prev.get("connector")
        if connector is not None:
            return connector

        # Check for increasing/decreasing values
        subPhrase1 = subPhrase.getPrev()
        # Get vis for previous sub-phrase
        statDict = subPhrase1.getStatDict()
        rankList = self.getStats(statDict, "Wx")
        if rankList is None or len(rankList) == 0:
            return self.setWords(node, "")
        subkeyList = self.getSubkeys(rankList)
        val1 = self.getVis(subkeyList)
        if val1 is None:
            return then
        # Get vis for current sub-phrase
        statDict = subPhrase.getStatDict()
        rankList = self.getStats(statDict, "Wx")
        if rankList is None or len(rankList) == 0:
            return self.setWords(node, "")
        subkeyList = self.getSubkeys(rankList)
        val2 = self.getVis(subkeyList)
        if val2 is None:
            return then

        if val1 > val2:
            connector = self.phrase_connector(
                tree, subPhrase, "decreasing to", elementName)
        elif val1 < val2:
            connector = self.phrase_connector(
                tree, subPhrase, "increasing to", elementName)
        else:
            connector = then
        return connector

    def vectorConnector(self, tree, subPhrase):
        # return connector phrase to connect subPhrase and previous one
        elementName = subPhrase.getAncestor("firstElement").name
        becoming =  self.phrase_connector(tree, subPhrase, "becoming", elementName)
        #if subPhrase.get("null") or subPhrase.getPrev().get("null"):
        if self.isNull(subPhrase) or self.isNull(subPhrase.getPrev()):
             return becoming

        subPhrase1 = subPhrase.getPrev()
        mag1, dir1, dirStr1 = self.getVectorData(
            tree, subPhrase1, elementName, "Average")
        mag2, dir2, dirStr2 = self.getVectorData(
            tree, subPhrase, elementName, "Average")

        increasingTo = self.phrase_connector(
                    tree, subPhrase, "increasing to", elementName)
        decreasingTo = self.phrase_connector(
                    tree, subPhrase, "decreasing to", elementName)

        # Directions same
        if dirStr1 == dirStr2:
            increment = self.nlValue(self.increment_nlValue(
                tree, subPhrase, elementName, elementName), mag1)
            # Magnitudes same
            if abs(mag1-mag2) < increment:
                connector = becoming
            # Magnitudes different
            elif mag1 < mag2:
                connector = increasingTo
            else:
                connector = decreasingTo
        # Directions different
        else:
            magDiff = self.nlValue(self.vector_mag_difference_nlValue(
                tree, subPhrase, elementName, elementName), mag1)
            # Magnitudes same
            if abs(mag1 - mag2) < magDiff:
                connector = self.phrase_connector(
                    tree, subPhrase, "shifting to the", elementName)
            # Magnitudes different
            else:
                # If high wind conditions report both "becoming" and
                # "increasing/decreasing"
                # SOUTHEAST WINDS AROUND 70 MPH BECOMING SOUTH
                #   AND INCREASING TO AROUND 105 MPH
                increasing = mag1 < mag2
                if max(mag1, mag2) > self.highValue_threshold(
                    tree, subPhrase, elementName, elementName):
                    dirStr = subPhrase.get("dirStr")
                    words = subPhrase.get("words")
                    words = words.replace(dirStr+" ", "")
                    subPhrase.set("words", words)
                    direction = becoming + dirStr + " and"
                    if increasing:
                        connector = direction + increasingTo
                    else:
                        connector = direction + decreasingTo
                # Otherwise, report both "increasing" or "becoming"
                # SOUTHEAST WINDS AROUND 20 MPH BECOMING SOUTH
                #   AROUND 15 MPH
                else:
                    if increasing:
                        connector = increasingTo
                    else:
                        connector = becoming
        return connector

    def marine_vectorConnector(self, tree, subPhrase):
        # return connector phrase to connect subPhrase and previous one
        elementName = subPhrase.parent.get("firstElement").name
        if self.isNull(subPhrase) or self.isNull(subPhrase.getPrev()):
            return self.phrase_connector(tree, subPhrase, "becoming", elementName)

        subPhrase1 = subPhrase.getPrev()
        mag1, dir1, dirStr1 = self.getVectorData(
            tree, subPhrase1, elementName, "Average")
        mag2, dir2, dirStr2 = self.getVectorData(
            tree, subPhrase, elementName, "Average")

        if dirStr1 == dirStr2:
            increment = self.nlValue(self.increment_nlValue(
                tree, subPhrase, elementName, elementName), mag1)
            if abs(mag2-mag1) < increment:
                connector = self.phrase_connector(tree, subPhrase, "becoming", elementName)
            elif mag1 < mag2:
                connector = self.phrase_connector(tree, subPhrase, "rising to", elementName)
            else:
                connector = self.phrase_connector(tree, subPhrase, "easing to", elementName)
        else:
            magDiff = self.nlValue(self.vector_mag_difference_nlValue(
                tree, subPhrase, elementName, elementName), mag1)
            if abs(mag2 - mag1) < magDiff:
                # Put in test for sea breeze i.e. becoming onshore
                if self.seaBreeze_flag(tree, subPhrase, elementName) == 1:
                    connector = self.phrase_connector(tree, subPhrase, "becoming onshore", elementName)
                    # Remove subPhrase words
                    subPhrase.set("words", "")
                else:
                    movement = self.direction_movement(dir1, dir2)
                    if movement > 0: # clockwise
                        connector = self.phrase_connector(tree, subPhrase, "veering", elementName)
                    else:
                        connector = self.phrase_connector(tree, subPhrase, "backing", elementName)
            else:
                connector = self.phrase_connector(tree, subPhrase, "becoming", elementName)
        return connector

    def removeDirection(self, tree, subPhrase):
        # Remove the direction from the subPhrase words
        dirStr = subPhrase.get("dirStr")
        if dirStr is not None:
            words = subPhrase.get("words")
            words = words.replace(dirStr, "")
            subPhrase.set("words",words)

    def getVectorData(self, tree, subPhrase, elementName, accessMethod):
        # Get vector data for subPhrase for the given elementName
        statDict = subPhrase.getStatDict()
        stats = statDict[elementName]
        if stats is None:
            return None, None, None
        mag, dir = stats
        mag = self.getValue(mag, accessMethod)
        dirStr= self.dirToText(dir)
        return mag, dir, dirStr

    def getScalarData(self, tree, subPhrase, elementName, accessMethod):
        # Get scalar data for subPhrase for the given elementName
        matchingInfo = self.matchToWxInfo(tree, subPhrase, elementName, elementName)
        if matchingInfo != "":
            val = self.matchToWx(tree, subPhrase, elementName)
            if accessMethod == "MinMax":
                val = (val, val)
        else:
            firstElement = subPhrase.getAncestor("firstElement")
            dataType = firstElement.dataType
            statDict = subPhrase.getStatDict()
            val = statDict[elementName]
            val = self.getValue(val, accessMethod, dataType)
        return val

    def seaBreeze_flag(self, tree, subPhrase, elementName):
        # Return 1 if an onshore breeze is detected from the prior range
        # Get local effects areas directions for Offshore previous subPhrase
        # and Onshore for subPhrase
        #    offshoreDir, onshoreDir
        offShoreArea, onShoreArea = self.seaBreeze_areaLabels(tree, subPhrase)
        if offShoreArea is None:
            return 0
        subPhrase1 = subPhrase.getPrev()
        if subPhrase1 is None:
            return 0
        timeRange1 = subPhrase1.getTimeRange()
        timeRange2 = subPhrase.getTimeRange()
        areaLabel1 = subPhrase1.getAreaLabel()
        areaLabel2 = subPhrase.getAreaLabel()
        offshore = tree.stats.get(
            "Wind", timeRange1, offShoreArea, mergeMethod="Max",
            intersectWith=areaLabel1)
        onshore = tree.stats.get(
            "Wind", timeRange2, onShoreArea, mergeMethod="Max",
            intersectWith=areaLabel2)
        if offshore is None or onshore is None:
            return 0
        mag, offshoreDir = offshore
        mag, onshoreDir = onshore
        # Get thresholds
        offshore1, offshore2, onshore1, onshore2 = self.seaBreeze_thresholds(tree, subPhrase)
        if self.direction_between(offshoreDir, offshore1, offshore2) and \
            self.direction_between(onshoreDir, onshore1, onshore2):
            return 1
        return 0

    # Subphrase Level
    def checkRepeatingString(self, tree, node, str, strName, matchAreaLabels=1):
        # Given a text string, str, and a descriptive name for that string,
        # see if it repeats in the previous phrase, sub-phrase or embedded phrase.
        # If we find a repeating string, return an empty string
        # Otherwise return the original string.
        # If matchAreaLabels, the areaLabel of previous node must match
        # that of the current if we are to return an empty string.
        # This prevents phrases such as:
        #    Chance of rain and snow 20 percent windward rain and snow 40 percent leeward.
        #

        # Check sub-phrases
        #print "Check Repeating", node.getAncestor('name'), str
        #print "   matchAreaLabels", matchAreaLabels
        prevNode = node.getPrev()
        if prevNode is not None:
            if matchAreaLabels and \
               prevNode.getAreaLabel() != node.getAreaLabel():
                return str
            prevStr = prevNode.get(strName)
            if prevStr is not None and str == prevStr:
                # Do not repeat previous str
                #print "return 1"
                return ""
        # Check degenerate conjunctive local effect
        # We are looking for these conditions:
        #  --This phrase has only one sub-phrase
        #  --The previous phrase has only one sub-phrase AND
        #  has the same name as the current phrase (e.g. popMax_phrase
        #  --The str for the sub-phrases are the same
        phrase = node.getParent()
        #tree.printNode(phrase.parent)
        if len(phrase.childList) == 1:
            prevPhrase = phrase.getPrev()
            if prevPhrase is not None:
                if matchAreaLabels and \
                   prevPhrase.getAreaLabel() != node.getAreaLabel():
                    return str
                if prevPhrase.get("name") == phrase.get("name"):
                    if len(prevPhrase.childList) == 1:
                        prevSubPhrase = prevPhrase.childList[0]
                        prevStr = prevSubPhrase.get(strName)
                        if prevSubPhrase.get('words') is None:
                            # Must wait for previous words to finish
                            return -1
                        if prevStr is not None and str == prevStr:
                            # Do not repeat previous str
                            #print "return 2"
                            return ""
        return str

    # Local Effects

    def checkLocalEffects(self, tree, node):
        localEffectsList = self.getLocalEffectsList(tree, node)
        #print " le list", localEffectsList
        if localEffectsList is None or len(localEffectsList) == 0:
            return self.DONE()
        childList = node.get("childList")
        if childList is None or len(childList) < 1:
            return self.DONE()
        if self.__dict__.get('_leDebug',0):
            print "\nChecking local effects for", node.get('name'), node.getAreaLabel()
            print " node", node
            print " parent", node.parent
            print " disabled", node.get('disabledSubkeys'), node.getAncestor('disabledSubkeys')
            print "\ncomp phrases before:"
            self.printCompPhrases(tree, node)

        for localEffect in localEffectsList:
            # If ANY subPhrase has a local effect, create conjunctive local effect.
            # If ALL subPhrases have the same local effect "groups", use that grouping.
            # Otherwise, create a conjunctive phrase for each local effect area.
            flag = 0
            firstTime = 1
            sameGroups = 1
            for checkNode in childList:
                nodeFlag, nodeGroups = self.checkLocalEffect(tree, checkNode, localEffect)
                if nodeFlag:
                    flag = 1
                if firstTime:
                    groups = nodeGroups
                    firstTime = 0
                elif groups != nodeGroups:
                    # flag must be 1
                    sameGroups = 0
                    break
            if flag:
                # Create conjunctive local effect
                #print "Creating conjunctive local effect"
                if sameGroups == 0:
                    groups = []
                    leAreaList = self.getLeAreaList(tree, node, localEffect)
                    for leArea in leAreaList:
                        groups.append([leArea])
                nodeList = self.makeLocalEffectNodes(tree, node, localEffect, groups)

                # Applies only to the skyPopWx_phrase
                # Set up includeSky for new local effect nodes
                includeSky = self.getIncludeSky(tree, node)
                for newNode in nodeList:
                    newNode.set("includeSky", includeSky)
                    if self.__dict__.get('_leDebug',0):
                        print "newNode", newNode.get("name"), newNode.get("areaLabel")
                        print "    includeSky", includeSky, newNode
                node.replace(nodeList)


        if flag: # There is a local effect
            self.localEffect_hook(tree, node)
        if self.__dict__.get('_leDebug',0):
            print "\ncomp phrases after:", self.printCompPhrases(tree, node)
        return self.DONE()

    def checkLocalEffect(self, tree, node, localEffect):
        # Check each local effect area against all others for the given node.
        # Determine "groups" i.e. group the local effect areas according to
        # similar statistics.
        # Return
        #   -- a flag to indicate if any local effect areas showed differing
        #      statistics.
        #   -- the "groups"
        triggerMethod = localEffect.triggerMethod
        leAreaList = self.getLeAreaList(tree, node, localEffect)
        if len(leAreaList) == 0:
            return 0, []

        # Begin with one group consisting of first local effect edit area
        groups = [[leAreaList[0]]]
        # This loop checks each subsequent local effect edit area against
        # the existing groups and appends it to the first group which
        # has similar statistics.
        # If no existing group has similar statistics, a new group is
        # created.
        for leArea1 in leAreaList[1:]:
            addedToExisting = 0
            for group in groups:
                leArea2 = group[0]
                difference = self.checkThreshold(
                    tree, node, triggerMethod, leArea1, leArea2, localEffect)
                if difference == 0:
                    # Similar statistics, so
                    # append it to the current group
                    group.append(leArea1)
                    addedToExisting = 1
                    break
            if addedToExisting == 0:
                # Did not find similar group, so create a new group
                groups.append([leArea1])
        if len(groups) == 1:
            flag = 0
        else:
            flag = 1
        return flag, groups

    def getLocalEffectsList(self, tree, node):
        leList = node.get("localEffectsList")
        if type(leList) is types.MethodType:
            return leList(tree, node)
        else:
            return leList

    def getLeAreaList(self, tree, node, localEffect):
        leAreaList = localEffect.leAreaList
        if type(leAreaList) is types.MethodType:
            return leAreaList(tree, node)
        else:
            return leAreaList

    def getLeAreaLabel(self, tree, node, leArea):
        if leArea.areaLabel == "__Current__":
            return node.getAreaLabel()
        elif leArea.intersectFlag:
            return self.getIntersectName(node.getAreaLabel(), leArea.areaLabel)
            #return self.getIntersectName(tree.getAreaLabel(), leArea.areaLabel)
        else:
            return leArea.areaLabel

    def getLeQualifiers(self, tree, node, group):
        # Return the qualifiers for this group of leAreas
        # There is a qualifer for embedded local effect phrases
        # and one for conjunctive local effect phrases.
        embeddedQualifier = ""
        conjQualifier = ""
        length = len(group)
        index = 0
        for leArea in group:
            areaWords = leArea.areaWords
            if type(areaWords) is types.MethodType:
                areaWords = areaWords(tree, node, leArea)
            embeddedQualifier = embeddedQualifier + areaWords
            conjWords = leArea.conjAreaWords
            if type(conjWords) is types.MethodType:
                conjWords = conjWords(tree, node, leArea)
            conjQualifier = conjQualifier + conjWords
            # if last one, do not add conjunction
            if index == length - 1: break
            embeddedQualifier = embeddedQualifier + " and "
            conjQualifier = conjQualifier + " and "
            index = index + 1
        return embeddedQualifier, conjQualifier

    def checkThreshold(self, tree, node, triggerMethod, leArea1, leArea2, localEffect):
        # Return 1 if the difference between leArea1 and leArea2 stats is
        # greater than the threshold
        # Handles stats that are a min/max or a singleValue
        leArea1Label = self.getLeAreaLabel(tree, node, leArea1)
        leArea2Label = self.getLeAreaLabel(tree, node, leArea2)
        if type(triggerMethod) is types.MethodType:
            flag =  triggerMethod(tree, node, localEffect, leArea1Label, leArea2Label)
        else:
            first = node.getAncestor("firstElement")
            element = first.name
            dataType = first.dataType
            if dataType == self.WEATHER():
                mergeMethod = "Average"
            else:
                mergeMethod = "MinMax"
            timeRange = node.getTimeRange()
            area1Stats = tree.stats.get(element, timeRange, leArea1Label,
                                        mergeMethod=mergeMethod)
            area2Stats = tree.stats.get(element, timeRange, leArea2Label,
                                        mergeMethod=mergeMethod)
            area1Stats = self.applyDisabled(tree, node, area1Stats)
            area2Stats = self.applyDisabled(tree, node, area2Stats)
            if self.__dict__.get("_leDebug", 0):
                print "\nCheckThreshold", element, timeRange
                print leArea1Label, area1Stats
                print leArea2Label, area2Stats
            if area1Stats is None or area2Stats is None:
                return 0
            flag = self.checkLocalEffectDifference(
                tree, node, dataType, triggerMethod, area1Stats, area2Stats,
                leArea1Label, leArea2Label)
            if self.__dict__.get("_leDebug", 0):
                print "returning", flag
        return flag

    def applyDisabled(self, tree, node, stats):
        if stats is None:
            return stats
        disabledSubkeys = node.getAncestor('disabledSubkeys')
        #print "/n applyDisabled: disabled", disabledSubkeys
        #print "stats", stats
        if disabledSubkeys is not None:
            newStats = []
            for subkey, rank in stats:
                if subkey not in disabledSubkeys:
                    newStats.append((subkey, rank))
            stats = newStats
            if stats == []:
                emptyKey = WeatherSubKey.weatherSubKey(self._argDict['dataMgr'],
                    "<NoCov>", "<NoWx>", "<NoInten>", "<NoVis>", [])
                stats = [(emptyKey, 100)]
        return stats

    def checkLocalEffectDifference(self, tree, node, dataType, threshold,
                                   area1Stats, area2Stats, al1, al2):
        if dataType == self.DISCRETE():
            if area1Stats != area2Stats:
                return 1
            else:
                return 0
        if dataType == self.WEATHER():
            flag = self.checkWeatherSimilarity(
                tree, node, area1Stats, area2Stats, al1=al1, al2=al2)
            # checkWeatherSimilarity returns 0 if there IS a difference and, thus,
            # should be a local effect
            if flag == 0:
                return 1
            else:
                return 0
        if dataType == self.VECTOR():
            area1Stats, dir = area1Stats
            area2Stats, dir = area2Stats

        if type(area1Stats) is types.TupleType:
            min1, max1 = area1Stats
            min2, max2 = area2Stats
            diff1 = self.absDiff(min1, min2)
            diff2 = self.absDiff(max1, max2)
            # Check to see if one range is included within the other
            if self.rangeIncluded(min1, max1, min2, max2) == 1:
                return 0
            if self.rangeIncluded(min2, max2, min1, max1) == 1:
                return 0
            # Check to see if either min or max is greater than threshold
            if diff1 > threshold or diff2 > threshold:
                return 1
            else:
                return 0
        else:
            absDiff = self.absDiff(area1Stats, area2Stats)
            if absDiff > threshold:
                return 1
            else:
                return 0

    def checkSkyWxDifference(self, tree, node, localEffect, leArea1Label, leArea2Label):
        timeRange = node.getTimeRange()
        wxStats1 = tree.stats.get("Wx", timeRange, leArea1Label,
                                    mergeMethod="Average")
        wxStats2 = tree.stats.get("Wx", timeRange, leArea2Label,
                                    mergeMethod="Average")
        wxStats1 = self.applyDisabled(tree, node, wxStats1)
        wxStats2 = self.applyDisabled(tree, node, wxStats2)
        #print "wxStats1", wxStats1
        #print "wxStats2", wxStats2
        wxSame = self.checkWeatherSimilarity(
            tree, node, wxStats1, wxStats2, al1=leArea1Label, al2=leArea2Label)
        #print "wxSame", wxSame
        if wxSame == 0:
            wxDiff = 1
        else:
            wxDiff = 0

        skyDiff = self.checkSkyDifference(tree, node, localEffect,
                                          leArea1Label, leArea2Label)

        # Determine if ANY of the sub-phrases have a sky local effect
        # and store this information at the parent level for later
        # use by the "checkLocalEffects" method.
        skyLE = node.parent.get("skyLE")
        if skyLE is None:
            node.parent.set("skyLE", 0)
        if skyDiff:
            node.parent.set("skyLE", 1)
        #return wxDiff
        return skyDiff or wxDiff

    def getIncludeSky(self, tree, node):
        # If this is called, then we know we have a LE
        # i.e. there was a wx local effect.
        skyLE = node.get("skyLE")
        if skyLE:
            return None  # Want to make sure we check at the LE level
        else:
            return 0

    def checkSkyDifference(self, tree, node, localEffect,
                            leArea1Label, leArea2Label):
        timeRange = node.getTimeRange()
        skyValue1 = tree.stats.get("Sky", timeRange, leArea1Label,
                                    mergeMethod="Average")
        skyValue2 = tree.stats.get("Sky", timeRange, leArea2Label,
                                    mergeMethod="Average")
        if timeRange.duration() > 12*3600:
            dayNight = -1
        else:
            dayNight = self.getPeriod(timeRange, 1)
        words1 = self.sky_value(tree, node, self.getValue(skyValue1), dayNight)
        words2 = self.sky_value(tree, node, self.getValue(skyValue2), dayNight)
        return not self.similarSkyWords_flag(tree, node, words1, words2)

    def localEffect_hook(self, tree, node):
        return

    def rangeIncluded(self, min1, max1, min2, max2):
       # Return 1 if min1, max1 are included in min2, max2
       if min1 >= min2 and max1 <= max2:
           return 1
       return 0

    def absDiff(self, val1, val2):
        # Return the absolute difference between the values
        # Note: this handles negative values
        if (val1 > 0 and val2 > 0) or (val1 < 0 and val2 < 0):
            return abs(val1 - val2)
        else:
            return abs(val1) + abs(val2)

    def makeLocalEffectNodes(self, tree, node, localEffect, groups):
        # Make a node phrase for each group of local effect areas in groups
        nodeList = []
        for group in groups:
            leAreaLabel = self.getLeAreaLabel(tree, node, group[0])
            newNode = tree.copyPhrase(
                node, node.getTimeRange(), leAreaLabel,
                copyAttrs=["disabledSubkeys", "disabledElements",
                           "firstElement", "elementName", "elementInfoList",
                           "descriptor", "indentLabel"])
            embeddedQualifier, conjQualifier = self.getLeQualifiers(tree, node, group)
            newNode.set("embeddedQualifier", embeddedQualifier)
            newNode.set("conjunctiveQualifier", conjQualifier)
            newNode.set("localEffect", localEffect)
            newNode.set("leGroup", group)
            nodeList.append(newNode)
        return nodeList

    def printCompPhrases(self, tree, node):
        comp = node.getComponent()
        print "Component phrases for", node
        for phrase in comp.get('childList'):
            print phrase.get('name'), phrase.getAreaLabel(), phrase
            print "            ", phrase.get('words')
