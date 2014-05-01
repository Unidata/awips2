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
# ForecastNarrative.py
#
# Forecast type: "narrative"
# Class for processing Narrative Forecasts
#
# Author: hansen
# ----------------------------------------------------------------------------

import time, types
import TextRules
import SampleAnalysis
import Translator
import logging
import AbsTime
import TimeRange
from com.raytheon.uf.common.time import TimeRange as JavaTimeRange
from com.raytheon.viz.gfe.sampler import HistoSampler
from com.raytheon.viz.gfe.sampler import SamplerRequest
from com.raytheon.uf.common.dataplugin.gfe.db.objects import GridParmInfo
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID
from com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID
from java.util import ArrayList
import copy
import traceback
import JUtil

class Node:
    def __init__(self, childList, methodList):
        self.childList = childList
        self.methodList = methodList
        self.parent = None
        # Make tree bi-directional
        for child in childList:
            child.parent = self
        # Keep track of changes made to this node
        self.changeFlag = 0
        # Keep track of methods that are done
        self.doneList = []
    def getIndex(self):
        # If this node is a child,
        # return it's index in the childList of the parent
        try:
            return self.parent.childList.index(self)
        except:
            return None
    def getParent(self):
        return self.parent
    def getComponent(self):
        # Return this node's ancestor at the second level in the tree
        prevNode = None
        node = self
        while node.getParent() is not None:
            prevNode = node
            node = node.getParent()
        return prevNode
    def getComponentName(self):
        node = self
        compName = node.get("componentName")
        if compName is not None:
            return compName
        else:
            comp = node.getComponent()
            if comp is not None:
                return comp.get("name")
            else:
                return None
    def getNext(self):
        if self.parent is not None:
            index = self.getIndex()
            childList = self.parent.childList
            if len(childList) > index+1:
                return childList[index+1]
    def getPrev(self):
        if self.parent is not None:
            index = self.getIndex()
            childList = self.parent.childList
            if index > 0:
                return childList[index-1]
    def set(self, member, value):
        #print "    Setting", member,
        if hasattr(self, member):
            current = getattr(self, member)
            #print "current/value", current, value
            if current == value:
                #print "    No Change"
                return
        setattr(self, member, value)
        self.changeFlag = 1
        #print "   Changed"
    def get(self, member, default=None):
        if hasattr(self, member):
            return getattr(self, member)
        else:
            return default
    def printNode(self, node, indentStr=""):
        print "Node", node
        print indentStr + "  Methods"
        for method in node.methodList:
            if method in node.doneList:
                done = "DONE"
            else:
                done = ""
            print indentStr + "    ", method.__name__, done
        print indentStr + "  Attributes"
        dict = node.__dict__
        for key in dict:
            if key == "methodList" or key == "doneList":
                continue
            print indentStr + "    ", key, dict[key]
        print indentStr + "  Children ", len(node.childList)
        for child in node.childList:
            self.printNode(child, indentStr + "    ")
    def copy(self):
        newNode = Node([], [])
        dict = self.__dict__
        for key in dict:
            newNode.set(key, self.get(key))
        return newNode
    def insertChild(self, sibling, newChild, newFirst=0):
        # Insert the newChild
        # If newFirst, insert newChild before sibling,
        # else afterward.
        newChild.parent = self
        new = []
        for child in self.childList:
            if child == sibling:
                if newFirst:
                    new.append(newChild)
                    new.append(child)
                else:
                    new.append(child)
                    new.append(newChild)
            else:
                new.append(child)
        self.childList = new
    def remove(self):
        # Remove this node from it's parent child list
        parent = self.parent
        new = []
        for child in parent.childList:
            if child != self:
                new.append(child)
        parent.childList = new
        # Set the attribute for removing the child
        setattr(self, "removed", 1)
    def findChild(self, attr, value):
        # Find the child of this node with the given attribute
        # of the given value
        for child in self.childList:
            if child.get(attr) == value:
                return child
    def getProgeny(self):
        # Return a list of all progeny of this node
        progeny = self.childList
        for child in self.childList:
            childProgeny = child.getProgeny()
            if childProgeny is not None:
                progeny = progeny + child.getProgeny()
        return progeny
    def replace(self, nodeList):
        # Replace the current child node with the node list.
        # If top of tree, does nothing.
        childList = self.parent.childList
        newList = []
        for child in childList:
            if child == self:
                newList = newList + nodeList
            else:
                newList.append(child)
        self.parent.childList = newList
        # Remove any children of current node
        self.childList = []
        # Make this node defunct
        self.doneList = self.methodList
    def getTimeRange(self):
        if hasattr(self, "timeRange"):
            return self.timeRange
        # Look for an ancestor that has a timeRange associated with it
        if self.parent is not None:
            return self.parent.getTimeRange()
        return None
    def getStatDict(self):
        # Assume we are a subPhrase
        if hasattr(self, "statDict"):
            statDict =  self.statDict
            disabledElements = self.getAncestor("disabledElements")
            if disabledElements is not None:
                for key in statDict.keys():
                    for element in self.parent.disabledElements:
                        if key == element:
                            statDict[element] = None
            disabledSubkeys = self.getAncestor("disabledSubkeys")
            #print "disabledSubkey", disabledSubkeys
            if disabledSubkeys is not None:
                disabledWxTypes = []
                for disabledSubkey in disabledSubkeys:
                    disabledWxTypes.append(disabledSubkey.wxType())
                for key in statDict.keys():
                    if key == "Wx":
                        subkeys = statDict[key]
                        newList = []
                        for subkey in subkeys:
                            # Need to handle both "dominantWx" and
                            # "rankedWx" analysis
                            appendVal = subkey
                            if type(subkey) is types.TupleType:
                                subkey, rank = subkey
                            if subkey not in disabledSubkeys \
                                and subkey.wxType() not in disabledWxTypes:
                                newList.append(appendVal)
                        statDict[key] = newList
            return statDict
        else:
            return None
    def getAreaLabel(self):
        if hasattr(self, "areaLabel"):
            return self.areaLabel
        # Look for an ancestor that has an areaLabel associated with it
        if self.parent is not None:
            return self.parent.getAreaLabel()
        return None
    def getAncestor(self, attr):
        if hasattr(self, attr):
            return getattr(self, attr)
        # Look for an ancestor that has the given attribute associated with it
        if self.parent is not None:
            return self.parent.getAncestor(attr)
        return None
    def setAncestor(self, attr, value):
        if hasattr(self, attr):
            setattr(self, attr, value)
            return None
        # Look for an ancestor that has the given attribute associated with it
        if self.parent is not None:
            return self.parent.setAncestor(attr, value)
        return None
    def getDescendent(self, attr):
        if hasattr(self, attr):
            return getattr(self, attr)
        # Look for the first descendent that has the given attribute associated with it
        for child in self.childList:
            value = child.getDescendent(attr)
            if value is not None:
                return value
        return None
    
class Narrative(Node, TextRules.TextRules):
    # This is the root of the tree and, as such, has some special methods
    # and data members
    def __init__(self, methodList, componentList, statisticsDictionary,
                 issuanceInfo, library, histoSampler):
        self.stats = statisticsDictionary
        # Access to inherited methods
        self.library = library
        # A histoSampler for access to Topo
        self.histoSampler = histoSampler
        self.issuanceInfo = issuanceInfo

        # This is the root of the tree
        Node.__init__(self, componentList, methodList)
        TextRules.TextRules.__init__(self)

    def printTree(self):
        print "\n\nNarrative Tree\n"
        self.printNode(self, "")
    def getTopoHisto(self, areaLabel):
        editArea = self.library.findEditArea(None, areaLabel)
        return self.get("histoSampler").getTopoHisto(editArea.getId())
    def makeNode(self, children, methods, parent=None):
        node =  Node(children, methods)
        node.parent = parent
        return node
    def statisticsDictionary(self):
        return self.statisticsDictionary.dictionary()
    def getDataType(self, element):
        return self.library.getDataType(element)
    def getLimits(self, element):
        return self.library.getLimits(element)
    def makeComponent(self, name, timeRange, definition):
        return self.library.makeComponent(name, timeRange, definition)
    def makePhrase(self, phraseDef):
        return self.library.makePhrase(phraseDef)
    def copyPhrase(self, node, timeRange=None, areaLabel=None, parent=None,
                   copyAttrs=[]):
        phraseDef = node.get("phraseDef")
        newNode = self.library.makePhrase(phraseDef)
        # copy attributes from original node
        for attr in copyAttrs:
            newVal = node.get(attr)
            if type(newVal) is types.ListType:
                newList = []
                for item in newVal:
                    newList.append(item)
                newVal = newList
            newNode.set(attr, newVal)
        if areaLabel is None:
            areaLabel = node.getAreaLabel()
        newNode.set("areaLabel", areaLabel)
        if timeRange is None:
            timeRange = node.getTimeRange()
        newNode.set("timeRange", timeRange)
        if parent is None:
            parent = node.parent
        newNode.parent = parent
        # Preserve attributes
        newNode.set("args", node.get("args"))
        return newNode
    
    def addPhrase(self, prevPhrase, timeRange=None, areaLabel=None):
        # Make the new phrase follow given phrase
        newPhrase = self.copyPhrase(prevPhrase, timeRange, areaLabel)
        parent = prevPhrase.parent
        parent.insertChild(prevPhrase, newPhrase)
        return newPhrase
    def addPhraseDef(self, prevPhrase, phraseDef, timeRange=None, areaLabel=None):
        # Make the new phrase follow given prevPhrase using the given phraseDef
        newPhrase = self.library.makePhrase(phraseDef)
        if areaLabel is None:
            areaLabel = prevPhrase.getAreaLabel()
        newPhrase.set("areaLabel", areaLabel)
        if timeRange is None:
            timeRange = prevPhrase.getTimeRange()
        newPhrase.set("timeRange", timeRange)
        parent = prevPhrase.parent
        newPhrase.parent = parent
        parent.insertChild(prevPhrase, newPhrase)
        return newPhrase

class StatisticsDictionary(TextRules.TextRules):
    def __init__(self, dictionary, library):
        # Dictionary is a multi-level dictionary storing statistics
        self.dictionary = dictionary
        self.library = library
        TextRules.TextRules.__init__(self)
    def set(self, element, areaLabel, timeRange, statLabel, value):
        # Set the dictionary value according to keyOrder
        # E.g. dict[element][areaLabel][timeRange][statLabel] = value
        keyOrder = ["element", "areaLabel", "timeRange", "statLabel"]
        dict = self.dictionary
        execStr = "dict"
        index = 0
        lastIndex = len(self.keyOrder)-1
        for keyName in keyOrder:
            execStr = execStr + "["+keyName+"]"
            if index == lastIndex:
                exec execStr + "= value"
            else:
                # Make sure there is at least an empty dictionary
                # for this keyName
                try:
                    exec "result = " + execStr
                except:
                    exec execStr + "= {}"
            index = index + 1
    def get(self, element, timeRange, areaLabel=None, statLabel="", mergeMethod="List",
            intersectWith=None):
        if areaLabel is None:
            areaLabel = self.areaLabel
        if intersectWith is not None:
            areaLabel = self.library.getIntersectName(intersectWith, areaLabel)
        dictionary = self.dictionary
        dataType = self.library.getDataType(element)
        #print "Getting stats", element, mergeMethod, timeRange, areaLabel, statLabel

        # Get the raw value (could be simple value OR statsByRange)
        try:
            # See if there is an exact match entry
            value = dictionary[element][areaLabel][timeRange][statLabel]
        except:
            # Gather statsByRange for anything overlapping the timeRange
            value = []
            try:
                dict = dictionary[element][areaLabel]
            except:
                return None
            if statLabel != "":
                statLabel = element + "__" + statLabel
            matchFound = 0
            #if element == "Wx":
            #    print "\n\nstatLabel", statLabel, dict.keys()
            for subRange in dict.keys():
                statDict = dict[subRange]
                #if element == "Wx":
                #    print "statDict keys", statDict.keys()
                if statLabel in statDict.keys():
                    if subRange.overlaps(timeRange):
                        # If subRange covers the timeRange, treat as exact match
                        subValue = statDict[statLabel]
                        if subRange.contains(timeRange):
                            value =  subValue
                            matchFound = 1
                            break
                        if self.library.isStatsByRange(dataType, subValue):
                            for subStats, range in subValue:
                                #print "appending", subStats, range
                                value.append((subStats, range))
                        else:
                            #print "appending2", subValue, subRange
                            value.append((subValue, subRange))

            # IF we have "glued" together stats from timeRanges
            # overlapping the time range in question,
            # then we have to eliminate duplicates and
            # make sure the resulting statsByRange are
            # in chronological order.
            if matchFound == 0:# and mergeMethod == "List":
                # Make sure the subRanges are in order
                if len(value) > 0:
                    temp = []
                    #print "before sort", timeRange
                    for stats, subRange in value:
                        #print stats, subRange
                        temp.append((subRange.startTime(), (stats, subRange)))
                    temp.sort()
                    value = []
                    #print "after sort"
                    lastRange = None
                    for t in temp:
                        stats, subRange = t[1]
                        if lastRange is not None and subRange == lastRange:
                            continue
                        lastRange = subRange
                        #print t[1]
                        value.append(t[1])

        # Apply mergeMethod to the value
##        if areaLabel == "_OffShoreArea_intersect_Region_1" or areaLabel == "Region 1":
##        if element == "MaxT" and timeRange.duration > 24*3600:
##                print "\n Area", areaLabel, timeRange
##                print "    Merging", value
        value =  self.library.getMergedStats(
            value, mergeMethod, element, timeRange, areaLabel, dataType)

##        if element == "MaxT" and timeRange.duration > 24*3600:
##                print "    returning", value
        return value

    def printDictionary(self, element=None):
        if element is None:
            print "\n\nStatistics Dictionary\n"
            self.printDict(self.dictionary, "")
        else:
            try:
                print "\n\nStatistics Dictionary for "+element+"\n"
                self.printDict(self.dictionary[element], "")
            except:
                pass
    def printDict(self, dictionary, indentStr):
        for key in dictionary.keys():
            value = dictionary[key]
            if type(key) is types.StringType and key  == "":
                key = "EmptyString"
            print indentStr, key
            if type(value) is types.DictionaryType:
                self.printDict(value, indentStr + "   ")
            else:
                print indentStr, indentStr, value


class ForecastNarrative(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    #  This class processes a narrative-type text product.
    #  To use this class, set up a NarrativeDefinition indicating which
    #     components and consecutive time periods are to be in the narrative.
    #  After instantiating the class, call getNarrativeData to do all the
    #  sampling and analysis for the narrative.
    #  Then, for each edit area, call generateForecast.
    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)
        self.__gpiCache = {}
        self.log = logging.getLogger("FormatterRunner.ForecastNarrative.ForecastNarrative")

    def getNarrativeData(self, argDict, narrativeDefinition, timeRange, areaList, issuanceInfo):
        # Assemble the Tree for the Narrative
        # Samples and Analyzes all data for the Narrative
        # Assemble the StatisticsDictionary
        # If successful, returns None
        #  Otherwise returns a text string indicating the problem

        # General set up
        self.__ut = argDict["utility"]
        self.__narrativeDefinition = narrativeDefinition
        self.__timeRange = timeRange
        self.__areaList = areaList
        self.__priorPeriod = self.__ut.set(narrativeDefinition, "priorPeriod", None)
        self.__issuanceInfo = issuanceInfo
        self.__currentLocalTime, self.__shift = self.determineTimeShift()
        self.__lineLength = argDict["lineLength"]
        self.__combinations = argDict["combinations"]
        self.__ifpClient = argDict["ifpClient"]
        self.__argDict = argDict
        sampleFromServer = self.__ut.set(narrativeDefinition, "sampleFromServer", 0)
        try:
            self.__productTR = argDict["productTimeRange"]
            #print "Set from argDict"
        except:
            self.__productTR = timeRange
        #print "Setting productTR", self.__productTR

        # Caching for ReferenceData and ParmID's
        self.__areaCache = {}
        areaList = []
        for editArea, areaLabel in self.__areaList:
            if type(editArea) is types.StringType:
                #print "Get Edit Area FN: getting edit area 1", editArea
                editArea = self.getEditArea(editArea, argDict)
            self.__areaCache[areaLabel] = editArea
            areaList.append((editArea, areaLabel))
        self.__areaList = areaList
        self.__elementCache = {}
        self.__databaseID = argDict["databaseID"]

        # Break out the Definition into components and sampling information
        #   sets up self.__compList, self.__samplerRequests, self.__statisticsDict
        #print "\nNarrative Def", narrativeDefinition
        error = self.__breakOutDefinition(argDict, narrativeDefinition, timeRange, areaList)
        if error is not None:
            return error
        #for component, timeRange, definition in self.__compList:
        #    print component, timeRange

        # Call the Samplers
        # Get sample dictionary
        #  Sets up samplerDict:  WeatherElement: histoSampler
        time1 = time.time()
        if sampleFromServer:
            self.progressMessage(.3, 80, "Sampling Data from Server -- please wait...")
            self.__sampler = argDict["ifpClient"].sampleRequest(self.__samplerRequests)
        else:
            self.progressMessage(.3, 80, "Sampling Data -- please wait...")                        
            self.__sampler = HistoSampler(argDict["ifpClient"], self.__samplerRequests)
        print "Time to Sample Data", time.time()-time1
        #if error is not None:
        #    return error
        #print "Sampler", self.__sampler

        # Call SampleAnalysis methods to complete statisticsDict
        # and instantiate the StatisticsDictionary class
        #  Sets up self.__statisticsDict
        time1 = time.time()
        self.progressMessage(.6, 80, "Analyzing Data -- please wait...")
        error = self.__createStatisticsDictionary(argDict)
        print "Time to Get Statistics", time.time()-time1
        #error = self.__createStatisticsDictionary0(argDict)
        if error is not None:
            return error
        return None

    def ut(self):
        return self.__ut

    def sampler(self):
        return self.__sampler

    def statisticsDictionary(self):
        return self.__statisticsDictionary

    def issuanceInfo(self):
        return self.__issuanceInfo

    def generateForecast(self, argDict, editArea=None, areaLabel=None):
        self.__createNarrativeTree(argDict)
        if editArea is None:
            editArea, areaLabel = argDict["editArea"]
            if type(editArea) is types.StringType:
                editArea = self.getEditArea(editArea, argDict)
        # Otherwise, set argDict
        else:
            argDict["editArea"] = (editArea, areaLabel)
        self.__narrativeTree.editArea = editArea
        self.__narrativeTree.areaLabel =  areaLabel
        self.__narrativeTree.changeFlag = 0
        self.__narrativeTree.stats.areaLabel = areaLabel
        self.__trace = self.__narrativeTree.get("trace")
        self.__infiniteTrace = 0
        argDict["combinations"] = self.__narrativeTree.get("combinations")
        #self.__trace = 1
        self.__narrativeTree.lastChance = 0
        self.__narrativeTree.fixedIt = 0
        argDict["tree"] = self.__narrativeTree
        changesMade = 1
        time1 = time.time()
        passes = 0
        while changesMade:
            if self.__trace: print "\n####### Pass %d #####\n" % (passes + 1)

            # Tree traversal
            changesMade = self.traverseTree(self.__narrativeTree)
            if self.__trace: print "\n\nCHANGES IN PASS", changesMade
            #changesMade = 1
            passes = passes + 1
            self.__narrativeTree.passes = passes

            # Error recovery:
            #
            #  Check for infinite loop
            #    There are still changes being made to the tree,
            #    but we're in an infinite loop
            #  Check for no changes and empty words
            #    There are no more changes to the tree,
            #    but the words have not completed
            #  If either of these are true:
            #    --Do a "lastChance" pass to alert methods to
            #          finish if possible.
            #    --If still not done,
            #      Do a "FixIt" pass to fill in leaf
            #        nodes that have not finished and
            #        continue execution.
            
            if passes > self.__narrativeTree.get("passLimit") or \
               (changesMade == 0 and self.__narrativeTree.get('words') is None):
                if not self.__narrativeTree.lastChance:
                    # Do last chance pass
                    print "\nDoing Last Chance Pass"
                    self.__narrativeTree.lastChance = 1
                    changesMade = 1
                else:
                    # We already did a lastChance pass
                    if not self.__narrativeTree.fixedIt:
                        # Fix it, re-set passes and continue
                        print "\nDoing Fix it Pass"
                        changesMade = 1
                        self.__narrativeTree.passes = 0
                        self.__narrativeTree.fixedIt = 1
                        self.__problemPhrases = []
                        self.errorTraverse(self.__narrativeTree)
                    else:
                        # We already fixed it and still not done
                        # (This should never happen!)
                        # Stop execution: no more error recovery to attempt
                        changesMade = 0
                       
        print "Time for phrase generation for ", areaLabel, ":", time.time()-time1, "Passes", passes
        words =  self.__narrativeTree.get("words")
        if self.__narrativeTree.fixedIt:
            self.__problemPhrases = []
            self.errorTraverse(self.__narrativeTree)
            problems = self.errorMsg(passes)
            self.log.error(problems)
        if words is None:
            words = problems
        return words

    def traverseTree(self, node):
        # Top Down traversal

        if self.__trace:
            print "Traversing node:", node.get("name"), node.getAreaLabel(), node.getTimeRange()
            print "       ", node, node.parent

        # Execute methods at this level
        methodList = node.methodList
        for method in methodList:
            # Need to make sure that node has not been removed by some other method
            if method not in node.doneList and not hasattr(node, "removed"):
                time1 = time.time()
                done = method(self.__narrativeTree, node)
                if done:
                    # Add method to doneList
                    node.doneList.append(method)

                # Trace output    
                if self.__infiniteTrace:
                    print "Traversing node:", node.get("name"), node.getAreaLabel(), \
                          node.getTimeRange()
                    print "   Method:", method.__name__
                    print "   Done", done
                    print "   Node", node
                if self.__trace:
                    if done:
                        doneStr = "DONE"
                    else:
                        doneStr = ""
                    print "Method", method.__name__, doneStr, time.time()-time1,
                    print "      Words", node.get("words")                    

        # Execute methods of children
        # If ANY child is changed, we are not done
        childrenChanged = 0
        for child in node.childList:
            childChanged = self.traverseTree(child)
            if childChanged:
                childrenChanged = 1

        # See if we made any changes at this level OR at a child's level
        changesMade = childrenChanged | node.changeFlag
        if self.__trace: print 'Changes made:', changesMade
        # Re-set changeFlag
        node.changeFlag = 0
        return changesMade

    def __breakOutDefinition(self, argDict, definition, timeRange, areaList):
        #  Sets up self.__compList: (componentName, timeRange, definition),
        #          self.__samplerRequests
        #          self.__statisticsDict
        #
        # Set up self.__compList
        time1 = time.time()
        narrativeDef = self.__ut.set(definition,"narrativeDef", None)
        # Get list of tuples: forecastType, timeRange, definition
        self.__compList = self.__breakOutTimeRange(
            argDict, timeRange, narrativeDef, self.__currentLocalTime, self.__shift)
        # If error message, return error string
        if type(self.__compList) is types.StringType:
            return self.__compList

        #print "Time to make compList", time.time() - time1

        # Make samplerRequests
        firstTime = 1
        samplerRequests = ArrayList()
        statisticsDict = {}
        moreAreas = []

        time1 = time.time()
        for compName, timeRange, compDefinition in self.__compList:
            # Add the analysisList entries to the sampleList and statisticsDict
            # for this component
            analysisList = self.__ut.set(compDefinition, "analysisList", [])
            additionalAnalysisList = self.__ut.set(compDefinition, "additionalAnalysisList", [])
            if len(analysisList) == [] and additionalAnalysisList == []:
                continue
            sampleList = []
            # Make sampleList: (element, methodArgs, editArea, areaLabel)

            # First include the analysisList methods self.__areaList
            #print "\nRegular list"
            for analysis in analysisList:
                element, methodArgs = self.__getAnalysis(analysis)
                for editArea, areaLabel in self.__areaList:
                    #print "appending to sampleList", element, editArea.id(), areaLabel
                    sampleList.append((element, methodArgs, editArea, areaLabel))

            # Handle additional areas
            additionalAreas = self.__ut.set(compDefinition,"additionalAreas", [])
            additionalAnalysisList = self.__ut.set(
                compDefinition,"additionalAnalysisList", analysisList)
            #print "\nForecastNarrative additionalAreas"
            for element, leAreaList in additionalAreas:
                #print "element, leAreaList", element, leAreaList
                for areaLabel in leAreaList:
                    methodArgsList = self.__findMethods(element, additionalAnalysisList)
                    for methodArgs in methodArgsList:
                        editArea = self.findEditArea(None, areaLabel)
                        #print "appending to sampleList", element, editArea.id(), areaLabel
                        sampleList.append((element, methodArgs, editArea, areaLabel))

            # Handle intersect areas
            intersectAreas = self.__ut.set(compDefinition,"intersectAreas", [])
            # Determine intersectWithAreas
            intersectWithAreas = self.__ut.set(
                compDefinition,"intersectWithAreas", [])
            if intersectWithAreas == []:
                intersectWithAreas = self.__areaList
            else:
                intAreas = []
                for areaLabel in intersectWithAreas:
                    editArea = self.findEditArea(None, areaLabel)
                    intAreas.append((editArea, areaLabel))
                intersectWithAreas = intAreas
            # Determine intersectAnalysisList        
            intersectAnalysisList = self.__ut.set(
                compDefinition,"intersectAnalysisList", analysisList)
            # Set up intersections and sampleList entries
            #print "\nIntersect Areas"
            for element, leAreaList in intersectAreas:
                for leAreaLabel in leAreaList:
                    editAreas = self.__intersectEditAreas(
                        leAreaLabel, argDict, intersectWithAreas)
                    methodArgsList = self.__findMethods(element, intersectAnalysisList)
                    for editArea in editAreas:
                        for methodArgs in methodArgsList:
                            #print "appending to sampleList", element, editArea.id(), areaLabel
                            sampleList.append((element, methodArgs, editArea, editArea.getId().getName()))

            # Add to samplerRequests and statisticsDict
            self.__addToRequests(argDict, timeRange, compName, sampleList,
                                 samplerRequests, statisticsDict)

        self.__samplerRequests = samplerRequests
        self.__statisticsDict = statisticsDict
        #print "Time to create samplerRequests", time.time() - time1
        return None

    def __breakOutTimeRange(self, argDict, timeRange, narrative, currentLocalTime, shift):
        "Return a list of tuples: forecastType, timeRange "
        # A time period of 0 will be a 1-hour time range but the following
        #   period will also begin at the same start time.

        # "shift" is the number of hours to add to GMT to get local time.
        # All forecasts and start-end times are in GMT time, so the shift is
        # used only for labeling purposes, e.g. Today, Tonight, Monday, etc...

        getFcstDef = argDict["getFcstDef"]
        crange = timeRange
        prevPeriod = 0
        compList = []
        
        # Compute midnight of the creation day
        creationTime = argDict["creationTime"]
        localTime = time.localtime(creationTime)
        year = localTime[0]
        month = localTime[1]
        day = localTime[2]
        midnight = AbsTime.absTimeYMD(year, month, day, 0) - shift  # midnight LT
        
        for subType, period in narrative:
            #print "subType, period", subType, period

            # Determine sub-TimeRange
            if subType == "Custom":
                # Handle custom components - added in OB8.2.
                # "Custom" components are intended to replace "priorPeriod" which is removed.
                # "Custom" component entries in a narrative definition are of the form:
                #     ("Custom", (componentName, timeRange))
                # where timeRange can be (start_hours, end_hours) or an AFPS.TimeRange.
                # Start_hours and end_hours are relative to midnight local time
                # of the product creation date.
                subType, period = period
                if type(period) == types.TupleType and len(period) == 2:
                    startHour, endHour = period
                    compRange = TimeRange.TimeRange(midnight + startHour*3600,
                                               midnight + endHour*3600)
                else:
                    compRange = period
            else:
                # Handle normal component
                # If period is zero, make a 1 hour time range
                if period == 0:
                    duration = 1
                else:
                    duration = period
                if prevPeriod == 0:
                    start = crange.startTime()
                else:
                    start = crange.endTime()
                compRange = TimeRange.TimeRange(start, start + self.hrToSec(duration))
                crange = compRange
                prevPeriod = period

            # Get definition for component
            #print "finding in ForecastNarrative", subType
            if subType == "Phantom":
                found = 1
                argDict["forecastDef"] = {}
            else:
                found, module  = getFcstDef(subType, argDict)
            #print "found"

            if found == 0:
                s = "\nProblem finding or importing Text Product " + \
                  "Definition: " + subType + "\n"
                raise Exception, s
            forecastDef = argDict["forecastDef"]

            # Append to component list
            #print "Appending", subType, compRange, forecastDef
            compList.append((subType, compRange, forecastDef))

        # Re-set argDict
        argDict["forecastDef"] = self.__narrativeDefinition
        return compList

    def __getAnalysis(self, analysis):
        if len(analysis) == 2:
            element, method = analysis
            args = None
        else:
            element, method, args = analysis
        return element, (method, args)

    def __findMethods(self, element, analysisList):
        # Find the entries in the analysisList for the given element
        # and return a list of (method, args) for that element
        methodArgsList = []
        for analysis in analysisList:
            analysisElement, methodArgs = self.__getAnalysis(analysis)
            if element == analysisElement:
                methodArgsList.append(methodArgs)
        return methodArgsList

    def __intersectEditAreas(self, leAreaLabel, argDict, intersectWithAreas):
        #  Make a list of intersections of the local effect area (leAreaLabel) with
        #  all the edit areas in self.__areaList
        intersectAreas = []
        for editArea, areaLabel in intersectWithAreas:
            # Get the intersect name and see if it is in the cache
            intersectLabel = self.getIntersectName(areaLabel, leAreaLabel)
            try:
                intersectArea = self.__areaCache[intersectLabel]
            except:
                leArea = self.findEditArea(None, leAreaLabel)
                intersectArea = self.intersectAreas(intersectLabel, editArea, leArea)
                self.__areaCache[intersectLabel] = intersectArea
            if intersectArea is not None:
                #print "   Appending", intersectLabel
                intersectAreas.append(intersectArea)
            else:
                print "   Empty Intersection, skipping", intersectName
        return intersectAreas

    def __addToRequests(self, argDict, timeRange, componentName,
                        sampleList, samplerRequests, statisticsDict):
        innerList = ArrayList(len(sampleList))
        for element, methodArgs, editArea, areaLabel in sampleList:
            if element not in statisticsDict.keys():
                statisticsDict[element] = {}
            areaDict = statisticsDict[element]
            if areaLabel not in areaDict.keys():
                areaDict[areaLabel] = {}
            trDict = areaDict[areaLabel]
            if timeRange not in trDict.keys():
                trDict[timeRange] = ([], componentName, {})
            parmID = self.__parmID(element)
            innerList.add(SamplerRequest(parmID, editArea, timeRange.toJavaObj()))            
            #print "Adding to sampler Requests", parmID, editArea.id(), timeRange
            methodArgsList, componentName, statDict = trDict[timeRange]
            methodArgsList.append(methodArgs)
        samplerRequests.addAll(innerList)

    def findEditArea(self, editArea, areaLabel):
        # Return given editArea or cached editArea for the given label
        # Add to cache if necessary
        if areaLabel in self.__areaCache.keys():
            return self.__areaCache[areaLabel]
        else:
            if type(editArea) is str or str(editArea).find('Id') == -1:
                #print "Get Edit Area FN: getting edit area 2", areaLabel
                editArea = self.getEditAreas(self.__argDict, [areaLabel])[0]
            self.__areaCache[areaLabel] = editArea
            return editArea

    def __parmID(self, element):
        if element in self.__elementCache.keys():
            return self.__elementCache[element]
        else:
            parmID = self.getParmID(element, self.__databaseID)
            self.__elementCache[element] = parmID
            return parmID

    def __createStatisticsDictionary(self, argDict):
        # Call the SamplerAnalysis methods and expand self.__statisticsDict

        # Set up skeleton tree and node to be used when looking up user-configurable thresholds.
        tree, node = self.getSkeleton(self.__timeRange, None)

        for element in self.__statisticsDict.keys():
            parmID = self.__parmID(element)
            # Get conversion information
            dataType = self.getDataType(element)
            #print "Element", element, dataType
            # Must use product self to get any overrides to the
            # TextRules library that are in standard or local file
            # These may have to be looked up per areaLabel and timeRange if the field
            # wants it (see above), but currently looked up only once per element
            # to save on performance
            productSelf = self.__argDict["self"]
            inUnits = productSelf.element_inUnits(tree, node, element, element)
            outUnits = productSelf.element_outUnits(tree, node, element, element)
            convertMethod = productSelf.getConvertMethod(inUnits, outUnits, element)
            adjustMethod = productSelf.adjust_method(tree, node, element, element)
            roundingMethod = productSelf.rounding_method(tree, node, element, element)
            #rangeInfo = productSelf.getRangeInfo(tree, node, element)
            #
            areaDict = self.__statisticsDict[element]
            for areaLabel in areaDict.keys():
                tree.areaLabel = areaLabel
                node.areaLabel = areaLabel
                editArea = self.findEditArea(None, areaLabel)
                tree.editArea = editArea
                trDict = areaDict[areaLabel]
                keys = trDict.keys()
                for timeRange in keys:
                    node.timeRange = timeRange
                    methodArgsList, componentName, statDict = trDict[timeRange]
                    node.componentName = componentName
                    parmHisto = self.__sampler.getParmHisto(parmID, editArea.getId(), timeRange.toJavaObj())
                    index = 0
                    for methodArgs in methodArgsList:
                        stats = self.__getStatistics(
                            element, methodArgs, parmHisto, timeRange, editArea, componentName)
                        #if element == "WindChill":
                        #    method, args = methodArgs
                        #    print "before conversion ", method.__name__, stats, timeRange, editArea.id()
                        stats = self.__convertStatistics(
                            tree, node, productSelf, stats, dataType,
                            convertMethod, adjustMethod, roundingMethod, element, methodArgs)
                        #if element == "WindChill":
                        #    print "after conversion ", stats
                        self.__storeStatistics(stats, element, methodArgs, timeRange, trDict, index)
                        index = index +1

        # Another pass to remove the methodArgsList, componentName from trDict
        for element in self.__statisticsDict.keys():
            areaDict = self.__statisticsDict[element]
            for areaLabel in areaDict.keys():
                trDict = areaDict[areaLabel]
                for timeRange in trDict.keys():
                    methodArgsList, componentName, statDict = trDict[timeRange]
                    trDict[timeRange] = statDict

        self.__statisticsDictionary = StatisticsDictionary(self.__statisticsDict, self)
        #self.__statisticsDictionary.printDictionary("WindGust")
        #self.__statisticsDictionary.printDictionary("Wind")
        #self.__statisticsDictionary.printDictionary("PoP")
        #self.__statisticsDictionary.printDictionary("Wx")
        #self.__statisticsDictionary.printDictionary("MaxT")
        #self.__statisticsDictionary.printDictionary("WaveHeight")
        return None

    def __getStatistics(self, element, methodArgs, parmHisto, timeRange, editArea, componentName):
        method, args = methodArgs
        if parmHisto.getSampleLen() == 0:
            stats = None
        else:
            if args is not None:
                stats = method(parmHisto, timeRange, componentName, args)
            else:
                stats = method(parmHisto, timeRange, componentName)
            parmName = parmHisto.parmID().compositeNameUI()
            #if element == "SnowAmt":
            #    print "Called method", parmName, editArea.id().name(), method.__name__, timeRange
            #    print "    Result", stats
        return stats

    def __convertStatistics(self, tree, node, productSelf, statsByRange, dataType,
                            convertMethod, adjustMethod, roundingMethod,
                            elementName, methodArgs):
        # Converts the statistics in the statsByRange given the inUnits, outUnits and increment
        # Assumes that stats are either a single value or min/max
        # Vectors can be handled as well.
        # Weather keys are filtered.

        if statsByRange is None:
            return statsByRange
        if dataType == self.DISCRETE():
            return statsByRange

        simpleStatFlag = 0
        # Note: we do not checkTupleLists here since we want hourlyTemp
        # to pass the "isStatsByRange" test.  This way it will go thru
        # the conversion code below which works because
        # hourlyTemp is composed of 2-tuples and the statement:
        #   stats,subRange = statsByRange[i]
        # assigns the hour to the subRange and then works with the stats.
        #print "statsByRange", elementName, statsByRange
        if not self.isStatsByRange(dataType, statsByRange, checkTupleLists=0):
            simpleStatFlag = 1
            statsByRange = [(statsByRange, "")]

        # Check for binnedPercent which cannot be converted
        method, args = methodArgs
        if method.__name__ == "binnedPercent":
            return statsByRange

        numStats = len(statsByRange)
        newList = []
        for i in range(numStats):
            stats, subRange = statsByRange[i]
            if dataType == self.WEATHER():
                #print "stats before filtering", stats
                stats = productSelf.filterSubkeys(tree, node, stats)
                #print "stats after filtering", stats
            else:
                if dataType == self.VECTOR():
                    stats, dir = stats
                if type(stats) is types.TupleType:
                    min, max = stats
                    increment_nlValue = productSelf.increment_nlValue(
                        tree, node, elementName, elementName)
                    if min is not None:
                        min = convertMethod(min)
                        if type(adjustMethod) is types.MethodType:
                            min = adjustMethod(min)
                        min = productSelf.roundValue(
                            min, roundingMethod, "Nearest", increment_nlValue, 0)
                    if max is not None:
                        max = convertMethod(max)
                        if type(adjustMethod) is types.MethodType:
                            max = adjustMethod(max)
                        max = productSelf.roundValue(
                            max, roundingMethod, "Nearest", increment_nlValue, 1)
                    #min, max = productSelf.applyRangeValues(
                    #    tree, node, min, max, elementName, rangeInfo)
                    min, max = productSelf.applyRanges(tree, node, min, max, elementName)
                    stats = (min, max)
                else:
                    if stats is not None:
                        stats = convertMethod(stats)
                        if type(adjustMethod) is types.MethodType:
                            stats = adjustMethod(stats)
                        increment_nlValue = productSelf.increment_nlValue(
                            tree, node, elementName, elementName)
                        stats = productSelf.roundValue(
                            stats, roundingMethod, "Nearest", increment_nlValue, 1)
                if dataType == self.VECTOR():
                    stats = (stats, dir)
            newList.append(((stats), subRange))

        if simpleStatFlag:
            stats, tr = newList[0]
            return stats
        return newList

    def __storeStatistics(self, stats, element, methodArgs, timeRange, trDict, index):
        method, args = methodArgs
        statLabel =  element + "__" + method.__name__
        if index == 0:
            statLabels = ["", statLabel]
        else:
            statLabels = [statLabel]
        #if element == "Hazards":
        #    print "Storing Hazards", stats
        if type(stats) is types.ListType:
            # Expand statsByRange to individual time range entries
            # Skip special cases of hourlyTemp, discrete_percentages,
            # and list of wx or discrete subkeys
            try:
                value, tr = stats[0]
            except:
                tr = 0
            if type(tr) is types.IntType or type(tr) is types.FloatType:
                pass
            else:
                for value, tr in stats:
                    if tr not in trDict.keys():
                        trDict[tr] = ([], "", {})
                    methodArgsList, componentName, statDict = trDict[tr]
                    for statLabel in statLabels:
                        statDict[statLabel] = value
        methodArgsList, componentName, statDict = trDict[timeRange]
        for statLabel in statLabels:
            #if element == "Hazards":
            #    print "storing stats", statLabel, stats
            statDict[statLabel] = stats

    def __createNarrativeTree(self, argDict):
        #Components
        componentList = []
        for componentName, timeRange, definition in self.__compList:
            component = self.makeComponent(componentName, timeRange, definition)
            componentList.append(component)

        # Narrative
        methodList = self.__ut.set(self.__narrativeDefinition, "methodList", [])   
        self.__narrativeTree = Narrative(methodList, componentList,
                                         self.__statisticsDictionary,
                                         self.__issuanceInfo, self, self.__sampler)
        self.__narrativeTree.set("timeRange", self.__timeRange)
        self.__narrativeTree.set("productTimeRange", self.__productTR)
        self.__narrativeTree.set("lineLength", self.__lineLength)
        self.__narrativeTree.set("ifpClient", ["ifpClient"])
        self.__narrativeTree.set("combinations", self.__combinations)
        self.__narrativeTree.set("argDict", self.__argDict)
        for attr, default in [
            ("passLimit", 20),
            ("trace", 0),
            ("troubleList", None),
            ("problemList", None),
            ]:
            val = self.__ut.set(self.__narrativeDefinition, attr, default)            
            self.__narrativeTree.set(attr, val)            
        #self.__narrativeTree.printTree()
        return None

    def getDataType(self, element):
        if element in self.__gpiCache:
            return self.__gpiCache[element]

        parmID = self.__parmID(element)
        try:
            gridParmInfo = self.__ifpClient.getGridParmInfo(parmID)
        except RuntimeError, e:
            # AWIPS-I doesn't throw an error here best I can tell and
            # most of the time when we hit this except it will be because
            # a grid parm was requested that the server doesn't know about.
            # So we will not force an alert to be thrown here.
            # self.log.exception("Could not retrieve GridParmInfo for " + str(parmID.toString()))
            gridParmInfo = GridParmInfo()
        gridType = gridParmInfo.getGridType().toString()
        if gridType == "VECTOR":
            gridType = self.VECTOR()
        elif gridType == "SCALAR":
            gridType = self.SCALAR()
        elif gridType == "WEATHER":
            gridType = self.WEATHER()
        else:
            gridType = self.DISCRETE()
        self.__gpiCache[element] = gridType
        return gridType

    def getLimits(self, element):
        parmID = self.__parmID(element)
        gridParmInfo = self.__ifpClient.getGridParmInfo(parmID)
        return gridParmInfo.getMinValue(), gridParmInfo.getMaxValue()

    def makeComponent(self, componentName, timeRange, definition):
        # Phrases and Subphrases
        phrases = self.__ut.set(definition, "phraseList", [])
        phraseList = []
        for phraseDef in phrases:
            newPhrase = self.makePhrase(phraseDef)
            phraseList.append(newPhrase)
        # Components
        methodList = self.__ut.set(definition, "methodList", [])
        component = Node(phraseList, methodList)
        component.set("timeRange", timeRange)
        component.set("definition", definition)
        component.set("name", componentName)
        if componentName == "Phantom":
            component.set("words", "")
        return component

    def makePhrase(self, phraseDef):
        # Phrases can be a simple method or a tuple containing
        # an optional args list and/or an optional localEffects expressed as
        # a list of LocalEffect objects or as a method
        args = None
        localEffectsList = None
        if type(phraseDef) is types.TupleType:
            if len(phraseDef) == 2:
                phraseDef, object1 = phraseDef
                objects = [object1]
            else:
                phraseDef, object1, object2 = phraseDef
                objects = [object1, object2]
            for object in objects:
                # An object can be:
                #   A local effect expressed as a method
                #   A local effect expressed as a list of LocalEffect objects
                #   A list of arguments
                if type(object) is types.MethodType:
                    localEffectsList = object
                else: # must be list
                    if len(object) == 0:
                        localEffectsList = object
                    else:
                        entry = object[0]
                        if isinstance(entry, self.LocalEffect):
                            localEffectsList = object
                        else:
                            args = object
        phraseDict = phraseDef()
        phraseMethods = self.__ut.set(phraseDict, "phraseMethods", [])
        subPhraseMethods = self.__ut.set(phraseDict, "subPhraseMethods", [])
        # Add wordMethod and setUpMethod to methodLists
        setUpMethod = self.__ut.set(phraseDict, "setUpMethod", None)
        wordMethod = self.__ut.set(phraseDict, "wordMethod", None)

        if 0:
            if setUpMethod is not None:
                if localEffectsList is not None:
                    productSelf = self.__argDict["self"]
                    phraseMethods = [setUpMethod, productSelf.checkLocalEffects] + phraseMethods
                else:
                    phraseMethods = [setUpMethod] + phraseMethods
        else:
            if setUpMethod is not None:
                phraseMethods = [setUpMethod] + phraseMethods


            
        if wordMethod is not None:
            subPhraseMethods = subPhraseMethods + [wordMethod]
        # Phrases can have child phrases
        phraseList = self.__ut.set(phraseDict, "phraseList", [])
        phraseChildren = []
        for childPhrase in phraseList:
            phraseChildren.append(self.makePhrase(childPhrase))
        # Make new node
        phraseNode = Node(phraseChildren, phraseMethods)
        phraseNode.set("phraseDef", phraseDef)
        phraseNode.set("name", phraseDef.__name__)
        phraseNode.set("wordMethod", wordMethod)
        phraseNode.set("setUpMethod", setUpMethod)
        phraseNode.set("subPhraseMethods", subPhraseMethods)
        phraseNode.set("args", args)
        phraseNode.set("localEffectsList", localEffectsList)
        #print "\nMaking new phrase", phraseNode
        #traceback.print_stack(limit=6)

        return phraseNode

    def errorMsg(self, passes):
        if passes > self.__narrativeTree.passes:
            msg =  "\n\nWARNING: TOO MANY PASSES ON TREE. \nTraversal:"
        else:
            msg =  "\n\nWARNING: EMPTY WORDS FROM TREE. Traversal:"
        probPhrases = self.getProblemPhrases()
        msg += "\nPotential problem phrases are:\n" + probPhrases + \
        """

        Try overriding "subPhrase_limit" from WxPhrases and setting it to 10.
        Then report the problem on the listserver.
        Also, see the Text Product User Guide section,
        "Trouble-shooting Narrative Products".

        """
        return "\n" + `passes` + " PASSES. " + msg
    
    def getProblemPhrases(self,
                          attrList=["name", "words", "methodList", "doneList"],
                          ancestorList=["name"]):
        probList = self.__narrativeTree.get("problemList")
        if probList is not None:
            attrList = probList
        probStr = ""
        for node in self.__problemPhrases:
            probStr += "\n From Component: " + str(node.getComponentName())
            for attr in attrList:
                if attr in ["methodList", "doneList"]:
                    probStr = self.addMethods(probStr, node, attr)
                else:
                    probStr += "\n    " + attr + "   " + str(node.get(attr))
            probStr += "\n  Ancestor attributes"
            for ancAttr in ancestorList:
                probStr += "\n    " + ancAttr + "   " + str(node.getAncestor(ancAttr))
        return probStr

    def addMethods(self, probStr, node, attr):
        methodList = node.get(attr)
        probStr += "\n   " + attr
        if methodList is not None:
            for method in methodList:
                probStr += "\n    " + method.func_name
        return probStr

    def errorTraverse(self, node, attrList=["name", "words"], ancestorList=["name"],
                      fixPhrases=1):
        # Print out relevant attributes of each node
        #print "\nNode", node, node.getComponentName()
        #for attr in attrList:
        #    print "    ", attr, node.get(attr)
        #print "   DoneList", node.doneList
        #print "   MethodList", node.methodList
        #print "  Ancestor attributes"
        #for ancAttr in ancestorList:
        #    print "    ", ancAttr, node.getAncestor(ancAttr)
        print node, node.get('name'), node.get('words')
        childList = node.get('childList')
        if childList is None or childList == []:
            print "LEAF NODE"
            if node.get('words') is None:
                print "WITHOUT WORDS!!", node.getAncestor('name')
                errorWords = "|* Please enter " + node.getAncestor('name') + \
                             " and refer to log files for more explanation *|"
                node.set('words', errorWords)
                self.__problemPhrases.append(node)
        else:
            print "  Children"
            for child in node.get('childList'):
                self.errorTraverse(child, attrList=attrList)

    def getSkeleton(self, timeRange, areaLabel):
        tree = Node([],[])
        tree.combinations = self.__combinations
        tree.timeRange = self.__timeRange
        tree.productTimeRange = self.__productTR
        tree.areaLabel = areaLabel
        if areaLabel is not None:
            editArea = self.findEditArea(None, areaLabel)
            tree.editArea = editArea
        tree.library = self
        node = Node([],[])
        node.parent = tree
        node.timeRange = timeRange
        node.areaLabel = areaLabel
        node.componentName = ""
        for compName, compTR, compDefinition in self.__compList:
            if compTR.contains(timeRange):
                node.componentName = compName
                node.componentDef = compDefinition
        return tree, node

    def getMergedStats(self, value, mergeMethod, elementName, timeRange, areaLabel, dataType=None):
        # Merge the stats according to the mergeMethod:
        # Works with either single value or tuple stats (e.g. minMax, medianRange)
        # Works with either simple stats or statsByRange
        productSelf = self.__argDict["self"]

        if value is None:
            return value
        if dataType is None:
            dataType = self.SCALAR()
        if not self.isStatsByRange(dataType, value):
            if mergeMethod == "List":
                return [(value, timeRange)]
            else:
                if dataType == self.SCALAR() or dataType == self.VECTOR():
                    return self.getValue(value, mergeMethod, dataType)
                else:
                    return value
        else:
            # Take only subRanges that overlap time range
            #print "statsByRange", elementName, mergeMethod, timeRange, value
            value = self.screenStatsByRange(value, timeRange)
            if  mergeMethod == "List":
                return value
            elif mergeMethod == "MergeBins":
                return self.mergeBins(value, timeRange)
            else:
                if value == []:
                    return None
                #print "value for ", elementName, timeRange, areaLabel
                #print "     value", value
                # For performance, we spell out all the cases
                if dataType == self.SCALAR():
                    if mergeMethod == "Min":
                        val = None
                        for stats, subRange in value:
                            if stats is None:
                                continue
                            stats = self.getValue(stats, "Min")
                            if val is None or stats < val:
                                val = stats
                        return val
                    elif mergeMethod == "Max":
                        val = None
                        for stats, subRange in value:
                            if stats is None:
                                continue
                            stats = self.getValue(stats, "Max")
                            if val is None or stats > val:
                                val = stats
                        return val
                    elif mergeMethod == "MinMax":
                        min = None
                        max = None
                        for stats, subRange in value:
                            if stats is None:
                                continue
                            min1, max1 = self.getValue(stats, "MinMax")
                            if min1 < min or min is None:
                                min = min1
                            if max1 > max or max is None:
                                max = max1
                        if min is None or max is None:
                            return None
                        tree, node = self.getSkeleton(timeRange, areaLabel)
                        min, max = productSelf.applyRanges(tree, node, min, max, elementName)
                        return (min, max)
                    else: # Handle sum or average
                        sum = 0
                        count = 0
                        for stats, subRange in value:
                            if stats is None:
                                continue
                            stats = self.getValue(stats, "Average")
                            sum = sum + stats
                            count += 1
                        if count == 0:
                            return None
                        if mergeMethod == "Sum":
                            return sum
                        else: # Average
                            average = float(sum)/count
                            tree, node = self.getSkeleton(timeRange, areaLabel)
                            return productSelf.roundStatistic(tree, node, average, elementName)
                elif dataType == self.VECTOR():
                    #  Note that in these cases, a mag, dir is returned, but
                    #  the dir is simply taken from the last stat pair so that
                    #  it is somewhat meaningless
                    if mergeMethod == "Min":
                        val = None
                        for stats, subRange in value:
                            if stats is None:
                                continue
                            stats, dir = stats
                            stats = self.getValue(stats, "Min")
                            if val is None or stats < val:
                                val = stats
                        if val is None:
                            return None
                        return (val, dir)
                    elif mergeMethod == "Max":
                        val = None
                        for stats, subRange in value:
                            if stats is None:
                                continue
                            stats, dir = stats
                            stats = self.getValue(stats, "Max")
                            if val is None or stats > val:
                                val = stats
                        if val is None:
                            return None
                        return (val, dir)
                    elif mergeMethod == "MinMax":
                        min = None
                        max = None
                        for stats, subRange in value:
                            if stats is None:
                                continue
                            stats, dir = stats
                            min1, max1 = self.getValue(stats, "MinMax")
                            if min1 < min or min is None:
                                min = min1
                            if max1 > max or max is None:
                                max = max1
                        if min is None or max is None:
                            return None
                        tree, node = self.getSkeleton(timeRange, areaLabel)
                        min, max = productSelf.applyRanges(tree, node, min, max, elementName)
                        return ((min, max), dir)
                    else: # Handle sum or average
                        sum = 0
                        count = 0
                        for stats, subRange in value:
                            if stats is None:
                                continue
                            stats, dir = stats
                            stats = self.getValue(stats, "Average")
                            sum = sum + stats
                            count += 1
                        if count == 0:
                            return None
                        if mergeMethod == "Sum":
                            return (sum, dir)
                        else: # Average
                            average = float(sum)/count
                            tree, node = self.getSkeleton(timeRange, areaLabel)
                            return (productSelf.roundStatistic(tree, node, average, elementName), dir)
                elif dataType == self.WEATHER():
                    # Weather:  add up all subkeys/remove duplicates by
                    #     making wxkey and re-making subkeylist
                    #     Then filter subkeylist
                    subkeyList = []
                    for stats, subRange in value:
                        if stats is None:
                            continue
                        subkeyList = subkeyList + stats
                    tree, node = self.getSkeleton(timeRange, areaLabel)
                    #subkeyList = productSelf.combineSubKeys(tree, node, subkeyList)
                    #print "subkeyList before", subkeyList
                    subkeyList = productSelf.filterSubkeys(tree, node, subkeyList)
                    #print "subkeyList after", subkeyList
                    return subkeyList
                else:
                    # Discrete: add up all keys/ need to remove duplicates?
                    keyList = []
                    for stats, subRange in value:
                        if stats is None:
                            continue
                        keyList = keyList + stats
                    return keyList

    def isStatsByRange(self, dataType, value, checkTupleLists=1):
        if dataType == self.VECTOR() or dataType == self.SCALAR():
            if type(value) is types.ListType:
                if checkTupleLists:
                    # Look for special cases like hourlyTemp which
                    # does not return statsByRange, but is list of
                    # tuples
                    try:
                        firstValue = value[0]
                        subRange = firstValue[1]
                        if type(subRange) is types.IntType or \
                           type(subRange) is types.FloatType:
                            return 0
                    except:
                        pass
                return 1
            else:
                return 0
        else:
            # Need to check more closely for Weather and Discrete
            # Possibilities:
            #    **dominantWx: list of subkeys : return 0
            #    **rankedWx: list of (subkey, rank) tuples : return 0
            #    **dominantDiscreteValue: list of discrete keys: return 0
            #    discretePercentages: list of (key, percentage) tuples: return 0
            #    discreteTimeRangesByKey: list of (key, timerange) tuples: return 1
            #print "\ngot here", value
            if type(value) is types.ListType and len(value) > 0:
                try:
                    stats, tr = value[0]                    
                    if isinstance(tr, TimeRange.TimeRange):                                            
                        return 1
                    else:
                        #print "returning0 0"
                        return 0
                except:
                    #print "returning1 0"
                    return 0
            else:
                #print "returning2 0"
                return 0

    def screenStatsByRange(self, statsByRange, timeRange):
        newStatsByRange = []
        for stats, subRange in statsByRange:
            if subRange.overlaps(timeRange):
                newStatsByRange.append((stats, subRange))
        return newStatsByRange

    def mergeBins(self, value, timeRange):
        # Value is statsByRange of binLists i.e. (binList, subRange).
        # Each binList consists of tuples: (low, high, percent)
        # timeRange is the time range over which to merge bins
        # Return one merged time-weighted binList
        if value is None or len(value) == 0:
            return None
        newPercents = []
        newBins = []
        binList, subRange = value[0]
        if binList is None:
            return None
        numBins = len(binList)
        for bin in binList:
            low, high, percent = bin
            newPercents.append(0.0)
            newBins.append((low, high))

        for binList, subRange in value:
           # print "binList, subRange", binList, subRange
            if binList is None:
                continue
            weight = float(subRange.duration())/timeRange.duration()
            # If time range is greater than subRange, give a weight of 1
            # so that the percentage never exceeds 100.
            if weight > 1.0:
                weight = 1.0
            for i in range(numBins):
                low, high, percent = binList[i]
                newPercents[i] += percent * weight

        # Glue bin values to merged percentages
        for i in range(numBins):
            low, high = newBins[i]
            newBins[i] = ((low, high, newPercents[i]))
        return newBins
