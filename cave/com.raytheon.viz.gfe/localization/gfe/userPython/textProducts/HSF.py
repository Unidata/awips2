#-------------------------------------------------------------------------
# Description: HSF (High Seas Forecast)
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Version: 26 July 2016 - Received from Jeff Lewitsky
##
#
# SOFTWARE HISTORY
#  Date        Ticket#    Engineer    Description
#  ----------- ---------- ----------- --------------------------
#  07/29/2016   -         tlefebvre   Changed edit area retrieval and storage to work
#                                     outside CAVE so edit areas could be shared.
#  12/20/2017  DCS17686   tlefebvre   Initial baseline version.
#
##
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# HSF.py, HSF _<site>_<MultiPil>_Definition, HSF_<site>_Override
#-------------------------------------------------------------------------
# Customization Points:
#
# DEFINITION SECTION
#
# Required Configuration Items:
#
#  displayName      If not None, defines how product appears in GFE GUI
#  defaultEditAreas defines edit areas, default is Combinations
#
#  productName      defines name of product e.g. "COASTAL WATERS FORECAST"
#  fullStationID    Full station identifier, 4 letter, such as "KSLC".
#  wmoID            WMO ID code for product header, such as "FOUS45"
#  pil              Product pil, such as "OFFBOS"
#  areaName (opt.)  Area name for product header, such as "WESTERN NEW YORK"
#  wfoCityState     City,state that the WFO is located in, such as "BUFFALO, NY"
#
#  synopsisUGC      UGC code for Synopsis
#  synopsisHeading  Heading for Synopsis
#
# Optional Configuration Items
#
#  editAreaSuffix      default None. Allows for generating the body of the product for
#                      an edit area that is a subset (e.g. population areas) of the
#                      edit areas specified in the defaultEditAreas.  So given the edit area,
#                      "COZ035" and the editAreaSuffix is "_pt", then the edit area that
#                      will be sampled and reported for the body of the product will be
#                      "COZ035_pt".  If no such edit area exists, the system will simply
#                      use the original edit area.
#                      Note that Hazards will always be generated for the entire edit area.
#  mapNameForCombinations Name of the map background that is used for 
#                         creating/editing the combinations file.  This must 
#                         be defined or the GFE zone combiner
#  database               Source database for product. Can be "Official", 
#                         "Fcst" or "ISC"
#  outputFile             Defines the output location of the finished product
#                         when saved from the Formatter Launcher.
#  debug                  If on, debug_print statements will appear.
#  textdbPil              Defines the awips product identifier 
#                         (e.g., DENCCFDEN) that is used to store the product 
#                         in the AWIPS text database. 
#                         This value is also used for the default GUI entry for 
#                         storage.
#  awipsWANPil            Defines the awips product identifier 
#                         (e.g., KBOUCCFDEN) that is used to transmit the 
#                         product to the AWIPS WAN.
#                         This value is also used for the default GUI 
#                         entry for storage.
#  hazardSamplingThreshold  Defines the percentage coverage or number of
#                    grid points in a zone that must contain the hazard
#                    in order for it to be considered. Tuple (percent, points)
#
#  periodCombining     If 1, an attempt will be made to combine components
#                      or time periods into one.  Otherwise no period 
#                      combining will will be done.
#  includeEveningPeriod   Include a 6 hour Evening period on the 3rd day
#  useAbbreviations
#      If 1, use marine abbreviations e.g. TSTM instead of THUNDERSTORM, 
#      NW instead of NORTHWEST
#      (See marine_abbreviateText in the TextRules module)
#
#  Weather-related flags
#       hoursSChcEnds        - specifies hours past the beginning of the first
#                              first period of the product to stop including 'Slight
#                               Chance' or 'Isolated' weather types (ERH policy
#                               allows values of 1-5 * 12 hour periods)               
#
#  areaDictionary    Modify the AreaDictionary utility with UGC 
#                    information about zones
#
#  useHolidays              Set to 1 to use holidays in the time period labels
#
#  Trouble-shooting items
#    passLimit -- Limit on passes allowed through Narrative Tree
#    trace     -- Set to 1 to turn on trace through Narrative Tree   
#
# OVERRIDES
#
# Required Overrides
#
#  _Text1(), _Text2()  Descriptive text for header
#
# NARRATIVE CUSTOMIZATION POINTS
#   The phrases in this product can be customized in many ways by overriding
#   infrastructure methods in the Local file.
#   You will see common overrides in the Local file and you may change them
#   in that there.
#   For further customization, you can determine  which phrases your product is
#   using by examining the Component Product Definitions below.
#   Then, you can look up the phrase in the Text Product User Guide which will
#   describe the all the relevant override methods associated with the phrase.
#   Refer to the Customization section of the Text Product User Guide
#   for step-by-step information.
#
#-------------------------------------------------------------------------
# Weather Elements Needed:
#    Wind (every 3 hours to 3 days, then every 6 hours to 7 days)
#    WaveHeight and/or WindWaveHgt
#         (every 6 hours to 3 days, then every 12 hours to 7 days)
#    Wx (every 6 hours to 3 days, then every 12 hours to 7 days)
#    Optional:
#       WindGust (every 3 hours to 7 days)
#       Swell, Swell2, Period, Period2 (every 6 hours to 7 days)
#-------------------------------------------------------------------------
# Edit Areas Needed: None
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file:
# Combinations
#-------------------------------------------------------------------------
# Component Products:
#      OFFPeriod (component)
#      OFFPeriodMid (component)
#      OFFExtended (component)
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
#
# To look up tasks and their status, see the Text Product User Guide
# Section on "Tkgnats: Task Reporting System".
#-------------------------------------------------------------------------
# Additional Information:
#
#   COMMON OVERRIDES
#     from OFF:
#       _Text1
#       _Text2
#       _issuance_list
#       riverBarForecast_dict
#     from MarinePhrases
#       inlandWatersAreas
#       inlandWatersWave_element
#       seasWaveHeight_element
#       seasWindWave_element
#       waveHeight_wind_threshold
#       marine_wind_flag      
#       marine_wind_combining_flag      
#       marine_wind_verbose_flag      
#     from ConfigVariables
#       phrase_descriptor_dict
#       phrase_connector_dict
#       null_nlValue_dict
#       first_null_phrase_dict
#       null_phrase_dict
#       maximum_range_nlValue_dict
#       combine_singleValues_flag_dict
#     from WxPhrases:
#       embedded_visibility_flag
#       visibility_wx_threshold
#       significant_wx_visibility_subkeys
#       wxCoverageDescriptors
#       wxTypeDescriptors
#       wxAttributeDescriptors
#       wxIntensityDescriptors
#       wxCombinations
#       combine_T_RW
#     from SampleAnalysis
#       moderated_dict
#-------------------------------------------------------------------------
# Example Output:
#  Refer to the NWS Directives for Marine Services.
#-------------------------------------------------------------------------

import TextRules
import SampleAnalysis
import time, re, pickle, os, textwrap
import TimeRange
import AbsTime
from math import *
import numpy
import UserInfo
import subprocess
import xml.etree.ElementTree as ET
import EditAreaUtilities

from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID
#from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData_CoordinateType as CoordinateType

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
        i = 0
        while node.getParent() is not None and i < 100:
            prevNode = node
            node = node.getParent()
            i = i + 1
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
        print("Node", node)
        print(indentStr + "  Methods")
        for method in node.methodList:
            if method in node.doneList:
                done = "DONE"
            else:
                done = ""
            print(indentStr + "    ", method.__name__, done)
        print(indentStr + "  Attributes")
        dict = node.__dict__
        for key in dict:
            if key == "methodList" or key == "doneList":
                continue
            print(indentStr + "    ", key, dict[key])
        print(indentStr + "  Children ", len(node.childList))
        for child in node.childList:
            self.printNode(child, indentStr + "    ")

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
                for key in statDict:
                    for element in self.parent.disabledElements:
                        if key == element:
                            statDict[element] = None
            disabledSubkeys = self.getAncestor("disabledSubkeys")
            #print "disabledSubkey", disabledSubkeys
            if disabledSubkeys is not None:
                disabledWxTypes = []
                for disabledSubkey in disabledSubkeys:
                    disabledWxTypes.append(disabledSubkey.wxType())
                for key in statDict:
                    if key == "Wx":
                        subkeys = statDict[key]
                        newList = []
                        for subkey in subkeys:
                            # Need to handle both "dominantWx" and
                            # "rankedWx" analysis
                            appendVal = subkey
                            if type(subkey) is tuple:
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
        print("\n\nNarrative Tree\n")
        self.printNode(self, "")
    def getTopoHisto(self, areaLabel):
        editArea = self.library.findEditArea(None, areaLabel)
        return self.get("histoSampler").getTopoHisto(editArea.id())
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
            if type(newVal) is list:
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

class Statistics:
    def __init__(self, statDict):
            self._statDict = statDict
    def get(self, element, timeRange, areaLabel=None, statLabel="", mergeMethod="List",
            intersectWith=None):
            return self._statDict.get(element)

class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = []
    Definition =  {
        "type": "smart",
        "displayName": "None",
        "database": "Official",
        # Defines output location of finished product.
        "outputFile": "{prddir}/TEXT/HSF_<MultiPil>.txt",
        "debug": 0,
        # Name of map background for creating Combinations
        "mapNameForCombinations": "Marine_Zones_<site>", 

        "lineLength": 69,
        ## Edit Areas: Create Combinations file with edit area combinations.
        "defaultEditAreas" : "Combinations_OFF_<site>_<MultiPil>",
        "editAreaSuffix": None,
        # product identifiers
        "productName": "HIGH SEAS FORECAST", # product name 
        "fullStationID": "<fullStationID>",    # full station identifier (4letter)
        "wmoID": "<wmoID>",          # WMO ID
        "pil": "<pil>",            # Product pil
        "areaName": "<state>",             # Name of state, such as "GEORGIA" -- optional
        "wfoCityState": "<wfoCityState>",   # Location of WFO - city state

        "synopsisUGC": "",                # UGC code for synopsis
        "synopsisHeading": ".SYNOPSIS...",# Heading for synopsis
        
        "textdbPil": "<textdbPil>",       # Product ID for storing to AWIPS text database.
        "awipsWANPil": "<awipsWANPil>",   # Product ID for transmitting to AWIPS WAN.

        "hazardSamplingThreshold": (0, 1),  #(%cov, #points)

        "fixedExpire": 1,       #ensure VTEC actions don't affect segment expiration time

        "periodCombining" : 0,       # If 1, combine periods, if possible
        # Product-specific variables:
        # Set to one if you want a 6-hour evening period instead of
        # 18-hour period without lows
        "includeEveningPeriod": 0,
        "useAbbreviations": 1,
        
        "ccc": "MIA",               # AFOS node
        "tcmBasin": "EP",           # AT = Atlantic, EP = East Pacific, CP = Central Pacific
        
        # CCode flag - added for OPC 11/14/2017 CNJ
        "ccode": 0,

        # Weather-related flags
        "hoursSChcEnds": 24,        
        
        # Area Dictionary -- Descriptive information about zones
        "areaDictionary": "AreaDictionary", 
        "useHolidays": 0,            # Set to 1 to use holidays in the time period labels
        # Language
        "language": "english",

        # Trouble-shooting items
        "passLimit": 20,             # Limit on passes allowed through
                                     # Narrative Tree
        "trace": 0,                  # Set to 1 to turn on trace through
                                     # Narrative Tree for trouble-shooting
        # Mixed Case
        # LowerCase below needs to = 1 for mixed case AND must change mixedCaseProductIds.txt file under TextWS for store to not upper all -JL 05/24/2016
        # Also will need to change all hard-coded phrases AND phrases passed from MakeHSEditAreas changed to mixed case
        "lowerCase": 0, 
        "autoStore": 0,                                             
        }

    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)
        
        #editAreasPath = "/scratch/local/HighSeas/EditAreas/"   # for development in Boulder       
        editAreasPath = "/data/local/HighSeas/NH2/EditAreas/"       # for operations
        self._eaUtils = EditAreaUtilities.EditAreaUtilities(editAreasPath)
                
        
    def _Text1(self):
        return "SUPERSEDED BY NEXT ISSUANCE IN 6 HOURS\n\n" + \
               "SEAS GIVEN AS SIGNIFICANT WAVE HEIGHT...WHICH IS THE AVERAGE\n" + \
               "HEIGHT OF THE HIGHEST 1/3 OF THE WAVES. INDIVIDUAL WAVES MAY BE\n" + \
               "MORE THAN TWICE THE SIGNIFICANT WAVE HEIGHT.\n\n"
               
    # override _Text2 for each specific basin/product
    def _Text2(self):
        return "ATLANTIC FROM 07N TO 31N W OF 35W INCLUDING CARIBBEAN SEA AND\n" + \
               "GULF OF MEXICO\n\n"
#    def _Text2(self):
#         return "E PACIFIC FROM THE EQUATOR TO 30N E OF 140W AND 03.4S TO THE\n" + \
#                "EQUATOR E OF 120W\n\n"

    # Returns the specified product (string) with newlines inserted
    # such that no line exceeds maxChars characters.
    def _wrapLines(self, product, maxChars=64):

        # break out the product into lines
        lineList = []
        startPos = 0
        while startPos < len(product):
            pos = product.find("\n", startPos)
            if pos == startPos:
                lineList.append("")
            elif pos == -1:
                lineList.append(product[startPos:])  #get the rest
                break
            
            line = product[startPos:pos]   # slice out line
            wrappedLines = textwrap.wrap(line, maxChars)
            for w in wrappedLines:
                lineList.append(w)

            startPos = pos + 1

        finalProduct = ""
        for line in lineList:
            finalProduct = finalProduct + line + "\n"

        return finalProduct

    # Top-level object that calls all main subroutines
    def generateForecast(self, argDict):

        print("Generate Forecast")

        # baseline code - gets variables from the Definitions
        error = self._getVariables(argDict)
        if error is not None:
            return error
        
        # Determine time ranges - issuance times set here
        error = self._determineTimeRanges(argDict)
        if error is not None:
            return error
        
        # Creating the Features class from MakeHSFEditAreas tool input
        self._createFeatures(argDict)
        
        # Creating the Features class from grid-based tool input
        self._createGridBasedFeatures(argDict)
        
        # Creating the Features class from TCM conversion script input      
        self._createTCM_BasedFeatures(argDict)
        
        # Creating the Features class from VGF / XML input
        #self._createDrawableFeatures(argDict)

        
        print("Sampling data")
        # Sample the data for the areas in the Features created above
        error = self._sampleData(argDict)
        if error is not None:
            return error
        
        # Populate the Features with the sampled data
        self._populateFeatures(argDict)
        
        # Order the Features based on rules for the HSF product
        self._orderFeatures()

        #for feature in self._features:
        #    feature.printFeature()

        # Building the forecast text string
        fcst = ""
        fcst = self._preProcessProduct(fcst, argDict)
        fcst = self._makeSection(fcst, argDict, self._warningFeatures, "Warning")
        fcst = self._makeSection(fcst, argDict, self._synopsisFeatures, "Synopsis")
        fcst = self._postProcessProduct(fcst, argDict)
        
        return fcst

    # sets variables from the Definitions
    def _getVariables(self, argDict):
        # Make argDict accessible
        self.__argDict = argDict

        # Get Definition variables
        self._definition = argDict["forecastDef"]
        for (key, value) in self._definition.items():
            setattr(self, f"_{key}", value)

        # Get VariableList and _issuance_list variables
        varDict = argDict["varDict"]
        for (key, value) in varDict.items():
            if type(key) is tuple:
                label, variable = key
                setattr(self, f"_{variable}", value)

        self._databaseID = argDict["databaseID"]
        self._ifpClient =  argDict["ifpClient"]
                            
        self._language = argDict["language"]
        return None

    # Sets up issuance times - can be done in dictionary elsewhere
    # This method or dictionary goes into overrides to change issuance times for each product
    def _determineTimeRanges(self, argDict):
        
        # Calculate current times
        self._ddhhmmTime = self.getCurrentTime(
            argDict, "%d%H%M", shiftToLocal=0, stripLeading=0)
        staticIssueTime=re.sub(r'(\d{3,4} [AP]M).*',r'\1',self._productIssuance)
        self._timeLabel =  staticIssueTime + " " + self.getCurrentTime(
            #argDict, " %a %b %e %Y", stripLeading=1) #commented out per 00 UTC issue per M. Sardi (JL 07/21/16)
            argDict, " %a %b %e %Y", shiftToLocal=0, stripLeading=1)
        # Re-calculate issueTime
        self._issueTime = self.strToGMT(staticIssueTime)
        validTimeDict = {
            "0430 UTC": 430,
            "1030 UTC": 1030,
            "1630 UTC": 1630,
            "2230 UTC": 2230,
            }
        validTime = validTimeDict[self._productIssuance] - 430
        self._validTime = repr(validTime).zfill(4) + " UTC"
        return None

    ##############
    # Organize Features
        
    class Feature:
        def __init__(self):
            self.name = None
            self.basinName = None
            # Feature Type -- 'Named', 'GridBased', 'TCM_Based', 'Drawable'
            #     e.g. (Fog / Visibility, Convection, Heavy Freezing Spray)
            self.featureType = None 
            self.periods = []
            
            self.highestWarning = None
            self.highestWarningTimePeriod = None
            self.highestWarningHeadline = None
            self.earliestTimePeriod = None
            self.wxType = None
            self.wxIntensity = None
            self.wxCoverage = None
            self.phenomenonType = None
            self.GBareaList = None
                                                
            # Drawable Feature -- ingest from Drawable Features XML 
            self.drawableFeature = None
            self.autoText = None
        def printFeature(self):
            print("\nFeature -- Feature Type, Basin Name:", self.featureType, self.basinName)
            print("HighestWarning, HighestWarning TimePeriod:", self.highestWarning, self.highestWarningTimePeriod)
            print("wxType:", self.wxType, "wxCoverage:", self.wxCoverage, "wxIntensity:", self.wxIntensity)
            print("phenomenonType:", self.phenomenonType)
            
            if self.featureType == "GridBased":
                for area in self.GBareaList:
                    print("areaName:", area.areaName)
                    print("areaLabel:", area.areaLabel)
                    print("timePeriod:", area.timePeriod)
                    print("headline:", area.headline)
                    print("warningType:", area.warningType)
                    print("phenomenonType:", area.phenomenonType)
                    print("wxType:", area.wxType)
                    print("intensity:", area.intensity)
                    print("coverage:", area.coverage)
            else:
                print("periods:", self.periods)


            #print "  Periods"
            #for period in self.periods:
            #    period.printPeriod()
          
    class Period:
        def __init__(self):
            self.timePeriod = None
            self.areas = []
            self.drawables = []
        def printPeriod(self):
            print('    TimeRange', self.timePeriod)
            print('    Areas')
            for area in self.areas:
                area.printArea()
            for drawable in self.drawables:
                drawable.printDrawable()
          
    class Area:
        def __init__(self):
            self.areaName = None
            self.areaLabel = None
            self.refData = None
            self.timePeriod = None
            
            # Named Feature attributes
            self.statDict = {}
            self.headline = "None"
            self.methodList = []
            self.windWave = False
            self.windOnly = False
            self.waveOnly = False
            self.warningType = None
            
            # GridBased Feature attributes
            self.phenomenonType = None
            self.wxType = None
            self.intensity = None
            self.coverage = None
            
        def printArea(self):
            print('       name, label', self.areaName, self.areaLabel)
            print('       warningType', self.warningType)
            print('       windWave, windOnly, waveOnly', self.windWave, self.windOnly, self.waveOnly)
            print('       statDict', self.statDict)
            print('       wxType, intensity', self.wxType, self.intensity)
            
    class Drawable:
        def __init__(self):
            self.timePeriod = None
            self.drawableType = None
            self.pressureTag = None
            self.latLons = None
            self.movement = None 
        def printDrawable(self):
            print('      drawableType', self.drawableType)
            print('      timePeriod, pressureTag', self.timePeriod, self.pressureTag)
            print('      latLons', self.latLons)           
            
    def _createFeatures(self, argDict):
        ''' Set up 'skeleton' Feature objects from toolFeatures
             Input toolFeatures:
             [
             {'timePeriod': '00h', 'featureName': 'Feature1', 'basin': 'ATLC',
             'areaList': [
                      {'lon': None, 'pieState': None, 'radius': None, 'lat': None,
                         'areaDesc': 'WITHIN AREA BOUNDED BY 15S170E TO 15S169E TO 16S168E TO 17S169E TO 16S170E TO 15S170E', 
                         'basin': 'HSF_SP', 'areaName': 'Feature1_00h_EA1'},
                      {'lon': None, 'pieState': None, 'radius': None, 'lat': None,
                        'areaDesc': 'WITHIN AREA BOUNDED BY 15S171E TO 14S170E TO 15S168E TO 17S168E TO 18S169E TO 17S171E TO 15S171E',
                        'basin': 'HSF_SP', 'areaName': 'Feature1_00h_EA2'}, 
                      {'lon': None, 'pieState': None, 'radius': None, 'lat': None,
                       'areaDesc': 'WITHIN AREA BOUNDED BY 13S171E TO 13S169E TO 14S168E TO 17S167E TO 18S170E TO 16S172E TO 13S171E', 
                       'basin': 'HSF_SP', 'areaName': 'Feature1_00h_EA3'}
                       ]
                }
              ] 
        '''    
        
        # Call a sequence of method in order to get a gridLoc
        # We need his to process edit areas.
        parmNameLevel = "Wind_SFC"
        self.setUp(parmNameLevel, argDict)
        self._gridLoc = self.getGridLoc()
        self._dataMgr = argDict["dataMgr"]
        
        print("GridLoc:", dir(self._gridLoc))


        self._savePathFile = self.descriptorFileName()
        print("descFileName:", self._savePathFile)
        # Try to fetch the old object
        try:
            with open(self._savePathFile, "rb") as f:
                toolFeatures = pickle.load(f)
        except:
            toolFeatures = []
            print("Starting with an empty descriptor.")
                
        for f in toolFeatures:
            print(f)

        # Initialize windMax across features
        self._windMax = 0.0
        featureNameList = self._getFeatureNames(toolFeatures)
        self._features = []
        for featureName, basinName in featureNameList:
            print("Createfeature basinName:", basinName)
            feature = self.Feature()
            self._features.append(feature)
            feature.name = featureName
            feature.basinName = basinName
            feature.featureType = 'Named'
            feature.periods = [] 
            
            toolTimePeriodList = self._getTimePeriodList(featureName, toolFeatures)
            
            for toolTimePeriod in toolTimePeriodList:
                period = self.Period()
                feature.periods.append(period)
                
                toolAreaList = self._getAreaList(featureName, toolTimePeriod, toolFeatures)
                period.timePeriod = self._convertToTimeRange(toolTimePeriod)
                
                period.areas = []
                for areaName, areaDesc in toolAreaList:
                    area = self.Area()
                    period.areas.append(area)
                    area.areaName = areaName
                    print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^AREA NAME IS:", area.areaName)
                    area.refData = None
                    area.areaLabel = areaDesc
                    area.timePeriod = period.timePeriod
                    print("*****************************area desc is:", area.areaLabel)
                self._processAreaOverlaps(period.areas)

    def _getFeatureNames(self, toolFeatures):
        nameList = []
        featureList = []
        for t in toolFeatures:
            print("Tool feature:", t)
            featureName = t.get("featureName")
            if featureName not in nameList:
                featureList.append((t.get("featureName"), t.get('basin')))
                nameList.append(featureName)
        return featureList
                 
    def _getTimePeriodList(self, featureName, toolFeatures):
        tpList = []
        for t in toolFeatures:
            if t["featureName"] != featureName:
                continue
            if t["timePeriod"] not in tpList:
                tpList.append(t["timePeriod"])
        tpList.sort()
        return tpList

    def _getAreaList(self, featureName, timePeriod, toolFeatures):
        areaList = []
        #print "getting area list:", featureName, timePeriod
        for t in toolFeatures:
            if t["featureName"] != featureName or t["timePeriod"] != timePeriod:
                continue
            toolAreaList = t["areaList"]
            for area in toolAreaList:
                areaTuple = (area["areaName"], area["areaDesc"])
                areaList.append(areaTuple)        
        return areaList

    def _createGridBasedFeatures(self, argDict):
        # Add gridBasedFeatures to self._features

        
        # open the file
        self._gridBasedPathFile = self.gridBasedFileName()
        # Try to fetch the old object
        try:
            with open(self._gridBasedPathFile, "rb") as f:
                gridBasedFeatureList = pickle.load(f)
        except:
            gridBasedFeatureList = []
            print("Starting with an empty gridBasedFeature descriptor.")
        
        ### below this is the new code - 11/16/2017 CNJ ###
        for gridBasedFeature in gridBasedFeatureList:  # step through the feature list from the tool
            feature = self.Feature()
            feature.featureType = 'GridBased'
            feature.wxType = gridBasedFeature[0]["wxType"]
            feature.wxIntensity = gridBasedFeature[0]["intensity"]
            feature.wxCoverage = gridBasedFeature[0]["wxCoverage"]

            feature.phenomenonType = self._phenomenonTypeDict()[gridBasedFeature[0]["wxType"]]

            feature.earliestTimePeriod = self._convertToTimeRange(gridBasedFeature[0]["timePeriod"])
            # Populate the areas in this feature
            feature.GBareaList = []
            feature.periods = []
            for gbArea in gridBasedFeature:  
                feature.periods.append(self._convertToTimeRange(gridBasedFeature[0]["timePeriod"]))
                area = self.Area()
                area.areaName = gbArea["areaName"] 
                area.areaLabel = gbArea["areaDesc"] 
                area.timePeriod = self._convertToTimeRange(gbArea["timePeriod"])
                area.wxType = gbArea["wxType"] 
                area.intensity = gbArea["intensity"] 
                area.coverage = gbArea["wxCoverage"] 
                area.phenomenonType = feature.phenomenonType
                feature.GBareaList.append(area)
                # Populate the warning attributes
                if feature.wxType == "VA":
                    feature.highestWarning = "Ashfall"
                    area.warningType = "Ashfall"
                    feature.warningTimePeriod = feature.earliestTimePeriod
                    feature.highestWarningHeadline = self._getGridBasedHeadline(area)
                elif feature.wxType == 'ZY' and feature.wxIntensity == '+':
                    feature.highestWarning = "Heavy Freezing Spray"
                    area.warningType = "Heavy Freezing Spray"
                    feature.warningTimePeriod = feature.earliestTimePeriod
                    feature.highestWarningTimePeriod = feature.earliestTimePeriod
                    feature.highestWarningHeadline = self._getGridBasedHeadline(area)
            
            self._features.append(feature)                           


                #feature.printFeature()
        return

    def _getGridBasedHeadline(self, area):
        if area.wxType == 'ZY' and area.intensity == '+':
            return "...Heavy Freezing Spray Warning..."
        elif area.wxType == 'VA':
            return "...ASHFALL ADVISORY...\n[VOLCANO NAME] VOLCANO AT POSITION " + \
                    "[xx.xN xx.xW] IS CURRENTLY IN A STATE OF UNREST AND COULD ERUPT WITH " + \
                    "LITTLE NOTICE. MARINERS TRAVELING IN THE VICINITY OF [VOLCANO NAME] " + \
                    "ARE URGED TO EXERCISE CAUTION. IF MARINERS ENCOUNTER VOLCANIC ASH OR " + \
                    "FLOATING VOLCANIC DEBRIS...YOU ARE ENCOURAGED TO REPORT THE OBSERVATION " + \
                    "TO THE NATIONAL HURRICANE CENTER BY CALLING 305-229-4424.\n"
        return ""
    
    def _createTCM_BasedFeatures(self, argDict):
        # Create Feature classes from TCM conversion script input
        
        ccc = "MIA"    
        siteID = "AT"
        tcmBody=""
        for index in ["1", "2", "3", "4", "5"]:
        #for index in [tcm1, tcm2, tcm3]:
            pil = ccc + "WRK" + siteID + index
            tcmText = subprocess.check_output(["/awips2/fxa/bin/textdb", "-r", pil]).decode()
            
            tcmLines = tcmText.split('\n')
            tcmTimeStr = tcmLines[0]   # "2100 UTC FRI JAN 15 2016"
            if not self._tcmTimeOverlaps(tcmTimeStr):
                continue

            tcmBegin = tcmLines[2]
            tcmBody = "\n".join(tcmLines[2:]) 
            
            warningDict = {
                    "Hurricane": "...Hurricane Warning...",
                    "Hurricane Force": "...Hurricane Force Wind Warning...",
                    "Tropical Storm": "...Tropical Storm Warning",
                    "Storm": "...Storm Warning",
                    "Gale": "...Gale Warning",                  
                    }

            phenomenonDict = {
                    "Tropical Depression": "Tropical Depression",
                    "Post-Tropical": "Post-Tropical Cyclone",
                    "Remnants": "Remnants",
                    }
            
            feature = self.Feature()
            feature.featureType = 'TCM_Based'
            for key in warningDict:
                headline = warningDict.get(key)
                
                if tcmBegin.find(headline) > -1 or tcmBegin.find(headline.upper()) > -1:
                    feature.highestWarning = key 
                    feature.highestWarningTimePeriod = self._convertToTimeRange("00h")
                    break
                
            if not feature.highestWarning:
                for key in phenomenonDict:
                    phen = phenomenonDict.get(key)
                    if tcmBegin.find(phen) > -1 or tcmBegin.find(phen.upper()) > -1:
                        feature.phenomenonType = key
                        break                                                
            feature.earliestTimePeriod = self._convertToTimeRange("00h")
            feature.autoText = tcmBody.strip()            
            self._features.append(feature)
    
    def _tcmTimeOverlaps(self, tcmTimeStr):
        tcmTime = self.convertBaseTime(tcmTimeStr)
        curTime = time.time()
        
        ### 3 is the max number of hours for TCM overlap to be true
        threshold = 6 * 3600
        
        if abs(curTime - tcmTime) < threshold:
            return True
        return False

    def convertBaseTime(self, timeStr):
        # extract time parts from the str
        hour = int(timeStr[0:2])
        minute = int(timeStr[2:4])
        strList = timeStr.split(" ")
        monthStr = strList[3]
        month = self.monthNum(monthStr)
        day = int(strList[4])
        year = int(strList[5])

        # time.mktime returns time in seconds but in local time
        baseTime = time.mktime((year, month, day, hour, minute, 0, 0, 0, 0))

        # Adjustment to UTC
        diffTime = time.mktime(time.gmtime()) - time.mktime(time.localtime())

        # subtract timeZone and round to the nearest hour
        roundedTime = int((baseTime - diffTime) / 3600) * 3600

        return roundedTime

    def monthNum(self, monthStr):
        monthList = ["JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                     "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"]
        try:
            return monthList.index(monthStr) + 1
        except ValueError:
            return 0 

    def _readCurrentTCM(self, argDict):
        pass
        
    def _createDrawableFeatures(self, argDict):
        # Create Features from VGF / XML Drawable files
        
        # Associating any drawables that match and existing Named Feature
        print("***In Create Drawables***")
        remainingDrawables = []
        for drawableElement in self._ingestDrawables():
            
            print("DrawableElement:", drawableElement.printDrawable())
            
            if drawableElement.drawableType not in ['Ridge', 'Ice Edge', 'Gulf Stream']: 
                if self._associateDrawableElementWithFeature(drawableElement):
                    continue
            remainingDrawables.append(drawableElement)
                
        # For the remaining Drawables, group them based on compatibility types and proximity
        groups = []
        
        # group is a list of drawables
        # CJ change
        group = [remainingDrawables[0]]
        #group = [drawables[0]]
         
        remainingDrawables = remainingDrawables[1:]
        i = 0
        while remainingDrawables and i < 100:
            
            group, remainingDrawables, done = self._groupDrawables(group, remainingDrawables)
            if done:
                groups.append(group)
                if len(remainingDrawables) > 0:
                    group = remainingDrawables[0]
                    remainingDrawables = remainingDrawables[1:]
            i = i + 1
            print("i=", i)
        if group:
            groups.append(group)

        # this line replaced commented out code block above
        group = [remainingDrawables]
        
        # Create a Feature from each group 
        for group in groups:
            # Create a Drawable Feature
            feature = self.Feature()
            feature.featureType = 'Drawable'
            
            # Create all the periods as placeholders
            periods = []
            for index in ['00','24','48']:
                period = self.Period()
                period.timePeriod = self._convertToTimeRange(index+'h')
                periods.append(period)
                
            ### uncommenting the line below causes an infinite loop
            #feature.periods = periods
            
            if type(group) is list:
                for drawable in group:
                    print("feature.periods:", feature.periods)
                    for period in feature.periods:
                        if drawable.timePeriod == period.timePeriod:
                            period.drawables.append(drawable)
                            print("appending to period.drawables in list type")
                        else:
                            continue
            else:
                for period in feature.periods:
                    if group.timePeriod == period.timePeriod:
                        period.drawables.append(group)
                        print("appending to period.drawables in non-list type")
                    else:
                        continue
            
            
            for period in periods:
                if period.drawables:
                    feature.periods.append(period)   
            feature.periods.sort(key=self._sortPeriodsByTime)
            if len(feature.periods) > 0:
                feature.earliestTimePeriod = feature.periods[0].timePeriod                             
            self._features.append(feature)  
                              
    def _groupDrawables(self, group, drawables):
        # Try to add each drawable to the group
        done = True   
        newGroup = []
#         for g in group:
#             print "group is:", g
#             newGroup.append(g)
        print("group is:", type(group))
        newGroup = self._copyDrawables(group) 
        returnedDrawables = []
        if type(group) is list:
            for d1 in group:
                for d2 in drawables:
                    if self._compatibleDrawableTypes(d1, d2):
                        if self._proximity(d1, d2):
                            newGroup.append(d2)
                            done = False
                    else:
                        returnedDrawables.append(d2)
            return newGroup, returnedDrawables, done
        else:
            return group, returnedDrawables, True
    
    def _copyDrawables(self, group):
        print("dir:", dir(group))
        if type(group) is list:
            newList = []
            for g in group:
                newList.append(g)
            return newList
        else:  # it's a singleton
            drawable = self.Drawable()
            drawable.timePeriod = group.timePeriod
            drawable.latLons = group.latLons
            drawable.pressureTag = group.pressureTag
            drawable.movement = group.movement
            drawable.drawableType = group.drawableType
            return drawable

        return
    
    def _ingestDrawables(self):
        # Read in the files and use ElementTree to parse them and create Drawables
        drawables = []
        print('IngestDrawables')
        for t in ['24']:
        #for t in ['00','24','48']:
            fileName = '/localapps/dev/HSF/'+t+'.xml'
            #Below is where cron files live (note they get purged at H+45)
            #fileName = '/data/fxa/LOCAL/getvgf/data/'+t+'.xml'
            print("fileName", fileName)
            
            tree = ET.parse(fileName)
            timePeriod = self._convertToTimeRange(t+'h')
            # Get the Lines
            for line in tree.iter("Line"):
                drawable = self.Drawable()
                pgenType = line.attrib.get('pgenType')
                print("pgenType", pgenType)
                
                #pgenExcludeList = ["LINE_SOLID", "LINE_DASHED_6", "FILLED_ARROW", "POINTED_ARROW", "DRY_LINE", "General Text", "Contours", "None"]
                pgenExcludeList = ["LINE_SOLID", "LINE_DASHED_6", "FILLED_ARROW", "POINTED_ARROW", "DRY_LINE", "Contours", "None"]
                if pgenType in pgenExcludeList:
                    print("pgenType skipped:", pgenType)
                    continue
                drawable.drawableType = self._pgenTypeDecodeDict().get(pgenType)
                drawable.timePeriod = timePeriod
                drawable.latLons = self._getLatLons(line)
                drawable.printDrawable()
                drawables.append(drawable)
                
            # Get the collections with Symbols   
            for collection in tree.iter("DECollection"):
                for symbol in collection.iter("Symbol"):                
                    drawable = self.Drawable()
                    pgenType = symbol.attrib.get('pgenType')
                    print("pgenType", pgenType)
                    drawable.drawableType = self._pgenTypeDecodeDict().get(pgenType)
                    drawable.timePeriod = timePeriod
                    drawable.latLons = self._getLatLons(symbol)
                    for textline in collection.iter("textline"):
                        drawable.pressureTag = textline.text + " mb" 
                    print("printing collection drawable")               
                    drawable.printDrawable()
                    drawables.append(drawable)  
        return drawables
    
    def _best_way(self, number):
        if number%2==0:
            return "even"
        else:
            return "odd"
    
    def _getLatLons(self, node):
        latLons = []
        for point in node.findall("Point"):
                       
            lat = self.round(float(point.attrib.get("Lat")), "Nearest", 0.1)
            lat = int((lat + 0.25) * 2.0) / 2.0
            lat = float(lat)
            latmult = lat * 10
            if (self._best_way(latmult)) == "even":
                lat = int(lat)
            
            lon = self.round(float(point.attrib.get("Lon")), "Nearest", 0.1)
            lon = int((lon + 0.25) * 2.0) / 2.0
            lon = float(lon)
            lonmult = lon * 10
            if (self._best_way(lonmult)) == "even":
                lon = int(lon)
#             lat = float(point.attrib.get("Lat"))
#             lon = float(point.attrib.get("Lon"))
            latLons.append((lat, lon))
        return latLons
     
    def _associateDrawableElementWithFeature(self, drawableElement):
        # Determine if the drawableElement can be associated with a feature
        #  If so, determine if is associated 
        found = False
        latLons = drawableElement.latLons               
        for feature in self._features:
            if feature.featureType not in ['Named']:
                continue
            for period in feature.periods:                    
                if self._drawableElementOverlaps(period.areas, latLons):
                    period.drawables.append(drawableElement)
                    print("appending to period.drawables in associate")
                    found = True
        return found
    
    # TO DO -- complete this
    def _compatibleDrawableTypes(self, d1, d2):
        compatibleTypes = [('High', 'Ridge'), ('Trough', 'Low'), ('Tropical Wave', 'Low'), ('Low', 'Cold Front')]
        t1 = d1.drawableType
        t2 = d2.drawableType
        if t1 == t2:
            return True
        if (t1, t2) in compatibleTypes or (t2, t1) in compatibleTypes:
            return True
        else:
            return False
                                          
    def _sampleData(self, argDict):        
        elements = self._analysisList(argDict)
        periods = []
        areaTuples = []
        for feature in self._features:
            if feature.featureType != 'Named':
                continue
            for period in feature.periods:
                periods.append((period.timePeriod, 'timeLabel'))
                for area in period.areas:
                    if area.refData:
                        editArea = area.refData
                    else:
                        editArea = area.areaName
                    areaTuples.append((editArea, area.areaLabel))
                    
        sampleInfo = (elements, periods, areaTuples)
        #print "\nSampleInfo", sampleInfo        
        self._sampler = self.getSampler(argDict, sampleInfo)
        print("Sampler", self._sampler)

#####        
    def getSampler(self, argDict, sampleInfo, sampleFromServer=0):
        # Get a HistoSampler given
        #   sampleInfo, which is a list of tuples, or just a single tuple
        #     of tuples ([elements], [periods], [areas])
        #   the elements are [(name, method)] -- basically the analysis list
        #   the periods [(timeRange, label)]
        #   areas [(name,label)] or [(refData, label)] or [(refID, label)]
        ifpClient = argDict["ifpClient"]
        databaseID = argDict["databaseID"]

        from com.raytheon.viz.gfe.sampler import SamplerRequest, HistoSampler
        from java.util import ArrayList

        # convert input sampleInfo to samplerRequests
        samplerRequests = ArrayList()
        if type(sampleInfo) == tuple:
            sampleInfo = [sampleInfo]
        for si in sampleInfo:
            elements, periods, areas = si
            for e in elements:
                parmID = self.getParmID(e[0], databaseID)
                for p in periods:
                    for editArea, areaName in areas:
                        if type(editArea) is str:
                            samplerRequests.add(SamplerRequest( \
                              parmID, ReferenceID(editArea), p[0].toJavaObj()))
                        elif str(type(editArea)) == "<type 'PyJobject'>":
                            samplerRequests.add(SamplerRequest( \
                              parmID, editArea, p[0].toJavaObj()))

                        else:
                            raise Exception("area specification incorrect")

        # do sampling
        if sampleFromServer:
            sampler = ifpClient.sampleRequest(samplerRequests)
        else:
            sampler = HistoSampler(ifpClient.getJavaClient(), samplerRequests)        
        if sampler.isValid() != 1:
            print("Cannot Sample: Check for invalid Weather Elements, ",\
              "Invalid Areas", str(samplerRequests))
            return None
        #print "sampler ", sampler
        return sampler
        
#####        

    def _populateFeatures(self, argDict):
        
        # Populate Features with product information 
        elements = self._analysisList(argDict)
                         
        for feature in self._features: 
            if feature.featureType != 'Named':
                continue           
            featureAreaList = []
            
            print("Raw feature basin:", feature.basinName)
            
            for period in feature.periods:                                 
                areas = period.areas
                #print "******time period:", timePeriod, "**********"
                for area in areas:                    
                    featureAreaList.append(area)
                    
                    if area.refData:
                        areaData = area.refData
                    else:
                        areaData = area.areaName
                    
                    print("Populate Features using data:", areaData)
                    statDict = self.getStatDict(self._sampler, elements, period.timePeriod, areaData)
                    print("elements:", elements)
                    area.statDict = statDict
                    print("PopulateFeatures....Area....StatDict", statDict)
                    print("Area refdata:", area.areaName)
                    if area.refData:
                        polygons = area.refData.getPolygons
                        print("polygon methods:", dir(area.refData.getPolygons))
                        print("Polygons size:", polygons.__sizeof__())

                    # Look for various warnings for this period and area
                    for warningMethod in self._warningMethods():
                        found, headline, methodList, warningType = warningMethod(statDict)
                        print("warnings into warningMethod:", found, headline)
 
                        if found:
                            area.warningType = warningType
                            area.headline = headline
                            area.methodList = methodList                            
                            break
                        else:
                            if self._checkForWx(statDict):
                                methodList = self._wxMethodList()
                            else:
                                methodList = self._windWaveMethodList()
                            area.methodList = methodList
                    self._setWindWave(area, statDict)
            
            # If there are warnings: Find the highest earliest warning type
            if len(featureAreaList) > 0:
                featureAreaList.sort(key=self._sortAreasForWarningType)
                chosenArea = featureAreaList[0]
                feature.highestWarning = chosenArea.warningType
                feature.highestWarningHeadline = chosenArea.headline
                feature.highestWarningTimePeriod = chosenArea.timePeriod
            # Find earliest timePeriod
            timePeriodList = sorted(feature.periods, key=self._sortTimePeriods)
            feature.earliestTimePeriod = timePeriodList[0].timePeriod
        return
            
    def _orderFeatures(self):
        # Sort at the area level
        for feature in self._features:
            # Grid based features don't have periods so ignore this
            if feature.featureType == "GridBased":
                continue
            for period in feature.periods:
                period.areas.sort(key=self._sortAreas)
    
        self._warningFeatures = []
        self._synopsisFeatures = []
        # Sort at the feature level    
        for feature in self._features:
            if feature.highestWarning: 
                self._warningFeatures.append(feature)
            else:
                self._synopsisFeatures.append(feature)
        self._warningFeatures.sort(key=self._sortWarningFeatures)
        self._synopsisFeatures.sort(key=self._sortSynopsisFeatures)
        
    ##########    
        
    def _preProcessProduct(self, fcst, argDict):
        productName = self._productName.strip()       
        issuedByString = self.getIssuedByString()
        if self._windMax > 63:
           self._issuanceType = "PAN PAN"    
        else:
            self._issuanceType = "SECURITE"            
   
        fcst =  fcst + self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n" + \
               productName + "\n" +\
               "NWS " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n\n"
               
        if self._ccode:
            ### the next line must be changed to check HSFAT1 pil after testing - 11/14/2017 CNJ
            if self._pil == "HSFAT1":
                fcst = fcst + "CCODE/1:31:04:01:00/AOW/NWS/CCODE" + "\n"
            elif self._pil == "HSFEP1":
                fcst = fcst + "CCODE/1:31:12:01:00/AOW+POR/NWS/CCODE" + "\n"
            elif self._pil == "HSFEP3":
                fcst = fcst + "CCODE/1:31:16:01:00/AOW/NWS/CCODE" + "\n"
            else:
                pass
        
        fcst = fcst + self._Text1() + self._issuanceType + "\n\n" + self._Text2()
        fcst = self._validLabel(fcst, argDict)
        return fcst
        

    def _makeSection(self, fcst, argDict, features, sectionType):
        print("feature dump")
        for feature in features:
            print("FeatureType:", feature.featureType)
            print("Feature.phenomenonType:", feature.phenomenonType)
        

        if sectionType == "Warning":
            fcst = fcst + ".WARNINGS.\n\n"
        else:
            fcst = fcst + ".SYNOPSIS AND FORECAST.\n\n"
             
        self._analysisList(argDict)
        
        print("Feature Count:", len(self._features), "Section type:", sectionType)
        
        for feature in features:
            print("^^^FEATURE^^^")
            #feature.printFeature()
            print("^^^end feature^^^")
            fname = feature.name
            print("### FEATURE NAME:", fname)
            if feature.featureType == "TCM_Based":
                fcst = fcst + feature.autoText + "\n\n"
                continue
             
            # Process Volcanic Ash wxType
#             if feature.wxType == 'VA':
#                 fcst = fcst + feature.area.headline
#                 #print "Feature wxType is:", feature.wxType, "and is being skipped."
#                 continue 
            
            if sectionType == "Warning":                
                print("Section type is Warning")
                fcst = fcst + feature.highestWarningHeadline + "\n"
                print("highestWarningHeadline", feature.highestWarningHeadline)
                 
#             elif sectionType == "Synopsis":
            #print "Section type is Synopsis"
            print("Formatting feature type:", feature.featureType)
            if feature.featureType != "GridBased":
                for period in feature.periods:
                    timePeriod = period.timePeriod
                    #print "******time period:", timePeriod, "*******************"
                    if feature.periods.index(period) == 0:
                        basinName = feature.basinName
                    else:
                        basinName = None
                      
                    # next line was reporting blank time label for convection  
                    #fcst = fcst + self._getTimePeriodLabel(argDict, timePeriod, basinName)
                     
                    # next code block to not report blank time label for grid-based feature  
                    if feature.featureType == 'GridBased':
                        pass
                    else:
                        fcst = fcst + self._getTimePeriodLabel(argDict, timePeriod, basinName)
                     
                    print("*** before period drawables ***")
                    # Add in associated drawables
                    if period.drawables:
                        print("INTO PERIOD.DRAWABLES", period.drawables)
                        for drawable in period.drawables:
                            fcst = fcst + self._formatDrawable(drawable) #+ ".\n\n"
                             
                    print("*** after period drawables ***")
     
                    for area in period.areas:
                        areaWords =  self._getAreaWords(area).rstrip()
                        elementWords = self._getElementWords(feature, area, sectionType)                   
                        fcst = fcst + areaWords + " " + elementWords #+ ". "
                        #removed space above between "" because when a feature is dissipated
                        #it was adding an extra space in front of WINDS 20 KT OR LESS. -JL/3/24/16
                             
                    # next two lines may be printing extra carriage returns when there are no features     
                    fcst = fcst + "\n"
            elif feature.featureType == "GridBased":
                print("Formatting grid based features..............................................")
                for area in feature.GBareaList:
                    areaWords =  self._getAreaWords(area).rstrip()
                    elementWords = self._getGridBasedWxWords(area)                   
                                        
                        # next line to include time label for grid based feature
                    if area.wxType != 'VA':
                        fcst = fcst + self._getTimePeriodLabel(argDict, area.timePeriod)
                    #if feature.wxType == 'VA':
                    if area.wxType == 'VA':
                        fcst = fcst + "\n\n"
                        #fcst = fcst + area.headline + "\n\n"
                    else:
                        fcst = fcst + elementWords + " " + areaWords + ".\n"
                        print("**** element words:", elementWords, "****")
                        print("**** area words:", areaWords, "****")
                        #fcst = fcst + elementWords + " " + areaWords + ". "
            else:
                print("Skipping this feature in makeSection.+++++++++++++++++++++++++++++++++++++++++++++++++++")
                print("Feature Dump:", feature.printFeature())

                     
            fcst = fcst + "\n"
            
        if sectionType == "Warning":
            if self._noWarningFeatures():
                print("Found no warning features. ")
                fcst = fcst + ".NONE.\n\n"
        
        return fcst    
    
    
    def _getAreaWords(self, area):
        areaLabel = area.areaLabel
        # Killed feature have no area label so remove a space
        print("+&++++++++++++++++++++++++++++++AREA LABEL:", areaLabel)
        if areaLabel == "":
            return areaLabel
        else:
            return areaLabel + " "

    def _getElementWords(self, feature, area, sectionType):
        # Set up the correct method depending on the weather element or phenomenon
        statDict = area.statDict                    
        if feature.featureType == 'GridBased':
            periodStr = ""
            methodList = self._gridBasedWxMethods(area)
        else:
            periodStr = ". "
            if sectionType == "Warning":
                methodList = area.methodList
            else:
                if self._checkForWx(statDict):
                    methodList = self._wxMethodList()
                else:
                    methodList = self._windWaveMethodList()
        if area.wxType == "F" or area.wxType == "K":
            periodStr = "."
        words = self._makePhrases("", methodList, statDict, area.timePeriod, area.areaLabel, periodStr)     
        return words 
    
    def _formatDrawable(self, drawable):
        print("### DRAWABLE ###")
        drawable.printDrawable()
        print("### END DRAWABLE ###")
        outputStr = drawable.drawableType + ' from '
        length = len(drawable.latLons)
        for index in range(length):
            lat, lon = drawable.latLons[index]
            
            # Format lat/lon output
            if lat >= 0:
                lathemi = "N"
            else:
                lathemi = "S"
            # check dateline later
            if lon > 0:
                lonhemi = "E"
            else:
                lonhemi = "W"
            lat = abs(lat)
            lon = abs(lon)
            
            # removed space between lat and lon for drawables
            if lat < 10:
                outputStr = outputStr + '0' + str(lat) + lathemi + str(lon) + lonhemi
            else:
                outputStr = outputStr + str(lat) + lathemi + str(lon) + lonhemi
            
            if index < length-1:
                outputStr = outputStr + ' to '
        return outputStr
    
    def _noSynopsisFeatures(self):
        
        for f in self._features:
            if f.featureType == "Named":
                return False
        return True
    
    def _noWarningFeatures(self):
        for f in self._features:
            print("Feature.highestWarning:", f.highestWarning)
            if f.highestWarning is not None:
                return False
        return True

    def _postProcessProduct(self, fcst, argDict):
        ## Insert Labe and Forecaster Name at bottom of product
        #forecasterName = self._forecasterName.strip()
        #First line below only needed for HSFEP2#
        if self._noSynopsisFeatures():
            fcst = fcst + ".ENTIRE AREA WINDS 20 KT OR LESS. SEAS LESS THAN 8 FT."
        else:
            fcst = fcst + ".REMAINDER OF AREA WINDS 20 KT OR LESS. SEAS LESS THAN 8 FT."

        self._userInfo = UserInfo.UserInfo()
        forecasterName = self._userInfo._getForecasterName(argDict)
        
        #if fcst.find('HURRICANE') != -1:
        if re.search(r'\.\.\.HURRICANE*', fcst):
            fcst = re.sub(r'SECURITE', r'PAN PAN', fcst)
            if self._ccode:
                if self._pil == "HSFAT2":
                    fcst = re.sub(r'CCODE/1:31:04:01:00/AOW/NWS/CCODE', r'CCODE/2:31:04:11:00/AOW+AOE/NWS/CCODE', fcst)
                elif self._pil == "HSFEP1":
                    fcst = re.sub(r'CCODE/1:31:12:01:00/AOW+POR/NWS/CCODE', r'CCODE/2:31:12:11:00/AOW+POR+AOE/NWS/CCODE', fcst)
                elif self._pil == "HSFEP3":
                    fcst = re.sub(r'CCODE/1:31:16:01:00/AOW/NWS/CCODE', r'CCODE/2:31:16:11:00/AOW+POR+AOE/NWS/CCODE', fcst)
                else:
                    pass

        fcst = fcst + "\n\n" + "$$" + "\n" + ".FORECASTER " + forecasterName + ". NATIONAL HURRICANE CENTER."  
                      
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")

        fcst = self._wrapLines(fcst)

        return fcst
            
    #  SORT METHODS      
    def _warningOrder(self):
        return [
                "Hurricane",
                "Typhoon",
                "Hurricane Force",
                "Tropical Storm",
                "Storm",
                "Gale",
                "Heavy Freezing Spray",
                "Ashfall",
                "Space Weather",
                None,         
                ]
        
    def _basinOrder(self):
        return ["ATLC",
                "ATLC AND CARIBBEAN",
                "ATLC AND CARIBBEAN AND GULF OF MEXICO",
                "ATLC AND GULF OF MEXICO",
                "CARIBBEAN",
                "CARIBBEAN AND GULF OF MEXICO",
                "GULF OF MEXICO",
                None,
                ]

    def _phenomenonOrder(self):
        return [
                "Tropical Depression",
                "Post-Tropical",
                "Remnants",
                "Freezing Spray",
                "Fog",
                "Smoke",
                "Convection",
                None,
               ]
   
    def _phenomenonTypeDict(self):
        return {
                'ZY': 'Freezing Spray',
                'VA': 'Ashfall',
                'T': 'Convection',
                'F' : 'Fog',
                'K' : 'Smoke',
                #default: None,
                } 
        
    def _drawableOrder(self):
        return {
                "ITCZ",
                "Cold Front",
                "Forming Cold Front",
                "Dissipating Cold Front",
                "Warm Front",
                "Forming Warm Front",
                "Dissipating Warm Front",
                "Stationary Front",
                "Forming Stationary Front",
                "Dissipating Stationary Front",
                "Occluded Front",
                "Forming Occluded Front",
                "Dissipating Occluded Front",
                "Trough",
                "Tropical Wave",
                "Low",
                "High",
                "Ridge",
                "Ice Edge",
                "Gulf Stream",
                "Ignore"
                }
        
    def _pgenTypeDecodeDict(self):
        return {
                'COLD_FRONT': 'Cold Front',
                'COLD_FRONT_FORM': 'Forming Cold Front',
                'COLD_FRONT_DISS': 'Dissipating Cold Front',
                'WARM_FRONT': 'Warm Front',
                'WARM_FRONT_FORM': 'Forming Warm Front',
                'WARM_FRONT_DISS': 'Dissipating Warm Front',
                'STATIONARY_FRONT': 'Stationary Front',
                'STATIONARY_FRONT_FORM': 'Forming Stationary Front',
                'STATIONARY_FRONT_DISS': 'Dissipating Stationary Front',
                'OCCLUDED_FRONT': 'Occluded Front',
                'OCCULUDED_FRONT_FORM': 'Forming Occluded Front',
                'OCCLUDED_FRONT_DISS': 'Dissipating Occluded Front',
                'TROF': 'Trough',
                'TROPICAL_TROF': 'Tropical Wave',
                'LINE_DASHED_8': 'Shear Line',
                'FILLED_HIGH_PRESSURE_H': 'High',
                'LOW_X_FILLED': 'Low',
                'ZIGZAG': 'Ridge',
                'ZZZ_LINE': 'ITCZ',
                'DOUBLE_LINE': 'Monsoon Trough',
                'FILLED_CIRCLES': 'Ice Edge',
                'LINE_SOLID': 'Western Edge of the Gulf Stream',
                'LINE_DASHED_2': 'Eastern Edge of the Gulf Stream',
                'LINE_DASHED_6': 'Ignore',
                'FILLED_ARROW': 'Ignore',
                'POINTED_ARROW': 'Ignore',
                'DRY_LINE': 'Ignore',
                'General Text': 'Ignore',
                'Contours': 'Ignore',
                'None': 'Ignore'
                }

    @property
    def _sortAreasForWarningType(self):
        def cmpfunc(a, b):
            # Sorting to find the highest warning type for an entire Feature
            order = self._warningOrder()
            if order.index(a.warningType) < order.index(b.warningType):
                return -1
            elif order.index(a.warningType) > order.index(b.warningType):
                return 1
            if a.timePeriod.startTime() < b.timePeriod.startTime():
                return -1
            elif a.timePeriod.startTime() > b.timePeriod.startTime():
                return 1
            return 0
        return functools.cmp_to_key(cmpfunc)
    
    @property
    def _sortTimePeriods(self):
        def cmpfunc(a, b):
            # Sorting time periods within a Feature from earliest to latest
            if a.timePeriod.startTime() < b.timePeriod.startTime():
                return -1
            elif a.timePeriod.startTime() > b.timePeriod.startTime():
                return 1
            return 0
        return functools.cmp_to_key(cmpfunc)

    @property
    def _sortAreas(self):
        def cmpfunc(a, b):
            # Sorting areas within a period
            # a, b are area objects
            # print "SortAreas", "a:", a.windWave, "b:", b.windWave
            order = self._warningOrder()
            if order.index(a.warningType) < order.index(b.warningType):
                return -1
            elif order.index(a.warningType) > order.index(b.warningType):
                return 1
            if a.windWave:
                return 1
            if b.windWave:
                return -1
            if a.windOnly:
                return 1
            if b.windOnly:
                return -1
            if a.waveOnly:
                return 1
            if b.waveOnly:
                return -1
            if a.areaName < b.areaName:
                return -1
            if a.areaName > b.areaName:
                return 1
            return 0
        return functools.cmp_to_key(cmpfunc)

    @property
    def _sortWarningFeatures(self):
        def cmpfunc(a, b):
            # Sorting Features with Warnings into product order
            # a, b are Feature objects
            order = self._warningOrder()
            if a.featureType == "Named" and b.featureType == "GridBased":
                return -1
            elif b.featureType == "Named" and a.featureType == "GridBased":
                return 1
            if order.index(a.highestWarning) < order.index(b.highestWarning):
                return -1
            elif order.index(a.highestWarning) > order.index(b.highestWarning):
                return 1
            if a.highestWarningTimePeriod.startTime() < b.highestWarningTimePeriod.startTime():
                return -1
            elif a.highestWarningTimePeriod.startTime() > b.highestWarningTimePeriod.startTime():
                return 1
            order = self._basinOrder()
            if order.index(a.basinName) < order.index(b.basinName):
                return -1
            elif order.index(a.basinName) > order.index(b.basinName):
                return 1
            return 0
        return functools.cmp_to_key(cmpfunc)

    @property
    def _sortSynopsisFeatures(self):
        def cmpfunc(a, b):
            # Sorting Features without Warnings into product order
            # a, b are Feature objects
            if a.featureType == "Named" and b.featureType == "GridBased":
                return -1
            elif b.featureType == "Named" and a.featureType == "GridBased":
                return 1
            if a.earliestTimePeriod is None:
                return 1
            elif b.earliestTimePeriod is None:
                return -1
            order = self._phenomenonOrder()
            if order.index(a.phenomenonType) < order.index(b.phenomenonType):
                return -1
            elif order.index(a.phenomenonType) > order.index(b.phenomenonType):
                return 1

            # print "a.basinName:", a.basinName, "b.basinName:", b.basinName
            order = self._basinOrder()
            if order.index(a.basinName) < order.index(b.basinName):
                return -1
            elif order.index(a.basinName) > order.index(b.basinName):
                return 1
            if a.earliestTimePeriod.startTime() > b.earliestTimePeriod.startTime():
                return 1
                # return -1
            elif a.earliestTimePeriod.startTime() < b.earliestTimePeriod.startTime():
                return -1
                # return 1
            return 0
        return functools.cmp_to_key(cmpfunc)

    @property
    def _sortPeriodsByTime(self):
        def cmpfunc(a, b):
            if a.timePeriod.startTime() > b.timePeriod.startTime():
                return -1
            elif a.timePeriod.startTime() < b.timePeriod.startTime():
                return 1
            return 0
        return functools.cmp_to_key(cmpfunc)

    # Methods for populating Features, determining which have Warnings            
    def _warningMethods(self):
        return [
            self._checkHurricane,   
            self._checkHurricaneForce,
            self._checkTyphoon,
            self._checkTropicalStorm,
            self._checkStorm,
            self._checkGale,
            self._checkFreezingSpray,
            #self._checkAshfall,
            self._checkSpaceWx,
            ]

    def _checkHurricane(self, statDict):
        hazards = self.getStats(statDict, "Hazards")
        for hazardType, timeRange in hazards:
            if hazardType == "HU.W":
                return True, "...HURRICANE WARNING...", self._windWaveMethodList(), "Hurricane"
        return False, None, None, None

    def _checkTyphoon(self, statDict):
        hazards = self.getStats(statDict, "Hazards")
        for hazardType, timeRange in hazards:
            if hazardType == "TY.W":
                return True, "...TYPHOON WARNING...", self._windWaveMethodList(), "Typhoon"
        return False, None, None, None

    def _checkHurricaneForce(self, statDict):
        hazards = self.getStats(statDict, "Hazards")
        for hazardType, timeRange in hazards:
            if hazardType == "HF.W":
                return True, "...HURRICANE FORCE WIND WARNING...", self._windWaveMethodList(), "Hurricane Force"
        return False, None, None, None
    
    def _checkTropicalStorm(self, statDict):
        hazards = self.getStats(statDict, "Hazards")
        for hazardType, timeRange in hazards:
            if hazardType == "TR.W":
                return True, "...TROPICAL STORM WARNING...", self._windWaveMethodList(), "Tropical Storm"
        return False, None, None, None    

    def _checkStorm(self, statDict):
        hazards = self.getStats(statDict, "Hazards")
        for hazardType, timeRange in hazards:
            if hazardType == "SR.W":
                return True, "...STORM WARNING...", self._windWaveMethodList(), "Storm"
        return False, None, None, None
    
    def _checkGale(self, statDict):
        hazards = self.getStats(statDict, "Hazards")
        for hazardType, timeRange in hazards:
            if hazardType == "GL.W":
                return True, "...GALE WARNING...", self._windWaveMethodList(), "Gale"
        return False, None, None, None

    def _checkFreezingSpray(self, statDict):
        hazards = self.getStats(statDict, "Hazards")
        for hazardType, timeRange in hazards:
            if hazardType == "UP.W":
                return True, "...HEAVY FREEZING SPRAY WARNING...", self._WxMethodList(), "Heavy Freezing Spray"
        return False, None, None, None

    def _checkSpaceWx(self, statDict):
        return False, None, None, None
    
    def _setWindWave(self, area, statDict):
        minMax, dir = self.getStats(statDict, "Wind", "MinMax")
        windMin, windMax = minMax
        windThreshold = 22.5
        minMag, waveMax = self.getStats(statDict, "WaveHeight", "MinMax")
        waveThreshold = self.nlValue(self.null_nlValue(
            None, None, "WaveHeight", "WaveHeight"), waveMax)
        if windMax >= windThreshold and waveMax > waveThreshold:
            area.windWave = True
        elif windMax >= windThreshold:
            area.windOnly = True
        elif waveMax > waveThreshold:
            area.waveOnly = True
            
        ### Below not working to automatically change Securite to Pan Pan  
        if windMax > self._windMax:
            self._windMax = windMax 

    def _validLabel(self, fcst, argDict):

        curTime = argDict.get("creationTime")
        timeStr24 = time.strftime(" %a %b %e.", time.gmtime(curTime + 24*3600))
        timeStr48 = time.strftime(" %a %b %e.", time.gmtime(curTime + 48*3600))
        
        fcst = fcst + "SYNOPSIS VALID " + self._validTime + \
               self.getCurrentTime(argDict, "  %a %b %e.", shiftToLocal=0) + \
               "\n" + "24 hour forecast valid " + self._validTime + \
               timeStr24 + "\n" + \
               "48 hour forecast valid " + self._validTime + \
               timeStr48 + "\n\n"
               
        return fcst
 
 ### modified to add leading zero for 06 hour forecast ##
    def _getTimePeriodLabel(self, argDict, timeRange, basinName=None):
        now = argDict.get("creationTime")
        # truncate the current time to the last six hour time
        now = int(now / (3600* 6)) * 3600 * 6
        diffTime = timeRange.startTime() - AbsTime.AbsTime(now)
        
        if basinName:
            leading = '.'+ basinName + ' '
        else:
            leading = '.'
 
        if diffTime <= 0:
            return leading
        diffTime = diffTime // 3600
        
        ## added code - CJ 3/9/15
        if diffTime < 10:
            diffTime = str(diffTime)
            diffTime = diffTime.zfill(2)
            label =  leading + diffTime + " hour forecast "
        else:
            label =  leading + repr(diffTime)+ " hour forecast "
            
        return label
    
    #  Methods for translating from Feature Descriptors coming from the Tool
    #  to Feature Objects


    def _convertToTimeRange(self, timePeriod):

        hour = int(timePeriod[0:2])

        baseTime = int(time.time() / (3600 * 6)) * (3600 * 6)
        productTime = baseTime + (hour * 3600)
        
        timeRange = TimeRange.TimeRange(AbsTime.AbsTime(productTime),
                                   AbsTime.AbsTime(productTime + 6 * 3600))
        return timeRange

        
    # Returns the name of the file used to store the edit area information.
    def descriptorFileName(self):

        domain = self._displayName[-3:]

        #TextProduct instance has no attribute '_siteID below for AT2
        return f"/data/local/HighSeas/Formatter/{domain}/HSF_AT2HighSeasDescriptors.pic"

    # Returns the name of the file used to store the edit area information.
    def gridBasedFileName(self):

        sys, nodeName, release, version, machine = os.uname()
        domain = self._displayName[-3:]
        
        #TextProduct instance has no attribute '_siteID below for AT2
        return f"/data/local/HighSeas/Formatter/{domain}/HSF_AT2GridBasedFeatures.pic"


    def _checkForWx(self, statDict):
        wxStats = statDict.get("Wx")
        if wxStats is None:
            return False
        for wxStat in wxStats:
            subkey, rank = wxStat
            vis = self.getVis([subkey])
            if subkey.wxType() == "F"and vis <= 1:
                return True
        return False

    def _makePhrases(self, fcst, methodList, statDict, timePeriod, areaLabel, periodStr=". "):

        phraseWords = ""
        print("makePhrases methodList", methodList)
        
        for methodInfo in methodList:
            wordMethod, elementName, maxMin, elementType = methodInfo

            tree, node = self._makeTreeNode(
                methodInfo, statDict, timePeriod, areaLabel)
            #print "statDict after makeTreeNode", statDict
            self._applyRanges(tree, node, statDict, elementName, elementType)
            #print "statDict after applyRanges", statDict
            for subPhrase in node.get("childList"):
                wordMethod(tree, subPhrase)
            #tree.printNode(node)
            self.fillNulls(tree, node)
            if elementName == "Wind":
                self.embedDescriptor(tree, node)
            if wordMethod == self.visibility_words:
                descriptor = self.phrase_descriptor(tree, node, "Visibility", "Visibility")
                node.set("descriptor", descriptor)
            self.assembleSubPhrases(tree, node)       
            phraseWords = phraseWords + node.get("words") + periodStr

        fcst =  fcst + phraseWords
        return fcst

    def _gridBasedWxMethods(self, area):
        return [(self._gridBasedWxWords(area), "Wx", "List", self.WEATHER())]
    
    def _gridBasedWxWords(self, area):

        wxType = area.wxType
        intensity = area.intensity
        coverage = area.coverage
        if wxType == 'T' and intensity == '+' and coverage == "Iso": 
            words = 'isolated strong convection' 
        elif wxType == 'T' and intensity == '+' and coverage == "Sct": 
            words = 'scattered strong convection'
        elif wxType == 'T' and intensity == '+' and coverage == "Num": 
            words = 'numerous strong convection'
        elif wxType == 'T' and coverage == "Iso":
            words = 'isolated moderate convection'     
        elif wxType == 'T' and coverage == "Sct":
            words = 'scattered moderate convection' 
        elif wxType == 'T' and coverage == "Num":
            words = 'numerous moderate convection' 
        elif wxType == 'F' and intensity == '+':
            words = 'dense fog'            
        elif wxType == 'ZY' and intensity == '+':
            words = 'heavy freezing spray'
        elif wxType == 'ZY' and intensity == 'm':
            words = 'moderate freezing spray'
        elif wxType == 'ZY' and intensity == '-':
            words = 'light freezing spray'
        elif wxType == 'VA':
            words = ''
        elif wxType == 'K':
            words = 'dense smoke'
        else:
            words = 'Wx Type not found'
        return self.setWords(node, words)
    
    def _getGridBasedWxWords(self, area):
        wxType = area.wxType
        intensity = area.intensity
        coverage = area.coverage
        if wxType == 'T' and intensity == '+' and coverage == "Iso": 
            words = 'isolated strong convection' 
        elif wxType == 'T' and intensity == '+' and coverage == "Sct": 
            words = 'scattered strong convection'
        elif wxType == 'T' and intensity == '+' and coverage == "Num": 
            words = 'numerous strong convection'
        elif wxType == 'T' and coverage == "Iso":
            words = 'isolated moderate convection'     
        elif wxType == 'T' and coverage == "Sct":
            words = 'scattered moderate convection' 
        elif wxType == 'T' and coverage == "Num":
            words = 'numerous moderate convection'            
        elif wxType == 'F' and intensity == '+':
            words = 'dense fog'            
        elif wxType == 'ZY' and intensity == '+':
            words = 'heavy freezing spray'
        elif wxType == 'ZY' and intensity == 'm':
            words = 'moderate freezing spray'
        elif wxType == 'ZY' and intensity == '-':
            words = 'light freezing spray'
        elif wxType == 'VA':
            words = ''
        elif wxType == 'K':
            words = 'dense smoke'
        else:
            words = 'Wx Type not found'
        
        return words
        
        
    #####################
    #  Overrides

    def _windWaveMethodList(self):
        return [
                    # WINDS
                    #  wind_phrase
                    (self.vector_words, "Wind", "Max", self.VECTOR()),
                    #self.gust_phrase,
                    # WAVES
                    #(self.waveHeight_words, "WaveHeight", "Max", self.SCALAR()),
                    (self.wave_words, "WaveHeight", "Max", self.SCALAR()),
                    ]

    def _wxMethodList(self):
        return [
                    # WEATHER
                    (self.weather_words, "Wx", "List", self.WEATHER()),
                    (self.visibility_words, "Wx", "List", self.WEATHER()),
                    ]


    def _analysisList(self, argDict):
        return [
            #("Wind", self.vectorModeratedMinMax),
            ("Wind", self.vectorMinMax),
            #("WindGust", self.moderatedMax),
            ("WaveHeight", self.moderatedMinMax),
            ("Wx", self.rankedWx),
            ("Hazards", self.discreteTimeRangesByKey),
            ]
        
  
 

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
            
        # round to the nearest 5
        # Handle special caseS of 22.5 minMag.  
        if maxMag < 22.5:
            return 'null'
        if maxMag >= 22.5 and maxMag < 27.5 and minMag >= 22.5 and minMag < 27.5:
            words = '25 ' + units
        elif minMag >= 20 and minMag < 22.5 and maxMag >= 22.5 and maxMag < 27.5:
            words = '20 to 25 '+ units
        elif minMag >= 20 and minMag < 22.5 and maxMag >= 27.5 and maxMag < 32.5:
            words = '20 to 30 '+units
        else:        
            minMag = int((minMag + 2.5) / 5.0) * 5.0
            maxMag = int((maxMag + 2.5) / 5.0) * 5.0
             
            # Check for SingleValue
            if maxMag == minMag: #or minMag == 0:
                around = self.addSpace(
                    self.phrase_descriptor(tree, node, "around", elementName))
                words =  around + repr(int(maxMag)) + " " + units    
            else:    
                if int(minMag) < threshold:
                    upTo = self.addSpace(
                        self.phrase_descriptor(tree, node, "up to", elementName))
                    words = upTo + repr(int(maxMag)) + " " + units
                else:
                    valueConnector = self.value_connector(tree, node, elementName, elementName)
                    words =  repr(int(minMag)) + valueConnector + repr(int(maxMag)) + " " + units

        # This is an additional hook for customizing the magnitude wording
        words = self.vector_mag_hook(tree, node, minMag, maxMag, units, elementName, words)

        return words

    # OVERRIDE - to get single letter directions.
    def vector_dir(self, dir):
        if type(dir) is not str:
            dir = self.dirToText(dir)
        # Commented this out to get single letter directions - Tom.
##        dir = dir.replace("N", "north")
##        dir = dir.replace("S", "south")
##        dir = dir.replace("E", "east")
##        dir = dir.replace("W", "west")
        return dir
    
    def element_outUnits_dict(self, tree, node):
        dict = TextRules.TextRules.element_outUnits_dict(self, tree, node)
        dict["Visibility"] = "NM"
        dict["Wind"] = "kts"
        dict["WaveHeight"] = "ft"
        return dict

    def units_descriptor_dict(self, tree, node):
        # Dictionary of descriptors for various units
        return {
            "units": {
                "ft": "FT",
                "F":"",
                "C":"degrees",
                "K":"kelvins",
                "%":" percent",
                "in":"inches",
                "kts":"KT",
                "s":"seconds",
                "hrs":"hours",
                "m/s":"meters/second",
                "mph":"mph",
                "m":"meters",
                "m^2/s":"meters^2/second",
                "kt-ft":"knots-feet",
                "mm":"millimeters",
                "degrees": "degrees",
                "percent": "percent",
                },
            "unit": {
                "ft":"FT",
                "F":"",
                "C":"degree",
                "K":"kelvin",
                "%":" percent",
                "in":"inch",
                "kts":"KT",
                "s":"second",
                "hrs":"hour",
                "m/s":"meter/second",
                "mph":"mph",
                "m":"meter",
                "m^2/s":"meter^2/second",
                "kt-ft":"knot-foot",
                "mm":"millimeter",
                "degree": "degree",
                "percent": "percent",
                },
            }        

    #####
    #  NULL value phrases
    def first_null_phrase_dict(self, tree, node):
        # Phrase to use if values THROUGHOUT the period or
        # in the first period are Null (i.e. below threshold OR NoWx)
        # E.g.  LIGHT WINDS.    or    LIGHT WINDS BECOMING N 5 MPH.
        return {
            "Wind": "Winds 20 kt or less", 
            "Wind20ft": "light winds", 
            "TransWind": "light winds", 
            "FreeWind": "light winds", 
            "Swell": "light swells", 
            "Swell2": "",
            "Wx": "",
            "WindGust": "",
            "WaveHeight": "Seas less than 8 ft",
            "WindWaveHgt": "waves 2 ft or less",
            "CWR": "",
            }

    def null_phrase_dict(self, tree, node):
        # Phrase to use if values THROUGHOUT the period or
        # in the first period are Null (i.e. below threshold OR NoWx)
        # E.g.  LIGHT WINDS.    or    LIGHT WINDS BECOMING N 5 MPH.
        return {
            "Wind": "Winds 20 kt or less", 
            "Wind20ft": "light winds", 
            "TransWind": "light winds", 
            "FreeWind": "light winds", 
            "Swell": "light swells", 
            "Swell2": "",
            "Wx": "",
            "WindGust": "",
            "WaveHeight": "Seas less than 8 ft",
            "WindWaveHgt": "waves 2 ft or less",
            "CWR": "",
            }
    
    def null_nlValue_dict(self, tree, node):
        # Threshold below which values are considered "null" and
        # reported using the null_phrase (see above)
        return {
            "otherwise": 0,
            "Wind": 20,  
            #"Wind": 22.5,
            "WaveHeight": 8,  
            #"WindGust": 20,
            "Visibility": 1,
            }
   
    def maximum_range_nlValue_dict(self, tree, node):
        # Maximum range to be reported within a phrase
        #   e.g. 5 to 10 mph
        # Units depend on the product
        dict = TextRules.TextRules.maximum_range_nlValue_dict(self, tree, node)
        #-----------------------------------------------------------------------
        # COMMENT: Override max ranges for certain fields
        # This dict specifications allows for wind speed ranges of up to 20 mph
        # during tropical cyclone situations allowing for far better wind speed
        # phrases.
        #-----------------------------------------------------------------------
        dict["Wind"] = {
            (0, 30): 10,
            (30,50): 15,
            (50, 200):20,
            "default":5,
            }
             
        dict["WaveHeight"] = {
            (8,10):2,
            (10,20):5,
            (20,200):10,
            "default":1,
            }
        return dict
 
    # added to force ranges for sea heights with tropical turned on 9/7/11 CNJ/JL
    def minimum_range_nlValue_dict(self, tree, node):
        # This threshold is the "smallest" min/max difference allowed between values reported.
        # For example, if threshold is set to 5 for "MaxT", and the min value is 45
        # and the max value is 46, the range will be adjusted to at least a 5 degree
        # range e.g. 43-48.  These are the values that are then submitted for phrasing
        # such as:
        dict = TextRules.TextRules.minimum_range_nlValue_dict(self, tree, node)
        #   HIGHS IN THE MID 40S
        dict["Wind"] = {
            (0,30):0,
            (30,50):5,
            (50,200):10,
            "default":5,
            }
        dict["WaveHeight"] = {
            (8,10):1,
            (10,16):2,
            (16,28):4,
            (28,40):6,
            (40,200):10,
            "default":1,
            }
        return dict

    def phrase_descriptor_dict(self, tree, node):
        # Descriptors for phrases
        dict = TextRules.TextRules.phrase_descriptor_dict(self, tree, node)
        dict["Wind"] = "winds"
        dict["WaveHeight"] = "seas"
        dict["Visibility"] = "vsby occasionally"
        dict["seas"] = "seas"
        dict["mixed swell"] = "mixed swell"
        dict["waves"] = "seas"
        dict["up to"] =  "winds to"
        dict["around"] = ""
        return dict

##    def rounding_method_dict(self, tree, node):
##        # Special rounding methods
##        #
##        return {
##            "Wind": self.marineRounding,
##            }
              
    # WxPhrases Overrides
    def pop_wx_lower_threshold(self, tree, node):
        # Always report weather
        return 0

    # MarinePhrases Overrides
    def seasWaveHeight_element(self, tree, node):
        # Weather element to use for reporting seas
        # "COMBINED SEAS 10 TO 15 FEET."
        # IF above wind or swell thresholds
        return "WaveHeight"

    def waveHeight_wind_threshold(self, tree, node):
        # wind value above which waveHeight is reported vs. wind waves
        # Unit is knots
        return 0

    def wave_range(self, avg):
        # Make wave ranges based off the average wave value
        table = ((0, "less than 1 ft"), (1, "1 foot or less"),
                 (1.5, "1 to 2 ft"), (2, "1 to 3 ft"),
                 (3, "2 to 4 ft"), (4, "3 to 5 ft"),
                 (5, "3 to 6 ft"), (6, "4 to 7 ft"),
                 (7, "5 to 8 ft"), (8, "6 to 10 ft"),
                 (10, "8 to 12 ft"), (12, "10 to 14 ft"),
                 (14, "12 to 16 ft"), (18, "14 to 18 ft"),
                 (20, "15 to 20 ft"), (100, "over 20 ft"))
        range = ""
        for max, str in table:
            if avg <= max:
                range = str
                break
        return range
    

    # SampleAnalysis overrides
    def moderated_dict(self, parmHisto, timeRange, componentName):
        # This dictionary defines the low and high limit at which
        # outliers will be removed when calculating moderated stats.
        # By convention the first value listed is the percentage
        # allowed for low values and second the percentage allowed
        # for high values.
        dict = SampleAnalysis.SampleAnalysis.moderated_dict(self, parmHisto, timeRange, componentName)
        dict["Wind"] =  (0, 20)
        dict["WaveHeight"] = (5,5)
        return dict

    def dirList(self):
        dirSpan = 22.5
        base = 11.25
        return[
            ('N', 360-base, 361),
            ('N', 0, base),
            ('N TO NE', base, base+1*dirSpan),
            ('NE', base+1*dirSpan, base+2*dirSpan),
            ('NE TO E', base+2*dirSpan, base+3*dirSpan),
            ('E', base+3*dirSpan, base+4*dirSpan),
            ('E TO SE', base+4*dirSpan, base+5*dirSpan),
            ('SE', base+5*dirSpan, base+6*dirSpan),
            ('SE TO S', base+6*dirSpan, base+7*dirSpan),
            ('S', base+7*dirSpan, base+8*dirSpan),
            ('S TO SW', base+8*dirSpan, base+9*dirSpan),
            ('SW', base+9*dirSpan, base+10*dirSpan),
            ('SW TO W', base+10*dirSpan, base+11*dirSpan),
            ('W', base+11*dirSpan, base+12*dirSpan),
            ('W TO NW', base+12*dirSpan, base+13*dirSpan),
            ('NW', base+13*dirSpan, base+14*dirSpan),
            ('NW TO N', base+14*dirSpan, base+15*dirSpan),
            ]

    # Returns a list of the Hazards allowed for this product in VTEC format.
    # These are sorted in priority order - most important first.
    def allowedHazards(self):

        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        tropicalActions = ["NEW", "EXA", "EXB", "EXT", "UPG", "CAN", "CON", "EXP"]
        marineActions = ["NEW", "EXA", "EXB", "EXT", "CON"]
        return [
            ('HU.W', tropicalActions, 'Tropical'),     # HURRICANE WARNING
            ('TR.W', tropicalActions, 'Tropical'),     # TROPICAL STORM WARNING
            ('HF.W', marineActions, 'Marine'),       # HURRICANE FORCE WIND WARNING
            ('SR.W', marineActions, 'Marine'),       # STORM WARNING
            ('GL.W', marineActions, 'Marine'),       # GALE WARNING
            ('UP.W', allActions, 'IceAccr'),                        # HEAVY FREEZING SPRAY WARNING
            ('MH.Y', allActions, 'Ashfall')                        # VOLCANIC ASHFALL ADVISORY
            ]

    def significant_wx_visibility_subkeys(self, tree, node):
        # Weather values that constitute significant weather to
        # be reported regardless of visibility.
        # If your visibility_wx_threshold is None, you do not need
        # to set up these subkeys since weather will always be
        # reported.
        # Set of tuples of weather key search tuples in the form:
        #  (cov type inten)
        # Wildcards are permitted.
        return [("* *")]

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
        return [("*", "F", "*", "*", "")]


    #######################

    def _makeTreeNode(self, phraseInfo, statDict, timePeriod, areaLabel):
        phraseMethod, elementName, maxMin, elementType = phraseInfo

        # Set up temporary tree, node for this phrase
        subPhrase = Node([], [])
        node = Node([subPhrase], [])


        treeStatDict = {}
        for key in statDict:
            treeStatDict[key] = statDict.get(key)
        wxStats = treeStatDict.get("Wx")
        treeStatDict["Wx"] = [(wxStats, timePeriod)]
        
        statistics = Statistics(treeStatDict)
        tree = Narrative([],[node], statistics, None, self, None)
        tree.set("timeRange", timePeriod)
        tree.set("areaLabel", areaLabel)
        
        elementInfo = self.ElementInfo(elementName, maxMin, elementType)
        elementInfo.outUnits = self.element_outUnits(tree, node, elementName, elementName)

        subPhrase.set("statDict", statDict)

        node.set("descriptor", self.phrase_descriptor(tree, node, elementName, elementName))
        node.set("firstElement", elementInfo)
        node.set("elementInfo", elementInfo)
        node.set("elementInfoList", [elementInfo])
        node.set("elementName", elementName)
        node.set("setUpMethod", None)
        node.set("doneList", [None])
        return tree, node

    def _applyRanges(self, tree, node, statDict, elementName, elementType):
        if elementType == self.VECTOR():
            speed, dir = self.getStats(statDict, elementName)
            min, max = speed
            #print "IN _applyRanges min, max", min, max, elementName
            min, max = self.applyRanges(tree, node, min, max, elementName)
            statDict[elementName] = (min, max), dir
        elif elementType == self.SCALAR():
            min, max = self.getStats(statDict, elementName)
            #print "min, max", min, max, elementName
            min, max = self.applyRanges(tree, node, min, max, elementName)
            statDict[elementName] = (min, max)
        elif elementType == self.WEATHER():
            return

    # Overriding from TextUtils
    def getLimits(self, element):
        parmID = self.getParmID(element, self._databaseID)
        gridParmInfo = self._ifpClient.getGridParmInfo(parmID)
        return gridParmInfo.getMinValue(), gridParmInfo.getMaxValue()
    
##############################################################################################
#######################  Edit Area support methods  ##########################################
##############################################################################################

    def _getEditArea(self, editAreaName):
        # Returns an AFPS.ReferenceData object given an edit area name
        # as defined in the GFE
         
        eaMask = self._eaUtils.fetchEditArea(editAreaName)
        # Convert to standard Edit Area type
        editArea = self.decodeEditArea(eaMask)
     
        return editArea
     
    def _editAreaToMask(self, editArea):
        
        grid = editArea.getGrid().getNDArray().astype(numpy.bool8)
        
        return grid
#         return editArea.getGrid().__numpy__[0].astype(numpy.bool8)
 
     
    def _maskToEditArea(self, mask):
        # Returns a refData object for the given mask
        from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DBit
         
        nx = mask.shape[1]
        ny = mask.shape[0]
        bytes = mask.astype('int8')
        grid = Grid2DBit.createBitGrid(nx, ny, bytes)
         
        return ReferenceData(self._gridLoc, ReferenceID("test"), grid)
    
    def _setActiveEditArea(self, area):
        
        self._dataMgr.getRefManager().setActiveRefSet(area)
        return
    
    def _saveEditArea(self, editAreaName, refData):
        # Saves the AFPS.ReferenceData object with the given name
        refID = ReferenceID(editAreaName)
        refData = ReferenceData(refData.getGloc(), refID, refData.getGrid())
        self._dataMgr.getRefManager().saveRefSet(refData)

 
    def _processAreaOverlaps(self, areas):
        # Taking care of "donuts"
        gridSize = (self._gridLoc.gridSize().y, self._gridLoc.gridSize().x)
         
        sumMask = numpy.zeros(gridSize, bool)
        for area in areas:
            mask = self._eaUtils.fetchEditArea(area.areaName)
            ea = self._maskToEditArea(mask)
            
            overlap = mask & sumMask
            if sum(sum(overlap)):
                newMask = numpy.bitwise_xor(mask, overlap)
                newRefArea = self._maskToEditArea(newMask)
            else:
                newRefArea = ea
            
            self._saveEditArea(area.areaName+"Modified", newRefArea)
            area.areaName = area.areaName+"Modified"
            print("processOverlap...RefData", area.areaName, newRefArea)
            sumMask = sumMask | mask
             
        return
    
    def _drawableElementOverlaps(self, areas, latLons):
        for area in areas:
            #ea = self._getEditArea(area.areaName)
            eaMask = self._eaUtils.fetchEditArea(area.areaName)
            ea = self._maskToEditArea(eaMask)
            
            polygons = ea.getPolygons(ReferenceData.CoordinateType.LATLON)
            coords = polygons.getCoordinates()                
            for c in coords:
                for lat, lon in latLons:
                    if self._close((c.x, c.y), (lat, lon)):
                        return True
        return False
      
    def _close(self, ll1, ll2):
        (lat1, lon1) = ll1
        (lat2, lon2) = ll2
        distanceThreshold = 300  # km
        distance = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon2-lon1)) * 6371
        if distance < distanceThreshold:
            return True
        else:
            return False

    def _proximity(self, drawable1, drawable2):
        for lat1, lon1 in drawable1.latLons:
            for lat2, lon2 in drawable2.latLons:
                if self._close((lat1, lon1), (lat2, lon2)):
                    return True
        return False
                        
                    
        
     
    
##############################################################################################
#######################  END Edit Area support methods  ##########################################
##############################################################################################
