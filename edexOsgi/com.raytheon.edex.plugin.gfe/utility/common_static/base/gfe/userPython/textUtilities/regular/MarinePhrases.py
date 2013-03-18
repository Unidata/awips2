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
# MarinePhrases.py
# Methods for producing text forecast from SampleAnalysis statistics.
#
# Author: hansen
# ----------------------------------------------------------------------------

import ScalarPhrases
import VectorRelatedPhrases
import WxPhrases
import DiscretePhrases
import re, string

class MarinePhrases(ScalarPhrases.ScalarPhrases, VectorRelatedPhrases.VectorRelatedPhrases,
                  WxPhrases.WxPhrases, DiscretePhrases.DiscretePhrases):
    def __init__(self):    
        ScalarPhrases.ScalarPhrases.__init__(self)
        VectorRelatedPhrases.VectorRelatedPhrases.__init__(self)
        WxPhrases.WxPhrases.__init__(self)
        DiscretePhrases.DiscretePhrases.__init__(self)

    ############################################      
    ### MARINE PHRASES

    def marine_wind_flag(self, tree, node):
        # If 1, Wind wording will reflect the
        # crossing of significant thresholds such as gales.
        # E.g. "West gales to 35 knots." instead of "West winds 35 knots."
        return 0

    def marine_wind_combining_flag(self, tree, node):
        # If 1, Wind combining will reflect the
        # crossing of significant thresholds such as gales.
        # E.g. "HURRICANE FORCE WINDS TO 100 KNOTS." instead of
        # "NORTH HURRICANE FORCE WINDS TO 100 KNOTS EASING TO
        #  HURRICANE FORCE WINDS TO 80 KNOTS IN THE AFTERNOON."
        return 0

    def marine_wind_verbose_flag(self, tree, node):
        # Applies only if marine_wind_flag is 1 and the
        #  and marine_wind_combining_flag is 0.
        # If 1, Wind phrasing will repeat special descriptors to produce:
        # "NORTH HURRICANE FORCE WINDS TO 100 KNOTS EASING TO
        #  HURRICANCE FORCE WINDS TO 80 KNOTS IN THE AFTERNOON."
        # If 0, will produce:
        # "NORTH HURRICANE FORCE WINDS TO 100 KNOTS EASING TO
        #  80 KNOTS IN THE AFTERNOON."
        # 
        return 1

    def marine_wind_phrase(self):
        return {
            "setUpMethod": self.marine_wind_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),  
            }
    
    def marine_wind_withGusts_phrase(self):
        return {
            "setUpMethod": self.marine_wind_withGusts_setUp,
            "wordMethod": self.vector_words,
            "phraseMethods": self.standard_vector_phraseMethods(),  
            }    
    
    def marine_wind_setUp(self, tree, node):
        return self.wind_setUp(tree, node, gustFlag=0,
                               connectorMethod=self.marine_vectorConnector)

    def marine_wind_withGusts_setUp(self, tree, node):
        return self.wind_setUp(tree, node, gustFlag=1,
                               connectorMethod=self.marine_vectorConnector)
    
    def noWaveHeight_phrase(self, tree, node, elementName1, elementName2=""):
        if elementName1 != elementName2:
            elementNames = elementName1 + " or " + elementName2
        else:
            elementNames = elementName1
        return "|* Insufficient grids for " + elementNames + " during " + \
               str(node.getTimeRange())+ "*|"

    def waveHeight_wind_threshold(self, tree, node):
        # Wind value above which waveHeight (combined seas)
        # is reported vs. wind waves.
        # Also, the Swell phrase is omitted if this threshold is exceeded.
        # Unit is knots
        return 34

    def inlandWatersAreas(self, tree, node):
        # List of edit area names that are inland or bay waters
        #  as opposed to "seas"
        # The phrasing for these areas will be treated differently
        #  (see the wave_phrase)
        #
        # e.g.
        # return ["TampaBayWaters"]
        return []
    
    def inlandWatersWave_element(self, tree, node):
        # Weather element first and second choice to use for reporting inland waters waves
        # If there is incomplete or no data for the first element, the second will be used.
        return ("WindWaveHgt", "WaveHeight")

    def combinedSeas_threshold(self, tree, node):
        # See wave_phrase
        # If waves and swells are above this threshold,
        # combined seas will be reported AND no Swell phrase will be reported.
        # Units: feet
        return 7

    def seasWaveHeight_element(self, tree, node):
        # Weather element to use for reporting seas waves
        # IF above wind or swell thresholds
        return "WaveHeight"

    def seasWindWave_element(self, tree, node):
        # Weather element to use for reporting seas waves
        # IF above wind or swell thresholds
        return "WindWaveHgt"    
                    
    ### WaveHeight and WindWaveHgt
    def wave_withPeriods_phrase(self):
        return {
            "setUpMethod": self.wave_withPeriods_setUp,
            "wordMethod": self.wave_words,
            "phraseMethods": self.standard_phraseMethods()  
            }    
    def wave_phrase(self):
        return {
            "setUpMethod": self.wave_setUp,
            "wordMethod": self.wave_words,
            "phraseMethods": self.standard_phraseMethods()  
            }
    
    def wave_withPeriods_setUp(self, tree, node):
        return self.wave_setUp(tree, node, periodFlag=1)

    def wave_setUp(self, tree, node, periodFlag=0):
        areaLabel = node.getAreaLabel()
        timeRange = node.getTimeRange()
        
        inlandWaters = self.inlandWatersAreas(tree, node)
        if self.currentAreaContains(tree, inlandWaters) == 1:
            elementName, elementName2 = self.inlandWatersWave_element(tree, node)
            statsByRange = tree.stats.get(elementName, timeRange, areaLabel, mergeMethod="List")
            if statsByRange is None:
                elementName = elementName2
            # Do not report Period for inland waters
            periodFlag = 0
            descriptor = self.phrase_descriptor(tree, node, "inland waters", elementName)
            node.set("descriptor", descriptor)
        elif self.seasFlag(tree, node):
            # Use wave height elementName (default)
            elementName = self.seasWaveHeight_element(tree, node)            
            descriptor = self.phrase_descriptor(tree, node, "seas", elementName)
            node.set("descriptor", descriptor)
        else:
            # Use wind waves (default)
            elementName = self.seasWindWave_element(tree, node)
            periodFlag = 0
            descriptor = self.phrase_descriptor(tree, node, "waves", elementName)
            node.set("descriptor", descriptor)
            
        wave = self.ElementInfo(elementName, "List")
        elementInfoList = [wave]         
        if periodFlag:
            node.set("periodFlag", 1)
            period = self.ElementInfo("Period", "Average", primary=0)
            elementInfoList.append(period)
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        return self.DONE()

    def seasFlag(self, tree, node):
       # Return 1 if we are to report combined seas
       timeRange = node.getTimeRange()
       areaLabel = node.getAreaLabel()
       winds = tree.stats.get("Wind", timeRange, areaLabel, mergeMethod="Max")
       if winds is None:
           return 0
       maxWind, dir = winds

       # Determine if we will report combined seas OR wind waves
       seasFlag = 0
       if maxWind > self.waveHeight_wind_threshold(tree, node):
           seasFlag = 1
       else:
           swell = tree.stats.get("Swell", timeRange, areaLabel, mergeMethod="Max")
           swell2 = tree.stats.get("Swell2", timeRange, areaLabel, mergeMethod="Max")
           maxWave = tree.stats.get("WindWaveHgt", timeRange, areaLabel, mergeMethod="Max")
           if swell is None or maxWave is None:
               pass # Leave seasFlag at zero 
           else:
               # We'll decide to report combined seas by looking at
               # the MAX of waves and swells over the entire time period
               swells, dir = swell
               if swell2 is None:
                   swells2 = 0
               else:
                   swells2, dir = swell2
               threshold = self.combinedSeas_threshold(tree, node)
               if maxWave > threshold and \
                  (swells > threshold or swells2 > threshold):
                   seasFlag = 1
       return seasFlag

    def wave_words(self, tree, node):
        # Return a phrase for wave and optionally Period for the given subPhrase
        elementInfo = node.getAncestor("firstElement")
        elementName = elementInfo.name
        statDict = node.getStatDict()
        if statDict is None:
            return self.setWords(node,"")
        wave = self.getStats(statDict, elementName)
        if wave is None:
            return self.setWords(node, "")
        min, max = self.getValue(wave, "MinMax")
        threshold = self.nlValue(self.null_nlValue(
            tree, node, elementName, elementName), max)
        if int(min) < threshold and int(max) < threshold:
            return self.setWords(node, "null")
        waveStr = self.getScalarRangeStr(tree, node, elementName, min, max)
        units = self.units_descriptor(tree, node, "units", "ft")
        waveUnit = self.units_descriptor(tree, node, "unit", "ft")
        if int(min) == 1 and int(max) == 1:
            units = waveUnit
        words = waveStr + " " + units
        if "Period" in statDict.keys():
            period = self.getStats(statDict, "Period")
            if period is not None:
                avg = self.getValue(period, "Average")
                periodUnits = self.units_descriptor(tree, node, "units", "s")
                periodUnit = self.units_descriptor(tree, node, "unit", "s")
                avg = int(avg)
                if avg == 1:
                    periodUnits = periodUnit
                periodDescriptor = self.phrase_descriptor(
                    tree, node, "dominant period", elementName)
                words = words + " " + periodDescriptor + " " + `avg` + " " + periodUnits
        return self.setWords(node, words)

    def waveHeight_phrase(self):
        return {
            "setUpMethod": self.waveHeight_setUp,
            "wordMethod": self.waveHeight_words,
            "phraseMethods": self.standard_phraseMethods()  
            }    
    def waveHeight_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("WaveHeight", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)  
        return self.DONE()

    def waveHeight_words(self, tree, node):
        "Create phrase for waves"
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "WaveHeight")
        if stats is None:
            nodataPhrase = self.noWaveHeight_phrase(
                tree, node, "WaveHeight", "WaveHeight")
            return self.setWords(node.parent, nodataPhrase)

        min, max = self.getValue(stats, "MinMax")
        avg = (min + max)/2
        words = self.wave_range(avg)            
        return self.setWords(node, words)
            
    def wave_range(self, avg):
        # Make wave ranges based off the average wave value
        table = ((0, "less than 1 foot"), (1, "1 foot or less"),
                 (1.5, "1 to 2 feet"), (2, "1 to 3 feet"),
                 (3, "2 to 4 feet"), (4, "3 to 5 feet"),
                 (5, "3 to 6 feet"), (6, "4 to 7 feet"),
                 (7, "5 to 8 feet"), (8, "6 to 10 feet"),
                 (10, "8 to 12 feet"), (12, "10 to 14 feet"),
                 (14, "12 to 16 feet"), (18, "14 to 18 feet"),
                 (20, "15 to 20 feet"), (100, "over 20 feet"))
        range = ""
        for max, str in table:
            if avg <= max:
                range = str
                break
        return range

    ### Chop
    def chop_phrase(self):
        return {
            "setUpMethod": self.chop_setUp,
            "wordMethod": self.chop_words,
            "phraseMethods": self.standard_phraseMethods()  
            }    
    def chop_setUp(self, tree, node):
        # Only generate this phrase for inland waters areas
        inlandWaters = self.inlandWatersAreas(tree, node)
        if self.currentAreaContains(tree, inlandWaters) == 0:
            return self.setWords(node, "")

        # Set up for only one subPhrase.
        chop = self.ElementInfo("Wind", "Max", self.VECTOR())
        
        # Uncomment the following line if you want the chop_phrase to
        # have subPhrases e.g. "A light chop in morning."
        #chop = self.ElementInfo("Wind", "List", self.VECTOR())
        elementInfoList = [chop]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)
        descriptor = self.phrase_descriptor(tree, node, "chop", "chop")
        node.set("descriptor", descriptor)
        return self.DONE()
    
    def chop_words(self, tree, node):
        "Create phrase for chop"
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "Wind")
        if stats is None:
            return self.setWords(node, "")
        maxWind, dir = self.getValue(stats, "Max", self.VECTOR())         
        if maxWind <= 7: 
            value = "smooth"
        elif maxWind > 7 and maxWind <= 12: 
            value = "a light chop"       
        elif maxWind > 12 and maxWind <= 17: 
            value = "a moderate chop" 
        elif maxWind > 17 and maxWind <= 22: 
           value = "choppy" 
        elif maxWind > 22 and maxWind <= 27: 
            value = "rough" 
        elif maxWind > 27 and maxWind <= 32: 
            value = "very rough"        
        elif maxWind > 32: 
            value = "extremely rough" 
        else: 
            value = "!!!Chop phrase problem!!!"
        return self.setWords(node, value)

    ### Swell
    def swell_phrase(self):
        return {
            "setUpMethod": self.swell_setUp,
            "wordMethod": self.swell_words,
            "phraseMethods": self.standard_vector_phraseMethods(),  
            }
    
    def swell_withPeriods_phrase(self):
        return {
            "setUpMethod": self.swell_withPeriods_setUp,
            "wordMethod": self.swell_words,
            "phraseMethods": self.standard_vector_phraseMethods(),  
            }    
    
    def swell_withPeriods_setUp(self, tree, node):
        return self.swell_setUp(tree, node, periodFlag=1)

    def swell_setUp(self, tree, node, periodFlag=0):
        # Do not report swells for inland waters
        inlandWaters = self.inlandWatersAreas(tree, node)
        if self.currentAreaContains(tree, inlandWaters) == 1:
            return self.setWords(node, "")
        
        # Do not report swells if we are reporting combined seas
        if self.seasFlag(tree, node) == 1:
            return self.setWords(node, "")
        
        swell = self.ElementInfo("Swell", "List", self.VECTOR())
        elementInfoList = [swell]
        if periodFlag:
            swellPhrase = self.swell_withPeriods_phrase
        else:
            swellPhrase = self.swell_phrase            
        swell2 = self.ElementInfo(
            "Swell2", "MinMax", self.VECTOR(), phraseDef=swellPhrase) #, primary=0)
        elementInfoList = [swell, swell2]
        if periodFlag:
            node.set("periodFlag", 1)
            period = self.ElementInfo("Period", "MinMax", primary=0)
            period2 = self.ElementInfo("Period2", "MinMax", primary=0)
            elementInfoList.append(period)
            elementInfoList.append(period2)
        self.subPhraseSetUp(tree, node, elementInfoList, self.vectorConnector)
        return self.DONE()
            
    def swell_words(self, tree, node):
        # Create phrase for swell for a given set of stats in statsByRange
        #print "\n in swell words"
        periodFlag = node.getAncestor("periodFlag")
        statDict = node.getStatDict()
        #Check for Swell alone
        swell2 = self.getStats(statDict, "Swell2")
        if swell2 is None:
            oneSwell = 1
        else:
            oneSwell = 0

        # Swell and Swell2 subPhrases
        subPhraseParts = []
        elementInfoList = node.getAncestor("elementInfoList")
        for swell, period in [("Swell", "Period"), ("Swell2", "Period2")]:
            if swell == "Swell":
                checkRepeating = 1
            else:
                checkRepeating = 0
            for elementInfo in elementInfoList:
                if elementInfo.name == swell:
                    swellInfo = elementInfo
                    break
            swellWords = self.simple_vector_phrase(tree, node, swellInfo, checkRepeating)
            if swellWords == "null" or swellWords == "":
                subPhraseParts.append("")
                continue
            # Add Period
            periodPhrase = ""
            if periodFlag == 1:
                periodStats = self.getStats(statDict, period)
                periodPhrase = self.embedded_period_phrase(tree, node, periodStats)
                swellWords = swellWords + periodPhrase
            subPhraseParts.append(swellWords)

        #print "swell", node.getTimeRange(), subPhraseParts 
        if subPhraseParts[0] != "" and subPhraseParts[1] != "":
            words =  subPhraseParts[0] + " and " + subPhraseParts[1]
            # Check for mixed swell on first subPhrase
            if node.getIndex() == 0:
                mixedSwell = self.checkMixedSwell(tree, node, statDict)
                if mixedSwell:
                    mixedSwellDesc = self.phrase_descriptor(tree, node, "mixed swell", "Swell")
                    phrase = node.getParent()
                    phrase.set("descriptor", mixedSwellDesc)
                    phrase.doneList.append(self.embedDescriptor)
        elif subPhraseParts[0] != "":
           words = subPhraseParts[0]
        elif subPhraseParts[1] != "":
           words = subPhraseParts[1]
        else:
           words = "null"

        return self.setWords(node, words)

    def checkMixedSwell(self, tree, node, statDict):
        # Check for mixed swell wording
        # Return mixed swell phrase if appropriate
        # Otherwise, return None
        swell = self.getStats(statDict, "Swell")
        swell2 = self.getStats(statDict, "Swell2")
        if swell is None or swell2 is None:
            return 0
        swellMag, swellDir = swell
        swell2Mag, swell2Dir = swell2
        swellMag = self.getValue(swellMag)
        swell2Mag = self.getValue(swell2Mag)
        if self.direction_difference(swellDir, swell2Dir) >= 90.0 and \
               swellMag > 0 and \
               swell2Mag / swellMag > 0.50:
            return 1
        else:
            return 0

    ### Period
    def embedded_period_phrase(self, tree, node, periodStats):
        # Create a period phrase to be embedded with a Swell phrase
        if periodStats is None:
            return ""
        period = int(self.getValue(periodStats))

        outUnits = self.element_outUnits(tree, node, "Period", "Period")
        units = self.units_descriptor(tree, node, "units", outUnits)
        unit = self.units_descriptor(tree, node, "unit", outUnits)
        if period == 1:
            units = unit
        return " at " + `period` + " " + units

    def period_phrase(self):
        return {
            "setUpMethod": self.period_setUp,
            "wordMethod": self.period_words,
            "phraseMethods": self.standard_phraseMethods()  
            }    
    def period_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("Period", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector)    
        return self.DONE()
    
    def period_words(self, tree, node):
        # Return a phrase for Period for the given index in statsByRange
        statDict = node.getStatDict()
        stats = self.getStats(statDict, "Period")
        if stats is None:
            return self.setWords(node, "")
        periodValue = int(self.getValue(stats))

        outUnits = self.element_outUnits(tree, node, "Period", "Period")
        units = self.units_descriptor(tree, node, "units", outUnits)
        unit = self.units_descriptor(tree, node, "unit", outUnits)
        if periodValue == 1:
            units = unit
        return self.setWords(node, `periodValue` + " " + units)
    
    def marine_abbreviateText(self, fcst):
        #convert to upper case and add a space at the beginning to create
        #a word boundary on the first word (space is removed at end of method).
        fcst = " " + string.upper(fcst)
        fcst = re.sub(r'\n', r' ',fcst)
        fcst = re.sub(r'(\W)NORTH(\W)', r'\1N\2',fcst) 
        fcst = re.sub(r'(\W)SOUTH(\W)', r'\1S\2',fcst) 
        fcst = re.sub(r'(\W)EAST(\W)', r'\1E\2',fcst) 
        fcst = re.sub(r'(\W)WEST(\W)', r'\1W\2',fcst)
        fcst = re.sub(r'(\W)NORTHEAST(\W)', r'\1NE\2',fcst) 
        fcst = re.sub(r'(\W)SOUTHEAST(\W)', r'\1SE\2',fcst) 
        fcst = re.sub(r'(\W)SOUTHWEST(\W)', r'\1SW\2',fcst) 
        fcst = re.sub(r'(\W)NORTHWEST(\W)', r'\1NW\2',fcst)
        fcst = re.sub(r'(\W)KNOTS?(\W)', r'\1KT\2',fcst) 
##        fcst = re.sub(r'(\W)FOOT(\W)', r'\1FT\2',fcst) 
        fcst = re.sub(r'(\W)FEET(\W)', r'\1FT\2',fcst)
        fcst = re.sub(r'(\W)POSITION(\W)', r'\1PSN\2',fcst) 
        fcst = re.sub(r'(\W)VISIBILITY(\W)', r'\1VSBY\2',fcst) 
        fcst = re.sub(r'(\W)THUNDERSTORM', r'\1TSTM',fcst) 
        fcst = re.sub(r'(\W)AVERAGE(\W)', r'\1AVG\2',fcst) 
        fcst = re.sub(r'(\W)NAUTICAL MILES?(\W)', r'\1NM\2',fcst) 
        fcst = re.sub(r'(\W)ATLANTIC(\W)', r'\1ATLC\2',fcst) 
        fcst = re.sub(r'(\W)FATHOMS?(\W)', r'\1FM\2',fcst) 
        fcst = re.sub(r'(\W)LONGITUDE(\W)', r'\1LONG\2',fcst) 
        fcst = re.sub(r'(\W)PACIFIC(\W)', r'\1PAC\2',fcst) 
        fcst = re.sub(r'(\W)DEGREES?(\W)', r'\1DEG\2',fcst) 
        fcst = re.sub(r'(\W)MILLIBARS?(\W)', r'\1MB\2',fcst) 
        fcst = re.sub(r'(\W)PRESSURE(\W)', r'\1PRES\2',fcst)
        fcst = re.sub(r'(\W)SUNDAY(\W)', r'\1SUN\2',fcst)
        fcst = re.sub(r'(\W)MONDAY(\W)', r'\1MON\2',fcst)
        fcst = re.sub(r'(\W)TUESDAY(\W)', r'\1TUE\2',fcst)
        fcst = re.sub(r'(\W)WEDNESDAY(\W)', r'\1WED\2',fcst)
        fcst = re.sub(r'(\W)THURSDAY(\W)', r'\1THU\2',fcst)
        fcst = re.sub(r'(\W)FRIDAY(\W)', r'\1FRI\2',fcst)
        fcst = re.sub(r'(\W)SATURDAY(\W)', r'\1SAT\2',fcst)
        fcst = re.sub(r'^ ', r'',fcst)
        return fcst
 
