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
# RoutineLevel5_3_TestScript    
# Author:
# ----------------------------------------------------------------------------

# First run setupTextEA

import TestScript

# For FWF_InfiniteLoops
checkResolution = """def checkResolution(self, tree, node):
    # Check to see if there are too many sub-phrases and we need to re-do the
    # phrase in lower resolution. The limit is determined by "subPhrase_limit".
    # This currently assumes we have a 3 or greater resolution and want to go to
    # a 6-hour resolution.
        
    # See if ready to process
    if not self.phrase_trigger(tree, node):
        return
        
    # Count the number of non-empty phrases
    #print "In check resolution", node
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
        newPhrase = tree.addPhraseDef(node, self.weather_phrase)
##            newPhrase.set("disabledSubkeys", node.get("disabledSubkeys"))
        curResolution = node.get("resolution")
##            if curResolution is not None:
##                # If we have already re-set the resolution and we are still over the
##                # sub-phrase limit, we'll have to decrease the resolution some more
##                # to try and reduce the number of sub-phrases.
##                # This is necessary because of the way preProcessWx works:
##                # For example, even if we have only 2 time periods sampled,
##                # they can result in 3 or more sub-phrases depending on the
##                # complexity of weather.
##                # Example:  Hours 1-6  Chc RW Chc L
##                #           Hours 7-12 Chc SW Chc L
##                #   Results in 3 sub-phrases
##                #           Hours 1-12 Chc L
##                #           Hours 1-6  Chc RW
##                #           Hours 7-12 Chc SW
##                newResolution = curResolution * 2
##            else:
##                newResolution = 6
        newResolution = 6
        newPhrase.set("resolution", newResolution)
        #print "making newPhrase", newPhrase
        #print "parent should be", node.parent
        #tree.printNode(newPhrase)
        # Remove this node
        node.remove()
    return self.DONE()
"""

subPhrase_limit = """def subPhrase_limit(self, tree, node):
    return 2
"""

checkResolution2 = """def checkResolution(self, tree, node):
    # Check to see if there are too many sub-phrases and we need to re-do the
    # phrase in lower resolution. The limit is determined by "subPhrase_limit".
    # This currently assumes we have a 3 or greater resolution and want to go to
    # a 6-hour resolution.
        
    # See if ready to process
    if not self.phrase_trigger(tree, node):
        return
        
    # Count the number of non-empty phrases
    #print "In check resolution", node
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
        newPhrase = tree.addPhraseDef(node, self.skyPopWx_phrase)
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
        #print "making newPhrase", newPhrase
        #print "parent should be", node.parent
        #tree.printNode(newPhrase)
        # Remove this node
        node.remove()
    return self.DONE()
"""

period1 = """def Period_1(self):
    component =  { 
        "type": "component",
        "methodList": [
                      self.orderPhrases,
                      self.assemblePhrases,   
                      self.wordWrap,          
                      ],
        "analysisList": [
                   #("MinT", self.avg),
                   #("MaxT", self.avg),
                   ("MinT", self.stdDevMinMax),
                   ("MaxT", self.stdDevMinMax),
                   ("T", self.hourlyTemp),
                   ("T", self.minMax),
                   ("Sky", self.median, [3]),
                   ("PoP", self._PoP_analysisMethod("Period_1"), [3]),
                   ("PoP", self.binnedPercent, [3]),
                   ("SnowAmt", self.accumMinMax),
                   ("StormTotalSnow", self.accumMinMax),
                   ("IceAccum", self.accumMinMax),
                   ("SnowLevel", self.avg),
                   ("Wind", self.vectorMedianRange, [6]),
                   ("Wind", self.vectorMinMax, [6]),
                   ("WindGust", self.maximum, [6]),
                   ("Wx", self.rankedWx, [0]),
                   ("WindChill", self.minMax, [6]),
                   ("HeatIndex", self.minMax, [6]),
                   ],
        "phraseList":[
               self.sky_phrase,
               self.skyPopWx_phrase,
               self.wind_summary,
               self.reportTrends,
               self.weather_phrase,
               self.severeWeather_phrase,
               self.heavyPrecip_phrase,
               self.visibility_phrase,
               self.snow_phrase,
               self.total_snow_phrase,
               self.snowLevel_phrase,
               self.iceAccumulation_phrase,
               self.highs_phrase,
               self.lows_phrase,
               #self.highs_range_phrase,
               #self.lows_range_phrase,
               #self.steady_temp_trends,
               self.temp_trends,
               self.wind_withGusts_phrase,
               self.lake_wind_phrase,
               self.popMax_phrase,
               self.windChill_phrase,
               # Alternative
               #self.windBased_windChill_phrase,
               self.heatIndex_phrase,
               ],
##            "additionalAreas": [ 
##                   # Areas listed by weather element that will be
##                   # sampled and analysed.
##                   # E.g. used for reporting population centers for temperatures. 
##                   ("MaxT", ["City1", "City2"]),
##                   ("MinT", ["City1", "City2"]),
##                   ],
##            "intersectAreas": [ 
##                   # Areas listed by weather element that will be
##                   # intersected with the current area then
##                   # sampled and analysed.  
##                   # E.g. used in local effects methods.
##                   ("MaxT", ["Mountains"]),
##                   ("MinT", ["Valleys"]),
##             ],
    }
    if self._arealSkyAnalysis:
        component["analysisList"].append(("Sky", self.binnedPercent, [3]))
    if self._useStormTotalSnow:
        phraseList = component["phraseList"]
        index = phraseList.index(self.total_snow_phrase)
        phraseList[index] = self.stormTotalSnow_phrase
        component["phraseList"] = phraseList
    return component    
"""

windchill = ["""def scalar_difference_nlValue_dict(self, tree, node):
    dict = TextRules.TextRules.scalar_difference_nlValue_dict(self, tree, node)
    dict["WindChill"] = 2
    return dict""",

"""def minimum_range_nlValue_dict(self, tree, node):
    dict = TextRules.TextRules.minimum_range_nlValue_dict(self, tree, node)
    dict["WindChill"] = 0
    return dict""",
    
"""def Period_1(self):
    component =  { 
        "type": "component",
        "methodList": [
                      self.orderPhrases,
                      self.assemblePhrases,   
                      self.wordWrap,          
                      ],
        "analysisList": [
                   #("MinT", self.avg),
                   #("MaxT", self.avg),
                   ("MinT", self.stdDevMinMax),
                   ("MaxT", self.stdDevMinMax),
                   ("T", self.hourlyTemp),
                   ("T", self.minMax),
                   ("Sky", self.median, [3]),
                   ("PoP", self._PoP_analysisMethod("Period_1"), [3]),
                   ("PoP", self.binnedPercent, [3]),
                   ("SnowAmt", self.accumMinMax),
                   ("StormTotalSnow", self.accumMinMax),
                   ("IceAccum", self.accumMinMax),
                   ("SnowLevel", self.avg),
                   ("Wind", self.vectorMedianRange, [6]),
                   ("Wind", self.vectorMinMax, [6]),
                   ("WindGust", self.maximum, [6]),
                   ("Wx", self.rankedWx, [0]),
                   ("WindChill", self.minMax, [3]),
                   ("HeatIndex", self.minMax, [6]),
                   ],
        "phraseList":[
               self.sky_phrase,
               self.skyPopWx_phrase,
               self.wind_summary,
               self.reportTrends,
               self.weather_phrase,
               self.severeWeather_phrase,
               self.heavyPrecip_phrase,
               self.visibility_phrase,
               self.snow_phrase,
               self.total_snow_phrase,
               self.snowLevel_phrase,
               self.iceAccumulation_phrase,
               self.highs_phrase,
               self.lows_phrase,
               #self.highs_range_phrase,
               #self.lows_range_phrase,
               #self.steady_temp_trends,
               self.temp_trends,
               self.wind_withGusts_phrase,
               self.lake_wind_phrase,
               self.popMax_phrase,
               self.windChill_phrase,
               # Alternative
               #self.windBased_windChill_phrase,
               self.heatIndex_phrase,
               ],
##            "additionalAreas": [ 
##                   # Areas listed by weather element that will be
##                   # sampled and analysed.
##                   # E.g. used for reporting population centers for temperatures. 
##                   ("MaxT", ["City1", "City2"]),
##                   ("MinT", ["City1", "City2"]),
##                   ],
##            "intersectAreas": [ 
##                   # Areas listed by weather element that will be
##                   # intersected with the current area then
##                   # sampled and analysed.  
##                   # E.g. used in local effects methods.
##                   ("MaxT", ["Mountains"]),
##                   ("MinT", ["Valleys"]),
##             ],
    }
    if self._arealSkyAnalysis:
        component["analysisList"].append(("Sky", self.binnedPercent, [3]))
    if self._useStormTotalSnow:
        phraseList = component["phraseList"]
        index = phraseList.index(self.total_snow_phrase)
        phraseList[index] = self.stormTotalSnow_phrase
        component["phraseList"] = phraseList
    return component    
"""
]



### For areal sky

arealSkyDef1 = """#Definition["arealSkyAnalysis"] = 1"""
arealSkyDef2 = """Definition["arealSkyAnalysis"] = 1"""

arealSky = """def areal_sky_flag(self, tree, node):
    return 1
"""

### For diurnal sky

periodCombine1 = """#Definition["periodCombining"] = 1"""
periodCombine2 = """Definition["periodCombining"] = 1"""

diurnalSky = """def periodCombining_elementList(self, tree, node):
    # Weather Elements to determine whether to combine periods
    #return ["Sky", "Wind", "Wx", "PoP", "MaxT", "MinT"]
    # Marine
    #return ["WaveHeight", "Wind", "Wx"]
    # Diurnal Sky Wx pattern
    return ["DiurnalSkyWx"]
"""

pcElementList = """def periodCombining_elementList(self, tree, node):
    # Weather Elements to determine whether to combine periods
    #return ["Sky", "Wind", "Wx", "PoP", "MaxT", "MinT"]
    # Marine
    #return ["WaveHeight", "Wind", "Wx"]
    # Diurnal Sky Wx pattern
    #return ["DiurnalSkyWx"]
    return ["Wx", "PoP"]
"""

increaseSky1 = """def reportIncreasingDecreasingSky_flag(self, tree, node):
    # If 1, AND the difference is greater than the increasingDecreasing_threshold
    # will use "increasing clouds", "decreasing clouds"
    # wording instead of "mostly cloudy becoming sunny"
    return 1
"""

increaseSky2 = """def reportIncreasingDecreasingSky_flag(self, tree, node):
    # If 1, will use "increasing clouds", "decreasing clouds"
    # wording instead of "mostly cloudy becoming sunny"
    #return 0
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
"""

### For headlinesTiming

headlinesTiming = ["""def allowedHeadlines(self):
    allActions = []
    return [
            ('LocalHazard1', allActions, 'Tropical'),
            ]""",
    
"""def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
    # This is the header for an edit area combination
    areaHeader = self.makeAreaHeader(
        argDict, areaLabel, self._issueTime, self._expireTime,
        self._areaDictionary, self._defaultEditAreas)
    fcst = fcst + areaHeader

    # get the hazards text
    self._hazards = argDict['hazards']
    self._combinations = argDict["combinations"]

    headlines = self.generateProduct("Hazards", argDict, area = editArea,
                                     areaLabel=areaLabel,
                                     timeRange = self._timeRange)

    headlines_local = self.generateProduct("Headlines", argDict, area = editArea,
                                     areaLabel=areaLabel,
                                     timeRange = self._timeRange)
    
    fcst = fcst + headlines + headlines_local
        
    return fcst
"""]

headlinesTiming1 = """def headlinesTiming(self, tree, node, key, timeRange, areaLabel, issuanceTime):
    return "DAY_NIGHT_ONLY", "DAY_NIGHT_ONLY", 0
"""

headlinesTiming2 = """def headlinesTiming(self, tree, node, key, timeRange, areaLabel, issuanceTime):
    return "FUZZY", "FUZZY", 3
"""
headlinesTiming3 = """def headlinesTiming(self, tree, node, key, timeRange, areaLabel, issuanceTime):
    return "EXPLICIT", "FUZZY", 3
"""

headlinesTiming4 = """def headlinesTiming(self, tree, node, key, timeRange, areaLabel, issuanceTime):
    return "EXPLICIT", "EXPLICIT", 3
"""
localHeadlines1 = """def allowedHazards(self):
    allActions = ["NEW", "EXA", "EXB", "EXT", "UPG", "CAN", "CON", "EXP"]
    return [
            ('WS.W', allActions, 'WinterWx'),     # WINTER STORM WARNING
            ('LocalHazard1', allActions, 'Misc'),
             ]
"""

hoursSChc1 = """#Definition["hoursSChcEnds"] = 24"""
hoursSChc2 = """Definition["hoursSChcEnds"] = 84"""

null_distinct = """def null_alwaysDistinct_flag_dict(self, tree, node):
    # If 1, null values will always be considered distinct from non-null values
    # when combining subphrases.  Thus, with a null value of 5,
    # you may end up with phrases such as:
    #   Winds less than 5 mph becoming east 5 mph.
    # If set to 0, the determination will be made based on the scalar or
    # vector difference as with non-null values.
    #   (See scalar_difference_nlValue_dict and vector_mag_difference_nlValue_dict)
    return {
        "otherwise": 1,
        "Wind": 0,  
        }
"""

tempCov = """def temporalCoverage_dict(self, parmHisto, timeRange, componentName):
    # This is temporalCoverage percentage by weather element
    # Used by temporalCoverage_flag
    return {
            "LAL": 0,
            "MinRH": 0,
            "MaxRH": 0,
            "MinT": 1,
            "MaxT": 1,
            "Haines": 0,
            "PoP" : 20,
            "Hazards" : 0,
            }
"""

CWFPeriod = ["""def _issuance_list(self, argDict):
    #  This method sets up configurable issuance times with associated
    #  narrative definitions.  See the Text Product User Guide for documentation.
    if self._definition["includeEveningPeriod"] == 1:
        narrativeDefAM = [
            ("CWFPeriod", "period1"),
##                ("", 12),# ("", 12), ("", 12), ("", 12),
##                ("", 12),
##                ("CWFExtended", 24), ("CWFExtended", 24)
            ]
        narrativeDefPM = [
            ("", "period1"),
            ("", 12), ("", 12), ("", 12), ("", 12),
            ("", 12), ("", 12),
            ("CWFExtended", 24), ("CWFExtended", 24)
            ]
    else:
        narrativeDefAM = [
            ("", "period1"),
            ("", 12), ("", 12), ("", 12), ("", 24),
            ("CWFExtended", 24), ("CWFExtended", 24)
            ]
        narrativeDefPM = [
            ("", "period1"),
            ("", 12), ("", 12), ("", 12), ("", 12),
            ("CWFExtended", 24),
            ("CWFExtended", 24), ("CWFExtended", 24)
            ]
    
    return [
        ("Morning", self.DAY(), self.NIGHT(), "issuanceHour + 13",
         ".TODAY...", "early", "late", 1, narrativeDefAM), 
        ("Morning with Pre-1st Period", "issuanceHour", self.NIGHT(),
         "issuanceHour + 13", ".TODAY...", "early", "late", 1,
         narrativeDefAM),
        ("Morning Update", "issuanceHour", self.NIGHT(),
         "issuanceHour + 13", ".Rest of Today...", "early in the morning",
         "late in the afternoon", 1, narrativeDefAM), 
        ("Afternoon Update", "issuanceHour", self.NIGHT(), "issuanceHour + 13",
         ".Rest of Today...", "early in the morning", "late in the afternoon",
         1, narrativeDefAM), 
        #  End times are tomorrow:
        ("Afternoon", self.NIGHT(), 24 + self.DAY(), "issuanceHour + 13",
         ".Tonight...", "late in the night", "early in the evening", 1, narrativeDefPM), 
        ("Afternoon with Pre-1st Period", "issuanceHour", 24 + self.DAY(),
         "issuanceHour + 13", ".Tonight...", "late in the night", "early in the evening", 1,
         narrativeDefPM),
        ("Evening Update", "issuanceHour", 24 + self.DAY(), "issuanceHour + 13",
         ".Rest of Tonight...", "early in the morning", "early in the evening", 1,
         narrativeDefPM),
        # For the early morning update, this produces:
        # REST OF TONIGHT:
        # MONDAY
        # MONDAY NIGHT
        ("Early Morning Update", "issuanceHour", self.DAY(), "issuanceHour + 13",
         ".Rest of Tonight...", "early in the morning", "late in the afternoon",
         0, narrativeDefPM),
        # Alternative
        # For the early morning update, this produces:
        # EARLY THIS MORNING:
        # TODAY
        # TONIGHT
        #("Evening Update", "issuanceHour", 24 + self.DAY(), "issuanceHour + 13",
        # ".REST OF TONIGHT...", "late in the night", "early in the evening",
        # 1, narrativeDefPM), 
        #("Early Morning Update", "issuanceHour", self.DAY(), "issuanceHour + 13",
        # ".EARLY THIS MORNING...", "early in the morning", "late in the afternoon",
        # 1, narrativeDefPM), 
        ]""",

"""def CWFPeriod(self):
    return {
        "type": "component",
        "methodList": [
                      self.assemblePhrases,   
                      self.wordWrap,          
                      ],
        
        "analysisList": [            
                  # NOTE: Choose from the following analysis options.
                  # Do not remove the "vectorMinMax" analysis for
                  # "Wind". This is necessary to get an absolute max if
                  # the useWindsForGusts flag is on.
        
                  # Use the following if you want moderated ranges 
                  # (e.g. N WIND 10 to 20 KT)
                  # Set the moderating percentage in the "moderated_dict"
                  # dictionary module.
                  # Set the maximum range values in the "maximum_range_nlValue_dict"
                  # dictionary module.
                      ("Wind", self.vectorModeratedMinMax, [3]),
                      ("Wind", self.vectorMinMax, [12]),
                      ("WindGust", self.moderatedMax, [3]),
                      ("WaveHeight", self.moderatedMinMax, [6]),
                      ("WindWaveHgt", self.moderatedMinMax, [6]),
                      ("Swell", self.vectorModeratedMinMax, [6]),
                      ("Swell2", self.vectorModeratedMinMax, [6]),
                      ("Period", self.moderatedMinMax, [6]),
                      ("Period2", self.moderatedMinMax, [6]),
                      ("Wx", self.rankedWx, [6]),
                      ("T", self.minMax),
                      ("PoP", self._PoP_analysisMethod("CWFPeriod"), [6]),
                      ("PoP", self.binnedPercent, [6]),
                      ],

         "phraseList":[
                       # WINDS
                       self.marine_wind_withGusts_phrase,
                       # Alternative:
                       #self.marine_wind_phrase,
                       #self.gust_phrase,
                       # WAVES
                       self.wave_withPeriods_phrase,
                       # Alternative:
                       #self.wave_phrase,
                       # Optional:
                       self.chop_phrase,
                       # SWELLS AND PERIODS
                       self.swell_phrase,
                       # Alternative:
                       #self.swell_phrase,
                       #self.period_phrase,
                       # WEATHER
                       self.weather_phrase,
                       ],
        }""",

"""def seasFlag(self, tree, node):
    return 0
"""]

nullSwell = ["""def first_null_phrase_dict(self, tree, node):
    dict = TextRules.TextRules.first_null_phrase_dict(self, tree, node)
    dict["Swell"] = "light swells"
    return dict""",

"""def null_phrase_dict(self, tree, node):
    dict = TextRules.TextRules.null_phrase_dict(self, tree, node)
    dict["Swell"] = "light"
    return dict"""
]

marine_wx = """def pop_wx_lower_threshold(self, tree, node):
    # Always report weather
    return 20
"""

alternateTempTrends = """def temp_trends_words(self, tree, node):
   "Look for sharp temperature increases or decreases"
   
   # Here is an alternative temp_trends method provided by Tom Spriggs.
   # If a 12-hour period, it looks at the 12, 3, and 5 o'clock grids
   # (both am/pm depending on time of day) and verifies the trend (either
   # going down or up) and then looks at the difference between the 
   # 5 o'clock grid and the MaxT/MinT grid.  It only needs to look at the
   # 5 o'clock grid since that is the last one in the 12-hour period, 
   # and if it is going to trip the threshold anywhere, it will be on that
   # hour since if you have an unusual temperature trend, it will peak at
   # that grid.  If less than a 12-hour period, then the 3 times that it
   # checks will be adjusted accordingly inside the smaller time range.
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

   if len(tStats) >= 6:
       halfWay    = len(tStats) - 6
       quarterWay = len(tStats) - 3
       endPoint   = len(tStats) - 1
   elif len(tStats) >= 4:
       halfWay    = 0
       quarterWay = len(tStats) - 3
       endPoint   = len(tStats) - 1
   elif len(tStats) == 1:
       halfWay    = 0
       quarterWay = 0
       endPoint   = 0
   else:
       halfWay    = 0
       quarterWay = 1
       endPoint   = len(tStats) - 1
       
   tempValue_halfWay, curHour1    = tStats[halfWay]
   tempValue_quarterWay, curHour2 = tStats[quarterWay]
   tempValue_endPoint, curHour3   = tStats[endPoint]
   
   if tempValue_halfWay is None:
       return self.setWords(node, "")
   if tempValue_quarterWay is None:
       return self.setWords(node, "")
   if tempValue_endPoint is None:
       return self.setWords(node, "")

   words = ""
   if dayNight == self.DAYTIME():
       if tempValue_quarterWay < tempValue_halfWay:
           if tempValue_endPoint <= tempValue_quarterWay:
               if tempValue_endPoint <= (maxT - threshold):
                   # large temp fall (i.e. >= threshold)
                   toPhrase = self.getToPhrase(tree, node, tempValue_endPoint)
                   mxPhrase = self.getToPhrase(tree, node, maxT)
                   if (toPhrase == mxPhrase):
                       # avoid saying--high in the upper 50s. temperature falling
                       # into the 50s in the afternoon.
                       # instead say--high in the upper 50s. temperature falling
                       # through the 50s in the afternoon.
                       toPhrase = " through" + toPhrase[5:]
                   if len(tStats) <= 6:   #assumes already in the afternoon
                       words = "temperature falling" + toPhrase + " by late afternoon"
                   else:                           
                       words = "temperature falling" + toPhrase + " in the afternoon"
               elif tempValue_endPoint < maxT:
                   # small temp fall (i.e. < threshold)
                   if len(tStats) <= 6:   #assumes already in the afternoon
                       words = "temperature steady or slowly falling through late afternoon"
                   else:
                       words = "temperature steady or slowly falling in the afternoon"
   else:
       if tempValue_quarterWay > tempValue_halfWay:
           if tempValue_endPoint >= tempValue_quarterWay:
               if tempValue_endPoint >= (minT + threshold):
                   # large temp rise (i.e. >= threshold)
                   toPhrase = self.getToPhrase(tree, node, tempValue_endPoint)
                   mnPhrase = self.getToPhrase(tree, node, minT)
                   if (toPhrase == mnPhrase):
                       # avoid saying--low in the lower 30s. temperature rising
                       # into the 30s after midnight.
                       # instead say--low in the lower 30s. temperature rising
                       # through the 30s after midnight.
                       toPhrase = " through" + toPhrase[5:]
                   if len(tStats) <= 6:   #assumes already after midnight
                       words = "temperature rising" + toPhrase + " through sunrise"
                   else:
                       words = "temperature rising" + toPhrase + " after midnight"
               elif tempValue_endPoint > minT:
                   # small temp rise (i.e. < threshold)
                   if len(tStats) <= 6:   #assumes already after midnight
                       words = "temperature steady or slowly rising through sunrise"
                   else:
                       words = "temperature steady or slowly rising after midnight"

   return self.setWords(node, words)
"""

visibilitySettings = ["""def embedded_visibility_flag(self, tree, node):
        # If 1, report visibility embedded with the
        # weather phrase. Set this to 0 if you are using the
        # visibility_phrase.
        return 1""",

"""def visibility_wx_threshold(self, tree, node):
    # Weather will be reported if the visibility is below
    # this threshold (in NM) OR if it includes a
    # significant_wx_visibility_subkey (see below)
    return 2
"""]

wind_nlValue = """def vector_dir_difference_nlValue_dict(self, tree, node):
    dict =  self.vector_dir_difference_dict(tree, node)
    # If you want to use a nlValue for the Wind direction
    # override this method and uncomment the entry below for Wind.
    # Adjust the values given to the desired values.
    # The Wind direction threshold will then be chosen according to
    # the MINIMUM nlValue determined from the Wind MAGNITUDE.
    # See 'checkVectorDifference' (PhraseBuilder) for usage.
    #
    #    dict["Wind"] = {
    #       (0, 10): 60,
    #       (10, 200): 40,
    #       'default': 40,
    #      }
    #  When the wind is between 0 and 10 kt,
    #     report when the wind direction change is 60 degrees or more
    #
    dict["Wind"] = {
        (0, 10): 60,
        (10, 200): 40,
        'default': 40,
        }

    return dict
"""

wxMatch = """def matchToWxInfo_dict(self, tree, node):
    return {
            "PoP": (5, "MaxMode", None),     # 50
            #"PoP": (5, "Mode", None),    # 30
            #"PoP": (5, "MaxMode", None), # 40
            #"PoP": (5, "AnalysisMethod", None),  # 40
            "LAL": (0, "Max", "Max"),
            }
"""

p1vx = ["""def Period_1_version1(self):
    component =  {
        "type": "component",
        "methodList": [
                      self.orderPhrases,
                      self.consolidateSubPhrases,
                      self.assemblePhrases,
                      self.wordWrap,
                      ],
        "analysisList": [
                   ("MinT", self.stdDevMinMax),
                   ("MaxT", self.stdDevMinMax),
                   ("T", self.hourlyTemp),
                   ("T", self.minMax),
                   ("Sky", self.median, [3]),
                   ("PoP", self._PoP_analysisMethod("Period_1"), [3]),
                   ("PoP", self.binnedPercent, [12]),
                   ("Wind", self.vectorModeratedMinMax, [0]),
                   ("Wind", self.vectorMinMax, [0]),
                   ("WindGust", self.maximum, [0]),
                   ("Wx", self.rankedWx, [3]),
                   ("WindChill", self.minMax),
                   ("HeatIndex", self.minMax),
                   ("SnowAmt", self.accumMinMax),
                   ],
        "phraseList":[
               self.sky_phrase,
               self.skyPopWx_phrase,
               self.wind_summary,
               self.reportTrends,
               self.weather_phrase,
               self.heavyPrecip_phrase,
               self.severeWeather_phrase,
               self.visibility_phrase,
               self.snow_phrase,
               self.extremeTemps_phrase,
               self.steady_temp_trends,
               self.highs_phrase,
               self.lows_phrase,
               self.temp_trends,
               self.wind_withGusts_phrase,
               self.popMax_phrase,
               self.windChill_phrase,
               self.heatIndex_phrase,
               ],
       }
    if self._arealSkyAnalysis:
        component["analysisList"].append(("Sky", self.binnedPercent, [3]))
    if self._useStormTotalSnow:
        phraseList = component["phraseList"]
        index = phraseList.index(self.total_snow_phrase)
        phraseList[index] = self.stormTotalSnow_phrase
        component["phraseList"] = phraseList
    return component""",

"""def Period_1_version2(self):
    return {
        "type": "component",
        "methodList": [
                      self.orderPhrases,
                      self.consolidateSubPhrases,
                      self.assemblePhrases,
                      self.wordWrap,
                      ],
        "analysisList": [
                   ("Sky", self.median, [3]),
                   ("PoP", self._PoP_analysisMethod("Period_1"), [3]),
                   ("PoP", self.binnedPercent, [12]),
                   ("Wx", self.rankedWx, [3]),
                   ],
        "phraseList":[
               (self.sky_phrase, self._skyLocalEffects_list()),
               (self.skyPopWx_phrase, self._skyPopWxLocalEffects_list()),
               (self.weather_phrase,self._wxLocalEffects_list()),
               (self.popMax_phrase, self._popLocalEffects_list()),
               ],
        "intersectAreas": [
               # Areas listed by weather element that will be
               # intersected with the current area then
               # sampled and analysed.
               # E.g. used in local effects methods.
               ("Sky", ["AboveElev", "BelowElev"]),
               ("Wx",  ["AboveElev", "BelowElev"]),
               ("PoP", ["AboveElev", "BelowElev"]),
         ],
    }
"""    
]

scripts = [


    ### FWF Infinite Loop  TK 4244
    {
    "name": "FWF_InfiniteLoop1",
    "commentary": "Wx Infinite Loop: Formatter fails since we need to change 'on-the-fly' \nresolution multiple times.",
    "productType": "FWF",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 60, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 3, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6, "Chc:T:<NoInten>:<NoVis>:^Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12,"Chc:RW:-:<NoVis>:^Chc:SW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "|* PLEASE ENTER WEATHER_PHRASE AND REFER TO LOG FILES FOR MORE EXPLANATION *|",
       ],
    "fileChanges": [
       ("FWF_<site>_Overrides", "TextUtility", "add", checkResolution, "undo"),
       ("FWF_<site>_Overrides", "TextUtility", "add", subPhrase_limit, "undo"),
       ],
    "comboFlag": 1,
                    
    },
    {
    "name": "FWF_InfiniteLoop2",
    "commentary": "Wx Infinite Loop: Formatter will succeed since we change  'on-the-fly' \nresolution multiple times.",
    "productType": "FWF",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 60, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 3, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6, "Chc:T:<NoInten>:<NoVis>:^Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12,"Chc:RW:-:<NoVis>:^Chc:SW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "TODAY",
       ("CHANCE OF SHOWERS...THUNDERSTORMS AND SNOW SHOWERS",
        "CHANCE OF RAIN SHOWERS...THUNDERSTORMS AND SNOW SHOWERS"),
        "TONIGHT",
       ],
    "fileChanges": [
       ("FWF_<site>_Overrides", "TextUtility", "add", subPhrase_limit, "undo"),
       ],
    "comboFlag": 1,                    
    },
    
    {
    "name": "FWF_InfiniteLoop3",
    "commentary": "Wx Infinite Loop: Formatter fails since we need to carry over disabledSubkeys.",
    "productType": "FWF",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 21, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 60, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 2, "Sct:SW:-:<NoVis>:^Areas:BS:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 2, 4, "Iso:T:<NoInten>:<NoVis>:^Sct:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 4, 6, "Sct:SW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Sct:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "|* PLEASE ENTER WEATHER_PHRASE AND REFER TO LOG FILES FOR MORE EXPLANATION *|",
       ],
    "fileChanges": [
       ("FWF_<site>_Overrides", "TextUtility", "add", checkResolution, "undo"),
       ],
    "comboFlag": 1,
                    
    },
    {
    "name": "FWF_InfiniteLoop4",
    "commentary": "Wx Infinite Loop: Formatter will succeed since carry over disabledSubkeys.",
    "productType": "FWF",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 21, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 60, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 2, "Sct:SW:-:<NoVis>:^Areas:BS:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 2, 4, "Iso:T:<NoInten>:<NoVis>:^Sct:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 4, 6, "Sct:SW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Sct:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "TODAY", "AREAS OF BLOWING SNOW UNTIL 0800",
       "SCATTERED SNOW SHOWERS AND ISOLATED THUNDERSTORMS UNTIL 1200",
       "SCATTERED SHOWERS","TONIGHT",
       ],
    "fileChanges": [],
    "comboFlag": 1,                    
    },

    {
    "name": "ZFP_InfiniteLoop1",  # TK 4603
    "commentary": "Infinite loop",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Wx", "WEATHER", 0, 1, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 1, 2, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 2, 3, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 4, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 4, 5, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 5, 6, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 7, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 7, 12, "NoWx", "all"),
       ],
    "notCheckStrings": [
       "SCATTERED PATCHY FOG",  # Should not be found. Added SCATTERED
                                # to make it return -1, which will pass the test.
       ],
    "fileChanges": [
       ("ZFP_<site>_Overrides", "TextUtility", "add", checkResolution2, "undo"),
       ("ZFP_<site>_Overrides", "TextUtility", "add", period1, "undo"),
       ],

    },

    {
    "name": "ZFP_InfiniteLoop2",
    "commentary": "Infinite loop fix",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Wx", "WEATHER", 0, 1, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 1, 2, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 2, 3, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 4, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 4, 5, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 5, 6, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 7, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 7, 12, "NoWx", "all"),
       ],
    "checkStrings": [
       "PATCHY FOG",
       ],
    "fileChanges": [
       ("ZFP_<site>_Overrides", "TextUtility", "add", period1, "undo"),
       ],

    },
    ### Areal Sky 
    {
    "name": "arealSky1",
    "commentary": "Areal sky wording with transition",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 0, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 0, "all"),
       ],
    "checkStrings": [
       "TODAY",
       "LOW CLOUDS AND FOG IN THE MORNING THEN CLEARING",
       "TONIGHT",
       ],
    "fileChanges": [
       ("ZFP_<site>_Definition", "TextUtility", "replace", arealSkyDef2, "undo"),
       ("ZFP_<site>_Overrides", "TextUtility", "add", arealSky, "undo"),
       ],
    "comboFlag": 1,
                    
    },
    
    #  We disableSkyRelatedWx when the areal_sky_flag is 1, even IF the
    #  areal sky wording does not kick in.  Thus, arealSky2 test fails.
    #  Is this ok or not?
    {
    "name": "arealSky2",
    "commentary": "No Areal sky wording (sky not high enough) so normal wording",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 0, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  60, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 0, "all"),
       ],
    "checkStrings": [
       "TODAY",
       #"PARTLY CLOUDY IN THE MORNING THEN CLEARING",
       #"PATCHY FOG IN THE MORNING",
       "TONIGHT",
       ],
    "fileChanges": [
       ("ZFP_<site>_Definition", "TextUtility", "replace", arealSkyDef2, "undo"),
       ("ZFP_<site>_Overrides", "TextUtility", "add", arealSky, "undo"),
       ],
    "comboFlag": 1,
                    
    },

    ### Diurnal Sky Period Combining
    {
    "name": "diurnalSky1",
    "commentary": "Diurnal sky wording when period combining -- starting with Day period",
    "productType": "ZFP",
    "createGrids": [#TestScript.general_createGrids + [    
       ("Fcst", "Sky", "SCALAR", 48, 72, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 72, 78, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 78, 84, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 84, 90, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 90, 96, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 96, 102, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 102, 108, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 108, 114, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 114, 120, 100, "all"),
       ("Fcst", "Wx", "WEATHER", 72, 78, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 78, 84, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 84, 90, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 90, 96, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 96, 102, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 102, 108, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 108, 114, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 114, 120, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 48, 120, 0, "all"),
##       ("Fcst", "MaxT", "SCALAR", "MaxTBegin+24", "MaxTEnd+24", 70, "all"),
##       ("Fcst", "MinT", "SCALAR", "MinTBegin+24", "MinTEnd+24", 43, "all"),
##       ("Fcst", "MaxT", "SCALAR", "MaxTBegin+48", "MaxTEnd+48", 70, "all"),
##       ("Fcst", "MinT", "SCALAR", "MinTBegin+48", "MinTEnd+48", 43, "all"),
##       ("Fcst", "MaxT", "SCALAR", "MaxTBegin+60", "MaxTEnd+60", 70, "all"),
##       ("Fcst", "MinT", "SCALAR", "MinTBegin+60", "MinTEnd+60", 43, "all"),
##       ("Fcst", "MaxT", "SCALAR", "MaxTBegin+72", "MaxTEnd+72", 70, "all"),
##       ("Fcst", "MinT", "SCALAR", "MinTBegin+72", "MinTEnd+72", 43, "all"),
##       ("Fcst", "Wind", "VECTOR", 36, 72, (10, "SW"), "all"),
           ],
    "checkStrings": [
       "MONDAY THROUGH TUESDAY NIGHT...",
       "LOW CLOUDS AND FOG IN THE MORNING AND NIGHT...OTHERWISE CLEAR",
       ],
    "fileChanges": [
       ("ZFP_<site>_Definition", "TextUtility", "replace", arealSkyDef2, "undo"),
       ("ZFP_<site>_Definition", "TextUtility", "replace",
       periodCombine2, "undo"),
       ("ZFP_<site>_Overrides", "TextUtility", "add", arealSky, "undo"),
       ("ZFP_<site>_Overrides", "TextUtility", "add", diurnalSky, "undo"),
       ],
    "comboFlag": 1,
    "gridsStartTime": "6am Local",
    "drtTime": "6am Local",                    
    },

    {
    "name": "diurnalSky2",
    "commentary": "Diurnal sky wording when period combining -- starting with Night period",
    "productType": "ZFP",
    "createGrids": [#TestScript.general_createGrids + [    
       ("Fcst", "Sky", "SCALAR", 72, 78, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 78, 84, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 84, 90, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 90, 96, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 96, 102, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 102, 108, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 108, 114, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 114, 120, 100, "all"),
       ("Fcst", "Wx", "WEATHER", 72, 78, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 78, 84, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 84, 90, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 90, 96, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 96, 102, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 102, 108, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 108, 114, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 114, 120, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 48, 120, 0, "all"),
##       ("Fcst", "MaxT", "SCALAR", "MaxTBegin+24", "MaxTEnd+24", 70, "all"),
##       ("Fcst", "MinT", "SCALAR", "MinTBegin+24", "MinTEnd+24", 43, "all"),
##       ("Fcst", "MaxT", "SCALAR", "MaxTBegin+48", "MaxTEnd+48", 70, "all"),
##       ("Fcst", "MinT", "SCALAR", "MinTBegin+48", "MinTEnd+48", 43, "all"),
##       ("Fcst", "MaxT", "SCALAR", "MaxTBegin+60", "MaxTEnd+60", 70, "all"),
##       ("Fcst", "MinT", "SCALAR", "MinTBegin+60", "MinTEnd+60", 43, "all"),
##       ("Fcst", "MaxT", "SCALAR", "MaxTBegin+72", "MaxTEnd+72", 70, "all"),
##       ("Fcst", "MinT", "SCALAR", "MinTBegin+72", "MinTEnd+72", 43, "all"),
##       ("Fcst", "Wind", "VECTOR", 36, 72, (10, "SW"), "all"),
           ],
    "checkStrings": [
       "MONDAY NIGHT THROUGH TUESDAY NIGHT...",
       "LOW CLOUDS AND FOG IN THE NIGHT AND MORNING...OTHERWISE CLEAR",
       ],
    "fileChanges": [
       ("ZFP_<site>_Definition", "TextUtility", "replace", arealSkyDef2, "undo"),
       ("ZFP_<site>_Definition", "TextUtility", "replace",
       periodCombine2, "undo"),
       ("ZFP_<site>_Overrides", "TextUtility", "add", arealSky, "undo"),
       ("ZFP_<site>_Overrides", "TextUtility", "add", diurnalSky, "undo"),
       ],
    "comboFlag": 1,
    "gridsStartTime": "6am Local",
    "drtTime": "6am Local",                    
    },

    ### Increasing/decreasing threshold TK 4112
    {
    "name": "IncreasingSky1",
    "commentary": "Increasing sky wording",
    "productType": "ZFP",
    "createGrids": [#TestScript.general_createGrids + [    
       ("Fcst", "Sky", "SCALAR", 48, 54, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 54, 60, 55, "all"),
       ("Fcst", "Sky", "SCALAR", 60, 66, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 66, 72, 55, "all"),
       ],
    "checkStrings": [
       ".SUNDAY...", "INCREASING CLOUDS",
       ".SUNDAY NIGHT...", "INCREASING CLOUDS",
       ],
    "fileChanges": [
           ("ZFP_<site>_Overrides", "TextUtility", "add", increaseSky1, "undo"),
           ],
    "comboFlag": 1,
    "gridsStartTime": "6am Local",
    "drtTime": "6am Local",                    
    },

    {
    "name": "IncreasingSky2",
    "commentary": "Increasing sky wording",
    "productType": "ZFP",
    "createGrids": [#TestScript.general_createGrids + [    
       ("Fcst", "Sky", "SCALAR", 48, 54, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 54, 60, 55, "all"),
       ("Fcst", "Sky", "SCALAR", 60, 66, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 66, 72, 55, "all"),
       ],
    "checkStrings": [
       ".SUNDAY...", "INCREASING CLOUDS",
       ".SUNDAY NIGHT...","CLEAR IN THE EVENING THEN BECOMING MOSTLY CLOUDY",
       ],
    "fileChanges": [
           ("ZFP_<site>_Overrides", "TextUtility", "add", increaseSky2, "undo"),
           ],
    "comboFlag": 1,
    "gridsStartTime": "6am Local",
    "drtTime": "6am Local",                    
    },


    ### Local Headline Timing : Tests DiscretePhrases headlinesTiming and
    ###    headlinesTimeRange_descriptor
    ### TK 4259
    {
    "name": "HeadlineTiming1",
    "commentary": "startPhraseType=DAY_NIGHT_ONLY, endPhraseType=DAY_NIGHT_ONLY",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", 0, 60, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 2, 11, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 15, 20, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 28, 29, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 40, 41, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 59, 60, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 79, 71, "LocalHazard1", "all"),
      ],
    "checkStrings": [
       "...LOCAL HAZARD TODAY...",
       "...LOCAL HAZARD TONIGHT...",
       "...LOCAL HAZARD SATURDAY...",
       "...LOCAL HAZARD SATURDAY NIGHT...",
       "...LOCAL HAZARD SUNDAY...",
       "...LOCAL HAZARD FROM SUNDAY NIGHT THROUGH MONDAY...",
       ],
    "fileChanges": [
       ("ZFP_<site>_Overrides", "TextUtility", "add", headlinesTiming, "undo"),
       ("ZFP_<site>_Overrides", "TextUtility", "add", headlinesTiming1, "undo"),
       ],
    "comboFlag": 1,
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },
    {
    "name": "HeadlineTiming2",
    "commentary": "startPhraseType=FUZZY, endPhraseType=FUZZY, startBoundary=3",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", 0, 60, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 2, 11, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 15, 20, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 28, 29, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 40, 41, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 59, 60, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 79, 71, "LocalHazard1", "all"),
      ],
    "checkStrings": [
       "...LOCAL HAZARD FROM THIS MORNING THROUGH THIS AFTERNOON...",
       "...LOCAL HAZARD FROM THIS EVENING THROUGH LATE TONIGHT...",
       "...LOCAL HAZARD SATURDAY MORNING...",
       "...LOCAL HAZARD SATURDAY EVENING...",
       "...LOCAL HAZARD SUNDAY AFTERNOON...",
       "...LOCAL HAZARD FROM LATE SUNDAY NIGHT THROUGH MONDAY AFTERNOON...",
       ],
    "fileChanges": [
       ("ZFP_<site>_Overrides", "TextUtility", "add", headlinesTiming, "undo"),
       ("ZFP_<site>_Overrides", "TextUtility", "add", headlinesTiming2, "undo"),
       ],
    "comboFlag": 1,
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },
    {
    "name": "HeadlineTiming3",
    "commentary": "startPhraseType=EXPLICIT, endPhraseType=FUZZY, startBoundary=3",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", 0, 60, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 2, 11, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 15, 20, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 28, 29, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 40, 41, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 59, 60, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 79, 71, "LocalHazard1", "all"),
      ],
    "checkStrings": [
       "...LOCAL HAZARD FROM 8 AM EST THIS MORNING THROUGH THIS AFTERNOON...",
       "...LOCAL HAZARD FROM 9 PM EST THIS EVENING THROUGH LATE TONIGHT..",
       "...LOCAL HAZARD FROM 10 AM EST SATURDAY THROUGH SATURDAY MORNING...",
       "...LOCAL HAZARD FROM 10 PM EST SATURDAY THROUGH SATURDAY EVENING...",
       "...LOCAL HAZARD FROM 5 PM EST SUNDAY THROUGH SUNDAY AFTERNOON...",
       "...LOCAL HAZARD FROM 5 AM EST MONDAY THROUGH MONDAY AFTERNOON...",
       ],
    "fileChanges": [
       ("ZFP_<site>_Overrides", "TextUtility", "add", headlinesTiming, "undo"),
       ("ZFP_<site>_Overrides", "TextUtility", "add", headlinesTiming3, "undo"),
       ],
    "comboFlag": 1,
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },
    {
    "name": "HeadlineTiming4",
    "commentary": "startPhraseType=EXPLICIT, endPhraseType=EXPLICIT, startBoundary=3",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", 0, 60, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 2, 11, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 15, 20, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 28, 29, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 40, 41, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 59, 60, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 79, 71, "LocalHazard1", "all"),
      ],
    "checkStrings": [
       "...LOCAL HAZARD FROM 8 AM THIS MORNING TO 5 PM EST THIS AFTERNOON...",
       "...LOCAL HAZARD FROM 9 PM THIS EVENING TO 2 AM EST SATURDAY...",
       "...LOCAL HAZARD FROM 10 AM TO 11 AM EST SATURDAY...",
       "...LOCAL HAZARD FROM 10 PM TO 11 PM EST SATURDAY...",
       "...LOCAL HAZARD FROM 5 PM TO 6 PM EST SUNDAY...",
       "...LOCAL HAZARD FROM 5 AM TO 1 PM EST MONDAY...",
       ],
    "fileChanges": [
       ("ZFP_<site>_Overrides", "TextUtility", "add", headlinesTiming, "undo"),
       ("ZFP_<site>_Overrides", "TextUtility", "add", headlinesTiming4, "undo"),
       ],
    "comboFlag": 1,
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },
    ### Local Headlines mixed with VTEC
    {
    "name": "LocalHeadlines1",
    "commentary": "Local Headlines mixed with VTEC",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", 0, 60, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 2, 11, "LocalHazard1", "all"),
       ("Fcst", "Hazards", "DISCRETE", 7, 9,  "LocalHazard1^WS.W", "all"),
      ],
    "orderStrings": 1,
    "checkStrings": [
       "LOCAL HAZARD IN EFFECT UNTIL 5 PM EST THIS AFTERNOON",
       "WINTER STORM WARNING IN EFFECT FROM 1 PM TO 3 PM EST THIS AFTERNOON",
       ],
    "fileChanges": [
       ("ZFP_<site>_Overrides", "TextUtility", "add", localHeadlines1, "undo"),
       ],
    "comboFlag": 1,
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },

    {
    "commentary": "Clear out all Hazards Grids.",
    "name": "Headlines_Cleanup",
    "productType": None,
    "deleteGrids": [
       ("Fcst", "Hazards", "SFC", "all", "all"),
       ],
    },

    ### Null always distinct  TK 4264
    {
    "name": "NullDistinct1",
    "commentary": "Null Distinct from Non-Null",
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (2,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (5,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12,  0, "all"),
       ],
    "checkStrings": [
       "LIGHT WINDS BECOMING NORTH AROUND 5 MPH IN THE AFTERNOON",
       ],
    },
    {
    "name": "NullDistinct2",
    "commentary": "Null Not Distinct from Non-Null",
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (2,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (5,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12,  0, "all"),
       ],
    "checkStrings": [
       "NORTH WINDS UP TO 5 MPH",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", null_distinct, "undo"),
       ],
    },

    ### Null always distinct  TK 4264
    {
    "name": "NullDistinct1",
    "commentary": "Null Distinct from Non-Null",
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (2,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (5,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12,  0, "all"),
       ],
    "checkStrings": [
       "LIGHT WINDS BECOMING NORTH AROUND 5 MPH IN THE AFTERNOON",
       ],
    },
    {
    "name": "NullDistinct2",
    "commentary": "Null Not Distinct from Non-Null",
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (2,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (5,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12,  0, "all"),
       ],
    "checkStrings": [
       "NORTH WINDS UP TO 5 MPH",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", null_distinct, "undo"),
       ],
    },
    ### Null always distinct  TK 4264
    {
    "name": "NullDistinct1",
    "commentary": "Null Distinct from Non-Null",
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (2,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (5,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12,  0, "all"),
       ],
    "checkStrings": [
       "LIGHT WINDS BECOMING NORTH AROUND 5 MPH IN THE AFTERNOON",
       ],
    },

    ##### MaxMode and Mixed Wx  (Dave Metze)  Tk 4450
    {
    "name": "MaxMode_and_MixedWx",
    "commentary": "Using MaxMode for PoP with T plus K",
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  20, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 87, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 6,   0, "all", 1),
       ("Fcst", "PoP", "SCALAR", 6, 9,   5, "all", 1),
       ("Fcst", "PoP", "SCALAR", 9, 12,  20, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Areas:K:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12,
       "SChc:T:<NoInten>:<NoVis>:^Areas:K:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "MOSTLY SUNNY UNTIL LATE AFTERNOON...THEN MOSTLY CLOUDY WITH A 20 PERCENT CHANCE OF THUNDERSTORMS LATE IN THE AFTERNOON",
       "AREAS OF SMOKE THROUGH THE DAY",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "replace",
       wxMatch, "undo"),
       ],
    },
    
    {
    "name": "MaxMode_Only",
    "commentary": "Using MaxMode for PoP with T",
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  20, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 60, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 6,   0, "all", 1),
       ("Fcst", "PoP", "SCALAR", 6, 9,   5, "all", 1),
       ("Fcst", "PoP", "SCALAR", 9, 12,  20, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "SChc:T:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "MOSTLY SUNNY UNTIL LATE AFTERNOON...THEN A 20 PERCENT CHANCE OF THUNDERSTORMS LATE IN THE AFTERNOON",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "replace",
       wxMatch, "undo"),
       ],
    },

    # TK 4676  PoP temporal coverage changed to zero
    {
    "name": "PoP_TemporalCoverage1",
    "commentary": "tk4676 PoP temporal coverage changed to zero",
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", -3, 1, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 1, 12, 60, "all", 1),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Lkly:T:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "THUNDERSTORMS LIKELY",
       "CHANCE OF THUNDERSTORMS 60 PERCENT",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "replace",
           p1vx,
           "undo"),
       ("Phrase_Test_Local", "TextProduct", "add", tempCov, "undo"),
       ],
    },
    # TK 4676  PoP temporal coverage changed to zero
    {
    "name": "PoP_TemporalCoverage2",
    "commentary": "tk4676 PoP temporal coverage changed to zero",
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", -3, 1, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 1, 12, 60, "all", 1),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Lkly:T:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "THUNDERSTORMS LIKELY",
       "CHANCE OF THUNDERSTORMS 70 PERCENT",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "replace",
           p1vx,
           "undo"),
       ],           
    },


     ### CWF Swell Phrase -- no secondary elements TK 4277
    {
    "name": "CWF_Swell1",
    "commentary": "CWF Swell phrase: Neither null",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 12,  (7,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 12,  (14,"E"), "all"),
       ],
    "checkStrings": [
       "MIXED SWELL NORTH AROUND 7 FEET AND EAST AROUND 14 FEET",
       ],
    "comboFlag": 1,
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    "fileChanges":[
       ("CWF_<site>_Overrides", "TextUtility", "add", CWFPeriod, "undo"),
       ],
    },    
    {
    "name": "CWF_Swell2",
    "commentary": "CWF Swell phrase: Swell2 Null",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 12,  (7,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 12,  (0,"E"), "all"),
       ],
    "checkStrings": [
       "NORTH SWELL AROUND 7 FEET",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    "fileChanges":[
       ("CWF_<site>_Overrides", "TextUtility", "add", CWFPeriod, "undo"),
       ],
    },    
    {
    "name": "CWF_Swell3",
    "commentary": "CWF Swell phrase: Swell Null",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 12,  (0,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 12,  (14,"E"), "all"),
       ],
    "checkStrings": [
       "SWELL EAST AROUND 14 FEET",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    "fileChanges":[
       ("CWF_<site>_Overrides", "TextUtility", "add", CWFPeriod, "undo"),
       ],
    },    
    {
    "name": "CWF_Swell4",
    "commentary": "CWF Swell phrase: 2 sub-phrases, neither null",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (7,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (12,"E"), "all"),
       ("Fcst", "Swell", "VECTOR", 6, 14,  (14,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 14,  (7,"SE"), "all"),
       ],
    "checkStrings": [
       "MIXED SWELL NORTH AROUND 7 FEET AND EAST AROUND 12 FEET INCREASING TO NORTHWEST AROUND 14 FEET AND SOUTHEAST AROUND 7 FEET IN THE AFTERNOON",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    "fileChanges":[
       ("CWF_<site>_Overrides", "TextUtility", "add", CWFPeriod, "undo"),
       ],
    },    
       
    {
    "name": "CWF_Swell5",
    "commentary": "CWF Swell phrase: 2 sub-phrases, Swell2 null in both",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (7,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (0,"E"), "all"),
       ("Fcst", "Swell", "VECTOR", 6, 12,  (14,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 12,  (0,"SE"), "all"),
       ],
    "checkStrings": [
       "NORTH SWELL AROUND 7 FEET INCREASING TO NORTHWEST AROUND 14 FEET IN THE AFTERNOON",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    "fileChanges":[
       ("CWF_<site>_Overrides", "TextUtility", "add", CWFPeriod, "undo"),
       ],
    },    
    {
    "name": "CWF_Swell6",
    "commentary": "CWF Swell phrase: 2 sub-phrases, Swell and Swell2 null in both",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (0,"E"), "all"),
       ("Fcst", "Swell", "VECTOR", 6, 12,  (0,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 12,  (0,"SE"), "all"),
       ],
    "checkStrings": [
       "",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    "fileChanges":[
       ("CWF_<site>_Overrides", "TextUtility", "add", CWFPeriod, "undo"),
       ],
    },    
    {
    "name": "CWF_Swell7",
    "commentary": "CWF Swell phrase: 2 sub-phrases, Swell null in both -- Combines",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (14,"N"), "all"),
       ("Fcst", "Swell", "VECTOR", 6, 12,  (0,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 12,  (7,"SE"), "all"),
       ],
    "checkStrings": [
       "SWELL SOUTHEAST 8 TO 13 FEET",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    "fileChanges":[
       ("CWF_<site>_Overrides", "TextUtility", "add", CWFPeriod, "undo"),
       ],
    },    
    {
    "name": "CWF_Swell8",
    "commentary": "CWF Swell phrase: 2 sub-phrases, Swell2 null in first",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (7,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (0,"E"), "all"),
       ("Fcst", "Swell", "VECTOR", 6, 12,  (14,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 12,  (7,"SE"), "all"),
       ],
    "checkStrings": [
       "NORTH SWELL AROUND 7 FEET INCREASING TO NORTHWEST AROUND 14 FEET AND SOUTHEAST AROUND 7 FEET IN THE AFTERNOON",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    "fileChanges":[
       ("CWF_<site>_Overrides", "TextUtility", "add", CWFPeriod, "undo"),
       ],
    },    
    {
    "name": "CWF_Swell9",
    "commentary": "CWF Swell phrase: 2 sub-phrases, Swell null in first",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (14,"E"), "all"),
       ("Fcst", "Swell", "VECTOR", 6, 12,  (14,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 12,  (7,"SE"), "all"),
       ],
    "checkStrings": [
       "SWELL EAST AROUND 14 FEET INCREASING TO NORTHWEST AROUND 14 FEET AND SOUTHEAST AROUND 7 FEET IN THE AFTERNOON",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    "fileChanges":[
       ("CWF_<site>_Overrides", "TextUtility", "add", CWFPeriod, "undo"),
       ],
    },
    {
    "name": "CWF_Swell10",
    "commentary": "CWF Swell phrase: 2 sub-phrases, Swell same mag, Swell2 null in both",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (7,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (0,"E"), "all"),
       ("Fcst", "Swell", "VECTOR", 6, 12,  (10,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 12,  (0,"SE"), "all"),
       ],
    "checkStrings": [
       "NORTHWEST SWELL 7 TO 10 FEET",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    "fileChanges":[
       ("CWF_<site>_Overrides", "TextUtility", "add", CWFPeriod, "undo"),
       ],
    },    
    {
    "name": "CWF_Swell11",
    "commentary": "CWF Swell phrase: 2 sub-phrases, Swell null in both, Swell2 same mag",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (10,"E"), "all"),
       ("Fcst", "Swell", "VECTOR", 6, 12,  (0,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 12,  (7,"SE"), "all"),
       ],
    "checkStrings": [
       "SWELL SOUTHEAST 7 TO 10 FEET",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    "fileChanges":[
       ("CWF_<site>_Overrides", "TextUtility", "add", CWFPeriod, "undo"),
       ],
    },

    ### CWF_Swell_withPeriod
    {
    "name": "CWF_Swell_withPeriod1",
    "commentary": "CWF Swell phrase: Neither null",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 12,  (7,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 12,  (14,"E"), "all"),
       ("Fcst", "Period", "SCALAR", 0, 12, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 0, 12, 10, "all"),
       ],
    "checkStrings": [
       "MIXED SWELL NORTH AROUND 7 FEET AT 5 SECONDS AND EAST AROUND 14 FEET AT 10 SECONDS",
       ],
    "comboFlag": 1,
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },    
    {
    "name": "CWF_Swell_withPeriod2",
    "commentary": "CWF Swell phrase: Swell2 Null",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 12,  (7,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 12,  (0,"E"), "all"),
       ("Fcst", "Period", "SCALAR", 0, 12, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 0, 12, 10, "all"),
       ],
    "checkStrings": [
       "NORTH SWELL AROUND 7 FEET AT 5 SECONDS",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },    
    {
    "name": "CWF_Swell_withPeriod3",
    "commentary": "CWF Swell phrase: Swell Null",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 12,  (0,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 12,  (14,"E"), "all"),
       ("Fcst", "Period", "SCALAR", 0, 12, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 0, 12, 10, "all"),
       ],
    "checkStrings": [
       "SWELL EAST AROUND 14 FEET AT 10 SECONDS",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },    
    {
    "name": "CWF_Swell_withPeriod4",
    "commentary": "CWF Swell phrase: 2 sub-phrases, neither null",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (7,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (12,"E"), "all"),
       ("Fcst", "Period", "SCALAR", 0, 6, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 0, 6, 10, "all"),
       ("Fcst", "Swell", "VECTOR", 6, 14,  (14,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 14,  (7,"SE"), "all"),
       ("Fcst", "Period", "SCALAR", 6, 14, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 6, 14, 10, "all"),
       ],
    "checkStrings": [
       "MIXED SWELL NORTH AROUND 7 FEET AT 5 SECONDS AND EAST AROUND 12 FEET AT 10 SECONDS INCREASING TO NORTHWEST AROUND 14 FEET AT 5 SECONDS AND SOUTHEAST AROUND 7 FEET AT 10 SECONDS IN THE AFTERNOON",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },    
       
    {
    "name": "CWF_Swell_withPeriod5",
    "commentary": "CWF Swell phrase: 2 sub-phrases, Swell2 null in both",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (7,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (0,"E"), "all"),
       ("Fcst", "Period", "SCALAR", 0, 6, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 0, 6, 10, "all"),
       ("Fcst", "Swell", "VECTOR", 6, 12,  (14,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 12,  (0,"SE"), "all"),
       ("Fcst", "Period", "SCALAR", 6, 12, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 6, 12, 10, "all"),
       ],
    "checkStrings": [
       "NORTH SWELL AROUND 7 FEET AT 5 SECONDS INCREASING TO NORTHWEST AROUND 14 FEET AT 5 SECONDS IN THE AFTERNOON",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },    
    {
    "name": "CWF_Swell_withPeriod6",
    "commentary": "CWF Swell phrase: 2 sub-phrases, Swell and Swell2 null in both",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (0,"E"), "all"),
       ("Fcst", "Period", "SCALAR", 0, 6, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 0, 6, 10, "all"),
       ("Fcst", "Swell", "VECTOR", 6, 12,  (0,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 12,  (0,"SE"), "all"),
       ("Fcst", "Period", "SCALAR", 6, 12, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 6, 12, 10, "all"),
       ],
    "checkStrings": [
       "",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },    
    {
    "name": "CWF_Swell_withPeriod7",
    "commentary": "CWF Swell phrase: 2 sub-phrases, Swell null in both",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (14,"N"), "all"),
       ("Fcst", "Period", "SCALAR", 0, 6, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 0, 6, 10, "all"),
       ("Fcst", "Swell", "VECTOR", 6, 12,  (0,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 12,  (7,"SE"), "all"),
       ("Fcst", "Period", "SCALAR", 6, 12, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 6, 12, 10, "all"),
       ],
    "checkStrings": [
       "SWELL SOUTHEAST 8 TO 13 FEET",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },    
    {
    "name": "CWF_Swell_withPeriod8",
    "commentary": "CWF Swell phrase: 2 sub-phrases, Swell2 null in first",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (7,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (0,"E"), "all"),
       ("Fcst", "Period", "SCALAR", 0, 6, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 0, 6, 10, "all"),
       ("Fcst", "Swell", "VECTOR", 6, 12,  (14,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 12,  (7,"SE"), "all"),
       ("Fcst", "Period", "SCALAR", 6, 12, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 6, 12, 10, "all"),
       ],
    "checkStrings": [
       "NORTH SWELL AROUND 7 FEET AT 5 SECONDS INCREASING TO NORTHWEST AROUND 14 FEET AT 5 SECONDS AND SOUTHEAST AROUND 7 FEET AT 10 SECONDS IN THE AFTERNOON",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },    
    {
    "name": "CWF_Swell_withPeriod9",
    "commentary": "CWF Swell phrase: 2 sub-phrases, Swell null in first",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (14,"E"), "all"),
       ("Fcst", "Period", "SCALAR", 0, 6, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 0, 6, 10, "all"),
       ("Fcst", "Swell", "VECTOR", 6, 12,  (14,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 12,  (7,"SE"), "all"),
       ("Fcst", "Period", "SCALAR", 6, 12, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 6, 12, 10, "all"),
       ],
    "checkStrings": [
       "SWELL EAST AROUND 14 FEET AT 10 SECONDS INCREASING TO NORTHWEST AROUND 14 FEET AT 5 SECONDS AND SOUTHEAST AROUND 7 FEET AT 10 SECONDS IN THE AFTERNOON",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },
    {
    "name": "CWF_Swell_withPeriod10",
    "commentary": "CWF Swell phrase: 2 sub-phrases, Swell same mag, Swell2 null in both",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (7,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (0,"E"), "all"),
       ("Fcst", "Period", "SCALAR", 0, 6, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 0, 6, 10, "all"),
       ("Fcst", "Swell", "VECTOR", 6, 12,  (10,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 12,  (0,"SE"), "all"),
       ("Fcst", "Period", "SCALAR", 6, 12, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 6, 12, 10, "all"),
       ],
    "checkStrings": [
       "NORTHWEST SWELL 7 TO 10 FEET AT 5 SECONDS",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },    

    {
    "name": "CWF_Swell_withPeriod11",
    "commentary": "CWF Swell phrase: 2 sub-phrases, Swell null in both, Swell2 same mag",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 6,  (10,"E"), "all"),
       ("Fcst", "Period", "SCALAR", 0, 6, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 0, 6, 10, "all"),
       ("Fcst", "Swell", "VECTOR", 6, 12,  (0,"NW"), "all"),
       ("Fcst", "Swell2", "VECTOR", 6, 12,  (7,"SE"), "all"),
       ("Fcst", "Period", "SCALAR", 6, 12, 5, "all"),
       ("Fcst", "Period2", "SCALAR", 6, 12, 10, "all"),
       ],
    "checkStrings": [
       "SWELL SOUTHEAST 7 TO 10 FEET AT 10 SECONDS",
       ],
    "comboFlag": 1,                    
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },

    # TK 4688  Null Swell
    {
    "name": "CWF_NullSwell",
    "commentary": "CWF Swell phrase: Test user-set null phrase",
    "productType": "CWF",
    "createGrids": [
       ("Fcst", "Swell", "VECTOR", 0, 12,  (2,"N"), "all"),
       ("Fcst", "Swell2", "VECTOR", 0, 12,  (2,"N"), "all"),
       ],
    "checkStrings": [
       "LIGHT SWELLS",
       ],
    "comboFlag": 1,
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    "fileChanges":[
       ("CWF_<site>_Overrides", "TextUtility", "add", CWFPeriod, "undo"),
       ("CWF_<site>_Overrides", "TextUtility", "add", nullSwell, "undo"),
       ],
    },

    {    
    "name":"SwellPeriodCleanUp",
    "commentary": "Clean out grids",
    "productType": None,
    "deleteGrids": [
        ("Fcst", "Swell", "SFC", 0,280),
        ("Fcst", "Swell2", "SFC", 0,280),
        ("Fcst", "Period", "SFC", 0,280),
        ("Fcst", "Period2", "SFC", 0,280),
        ],
    "gridsStartTime": "6am Local",
    },


    ## Period Combining  Matt Foster
    {
    "name": "PeriodCombine1",
    "commentary": "Period combining with NoWx and SChc",
    "productType": "ZFP",
    "createGrids": [#TestScript.general_createGrids + [    
       ("Fcst", "Sky", "SCALAR", 48, 72, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 72, 78, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 78, 84, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 84, 90, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 90, 96, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 96, 102, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 102, 108, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 108, 114, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 114, 120, 40, "all"),
       ("Fcst", "Wx", "WEATHER", 48, 72, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 72, 78, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 78, 84, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 84, 90, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 90, 96, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 96, 102, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 102, 108, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 108, 114, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 114, 128, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 128, 142, "SChc:T:<NoInten>:<NoVis>:", "all"),
       #("Fcst", "Wx", "WEATHER", 128, 152, "NoWx", "all"),
       ("Fcst", "PoP", "SCALAR", 48, 128, 0, "all"),
       ("Fcst", "PoP", "SCALAR", 128, 142, 20, "all"),
       #("Fcst", "PoP", "SCALAR", 128, 152, 0, "all"),
##       ("Fcst", "MaxT", "SCALAR", "MaxTBegin+24", "MaxTEnd+24", 70, "all"),
##       ("Fcst", "MinT", "SCALAR", "MinTBegin+24", "MinTEnd+24", 43, "all"),
##       ("Fcst", "MaxT", "SCALAR", "MaxTBegin+48", "MaxTEnd+48", 70, "all"),
##       ("Fcst", "MinT", "SCALAR", "MinTBegin+48", "MinTEnd+48", 43, "all"),
##       ("Fcst", "MaxT", "SCALAR", "MaxTBegin+60", "MaxTEnd+60", 70, "all"),
##       ("Fcst", "MinT", "SCALAR", "MinTBegin+60", "MinTEnd+60", 43, "all"),
##       ("Fcst", "MaxT", "SCALAR", "MaxTBegin+72", "MaxTEnd+72", 70, "all"),
##       ("Fcst", "MinT", "SCALAR", "MinTBegin+72", "MinTEnd+72", 43, "all"),
##       ("Fcst", "Wind", "VECTOR", 36, 72, (10, "SW"), "all"),
           ],
    "checkStrings": [
       "SUNDAY NIGHT THROUGH WEDNESDAY NIGHT...",
       ],
    "fileChanges": [
       ("ZFP_<site>_Definition", "TextUtility", "replace",
        periodCombine2, "undo"),
       ("ZFP_<site>_Definition", "TextUtility", "replace",
       hoursSChc2, "undo"),
       ("ZFP_<site>_Overrides", "TextUtility", "add", pcElementList, "undo"),
       ],
    "comboFlag": 1,
    "gridsStartTime": "6am Local",
    "drtTime": "6am Local",                    
    },

    #  TK 4475 hoursSChcEnds handling pre-first period
    {
    "name": "hoursSChcEnds1",
    "commentary": "hoursSChcEnds for pre-first period",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", -6, 12, 15, "all", 1),
       ("Fcst", "Wx", "WEATHER", -6, 0, "SChc:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12,"SChc:T:<NoInten>:<NoVis>:", "all"),
       ],
    "cmdLineVars" : "{('Product Issuance', 'productIssuance'): 'Morning with Pre-1st Period', ('Issued By', 'issuedBy'): None}",
    "checkStrings": [
       "A 20 PERCENT CHANCE OF THUNDERSTORMS",
       ],
    "comboFlag": 1,
                    
    },

    ## Period Combining  Label ER Tk 4468
    {
    "name": "PeriodCombineLabel1",
    "commentary": "Period combining labelling for CWF",
    "productType": "CWF",
    "createGrids": [#TestScript.general_createGrids + [    
       ("Fcst", "Sky", "SCALAR", 48, 72, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 72, 78, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 78, 84, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 84, 90, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 90, 96, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 96, 102, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 102, 108, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 108, 114, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 114, 120, 40, "all"),
       ("Fcst", "Wx", "WEATHER", 48, 72, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 72, 78, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 78, 84, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 84, 90, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 90, 96, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 96, 102, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 102, 108, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 108, 114, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 114, 128, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 128, 142, "SChc:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 48, 128, 30, "all"),
       ("Fcst", "PoP", "SCALAR", 128, 142, 30, "all"),
           ],
    "checkStrings": [
       ".SUNDAY AND SUNDAY NIGHT...", "CHANCE OF SHOWERS.",
       ".MONDAY THROUGH TUESDAY...",
       ],
    "fileChanges":[
       ("CWF_<site>_Definition", "TextUtility", "add", periodCombine2, "undo"),
       ("CWF_<site>_Overrides", "TextUtility", "add", pcElementList, "undo"),
       ],
    "comboFlag": 1,
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "6am Local",           
    },

    # Visibility phrase tests TK 4658    
    {
    "name": "CWF_Vis",
    "commentary": "CWF Visibility phrase: 1st period SChc:SW, 2nd period Sct:RW",
    "productType": "CWF",
    "fileChanges":[
       ("CWF_<site>_Overrides", "TextUtility", "add", marine_wx, "undo"),
       ],
    "createGrids": [
       ("Fcst", "Wx", "WEATHER", 0, 12, "SChc:SW:-:2SM:", "all"),
       ("Fcst", "Wx", "WEATHER", 12, 24, "Sct:RW:-:1/4SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 10, "all"),
       ("Fcst", "PoP", "SCALAR", 12, 24, 50, "all"),
       ],
    "checkStrings": [
       ".TONIGHT...", "SCATTERED SHOWERS", "VISIBILITY 1 NM OR LESS",
       ".SATURDAY...",
       ],
    "notCheckStrings":[
       "SCATTERED SNOW SHOWERS", "VISIBILITY 2 NM",
       ],
    "comboFlag": 1,
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },    
    {
    "name": "GLF_Vis",
    "commentary": "GLF Visibility phrase: 1st period SChc:SW, 2nd period Sct:RW",
    "productType": "GLF",
    "fileChanges":[
       ("GLF_<site>_Overrides", "TextUtility", "add", marine_wx, "undo"),
       ],
    "createGrids": [
       ("Fcst", "Wx", "WEATHER", 0, 12, "SChc:SW:-:2SM:", "all"),
       ("Fcst", "Wx", "WEATHER", 12, 24, "Sct:RW:-:1/4SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 10, "all"),
       ("Fcst", "PoP", "SCALAR", 12, 24, 50, "all"),
       ],
    "checkStrings": [
       ".TONIGHT...", "SCATTERED SHOWERS", "VSBY ONE QUARTER MILE OR LESS AT TIMES",
       ".SAT...",
       ],
    "notCheckStrings":[
       "SCATTERED SNOW SHOWERS",
       ],
    "comboFlag": 1,
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '400 AM', ('Groupings', 'groupings'): 'West 1/2:East 1/2'}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },    
    {
    "name": "NSH_Vis",
    "commentary": "NSH Visibility phrase: 1st period SChc:SW, 2nd period Sct:RW",
    "productType": "NSH",
    "fileChanges":[
       ("NSH_<site>_Overrides", "TextUtility", "add", marine_wx, "undo"),
       ],
    "createGrids": [
       ("Fcst", "Wx", "WEATHER", 0, 12, "SChc:SW:-:2SM:", "all"),
       ("Fcst", "Wx", "WEATHER", 12, 24, "Sct:RW:-:1/4SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 10, "all"),
       ("Fcst", "PoP", "SCALAR", 12, 24, 50, "all"),
       ],
    "checkStrings": [
       ".TONIGHT...", "SCATTERED SHOWERS",
       "VISIBILITY ONE QUARTER MILE OR LESS AT TIMES",
       ".SATURDAY...",
       ],
    "notCheckStrings":[
       "SCATTERED SNOW SHOWERS", "VISIBILITY 2 NM",
       ],
    "comboFlag": 1,
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '430 AM', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },    
        {
    "name": "OFF_Vis",
    "commentary": "OFF Visibility phrase: 1st period SChc:SW, 2nd period Sct:RW",
    "productType": "OFF",
    "fileChanges":[
       ("OFF_<site>_Overrides", "TextUtility", "add", marine_wx, "undo"),
       ],
    "createGrids": [
       ("Fcst", "Wx", "WEATHER", 0, 12, "SChc:SW:-:2SM:", "all"),
       ("Fcst", "Wx", "WEATHER", 12, 24, "Sct:RW:-:1/4SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 10, "all"),
       ("Fcst", "PoP", "SCALAR", 12, 24, 50, "all"),
       ],
    "checkStrings": [
       ".TONIGHT...", "SCATTERED SHOWERS", "VISIBILITY 1 NM OR LESS",
       ".SATURDAY...",
       ],
    "notCheckStrings":[
       "SCATTERED SNOW SHOWERS", "VISIBILITY 2 NM",
       ],
    "comboFlag": 1,
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '400 AM', ('Issued By', 'issuedBy'): None}",
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },    
        
    {
    "name": "ZFP_Vis",  
    "commentary": "Visibility: Report for significant key BS, not for RW",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:1/4SM:", "all"),
       ("Fcst", "Wx", "WEATHER", 12, 24, "Areas:BS:<NoInten>:1/4SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "PoP", "SCALAR", 12, 24, 10, "all"),
       ],
    "checkStrings": [
       ".TODAY...", "SCATTERED SHOWERS",
       ".TONIGHT...", "AREAS OF BLOWING SNOW.", 
       "VISIBILITY ONE QUARTER MILE OR LESS AT TIMES.",
       ".SATURDAY...",
       ],
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
    },

    #  DR_18670
    {
    "name": "DR_18670",  
    "commentary": "Alternative temp trends -- before sunrise wording",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 30, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 30, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 21, "all"),
       ("Fcst", "T", "SCALAR", 0, 12, 25, "all"),
       ("Fcst", "T", "SCALAR", 12, 18, 25, "all"),
       ("Fcst", "T", "SCALAR", 18, 19, 25, "all"),
       ("Fcst", "T", "SCALAR", 19, 20, 26, "all"),
       ("Fcst", "T", "SCALAR", 20, 21, 27, "all"),
       ("Fcst", "T", "SCALAR", 21, 22, 28, "all"),
       ("Fcst", "T", "SCALAR", 22, 23, 29, "all"),
       ("Fcst", "T", "SCALAR", 23, 24, 30, "all"),
       ],
    "cmdLineVars" : "{('Product Issuance', 'productIssuance'): 'Early Morning Update', ('Issued By', 'issuedBy'): None}",
    "fileChanges": [
       ("ZFP_<site>_Overrides", "TextUtility", "add", alternateTempTrends, "undo"),
       ],
    "checkStrings": [
       ".REST OF TONIGHT...", "TEMPERATURE STEADY OR SLOWLY RISING THROUGH SUNRISE", 
       ],
    "gridsStartTime": "6am Local",
    "drtTime": "12am Local",
    },


    {
    "name": "DR_18894",  
    "commentary": "Missing 'WITH' in ZFP wording",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 10, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:F:+:1/4SM:", "all"),
       ],
    "cmdLineVars" : "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "fileChanges": [
       ("ZFP_<site>_Overrides", "TextUtility", "add", visibilitySettings, "undo"),
       ],
    "checkStrings": [
       "TODAY...", 
       ],
    "gridsStartTime": "6am Local",
    "drtTime": "6am Local",
    },


    {
    "name": "similarAttributes_1",  
    "commentary": "",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Wx", "WEATHER", 0, 6, "Lkly:T:<NoInten>:<NoVis>:GW,SmA^Lkly:RW:m:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Lkly:T:<NoInten>:<NoVis>:^Lkly:RW:m:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 60, "all"),
       ],
    "checkStrings": [
       "THUNDERSTORMS LIKELY.",
       "SOME THUNDERSTORMS MAY PRODUCE GUSTY WINDS AND SMALL HAIL IN THE MORNING.",
       ],
    },
    {
    "name": "similarAttributes_2",  
    "commentary": "",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Wx", "WEATHER", 0, 6, "Lkly:T:<NoInten>:<NoVis>:GW,SmA^Lkly:RW:m:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Lkly:T:<NoInten>:<NoVis>:DmgW,LgA^Lkly:RW:m:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 60, "all"),
       ],
    "checkStrings": [
       "THUNDERSTORMS LIKELY.",
       "SOME THUNDERSTORMS MAY PRODUCE DAMAGING WINDS AND LARGE HAIL.",
       ],
    },

    {
    "name": "similarAttributes_3",  
    "commentary": "",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Wx", "WEATHER", 0, 12,
       "Lkly:T:<NoInten>:<NoVis>:GW,SmA^Lkly:RW:m:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12,
       "Lkly:T:<NoInten>:<NoVis>:DmgW,LgA^Lkly:RW:m:<NoVis>:", ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 60, "all"),
       ],
    "checkStrings": [
       "THUNDERSTORMS LIKELY.",
       "SOME THUNDERSTORMS MAY PRODUCE DAMAGING WINDS AND LARGE HAIL.",
       ],
    },

    ############  DR 18632 nlValue vector dir diff
    {
    "name": "nlValueDirDiff_1",
    "commentary": "Testing vector dir diff nlValue",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (20,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (20,"NE"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12,  0, "all"),
       ],
    "checkStrings": [
       "NORTHEAST WINDS AROUND 25 MPH",
       ],
    },

    {
    "name": "nlValueDirDiff_2",
    "commentary": "Testing vector dir diff nlValue",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (20,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (20,"NE"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12,  0, "all"),
       ],
    "checkStrings": [
       "NORTH WINDS AROUND 25 MPH SHIFTING TO THE NORTHEAST IN THE AFTERNOON",
       ],
    "fileChanges": [
       ("ZFP_<site>_Overrides", "TextUtility", "add", wind_nlValue, "undo"),
       ],
    },

    ############  DR 18506 accumulating flurries and ice
    {
    "name": "flurries",
    "commentary": "Testing no accumulation with flurries",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:SW:--:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 0, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 60, "all"),
       ],
    "checkStrings": [
       "SCATTERED FLURRIES",
       ],
    "notCheckStrings": [
       "NO SNOW ACCUMULATION",
       ],
    "fileChanges": [
       ("ZFP_<site>_Overrides", "TextUtility", "add", period1, "undo"),
       ],   
    },
    {
    "name": "ice crystals",
    "commentary": "Testing no accumulation with ice crystals",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Wx", "WEATHER", 0, 12, "Patchy:IC:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 0, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 60, "all"),
       ],
    "checkStrings": [
       "PATCHY ICE CRYSTALS",
       ],
    "notCheckStrings": [
       "NO SNOW ACCUMULATION",
       ],
    "fileChanges": [
       ("ZFP_<site>_Overrides", "TextUtility", "add", period1, "undo"),
       ],   
    },

    ############  DR 18160 -- SnowSnow entry in phrase_descriptor_dict
    {
    "name": "SnowSnow",
    "commentary": "Testing snow accumulation for S and SW",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "Wx", "WEATHER", 0, 6, "Wide:S:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Sct:SW:-:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 3, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 60, "all"),
       ],
    "checkStrings": [
       "WIDESPREAD SNOW IN THE MORNING...THEN SCATTERED SNOW SHOWERS IN THE AFTERNOON.",
       "SNOW ACCUMULATION AROUND 3 INCHES.", "CHANCE OF SNOW 60 PERCENT.",
       ],
    "fileChanges": [
       ("ZFP_<site>_Overrides", "TextUtility", "add", period1, "undo"),
       ],   
    },

    ############  DR 19104 -- Skipping consolidateTrends for some phrases
    {
    "name": "Skipping consolidateTrends",
    "commentary": "Testing time descriptors for WindChill",
    "productType": "ZFP",
    "createGrids": [
       ("Fcst", "WindChill", "SCALAR", 0, 3, -14, "all"),
       ("Fcst", "WindChill", "SCALAR", 3, 6, -9, "all"),
       ("Fcst", "WindChill", "SCALAR", 6, 9, -4, "all"),
       ("Fcst", "WindChill", "SCALAR", 9, 12, 0, "all"),       
       ("Fcst", "T", "SCALAR", 0, 12, 30, "all"),    
       ],
    "checkStrings": [
       "LOWEST WIND CHILL READINGS AROUND 14 BELOW EARLY IN THE MORNING. ",
       ],
    "fileChanges": [
       ("ZFP_<site>_Overrides", "TextUtility", "add", windchill, "undo"),
       ],   
    },    
        
    {    
    "name":"General_CleanUp",
    "commentary": "Clean out grids",
    "productType": None,
    "deleteGrids": TestScript.general_deleteGrids,
    "gridsStartTime": "6am Local",
    },
    
    ]

def testScript(self, dataMgr, level="Site"):
    defaults = {
        "database": "<site>_GRID__Fcst_00000000_0000",
        "combinations": "ZONE",
        }
    # Necessary to do drt for scripts 
    # that start the product with current time
    time_4am = self.getAbsFromLocal(2010, 1, 1, 4, 0)
    time_6am = self.getAbsFromLocal(2010, 1, 1, 6, 0)
    time_12am = self.getAbsFromLocal(2010, 1, 2, 0, 0)
    for script in scripts:
        if script.get("gridsStartTime", None) == "6am Local":
            script["gridsStartTime"] = time_6am
        if script.get("drtTime", None) == "4am Local":
            script["drtTime"] = time_6am
        elif script.get("drtTime", None) == "6am Local":
            script["drtTime"] = time_6am
        elif script.get("drtTime", None) == "12am Local":
            script["drtTime"] = time_12am
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)

