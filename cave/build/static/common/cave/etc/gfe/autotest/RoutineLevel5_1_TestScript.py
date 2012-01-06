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
# RoutineLevel5_1_TestScript    Snow Accum/PopWx modes
#
# Author:
# ----------------------------------------------------------------------------

# First run setupTextEA


popWx1 = """def matchToWxInfo_dict(self, tree, node):
    return {
        "PoP": (5, "Max", None),     # 50
        #"PoP": (5, "Mode", None),    # 30
        #"PoP": (5, "MaxMode", None), # 40
        #"PoP": (5, "AnalysisMethod", None),  # 40
        "LAL": (0, "Max", "Max"),
        }
"""
popWx2 = """def matchToWxInfo_dict(self, tree, node):
    return {
        #"PoP": (5, "Max", None),     # 50
        "PoP": (5, "Mode", None),    # 30
        #"PoP": (5, "MaxMode", None), # 40
        #"PoP": (5, "AnalysisMethod", None),  # 40
        "LAL": (0, "Max", "Max"),
        }
"""
popWx3 = """def matchToWxInfo_dict(self, tree, node):
    return {
        #"PoP": (5, "Max", None),     # 50
        #"PoP": (5, "Mode", None),    # 30
        "PoP": (5, "MaxMode", None), # 40
        #"PoP": (5, "AnalysisMethod", None),  # 40
        "LAL": (0, "Max", "Max"),
        }
"""
popWx4 = """def matchToWxInfo_dict(self, tree, node):
    return {
        #"PoP": (5, "Max", None),     # 50
        #"PoP": (5, "Mode", None),    # 30
        #"PoP": (5, "MaxMode", None), # 40
        "PoP": (5, "AnalysisMethod", None),  # 40
        "LAL": (0, "Max", "Max"),
        }
"""

popWx5 = """def matchToWxInfo_dict(self, tree, node):
    return {
            "PoP": (5, "MaxMode", None),     # 50
            #"PoP": (5, "Mode", None),    # 30
            #"PoP": (5, "MaxMode", None), # 40
            #"PoP": (5, "AnalysisMethod", None),  # 40
            "LAL": (0, "Max", "Max"),
            }
"""

aggCov1 = """("Wx", self.rankedWx, [3]),"""
aggCov2 = """def Period_1_version2(self):
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
                       ("PoP", self.binnedPercent, [3]),
                       ("Wx", self.rankedWx, [6]),
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

wtAggCov = """def aggregateCov_algorithm(self, parmHisto, timeRange, componentName):
    return self.getWeightedAggregateCov
"""
existWtAggCov = """def aggregateCov_algorithm(self, parmHisto, timeRange, componentName):
    return self.getExistingWeightedAggregateCov
"""
highestAggCov = """def aggregateCov_algorithm(self, parmHisto, timeRange, componentName):
    return self.getHighestWeightedAggregateCov
"""

checkPercentages = """def checkPercentages(self, parmHisto, timeRange, componentName, wxKey, keyRankDict):
    # If a wxKey does not pass the wxkey_coverage_percentage, this method will be called
    # to give another chance.
    # You can use the keyRankDict:
    #   subkey : (rank, percent coverage)
    # to allow the wxKey to pass based on other values in the grid.
    # For example:  If I have 10% RW 10% SW, neither RW or SW will be reported
    # Using the keyRankDict, I can allow them to pass when I learn 
    #  that 20% of my area is covered with precip.
    # Here's how this might be done:
    #
    precip = ["SW", "RW", "R", "S"]
    totalPrecip = 0
    if wxKey.wxType() in precip:
        for subkey in keyRankDict.keys():
            if subkey.wxType() in precip:
                rank, percent = keyRankDict[subkey]
                print "subkey, percent", subkey, percent
                totalPrecip += percent
    print "total", totalPrecip
    if totalPrecip > 15:
        return 1
    else:
        return 0        
    return 0
"""
noWxThreshold = """def noWx_percentage(self, parmHisto, timeRange, componentName):
    # If the raw rank (areal and temporal coverage) of NoWx exceeds this value,
    # NoWx will be reported (all other weather keys will be ignored).
    return 30    
"""

skyMod = """def sky_timeDescriptorModeration(self, tree, node):
    # If only two subphrases, turn off second time descriptor
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
        childList[0].set("timeDescFlag", 1)
        childList[1].set("timeDescFlag", 0)
    return self.DONE()
"""

# Runs Phrase_Test_Local for each test
scripts = [
    {
    "name": "SnowAccum1",
    "commentary": "Snow with no snow accumulation",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Lkly:S:-:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "SNOW LIKELY",
       "NO SNOW ACCUMULATION"
       ],
    },
    {
    "name": "SnowAccum2",
    "commentary": "Snow with 2 inch accumulation",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Lkly:S:-:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 2, "all"),
       ],
    "checkStrings": [
       "SNOW LIKELY",
       "SNOW ACCUMULATION AROUND 2 INCHES"
       ],
    },
    {
    "name": "SnowAccum3",
    "commentary": "Sleet with no accumulation",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Lkly:IP:-:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "SLEET LIKELY",
       "NO SLEET ACCUMULATION"
       ],
    },
    {
    "name": "SnowAccum4",
    "commentary": "Sleet with 2 inch accumulation",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Lkly:IP:-:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 2, "all"),
       ],
    "checkStrings": [
       "SLEET LIKELY",
       "SLEET ACCUMULATION AROUND 2 INCHES"
       ],
    },
    {
    "name": "SnowAccum5",
    "commentary": "Snow and Sleet with no accumulation",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Lkly:S:-:<NoVis>:^Lkly:IP:-:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "SNOW AND LIGHT SLEET LIKELY",
       "NO SNOW AND SLEET ACCUMULATION"
       ],
    },
    {
    "name": "SnowAccum6",
    "commentary": "Snow and Sleet with 2 inch accumulation",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Lkly:S:-:<NoVis>:^Lkly:IP:-:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 2, "all"),
       ],
    "checkStrings": [
       "SNOW AND LIGHT SLEET LIKELY",
       "SNOW AND SLEET ACCUMULATION AROUND 2 INCHES"
       ],
    },
    {
    "name": "SnowAccum7",
    "commentary": "Snow and Sleet, no accumluation, with ice crystals",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "Lkly:S:-:<NoVis>:^Lkly:IP:-:<NoVis>:^Areas:IC:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, .05, "all"),
       ],
    "checkStrings": [
       "SNOW AND LIGHT SLEET LIKELY",
       "AREAS OF ICE CRYSTALS",
       "LITTLE OR NO SNOW AND SLEET ACCUMULATION"
       ],
    },
    {
    "name": "SnowAccum8",
    "commentary": "Areas of blowing snow, snow amount 0.05 inches",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 0, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Areas:BS:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, .05, "all"),
       ],
    "checkStrings": ["AREAS OF BLOWING SNOW"],
    "notCheckStrings": ["NO SNOW ACCUMULATION"],
    },


    # Keep this one last so that the grids will be set up for testing the
    # different algorithm options.
    {
    "name": "PoPWx1",
    "commentary": "PopWx algorithm with max option",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  40, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, ["AboveElev"]),
       ("Fcst", "PoP", "SCALAR", 3, 12, 30, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Chc:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
    # Algorithm set different ways for PoP in the matchToWxInfo_dict
    # Set inside Phrase_Test_Local file.
       "50 PERCENT",   # Max  
       #"30 PERCENT",   # Mode
       #"40 PERCENT",   # MaxMode
       #"40 PERCENT",   # AnalysisMethod
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", popWx1, "undo"),
       ],
    },

    {
    "name": "PoPWx2",
    "commentary": "PopWx algorithm with mode option",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  40, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, ["AboveElev"]),
       ("Fcst", "PoP", "SCALAR", 3, 12, 30, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Chc:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
    # Algorithm set different ways for PoP in the matchToWxInfo_dict
    # Set inside Phrase_Test_Local file.
       #"50 PERCENT",   # Max  
       "30 PERCENT",   # Mode
       #"40 PERCENT",   # MaxMode
       #"40 PERCENT",   # AnalysisMethod
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", popWx2, "undo"),
       ],
    },

    {
    "name": "PoPWx3",
    "commentary": "PopWx algorithm with max mode option",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  40, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, ["AboveElev"]),
       ("Fcst", "PoP", "SCALAR", 3, 12, 30, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Chc:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
    # Algorithm set different ways for PoP in the matchToWxInfo_dict
    # Set inside Phrase_Test_Local file.
       #"50 PERCENT",   # Max  
       #"30 PERCENT",   # Mode
       "40 PERCENT",    # MaxMode
       #"40 PERCENT",   # AnalysisMethod
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", popWx3, "undo"),
       ],
    },

    {
    "name": "PoPWx4",
    "commentary": "PopWx algorithm with analysis method option",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  40, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, ["AboveElev"]),
       ("Fcst", "PoP", "SCALAR", 3, 12, 30, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Chc:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
    # Algorithm set different ways for PoP in the matchToWxInfo_dict
    # Set inside Phrase_Test_Local file.
       #"50 PERCENT",   # Max  
       #"30 PERCENT",   # Mode
       #"40 PERCENT",   # MaxMode
       "40 PERCENT",   # AnalysisMethod
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", popWx4, "undo"),
       ],
    },

    ##### PoP/Wx consistency  (Lyle)
    ## Problem solved by going to 3-hourly PoP analysis in later periods...
    ## These are "playground" tests -- can be modified or added to so we
    ## can try to repeat field problems.
    {
    "name": "PopWxConsistency1",
    "commentary": "Pop Wx consistency",
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 2, 20, "all", 1),
       ("Fcst", "PoP", "SCALAR", 2, 4, 30, "all", 1),
       ("Fcst", "PoP", "SCALAR", 4, 6, 40, "all", 1),
       ("Fcst", "PoP", "SCALAR", 4, 6, 40, "all", 1),
       ("Fcst", "PoP", "SCALAR", 4, 6, 40, "all", 1),
       ("Fcst", "PoP", "SCALAR", 6, 9, 45, "all", 1),
       ("Fcst", "PoP", "SCALAR", 9, 12, 55, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 2, "SChc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 2, 4, "SChc:RW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 2, 4, "Chc:RW:-:<NoVis>:",  ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 4, 9, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12,"Chc:RW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 9, 12,"Lkly:RW:-:<NoVis>:", ["BelowElev"]),
       ],
    "checkStrings": [
        "",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "replace", (hoursSChc1, hoursSChc2), "undo"),
       ],
    },
    {
    "name": "PopWxConsistency2",
    "commentary": "Pop Wx consistency",
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 2, 50, "all", 1),
       ("Fcst", "PoP", "SCALAR", 2, 4, 30, "all", 1),
       ("Fcst", "PoP", "SCALAR", 4, 6, 30, "all", 1),
       ("Fcst", "PoP", "SCALAR", 6, 9, 30, "all", 1),
       ("Fcst", "PoP", "SCALAR", 9, 12, 30, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 2, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 2, 4, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 4, 9, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12,"Chc:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
        "",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "replace", (hoursSChc1, hoursSChc2), "undo"),
       ("Phrase_Test_Local", "TextProduct", "replace",
       popWx5, "undo"),
       ],
    },
    {
    "name": "PopWxConsistency3",
    "commentary": """
           Pop Wx consistency -- Run in Period 2_3 (6-hour resolution)
           With baseline:
             MOSTLY CLOUDY WITH A 50 PERCENT CHANCE OF SHOWERS.
    
           With getWeightedAggregateCov, get:
             SHOWERS LIKELY IN THE MORNING...THEN CHANCE OF SHOWERS IN THE AFTERNOON.
             CHANCE OF PRECIPITATION 70 PERCENT.
          """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 3,  80, "all", 1),
       ("Fcst", "PoP", "SCALAR", 3, 12, 50, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 3, "Def:RW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 3, "Lkly:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 3, 12, "Chc:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
        "",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "replace", (hoursSChc1, hoursSChc2), "undo"),
       ],
    },

    {
    "name": "PopWxConsistency4",
    "commentary": """
           refinement for popType 
          """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12,  60, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Lkly:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
        "SHOWERS LIKELY AND ISOLATED THUNDERSTORMS",
        "CHANCE OF PRECIPITATION 60 PERCENT",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "replace", (hoursSChc1, hoursSChc2), "undo"),
       ],
    },

    {
    "name": "PopWxConsistency5",
    "commentary": """
           refinement for popType
          """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12,  60, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Lkly:R:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "SChc:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
        "RAIN LIKELY IN THE MORNING...THEN SLIGHT CHANCE OF SHOWERS IN THE AFTERNOON",
        "CHANCE OF PRECIPITATION 60 PERCENT",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "replace", (hoursSChc1, hoursSChc2), "undo"),
       ],
    },

    {
    "name": "PopWxConsistency6",
    "commentary": """
           refinement for popType
          """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12,  60, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Lkly:R:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "SChc:R:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
        "RAIN LIKELY IN THE MORNING...THEN SLIGHT CHANCE OF RAIN IN THE AFTERNOON",
        "CHANCE OF RAIN 60 PERCENT",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "replace", (hoursSChc1, hoursSChc2), "undo"),
       ],
    },
    
    {
    "name": "PopWxConsistency7",  # Lyle 1/12/06
    "commentary": """
        If you run this data in Period 3, you get different results
        because of the 6-hour resolution
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 1,  30, "all", 1),
       ("Fcst", "PoP", "SCALAR", 1, 2,  50, "all", 1),
       ("Fcst", "PoP", "SCALAR", 2, 3,  70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 3, 4,  90, "all", 1),
       ("Fcst", "PoP", "SCALAR", 4, 5,  100, "all", 1),
       ("Fcst", "PoP", "SCALAR", 5, 6,  90, "all", 1),
       ("Fcst", "PoP", "SCALAR", 6, 7,  80, "all", 1),
       ("Fcst", "PoP", "SCALAR", 7, 8,  70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 8, 9,  50, "all", 1),
       ("Fcst", "PoP", "SCALAR", 9, 10,  40, "all", 1),
       ("Fcst", "PoP", "SCALAR", 10, 11,  30, "all", 1),
       ("Fcst", "PoP", "SCALAR", 11, 12,  30, "all", 1),
       
       ("Fcst", "Wx", "WEATHER", 0, 1, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 1, 2, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 2, 3, "Lkly:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 4, "Def:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 4, 5, "Def:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 5, 6, "Def:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 7, "Def:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 7, 8, "Lkly:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 8, 9, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 10, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 10, 11, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 11, 12, "Chc:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
        "SHOWERS IN THE MORNING...THEN SHOWERS LIKELY IN THE AFTERNOON",
        "CHANCE OF SHOWERS NEAR 100 PERCENT",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", wtAggCov, "undo"),
       ],
    },

    #### NoWx Threshold 4551
    {
    "name": "NoWxThreshold1",
    "commentary": "NoWx Threshold",
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 6, 0, ["AboveElev"], 1),
       ("Fcst", "PoP", "SCALAR", 6, 12, 30, ["AboveElev"], 1),
       ("Fcst", "PoP", "SCALAR", 0, 6, 30, ["BelowElev"], 1),
       ("Fcst", "PoP", "SCALAR", 6, 12, 30, ["BelowElev"], 1),
       ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:",["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12,"Chc:RW:-:<NoVis>:", ["BelowElev"]),
       ],
    "checkStrings": [
        "MOSTLY CLOUDY",
        "A 30 PERCENT CHANCE OF SHOWERS IN THE AFTERNOON",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", noWxThreshold, "undo"),
       ],
    },    

    #### aggregateCov_algorithm  TK 4478

    {
    "name": "AggregateCov1",
    "commentary": """
          Hour 0-3  SChc RW  PoP 15
          Hour 3-6  Chc  RW  PoP 30
          Hour 6-12 NoWx
          Using getAggregateCov
          ("Wx", self.rankedWx, [6]) instead of ("Wx", self.rankedWx, [3])
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 3,  15, "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  30, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12,  0, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3, "SChc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", "all"),
       ],
    "checkStrings": [
       "A 30 PERCENT CHANCE OF SHOWERS IN THE MORNING",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "replace", aggCov2, "undo"),
       ],
    },

    {
    "name": "AggregateCov2",
    "commentary": """
          Hour 0-3  SChc RW  PoP 15
          Hour 3-6  Chc  RW  PoP 30
          Hour 6-12 NoWx
          Using getWeightedAggregateCov
          ("Wx", self.rankedWx, [6]) instead of ("Wx", self.rankedWx, [3])
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 3,  15, "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  30, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12,  0, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3, "SChc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", "all"),
       ],
    "checkStrings": [
       "A 30 PERCENT CHANCE OF SHOWERS IN THE MORNING",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "replace", aggCov2, "undo"),
       ("Phrase_Test_Local", "TextProduct", "add", wtAggCov, "undo"),
       ],
    },

    {
    "name": "AggregateCov3",
    "commentary": """
          Hour 0-3  SChc RW  PoP 15
          Hour 3-6  Chc  RW  PoP 30
          Hour 6-12 NoWx
          Using getAggregateCov
          ("Wx", self.rankedWx, [3])
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 3,  15, "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  30, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12,  0, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3, "SChc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", "all"),
       ],
    "checkStrings": [
       "SLIGHT CHANCE OF SHOWERS EARLY IN THE MORNING...THEN CHANCE OF SHOWERS LATE IN THE MORNING",
       "CHANCE OF SHOWERS 30 PERCENT",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "replace", (aggCov1, aggCov2), "undo"),
       #("Phrase_Test_Local", "TextProduct", "add", wtAggCov, "undo"),
       ],
    },
    {
    "name": "AggregateCov4",
    "commentary": """
          Hour 0-3  SChc RW  PoP 15
          Hour 3-6  Chc  RW  PoP 30
          Hour 6-12 NoWx
          Using getWeightedAggregateCov
          ("Wx", self.rankedWx, [3])
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 3,  15, "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  30, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12,  0, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3, "SChc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", "all"),
       ],
    "checkStrings": [
       "SLIGHT CHANCE OF SHOWERS EARLY IN THE MORNING...THEN CHANCE OF SHOWERS LATE IN THE MORNING",
       "CHANCE OF SHOWERS 30 PERCENT",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "replace", (aggCov1, aggCov2), "undo"),
       ("Phrase_Test_Local", "TextProduct", "add", wtAggCov, "undo"),
       ],
    },
    {
    "name": "AggregateCov5",
    "commentary": """
          Hour 0-3  SChc RW  PoP 15
          Hour 3-6  Chc  RW  PoP 30
          Hour 6-12 NoWx
          Using getWeightedAggregateCov
          ("Wx", self.rankedWx, [3])
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 3,  15, "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  30, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12,  0, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3, "SChc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", "all"),
       ],
    "checkStrings": [
       "SLIGHT CHANCE OF SHOWERS EARLY IN THE MORNING...THEN CHANCE OF SHOWERS LATE IN THE MORNING",
       "CHANCE OF SHOWERS 30 PERCENT",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "replace", (aggCov1, aggCov2), "undo"),
       ("Phrase_Test_Local", "TextProduct", "add", existWtAggCov, "undo"),
       ],
    },

    {
    "name": "AggregateCov6",
    "commentary": """
          40% of area: Iso RW
          25% of area: Sct RW
          25% of area: Num RW
          Using getAggregateCov
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12,  15, ["AboveElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  30,
          ["GMZ850", "GMZ870", "GMZ853", "GMZ830"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  50, 
          ["GMZ873", "GMZ876", "GMZ856"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:RW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:",
          ["GMZ850", "GMZ870", "GMZ853", "GMZ830"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:RW:-:<NoVis>:", 
          ["GMZ873", "GMZ876", "GMZ856"]),
       ],
    "checkStrings": [
       "ISOLATED SHOWERS",
       ],
    "fileChanges": [
       ],
    },

    {
    "name": "AggregateCov7",
    "commentary": """
          40% of area: Iso RW
          25% of area: Sct RW
          25% of area: Num RW
          Using getWeightedAggregateCov
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12,  15, ["AboveElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  30,
          ["GMZ850", "GMZ870", "GMZ853", "GMZ830"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  50, 
          ["GMZ873", "GMZ876", "GMZ856"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:RW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:",
          ["GMZ850", "GMZ870", "GMZ853", "GMZ830"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:RW:-:<NoVis>:", 
          ["GMZ873", "GMZ876", "GMZ856"]),
       ],
    "checkStrings": [
       "SCATTERED SHOWERS",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", wtAggCov, "undo"),
       ],
    },

    #### aggregateCov_algorithm  TK 4534
    {
    "name": "AggregateCov8",
    "commentary": """
          40% of area: Iso RW
          25% of area: Sct RW
          25% of area: Num RW
          Using getExistingAggregateCov
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12,  15, ["AboveElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  30,
          ["GMZ850", "GMZ870", "GMZ853", "GMZ830"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  50, 
          ["GMZ873", "GMZ876", "GMZ856"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:RW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:",
          ["GMZ850", "GMZ870", "GMZ853", "GMZ830"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:RW:-:<NoVis>:", 
          ["GMZ873", "GMZ876", "GMZ856"]),
       ],
    "checkStrings": [
       "NUMEROUS SHOWERS",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", existWtAggCov, "undo"),
       ],
    },
    
    {
    "name": "AggregateCov9",
    "commentary": """
          60% of area: Iso RW
          40% of area: Num RW
          Using getAggregateCov
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12,  15, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  30, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "ISOLATED SHOWERS",
       ],
    },

    {
    "name": "AggregateCov10",
    "commentary": """
          60% of area: Iso RW
          40% of area: Num RW
          Using getWeightedAggregateCov
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12,  15, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  30, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "SCATTERED SHOWERS",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", wtAggCov, "undo"),
       ],
    },

    {
    "name": "AggregateCov11",
    "commentary": """
          60% of area: Iso RW
          40% of area: Num RW
          Using getExistingWeightedAggregateCov
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12,  15, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  30, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "NUMEROUS SHOWERS",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", existWtAggCov, "undo"),
       ],
    },    

    ### TK 4578 getHighestWeightedAggregateCov
    {
    "name": "AggregateCov12",
    "commentary": """
          3% Num and 16% Sct
          Need to report Sct since Num does not meet threshold
          Using getHighestWeightedAggregateCov
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12,  15,
           ["FLZ042","FLZ043","FLZ048","FLZ049","FLZ051", "FLZ052"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  80, ["FLZ039"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:",
           ["FLZ042","FLZ043","FLZ048","FLZ049","FLZ051", "FLZ052"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:RW:-:<NoVis>:", ["FLZ039"]),
       ],
    "checkStrings": [
       "SCATTERED SHOWERS",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", highestAggCov, "undo"),
       ],
    },    
    {
    "name": "AggregateCov13",
    "commentary": """
          9% Num and 10% Sct
          Need to report Sct since neither meets threshold and Sct has
          highest coverage (even tho below threshold)
          Using getHighestWeightedAggregateCov
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12,  15,
           ["FLZ042","FLZ043","FLZ048","FLZ049","FLZ051"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  80, ["FLZ039", "FLZ052"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:",
           ["FLZ042","FLZ043","FLZ048","FLZ049","FLZ051"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:RW:-:<NoVis>:", ["FLZ039", "FLZ052"]), 
       ],
    "checkStrings": [
       "SCATTERED SHOWERS",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", highestAggCov, "undo"),
       ],
    },    
    {  # Ken Drozd wants strongest coverage for each wxType Tk 4555
    "name": "AggregateCov14",
    "commentary": "Pop Wx consistency",
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12,  80, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:RW:-:<NoVis>:^Num:SW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:^Num:SW:-:<NoVis>:", ["BelowElev"]),
       ],
    "checkStrings": [
        ("NUMEROUS SHOWERS AND SNOW SHOWERS.", "NUMEROUS RAIN SHOWERS AND SNOW SHOWERS."),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", highestAggCov, "undo"),
       ],
    },

    {
    "name": "AggregateCov15",
    "commentary": """
          Neither RW (8%) or SW (8%) will be reported normally.
          With "checkPercentages", they will be accepted.
          Using getHighestWeightedAggregateCov
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12,  20, ["FLZ039", "FLZ042", "FLZ051"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  50, ["FLZ049", "FLZ052"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:SW:-:<NoVis>:", ["FLZ039", "FLZ042", "FLZ051"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["FLZ049", "FLZ052"]),
       ],
    "checkStrings": [
       ("MOSTLY CLOUDY WITH SCATTERED SHOWERS AND SNOW SHOWERS",
       "MOSTLY CLOUDY WITH SCATTERED RAIN SHOWERS AND SNOW SHOWERS",
       ),
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "add", highestAggCov, "undo"),
       ("Phrase_Test_Local", "TextProduct", "add", checkPercentages, "undo"),
       ],
    },    

    {
    "name": "AggregateCov16",
    "commentary": """
          Neither RW (8%) or SW (8%) will be reported normally.
          With "checkPercentages", they will be accepted.
          Note, however, that this may not be the intention since
          they are mixed and thus cover only 8% area...
          The "checkPercentages" method could use refinement.
          Using getHighestWeightedAggregateCov
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12,  20, ["FLZ039", "FLZ042", "FLZ051"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  50, ["FLZ049", "FLZ052"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:SW:-:<NoVis>:^Sct:RW:-:<NoVis>:",
       ["FLZ039", "FLZ042", "FLZ051"]),
       #("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["FLZ049", "FLZ052"]),
       ],
    "checkStrings": [
       ("MOSTLY CLOUDY WITH SCATTERED SHOWERS AND SNOW SHOWERS",
       "MOSTLY CLOUDY WITH SCATTERED RAIN SHOWERS AND SNOW SHOWERS",
       ),
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "add", highestAggCov, "undo"),
       ("Phrase_Test_Local", "TextProduct", "add", checkPercentages, "undo"),
       ],
    },

    # Pop/Sky lower threshold TK 4663
    {
    "name": "PopSkyLowerThreshold1",
    "commentary": """
          Do not report sky if the majority of the period has < 60% PoP.
          In this case, we do want to report Sky since PoP is 20% for
          the majority of the period.          
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 3,  20, "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  20, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 9,  20, "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12,  100, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 9, "SChc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12, "Lkly:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "MOSTLY CLOUDY",
       ],
    },    
    {
    "name": "PopSkyLowerThreshold2",
    "commentary": """
          Do not report sky if the majority of the period has >= 60% PoP.
          In this case, we do not want to report Sky since PoP is >= 60% for
          the majority of the period.          
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 3,  20, "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  60, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 9,  60, "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12,  100, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3, "SChc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 12, "Lkly:RW:-:<NoVis>:", "all"),
       ],
    "notCheckStrings": [
       "MOSTLY CLOUDY",
       ],
    },    
    {
    "name": "PopSkyLowerThreshold3",
    "commentary": """
          Do not report sky if the majority of the period has >= 60% PoP.
          In this case, we do want to report Sky since PoP is not >= 60% for
          the majority of the period.          
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  50, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 12, 87, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 6,  40, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12,  80, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Lkly:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "MOSTLY SUNNY WITH CHANCE OF SHOWERS IN THE MORNING...THEN MOSTLY CLOUDY WITH SHOWERS LIKELY IN THE AFTERNOON",
       ],
    },    
    {
    "name": "PopSkyLowerThreshold4",
    "commentary": """                
          Do not report sky if the majority of the period has >= 60% PoP.
          In this case, we do not report sky since the majority of the period has > 60 PoP.
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 3,  20, "all", 1),
       ("Fcst", "Sky", "SCALAR", 3, 12, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 3,  20, "all"),
       ("Fcst", "PoP", "SCALAR", 3, 12, 80, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3, "Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 12, "Lkly:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "SUNNY WITH CHANCE OF SHOWERS EARLY IN THE MORNING...THEN SHOWERS LIKELY IN THE LATE MORNING AND AFTERNOON",
       ],
    },

    # TK 4675  Sky Time Descriptor moderation
    {
    "name": "SkyTimeModeration1",
    "commentary": """                
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 7, 90, "all", 1),
       ("Fcst", "Sky", "SCALAR", 7, 8, 90, "all", 1),
       ("Fcst", "Sky", "SCALAR", 8, 9, 90, "all", 1),
       ("Fcst", "Sky", "SCALAR", 9, 10, 50, "all", 1),
       ("Fcst", "Sky", "SCALAR", 10, 11, 50, "all", 1),
       ("Fcst", "Sky", "SCALAR", 11, 12, 50, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 6,  100, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 20,  "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Wide:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "SChc:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "MOSTLY SUNNY.",
       "WIDESPREAD SHOWERS IN THE MORNING...THEN SLIGHT CHANCE OF SHOWERS IN THE AFTERNOON", 
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", skyMod, "undo"),
       ],
    },
    {
    "name": "SkyTimeModeration2",
    "commentary": """                
    """,
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all", 1),
       ("Fcst", "Sky", "SCALAR", 6, 7, 90, "all", 1),
       ("Fcst", "Sky", "SCALAR", 7, 8, 90, "all", 1),
       ("Fcst", "Sky", "SCALAR", 8, 9, 90, "all", 1),
       ("Fcst", "Sky", "SCALAR", 9, 10, 50, "all", 1),
       ("Fcst", "Sky", "SCALAR", 10, 11, 50, "all", 1),
       ("Fcst", "Sky", "SCALAR", 11, 12, 50, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 6,  100, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 20,  "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Wide:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "SChc:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
        "BECOMING MOSTLY SUNNY LATE IN THE AFTERNOON.",
        "WIDESPREAD SHOWERS IN THE MORNING...THEN SLIGHT CHANCE OF SHOWERS IN THE AFTERNOON.",
       ],
    },
    ]

import TestScript
def testScript(self, dataMgr):
    defaults = {
        "cmdLineVars" :"{('Product Issuance', 'productIssuance'): 'Morning', ('Issuance Type', 'issuanceType'): 'ROUTINE', ('Issued By', 'issuedBy'): None}",
        "productType": "Phrase_Test_Local",
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)


