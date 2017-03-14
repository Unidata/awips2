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


popWx1 = """
    def matchToWxInfo_dict(self, tree, node):
        return {
            "PoP": (5, "Max", None),     # 50
            #"PoP": (5, "Mode", None),    # 30
            #"PoP": (5, "MaxMode", None), # 40
            #"PoP": (5, "AnalysisMethod", None),  # 40
            "LAL": (0, "Max", "Max"),
            }

"""
popWx2 = """
    def matchToWxInfo_dict(self, tree, node):
        return {
            #"PoP": (5, "Max", None),     # 50
            "PoP": (5, "Mode", None),    # 30
            #"PoP": (5, "MaxMode", None), # 40
            #"PoP": (5, "AnalysisMethod", None),  # 40
            "LAL": (0, "Max", "Max"),
            }

"""
popWx3 = """
    def matchToWxInfo_dict(self, tree, node):
        return {
            #"PoP": (5, "Max", None),     # 50
            #"PoP": (5, "Mode", None),    # 30
            "PoP": (5, "MaxMode", None), # 40
            #"PoP": (5, "AnalysisMethod", None),  # 40
            "LAL": (0, "Max", "Max"),
            }
"""
popWx4 = """
    def matchToWxInfo_dict(self, tree, node):
        return {
            #"PoP": (5, "Max", None),     # 50
            #"PoP": (5, "Mode", None),    # 30
            #"PoP": (5, "MaxMode", None), # 40
            "PoP": (5, "AnalysisMethod", None),  # 40
            "LAL": (0, "Max", "Max"),
            }

"""

aggCov1 = """("Wx", self.rankedWx, [3]),"""
aggCov2 = """("Wx", self.rankedWx, [6]),"""

wtAggCov = """

    def aggregateCov_algorithm(self, parmHisto, timeRange, componentName):
        return self.getWeightedAggregateCov

"""
existWtAggCov = """

    def aggregateCov_algorithm(self, parmHisto, timeRange, componentName):
        return self.getExistingWeightedAggregateCov

"""
highestAggCov = """

    def aggregateCov_algorithm(self, parmHisto, timeRange, componentName):
        return self.getHighestWeightedAggregateCov

"""

checkPercentages = """

    def checkPercentages(self, parmHisto, timeRange, componentName, wxKey, keyRankDict):
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
noWxThreshold = """

    def noWx_percentage(self, parmHisto, timeRange, componentName):
        # If the raw rank (areal and temporal coverage) of NoWx exceeds this value,
        # NoWx will be reported (all other weather keys will be ignored).
        return 30    

"""

skyMod = """

    def sky_timeDescriptorModeration(self, tree, node):
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
       "Snow likely",
       "No snow accumulation"
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
       "Snow likely",
       "Snow accumulation around 2 inches"
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
       "sleet likely",
       "No sleet accumulation"
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
       "sleet likely",
       "Sleet accumulation around 2 inches"
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
       "Snow and light sleet likely",
       "No snow and sleet accumulation"
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
       "Snow and light sleet likely",
       "Snow and sleet accumulation around 2 inches"
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
       "Snow and light sleet likely",
       "Areas of ice crystals",
       "Little or no snow and sleet accumulation"
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
    "checkStrings": ["Areas of blowing snow"],
    "notCheckStrings": ["No snow accumulation"],
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
       "50 percent",   # Max  
       #"30 percent",   # Mode
       #"40 percent",   # MaxMode
       #"40 percent",   # AnalysisMethod
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", popWx1, "undo"),
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
       #"50 percent",   # Max  
       "30 percent",   # Mode
       #"40 percent",   # MaxMode
       #"40 percent",   # AnalysisMethod
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", popWx2, "undo"),
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
       #"50 percent",   # Max  
       #"30 percent",   # Mode
       "40 percent",    # MaxMode
       #"40 percent",   # AnalysisMethod
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", popWx3, "undo"),
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
       #"50 percent",   # Max  
       #"30 percent",   # Mode
       #"40 percent",   # MaxMode
       "40 percent",   # AnalysisMethod
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", popWx4, "undo"),
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
       #("Phrase_Test_Local", "TextUtility", "replace", (hoursSChc1, hoursSChc2), "undo"),
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
       #("Phrase_Test_Local", "TextUtility", "replace", (hoursSChc1, hoursSChc2), "undo"),
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('"PoP": (5, "Max", None),', '"PoP": (5, "MaxMode", None),'), "undo"),
       ],
    },
    {
    "name": "PopWxConsistency3",
    "commentary": """
           Pop Wx consistency -- Run in Period 2_3 (6-hour resolution)
           With baseline:
             Mostly cloudy with a 50 percent chance of showers.
    
           With getWeightedAggregateCov, get:
             Showers likely in the morning, then chance of showers in the afternoon.
             Chance of precipitation 70 percent.
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
       #("Phrase_Test_Local", "TextUtility", "replace", (hoursSChc1, hoursSChc2), "undo"),
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
        "Showers likely and isolated thunderstorms",
        "Chance of precipitation 60 percent",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextUtility", "replace", (hoursSChc1, hoursSChc2), "undo"),
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
        "Rain likely in the morning, then slight chance of showers in the afternoon",
        "Chance of precipitation 60 percent",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextUtility", "replace", (hoursSChc1, hoursSChc2), "undo"),
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
        "Rain likely in the morning, then slight chance of rain in the afternoon",
        "Chance of rain 60 percent",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextUtility", "replace", (hoursSChc1, hoursSChc2), "undo"),
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
        "Showers in the morning, then showers likely in the afternoon",
        "Chance of showers near 100 percent",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", wtAggCov, "undo"),
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
        "Mostly cloudy",
        "A 30 percent chance of showers in the afternoon",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", noWxThreshold, "undo"),
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
       "A 30 percent chance of showers in the morning",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "replace", (aggCov1, aggCov2), "undo"),
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
       "A 30 percent chance of showers in the morning",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "replace", (aggCov1, aggCov2), "undo"),
       ("Phrase_Test_Local", "TextUtility", "add", wtAggCov, "undo"),
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
       "Slight chance of showers early in the morning, then chance of showers late in the morning",
       "Chance of showers 30 percent",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextUtility", "replace", (aggCov1, aggCov2), "undo"),
       #("Phrase_Test_Local", "TextUtility", "add", wtAggCov, "undo"),
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
       "Slight chance of showers early in the morning, then chance of showers late in the morning",
       "Chance of showers 30 percent",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextUtility", "replace", (aggCov1, aggCov2), "undo"),
       ("Phrase_Test_Local", "TextUtility", "add", wtAggCov, "undo"),
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
       "Slight chance of showers early in the morning, then chance of showers late in the morning",
       "Chance of showers 30 percent",
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextUtility", "replace", (aggCov1, aggCov2), "undo"),
       ("Phrase_Test_Local", "TextUtility", "add", existWtAggCov, "undo"),
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
       "isolated showers",
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
       "scattered showers",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", wtAggCov, "undo"),
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
       "numerous showers",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", existWtAggCov, "undo"),
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
       "isolated showers",
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
       "scattered showers",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", wtAggCov, "undo"),
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
       "numerous showers",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", existWtAggCov, "undo"),
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
           ["FLZ142", "FLZ242","FLZ043","FLZ148", "FLZ248","FLZ149", "FLZ249","FLZ151", "FLZ251", "FLZ052"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  80, ["FLZ139", "FLZ239"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:",
           ["FLZ142", "FLZ242","FLZ043","FLZ148", "FLZ248","FLZ149", "FLZ249","FLZ151", "FLZ251", "FLZ052"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:RW:-:<NoVis>:", ["FLZ139", "FLZ239"]),
       ],
    "checkStrings": [
       "scattered showers",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", highestAggCov, "undo"),
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
           ["FLZ142", "FLZ242","FLZ043","FLZ148", "FLZ248","FLZ149", "FLZ249","FLZ151", "FLZ251"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  80, ["FLZ139", "FLZ239", "FLZ052"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:",
           ["FLZ142", "FLZ242","FLZ043","FLZ148", "FLZ248","FLZ149", "FLZ249","FLZ151", "FLZ251"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:RW:-:<NoVis>:", ["FLZ139", "FLZ239", "FLZ052"]), 
       ],
    "checkStrings": [
       "scattered showers",
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", highestAggCov, "undo"),
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
        ("Numerous showers and snow showers.", "Numerous rain showers and snow showers."),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", highestAggCov, "undo"),
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
       ("Fcst", "PoP", "SCALAR", 0, 12,  20, ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ151", "FLZ251"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  50, ["FLZ149", "FLZ249", "FLZ052"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:SW:-:<NoVis>:", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ151", "FLZ251"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["FLZ149", "FLZ249", "FLZ052"]),
       ],
    "checkStrings": [
       ("Mostly cloudy with scattered showers and snow showers",
       "Mostly cloudy with scattered rain showers and snow showers",
       ),
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextUtility", "add", highestAggCov, "undo"),
       ("Phrase_Test_Local", "TextUtility", "add", checkPercentages, "undo"),
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
       ("Fcst", "PoP", "SCALAR", 0, 12,  20, ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ151", "FLZ251"]),
       ("Fcst", "PoP", "SCALAR", 0, 12,  50, ["FLZ149", "FLZ249", "FLZ052"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:SW:-:<NoVis>:^Sct:RW:-:<NoVis>:",
       ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ151", "FLZ251"]),
       #("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["FLZ149", "FLZ249", "FLZ052"]),
       ],
    "checkStrings": [
       ("Mostly cloudy with scattered showers and snow showers",
       "Mostly cloudy with scattered rain showers and snow showers",
       ),
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextUtility", "add", highestAggCov, "undo"),
       ("Phrase_Test_Local", "TextUtility", "add", checkPercentages, "undo"),
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
       "Mostly cloudy",
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
       "Mostly cloudy",
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
       "Mostly sunny with chance of showers in the morning, then mostly cloudy with showers likely in the afternoon",
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
       "Sunny with chance of showers early in the morning, then showers likely in the late morning and afternoon",
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
       "Mostly sunny.",
       "Widespread showers in the morning, then slight chance of showers in the afternoon", 
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", skyMod, "undo"),
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
        "Becoming mostly sunny late in the afternoon.",
        "Widespread showers in the morning, then slight chance of showers in the afternoon.",
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


