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
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# RoutineLevel5_4_TestScript    
# Author:
# ----------------------------------------------------------------------------

# Temp phrase
# Sky trends with new Sky categories

addPeriods = """

    def _10_503_issuance_list(self, argDict):
        seriesDefAM = [
            ("Period_1", "period1"), #("Phantom", 12),
            ("Period_2_3", 12), ("Period_2_3", 12),
            ("Period_4_5", 12), ("Period_4_5", 12),
            ("Period_6_14", 12), #("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12),
##            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12),
            ]
        seriesDefPM = [
            ("Period_1", "period1"),
            ("Period_2_3", 12), ("Period_2_3", 12),
            ("Period_4_5", 12), ("Period_4_5", 12),
            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12),
            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12),
            ("Period_6_14", 12),
            ]
 
        return [
            ("Morning", self.DAY(), self.NIGHT(), self.NIGHT(),
             ".TODAY...", "early in the morning", "late in the afternoon",
             1, seriesDefAM),
            ("Morning with Pre-1st Period", self.DAY()-2, self.NIGHT(), self.NIGHT(),
             ".TODAY...", "early in the morning", "late in the afternoon",
             1, seriesDefAM),
            ("Morning Update", "issuanceHour", self.NIGHT(), self.NIGHT(),
             ".REST OF TODAY...", "early in the morning", "late in the afternoon",
             1, seriesDefAM),
            ("Afternoon Update", "issuanceHour", self.NIGHT(), self.NIGHT(),
             ".REST OF TODAY...", "early in the morning","late in the afternoon",
             1, seriesDefAM),
            #  End times are tomorrow:
            ("Afternoon", self.NIGHT(), 24 + self.DAY(), 24 + self.DAY(),
             ".TONIGHT...", "late in the night", "early in the evening",
             1, seriesDefPM),
            ("Afternoon with Pre-1st Period", self.NIGHT()-2, 24 + self.DAY(), 24 + self.DAY(),
             ".TONIGHT...", "late in the night", "early in the evening",
             1, seriesDefPM),
            ("Evening Update", "issuanceHour", 24 + self.DAY(), 24 + self.DAY(),
             ".REST OF TONIGHT...", "early in the morning","early in the evening",
             1, seriesDefPM),
            # For the early morning update, this produces:
            # Rest of Tonight:
            # Monday
            # Monday Night
            ("Early Morning Update", "issuanceHour", self.DAY(), self.DAY(),
             ".REST OF TONIGHT...", "early in the morning","late in the afternoon",
             0, seriesDefPM),
            # Alternative
            # For the early morning update, this produces:
            # Early This Morning:
            # Today
            # Tonight
            #("Evening Update", "issuanceHour", 24 + self.DAY(), 24 + self.DAY(),
            # ".REST OF TONIGHT...", "late in the night", "early in the evening",
            # 1, seriesDefPM),
            #("Early Morning Update", "issuanceHour", self.DAY(), self.DAY(),
            # ".EARLY THIS MORNING...", "early in the morning", "late in the afternoon",
            # 1, seriesDefPM),
            ]


"""

addTonight = """
    def _10_503_issuance_list(self, argDict):
        seriesDefAM = [
            ("Period_1", "period1"), #("Phantom", 12),
            ("Period_2_3", 12), #("Period_2_3", 12),
##            ("Period_4_5", 12), ("Period_4_5", 12),
##            ("Period_6_14", 12), #("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12),
##            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12),
            ]
        seriesDefPM = [
            ("Period_1", "period1"),
            ("Period_2_3", 12), ("Period_2_3", 12),
            ("Period_4_5", 12), ("Period_4_5", 12),
            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12),
            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12),
            ("Period_6_14", 12),
            ]
 
        return [
            ("Morning", self.DAY(), self.NIGHT(), self.NIGHT(),
             ".TODAY...", "early in the morning", "late in the afternoon",
             1, seriesDefAM),
            ("Morning with Pre-1st Period", self.DAY()-2, self.NIGHT(), self.NIGHT(),
             ".TODAY...", "early in the morning", "late in the afternoon",
             1, seriesDefAM),
            ("Morning Update", "issuanceHour", self.NIGHT(), self.NIGHT(),
             ".REST OF TODAY...", "early in the morning", "late in the afternoon",
             1, seriesDefAM),
            ("Afternoon Update", "issuanceHour", self.NIGHT(), self.NIGHT(),
             ".REST OF TODAY...", "early in the morning","late in the afternoon",
             1, seriesDefAM),
            #  End times are tomorrow:
            ("Afternoon", self.NIGHT(), 24 + self.DAY(), 24 + self.DAY(),
             ".TONIGHT...", "late in the night", "early in the evening",
             1, seriesDefPM),
            ("Afternoon with Pre-1st Period", self.NIGHT()-2, 24 + self.DAY(), 24 + self.DAY(),
             ".TONIGHT...", "late in the night", "early in the evening",
             1, seriesDefPM),
            ("Evening Update", "issuanceHour", 24 + self.DAY(), 24 + self.DAY(),
             ".REST OF TONIGHT...", "early in the morning","early in the evening",
             1, seriesDefPM),
            # For the early morning update, this produces:
            # Rest of Tonight:
            # Monday
            # Monday Night
            ("Early Morning Update", "issuanceHour", self.DAY(), self.DAY(),
             ".REST OF TONIGHT...", "early in the morning","late in the afternoon",
             0, seriesDefPM),
            # Alternative
            # For the early morning update, this produces:
            # Early This Morning:
            # Today
            # Tonight
            #("Evening Update", "issuanceHour", 24 + self.DAY(), 24 + self.DAY(),
            # ".REST OF TONIGHT...", "late in the night", "early in the evening",
            # 1, seriesDefPM),
            #("Early Morning Update", "issuanceHour", self.DAY(), self.DAY(),
            # ".EARLY THIS MORNING...", "early in the morning", "late in the afternoon",
            # 1, seriesDefPM),
            ]

    def Period_2_3(self):
        # No Lake Wind phrase
        component = {
            "type": "component",
            "methodList": [
                          self.orderPhrases,
                          self.consolidateSubPhrases,
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
                       ("PoP", self._PoP_analysisMethod("Period_2_3"), [6]),
                       ("PoP", self.binnedPercent, [6]),
                       ("SnowAmt", self.accumMinMax),
                       ("StormTotalSnow", self.accumMinMax),
                       ("IceAccum", self.accumMinMax),
                       ("SnowLevel", self.avg),
                       ("Wind", self.vectorMedianRange, [6]),
                       ("Wind", self.vectorMinMax, [6]),
                       ("WindGust", self.maximum, [6]),
                       ("Wx", self.rankedWx, [6]),
                       ("WindChill", self.minMax, [6]),
                       ("HeatIndex", self.minMax, [6]),
                      ],
            "phraseList":[
                   self.wind_summary,
                   self.reportTrends,
                   self.sky_phrase,
                   self.skyPopWx_phrase,
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
                   self.steady_temp_trends,
                   self.temp_trends,
                   self.wind_withGusts_phrase,
#                   self.lake_wind_phrase,
                   self.popMax_phrase,
                   self.windChill_phrase,
                   self.heatIndex_phrase,
                  ],
            }
        if self._arealSkyAnalysis:
            component["analysisList"].append(("Sky", self.binnedPercent, [6]))
        if self._useStormTotalSnow:
            phraseList = component["phraseList"]
            index = phraseList.index(self.total_snow_phrase)
            phraseList[index] = self.stormTotalSnow_phrase
            component["phraseList"] = phraseList
        return component

"""

import TestScript

scripts = [

    ### Temp phrases
    {
    "name": "Temp_50",
    "commentary": """
    MaxT -- 50    
    Steady temperatures.
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 50, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 50, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 50, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 50, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 50, ["BelowElev"]),
       ],
    "checkStrings": ["Near steady temperature around 50"],
    },

    {
    "name": "Temp_38-42",
    "commentary": """
    MaxT -- 38-42    
    Implied range terminology. NEAR and AROUND are defined as plus or
    minus two degrees about a certain number.
    For example, Lows AROUND 40 means 38 to 42 inclusive.
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 38, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 42, ["BelowElev"]),
       ],
    "checkStrings": ["Highs around 40"],
    },
    
    {
    "name": "Temp_50-53",
    "commentary": """
    MaxT -- 50-53    
    LOWER 50S (50, 51, 52, 53)
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 50, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 53, ["BelowElev"]),
       ],
    "checkStrings": ["Highs in the lower 50s"],
    },
    

    {
    "name": "Temp_54-56",
    "commentary": """
    MaxT -- 54-56    
    MID 50S (54, 55, 56)
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 54, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 56, ["BelowElev"]),
       ],
    "checkStrings": ["Highs in the mid 50s"],
    },
    

    {
    "name": "Temp_57-59",
    "commentary": """
    MaxT -- 57-59    
    UPPER 50S (57, 58, 59)
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 57, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 59, ["BelowElev"]),
       ],
    "checkStrings": ["Highs in the upper 50s"],
    },
    

    {
    "name": "Temp_47-52",
    "commentary": """
    MaxT -- 47-52    
    A specific range of five degrees (5 to 10 degrees in mountainous areas)
    Lows 20 to 25
    Highs 47 to 52
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 47, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 52, ["BelowElev"]),
       ],
    "checkStrings": ["Highs 47 to 52"],
    },
    
    {
    "name": "Temp_25-30",
    "commentary": """
    MaxT -- 25-30    
    A specific range of five degrees (5 to 10 degrees in mountainous areas)
    Lows 20 to 25
    Highs 47 to 52
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 25, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 30, ["BelowElev"]),
       ],
    "checkStrings": ["Highs 25 to 30"],
    },
    
    {
    "name": "Temp_103-108",
    "commentary": """
    MaxT -- 103 - 108    
    A specific range of five degrees (5 to 10 degrees in mountainous areas)
    Lows 20 to 25
    Highs 47 to 52
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 103, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 108, ["BelowElev"]),
       ],
    "checkStrings": ["Highs 103 to 108"],
    },
    
    {
    "name": "Temp_100-105",
    "commentary": """
    MaxT -- 100 - 105    
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 100, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 105, ["BelowElev"]),
       ],
    "checkStrings": ["Highs 100 to 105"],
    },
    
    {
    "name": "Temp_98-102",
    "commentary": """
    MaxT -- 98 - 102    
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 98, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 102, ["BelowElev"]),
       ],
    "checkStrings": ["Highs around 100"],
    },
    
    {
    "name": "Temp_98-103",
    "commentary": """
    MaxT -- 98 - 103    
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 98, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 103, ["BelowElev"]),
       ],
    "checkStrings": ["Highs 98 to 103"],
    },

    # Crossing LOWER, MID, UPPER boundaries
    {
    "name": "Temp_50-54",
    "commentary": """
    MaxT -- 50-54    
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 50, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 54, ["BelowElev"]),
       ],
    "checkStrings": ["Highs in the lower 50s"],
    },
    {
    "name": "Temp_53-54",
    "commentary": """
    MaxT -- 53-54    
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 53, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 54, ["BelowElev"]),
       ],
    "checkStrings": ["Highs in the mid 50s"],
    },
    {
    "name": "Temp_54-57",
    "commentary": """
    MaxT -- 54-57    
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 54, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 57, ["BelowElev"]),
       ],
    "checkStrings": ["Highs in the mid 50s"],
    },
    {
    "name": "Temp_56-58",
    "commentary": """
    MaxT -- 56-58
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 56, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 58, ["BelowElev"]),
       ],
    "checkStrings": ["Highs in the upper 50s"],
    },
   
    {
    "name": "Temp_-2-2",
    "commentary": """
    MaxT -- -2 - 2    
    Implied range terminology. NEAR zero is also permitted.
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", -2, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 2, ["BelowElev"]),
       ],
    "checkStrings": ["Highs near zero"],
    },

    {
    "name": "Temp_2-3",
    "commentary": """
    MaxT -- 2-3   
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 2, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 3, ["BelowElev"]),
       ],
    "checkStrings": ["Highs around 3"],
    },
    {
    "name": "Temp_4-6",
    "commentary": """
    MaxT -- 4-6   
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 4, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 6, ["BelowElev"]),
       ],
    "checkStrings": ["Highs around 5"],
    },
    {
    "name": "Temp_6-9",
    "commentary": """
    MaxT -- 6-9   
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 6, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 9, ["BelowElev"]),
       ],
    "checkStrings": ["Highs around 8"],
    },

    {
    "name": "Temp_12-14",
    "commentary": """
    MaxT -- 12-14   
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 12, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 14, ["BelowElev"]),
       ],
    "checkStrings": ["Highs around 13"],
    },
    
    {
    "name": "Temp_10-14",
    "commentary": """
    MaxT -- 10-14   
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 10, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 14, ["BelowElev"]),
       ],
    "checkStrings": ["Highs around 12"],
    },

    {
    "name": "Temp_11-14",
    "commentary": """
    MaxT -- 11-14   
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 11, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 14, ["BelowElev"]),
       ],
    "checkStrings": ["Highs around 13"],
    },

    {
    "name": "Temp_11-16",
    "commentary": """
    MaxT -- 11-16   
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 11, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 16, ["BelowElev"]),
       ],
    "checkStrings": ["Highs 11 to 16"],
    },

    {
    "name": "Temp_-5-5",
    "commentary": """
    MaxT -- -5 - 5    
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", -5, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 5, ["BelowElev"]),
       ],
    "checkStrings": ["Highs 5 below to 5 above zero"],
    },
    
    {
    "name": "Temp_-5--10",
    "commentary": """
    MaxT -- -5 - -10    
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", -5, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", -10, ["BelowElev"]),
       ],
    "checkStrings": ["Highs 5 below to 10 below zero"],
    },

    {
    
    "name": "Temp_0-5",
    "commentary": """
    MaxT -- 0 - 5     
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 5, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 0, ["BelowElev"]),
       ],
    "checkStrings": ["Highs zero to 5 above"],
    },

    {
    
    "name": "Temp_0--5",
    "commentary": """
    MaxT -- 0 - -5     
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", -5, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 0, ["BelowElev"]),
       ],
    "checkStrings": ["Highs 5 below to zero"],
    },
    
    {    
    "name": "Temp_Trends1",
    "commentary": """
    Trends    
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 70, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 60, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 48, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 70, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 70, ["BelowElev"]),
       ],
    "checkStrings": ["Highs around 70", "Temperatures falling into the upper 40s in the afternoon"],
    },
    {    
    "name": "Temp_Trends2",
    "commentary": """
    Trends    
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "T", "SCALAR", 0, 3, 70, "all"),
       ("Fcst", "T", "SCALAR", 3, 6, 70, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 70, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 70, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 70, ["BelowElev"]),
       ],
    "checkStrings": ["Near steady temperature around 70"],
    "notCheckString": ["Highs"],
    },

    # Sky trend tests
    {    
    "name": "SkyTrends1",
    "commentary": """
    Trends  sunny --> sunny (5)
            clear --> clear
            sunny --> sunny (25)
            clear --> mostly clear
            sunny --> mostly sunny (50)
            clear --> partly cloudy    
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 3, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 9, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 5, "all"),
       
       ("Fcst", "Sky", "SCALAR", 12, 15, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 15, 18, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 18, 21, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 21, 24, 5, "all"),
       
       ("Fcst", "Sky", "SCALAR", 24, 27, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 27, 30, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 30, 33, 25, "all"),
       ("Fcst", "Sky", "SCALAR", 33, 36, 25, "all"),
       
       ("Fcst", "Sky", "SCALAR", 36, 39, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 39, 42, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 42, 45, 25, "all"),
       ("Fcst", "Sky", "SCALAR", 45, 48, 25, "all"),
       
       ("Fcst", "Sky", "SCALAR", 48, 51, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 51, 54, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 54, 57, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 57, 60, 50, "all"),
       
       ("Fcst", "Sky", "SCALAR", 60, 63, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 63, 66, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 66, 69, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 69, 72, 50, "all"),

       ("Fcst", "PoP", "SCALAR", 0, 72, 0, "all"),
       ],
    "checkStrings": [
        ".TODAY...", "Sunny.",
        ".TONIGHT...", "Clear.",
        "...", "Sunny.",
        "...", "Mostly clear.",
        "...", "Mostly sunny.",
        "...", "Clear in the evening then becoming partly cloudy.",
        ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", addPeriods, "undo"),
       ],
    },
    
    {    
    "name": "SkyTrends2",
    "commentary": """
    Trends  
            sunny --> party sunny (69)
            clear --> mostly cloudy
            sunny --> mostly cloudy (87)
            clear --> mostly cloudy
            sunny --> cloudy  (100)
            clear --> cloudy
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 3, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 9, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 69, "all"),
       
       ("Fcst", "Sky", "SCALAR", 12, 15, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 15, 18, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 18, 21, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 21, 24, 69, "all"),
       
       ("Fcst", "Sky", "SCALAR", 24, 27, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 27, 30, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 30, 33, 87, "all"),
       ("Fcst", "Sky", "SCALAR", 33, 36, 87, "all"),
       
       ("Fcst", "Sky", "SCALAR", 36, 39, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 39, 42, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 42, 45, 87, "all"),
       ("Fcst", "Sky", "SCALAR", 45, 48, 87, "all"),
       
       ("Fcst", "Sky", "SCALAR", 48, 51, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 51, 54, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 54, 57, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 57, 60, 100, "all"),
       
       ("Fcst", "Sky", "SCALAR", 60, 63, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 63, 66, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 66, 69, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 69, 72, 100, "all"),

       ("Fcst", "PoP", "SCALAR", 0, 72, 0, "all"),

       ],
    "checkStrings": [
        ".TODAY...", "Sunny in the morning then becoming partly sunny",
        ".TONIGHT...", "Clear in the evening then becoming mostly cloudy",
        "...", "Sunny in the morning then becoming mostly cloudy",
        "...", "Clear in the evening then becoming mostly cloudy",
        "...", "Sunny in the morning then becoming cloudy",
        "...", "Clear in the evening then becoming cloudy",
        ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", addPeriods, "undo"),
       ],
    },

    {    
    "name": "SkyTrends3",
    "commentary": """
    Trends  day/night periods:
    
            sunny (25) --> mostly sunny (50)
            mostly clear --> partly cloudy
            sunny (25) --> partly sunny (69)
            mostly clear --> mostly cloudy
            sunny (25) --> mostly cloudy (87)
            mostly clear --> mostly cloudy
            sunny (25) --> cloudy (100)
            mostly clear --> cloudy
    
    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 3, 25, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6, 25, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 9, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 50, "all"),
       
       ("Fcst", "Sky", "SCALAR", 12, 15, 25, "all"),
       ("Fcst", "Sky", "SCALAR", 15, 18, 25, "all"),
       ("Fcst", "Sky", "SCALAR", 18, 21, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 21, 24, 50, "all"),
       
       ("Fcst", "Sky", "SCALAR", 24, 27, 25, "all"),
       ("Fcst", "Sky", "SCALAR", 27, 30, 25, "all"),
       ("Fcst", "Sky", "SCALAR", 30, 33, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 33, 36, 69, "all"),
       
       ("Fcst", "Sky", "SCALAR", 36, 39, 25, "all"),
       ("Fcst", "Sky", "SCALAR", 39, 42, 25, "all"),
       ("Fcst", "Sky", "SCALAR", 42, 45, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 45, 48, 69, "all"),
       
       ("Fcst", "Sky", "SCALAR", 48, 51, 25, "all"),
       ("Fcst", "Sky", "SCALAR", 51, 54, 25, "all"),
       ("Fcst", "Sky", "SCALAR", 54, 57, 87, "all"),
       ("Fcst", "Sky", "SCALAR", 57, 60, 87, "all"),
       
       ("Fcst", "Sky", "SCALAR", 60, 63, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 63, 66, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 66, 69, 87, "all"),
       ("Fcst", "Sky", "SCALAR", 69, 72, 87, "all"),

       ("Fcst", "Sky", "SCALAR", 72, 75, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 75, 78, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 78, 81, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 81, 84, 100, "all"),
       
       ("Fcst", "Sky", "SCALAR", 84, 87, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 87, 90, 5, "all"),
       ("Fcst", "Sky", "SCALAR", 90, 93, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 93, 96, 100, "all"),
       
       ("Fcst", "PoP", "SCALAR", 0, 96, 0, "all"),
       
       ],
    "checkStrings": [
        ".TODAY...", "Mostly sunny",
        ".TONIGHT...", "Partly cloudy",
        "...", "Sunny in the morning then becoming partly sunny",
        "...", "Mostly clear in the evening then becoming mostly cloudy",
        "...", "Sunny in the morning then becoming mostly cloudy",
        "...", "Clear in the evening then becoming mostly cloudy",
        ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", addPeriods, "undo"),
       ],
    },
    
    {    
    "name": "SkyTrends4",
    "commentary": """
    Trends      
            mostly sunny (50) --> partly sunny (69)
            partly cloudy --> mostly cloudy
            mostly sunny (50) --> mostly cloudy (87)
            partly cloudy --> mostly cloudy            
            mostly sunny (50) --> cloudy (100)
            partly cloudy --> cloudy

    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 3, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 9, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 69, "all"),
       
       ("Fcst", "Sky", "SCALAR", 12, 15, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 15, 18, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 18, 21, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 21, 24, 69, "all"),
       
       ("Fcst", "Sky", "SCALAR", 24, 27, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 27, 30, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 30, 33, 87, "all"),
       ("Fcst", "Sky", "SCALAR", 33, 36, 87, "all"),
       
       ("Fcst", "Sky", "SCALAR", 36, 39, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 39, 42, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 42, 45, 87, "all"),
       ("Fcst", "Sky", "SCALAR", 45, 48, 87, "all"),
       
       ("Fcst", "Sky", "SCALAR", 48, 51, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 51, 54, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 54, 57, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 57, 60, 100, "all"),
       
       ("Fcst", "Sky", "SCALAR", 60, 63, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 63, 66, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 66, 69, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 69, 72, 100, "all"),

       ("Fcst", "PoP", "SCALAR", 0, 72, 0, "all"),

       ],
    "checkStrings": [
        ".TODAY...", "Partly sunny.",
        ".TONIGHT...", "Partly cloudy in the evening then becoming mostly cloudy",
        "...", "Mostly sunny in the morning then becoming mostly cloudy",
        "...", "Partly cloudy in the evening then becoming mostly cloudy",
        "...", "Mostly sunny in the morning then becoming cloudy",
        "...", "Partly cloudy in the evening then becoming cloudy",
        ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", addPeriods, "undo"),
       ],
    },

    {    
    "name": "SkyTrends5",
    "commentary": """
    Trends         
            partly sunny (69) --> mostly cloudy (87)
            mostly cloudy --> cloudy            
            partly sunny (69)--> cloudy (100)
            mostly cloudy --> cloudy

            mostly cloudy (87) --> cloudy (100)
            mostly cloudy --> cloudy
    

    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 3, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 9, 87, "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 87, "all"),
       
       ("Fcst", "Sky", "SCALAR", 12, 15, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 15, 18, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 18, 21, 87, "all"),
       ("Fcst", "Sky", "SCALAR", 21, 24, 87, "all"),
       
       ("Fcst", "Sky", "SCALAR", 24, 27, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 27, 30, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 30, 33, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 33, 36, 100, "all"),
       
       ("Fcst", "Sky", "SCALAR", 36, 39, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 39, 42, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 42, 45, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 45, 48, 100, "all"),
       
       ("Fcst", "Sky", "SCALAR", 48, 51, 87, "all"),
       ("Fcst", "Sky", "SCALAR", 51, 54, 87, "all"),
       ("Fcst", "Sky", "SCALAR", 54, 57, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 57, 60, 100, "all"),
       
       ("Fcst", "Sky", "SCALAR", 60, 63, 87, "all"),
       ("Fcst", "Sky", "SCALAR", 63, 66, 87, "all"),
       ("Fcst", "Sky", "SCALAR", 66, 69, 100, "all"),
       ("Fcst", "Sky", "SCALAR", 69, 72, 100, "all"),

       ("Fcst", "PoP", "SCALAR", 0, 72, 0, "all"),

       ],
    "checkStrings": [
        ".TODAY...", "Mostly cloudy",
        ".TONIGHT...", "Mostly cloudy.",
        "...", "Partly sunny in the morning then becoming cloudy",
        "...", "Mostly cloudy",
        "...", "Cloudy.",
        "...", "Cloudy.",
        ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", addPeriods, "undo"),
       ],
    },
    {    
    "name": "SkyTrends6",
    "commentary": """
    Trends         
       Day: sunny --> mostly sunny --> partly sunny --> mostly cloudy
     Night: mostly clear --> partly cloudy --> mostly cloudy --> mostly cloudy

    """,
    "productType": "Phrase_Test_Local",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 3, 25, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 9, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 87, "all"),
       
       ("Fcst", "Sky", "SCALAR", 12, 15, 25, "all"),
       ("Fcst", "Sky", "SCALAR", 15, 18, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 18, 21, 69, "all"),
       ("Fcst", "Sky", "SCALAR", 21, 24, 87, "all"),      

       ("Fcst", "PoP", "SCALAR", 0, 72, 0, "all"),

       ],
    "checkStrings": [
        ".TODAY...", "Mostly sunny until late afternoon then becoming mostly cloudy",
        ".TONIGHT...", "Partly cloudy in the evening then becoming mostly cloudy", 
        ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextUtility", "add", addTonight, "undo"),
       ],
    },

    ]

maxT1 = """("MaxT", self.stdDevMinMax)"""
maxT2 = """("MaxT", self.minMax)"""

def testScript(self, dataMgr, level="Site"):
    defaults = {
        "database": "<site>_GRID__Fcst_00000000_0000",
        "combinations": "ZONE",
        "fileChanges": [
           ("Phrase_Test_Local", "TextUtility", "replace", (maxT1, maxT2), "undo"),
        ],
        }
    # Necessary to do drt for scripts 
    # that start the product with current time
    time_4am = self.getAbsFromLocal(2010, 1, 1, 4, 0)
    time_6am = self.getAbsFromLocal(2010, 1, 1, 6, 0)
    for script in scripts:
        if script.get("gridsStartTime", None) == "6am Local":
            script["gridsStartTime"] = time_6am
        if script.get("drtTime", None) == "4am Local":
            script["drtTime"] = time_6am
        elif script.get("drtTime", None) == "6am Local":
            script["drtTime"] = time_6am
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)

