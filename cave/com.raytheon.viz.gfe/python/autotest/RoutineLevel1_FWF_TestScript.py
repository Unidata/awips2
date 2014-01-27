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
# FWF tests
#
# Author: hansen
# ----------------------------------------------------------------------------


#### File Changes

testDefinitions  = [
'Definition["tempLocalEffects"] = 1',
'Definition["windLocalEffects"] = 1',
'Definition["summaryExtended"] = 0',
#'Definition["summaryArea"] = "FireArea"',
'Definition["individualExtended"] = 0',
'Definition["extendedLabel"] = 1',
'Definition["includeTrends"] = 0',
]


windAreas = """def ridgeValleyAreas(self, tree, node):
        # List of edit area names for which we want
        # ridge/valley winds reported:
        #
        # 20-FOOT WINDS...
        #     VALLEYS/LWR SLOPES...
        #     RIDGES/UPR SLOPES....       
        #
        # e.g.
        # return ["Area1"]
        #
        return ["FLZ042"]
"""

testDefinitions1  = [
'Definition["tempLocalEffects"] = 1',
'Definition["windLocalEffects"] = 0',
'Definition["summaryExtended"] = 0',
#'Definition["summaryArea"] = "FireArea"',
'Definition["individualExtended"] = 0',
'Definition["extendedLabel"] = 1',
'Definition["includeTrends"] = 1'
]


testDefinitions2 =[
'Definition["tempLocalEffects"] = 1',
'Definition["useRH"] = 1'
]

skyWeather1 = [
"""def skyWeather_byTimeRange_compoundPhrase(self):
        return {
            "phraseList": [
                self.fireSky_phrase,
                (self.weather_phrase,self._wxLocalEffects_list()),
                ],
            "phraseMethods": [
                self.consolidateSubPhrases,
                self.assembleSentences,
                self.skyWeather_finishUp, 
            ],
            }""",
"""def getFirePeriod_intersectAreas(self):
        tempList = []
        windList = []
        if self._tempLocalEffects:
            tempList = [
                ("MinT", ["BelowElev", "AboveElev"]),
                ("MaxT", ["BelowElev", "AboveElev"]),
                ("MinRH", ["BelowElev", "AboveElev"]),
                ("MaxRH", ["BelowElev", "AboveElev"]),
                ("RH", ["BelowElev", "AboveElev"]),
                ("Ttrend", ["BelowElev", "AboveElev"]),
                ("RHtrend", ["BelowElev", "AboveElev"]),
                ]
        if self._windLocalEffects:
            windList = [
                ("Wind", ["Valleys", "Ridges"]),
                ("Wind20ft", ["Valleys", "Ridges"]),
                ("WindGust", ["Valleys", "Ridges"]),
                ]
        wxList = [
            ("Wx", ["AboveElev", "BelowElev"]),
            ("PoP", ["AboveElev", "BelowElev"]),
            ]
        return tempList + windList + wxList
"""
]

skyWeather2 = [
"""def skyWeather_byTimeRange_compoundPhrase(self):
        return {
            "phraseList": [
                (self.fireSky_phrase, self._skyLocalEffects_list()),
                (self.skyPopWx_phrase, self._skyPopWxLocalEffects_list()),
                (self.weather_phrase,self._wxLocalEffects_list()),
                (self.popMax_phrase, self._popLocalEffects_list()),
                ],
            "phraseMethods": [
                self.consolidateSubPhrases,
                self.assembleSentences,
                self.skyWeather_finishUp, 
            ],
            }""",
"""def getFirePeriod_intersectAreas(self):
        tempList = []
        windList = []
        if self._tempLocalEffects:
            tempList = [
                ("MinT", ["BelowElev", "AboveElev"]),
                ("MaxT", ["BelowElev", "AboveElev"]),
                ("MinRH", ["BelowElev", "AboveElev"]),
                ("MaxRH", ["BelowElev", "AboveElev"]),
                ("RH", ["BelowElev", "AboveElev"]),
                ("Ttrend", ["BelowElev", "AboveElev"]),
                ("RHtrend", ["BelowElev", "AboveElev"]),
                ]
        if self._windLocalEffects:
            windList = [
                ("Wind", ["Valleys", "Ridges"]),
                ("Wind20ft", ["Valleys", "Ridges"]),
                ("WindGust", ["Valleys", "Ridges"]),
                ]
        wxList = [
            ("Sky", ["AboveElev", "BelowElev"]),
            ("PoP", ["AboveElev", "BelowElev"]),
            ("Wx", ["AboveElev", "BelowElev"]),
            ]
        return tempList + windList + wxList
"""
]

localEffectSetup = [
"""def _skyLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "windward")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        return [self.LocalEffect([leArea1, leArea2], self.checkSkyDifference, "...")]
""", 
"""def _wxLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "windward")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        return [self.LocalEffect([leArea1, leArea2], 0, "...")]
""",
"""def _popLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "windward")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        return [self.LocalEffect([leArea1, leArea2], 20, "...")]
""",
"""def _skyPopWxLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "windward")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        # Set threshold to be used by checkSkyWxDifference
        self._skyLocalEffectThreshold = 38
        return [self.LocalEffect([leArea1, leArea2],
                                 self.checkSkyWxDifference, "...")]
"""
]
    

import TestScript
import CreateGrids

scripts = [
    {
    "commentary":"""
    Morning Test at 4 a.m.
    """,
    "name":"FWF_0", 
    "productType":"FWF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
            ],
    "createGrids": CreateGrids.Public_createGrids + CreateGrids.Fire_createGrids,
    "fileChanges": [
       ],
    },
    {
    "commentary":"""
    Morning Test with tempLocalEffects, windLocalEffects, summaryExtended
                 individualExtended turned off, includeTrends turned off
    """,
    "name":"FWF_1", 
    "productType":"FWF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
          "TODAY",
          "MAX TEMPERATURE.....AROUND 70...EXCEPT AROUND 30 ABOVE TIMBERLINE.",
          "MIN HUMIDITY........50 PERCENT...EXCEPT 20 PERCENT ABOVE TIMBERLINE.",
          "20-FOOT WINDS......."
          "VALLEYS/LWR SLOPES...SOUTHWEST AROUND 10 MPH.",
          "RIDGES/UPR SLOPES....VERY WINDY. NORTH AROUND 45 MPH",
          "TONIGHT",
          "MIN TEMPERATURE.....AROUND 50...EXCEPT AROUND 20 ABOVE TIMBERLINE.",
          "MAX HUMIDITY........70 PERCENT...EXCEPT 30 PERCENT ABOVE TIMBERLINE.",
        ],

    "notCheckStrings": [
          "24 HR TREND......",
          ],
    "createGrids": [
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 30, ["AboveElev"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 70, ["BelowElev"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 20, ["AboveElev"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 50, ["BelowElev"]),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin", "MaxRHEnd", 30,["AboveElev"] ),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin", "MaxRHEnd", 70,["BelowElev"] ),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin", "MinRHEnd", 20, ["AboveElev"]),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin", "MinRHEnd", 50, ["BelowElev"]),
        ("Fcst", "RH", "SCALAR", 0, 12, 40, "all"),
        ("Fcst", "RH", "SCALAR", 12, 24, 40, "all"),
        ("Fcst", "Wind", "VECTOR", 0, 12, (40, "N"), ["Ridges"]),
        ("Fcst", "Wind", "VECTOR", 0, 12, (10, "SW"), ["Valleys"]),
        ("Fcst", "Wind20ft", "VECTOR", 0, 12, (40, "N"), ["Ridges"]),
        ("Fcst", "Wind20ft", "VECTOR", 0, 12, (10, "SW"), ["Valleys"]),
    ],
    "fileChanges": [
       ("FWF_<site>_Definition", "TextUtility", "add", testDefinitions, "undo"),
       ("FWF_<site>_Overrides", "TextUtility", "add", windAreas, "undo"),
       ],
    },
    {
    "commentary":"""
     useRH
    """,
    "name":"FWF_2", 
    "productType":"FWF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
          "TODAY",
          "MIN HUMIDITY........70 PERCENT...EXCEPT 30 PERCENT ABOVE TIMBERLINE.",
          "TONIGHT",
          "MAX HUMIDITY........50 PERCENT...EXCEPT 20 PERCENT ABOVE TIMBERLINE.",
          ],
    "createGrids": [
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 30, ["AboveElev"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 70, ["BelowElev"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 20, ["AboveElev"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 50, ["BelowElev"]),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin", "MaxRHEnd", 40,["AboveElev"] ),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin", "MaxRHEnd", 40,["BelowElev"] ),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin", "MinRHEnd", 40, ["AboveElev"]),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin", "MinRHEnd", 40, ["BelowElev"]),
        ("Fcst", "RH", "SCALAR", 0, 12, 30, ["AboveElev"]),
        ("Fcst", "RH", "SCALAR", 0, 12, 70, ["BelowElev"]),
        ("Fcst", "RH", "SCALAR", 12, 24, 20,["AboveElev"] ),
        ("Fcst", "RH", "SCALAR", 12, 24, 50, ["BelowElev"]),
        ("Fcst", "Wind", "VECTOR", 0, 12, (10, "SW"), ["Ridges"]),
        ("Fcst", "Wind", "VECTOR", 0, 12, (10, "SW"), ["Valleys"]),
    ], 
    "fileChanges": [
       ("FWF_<site>_Definition", "TextUtility", "add", testDefinitions2, "undo"),
       ],
    },
    
    {
    "commentary":"""
    Local effect in compound weather phrase
    """,
    "name":"FWF_3", 
    "productType":"FWF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
            "SKY/WEATHER.........MOSTLY SUNNY (40-50 PERCENT). WIDESPREAD SHOWERS WINDWARD...ISOLATED SHOWERS LEEWARD.",
                    ],
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 20, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 80, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 20, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Wide:RW:-:<NoVis>:", ["AboveElev"]),
       ], 
    "fileChanges": [
       ("FWF_<site>_Overrides", "TextUtility", "add", skyWeather1, "undo"),
       ("FWF_<site>_Overrides", "TextUtility", "add", localEffectSetup, "undo"),
       ],  
    },
    {
    "commentary":"""
    Local effect for compound skyPopWx phrase.
    This shows the capability to do combined phrase local effects in compound phrases.
    There are 2 problems:
    1. Redundant sky wording:
       (SUNNY (15-25PERCENT)...SUNNY WITH ISOLATED SHOWERS.
    This is because the skyPopWx_phrase assumes we are using the sky_phrase
    rather than the fireSky_phrase.
    If someone wanted to do this type of thing, they would have to either decide
    to use the sky_phrase instead of fireSky_phrase OR modify the skyPopWx_phrase
    (in checkSkyPopWx when removing component sky phrases) to work with the
    fireSky_phrase.

    2. Missing popType:
       "Chance of 20 percent leeward"  should be "Chance of showers 20 percent leeward."
       The problem is in "checkRepeatingString" (PhraseBuilder) which does not handle
       compound phrases well.  This is a known bug.
    
    """,
    "name":"FWF_4", 
    "productType":"FWF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
            "SKY/WEATHER.........WINDWARD...MOSTLY CLOUDY (65-75 PERCENT)...WIDESPREAD SHOWERS...CHANCE OF SHOWERS 80 PERCENT. LEEWARD...SUNNY (15-25 PERCENT)...SUNNY WITH ISOLATED SHOWERS...CHANCE OF 20 PERCENT."
            ],    
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 20, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 80, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 20, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Wide:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       ("FWF_<site>_Overrides", "TextUtility", "add", skyWeather2, "undo"),
       ("FWF_<site>_Overrides", "TextUtility", "add", localEffectSetup, "undo"),
       ],
    },
    {
    "commentary":"""
    Trends Test - Afternoon update at 2 p.m.
    This should produce the same trends as in FWF_0 above (DR 18581)
    """,
    "name":"FWF_Trends_AfternoonUpdate", 
    "productType":"FWF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
            "200 PM EST FRI JAN 1 2010",
            ".REST OF TODAY...",
            "MAX TEMPERATURE.....AROUND 78.",
            "24 HR TREND......18 DEGREES WARMER.",
            "MIN HUMIDITY........65 PERCENT.",
            "24 HR TREND......25 PERCENT WETTER.",
            ".TONIGHT...",
            "MIN TEMPERATURE.....AROUND 60.",
            "24 HR TREND......20 DEGREES WARMER.",
            "MAX HUMIDITY........78 PERCENT.",
            "24 HR TREND......18 PERCENT WETTER.",
            ],
    "createGrids": CreateGrids.Public_createGrids + CreateGrids.Fire_createGrids,
    "drtHour": 14,
    "fileChanges": [
       ],
    },
    {
    "commentary":"""
    Trends Test - Evening update at 10 p.m.
    This should produce the same trends as in FWF_0 above (DR 18581)
    """,
    "name":"FWF_Trends_EveningUpdate", 
    "productType":"FWF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Evening Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
            "1000 PM EST FRI JAN 1 2010",
            ".REST OF TONIGHT...",
            "MIN TEMPERATURE.....AROUND 60.",
            "24 HR TREND......20 DEGREES WARMER.",
            "MAX HUMIDITY........78 PERCENT.",
            "24 HR TREND......18 PERCENT WETTER.",
            ],
    "createGrids": CreateGrids.Public_createGrids + CreateGrids.Fire_createGrids,
    "drtHour": 22,
    "fileChanges": [
       ],
    },

    {
    "commentary":"""
    Trends Test with tempLocalEffects - Afternoon update (DR 18581)
    """,
    "name":"FWF_Trends_LE_AfternoonUpdate",
    "productType":"FWF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
          "200 PM EST FRI JAN 1 2010",
          ".REST OF TODAY...",
          "MAX TEMPERATURE.....AROUND 70...EXCEPT AROUND 30 ABOVE TIMBERLINE.",
          "24 HR TREND......10 DEGREES WARMER...EXCEPT 30 DEGREES COOLER ABOVE TIMBERLINE.",
          "MIN HUMIDITY........50 PERCENT...EXCEPT 20 PERCENT ABOVE TIMBERLINE.",
          "24 HR TREND......10 PERCENT WETTER...EXCEPT 20 PERCENT DRIER ABOVE TIMBERLINE.",
          ".TONIGHT...",
          "MIN TEMPERATURE.....AROUND 50...EXCEPT AROUND 20 ABOVE TIMBERLINE.",
          "24 HR TREND......10 DEGREES WARMER...EXCEPT 20 DEGREES COOLER ABOVE TIMBERLINE.",
          "MAX HUMIDITY........70 PERCENT...EXCEPT 30 PERCENT ABOVE TIMBERLINE.",
          "24 HR TREND......10 PERCENT WETTER...EXCEPT 30 PERCENT DRIER ABOVE TIMBERLINE.",
        ],
    "createGrids": [
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 30, ["AboveElev"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 70, ["BelowElev"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 20, ["AboveElev"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 50, ["BelowElev"]),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin", "MaxRHEnd", 30,["AboveElev"] ),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin", "MaxRHEnd", 70,["BelowElev"] ),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin", "MinRHEnd", 20, ["AboveElev"]),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin", "MinRHEnd", 50, ["BelowElev"]),
        ("Fcst", "RH", "SCALAR", 0, 12, 40, "all"),
        ("Fcst", "RH", "SCALAR", 12, 24, 40, "all"),
    ],
    "drtHour": 14,
    "fileChanges": [
       ("FWF_<site>_Definition", "TextUtility", "add", testDefinitions1, "undo"),
       ],
    },

    {
    "commentary":"""
    Trends Test with tempLocalEffects - Evening update (DR 18581)
    """,
    "name":"FWF_Trends_LE_EveningUpdate",
    "productType":"FWF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Evening Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
          "1000 PM EST FRI JAN 1 2010",
          ".REST OF TONIGHT...",
          "MIN TEMPERATURE.....AROUND 50...EXCEPT AROUND 20 ABOVE TIMBERLINE.",
          "24 HR TREND......10 DEGREES WARMER...EXCEPT 20 DEGREES COOLER ABOVE TIMBERLINE.",
          "MAX HUMIDITY........70 PERCENT...EXCEPT 30 PERCENT ABOVE TIMBERLINE.",
          "24 HR TREND......10 PERCENT WETTER...EXCEPT 30 PERCENT DRIER ABOVE TIMBERLINE.",
        ],
    "createGrids": [
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 30, ["AboveElev"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 70, ["BelowElev"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 20, ["AboveElev"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 50, ["BelowElev"]),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin", "MaxRHEnd", 30,["AboveElev"] ),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin", "MaxRHEnd", 70,["BelowElev"] ),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin", "MinRHEnd", 20, ["AboveElev"]),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin", "MinRHEnd", 50, ["BelowElev"]),
        ("Fcst", "RH", "SCALAR", 0, 12, 40, "all"),
        ("Fcst", "RH", "SCALAR", 12, 24, 40, "all"),
    ],
    "drtHour": 22,
    "fileChanges": [
       ("FWF_<site>_Definition", "TextUtility", "add", testDefinitions1, "undo"),
       ],
    },

    {
    "name":"FWF_CleanUp",
    "commentary": "Clean out grids",
    "productType": None,
    "deleteGrids": TestScript.general_deleteGrids,
    },
    ]

def testScript(self, dataMgr, level="Site"):
    gridsStartTime = self.getAbsFromLocal(2010, 1, 1, 6, 0)
    drtTime = self.getAbsFromLocal(2010, 1, 1, 4, 0)    
    defaults = {
        "gridsStartTime": gridsStartTime,
        "drtTime": drtTime,
        "internalStrip": 1, 
        "orderStrings": 1,
        "combinations": "Combinations_FWF_<site>"
        }
    for script in scripts:
        drtHour = script.get("drtHour", None)
        if drtHour is not None:
            script["drtTime"] =  self.getAbsFromLocal(2010, 1, 1, drtHour, 0)      
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults, level=level)


