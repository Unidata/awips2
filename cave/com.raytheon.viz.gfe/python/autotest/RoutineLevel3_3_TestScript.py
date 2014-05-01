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
# RoutineLevel3_3_TestScript    SkyPopWx Local Effects
#
# Author:
# ----------------------------------------------------------------------------

# First run setupTextEA

periodVer1 = """TextProduct.Definition["Period_1_version"] = 1"""
periodVer2 = """TextProduct.Definition["Period_1_version"] = 2"""

consolidateSkyPopWx = """def useSkyPopWx_consolidation(self, tree, node):
        # If set to 1, the skyPopWx phrase will consolidate weather keys that
        # span all time ranges to produce:
        #   PARTLY CLOUDY WITH A CHANCE OF RAIN.
        #   SNOW IN THE MORNING...THEN SLEET IN THE AFTERNOON.
        #
        # instead of:
        #    PARTLY CLOUDY. CHANCE OF RAIN AND SNOW IN THE MORNING
        #  ...THEN A CHANCE OF RAIN AND SLEET IN THE AFTERNOON. 
        return 1
"""
repeatingEmbedded = """def repeatingEmbedded_localEffect_threshold(self, tree, component):
        return 1
"""

localEffectSetup = [
"""def _skyLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        return [self.LocalEffect([leArea1, leArea2], self.checkSkyDifference, "...")]
""",
"""def _wxLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        return [self.LocalEffect([leArea1, leArea2], 0, "...")]
""",
"""def _popLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        return [self.LocalEffect([leArea1, leArea2], 20, "...")]
""",        
"""def _skyPopWxLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        # Set threshold to be used by checkSkyWxDifference
        self._skyLocalEffectThreshold = 38
        return [self.LocalEffect([leArea1, leArea2],
                                 self.checkSkyWxDifference, "...")]
"""
]

troubleShooting = """

"""


# Runs Phrase_Test_Local for each test
scripts = [
    {
    "name": "F3",
    "commentary": "No Local Effects",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "MOSTLY SUNNY WITH SCATTERED SHOWERS"
       ],
    },
    {
    "name": "F4",
    "commentary": "Local Effect for Sky",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 20, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "MOSTLY CLOUDY WITH SCATTERED SHOWERS WINDWARD...SUNNY WITH SCATTERED SHOWERS LEEWARD",
       ],
    },
    {
    "name": "F5",
    "commentary": "Local Effect for Wx",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Wide:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "MOSTLY SUNNY",
       "WIDESPREAD SHOWERS WINDWARD...ISOLATED SHOWERS LEEWARD",
       ],
    },


    {
    "name": "F6",
    # Note: The Leeward phrase is a sky_phrase while the
    # Windward phrase is a skyPopWx_phrase.
    # With the lePhraseGroupNames of
    #    ("skyPopWx_phrase", "weather_phrase", "sky_phrase", "pop_phrase")
    # They are reported as one embedded phrase.
    "commentary": "Local Effect for Sky, Wx with NoWx in one area",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 0, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 20, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "SUNNY LEEWARD...MOSTLY CLOUDY WITH SCATTERED SHOWERS WINDWARD...CHANCE OF SHOWERS 50 PERCENT WINDWARD."
       ],
    },
    {
    "name": "F7",
    "commentary": "Local Effect for Sky, Wx",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 20, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Wide:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "MOSTLY CLOUDY WITH WIDESPREAD SHOWERS WINDWARD...SUNNY WITH ISOLATED SHOWERS LEEWARD",
       ],
    },
    {
    "name": "F8",
    "commentary": "Local Effect for Sky with timers for one area",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6, 20, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 6, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 55, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "LEEWARD...SUNNY IN THE MORNING THEN BECOMING PARTLY SUNNY...SCATTERED SHOWERS",
       "WINDWARD...MOSTLY CLOUDY WITH SCATTERED SHOWERS",
       ],
    },

    {
    "name": "F9",
    "commentary": "Local Effect for Sky with timers for both areas",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6, 20, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 6, 55, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 55, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 88, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "WINDWARD...PARTLY SUNNY IN THE MORNING THEN BECOMING CLOUDY",
       "LEEWARD...SUNNY IN THE MORNING THEN BECOMING PARTLY SUNNY",
       "SCATTERED SHOWERS",
       "CHANCE OF SHOWERS 50 PERCENT",
       ],
    },    
    {
    "name": "F10",
    "commentary": "Local Effect for Wx with timers for one area",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Wide:RW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "MOSTLY SUNNY",
       "LEEWARD...ISOLATED SHOWERS IN THE MORNING",
       "WINDWARD...WIDESPREAD SHOWERS",
       ],
    },

    {
    "name": "F11",
    "commentary": "Local Effect for Wx with timers for both areas",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Iso:RW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "MOSTLY SUNNY",
       "WINDWARD...ISOLATED SHOWERS IN THE MORNING...THEN WIDESPREAD SHOWERS IN THE AFTERNOON",
       "LEEWARD...ISOLATED SHOWERS IN THE AFTERNOON",
       ],
    },
    {
    "name": "F12",
    "commentary": "Local Effect for Wx with timers for one area and Sky with timers for one area",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6, 20, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 6, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 55, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Wide:RW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "WINDWARD...MOSTLY CLOUDY WITH WIDESPREAD SHOWERS",
       "LEEWARD...SUNNY WITH ISOLATED SHOWERS IN THE MORNING...THEN PARTLY SUNNY WITH WIDESPREAD SHOWERS IN THE AFTERNOON",
       ],
    },
    {
    "name": "F13",
    "commentary": "Local Effect for Wx with timers for both areas, Sky with timers for both areas",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6, 20, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 6, 50, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 55, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 88, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Iso:SW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:SW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "WINDWARD...MOSTLY SUNNY WITH ISOLATED SNOW SHOWERS IN THE MORNING...THEN CLOUDY WITH WIDESPREAD SNOW SHOWERS IN THE AFTERNOON",
       "LEEWARD...SUNNY WITH ISOLATED SHOWERS IN THE MORNING...THEN PARTLY SUNNY WITH WIDESPREAD SHOWERS IN THE AFTERNOON",
       ],
    },
    {
    "name": "F14",
    "commentary": "Local Effect for Wx with timers for one area,  Sky with no timers",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6, 50, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 6, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 50, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Wide:SW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:SW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "LEEWARD...MOSTLY SUNNY...ISOLATED SHOWERS IN THE MORNING...THEN WIDESPREAD SHOWERS IN THE AFTERNOON",
       "WINDWARD...MOSTLY CLOUDY WITH WIDESPREAD SNOW SHOWERS",
       "CHANCE OF PRECIPITATION 50 PERCENT",
       ],
    },
    {
    "name": "F15",
    "commentary": "Local Effect for Wx with no timers,  Sky with timers for one area",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6, 20, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 6, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 55, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Wide:SW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:SW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "LEEWARD...SUNNY IN THE MORNING THEN BECOMING PARTLY SUNNY...ISOLATED SHOWERS",
       "WINDWARD...MOSTLY CLOUDY WITH WIDESPREAD SNOW SHOWERS",
       ],
    },
    {
    "name": "F16",
    "commentary": "Local Effect for Wx with timers for both areas,  Sky with no timers",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6, 50, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 6, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 50, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Iso:SW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:SW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "WINDWARD...MOSTLY CLOUDY...ISOLATED SNOW SHOWERS IN THE MORNING...THEN WIDESPREAD SNOW SHOWERS IN THE AFTERNOON",
       "LEEWARD...MOSTLY SUNNY...ISOLATED SHOWERS IN THE MORNING...THEN WIDESPREAD SHOWERS IN THE AFTERNOON",
       "CHANCE OF PRECIPITATION 50 PERCENT",
       ],
    },
    {
    "name": "F17",
    "commentary": "Local Effect for Wx with no timers,  Sky with timers for both areas",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6, 20, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 6, 55, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 55, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 88, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Wide:SW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:SW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "WINDWARD...PARTLY SUNNY IN THE MORNING THEN BECOMING CLOUDY",
       "...WIDESPREAD SNOW SHOWERS",
       "LEEWARD...SUNNY IN THE MORNING THEN BECOMING PARTLY SUNNY",
       "...ISOLATED SHOWERS",
       ],
    },


    {
    "name": "F18",
    "commentary": "Local Effect for Wx with timers for both areas,  Sky with timers for one area",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6, 20, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 6, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 55, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Iso:SW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:SW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "WINDWARD...MOSTLY CLOUDY",
       "...ISOLATED SNOW SHOWERS IN THE MORNING...THEN WIDESPREAD SNOW SHOWERS IN THE AFTERNOON",
       "LEEWARD...SUNNY WITH ISOLATED SHOWERS IN THE MORNING...THEN PARTLY SUNNY WITH WIDESPREAD SHOWERS IN THE AFTERNOON",
       ],
    },


    {
    "name": "F19",
    "commentary": "Local Effect for Wx with timers for one area,  Sky with timers for both areas",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6, 20, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 6, 55, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 55, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 88, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Iso:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Wide:SW:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:SW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "WINDWARD...PARTLY SUNNY IN THE MORNING THEN BECOMING CLOUDY",
       "...WIDESPREAD SNOW SHOWERS",
       "LEEWARD...SUNNY WITH ISOLATED SHOWERS IN THE MORNING...THEN PARTLY SUNNY WITH WIDESPREAD SHOWERS IN THE AFTERNOON",
       ],
    },


    #### Consolidating LE's
    {
    "name": "F20",
    "commentary": """
            TK 4312 -- Eliminates duplicate phrasing for local effects when consolidating.
            NOTE: Chance of rain...chance of rain 40 percent.
             is "ok" cuz with morning T storms there are 2 subphrases to account for in
             in the PoP phrase.  
       """,
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 40, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:R:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6,
        "Lkly:S:-:<NoVis>:^Lkly:R:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:R:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Lkly:S:-:<NoVis>:^Lkly:R:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo")
    ],
    "checkStrings": [
       "ISOLATED THUNDERSTORMS IN THE MORNING.",
       "RAIN AND SNOW LIKELY WINDWARD...A 40 PERCENT CHANCE OF RAIN LEEWARD.",
       "CHANCE OF PRECIPITATION 70 PERCENT WINDWARD",
       ],
    },

    {
    "name": "F21",
    "commentary": "No local effect for Wx, No local effect for PoP",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:^Chc:T:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:^Chc:T:<NoInten>:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo")
    ],
    "checkStrings": [
       "CLOUDY",
       "CHANCE OF THUNDERSTORMS IN THE MORNING...THEN CHANCE OF SHOWERS IN THE AFTERNOON",
       "CHANCE OF PRECIPITATION 50 PERCENT",
       ],
    },

    {
    "name": "F22",  # Note: T and RW are combined for Leeward since both are Chc
    "commentary": "Local effect for Wx, Local effect for PoP",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 40, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:^Chc:T:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Lkly:RW:-:<NoVis>:^Chc:T:<NoInten>:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Lkly:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo")
    ],
    "checkStrings": [
        "CLOUDY.",
        "CHANCE OF THUNDERSTORMS IN THE MORNING.",
        "WINDWARD...SHOWERS LIKELY...CHANCE OF PRECIPITATION 70 PERCENT.",
        "LEEWARD...CHANCE OF SHOWERS IN THE AFTERNOON...CHANCE OF PRECIPITATION 40 PERCENT.",
       ],
    },
    
    {
    "name": "F23",
    "commentary": "Local effect for Wx, No local effect for PoP",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 40, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Lkly:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Lkly:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo")
    ],
    "checkStrings": [
       "CLOUDY", 
       "ISOLATED THUNDERSTORMS IN THE MORNING",
       "SHOWERS LIKELY WINDWARD...A 40 PERCENT CHANCE OF SHOWERS LEEWARD",
       "CHANCE OF PRECIPITATION 70 PERCENT WINDWARD",
       ],
    },
    
    {     
    "name": "F24",
    "commentary": "No local effect for Wx, Local effect for PoP",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 40, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo")
    ],
    "checkStrings": [
       "ISOLATED THUNDERSTORMS IN THE MORNING",
       "CLOUDY WITH CHANCE OF SHOWERS", 
       "CHANCE OF PRECIPITATION 50 PERCENT WINDWARD...40 PERCENT LEEWARD",
       ],
    },

    {     
    "name": "F25",
    "commentary": "Fog local effect",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 5, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 5, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Wide:F:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["AboveElev"]),
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo"),
       ],
    "checkStrings": [
       "LEEWARD...WIDESPREAD FOG IN THE MORNING",
       ],
    },

    {     
    "name": "F26",
    "commentary": "Fog local effect",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 9, 0, "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 0, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 9, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12, "Wide:F:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 9, 12, "SChc:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo"),
       ],
    "checkStrings": [
       "WINDWARD...A 20 PERCENT CHANCE OF SHOWERS LATE IN THE AFTERNOON",
       "LEEWARD...WIDESPREAD FOG LATE IN THE AFTERNOON",
       ],
    },
    {     
    "name": "F27",
    "commentary": "Fog local effects",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 9, 0, "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 9, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12,
       "Areas:F:<NoInten>:<NoVis>:^SChc:T:<NoInten>:<NoVis>:^SChc:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 9, 12, "SChc:T:<NoInten>:<NoVis>:^SChc:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo"),
       ],
    "checkStrings": [
       "A 20 PERCENT CHANCE OF THUNDERSTORMS LATE IN THE AFTERNOON",
       "LEEWARD...AREAS OF FOG LATE IN THE AFTERNOON",
       ],
    },

    {     
    "name": "F28",
    "commentary": "Fog local effects with visibility",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 9, 0, "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 9, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12,
       "Wide:F:<NoInten>:1SM:^SChc:T:<NoInten>:<NoVis>:^SChc:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 9, 12, "SChc:T:<NoInten>:<NoVis>:^SChc:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo"),
       ],
    "checkStrings": [
       "A 20 PERCENT CHANCE OF THUNDERSTORMS LATE IN THE AFTERNOON",
       "LEEWARD...WIDESPREAD FOG LATE IN THE AFTERNOON",
       ],
    },

    {     
    "name": "F29",
    "commentary": "Fog with no local effects",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Lkly:L:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "MOSTLY SUNNY", "DRIZZLE LIKELY",
       ],
    "notCheckStrings": [
       "MOSTLY SUNNY WITH DRIZZLE LIKELY",
       ],
    },

    {     
    "name": "F30",
    "commentary": "Fog local effect",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 9, 0, "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 0, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 9, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12, "Wide:F:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 9, 12, "Patchy:F:<NoInten>:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo"),
       ],
    "checkStrings": [
       "WINDWARD...PATCHY FOG LATE IN THE AFTERNOON",
       "LEEWARD...WIDESPREAD FOG LATE IN THE AFTERNOON",
       ],
    },

    
    ### Using empty qualifier  TK 4518
    {     
    "name": "F31",
    "commentary": "Fog local effect",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 5, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 5, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Wide:F:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo"),
       ],
    "checkStrings": [
       "LEEWARD...WIDESPREAD FOG IN THE MORNING",
       ],
    },

    {     
    "name": "F32",
    "commentary": "Fog local effect",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 9, 0, "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 0, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 9, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12, "Wide:F:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 9, 12, "SChc:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextProduct", "replace", (periodVer1, periodVer2), "undo"),
       ],
    "checkStrings": [
       "MOSTLY SUNNY",
       "A 20 PERCENT CHANCE OF SHOWERS LATE IN THE AFTERNOON",
       "LEEWARD...WIDESPREAD FOG LATE IN THE AFTERNOON",
       ],
    },
    {     
    "name": "F33",
    "commentary": "Fog local effects",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 9, 0, "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 9, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12,
       "Areas:F:<NoInten>:<NoVis>:^SChc:T:<NoInten>:<NoVis>:^SChc:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 9, 12, "SChc:T:<NoInten>:<NoVis>:^SChc:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo"),
       ],
    "checkStrings": [
       "MOSTLY SUNNY.",
       "A 20 PERCENT CHANCE OF THUNDERSTORMS LATE IN THE AFTERNOON",
       "LEEWARD...AREAS OF FOG LATE IN THE AFTERNOON",
       ],
    },

    {     
    "name": "F34",
    "commentary": "Fog local effects with visibility",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 9, 0, "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 9, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12,
       "Wide:F:<NoInten>:1SM:^SChc:T:<NoInten>:<NoVis>:^SChc:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 9, 12, "SChc:T:<NoInten>:<NoVis>:^SChc:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [ 
       ("Phrase_Test_Local", "TextProduct", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo"),
       ],
    "checkStrings": [
       "MOSTLY SUNNY",
       "A 20 PERCENT CHANCE OF THUNDERSTORMS LATE IN THE AFTERNOON",
       "LEEWARD...WIDESPREAD FOG LATE IN THE AFTERNOON",
       ],
    },

    {     
    "name": "F35",
    "commentary": "Drizzle with no local effects",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Lkly:L:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "MOSTLY SUNNY", "DRIZZLE LIKELY",
       ],
    "notCheckStrings": [
       "MOSTLY SUNNY WITH DRIZZLE LIKELY",
       ],
    },    
    {     
    "name": "F36",
    "commentary": "Fog local effect",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 9, 0, "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 0, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, ["AboveElev"]),
       #("Fcst", "Sky", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 9, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12, "Wide:F:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 9, 12, "Patchy:F:<NoInten>:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo"),
       ],
    "checkStrings": [
       "MOSTLY SUNNY",
       "PATCHY FOG LATE IN THE AFTERNOON",
       "LEEWARD...WIDESPREAD FOG LATE IN THE AFTERNOON",
       ],
    },

    ### without consolidation
    {
    #  Minor bug with spacing results in "combineSentences" (PhraseBuilder)
    "name": "F37",
    "commentary": "Same as F20 but No consolidating",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 40, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:R:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6,
        "Lkly:S:-:<NoVis>:^Lkly:R:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:R:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Lkly:S:-:<NoVis>:^Lkly:R:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "WINDWARD...RAIN...SNOW LIKELY AND ISOLATED THUNDERSTORMS IN THE MORNING...THEN RAIN AND SNOW LIKELY IN THE AFTERNOON...CHANCE OF PRECIPITATION 70 PERCENT.",
       "LEEWARD...CHANCE OF RAIN AND ISOLATED THUNDERSTORMS IN THE MORNING...THEN CHANCE OF RAIN IN THE AFTERNOON...CHANCE OF PRECIPITATION 40 PERCENT",
       ],
    },
    
    {
    "name": "F38",
    "commentary": "Same as F22 without consolidation: Local effect for Wx, Local effect for PoP",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 40, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:^Chc:T:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Lkly:RW:-:<NoVis>:^Chc:T:<NoInten>:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Lkly:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "CLOUDY.",
       "WINDWARD...SHOWERS LIKELY AND CHANCE OF THUNDERSTORMS IN THE MORNING...THEN SHOWERS LIKELY IN THE AFTERNOON...CHANCE OF PRECIPITATION 70 PERCENT.",
       "LEEWARD...CHANCE OF THUNDERSTORMS IN THE MORNING...THEN CHANCE OF SHOWERS IN THE AFTERNOON...CHANCE OF PRECIPITATION 40 PERCENT",
       ],
    },

    {
    "name": "F39",
    "commentary": "Same as F23 without consolidation. Local effect for Wx, No local effect for PoP",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Lkly:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Lkly:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "CLOUDY", 
       "WINDWARD...SHOWERS LIKELY AND ISOLATED THUNDERSTORMS IN THE MORNING...THEN SHOWERS LIKELY IN THE AFTERNOON",
       "LEEWARD...CHANCE OF SHOWERS AND ISOLATED THUNDERSTORMS IN THE MORNING...THEN CHANCE OF SHOWERS IN THE AFTERNOON", 
       "CHANCE OF PRECIPITATION 50 PERCENT",
       ],
    },

    {     
    "name": "F40",
    "commentary": "Same as F24 without consolidation. No local effect for Wx, Local effect for PoP",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 40, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["AboveElev"]),
       ],  
    "checkStrings": [
       "CLOUDY",
       "CHANCE OF SHOWERS AND ISOLATED THUNDERSTORMS IN THE MORNING...THEN CHANCE OF SHOWERS IN THE AFTERNOON", 
       "CHANCE OF PRECIPITATION 50 PERCENT WINDWARD...40 PERCENT LEEWARD",
       ],
    },

    {     
    "name": "F41",
    "commentary": "",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 20, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 20, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12,
       "Iso:RW:-:<NoVis>:^Areas:F:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12,
       "Iso:RW:-:<NoVis>:^Iso:SW:-:<NoVis>:^Areas:F:<NoInten>:<NoVis>:",
       ["AboveElev"]),
       ],  
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo"),
       ],
    "checkStrings": [
       "MOSTLY CLOUDY",
       "ISOLATED SHOWERS AND SNOW SHOWERS...ISOLATED SHOWERS LEEWARD.",
       "AREAS OF FOG.",
       "CHANCE OF PRECIPITATION 20 PERCENT",
       ],
    },

    {   
    "name": "F42",
    "commentary": "Depends on removing spawned phrases when SPW is removed",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 10, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Areas:F:<NoInten>:<NoVis>:", "all"),
       ],  
    "fileChanges": [
       ],
    "checkStrings": [
       "MOSTLY CLOUDY",
       "AREAS OF FOG",
       ],
    },
    
    {
    "name": "F43",  # ??????
    "commentary": "",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 30, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Chc:R:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Lkly:S:-:<NoVis>:^Lkly:R:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo")
    ],
    "checkStrings": [
       "RAIN AND SNOW LIKELY WINDWARD...A 30 PERCENT CHANCE OF RAIN LEEWARD",
       "CHANCE OF PRECIPITATION 70 PERCENT WINDWARD",
       ],
    },
    {
    "name": "F44", # ?????
    "commentary": "",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 30, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 90, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 60, ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Chc:R:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Lkly:S:-:<NoVis>:^Lkly:R:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo")
    ],
    "checkStrings": [
       "RAIN AND SNOW LIKELY WINDWARD...PARTLY SUNNY WITH A 30 PERCENT CHANCE OF RAIN LEEWARD.",
       "CHANCE OF PRECIPITATION 70 PERCENT WINDWARD",
       ],
    },
    {
    "name": "F45",  # Needed checkSPW to remove pop phrases for
                    # component as well as local effect area.
    "commentary": "",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 30, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 20, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 90, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 60, ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Chc:R:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "SChc:S:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo")
    ],
    "checkStrings": [
       "CLOUDY WITH A 20 PERCENT CHANCE OF SNOW WINDWARD...PARTLY SUNNY WITH A 30 PERCENT CHANCE OF RAIN LEEWARD",
       ],
    "notCheckStrings": [
       "CHANCE OF PRECIPITATION 30 PERCENT",
       ],
    },

    {
    "name": "F46",
    "commentary": """
           Same set up as F20, but with repeatingEmbedded_localEffect_threshold set to 
           0 instead of 2
       """,
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 40, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:R:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6,
        "Lkly:S:-:<NoVis>:^Lkly:R:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:R:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Lkly:S:-:<NoVis>:^Lkly:R:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextProduct", "add", repeatingEmbedded, "undo"),    
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo")
    ],
    "checkStrings": [
       "ISOLATED THUNDERSTORMS IN THE MORNING",
       "WINDWARD...RAIN AND SNOW LIKELY...CHANCE OF PRECIPITATION 70 PERCENT",
       "LEEWARD...A 40 PERCENT CHANCE OF RAIN",
       ],
    },

    {
    "name": "F47", 
    "commentary": "Duplicate sky??",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 30, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 20, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 60, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 60, ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Chc:R:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "SChc:S:-:<NoVis>:", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo")
    ],
    "checkStrings": [
       "PARTLY SUNNY",
       "A 20 PERCENT CHANCE OF SNOW WINDWARD...A 30 PERCENT CHANCE OF RAIN LEEWARD",
       ],
    "notCheckStrings": [
       "CHANCE OF PRECIPITATION 30 PERCENT",
       ],
    },
    {
    "name": "F48",
    "commentary": """
        Repeating Wx in local effect phrases
        Needed to remove compArea weather_phrase as well as local effect one
        """,
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6, 20, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 6, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 50, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "MOSTLY CLOUDY WITH SCATTERED SHOWERS WINDWARD...MOSTLY SUNNY WITH SCATTERED SHOWERS LEEWARD",
       "CHANCE OF SHOWERS 50 PERCENT",
       ],
    "notCheckStrings": ["SCATTERED SHOWERS."],
    },

    {     
    "name": "F49",
    "commentary": "Fog local effect and changing sky",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 87, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Wide:F:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", ["AboveElev"]),
       ],
    "fileChanges": [
       #("Phrase_Test_Local", "TextProduct", "add", consolidateSkyPopWx, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo"),
       ],
    "checkStrings": [
       "WIDESPREAD FOG LEEWARD",
       ],
    },

    {
    "name": "F50",
    "commentary": "Local Effect for Wx with NoWx in one area",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 0, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 70, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:SW:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "NUMEROUS SNOW SHOWERS WINDWARD...CHANCE OF SNOW 70 PERCENT WINDWARD"
       ],
    },
    {
    "name": "F51",  ## Wade
    "commentary": "Local effect for Wx, Pop -- coverage terms",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 30, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 70, ["BelowElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 70, ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:SW:-:<NoVis>:^Num:RW:-:<NoVis>:^", ["AboveElev"]),
       ],
    "checkStrings": [
       "NUMEROUS SHOWERS AND SNOW SHOWERS WINDWARD...SCATTERED SHOWERS LEEWARD...CHANCE OF PRECIPITATION 70 PERCENT WINDWARD...CHANCE OF SHOWERS 30 PERCENT LEEWARD"
       ],
    },


    {     
    "name": "F52",  # Metze 1
    "commentary": "Fog and rain local effect",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 25, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 5, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 25, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 87, ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12,
          "Chc:R:-:<NoVis>:^Wide:F:<NoInten>:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", ["AboveElev"]),
       ],
    "fileChanges": [
       ("Phrase_Test_Local", "TextProduct", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextProduct", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo"),
       ],
    "checkStrings": [
       "SUNNY...",
       "MOSTLY CLOUDY WITH A 30 PERCENT CHANCE OF RAIN LEEWARD...WIDESPREAD FOG LEEWARD.",
       ],
    "notCheckStrings":[
       "WIDESPREAD FOG LEEWARD...CHANCE OF RAIN LEEWARD",
       ],
    },
    {     
    "name": "F53",  # Metze 2
    "commentary": "PoP Local effect",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 40, ["BelowElev"]),
       ("Fcst", "PoP", "SCALAR", 0, 12, 80, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 87, ["AboveElev"]),
       ("Fcst", "Sky", "SCALAR", 0, 12, 87, ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6,
          "Chc:R:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12,
          "Chc:R:-:<NoVis>:^Chc:S:-:<NoVis>:", ["BelowElev"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Pds:S:-:<NoVis>:", ["AboveElev"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Pds:S:-:<NoVis>:", ["AboveElev"]),
       ],
    "checkStrings": [
       "WINDWARD...PERIODS OF SNOW...CHANCE OF SNOW 80 PERCENT.",
       "LEEWARD...CHANCE OF RAIN IN THE MORNING...THEN CHANCE OF RAIN AND SNOW IN THE AFTERNOON...CHANCE OF PRECIPITATION 40 PERCENT.",
       ],
    },


    {     
    "name": "F54",  # Ireland
    "commentary": "Reporting local effect when no local effect",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 35, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", "all"),
       ],
    "checkStrings": [
       ],
    },
    
    ]


import TestScript
def testScript(self, dataMgr):
    defaults = {
        "cmdLineVars" :"{('Product Issuance', 'productIssuance'): 'Morning', ('Issuance Type', 'issuanceType'): 'ROUTINE', ('Issued By', 'issuedBy'): None}",
        "productType": "Phrase_Test_Local",
        "fileChanges" : [
           ("Phrase_Test_Local", "TextProduct", "replace", periodVer2, "undo")
           ],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)



