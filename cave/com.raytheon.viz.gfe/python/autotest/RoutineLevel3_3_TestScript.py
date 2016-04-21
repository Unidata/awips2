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

periodVer1 = """Definition["Period_1_version"] = 1"""
periodVer2 = """Definition["Period_1_version"] = 2"""

consolidateSkyPopWx = """
    def useSkyPopWx_consolidation(self, tree, node):
        # If set to 1, the skyPopWx phrase will consolidate weather keys that
        # span all time ranges to produce:
        #   Partly cloudy with a chance of rain.
        #   Snow in the morning...then sleet in the afternoon.
        #
        # instead of:
        #    Partly cloudy. Chance of rain and snow in the morning
        #  ...then a chance of rain and sleet in the afternoon. 
        return 1
"""
repeatingEmbedded = """

    def repeatingEmbedded_localEffect_threshold(self, tree, component):
        return 1
    
"""

localEffectSetup = """

    def _skyLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        return [self.LocalEffect([leArea1, leArea2], self.checkSkyDifference, "...")]
    
    def _wxLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        return [self.LocalEffect([leArea1, leArea2], 0, "...")]

    def _popLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        return [self.LocalEffect([leArea1, leArea2], 20, "...")]

    def _skyPopWxLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("AboveElev", "")
        leArea2 = self.LocalEffectArea("BelowElev", "leeward")
        # Set threshold to be used by checkSkyWxDifference
        self._skyLocalEffectThreshold = 38
        return [self.LocalEffect([leArea1, leArea2],
                                 self.checkSkyWxDifference, "...")]

"""

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
       "Mostly sunny with scattered showers"
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
       "Mostly cloudy with scattered showers windward...sunny with scattered showers leeward",
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
       "Mostly sunny",
       "Widespread showers windward...isolated showers leeward",
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
       "Sunny leeward...mostly cloudy with scattered showers windward...chance of showers 50 percent windward."
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
       "Mostly cloudy with widespread showers windward...sunny with isolated showers leeward",
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
       "Leeward...sunny in the morning then becoming partly sunny...Scattered showers",
       "Windward...mostly cloudy with scattered showers",
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
       "Windward...partly sunny in the morning then becoming cloudy",
       "Leeward...sunny in the morning then becoming partly sunny",
       "Scattered showers",
       "Chance of showers 50 percent",
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
       "Mostly sunny",
       "Leeward...isolated showers in the morning",
       "Windward...widespread showers",
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
       "Mostly sunny",
       "Windward...isolated showers in the morning...then widespread showers in the afternoon",
       "Leeward...isolated showers in the afternoon",
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
       "Windward...mostly cloudy with widespread showers",
       "Leeward...sunny with isolated showers in the morning...then partly sunny with widespread showers in the afternoon",
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
       "Windward...mostly sunny with isolated snow showers in the morning...then cloudy with widespread snow showers in the afternoon",
       "Leeward...sunny with isolated showers in the morning...then partly sunny with widespread showers in the afternoon",
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
       "Leeward...mostly sunny...Isolated showers in the morning...then widespread showers in the afternoon",
       "Windward...mostly cloudy with widespread snow showers",
       "Chance of precipitation 50 percent",
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
       "Leeward...sunny in the morning then becoming partly sunny...Isolated showers",
       "Windward...mostly cloudy with widespread snow showers",
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
       "Windward...mostly cloudy...Isolated snow showers in the morning...then widespread snow showers in the afternoon",
       "Leeward...mostly sunny...Isolated showers in the morning...then widespread showers in the afternoon",
       "Chance of precipitation 50 percent",
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
       "Windward...partly sunny in the morning then becoming cloudy",
       "...Widespread snow showers",
       "Leeward...sunny in the morning then becoming partly sunny",
       "...Isolated showers",
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
       "Windward...mostly cloudy",
       "...Isolated snow showers in the morning...then widespread snow showers in the afternoon",
       "Leeward...sunny with isolated showers in the morning...then partly sunny with widespread showers in the afternoon",
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
       "Windward...partly sunny in the morning then becoming cloudy",
       "...Widespread snow showers",
       "Leeward...sunny with isolated showers in the morning...then partly sunny with widespread showers in the afternoon",
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
       ("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo")
    ],
    "checkStrings": [
       "Isolated thunderstorms in the morning.",
       "Rain and snow likely windward...a 40 percent chance of rain leeward.",
       "chance of precipitation 70 percent windward",
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
       ("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo")
    ],
    "checkStrings": [
       "Cloudy",
       "Chance of thunderstorms in the morning...then chance of showers in the afternoon",
       "Chance of precipitation 50 percent",
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
       ("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo")
    ],
    "checkStrings": [
        "Cloudy.",
        "Chance of thunderstorms in the morning.",
        "Windward...showers likely...Chance of precipitation 70 percent.",
        "Leeward...chance of showers in the afternoon...Chance of precipitation 40 percent.",
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
       ("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo")
    ],
    "checkStrings": [
       "Cloudy", 
       "Isolated thunderstorms in the morning",
       "Showers likely windward...a 40 percent chance of showers leeward",
       "chance of precipitation 70 percent windward",
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
       ("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo")
    ],
    "checkStrings": [
       "Isolated thunderstorms in the morning",
       "Cloudy with chance of showers", 
       "Chance of precipitation 50 percent windward...40 percent leeward",
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
       #("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo"),
       ],
    "checkStrings": [
       "Leeward...widespread fog in the morning",
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
       #("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo"),
       ],
    "checkStrings": [
       "Windward...a 20 percent chance of showers late in the afternoon",
       "Leeward...widespread fog late in the afternoon",
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
       #("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo"),
       ],
    "checkStrings": [
       "A 20 percent chance of thunderstorms late in the afternoon",
       "Leeward...areas of fog late in the afternoon",
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
       #("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo"),
       ],
    "checkStrings": [
       "A 20 percent chance of thunderstorms late in the afternoon",
       "Leeward...widespread fog late in the afternoon",
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
       "Mostly sunny", "Drizzle likely",
       ],
    "notCheckStrings": [
       "Mostly sunny with drizzle likely",
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
       #("Phrase_Test_Local", "TextUtility", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo"),
       ],
    "checkStrings": [
       "Windward...patchy fog late in the afternoon",
       "Leeward...widespread fog late in the afternoon",
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
       ("Phrase_Test_Local", "TextUtility", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo"),
       ],
    "checkStrings": [
       "Leeward...widespread fog in the morning",
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
       ("Phrase_Test_Local", "TextUtility", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo"),
       ],
    "checkStrings": [
       "Mostly sunny",
       "A 20 percent chance of showers late in the afternoon",
       "Leeward...widespread fog late in the afternoon",
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
       ("Phrase_Test_Local", "TextUtility", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo"),
       ],
    "checkStrings": [
       "Mostly sunny.",
       "A 20 percent chance of thunderstorms late in the afternoon",
       "Leeward...areas of fog late in the afternoon",
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
       ("Phrase_Test_Local", "TextUtility", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo"),
       ],
    "checkStrings": [
       "Mostly sunny",
       "A 20 percent chance of thunderstorms late in the afternoon",
       "Leeward...widespread fog late in the afternoon",
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
       "Mostly sunny", "Drizzle likely",
       ],
    "notCheckStrings": [
       "Mostly sunny with drizzle likely",
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
       ("Phrase_Test_Local", "TextUtility", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo"),
       ],
    "checkStrings": [
       "Mostly sunny",
       "Patchy fog late in the afternoon",
       "Leeward...widespread fog late in the afternoon",
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
       "windward...Rain...snow likely and isolated thunderstorms in the morning...then rain and snow likely in the afternoon...Chance of precipitation 70 percent.",
       "Leeward...chance of rain and isolated thunderstorms in the morning...then chance of rain in the afternoon...Chance of precipitation 40 percent",
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
       "Cloudy.",
       "windward...showers likely and chance of thunderstorms in the morning...then showers likely in the afternoon...Chance of precipitation 70 percent.",
       "Leeward...chance of thunderstorms in the morning...then chance of showers in the afternoon...Chance of precipitation 40 percent",
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
       "Cloudy", 
       "windward...showers likely and isolated thunderstorms in the morning...then showers likely in the afternoon",
       "Leeward...chance of showers and isolated thunderstorms in the morning...then chance of showers in the afternoon", 
       "Chance of precipitation 50 percent",
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
       "Cloudy",
       "Chance of showers and isolated thunderstorms in the morning...then chance of showers in the afternoon", 
       "Chance of precipitation 50 percent windward...40 percent leeward",
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
       ("Phrase_Test_Local", "TextUtility", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo"),
       ],
    "checkStrings": [
       "Mostly cloudy",
       "Isolated showers and snow showers...isolated showers leeward.",
       "Areas of fog.",
       "Chance of precipitation 20 percent",
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
       "Mostly cloudy",
       "Areas of fog",
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
       ("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo")
    ],
    "checkStrings": [
       "Rain and snow likely windward...a 30 percent chance of rain leeward",
       "chance of precipitation 70 percent windward",
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
       ("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo")
    ],
    "checkStrings": [
       "Rain and snow likely windward...partly sunny with a 30 percent chance of rain leeward.",
       "chance of precipitation 70 percent windward",
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
       ("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo")
    ],
    "checkStrings": [
       "Cloudy with a 20 percent chance of snow windward...partly sunny with a 30 percent chance of rain leeward",
       ],
    "notCheckStrings": [
       "Chance of precipitation 30 percent",
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
       ("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextUtility", "add", repeatingEmbedded, "undo"),    
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo")
    ],
    "checkStrings": [
       "Isolated thunderstorms in the morning",
       "Windward...rain and snow likely...Chance of precipitation 70 percent",
       "Leeward...a 40 percent chance of rain",
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
       ("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo")
    ],
    "checkStrings": [
       "Partly sunny",
       "A 20 percent chance of snow windward...a 30 percent chance of rain leeward",
       ],
    "notCheckStrings": [
       "Chance of precipitation 30 percent",
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
       "Mostly cloudy with scattered showers windward...mostly sunny with scattered showers leeward",
       "Chance of showers 50 percent",
       ],
    "notCheckStrings": ["Scattered showers."],
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
       #("Phrase_Test_Local", "TextUtility", "add", consolidateSkyPopWx, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo"),
       ],
    "checkStrings": [
       "Widespread fog leeward",
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
       "Numerous snow showers windward...chance of snow 70 percent windward"
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
       "Numerous showers and snow showers windward...scattered showers leeward...chance of precipitation 70 percent windward...chance of showers 30 percent leeward"
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
       ("Phrase_Test_Local", "TextUtility", "add", localEffectSetup, "undo"),    
       #("Phrase_Test_Local", "TextUtility", "add", troubleShooting, "undo"),
       ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo"),
       ],
    "checkStrings": [
       "Sunny...",
       "mostly cloudy with a 30 percent chance of rain leeward...widespread fog leeward.",
       ],
    "notCheckStrings":[
       "Widespread fog leeward...chance of rain leeward",
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
       "Windward...periods of snow...Chance of snow 80 percent.",
       "Leeward...chance of rain in the morning...then chance of rain and snow in the afternoon...Chance of precipitation 40 percent.",
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
           ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo")
           ],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)



