# #
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
# #
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# RoutineLevel4_1_TestScript    Local Effects
#
# Author:
# ----------------------------------------------------------------------------

# First run setupTextEA



windLE1 = """Definition["windLE_list"] = 1"""
windLE2 = """Definition["windLE_list"] = 2"""

tempLE1 = """Definition["tempLE_list"] = 1"""
tempLE2 = """Definition["tempLE_list"] = 2"""

periodLE1 = """Definition["Period_1_version"] = 1"""
periodLE2 = """Definition["Period_1_version"] = 2"""
periodLE3 = """Definition["Period_1_version"] = 3"""

tempLE_method1 = """Definition["tempLE_method"] = 1"""
tempLE_method2 = """Definition["tempLE_method"] = 2"""


snowLE1 = """##                   (self.weather_phrase,self._wxLocalEffects_list()),             
##                   (self.snow_phrase,self._snowAmtLocalEffects_list()),
##                   (self.total_snow_phrase,self._totalSnowAmtLocalEffects_list()),
"""

snowLE2 = """                     (self.weather_phrase,self._wxLocalEffects_list()),             
                     (self.snow_phrase,self._snowAmtLocalEffects_list()),
                     (self.total_snow_phrase,self._totalSnowAmtLocalEffects_list()),
"""

snow2LE1 = """##            ("Period_2_3", 12), """

snow2LE2 = """              ("Period_2_3", 12), """

# Runs LE_Test_Local for each test

scripts = [
    {
    "name": "LE1",
    "commentary": "Local Effects: MaxT (21,40), Wind (N30,N10), Gust 0",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 21, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 12, (30, "N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 12, (10, "N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "Highs around 40, except in the lower 20s in the mountains",
       "North winds around 10 mph, except north around 35 mph in the mountains",
       ],
    },
    {
    "name": "LE2",
    "commentary": "Local Effects: Wind (N20,N10) -> (N30,N20), Gust 0",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6, (20, "N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 6, (10, "N"), ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (30, "N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (20, "N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "North winds around 10 mph increasing to around 25 mph in the afternoon",
       "In the mountains, north winds around 25 mph increasing to around 35 mph in the afternoon",
       ],
    },
    {
    "name": "LE3",
    "commentary": "Local Effects: Wind (N20,0), Gust 0",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 12, (20, "N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 12, (0, "N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "Light winds, except north around 25 mph in the mountains",
       ],
    },
    {
    "name": "LE4",
    "commentary": "Local Effects: Wind (N20,0) -> (N30,0), Gust 0",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6, (20, "N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 6, (0, "N"), ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (30, "N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (0, "N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "Light winds",
       "In the mountains, north winds around 25 mph increasing to around 35 mph in the afternoon",
       ],
    },
    {
    "name": "LE5",
    "commentary": "Local Effects: Wind (N20,N10), Gust 0, windLE_list=1",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 21, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 12, (20, "N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 12, (10, "N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "North winds around 25 mph in the mountains, otherwise north around 10 mph",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", (windLE1, windLE2), "undo")
       ],

    },

    {
    "name": "LE6",
    "commentary": "Local Effects: Wind (N20,N10) -> (N30,N20), Gust 0, windLE_list=1",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6, (20, "N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 6, (10, "N"), ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (30, "N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (20, "N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "In the mountains, north winds around 25 mph increasing to around 35 mph in the afternoon",
       "In the valleys, north winds around 10 mph increasing to around 25 mph in the afternoon",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", (windLE1, windLE2), "undo")
       ],

    },

    {
    "name": "LE7",
    "commentary": "Local Effects: Temp (21, 40), Wind (N20,N10), Gust 0, tempLE_list=2",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 21, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 12, (20, "N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 12, (10, "N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "Highs around 40, except in the lower 20s in the mountains",
       "North winds around 10 mph, except north around 25 mph in the mountains",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", (tempLE1, tempLE2), "undo")
       ],

    },

    {
    "name": "LE8",
    "commentary": "Local Effects: MaxT (20,20,20), Period_1_version=1",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area3"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area1"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area2"]),
       ],
    "checkStrings": [
       "Highs around 20",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", (periodLE1, periodLE2), "undo")
       ],
    },
    {
    "name": "LE9",
    "commentary": "Local Effects: MaxT (20,20,40), Period_1_version=1",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area3"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area1"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["area2"]),
       ],
    "checkStrings": [
       "Highs around 20, except around 40 in the benches",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", (periodLE1, periodLE2), "undo")
       ],
    },
    {
    "name": "LE10",
    "commentary": "Local Effects: MaxT (20,30,40), Period_1_version=1",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area3"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 30, ["area1"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["area2"]),
       ],
    "checkStrings": [
       "Highs around 20, except around 30 in the rush valley",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", (periodLE1, periodLE2), "undo")
       ],
    },

    {
    "name": "LE11",
    "commentary": "Local Effects: MaxT (20,30,40), Period_1_version=2",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area3"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 30, ["area1"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["area2"]),
       ],
    "checkStrings": [
       "Highs around 20 in the city, and around 30 in the rush valley, and around 40 in the benches",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", [(periodLE1, periodLE2), (tempLE_method1, tempLE_method2)], "undo"),
       ],
    },
    {
    "name": "LE12",
    "commentary": "Local Effects: MaxT (20,40,20), Period_1_version=2",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area3"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["area1"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area2"]),
       ],
    "checkStrings": [
       "Highs around 20 in the city and in the benches, and around 40 in the rush valley",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", [(periodLE1, periodLE2), (tempLE_method1, tempLE_method2)], "undo")
       ],
    },
    {
    "name": "LE13",
    "commentary": "Local Effects: MaxT (20,40,40), Period_1_version=2",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area3"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["area1"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["area2"]),
       ],
    "checkStrings": [
       "Highs around 20 in the city, and around 40 in the rush valley and in the benches",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", [(periodLE1, periodLE2), (tempLE_method1, tempLE_method2)], "undo"),
       ],
    },
    {
    "name": "LE14",
    "commentary": "Local Effects: MaxT (20,20,40), Period_1_version=2",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area3"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area1"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["area2"]),
       ],
    "checkStrings": [
       "Highs around 20 in the city and in the rush valley, and around 40 in the benches",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", [(periodLE1, periodLE2), (tempLE_method1, tempLE_method2)], "undo"),
       ],
    },
    {
    "name": "LE15",
    "commentary": "Local Effects: SnowAmt",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 48, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Lkly:S:-:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 3, ["area3"]),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 3, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 3, ["BelowElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 12, 24, 5, ["area3"]),
       ("Fcst", "SnowAmt", "SCALAR", 12, 24, 5, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 12, 24, 5, ["BelowElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 24, 36, 1, ["area3"]),
       ("Fcst", "SnowAmt", "SCALAR", 24, 36, 1, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 24, 36, 1, ["BelowElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 36, 48, 0, ["area3"]),
       ("Fcst", "SnowAmt", "SCALAR", 36, 48, 0, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 36, 48, 0, ["BelowElev"]),
       ],
    "checkStrings": [
       ".TODAY...", "Snow accumulation around 3 inches",
       ".TONIGHT...", "Snow accumulation around 5 inches",
       "...", "Snow accumulation around 1 inch",
       "...", "No snow accumulation",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", [(snowLE1, snowLE2), (snow2LE1, snow2LE2)], "undo"),
       ],
    "stringOrder": "yes",
    },
    {
    "name": "LE16",
    "commentary": "Local Effects: SnowAmt",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 48, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Lkly:S:-:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 5, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 2, ["BelowElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 12, 24, 4, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 12, 24, 1, ["BelowElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 24, 36, 3, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 24, 36, 1, ["BelowElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 36, 48, 0, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 36, 48, 0, ["BelowElev"]),
       ],
    "checkStrings": [
       ".TODAY...", "Snow accumulation around 2 inches, except around 5 inches above timberline",
       ".TONIGHT...", "Snow accumulation around 1 inch, except around 4 inches above timberline",
       "...", "Snow accumulation of 1 to 3 inches",
       "Total snow accumulation around 4 inches, except around 12 inches above timberline",
       "...", "No snow accumulation",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", [(snowLE1, snowLE2), (snow2LE1, snow2LE2)], "undo"),
       ],
    "stringOrder": "yes",
    },

    {
    "name": "LE17",  # Wade and Ballard
    "commentary": "Local Effects: Wind (N20,N10) -> (N30,N10)",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6, (20, "N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 6, (10, "N"), ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (30, "N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (10, "N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "North winds around 10 mph. In the mountains, north winds around 25 mph increasing to around 35 mph in the afternoon.",
       ],

    },
    {
    "name": "LE18",  # Wade and Ballard
    "commentary": "Local Effects: Wind (N10,N20) -> (N10,N30)",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6, (10, "N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 6, (20, "N"), ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (10, "N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (30, "N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       # "North winds around 25 mph increasing to around 35 mph in the afternoon. North winds around 10 mph in the mountains.",
       "North winds around 25 mph increasing to around 35 mph in the afternoon. In the mountains, north winds around 10 mph.",
       ],
    },

    {
    "name": "LE19",
    "commentary": "Local Effects for non-intersecting areas -- CASE 3 for sub-phrase consolidation",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 48, 30, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 48, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 48, "NoWx", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:SW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", ["area2"]),
       ],
    "checkStrings": [
       "Mostly sunny.",
       "A 50 percent chance of showers in the rush valley, patchy fog in the rush valley, a 50 percent chance of snow showers in the benches, patchy fog in the benches.",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", (periodLE1, periodLE3), "undo"),
       ],
    "stringOrder": "yes",
    },
    {
    "name": "LE20",
    "commentary": "Local Effects for non-intersecting areas -- CASE 3 for sub-phrase consolidation",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 48, 30, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:T:<NoInten>:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:T:<NoInten>:<NoVis>:", ["area2"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:SW:-:<NoVis>:", ["area2"]),
       ],
    "checkStrings": [
       "Mostly sunny.",
       "In the rush valley, chance of thunderstorms in the morning, then chance of showers in the afternoon.",
       "In the benches, chance of thunderstorms in the morning, then chance of snow showers in the afternoon.",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", (periodLE1, periodLE3), "undo"),
       ],
    "stringOrder": "yes",
    },

    {
    "name": "LE21",
    "commentary": "Local Effects for non-intersecting areas -- CASE 3 for sub-phrase consolidation",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 48, 30, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Chc:T:<NoInten>:<NoVis>:", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:T:<NoInten>:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:T:<NoInten>:<NoVis>:", ["area2"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:SW:-:<NoVis>:", ["area2"]),
       ],
    "checkStrings": [
       "Mostly sunny.",
       "In the city, a 50 percent chance of thunderstorms.",
       "In the rush valley, chance of thunderstorms in the morning, then chance of showers in the afternoon.",
       "In the benches, chance of thunderstorms in the morning, then chance of snow showers in the afternoon.",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", (periodLE1, periodLE3), "undo"),
       ],
    "stringOrder": "yes",
    },

    {
    "name": "LE22",
    "commentary": "Local Effects for non-intersecting areas -- CASE 2 for sub-phrase consolidation",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 48, 30, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 48, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Patchy:F:<NoInten>:<NoVis>:", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:SW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", ["area2"]),
       ],
    "checkStrings": [
       "Mostly sunny.",
       "A 50 percent chance of showers in the rush valley, a 50 percent chance of snow showers in the benches, chance of showers in the rush valley, chance of snow showers in the benches.",
       "Patchy fog.",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", (periodLE1, periodLE3), "undo"),
       ],
    "stringOrder": "yes",
    },
    {
    "name": "LE23",
    "commentary": "Local Effects for non-intersecting areas",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 48, 30, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 48, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 48, "NoWx", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:RW:-:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:SW:-:<NoVis>:", ["area2"]),
       ],
    "checkStrings": [
       "Mostly sunny.",
       "A 50 percent chance of showers in the rush valley, a 50 percent chance of snow showers in the benches.",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", (periodLE1, periodLE3), "undo"),
       ],
    "stringOrder": "yes",
    },
    {
    "name": "LE24",
    "commentary": "Local Effects for non-intersecting areas -- no consolidation necessary",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 48, 30, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 48, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:SW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", ["area2"]),
       ],
    "checkStrings": [
       "Mostly sunny.",
       "A 50 percent chance of showers in the city and in the rush valley, a 50 percent chance of snow showers in the benches",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", (periodLE1, periodLE3), "undo"),
       ],
    "stringOrder": "yes",
    },

    ]

import CreateGrids
import TestScript
def testScript(self, dataMgr):
    defaults = {
        "cmdLineVars" :"{('Product Issuance', 'productIssuance'): 'Morning', ('Issuance Type', 'issuanceType'): 'ROUTINE', ('Issued By', 'issuedBy'): None}",
        "deleteGrids": CreateGrids.Delete_grids,
        "productType": "LE_Test_Local",
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)



