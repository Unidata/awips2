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
# Phrase Tests for Sky, PoP, Weather words
#
# Author:
# ----------------------------------------------------------------------------

# First run setupTextEA

# Runs Phrase_Test_Local for each test
scripts = [
    {
    "name": "E1",
    "commentary": "PoP 0%, Sky 10%, NoWx",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", "all"),
       ],
    "checkStrings": [
       "Sunny"
       ],
    },
    {
    "name": "E2",
    "commentary": "PoP 50%, Sky 70%, Sct RW-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       ("Mostly cloudy with scattered rain showers",
       "Mostly cloudy with scattered showers"),
       "Chance of showers 50 percent",
       ],
    },
    {
    "name": "E3",
    "commentary": "PoP 50%, Sky 80%, SChc RW-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  80, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12,  "SChc:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       ("Mostly cloudy with a 20 percent chance of rain showers",
       "Mostly cloudy with a 20 percent chance of showers"),
       ],
    },
    {
    "name": "E4",
    "commentary": "PoP 50%, Sky 80%, SChc RW-, Patchy F",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  80, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "SChc:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
       ("Mostly cloudy with a 20 percent chance of rain showers",
       "Mostly cloudy with a 20 percent chance of showers"),
       "Patchy fog",
       ],
    },
    {
    "name": "E5",
    "commentary": "PoP 0->20%, Sky 100->100%, NoWx -> SChc T",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  100, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,  "NoWx", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 20, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 100, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "SChc:T:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy",
       "A 20 percent chance of thunderstorms in the afternoon",
       ],
    },
    {
    "name": "E6",
    "commentary": "PoP 50->50%, Sky 90->80%, SChc RW- Patchy F --> SChc RW-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  90, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,
        "SChc:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 80, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "SChc:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Patchy fog in the morning",
       ("Mostly cloudy with a 20 percent chance of rain showers",
       "Mostly cloudy with a 20 percent chance of showers"),
       ],
    },
    {
    "name": "E7",
    "commentary": "PoP 50->50%, Sky 70->80%, SChc RW- Patchy F --> SChc RW-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,
        "Sct:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 80, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Sct:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Patchy fog in the morning",
       ("Mostly cloudy with scattered rain showers","Mostly cloudy with scattered showers"),
       "Chance of showers 50 percent",
       ],
    },
    {
    "name": "E8",
    "commentary": "PoP 50%, Sky 80%, Sct RW-, Patchy F",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 80, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "Sct:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
       ("Mostly cloudy with scattered rain showers", "Mostly cloudy with scattered showers"),
       "Patchy fog",
       "Chance of showers 50 percent",
       ],
    },
    {
    "name": "E9",
    "commentary": "PoP 50->50%, Sky 100->70%, SChc RW- Patchy F --> Wide S-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  100, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,
        "SChc:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Wide:S:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Mostly cloudy",
       "Patchy fog in the morning",
       ("Slight chance of showers in the morning...then widespread show in the afternoon",
        "Slight chance of rain showers in the morning...then widespread snow in the afternoon"),
       "Chance of precipitation 50 percent",
       ],
    },
    {
    "name": "E10",
    "commentary": "PoP 50->50%, Sky 100->70%, SChc RW- Patchy F --> Chc S-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  100, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,
        "SChc:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:S:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Mostly cloudy",
       "Patchy fog in the morning",
       ("Slight chance of showers in the morning...then chance of snow in the afternoon",
        "Slight chance of rain showers in the morning...then chance of snow in the afternoon"), 
       "Chance of precipitation 50 percent",
       ],
    },
    {
    "name": "E11",
    "commentary": "PoP 50->50->50%, Sky 20->80->60%, SChc RW- -> SChc S- -> Chc T",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 3,  20, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3,"SChc:RW:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6,  80, "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6,"SChc:S:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 60, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:T:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Sunny early in the morning then becoming mostly cloudy",
       ("Slight chance of showers early in the morning...then slight chance of snow late in the morning",
        "Slight chance of rain showers early in the morning...then slight chance of snow late in the morning"),
       "Chance of thunderstorms in the afternoon",
       "Chance of precipitation 50 percent",
       ],
    },
    {
    "name": "E12",
    "commentary": "PoP 50->50->50%, Sky 70->80->70%, SChc RW- Patchy F --> SChc S- --> Chc T",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 3,  70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3,
        "SChc:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6,  80, "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6,"SChc:S:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:T:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Mostly cloudy",
       "Patchy fog early in the morning",
       ("Slight chance of showers early in the morning...then slight chance of snow late in the morning",
        "Slight chance of rain showers early in the morning...then slight chance of snow late in the morning"),
       "Chance of thunderstorms in the afternoon",
       "Chance of precipitation 50 percent",
       ],
    },
    {
    #  if T thru-out:
    #     if T+ thru-out:
    #         no TD  (E13.2)
    #     else:
    #         need TD  (E13.1)
    #  elif T+ timeRange == T timeRange:
    #     no TD (E13)
    #  else:
    #     need TD (E13.3)
    "name": "E13",
    "commentary": """
    T not throughout -- T+ timeRange == T timeRange ->
    No Time Descriptor on severeWeather_phrase
    """,
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Wide:R:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:T:+:<NoVis>:^Wide:R:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy",
       "Widespread rain in the morning...then widespread rain and chance of thunderstorms in the afternoon",
       "Some thunderstorms may be severe",
       "Chance of precipitation 50 percent",
       ],
    },
    {
    "name": "E13.1",
    "commentary": """
    T throughout -- severe T not throughout ->
    Time Descriptor on severeWeather_phrase
    """,
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:T:<NoInten>:<NoVis>:^Wide:R:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:T:+:<NoVis>:^Wide:R:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy with widespread rain and chance of thunderstorms",
       "Some thunderstorms may be severe in the afternoon",
       "Chance of precipitation 50 percent",
       ],
    },
    {
    "name": "E13.2",
    "commentary": """
    T throughout -- severe T throughout ->
    No Time Descriptor on severeWeather_phrase
    """,
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:T:+:<NoVis>:^Wide:R:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:T:+:<NoVis>:^Wide:R:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy with widespread rain and chance of thunderstorms",
       "Some thunderstorms may be severe",
       "Chance of precipitation 50 percent",
       ],
    },
    {
    "name": "E13.3",
    "commentary": """
    T not throughout -- severe T does not match T ->
    Time Descriptor on severeWeather_phrase
    """,
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 9, "Sct:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12, "Sct:T:+:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Scattered thunderstorms in the afternoon",
       "Some thunderstorms may be severe late in the afternoon",
       ],
    },

    {
    "name": "E14",
    "commentary": "PoP 50->50%, Sky 95->95%, Chc T Chc RW- --> Chc T+ Chc RW-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,
        "Chc:T:<NoInten>:<NoVis>:^Chc:RW:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12,
        "Chc:T:+:<NoVis>:^Chc:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy with a 50 percent chance of thunderstorms", 
       "Some thunderstorms may be severe in the afternoon",
       ],
    },
    # E15 skipped
    {
    "name": "E16",
    "commentary": "PoP 50%, Sky 95%, Wide S- Chc ZR- Chc IP-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "Wide:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:^Chc:IP:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy with widespread snow...chance of light freezing rain and light sleet",
       ],
    },
    {
    "name": "E17",
    "commentary": "PoP 50%, Sky 95%, Wide S- Chc ZR- SChc IP- --> Lkly S- Chc IP- --> Chc R-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 3,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3,
        "Wide:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:^SChc:IP:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6,
        "Lkly:S:-:<NoVis>:^Chc:IP:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12,
        "Chc:R:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Widespread snow with possible freezing rain and sleet early in the morning...then snow likely and chance of light sleet late in the morning",
       "Chance of rain in the afternoon",
       ],
    },
    {
    "name": "E18",
    "commentary": "PoP 50%, Sky 95%, Wide S- Ocnl ZR- Wide IP- --> Lkly S- Chc IP- --> Chc R-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 3,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3,
        "Wide:S:-:<NoVis>:^Ocnl:ZR:-:<NoVis>:^Wide:IP:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6,
        "Lkly:S:-:<NoVis>:^Chc:IP:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12,
        "Chc:R:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Widespread snow...light sleet and occasional light freezing rain early in the morning...then snow likely and chance of light sleet late in the morning",
       "Chance of rain in the afternoon",
       ],
    },
    {
    "name": "E19",
    "commentary": "PoP 50%, Sky 95%, Lkly S- Chc IP- -> Lkly R-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 3,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3, "Lkly:S:-:<NoVis>:^Chc:IP:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 3, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 3, 12, "Lkly:R:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Snow likely and chance of light sleet early in the morning...then rain likely in the late morning and afternoon",
       ],
    },
    {
    "name": "E17",
    "commentary": "PoP 50%, Sky 95%, Wide S+ -> Lkly S- Chc ZR- SChc IP- -> Chc R- -> SChc TRW-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 3,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3,
        "Wide:S:+:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6,
        "Lkly:S:-:<NoVis>:^Chc:ZR:-:<NoVis>:^SChc:IP:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 9, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 9, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 9,
        "Chc:R:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12,
        "SChc:T:<NoInten>:<NoVis>:^SChc:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Widespread snow with possible freezing rain and sleet in the morning...then chance of rain and slight chance of thunderstorms in the afternoon",
       ],
    },
    {
    "name": "E21",
    "commentary": "PoP 50%, Sky 95%, Ocnl RW-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 3,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3,
        "Lkly:R:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6,
        "NoWx", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12,
        "Ocnl:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Rain likely early in the morning",
       ("Occasional rain showers in the afternoon",
        "Occasional showers in the afternoon"),
       ],
    },
    {
    "name": "E22",
    "commentary": "PoP 50%, Sky 95%, Ocnl RW- Ocnl S- SChc IP-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "Ocnl:R:-:<NoVis>:^Ocnl:S:-:<NoVis>:^SChc:IP:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy with occasional rain...snow and slight chance of light sleet"
       ],
    },
    {
    "name": "E23",
    "commentary": "PoP 50%, Sky 95%, Def S- -> Def S- SChc IP- SChc ZR-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,  "Def:S:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12,
        "Def:S:-:<NoVis>:^SChc:IP:-:<NoVis>:^SChc:ZR:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Snow in the morning...then snow...slight chance of light freezing rain and light sleet in the afternoon",
       ],
    },
    {
    "name": "E24",
    "commentary": "PoP 50%, Sky 95%, Chc R -> Lkly R Lkly IP -> Def S Def IP -> Def S",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 3,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3,
        "Chc:R:m:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6,
        "Lkly:R:m:<NoVis>:^Lkly:IP:m:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 9, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 9, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 9,
        "Def:S:m:<NoVis>:^Def:IP:m:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12,
        "Def:S:m:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Rain and sleet likely in the morning...then snow and sleet in the afternoon",
       ],
    },
    {
    "name": "E25",
    "commentary": "PoP 50%, Sky 95%, Lkly R- -> Lkly R- Lkly S-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,  "Lkly:R:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12,
        "Lkly:R:-:<NoVis>:^Lkly:S:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy",
       "Rain likely in the morning...then rain and snow likely in the afternoon",
       "Chance of precipitation 50 percent",
       ],
    },
    {
    "name": "E26",
    "commentary": "PoP 50%, Sky 95%, Lkly R- SChc T",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "Lkly:R:-:<NoVis>:^SChc:T:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy with rain likely and slight chance of thunderstorms",
       "Chance of precipitation 50 percent",
       ],
    },
    {
    "name": "E27",
    "commentary": "PoP 50%, Sky 95%, Lkly R-S-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "Lkly:R:-:<NoVis>:^Lkly:S:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy with a 50 percent chance of rain and snow",
       ],
    },
    {
    "name": "E28",
    "commentary": "PoP 50%, Sky 95%, Num TRW+ -> Iso RW-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,
        "Num:T:<NoInten>:<NoVis>:^Num:RW:+:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12,
        "Iso:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy",
       ("Numerous showers and thunderstorms in the morning...then isolated rain showers in the afternoon",
        "Numerous showers and thunderstorms in the morning...then isolated showers in the afternoon"),
       "Locally heavy rainfall possible in the morning", 
       "Chance of precipitation 50 percent",
       ],
    },
    {
    "name": "E29",
    "commentary": "PoP 50%, Sky 95%, Num T+RW-LgA -> Iso RW-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,
        "Num:T:+:<NoVis>:LgA^Num:RW:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12,
        "Iso:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy",
       ("Numerous thunderstorms in the morning...then isolated rain showers in the afternoon",
        "Numerous thunderstorms in the morning...then isolated showers in the afternoon"),
        "thunderstorms may be severe with large hail", 
       "Chance of precipitation 50 percent",
       ],
    },
    {
    "name": "E30",
    "commentary": "PoP 50%, Sky 95%, Num SW- SChc IP- Iso T -> Iso RW-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 3,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3,
        "Num:SW:-:<NoVis>:^SChc:IP:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 3, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 3, 12,
        "Iso:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy",
       ("Numerous snow showers with possible sleet and thunderstorms early in the morning...then isolated rain showers in the late morning and afternoon",
        "Numerous snow showers with possible sleet and thunderstorms early in the morning...then isolated showers in the late morning and afternoon"),
       "Chance of precipitation 50 percent",
       ],
    },
    {
    "name": "E31",
    "commentary": "PoP 50%, Sky 95%, Lkly S+ SChc IP- -> SChc ZL-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 3,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3,
        "Lkly:S:+:<NoVis>:^SChc:IP:-:<NoVis>:^SChc:ZL:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 3, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 3, 12,
        "Iso:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Snow likely...slight chance of light freezing drizzle and light sleet early in the morning...then isolated showers in the late morning and afternoon",
       "Snow may be heavy at times early in the morning",
       "Chance of precipitation 50 percent",
       ],
    },
    {
    "name": "E32",
    "commentary": "PoP 50%, Sky 95%, Ocnl S+ -> Lkly S+ SChc ZL- Chc IP- -> Chc R-S-IP- SChc ZL- -> Chc R-",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 3,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3,
        "Ocnl:S:+:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6,
        "Lkly:S:+:<NoVis>:^SChc:ZL:-:<NoVis>:^Chc:IP:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 9, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 9, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 9,
        "Chc:R:-:<NoVis>:^Chc:S:-:<NoVis>:^Chc:IP:-:<NoVis>:^SChc:ZL:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12,
        "Chc:R:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Occasional snow in the morning.",
       "Chance of light sleet and slight chance of light freezing drizzle through the day",
       "Chance of rain and snow in the afternoon.",
       "Snow may be heavy at times in the morning",
       "Chance of precipitation 50 percent",
       ],
    },
    # Skipped E33-E36
    {
    "name": "E37",
    "commentary": "PoP 50%, Sky 95%, SChc T -> Chc T",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "SChc:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:T:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Slight chance of thunderstorms in the morning...then chance of thunderstorms in the afternoon",
       "Chance of thunderstorms 50 percent",
       ],
    },
    {
    "name": "E38",
    "commentary": "PoP 50%, Sky 95%, SChc T HvyRn -> Chc T",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "SChc:T:<NoInten>:<NoVis>:HvyRn", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:T:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Slight chance of thunderstorms in the morning...then chance of thunderstorms in the afternoon",
       "Some thunderstorms may produce heavy rainfall in the morning",
       "Chance of thunderstorms 50 percent",
       ],
    },
    {
    "name": "E39",
    "commentary": "PoP 50%, Sky 95%, SChc T+ -> Chc T",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "SChc:T:+:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:T:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy",
       "Slight chance of thunderstorms in the morning...then chance of thunderstorms in the afternoon",
       "Some thunderstorms may be severe in the morning",
       "Chance of thunderstorms 50 percent",
       ],
    },
    # Skipped E40-41    
    {
    "name": "E42",
    "commentary": "PoP 50%, Sky 95%, Chc T+ -> NoWx",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:T:+:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", "all"),
       ],
    "checkStrings": [
       "Cloudy",
       "A 50 percent chance of thunderstorms in the morning",
       "Some thunderstorms may be severe",
       ],
    },
    
    {
    "name": "E43",  # TK 4469
    "commentary": "Testing ellipses (tk4469)",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:T:+:<NoVis>:^Chc:R:-:<NoVis>:^Chc:S:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", "all"),
       ],
    "checkStrings": [
       "chance of rain...thunderstorms and snow in the morning",
       ],
    },
    {
    "name": "E44", # TK 4469
    "commentary": "Testing ellipses (tk4469)",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,
        "Chc:T:+:<NoVis>:^Chc:R:-:<NoVis>:^Chc:S:-:<NoVis>:", "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 95, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12,  80, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12,
        "Wide:T:<NoInten>:<NoVis>:^Wide:R:-:<NoVis>:^Wide:S:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
        "Chance of rain...thunderstorms and snow in the morning...then widespread rain...thunderstorms and snow in the afternoon",
        ],
       
    },

    {
    "name": "E45",  # TK 4481 Combine Heavy
    "commentary": "tk 4481 combine heavy",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3,
        "Num:T:<NoInten>:<NoVis>:^Num:RW:+:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6,
        "Num:SW:+:<NoVis>:^Num:RW:+:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12,
        "Num:SW:+:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy", "Precipitation may be heavy at times", 
       ],
    },

    {
    "name": "E46",  # TK 4481 Combine Heavy
    "commentary": "tk 4481 combine heavy",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12,  50, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  95, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,
        "Ocnl:T:+:<NoVis>:^Ocnl:RW:+:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 9,
        "Ocnl:T:+:<NoVis>:^Num:RW:m:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12,
        "Chc:T:<NoInten>:<NoVis>:^Chc:RW:m:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Cloudy", "Locally heavy rainfall possible in the morning.",
       ],
    },

    ### Skipped G1-G5 
    {
    "name": "G6",
    "commentary": "Both null",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 12,  0, "all"),
       ],
    "checkStrings": [
       "Light winds",
       ],
    "fileChanges": [],
    },
    {
    "name": "G7",
    "commentary": "Wind null",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 12,  30, "all"),
       ],
    "checkStrings": [
       "Light winds",
       ],
    "fileChanges": [],
    },
    {
    "name": "G8",
    "commentary": "Wind N10, Gust 30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 12,  (10,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 12,  30, "all"),
       ],
    "checkStrings": [
       "North winds around 10 mph with gusts to around 35 mph",
       ],
    "fileChanges": [],
    },
    {
    "name": "G9",
    "commentary": "Wind Calm->N15, Gust 0->0",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 0, "all"),
       ],
    "checkStrings": [
       "Light winds becoming north around 15 mph in the afternoon",
       ],
    "fileChanges": [],
    },
    {
    "name": "G10",
    "commentary": "Wind Calm->N15, Gust 30->0",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  30, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 0, "all"),
       ],
    "checkStrings": [
       "Light winds becoming north around 15 mph in the afternoon",
       ],
    "fileChanges": [],
    },
    {
    "name": "G11",
    "commentary": "Wind Calm->N15, Gust 0->30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 30, "all"),
       ],
    "checkStrings": [
       "Light winds becoming north around 15 mph with gusts to around 35 mph in the afternoon",
       ],
    "fileChanges": [],
    },
    {
    "name": "G12",
    "commentary": "Wind Calm->N15, Gust 30->30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 30, "all"),
       ],
    "checkStrings": [
    "Light winds becoming north around 15 mph in the afternoon",
    "Gusts up to 35 mph",
       ],
    "fileChanges": [],
    },
    {
    "name": "G13",
    "commentary": "Wind N15->0, Gust 0->0",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6, 0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 0, "all"),
       ],
    "checkStrings": [
       "North winds around 15 mph in the morning becoming light",
       ],
    "fileChanges": [],
    },

    {
    "name": "G14",
    "commentary": "Wind N15->0, Gust 30->0",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 0, "all"),
       ],
    "checkStrings": [
       "North winds around 15 mph with gusts to around 35 mph in the morning becoming light",
       ],
    "fileChanges": [],
    },
    {
    "name": "G15",
    "commentary": "Wind N15->0, Gust 0->30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6, 0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 30, "all"),
       ],
    "checkStrings": [
       "North winds around 15 mph in the morning becoming light",
       ],
    "fileChanges": [],
    },
    {
    "name": "G16",
    "commentary": "Wind N15->0, Gust 30->30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 30, "all"),
       ],
    "checkStrings": [
       "North winds around 15 mph in the morning becoming light",
       "Gusts up to 35 mph",
       ],
    "fileChanges": [],
    },
    {
    "name": "G17",
    "commentary": "Wind 0->0->0->0, Gust 30->30->30->30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 3,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 3, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 3, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 3, 6, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 9,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 9, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 9, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 9, 12, 30, "all"),
       ],
    "checkStrings": [
       "Light winds",
       ],
    "fileChanges": [],
    },
    {
    "name": "G18",
    "commentary": "Wind N10->0->0->0, Gust 30->30->30->30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 3,  (10,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 3, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 3, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 3, 6, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 9,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 9, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 9, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 9, 12, 30, "all"),
       ],
    "checkStrings": [
       "North winds around 10 mph early in the morning becoming light",
       "Gusts up to 35 mph",
       ],
    "fileChanges": [],
    },
    {
    "name": "G19",
    "commentary": "Wind 0->N10->0->0, Gust 30->30->30->30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 3,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 3, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 3, 6,  (10,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 3, 6, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 9,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 9, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 9, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 9, 12, 30, "all"),
       ],
    "checkStrings": [
       "Light winds becoming north around 10 mph late in the morning...then becoming light in the afternoon",
       ],
    "fileChanges": [],
    },
    {
    "name": "G20",
    "commentary": "Wind N10->0->N20->0, Gust 40->40->40->40",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 3,  (10,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 3, 40, "all"),
       ("Fcst", "Wind", "VECTOR", 3, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 3, 6, 40, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 9,  (20,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 9, 40, "all"),
       ("Fcst", "Wind", "VECTOR", 9, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 9, 12, 40, "all"),
       ],
    "checkStrings": [
       "breezy",
       "North winds around 10 mph early in the morning becoming light...then becoming north around 25 mph early in the afternoon becoming light",
       "Gusts up to 45 mph",
       ],
    "fileChanges": [],
    },
    {
    "name": "G21",
    "commentary": "Wind N10->0->0->N20, Gust 40->40->40->40",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 3,  (10,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 3, 40, "all"),
       ("Fcst", "Wind", "VECTOR", 3, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 3, 6, 40, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 9,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 9, 40, "all"),
       ("Fcst", "Wind", "VECTOR", 9, 12,  (20,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 9, 12, 40, "all"),
       ],
    "checkStrings": [
       "breezy",
       "North winds around 10 mph early in the morning becoming light...then becoming north around 25 mph late in the afternoon",
       "Gusts up to 45 mph",
       ],
    "fileChanges": [],
    },
    
    {
    "name": "Pass2_G6",
    "commentary": "Both null",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 12,  0, "all"),
       ],
    "checkStrings": [
       "",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },
    {
    "name": "Pass2_G7",
    "commentary": "Wind null",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 12,  30, "all"),
       ],
    "checkStrings": [
       "",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },
    {
    "name": "Pass2_G8",
    "commentary": "Wind N10, Gust 30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 12,  (10,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 12,  30, "all"),
       ],
    "checkStrings": [
       "North winds around 10 mph with gusts to around 35 mph",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },
    {
    "name": "Pass2_G9",
    "commentary": "Wind 0->N15, Gust 0->0",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 0, "all"),
       ],
    "checkStrings": [
       "North winds around 15 mph in the afternoon",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },
    {
    "name": "Pass2_G10",
    "commentary": "Wind 0->N15, Gust 30->0",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  30, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 0, "all"),
       ],
    "checkStrings": [
       "North winds around 15 mph in the afternoon",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },
    {
    "name": "Pass2_G11",
    "commentary": "Wind 0->N15, Gust 0->30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 30, "all"),
       ],
    "checkStrings": [
       "North winds around 15 mph with gusts to around 35 mph in the afternoon",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },
    {
    "name": "Pass2_G12",
    "commentary": "Wind 0->N15, Gust 30->30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 30, "all"),
       ],
    "checkStrings": [
    "North winds around 15 mph in the afternoon",
    "Gusts up to 35 mph",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },
    {
    "name": "Pass2_G13",
    "commentary": "Wind N15->0, Gust 0->0",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6, 0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 0, "all"),
       ],
    "checkStrings": [
       "North winds around 15 mph in the morning",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },

    {
    "name": "Pass2_G14",
    "commentary": "Wind N15->0, Gust 30->0",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 0, "all"),
       ],
    "checkStrings": [
       "North winds around 15 mph with gusts to around 35 mph in the morning",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },
    {
    "name": "Pass2_G15",
    "commentary": "Wind N15->0, Gust 0->30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6, 0, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 30, "all"),
       ],
    "checkStrings": [
       "North winds around 15 mph in the morning",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },
    {
    "name": "Pass2_G16",
    "commentary": "Wind N15->0, Gust 30->30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (15,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12, 30, "all"),
       ],
    "checkStrings": [
       "North winds around 15 mph in the morning. Gusts up to 35 mph",
       "Gusts up to 35 mph",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },
    {
    "name": "Pass2_G17",
    "commentary": "Wind 0->0->0->0, Gust 30->30->30->30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 3,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 3, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 3, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 3, 6, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 9,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 9, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 9, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 9, 12, 30, "all"),
       ],
    "checkStrings": [
       "",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },
    {
    "name": "Pass2_G18",
    "commentary": "Wind N10->0->0->0, Gust 30->30->30->30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 3,  (10,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 3, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 3, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 3, 6, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 9,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 9, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 9, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 9, 12, 30, "all"),
       ],
    "checkStrings": [
       "North winds around 10 mph early in the morning",
       "Gusts up to 35 mph",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },
    {
    "name": "Pass2_G19",
    "commentary": "Wind 0->N10->0->0, Gust 30->30->30->30",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 3,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 3, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 3, 6,  (10,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 3, 6, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 9,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 9, 30, "all"),
       ("Fcst", "Wind", "VECTOR", 9, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 9, 12, 30, "all"),
       ],
    "checkStrings": [
       "North winds around 10 mph late in the morning",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },
    {
    "name": "Pass2_G20",
    "commentary": "Wind N10->0->N20->0, Gust 40->40->40->40",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 3,  (10,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 3, 40, "all"),
       ("Fcst", "Wind", "VECTOR", 3, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 3, 6, 40, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 9,  (20,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 9, 40, "all"),
       ("Fcst", "Wind", "VECTOR", 9, 12,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 9, 12, 40, "all"),
       ],
    "checkStrings": [
       "breezy",
       "North winds around 10 mph early in the morning becoming north around 25 mph early in the afternoon",
       "Gusts up to 45 mph",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
       ],    
    },
    {
    "name": "Pass2_G21",
    "commentary": "Wind N10->0->0->N20, Gust 40->40->40->40",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 3,  (10,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 3, 40, "all"),
       ("Fcst", "Wind", "VECTOR", 3, 6,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 3, 6, 40, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 9,  (0,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 9, 40, "all"),
       ("Fcst", "Wind", "VECTOR", 9, 12,  (20,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 9, 12, 40, "all"),
       ],
    "checkStrings": [
       "breezy",
       "North winds around 10 mph early in the morning becoming north around 25 mph late in the afternoon",
       "Gusts up to 45 mph",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextUtility", "replace",
       ('#dict["Wind"] =  ""', 'dict["Wind"] =  ""'), "undo"),
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

