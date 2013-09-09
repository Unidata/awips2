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

def1 = ["""def first_null_phrase_dict(self, tree, node):   
    import TextRules     
    dict = TextRules.TextRules.first_null_phrase_dict(self, tree, node)    
    dict["Wind"] =  ""
    return dict""",

"""def null_phrase_dict(self, tree, node):
    import TextRules    
    dict = TextRules.TextRules.null_phrase_dict(self, tree, node)
    dict["Wind"] =  ""
    dict["Wx"] =  ""
    return dict"""
]

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
       "SUNNY"
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
       ("MOSTLY CLOUDY WITH SCATTERED RAIN SHOWERS",
       "MOSTLY CLOUDY WITH SCATTERED SHOWERS"),
       "CHANCE OF SHOWERS 50 PERCENT",
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
       ("MOSTLY CLOUDY WITH A 20 PERCENT CHANCE OF RAIN SHOWERS",
       "MOSTLY CLOUDY WITH A 20 PERCENT CHANCE OF SHOWERS"),
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
       ("MOSTLY CLOUDY WITH A 20 PERCENT CHANCE OF RAIN SHOWERS",
       "MOSTLY CLOUDY WITH A 20 PERCENT CHANCE OF SHOWERS"),
       "PATCHY FOG",
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
       "CLOUDY",
       "A 20 PERCENT CHANCE OF THUNDERSTORMS IN THE AFTERNOON",
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
       "PATCHY FOG IN THE MORNING",
       ("MOSTLY CLOUDY WITH A 20 PERCENT CHANCE OF RAIN SHOWERS",
       "MOSTLY CLOUDY WITH A 20 PERCENT CHANCE OF SHOWERS"),
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
       "PATCHY FOG IN THE MORNING",
       ("MOSTLY CLOUDY WITH SCATTERED RAIN SHOWERS","MOSTLY CLOUDY WITH SCATTERED SHOWERS"),
       "CHANCE OF SHOWERS 50 PERCENT",
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
       ("MOSTLY CLOUDY WITH SCATTERED RAIN SHOWERS", "MOSTLY CLOUDY WITH SCATTERED SHOWERS"),
       "PATCHY FOG",
       "CHANCE OF SHOWERS 50 PERCENT",
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
       "MOSTLY CLOUDY",
       "PATCHY FOG IN THE MORNING",
       ("SLIGHT CHANCE OF SHOWERS IN THE MORNING...THEN WIDESPREAD SNOW IN THE AFTERNOON",
        "SLIGHT CHANCE OF RAIN SHOWERS IN THE MORNING...THEN WIDESPREAD SNOW IN THE AFTERNOON"),
       "CHANCE OF PRECIPITATION 50 PERCENT",
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
       "MOSTLY CLOUDY",
       "PATCHY FOG IN THE MORNING",
       ("SLIGHT CHANCE OF SHOWERS IN THE MORNING...THEN CHANCE OF SNOW IN THE AFTERNOON",
        "SLIGHT CHANCE OF RAIN SHOWERS IN THE MORNING...THEN CHANCE OF SNOW IN THE AFTERNOON"), 
       "CHANCE OF PRECIPITATION 50 PERCENT",
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
       "SUNNY EARLY IN THE MORNING THEN BECOMING MOSTLY CLOUDY",
       ("SLIGHT CHANCE OF SHOWERS EARLY IN THE MORNING...THEN SLIGHT CHANCE OF SNOW LATE IN THE MORNING",
        "SLIGHT CHANCE OF RAIN SHOWERS EARLY IN THE MORNING...THEN SLIGHT CHANCE OF SNOW LATE IN THE MORNING"),
       "CHANCE OF THUNDERSTORMS IN THE AFTERNOON",
       "CHANCE OF PRECIPITATION 50 PERCENT",
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
       "MOSTLY CLOUDY",
       "PATCHY FOG EARLY IN THE MORNING",
       ("SLIGHT CHANCE OF SHOWERS EARLY IN THE MORNING...THEN SLIGHT CHANCE OF SNOW LATE IN THE MORNING",
        "SLIGHT CHANCE OF RAIN SHOWERS EARLY IN THE MORNING...THEN SLIGHT CHANCE OF SNOW LATE IN THE MORNING"),
       "CHANCE OF THUNDERSTORMS IN THE AFTERNOON",
       "CHANCE OF PRECIPITATION 50 PERCENT",
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
       "CLOUDY",
       "WIDESPREAD RAIN IN THE MORNING...THEN WIDESPREAD RAIN AND CHANCE OF THUNDERSTORMS IN THE AFTERNOON",
       "SOME THUNDERSTORMS MAY BE SEVERE",
       "CHANCE OF PRECIPITATION 50 PERCENT",
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
       "CLOUDY WITH WIDESPREAD RAIN AND CHANCE OF THUNDERSTORMS",
       "SOME THUNDERSTORMS MAY BE SEVERE IN THE AFTERNOON",
       "CHANCE OF PRECIPITATION 50 PERCENT",
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
       "CLOUDY WITH WIDESPREAD RAIN AND CHANCE OF THUNDERSTORMS",
       "SOME THUNDERSTORMS MAY BE SEVERE",
       "CHANCE OF PRECIPITATION 50 PERCENT",
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
       "SCATTERED THUNDERSTORMS IN THE AFTERNOON",
       "SOME THUNDERSTORMS MAY BE SEVERE LATE IN THE AFTERNOON",
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
       "CLOUDY WITH A 50 PERCENT CHANCE OF THUNDERSTORMS", 
       "SOME THUNDERSTORMS MAY BE SEVERE IN THE AFTERNOON",
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
       "CLOUDY WITH WIDESPREAD SNOW...CHANCE OF LIGHT FREEZING RAIN AND LIGHT SLEET",
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
       "WIDESPREAD SNOW WITH POSSIBLE FREEZING RAIN AND SLEET EARLY IN THE MORNING...THEN SNOW LIKELY AND CHANCE OF LIGHT SLEET LATE IN THE MORNING",
       "CHANCE OF RAIN IN THE AFTERNOON",
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
       "WIDESPREAD SNOW...LIGHT SLEET AND OCCASIONAL LIGHT FREEZING RAIN EARLY IN THE MORNING...THEN SNOW LIKELY AND CHANCE OF LIGHT SLEET LATE IN THE MORNING",
       "CHANCE OF RAIN IN THE AFTERNOON",
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
       "SNOW LIKELY AND CHANCE OF LIGHT SLEET EARLY IN THE MORNING...THEN RAIN LIKELY IN THE LATE MORNING AND AFTERNOON",
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
       "WIDESPREAD SNOW WITH POSSIBLE FREEZING RAIN AND SLEET IN THE MORNING...THEN CHANCE OF RAIN AND SLIGHT CHANCE OF THUNDERSTORMS IN THE AFTERNOON",
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
       "RAIN LIKELY EARLY IN THE MORNING",
       ("OCCASIONAL RAIN SHOWERS IN THE AFTERNOON",
        "OCCASIONAL SHOWERS IN THE AFTERNOON"),
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
       "CLOUDY WITH OCCASIONAL RAIN...SNOW AND SLIGHT CHANCE OF LIGHT SLEET"
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
       "SNOW IN THE MORNING...THEN SNOW...SLIGHT CHANCE OF LIGHT FREEZING RAIN AND LIGHT SLEET IN THE AFTERNOON",
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
       "RAIN AND SLEET LIKELY IN THE MORNING...THEN SNOW AND SLEET IN THE AFTERNOON",
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
       "CLOUDY",
       "RAIN LIKELY IN THE MORNING...THEN RAIN AND SNOW LIKELY IN THE AFTERNOON",
       "CHANCE OF PRECIPITATION 50 PERCENT",
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
       "CLOUDY WITH RAIN LIKELY AND SLIGHT CHANCE OF THUNDERSTORMS",
       "CHANCE OF PRECIPITATION 50 PERCENT",
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
       "CLOUDY WITH A 50 PERCENT CHANCE OF RAIN AND SNOW",
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
       "CLOUDY",
       ("NUMEROUS SHOWERS AND THUNDERSTORMS IN THE MORNING...THEN ISOLATED RAIN SHOWERS IN THE AFTERNOON",
        "NUMEROUS SHOWERS AND THUNDERSTORMS IN THE MORNING...THEN ISOLATED SHOWERS IN THE AFTERNOON"),
       "LOCALLY HEAVY RAINFALL POSSIBLE IN THE MORNING", 
       "CHANCE OF PRECIPITATION 50 PERCENT",
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
       "CLOUDY",
       ("NUMEROUS THUNDERSTORMS IN THE MORNING...THEN ISOLATED RAIN SHOWERS IN THE AFTERNOON",
        "NUMEROUS THUNDERSTORMS IN THE MORNING...THEN ISOLATED SHOWERS IN THE AFTERNOON"),
       "THUNDERSTORMS MAY BE SEVERE WITH LARGE HAIL", 
       "CHANCE OF PRECIPITATION 50 PERCENT",
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
       "CLOUDY",
       ("NUMEROUS SNOW SHOWERS WITH POSSIBLE SLEET AND THUNDERSTORMS EARLY IN THE MORNING...THEN ISOLATED RAIN SHOWERS IN THE LATE MORNING AND AFTERNOON",
        "NUMEROUS SNOW SHOWERS WITH POSSIBLE SLEET AND THUNDERSTORMS EARLY IN THE MORNING...THEN ISOLATED SHOWERS IN THE LATE MORNING AND AFTERNOON"),
       "CHANCE OF PRECIPITATION 50 PERCENT",
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
       "SNOW LIKELY...SLIGHT CHANCE OF LIGHT FREEZING DRIZZLE AND LIGHT SLEET EARLY IN THE MORNING...THEN ISOLATED SHOWERS IN THE LATE MORNING AND AFTERNOON",
       "SNOW MAY BE HEAVY AT TIMES EARLY IN THE MORNING",
       "CHANCE OF PRECIPITATION 50 PERCENT",
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
       "OCCASIONAL SNOW IN THE MORNING.",
       "CHANCE OF LIGHT SLEET AND SLIGHT CHANCE OF LIGHT FREEZING DRIZZLE THROUGH THE DAY",
       "CHANCE OF RAIN AND SNOW IN THE AFTERNOON.",
       "SNOW MAY BE HEAVY AT TIMES IN THE MORNING",
       "CHANCE OF PRECIPITATION 50 PERCENT",
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
       "SLIGHT CHANCE OF THUNDERSTORMS IN THE MORNING...THEN CHANCE OF THUNDERSTORMS IN THE AFTERNOON",
       "CHANCE OF THUNDERSTORMS 50 PERCENT",
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
       "SLIGHT CHANCE OF THUNDERSTORMS IN THE MORNING...THEN CHANCE OF THUNDERSTORMS IN THE AFTERNOON",
       "SOME THUNDERSTORMS MAY PRODUCE HEAVY RAINFALL IN THE MORNING",
       "CHANCE OF THUNDERSTORMS 50 PERCENT",
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
       "CLOUDY",
       "SLIGHT CHANCE OF THUNDERSTORMS IN THE MORNING...THEN CHANCE OF THUNDERSTORMS IN THE AFTERNOON",
       "SOME THUNDERSTORMS MAY BE SEVERE IN THE MORNING",
       "CHANCE OF THUNDERSTORMS 50 PERCENT",
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
       "CLOUDY",
       "A 50 PERCENT CHANCE OF THUNDERSTORMS IN THE MORNING",
       "SOME THUNDERSTORMS MAY BE SEVERE",
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
       "CHANCE OF RAIN...THUNDERSTORMS AND SNOW IN THE MORNING",
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
        "CHANCE OF RAIN...THUNDERSTORMS AND SNOW IN THE MORNING...THEN WIDESPREAD RAIN...THUNDERSTORMS AND SNOW IN THE AFTERNOON",
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
       "CLOUDY", "PRECIPITATION MAY BE HEAVY AT TIMES", 
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
       "CLOUDY", "LOCALLY HEAVY RAINFALL POSSIBLE IN THE MORNING.",
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
       "LIGHT WINDS",
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
       "LIGHT WINDS",
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
       "NORTH WINDS AROUND 10 MPH WITH GUSTS TO AROUND 35 MPH",
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
       "LIGHT WINDS BECOMING NORTH AROUND 15 MPH IN THE AFTERNOON",
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
       "LIGHT WINDS BECOMING NORTH AROUND 15 MPH IN THE AFTERNOON",
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
       "LIGHT WINDS BECOMING NORTH AROUND 15 MPH WITH GUSTS TO AROUND 35 MPH IN THE AFTERNOON",
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
    "LIGHT WINDS BECOMING NORTH AROUND 15 MPH IN THE AFTERNOON",
    "GUSTS UP TO 35 MPH",
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
       "NORTH WINDS AROUND 15 MPH IN THE MORNING BECOMING LIGHT",
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
       "NORTH WINDS AROUND 15 MPH WITH GUSTS TO AROUND 35 MPH IN THE MORNING BECOMING LIGHT",
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
       "NORTH WINDS AROUND 15 MPH IN THE MORNING BECOMING LIGHT",
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
       "NORTH WINDS AROUND 15 MPH IN THE MORNING BECOMING LIGHT",
       "GUSTS UP TO 35 MPH",
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
       "LIGHT WINDS",
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
       "NORTH WINDS AROUND 10 MPH EARLY IN THE MORNING BECOMING LIGHT",
       "GUSTS UP TO 35 MPH",
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
       "LIGHT WINDS BECOMING NORTH AROUND 10 MPH LATE IN THE MORNING...THEN BECOMING LIGHT IN THE AFTERNOON",
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
       "BREEZY",
       "NORTH WINDS AROUND 10 MPH EARLY IN THE MORNING BECOMING LIGHT...THEN BECOMING NORTH AROUND 25 MPH EARLY IN THE AFTERNOON BECOMING LIGHT",
       "GUSTS UP TO 45 MPH",
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
       "BREEZY",
       "NORTH WINDS AROUND 10 MPH EARLY IN THE MORNING BECOMING LIGHT...THEN BECOMING NORTH AROUND 25 MPH LATE IN THE AFTERNOON",
       "GUSTS UP TO 45 MPH",
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
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
       "NORTH WINDS AROUND 10 MPH WITH GUSTS TO AROUND 35 MPH",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
       "NORTH WINDS AROUND 15 MPH IN THE AFTERNOON",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
       "NORTH WINDS AROUND 15 MPH IN THE AFTERNOON",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
       "NORTH WINDS AROUND 15 MPH WITH GUSTS TO AROUND 35 MPH IN THE AFTERNOO",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
    "NORTH WINDS AROUND 15 MPH IN THE AFTERNOON",
    "GUSTS UP TO 35 MPH",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
       "NORTH WINDS AROUND 15 MPH IN THE MORNING",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
       "NORTH WINDS AROUND 15 MPH WITH GUSTS TO AROUND 35 MPH IN THE MORNING",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
       "NORTH WINDS AROUND 15 MPH IN THE MORNING",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
       "NORTH WINDS AROUND 15 MPH IN THE MORNING. GUSTS UP TO 35 MPH",
       "GUSTS UP TO 35 MPH",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
       "NORTH WINDS AROUND 10 MPH EARLY IN THE MORNING",
       "GUSTS UP TO 35 MPH",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
       "NORTH WINDS AROUND 10 MPH LATE IN THE MORNING",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
       "BREEZY",
       "NORTH WINDS AROUND 10 MPH EARLY IN THE MORNING BECOMING NORTH AROUND 25 MPH EARLY IN THE AFTERNOON",
       "GUSTS UP TO 45 MPH",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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
       "BREEZY",
       "NORTH WINDS AROUND 10 MPH EARLY IN THE MORNING BECOMING NORTH AROUND 25 MPH LATE IN THE AFTERNOON",
       "GUSTS UP TO 45 MPH",
       ],
    "fileChanges":[
       ("Phrase_Test_Local", "TextProduct", "replace",
       def1, "undo"),
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

