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
# Phrase_Tests for Seasonal scenarios and more
#
# Author:
# ----------------------------------------------------------------------------

# Runs Phrase_Test_Local for each test
scripts = [
    {
    "name": "A1",
    "commentary": """Phrase Test:
      Sunny until late afternoon then becoming partly sunny.
      A 20 percent chance of thunderstorms late in the afternoon""",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 9, 10, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 9, 20, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 9, "NoWx", "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 55, "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12, "SChc:T:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
            "Sunny until late afternoon then becoming partly sunny.",
            "A 20 percent chance of thunderstorms late in the afternoon"
            ],
    },
    {
    "name": "A2",
    "commentary": """Phrase Test:
      showers and thunderstorms in the morning...then mostly sunny early in the afternoon
      Mostly sunny with slight chance of thunderstorms late in the afternoon
      Locally heavy rainfall possible in the morning""",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6,  90, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 9,  10, "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  90, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 9,  30, "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,  "Def:T:<NoInten>:<NoVis>:^Def:RW:+:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 9,  "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12, "SChc:T:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "showers and thunderstorms in the morning...then mostly sunny early in the afternoon",
       "Mostly sunny with slight chance of thunderstorms late in the afternoon",
       "Locally heavy rainfall possible in the morning",
       ],
    },
    {
    "name": "A3",
    "commentary": """Phrase Test:
      Sunny""",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  20, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12,  "NoWx", "all"),
       ],
    "checkStrings": [
       "Sunny",
       ],
    },
    {
    "name": "A4",
    "commentary": """Phrase Test:
      Mostly sunny
      Scattered thunderstorms in the late morning and afternoon
      Chance of thunderstorms 30 percent""",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3, 10, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 3,  40, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3,  "NoWx", "all"),
       ("Fcst", "PoP", "SCALAR", 3, 12, 30, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 12, 60, "all"),
       ("Fcst", "Wx", "WEATHER", 3, 12, "Sct:T:<NoInten>:<NoVis>:^Sct:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "Mostly sunny",
       "Scattered thunderstorms in the late morning and afternoon",
       "Chance of thunderstorms 30 percent",
       ],
    },
    {
    "name": "A5",
    "commentary": """Phrase Test:
      Sunny in the morning...then partly sunny with scattered thunderstorms in the afternoon
      thunderstorms may produce heavy rainfall and frequent lightning in the afternoon
      Chance of thunderstorms 50 percent""",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6, 10, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  25, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,  "NoWx", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 60, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Sct:T:<NoInten>:<NoVis>:FL,HvyRn", "all"),
       ],
    "checkStrings": [
       "Sunny in the morning...then partly sunny with scattered thunderstorms in the afternoon",
       "thunderstorms may produce heavy rainfall and frequent lightning in the afternoon",
       "Chance of thunderstorms 50 percent"
       ],
    },
    {
    "name": "B1",
    "commentary": """Phrase Test:
      Mostly cloudy with a 30 percent chance of rain in the morning...then mostly sunny in the afternoon
      Windy
      Temperatures falling into the mid 50s in the afternoon
      Southwest winds around 10 mph increasing to north around 30 mph in the afternoon""",
    "createGrids": [
       ("Fcst", "PoP",  "SCALAR", 0, 6, 30, "all"),
       ("Fcst", "Sky",  "SCALAR", 0, 6,  70, "all"),
       ("Fcst", "Wx",   "WEATHER", 0, 6,  "Chc:R:-:<NoVis>:", "all"),
       ("Fcst", "T",    "SCALAR", 0, 6,  75, "all"),
       ("Fcst", "MaxT",  "SCALAR", "MaxTBegin", "MaxTEnd",  80, "all"),
       ("Fcst", "Wind", "VECTOR", 0, 6,  (10,"SW"), "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  0, "all"),
       ("Fcst", "PoP",  "SCALAR", 6, 12, 10, "all"),
       ("Fcst", "Sky",  "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Wx",  "WEATHER", 6, 12, "NoWx", "all"),
       ("Fcst", "T",    "SCALAR", 6, 12,  55, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (25,"N"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 12,  0, "all"),
       ],
    "checkStrings": [
       "Mostly cloudy with a 30 percent chance of rain in the morning...then mostly sunny in the afternoon",
       "Windy",
       "Temperatures falling into the mid 50s in the afternoon",
       "Southwest winds around 10 mph increasing to north around 30 mph in the afternoon",
       ],
    },
    {
    "name": "B2",
    "commentary": """Phrase Test:
      thunderstorms
      Locally heavy rainfall possible
      Hurricane force winds
      Highs around 80
      Southeast winds around 70 mph with gusts to around 130 mph becoming south and increasing to around 90 mph in the afternoon""",
    "createGrids": [
       ("Fcst", "PoP",  "SCALAR", 0, 6, 100, "all"),
       ("Fcst", "Sky",  "SCALAR", 0, 6,  90, "all"),
       ("Fcst", "Wx",   "WEATHER", 0, 6,  "Def:T:<NoInten>:<NoVis>:HvyRn^Def:RW:+:<NoVis>:", "all"),
       ("Fcst", "WindGust", "SCALAR", 0, 6,  85, "all"),
       ("Fcst", "Wind", "VECTOR", 0, 6,  (60,"SE"), "all"),
       ("Fcst", "PoP",  "SCALAR", 6, 12, 100, "all"),
       ("Fcst", "Sky",  "SCALAR", 6, 12, 90, "all"),
       ("Fcst", "Wx",  "WEATHER", 6, 12, "Def:T:<NoInten>:<NoVis>:HvyRn^Def:RW:+:<NoVis>:", "all"),
       ("Fcst", "WindGust",    "SCALAR", 0, 6,  115, "all"),
       ("Fcst", "Wind", "VECTOR", 6, 12,  (80,"S"), "all"),
       ],
    "checkStrings": [
       "thunderstorms",
       "Locally heavy rainfall possible",
       "Hurricane force winds",
       "Highs around 80",
       "Southeast winds around 70 mph with gusts to around 130 mph becoming south and increasing to around 90 mph in the afternoon",
       ],
    },
    {
    "name": "B3",
    "commentary": """Phrase Test:
      Sunny
      Light winds
      Widespread frost""",
    "createGrids": [
       ("Fcst", "PoP",  "SCALAR", 0, 12, 0, "all"),
       ("Fcst", "Sky",  "SCALAR", 0, 12,  10, "all"),
       ("Fcst", "Wx",   "WEATHER", 0, 12,  "Wide:FR:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wind", "VECTOR", 0, 12,  (0,"SW"), "all"),
       ],
    "checkStrings": [
       "Sunny",
       "Light winds",
       "Widespread frost",
       ],
    },
    {
    "name": "C1",
    "commentary": """Phrase Test:
       Mostly cloudy
       Breezy
       North winds around 25 mph
       Wind chill readings 26 below to 36 below zero""",
    "createGrids": [
       ("Fcst", "PoP",  "SCALAR", 0, 12, 10, "all"),
       ("Fcst", "Sky",  "SCALAR", 0, 12,  80, "all"),
       ("Fcst", "Wx",   "WEATHER", 0, 12,  "NoWx", "all"),
       ("Fcst", "Wind", "VECTOR", 0, 12,  (20,"N"), "all"),
       ("Fcst", "WindChill", "SCALAR", 0, 12, -36 , "all"),
       ],
    "checkStrings": [
       "Mostly cloudy",
       "Breezy",
       "North winds around 25 mph",
       "Wind chill readings 26 below to 36 below zero",
       ],
    },
    {
    "name": "C2",  # Heavy Snow
    "commentary": """Phrase Test: Heavy Snow
       Snow likely in the morning...then snow in the afternoon
       Snow may be heavy at times in the afternoon""",
    "createGrids": [
       ("Fcst", "PoP",  "SCALAR", 0, 6, 80, "all"),
       ("Fcst", "Sky",  "SCALAR", 0, 6,  80, "all"),
       ("Fcst", "Wx",   "WEATHER", 0, 6,  "Lkly:S:m:<NoVis>:", "all"),
       ("Fcst", "PoP",  "SCALAR", 6, 12, 100, "all"),
       ("Fcst", "Sky",  "SCALAR", 6, 12, 100, "all"),
       ("Fcst", "Wx",  "WEATHER", 6, 12, "Def:S:+:1/4SM:", "all"),
       ],
    "checkStrings": [
       "Snow likely in the morning...then snow in the afternoon",
       "Snow may be heavy at times in the afternoon",
       #"VISIBILITY ONE QUARTER MILE OR LESS AT TIMES IN THE AFTERNOON",
       ],
    },


    {
    "name": "InvWxMessage",  # TK 4627
    "commentary": "Invalid Wx Significant Status Bar Message (tk4627)",
    "createGrids": [
       ("Fcst", "Wx",  "WEATHER", 0, 12, "Sct:T:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "",
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




