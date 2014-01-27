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
      SUNNY UNTIL LATE AFTERNOON THEN BECOMING PARTLY SUNNY.
      A 20 PERCENT CHANCE OF THUNDERSTORMS LATE IN THE AFTERNOON""",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 9, 10, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 9, 20, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 9, "NoWx", "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 55, "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12, "SChc:T:<NoInten>:<NoVis>:", "all"),
       ],
    "checkStrings": [
            "SUNNY UNTIL LATE AFTERNOON THEN BECOMING PARTLY SUNNY.",
            "A 20 PERCENT CHANCE OF THUNDERSTORMS LATE IN THE AFTERNOON"
            ],
    },
    {
    "name": "A2",
    "commentary": """Phrase Test:
      SHOWERS AND THUNDERSTORMS IN THE MORNING...THEN MOSTLY SUNNY EARLY IN THE AFTERNOON
      MOSTLY SUNNY WITH SLIGHT CHANCE OF THUNDERSTORMS LATE IN THE AFTERNOON
      LOCALLY HEAVY RAINFALL POSSIBLE IN THE MORNING""",
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
       "SHOWERS AND THUNDERSTORMS IN THE MORNING...THEN MOSTLY SUNNY EARLY IN THE AFTERNOON",
       "MOSTLY SUNNY WITH SLIGHT CHANCE OF THUNDERSTORMS LATE IN THE AFTERNOON",
       "LOCALLY HEAVY RAINFALL POSSIBLE IN THE MORNING",
       ],
    },
    {
    "name": "A3",
    "commentary": """Phrase Test:
      SUNNY""",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 12, 0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  20, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12,  "NoWx", "all"),
       ],
    "checkStrings": [
       "SUNNY",
       ],
    },
    {
    "name": "A4",
    "commentary": """Phrase Test:
      MOSTLY SUNNY
      SCATTERED THUNDERSTORMS IN THE LATE MORNING AND AFTERNOON
      CHANCE OF THUNDERSTORMS 30 PERCENT""",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 3, 10, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 3,  40, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 3,  "NoWx", "all"),
       ("Fcst", "PoP", "SCALAR", 3, 12, 30, "all"),
       ("Fcst", "Sky", "SCALAR", 3, 12, 60, "all"),
       ("Fcst", "Wx", "WEATHER", 3, 12, "Sct:T:<NoInten>:<NoVis>:^Sct:RW:-:<NoVis>:", "all"),
       ],
    "checkStrings": [
       "MOSTLY SUNNY",
       "SCATTERED THUNDERSTORMS IN THE LATE MORNING AND AFTERNOON",
       "CHANCE OF THUNDERSTORMS 30 PERCENT",
       ],
    },
    {
    "name": "A5",
    "commentary": """Phrase Test:
      SUNNY IN THE MORNING...THEN PARTLY SUNNY WITH SCATTERED THUNDERSTORMS IN THE AFTERNOON
      THUNDERSTORMS MAY PRODUCE HEAVY RAINFALL AND FREQUENT LIGHTNING IN THE AFTERNOON
      CHANCE OF THUNDERSTORMS 50 PERCENT""",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 6, 10, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 6,  25, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6,  "NoWx", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 12, 50, "all"),
       ("Fcst", "Sky", "SCALAR", 6, 12, 60, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Sct:T:<NoInten>:<NoVis>:FL,HvyRn", "all"),
       ],
    "checkStrings": [
       "SUNNY IN THE MORNING...THEN PARTLY SUNNY WITH SCATTERED THUNDERSTORMS IN THE AFTERNOON",
       "THUNDERSTORMS MAY PRODUCE HEAVY RAINFALL AND FREQUENT LIGHTNING IN THE AFTERNOON",
       "CHANCE OF THUNDERSTORMS 50 PERCENT"
       ],
    },
    {
    "name": "B1",
    "commentary": """Phrase Test:
      MOSTLY CLOUDY WITH A 30 PERCENT CHANCE OF RAIN IN THE MORNING...THEN MOSTLY SUNNY IN THE AFTERNOON
      WINDY
      TEMPERATURES FALLING INTO THE MID 50S IN THE AFTERNOON
      SOUTHWEST WINDS AROUND 10 MPH INCREASING TO NORTH AROUND 30 MPH IN THE AFTERNOON""",
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
       "MOSTLY CLOUDY WITH A 30 PERCENT CHANCE OF RAIN IN THE MORNING...THEN MOSTLY SUNNY IN THE AFTERNOON",
       "WINDY",
       "TEMPERATURES FALLING INTO THE MID 50S IN THE AFTERNOON",
       "SOUTHWEST WINDS AROUND 10 MPH INCREASING TO NORTH AROUND 30 MPH IN THE AFTERNOON",
       ],
    },
    {
    "name": "B2",
    "commentary": """Phrase Test:
      THUNDERSTORMS
      LOCALLY HEAVY RAINFALL POSSIBLE
      HURRICANE FORCE WINDS
      HIGHS AROUND 80
      SOUTHEAST WINDS AROUND 70 MPH WITH GUSTS TO AROUND 130 MPH BECOMING SOUTH AND INCREASING TO AROUND 90 MPH IN THE AFTERNOON""",
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
       "THUNDERSTORMS",
       "LOCALLY HEAVY RAINFALL POSSIBLE",
       "HURRICANE FORCE WINDS",
       "HIGHS AROUND 80",
       "SOUTHEAST WINDS AROUND 70 MPH WITH GUSTS TO AROUND 130 MPH BECOMING SOUTH AND INCREASING TO AROUND 90 MPH IN THE AFTERNOON",
       ],
    },
    {
    "name": "B3",
    "commentary": """Phrase Test:
      SUNNY
      LIGHT WINDS
      WIDESPREAD FROST""",
    "createGrids": [
       ("Fcst", "PoP",  "SCALAR", 0, 12, 0, "all"),
       ("Fcst", "Sky",  "SCALAR", 0, 12,  10, "all"),
       ("Fcst", "Wx",   "WEATHER", 0, 12,  "Wide:FR:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "Wind", "VECTOR", 0, 12,  (0,"SW"), "all"),
       ],
    "checkStrings": [
       "SUNNY",
       "LIGHT WINDS",
       "WIDESPREAD FROST",
       ],
    },
    {
    "name": "C1",
    "commentary": """Phrase Test:
       MOSTLY CLOUDY
       BREEZY
       NORTH WINDS AROUND 25 MPH
       WIND CHILL READINGS 26 BELOW TO 36 BELOW ZERO""",
    "createGrids": [
       ("Fcst", "PoP",  "SCALAR", 0, 12, 10, "all"),
       ("Fcst", "Sky",  "SCALAR", 0, 12,  80, "all"),
       ("Fcst", "Wx",   "WEATHER", 0, 12,  "NoWx", "all"),
       ("Fcst", "Wind", "VECTOR", 0, 12,  (20,"N"), "all"),
       ("Fcst", "WindChill", "SCALAR", 0, 12, -36 , "all"),
       ],
    "checkStrings": [
       "MOSTLY CLOUDY",
       "BREEZY",
       "NORTH WINDS AROUND 25 MPH",
       "WIND CHILL READINGS 26 BELOW TO 36 BELOW ZERO",
       ],
    },
    {
    "name": "C2",  # Heavy Snow
    "commentary": """Phrase Test: Heavy Snow
       SNOW LIKELY IN THE MORNING...THEN SNOW IN THE AFTERNOON
       SNOW MAY BE HEAVY AT TIMES IN THE AFTERNOON""",
    "createGrids": [
       ("Fcst", "PoP",  "SCALAR", 0, 6, 80, "all"),
       ("Fcst", "Sky",  "SCALAR", 0, 6,  80, "all"),
       ("Fcst", "Wx",   "WEATHER", 0, 6,  "Lkly:S:m:<NoVis>:", "all"),
       ("Fcst", "PoP",  "SCALAR", 6, 12, 100, "all"),
       ("Fcst", "Sky",  "SCALAR", 6, 12, 100, "all"),
       ("Fcst", "Wx",  "WEATHER", 6, 12, "Def:S:+:1/4SM:", "all"),
       ],
    "checkStrings": [
       "SNOW LIKELY IN THE MORNING...THEN SNOW IN THE AFTERNOON",
       "SNOW MAY BE HEAVY AT TIMES IN THE AFTERNOON",
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




