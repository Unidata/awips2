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
# Multiple time zone tests
#
# Author: mathewson
# ----------------------------------------------------------------------------


#time zone definitions for AreaDictionary
tzdef = ["AreaDictionary['FLZ050']['ugcTimeZone'] = 'EST5EDT'",
"AreaDictionary['FLZ051']['ugcTimeZone'] = 'America/Puerto_Rico'",
"AreaDictionary['FLZ052']['ugcTimeZone'] = 'EST5'",
"AreaDictionary['FLZ055']['ugcTimeZone'] = 'CST6CDT'"
]

scripts = [

    {
    "commentary": "Deleting hazard grids.",
    "name": "MultipleTZ_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

#same time zone both zones
    {
    "commentary": "Hazard with same time zone in two zones - EST5EDT",
    "name": "MultipleTZ_1",
    "drtTime": "20100101_1000",
    "gridsStartTime": "20100101_0000",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 13, 18, "HT.Y", ["FLZ050","FLZ042"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 011000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "500 AM EST FRI JAN 1 2010",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ042-050-011800-",
      "/O.NEW.KTBW.HT.Y.0001.100101T1300Z-100101T1800Z/",
      "CITRUS-PINELLAS-",
      "500 AM EST FRI JAN 1 2010",
      "...HEAT ADVISORY IN EFFECT FROM 8 AM THIS MORNING TO 1 PM EST THIS AFTERNOON...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HEAT ADVISORY...WHICH IS IN EFFECT FROM 8 AM THIS MORNING TO 1 PM EST THIS AFTERNOON.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "A HEAT ADVISORY MEANS THAT A PERIOD OF HOT TEMPERATURES IS EXPECTED. THE COMBINATION OF HOT TEMPERATURES AND HIGH HUMIDITY WILL COMBINE TO CREATE A SITUATION IN WHICH HEAT ILLNESSES ARE POSSIBLE. DRINK PLENTY OF FLUIDS...STAY IN AN AIR-CONDITIONED ROOM...STAY OUT OF THE SUN...AND CHECK UP ON RELATIVES AND NEIGHBORS.",
      "$$",
       ],
    },

    {
    "commentary": "Hazard with same effective time zone -winter EST5,EST5EDT, with actual time zones of EST5EDT and EST5",
    "name": "MultipleTZ_2",
    "drtTime": "20100101_1000",
    "gridsStartTime": "20100101_0000",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 13, 18, "HT.Y", ["FLZ050","FLZ052"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 011000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "500 AM EST FRI JAN 1 2010",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ050-052-011800-",
      "/O.NEW.KTBW.HT.Y.0001.100101T1300Z-100101T1800Z/",
      "PINELLAS-POLK-",
      "500 AM EST FRI JAN 1 2010",
      "...HEAT ADVISORY IN EFFECT FROM 8 AM THIS MORNING TO 1 PM EST THIS AFTERNOON...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HEAT ADVISORY...WHICH IS IN EFFECT FROM 8 AM THIS MORNING TO 1 PM EST THIS AFTERNOON.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "A HEAT ADVISORY MEANS THAT A PERIOD OF HOT TEMPERATURES IS EXPECTED. THE COMBINATION OF HOT TEMPERATURES AND HIGH HUMIDITY WILL COMBINE TO CREATE A SITUATION IN WHICH HEAT ILLNESSES ARE POSSIBLE. DRINK PLENTY OF FLUIDS...STAY IN AN AIR-CONDITIONED ROOM...STAY OUT OF THE SUN...AND CHECK UP ON RELATIVES AND NEIGHBORS.",
      "$$",
       ],
    },


    {
    "commentary": "Hazard with diff effective time zone -summer EST5,EST5EDT, with actual time zones of EST5, EST5EDT",
    "name": "MultipleTZ_3",
    "drtTime": "20100701_1000",
    "gridsStartTime": "20100701_0000",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 13, 18, "HT.Y", ["FLZ050","FLZ052"]),
       ],
    "checkStrings": [
       "WWUS72 KTBW 011000",
       "NPWTBW",
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "600 AM EDT THU JUL 1 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ050-052-011800-",
       "/O.NEW.KTBW.HT.Y.0001.100701T1300Z-100701T1800Z/",
       "PINELLAS-POLK-",
       "600 AM EDT THU JUL 1 2010 /500 AM EST THU JUL 1 2010/",
       "...HEAT ADVISORY IN EFFECT FROM 9 AM EDT /8 AM EST/ THIS MORNING TO 2 PM EDT /1 PM EST/ THIS AFTERNOON...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HEAT ADVISORY...WHICH IS IN EFFECT FROM 9 AM EDT /8 AM EST/ THIS MORNING TO 2 PM EDT /1 PM EST/ THIS AFTERNOON.",
       "|* SEGMENT TEXT GOES HERE *|.",
       "A HEAT ADVISORY MEANS THAT A PERIOD OF HOT TEMPERATURES IS EXPECTED. THE COMBINATION OF HOT TEMPERATURES AND HIGH HUMIDITY WILL COMBINE TO CREATE A SITUATION IN WHICH HEAT ILLNESSES ARE POSSIBLE. DRINK PLENTY OF FLUIDS...STAY IN AN AIR-CONDITIONED ROOM...STAY OUT OF THE SUN...AND CHECK UP ON RELATIVES AND NEIGHBORS.",
       "$$",
       ],
    },


    {
    "commentary": "Hazard with different time zones-winter EST5EDT,CST6CDT",
    "name": "MultipleTZ_4",
    "drtTime": "20100101_1000",
    "gridsStartTime": "20100101_0000",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 13, 18, "HT.Y", ["FLZ050","FLZ055"]),
       ],
    "checkStrings": [
       "WWUS72 KTBW 011000 NPWTBW",
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "500 AM EST FRI JAN 1 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ050-055-011800-",
       "/O.NEW.KTBW.HT.Y.0001.100101T1300Z-100101T1800Z/",
       "PINELLAS-MANATEE-",
       "500 AM EST FRI JAN 1 2010 /400 AM CST FRI JAN 1 2010/",
       "...HEAT ADVISORY IN EFFECT FROM 8 AM EST /7 AM CST/ THIS MORNING TO 1 PM EST /NOON CST/ THIS AFTERNOON...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HEAT ADVISORY...WHICH IS IN EFFECT FROM 8 AM EST /7 AM CST/ THIS MORNING TO 1 PM EST /NOON CST/ THIS AFTERNOON.",
       "|* SEGMENT TEXT GOES HERE *|.",
       "A HEAT ADVISORY MEANS THAT A PERIOD OF HOT TEMPERATURES IS EXPECTED. THE COMBINATION OF HOT TEMPERATURES AND HIGH HUMIDITY WILL COMBINE TO CREATE A SITUATION IN WHICH HEAT ILLNESSES ARE POSSIBLE. DRINK PLENTY OF FLUIDS...STAY IN AN AIR-CONDITIONED ROOM...STAY OUT OF THE SUN...AND CHECK UP ON RELATIVES AND NEIGHBORS.",
       "$$",
       ],
    },

    {
    "commentary": "Hazard with same effective time zone-summer EST4,CST5CDT",
    "name": "MultipleTZ_5",
    "drtTime": "20100701_1000",
    "gridsStartTime": "20100701_0000",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 13, 18, "HT.Y", ["FLZ052","FLZ055"]),
       ],
    "checkStrings": [
       "WWUS72 KTBW 011000",
       "NPWTBW",
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "600 AM EDT THU JUL 1 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ052-055-011800-",
       "/O.NEW.KTBW.HT.Y.0001.100701T1300Z-100701T1800Z/",
       "POLK-MANATEE-",
       "INCLUDING THE CITIES OF...LAKELAND...WINTER HAVEN...BRADENTON...",
       "BAYSHORE GARDENS...PALMETTO",
       "500 AM EST THU JUL 1 2010 /500 AM CDT THU JUL 1 2010/",
       "...HEAT ADVISORY IN EFFECT FROM 8 AM EST /8 AM CDT/ THIS MORNING TO 1 PM EST /1 PM CDT/ THIS AFTERNOON...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HEAT ADVISORY...WHICH IS IN EFFECT FROM 8 AM EST /8 AM CDT/ THIS MORNING TO 1 PM EST /1 PM CDT/ THIS AFTERNOON.",
       "|* SEGMENT TEXT GOES HERE *|.",
       "A HEAT ADVISORY MEANS THAT A PERIOD OF HOT TEMPERATURES IS EXPECTED. THE COMBINATION OF HOT TEMPERATURES AND HIGH HUMIDITY WILL COMBINE TO CREATE A SITUATION IN WHICH HEAT ILLNESSES ARE POSSIBLE. DRINK PLENTY OF FLUIDS...STAY IN AN AIR-CONDITIONED ROOM...STAY OUT OF THE SUN...AND CHECK UP ON RELATIVES AND NEIGHBORS.",
       "$$",
       ],
    },


    {
    "commentary": "Hazard with multiple time zones-summer",
    "name": "MultipleTZ_6",
    "drtTime": "20100701_1000",
    "gridsStartTime": "20100701_0000",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 13, 18, "HT.Y", ["FLZ050","FLZ051","FLZ052","FLZ055"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 011000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "600 AM EDT THU JUL 1 2010",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ050>052-055-011800-",
      "/O.NEW.KTBW.HT.Y.0001.100701T1300Z-100701T1800Z/",
      "PINELLAS-HILLSBOROUGH-POLK-MANATEE-",
      "600 AM EDT THU JUL 1 2010 /600 AM AST THU JUL 1 2010/ /500 AM EST THU JUL 1 2010/ /500 AM CDT THU JUL 1 2010/",
      "...HEAT ADVISORY IN EFFECT FROM 9 AM EDT /9 AM AST/ /8 AM EST/ /8 AM CDT/ THIS MORNING TO 2 PM EDT /2 PM AST/ /1 PM EST/ /1 PM CDT/ THIS AFTERNOON...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HEAT ADVISORY...WHICH IS IN EFFECT FROM 9 AM EDT /9 AM AST/ /8 AM EST/ /8 AM CDT/ THIS MORNING TO 2 PM EDT /2 PM AST/ /1 PM EST/ /1 PM CDT/ THIS AFTERNOON.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "A HEAT ADVISORY MEANS THAT A PERIOD OF HOT TEMPERATURES IS EXPECTED. THE COMBINATION OF HOT TEMPERATURES AND HIGH HUMIDITY WILL COMBINE TO CREATE A SITUATION IN WHICH HEAT ILLNESSES ARE POSSIBLE. DRINK PLENTY OF FLUIDS...STAY IN AN AIR-CONDITIONED ROOM...STAY OUT OF THE SUN...AND CHECK UP ON RELATIVES AND NEIGHBORS.",
      "$$",
       ],
    },


    {
    "commentary": "Hazard with multiple time zones-winter",
    "name": "MultipleTZ_7",
    "drtTime": "20100101_1000",
    "gridsStartTime": "20100101_0000",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 13, 18, "HT.Y", ["FLZ050","FLZ051","FLZ052","FLZ055"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 011000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "500 AM EST FRI JAN 1 2010",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ050>052-055-011800-",
      "/O.NEW.KTBW.HT.Y.0001.100101T1300Z-100101T1800Z/",
      "PINELLAS-HILLSBOROUGH-POLK-MANATEE-",
      "500 AM EST FRI JAN 1 2010 /600 AM AST FRI JAN 1 2010/ /400 AM CST FRI JAN 1 2010/",
      "...HEAT ADVISORY IN EFFECT FROM 8 AM EST /9 AM AST/ /7 AM CST/ THIS MORNING TO 1 PM EST /2 PM AST/ /NOON CST/ THIS AFTERNOON...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HEAT ADVISORY...WHICH IS IN EFFECT FROM 8 AM EST /9 AM AST/ /7 AM CST/ THIS MORNING TO 1 PM EST /2 PM AST/ /NOON CST/ THIS AFTERNOON.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "A HEAT ADVISORY MEANS THAT A PERIOD OF HOT TEMPERATURES IS EXPECTED. THE COMBINATION OF HOT TEMPERATURES AND HIGH HUMIDITY WILL COMBINE TO CREATE A SITUATION IN WHICH HEAT ILLNESSES ARE POSSIBLE. DRINK PLENTY OF FLUIDS...STAY IN AN AIR-CONDITIONED ROOM...STAY OUT OF THE SUN...AND CHECK UP ON RELATIVES AND NEIGHBORS.",
      "$$",
       ],
    },

    {
    "commentary": "Deleting hazard grids.",
    "name": "MultipleTZ_Cleanup",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    "fileChanges": [],
    },


    ]


import TestScript
def testScript(self, dataMgr, level="Site"):
    defaults = {
        "cmdLineVars": None,
        "comboFlag": 0,
        "vtecDecode": 0,
        "vtecMode": 'O',
        "combinations": None,
        "orderStrings": 1,
        "productType": "Hazard_NPW_Local",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        "fileChanges": [
           ("AreaDictionary", "TextUtility", "add", tzdef, "delete"),
           ],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults, level=level)


