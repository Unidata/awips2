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
tzdef = """
AreaDictionary['FLZ050']['ugcTimeZone'] = 'EST5EDT'
AreaDictionary['FLZ052']['ugcTimeZone'] = 'EST5'
AreaDictionary['FLZ151']['ugcTimeZone'] = 'America/Puerto_Rico'
AreaDictionary['FLZ155']['ugcTimeZone'] = 'CST6CDT'

"""

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
       ("Fcst", "Hazards", "DISCRETE", 13, 18, "HT.Y", ["FLZ050","FLZ142"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 011000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "500 AM EST Fri Jan 1 2010",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ050-142-011800-",
      "/O.NEW.KTBW.HT.Y.0001.100101T1300Z-100101T1800Z/",
      "Pinellas-Coastal Citrus-",
      "500 AM EST Fri Jan 1 2010",
      "...HEAT ADVISORY IN EFFECT FROM 8 AM THIS MORNING TO 1 PM EST THIS AFTERNOON...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Heat Advisory...which is in effect from 8 AM this morning to 1 PM EST this afternoon.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "A Heat Advisory means that a period of hot temperatures is expected. The combination of hot temperatures and high humidity will combine to create a situation in which heat illnesses are possible. Drink plenty of fluids...stay in an air-conditioned room...stay out of the sun...and check up on relatives and neighbors.",
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
      "National Weather Service Tampa Bay Ruskin FL",
      "500 AM EST Fri Jan 1 2010",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ050-052-011800-",
      "/O.NEW.KTBW.HT.Y.0001.100101T1300Z-100101T1800Z/",
      "Pinellas-Polk-",
      "500 AM EST Fri Jan 1 2010",
      "...HEAT ADVISORY IN EFFECT FROM 8 AM THIS MORNING TO 1 PM EST THIS AFTERNOON...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Heat Advisory...which is in effect from 8 AM this morning to 1 PM EST this afternoon.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "A Heat Advisory means that a period of hot temperatures is expected. The combination of hot temperatures and high humidity will combine to create a situation in which heat illnesses are possible. Drink plenty of fluids...stay in an air-conditioned room...stay out of the sun...and check up on relatives and neighbors.",
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
       "National Weather Service Tampa Bay Ruskin FL",
       "600 AM EDT Thu Jul 1 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ050-052-011800-",
       "/O.NEW.KTBW.HT.Y.0001.100701T1300Z-100701T1800Z/",
       "Pinellas-Polk-",
       "600 AM EDT Thu Jul 1 2010 /500 AM EST Thu Jul 1 2010/",
       "...HEAT ADVISORY IN EFFECT FROM 9 AM EDT /8 AM EST/ THIS MORNING TO 2 PM EDT /1 PM EST/ THIS AFTERNOON...",
       "The National Weather Service in Tampa Bay Ruskin has issued a Heat Advisory...which is in effect from 9 AM EDT /8 AM EST/ this morning to 2 PM EDT /1 PM EST/ this afternoon.",
#        "|* SEGMENT TEXT GOES HERE *|.",
       "A Heat Advisory means that a period of hot temperatures is expected. The combination of hot temperatures and high humidity will combine to create a situation in which heat illnesses are possible. Drink plenty of fluids...stay in an air-conditioned room...stay out of the sun...and check up on relatives and neighbors.",
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
       ("Fcst", "Hazards", "DISCRETE", 13, 18, "HT.Y", ["FLZ050","FLZ155"]),
       ],
    "checkStrings": [
       "WWUS72 KTBW 011000 NPWTBW",
       "URGENT - WEATHER MESSAGE",
       "National Weather Service Tampa Bay Ruskin FL",
       "500 AM EST Fri Jan 1 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ050-155-011800-",
       "/O.NEW.KTBW.HT.Y.0001.100101T1300Z-100101T1800Z/",
       "Pinellas-Coastal Manatee-",
       "500 AM EST Fri Jan 1 2010 /400 AM CST Fri Jan 1 2010/",
       "...HEAT ADVISORY IN EFFECT FROM 8 AM EST /7 AM CST/ THIS MORNING TO 1 PM EST /NOON CST/ THIS AFTERNOON...",
       "The National Weather Service in Tampa Bay Ruskin has issued a Heat Advisory...which is in effect from 8 AM EST /7 AM CST/ this morning to 1 PM EST /noon CST/ this afternoon.",
#        "|* SEGMENT TEXT GOES HERE *|.",
       "A Heat Advisory means that a period of hot temperatures is expected. The combination of hot temperatures and high humidity will combine to create a situation in which heat illnesses are possible. Drink plenty of fluids...stay in an air-conditioned room...stay out of the sun...and check up on relatives and neighbors.",
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
       ("Fcst", "Hazards", "DISCRETE", 13, 18, "HT.Y", ["FLZ052","FLZ155"]),
       ],
    "checkStrings": [
       "WWUS72 KTBW 011000",
       "NPWTBW",
       "URGENT - WEATHER MESSAGE",
       "National Weather Service Tampa Bay Ruskin FL",
       "600 AM EDT Thu Jul 1 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ052-155-011800-",
       "/O.NEW.KTBW.HT.Y.0001.100701T1300Z-100701T1800Z/",
       "Polk-Coastal Manatee-",
#        "INCLUDING THE CITIES OF...LAKELAND...WINTER HAVEN...BRADENTON...",
#        "BAYSHORE GARDENS...PALMETTO",
       "500 AM EST Thu Jul 1 2010 /500 AM CDT Thu Jul 1 2010/",
       "...HEAT ADVISORY IN EFFECT FROM 8 AM EST /8 AM CDT/ THIS MORNING TO 1 PM EST /1 PM CDT/ THIS AFTERNOON...",
       "The National Weather Service in Tampa Bay Ruskin has issued a Heat Advisory...which is in effect from 8 AM EST /8 AM CDT/ this morning to 1 PM EST /1 PM CDT/ this afternoon.",
#        "|* SEGMENT TEXT GOES HERE *|.",
       "A Heat Advisory means that a period of hot temperatures is expected. The combination of hot temperatures and high humidity will combine to create a situation in which heat illnesses are possible. Drink plenty of fluids...stay in an air-conditioned room...stay out of the sun...and check up on relatives and neighbors.",
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
       ("Fcst", "Hazards", "DISCRETE", 13, 18, "HT.Y", ["FLZ050","FLZ151","FLZ052","FLZ155"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 011000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "600 AM EDT Thu Jul 1 2010",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ050-052-151-155-011800-",
      "/O.NEW.KTBW.HT.Y.0001.100701T1300Z-100701T1800Z/",
      "Pinellas-Polk-Coastal Hillsborough-Coastal Manatee-",
      "600 AM EDT Thu Jul 1 2010 /500 AM EST Thu Jul 1 2010/ /600 AM AST Thu Jul 1 2010/ /500 AM CDT Thu Jul 1 2010/",
      "...HEAT ADVISORY IN EFFECT FROM 9 AM EDT /8 AM EST/ /9 AM AST/ /8 AM CDT/ THIS MORNING TO 2 PM EDT /1 PM EST/ /2 PM AST/ /1 PM CDT/ THIS AFTERNOON...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Heat Advisory...which is in effect from 9 AM EDT /8 AM EST/ /9 AM AST/ /8 AM CDT/ this morning to 2 PM EDT /1 PM EST/ /2 PM AST/ /1 PM CDT/ this afternoon.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "A Heat Advisory means that a period of hot temperatures is expected. The combination of hot temperatures and high humidity will combine to create a situation in which heat illnesses are possible. Drink plenty of fluids...stay in an air-conditioned room...stay out of the sun...and check up on relatives and neighbors.",
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
       ("Fcst", "Hazards", "DISCRETE", 13, 18, "HT.Y", ["FLZ050","FLZ151","FLZ052","FLZ155"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 011000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "500 AM EST Fri Jan 1 2010",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ050-052-151-155-011800-",
      "/O.NEW.KTBW.HT.Y.0001.100101T1300Z-100101T1800Z/",
      "Pinellas-Polk-Coastal Hillsborough-Coastal Manatee-",
      "500 AM EST Fri Jan 1 2010 /600 AM AST Fri Jan 1 2010/ /400 AM CST Fri Jan 1 2010/",
      "...HEAT ADVISORY IN EFFECT FROM 8 AM EST /9 AM AST/ /7 AM CST/ THIS MORNING TO 1 PM EST /2 PM AST/ /NOON CST/ THIS AFTERNOON...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Heat Advisory...which is in effect from 8 AM EST /9 AM AST/ /7 AM CST/ this morning to 1 PM EST /2 PM AST/ /noon CST/ this afternoon.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "A Heat Advisory means that a period of hot temperatures is expected. The combination of hot temperatures and high humidity will combine to create a situation in which heat illnesses are possible. Drink plenty of fluids...stay in an air-conditioned room...stay out of the sun...and check up on relatives and neighbors.",
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


