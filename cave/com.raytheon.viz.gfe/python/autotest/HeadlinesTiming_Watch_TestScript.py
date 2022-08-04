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
# Headlines Timing
#
# Author:
# ----------------------------------------------------------------------------

scripts = [
    {
    "commentary": "Clear out all Hazards Table and Grids.",
    "name": "HeadlinesTiming_Watch_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },
    {
    "commentary": "Testing midnight issuance - 3 hr event.",
    "name": "HeadlinesTiming_Watch_1",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T0510Z-100101T0800Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect until 3 AM EST early this morning",
                     ],
    },

    {
    "commentary": "Testing midnight issuance - 4-9 hour event",
    "name": "HeadlinesTiming_Watch_2",
    "drtTime": "20100101_0500",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 4, 9, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T0900Z-100101T1400Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT FROM 4 AM TO 9 AM EST THIS MORNING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect from 4 AM to 9 AM EST this morning",
                     ],
    },

    {
    "commentary": "Testing midnight issuance - 4-15 hour event",
    "name": "HeadlinesTiming_Watch_3",
    "drtTime": "20100101_0500",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 4, 15, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T0900Z-100101T2000Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT FROM 4 AM EST EARLY THIS MORNING THROUGH THIS AFTERNOON...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect from 4 AM EST early this morning through this afternoon",
                     ],
    },

    {
    "commentary": "Testing midnight issuance - 12-15 hour event",
    "name": "HeadlinesTiming_Watch_4",
    "drtTime": "20100101_0500",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 12, 15, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T1700Z-100101T2000Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT THIS AFTERNOON...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect this afternoon",
                     ],
    },


    {
    "commentary": "Testing midnight issuance - 13-22 hour event",
    "name": "HeadlinesTiming_Watch_5",
    "drtTime": "20100101_0500",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 13, 22, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T1800Z-100102T0300Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT FROM THIS AFTERNOON THROUGH THIS EVENING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect from this afternoon through this evening",
                     ],
    },
    {
    "commentary": "Testing midnight issuance - 0-25 hour event",
    "name": "HeadlinesTiming_Watch_6",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 25, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T0510Z-100102T0600Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT THROUGH LATE TONIGHT...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect through late tonight",
                     ],
    },
    {
    "commentary": "Testing midnight issuance - 0-31 hour event",
    "name": "HeadlinesTiming_Watch_7",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 31, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T0510Z-100102T1200Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT THROUGH SATURDAY MORNING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect through Saturday morning",
                     ],
    },
    {
    "commentary": "Testing midnight issuance - 0-37 hour event",
    "name": "HeadlinesTiming_Watch_8",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 37, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T0510Z-100102T1800Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT THROUGH SATURDAY AFTERNOON...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect through Saturday afternoon",
                     ],
    },
    {
    "commentary": "Testing midnight issuance - 0-43 hour event",
    "name": "HeadlinesTiming_Watch_9",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 43, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T0510Z-100103T0000Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT THROUGH SATURDAY EVENING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect through Saturday evening",
                     ],
    },
    {
    "commentary": "Testing midnight issuance - 0-49 hour event",
    "name": "HeadlinesTiming_Watch_10",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 49, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T0510Z-100103T0600Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT THROUGH LATE SATURDAY NIGHT...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect through late Saturday night",
                     ],
    },
    {
    "commentary": "Testing midnight issuance - 0-55 hour event",
    "name": "HeadlinesTiming_Watch_11",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 55, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T0510Z-100103T1200Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT THROUGH SUNDAY MORNING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect through Sunday morning",
                     ],
    },

    {
    "commentary": "Issuance Time/Event Start on same calendar day",
    "name": "HeadlinesTiming_Watch_12",
    "drtTime": "20100101_0900",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 20, 40, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100102T0100Z-100102T2100Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT FROM THIS EVENING THROUGH SATURDAY AFTERNOON...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect from this evening through Saturday afternoon",
                     ],
    },

    
    {
    "commentary": "Issuance Time/Event Start on same calendar day - sc1",
    "name": "HeadlinesTiming_Watch_13",
    "drtTime": "20100101_0900",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 18, 23, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T2300Z-100102T0400Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT THIS EVENING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect this evening",
                     ],
    },

    
    {
    "commentary": "Issuance Time/Event Start on different day",
    "name": "HeadlinesTiming_Watch_14",
    "drtTime": "20100101_2000",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 29, 29+24, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100102T1000Z-100103T1000Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT FROM LATE TONIGHT THROUGH LATE SATURDAY NIGHT...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect from late tonight through late Saturday night",
                     ],
    },

    
    {
    "commentary": "EventStart 3+ from issuance, within 12hr, event end within 12",
    "name": "HeadlinesTiming_Watch_15",
    "drtTime": "20100101_1300",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 13, 19, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T1800Z-100102T0000Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT FROM 1 PM THIS AFTERNOON TO 7 PM EST THIS EVENING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect from 1 PM this afternoon to 7 PM EST this evening",
                     ],
    },

    
    {
    "commentary": "EventStart <3 from issuance, event end within 12",
    "name": "HeadlinesTiming_Watch_16",
    "drtTime": "20100101_1300",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 8, 19, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T1300Z-100102T0000Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT UNTIL 7 PM EST THIS EVENING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect until 7 PM EST this evening",
                     ],
    },

    {
    "commentary": "EventStart 3-12 from issuance, event end 12+",
    "name": "HeadlinesTiming_Watch_17",
    "drtTime": "20100101_1300",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 14, 32, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T1900Z-100102T1300Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT FROM 2 PM EST THIS AFTERNOON THROUGH SATURDAY MORNING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect from 2 PM EST this afternoon through Saturday morning",
                     ],
    },

    {
    "commentary": "EventStart <3 from issuance, event end 12+",
    "name": "HeadlinesTiming_Watch_18",
    "drtTime": "20100101_1315",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 10, 32, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T1500Z-100102T1300Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT THROUGH SATURDAY MORNING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect through Saturday morning",
                     ],
    },


    {
    "commentary": "EventStart 12+ from issuance, event end 12+",
    "name": "HeadlinesTiming_Watch_19",
    "drtTime": "20100101_1300",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 21, 44, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100102T0200Z-100103T0100Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT FROM THIS EVENING THROUGH SATURDAY EVENING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect from this evening through Saturday evening",
                     ],
    },


    {
    "commentary": "EventStart 3+ from issuance, within 12hr, event end within 12 at 6 PM",
    "name": "HeadlinesTiming_Watch_20",
    "drtTime": "20100101_1300",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 13, 18, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T1800Z-100101T2300Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT FROM 1 PM THIS AFTERNOON TO 6 PM EST THIS EVENING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect from 1 PM this afternoon to 6 PM EST this evening",
                     ],
    },

    
    {
    "commentary": "EventStart <3 from issuance, event end within 12 at 6 PM",
    "name": "HeadlinesTiming_Watch_21",
    "drtTime": "20100101_1300",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 8, 18, "WS.A", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.A.0001.100101T1300Z-100101T2300Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WATCH IN EFFECT UNTIL 6 PM EST THIS EVENING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch, which is in effect until 6 PM EST this evening",
                     ],
    },

    {
    "commentary": "Clear Hazards Table",
    "name": "Cleanup",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },


    ]

       
import TestScript
def testScript(self, dataMgr):
    defaults = {
        "database": "<site>_GRID__Fcst_00000000_0000",
        "publishGrids": 0,
        "decodeVTEC": 0,
        "gridsStartTime": "20100101_0500",
        "orderStrings": 1,
        "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




