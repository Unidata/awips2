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
    "name": "HeadlinesTiming_Warn_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    "comboFlag": 0,
    },
    {
    "commentary": "Testing midnight issuance - 3 hr event.",
    "name": "HeadlinesTiming_Warn_1",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T0510Z-100101T0800Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect until 3 AM EST early this morning",
                     ],
    },

    {
    "commentary": "Testing midnight issuance - 4-9 hour event",
    "name": "HeadlinesTiming_Warn_2",
    "drtTime": "20100101_0500",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 4, 9, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T0900Z-100101T1400Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT FROM 4 AM TO 9 AM EST THIS MORNING...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect from 4 AM to 9 AM EST this morning",
                     ],
    },

    {
    "commentary": "Testing midnight issuance - 4-15 hour event",
    "name": "HeadlinesTiming_Warn_3",
    "drtTime": "20100101_0500",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 4, 15, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T0900Z-100101T2000Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT FROM 4 AM EARLY THIS MORNING TO 3 PM EST THIS AFTERNOON...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect from 4 AM early this morning to 3 PM EST this afternoon",
                     ],
    },

    {
    "commentary": "Testing midnight issuance - 12-15 hour event",
    "name": "HeadlinesTiming_Warn_4",
    "drtTime": "20100101_0500",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 12, 15, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T1700Z-100101T2000Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT FROM NOON TODAY TO 3 PM EST THIS AFTERNOON...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect from noon today to 3 PM EST this afternoon",
                     ],
    },


    {
    "commentary": "Testing midnight issuance - 13-22 hour event",
    "name": "HeadlinesTiming_Warn_5",
    "drtTime": "20100101_0500",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 13, 22, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T1800Z-100102T0300Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT FROM 1 PM THIS AFTERNOON TO 10 PM EST THIS EVENING...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect from 1 PM this afternoon to 10 PM EST this evening",
                     ],
    },
    {
    "commentary": "Testing midnight issuance - 0-25 hour event",
    "name": "HeadlinesTiming_Warn_6",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 25, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T0510Z-100102T0600Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT UNTIL 1 AM EST SATURDAY...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect until 1 AM EST Saturday",
                     ],
    },
    {
    "commentary": "Testing midnight issuance - 0-31 hour event",
    "name": "HeadlinesTiming_Warn_7",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 31, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T0510Z-100102T1200Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT UNTIL 7 AM EST SATURDAY...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect until 7 AM EST Saturday",
                     ],
    },
    {
    "commentary": "Testing midnight issuance - 0-37 hour event",
    "name": "HeadlinesTiming_Warn_8",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 37, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T0510Z-100102T1800Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT UNTIL 1 PM EST SATURDAY...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect until 1 PM EST Saturday",
                     ],
    },
    {
    "commentary": "Testing midnight issuance - 0-43 hour event",
    "name": "HeadlinesTiming_Warn_9",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 43, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T0510Z-100103T0000Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT UNTIL 7 PM EST SATURDAY...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect until 7 PM EST Saturday",
                     ],
    },
    {
    "commentary": "Testing midnight issuance - 0-49 hour event",
    "name": "HeadlinesTiming_Warn_10",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 49, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T0510Z-100103T0600Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT UNTIL 1 AM EST SUNDAY...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect until 1 AM EST Sunday",
                     ],
    },
    {
    "commentary": "Testing midnight issuance - 0-55 hour event",
    "name": "HeadlinesTiming_Warn_11",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 55, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T0510Z-100103T1200Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT UNTIL 7 AM EST SUNDAY...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect until 7 AM EST Sunday",
                     ],
    },
    {
    "commentary": "Testing noon Today -start time",
    "name": "HeadlinesTiming_Warn_12",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 12, 36, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T1700Z-100102T1700Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT FROM NOON TODAY TO NOON EST SATURDAY...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect from noon today to noon EST Saturday",
                     ],
    },
    {
    "commentary": "Testing noon Today - end time",
    "name": "HeadlinesTiming_Warn_13",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 4, 12, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T0900Z-100101T1700Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT FROM 4 AM EARLY THIS MORNING TO NOON EST TODAY...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect from 4 AM early this morning to noon EST today.",
                     ],
    },
    {
    "commentary": "Testing 10am issuance, event 1pm-5pm",
    "name": "HeadlinesTiming_Warn_14",
    "drtTime": "20100101_1500",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 13, 17, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T1800Z-100101T2200Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT FROM 1 PM TO 5 PM EST THIS AFTERNOON...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect from 1 PM to 5 PM EST this afternoon.",
                     ],
    },
    {
    "commentary": "Testing 1am issuance, event 4am-9am",
    "name": "HeadlinesTiming_Warn_15",
    "drtTime": "20100101_0600",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 4, 9, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T0900Z-100101T1400Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT FROM 4 AM TO 9 AM EST THIS MORNING...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect from 4 AM to 9 AM EST this morning.",
                     ],
    },
    {
    "commentary": "Testing issuance time event start time on different days",
    "name": "HeadlinesTiming_Warn_16",
    "drtTime": "20100101_2000",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 29, 53, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100102T1000Z-100103T1000Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT FROM 5 AM SATURDAY TO 5 AM EST SUNDAY...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect from 5 AM Saturday to 5 AM EST Sunday.",
                     ],
    },
    {
    "commentary": "Testing issuance time event start time on different days, but start/end time same day",
    "name": "HeadlinesTiming_Warn_17",
    "drtTime": "20100102_0300",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 29, 41, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100102T1000Z-100102T2200Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT FROM 5 AM TO 5 PM EST SATURDAY...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect from 5 AM to 5 PM EST Saturday.",
                     ],
    },
    {
    "commentary": "Testing issuance time same as event start time",
    "name": "HeadlinesTiming_Warn_18",
    "drtTime": "20100101_0900",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 4, 20, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T0900Z-100102T0100Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT UNTIL 8 PM EST THIS EVENING...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect until 8 PM EST this evening.",
                     ],
    },
    {
    "commentary": "Testing event end time on different day from issuance time",
    "name": "HeadlinesTiming_Warn_19",
    "drtTime": "20100101_2100",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 16, 26, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100101T2100Z-100102T0700Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT UNTIL 2 AM EST SATURDAY...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect until 2 AM EST Saturday.",
                     ],
    },
    {
    "commentary": "Testing event start time < issuance+3",
    "name": "HeadlinesTiming_Warn_20",
    "drtTime": "20100102_0315",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 25, 34, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100102T0600Z-100102T1500Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT UNTIL 10 AM EST SATURDAY...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect until 10 AM EST Saturday.",
                     ],
    },
    {
    "commentary": "Testing midnight tonight starting time",
    "name": "HeadlinesTiming_Warn_21",
    "drtTime": "20100101_2100",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 24, 48, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100102T0500Z-100103T0500Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT FROM MIDNIGHT TONIGHT TO MIDNIGHT EST SATURDAY NIGHT...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect from midnight tonight to midnight EST Saturday night.",
                     ],
    },
    {
    "commentary": "Testing midnight tonight ending time",
    "name": "HeadlinesTiming_Warn_22",
    "drtTime": "20100101_2100",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 20, 24, "WS.W", ["FLZ139"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "/O.NEW.KTBW.WS.W.0001.100102T0100Z-100102T0500Z/",
                     "Coastal Levy-",
                     "...WINTER STORM WARNING IN EFFECT FROM 8 PM THIS EVENING TO MIDNIGHT EST TONIGHT...",
#                     "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning, which is in effect from 8 PM this evening to midnight EST tonight.",
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




