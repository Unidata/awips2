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
#
# Author:
# ----------------------------------------------------------------------------
    
scripts = [

    {
    "commentary": "Start WW.Y for 24 hours.",
    "name": "DR21060_SetUp",
    "drtTime": "20200120_0510",
    "productType": "Hazard_WSW_Local",
    "clearHazardsTable": 1,
    "decodeVTEC": 1,
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "WW.Y", ["FLZ139","FLZ142"]),
       ],
    "checkStrings": [
       "WWUS42 KTBW 200510",
       "WSWTBW",
       "URGENT - WINTER WEATHER MESSAGE",
       "National Weather Service Tampa Bay Ruskin FL",
       "1210 AM EST Mon Jan 20 2020",
       "FLZ139-142-201315-",
       "/O.NEW.KTBW.WW.Y.0001.200120T0510Z-200121T0500Z/",
       "Coastal Levy-Coastal Citrus-",
       "1210 AM EST Mon Jan 20 2020",
       "...WINTER WEATHER ADVISORY IN EFFECT UNTIL MIDNIGHT EST TONIGHT...",
       "$$"
       ],
    },

    {
    "commentary": "Split the event in two. No CANcel as start time didn't change.",
    "name": "DR21060_Split",
    "drtTime": "20200120_0520",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 10, "WW.Y", ["FLZ139","FLZ142"]),
       ("Fcst", "Hazards", "DISCRETE", 12, 24, "WW.Y", ["FLZ139","FLZ142"]),
       ],
    "checkStrings": [
       "WWUS42 KTBW 200520",
       "WSWTBW",
       "URGENT - WINTER WEATHER MESSAGE",
       "National Weather Service Tampa Bay Ruskin FL",
       "1220 AM EST Mon Jan 20 2020",
       "FLZ139-142-201330-",
       "/O.NEW.KTBW.WW.Y.0002.200120T1700Z-200121T0500Z/",
       "/O.EXT.KTBW.WW.Y.0001.000000T0000Z-200120T1500Z/",
       "Coastal Levy-Coastal Citrus-",
       "1220 AM EST Mon Jan 20 2020",
       "...WINTER WEATHER ADVISORY NOW IN EFFECT UNTIL 10 AM EST THIS MORNING...",
       "...WINTER WEATHER ADVISORY IN EFFECT FROM NOON TODAY TO MIDNIGHT EST TONIGHT...",
       "$$"
       ],
    "notCheckStrings": [
       "/O.CAN.KTBW.WW.Y.0001.000000T0000Z-200121T0500Z/",
       ],
    },

    {
    "commentary": "Delay the start.",
    "name": "DR21060_Delay_Start",
    "drtTime": "20200120_0520",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 2, 10, "WW.Y", ["FLZ139","FLZ142"]),
       ("Fcst", "Hazards", "DISCRETE", 12, 24, "WW.Y", ["FLZ139","FLZ142"]),
       ],
    "checkStrings": [
       "WWUS42 KTBW 200520",
       "WSWTBW",
       "URGENT - WINTER WEATHER MESSAGE",
       "National Weather Service Tampa Bay Ruskin FL",
       "1220 AM EST Mon Jan 20 2020",
       "FLZ139-142-201330-",
       "/O.CAN.KTBW.WW.Y.0001.000000T0000Z-200121T0500Z/",
       "/O.NEW.KTBW.WW.Y.0002.200120T0700Z-200120T1500Z/",
       "/O.NEW.KTBW.WW.Y.0003.200120T1700Z-200121T0500Z/",
       "Coastal Levy-Coastal Citrus-",
       "1220 AM EST Mon Jan 20 2020",
       "...WINTER WEATHER ADVISORY IN EFFECT UNTIL 10 AM EST THIS MORNING...",
       "...WINTER WEATHER ADVISORY IN EFFECT FROM NOON TODAY TO MIDNIGHT EST TONIGHT...",
       "$$"
       ],
    },

    {
    "commentary": "Upgrade the early portion to BZ.W.",
    "name": "DR21060_Upgrade_1",
    "drtTime": "20200120_0520",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 12, "BZ.W", ["FLZ139","FLZ142"]),
       ("Fcst", "Hazards", "DISCRETE", 12, 24, "WW.Y", ["FLZ139","FLZ142"]),
       ],
    "checkStrings": [
       "WWUS42 KTBW 200520",
       "WSWTBW",
       "URGENT - WINTER WEATHER MESSAGE",
       "National Weather Service Tampa Bay Ruskin FL",
       "1220 AM EST Mon Jan 20 2020",
       "FLZ139-142-201330-",
       "/O.UPG.KTBW.WW.Y.0001.000000T0000Z-200121T0500Z/",
       "/O.NEW.KTBW.BZ.W.0001.200120T0520Z-200120T1700Z/",
       "/O.NEW.KTBW.WW.Y.0002.200120T1700Z-200121T0500Z/",
       "Coastal Levy-Coastal Citrus-",
       "1220 AM EST Mon Jan 20 2020",
       "...BLIZZARD WARNING IN EFFECT UNTIL NOON EST TODAY...",
       "...WINTER WEATHER ADVISORY IN EFFECT FROM NOON TODAY TO MIDNIGHT EST TONIGHT...",
       "$$"
       ],
    },

    {
    "commentary": "Start the upgrade later and end it sooner.",
    "name": "DR21060_Upgrade_2",
    "drtTime": "20200120_0520",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 3, 10, "WS.W", ["FLZ139","FLZ142"]),
       ("Fcst", "Hazards", "DISCRETE", 12, 24, "WW.Y", ["FLZ139","FLZ142"]),
       ],
    "checkStrings": [
       "WWUS42 KTBW 200520",
       "WSWTBW",
       "URGENT - WINTER WEATHER MESSAGE",
       "National Weather Service Tampa Bay Ruskin FL",
       "1220 AM EST Mon Jan 20 2020",
       "FLZ139-142-201330-",
       "/O.UPG.KTBW.WW.Y.0001.000000T0000Z-200121T0500Z/",
       "/O.NEW.KTBW.WS.W.0001.200120T0800Z-200120T1500Z/",
       "/O.NEW.KTBW.WW.Y.0002.200120T1700Z-200121T0500Z/",
       "Coastal Levy-Coastal Citrus-",
       "1220 AM EST Mon Jan 20 2020",
       "...WINTER STORM WARNING IN EFFECT UNTIL 10 AM EST THIS MORNING...",
       "...WINTER WEATHER ADVISORY IN EFFECT FROM NOON TODAY TO MIDNIGHT EST TONIGHT...",
       "$$"
       ],
    },

    {
    "commentary": "Upgrade comes at the tail end.",
    "name": "DR21060_Upgrade_3",
    "drtTime": "20200120_0520",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 2, 12, "WW.Y", ["FLZ139","FLZ142"]),
       ("Fcst", "Hazards", "DISCRETE", 15, 20, "BZ.W", ["FLZ139","FLZ142"]),
       ],
    "checkStrings": [
       "WWUS42 KTBW 200520",
       "WSWTBW",
       "URGENT - WINTER WEATHER MESSAGE",
       "National Weather Service Tampa Bay Ruskin FL",
       "1220 AM EST Mon Jan 20 2020",
       "FLZ139-142-201330-",
       "/O.UPG.KTBW.WW.Y.0001.000000T0000Z-200121T0500Z/",
       "/O.NEW.KTBW.BZ.W.0001.200120T2000Z-200121T0100Z/",
       "/O.NEW.KTBW.WW.Y.0002.200120T0700Z-200120T1700Z/",
       "Coastal Levy-Coastal Citrus-",
       "1220 AM EST Mon Jan 20 2020",
       "...WINTER WEATHER ADVISORY IN EFFECT UNTIL NOON EST TODAY...",
       "...BLIZZARD WARNING IN EFFECT FROM 3 PM THIS AFTERNOON TO 8 PM EST THIS EVENING...",
       "$$"
       ],
    },
    
    {
    "commentary": "Replace an early portion with WC.Y.",
    "name": "DR21060_Replacement",
    "drtTime": "20200120_0520",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 2, 10, "WC.Y", ["FLZ139","FLZ142"]),
       ("Fcst", "Hazards", "DISCRETE", 12, 24, "WW.Y", ["FLZ139","FLZ142"]),
       ],
    "checkStrings": [
       "WWUS42 KTBW 200520",
       "WSWTBW",
       "URGENT - WINTER WEATHER MESSAGE",
       "National Weather Service Tampa Bay Ruskin FL",
       "1220 AM EST Mon Jan 20 2020",
       "FLZ139-142-201330-",
       "/O.CAN.KTBW.WW.Y.0001.000000T0000Z-200121T0500Z/",
       "/O.NEW.KTBW.WC.Y.0001.200120T0700Z-200120T1500Z/",
       "/O.NEW.KTBW.WW.Y.0002.200120T1700Z-200121T0500Z/",
       "Coastal Levy-Coastal Citrus-",
       "1220 AM EST Mon Jan 20 2020",
       "...WIND CHILL ADVISORY IN EFFECT UNTIL 10 AM EST THIS MORNING...",
       "...WINTER WEATHER ADVISORY IN EFFECT FROM NOON TODAY TO MIDNIGHT EST TONIGHT...",
       "$$"
       ],
    },
    
    {
    "commentary": "Cleanup.",
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
        "gridsStartTime": "20200120_0500",
        "orderStrings": 1,
        "vtecMode": "O",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)
