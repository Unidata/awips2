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
    "name": "EXTUPG_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

    {
    "commentary": "Setting initial event",
    "name": "EXTUPG_1",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "ZR.Y", ["FLZ050"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "1210 AM EST Fri Jan 1 2010",
                     "...|*Overview headline (must edit)*|...",
                     ".|*Overview (must edit)*|.",
                     "FLZ050-011315-",
                     "/O.NEW.KTBW.ZR.Y.0001.100101T0510Z-100102T0500Z/",
                     "Pinellas-",
                     "1210 AM EST Fri Jan 1 2010",
                     "...FREEZING RAIN ADVISORY IN EFFECT UNTIL MIDNIGHT EST TONIGHT...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Freezing Rain Advisory...which is in effect until midnight EST tonight.",
#                      "|* SEGMENT TEXT GOES HERE *|.",
                     "A Freezing Rain Advisory means that periods of freezing rain or freezing drizzle will cause travel difficulties. Be prepared for slippery roads. Slow down and use caution while driving.",
                     "$$",
                     ],
    },

    {
    "commentary": "Upgrading all and then extending",
    "name": "EXTUPG_2",
    "drtTime": "20100101_0520",
    "productType": "Hazard_WSW_Local",
    "decodeVTEC": 0,   #don't decode
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "BZ.W", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 24, 36, "ZR.Y", ["FLZ050"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "1220 AM EST Fri Jan 1 2010",
                     "...|*Overview headline (must edit)*|...",
                     ".|*Overview (must edit)*|.",
                     "FLZ050-011330-",
                     "/O.UPG.KTBW.ZR.Y.0001.000000T0000Z-100102T0500Z/",
                     "/O.NEW.KTBW.BZ.W.0001.100101T0520Z-100102T0500Z/",
                     "/O.NEW.KTBW.ZR.Y.0002.100102T0500Z-100102T1700Z/",
                     "Pinellas-",
                     "1220 AM EST Fri Jan 1 2010",
                     "...BLIZZARD WARNING IN EFFECT UNTIL MIDNIGHT EST TONIGHT...",
                     "...FREEZING RAIN ADVISORY IN EFFECT FROM MIDNIGHT TONIGHT TO NOON EST SATURDAY...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Blizzard Warning...which is in effect until midnight EST tonight. A Freezing Rain Advisory has also been issued. This Freezing Rain Advisory is in effect from midnight tonight to noon EST Saturday. The Freezing Rain Advisory is no longer in effect.",
                     "A Blizzard Warning means severe winter weather conditions are expected or occurring. Falling and blowing snow with strong winds and poor visibilities are likely. This will lead to whiteout conditions...making travel extremely dangerous. Do not travel. If you must travel...have a winter survival kit with you. If you get stranded...stay with your vehicle.",
                     "A Freezing Rain Advisory means that periods of freezing rain or freezing drizzle will cause travel difficulties. Be prepared for slippery roads. Slow down and use caution while driving.",
                     "$$",
                     ],
    },

    {
    "commentary": "Upgrading in middle and then extending",
    "name": "EXTUPG_3",
    "drtTime": "20100101_0520",
    "productType": "Hazard_WSW_Local",
    "decodeVTEC": 0,   #don't decode
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 12, "ZR.Y", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 12, 24, "BZ.W", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 24, 36, "ZR.Y", ["FLZ050"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "1220 AM EST Fri Jan 1 2010",
                     "...|*Overview headline (must edit)*|...",
                     ".|*Overview (must edit)*|.",
                     "FLZ050-011330-",
                     "/O.NEW.KTBW.BZ.W.0001.100101T1700Z-100102T0500Z/",
                     "/O.NEW.KTBW.ZR.Y.0002.100102T0500Z-100102T1700Z/",
                     "/O.EXT.KTBW.ZR.Y.0001.000000T0000Z-100101T1700Z/",
                     "Pinellas-",
                     "1220 AM EST Fri Jan 1 2010",
                     "...FREEZING RAIN ADVISORY NOW IN EFFECT UNTIL NOON EST TODAY...",
                     "...BLIZZARD WARNING IN EFFECT FROM NOON TODAY TO MIDNIGHT EST TONIGHT...",
                     "...FREEZING RAIN ADVISORY IN EFFECT FROM MIDNIGHT TONIGHT TO NOON EST SATURDAY...",
                     ],
    },

    {
    "commentary": "Normal extention in time - overlapping",
    "name": "EXTUPG_4",
    "drtTime": "20100101_0520",
    "productType": "Hazard_WSW_Local",
    "decodeVTEC": 0,   #don't decode
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 36, "ZR.Y", ["FLZ050"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "1220 AM EST Fri Jan 1 2010",
                     "...|*Overview headline (must edit)*|...",
                     ".|*Overview (must edit)*|.",
                     "FLZ050-011330-",
                     "/O.EXT.KTBW.ZR.Y.0001.000000T0000Z-100102T1700Z/",
                     "Pinellas-",
                     "1220 AM EST Fri Jan 1 2010",
                     "...FREEZING RAIN ADVISORY NOW IN EFFECT UNTIL NOON EST SATURDAY...",
#                      "The Freezing Rain Advisory is now in effect until noon EST Saturday.",
#                      "|* SEGMENT TEXT GOES HERE *|.",
                     "A Freezing Rain Advisory means that periods of freezing rain or freezing drizzle will cause travel difficulties. Be prepared for slippery roads. Slow down and use caution while driving.",
                     "$$",
                     ],
    },

    {
    "commentary": "Normal extention in time - non-overlapping",
    "name": "EXTUPG_5",
    "drtTime": "20100101_0520",
    "productType": "Hazard_WSW_Local",
    "decodeVTEC": 0,   #don't decode
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 24, 36, "ZR.Y", ["FLZ050"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "1220 AM EST Fri Jan 1 2010",
                     "...|*Overview headline (must edit)*|...",
                     ".|*Overview (must edit)*|.",
                     "FLZ050-011330-",
                     "/O.CAN.KTBW.ZR.Y.0001.000000T0000Z-100102T0500Z/",
                     "/O.NEW.KTBW.ZR.Y.0002.100102T0500Z-100102T1700Z/",
                     "Pinellas-",
                     "1220 AM EST Fri Jan 1 2010",
                     "...FREEZING RAIN ADVISORY IN EFFECT FROM MIDNIGHT TONIGHT TO NOON EST SATURDAY...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Freezing Rain Advisory...which is in effect from midnight tonight to noon EST Saturday.",
#                      "|* SEGMENT TEXT GOES HERE *|.",
                     "A Freezing Rain Advisory means that periods of freezing rain or freezing drizzle will cause travel difficulties. Be prepared for slippery roads. Slow down and use caution while driving.",
                     "$$",
                     ],
    },

    {
    "commentary": "Upgrading in middle",
    "name": "EXTUPG_6",
    "drtTime": "20100101_0520",
    "productType": "Hazard_WSW_Local",
    "decodeVTEC": 0,   #don't decode
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 12, "ZR.Y", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 12, 18, "BZ.W", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 18, 24, "ZR.Y", ["FLZ050"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "1220 AM EST Fri Jan 1 2010",
                     "...|*Overview headline (must edit)*|...",
                     ".|*Overview (must edit)*|.",
                     "FLZ050-011330-",
                     "/O.NEW.KTBW.BZ.W.0001.100101T1700Z-100101T2300Z/",
                     "/O.NEW.KTBW.ZR.Y.0002.100101T2300Z-100102T0500Z/",
                     "/O.EXT.KTBW.ZR.Y.0001.000000T0000Z-100101T1700Z/",
                     "Pinellas-",
                     "1220 AM EST Fri Jan 1 2010",
                     "...FREEZING RAIN ADVISORY NOW IN EFFECT UNTIL NOON EST TODAY...",
                     "...BLIZZARD WARNING IN EFFECT FROM NOON TODAY TO 6 PM EST THIS EVENING...",
                     "...FREEZING RAIN ADVISORY IN EFFECT FROM 6 PM THIS EVENING TO MIDNIGHT EST TONIGHT...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a Blizzard Warning...which is in effect from noon today to 6 PM EST this evening. A Freezing Rain Advisory has also been issued. This Freezing Rain Advisory is in effect from 6 PM this evening to midnight EST tonight.",
#                      "|* SEGMENT TEXT GOES HERE *|.",
                     "A Freezing Rain Advisory means that periods of freezing rain or freezing drizzle will cause travel difficulties. Be prepared for slippery roads. Slow down and use caution while driving.",
                     "A Blizzard Warning means severe winter weather conditions are expected or occurring. Falling and blowing snow with strong winds and poor visibilities are likely. This will lead to whiteout conditions...making travel extremely dangerous. Do not travel. If you must travel...have a winter survival kit with you. If you get stranded...stay with your vehicle.",
                     "$$",
                     ],
    },

    {
    "commentary": "Cleanup.",
    "name": "EXTUPG_7",
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
        "decodeVTEC": 1,
        "gridsStartTime": "20100101_0500",
        "orderStrings": 1,
        "vtecMode": "O",
        "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




