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
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "1210 AM EST FRI JAN 1 2010",
                     "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
                     ".|*OVERVIEW (MUST EDIT)*|.",
                     "FLZ050-011315-",
                     "/O.NEW.KTBW.ZR.Y.0001.100101T0510Z-100102T0500Z/",
                     "PINELLAS-",
                     "1210 AM EST FRI JAN 1 2010",
                     "...FREEZING RAIN ADVISORY IN EFFECT UNTIL MIDNIGHT EST TONIGHT...",
                     "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A FREEZING RAIN ADVISORY...WHICH IS IN EFFECT UNTIL MIDNIGHT EST TONIGHT.",
                     "|* SEGMENT TEXT GOES HERE *|.",
                     "A FREEZING RAIN ADVISORY MEANS THAT PERIODS OF FREEZING RAIN OR FREEZING DRIZZLE WILL CAUSE TRAVEL DIFFICULTIES. BE PREPARED FOR SLIPPERY ROADS. SLOW DOWN AND USE CAUTION WHILE DRIVING.",
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
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "1220 AM EST FRI JAN 1 2010",
                     "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
                     ".|*OVERVIEW (MUST EDIT)*|.",
                     "FLZ050-011330-",
                     "/O.UPG.KTBW.ZR.Y.0001.000000T0000Z-100102T0500Z/",
                     "/O.NEW.KTBW.BZ.W.0001.100101T0520Z-100102T0500Z/",
                     "/O.NEW.KTBW.ZR.Y.0002.100102T0500Z-100102T1700Z/",
                     "PINELLAS-",
                     "1220 AM EST FRI JAN 1 2010",
                     "...BLIZZARD WARNING IN EFFECT UNTIL MIDNIGHT EST TONIGHT...",
                     "...FREEZING RAIN ADVISORY IN EFFECT FROM MIDNIGHT TONIGHT TO NOON EST SATURDAY...",
                     "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A BLIZZARD WARNING...WHICH IS IN EFFECT UNTIL MIDNIGHT EST TONIGHT. A FREEZING RAIN ADVISORY HAS ALSO BEEN ISSUED. THIS FREEZING RAIN ADVISORY IS IN EFFECT FROM MIDNIGHT TONIGHT TO NOON EST SATURDAY. THE FREEZING RAIN ADVISORY IS NO LONGER IN EFFECT.",
                     "A BLIZZARD WARNING MEANS SEVERE WINTER WEATHER CONDITIONS ARE EXPECTED OR OCCURRING. FALLING AND BLOWING SNOW WITH STRONG WINDS AND POOR VISIBILITIES ARE LIKELY. THIS WILL LEAD TO WHITEOUT CONDITIONS...MAKING TRAVEL EXTREMELY DANGEROUS. DO NOT TRAVEL. IF YOU MUST TRAVEL...HAVE A WINTER SURVIVAL KIT WITH YOU. IF YOU GET STRANDED...STAY WITH YOUR VEHICLE.",
                     "A FREEZING RAIN ADVISORY MEANS THAT PERIODS OF FREEZING RAIN OR FREEZING DRIZZLE WILL CAUSE TRAVEL DIFFICULTIES. BE PREPARED FOR SLIPPERY ROADS. SLOW DOWN AND USE CAUTION WHILE DRIVING.",
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
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "1220 AM EST FRI JAN 1 2010",
                     "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
                     ".|*OVERVIEW (MUST EDIT)*|.",
                     "FLZ050-011330-",
                     "/O.NEW.KTBW.BZ.W.0001.100101T1700Z-100102T0500Z/",
                     "/O.NEW.KTBW.ZR.Y.0002.100102T0500Z-100102T1700Z/",
                     "/O.EXT.KTBW.ZR.Y.0001.000000T0000Z-100101T1700Z/",
                     "PINELLAS-",
                     "1220 AM EST FRI JAN 1 2010",
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
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "1220 AM EST FRI JAN 1 2010",
                     "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
                     ".|*OVERVIEW (MUST EDIT)*|.",
                     "FLZ050-011330-",
                     "/O.EXT.KTBW.ZR.Y.0001.000000T0000Z-100102T1700Z/",
                     "PINELLAS-",
                     "1220 AM EST FRI JAN 1 2010",
                     "...FREEZING RAIN ADVISORY NOW IN EFFECT UNTIL NOON EST SATURDAY...",
                     "THE FREEZING RAIN ADVISORY IS NOW IN EFFECT UNTIL NOON EST SATURDAY.",
                     "|* SEGMENT TEXT GOES HERE *|.",
                     "A FREEZING RAIN ADVISORY MEANS THAT PERIODS OF FREEZING RAIN OR FREEZING DRIZZLE WILL CAUSE TRAVEL DIFFICULTIES. BE PREPARED FOR SLIPPERY ROADS. SLOW DOWN AND USE CAUTION WHILE DRIVING.",
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
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "1220 AM EST FRI JAN 1 2010",
                     "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
                     ".|*OVERVIEW (MUST EDIT)*|.",
                     "FLZ050-011330-",
                     "/O.CAN.KTBW.ZR.Y.0001.000000T0000Z-100102T0500Z/",
                     "/O.NEW.KTBW.ZR.Y.0002.100102T0500Z-100102T1700Z/",
                     "PINELLAS-",
                     "1220 AM EST FRI JAN 1 2010",
                     "...FREEZING RAIN ADVISORY IN EFFECT FROM MIDNIGHT TONIGHT TO NOON EST SATURDAY...",
                     "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A FREEZING RAIN ADVISORY...WHICH IS IN EFFECT FROM MIDNIGHT TONIGHT TO NOON EST SATURDAY.",
                     "|* SEGMENT TEXT GOES HERE *|.",
                     "A FREEZING RAIN ADVISORY MEANS THAT PERIODS OF FREEZING RAIN OR FREEZING DRIZZLE WILL CAUSE TRAVEL DIFFICULTIES. BE PREPARED FOR SLIPPERY ROADS. SLOW DOWN AND USE CAUTION WHILE DRIVING.",
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
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "1220 AM EST FRI JAN 1 2010",
                     "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
                     ".|*OVERVIEW (MUST EDIT)*|.",
                     "FLZ050-011330-",
                     "/O.NEW.KTBW.BZ.W.0001.100101T1700Z-100101T2300Z/",
                     "/O.NEW.KTBW.ZR.Y.0002.100101T2300Z-100102T0500Z/",
                     "/O.EXT.KTBW.ZR.Y.0001.000000T0000Z-100101T1700Z/",
                     "PINELLAS-",
                     "1220 AM EST FRI JAN 1 2010",
                     "...FREEZING RAIN ADVISORY NOW IN EFFECT UNTIL NOON EST TODAY...",
                     "...BLIZZARD WARNING IN EFFECT FROM NOON TODAY TO 6 PM EST THIS EVENING...",
                     "...FREEZING RAIN ADVISORY IN EFFECT FROM 6 PM THIS EVENING TO MIDNIGHT EST TONIGHT...",
                     "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A BLIZZARD WARNING...WHICH IS IN EFFECT FROM NOON TODAY TO 6 PM EST THIS EVENING. A FREEZING RAIN ADVISORY HAS ALSO BEEN ISSUED. THIS FREEZING RAIN ADVISORY IS IN EFFECT FROM 6 PM THIS EVENING TO MIDNIGHT EST TONIGHT. THE FREEZING RAIN ADVISORY IS NOW IN EFFECT UNTIL NOON EST TODAY.",
                     "|* SEGMENT TEXT GOES HERE *|.",
                     "A FREEZING RAIN ADVISORY MEANS THAT PERIODS OF FREEZING RAIN OR FREEZING DRIZZLE WILL CAUSE TRAVEL DIFFICULTIES. BE PREPARED FOR SLIPPERY ROADS. SLOW DOWN AND USE CAUTION WHILE DRIVING.",
                     "A BLIZZARD WARNING MEANS SEVERE WINTER WEATHER CONDITIONS ARE EXPECTED OR OCCURRING. FALLING AND BLOWING SNOW WITH STRONG WINDS AND POOR VISIBILITIES ARE LIKELY. THIS WILL LEAD TO WHITEOUT CONDITIONS...MAKING TRAVEL EXTREMELY DANGEROUS. DO NOT TRAVEL. IF YOU MUST TRAVEL...HAVE A WINTER SURVIVAL KIT WITH YOU. IF YOU GET STRANDED...STAY WITH YOUR VEHICLE.",
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
        "database": "<site>_GRID__Official_00000000_0000",
        "publishGrids": 1,
        "decodeVTEC": 1,
        "gridsStartTime": "20100101_0500",
        "orderStrings": 1,
        "vtecMode": "O",
        "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




