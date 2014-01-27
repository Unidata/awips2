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
    "name": "WCN_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    "decodeVTEC": 0,
    },
    {
    "commentary": "Testing midnight issuance - 3 hr event.",
    "name": "WCN_1",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "TO.A:0111", ["FLC017","GMZ870"]),
       ],
    "checkStrings": ["EXPERIMENTAL...WATCH COUNTY NOTIFICATION FOR WATCH 111",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "FLC017-",
                     "/E.NEW.KTBW.TO.A.0111.100101T0510Z-100101T0800Z/",
                     "THE NATIONAL WEATHER SERVICE HAS ISSUED TORNADO WATCH 111 IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING FOR THE FOLLOWING AREAS",
                     "IN FLORIDA THIS WATCH INCLUDES 1 COUNTY",
                     "IN WEST CENTRAL FLORIDA",
                     "CITRUS",
                     "GMZ870-",
                     "/E.NEW.KTBW.TO.A.0111.100101T0510Z-100101T0800Z/",
                     "THE NATIONAL WEATHER SERVICE HAS ISSUED TORNADO WATCH 111 IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING FOR THE FOLLOWING AREAS",
                     "THIS WATCH INCLUDES THE FOLLOWING ADJACENT COASTAL WATERS",
                     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM"

                     ],
    },

    {
    "commentary": "Testing continuation.",
    "name": "WCN_2",
    "drtTime": "20100101_0530",
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "TO.A:0111", ["FLC017","GMZ870"]),
       ],
    "checkStrings": ["EXPERIMENTAL...WATCH COUNTY NOTIFICATION FOR WATCH 111",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "FLC017-",
                     "/E.CON.KTBW.TO.A.0111.000000T0000Z-100101T0800Z/",
                     "TORNADO WATCH 111 REMAINS VALID UNTIL 3 AM EST EARLY THIS MORNING FOR THE FOLLOWING AREAS",
                     "IN FLORIDA THIS WATCH INCLUDES 1 COUNTY",
                     "IN WEST CENTRAL FLORIDA",
                     "CITRUS",
                     "GMZ870-",
                     "/E.CON.KTBW.TO.A.0111.000000T0000Z-100101T0800Z/",
                     "TORNADO WATCH 111 REMAINS VALID UNTIL 3 AM EST EARLY THIS MORNING FOR THE FOLLOWING AREAS",
                     "THIS WATCH INCLUDES THE FOLLOWING ADJACENT COASTAL WATERS",
                     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM"

                     ],
    },

    {
    "commentary": "Testing expire before expire time.",
    "name": "WCN_3",
    "drtTime": "20100101_0745",
    "productType": "Hazard_WCN_Local",
    "decodeVTEC": 0,
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "TO.A:0111", ["FLC017","GMZ870"]),
       ],
    "checkStrings": ["EXPERIMENTAL...WATCH COUNTY NOTIFICATION FOR WATCH 111",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "FLC017-",
                     "/E.EXP.KTBW.TO.A.0111.000000T0000Z-100101T0800Z/",
                     "THE NATIONAL WEATHER SERVICE WILL ALLOW TORNADO WATCH 111 TO EXPIRE AT 3 AM EST EARLY THIS MORNING FOR THE FOLLOWING AREAS",
                     "IN FLORIDA THIS ALLOWS TO EXPIRE 1 COUNTY",
                     "IN WEST CENTRAL FLORIDA",
                     "CITRUS",
                     "GMZ870-",
                     "/E.EXP.KTBW.TO.A.0111.000000T0000Z-100101T0800Z/",
                     "THE NATIONAL WEATHER SERVICE WILL ALLOW TORNADO WATCH 111 TO EXPIRE AT 3 AM EST EARLY THIS MORNING FOR THE FOLLOWING AREAS",
                     "THIS ALLOWS TO EXPIRE THE FOLLOWING ADJACENT COASTAL WATERS",
                     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM"

                     ],
    },


    {
    "commentary": "Testing expire after expire time.",
    "name": "WCN_4",
    "drtTime": "20100101_0815",
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "TO.A:0111", ["FLC017","GMZ870"]),
       ],
    "checkStrings": ["EXPERIMENTAL...WATCH COUNTY NOTIFICATION FOR WATCH 111",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "FLC017-",
                     "/E.EXP.KTBW.TO.A.0111.000000T0000Z-100101T0800Z/",
                     "THE NATIONAL WEATHER SERVICE HAS ALLOWED TORNADO WATCH 111 TO EXPIRE FOR THE FOLLOWING AREAS",
                     "IN FLORIDA THIS ALLOWS TO EXPIRE 1 COUNTY",
                     "IN WEST CENTRAL FLORIDA",
                     "CITRUS",
                     "GMZ870-",
                     "/E.EXP.KTBW.TO.A.0111.000000T0000Z-100101T0800Z/",
                     "THE NATIONAL WEATHER SERVICE HAS ALLOWED TORNADO WATCH 111 TO EXPIRE FOR THE FOLLOWING AREAS",
                     "THIS ALLOWS TO EXPIRE THE FOLLOWING ADJACENT COASTAL WATERS",
                     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM"

                     ],
    },

    {
    "commentary": "Testing new issuance of SV.A",
    "name": "WCN_5",
    "drtTime": "20100101_0900",
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 7, "SV.A:0112", ["FLC017","GMZ870"]),
       ],
    "checkStrings": ["EXPERIMENTAL...WATCH COUNTY NOTIFICATION FOR WATCH 112",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "FLC017-",
                     "/E.NEW.KTBW.SV.A.0112.100101T0900Z-100101T1200Z/",
                     "THE NATIONAL WEATHER SERVICE HAS ISSUED SEVERE THUNDERSTORM WATCH 112 IN EFFECT UNTIL 7 AM EST THIS MORNING FOR THE FOLLOWING AREAS",
                     "IN FLORIDA THIS WATCH INCLUDES 1 COUNTY",
                     "IN WEST CENTRAL FLORIDA",
                     "CITRUS",
                     "GMZ870-",
                     "/E.NEW.KTBW.SV.A.0112.100101T0900Z-100101T1200Z/",
                     "THE NATIONAL WEATHER SERVICE HAS ISSUED SEVERE THUNDERSTORM WATCH 112 IN EFFECT UNTIL 7 AM EST THIS MORNING FOR THE FOLLOWING AREAS",
                     "THIS WATCH INCLUDES THE FOLLOWING ADJACENT COASTAL WATERS",
                     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM"

                     ],
    },
    {
    "commentary": "Testing cancel of SV.A",
    "name": "WCN_6",
    "drtTime": "20100101_1100",
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ],
    "checkStrings": ["EXPERIMENTAL...WATCH COUNTY NOTIFICATION FOR WATCH 112",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "FLC017-",
                     "/E.CAN.KTBW.SV.A.0112.000000T0000Z-100101T1200Z/",
                     "THE NATIONAL WEATHER SERVICE HAS CANCELLED SEVERE THUNDERSTORM WATCH 112 FOR THE FOLLOWING AREAS",
                     "IN FLORIDA THIS CANCELS 1 COUNTY",
                     "IN WEST CENTRAL FLORIDA",
                     "CITRUS",
                     "GMZ870-",
                     "/E.CAN.KTBW.SV.A.0112.000000T0000Z-100101T1200Z/",
                     "THE NATIONAL WEATHER SERVICE HAS CANCELLED SEVERE THUNDERSTORM WATCH 112 FOR THE FOLLOWING AREAS",
                     "THIS CANCELS THE FOLLOWING ADJACENT COASTAL WATERS",
                     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM"

                     ],
    },

    {
    "commentary": "Testing new issuance of SV.A",
    "name": "WCN_7",
    "drtTime": "20100101_1205",
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 12, "SV.A:0115", ["FLC017","GMZ870"]),
       ],
    "checkStrings": ["EXPERIMENTAL...WATCH COUNTY NOTIFICATION FOR WATCH 115",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "FLC017-",
                     "/E.NEW.KTBW.SV.A.0115.100101T1205Z-100101T1700Z/",
                     "THE NATIONAL WEATHER SERVICE HAS ISSUED SEVERE THUNDERSTORM WATCH 115 IN EFFECT UNTIL NOON EST TODAY FOR THE FOLLOWING AREAS",
                     "IN FLORIDA THIS WATCH INCLUDES 1 COUNTY",
                     "IN WEST CENTRAL FLORIDA",
                     "CITRUS",
                     "GMZ870-",
                     "/E.NEW.KTBW.SV.A.0115.100101T1205Z-100101T1700Z/",
                     "THE NATIONAL WEATHER SERVICE HAS ISSUED SEVERE THUNDERSTORM WATCH 115 IN EFFECT UNTIL NOON EST TODAY FOR THE FOLLOWING AREAS",
                     "THIS WATCH INCLUDES THE FOLLOWING ADJACENT COASTAL WATERS",
                     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM"

                     ],
    },
    {
    "commentary": "Testing EXA SV.A",
    "name": "WCN_8",
    "drtTime": "20100101_1300",
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 12, "SV.A:0115", ["FLC017","FLC053","GMZ870"]),
       ],
    "checkStrings": ["EXPERIMENTAL...WATCH COUNTY NOTIFICATION FOR WATCH 115",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "FLC053-",
                     "/E.EXA.KTBW.SV.A.0115.000000T0000Z-100101T1700Z/",
                     "THE NATIONAL WEATHER SERVICE HAS EXTENDED SEVERE THUNDERSTORM WATCH 115 TO INCLUDE THE FOLLOWING AREAS UNTIL NOON EST TODAY",
                     "IN FLORIDA THIS WATCH INCLUDES 1 COUNTY",
                     "IN WEST CENTRAL FLORIDA",
                     "HERNANDO",
                     "FLC017-",
                     "/E.CON.KTBW.SV.A.0115.000000T0000Z-100101T1700Z/",
                     "SEVERE THUNDERSTORM WATCH 115 REMAINS VALID UNTIL NOON EST TODAY FOR THE FOLLOWING AREAS",
                     "IN FLORIDA THIS WATCH INCLUDES 1 COUNTY",
                     "IN WEST CENTRAL FLORIDA",
                     "CITRUS",
                     "GMZ870-",
                     "/E.CON.KTBW.SV.A.0115.000000T0000Z-100101T1700Z/",
                     "SEVERE THUNDERSTORM WATCH 115 REMAINS VALID UNTIL NOON EST TODAY FOR THE FOLLOWING AREAS",
                     "THIS WATCH INCLUDES THE FOLLOWING ADJACENT COASTAL WATERS",
                     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM"

                     ],
    },
    {
    "commentary": "Testing CON after EXA SV.A",
    "name": "WCN_9",
    "drtTime": "20100101_1400",
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 12, "SV.A:0115", ["FLC017","FLC053","GMZ870"]),
       ],
    "checkStrings": ["EXPERIMENTAL...WATCH COUNTY NOTIFICATION FOR WATCH 115",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "FLC017-053-",
                     "/E.CON.KTBW.SV.A.0115.000000T0000Z-100101T1700Z/",
                     "SEVERE THUNDERSTORM WATCH 115 REMAINS VALID UNTIL NOON EST TODAY FOR THE FOLLOWING AREAS",
                     "IN FLORIDA THIS WATCH INCLUDES 2 COUNTIES",
                     "IN WEST CENTRAL FLORIDA",
                     "CITRUS HERNANDO",
                     "GMZ870-",
                     "/E.CON.KTBW.SV.A.0115.000000T0000Z-100101T1700Z/",
                     "SEVERE THUNDERSTORM WATCH 115 REMAINS VALID UNTIL NOON EST TODAY FOR THE FOLLOWING AREAS",
                     "THIS WATCH INCLUDES THE FOLLOWING ADJACENT COASTAL WATERS",
                     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM"

                     ],
    },
    {
    "commentary": "Testing EXT, EXB after EXA SV.A",
    "name": "WCN_10",
    "drtTime": "20100101_1500",
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 14, "SV.A:0115", ["FLC101","FLC017","FLC053","GMZ870"]),
       ],
    "checkStrings": ["EXPERIMENTAL...WATCH COUNTY NOTIFICATION FOR WATCH 115",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "FLC101-",
                     "/E.EXB.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "THE NATIONAL WEATHER SERVICE HAS EXTENDED SEVERE THUNDERSTORM WATCH 115 TO INCLUDE THE FOLLOWING AREAS UNTIL 2 PM EST THIS AFTERNOON",
                     "IN FLORIDA THIS WATCH INCLUDES 1 COUNTY",
                     "IN WEST CENTRAL FLORIDA",
                     "PASCO",
                     "FLC017-053-",
                     "/E.EXT.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "SEVERE THUNDERSTORM WATCH 115...PREVIOUSLY IN EFFECT UNTIL NOON EST TODAY...IS NOW IN EFFECT UNTIL 2 PM EST THIS AFTERNOON FOR THE FOLLOWING AREAS",
                     "IN FLORIDA THIS WATCH INCLUDES 2 COUNTIES",
                     "IN WEST CENTRAL FLORIDA",
                     "CITRUS HERNANDO",
                     "GMZ870-",
                     "/E.EXT.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "SEVERE THUNDERSTORM WATCH 115...PREVIOUSLY IN EFFECT UNTIL NOON EST TODAY...IS NOW IN EFFECT UNTIL 2 PM EST THIS AFTERNOON FOR THE FOLLOWING AREAS",
                     "THIS WATCH INCLUDES THE FOLLOWING ADJACENT COASTAL WATERS",
                     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM"

                     ],
    },
    {
    "commentary": "Testing NEW/CAN TO.A",
    "name": "WCN_11",
    "drtTime": "20100101_1700",
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 14, "TO.A:0116", ["FLC101","FLC017"]),
       ("Fcst", "Hazards", "DISCRETE", 14, 19, "TO.A:0116", ["FLC101","FLC017"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 14, "SV.A:0115", ["GMZ870"]),
       ],
    "checkStrings": ["EXPERIMENTAL...WATCH COUNTY NOTIFICATION FOR WATCHES 115/116",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "FLC017-101-",
                     "/E.CAN.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "/E.NEW.KTBW.TO.A.0116.100101T1700Z-100102T0000Z/",
                     "THE NATIONAL WEATHER SERVICE HAS ISSUED TORNADO WATCH 116 UNTIL 7 PM EST THIS EVENING WHICH REPLACES A PORTION OF SEVERE THUNDERSTORM WATCH 115. THE NEW WATCH IS VALID FOR THE FOLLOWING AREAS",
                     "IN FLORIDA THE NEW WATCH INCLUDES 2 COUNTIES",
                     "IN WEST CENTRAL FLORIDA",
                     "CITRUS PASCO",
                     "FLC053-",
                     "/E.CAN.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "THE NATIONAL WEATHER SERVICE HAS CANCELLED SEVERE THUNDERSTORM WATCH 115 FOR THE FOLLOWING AREAS",
                     "IN FLORIDA THIS CANCELS 1 COUNTY",
                     "IN WEST CENTRAL FLORIDA",
                     "HERNANDO",
                     "GMZ870-",
                     "/E.CON.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "SEVERE THUNDERSTORM WATCH 115 REMAINS VALID UNTIL 2 PM EST THIS AFTERNOON FOR THE FOLLOWING AREAS",
                     "THIS WATCH INCLUDES THE FOLLOWING ADJACENT COASTAL WATERS",
                     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM"

                     ],
    },
    {
    "commentary": "Testing before EXP/EXA TO.A",
    "name": "WCN_12",
    "drtTime": "20100101_1850",
    "decodeVTEC": 0,  #don't decode the VTEC this time
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 14, "TO.A:0116", ["FLC101","FLC017"]),
       ("Fcst", "Hazards", "DISCRETE", 14, 19, "TO.A:0116", ["FLC101","FLC017","GMZ870"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 14, "SV.A:0115", ["GMZ870"]),
       ],
    "checkStrings": ["EXPERIMENTAL...WATCH COUNTY NOTIFICATION FOR WATCHES 115/116",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "GMZ870-",
                     "/E.EXP.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "/E.EXB.KTBW.TO.A.0116.100101T1900Z-100102T0000Z/",
                     "THE NATIONAL WEATHER SERVICE HAS ISSUED TORNADO WATCH 116 UNTIL 7 PM EST THIS EVENING. SEVERE THUNDERSTORM WATCH 115 WILL BE ALLOWED TO EXPIRE. THE NEW WATCH IS VALID FOR THE FOLLOWING AREAS",
                     "THE NEW WATCH INCLUDES THE FOLLOWING ADJACENT COASTAL WATERS",
                     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM",

                     "FLC017-101-",
                     "/E.CON.KTBW.TO.A.0116.000000T0000Z-100102T0000Z/",
                     "TORNADO WATCH 116 REMAINS VALID UNTIL 7 PM EST THIS EVENING FOR THE FOLLOWING AREAS",
                     "IN FLORIDA THIS WATCH INCLUDES 2 COUNTIES",
                     "IN WEST CENTRAL FLORIDA",
                     "CITRUS PASCO",
                     ],
       },
{
    "commentary": "Testing after EXP/EXA TO.A",
    "name": "WCN_13",
    "drtTime": "20100101_1910",
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 14, 19, "TO.A:0116", ["FLC101","FLC017","GMZ870"]),
       ],
    "checkStrings": ["EXPERIMENTAL...WATCH COUNTY NOTIFICATION FOR WATCHES 115/116",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "GMZ870-",
                     "/E.EXP.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "/E.EXA.KTBW.TO.A.0116.000000T0000Z-100102T0000Z/",
                     "THE NATIONAL WEATHER SERVICE HAS ISSUED TORNADO WATCH 116 UNTIL 7 PM EST THIS EVENING. SEVERE THUNDERSTORM WATCH 115 HAS EXPIRED. THE NEW WATCH IS VALID FOR THE FOLLOWING AREAS",
                     "THE NEW WATCH INCLUDES THE FOLLOWING ADJACENT COASTAL WATERS",
                     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM",

                     "FLC017-101-",
                     "/E.CON.KTBW.TO.A.0116.000000T0000Z-100102T0000Z/",
                     "TORNADO WATCH 116 REMAINS VALID UNTIL 7 PM EST THIS EVENING FOR THE FOLLOWING AREAS",
                     "IN FLORIDA THIS WATCH INCLUDES 2 COUNTIES",
                     "IN WEST CENTRAL FLORIDA",
                     "CITRUS PASCO",
                     ],
    },

    {
    "commentary": "Canceling out all hazards.",
    "name": "WCN_14",
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ],
    "checkStrings": [],
    "decodeVTEC": 0,
    },
    
    {
    "commentary": "Deleting hazard grids.",
    "name": "WCN_15",
    "productType": "Hazard_WCN_Local",
    "clearHazardsTable": 1,
    "checkStrings": [],
    "decodeVTEC":0,
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
        "vtecMode": "E",
        "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




