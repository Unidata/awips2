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
    "checkStrings": ["EXPERIMENTAL...Watch County Notification for Watch 111",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLC017-",
                     "/E.NEW.KTBW.TO.A.0111.100101T0510Z-100101T0800Z/",
                     "The National Weather Service has issued Tornado Watch 111 in effect until 3 AM EST early this morning for the following areas",
                     "In Florida this watch includes 1 county",
                     "In west central Florida",
                     "Citrus",
                     "GMZ870-",
                     "/E.NEW.KTBW.TO.A.0111.100101T0510Z-100101T0800Z/",
                     "The National Weather Service has issued Tornado Watch 111 in effect until 3 AM EST early this morning for the following areas",
                     "This watch includes the following adjacent coastal waters",
                     "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM"

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
    "checkStrings": ["EXPERIMENTAL...Watch County Notification for Watch 111",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLC017-",
                     "/E.CON.KTBW.TO.A.0111.000000T0000Z-100101T0800Z/",
                     "Tornado Watch 111 remains valid until 3 AM EST early this morning for the following areas",
                     "In Florida this watch includes 1 county",
                     "In west central Florida",
                     "Citrus",
                     "GMZ870-",
                     "/E.CON.KTBW.TO.A.0111.000000T0000Z-100101T0800Z/",
                     "Tornado Watch 111 remains valid until 3 AM EST early this morning for the following areas",
                     "This watch includes the following adjacent coastal waters",
                     "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM"

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
    "checkStrings": ["EXPERIMENTAL...Watch County Notification for Watch 111",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLC017-",
                     "/E.EXP.KTBW.TO.A.0111.000000T0000Z-100101T0800Z/",
                     "The National Weather Service will allow Tornado Watch 111 to expire at 3 AM EST early this morning for the following areas",
                     "In Florida this allows to expire 1 county",
                     "In west central Florida",
                     "Citrus",
                     "GMZ870-",
                     "/E.EXP.KTBW.TO.A.0111.000000T0000Z-100101T0800Z/",
                     "The National Weather Service will allow Tornado Watch 111 to expire at 3 AM EST early this morning for the following areas",
                     "This allows to expire the following adjacent coastal waters",
                     "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM"

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
    "checkStrings": ["EXPERIMENTAL...Watch County Notification for Watch 111",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLC017-",
                     "/E.EXP.KTBW.TO.A.0111.000000T0000Z-100101T0800Z/",
                     "The National Weather Service has allowed Tornado Watch 111 to expire for the following areas",
                     "In Florida this allows to expire 1 county",
                     "In west central Florida",
                     "Citrus",
                     "GMZ870-",
                     "/E.EXP.KTBW.TO.A.0111.000000T0000Z-100101T0800Z/",
                     "The National Weather Service has allowed Tornado Watch 111 to expire for the following areas",
                     "This allows to expire the following adjacent coastal waters",
                     "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM"

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
    "checkStrings": ["EXPERIMENTAL...Watch County Notification for Watch 112",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLC017-",
                     "/E.NEW.KTBW.SV.A.0112.100101T0900Z-100101T1200Z/",
                     "The National Weather Service has issued Severe Thunderstorm Watch 112 in effect until 7 AM EST this morning for the following areas",
                     "In Florida this watch includes 1 county",
                     "In west central Florida",
                     "Citrus",
                     "GMZ870-",
                     "/E.NEW.KTBW.SV.A.0112.100101T0900Z-100101T1200Z/",
                     "The National Weather Service has issued Severe Thunderstorm Watch 112 in effect until 7 AM EST this morning for the following areas",
                     "This watch includes the following adjacent coastal waters",
                     "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM"

                     ],
    },
    {
    "commentary": "Testing cancel of SV.A",
    "name": "WCN_6",
    "drtTime": "20100101_1100",
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ],
    "checkStrings": ["EXPERIMENTAL...Watch County Notification for Watch 112",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLC017-",
                     "/E.CAN.KTBW.SV.A.0112.000000T0000Z-100101T1200Z/",
                     "The National Weather Service has cancelled Severe Thunderstorm Watch 112 for the following areas",
                     "In Florida this cancels 1 county",
                     "In west central Florida",
                     "Citrus",
                     "GMZ870-",
                     "/E.CAN.KTBW.SV.A.0112.000000T0000Z-100101T1200Z/",
                     "The National Weather Service has cancelled Severe Thunderstorm Watch 112 for the following areas",
                     "This cancels the following adjacent coastal waters",
                     "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM"

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
    "checkStrings": ["EXPERIMENTAL...Watch County Notification for Watch 115",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLC017-",
                     "/E.NEW.KTBW.SV.A.0115.100101T1205Z-100101T1700Z/",
                     "The National Weather Service has issued Severe Thunderstorm Watch 115 in effect until noon EST today for the following areas",
                     "In Florida this watch includes 1 county",
                     "In west central Florida",
                     "Citrus",
                     "GMZ870-",
                     "/E.NEW.KTBW.SV.A.0115.100101T1205Z-100101T1700Z/",
                     "The National Weather Service has issued Severe Thunderstorm Watch 115 in effect until noon EST today for the following areas",
                     "This watch includes the following adjacent coastal waters",
                     "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM"

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
    "checkStrings": ["EXPERIMENTAL...Watch County Notification for Watch 115",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLC053-",
                     "/E.EXA.KTBW.SV.A.0115.000000T0000Z-100101T1700Z/",
                     "The National Weather Service has extended Severe Thunderstorm Watch 115 to include the following areas until noon EST today",
                     "In Florida this watch includes 1 county",
                     "In west central Florida",
                     "Hernando",
                     "FLC017-",
                     "/E.CON.KTBW.SV.A.0115.000000T0000Z-100101T1700Z/",
                     "Severe Thunderstorm Watch 115 remains valid until noon EST today for the following areas",
                     "In Florida this watch includes 1 county",
                     "In west central Florida",
                     "Citrus",
                     "GMZ870-",
                     "/E.CON.KTBW.SV.A.0115.000000T0000Z-100101T1700Z/",
                     "Severe Thunderstorm Watch 115 remains valid until noon EST today for the following areas",
                     "This watch includes the following adjacent coastal waters",
                     "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM"

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
    "checkStrings": ["EXPERIMENTAL...Watch County Notification for Watch 115",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLC017-053-",
                     "/E.CON.KTBW.SV.A.0115.000000T0000Z-100101T1700Z/",
                     "Severe Thunderstorm Watch 115 remains valid until noon EST today for the following areas",
                     "In Florida this watch includes 2 counties",
                     "In west central Florida",
                     "Citrus Hernando",
                     "GMZ870-",
                     "/E.CON.KTBW.SV.A.0115.000000T0000Z-100101T1700Z/",
                     "Severe Thunderstorm Watch 115 remains valid until noon EST today for the following areas",
                     "This watch includes the following adjacent coastal waters",
                     "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM"

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
    "checkStrings": ["EXPERIMENTAL...Watch County Notification for Watch 115",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLC101-",
                     "/E.EXB.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "The National Weather Service has extended Severe Thunderstorm Watch 115 to include the following areas until 2 PM EST this afternoon",
                     "In Florida this watch includes 1 county",
                     "In west central Florida",
                     "Pasco",
                     "FLC017-053-",
                     "/E.EXT.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "Severe Thunderstorm Watch 115, previously in effect until noon EST today, is now in effect until 2 PM EST this afternoon for the following areas",
                     "In Florida this watch includes 2 counties",
                     "In west central Florida",
                     "Citrus Hernando",
                     "GMZ870-",
                     "/E.EXT.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "Severe Thunderstorm Watch 115, previously in effect until noon EST today, is now in effect until 2 PM EST this afternoon for the following areas",
                     "This watch includes the following adjacent coastal waters",
                     "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM"

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
    "checkStrings": ["EXPERIMENTAL...Watch County Notification for Watches 115/116",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLC017-101-",
                     "/E.CAN.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "/E.NEW.KTBW.TO.A.0116.100101T1700Z-100102T0000Z/",
                     "The National Weather Service has issued Tornado Watch 116 until 7 PM EST this evening which replaces a portion of Severe Thunderstorm Watch 115. The new watch is valid for the following areas",
                     "In Florida the new watch includes 2 counties",
                     "In west central Florida",
                     "Citrus Pasco",
                     "FLC053-",
                     "/E.CAN.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "The National Weather Service has cancelled Severe Thunderstorm Watch 115 for the following areas",
                     "In Florida this cancels 1 county",
                     "In west central Florida",
                     "Hernando",
                     "GMZ870-",
                     "/E.CON.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "Severe Thunderstorm Watch 115 remains valid until 2 PM EST this afternoon for the following areas",
                     "This watch includes the following adjacent coastal waters",
                     "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM"

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
    "checkStrings": ["EXPERIMENTAL...Watch County Notification for Watches 115/116",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "GMZ870-",
                     "/E.EXP.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "/E.EXB.KTBW.TO.A.0116.100101T1900Z-100102T0000Z/",
                     "The National Weather Service has issued Tornado Watch 116 until 7 PM EST this evening. Severe Thunderstorm Watch 115 will be allowed to expire. The new watch is valid for the following areas",
                     "The new watch includes the following adjacent coastal waters",
                     "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM",

                     "FLC017-101-",
                     "/E.CON.KTBW.TO.A.0116.000000T0000Z-100102T0000Z/",
                     "Tornado Watch 116 remains valid until 7 PM EST this evening for the following areas",
                     "In Florida this watch includes 2 counties",
                     "In west central Florida",
                     "Citrus Pasco",
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
    "checkStrings": ["EXPERIMENTAL...Watch County Notification for Watches 115/116",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "GMZ870-",
                     "/E.EXP.KTBW.SV.A.0115.000000T0000Z-100101T1900Z/",
                     "/E.EXA.KTBW.TO.A.0116.000000T0000Z-100102T0000Z/",
                     "The National Weather Service has issued Tornado Watch 116 until 7 PM EST this evening. Severe Thunderstorm Watch 115 has expired. The new watch is valid for the following areas",
                     "The new watch includes the following adjacent coastal waters",
                     "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM",

                     "FLC017-101-",
                     "/E.CON.KTBW.TO.A.0116.000000T0000Z-100102T0000Z/",
                     "Tornado Watch 116 remains valid until 7 PM EST this evening for the following areas",
                     "In Florida this watch includes 2 counties",
                     "In west central Florida",
                     "Citrus Pasco",
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
        "database": "<site>_GRID__Fcst_00000000_0000",
        "publishGrids": 0,
        "decodeVTEC": 1,
        "gridsStartTime": "20100101_0500",
        "orderStrings": 1,
        "vtecMode": "E",
        "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




