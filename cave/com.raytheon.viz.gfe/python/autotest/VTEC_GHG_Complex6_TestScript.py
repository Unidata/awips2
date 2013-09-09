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
# More VTEC_GHG Complex Hazard Tests 
#
# Author:
# ----------------------------------------------------------------------------

scripts = [
    {
    "commentary": "Clear out all Hazards Table and Grids.",
    "name": "GHG_Complex6_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },
    {

    "commentary": """
    Creating a FW.A hazard for Area1
    Area1 (FLZ039, FLZ042) 18-27
    """,
    
    "name": "GHG_Complex6_1",
    "productType": "Hazard_RFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 18, 27, "FW.A", ["FLZ039", "FLZ042"]),
       ],
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-042-010800-",
      "/O.NEW.KTBW.FW.A.0001.100101T1800Z-100102T0300Z/",
      "LEVY-CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...FIRE WEATHER WATCH IN EFFECT FROM FRIDAY AFTERNOON THROUGH FRIDAY EVENING...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A FIRE WEATHER WATCH...WHICH IS IN EFFECT FROM FRIDAY AFTERNOON THROUGH FRIDAY EVENING. ",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A FIRE WEATHER WATCH MEANS THAT CRITICAL FIRE WEATHER CONDITIONS ARE FORECAST TO OCCUR. LISTEN FOR LATER FORECASTS AND POSSIBLE RED FLAG WARNINGS.",
      "&&",
      "$$",
                     ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex6_2",
    "productType": "Hazard_RFW_Local",
    "deleteGrids": [],
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-042-010800-",
      "/O.CON.KTBW.FW.A.0001.100101T1800Z-100102T0300Z/",
      "LEVY-CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...FIRE WEATHER WATCH REMAINS IN EFFECT FROM FRIDAY AFTERNOON THROUGH FRIDAY EVENING...",
      "A FIRE WEATHER WATCH REMAINS IN EFFECT FROM FRIDAY AFTERNOON THROUGH FRIDAY EVENING. ",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A FIRE WEATHER WATCH MEANS THAT CRITICAL FIRE WEATHER CONDITIONS ARE FORECAST TO OCCUR. LISTEN FOR LATER FORECASTS AND POSSIBLE RED FLAG WARNINGS.",
      "&&",
      "$$",
                     ],
    },
    {

    "commentary": """
    Upgrading the FW.A hazard in Area1 to a FW.W hazard
    """,
    
    "name": "GHG_Complex6_3",
    "productType": "Hazard_RFW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 18, 27, "FW.W", ["FLZ039", "FLZ042"]),
        ],
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-042-010800-",
      "/O.UPG.KTBW.FW.A.0001.100101T1800Z-100102T0300Z/",
      "/O.NEW.KTBW.FW.W.0001.100101T1800Z-100102T0300Z/",
      "LEVY-CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...RED FLAG WARNING IN EFFECT FROM 1 PM TO 10 PM EST FRIDAY...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A RED FLAG WARNING...WHICH IS IN EFFECT FROM 1 PM TO 10 PM EST FRIDAY. THE FIRE WEATHER WATCH IS NO LONGER IN EFFECT.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A RED FLAG WARNING MEANS THAT CRITICAL FIRE WEATHER CONDITIONS ARE EITHER OCCURRING NOW...OR WILL SHORTLY. A COMBINATION OF STRONG WINDS...LOW RELATIVE HUMIDITY...AND WARM TEMPERATURES WILL CREATE EXPLOSIVE FIRE GROWTH POTENTIAL.",
      "&&",
      "$$",
                     ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex6_4",
    "productType": "Hazard_RFW_Local",
    "deleteGrids": [],        
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-042-010800-",
      "/O.CON.KTBW.FW.W.0001.100101T1800Z-100102T0300Z/",
      "LEVY-CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...RED FLAG WARNING REMAINS IN EFFECT FROM 1 PM TO 10 PM EST FRIDAY...",
      "A RED FLAG WARNING REMAINS IN EFFECT FROM 1 PM TO 10 PM EST FRIDAY.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A RED FLAG WARNING MEANS THAT CRITICAL FIRE WEATHER CONDITIONS ARE EITHER OCCURRING NOW...OR WILL SHORTLY. A COMBINATION OF STRONG WINDS...LOW RELATIVE HUMIDITY...AND WARM TEMPERATURES WILL CREATE EXPLOSIVE FIRE GROWTH POTENTIAL.",
      "&&",
      "$$",
                     ],
    },
    {

    "commentary": """
    Creating a new FW.A hazard for Area1 with a new time range
    Area1 (FLZ039, FLZ042) 42-54
    """,
    
    "name": "GHG_Complex6_5",
    "productType": "Hazard_RFW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 18, 27, "FW.W", ["FLZ039", "FLZ042"]),
        ("Fcst", "Hazards", "DISCRETE", 42, 54, "FW.A", ["FLZ039", "FLZ042"]),],
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-042-010800-",
      "/O.NEW.KTBW.FW.A.0002.100102T1800Z-100103T0600Z/",
      "/O.CON.KTBW.FW.W.0001.100101T1800Z-100102T0300Z/",
      "LEVY-CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...RED FLAG WARNING REMAINS IN EFFECT FROM 1 PM TO 10 PM EST FRIDAY...",
      "...FIRE WEATHER WATCH IN EFFECT FROM SATURDAY AFTERNOON THROUGH LATE SATURDAY NIGHT...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A FIRE WEATHER WATCH...WHICH IS IN EFFECT FROM SATURDAY AFTERNOON THROUGH LATE SATURDAY NIGHT. A RED FLAG WARNING REMAINS IN EFFECT FROM 1 PM TO 10 PM EST FRIDAY.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A RED FLAG WARNING MEANS THAT CRITICAL FIRE WEATHER CONDITIONS ARE EITHER OCCURRING NOW...OR WILL SHORTLY. A COMBINATION OF STRONG WINDS...LOW RELATIVE HUMIDITY...AND WARM TEMPERATURES WILL CREATE EXPLOSIVE FIRE GROWTH POTENTIAL.",
      "A FIRE WEATHER WATCH MEANS THAT CRITICAL FIRE WEATHER CONDITIONS ARE FORECAST TO OCCUR. LISTEN FOR LATER FORECASTS AND POSSIBLE RED FLAG WARNINGS.",
      "&&",
      "$$",
                     ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex6_6",
    "productType": "Hazard_RFW_Local",
    "deleteGrids": [],        
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-042-010800-",
      "/O.CON.KTBW.FW.W.0001.100101T1800Z-100102T0300Z/",
      "/O.CON.KTBW.FW.A.0002.100102T1800Z-100103T0600Z/",
      "LEVY-CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...RED FLAG WARNING REMAINS IN EFFECT FROM 1 PM TO 10 PM EST FRIDAY...",
      "...FIRE WEATHER WATCH REMAINS IN EFFECT FROM SATURDAY AFTERNOON THROUGH LATE SATURDAY NIGHT...",
      "A RED FLAG WARNING REMAINS IN EFFECT FROM 1 PM TO 10 PM EST FRIDAY. A FIRE WEATHER WATCH REMAINS IN EFFECT FROM SATURDAY AFTERNOON THROUGH LATE SATURDAY NIGHT.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A RED FLAG WARNING MEANS THAT CRITICAL FIRE WEATHER CONDITIONS ARE EITHER OCCURRING NOW...OR WILL SHORTLY. A COMBINATION OF STRONG WINDS...LOW RELATIVE HUMIDITY...AND WARM TEMPERATURES WILL CREATE EXPLOSIVE FIRE GROWTH POTENTIAL.",
      "A FIRE WEATHER WATCH MEANS THAT CRITICAL FIRE WEATHER CONDITIONS ARE FORECAST TO OCCUR. LISTEN FOR LATER FORECASTS AND POSSIBLE RED FLAG WARNINGS.",
      "&&",
      "$$",
                     ],
    }, 
    {

    "commentary": """
    Changing the FW.W hazard in Area1 to no hazards
    Area1 (FLZ039, FLZ042) 18-27
    """,
    
    "name": "GHG_Complex6_7",
    "productType": "Hazard_RFW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 18, 27, "<None>", ["FLZ039", "FLZ042"]),
        ("Fcst", "Hazards", "DISCRETE", 42, 54, "FW.A", ["FLZ039", "FLZ042"]),],
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-042-010800-",
      "/O.CAN.KTBW.FW.W.0001.100101T1800Z-100102T0300Z/",
      "/O.CON.KTBW.FW.A.0002.100102T1800Z-100103T0600Z/",
      "LEVY-CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...FIRE WEATHER WATCH REMAINS IN EFFECT FROM SATURDAY AFTERNOON THROUGH LATE SATURDAY NIGHT...",
      "...RED FLAG WARNING IS CANCELLED...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE RED FLAG WARNING. A FIRE WEATHER WATCH REMAINS IN EFFECT FROM SATURDAY AFTERNOON THROUGH LATE SATURDAY NIGHT.",
#      "|*|*|* SEGMENT TEXT GOES HERE *|.*|*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A FIRE WEATHER WATCH MEANS THAT CRITICAL FIRE WEATHER CONDITIONS ARE FORECAST TO OCCUR. LISTEN FOR LATER FORECASTS AND POSSIBLE RED FLAG WARNINGS.",
      "&&",
      "$$",

                     ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex6_8",
    "productType": "Hazard_RFW_Local",
    "deleteGrids": [],        
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-042-010800-",
      "/O.CON.KTBW.FW.A.0002.100102T1800Z-100103T0600Z/",
      "LEVY-CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...FIRE WEATHER WATCH REMAINS IN EFFECT FROM SATURDAY AFTERNOON THROUGH LATE SATURDAY NIGHT...",
      "A FIRE WEATHER WATCH REMAINS IN EFFECT FROM SATURDAY AFTERNOON THROUGH LATE SATURDAY NIGHT.",
#      "|*|*|* SEGMENT TEXT GOES HERE *|.*|*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A FIRE WEATHER WATCH MEANS THAT CRITICAL FIRE WEATHER CONDITIONS ARE FORECAST TO OCCUR. LISTEN FOR LATER FORECASTS AND POSSIBLE RED FLAG WARNINGS.",
      "&&",
      "$$",
                     ],
    },
    {
    "commentary": "Canceling out all hazards.",
    "name": "GHG_Complex6_9",
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
        "gridsStartTime": "20100101_0000",
        "orderStrings": 1,
        "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)







