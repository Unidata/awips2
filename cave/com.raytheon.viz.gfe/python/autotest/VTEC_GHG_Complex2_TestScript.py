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
# more VTEC_GHG Complex Hazard Tests 
#
# Author:
# ----------------------------------------------------------------------------

scripts = [
    {
    "commentary": "Clear out all Hazards Table and Grids.",
    "name": "GHG_Complex2_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },
    {
    
    "commentary": """
    Creating a WS.A hazard for Area1 and Area2
    Area1 (FLZ039, FLZ042, FLZ043) 21-45
    Area2 (FLZ052, FLZ055, FLZ056) 27-45
    Note that we had to condense the times into two time ranges to accomodate for the overlapping
    """,
    
    "name": "GHG_Complex2_1",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 21, 27, "WS.A", ["FLZ039", "FLZ042", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 27, 45, "WS.A", ["FLZ039", "FLZ042", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 27, 45, "WS.A", ["FLZ052", "FLZ055", "FLZ056"]),
        ],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-042-043-010800-",
      "/O.NEW.KTBW.WS.A.0001.100101T2100Z-100102T2100Z/",
      "LEVY-CITRUS-SUMTER-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER STORM WATCH IN EFFECT FROM FRIDAY AFTERNOON THROUGH SATURDAY AFTERNOON...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A WINTER STORM WATCH...WHICH IS IN EFFECT FROM FRIDAY AFTERNOON THROUGH SATURDAY AFTERNOON.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "A WINTER STORM WATCH MEANS THERE IS A POTENTIAL FOR SIGNIFICANT SNOW...SLEET...OR ICE ACCUMULATIONS THAT MAY IMPACT TRAVEL. CONTINUE TO MONITOR THE LATEST FORECASTS.",
      "$$",
      "FLZ052-055-056-010800-",
      "/O.NEW.KTBW.WS.A.0001.100102T0300Z-100102T2100Z/",
      "POLK-MANATEE-HARDEE-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER STORM WATCH IN EFFECT FROM FRIDAY EVENING THROUGH SATURDAY AFTERNOON...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A WINTER STORM WATCH...WHICH IS IN EFFECT FROM FRIDAY EVENING THROUGH SATURDAY AFTERNOON.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A WINTER STORM WATCH MEANS THERE IS A POTENTIAL FOR SIGNIFICANT SNOW...SLEET...OR ICE ACCUMULATIONS THAT MAY IMPACT TRAVEL. CONTINUE TO MONITOR THE LATEST FORECASTS.",
      "&&",
      "$$",
                     ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex2_2",
    "productType": "Hazard_WSW_Local",
    "deleteGrids": [],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-042-043-010800-",
      "/O.CON.KTBW.WS.A.0001.100101T2100Z-100102T2100Z/",
      "LEVY-CITRUS-SUMTER-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER STORM WATCH REMAINS IN EFFECT FROM FRIDAY AFTERNOON THROUGH SATURDAY AFTERNOON...",
      "A WINTER STORM WATCH REMAINS IN EFFECT FROM FRIDAY AFTERNOON THROUGH SATURDAY AFTERNOON.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "A WINTER STORM WATCH MEANS THERE IS A POTENTIAL FOR SIGNIFICANT SNOW...SLEET...OR ICE ACCUMULATIONS THAT MAY IMPACT TRAVEL. CONTINUE TO MONITOR THE LATEST FORECASTS.",
      "$$",
      "FLZ052-055-056-010800-",
      "/O.CON.KTBW.WS.A.0001.100102T0300Z-100102T2100Z/",
      "POLK-MANATEE-HARDEE-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER STORM WATCH REMAINS IN EFFECT FROM FRIDAY EVENING THROUGH SATURDAY AFTERNOON...",
      "A WINTER STORM WATCH REMAINS IN EFFECT FROM FRIDAY EVENING THROUGH SATURDAY AFTERNOON.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A WINTER STORM WATCH MEANS THERE IS A POTENTIAL FOR SIGNIFICANT SNOW...SLEET...OR ICE ACCUMULATIONS THAT MAY IMPACT TRAVEL. CONTINUE TO MONITOR THE LATEST FORECASTS.",
      "&&",
      "$$",
                     ],
    },
    {
    
    "commentary": """
    Expanding the WS.A hazard of Area2
    Area2 (FLZ052, FLZ055, FLZ056, FLZ057, FLZ060, FLZ061) 27-45
    """,
    
    "name": "GHG_Complex2_3",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 21, 27, "WS.A", ["FLZ039", "FLZ042", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 27, 45, "WS.A", ["FLZ039", "FLZ042", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 27, 45, "WS.A", ["FLZ052", "FLZ055", "FLZ056", "FLZ057", "FLZ060", "FLZ061"]),
        ],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ057-060-061-010800-",
      "/O.EXA.KTBW.WS.A.0001.100102T0300Z-100102T2100Z/",
      "HIGHLANDS-SARASOTA-DE SOTO-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER STORM WATCH IN EFFECT FROM FRIDAY EVENING THROUGH SATURDAY AFTERNOON...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A WINTER STORM WATCH...WHICH IS IN EFFECT FROM FRIDAY EVENING THROUGH SATURDAY AFTERNOON.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A WINTER STORM WATCH MEANS THERE IS A POTENTIAL FOR SIGNIFICANT SNOW...SLEET...OR ICE ACCUMULATIONS THAT MAY IMPACT TRAVEL. CONTINUE TO MONITOR THE LATEST FORECASTS.",
      "&&",
      "$$",
      "FLZ039-042-043-010800-",
      "/O.CON.KTBW.WS.A.0001.100101T2100Z-100102T2100Z/",
      "LEVY-CITRUS-SUMTER-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER STORM WATCH REMAINS IN EFFECT FROM FRIDAY AFTERNOON THROUGH SATURDAY AFTERNOON...",
      "A WINTER STORM WATCH REMAINS IN EFFECT FROM FRIDAY AFTERNOON THROUGH SATURDAY AFTERNOON.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "A WINTER STORM WATCH MEANS THERE IS A POTENTIAL FOR SIGNIFICANT SNOW...SLEET...OR ICE ACCUMULATIONS THAT MAY IMPACT TRAVEL. CONTINUE TO MONITOR THE LATEST FORECASTS.",
      "$$",
      "FLZ052-055-056-010800-",
      "/O.CON.KTBW.WS.A.0001.100102T0300Z-100102T2100Z/",
      "POLK-MANATEE-HARDEE-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER STORM WATCH REMAINS IN EFFECT FROM FRIDAY EVENING THROUGH SATURDAY AFTERNOON...",
      "A WINTER STORM WATCH REMAINS IN EFFECT FROM FRIDAY EVENING THROUGH SATURDAY AFTERNOON.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A WINTER STORM WATCH MEANS THERE IS A POTENTIAL FOR SIGNIFICANT SNOW...SLEET...OR ICE ACCUMULATIONS THAT MAY IMPACT TRAVEL. CONTINUE TO MONITOR THE LATEST FORECASTS.",
      "&&",
      "$$",
                     ],
    },
    {
    "commentary": "No changes are made to the existing hazards.", 
    "name": "GHG_Complex2_4",
    "productType": "Hazard_WSW_Local",
    "deleteGrids": [],        
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-042-043-010800-",
      "/O.CON.KTBW.WS.A.0001.100101T2100Z-100102T2100Z/",
      "LEVY-CITRUS-SUMTER-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER STORM WATCH REMAINS IN EFFECT FROM FRIDAY AFTERNOON THROUGH SATURDAY AFTERNOON...",
      "A WINTER STORM WATCH REMAINS IN EFFECT FROM FRIDAY AFTERNOON THROUGH SATURDAY AFTERNOON.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A WINTER STORM WATCH MEANS THERE IS A POTENTIAL FOR SIGNIFICANT SNOW...SLEET...OR ICE ACCUMULATIONS THAT MAY IMPACT TRAVEL. CONTINUE TO MONITOR THE LATEST FORECASTS.",
      "&&",
      "$$",
      "FLZ052-055>057-060-061-010800-",
      "/O.CON.KTBW.WS.A.0001.100102T0300Z-100102T2100Z/",
      "POLK-MANATEE-HARDEE-HIGHLANDS-SARASOTA-DE SOTO-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER STORM WATCH REMAINS IN EFFECT FROM FRIDAY EVENING THROUGH SATURDAY AFTERNOON...",
      "A WINTER STORM WATCH REMAINS IN EFFECT FROM FRIDAY EVENING THROUGH SATURDAY AFTERNOON.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A WINTER STORM WATCH MEANS THERE IS A POTENTIAL FOR SIGNIFICANT SNOW...SLEET...OR ICE ACCUMULATIONS THAT MAY IMPACT TRAVEL. CONTINUE TO MONITOR THE LATEST FORECASTS.",
      "&&",
      "$$",
                     ],
    },
    {
    
    "commentary": """
    Replacing the WS.A hazard in Area1 with a WS.W hazard
    Area1 (FLZ039, FLZ042, FLZ043) 21-45
    Replacing a portion of the WS.A hazard in Area2 with no hazards
    Area2 (FLZ052, FLZ055, FLZ056, FLZ057) 27-45
    Replacing a portion of the WS.A hazard in Area2 with a WW.Y hazard
    Area2 (FLZ060, FLZ061) 27-45
    """,
    
    "name": "GHG_Complex2_5",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 21, 27, "WS.W", ["FLZ039", "FLZ042", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 27, 45, "WS.W", ["FLZ039", "FLZ042", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 27, 45, "<None>", ["FLZ052", "FLZ055", "FLZ056", "FLZ057"]),
        ("Fcst", "Hazards", "DISCRETE", 27, 45, "WW.Y", ["FLZ060", "FLZ061"]), 
        ],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ052-055>057-010100-",
      "/O.CAN.KTBW.WS.A.0001.100102T0300Z-100102T2100Z/",
      "POLK-MANATEE-HARDEE-HIGHLANDS-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER STORM WATCH IS CANCELLED...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE WINTER STORM WATCH.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "$$",
      "FLZ039-042-043-010800-",
      "/O.UPG.KTBW.WS.A.0001.100101T2100Z-100102T2100Z/",
      "/O.NEW.KTBW.WS.W.0001.100101T2100Z-100102T2100Z/",
      "LEVY-CITRUS-SUMTER-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER STORM WARNING IN EFFECT FROM 4 PM FRIDAY TO 4 PM EST SATURDAY...",
#      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A WINTER STORM WARNING...WHICH IS IN EFFECT FROM 4 PM FRIDAY TO 4 PM EST SATURDAY. THE WINTER STORM WATCH IS NO LONGER IN EFFECT.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
#      "A WINTER STORM WARNING MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
      "&&",
      "$$",
      "FLZ060-061-010800-",
      "/O.UPG.KTBW.WS.A.0001.100102T0300Z-100102T2100Z/",
      "/O.NEW.KTBW.WW.Y.0001.100102T0300Z-100102T2100Z/",
      "SARASOTA-DE SOTO-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER WEATHER ADVISORY IN EFFECT FROM 10 PM FRIDAY TO 4 PM EST SATURDAY...",
#      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A WINTER WEATHER ADVISORY...WHICH IS IN EFFECT FROM 10 PM FRIDAY TO 4 PM EST SATURDAY. THE WINTER STORM WATCH IS NO LONGER IN EFFECT.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
#      "A WINTER WEATHER ADVISORY MEANS THAT PERIODS OF SNOW...SLEET...OR FREEZING RAIN WILL CAUSE TRAVEL DIFFICULTIES. BE PREPARED FOR SLIPPERY ROADS AND LIMITED VISIBILITIES...AND USE CAUTION WHILE DRIVING.",
      "&&",
      "$$",
                     ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex2_6",
    "productType": "Hazard_WSW_Local",
    "deleteGrids": [],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "/O.CON.KTBW.WS.W.0001.100101T2100Z-100102T2100Z/",
                     "LEVY-CITRUS-SUMTER-",
                     "...WINTER STORM WARNING REMAINS IN EFFECT FROM 4 PM FRIDAY TO 4 PM EST SATURDAY...",
                     "/O.CON.KTBW.WW.Y.0001.100102T0300Z-100102T2100Z/",
                     "SARASOTA-DE SOTO-",
                     "...WINTER WEATHER ADVISORY REMAINS IN EFFECT FROM 10 PM FRIDAY TO 4 PM EST SATURDAY...",
                     ],
    }, 
    {
    
    "commentary": """
    Changing the time of Area1
    Area1 (FLZ039, FLZ042, FLZ043) 0-45
    Changing the time of Area2
    Area2 (FLZ060, FLZ061) 0-45
    """,
    
    "name": "GHG_Complex2_7",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 45, "WS.W", ["FLZ039", "FLZ042", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 0, 45, "WW.Y", ["FLZ060", "FLZ061"]),
        ],
    "checkStrings": [
       "WWUS42 KTBW 010000",
       "WSWTBW",
       "URGENT - WINTER WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ039-042-043-010800-",
       "/O.EXT.KTBW.WS.W.0001.100101T0000Z-100102T2100Z/",
       "LEVY-CITRUS-SUMTER-",
       "700 PM EST THU DEC 31 2009",
       "...WINTER STORM WARNING NOW IN EFFECT UNTIL 4 PM EST SATURDAY...",
       "THE WINTER STORM WARNING IS NOW IN EFFECT UNTIL 4 PM EST SATURDAY.",
#       "|*|* SEGMENT TEXT GOES HERE *|.*|",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
#       "A WINTER STORM WARNING MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
       "&&",
       "$$",
       "FLZ060-061-010800-",
       "/O.EXT.KTBW.WW.Y.0001.100101T0000Z-100102T2100Z/",
       "SARASOTA-DE SOTO-",
       "700 PM EST THU DEC 31 2009",
       "...WINTER WEATHER ADVISORY NOW IN EFFECT UNTIL 4 PM EST SATURDAY...",
       "THE WINTER WEATHER ADVISORY IS NOW IN EFFECT UNTIL 4 PM EST SATURDAY.",
#       "|*|* SEGMENT TEXT GOES HERE *|.*|",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
#       "A WINTER WEATHER ADVISORY MEANS THAT PERIODS OF SNOW...SLEET...OR FREEZING RAIN WILL CAUSE TRAVEL DIFFICULTIES. BE PREPARED FOR SLIPPERY ROADS AND LIMITED VISIBILITIES...AND USE CAUTION WHILE DRIVING.",
       "&&",
       "$$",
       ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex2_8",
    "productType": "Hazard_WSW_Local",
    "deleteGrids": [],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-042-043-010800-",
      "/O.CON.KTBW.WS.W.0001.000000T0000Z-100102T2100Z/",
      "LEVY-CITRUS-SUMTER-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER STORM WARNING REMAINS IN EFFECT UNTIL 4 PM EST SATURDAY...",
      "A WINTER STORM WARNING REMAINS IN EFFECT UNTIL 4 PM EST SATURDAY.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
#      "A WINTER STORM WARNING MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
      "&&",
      "$$",
      "FLZ060-061-010800-",
      "/O.CON.KTBW.WW.Y.0001.000000T0000Z-100102T2100Z/",
      "SARASOTA-DE SOTO-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER WEATHER ADVISORY REMAINS IN EFFECT UNTIL 4 PM EST SATURDAY...",
      "A WINTER WEATHER ADVISORY REMAINS IN EFFECT UNTIL 4 PM EST SATURDAY.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
#      "A WINTER WEATHER ADVISORY MEANS THAT PERIODS OF SNOW...SLEET...OR FREEZING RAIN WILL CAUSE TRAVEL DIFFICULTIES. BE PREPARED FOR SLIPPERY ROADS AND LIMITED VISIBILITIES...AND USE CAUTION WHILE DRIVING.",
      "&&",
      "$$",
                     ],
    },
    {
    
    "commentary": """
    Removing a portion of the WS.W hazard from Area1 and setting it to no hazards
    Area1 (FLZ039) 0-45
    Removing the WW.Y hazard for all of Area2
    Area2 (FLZ060, FLZ061) 0-45
    """,

    "name": "GHG_Complex2_9",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 45, "<None>", ["FLZ039"]),
        ("Fcst", "Hazards", "DISCRETE", 0, 45, "WS.W", ["FLZ042", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 0, 45, "<None>", ["FLZ060", "FLZ061"]),
        ],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-010100-",
      "/O.CAN.KTBW.WS.W.0001.000000T0000Z-100102T2100Z/",
      "LEVY-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER STORM WARNING IS CANCELLED...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE WINTER STORM WARNING.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "$$",
      "FLZ060-061-010100-",
      "/O.CAN.KTBW.WW.Y.0001.000000T0000Z-100102T2100Z/",
      "SARASOTA-DE SOTO-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER WEATHER ADVISORY IS CANCELLED...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE WINTER WEATHER ADVISORY.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "$$",
      "FLZ042-043-010800-",
      "/O.CON.KTBW.WS.W.0001.000000T0000Z-100102T2100Z/",
      "CITRUS-SUMTER-",
      "700 PM EST THU DEC 31 2009",
      "...WINTER STORM WARNING REMAINS IN EFFECT UNTIL 4 PM EST SATURDAY...",
      "A WINTER STORM WARNING REMAINS IN EFFECT UNTIL 4 PM EST SATURDAY.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
#      "A WINTER STORM WARNING MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
      "&&",
      "$$",
                     ],
    },
    {
    "commentary": "Canceling out all hazards.",
    "name": "GHG_Complex2_10",
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








