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
    Area1 (FLZ139, FLZ239, FLZ142, FLZ242, FLZ043) 21-45
    Area2 (FLZ052, FLZ155, FLZ255, FLZ056) 27-45
    Note that we had to condense the times into two time ranges to accommodate for the overlapping
    """,
    
    "name": "GHG_Complex2_1",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 21, 27, "WS.A", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 27, 45, "WS.A", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 27, 45, "WS.A", ["FLZ052", "FLZ155", "FLZ255", "FLZ056"]),
        ],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ043-139-142-239-242-010800-",
      "/O.NEW.KTBW.WS.A.0001.100101T2100Z-100102T2100Z/",
      "SUMTER-COASTAL LEVY-COASTAL CITRUS-INLAND LEVY-INLAND CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Storm Watch in effect from Friday afternoon through Saturday afternoon...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch...which is in effect from Friday afternoon through Saturday afternoon.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "A Winter Storm Watch means there is a potential for significant snow...sleet...or ice accumulations that may impact travel. Continue to monitor the latest forecasts.",
      "$$",
      "FLZ052-056-155-255-010800-",
      "/O.NEW.KTBW.WS.A.0001.100102T0300Z-100102T2100Z/",
      "POLK-HARDEE-COASTAL MANATEE-INLAND MANATEE-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Storm Watch in effect from Friday evening through Saturday afternoon...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch...which is in effect from Friday evening through Saturday afternoon.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "Precautionary/preparedness actions...",
      "A Winter Storm Watch means there is a potential for significant snow...sleet...or ice accumulations that may impact travel. Continue to monitor the latest forecasts.",
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
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ043-139-142-239-242-010800-",
      "/O.CON.KTBW.WS.A.0001.100101T2100Z-100102T2100Z/",
      "SUMTER-COASTAL LEVY-COASTAL CITRUS-INLAND LEVY-INLAND CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Storm Watch remains in effect from Friday afternoon through Saturday afternoon...",
#       "A Winter Storm Watch remains in effect from Friday afternoon through Saturday afternoon.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "A Winter Storm Watch means there is a potential for significant snow...sleet...or ice accumulations that may impact travel. Continue to monitor the latest forecasts.",
      "$$",
      "FLZ052-056-155-255-010800-",
      "/O.CON.KTBW.WS.A.0001.100102T0300Z-100102T2100Z/",
      "POLK-HARDEE-COASTAL MANATEE-INLAND MANATEE-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Storm Watch remains in effect from Friday evening through Saturday afternoon...",
#       "A Winter Storm Watch remains in effect from Friday evening through Saturday afternoon.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "Precautionary/preparedness actions...",
      "A Winter Storm Watch means there is a potential for significant snow...sleet...or ice accumulations that may impact travel. Continue to monitor the latest forecasts.",
      "&&",
      "$$",
                     ],
    },
    {
    
    "commentary": """
    Expanding the WS.A hazard of Area2
    Area2 (FLZ052, FLZ155, FLZ255, FLZ056, FLZ057, FLZ160, FLZ260, FLZ061) 27-45
    """,
    
    "name": "GHG_Complex2_3",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 21, 27, "WS.A", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 27, 45, "WS.A", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 27, 45, "WS.A", ["FLZ052", "FLZ155", "FLZ255", "FLZ056", "FLZ057", "FLZ160", "FLZ260", "FLZ061"]),
        ],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ057-061-160-260-010800-",
      "/O.EXA.KTBW.WS.A.0001.100102T0300Z-100102T2100Z/",
      "HIGHLANDS-DESOTO-COASTAL SARASOTA-INLAND SARASOTA-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Storm Watch in effect from Friday evening through Saturday afternoon...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch...which is in effect from Friday evening through Saturday afternoon.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "Precautionary/preparedness actions...",
      "A Winter Storm Watch means there is a potential for significant snow...sleet...or ice accumulations that may impact travel. Continue to monitor the latest forecasts.",
      "&&",
      "$$",
      "FLZ043-139-142-239-242-010800-",
      "/O.CON.KTBW.WS.A.0001.100101T2100Z-100102T2100Z/",
      "SUMTER-COASTAL LEVY-COASTAL CITRUS-INLAND LEVY-INLAND CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Storm Watch remains in effect from Friday afternoon through Saturday afternoon...",
#       "A Winter Storm Watch remains in effect from Friday afternoon through Saturday afternoon.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "A Winter Storm Watch means there is a potential for significant snow...sleet...or ice accumulations that may impact travel. Continue to monitor the latest forecasts.",
      "$$",
      "FLZ052-056-155-255-010800-",
      "/O.CON.KTBW.WS.A.0001.100102T0300Z-100102T2100Z/",
      "POLK-HARDEE-COASTAL MANATEE-INLAND MANATEE-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Storm Watch remains in effect from Friday evening through Saturday afternoon...",
#       "A Winter Storm Watch remains in effect from Friday evening through Saturday afternoon.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "Precautionary/preparedness actions...",
      "A Winter Storm Watch means there is a potential for significant snow...sleet...or ice accumulations that may impact travel. Continue to monitor the latest forecasts.",
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
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ043-139-142-239-242-010800-",
      "/O.CON.KTBW.WS.A.0001.100101T2100Z-100102T2100Z/",
      "SUMTER-COASTAL LEVY-COASTAL CITRUS-INLAND LEVY-INLAND CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Storm Watch remains in effect from Friday afternoon through Saturday afternoon...",
#       "A Winter Storm Watch remains in effect from Friday afternoon through Saturday afternoon.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "Precautionary/preparedness actions...",
      "A Winter Storm Watch means there is a potential for significant snow...sleet...or ice accumulations that may impact travel. Continue to monitor the latest forecasts.",
      "&&",
      "$$",
      "FLZ052-056-057-061-155-160-255-260-010800-",
      "/O.CON.KTBW.WS.A.0001.100102T0300Z-100102T2100Z/",
      "POLK-HARDEE-HIGHLANDS-DESOTO-COASTAL MANATEE-COASTAL SARASOTA-",
      "INLAND MANATEE-INLAND SARASOTA-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Storm Watch remains in effect from Friday evening through Saturday afternoon...",
#       "A Winter Storm Watch remains in effect from Friday evening through Saturday afternoon.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "Precautionary/preparedness actions...",
      "A Winter Storm Watch means there is a potential for significant snow...sleet...or ice accumulations that may impact travel. Continue to monitor the latest forecasts.",
      "&&",
      "$$",
                     ],
    },
    {
    
    "commentary": """
    Replacing the WS.A hazard in Area1 with a WS.W hazard
    Area1 (FLZ139, FLZ239, FLZ142, FLZ242, FLZ043) 21-45
    Replacing a portion of the WS.A hazard in Area2 with no hazards
    Area2 (FLZ052, FLZ155, FLZ255, FLZ056, FLZ057) 27-45
    Replacing a portion of the WS.A hazard in Area2 with a WW.Y hazard
    Area2 (FLZ160, FLZ260, FLZ061) 27-45
    """,
    
    "name": "GHG_Complex2_5",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 21, 27, "WS.W", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 27, 45, "WS.W", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 27, 45, "<None>", ["FLZ052", "FLZ155", "FLZ255", "FLZ056", "FLZ057"]),
        ("Fcst", "Hazards", "DISCRETE", 27, 45, "WW.Y", ["FLZ160", "FLZ260", "FLZ061"]), 
        ],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ052-056-057-155-255-010100-",
      "/O.CAN.KTBW.WS.A.0001.100102T0300Z-100102T2100Z/",
      "POLK-HARDEE-HIGHLANDS-COASTAL MANATEE-INLAND MANATEE-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Storm Watch is cancelled...",
      "The National Weather Service in Tampa Bay Ruskin has cancelled the Winter Storm Watch.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "$$",
      "FLZ043-139-142-239-242-010800-",
      "/O.UPG.KTBW.WS.A.0001.100101T2100Z-100102T2100Z/",
      "/O.NEW.KTBW.WS.W.0001.100101T2100Z-100102T2100Z/",
      "SUMTER-COASTAL LEVY-COASTAL CITRUS-INLAND LEVY-INLAND CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Storm Warning in effect from 4 PM Friday to 4 PM EST Saturday...",
#      "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning...which is in effect from 4 PM Friday to 4 PM EST Saturday. The Winter Storm Watch IS NO LONGER IN EFFECT.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "Precautionary/preparedness actions...",
#      "A Winter Storm Warning means significant amounts of snow...sleet...and ice are expected or occurring. Strong winds are also possible. This will make travel very hazardous or impossible.",
      "&&",
      "$$",
      "FLZ061-160-260-010800-",
      "/O.UPG.KTBW.WS.A.0001.100102T0300Z-100102T2100Z/",
      "/O.NEW.KTBW.WW.Y.0001.100102T0300Z-100102T2100Z/",
      "DESOTO-COASTAL SARASOTA-INLAND SARASOTA-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Weather Advisory in effect from 10 PM Friday to 4 PM EST Saturday...",
#      "The National Weather Service in Tampa Bay Ruskin has issued a Winter Weather Advisory...which is in effect from 10 PM Friday to 4 PM EST Saturday. The Winter Storm Watch IS NO LONGER IN EFFECT.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "Precautionary/preparedness actions...",
#      "A Winter Weather Advisory means that periods of snow...sleet...or freezing rain will cause travel difficulties. Be prepared for slippery roads and limited visibilities...and use caution while driving.",
      "&&",
      "$$",
      ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex2_6",
    "productType": "Hazard_WSW_Local",
    "deleteGrids": [],
    "checkStrings": [
      "URGENT - WINTER WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "FLZ043-139-142-239-242-010800-",
      "/O.CON.KTBW.WS.W.0001.100101T2100Z-100102T2100Z/",
      "SUMTER-COASTAL LEVY-COASTAL CITRUS-INLAND LEVY-INLAND CITRUS-",
      "...Winter Storm Warning remains in effect from 4 PM Friday to 4 PM EST Saturday...",
      
      "FLZ061-160-260-010800-",
      "/O.CON.KTBW.WW.Y.0001.100102T0300Z-100102T2100Z/",
      "DESOTO-COASTAL SARASOTA-INLAND SARASOTA-",
      "...Winter Weather Advisory remains in effect from 10 PM Friday to 4 PM EST Saturday...",
      ],
    }, 
    {
    
    "commentary": """
    Changing the time of Area1
    Area1 (FLZ139, FLZ239, FLZ142, FLZ242, FLZ043) 0-45
    Changing the time of Area2
    Area2 (FLZ160, FLZ260, FLZ061) 0-45
    """,
    
    "name": "GHG_Complex2_7",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 45, "WS.W", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 0, 45, "WW.Y", ["FLZ160", "FLZ260", "FLZ061"]),
        ],
    "checkStrings": [
       "WWUS42 KTBW 010000",
       "WSWTBW",
       "URGENT - WINTER WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ043-139-142-239-242-010800-",
       "/O.EXT.KTBW.WS.W.0001.100101T0000Z-100102T2100Z/",
       "SUMTER-COASTAL LEVY-COASTAL CITRUS-INLAND LEVY-INLAND CITRUS-",
       "700 PM EST THU DEC 31 2009",
       "...Winter Storm Warning now in effect until 4 PM EST Saturday...",
#        "The Winter Storm Warning is now in effect until 4 PM EST Saturday.",
#       "|*|* SEGMENT TEXT GOES HERE *|.*|",
       "Precautionary/preparedness actions...",
#       "A Winter Storm Warning means significant amounts of snow...sleet...and ice are expected or occurring. Strong winds are also possible. This will make travel very hazardous or impossible.",
       "&&",
       "$$",
       "FLZ061-160-260-010800-",
       "/O.EXT.KTBW.WW.Y.0001.100101T0000Z-100102T2100Z/",
       "DESOTO-COASTAL SARASOTA-INLAND SARASOTA-",
       "700 PM EST THU DEC 31 2009",
       "...Winter Weather Advisory now in effect until 4 PM EST Saturday...",
#        "The Winter Weather Advisory is now in effect until 4 PM EST Saturday.",
#       "|*|* SEGMENT TEXT GOES HERE *|.*|",
       "Precautionary/preparedness actions...",
#       "A Winter Weather Advisory means that periods of snow...sleet...or freezing rain will cause travel difficulties. Be prepared for slippery roads and limited visibilities...and use caution while driving.",
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
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ043-139-142-239-242-010800-",
      "/O.CON.KTBW.WS.W.0001.000000T0000Z-100102T2100Z/",
      "SUMTER-COASTAL LEVY-COASTAL CITRUS-INLAND LEVY-INLAND CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Storm Warning remains in effect until 4 PM EST Saturday...",
#       "A Winter Storm Warning remains in effect until 4 PM EST Saturday.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "Precautionary/preparedness actions...",
#      "A Winter Storm Warning means significant amounts of snow...sleet...and ice are expected or occurring. Strong winds are also possible. This will make travel very hazardous or impossible.",
      "&&",
      "$$",
      "FLZ061-160-260-010800-",
      "/O.CON.KTBW.WW.Y.0001.000000T0000Z-100102T2100Z/",
      "DESOTO-COASTAL SARASOTA-INLAND SARASOTA-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Weather Advisory remains in effect until 4 PM EST Saturday...",
#       "A Winter Weather Advisory remains in effect until 4 PM EST Saturday.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "Precautionary/preparedness actions...",
#      "A Winter Weather Advisory means that periods of snow...sleet...or freezing rain will cause travel difficulties. Be prepared for slippery roads and limited visibilities...and use caution while driving.",
      "&&",
      "$$",
                     ],
    },
    {
    
    "commentary": """
    Removing a portion of the WS.W hazard from Area1 and setting it to no hazards
    Area1 (FLZ139, FLZ239) 0-45
    Removing the WW.Y hazard for all of Area2
    Area2 (FLZ160, FLZ260, FLZ061) 0-45
    """,

    "name": "GHG_Complex2_9",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 45, "<None>", ["FLZ139", "FLZ239"]),
        ("Fcst", "Hazards", "DISCRETE", 0, 45, "WS.W", ["FLZ142", "FLZ242", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 0, 45, "<None>", ["FLZ160", "FLZ260", "FLZ061"]),
        ],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ139-239-010100-",
      "/O.CAN.KTBW.WS.W.0001.000000T0000Z-100102T2100Z/",
      "COASTAL LEVY-INLAND LEVY-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Storm Warning is cancelled...",
      "The National Weather Service in Tampa Bay Ruskin has cancelled the Winter Storm Warning.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "$$",
      "FLZ061-160-260-010100-",
      "/O.CAN.KTBW.WW.Y.0001.000000T0000Z-100102T2100Z/",
      "DESOTO-COASTAL SARASOTA-INLAND SARASOTA-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Weather Advisory is cancelled...",
      "The National Weather Service in Tampa Bay Ruskin has cancelled the Winter Weather Advisory.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "$$",
      "FLZ043-142-242-010800-",
      "/O.CON.KTBW.WS.W.0001.000000T0000Z-100102T2100Z/",
      "SUMTER-COASTAL CITRUS-INLAND CITRUS-",
      "700 PM EST THU DEC 31 2009",
      "...Winter Storm Warning remains in effect until 4 PM EST Saturday...",
#       "A Winter Storm Warning remains in effect until 4 PM EST Saturday.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "Precautionary/preparedness actions...",
#      "A Winter Storm Warning means significant amounts of snow...sleet...and ice are expected or occurring. Strong winds are also possible. This will make travel very hazardous or impossible.",
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
        "database": "<site>_GRID__Fcst_00000000_0000",
        "publishGrids": 0,
        "decodeVTEC": 1,
        "gridsStartTime": "20100101_0000",
        "orderStrings": 1,
        "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)








