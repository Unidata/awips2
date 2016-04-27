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
# More VTEC_GHG Complex Hazard Tests! 
#
# Author:
# ----------------------------------------------------------------------------

scripts = [
    {
    "commentary": "Clear out all Hazards Table and Grids.", 
    "name": "GHG_Complex4_0",
    "productType": None,
    "clearHazardsTable" : 1,
    "checkStrings": [],
    },
    {

    "commentary": """
    Creating a FG.Y hazard for Area1 and Area2
    Area1 (FLZ139, FLZ239, FLZ142, FLZ242, FLZ043) 0-1
    Area2 (FLZ148, FLZ248, FLZ149, FLZ249, FLZ050) 1-24
    """,
    
    "name": "GHG_Complex4_1",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 1, "FG.Y", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 1, 24, "FG.Y", ["FLZ148", "FLZ248", "FLZ149", "FLZ249", "FLZ050"]),
        ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ043-139-142-239-242-010100-",
      "/O.NEW.KTBW.FG.Y.0001.100101T0000Z-100101T0100Z/",
      "Sumter-Coastal Levy-Coastal Citrus-Inland Levy-Inland Citrus-",
      "700 PM EST Thu Dec 31 2009",
      "...DENSE FOG ADVISORY IN EFFECT UNTIL 8 PM EST THIS EVENING...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Dense Fog Advisory...which is in effect until 8 PM EST this evening.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Dense Fog Advisory means visibilities will frequently be reduced to less than one quarter mile. If driving...slow down...use your headlights...and leave plenty of distance ahead of you.",
      "&&",
      "$$",
      "FLZ050-148-149-248-249-010800-",
      "/O.NEW.KTBW.FG.Y.0001.100101T0100Z-100102T0000Z/",
      "Pinellas-Coastal Hernando-Coastal Pasco-Inland Hernando-",
      "Inland Pasco-",
      "700 PM EST Thu Dec 31 2009",
      "...DENSE FOG ADVISORY IN EFFECT UNTIL 7 PM EST FRIDAY...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Dense Fog Advisory...which is in effect until 7 PM EST Friday.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Dense Fog Advisory means visibilities will frequently be reduced to less than one quarter mile. If driving...slow down...use your headlights...and leave plenty of distance ahead of you.",
      "&&",
      "$$",
                     ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex4_2",
    "productType": "Hazard_NPW_Local",
    "deleteGrids": [],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ043-139-142-239-242-010100-",
      "/O.CON.KTBW.FG.Y.0001.000000T0000Z-100101T0100Z/",
      "Sumter-Coastal Levy-Coastal Citrus-Inland Levy-Inland Citrus-",
      "700 PM EST Thu Dec 31 2009",
      "...DENSE FOG ADVISORY REMAINS IN EFFECT UNTIL 8 PM EST THIS EVENING...",
#       "A Dense Fog Advisory remains in effect until 8 PM EST this evening.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Dense Fog Advisory means visibilities will frequently be reduced to less than one quarter mile. If driving...slow down...use your headlights...and leave plenty of distance ahead of you.",
      "&&",
      "$$",
      "FLZ050-148-149-248-249-010800-",
      "/O.CON.KTBW.FG.Y.0001.100101T0100Z-100102T0000Z/",
      "Pinellas-Coastal Hernando-Coastal Pasco-Inland Hernando-",
      "Inland Pasco-",
      "700 PM EST Thu Dec 31 2009",
      "...DENSE FOG ADVISORY REMAINS IN EFFECT UNTIL 7 PM EST FRIDAY...",
#       "A Dense Fog Advisory remains in effect until 7 PM EST Friday.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Dense Fog Advisory means visibilities will frequently be reduced to less than one quarter mile. If driving...slow down...use your headlights...and leave plenty of distance ahead of you.",
      "&&",
      "$$",
                     ],      
    },
    {

    "commentary": """
    Creating a new area (Area3) with a HW.A hazard
    Note that we jumped ahead in time 1 hour in order for the FG.Y hazard in Area1 to expire
    Area3 (FLZ043, FLZ052, FLZ057) 33-75
    Area1 (FLZ139, FLZ239, FLZ142, FLZ242, FLZ043) 0-1
    """,
    
    "name": "GHG_Complex4_3",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 1, "FG.Y", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 1, 24, "FG.Y", ["FLZ148", "FLZ248", "FLZ149", "FLZ249", "FLZ050"]),
        ("Fcst", "Hazards", "DISCRETE", 33, 75, "HW.A", ["FLZ043", "FLZ052", "FLZ057"]),
        ],
    "checkStrings": [
      "WWUS72 KTBW 010115",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "815 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ043-010915-",
      "/O.EXP.KTBW.FG.Y.0001.000000T0000Z-100101T0100Z/",
      "/O.NEW.KTBW.HW.A.0001.100102T0900Z-100104T0300Z/",
      "Sumter-",
      "815 PM EST Thu Dec 31 2009",
      "...HIGH WIND WATCH IN EFFECT FROM LATE FRIDAY NIGHT THROUGH SUNDAY EVENING...",
      "...DENSE FOG ADVISORY HAS EXPIRED...",
      "The National Weather Service in Tampa Bay Ruskin has issued a High Wind Watch...which is in effect from late Friday night through Sunday evening.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A High Wind Watch means there is the potential for a hazardous high wind event. Sustained winds of at least 40 mph...or gusts of 58 mph or stronger may occur. Continue to monitor the latest forecasts.",
      "&&",
      "$$",
      "FLZ139-142-239-242-010215-",
      "/O.EXP.KTBW.FG.Y.0001.000000T0000Z-100101T0100Z/",
      "Coastal Levy-Coastal Citrus-Inland Levy-Inland Citrus-",
      "815 PM EST Thu Dec 31 2009",
      "...DENSE FOG ADVISORY HAS EXPIRED...",
#       "The Dense Fog Advisory is no longer in effect.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "$$",
      "FLZ050-148-149-248-249-010915-",
      "/O.CON.KTBW.FG.Y.0001.000000T0000Z-100102T0000Z/",
      "Pinellas-Coastal Hernando-Coastal Pasco-Inland Hernando-",
      "Inland Pasco-",
      "815 PM EST Thu Dec 31 2009",
      "...DENSE FOG ADVISORY REMAINS IN EFFECT UNTIL 7 PM EST FRIDAY...",
#       "A Dense Fog Advisory remains in effect until 7 PM EST Friday.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Dense Fog Advisory means visibilities will frequently be reduced to less than one quarter mile. If driving...slow down...use your headlights...and leave plenty of distance ahead of you.",
      "&&",
      "$$",
      "FLZ052-057-010915-",
      "/O.NEW.KTBW.HW.A.0001.100102T0900Z-100104T0300Z/",
      "Polk-Highlands-",
      "815 PM EST Thu Dec 31 2009",
      "...HIGH WIND WATCH IN EFFECT FROM LATE FRIDAY NIGHT THROUGH SUNDAY EVENING...",
      "The National Weather Service in Tampa Bay Ruskin has issued a High Wind Watch...which is in effect from late Friday night through Sunday evening.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A High Wind Watch means there is the potential for a hazardous high wind event. Sustained winds of at least 40 mph...or gusts of 58 mph or stronger may occur. Continue to monitor the latest forecasts.",
      "&&",
      "$$",
                     ],
    "drtTime":"20100101_0115",
    },
    {
    
    "commentary": """
    Removing Area2 and the FG.Y hazard
    Area2 (FLZ148, FLZ248, FLZ149, FLZ249, FLZ050) 1-24
    Upgrading the HW.A hazard in Area3 to a HW.W hazard
    Area3 (FLZ043, FLZ052, FLZ057) 33-75
    """,
    
    "name": "GHG_Complex4_4",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 1, "FG.Y", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 1, 24, "<None>", ["FLZ148", "FLZ248", "FLZ149", "FLZ249", "FLZ050"]),
        ("Fcst", "Hazards", "DISCRETE", 33, 75, "HW.W", ["FLZ043", "FLZ052", "FLZ057"]),
        ],        
    "checkStrings": [
      "WWUS72 KTBW 010115",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "815 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ050-148-149-248-249-010215-",
      "/O.CAN.KTBW.FG.Y.0001.000000T0000Z-100102T0000Z/",
      "Pinellas-Coastal Hernando-Coastal Pasco-Inland Hernando-",
      "Inland Pasco-",
      "815 PM EST Thu Dec 31 2009",
      "...DENSE FOG ADVISORY IS CANCELLED...",
      "The National Weather Service in Tampa Bay Ruskin has cancelled the Dense Fog Advisory.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "$$",
      "FLZ043-052-057-010915-",
      "/O.UPG.KTBW.HW.A.0001.100102T0900Z-100104T0300Z/",
      "/O.NEW.KTBW.HW.W.0001.100102T0900Z-100104T0300Z/",
      "Sumter-Polk-Highlands-",
      "815 PM EST Thu Dec 31 2009",
      "...HIGH WIND WARNING IN EFFECT FROM 4 AM SATURDAY TO 10 PM EST SUNDAY...",
      "The National Weather Service in Tampa Bay Ruskin has issued a High Wind Warning...which is in effect from 4 AM Saturday to 10 PM EST Sunday. The High Wind Watch is no longer in effect.",
#      "|*|*|* SEGMENT TEXT GOES HERE *|.*|*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A High Wind Warning means a hazardous high wind event is expected or occurring. Sustained wind speeds of at least 40 mph or gusts of 58 mph or more can lead to property damage.",
      "&&",
      "$$",
                     ],
    "drtTime":"20100101_0115",
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex4_5",
    "productType": "Hazard_NPW_Local",
    "deleteGrids": [],
    "checkStrings": [
      "WWUS72 KTBW 010115",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "815 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ043-052-057-010915-",
      "/O.CON.KTBW.HW.W.0001.100102T0900Z-100104T0300Z/",
      "Sumter-Polk-Highlands-",
      "815 PM EST Thu Dec 31 2009",
      "...HIGH WIND WARNING REMAINS IN EFFECT FROM 4 AM SATURDAY TO 10 PM EST SUNDAY...",
#       "A High Wind Warning remains in effect from 4 AM Saturday to 10 PM EST Sunday.",
#      "|*|*|* SEGMENT TEXT GOES HERE *|.*|*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A High Wind Warning means a hazardous high wind event is expected or occurring. Sustained wind speeds of at least 40 mph or gusts of 58 mph or more can lead to property damage.",
      "&&",
      "$$",
                  ],
    "drtTime":"20100101_0115",
    },
    {
    
    "commentary": """
    Reducing a portion of the HW.W hazard in Area3 and setting it to no hazards
    Area3 (FLZ057) 33-75
    """,
    
    "name": "GHG_Complex4_6",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 1, "FG.Y", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 33, 75, "HW.W", ["FLZ043", "FLZ052"]),
        ("Fcst", "Hazards", "DISCRETE", 33, 75, "<None>", ["FLZ057"]),
        ],
    "checkStrings": [
       "WWUS72 KTBW 010115",
       "NPWTBW",
       "URGENT - WEATHER MESSAGE",
       "National Weather Service Tampa Bay Ruskin FL",
       "815 PM EST Thu Dec 31 2009",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ057-010215-",
       "/O.CAN.KTBW.HW.W.0001.100102T0900Z-100104T0300Z/",
       "Highlands-",
       "815 PM EST Thu Dec 31 2009",
       "...HIGH WIND WARNING IS CANCELLED...",
       "The National Weather Service in Tampa Bay Ruskin has cancelled the High Wind Warning.",
#       "|* SEGMENT TEXT GOES HERE *|.",
       "$$",
       "FLZ043-052-010915-",
       "/O.CON.KTBW.HW.W.0001.100102T0900Z-100104T0300Z/",
       "Sumter-Polk-",
       "815 PM EST Thu Dec 31 2009",
       "...HIGH WIND WARNING REMAINS IN EFFECT FROM 4 AM SATURDAY TO 10 PM EST SUNDAY...",
#        "A High Wind Warning remains in effect from 4 AM Saturday to 10 PM EST Sunday.",
#       "|*|*|* SEGMENT TEXT GOES HERE *|.*|*|",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A High Wind Warning means a hazardous high wind event is expected or occurring. Sustained wind speeds of at least 40 mph or gusts of 58 mph or more can lead to property damage.",
       "&&",
       "$$",
                     ],
    "drtTime":"20100101_0115",
    }, 
    {

    "commentary": """
    Replacing the HW.W hazard in Area3 with no hazards
    Area3 (FLZ043, FLZ052) 33-75
    """,
    
    "name": "GHG_Complex4_7",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 1, "FG.Y", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 33, 75, "<None>", ["FLZ043", "FLZ052"]),
        ],
    "checkStrings": [
      "WWUS72 KTBW 010115",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "815 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ043-052-010215-",
      "/O.CAN.KTBW.HW.W.0001.100102T0900Z-100104T0300Z/",
      "Sumter-Polk-",
#       "INCLUDING THE CITIES OF...WILDWOOD...LAKE PANASOFFKEE...",
#       "BUSHNELL...LAKELAND...WINTER HAVEN",
      "815 PM EST Thu Dec 31 2009",
      "...HIGH WIND WARNING IS CANCELLED...",
      "The National Weather Service in Tampa Bay Ruskin has cancelled the High Wind Warning.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "$$",
                     ],
    "drtTime":"20100101_0115",
    },
    {
    "commentary": "Canceling out all hazards.",
    "name": "GHG_Complex4_8",
    "productType": None,
    "checkStrings": [],
    "clearHazardsTable": 1,
    "drtTime":"20100101_0115",
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













