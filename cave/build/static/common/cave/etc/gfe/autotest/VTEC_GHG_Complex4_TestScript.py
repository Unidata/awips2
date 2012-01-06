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
    Area1 (FLZ039, FLZ042, FLZ043) 0-1
    Area2 (FLZ048, FLZ049, FLZ050) 1-24
    """,
    
    "name": "GHG_Complex4_1",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 1, "FG.Y", ["FLZ039", "FLZ042", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 1, 24, "FG.Y", ["FLZ048", "FLZ049", "FLZ050"]),
        ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-042-043-010100-",
      "/O.NEW.KTBW.FG.Y.0001.100101T0000Z-100101T0100Z/",
      "LEVY-CITRUS-SUMTER-",
      "700 PM EST THU DEC 31 2009",
      "...DENSE FOG ADVISORY IN EFFECT UNTIL 8 PM EST THIS EVENING...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A DENSE FOG ADVISORY...WHICH IS IN EFFECT UNTIL 8 PM EST THIS EVENING.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A DENSE FOG ADVISORY MEANS VISIBILITIES WILL FREQUENTLY BE REDUCED TO LESS THAN ONE QUARTER MILE. IF DRIVING...SLOW DOWN...USE YOUR HEADLIGHTS...AND LEAVE PLENTY OF DISTANCE AHEAD OF YOU",
      "&&",
      "$$",
      "FLZ048>050-010800-",
      "/O.NEW.KTBW.FG.Y.0001.100101T0100Z-100102T0000Z/",
      "HERNANDO-PASCO-PINELLAS-",
      "700 PM EST THU DEC 31 2009",
      "...DENSE FOG ADVISORY IN EFFECT UNTIL 7 PM EST FRIDAY...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A DENSE FOG ADVISORY...WHICH IS IN EFFECT UNTIL 7 PM EST FRIDAY.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A DENSE FOG ADVISORY MEANS VISIBILITIES WILL FREQUENTLY BE REDUCED TO LESS THAN ONE QUARTER MILE. IF DRIVING...SLOW DOWN...USE YOUR HEADLIGHTS...AND LEAVE PLENTY OF DISTANCE AHEAD OF YOU",
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
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ039-042-043-010100-",
      "/O.CON.KTBW.FG.Y.0001.000000T0000Z-100101T0100Z/",
      "LEVY-CITRUS-SUMTER-",
      "700 PM EST THU DEC 31 2009",
      "...DENSE FOG ADVISORY REMAINS IN EFFECT UNTIL 8 PM EST THIS EVENING...",
      "A DENSE FOG ADVISORY REMAINS IN EFFECT UNTIL 8 PM EST THIS EVENING.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A DENSE FOG ADVISORY MEANS VISIBILITIES WILL FREQUENTLY BE REDUCED TO LESS THAN ONE QUARTER MILE. IF DRIVING...SLOW DOWN...USE YOUR HEADLIGHTS...AND LEAVE PLENTY OF DISTANCE AHEAD OF YOU",
      "&&",
      "$$",
      "FLZ048>050-010800-",
      "/O.CON.KTBW.FG.Y.0001.100101T0100Z-100102T0000Z/",
      "HERNANDO-PASCO-PINELLAS-",
      "700 PM EST THU DEC 31 2009",
      "...DENSE FOG ADVISORY REMAINS IN EFFECT UNTIL 7 PM EST FRIDAY...",
      "A DENSE FOG ADVISORY REMAINS IN EFFECT UNTIL 7 PM EST FRIDAY.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A DENSE FOG ADVISORY MEANS VISIBILITIES WILL FREQUENTLY BE REDUCED TO LESS THAN ONE QUARTER MILE. IF DRIVING...SLOW DOWN...USE YOUR HEADLIGHTS...AND LEAVE PLENTY OF DISTANCE AHEAD OF YOU",
      "&&",
      "$$",
                     ],      
    },
    {

    "commentary": """
    Creating a new area (Area3) with a HW.A hazard
    Note that we jumped ahead in time 1 hour in order for the FG.Y hazard in Area1 to expire
    Area3 (FLZ043, FLZ052, FLZ057) 33-75
    Area1 (FLZ039, FLZ042, FLZ043) 0-1
    """,
    
    "name": "GHG_Complex4_3",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 1, "FG.Y", ["FLZ039", "FLZ042", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 1, 24, "FG.Y", ["FLZ048", "FLZ049", "FLZ050"]),
        ("Fcst", "Hazards", "DISCRETE", 33, 75, "HW.A", ["FLZ043", "FLZ052", "FLZ057"]),
        ],
    "checkStrings": [
      "WWUS72 KTBW 010115",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "815 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ043-010915-",
      "/O.EXP.KTBW.FG.Y.0001.000000T0000Z-100101T0100Z/",
      "/O.NEW.KTBW.HW.A.0001.100102T0900Z-100104T0300Z/",
      "SUMTER-",
      "815 PM EST THU DEC 31 2009",
      "...HIGH WIND WATCH IN EFFECT FROM LATE FRIDAY NIGHT THROUGH SUNDAY EVENING...",
      "...DENSE FOG ADVISORY HAS EXPIRED...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HIGH WIND WATCH...WHICH IS IN EFFECT FROM LATE FRIDAY NIGHT THROUGH SUNDAY EVENING. THE DENSE FOG ADVISORY IS NO LONGER IN EFFECT.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A HIGH WIND WATCH MEANS THERE IS THE POTENTIAL FOR A HAZARDOUS HIGH WIND EVENT. SUSTAINED WINDS OF AT LEAST 40 MPH...OR GUSTS OF 58 MPH OR STRONGER MAY OCCUR. CONTINUE TO MONITOR THE LATEST FORECASTS.",
      "&&",
      "$$",
      "FLZ039-042-010215-",
      "/O.EXP.KTBW.FG.Y.0001.000000T0000Z-100101T0100Z/",
      "LEVY-CITRUS-",
      "815 PM EST THU DEC 31 2009",
      "...DENSE FOG ADVISORY HAS EXPIRED...",
      "THE DENSE FOG ADVISORY IS NO LONGER IN EFFECT.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "$$",
      "FLZ048>050-010915-",
      "/O.CON.KTBW.FG.Y.0001.000000T0000Z-100102T0000Z/",
      "HERNANDO-PASCO-PINELLAS-",
      "815 PM EST THU DEC 31 2009",
      "...DENSE FOG ADVISORY REMAINS IN EFFECT UNTIL 7 PM EST FRIDAY...",
      "A DENSE FOG ADVISORY REMAINS IN EFFECT UNTIL 7 PM EST FRIDAY.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A DENSE FOG ADVISORY MEANS VISIBILITIES WILL FREQUENTLY BE REDUCED TO LESS THAN ONE QUARTER MILE. IF DRIVING...SLOW DOWN...USE YOUR HEADLIGHTS...AND LEAVE PLENTY OF DISTANCE AHEAD OF YOU",
      "&&",
      "$$",
      "FLZ052-057-010915-",
      "/O.NEW.KTBW.HW.A.0001.100102T0900Z-100104T0300Z/",
      "POLK-HIGHLANDS-",
      "815 PM EST THU DEC 31 2009",
      "...HIGH WIND WATCH IN EFFECT FROM LATE FRIDAY NIGHT THROUGH SUNDAY EVENING...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HIGH WIND WATCH...WHICH IS IN EFFECT FROM LATE FRIDAY NIGHT THROUGH SUNDAY EVENING.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A HIGH WIND WATCH MEANS THERE IS THE POTENTIAL FOR A HAZARDOUS HIGH WIND EVENT. SUSTAINED WINDS OF AT LEAST 40 MPH...OR GUSTS OF 58 MPH OR STRONGER MAY OCCUR. CONTINUE TO MONITOR THE LATEST FORECASTS.",
      "&&",
      "$$",
                     ],
    "drtTime":"20100101_0115",
    },
    {
    
    "commentary": """
    Removing Area2 and the FG.Y hazard
    Area2 (FLZ048, FLZ049, FLZ050) 1-24
    Upgrading the HW.A hazard in Area3 to a HW.W hazard
    Area3 (FLZ043, FLZ052, FLZ057) 33-75
    """,
    
    "name": "GHG_Complex4_4",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 1, "FG.Y", ["FLZ039", "FLZ042", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 1, 24, "<None>", ["FLZ048", "FLZ049", "FLZ050"]),
        ("Fcst", "Hazards", "DISCRETE", 33, 75, "HW.W", ["FLZ043", "FLZ052", "FLZ057"]),
        ],        
    "checkStrings": [
      "WWUS72 KTBW 010115",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "815 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ048>050-010215-",
      "/O.CAN.KTBW.FG.Y.0001.000000T0000Z-100102T0000Z/",
      "HERNANDO-PASCO-PINELLAS-",
      "815 PM EST THU DEC 31 2009",
      "...DENSE FOG ADVISORY IS CANCELLED...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE DENSE FOG ADVISORY.",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "$$",
      "FLZ043-052-057-010915-",
      "/O.UPG.KTBW.HW.A.0001.100102T0900Z-100104T0300Z/",
      "/O.NEW.KTBW.HW.W.0001.100102T0900Z-100104T0300Z/",
      "SUMTER-POLK-HIGHLANDS-",
      "815 PM EST THU DEC 31 2009",
      "...HIGH WIND WARNING IN EFFECT FROM 4 AM SATURDAY TO 10 PM EST SUNDAY...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HIGH WIND WARNING...WHICH IS IN EFFECT FROM 4 AM SATURDAY TO 10 PM EST SUNDAY. THE HIGH WIND WATCH IS NO LONGER IN EFFECT.",
#      "|*|*|* SEGMENT TEXT GOES HERE *|.*|*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A HIGH WIND WARNING MEANS A HAZARDOUS HIGH WIND EVENT IS EXPECTED OR OCCURRING. SUSTAINED WIND SPEEDS OF AT LEAST 40 MPH OR GUSTS OF 58 MPH OR MORE CAN LEAD TO PROPERTY DAMAGE.",
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
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "815 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ043-052-057-010915-",
      "/O.CON.KTBW.HW.W.0001.100102T0900Z-100104T0300Z/",
      "SUMTER-POLK-HIGHLANDS-",
      "815 PM EST THU DEC 31 2009",
      "...HIGH WIND WARNING REMAINS IN EFFECT FROM 4 AM SATURDAY TO 10 PM EST SUNDAY...",
      "A HIGH WIND WARNING REMAINS IN EFFECT FROM 4 AM SATURDAY TO 10 PM EST SUNDAY.",
#      "|*|*|* SEGMENT TEXT GOES HERE *|.*|*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A HIGH WIND WARNING MEANS A HAZARDOUS HIGH WIND EVENT IS EXPECTED OR OCCURRING. SUSTAINED WIND SPEEDS OF AT LEAST 40 MPH OR GUSTS OF 58 MPH OR MORE CAN LEAD TO PROPERTY DAMAGE.",
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
        ("Fcst", "Hazards", "DISCRETE", 0, 1, "FG.Y", ["FLZ039", "FLZ042", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 33, 75, "HW.W", ["FLZ043", "FLZ052"]),
        ("Fcst", "Hazards", "DISCRETE", 33, 75, "<None>", ["FLZ057"]),
        ],
    "checkStrings": [
       "WWUS72 KTBW 010115",
       "NPWTBW",
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "815 PM EST THU DEC 31 2009",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ057-010215-",
       "/O.CAN.KTBW.HW.W.0001.100102T0900Z-100104T0300Z/",
       "HIGHLANDS-",
       "815 PM EST THU DEC 31 2009",
       "...HIGH WIND WARNING IS CANCELLED...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE HIGH WIND WARNING.",
#       "|* SEGMENT TEXT GOES HERE *|.",
       "$$",
       "FLZ043-052-010915-",
       "/O.CON.KTBW.HW.W.0001.100102T0900Z-100104T0300Z/",
       "SUMTER-POLK-",
       "815 PM EST THU DEC 31 2009",
       "...HIGH WIND WARNING REMAINS IN EFFECT FROM 4 AM SATURDAY TO 10 PM EST SUNDAY...",
       "A HIGH WIND WARNING REMAINS IN EFFECT FROM 4 AM SATURDAY TO 10 PM EST SUNDAY.",
#       "|*|*|* SEGMENT TEXT GOES HERE *|.*|*|",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A HIGH WIND WARNING MEANS A HAZARDOUS HIGH WIND EVENT IS EXPECTED OR OCCURRING. SUSTAINED WIND SPEEDS OF AT LEAST 40 MPH OR GUSTS OF 58 MPH OR MORE CAN LEAD TO PROPERTY DAMAGE.",
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
        ("Fcst", "Hazards", "DISCRETE", 0, 1, "FG.Y", ["FLZ039", "FLZ042", "FLZ043"]),
        ("Fcst", "Hazards", "DISCRETE", 33, 75, "<None>", ["FLZ043", "FLZ052"]),
        ],
    "checkStrings": [
      "WWUS72 KTBW 010115",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "815 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ043-052-010215-",
      "/O.CAN.KTBW.HW.W.0001.100102T0900Z-100104T0300Z/",
      "SUMTER-POLK-",
      "INCLUDING THE CITIES OF...WILDWOOD...LAKE PANASOFFKEE...",
      "BUSHNELL...LAKELAND...WINTER HAVEN",
      "815 PM EST THU DEC 31 2009",
      "...HIGH WIND WARNING IS CANCELLED...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE HIGH WIND WARNING.",
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
        "database": "<site>_GRID__Official_00000000_0000",
        "publishGrids": 1,
        "decodeVTEC": 1,
        "gridsStartTime": "20100101_0000",
        "orderStrings": 1,
        "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)













