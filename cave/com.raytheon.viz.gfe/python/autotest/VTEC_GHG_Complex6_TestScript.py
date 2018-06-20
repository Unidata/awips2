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
    Area1 (FLZ139, FLZ239, FLZ142, FLZ242) 18-27
    """,
    
    "name": "GHG_Complex6_1",
    "productType": "Hazard_RFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 18, 27, "FW.A", ["FLZ139", "FLZ239", "FLZ142", "FLZ242"]),
       ],
    "cmdLineVars" : "{('Select RFW Type', 'rfwType'): [], ('Source for Headline and \\nAffected Area Bullet', 'elevationSource'): 'Grids'}",
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ139-142-239-242-010800-",
      "/O.NEW.KTBW.FW.A.0001.100101T1800Z-100102T0300Z/",
      "Coastal Levy-Coastal Citrus-Inland Levy-Inland Citrus-",
      "700 PM EST Thu Dec 31 2009",
      "...FIRE WEATHER WATCH IN EFFECT FROM FRIDAY AFTERNOON THROUGH FRIDAY EVENING",
      "The National Weather Service in Tampa Bay Ruskin has issued a Fire Weather Watch, which is in effect from Friday afternoon through Friday evening. ",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Fire Weather Watch means that critical fire weather conditions are forecast to occur. Listen for later forecasts and possible Red Flag Warnings.",
      "&&",
      "$$",
                     ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex6_2",
    "productType": "Hazard_RFW_Local",
    "deleteGrids": [],
    "cmdLineVars" : "{('Select RFW Type', 'rfwType'): [], ('Source for Headline and \\nAffected Area Bullet', 'elevationSource'): 'Grids'}",
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ139-142-239-242-010800-",
      "/O.CON.KTBW.FW.A.0001.100101T1800Z-100102T0300Z/",
      "Coastal Levy-Coastal Citrus-Inland Levy-Inland Citrus-",
      "700 PM EST Thu Dec 31 2009",
      "...FIRE WEATHER WATCH REMAINS IN EFFECT FROM FRIDAY AFTERNOON THROUGH FRIDAY EVENING",
#       "A Fire Weather Watch remains in effect from Friday afternoon through Friday evening. ",
#      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Fire Weather Watch means that critical fire weather conditions are forecast to occur. Listen for later forecasts and possible Red Flag Warnings.",
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
        ("Fcst", "Hazards", "DISCRETE", 18, 27, "FW.W", ["FLZ139", "FLZ239", "FLZ142", "FLZ242"]),
        ],
    "cmdLineVars" : "{('Select RFW Type', 'rfwType'): [], ('Source for Headline and \\nAffected Area Bullet', 'elevationSource'): 'Grids'}",
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ139-142-239-242-010800-",
      "/O.UPG.KTBW.FW.A.0001.100101T1800Z-100102T0300Z/",
      "/O.NEW.KTBW.FW.W.0001.100101T1800Z-100102T0300Z/",
      "Coastal Levy-Coastal Citrus-Inland Levy-Inland Citrus-",
      "700 PM EST Thu Dec 31 2009",
      "...RED FLAG WARNING IN EFFECT FROM 1 PM TO 10 PM EST FRIDAY",
      "The National Weather Service in Tampa Bay Ruskin has issued a Red Flag Warning, which is in effect from 1 PM to 10 PM EST Friday. The Fire Weather Watch is no longer in effect.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Red Flag Warning means that critical fire weather conditions are either occurring now, or will shortly.",
      "&&",
      "$$",
                     ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex6_4",
    "productType": "Hazard_RFW_Local",
    "deleteGrids": [],        
    "cmdLineVars" : "{('Select RFW Type', 'rfwType'): [], ('Source for Headline and \\nAffected Area Bullet', 'elevationSource'): 'Grids'}",
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ139-142-239-242-010800-",
      "/O.CON.KTBW.FW.W.0001.100101T1800Z-100102T0300Z/",
      "Coastal Levy-Coastal Citrus-Inland Levy-Inland Citrus-",
      "700 PM EST Thu Dec 31 2009",
      "...RED FLAG WARNING REMAINS IN EFFECT FROM 1 PM TO 10 PM EST FRIDAY",
#       "A Red Flag Warning remains in effect from 1 PM to 10 PM EST Friday.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Red Flag Warning means that critical fire weather conditions are either occurring now, or will shortly.",
      "&&",
      "$$",
                     ],
    },
    {

    "commentary": """
    Creating a new FW.A hazard for Area1 with a new time range
    Area1 (FLZ139, FLZ239, FLZ142, FLZ242) 42-54
    """,
    
    "name": "GHG_Complex6_5",
    "productType": "Hazard_RFW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 18, 27, "FW.W", ["FLZ139", "FLZ239", "FLZ142", "FLZ242"]),
        ("Fcst", "Hazards", "DISCRETE", 42, 54, "FW.A", ["FLZ139", "FLZ239", "FLZ142", "FLZ242"]),],
    "cmdLineVars" : "{('Select RFW Type', 'rfwType'): [], ('Source for Headline and \\nAffected Area Bullet', 'elevationSource'): 'Grids'}",
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ139-142-239-242-010800-",
      "/O.NEW.KTBW.FW.A.0002.100102T1800Z-100103T0600Z/",
      "/O.CON.KTBW.FW.W.0001.100101T1800Z-100102T0300Z/",
      "Coastal Levy-Coastal Citrus-Inland Levy-Inland Citrus-",
      "700 PM EST Thu Dec 31 2009",
      "...RED FLAG WARNING REMAINS IN EFFECT FROM 1 PM TO 10 PM EST FRIDAY",
      "...FIRE WEATHER WATCH IN EFFECT FROM SATURDAY AFTERNOON THROUGH LATE SATURDAY NIGHT",
      "The National Weather Service in Tampa Bay Ruskin has issued a Fire Weather Watch, which is in effect from Saturday afternoon through late Saturday night.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Red Flag Warning means that critical fire weather conditions are either occurring now, or will shortly.",
      "A Fire Weather Watch means that critical fire weather conditions are forecast to occur. Listen for later forecasts and possible Red Flag Warnings.",
      "&&",
      "$$",
                     ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex6_6",
    "productType": "Hazard_RFW_Local",
    "deleteGrids": [],        
    "cmdLineVars" : "{('Select RFW Type', 'rfwType'): [], ('Source for Headline and \\nAffected Area Bullet', 'elevationSource'): 'Grids'}",
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ139-142-239-242-010800-",
      "/O.CON.KTBW.FW.W.0001.100101T1800Z-100102T0300Z/",
      "/O.CON.KTBW.FW.A.0002.100102T1800Z-100103T0600Z/",
      "Coastal Levy-Coastal Citrus-Inland Levy-Inland Citrus-",
      "700 PM EST Thu Dec 31 2009",
      "...RED FLAG WARNING REMAINS IN EFFECT FROM 1 PM TO 10 PM EST FRIDAY",
      "...FIRE WEATHER WATCH REMAINS IN EFFECT FROM SATURDAY AFTERNOON THROUGH LATE SATURDAY NIGHT",
#       "A Red Flag Warning remains in effect from 1 PM to 10 PM EST Friday. A Fire Weather Watch remains in effect from Saturday afternoon through late Saturday night.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Red Flag Warning means that critical fire weather conditions are either occurring now, or will shortly.",
      "A Fire Weather Watch means that critical fire weather conditions are forecast to occur. Listen for later forecasts and possible Red Flag Warnings.",
      "&&",
      "$$",
                     ],
    }, 
    {

    "commentary": """
    Changing the FW.W hazard in Area1 to no hazards
    Area1 (FLZ139, FLZ239, FLZ142, FLZ242) 18-27
    """,
    
    "name": "GHG_Complex6_7",
    "productType": "Hazard_RFW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 18, 27, "<None>", ["FLZ139", "FLZ239", "FLZ142", "FLZ242"]),
        ("Fcst", "Hazards", "DISCRETE", 42, 54, "FW.A", ["FLZ139", "FLZ239", "FLZ142", "FLZ242"]),],
    "cmdLineVars" : "{('Select RFW Type', 'rfwType'): [], ('Source for Headline and \\nAffected Area Bullet', 'elevationSource'): 'Grids'}",
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ139-142-239-242-010800-",
      "/O.CAN.KTBW.FW.W.0001.100101T1800Z-100102T0300Z/",
      "/O.CON.KTBW.FW.A.0002.100102T1800Z-100103T0600Z/",
      "Coastal Levy-Coastal Citrus-Inland Levy-Inland Citrus-",
      "700 PM EST Thu Dec 31 2009",
      "...FIRE WEATHER WATCH REMAINS IN EFFECT FROM SATURDAY AFTERNOON THROUGH LATE SATURDAY NIGHT",
      "...RED FLAG WARNING IS CANCELLED",
      "The National Weather Service in Tampa Bay Ruskin has cancelled the Red Flag Warning.",
#      "|*|*|* SEGMENT TEXT GOES HERE *|.*|*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Fire Weather Watch means that critical fire weather conditions are forecast to occur. Listen for later forecasts and possible Red Flag Warnings.",
      "&&",
      "$$",

                     ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex6_8",
    "productType": "Hazard_RFW_Local",
    "deleteGrids": [],
    "decodeVTEC": 0, 
    "cmdLineVars" : "{('Select RFW Type', 'rfwType'): [], ('Source for Headline and \\nAffected Area Bullet', 'elevationSource'): 'Grids'}",
    "checkStrings": [
      "WWUS82 KTBW 010000",
      "RFWTBW",
      "URGENT - FIRE WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ139-142-239-242-010800-",
      "/O.CON.KTBW.FW.A.0002.100102T1800Z-100103T0600Z/",
      "Coastal Levy-Coastal Citrus-Inland Levy-Inland Citrus-",
      "700 PM EST Thu Dec 31 2009",
      "...FIRE WEATHER WATCH REMAINS IN EFFECT FROM SATURDAY AFTERNOON THROUGH LATE SATURDAY NIGHT",
#       "A Fire Weather Watch remains in effect from Saturday afternoon through late Saturday night.",
#      "|*|*|* SEGMENT TEXT GOES HERE *|.*|*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Fire Weather Watch means that critical fire weather conditions are forecast to occur. Listen for later forecasts and possible Red Flag Warnings.",
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
        "database": "<site>_GRID__Fcst_00000000_0000",
        "publishGrids": 0,
        "decodeVTEC": 1,
        "gridsStartTime": "20100101_0000",
        "orderStrings": 1,
        "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)







