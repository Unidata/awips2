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
# Tests overview options on hazard products
#
# Author:
# ----------------------------------------------------------------------------

def1a = """#Definition["includeOverviewHeadline"] = 1"""
def1b = """TextProduct.Definition["includeOverviewHeadline"] = 0"""
def2a = """#Definition["includeOverview"] = 1"""
def2b = """TextProduct.Definition["includeOverview"] = 0"""


scripts = [
    {
    "commentary": "Clear out all Hazards Table and Grids.",
    "name": "HazOverview_WSW_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

    {
    "commentary": "WSW check: with overview headline and overview",
    "name": "HazOverview_WSW_1",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "WS.A", ["FLZ039"]),
       ],
    "checkStrings": [
       "URGENT - WINTER WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [],
    },

    {
    "commentary": "WSW check: with just overview",
    "name": "HazOverview_WSW_2",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "WS.A", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_WSW_Local", "TextProduct", "replace", def1b, "undo")],

    "checkStrings": [
       "URGENT - WINTER WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
        ],
    },
    {
    "commentary": "WSW check: with just overview headline",
    "name": "HazOverview_WSW_3",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "WS.A", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_WSW_Local", "TextProduct", "replace", def2b, "undo")],
    "checkStrings": [
       "URGENT - WINTER WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       ".|*OVERVIEW (MUST EDIT)*|.",
        ],
    },
    {
    "commentary": "WSW check: with neither overview nor overview headline",
    "name": "HazOverview_WSW_4",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "WS.A", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_WSW_Local", "TextProduct", "replace", def2b, "undo"), ("Hazard_WSW_Local", "TextProduct", "replace", def1b, "undo")],
    "checkStrings": [
       "URGENT - WINTER WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
                     ],
    },
    {
    "commentary": "Canceling out all hazards.",
    "name": "HazOverview_WSW_5",
    "productType": None,
    "checkStrings": [],
    "clearHazardsTable": 1,
    },
    
# NPW
    {
    "commentary": "Clear out all Hazards Table and Grids in prep for NPW.",
    "name": "HazOverview_NPW_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

    {
    "commentary": "NPW check: with both overview headline and overview",
    "name": "HazOverview_NPW_1",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "FR.Y", ["FLZ039"]),
       ],
    "checkStrings": [
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [],
    },

    {
    "commentary": "NPW check: with just overview",
    "name": "HazOverview_NPW_2",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "FR.Y", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_NPW_Local", "TextProduct", "replace", def1b, "undo")],

    "checkStrings": [
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
        ],
    },
    {
    "commentary": "NPW check: with just overview headline",
    "name": "HazOverview_NPW_3",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "FR.Y", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_NPW_Local", "TextProduct", "replace", def2b, "undo")],
    "checkStrings": [
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       ".|*OVERVIEW (MUST EDIT)*|.",
        ],
    },
    {
    "commentary": "NPW check: with neither overview headline nor overview",
    "name": "HazOverview_NPW_4",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "FR.Y", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_NPW_Local", "TextProduct", "replace", def2b, "undo"), ("Hazard_NPW_Local", "TextProduct", "replace", def1b, "undo")],
    "checkStrings": [
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
                     ],
    },
    {
    "commentary": "Canceling out all hazards.",
    "name": "HazOverview_NPW_5",
    "productType": None,
    "checkStrings": [],
    "clearHazardsTable": 1,
    },
    
# RFW 
    {
    "commentary": "Clear out all Hazards Table and Grids for RFW.",
    "name": "HazOverview_RFW_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

    {
    "commentary": "RFW check: with both overview headline and overview",
    "name": "HazOverview_RFW_1",
    "productType": "Hazard_RFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "FW.A", ["FLZ039"]),
       ],
    "checkStrings": [
       "URGENT - FIRE WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [],
    },

    {
    "commentary": "RFW check: with only overview",
    "name": "HazOverview_RFW_2",
    "productType": "Hazard_RFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "FW.A", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_RFW_Local", "TextProduct", "replace", def1b, "undo")],

    "checkStrings": [
       "URGENT - FIRE WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
        ],
    },
    {
    "commentary": "RFW check: with only overview headline",
    "name": "HazOverview_RFW_3",
    "productType": "Hazard_RFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "FW.A", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_RFW_Local", "TextProduct", "replace", def2b, "undo")],
    "checkStrings": [
       "URGENT - FIRE WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       ".|*OVERVIEW (MUST EDIT)*|.",
        ],
    },
    {
    "commentary": "RFW check: with neither overview headline nor overview",
    "name": "HazOverview_RFW_4",
    "productType": "Hazard_RFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "FW.A", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_RFW_Local", "TextProduct", "replace", def2b, "undo"), ("Hazard_RFW_Local", "TextProduct", "replace", def1b, "undo")],
    "checkStrings": [
       "URGENT - FIRE WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
                     ],
    },
    {
    "commentary": "Canceling out all hazards.",
    "name": "HazOverview_RFW_5",
    "productType": None,
    "checkStrings": [],
    "clearHazardsTable": 1,
    },
    
    
# CFW
    {
    "commentary": "Clear out all Hazards Table and Grids for CFW.",
    "name": "HazOverview_CFW_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

    {
    "commentary": "CFW check: with both overview headline and overview",
    "name": "HazOverview_CFW_1",
    "productType": "Hazard_CFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "LS.A", ["FLZ039"]),
       ],
    "checkStrings": [
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "LAKESHORE HAZARD MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [],
    },

    {
    "commentary": "CFW check: with only overview",
    "name": "HazOverview_CFW_2",
    "productType": "Hazard_CFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "LS.A", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_CFW_Local", "TextProduct", "replace", def1b, "undo")],

    "checkStrings": [
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "LAKESHORE HAZARD MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
        ],
    },
    {
    "commentary": "CFW check: with only overview headline",
    "name": "HazOverview_CFW_3",
    "productType": "Hazard_CFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "LS.A", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_CFW_Local", "TextProduct", "replace", def2b, "undo")],
    "checkStrings": [
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "LAKESHORE HAZARD MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       ".|*OVERVIEW (MUST EDIT)*|.",
        ],
    },
    {
    "commentary": "CFW check: with neither overview headline nor overview",
    "name": "HazOverview_CFW_4",
    "productType": "Hazard_CFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "LS.A", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_CFW_Local", "TextProduct", "replace", def2b, "undo"), ("Hazard_CFW_Local", "TextProduct", "replace", def1b, "undo")],
    "checkStrings": [
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "LAKESHORE HAZARD MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
                     ],
    },
    {
    "commentary": "Canceling out all hazards.",
    "name": "HazOverview_CFW_5",
    "productType": None,
    "checkStrings": [],
    "clearHazardsTable": 1,
    },
    
# FFA - overview, but implemented in FFA not Generic Hazards
    {
    "commentary": "Clear out all Hazards Table and Grids for FFA.",
    "name": "HazOverview_FFA_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

    {
    "commentary": "FFA check: with both overview headline and overview",
    "name": "HazOverview_FFA_1",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "FA.A", ["FLZ039"]),
       ],
    "checkStrings": [
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "FLOOD WATCH",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [],
    },

    {
    "commentary": "FFA check: with only overview",
    "name": "HazOverview_FFA_2",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "FA.A", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_FFA_Local", "TextProduct", "replace", def1b, "undo")],

    "checkStrings": [
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "FLOOD WATCH",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
        ],
    },
    {
    "commentary": "FFA check: with only overview headline",
    "name": "HazOverview_FFA_3",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "FA.A", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_FFA_Local", "TextProduct", "replace", def2b, "undo")],
    "checkStrings": [
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "FLOOD WATCH",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       ".|*OVERVIEW (MUST EDIT)*|.",
        ],
    },
    {
    "commentary": "FFA check: with neither overview headline nor overview",
    "name": "HazOverview_FFA_4",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "FA.A", ["FLZ039"]),
       ],
    "fileChanges": [("Hazard_FFA_Local", "TextProduct", "replace", def2b, "undo"), ("Hazard_FFA_Local", "TextProduct", "replace", def1b, "undo")],
    "checkStrings": [
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "FLOOD WATCH",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "FLZ039-010800-",
                     ],
    "notCheckStrings": [
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
                     ],
    },
    {
    "commentary": "Canceling out all hazards.",
    "name": "HazOverview_FFA_5",
    "productType": None,
    "checkStrings": [],
    "clearHazardsTable": 1,
    },
    
# MWS - no overview section
    {
    "commentary": "Clear out all Hazards Table and Grids for MWS.",
    "name": "HazOverview_MWS_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

    {
    "commentary": "MWS check: no overview headline nor overview - default",
    "name": "HazOverview_MWS_1",
    "productType": "Hazard_MWS_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "MA.S", ["GMZ853"]),
       ],
    "checkStrings": [
       "MARINE WEATHER STATEMENT",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "GMZ853-010800-",
                     ],
    "notCheckStrings": [
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
                     ],
    },

    {
    "commentary": "Canceling out all hazards.",
    "name": "HazOverview_MWS_2",
    "productType": None,
    "checkStrings": [],
    "clearHazardsTable": 1,
    },
    
# WCN - no overview section
    {
    "commentary": "Clear out all Hazards Table and Grids for WCN.",
    "name": "HazOverview_WCN_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

    {
    "commentary": "WCN check: no overview headline nor overview - default",
    "name": "HazOverview_WCN_1",
    "productType": "Hazard_WCN_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "TO.A:0123", ["FLC057"]),
       ],
    "checkStrings": [
       "WCNTBW",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "FLC057-011500-",
                     ],
    "notCheckStrings": [
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
                     ],
    },

    {
    "commentary": "Canceling out all hazards.",
    "name": "HazOverview_WCN_2",
    "productType": None,
    "checkStrings": [],
    "clearHazardsTable": 1,
    },
    
# MWW
    {
    "commentary": "Clear out all Hazards Table and Grids in prep for MWW.",
    "name": "HazOverview_MWW_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

    {
    "commentary": "MWW check: with both overview headline and overview",
    "name": "HazOverview_MWW_1",
    "productType": "Hazard_MWW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "SC.Y", ["GMZ830"]),
       ],
    "checkStrings": [
       "URGENT - MARINE WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "GMZ830-010800-",
       ],
    "notCheckStrings": [],
    },

    {
    "commentary": "MWW check: with just overview",
    "name": "HazOverview_MWW_2",
    "productType": "Hazard_MWW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "SC.Y", ["GMZ830"]),
       ],
    "fileChanges": [("Hazard_MWW_Local", "TextProduct", "replace", def1b, "undo")],

    "checkStrings": [
       "URGENT - MARINE WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "GMZ830-010800-",
                     ],
    "notCheckStrings": [
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
        ],
    },
    {
    "commentary": "MWW check: with just overview headline",
    "name": "HazOverview_MWW_3",
    "productType": "Hazard_MWW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "SC.Y", ["GMZ830"]),
       ],
    "fileChanges": [("Hazard_MWW_Local", "TextProduct", "replace", def2b, "undo")],
    "checkStrings": [
       "URGENT - MARINE WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       "GMZ830-010800-",
                     ],
    "notCheckStrings": [
       ".|*OVERVIEW (MUST EDIT)*|.",
        ],
    },
    {
    "commentary": "MWW check: with neither overview headline nor overview",
    "name": "HazOverview_MWW_4",
    "productType": "Hazard_MWW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "SC.Y", ["GMZ830"]),
       ],
    "fileChanges": [("Hazard_MWW_Local", "TextProduct", "replace", def2b, "undo"), ("Hazard_MWW_Local", "TextProduct", "replace", def1b, "undo")],
    "checkStrings": [
       "URGENT - MARINE WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "GMZ830-010800-",
                     ],
    "notCheckStrings": [
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
                     ],
    },
    {
    "commentary": "Canceling out all hazards.",
    "name": "HazOverview_MWW_5",
    "productType": None,
    "checkStrings": [],
    "clearHazardsTable": 1,
    },
    ]

       
import TestScript
def testScript(self, dataMgr):
    defaults = {
        "database": "<site>_GRID__Official_00000000_0000",
        "publishGrids": 1,
        "decodeVTEC": 0,
        "gridsStartTime": "20100101_0000",
        "orderStrings": 1,
        "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




