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
# Headline UPG phrasing for tropical events
#
# Author:
# ----------------------------------------------------------------------------

scripts = [
    {
    "commentary": "Clear out all Hazards Table and Grids.",
    "name": "HeadlineUPG_Init",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },


#scenario SC.Y ---> GL.W (no UPG headline phrase present)
    {
    "commentary": "Initial setup of SC.Y for SC.Y--->GL.W (no UPG headline phrase present)",
    "name": "HeadlineUPG_1a",
    "drtTime": "20101203_0200",
    "gridsStartTime": "20101203_0000",
    "productType": "Hazard_MWW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 36, "SC.Y", ["GMZ870"]),
       ],
    "checkStrings": [
       "WHUS72 KTBW 030200",
       "MWWTBW",
       "URGENT - MARINE WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "900 PM EST THU DEC 2 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "GMZ870-031000-",
       "/O.NEW.KTBW.SC.Y.0001.101203T0200Z-101204T1200Z/",
       "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM-",
       "900 PM EST THU DEC 2 2010",
       "...SMALL CRAFT ADVISORY IN EFFECT UNTIL 7 AM EST SATURDAY...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A SMALL CRAFT ADVISORY...WHICH IS IN EFFECT UNTIL 7 AM EST SATURDAY.",
       "|* SEGMENT TEXT GOES HERE *|.",
       "$$",
       ],
    },

    {
    "commentary": "SC.Y ---> GL.W for SC.Y--->GL.W (no UPG headline phrase present)",
    "name": "HeadlineUPG_1b",
    "drtTime": "20101203_0200",
    "gridsStartTime": "20101203_0000",
    "productType": "Hazard_MWW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 36, "GL.W", ["GMZ870"]),
       ],
    "notCheckStrings": ["...SMALL CRAFT ADVISORY NO LONGER IN EFFECT"],
    "checkStrings": [
       "WHUS72 KTBW 030200",
       "MWWTBW",
       "URGENT - MARINE WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "900 PM EST THU DEC 2 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "GMZ870-031000-",
       "/O.UPG.KTBW.SC.Y.0001.000000T0000Z-101204T1200Z/",
       "/O.NEW.KTBW.GL.W.0001.101203T0200Z-101204T1200Z/",
       "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM-",
       "900 PM EST THU DEC 2 2010",
       "...GALE WARNING IN EFFECT UNTIL 7 AM EST SATURDAY...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A GALE WARNING...WHICH IS IN EFFECT UNTIL 7 AM EST SATURDAY. THE SMALL CRAFT ADVISORY IS NO LONGER IN EFFECT.",
       "$$",
       ],
    },

    {
    "commentary": "Deleting hazard grids.",
    "name": "HeadlineUPG_1c",
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
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




