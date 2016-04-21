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
       "National Weather Service Tampa Bay Ruskin FL",
       "900 PM EST Thu Dec 2 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "GMZ870-031000-",
       "/O.NEW.KTBW.SC.Y.0001.101203T0200Z-101204T1200Z/",
       "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM-",
       "900 PM EST Thu Dec 2 2010",
       "...SMALL CRAFT ADVISORY IN EFFECT UNTIL 7 AM EST SATURDAY...",
       "The National Weather Service in Tampa Bay Ruskin has issued a Small Craft Advisory...which is in effect until 7 AM EST Saturday.",
#        "|* SEGMENT TEXT GOES HERE *|.",
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
    "notCheckStrings": ["...Small Craft Advisory no longer in effect"],
    "checkStrings": [
       "WHUS72 KTBW 030200",
       "MWWTBW",
       "URGENT - MARINE WEATHER MESSAGE",
       "National Weather Service Tampa Bay Ruskin FL",
       "900 PM EST Thu Dec 2 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "GMZ870-031000-",
       "/O.UPG.KTBW.SC.Y.0001.000000T0000Z-101204T1200Z/",
       "/O.NEW.KTBW.GL.W.0001.101203T0200Z-101204T1200Z/",
       "Waters from Tarpon Springs to Suwannee River FL out 20 to 60 NM-",
       "900 PM EST Thu Dec 2 2010",
       "...GALE WARNING IN EFFECT UNTIL 7 AM EST SATURDAY...",
       "The National Weather Service in Tampa Bay Ruskin has issued a Gale Warning...which is in effect until 7 AM EST Saturday. The Small Craft Advisory is no longer in effect.",
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
        "database": "<site>_GRID__Fcst_00000000_0000",
        "publishGrids": 0,
        "decodeVTEC": 1,
        "gridsStartTime": "20100101_0500",
        "orderStrings": 1,
        "vtecMode": "O",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




