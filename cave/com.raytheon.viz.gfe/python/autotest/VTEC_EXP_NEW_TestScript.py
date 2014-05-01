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
    "name": "EXPNEW_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

    {
    "commentary": "Setting initial event",
    "name": "EXPNEW_1",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 8, 24+8, "WS.W", ["FLZ050"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "1210 AM EST FRI JAN 1 2010",
                     "FLZ050-",
                     "/O.NEW.KTBW.WS.W.0001.100101T1300Z-100102T1300Z/",
                     "PINELLAS-",
                     "1210 AM EST FRI JAN 1 2010",
                     "...WINTER STORM WARNING IN EFFECT FROM 8 AM THIS MORNING TO 8 AM EST SATURDAY...",
#                     "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A WINTER STORM WARNING...WHICH IS IN EFFECT FROM 8 AM THIS MORNING TO 8 AM EST SATURDAY.",
                     ],
    },

    {
    "commentary": "Extending right before EXP ",
    "name": "EXPNEW_2",
    "drtTime": "20100102_1250",
    "decodeVTEC": 0,   #don't decode
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 8, 32, "WS.W", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 32, 40, "WS.W", ["FLZ050"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "750 AM EST SAT JAN 2 2010",
                     "FLZ050-",
                     "/O.EXT.KTBW.WS.W.0001.000000T0000Z-100102T2100Z/",
                     "PINELLAS-",
                     "...WINTER STORM WARNING NOW IN EFFECT UNTIL 4 PM EST THIS AFTERNOON...",
                     "THE WINTER STORM WARNING IS NOW IN EFFECT UNTIL 4 PM EST THIS AFTERNOON.",
                     ],
    },

    {
    "commentary": "Extending after EXP ",
    "name": "EXPNEW_3",
    "drtTime": "20100102_1301",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 8, 32, "WS.W", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 32, 40, "WS.W", ["FLZ050"]),
       ],
    "checkStrings": ["URGENT - WINTER WEATHER MESSAGE",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "801 AM EST SAT JAN 2 2010",
                     "FLZ050-",
                     "/O.EXP.KTBW.WS.W.0001.000000T0000Z-100102T1300Z/",
                     "/O.NEW.KTBW.WS.W.0002.100102T1301Z-100102T2100Z/",
                     "PINELLAS-",
                     "...WINTER STORM WARNING IN EFFECT UNTIL 4 PM EST THIS AFTERNOON...",
                     "...WINTER STORM WARNING HAS EXPIRED...",
#                     "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A WINTER STORM WARNING...WHICH IS IN EFFECT UNTIL 4 PM EST THIS AFTERNOON. THE WINTER STORM WARNING IS NO LONGER IN EFFECT.",
                     ],
    },


    {
    "commentary": "Cleanup.",
    "name": "EXPNEW_4",
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
        "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




