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
# More complex Hazards Tests 
#
# Author:
# ----------------------------------------------------------------------------


scripts = [
    {
    "name": "Hazards0",
    "productType": None,
    "commentary": "Deleting hazard grids.",
    "deleteGrids": [
       ("Fcst", "Hazards", "SFC", "all", "all"),
       ],
    "clearHazardsTable": 1,
    "checkStrings": [],
    },
    {
    "name": "Hazards1",
    "commentary": "Setting up CF.Y for three zones to generate a NEW vtec.",
    "productType": "Hazard_CFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", 22, 28, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 22, 28, "CF.Y", ["FLZ148", "FLZ149", "FLZ050"]),
       ],
    "checkStrings": [
       "Coastal Hazard Message",
       "National Weather Service Tampa Bay Ruskin FL",
       "700 PM EST Thu Dec 31 2009",
       "FLZ050-148-149-010800-",
       "/O.NEW.KTBW.CF.Y.0001.100101T2200Z-100102T0400Z/",
       "Pinellas-Coastal Hernando-Coastal Pasco-",
       "700 PM EST Thu Dec 31 2009",
       "...COASTAL FLOOD ADVISORY IN EFFECT FROM 5 PM TO 11 PM EST FRIDAY...",
       "The National Weather Service in Tampa Bay Ruskin has issued a Coastal Flood Advisory, which is in effect from 5 PM to 11 PM EST Friday.",
#       "|* SEGMENT TEXT GOES HERE *|.",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A Coastal Flood Advisory indicates that onshore winds and tides will combine to generate flooding of low areas along the shore.",
       "&&",
       "$$",
                  ],
    },
    {
    "name": "Hazards2",
    "productType": "Hazard_CFW_Local",
    "commentary": "No grid changes, CF.Y in three zones to generate a CON vtec.",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", 22, 28, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 22, 28, "CF.Y", ["FLZ148", "FLZ149", "FLZ050"]),
       ],
    "checkStrings": [
       "Coastal Hazard Message",
       "National Weather Service Tampa Bay Ruskin FL",
       "700 PM EST Thu Dec 31 2009",
       "FLZ050-148-149-010800-",
       "/O.CON.KTBW.CF.Y.0001.100101T2200Z-100102T0400Z/",
       "Pinellas-Coastal Hernando-Coastal Pasco-",
       "700 PM EST Thu Dec 31 2009",
       "...COASTAL FLOOD ADVISORY REMAINS IN EFFECT FROM 5 PM TO 11 PM EST FRIDAY...",
#        "A Coastal Flood Advisory remains in effect from 5 PM to 11 PM EST Friday.",
#       "|* SEGMENT TEXT GOES HERE *|.",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A Coastal Flood Advisory indicates that onshore winds and tides will combine to generate flooding of low areas along the shore.",
       "&&",
       "$$",
                  ],
    },
    {
    "name": "Hazards3",
    "productType": "Hazard_CFW_Local",
    "commentary": "Extending ending time of the CF.Y in three zones to generate a EXT vtec.",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", 22, 34, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 22, 34, "CF.Y", ["FLZ148", "FLZ149", "FLZ050"]),
        ],
    "checkStrings": [
        "Coastal Hazard Message",
        "National Weather Service Tampa Bay Ruskin FL",
        "700 PM EST Thu Dec 31 2009",
        "FLZ050-148-149-010800-",
        "/O.EXT.KTBW.CF.Y.0001.100101T2200Z-100102T1000Z/",
        "Pinellas-Coastal Hernando-Coastal Pasco-",
        "700 PM EST Thu Dec 31 2009",
        "...COASTAL FLOOD ADVISORY NOW IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY...",
#         "The Coastal Flood Advisory is now in effect from 5 PM Friday to 5 AM EST Saturday.",
#        "|* SEGMENT TEXT GOES HERE *|.",
        "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
        "A Coastal Flood Advisory indicates that onshore winds and tides will combine to generate flooding of low areas along the shore.",
        "&&",
        "$$",

                  ],
    },
    {
    "name": "Hazards4",
    "productType": "Hazard_CFW_Local",
    "commentary": "Moving starting time earlier for the CF.Y in three zones to generate a EXT vtec.",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", 22, 34, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 16, 28, "CF.Y", ["FLZ148", "FLZ149", "FLZ050"]),
        ],
    "checkStrings": [
       "Coastal Hazard Message",
       "National Weather Service Tampa Bay Ruskin FL",
       "700 PM EST Thu Dec 31 2009",
       "FLZ050-148-149-010800-",
       "/O.EXT.KTBW.CF.Y.0001.100101T1600Z-100102T0400Z/",
       "Pinellas-Coastal Hernando-Coastal Pasco-",
       "700 PM EST Thu Dec 31 2009",
       "...COASTAL FLOOD ADVISORY NOW IN EFFECT FROM 11 AM TO 11 PM EST FRIDAY...",
#        "The Coastal Flood Advisory is now in effect from 11 AM to 11 PM EST Friday.",
#       "|* SEGMENT TEXT GOES HERE *|.",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A Coastal Flood Advisory indicates that onshore winds and tides will combine to generate flooding of low areas along the shore.",
       "&&",
       "$$",
                  ],
    },
    {
    "name": "Hazards5",
    "productType": "Hazard_CFW_Local",
    "commentary": "Adding the CF.Y to one more zone to generate a EXA with CON vtec in two segments.",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", 16, 28, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 16, 28, "CF.Y", ["FLZ148", "FLZ149", "FLZ050", "FLZ165"]),
    ],
    "checkStrings": [
        "Coastal Hazard Message",
        "National Weather Service Tampa Bay Ruskin FL",
        "700 PM EST Thu Dec 31 2009",
        "FLZ165-010800-",
        "/O.EXA.KTBW.CF.Y.0001.100101T1600Z-100102T0400Z/",
        "Coastal Lee-",
        "700 PM EST Thu Dec 31 2009",
        "...COASTAL FLOOD ADVISORY IN EFFECT FROM 11 AM TO 11 PM EST FRIDAY...",
        "The National Weather Service in Tampa Bay Ruskin has issued a Coastal Flood Advisory, which is in effect from 11 AM to 11 PM EST Friday.",
#        "|* SEGMENT TEXT GOES HERE *|.",
        "A Coastal Flood Advisory indicates that onshore winds and tides will combine to generate flooding of low areas along the shore.",
        "$$",
        "FLZ050-148-149-010800-",
        "/O.CON.KTBW.CF.Y.0001.100101T1600Z-100102T0400Z/",
        "Pinellas-Coastal Hernando-Coastal Pasco-",
        "700 PM EST Thu Dec 31 2009",
        "...COASTAL FLOOD ADVISORY REMAINS IN EFFECT FROM 11 AM TO 11 PM EST FRIDAY...",
#         "A Coastal Flood Advisory remains in effect from 11 AM to 11 PM EST Friday.",
#        "|* SEGMENT TEXT GOES HERE *|.",
        "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
        "A Coastal Flood Advisory indicates that onshore winds and tides will combine to generate flooding of low areas along the shore.",
        "&&",
        "$$",
                  ],
    },
    {
    "name": "Hazards6",
    "productType": "Hazard_CFW_Local",
    "commentary": "Changing the ending time of the CF.Y and adding another zone to generate a EXB with EXT vtec in two segments.",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", 16, 28, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 22, 34, "CF.Y", ["FLZ148", "FLZ149", "FLZ050", "FLZ165", "FLZ139"]),
        ],
    "checkStrings": [
       "Coastal Hazard Message",
       "National Weather Service Tampa Bay Ruskin FL",
       "700 PM EST Thu Dec 31 2009",
       "FLZ139-010800-",
       "/O.EXB.KTBW.CF.Y.0001.100101T2200Z-100102T1000Z/",
       "Levy-",
       "700 PM EST Thu Dec 31 2009",
       "...COASTAL FLOOD ADVISORY IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY...",
       "The National Weather Service in Tampa Bay Ruskin has issued a Coastal Flood Advisory, which is in effect from 5 PM Friday to 5 AM EST Saturday.",
#       "|* SEGMENT TEXT GOES HERE *|.",
       "A Coastal Flood Advisory indicates that onshore winds and tides will combine to generate flooding of low areas along the shore.",
       "$$",
       "FLZ050-148-149-165-010800-",
       "/O.EXT.KTBW.CF.Y.0001.100101T2200Z-100102T1000Z/",
       "Pinellas-Coastal Hernando-Coastal Pasco-Coastal Lee-",
       "700 PM EST Thu Dec 31 2009",
       "...COASTAL FLOOD ADVISORY NOW IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY...",
#        "The Coastal Flood Advisory is now in effect from 5 PM Friday to 5 AM EST Saturday.",
#       "|* SEGMENT TEXT GOES HERE *|.",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A Coastal Flood Advisory indicates that onshore winds and tides will combine to generate flooding of low areas along the shore.",
       "&&",
       "$$",
                  ],
    }, 
    {
    "name": "Hazards7",
    "productType": "Hazard_CFW_Local",
    "commentary": "Removing the CF.Y from two zones, but leaving it in the other three zones to generate a CAN and CON vtec in two segments.",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", 22, 34, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 22, 34, "CF.Y", ["FLZ148", "FLZ149", "FLZ050"]),
        ],
    "checkStrings": [
       "Coastal Hazard Message",
       "National Weather Service Tampa Bay Ruskin FL",
       "700 PM EST Thu Dec 31 2009",
       "FLZ139-165-010100-",
       "/O.CAN.KTBW.CF.Y.0001.100101T2200Z-100102T1000Z/",
       "Coastal Levy-Coastal Lee-",
       "700 PM EST Thu Dec 31 2009",
       "...COASTAL FLOOD ADVISORY IS CANCELLED...",
       "The National Weather Service in Tampa Bay Ruskin has cancelled the Coastal Flood Advisory.",
       "$$",
       "FLZ050-148-149-010800-",
       "/O.CON.KTBW.CF.Y.0001.100101T2200Z-100102T1000Z/",
       "Pinellas-Coastal Hernando-Coastal Pasco-",
       "700 PM EST Thu Dec 31 2009",
       "...COASTAL FLOOD ADVISORY REMAINS IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY...",
#        "A Coastal Flood Advisory remains in effect from 5 PM Friday to 5 AM EST Saturday.",
#       "|* SEGMENT TEXT GOES HERE *|.",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A Coastal Flood Advisory indicates that onshore winds and tides will combine to generate flooding of low areas along the shore.",
       "&&",
       "$$",
                  ],
    },
    {
    "name": "Hazards8",
    "productType": "Hazard_CFW_Local",
    "commentary": "Upgrading the CF.Y to a CF.W to geenerate a UPG/NEW vtec.",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", 22, 34, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 22, 34, "CF.W", ["FLZ148", "FLZ149", "FLZ050"]),
        ],
    "checkStrings": [
        "URGENT - IMMEDIATE BROADCAST REQUESTED",
        "Coastal Hazard Message",
        "National Weather Service Tampa Bay Ruskin FL",
        "700 PM EST Thu Dec 31 2009",
        "FLZ050-148-149-010800-",
        "/O.UPG.KTBW.CF.Y.0001.100101T2200Z-100102T1000Z/",
        "/O.NEW.KTBW.CF.W.0001.100101T2200Z-100102T1000Z/",
        "Pinellas-Coastal Hernando-Coastal Pasco-",
        "700 PM EST Thu Dec 31 2009",
        "...COASTAL FLOOD WARNING IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY...",
        "The National Weather Service in Tampa Bay Ruskin has issued a Coastal Flood Warning, which is in effect from 5 PM Friday to 5 AM EST Saturday. The Coastal Flood Advisory is no longer in effect.",
        "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
        "A Coastal Flood Warning means that flooding is occurring or imminent. Coastal residents in the warned area should be alert for rising water, and take appropriate action to protect life and property.",
        "&&",
        "$$",
                  ],
    },
    {
    "name": "Hazards9",
    "productType": "Hazard_CFW_Local",
    "commentary": "Adding a new CF.Y event in one zone, while leaving the existing CF.W in the other zones, to generate two segments with a NEW/CON CF.Y/CF.W and CON CF.W.",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", 35, 41, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 35, 41, "CF.Y", ["FLZ149"]),
        ],
    "checkStrings": [
        "Coastal Hazard Message",
        "National Weather Service Tampa Bay Ruskin FL",
        "700 PM EST Thu Dec 31 2009",
        "FLZ149-010800-",
        "/O.NEW.KTBW.CF.Y.0002.100102T1100Z-100102T1700Z/",
        "/O.CON.KTBW.CF.W.0001.100101T2200Z-100102T1000Z/",
        "Pasco-",
        "700 PM EST Thu Dec 31 2009",
        "...COASTAL FLOOD WARNING REMAINS IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY...",
        "...COASTAL FLOOD ADVISORY IN EFFECT FROM 6 AM TO NOON EST SATURDAY...",
#        "The National Weather Service in Tampa Bay Ruskin has issued a Coastal Flood Advisory, which is in effect from 6 AM to noon EST Saturday. A Coastal Flood Warning remains in effect from 5 PM Friday to 5 AM EST Saturday.",
        "The National Weather Service in Tampa Bay Ruskin has issued a Coastal Flood Advisory, which is in effect from 6 AM to noon EST Saturday.",
        "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
        "A Coastal Flood Warning means that flooding is occurring or imminent. Coastal residents in the warned area should be alert for rising water, and take appropriate action to protect life and property.",
        "A Coastal Flood Advisory indicates that onshore winds and tides will combine to generate flooding of low areas along the shore.",
        "&&",
        "$$",
        "FLZ050-148-010800-",
        "/O.CON.KTBW.CF.W.0001.100101T2200Z-100102T1000Z/",
        "Pinellas-Coastal Hernando-",
        "700 PM EST Thu Dec 31 2009",
        "...COASTAL FLOOD WARNING REMAINS IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY...",
#         "A Coastal Flood Warning remains in effect from 5 PM Friday to 5 AM EST Saturday.",
        "A Coastal Flood Warning means that flooding is occurring or imminent. Coastal residents in the warned area should be alert for rising water, and take appropriate action to protect life and property.",
        "$$",
                  ],
    },
    {
    "name": "Hazards10",
    "productType": "Hazard_CFW_Local",
    "commentary": "Removing all hazards, to generate CAN statements for CF.W and CF.Y events, in separate segments.",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", 16, 41, "<None>", "all"),
       ],
    "checkStrings": ["Coastal Hazard Message",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "700 PM EST Thu Dec 31 2009",
                     "FLZ149-",
                     "/O.CAN.KTBW.CF.W.0001.100101T2200Z-100102T1000Z/",
                     "/O.CAN.KTBW.CF.Y.0002.100102T1100Z-100102T1700Z/",
                     "Pasco-",
                     "700 PM EST Thu Dec 31 2009",
                     "...COASTAL FLOOD WARNING IS CANCELLED...",
                     "...COASTAL FLOOD ADVISORY IS CANCELLED...",
                     "The National Weather Service in Tampa Bay Ruskin has cancelled the Coastal Flood Warning. The Coastal Flood Advisory has been cancelled.",
                     "FLZ050-148",
                     "/O.CAN.KTBW.CF.W.0001.100101T2200Z-100102T1000Z/",
                     "Pinellas-Coastal Hernando-",
                     "700 PM EST Thu Dec 31 2009",
                     "...COASTAL FLOOD WARNING IS CANCELLED...",
                     "The National Weather Service in Tampa Bay Ruskin has cancelled the Coastal Flood Warning.",
         ],
    },
    {
    "name": "Hazards11",
    "productType": "Hazard_CFW_Local",
    "commentary": "Generating a LS.S in one zone, to generate NEW vtec.",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", 20, 28, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 20, 28, "LS.S", ["FLZ139"]),
        ],
    "checkStrings": ["Lakeshore Hazard Message",
                  "National Weather Service Tampa Bay Ruskin FL",
                  "/O.NEW.KTBW.LS.S.0001.100101T2000Z-100102T0400Z/",
                  "Levy-",
#                   "|* STATEMENT TEXT GOES HERE *|.",
                  "$$",
                  ],
    },
    {
    "name": "HazardsCleanup",
    "commentary": "Cleanup of hazards grids and hazard table",
    "productType": None,
    "deleteGrids": [
       ("Fcst", "Hazards", "SFC", "all", "all"),
       ],
    "clearHazardsTable": 1,
    "checkStrings": [],
    },
    ]
       

import TestScript
def testScript(self, dataMgr):
    defaults = {
        "cmdLineVars" : "{('Issued By', 'issuedBy'): None}",
        "gridsStartTime": "20100101_0000",
        "database": "<site>_GRID__Fcst_00000000_0000",
        "publishGrids": 0,
        "decodeVTEC": 1,
        "orderStrings": 1,
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)









