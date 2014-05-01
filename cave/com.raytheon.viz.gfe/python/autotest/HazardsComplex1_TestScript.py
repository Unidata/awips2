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
       ("Fcst", "Hazards", "DISCRETE", 22, 28, "CF.Y", ["FLZ048", "FLZ049", "FLZ050"]),
       ],
    "checkStrings": [
       "COASTAL HAZARD MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "FLZ048>050-010800-",
       "/O.NEW.KTBW.CF.Y.0001.100101T2200Z-100102T0400Z/",
       "HERNANDO-PASCO-PINELLAS-",
       "700 PM EST THU DEC 31 2009",
       "...COASTAL FLOOD ADVISORY IN EFFECT FROM 5 PM TO 11 PM EST FRIDAY...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A COASTAL FLOOD ADVISORY...WHICH IS IN EFFECT FROM 5 PM TO 11 PM EST FRIDAY.",
       "|* SEGMENT TEXT GOES HERE *|.",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A COASTAL FLOOD ADVISORY INDICATES THAT ONSHORE WINDS AND TIDES WILL COMBINE TO GENERATE FLOODING OF LOW AREAS ALONG THE SHORE.",
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
       ("Fcst", "Hazards", "DISCRETE", 22, 28, "CF.Y", ["FLZ048", "FLZ049", "FLZ050"]),
       ],
    "checkStrings": [
       "COASTAL HAZARD MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "FLZ048>050-010800-",
       "/O.CON.KTBW.CF.Y.0001.100101T2200Z-100102T0400Z/",
       "HERNANDO-PASCO-PINELLAS-",
       "700 PM EST THU DEC 31 2009",
       "...COASTAL FLOOD ADVISORY REMAINS IN EFFECT FROM 5 PM TO 11 PM EST FRIDAY...",
       "A COASTAL FLOOD ADVISORY REMAINS IN EFFECT FROM 5 PM TO 11 PM EST FRIDAY.",
       "|* SEGMENT TEXT GOES HERE *|.",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A COASTAL FLOOD ADVISORY INDICATES THAT ONSHORE WINDS AND TIDES WILL COMBINE TO GENERATE FLOODING OF LOW AREAS ALONG THE SHORE.",
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
        ("Fcst", "Hazards", "DISCRETE", 22, 34, "CF.Y", ["FLZ048", "FLZ049", "FLZ050"]),
        ],
    "checkStrings": [
        "COASTAL HAZARD MESSAGE",
        "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
        "700 PM EST THU DEC 31 2009",
        "FLZ048>050-010800-",
        "/O.EXT.KTBW.CF.Y.0001.100101T2200Z-100102T1000Z/",
        "HERNANDO-PASCO-PINELLAS-",
        "700 PM EST THU DEC 31 2009",
        "...COASTAL FLOOD ADVISORY NOW IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY...",
        "THE COASTAL FLOOD ADVISORY IS NOW IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY.",
        "|* SEGMENT TEXT GOES HERE *|.",
        "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
        "A COASTAL FLOOD ADVISORY INDICATES THAT ONSHORE WINDS AND TIDES WILL COMBINE TO GENERATE FLOODING OF LOW AREAS ALONG THE SHORE.",
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
        ("Fcst", "Hazards", "DISCRETE", 16, 28, "CF.Y", ["FLZ048", "FLZ049", "FLZ050"]),
        ],
    "checkStrings": [
       "COASTAL HAZARD MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "FLZ048>050-010800-",
       "/O.EXT.KTBW.CF.Y.0001.100101T1600Z-100102T0400Z/",
       "HERNANDO-PASCO-PINELLAS-",
       "700 PM EST THU DEC 31 2009",
       "...COASTAL FLOOD ADVISORY NOW IN EFFECT FROM 11 AM TO 11 PM EST FRIDAY...",
       "THE COASTAL FLOOD ADVISORY IS NOW IN EFFECT FROM 11 AM TO 11 PM EST FRIDAY.",
       "|* SEGMENT TEXT GOES HERE *|.",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A COASTAL FLOOD ADVISORY INDICATES THAT ONSHORE WINDS AND TIDES WILL COMBINE TO GENERATE FLOODING OF LOW AREAS ALONG THE SHORE.",
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
        ("Fcst", "Hazards", "DISCRETE", 16, 28, "CF.Y", ["FLZ048", "FLZ049", "FLZ050", "FLZ065"]),
    ],
    "checkStrings": [
        "COASTAL HAZARD MESSAGE",
        "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
        "700 PM EST THU DEC 31 2009",
        "FLZ065-010800-",
        "/O.EXA.KTBW.CF.Y.0001.100101T1600Z-100102T0400Z/",
        "LEE-",
        "700 PM EST THU DEC 31 2009",
        "...COASTAL FLOOD ADVISORY IN EFFECT FROM 11 AM TO 11 PM EST FRIDAY...",
        "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A COASTAL FLOOD ADVISORY...WHICH IS IN EFFECT FROM 11 AM TO 11 PM EST FRIDAY.",
        "|* SEGMENT TEXT GOES HERE *|.",
        "A COASTAL FLOOD ADVISORY INDICATES THAT ONSHORE WINDS AND TIDES WILL COMBINE TO GENERATE FLOODING OF LOW AREAS ALONG THE SHORE.",
        "$$",
        "FLZ048>050-010800-",
        "/O.CON.KTBW.CF.Y.0001.100101T1600Z-100102T0400Z/",
        "HERNANDO-PASCO-PINELLAS-",
        "700 PM EST THU DEC 31 2009",
        "...COASTAL FLOOD ADVISORY REMAINS IN EFFECT FROM 11 AM TO 11 PM EST FRIDAY...",
        "A COASTAL FLOOD ADVISORY REMAINS IN EFFECT FROM 11 AM TO 11 PM EST FRIDAY.",
        "|* SEGMENT TEXT GOES HERE *|.",
        "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
        "A COASTAL FLOOD ADVISORY INDICATES THAT ONSHORE WINDS AND TIDES WILL COMBINE TO GENERATE FLOODING OF LOW AREAS ALONG THE SHORE.",
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
        ("Fcst", "Hazards", "DISCRETE", 22, 34, "CF.Y", ["FLZ048", "FLZ049", "FLZ050", "FLZ065", "FLZ039"]),
        ],
    "checkStrings": [
       "COASTAL HAZARD MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "FLZ039-010800-",
       "/O.EXB.KTBW.CF.Y.0001.100101T2200Z-100102T1000Z/",
       "LEVY-",
       "700 PM EST THU DEC 31 2009",
       "...COASTAL FLOOD ADVISORY IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A COASTAL FLOOD ADVISORY...WHICH IS IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY.",
       "|* SEGMENT TEXT GOES HERE *|.",
       "A COASTAL FLOOD ADVISORY INDICATES THAT ONSHORE WINDS AND TIDES WILL COMBINE TO GENERATE FLOODING OF LOW AREAS ALONG THE SHORE.",
       "$$",
       "FLZ048>050-065-010800-",
       "/O.EXT.KTBW.CF.Y.0001.100101T2200Z-100102T1000Z/",
       "HERNANDO-PASCO-PINELLAS-LEE-",
       "700 PM EST THU DEC 31 2009",
       "...COASTAL FLOOD ADVISORY NOW IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY...",
       "THE COASTAL FLOOD ADVISORY IS NOW IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY.",
       "|* SEGMENT TEXT GOES HERE *|.",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A COASTAL FLOOD ADVISORY INDICATES THAT ONSHORE WINDS AND TIDES WILL COMBINE TO GENERATE FLOODING OF LOW AREAS ALONG THE SHORE.",
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
        ("Fcst", "Hazards", "DISCRETE", 22, 34, "CF.Y", ["FLZ048", "FLZ049", "FLZ050"]),
        ],
    "checkStrings": [
       "COASTAL HAZARD MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST THU DEC 31 2009",
       "FLZ039-065-010100-",
       "/O.CAN.KTBW.CF.Y.0001.100101T2200Z-100102T1000Z/",
       "LEVY-LEE-",
       "700 PM EST THU DEC 31 2009",
       "...COASTAL FLOOD ADVISORY IS CANCELLED...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE COASTAL FLOOD ADVISORY.",
       "$$",
       "FLZ048>050-010800-",
       "/O.CON.KTBW.CF.Y.0001.100101T2200Z-100102T1000Z/",
       "HERNANDO-PASCO-PINELLAS-",
       "700 PM EST THU DEC 31 2009",
       "...COASTAL FLOOD ADVISORY REMAINS IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY...",
       "A COASTAL FLOOD ADVISORY REMAINS IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY.",
       "|* SEGMENT TEXT GOES HERE *|.",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A COASTAL FLOOD ADVISORY INDICATES THAT ONSHORE WINDS AND TIDES WILL COMBINE TO GENERATE FLOODING OF LOW AREAS ALONG THE SHORE.",
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
        ("Fcst", "Hazards", "DISCRETE", 22, 34, "CF.W", ["FLZ048", "FLZ049", "FLZ050"]),
        ],
    "checkStrings": [
        "URGENT - IMMEDIATE BROADCAST REQUESTED",
        "COASTAL HAZARD MESSAGE",
        "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
        "700 PM EST THU DEC 31 2009",
        "FLZ048>050-010800-",
        "/O.UPG.KTBW.CF.Y.0001.100101T2200Z-100102T1000Z/",
        "/O.NEW.KTBW.CF.W.0001.100101T2200Z-100102T1000Z/",
        "HERNANDO-PASCO-PINELLAS-",
        "700 PM EST THU DEC 31 2009",
        "...COASTAL FLOOD WARNING IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY...",
        "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A COASTAL FLOOD WARNING...WHICH IS IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY. THE COASTAL FLOOD ADVISORY IS NO LONGER IN EFFECT.",
        "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
        "A COASTAL FLOOD WARNING MEANS THAT FLOODING IS OCCURRING OR IMMINENT. COASTAL RESIDENTS IN THE WARNED AREA SHOULD BE ALERT FOR RISING WATER...AND TAKE APPROPRIATE ACTION TO PROTECT LIFE AND PROPERTY.",
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
        ("Fcst", "Hazards", "DISCRETE", 35, 41, "CF.Y", ["FLZ049"]),
        ],
    "checkStrings": [
        "COASTAL HAZARD MESSAGE",
        "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
        "700 PM EST THU DEC 31 2009",
        "FLZ049-010800-",
        "/O.NEW.KTBW.CF.Y.0002.100102T1100Z-100102T1700Z/",
        "/O.CON.KTBW.CF.W.0001.100101T2200Z-100102T1000Z/",
        "PASCO-",
        "700 PM EST THU DEC 31 2009",
        "...COASTAL FLOOD WARNING REMAINS IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY...",
        "...COASTAL FLOOD ADVISORY IN EFFECT FROM 6 AM TO NOON EST SATURDAY...",
        "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A COASTAL FLOOD ADVISORY...WHICH IS IN EFFECT FROM 6 AM TO NOON EST SATURDAY. A COASTAL FLOOD WARNING REMAINS IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY.",
        "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
        "A COASTAL FLOOD WARNING MEANS THAT FLOODING IS OCCURRING OR IMMINENT. COASTAL RESIDENTS IN THE WARNED AREA SHOULD BE ALERT FOR RISING WATER...AND TAKE APPROPRIATE ACTION TO PROTECT LIFE AND PROPERTY.",
        "A COASTAL FLOOD ADVISORY INDICATES THAT ONSHORE WINDS AND TIDES WILL COMBINE TO GENERATE FLOODING OF LOW AREAS ALONG THE SHORE.",
        "&&",
        "$$",
        "FLZ048-050-010800-",
        "/O.CON.KTBW.CF.W.0001.100101T2200Z-100102T1000Z/",
        "HERNANDO-PINELLAS-",
        "700 PM EST THU DEC 31 2009",
        "...COASTAL FLOOD WARNING REMAINS IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY...",
        "A COASTAL FLOOD WARNING REMAINS IN EFFECT FROM 5 PM FRIDAY TO 5 AM EST SATURDAY.",
        "A COASTAL FLOOD WARNING MEANS THAT FLOODING IS OCCURRING OR IMMINENT. COASTAL RESIDENTS IN THE WARNED AREA SHOULD BE ALERT FOR RISING WATER...AND TAKE APPROPRIATE ACTION TO PROTECT LIFE AND PROPERTY.",
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
    "checkStrings": ["COASTAL HAZARD MESSAGE",
                     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                     "700 PM EST THU DEC 31 2009",
                     "FLZ049-",
                     "/O.CAN.KTBW.CF.W.0001.100101T2200Z-100102T1000Z/",
                     "/O.CAN.KTBW.CF.Y.0002.100102T1100Z-100102T1700Z/",
                     "PASCO-",
                     "700 PM EST THU DEC 31 2009",
                     "...COASTAL FLOOD WARNING IS CANCELLED...",
                     "...COASTAL FLOOD ADVISORY IS CANCELLED...",
                     "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE COASTAL FLOOD WARNING. THE COASTAL FLOOD ADVISORY HAS BEEN CANCELLED.",
                     "FLZ048-050-",
                     "/O.CAN.KTBW.CF.W.0001.100101T2200Z-100102T1000Z/",
                     "HERNANDO-PINELLAS-",
                     "700 PM EST THU DEC 31 2009",
                     "...COASTAL FLOOD WARNING IS CANCELLED...",
                     "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE COASTAL FLOOD WARNING.",
         ],
    },
    {
    "name": "Hazards11",
    "productType": "Hazard_CFW_Local",
    "commentary": "Generating a LS.S in one zone, to generate NEW vtec.",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", 20, 28, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 20, 28, "LS.S", ["FLZ039"]),
        ],
    "checkStrings": ["LAKESHORE HAZARD MESSAGE",
                  "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
                  "/O.NEW.KTBW.LS.S.0001.100101T2000Z-100102T0400Z/",
                  "LEVY-",
                  "|* STATEMENT TEXT GOES HERE *|.",
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
        "database": "<site>_GRID__Official_00000000_0000",
        "publishGrids": 1,
        "decodeVTEC": 1,
        "orderStrings": 1,
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)









