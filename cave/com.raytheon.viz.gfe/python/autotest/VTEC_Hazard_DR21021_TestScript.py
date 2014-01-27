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
# DR21021
#
# Author: mathewson
# ----------------------------------------------------------------------------

import TestScript

overrides = """
def _inlandAreas(self):
    return [
        "FLZ039", "FLZ042", "FLZ043", "FLZ048", "FLZ049", "FLZ050",
        ]
def _coastalAreas(self):
    return [
        "FLZ039", "FLZ042", "FLZ043", "FLZ048", "FLZ049", "FLZ050",
        ]
def _marineAreas(self):
    return [
        "GMZ830", "GMZ850", "GMZ853", "GMZ856", "GMZ870","GMZ873","GMZ876"
        ]    

def _cwa(self):
    return "TBW"

def _cwa_descriptor(self):
    return "CENTRAL WEST FLORIDA"

def _maor_descriptor(self):
    return "WEST CENTRAL FLORIDA AND THE GULF OF MEXICO"

def _cwa_maor_descriptor(self):
    return "WEST FLORIDA AND THE GULF OF MEXICO"

def _localReferencePoints(self):
    # Give the name and lat/lon for each local reference point
    return [
            ("Tampa Bay, FL", (28.01, -82.48)),
            ("Cape Coral, FL", (26.63, -82.00)),
            ("Lakeland, FL", (28.04, -81.95)),
            ("Sarasota, FL", (27.37, -82.55)),
            ]               

def _localReferencePoints_defaults(self):
    # Give a list of the local reference point names to be
    #  turned on by default
    return ["Tampa Bay, FL", "Sarasota, FL"]
"""

scripts = [
    {    
    "name":"TAE_DR21021_1", 
    "productType": None,
    "commentary": "Clear out all Hazard Tables and Grids",
    "checkStrings": [
         ],
    "clearHazardsTable": 1,
    },

    {    
    "name":"TAE_DR21021_2", 
    "productType": "Hazard_HLS",
    "drtTime": "20100201_0000",
    "gridsStartTime": "20100201_0000",
    "commentary": "TAE initial setup - step 1, create TR.W etn=0001",
    "checkStrings": [
      "GMZ830-020000-",
      "/O.NEW.KTBW.TR.W.0001.100201T0000Z-000000T0000Z/",
         ],
    "createGrids": [
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "<None>", "all"),
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "TR.W", ["GMZ830"]),
         ],
    },

    {    
    "name":"TAE_DR21021_3", 
    "productType": "Hazard_HLS",
    "drtTime": "20100202_0000",
    "commentary": "TAE initial setup - step 1, cancel TR.W",
    "checkStrings": [
      "GMZ830-020100-",
      "/O.CAN.KTBW.TR.W.0001.000000T0000Z-000000T0000Z/",
      ],
    "createGrids": [
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "<None>", "all"),
      ],
    },

    {    
    "name":"TAE_DR21021_4", 
    "productType": "Hazard_HLS",
    "drtTime": "20100401_0000",
    "gridsStartTime": "20100401_0000",
    "commentary": "TAE initial setup - setting up active table, create events for testing (NEW)",
    "checkStrings": [
      "FLZ039-020000-",
      "/O.NEW.KTBW.TR.W.1011.100401T0000Z-000000T0000Z/",
      "FLZ042-020000-",
      "/O.NEW.KTBW.TR.W.1011.100401T0000Z-000000T0000Z/",
      "FLZ043-020000-",
      "/O.NEW.KTBW.TR.W.1011.100401T0000Z-000000T0000Z/",
      "FLZ048-020000-",
      "/O.NEW.KTBW.HU.W.1011.100401T0000Z-000000T0000Z/",
      "FLZ049-020000-",
      "/O.NEW.KTBW.HU.W.1011.100401T0000Z-000000T0000Z/",
      "FLZ050-020000-",
      "/O.NEW.KTBW.HU.W.1011.100401T0000Z-000000T0000Z/",
      "GMZ850-020000-",
      "/O.NEW.KTBW.TR.W.0002.100401T0000Z-000000T0000Z/",
      "GMZ830-020000-",
      "/O.NEW.KTBW.TR.W.0002.100401T0000Z-000000T0000Z/",
      "GMZ853-020000-",
      "/O.NEW.KTBW.TR.W.0002.100401T0000Z-000000T0000Z/",
      "GMZ856-020000-",
      "/O.NEW.KTBW.TR.W.0002.100401T0000Z-000000T0000Z/",
      "GMZ870-020000-",
      "/O.NEW.KTBW.HU.W.0001.100401T0000Z-000000T0000Z/",
      "GMZ873-020000-",
      "/O.NEW.KTBW.HU.W.0001.100401T0000Z-000000T0000Z/",
      ],

    "createGrids": [
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "<None>", "all"),
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "TR.W", ["GMZ830","GMZ850","GMZ853","GMZ856"]),
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "HU.W", ["GMZ873","GMZ870"]),
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "TR.W:1011", ["FLZ039","FLZ042","FLZ043"]),
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "HU.W:1011", ["FLZ048","FLZ049","FLZ050"]),
      ],
    },

    {    
    "name":"TAE_DR21021_5", 
    "productType": "Hazard_HLS",
    "drtTime": "20100401_0000",
    "gridsStartTime": "20100401_0000",
    "commentary": "TAE initial setup - setting up active table, create events for testing (CON) - this is the initial testing state we need",
    "checkStrings": [
      "FLZ039-020000-",
      "/O.CON.KTBW.TR.W.1011.000000T0000Z-000000T0000Z/",
      "FLZ042-020000-",
      "/O.CON.KTBW.TR.W.1011.000000T0000Z-000000T0000Z/",
      "FLZ043-020000-",
      "/O.CON.KTBW.TR.W.1011.000000T0000Z-000000T0000Z/",
      "FLZ048-020000-",
      "/O.CON.KTBW.HU.W.1011.000000T0000Z-000000T0000Z/",
      "FLZ049-020000-",
      "/O.CON.KTBW.HU.W.1011.000000T0000Z-000000T0000Z/",
      "FLZ050-020000-",
      "/O.CON.KTBW.HU.W.1011.000000T0000Z-000000T0000Z/",
      "GMZ850-020000-",
      "/O.CON.KTBW.TR.W.0002.000000T0000Z-000000T0000Z/",
      "GMZ830-020000-",
      "/O.CON.KTBW.TR.W.0002.000000T0000Z-000000T0000Z/",
      "GMZ853-020000-",
      "/O.CON.KTBW.TR.W.0002.000000T0000Z-000000T0000Z/",
      "GMZ856-020000-",
      "/O.CON.KTBW.TR.W.0002.000000T0000Z-000000T0000Z/",
      "GMZ870-020000-",
      "/O.CON.KTBW.HU.W.0001.000000T0000Z-000000T0000Z/",
      "GMZ873-020000-",
      "/O.CON.KTBW.HU.W.0001.000000T0000Z-000000T0000Z/",
      ],
    "createGrids": [
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "<None>", "all"),
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "TR.W", ["GMZ830","GMZ850","GMZ853","GMZ856"]),
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "HU.W", ["GMZ873","GMZ870"]),
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "TR.W:1011", ["FLZ039","FLZ042","FLZ043"]),
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "HU.W:1011", ["FLZ048","FLZ049","FLZ050"]),
      ],
    },

    {    
    "name":"TAE_DR21021_6", 
    "productType": "Hazard_HLS",
    "drtTime": "20100401_0100",
    "gridsStartTime": "20100401_0000",
    "commentary": "TAE setting grids that caused error, change HU.W to TR.W",
    "checkStrings": [
      "FLZ039-020100-",
      "/O.CON.KTBW.TR.W.1011.000000T0000Z-000000T0000Z/",
      "FLZ042-020100-",
      "/O.CON.KTBW.TR.W.1011.000000T0000Z-000000T0000Z/",
      "FLZ043-020100-",
      "/O.CON.KTBW.TR.W.1011.000000T0000Z-000000T0000Z/",
      "FLZ048-020100-",
      "/O.CAN.KTBW.HU.W.1011.000000T0000Z-000000T0000Z/",
      "/O.EXA.KTBW.TR.W.1011.000000T0000Z-000000T0000Z/",
      "FLZ049-020100-",
      "/O.CAN.KTBW.HU.W.1011.000000T0000Z-000000T0000Z/",
      "/O.EXA.KTBW.TR.W.1011.000000T0000Z-000000T0000Z/",
      "FLZ050-020100-",
      "/O.CAN.KTBW.HU.W.1011.000000T0000Z-000000T0000Z/",
      "/O.EXA.KTBW.TR.W.1011.000000T0000Z-000000T0000Z/",
      "GMZ850-020100-",
      "/O.CON.KTBW.TR.W.0002.000000T0000Z-000000T0000Z/",
      "GMZ830-020100-",
      "/O.CON.KTBW.TR.W.0002.000000T0000Z-000000T0000Z/",
      "GMZ853-020100-",
      "/O.CON.KTBW.TR.W.0002.000000T0000Z-000000T0000Z/",
      "GMZ856-020100-",
      "/O.CON.KTBW.TR.W.0002.000000T0000Z-000000T0000Z/",
      "GMZ870-020100-",
      "/O.CAN.KTBW.HU.W.0001.000000T0000Z-000000T0000Z/",
      "/O.EXA.KTBW.TR.W.0002.000000T0000Z-000000T0000Z/",
      "GMZ873-020100-",
      "/O.CAN.KTBW.HU.W.0001.000000T0000Z-000000T0000Z/",
      "/O.EXA.KTBW.TR.W.0002.000000T0000Z-000000T0000Z/",
      ],
    "createGrids": [
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "<None>", "all"),
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "TR.W:1011", ["FLZ048","FLZ049","FLZ050","FLZ039","FLZ042","FLZ043"]),
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "TR.W", ["GMZ830","GMZ850","GMZ853","GMZ873","GMZ856","GMZ870"]),
      ],
    },

    {    
    "name":"TAE_DR21021_7, MOB_DR21021_1", 
    "productType": None,
    "commentary": "Clear out all Hazard Tables and Grids",
    "checkStrings": [
         ],
    "clearHazardsTable": 1,
    },

    {    
    "name":"MOB_DR21021_2", 
    "productType": "Hazard_HLS",
    "drtTime": "20101108_2214",
    "gridsStartTime": "20101108_2200",
    "commentary": "MOB initial setup - setting up active table",
    "checkStrings": [
      "FLZ039-092215-",
      "/O.NEW.KTBW.HU.A.1011.101108T2214Z-000000T0000Z/",
      "FLZ042-092215-",
      "/O.NEW.KTBW.HU.A.1011.101108T2214Z-000000T0000Z/",
      "FLZ043-092215-",
      "/O.NEW.KTBW.HU.A.1011.101108T2214Z-000000T0000Z/",
      "FLZ048-092215-",
      "/O.NEW.KTBW.HU.A.1011.101108T2214Z-000000T0000Z/",
      "FLZ049-092215-",
      "/O.NEW.KTBW.HU.A.1011.101108T2214Z-000000T0000Z/",
      "FLZ050-092215-",
      "/O.NEW.KTBW.HU.A.1011.101108T2214Z-000000T0000Z/",
      ],
    "createGrids": [
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "<None>", "all"),
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "HU.A:1011", ["FLZ049","FLZ050","FLZ039","FLZ042","FLZ043","FLZ048"]),
      ],
    },

    {    
    "name":"MOB_DR21021_3", 
    "productType": "Hazard_HLS",
    "drtTime": "20101108_2349",
    "gridsStartTime": "20101108_2200",
    "commentary": "MOB test - adding marine HU.A",
    "checkStrings": [
      "FLZ039-100000-",
      "/O.CON.KTBW.HU.A.1011.000000T0000Z-000000T0000Z/",
      "FLZ042-100000-",
      "/O.CON.KTBW.HU.A.1011.000000T0000Z-000000T0000Z/",
      "FLZ043-100000-",
      "/O.CON.KTBW.HU.A.1011.000000T0000Z-000000T0000Z/",
      "FLZ048-100000-",
      "/O.CON.KTBW.HU.A.1011.000000T0000Z-000000T0000Z/",
      "FLZ049-100000-",
      "/O.CON.KTBW.HU.A.1011.000000T0000Z-000000T0000Z/",
      "FLZ050-100000-",
      "/O.CON.KTBW.HU.A.1011.000000T0000Z-000000T0000Z/",
      "GMZ850-100000-",
      "/O.NEW.KTBW.HU.A.0001.101108T2349Z-000000T0000Z/",
      "GMZ853-100000-",
      "/O.NEW.KTBW.HU.A.0001.101108T2349Z-000000T0000Z/",
      "GMZ870-100000-",
      "/O.NEW.KTBW.HU.A.0001.101108T2349Z-000000T0000Z/",
      "GMZ876-100000-",
      "/O.NEW.KTBW.HU.A.0001.101108T2349Z-000000T0000Z/",
      ],

    "createGrids": [
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "<None>", "all"),
      ("Fcst", "Hazards", "DISCRETE", 0, 24, "HU.A:1011", ["FLZ048","FLZ050"]),
      ("Fcst", "Hazards", "DISCRETE", 24, 48, "<None>", "all"),
      ("Fcst", "Hazards", "DISCRETE", 24, 48, "HU.A", ["FLZ039","FLZ042","FLZ043","FLZ048","FLZ049","FLZ050","GMZ850","GMZ853","GMZ870","GMZ876"]),
      ],
    },

    {    
    "name":"MOB_DR21021_4", 
    "productType": None,
    "commentary": "Clear out all Hazard Tables and Grids",
    "checkStrings": [
         ],
    "clearHazardsTable": 1,
    },
    ]

def testScript(self, dataMgr, level="Site"):
    gridsStartTime = self.getAbsFromLocal(2010, 6, 1, 0, 0)
    drtTime = self.getAbsFromLocal(2010, 6, 1, 4, 0)
    defaults = {
        "gridsStartTime": gridsStartTime,
        "drtTime": drtTime,
        "internalStrip": 0, 
        "decodeVTEC": 1,
        "publishGrids": 1,
        "orderStrings": 1,
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        "fileChanges": [
          ("Hazard_HLS_<site>_Overrides", "TextUtility", "add", overrides, "undo")],
        "comboFlag": 1, 
        "cmdLineVars": "{('StormInfo_entry:', 'StormInfo_entry'): '', ('OverviewEditMode:', 'OverviewEditMode'): 'CreateFromGUI', ('MainHeadline_entry:', 'MainHeadline_entry'): '', ('NextUpdate:', 'NextUpdate'): 'Shortly', ('EventContext:', 'EventContext'): 'NonEvent', ('StormInfo:', 'StormInfo'): 'N/A (unnamed) -- Enter Storm Name below:', ('Uncertainty:', 'Uncertainty'): 'N/A', ('Issued By', 'issuedBy'): None, ('MainHeadline:', 'MainHeadline'): 'Enter', ('NextUpdate_entry:', 'NextUpdate_entry'): '', ('segments:', 'segments'): [(1, ['FLZ039'], 'Abbreviated', 'FirstIssuance', [], {'usePrev_HU_S_Headline': None, 'userHeadline_HU_S': None}), (2, ['FLZ042'], 'Abbreviated', 'FirstIssuance', [], {'usePrev_HU_S_Headline': None, 'userHeadline_HU_S': None}), (3, ['FLZ043'], 'Abbreviated', 'FirstIssuance', [], {'usePrev_HU_S_Headline': None, 'userHeadline_HU_S': None}), (4, ['FLZ048'], 'Abbreviated', 'FirstIssuance', [], {'usePrev_HU_S_Headline': None, 'userHeadline_HU_S': None}), (5, ['FLZ049'], 'Abbreviated', 'FirstIssuance', [], {'usePrev_HU_S_Headline': None, 'userHeadline_HU_S': None}), (6, ['FLZ050'], 'Abbreviated', 'FirstIssuance', [], {'usePrev_HU_S_Headline': None, 'userHeadline_HU_S': None}), (7, ['GMZ850'], 'Abbreviated', 'FirstIssuance', [], {'usePrev_HU_S_Headline': None, 'userHeadline_HU_S': None}), (8, ['GMZ830'], 'Abbreviated', 'FirstIssuance', [], {'usePrev_HU_S_Headline': None, 'userHeadline_HU_S': None}), (9, ['GMZ853'], 'Abbreviated', 'FirstIssuance', [], {'usePrev_HU_S_Headline': None, 'userHeadline_HU_S': None}), (10, ['GMZ856'], 'Abbreviated', 'FirstIssuance', [], {'usePrev_HU_S_Headline': None, 'userHeadline_HU_S': None}), (11, ['GMZ870'], 'Abbreviated', 'FirstIssuance', [], {'usePrev_HU_S_Headline': None, 'userHeadline_HU_S': None}),(12, ['GMZ873'], 'Abbreviated', 'FirstIssuance', [], {'usePrev_HU_S_Headline': None, 'userHeadline_HU_S': None}),(13, ['GMZ876'], 'Abbreviated', 'FirstIssuance', [], {'usePrev_HU_S_Headline': None, 'userHeadline_HU_S': None})], ('LocalReferencePoints:', 'LocalReferencePoints'): [('Mobile, AL', (30.670000000000002, -88.129999999999995)), ('Pensacola, FL', (30.449999999999999, -87.200000000000003))]}",
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults, level=level)

