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
# AFD Tests
#
# Author:
# ----------------------------------------------------------------------------

def1 = """#Definition["state_IDs"] = ["ST"]"""
def2 = """Definition["state_IDs"] = ["FL"]"""

def3 = """

Combinations.append((["FLZ999"], "Region99"))
a = EASourceMap['FireWxZones_TBW']
a.append('FLZ999')
EASourceMap['FireWxZones_TBW'].append("FLZ999")

"""

definitions = """Definition["abbreviateUGCs"] = 0"""

scripts = [
    {
    "commentary": "Clear out all Hazards Table and Grids.",
    "name": "AFDHazardBlock_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

    {
    "name":"WSW_1", 
    "productType":"Hazard_WSW", 
    "commentary": "Creating ZR.Y hazard for three zones in WSW product.",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "ZR.Y", ["FLZ050","FLZ051","FLZ065"]),],
    "comboFlag": 0, 
    "decodeVTEC": 1,
    "vtecMode": "O",
    "checkStrings": [
       ],
    },   

    {
    "name":"AFDHazardBlockAFD_1", 
    "productType":"AFD", 
    "commentary": "Testing AFD hazard block, with no fire weather hazard in fire weather area.",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "ZR.Y", ["FLZ050","FLZ051","FLZ065"]),],
    "comboFlag": 0, 
    "writableCopies": [
        ("FLZ050","REFERENCE","FLZ999"),
        #("EditAreas_PublicMarineFireWx_<site>","COMBINATIONS","EditAreas_PublicMarineFireWx_<site>"),
        ],
    "fileChanges": [
      ("AFD_<site>_Definition", "TextProduct", "replace", def2, "undo"),
      ("EditAreas_PublicMarineFireWx_<site>", "COMBINATIONS", "add", 'Combinations.append((["FLZ999"], "Region99"))', "delete"),
      ("EditAreas_PublicMarineFireWx_<site>", "COMBINATIONS", "add", 'EASourceMap["FireWxZones_TBW"].append("FLZ999")', "delete"),      
      ],
    "cmdLineVars": "{('Issued By', 'issuedBy'): None, ('IncludePrevious AFD?', 'includePreviousAFD'): 'NO', ('Long TermForecaster', 'longTermFcstrNumber'): '99', ('Product Issuance', 'productIssuance'): 'Morning', ('Short TermForecaster', 'shortTermFcstrNumber'): '99', ('OptionalTopics', 'optionalTopics'): [], ('AviationForecaster', 'aviationFcstrNumber'): '99'}",
    "notCheckStrings": [
       "FREEZING RAIN ADVISORY UNTIL MIDNIGHT EST TONIGHT FOR .",
       ],
    "checkStrings": [
       "FXUS62 KTBW 160500",
       "AFDTBW",
       "AREA FORECAST DISCUSSION",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "1200 AM EST SAT JAN 16 2010",
       ".SHORT TERM...",
       ".LONG TERM...",
       "&&",
       ".TBW WATCHES/WARNINGS/ADVISORIES...",
       "FREEZING RAIN ADVISORY UNTIL MIDNIGHT EST TONIGHT FOR FLZ050-051-",
       "065.",
       "&&",
       "$$",
       ],
    },
    
    {
    "name":"AFDHazardBlockAFD_2", 
    "productType":"AFD", 
    "commentary": "Testing AFD hazard block, with multiple segments defined in hazards grid, with results of all combined into one item",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", -10, 0, "ZR.Y:1", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", -10, 0, "ZR.Y:2", ["FLZ065"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "ZR.Y:1", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "ZR.Y:2", ["FLZ065"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "ZR.Y:3", ["FLZ051"])],
    "comboFlag": 0, 
    "fileChanges": [
      ("AFD_<site>_Definition", "TextProduct", "replace", def2, "undo"),
      ],
    "cmdLineVars": "{('Issued By', 'issuedBy'): None, ('IncludePrevious AFD?', 'includePreviousAFD'): 'NO', ('Long TermForecaster', 'longTermFcstrNumber'): '99', ('Product Issuance', 'productIssuance'): 'Morning', ('Short TermForecaster', 'shortTermFcstrNumber'): '99', ('OptionalTopics', 'optionalTopics'): [], ('AviationForecaster', 'aviationFcstrNumber'): '99'}",
    "checkStrings": [
       "AFDTBW",
       "AREA FORECAST DISCUSSION",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "1200 AM EST SAT JAN 16 2010",
       ".SHORT TERM...",
       ".LONG TERM...",
       "&&",
       ".TBW WATCHES/WARNINGS/ADVISORIES...",
       "FREEZING RAIN ADVISORY UNTIL MIDNIGHT EST TONIGHT FOR FLZ050-051-",
       "065.",
       "&&",
       "$$",
       ],
    },
    
    {
    "name":"AFD_2", 
    "productType":"AFD", 
    "commentary": "Testing AFD with optional topics AVIATION and MARINE",
    "clearHazardsTable": 1,
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "ZR.Y", ["FLZ050","FLZ051","FLZ052"]),
       ],
    "comboFlag": 0, 
    "fileChanges": [
      ("AFD_<site>_Definition", "TextProduct", "replace", def2, "undo"),
      ],
    "cmdLineVars": "{('Issued By', 'issuedBy'): None, ('IncludePrevious AFD?', 'includePreviousAFD'): 'NO', ('Long TermForecaster', 'longTermFcstrNumber'): '99', ('Product Issuance', 'productIssuance'): 'Morning', ('Short TermForecaster', 'shortTermFcstrNumber'): '99', ('OptionalTopics', 'optionalTopics'): ['AVIATION', 'MARINE'], ('AviationForecaster', 'aviationFcstrNumber'): '99'}",
    "checkStrings": [
       "FXUS62 KTBW 160500",
       "AFDTBW",
       "AREA FORECAST DISCUSSION",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "1200 AM EST SAT JAN 16 2010",
       ".SHORT TERM...",
       ".LONG TERM...",
       "&&",
       ".TBW WATCHES/WARNINGS/ADVISORIES...",
       "FREEZING RAIN ADVISORY UNTIL MIDNIGHT EST TONIGHT FOR FLZ050>052.",
       "&&",
       "$$",
       ],
    },   

    {
    "name":"AFD_3", 
    "productType":"AFD", 
    "commentary": "Testing AFD with optional topics UPDATE and SYNOPSIS",
    "clearHazardsTable": 1,
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "ZR.Y", ["FLZ050","FLZ051","FLZ052"]),
       ],
    "comboFlag": 0, 
    "fileChanges": [
      ("AFD_<site>_Definition", "TextProduct", "replace", def2, "undo"),
      ("AFD_<site>_Definition", "TextProduct", "add", definitions, "undo"),
      ],
    "cmdLineVars": "{('Issued By', 'issuedBy'): None, ('IncludePrevious AFD?', 'includePreviousAFD'): 'NO', ('Long TermForecaster', 'longTermFcstrNumber'): '99', ('Product Issuance', 'productIssuance'): 'Morning', ('Short TermForecaster', 'shortTermFcstrNumber'): '99', ('OptionalTopics', 'optionalTopics'): ['.UPDATE...', '.SYNOPSIS...'], ('AviationForecaster', 'aviationFcstrNumber'): '99'}",
    "checkStrings": [
       "FXUS62 KTBW 160500",
       "AFDTBW",
       "AREA FORECAST DISCUSSION",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "1200 AM EST SAT JAN 16 2010",
       ".UPDATE...",
       "&&",
       ".SYNOPSIS...",
       "&&",
       ".SHORT TERM...",
       ".LONG TERM...",
       "&&",
       ".TBW WATCHES/WARNINGS/ADVISORIES...",
       "FREEZING RAIN ADVISORY UNTIL MIDNIGHT EST TONIGHT FOR FLZ050-",
       "FLZ051-FLZ052.",
       "&&",
       "$$",
       ],
    },
    
    {
    "name":"AFD_4",
    "commentary": "Testing AFD using forecaster names and numbers",
    "productType":"AFD", 
    "clearHazardsTable": 1,
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "ZR.Y", ["FLZ050","FLZ051","FLZ052"]),
       ],
    "comboFlag": 0, 
    "fileChanges": [
      ("AFD_<site>_Definition", "TextProduct", "replace", def2, "undo"),
      ("AFD_<site>_Definition", "TextProduct", "add", definitions, "undo"),
      ],
    "cmdLineVars": "{('Issued By', 'issuedBy'): None, ('IncludePrevious AFD?', 'includePreviousAFD'): 'NO', ('Long TermForecaster', 'longTermFcstrNumber'): 'Fred', ('Product Issuance', 'productIssuance'): 'Morning', ('Short TermForecaster', 'shortTermFcstrNumber'): '99', ('OptionalTopics', 'optionalTopics'): ['.UPDATE...', '.SYNOPSIS...'], ('AviationForecaster', 'aviationFcstrNumber'): '99'}",
    "checkStrings": [
       "FXUS62 KTBW 160500",
       "AFDTBW",
       "AREA FORECAST DISCUSSION",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "1200 AM EST SAT JAN 16 2010",
       ".UPDATE...",
       "&&",
       ".SYNOPSIS...",
       "&&",
       ".SHORT TERM...",
       ".LONG TERM...",
       "&&",
       ".TBW WATCHES/WARNINGS/ADVISORIES...",
       "FREEZING RAIN ADVISORY UNTIL MIDNIGHT EST TONIGHT FOR FLZ050-",
       "FLZ051-FLZ052.",
       "&&",
       "$$",
       "99/FRED",
       ],
    },

    {
    "commentary": "Deleting hazard grids.",
    "name": "Cleanup",
    "productType": None,
    "checkStrings": [],
    "clearHazardsTable": 1,
    },
    ]

       
import TestScript
def testScript(self, dataMgr):
    gridsStartTime = self.getAbsFromLocal(2010, 1, 1, 0, 0)
    drtTime = self.getAbsFromLocal(2010, 1, 1, 4, 0)
    defaults = {
        "gridsStartTime": "20100116_0500",
        "drtTime": "20100116_0500",
        "database": "<site>_GRID__Fcst_00000000_0000",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        "publishGrids": 1,
        "decodeVTEC": 0,
        "orderStrings": 1,
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




