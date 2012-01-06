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
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Various non-routine products e.g. tables, simple tables
#
# Author:
# ----------------------------------------------------------------------------

def1 = "Definition =  {"
def2 = [
"""def begText(self, fcstDef, argDict):
    return "Beginning text from method\\n"
""",
"""def endText(self, fcstDef, argDict):
    return "Ending text from method\\n"
"""]

begEndText1 ="    }"

begEndText2=[
      "Definition['beginningText'] = mod.begText",
     " Definition['endingText'] = mod.endText"
]

scripts = [
    {
    "name":"Met1", 
    "productType":"MultipleElementTable",
    "commentary": "Basic Multiple Element Table - morning issuance",
    "cmdLineVars":  "{('Forecast Product', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "comboFlag": 0,
    "checkStrings": ["TEMPERATURE", "PRECIPITATION", "AREA 1"]
    },
    
    {
    "name":"Met2", 
    "productType":"MultipleElementTable",
    "commentary": "Basic Multiple Element Table - afternoon issuance",
    "cmdLineVars":  "{('Forecast Product', 'productIssuance'): 'Afternoon', ('Issued By', 'issuedBy'): None}",
    "comboFlag":  0, 
    "checkStrings":["TEMPERATURE","PRECIPITATION", "AREA 1"]
    },
    
    {
    "name":"RDFcst1", 
    "productType":"RDFcst",
    "commentary": "Basic RDFcst with area1 and area2",
    "cmdLineVars":  "{('Issued By', 'issuedBy'): None, 'Choose Edit Areas': ['area1', 'area2']}",
    "comboFlag":  0, 
    "checkStrings":["24 Hour Tabular Forecast for Area1", "6 AM    9 AM   12 PM    3 PM"]
    },

    {
    "name":"ElementByPeriod1", 
    "productType":"ElementByPeriod",
    "commentary": "Basic ElementByPeriod with area1",
    "cmdLineVars":  "{('Issued By', 'issuedBy'): None, 'Choose Edit Areas': ['area1']}",
    "comboFlag":  0, 
    "checkStrings":["Weather Element Table for Area 1", "Weather Element"]
    },
    
    {
    "name":"FirePeriodTable1", 
    "productType":"FirePeriodTable",
    "commentary": "Basic FirePeriodTable with area1 and area2",
    "cmdLineVars":  "{('Issued By', 'issuedBy'): None, 'Choose Edit Areas': ['area1', 'area2']}",
    "comboFlag":  0, 
    "checkStrings":["Edit Area   Sky (%)   LAL     MaxT    MinT ", "Area 1"]
    },

    {
    "name":"LE1", 
    "productType":"LE_Test_Local",
    "commentary": "Basic LE_Test_Local - morning issuance",
    "cmdLineVars":  "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Issuance Type', 'issuanceType'): 'ROUTINE'}",
    "comboFlag":  0, 
    "checkStrings":["ZONE FORECAST PRODUCT", ".TODAY"]
    },

    {
    "name":"Marine1", 
    "productType":"MarineSnapshotTable",
    "commentary": "Basic Marine Snapshot Table",
    "cmdLineVars":  "{('Issued By', 'issuedBy'): None, 'Choose Time Ranges': ['Today', 'Tonight', 'Tomorrow'], 'Choose Edit Areas': ['area1', 'area2']}",
    "comboFlag":  0, 
    "checkStrings":["Edit Area   Wind (kt)  Waves (ft)  Swells (ft)", "Area 1"]
    },

    {
    "name":"PeriodByArea1", 
    "productType":"PeriodByArea",
    "commentary": "Basic PeriodByArea1",
    "cmdLineVars":  "{('Issued By', 'issuedBy'): None, 'Choose Time Ranges': ['Today', 'Tonight', 'Tomorrow'], 'Choose Edit Areas': ['area1', 'area2']}",
    "comboFlag":  0,
    "checkStrings": [
          "Temperatures for Today",
          "Time Period   Area 1  Area 2"
          ],
    },
    {
    "name":"PeriodByArea2",
    "commentary": "Testing the beginningText/endingText as methods",
    "productType":"PeriodByArea",
    "cmdLineVars":  "{('Issued By', 'issuedBy'): None, 'Choose Time Ranges': ['Today', 'Tonight', 'Tomorrow'], 'Choose Edit Areas': ['area1', 'area2']}",
    "comboFlag":  0,
    "checkStrings": [
          "Beginning text from method",
          "Temperatures for Today",
          "Time Period   Area 1  Area 2",
          "Ending text from method",
          ],
    "fileChanges" : [
           ("PeriodByArea", "TextProduct", "replace", def2, "undo"),
           ("PeriodByArea", "TextProduct", "replace", begEndText2, "undo"),
           ],
    },

    {
    "name":"PeriodByElement1", 
    "productType":"PeriodByElement",
    "commentary": "Basic PeriodByElement Table",
    "cmdLineVars":  "{('Issued By', 'issuedBy'): None, 'Choose Time Ranges': ['Today', 'Tonight', 'Tomorrow'], 'Choose Edit Areas': ['area1', 'area2']}",
    "comboFlag":  0, 
    "checkStrings":["Area 1 for Today ", "Time Period   Sky (%)  Wind (mph)   Temp   Precip (%)"]
    },

    {
    "name":"PeriodTable1", 
    "productType":"PeriodTable",
    "commentary": "Basic PeriodTable",
    "cmdLineVars":  "{('Issued By', 'issuedBy'): None, 'Choose Time Ranges': ['Today', 'Tonight', 'Tomorrow'], 'Choose Edit Areas': ['area1', 'area2']}",
    "comboFlag":  0, 
    "checkStrings":["Edit Area   Sky (%)  Wind (mph)  Max Temp  Min Temp  Precip (%) ", "Area 1"]
    },

    {
    "name":"QPF1", 
    "productType":"QPFTable",
    "commentary": "Basic QPFTable",
    "cmdLineVars":  "{('Issued By', 'issuedBy'): None, 'Choose Edit Areas': ['area1', 'area2']}",
    "comboFlag":  0, 
    "checkStrings":["Experimental QPF Table", "Edit Area    QPF"]
    },

    {
    "name":"Rec1", 
    "productType":"RecreationFcst_Local",
    "commentary": "Basic Recreation Forecast",
    "cmdLineVars":  "{('Extended', 'extended'): 'With Extended', ('Issued By', 'issuedBy'): None, ('Product Title', 'title'): 'Recreation Statement', ('Choose Starting Time Range:', 'timeRangeName'): 'Tomorrow', ('Number of days:', 'numPeriods'): 2}",
    "comboFlag":  0, 
    "checkStrings":["DEW POINTS", "PRECIPITATION"]
    },

    {
    "name":"SET1", 
    "productType":"SmartElementTable_Local",
    "commentary": "Basic Smart Element Table - morning issuance",
    "cmdLineVars":  "{('Issued By', 'issuedBy'): None, 'Forecast Product': 'Morning'}",
    "comboFlag":  0,
    "checkStrings": ["TEMPERATURE", "PRECIPITATION ", "AREA 1"]
    },

    {
    "name":"SET2", 
    "productType":"SmartElementTable_Local",
    "commentary": "Basic Smart Element Table - afternoon issuance",
    "cmdLineVars":  "{('Issued By', 'issuedBy'): None, 'Forecast Product': 'Afternoon'}",
    "comboFlag":  0, 
    "checkStrings":["TEMPERATURE", "PRECIPITATION", "AREA 1"]
    },

    {
    "name":"SurfaceTemp1", 
    "productType":"SurfaceTemp",
    "commentary": "Basic SurfaceTemp product test",
    "cmdLineVars":  "{('Issued By', 'issuedBy'): None, 'Choose Edit Areas': ['area1', 'area2']}",
    "comboFlag":  0, 
    "checkStrings":["Experimental Surface Temperature Guidance Product", "Edit Area"]
    },   
    ]             


import TestScript
def testScript(self, dataMgr, level="Site"):
    defaults = {
        "internalStrip": 0,
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults, level=level)
