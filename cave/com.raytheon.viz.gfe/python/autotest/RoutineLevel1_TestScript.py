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
# Routine Products Site level
#
# Author:
# ----------------------------------------------------------------------------

def1 = """#Definition["state_IDs"] = ["ST"]"""
def2 = """Definition["state_IDs"] = ["FL"]"""

scripts = [
    {    
    "name":"AFD1",
    "commentary": "Basic AFD formatter run.",
    "productType":"AFD",
    "cmdLineVars": "{('Issued By', 'issuedBy'): None, ('IncludePrevious AFD?', 'includePreviousAFD'): 'NO', ('Long TermForecaster', 'longTermFcstrNumber'): '99', ('Product Issuance', 'productIssuance'): 'Morning', ('Short TermForecaster', 'shortTermFcstrNumber'): '99', ('OptionalTopics', 'optionalTopics'): [], ('AviationForecaster', 'aviationFcstrNumber'): '99'}",
    "comboFlag": 0,
    "checkStrings": ["Area Forecast Discussion", ".SHORT TERM"],
    "createGrids": [("Fcst", "Hazards", "DISCRETE", 0, 12, "WS.W", "all")],
    "fileChanges": [("AFD_<site>_Definition", "TextUtility", "replace", (def1, def2), "undo")],
    },
    {    
    "name":"AFM1", 
    "productType":"AFM",
    "commentary": "Basic AFM formatter run.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1,
    "combinations" : "ZONE",
    "checkStrings": ["Area Forecast Matrices", "DATE", "MAX/MIN"],
    },
    {

    "name":"CCF1", 
    "productType":"CCF",
    "commentary": "Basic CCF formatter run.",
     "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Forecaster Number', 'forecasterNumber'): 99.0}",
    "comboFlag": 0, 
    "checkStrings": ["CCF", "AREA1", "AREA2"],    
    },
    {
    "name":"CWF1", 
    "productType":"CWF",
    "commentary": "Basic CWF formatter run.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "comboFlag":  1,
    "combinations": "ZONE",
    "checkStrings": ["Coastal Waters Forecast", ".TODAY...", ".TONIGHT..."],
    },
    {
    "name":"CWF_Pacific1", 
    "productType":"CWF_Pacific",
    "commentary": "Basic CWF_Pacific formatter run.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "comboFlag":  1, 
    "combinations": "ZONE",
    "checkStrings": ["Coastal Waters Forecast", ".TODAY...", ".TONIGHT..."], 
    },
    {
    "name":"FWF1", 
    "productType":"FWF",
    "commentary": "Basic FWF formatter run.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "comboFlag":  1, 
    "combinations": "ZONE",
    "checkStrings": ["Fire Weather Planning Forecast", ".TODAY...", "SKY/WEATHER"], 
    },
    {
    "name":"FWFTable1", 
    "productType":"FWFTable",
    "commentary": "Basic FWFTable formatter run.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "comboFlag":  1, 
    "combinations": "ZONE",
    "checkStrings": ["Fire Weather Planning Forecast", ""], 
    },
    {
    "name":"FWM1", 
    "productType":"FWM",
    "commentary": "Basic FWM formatter run.",
    "cmdLineVars":  "{('Issued By', 'issuedBy'): None}",
    "comboFlag":  0, 
    "checkStrings": ["FWM", "FCST", "ZONE"], 
    },
    {
    "name":"FWS1", 
    "productType":"FWS",
    "commentary": "Basic FWS formatter run.",
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'AGENCY 1', ('Tomorrow Elements', 'tomorrowElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Narrative Only', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '01/01/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 3, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",
    "comboFlag":  0, 
    "checkStrings": ["Spot Forecast", ".TODAY...", "TEMPERATURE"], 
    },
    
    {
    "name":"GLF1", 
    "productType":"GLF",
    "commentary": "Basic GLF formatter run.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '400 AM', ('Issued By', 'issuedBy'): None}",
    "comboFlag":  0, 
    "checkStrings": ["Lake Superior forecast beyond five nautical", ".TODAY...", ".TONIGHT..."], 
    },
    {
    "name":"MVF1", 
    "productType":"MVF",
    "commentary": "Basic MVF formatter run.",
    "cmdLineVars": "{('Issued By', 'issuedBy'): None, ('Forecaster Number', 'forecasterNumber'): 99.0}",
    "comboFlag":  0, 
    "checkStrings": ["MVF", "%%"], 
    },
    {
    "name":"NOW1", 
    "productType":"NOW_Local",
    "commentary": "Basic NOW formatter run.",
    "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
    "comboFlag":  1, 
    "combinations": "ZONE",
    "checkStrings": ["Short Term Forecast", ".NOW"], 
    },
    {
    "name":"NSH1", 
    "productType":"NSH",
    "commentary": "Basic NSH formatter run.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '430 AM', ('Issued By', 'issuedBy'): None}",
    "comboFlag":  1, 
    "combinations": "ZONE",
    "checkStrings": ["Nearshore Marine Forecast", ".TODAY...", ".TONIGHT..."], 
    },
    {
    "name":"OFF1", 
    "productType":"OFF",
    "commentary": "Basic OFF formatter run.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '400 AM', ('Issued By', 'issuedBy'): None}",
    "comboFlag":  1, 
    "combinations": "ZONE",
    "checkStrings": ["Offshore Forecast", ".TODAY...", ".TONIGHT..."], 
    },
    {
    "name":"PFM1", 
    "productType":"PFM",
    "commentary": "Basic PFM formatter run.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
    "comboFlag":  0, 
    "checkStrings": ["Point Forecast Matrices", "DATE", "MAX/MIN"], 
    },
    {
    "name":"SAF1", 
    "productType":"SAF",
    "commentary": "Basic SAF formatter run.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Number of Periods', 'numPeriods'): 'All'}",
    "comboFlag":  0, 
    "checkStrings": ["Now for the official National Weather Service forecast", "Today,", "Tonight,"], 
    },
    {
    "name":"SRF1", 
    "productType":"SRF",
    "commentary": "Basic SRF formatter run.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Number of Periods', 'numPeriods'): 'All'}",
    "comboFlag":  0, 
    "checkStrings": ["SRFABC", "Surfzone Forecast for Florida"] 
    },
    {
    "name":"SFT1", 
    "productType":"SFT",
    "commentary": "Basic SFT formatter run.",
    "comboFlag": 0, 
    "checkStrings": ["Tabular State Forecast", "Today"], 
    },
    {
    "name":"SPS", 
    "productType":"SPS_Local",
    "commentary": "Basic SPS formatter run.",
    "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
    "combinations": "ZONE",
    "checkStrings": ["", ""], 
    },
    {
    "name":"ZFP1", 
    "productType":"ZFP", 
    "commentary": "Basic ZFP formatter run.",
    "comboFlag": 1, 
    "combinations": "ZONE",
    "checkStrings": ["Zone Forecast Product", ".TODAY...", ".TONIGHT..."], 
    },   
    ]

import TestScript
def testScript(self, dataMgr, level="Site"):
    return TestScript.generalTestScript(self, dataMgr, scripts, {}, level=level)



