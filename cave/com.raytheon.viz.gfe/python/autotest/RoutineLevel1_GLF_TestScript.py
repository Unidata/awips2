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
# GLF tests
#
# Author: hansen
# ----------------------------------------------------------------------------
import TestScript

scripts = [
    {
    "commentary":"""
    Morning Test at 4 a.m.
    """,
    "name":"GLF_1", 
    "productType":"GLF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '400 AM', ('Groupings', 'groupings'): 'West 1/2:East 1/2'}",
    "comboFlag": 0, 
    "checkStrings": ["...STORM WATCH IN EFFECT THROUGH THIS EVENING..."],
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", 0, 24, "SR.A", "all"),
        ],
    },
    {
    "commentary":"""
    Morning Test at 4 a.m.
    """,
    "name":"GLF_2", 
    "productType":"GLF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '400 AM', ('Groupings', 'groupings'): 'West 1/2:East 1/2'}",
    "comboFlag": 0, 
    "checkStrings": [
        "WEST HALF", "...STORM WATCH IN EFFECT THROUGH THIS EVENING...",
        "EAST HALF", "...HAZARDOUS SEAS WATCH IN EFFECT THROUGH THIS EVENING...",
        ],
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", 0, 24, "SR.A", ["west_half"]),
        ("Fcst", "Hazards", "DISCRETE", 0, 24, "SE.A", ["east_half"]),
        ],
    },
    {
    "commentary":"""
    Morning Test at 4 a.m.
    """,
    "name":"GLF_3", 
    "productType":"GLF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '400 AM', ('Groupings', 'groupings'): 'Entire Lake'}",
    "comboFlag": 0, 
    "checkStrings": [
       "...STORM WATCH IN EFFECT THROUGH THIS EVENING..."
        ],
    "notCheckStrings": [
       "WEST HALF", "EAST HALF",
        ],
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", 0, 24, "SR.A", ["west_half"]),
        ("Fcst", "Hazards", "DISCRETE", 0, 24, "SE.A", ["east_half"]),
        ],
    },
    ]

def testScript(self, dataMgr, level="Site"):
    gridsStartTime = self.getAbsFromLocal(2010, 1, 1, 0, 0)
    drtTime = self.getAbsFromLocal(2010, 1, 1, 4, 0)    
    defaults = {
        "gridsStartTime": gridsStartTime,
        "drtTime": drtTime,
        "orderStrings": 1,
        "internalStrip": 0, 
        }
    for script in scripts:
        drtHour = script.get("drtHour", None)
        if drtHour is not None:
            script["drtTime"] =  self.getAbsFromLocal(2010, 1, 1, drtHour, 0)      
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults, level=level)


