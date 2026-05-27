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
# SPW tests
#
# Author: hansen
# ----------------------------------------------------------------------------


XTestGenerationDictionary = {
    "Precip": {
       "element": "Wx",
       "cases": {
           "noLE": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12,"Chc:RW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12,"Chc:RW:-:<NoVis>:", ["BelowElev"]),
                ],
           "LE": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "Lkly:RW:-:<NoVis>:", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Lkly:RW:-:<NoVis>:", ["BelowElev"]),
                ],
           },
       },

    "Sky": {
       "element": "Sky",
       "cases": {
           "noLE": [
                ("Fcst", "Sky", "SCALAR", 0, 6, 50, ["AboveElev"]),
                ("Fcst", "Sky", "SCALAR", 0, 6, 50, ["BelowElev"]),
                ("Fcst", "Sky", "SCALAR", 6, 12, 50, ["AboveElev"]),
                ("Fcst", "Sky", "SCALAR", 6, 12, 50, ["BelowElev"]),
                ],
           "LE": [
                ("Fcst", "Sky", "SCALAR", 0, 6, 50, ["AboveElev"]),
                ("Fcst", "Sky", "SCALAR", 0, 6, 87, ["BelowElev"]),
                ("Fcst", "Sky", "SCALAR", 6, 12, 50, ["AboveElev"]),
                ("Fcst", "Sky", "SCALAR", 6, 12, 87, ["BelowElev"]),
                ],
           },
       },
    }

TestGenerationDictionary = {

    "Precip": {
       "element": "Wx",
       "cases": {
           "null": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["BelowElev"]),
                ],
           "noLE": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12,"Chc:RW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12,"Chc:RW:-:<NoVis>:", ["BelowElev"]),
                ],
           "LE": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "Lkly:RW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Lkly:RW:-:<NoVis>:", ["BelowElev"]),
                ],
           "LE1": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "Lkly:RW:-:<NoVis>:", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["BelowElev"]),
                ],
           "LE2": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:RW:-:<NoVis>:", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Lkly:RW:-:<NoVis>:", ["BelowElev"]),
                ],
           },
       },
       
    "NonPrecip": {
       "element": "Wx",
       "cases": {
           "null": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["BelowElev"]),
                ],
           "noLE": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "Patchy:F:<NoInten>:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "Patchy:F:<NoInten>:<NoVis>:", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Patchy:F:<NoInten>:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Patchy:F:<NoInten>:<NoVis>:", ["BelowElev"]),
                ],
           "LE": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "Patchy:F:<NoInten>:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "Areas:F:<NoInten>:<NoVis>:", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Patchy:F:<NoInten>:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Areas:F:<NoInten>:<NoVis>:", ["BelowElev"]),
                ],
           "LE1": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "Patchy:F:<NoInten>:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "Areas:F:<NoInten>:<NoVis>:", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Patchy:F:<NoInten>:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Patchy:F:<NoInten>:<NoVis>:", ["BelowElev"]),
                ],
           "LE2": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "Patchy:F:<NoInten>:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "Patchy:F:<NoInten>:<NoVis>:", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Patchy:F:<NoInten>:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Areas:F:<NoInten>:<NoVis>:", ["BelowElev"]),
                ],
           },
       },

    "Consolidation": {
       "element": "Wx",
       "cases": {
           "null": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["BelowElev"]),
                ],
           "noLE": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "SChc:SW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "SChc:SW:-:<NoVis>:", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12,"NoWx", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12,"NoWx", ["BelowElev"]),
                ],
           "LE": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "SChc:SW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "Lkly:SW:-:<NoVis>:", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["BelowElev"]),
                ],
           "LE1": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "SChc:SW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "Lkly:SW:-:<NoVis>:", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "NoWx", ["BelowElev"]),
                ],
           "LE2": [
                ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 0, 6, "NoWx", ["BelowElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "SChc:SW:-:<NoVis>:", ["AboveElev"]),
                ("Fcst", "Wx", "WEATHER", 6, 12, "Lkly:SW:-:<NoVis>:", ["BelowElev"]),
                ],
           },
       },

    "Sky": {
       "element": "Sky",
       "cases": {
           "noLE": [
                ("Fcst", "Sky", "SCALAR", 0, 6, 50, ["AboveElev"]),
                ("Fcst", "Sky", "SCALAR", 0, 6, 50, ["BelowElev"]),
                ("Fcst", "Sky", "SCALAR", 6, 12, 50, ["AboveElev"]),
                ("Fcst", "Sky", "SCALAR", 6, 12, 50, ["BelowElev"]),
                ],
           "LE": [
                ("Fcst", "Sky", "SCALAR", 0, 6, 50, ["AboveElev"]),
                ("Fcst", "Sky", "SCALAR", 0, 6, 87, ["BelowElev"]),
                ("Fcst", "Sky", "SCALAR", 6, 12, 50, ["AboveElev"]),
                ("Fcst", "Sky", "SCALAR", 6, 12, 87, ["BelowElev"]),
                ],
           "LE1": [
                ("Fcst", "Sky", "SCALAR", 0, 6, 50, ["AboveElev"]),
                ("Fcst", "Sky", "SCALAR", 0, 6, 87, ["BelowElev"]),
                ("Fcst", "Sky", "SCALAR", 6, 12, 50, ["AboveElev"]),
                ("Fcst", "Sky", "SCALAR", 6, 12, 50, ["BelowElev"]),
                ],
           "LE2": [
                ("Fcst", "Sky", "SCALAR", 0, 6, 50, ["AboveElev"]),
                ("Fcst", "Sky", "SCALAR", 0, 6, 50, ["BelowElev"]),
                ("Fcst", "Sky", "SCALAR", 6, 12, 50, ["AboveElev"]),
                ("Fcst", "Sky", "SCALAR", 6, 12, 87, ["BelowElev"]),
                ],
           },
       },

    "PoP": {
       "element": "PoP",
       "cases": {
           "noLE": [
                ("Fcst", "PoP", "SCALAR", 0, 6, 40, ["AboveElev"]),
                ("Fcst", "PoP", "SCALAR", 0, 6, 40, ["BelowElev"]),
                ("Fcst", "PoP", "SCALAR", 6, 12, 40, ["AboveElev"]),
                ("Fcst", "PoP", "SCALAR", 6, 12, 40, ["BelowElev"]),
                ],
           "LE": [
                ("Fcst", "PoP", "SCALAR", 0, 6, 40, ["AboveElev"]),
                ("Fcst", "PoP", "SCALAR", 0, 6, 80, ["BelowElev"]),
                ("Fcst", "PoP", "SCALAR", 6, 12, 40, ["AboveElev"]),
                ("Fcst", "PoP", "SCALAR", 6, 12, 80, ["BelowElev"]),
                ],
           "LE1": [
                ("Fcst", "PoP", "SCALAR", 0, 6, 40, ["AboveElev"]),
                ("Fcst", "PoP", "SCALAR", 0, 6, 80, ["BelowElev"]),
                ("Fcst", "PoP", "SCALAR", 6, 12, 40, ["AboveElev"]),
                ("Fcst", "PoP", "SCALAR", 6, 12, 40, ["BelowElev"]),
                ],
           "LE2": [
                ("Fcst", "PoP", "SCALAR", 0, 6, 40, ["AboveElev"]),
                ("Fcst", "PoP", "SCALAR", 0, 6, 40, ["BelowElev"]),
                ("Fcst", "PoP", "SCALAR", 6, 12, 40, ["AboveElev"]),
                ("Fcst", "PoP", "SCALAR", 6, 12, 80, ["BelowElev"]),
                ],
           },
       },

    }


periodVer1 = """Definition["Period_1_version"] = 1"""
periodVer2 = """Definition["Period_1_version"] = 2"""

ExtraVariables = [("periodVer1", periodVer1), ("periodVer2",periodVer2)]

TestScriptCall = """

import TestScript
def testScript(self, dataMgr):
    defaults = {
        "cmdLineVars" :"{('Product Issuance', 'productIssuance'): 'Morning', ('Issuance Type', 'issuanceType'): 'ROUTINE', ('Issued By', 'issuedBy'): None}",
        "productType": "Phrase_Test_Local",
        "fileChanges" : [
           ("Phrase_Test_Local", "TextUtility", "replace", (periodVer1, periodVer2), "undo")
           ],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)

"""
