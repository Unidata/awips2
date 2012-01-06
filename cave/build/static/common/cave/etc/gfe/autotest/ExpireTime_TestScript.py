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
# Expire Time Test Case
#
# Author:
# ----------------------------------------------------------------------------

def1 = """#Definition["state_IDs"] = ["ST"]"""
def2 = """Definition["state_IDs"] = ["FL"]"""

pfm1 = """Definition["defaultEditAreas"] = [
  ('FLZ050','FLZ050\\nGFE TEST\\n35.00N  90.00W\\n35'),
  ]
"""



scripts = [
    {
    "commentary": "Clear out all Hazards Table and Grids.",
    "name": "Expire_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

    {
    "name":"ExpireAFM_am", 
    "productType":"AFM", 
    "commentary": "Checking product expire time for AFM, with Morning issuance.",
    "comboFlag": 1, 
    "combinations": [(["FLZ050"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",

    "checkStrings": [
      "FOUS52 KTBW 010800",
      "AFMTBW",
      "AREA FORECAST MATRICES",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ050-012100-",
      "PINELLAS-",
      "300 AM EST MON FEB 1 2010",
       ],
    },

    {
    "name":"ExpireAFM_pm", 
    "productType":"AFM", 
    "commentary": "Checking product expire time for AFM, with Afternoon issuance.",
    "comboFlag": 1, 
    "combinations": [(["FLZ050"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FOUS52 KTBW 010800",
      "AFMTBW",
      "AREA FORECAST MATRICES",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ050-020900-",
      "PINELLAS-",
      "300 AM EST MON FEB 1 2010",
       ],
    },

    {
    "name":"ExpireCWF_Morning", 
    "productType":"CWF", 
    "commentary": "Checking product expire time for CWF, with Morning issuance.",
    "comboFlag": 1, 
    "combinations": [(["GMZ870"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FZUS52 KTBW 010800",
      "CWFTBW",
      "COASTAL WATERS FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "GMZ800-012100-",
      "300 AM EST MON FEB 1 2010",
      "$$",
      "GMZ870-012100-",
      "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM-",
      "300 AM EST MON FEB 1 2010",
      ],
    },

    {
    "name":"ExpireCWF_Morning Update", 
    "productType":"CWF", 
    "commentary": "Checking product expire time for CWF, with Morning Update AM issuance.",
    "comboFlag": 1, 
    "combinations": [(["GMZ870"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FZUS52 KTBW 010800",
      "CWFTBW",
      "COASTAL WATERS FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "GMZ800-012100-",
      "300 AM EST MON FEB 1 2010",
      "SYNOPSIS FOR BONITA BEACH TO SUWANNEE RIVER FL OUT 60 NM",
      "$$",
      "GMZ870-012100-",
      "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM-",
      "300 AM EST MON FEB 1 2010",
      "$$",
       ],
    },

    {
    "name":"ExpireCWF_AfternoonUpdate", 
    "productType":"CWF", 
    "commentary": "Checking product expire time for CWF, with Afternoon Update issuance.",
    "comboFlag": 1, 
    "combinations": [(["GMZ870"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FZUS52 KTBW 010800",
      "CWFTBW",
      "COASTAL WATERS FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "GMZ800-012100-",
      "300 AM EST MON FEB 1 2010",
      "SYNOPSIS FOR BONITA BEACH TO SUWANNEE RIVER FL OUT 60 NM",
      "$$",
      "GMZ870-012100-",
      "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM-",
      "300 AM EST MON FEB 1 2010",
      "$$",
       ],
    },

    {
    "name":"ExpireCWF_EveningUpdate", 
    "productType":"CWF", 
    "commentary": "Checking product expire time for CWF, with Evening Update issuance.",
    "comboFlag": 1, 
    "combinations": [(["GMZ870"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Evening Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FZUS52 KTBW 010800",
      "CWFTBW ",
      "COASTAL WATERS FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "GMZ800-012100-",
      "300 AM EST MON FEB 1 2010",
      "SYNOPSIS FOR BONITA BEACH TO SUWANNEE RIVER FL OUT 60 NM",
      "$$",
      "GMZ870-012100-",
      "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM- ",
      "300 AM EST MON FEB 1 2010",
      "$$",
      ],
    },

    {
    "name":"ExpireCWFPac_Morning", 
    "productType":"CWF_Pacific", 
    "commentary": "Checking product expire time for CWF_Pacific, with Morning issuance.",
    "comboFlag": 1, 
    "combinations": [(["GMZ870"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FZUS52 KTBW 010800",
      "CWFTBW",
      "COASTAL WATERS FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "GMZ800-012100-",
      "300 AM EST MON FEB 1 2010",
      "$$",
      "GMZ870-012100-",
      "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM-",
      "300 AM EST MON FEB 1 2010",
       ],
    },

    {
    "name":"ExpireCWFPac_MorningUpdate", 
    "productType":"CWF_Pacific", 
    "commentary": "Checking product expire time for CWF_Pacific, with Morning Update issuance.",
    "comboFlag": 1, 
    "combinations": [(["GMZ870"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FZUS52 KTBW 010800",
      "CWFTBW",
      "COASTAL WATERS FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "GMZ800-012100-",
      "300 AM EST MON FEB 1 2010",
      "SYNOPSIS FOR BONITA BEACH TO SUWANNEE RIVER FL OUT 60 NM",
      "$$",
      "GMZ870-012100-",
      "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM-",
      "300 AM EST MON FEB 1 2010",
      "$$",
      ],
    },

    {
    "name":"ExpireCWFPac_AfternoonUpdate", 
    "productType":"CWF_Pacific", 
    "commentary": "Checking product expire time for CWF_Pacific, with Afternoon Update issuance.",
    "comboFlag": 1, 
    "combinations": [(["GMZ870"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FZUS52 KTBW 010800",
      "CWFTBW",
      "COASTAL WATERS FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "GMZ800-012100-",
      "300 AM EST MON FEB 1 2010",
      "SYNOPSIS FOR BONITA BEACH TO SUWANNEE RIVER FL OUT 60 NM",
      "$$",
      "GMZ870-012100-",
      "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM-",
      "300 AM EST MON FEB 1 2010",
      "$$",
      ],
    },

    {
    "name":"ExpireCWFPac_EveningUpdate", 
    "productType":"CWF_Pacific", 
    "commentary": "Checking product expire time for CWF_Pacific, with Evening Update issuance.",
    "comboFlag": 1, 
    "combinations": [(["GMZ870"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Evening Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FZUS52 KTBW 010800",
      "CWFTBW ",
      "COASTAL WATERS FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "GMZ800-012100-",
      "300 AM EST MON FEB 1 2010",
      "SYNOPSIS FOR BONITA BEACH TO SUWANNEE RIVER FL OUT 60 NM",
      "$$",
      "GMZ870-012100-",
      "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM- ",
      "300 AM EST MON FEB 1 2010",
      "$$",
       ],
    },

    {
    "name":"ExpireFWF_am",
    "productType":"FWF", 
    "commentary": "Checking product expire time for FWF, with Morning issuance.",
    "comboFlag": 1, 
    "combinations": [(["FLZ050"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FNUS52 KTBW 010800",
      "FWFTBW",
      "FIRE WEATHER PLANNING FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ050-012100-",
      "PINELLAS-",
       ],
    },

    {
    "name":"ExpireFWF_amU",
    "productType":"FWF", 
    "commentary": "Checking product expire time for FWF, with Morning Update issuance.",
    "comboFlag": 1, 
    "combinations": [(["FLZ050"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FNUS52 KTBW 010800",
      "FWFTBW",
      "FIRE WEATHER PLANNING FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ050-012100-",
      "PINELLAS-",
      ],
    },

    {
    "name":"ExpireFWF_pmU",
    "productType":"FWF", 
    "commentary": "Checking product expire time for FWF, with Afternoon Update issuance.",
    "comboFlag": 1, 
    "combinations": [(["FLZ050"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FNUS52 KTBW 010800",
      "FWFTBW",
      "FIRE WEATHER PLANNING FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ050-012100-",
      "PINELLAS-",
      ],
    },

    {
    "name":"ExpireFWF_pm",
    "productType":"FWF", 
    "commentary": "Checking product expire time for FWF, with Afternoon issuance.",
    "comboFlag": 1, 
    "combinations": [(["FLZ050"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FNUS52 KTBW 010800",
      "FWFTBW",
      "FIRE WEATHER PLANNING FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ050-020900-",
      "PINELLAS-",
       ],
    },

    {
    "name":"ExpireFWF_eU",
    "productType":"FWF", 
    "commentary": "Checking product expire time for FWF, with Evening Update issuance.",
    "comboFlag": 1, 
    "combinations": [(["FLZ050"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Evening Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FNUS52 KTBW 010800",
      "FWFTBW",
      "FIRE WEATHER PLANNING FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ050-020900-",
      "PINELLAS-",
        ],
    },

    {
    "name":"ExpireFWF_emu",
    "productType":"FWF", 
    "commentary": "Checking product expire time for FWF, with Early Morning Update issuance.",
    "comboFlag": 1, 
    "combinations": [(["FLZ050"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Early Morning Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FNUS52 KTBW 010800",
      "FWFTBW",
      "FIRE WEATHER PLANNING FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ050-010900-",
      "PINELLAS-",
      ],
    },

    {
    "name":"ExpireFWFTab_am",
    "productType":"FWF", 
    "commentary": "Checking product expire time for FWF, with Morning issuance.",
    "comboFlag": 1, 
    "combinations": [(["FLZ050"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FNUS52 KTBW 010800",
      "FWFTBW",
      "FIRE WEATHER PLANNING FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      ".DISCUSSION...",
      "FLZ050-012100-",
      "PINELLAS-",
      "300 AM EST MON FEB 1 2010",
       ],
    },

    {
    "name":"ExpireFWFTab_pm",
    "productType":"FWF", 
    "commentary": "Checking product expire time for FWF, with Afternoon issuance.",
    "comboFlag": 1, 
    "combinations": [(["FLZ050"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FNUS52 KTBW 010800",
      "FWFTBW",
      "FIRE WEATHER PLANNING FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      ".DISCUSSION...",
      "FLZ050-020900-",
      "PINELLAS-",
      "300 AM EST MON FEB 1 2010",
       ],

    },

    {
    "name":"ExpireGLF_4am",
    "productType":"GLF", 
    "commentary": "Checking product expire time for GLF, with 400 AM issuance.",
    "comboFlag": 0, 
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '400 AM', ('Groupings', 'groupings'): 'West 1/2:East 1/2'}",
    "checkStrings": [
      "UFUS42 KTBW 010800",
      "GLFABC",
      "LSZ260-012100-",
      "OPEN LAKES FORECAST FOR STATENAME",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "LSZ261-012100-",
       ],
    },

    {
    "name":"ExpireGLF_10am",
    "productType":"GLF", 
    "commentary": "Checking product expire time for GLF, with 1000 AM issuance.",
    "comboFlag": 0, 
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '1000 AM', ('Groupings', 'groupings'): 'West 1/2:East 1/2'}",
    "checkStrings": [
      "UFUS42 KTBW 010800",
      "GLFABC",
      "LSZ260-012100-",
      "OPEN LAKES FORECAST FOR STATENAME",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "LSZ261-012100-",
       ],
    },

    {
    "name":"ExpireGLF_4pm",
    "productType":"GLF", 
    "commentary": "Checking product expire time for GLF, with 400 PM issuance.",
    "comboFlag": 0, 
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '400 PM', ('Groupings', 'groupings'): 'West 1/2:East 1/2'}",
    "checkStrings": [
      "UFUS42 KTBW 010800",
      "GLFABC",
      "LSZ260-020900-",
      "OPEN LAKES FORECAST FOR STATENAME",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "LSZ261-020900-",
       ],
    },

    {
    "name":"ExpireGLF_10pm",
    "productType":"GLF", 
    "commentary": "Checking product expire time for GLF, with 1000 PM issuance.",
    "comboFlag": 0, 
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '1000 PM', ('Groupings', 'groupings'): 'West 1/2:East 1/2'}",
    "checkStrings": [
      "UFUS42 KTBW 010800",
      "GLFABC",
      "LSZ260-020900-",
      "OPEN LAKES FORECAST FOR STATENAME",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "LSZ261-020900-",
       ],
    },

    {
    "name":"ExpireNSH_430am",
    "productType":"NSH", 
    "commentary": "Checking product expire time for NSH, with 430 AM issuance.",
    "comboFlag": 1, 
    "combinations": [(["GMZ870"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '430 AM', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "UFUS42 KTBW 010800",
      "NSHABC",
      "NEARSHORE MARINE FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FOR WATERS WITHIN FIVE NAUTICAL MILES OF SHORE ON LAKE (NAME)",
      "GMZ870-011600-",
      "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM-",
      "300 AM EST MON FEB 1 2010",
        ],
    },

    {
    "name":"ExpireNSH_amU",
    "productType":"NSH", 
    "commentary": "Checking product expire time for NSH, with Morning Update issuance.",
    "comboFlag": 1, 
    "combinations": [(["GMZ870"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "UFUS42 KTBW 010800",
      "NSHABC",
      "NEARSHORE MARINE FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FOR WATERS WITHIN FIVE NAUTICAL MILES OF SHORE ON LAKE (NAME)",
      "GMZ870-012200-",
      "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM-",
      "300 AM EST MON FEB 1 2010",
        ],
    },

    {
    "name":"ExpireNSH_430pm",
    "productType":"NSH", 
    "commentary": "Checking product expire time for NSH, with 430 PM issuance.",
    "comboFlag": 1, 
    "combinations": [(["GMZ870"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '430 PM', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
     "UFUS42 KTBW 010800",
     "NSHABC",
     "NEARSHORE MARINE FORECAST FOR FLORIDA",
     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
     "300 AM EST MON FEB 1 2010",
     "FOR WATERS WITHIN FIVE NAUTICAL MILES OF SHORE ON LAKE (NAME)",
     "GMZ870-020400-",
     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM-",
     "300 AM EST MON FEB 1 2010",
        ],
    },

    {
    "name":"ExpireNSH_pmU",
    "productType":"NSH", 
    "commentary": "Checking product expire time for NSH, with Evening Update issuance.",
    "comboFlag": 1, 
    "combinations": [(["GMZ870"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Evening Update', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
     "UFUS42 KTBW 010800",
     "NSHABC",
     "NEARSHORE MARINE FORECAST FOR FLORIDA",
     "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
     "300 AM EST MON FEB 1 2010",
     "FOR WATERS WITHIN FIVE NAUTICAL MILES OF SHORE ON LAKE (NAME)",
     "GMZ870-021000-",
     "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM-",
     "300 AM EST MON FEB 1 2010",
        ],
    },

    {
    "name":"ExpireOFF_4am",
    "productType":"OFF",
    "commentary": "Checking product expire time for OFF, with 400 AM issuance.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '400 AM', ('Issued By', 'issuedBy'): None}",
    "comboFlag":  1,
    "combinations": [(["GMZ870"],"")],
    "checkStrings": [
      "UFUS42 KTBW 010800",
      "OFFABC",
      "OFFSHORE FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "400 AM EST MON FEB 1 2010",
      "-012100-",
      "400 AM EST MON FEB 1 2010",
      ".SYNOPSIS...",
      "GMZ870-012100-",
      "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM-",
      "400 AM EST MON FEB 1 2010",
       ],
    },

    {
    "name":"ExpireOFF_4pm",
    "productType":"OFF",
    "commentary": "Checking product expire time for OFF, with 400 PM issuance.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): '400 PM', ('Issued By', 'issuedBy'): None}",
    "comboFlag":  1,
    "combinations": [(["GMZ870"],"")],
    "checkStrings": [
      "UFUS42 KTBW 010800",
      "OFFABC",
      "OFFSHORE FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "400 PM EST MON FEB 1 2010",
      "-020900-",
      "400 PM EST MON FEB 1 2010",
      ".SYNOPSIS...",
      "GMZ870-020900-",
      "WATERS FROM TARPON SPRINGS TO SUWANNEE RIVER FL OUT 20 TO 60 NM-",
      "400 PM EST MON FEB 1 2010",
       ],
    },


    {
    "name":"ExpirePFM_am", 
    "productType":"PFM", 
    "commentary": "Checking product expire time for PFM, with Morning issuance.",
    "comboFlag": 0, 
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "internalStrip": 0,
    "fileChanges": [
          ("PFM_<site>_Definition", "TextUtility", "add", pfm1, "delete"),
          ],
    "checkStrings": [
      "FOUS52 KTBW 010800",
      "PFMTBW",
      "POINT FORECAST MATRICES",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ050-012100-",
      "GFE TEST",
      "35.00N  90.00W ELEV. 35 FT",
      "300 AM EST MON FEB 1 2010",
       ],
    },

    {
    "name":"ExpirePFM_pm", 
    "productType":"PFM", 
    "commentary": "Checking product expire time for PFM, with Afternoon issuance.",
    "comboFlag": 1, 
    "combinations": [(["FLZ050"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon', ('Issued By', 'issuedBy'): None }",
    "internalStrip": 0,
    "fileChanges": [
          ("PFM_<site>_Definition", "TextUtility", "add", pfm1, "delete"),
          ],
    "checkStrings": [
      "FOUS52 KTBW 010800",
      "PFMTBW",
      "POINT FORECAST MATRICES",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ050-020900-",
      "GFE TEST",
      "35.00N  90.00W ELEV. 35 FT",
      "300 AM EST MON FEB 1 2010",
        ],
    },


    {
    "name":"ExpireSFT_am", 
    "productType":"SFT", 
    "commentary": "Checking product expire time for SFT, with Morning issuance.",
    "comboFlag": 0, 
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FPUS62 KTBW 010800",
      "SFTTBW",
      "STZ000-012200-",
      "TABULAR STATE FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
       ],
    },

    {
    "name":"ExpireSFT_pm", 
    "productType":"SFT", 
    "commentary": "Checking product expire time for SFT, with Afternoon issuance.",
    "comboFlag": 1, 
    "combinations": [(["FLZ050"],"")],
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
      "FPUS62 KTBW 010800",
      "SFTTBW",
      "STZ000-021000-",
      "TABULAR STATE FORECAST FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      ],
    },

    {
    "name": "ExpireZFP_am",
    "productType":"ZFP", 
    "commentary": "Checking product expire time for ZFP, with Morning issuance.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "combinations": [(["FLZ039"],"")],
    "checkStrings": [
      "FPUS52 KTBW 010800",
      "ZFPTBW",
      "ZONE FORECAST PRODUCT FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ039-012100-",
      "LEVY-",
      "300 AM EST MON FEB 1 2010",
       ],
    },   

    {
    "name": "ExpireZFP_amP1st",
    "productType":"ZFP", 
    "commentary": "Checking product expire time for ZFP, with Morning with Pre-1st Period issuance.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning with Pre-1st Period', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "combinations": [(["FLZ039"],"")],
    "checkStrings": [
      "FPUS52 KTBW 010800",
      "ZFPTBW",
      "ZONE FORECAST PRODUCT FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ039-012100-",
      "LEVY-",
      "300 AM EST MON FEB 1 2010",
      ],
    },   

    {
    "name": "ExpireZFP_amU",
    "productType":"ZFP", 
    "commentary": "Checking product expire time for ZFP, with Morning Update issuance.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning Update', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "combinations": [(["FLZ039"],"")],
    "checkStrings": [
      "FPUS52 KTBW 010800",
      "ZFPTBW",
      "ZONE FORECAST PRODUCT FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ039-012100-",
      "LEVY-",
      "300 AM EST MON FEB 1 2010",
      ],
    },   

    {
    "name": "ExpireZFP_pmU",
    "productType":"ZFP", 
    "commentary": "Checking product expire time for ZFP, with Afternoon Update issuance.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon Update', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "combinations": [(["FLZ039"],"")],
    "checkStrings": [
      "FPUS52 KTBW 010800",
      "ZFPTBW",
      "ZONE FORECAST PRODUCT FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ039-012100-",
      "LEVY-",
      "300 AM EST MON FEB 1 2010",
      ],
    },   

    {
    "name": "ExpireZFP_pm",
    "productType":"ZFP", 
    "commentary": "Checking product expire time for ZFP, with Afternoon issuance.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "combinations": [(["FLZ039"],"")],
    "checkStrings": [
      "FPUS52 KTBW 010800",
      "ZFPTBW",
      "ZONE FORECAST PRODUCT FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ039-020900-",
      "LEVY-",
      "300 AM EST MON FEB 1 2010",
      ],
    },   

    {
    "name": "ExpireZFP_pmP1st",
    "productType":"ZFP", 
    "commentary": "Checking product expire time for ZFP, with Afternoon with Pre-1st Period issuance.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon with Pre-1st Period', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "combinations": [(["FLZ039"],"")],
    "checkStrings": [
      "FPUS52 KTBW 010800",
      "ZFPTBW",
      "ZONE FORECAST PRODUCT FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ039-020900-",
      "LEVY-",
      "300 AM EST MON FEB 1 2010",
       ],
    },   

    {
    "name": "ExpireZFP_eU",
    "productType":"ZFP", 
    "commentary": "Checking product expire time for ZFP, with Evening Update issuance.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Evening Update', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "combinations": [(["FLZ039"],"")],
    "checkStrings": [
      "FPUS52 KTBW 010800",
      "ZFPTBW",
      "ZONE FORECAST PRODUCT FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ039-020900-",
      "LEVY-",
      "300 AM EST MON FEB 1 2010",
        ],
    },   

    {
    "name": "ExpireZFP_emU",
    "productType":"ZFP", 
    "commentary": "Checking product expire time for ZFP, with Early Morning Update issuance.",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Early Morning Update', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "combinations": [(["FLZ039"],"")],
    "checkStrings": [
      "FPUS52 KTBW 010800",
      "ZFPTBW",
      "ZONE FORECAST PRODUCT FOR FLORIDA",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "300 AM EST MON FEB 1 2010",
      "FLZ039-010900-",
      "LEVY-",
      "300 AM EST MON FEB 1 2010",
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
        "drtTime": "20100201_0800",
        "database": "<site>_GRID__Fcst_00000000_0000",
        "createGrids": [],
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        "publishGrids": 0,
        "decodeVTEC": 0,
        "orderStrings": 1,
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




