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
# CCF tests
#
# Author: cheatwood
# ----------------------------------------------------------------------------

# Wx, Sky, MaxT, MinT, Wind are 24 hour grids
# PoP, SnowAmt are 12 hour grids
# All grids, except Sky
# Sky grids are included in each CCF test case

CCF_createGrids = [
        ("Fcst", "Wx", "WEATHER", 0, 24, "NoWx", "all"),
        ("Fcst", "Wx", "WEATHER", 0, 24, "Wide:R:--:<NoVis>:^Wide:S:-:<NoVis>:", ["area1"]),
        ("Fcst", "Wx", "WEATHER", 0, 24, "Wide:S:+:1/4SM:", ["area2"]),
        ("Fcst", "Wx", "WEATHER", 0, 24, "Wide:RW:--:<NoVis>:^Wide:T:<NoInten>:<NoVis>:", ["area3"]),
        ("Fcst", "Wx", "WEATHER", 24, 48, "NoWx", "all"),
        ("Fcst", "Wx", "WEATHER", 24, 48, "Chc:T:<NoInten>:<NoVis>:", ["area1"]),
        ("Fcst", "Wx", "WEATHER", 24, 48, "SChc:R:--:<NoVis>:", ["area2"]),
        ("Fcst", "Wx", "WEATHER", 24, 48, "SChc:ZR:--:<NoVis>:", ["area3"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 70, ["area1"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 43, ["area1"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 80, ["area2"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 47, ["area2"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 90, ["area3"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 49, ["area3"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 24", "MaxTEnd + 24", 73, ["area1"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 24", "MinTEnd + 24", 45, ["area1"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 24", "MaxTEnd + 24", 81, ["area2"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 24", "MinTEnd + 24", 48, ["area2"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 24", "MaxTEnd + 24", 92, ["area3"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 24", "MinTEnd + 24", 50, ["area3"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 48", "MaxTEnd + 48", 75, ["area1"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 48", "MaxTEnd + 48", 82, ["area2"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 48", "MaxTEnd + 48", 95, ["area3"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 48", "MinTEnd + 48", 47, ["area1"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 48", "MinTEnd + 48", 50, ["area2"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 48", "MinTEnd + 48", 52, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 0, 12, 45, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 0, 12, 45, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 0, 12, 45, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 12, 24, 45, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 12, 24, 45, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 12, 24, 45, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 24, 36, 50, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 24, 36, 60, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 24, 36, 70, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 36, 48, 50, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 36, 48, 60, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 36, 48, 70, ["area3"]),
        ("Fcst", "Wind", "VECTOR", 0, 24, (40, "SW"), "all"),
        ("Fcst", "Wind", "VECTOR", 24, 48, (35, "NW"), "all"),
        ("Fcst", "Wind", "VECTOR", 48, 72, (45, "W"), "all"),
        ("Fcst", "Wind", "VECTOR", 72, 96, (50, "NW"), "all"),
        ("Fcst", "Wind", "VECTOR", 96, 120, (45, "N"), "all"),
        ("Fcst", "Wind", "VECTOR", 120, 144, (40, "NW"), "all"),
        ("Fcst", "Wind", "VECTOR", 144, 168, (30, "W"), "all"),
        ("Fcst", "Wind", "VECTOR", 168, 192, (35, "W"), "all"),
        ("Fcst", "SnowAmt", "SCALAR", 0, 12, 0, "all"), 
        ("Fcst", "SnowAmt", "SCALAR", 12, 24, 6, "all"),
        ("Fcst", "SnowAmt", "SCALAR", 24, 36, 3, "all"),
        ("Fcst", "SnowAmt", "SCALAR", 36, 48, 1, "all"),
        ("Fcst", "Wx", "WEATHER", 48, 72, "NoWx", "all"),
        ("Fcst", "Wx", "WEATHER", 48, 72, "Iso:RW:--:<NoVis>:", ["area1"]),
        ("Fcst", "Wx", "WEATHER", 48, 72, "Areas:ZL:--:<NoVis>:", ["area2"]),
        ("Fcst", "Wx", "WEATHER", 48, 72, "Sct:SW:--:<NoVis>:", ["area3"]),
        ("Fcst", "Wx", "WEATHER", 72, 96, "NoWx", "all"),
        ("Fcst", "Wx", "WEATHER", 72, 96, "Def:BS:<NoInten>:<NoVis>:", ["area1"]),
        ("Fcst", "Wx", "WEATHER", 72, 96, "SChc:S:-:<NoVis>:", ["area2"]),
        ("Fcst", "Wx", "WEATHER", 72, 96, "Def:BS:<NoInten>:<NoVis>:", ["area3"]),
        ("Fcst", "Wx", "WEATHER", 96, 120, "NoWx", "all"),
        ("Fcst", "Wx", "WEATHER", 96, 120, "Iso:SW:-:<NoVis>:", ["area1"]),
        ("Fcst", "Wx", "WEATHER", 96, 120, "Areas:F:+:<NoVis>:", ["area2"]),
        ("Fcst", "Wx", "WEATHER", 96, 120, "Wide:R:--:<NoVis>:", ["area3"]), 
        ("Fcst", "Wx", "WEATHER", 120, 144, "NoWx", "all"),
        ("Fcst", "Wx", "WEATHER", 120, 144, "Wide:L:--:<NoVis>:", ["area1"]),
        ("Fcst", "Wx", "WEATHER", 120, 144, "Patchy:L:--:<NoVis>:", ["area2"]),
        ("Fcst", "Wx", "WEATHER", 120, 144, "Patchy:BD:<NoInten>:<NoVis>:", ["area3"]),
        ("Fcst", "Wx", "WEATHER", 144, 168, "NoWx", "all"),
        ("Fcst", "Wx", "WEATHER", 144, 168, "Wide:IP:--:<NoVis>:", ["area1"]),
        ("Fcst", "Wx", "WEATHER", 144, 168, "Def:H:<NoInten>:<NoVis>:", ["area2"]),
        ("Fcst", "Wx", "WEATHER", 144, 168, "Patchy:K:<NoInten>:<NoVis>:", ["area3"]),
        ("Fcst", "Wx", "WEATHER", 168, 192, "NoWx", "all"),
        ("Fcst", "Wx", "WEATHER", 168, 192, "Wide:R:--:<NoVis>:", ["area1"]),
        ("Fcst", "Wx", "WEATHER", 168, 192, "Sct:RW:-:<NoVis>:", ["area2"]),
        ("Fcst", "Wx", "WEATHER", 168, 192, "Chc:R:+:<NoVis>:", ["area3"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 72", "MinTEnd + 72", 47, ["area1"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 72", "MaxTEnd + 72", 77, ["area1"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 72", "MinTEnd + 72", 50, ["area2"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 72", "MaxTEnd + 72", 85, ["area2"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 72", "MinTEnd + 72", 52, ["area3"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 72", "MaxTEnd + 72", 96, ["area3"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 96", "MinTEnd + 96", 49, ["area1"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 96", "MaxTEnd + 96", 79, ["area1"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 96", "MinTEnd + 96", 51, ["area2"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 96", "MaxTEnd + 96", 86, ["area2"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 96", "MinTEnd + 96", 54, ["area3"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 96", "MaxTEnd + 96", 100, ["area3"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 120", "MinTEnd + 120", 49, ["area1"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 120", "MaxTEnd + 120", 81, ["area1"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 120", "MinTEnd + 120", 53, ["area2"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 120", "MaxTEnd + 120", 60, ["area2"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 120", "MinTEnd + 120", 55, ["area3"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 120", "MaxTEnd + 120", 103, ["area3"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 144", "MinTEnd + 144", 49, ["area1"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 144", "MaxTEnd + 144", 81, ["area1"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 144", "MinTEnd + 144", 50, ["area2"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 144", "MaxTEnd + 144", 80, ["area2"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 144", "MinTEnd + 144", 55, ["area3"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 144", "MaxTEnd + 144", 96, ["area3"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 168", "MaxTEnd + 168", 83, ["area1"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 168", "MaxTEnd + 168", 85, ["area2"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 168", "MaxTEnd + 168", 100, ["area3"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 168", "MinTEnd + 168", 52, ["area1"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 168", "MinTEnd + 168", 54, ["area2"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 168", "MinTEnd + 168", 58, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 36, 48, 50, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 36, 48, 60, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 36, 48, 50, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 48, 60, 55, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 48, 60, 55, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 48, 60, 60, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 60, 72, 60, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 60, 72, 65, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 60, 72, 66, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 72, 84, 50, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 72, 84, 55, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 72, 84, 60, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 84, 96, 50, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 84, 96, 50, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 84, 96, 50, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 96, 108, 45, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 96, 108, 50, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 96, 108, 45, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 108, 120, 50, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 108, 120, 55, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 108, 120, 50, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 120, 132, 55, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 120, 132, 60, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 120, 132, 55, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 132, 144, 45, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 132, 144, 50, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 132, 144, 55, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 144, 156, 50, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 144, 156, 55, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 144, 156, 60, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 156, 168, 50, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 156, 168, 55, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 156, 168, 60, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 168, 180, 50, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 168, 180, 55, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 168, 180, 60, ["area3"]),
        ("Fcst", "PoP", "SCALAR", 180, 192, 50, ["area1"]),
        ("Fcst", "PoP", "SCALAR", 180, 192, 55, ["area2"]),
        ("Fcst", "PoP", "SCALAR", 180, 192, 60, ["area3"]),
        ]

CCF_deleteGrids = [
        ("Fcst", "PoP", "SFC", 0,280),
        ("Fcst", "MaxT", "SFC", 0,280),
        ("Fcst", "MinT", "SFC", 0,280),
        ("Fcst", "T", "SFC", 0,280),
        ("Fcst", "Td", "SFC", 0,280),
        ("Fcst", "WindChill", "SFC", 0,280),
        ("Fcst", "HeatIndex", "SFC", 0,280),
        ("Fcst", "Wind", "SFC", 0,280),
        ("Fcst", "Sky", "SFC", 0,280),
        ("Fcst", "WindGust", "SFC", 0,280),
        ("Fcst", "Wx", "SFC", 0,280),
        ("Fcst", "QPF", "SFC", 0,280),
        ("Fcst", "SnowAmt", "SFC", 0,280),
        ]

scripts = [

# Morning CCF test
# Wind dominates in the Wx grids
    {
    "commentary": """
    Morning CCF test
    Wind, N, overrides some Wx codes in the Wx grids
    """,
    "name":"CCF_1", 
    "productType":"CCF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Forecaster Number', 'forecasterNumber'): 99.0 }",
    "comboFlag": 1, 
    "checkStrings": [
        "CCFTBW",
        "AREA1 OT 070/043 073/045 075 99555 0102/0406/0103",
        "WQJLX 047/077 047/079 049/081 049/081 6665556655",
        "AREA2 PR 080/047 081/048 082 99566 0102/0406/0103",
        "ZSNLN 050/085 050/086 051/060 053/080 6776566666",
        "AREA3 TY 090/049 092/050 095 99577 0102/0406/0103",
        "JQRNN 052/096 052/100 054/103 055/096 6776556666",
        ],
    "createGrids": CCF_createGrids + [
    ("Fcst", "Sky", "SCALAR", 0, 24, 0, "all"),
    ("Fcst", "Sky", "SCALAR", 24, 48, 96, "all"),
    ("Fcst", "Sky", "SCALAR", 48, 72, 70, "all"),
    ("Fcst", "Sky", "SCALAR", 72, 96, 32, "all"),
    ("Fcst", "Sky", "SCALAR", 96, 120, 0, "all"),
    ("Fcst", "Sky", "SCALAR", 120, 144, 96, "all"),
    ("Fcst", "Sky", "SCALAR", 144, 168, 70, "all"),
    ("Fcst", "Sky", "SCALAR", 168, 192, 32, "all"),
    ],
    },

# Afternoon CCF test
# Wind dominates in Wx grids
    {
    "commentary": """
    Afternoon CCF test
    Wind, N, overrides some Wx codes in the Wx grids
    """,
    "name":"CCF_2", 
    "productType":"CCF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon', ('Issued By', 'issuedBy'): None, ('Forecaster Number', 'forecasterNumber'): 99.0 }",
    "comboFlag": 1, 
    "checkStrings": [
        "CCFTBW",
        "AREA1 TW 043/073 045/075 047 99556 0406/0103/0001",
        "QJLXR 077/047 079/049 081/049 081/049 083 66555665555",
        "AREA2 RZ 047/081 048/082 050 99666 0406/0103/0001",
        "SNLNW 085/050 086/051 060/053 080/050 085 77656666666",
        "AREA3 YJ 049/092 050/095 052 99776 0406/0103/0001",
        "QRNNR 096/052 100/054 103/055 096/055 100 77655666666",
        ],
    "createGrids": CCF_createGrids + [
    ("Fcst", "Sky", "SCALAR", 0, 24, 0, "all"),
    ("Fcst", "Sky", "SCALAR", 24, 48, 96, "all"),
    ("Fcst", "Sky", "SCALAR", 48, 72, 70, "all"),
    ("Fcst", "Sky", "SCALAR", 72, 96, 32, "all"),
    ("Fcst", "Sky", "SCALAR", 96, 120, 0, "all"),
    ("Fcst", "Sky", "SCALAR", 120, 144, 96, "all"),
    ("Fcst", "Sky", "SCALAR", 144, 168, 70, "all"),
    ("Fcst", "Sky", "SCALAR", 168, 192, 32, "all"),
    ],
    },

# Morning CCF test
# Changed MaxT to include a "VRYHOT = G" and "VRYCOLD = I" Wx code
# Changed Winds to "0" so they would not dominate over the other Wx grids
# Wind "0, 24" was kept at "35" to keep a "BLZZRD = P" Wx code
    {
    "commentary": """
    Morning CCF test
    MaxT is set at 120 and 144 hours to include a VRYHOT = G Wx code for AREA3 and a VRYCOLD = I Wx code for AREA2
    The Wind grids are set to 0, except at 0-24 hours, to not override any Wx codes 
    Wind, 0-24 hours, is kept at 35 to output a BLZZRD = P Wx code
    """,
    "name":"CCF_3", 
    "productType":"CCF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Forecaster Number', 'forecasterNumber'): 99.0 }",
    "comboFlag": 1, 
    "checkStrings": [
        "CCFTBW",
        "AREA1 OT 070/043 073/045 075 99555 0102/0406/0103",
        "WQJLX 047/077 047/079 049/081 049/081 6665556655",
        "AREA2 PR 080/047 081/048 082 99566 0102/0406/0103",
        "ZSFLI 050/085 050/086 051/060 053/018 6776566666",
        "AREA3 TY 090/049 092/050 095 99577 0102/0406/0103",
        "JQRGK 052/096 052/100 054/106 055/096 6776556666",
        ],
    "createGrids": CCF_createGrids + [
    ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 120", "MaxTEnd + 120", 106, ["area3"]),
    ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 144", "MaxTEnd + 144", 18, ["area2"]),
    ("Fcst", "MinT", "SCALAR", "MinTBegin + 144", "MinTEnd + 144", 13, ["area2"]),
    ("Fcst", "Wind", "VECTOR", 0, 24, (35, "SW"), "all"),
    ("Fcst", "Wind", "VECTOR", 24, 48, (0, "NW"), "all"),
    ("Fcst", "Wind", "VECTOR", 48, 72, (0, "W"), "all"),
    ("Fcst", "Wind", "VECTOR", 72, 96, (0, "NW"), "all"),
    ("Fcst", "Wind", "VECTOR", 96, 120, (0, "N"), "all"),
    ("Fcst", "Wind", "VECTOR", 120, 144, (0, "NW"), "all"),
    ("Fcst", "Wind", "VECTOR", 144, 168, (0, "W"), "all"),
    ("Fcst", "Wind", "VECTOR", 168, 192, (0, "W"), "all"),
    ("Fcst", "Sky", "SCALAR", 0, 24, 0, "all"),
    ("Fcst", "Sky", "SCALAR", 24, 48, 96, "all"),
    ("Fcst", "Sky", "SCALAR", 48, 72, 70, "all"),
    ("Fcst", "Sky", "SCALAR", 72, 96, 32, "all"),
    ("Fcst", "Sky", "SCALAR", 96, 120, 0, "all"),
    ("Fcst", "Sky", "SCALAR", 120, 144, 96, "all"),
    ("Fcst", "Sky", "SCALAR", 144, 168, 70, "all"),
    ("Fcst", "Sky", "SCALAR", 168, 192, 32, "all"),
    ],
    },

# Afternoon CCF test
# Changed Winds to "0" so they would not be dominate over the other Wx grids

    {
    "commentary": """
    Afternoon CCF test
    Wind grids are set to 0 so they will not dominate over the other Wx grids
    """,
    "name":"CCF_4", 
    "productType":"CCF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon', ('Issued By', 'issuedBy'): None, ('Forecaster Number', 'forecasterNumber'): 99.0 }",
    "comboFlag": 1, 
    "checkStrings": [
        "CCFTBW",
        "AREA1 TE 043/073 045/075 047 99555 0406/0103/0001",
        "QJLXR 077/047 079/049 081/049 081/049 083 05555665555",
        "AREA2 RZ 047/081 048/082 050 99666 0406/0103/0001",
        "SFLHW 085/050 086/051 060/053 080/050 085 06656666666",
        "AREA3 YM 049/092 050/095 052 99775 0406/0103/0001",
        "QRDKR 096/052 100/054 103/055 096/055 100 16655666666",
        ],
    "createGrids": CCF_createGrids + [
    ("Fcst", "Wind", "VECTOR", 0, 24, (35, "SW"), "all"),
    ("Fcst", "Wind", "VECTOR", 24, 48, (0, "NW"), "all"),
    ("Fcst", "Wind", "VECTOR", 48, 72, (0, "W"), "all"),
    ("Fcst", "Wind", "VECTOR", 72, 96, (0, "NW"), "all"),
    ("Fcst", "Wind", "VECTOR", 96, 120, (0, "N"), "all"),
    ("Fcst", "Wind", "VECTOR", 120, 144, (0, "NW"), "all"),
    ("Fcst", "Wind", "VECTOR", 144, 168, (0, "W"), "all"),
    ("Fcst", "Wind", "VECTOR", 168, 192, (0, "W"), "all"),
    ("Fcst", "Sky", "SCALAR", 0, 24, 0, "all"),
    ("Fcst", "Sky", "SCALAR", 24, 48, 96, "all"),
    ("Fcst", "Sky", "SCALAR", 48, 72, 70, "all"),
    ("Fcst", "Sky", "SCALAR", 72, 96, 32, "all"),
    ("Fcst", "Sky", "SCALAR", 96, 120, 0, "all"),
    ("Fcst", "Sky", "SCALAR", 120, 144, 96, "all"),
    ("Fcst", "Sky", "SCALAR", 144, 168, 70, "all"),
    ("Fcst", "Sky", "SCALAR", 168, 192, 32, "all"),
    ("Fcst", "PoP", "SCALAR", 48, 72, 10, ["area3"]),
    ],
    },

# Afternoon CCF test
# Changed Wx grids to "NoWx"
# Changed all Wind grids to "0"
# "Sky" grids are now visible 
    {
    "commentary": """
    Afternoon CCF test
    Wx grids are set to NoWx
    Wind grids are set to 0
    Sky Wx code is visible
    """,
    "name":"CCF_5", 
    "productType":"CCF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon', ('Issued By', 'issuedBy'): None, ('Forecaster Number', 'forecasterNumber'): 99.0 }",
    "comboFlag": 1, 
    "checkStrings": [
        "CCFTBW",
        "AREA1 CE 043/073 045/075 047 99556 0406/0103/0001",
        "BUCEB 077/047 079/049 081/049 081/049 083 66555665555",
        "AREA2 CE 047/081 048/082 050 99666 0406/0103/0001",
        "BUCEB 085/050 086/051 060/053 080/050 085 77656666666",
        "AREA3 CE 049/092 050/095 052 99776 0406/0103/0001",
        "BUCEB 096/052 100/054 103/055 096/055 100 77655666666",
        ],
    "createGrids": CCF_createGrids + [
    ("Fcst", "Wx", "WEATHER", 0, 24, "NoWx", "all"),
    ("Fcst", "Wx", "WEATHER", 24, 48, "NoWx", "all"),
    ("Fcst", "Wx", "WEATHER", 48, 72, "NoWx", "all"),
    ("Fcst", "Wx", "WEATHER", 72, 96, "NoWx", "all"),
    ("Fcst", "Wx", "WEATHER", 96, 120, "NoWx", "all"),
    ("Fcst", "Wx", "WEATHER", 120, 144, "NoWx", "all"),
    ("Fcst", "Wx", "WEATHER", 144, 168, "NoWx", "all"),
    ("Fcst", "Wx", "WEATHER", 168, 192, "NoWx", "all"),
    ("Fcst", "Wind", "VECTOR", 0, 24, (0, "SW"), "all"),
    ("Fcst", "Wind", "VECTOR", 24, 48, (0, "NW"), "all"),
    ("Fcst", "Wind", "VECTOR", 48, 72, (0, "W"), "all"),
    ("Fcst", "Wind", "VECTOR", 72, 96, (0, "NW"), "all"),
    ("Fcst", "Wind", "VECTOR", 96, 120, (0, "N"), "all"),
    ("Fcst", "Wind", "VECTOR", 120, 144, (0, "NW"), "all"),
    ("Fcst", "Wind", "VECTOR", 144, 168, (0, "W"), "all"),
    ("Fcst", "Wind", "VECTOR", 168, 192, (0, "W"), "all"),
    ("Fcst", "Sky", "SCALAR", 0, 24, 0, "all"),
    ("Fcst", "Sky", "SCALAR", 24, 48, 96, "all"),
    ("Fcst", "Sky", "SCALAR", 48, 72, 70, "all"),
    ("Fcst", "Sky", "SCALAR", 72, 96, 32, "all"),
    ("Fcst", "Sky", "SCALAR", 96, 120, 0, "all"),
    ("Fcst", "Sky", "SCALAR", 120, 144, 96, "all"),
    ("Fcst", "Sky", "SCALAR", 144, 168, 70, "all"),
    ("Fcst", "Sky", "SCALAR", 168, 192, 32, "all"),
    ],
    },

# Clean up for CCF6 test
# Need to clear out "Sky" grids for test CCF6 below
    {    
    "name":"CCFCleanUpforCCF6",
    "commentary": "Clean out grids",
    "productType": None,
    "deleteGrids": [
        ("Fcst", "Sky", "SFC", 0, 192),
        ],
    "fileChanges": [],
    },

# Must run CCF6 and CCFCleanUpforCCF6 together in order to clean out the Sky codes to get the "FAIR = A" Sky code
# Afternoon CCF test
# Changed Wx grids to "NoWx"
# Changed all Wind grids to "0"
# "Sky" grids are absent in order to get a "FAIR = A" Wx code  
    {
    "commentary": """
    Afternoon CCF test
    Wx grids are set to NoWx
    Wind grids are set to 0
    Sky grids are absent in order to get a FAIR = A Wx code
    """,
    "name":"CCF6", 
    "productType":"CCF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon', ('Issued By', 'issuedBy'): None, ('Forecaster Number', 'forecasterNumber'): 99.0 }",
    "comboFlag": 1, 
    "checkStrings": [
        "CCFTBW",
        "AREA1 AA 043/073 045/075 047 99556 0406/0103/0001",
        "AAAAA 077/047 079/049 081/049 081/049 083 66555665555",
        "AREA2 AA 047/081 048/082 050 99666 0406/0103/0001",
        "AAAAA 085/050 086/051 060/053 080/050 085 77656666666",
        "AREA3 AA 049/092 050/095 052 99776 0406/0103/0001",
        "AAAAA 096/052 100/054 103/055 096/055 100 77655666666",
        ],
    "createGrids": CCF_createGrids + [
    ("Fcst", "Wx", "WEATHER", 0, 24, "NoWx", "all"),
    ("Fcst", "Wx", "WEATHER", 24, 48, "NoWx", "all"),
    ("Fcst", "Wx", "WEATHER", 48, 72, "NoWx", "all"),
    ("Fcst", "Wx", "WEATHER", 72, 96, "NoWx", "all"),
    ("Fcst", "Wx", "WEATHER", 96, 120, "NoWx", "all"),
    ("Fcst", "Wx", "WEATHER", 120, 144, "NoWx", "all"),
    ("Fcst", "Wx", "WEATHER", 144, 168, "NoWx", "all"),
    ("Fcst", "Wx", "WEATHER", 168, 192, "NoWx", "all"),
    ("Fcst", "Wind", "VECTOR", 0, 24, (0, "SW"), "all"),
    ("Fcst", "Wind", "VECTOR", 24, 48, (0, "NW"), "all"),
    ("Fcst", "Wind", "VECTOR", 48, 72, (0, "W"), "all"),
    ("Fcst", "Wind", "VECTOR", 72, 96, (0, "NW"), "all"),
    ("Fcst", "Wind", "VECTOR", 96, 120, (0, "N"), "all"),
    ("Fcst", "Wind", "VECTOR", 120, 144, (0, "NW"), "all"),
    ("Fcst", "Wind", "VECTOR", 144, 168, (0, "W"), "all"),
    ("Fcst", "Wind", "VECTOR", 168, 192, (0, "W"), "all"),
    ],
    },  

    ]


defaultEditAreas = """Definition["defaultEditAreas"] = [
    ("area1", "AREA1"),
    ("area2", "AREA2"),
    ("area3", "AREA3"),
]

"""

import TestScript
import AbsTime
def testScript(self, dataMgr, level="Site"):
    today = self.getTimeRange("Today").startTime()
    today_10Z = AbsTime.absTimeYMD(today.year, today.month, today.day,
                                10, 0)
    defaults = {
        "internalStrip": 0,
        "deleteGrids": CCF_deleteGrids,
        "gridsStartTime": today_10Z,
        "fileChanges": [
           ("CCF_<site>_Definition", "TextUtility", "add", defaultEditAreas, "undo"),
           ],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults, level=level)


