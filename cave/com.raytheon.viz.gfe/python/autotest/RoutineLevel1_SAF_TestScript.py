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
# SAF tests
#
# Author: hansen
# ----------------------------------------------------------------------------

import TestScript

general_createGrids = [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 70, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 43, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "Td", "SCALAR", 6, 9, 30, "all"),  
       ("Fcst", "Wind", "VECTOR", 6, 9, (10, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 6, 9, 0, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 9, "Iso:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 9, 0, "all"),
       ("Fcst", "QPF", "SCALAR", 6, 9, .01, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 6, 9, .05, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 6, 9, 1, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 65, "all"),
       ("Fcst", "Td", "SCALAR", 9, 12, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 9, 12, (15, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 7, "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12,
        "Sct:T:<NoInten>:<NoVis>:^Num:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 9, 12, 0, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 9, 12, .01, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 9, 12, 0, "all"),
       ("Fcst", "T", "SCALAR", 12, 15, 60, "all"),
       ("Fcst", "Td", "SCALAR", 12, 15, 45, "all"),  
       ("Fcst", "Wind", "VECTOR", 12, 15, (15, "SE"), "all"),
       ("Fcst", "Sky", "SCALAR", 12, 15, 6, "all"),
       ("Fcst", "Wx", "WEATHER", 12, 15,
        "Wide:T:<NoInten>:<NoVis>:^Ocnl:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 12, 15, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 12, 15, 0, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 12, 15, .01, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 12, 15, 2, "all"),
       ("Fcst", "T", "SCALAR", 15, 18, 55, "all"),
       ("Fcst", "Td", "SCALAR", 15, 18, 50, "all"),  
       ("Fcst", "Wind", "VECTOR", 15, 18, (25, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 15, 18, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 15, 18,
        "SChc:T:<NoInten>:<NoVis>:^Chc:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 15, 18, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 15, 18, .05, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 15, 18, .1, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 15, 18, 1, "all"),
       ("Fcst", "T", "SCALAR", 18, 21, 50, "all"),
       ("Fcst", "Td", "SCALAR", 18, 21, 50, "all"),  
       ("Fcst", "Wind", "VECTOR", 18, 21, (30, "NE"), "all"),
       ("Fcst", "Sky", "SCALAR", 18, 21, 15, "all"),
       ("Fcst", "Wx", "WEATHER", 18, 21,
        "Lkly:T:<NoInten>:<NoVis>:^Def:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 18, 21, 25, "all"),
       ("Fcst", "QPF", "SCALAR", 18, 21, .08, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 18, 21, .2, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 18, 21, 1.5, "all"),
       ("Fcst", "T", "SCALAR", 21, 24, 50, "all"),
       ("Fcst", "Td", "SCALAR", 21, 24, 48, "all"),  
       ("Fcst", "Wind", "VECTOR", 21, 24, (25, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 21, 24, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 21, 24,
        "Frq:T:<NoInten>:<NoVis>:^Brf:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 21, 24, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 21, 24, .1, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 21, 24, .25, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 21, 24, 1.2, "all"),
       ("Fcst", "T", "SCALAR", 24, 27, 48, "all"),
       ("Fcst", "Td", "SCALAR", 24, 27, 48, "all"),  
       ("Fcst", "Wind", "VECTOR", 24, 27, (22, "NW"), "all"),
       ("Fcst", "Sky", "SCALAR", 24, 27, 19, "all"),
       ("Fcst", "Wx", "WEATHER", 24, 27,
        "Pds:T:<NoInten>:<NoVis>:^Inter:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 24, 27, 25, "all"),
       ("Fcst", "QPF", "SCALAR", 24, 27, 1, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 24, 27, 1, "all"),
       ("Fcst", "T", "SCALAR", 27, 30, 43, "all"),
       ("Fcst", "Td", "SCALAR", 27, 30, 43, "all"),  
       ("Fcst", "Wind", "VECTOR", 27, 30, (15, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 27, 30, 30, "all"),
       ("Fcst", "Wx", "WEATHER", 27, 30, "Wide:R:-:3SM:^Ocnl:R:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 27, 30, 50, "all"),
       ("Fcst", "QPF", "SCALAR", 27, 30, 3, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 27, 30, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 24", "MaxTEnd + 24", 50, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 24", "MinTEnd + 24", 33, "all"),
       ("Fcst", "T", "SCALAR", 30, 33, 40, "all"),
       ("Fcst", "Td", "SCALAR", 30, 33, 39, "all"),  
       ("Fcst", "Wind", "VECTOR", 30, 33, (10, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 30, 33, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 30, 33, "SChc:R:--:0SM:^Chc:R:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 30, 33, 60, "all"),
       ("Fcst", "QPF", "SCALAR", 30, 33, 2.5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 30, 33, 0, "all"),
       ("Fcst", "T", "SCALAR", 33, 36, 41, "all"),
       ("Fcst", "Td", "SCALAR", 33, 36, 39, "all"),  
       ("Fcst", "Wind", "VECTOR", 33, 36, (10, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 33, 36, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 33, 36, "Lkly:R:m:1/2SM:^Def:R:m:4SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 33, 36, 70, "all"),
       ("Fcst", "QPF", "SCALAR", 33, 36, 4, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 33, 36, 0, "all"),
       ("Fcst", "T", "SCALAR", 36, 39, 40, "all"),
       ("Fcst", "Td", "SCALAR", 36, 39, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 36, 39, (5, "SE"), "all"),
       ("Fcst", "Sky", "SCALAR", 36, 39, 100, "all"),
       ("Fcst", "Wx", "WEATHER", 36, 39, "Frq:R:+:<NoVis>:^Brf:R:+:5SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 36, 39, 100, "all"),
       ("Fcst", "QPF", "SCALAR", 36, 39, 5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 36, 39, 0, "all"),
       ("Fcst", "T", "SCALAR", 39, 42, 33, "all"),
       ("Fcst", "Td", "SCALAR", 39, 42, 33, "all"),  
       ("Fcst", "Wind", "VECTOR", 39, 42, (5, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 39, 42, 100, "all"),
       ("Fcst", "Wx", "WEATHER", 39, 42, "Pds:R:m:<NoVis>:^Inter:R:m:2SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 39, 42, 100, "all"),
       ("Fcst", "QPF", "SCALAR", 39, 42, 5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 39, 42, 0, "all"),
       ("Fcst", "T", "SCALAR", 42, 45, 35, "all"),
       ("Fcst", "Td", "SCALAR", 42, 45, 33, "all"),  
       ("Fcst", "Wind", "VECTOR", 42, 45, (2.5, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 42, 45, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 42, 45, "Iso:RW:+:<NoVis>:^Sct:RW:+:3/4SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 42, 45, 95, "all"),
       ("Fcst", "QPF", "SCALAR", 42, 45, 5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 42, 45, 0, "all"),
       ("Fcst", "T", "SCALAR", 45, 48, 40, "all"),
       ("Fcst", "Td", "SCALAR", 45, 48, 35, "all"),  
       ("Fcst", "Wind", "VECTOR", 45, 48, (2, "NE"), "all"),
       ("Fcst", "Sky", "SCALAR", 45, 48, 85, "all"),
       ("Fcst", "Wx", "WEATHER", 45, 48, "Num:RW:m:<NoVis>:^Wide:RW:-:1/2SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 45, 48, 65, "all"),
       ("Fcst", "QPF", "SCALAR", 45, 48, 1, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 45, 48, 5, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 45, 48, 0, "all"),
       ("Fcst", "T", "SCALAR", 48, 51, 45, "all"),
       ("Fcst", "Td", "SCALAR", 48, 51, 38, "all"),  
       ("Fcst", "Wind", "VECTOR", 48, 51, (5, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 48, 51, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 48, 51, "Ocnl:RW:-:<NoVis>:^SChc:RW:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 48, 51, 60, "all"),
       ("Fcst", "QPF", "SCALAR", 48, 51, .5, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 48, 51, 3, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 48, 51, 0, "all"),
       ("Fcst", "T", "SCALAR", 51, 54, 50, "all"),
       ("Fcst", "Td", "SCALAR", 51, 54, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 51, 54, (7, "NW"), "all"),
       ("Fcst", "Sky", "SCALAR", 51, 54, 35, "all"),
       ("Fcst", "Wx", "WEATHER", 51, 54, "Chc:RW:--:<NoVis>:^Lkly:RW:--:1SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 51, 54, 50, "all"),
       ("Fcst", "QPF", "SCALAR", 51, 54, .5, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 51, 54, 2, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 51, 54, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 48", "MaxTEnd + 48", 70, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 48", "MinTEnd + 48", 50, "all"),
       ("Fcst", "T", "SCALAR", 54, 57, 50, "all"),
       ("Fcst", "Td", "SCALAR", 54, 57, 45, "all"),  
       ("Fcst", "Wind", "VECTOR", 54, 57, (10, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 54, 57, 30, "all"),
       ("Fcst", "Wx", "WEATHER", 54, 57, "Def:RW:--:<NoVis>:^Frq:RW:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 54, 57, 55, "all"),
       ("Fcst", "QPF", "SCALAR", 54, 57, .25, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 54, 57, 1, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 54, 57, 0, "all"),
       ("Fcst", "T", "SCALAR", 57, 60, 55, "all"),
       ("Fcst", "Td", "SCALAR", 57, 60, 47, "all"),  
       ("Fcst", "Wind", "VECTOR", 57, 60, (12, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 57, 60, 40, "all"),
       ("Fcst", "Wx", "WEATHER", 57, 60, "Brf:RW:-:4SM:^Pds:RW:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 57, 60, 40, "all"),
       ("Fcst", "QPF", "SCALAR", 57, 60, .5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 57, 60, 0, "all"),
       ("Fcst", "T", "SCALAR", 60, 63, 65, "all"),
       ("Fcst", "Td", "SCALAR", 60, 63, 43, "all"),  
       ("Fcst", "Wind", "VECTOR", 60, 63, (15, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 60, 63, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 60, 63, "Inter:RW:--:<NoVis>:^Wide:L:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 60, 63, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 60, 63, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 60, 63, 0, "all"),
       ("Fcst", "T", "SCALAR", 63, 66, 70, "all"),
       ("Fcst", "Td", "SCALAR", 63, 66, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 63, 66, (15, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 63, 66, 5, "all"),
       ("Fcst", "Wx", "WEATHER", 63, 66, "Ocnl:L:--:<NoVis>:^SChc:L:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 63, 66, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 63, 66, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 63, 66, 0, "all"),
       ("Fcst", "T", "SCALAR", 66, 69, 68, "all"),
       ("Fcst", "Td", "SCALAR", 66, 69, 55, "all"),  
       ("Fcst", "Wind", "VECTOR", 66, 69, (20, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 66, 69, 75, "all"),
       ("Fcst", "Wx", "WEATHER", 66, 69, "Chc:L:-:3SM:^Lkly:L:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 66, 69, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 66, 69, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 66, 69, 0, "all"),
       ("Fcst", "T", "SCALAR", 69, 75, 69, "all"),
       ("Fcst", "Td", "SCALAR", 69, 75, 56, "all"),  
       ("Fcst", "Wind", "VECTOR", 69, 75, (10, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 69, 75, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 69, 75, "Def:L:--:<NoVis>:^Areas:L:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 69, 75, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 69, 75, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 69, 75, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 72", "MaxTEnd + 72", 71, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 72", "MinTEnd + 72", 65, "all"),
       ("Fcst", "T", "SCALAR", 75, 81, 69, "all"),
       ("Fcst", "Td", "SCALAR", 75, 81, 60, "all"),  
       ("Fcst", "Wind", "VECTOR", 75, 81, (5, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 75, 81, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 75, 81, "Patchy:L:--:<NoVis>:^Frq:L:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 75, 81, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 75, 81, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 75, 81, 0, "all"),
       ("Fcst", "T", "SCALAR", 81, 87, 70, "all"),
       ("Fcst", "Td", "SCALAR", 81, 87, 61, "all"),  
       ("Fcst", "Wind", "VECTOR", 81, 87, (20, "SE"), "all"),
       ("Fcst", "Sky", "SCALAR", 81, 87, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 81, 87, "Brf:L:--:<NoVis>:^Pds:L:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 81, 87, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 81, 87, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 81, 87, 0, "all"),
       ("Fcst", "T", "SCALAR", 87, 93, 71, "all"),
       ("Fcst", "Td", "SCALAR", 87, 93, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 87, 93, (15, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 87, 93, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 87, 93, "Inter:L:-:<NoVis>:^Wide:ZL:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 87, 93, 30, "all"),
       ("Fcst", "QPF", "SCALAR", 87, 93, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 87, 93, 0, "all"),
       ("Fcst", "T", "SCALAR", 93, 99, 65, "all"),
       ("Fcst", "Td", "SCALAR", 93, 99, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 93, 99, (23, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 93, 99, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 93, 99, "Ocnl:ZL:-:<NoVis>:^SChc:ZL:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 93, 99, 50, "all"),
       ("Fcst", "QPF", "SCALAR", 93, 99, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 93, 99, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 96", "MaxTEnd + 96", 75, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 96", "MinTEnd + 96", 68, "all"),
       ("Fcst", "T", "SCALAR", 99, 105, 68, "all"),
       ("Fcst", "Td", "SCALAR", 99, 105, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 99, 105, (31, "NE"), "all"),
       ("Fcst", "Sky", "SCALAR", 99, 105, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 99, 105, "Chc:ZL:--:<NoVis>:^Lkly:ZL:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 99, 105, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 99, 105, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 99, 105, 0, "all"),
       ("Fcst", "T", "SCALAR", 105, 111, 70, "all"),
       ("Fcst", "Td", "SCALAR", 105, 111, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 105, 111, (40, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 105, 111, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 105, 111, "Def:ZL:--:<NoVis>:^Areas:ZL:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 105, 111, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 105, 111, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 105, 111, 0, "all"),
       ("Fcst", "T", "SCALAR", 111, 117, 73, "all"),
       ("Fcst", "Td", "SCALAR", 111, 117, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 111, 117, (5, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 111, 117, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 111, 117, "Patchy:ZL:--:<NoVis>:^Frq:ZL:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 111, 117, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 111, 117, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 111, 117, 0, "all"),
       ("Fcst", "T", "SCALAR", 117, 123, 75, "all"),
       ("Fcst", "Td", "SCALAR", 117, 123, 60, "all"),  
       ("Fcst", "Wind", "VECTOR", 117, 123, (45, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 117, 123, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 117, 123, "Brf:ZL:--:<NoVis>:^Pds:ZL:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 117, 123, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 117, 123, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 117, 123, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 120", "MaxTEnd + 120", 83, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 120", "MinTEnd + 120", 78, "all"),
       ("Fcst", "T", "SCALAR", 123, 129, 78, "all"),
       ("Fcst", "Td", "SCALAR", 123, 129, 55, "all"),  
       ("Fcst", "Wind", "VECTOR", 123, 129, (17, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 123, 129, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 123, 129, "Inter:ZL:--:<NoVis>:^Wide:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 123, 129, 15, "all"),
       ("Fcst", "QPF", "SCALAR", 123, 129, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 123, 129, 0, "all"),
       ("Fcst", "T", "SCALAR", 129, 135, 80, "all"),
       ("Fcst", "Td", "SCALAR", 129, 135, 50, "all"),  
       ("Fcst", "Wind", "VECTOR", 129, 135, (12, "SE"), "all"),
       ("Fcst", "Sky", "SCALAR", 129, 135, 20, "all"),
       ("Fcst", "Wx", "WEATHER", 129, 135, "Ocnl:ZR:--:<NoVis>:^SChc:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 129, 135, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 129, 135, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 129, 135, 0, "all"),
       ("Fcst", "T", "SCALAR", 135, 141, 81, "all"),
       ("Fcst", "Td", "SCALAR", 135, 141, 45, "all"),  
       ("Fcst", "Wind", "VECTOR", 135, 141, (15, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 135, 141, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 135, 141, "Chc:ZR:--:<NoVis>:^Lkly:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 135, 141, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 135, 141, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 135, 141, 0, "all"),
       ("Fcst", "T", "SCALAR", 141, 147, 83, "all"),
       ("Fcst", "Td", "SCALAR", 141, 147, 43, "all"),  
       ("Fcst", "Wind", "VECTOR", 141, 147, (25, "NW"), "all"),
       ("Fcst", "Sky", "SCALAR", 141, 147, 20, "all"),
       ("Fcst", "Wx", "WEATHER", 141, 147, "Def:ZR:--:<NoVis>:^Frq:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 141, 147, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 141, 147, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 141, 147, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 144", "MaxTEnd + 144", 90, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 144", "MinTEnd + 144", 83, "all"),
       ("Fcst", "T", "SCALAR", 147, 153, 83, "all"),
       ("Fcst", "Td", "SCALAR", 147, 153, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 147, 153, (22, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 147, 153, 20, "all"),
       ("Fcst", "Wx", "WEATHER", 147, 153, "Brf:ZR:--:<NoVis>:^Pds:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 147, 153, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 147, 153, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 147, 153, 0, "all"),
       ("Fcst", "T", "SCALAR", 153, 159, 85, "all"),
       ("Fcst", "Td", "SCALAR", 153, 159, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 153, 159, (31, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 153, 159, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 153, 159, "Inter:ZR:--:<NoVis>:^Wide:S:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 153, 159, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 153, 159, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 153, 159, 0, "all"),
       ("Fcst", "T", "SCALAR", 159, 165, 86, "all"),
       ("Fcst", "Td", "SCALAR", 159, 165, 39, "all"),  
       ("Fcst", "Wind", "VECTOR", 159, 165, (45, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 159, 165, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 159, 165, "Ocnl:S:--:<NoVis>:^SChc:S:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 159, 165, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 159, 165, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 159, 165, 0, "all"),
       ("Fcst", "T", "SCALAR", 165, 171, 90, "all"),
       ("Fcst", "Td", "SCALAR", 165, 171, 30, "all"),  
       ("Fcst", "Wind", "VECTOR", 165, 171, (10, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 165, 171, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 165, 171, "Chc:S:--:<NoVis>:^Lkly:S:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 165, 171, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 165, 171, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 165, 171, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 168", "MaxTEnd + 168", 93, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 168", "MinTEnd + 168", 90, "all"),
       ("Fcst", "T", "SCALAR", 171, 177, 93, "all"),
       ("Fcst", "Td", "SCALAR", 171, 177, 30, "all"),  
       ("Fcst", "Wind", "VECTOR", 171, 177, (10, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 171, 177, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 171, 177, "Def:S:--:<NoVis>:^Frq:S:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 171, 177, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 171, 177, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 171, 177, 0, "all"),
       ("Fcst", "T", "SCALAR", 177, 183, 91, "all"),
       ("Fcst", "Td", "SCALAR", 177, 183, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 177, 183, (15, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 177, 183, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 177, 183, "Brf:S:--:<NoVis>:^Pds:S:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 177, 183, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 177, 183, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 177, 183, 0, "all"),
       ("Fcst", "T", "SCALAR", 183, 189, 90, "all"),
       ("Fcst", "Td", "SCALAR", 183, 189, 35, "all"),  
       ("Fcst", "Wind", "VECTOR", 183, 189, (20, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 183, 189, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 183, 189, "Inter:S:--:<NoVis>:^Iso:SW:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 183, 189, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 183, 189, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 183, 189, 0, "all"),

       ("Fcst", "WindGust", "SCALAR", 0, 189, 0, "all"),
       ]

        
general_deleteGrids = [
        ("Fcst", "PoP", "SFC", -100,280),
        ("Fcst", "MaxT", "SFC", -100,280),
        ("Fcst", "MinT", "SFC", -100,280),
        ("Fcst", "T", "SFC", -100,280),
        ("Fcst", "Td", "SFC", -100,280),
        ("Fcst", "WindChill", "SFC", -100,280),
        ("Fcst", "HeatIndex", "SFC", -100,280),
        ("Fcst", "StormTotalSnow", "SFC", -100,280),
        ("Fcst", "SnowLevel", "SFC", -100,280),
        ("Fcst", "FzLevel", "SFC", -100,280),
        ("Fcst", "RH", "SFC", -100,280),
        ("Fcst", "Wind", "SFC", -100,280),
        ("Fcst", "Sky", "SFC", -100,280),
        ("Fcst", "WindGust", "SFC", -100,280),
        ("Fcst", "Wx", "SFC", -100,280),
        ("Fcst", "QPF", "SFC", -100,280),
        ("Fcst", "SnowAmt", "SFC", -100,280),
        ("Fcst", "Hazards", "SFC", -100,280),
        ]



# Backward compatible test  -- add this in for iTool test
backward_compatible = """

# Backward compatible test  -- add this in for iTool test
Definition["lac"] = "VAC910c"
Definition["pil"] = "WBCSAFNW1"


"""

dir1 = """#Definition["directiveType"] = \"_five12hr_24hrExtended_issuance_list\""""
dir2 = """Definition["directiveType"] = \"_five12hr_24hrExtended_issuance_list\""""


options = """

# Flag to repeat first period at the end. 1 or 0
Definition["repeat1stPeriod"] = 1

# summaryExtended - flag 0 or 1 to generate a summary extended
# forecast. If 1, you must define summaryAreaDict.
# Also turn off extendedLabel and includeExtended
Definition["summaryExtended"] = 1

# summaryAreaDict - dictionary with keys of the LAC ID
# and values of a tuple of (editAreaName, areaLabel) where
# areaLabel is a label string such as "Western Virginia"
# editAreaName must be the name of a GFE defined edit area
Definition["summaryAreaDict"] = {
        "VAC910c":("area1", "the Roanoke area."),
        "NCC940c":("area3",
                   "the northern foothills and mountains of North Carolina."),
        }

# summaryExtendedIntro is a string to introduce the extended
# such as "The extended forecast for"
Definition["summaryExtendedIntro"] = "Now for the Extended Forecast for"

"""

hazards = """

    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "UPG", "CAN", "CON", "EXP"]
        return [
            ('HU.W', allActions, 'Tropical'),     # Hurricane Warning
            ('TY.W', allActions, 'Tropical1'),     # Typhoon Warning
            ('TR.W', allActions, 'Tropical1'),     # Tropical Storm Warning
            ('HU.A', allActions, 'Tropical'),     # Hurricane Watch
            ('TY.A', allActions, 'Tropical1'),     # Typhoon Watch
            ('TR.A', allActions, 'Tropical1'),     # Tropical Storm Watch
            ('HI.W', allActions, 'TropicalNPW'),  # Inland Hurricane Warning
            ('TI.W', allActions, 'TropicalNPW'),  # Inland Tropical Storm Warning
            ('HF.W', allActions, 'Marine'),       # Hurricane Force Wind Warning
            ('HI.A', allActions, 'TropicalNPW'),  # Inland Hurricane Watch
            ('TI.A', allActions, 'TropicalNPW'),  # Inland Tropical Storm Watch
            ('BZ.W', allActions, 'WinterWx'),     # Blizzard Warning
            ('IS.W', allActions, 'WinterWx'),     # Ice Storm Warning
            ('HP.W', allActions, 'WinterWx'),     # Heavy Sleet Warning
            ('IP.W', allActions, 'WinterWx'),     # Sleet Warning
            ('LE.W', allActions, 'WinterWx'),     # Lake Effect Snow Warning
            ('HS.W', allActions, 'WinterWx'),     # Heavy Snow Warning
            ('WS.W', allActions, 'WinterWx'),     # Winter Storm Warning
            ('ZR.Y', allActions, 'WinterWx'),     # Freezing Rain Advisory
            ('HP.Y', allActions, 'WinterWx'),     # Heavy Sleet Advisory
            ('IP.Y', allActions, 'WinterWx'),     # Sleet Advisory
            ('LE.Y', allActions, 'WinterWx'),     # Lake Effect Snow Advisory
            ('SB.Y', allActions, 'WinterWx'),     # Snow and Blowing Snow Advisory
            ('SN.Y', allActions, 'WinterWx'),     # Snow Advisory
            ('BS.Y', allActions, 'WinterWx'),     # Blowing Snow Advisory
            ('WW.Y', allActions, 'WinterWx'),     # Winter Weather Advisory
            ('BZ.A', allActions, 'WinterWx'),     # Blizzard Watch
            ('HP.A', allActions, 'WinterWx'),     # Heavy Sleet Watch
            ('LE.A', allActions, 'WinterWx'),     # Lake Effect Snow Watch
            ('WS.A', allActions, 'WinterWx'),     # Winter Storm Watch
            ('WC.W', allActions, 'WindChill'),    # Wind Chill Warning
            ('WC.Y', allActions, 'WindChill'),    # Wind Chill Advisory
            ('WC.A', allActions, 'WindChill'),    # Wind Chill Watch
            ('DS.W', allActions, 'Dust'),         # Dust Storm Warning
            ('DU.Y', allActions, 'Dust'),         # Blowing Dust Advisory
            ('EC.W', allActions, 'Cold'),         # Extreme Cold Warning
            ('EC.A', allActions, 'Cold'),         # Extreme Cold Watch
            ('EH.W', allActions, 'Heat'),         # Excessive Heat Warning
            ('EH.A', allActions, 'Heat'),         # Excessive Heat Watch
            ('HT.Y', allActions, 'Heat'),         # Heat Advisory
            ('FG.Y', allActions, 'Fog'),          # Dense Fog Advisory
            ('FZ.W', allActions, 'FrostFreeze'),  # Freeze Warning
            ('FR.W', allActions, 'FrostFreeze'),  # Frost Warning
            ('FR.Y', allActions, 'FrostFreeze'),  # Frost Advisory
            ('FZ.A', allActions, 'FrostFreeze'),  # Freeze Watch
            ('HW.W', allActions, 'Wind'),         # High Wind Warning
            ('WI.Y', allActions, 'Wind'),         # Wind Advisory
            ('LW.Y', allActions, 'Wind'),         # Lake Wind Advisory
            ('HW.A', allActions, 'Wind'),         # High Wind Watch
            ('SM.Y', allActions, 'Smoke'),        # Dense Smoke Advisory
            ('ZF.Y', allActions, 'FreezeFog'),    # Freezing Fog Advisory
            ('FF.A', allActions, 'Flood'),        # Flash Flood Watch
            ('FA.A', allActions, 'Flood'),        # Flood Watch
            ('FA.W', allActions, 'Flood'),        # Flood Warning
            ('FA.Y', allActions, 'Flood'),        # Flood Advisory    
            ('CF.Y', allActions, 'CoastalFlood'), # Coastal Flood Advisory
            ('LS.Y', allActions, 'CoastalFlood'), # Lakeshore Flood Advisory
            ('CF.A', allActions, 'CoastalFlood'), # Coastal Flood Watch
            ('LS.A', allActions, 'CoastalFlood'), # Lakeshore Flood Watch
            ('UP.W', allActions, 'IceAccr'),      # Ice Accretion Warning
            ('UP.Y', allActions, 'IceAccr'),      # Ice Accretion Advisory
            ('AS.Y', allActions, 'AirStag'),      # Air Stagnation Advisory
            ('SU.W', allActions, 'HighSurf'),     # High Surf Warning
            ('SU.Y', allActions, 'HighSurf'),     # High Surf Advisory
            ('AF.Y', allActions, 'Ashfall'),      # Volcanic Ashfall Advisory
            ('LO.Y', allActions, 'Drought'),      # Low Water Advisory
            ('TO.A', allActions, 'Convective'),   # Tornado Watch
            ('SV.A', allActions, 'Convective'),   # Severe Thunderstorm Watch
             ]

"""

sun_thru_thurs = [
       "Sunday and Sunday Night,",
       "Mostly clear.",
       "Sprinkles and occasional drizzle during the day, then drizzle overnight.",
       "Highs around 70.",

       "Monday and Monday Night,",
       "Breezy.","Partly cloudy.",
       "Periods of drizzle during the day.","Occasional light freezing drizzle.",
       "Intermittent drizzle overnight.","Highs in the lower 70s.",
       "Lows in the mid 60s.",

       "Tuesday and Tuesday Night,",
       "Windy.", "Mostly clear.", "Very light freezing drizzle.", "Highs in the mid 70s.",
       "Lows in the upper 60s.",

       "Wednesday and Wednesday Night,",
       "Windy.", "Mostly clear.","Intermittent very light freezing drizzle.",
       "Highs in the lower 80s.","Lows in the upper 70s.",

       "Thursday and Thursday Night,",
       "Windy.", "Mostly clear.","Highs around 90.","Lows in the lower 80s.",
       ]

scripts = [
    {
    "name":"SAF_1", 
    "productType":"SAF",
    "commentary": "Morning Issuance -- no options",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Number of Periods', 'numPeriods'): 'All', ('Issuance Type', 'issuanceType'): 'ROUTINE'}",
    "createGrids": general_createGrids,
    "checkStrings": [
       """aT_ENGWBCSAFNW110010109001001010900 CD VAC910c1001012300""",
       """Now for the official National Weather Service forecast for Southwest Mountains including YourTown""",
       "Today,",
       "Sunny.",
       "Occasional thunderstorms early in the afternoon. ",
       "Chance of thunderstorms late in the afternoon.","Highs around 70.",
       "Southwest winds around 10 mph shifting to the southeast in the afternoon.",
       "Chance of thunderstorms 20 percent.",
       
       "Tonight,",
       "Windy.", "Mostly clear.",
       "Thunderstorms until midnight, then occasional rain and intermittent thunderstorms after midnight.",
       "Lows in the lower 40s.",
       "Northeast winds 30 to 35 mph becoming west around 15 mph after midnight.","Chance of precipitation 50 percent.",
       
       "Saturday,",
       "Cooler.","Rain in the morning, then periods of rain in the afternoon.",
       "Rain may be heavy at times in the afternoon.",
       #"Visibility one quarter mile or less at times in the morning.",
       "Highs around 50.",
       "Southwest winds around 10 mph shifting to the southeast in the afternoon.",
       "Chance of rain near 100 percent.",
       
       "Saturday Night,",
       "Colder.",
       "Widespread rain showers until midnight, then partly cloudy with sprinkles likely after midnight.","Locally heavy rainfall possible until midnight.",
       "Lows in the lower 30s.",
       "East winds up to 5 mph shifting to the northwest after midnight.",
       
       "Sunday,",
       "Warmer, sunny.",
       "Rain showers in the morning, then occasional drizzle and intermittent sprinkles in the afternoon.",
       "Highs around 70.","East winds 10 to 15 mph.",
       "Chance of showers 60 percent.",
       
       "Sunday Night,",
       "Warmer", "Mostly clear", "Drizzle", "Near steady temperature in the upper 60s.",
       # DR_18363 "Lows around 50.",
       
       "Monday,",
       "Breezy, sunny.","Periods of drizzle through the day.",
       "Widespread light freezing drizzle in the afternoon.",
       "Near steady temperature around 70.", # DR_18363  "Highs in the lower 70s.",
       
       "Monday Night,",
       "Breezy, warmer.",
       "Partly cloudy.","Intermittent drizzle until midnight.",
       "Occasional light freezing drizzle through the night.",
       "Lows in the mid 60s.",
       
       "Tuesday,",
       "Windy, sunny.","Very light freezing drizzle.","Highs in the mid 70s.",
       
       "Tuesday Night,",
       "Mostly clear.","Periods of very light freezing drizzle.","Lows in the upper 60s.",
       
       "Wednesday,",
       "Sunny.","Intermittent very light freezing drizzle in the morning.",
       "Near steady temperature around 80.", # DR_18363 "Highs in the lower 80s."

       "Wednesday Night,",
       "Windy.", "Mostly clear.", "Near steady temperature in the lower 80s.",
       # DR_18363 "Lows in the upper 70s.",
       
       "Thursday,",
       "Windy, sunny.", "Near steady temperature in the mid 80s.",
       # DR_18363 "Highs around 90.",
       
       "Now for the official National Weather Service forecast for Eastern Virginia",
       "Today,", "Sunny.","Occasional thunderstorms early in the afternoon.",
       
       "B",
       "aT_ENGWBCSAFNW410010109001001010900 CD NCC940c1001012300",
       "Now for the official National Weather Service forecast for Northern North Carolina",
       "Today,", "Sunny.","Occasional thunderstorms early in the afternoon.",
       "B",
       ],
    },

    {
    "name":"SAF_2", 
    "commentary": "Backward Compatibility -- using 'lac' entry",
    "productType":"SAF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Number of Periods', 'numPeriods'): 'All', ('Issuance Type', 'issuanceType'): 'ROUTINE'}",
    "createGrids": general_createGrids,
    "fileChanges": [
       ("SAF_<site>_Definition", "TextUtility", "add", backward_compatible, "undo"),
       ],
    "checkStrings": [
        "aT_ENGWBCSAFNW110010109001001010900 CD VAC910c1001012300",
        "Now for the official National Weather Service forecast for Southwest Mountains including YourTown",
        "Today,", "Sunny.","Occasional thunderstorms early in the afternoon.",
        "Now for the official National Weather Service forecast for Eastern Virginia",
        "Today,", "Sunny.","Occasional thunderstorms early in the afternoon.",
        "Now for the official National Weather Service forecast for Northern North Carolina",
        ],
     "notCheckStrings":[
       "Again, the forecast for Southwest Mountains including YourTown for Today,",
       "aT_ENGWBCSAFNW410010109001001010900 CD NCC940c1001012300",
       "Now for the Extended Forecast for the Roanoke area.",
       ],
    },
    
    {
    "name":"SAF_3", 
    "commentary": "Options: repeat1stPeriod and summary Extended",
    "productType":"SAF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Number of Periods', 'numPeriods'): 'All', ('Issuance Type', 'issuanceType'): 'ROUTINE'}",
    "createGrids": general_createGrids,
    "fileChanges": [
       ("SAF_<site>_Definition", "TextUtility", "add", options, "undo"),
       ],
    "checkStrings": [
       "aT_ENGWBCSAFNW110010109001001010900 CD VAC910c1001012300",
       "Now for the official National Weather Service forecast for Southwest Mountains including YourTown",
       "Today,",
       "Sunny.","Occasional thunderstorms early in the afternoon.",
       "Again, the forecast for Southwest Mountains including YourTown for Today,",
       "Sunny.","Occasional thunderstorms early in the afternoon.",
       "Chance of thunderstorms late in the afternoon.","Highs around 70.",
       "Southwest winds around 10 mph shifting to the southeast in the afternoon.",
       "Chance of thunderstorms 20 percent.",
       
       "Now for the official National Weather Service forecast for Eastern Virginia",
       "Today,",
       "Sunny.","Occasional thunderstorms early in the afternoon.",
       "Again, the forecast for Eastern Virginia for Today,",
       "Sunny.","Occasional thunderstorms early in the afternoon.",
       "Chance of thunderstorms late in the afternoon.","Highs around 70.",
       "Southwest winds around 10 mph shifting to the southeast in the afternoon.",
       "Chance of thunderstorms 20 percent.",
       
       "Now for the Extended Forecast for the Roanoke area.",
       ] + sun_thru_thurs +
       
       [
       "B",
       "aT_ENGWBCSAFNW410010109001001010900 CD NCC940c1001012300",
       "Now for the official National Weather Service forecast for Northern North Carolina",
       "Today,",
       "Sunny.","Occasional thunderstorms early in the afternoon.",
       "Again, the forecast for Northern North Carolina for Today,",
       "Sunny.","Occasional thunderstorms early in the afternoon.",
       "Chance of thunderstorms late in the afternoon.","Highs around 70.",
       "Southwest winds around 10 mph shifting to the southeast in the afternoon.",
       "Chance of thunderstorms 20 percent.",
       
       "Now for the Extended Forecast for the northern foothills and mountains of north carolina.",
       ] + sun_thru_thurs,
    },
     
    {    
    "name":"SAF_4", 
    "commentary": "Including Hazards",
    "productType":"SAF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Number of Periods', 'numPeriods'): 'All', ('Issuance Type', 'issuanceType'): 'ROUTINE'}",
    "createGrids": general_createGrids +
       [("Fcst", "Hazards", "DISCRETE", 0, 39, "WS.A", "all"), ],
    "fileChanges": [
       ("SAF_<site>_Overrides", "TextUtility", "add", hazards, "undo"),
       ],
    "checkStrings": [
        "WINTER STORM WATCH IN EFFECT THROUGH SATURDAY AFTERNOON"
        ],
    },
    
    {    
    "name":"SAF_5", 
    "commentary": "Testing directiveType _five12hr_24hrExtended_issuance_list",
    "productType":"SAF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'TODAY - beginning at 6AM', ('Issued By', 'issuedBy'): None, ('Number of Periods', 'numPeriods'): 'All', ('Issuance Type', 'issuanceType'): 'ROUTINE'}",
    "createGrids": general_createGrids +
       [("Fcst", "Hazards", "DISCRETE", 0, 39, "WS.A", "all"), ],
    "fileChanges": [
       ("SAF_<site>_Definition", "TextUtility", "replace", (dir1, dir2), "undo"),
       ],
    "checkStrings": [
        "Now for the official National Weather Service forecast for Northern North Carolina",
        "Today,",
        "Sunny.","Occasional thunderstorms early in the afternoon.",
        "Chance of thunderstorms late in the afternoon.","Highs around 70.",
        "Southwest winds around 10 mph shifting to the southeast in the afternoon.",
        "Chance of thunderstorms 20 percent",
        ],
    },
    
    {    
    "name":"SAF_Cleanup",
    "commentary": "Clean out grids",
    "productType": None,
    "deleteGrids": TestScript.general_deleteGrids,
    },
    ]    

def testScript(self, dataMgr, level="Site"):
    gridsStartTime = self.getAbsFromLocal(2010, 1, 1, 0, 0)
    drtTime = self.getAbsFromLocal(2010, 1, 1, 4, 0)
    defaults = {
        "gridsStartTime": gridsStartTime,
        "drtTime": drtTime,
        "orderStrings": 1,
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults, level=level)

