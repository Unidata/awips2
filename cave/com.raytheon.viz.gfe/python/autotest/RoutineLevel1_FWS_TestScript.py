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
# FWS tests
#
# Author: hansen
# ----------------------------------------------------------------------------

## NOTE:  If test fails on StqInput Tests by putting up dialog, try
##   --removing any overrides in FWS_TBW_Overrides
##   --restarting the GFE
##   --See if there is an import error in FWS_Overrides or FWS_TBW_Overrides
##   If the current override has a problem, 
##     it picks up the pyo that's lying around from last time.
##     So you'll end up with testIndex of 0 every time.

import TestScript

FWS_createGrids = [    
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin -24", "MaxTEnd -24", 60, "all"),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 78, "all"),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 24", "MaxTEnd + 24", 79, "all"),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 48", "MaxTEnd + 48", 78, "all"),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 72", "MaxTEnd + 72", 80, "all"),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 96", "MaxTEnd + 96", 81, "all"),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 120", "MaxTEnd + 120", 83, "all"),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 144", "MaxTEnd + 144", 84, "all"),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 168", "MaxTEnd + 168", 86, "all"),

        ("Fcst", "MinT", "SCALAR", "MinTBegin-24", "MinTEnd-24", 40, "all"),
        ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 60, "all"),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 24", "MinTEnd + 24", 68, "all"),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 48", "MinTEnd + 48", 65, "all"),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 72", "MinTEnd + 72", 64, "all"),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 96", "MinTEnd + 96", 63, "all"),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 120", "MinTEnd + 120", 66, "all"),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 144", "MinTEnd + 144", 68, "all"),
        ("Fcst", "MinT", "SCALAR", "MinTBegin + 168", "MinTEnd + 168", 67, "all"),

        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin-24", "MaxRHEnd-24", 60, "all"),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin", "MaxRHEnd", 78, "all"),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin + 24", "MaxRHEnd + 24", 80, "all"),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin + 48", "MaxRHEnd + 48", 85, "all"),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin + 72", "MaxRHEnd + 72", 90, "all"),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin + 96", "MaxRHEnd + 96", 87, "all"),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin + 120", "MaxRHEnd + 120", 88, "all"),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin + 144", "MaxRHEnd + 144", 89, "all"),
        ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin + 168", "MaxRHEnd + 168", 90, "all"),
        
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin-24", "MinRHEnd-24", 40, "all"),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin", "MinRHEnd", 65, "all"),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin + 24", "MinRHEnd + 24", 68, "all"),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin + 48", "MinRHEnd + 48", 70, "all"),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin + 72", "MinRHEnd + 72", 73, "all"),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin + 96", "MinRHEnd + 96", 74, "all"),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin + 120", "MinRHEnd + 120", 72, "all"),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin + 144", "MinRHEnd + 144", 70, "all"),
        ("Fcst", "MinRH", "SCALAR", "MinRHBegin + 168", "MinRHEnd + 168", 71, "all"),
        
        ("Fcst", "Wind", "VECTOR", -24, 0, (10, "SW"), "all"),
        ("Fcst", "Wind", "VECTOR", 0, 12, (10, "SW"), "all"),
        ("Fcst", "Wind", "VECTOR", 12, 24, (40, "SE"), "all"),
        ("Fcst", "Wind", "VECTOR", 24, 36, (35, "NW"), "all"),
        ("Fcst", "Wind", "VECTOR", 36, 48, (45, "W"), "all"),
        ("Fcst", "Wind", "VECTOR", 48, 60, (50, "SW"), "all"),
        ("Fcst", "Wind", "VECTOR", 60, 72, (45, "E"), "all"),
        ("Fcst", "Wind", "VECTOR", 72, 84, (60, "W"), "all"),
        ("Fcst", "Wind", "VECTOR", 84, 96, (55, "SW"), "all"),
        ("Fcst", "Wind", "VECTOR", 96, 108, (55, "SW"), "all"),
        ("Fcst", "Wind", "VECTOR", 108, 120, (42, "E"), "all"),
        ("Fcst", "Wind", "VECTOR", 120, 132, (45, "E"), "all"),
        ("Fcst", "Wind", "VECTOR", 132, 144, (46, "E"), "all"),
        ("Fcst", "Wind", "VECTOR", 144, 156, (48, "E"), "all"),
        ("Fcst", "Wind", "VECTOR", 156, 168, (60, "E"), "all"),
        ("Fcst", "Wind", "VECTOR", 168, 180, (35, "E"), "all"),
        ("Fcst", "Wind", "VECTOR", 180, 192, (50, "E"), "all"),
            
        ("Fcst", "TransWind", "VECTOR", -24, 0, (10, "SW"), "all"),
        ("Fcst", "TransWind", "VECTOR", 0, 12, (10, "SW"), "all"),
        ("Fcst", "TransWind", "VECTOR", 12, 24, (5, "W"), "all"),
        ("Fcst", "TransWind", "VECTOR", 24, 48, (10, "NW"), "all"),
        ("Fcst", "TransWind", "VECTOR", 48, 72, (20, "W"), "all"),
        ("Fcst", "TransWind", "VECTOR", 72, 96, (30, "W"), "all"),
        ("Fcst", "TransWind", "VECTOR", 96, 120, (40, "W"), "all"),
        ("Fcst", "TransWind", "VECTOR", 120, 144, (50, "W"), "all"),
        ("Fcst", "TransWind", "VECTOR", 144, 168, (60, "W"), "all"),
        ("Fcst", "TransWind", "VECTOR", 168, 192, (70, "W"), "all"),

        ("Fcst", "FreeWind", "VECTOR", -24, 0, (10, "SW"), "all"),
        ("Fcst", "FreeWind", "VECTOR", 0, 12, (10, "SW"), "all"),
        ("Fcst", "FreeWind", "VECTOR", 12, 24, (5, "W"), "all"),
        ("Fcst", "FreeWind", "VECTOR", 24, 48, (10, "NW"), "all"),
        ("Fcst", "FreeWind", "VECTOR", 48, 72, (20, "W"), "all"),
        ("Fcst", "FreeWind", "VECTOR", 72, 96, (30, "W"), "all"),
        ("Fcst", "FreeWind", "VECTOR", 96, 120, (40, "W"), "all"),
        ("Fcst", "FreeWind", "VECTOR", 120, 144, (50, "W"), "all"),
        ("Fcst", "FreeWind", "VECTOR", 144, 168, (60, "W"), "all"),
        ("Fcst", "FreeWind", "VECTOR", 168, 192, (70, "W"), "all"),

        ("Fcst", "WindGust", "SCALAR", -24, 0, 25, "all"),
        ("Fcst", "WindGust", "SCALAR", 0, 12, 25, "all"),
        ("Fcst", "WindGust", "SCALAR", 12, 24, 0, "all"),
        ("Fcst", "WindGust", "SCALAR", 24, 36, 0, "all"),
        ("Fcst", "WindGust", "SCALAR", 36, 48, 0, "all"),
        ("Fcst", "WindGust", "SCALAR", 48, 60, 0, "all"),
        ("Fcst", "WindGust", "SCALAR", 60, 72, 0, "all"),
        ("Fcst", "WindGust", "SCALAR", 72, 84, 0, "all"),
        ("Fcst", "WindGust", "SCALAR", 84, 96, 0, "all"),
        ("Fcst", "WindGust", "SCALAR", 96, 108, 0, "all"),

        ("Fcst", "T", "SCALAR", 24, 0, 100, "all"),
        ("Fcst", "T", "SCALAR", 0, 12, 100, "all"),
        ("Fcst", "T", "SCALAR", 12, 24, 95, "all"),
        ("Fcst", "T", "SCALAR", 24, 36, 0, "all"),
        ("Fcst", "T", "SCALAR", 36, 48, 15, "all"),
        ("Fcst", "T", "SCALAR", 48, 60, 30, "all"),
        ("Fcst", "T", "SCALAR", 60, 72, 55, "all"),
        ("Fcst", "T", "SCALAR", 72, 84, 65, "all"),
        ("Fcst", "T", "SCALAR", 84, 96, 70, "all"),
        ("Fcst", "T", "SCALAR", 96, 108, 30, "all"),
        ("Fcst", "T", "SCALAR", 108, 120, 48, "all"),
        ("Fcst", "T", "SCALAR", 120, 132, 100, "all"),
        ("Fcst", "T", "SCALAR", 132, 144, 10, "all"),
        ("Fcst", "T", "SCALAR", 144, 156, 75, "all"),
        ("Fcst", "T", "SCALAR", 156, 168, 25, "all"),
        ("Fcst", "T", "SCALAR", 168, 180, 20, "all"),
        ("Fcst", "T", "SCALAR", 180, 192, 87, "all"),

        ("Fcst", "Td", "SCALAR", 24, 0, 100, "all"),
        ("Fcst", "Td", "SCALAR", 0, 12, 100, "all"),
        ("Fcst", "Td", "SCALAR", 12, 24, 95, "all"),
        ("Fcst", "Td", "SCALAR", 24, 36, 0, "all"),
        ("Fcst", "Td", "SCALAR", 36, 48, 15, "all"),
        ("Fcst", "Td", "SCALAR", 48, 60, 30, "all"),
        ("Fcst", "Td", "SCALAR", 60, 72, 55, "all"),
        ("Fcst", "Td", "SCALAR", 72, 84, 65, "all"),
        ("Fcst", "Td", "SCALAR", 84, 96, 70, "all"),
        ("Fcst", "Td", "SCALAR", 96, 108, 30, "all"),
        ("Fcst", "Td", "SCALAR", 108, 120, 48, "all"),
        ("Fcst", "Td", "SCALAR", 120, 132, 100, "all"),
        ("Fcst", "Td", "SCALAR", 132, 144, 10, "all"),
        ("Fcst", "Td", "SCALAR", 144, 156, 75, "all"),
        ("Fcst", "Td", "SCALAR", 156, 168, 25, "all"),
        ("Fcst", "Td", "SCALAR", 168, 180, 20, "all"),
        ("Fcst", "Td", "SCALAR", 180, 192, 87, "all"),

        ("Fcst", "RH", "SCALAR", 24, 0, 100, "all"),
        ("Fcst", "RH", "SCALAR", 0, 12, 100, "all"),
        ("Fcst", "RH", "SCALAR", 12, 24, 95, "all"),
        ("Fcst", "RH", "SCALAR", 24, 36, 0, "all"),
        ("Fcst", "RH", "SCALAR", 36, 48, 15, "all"),
        ("Fcst", "RH", "SCALAR", 48, 60, 30, "all"),
        ("Fcst", "RH", "SCALAR", 60, 72, 55, "all"),
        ("Fcst", "RH", "SCALAR", 72, 84, 65, "all"),
        ("Fcst", "RH", "SCALAR", 84, 96, 70, "all"),
        ("Fcst", "RH", "SCALAR", 96, 108, 30, "all"),
        ("Fcst", "RH", "SCALAR", 108, 120, 48, "all"),
        ("Fcst", "RH", "SCALAR", 120, 132, 100, "all"),
        ("Fcst", "RH", "SCALAR", 132, 144, 10, "all"),
        ("Fcst", "RH", "SCALAR", 144, 156, 75, "all"),
        ("Fcst", "RH", "SCALAR", 156, 168, 25, "all"),
        ("Fcst", "RH", "SCALAR", 168, 180, 20, "all"),
        ("Fcst", "RH", "SCALAR", 180, 192, 87, "all"),

        ("Fcst", "Sky", "SCALAR", 24, 0, 100, "all"),
        ("Fcst", "Sky", "SCALAR", 0, 12, 100, "all"),
        ("Fcst", "Sky", "SCALAR", 12, 24, 95, "all"),
        ("Fcst", "Sky", "SCALAR", 24, 36, 0, "all"),
        ("Fcst", "Sky", "SCALAR", 36, 48, 15, "all"),
        ("Fcst", "Sky", "SCALAR", 48, 60, 30, "all"),
        ("Fcst", "Sky", "SCALAR", 60, 72, 55, "all"),
        ("Fcst", "Sky", "SCALAR", 72, 84, 65, "all"),
        ("Fcst", "Sky", "SCALAR", 84, 96, 70, "all"),
        ("Fcst", "Sky", "SCALAR", 96, 108, 30, "all"),
        ("Fcst", "Sky", "SCALAR", 108, 120, 48, "all"),
        ("Fcst", "Sky", "SCALAR", 120, 132, 100, "all"),
        ("Fcst", "Sky", "SCALAR", 132, 144, 10, "all"),
        ("Fcst", "Sky", "SCALAR", 144, 156, 75, "all"),
        ("Fcst", "Sky", "SCALAR", 156, 168, 25, "all"),
        ("Fcst", "Sky", "SCALAR", 168, 180, 20, "all"),
        ("Fcst", "Sky", "SCALAR", 180, 192, 87, "all"),
                
        ("Fcst", "Wx", "WEATHER", -24, 0, "Patchy:F:+:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 0, 12, "Patchy:F:+:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 12, 24, "Wide:T:<NoInten>:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 24, 36, "Chc:RW:-:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 36, 48, "Frq:R:--:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 48, 60, "Wide:ZR:-:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 60, 72, "Lkly:S:--:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 72, 84, "Wide:IP:--:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 84, 96, "Areas:BS:<NoInten>:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 96, 108, "Patchy:F:<NoInten>:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 108, 120, "Lkly:L:--:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 120, 132, "SChc:ZL:--:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 132, 144, "Num:T:<NoInten>:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 144, 156, "Iso:ZY:-:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 156, 168, "Areas:FR:<NoInten>:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 168, 180, "Chc:RW:-:<NoVis>:", "all"),
        ("Fcst", "Wx", "WEATHER", 180, 192, "Brf:R:m:<NoVis>:", "all"),

        ("Fcst", "PoP", "SCALAR", -24, 0, 0, "all"),
        ("Fcst", "PoP", "SCALAR", 0, 12, 0, "all"),
        ("Fcst", "PoP", "SCALAR", 12, 24, 90 , "all"),
        ("Fcst", "PoP", "SCALAR", 24, 36, 90, "all"),
        ("Fcst", "PoP", "SCALAR", 36, 48, 90, "all"),
        ("Fcst", "PoP", "SCALAR", 48, 60, 90, "all"),
        ("Fcst", "PoP", "SCALAR", 60, 72, 70, "all"),
        ("Fcst", "PoP", "SCALAR", 72, 84, 90, "all"),
        ("Fcst", "PoP", "SCALAR", 84, 96, 0, "all"),
        ("Fcst", "PoP", "SCALAR", 96, 108, 0, "all"),
        ("Fcst", "PoP", "SCALAR", 108, 120, 70, "all"),
        ("Fcst", "PoP", "SCALAR", 120, 132, 20, "all"),
        ("Fcst", "PoP", "SCALAR", 132, 144, 70, "all"),
        ("Fcst", "PoP", "SCALAR", 144, 156, 20, "all"),
        ("Fcst", "PoP", "SCALAR", 156, 168, 0, "all"),
        ("Fcst", "PoP", "SCALAR", 168, 180, 40, "all"),
        ("Fcst", "PoP", "SCALAR", 180, 192, 90, "all"),
        
        ("Fcst", "LAL", "SCALAR", -24, 0, 1, "all"),
        ("Fcst", "LAL", "SCALAR", 0, 12, 1, "all"),
        ("Fcst", "LAL", "SCALAR", 12, 24, 2, "all"),
        ("Fcst", "LAL", "SCALAR", 24, 36, 3, "all"),
        ("Fcst", "LAL", "SCALAR", 36, 48, 4, "all"),
        ("Fcst", "LAL", "SCALAR", 48, 60, 5, "all"),
        ("Fcst", "LAL", "SCALAR", 60, 72, 6, "all"),
        ("Fcst", "LAL", "SCALAR", 72, 84, 3, "all"),
        ("Fcst", "LAL", "SCALAR", 84, 96, 1, "all"),
        ("Fcst", "LAL", "SCALAR", 96, 108, 2, "all"),
        ("Fcst", "LAL", "SCALAR", 108, 120, 4, "all"),
        ("Fcst", "LAL", "SCALAR", 120, 132, 5, "all"),
        ("Fcst", "LAL", "SCALAR", 132, 144, 3, "all"),
        ("Fcst", "LAL", "SCALAR", 144, 156, 2, "all"),
        ("Fcst", "LAL", "SCALAR", 156, 168, 5, "all"),
        ("Fcst", "LAL", "SCALAR", 168, 180, 6, "all"),
        ("Fcst", "LAL", "SCALAR", 180, 192, 3, "all"),
                        
        ("Fcst", "CWR", "SCALAR", -24, 0, 0, "all"),
        ("Fcst", "CWR", "SCALAR", 0, 12, 0, "all"),
        ("Fcst", "CWR", "SCALAR", 12, 24, 20, "all"),
        ("Fcst", "CWR", "SCALAR", 24, 36, 30, "all"),
        ("Fcst", "CWR", "SCALAR", 36, 48, 30, "all"),
        ("Fcst", "CWR", "SCALAR", 48, 60, 45, "all"),
        ("Fcst", "CWR", "SCALAR", 60, 72, 60, "all"),
        ("Fcst", "CWR", "SCALAR", 72, 84, 25, "all"),
        ("Fcst", "CWR", "SCALAR", 84, 96, 47, "all"),
        ("Fcst", "CWR", "SCALAR", 96, 108, 34, "all"),
        ("Fcst", "CWR", "SCALAR", 108, 120, 60, "all"),
        ("Fcst", "CWR", "SCALAR", 120, 132, 55, "all"),
        ("Fcst", "CWR", "SCALAR", 132, 144, 50, "all"),
        ("Fcst", "CWR", "SCALAR", 144, 156, 20, "all"),
        ("Fcst", "CWR", "SCALAR", 156, 168, 10, "all"),
        ("Fcst", "CWR", "SCALAR", 168, 180, 5, "all"),
        ("Fcst", "CWR", "SCALAR", 180, 192, 40, "all"),
                
        ("Fcst", "QPF", "SCALAR", -24, 0, 0, "all"),
        ("Fcst", "QPF", "SCALAR", 0, 12, 0, "all"),
        ("Fcst", "QPF", "SCALAR", 12, 24, 0.05, "all"),
        ("Fcst", "QPF", "SCALAR", 24, 36, 0.1, "all"),
        ("Fcst", "QPF", "SCALAR", 36, 48, 0, "all"),
        ("Fcst", "QPF", "SCALAR", 48, 60, 5, "all"),
        ("Fcst", "QPF", "SCALAR", 60, 72, 4.5, "all"),
        ("Fcst", "QPF", "SCALAR", 72, 84, 1.5, "all"),
        ("Fcst", "QPF", "SCALAR", 84, 96, 2.5, "all"),
        ("Fcst", "QPF", "SCALAR", 96, 108, 3.5, "all"),
        ("Fcst", "QPF", "SCALAR", 108, 120, 4.0, "all"),
        ("Fcst", "QPF", "SCALAR", 120, 132, 1.0, "all"),
        ("Fcst", "QPF", "SCALAR", 132, 144, 2.0, "all"),
        ("Fcst", "QPF", "SCALAR", 144, 156, 3.0, "all"),
        ("Fcst", "QPF", "SCALAR", 156, 168, 1.3, "all"),
        ("Fcst", "QPF", "SCALAR", 168, 180, 0.12, "all"),
        ("Fcst", "QPF", "SCALAR", 180, 192, 0.34, "all"),
        
        ("Fcst", "Haines", "SCALAR", -24, 0, 2, "all"),
        ("Fcst", "Haines", "SCALAR", 0, 12, 2, "all"),
        ("Fcst", "Haines", "SCALAR", 12, 24, 3, "all"),
        ("Fcst", "Haines", "SCALAR", 24, 36, 4, "all"),
        ("Fcst", "Haines", "SCALAR", 36, 48, 6, "all"),
        ("Fcst", "Haines", "SCALAR", 48, 60, 2, "all"),
        ("Fcst", "Haines", "SCALAR", 60, 72, 3, "all"),
        ("Fcst", "Haines", "SCALAR", 72, 84, 2, "all"),
        ("Fcst", "Haines", "SCALAR", 84, 96, 3, "all"),
        ("Fcst", "Haines", "SCALAR", 96, 108, 5, "all"),
        ("Fcst", "Haines", "SCALAR", 108, 120, 6, "all"),
        ("Fcst", "Haines", "SCALAR", 120, 132, 3, "all"),
        ("Fcst", "Haines", "SCALAR", 132, 144, 2, "all"),
        ("Fcst", "Haines", "SCALAR", 144, 156, 3, "all"),
        ("Fcst", "Haines", "SCALAR", 156, 168, 4, "all"),
        ("Fcst", "Haines", "SCALAR", 168, 180, 3, "all"),
        ("Fcst", "Haines", "SCALAR", 180, 192, 6, "all"),
        
        ("Fcst", "MixHgt", "SCALAR", -24, 0, 50, "all"),
        ("Fcst", "MixHgt", "SCALAR", 0, 24, 50, "all"),
        ("Fcst", "MixHgt", "SCALAR", 24, 48, 100, "all"),
        ("Fcst", "MixHgt", "SCALAR", 48, 72, 4000, "all"),
        ("Fcst", "MixHgt", "SCALAR", 72, 96, 3500, "all"),
        ("Fcst", "MixHgt", "SCALAR", 96, 120, 300, "all"),
        ("Fcst", "MixHgt", "SCALAR", 120, 144, 10, "all"),
        ("Fcst", "MixHgt", "SCALAR", 144, 168, 5000, "all"),
        ("Fcst", "MixHgt", "SCALAR", 168, 192, 600, "all"),
        
        ("Fcst", "MarineLayer", "SCALAR", 0, 24, 1000, "all"),
        ("Fcst", "MarineLayer", "SCALAR", 24, 48, 2000, "all"),
        ("Fcst", "MarineLayer", "SCALAR", 48, 72, 4000, "all"),
        ("Fcst", "MarineLayer", "SCALAR", 72, 96, 5280, "all"),
        ("Fcst", "MarineLayer", "SCALAR", 96, 120, 6500, "all"),
        ("Fcst", "MarineLayer", "SCALAR", 120, 144, 10000, "all"),
        ("Fcst", "MarineLayer", "SCALAR", 144, 168, 12300, "all"),
        ("Fcst", "MarineLayer", "SCALAR", 168, 192, 14500, "all"),

        ("Fcst", "Wind20ft", "VECTOR", -24, 0, (5, "N"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 0, 12, (5, "N"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 12, 24, (40, "NE"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 24, 36, (10, "NW"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 36, 48, (0, "N"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 48, 60, (125, "E"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 60, 72, (90, "S"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 72, 84, (50, "S"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 84, 96, (100, "S"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 96, 108, (0, "S"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 108, 120, (10, "S"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 120, 132, (30, "S"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 132, 144, (60, "S"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 144, 156, (25, "S"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 156, 168, (68, "S"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 168, 180, (15, "S"), "all"),
        ("Fcst", "Wind20ft", "VECTOR", 180, 192, (2, "S"), "all"),
        
        ("Fcst", "VentRate", "SCALAR", -24, 0, 160000, "all"),
        ("Fcst", "VentRate", "SCALAR", 0, 12, 160000, "all"),
        ("Fcst", "VentRate", "SCALAR", 12, 24, 100000, "all"),
        ("Fcst", "VentRate", "SCALAR", 24, 36, 50000, "all"),
        ("Fcst", "VentRate", "SCALAR", 36, 48, 20000, "all"),
        ("Fcst", "VentRate", "SCALAR", 48, 60, 70000, "all"),
        ("Fcst", "VentRate", "SCALAR", 60, 144, 4000, "all"),
        ("Fcst", "VentRate", "SCALAR", 144, 168, 6900, "all"),
        ("Fcst", "VentRate", "SCALAR", 168, 192, 30000, "all"),
        
        ("Fcst", "Stability", "SCALAR", -24, 0, 1, "all"),
        ("Fcst", "Stability", "SCALAR", 0, 12, 1, "all"),
        ("Fcst", "Stability", "SCALAR", 12, 24, 2, "all"),
        ("Fcst", "Stability", "SCALAR", 24, 36, 1, "all"),
        ("Fcst", "Stability", "SCALAR", 36, 48, 3, "all"),
        ("Fcst", "Stability", "SCALAR", 48, 60, 4, "all"),
        ("Fcst", "Stability", "SCALAR", 60, 72, 5, "all"),
        ("Fcst", "Stability", "SCALAR", 72, 84, 1, "all"),
        ("Fcst", "Stability", "SCALAR", 84, 96, 2, "all"),
        ("Fcst", "Stability", "SCALAR", 96, 108, 3, "all"),
        ("Fcst", "Stability", "SCALAR", 108, 120, 4, "all"),
        ("Fcst", "Stability", "SCALAR", 120, 132, 5, "all"),
        ("Fcst", "Stability", "SCALAR", 132, 144, 4, "all"),
        ("Fcst", "Stability", "SCALAR", 144, 156, 3, "all"),
        ("Fcst", "Stability", "SCALAR", 156, 168, 2, "all"),
        ("Fcst", "Stability", "SCALAR", 168, 180, 1, "all"),
        ("Fcst", "Stability", "SCALAR", 180, 192, 3, "all"),
        
        ("Fcst", "HrsOfSun", "SCALAR", -24, 0, 6, "all"),
        ("Fcst", "HrsOfSun", "SCALAR", 0, 24, 6, "all"),
        ("Fcst", "HrsOfSun", "SCALAR", 24, 48, 7, "all"),
        ("Fcst", "HrsOfSun", "SCALAR", 48, 72, 5, "all"),
        ("Fcst", "HrsOfSun", "SCALAR", 72, 96, 5, "all"),
        ("Fcst", "HrsOfSun", "SCALAR", 96, 120, 5, "all"),
        ("Fcst", "HrsOfSun", "SCALAR", 120, 144, 5, "all"),
        ("Fcst", "HrsOfSun", "SCALAR", 144, 168, 5, "all"),
        ("Fcst", "HrsOfSun", "SCALAR", 168, 192, 5, "all"),
        
        ("Fcst", "DSI", "SCALAR", -24, 0, 0, "all"),
        ("Fcst", "DSI", "SCALAR", 0, 12, 0, "all"),
        ("Fcst", "DSI", "SCALAR", 12, 24, 2, "all"),
        ("Fcst", "DSI", "SCALAR", 24, 36, 6, "all"),
        ("Fcst", "DSI", "SCALAR", 36, 48, 1, "all"),
        ("Fcst", "DSI", "SCALAR", 48, 60, 5, "all"),
        ("Fcst", "DSI", "SCALAR", 60, 72, 4, "all"),
        ("Fcst", "DSI", "SCALAR", 72, 84, 3, "all"),
        ("Fcst", "DSI", "SCALAR", 84, 96, 2, "all"),
        ("Fcst", "DSI", "SCALAR", 96, 108, 1, "all"),
        ("Fcst", "DSI", "SCALAR", 108, 120, 0, "all"),
        ("Fcst", "DSI", "SCALAR", 120, 132, 5, "all"),
        ("Fcst", "DSI", "SCALAR", 132, 144, 4, "all"),
        ("Fcst", "DSI", "SCALAR", 144, 156, 3, "all"),
        ("Fcst", "DSI", "SCALAR", 156, 168, 2, "all"),
        ("Fcst", "DSI", "SCALAR", 168, 180, 1, "all"),
        ("Fcst", "DSI", "SCALAR", 180, 192, 0, "all"),
        ]

Sky_createGrids = [
        ("Fcst", "Sky", "SCALAR", 0, 12, 25, "all"),
        ("Fcst", "Sky", "SCALAR", 12, 24, 50, "all"),
        ("Fcst", "Sky", "SCALAR", 24, 36, 75, "all"),
        ]

testProcVarList1 = """

    def getPreviousProduct(self, stqPil, searchString, version=0):
        stqProducts = self._stqProducts()
        if version > len(stqProducts) - 1:
            return None
        else:
            return stqProducts[version]

    def _callProcessVariableList(self, title, varList, varDict):
        # Transfer varList to varDict
        print "Test Index", self._testIndex
        desFireName = "Please Choose a Fire", "fireName"
        issuance = "Product Issuance:","productIssuance"
        for entry in varList:
            print "**** entry:", entry
            name = entry[0]
            #print "name", name
            default = entry[1]
            if name == desFireName:
                valueList = entry[3]
            if name == issuance:
                try:
                    default = self._setIssuance
                    print "setting issuance", self._setIssuance
                except:
                    pass                
            try:
                default = float(default)
            except:
                pass
            varDict[name] = default
        if varDict.has_key(desFireName):
            varDict[desFireName] = valueList[self._testIndex]
        varDict[('Forecaster:', 'forecaster')] = "FORECASTER C"
        for key in varDict.keys():
            print key, varDict[key]
        return varDict

    def _makeFcstTimeStatement(self, fcst, argDict):
        actualTimeZone = self._getActualTimeZone()   
        #self._fireDateTime = time.strptime(
        #    self._fireTime + ' ' + self._fireDate + ' ' + actualTimeZone,
        #    '%H%M %m/%d/%y %Z')
        self._fireDateTime = time.strptime(
            self._fireTime + ' ' + self._fireDate,
            '%H%M %m/%d/%y')
        requestWords = self._getRequestWords()
        fcst = fcst + time.strftime(
            'Forecast is based on ' + requestWords + ' time of %H%M %Z on %B %d. ',
            self._fireDateTime)
        fcst = fcst + "\\n"
        self._makeFireTimeRange()
        return fcst    

    def _stqProducts(self):
        products = [
            \"\"\"        

            BMBB91 KBYZ 131500
            STQBYZ

            A SPOT FORECAST REQUEST HAS JUST BEEN RECEIVED FOR A WILDFIRE
            NAMED "IFPS TEST"

                       PRIORITY:  IMMEDIATE
                           DATE:  10/13/04
                           TIME:  0912
                   PROJECT NAME:  IFPS TEST 0
                   PROJECT TYPE:  WILDFIRE
              REQUESTING AGENCY:  WFO Billings
            REQUESTING OFFICIAL:  Virgil Middendorf
                            FAX:  (406) 652-3214
                EMERGENCY PHONE:  (406) 652-0851 
                       LOCATION:  
                          STATE:  MT
                           DLAT:  28.27
                           DLON:  82.19
                       EXPOSURE:  N
                      FUEL TYPE:  Grass
                     SHELTERING:  UNSHELTERED
               BOTTOM ELEVATION:  3200
                  TOP ELEVATION:  3200
                   SIZE (ACRES):  1

            WEATHER CONDITIONS AT PROJECT OR FROM NEARBY STATIONS
            SITE ELEV=3200 TIME=0900 WIND=SW10 T=50 TW=45 RH=90 TD= Cloudy
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 

            ...REMARKS...
            TEST

            ...WEATHER PARAMETERS REQUESTED...
                  CLOUDS / WEATHER: 1,0,1
            CHANCE OF WETTING RAIN: 1,0,1
                       TEMPERATURE: 1,0,1
                 RELATIVE HUMIDITY: 0,0,1
                    EYE LEVEL WIND: 1,0,0
                  SMOKE DISPERSION: 1,0,0

            SITE:  BYZ
            OFILE:  20041013.IFPST.01
            TIMEZONE:  MST7MDT
            
            \"\"\",            
            \"\"\"
            BMBB91 KBYZ 131501
            STQBYZ

            A SPOT FORECAST REQUEST HAS JUST BEEN RECEIVED FOR A WILDFIRE
            NAMED "IFPS TEST"

                       PRIORITY:  IMMEDIATE
                           DATE:  10/13/04
                           TIME:  0912
                   PROJECT NAME:  IFPS TEST 1
                   PROJECT TYPE:  HAZMAT
              REQUESTING AGENCY:  WFO Billings
            REQUESTING OFFICIAL:  Virgil Middendorf
                            FAX:  (406) 652-3214
                EMERGENCY PHONE:  (406) 652-0851 
                       LOCATION:  
                          STATE:  MT
                           DLAT:  28.27
                           DLON:  82.19
                       EXPOSURE:  N
                      FUEL TYPE:  Grass
                     SHELTERING:  UNSHELTERED
               BOTTOM ELEVATION:  3200
                  TOP ELEVATION:  3200
                   SIZE (ACRES):  1

            WEATHER CONDITIONS AT PROJECT OR FROM NEARBY STATIONS
            SITE ELEV=3200 TIME=0900 WIND=SW10 T=50 TW=45 RH=90 TD= Cloudy
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 

            ...REMARKS...
            TEST

            ...WEATHER PARAMETERS REQUESTED...
                  CLOUDS / WEATHER: 1,0,1
            CHANCE OF WETTING RAIN: 1,0,1
                       TEMPERATURE: 1,0,0
                 RELATIVE HUMIDITY: 1,0,0
                    EYE LEVEL WIND: 1,0,0
                  SMOKE DISPERSION: 1,0,0

            SITE:  BYZ
            OFILE:  20041013.IFPST.01
            TIMEZONE:  MST7MDT

            \"\"\",
            \"\"\"
            BMBB91 KBYZ 131502
            STQBYZ

            A SPOT FORECAST REQUEST HAS JUST BEEN RECEIVED FOR A WILDFIRE
            NAMED "IFPS TEST"

                       PRIORITY:  IMMEDIATE
                           DATE:  10/13/04
                           TIME:  0912
                   PROJECT NAME:  IFPS TEST 2
                   PROJECT TYPE:  PRESCRIBED
              REQUESTING AGENCY:  WFO Billings
            REQUESTING OFFICIAL:  Virgil Middendorf
                            FAX:  (406) 652-3214
                EMERGENCY PHONE:  (406) 652-0851 
                       LOCATION:  
                          STATE:  MT
                           DLAT:  28.27
                           DLON:  82.19
                       EXPOSURE:  N
                      FUEL TYPE:  Grass
                     SHELTERING:  UNSHELTERED
               BOTTOM ELEVATION:  3200
                  TOP ELEVATION:  3200
                   SIZE (ACRES):  1

            WEATHER CONDITIONS AT PROJECT OR FROM NEARBY STATIONS
            SITE ELEV=3200 TIME=0900 WIND=SW10 T=50 TW=45 RH=90 TD= Cloudy
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 

            ...REMARKS...
            TEST

            ...WEATHER PARAMETERS REQUESTED...
                  CLOUDS / WEATHER: 1,1,1
            CHANCE OF WETTING RAIN: 1,1,1
                       TEMPERATURE: 1,1,0
                 RELATIVE HUMIDITY: 1,1,0
                    EYE LEVEL WIND: 1,0,0
                  SMOKE DISPERSION: 1,0,0

            SITE:  BYZ
            OFILE:  20041013.IFPST.01
            TIMEZONE:  MST7MDT

            \"\"\",
            \"\"\"
            BMBB91 KBYZ 131503
            STQBYZ

            A SPOT FORECAST REQUEST HAS JUST BEEN RECEIVED FOR A PRESCRIBED
            NAMED "IFPS TEST"

                       PRIORITY:  IMMEDIATE
                           DATE:  10/13/04
                           TIME:  0912
                   PROJECT NAME:  IFPS TEST 3
                   PROJECT TYPE:  PRESCRIBED
              REQUESTING AGENCY:  WFO Billings
            REQUESTING OFFICIAL:  Virgil Middendorf
                            FAX:  (406) 652-3214
                EMERGENCY PHONE:  (406) 652-0851 
                       LOCATION:  
                          STATE:  MT
                           DLAT:  28.27
                           DLON:  82.19
                      EXPOSURE:  N
                      FUEL TYPE:  Grass
                     SHELTERING:  UNSHELTERED
               BOTTOM ELEVATION:  3200
                  TOP ELEVATION:  3200
                   SIZE (ACRES):  1

            WEATHER CONDITIONS AT PROJECT OR FROM NEARBY STATIONS
            SITE ELEV=3200 TIME=0900 WIND=SW10 T=50 TW=45 RH=90 TD= Cloudy
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 

            ...REMARKS...
            TEST

            ...WEATHER PARAMETERS REQUESTED...
                  CLOUDS / WEATHER: 1,1,1
            CHANCE OF WETTING RAIN: 1,1,1
                       TEMPERATURE: 1,1,0
                 RELATIVE HUMIDITY: 1,1,0
                    EYE LEVEL WIND: 0,0,0
                  SMOKE DISPERSION: 0,0,0

            SITE:  BYZ
            OFILE:  20041013.IFPST.01
            TIMEZONE:  MST7MDT
            \"\"\",
            \"\"\"

            BMBB91 KBYZ 131504
            STQBYZ

            A SPOT FORECAST REQUEST HAS JUST BEEN RECEIVED FOR A PRESCRIBED
            NAMED "IFPS TEST"

                       PRIORITY:  IMMEDIATE
                           DATE:  10/13/04
                           TIME:  0912
                   PROJECT NAME:  IFPS TEST 4
                   PROJECT TYPE:  PRESCRIBED
              REQUESTING AGENCY:  WFO Billings
            REQUESTING OFFICIAL:  Virgil Middendorf
                            FAX:  (406) 652-3214
                EMERGENCY PHONE:  (406) 652-0851 
                       LOCATION:  
                          STATE:  MT
                           DLAT:  28.27
                           DLON:  82.19
                       EXPOSURE:  N
                      FUEL TYPE:  Grass
                     SHELTERING:  UNSHELTERED
               BOTTOM ELEVATION:  3200
                  TOP ELEVATION:  3200
                   SIZE (ACRES):  1

            WEATHER CONDITIONS AT PROJECT OR FROM NEARBY STATIONS
            SITE ELEV=3200 TIME=0900 WIND=SW10 T=50 TW=45 RH=90 TD= Cloudy
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 

            ...REMARKS...
            TEST

            ...WEATHER PARAMETERS REQUESTED...
                  CLOUDS / WEATHER: 1,1,1
            CHANCE OF WETTING RAIN: 1,1,1
                       TEMPERATURE: 1,1,0
                 RELATIVE HUMIDITY: 1,1,0
                    EYE LEVEL WIND: 1,0,0
                  SMOKE DISPERSION: 1,0,0
                    CLEARING INDEX: 1,1,1
                   STABILITY CLASS: 1,1,1
                      MARINE LAYER: 1,1,1
                      HAINES INDEX: 1,1,1
            
            SITE:  BYZ
            OFILE:  20041013.IFPST.01
            TIMEZONE:  MST7MDT

            \"\"\",
            \"\"\"
            BMBB91 KBYZ 131505
            STQBYZ

            A SPOT FORECAST REQUEST HAS JUST BEEN RECEIVED FOR A PRESCRIBED
            NAMED "IFPS TEST"

                       PRIORITY:  IMMEDIATE
                           DATE:  10/13/04
                           TIME:  0912
                   PROJECT NAME:  IFPS TEST 5
                   PROJECT TYPE:  PRESCRIBED
              REQUESTING AGENCY:  WFO Billings
            REQUESTING OFFICIAL:  Virgil Middendorf
                            FAX:  (406) 652-3214
                EMERGENCY PHONE:  (406) 652-0851 
                       LOCATION:  
                          STATE:  MT
                           DLAT:  28.27
                           DLON:  82.19
                       EXPOSURE:  N
                      FUEL TYPE:  Grass
                     SHELTERING:  UNSHELTERED
               BOTTOM ELEVATION:  3200
                  TOP ELEVATION:  3200
                   SIZE (ACRES):  1

            WEATHER CONDITIONS AT PROJECT OR FROM NEARBY STATIONS
            SITE ELEV=3200 TIME=0900 WIND=SW10 T=50 TW=45 RH=90 TD= Cloudy
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 

            ...REMARKS...
            TEST

            ...WEATHER PARAMETERS REQUESTED...
                  CLOUDS / WEATHER: 1,1,1
            CHANCE OF WETTING RAIN: 1,1,1
                       TEMPERATURE: 1,1,0
                 RELATIVE HUMIDITY: 1,1,0
                    EYE LEVEL WIND: 1,0,0
                  SMOKE DISPERSION: 1,0,0
                    CLEARING INDEX: 1,1,0
                   STABILITY CLASS: 1,1,0
                      MARINE LAYER: 1,1,0
                      HAINES INDEX: 1,1,0

            SITE:  BYZ
            OFILE:  20041013.IFPST.01
            TIMEZONE:  MST7MDT

            \"\"\",
            \"\"\"
            BMBB91 KBYZ 131506
            STQBYZ

            A SPOT FORECAST REQUEST HAS JUST BEEN RECEIVED FOR A PRESCRIBED
            NAMED "IFPS TEST"

                       PRIORITY:  IMMEDIATE
                           DATE:  10/13/04
                           TIME:  0912
                   PROJECT NAME:  IFPS TEST 6
                   PROJECT TYPE:  PRESCRIBED
              REQUESTING AGENCY:  WFO Billings
            REQUESTING OFFICIAL:  Virgil Middendorf
                            FAX:  (406) 652-3214
                EMERGENCY PHONE:  (406) 652-0851 
                       LOCATION:  
                          STATE:  MT
                           DLAT:  28.27
                           DLON:  82.19
                       EXPOSURE:  N
                      FUEL TYPE:  Grass
                     SHELTERING:  UNSHELTERED
               BOTTOM ELEVATION:  3200
                  TOP ELEVATION:  3200
                   SIZE (ACRES):  1

            WEATHER CONDITIONS AT PROJECT OR FROM NEARBY STATIONS
            SITE ELEV=3200 TIME=0900 WIND=SW10 T=50 TW=45 RH=90 TD= Cloudy
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 

            ...REMARKS...
            TEST

            ...WEATHER PARAMETERS REQUESTED...
                  CLOUDS / WEATHER: 1,1,1
            CHANCE OF WETTING RAIN: 1,1,1
                       TEMPERATURE: 1,1,0
                 RELATIVE HUMIDITY: 1,1,0
                    EYE LEVEL WIND: 1,0,0
                  SMOKE DISPERSION: 1,0,0
                    CLEARING INDEX: 1,0,0
                   STABILITY CLASS: 1,0,0
                      MARINE LAYER: 1,0,0
                      HAINES INDEX: 1,0,0

            SITE:  BYZ
            OFILE:  20041013.IFPST.01
            TIMEZONE:  MST7MDT

            \"\"\",
            \"\"\"
            BMBB91 KBYZ 131507
            STQBYZ

            A SPOT FORECAST REQUEST HAS JUST BEEN RECEIVED FOR A PRESCRIBED
            NAMED "IFPS TEST"

                       PRIORITY:  IMMEDIATE
                           DATE:  10/13/04
                           TIME:  0912
                   PROJECT NAME:  IFPS TEST 7
                   PROJECT TYPE:  PRESCRIBED
              REQUESTING AGENCY:  WFO Billings
            REQUESTING OFFICIAL:  Virgil Middendorf
                            FAX:  (406) 652-3214
                EMERGENCY PHONE:  (406) 652-0851 
                       LOCATION:  
                          STATE:  MT
                           DLAT:  28.27
                           DLON:  82.19
                       EXPOSURE:  N
                      FUEL TYPE:  Grass
                     SHELTERING:  UNSHELTERED
               BOTTOM ELEVATION:  3200
                  TOP ELEVATION:  3200
                   SIZE (ACRES):  1

            WEATHER CONDITIONS AT PROJECT OR FROM NEARBY STATIONS
            SITE ELEV=3200 TIME=0900 WIND=SW10 T=50 TW=45 RH=90 TD= Cloudy
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 

            ...REMARKS...
            TEST

            ...WEATHER PARAMETERS REQUESTED...
                  CLOUDS / WEATHER: 1,1,1
            CHANCE OF WETTING RAIN: 1,1,1
                       TEMPERATURE: 1,1,0
                 RELATIVE HUMIDITY: 1,1,0
                    EYE LEVEL WIND: 1,0,0
                  SMOKE DISPERSION: 1,0,0
                    CLEARING INDEX: 1,0,1
                   STABILITY CLASS: 1,0,1
                      MARINE LAYER: 1,0,1
                      HAINES INDEX: 1,0,1

            SITE:  BYZ
            OFILE:  20041013.IFPST.01
            TIMEZONE:  MST7MDT
            \"\"\",
            \"\"\"
            BMBB91 KBYZ 131508
            STQBYZ

            A SPOT FORECAST REQUEST HAS JUST BEEN RECEIVED FOR A PRESCRIBED
            NAMED "IFPS TEST"

                       PRIORITY:  IMMEDIATE
                           DATE:  10/13/04
                           TIME:  0912
                   PROJECT NAME:  IFPS TEST 8
                   PROJECT TYPE:  PRESCRIBED
              REQUESTING AGENCY:  WFO Billings
            REQUESTING OFFICIAL:  Virgil Middendorf
                            FAX:  (406) 652-3214
                EMERGENCY PHONE:  (406) 652-0851 
                       LOCATION:  
                          STATE:  MT
                           DLAT:  28.27
                           DLON:  82.19
                       EXPOSURE:  N
                      FUEL TYPE:  Grass
                     SHELTERING:  UNSHELTERED
               BOTTOM ELEVATION:  3200
                  TOP ELEVATION:  3200
                   SIZE (ACRES):  1

            WEATHER CONDITIONS AT PROJECT OR FROM NEARBY STATIONS
            SITE ELEV=3200 TIME=0900 WIND=SW10 T=50 TW=45 RH=90 TD= Cloudy
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 

            ...REMARKS...
            TEST

            ...WEATHER PARAMETERS REQUESTED...
                  CLOUDS / WEATHER: 1,1,0
            CHANCE OF WETTING RAIN: 1,1,0
                       TEMPERATURE: 1,1,0
                 RELATIVE HUMIDITY: 1,1,0
                    EYE LEVEL WIND: 1,0,0
                  SMOKE DISPERSION: 1,0,0

            SITE:  BYZ
            OFILE:  20041013.IFPST.01
            TIMEZONE:  MST7MDT
            \"\"\",
            \"\"\"
            BMBB91 KBYZ 131509
            STQBYZ

            A SPOT FORECAST REQUEST HAS JUST BEEN RECEIVED FOR A PRESCRIBED
            NAMED "IFPS TEST"

                       PRIORITY:  IMMEDIATE
                           DATE:  10/13/04
                           TIME:  0912
                   PROJECT NAME:  IFPS TEST 9
                   PROJECT TYPE:  PRESCRIBED
              REQUESTING AGENCY:  WFO Billings
            REQUESTING OFFICIAL:  Virgil Middendorf
                            FAX:  (406) 652-3214
                EMERGENCY PHONE:  (406) 652-0851 
                       LOCATION:  
                          STATE:  MT
                           DLAT:  28.27
                           DLON:  82.19
                       EXPOSURE:  N
                      FUEL TYPE:  Grass
                     SHELTERING:  UNSHELTERED
               BOTTOM ELEVATION:  3200
                  TOP ELEVATION:  3200
                   SIZE (ACRES):  1

            WEATHER CONDITIONS AT PROJECT OR FROM NEARBY STATIONS
            SITE ELEV=3200 TIME=0900 WIND=SW10 T=50 TW=45 RH=90 TD= Cloudy
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 
             ELEV= TIME= WIND= T= TW= RH= TD= 

            ...REMARKS...
            RIDGE TOP WIND: 10-20 W/ G30.

            ...WEATHER PARAMETERS REQUESTED...
                  CLOUDS / WEATHER: 0,1,1
            CHANCE OF WETTING RAIN: 0,1,1
                       TEMPERATURE: 0,1,0
                 RELATIVE HUMIDITY: 0,1,0
                    EYE LEVEL WIND: 0,0,0
                  SMOKE DISPERSION: 0,0,0

            SITE:  BYZ
            OFILE:  20041013.IFPST.01
            TIMEZONE:  MST7MDT
            \"\"\",   

            ]
        return products

"""
    
    
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
        "VAC910c":("area1", "The Roanoke Area."),
        "NCC940c":("area3",
                   "The northern foothills and mountains of North Carolina."),
        }

# summaryExtendedIntro is a string to introduce the extended
# such as "The extended forecast for"
Definition["summaryExtendedIntro"] = "Now for the Extended Forecast for"

"""

testOverrides = """ 

    def _weInfoList(self):
        # This is the list of possible weather parameters listed under the
        # ...WEATHER PARAMETERS REQUESTED... section in your STQ Product.
        # These are listed in the order they will appear in the product.
        #
        # Weather Elements: If you have a weather element to add,
        # then send an email to Virgil.Middendorf@noaa.gov with your addition.
        # I will baseline it.
        #
        # Phrases: You can override this method and edit the phrase method if you
        # don't like the one used in baseline.
      
        # For each element, we list:
        #     --an identifier
        #     --flag to indicate if this is a default element
        #     --the FWF phrase (or list of phrases) to include in the product
        #     --a list of search strings that must appear in
        #       the STQ product to specify the element.
        #       Each search string in the list may be a tuple in which case any of
        #       the entries in the tuple will satsify the search.

        if self._useRH:
            dayRH = "RH"
            nightRH = "RH"
        else:
            dayRH = "MinRH"
            nightRH = "MaxRH"
        if self._wind20ftHeader:
            wind = [self.fireWind_label_phrase, self.fireWind_compoundPhrase]
        else:
            wind = [self.fireWind_compoundPhrase]
        return [
            ("SKY/WEATHER",     1, self.skyWeather_byTimeRange_compoundPhrase,
             [("SKY", "CLOUDS"), "WEATHER"]),
            ("BEGIN/END OF PCPN", 1, self.pcpnTiming_phrase,
             ["BEGIN", "END", "PCPN"]),
            ("TEMPERATURE",      1, (self.dayOrNight_phrase, ["MaxT", "MinT", 1, 1]),
             [("TEMPERATURE", "TEMP")]),  
            ("HUMIDITY",         1, (self.dayOrNight_phrase, [dayRH, nightRH, 1, 1]),
             [("RH", "HUMIDITY")]),
            ("DEWPOINT",        1, self.td_phrase,  
             ["DEWPOINT"]),
            ("20 FOOT WINDS",   1, wind,            
             ["20", "WIND", ("FT", "FOOT")]),
            ("EYE LEVEL WINDS",  1, self.fireEyeWind_compoundPhrase,
             [("EYE","10"), "WIND"]),
            ("WIND SHIFT",      1, self.fireWindShift_label_phrase,  
             ["WIND", "SHIFT"]),
            ("RIDGE TOP WIND",  1, self.freeWind_phrase,  
             ["WIND", "RIDGE", "TOP"]),
            ("SURROUNDING RIDGE", 1,  self.surroundingRidgeWind_phrase,  
             ["SURROUNDING", "RIDGE", "WIND"]),
            ("CWR",              1, self.cwr_phrase,  
             [("CWR", "WETTING RAIN")]),
            ("POP", 1, self.pop_phrase,  
             [("PRECIPITATION", "CHANCE OF PCPN", "POP")]),
            ("LIGHTNING ACTIVITY LEVEL", 1, self.lal_phrase,
             [("LAL", "LIGHTNING")]),
            ("SMOKE DISPERSION", 1, [self.mixingHgt_phrase, self.transportWind_phrase],
             [("SMOKE", "DISPERSION")]),              
            ("MIXING HEIGHT",   1, self.mixingHgt_phrase,
             ["MIXING"]),
            ("TRANSPORT WINDS", 1, self.transportWind_phrase,
             ["TRANSPORT", "WIND"]),
            ("LDSI", 1, self.ldsi_phrase,
             ["LDSI"]),
            ("LVORI", 1, self.lvori_phrase,
             ["LVORI"]),
            ("DISPERSION INDEX",  1, self.dsi_phrase,
             ["DISPERSION", "INDEX"]),
            ("CLEARING INDEX",  1, self.smokeDispersal_phrase,
             ["CLEARING", "INDEX"]),
            ("STABILITY CLASS", 1, self.stabilityClass_phrase,
             ["STABILITY"]),
            ("MARINE LAYER",    1, self.marineLayer_phrase,
             ["MARINE", "LAYER"]),
            ("HAINES INDEX",    1, self.haines_phrase,
             ["HAINES", "INDEX"]),
            ]

    def _rowList(self, colWidth=1):
        if self._tableWindElementSplit == "no" and colWidth == 7: # 2 hourly
            wind = [("20 FT WIND......", self._wind_value),
                    ("20 FT WIND GUST.", self._windGust_value)]
        elif self._tableWindElementSplit == "no" and colWidth > 7: # 3-4 hourly
            wind = [("20 FT WIND......", self._windWithGust_value)]
        else:
            wind = [("20 FT WIND DIR..", self._windDir_value),  # 1 hourly
                    ("20 FT WIND SPD..", self._windSpd_value),
                    ("20 FT WIND GUST.", self._windGust_value)]
        if self._tableEyeWindElementSplit == "no" and colWidth == 7:
            eyewind = [("EYE LEVEL WIND..", self._eyewind_value),
                       ("EYE LEVEL WIND..", self._eyewindGust_value)]
        elif self._tableEyeWindElementSplit == "no" and colWidth > 7:
            eyewind = [("EYE LEVEL WIND..", self._eyewindWithGust_value)]
        else:
            eyewind = [("EYE LVL WND DIR.", self._eyewindDir_value),
                       ("EYE LVL WND SPD.", self._eyewindSpd_value),
                       ("EYE LVL WND GST.", self._eyewindGust_value)]
        if self._tableRidgeElementSplit == "no" and colWidth >=7:
            ridge = [("RIDGETOP WIND...", self._ridge_value)]
        else:
            ridge = [("RIDGE WIND DIR..", self._ridgeDir_value),
                     ("RIDGE WIND SPD..", self._ridgeSpd_value)]

        # Mixing Height and Transport winds

        if self._tabularMixingHeightUnits == "ft" and colWidth > 4:
            mixLabel = "MIX HGT (FT)...."
            mixMetricLabel = "MIX HGT (M)....."                
        else:
            mixLabel = "MIX HGT (KFT)..."
            mixMetricLabel = "MIX HGT (KM)...."
            
        if self._transportWindLabel == "mix":
            transLabel = "MIXNG WIND......"
            transMetricLabel = "MIX WIND (M/S).."
            transDirLabel = "MIXNG WIND DIR.."
            transSpdLabel = "MIXNG WIND SPD.."
            transSpdMetricLabel = "MIX WND SPD M/S."
        else:
            transLabel = "TRANSPORT WIND.."
            transMetricLabel = "TRAN WIND (M/S)."
            transDirLabel = "TRANSP WIND DIR."
            transSpdLabel = "TRANSP WIND SPD."
            transSpdMetricLabel = "TRANS SPD (M/S)."
                            
        if self._tableTransElementSplit == "no" and colWidth >=7:
            # Baseline
            if self._includeMetricDispersion == "yes":
                smoke = [(mixLabel, self._mixingHeight_value),
                         (mixMetricLabel, self._mixingHeightMetric_value),
                         (transLabel, self._trans_value),
                         (transMetricLabel, self._transMetric_value)]
                trans = [(transLabel, self._trans_value),
                         (transMetricLabel, self._transMetric_value)]
            else:
                smoke = [(mixLabel, self._mixingHeight_value),
                         (transLabel, self._trans_value)]
                trans = [(transLabel, self._trans_value)]
        else:
            # Baseline
            if self._includeMetricDispersion == "yes":
                smoke = [(mixLabel, self._mixingHeight_value),
                         (mixMetricLabel, self._mixingHeightMetric_value),
                         (transDirLabel, self._transDir_value),
                         (transSpdLabel, self._transSpd_value),
                         (transSpdMetricLabel, self._transSpdMetric_value)]
                trans = [(transDirLabel, self._transDir_value),
                         (transSpdLabel, self._transSpd_value),
                         (transSpdMetricLabel, self._transSpdMetric_value)]
            else:
                smoke = [(mixLabel, self._mixingHeight_value),
                         (transDirLabel, self._transDir_value),
                         (transSpdLabel, self._transSpd_value)]
                trans = [(transDirLabel, self._transDir_value),
                         (transSpdLabel, self._transSpd_value)]
        if self._includeMetricDispersion == "yes":
            mix = [(mixLabel, self._mixingHeight_value),
                   (mixMetricLabel, self._mixingHeightMetric_value)]
        else:
            mix = [(mixLabel, self._mixingHeight_value)]
            
        return [
            # Set to Directive requirements
            # Each entry is a tuple:
            #  (Narrative Element, narrativeToo, tableRows)
            #  
            #  If narrativeToo is 1, then the narrative phrase will be included
            #     in the narrative portion of the product as well.
            #  tableRows is a list of (label:method) pairs.
            #
            ("SKY/WEATHER"             , 1,
             [("SKY (%).........", self._sky_value),
              ("WEATHER COV.....", self._weatherCov_value),
              ("WEATHER TYPE....", self._weatherType_value),
              ]),
            ("TEMPERATURE"             , 1,[("TEMP............", self._temp_value)]),
            ("HUMIDITY"                , 1,[("RH..............", self._rh_value)]),
            ("20 FOOT WINDS"           , 1, wind),
            ("EYE LEVEL WINDS"         , 1, eyewind),
            ("RIDGE TOP WIND"          , 1, ridge),
            ("SMOKE DISPERSION"        , 1, smoke),
            ("MIXING HEIGHT"           , 1, mix),
            ("TRANSPORT WINDS"         , 1, trans),
            ("DISPERSION INDEX"        , 1,[("DISPERSION......", self._dsi_value)]),
            ("LDSI"                    , 1,[("DISPERSION IDX..", self._ldsi_value)]),
            ("LVORI"                   , 1,[("LVORI...........", self._lvori_value)]),
            ("CWR"                     , 1,[("CWR.............", self._cwr_value)]),
            ("POP"                     , 1,[("CHC OF PCPN (%).", self._pop_value)]),
            ("LIGHTNING ACTIVITY LEVEL", 1,[("LAL.............", self._lal_value)]),
            ("HAINES INDEX"            , 1,[("HAINES INDEX....", self._haines_value)]),
            ]

    def _ldsi_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        dsi = self._getTableStats(tree, "DSI", timeRange, node.getAreaLabel())
        #dsi = self._getTableStats(tree, "LDSI", timeRange, node.getAreaLabel())
        if dsi is None:
            return "M"        
        return `int(dsi + 0.5)`

    def _lvori_value(self, statDict, timeRange, argList):
        tree, node, colWidth = tuple(argList)        
        lvori = self._getTableStats(tree, "DSI", timeRange, node.getAreaLabel())
        #lvori = self._getTableStats(tree, "LVORI", timeRange, node.getAreaLabel())
        if lvori is None:
            return "M"        
        return `int(lvori + 0.5)`
    
    def ldsi_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("DSI", "List")]
        #elementInfoList = [self.ElementInfo("LDSI", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "LDSI................")
        return self.DONE()

    def ldsi_words(self, tree, node):
        "Create phrase Probability of Precipitation"
        statDict = node.getStatDict()
        ldsi = self.getStats(statDict, "DSI")
        #ldsi = self.getStats(statDict, "LDSI")
        if ldsi is None:
            return self.setWords(node.parent, "MISSING")
        ldsi = self.getValue(ldsi)
        words =  `int(ldsi + 0.5)` 
        return self.setWords(node, words)
            

    def lvori_setUp(self, tree, node):
        elementInfoList = [self.ElementInfo("DSI", "List")]
        #elementInfoList = [self.ElementInfo("LVORI", "List")]
        self.subPhraseSetUp(tree, node, elementInfoList, self.scalarConnector) 
        node.set("descriptor", "")
        node.set("indentLabel", "LVORI...............")
        return self.DONE()      

    def lvori_words(self, tree, node):
        statDict = node.getStatDict()
        #lvori = self.getStats(statDict, "LVORI")
        lvori = self.getStats(statDict, "DSI")
        if lvori is None:
            return self.setWords(node.parent, "MISSING")
        lvori = self.getValue(lvori)
        words =  `int(lvori + 0.5)` 
        return self.setWords(node, words)

    
"""

definitions = """

Definition["useRH"] = 1
Definition["tabularAllPeriods"] = 'no'
Definition["shortTermOnly"] = 0
Definition["tempLocalEffects"] = 1
Definition["windLocalEffects"] = 1
Definition["includeCreationTimeOnGUI"] = 0
Definition["withIgnitionTimes"] = "no"

Definition["tableStartTimeOffset"] = 0
Definition["elementFormatDict"] = {
    "Sky" : "alpha",       #"numeric",
    "Wind": "numeric",     #"alpha",
    "EyeWind": "numeric",  #"alpha",
    "RidgeWind": "numeric",#"alpha",
    "TransWind": "numeric",#"alpha",
    "TransMetWind": "numeric",#"alpha",
    }

Definition["tabularMixingHeightUnits"] = "kft"   
Definition["transportWindLabel"] = "mix"
Definition["includeMetricDispersion"] = "yes"
Definition["20ftWindParm"] = "Wind20ft"
Definition["wind20ftHeader"] = 1  
Definition["tableWindElementSplit"] = "yes"
Definition["tableEyeWindElementSplit"] = "yes"
Definition["tableRidgeElementSplit"] = "yes"
Definition["tableTransElementSplit"] = "yes"
Definition["cwrParm"] = "CWR"

"""

definition1 = """
Definition["tableStartTimeMode"] = "current"
"""
definition2 = """
Definition["tableStartTimeMode"] = "productStart"
"""
definition0 = """
Definition["cwrParm"] = "CWR"
"""
definition3 = """
Definition["elementFormatDict"]["Sky"] = "alpha"
"""
definition4 = """
Definition["agencyList"] = [
    (1,"Agency 1"),
    (2,"WFO Billings"),
    ]
"""

scripts = [
    {    
    "name":"FWS_Init",
    "commentary": "Clean out grids",
    "productType": None,
    "deleteGrids": [
        ("Fcst", "PoP", "SFC", -24, 240),
        ("Fcst", "MaxT", "SFC", -24, 280),
        ("Fcst", "MinT", "SFC", -24, 240),
        ("Fcst", "T", "SFC", -24, 240),
        ("Fcst", "Td", "SFC", -24, 240),
        ("Fcst", "WindChill", "SFC", -24, 240),
        ("Fcst", "HeatIndex", "SFC", -24, 240),
        ("Fcst", "Wind", "SFC", -24, 240),
        ("Fcst", "Sky", "SFC", -24, 240),
        ("Fcst", "WindGust", "SFC", -24, 240),
        ("Fcst", "Wx", "SFC", -24, 240),
        ("Fcst", "QPF", "SFC", -24, 240),
        ("Fcst", "SnowAmt", "SFC", -24, 240),
        ("Fcst", "MaxRH", "SFC", -24, 240),
        ("Fcst", "MinRH", "SFC", -24, 280),
        ("Fcst", "TransWind", "SFC", -24, 240),
        ("Fcst", "FreeWind", "SFC", -24, 240),
        ("Fcst", "LAL", "SFC", -24, 240),
        ("Fcst", "CWR", "SFC", -24, 240),
        ("Fcst", "Haines", "SFC", -24, 240),
        ("Fcst", "MixHgt", "SFC", -24, 240),
        ("Fcst", "Wind20ft", "SFC", -24, 240),
        ("Fcst", "VentRate", "SFC", -24, 240),
        ("Fcst", "MarineLayer", "SFC", -24, 240),
        ("Fcst", "Stability", "SFC", -24, 240),
        ("Fcst", "HrsOfSun", "SFC", -24, 240),
        ("Fcst", "DSI", "SFC", -24, 240),
        ("Fcst", "RHtrend", "SFC", -24, 240),
        ("Fcst", "Ttrend", "SFC", -24, 240),
        ]
    },

    {
    "name":"FWS_1",
    "productType":"FWS",
    "commentary": "Morning Issuance",
    "createGrids": FWS_createGrids,
    "fileChanges":[
       ("FWS_<site>_Overrides", "TextUtility", "add", testOverrides, "undo"),
       ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
     ],
     "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 3, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",

    "checkStrings": [
        "FNUS72 KTBW 010900",
        "FWSTBW",

        "Spot Forecast for xxxx...Agency 1",
        "National Weather Service Tampa Bay Ruskin FL",
        "400 AM EST Fri Jan 1 2010",
        "Forecast is based on ignition time of 1300 EST on January 01.",
        "If conditions become unrepresentative...contact the National Weather Service.",

        ".DISCUSSION...",

        ".TODAY...",
        "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........100 at ignition...Max 78.",
        "RH..................100 percent at ignition...Min 65 percent.",
        "DEWPOINT............100.",
        "WIND (20 FT)........Winds southwest at 9 mph at ignition...otherwise North winds around 6 mph with gusts to around 30 mph.",
        "EYE LEVEL WINDS.....North winds around 6 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......Southwest around 10 mph.",
        "SURROUNDING RIDGE...Southwest around 10 mph.",
        "CWR.................0 percent.",
        "CHANCE OF PCPN......0 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL at ignition...otherwise 100 ft AGL.",
        "TRANSPORT WINDS.....Southwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL at ignition...otherwise 100 ft AGL.",
        "TRANSPORT WINDS.....Southwest around 12 mph.",
        "DISPERSION INDEX....0.",
        "LVORI...............0.",
        "DISPERSION..........0. ",
        "SMOKE DISPERSAL.....Excellent /160000 knot-ft/ at ignition. Max...excellent /160000 knot-ft/.",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........1000.",
        "HAINES INDEX........2 or very low potential for large plume dominated fire growth at ignition...max 2.",

        "TIME (EST)      1PM 2PM 3PM 4PM 5PM",
        "SKY (%).........100 100 100 100 100",
        "WEATHER COV.....PTY PTY PTY PTY PTY",
        "WEATHER TYPE....FOG FOG FOG FOG FOG ",
        "TEMP............100 100 100 100 100",
        "RH..............100 100 100 100 100",
        "20 FT WIND DIR..SW  SW  SW  SW  SW ",
        "20 FT WIND SPD..7   7   7   7   7 ",
        "20 FT WIND GUST.30  30  30  30  30 ",
        "EYE LVL WND DIR.SW  SW  SW  SW  SW",
        "EYE LVL WND SPD.7   7   7   7   7 ",
        "EYE LVL WND GST.30  30  30  30  30 ",
        "RIDGE WIND DIR..SW  SW  SW  SW  SW",
        "RIDGE WIND SPD..10  10  10  10  10",
        "MIX HGT (KFT)...0.1 0.1 0.1 0.1 0.1 ",
        "TRANSP WIND DIR.SW  SW  SW  SW  SW ",
        "TRANSP WIND SPD.12  12  12  12  12",
        "MIX HGT (KFT)...0.1 0.1 0.1 0.1 0.1 ",
        "TRANSP WIND DIR.SW  SW  SW  SW  SW",
        "TRANSP WIND SPD.12  12  12  12  12",
        "DISPERSION......0   0   0   0   0 ",
        "DISPERSION IDX..0   0   0   0   0 ",
        "LVORI...........0   0   0   0   0 ",
        "CWR.............0   0   0   0   0",
        "CHC OF PCPN (%).0   0   0   0   0",
        "LAL.............1   1   1   1   1",
        "HAINES INDEX....2   2   2   2   2",

        ".TONIGHT...",

        "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Min 60.",
        "RH..................Max 78 percent.",
        "DEWPOINT............95.",
        "WIND (20 FT)........Very windy. Northeast winds around 46 mph.",
        "EYE LEVEL WINDS.....Very windy. Northeast winds around 46 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......West around 5 mph.",
        "SURROUNDING RIDGE...West around 5 mph.",
        "CWR.................20 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................2.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....West around 6 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....West around 6 mph.",
        "DISPERSION INDEX....2.",
        "LVORI...............2.",
        "DISPERSION..........2.",
        "SMOKE DISPERSAL.....Excellent (100000 knot-ft).",
        "STABILITY CLASS.....2.",
        "MARINE LAYER........1000.",
        "HAINES INDEX........3 or very low potential for large plume dominated fire growth.",

        "TIME (EST)      6 PM   8 PM   10 PM  MIDNGT 2 AM   4 AM",
        "SKY (%).........95     95     95     95     95     95",
        "WEATHER COV.....WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD ",
        "WEATHER TYPE....TSTORM TSTORM TSTORM TSTORM TSTORM TSTORM ",
        "TEMP............95     95     95     95     95     95",
        "RH..............95     95     95     95     95     95",
        "20 FT WIND......SE 30  SE 30  SE 30  SE 30  SE 30  SE 30 ",
        "20 FT WIND GUST.",
        "EYE LEVEL WIND..SE 30  SE 30  SE 30  SE 30  SE 30  SE 30",
        "EYE LEVEL WIND..",
        "RIDGETOP WIND...W 5    W 5    W 5    W 5    W 5    W 5",
        "MIX HGT (FT)....100    100    100    100    100    100",
        "TRANSPORT WIND..W 6    W 6    W 6    W 6    W 6    W 6",
        "MIX HGT (FT)....100    100    100    100    100    100",
        "TRANSPORT WIND..W 6    W 6    W 6    W 6    W 6    W 6",
        "DISPERSION......2      2      2      2      2      2",
        "DISPERSION IDX..2      2      2      2      2      2",
        "LVORI...........2      2      2      2      2      2",
        "CWR.............20     20     20     20     20     20",
        "CHC OF PCPN (%).90     90     90     90     90     90",
        "LAL.............2      2      2      2      2      2",
        "HAINES INDEX....3      3      3      3      3      3",

        ".SATURDAY...",

        "SKY/WEATHER.........Sunny (0-5 percent). Chance of rain showers.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Max 79.",
        "RH..................Min 68 percent.",
        "DEWPOINT............0.",
        "WIND (20 FT)........Northwest winds around 12 mph.",
        "EYE LEVEL WINDS.....Northwest winds around 12 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......Northwest around 10 mph.",
        "SURROUNDING RIDGE...Northwest around 10 mph.",
        "CWR.................30 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Northwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Northwest around 12 mph.",
        "DISPERSION INDEX....6.",
        "LVORI...............6.",
        "DISPERSION..........6.",
        "SMOKE DISPERSAL.....Good (50000 knot-ft).",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........2000.",
        "HAINES INDEX........4 or low potential for large plume dominated fire growth.",

        "TIME (EST)      6 AM      9 AM      NOON      3 PM",
        "SKY (%).........0         0         0         0 ",
        "WEATHER COV.....CHANCE    CHANCE    CHANCE    CHANCE ",
        "WEATHER TYPE....RNSHWR    RNSHWR    RNSHWR    RNSHWR",
        "TEMP............0         0         0         0",
        "RH..............0         0         0         0",
        "20 FT WIND......NW 26     NW 26     NW 26     NW 26",
        "EYE LEVEL WIND..NW 26     NW 26     NW 26     NW 26",
        "RIDGETOP WIND...NW 10     NW 10     NW 10     NW 10",
        "MIX HGT (FT)....100       100       100       100",
        "TRANSPORT WIND..NW 12     NW 12     NW 12     NW 12",
        "MIX HGT (FT)....100       100       100       100",
        "TRANSPORT WIND..NW 12     NW 12     NW 12     NW 12",
        "DISPERSION......6         6         6         6 ",
        "DISPERSION IDX..6         6         6         6",
        "LVORI...........6         6         6         6 ",
        "CWR.............30        30        30        30",
        "CHC OF PCPN (%).90        90        90        90",
        "LAL.............3         3         3         3 ",
        "HAINES INDEX....4         4         4         4",

        "$$",
        "FORECASTER...Forecaster C",
        "REQUESTED BY...YYYY",
        "TYPE OF REQUEST...Prescribed",
        ]
    },

    {    
    "name":"FWS_2",
    "productType":"FWS",
    "commentary": "Morning Update",
    "fileChanges":[
       ("FWS_<site>_Overrides", "TextUtility", "add", testOverrides, "undo"),
       ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
     ],
     "createGrids": FWS_createGrids,
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Morning Update', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 2, ('Tab Hrs', 'tonightTableRes'): 3, ('Tab Hrs', 'tomorrowTableRes'): 4, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",
    "drtHour": 10,
    "checkStrings": [
        "Forecast is based on ignition time of 1300 EST on January 01.",
        "If conditions become unrepresentative...contact the National Weather Service.",
        ".REST OF TODAY...",

        "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........100 at ignition...Max 78.",
        "RH..................100 percent at ignition...Min 65 percent.",
        "DEWPOINT............100.",
        "WIND (20 FT)........Winds southwest at 9 mph at ignition...otherwise North winds around 6 mph with gusts to around 30 mph.",
        "EYE LEVEL WINDS.....North winds around 6 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......Southwest around 10 mph.",
        "SURROUNDING RIDGE...Southwest around 10 mph.",
        "CWR.................0 percent.",
        "CHANCE OF PCPN......0 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL at ignition...otherwise 100 ft AGL.",
        "TRANSPORT WINDS.....Southwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL at ignition...otherwise 100 ft AGL.",
        "TRANSPORT WINDS.....Southwest around 12 mph.",
        "DISPERSION INDEX....0.",
        "LVORI...............0.",
        "DISPERSION..........0.",
        "SMOKE DISPERSAL.....Excellent /160000 knot-ft/ at ignition. Max...excellent /160000 knot-ft/.",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........1000.",
        "HAINES INDEX........2 or very low potential for large plume dominated fire growth at ignition...max 2.",

        "TIME (EST)      1 PM   3 PM   5 PM",
        "SKY (%).........100    100    100",
        "WEATHER COV.....PATCHY PATCHY PATCHY",
        "WEATHER TYPE....FOG    FOG    FOG",
        "TEMP............100    100    100",
        "RH..............100    100    100",
        "20 FT WIND......SW 7   SW 7   SW 7",
        "20 FT WIND GUST.30     30     30",
        "EYE LEVEL WIND..SW 7   SW 7   SW 7",
        "EYE LEVEL WIND..30     30     30",
        "RIDGETOP WIND...SW 10  SW 10  SW 10",
        "MIX HGT (FT)....100    100    100",
        "TRANSPORT WIND..SW 12  SW 12  SW 12",
        "MIX HGT (FT)....100    100    100",
        "TRANSPORT WIND..SW 12  SW 12  SW 12",
        "DISPERSION......0      0      0",
        "DISPERSION IDX..0      0      0",
        "LVORI...........0      0      0",
        "CWR.............0      0      0",
        "CHC OF PCPN (%).0      0      0",
        "LAL.............1      1      1",
        "HAINES INDEX....2      2      2",

        ".TONIGHT...",

        "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Min 60.",
        "RH..................Max 78 percent.",
        "DEWPOINT............95.",
        "WIND (20 FT)........Very windy. Northeast winds around 46 mph.",
        "EYE LEVEL WINDS.....Very windy. Northeast winds around 46 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......West around 5 mph.",
        "SURROUNDING RIDGE...West around 5 mph.",
        "CWR.................20 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................2.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....West around 6 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....West around 6 mph.",
        "DISPERSION INDEX....2.",
        "LVORI...............2.",
        "DISPERSION..........2.",
        "SMOKE DISPERSAL.....Excellent (100000 KNOT-ft).",
        "STABILITY CLASS.....2.",
        "MARINE LAYER........1000.",
        "HAINES INDEX........3 or very low potential for large plume dominated fire growth.",

        "TIME (EST)      6 PM      9 PM      MIDNGT    3 AM",
        "SKY (%).........95        95        95        95",
        "WEATHER COV.....WIDSPD    WIDSPD    WIDSPD    WIDSPD",
        "WEATHER TYPE....TSTORM    TSTORM    TSTORM    TSTORM",
        "TEMP............95        95        95        95",
        "RH..............95        95        95        95",
        "20 FT WIND......SE 30     SE 30     SE 30     SE 30",
        "EYE LEVEL WIND..SE 30     SE 30     SE 30     SE 30",
        "RIDGETOP WIND...W 5       W 5       W 5       W 5",
        "MIX HGT (FT)....100       100       100       100",
        "TRANSPORT WIND..W 6       W 6       W 6       W 6",
        "MIX HGT (FT)....100       100       100       100",
        "TRANSPORT WIND..W 6       W 6       W 6       W 6",
        "DISPERSION......2         2         2         2",
        "DISPERSION IDX..2         2         2         2",
        "LVORI...........2         2         2         2",
        "CWR.............20        20        20        20",
        "CHC OF PCPN (%).90        90        90        90",
        "LAL.............2         2         2         2",
        "HAINES INDEX....3         3         3         3",

        ".SATURDAY...",

        "SKY/WEATHER.........Sunny (0-5 percent). Chance of rain showers.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Max 79.",
        "RH..................Min 68 percent.",
        "DEWPOINT............0.",
        "WIND (20 FT)........Northwest winds around 12 mph.",
        "EYE LEVEL WINDS.....Northwest winds around 12 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......Northwest around 10 mph.",
        "SURROUNDING RIDGE...Northwest around 10 mph.",
        "CWR.................30 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Northwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Northwest around 12 mph.",
        "DISPERSION INDEX....6.",
        "LVORI...............6.",
        "DISPERSION..........6.",
        "SMOKE DISPERSAL.....Good (50000 knot-ft).",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........2000.",
        "HAINES INDEX........4 or low potential for large plume dominated fire growth.",

        "TIME (EST)      6 AM         10 AM        2 PM",
        "SKY (%).........0            0            0",
        "WEATHER COV.....CHANCE       CHANCE       CHANCE",
        "WEATHER TYPE....RNSHWR       RNSHWR       RNSHWR",
        "TEMP............0            0            0",
        "RH..............0            0            0",
        "20 FT WIND......NW 26        NW 26        NW 26",
        "EYE LEVEL WIND..NW 26        NW 26        NW 26",
        "RIDGETOP WIND...NW 10        NW 10        NW 10",
        "MIX HGT (FT)....100          100          100",
        "TRANSPORT WIND..NW 12        NW 12        NW 12",
        "MIX HGT (FT)....100          100          100",
        "TRANSPORT WIND..NW 12        NW 12        NW 12",
        "DISPERSION......6            6            6",
        "DISPERSION IDX..6            6            6",
        "LVORI...........6            6            6",
        "CWR.............30           30           30",
        "CHC OF PCPN (%).90           90           90",
        "LAL.............3            3            3",
        "HAINES INDEX....4            4            4",

        "$$",
        "FORECASTER...Forecaster C",
        "REQUESTED BY...YYYY",
        "TYPE OF REQUEST...Prescribed",
       ],

    },

    {    
    "name":"FWS_3",
    "productType":"FWS",
    "commentary": "Afternoon Issuance",
    "fileChanges":[
       ("FWS_<site>_Overrides", "TextUtility", "add", testOverrides, "undo"),
       ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
     ],
    "createGrids": FWS_createGrids,
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Afternoon', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 3, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",

    "checkStrings": [
        "Forecast is based on ignition time of 1300 EST on January 01.",
        "If conditions become unrepresentative...contact the National Weather Service.",

        ".DISCUSSION...",

        ".TONIGHT...",

        "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Min 60.",
        "RH..................Max 78 percent.",
        "DEWPOINT............95.",
        "WIND (20 FT)........Very windy. Northeast winds around 46 mph.",
        "EYE LEVEL WINDS.....Very windy. Northeast winds around 46 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......West around 5 mph.",
        "SURROUNDING RIDGE...West around 5 mph.",
        "CWR.................20 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................2.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....West around 6 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....West around 6 mph.",
        "DISPERSION INDEX....2.",
        "LVORI...............2.",
        "DISPERSION..........2.",
        "SMOKE DISPERSAL.....Excellent (100000 knot-ft).",
        "STABILITY CLASS.....2.",
        "MARINE LAYER........1000.",
        "HAINES INDEX........3 or very low potential for large plume dominated fire growth.",

        "TIME (EST)      6 PM   8 PM   10 PM  MIDNGT 2 AM   4 AM",
        "SKY (%).........95     95     95     95     95     95",
        "WEATHER COV.....WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD",
        "WEATHER TYPE....TSTORM TSTORM TSTORM TSTORM TSTORM TSTORM",
        "TEMP............95     95     95     95     95     95",
        "RH..............95     95     95     95     95     95",
        "20 FT WIND......SE 30  SE 30  SE 30  SE 30  SE 30  SE 30",
        "20 FT WIND GUST.",
        "EYE LEVEL WIND..SE 30  SE 30  SE 30  SE 30  SE 30  SE 30",
        "EYE LEVEL WIND..",
        "RIDGETOP WIND...W 5    W 5    W 5    W 5    W 5    W 5",
        "MIX HGT (FT)....100    100    100    100    100    100",
        "TRANSPORT WIND..W 6    W 6    W 6    W 6    W 6    W 6",
        "MIX HGT (FT)....100    100    100    100    100    100",
        "TRANSPORT WIND..W 6    W 6    W 6    W 6    W 6    W 6",
        "DISPERSION......2      2      2      2      2      2",
        "DISPERSION IDX..2      2      2      2      2      2",
        "LVORI...........2      2      2      2      2      2",
        "CWR.............20     20     20     20     20     20",
        "CHC OF PCPN (%).90     90     90     90     90     90",
        "LAL.............2      2      2      2      2      2",
        "HAINES INDEX....3      3      3      3      3      3",

        ".SATURDAY...",

        "SKY/WEATHER.........Sunny (0-5 percent). Chance of rain showers.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Max 79.",
        "RH..................Min 68 percent.",
        "DEWPOINT............0.",
        "WIND (20 FT)........Northwest winds around 12 mph.",
        "EYE LEVEL WINDS.....Northwest winds around 12 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......Northwest around 10 mph.",
        "SURROUNDING RIDGE...Northwest around 10 mph.",
        "CWR.................30 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Northwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Northwest around 12 mph.",
        "DISPERSION INDEX....6.",
        "LVORI...............6.",
        "DISPERSION..........6.",
        "SMOKE DISPERSAL.....Good (50000 knot-ft).",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........2000.",
        "HAINES INDEX........4 or low potential for large plume dominated fire growth.",

        "TIME (EST)      6 AM      9 AM      NOON      3 PM",
        "SKY (%).........0         0         0         0",
        "WEATHER COV.....CHANCE    CHANCE    CHANCE    CHANCE",
        "WEATHER TYPE....RNSHWR    RNSHWR    RNSHWR    RNSHWR",
        "TEMP............0         0         0         0",
        "RH..............0         0         0         0",
        "20 FT WIND......NW 26     NW 26     NW 26     NW 26",
        "EYE LEVEL WIND..NW 26     NW 26     NW 26     NW 26",
        "RIDGETOP WIND...NW 10     NW 10     NW 10     NW 10",
        "MIX HGT (FT)....100       100       100       100",
        "TRANSPORT WIND..NW 12     NW 12     NW 12     NW 12",
        "MIX HGT (FT)....100       100       100       100",
        "TRANSPORT WIND..NW 12     NW 12     NW 12     NW 12",
        "DISPERSION......6         6         6         6",
        "DISPERSION IDX..6         6         6         6",
        "LVORI...........6         6         6         6",
        "CWR.............30        30        30        30",
        "CHC OF PCPN (%).90        90        90        90",
        "LAL.............3         3         3         3",
        "HAINES INDEX....4         4         4         4",

        "$$",
        "FORECASTER...Forecaster C",
        "REQUESTED BY...YYYY",
        "TYPE OF REQUEST...Prescribed",
        ],

    },
    {    
    "name":"FWS_4",
    "productType":"FWS",
    "commentary": "Early Morning Update",
    "fileChanges":[
       ("FWS_<site>_Overrides", "TextUtility", "add", testOverrides, "undo"),
       ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
     ],
    "createGrids": FWS_createGrids,
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Early Morning Update', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 3, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",
    "drtHour": 2,
    "checkStrings": [
        "FNUS72 KTBW 010700",
        "FWSTBW",

        "Spot Forecast for xxxx...Agency 1",
        "National Weather Service Tampa Bay Ruskin FL",
        "200 AM EST Fri Jan 1 2010",

        "Forecast is based on ignition time of 1300 EST on January 01.",
        "If conditions become unrepresentative...contact the National Weather Service.",

        ".DISCUSSION...",

        ".REST OF TONIGHT...",

        "SKY/WEATHER.........Patchy dense fog.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Min 40.",
        "RH..................Max 60 percent.",
        "DEWPOINT............MISSING.",
        "WIND (20 FT)........North winds around 6 mph with gusts to around 30 mph.",
        "EYE LEVEL WINDS.....North winds around 6 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......Southwest around 10 mph.",
        "SURROUNDING RIDGE...Southwest around 10 mph.",
        "CWR.................0 percent.",
        "CHANCE OF PCPN......0 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Southwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Southwest around 12 mph.",
        "DISPERSION INDEX....0.",
        "LVORI...............0.",
        "DISPERSION..........0.",
        "SMOKE DISPERSAL.....Excellent (160000 knot-ft).",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........MISSING.",
        "HAINES INDEX........2 or very low potential for large plume dominated fire growth.",

        "TIME (EST)      2 AM   4 AM",
        "SKY (%).........M      M",
        "WEATHER COV.....PATCHY PATCHY",
        "WEATHER TYPE....FOG    FOG",
        "TEMP............M      M ",
        "RH..............M      M ",
        "20 FT WIND......SW 7   SW 7",
        "20 FT WIND GUST.30     30",
        "EYE LEVEL WIND..SW 7   SW 7",
        "EYE LEVEL WIND..30     30",
        "RIDGETOP WIND...SW 10  SW 10",
        "MIX HGT (FT)....100    100",
        "TRANSPORT WIND..SW 12  SW 12",
        "MIX HGT (FT)....100    100",
        "TRANSPORT WIND..SW 12  SW 12",
        "DISPERSION......0      0",
        "DISPERSION IDX..0      0",
        "LVORI...........0      0",
        "CWR.............0      0",
        "CHC OF PCPN (%).0      0",
        "LAL.............1      1",
        "HAINES INDEX....2      2",

        ".FRIDAY...",

        "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........100 at ignition...Max 78.",
        "RH..................100 percent at ignition...Min 65 percent.",
        "DEWPOINT............100.",
        "WIND (20 FT)........Winds southwest at 9 mph at ignition...otherwise North winds around 6 mph with gusts to around 30 mph.",
        "EYE LEVEL WINDS.....North winds around 6 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......Southwest around 10 mph.",
        "SURROUNDING RIDGE...Southwest around 10 mph.",
        "CWR.................0 percent.",
        "CHANCE OF PCPN......0 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL at ignition...otherwise 100 ft AGL.",
        "TRANSPORT WINDS.....Southwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL at ignition...otherwise 100 ft AGL.",
        "TRANSPORT WINDS.....Southwest around 12 mph.",
        "DISPERSION INDEX....0.",
        "LVORI...............0.",
        "DISPERSION..........0.",
        "SMOKE DISPERSAL.....Excellent /160000 knot-ft/ at ignition. Max...excellent /160000 knot-ft/.",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........1000.",
        "HAINES INDEX........2 or very low potential for large plume dominated fire growth at ignition...max 2.",

        "TIME (EST)      6 AM      9 AM      NOON      3 PM ",
        "SKY (%).........100       100       100       100",
        "WEATHER COV.....PATCHY    PATCHY    PATCHY    PATCHY",
        "WEATHER TYPE....FOG       FOG       FOG       FOG",
        "TEMP............100       100       100       100",
        "RH..............100       100       100       100",
        "20 FT WIND......SW 7G30   SW 7G30   SW 7G30   SW 7G30",
        "EYE LEVEL WIND..SW 7G30   SW 7G30   SW 7G30   SW 7G30",
        "RIDGETOP WIND...SW 10     SW 10     SW 10     SW 10",
        "MIX HGT (FT)....100       100       100       100",
        "TRANSPORT WIND..SW 12     SW 12     SW 12     SW 12",
        "MIX HGT (FT)....100       100       100       100",
        "TRANSPORT WIND..SW 12     SW 12     SW 12     SW 12",
        "DISPERSION......0         0         0         0",
        "DISPERSION IDX..0         0         0         0",
        "LVORI...........0         0         0         0",
        "CWR.............0         0         0         0",
        "CHC OF PCPN (%).0         0         0         0",
        "LAL.............1         1         1         1",
        "HAINES INDEX....2         2         2         2",

        "$$",
        "FORECASTER...Forecaster C",
        "REQUESTED BY...YYYY",
        "TYPE OF REQUEST...Prescribed",
       ],

    },
    {    
    "name":"FWS_5",
    "productType":"FWS",
    "commentary": "Next Day Issuance",
    "fileChanges":[
       ("FWS_<site>_Overrides", "TextUtility", "add", testOverrides, "undo"),
       ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
     ],
    "createGrids": FWS_createGrids,
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Next Day', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 3, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",

    "checkStrings": [
        "FNUS72 KTBW 010900",
        "FWSTBW",

        "Spot Forecast for xxxx...Agency 1",
        "National Weather Service Tampa Bay Ruskin FL",
        "400 AM EST Fri Jan 1 2010",

        "Forecast is based on ignition time of 1300 EST on January 01.",
        "If conditions become unrepresentative...contact the National Weather Service.",

        ".DISCUSSION...",

        ".TODAY...",

        "SKY/WEATHER.........Sunny (0-5 percent). Chance of rain showers.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Max 79.",
        "RH..................Min 68 percent.",
        "DEWPOINT............0.",
        "WIND (20 FT)........Northwest winds around 12 mph.",
        "EYE LEVEL WINDS.....Northwest winds around 12 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......Northwest around 10 mph.",
        "SURROUNDING RIDGE...Northwest around 10 mph.",
        "CWR.................30 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Northwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Northwest around 12 mph.",
        "DISPERSION INDEX....6.",
        "LVORI...............6.",
        "DISPERSION..........6.",
        "SMOKE DISPERSAL.....Good (50000 knot-ft).",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........2000.",
        "HAINES INDEX........4 or low potential for large plume dominated fire growth.",

        "TIME (EST)      6AM 7AM 8AM 9AM 10A 11A 12P 1PM 2PM 3PM 4PM 5PM",
        "SKY (%).........0   0   0   0   0   0   0   0   0   0   0   0",
        "WEATHER COV.....CHC CHC CHC CHC CHC CHC CHC CHC CHC CHC CHC CHC",
        "WEATHER TYPE....RW  RW  RW  RW  RW  RW  RW  RW  RW  RW  RW  RW",
        "TEMP............0   0   0   0   0   0   0   0   0   0   0   0",
        "RH..............0   0   0   0   0   0   0   0   0   0   0   0",
        "20 FT WIND DIR..NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW",
        "20 FT WIND SPD..26  26  26  26  26  26  26  26  26  26  26  26",
        "20 FT WIND GUST.",
        "EYE LVL WND DIR.NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW",
        "EYE LVL WND SPD.26  26  26  26  26  26  26  26  26  26  26  26",
        "EYE LVL WND GST.",
        "RIDGE WIND DIR..NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW",
        "RIDGE WIND SPD..10  10  10  10  10  10  10  10  10  10  10  10",
        "MIX HGT (KFT)...0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "TRANSP WIND DIR.NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW",
        "TRANSP WIND SPD.12  12  12  12  12  12  12  12  12  12  12  12",
        "MIX HGT (KFT)...0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "TRANSP WIND DIR.NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW",
        "TRANSP WIND SPD.12  12  12  12  12  12  12  12  12  12  12  12",
        "DISPERSION......6   6   6   6   6   6   6   6   6   6   6   6",
        "DISPERSION IDX..6   6   6   6   6   6   6   6   6   6   6   6",
        "LVORI...........6   6   6   6   6   6   6   6   6   6   6   6",
        "CWR.............30  30  30  30  30  30  30  30  30  30  30  30",
        "CHC OF PCPN (%).90  90  90  90  90  90  90  90  90  90  90  90",
        "LAL.............3   3   3   3   3   3   3   3   3   3   3   3",
        "HAINES INDEX....4   4   4   4   4   4   4   4   4   4   4   4",

        ".TONIGHT...",

        "SKY/WEATHER.........MOSTLY CLEAR (10-20 percent). FREQUENT LIGHT RAIN.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Min 68.",
        "RH..................Max 80 percent.",
        "DEWPOINT............15.",
        "WIND (20 FT)........LIGHT WINDS.",
        "EYE LEVEL WINDS.....LIGHT WINDS.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......Northwest around 10 mph.",
        "SURROUNDING RIDGE...Northwest around 10 mph.",
        "CWR.................30 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Northwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Northwest around 12 mph.",
        "DISPERSION INDEX....1.",
        "LVORI...............1.",
        "DISPERSION..........1.",
        "SMOKE DISPERSAL.....FAIR (20000 knot-ft).",
        "STABILITY CLASS.....3.",
        "MARINE LAYER........2000.",
        "HAINES INDEX........6 OR HIGH potential for large plume dominated fire growth.",

        "TIME (EST)      6 PM   8 PM   10 PM  MIDNGT 2 AM   4 AM",
        "SKY (%).........15     15     15     15     15     15",
        "WEATHER COV.....FRQNT  FRQNT  FRQNT  FRQNT  FRQNT  FRQNT",
        "WEATHER TYPE....RAIN   RAIN   RAIN   RAIN   RAIN   RAIN",
        "TEMP............15     15     15     15     15     15",
        "RH..............15     15     15     15     15     15",
        "20 FT WIND......W 33   W 33   W 33   W 33   W 33   W 33",
        "20 FT WIND GUST.",
        "EYE LEVEL WIND..W 33   W 33   W 33   W 33   W 33   W 33",
        "EYE LEVEL WIND..",
        "RIDGETOP WIND...NW 10  NW 10  NW 10  NW 10  NW 10  NW 10",
        "MIX HGT (FT)....100    100    100    100    100    100",
        "TRANSPORT WIND..NW 12  NW 12  NW 12  NW 12  NW 12  NW 12",
        "MIX HGT (FT)....100    100    100    100    100    100",
        "TRANSPORT WIND..NW 12  NW 12  NW 12  NW 12  NW 12  NW 12",
        "DISPERSION......1      1      1      1      1      1",
        "DISPERSION IDX..1      1      1      1      1      1",
        "LVORI...........1      1      1      1      1      1",
        "CWR.............30     30     30     30     30     30",
        "CHC OF PCPN (%).90     90     90     90     90     90",
        "LAL.............4      4      4      4      4      4",
        "HAINES INDEX....6      6      6      6      6      6",

        ".SUNDAY...",

        "SKY/WEATHER.........Mostly sunny (25-35 percent). Widespread light freezing rain.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Max 78.",
        "RH..................Min 70 percent.",
        "DEWPOINT............30.",
        "WIND (20 FT)........Hurricane force winds. East winds around 144 mph.",
        "EYE LEVEL WINDS.....Hurricane force winds. East winds around 144 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......West around 25 mph.",
        "SURROUNDING RIDGE...West around 25 mph.",
        "CWR.................45 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......4000 ft AGL.",
        "TRANSPORT WINDS.....West around 23 mph.",
        "MIXING HEIGHT.......4000 ft AGL.",
        "TRANSPORT WINDS.....West around 23 mph.",
        "DISPERSION INDEX....5.",
        "LVORI...............5.",
        "DISPERSION..........5.",
        "SMOKE DISPERSAL.....Excellent (70000 knot-ft).",
        "STABILITY CLASS.....4.",
        "MARINE LAYER........4000.",
        "HAINES INDEX........2 or very low potential for large plume dominated fire growth.",

        "TIME (EST)      6 AM      9 AM      NOON      3 PM",
        "SKY (%).........30        30        30        30",
        "WEATHER COV.....WIDSPD    WIDSPD    WIDSPD    WIDSPD",
        "WEATHER TYPE....FZRAIN    FZRAIN    FZRAIN    FZRAIN",
        "TEMP............30        30        30        30",
        "RH..............30        30        30        30",
        "20 FT WIND......SW 37     SW 37     SW 37     SW 37",
        "EYE LEVEL WIND..SW 37     SW 37     SW 37     SW 37",
        "RIDGETOP WIND...W 25      W 25      W 25      W 25",
        "MIX HGT (FT)....4000      4000      4000      4000",
        "TRANSPORT WIND..W 23      W 23      W 23      W 23",
        "MIX HGT (FT)....4000      4000      4000      4000",
        "TRANSPORT WIND..W 23      W 23      W 23      W 23",
        "DISPERSION......5         5         5         5",
        "DISPERSION IDX..5         5         5         5",
        "LVORI...........5         5         5         5",
        "CWR.............50        50        50        50",
        "CHC OF PCPN (%).90        90        90        90",
        "LAL.............5         5         5         5",
        "HAINES INDEX....2         2         2         2",

        "$$",
        "FORECASTER...Forecaster C",
        "REQUESTED BY...YYYY",
        "TYPE OF REQUEST...Prescribed",
        ],

    },
    {    
    "name":"FWS_6",
    "productType":"FWS",
    "commentary": "Afternoon Update",
    "fileChanges":[
       ("FWS_<site>_Overrides", "TextUtility", "add", testOverrides, "undo"),
       ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
     ],
    "createGrids": FWS_createGrids,
    "drtHour": 14,
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Afternoon Update', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 4, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",

    "checkStrings": [

        "Spot Forecast for xxxx...Agency 1",
        "National Weather Service Tampa Bay Ruskin FL",
        "200 PM EST Fri Jan 1 2010",

        "Forecast is based on ignition time of 1300 EST on January 01.",
        "If conditions become unrepresentative...contact the National Weather Service.",

        ".DISCUSSION...",

        ".REST OF TODAY...",

        "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Max 78.",
        "RH..................Min 65 percent.",
        "DEWPOINT............100.",
        "WIND (20 FT)........North winds around 6 mph with gusts to around 30 mph.",
        "EYE LEVEL WINDS.....North winds around 6 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......Southwest around 10 mph.",
        "SURROUNDING RIDGE...Southwest around 10 mph.",
        "CWR.................0 percent.",
        "CHANCE OF PCPN......0 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Southwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Southwest around 12 mph.",
        "DISPERSION INDEX....0.",
        "LVORI...............0.",
        "DISPERSION..........0.",
        "SMOKE DISPERSAL.....Excellent (160000 knot-ft).",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........1000.",
        "HAINES INDEX........2 or very low potential for large plume dominated fire growth.",

        "TIME (EST)      2PM 3PM 4PM 5PM",
        "SKY (%).........100 100 100 100",
        "WEATHER COV.....PTY PTY PTY PTY",
        "WEATHER TYPE....FOG FOG FOG FOG",
        "TEMP............100 100 100 100",
        "RH..............100 100 100 100",
        "20 FT WIND DIR..SW  SW  SW  SW",
        "20 FT WIND SPD..7   7   7   7",
        "20 FT WIND GUST.30  30  30  30",
        "EYE LVL WND DIR.SW  SW  SW  SW",
        "EYE LVL WND SPD.7   7   7   7",
        "EYE LVL WND GST.30  30  30  30",
        "RIDGE WIND DIR..SW  SW  SW  SW",
        "RIDGE WIND SPD..10  10  10  10",
        "MIX HGT (KFT)...0.1 0.1 0.1 0.1",
        "TRANSP WIND DIR.SW  SW  SW  SW",
        "TRANSP WIND SPD.12  12  12  12",
        "MIX HGT (KFT)...0.1 0.1 0.1 0.1",
        "TRANSP WIND DIR.SW  SW  SW  SW",
        "TRANSP WIND SPD.12  12  12  12",
        "DISPERSION......0   0   0   0",
        "DISPERSION IDX..0   0   0   0",
        "LVORI...........0   0   0   0",
        "CWR.............0   0   0   0",
        "CHC OF PCPN (%).0   0   0   0",
        "LAL.............1   1   1   1",
        "HAINES INDEX....2   2   2   2",

        ".TONIGHT...",

        "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Min 60.",
        "RH..................Max 78 percent.",
        "DEWPOINT............95.",
        "WIND (20 FT)........Very windy. Northeast winds around 46 mph.",
        "EYE LEVEL WINDS.....Very windy. Northeast winds around 46 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......West around 5 mph.",
        "SURROUNDING RIDGE...West around 5 mph.",
        "CWR.................20 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................2.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....West around 6 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....West around 6 mph.",
        "DISPERSION INDEX....2.",
        "LVORI...............2.",
        "DISPERSION..........2.",
        "SMOKE DISPERSAL.....Excellent (100000 knot-ft).",
        "STABILITY CLASS.....2.",
        "MARINE LAYER........1000.",
        "HAINES INDEX........3 or very low potential for large plume dominated fire growth.",

        "TIME (EST)      6 PM   8 PM   10 PM  MIDNGT 2 AM   4 AM",
        "SKY (%).........95     95     95     95     95     95",
        "WEATHER COV.....WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD",
        "WEATHER TYPE....TSTORM TSTORM TSTORM TSTORM TSTORM TSTORM",
        "TEMP............95     95     95     95     95     95",
        "RH..............95     95     95     95     95     95",
        "20 FT WIND......SE 30  SE 30  SE 30  SE 30  SE 30  SE 30",
        "20 FT WIND GUST.",
        "EYE LEVEL WIND..SE 30  SE 30  SE 30  SE 30  SE 30  SE 30",
        "EYE LEVEL WIND..",
        "RIDGETOP WIND...W 5    W 5    W 5    W 5    W 5    W 5",
        "MIX HGT (FT)....100    100    100    100    100    100",
        "TRANSPORT WIND..W 6    W 6    W 6    W 6    W 6    W 6",
        "MIX HGT (FT)....100    100    100    100    100    100",
        "TRANSPORT WIND..W 6    W 6    W 6    W 6    W 6    W 6",
        "DISPERSION......2      2      2      2      2      2",
        "DISPERSION IDX..2      2      2      2      2      2",
        "LVORI...........2      2      2      2      2      2",
        "CWR.............20     20     20     20     20     20",
        "CHC OF PCPN (%).90     90     90     90     90     90",
        "LAL.............2      2      2      2      2      2",
        "HAINES INDEX....3      3      3      3      3      3",

        ".SATURDAY...",

        "SKY/WEATHER.........Sunny (0-5 percent). Chance of rain showers.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Max 79.",
        "RH..................Min 68 percent.",
        "DEWPOINT............0.",
        "WIND (20 FT)........Northwest winds around 12 mph.",
        "EYE LEVEL WINDS.....Northwest winds around 12 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......Northwest around 10 mph.",
        "SURROUNDING RIDGE...Northwest around 10 mph.",
        "CWR.................30 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Northwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Northwest around 12 mph.",
        "DISPERSION INDEX....6.",
        "LVORI...............6.",
        "DISPERSION..........6.",
        "SMOKE DISPERSAL.....Good (50000 knot-ft).",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........2000.",
        "HAINES INDEX........4 or low potential for large plume dominated fire growth.",

        "TIME (EST)      6 AM         10 AM        2 PM",
        "SKY (%).........0            0            0",
        "WEATHER COV.....CHANCE       CHANCE       CHANCE",
        "WEATHER TYPE....RNSHWR       RNSHWR       RNSHWR",
        "TEMP............0            0            0",
        "RH..............0            0            0",
        "20 FT WIND......NW 26        NW 26        NW 26",
        "EYE LEVEL WIND..NW 26        NW 26        NW 26",
        "RIDGETOP WIND...NW 10        NW 10        NW 10",
        "MIX HGT (FT)....100          100          100",
        "TRANSPORT WIND..NW 12        NW 12        NW 12",
        "MIX HGT (FT)....100          100          100",
        "TRANSPORT WIND..NW 12        NW 12        NW 12",
        "DISPERSION......6            6            6",
        "DISPERSION IDX..6            6            6",
        "LVORI...........6            6            6",
        "CWR.............30           30           30",
        "CHC OF PCPN (%).90           90           90",
        "LAL.............3            3            3",
        "HAINES INDEX....4            4            4",

        "$$",
        "FORECASTER...Forecaster C",
        "REQUESTED BY...YYYY",
        "TYPE OF REQUEST...Prescribed",
        ],

    },
    {    
    "name":"FWS_7",
    "productType":"FWS",
    "commentary": "Next Day Issuance",
    "fileChanges":[
       ("FWS_<site>_Overrides", "TextUtility", "add", testOverrides, "undo"),
       ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
     ],
    "createGrids": FWS_createGrids,
    "drtHour": 22,
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Evening Update', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 4, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",

    "checkStrings": [
        "FNUS72 KTBW 020300",
        "FWSTBW",

        "Spot Forecast for xxxx...Agency 1",
        "National Weather Service Tampa Bay Ruskin FL",
        "1000 PM EST Fri Jan 1 2010",

        "Forecast is based on ignition time of 1300 EST on January 01.",
        "If conditions become unrepresentative...contact the National Weather Service.",
        
        ".DISCUSSION...",

        ".REST OF TONIGHT...",

        "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Min 60.",
        "RH..................Max 78 percent.",
        "DEWPOINT............95.",
        "WIND (20 FT)........Very windy. Northeast winds around 46 mph.",
        "EYE LEVEL WINDS.....Very windy. Northeast winds around 46 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......West around 5 mph.",
        "SURROUNDING RIDGE...West around 5 mph.",
        "CWR.................20 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................2.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....West around 6 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....West around 6 mph.",
        "DISPERSION INDEX....2.",
        "LVORI...............2.",
        "DISPERSION..........2.",
        "SMOKE DISPERSAL.....Excellent (100000 knot-ft).",
        "STABILITY CLASS.....2.",
        "MARINE LAYER........1000.",
        "HAINES INDEX........3 or very low potential for large plume dominated fire growth.",

        "TIME (EST)      10 PM  MIDNGT 2 AM   4 AM",
        "SKY (%).........95     95     95     95",
        "WEATHER COV.....WIDSPD WIDSPD WIDSPD WIDSPD",
        "WEATHER TYPE....TSTORM TSTORM TSTORM TSTORM",
        "TEMP............95     95     95     95",
        "RH..............95     95     95     95",
        "20 FT WIND......SE 30  SE 30  SE 30  SE 30",
        "20 FT WIND GUST.",
        "EYE LEVEL WIND..SE 30  SE 30  SE 30  SE 30",
        "EYE LEVEL WIND..",
        "RIDGETOP WIND...W 5    W 5    W 5    W 5",
        "MIX HGT (FT)....100    100    100    100",
        "TRANSPORT WIND..W 6    W 6    W 6    W 6",
        "MIX HGT (FT)....100    100    100    100",
        "TRANSPORT WIND..W 6    W 6    W 6    W 6",
        "DISPERSION......2      2      2      2",
        "DISPERSION IDX..2      2      2      2",
        "LVORI...........2      2      2      2",
        "CWR.............20     20     20     20",
        "CHC OF PCPN (%).90     90     90     90",
        "LAL.............2      2      2      2",
        "HAINES INDEX....3      3      3      3",

        ".SATURDAY...",

        "SKY/WEATHER.........Sunny (0-5 percent). Chance of rain showers.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Max 79.",
        "RH..................Min 68 percent.",
        "DEWPOINT............0.",
        "WIND (20 FT)........Northwest winds around 12 mph.",
        "EYE LEVEL WINDS.....Northwest winds around 12 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......Northwest around 10 mph.",
        "SURROUNDING RIDGE...Northwest around 10 mph.",
        "CWR.................30 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Northwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "TRANSPORT WINDS.....Northwest around 12 mph.",
        "DISPERSION INDEX....6.",
        "LVORI...............6.",
        "DISPERSION..........6.",
        "SMOKE DISPERSAL.....Good (50000 knot-ft).",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........2000.",
        "HAINES INDEX........4 or low potential for large plume dominated fire growth.",

        "TIME (EST)      6 AM         10 AM        2 PM",
        "SKY (%).........0            0            0",
        "WEATHER COV.....CHANCE       CHANCE       CHANCE",
        "WEATHER TYPE....RNSHWR       RNSHWR       RNSHWR",
        "TEMP............0            0            0",
        "RH..............0            0            0",
        "20 FT WIND......NW 26        NW 26        NW 26",
        "EYE LEVEL WIND..NW 26        NW 26        NW 26",
        "RIDGETOP WIND...NW 10        NW 10        NW 10",
        "MIX HGT (FT)....100          100          100",
        "TRANSPORT WIND..NW 12        NW 12        NW 12",
        "MIX HGT (FT)....100          100          100",
        "TRANSPORT WIND..NW 12        NW 12        NW 12",
        "DISPERSION......6            6            6",
        "DISPERSION IDX..6            6            6",
        "LVORI...........6            6            6",
        "CWR.............30           30           30",
        "CHC OF PCPN (%).90           90           90",
        "LAL.............3            3            3",
        "HAINES INDEX....4            4            4",

        "$$",
        "FORECASTER...Forecaster C",
        "REQUESTED BY...YYYY",
        "TYPE OF REQUEST...Prescribed",
        ],

    },
    {    
    "name":"FWS_8",
    "productType":"FWS",
    "commentary": "User-supplied Creation Date and Time",
    "fileChanges":[
       ("FWS_<site>_Overrides", "TextUtility", "add", testOverrides, "undo"),
       ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
     ],
    "createGrids": FWS_createGrids,
    "drtHour": 22,
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Evening Update', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Creation Date', 'creationDate'): '1/3/10', ('Creation Time', 'creationTime'): '0600', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 4, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 'None', ('TimeZone:', 'fireTZ'): 'EST5EDT'}",

    "checkStrings": [
        "FNUS72 KTBW 031100",
        "FWSTBW",

        "Spot Forecast for xxxx...Agency 1",
        "National Weather Service Tampa Bay Ruskin FL",
        "600 AM EST Sun Jan 3 2010",

        "Forecast is based on ignition time of 1300 EST on January 01.",
        "If conditions become unrepresentative...contact the National Weather Service.",

        ".DISCUSSION...",

        ".SUNDAY...",

        "SKY/WEATHER.........Partly cloudy (40-50 percent). Widespread light freezing rain...then very light snow likely.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Min 65.",
        "RH..................Max 82 percent.",
        "DEWPOINT............55.",
        "WIND (20 FT)........Hurricane force winds. South winds 104 TO 144 mph.",
        "EYE LEVEL WINDS.....Hurricane force winds. South winds 104 TO 144 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......West around 25 mph.",
        "SURROUNDING RIDGE...West around 25 mph.",
        "CWR.................60 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......4000 ft AGL.",
        "TRANSPORT WINDS.....West around 23 mph.",
        "MIXING HEIGHT.......4000 ft AGL.",
        "TRANSPORT WINDS.....West around 23 mph.",
        "DISPERSION INDEX....5.",
        "LVORI...............5.",
        "DISPERSION..........5.",
        "SMOKE DISPERSAL.....Poor to excellent (4000-70000 knot-ft).",
        "STABILITY CLASS.....4.",
        "MARINE LAYER........4000.",
        "HAINES INDEX........2 TO 3 OR or very low potential for large plume dominated fire growth.",

        "TIME (EST)      6 AM   8 AM   10 AM  NOON   2 PM   4 PM   6 PM   8 PM   10 PM  MIDNGT 2 AM   4 AM",
        "SKY (%).........30     30     30     30     30     30     55     55     55     55     55     55",
        "WEATHER COV.....WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD LIKELY LIKELY LIKELY LIKELY LIKELY LIKELY",
        "WEATHER TYPE....FZRAIN FZRAIN FZRAIN FZRAIN FZRAIN FZRAIN SNOW   SNOW   SNOW   SNOW   SNOW   SNOW",
        "TEMP............30     30     30     30     30     30     55     55     55     55     55     55",
        "RH..............30     30     30     30     30     30     55     55     55     55     55     55",
        "20 FT WIND......SW 37  SW 37  SW 37  SW 37  SW 37  SW 37  E 33   E 33   E 33   E 33   E 33   E 33",
        "20 FT WIND GUST.",
        "EYE LEVEL WIND..SW 37  SW 37  SW 37  SW 37  SW 37  SW 37  E 33   E 33   E 33   E 33   E 33   E 33",
        "EYE LEVEL WIND..",
        "RIDGETOP WIND...W 25   W 25   W 25   W 25   W 25   W 25   W 25   W 25   W 25   W 25   W 25   W 25",
        "MIX HGT (FT)....4000   4000   4000   4000   4000   4000   4000   4000   4000   4000   4000   4000",
        "TRANSPORT WIND..W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23",
        "MIX HGT (FT)....4000   4000   4000   4000   4000   4000   4000   4000   4000   4000   4000   4000",
        "TRANSPORT WIND..W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23",
        "DISPERSION......5      5      5      5      5      5      4      4      4      4      4      4",
        "DISPERSION IDX..5      5      5      5      5      5      4      4      4      4      4      4",
        "LVORI...........5      5      5      5      5      5      4      4      4      4      4      4",
        "CWR.............50     50     50     50     50     50     60     60     60     60     60     60",
        "CHC OF PCPN (%).90     90     90     90     90     90     70     70     70     70     70     70",
        "LAL.............5      5      5      5      5      5      6      6      6      6      6      6",
        "HAINES INDEX....2      2      2      2      2      2      3      3      3      3      3      3",

        ".MONDAY...",

        "SKY/WEATHER.........Partly sunny (60-70 percent). Widespread very light sleet.",
        "BEGIN/END OF PCPN...",
        "TEMPERATURE.........Max 80.",
        "RH..................Min 73 percent.",
        "DEWPOINT............65.",
        "WIND (20 FT)........STRONG WINDS. South winds around 58 mph.",
        "EYE LEVEL WINDS.....STRONG WINDS. South winds around 58 mph.",
        "WIND SHIFT..........",
        "RIDGETOP WIND.......West around 35 mph.",
        "SURROUNDING RIDGE...West around 35 mph.",
        "CWR.................25 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......3500 ft AGL.",
        "TRANSPORT WINDS.....West around 35 mph.",
        "MIXING HEIGHT.......3500 ft AGL.",
        "TRANSPORT WINDS.....West around 35 mph.",
        "DISPERSION INDEX....3.",
        "LVORI...............3.",
        "DISPERSION..........3.",
        "SMOKE DISPERSAL.....Poor (4000 knot-ft).",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........5300.",
        "HAINES INDEX........2 or very low potential for large plume dominated fire growth.",

        "$$",
        "FORECASTER...Forecaster C",
        "REQUESTED BY...YYYY",
        "TYPE OF REQUEST...Prescribed",
        ],

    },

    {
    "name":"FWS_9",
    "productType":"FWS",
    "commentary": "Morning Issuance with Definition settings",
    "createGrids": FWS_createGrids,
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'no', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 'None', ('Tab Hrs', 'tomorrowTableRes'): 'None', ('TimeZone:', 'fireTZ'): 'EST5EDT', ('Check Items to Include:', 'extendedQuestions'): ['Include Day 3-5 Extended?', 'Include Day 6-7 Extended?', 'Include Day 8-14 Outlook?']}",
    "fileChanges":[
       ("FWS_<site>_Overrides", "TextUtility", "add", testOverrides, "undo"),
       ("FWS_<site>_Definition", "TextUtility", "add", definitions, "undo"),
       ("FWS_<site>_Definition", "TextUtility", "add", definition1, "undo"),
       ],

    "checkStrings": [
        "FNUS72 KTBW 010900",
        "FWSTBW",

        "Spot Forecast for xxxx...Agency 1",
        "National Weather Service Tampa Bay Ruskin FL",
        "400 AM EST Fri Jan 1 2010",

        "If conditions become unrepresentative...contact the National Weather Service.",

        ".DISCUSSION...",

        ".TODAY...",

        "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
        "BEGIN/END OF PCPN...",
        "MAX TEMPERATURE.....Around 78.",
        "MIN HUMIDITY........100 percent.",
        "DEWPOINT............100.",
        "WIND (20 FT)........",
        "SLOPE/VALLEY.......North winds around 6 mph with gusts to around 30 mph.",
        "EYE LEVEL WINDS.....North winds around 6 mph.",
        "WIND SHIFT..........",
        "RIDGETOP...........Southwest around 10 mph.",
        "SURROUNDING RIDGE..Southwest around 10 mph.",
        "CWR.................0 percent.",
        "CHANCE OF PCPN......0 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL.",
        "MIXING WINDS........Southwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "MIXING WINDS........Southwest around 12 mph.",
        "DISPERSION INDEX....0.",
        "LVORI...............0.",
        "DISPERSION..........0.",
        "SMOKE DISPERSAL.....Excellent (160000 knot-ft).",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........1000.",
        "HAINES INDEX........2 or very low potential for large plume dominated fire growth.",

        "TIME (EST)      4AM 5AM 6AM 7AM 8AM 9AM 10A 11A 12P 1PM 2PM 3PM",
        "SKY (%).........M   M   CDY CDY CDY CDY CDY CDY CDY CDY CDY CDY",
        "WEATHER COV.....PTY PTY PTY PTY PTY PTY PTY PTY PTY PTY PTY PTY",
        "WEATHER TYPE....FOG FOG FOG FOG FOG FOG FOG FOG FOG FOG FOG FOG",
        "TEMP............M   M   100 100 100 100 100 100 100 100 100 100",
        "RH..............M   M   100 100 100 100 100 100 100 100 100 100",
        "20 FT WIND DIR..N   N   N   N   N   N   N   N   N   N   N   N",
        "20 FT WIND SPD..6   6   6   6   6   6   6   6   6   6   6   6",
        "20 FT WIND GUST.30  30  30  30  30  30  30  30  30  30  30  30",
        "EYE LVL WND DIR.230 230 230 230 230 230 230 230 230 230 230 230",
        "EYE LVL WND SPD.7   7   7   7   7   7   7   7   7   7   7   7",
        "EYE LVL WND GST.30  30  30  30  30  30  30  30  30  30  30  30",
        "RIDGE WIND DIR..230 230 230 230 230 230 230 230 230 230 230 230",
        "RIDGE WIND SPD..10  10  10  10  10  10  10  10  10  10  10  10",
        "MIX HGT (KFT)...0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "MIX HGT (KM)....0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "MIXNG WIND DIR..230 230 230 230 230 230 230 230 230 230 230 230",
        "MIXNG WIND SPD..12  12  12  12  12  12  12  12  12  12  12  12",
        "MIX WND SPD M/S.5   5   5   5   5   5   5   5   5   5   5   5",
        "MIX HGT (KFT)...0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "MIX HGT (KM)....0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "MIXNG WIND DIR..230 230 230 230 230 230 230 230 230 230 230 230",
        "MIXNG WIND SPD..12  12  12  12  12  12  12  12  12  12  12  12",
        "MIX WND SPD M/S.5   5   5   5   5   5   5   5   5   5   5   5",
        "DISPERSION......0   0   0   0   0   0   0   0   0   0   0   0",
        "DISPERSION IDX..0   0   0   0   0   0   0   0   0   0   0   0",
        "LVORI...........0   0   0   0   0   0   0   0   0   0   0   0",
        "CWR.............0   0   0   0   0   0   0   0   0   0   0   0",
        "CHC OF PCPN (%).0   0   0   0   0   0   0   0   0   0   0   0",
        "LAL.............1   1   1   1   1   1   1   1   1   1   1   1",
        "HAINES INDEX....2   2   2   2   2   2   2   2   2   2   2   2",

        ".TONIGHT...",

        "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
        "BEGIN/END OF PCPN...",
        "MIN TEMPERATURE.....Around 60.",
        "MAX HUMIDITY........95 percent.",
        "DEWPOINT............95.",
        "WIND (20 FT)........",
        "SLOPE/VALLEY.......Very windy. Northeast winds around 46 mph.",
        "EYE LEVEL WINDS.....Very windy. Northeast winds around 46 mph.",
        "WIND SHIFT..........",
        "RIDGETOP...........West around 5 mph.",
        "SURROUNDING RIDGE..West around 5 mph.",
        "CWR.................20 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................2.",
        "MIXING HEIGHT.......100 ft AGL.",
        "MIXING WINDS........West around 6 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "MIXING WINDS........West around 6 mph.",
        "DISPERSION INDEX....2.",
        "LVORI...............2.",
        "DISPERSION..........2.",
        "SMOKE DISPERSAL.....Excellent (100000 knot-ft).",
        "STABILITY CLASS.....2.",
        "MARINE LAYER........1000.",
        "HAINES INDEX........3 or very low potential for large plume dominated fire growth.",


        ".SATURDAY...",

        "SKY/WEATHER.........Sunny (0-5 percent). Chance of rain showers.",
        "BEGIN/END OF PCPN...",
        "MAX TEMPERATURE.....around 79.",
        "MIN HUMIDITY........0 percent.",
        "DEWPOINT............0.",
        "WIND (20 FT)........",
        "SLOPE/VALLEY.......Northwest winds around 12 mph.",
        "EYE LEVEL WINDS.....Northwest winds around 12 mph.",
        "WIND SHIFT..........",
        "RIDGETOP...........Northwest around 10 mph.",
        "SURROUNDING RIDGE..Northwest around 10 mph.",
        "CWR.................30 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL.",
        "MIXING WINDS........Northwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "MIXING WINDS........Northwest around 12 mph.",
        "DISPERSION INDEX....6.",
        "LVORI...............6.",
        "DISPERSION..........6.",
        "SMOKE DISPERSAL.....Good (50000 knot-ft).",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........2000.",
        "HAINES INDEX........4 or low potential for large plume dominated fire growth.",

        ".FORECAST DAYS 3 THROUGH 7...",
        ".SUNDAY...",
        "Strong winds. Widespread light freeaing rain and frequent light rain. Lows in the upper 60s. Highs in the upper 70s. Northeast winds around 72 mph.",
        ".MONDAY...",
        "Hurricane force winds. Widespread very light sleet and very light snow likely. Lows in the mid 60s. Highs around 80. South winds around 81 mph.",
        ".TUESDAY...",
        "Strong winds. Partly cloudy. Areas of blowing snow...then patchy fog. Lows in the mid 60s. Highs in the lower 80s. South winds around 58 mph.",
        ".WEDNESDAY...",
        "Mostly cloudy. Drizzle likely...then slight chance of very light freezing drizzle. Lows in the lower 60s. Highs in the lower 80s.",
        ".THURSDAY...",
        "Partly cloudy. Numerous thunderstorms. Isolated light freezing spray. Lows in the mid 60s. Highs in the mid 80s.",

        ".OUTLOOK FOR FRIDAY JANUARY 08 THROUGH THURSDAY JANUARY 14...",
        "...Put 8 to 14 day outlook text here...",

        "$$",
        "FORECASTER...Forecaster C",
        "REQUESTED BY...YYYY",
        "TYPE OF REQUEST...Prescribed",

        ],

    },
    {
    "name":"FWS_10",
    "productType":"FWS",
    "commentary": "Morning Issuance with Definition settings",
    "createGrids": FWS_createGrids,
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['SKY/WEATHER', 'BEGIN/END OF PCPN', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'EYE LEVEL WINDS', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'MIXING HEIGHT', 'TRANSPORT WINDS', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'MARINE LAYER', 'HAINES INDEX'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'no', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 'None', ('Tab Hrs', 'tomorrowTableRes'): 'None', ('TimeZone:', 'fireTZ'): 'EST5EDT'}",
    "fileChanges":[
       ("FWS_<site>_Overrides", "TextUtility", "add", testOverrides, "undo"),
       ("FWS_<site>_Definition", "TextUtility", "add", definitions, "undo"),
       ("FWS_<site>_Definition", "TextUtility", "add", definition2, "undo"),
       ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
       ],

    "checkStrings": [
        "FNUS72 KTBW 010900",
        "FWSTBW",

        "Spot Forecast for xxxx...Agency 1",
        "National Weather Service Tampa Bay Ruskin FL",
        "400 AM EST Fri Jan 1 2010",

        "If conditions become unrepresentative...contact the National Weather Service.",

        ".DISCUSSION...",

        ".TODAY...",

        "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
        "BEGIN/END OF PCPN...",
        "MAX TEMPERATURE.....Around 78.",
        "MIN HUMIDITY........100 percent.",
        "DEWPOINT............100.",
        "WIND (20 FT)........",
        "SLOPE/VALLEY.......North winds around 6 mph with gusts to around 30 mph.",
        "EYE LEVEL WINDS.....North winds around 6 mph.",
        "WIND SHIFT..........",
        "RIDGETOP...........Southwest around 10 mph.",
        "SURROUNDING RIDGE..Southwest around 10 mph.",
        "CWR.................0 percent.",
        "CHANCE OF PCPN......0 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL.",
        "MIXING WINDS........Southwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "MIXING WINDS........Southwest around 12 mph.",
        "DISPERSION INDEX....0.",
        "LVORI...............0.",
        "DISPERSION..........0.",
        "SMOKE DISPERSAL.....Excellent (160000 knot-ft).",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........1000.",
        "HAINES INDEX........2 or very low potential for large plume dominated fire growth.",

        "TIME (EST)      6AM 7AM 8AM 9AM 10A 11A 12P 1PM 2PM 3PM 4PM 5PM",
        "SKY (%).........CDY CDY CDY CDY CDY CDY CDY CDY CDY CDY CDY CDY",
        "WEATHER COV.....PTY PTY PTY PTY PTY PTY PTY PTY PTY PTY PTY PTY",
        "WEATHER TYPE....FOG FOG FOG FOG FOG FOG FOG FOG FOG FOG FOG FOG",
        "TEMP............100 100 100 100 100 100 100 100 100 100 100 100",
        "RH..............100 100 100 100 100 100 100 100 100 100 100 100",
        "20 FT WIND DIR..N   N   N   N   N   N   N   N   N   N   N   N",
        "20 FT WIND SPD..6   6   6   6   6   6   6   6   6   6   6   6",
        "20 FT WIND GUST.30  30  30  30  30  30  30  30  30  30  30  30",
        "EYE LVL WND DIR.230 230 230 230 230 230 230 230 230 230 230 230",
        "EYE LVL WND SPD.7   7   7   7   7   7   7   7   7   7   7   7",
        "EYE LVL WND GST.30  30  30  30  30  30  30  30  30  30  30  30",
        "RIDGE WIND DIR..230 230 230 230 230 230 230 230 230 230 230 230",
        "RIDGE WIND SPD..10  10  10  10  10  10  10  10  10  10  10  10",
        "MIX HGT (KFT)...0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "MIX HGT (KM)....0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "MIXNG WIND DIR..230 230 230 230 230 230 230 230 230 230 230 230",
        "MIXNG WIND SPD..12  12  12  12  12  12  12  12  12  12  12  12",
        "MIX WND SPD M/S.5   5   5   5   5   5   5   5   5   5   5   5",
        "MIX HGT (KFT)...0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "MIX HGT (KM)....0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "MIXNG WIND DIR..230 230 230 230 230 230 230 230 230 230 230 230",
        "MIXNG WIND SPD..12  12  12  12  12  12  12  12  12  12  12  12",
        "MIX WND SPD M/S.5   5   5   5   5   5   5   5   5   5   5   5",
        "DISPERSION......0   0   0   0   0   0   0   0   0   0   0   0",
        "DISPERSION IDX..0   0   0   0   0   0   0   0   0   0   0   0",
        "LVORI...........0   0   0   0   0   0   0   0   0   0   0   0",
        "CWR.............0   0   0   0   0   0   0   0   0   0   0   0",
        "CHC OF PCPN (%).0   0   0   0   0   0   0   0   0   0   0   0",
        "LAL.............1   1   1   1   1   1   1   1   1   1   1   1",
        "HAINES INDEX....2   2   2   2   2   2   2   2   2   2   2   2",

        ".TONIGHT...",

        "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
        "BEGIN/END OF PCPN...",
        "MIN TEMPERATURE.....Around 60.",
        "MAX HUMIDITY........95 percent.",
        "DEWPOINT............95.",
        "WIND (20 FT)........",
        " SLOPE/VALLEY.......Very windy. Northeast winds around 46 mph.",
        "EYE LEVEL WINDS.....Very windy. Northeast winds around 46 mph.",
        "WIND SHIFT..........",
        "RIDGETOP...........West around 5 mph.",
        "SURROUNDING RIDGE..West around 5 mph.",
        "CWR.................20 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................2.",
        "MIXING HEIGHT.......100 ft AGL.",
        "MIXING WINDS........West around 6 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "MIXING WINDS........West around 6 mph.",
        "DISPERSION INDEX....2.",
        "LVORI...............2.",
        "DISPERSION..........2.",
        "SMOKE DISPERSAL.....Excellent (100000 knot-ft).",
        "STABILITY CLASS.....2.",
        "MARINE LAYER........1000.",
        "HAINES INDEX........3 or very low potential for large plume dominated fire growth.",


        ".SATURDAY...",

        "SKY/WEATHER.........Sunny (0-5 percent). Chance of rain showers.",
        "BEGIN/END OF PCPN...",
        "MAX TEMPERATURE.....Around 79.",
        "MIN HUMIDITY........0 percent.",
        "DEWPOINT............0.",
        "WIND (20 FT)........",
        "SLOPE/VALLEY.......Northwest winds around 12 mph.",
        "EYE LEVEL WINDS.....Northwest winds around 12 mph.",
        "WIND SHIFT..........",
        "RIDGETOP...........Northwest around 10 mph.",
        "SURROUNDING RIDGE..Northwest around 10 mph.",
        "CWR.................30 percent.",
        "CHANCE OF PCPN......90 percent.",
        "LAL.................1.",
        "MIXING HEIGHT.......100 ft AGL.",
        "MIXING WINDS........Northwest around 12 mph.",
        "MIXING HEIGHT.......100 ft AGL.",
        "MIXING WINDS........Northwest around 12 mph.",
        "DISPERSION INDEX....6.",
        "LVORI...............6.",
        "DISPERSION..........6.",
        "SMOKE DISPERSAL.....Good (50000 knot-ft).",
        "STABILITY CLASS.....1.",
        "MARINE LAYER........2000.",
        "HAINES INDEX........4 or low potential for large plume dominated fire growth.",

        "$$",
        "FORECASTER...Forecaster C",
        "REQUESTED BY...YYYY",
        "TYPE OF REQUEST...Prescribed",
       ],

    },

    {
    "name":"FWS_Hourly_Sky_Table",
    "productType":"FWS",
    "commentary": "Test Hourly Sky Table",
    "createGrids": Sky_createGrids,
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['SKY/WEATHER'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['SKY/WEATHER'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['SKY/WEATHER'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular Only', ('Include Ignition Times?', 'withIgnitionTimes'): 'no', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '0600', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 1, ('Tab Hrs', 'tomorrowTableRes'): 1, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",
    "fileChanges":[
       ("FWS_<site>_Definition", "TextUtility", "add", definition3, "undo"),
       ],

    "checkStrings": [
        ".TODAY...",
        "TIME (EST)      6AM 7AM 8AM 9AM 10A 11A 12P 1PM 2PM 3PM 4PM 5PM",
        "SKY (%).........MCR MCR MCR MCR MCR MCR MCR MCR MCR MCR MCR MCR",
        ".TONIGHT...",
        "TIME (EST)      6PM 7PM 8PM 9PM 10P 11P MID 1AM 2AM 3AM 4AM 5AM",
        "SKY (%).........PC  PC  PC  PC  PC  PC  PC  PC  PC  PC  PC  PC",
        ".SATURDAY...",
        "TIME (EST)      6AM 7AM 8AM 9AM 10A 11A 12P 1PM 2PM 3PM 4PM 5PM",
        "SKY (%).........MC  MC  MC  MC  MC  MC  MC  MC  MC  MC  MC  MC",
       ],

    },

# FIXME: these tests cause the auto test process to crash.
#
#     {
#     "name":"FWS_StqInput_0",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct as input",
#     "createGrids": FWS_createGrids,
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":0},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ],
#     "checkStrings": [
#             "IFPS TEST 0",
#             ".TODAY...",
#             "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
#             "CWR.................0 percent.",
#             "MAX TEMPERATURE.....Around 78.",
#             "EYE LEVEL WINDS.....North winds around 6 mph.",
#             "MIXING HEIGHT.......100 ft AGL.",
#             "TRANSPORT WINDS.....Southwest around 12 mph.",
#             "MIN HUMIDITY........65 percent.",
#             "WIND (20 FT)........North winds around 6 mph with gusts to around 30 mph",
# 
#             ".SATURDAY...",
#             "SKY/WEATHER.........Sunny (0-5 percent). Chance of showers.",
#             "CWR.................30 percent.",
#             "MAX TEMPERATURE.....Around 79. ",
#             "MIN HUMIDITY........68 percent. ",
#             "WIND (20 FT)........Northwest winds around 12 mph.",
#             "EYE LEVEL WINDS.....Northwest winds around 12 mph.",
#             ],
#     },
# 
#     {
#     "name":"FWS_StqInput_1",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct as input",
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":1},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ],
#     "checkStrings": [
#         "IFPS TEST 1",
#         ".TODAY...",
#         "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "MAX TEMPERATURE.....Around 78.",
#         "MIN HUMIDITY........65 percent.",
#         "EYE LEVEL WINDS.....North winds around 6 mph.",
#         "MIXING HEIGHT.......100 ft AGL.",
#         "TRANSPORT WINDS.....Southwest around 12 mph.",
# 
#         ".SATURDAY...",
#         "SKY/WEATHER.........Sunny (0-5 percent). Chance of showers.",
#         "CWR.................30 percent.",
#          
#         ],
#     },
# 
#     {
#     "name":"FWS_StqInput_2",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct as input",
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":2},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ],
#     "checkStrings": [
#         "IFPS TEST 2",
#         ".TODAY...",
#         "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "MAX TEMPERATURE.....Around 78.",
#         "MIN HUMIDITY........65 percent.",
#         "EYE LEVEL WINDS.....North winds around 6 mph.",
#         "MIXING HEIGHT.......100 ft AGL.",
#         "TRANSPORT WINDS.....Southwest around 12 mph.",
# 
#         ".TONIGHT...",
#         "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "MIN TEMPERATURE.....Around 60.",
#         "MAX HUMIDITY........78 percent.",
# 
#         ".SATURDAY...",
#         "SKY/WEATHER.........Sunny (0-5 percent). Chance of showers.",
#         "CWR.................30 percent.",
#         ],
#     },
#     {
#     "name":"FWS_StqInput_3",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct as input",
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":3},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ],
#     "checkStrings": [
#         "IFPS TEST 3",
#         ".TODAY...",
#         "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "MAX TEMPERATURE.....Around 78.",
#         "MIN HUMIDITY........65 percent.",
# 
#         ".TONIGHT...",
#         "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "MIN TEMPERATURE.....Around 60.",
#         "MAX HUMIDITY........78 percent.",
# 
#         ".SATURDAY...",
#         "SKY/WEATHER.........Sunny (0-5 percent). Chance of showers.",
#         "CWR.................30 percent.",
#         ],
#     },
#     {
#     "name":"FWS_StqInput_4",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct as input",
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":4},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ],
#     "checkStrings": [
#         "IFPS TEST 4",
#         ".TODAY...",
#         "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "MAX TEMPERATURE.....Around 78.",
#         "MIN HUMIDITY........65 percent.",
#         "EYE LEVEL WINDS.....North winds around 6 mph.",
#         "MIXING HEIGHT.......100 ft AGL.",
#         "TRANSPORT WINDS.....Southwest around 12 mph.",
#         "SMOKE DISPERSAL.....Excellent (160000 knot-ft).",
#         "STABILITY CLASS.....1.",
#         "MARINE LAYER........1000.",
#         "HAINES INDEX........2 or very low potential for large plume dominated fire growth.",
# 
#         ".TONIGHT...",
#         "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "MIN TEMPERATURE.....Around 60.",
#         "MAX HUMIDITY........78 percent.",
#         "SMOKE DISPERSAL.....Excellent (100000 knot-ft).",
#         "STABILITY CLASS.....2.",
#         "MARINE LAYER........1000.",
#         "HAINES INDEX........3 or very low potential for large plume dominated fire growth.",
# 
#         ".SATURDAY...",
#         "SKY/WEATHER.........Sunny (0-5 percent). Chance of showers.",
#         "CWR.................30 percent.",
#         "SMOKE DISPERSAL.....Good (50000 knot-ft).",
#         "STABILITY CLASS.....1.",
#         "MARINE LAYER........2000.",
#         "HAINES INDEX........4 or low potential for large plume dominated fire growth",
#         ],
#     },
#     {
#     "name":"FWS_StqInput_5",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct as input",
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":5},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ],
#     "checkStrings": [
#         "IFPS TEST 5",
#         ".TODAY...",
#         "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "MAX TEMPERATURE.....Around 78.",
#         "MIN HUMIDITY........65 percent.",
#         "EYE LEVEL WINDS.....North winds around 6 mph.",
#         "MIXING HEIGHT.......100 ft AGL.",
#         "TRANSPORT WINDS.....Southwest around 12 mph.",
#         "SMOKE DISPERSAL.....Excellent (160000 knot-ft).",
#         "STABILITY CLASS.....1.",
#         "MARINE LAYER........1000.",
#         "HAINES INDEX........2 or very low potential for large plume dominated fire growth.",
# 
#         ".TONIGHT...",
#         "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "MIN TEMPERATURE.....Around 60.",
#         "MAX HUMIDITY........78 percent.",
#         "SMOKE DISPERSAL.....Excellent (100000 knot-ft).",
#         "STABILITY CLASS.....2.",
#         "MARINE LAYER........1000.",
#         "HAINES INDEX........3 or very low potential for large plume dominated fire growth.",
# 
#         ".SATURDAY...",
#         "SKY/WEATHER.........Sunny (0-5 percent). Chance of showers.",
#         "CWR.................30 percent.",
#         ],
#     },
#     {
#     "name":"FWS_StqInput_6",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct as input",
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":6},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ],
#     "checkStrings": [
#         "IFPS TEST 6",
#         ".TODAY...",
#         "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "MAX TEMPERATURE.....Around 78.",
#         "MIN HUMIDITY........65 percent.",
#         "EYE LEVEL WINDS.....North winds around 6 mph.",
#         "MIXING HEIGHT.......100 ft AGL.",
#         "TRANSPORT WINDS.....Southwest around 12 mph.",
#         "SMOKE DISPERSAL.....Excellent (160000 knot-ft).",
#         "STABILITY CLASS.....1.",
#         "MARINE LAYER........1000.",
#         "HAINES INDEX........2 or very low potential for large plume dominated fire growth.",
# 
#         ".TONIGHT...",
#         "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "MIN TEMPERATURE.....Around 60.",
#         "MAX HUMIDITY........78 percent.",
# 
#         ".SATURDAY...",
#         "SKY/WEATHER.........Sunny (0-5 percent). Chance of showers.",
#         "CWR.................30 percent.",
#         ],
#     },
#     {
#     "name":"FWS_StqInput_7",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct as input",
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":7},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ],
#     "checkStrings": [
#         "IFPS TEST 7",
#         ".TODAY...",
#         "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "MAX TEMPERATURE.....Around 78.",
#         "MIN HUMIDITY........65 percent.",
#         "EYE LEVEL WINDS.....North winds around 6 mph.",
#         "MIXING HEIGHT.......100 ft AGL.",
#         "TRANSPORT WINDS.....Southwest around 12 mph.",
#         "SMOKE DISPERSAL.....Excellent (160000 knot-ft).",
#         "STABILITY CLASS.....1.",
#         "MARINE LAYER........1000.",
#         "HAINES INDEX........2 or very low potential for large plume dominated fire growth.",
# 
#         ".TONIGHT...",
#         "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "MIN TEMPERATURE.....Around 60.",
#         "MAX HUMIDITY........78 percent.",
# 
#         ".SATURDAY...",
#         "SKY/WEATHER.........Sunny (0-5 percent). Chance of showers.",
#         "CWR.................30 percent.",
#         "SMOKE DISPERSAL.....Good (50000 knot-ft).",
#         "STABILITY CLASS.....1.",
#         "MARINE LAYER........2000.",
#         "HAINES INDEX........4 or low potential for large plume dominated fire growth.",
#         "REQUESTED BY...Virgil Middendorf",
#         "TYPE OF REQUEST...Prescribed",
#         ".TAG 20041013.IFPST.01/BYZ",
#         ],
#     },
#     {
#     "name":"FWS_StqInput_8",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct as input",
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":8},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ],
#     "checkStrings": [
#         "IFPS TEST 8",
#         ".TODAY...",
#         "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "MAX TEMPERATURE.....Around 78.",
#         "MIN HUMIDITY........65 percent.",
#         "EYE LEVEL WINDS.....North winds around 6 mph.",
#         "MIXING HEIGHT.......100 ft AGL.",
#         "TRANSPORT WINDS.....Southwest around 12 mph.",
# 
#         ".TONIGHT...",
#         "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "MIN TEMPERATURE.....Around 60.",
#         "MAX HUMIDITY........78 percent.",
#         ],
#     "notCheckStrings": [
#         ".SATURDAY...",
#         ],
#     },
#     {
#     "name":"FWS_StqInput_9",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct as input",
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":9},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ],
#     "checkStrings": [
#         "IFPS TEST 9",
#         ".TONIGHT...",
#         "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "MIN TEMPERATURE.....Around 60.",
#         "MAX HUMIDITY........78 percent.",
# 
#         ".SATURDAY...",
#         "SKY/WEATHER.........Sunny (0-5 percent). Chance of showers.",
#         "CWR.................30 percent.",
#         
#         ],
#     "notCheckStrings": [
#         ".TODAY...",
#         ],
#     },
# 
# 
#     {
#     "name":"FWS_StqInput_10",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct as input",
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":0, "setIssuance": "Morning Update"},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ],
#     "drtHour": 10,
#     "checkStrings": [
#         "IFPS TEST 0",
#         ".REST OF TODAY...",
#         "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "MAX TEMPERATURE.....Around 78.",
#         "EYE LEVEL WINDS.....North winds around 6 mph.",
#         "MIXING HEIGHT.......100 ft AGL.",
#         "TRANSPORT WINDS.....Southwest around 12 mph.",
#         "MIN HUMIDITY........65 percent.",
#         "WIND (20 FT)........North winds around 6 mph with gusts to around 30 mph.",
# 
#         ".SATURDAY...",
#         "SKY/WEATHER.........Sunny (0-5 percent). Chance of showers.",
#         "CWR.................30 percent.",
#         "MAX TEMPERATURE.....Around 79.",
#         "MIN HUMIDITY........68 percent.",
#         "WIND (20 FT)........Northwest winds around 12 mph.",
#         "EYE LEVEL WINDS.....Northwest winds around 12 mph." ,
#             
#         ],
#     },
# 
#     {
#     "name":"FWS_StqInput_11",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct as input",
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":1, "setIssuance": "Afternoon"},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ],
#     "checkStrings": [
#         "IFPS TEST 1",
#         ".SATURDAY...",
#         "SKY/WEATHER.........Sunny (0-5 percent). Chance of showers.",
#         "CWR.................30 percent.",
#         ],
#     "notCheckStrings": [
#         ".TODAY...", ".TONIGHT...",
#         ],
#     },
# 
#     {
#     "name":"FWS_StqInput_12",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct as input",
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":2, "setIssuance": "Evening Update"},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ],
#     "drtHour": 20,
#     "checkStrings": [
#         "IFPS TEST 2",
#         ".REST OF TONIGHT...",
#         "SKY/WEATHER.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "MIN TEMPERATURE.....Around 60.",
#         "MAX HUMIDITY........78 percent.",
# 
#         ".SATURDAY...",
#         "SKY/WEATHER.........Sunny (0-5 percent). Chance of showers.",
#         "CWR.................30 percent.",
#         ],
#     "notCheckStrings": [
#         ".TODAY...",
#         ],
#     },
#     {
#     "name":"FWS_StqInput_13",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct as input",
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":3, "setIssuance": "Early Morning Update"},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ],
#     "drtHour": 2,
#     "checkStrings": [
#         "IFPS TEST 3",
#         ".REST OF TONIGHT...",
#         "CWR.................0 percent",
#         "MIN TEMPERATURE.....Around 40.",
#         "MAX HUMIDITY........60 percent.",
# 
#         ".FRIDAY...",
#         "SKY/WEATHER.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         ],
#     },
# 
#     {
#     "name":"FWS_StqInput_Known_Requestor",
#     "productType":"FWS",
#     "commentary": "Testing an StqProduct with known requesting agency",
#     "cmdLineVars":"_processVariableList",
#     "callVariables": {"testIndex":0},
#     "fileChanges": [
#        ("FWS_<site>_Overrides", "TextUtility", "add", testProcVarList1, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition0, "undo"),
#        ("FWS_<site>_Definition", "TextUtility", "add", definition4, "undo"),
#        ],
#     "checkStrings": [
#             "IFPS TEST 0",
#             ".TODAY...",
#             "SKY/WEATHER.........",
#             "CWR.................",
#             "MAX TEMPERATURE.....",
#             "EYE LEVEL WINDS.....",
#             "MIXING HEIGHT.......",
#             "TRANSPORT WINDS.....",
#             "MIN HUMIDITY........",
#             "WIND (20 FT)........",
#             ".SATURDAY...",
#             "SKY/WEATHER.........",
#             "CWR.................",
#             "MAX TEMPERATURE.....",
#             "MIN HUMIDITY........",
#             "WIND (20 FT)........",
#             "EYE LEVEL WINDS.....",
#         ],
#     },

    {    
    "name":"FWS_CleanUp",
    "commentary": "Clean out grids",
    "productType": None,
    "deleteGrids": [
        ("Fcst", "FreeWind", "SFC", -24, 240),
        ("Fcst", "RH", "SFC", -24, 240),
        ("Fcst", "PoP", "SFC", -24, 240),
        ("Fcst", "MaxT", "SFC", -24, 280),
        ("Fcst", "MinT", "SFC", -24, 240),
        ("Fcst", "T", "SFC", -24, 240),
        ("Fcst", "Td", "SFC", -24, 240),
        ("Fcst", "WindChill", "SFC", -24, 240),
        ("Fcst", "HeatIndex", "SFC", -24, 240),
        ("Fcst", "Wind", "SFC", -24, 240),
        ("Fcst", "Sky", "SFC", -24, 240),
        ("Fcst", "WindGust", "SFC", -24, 240),
        ("Fcst", "Wx", "SFC", -24, 240),
        ("Fcst", "QPF", "SFC", -24, 240),
        ("Fcst", "SnowAmt", "SFC", -24, 240),
        ("Fcst", "MaxRH", "SFC", -24, 240),
        ("Fcst", "MinRH", "SFC", -24, 280),
        ("Fcst", "TransWind", "SFC", -24, 240),
        ("Fcst", "LAL", "SFC", -24, 240),
        ("Fcst", "CWR", "SFC", -24, 240),
        ("Fcst", "Haines", "SFC", -24, 240),
        ("Fcst", "MixHgt", "SFC", -24, 240),
        ("Fcst", "Wind20ft", "SFC", -24, 240),
        ("Fcst", "VentRate", "SFC", -24, 240),
        ("Fcst", "MarineLayer", "SFC", -24, 240),
        ("Fcst", "Stability", "SFC", -24, 240),
        ("Fcst", "HrsOfSun", "SFC", -24, 240),
        ("Fcst", "DSI", "SFC", -24, 240),
        ("Fcst", "RHtrend", "SFC", -24, 240),
        ("Fcst", "Ttrend", "SFC", -24, 240),
        ]
    },
    ]    

def testScript(self, dataMgr, level="Site"):
    gridsStartTime = self.getAbsFromLocal(2010, 1, 1, 6, 0)
    drtTime = self.getAbsFromLocal(2010, 1, 1, 4, 0)
    defaults = {
        "gridsStartTime": gridsStartTime,
        "drtTime": drtTime,
        "orderStrings": 1,
        "internalStrip": 2, # To handle both tabular and narrative output
        }
    # Handle start times for different issuances
    for script in scripts:
        drtHour = script.get("drtHour", None)
        if drtHour is not None:
            script["drtTime"] = self.getAbsFromLocal(2010, 1, 1, drtHour, 0)
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults, level=level)

