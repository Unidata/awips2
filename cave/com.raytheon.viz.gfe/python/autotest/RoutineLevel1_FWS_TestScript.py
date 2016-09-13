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
                   Stability class: 1,1,1
                      Marine layer: 1,1,1
                      Haines Index: 1,1,1
            
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
                   Stability class: 1,1,0
                      Marine layer: 1,1,0
                      Haines Index: 1,1,0

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
                   Stability class: 1,0,0
                      Marine layer: 1,0,0
                      Haines Index: 1,0,0

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
                   Stability class: 1,0,1
                      Marine layer: 1,0,1
                      Haines Index: 1,0,1

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
            ("Sky/weather",     1, self.skyWeather_byTimeRange_compoundPhrase,
             [("SKY", "CLOUDS"), "WEATHER"]),
            ("Begin/end of pcpn", 1, self.pcpnTiming_phrase,
             ["BEGIN", "END", "PCPN"]),
            ("TEMPERATURE",      1, (self.dayOrNight_phrase, ["MaxT", "MinT", 1, 1]),
             [("TEMPERATURE", "TEMP")]),  
            ("HUMIDITY",         1, (self.dayOrNight_phrase, [dayRH, nightRH, 1, 1]),
             [("RH", "HUMIDITY")]),
            ("DEWPOINT",        1, self.td_phrase,  
             ["DEWPOINT"]),
            ("20 FOOT WINDS",   1, wind,            
             ["20", "WIND", ("FT", "FOOT")]),
            ("Eye level winds",  1, self.fireEyeWind_compoundPhrase,
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
            ("Mixing height",   1, self.mixingHgt_phrase,
             ["MIXING"]),
            ("Transport winds", 1, self.transportWind_phrase,
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
            ("Marine layer",    1, self.marineLayer_phrase,
             ["MARINE", "LAYER"]),
            ("Haines Index",    1, self.haines_phrase,
             ["HAINES", "INDEX"]),
            ]

    def _rowList(self, colWidth=1):
        if self._tableWindElementSplit == "no" and colWidth == 7: # 2 hourly
            wind = [("20 ft wind......", self._wind_value),
                    ("20 ft wind gust.", self._windGust_value)]
        elif self._tableWindElementSplit == "no" and colWidth > 7: # 3-4 hourly
            wind = [("20 ft wind......", self._windWithGust_value)]
        else:
            wind = [("20 ft wind dir..", self._windDir_value),  # 1 hourly
                    ("20 ft wind spd..", self._windSpd_value),
                    ("20 ft wind gust.", self._windGust_value)]
        if self._tableEyeWindElementSplit == "no" and colWidth == 7:
            eyewind = [("Eye level wind..", self._eyewind_value),
                       ("Eye level wind..", self._eyewindGust_value)]
        elif self._tableEyeWindElementSplit == "no" and colWidth > 7:
            eyewind = [("Eye level wind..", self._eyewindWithGust_value)]
        else:
            eyewind = [("Eye lvl wnd dir.", self._eyewindDir_value),
                       ("Eye lvl wnd spd.", self._eyewindSpd_value),
                       ("Eye lvl wnd gst.", self._eyewindGust_value)]
        if self._tableRidgeElementSplit == "no" and colWidth >=7:
            ridge = [("Ridgetop wind...", self._ridge_value)]
        else:
            ridge = [("Ridge wnd dir..", self._ridgeDir_value),
                     ("Ridge wnd spd..", self._ridgeSpd_value)]

        # Mixing Height and Transport winds

        if self._tabularMixingHeightUnits == "ft" and colWidth > 4:
            mixLabel = "Mix hgt (ft)...."
            mixMetricLabel = "Mix hgt (m)....."                
        else:
            mixLabel = "Mix hgt (kft)..."
            mixMetricLabel = "Mix hgt (km)...."
            
        if self._transportWindLabel == "mix":
            transLabel = "Mixing wind......"
            transMetricLabel = "Mix wind (m/s).."
            transDirLabel = "Mixng wind dir.."
            transSpdLabel = "Mixng wind spd.."
            transSpdMetricLabel = "Mix wnd spd m/s."
        else:
            transLabel = "Transport wind.."
            transMetricLabel = "Tran wind (m/s)."
            transDirLabel = "Transp wind dir."
            transSpdLabel = "Transp wind spd."
            transSpdMetricLabel = "Trans spd (m/s)."
                            
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
            ("Sky/weather"             , 1,
             [("Sky (%).........", self._sky_value),
              ("Weather cov.....", self._weatherCov_value),
              ("Weather type....", self._weatherType_value),
              ]),
            ("TEMPERATURE"             , 1,[("Temp............", self._temp_value)]),
            ("HUMIDITY"                , 1,[("RH..............", self._rh_value)]),
            ("20 FOOT WINDS"           , 1, wind),
            ("Eye level winds"         , 1, eyewind),
            ("RIDGE TOP WIND"          , 1, ridge),
            ("SMOKE DISPERSION"        , 1, smoke),
            ("Mixing height"           , 1, mix),
            ("Transport winds"         , 1, trans),
            ("DISPERSION INDEX"        , 1,[("Dispersion......", self._dsi_value)]),
            ("LDSI"                    , 1,[("Dispersion idx..", self._ldsi_value)]),
            ("LVORI"                   , 1,[("LVORI...........", self._lvori_value)]),
            ("CWR"                     , 1,[("CWR.............", self._cwr_value)]),
            ("POP"                     , 1,[("Chc of pcpn (%).", self._pop_value)]),
            ("LIGHTNING ACTIVITY LEVEL", 1,[("LAL.............", self._lal_value)]),
            ("Haines Index"            , 1,[("Haines Index....", self._haines_value)]),
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
     "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 3, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",

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
        "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
        "Begin/end of pcpn...",
        "Temperature.........100 at ignition...Max 78.",
        "RH..................100 percent at ignition...Min 65 percent.",
        "Dewpoint............100.",
        "Wind (20 ft)........Winds southwest at 9 mph at ignition...otherwise North winds around 6 mph with gusts to around 30 mph.",
        "Eye level winds.....North winds around 6 mph.",
        "Wind shift..........",
        "Ridgetop wind.......Southwest around 10 mph.",
        "Surrounding ridge...Southwest around 10 mph.",
        "CWR.................0 percent.",
        "Chance of pcpn......0 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL at ignition...otherwise 100 ft AGL.",
        "Transport winds.....Southwest around 12 mph.",
        "Mixing height.......100 ft AGL at ignition...otherwise 100 ft AGL.",
        "Transport winds.....Southwest around 12 mph.",
        "Dispersion index....0.",
        "LVORI...............0.",
        "Dispersion..........0. ",
        "Smoke dispersal.....Excellent /160000 knot-ft/ at ignition. Max...excellent /160000 knot-ft/.",
        "Stability class.....1.",
        "Marine layer........1000.",
        "Haines Index........2 or very low potential for large plume dominated fire growth at ignition...max 2.",

        "Time (EST)      1PM 2PM 3PM 4PM 5PM",
        "Sky (%).........100 100 100 100 100",
        "Weather cov.....PTY PTY PTY PTY PTY",
        "Weather type....FOG FOG FOG FOG FOG ",
        "Temp............100 100 100 100 100",
        "RH..............100 100 100 100 100",
        "20 ft wind dir..SW  SW  SW  SW  SW ",
        "20 ft wind spd..7   7   7   7   7 ",
        "20 ft wind gust.30  30  30  30  30 ",
        "Eye lvl wnd dir.SW  SW  SW  SW  SW",
        "Eye lvl wnd spd.7   7   7   7   7 ",
        "Eye lvl wnd gst.30  30  30  30  30 ",
        "Ridge wnd dir..SW  SW  SW  SW  SW",
        "Ridge wnd spd..10  10  10  10  10",
        "Mix hgt (kft)...0.1 0.1 0.1 0.1 0.1 ",
        "Transp wind dir.SW  SW  SW  SW  SW ",
        "Transp wind spd.12  12  12  12  12",
        "Mix hgt (kft)...0.1 0.1 0.1 0.1 0.1 ",
        "Transp wind dir.SW  SW  SW  SW  SW",
        "Transp wind spd.12  12  12  12  12",
        "Dispersion......0   0   0   0   0 ",
        "Dispersion idx..0   0   0   0   0 ",
        "LVORI...........0   0   0   0   0 ",
        "CWR.............0   0   0   0   0",
        "Chc of pcpn (%).0   0   0   0   0",
        "LAL.............1   1   1   1   1",
        "Haines Index....2   2   2   2   2",

        ".TONIGHT...",

        "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
        "Begin/end of pcpn...",
        "Temperature.........Min 60.",
        "RH..................Max 78 percent.",
        "Dewpoint............95.",
        "Wind (20 ft)........Very windy. Northeast winds around 46 mph.",
        "Eye level winds.....Very windy. Northeast winds around 46 mph.",
        "Wind shift..........",
        "Ridgetop wind.......West around 5 mph.",
        "Surrounding ridge...West around 5 mph.",
        "CWR.................20 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................2.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....West around 6 mph.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....West around 6 mph.",
        "Dispersion index....2.",
        "LVORI...............2.",
        "Dispersion..........2.",
        "Smoke dispersal.....Excellent (100000 knot-ft).",
        "Stability class.....2.",
        "Marine layer........1000.",
        "Haines Index........3 or very low potential for large plume dominated fire growth.",

        "Time (EST)      6 PM   8 PM   10 PM  Midngt 2 AM   4 AM",
        "Sky (%).........95     95     95     95     95     95",
        "Weather cov.....WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD ",
        "Weather type....TSTORM TSTORM TSTORM TSTORM TSTORM TSTORM ",
        "Temp............95     95     95     95     95     95",
        "RH..............95     95     95     95     95     95",
        "20 ft wind......SE 30  SE 30  SE 30  SE 30  SE 30  SE 30 ",
        "20 ft wind gust.",
        "Eye level wind..SE 30  SE 30  SE 30  SE 30  SE 30  SE 30",
        "Eye level wind..",
        "Ridgetop wind...W 5    W 5    W 5    W 5    W 5    W 5",
        "Mix hgt (ft)....100    100    100    100    100    100",
        "Transport wind..W 6    W 6    W 6    W 6    W 6    W 6",
        "Mix hgt (ft)....100    100    100    100    100    100",
        "Transport wind..W 6    W 6    W 6    W 6    W 6    W 6",
        "Dispersion......2      2      2      2      2      2",
        "Dispersion idx..2      2      2      2      2      2",
        "LVORI...........2      2      2      2      2      2",
        "CWR.............20     20     20     20     20     20",
        "Chc of pcpn (%).90     90     90     90     90     90",
        "LAL.............2      2      2      2      2      2",
        "Haines Index....3      3      3      3      3      3",

        ".SATURDAY...",

        "Sky/weather.........Sunny (0-5 percent). Chance of rain showers.",
        "Begin/end of pcpn...",
        "Temperature.........Max 79.",
        "RH..................Min 68 percent.",
        "Dewpoint............0.",
        "Wind (20 ft)........Northwest winds around 12 mph.",
        "Eye level winds.....Northwest winds around 12 mph.",
        "Wind shift..........",
        "Ridgetop wind.......Northwest around 10 mph.",
        "Surrounding ridge...Northwest around 10 mph.",
        "CWR.................30 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Northwest around 12 mph.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Northwest around 12 mph.",
        "Dispersion index....6.",
        "LVORI...............6.",
        "Dispersion..........6.",
        "Smoke dispersal.....Good (50000 knot-ft).",
        "Stability class.....1.",
        "Marine layer........2000.",
        "Haines Index........4 or low potential for large plume dominated fire growth.",

        "Time (EST)      6 AM      9 AM      Noon      3 PM",
        "Sky (%).........0         0         0         0 ",
        "Weather cov.....CHANCE    CHANCE    CHANCE    CHANCE ",
        "Weather type....RNSHWR    RNSHWR    RNSHWR    RNSHWR",
        "Temp............0         0         0         0",
        "RH..............0         0         0         0",
        "20 ft wind......NW 26     NW 26     NW 26     NW 26",
        "Eye level wind..NW 26     NW 26     NW 26     NW 26",
        "Ridgetop wind...NW 10     NW 10     NW 10     NW 10",
        "Mix hgt (ft)....100       100       100       100",
        "Transport wind..NW 12     NW 12     NW 12     NW 12",
        "Mix hgt (ft)....100       100       100       100",
        "Transport wind..NW 12     NW 12     NW 12     NW 12",
        "Dispersion......6         6         6         6 ",
        "Dispersion idx..6         6         6         6",
        "LVORI...........6         6         6         6 ",
        "CWR.............30        30        30        30",
        "Chc of pcpn (%).90        90        90        90",
        "LAL.............3         3         3         3 ",
        "Haines Index....4         4         4         4",

        "$$",
        "FORECASTER...Forecaster C",
        "Requested by...YYYY",
        "Type of request...Prescribed",
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
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Morning Update', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 2, ('Tab Hrs', 'tonightTableRes'): 3, ('Tab Hrs', 'tomorrowTableRes'): 4, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",
    "drtHour": 10,
    "checkStrings": [
        "Forecast is based on ignition time of 1300 EST on January 01.",
        "If conditions become unrepresentative...contact the National Weather Service.",
        ".REST OF TODAY...",

        "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
        "Begin/end of pcpn...",
        "Temperature.........100 at ignition...Max 78.",
        "RH..................100 percent at ignition...Min 65 percent.",
        "Dewpoint............100.",
        "Wind (20 ft)........Winds southwest at 9 mph at ignition...otherwise North winds around 6 mph with gusts to around 30 mph.",
        "Eye level winds.....North winds around 6 mph.",
        "Wind shift..........",
        "Ridgetop wind.......Southwest around 10 mph.",
        "Surrounding ridge...Southwest around 10 mph.",
        "CWR.................0 percent.",
        "Chance of pcpn......0 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL at ignition...otherwise 100 ft AGL.",
        "Transport winds.....Southwest around 12 mph.",
        "Mixing height.......100 ft AGL at ignition...otherwise 100 ft AGL.",
        "Transport winds.....Southwest around 12 mph.",
        "Dispersion index....0.",
        "LVORI...............0.",
        "Dispersion..........0.",
        "Smoke dispersal.....Excellent /160000 knot-ft/ at ignition. Max...excellent /160000 knot-ft/.",
        "Stability class.....1.",
        "Marine layer........1000.",
        "Haines Index........2 or very low potential for large plume dominated fire growth at ignition...max 2.",

        "Time (EST)      1 PM   3 PM   5 PM",
        "Sky (%).........100    100    100",
        "Weather cov.....PATCHY PATCHY PATCHY",
        "Weather type....FOG    FOG    FOG",
        "Temp............100    100    100",
        "RH..............100    100    100",
        "20 ft wind......SW 7   SW 7   SW 7",
        "20 ft wind gust.30     30     30",
        "Eye level wind..SW 7   SW 7   SW 7",
        "Eye level wind..30     30     30",
        "Ridgetop wind...SW 10  SW 10  SW 10",
        "Mix hgt (ft)....100    100    100",
        "Transport wind..SW 12  SW 12  SW 12",
        "Mix hgt (ft)....100    100    100",
        "Transport wind..SW 12  SW 12  SW 12",
        "Dispersion......0      0      0",
        "Dispersion idx..0      0      0",
        "LVORI...........0      0      0",
        "CWR.............0      0      0",
        "Chc of pcpn (%).0      0      0",
        "LAL.............1      1      1",
        "Haines Index....2      2      2",

        ".TONIGHT...",

        "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
        "Begin/end of pcpn...",
        "Temperature.........Min 60.",
        "RH..................Max 78 percent.",
        "Dewpoint............95.",
        "Wind (20 ft)........Very windy. Northeast winds around 46 mph.",
        "Eye level winds.....Very windy. Northeast winds around 46 mph.",
        "Wind shift..........",
        "Ridgetop wind.......West around 5 mph.",
        "Surrounding ridge...West around 5 mph.",
        "CWR.................20 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................2.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....West around 6 mph.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....West around 6 mph.",
        "Dispersion index....2.",
        "LVORI...............2.",
        "Dispersion..........2.",
        "Smoke dispersal.....Excellent (100000 KNOT-ft).",
        "Stability class.....2.",
        "Marine layer........1000.",
        "Haines Index........3 or very low potential for large plume dominated fire growth.",

        "Time (EST)      6 PM      9 PM      Midngt    3 AM",
        "Sky (%).........95        95        95        95",
        "Weather cov.....WIDSPD    WIDSPD    WIDSPD    WIDSPD",
        "Weather type....TSTORM    TSTORM    TSTORM    TSTORM",
        "Temp............95        95        95        95",
        "RH..............95        95        95        95",
        "20 ft wind......SE 30     SE 30     SE 30     SE 30",
        "Eye level wind..SE 30     SE 30     SE 30     SE 30",
        "Ridgetop wind...W 5       W 5       W 5       W 5",
        "Mix hgt (ft)....100       100       100       100",
        "Transport wind..W 6       W 6       W 6       W 6",
        "Mix hgt (ft)....100       100       100       100",
        "Transport wind..W 6       W 6       W 6       W 6",
        "Dispersion......2         2         2         2",
        "Dispersion idx..2         2         2         2",
        "LVORI...........2         2         2         2",
        "CWR.............20        20        20        20",
        "Chc of pcpn (%).90        90        90        90",
        "LAL.............2         2         2         2",
        "Haines Index....3         3         3         3",

        ".SATURDAY...",

        "Sky/weather.........Sunny (0-5 percent). Chance of rain showers.",
        "Begin/end of pcpn...",
        "Temperature.........Max 79.",
        "RH..................Min 68 percent.",
        "Dewpoint............0.",
        "Wind (20 ft)........Northwest winds around 12 mph.",
        "Eye level winds.....Northwest winds around 12 mph.",
        "Wind shift..........",
        "Ridgetop wind.......Northwest around 10 mph.",
        "Surrounding ridge...Northwest around 10 mph.",
        "CWR.................30 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Northwest around 12 mph.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Northwest around 12 mph.",
        "Dispersion index....6.",
        "LVORI...............6.",
        "Dispersion..........6.",
        "Smoke dispersal.....Good (50000 knot-ft).",
        "Stability class.....1.",
        "Marine layer........2000.",
        "Haines Index........4 or low potential for large plume dominated fire growth.",

        "Time (EST)      6 AM         10 AM        2 PM",
        "Sky (%).........0            0            0",
        "Weather cov.....CHANCE       CHANCE       CHANCE",
        "Weather type....RNSHWR       RNSHWR       RNSHWR",
        "Temp............0            0            0",
        "RH..............0            0            0",
        "20 ft wind......NW 26        NW 26        NW 26",
        "Eye level wind..NW 26        NW 26        NW 26",
        "Ridgetop wind...NW 10        NW 10        NW 10",
        "Mix hgt (ft)....100          100          100",
        "Transport wind..NW 12        NW 12        NW 12",
        "Mix hgt (ft)....100          100          100",
        "Transport wind..NW 12        NW 12        NW 12",
        "Dispersion......6            6            6",
        "Dispersion idx..6            6            6",
        "LVORI...........6            6            6",
        "CWR.............30           30           30",
        "Chc of pcpn (%).90           90           90",
        "LAL.............3            3            3",
        "Haines Index....4            4            4",

        "$$",
        "FORECASTER...Forecaster C",
        "Requested by...YYYY",
        "Type of request...Prescribed",
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
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Afternoon', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 3, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",

    "checkStrings": [
        "Forecast is based on ignition time of 1300 EST on January 01.",
        "If conditions become unrepresentative...contact the National Weather Service.",

        ".DISCUSSION...",

        ".TONIGHT...",

        "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
        "Begin/end of pcpn...",
        "Temperature.........Min 60.",
        "RH..................Max 78 percent.",
        "Dewpoint............95.",
        "Wind (20 ft)........Very windy. Northeast winds around 46 mph.",
        "Eye level winds.....Very windy. Northeast winds around 46 mph.",
        "Wind shift..........",
        "Ridgetop wind.......West around 5 mph.",
        "Surrounding ridge...West around 5 mph.",
        "CWR.................20 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................2.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....West around 6 mph.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....West around 6 mph.",
        "Dispersion index....2.",
        "LVORI...............2.",
        "Dispersion..........2.",
        "Smoke dispersal.....Excellent (100000 knot-ft).",
        "Stability class.....2.",
        "Marine layer........1000.",
        "Haines Index........3 or very low potential for large plume dominated fire growth.",

        "Time (EST)      6 PM   8 PM   10 PM  Midngt 2 AM   4 AM",
        "Sky (%).........95     95     95     95     95     95",
        "Weather cov.....WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD",
        "Weather type....TSTORM TSTORM TSTORM TSTORM TSTORM TSTORM",
        "Temp............95     95     95     95     95     95",
        "RH..............95     95     95     95     95     95",
        "20 ft wind......SE 30  SE 30  SE 30  SE 30  SE 30  SE 30",
        "20 ft wind gust.",
        "Eye level wind..SE 30  SE 30  SE 30  SE 30  SE 30  SE 30",
        "Eye level wind..",
        "Ridgetop wind...W 5    W 5    W 5    W 5    W 5    W 5",
        "Mix hgt (ft)....100    100    100    100    100    100",
        "Transport wind..W 6    W 6    W 6    W 6    W 6    W 6",
        "Mix hgt (ft)....100    100    100    100    100    100",
        "Transport wind..W 6    W 6    W 6    W 6    W 6    W 6",
        "Dispersion......2      2      2      2      2      2",
        "Dispersion idx..2      2      2      2      2      2",
        "LVORI...........2      2      2      2      2      2",
        "CWR.............20     20     20     20     20     20",
        "Chc of pcpn (%).90     90     90     90     90     90",
        "LAL.............2      2      2      2      2      2",
        "Haines Index....3      3      3      3      3      3",

        ".SATURDAY...",

        "Sky/weather.........Sunny (0-5 percent). Chance of rain showers.",
        "Begin/end of pcpn...",
        "Temperature.........Max 79.",
        "RH..................Min 68 percent.",
        "Dewpoint............0.",
        "Wind (20 ft)........Northwest winds around 12 mph.",
        "Eye level winds.....Northwest winds around 12 mph.",
        "Wind shift..........",
        "Ridgetop wind.......Northwest around 10 mph.",
        "Surrounding ridge...Northwest around 10 mph.",
        "CWR.................30 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Northwest around 12 mph.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Northwest around 12 mph.",
        "Dispersion index....6.",
        "LVORI...............6.",
        "Dispersion..........6.",
        "Smoke dispersal.....Good (50000 knot-ft).",
        "Stability class.....1.",
        "Marine layer........2000.",
        "Haines Index........4 or low potential for large plume dominated fire growth.",

        "Time (EST)      6 AM      9 AM      Noon      3 PM",
        "Sky (%).........0         0         0         0",
        "Weather cov.....CHANCE    CHANCE    CHANCE    CHANCE",
        "Weather type....RNSHWR    RNSHWR    RNSHWR    RNSHWR",
        "Temp............0         0         0         0",
        "RH..............0         0         0         0",
        "20 ft wind......NW 26     NW 26     NW 26     NW 26",
        "Eye level wind..NW 26     NW 26     NW 26     NW 26",
        "Ridgetop wind...NW 10     NW 10     NW 10     NW 10",
        "Mix hgt (ft)....100       100       100       100",
        "Transport wind..NW 12     NW 12     NW 12     NW 12",
        "Mix hgt (ft)....100       100       100       100",
        "Transport wind..NW 12     NW 12     NW 12     NW 12",
        "Dispersion......6         6         6         6",
        "Dispersion idx..6         6         6         6",
        "LVORI...........6         6         6         6",
        "CWR.............30        30        30        30",
        "Chc of pcpn (%).90        90        90        90",
        "LAL.............3         3         3         3",
        "Haines Index....4         4         4         4",

        "$$",
        "FORECASTER...Forecaster C",
        "Requested by...YYYY",
        "Type of request...Prescribed",
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
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Early Morning Update', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 3, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",
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

        "Sky/weather.........Patchy dense fog.",
        "Begin/end of pcpn...",
        "Temperature.........Min 40.",
        "RH..................Max 60 percent.",
        "Dewpoint............MISSING.",
        "Wind (20 ft)........North winds around 6 mph with gusts to around 30 mph.",
        "Eye level winds.....North winds around 6 mph.",
        "Wind shift..........",
        "Ridgetop wind.......Southwest around 10 mph.",
        "Surrounding ridge...Southwest around 10 mph.",
        "CWR.................0 percent.",
        "Chance of pcpn......0 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Southwest around 12 mph.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Southwest around 12 mph.",
        "Dispersion index....0.",
        "LVORI...............0.",
        "Dispersion..........0.",
        "Smoke dispersal.....Excellent (160000 knot-ft).",
        "Stability class.....1.",
        "Marine layer........MISSING.",
        "Haines Index........2 or very low potential for large plume dominated fire growth.",

        "Time (EST)      2 AM   4 AM",
        "Sky (%).........M      M",
        "Weather cov.....PATCHY PATCHY",
        "Weather type....FOG    FOG",
        "Temp............M      M ",
        "RH..............M      M ",
        "20 ft wind......SW 7   SW 7",
        "20 ft wind gust.30     30",
        "Eye level wind..SW 7   SW 7",
        "Eye level wind..30     30",
        "Ridgetop wind...SW 10  SW 10",
        "Mix hgt (ft)....100    100",
        "Transport wind..SW 12  SW 12",
        "Mix hgt (ft)....100    100",
        "Transport wind..SW 12  SW 12",
        "Dispersion......0      0",
        "Dispersion idx..0      0",
        "LVORI...........0      0",
        "CWR.............0      0",
        "Chc of pcpn (%).0      0",
        "LAL.............1      1",
        "Haines Index....2      2",

        ".FRIDAY...",

        "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
        "Begin/end of pcpn...",
        "Temperature.........100 at ignition...Max 78.",
        "RH..................100 percent at ignition...Min 65 percent.",
        "Dewpoint............100.",
        "Wind (20 ft)........Winds southwest at 9 mph at ignition...otherwise North winds around 6 mph with gusts to around 30 mph.",
        "Eye level winds.....North winds around 6 mph.",
        "Wind shift..........",
        "Ridgetop wind.......Southwest around 10 mph.",
        "Surrounding ridge...Southwest around 10 mph.",
        "CWR.................0 percent.",
        "Chance of pcpn......0 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL at ignition...otherwise 100 ft AGL.",
        "Transport winds.....Southwest around 12 mph.",
        "Mixing height.......100 ft AGL at ignition...otherwise 100 ft AGL.",
        "Transport winds.....Southwest around 12 mph.",
        "Dispersion index....0.",
        "LVORI...............0.",
        "Dispersion..........0.",
        "Smoke dispersal.....Excellent /160000 knot-ft/ at ignition. Max...excellent /160000 knot-ft/.",
        "Stability class.....1.",
        "Marine layer........1000.",
        "Haines Index........2 or very low potential for large plume dominated fire growth at ignition...max 2.",

        "Time (EST)      6 AM      9 AM      Noon      3 PM ",
        "Sky (%).........100       100       100       100",
        "Weather cov.....PATCHY    PATCHY    PATCHY    PATCHY",
        "Weather type....FOG       FOG       FOG       FOG",
        "Temp............100       100       100       100",
        "RH..............100       100       100       100",
        "20 ft wind......SW 7G30   SW 7G30   SW 7G30   SW 7G30",
        "Eye level wind..SW 7G30   SW 7G30   SW 7G30   SW 7G30",
        "Ridgetop wind...SW 10     SW 10     SW 10     SW 10",
        "Mix hgt (ft)....100       100       100       100",
        "Transport wind..SW 12     SW 12     SW 12     SW 12",
        "Mix hgt (ft)....100       100       100       100",
        "Transport wind..SW 12     SW 12     SW 12     SW 12",
        "Dispersion......0         0         0         0",
        "Dispersion idx..0         0         0         0",
        "LVORI...........0         0         0         0",
        "CWR.............0         0         0         0",
        "Chc of pcpn (%).0         0         0         0",
        "LAL.............1         1         1         1",
        "Haines Index....2         2         2         2",

        "$$",
        "FORECASTER...Forecaster C",
        "Requested by...YYYY",
        "Type of request...Prescribed",
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
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Next Day', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 3, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",

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

        "Sky/weather.........Sunny (0-5 percent). Chance of rain showers.",
        "Begin/end of pcpn...",
        "Temperature.........Max 79.",
        "RH..................Min 68 percent.",
        "Dewpoint............0.",
        "Wind (20 ft)........Northwest winds around 12 mph.",
        "Eye level winds.....Northwest winds around 12 mph.",
        "Wind shift..........",
        "Ridgetop wind.......Northwest around 10 mph.",
        "Surrounding ridge...Northwest around 10 mph.",
        "CWR.................30 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Northwest around 12 mph.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Northwest around 12 mph.",
        "Dispersion index....6.",
        "LVORI...............6.",
        "Dispersion..........6.",
        "Smoke dispersal.....Good (50000 knot-ft).",
        "Stability class.....1.",
        "Marine layer........2000.",
        "Haines Index........4 or low potential for large plume dominated fire growth.",

        "Time (EST)      6AM 7AM 8AM 9AM 10A 11A 12P 1PM 2PM 3PM 4PM 5PM",
        "Sky (%).........0   0   0   0   0   0   0   0   0   0   0   0",
        "Weather cov.....CHC CHC CHC CHC CHC CHC CHC CHC CHC CHC CHC CHC",
        "Weather type....RW  RW  RW  RW  RW  RW  RW  RW  RW  RW  RW  RW",
        "Temp............0   0   0   0   0   0   0   0   0   0   0   0",
        "RH..............0   0   0   0   0   0   0   0   0   0   0   0",
        "20 ft wind dir..NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW",
        "20 ft wind spd..26  26  26  26  26  26  26  26  26  26  26  26",
        "20 ft wind gust.",
        "Eye lvl wnd dir.NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW",
        "Eye lvl wnd spd.26  26  26  26  26  26  26  26  26  26  26  26",
        "Eye lvl wnd gst.",
        "Ridge wnd dir..NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW",
        "Ridge wnd spd..10  10  10  10  10  10  10  10  10  10  10  10",
        "Mix hgt (kft)...0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "Transp wind dir.NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW",
        "Transp wind spd.12  12  12  12  12  12  12  12  12  12  12  12",
        "Mix hgt (kft)...0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "Transp wind dir.NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW  NW",
        "Transp wind spd.12  12  12  12  12  12  12  12  12  12  12  12",
        "Dispersion......6   6   6   6   6   6   6   6   6   6   6   6",
        "Dispersion idx..6   6   6   6   6   6   6   6   6   6   6   6",
        "LVORI...........6   6   6   6   6   6   6   6   6   6   6   6",
        "CWR.............30  30  30  30  30  30  30  30  30  30  30  30",
        "Chc of pcpn (%).90  90  90  90  90  90  90  90  90  90  90  90",
        "LAL.............3   3   3   3   3   3   3   3   3   3   3   3",
        "Haines Index....4   4   4   4   4   4   4   4   4   4   4   4",

        ".TONIGHT...",

        "Sky/weather.........MOSTLY CLEAR (10-20 percent). FREQUENT LIGHT RAIN.",
        "Begin/end of pcpn...",
        "Temperature.........Min 68.",
        "RH..................Max 80 percent.",
        "Dewpoint............15.",
        "Wind (20 ft)........LIGHT WINDS.",
        "Eye level winds.....LIGHT WINDS.",
        "Wind shift..........",
        "Ridgetop wind.......Northwest around 10 mph.",
        "Surrounding ridge...Northwest around 10 mph.",
        "CWR.................30 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Northwest around 12 mph.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Northwest around 12 mph.",
        "Dispersion index....1.",
        "LVORI...............1.",
        "Dispersion..........1.",
        "Smoke dispersal.....FAIR (20000 knot-ft).",
        "Stability class.....3.",
        "Marine layer........2000.",
        "Haines Index........6 OR HIGH potential for large plume dominated fire growth.",

        "Time (EST)      6 PM   8 PM   10 PM  Midngt 2 AM   4 AM",
        "Sky (%).........15     15     15     15     15     15",
        "Weather cov.....FRQNT  FRQNT  FRQNT  FRQNT  FRQNT  FRQNT",
        "Weather type....RAIN   RAIN   RAIN   RAIN   RAIN   RAIN",
        "Temp............15     15     15     15     15     15",
        "RH..............15     15     15     15     15     15",
        "20 ft wind......W 33   W 33   W 33   W 33   W 33   W 33",
        "20 ft wind gust.",
        "Eye level wind..W 33   W 33   W 33   W 33   W 33   W 33",
        "Eye level wind..",
        "Ridgetop wind...NW 10  NW 10  NW 10  NW 10  NW 10  NW 10",
        "Mix hgt (ft)....100    100    100    100    100    100",
        "Transport wind..NW 12  NW 12  NW 12  NW 12  NW 12  NW 12",
        "Mix hgt (ft)....100    100    100    100    100    100",
        "Transport wind..NW 12  NW 12  NW 12  NW 12  NW 12  NW 12",
        "Dispersion......1      1      1      1      1      1",
        "Dispersion idx..1      1      1      1      1      1",
        "LVORI...........1      1      1      1      1      1",
        "CWR.............30     30     30     30     30     30",
        "Chc of pcpn (%).90     90     90     90     90     90",
        "LAL.............4      4      4      4      4      4",
        "Haines Index....6      6      6      6      6      6",

        ".SUNDAY...",

        "Sky/weather.........Mostly sunny (25-35 percent). Widespread light freezing rain.",
        "Begin/end of pcpn...",
        "Temperature.........Max 78.",
        "RH..................Min 70 percent.",
        "Dewpoint............30.",
        "Wind (20 ft)........Hurricane force winds. East winds around 144 mph.",
        "Eye level winds.....Hurricane force winds. East winds around 144 mph.",
        "Wind shift..........",
        "Ridgetop wind.......West around 25 mph.",
        "Surrounding ridge...West around 25 mph.",
        "CWR.................45 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................1.",
        "Mixing height.......4000 ft AGL.",
        "Transport winds.....West around 23 mph.",
        "Mixing height.......4000 ft AGL.",
        "Transport winds.....West around 23 mph.",
        "Dispersion index....5.",
        "LVORI...............5.",
        "Dispersion..........5.",
        "Smoke dispersal.....Excellent (70000 knot-ft).",
        "Stability class.....4.",
        "Marine layer........4000.",
        "Haines Index........2 or very low potential for large plume dominated fire growth.",

        "Time (EST)      6 AM      9 AM      Noon      3 PM",
        "Sky (%).........30        30        30        30",
        "Weather cov.....WIDSPD    WIDSPD    WIDSPD    WIDSPD",
        "Weather type....FZRAIN    FZRAIN    FZRAIN    FZRAIN",
        "Temp............30        30        30        30",
        "RH..............30        30        30        30",
        "20 ft wind......SW 37     SW 37     SW 37     SW 37",
        "Eye level wind..SW 37     SW 37     SW 37     SW 37",
        "Ridgetop wind...W 25      W 25      W 25      W 25",
        "Mix hgt (ft)....4000      4000      4000      4000",
        "Transport wind..W 23      W 23      W 23      W 23",
        "Mix hgt (ft)....4000      4000      4000      4000",
        "Transport wind..W 23      W 23      W 23      W 23",
        "Dispersion......5         5         5         5",
        "Dispersion idx..5         5         5         5",
        "LVORI...........5         5         5         5",
        "CWR.............50        50        50        50",
        "Chc of pcpn (%).90        90        90        90",
        "LAL.............5         5         5         5",
        "Haines Index....2         2         2         2",

        "$$",
        "FORECASTER...Forecaster C",
        "Requested by...YYYY",
        "Type of request...Prescribed",
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
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Afternoon Update', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 4, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",

    "checkStrings": [

        "Spot Forecast for xxxx...Agency 1",
        "National Weather Service Tampa Bay Ruskin FL",
        "200 PM EST Fri Jan 1 2010",

        "Forecast is based on ignition time of 1300 EST on January 01.",
        "If conditions become unrepresentative...contact the National Weather Service.",

        ".DISCUSSION...",

        ".REST OF TODAY...",

        "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
        "Begin/end of pcpn...",
        "Temperature.........Max 78.",
        "RH..................Min 65 percent.",
        "Dewpoint............100.",
        "Wind (20 ft)........North winds around 6 mph with gusts to around 30 mph.",
        "Eye level winds.....North winds around 6 mph.",
        "Wind shift..........",
        "Ridgetop wind.......Southwest around 10 mph.",
        "Surrounding ridge...Southwest around 10 mph.",
        "CWR.................0 percent.",
        "Chance of pcpn......0 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Southwest around 12 mph.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Southwest around 12 mph.",
        "Dispersion index....0.",
        "LVORI...............0.",
        "Dispersion..........0.",
        "Smoke dispersal.....Excellent (160000 knot-ft).",
        "Stability class.....1.",
        "Marine layer........1000.",
        "Haines Index........2 or very low potential for large plume dominated fire growth.",

        "Time (EST)      2PM 3PM 4PM 5PM",
        "Sky (%).........100 100 100 100",
        "Weather cov.....PTY PTY PTY PTY",
        "Weather type....FOG FOG FOG FOG",
        "Temp............100 100 100 100",
        "RH..............100 100 100 100",
        "20 ft wind dir..SW  SW  SW  SW",
        "20 ft wind spd..7   7   7   7",
        "20 ft wind gust.30  30  30  30",
        "Eye lvl wnd dir.SW  SW  SW  SW",
        "Eye lvl wnd spd.7   7   7   7",
        "Eye lvl wnd gst.30  30  30  30",
        "Ridge wnd dir..SW  SW  SW  SW",
        "Ridge wnd spd..10  10  10  10",
        "Mix hgt (kft)...0.1 0.1 0.1 0.1",
        "Transp wind dir.SW  SW  SW  SW",
        "Transp wind spd.12  12  12  12",
        "Mix hgt (kft)...0.1 0.1 0.1 0.1",
        "Transp wind dir.SW  SW  SW  SW",
        "Transp wind spd.12  12  12  12",
        "Dispersion......0   0   0   0",
        "Dispersion idx..0   0   0   0",
        "LVORI...........0   0   0   0",
        "CWR.............0   0   0   0",
        "Chc of pcpn (%).0   0   0   0",
        "LAL.............1   1   1   1",
        "Haines Index....2   2   2   2",

        ".TONIGHT...",

        "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
        "Begin/end of pcpn...",
        "Temperature.........Min 60.",
        "RH..................Max 78 percent.",
        "Dewpoint............95.",
        "Wind (20 ft)........Very windy. Northeast winds around 46 mph.",
        "Eye level winds.....Very windy. Northeast winds around 46 mph.",
        "Wind shift..........",
        "Ridgetop wind.......West around 5 mph.",
        "Surrounding ridge...West around 5 mph.",
        "CWR.................20 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................2.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....West around 6 mph.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....West around 6 mph.",
        "Dispersion index....2.",
        "LVORI...............2.",
        "Dispersion..........2.",
        "Smoke dispersal.....Excellent (100000 knot-ft).",
        "Stability class.....2.",
        "Marine layer........1000.",
        "Haines Index........3 or very low potential for large plume dominated fire growth.",

        "Time (EST)      6 PM   8 PM   10 PM  Midngt 2 AM   4 AM",
        "Sky (%).........95     95     95     95     95     95",
        "Weather cov.....WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD",
        "Weather type....TSTORM TSTORM TSTORM TSTORM TSTORM TSTORM",
        "Temp............95     95     95     95     95     95",
        "RH..............95     95     95     95     95     95",
        "20 ft wind......SE 30  SE 30  SE 30  SE 30  SE 30  SE 30",
        "20 ft wind gust.",
        "Eye level wind..SE 30  SE 30  SE 30  SE 30  SE 30  SE 30",
        "Eye level wind..",
        "Ridgetop wind...W 5    W 5    W 5    W 5    W 5    W 5",
        "Mix hgt (ft)....100    100    100    100    100    100",
        "Transport wind..W 6    W 6    W 6    W 6    W 6    W 6",
        "Mix hgt (ft)....100    100    100    100    100    100",
        "Transport wind..W 6    W 6    W 6    W 6    W 6    W 6",
        "Dispersion......2      2      2      2      2      2",
        "Dispersion idx..2      2      2      2      2      2",
        "LVORI...........2      2      2      2      2      2",
        "CWR.............20     20     20     20     20     20",
        "Chc of pcpn (%).90     90     90     90     90     90",
        "LAL.............2      2      2      2      2      2",
        "Haines Index....3      3      3      3      3      3",

        ".SATURDAY...",

        "Sky/weather.........Sunny (0-5 percent). Chance of rain showers.",
        "Begin/end of pcpn...",
        "Temperature.........Max 79.",
        "RH..................Min 68 percent.",
        "Dewpoint............0.",
        "Wind (20 ft)........Northwest winds around 12 mph.",
        "Eye level winds.....Northwest winds around 12 mph.",
        "Wind shift..........",
        "Ridgetop wind.......Northwest around 10 mph.",
        "Surrounding ridge...Northwest around 10 mph.",
        "CWR.................30 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Northwest around 12 mph.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Northwest around 12 mph.",
        "Dispersion index....6.",
        "LVORI...............6.",
        "Dispersion..........6.",
        "Smoke dispersal.....Good (50000 knot-ft).",
        "Stability class.....1.",
        "Marine layer........2000.",
        "Haines Index........4 or low potential for large plume dominated fire growth.",

        "Time (EST)      6 AM         10 AM        2 PM",
        "Sky (%).........0            0            0",
        "Weather cov.....CHANCE       CHANCE       CHANCE",
        "Weather type....RNSHWR       RNSHWR       RNSHWR",
        "Temp............0            0            0",
        "RH..............0            0            0",
        "20 ft wind......NW 26        NW 26        NW 26",
        "Eye level wind..NW 26        NW 26        NW 26",
        "Ridgetop wind...NW 10        NW 10        NW 10",
        "Mix hgt (ft)....100          100          100",
        "Transport wind..NW 12        NW 12        NW 12",
        "Mix hgt (ft)....100          100          100",
        "Transport wind..NW 12        NW 12        NW 12",
        "Dispersion......6            6            6",
        "Dispersion idx..6            6            6",
        "LVORI...........6            6            6",
        "CWR.............30           30           30",
        "Chc of pcpn (%).90           90           90",
        "LAL.............3            3            3",
        "Haines Index....4            4            4",

        "$$",
        "FORECASTER...Forecaster C",
        "Requested by...YYYY",
        "Type of request...Prescribed",
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
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Evening Update', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 4, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",

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

        "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
        "Begin/end of pcpn...",
        "Temperature.........Min 60.",
        "RH..................Max 78 percent.",
        "Dewpoint............95.",
        "Wind (20 ft)........Very windy. Northeast winds around 46 mph.",
        "Eye level winds.....Very windy. Northeast winds around 46 mph.",
        "Wind shift..........",
        "Ridgetop wind.......West around 5 mph.",
        "Surrounding ridge...West around 5 mph.",
        "CWR.................20 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................2.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....West around 6 mph.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....West around 6 mph.",
        "Dispersion index....2.",
        "LVORI...............2.",
        "Dispersion..........2.",
        "Smoke dispersal.....Excellent (100000 knot-ft).",
        "Stability class.....2.",
        "Marine layer........1000.",
        "Haines Index........3 or very low potential for large plume dominated fire growth.",

        "Time (EST)      10 PM  Midngt 2 AM   4 AM",
        "Sky (%).........95     95     95     95",
        "Weather cov.....WIDSPD WIDSPD WIDSPD WIDSPD",
        "Weather type....TSTORM TSTORM TSTORM TSTORM",
        "Temp............95     95     95     95",
        "RH..............95     95     95     95",
        "20 ft wind......SE 30  SE 30  SE 30  SE 30",
        "20 ft wind gust.",
        "Eye level wind..SE 30  SE 30  SE 30  SE 30",
        "Eye level wind..",
        "Ridgetop wind...W 5    W 5    W 5    W 5",
        "Mix hgt (ft)....100    100    100    100",
        "Transport wind..W 6    W 6    W 6    W 6",
        "Mix hgt (ft)....100    100    100    100",
        "Transport wind..W 6    W 6    W 6    W 6",
        "Dispersion......2      2      2      2",
        "Dispersion idx..2      2      2      2",
        "LVORI...........2      2      2      2",
        "CWR.............20     20     20     20",
        "Chc of pcpn (%).90     90     90     90",
        "LAL.............2      2      2      2",
        "Haines Index....3      3      3      3",

        ".SATURDAY...",

        "Sky/weather.........Sunny (0-5 percent). Chance of rain showers.",
        "Begin/end of pcpn...",
        "Temperature.........Max 79.",
        "RH..................Min 68 percent.",
        "Dewpoint............0.",
        "Wind (20 ft)........Northwest winds around 12 mph.",
        "Eye level winds.....Northwest winds around 12 mph.",
        "Wind shift..........",
        "Ridgetop wind.......Northwest around 10 mph.",
        "Surrounding ridge...Northwest around 10 mph.",
        "CWR.................30 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Northwest around 12 mph.",
        "Mixing height.......100 ft AGL.",
        "Transport winds.....Northwest around 12 mph.",
        "Dispersion index....6.",
        "LVORI...............6.",
        "Dispersion..........6.",
        "Smoke dispersal.....Good (50000 knot-ft).",
        "Stability class.....1.",
        "Marine layer........2000.",
        "Haines Index........4 or low potential for large plume dominated fire growth.",

        "Time (EST)      6 AM         10 AM        2 PM",
        "Sky (%).........0            0            0",
        "Weather cov.....CHANCE       CHANCE       CHANCE",
        "Weather type....RNSHWR       RNSHWR       RNSHWR",
        "Temp............0            0            0",
        "RH..............0            0            0",
        "20 ft wind......NW 26        NW 26        NW 26",
        "Eye level wind..NW 26        NW 26        NW 26",
        "Ridgetop wind...NW 10        NW 10        NW 10",
        "Mix hgt (ft)....100          100          100",
        "Transport wind..NW 12        NW 12        NW 12",
        "Mix hgt (ft)....100          100          100",
        "Transport wind..NW 12        NW 12        NW 12",
        "Dispersion......6            6            6",
        "Dispersion idx..6            6            6",
        "LVORI...........6            6            6",
        "CWR.............30           30           30",
        "Chc of pcpn (%).90           90           90",
        "LAL.............3            3            3",
        "Haines Index....4            4            4",

        "$$",
        "FORECASTER...Forecaster C",
        "Requested by...YYYY",
        "Type of request...Prescribed",
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
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Evening Update', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Creation Date', 'creationDate'): '1/3/10', ('Creation Time', 'creationTime'): '0600', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'yes', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 4, ('Tab Hrs', 'tonightTableRes'): 2, ('Tab Hrs', 'tomorrowTableRes'): 'None', ('TimeZone:', 'fireTZ'): 'EST5EDT'}",

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

        "Sky/weather.........Partly cloudy (40-50 percent). Widespread light freezing rain...then very light snow likely.",
        "Begin/end of pcpn...",
        "Temperature.........Min 65.",
        "RH..................Max 82 percent.",
        "Dewpoint............55.",
        "Wind (20 ft)........Hurricane force winds. South winds 104 TO 144 mph.",
        "Eye level winds.....Hurricane force winds. South winds 104 TO 144 mph.",
        "Wind shift..........",
        "Ridgetop wind.......West around 25 mph.",
        "Surrounding ridge...West around 25 mph.",
        "CWR.................60 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................1.",
        "Mixing height.......4000 ft AGL.",
        "Transport winds.....West around 23 mph.",
        "Mixing height.......4000 ft AGL.",
        "Transport winds.....West around 23 mph.",
        "Dispersion index....5.",
        "LVORI...............5.",
        "Dispersion..........5.",
        "Smoke dispersal.....Poor to excellent (4000-70000 knot-ft).",
        "Stability class.....4.",
        "Marine layer........4000.",
        "Haines Index........2 TO 3 OR or very low potential for large plume dominated fire growth.",

        "Time (EST)      6 AM   8 AM   10 AM  Noon   2 PM   4 PM   6 PM   8 PM   10 PM  Midngt 2 AM   4 AM",
        "Sky (%).........30     30     30     30     30     30     55     55     55     55     55     55",
        "Weather cov.....WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD WIDSPD LIKELY LIKELY LIKELY LIKELY LIKELY LIKELY",
        "Weather type....FZRAIN FZRAIN FZRAIN FZRAIN FZRAIN FZRAIN SNOW   SNOW   SNOW   SNOW   SNOW   SNOW",
        "Temp............30     30     30     30     30     30     55     55     55     55     55     55",
        "RH..............30     30     30     30     30     30     55     55     55     55     55     55",
        "20 ft wind......SW 37  SW 37  SW 37  SW 37  SW 37  SW 37  E 33   E 33   E 33   E 33   E 33   E 33",
        "20 ft wind gust.",
        "Eye level wind..SW 37  SW 37  SW 37  SW 37  SW 37  SW 37  E 33   E 33   E 33   E 33   E 33   E 33",
        "Eye level wind..",
        "Ridgetop wind...W 25   W 25   W 25   W 25   W 25   W 25   W 25   W 25   W 25   W 25   W 25   W 25",
        "Mix hgt (ft)....4000   4000   4000   4000   4000   4000   4000   4000   4000   4000   4000   4000",
        "Transport wind..W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23",
        "Mix hgt (ft)....4000   4000   4000   4000   4000   4000   4000   4000   4000   4000   4000   4000",
        "Transport wind..W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23   W 23",
        "Dispersion......5      5      5      5      5      5      4      4      4      4      4      4",
        "Dispersion idx..5      5      5      5      5      5      4      4      4      4      4      4",
        "LVORI...........5      5      5      5      5      5      4      4      4      4      4      4",
        "CWR.............50     50     50     50     50     50     60     60     60     60     60     60",
        "Chc of pcpn (%).90     90     90     90     90     90     70     70     70     70     70     70",
        "LAL.............5      5      5      5      5      5      6      6      6      6      6      6",
        "Haines Index....2      2      2      2      2      2      3      3      3      3      3      3",

        ".MONDAY...",

        "Sky/weather.........Partly sunny (60-70 percent). Widespread very light sleet.",
        "Begin/end of pcpn...",
        "Temperature.........Max 80.",
        "RH..................Min 73 percent.",
        "Dewpoint............65.",
        "Wind (20 ft)........STRONG WINDS. South winds around 58 mph.",
        "Eye level winds.....STRONG WINDS. South winds around 58 mph.",
        "Wind shift..........",
        "Ridgetop wind.......West around 35 mph.",
        "Surrounding ridge...West around 35 mph.",
        "CWR.................25 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................1.",
        "Mixing height.......3500 ft AGL.",
        "Transport winds.....West around 35 mph.",
        "Mixing height.......3500 ft AGL.",
        "Transport winds.....West around 35 mph.",
        "Dispersion index....3.",
        "LVORI...............3.",
        "Dispersion..........3.",
        "Smoke dispersal.....Poor (4000 knot-ft).",
        "Stability class.....1.",
        "Marine layer........5300.",
        "Haines Index........2 or very low potential for large plume dominated fire growth.",

        "$$",
        "FORECASTER...Forecaster C",
        "Requested by...YYYY",
        "Type of request...Prescribed",
        ],

    },

    {
    "name":"FWS_9",
    "productType":"FWS",
    "commentary": "Morning Issuance with Definition settings",
    "createGrids": FWS_createGrids,
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'no', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 'None', ('Tab Hrs', 'tomorrowTableRes'): 'None', ('TimeZone:', 'fireTZ'): 'EST5EDT', ('Check Items to Include:', 'extendedQuestions'): ['Include Day 3-5 Extended?', 'Include Day 6-7 Extended?', 'Include Day 8-14 Outlook?']}",
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

        "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
        "Begin/end of pcpn...",
        "Max temperature.....Around 78.",
        "Min humidity........100 percent.",
        "Dewpoint............100.",
        "Wind (20 ft)........",
        "Slope/valley.......North winds around 6 mph with gusts to around 30 mph.",
        "Eye level winds.....North winds around 6 mph.",
        "Wind shift..........",
        "Ridgetop...........Southwest around 10 mph.",
        "Surrounding ridge..Southwest around 10 mph.",
        "CWR.................0 percent.",
        "Chance of pcpn......0 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL.",
        "Mixing winds........Southwest around 12 mph.",
        "Mixing height.......100 ft AGL.",
        "Mixing winds........Southwest around 12 mph.",
        "Dispersion index....0.",
        "LVORI...............0.",
        "Dispersion..........0.",
        "Smoke dispersal.....Excellent (160000 knot-ft).",
        "Stability class.....1.",
        "Marine layer........1000.",
        "Haines Index........2 or very low potential for large plume dominated fire growth.",

        "Time (EST)      4AM 5AM 6AM 7AM 8AM 9AM 10A 11A 12P 1PM 2PM 3PM",
        "Sky (%).........M   M   CDY CDY CDY CDY CDY CDY CDY CDY CDY CDY",
        "Weather cov.....PTY PTY PTY PTY PTY PTY PTY PTY PTY PTY PTY PTY",
        "Weather type....FOG FOG FOG FOG FOG FOG FOG FOG FOG FOG FOG FOG",
        "Temp............M   M   100 100 100 100 100 100 100 100 100 100",
        "RH..............M   M   100 100 100 100 100 100 100 100 100 100",
        "20 ft wind dir..N   N   N   N   N   N   N   N   N   N   N   N",
        "20 ft wind spd..6   6   6   6   6   6   6   6   6   6   6   6",
        "20 ft wind gust.30  30  30  30  30  30  30  30  30  30  30  30",
        "Eye lvl wnd dir.230 230 230 230 230 230 230 230 230 230 230 230",
        "Eye lvl wnd spd.7   7   7   7   7   7   7   7   7   7   7   7",
        "Eye lvl wnd gst.30  30  30  30  30  30  30  30  30  30  30  30",
        "Ridge wnd dir..230 230 230 230 230 230 230 230 230 230 230 230",
        "Ridge wnd spd..10  10  10  10  10  10  10  10  10  10  10  10",
        "Mix hgt (kft)...0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "Mix hgt (km)....0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "Mixng wind dir..230 230 230 230 230 230 230 230 230 230 230 230",
        "Mixng wind spd..12  12  12  12  12  12  12  12  12  12  12  12",
        "Mix wnd spd m/s.5   5   5   5   5   5   5   5   5   5   5   5",
        "Mix hgt (kft)...0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "Mix hgt (km)....0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "Mixng wind dir..230 230 230 230 230 230 230 230 230 230 230 230",
        "Mixng wind spd..12  12  12  12  12  12  12  12  12  12  12  12",
        "Mix wnd spd m/s.5   5   5   5   5   5   5   5   5   5   5   5",
        "Dispersion......0   0   0   0   0   0   0   0   0   0   0   0",
        "Dispersion idx..0   0   0   0   0   0   0   0   0   0   0   0",
        "LVORI...........0   0   0   0   0   0   0   0   0   0   0   0",
        "CWR.............0   0   0   0   0   0   0   0   0   0   0   0",
        "Chc of pcpn (%).0   0   0   0   0   0   0   0   0   0   0   0",
        "LAL.............1   1   1   1   1   1   1   1   1   1   1   1",
        "Haines Index....2   2   2   2   2   2   2   2   2   2   2   2",

        ".TONIGHT...",

        "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
        "Begin/end of pcpn...",
        "Min temperature.....Around 60.",
        "Max humidity........95 percent.",
        "Dewpoint............95.",
        "Wind (20 ft)........",
        "Slope/valley.......Very windy. Northeast winds around 46 mph.",
        "Eye level winds.....Very windy. Northeast winds around 46 mph.",
        "Wind shift..........",
        "Ridgetop...........West around 5 mph.",
        "Surrounding ridge..West around 5 mph.",
        "CWR.................20 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................2.",
        "Mixing height.......100 ft AGL.",
        "Mixing winds........West around 6 mph.",
        "Mixing height.......100 ft AGL.",
        "Mixing winds........West around 6 mph.",
        "Dispersion index....2.",
        "LVORI...............2.",
        "Dispersion..........2.",
        "Smoke dispersal.....Excellent (100000 knot-ft).",
        "Stability class.....2.",
        "Marine layer........1000.",
        "Haines Index........3 or very low potential for large plume dominated fire growth.",


        ".SATURDAY...",

        "Sky/weather.........Sunny (0-5 percent). Chance of rain showers.",
        "Begin/end of pcpn...",
        "Max temperature.....around 79.",
        "Min humidity........0 percent.",
        "Dewpoint............0.",
        "Wind (20 ft)........",
        "Slope/valley.......Northwest winds around 12 mph.",
        "Eye level winds.....Northwest winds around 12 mph.",
        "Wind shift..........",
        "Ridgetop...........Northwest around 10 mph.",
        "Surrounding ridge..Northwest around 10 mph.",
        "CWR.................30 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL.",
        "Mixing winds........Northwest around 12 mph.",
        "Mixing height.......100 ft AGL.",
        "Mixing winds........Northwest around 12 mph.",
        "Dispersion index....6.",
        "LVORI...............6.",
        "Dispersion..........6.",
        "Smoke dispersal.....Good (50000 knot-ft).",
        "Stability class.....1.",
        "Marine layer........2000.",
        "Haines Index........4 or low potential for large plume dominated fire growth.",

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
        "Requested by...YYYY",
        "Type of request...Prescribed",

        ],

    },
    {
    "name":"FWS_10",
    "productType":"FWS",
    "commentary": "Morning Issuance with Definition settings",
    "createGrids": FWS_createGrids,
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['Sky/weather', 'Begin/end of pcpn', 'TEMPERATURE', 'HUMIDITY', 'DEWPOINT', '20 FOOT WINDS', 'Eye level winds', 'WIND SHIFT', 'RIDGE TOP WIND', 'SURROUNDING RIDGE', 'CWR', 'POP', 'LIGHTNING ACTIVITY LEVEL', 'SMOKE DISPERSION', 'Mixing height', 'Transport winds', 'LDSI', 'LVORI', 'DISPERSION INDEX', 'CLEARING INDEX', 'STABILITY CLASS', 'Marine layer', 'Haines Index'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular/Narrative', ('Include Ignition Times?', 'withIgnitionTimes'): 'no', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '1300', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 'None', ('Tab Hrs', 'tomorrowTableRes'): 'None', ('TimeZone:', 'fireTZ'): 'EST5EDT'}",
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

        "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
        "Begin/end of pcpn...",
        "Max temperature.....Around 78.",
        "Min humidity........100 percent.",
        "Dewpoint............100.",
        "Wind (20 ft)........",
        "Slope/valley.......North winds around 6 mph with gusts to around 30 mph.",
        "Eye level winds.....North winds around 6 mph.",
        "Wind shift..........",
        "Ridgetop...........Southwest around 10 mph.",
        "Surrounding ridge..Southwest around 10 mph.",
        "CWR.................0 percent.",
        "Chance of pcpn......0 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL.",
        "Mixing winds........Southwest around 12 mph.",
        "Mixing height.......100 ft AGL.",
        "Mixing winds........Southwest around 12 mph.",
        "Dispersion index....0.",
        "LVORI...............0.",
        "Dispersion..........0.",
        "Smoke dispersal.....Excellent (160000 knot-ft).",
        "Stability class.....1.",
        "Marine layer........1000.",
        "Haines Index........2 or very low potential for large plume dominated fire growth.",

        "Time (EST)      6AM 7AM 8AM 9AM 10A 11A 12P 1PM 2PM 3PM 4PM 5PM",
        "Sky (%).........CDY CDY CDY CDY CDY CDY CDY CDY CDY CDY CDY CDY",
        "Weather cov.....PTY PTY PTY PTY PTY PTY PTY PTY PTY PTY PTY PTY",
        "Weather type....FOG FOG FOG FOG FOG FOG FOG FOG FOG FOG FOG FOG",
        "Temp............100 100 100 100 100 100 100 100 100 100 100 100",
        "RH..............100 100 100 100 100 100 100 100 100 100 100 100",
        "20 ft wind dir..N   N   N   N   N   N   N   N   N   N   N   N",
        "20 ft wind spd..6   6   6   6   6   6   6   6   6   6   6   6",
        "20 ft wind gust.30  30  30  30  30  30  30  30  30  30  30  30",
        "Eye lvl wnd dir.230 230 230 230 230 230 230 230 230 230 230 230",
        "Eye lvl wnd spd.7   7   7   7   7   7   7   7   7   7   7   7",
        "Eye lvl wnd gst.30  30  30  30  30  30  30  30  30  30  30  30",
        "Ridge wnd dir..230 230 230 230 230 230 230 230 230 230 230 230",
        "Ridge wnd spd..10  10  10  10  10  10  10  10  10  10  10  10",
        "Mix hgt (kft)...0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "Mix hgt (km)....0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "Mixng wind dir..230 230 230 230 230 230 230 230 230 230 230 230",
        "Mixng wind spd..12  12  12  12  12  12  12  12  12  12  12  12",
        "Mix wnd spd m/s.5   5   5   5   5   5   5   5   5   5   5   5",
        "Mix hgt (kft)...0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "Mix hgt (km)....0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1",
        "Mixng wind dir..230 230 230 230 230 230 230 230 230 230 230 230",
        "Mixng wind spd..12  12  12  12  12  12  12  12  12  12  12  12",
        "Mix wnd spd m/s.5   5   5   5   5   5   5   5   5   5   5   5",
        "Dispersion......0   0   0   0   0   0   0   0   0   0   0   0",
        "Dispersion idx..0   0   0   0   0   0   0   0   0   0   0   0",
        "LVORI...........0   0   0   0   0   0   0   0   0   0   0   0",
        "CWR.............0   0   0   0   0   0   0   0   0   0   0   0",
        "Chc of pcpn (%).0   0   0   0   0   0   0   0   0   0   0   0",
        "LAL.............1   1   1   1   1   1   1   1   1   1   1   1",
        "Haines Index....2   2   2   2   2   2   2   2   2   2   2   2",

        ".TONIGHT...",

        "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
        "Begin/end of pcpn...",
        "Min temperature.....Around 60.",
        "Max humidity........95 percent.",
        "Dewpoint............95.",
        "Wind (20 ft)........",
        " Slope/valley.......Very windy. Northeast winds around 46 mph.",
        "Eye level winds.....Very windy. Northeast winds around 46 mph.",
        "Wind shift..........",
        "Ridgetop...........West around 5 mph.",
        "Surrounding ridge..West around 5 mph.",
        "CWR.................20 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................2.",
        "Mixing height.......100 ft AGL.",
        "Mixing winds........West around 6 mph.",
        "Mixing height.......100 ft AGL.",
        "Mixing winds........West around 6 mph.",
        "Dispersion index....2.",
        "LVORI...............2.",
        "Dispersion..........2.",
        "Smoke dispersal.....Excellent (100000 knot-ft).",
        "Stability class.....2.",
        "Marine layer........1000.",
        "Haines Index........3 or very low potential for large plume dominated fire growth.",


        ".SATURDAY...",

        "Sky/weather.........Sunny (0-5 percent). Chance of rain showers.",
        "Begin/end of pcpn...",
        "Max temperature.....Around 79.",
        "Min humidity........0 percent.",
        "Dewpoint............0.",
        "Wind (20 ft)........",
        "Slope/valley.......Northwest winds around 12 mph.",
        "Eye level winds.....Northwest winds around 12 mph.",
        "Wind shift..........",
        "Ridgetop...........Northwest around 10 mph.",
        "Surrounding ridge..Northwest around 10 mph.",
        "CWR.................30 percent.",
        "Chance of pcpn......90 percent.",
        "LAL.................1.",
        "Mixing height.......100 ft AGL.",
        "Mixing winds........Northwest around 12 mph.",
        "Mixing height.......100 ft AGL.",
        "Mixing winds........Northwest around 12 mph.",
        "Dispersion index....6.",
        "LVORI...............6.",
        "Dispersion..........6.",
        "Smoke dispersal.....Good (50000 knot-ft).",
        "Stability class.....1.",
        "Marine layer........2000.",
        "Haines Index........4 or low potential for large plume dominated fire growth.",

        "$$",
        "FORECASTER...Forecaster C",
        "Requested by...YYYY",
        "Type of request...Prescribed",
       ],

    },

    {
    "name":"FWS_Hourly_Sky_Table",
    "productType":"FWS",
    "commentary": "Test Hourly Sky Table",
    "createGrids": Sky_createGrids,
    "cmdLineVars": "{('Product Issuance:', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Agency:', 'requestingAgency'): 'Agency 1', ('Tomorrow Elements', 'tomorrowElements'): ['Sky/weather'], ('Fire Longitude (Deg)...................', 'fireLongitude'): 82.19, ('WebSiteTag:', 'webSiteTag'): '', ('Check Items to Include:', 'extendedQuestions'): [], ('Forecaster:', 'forecaster'): ['FORECASTER C'], ('Type of Fire:', 'fireType'): 'PRESCRIBED', ('Name of Agency Contact..........', 'agencyContact'): 'yyyy', ('Today Elements', 'todayElements'): ['Sky/weather'], ('Fire Latitude (Deg).......................', 'fireLatitude'): 28.27, ('WFOid:', 'wfoID'): '', ('Fire Size (Acres) .........................', 'fireSize'): .005, ('Name of Fire ...................................', 'fireName'): 'xxxx', ('Tonight Elements', 'tonightElements'): ['Sky/weather'], ('Creation Date', 'creationDate'): '', ('Creation Time', 'creationTime'): '', ('What Type of Forecast?', 'forecastType'): 'Tabular Only', ('Include Ignition Times?', 'withIgnitionTimes'): 'no', ('Name of Agency if not listed....', 'otherAgencyName'): 'xxxx', ('Date of Fire .....................................', 'fireDate'): '1/1/10', ('Time of Fire .....................................', 'fireTime'): '0600', ('Tab Hrs', 'todayTableRes'): 1, ('Tab Hrs', 'tonightTableRes'): 1, ('Tab Hrs', 'tomorrowTableRes'): 1, ('TimeZone:', 'fireTZ'): 'EST5EDT'}",
    "fileChanges":[
       ("FWS_<site>_Definition", "TextUtility", "add", definition3, "undo"),
       ],

    "checkStrings": [
        ".TODAY...",
        "Time (EST)      6AM 7AM 8AM 9AM 10A 11A 12P 1PM 2PM 3PM 4PM 5PM",
        "Sky (%).........MCR MCR MCR MCR MCR MCR MCR MCR MCR MCR MCR MCR",
        ".TONIGHT...",
        "Time (EST)      6PM 7PM 8PM 9PM 10P 11P Mid 1AM 2AM 3AM 4AM 5AM",
        "Sky (%).........PC  PC  PC  PC  PC  PC  PC  PC  PC  PC  PC  PC",
        ".SATURDAY...",
        "Time (EST)      6AM 7AM 8AM 9AM 10A 11A 12P 1PM 2PM 3PM 4PM 5PM",
        "Sky (%).........MC  MC  MC  MC  MC  MC  MC  MC  MC  MC  MC  MC",
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
#             "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
#             "CWR.................0 percent.",
#             "Max temperature.....Around 78.",
#             "Eye level winds.....North winds around 6 mph.",
#             "Mixing height.......100 ft AGL.",
#             "Transport winds.....Southwest around 12 mph.",
#             "Min humidity........65 percent.",
#             "Wind (20 ft)........North winds around 6 mph with gusts to around 30 mph",
# 
#             ".SATURDAY...",
#             "Sky/weather.........Sunny (0-5 percent). Chance of showers.",
#             "CWR.................30 percent.",
#             "Max temperature.....Around 79. ",
#             "Min humidity........68 percent. ",
#             "Wind (20 ft)........Northwest winds around 12 mph.",
#             "Eye level winds.....Northwest winds around 12 mph.",
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
#         "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "Max temperature.....Around 78.",
#         "Min humidity........65 percent.",
#         "Eye level winds.....North winds around 6 mph.",
#         "Mixing height.......100 ft AGL.",
#         "Transport winds.....Southwest around 12 mph.",
# 
#         ".SATURDAY...",
#         "Sky/weather.........Sunny (0-5 percent). Chance of showers.",
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
#         "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "Max temperature.....Around 78.",
#         "Min humidity........65 percent.",
#         "Eye level winds.....North winds around 6 mph.",
#         "Mixing height.......100 ft AGL.",
#         "Transport winds.....Southwest around 12 mph.",
# 
#         ".TONIGHT...",
#         "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "Min temperature.....Around 60.",
#         "Max humidity........78 percent.",
# 
#         ".SATURDAY...",
#         "Sky/weather.........Sunny (0-5 percent). Chance of showers.",
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
#         "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "Max temperature.....Around 78.",
#         "Min humidity........65 percent.",
# 
#         ".TONIGHT...",
#         "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "Min temperature.....Around 60.",
#         "Max humidity........78 percent.",
# 
#         ".SATURDAY...",
#         "Sky/weather.........Sunny (0-5 percent). Chance of showers.",
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
#         "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "Max temperature.....Around 78.",
#         "Min humidity........65 percent.",
#         "Eye level winds.....North winds around 6 mph.",
#         "Mixing height.......100 ft AGL.",
#         "Transport winds.....Southwest around 12 mph.",
#         "Smoke dispersal.....Excellent (160000 knot-ft).",
#         "Stability class.....1.",
#         "Marine layer........1000.",
#         "Haines Index........2 or very low potential for large plume dominated fire growth.",
# 
#         ".TONIGHT...",
#         "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "Min temperature.....Around 60.",
#         "Max humidity........78 percent.",
#         "Smoke dispersal.....Excellent (100000 knot-ft).",
#         "Stability class.....2.",
#         "Marine layer........1000.",
#         "Haines Index........3 or very low potential for large plume dominated fire growth.",
# 
#         ".SATURDAY...",
#         "Sky/weather.........Sunny (0-5 percent). Chance of showers.",
#         "CWR.................30 percent.",
#         "Smoke dispersal.....Good (50000 knot-ft).",
#         "Stability class.....1.",
#         "Marine layer........2000.",
#         "Haines Index........4 or low potential for large plume dominated fire growth",
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
#         "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "Max temperature.....Around 78.",
#         "Min humidity........65 percent.",
#         "Eye level winds.....North winds around 6 mph.",
#         "Mixing height.......100 ft AGL.",
#         "Transport winds.....Southwest around 12 mph.",
#         "Smoke dispersal.....Excellent (160000 knot-ft).",
#         "Stability class.....1.",
#         "Marine layer........1000.",
#         "Haines Index........2 or very low potential for large plume dominated fire growth.",
# 
#         ".TONIGHT...",
#         "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "Min temperature.....Around 60.",
#         "Max humidity........78 percent.",
#         "Smoke dispersal.....Excellent (100000 knot-ft).",
#         "Stability class.....2.",
#         "Marine layer........1000.",
#         "Haines Index........3 or very low potential for large plume dominated fire growth.",
# 
#         ".SATURDAY...",
#         "Sky/weather.........Sunny (0-5 percent). Chance of showers.",
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
#         "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "Max temperature.....Around 78.",
#         "Min humidity........65 percent.",
#         "Eye level winds.....North winds around 6 mph.",
#         "Mixing height.......100 ft AGL.",
#         "Transport winds.....Southwest around 12 mph.",
#         "Smoke dispersal.....Excellent (160000 knot-ft).",
#         "Stability class.....1.",
#         "Marine layer........1000.",
#         "Haines Index........2 or very low potential for large plume dominated fire growth.",
# 
#         ".TONIGHT...",
#         "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "Min temperature.....Around 60.",
#         "Max humidity........78 percent.",
# 
#         ".SATURDAY...",
#         "Sky/weather.........Sunny (0-5 percent). Chance of showers.",
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
#         "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "Max temperature.....Around 78.",
#         "Min humidity........65 percent.",
#         "Eye level winds.....North winds around 6 mph.",
#         "Mixing height.......100 ft AGL.",
#         "Transport winds.....Southwest around 12 mph.",
#         "Smoke dispersal.....Excellent (160000 knot-ft).",
#         "Stability class.....1.",
#         "Marine layer........1000.",
#         "Haines Index........2 or very low potential for large plume dominated fire growth.",
# 
#         ".TONIGHT...",
#         "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "Min temperature.....Around 60.",
#         "Max humidity........78 percent.",
# 
#         ".SATURDAY...",
#         "Sky/weather.........Sunny (0-5 percent). Chance of showers.",
#         "CWR.................30 percent.",
#         "Smoke dispersal.....Good (50000 knot-ft).",
#         "Stability class.....1.",
#         "Marine layer........2000.",
#         "Haines Index........4 or low potential for large plume dominated fire growth.",
#         "Requested by...Virgil Middendorf",
#         "Type of request...Prescribed",
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
#         "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "Max temperature.....Around 78.",
#         "Min humidity........65 percent.",
#         "Eye level winds.....North winds around 6 mph.",
#         "Mixing height.......100 ft AGL.",
#         "Transport winds.....Southwest around 12 mph.",
# 
#         ".TONIGHT...",
#         "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "Min temperature.....Around 60.",
#         "Max humidity........78 percent.",
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
#         "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "Min temperature.....Around 60.",
#         "Max humidity........78 percent.",
# 
#         ".SATURDAY...",
#         "Sky/weather.........Sunny (0-5 percent). Chance of showers.",
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
#         "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
#         "CWR.................0 percent",
#         "Max temperature.....Around 78.",
#         "Eye level winds.....North winds around 6 mph.",
#         "Mixing height.......100 ft AGL.",
#         "Transport winds.....Southwest around 12 mph.",
#         "Min humidity........65 percent.",
#         "Wind (20 ft)........North winds around 6 mph with gusts to around 30 mph.",
# 
#         ".SATURDAY...",
#         "Sky/weather.........Sunny (0-5 percent). Chance of showers.",
#         "CWR.................30 percent.",
#         "Max temperature.....Around 79.",
#         "Min humidity........68 percent.",
#         "Wind (20 ft)........Northwest winds around 12 mph.",
#         "Eye level winds.....Northwest winds around 12 mph." ,
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
#         "Sky/weather.........Sunny (0-5 percent). Chance of showers.",
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
#         "Sky/weather.........Cloudy (90-100 percent). Widespread thunderstorms.",
#         "CWR.................20 percent.",
#         "Min temperature.....Around 60.",
#         "Max humidity........78 percent.",
# 
#         ".SATURDAY...",
#         "Sky/weather.........Sunny (0-5 percent). Chance of showers.",
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
#         "Min temperature.....Around 40.",
#         "Max humidity........60 percent.",
# 
#         ".FRIDAY...",
#         "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog.",
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
#             "Sky/weather.........",
#             "CWR.................",
#             "Max temperature.....",
#             "Eye level winds.....",
#             "Mixing height.......",
#             "Transport winds.....",
#             "Min humidity........",
#             "Wind (20 ft)........",
#             ".SATURDAY...",
#             "Sky/weather.........",
#             "CWR.................",
#             "Max temperature.....",
#             "Min humidity........",
#             "Wind (20 ft)........",
#             "Eye level winds.....",
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

