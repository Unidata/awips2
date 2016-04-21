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
# ZFP tests
#
# Author: cheatwood
# ----------------------------------------------------------------------------

#### File Changes
fc1_1 = """#Definition["useStormTotalSnow"] = 1"""
fc1_2 = """Definition["useStormTotalSnow"] = 1"""
fc1_3 = """#Definition["includeMultipleElementTable"] = 1"""
fc1_4 = """Definition["includeMultipleElementTable"] = 1"""
fc1_5 = """#Definition["singleValueFormat"] = 1"""
fc1_6 = """Definition["singleValueFormat"] = 1"""
fc1_7 = """#Definition["elementList"] = ["Temp", "PoP"]"""
fc1_8 = """Definition["elementList"] = ["Temp", "PoP", "Humidity"]"""

#### Create Grids
# 3 hour grids

cg_3_6 = [
       ("Fcst", "T", "SCALAR", 3, 6, 66, "all"),
       ("Fcst", "Td", "SCALAR", 3, 6, 35, "all"),
       ("Fcst", "Wind", "VECTOR", 3, 6, (15, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 3, 6, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 3, 6, "Sct:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 3, 6, 50, "all"),
       ("Fcst", "QPF", "SCALAR", 3, 6, .03, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 3, 6, .08, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 3, 6, 0, "all"),
       ("Fcst", "RH", "SCALAR", 3, 6, 60, "all"),
       ("Fcst", "WindChill", "SCALAR", 3, 6, 20, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 3, 6, 106, "all"),
       ("Fcst", "WindGust", "SCALAR", 3, 6, 25, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 3, 6, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 3, 6, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 3, 6, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 100, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 0, "all"),
       ] 


cg_6_9 = [       
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "Td", "SCALAR", 6, 9, 30, "all"),  
       ("Fcst", "Wind", "VECTOR", 6, 9, (10, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 6, 9, 0, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 9, "Iso:T:<NoInten>:<NoVis>:", "all"),       
       ("Fcst", "PoP", "SCALAR", 6, 9, 0, "all"),
       ("Fcst", "QPF", "SCALAR", 6, 9, .01, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 6, 9, .05, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 6, 9, 1, "all"),
       ("Fcst", "RH", "SCALAR", 6, 9, 60, "all"),
       ("Fcst", "WindChill", "SCALAR", 6, 9, 10, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 6, 9, 106, "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 9, 25, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 6, 9, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 6, 9, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 6, 9, 0, "all"),
       ]

cg_9_12 = [
       ("Fcst", "T", "SCALAR", 9, 12, 65, "all"),
       ("Fcst", "Td", "SCALAR", 9, 12, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 9, 12, (15, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 50, "all"),       
       ("Fcst", "Wx", "WEATHER", 9, 12,"Num:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 9, 12, 0, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 9, 12, .01, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 9, 12, 0, "all"),
       ("Fcst", "RH", "SCALAR", 9, 12, 60, "all"),
       ("Fcst", "WindChill", "SCALAR", 9, 12, 30, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 9, 12, 95, "all"),
       ("Fcst", "WindGust", "SCALAR", 9, 12, 25, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 9, 12, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 9, 12, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 9, 12, 0, "all"),
       ]

cg_12_15 = [
       ("Fcst", "T", "SCALAR", 12, 15, 60, "all"),
       ("Fcst", "Td", "SCALAR", 12, 15, 45, "all"),  
       ("Fcst", "Wind", "VECTOR", 12, 15, (15, "SE"), "all"),
       ("Fcst", "Sky", "SCALAR", 12, 15, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 12, 15,"Wide:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 12, 15, 100, "all"),
       ("Fcst", "QPF", "SCALAR", 12, 15, .05, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 12, 15, .1, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 12, 15, 2, "all"),
       ("Fcst", "RH", "SCALAR", 12, 15, 60, "all"),
       ("Fcst", "WindChill", "SCALAR", 12, 15, 20, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 12, 15, 106, "all"),
       ("Fcst", "WindGust", "SCALAR", 12, 15, 25, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 12, 15, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 12, 15, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 12, 15, 0, "all"),
       ]

cg_15_18 = [
       ("Fcst", "T", "SCALAR", 15, 18, 55, "all"),
       ("Fcst", "Td", "SCALAR", 15, 18, 50, "all"),       
       ("Fcst", "Wind", "VECTOR", 15, 18, (25, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 15, 18, 100, "all"),
       ("Fcst", "Wx", "WEATHER", 15, 18,"SChc:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 15, 18, 90, "all"),
       ("Fcst", "QPF", "SCALAR", 15, 18, .05, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 15, 18, .1, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 15, 18, 1, "all"),
       ("Fcst", "RH", "SCALAR", 15, 18, 95, "all"),
       ("Fcst", "WindChill", "SCALAR", 15, 18, 0, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 15, 18, 80, "all"),
       ("Fcst", "WindGust", "SCALAR", 15, 18, 35, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 15, 18, 1, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 15, 18, 300, "all"),
       ("Fcst", "FzLevel", "SCALAR", 15, 18, 2000, "all"),
       ]

cg_18_21 = [
       ("Fcst", "T", "SCALAR", 18, 21, 50, "all"),
       ("Fcst", "Td", "SCALAR", 18, 21, 50, "all"),  
       ("Fcst", "Wind", "VECTOR", 18, 21, (30, "NE"), "all"),
       ("Fcst", "Sky", "SCALAR", 18, 21, 15, "all"),
       ("Fcst", "Wx", "WEATHER", 18, 21,"Lkly:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 18, 21, 25, "all"),
       ("Fcst", "QPF", "SCALAR", 18, 21, .08, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 18, 21, .2, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 18, 21, 1.5, "all"),
       ("Fcst", "RH", "SCALAR", 18, 21, 90, "all"),
       ("Fcst", "WindChill", "SCALAR", 18, 21, 0, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 18, 21, 60, "all"),
       ("Fcst", "WindGust", "SCALAR", 18, 21, 35, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 18, 21, 1, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 18, 21, 300, "all"),
       ("Fcst", "FzLevel", "SCALAR", 18, 21, 2000, "all"),
       ]

cg_21_24 = [
       ("Fcst", "T", "SCALAR", 21, 24, 50, "all"),
       ("Fcst", "Td", "SCALAR", 21, 24, 48, "all"),  
       ("Fcst", "Wind", "VECTOR", 21, 24, (25, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 21, 24, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 21, 24,"Brf:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 21, 24, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 21, 24, .1, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 21, 24, .25, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 21, 24, 1.2, "all"),
       ("Fcst", "RH", "SCALAR", 21, 24, 90, "all"),
       ("Fcst", "WindChill", "SCALAR", 21, 24, 0, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 21, 24, 60, "all"),
       ("Fcst", "WindGust", "SCALAR", 21, 24, 35, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 21, 24, 1, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 21, 24, 300, "all"),
       ("Fcst", "FzLevel", "SCALAR", 21, 24, 2000, "all"),
       ]

cg_24_27 = [
       ("Fcst", "T", "SCALAR", 24, 27, 48, "all"),
       ("Fcst", "Td", "SCALAR", 24, 27, 48, "all"),  
       ("Fcst", "Wind", "VECTOR", 24, 27, (22, "NW"), "all"),
       ("Fcst", "Sky", "SCALAR", 24, 27, 19, "all"),
       ("Fcst", "Wx", "WEATHER", 24, 27,"Inter:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 24, 27, 25, "all"),
       ("Fcst", "QPF", "SCALAR", 24, 27, 1, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 24, 27, 1, "all"),
       ("Fcst", "RH", "SCALAR", 24, 27, 90, "all"),
       ("Fcst", "WindChill", "SCALAR", 24, 27, 0, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 24, 27, 60, "all"),
       ("Fcst", "WindGust", "SCALAR", 24, 27, 35, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 24, 27, 1, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 24, 27, 300, "all"),
       ("Fcst", "FzLevel", "SCALAR", 24, 27, 2000, "all"),
       ]

cg_27_30 = [
       ("Fcst", "T", "SCALAR", 27, 30, 43, "all"),
       ("Fcst", "Td", "SCALAR", 27, 30, 43, "all"),  
       ("Fcst", "Wind", "VECTOR", 27, 30, (15, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 27, 30, 30, "all"),
       ("Fcst", "Wx", "WEATHER", 27, 30, "Wide:R:-:3SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 27, 30, 50, "all"),
       ("Fcst", "QPF", "SCALAR", 27, 30, 3, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 27, 30, 0, "all"),
       ("Fcst", "RH", "SCALAR", 27, 30, 100, "all"),
       ("Fcst", "WindChill", "SCALAR", 27, 30, 1, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 27, 30, 100, "all"),
       ("Fcst", "WindGust", "SCALAR", 27, 30, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 27, 30, 0.5, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 27, 30, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 27, 30, 500, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 24", "MaxTEnd + 24", 50, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 24", "MinTEnd + 24", 33, "all"),
       ]

cg_30_33 = [
       ("Fcst", "T", "SCALAR", 30, 33, 40, "all"),
       ("Fcst", "Td", "SCALAR", 30, 33, 39, "all"),  
       ("Fcst", "Wind", "VECTOR", 30, 33, (10, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 30, 33, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 30, 33, "SChc:R:--:0SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 30, 33, 60, "all"),
       ("Fcst", "QPF", "SCALAR", 30, 33, 2.5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 30, 33, 0, "all"),
       ("Fcst", "RH", "SCALAR", 30, 33, 100, "all"),
       ("Fcst", "WindChill", "SCALAR", 30, 33, 1, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 30, 33, 100, "all"),
       ("Fcst", "WindGust", "SCALAR", 30, 33, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 30, 33, 0.5, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 30, 33, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 30, 33, 500, "all"),
       ]

cg_33_36 = [
       ("Fcst", "T", "SCALAR", 33, 36, 41, "all"),      
       ("Fcst", "Td", "SCALAR", 33, 36, 39, "all"),  
       ("Fcst", "Wind", "VECTOR", 33, 36, (10, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 33, 36, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 33, 36, "Lkly:R:m:1/2SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 33, 36, 70, "all"),
       ("Fcst", "QPF", "SCALAR", 33, 36, 4, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 33, 36, 0, "all"),
       ("Fcst", "RH", "SCALAR", 33, 36, 100, "all"),
       ("Fcst", "WindChill", "SCALAR", 33, 36, 1, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 33, 36, 100, "all"),
       ("Fcst", "WindGust", "SCALAR", 33, 36, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 33, 36, 0.5, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 33, 36, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 33, 36, 500, "all"),
       ]

cg_36_39 = [
       ("Fcst", "T", "SCALAR", 36, 39, 40, "all"),
       ("Fcst", "Td", "SCALAR", 36, 39, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 36, 39, (5, "SE"), "all"),
       ("Fcst", "Sky", "SCALAR", 36, 39, 100, "all"),
       ("Fcst", "Wx", "WEATHER", 36, 39, "Brf:R:+:5SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 36, 39, 100, "all"),
       ("Fcst", "QPF", "SCALAR", 36, 39, 5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 36, 39, 0, "all"),
       ("Fcst", "RH", "SCALAR", 36, 39, 100, "all"),
       ("Fcst", "WindChill", "SCALAR", 36, 39, 1, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 36, 39, 100, "all"),
       ("Fcst", "WindGust", "SCALAR", 36, 39, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 36, 39, 0.5, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 36, 39, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 36, 39, 500, "all"),
       ]

cg_39_42 = [
       ("Fcst", "T", "SCALAR", 39, 42, 33, "all"),
       ("Fcst", "Td", "SCALAR", 39, 42, 33, "all"),  
       ("Fcst", "Wind", "VECTOR", 39, 42, (5, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 39, 42, 100, "all"),
       ("Fcst", "Wx", "WEATHER", 39, 42, "Inter:R:m:2SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 39, 42, 100, "all"),
       ("Fcst", "QPF", "SCALAR", 39, 42, 5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 39, 42, 0, "all"),
       ("Fcst", "RH", "SCALAR", 39, 42, 85, "all"),
       ("Fcst", "WindChill", "SCALAR", 39, 42, 5, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 39, 42, 95, "all"),
       ("Fcst", "WindGust", "SCALAR", 39, 42, 40, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 39, 42, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 39, 42, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 39, 42, 0, "all"),
       ]

cg_42_45 = [
       ("Fcst", "T", "SCALAR", 42, 45, 35, "all"),
       ("Fcst", "Td", "SCALAR", 42, 45, 33, "all"),  
       ("Fcst", "Wind", "VECTOR", 42, 45, (2.5, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 42, 45, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 42, 45, "Iso:RW:+:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 42, 45, 95, "all"),
       ("Fcst", "QPF", "SCALAR", 42, 45, 5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 42, 45, 0, "all"),
       ("Fcst", "RH", "SCALAR", 42, 45, 85, "all"),
       ("Fcst", "WindChill", "SCALAR", 42, 45, 5, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 42, 45, 95, "all"),
       ("Fcst", "WindGust", "SCALAR", 42, 45, 40, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 42, 45, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 42, 45, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 42, 45, 0, "all"),
       ]

cg_45_48 = [
       ("Fcst", "T", "SCALAR", 45, 48, 40, "all"),
       ("Fcst", "Td", "SCALAR", 45, 48, 35, "all"),  
       ("Fcst", "Wind", "VECTOR", 45, 48, (2, "NE"), "all"),
       ("Fcst", "Sky", "SCALAR", 45, 48, 85, "all"),
       ("Fcst", "Wx", "WEATHER", 45, 48, "Wide:RW:-:1/2SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 45, 48, 65, "all"),
       ("Fcst", "QPF", "SCALAR", 45, 48, 1, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 45, 48, 5, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 45, 48, 0, "all"),
       ("Fcst", "RH", "SCALAR", 45, 48, 85, "all"),
       ("Fcst", "WindChill", "SCALAR", 45, 48, 5, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 45, 48, 95, "all"),
       ("Fcst", "WindGust", "SCALAR", 45, 48, 40, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 45, 48, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 45, 48, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 45, 48, 0, "all"),
       ]

cg_48_51 = [
       ("Fcst", "T", "SCALAR", 48, 51, 45, "all"),
       ("Fcst", "Td", "SCALAR", 48, 51, 38, "all"),  
       ("Fcst", "Wind", "VECTOR", 48, 51, (5, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 48, 51, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 48, 51, "SChc:RW:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 48, 51, 60, "all"),
       ("Fcst", "QPF", "SCALAR", 48, 51, .5, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 48, 51, 3, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 48, 51, 0, "all"),
       ("Fcst", "RH", "SCALAR", 48, 51, 85, "all"),
       ("Fcst", "WindChill", "SCALAR", 48, 51, 5, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 48, 51, 95, "all"),
       ("Fcst", "WindGust", "SCALAR", 48, 51, 40, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 48, 51, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 48, 51, 0, "all"),      
       ("Fcst", "FzLevel", "SCALAR", 48, 51, 0, "all"),
       ]

cg_51_54 = [
       ("Fcst", "T", "SCALAR", 51, 54, 50, "all"),
       ("Fcst", "Td", "SCALAR", 51, 54, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 51, 54, (7, "NW"), "all"),
       ("Fcst", "Sky", "SCALAR", 51, 54, 35, "all"),
       ("Fcst", "Wx", "WEATHER", 51, 54, "Lkly:RW:--:1SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 51, 54, 50, "all"),
       ("Fcst", "QPF", "SCALAR", 51, 54, .5, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 51, 54, 2, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 51, 54, 0, "all"),
       ("Fcst", "RH", "SCALAR", 51, 54, 80, "all"),
       ("Fcst", "WindChill", "SCALAR", 51, 54, -1, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 51, 54, 80, "all"),
       ("Fcst", "WindGust", "SCALAR", 51, 54, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 51, 54, 0.1, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 51, 54, 500, "all"),
       ("Fcst", "FzLevel", "SCALAR", 51, 54, 800, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 48", "MaxTEnd + 48", 70, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 48", "MinTEnd + 48", 50, "all"),
       ]

cg_54_57 = [
       ("Fcst", "T", "SCALAR", 54, 57, 50, "all"),
       ("Fcst", "Td", "SCALAR", 54, 57, 45, "all"),  
       ("Fcst", "Wind", "VECTOR", 54, 57, (10, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 54, 57, 30, "all"),
       ("Fcst", "Wx", "WEATHER", 54, 57, "Def:RW:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 54, 57, 55, "all"),
       ("Fcst", "QPF", "SCALAR", 54, 57, .25, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 54, 57, 1, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 54, 57, 0, "all"),
       ("Fcst", "RH", "SCALAR", 54, 57, 80, "all"),
       ("Fcst", "WindChill", "SCALAR", 54, 57, -1, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 54, 57, 80, "all"),
       ("Fcst", "WindGust", "SCALAR", 54, 57, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 54, 57, 0.1, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 54, 57, 500, "all"),
       ("Fcst", "FzLevel", "SCALAR", 54, 57, 800, "all"),
       ]

cg_57_60 = [
       ("Fcst", "T", "SCALAR", 57, 60, 55, "all"),
       ("Fcst", "Td", "SCALAR", 57, 60, 47, "all"),  
       ("Fcst", "Wind", "VECTOR", 57, 60, (12, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 57, 60, 40, "all"),
       ("Fcst", "Wx", "WEATHER", 57, 60, "Brf:RW:-:4SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 57, 60, 40, "all"),
       ("Fcst", "QPF", "SCALAR", 57, 60, .5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 57, 60, 0, "all"),
       ("Fcst", "RH", "SCALAR", 57, 60, 80, "all"),
       ("Fcst", "WindChill", "SCALAR", 57, 60, -1, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 57, 60, 80, "all"),             
       ("Fcst", "WindGust", "SCALAR", 57, 60, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 57, 60, 0.1, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 57, 60, 500, "all"),
       ("Fcst", "FzLevel", "SCALAR", 57, 60, 800, "all"),
       ]

cg_60_63 = [
       ("Fcst", "T", "SCALAR", 60, 63, 65, "all"),
       ("Fcst", "Td", "SCALAR", 60, 63, 43, "all"),  
       ("Fcst", "Wind", "VECTOR", 60, 63, (15, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 60, 63, 0, "all"),
       ("Fcst", "Wx", "WEATHER", 60, 63, "Wide:L:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 60, 63, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 60, 63, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 60, 63, 0, "all"),
       ("Fcst", "RH", "SCALAR", 60, 63, 80, "all"),
       ("Fcst", "WindChill", "SCALAR", 60, 63, -1, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 60, 63, 80, "all"),
       ("Fcst", "WindGust", "SCALAR", 60, 63, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 60, 63, 0.1, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 60, 63, 500, "all"),
       ("Fcst", "FzLevel", "SCALAR", 60, 63, 800, "all"),
       ]

cg_63_66 = [       
       ("Fcst", "T", "SCALAR", 63, 66, 70, "all"),
       ("Fcst", "Td", "SCALAR", 63, 66, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 63, 66, (15, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 63, 66, 5, "all"),
       ("Fcst", "Wx", "WEATHER", 63, 66, "SChc:L:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 63, 66, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 63, 66, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 63, 66, 0, "all"),
       ("Fcst", "RH", "SCALAR", 63, 66, 30, "all"),
       ("Fcst", "WindChill", "SCALAR", 63, 66, 10, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 63, 66, 106, "all"),
       ("Fcst", "WindGust", "SCALAR", 63, 66, 50, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 63, 66, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 63, 66, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 63, 66, 0, "all"),
       ]

cg_66_69 = [
       ("Fcst", "T", "SCALAR", 66, 69, 68, "all"),
       ("Fcst", "Td", "SCALAR", 66, 69, 55, "all"),  
       ("Fcst", "Wind", "VECTOR", 66, 69, (20, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 66, 69, 75, "all"),
       ("Fcst", "Wx", "WEATHER", 66, 69, "Chc:L:-:3SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 66, 69, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 66, 69, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 66, 69, 0, "all"),
       ("Fcst", "RH", "SCALAR", 66, 69, 30, "all"),
       ("Fcst", "WindChill", "SCALAR", 66, 69, 10, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 66, 69, 95, "all"),
       ("Fcst", "WindGust", "SCALAR", 66, 69, 50, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 66, 69, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 66, 69, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 66, 69, 0, "all"),
       ]

# Change to 6 hour grids

cg_69_75 = [
       ("Fcst", "T", "SCALAR", 69, 75, 69, "all"),
       ("Fcst", "Td", "SCALAR", 69, 75, 56, "all"),  
       ("Fcst", "Wind", "VECTOR", 69, 75, (10, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 69, 75, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 69, 75, "Areas:L:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 69, 75, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 69, 75, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 69, 75, 0, "all"),
       ("Fcst", "RH", "SCALAR", 69, 75, 30, "all"),
       ("Fcst", "WindChill", "SCALAR", 69, 75, 10, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 69, 75, 106, "all"),
       ("Fcst", "WindGust", "SCALAR", 69, 75, 50, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 69, 75, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 69, 75, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 69, 75, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 72", "MaxTEnd + 72", 71, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 72", "MinTEnd + 72", 65, "all"),
       ]

cg_75_81 = [
       ("Fcst", "T", "SCALAR", 75, 81, 69, "all"),
       ("Fcst", "Td", "SCALAR", 75, 81, 60, "all"),  
       ("Fcst", "Wind", "VECTOR", 75, 81, (5, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 75, 81, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 75, 81, "Frq:L:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 75, 81, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 75, 81, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 75, 81, 0, "all"),
       ("Fcst", "RH", "SCALAR", 75, 81, 40, "all"),
       ("Fcst", "WindChill", "SCALAR", 75, 81, 30, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 75, 81, 100, "all"),
       ("Fcst", "WindGust", "SCALAR", 75, 81, 40, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 75, 81, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 75, 81, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 75, 81, 0, "all"),
       ]

cg_81_87 = [       
       ("Fcst", "T", "SCALAR", 81, 87, 70, "all"),
       ("Fcst", "Td", "SCALAR", 81, 87, 61, "all"),  
       ("Fcst", "Wind", "VECTOR", 81, 87, (20, "SE"), "all"),
       ("Fcst", "Sky", "SCALAR", 81, 87, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 81, 87, "Pds:L:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 81, 87, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 81, 87, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 81, 87, 0, "all"),
       ("Fcst", "RH", "SCALAR", 81, 87, 40, "all"),
       ("Fcst", "WindChill", "SCALAR", 81, 87, 30, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 81, 87, 100, "all"),
       ("Fcst", "WindGust", "SCALAR", 81, 87, 40, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 81, 87, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 81, 87, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 81, 87, 0, "all"),
       ]

cg_87_93 = [
       ("Fcst", "T", "SCALAR", 87, 93, 71, "all"),
       ("Fcst", "Td", "SCALAR", 87, 93, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 87, 93, (15, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 87, 93, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 87, 93, "Wide:ZL:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 87, 93, 30, "all"),
       ("Fcst", "QPF", "SCALAR", 87, 93, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 87, 93, 0, "all"),
       ("Fcst", "RH", "SCALAR", 87, 93, 80, "all"),
       ("Fcst", "WindChill", "SCALAR", 87, 93, -1, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 87, 93, 80, "all"),
       ("Fcst", "WindGust", "SCALAR", 87, 93, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 87, 93, 0.1, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 87, 93, 500, "all"),
       ("Fcst", "FzLevel", "SCALAR", 87, 93, 800, "all"),
       ]

cg_93_99 = [
       ("Fcst", "T", "SCALAR", 93, 99, 65, "all"),
       ("Fcst", "Td", "SCALAR", 93, 99, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 93, 99, (23, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 93, 99, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 93, 99, "SChc:ZL:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 93, 99, 50, "all"),
       ("Fcst", "QPF", "SCALAR", 93, 99, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 93, 99, 0, "all"),
       ("Fcst", "RH", "SCALAR", 93, 99, 80, "all"),
       ("Fcst", "WindChill", "SCALAR", 93, 99, -1, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 93, 99, 80, "all"),
       ("Fcst", "WindGust", "SCALAR", 93, 99, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 93, 99, 0.1, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 93, 99, 500, "all"),
       ("Fcst", "FzLevel", "SCALAR", 93, 99, 800, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 96", "MaxTEnd + 96", 75, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 96", "MinTEnd + 96", 68, "all"),
       ]

cg_99_105 = [
       ("Fcst", "T", "SCALAR", 99, 105, 68, "all"),
       ("Fcst", "Td", "SCALAR", 99, 105, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 99, 105, (31, "NE"), "all"),
       ("Fcst", "Sky", "SCALAR", 99, 105, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 99, 105, "Chc:ZL:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 99, 105, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 99, 105, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 99, 105, 0, "all"),
       ("Fcst", "RH", "SCALAR", 99, 105, 10, "all"),
       ("Fcst", "WindChill", "SCALAR", 99, 105, 35, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 99, 105, 110, "all"),
       ("Fcst", "WindGust", "SCALAR", 99, 105, 55, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 99, 105, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 99, 105, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 99, 105, 0, "all"),
       ]

cg_105_111 = [       
       ("Fcst", "T", "SCALAR", 105, 111, 70, "all"),
       ("Fcst", "Td", "SCALAR", 105, 111, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 105, 111, (40, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 105, 111, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 105, 111, "Areas:ZL:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 105, 111, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 105, 111, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 105, 111, 0, "all"),
       ("Fcst", "RH", "SCALAR", 105, 111, 10, "all"),
       ("Fcst", "WindChill", "SCALAR", 105, 111, 35, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 105, 111, 110, "all"),
       ("Fcst", "WindGust", "SCALAR", 105, 111, 55, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 105, 111, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 105, 111, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 105, 111, 0, "all"),
       ]
       
cg_111_117 = [
       ("Fcst", "T", "SCALAR", 111, 117, 73, "all"),
       ("Fcst", "Td", "SCALAR", 111, 117, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 111, 117, (5, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 111, 117, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 111, 117, "Patchy:ZL:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 111, 117, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 111, 117, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 111, 117, 0, "all"),
       ("Fcst", "RH", "SCALAR", 111, 117, 20, "all"),
       ("Fcst", "WindChill", "SCALAR", 111, 117, 40, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 111, 117, 95, "all"),
       ("Fcst", "WindGust", "SCALAR", 111, 117, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 111, 117, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 111, 117, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 111, 117, 0, "all"),
       ]
       
cg_117_123 = [
       ("Fcst", "T", "SCALAR", 117, 123, 75, "all"),
       ("Fcst", "Td", "SCALAR", 117, 123, 60, "all"),  
       ("Fcst", "Wind", "VECTOR", 117, 123, (45, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 117, 123, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 117, 123, "Pds:ZL:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 117, 123, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 117, 123, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 117, 123, 0, "all"),
       ("Fcst", "RH", "SCALAR", 117, 123, 20, "all"),
       ("Fcst", "WindChill", "SCALAR", 117, 123, 40, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 117, 123, 95, "all"),
       ("Fcst", "WindGust", "SCALAR", 117, 123, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 117, 123, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 117, 123, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 117, 123, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 120", "MaxTEnd + 120", 83, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 120", "MinTEnd + 120", 78, "all"),
       ]

cg_123_129 = [
       ("Fcst", "T", "SCALAR", 123, 129, 78, "all"),
       ("Fcst", "Td", "SCALAR", 123, 129, 55, "all"),  
       ("Fcst", "Wind", "VECTOR", 123, 129, (17, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 123, 129, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 123, 129, "Wide:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 123, 129, 15, "all"),
       ("Fcst", "QPF", "SCALAR", 123, 129, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 123, 129, 0, "all"),
       ("Fcst", "RH", "SCALAR", 123, 129, 100, "all"),
       ("Fcst", "WindChill", "SCALAR", 123, 129, -5, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 123, 129, 80, "all"),
       ("Fcst", "WindGust", "SCALAR", 123, 129, 50, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 123, 129, 0.1, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 123, 129, 500, "all"),
       ("Fcst", "FzLevel", "SCALAR", 123, 129, 800, "all"),
       ]
       
cg_129_135 = [
       ("Fcst", "T", "SCALAR", 129, 135, 80, "all"),
       ("Fcst", "Td", "SCALAR", 129, 135, 50, "all"),  
       ("Fcst", "Wind", "VECTOR", 129, 135, (12, "SE"), "all"),
       ("Fcst", "Sky", "SCALAR", 129, 135, 20, "all"),
       ("Fcst", "Wx", "WEATHER", 129, 135, "Ocnl:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 129, 135, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 129, 135, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 129, 135, 0, "all"),
       ("Fcst", "RH", "SCALAR", 129, 135, 100, "all"),
       ("Fcst", "WindChill", "SCALAR", 129, 135, -5, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 129, 135, 80, "all"),
       ("Fcst", "WindGust", "SCALAR", 129, 135, 50, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 129, 135, 0.1, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 129, 135, 500, "all"),
       ("Fcst", "FzLevel", "SCALAR", 129, 135, 800, "all"),
       ]

cg_135_141 = [
       ("Fcst", "T", "SCALAR", 135, 141, 81, "all"),
       ("Fcst", "Td", "SCALAR", 135, 141, 45, "all"),  
       ("Fcst", "Wind", "VECTOR", 135, 141, (15, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 135, 141, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 135, 141, "Chc:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 135, 141, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 135, 141, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 135, 141, 0, "all"),
       ("Fcst", "RH", "SCALAR", 135, 141, 80, "all"),
       ("Fcst", "WindChill", "SCALAR", 135, 141, -20, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 135, 141, 106, "all"),
       ("Fcst", "WindGust", "SCALAR", 135, 141, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 135, 141, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 135, 141, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 135, 141, 3000, "all"),
       ]
       
cg_141_147 = [
       ("Fcst", "T", "SCALAR", 141, 147, 83, "all"),
       ("Fcst", "Td", "SCALAR", 141, 147, 43, "all"),  
       ("Fcst", "Wind", "VECTOR", 141, 147, (25, "NW"), "all"),
       ("Fcst", "Sky", "SCALAR", 141, 147, 20, "all"),
       ("Fcst", "Wx", "WEATHER", 141, 147, "Frq:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 141, 147, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 141, 147, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 141, 147, 0, "all"),
       ("Fcst", "RH", "SCALAR", 141, 147, 80, "all"),
       ("Fcst", "WindChill", "SCALAR", 141, 147, -20, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 141, 147, 106, "all"),
       ("Fcst", "WindGust", "SCALAR", 141, 147, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 141, 147, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 141, 147, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 141, 147, 3000, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 144", "MaxTEnd + 144", 90, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 144", "MinTEnd + 144", 83, "all"),
       ]

cg_147_153 = [
       ("Fcst", "T", "SCALAR", 147, 153, 83, "all"),
       ("Fcst", "Td", "SCALAR", 147, 153, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 147, 153, (22, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 147, 153, 20, "all"),
       ("Fcst", "Wx", "WEATHER", 147, 153, "Pds:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 147, 153, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 147, 153, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 147, 153, 0, "all"),
       ("Fcst", "RH", "SCALAR", 147, 153, 85, "all"),
       ("Fcst", "WindChill", "SCALAR", 147, 153, 5, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 147, 153, 95, "all"),
       ("Fcst", "WindGust", "SCALAR", 147, 153, 40, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 147, 153, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 147, 153, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 147, 153, 0, "all"),
       ]
       
cg_153_159 = [
       ("Fcst", "T", "SCALAR", 153, 159, 85, "all"),
       ("Fcst", "Td", "SCALAR", 153, 159, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 153, 159, (31, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 153, 159, 80, "all"),
       ("Fcst", "Wx", "WEATHER", 153, 159, "Wide:R:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 153, 159, 80, "all"),
       ("Fcst", "QPF", "SCALAR", 153, 159, 1, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 153, 159, 0, "all"),
       ("Fcst", "RH", "SCALAR", 153, 159, 85, "all"),
       ("Fcst", "WindChill", "SCALAR", 153, 159, 5, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 153, 159, 95, "all"),
       ("Fcst", "WindGust", "SCALAR", 153, 159, 40, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 153, 159, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 153, 159, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 153, 159, 0, "all"),
       ]
       
cg_159_165 = [
       ("Fcst", "T", "SCALAR", 159, 165, 86, "all"),
       ("Fcst", "Td", "SCALAR", 159, 165, 39, "all"),  
       ("Fcst", "Wind", "VECTOR", 159, 165, (45, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 159, 165, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 159, 165, "Ocnl:S:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 159, 165, 100, "all"),
       ("Fcst", "QPF", "SCALAR", 159, 165, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 159, 165, 1, "all"),
       ("Fcst", "RH", "SCALAR", 159, 165, 80, "all"),
       ("Fcst", "WindChill", "SCALAR", 159, 165, -20, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 159, 165, 100, "all"),
       ("Fcst", "WindGust", "SCALAR", 159, 165, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 159, 165, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 159, 165, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 159, 165, 3000, "all"),
       ]
       
cg_165_171 = [
       ("Fcst", "T", "SCALAR", 165, 171, 90, "all"),
       ("Fcst", "Td", "SCALAR", 165, 171, 30, "all"),  
       ("Fcst", "Wind", "VECTOR", 165, 171, (10, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 165, 171, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 165, 171, "Lkly:RW:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 165, 171, 100, "all"),
       ("Fcst", "QPF", "SCALAR", 165, 171, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 165, 171, 1, "all"),
       ("Fcst", "RH", "SCALAR", 165, 171, 80, "all"),
       ("Fcst", "WindChill", "SCALAR", 165, 171, -20, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 165, 171, 95, "all"),
       ("Fcst", "WindGust", "SCALAR", 165, 171, 30, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 165, 171, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 165, 171, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 165, 171, 3000, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 168", "MaxTEnd + 168", 93, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 168", "MinTEnd + 168", 90, "all"),
       ]

cg_171_177 = [
       ("Fcst", "T", "SCALAR", 171, 177, 93, "all"),
       ("Fcst", "Td", "SCALAR", 171, 177, 30, "all"),  
       ("Fcst", "Wind", "VECTOR", 171, 177, (10, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 171, 177, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 171, 177, "Chc:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 171, 177, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 171, 177, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 171, 177, 0, "all"),
       ("Fcst", "RH", "SCALAR", 171, 177, 60, "all"),
       ("Fcst", "WindChill", "SCALAR", 171, 177, -20, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 171, 177, 115, "all"),
       ("Fcst", "WindGust", "SCALAR", 171, 177, 25, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 171, 177, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 171, 177, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 171, 177, 0, "all"),
       ]
       
cg_177_183 = [
       ("Fcst", "T", "SCALAR", 177, 183, 91, "all"),
       ("Fcst", "Td", "SCALAR", 177, 183, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 177, 183, (15, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 177, 183, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 177, 183, "Patchy:F:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 177, 183, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 177, 183, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 177, 183, 0, "all"),
       ("Fcst", "RH", "SCALAR", 177, 183, 60, "all"),
       ("Fcst", "WindChill", "SCALAR", 177, 183, -20, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 177, 183, 115, "all"),
       ("Fcst", "WindGust", "SCALAR", 177, 183, 25, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 177, 183, 0, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 177, 183, 0, "all"),
       ("Fcst", "FzLevel", "SCALAR", 177, 183, 0, "all"),
       ]

cg_183_189 = [
       ("Fcst", "T", "SCALAR", 183, 189, 90, "all"),
       ("Fcst", "Td", "SCALAR", 183, 189, 35, "all"),  
       ("Fcst", "Wind", "VECTOR", 183, 189, (20, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 183, 189, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 183, 189, "Pds:L:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 183, 189, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 183, 189, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 183, 189, 0, "all"),
       ("Fcst", "RH", "SCALAR", 183, 189, 90, "all"),
       ("Fcst", "WindChill", "SCALAR", 183, 189, 0, "all"),
       ("Fcst", "HeatIndex", "SCALAR", 183, 189, 120, "all"),
       ("Fcst", "WindGust", "SCALAR", 183, 189, 35, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 183, 189, 1, "all"),
       ("Fcst", "SnowLevel", "SCALAR", 183, 189, 300, "all"),
       ("Fcst", "FzLevel", "SCALAR", 183, 189, 2000, "all"),
       ]

cg_hazard = [
       ("Fcst", "Hazards", "DISCRETE", 6, 12, "WS.A", "all"),
       ]

cg_WindChill = [
       ("Fcst", "WindChill", "SFC", 6, 189),
       ]

cg_HeatIndex = [
       ("Fcst", "HeatIndex", "SFC", 6, 189),
       ]

cg_StormTotalSnow = [
       ("Fcst", "StormTotalSnow", "SCALAR", 6, 18, 1, "all"),
       ("Fcst", "StormTotalSnow", "SCALAR", 18, 30, 2, "all"),
       ]

cg_MaxT = [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 93, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 24", "MaxTEnd + 24", 87, "all"),
       ]

cg_MinT = [
       ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 60, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 24", "MinTEnd + 24", 56, "all"),
       ]

cg_PoP = [
       ("Fcst", "PoP", "SCALAR", 6, 12, 90, "all"),
       ("Fcst", "PoP", "SCALAR", 12, 18, 40, "all"),
       ("Fcst", "PoP", "SCALAR", 18, 30, 100, "all"),
       ("Fcst", "PoP", "SCALAR", 30, 42, 30, "all"),
       ]

cg_MaxRH = [
       ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin", "MaxRHEnd", 100, "all"),
       ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin + 24", "MaxRHEnd + 24", 65, "all"),
       ]

cg_MinRH = [
       ("Fcst", "MinRH", "SCALAR", "MinRHBegin", "MinRHEnd", 50, "all"),
       ("Fcst", "MinRH", "SCALAR", "MinRHBegin + 24", "MinRHEnd + 24", 30, "all"),
       ]

cg_MaxMinT = [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 93, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin - 24", "MaxTEnd - 24", 73, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 50, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin - 24", "MinTEnd - 24", 70, "all"),
       ]

general_createGrids = cg_3_6 + \
                      cg_6_9 + \
                      cg_9_12 + \
                      cg_12_15 + \
                      cg_15_18 + \
                      cg_18_21 + \
                      cg_21_24 + \
                      cg_24_27 + \
                      cg_27_30 + \
                      cg_30_33 + \
                      cg_33_36 + \
                      cg_36_39 + \
                      cg_39_42 + \
                      cg_42_45 + \
                      cg_45_48 + \
                      cg_48_51 + \
                      cg_51_54 + \
                      cg_54_57 + \
                      cg_57_60 + \
                      cg_60_63 + \
                      cg_63_66 + \
                      cg_66_69 + \
                      cg_69_75 + \
                      cg_75_81 + \
                      cg_81_87 + \
                      cg_87_93 + \
                      cg_93_99 + \
                      cg_99_105 + \
                      cg_105_111 + \
                      cg_111_117 + \
                      cg_117_123 + \
                      cg_123_129 + \
                      cg_129_135 + \
                      cg_135_141 + \
                      cg_141_147 + \
                      cg_147_153 + \
                      cg_153_159 + \
                      cg_159_165 + \
                      cg_165_171 + \
                      cg_171_177 + \
                      cg_177_183 + \
                      cg_183_189 + \
                      cg_hazard

sparse_deleteGrids = cg_WindChill + \
                     cg_HeatIndex

sts_createGrids = cg_StormTotalSnow

imet_createGrids = cg_MaxT + \
                   cg_MinT + \
                   cg_PoP

imet2_createGrids = cg_MaxT + \
                    cg_MinT + \
                    cg_PoP + \
                    cg_MaxRH + \
                    cg_MinRH

reptrends_createGrids = cg_MaxMinT

def tropical(checkDaylight=False):
    if checkDaylight:
        dlt = time.localtime(time.time())[8]
    else:
        dlt = 0
    return [
       ("Fcst", "Hazards", "DISCRETE", 6, 189, "HU.A^TR.W", "all"),
       ("Fcst", "Wind", "VECTOR", 6, 189, (40, "W"), "all"),
       ("Fcst", "WindGust", "SCALAR", 6, 189, 55, "all"),
       
       ("Fcst", "pwsD34", "SCALAR", 11+dlt, 23+dlt, 30, "all"),
       ("Fcst", "pwsD64", "SCALAR", 11+dlt, 23+dlt, 30, "all"),
       ("Fcst", "pwsN34", "SCALAR", 23+dlt, 35+dlt, 30, "all"),
       ("Fcst", "pwsN64", "SCALAR", 23+dlt, 35+dlt, 30, "all"),
       
       ("Fcst", "pwsD34", "SCALAR", 35+dlt, 47+dlt, 30, "all"),
       ("Fcst", "pwsD64", "SCALAR", 35+dlt, 47+dlt, 30, "all"),
       ("Fcst", "pwsN34", "SCALAR", 47+dlt, 59+dlt, 30, "all"),
       ("Fcst", "pwsN64", "SCALAR", 47+dlt, 59+dlt, 30, "all"),
       
       ("Fcst", "pwsD34", "SCALAR", 59+dlt, 71+dlt, 30, "all"),
       ("Fcst", "pwsD64", "SCALAR", 59+dlt, 71+dlt, 30, "all"),
       ("Fcst", "pwsN34", "SCALAR", 71+dlt, 83+dlt, 30, "all"),
       ("Fcst", "pwsN64", "SCALAR", 71+dlt, 83+dlt, 30, "all"),

              
       ("Fcst", "pwsD34", "SCALAR", 83+dlt, 95+dlt, 30, "all"),
       ("Fcst", "pwsD64", "SCALAR", 83+dlt, 95+dlt, 30, "all"),
       ("Fcst", "pwsN34", "SCALAR", 95+dlt, 107+dlt, 30, "all"),
       ("Fcst", "pwsN64", "SCALAR", 95+dlt, 107+dlt, 30, "all"),

              
       ("Fcst", "pwsD34", "SCALAR", 107+dlt, 119+dlt, 30, "all"),
       ("Fcst", "pwsD64", "SCALAR", 107+dlt, 119+dlt, 30, "all"),
       ("Fcst", "pwsN34", "SCALAR", 119+dlt, 131+dlt, 30, "all"),
       ("Fcst", "pwsN64", "SCALAR", 119+dlt, 131+dlt, 30, "all"),

              
       ("Fcst", "pwsD34", "SCALAR", 131+dlt, 143+dlt, 30, "all"),
       ("Fcst", "pwsD64", "SCALAR", 131+dlt, 143+dlt, 30, "all"),
       ("Fcst", "pwsN34", "SCALAR", 143+dlt, 155+dlt, 30, "all"),
       ("Fcst", "pwsN64", "SCALAR", 143+dlt, 155+dlt, 30, "all"),

       
       ("Fcst", "pwsD34", "SCALAR", 155+dlt, 167+dlt, 30, "all"),
       ("Fcst", "pwsD64", "SCALAR", 155+dlt, 167+dlt, 30, "all"),
       ("Fcst", "pwsN34", "SCALAR", 167+dlt, 179+dlt, 30, "all"),
       ("Fcst", "pwsN64", "SCALAR", 167+dlt, 179+dlt, 30, "all"),              

       ]


tonight = [
    ".TONIGHT...",
    "Windy.","Mostly clear.",
    "Brief thunderstorms in the evening...then widespread rain and intermittent thunderstorms after midnight.",
    "Lows near zero.","Temperatures rising into the lower 40s after midnight.",
    "Northeast winds 30 to 35 mph becoming west around 15 mph after midnight.",
    "Chance of precipitation 50 percent.","Wind chill readings zero to 10 above zero.",
    ]

sat_thru_thurs = [
    ".SATURDAY...",
    "Rain likely in the morning...then intermittent rain in the afternoon.",
    "Much cooler.","Rain may be heavy at times in the afternoon.",
    #"VISIBILITY ONE QUARTER MILE OR LESS AT TIMES in the morning.",
    "Snow level 0 feet.",
    "Highs around 50.",
    "Southwest winds around 10 mph with gusts to around 35 mph shifting to the southeast with gusts to around 45 mph in the afternoon.",
    
    ".SATURDAY NIGHT...",
    "Widespread rain showers in the evening...then partly cloudy with sprinkles likely after midnight.",
    "Not as cold.","Locally heavy rainfall possible in the evening.","Lows in the lower 30s.",
    "East winds up to 5 mph shifting to the northwest after midnight.","Gusts up to 45 mph.",
    "Wind chill readings 1 below to 15 above zero.",
    
    ".SUNDAY...",
    "Warmer.","Rain showers in the morning...then widespread drizzle in the afternoon.",
    "Highs around 70.",
    # Take this out because test ZFP8 has this as Period_6_14 instead of Period_4_5
    # and thus does not have a wind_withGusts_phrase
    #"East winds 10 to 15 mph.",
    #"Gusts up to 35 mph increasing to 60 mph in the afternoon.",
    "Chance of showers 60 percent.",
    "Wind chill readings 1 below to 10 above zero.",
    
    ".SUNDAY NIGHT...",
    "Warmer.",
    "Mostly clear.","Areas of drizzle in the evening...then frequent drizzle after midnight.",
    "Near steady temperature in the upper 60s.", # DR_18363 "Lows around 50.",
    
    ".MONDAY...",
    "Breezy","Sunny","Periods of drizzle through the day.",
    "Widespread light freezing drizzle in the afternoon.",
    "Near steady temperature around 70.", # DR_18363 "Highs in the lower 70s.",
    "Lowest wind chill readings 1 below to 30 above zero in the afternoon.",
    
    ".MONDAY NIGHT...",
    "Breezy...warmer.",
    "Partly cloudy.",
    "Widespread light freezing drizzle in the evening...then chance of light freezing drizzle after midnight.",
    "Lows in the mid 60s.",
    "Lowest wind chill readings 1 below to 9 above zero in the evening.",
    "Heat index readings around 110 after midnight.",
    
    ".TUESDAY...",
    "Windy","Sunny.","Areas of very light freezing drizzle.", "Highs in the mid 70s.",
    "Heat index readings around 110.",
    
    ".TUESDAY NIGHT...",
    "Mostly clear.","Periods of very light freezing drizzle.","Lows in the upper 60s.",
    "Lowest wind chill readings 5 below to 40 above zero after midnight.",
    
    ".WEDNESDAY...",
    "Sunny.", "Near steady temperature around 80.", # DR_18363 "Highs in the lower 80s.",
    "Lowest wind chill readings 5 below to 20 below zero in the afternoon.",
    
    ".WEDNESDAY NIGHT...",
    "Windy.", "Mostly clear.", "Near steady temperature in the lower 80s.",
    # DR_18363 "Lows in the upper 70s.",
    "Lowest wind chill readings 10 below to 20 below zero in the evening.",
    
    ".THURSDAY...",
    "Windy.", "Widespread light rain and periods of light freezing rain in the morning...then widespread light rain and occasional very light snow in the afternoon.",
    "Near steady temperature in the mid 80s.", # DR_18363 "Highs around 90.",
    "Chance of precipitation near 100 percent.",
    "Lowest wind chill readings 20 below to 5 above zero in the afternoon.",
    ]

thursNite_fri = [
    ".THURSDAY NIGHT...",
    "Occasional very light snow and sprinkles likely in the evening...then mostly clear with sprinkles likely and chance of thunderstorms after midnight.",
    "Light snow accumulations.","Lows in the lower 80s.",
    "Wind chill readings 10 below to 20 below zero.",
    "Heat index readings 108 to 115 after midnight.",
    
    ".FRIDAY...",
    "Breezy","Sunny.","Patchy fog through the day.","Periods of drizzle in the afternoon.",
    "Near steady temperature in the lower 90s",
    "Lowest wind chill readings 10 below to 20 below zero in the morning.",
    "Heat index readings 110 to 120.",
    ]


tonight_thru_thurs = tonight + sat_thru_thurs

latLonEditAreas = """

Definition["defaultEditAreas"] = [
                ((28.48, -82.39, 0), "Area 1"),
                ((28.48, -82.30, .0001), "Area 2"),
                ]
                
"""

import TestScript

scripts = [
    {
    "commentary":"""
    Morning Test at 4 a.m.
    """,
    "name":"ZFP_1", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "400 AM EST Fri Jan 1 2010",
                    "...WINTER STORM WATCH IN EFFECT UNTIL NOON EST TODAY...",
                    ".TODAY...",
                    "Sunny in the morning.",
                    "Widespread thunderstorms early in the afternoon.",
                    "Slight chance of thunderstorms late in the afternoon.","Highs around 100.",
                    "Temperatures falling into the mid 50s in the afternoon.",
                    "Southwest winds around 10 mph with gusts to around 30 mph shifting to the southeast with gusts to around 40 mph in the afternoon.",
                    "Chance of thunderstorms near 100 percent",
                    "Lowest wind chill readings zero to 20 above zero in the afternoon.",
                    ] + tonight_thru_thurs,
#    "createGrids": TestScript.general_createGrids,
    "createGrids": general_createGrids,
    },
    {
    "commentary":"""
    Morning with Pre- 1st Period Test at 4 a.m.
    """,
    "name":"ZFP_2", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning with Pre-1st Period', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "400 AM EST Fri Jan 1 2010",
                    "...WINTER STORM WATCH IN EFFECT UNTIL NOON EST TODAY...",
                    ".TODAY...",
                    "Cloudy early in the morning then becoming sunny then becoming mostly sunny late in the morning",
                    "Scattered thunderstorms early in the morning.",
                    "Widespread thunderstorms early in the afternoon...then slight chance of thunderstorms late in the afternoon.",
                    "Highs around 100.",
                    "Southwest winds 10 to 15 mph with gusts to around 30 mph shifting to the southeast with gusts to around 40 mph in the afternoon.",
                    "Lowest wind chill readings zero to 20 above zero in the afternoon.",
                    ] + tonight_thru_thurs,
    
    "createGrids": general_createGrids, 
    },
    {
    "commentary":"""
    Morning Update
    """,
    "name":"ZFP_3", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning Update', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "1000 AM EST Fri Jan 1 2010",
                    "...WINTER STORM WATCH IN EFFECT UNTIL NOON EST TODAY...",
                    ".REST OF TODAY...",
                    "Mostly sunny late in the morning.",
                    "Widespread thunderstorms early in the afternoon.",
                    "Slight chance of thunderstorms late in the afternoon.","Highs around 100.",
                    "Temperatures falling into the mid 50s in the afternoon.",
                    "South winds around 15 mph.",
                    "Gusts up to 30 mph increasing to 40 mph in the afternoon.",
                    "Lowest wind chill readings zero to 20 above zero in the afternoon.",
                    ] + tonight_thru_thurs,
    
    "createGrids": general_createGrids, 
    "drtHour": 10,
    },
    {
    "commentary":"""
    Afternoon Update Test at 2 p.m.
    """,
    "name":"ZFP_4", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon Update', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "200 PM EST Fri Jan 1 2010",
                    ".REST OF TODAY...",
                    "Windy.","Widespread thunderstorms early in the afternoon...then slight chance of thunderstorms late in the afternoon.",
                    "Highs around 100.","Temperatures falling into the mid 50s in the afternoon.",
                    "East winds around 30 mph.","Wind chill readings zero to 20 above zero.",
                    ] + tonight_thru_thurs,
    "createGrids": general_createGrids, 
    "drtHour": 14,
    },
    {
    "commentary":"""
    Afternoon Test at 6 p.m.
    """,
    "name":"ZFP_5", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "600 PM EST Fri Jan 1 2010",
                    ".TONIGHT...",
                    "Windy.","Mostly clear.",
                    "Thunderstorms likely early in the evening...then brief thunderstorms in the late evening and early morning.",
                    "Widespread rain late in the night.","Lows near zero.",
                    "Temperatures rising into the lower 40s after midnight.",
                    "Northeast winds 30 to 35 mph becoming west around 15 mph after midnight.",
                    "Chance of precipitation 50 percent.",
                    "Wind chill readings zero to 10 above zero.",
                    ] + sat_thru_thurs + thursNite_fri,
    "createGrids": general_createGrids,
    "drtHour": 18,
    },
    {
    "commentary":"""
    Afternoon with Pre- 1st Period Test at 6 p.m.
    """,
    "name":"ZFP_6", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon with Pre-1st Period', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "600 PM EST Fri Jan 1 2010",
                    ".TONIGHT...",
                    "Windy.",
                    "Cloudy early in the evening then clearing","Lows near zero.",
                    "East winds around 30 mph shifting to the northeast in the evening...then becoming west around 15 mph after midnight",
                    "Wind chill readings zero to 10 above zero.", 
                    ] + sat_thru_thurs + thursNite_fri,

    "createGrids": general_createGrids,
    "drtHour": 18,
    },
    {
    "commentary":"""
    Evening Update Test at 10 p.m.
    """,
    "name":"ZFP_7", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Evening Update', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "1000 PM EST Fri Jan 1 2010",
                    ".REST OF TONIGHT...",
                    "Windy.","Mostly clear.",
                    "Intermittent thunderstorms in the late evening and early morning...then widespread rain early in the morning.",
                    "Lows near zero.","Temperatures rising into the lower 40s after midnight.",
                    "North winds around 30 mph becoming west around 15 mph after midnight.",
                    "Chance of precipitation 50 percent.",
                    "Wind chill readings zero to 10 above zero.",
                    ] + sat_thru_thurs + thursNite_fri,
  
    "createGrids": general_createGrids,
    "drtHour": 22,
    },
    {
    "commentary":"""
    Early Morning Update Test at 2 a.m.
    """,
    "name":"ZFP_8", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Early Morning Update', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "200 AM EST Fri Jan 1 2010",
                    "...WINTER STORM WATCH IN EFFECT FROM 6 AM THIS MORNING TO NOON EST TODAY...",
                    ".REST OF TONIGHT...",
                    "Southwest winds around 15 mph with gusts to around 30 mph",
                    ".FRIDAY...",
                    "Sunny in the morning...then widespread thunderstorms in the afternoon.",
                    "Highs around 100.",
                    "Temperatures falling into the mid 50s in the afternoon.",
                    "Southwest winds around 10 mph with gusts to around 30 mph shifting to the southeast with gusts to around 40 mph in the afternoon.",
                    "Lowest wind chill readings zero to 20 above zero in the afternoon.",
                    
                    ".FRIDAY NIGHT...",
                    "Windy.",
                    "Mostly clear.",
                    "Brief thunderstorms in the evening...then widespread rain and intermittent thunderstorms after midnight.",
                    "Lows near zero.","Temperatures rising into the lower 40s after midnight.",
                    "Northeast winds 30 to 35 mph becoming west around 15 mph after midnight.",
                    "Chance of precipitation 50 percent.",
                    "Wind chill readings zero to 10 above zero.", 
                    ] + sat_thru_thurs,
    "createGrids": general_createGrids,
    "drtHour": 2,
    },
    {
    "commentary":"""
    Deleting WindChill and HeatIndex grids
    """,
    "name":"ZFP_9", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "400 AM EST Fri Jan 1 2010",
                    "...WINTER STORM WATCH IN EFFECT UNTIL NOON EST TODAY...",
                    ".TODAY...",
                    "Sunny in the morning.",
                    "Widespread thunderstorms early in the afternoon.",
                    "Slight chance of thunderstorms late in the afternoon.","Highs around 100.",
                    "Temperatures falling into the mid 50s in the afternoon.",
                    "Southwest winds around 10 mph with gusts to around 30 mph shifting to the southeast with gusts to around 40 mph in the afternoon.",

                    ".TONIGHT...",
                    "Windy.", "Mostly clear. ",
                    "Brief thunderstorms in the evening...then widespread rain and intermittent thunderstorms after midnight.",
                    "Lows near zero.","Temperatures rising into the lower 40s after midnight.",
                    "Northeast winds 30 to 35 mph becoming west around 15 mph after midnight.",
                    "Chance of precipitation 50 percent.",
                    ],
    "notCheckStrings": [
           "Wind chill readings zero to 10 above zero.",
           "Heat index readings around 110 after midnight.",
           ],
    "deleteGrids": sparse_deleteGrids,
    "drtHour": 4,
    },

    {
    "commentary":"""
    Using StormTotalSnow grid
    """,
    "name":"ZFP_10", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "drtHour": 18,
    "createGrids": sts_createGrids,
    "fileChanges": [
       ("ZFP_<site>_Definition", "TextUtility", "replace", (fc1_1, fc1_2), "undo"),
       ],
    "checkStrings": ["Zone Forecast Product for Florida",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "600 PM EST Fri Jan 1 2010",
                     ".TODAY...",
                     "Storm total snow accumulation around 1 inch",
                     ".TONIGHT...",
                     "Storm total snow accumulation around 2 inches",
                     ],
    },

    {
    "commentary":"""
    Using includeMultipleElementTable grid
    """,
    "name":"ZFP_11",
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1,
    "drtHour": 18,
    "createGrids": imet_createGrids,
    "fileChanges": [
       ("ZFP_<site>_Definition", "TextUtility", "replace", (fc1_3, fc1_4), "undo"),
       ],
    "checkStrings": ["Zone Forecast Product for Florida",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "600 PM EST Fri Jan 1 2010",
                     "Spot temperatures and probabilities of measurable precipitation are for Today...Tonight...and Saturday.",
                     ".TODAY...", "Highs in the lower 90s.",
                     ".TONIGHT...", "Lows around 60.",
                     ".SATURDAY...", "Highs in the upper 80s.",
                     ".SATURDAY NIGHT...", "Lows in the mid 50s.",
                     "               TEMPERATURE     /    PRECIPITATION",
                     "City 4       93     60     87  /  90    100     30",
                     ],
    },
    
    {
    "commentary":"""
    Using includeMultipleElementTable and SingleValueFormat grids
    """,
    "name":"ZFP_12",
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1,
    "drtHour": 18,
    "createGrids": imet_createGrids,
    "fileChanges": [
       ("ZFP_<site>_Definition", "TextUtility", "replace", (fc1_3, fc1_4), "undo"),
       ("ZFP_<site>_Definition", "TextUtility", "replace", (fc1_5, fc1_6), "undo"),
       ],
    "checkStrings": ["Zone Forecast Product for Florida",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "600 PM EST Fri Jan 1 2010",
                     "Spot temperatures and probabilities of measurable precipitation are for and Today.",
                     ".TODAY...", "Highs in the lower 90s.",
                     ".TONIGHT...", "Lows around 60.",
                     ".SATURDAY...", "Highs in the upper 80s.",
                     ".SATURDAY NIGHT...", "Lows in the mid 50s.",
                     "            TEMP/ POP",
                     "City 4      93    90",
                     ],
    },
    
    {
    "commentary":"""
    Using includeMultipleElementTable grids with Humidity
    """,
    "name":"ZFP_13",
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1,
    "drtHour": 18,
    "createGrids": imet2_createGrids,
    "fileChanges": [
       ("ZFP_<site>_Definition", "TextUtility", "replace", (fc1_3, fc1_4), "undo"),
       ("ZFP_<site>_Definition", "TextUtility", "replace", (fc1_7, fc1_8), "undo"),
       ],
    "checkStrings": ["Zone Forecast Product for Florida",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "600 PM EST Fri Jan 1 2010",
                     "Spot temperatures and probabilities of measurable precipitation are for Today...Tonight...and Saturday.",
                     ".TODAY...", "Highs in the lower 90s.",
                     ".TONIGHT...", "Lows around 60.",
                     ".SATURDAY...", "Highs in the upper 80s.",
                     ".SATURDAY NIGHT...", "Lows in the mid 50s.",
                     "            TEMPERATURE  / PRECIPITATION /    HUMIDITY",
                     "City 4       93  60  87  /  90 100  30   /   50 100  30",
                     ],
    },

    {
    "commentary":"""
    Using includeMultipleElementTable and SingleValueFormat grids with Humidity
    """,
    "name":"ZFP_14",
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1,
    "drtHour": 18,
    "createGrids": imet2_createGrids,
    "fileChanges": [
       ("ZFP_<site>_Definition", "TextUtility", "replace", (fc1_3, fc1_4), "undo"),
       ("ZFP_<site>_Definition", "TextUtility", "replace", (fc1_7, fc1_8), "undo"),
       ("ZFP_<site>_Definition", "TextUtility", "replace", (fc1_5, fc1_6), "undo"),
       ],
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "600 PM EST Fri Jan 1 2010",
                    "Spot temperatures and probabilities of measurable precipitation are for and Today.",
                    ".TODAY...", "Highs in the lower 90s.",
                    ".TONIGHT...", "Lows around 60.",
                    ".SATURDAY...", "Highs in the upper 80s.",
                    ".SATURDAY NIGHT...", "Lows in the mid 50s.",
                    "            TEMP/ POP / HUM",
                    "City 4      93    90    50",
                    ],
    },

    {
    "commentary":"""
    Using includeMultipleElementTable with Afternoon Update
    """,
    "name":"ZFP_15",
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon Update', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1,
    "drtHour": 14,
    "createGrids": imet_createGrids,
    "fileChanges": [
       ("ZFP_<site>_Definition", "TextUtility", "replace", (fc1_3, fc1_4), "undo"),
       ],
    "checkStrings": ["Zone Forecast Product for Florida",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "200 PM EST Fri Jan 1 2010",
                     "Spot temperatures and probabilities of measurable precipitation are for Rest of Today...Tonight...and Saturday.",
                     ".REST OF TODAY...", "Highs in the lower 90s.",
                     ".TONIGHT...", "Lows around 60.",
                     ".SATURDAY...", "Highs in the upper 80s.",
                     ".SATURDAY NIGHT...", "Lows in the mid 50s.",
                     "               TEMPERATURE     /    PRECIPITATION",
                     "City 4       93     60     87  /  40    100     30",
                     ],
    },

    {
    "commentary":"""
    Morning Test using Lat/Lon edit areas
    """,
    "name":"ZFP_LatLonAreas", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 0, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "400 AM EST Fri Jan 1 2010",
                    # First lat/lon area
                    "-012100-",
                    "...WINTER STORM WATCH IN EFFECT UNTIL NOON EST TODAY...",
                    ".TODAY...",
                    "Sunny in the morning.",
                    "Widespread thunderstorms early in the afternoon.",
                    "Slight chance of thunderstorms late in the afternoon.","Highs around 100.",
                    "Temperatures falling into the mid 50s in the afternoon.",
                    "South winds 10 to 15 mph with gusts to around 30 mph increasing to east 15 to 30 mph in the afternoon.",
                    "Chance of thunderstorms near 100 percent.",
                    "Lowest wind chill readings zero to 20 above zero in the afternoon",
                    # Second lat/lon area
                    "-012100-",
                    "...WINTER STORM WATCH IN EFFECT UNTIL NOON EST TODAY...",
                    ".TODAY...",
                    "Sunny in the morning.",
                    "Widespread thunderstorms early in the afternoon.",
                    "Slight chance of thunderstorms late in the afternoon.","Highs around 100.",
                    "Temperatures falling into the mid 50s in the afternoon.",
                    "South winds 10 to 15 mph with gusts to around 30 mph increasing to east 15 to 30 mph in the afternoon.",
                    "Chance of thunderstorms near 100 percent.",
                    "Lowest wind chill readings zero to 20 above zero in the afternoon",
                    ],
    "createGrids": general_createGrids,
    "fileChanges": [
       ("ZFP_<site>_Definition", "TextUtility", "add", latLonEditAreas, "undo"),    
       ],
    },

    {
    "commentary":"""
    First period total snow report
    """,
    "name":"ZFP_FirstPeriodTotalSnow1", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "400 AM EST Fri Jan 1 2010",
                    "...WINTER STORM WATCH IN EFFECT UNTIL NOON EST TODAY...",
                    ".TODAY...",
                    "Snow accumulation around 3 inches",
                    "Total snow accumulation around 7 inches",
                    ".TONIGHT...",
                    ],
    "createGrids": [
       ("Fcst", "SnowAmt", "SCALAR", 0, 6, 4, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 6, 18, 3, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 18, 30, 0, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Ocnl:S:--:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 18, "Ocnl:S:--:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 18, 30, "NoWx", "all"),
       ("Fcst", "PoP", "SCALAR", 0, 6, 90, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 18, 90, "all"),
       ("Fcst", "PoP", "SCALAR", 18, 30, 0, "all"),
       ],
    },
    
    {
    "commentary":"""
    First period total snow report with zero past snow amt grid
    """,
    "name":"ZFP_FirstPeriodTotalSnow2", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "400 AM EST Fri Jan 1 2010",
                    "...WINTER STORM WATCH IN EFFECT UNTIL NOON EST TODAY...",
                    ".TODAY...",
                    ],
    "notCheckStrings": ["Total snow accumulation"],
    "createGrids": [
       ("Fcst", "SnowAmt", "SCALAR", 0, 6, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 6, 18, 3, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 18, 120, 0, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Ocnl:S:--:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 6, 18, "Ocnl:S:--:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 18, 30, "NoWx", "all"),
       ("Fcst", "PoP", "SCALAR", 0, 6, 90, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 18, 90, "all"),
       ("Fcst", "PoP", "SCALAR", 18, 30, 0, "all"),
       ],
    },

    {
    "commentary":"""
    Snow in the afternoon with no past snow on update issuance -
    there should have no "new" snow accumulation wording
    """,
    "name":"ZFP_FirstPeriodSnow_Update", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning Update', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "1000 AM EST Fri Jan 1 2010",
                    ".REST OF TODAY...",
                    "Snow likely in the afternoon.",
                    "Snow accumulation around 2 inches.",
                    "Chance of snow 60 percent."
                    ],
    "notCheckStrings": ["New snow accumulation"],
    "createGrids": [
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 12, 18, 2, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 18, 120, 0, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 12, 18, "Lkly:S:m:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 18, 30, "NoWx", "all"),
       ("Fcst", "PoP", "SCALAR", 0, 6, 0, "all"),
       ("Fcst", "PoP", "SCALAR", 6, 18, 60, "all"),
       ("Fcst", "PoP", "SCALAR", 18, 30, 0, "all"),
       ],
    "drtHour": 10,
    },

    {
    "commentary":"""
    Check total snow amount in the second period when there is past snow.
    Also check a past snow grid longer than 12 hours.
    """,
    "name":"ZFP_SecondPeriod_TotalSnow", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "400 AM EST Fri Jan 1 2010",
                    ".TODAY...",
                    "Snow likely.",
                    "Snow accumulation around 4 inches.",
                    "Chance of snow 60 percent.",
                    ".TONIGHT...",
                    "Snow likely.",
                    "Snow accumulation around 2 inches.",
                    "Total snow accumulation around 10 inches.",
                    "Chance of snow 60 percent.",
                    ],
    "createGrids": [
       ("Fcst", "SnowAmt", "SCALAR", -18, 6, 4, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 6, 18, 4, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 18, 30, 2, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 30, 120, 0, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 18, "Lkly:S:m:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 18, 30, "Lkly:S:m:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 30, 42, "Chc:S:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 18, 60, "all"),
       ("Fcst", "PoP", "SCALAR", 18, 30, 60, "all"),
       ("Fcst", "PoP", "SCALAR", 30, 42, 30, "all"),
       ],
    },

    # reportTrends had problem reporting trends in the first period
    # on update issuances (DR 18581)
    {
    "commentary":"""
    Report trends Afternoon update
    """,
    "name":"ZFP_repTrends_AfternoonUpdate", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon Update', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "200 PM EST Fri Jan 1 2010",
                    ".REST OF TODAY...",
                    "warmer.", "Highs in the lower 90s.",
                    ".TONIGHT...",
                    "cooler.", "Lows around 50.",
                    ],
    "createGrids": reptrends_createGrids,
    "drtHour": 14,
    },

    # reportTrends had problem reporting trends in the first period
    # on update issuances (DR 18581)
    {
    "commentary":"""
    Report trends Evening Update Test
    """,
    "name":"ZFP_repTrends_EveningUpdate", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Evening Update', ('Issued By', 'issuedBy'): None }",
    "comboFlag": 1, 
    "checkStrings": [
                    "Zone Forecast Product for Florida",
                    "National Weather Service Tampa Bay Ruskin FL",
                    "1000 PM EST Fri Jan 1 2010",
                    ".REST OF TONIGHT...",
                    "cooler.", "Lows around 50.",
                    ],
    "createGrids": reptrends_createGrids,
    "drtHour": 22,
    },


    # Tropical phrases (DR 19851)
    #  Pablo Santos and Matt Belk can thoroughly test the Tropical formatters
    #  These tests are just a small sanity check.
    #  Since there is no CWF TestScript, the CWF test is here as well.
    {
    "commentary":"""
    Test Tropical phrasing
    """,
    "name":"ZFP_Tropical", 
    "productType":"ZFP",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None,('Include Tropical?', 'includeTropical'): 'Yes' }",
    "comboFlag": 1, 
    "checkStrings": [
                    ".TODAY...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                    ".TONIGHT...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                    ".SATURDAY...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                    ".SATURDAY NIGHT...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                     ".SUNDAY...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                     ".SUNDAY NIGHT...",
                    "Hurricane conditions possible",
                    ".MONDAY...",
                    "Hurricane conditions possible",
                    ".MONDAY NIGHT...",
                    "Hurricane conditions possible",
                    ".TUESDAY...",
                    "Hurricane conditions possible",
                    ".TUESDAY NIGHT...",
                    "Hurricane conditions possible",
                    ".WEDNESDAY...",
                    "Hurricane conditions possible",                    
                    ".WEDNESDAY NIGHT...",
                    "Hurricane conditions possible",
                    ".THURSDAY...",
                    "Hurricane conditions possible",                    
                    ],
    "createGrids": tropical(),
    # Uncomment these lines for testing in a Today time frame
    #"createGrids": tropical(True),
    #"gridsStartTime": "Midnight Today",
    #"drtTime": None,
    },
    {
    "commentary":"""
    Test Tropical phrasing
    """,
    "name":"CWF_Tropical", 
    "productType":"CWF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None,('Include Tropical?', 'includeTropical'): 'Yes' }",
    "comboFlag": 1, 
    "checkStrings": [
                    ".TODAY...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                    ".TONIGHT...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                    ".SATURDAY...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                    ".SATURDAY NIGHT...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                     ".SUNDAY...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                     ".SUNDAY NIGHT...",
                    "Hurricane conditions possible",
                    ".MONDAY...",
                    "Hurricane conditions possible",
                    ".MONDAY NIGHT...",
                    "Hurricane conditions possible",
                    ".TUESDAY...",
                    "Hurricane conditions possible",
                    ],
    "createGrids": tropical(),
    },
    {
    "commentary":"""
    Test Tropical phrasing
    """,
    "name":"CWF_Pacific_Tropical", 
    "productType":"CWF_Pacific",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None,('Include Tropical?', 'includeTropical'): 'Yes' }",
    "comboFlag": 1, 
    "checkStrings": [
                    ".TODAY...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                    ".TONIGHT...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                    ".SAT...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                    ".SAT NIGHT...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                    ".SUN...",
                    "Tropical storm conditions expected with hurricane conditions possible",
                    ".SUN NIGHT...",
                    "Hurricane conditions possible",
                    ],
    "createGrids": tropical(),
    },
           
    {
    "name":"ZFP_CleanUp",
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
        "internalStrip": 0, 
        }
    for script in scripts:
        drtHour = script.get("drtHour", None)
        if drtHour is not None:
            script["drtTime"] =  self.getAbsFromLocal(2010, 1, 1, drtHour, 0)      
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults, level=level)


