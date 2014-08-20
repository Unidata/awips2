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
# SerpConfig.py
#
#  Config information for the Serp tool.
#
# ----------------------------------------------------------------------------
Config = {}
#
#  Locations should be a dictionary, with keynames that are "group names" and
#  values that are lists of point information for points in that group. 
#  For each point in the list, a tuple with 3 values is necessary, with a 
#  ("name",lat,lon).
#
#  The tool will automatically provide a group of "Current Samples".  If you
#  ONLY want that group, and no pre-defined groups, then make an empty 
#  dictionary like: Config["Locations"]={}
#
Config["Locations"]={
    "Zone Cities":    [("Boise",             43.57, -116.22 ),
                       ("Ontario",           44.02, -117.01 ),
                       ("Caldwell",          43.64, -116.64 ),
                       ("Mountain Home",     43.05, -115.87 ),
                       ("Twin Falls",        42.50, -114.45 ),
                       ("Jerome",            42.73, -114.45 ),
                       ("Council",           44.73, -116.43 ),
                       ("Cambridge",         44.57, -116.68 ),
                       ("McCall",            44.88, -116.10 ),
                       ("Idaho City",        43.83, -115.83 ),
                       ("Fairfield",         43.30, -114.80 ),
                       ("Baker",             44.83, -117.82 ),
                       ("Burns",             43.60, -118.95 ),
                       ("Rome",              42.58, -117.88 ),
                      ],
    "RAWS+ZoneCities":[("Boise",             43.57, -116.22 ),
                       ("Ontario",           44.02, -117.01 ),
                       ("Caldwell",          43.64, -116.64 ),
                       ("Mountain Home",     43.05, -115.87 ),
                       ("Twin Falls",        42.50, -114.45 ),
                       ("Jerome",            42.73, -114.45 ),
                       ("Council",           44.73, -116.43 ),
                       ("Cambridge",         44.57, -116.68 ),
                       ("McCall",            44.88, -116.10 ),
                       ("Idaho City",        43.83, -115.83 ),
                       ("Fairfield",         43.30, -114.80 ),
                       ("Baker",             44.83, -117.82 ),
                       ("Burns",             43.60, -118.95 ),
                       ("Rome",              42.58, -117.88 ),
                       ("Ski Hill",          44.940,-116.188),
                       ("Weiser River",      44.848,-116.428),
                       ("Snake River",       45.100,-116.737),         
                       ("Lodgepole",         45.379,-115.189),
                       ("TeaPot",            44.904,-115.738),
                       ("Bearskin Creek",    44.385,-115.550),
                       ("Pine Creek",        44.250,-116.199),
                       ("Little Anderson",   44.091,-115.881),
                       ("Town Creek",        43.944,-115.917),
                       ("Wagontown",         43.573,-115.327),
                       ("Lucky Peak",        43.588,-115.992),
                       ("Dead Indian Ridge", 44.326,-117.169),
                       ("Horse Butte",       42.417,-115.228),
                       ("Brace Flat",        42.352,-116.692),
                       ("Triangle",          42.829,-116.589),
                       ("Twin Buttes",       42.691,-115.195),
                       ("Pole Creek",        42.069,-115.786),
                       ("Sho-Pai",           42.018,-116.213),
                       ("Deer Haven",        43.174,-115.152),
                       ("Bull Springs",      42.080,-114.485),
                       ("Riddle Mountain",   43.101,-118.498),
                       ("Wagontire",         43.343,-119.881),
                       ("Sage Hen",          43.514,-119.294),
                       ("Basque Hills",      42.255,-118.968),
                       ("Fish Fin Rim",      42.47, -119.18 ),
                       ("P Hill",            42.823,-118.935),
                       ("Bald Moutain",      43.557,-118.407),
                       ("Foster Flat",       42.974,-119.246),
                       ("Moon Hill",         42.859,-118.679),
                       ("Little McCoy Creek",42.708,-118.510),
                       ("Grassy Mountain",   42.626,-117.395),
                       ("Kelsay Butte",      43.901,-117.987),
                       ("Owyhee Ridge",      43.518,-117.240),
                       ("Red Butte",         43.536,-117.835),
                       ("Alkali Flat",       44.087,-117.226),
                       ("Flagstaff Hill",    44.814,-117.729),
                       ("Elk Creek",         44.758,-117.971),
                       ("Yellowpine",        44.526,-118.323),
                      ],
        }
#
#  "DefaultGroup" is a string with the name of the default sample group. It
#  must match one of the sample group labels specified in "Locations" above OR 
#  be "Current Samples" (that group is provided automatically)
#
Config["DefaultGroup"] = "Zone Cities"
#
#  "MaxPointsInColumn" is an integer number, with the max points to list in 
#  each column of the dialog before another column is started.  However, once 
#  another column is added, the number of points in each column is calculated
#  to be as equal as possible.
#
Config["MaxPointsInColumn"]=10
#
#  "ElevationDefault" is a string with either "On" or "Off".  This sets the
#  default state of elevation effects when the dialog starts up.
#
Config["ElevationDefault"]="On"
#
#==============================================================================
#
#  The following empty code is here to fool the ifpServer into
#  thinking it's a tool.  This is so that the configuration will 
#  appear right next to the primary tool.
#
#  DO NOT CHANGE THE LINES BELOW
#
ToolType = "numeric"
WeatherElementEdited = "None"
from numpy import *
HideTool = 1

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
    def execute(self):
        return
