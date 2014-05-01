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
#----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without
# technical support, and with no warranty, express or implied, as to
# its usefulness for any purpose.
#
# ER_NDFD_GridCheck # # Author:
# Chris Gibson NWSFO SLC    10/03
# Updated for ER : Jim Hayes 7/03 
# Updated        : Tom Mazza 7/04
#                  -handles new NDFD times
#                  -includes configuration section
#                  -added logic to look for summer or winter grids as appropriate
# Developer email Chris.Gibson@noaa.gov, Thomas.Mazza@noaa.gov.
#
# Checks for grids in the Fcst database for each time required in the
# NDFD matrix. Specifies "groups" of weather elements for Public, Fire Weather, etc.
# Uses 00Z as base time for timerange (TR). Thus, no dependence on new model
# data for TR.
#
# 00Z to 12Z grids are needed for case of evening
# update after 00z. Elements and times are configured in dictionaries
# below.  Uses getGridInfo to test if grid exists. 
# ----------------------------------------------------------------------------
# Known version issues: None Tested in GFE RPP 17/18/19 (Does NOT
# work in RPP18.5)
# Works in IFPS15; model name changes to be made for IFPS16
# edit areas needed: None
# Smart tools needed: None
# Further work:  Needs a getTime function to create time ranges for each
# element and depending on the time of day (issuance).
# Potentially this could eliminate the need for
# different tables for day and night shifts.
#
#
################################################################################################
#
#  Configuration Section
#
#  Here is where to control whether this procedure shows up in GFE and, if so, under which menu;
#  control the weather element groups available, i.e., remove "Marine" if in an inland office;
#  and to control the start of the day shift, and the start and end of winter and summer to
#  determine which public weather elements to check for.
#
availableElementGroups = ["Public", "Fire Weather", "Marine"]
#
# Marine can be eliminated for inland sites.  It could be replaced with another group, but
# the elements and valid time info in the def marine_elements_dict_DayShift and _MidShift
# modules below need edited to include the appropriate infoprmation.
#
#################################################################################
#
#  Control whether this procedure shows up in GFE and, if so, under which menu:
#
##MenuItems = ["Consistency"]
MenuItems = [""]
#
#
#################################################################################
#
#
#  Control the weather element groups available:
#
availableElementGroups = []
availableElementGroups.append(("Public"))
availableElementGroups.append(("Fire Weather"))
#availableElementGroups.append(("Marine")) ## also uncomment the marine section near the bottom

VariableList = []
VariableList.append(("Elements List for Grid Completeness Check" , "", "label"))
VariableList.append(("Which element group(s)?" ,
                    ["Public"], "check",
                    availableElementGroups))

#
#
#################################################################################
#
#
#  Control the start of the day shift:
#  (Mid Shift hard-coded to begin at 00Z)
#
#
startDayShift = 15 # (15Z)
#
#
#################################################################################
#
#
#  Control the start and end of winter and summer:
#  (Seasons can overlap or be disjoint - e.g., entries of 10, 5, 5 and 10 below,
#   respectively, mean May and October are part of both winter and summer, which,
#   in turn, means that both winter-specific and summer-specific elements will
#   be checked for.)
#
#
StartWinter = 10   # (October is first month of Winter)
EndWinter   =  5   # (May is last month of Winter)
StartSummer =  5   # (May is first month of Summer)
EndSummer   = 10   # (October is last month of Summer)
#
#  End Configuration Section
#
################################################################################################


import SmartScript, string, time
class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def public_elements_dict_DayShift_Both(self): ## used 15Z-2359Z
        return [
        ## Format ("WeatherElement", Startgrid, interval, Finalgrid), Start/Final based on 00z.
            ("MaxT", 36, 24, 191),
            ("MinT", 24, 24, 180), 
            ("T", 24, 3, 192),
            ("Td", 24, 3, 192),
            ("HeatIndex", 24, 3, 96),
            ("WindChill", 24, 3, 96),
            ("RH", 24, 3, 192),
            ("PoP", 24, 12, 192),
            ("PoP12hr", 24, 12, 192),
            ("Sky", 24, 3, 192),
            ("Wind", 24, 3, 192),
            ("WindGust", 24, 3, 144),
            ("Wx", 24, 3, 192),
            ("QPF", 24, 6, 95),
            ("QPF6hr", 24, 6, 95),
            ("SnowAmt", 24, 6, 71),
            ("SnowAmt6hr", 24, 6, 71),
            ]

    def public_elements_dict_MidShift_Both(self): ## used 00Z-1459Z
        return [
            ("MaxT", 12, 24, 167),
            ("MinT", 24, 24, 156), 
            ("T", 12, 3, 168),
            ("Td", 12, 3, 168),
            ("HeatIndex", 12, 3, 72),
            ("WindChill", 12, 3, 72),
            ("RH", 12, 3, 168),
            ("PoP", 12, 12, 168),
            ("PoP12hr", 12, 12, 168),
            ("Sky", 12, 3, 168),
            ("Wind", 12, 3,168),
            ("WindGust", 12, 3, 132),
            ("Wx", 12,3, 168),
            ("QPF", 12, 6, 71),
            ("QPF6hr", 12, 6, 71),
            ("SnowAmt", 12, 6, 47),
            ("SnowAmt6hr", 12, 6, 47)
            ]
    
    def public_elements_dict_DayShift_Winter(self): ## used 15Z-2359Z
        return [
        ## Format ("WeatherElement", Startgrid, interval, Finalgrid), Start/Final based on 00z.
            ("MaxT", 36, 24, 191),
            ("MinT", 24, 24, 180), 
            ("T", 24, 3, 192),
            ("Td", 24, 3, 192),
            ("WindChill", 24, 3, 96),
            ("RH", 24, 3, 192),
            ("PoP", 24, 12, 192),
            ("PoP12hr", 24, 12, 192),
            ("Sky", 24, 3, 192),
            ("Wind", 24, 3, 192),
            ("WindGust", 24, 3, 144),
            ("Wx", 24, 3, 192),
            ("QPF", 24, 6, 95),
            ("QPF6hr", 24, 6, 95),
            ("SnowAmt", 24, 6, 71),
            ("SnowAmt6hr", 24, 6, 71),
            ]

    def public_elements_dict_MidShift_Winter(self): ## used 00Z-1459Z
        return [
            ("MaxT", 12, 24, 167),
            ("MinT", 24, 24, 156), 
            ("T", 12, 3, 168),
            ("Td", 12, 3, 168),
            ("WindChill", 12, 3, 72),
            ("RH", 12, 3, 168),
            ("PoP", 12, 12, 168),
            ("PoP12hr", 12, 12, 168),
            ("Sky", 12, 3, 168),
            ("Wind", 12, 3,168),
            ("WindGust", 12, 3, 132),
            ("Wx", 12,3, 168),
            ("QPF", 12, 6, 71),
            ("QPF6hr", 12, 6, 71),
            ("SnowAmt", 12, 6, 47),
            ("SnowAmt6hr", 12, 6, 47)
            ]
    
    def public_elements_dict_DayShift_Summer(self): ## used 15Z-2359Z
        return [
        ## Format ("WeatherElement", Startgrid, interval, Finalgrid), Start/Final based on 00z.
            ("MaxT", 36, 24, 191),
            ("MinT", 24, 24, 180), 
            ("T", 24, 3, 192),
            ("Td", 24, 3, 192),
            ("HeatIndex", 24, 3, 96),
            ("RH", 24, 3, 192),
            ("PoP", 24, 12, 192),
            ("PoP12hr", 24, 12, 192),
            ("Sky", 24, 3, 192),
            ("Wind", 24, 3, 192),
            ("WindGust", 24, 3, 144),
            ("Wx", 24, 3, 192),
            ("QPF", 24, 6, 95),
            ("QPF6hr", 24, 6, 95),
            ]

    def public_elements_dict_MidShift_Summer(self): ## used 00Z-1459Z
        return [
            ("MaxT", 12, 24, 167),
            ("MinT", 24, 24, 156), 
            ("T", 12, 3, 168),
            ("Td", 12, 3, 168),
            ("HeatIndex", 12, 3, 72),
            ("RH", 12, 3, 168),
            ("PoP", 12, 12, 168),
            ("PoP12hr", 12, 12, 168),
            ("Sky", 12, 3, 168),
            ("Wind", 12, 3,168),
            ("WindGust", 12, 3, 132),
            ("Wx", 12,3, 168),
            ("QPF", 12, 6, 71),
            ("QPF6hr", 12, 6, 71),
            ]
    
    def public_elements_dict_DayShift_None(self): ## used 15Z-2359Z
        return [
        ## Format ("WeatherElement", Startgrid, interval, Finalgrid), Start/Final based on 00z.
            ("MaxT", 36, 24, 191),
            ("MinT", 24, 24, 180), 
            ("T", 24, 3, 192),
            ("Td", 24, 3, 192),
            ("RH", 24, 3, 192),
            ("PoP", 24, 12, 192),
            ("PoP12hr", 24, 12, 192),
            ("Sky", 24, 3, 192),
            ("Wind", 24, 3, 192),
            ("WindGust", 24, 3, 144),
            ("Wx", 24, 3, 192),
            ("QPF", 24, 6, 95),
            ("QPF6hr", 24, 6, 95),
            ]

    def public_elements_dict_MidShift_None(self): ## used 00Z-1459Z
        return [
            ("MaxT", 12, 24, 167),
            ("MinT", 24, 24, 156), 
            ("T", 12, 3, 168),
            ("Td", 12, 3, 168),
            ("RH", 12, 3, 168),
            ("PoP", 12, 12, 168),
            ("PoP12hr", 12, 12, 168),
            ("Sky", 12, 3, 168),
            ("Wind", 12, 3,168),
            ("WindGust", 12, 3, 132),
            ("Wx", 12,3, 168),
            ("QPF", 12, 6, 71),
            ("QPF6hr", 12, 6, 71),
            ]

    def marine_elements_dict_DayShift(self):
        return [
            ("WaveHeight", 24, 12, 144),
            ("Vsby", 24, 12, 144),
            ]

    def marine_elements_dict_MidShift(self):
        return [
            ("WaveHeight", 12, 12, 120),
            ("Vsby", 12, 12, 120),
            ]

    def fwx_elements_dict_DayShift(self):
        return [
            ("MinRH", 20, 24, 72),
            ("MaxRH", 32, 24, 60), 
            ("LAL", 18, 6, 72),
            ("MixHgt", 24, 12, 72),
            ("TransWind", 24, 12, 72),
            ("Haines", 18, 6, 72),
            ]

    def fwx_elements_dict_MidShift(self):
        return [
            ("MinRH", 8, 24, 60),
            ("MaxRH", 20, 24, 48), 
            ("LAL", 6, 6, 60),
            ("MixHgt", 12, 12, 60),
            ("TransWind", 12, 12, 60),
            ("Haines", 6, 6, 60),
]

    def execute(self, editArea, timeRange, varDict):
        missingFlag = 99 ## if stays 99 then no valid list selected.

        ## Figure out if day or mid shift ZFP package.
        time1 = time.gmtime()
        (year, month, day, h, m, s, w, day, dst) = time1
        ## print "ZHour: ", h
        ## Adjust hours as needed in Zulu.
        if h > startDayShift - 1: # day shift
            Day = 1
        else:
            Day = 0
#        print month, StartWinter, EndSummer
        Season = "None"

        ##########################################################
        #
        #  Check for summer and not winter
        #
        if month > StartSummer - 1 and month > EndWinter:
            if month < EndSummer + 1 and month < StartWinter:
                Season = "Summer"
        #
        #
        ##########################################################
        #
        #  Check for winter and not summer
        #
        if Season == "None":
            if month < EndWinter + 1 or month > StartWinter - 1:
                print month, StartWinter, EndSummer
                if month < StartSummer or month > EndSummer:
                    Season = "Winter"
        #
        #
        ##########################################################
        #
        #  Check for winter and summer - seasons overlap
        #
        if Season == "None":
            if month > StartSummer - 1 and month < EndSummer + 1:
                if month < EndWinter + 1 or month > StartWinter - 1:
                    Season = "Both"
        #
        #
        ##########################################################
        #
        #  Otherwise not winter and not summer - seasons disjoint
        #  - Season left as "None".
        #
        print Season

        
        ## print "Day: ", Day 
        ## print "month: ", month

        print varDict["Which element group(s)?"]

        elementGroups = varDict["Which element group(s)?"]

##        for elementList in varDict["Which element group(s)?"]:
##            missingFlag = 0
        for elementGroup in elementGroups:
        # loop through element groups
            missingFlag = 0
        
            if elementGroup == "Public":
                ## public elements
                ## Check for shift time here and use appropriate dictionary.
                if Day == 1:
                    if Season == "Summer":
                        element_list = self.public_elements_dict_DayShift_Summer()
                    elif Season == "Winter":
                        element_list = self.public_elements_dict_DayShift_Winter()
                    elif Season == "Both":
                        element_list = self.public_elements_dict_DayShift_Both()
                    else:
                        element_list = self.public_elements_dict_DayShift_None()
                    endInterval3 = 84  #  end of 3 hourly requirement (see below)
                else:
                    if Season == "Summer":
                        element_list = self.public_elements_dict_MidShift_Summer()
                    elif Season == "Winter":
                        element_list = self.public_elements_dict_MidShift_Winter()
                    elif Season == "Both":
                        element_list = self.public_elements_dict_MidShift_Both()
                    else:
                        element_list = self.public_elements_dict_MidShift_None()
                    endInterval3 = 60  #  end of 3 hourly requirement (see below)

                print "element_list = ", element_list
                for x in xrange(len(element_list)):
                # loop through elements
                    element, StartTime, Interval, FinalTime = element_list[x]
                    Grid = StartTime
                    print x, element_list[x], Grid, FinalTime
                    while (Grid <= FinalTime):

                        ## End of 3 hourly requirement:
                        ## Hourly public elements required every 3 hours in the short term
                        ## are only required every 6 hours after 72 hours (84 hours from 00Z).
                        if Interval < 6 and Grid >= endInterval3:
                            Interval = 6
                            
                        ## get timeRange based on 00z today
                        ## TR of hour to+1 hour returns 00Z-01Z, 03Z-04Z, etc. This matches
                        ## NDFD which shows "snapshots" of grids at these times, as opposed
                        ## to snaphots of 23-00Z, 02-03Z, etc. Satisfies NDFD requirements
                        ## as of the spring of 2004, when their code was changed from the old
                        ## NDFD, which showed "snapshots" of grids at 23-00Z, 02-03Z, etc.
                        ## Either way allows dictionaries to remain 3,6,9,12, etc.
        ##                    timeRange = self.createTimeRange(Grid-1, Grid, mode="Zulu")
                        timeRange = self.createTimeRange(Grid, Grid+1, mode="Zulu")
                        gridInfo = self.getGridInfo("Fcst", element, "SFC", timeRange)
                        if gridInfo == []:
                            missingFlag = 1
                            message = "Missing " + element + " grid for " + str(timeRange)
                            self.statusBarMsg(message ,"S")
                        Grid += Interval

            elif elementGroup == "Fire Weather":
                ## fwx elements
                if Day == 1:
                    element_list = self.fwx_elements_dict_DayShift()
                else:
                    element_list = self.fwx_elements_dict_MidShift()

                for x in xrange(len(element_list)):
                # loop through elements
                    element, StartTime, Interval, FinalTime = element_list[x]
                    Grid = StartTime
                    while (Grid <= FinalTime):
                        ## get timeRange based on 00z today
    ##                    timeRange = self.createTimeRange(Grid-1, Grid, mode="Zulu") 
                        timeRange = self.createTimeRange(Grid, Grid+1, mode="Zulu") 
                        gridInfo = self.getGridInfo("Fcst", element, "SFC", timeRange)
                        if gridInfo == []:
                            missingFlag = 1
                            message = "Missing " + element + " grid for " + str(timeRange)
                            self.statusBarMsg(message ,"S")
                        Grid += Interval 

            ## This section for Marine
##            elif elementGroup == "Marine":
##                if Day == 1:
##                    element_list = self.marine_elements_dict_DayShift()
##                else:
##                    element_list = self.marine_elements_dict_MidShift()
##
##                for x in xrange(len(element_list)):
##                # loop through elements
##                    element, StartTime, Interval, FinalTime = element_list[x]
##                    Grid = StartTime
##                    while (Grid <= FinalTime):
##                        ## get timeRange based on 00z today
##    ##                    timeRange = self.createTimeRange(Grid-1, Grid, mode="Zulu") 
##                        timeRange = self.createTimeRange(Grid, Grid+1, mode="Zulu")
##                        gridInfo = self.getGridInfo("Fcst", element, "SFC", timeRange)
##                        if gridInfo == []:
##                            missingFlag = 1
##                            message = "Missing " + element + " grid for " + str(timeRange)
##                            self.statusBarMsg(message ,"S")
##                        Grid = Grid + Interval

        if missingFlag == 0:
            self.statusBarMsg("-- All necessary grids found. --","R")
        else:
            self.statusBarMsg("-- Missing grids listed below !! --","S")       
  
       
 
