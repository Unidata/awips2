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
# ERQCcheck
#
# Authors:  Matthew H. Belk     WFO Taunton, MA         Created: 04/25/03
#           Thomas R. Mazza     WFO Charleston, WV      Last Modified: 3/29/06
#           Some of the modules used by this procedure were edited from modules
#           originally written by Bob Stauber, Steve Nelson, Jim Hayes, Paul
#           Jendrowski and Tom LeFebvre.
# ----------------------------------------------------------------------------

import time
import AbsTime

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify
MenuItems = ["Consistency"]
import LogStream, time

# The ToolList is optional, but recommended, if you are calling
# Smart Tools from your Script.
# If present, it can be used to show which grids will be
# modified by the Script.

ToolList = []

#################################################################################
#
#
#  Control the weather element groups available:
#
availableElementGroups = ["Public", "Fire Weather"]
#availableElementGroups = ["Public", "Fire Weather", "Marine"]
#
# Marine can be eliminated for inland sites.


VariableList = []
                #################################################################
                #
                # get time range to check

VariableList.append(("Use Selected Time Range from the Grid Manager ?", "Y", "radio",
                  ["N", "Y"]))
VariableList.append(("If not, Pick period to start with here:", "Today", "radio",
                 ["Today (00Z Cycle Only)", "Tonight", "Tomorrow", "Tomorrow Night", "Next Day", "Next Day Night",
                  "Day3 day", "Day3 night", "Day4", "Day5", "Day6", "Day7"]))
VariableList.append(("...and to end with here:", "Day7", "radio",
                 ["Today (00Z Cycle Only)", "Tonight", "Tomorrow", "Tomorrow Night", "Next Day", "Next Day Night",
                  "Day3 day", "Day3 night", "Day4", "Day5", "Day6", "Day7"]))
VariableList.append(("00Z or 12Z cycle (don't use Today if 12Z) ?", "Auto", "radio",
                 ["Auto", "00Z cycle", "12Z cycle"]))
VariableList.append(("", "", "label"))

                #################################################################
                #
                # get elements to check

VariableList.append(("All (Overrides other choices if not No)", "No", "radio",
                 ["No", "Highlight only", "Fix All"]))
VariableList.append(("NDFD Grid Check (Checks all elements all 7 days)", "No", "radio",
                 ["No", "Yes"]))
VariableList.append(("Which element group(s)?" ,
                 ["Public"], "check",
                 availableElementGroups,
                 ))
VariableList.append(("For each element, choose No not to check, Highlight only to highlight inconsistencies, \
and Fix to actually fix inconsistencies.", "", "label"))
##VariableList.append(("Checks for Temperatures and Wind Gusts (Checks all 7 days)..." , "", "label"))
VariableList.append(("Checks for Temperatures and Wind Gusts (Checks all 7 days), and for Sky, PoP, Wx, QPF and SnowAmt :", "", "label"))
VariableList.append(("Temperatures", "No", "radio",
                 ["No", "Highlight only", "Fix"]))
VariableList.append(("Wind Gusts", "No", "radio",
                 ["No", "Highlight only", "Fix"]))
##VariableList.append(("Zero out wind gusts not in excess of sustained wind by more than:" , "5", "numeric"))
##VariableList.append(("Minimum wind gust to report:" , "15", "numeric"))
##VariableList.append(("Checks for Sky, PoP, Wx, QPF and SnowAmt...", "", "label"))
VariableList.append(("CheckSkyWithPoP", "No", "radio",
                 ["No", "Highlight only", "Fix"]))
VariableList.append(("Sky vs PoP Relationship:", "add", "radio",
                       ["add", "multiply", "Sky Limit"]))
##VariableList.append(("CheckPoPwithSky", "No", "radio",
##                 ["No", "Highlight only", "Fix"]))
##VariableList.append(("CheckPoPwithWx", "No", "radio",
##                 ["No", "Highlight only", "Fix"]))
##VariableList.append(("CheckWxWithPoP", "No", "radio",
##                 ["No", "Highlight only", "Fix"]))
##VariableList.append(("NoPoPNoQPF", "No", "radio",
##                 ["No", "Highlight only", "Fix"]))
##VariableList.append(("NoPoPNoSnowAmt", "No", "radio",
##                 ["No", "Highlight only", "Fix"]))
##VariableList.append(("Run PPI", "Yes", "radio", ## For offices doing Precipitation Probability Index images for the web
##                 ["No", "Yes"]))                ## (Also uncomment the two PPI sections near the bottom)
VariableList.append(("For wind gusts :", "", "label"))
VariableList.append(("Limit wind gusts in excess of sustained wind by:", "12", "numeric"))
VariableList.append(("Make wind gusts in excess of sustained wind by factor of at least:", "1.0", "numeric"))
VariableList.append(("For Sky and PoP :", "", "label"))
VariableList.append(("For add, multiply (smaller factor), by how much ?" , "20", "numeric"))
VariableList.append(("For Sky Limit, only Sky less than Limit affected; it is raised to the Limit:", "", "label"))
VariableList.append(("Enter Sky Limit: the minimum Sky cover needed to support Wx:" , 60, "numeric"))
##VariableList.append(("Enter minimum PoP for measurable precip:", 15, "numeric"))
VariableList.append(("Enter Sky cover for 5% PoP:" , 30, "numeric"))
VariableList.append(('For checks between QPF, SnowAmt, PoP and Wx, if "Cleanup" is selected, then\nonly cleanup actions will run. No checks will be made, regardless of the above settings.', '', 'label'))
VariableList.append(('Check_Cleanup', 'Check', 'radio', ['Check', 'Cleanup']))
VariableList.append(('Run SnowAmt/QPF Check?', ['Yes'], 'check', ['Yes']))
VariableList.append(('Run SnowAmt/Wx Check?', ['Yes'], 'check', ['Yes']))
VariableList.append(('Run QPF/PoP Check?', ['Yes'], 'check', ['Yes']))
VariableList.append(('Run QPF/Wx Check?', ['Yes'], 'check', ['Yes']))

#  Procedures and Tools used in QCcheck with credits
#
#  In this table, Procedures and Tools used by a Procedure
#  are indented once (4 spaces), Procedures and Tools used
#  by a Procedure used by a Procedure indented again, etc.
#
#  Tools with two pound signs (##) at the beginning of the
#  are baseline tools used by ERQCcheck, and are listed
#  here for reference only.
#
#   Procedure or Tool               Procedure   Tool (Wx        Credit
#                                               Element
#                                               Edited)
#
#   ERQCcheck                       Procedure                   Nelson/Mazza
#   NDFDgridCheck                   Procedure                   Hayes, James
#   CheckTemepratues                Procedure                   LeFebvre, Tom
##          RHTool                              RH
##          WindChillTool                       WindChill
##          HeatIndexTool                       HeatIndex
#   CheckWindGust                   Procedure                   LeFebvre/Mazza
#       CheckSkyWithPoP                         Sky             Nelson, Steve
#       CheckPoPwithWx                          PoP             Nelson
#       CheckWx                                 Wx              Nelson
#       CheckQPF                                QPF             
#       CheckSnowAmt                            SnowAmt
#       EnufCloudForPoP                         Sky             Mazza
#       ForcePoPtoWx                            PoP
#       PoP12hrFmMaxPoP                         PoP12hr         Mazza
#       NoPoPNoQPF                              QPF             Mazza
#       NoPoPNoSnowAmt                          SnowAmt         Mazza
#
#  The following tools and procedures are no longer used by
#  ERQCcheck since Tom LeFebvre's CheckTandTd. This reduces
#  the total number of Procedures and Tools involved from 31 to 14.
#  These Tools and Procedures can safely be removed from your local GFE.
#
#       CheckMaxTvsMinTvsMaxT       Procedure                   Stauber/Mazza
#           MakeTemporaryMinT                   MinT            Stauber, Bob
#           TempMinTfmMinT                      MinT            Stauber/Mazza
#           CheckMinTagainstMaxT                TempMinT        Stauber/Mazza
#           CheckMaxTagainstMinT                MaxT            Stauber
#       CheckTagainstMaxTandMinT    Procedure                   Stauber/Mazza
#           CheckTagainstMinT                   T               Stauber
#           CheckTd                             Td              Stauber
#           CheckTagainstMaxT                   T               Stauber
#       MaxTvsMinTvsMaxT            Procedure                   Stauber/Mazza
##          MakeTemporaryMinT                   MinT            Stauber
##          TempMinTfmMinT                      MinT            Stauber
#           MinTaobMaxT                         TempMinT        Stauber
#           MinTfrTempMinT                      MinT            Stauber
#           MaxTaoaMinT                         MaxT            Stauber
#       ForceTbetweenMaxTandMinT    Procedure                   Stauber/Mazza
#           MakeTaoaMinT                        T               Stauber
#           MakeTaobMaxT                        T               Stauber
#           TdLessTbyUsrAmt                     Td              Mazza, Thomas
#       CheckTd                                 Td


import SmartScript
## For documentation on the available commands,
##   see the SmartScript Utility, which can be viewed from
##   the Edit Actions Dialog Utilities window

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, editArea, timeRange, varDict):
        # Checks chosen dependent elements in GFE against appropriate independent elements
        self.logProcUse("Procedure execution")

        ####################################################################################
        #
        #  Configuration Section
        #
        #  Zulu time threshold for procedure to begin using 12Z as the cycle
        #  (periods are pushed one day ahead, thereby adding a new day to the forecast)
        #
        start12Zcycle = 12
        #
        #  Zulu time for beginning of daytime period, e.g., at what zulu hour do periods
        #  like "Today", "Tomorrow" and "Day5" begin
        #
        startDayPeriod = 12
        #
        #  Difference in hours between zulu time and local standard time (zulu - local)
        #
        localStandardOffset = 5
        #
        #  Zulu time for beginning of MaxT grid
        #
        maxTstartTimeZ = 12
        #
        #  Duration of MaxT grid
        #
        maxTdur = 12
        #
        #  Zulu time for beginning of MinT grid
        #
        minTstartTimeZ = 0
        #
        #  Duration of MinT grid
        #
        minTdur = 13
        #
        QCarea = self.getEditArea("ISC_Send_Area")
        #
        GustLessSusMin = 5  # Zero out wind gusts not in excess of sustained wind by more than
        #
        minGust = 15  # Minimum wind gust to report
        #
        TemperatureProc = "CheckTandTd"
        #  I set this up so that if we chnage the name of CheckTandTd.Procedure, we could
        #  just chnage it here instead of every opccurence within this procedure.
        #TemperatureProc = "CheckTemperatures"
        WindGustProc = "CheckWindGust"
        startHeatIndex = 4      ## First month to report heat index
        endHeatIndex = 10       ## Last month to report heat index
        startWindChill = 10     ## First month to report wind chill
        endWindChill = 4        ## Last month to report wind chill
        #
        #  End Configuration Section
        #
        ####################################################################################

        Allareas = editArea  ##  Hold all areas when procedure invoked w/o wedit area selected.
                             ##  This is necessary for grids created from scratch

##        areaFmProc = self.getEditArea(varDict["savedEditArea"])
##        procAreaMask = self.encodeEditArea(areaFmProc)
        # Get user chosen time range

        deltaStartDay = startDayPeriod - 12
        varDict["Start 12Z cycle at zulu time"] = start12Zcycle
        varDict["Hours past 12Z to start day periods"] = deltaStartDay
        varDict["Local standard offset"] = localStandardOffset
        varDict["MaxT start time"] = maxTstartTimeZ
        varDict["MaxT duration"] = maxTdur
        varDict["MinT start time"] = minTstartTimeZ
        varDict["MinT duration"] = minTdur

        now = time.localtime(time.time())
        nowZ = time.gmtime(time.time())
        if now[8] == 1: edton = 1   ##  Lying on an eiderdown
        else: edton = 0
        cycle = varDict["00Z or 12Z cycle (don't use Today if 12Z) ?"]
        if cycle == "Auto":
            if now[3] >= start12Zcycle:
    ##        if now[3] > 12 and now[3] - edton < 19:
                cycle = "12Z cycle"
            else:
                cycle = "00Z cycle"
                
        if varDict["Use Selected Time Range from the Grid Manager ?"] == "Y":

            self.p_timeHeader = time.strftime("%Z", time.localtime(time.time()))
            timeHeader = time.strftime("%Z", time.localtime(time.time()))

            present = AbsTime.current()

            timeHeader = self.p_timeHeader
            
            today = AbsTime.absTimeYMD(present.year, present.month, present.day)
            startTimeOffset = (timeRange.startTime() - today) / 3600
            endTimeOffset = (timeRange.endTime() - today) / 3600
##            print startTimeOffset, endTimeOffset
            SubtractADay = 0
##            if now[3] - edton > 23 - localStandardOffset:
##                SubtractADay = 24
            starth = startTimeOffset - SubtractADay
            endh = endTimeOffset - SubtractADay
##            print "ERQC: now[3], starth, endh:", now[3], starth, endh

        else:

            startPeriod = varDict["If not, Pick period to start with here:"]
            endPeriod = varDict["...and to end with here:"]

            startEnd = [("Today (00Z Cycle Only)", 12, 24), ("Tonight", 24, 36), ("Tomorrow", 36, 48), ("Tomorrow Night", 48, 60),
                        ("Next Day", 60, 72), ("Next Day Night", 72, 84), ("Day3 day", 84, 96), ("Day3 night", 96, 108),
                        ("Day4", 108, 132), ("Day5", 132, 156), ("Day6", 156, 180), ("Day7", 180, 205)
                        ]
            
            for i in xrange(len(startEnd)):
                                    
                period, startHour, endHour = startEnd[i]

                ################################################################################################
                #
                #  Now determine the time period chosen
            
                if period == startPeriod:
                    starth = startHour
                    h = starth
                if period == endPeriod:
                    endh = endHour
                        
####                    if starth < firsth:
####                        firsth = starth
####                    if endh > finalh:
####                        finalh = endh
                        
            ################################################################################################
            #
            #  Handle exceptions
            
            if endPeriod == "Day6" and cycle == "00Z cycle":
                endh = 181
                finalNight = 1

            if endPeriod == "Day7" or endPeriod == "Day8":
                finalNight = 1
                
            if now[3] - edton > 23 - localStandardOffset:
                starth -= 24
                if starth < 0:
                    starth = 0
                endh -= 24
                if endh < 0:
                    endh = 11
                    

            starth -= deltaStartDay
            endh -= deltaStartDay

        timeRange = self.createTimeRange(starth, endh, "Zulu")

        ########################################################################
        #
        #  What follows here is a little louie that involves calling other
        #  procedures, namely CheckTagainstMaxTandMinT and
        #  ForceTbetweenMaxTandMinT.  These procedures repeat the timeRange
        #  logic above, so they could be run independently.  But this means, for
        #  using the selected time range, where subtracting one from the end time
        #  is necessary to effect the proper time range, this hour needs added
        #  again before calling one of these procedures, to avoid the duplicate
        #  subtraction.
        
        timeRangePlusOneHr = self.createTimeRange(starth, endh + 1, "Zulu")

################################################################################
#
# Period    Today       Tonight     Tomorrow    Tomorrow    Next Day    Next Day
#                                               Night                   Night
#
# starth    12          24          36          48          60          72
# endH      23          35          47          59          71          83
#
# Period    Day4        Day5        Day6        Day7
#
# starth    84          108         132         156
# endH      107         131         155         181
#
################################################################################         

        #  Define variables for wind gust check:
##        varDict["Wind Gust QC:"] = ["Ensure gusts where >=10kts Sustained", "Zero out where minimal difference"]
##        varDict["Zero out wind gusts not in excess of sustained wind by more than:"] = GustLessSusMin
##        varDict["Minimum wind gust to report:"] = minGust

        # Define a list of elements for which to create grids if necessary
##        makeList = ['RH', 'HeatIndex', 'WindChill', 'PPI']
##        makeList = ['RH''PPI']
        makeList = []
        makeList.append(("RH"))
        makeList.append(("PPI"))
        curMon = nowZ[1]
        
        if curMon >= startWindChill or curMon <= endWindChill:
            makeList.append(('WindChill'))
        if curMon >= startHeatIndex and curMon <= endHeatIndex:
            makeList.append(('HeatIndex'))
        
        # Get list of SmartTools to run
        all = varDict["All (Overrides other choices if not No)"]
        if all == "Highlight only":
##            varDict["Which element group(s)?"] = ["Public", "Fire Weather", "Marine"],
            self.callProcedure("NDFDgridCheck",
                               timeRange=timeRange, varDict=varDict, editArea=QCarea)
            varDict["Check or Force:"] = "Check Only"
            self.callProcedure(TemperatureProc,
                               varDict=varDict, editArea=QCarea)
            self.callProcedure(WindGustProc,
                               varDict=varDict, editArea=QCarea)
##            ToolList.append(("WindGustQC", "WindGust"))
            ToolList.append(("CheckSkyWithPoP", "PoP"))
##            ToolList.append(("CheckPoPwithSky", "PoP"))
#            ToolList.append(("CheckPoPwithWx", "PoP"))
#            ToolList.append(("CheckWx", "Wx"))
#            ToolList.append(("CheckQPF", "QPF"))
#            ToolList.append(("CheckQPF", "QPF6hr"))
#            ToolList.append(("CheckQPF", "QPF12hr"))
#            ToolList.append(("CheckSnowAmt", "SnowAmt"))
#            ToolList.append(("CheckSnowAmt", "SnowAmt6hr"))
#            ToolList.append(("CheckSnowAmt", "SnowAmt12hr"))

        elif all == "Fix All":
##        elif all == "Fix All (Force Sky to PoP)" or all == "Fix All (Force PoP to Sky)":
##            varDict["Which element group(s)?"] = ["Public", "Fire Weather", "Marine"],
            self.callProcedure("NDFDgridCheck",
                               timeRange=timeRange, varDict=varDict, editArea=QCarea)
            varDict["Check or Force:"] = "Force: TMin<=T<=TMax\n and Td<=T"
            self.callProcedure(TemperatureProc,
                               varDict=varDict, editArea=QCarea)
            varDict["Check or Force:"] = "Force: WindGust>=Wind"
            self.callProcedure(WindGustProc,
                               varDict=varDict, editArea=QCarea)
##            ToolList.append(("WindGustQC", "WindGust"))
            ToolList.append(("EnufCloudForPoP", "Sky"))
##            if all == "Fix All (Force Sky to PoP)":
##                ToolList.append(("EnufCloudForPoP", "Sky"))
##            elif all == "Fix All (Force PoP to Sky)":
##                ToolList.append(("ForcePoPtoSky", "PoP"))
#            ToolList.append(("ForcePoPtoWx", "PoP"))
#            ToolList.append(("WxCovMatchPoP", "Wx"))
#            ToolList.append(("PoP12hrFmMaxPoP", "PoP12hr"))
#            ToolList.append(("NoPoPNoQPF", "QPF"))
#            ToolList.append(("QPF6hrFmQPFsum", "QPF6hr"))
#            ToolList.append(("QPF12hrFmQPFsum", "QPF12hr"))
#            ToolList.append(("NoPoPNoSnowAmt", "SnowAmt"))
##            self.createFromScratchCmd(["PPI"], timeRange, # (Also uncomment out Variablelist.append lines at top)
##                                  repeat=1, duration=1)
##            ToolList.append(("PPIfmPoP", "PPI"))    # For offices doing Precipitation Probability Index images for the web
#            ToolList.append(("SnowAmt6hrFmSnowAmt", "SnowAmt6hr"))
#            ToolList.append(("SnowAmt12hr", "SnowAmt12hr"))

        else:
            if varDict["NDFD Grid Check (Checks all elements all 7 days)"] == "Yes":
##                varDict["Which element group(s)?"] = ["Public", "Fire Weather", "Marine"],
                self.callProcedure("NDFDgridCheck",
                                   timeRange=timeRange, varDict=varDict, editArea=QCarea)
            if varDict["Temperatures"] == "Highlight only":
                varDict["Check or Force:"] = "Check Only"
                self.callProcedure(TemperatureProc,
                                   varDict=varDict, editArea=QCarea)
            elif varDict["Temperatures"] == "Fix":
                varDict["Check or Force:"] = "Force: TMin<=T<=TMax\n and Td<=T"
                self.callProcedure(TemperatureProc,
                                   varDict=varDict, editArea=QCarea)
            if varDict["Wind Gusts"] == "Highlight only":
                varDict["Check or Force:"] = "Check Only"
                self.callProcedure(WindGustProc,
                                   varDict=varDict, editArea=QCarea)
            elif varDict["Wind Gusts"] == "Fix":
                varDict["Check or Force:"] = "Force: WindGust>=Wind"
                self.callProcedure(WindGustProc,
                                   varDict=varDict, editArea=QCarea)
##                ToolList.append(("WindGustQC", "WindGust"))
                
            if varDict["CheckSkyWithPoP"] == "Highlight only":
                ToolList.append(("CheckSkyWithPoP", "PoP"))
            elif varDict["CheckSkyWithPoP"] == "Fix":
                ToolList.append(("EnufCloudForPoP", "Sky"))

##            if varDict["CheckPoPwithSky"] == "Highlight only":
##                ToolList.append(("CheckPoPwithSky", "PoP"))
##            elif varDict["CheckPoPwithSky"] == "Fix":
##                ToolList.append(("ForcePoPtoSky", "PoP"))
##                ToolList.append(("PoP12hrFmMaxPoP", "PoP12hr"))

##            if varDict["CheckPoPwithWx"] == "Highlight only":
##                ToolList.append(("CheckPoPwithWx", "PoP"))
###                ToolList.append(("CheckPoP", "PoP12hr"))
##            elif varDict["CheckPoPwithWx"] == "Fix":
##                ToolList.append(("ForcePoPtoWx", "PoP"))
###                ToolList.append(("PoP12hrFmMaxPoP", "PoP12hr"))
##
##            if varDict["CheckWxWithPoP"] == "Highlight only":
##                ToolList.append(("CheckWx", "Wx"))
##
##            if varDict["CheckWxWithPoP"] == "Fix":
##                ToolList.append(("WxCovMatchPoP", "Wx"))
##
##            if varDict["NoPoPNoQPF"] == "Highlight only":
##                ToolList.append(("CheckQPF", "QPF"))
###                ToolList.append(("CheckQPF", "QPF6hr"))
###                ToolList.append(("CheckQPF", "QPF12hr"))
##            elif varDict["NoPoPNoQPF"] == "Fix":
##                ToolList.append(("NoPoPNoQPF", "QPF"))
###                ToolList.append(("QPF6hrFmQPFsum", "QPF6hr"))
###                ToolList.append(("QPF12hrFmQPFsum", "QPF12hr"))
##            if varDict["NoPoPNoSnowAmt"] == "Highlight only":
##                ToolList.append(("CheckSnowAmt", "SnowAmt"))
###                ToolList.append(("CheckSnowAmt", "SnowAmt6hr"))
###                ToolList.append(("CheckSnowAmt", "SnowAmt12hr"))
##            elif varDict["NoPoPNoSnowAmt"] == "Fix":
##                ToolList.append(("NoPoPNoSnowAmt", "SnowAmt"))
##            if varDict["Run PPI"] == "Yes":    # For offices doing Precipitation Probability Index images for the web
##                self.createFromScratchCmd(["PPI"], timeRange, # (Also uncomment out Variablelist.append lines at top)
##                                      repeat=1, duration=1)
##                ToolList.append(("PPIfmPoP", "PPI"))

#            elif varDict["NoPoPNoSnowAmt"] == "Re-run from QPF":
#                ToolList.append(("SnowDog", "SnowAmt"))
#                ToolList.append(("SnowAmt6hrFmSnowAmt", "SnowAmt6hr"))
#                ToolList.append(("SnowAmt12hr", "SnowAmt12hr"))
                
        QPFSnowWxPoPCheck = []
        if 'Yes' in varDict['Run SnowAmt/QPF Check?']:
            QPFSnowWxPoPCheck.append((1))
        if 'Yes' in varDict['Run SnowAmt/Wx Check?']:
            QPFSnowWxPoPCheck.append((1))
        if 'Yes' in varDict['Run QPF/PoP Check?']:
            QPFSnowWxPoPCheck.append((1))
        if 'Yes' in varDict['Run QPF/Wx Check?']:
            QPFSnowWxPoPCheck.append((1))
#        print "QPFSnowWxPoPCheck and its length :",  QPFSnowWxPoPCheck, len(QPFSnowWxPoPCheck)
        if len(QPFSnowWxPoPCheck) > 0:
            self.callProcedure("SnowAmtQPFPoPWxCheck",
                           varDict=varDict, editArea=QCarea, timeRange=timeRange)

        # For each SmartTool in the list
        for toolName, elementName in ToolList:

            # Send a message to the status bar
            self.statusBarMsg('ER_QC_Check running -> %s' % (toolName), 'R')

            # If this element is in the 'make grids' list
            if elementName in makeList:
##        makeList = ['RH', 'HeatIndex', 'WindChill', 'PPI']  ## defined above
                
                # Ensure we have grids for this element
                self.createFromScratchCmd([elementName], timeRange,
                                          repeat=1, duration=1)
                ## these are all hourly so can create hourly and obviate
                ## the need for the fragment call below.

##                # Fragment these fields
##                self.fragmentCmd([elementName], timeRange)

                editArea = Allareas
        
                # Execute this SmartTool
                error = self.callSmartTool(toolName, elementName,
                                  editArea, timeRange, varDict,
                                  missingDataMode="Create")

            else:
                editArea = QCarea
                # Execute this SmartTool
                if toolName == "WxCovMatchPoP":
                    error = self.callSmartTool(toolName, elementName,
                                      editArea, timeRange,
                                      missingDataMode="Create")
                else:
                    error = self.callSmartTool(toolName, elementName,
                                      editArea, timeRange, varDict,
                                      missingDataMode="Create")
            
            if error is not None:
                break
            

        # Send a message to the status bar
        self.statusBarMsg('ER_QC_Check completed!', 'R')

        
        


    def logProcUse(self, string):
        gtime = time.gmtime()
        ts = "%4.4d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d" % (gtime[0], gtime[1], gtime[2],
                                                  gtime[3], gtime[4], gtime[5])
        LogStream.logEvent("%s| %s" % (ts, string))
