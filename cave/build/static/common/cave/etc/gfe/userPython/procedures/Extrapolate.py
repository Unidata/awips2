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
# Extrapolate
#
# Author:                  Thomas R. Mazza
#                          adapted from EditAreaAdjust tool written by
#                          Les Colin
# Additional Contribution: Todd Lericos
# Last Updated:            Tue 10 Jun 8
# last Submitted to str:   Tue 10 Jun 8
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Edit"]
import LogStream, time
from math import *

# The ToolList is optional, but recommended, if you are calling
# Smart Tools from your Script.
# If present, it can be used to show which grids will be
# modified by the Script.

##ToolList = [("T_Tool", "T"),
##            ("PoP_Tool", "PoP"),
##            ("Wind_Tool", "Wind"),
##          ]

#######   CONFIGURATION SECTION    #########################################################################
#
#  Add or delete models according to whether or not they are available at your office.
#

#sourceList = ["NAM12", "GFS40", "RUC40"]
sourceList = ["NAM12", "GFS40", "RUC13", "wrfnmm", "wrfarw", "WSETA"]
toolName = 'Extrapolate'

#
#######   END CONFIGURATION SECTION    #####################################################################

sourceList.append(("Fcst"))
sourceList.append(("Observed (enter below)"))


### If desired, Set up variables to be solicited from the user:
##  If your script calls Smart Tools, this VariableList should cover
##  cover all the variables necessary for the tools.

VariableList = []
VariableList.append(("Extrapolate:", "Forward in Time", "radio", ["Forward in Time", "Backward in Time"]))
VariableList.append(("Source:", "Observed (enter below)", "radio", sourceList))
VariableList.append(("Wind Level if using model:","MB700","radio",["MB925","MB850","MB700","MB500", "MB925-850", "MB850-700", "MB925-700", "MB925-500", "MB850-500", "MB700-500"]))
VariableList.append(("Movement Speed (Kts):", "15", "numeric"))
VariableList.append(("Movement Direction:" , "90", "numeric"))
VariableList.append(("Backfill upstream edges with:", "Original data", "radio", ["Original data", "Data from very edge\n(Fcst or Model only)", "Zeros"]))

import time
import AbsTime
import SmartScript
## For documentation on the available commands,
##   see the SmartScript Utility, which can be viewed from
##   the Edit Actions Dialog Utilities window

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, editArea, timeRange, varDict):
        # Calls each Smart Tool: T_Tool, PoP_Tool, Wind_Tool
        self.logProcUse("Procedure execution")

        ################################################################################################
        #
        #  Use selected time range

        self.p_timeHeader = time.strftime("%Z",time.localtime(time.time()))
        timeHeader = time.strftime("%Z",time.localtime(time.time()))

        present = AbsTime.current()

        timeHeader = self.p_timeHeader
        
        today = AbsTime.absTimeYMD(present.year, present.month, present.day)
        startTimeOffset = (timeRange.startTime() - today) / 3600
        endTimeOffset = (timeRange.endTime() - today) / 3600
        starth = startTimeOffset
        endh = endTimeOffset

        validConstraints = 0
        gridInfoList = self.getGridInfo("Fcst", "variableElement", "SFC", timeRange)
        for g in gridInfoList:

            tCon = g.tc().getRepeatInterval()
            if tCon > 3600:
                validConstraints = 1

        if validConstraints == 0:
            source = varDict["Source:"]
            missingHours = 0

            timeRange = self.createTimeRange(starth, endh, "Zulu")
            self.fragmentCmd(["variableElement"], timeRange)
            h=starth

            modelList = ["NAM12", "GFS40", "RUC13", "wrfnmm", "wrfarw", "WSETA", "Fcst"]   #####################################################

            if varDict["Source:"] not in modelList and varDict["Source:"] != "Observed (enter below)":
                modelList.append((varDict["Source:"]))
                
            if varDict["Extrapolate:"] == "Forward in Time":
        
                while h < endh - 1:

                    h += 1
                    timeRange = self.createTimeRange(h, h+1, "Zulu")
                    tr = self.createTimeRange(h-1, h, "Zulu")
                    self.timeShiftCmd(["variableElement"], 1, 1, tr)
                    self.callSmartTool("MoveFeatureBySpeed", "variableElement",
                                        editArea, timeRange, varDict)
                    
            else:
                if varDict["Source:"] in modelList:     
                    dir = -90
                else:
                    dir = varDict["Movement Direction:"]
                    dir += 180
                    if dir > 360:
                        dir -= 360
                        
                varDict["Movement Direction:"] = dir
                h = endh
                while h > starth + 1:
                    timeRange = self.createTimeRange(h-2, h-1, "Zulu")

                    tr = self.createTimeRange(h-1, h, "Zulu")
                    self.timeShiftCmd(["variableElement"], 1, -1, tr)
                    self.callSmartTool("MoveFeatureBySpeed", "variableElement",
                                        editArea, timeRange, varDict)
                    h -= 1
        else:
            LogStream.logProblem(toolName, ': repeating intervals must be of 1 hour')
            return None
                      # leaves current WindGust grid alone
    def logProcUse(self,string):
        gtime=time.gmtime()
        ts="%4.4d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d"%(gtime[0],gtime[1],gtime[2],
                                                  gtime[3],gtime[4],gtime[5])
        LogStream.logEvent("%s| %s" % (ts,string))
