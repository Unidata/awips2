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
# Populate_SkyProcedure
#
# Author: Pete Banacos (began: 12/12/06)
# Last Updated: 10/07/08
#
# This program makes use of the Populate_SkyTool to populate sky grids.
# 10/7/08: Update to allow flexible time range (start and end time doesn't
#          need to correspond to model available times)
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify
MenuItems = ["Populate"]

# The ToolList is optional, but recommended, if you are calling
# Smart Tools from your Script.
# If present, it can be used to show which grids will be
# modified by the Script.

ToolList = [("Populate_SkyTool", "Sky"),
          ]
VariableList = [("Populate SkyProcedure Version 1.0","","label"),
    ("Note:" , "Before running, highlight desired time range in the Grid Manager.", "alphaNumeric"),           
    ("Model:", "NAM12", "radio", ["GFS40", "NAM12"]),
    ("Model Run:", "Current", "radio", ["Current", "Previous"]),
    ("Layer depth:", "50mb", "radio", ["50mb", "25mb"]),
    ("Use RH w.r.t. ICE @ T < -25C?", "No", "radio", ["Yes", "No"]), 
    ("", "", "label"),
    ("Include high clouds (500-300mb)?", "No", "radio", ["Yes", "No"]),
    ("Include clouds below 925mb?", "Yes", "radio", ["Yes", "No"]),                                               
    ("5% Sky Cover threshold at RH percentage:", 60., "scale", [44., 74.],2.0),
    ("Above value sets RH threshold for CLR skies.", "", "label"),    
    ("Calibration:", 1.00, "scale", [1.00, 1.50],0.02),
    ("Raise calibration to get more sky cover for a given RH.", "", "label"),    
    (" ---   Limit Values Section   --- ", "", "label"),
    ("Don't give me sky cover above (percent):", 100, "scale", [0, 100], 1),
    ("Don't give me sky cover below (percent):", 0, "scale", [0, 100], 1),
    ]

import SmartScript
import time
from Tkinter import *

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, editArea, timeRange, varDict):

        databaseID = self.findDatabase("Fcst")
        model1 = varDict["Model:"]
        modelrun = varDict["Model Run:"]
        modeltemp = "D2D_" + model1
        
        if modelrun == "Current":
            model = self.findDatabase(modeltemp, 0)
        else:
            model = self.findDatabase(modeltemp, -1)
                
        time1 = time.gmtime()
        (year,month,day,h,m,s,w,day,dst)=time1
        shift = (h*3600) + (m*60) + s
        starth = timeRange.startTime().unixTime()
        endh = timeRange.endTime().unixTime()
        now = int(time.time())
        zero = now - shift
        starttime = (starth-zero)/3600
        endtime = (endh-zero)/3600

        # print timeRange
        # print "start_hr:", starttime, "end_hr:", endtime
        
        if model1 == "NAM12":
            self.deleteCmd(['Sky'], timeRange)
            for i in xrange(starttime,endtime):
                curmod = i % 3
                if curmod == 0:
                    # print "make grid for hour", i 
                    Sky_timeRange = self.createTimeRange(i,i+1,"Zulu")
                    # print "Sky_TimeRange:", Sky_timeRange
                    self.createFromScratchCmd(['Sky'], Sky_timeRange, 1, 1)
                    self.callSmartTool("Populate_SkyTool",'Sky', timeRange=Sky_timeRange, varDict=varDict)
        else:
            # Use "GFS40"...
            self.deleteCmd(['Sky'], timeRange)
            for i in xrange(starttime,endtime):
                curmod = i % 6
                if curmod == 0:
                    # print "make grid for hour", i 
                    Sky_timeRange = self.createTimeRange(i,i+1,"Zulu")
                    # print "Sky_TimeRange:", Sky_timeRange
                    self.createFromScratchCmd(['Sky'], Sky_timeRange, 1, 1)
                    self.callSmartTool("Populate_SkyTool",'Sky', timeRange=Sky_timeRange, varDict=varDict)

        # Interpolate gaps. Use fcst database either side of time range...
        self.interpolateCmd(['Sky'], timeRange, "GAPS", "SYNC")
