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
# Collaborate_PoP_SnowAmt_QPF - Version 3.0
#
# Description: Calls smart tools to generate PoP12hr, SnowAmt6hr, and QPF6hr
#              grids from the "floating" PoP, SnowAmt, and QPF grids. These
#              grids can be used for intersite coordination purposes, as well
#              as the generation of graphics for the internet. 
#              
# Author: John Rozbicki (IFPS FP), NWS Buffalo, NY
#
# Changes for Version 3.0 (August 2003):
#       * Changed to create collaboration grids using time constraints that
#         are based on GMT instead of LT. 
#       * Modified the samplelocalConfig.py file to reflect the new time
#         constraints for the collaboration grids.
#       * Modified the procedure code to use GMT instead of LT when
#         determining the current time and the initial time ranges for the
#         collaboration grids.
#       * Added code to allow a site to specify a default edit area over
#         which to run the tool.
#
# Changes/Enhancements for Version 2.1 (July 2003):
#       * Changed to use version 2.0 of Paul Jendrowski's MakeTmpGrid smart
#         tool (allows procedure to run correctly in IFPS 14).
#       * Corrected samplelocalConfig.py file to indicate correct ISC time
#         constraints.
#       *** No Changes were made to the procedure code in this file with
#           Version 2.1. ***
#
# Enhancements for Version 2.0 (March 2003):
#       (courtesy Paul Jendrowski, NWS Blacksburg, VA 2/25/03)
#       * Changed to use generic smart tools.
#       * Changed to to do better checking for existing grids and time ranges.
#       * Changed so the procedure now automatically deletes the temporary
#         grids needed to sum up SnowAmt and QPF into collaboration grids.
#
# Date Last Modified: 08/10/03
# Purpose: Modify to use GMT instead of LT when determining the current time
#          and the initial time ranges for the collaboration grids
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify
MenuItems = ["Consistency"]

# If desired, Set up variables to be solicited from the user:
VariableList = [
         ("Select Time Range:","All", "radio",
          ["Selected Time", "All"]),
        ]

# import time module as well as SmartScript
import time
import SmartScript
import TimeRange

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, editArea, timeRange, varDict):
        # Calls each Smart Tool in ToolList

        # The following variable controls whether the tool uses a pre-defined edit area.
        # Set to "yes" to use a pre-defined area; "no" otherwise. Use all lowercase letters
        # when typing "yes" or "no".
        # Default Setting: "no"
        UseDefaultEditArea = "no"
        
        # The following line specifies the pre-defined edit area used by the tool (if applicable)
        # To use such an area, uncomment the line, and substitute the name of your edit area for
        # "editareaname".
        # Default Setting: Not used (Commented out)
        #DefaultEditArea = self.getEditArea("editareaname")

        # Get time range from varDict
        period = varDict["Select Time Range:"]

        # Get current Greenwich Mean Time (GMT)
        GMT = time.gmtime(time.time())

        # Get current hour from GMT
        gmthour = GMT[3]
        #print "GMT hour=", gmthour

        # Create the timeranges over which to create the grids from scratch
        # (timerange varies by element and initial period)
        if period == "Selected Time":
            # Set time range of grids that are actually there
            PoPtimeRange = self._getGridTimeRange("Fcst", "PoP", "SFC", timeRange)
            SnowtimeRange = self._getGridTimeRange("Fcst", "SnowAmt", "SFC", timeRange)
            QPFtimeRange = self._getGridTimeRange("Fcst", "QPF", "SFC", timeRange)
#            print "Selected tr=",timeRange
        else:
# 
# Here is where you can adjust the ending times of the preset time ranges used by the
# "All" option. To change these, simply modify the ending time of the following calls
# to createTimeRange to meet your local needs.
# 
            # First set a reasonable time range then determine which grids are
            # actually there. The start time is the current hour in local time.
            tr = self.createTimeRange(gmthour, 192, "Zulu")
            PoPtimeRange = self._getGridTimeRange("Fcst", "PoP", "SFC", tr)

            tr = self.createTimeRange(gmthour, 60, "Zulu")
            SnowtimeRange = self._getGridTimeRange("Fcst", "SnowAmt", "SFC", tr)

            tr = self.createTimeRange(gmthour, 96, "Zulu")
            QPFtimeRange = self._getGridTimeRange("Fcst", "QPF", "SFC", tr)

        # Next, check to make sure the PoP, QPF, and SnowAmt grids exist
        pop = 1
        qpf = 1
        snow = 1
        if PoPtimeRange.duration() < 3600:
            pop = 0
        if SnowtimeRange.duration() < 3600:
            snow = 0
        if QPFtimeRange.duration() < 3600:
            qpf = 0
        varDict["Model"] = "Fcst"
        
        # Now, create the PoP12hr, SnowAmt6hr, and QPF6hr grids from scratch.
        # This is done first since the time constraints for these grids may cause GFE to
        # create grids with a timeRange outside of the actual PoP, SnowAmt, and QPF grids.
        # Then we redefine the time ranges to correspond to just those newly created grids
        # that actually intersect the source grids. This step is needed so that the
        # temporary SnowAmt and QPF grids (tmpSnowAmt and tmpQPF) are fragmented correctly.
        #
        # NOTE: During this step, the createFromScratchCmd repeat and duration options are
        # set to 1 so we don't have to redefine the time ranges to start on integral times
        # of the various time constraints. This will force GFE to create grids from scratch
        # using the native time constraints of the PoP12hr, SnowAmt6hr, and QPF6hr elements
        # (which are defined in your localConfig.py file).
        if pop == 1:
            self.createFromScratchCmd(["PoP12hr"], PoPtimeRange, 1, 1)
            varDict["Element"] = "PoP"
            if UseDefaultEditArea == "yes":
                self.callSmartTool("getMaxGrid", "PoP12hr", varDict=varDict,
                              timeRange=PoPtimeRange, editArea=DefaultEditArea)
            else:
                self.callSmartTool("getMaxGrid", "PoP12hr", varDict=varDict,
                              timeRange=PoPtimeRange)
        if snow == 1:
#            print "create snow 1=",SnowtimeRange
            self.createFromScratchCmd(["SnowAmt6hr"], SnowtimeRange, 1, 1)
            scratchTR = self._getGridTimeRange("Fcst", "SnowAmt6hr", "SFC", SnowtimeRange)
            # Now we have to make sure to get all source grids that intersect the scratch grids
            SnowtimeRange = self._getGridTimeRange("Fcst", "SnowAmt", "SFC", scratchTR)
#            print "scratch snow tr=",SnowtimeRange
            # Make the tempSnowAmt grids and fragment them
            if UseDefaultEditArea == "yes":
                self.callSmartTool("MakeTmpGrid", "SnowAmt", varDict=varDict,
                              timeRange=SnowtimeRange, editArea=DefaultEditArea)
            else:
                self.callSmartTool("MakeTmpGrid", "SnowAmt", varDict=varDict,
                              timeRange=SnowtimeRange)
            self.fragmentCmd(["tmpSnowAmt"], SnowtimeRange)
            # Sum the values in the tempSnowAmt grids to create the SnowAmt6hr grids
            varDict["Element"] = "tmpSnowAmt"
            if UseDefaultEditArea == "yes":
                self.callSmartTool("getSumGrids", "SnowAmt6hr", varDict=varDict,
                               timeRange=scratchTR, editArea=DefaultEditArea)
            else:
                self.callSmartTool("getSumGrids", "SnowAmt6hr", varDict=varDict,
                               timeRange=scratchTR)
            # Delete the tempSnowAmt grids
            try:
                # self.deleteObject("tmpSnowAmt", "FcstGrid")
                self.unloadWE("Fcst", "tmpSnowAmt", "SFC")
            except:
                pass
        if qpf == 1:
#            print "create qpf 1=",QPFtimeRange
            self.createFromScratchCmd(["QPF6hr"], QPFtimeRange, 1, 1)
            scratchTR = self._getGridTimeRange("Fcst", "QPF6hr", "SFC", QPFtimeRange)
            # Now we have to make sure to get all source grids that intersect the scratch grids
            QPFtimeRange = self._getGridTimeRange("Fcst", "QPF", "SFC", scratchTR)
            # Make the tempQPF grids and fragment them
            if UseDefaultEditArea == "yes":
                self.callSmartTool("MakeTmpGrid", "QPF", varDict=varDict,
                               timeRange=QPFtimeRange, editArea=DefaultEditArea)
            else:
                self.callSmartTool("MakeTmpGrid", "QPF", varDict=varDict,
                               timeRange=QPFtimeRange)
            self.fragmentCmd(["tmpQPF"], QPFtimeRange)
            # Sum the values in the tempQPF grids to create the QPF6hr grids
            varDict["Element"] = "tmpQPF"
            if UseDefaultEditArea == "yes":
                self.callSmartTool("getSumGrids", "QPF6hr", varDict=varDict,
                               timeRange=scratchTR, editArea=DefaultEditArea)
            else:
                self.callSmartTool("getSumGrids", "QPF6hr", varDict=varDict,
                               timeRange=scratchTR)
            # Delete the tempQPF grids
            try:
                # self.deleteObject("tmpQPF", "FcstGrid")
                self.unloadWE("Fcst", "tmpQPF", "SFC")
            except:
                pass


    def _getGridTimeRange(self, model,parm,level,timeRange):
    # Returns a timeRange covering any grids that intersect input timeRange
    # Returns a timeRange with duration less than 3600 if no grids found
        info = self.getGridInfo(model, parm, level, timeRange)
        if info != []:
            st = info[0].gridTime().startTime()
            et = info[len(info) - 1].gridTime().endTime()
            tr = TimeRange.TimeRange(st,et)
        else:
            tr = TimeRange.TimeRange(timeRange.startTime(),timeRange.startTime() + 1)
        return tr
