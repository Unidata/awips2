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
# ProcedureCmds -- test and example procedure commands
#
# Author: hansen
# ----------------------------------------------------------------------------

import MyDialog
import SmartScript

MenuItems = ["Populate"]

VariableList = [
         ("Model" , "", "model"),
         ("Model Elements" , ["All"], "check", ["All", "Wx", "T", "Td", "Wind",
                                                "MaxT", "MinT", "Sky", "PoP",
                                                "QPF"]),
         ("Begin Hour" , 0, "scale", [0, 120]),
         ("Initialize From Model", "", "D2D_model"),
         ("", 600, "scrollbar"),
          ]

AllElements =['T','Td','MaxT','MinT','Wind','Sky','Wx','PoP','QPF']

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
    def execute(self, editArea, timeRange, varDict):

        # Put up In Progress dialog
        dialog = MyDialog.MyDialog(None,"   Status ","   Loading Grids   ")

        # Get Variables
        model = varDict["Model"]
        # getDatabase -- convert user input to databaseID
        databaseID = self.getDatabase(model)
        elements = varDict["Model Elements"]
        if "All" in elements:
            elements = AllElements
        beginHour = varDict["Begin Hour"]

        # createTimeRange -- Create time ranges relative to chosen model
        timeRange_120_240 = self.createTimeRange(
            120, 240, "Database", databaseID)
        timeRange_begin_60 = self.createTimeRange(
            beginHour, 60, "Database", databaseID)
        timeRange_begin_24 = self.createTimeRange(
            beginHour, 24, "Database", databaseID)
        # findDatabase -- Find databaseID for gfsLR model
        gfsLR_databaseID = self.findDatabase("gfsLR")

        # copy Commands
        print "Copy commands"
        copyTimeRange = self.getTimeRange(beginHour, 120, 'gfsLR')
        self.copyCmd(elements, gfsLR_databaseID, copyTimeRange)
        self.copyCmd(elements, gfsLR_databaseID, timeRange_120_240)
        self.copyCmd(elements, databaseID, timeRange_begin_60)

        # createFromScratch
        # Create 1-hour grids repeating every 6 hours over the entire time range
        print "Create From Scratch"
        createTimeRange = self.getTimeRange(3, 4, 'gfsLR')
        self.createFromScratchCmd(['MixHgt'], createTimeRange)

        # split
        print "Split"
        splitTimeRange = self.getTimeRange(beginHour, 48, 'gfsLR')
        self.splitCmd(['MixHgt'], splitTimeRange)
        self.splitCmd(['MixHgt'], timeRange_begin_60)

        # fragment
        print "Fragment"
        fragmentTimeRange = self.getTimeRange(beginHour, 48, 'gfsLR')
        self.fragmentCmd(['MixHgt'], fragmentTimeRange)
        self.fragmentCmd(['MixHgt'], timeRange_begin_60)

        # remove
        print "Remove"
        removeTimeRange = self.getTimeRange(beginHour, 48, 'gfsLR')
        self.deleteCmd(['MixHgt'], removeTimeRange)
        print "CreateFromScratchCmd"
        self.createFromScratchCmd(
            ['MixHgt'], timeRange_begin_60, repeat=6, duration=1)
        # deleteCmd
        print "Delete"
        self.deleteCmd(['MixHgt'], timeRange_begin_60)
        self.createFromScratchCmd(
            ['MixHgt'], timeRange_begin_60, repeat=6, duration=1)

        # zero
        print "Zero"
        zeroTimeRange = self.getTimeRange(beginHour, 6, 'gfsLR')
        self.zeroCmd(['MixHgt'], zeroTimeRange)
        self.zeroCmd(['MixHgt'], timeRange_begin_60)

        # assignValue
        print "AssignValue"
        assignTimeRange = self.getTimeRange(beginHour, 6, 'gfsLR')
        self.assignValueCmd(['MixHgt'], assignTimeRange, 2000)
        self.assignValueCmd(['MixHgt'], timeRange_begin_60, 3000)

        # getEditArea
        toolEditArea = self.getEditArea("ISC_Send_Area")
        
        # callSmartTool -- Run Smart Tool using created time range
        print "callSmartTool"
        varDict["Initialize From Model: "] = varDict["Initialize From Model"]
        self.callSmartTool("MixHgt_Init","MixHgt",
                toolEditArea, timeRange_begin_24, varDict)

        # timeShift
        print "TimeShift"
        shiftTimeRange = self.getTimeRange(beginHour, 24, 'gfsLR')
        self.timeShiftCmd(['T', 'Wind'], 1, 3, shiftTimeRange)
        self.timeShiftCmd(['T', 'Wind'], 1, 3, timeRange_begin_24)
        
        # interpolate
        print "Interpolate"
        interpolateTimeRange = self.getTimeRange(beginHour, 6, 'gfsLR')
        self.interpolateCmd(elements, interpolateTimeRange, "GAPS", "SYNC", 0, 0)
        self.interpolateCmd(elements, timeRange_begin_24, "GAPS", "ASYNC", 0, 0)
        
        # 
        # Destroy In Progress dialog
        dialog.destroy()

    def getTimeRange(self, hourStart, hourEnd, modelBase):
        databaseID = self.findDatabase(modelBase)
        timeRange = self.createTimeRange(hourStart, hourEnd, "Database", databaseID)
        return timeRange
        
        