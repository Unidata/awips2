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
# ISC_Discrepancies
#
#  This procedure runs the Show_ISC_Highlights tool on a set of weather elements.
#  The set-up dialog allows the user to choose:
#
#    A set of weather elements for which to run the procedure
#    A time range -- time range over which to run the procedure
#
#   It uses the ISC_Utility and ISC_Utility_Local for setting up
#    the algorithm, edit area, and thresholds.
#
# Author: hansen
# ----------------------------------------------------------------------------
MenuItems = ["Consistency"]

import ISC_Utility_Local
import SmartScript
import time
import ProcessVariableList

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        self._dbss = dbss
        SmartScript.SmartScript.__init__(self, self._dbss)
        
    def execute(self, editArea, timeRange, varDict):
        # Calls the Show_ISC_Highlights for each element

        #self._utility = ISC_Utility_Local.ISC_Utility_Local(
        #    self._dbss, self.eaMgr())
        self._utility = ISC_Utility_Local.ISC_Utility_Local(
            self._dbss, None)
        
        # Put up VariableList dialog
        variableList = []
        # Determine Elements
        elementList = self._utility._getElementList()
        variableList.append(("Elements" , elementList, "check", elementList))
        # Determine Time Ranges
        trList = self._utility._getTimeRangeList()
        variableList.append(("Time Range", "All Grids", "radio", trList))
        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList(
            "ISC_Discrepancies", variableList, varDict,
            None) 
        status = processVarList.status()
        if status != "OK": 
            return

        # Determine Elements and Time Range
        elements = processVarList.varDict()["Elements"]
        trName = processVarList.varDict()["Time Range"]
        timeRange = self._utility._convertTimeRange(trName)
        area = None

        # Run Smart Tool for each Element over Time Range
        time1 = time.time()
        for elementName in elements:
            if elementName == "Wx":
                print "Not yet able to identify discrepancies for Wx"
                continue
            print "Running Discrepancies for ", elementName
            time2 = time.time()
            if len(self.getParm("Fcst", elementName, "SFC").getGridInventory(timeRange.toJavaObj())) > 0:
                error = self.callSmartTool("Show_ISC_Highlights", elementName,
                                  area, timeRange, varDict, missingDataMode="Skip")
                print "  Time :", time.time() - time2
                if error is not None:
                    break

        print "Total Time ", time.time() - time1

