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
# ExProc1
#
# Author:
# ----------------------------------------------------------------------------

MenuItems = ["Edit"]
ToolList = [
##            ("AdjustValue_Up", "variableElement"),
##            ("Smooth", "variableElement"),
            ("AdjustValue_Up", "T"),
            ("Smooth", "T"),
          ]
import SmartScript
class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
    def execute(self, editArea, timeRange, varDict):
        # Calls each Smart Tool: T_Tool, PoP_Tool, Wind_Tool
        for toolName, elementName in ToolList:
              error = self.callSmartTool(toolName, elementName,
                              editArea, timeRange, varDict)
              if error is not None:
                  break
