##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
# 
# QPF_SnowAmt.py
#
# Author: hansen
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

MenuItems = ["Edit"]

ToolList = [
    ("QPF_SmartTool", "QPF"),
    ("SnowAmt_SmartTool", "SnowAmt"),
    ]
VariableList = [
         ("Vertical Motion Influence" , 50, "scale", [0,100]), 
        ]

import SmartScript

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, editArea, timeRange, varDict):
        # Calls each Smart Tool

        for toolName, elementName in ToolList:
           error = self.callSmartTool(toolName, elementName,
                              editArea, timeRange, varDict)
           if error is not None:
               break

