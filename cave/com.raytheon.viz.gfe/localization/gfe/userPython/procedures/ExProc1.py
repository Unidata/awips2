##
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
