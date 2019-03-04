##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ExProc2
#
# Author:
# ----------------------------------------------------------------------------

MenuItems = ["Verify"]

import SmartScript
VariableList = [("Model:" , "", "model")]

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        
    def execute(self, editArea, varDict):
        """Copy from model, create grids, run smart tool."""

        model = varDict["Model:"]
        databaseID = self.getDatabase(model)
        timeRange = self.createTimeRange(12, 96, "Database", databaseID)
        elements = ['T', 'Wind','Wx']
        self.copyCmd(elements, databaseID, timeRange)
        self.createFromScratchCmd(['T'], timeRange, repeat=3, duration=1)
        self.callSmartTool("ExSS4","T", editArea, timeRange, #varDict,
               missingDataMode="Create")
