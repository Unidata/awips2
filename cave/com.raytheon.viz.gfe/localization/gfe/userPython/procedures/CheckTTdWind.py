##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CheckTTdWind
#
# Author: Tom LeFebvre
#
# Version Date: 4 January 2006
# Version: 6.5
#
# 8/28/2015    yteng    Call CheckTandTd and CheckWindGust for processing 
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify
MenuItems = ["Consistency"]

VariableList = [("Check or Force:" , "Check Only", "radio",
                 ["Check Only", "Force: TMin<=T<=TMax\n Td<=T\nWind<=WindGust"]),
                ]

import SmartScript
import TimeRange

DAY_IN_SECS = 24 * 3600


class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    ##
    # Main entry point of this procedure. If varDict["Check or Force"] is 
    # "Check Only", temporary grids will be created. Otherwise, the minT, maxT,
    # T, and Td grids may be changed.
    # @param varDict: Determines whether temporary grids are created or 
    # temperature grids are modified.
    # @type varDict: Python dictionary of strings to strings 
    def execute(self, timeRange, varDict):
        if timeRange is None or not timeRange.isValid():
            start = self._gmtime() - (2 * DAY_IN_SECS) # two days ago
            end = self._gmtime() + 10 * DAY_IN_SECS  # 10 days from now
            timeRange = TimeRange.TimeRange(start, end)

        self.callProcedure("CheckTandTd", timeRange=timeRange, varDict=varDict)
        self.callProcedure("CheckWindGust", timeRange=timeRange, varDict=varDict)


