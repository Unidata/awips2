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
# CheckTTdWind
#
# Author: Tom LeFebvre
#
# Version Date: 4 January 2006
# Version: 6.5
#
# 8/26/2015    yteng    Call CheckTandTd and CheckWindGust for processing
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify
MenuItems = ["Consistency"]

VariableList = [("Check or Force:" , "Check Only", "radio",
                 ["Check Only", "Force: TMin<=T<=TMax\n Td<=T\nWind<=WindGust"]),
                ]

import SmartScript


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
        if timeRange is None:
            yesterday = self._gmtime() - (2 * DAY_IN_SECS) # two days ago
            later = self._gmtime() + 10 * DAY_IN_SECS  # 10 days from now
            timeRange = TimeRange.TimeRange(yesterday, later)
        if isinstance(timeRange, JavaWrapperClass):
            timeRange = timeRange.toJavaObj()

        self.callProcedure("CheckTandTd", timeRange=timeRange, varDict=varDict)
        self.callProcedure("CheckWindGust", timeRange=timeRange, varDict=varDict)
