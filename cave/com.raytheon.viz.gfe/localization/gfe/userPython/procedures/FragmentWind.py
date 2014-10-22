# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# FragmentWind
#
# Author:
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Populate"]

import SmartScript
import time, os, AbsTime, TimeRange
import numpy.oldnumeric

## For documentation on the available commands,
##   see the SmartScript Utility, which can be viewed from
##   the Edit Actions Dialog Utilities window

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, editArea, timeRange, varDict):
        # Calls each Smart Tool: T_Tool, PoP_Tool, Wind_Tool

        gmTime = time.gmtime(time.time())
        current = AbsTime.absTimeYMD(gmTime[0],gmTime[1],gmTime[2], gmTime[3], 0, 0)
        current_gmHour = gmTime[3]
        start = time.time() - 48*3600
        end = time.time() + (24-current_gmHour + 204)*3600
        timeRange = TimeRange.TimeRange(AbsTime.AbsTime(start), AbsTime.AbsTime(end))

# MRV added 4/27 Fragment Wind grids...
        self.saveElements(["Wind"])
        print "TimeRange=",timeRange
        self.fragmentCmd(['Wind'], timeRange)
        #

        self.saveElements(["Wind"])
        #self.publishElements(["Wind"], timeRange)
