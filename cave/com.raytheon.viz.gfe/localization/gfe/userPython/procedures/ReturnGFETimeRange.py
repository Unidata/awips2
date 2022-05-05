# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ReturnGFETimeRange - Version 20181114
#
# Author: lefebvre
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Edit"]
import LogStream, time
from math import *

VariableList = []

import time
import SmartScript

## For documentation on the available commands,
##   see the SmartScript Utility, which can be viewed from
##   the Edit Actions Dialog Utilities window

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, timeRange, varDict):

        varDict["SelectedTimeRange"] = (timeRange.startTime().unixTime(), timeRange.endTime().unixTime())

        return



