# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ReturnGFETimeRange - Version 20191212
#
# Author: lefebvre
# ----------------------------------------------------------------------------

import SmartScript

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Edit"]

VariableList = []


class Procedure(SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, timeRange, varDict):

        # print("ReturnTR:timeRange:", timeRange, "type:", type(timeRange))
        varDict["SelectedTimeRange"] = (
            timeRange.startTime().unixTime(),
            timeRange.endTime().unixTime(),
        )

        return
