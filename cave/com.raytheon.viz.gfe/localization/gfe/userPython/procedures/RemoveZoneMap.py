# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# RemoveZoneMap.py
#
# Author: lefebvre
# ----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ----------------------------------------
# May 20, 2020 22033      tlefebvr    Addressed code review comments.
# 
# ----------------------------------------------------------------------------

MenuItems = ["Edit"]

import SmartScript

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, editArea, timeRange, varDict):
        name = "NHCZoneMap"
        category = "ZoneMap"

        self.deleteObject(name, category)

