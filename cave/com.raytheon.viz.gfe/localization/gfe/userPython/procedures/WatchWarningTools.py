# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# WatchWarningTools
#
# Authors: Matt Belk, Tom LeFebvre, Pablo Santos, Shannon White
# ------------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# May 20, 2020 22033      tlefebvr    Addressed code review comments
# ----------------------------------------------------------------------------

MenuItems = ["Populate"]

VariableList = [ ("Select Tool to Run:", "StormInfo", "radio",
                 ["StormInfo", "SelectBreakpoints", "RecommendWindWW",
                  "CreateProposedSS", "SendProposedToWFO",
                  "MergeWFOEdits", "FinalizeHazards", "Finalize_KML",
                  "CreateNatlTCVZoneGroups"]), #"SaveCurrentComboToPrev"]),
               ]

import SmartScript

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, varDict):

        toolName = varDict["Select Tool to Run:"]
        self.callProcedure(toolName)

        return

