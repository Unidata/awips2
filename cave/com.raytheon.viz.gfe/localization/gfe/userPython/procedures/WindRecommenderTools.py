
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# WindRecommenderTools
#
# Authors: Matt Belk, Tom LeFebvre, Pablo Santos, Shannon White
# Sept   9, 2020 21020     tlefebvr    Changed toolList based on siteID.
# Sept  14, 2020 21020     tlefebvr    Changed back to generic procedure.
# July  29, 2021 22531     tlefebvr    Final Code clean-up.
# Aug.  23, 2021 22531     tlefebvr    Changed "tool" to "procedure" based on code review.
# ----------------------------------------------------------------------------

MenuItems = ["None"]

import SmartScript

# Order in which procedure names will appear in the GUI.
procList = ["StormInfo", "SelectBreakPoints", "RecommendWindWW", "SaveAndSendJSONFile",
            "RestoreWindWWHazards - RUN ONLY AFTER National TCV is transmitted",
            "ArchiveHazardGridsToJSON", "ArchiveTPCWindProbStats",
            "GenerateWindWWXML", "BasinCrossingCyclone", "UpdateJSONFromTextProduct"]

VariableList = [("Select Procedure to Run:" , "StormInfo", "radio", procList),
                ]


class Procedure (SmartScript.SmartScript):

    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, varDict):

        procName = varDict["Select Procedure to Run:"]

        if procName == "RestoreWindWWHazards - RUN ONLY AFTER National TCV is transmitted":
            procName = "RestoreWindWWHazards"

        self.callProcedure(procName)

        return