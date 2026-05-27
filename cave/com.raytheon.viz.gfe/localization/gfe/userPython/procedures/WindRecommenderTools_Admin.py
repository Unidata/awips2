# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# WindRecommenderTools
#
# Authors: Matt Belk, Tom LeFebvre, Pablo Santos, Shannon White
# Sept   9, 2020 21020     tlefebvr    Changed toolList based on siteID.
# Sept  14, 2020 21020     tlefebvr    Changed back to generic tool

# ----------------------------------------------------------------------------

MenuItems = ["None"]

import SmartScript

toolList = ["StormInfo", "SelectBreakPoints", "RecommendWindWW", "SaveAndSendJSONFile",
            "RestoreWindWWHazards - RUN ONLY AFTER National TCV is transmitted",
            "ArchiveHazardGridsToJSON", "ArchiveTPCWindProbStats", "GenerateWindWWXML",
            "BasinCrossingCyclone", "ArchiveStormSummary", "SummarizeArchivedHazards",
            "UpdateJSONFromTextProduct", "RemoveAllJSONFiles", "RemoveRandomJSONFile",
            "RemoveZoneMap", "CopyProposedTropWindWW", "MergeProposedWW",
            ]

VariableList = [("Select Tool to Run:" , "StormInfo", "radio", toolList),
                ]


class Procedure (SmartScript.SmartScript):

    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, varDict):
        toolName = varDict["Select Tool to Run:"]

        if toolName == "RestoreWindWWHazards - RUN ONLY AFTER National TCV is transmitted":
            toolName = "RestoreWindWWHazards"

        self.callProcedure(toolName)

        return