# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# RemoveRandomJSONFile.py
#
# Author: lefebvre
#
# # SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# Apr  13, 2022 22531      tlefebvr   Initial creation
# ----------------------------------------------------------------------------
MenuItems = ["None"]

VariableList = []

import LocalizationSupport
import ProcessVariableList
import os
import TropicalUtility
import WindWWUtils


class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)

        self._WindWWUtils = WindWWUtils.WindWWUtils(dbss)

    def execute(self, editArea, timeRange, varDict):
        path = self._getAdvisoryPath()

        # Returns a list for each basin so reduce to simple list
        stormInfoDicts = self._WindWWUtils.getStormInfoDicts()
        # Fetch the list of advisories
        pilList = []
        for dict in stormInfoDicts:
            pilList.append(dict["pil"])

        if len(pilList) == 0:
            self.statusBarMsg("There are no JSON files to remove.", "A")
            return

        pilList.sort()

        variableList = []
        variableList.append(("Select bulletin to remove:", pilList[0], "radio", pilList))
        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList("Permanently remove JSON file", variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            self.cancel()
            return

        advisory = varDict["Select bulletin to remove:"]
        path = self._getAdvisoryPath()
        siteID = self.getSiteID()

        fileName = os.path.join(path, f"{advisory}.json")
        LocalizationSupport.deleteFile(LocalizationSupport.CAVE_STATIC, LocalizationSupport.SITE,
                                               siteID, fileName)