# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# RemoveAllJSONFiles.py
#
#
# Author: lefebvre

# # SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# May 15 2020 21844      lefebvre     Original version
# Apr 13 2022 22531      tlefebvre    Made a few changes for Python3 compatibility.
# Sep 15 2022 22531         santos/composano Fixes post 21.4.1-13 testing.
# ----------------------------------------------------------------------------
MenuItems = ["None"]

VariableList = []
msgStr = "\n Permanently removing all (AT1-PQ5) JSON files:\n\n Are you sure?"
VariableList.append((msgStr, "No", "radio", ["Yes", "No"]))

import LocalizationSupport
import os
import TropicalUtility
import operator


class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)

    def execute(self, varDict):
        # Make sure we're sure.
        if varDict[msgStr] != "Yes":
            return

        path = self._getAdvisoryPath()

        allBasinNames = self.basinNames()
        advisoryList = self.basinBins(allBasinNames)
        basinList = []
        for advList in advisoryList:
            for adv in advList:
                basinList.append(adv)
        # Returns a list for each basin so reduce to simple list
#         advisoryList = reduce(operator.concat, advisoryList)
        # Iterate over every advisory type, make the fileName, and delete the file.
        for siteID in self._activeSiteIDs:
            for advisory in basinList:
                fileName = os.path.join(path, f"{advisory}.json")
                LocalizationSupport.deleteFile(LocalizationSupport.CAVE_STATIC, LocalizationSupport.SITE,
                                               siteID, fileName)