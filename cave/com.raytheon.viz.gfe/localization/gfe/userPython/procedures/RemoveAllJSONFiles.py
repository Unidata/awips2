# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# RemoveAllJSONFiles.py
#
# Author: lefebvre
# ----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ----------------------------------------
# May 15, 2020 21844      lefebvre    Original version
# 
##############################################################################

MenuItems = ["Edit"]

VariableList = []
msgStr = "\n Permanently removing all (AT1-WP5) JSON files:\n\n Are you sure?"
VariableList.append((msgStr, "No", "radio", ["Yes", "No"]))

import LocalizationSupport
import os
import TropicalUtility
import WindWWUtils
import functools
import operator

class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        
        self._WindWWUtils = WindWWUtils.WindWWUtils(dbss)        

    def execute(self, varDict):
        
        # Make sure we're sure.
        if varDict[msgStr] != "Yes":
            return
                
        path = self._getAdvisoryPath()

        allBasinNames = self._WindWWUtils.basinNames()
        advisoryList = self._WindWWUtils.basinBins(allBasinNames)

        # Returns a list for each basin so reduce to simple list 
        advisoryList = functools.reduce(operator.concat, advisoryList)
        
        # Iterate over every advisory type, make the fileName, and delet the file.
        for siteID in self._activeSiteIDs:
            for advisory in advisoryList:
                fileName = os.path.join(path, advisory + ".json") 
                LocalizationSupport.deleteFile(LocalizationSupport.CAVE_STATIC, LocalizationSupport.SITE,
                                               siteID, fileName)

