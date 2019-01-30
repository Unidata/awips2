# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# UserInfo.py
#
# getForecasterName
# Return the forecaster name that should be added to the end of the product
#
# Author: fachorn
#
# History:
# F.Achorn/OPC    03/08/13    initial creation
# F.Achorn/OPC    09/24/13    modified to get the debug level corectly from argDict
# ----------------------------------------------------------------------------

import TextRules
import SampleAnalysis
import time, string
import os

class UserInfo(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)

    def forecasterDict(self):
        self.debug_print("Debug: forecasterDict in UserInfo")
        return {
            "clandsea": "Landsea"
            }

    def _getForecasterName(self, argDict):
        # get the debug level from the argDict, if available
        try:
            self._debug = argDict["debug"]
        except:
           self._debug = 1
        self.debug_print("Debug: _getForecasterName in UserInfo")
        userName = os.environ["USER"]
        if userName in self.forecasterDict().keys():
            forecaster = self.forecasterDict()[userName]
        else:
            forecaster = "NATIONAL HURRICANE CENTER"
        self.debug_print("Forecaster = " + forecaster)
        return forecaster

