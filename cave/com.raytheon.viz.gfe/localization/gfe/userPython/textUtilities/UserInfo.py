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
import os

class UserInfo(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)

    def forecasterDict(self):
        self.debug_print("Debug: forecasterDict in UserInfo")
        return {
            "clandsea": "Landsea",
            "mnelson": "Nelson", 
            "sstripli": "Stripling",
            "echrist": "Christensen", 
            "jlewitsk": "Lewitsky", 
            "alevine": "AL", 
            "jaguirre": "Aguirre",
            "grubio": "GR", 
            "dmundell": "Mundell", 
            "cmcelroy": "McElroy", 
            "alatto": "Latto", 
            "mformosa": "Formosa", 
            "nramos": "Ramos", 
            "mtichace": "MT",
            "erivera": "ERA", 
            "jcangial": "Cangialosi", 
            "sstewart": "Stewart",
            "pmanougi": "Manougian",
            "dfigursk": "DJF",
            "Jsienkie": "Sienkiewicz",
            "fachorn": "FAchorn",
            "cjuckins": "Juckins",
            "jclark": "Clark",
            "tcollins": "Collins",
            "jkells": "Kells",
            "dmills": "Mills",
            "dscovil": "Scovil",
            "kachorn": "KAchorn",
            "gbancrof": "Bancroft",
            "kbell": "Bell",
            "tholley": "Holley",
            "mhuffman": "Huffman",
            "dkosier": "Kosier",
            "jkrekele": "Krekeler",
            "fmusonda": "Musonda",
            "jnolt": "Nolt",
            "breinhar": "Reinhart",
            "mrowland": "Rowland",
            "tshaw": "Shaw",
            "lsommerv": "Sommerville",
            "elau": "ELau",
            "cbrenchl": "Brenchley",
            "rballard": "RBallard",
            "jbravend": "Bravender",
            "kkodama": "Kodama",
            "tbirchar": "Birchard",
            "cjacobso": "Jacobson",
            "jjelsema": "Jelsema",
            "jpowell": "Powell",
            "dwroe": "Wroe",
            "mballard": "MBallard",
            "abedal": "Bedal",
            "rbohlin": "Bohlin",
            "bburke": "Burke",
            "pdonalds": "Donaldson",
            "leaton": "Eaton",
            "mfoster": "Foster",
            "agibbs": "Gibbs",
            "shouston": "Houston",
            "nhui": "Hui",
            "rkinel": "Kinel",
            "hlau": "Lau",
            "imorriso": "Morrison",
            "tstall": "Stall",
            "valmanza": "Almanza",
            "chevalie": "Chevalier",
            "mdye": "Dye",
            "jsaucier": "Saucier",
            "msardi": "MAS",
            "tlefebvre": "LeFebvre",
            "thansen": "Hansen",
            "swhite": "White",
            }

    def _getForecasterName(self, argDict):
        # get the debug level from the argDict, if available
        try:
            self._debug = argDict["debug"]
        except:
           self._debug = 1
        self.debug_print("Debug: _getForecasterName in UserInfo")
        userName = os.environ["USER"]
        if userName in self.forecasterDict():
            forecaster = self.forecasterDict()[userName]
        else:
            forecaster = "NATIONAL HURRICANE CENTER"
        self.debug_print("Forecaster = " + forecaster)
        return forecaster

