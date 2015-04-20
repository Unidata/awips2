##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CivilEmerg_EQR_Local
#  This product produces an earthquake report.
#
# Author: davis
# ----------------------------------------------------------------------------
##
#
# SOFTWARE HISTORY
# Date            Ticket#        Engineer    Description
# ------------    ----------     ----------- --------------------------
# Oct 20, 2014    #3685          randerso    Changed to support mixed case
#
##

#-------------------------------------------------------------------------
# Example Output:
#  Refer to the NWS 10-518 Directive for further information.
#-------------------------------------------------------------------------

import CivilEmerg
import TextRules
import string, time, re, os, types, copy

class TextProduct(CivilEmerg.TextProduct):
    Definition = copy.deepcopy(CivilEmerg.TextProduct.Definition)
    Definition["displayName"] = "CivilEmergency_EQR_<MultiPil> (Earthquake Report)"
             # for Product Generation Menu
    Definition["outputFile"] = "{prddir}/TEXT/EQR_<MultiPil>.txt"
    ## Edit Areas: Create Combinations file with edit area combinations.
    Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display
    Definition["defaultEditAreas"] = "Combinations_EQR_<site>"
    Definition["productName"] = "Earthquake Report" # product name
    Definition["wmoID"] = "<wmoID>"                      # WMO code
    Definition["pil"] = "<pil>"                      # product pil
    Definition["textdbPil"] = "<textdbPil>"          # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"      # Product ID for transmitting to AWIPS WAN.

    Definition["mapNameForCombinations"] = "Zones_<site>"

    VariableList = [
             (("Issuance Type", "issuanceType") , "Preliminary", "radio",
              ["Preliminary", "Update"]),
             (("Official Earthquake Info Source:", "eqInfo") , "Golden", "radio",
              ["Golden", "WC/ATWC", "PTWC"]),
             (("Felt:", "felt") , "Weakly", "radio",
              ["Weakly", "Moderately", "Strongly", "Very strongly"]), 
             (("How Many Reports:", "extent") , "A single person", "radio",
              ["A single person", "A few people", "Many people", "Numerous persons"]),
             (("Damage", "damage") , "No", "radio",
              ["No", "Slight", "Moderate", "Considerable", "Extensive"]),
             (("Damage Type", "damageType") , ["No damage"], "check",
              ["No damage", "Objects falling from shelves", "Dishes rattled or broken",
               "Cracked chimneys", "Communications towers fallen",
               "Collapsed bridges", "Collapsed overpasses", "Train rails bent",
               "Fissures have opened in the ground", "Gas mains broken",
               "Complete destruction of structures", "Some casualties"]),
             ] 

    def __init__(self):
        CivilEmerg.TextProduct.__init__(self)        

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"

        fcst = fcst + "An earthquake has been felt " + self._felt + " by " +\
          self._extent + " " + "in the |*enter area*| area. " + \
          self._damage + " damage has been reported. "

        if self._damage != "No":
            fcst = fcst + " Damage reports so far..."
            for each in self._damageType:
                if each != "No damage":
                    fcst = fcst + each + "..."
            fcst = string.rstrip(fcst, ".")
            fcst = fcst + "."

        fcst = fcst + "\n\n"

        if self._eqInfo == "Golden":
            eqOffice = "National Earthquake Information Center in Golden Colorado"
        elif self._eqInfo == "Wc/atwc":
            eqOffice = "West Coast/Alaska Tsunami Warning Center"
        else:
            eqOffice = "Pacific Tsunami Warning Center"
            
        
        if self._issuanceType == "Preliminary":
            fcst = fcst + "Information released in this statement is preliminary. Updates...including Richter scale magnitude...will be provided as more information becomes available from the " + eqOffice + "."
        else:
            fcst = fcst + "The " + eqOffice + " has indicated that an earthquake magnitude *mag* on the Richter scale was centered at *lat*/*lon* or about *miles* *direction* of *city*...*state*.\n\nAny further information will be made available when it is received from the " + eqOffice + "."
        
        return fcst

    def _postProcessProduct(self, fcst, argDict):
        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "...", "-"])
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst
