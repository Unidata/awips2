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
    Definition["productName"] = "EARTHQUAKE REPORT" # product name
    Definition["wmoID"] = "<wmoID>"                      # WMO code
    Definition["pil"] = "<pil>"                      # product pil
    Definition["textdbPil"] = "<textdbPil>"          # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"      # Product ID for transmitting to AWIPS WAN.

    Definition["mapNameForCombinations"] = "Zones_<site>"

    VariableList = [
             (("Issuance Type", "issuanceType") , "PRELIMINARY", "radio",
              ["PRELIMINARY", "UPDATE"]),
             (("Official Earthquake Info Source:", "eqInfo") , "GOLDEN", "radio",
              ["GOLDEN", "WC/ATWC", "PTWC"]),
             (("Felt:", "felt") , "WEAKLY", "radio",
              ["WEAKLY", "MODERATELY", "STRONGLY", "VERY STRONGLY"]), 
             (("How Many Reports:", "extent") , "A SINGLE PERSON", "radio",
              ["A SINGLE PERSON", "A FEW PEOPLE", "MANY PEOPLE", "NUMEROUS PERSONS"]),
             (("Damage", "damage") , "NO", "radio",
              ["NO", "SLIGHT", "MODERATE", "CONSIDERABLE", "EXTENSIVE"]),
             (("Damage Type", "damageType") , ["NO DAMAGE"], "check",
              ["NO DAMAGE", "OBJECTS FALLING FROM SHELVES", "DISHES RATTLED OR BROKEN",
               "CRACKED CHIMNEYS", "COMMUNICATIONS TOWERS FALLEN",
               "COLLAPSED BRIDGES", "COLLAPSED OVERPASSES", "TRAIN RAILS BENT",
               "FISSURES HAVE OPENED IN THE GROUND", "GAS MAINS BROKEN",
               "COMPLETE DESTRUCTION OF STRUCTURES", "SOME CASUALTIES"]),
             ] 

    def __init__(self):
        CivilEmerg.TextProduct.__init__(self)        

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"

        fcst = fcst + "AN EARTHQUAKE HAS BEEN FELT " + self._felt + " BY " +\
          self._extent + " " + "IN THE |*enter area*| AREA. " + \
          self._damage + " DAMAGE HAS BEEN REPORTED. "

        if self._damage != "NO":
            fcst = fcst + " DAMAGE REPORTS SO FAR..."
            for each in self._damageType:
                if each != "NO DAMAGE":
                    fcst = fcst + each + "..."
            fcst = string.rstrip(fcst, ".")
            fcst = fcst + "."

        fcst = fcst + "\n\n"

        if self._eqInfo == "GOLDEN":
            eqOffice = "NATIONAL EARTHQUAKE INFORMATION CENTER IN GOLDEN COLORADO"
        elif self._eqInfo == "WC/ATWC":
            eqOffice = "WEST COAST/ALASKA TSUNAMI WARNING CENTER"
        else:
            eqOffice = "PACIFIC TSUNAMI WARNING CENTER"
            
        
        if self._issuanceType == "PRELIMINARY":
            fcst = fcst + "INFORMATION RELEASED IN THIS STATEMENT IS PRELIMINARY. UPDATES...INCLUDING RICHTER SCALE MAGNITUDE...WILL BE PROVIDED AS MORE INFORMATION BECOMES AVAILABLE FROM THE " + eqOffice + "."
        else:
            fcst = fcst + "THE " + eqOffice + " HAS INDICATED THAT AN EARTHQUAKE MAGNITUDE *MAG* ON THE RICHTER SCALE WAS CENTERED AT *LAT*/*LON* OR ABOUT *MILES* *DIRECTION* OF *CITY*...*STATE*.\n\nANY FURTHER INFORMATION WILL BE MADE AVAILABLE WHEN IT IS RECEIVED FROM THE " + eqOffice + "."
        
        return fcst

    def _postProcessProduct(self, fcst, argDict):
        fcst = string.upper(fcst)
        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "...", "-"])
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst
