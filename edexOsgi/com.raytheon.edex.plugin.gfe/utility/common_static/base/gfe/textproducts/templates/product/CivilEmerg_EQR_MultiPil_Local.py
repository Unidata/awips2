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

##
# This is a base file that is not intended to be overridden.
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
# Oct 10, 2016    #5749          randerso    Converted ellipses to commas
# Feb 21, 2018    #7075          dgilling    Add issuanceType to MND header.
#
##

#-------------------------------------------------------------------------
# Example Output:
#  Refer to the NWS 10-518 Directive for further information.
#-------------------------------------------------------------------------

import copy
import types

import CivilEmerg


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
             (("Felt:", "felt") , "weakly", "radio",
              ["weakly", "moderately", "strongly", "very strongly"]), 
             (("How Many Reports:", "extent") , "a single person", "radio",
              ["a single person", "a few people", "many people", "numerous persons"]),
             (("Damage", "damage") , "No", "radio",
              ["No", "Slight", "Moderate", "Considerable", "Extensive"]),
             (("Damage Type", "damageType") , [], "check",
              ["objects falling from shelves", "dishes rattled or broken",
               "cracked chimneys", "communications towers fallen",
               "collapsed bridges", "collapsed overpasses", "train rails bent",
               "fissures have opened in the ground", "gas mains broken",
               "complete destruction of structures", "some casualties"]),
             ] 

    def __init__(self):
        CivilEmerg.TextProduct.__init__(self)        

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        varDict = argDict["varDict"]
        for (key, value) in varDict.items():
            if type(key) is tuple:
                label, variable = key
                setattr(self, f"_{variable}", value)

        fcst = fcst + "An earthquake has been felt " + self._felt + " by " +\
          self._extent + " " + "in the |*enter area*| area. " + \
          self._damage + " damage has been reported. "

        if self._damage != "No":
            fcst = fcst + " Damage reports so far: "
            fcst = fcst + self.punctuateList(self._damageType)
            fcst = fcst + "."

        fcst = fcst + "\n\n"

        if self._eqInfo == "Golden":
            eqOffice = "National Earthquake Information Center in Golden Colorado"
        elif self._eqInfo == "WC/ATWC":
            eqOffice = "West Coast/Alaska Tsunami Warning Center"
        else:
            eqOffice = "Pacific Tsunami Warning Center"
            
        
        if self._issuanceType == "Preliminary":
            fcst = fcst + "Information released in this statement is preliminary. Updates, including Richter scale magnitude, will be provided as more information becomes available from the " + eqOffice + "."
        else:
            fcst = fcst + "The " + eqOffice + " has indicated that an earthquake magnitude *mag* on the Richter scale was centered at *lat*/*lon* or about *miles* *direction* of *city*, *state*.\n\nAny further information will be made available when it is received from the " + eqOffice + "."
        
        return fcst

    def _postProcessProduct(self, fcst, argDict):
        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "...", "-"])
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        #
        # First, generate WMO lines
        #
        fcst = "{} {} {}\n{}\n".format(self._wmoID, self._fullStationID, self._ddhhmmTime, self._pil)

        #
        # Next, add the non-segmented UGC data
        #
        areaHeader = self.makeAreaHeader(
            argDict, areaLabel, self._issueTime, self._expireTime,
            self._areaDictionary, self._defaultEditAreas, cityDescriptor=self._cityDescriptor,
            includeCities=self._includeCities, includeZoneNames = self._includeZoneNames,
            includeIssueTime = self._includeIssueTime)
        fcst += areaHeader + "\n"

        #
        # Last, add the product name/time lines
        #
        try:
            if self._eas == "None":
                self._eas = ""
            else:
                self._eas = self._eas + "\n"
        except:
            self._eas = ""

        try:
            source = self._source + '\n'
        except:
            source = ""

        issuedByString = self.getIssuedByString()
        productName = "{}...{}".format(self._productName, self._issuanceType)
        productName = self.checkTestMode(argDict, productName)
        fcst += self._eas + \
                productName + "\n" + \
                source + \
               "Relayed by National Weather Service " + self._wfoCityState + "\n" + \
                issuedByString + \
                self._timeLabel + "\n\n"
        return fcst
