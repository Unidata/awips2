# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# StormInfo - Version 3.0
#
# Authors: Matt Belk (BOX), Shannon White (OCWWS), Tom LeFebvre (GSD), Pablo Santos (MFL)
#
# ----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# Sep 13, 2016                        Adjustments from Hermine to add
#                                     Post-Tropical stormType.
# Sep 19, 2016 19293      randerso    Initial baseline check in
#
########################################################################

import os
import time

import ProcessVariableList
import StormNames
import TropicalUtility


MenuItems = ["None"]

class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)


    def execute(self):

        # Get list of available storms
        stormList = self.extractStormInfo()

        # Get info on what storm this is
        bogusStormName = "ZYXWWXYZ" # Define a bogus storm so none will be defaulted
        selectedName = self.determineStorm(stormList, bogusStormName)

        # Ensure we have a choice
        if selectedName == bogusStormName:
            self.statusBarMsg("Please rerun StormInfo and select a storm name.", "U")
            return
        else:
            stormName = selectedName.strip()

        #  Define a dictionary of PILs to use for each basin
        PILs = {
                "Atlantic": ["AT1", "AT2", "AT3", "AT4", "AT5"],
                "Eastern Pacific": ["EP1", "EP2", "EP3", "EP4", "EP5"],
                "Central Pacific": ["CP1", "CP2", "CP3", "CP4", "CP5"],
                }

        Numbers = ["One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight",
                   "Nine", "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen",
                   "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen", "Twenty",
                   "Twenty-One", "Twenty-Two", "Twenty-Three", "Twenty-Four", "Twenty-Five"]

        #  Set things up for the Atlantic by default
        Basin = "Atlantic"
        maxLists = len(StormNames.NameDict[Basin])

        #  Get the current UTC year - two digits only
        curYear = self._gmtime().timetuple().tm_year % 100

        stormList = self.extractStormInfo()

        # Build the Variable Lists dynamically based on the chosen storm

        # If New, make default GUI
        newStorm = False
        if stormName == "New":
            newStorm = True
            variableList = []
            variableList.append(("AWIPS bin number", "", "radio", PILs[Basin]))
            variableList.append(("Storm Type", "Tropical Storm", "radio",
                                 ["Potential Tropical Cyclone", "Subtropical Depression", "Subtropical Storm",
                                  "Tropical Depression", "Tropical Storm", "Hurricane", "Post-Tropical Cyclone"]))
            variableList.append(("Storm Name" , "", "radio", StormNames.NameDict[Basin][curYear % maxLists]))
            variableList.append(("Other Storm Name (e.g. Alpha but NOT Three)", "", "alphaNumeric"))
            variableList.append(("Storm Number", 1, "scale", [1, 25], 1))
            variableList.append(("Advisory Type" , "Routine", "radio",
                                 ["Routine", "Special", "Intermediate"]))
            variableList.append(("Advisory Number" , "", "alphaNumeric"))

        # If existing storm, build GUI using previous choices
        else:
            for sDict in stormList:
                if sDict["stormName"] == stormName:
                    PIL = sDict["pil"]
                    stormType = sDict["stormType"]
                    stormNum = sDict["stormNumber"]
                    advisoryType = sDict["advisoryType"]
                    advisoryNum = sDict["advisoryNumber"]

            variableList = []
            variableList.append(("AWIPS bin number", PIL, "radio", [PIL]))
            variableList.append(("Storm Type", stormType, "radio",
                                 ["Potential Tropical Cyclone", "Subtropical Depression", "Subtropical Storm",
                                  "Tropical Depression", "Tropical Storm", "Hurricane", "Post-Tropical Cyclone"]))
            if stormName in Numbers:
                variableList.append(("Storm Name", "None", "radio", StormNames.NameDict[Basin][curYear % maxLists]))
            elif stormName not in StormNames.NameDict[Basin][curYear % maxLists]:
                variableList.append(("Other Storm Name (e.g. Alpha but NOT Three)", stormName, "alphaNumeric"))
            else:
                variableList.append(("Storm Name", stormName, "radio", [stormName]))
            variableList.append(("Storm Number", stormNum, "radio", [stormNum]))
            variableList.append(("Advisory Type" , advisoryType , "radio",
                                 ["Routine", "Special", "Intermediate"]))
            variableList.append(("Advisory Number" , advisoryNum, "alphaNumeric"))


        # Display the GUI
        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList(
            "Set Advisory Information", variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            self.cancel()

        #  Collect all the info provided by the forecaster
        pil = varDict["AWIPS bin number"].strip()
        stormType = varDict["Storm Type"].strip()
        if varDict.has_key("Other Storm Name (e.g. Alpha but NOT Three)"):
            otherStormName = varDict["Other Storm Name (e.g. Alpha but NOT Three)"].strip()
        else:
            otherStormName = ""

        if otherStormName is not "":
            stormName = otherStormName
        else:
            if len(varDict["Storm Name"]) == 0:
                stormName = "None"
            else:
                stormName = varDict["Storm Name"]

        # QC the storm number
        stormNumber = int(varDict["Storm Number"])
        try:
            pilModNumber = int(pil[2:])
            if pilModNumber == 5:
                pilModNumber = 0
        except:
            self.statusBarMsg("You did not provide a correct bin. Please rerun StormInfo.", "U")
            return

        if stormNumber % 5 != pilModNumber:
            self.statusBarMsg("The chosen storm number is not correct for chosen bin. Please rerun StormInfo.", "S")
            return

        if stormName == "None":
            stormName = Numbers[int(stormNumber)-1]

        advisoryType = varDict["Advisory Type"].strip()

        try:
            advisoryNumber = varDict["Advisory Number"].strip()
        except:
            advisoryNumber = ""

        if advisoryNumber == "":
            self.statusBarMsg("The advisory number is missing. Please rerun StormInfo.", "S")
            return
        
        if advisoryType == "Intermediate":
            advisoryNumber += "A"

        if not newStorm:
            if advisoryNumber == advisoryNum:
                self.statusBarMsg("You did not increment the advisory number. Please rerun StormInfo. ", "A")
                return


        #  Open the file to store all the info for this particular storm
        stormDict = {}
        stormDict["stormType"] = stormType
        stormDict["stormName"] = stormName
        stormDict["stormNumber"] = stormNumber
        stormDict["advisoryType"] = advisoryType
        stormDict["advisoryNumber"] = advisoryNumber
        stormDict["pil"] = pil

        #  Save the info for this storm
        self._saveAdvisory(pil, stormDict)

