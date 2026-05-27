# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# StormInfo - Version 4.1
#
# Authors: Matt Belk (BOX), Shannon White (OCWWS), Tom LeFebvre (GSL), Pablo Santos (MFL)
#
# ----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Sep 13, 2016                     Adjustments from Hermine to add
#                                  Post-Tropical stormType.
# Sep 19, 2016  19293    randerso  Initial baseline check in
# Aug 05, 2019           tlefebvr  Modified to Support Wind Recommender as well
# Apr 22, 2020           tlefebvr  Old JSON files are now purged when older
#                                  than a day. JSON file written to all active
#                                  sites to keep them consistent.
# May 01, 2020  22033    tlefebvr  Added code to support CPHC forecasts
# May 05  2020  22033    tlefebvr  Added WP bins to bin list.
# May 07  2020  22033    tlefebvr  Fixed bug that failed to update stormName
#                                  when upgrading to named storm.
# May 11  2020  22033    tlefebvr  Added code to check stormID to ensure no
#                                  duplicates.
# May 12  2020  22033    tlefebvr  Added dialog to ask if storm number should
#                                  be reused.
# May 13  2020  22033    tlefebvr  Added method to save JSON file to text
#                                  product so WFOs get the information.
#                                  Plus other tweaks.
# May 13  2020  22033    tlefebvr  Active storms limited to those in the local
#                                  basin.
# May 14  2020  22033    tlefebvr  Modified to use WWUtil ***Sites methods.
# May 15  2020  22033    tlefebvr  Fixed issue with Numbered storm names not
#                                  appearing. Third try.
# May 18  2020  22033    tlefebvr  Filtered stormNames by siteID.
# May 19  2020  22033    tlefebvr  Fixed storm filtering bug.
# May 21  2020  22033    tlefebvr  Addressed code review comments.
# May 27  2020  22033    tlefebvr  Added StormID to existing storm GUI.
# May 28  2020  22033    tlefebvr  Addressed code review comment.
# May 28  2020  22033    tlefebvr  Added stormID to stormName label
# May 29  2020  22033    tlefebvr  Refactored makeStormID into WindWWUtils.
# May 30  2020  22033    tlefebvr  Fixed bug in filtering active storms.
# Feb 16  2020  22033    tlefebvr  Changed GUI code to use
#                                  TropicalUtility.maxStorms().
# Feb 19, 2020  22033    tlefebvr  When stormNUmber is out of sequence with bin
#                                  number no a dialog asks the user if they are
#                                  sure rather than aborting the tool.
# May 10, 2021  22033    tlefebvr  Changed data file location to edex/data/share.
# May 11, 2021  22033    tlefebvr  A few adjustments to accommodate adding new bins.
# May 13, 2021  22033    tlefebvr  Change method to read advisories to
#                                  WindWWUtils version.
# May 13, 2021  22033    tlefebvr  Changed saveAdvisory to use common version.
# May 18, 2021  22033    tlefebvr  Removed GUM override for testing.
# May 27, 2021  22033    tlefebvr  Fixed a bug with pilList defined in wrong
#                                  block.
# Jun 07, 2021  22033    tlefebvr  Fixed issues with StormNames/siteID and
#                                  added code to list supplemental names.
# Jul 29, 2021  22531    tlefebvr  Final code clean-up before check-in.
# Aug 24, 2021  22531    tlefebvr  Removed extraneous import.
# Sep 15, 2021  8657     randerso  Moved makeStormID() up to TropicalUtility
#                                  Additional code refactoring and cleanup
# Jan 20, 2022  22531    psantos/scamp    Added 3rd arg with gfesiteID when calling pushJsonFile
# Mar 10, 2022  22531    tlefebvr    Added "advisoryTime" to JSON file for statistics purposes.
# Apr 13, 2022  22531    tlefebvr    Made a few changes for Python3 compatibility.
# May 23, 2022  23092    jkelmer   Replaced "is" with == for literal comparison
# Jun 12, 2023  2029646  santos/ps Fix for extra space when determining operational mode.
# Jun 13, 2023  2029646  swhite    Add "Remnants of" to Storm Type
#
########################################################################

# #
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
# #
import ProcessVariableList
import StormNames
import TropicalUtility
import WindWWUtils
import LocalizationSupport
import os

MenuItems = ["None"]

single_words = ['zero', 'one', 'two', 'three', 'four',
                 'five', 'six', 'seven', 'eight', 'nine',
                 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen',
                 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen']
tens_multiples = [None, None, 'twenty', 'thirty', 'forty',
                  'fifty', 'sixty', 'seventy', 'eighty', 'ninety']
valid_range = range(0, 100)
valid_cases = [str.upper, str.lower, str.title]
valid_case_checks = [None, str.isupper, str.islower, str.istitle]


class Procedure (WindWWUtils.WindWWUtils):

    def __init__(self, dbss):
        WindWWUtils.WindWWUtils.__init__(self, dbss)
        self._path = self.getDataFilePath()

    def removeOldJSONFiles(self, stormList):
        """
        Checks the "lastModified" time and if it's older than purgeAge,
        the file is removed from the storage system.
        """
        purgeAge = 24 * 3600  # one day
        deleteList = []
        for storm in stormList:
            if "lastModified" not in storm:
                continue
            if (self._gmtime().unixTime() - storm["lastModified"]) > purgeAge:
                deleteList.append(storm)
        # If no old storms found, bail.
        if not deleteList:
            return stormList

        # Iterate over every advisory type, get the fileName, and delete the file.
        for delStorm in deleteList:
            # Remove it from the internal list
            stormList.remove(delStorm)
            # Remove it from Localization storage
            fileName = self._getAdvisoryFilename(delStorm["pil"])
            self.statusBarMsg("Removing Old Tropical JSON file: " + fileName, "S")
            for siteID in self.activeSiteIDs():
                LocalizationSupport.deleteFile(LocalizationSupport.CAVE_STATIC,
                                               LocalizationSupport.SITE, siteID, fileName)

        return stormList

    def getPILList(self, siteID):
        """
        Returns the list of PILs or Bins based on the site ID.
        """
        basinList = self.forecastBasins(siteID)
        if not basinList:
            self.statusBarMsg("This tool is not configured for " + siteID, "S")
            return []
        pilList = []
        for pils in self.basinBins(basinList):
            pilList += pils
        return pilList

    def makeStormList(self, basinDict):
        """
        Returns the list of storm for the specified basinList.
        """
        stormList = []
        for i in sorted(basinDict.keys()):
            stormList += basinDict[i]
        return stormList

    def shuffleStormList(self, rawStormList):
        """
        Rearranges the stormList so that it starts with the next available
        storm. Only works when there are activeStorms.
        """
        # If no active storms we don't know where to start
        # so just return the raw list.
        if not self._activeStormNames:
            return rawStormList

        lastStorm = self._activeStormNames[-1]
        if lastStorm in rawStormList:
            startIndex = rawStormList.index(lastStorm) + 1
            if startIndex == len(rawStormList):
                startIndex = 0
            stormList = rawStormList[startIndex:] + rawStormList[0:startIndex]
            return stormList

        return rawStormList

    def getStormNameList(self, siteID, currentYear):
        """
        Fetches the stormName list based on the siteID and in the case
        of NHC the year, as they rotate stormName lists each year.
        """
        basinList = self.forecastBasins(siteID)
        if not basinList:
            self.statusBarMsg(f"This tool is not configured for {siteID}", "S")
            return []
        stormList = []
        for basin in basinList:
            basinDict = StormNames.NameDict[basin]
            if basin in ["Atlantic", "Eastern Pacific"]:
                listNum = currentYear % len(basinDict)
                basinStorms = StormNames.NameDict[basin][listNum]
                # Add the supplemental list for this basin
                suppList = StormNames.NameDict.get(basin + "_Supplemental", None)
                if suppList:
                    basinStorms += suppList
            else:
                basinStorms = self.makeStormList(basinDict)
            stormList += basinStorms
        return stormList

    def checkGUISelections(self, varDict, bogusStormName, existingStormLabel,
                                           newStormLabel):
        """
        Checks all the GUI selections to ensure only one storm name is selected.
        Returns the name of the selected storm, if valid. Returns a non-null
        message if GUI selection were incorrect.
        """
        message = "Please select ONE and ONLY ONE storm"
        stormName = ""
        # Check the varDict and define the new and existing storm names.
        if not varDict[newStormLabel]:
            newStormName = ""
        elif len(varDict[newStormLabel]) > 1:
            return stormName, message
        else:
            newStormName = varDict[newStormLabel][0]

        if existingStormLabel not in varDict or not varDict[existingStormLabel]:
            existingStormName = ""
        elif len(varDict[existingStormLabel]) > 1:
            return stormName, message
        else:
            existingStormName = varDict[existingStormLabel][0]

        # Now check to see that one and only one is define
        if (newStormName == bogusStormName and existingStormName == bogusStormName) \
            or (newStormName == "" and existingStormName == "") \
            or (newStormName != "" and existingStormName != ""):
            return stormName, message

        stormName = newStormName
        if stormName == "":
            stormName = existingStormName
        message = ""

        return stormName, message

    def askAreYouSure(self, stormNumber, pil):
        """
        Pops a dialog that asks if the user wants to specify a used stormID.
        Returns True if they wish to continue.
        """
        if pil is not None:
            question = f"Bin:  {pil}  and stormNumber: {stormNumber}" + \
                       "  are out of sequence.\nUse the StormNumber anyway?"
        else:
            question = f"StormNumber:  {stormNumber}  may have been used before.\n" + \
                       "Are you sure you want to use this storm number?"
        variableList = []
        variableList.append((question, "No", "radio", ["Yes", "No"]))

        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList(
            f'"{stormNumber}" is a previously used storm number',
            variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            return False

        if varDict[question] == "Yes":
            return True

        return False

    def saveJSONFileToTextProduct(self, bulletin, siteID):
        """
        Runs a script that save the specified JSON file to a text product.
        Then it is automatically sent to the WFOs.
        """
        mode = " operational "
        if self.gfeOperatingMode() == "PRACTICE":
            mode = " practice "
        execStr = os.path.join(self._path, "scripts", f"nhc_pushJsonFile.sh MIAJSN{bulletin}{mode}{siteID}")
        os.system(execStr)
        return

    def filterStormList(self, stormList, siteID):
        """
        Filters storm list by looking at valid stormNames for this siteID.
        """
        pilList = self.getPILList(siteID)
        filteredStormList = [storm for storm in stormList if storm["pil"] in pilList]
        return filteredStormList

    def __to_qualname(self, list):
        return '[' + ', '.join([getattr(x, "__qualname__", "None") for x in list]) + ']'

    def convert_to_words(self, value, case=str.lower):
        """
        Converts an integer value to the textual representation of the value
            Parameters:
                value:   integer in the range 0-99
                case:    optional case conversion function [str.upper, str.lower, str.title]
                         defaults to str.lower
            Returns:
                String containing the textual representation of value
        """
        if value not in valid_range:
            raise ValueError(f"{value} not in supported range: {valid_range[0]}-{valid_range[-1]}")
        if case not in valid_cases:
            raise ValueError(f"case must be one of {self.__to_qualname(valid_cases)}")
        if value < len(single_words):
            return case(single_words[value])
        tens = value // 10
        units = value % 10
        if units:
            return case("-".join([tens_multiples[tens], single_words[units]]))
        return case(tens_multiples[tens])

    def is_numeric_words(self, s, case_check=None):
        """
        Check if a string contains the textual representation of an integer in the range 0-99
            Parameters:
                s:            string to be checked
                case_check:   optional case checking function [None, str.isupper, str.islower, str.istitle]
                              defaults to None (case insensitive)
            Returns:
                True if the string is a valid textual representation of an integer in the range 0-99
                using the specified case
        """
        if case_check not in valid_case_checks:
            raise ValueError(f"case_check must be one of {self.__to_qualname(valid_case_checks)}")
        if isinstance(s, str):
            if case_check and not case_check(s):
                return False
            s = s.lower()
            words = s.split("-")
            if len(words) == 2:
                return words[0] in tens_multiples and words[1] in single_words[1:10]
            elif len(words) == 1:
                return s in single_words or s in tens_multiples
        return False

    def execute(self):
        #  Get the current UTC year - two digits only
        curYear = self._gmtime().timetuple().tm_year % 100
        siteID = self.getSiteID()
        # Get list of available storms
        stormList = list(self.fetchStormInfo().values())
        # Purge old JSON files
        stormList = self.removeOldJSONFiles(stormList)

        stormList = self.filterStormList(stormList, siteID)

        # Get info on what storm this is
        bogusStormName = "ZYXWWXYZ"  # Define a bogus storm so none will be defaulted

        stormNameList = self.getStormNameList(siteID, curYear)

        self._activeStormNames = []
        for storm in stormList:
            if storm.get("stormID", None) is None:
                self.statusBarMsg("StormID not found in stormInfo.", "S")

            self._activeStormNames.append(storm["stormName"] + ":" + storm["stormID"])
        self._activeStormNames.sort()

        # If there are active storms, put the next name on top
        stormNameList = self.shuffleStormList(stormNameList)
        # Ensure "None" is on top
        if "None" in stormNameList:
            stormNameList.remove("None")
            stormNameList.insert(0, "None")

        variableList = []
        existingStormLabel = "   Select an \nExisting Storm\n"
        if len(self._activeStormNames) > 0:
            variableList.append((existingStormLabel, [], "check", self._activeStormNames))

        # Remove any active storm names
        for storm in stormList:
            stormName = storm["stormName"]
            if stormName in stormNameList:
                stormNameList.remove(stormName)

        # Add available storm names to GUI
        newStormLabel = "Select a New Storm\n"
        variableList.append((newStormLabel, [], "check", stormNameList))

        # Display the GUI
        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList(
            "Set Advisory Information", variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            self.cancel()

        # Validate the GUI selections
        stormName, response = self.checkGUISelections(varDict, bogusStormName,
                                existingStormLabel, newStormLabel)

        if response != "":
            self.statusBarMsg("Please select ONE and ONLY one storm.", "S")
            return

        if ":" in stormName:
            stormName = stormName[:stormName.index(":")]

        # Build the Variable Lists dynamically based on the chosen storm
        # If New, make default GUI
        newStorm = False
        pilList = self.getPILList(siteID)
        if stormName in stormNameList:  # New storm
            newStorm = True
            variableList = []
            variableList.append(("AWIPS bin number", "", "radio", pilList))
            variableList.append(("Storm Type", "Tropical Storm", "radio",
                                 ["Potential Tropical Cyclone", "Subtropical Depression", "Subtropical Storm",
                                  "Tropical Depression", "Tropical Storm", "Hurricane", "Post-Tropical Cyclone", "Remnants of"]))
            variableList.append(("Other Storm Name (e.g. Alpha but NOT Three)", "", "alphaNumeric"))
            variableList.append(("Storm Number", 1, "scale", [1, self.maxStorms()], 1))
            variableList.append(("Advisory Type" , "Routine", "radio",
                                 ["Routine", "Special", "Intermediate"]))
            variableList.append(("Advisory Number" , "", "alphaNumeric"))

        # If existing storm, build GUI using JSON file info
        else:
            for sDict in stormList:
                if sDict["stormName"] == stormName:
                    PIL = sDict["pil"]
                    stormType = sDict["stormType"]
                    stormNum = sDict["stormNumber"]
                    advisoryType = sDict["advisoryType"]
                    advisoryNum = sDict["advisoryNumber"]
                    stormID = sDict["stormID"]

            variableList = []
            variableList.append(("AWIPS bin number", PIL, "radio", [PIL]))
            variableList.append(("Storm Type", stormType, "radio",
                                 ["Potential Tropical Cyclone", "Subtropical Depression", "Subtropical Storm",
                                  "Tropical Depression", "Tropical Storm", "Hurricane", "Post-Tropical Cyclone", "Remnants of"]))
            if self.is_numeric_words(stormName, case_check=str.istitle):
                variableList.append(("Storm Name", "None", "radio", stormNameList))
            elif stormName not in stormNameList:
                variableList.append(("Other Storm Name (e.g. Alpha but NOT Three)", stormName, "alphaNumeric"))
            else:
                variableList.append(("Storm Name", stormName, "radio", [stormName]))
            variableList.append(("Storm Number", stormNum, "radio", [stormNum]))
            variableList.append(("Advisory Type" , advisoryType , "radio",
                                 ["Routine", "Special", "Intermediate"]))
            variableList.append(("Advisory Number" , advisoryNum, "alphaNumeric"))
            variableList.append(("StormID: " + stormID, "", "label"))

        # Display the GUI
        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList(
            "Set Advisory Information for " + stormName, variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            self.cancel()

        #  Collect all the info provided by the forecaster
        pil = varDict["AWIPS bin number"].strip()
        stormType = varDict["Storm Type"].strip()
        if "Other Storm Name (e.g. Alpha but NOT Three)" in varDict:
            otherStormName = varDict["Other Storm Name (e.g. Alpha but NOT Three)"].strip()
        else:
            otherStormName = ""

        if otherStormName:
            stormName = otherStormName
        elif "Storm Name" in varDict:
            stormName = varDict["Storm Name"]

        # QC the storm number but only for NHC
        stormNumber = int(varDict["Storm Number"])
        # Special rules for "AT" storms
        if "AT" in pil:
            divisor = len(self._basinBins["Atlantic"])
            try:
                pilNumber = int(pil[-1])
            except:
                self.statusBarMsg("You did not provide a correct bin. Please rerun StormInfo.", "U")
                return
            if (stormNumber % divisor) != pilNumber:
                if not self.askAreYouSure(stormNumber, pil):
                    return

        if stormName == "None":
            stormName = self.convert_to_words(int(stormNumber), case=str.title)

        advisoryType = varDict["Advisory Type"].strip()

        try:
            advisoryNumber = varDict["Advisory Number"].strip()
        except:
            advisoryNumber = ""

        if not advisoryNumber:
            self.statusBarMsg("The advisory number is missing. Please rerun StormInfo.", "S")
            return

        if advisoryType == "Intermediate":
            advisoryNumber += "A"

        stormID = self.makeStormID(pil, stormNumber)
        if not newStorm:
            if advisoryNumber == advisoryNum:
                self.statusBarMsg("You did not increment the advisory number. Make sure that is what you want. Proceeded without changing Adv number", "A")
        else:  # It's a new storm. Save the stormID in the history
            stormIDHistory = self.stormIDHistory()
            # Check to see if the stormNumber has been used before for this basin.
            if stormID in stormIDHistory:
                if not self.askAreYouSure(stormNumber, None):
                    return

            self.updateStormIDHistory(stormID)

        #  Open the file to store all the info for this particular storm
        stormDict = {}
        # First find the storm we're working on and start with that to preserve other data
        for stormInfo in stormList:
            if stormInfo["stormName"] == stormName:
                stormDict = stormInfo
        # Over write some of the values
        advisoryNumber = advisoryNumber.replace("\n", "")
        advisoryNumber = advisoryNumber.replace(" ", "")
        stormDict["stormType"] = stormType
        stormDict["stormName"] = stormName
        stormDict["stormNumber"] = stormNumber
        stormDict["advisoryType"] = advisoryType
        stormDict["advisoryNumber"] = advisoryNumber
        stormDict["pil"] = pil
        stormDict["stormID"] = stormID
        # Get the time and round to the nearest hour
        hour = 3600
        now = int((self._gmtime().unixTime() + hour / 2) / hour) * hour
        stormDict["advisoryTime"] = now

        #  Save the info for this storm
        self.saveAdvisory(pil, stormDict, siteID)

        self.saveJSONFileToTextProduct(pil, siteID)

        if siteID not in self.NHCSites():
            self.statusBarMsg(f"{pil} successfully saved.", "R")
