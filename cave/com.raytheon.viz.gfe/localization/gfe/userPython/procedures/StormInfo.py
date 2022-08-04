# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# StormInfo - Version 4.0
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
# Aug  5, 2019            TLef        Modified to Support Wind Recommender as well
# Apr 22, 2020            tlefebvre   Old JSON files are now purged when older than
#                                     a day. JSON file written to all active sites
#                                     to keep them consistent.
# May  1, 2020  22033     tlefebvre   Added code to support CPHC forecasts
# May  5  2020  22033     tlefebvre   Added WP bins to bin list.
# May  7  2020  22033     tlefebvre   Fixed bug that failed to update stormName when
#                                     upgrading to named storm.
# May 11  2020  22033     tlefebvre   Added code to check stormID to ensure no
#                                     duplcates.
# May 12  2020  22033     tlefebvre   Added dialog to ask if storm number should be
#                                     reused.
# May 13  2020  22033     tlefebvre   Added method to save JSON file to text product
#                                     so WFOs get the information. Plus other tweaks.
# May 13  2020  22033     tlefebvre   Active storms limited to those in the local basin.
# May 14  2020  22033     tlefebvre   Modified to use WWUtil ***Sites methods.
# May 15  2020  22033     tlefebvre   Fixed issue with Numbered storm names not
#                                     appearing. Third try.
# May 18  2020  22033     tlefebvre   Filtered stormNames by siteID.
# May 19  2020  22033     tlefebvre   Fixed storm filtering bug.
# May 21  2020  22033     tlefebvre   Addressed code review comments.
# May 27  2020  22033     tlefebvre   Added StormID to existing storm GUI.
# May 28  2020  22033     tlefebvre   Addressed code review comment.
# May 28  2020  22033     tlefebvre   Added stormID to stormName label
# May 29  2020  22033     tlefebvre   Refactored makeStormID into WindWWUtils.
# May 30  2020  22033     tlefebvre   Fixed bug in filtering active storms.
#
########################################################################

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##
import ProcessVariableList
import StormNames
import TropicalUtility
import WindWWUtils
import LocalizationSupport
import os

MenuItems = ["None"]

class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._WindWWUtils = WindWWUtils.WindWWUtils(self._dbss)

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
        Currently active storms are removed as they are in use.
        """ 
        pilList = []
        if siteID in self._WindWWUtils.NHCSites():
            pilList = self._basinBins["Atlantic"] + self._basinBins["Eastern Pacific"]
            return pilList
        elif siteID in self._WindWWUtils.HFOSites():
            pilList = self._basinBins["Central Pacific"] + self._basinBins["Western Pacific"]
            return pilList
        elif siteID in self._WindWWUtils.GUMSites():
            pilList = self._basinBins["Western Pacific"]
            return pilList
        else:
            self.statusBarMsg("This tool is not configured for " + siteID, "S")
            return []

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
        stormList = []
        if siteID in self._WindWWUtils.NHCSites():
            basinList = ["Atlantic", "Eastern Pacific"]
            for basin in basinList:
                listNum = currentYear % len(StormNames.NameDict[basin])
                stormList += StormNames.NameDict[basin][listNum]
            return stormList
        elif siteID in self._WindWWUtils.HFOSites():
            basinDict = StormNames.NameDict["Central Pacific"]
            stormList = self.makeStormList(basinDict)
            return stormList
        elif siteID in self._WindWWUtils.GUMSites():
            basinDict = StormNames.NameDict["Western Pacific"]
            stormList = self.makeStormList(basinDict)
            return stormList
        else:
            self.statusBarMsg("This tool is not configured for " + siteID, "S")

        return []

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

    def askAreYouSure(self, stormNumber):
        """
        Pops a dialog that asks if the user wants to specify a used stormID.
        Returns True if they wish to continue. 
        """
        variableList = []
        question = "Are you sure you want to reuse this storm number?"
        variableList.append((question, "No", "radio", ["Yes", "No"]))
        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList(
            '"' + str(stormNumber) + '"' + ' is a previously used storm number',
            variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            return False

        if varDict[question] == "Yes":
            return True

        return False

    def saveJSONFileToTextProduct(self, bulletin):
        """
        Runs a script that save the specified JSON file to a text product.
        Then it is automatically sent to the WFOs.  
        """
        mode = " operational"
        if self.gfeOperatingMode() == "PRACTICE":
            mode = " practice"               
        execStr = "/localapps/runtime/RecommendWindWatchWarning/nhc_pushJsonFile.sh MIAJSN" + bulletin + mode
        os.system(execStr)
        return
    
    def filterStormList(self, stormList, siteID, currentYear):
        """
        Filters storm list by looking at valid stormNames for this siteID.
        """
        basinList = []
        if siteID in self._WindWWUtils.NHCSites():
            basinList = ["AT", "EP"]
        elif siteID in self._WindWWUtils.HFOSites():
            basinList = ["CP"]
        elif siteID in self._WindWWUtils.GUMSites():
            basinList = ["WP"]

        filteredStormList = [storm for storm in stormList if storm["pil"][:2] in basinList]
        
        return filteredStormList

    def execute(self):
        #  Get the current UTC year - two digits only
        curYear = self._gmtime().timetuple().tm_year % 100
        siteID = self.getSiteID()
        # Get list of available storms
        stormList = self.extractStormInfo(filterATOnly=False)
        # Purge old JSON files
        stormList = self.removeOldJSONFiles(stormList)

        stormList = self.filterStormList(stormList, siteID, curYear)

        # Get info on what storm this is
        bogusStormName = "ZYXWWXYZ" # Define a bogus storm so none will be defaulted

        Numbers = ["One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight",
                   "Nine", "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen",
                   "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen", "Twenty",
                   "Twenty-One", "Twenty-Two", "Twenty-Three", "Twenty-Four", "Twenty-Five"]

        stormNameList = self.getStormNameList(siteID, curYear)
        
        self._activeStormNames = []
        for storm in stormList:
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

        if response is not "":
            self.statusBarMsg("Please select ONE and ONLY one storm.", "S")
            return
        
        if ":" in stormName:
            stormName = stormName[:stormName.index(":")]
        self._basinBins = self._WindWWUtils._basinBins

        # Build the Variable Lists dynamically based on the chosen storm
        # If New, make default GUI
        newStorm = False
        if stormName in stormNameList: # New storm
            newStorm = True
            pilList = self.getPILList(siteID)
            variableList = []
            variableList.append(("AWIPS bin number", "", "radio", pilList))
            variableList.append(("Storm Type", "Tropical Storm", "radio",
                                 ["Potential Tropical Cyclone", "Subtropical Depression", "Subtropical Storm",
                                  "Tropical Depression", "Tropical Storm", "Hurricane", "Post-Tropical Cyclone"]))
            variableList.append(("Other Storm Name (e.g. Alpha but NOT Three)", "", "alphaNumeric"))
            variableList.append(("Storm Number", 1, "scale", [1, 25], 1))
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
                                  "Tropical Depression", "Tropical Storm", "Hurricane", "Post-Tropical Cyclone"]))
            if stormName in Numbers:
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
        if siteID in self._WindWWUtils.NHCSites():
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

        if not advisoryNumber:
            self.statusBarMsg("The advisory number is missing. Please rerun StormInfo.", "S")
            return

        if advisoryType == "Intermediate":
            advisoryNumber += "A"

        stormID = ""
        if not newStorm:
            if advisoryNumber == advisoryNum:
                self.statusBarMsg("You did not increment the advisory number. Make sure that is what you want. Proceeded without changing Adv number", "A")
        else:  # It's a new storm. Save the stormID in the history
            stormID = self._WindWWUtils.makeStormID(pil, stormNumber)
            stormIDHistory = self._WindWWUtils.stormIDHistory()
            # Check to see if the stormNumber has been used before for this basin.
            if stormID in stormIDHistory:
                if not self.askAreYouSure(stormNumber):
                    return

            self._WindWWUtils.updateStormIDHistory(stormID)

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
        if newStorm:
            stormDict["stormID"] = stormID

        #  Save the info for this storm
        self._saveAdvisory(pil, stormDict)

        self.saveJSONFileToTextProduct(pil)
        
        if siteID not in self._WindWWUtils.NHCSites():
            self.statusBarMsg(pil + " successfully saved.", "R")
