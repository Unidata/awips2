# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# GenerateWindWWXML
#
# This tool generates an XML file based on the hazards defined in the JSON
# files. The XML file is source of the web graphic for NHC.
#
# April 13, 2020 21020      tlefebvr    Removed empty tags.
# April 13, 2020 21020      tlefebvr    Slight adjustment to output format.
# May    6, 2020 21020      tlefebvr    Code clean-up
# May   27, 2020 21020      tlefebvr    Re-implemented using ElementTree
# May   28, 2020 21020      tlefebvr    More code review changes.
# June   3, 2020 21020      tlefebvr    Moved PATH definition to top of module.
# 
################################################################################

MenuItems = ["Populate"]

import SmartScript
import TropicalUtility
import WindWWUtils
import xml.etree.ElementTree as et
from xml.etree.ElementTree import ElementTree

# XML files will be written to this directory
PATH = "/tmp/" 

class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._dbss = dbss
        # Instantiate the WindWWUtils module
        self._WindWWUtils = WindWWUtils.WindWWUtils(self._dbss)

    def getAdvisoryNames(self):
        """
        Fetches all of the advisory names.
        """
        fileNames = self._getStormAdvisoryNames() # fetch the JSON fileNames
        # Strip the .json
        finalList = [fileName[:-5] for fileName in fileNames]
        return finalList

    def fetchStormInfo(self):
        """
        Fetch all the storm info dictionaries.
        """
        stormInfoDictList = self._WindWWUtils.getStormInfoDicts()
        stormInfoDicts = {}
        for stormInfo in stormInfoDictList:
            # Add the Breakpoints key if we don't have it.
            if "Breakpoints" not in stormInfo:
                stormInfo["Breakpoints"] = {}

            stormInfoDicts[stormInfo["pil"]] = stormInfo

        return stormInfoDicts

    def parsePhenSig(self, hazard):
        """
        Decomposes the hazard key into phen and sig parts.
        """
        phenSigList = hazard.split("^")

        finalList = []
        for phenSig in phenSigList:
            parts = phenSig.split(".")
            phen = parts[0]
            sig = parts[1]
            finalList.append((phen, sig))

        return finalList

    def execute(self, editArea, timeRange, varDict):

        # Fetch the active advisory names from the JSON files.
        self._advisoryNames = sorted(self.getAdvisoryNames())
        if not self._advisoryNames:
            self.statusBarMsg("No Advisory files found. Please run StormInfo first.", "U")
            return

        # Fetch the storm information from the JSON files.        
        self._stormInfoDict = self.fetchStormInfo()
        for advisory in self._stormInfoDict:
            stormName = self._stormInfoDict[advisory]["stormName"]
            stormData = et.Element(stormName)
            if "latLonDict" not in self._stormInfoDict[advisory]:
                continue
            for hazard in self._stormInfoDict[advisory]["latLonDict"]:
                latLonSegments = self._stormInfoDict[advisory]["latLonDict"][hazard]
                if not latLonSegments:
                    continue
                phenSigList = self.parsePhenSig(hazard)
                for phen, sig in phenSigList:
                    
                    for segment in latLonSegments:
                        latLonStr = ""
                        for lat, lon in segment:   
                            latLonStr += str(lat) + " " + str(lon) + " "
                        # Define the SubElements and values
                        phenItem = et.SubElement(stormData, "WWType").text=phen
                        sigItem = et.SubElement(stormData, "WWCat").text=sig
                        pointsItem = et.SubElement(stormData, "WWPoints").text=latLonStr

            stormDataStr = et.tostring(stormData)
            pathname = PATH + stormName + ".xml"
            ElementTree(stormData).write(pathname, encoding="utf-8", xml_declaration=True)

