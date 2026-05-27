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
# SOFTWARE HISTORY
#
# Date         Ticket#  Engineer    Description
# ------------ -------- ----------- ------------------------------------------
# Apr 13, 2020 21020    tlefebvr    Removed empty tags.
# Apr 13, 2020 21020    tlefebvr    Slight adjustment to output format.
# May  6, 2020 21020    tlefebvr    Code clean-up
# May 27, 2020 21020    tlefebvr    Re-implemented using ElementTree
# May 28, 2020 21020    tlefebvr    More code review changes.
# Jun  3, 2020 21020    tlefebvr    Moved PATH definition to top of module.
# Jul 23, 2020 21020    tlefebvr    Added a time string to the filename.
# Aug 17, 2020 21020    tlefebvr    Changed path to include storm name.
# May 10, 2021 22033    tlefebvr    Changed data file location to edex/data/share.
# May 13, 2021 22033    tlefebvr    Now using WindWWUtils fetchStormInfo for consistency.
# Jul 29, 2021 22531    tlefebvr    Final cleanup before check-in.
# Aug 24, 2021 22531    tlefebvr    Modified logic around makedir for Py2 and Py3 compatibility.
# Apr 13, 2022 22531    tlefebvr    Made a few changes for Python3 compatibility.
# Sep 29, 2023 2036298  santos/composano/white Fixes post 21.4.1-13 baseline
#
################################################################################

MenuItems = ["None"]

import SmartScript
import os
import errno
import TropicalUtility
import WindWWUtils
import xml.etree.ElementTree as et
from xml.etree.ElementTree import ElementTree


class Procedure(TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._dbss = dbss
        # Instantiate the WindWWUtils module
        self._WindWWUtils = WindWWUtils.WindWWUtils(self._dbss)
        self._path = self._WindWWUtils.getDataFilePath()

    def getAdvisoryNames(self):
        """
        Fetches all of the advisory names.
        """
        fileNames = self._getStormAdvisoryNames()  # fetch the JSON fileNames
        # Strip the .json
        finalList = [fileName[:-5] for fileName in fileNames]

        return finalList

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
        self._stormInfoDict = self._WindWWUtils.fetchStormInfo()
        for advisory in self._stormInfoDict:
            stormName = self._stormInfoDict[advisory]["stormName"]
            lastModified = self._stormInfoDict[advisory]["lastModified"]
            advisoryNumber = self._stormInfoDict[advisory]["advisoryNumber"]
            pil = self._stormInfoDict[advisory]["pil"]
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
                        phenItem = et.SubElement(stormData, "WWType").text = phen
                        sigItem = et.SubElement(stormData, "WWCat").text = sig
                        pointsItem = et.SubElement(stormData, "WWPoints").text = latLonStr

            stormDataStr = et.tostring(stormData)

            # Make the directory path
            practiceMode = self.gfeOperatingMode() == "PRACTICE"
            filepath = os.path.join(self._path, "xml", "Practice" if practiceMode else "", stormName)
            try:
                os.makedirs(filepath)
            except OSError as e:
                if e.errno != errno.EEXIST:
                    raise
            pathname = os.path.join(filepath, f"{pil}_{advisoryNumber}")
            ElementTree(stormData).write(pathname, encoding="utf-8", xml_declaration=True)

            # Display a message in CAVE
            self.statusBarMsg("Procedure completed. Produced an .xml file for wwgraph.", "A")
