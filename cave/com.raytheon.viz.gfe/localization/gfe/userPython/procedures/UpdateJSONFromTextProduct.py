# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# UpdateJSONFromTextProduct.py
#
# Author: lefebvre
#
# May   30, 2020   22033      tlefebvr   Added extractJSON text.
# May   13, 2021   22033      tlefebvr   Changed saveAdvisory to use common version.
# Jul   29, 2021   22531      tlefebvr   Final code clean-up before check-in.
# Sep    2, 2021   22531      tlefebvr   Updated interface to getTextProcuctFromDB.
# Jan    6, 2022   22531      tlefebvr   Calling WindWWUtils version of getTextProdFromDB.
# Jan   20, 2022   22531      tlefebvr   Fixed issue with text PIL in PRACTICE mode.
# Jan   28, 2022   22531      tlefebvr   Fixed text decoding into JSON object.
# Apr   13, 2022   22531      tlefebvr   Made a few changes for Python3 compatibility.
#----------------------------------------------------------------------------

MenuItems = ["None"]

import time
import SmartScript
import TropicalUtility
import WindWWUtils
import json
import ProcessVariableList


class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._dbss = dbss

        self._WindWWUtils = WindWWUtils.WindWWUtils(self._dbss)

    def stripHeader(self, textList):
        """
        Strip the header text from the text product.
        """
        startStr = "{\n"
        if startStr in textList:
            startIndex = textList.index(startStr)
            finalText = textList[startIndex:]
        else:
            finalText = ""
        return finalText

    def execute(self, editArea, timeRange, varDict):

        self._hazardOrder = ["<None>", "TR.A", "HU.A", "TR.W", "TR.W^HU.A", "HU.W"]
#         stormInfoList = self._WindWWUtils.getStormInfoDicts()
#         self._stormInfoDict = {}
#         for stormInfo in stormInfoList:
#             self._stormInfoDict[stormInfo["pil"]] = stormInfo
        # Make a GUI listing all the active bins
        siteID = self.getSiteID()
        pilList = []
        for basin in self.forecastBasins(siteID):
            pilList += self._basinBins[basin]
        variableList = [("Bin number", "", "radio", pilList)]
        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList(
            "Select a Bin Number:", variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            return

        selectedBin = varDict["Bin number"]

        if self.gfeOperatingMode() in ["PRACTICE", "TEST"]:
            productID = "MIAJSNWK" + selectedBin[-1]
        else:
            productID = "MIAJSN" + selectedBin

        textProduct = self._WindWWUtils.getTextProductFromDB(productID, mode="OPERATIONAL")
        if not textProduct:
            self.statusBarMsg(productID + " not found in text database", "S")
            return

        textProduct = self.stripHeader(textProduct)
        # Concatenate into one large string
        textProduct = "".join(textProduct)
        # Convert text product to JSON dict
        try:
            textStormInfo = json.loads(textProduct)
            textLastModified = textStormInfo["lastModified"]
        except:
            self.statusBarMsg("Error converting text product " + productID + " to JSON.", "S")
            return

        # Only bother with recent bulletins
        elapsed = self._gmtime().unixTime() - textLastModified
        if elapsed > 3 * 3600:
            print("The product for", selectedBin, "is too old:", time.asctime(time.gmtime(textLastModified)))
            return

        # Fetch the stormInfo we have in the JSON files.
        stormInfoList = self._WindWWUtils.getStormInfoDicts()
        stormInfoDict = {}
        for stormInfo in stormInfoList:
            stormInfoDict[stormInfo["pil"]] = stormInfo

        saveJSON = True
        if selectedBin in stormInfoDict:
            jsonLastModified = stormInfoDict[selectedBin]["lastModified"]
            if jsonLastModified > textLastModified:
                saveJSON = False

        # Save if needed
        if saveJSON:
            siteID = self.getSiteID()
            self._WindWWUtils.saveAdvisory(selectedBin, textStormInfo, siteID)
            self.statusBarMsg(selectedBin + " has been saved to the server.", "S")
        else:
            self.statusBarMsg("Advisory info in the text product is older than the JSON files. Nothing saved.", "A")
