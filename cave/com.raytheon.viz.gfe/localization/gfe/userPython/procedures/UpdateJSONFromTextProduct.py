# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# UpdateJSONFromTextProduct.py
#
# Author: lefebvre
# ----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ----------------------------------------
# May 30, 2020 22033      tlefebvr    Added extractJSON text.
# ----------------------------------------------------------------------------

MenuItems = ["Edit"]
import LogStream, time
from math import *

import time
import AbsTime
import SmartScript
import TropicalUtility
import WindWWUtils
import json

class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)
        self._dbss = dbss
        
        self._WindWWUtils = WindWWUtils.WindWWUtils(self._dbss)

    def extractJSONText(self, textList):
        """
        Strip the header text from the text product.
        """
        startStr = "{\n"
        if startStr in textList:
            startIndex = textList.index(startStr)
            finalText = textList[startIndex:]
        else:
            self.statusBarMsg("Error parsing JSON text from text product.", "S")
            finalText = ""

        return finalText

    def execute(self, editArea, timeRange, varDict):
        
        siteID = self.getSiteID()
        
        forecastBasins = self._WindWWUtils.forecastBasins(siteID)
        
        binList = []
        for basin in forecastBasins:
            binList += self._WindWWUtils._basinBins[basin]
            
        self._hazardOrder =  ["<None>", "TR.A", "HU.A", "TR.W", "TR.W^HU.A", "HU.W"]
        self._stormInfoDict = self._WindWWUtils.fetchStormInfo(self._hazardOrder)

        for bin in binList:
            productID = "MIAJSM" + bin
            textProduct = self.getTextProductFromDB(productID)
            if not textProduct:
                continue
            
            textProduct = self.extractJSONText(textProduct)
            # Convert text product to JSON dict
            try:
                stormInfo = json.loads(textProduct)
                textLastModified = stormInfo["lastModified"]
            except:
                self.statusBarMsg("Error converting text product " + productID + " to JSON.", "S")
                continue

            # Only bother with recent bulletins
            elapsed = self._gmtime().unixTime() - textLastModified
            if elapsed > 3 * 3600:
                continue
            
            # Figure out if we need to save
            saveJSON = False
            if bin not in self._stormInfoDict:
                saveJSON = True
            else:
                jsonLastModified = self._stormInfoDict[bin]["lastModified"]
                if jsonLastModified < textLastModified:
                    saveJSON = True

            # Save if needed
            if saveJSON:
                self._saveAdvisory(bin, stormInfo)
                self.statusBarMsg(bin + " has been saved to the server.", "S")

