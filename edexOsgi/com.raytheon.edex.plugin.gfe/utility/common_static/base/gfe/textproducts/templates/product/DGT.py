# ----------------------------------------------------------------------------
# DGT
# Drought Information Statement
#
# Author: NWS Formatter Task Force
# ----------------------------------------------------------------------------
##
#
# SOFTWARE HISTORY
# Date            Ticket#        Engineer    Description
# ------------    ----------     ----------- --------------------------
# 03/15/2020       DCS21339       NFTF        Add NFTF DGT to baseline
# 08/12/2024       DCS2037810     NFTF        Update format per latest AFS requirements
#
##
# -------------------------------------------------------------------------
# Example Output:
# Refer to the NWS 10-1201 Directive for further information.
# -------------------------------------------------------------------------
#
# Included Methods
#
# ---------------------------------------------------------------------
#
#     _preProcessProduct:
#        DGT version of GenericReport._preProcessProduct.
#
#        This is an unsegmented product so don't do anything. The header will be added in
#        _preProcessArea.
#
#     _preProcessArea:
#        DGT version of GenericReport._preProcessArea.
#
#        Creates product header for an unsegmented product.
#
#     _makeProduct:
#        DGT version of GenericReport._makeProduct.
#
#     _postProcessProduct:
#        DGT version of GenericReport._postProcessProduct.
#
#        Handles word-wrapping, line feeds.
#
# ---------------------------------------------------------------------
import re
import GenericReport
import time
class TextProduct(GenericReport.TextProduct):
    VariableList = [
        (
            ("Include Previous Product Text?", "includeOldText"),
            "No",
            "radio",
            ["Yes", "No"],
        )
    ]
    Definition = {
        "type": "smart",
        "displayName": None,
        "database": "Official",  # Source database. "Official", "Fcst", or "ISC"
        "outputFile": "{prddir}/TEXT/DGT.txt",
        "debug": 0,
        "areaType": "FIPS",  # default UGC type
        # Edit Areas: Create Combinations file with edit area combinations.
        "defaultEditAreas": "Combinations_DGT_<site>",
        "includeCities": 0,  # Cities included in area header
        "cityDescriptor": "Including the cities of",
        "includeIssueTime": 0,
        "includeZoneNames": 0,  # Zone names will be included in the area header
        # product identifiers
        "productName": "Drought Information Statement",  # product name
        "fullStationID": "<fullStationID>",  # 4 letter station ID
        "wmoID": "<wmoID>",  # WMO code
        "wfoCityState": "<wfoCityState>",  # Location of WFO
        "pil": "<pil>",  # product pil
        "textdbPil": "<textdbPil>",  # Product ID for storing to AWIPS text database.
        "awipsWANPil": "<awipsWANPil>",  # Product ID for transmitting to AWIPS WAN.
        "wfoSiteID": "<site>",
        # Area Dictionary -- Descriptive information about zones
        "areaDictionary": "AreaDictionary",
        # 1 to cause zone combiner to display
        "showZoneCombiner": 1,
        # Use only a single zone combination (1 for non-segmented product, 0 - segmented)
        "singleComboOnly": 1,
        # Language
        "language": "english",
        "lineLength": 66,  # Maximum line length
        # Expiration
        "purgeTime": 336,  # Default Expiration in hours
        # Define the AWIPS text database PIL used to retrieve the last issued DGT.
        "prevProdPIL": "<textdbPil>",
    }
    # Set mapNameForCombinations properly for zones or counties
    if Definition["areaType"] == "FIPS":
        Definition["mapNameForCombinations"] = "FIPS_<site>"
    else:
        Definition["mapNameForCombinations"] = "Zones_<site>"
    def __init__(self):
        GenericReport.TextProduct.__init__(self)
    def _preProcessProduct(self, fcst, argDict):
        """DGT version of GenericReport._preProcessProduct.

        This is an unsegmented product so don't do anything. The header will be added in
        _preProcessArea."""
        return fcst
    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        """DGT version of GenericReport._preProcessArea.

        Creates product header for an unsegmented product."""
        # First, generate WMO lines
        fcst = f"{self._wmoID} {self._fullStationID} {self._ddhhmmTime}\n{self._pil}\n"
        # Next, add the non-segmented UGC data
        areaHeader = self.makeAreaHeader(
            argDict,
            areaLabel,
            self._issueTime,
            self._expireTime,
            self._areaDictionary,
            self._defaultEditAreas,
            cityDescriptor=self._cityDescriptor,
            includeCities=self._includeCities,
            includeZoneNames=self._includeZoneNames,
            includeIssueTime=self._includeIssueTime,
        )
        fcst += f"{areaHeader}\n"
        # Last, add the product name/time lines
        issuedByString = self.getIssuedByString()
        productName = self.checkTestMode(argDict, self._productName)
        fcst += (
            f"{productName}\nNational Weather Service {self._wfoCityState}\n"
            f"{issuedByString}{self._timeLabel}\n\n"
        )
        return fcst
    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        """
        DGT version of GenericReport._makeProduct.
        """
        # Start the product from scratch
        if getattr(self, "_includeOldText", "No").lower() == "yes":
            prevProd = self.getPreviousProduct(self._prevProdPIL)
            # If we actually found the previous text
            if prevProd:
                # Merge the forecasts
                prod = re.compile(
                    r"[0-9][ ][0-9][0-9][0-9][0-9]\n\n(.*?)[$$]", re.DOTALL
                )
                product = prod.findall(prevProd)
                if product:
                    fcst += product[0]
                    return fcst
        # Body of product
        bodyText = ""
        wfoCityState = getattr(self, "_wfoCityState", "|* City State *|")
        SID = getattr(self, "_wfoSiteID", " |* XXX *|")
        sid = SID.lower()
        strDate = time.strftime("%m%d%Y", time.localtime(time.time()))
        bodyText += (
            "For the latest Drought Information Statement from the "
            f"National Weather Service in {wfoCityState}, see: "
            f"www.weather.gov/media/{sid}/DGT/DGT_{SID}_|*{strDate}*|.pdf\n\n"
        )
        bodyText += (
            "For the latest accessible, text-only Drought Information Statement from the "
            f"National Weather Service in {wfoCityState}, see: "
            f"www.weather.gov/media/{sid}/DGT/DGT_{SID}_|*{strDate}*|.txt\n\n"
        )
        bodyText += (
            f"National Weather Service {wfoCityState} Drought Information Statement "
            f"web page: www.weather.gov/{sid}/DroughtInformationStatement"
        )
        # Do some text cleanup
        bodyText = self.endline(bodyText, linelength=self._lineLength)
        return fcst + bodyText
    def _postProcessProduct(self, fcst, argDict):
        """DGT version of GenericReport._postProcessProduct.

        Handles word-wrapping, line feeds."""
        # Clean up multiple line feeds
        fixMultiLF = re.compile(r"(\n\n)\n*", re.DOTALL)
        fcst = fixMultiLF.sub(r"\1", fcst)
        fcst = self.endline(
            fcst, linelength=self._lineLength, breakStr=[" ", "...", "-", ", "]
        )
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, f"{self._displayName} Complete")
        return fcst