# ----------------------------------------------------------------------------
# ESF
# Hydrologic Outlook
#
# Author: NWS Formatter Task Force
# ----------------------------------------------------------------------------
##
#
# SOFTWARE HISTORY
# Date          Ticket#     Engineer    Description
# ------------  ----------  ----------- --------------------------
# 03/15/2020    DCS21339     NFTF        Add NFTF ESF to baseline
# 01/11/2021    DR22424      NFTF        Replace _prevProdPIL with _textdbPil in _makeProduct
#
##
# -------------------------------------------------------------------------
# Example Output:
# Refer to the NWS 10-922 Directive for further information.
# -------------------------------------------------------------------------
#
# Included Methods
#
# ---------------------------------------------------------------------
#
#     _preProcessProduct:
#        ESF version of GenericReport._preProcessProduct.
#
#        Removed string manipulation.
#
#     _preProcessArea:
#        ESF version of GenericReport._preProcessArea.
#
#        Handles product name.
#
#     _makeProduct:
#        ESF version of GenericReport._makeProduct.
#
#        Creates a placeholder tag or imports previous version.
#
#     _postProcessProduct:
#        ESF version of GenericReport._postProcessProduct.
#
#        Handles word-wrapping, line feeds.
#
# ---------------------------------------------------------------------
import re
import GenericReport


class TextProduct(GenericReport.TextProduct):
    """ESF TextProduct base class"""

    VariableList = [
        (("Include Previous Product Text?", "includeOldText"), "No", "radio", ["Yes", "No"]),
        (
            ("Product Name:", "productName"),
            "Hydrologic Outlook",
            "radio",
            [
                "Hydrologic Outlook",
                "Water Supply Outlook",
                "Probabilistic Hydrologic Outlook",
                "Winter/Spring Flood Potential Outlook",
            ],
        ),
    ]

    Definition = {
        "type": "smart",
        "displayName": None,  # for Product Generation Menu
        "database": "Official",  # Source database. "Official", "Fcst", or "ISC"
        "outputFile": "{prddir}/TEXT/ESF.txt",
        "debug": 0,
        "areaType": "FIPS",  # default UGC type
        # Edit Areas: Create Combinations file with edit area combinations.
        "defaultEditAreas": "Combinations_ESF_<site>",
        "showZoneCombiner": 1,
        # product identifiers
        "productName": "Hydrologic Outlook",  # product name
        "fullStationID": "<fullStationID>",  # 4 letter station ID
        "wmoID": "<wmoID>",  # WMO code
        "wfoCityState": "<wfoCityState>",  # Location of WFO
        "pil": "<pil>",  # product pil
        "textdbPil": "<textdbPil>",  # Product ID for storing to AWIPS text database.
        "awipsWANPil": "<awipsWANPil>",  # Product ID for transmitting to AWIPS WAN.
        "wfoSiteID": "<site>",
        # Area Dictionary -- Descriptive information about zones
        "areaDictionary": "AreaDictionary",
        # Language
        "language": "english",
        "lineLength": 66,  # Maximum line length
        "includeCities": 0,  # Cities included in area header
        "cityDescriptor": "Including the cities of",
        "includeZoneNames": 0,  # Zone names will be included in the area header
        "includeIssueTime": 0,  # This should be set to zero
        "singleComboOnly": 1,  # Used for non-segmented products
        "purgeTime": 12,  # Expiration in hours
    }

    # DO NOT OVERRIDE THE FOLLOWING CODE BLOCK
    # It is necessary to properly set for zones or counties
    if "<site>" in ["AFG", "AJK", "AICE", "ALU", "AER", "ACR", "AFC", "HFO", "GUM", "PPG"]:
        Definition["areaType"] = "ZONES"

    if Definition["areaType"] == "FIPS":
        # Name of map background for creating Combinations
        Definition["mapNameForCombinations"] = "FIPS_<site>"
    else:
        # Name of map background for creating Combinations
        Definition["mapNameForCombinations"] = "Zones_<site>"

    def __init__(self):
        GenericReport.TextProduct.__init__(self)

    def _preProcessProduct(self, fcst, argDict):
        """ESF version of GenericReport._preProcessProduct.

        Removed string manipulation.
        """

        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        """ESF version of GenericReport._preProcessArea.

        Handles product name.
        """

        # First, generate WMO lines
        fcst = "{} {} {}\n{}\n".format(
            self._wmoID, self._fullStationID, self._ddhhmmTime, self._pil
        )

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
        fcst += areaHeader + "\n"

        # Last, add the product name/time lines
        issuedByString = self.getIssuedByString()
        productName = self.checkTestMode(argDict, self._productName)
        fcst += "{}\nNational Weather Service {}\n{}{}\n\n".format(
            productName, self._wfoCityState, issuedByString, self._timeLabel
        )

        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        """ESF version of GenericReport._makeProduct.

        Creates a placeholder tag or imports previous version."""

        # Start the product from scratch
        if self._includeOldText == "No":
            fcst += (
                "|* ...OPTIONAL HEADLINE CONCERNING HYDROLOGIC CONDITIONS... *|\n\n"
                "|* Insert Forecast and Narrative Hydrologic Information Here *|\n\n"
            )

        # self._includeOldText == "Yes" - Start the product from a previous version
        else:

            # Get previous product
            prevProd = self.getPreviousProduct(self._textdbPil)

            # If we actually found the previous text
            if prevProd:

                # Merge the forecasts
                prod = re.compile(r"[0-9][ ][0-9][0-9][0-9][0-9]\n\n(.*?)\$\$", re.DOTALL)
                product = prod.findall(prevProd)
                fcst += "\n\n" + product[0].strip() + "\n\n"

        return fcst

    def _postProcessProduct(self, fcst, argDict):
        """ESF version of GenericReport._postProcessProduct.

        Handles word-wrapping, line feeds.
        """

        # Clean up multiple line feeds
        fixMultiLF = re.compile(r"(\n\n)\n*", re.DOTALL)
        fcst = fixMultiLF.sub(r"\1", fcst)

        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "...", "-"])
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")

        return fcst
