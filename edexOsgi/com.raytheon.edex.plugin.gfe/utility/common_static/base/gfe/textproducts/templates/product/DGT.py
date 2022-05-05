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
        (("Include Previous Product Text?", "includeOldText"), "No", "radio", ["Yes", "No"])
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
        # product specific definitions
        # "localWebsites" defines whatever local websites you would like to have in the
        # "RELATED WEB SITES" section, Python List
        "localWebsites": [],
        # "acknowledgements" defines any acknowledgements to local, regional or national agencies
        "acknowledgements": "",
        # "addressWFO": Your local WFO address that will be appended at the bottom of the product
        "addressWFO": "",
    }

    # Set mapNameForCombinations properly for zones or counties
    if Definition["areaType"] == "FIPS":
        # Name of map background for creating Combinations
        Definition["mapNameForCombinations"] = "FIPS_<site>"
    else:
        # Name of map background for creating Combinations
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
        """DGT version of GenericReport._makeProduct."""

        # Start the product from scratch
        if self._includeOldText == "No":

            # Set up mandatory sections

            # Headline
            fcst += "|* ...INSERT MANDATORY HEADLINE... *|\n\n"

            # Synopsis
            fcst += ".SYNOPSIS:\n\n"

            fcst += ".Drought intensity and extent:\n|*<Insert Text>*|\n\n"
            fcst += ".Hydrologic conditions:\n|*<Insert Text>*|\n\n"

            # Summary of impacts
            fcst += ".SUMMARY OF IMPACTS:\n\n|*<Insert Text>*|\n\n"

            # Drought Mitigation Actions
            fcst += ".DROUGHT MITIGATION ACTIONS:\n\n|*<Insert Text>*|\n\n"

            # Local Drought Outloook
            fcst += ".LOCAL DROUGHT OUTLOOK:\n\n|*<Insert Text>*|\n\n"

            # Next Issuance
            nextMonth = str(time.strftime("%A %B %-d", time.localtime(time.time() + 28 * 86400)))
            fcst += (
                ".NEXT ISSUANCE DATE:\n\nThis product will be updated {} or sooner if "
                "drought conditions change significantly.\n\n".format(nextMonth)
            )

            # Mandatory Drought Websites
            fcst += (
                ".RELATED WEB SITES:\n\nAdditional information on current drought "
                "conditions may be found at the following web addresses:\n"
                "US Drought Monitor: https://droughtmonitor.unl.edu\n"
                "US Drought Information System: https://www.drought.gov\n"
                "NOAA Drought Page: https://www.cpc.ncep.noaa.gov/products/Drought\n"
            )

            # Additional WFO Websites
            for site in self._localWebsites:
                fcst += site + "\n"

            # Mandatory River Websites
            fcst += (
                "\nAdditional water and river information:\n"
                "NWS: https://water.weather.gov\n"
                "OWP: https://water.noaa.gov\n"
                "US Geological Survey (USGS): https://water.usgs.gov\n"
                "US Army Corps of Engineers (USACE): https://www.usace.army.mil\n\n"
            )

            # Acknowledgments
            fcst += ".ACKNOWLEDGMENTS:\n\n" + self._acknowledgements + "\n\n"

            # Questions/Comments
            fcst += (
                ".CONTACT INFORMATION:\n\n"
                "If you have questions or comments about this Drought Information "
                "Statement, please contact:\n\n"
            ) + self._addressWFO

        else:  # self._includeOldText == "Yes" - Start the product from a previous version
            # Get previous product
            prevProd = self.getPreviousProduct(self._prevProdPIL)
            # If we actually found the previous text
            if prevProd != "":
                # Merge the forecasts
                prod = re.compile(r"[0-9][ ][0-9][0-9][0-9][0-9]\n\n(.*?)[$$]", re.DOTALL)
                product = prod.findall(prevProd)
                fcst += product[0]

        return fcst

    def _postProcessProduct(self, fcst, argDict):
        """DGT version of GenericReport._postProcessProduct.

         Handles word-wrapping, line feeds."""

        # Clean up multiple line feeds
        fixMultiLF = re.compile(r"(\n\n)\n*", re.DOTALL)
        fcst = fixMultiLF.sub(r"\1", fcst)

        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "...", "-", ", "])
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")

        return fcst
