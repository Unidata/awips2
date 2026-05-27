##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
#
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
#
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
#
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
# ----------------------------------------------------------------------------
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/15/2020      DCS21339      NFTF           Updated for HazSimp format
#
##
# This is a base file that is not intended to be overridden.
##

########################################################################
# Hazard_CFW.py
#
#
##########################################################################
import copy
import re

import GenericHazards


class TextProduct(GenericHazards.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition["displayName"] = "BaselineHazard_CFW_<MultiPil> (Coastal/LakeShore Flooding)"

    Definition["defaultEditAreas"] = "EditAreas_PublicZones_<site>_<MultiPil>"

    # Map background for creating Combinations
    Definition["mapNameForCombinations"] = "Zones_<site>"

    # Header configuration items
    Definition["productName"] = "Coastal Hazard Message"  # Warning! DO NOT CHANGE.
    # The productName gets substituted later in the formatter!

    Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
    Definition["wmoID"] = "<wmoID>"  # WMO ID
    Definition["pil"] = "<pil>"  # product pil
    # Definition["areaName"] = "Statename"  # Name of state, such as "Georgia"
    Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
    Definition["wfoCity"] = "<wfoCity>"  # WFO Name as it should appear in a text product
    Definition["textdbPil"] = "<textdbPil>"  # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"  # Product ID for transmitting to AWIPS WAN.
    Definition["outputFile"] = "{prddir}/TEXT/CFW_<MultiPil>.txt"
    Definition["bulletProd"] = 1  # If 1, the product has a bullet format
    Definition["hazSimpBullets"] = ["WHAT", "WHERE", "WHEN", "IMPACTS", "ADDITIONAL DETAILS"]

    # OPTIONAL CONFIGURATION ITEMS
    # Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
    # Definition["displayOutputDialog"] = 0  # If 1 will display results when finished
    # Definition["debug"] = 1
    # Definition["headlineEditAreaGroup"] = "Zones" # Name of EditAreaGroup for sampling headlines

    Definition["purgeTime"] = 8  # Maximum hours for expireTime from issueTime
    Definition["includeCities"] = 0  # Cities included in area header
    Definition["accurateCities"] = 0  # 1: cities are based on grids; 0: full list is included
    Definition["cityLocation"] = "CityLocation"  # City lat/lon dictionary to use
    # Definition["cityDescriptor"] = "Including the cities of"
    Definition["includeZoneNames"] = 1  # Zone names will be included in the area header
    Definition["lineLength"] = 66  # line length
    Definition["easPhrase"] = "URGENT - IMMEDIATE BROADCAST REQUESTED"
    Definition["includeOverviewHeadline"] = 0  # Policy no longer allows the Overview section
    Definition["includeOverview"] = 0  # Policy no longer allows the Overview section

    # Definition["hazardSamplingThreshold"] = (10, None)  # (%cov, #points)

    # Text to insert below the last $$ of the product. Use "" for no text.
    Definition["urlText"] = ""
    # multiple line example
    # Definition["urlText"] = ("For more information from the National Weather Service visit\n"
    #                          "http://weather.gov/saltlakecity")

    def __init__(self):
        GenericHazards.TextProduct.__init__(self)

    def allowedHazards(self):
        """Hazards allowed in CFW products."""

        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
            ("CF.W", allActions, "CoastalFlood"),           # COASTAL FLOOD WARNING
            ("CF.Y", allActions, "CoastalFlood"),           # COASTAL FLOOD ADVISORY
            ("CF.A", allActions, "CoastalFlood"),           # COASTAL FLOOD WATCH
            ("CF.S", allActions, "CoastalFloodStatement"),  # COASTAL FLOOD STATEMENT
            ("LS.W", allActions, "CoastalFlood"),           # LAKESHORE FLOOD WARNING
            ("LS.Y", allActions, "CoastalFlood"),           # LAKESHORE FLOOD ADVISORY
            ("LS.A", allActions, "CoastalFlood"),           # LAKESHORE FLOOD WATCH
            ("LS.S", allActions, "CoastalFloodStatement"),  # LAKESHORE FLOOD STATEMENT
            ("SU.W", allActions, "HighSurf"),               # HIGH SURF WARNING
            ("SU.Y", allActions, "HighSurf"),               # HIGH SURF ADVISORY
            ("BH.S", allActions, "BeachHaz"),               # BEACH HAZARDS STATEMENT
            ("RP.S", allActions, "RipCurrent"),             # HIGH RIP CURRENT RISK
        ]

    def _bulletDict(self):
        """Set the default bullets for each hazard phenomenon."""

        bulletList = ",".join(self._hazSimpBullets)
        return {
            "CF": (bulletList),  # coastal flood warning, advisory, watch
            "LS": (bulletList),  # lake shore flood warning, advisory, watch
            "BH": (bulletList),  # hazardous beach conditions
            "SU": (bulletList),  # high surf warning, advisory
            "RP": (bulletList),  # high rip current risk
        }

    def _bulletOrder(self):
        """Set the default bullet order."""

        return self._hazSimpBullets

    def _makeProduct(self, fcst, segmentAreas, argDict):
        """Create bullets and Precautionary/Preparedness Actions."""

        argDict["language"] = self._language

        # This section generates the headline on the segment

        # Stuff argDict with the segmentAreas for DiscretePhrases
        argDict["segmentAreas"] = segmentAreas

        editArea = segmentAreas[0]
        areaLabel = editArea
        headlines = self.generateProduct("Hazards", argDict, area=editArea,
                                         areaLabel=areaLabel,
                                         timeRange=self._timeRange)
        fcst += headlines

        # This section generates the attribution statements and calls-to-action
        hazardsC = argDict["hazards"]
        listOfHazards = hazardsC.getHazardList(segmentAreas)
        segBody = self.hazardBodyText(listOfHazards, argDict)

        # Remove any attribution section before the bullets or cancellation framing code
        segBody = re.sub("(?i)^[\s\S]*?(\* *WHAT)", r"\1", segBody)
        segBody = re.sub("(?i)^[\s\S]*?(\|\* Wrap-up text goes here \*\|)", r"\1", segBody)
        fcst += segBody

        # If an overview exists for this product, calculate  it
        self.overviewText(listOfHazards, "CFW")

        # Clean up and return
        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "-", "..."])
        return fcst

    def _postProcessProduct(self, fcst, argDict):
        """Replaces the productName if necessary for Lakeshore flood products.
        Adds custom URL text."""

        # If an overview exists for this product, insert it
        overview = self.finalOverviewText()
        overviewSearch = re.compile(r"Default overview section", re.DOTALL)
        fcst = overviewSearch.sub(overview, fcst)

        urgent = 0
        fullKeyList = []
        newList = ["NEW", "EXA", "EXB"]

        hazardsC = argDict["hazards"]
        segmentList = self.organizeHazards(hazardsC.rawAnalyzedTable())
        for segmentAreas in segmentList:
            listOfHazards = hazardsC.getHazardList(segmentAreas)
            for eachHazard in listOfHazards:
                if eachHazard["phensig"] not in fullKeyList:
                    fullKeyList.append(eachHazard["phensig"])
                if eachHazard["phensig"] in ["CF.W", "CF.A", "LS.W", "LS.A"]:
                    if eachHazard["act"] in newList:
                        urgent = 1

        # Remove EAS line if not urgent
        if urgent == 0 and len(self._easPhrase):
            fcst = fcst.replace(self._easPhrase + "\n", "", 1)

        # Rename the product if necessary based on VTEC codes
        for each in fullKeyList:
            if each in ["LS.W", "LS.A", "LS.Y", "LS.S"]:
                productName = "Lakeshore Hazard Message"
                fcst = fcst.replace(self._productName, productName, 1)
                break

        # Added to place line feeds in the CAP tags to keep separate from CTAs
        fcst = fcst.replace(
            r"PRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.",
            r"\nPRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.\n",
        )
        fcst = fcst.replace(".:", ".")
        fcst = fcst.replace("\n ", "\n")
        fcst = fcst.replace("&&", "\n&&\n")

        # Prevent empty Call to Action Tags
        fcst = re.sub(r"\nPRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.\s*&&\n", "", fcst)

        # Remove any empty framing code
        fcst = re.sub(r"\|\*\s*\*\|", "", fcst)

        # Indent the bullet text
        fcst = self._indentBulletText(fcst)

        # Clean up multiple line feeds
        fixMultiLF = re.compile(r"(\n\n)\n*", re.DOTALL)
        fcst = fixMultiLF.sub(r"\1", fcst)

        # Finish Progress Meter
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")

        # Add the url text from the configuration section
        fcst += "\n" + self._urlText

        return fcst
