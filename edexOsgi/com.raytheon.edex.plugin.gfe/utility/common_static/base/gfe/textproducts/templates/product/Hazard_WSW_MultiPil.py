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
# Hazard_WSW.py
#
#
##########################################################################
import GenericHazards
import copy
import re


class TextProduct(GenericHazards.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition["displayName"] = "BaselineHazard_WSW_<MultiPil> (Winter Wx Product)"

    Definition["defaultEditAreas"] = "EditAreas_PublicZones_<site>_<MultiPil>"

    # Map background for creating Combinations
    Definition["mapNameForCombinations"] = "Zones_<site>"

    # Special multiple product domains for certain sites:
    if "<site>" == "AFG":
        if "_<MultiPil>" == "_AFG":
            Definition["subDomainUGCs"] = ["AKZ218","AKZ219","AKZ220","AKZ221",
                                           "AKZ222","AKZ223","AKZ224","AKZ225",
                                           "AKZ226"]
        elif "_<MultiPil>" == "_NSB":
            Definition["subDomainUGCs"] = ["AKZ201","AKZ202","AKZ203","AKZ204",
                                           "AKZ205","AKZ206"]
        elif "_<MultiPil>" == "_WCZ":
            Definition["subDomainUGCs"] = ["AKZ207","AKZ208","AKZ209","AKZ210",
                                           "AKZ211","AKZ212","AKZ213","AKZ214",
                                           "AKZ215","AKZ216","AKZ217","AKZ227"]

    # Header configuration items
    Definition["productName"] = "URGENT - WINTER WEATHER MESSAGE"  # name of product
    Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
    Definition["wmoID"] = "<wmoID>"  # WMO ID
    Definition["pil"] = "<pil>"  # product pil
    # Definition["areaName"] = "Statename"  # Name of state, such as "Georgia"
    Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
    Definition["wfoCity"] = "<wfoCity>"  # WFO Name as it should appear in a text product
    Definition["textdbPil"] = "<textdbPil>"  # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"  # Product ID for transmitting to AWIPS WAN.
    Definition["outputFile"] = "{prddir}/TEXT/WSW_<MultiPil>.txt"

    # OPTIONAL CONFIGURATION ITEMS
    # Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
    # Definition["displayOutputDialog"] = 0  # If 1 will display results when finished
    # Definition["debug"] = 1
    # Definition["headlineEditAreaGroup"] = "Zones" # Name of EditAreaGroup for sampling headlines

    Definition["purgeTime"] = 8  # Maximum hours for expireTime from issueTime
    Definition["includeCities"] = 1  # Cities included in area header
    Definition["accurateCities"] = 0  # 1: cities are based on grids; 0: full list is included
    Definition["cityLocation"] = "CityLocation"  # City lat/lon dictionary to use
    Definition["cityDescriptor"] = "Including the cities of"
    Definition["includeZoneNames"] = 1  # Zone names will be included in the area header
    Definition["lineLength"] = 66  # Zone names will be included in the area header
    # Definition["easPhrase"] = ""       # Optional EAS phrase to be include in product header
    Definition["includeOverviewHeadline"] = 0  # If 1, the overview header is templated
    Definition["includeOverview"] = 0  # If 1, the overview section is templated
    Definition["bulletProd"] = 1  # If 1, the product is bulletted
    Definition["hazSimpBullets"] = ["WHAT", "WHERE", "WHEN", "IMPACTS", "ADDITIONAL DETAILS"]
    # Definition["hazardSamplingThreshold"] = (10, None)  # (%cov, #points)

    def __init__(self):
        GenericHazards.TextProduct.__init__(self)

    def allowedHazards(self):
        """Hazards allowed in WSW products."""

        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
            ("BZ.W", allActions, "WinterWx"),   # BLIZZARD WARNING
            ("IS.W", allActions, "WinterWx"),   # ICE STORM WARNING
            ("LE.W", allActions, "WinterWx"),   # LAKE EFFECT SNOW WARNING
            ("WS.W", allActions, "WinterWx"),   # WINTER STORM WARNING
            ("WW.Y", allActions, "WinterWx"),   # WINTER WEATHER ADVISORY
            ("WS.A", allActions, "WinterWx"),   # WINTER STORM WATCH
            ("WC.W", allActions, "WindChill"),  # WIND CHILL WARNING
            ("WC.Y", allActions, "WindChill"),  # WIND CHILL ADVISORY
            ("WC.A", allActions, "WindChill"),  # WIND CHILL WATCH
        ]

    def _bulletDict(self):
        """Set the default bullets for each hazard phenomenon."""

        bulletList = ",".join(self._hazSimpBullets)
        return {
            "WS": (bulletList),  # Winter Storm
            "WW": (bulletList),  # Winter Weather
            "LE": (bulletList),  # Lake Effect
            "BZ": (bulletList),  # Blizzard
            "ZR": (bulletList),  # Freezing Rain
            "IS": (bulletList),  # Ice Storm
            "WC": (bulletList),  # Wind Chill
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
        self.overviewText(listOfHazards, "WSW")

        # Clean up and return
        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "-", "..."])
        return fcst
