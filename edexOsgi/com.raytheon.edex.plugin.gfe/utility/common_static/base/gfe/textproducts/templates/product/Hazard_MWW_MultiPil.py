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
# Hazard_MWW.py
#
#
##########################################################################
import GenericHazards
import copy
import re


class TextProduct(GenericHazards.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition["displayName"] = "BaselineHazard_MWW_<MultiPil> (Marine Weather)"

    Definition["defaultEditAreas"] = "EditAreas_MarineZones_<site>_<MultiPil>"

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
    Definition["productName"] = "URGENT - MARINE WEATHER MESSAGE"  # name of product
    Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
    Definition["wmoID"] = "<wmoID>"  # WMO ID
    Definition["pil"] = "<pil>"  # product pil
    # Definition["areaName"] = "Statename"  # Name of state, such as "Georgia"
    Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
    Definition["wfoCity"] = "<wfoCity>"  # WFO Name as it should appear in a text product
    Definition["textdbPil"] = "<textdbPil>"  # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"  # Product ID for transmitting to AWIPS WAN.
    Definition["outputFile"] = "{prddir}/TEXT/MWW_<MultiPil>.txt"

    # OPTIONAL CONFIGURATION ITEMS
    # Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
    # Definition["displayOutputDialog"] = 0  # If 1 will display results when finished
    # Definition["debug"] = 1
    # Definition["headlineEditAreaGroup"] = "MZones" # Name of EditAreaGroup for sampling headlines

    Definition["purgeTime"] = 8  # Maximum hours for expireTime from issueTime
    # Definition["includeCities"] = 1  # Cities included in area header
    # Definition["cityDescriptor"] = "Including the cities of"
    Definition["includeZoneNames"] = 1  # Zone names will be included in the area header
    # Definition["easPhrase"] = ""       # Optional EAS phrase to be include in product header
    Definition["lineLength"] = 66
    Definition["includeOverviewHeadline"] = 0  # Policy no longer allows the Overview section
    Definition["includeOverview"] = 0  # Policy no longer allows the Overview section
    Definition["bulletProd"] = 1  # If 1, the product has a bullet format
    Definition["hazSimpBullets"] = ["WHAT", "WHERE", "WHEN", "IMPACTS", "ADDITIONAL DETAILS"]

    # Definition["hazardSamplingThreshold"] = (10, None)  # (%cov, #points)

    # Text to insert below the last $$ of the product. Use "" for no text.
    Definition["urlText"] = ""
    # multiple line example
    # Definition["urlText"] = ("For more information from the National Weather Service visit\n"
    #                          "http://weather.gov/saltlakecity")

    def __init__(self):
        GenericHazards.TextProduct.__init__(self)

    def allowedHazards(self):
        """Hazards allowed in MWW products."""

        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
            ("HU.W", allActions, "Tropical"),   # HURRICANE WARNING
            ("TR.W", allActions, "Tropical1"),  # TROPICAL STORM WARNING
            ("GL.W", allActions, "Marine3"),    # GALE WARNING
            ("HF.W", allActions, "Marine"),     # HURRICANE FORCE WIND WARNING
            ("MH.W", allActions, "Ashfall"),    # VOLCANIC ASHFALL WARNING
            ("SE.W", allActions, "Marine4"),    # HAZARDOUS SEAS
            ("SR.W", allActions, "Marine"),     # STORM WARNING
            ("UP.W", allActions, "IceAccr"),    # HEAVY FREEZING SPRAY WARNING
            ("MH.Y", allActions, "Ashfall"),    # VOLCANIC ASHFALL ADVISORY
            ("BW.Y", allActions, "Marine"),     # BRISK WIND ADVISORY
            ("MF.Y", allActions, "Fog"),        # DENSE FOG ADVISORY
            ("LO.Y", allActions, "LowWater"),   # LOW WATER ADVISORY
            ("SC.Y", allActions, "Marine3"),    # SMALL CRAFT ADVISORY
            ("MS.Y", allActions, "Smoke"),      # DENSE SMOKE ADVISORY
            ("UP.Y", allActions, "IceAccr"),    # HEAVY FREEZING SPRAY ADVISORY
            ("HU.A", allActions, "Tropical"),   # HURRICANE WATCH
            ("TR.A", allActions, "Tropical1"),  # TROPICAL STORM WATCH
            ("GL.A", allActions, "Marine3"),    # GALE WATCH
            ("HF.A", allActions, "Marine"),     # HURRICANE FORCE WIND WATCH
            ("SE.A", allActions, "Marine4"),    # HAZARDOUS SEAS WATCH
            ("SR.A", allActions, "Marine"),     # STORM WATCH
            ("UP.A", allActions, "IceAccr"),    # HEAVY FREEZING SPRAY WATCH
        ]

    def _bulletDict(self):
        """Set the default bullets for each hazard phenomenon."""

        bulletList = ",".join(self._hazSimpBullets)
        return {
            "HU": (bulletList),  # hurricane warning, watch
            "TR": (bulletList),  # tropical storm warning, watch
            "GL": (bulletList),  # gale warning, watch
            "HF": (bulletList),  # hurricane force wind warnings, watch
            "MH": (bulletList),  # volcanic ashfall warning, advisory
            "SE": (bulletList),  # hazardous seas warning, watch
            "SR": (bulletList),  # storm warning, watch
            "UP": (bulletList),  # heavy freezing spray warnings, advisory, watch
            "BW": (bulletList),  # brisk wind advisory
            "MF": (bulletList),  # dense fog advisory
            "LO": (bulletList),  # low water advisory
            "SC": (bulletList),  # small craft advisory
            "MS": (bulletList),  # dense smoke advisory
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
        self.overviewText(listOfHazards, "MWW")

        # Clean up and return
        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "-", "..."])
        return fcst

    def _postProcessProduct(self, fcst, argDict):
        """Adds URL text."""

        # If an overview exists for this product, insert it
        overview = self.finalOverviewText()
        overviewSearch = re.compile(r"Default overview section", re.DOTALL)
        fcst = overviewSearch.sub(overview, fcst)

        # Added to place line feeds in the CAP tags to keep separate from CTAs
        fcst = fcst.replace(r"PRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.",
                            r"\nPRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.\n")
        fcst = fcst.replace(".:", ".")
        fcst = fcst.replace("\n ", "\n")
        fcst = fcst.replace("&&", "\n&&\n")

        # Prevent empty Call to Action tags
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

        # add the url text from the configuration section
        fcst += "\n" + self._urlText

        return fcst
