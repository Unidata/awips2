# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TCImpactGraphics_KML
#
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer     Description
# ------------- -------- ------------ -----------------------------------------
# Apr 19, 2011           P. Santos    Initial creation
#                        J. Maloney
# Jul 27, 2012           S. White     made A2 compatible
#                        R. Anderson
# Jul 30, 2012           J. Maloney   made it work with A2
#                        P. Santos
# Sep 09, 2014           J. Maloney   for 2015 season, removed MarineThreat,
#                                     renamed CoastalThreat -> StormSurgeThreat,
#                                             InlandThreat -> FloodingRainThreat,
#                                     removed verylow from kml styles in header
# Sep 11, 2014           J. Maloney   site ID is now automatically retrieved from
#                        S. White     the environment.
#
# Sep 16, 2014           J. Maloney   impact statements are now read from
#                        T. Lefebvre  TCVDictionary (in Utilities) and formatted
#                                     on-the-fly!
# Oct 21, 2014           J. Maloney   products now go in /awips2/GFESuite/hti/data.
# Jun 09, 2017           P. Santos    Remove old labels for 2018.
# Jun 21, 2017           J. Maloneu   added CDATA tags to Placemark descriptions
# Jul 25, 2017           P. Santos    added EA config and threatPhrase dicts
#                        S. White
# May 21, 2021  8467     R. Anderson  products now written to
#                                     /awips2/GFESuite/hti/data/site to avoid
#                                     conflicts when in service backup
# Jul 12, 2021  DCS22519 J. Lamb      Remove redundant call to kml_legend.sh
#                                     Eliminate need for configuring edit areas or
#                                     modifying tool at non-surge sites
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

MenuItems = ["Populate"]

import os
import time

import SmartScript
import TCVDictionary
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData
import numpy as np


CoordinateType = ReferenceData.CoordinateType

###      CONFIG section READ         ##########

# The kml txt files will be placed in /awips2/GFESuite/hti/data/<sid>/threatWEName.kml.txt
# From there it is synchronized to the web servers along with the graphics.
# At NIDS a php script will convert the file from txt to kml to allow it to be viewed.

# Also, make sure the ownership of the kml.txt files created is fxa:fxalpha with permissions set
# to 666.

# You can test the files created by copying them outside AWIPS and renaming them .kml.
# Then open them with Google Earth.

# Rarely needs to be overridden - land-only areas will default to baseline edit
# area for your site (XXX).
######################################################################
editAreaDict = {
    # "StormSurgeThreat" : "StormSurgeWW_EditArea_Local", # Leave as-is
    #   "WindThreat" : "XXX",            # Land-only EA
    #   "FloodingRainThreat" : "XXX",    # Land-only EA
    #   "TornadoThreat" : "XXX"          # Land-only EA
}

##### End Config #########

threatPhraseDict = {
    "Wind": {
        "Extreme": "Potential for wind greater than 110 mph",
        "High": "Potential for wind 74 to 110 mph",
        "Mod": "Potential for wind 58 to 73 mph",
        "Elevated": "Potential for wind 39 to 57 mph",
        "None": "Wind less than 39 mph",
    },
    "Storm Surge": {
        "Extreme": "Potential for storm surge flooding greater than 9 feet above ground",
        "High": "Potential for storm surge flooding greater than 6 feet above ground",
        "Mod": "Potential for storm surge flooding greater than 3 feet above ground",
        "Elevated": "Potential for storm surge flooding greater than 1 foot above ground",
        "None": "Little to no storm surge flooding",
    },
    "Flooding Rain": {
        "Extreme": "Potential for extreme flooding rain",
        "High": "Potential for major flooding rain",
        "Mod": "Potential for moderate flooding rain",
        "Elevated": "Potential for localized flooding rain",
        "None": "Little or no potential for flooding rain",
    },
    "Tornado": {
        "Extreme": "Potential for an outbreak of tornadoes",
        "High": "Potential for many tornadoes",
        "Mod": "Potential for several tornadoes",
        "Elevated": "Potential for a few tornadoes",
        "None": "Tornadoes not expected",
    },
}


class Procedure(SmartScript.SmartScript):

    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def makeThreatKML(self, threatWEName, threatKeys, threatGrid_kml):

        kml_dir = "/awips2/GFESuite/hti/data/{}".format(self._siteID.lower())

        # Ensure the kml directory exists
        # Python 3
        try:
            os.makedirs(kml_dir, mode=0o770, exist_ok=True)
        # Python 2
        except TypeError:
            if not os.path.exists(kml_dir):
                os.makedirs(kml_dir)
                os.chmod(kml_dir, 0o770)

        kml_filename = "{}/{}.kml.txt".format(kml_dir, threatWEName)

        weDict = {
            "TornadoThreat": "Tornado",
            "StormSurgeThreat": "Storm Surge",
            "WindThreat": "Wind",
            "FloodingRainThreat": "Flooding Rain",
        }

        styleDict = {
            "Extreme": "#extreme",
            "High": "#high",
            "Mod": "#moderate",
            "Elevated": "#low",
        }

        with open(kml_filename, "w") as kml:
            kml.write(
                '<?xml version="1.0" encoding="UTF-8"?>\n'
                '<kml xmlns="http://www.opengis.net/kml/2.2">\n'
                '<Document><name>{Threat}.kml</name>\n<Style id="none"><PolyStyle>'
                "<color>99e0e0e0</color><outline>0</outline></PolyStyle>"
                "</Style>\n"
                '<Style id="low"><PolyStyle><color>9900ffff</color><outline>0</outline>'
                "</PolyStyle></Style>\n"
                '<Style id="moderate"><PolyStyle><color>990fa7ff</color>'
                "<outline>0</outline></PolyStyle></Style>\n"
                '<Style id="high"><PolyStyle><color>990000ff</color><outline>0</outline>'
                "</PolyStyle></Style>\n"
                '<Style id="extreme"><PolyStyle><color>99cc00cc</color>'
                "<outline>0</outline></PolyStyle></Style>\n"
                "<Folder><name>{Threat}</name><open>0</open>\n"
                "<ScreenOverlay><name>Product Legend</name>"
                "<description>Product Legend</description><visibility>1</visibility><Icon>"
                "<href>http://www.nws.noaa.gov/images/ghls/{sid}/{threat}legend.png</href>"
                '</Icon><overlayXY x=".02" y=".01" xunits="fraction" yunits="fraction" />'
                '<screenXY x=".02" y=".01" xunits="fraction" yunits="fraction" />'
                '<rotationXY x=".02" y=".01" xunits="fraction" yunits="fraction" />'
                '<size x="-1" y="-1" xunits="fraction" yunits="fraction" />'
                "</ScreenOverlay>".format(
                    sid=self._siteID.lower(),
                    Threat=threatWEName,
                    threat=threatWEName.lower(),
                )
            )

            # initialize a flag.  It will only be False for the first polygon in the
            # file.
            firstPolygonWritten = False

            for key in threatKeys:

                # get index for this key
                hazIndex = self.getIndex(key, threatKeys)

                mask = threatGrid_kml == hazIndex

                if not mask.any():
                    continue

                # make an editArea from the mask
                editArea = self.decodeEditArea(mask)

                # extract the polygons from the edit area
                polygons = editArea.getPolygons(CoordinateType.LATLON)

                # pull out the impact statements from the TCVDictionary
                # We need to match the threatWEName to the entries found
                # in the TCVDictionary
                threat = weDict.get(threatWEName)

                # Retrieve the new threat description from the dictionary
                threatPhrase = threatPhraseDict.get(threat, {}).get(key, "")

                # Extract the appropriate list from the dictionary, join them
                # into a string, and make them separate bullets
                impactStatement = ""
                impactList = TCVDictionary.PotentialImpactStatements.get(threat, {}).get(
                    key, ""
                )
                impactStatement = "<br />* ".join(impactList)
                impactStatement = "* " + impactStatement

                # Put our kml header together
                kmlHeader = (
                    "<Placemark><name>Threat Level - {}</name><description>"
                    "<![CDATA[<b>Potential Impacts Include:</b><br />{}]]>"
                    "</description>\n<styleUrl>{}</styleUrl>\n".format(
                        threatPhrase, impactStatement, styleDict.get(key, "#none")
                    )
                )

                for i in range(polygons.getNumGeometries()):
                    poly = polygons.getGeometryN(i)
                    shell = poly.getExteriorRing()
                    if shell:
                        # If shell is true, it's a new polygon
                        if firstPolygonWritten:
                            # If flag is YES, this is not the first polygon we're writing
                            # so close the previous polygon before continuing.
                            kml.write("</Polygon></Placemark>\n")

                        kml.write(kmlHeader)
                        kml.write("<Polygon><outerBoundaryIs><LinearRing><coordinates>")

                        for c in shell.getCoordinates():
                            kml.write(str(c.x) + "," + str(c.y) + ",0 \n")

                        kml.write("</coordinates></LinearRing></outerBoundaryIs>")
                        # Now that we've written at least one polygon, set flag to True
                        firstPolygonWritten = True

                        # CHECK FOR INNER LOOPS (HOLES)
                        for j in range(poly.getNumInteriorRing()):
                            hole = poly.getInteriorRingN(j)

                            kml.write("<innerBoundaryIs><LinearRing><coordinates>")
                            for c in hole.getCoordinates():
                                kml.write(str(c.x) + "," + str(c.y) + ",0 \n")

                            kml.write("</coordinates></LinearRing></innerBoundaryIs>")

            kml.write("</Polygon></Placemark>\n</Folder></Document></kml>\n")

        return

    def execute(self, varDict):

        curHr = self._gmtime().timetuple().tm_hour
        tr = self.createTimeRange(curHr - 12, curHr + 12, "Zulu")

        threatlist = [
            "WindThreat",
            "FloodingRainThreat",
            "TornadoThreat",
        ]

        self._siteID = self.getSiteID()

        surgeOffices = [
            "AKQ",
            "BOX",
            "BRO",
            "CAR",
            "CHS",
            "CRP",
            "GYX",
            "HFO",
            "HGX",
            "ILM",
            "JAX",
            "KEY",
            "LCH",
            "LIX",
            "LWX",
            "MFL",
            "MHX",
            "MLB",
            "MOB",
            "OKX",
            "PHI",
            "SJU",
            "TAE",
            "TBW",
        ]
        if self._siteID in surgeOffices:
            threatlist.append("StormSurgeThreat")

        for threatWEName in threatlist:

            htiInv = self.getGridInfo("Fcst", threatWEName, "SFC", tr)
            if not htiInv:
                print("No {} grid found!".format(threatWEName))
                continue

            threatGrid, threatKeys = self.getGrids("Fcst", threatWEName, "SFC", tr)

            # Check for overridden edit area, otherwise set default
            if threatWEName == "StormSurgeThreat":
                eaName = editAreaDict.get(threatWEName, "StormSurgeWW_EditArea_Local")
            else:
                eaName = editAreaDict.get(threatWEName, self._siteID)

            editArea = self.getEditArea(eaName)
            threatEditArea = self.encodeEditArea(editArea)

            threatGrid_kml = np.where(threatEditArea, threatGrid, threatGrid - 9.0)

            self.makeThreatKML(threatWEName, threatKeys, threatGrid_kml)

        return