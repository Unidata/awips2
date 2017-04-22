
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Finalize_KML
# Version 3.0
#
# Author: Joe Maloney/P. Santos
#
# IMPORTANT: Uses /data/local/GFEnhc/archive_ss_gfx.sh
#
# ----------------------------------------------------------------------------
#
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# Jun 20, 2012            Santos      Initial creation
# Apr 12, 2016            LeFebvre    Code cleanup and refactoring
# Sep 19, 2016 19293      randerso    Initial baseline check in
#
########################################################################

MenuItems = ["None"]

import os
import time

import TropicalUtility
import numpy as np

from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData
CoordinateType = ReferenceData.CoordinateType


class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)


    def makeThreatKML(self, discreteWEName, discreteKeys, discreteGrid_kml, stormNum):

# CONFIG READ: the directory below is the directory where the KML txt files will be dumped.
# From there it is syncronized to the web servers along with the graphics. If you set up
# your gHLS scripts and data directories in a different place than recommended in the install
# instructions, you would need to change that directory here. Do not change .kml.txt to .kml.
# Only .txt file can be uploaded as include files. In the servers a php script will convert the
# file name so that a browser can properly interpret it as a KML file to be open with Google
# Earth or equivalent application.

# Also, make sure the ownership of the kml.txt files created below is fxa:fxalpha with permissions set
# to 666.

# You can test the KML files created by copying them outside AWIPS and renaming them .kml. Then open them with
# Google Earth.

        date = time.strftime("%Y%m")
        datetime = time.strftime("%Y%m%d")

        #  Define a file name for the output KML file
        kml_filename = '/data/local/WWTool/kml/' + discreteWEName + '_' + datetime + '_AT' + stormNum + '.kml'

        print "KML FILE IS: ", kml_filename

        with open(kml_filename, 'w') as kml:
            kml.write('<?xml version="1.0" encoding="UTF-8"?>\n')
            kml.write('<kml xmlns="http://www.opengis.net/kml/2.2">\n')
            kml.write('<Document><name>'+discreteWEName+'.kml</name>\n')

            # TODO: do we need to keep these commented out lines?
            #kml.write('<Style id="SSA"><PolyStyle><color>9900a5ff</color><outline>0</outline></PolyStyle></Style>\n')
            #kml.write('<Style id="SSW"><PolyStyle><color>99cc33ff</color><outline>0</outline></PolyStyle></Style>\n')
            #kml.write('<Style id="SSA"><PolyStyle><color>ff0091ff</color><outline>0</outline></PolyStyle></Style>\n')watch orange
            #kml.write('<Style id="SSW"><PolyStyle><color>ff0000ff</color><outline>0</outline></PolyStyle></Style>\n')warning red
            kml.write('<Style id="SSA"><PolyStyle><color>ffc794c9</color><outline>0</outline></PolyStyle></Style>\n')
            kml.write('<Style id="SSW"><PolyStyle><color>ff771cdd</color><outline>0</outline></PolyStyle></Style>\n')
            kml.write('<Folder><name>'+discreteWEName+'</name><open>0</open>\n')

            print "DISCRETEKEYS ARE: ", discreteKeys

            # initialize a flag.  It will only be NO for the first polygon in the file.
            flag = False

            #  Process all of the keys found in this discrete grid
            for hazIndex, key in enumerate(discreteKeys):

                #  Skip the "<None>" value
                if "None" in key:
                    continue

                #  Identify all portions of the grid where this key applies
                mask = discreteGrid_kml == hazIndex

                #  If there are no areas associated with this key - move on
                #  (this should not happen often)
                if not mask.any():
                    continue

                #  Make an editArea from the current mask
                editArea = self.decodeEditArea(mask)

                #  Extract the polygons from the edit area
                polygons = editArea.getPolygons(CoordinateType.LATLON)

    # CONFIG READ: The following section is the one that needs the most local config. For each key and threat element
    # below you must type in the generic impact definitions that you submitted to the national ghls page. For example,
    # for the case of MLB for winds, Those would be found here:
    # http://www.weather.gov/ghls/php/ghls_index.php?sid=mlb&threat=wind#none

                #-------------------------------------------------------------------
                #  Handle storm surge watches and warnings, if we're dealing with
                #  the collaboration grids

                #  Storm Surge Watch
                if key.startswith("SS.A"):

                    if discreteWEName in ["InitialSS","ProposedSS","tempProposedSS"]:
                        kmlHeader='<Placemark><name>Storm Surge Watch</name><description>Storm Surge Watch in Effect</description>\n<styleUrl>#SSA</styleUrl>\n'

                #  Storm Surge Warning
                elif key.startswith("SS.W"):
                    if discreteWEName in ["InitialSS","ProposedSS","tempProposedSS"]:
                        kmlHeader='<Placemark><name>Storm Surge Warning</name><description>Storm Surge Warning in Effect</description>\n<styleUrl>#SSW</styleUrl>\n'

                #-------------------------------------------------------------------
                #  Produce KML for every polygon associated with this particular key

                for i in range(polygons.getNumGeometries()):
                    poly = polygons.getGeometryN(i)
                    shell = poly.getExteriorRing();
                    if shell:
                        if flag:
                            kml.write('</Polygon></Placemark>\n')

                        kml.write(kmlHeader)
                        kml.write('<Polygon><outerBoundaryIs><LinearRing><coordinates>')
                        print "Outer shell coordinates:"
                        for c in shell.getCoordinates():
                            line = str(c.x) + ',' + str(c.y) + ',0 \n'
                            kml.write(line)

                        kml.write('</coordinates></LinearRing></outerBoundaryIs>')
                          # Now that we've written at least one polygon, set flag to YES
                        flag = True

                        for j in xrange(poly.getNumInteriorRing()):
                            hole = poly.getInteriorRingN(j)
                            print "Hole",j,"coordinates:"
                            kml.write('<innerBoundaryIs><LinearRing><coordinates>')
                            for c in hole.getCoordinates():
                                line = str(c.x) + ',' + str(c.y) + ',0 \n'
                                kml.write(line)

                            kml.write('</coordinates></LinearRing></innerBoundaryIs>')

            kmlEnd='</Polygon></Placemark>\n</Folder></Document></kml>\n'
            kml.write(kmlEnd)


    # For each unique ETN, determine the mask that covers that ETN. Return the
    # result in a dictionary ETN:mask
    def calcStormMasks(self, threatGrid, threatKeys):

        #  Initialize the dictionary
        stormDict = {}

        #  process all the keys within this discrete grid
        for index, key in enumerate(threatKeys):

            #  Get the storm number, the last two digits, from the ETN
            etn = self.getETN(key)[-2:]

            #  Skip the "<None>" value, we don't need it.  Also skip any key
            #  which does not contain a valid storm number
            if "None" in key or not etn.isdigit():
                continue

            #  Identify areas where this key is applied
            mask = (threatGrid == index)

            #  If we already have a mask for this storm, add to it
            if stormDict.has_key(etn):
                stormDict[etn] |= mask

            #  Otherwise, make a new dictionary entry
            else:
                stormDict[etn] = mask

        #  Return the completed mask dictionary
        return stormDict


    def execute(self):

        # TODO: should this commented out line be removed?
        #os.system('/data/local/GFEnhc/archive_ss_gfx.sh')

        #  Make a timeRange that starts at top of this hour and 12 hours long
        start = int(self._gmtime().unixTime() / 3600) * 3600
        end = start + 12 * 3600
        tr = self.GM_makeTimeRange(start, end)

        #  Define some conditions before we start processing
        discreteList = ['ProposedSS'] #['InitialSS','ProposedSS','tempProposedSS']
        editAreaMask = self.encodeEditArea("StormSurgeWW_EditArea")

        #  Process all of the specified discrete grids
        for discreteWEName in discreteList:

            discreteGrid, discreteKeys = self.getGrids("Fcst", discreteWEName, "SFC", tr)

            # Get the masks that cover each ETN for this grid
            stormMaskDict = self.calcStormMasks(discreteGrid, discreteKeys)

            #  Process the mask for each storm we found
            for stormNum in stormMaskDict.keys():

                print "****************stormNum: ", stormNum
                # Initialize grid to -9 everywhere, we're not sure why at this point
                discreteGrid_kml = np.zeros(self.getGridShape(), np.int8) - 9

                # Get the mask associated with this particular storm
                threatMask = stormMaskDict[stormNum]

                #  Update the grid we're going to convert to KML only with data
                #  where we have actual threats
                discreteGrid_kml[threatMask] = discreteGrid[threatMask]

                #  Format the final KML for this storm
                self.makeThreatKML(discreteWEName, discreteKeys, discreteGrid_kml, stormNum)

        # Copy all the KML files to the destination
        # TODO: should this commented out line be removed?
#        os.system('ssh dx4-nhcn rsync -av --delete -e ssh /data/local/GFEnhc/archive ldad@ls1:/data/ldad/share/test/.')
