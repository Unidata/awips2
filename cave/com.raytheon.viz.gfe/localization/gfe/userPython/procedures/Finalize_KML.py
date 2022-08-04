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
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# Jun 20, 2012            Santos      Initial creation
# Apr 12, 2016            LeFebvre    Code cleanup and refactoring
# Sep 19, 2016 19293      randerso    Initial baseline check in
# May 3, 2019  21021      swhite      modified for HFO SSWW implementation
# Oct 18, 2019 21021      nhardin     Refactoring/Cleanup for check in
# Aug 20, 2021 22718      nhardin     Fix for blank KML file being produced for storm surge
########################################################################

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

MenuItems = ["None"]

import os
import time
import TropicalUtility
import numpy as np
import LogStream

from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData
CoordinateType = ReferenceData.CoordinateType


class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)

    ################USER OVERRIDES##############
    def getKML_Directory(self, basinNum):
        if basinNum == "10":
            kmlDirectory = '/data/local/WWTool/kml/'
        else:
            kmlDirectory = '/awips2/GFESuite/hti/data/'

        return kmlDirectory

    ############################################

    def makeTimeRange(self):
        '''
        Makes time range starting at top of hour and lasts for 12 hours
        '''
        start = int(self._gmtime().unixTime() / 3600) * 3600
        end = start + 12 * 3600
        return self.GM_makeTimeRange(start, end)

    def createDiscreteList(self):
        '''
        Creates discrete list of weather elements to write to KML
        '''
        return ['ProposedSS']

    def createEditAreaMask(self):
        '''
        Creates edit area of mask used to constrain extent of threatGrid
        '''
        if self.getSiteID() == "NHA":
            editAreaMask = self.encodeEditArea("StormSurgeWW_EditArea")
        else:
            editAreaMask = self.encodeEditArea("StormSurgeWW_EditArea_Local")

        return editAreaMask

    def calcStormMasks(self, threatGrid, threatKeys):
        '''
        For each unique ETN, determine the mask that covers that ETN.
        Return result in a dictionary ETN:mask
        '''
        stormDict = {}
        for index, key in enumerate(threatKeys):
            etn = self.getETN(key)[-4:]

            if "None" in key or not etn.isdigit():
                continue

            mask = (threatGrid == index)

            if etn in stormDict:
                stormDict[etn] |= mask
            else:
                stormDict[etn] = mask

        return stormDict

    def makeThreatKML(self, discreteWEName, discreteKeys, discreteGrid_kml, etn):
        '''
        Makes KML file based on hazard keys
        '''
        datetime = self.getDateString()

        basinNum, stormNum, basin = self.getBasinInfo(etn)

        kmlFilename = self.createKML_Filename(discreteWEName, datetime, basinNum, stormNum, basin)

        with open(kmlFilename, 'w') as kml:
            kml = self.createKML(kml, discreteWEName, discreteKeys)
            self.flag = False
            for hazIndex, key in enumerate(discreteKeys):
                if "None" in key:
                    continue
                polygons = self.createKeyPolygons(discreteGrid_kml, hazIndex, key)
                kmlHeader = self.createKML_Header(discreteWEName, hazIndex, key)
                kml = self.createKML_Polygons(kml, polygons, kmlHeader)

            kml = self.closeKML(kml)

    def getDateString(self):
        '''
        Creates date time string
        '''
        return time.strftime("%Y%m%d")

    def getBasinInfo(self, etn):
        '''
        Returns basin information used for KML metadata
        '''
        basinNum = str(etn[:2])
        stormNum = str(etn[2:])

        if basinNum == "10":
            basin = "_AT"
        elif basinNum == "20":
           basin = "_EP"
        elif basinNum == "30":
            basin = "_CP"
        elif basinNum == "40":
            basin = "_WP"
        else:
            self.statusBarMsg("Unknown basin: " + basinNum, "U")
            return

        return basinNum, stormNum, basin

    def createKML_Filename(self, discreteWEName, datetime, basinNum, stormNum, basin):
        '''
        Define a file name for the output KML file
        '''
        kmlDirectory = self.getKML_Directory(basinNum)

        kmlFilename = os.path.join(kmlDirectory, discreteWEName + '_' + datetime + basin + stormNum + '.kml')

        msg = "KML FILE IS: ", kmlFilename
        LogStream.logDebug(msg)

        return kmlFilename

    def createKML(self, kml, discreteWEName, discreteKeys):
        '''
        Begin writing KML file
        '''
        kml.write('<?xml version="1.0" encoding="UTF-8"?>\n')
        kml.write('<kml xmlns="http://www.opengis.net/kml/2.2">\n')
        kml.write('<Document><name>'+discreteWEName+'.kml</name>\n')

        kml.write('<Style id="SSA"><PolyStyle><color>ffc794c9</color><outline>0</outline></PolyStyle></Style>\n')
        kml.write('<Style id="SSW"><PolyStyle><color>ff771cdd</color><outline>0</outline></PolyStyle></Style>\n')
        kml.write('<Folder><name>'+discreteWEName+'</name><open>0</open>\n')

        msg = "DISCRETEKEYS ARE: ", discreteKeys
        LogStream.logDebug(msg)

        return kml

    def createKeyPolygons(self, discreteGrid_kml, hazIndex, key):
        '''
        Create polygons of key area using mask of grid
        '''
        mask = discreteGrid_kml == hazIndex

        editArea = self.decodeEditArea(mask)

        polygons = editArea.getPolygons(CoordinateType.LATLON)

        return polygons

    def createKML_Header(self, discreteWEName, hazIndex, key):
        '''
        Create KML header for SS.A or SS.W assuming we are working with the collaboration grids.
        '''
        if key.startswith("SS.A"):
            if discreteWEName in ["InitialSS", "ProposedSS", "tempProposedSS"]:
                kmlHeader = '<Placemark><name>Storm Surge Watch</name><description>Storm Surge Watch in Effect</description>\n<styleUrl>#SSA</styleUrl>\n'

        elif key.startswith("SS.W"):
            if discreteWEName in ["InitialSS", "ProposedSS", "tempProposedSS"]:
                kmlHeader = '<Placemark><name>Storm Surge Warning</name><description>Storm Surge Warning in Effect</description>\n<styleUrl>#SSW</styleUrl>\n'
        else:
            kmlHeader = '<Placemark><name></name><description></description>\n<styleUrl></styleUrl>\n'
        return kmlHeader

    def createKML_Polygons(self, kml, polygons, kmlHeader):
        '''
        Creates KML for every polygon associated with particular key in discreteKeys
        '''
        if kmlHeader is not None:
            for i in range(polygons.getNumGeometries()):
                poly = polygons.getGeometryN(i)
                shell = poly.getExteriorRing()
                if shell:
                    if self.flag:
                        kml.write('</Polygon></Placemark>\n')

                    kml.write(kmlHeader)
                    kml.write('<Polygon><outerBoundaryIs><LinearRing><coordinates>')
                    LogStream.logDebug("Outer shell coordinates:")
                    for c in shell.getCoordinates():
                        line = str(c.x) + ',' + str(c.y) + ',0 \n'
                        kml.write(line)

                    kml.write('</coordinates></LinearRing></outerBoundaryIs>')
                      # Now that we've written at least one polygon, set flag to YES
                    self.flag = True

                    for j in range(poly.getNumInteriorRing()):
                        hole = poly.getInteriorRingN(j)
                        msg = "Hole",j,"coordinates:"
                        LogStream.logDebug(msg)
                        kml.write('<innerBoundaryIs><LinearRing><coordinates>')
                        for c in hole.getCoordinates():
                            line = str(c.x) + ',' + str(c.y) + ',0 \n'
                            kml.write(line)

                        kml.write('</coordinates></LinearRing></innerBoundaryIs>')

        return kml

    def closeKML(self, kml):
        '''
        Finishes KML writing and returns finished KML file
        '''
        kmlEnd='</Polygon></Placemark>\n</Folder></Document></kml>\n'
        kml.write(kmlEnd)

        return kml

    def execute(self):
        tr = self.makeTimeRange()
        discreteList = self.createDiscreteList()
        editAreaMask = self.createEditAreaMask()

        for discreteWEName in discreteList:
            discreteGrid, discreteKeys = self.getGrids("Fcst", discreteWEName, "SFC", tr)
            stormMaskDict = self.calcStormMasks(discreteGrid, discreteKeys)

            for etn in stormMaskDict:
                discreteGrid_kml = self.newGrid(-9, dtype=np.int8)
                threatMask = stormMaskDict[etn]
                discreteGrid_kml[threatMask] = discreteGrid[threatMask]
                self.makeThreatKML(discreteWEName, discreteKeys, discreteGrid_kml, etn)
