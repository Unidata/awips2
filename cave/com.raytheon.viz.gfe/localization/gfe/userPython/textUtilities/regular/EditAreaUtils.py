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
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# EditAreaUtils.py
# Utilities for dealing with Edit Areas.
#
# Author: hansen
# ----------------------------------------------------------------------------

import string, types, time
import math, logging
import TextUtils
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData, ReferenceID
from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DBit as JavaGrid2DBit
from java.util import ArrayList

class  EditAreaUtils(TextUtils.TextUtils):
    def __init__(self):
        self.__gridLoc = None
        TextUtils.TextUtils.__init__(self)
        self.__comboNumber = -1
        self.log = logging.getLogger("FormatterRunner.EditAreaUtils.EditAreaUtils")

    def setUp(self, parmNameAndLevel, argDict):
        # Must set this up in order to do intersections and unions
        self.__ifpClient = argDict["ifpClient"]
        self.__gridLoc = self.__ifpClient.getDBGridLocation()
        self.__dataMgr = argDict["dataMgr"]

    def getIFPClient(self):
        return self.__ifpClient

    def getGridLoc(self):
        return self.__gridLoc

    def getAreaList(self, argDict):
        # Get the list of edit areas and display names for the product
        return argDict["editAreas"]

    def currentAreaContains(self, tree, areaNameList, areaName=None):
        # Returns 1 if any of the current edit area(s) are equal to
        #  OR intersect with any of the areas listed in the areaNameList
        # If a Combinations file is being used, this method
        #  will return 1 if ANY of the current component edit areas
        #  are in the areaNameList
        # Otherwise, returns 0
        #
        # Example:
        #        inlandWaters = self.inlandWatersAreas(tree, node)
        #        if self.currentAreaContains(tree, inlandWaters):

        curAreaNames = self.getCurrentAreaNames(tree, areaName)
        for curAreaName in curAreaNames:
            for areaName in areaNameList:
                if curAreaName == areaName:
                    return 1
                if self.inQuery(tree, areaName, curAreaName):
                    return 1
        return 0

    def currentAreaConsistsOf(self, tree, areaNameList):
        # Returns 1 if the current edit area is equal to OR consists
        #    exclusively of areas listed in the areaNameList
        # If a Combinations file is being used, this method
        #  will return 1 if ALL of the current component edit areas
        #  are in the areaNameList
        # Otherwise, returns 0
        areaNames = self.getCurrentAreaNames(tree)
        for areaName in areaNames:
            if not areaName in areaNameList:
                return 0
        return 1

    def getEditAreas(self, argDict, areas):
        # Get the ReferenceArea given
        #  a name or ReferenceID or ReferenceData(just return)
        refDataList = []
        for area in areas:
            if type(area) is str: # Get area from name
                id = ReferenceID(area)
                jlist = ArrayList()
                jlist.add(id)
                tmp = argDict["ifpClient"].getReferenceData(jlist)                
                refDataList.append(tmp.get(0))
            else:
                if str(area).find('Id') > -1:
                    refDataList.append(area)
                else:
                        jlist = ArrayList()
                        jlist.add(id)
                        tmp = argDict["ifpClient"].getReferenceData(jlist)                        
                        refDataList.append(tmp.get(0))
                
        return refDataList

    def getEditArea(self, editAreaName, argDict):
        # Returns an AFPS.ReferenceData object given an edit area name
        # as defined in the GFE
        refID = ReferenceID(editAreaName)
        refList = ArrayList()
        refList.add(refID)
        tmp = argDict["ifpClient"].getReferenceData(refList).get(0)
        if (tmp is not None):
            tmp.getGrid()
        return tmp

    def createLatLonArea(self, lat, lon, dim):
        # Create a square edit area given a latitude, longitude,
        #  and kilometer dimension for the sides.
        # Example:
        #   area = self.createLatLonArea(40.93, -106.26, 5)
        #
        # If dim is zero, make edit area of the one grid
        #  point closest to the lat/lon value.
        #
        name = self.getLatLonAreaName((lat, lon, dim))
        if dim != 0:
            for x in range(100):
                points = self.makeSquare(lat, lon, dim)
                pointList = []
                for point in points:
                    pointList.append(self.makePoint(point))
                refData = self.makeArea(pointList, refname=name)
                # Make sure we have at least one grid point in
                # the edit area
                if refData.getGrid().isAnyBitsSet():
                    return refData
                # Increment dim and try again
                dim += 0.25
            msg = "\nWARNING!!! EMPTY EDIT AREA. INCREASE LAT/LON AREA DIMENSION!!\n"
            self.log.warning(msg)
            return None
        else:
            # Get grid cell coordinates for lat/lon
            gridLoc = self.getGridLoc()
            cc2D = gridLoc.gridCell(lat, lon)
            # convert grid cell to Grid2DBit with single bit set
            grid2Dbit = JavaGrid2DBit(
                gridLoc.gridSize().x, gridLoc.gridSize().y)
            grid2Dbit.set(int(cc2D.x), int(cc2D.y))
            #refData = gridLoc.convertToReferenceData(grid2Dbit)
            refID = ReferenceID(name)
            refData = ReferenceData(gridLoc, refID, grid2Dbit)
            #refData.setId(refID)
            return refData
    
    def getLatLonAreaName(self, latLonTuple):
        lat, lon, dim = latLonTuple
        name = "Ref" + '%s%s%s' % (lat, lon, dim)
        name = name.replace(".","")
        name = name.replace("-","")
        return name

    def makeSquare(self, lat, lon, km):
        " Make a list of square of given km  around lat,lon"
        # The 222 value should probably by 111.
        latinc = km/222.0
        loninc = math.cos(lat/57.17) * km / 222.0

        latTop = lat + latinc
        latBottom =lat - latinc
        lonLeft = lon - loninc
        lonRight = lon + loninc

        points = []
        points.append(`latTop`+","+ `lonRight`)
        points.append(`latTop`+","+ `lonLeft`)
        points.append(`latBottom`+","+ `lonLeft`)
        points.append(`latBottom`+","+`lonRight`)
        return points

    def makePoint(self, point):
        " Make a CartCoord2D from the point in format: x,y"
        from com.vividsolutions.jts.geom import Coordinate
        ind = string.find(point,",")
        latStr = point[0:ind-1]
        lonStr = point[ind+1:len(point)]
        lat = float(latStr)
        lon = float(lonStr)    
        return Coordinate(lon,lat)

    def makeArea(self, pointList, refname=None):
        " Make a Reference Area with a unique ReferenceID"
        from com.vividsolutions.jts.geom import GeometryFactory, LinearRing, Coordinate, Polygon
        from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData_CoordinateType as CoordinateType
        geomFactory = GeometryFactory()
        import jep
        size = len(pointList)
        if pointList[0] != pointList[size-1]: # closing the loop
            pointList.append(pointList[0])
        pointArray = jep.jarray(len(pointList), Coordinate)
        for i in range(len(pointList)):
            pointArray[i] = pointList[i]
        lr = geomFactory.createLinearRing(pointArray)
        poly = geomFactory.createPolygon(lr, jep.jarray(0, LinearRing))
        polyArray = jep.jarray(1, Polygon)
        polyArray[0] = poly
        region = geomFactory.createMultiPolygon(polyArray)
        if refname is None:
            refname = "Ref" + getTime()    
        refId = ReferenceID(refname)
        refData = ReferenceData(self.__gridLoc, refId, region, CoordinateType.valueOf("LATLON"))
        # randerso: I don't think this is necessary
        # refData.convertToAWIPS()
        return refData

    def getTime(self):
        "Return an ascii string for the current time without spaces or :'s"
        timeStr =  `time.time()`
        timeStr = string.replace(timeStr,".","_")
        return timeStr

    def getIntersectName(self, areaName, localEffectArea):
        name =  "_" + localEffectArea + "_intersect_"+areaName
        name = string.replace(name, " ", "_")
        return name

    def inQuery(self, tree, areaName, queryStr):
        # Check to see if areaName is part of an intersection
        if queryStr[0] == "_":
            queryStr = queryStr[1:]
        queryNames = queryStr.split("_intersect_")
        for queryName in queryNames:
            queryName = queryName.replace("_", " ")
            queryAreaNames = self.getCurrentAreaNames(tree, queryName)
            if areaName in queryAreaNames:
                return 1
        return 0
          
    def unionAreas(self, name, area1, area2):
        # OR the areas (ReferenceData objects)
        # together and return a ReferenceData object
        cpy = ReferenceData(area1)
        refData = cpy.orEquals(area2)
        #refData.convertToLatLon()
        refData.setId(ReferenceID(name))
        refData.getGrid()
        return refData

    def intersectAreas(self, name, area1, area2):
        # AND the areas (ReferenceData objects)
        # together and return a ReferenceData object
        cpy = ReferenceData(area1)
        refData = cpy.andEquals(area2)
        #refData.convertToLatLon()
        refData.setId(ReferenceID(name))
        refData.getGrid()
        return refData

    def getCurrentAreaNames(self, tree, areaName=None):
        # Returns the current list of areaNames being processed.
        # If the areaName is None, use the current area
        # If a Combinations file is being used, there may
        #  be multiple names in the list
        # Otherwise, it will be a list of one edit area name
        #
        # "tree" could be a narrative tree OR argDict
        combinations = None
        if type(tree) is types.DictType:
            # tree is argDict
            if areaName is None:
                editArea, areaLabel = tree["editArea"]
                areaName = editArea.getId().getName()
            else:
                areaLabel = areaName
            combinations = tree["combinations"]
        else:
            # tree
            if areaName is None:
                editArea = tree.get("editArea")
                areaName = editArea.getId().getName()
                areaLabel = tree.get("areaLabel")
            else:
                areaLabel = areaName
            combinations = tree.get("combinations")
        areaNames = [areaName]

        if combinations is not None:
            for comboList, label in combinations:
                if label == areaLabel:
                    areaNames = comboList
        return areaNames

    def saveEditAreas(self, editAreas):
        # Save a list of ReferenceData objects
        refMgr = self.__dataMgr.getRefManager()
        for ea in editAreas:        
            refMgr.saveRefSet(ea)
        
    def getComboNumber(self):
        # Put initial comboNumber from constructor into EditAreaUtils as well.
        self.__comboNumber = self.__comboNumber + 1
        return self.__comboNumber

    def getUnion(self, argDict, areaLabels, areaPrefix):
        GridLoc = self.getIFPClient().getDBGridLocation()
        area = None
        for areaLabel in areaLabels:
            newArea = self.getEditArea(areaLabel, argDict)
            if areaLabels.index(areaLabel) == 0:
                comboNumber = self.getComboNumber()
                label = areaPrefix + `int(time.time())` + `comboNumber`
                refId = ReferenceID(label)
                from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData_CoordinateType as CoordinateType
                coordType = CoordinateType.valueOf('LATLON')
                area = ReferenceData(
                    GridLoc, refId, newArea.getPolygons(coordType), coordType)
                # randerso: I don't think this is necessary
                # area.convertToAWIPS()
            area = self.unionAreas(label, area, newArea)
        return area
    
    def cleanOutEditAreas(self, areaPrefix):
        # Delete all edit areas that have the given areaPrefix
        #time1 = time.time()
        areaList = []
        inventory = self.getIFPClient().getReferenceInventory()
        for areaId in inventory:
            areaName = areaId.name()
            if areaName.find(areaPrefix) == 0:
                areaList.append(areaName)
        self.deleteEditAreas(areaList)
        #print "Time to delete", time.time() - time1

    def deleteEditAreas(self, editAreas):
        # Delete a list of ReferenceData, ReferenceID, or string objects        
        ids = []
        for area in editAreas:
            if type(area) is str:
                ids.append(ReferenceID(area))
            else:
                try:
                    # reference data
                    ids.append(area.getId())
                except:
                    # reference id
                    ids.append(area)            
        refMgr = self.__dataMgr.getRefManager()        
        for ea in ids:            
            refMgr.deleteRefSet(ea, False)
