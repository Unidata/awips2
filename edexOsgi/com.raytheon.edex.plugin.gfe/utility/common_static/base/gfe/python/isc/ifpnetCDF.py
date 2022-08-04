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
# ----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- ---------------------------------------------
# Jul 06, 2009  1995     bphillip  Initial Creation.
# Mar 11, 2013  1759     dgilling  Removed unneeded methods.
# Apr 23, 2013  1937     dgilling  Reimplement WECache to match
#                                  A1, big perf improvement.
# May 23, 2013  1759     dgilling  Remove unnecessary imports.
# Jun 13, 2013  2044     randerso  Updated for changes to TopoDatabaseManager
# Jul 25, 2013  2233     randerso  Improved memory utilization and performance
# Aug 09, 2013  1571     randerso  Changed projections to use the Java
#                                  ProjectionType enumeration
# Sep 20, 2013  2405     dgilling  Clip grids before inserting into cache.
# Oct 22, 2013  2405     rjpeter   Remove WECache and store directly to cube.
# Oct 31, 2013  2508     randerso  Change to use DiscreteGridSlice.getKeys()
# Aug 14, 2014  3526     randerso  Fixed to get sampling definition from
#                                  appropriate site
# Jan 13, 2015  3955     randerso  Changed to use ifpServer.getTopoData
# Feb 17, 2015  4139     randerso  Removed timeFromComponents and dependent
#                                  functions in favor of calendar.timegm
# Apr 23, 2015  4259     njensen   Updated for new JEP API
# May 13, 2015  4427     dgilling  Add siteIdOverride field.
# Aug 06, 2015  4718     dgilling  Optimize casting when using where with
#                                  NumPy 1.9.
# Apr 07, 2016  5539     randerso  Reversed order of parameters/return value in
#                                  collapseKey
#                                  to match order of Wx/Discrete tuple
# May 27, 2016  19014    ryu       Fix rounding issue causing Td to be greater
#                                  than T in output netCDF file.
# Sep 12, 2016  5861     randerso  Remove references to IFPServerConfigManager
#                                  which was largely redundant with IFPServer.
# Oct 31, 2016  5979     njensen   Cast to primitives for compatibility
# Nov 21, 2016  5959     njensen   Removed unused imports and made more pythonic
# Feb 06, 2017  5959     randerso  Removed Java .toString() calls
# Jul 31, 2017  6342     randerso  Removed unused imports. Fixed long standing
#                                  bug in extremaOfSetBits when mask does not overlap grid
# Jan 15, 2018  6867     dgilling  Add exception handler to ensure no NetCDFFile objects
#                                  are left open.
# Feb 15, 2018  6659     dgilling  Allow user level sampling definition files to be used.
# Jan 10, 2019  21059    ryu       Convert boolean variable attribute to int type (Jep
#                                  returned Java booean as int to python prior to v3.7.)
# May 03, 2019  7842     dgilling  Update to use library netcdf4-python.
# Sep 05, 2019  7842     randerso  Fix error handling issue exposed by not having netcdf4
#                                  in sharedmodules.txt
# Jan 20, 2022  8749     dgilling  Optimize attribute storage,
#                                  add additional logging to track performance.
# Apr 27, 2022  8852     dgilling  Fix store*WE functions to ensure WE inventory and grid
#                                  cube sizes remain in sync.
# May 19, 2022  8749     randerso  Add projection attributes back in for Topo
#                                  and lat/lon grids. Renamed storeProjectionAttributes
#                                  to getProjectionAttributes since it no longer stores.
#
##

##
# This is a base file that is not intended to be overridden.
##

import calendar
from collections import OrderedDict
import gzip
import itertools
import logging
import os
import shutil
import stat
import time
import traceback

import netCDF4
import numpy

import JUtil
from com.raytheon.edex.plugin.gfe.server import IFPServer
from com.raytheon.edex.plugin.gfe.smartinit import IFPDB
from com.raytheon.uf.common.dataplugin.gfe.config import ProjectionData
from com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID
from com.raytheon.uf.common.localization import LocalizationContext
from com.raytheon.uf.common.localization import PathManagerFactory
import iscUtil
from org.locationtech.jts.geom import Coordinate

ProjectionType = ProjectionData.ProjectionType
LocalizationType = LocalizationContext.LocalizationType
LocalizationLevel = LocalizationContext.LocalizationLevel

# Original A1 BATCH WRITE COUNT was 10, we found doubling that
# lead to a significant performance increase.
BATCH_WRITE_COUNT = 20
BATCH_DELAY = 0.0
ifpNetcdfLogger = None


## Logging methods ##
def initLogger(logFile=None):
    global ifpNetcdfLogger
    ifpNetcdfLogger = iscUtil.getLogger("ifpnetCDF", logFile, logLevel=logging.INFO)


def logEvent(*msg):
    ifpNetcdfLogger.info(iscUtil.tupleToString(*msg))


def logProblem(*msg):
    ifpNetcdfLogger.error(iscUtil.tupleToString(*msg))


def logException(*msg):
    ifpNetcdfLogger.exception(iscUtil.tupleToString(*msg))


def logVerbose(*msg):
    ifpNetcdfLogger.debug(iscUtil.tupleToString(*msg))


def logDebug(*msg):
    logVerbose(iscUtil.tupleToString(*msg))


def retrieveData(we, inv, clipArea):
    lst = list(inv.keys())
    histDict = OrderedDict()
    cube = None
    keyList = None
    gridType = str(we.getGpi().getGridType())

    # clipped size
    clipSize = (clipArea[1] - clipArea[0] + 1, clipArea[3] - clipArea[2] + 1)
    gridCount = len(inv)
    logDebug(f"retrieveData(): ParmID [{we.getParmid().getParmId()}], gridCount {gridCount}")

    t0 = time.perf_counter()
    if gridType == "SCALAR":
        cube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]), dtype=numpy.float32)
    elif gridType == "VECTOR":
        magCube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]), dtype=numpy.float32)
        dirCube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]), dtype=numpy.float32)
        cube = (magCube, dirCube)
    elif gridType == "WEATHER" or gridType == "DISCRETE":
        cube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]), dtype=numpy.int8)
        keyList = []
    t1 = time.perf_counter()
    logDebug(f"retrieveData(): Time to create cube: {t1-t0}")

    t0 = time.perf_counter()
    cubeIdx = 0
    while len(lst):
        i = lst[:BATCH_WRITE_COUNT]
        javaTRs = [iscUtil.toJavaTimeRange(tr) for tr in i]
        t1 = time.perf_counter()
        gridsAndHist = we.get(javaTRs, True)
        t2 = time.perf_counter()
        size = gridsAndHist.size()
        logDebug(f"retrieveData(): Time to retrieve {size} grids: {t2-t1}")
        for idx in range(size):
            pair = gridsAndHist.get(idx)
            grid = pair.getFirst()
            tr = iscUtil.transformTime(grid.getValidTime())
            encodeGridSlice(grid, gridType, clipArea, cube, cubeIdx, keyList)
            cubeIdx += 1
            histDict[tr] = encodeGridHistory(pair.getSecond())
        t3 = time.perf_counter()
        logDebug(f"retrieveData(): Time to encode {size} grids: {t3-t2}")
        lst = lst[BATCH_WRITE_COUNT:]
        time.sleep(BATCH_DELAY)
        t4 = time.perf_counter()
        logDebug(f"retrieveData(): Total time to retrieve/encode inventory: {t4-t0}")

    if len(histDict) != gridCount:
        t0 = time.perf_counter()

        # retrieved less grids than originally expected, purge ran?
        gridCount = len(histDict)
        timesToRemove = set(inv.keys()) - set(histDict.keys())
        for tr in timesToRemove:
            del inv[tr]

        if gridType == "SCALAR":
            oldCube = cube
            cube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]), dtype=numpy.float32)
            for idx in range(gridCount):
                cube[idx] = oldCube[idx]
        elif gridType == "VECTOR":
            oldMagCube = magCube
            magCube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]), dtype=numpy.float32)
            oldDirCube = dirCube
            dirCube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]), dtype=numpy.float32)
            cube = (magCube, dirCube)
            for idx in range(gridCount):
                magCube[idx] = oldMagCube[idx]
                dirCube[idx] = oldDirCube[idx]
        elif gridType == "WEATHER" or gridType == "DISCRETE":
            oldCube = cube
            cube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]), dtype=numpy.int8)
            for idx in range(gridCount):
                cube[idx] = oldCube[idx]
        t1 = time.perf_counter()
        logDebug(f"retrieveData(): Time to resolve purged grids: {t1-t0}")
    return (cube, histDict, keyList)


###-------------------------------------------------------------------------###
### cube and keyList are out parameters to be filled by this method, idx is the index into cube to use
def encodeGridSlice(grid, gridType, clipArea, cube, idx, keyList):
    if gridType == "SCALAR":
        cube[idx] = clipToExtrema(grid.getNDArray(), clipArea)
    elif gridType == "VECTOR":
        vecGrids = grid.getNDArray()
        cube[0][idx] = clipToExtrema(vecGrids[0], clipArea)
        cube[1][idx] = clipToExtrema(vecGrids[1], clipArea)
    elif gridType == "WEATHER" or gridType == "DISCRETE":
        keys = grid.getKeys()
        gridKeys = []

        for theKey in keys:
            gridKeys.append(str(theKey))
        keyList.append(gridKeys)
        cube[idx] = clipToExtrema(grid.getNDArray(), clipArea)


def encodeGridHistory(histories):
    retVal = []
    for i in range(histories.size()):
        retVal.append(histories.get(i).getCodedString())
    return tuple(retVal)


###-------------------------------------------------------------------------###
### Processes the parm list.  If the list is empty, get all of the parms
### from the database.  If any are missing _SFC add it.
def processParmList(argDict, db):
    parmList = argDict['parmList']
    dbList = db.getKeys()
    if len(parmList) == 0:
        parmList = JUtil.javaStringListToPylist(dbList)

    # now add _SFC to all parmNames with no underscore
    for i in range(0, len(parmList)):
        if '_' not in parmList[i]:
            parmList[i] = parmList[i] + "_SFC"

    # now verify that the parm is in the database
    final = []
    for p in parmList:
        if dbList.contains(p):
            final.append(p)
    return final


###-------------------------------------------------------------------------###
### Returns true if the specified time is contained within the timeRange
def contains(timerange, time):
    if timerange[1] - timerange[0]:
        return ((time >= timerange[0]) and (time < timerange[1]))
    return time == timerange[0]


###-------------------------------------------------------------------------###
### Returns intersection time range of two time ranges, if no intersection
###  then None is returned.
def intersection(tr1, tr2):
    if tr1[0] < tr2[0]:
        startTime = tr2[0]
    else:
        startTime = tr1[0]
    if tr1[1] > tr2[1]:
        endTime = tr2[1]
    else:
        endTime = tr1[1]
    if startTime >= endTime:
        return None   # no intersection
    else:
        return (startTime, endTime)


###-------------------------------------------------------------------------###
def overlaps(tr1, tr2):
    "Returns true if the specified time ranges overlap"
    if contains(tr2, tr1[0]) or contains(tr1, tr2[0]):
        return 1
    return 0


###-------------------------------------------------------------------------###
### Makes an integer from the specified string in seconds since 1-Jan-1970 00Z
def getIntTime(timeStr):
    "Create an Integer time from a string: YYYYMMDD_HHMM"

    try:
        timeTuple = time.strptime(timeStr, "%Y%m%d_%H%M")
    except:
        logProblem(timeStr, \
          "is not a valid time string.  Use YYYYMMDD_HHMM", traceback.format_exc())
        s = timeStr + " is not a valid time string.  Use YYYYMMDD_HHMM"
        raise SyntaxError(s)
    return calendar.timegm(timeTuple)


###-------------------------------------------------------------------------###
### Makes a TimeRange from the input string of the form YYYYMMDD_HHMM.
def makeTimeRange(startString, endString):
    "Makes a timeRange from the specified time strings."
    try:
        t1 = getIntTime(startString)
        t2 = getIntTime(endString)
    except:
        raise Exception("Can't decode YYYYMMDD_HHMM string")

    return (t1, t2)


###-------------------------------------------------------------------------###
def timeRangeAsString(tr):
    "Prints timeRange in YYYYMMDD_HHMM format"
    return time.strftime("%Y%m%d_%H%M", time.gmtime(tr[0])) + " --- " \
 +time.strftime("%Y%m%d_%H%M", time.gmtime(tr[1]))


###-------------------------------------------------------------------------###
def extremaOfSetBits(mask):
    "Returns tuple of extrema of set bits (minx,maxx, miny,maxy)"
    nz = numpy.nonzero(mask)

    minx = miny = 0
    maxx = mask.shape[1] - 1
    maxy = mask.shape[0] - 1

    if nz[1].any():
       minx = nz[1].min()
       maxx = nz[1].max()

    if nz[0].any():
       miny = nz[0].min()
       maxy = nz[0].max()

    return (minx, maxx, miny, maxy)


###------------------------------------------------------------------------###
def clipToExtrema(grid, clipArea):
    "Clips grid to info in clipArea: (minx, maxx, miny, maxy)"
    if clipArea[0] == -1:
        return grid    # no clipping at all
    minx = clipArea[0]
    maxx = clipArea[1]
    miny = clipArea[2]
    maxy = clipArea[3]
    return grid[miny:maxy + 1, minx:maxx + 1]


###-------------------------------------------------------------------------###
### Returns a list of dimension names based on the tuple of integer sizes,
### as well as the names of the dimensions.
### Adds the dimension to the netCDF file, if necessary.
### Special case, if dimension of zero, use a different name.
def getDims(cdfFile, dimSizes, dimNames):
    if len(dimSizes) != len(dimNames):
        raise Exception("dimSizes and dimNames not same size")
    dimList = list(dimSizes)
    dimNames = list(dimNames)
    actDimNames = []
    existingDimList = list(cdfFile.dimensions.keys())

    for x in range(len(dimList)):
        dimName = "DIM_" + str(dimSizes[x])
        actDimNames.append(dimName)
        if dimName not in existingDimList:
            cdfFile.createDimension(dimName, dimSizes[x])
            existingDimList.append(dimName)

    return tuple(actDimNames)


###-------------------------------------------------------------------------###
def getMaskGrid(ifpServer, editAreaName, dbId):
    #make a mask with all bits set (y,x)
    domain = ifpServer.getConfig().dbDomain()
    mask = numpy.ones((int(domain.getNy()), int(domain.getNx())), dtype=numpy.bool)

    if editAreaName == "":
        return mask

    # get the edit area
    try:
        mask = iscUtil.getEditArea(editAreaName, DatabaseID(dbId).getSiteId())
        mask.setGloc(domain)
        mask = mask.getGrid().getNDArray().astype(numpy.bool)
    except:
        logProblem("Edit area:", editAreaName, "not found. Storing entire grid.", traceback.format_exc())

    return mask


###-------------------------------------------------------------------------###
def storeLatLonGrids(ifpServer, cdfFile, krunch, clipArea):
    # Get the grid location and projection information
    gridLoc = ifpServer.getConfig().dbDomain()
    pDict = gridLoc.getProjection()

    latLonGrid = gridLoc.getLatLonGrid()

    latLonGrid = numpy.reshape(latLonGrid, (2, int(gridLoc.getNy()), int(gridLoc.getNx())), order='F')

    # clip them
    lonGrid = clipToExtrema(latLonGrid[0], clipArea)
    latGrid = clipToExtrema(latLonGrid[1], clipArea)

    # recast the arrays for compatibility with netCDF
    lonGrid = numpy.flipud(lonGrid)
    latGrid = numpy.flipud(latGrid)

    # clipped size
    clipSize = (clipArea[1] - clipArea[0] + 1, clipArea[3] - clipArea[2] + 1)

    newsize = (clipSize[1], clipSize[0])  #y,x
    latGrid = numpy.resize(latGrid, newsize)
    lonGrid = numpy.resize(lonGrid, newsize)

    dims = getDims(cdfFile, latGrid.shape, ("y", "x"))

    # store latitude grid
    attrs = {}
    if krunch:
        latVar = cdfFile.createVariable("latitude", 'h', dims)
        latGrid = (latGrid * 100).astype(numpy.int16)
        latVar[:] = latGrid
        attrs["dataMultiplier"] = 0.01
        attrs["dataOffset"] = 0
    else:
        latVar = cdfFile.createVariable("latitude", 'f', dims)
        latVar[:] = latGrid

    # make the netCDF attributes
    # Descriptive Name
    attrs["descriptiveName"] = "latitude"

    # coordinate information
    origGridSize = Coordinate(float(gridLoc.getNx()), float(gridLoc.getNy()))
    origOrigin = gridLoc.getOrigin()
    origExtent = gridLoc.getExtent()

    cellSize = (origExtent.x / (origGridSize.x - 1),
                origExtent.y / (origGridSize.y - 1))
    clippedExtent = (cellSize[0] * (clipSize[0] - 1),
      cellSize[1] * (clipSize[1] - 1))
    domainOffset = (clipArea[0] * cellSize[0], (origGridSize.y - clipArea[3] - 1) * cellSize[1])

    clippedOrigin = (origOrigin.x + domainOffset[0] ,
                     origOrigin.y + domainOffset[1])

    # gridSize, domain origin/extent
    attrs["gridSize"] = clipSize
    attrs["domainOrigin"] = clippedOrigin
    attrs["domainExtent"] = clippedExtent

    #units
    attrs["units"] = "degrees"
    # projection info - store whatever is in the dictionary
    t0 = time.perf_counter()
    projectionAttrs = getProjectionAttributes(pDict)
    t1 = time.perf_counter()
    logDebug(f"storeLatLonGrids(): calc projection attributes: {t1-t0}")

    attrs.update(projectionAttrs)
    t0 = time.perf_counter()
    latVar.setncatts(attrs)
    t1 = time.perf_counter()
    logDebug(f"storeLatLonGrids(): call Variable.setncatts(): {t1-t0}")

    #  store longitude grid
    attrs = {}
    if krunch:
        lonVar = cdfFile.createVariable("longitude", 'h', dims)
        lonGrid = (lonGrid * 100).astype(numpy.int16)
        lonVar[:] = lonGrid
        attrs["dataMultiplier"] = 0.01
        attrs["dataOffset"] = 0
    else:
        lonVar = cdfFile.createVariable("longitude", 'f', dims)
        lonVar[:] = lonGrid

    # Descriptive Name
    attrs["descriptiveName"] = "longitude"

    # gridSize, domain origin/extent
    attrs["gridSize"] = clipSize
    attrs["domainOrigin"] = clippedOrigin
    attrs["domainExtent"] = clippedExtent

    #units
    attrs["units"] = "degrees"

    # projection info - store whatever is in the dictionary
    attrs.update(projectionAttrs)

    t0 = time.perf_counter()
    lonVar.setncatts(attrs)
    t1 = time.perf_counter()
    logDebug(f"storeLatLonGrids(): call Variable.setncatts(): {t1-t0}")

    logEvent("Saved Latitude/Longitude Grid")


###-------------------------------------------------------------------------###
def storeTopoGrid(ifpServer, cdfFile, clipArea):
    "Stores the topo grid in the database"

    # Get the grid location and projection information
    gridLoc = ifpServer.getConfig().dbDomain()
    pDict = gridLoc.getProjection()

    # Get the topo grid
    topoGrid = ifpServer.getTopoData(gridLoc).getPayload().getNDArray()
    topoGrid = clipToExtrema(topoGrid, clipArea)
    topoGrid = numpy.flipud(topoGrid)

    # clipped size
    clipGridSize = (clipArea[1] - clipArea[0] + 1, clipArea[3] - clipArea[2] + 1)

    newsize = (clipGridSize[1], clipGridSize[0])  #y,x
    topoGrid = numpy.resize(topoGrid, newsize)

    dims = getDims(cdfFile, topoGrid.shape, ("y", "x"))

    # create the netcdf variable
    var = cdfFile.createVariable("Topo", 'h', dims)

    # round to nearest foot
    topoGrid = numpy.array((topoGrid + 0.5) / 1).astype(numpy.int16)

    var[:] = topoGrid

    # make the netCDF attributes
    # Descriptive Name
    attrs = {}
    attrs["descriptiveName"] = "Topography"

    # coordinate information
    origGridSize = Coordinate(float(str(gridLoc.getNx())), float(str(gridLoc.getNy())))
    origOrigin = gridLoc.getOrigin()
    origExtent = gridLoc.getExtent()

    cellSize = (origExtent.x / (origGridSize.x - 1),
                origExtent.y / (origGridSize.y - 1))
    clippedExtent = (cellSize[0] * (clipGridSize[0] - 1),
      cellSize[1] * (clipGridSize[1] - 1))
    domainOffset = (clipArea[0] * cellSize[0], (origGridSize.y - clipArea[3] - 1) * cellSize[1])

    clippedOrigin = (origOrigin.x + domainOffset[0] ,
                     origOrigin.y + domainOffset[1])

    # gridSize
    attrs["gridSize"] = clipGridSize

    # Domain origin
    attrs["domainOrigin"] = clippedOrigin
    # Domain extent
    attrs["domainExtent"] = clippedExtent
    #units
    attrs["units"] = "ft"
    # projection info - store whatever is in the dictionary
    t0 = time.perf_counter()
    projectionAttrs = getProjectionAttributes(pDict)
    t1 = time.perf_counter()
    logDebug(f"storeTopoGrid(): calc projection attributes: {t1-t0}")

    attrs.update(projectionAttrs)
    t0 = time.perf_counter()
    var.setncatts(attrs)
    t1 = time.perf_counter()
    logDebug(f"storeTopoGrid(): call Variable.setncatts(): {t1-t0}")

    logEvent("Saved Topo Grid")


###-------------------------------------------------------------------------###
###
def storeGridDataHistory(cdfFile, we, histDict):
    "Stores the Grid Data history string for each grid in we."

    # get the maximum size of the history string
    maxHistSize = 0
    histList = []
    for (tr, his) in histDict.items():
        hisString = ''
        for i, h in enumerate(his):
            hisString = hisString + str(h)
            if i != len(his) - 1:
                hisString = hisString + " ^"
        histList.append(hisString)
        maxHistSize = max(maxHistSize, len(hisString))

    # Make the history variable and fill it
    histShape = (len(histList), maxHistSize + 1)
    histCube = numpy.zeros(histShape, 'c')
    for slot, hisString in enumerate(histList):
        histCube[slot] = netCDF4.stringtoarr(hisString, histShape[1])

    # make the history variable anyway.  iscMosaic needs it.
    elemName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()
    dimNames = ["ngrids_" + elemName, "histLen_" + elemName]
    dims = getDims(cdfFile, histShape, dimNames)
    varName = elemName + "_GridHistory"

    var = cdfFile.createVariable(varName, 'c', dims)

    if len(histList) > 0:
        # store the cube in the netCDF cdfFile
        var[:] = histCube
    return


###-------------------------------------------------------------------------###
###
def calcKrunchValues(we):
    #Based on the weather element, will return information pertaining
    #to the dataType, multiplier, offset, and missing value to use for this
    #element.  Returns (dataType, multiplier, offset, missingValue, pythonType)

    maxV = we.getGpi().getMaxValue()
    minV = we.getGpi().getMinValue()
    precision = pow(10, we.getGpi().getPrecision())

    nentries = ((maxV - minV) * precision) + 1

    # check for byte possibilities
    if nentries <= pow(2, 8) - 1:
        multiplier = precision
        offset = 0
        minVarValue = -126
        maxVarValue = 127
        if minV * multiplier < minVarValue:
            offset = minV - minVarValue // multiplier
        if maxV * multiplier > maxVarValue:
            offset = maxV - maxVarValue // multiplier
        missingValue = -127
        dataType = "b"
        pythonType = numpy.int8

    # check for short possibilities
    elif nentries <= pow(2, 16) - 2:
        multiplier = precision
        offset = 0
        maxVarValue = pow(2, 15) - 1
        minVarValue = -(pow(2, 15) - 2)
        if minV * multiplier < minVarValue:
            offset = minV - minVarValue // multiplier
        if maxV * multiplier > maxVarValue:
            offset = maxV - maxVarValue // multiplier
        missingValue = minVarValue - 1
        dataType = "h"
        pythonType = numpy.int16

    # else full 32-bit float processing, no krunching needed
    else:
        multiplier = None
        offset = None
        dataType = "f"
        missingValue = -30000.0
        pythonType = numpy.float32
    return (dataType, multiplier, offset, missingValue, pythonType)


###-------------------------------------------------------------------------###
def getProjectionAttributes(projectionData):
    projectionAttrs = {}
    # store the attributes common to all projections
    projectionAttrs["latLonLL"] = (projectionData.getLatLonLL().x, projectionData.getLatLonLL().y)
    projectionAttrs["latLonUR"] = (projectionData.getLatLonUR().x, projectionData.getLatLonUR().y)
    projectionAttrs["gridPointLL"] = (projectionData.getGridPointLL().x, projectionData.getGridPointLL().y)
    projectionAttrs["gridPointUR"] = (projectionData.getGridPointUR().x, projectionData.getGridPointUR().y)
    projectionType = projectionData.getProjectionType()
    projectionAttrs["projectionType"] = str(projectionType)

    # Now store the projection specific attributes
    if ProjectionType.LAMBERT_CONFORMAL.equals(projectionType):
        projectionAttrs["latLonOrigin"] = (projectionData.getLatLonOrigin().x, projectionData.getLatLonOrigin().y)
        projectionAttrs["stdParallelOne"] = projectionData.getStdParallelOne()
        projectionAttrs["stdParallelTwo"] = projectionData.getStdParallelTwo()

    if ProjectionType.POLAR_STEREOGRAPHIC.equals(projectionType):
        projectionAttrs["lonOrigin"] = projectionData.getLonOrigin()

    if ProjectionType.MERCATOR.equals(projectionType):
        projectionAttrs["lonCenter"] = projectionData.getLonCenter()

    return projectionAttrs


###-------------------------------------------------------------------------###
def storeWEAttributes(var, we, timeList, databaseID, clipArea, siteIdOverride, extraAttrs):
    "Stores attributes in the netCDF file for any weather element"

    attrs = {}

    # Note that geo information is modified based on the clip info.

    # TimeRanges
    attrs["validTimes"] = list(itertools.chain.from_iterable(timeList))

    # Descriptive Name
    attrs["descriptiveName"] = we.getGpi().getDescriptiveName()

    # gridSize
    clipGridSize = (clipArea[1] - clipArea[0] + 1, clipArea[3] - clipArea[2] + 1)
    attrs["gridSize"] = clipGridSize

    # Domain origin and extent
    gridLoc = we.getGpi().getGridLoc()
    origGridSize = Coordinate(float(str(gridLoc.getNx())), float(str(gridLoc.getNy())))
    origOrigin = gridLoc.getOrigin()
    origExtent = gridLoc.getExtent()

    cellSize = (origExtent.x / (origGridSize.x - 1),
                origExtent.y / (origGridSize.y - 1))

    clippedExtent = (cellSize[0] * (clipGridSize[0] - 1),
      cellSize[1] * (clipGridSize[1] - 1))

    domainOffset = (clipArea[0] * cellSize[0], (origGridSize.y - clipArea[3] - 1) * cellSize[1])

    clippedOrigin = (origOrigin.x + domainOffset[0] ,
                     origOrigin.y + domainOffset[1])

    attrs["domainOrigin"] = clippedOrigin
    attrs["domainExtent"] = clippedExtent

    # Min/Max allowable values
    attrs["minMaxAllowedValues"] = (we.getGpi().getMinValue(), we.getGpi().getMaxValue())

    # determine correct siteID to write to netCDF file
    # we needed this siteIdOverride incase we're exporting grids from a subdomain
    srcSiteId = we.getParmid().getDbId().getSiteId()
    destSideId = srcSiteId
    if siteIdOverride:
        destSideId = siteIdOverride
    fixedDbId = databaseID.replace(srcSiteId + "_", destSideId + "_", 1)

    # data type
    attrs["gridType"] = str(we.getGpi().getGridType())
    # database ID
    attrs["databaseID"] = fixedDbId
    # siteID
    attrs["siteID"] = destSideId
    # units
    attrs["units"] = we.getGpi().getUnitString()
    # level
    attrs["level"] = we.getParmid().getParmLevel()
    # timeConstraints
    attrs["timeConstraints"] = (we.getGpi().getTimeConstraints().getStartTime(), we.getGpi().getTimeConstraints().getDuration(), we.getGpi().getTimeConstraints().getRepeatInterval())
    # precision
    attrs["precision"] = we.getGpi().getPrecision()

    # rate parm
    attrs["rateDependent"] = int(we.getGpi().isRateParm())

    # projection info - store whatever is in the dictionary
    t0 = time.perf_counter()
    projectionAttrs = getProjectionAttributes(gridLoc.getProjection())
    t1 = time.perf_counter()
    logDebug(f"storeWEAttributes(): calc projection attributes: {t1-t0}")

    attrs.update(projectionAttrs)
    attrs.update(extraAttrs)
    t0 = time.perf_counter()
    var.setncatts(attrs)
    t1 = time.perf_counter()
    logDebug(f"storeWEAttributes(): call Variable.setncatts(): {t1-t0}")

    return


def findOverlappingTimes(trList, timeRange):
   items = [(tr, interTr) for tr, interTr in ((t, intersection(t, timeRange)) for t in trList) if interTr is not None]
   return OrderedDict(items)


###-------------------------------------------------------------------------###
### Stores the specified Scalar WE in the netCDF file whose grids fall within
### the specified timeRange.
def storeScalarWE(we, trList, cdfFile, timeRange, databaseID,
                  invMask, trim, clipArea, krunch, siteIdOverride):
    "Stores a weather element to the netCDF file"

    # get the data and store it in a Numeric array.
    t0 = time.perf_counter()
    overlappingTimes = findOverlappingTimes(trList, timeRange)
    t1 = time.perf_counter()
    logDebug(f"storeScalarWE(): Time to calculate overlapping times: {t1-t0}")

    (cube, histDict, keyList) = retrieveData(we, overlappingTimes, clipArea)
    gridCount = len(cube)

    if we.getGpi().isRateParm():
        for (i, (ot, it)) in enumerate(overlappingTimes.items()):
            if ot != it:
                durRatio = (it[1] - it[0]) / (ot[1] - ot[0])
                cube[i] *= durRatio

    ### Make sure we found some grids
    # make the variable name
    varName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()

    if len(cube) == 0:
        logVerbose("No", varName, "grids found")

    #get the dimension List
    dimNames = ["ngrids_" + varName, "y", "x"]
    dims = getDims(cdfFile, cube.shape, dimNames)

    # Round the values according to the precision
    if trim:
        if krunch:
            dataType, multiplier, offset, fillValue, pythonType = \
              calcKrunchValues(we)
        else:
            dataType, multiplier, offset, fillValue, pythonType = \
             ('f', None, None, -30000.0, numpy.float32)

        # krunch
        if multiplier is not None:
            cube -= offset
            cube *= multiplier
            numpy.floor(cube + 0.5, out=cube)
        # normal trim
        else:
            digits = we.getGpi().getPrecision()
            numpy.around(cube, digits, cube)
        cube = cube.astype(pythonType)

    else:
        dataType, multiplier, offset, fillValue, pythonType = \
         ('f', None, None, -30000.0, numpy.float32)

    # mask the data
    cube[:, invMask] = fillValue

    # create the variable
    var = cdfFile.createVariable(varName, dataType, dims)
    extraAttrs = {"fillValue": fillValue, }
    if multiplier is not None:
        extraAttrs["dataMultiplier"] = 1.0 / multiplier
        extraAttrs["dataOffset"] = offset
    # Store the attributes
    t0 = time.perf_counter()
    storeWEAttributes(var, we, list(overlappingTimes.values()), databaseID, clipArea, siteIdOverride, extraAttrs)
    t1 = time.perf_counter()
    logDebug(f"storeScalarWE(): Time to store WE attributes for parm [{varName}]: {t1-t0}")

    # Save the grids to the netCDF cdfFile
    t0 = time.perf_counter()
    for i in range(len(cube)):
        var[i] = numpy.flipud(cube[i])
    t1 = time.perf_counter()
    logDebug(f"storeScalarWE(): Time to store inventory for parm [{varName}]: {t1-t0}")

    ## Extract the GridDataHistory info and save it
    t0 = time.perf_counter()
    storeGridDataHistory(cdfFile, we, histDict)
    t1 = time.perf_counter()
    logDebug(f"storeScalarWE(): Time to store grid history for parm [{varName}]: {t1-t0}")

    logEvent("Saved", gridCount, varName, " grids")

    return gridCount


###-------------------------------------------------------------------------###
### Stores the specified Vector WE in the netCDF cdfFile whose grids fall within
### the specified timeRange.
def storeVectorWE(we, trList, cdfFile, timeRange,
                  databaseID, invMask, trim, clipArea, krunch, siteIdOverride):
    "Stores a vector weather element to the netCDF file"

    # get the data and store it in a Numeric array.
    t0 = time.perf_counter()
    overlappingTimes = findOverlappingTimes(trList, timeRange)
    t1 = time.perf_counter()
    logDebug(f"storeVectorWE(): Time to find overlapping times: {t1-t0}")

    ((magCube, dirCube), histDict, keyList) = retrieveData(we, overlappingTimes, clipArea)
    gridCount = len(magCube)

    if we.getGpi().isRateParm():
        for (i, (ot, it)) in enumerate(overlappingTimes.items()):
            if ot != it:
                durRatio = (it[1] - it[0]) / (ot[1] - ot[0])
                cube[i] *= durRatio

    varName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()

    ### Make sure we found some grids
    if len(magCube) == 0:
        logVerbose("No", varName, "grids found")

    # make the variable name
    magVarName = we.getParmid().getParmName() + "_Mag_" + we.getParmid().getParmLevel()
    dirVarName = we.getParmid().getParmName() + "_Dir_" + we.getParmid().getParmLevel()

    #get the dimension List
    dimNames = ["ngrids_" + varName, "y", "x"]
    dims = getDims(cdfFile, magCube.shape, dimNames)

    # Round the values according to the precision
    if trim:
        if krunch:
            mformat, mmultiplier, moffset, mfillValue, mpythonType = \
              calcKrunchValues(we)
            dformat, dmultiplier, doffset, dfillValue, dpythonType = \
              ('b', 0.1, 0.0, -127, numpy.int8)
        else:
            mformat, mmultiplier, moffset, mfillValue, mpythonType = \
             ('f', None, None, -30000.0, numpy.dtype(numpy.float32))
            dformat, dmultiplier, doffset, dfillValue, dpythonType = \
             ('f', None, None, -30000.0, numpy.float32)

        # krunch magnitude
        if mmultiplier is not None:
            magCube -= moffset
            magCube *= mmultiplier
            numpy.around(magCube, out=magCube)

        # normal trim for magnitude
        else:
            digits = we.getGpi().getPrecision()
            numpy.around(magCube, digits, magCube)
        magCube = magCube.astype(mpythonType)

        # krunch direction
        if dmultiplier is not None:
            dirCube -= doffset
            dirCube *= dmultiplier
            numpy.around(dirCube, out=dirCube)

        # normal trim for direction
        else:
            numpy.around(dirCube, -1, dirCube)
            dirCube[numpy.greater_equal(dirCube, 360.0)] -= 360.0
        dirCube = dirCube.astype(dpythonType)

    else:
        mformat, mmultiplier, moffset, mfillValue, mpythonType = \
          ('f', None, None, -30000.0, numpy.float32)
        dformat, dmultiplier, doffset, dfillValue, dpythonType = \
          ('f', None, None, -30000.0, numpy.float32)

    magCube[:, invMask] = mfillValue
    dirCube[:, invMask] = dfillValue

    # create the variable
    magVar = cdfFile.createVariable(magVarName, mformat, dims)
    magExtraAttrs = {"descriptiveName": f"{we.getGpi().getDescriptiveName()} Magnitude",
                     "fillValue": mfillValue,
                     }
    if mmultiplier is not None:
        magExtraAttrs["dataMultiplier"] = 1.0 / mmultiplier
        magExtraAttrs["dataOffset"] = moffset
    t0 = time.perf_counter()
    storeWEAttributes(magVar, we, list(overlappingTimes.values()), databaseID, clipArea, siteIdOverride, magExtraAttrs)
    t1 = time.perf_counter()
    logDebug(f"storeVectorWE(): Time to store WE attributes for parm [{magVarName}]: {t1-t0}")

    dirVar = cdfFile.createVariable(dirVarName, dformat, dims)
    dirExtraAttrs = {"descriptiveName": f"{we.getGpi().getDescriptiveName()} Direction",
                     "fillValue": dfillValue,
                     "minMaxAllowedValues": (0.0, 360.0),
                     "units": "degrees",

                     }
    if dmultiplier is not None:
        dirExtraAttrs["dataMultiplier"] = 1.0 / dmultiplier
        dirExtraAttrs["dataOffset"] = doffset
    if trim:
        dirPrecision = -1
    else:
        dirPrecision = 0
    dirExtraAttrs["precision"] = dirPrecision
    t0 = time.perf_counter()
    storeWEAttributes(dirVar, we, list(overlappingTimes.values()), databaseID, clipArea, siteIdOverride, dirExtraAttrs)
    t1 = time.perf_counter()
    logDebug(f"storeVectorWE(): Time to store WE attributes for parm [{dirVarName}]: {t1-t0}")

    # Save the grid to the netCDF cdfFile
    t0 = time.perf_counter()
    for i in range(len(magCube)):
        magVar[i] = numpy.flipud(magCube[i])
        dirVar[i] = numpy.flipud(dirCube[i])
    t1 = time.perf_counter()
    logDebug(f"storeVectorWE(): Time to store grids for parm [{varName}]: {t1-t0}")

    ## Extract the GridDataHistory info and save it
    t0 = time.perf_counter()
    storeGridDataHistory(cdfFile, we, histDict)
    t1 = time.perf_counter()
    logDebug(f"storeVectorWE(): Time to store grid data history for parm [{varName}]: {t1-t0}")

    logEvent("Saved", gridCount, varName, "grids")

    return gridCount * 2   #vector has two grids


###-------------------------------------------------------------------------###
# Collapse key and bytes. (for discrete and weather)
### Returns tuple of (updated grid, updated key)
def collapseKey(grid, keys):
    #make list of unique indexes in the grid
    flatGrid = grid.flat
    used = numpy.zeros((len(keys)), dtype=numpy.bool)
    for n in range(flatGrid.__array__().shape[0]):
        used[0xFF & flatGrid[n]] = True

    #make reverse map
    indexList = []
    newKeys = []
    j = 0
    for i in range(len(keys)):
       if used[i]:
           indexList.append(j)
           newKeys.append(keys[i])
           j = j + 1
       else:
           indexList.append(-1)

    # modify the data
    newGrid = grid
    for k in range(len(indexList)):
       mask = numpy.equal(numpy.int8(k), grid)
       newGrid = numpy.where(mask, numpy.int8(indexList[k]), newGrid).astype(numpy.int8)

    return (newGrid, newKeys)


###-------------------------------------------------------------------------###
# Stores the specified Weather WE in the netCDF file whose grids fall within
### the specified timeRange.
def storeWeatherWE(we, trList, cdfFile, timeRange, databaseID, invMask, clipArea, siteIdOverride):
    "Stores the Weather weather element to the netCDF file"

    # get the data and store it in a Numeric array.
    t0 = time.perf_counter()
    overlappingTimes = findOverlappingTimes(trList, timeRange)
    t1 = time.perf_counter()
    logDebug(f"storeWeatherWE(): Time to find overlapping times: {t1-t0}")

    (byteCube, histDict, keyList) = retrieveData(we, overlappingTimes, clipArea)
    gridCount = len(histDict)

    # make the variable name
    varName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()

    ### Make sure we found some grids
    if len(byteCube) == 0:
        logVerbose("No", varName, "grids found")

    #get the dimension List
    dimNames = ["ngrids_" + varName, "y", "x"]
    dims = getDims(cdfFile, byteCube.shape, dimNames)

    # create the netCDF variable - 'b' for byte type
    var = cdfFile.createVariable(varName, 'b', dims)

    # Store the attributes
    fillValue = -127
    extraAttrs = {"fillValue": fillValue, }
    t0 = time.perf_counter()
    storeWEAttributes(var, we, list(overlappingTimes.values()), databaseID, clipArea, siteIdOverride, extraAttrs)
    t1 = time.perf_counter()
    logDebug(f"storeWeatherWE(): Time to store WE attributes for parm [{varName}]: {t1-t0}")

    #  Process the weather keys so we store only what is necessary

    for g in range(byteCube.shape[0]):
        (byteCube[g], keyList[g]) = collapseKey(byteCube[g], keyList[g])

    # Mask the values
    byteCube[:, invMask] = fillValue

    # Save the grids to the netCDF cdfFile
    t0 = time.perf_counter()
    for i in range(len(byteCube)):
        var[i] = numpy.flipud(byteCube[i])
    t1 = time.perf_counter()
    logDebug(f"storeWeatherWE(): Time to store inventory for parm [{varName}]: {t1-t0}")

    t0 = time.perf_counter()
    # Find the max number of keys and max length for all keys
    maxKeyCount = 1
    maxKeySize = 0

    for k in keyList:
        if len(k) > maxKeyCount:
            maxKeyCount = len(k)

        for s in k:
            if len(s) > maxKeySize:
                maxKeySize = len(s)

    # create a new netCDF variable to hold the weather keys
    wxShape = (gridCount, maxKeyCount, maxKeySize + 1) # zero byte at
                                                       # the end
    dimNames = ["ngrids_" + varName, "nkeys_" + varName, "keylen_" + varName]
    dims = getDims(cdfFile, wxShape, dimNames)
    keyVarName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel() + "_wxKeys"
    keyVar = cdfFile.createVariable(keyVarName, 'c', dims)

    chars = numpy.zeros(wxShape, 'c')

    # now save the weather keys in the netCDF cdfFile
    for g in range(0, gridCount):
        for k in range(0, len(keyList[g])):
            for c in range(0, len(keyList[g][k])):
                chars[g][k][c] = keyList[g][k][c]
    if len(byteCube):
        keyVar[:] = chars
    t1 = time.perf_counter()
    logDebug(f"storeWeatherWE(): Time to store Wx keys for parm [{varName}]: {t1-t0}")

    ## Extract the GridDataHistory info and save it
    t0 = time.perf_counter()
    storeGridDataHistory(cdfFile, we, histDict)
    t1 = time.perf_counter()
    logDebug(f"storeWeatherWE(): Time to store grid data history for parm [{varName}]: {t1-t0}")

    logEvent("Saved", gridCount, varName, "grids")

    return gridCount


###-------------------------------------------------------------------------###
# Stores the specified Discrete WE in the netCDF cdfFile whose grids fall within
### the specified timeRange.
def storeDiscreteWE(we, trList, cdfFile, timeRange, databaseID, invMask, clipArea, siteIdOverride):
    "Stores the Weather weather element to the netCDF file"

    # get the data and store it in a Numeric array.
    t0 = time.perf_counter()
    overlappingTimes = findOverlappingTimes(trList, timeRange)
    t1 = time.perf_counter()
    logDebug(f"storeDiscreteWE(): Time to find overlapping times: {t1-t0}")

    (byteCube, histDict, keyList) = retrieveData(we, overlappingTimes, clipArea)
    gridCount = len(histDict)

    # make the variable name
    varName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()

    ### Make sure we found some grids
    if len(byteCube) == 0:
        logVerbose("No", varName, "grids found")

    #get the dimension List
    dimNames = ["ngrids_" + varName, "y", "x"]
    dims = getDims(cdfFile, byteCube.shape, dimNames)

    # create the netCDF variable - 'b' for byte type
    var = cdfFile.createVariable(varName, 'b', dims)

    # Store the attributes
    fillValue = -127
    extraAttrs = {"fillValue": fillValue, }
    t0 = time.perf_counter()
    storeWEAttributes(var, we, list(overlappingTimes.values()), databaseID, clipArea, siteIdOverride, extraAttrs)
    t1 = time.perf_counter()
    logDebug(f"storeDiscreteWE(): Time to store WE attributes for parm [{varName}]: {t1-t0}")

    #  Process the discrete keys so we store only what is necessary

    for g in range(byteCube.shape[0]):
        (byteCube[g], keyList[g]) = collapseKey(byteCube[g], keyList[g])

    # Mask the values
    byteCube[:, invMask] = fillValue

    # Save the grids to the netCDF cdfFile
    t0 = time.perf_counter()
    for i in range(len(byteCube)):
        var[i] = numpy.flipud(byteCube[i])
    t1 = time.perf_counter()
    logDebug(f"storeDiscreteWE(): Time to store grids for parm [{varName}]: {t1-t0}")

    t0 = time.perf_counter()
    # Find the max number of keys and max length for all keys
    maxKeyCount = 1
    maxKeySize = 0
    for k in keyList:
        if len(k) > maxKeyCount:
            maxKeyCount = len(k)
        for s in k:
            if len(s) > maxKeySize:
                maxKeySize = len(s)

    # create a new netCDF variable to hold the discrete keys
    disShape = (gridCount, maxKeyCount, maxKeySize + 1) # zero byte at
                                                       # the end
    dimNames = ["ngrids_" + varName, "nkeys_" + varName, "keylen_" + varName]
    dims = getDims(cdfFile, disShape, dimNames)
    keyVarName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel() + "_keys"
    keyVar = cdfFile.createVariable(keyVarName, 'c', dims)

    chars = numpy.zeros(disShape, 'c')

    # now save the discrete keys in the netCDF cdfFile
    for g in range(0, gridCount):
        for k in range(0, len(keyList[g])):
            for c in range(0, len(keyList[g][k])):
                chars[g][k][c] = keyList[g][k][c]
    if len(byteCube):
        keyVar[:] = chars
    t1 = time.perf_counter()
    logDebug(f"storeDiscreteWE(): Time to store discrete keys for parm [{varName}]: {t1-t0}")

    ## Extract the GridDataHistory info and save it
    t0 = time.perf_counter()
    storeGridDataHistory(cdfFile, we, histDict)
    t1 = time.perf_counter()
    logDebug(f"storeDiscreteWE(): Time to store grid data history for parm [{varName}]: {t1-t0}")

    logEvent("Saved", gridCount, varName, "grids")

    return gridCount


###-------------------------------------------------------------------------###
### Store some global attribute to the cdfFile
def storeGlobalAtts(cdfFile, argDict):
    globalAttrs = {}
    currentTime = int(time.time())
    asciiTime = time.asctime(time.gmtime(currentTime))
    globalAttrs["creationTime"] = currentTime
    globalAttrs["creationTimeString"] = asciiTime
    if argDict['krunch']:
        globalAttrs["fileFormatVersion"] = "20030117"
    else:
        globalAttrs["fileFormatVersion"] = "20010816"
    globalAttrs["startProcTime"] = argDict['startTime']
    globalAttrs["endProcTime"] = argDict['endTime']
    cdfFile.setncatts(globalAttrs)
    return


###-------------------------------------------------------------------------###
### Compresses the cdfFile using the gzip library
def compressFile(filename, factor):
    if factor < 1:
        factor = 1
    elif factor > 9:
        factor = 9

    with open(filename, "rb") as fp:
        with gzip.open(filename + ".gz", "wb", factor) as fpout:
            shutil.copyfileobj(fp, fpout)

    # remove the orginal file
    os.remove(filename)


###------------
# getSamplingDefinition - accesses server to retrieve definition,
# returns None or the sampling definition as Python.
def getSamplingDefinition(configName, userId, siteId):
    if not configName:
        return None

    locFile = None
    fileName = "isc/utilities/" + configName + ".py"
    pathManager = PathManagerFactory.getPathManager()

    if userId not in ['SITE', 'BASE']:
        context = pathManager.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.USER)
        context.setContextName(userId)
        locFile = pathManager.getFile(context, fileName)

    if userId != 'BASE' and (locFile is None or not locFile.exists()):
        context = pathManager.getContextForSite(LocalizationType.COMMON_STATIC, siteId)
        locFile = pathManager.getFile(context, fileName)

    if locFile is None or not locFile.exists():
        context = pathManager.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.BASE)
        locFile = pathManager.getFile(context, fileName)

    if locFile is None or not locFile.exists():
        s = "Sampling Definition " + configName + " not found, using all grids."
        logProblem(s)
        return None

    try:
        namespace = {}
        with open(locFile.getAbsolutePath(), "rb") as fh:
            exec(fh.read(), namespace)
        return namespace['SampleDef']
    except:
        s = "Bad Sampling Definition found [" + configName + \
          "], using all grids."
        logProblem(s, traceback.format_exc())
        return None


###------------
# determineSamplingInventory based on sampling definition
# returns inventory of time ranges to include in the netCDF file.
def determineSamplingValues(samplingDef, parmName, inventory, currentTime):
    # we're going to get inventory as a PyJObject (List<TimeRange>, actually),
    # but to best match AWIPS-1 will return a list of their tuple-based
    # time range objects, regardless if we have a valid sample definition or not

    if samplingDef is None or inventory.size() == 0:
        newInv = []
        for i in range(0, inventory.size()):
            newInv.append(iscUtil.transformTime(inventory.get(i)))
        return newInv   #all grids

    basetimeDef, offsetDef = samplingDef.get(parmName, samplingDef['default'])

    lastInvT = iscUtil.transformTime(inventory.get(inventory.size() - 1))[1]  #ending time for last grid
    firstInvT = iscUtil.transformTime(inventory.get(0))[0]  #starting time for first grid

    # determine basetime
    bts = []
    bt = int(currentTime / 86400) * 86400 #0z today
    while bt >= firstInvT:
        bt = bt - 86400   #back up a day until we are earlier than 1st grid
    while bt < lastInvT:
        for bval in basetimeDef:
            bts.append(bt + bval)
        bt = bt + 86400
    basetime = None
    for bt in bts:
        if currentTime >= bt:
            basetime = bt
        else:
            break

    # now determine the set of possible times
    checkTimes = []
    # lastInvT = inventory[ -1][1]  #ending time for last grid

    tval = basetime  #begin at the basetime
    tupleNumber = 0
    while tupleNumber < len(offsetDef):
        beginT, intervalT = offsetDef[tupleNumber]
        tval = basetime + beginT
        while tval < lastInvT:
            if tupleNumber < len(offsetDef) - 1:
                if tval < basetime + offsetDef[tupleNumber + 1][0]:
                    checkTimes.append(tval)
                    tval = tval + intervalT  #still in this tuple
                else:
                    break   #go onto the next tuple
            else:
                checkTimes.append(tval)
                tval = tval + intervalT  #can't compare - in last tuple
        tupleNumber = tupleNumber + 1

    #match them up with the inventory to select the intersecting times
    inven = []
    startIndexCheck = 0
    for i in range(0, inventory.size()):
        inv = iscUtil.transformTime(inventory.get(i))
        for x in range(startIndexCheck, len(checkTimes)):
            if contains(inv, checkTimes[x]):
                startIndexCheck = x + 1
                if inv not in inven:
                    inven.append(inv)
                break
    return inven


###-------------------------------------------------------------------------###
### Main program
def main(outputFilename, parmList, databaseID, startTime,
         endTime, mask, geoInfo, compressFileFlag, configFileName,
         compressFileFactor, trim, krunch, userID, logFileName, siteIdOverride):
    initLogger(logFileName)

#    LogStream.ttyLogOn()
    logEvent("ifpnetCDF Starting")
#    LogStream.logEvent(AFPS.DBSubsystem_getBuildDate(),
#      AFPS.DBSubsystem_getBuiltBy(), AFPS.DBSubsystem_getBuiltOn(),
#      AFPS.DBSubsystem_getBuildVersion())

    if hasattr(parmList, 'java_name'):
        parmList = JUtil.javaObjToPyVal(parmList)

    argDict = {"outputFilename": outputFilename,
               "parmList": parmList,
               "databaseID": databaseID,
               "startTime": startTime,
               "endTime": endTime,
               "mask": mask,
               "geoInfo": bool(geoInfo),
               "compressFile": bool(compressFileFlag),
               "configFileName": configFileName,
               "compressFileFactor": int(compressFileFactor),
               "trim": bool(trim),
               "krunch": bool(krunch),
               "userID": userID,
               "siteIdOverride": siteIdOverride, }
    logEvent("Command: ", argDict)

    start = time.perf_counter()
    siteId = DatabaseID(databaseID).getSiteId()
    ifpServer = IFPServer.getActiveServer(siteId)
    if ifpServer is None:
            raise Exception("No active IFPServer for site: " + siteId)

    try:
        timeRange = makeTimeRange(argDict['startTime'], argDict['endTime'])
    except:
        logException("Unable to create TimeRange from arguments: startTime= " + str(argDict['startTime']) + ", endTime= " + argDict['endTime'])
        return

    # See if the databaseID is valid.  An exception will be tossed
    db = IFPDB(argDict['databaseID'])

    # Fill in the parmList with all parms if the input list is empty
    argDict['parmList'] = processParmList(argDict, db)

    # Determine the mask
    maskGrid = getMaskGrid(ifpServer, argDict['mask'], argDict['databaseID'])
    origGridSize = maskGrid.shape
    clipArea = extremaOfSetBits(maskGrid)

    maskGrid = clipToExtrema(maskGrid, clipArea)
    clippedGridSize = maskGrid.shape
    validPointCount = float(numpy.count_nonzero(maskGrid))

    #invert the mask grid
    invMask = numpy.logical_not(maskGrid)

    # Determine sampling definition
    siteId = DatabaseID(argDict['databaseID']).getSiteId()
    samplingDef = getSamplingDefinition(argDict['configFileName'], argDict['userID'], siteId)
    logVerbose("Sampling Definition:", samplingDef)

    partial_complete = False
    try:
        # Open the netCDF file
        with netCDF4.Dataset(argDict['outputFilename'], 'w', format='NETCDF3_CLASSIC', diskless=True, persist=True) as cdfFile:

            totalGrids = 0
            for p in argDict['parmList']:
                try:
                    we = db.getItem(p)

                    #determine inventory that we want to keep
                    t0 = time.perf_counter()
                    parmTimes = we.getKeys()
                    t1 = time.perf_counter()
                    logDebug(f"main(): Time to retrieve inventory for ParmID [{p}]: {t1-t0}")
                    logDebug(f"main(): ParmID [{p}], len(parmTimes): {len(parmTimes)}")

                    t0 = time.perf_counter()
                    weInv = determineSamplingValues(samplingDef, p, parmTimes, time.time())
                    t1 = time.perf_counter()
                    logDebug(f"main(): Time to call determineSamplingValues for ParmID [{p}]: {t1-t0}")
                    logDebug(f"main(): ParmID [{p}], len(weInv): {len(weInv)}")

                    t0 = time.perf_counter()
                    gridType = str(we.getGpi().getGridType())
                    if gridType == "SCALAR":
                        nGrids = storeScalarWE(we, weInv, cdfFile, timeRange,
                          argDict['databaseID'], invMask, argDict['trim'], clipArea,
                          argDict['krunch'], argDict['siteIdOverride'])
                    elif gridType == "VECTOR":
                        nGrids = storeVectorWE(we, weInv, cdfFile, timeRange,
                          argDict['databaseID'], invMask, argDict['trim'], clipArea,
                          argDict['krunch'], argDict['siteIdOverride'])
                    elif gridType == "WEATHER":
                        nGrids = storeWeatherWE(we, weInv, cdfFile, timeRange,
                          argDict['databaseID'], invMask, clipArea, argDict['siteIdOverride'])
                    elif gridType == "DISCRETE":
                        nGrids = storeDiscreteWE(we, weInv, cdfFile, timeRange,
                          argDict['databaseID'], invMask, clipArea, argDict['siteIdOverride'])
                    else:
                        s = "Grids of type: " + we.gridType + " are not supported, " + \
                          "parm=" + p
                        logProblem(s)
                        raise Exception(s)

                    t1 = time.perf_counter()
                    logDebug(f"main(): Time to store grids for ParmID [{p}]: {t1-t0}")
                    totalGrids = totalGrids + nGrids
                except:
                    logException("Could not process parm [" + p + "]: ")
                    partial_complete = True

            # store the topo and lat, lon grids if the -g was present
            if argDict["geoInfo"]:
                storeTopoGrid(ifpServer, cdfFile, clipArea)
                storeLatLonGrids(ifpServer, cdfFile, argDict['krunch'], clipArea)
                totalGrids += 3

            storeGlobalAtts(cdfFile, argDict)
    except:
        logException("Error creating cdfFile: %s " % argDict['outputFilename'])

    if partial_complete:
        os.remove(argDict['outputFilename'])
        return -1

    fu = os.stat(argDict['outputFilename'])[stat.ST_SIZE]
    mb = fu / (1024.0 * 1024.0)
    logEvent("Uncompressed Size: ", "%-.3f" % (mb), " MBytes")
    stop1 = time.perf_counter()

    # Grid statistics
    logEvent("Original Grid Size:", origGridSize)
    logEvent("Clipped Grid Size: ", clippedGridSize)
    logEvent("Valid Points in Grid: ", validPointCount)
    logEvent("Total Number of Grids: ", totalGrids)

    perClipped = 100.0 * clippedGridSize[0] * clippedGridSize[1] / \
      (origGridSize[0] * origGridSize[1])
    perValid = 100.0 * validPointCount / (origGridSize[0] * origGridSize[1])
    logEvent("Percent ClippedPts/Original: ", "%-.1f" % (perClipped),
      "%")
    logEvent("Percent ValidPts/Original: ", "%-.1f" % (perValid),
      "%")

    kpts = totalGrids * validPointCount / 1000.0
    logEvent("Total Points: ", "%-.3f" % (kpts), "Kpoints")

    if totalGrids > 0 and validPointCount > 0:
        bitsPerPointUncompressed = (fu * 8.0) / (totalGrids * validPointCount)
        logEvent("Bits Per Point Uncompressed: ",
          "%-.2f" % (bitsPerPointUncompressed))

    # Finally compress the data with gzip and remove the other cdfFile
    if argDict['compressFile']:
        logEvent("Compressing Output")
        compressFile(argDict['outputFilename'], argDict['compressFileFactor'])
        f = os.stat(argDict['outputFilename'] + ".gz")[stat.ST_SIZE]
        per = 100.0 - 100.0 * f / fu
        mb = f / (1024.0 * 1024.0)
        logEvent("Compressed Size: ", "%-.3f" % (mb), " MBytes")
        logEvent("CompressionPercent=", "%-.1f" % (per), "%")

        if totalGrids > 0 and validPointCount > 0:
            bitsPerPointCompressed = (f * 8.0) / (totalGrids * validPointCount)
            logEvent("Bits Per Point Compressed: ",
              "%-.2f" % (bitsPerPointCompressed))

    stop = time.perf_counter()
    logEvent("Elapsed/CPU time: ",
      f"{stop1 - start:-.2f} processing,",
      f"{stop - stop1:-.2f} compress,",
      f"{stop - start:-.2f} total")
    logEvent("ifpnetCDF Finished")

#if __name__ == "__main__":
#    main()
    # profile stuff
#     import profile, pstats
#     profile.run('main()', 'pyprof.out')
#     p = pstats.Stats('pyprof.out')
#     p.strip_dirs()
#     p.sort_stats('time', 'calls').print_stats(15)
#     p.print_callers(15)
