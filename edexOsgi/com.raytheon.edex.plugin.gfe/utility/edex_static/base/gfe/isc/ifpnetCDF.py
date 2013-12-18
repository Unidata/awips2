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


import string, getopt, sys, time, gzip, os, LogStream, stat, traceback
from collections import OrderedDict
import numpy
#import pupynere as NetCDF
try:
    # dev environment
    from Scientific.IO import NetCDF
except:
    # runtime we don't have the whole scientific package
    import NetCDF
import JUtil
import iscUtil

from java.util import ArrayList
from java.io import File
from com.vividsolutions.jts.geom import Coordinate
from com.raytheon.edex.plugin.gfe.config import IFPServerConfig
from com.raytheon.edex.plugin.gfe.config import IFPServerConfigManager
from com.raytheon.uf.common.dataplugin.gfe.config import ProjectionData_ProjectionType as ProjectionType
from com.raytheon.edex.plugin.gfe.smartinit import IFPDB
from com.raytheon.edex.plugin.gfe.util import CartDomain2D
from com.raytheon.edex.plugin.gfe.server import IFPServer
from com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData_CoordinateType as CoordinateType
from com.raytheon.uf.common.serialization import SerializationUtil
from com.raytheon.uf.common.localization import LocalizationFile
from com.raytheon.uf.common.localization import PathManagerFactory
from com.raytheon.uf.common.localization import LocalizationContext 
from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType
from com.raytheon.uf.common.localization import LocalizationContext_LocalizationLevel as LocalizationLevel

#
# Port of ifpNetCDF
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/06/09        1995          bphillip       Initial Creation.
#    03/11/13        1759          dgilling       Removed unneeded methods.
#    04/23/13        1937          dgilling       Reimplement WECache to match
#                                                 A1, big perf improvement.
#    05/23/13        1759          dgilling       Remove unnecessary imports.
#    06/13/13        2044          randerso       Updated for changes to TopoDatabaseManager
#    07/25/13        2233          randerso       Improved memory utilization and performance
#    08/09/2013      1571          randerso       Changed projections to use the Java             
#                                                 ProjectionType enumeration
#    09/20/13        2405          dgilling       Clip grids before inserting into cache.
#    10/22/13        2405          rjpeter        Remove WECache and store directly to cube.
#    10/31/2013      2508          randerso       Change to use DiscreteGridSlice.getKeys()
#

# Original A1 BATCH WRITE COUNT was 10, we found doubling that
# lead to a significant performance increase.
BATCH_WRITE_COUNT = 20 
BATCH_DELAY = 0.0
ifpNetcdfLogger=None

## Logging methods ##
def initLogger(logFile=None):
    global ifpNetcdfLogger
    ifpNetcdfLogger = iscUtil.getLogger("ifpnetCDF",logFile)

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
    lst = list(inv)
    trs=[]
    histDict = OrderedDict()
    cube = None
    keyList = None
    gridType = str(we.getGpi().getGridType())

    # clipped size
    clipSize = (clipArea[1] - clipArea[0] + 1, clipArea[3] - clipArea[2] + 1)
    gridCount = len(inv)
    
    if gridType == "SCALAR":
        cube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]), dtype=numpy.float32)
    elif gridType == "VECTOR":
        magCube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]),dtype=numpy.float32)
        dirCube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]),dtype=numpy.float32)
        cube = (magCube, dirCube)
    elif gridType == "WEATHER" or gridType == "DISCRETE":
        cube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]), dtype=numpy.int8)
        keyList = []
        
    cubeIdx = 0
    while len(lst):
        i = lst[:BATCH_WRITE_COUNT]
        javaTRs = ArrayList()
        for tr in i:
            javaTRs.add(iscUtil.toJavaTimeRange(tr))
        gridsAndHist = we.get(javaTRs, True)
        size = gridsAndHist.size()
        for idx in xrange(size):
            pair = gridsAndHist.get(idx)
            grid = pair.getFirst()
            tr = iscUtil.transformTime(grid.getValidTime())
            encodeGridSlice(grid, gridType, clipArea, cube, cubeIdx, keyList)
            cubeIdx += 1
            histDict[tr] = encodeGridHistory(pair.getSecond())
        lst = lst[BATCH_WRITE_COUNT:]
        time.sleep(BATCH_DELAY)

    if len(histDict) != gridCount:
        # retrieved less grids than originally expected, purge ran?
        gridCount = len(histDict)

        if gridType == "SCALAR":
            oldCube = cube
            cube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]), dtype=numpy.float32)
            for idx in xrange(gridCount):
                cube[idx] = oldCube[idx]
        elif gridType == "VECTOR":
            oldMagCube = magCube
            magCube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]),dtype=numpy.float32)
            oldDirCube = dirCube
            dirCube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]),dtype=numpy.float32)
            cube = (magCube, dirCube)
            for idx in xrange(gridCount):
                magCube[idx] = oldMagCube[idx]
                dirCube[idx] = oldDirCube[idx]
        elif gridType == "WEATHER" or gridType == "DISCRETE":
            oldCube = cube
            cube = numpy.empty(shape=(gridCount, clipSize[1], clipSize[0]), dtype=numpy.int8)
            for idx in xrange(gridCount):
                cube[idx] = oldCube[idx]
    return (cube, histDict, keyList)

###-------------------------------------------------------------------------###
### cube and keyList are out parameters to be filled by this method, idx is the index into cube to use
def encodeGridSlice(grid, gridType, clipArea, cube, idx, keyList):
    if gridType == "SCALAR":
        cube[idx] = clipToExtrema(grid.__numpy__[0], clipArea)
    elif gridType == "VECTOR":
        vecGrids = grid.__numpy__
        cube[0][idx] = clipToExtrema(vecGrids[0], clipArea)
        cube[1][idx] = clipToExtrema(vecGrids[1], clipArea)
    elif gridType == "WEATHER" or gridType == "DISCRETE":
        keys = grid.getKeys()
        gridKeys = []

        for theKey in keys:
            gridKeys.append(theKey.toString())
        keyList.append(gridKeys)
        cube[idx]= clipToExtrema(grid.__numpy__[0], clipArea)

def encodeGridHistory(histories):
    retVal = []
    for i in xrange(histories.size()):
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
        if parmList[i].find("_") == -1:
            parmList[i] = parmList[i] + "_SFC"

    # now verify that the parm is in the database
    final = []
    for p in parmList:
        if dbList.contains(p):
            final.append(p)
    return final

###-------------------------------------------------------------------------###
# leap year routine
def leapYear(year):
    return (year % 4 == 0 and (year + 100) % 400 != 0)

###-------------------------------------------------------------------------###
# days in month routine
def daysInMonth(month, year):
    days = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

    if month != 2:
        return days[month - 1]

    # special February handling for leap years
    if leapYear(year):
        return days[1] + 1
    else:
        return days[1]

###-------------------------------------------------------------------------###
# convert to time from components
# 0-year,1-month,2-day,3-hour,4-min,5-sec
def timeFromComponents(timeTuple):
    epochDays = 0
    pyear = 1970
    pmonth = 1  # startTime

    while pyear != timeTuple[0]:
        epochDays = epochDays + 365   # days in year
        if leapYear(pyear):
           epochDays = epochDays + 1    # account for leap year
        pyear = pyear + 1

    while pmonth != timeTuple[1]:
        epochDays = epochDays + daysInMonth(pmonth, timeTuple[0])
        pmonth = pmonth + 1

    epochDays = epochDays + timeTuple[2] - 1;   # but not this day

    epochTime = epochDays * 86400 + \
      timeTuple[3] * 3600 + timeTuple[4] * 60 + timeTuple[5]

    return int(epochTime)


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
        intTime = time.strptime(timeStr, "%Y%m%d_%H%M")
    except:
        logProblem(timeStr, \
          "is not a valid time string.  Use YYYYMMDD_HHMM",traceback.format_exc())
        s = timeStr + " is not a valid time string.  Use YYYYMMDD_HHMM"
        raise SyntaxError, s
        return
    return timeFromComponents(intTime)

###-------------------------------------------------------------------------###
### Makes a TimeRange from the input string of the form YYYYMMDD_HHMM.
def makeTimeRange(startString, endString):
    "Makes a timeRange from the specified time strings."
    try:
        t1 = getIntTime(startString)
        t2 = getIntTime(endString)
    except:
        raise Exception, "Can't decode YYYYMMDD_HHMM string"

    return (t1, t2)


###-------------------------------------------------------------------------###
def timeRangeAsString(tr):
    "Prints timeRange in YYYYMMDD_HHMM format"
    return time.strftime("%Y%m%d_%H%M", time.gmtime(tr[0])) + " --- " \
 + time.strftime("%Y%m%d_%H%M", time.gmtime(tr[1]))

###-------------------------------------------------------------------------###
def extremaOfSetBits(mask):
    "Returns tuple of extrema of set bits (minx,maxx, miny,maxy)"
    xs = numpy.sum(mask, 0)
    ys = numpy.sum(mask, 1)
    
    minx = maxx = miny = maxy = -1
    for x in range(xs.shape[0]):
        if xs[x] != 0:
            if minx == -1:
                minx = x
            maxx = x
    for y in range(ys.shape[0]):
        if ys[y] != 0:
            if miny == -1:
                miny = y
            maxy = y
    if minx == -1:
        minx = 0
    if maxx == -1:
        maxx = mask.shape[0]
    if miny == -1:
        miny = 0
    if maxy == -1:
        maxy = mask.shape[1]
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
def getDims(file, dimSizes, dimNames):
    if len(dimSizes) != len(dimNames):
        raise Exception, "dimSizes and dimNames not same size"
    dimList = list(dimSizes)
    dimNames = list(dimNames)
    actDimNames = []
    existingDimList = file.dimensions.keys()
    
    for x in xrange(len(dimList)):
        dimName = "DIM_" + str(dimSizes[x])
        actDimNames.append(dimName)
        if dimName not in existingDimList:
            file.createDimension(dimName, dimSizes[x])
            existingDimList.append(dimName)

    return tuple(actDimNames)

###-------------------------------------------------------------------------###
def getMaskGrid(client, editAreaName, dbId):
    #make a mask with all bits set (y,x)
    domain = IFPServerConfigManager.getServerConfig(DatabaseID(dbId).getSiteId()).dbDomain()
    mask = numpy.ones((domain.getNy().intValue(), domain.getNx().intValue()))

    if editAreaName == "": 
        return mask
    
    # get the edit area
    try:
        mask = iscUtil.getEditArea(editAreaName, DatabaseID(dbId).getSiteId())
        mask.setGloc(domain)
        mask = numpy.reshape(mask.getGrid().__numpy__[0], (mask.getGrid().getNumpyY(), mask.getGrid().getNumpyX()))
    except:
        logProblem("Edit area:", editAreaName, "not found. Storing entire grid.",traceback.format_exc())

    return mask

###-------------------------------------------------------------------------###
def storeLatLonGrids(client, file, databaseID, invMask, krunch, clipArea):
    # db = client[databaseID]

    # Get the grid location and projection information
    gridLoc = IFPServerConfigManager.getServerConfig(DatabaseID(databaseID).getSiteId()).dbDomain()
    pDict = gridLoc.getProjection()

    latLonGrid = gridLoc.getLatLonGrid().__numpy__[0];
    
    latLonGrid = numpy.reshape(latLonGrid, (2,gridLoc.getNy().intValue(),gridLoc.getNx().intValue()), order='F')

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

    dims = getDims(file, latGrid.shape, ("y", "x"))


    # store latitude grid
    if krunch:
        latVar = file.createVariable("latitude", 'h', dims)
        latGrid = (latGrid * 100).astype(numpy.int16)
        latVar[:] = latGrid
        setattr(latVar, "dataMultiplier", 0.01)
        setattr(latVar, "dataOffset", 0)
    else:
        latVar = file.createVariable("latitude", 'f', dims)
        latVar[:] = latGrid
 
    # make the netCDF attributes
    # Descriptive Name
    setattr(latVar, "descriptiveName", "latitude")

    # coordinate information
    origGridSize = Coordinate(gridLoc.getNx().floatValue(), gridLoc.getNy().floatValue())
    origOrigin = gridLoc.getOrigin()
    origExtent = gridLoc.getExtent()
    
    cellSize = (origExtent.x / (origGridSize.x - 1),
                origExtent.y / (origGridSize.y - 1))
    clippedExtent = (cellSize[0] * (clipSize[0] - 1),
      cellSize[1] * (clipSize[1] - 1))
    domainOffset = (clipArea[0] * cellSize[0], (origGridSize.y - clipArea[3]-1) * cellSize[1])
    
    clippedOrigin = (origOrigin.x + domainOffset[0] ,
                     origOrigin.y + domainOffset[1])

    # gridSize, domain origin/extent
    setattr(latVar, "gridSize", clipSize)
    setattr(latVar, "domainOrigin", clippedOrigin)
    setattr(latVar, "domainExtent", clippedExtent)

    #units
    setattr(latVar, "units", "degrees")
    # projection info - store whatever is in the dictionary
    storeProjectionAttributes(latVar, pDict)

    #  store longitude grid
    if krunch:
        lonVar = file.createVariable("longitude", 'h', dims)
        lonGrid = (lonGrid * 100).astype(numpy.int16)
        lonVar[:] = lonGrid
        setattr(lonVar, "dataMultiplier", 0.01)
        setattr(lonVar, "dataOffset", 0)
    else:
        lonVar = file.createVariable("longitude", 'f', dims)
        lonVar[:] = lonGrid

    # Descriptive Name
    setattr(lonVar, "descriptiveName", "longitude")

    # gridSize, domain origin/extent
    setattr(lonVar, "gridSize", clipSize)
    setattr(lonVar, "domainOrigin", clippedOrigin)
    setattr(lonVar, "domainExtent", clippedExtent) 

    #units
    setattr(lonVar, "units", "degrees")
    # projection info - store whatever is in the dictionary
    storeProjectionAttributes(lonVar, pDict)

    logEvent("Saved Latitude/Longitude Grid")

###-------------------------------------------------------------------------###
def storeTopoGrid(client, file, databaseID, invMask, clipArea):
    "Stores the topo grid in the database"

    # Get the grid location and projection information
    ifpServer = IFPServer.getActiveServer(DatabaseID(databaseID).getSiteId())
    
    gridLoc = ifpServer.getConfig().dbDomain()
    pDict = gridLoc.getProjection()

    # Get the topo grid
    topoGrid = ifpServer.getTopoMgr().getTopoData(gridLoc).getPayload().__numpy__[0]
    topoGrid = clipToExtrema(topoGrid, clipArea)
    topoGrid = numpy.flipud(topoGrid)
    
    # clipped size
    clipGridSize = (clipArea[1] - clipArea[0] + 1, clipArea[3] - clipArea[2] + 1)

    newsize = (clipGridSize[1], clipGridSize[0])  #y,x
    topoGrid = numpy.resize(topoGrid, newsize)

    dims = getDims(file, topoGrid.shape, ("y", "x"))

    # create the netcdf variable
    var = file.createVariable("Topo", 'h', dims)

    # round to nearest foot
    topoGrid = numpy.array((topoGrid + 0.5) / 1).astype(numpy.int16)

    var[:] = topoGrid

    # make the netCDF attributes
    # Descriptive Name
    setattr(var, "descriptiveName", "Topography")

    # coordinate information
    origGridSize = Coordinate(float(str(gridLoc.getNx())), float(str(gridLoc.getNy())))
    origOrigin = gridLoc.getOrigin()
    origExtent = gridLoc.getExtent()
    
    cellSize = (origExtent.x / (origGridSize.x - 1),
                origExtent.y / (origGridSize.y - 1))
    clippedExtent = (cellSize[0] * (clipGridSize[0] - 1),
      cellSize[1] * (clipGridSize[1] - 1))
    domainOffset = (clipArea[0] * cellSize[0], (origGridSize.y - clipArea[3]-1) * cellSize[1])
    
    clippedOrigin = (origOrigin.x + domainOffset[0] ,
                     origOrigin.y + domainOffset[1])

    # gridSize
    setattr(var, "gridSize", clipGridSize)

    # Domain origin
    setattr(var, "domainOrigin", clippedOrigin)
    # Domain extent
    setattr(var, "domainExtent", clippedExtent)
    #units
    setattr(var, "units", "ft")
    # projection info - store whatever is in the dictionary
    storeProjectionAttributes(var, pDict)

    logEvent("Saved Topo Grid")

###-------------------------------------------------------------------------###
###
def storeGridDataHistory(file, we, histDict):
    "Stores the Grid Data history string for each grid in we."
    
    # get the maximum size of the history string 
    maxHistSize = 0
    histList = []
    for (tr, his) in histDict.items():
        hisString = ''
        for i,h in enumerate(his):
            hisString = hisString + str(h)
            if i != len(his) - 1:
                hisString = hisString + " ^"
        histList.append(hisString)
        maxHistSize = max(maxHistSize,len(hisString))
        
    # Make the history variable and fill it
    histShape = (len(histList), maxHistSize + 1)
    histCube = numpy.zeros(histShape, 'c')
    for slot, hisString in enumerate(histList):
        histCube[slot:] = hisString
    
    # make the history variable anyway.  iscMosaic needs it.
    elemName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()
    dimNames = ["ngrids_" + elemName, "histLen_" + elemName]
    dims = getDims(file, histShape, dimNames)
    varName = elemName + "_GridHistory"

    var = file.createVariable(varName, 'c', dims)

    if len(histList) > 0:
        # store the cube in the netCDF file
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
            offset = minV - minVarValue / multiplier
        if maxV * multiplier > maxVarValue:
            offset = maxV - maxVarValue / multiplier
        missingValue = -127
        format = "b"
        pythonType = numpy.int8

    # check for short possibilities
    elif nentries <= pow(2, 16) - 2:
        multiplier = precision
        offset = 0
        maxVarValue = pow(2, 15) - 1
        minVarValue = -(pow(2, 15) - 2)
        if minV * multiplier < minVarValue:
            offset = minV - minVarValue / multiplier
        if maxV * multiplier > maxVarValue:
            offset = maxV - maxVarValue / multiplier
        missingValue = minVarValue - 1
        format = "h"
        pythonType = numpy.int16

    # else full 32-bit float processing, no krunching needed
    else:
        multiplier = None
        offset = None
        format = "f"
        missingValue = -30000.0
        pythonType = numpy.float32
    return (format, multiplier, offset, missingValue, pythonType)


###-------------------------------------------------------------------------###
def storeProjectionAttributes(var, projectionData):

    projectionType = projectionData.getProjectionType()
    # store the attributes common to all projections
    setattr(var, "latLonLL", (projectionData.getLatLonLL().x, projectionData.getLatLonLL().y))
    setattr(var, "latLonUR", (projectionData.getLatLonUR().x, projectionData.getLatLonUR().y))
    setattr(var, "gridPointLL", (projectionData.getGridPointLL().x, projectionData.getGridPointLL().y))
    setattr(var, "gridPointUR", (projectionData.getGridPointUR().x, projectionData.getGridPointUR().y))
    setattr(var, "projectionType", projectionType.toString())

    # Now store the projection specific attributes
    if ProjectionType.LAMBERT_CONFORMAL.equals(projectionType):
        setattr(var, "latLonOrigin", (projectionData.getLatLonOrigin().x, projectionData.getLatLonOrigin().y))
        setattr(var, "stdParallelOne", projectionData.getStdParallelOne())
        setattr(var, "stdParallelTwo", projectionData.getStdParallelTwo())

    if ProjectionType.POLAR_STEREOGRAPHIC.equals(projectionType):
        setattr(var, "lonOrigin", projectionData.getLonOrigin())

    if ProjectionType.MERCATOR.equals(projectionType):
        setattr(var, "lonCenter", projectionData.getLonCenter())

    return

###-------------------------------------------------------------------------###
def storeWEAttributes(var, we, timeList, databaseID, clipArea):
    "Stores attributes in the netCDF file for any weather element"
    
    # Note that geo information is modified based on the clip info.

    # TimeRanges
    import itertools
    setattr(var, "validTimes", list(itertools.chain.from_iterable(timeList))) 

    # Descriptive Name
    setattr(var, "descriptiveName", we.getGpi().getDescriptiveName())

    # gridSize
    clipGridSize = (clipArea[1] - clipArea[0] + 1, clipArea[3] - clipArea[2] + 1)
    setattr(var, "gridSize", clipGridSize)

    # Domain origin and extent
    gridLoc = we.getGpi().getGridLoc()
    origGridSize = Coordinate(float(str(gridLoc.getNx())), float(str(gridLoc.getNy())))
    origOrigin = gridLoc.getOrigin()
    origExtent = gridLoc.getExtent()
    
    
    cellSize = (origExtent.x / (origGridSize.x - 1),
                origExtent.y / (origGridSize.y - 1))
    
    clippedExtent = (cellSize[0] * (clipGridSize[0] - 1),
      cellSize[1] * (clipGridSize[1] - 1))
    
    domainOffset = (clipArea[0] * cellSize[0], (origGridSize.y - clipArea[3]-1) * cellSize[1])
    
    clippedOrigin = (origOrigin.x + domainOffset[0] ,
                     origOrigin.y + domainOffset[1])

    setattr(var, "domainOrigin", clippedOrigin)
    setattr(var, "domainExtent", clippedExtent)

    # Min/Max allowable values
    setattr(var, "minMaxAllowedValues", (we.getGpi().getMinValue(), we.getGpi().getMaxValue()))

    # data type
    setattr(var, "gridType", we.getGpi().getGridType().toString())
    # database ID
    setattr(var, "databaseID", databaseID)
    # siteID
    #setattr(var, "siteID", we.siteID) 
    setattr(var, "siteID", we.getParmid().getDbId().getSiteId())
    # units
    setattr(var, "units", we.getGpi().getUnitString())
    # level
    setattr(var, "level", we.getParmid().getParmLevel())
    # timeConstraints
    setattr(var, "timeConstraints", (we.getGpi().getTimeConstraints().getStartTime(), we.getGpi().getTimeConstraints().getDuration(), we.getGpi().getTimeConstraints().getRepeatInterval()))
    # precision
    setattr(var, "precision", we.getGpi().getPrecision())

    # rate parm
    setattr(var, "rateDependent", we.getGpi().isRateParm())

    # projection info - store whatever is in the dictionary
    storeProjectionAttributes(var, gridLoc.getProjection())

    return


def findOverlappingTimes(trList, timeRange):
    timeList = []
    overlappingTimes = []
    for t in trList:
        interTR = intersection(t, timeRange)
        if interTR is not None:
            overlappingTimes.append(t)
            timeList.append(interTR)

    return timeList, overlappingTimes

###-------------------------------------------------------------------------###
### Stores the specified Scalar WE in the netCDF file whose grids fall within
### the specified timeRange.
def storeScalarWE(we, trList, file, timeRange, databaseID,
                  invMask, trim, clipArea, krunch):
    "Stores a weather element to the netCDF file"

    # get the data and store it in a Numeric array.
    timeList, overlappingTimes = findOverlappingTimes(trList, timeRange)

    (cube, histDict, keyList) = retrieveData(we, overlappingTimes, clipArea)
    gridCount = len(cube)
    for i in xrange(len(overlappingTimes) -1, -1, -1):
        ot = overlappingTimes[i]
        if not ot in histDict:
            del overlappingTimes[i]
            del timeList[i]
        elif we.getGpi().isRateParm():
            durRatio = (float(timeList[i][1]-timeList[i][0]))/float((ot[1]-ot[0]))
            cube[i] *= durRatio
    
    ### Make sure we found some grids
    # make the variable name
    varName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()

    if len(cube) == 0:
        logVerbose("No", varName, "grids found")

    #get the dimension List
    dimNames = ["ngrids_" + varName, "y", "x"]
    dims = getDims(file, cube.shape, dimNames)

    # Round the values according to the precision
    if trim:
        if krunch:
            format, multiplier, offset, fillValue, pythonType = \
              calcKrunchValues(we)
        else:
            format, multiplier, offset, fillValue, pythonType = \
             ('f', None, None, -30000.0, numpy.float32)

        # krunch
        if multiplier is not None:
            cube -= offset
            cube *= multiplier
            numpy.around(cube,out=cube)
        # normal trim
        else:
            digits = we.getGpi().getPrecision()
            numpy.around(cube, digits, cube)
        cube = cube.astype(pythonType)

    else:
        format, multiplier, offset, fillValue, pythonType = \
         ('f', None, None, -30000.0, numpy.float32)

    # mask the data
    cube[:,invMask] = fillValue
    
    # create the variable
    var = file.createVariable(varName, format, dims)
    if multiplier is not None:
        setattr(var, "dataMultiplier", 1.0 / multiplier)
        setattr(var, "dataOffset", offset)

    # Save the grids to the netCDF file
    for i in range(len(cube)):
        var[i] = numpy.flipud(cube[i])

    # Store the attributes
    storeWEAttributes(var, we, timeList, databaseID, clipArea)
    setattr(var, "fillValue", fillValue)

    ## Extract the GridDataHistory info and save it
    storeGridDataHistory(file, we, histDict)

    logEvent("Saved", gridCount, varName, " grids")

    return gridCount

###-------------------------------------------------------------------------###
### Stores the specified Vector WE in the netCDF file whose grids fall within
### the specified timeRange.
def storeVectorWE(we, trList, file, timeRange,
                  databaseID, invMask, trim, clipArea, krunch):
    "Stores a vector weather element to the netCDF file"

    # get the data and store it in a Numeric array.
    timeList, overlappingTimes = findOverlappingTimes(trList, timeRange)

    ((magCube, dirCube), histDict, keyList) = retrieveData(we, overlappingTimes, clipArea)
    gridCount = len(magCube)
    for i in xrange(len(overlappingTimes) -1, -1, -1):
        ot = overlappingTimes[i]
        if not ot in histDict:
            del overlappingTime[i]
            del timeList[i]
        elif we.getGpi().isRateParm():
            durRatio = (float(timeList[i][1]-timeList[i][0]))/float((ot[1]-ot[0]))
            magCube[i] *= durRatio

    varName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()
    
    ### Make sure we found some grids
    if len(magCube) == 0:
        logVerbose("No", varName, "grids found")

    # make the variable name
    magVarName = we.getParmid().getParmName() + "_Mag_" + we.getParmid().getParmLevel()
    dirVarName = we.getParmid().getParmName() + "_Dir_" + we.getParmid().getParmLevel()

    #get the dimension List
    dimNames = ["ngrids_" + varName, "y", "x"]
    dims = getDims(file, magCube.shape, dimNames)

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
            numpy.around(magCube,out=magCube)
            
        # normal trim for magnitude
        else:
            digits = we.getGpi().getPrecision()
            numpy.around(magCube, digits, magCube)
        magCube = magCube.astype(mpythonType)

        # krunch direction
        if dmultiplier is not None:
            dirCube -= doffset
            dirCube *= dmultiplier
            numpy.around(dirCube,out=dirCube)

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

    magCube[:,invMask] = mfillValue
    dirCube[:,invMask] = dfillValue

    # create the variable
    magVar = file.createVariable(magVarName, mformat, dims)
    dirVar = file.createVariable(dirVarName, dformat, dims)
    if mmultiplier is not None:
        setattr(magVar, "dataMultiplier", 1.0 / mmultiplier)
        setattr(magVar, "dataOffset", moffset)
    if dmultiplier is not None:
        setattr(dirVar, "dataMultiplier", 1.0 / dmultiplier)
        setattr(dirVar, "dataOffset", doffset)

    # Save the grid to the netCDF file
    for i in range(len(magCube)):
        magVar[i] = numpy.flipud(magCube[i])
        dirVar[i] = numpy.flipud(dirCube[i])
        
    # Store the attributes - overwrite some for mag and dir
    storeWEAttributes(magVar, we, timeList, databaseID, clipArea)

    # Change the descriptive name
    setattr(magVar, "descriptiveName", we.getGpi().getDescriptiveName() + " Magnitude")
    setattr(magVar, "fillValue", mfillValue)
    storeWEAttributes(dirVar, we, timeList, databaseID, clipArea)

    # Special case attributes for wind direction
    setattr(dirVar, "minMaxAllowedValues", (0.0, 360.0))
    setattr(dirVar, "descriptiveName", we.getGpi().getDescriptiveName() + " Direction")
    setattr(dirVar, "units", "degrees")
    if trim:
        dirPrecision = -1
    else:
        dirPrecision = 0
    setattr(dirVar, "precision", dirPrecision)
    setattr(dirVar, "fillValue", dfillValue)

    ## Extract the GridDataHistory info and save it
    storeGridDataHistory(file, we, histDict)

    logEvent("Saved", gridCount, varName, "grids")

    return gridCount * 2   #vector has two grids


###-------------------------------------------------------------------------###
# Collapse key and bytes. (for discrete and weather)
### Returns tuple of (updated key, updated grid)
def collapseKey(keys, grid):
    #make list of unique indexes in the grid
    flatGrid = grid.flat
    used = []
    for n in range(flatGrid.__array__().shape[0]):
        if flatGrid[n] not in used:
            used.append(flatGrid[n])

    #make reverse map
    map = []
    newKeys = []
    j = 0
    for i in range(len(keys)):
       if i in used:
           map.append(j)
           newKeys.append(keys[i])
           j = j + 1
       else:
           map.append(-1)

    # modify the data
    newGrid = grid
    for k in range(len(map)):
       mask = numpy.equal(k, grid)
       newGrid = numpy.where(mask, map[k], newGrid).astype(numpy.int8)

    return (newKeys, newGrid)

###-------------------------------------------------------------------------###
# Stores the specified Weather WE in the netCDF file whose grids fall within
### the specified timeRange.
def storeWeatherWE(we, trList, file, timeRange, databaseID, invMask, clipArea):
    "Stores the Weather weather element to the netCDF file"

    # get the data and store it in a Numeric array.
    timeList, overlappingTimes = findOverlappingTimes(trList, timeRange)

    (byteCube, histDict, keyList) = retrieveData(we, overlappingTimes, clipArea)
    gridCount = len(histDict)
    for i in xrange(len(overlappingTimes) -1, -1, -1):
        ot = overlappingTimes[i]
        if not ot in histDict:
            del overlappingTime[i]
            del timeList[i]

    # make the variable name
    varName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()

    ### Make sure we found some grids
    if len(byteCube) == 0:
        logVerbose("No", varName, "grids found")

    #get the dimension List
    dimNames = ["ngrids_" + varName, "y", "x"]
    dims = getDims(file, byteCube.shape, dimNames)
    
    # create the netCDF variable - 'b' for byte type
    var = file.createVariable(varName, 'b', dims)

    #  Process the weather keys so we store only what is necessary
    
    for g in range(byteCube.shape[0]):
        (keyList[g], byteCube[g]) = collapseKey(keyList[g], byteCube[g])

    # Mask the values
    fillValue = -127
    byteCube[:,invMask] =fillValue

    # Save the grids to the netCDF file
    for i in range(len(byteCube)):
        var[i] = numpy.flipud(byteCube[i])
    
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
    dims = getDims(file, wxShape, dimNames)
    keyVarName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel() + "_wxKeys"
    keyVar = file.createVariable(keyVarName, 'c', dims)
    
    chars = numpy.zeros(wxShape, 'c')

    # now save the weather keys in the netCDF file
    for g in range(0, gridCount):
        for k in range(0, len(keyList[g])):
            for c in range(0, len(keyList[g][k])):
                chars[g][k][c] = keyList[g][k][c]
    if len(byteCube):
        keyVar[:] = chars

    # Store the attributes
    storeWEAttributes(var, we, timeList, databaseID, clipArea)
    setattr(var, "fillValue", fillValue)

    ## Extract the GridDataHistory info and save it
    storeGridDataHistory(file, we, histDict)

    logEvent("Saved", gridCount, varName, "grids")

    return gridCount

###-------------------------------------------------------------------------###
# Stores the specified Discrete WE in the netCDF file whose grids fall within
### the specified timeRange.
def storeDiscreteWE(we, trList, file, timeRange, databaseID, invMask, clipArea):
    "Stores the Weather weather element to the netCDF file"

    # get the data and store it in a Numeric array.
    timeList, overlappingTimes = findOverlappingTimes(trList, timeRange)

    (byteCube, histDict, keyList) = retrieveData(we, overlappingTimes, clipArea)
    gridCount = len(histDict)
    for i in xrange(len(overlappingTimes) -1, -1, -1):
        ot = overlappingTimes[i]
        if not ot in histDict:
            del overlappingTime[i]
            del timeList[i]

    # make the variable name
    varName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()

    ### Make sure we found some grids
    if len(byteCube) == 0:
        logVerbose("No", varName, "grids found")

    #get the dimension List
    dimNames = ["ngrids_" + varName, "y", "x"]
    dims = getDims(file, byteCube.shape, dimNames)

    # create the netCDF variable - 'b' for byte type
    var = file.createVariable(varName, 'b', dims)
    
    #  Process the discrete keys so we store only what is necessary

    for g in range(byteCube.shape[0]):
        (keyList[g], byteCube[g]) = collapseKey(keyList[g], byteCube[g])

    # Mask the values
    fillValue = -127
    byteCube[:,invMask] = fillValue

    # Save the grids to the netCDF file
    for i in range(len(byteCube)):
        var[i] = numpy.flipud(byteCube[i])

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
    dims = getDims(file, disShape, dimNames)
    keyVarName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel() + "_keys"
    keyVar = file.createVariable(keyVarName, 'c', dims)

    chars = numpy.zeros(disShape, 'c')

    # now save the discrete keys in the netCDF file
    for g in range(0, gridCount):
        for k in range(0, len(keyList[g])):
            for c in range(0, len(keyList[g][k])):
                chars[g][k][c] = keyList[g][k][c]
    if len(byteCube):
        keyVar[:] = chars

    # Store the attributes
    storeWEAttributes(var, we, timeList, databaseID, clipArea)
    setattr(var, "fillValue", fillValue)

    ## Extract the GridDataHistory info and save it
    storeGridDataHistory(file, we, histDict)

    logEvent("Saved", gridCount, varName, "grids")

    return gridCount

###-------------------------------------------------------------------------###
### Store some global attribute to the file
def storeGlobalAtts(file, argDict):
    currentTime = int(time.time())
    asciiTime = time.asctime(time.gmtime(int(time.time())))
    setattr(file, "creationTime", currentTime)
    setattr(file, "creationTimeString", asciiTime)
    if argDict['krunch']:
        setattr(file, "fileFormatVersion", "20030117")
    else:
        setattr(file, "fileFormatVersion", "20010816")
    setattr(file, "startProcTime", argDict['startTime'])
    setattr(file, "endProcTime", argDict['endTime'])
    return

###-------------------------------------------------------------------------###
### Compresses the file using the gzip library
def compressFile(filename, factor): 
    
    if factor < 1:
        factor = 1
    elif factor > 9:
        factor = 9
    fp = open(filename, "rb")
    fpout = gzip.open(filename + ".gz", "wb", factor)
    buffer = fp.read(1024 * 16)
    while buffer != "":
        fpout.write(buffer)
        buffer = fp.read(1024 * 16)

    fp.close()
    fpout.close()
    # remove the orginal file
    os.remove(filename)

###------------
# getSamplingDefinition - accesses server to retrieve definition,
# returns None or the sampling definition as Python.
def getSamplingDefinition(client, configName):
    if configName is None:
        return None  
    file = PathManagerFactory.getPathManager().getStaticFile("isc/utilities/" + configName + ".py")
    if file is None:
        s = "Sampling Definition " + configName + " not found, using all grids."
        logProblem(s)
        return None
    from com.raytheon.uf.common.util import FileUtil
    data = FileUtil.file2String(file)    
    try:
        exec data
        return SampleDef
    except:
        s = "Bad Sampling Definition found [" + configName + \
          "], using all grids."
        logProblem(s,traceback.format_exc())
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

    lastInvT = iscUtil.transformTime(inventory.get(inventory.size()-1))[1]  #ending time for last grid
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
        for x in xrange(startIndexCheck, len(checkTimes)):
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
         compressFileFactor, trim, krunch, userID, logFileName):
    initLogger(logFileName)
    
    
#    LogStream.ttyLogOn()
    logEvent("ifpnetCDF Starting")
#    LogStream.logEvent(AFPS.DBSubsystem_getBuildDate(),
#      AFPS.DBSubsystem_getBuiltBy(), AFPS.DBSubsystem_getBuiltOn(),
#      AFPS.DBSubsystem_getBuildVersion())

    try:
        len(parmList)
    except TypeError:
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
               "userID": userID}
    logEvent("Command: ", argDict)

    a = os.times()
    cpu0 = a[0] + a[1]
    start = a[4]
    client = None

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
    maskGrid = getMaskGrid(None, argDict['mask'], argDict['databaseID'])
    origGridSize = maskGrid.shape
    clipArea = extremaOfSetBits(maskGrid)
    maskGrid = clipToExtrema(maskGrid, clipArea)
    clippedGridSize = maskGrid.shape
    validPointCount = float(numpy.add.reduce(numpy.add.reduce(maskGrid)))

    #invert the mask grid
    invMask = numpy.logical_not(maskGrid)
    #del maskGrid

    # Determine sampling definition
    samplingDef = getSamplingDefinition(client, argDict['configFileName'])
    logVerbose("Sampling Definition:", samplingDef)

    # Open the netCDF file
    file = NetCDF.NetCDFFile(argDict['outputFilename'], 'w')

    totalGrids = 0
    for p in argDict['parmList']:

        we = db.getItem(p)
        
        #determine inventory that we want to keep
        weInv = determineSamplingValues(samplingDef, p, we.getKeys(), time.time())
        
        gridType = str(we.getGpi().getGridType())
        if gridType == "SCALAR":
            nGrids = storeScalarWE(we, weInv, file, timeRange,
              argDict['databaseID'], invMask, argDict['trim'], clipArea,
              argDict['krunch'])
        elif gridType == "VECTOR":
            nGrids = storeVectorWE(we, weInv, file, timeRange,
              argDict['databaseID'], invMask, argDict['trim'], clipArea,
              argDict['krunch'])
        elif gridType == "WEATHER":
            nGrids = storeWeatherWE(we, weInv, file, timeRange,
              argDict['databaseID'], invMask, clipArea)
        elif gridType == "DISCRETE":
            nGrids = storeDiscreteWE(we, weInv, file, timeRange,
              argDict['databaseID'], invMask, clipArea)
        else:
            s = "Grids of type: " + we.gridType + " are not supported, " + \
              "parm=" + p
            logProblem(s)
            raise Exception, s

        totalGrids = totalGrids + nGrids

    # store the topo and lat, lon grids if the -g was present
    if argDict["geoInfo"]:
        storeTopoGrid(client, file, argDict['databaseID'], invMask, clipArea)
        storeLatLonGrids(client, file, argDict['databaseID'], invMask,
          argDict['krunch'], clipArea)
        totalGrids = totalGrids + 3

    storeGlobalAtts(file, argDict);

    file.close()

    fu = os.stat(argDict['outputFilename'])[stat.ST_SIZE]
    mb = fu / (1024.0 * 1024.0)
    logEvent("Uncompressed Size: ", "%-.3f" % (mb), " MBytes")
    a = os.times()
    cpu = a[0] + a[1]
    stop1 = a[4]

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

    # Finally compress the data with gzip and remove the other file
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

    a = os.times()
    cpugz = a[0] + a[1]
    stop = a[4]
    logEvent("Elapsed/CPU time: ", 
      "%-.2f" % (stop1 - start), "/", "%-.2f" % (cpu - cpu0), "processing,", 
      "%-.2f" % (stop - stop1), "/", "%-.2f" % (cpugz - cpu), "compress,",
      "%-.2f" % (stop - start), "/", "%-.2f" % (cpugz - cpu0), "total")
    #logEvent("stats: ", client.getStats())
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
