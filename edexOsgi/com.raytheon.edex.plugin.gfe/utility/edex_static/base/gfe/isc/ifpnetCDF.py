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
import numpy
import pupynere as netcdf
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
from com.raytheon.edex.plugin.gfe.server.database import TopoDatabaseManager
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
#    
# 
#


BATCH_WRITE_COUNT = 10 
BATCH_DELAY = 0.0

###-------------------------------------------------------------------------###
###  Prints the usage message
def usage():
   print """
 -h hostname:        Host name upon which the ifpServer is running.
 -r RPC port:        RPC port upon which the ifpServer is running.
 -u userID:          The user ID to connect with
 -o output file:     Specifies the name of the output file.
 -d database id:     DatabaseID from which to get the data.
                         format: (DEN_GRID__eta_19980604_1200)
 -p parmName:        Optional. If none specified, get all parms for
                         the database listed.  There may be several
                         parm names specified.
 -s start time:      Optional.  If no start time specified,
                         make start time = 0.
                         format: YYYYMMDD_HHMM (e.g. 19980604_1200)
 -e end time:        Optional.  If no end time specified, make end
                         time = Abstime::MaxFutureTime().
                         format: (19980604_1200)
 -m mask             Optional. Specifies the edit area to be used
                         as the clip mask. If no mask was specified
                         then the entire domain will be used. All
                         values outside the mask will be assigned
                         a missing value.
 -t [no argument]    Optional. If present, trim data resolution.
 -g [no argument]    Optional. If present, topography, latitude
                        and longitude grids will be stored.
 -c [no argument]    Optional. If present, the netCDF file will be
                        compressed by the gzip.
 -f factor           Optional. When provided in conjunction with the -c switch,
                        provides the compression factor of gzip (1-9). Default
                        is 6.
 -k [no argument]    Optional. If present, the netCDF file is really
                        shrunk, by using bytes and shorts to represent floats.
                        Requires the -t switch.
 -C configIntervalFilename
                     Optional. If present, controls the interval/spacing of
                        the grids. Identifies a configuration file defining
                        the timing constraints. The filename identifies a
                        file within the ifpServer TEXT/Utility directory and
                        must be a Python file.

"""


ifpNetcdfLogger=None

## Logging methods ##
def initLogger(logFile=None):
    global ifpNetcdfLogger
    import logging, siteConfig
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
    
###-------------------------------------------------------------------------###
###  Parses the command line options and saves the data in argDict
def getArgs(argv):
    try:
        optlist, args = getopt.getopt(argv[1:],
          'v:h:r:o:p:d:s:e:u:m:gctkf:C:') 
       # optlist, args = getopt.getopt(sys.argv[1:],
       #   'h:r:o:p:d:s:e:u:m:gctkf:C:') 
    except getopt.error, val:
        print val
        usage()
        return

    # initialize input arguments
    host = ""
    port = -1 
    outputFilename = "" 
    parmList = []
    databaseID = ""
    startTime = ""
    endTime = ""
    mask = ""
    geoInfo = 0
    compressFile = 0
    trim = 0
    userID = "SITE"
    krunch = 0
    compressFileFactor = 6
    configFileName = None
    logFile=None

    try:
        import siteConfig
        host = siteConfig.GFESUITE_SERVER 
        port = int(siteConfig.GFESUITE_PORT)
    except:
        pass

    for opt in optlist:
        if opt[0] == '-h':
            host = opt[1]
        elif opt[0] == '-r':
            port = int(opt[1]) 
        elif opt[0] == '-o':
            outputFilename = opt[1]
        elif opt[0] == '-C':
            configFileName = opt[1]
        elif opt[0] == '-p':
            p = opt[1]
            # Add SFC if necessary
            if string.find(p, "_SFC") == -1:
                p = p + "_SFC"
            if p not in parmList:
                parmList.append(p)
        elif opt[0] == '-d':
            databaseID = opt[1]
        elif opt[0] == '-u':
            userID = opt[1]
        elif opt[0] == '-s':
            startTime = opt[1]
        elif opt[0] == '-f':
            compressFileFactor = int(opt[1])
        elif opt[0] == '-e':
            endTime = opt[1]
        elif opt[0] == '-m':
            mask = opt[1]
        elif opt[0] == '-g':
            geoInfo = 1
        elif opt[0] == '-t':
            trim = 1
        elif opt[0] == '-k':
            krunch = 1
        elif opt[0] == '-c':
            compressFile = 1
        elif opt[0] == '-v':
            logFile=opt[1]
        
    initLogger(logFile)

    # Create dictionary of arguments
    argDict = {
        "host" : host,
        "port" : port,
        "outputFilename": outputFilename,
        "parmList": parmList,
        "databaseID": databaseID,
        "startTime": startTime,
        "endTime": endTime,
        "mask": mask,
        "geoInfo": geoInfo,
        "compressFile" : compressFile,
        "configFileName" : configFileName,
        "compressFileFactor" : compressFileFactor,
        "trim" : trim,
        "krunch" : krunch,
        "userID" : userID
        }
    return argDict

###-------------------------------------------------------------------------###
###  Checks each argument and takes action where appropriate
def checkArgs(argDict):
    if argDict['host'] == '':
        usage()
        logProblem("You must specify an ifpServer host name.")
        raise Exception, "You must specify an ifpServer host name."
    if argDict['port'] == -1:
        usage()
        logProblem("You must specify an ifpServer port number.")
        raise Exception, "You must specify an ifpServer port number."
    if argDict['databaseID'] == '':
        usage()
        logProblem("You must specify a database id.")
        raise Exception, "You must specify a database id."
    if argDict['outputFilename'] == '':
        argDict['outputFilename'] = 'ifpnetCDFFile.cdf'

    # validate start/end times
    if argDict['startTime'] == '':
        argDict['startTime'] = '19700101_0000'
    if argDict['endTime'] == '':
        argDict['endTime'] = '20371231_2359'

    # must have trim, if krunch specified
    if argDict['krunch'] == 1 and argDict['trim'] == 0:
        argDict['krunch'] = 0


    return argDict

class WECache(object):
    def __init__(self, we, inv):
        self._we = we
        self._grids = []
        self._hist = []
        self._inv = []
        lst = []
        lst = list(inv)
        while len(lst):
            i = lst[:BATCH_WRITE_COUNT]
            self._inv = self._inv + list(i)
            lst = lst[BATCH_WRITE_COUNT:]
            time.sleep(BATCH_DELAY)

    def keys(self):
        return tuple(self._inv)

    def __getitem__(self, key):
        grid = self._we.getItem(iscUtil.toJavaTimeRange(key))
        history = grid.getGridDataHistory()
        hist = []
        for i in range(0, history.size()):
            hist.append(history.get(i).getCodedString())
        gridType = grid.getGridInfo().getGridType().toString()
        if gridType == "SCALAR":
            return (grid.__numpy__[0], hist)
        elif gridType == "VECTOR":
            vecGrids = grid.__numpy__
            return ((vecGrids[0], vecGrids[1]), hist)
        elif gridType == "WEATHER":
            keys = grid.getKeys()
            keyList = []
            for theKey in keys:
                keyList.append(theKey.toString())
            return ((grid.__numpy__[0],keyList), hist)
        elif gridType =="DISCRETE":
            keys = grid.getKey()
            keyList = []
            for theKey in keys:
                keyList.append(theKey.toString())
            return ((grid.__numpy__[0],keyList), hist)


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
def storeLatLonGrids(client, file, databaseID, maskGrid, krunch, clipArea):
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
def storeTopoGrid(client, file, databaseID, maskGrid, clipArea):
    "Stores the topo grid in the database"

    # Get the grid location and projection information
    gridLoc = IFPServerConfigManager.getServerConfig(DatabaseID(databaseID).getSiteId()).dbDomain()
    pDict = gridLoc.getProjection()

    # Get the topo grid
    topoGrid = TopoDatabaseManager.getTopoDatabase(DatabaseID(databaseID).getSiteId()).getTopo().__numpy__[0]
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

def historyFunc(x, y):   
    return y[x][1]

###-------------------------------------------------------------------------###
###
def storeGridDataHistory(file, we, wec, trList, timeRange):
    "Stores the Grid Data history string for each grid in we."
    
    histories = []
    for tr in trList:
        histories.append(historyFunc(tr, wec))
    #histories = map(lambda x, y=wec: y[x][1], trList)
    
    # get the maximum size of the history string 
    maxHistSize = 0
    gridCount = 0
    firstSlot = -1
    lastSlot = 0

    for x in xrange(len(trList)):
        t = trList[x]
        his = histories[x]
        hisString = ''
        for i in xrange(len(his)):
            hisString = hisString + str(his[i])
            if i != len(his) - 1:
               hisString = hisString + " ^"
        if overlaps(t, timeRange):
            if firstSlot == -1:
                firstSlot = gridCount
            lastSlot = gridCount
            if len(hisString) > maxHistSize:
                maxHistSize = len(hisString)
        gridCount = gridCount + 1
        
    # Make the history variable and fill it
    histShape = (lastSlot - firstSlot + 1, maxHistSize + 1)
    if firstSlot != -1:
        histCube = numpy.zeros(histShape, 'c')
        slot = 0
        for i in xrange(firstSlot, lastSlot + 1):
            his = histories[i]
            hisString = ''
            for h in range(len(his)):
                hisString = hisString + str(his[h])
                if h != len(his) - 1:
                   hisString = hisString + " ^"
            histCube[slot:] = hisString
            slot = slot + 1
    
    # make the history variable anyway.  iscMosaic needs it.
    elemName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()
    dimNames = ["ngrids_" + elemName, "histLen_" + elemName]
    dims = getDims(file, histShape, dimNames)
    varName = elemName + "_GridHistory"

    var = file.createVariable(varName, 'c', dims)

    if firstSlot != -1:
        # store the cube in the netCDF file
        var[:] = histCube
    return

###-------------------------------------------------------------------------###
###
def calcKrunchValues(we):
    #Based on the weather element, will return information pertaining
    #to the dataType, multiplier, offset, and missing value to use for this
    #element.  Returns (dataType, multipler, offset, missingValue, pythonType)

    maxV = we.getGpi().getMaxValue()
    minV = we.getGpi().getMinValue()
    precision = pow(10, we.getGpi().getPrecision())

    nentries = ((maxV - minV) * precision) + 1

    # check for byte possibilities
    if nentries <= pow(2, 8) - 1:
        multipler = precision
        offset = 0
        minVarValue = -126
        maxVarValue = 127
        if minV * multipler < minVarValue:
            offset = minV - minVarValue / multipler
        if maxV * multipler > maxVarValue:
            offset = maxV - maxVarValue / multipler
        missingValue = -127
        format = "b"
        pythonType = numpy.int8

    # check for short possibilities
    elif nentries <= pow(2, 16) - 2:
        multipler = precision
        offset = 0
        maxVarValue = pow(2, 15) - 1
        minVarValue = -(pow(2, 15) - 2)
        if minV * multipler < minVarValue:
            offset = minV - minVarValue / multipler
        if maxV * multipler > maxVarValue:
            offset = maxV - maxVarValue / multipler
        missingValue = minVarValue - 1
        format = "h"
        pythonType = numpy.int16

    # else full 32-bit float processing, no krunching needed
    else:
        multipler = None
        offset = None
        format = "f"
        missingValue = -30000.0
        pythonType = numpy.float32
    return (format, multipler, offset, missingValue, pythonType)


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
    if projectionType.toString() == "LAMBERT_CONFORMAL":
        setattr(var, "latLonOrigin", (projectionData.getLatLonOrigin().x, projectionData.getLatLonOrigin().y))
        setattr(var, "stdParallelOne", projectionData.getStdParallelOne())
        setattr(var, "stdParallelTwo", projectionData.getStdParallelTwo())

    if projectionType.toString() == "POLAR_STEREOGRAPHIC":
        setattr(var, "lonOrigin", projectionData.getLonOrigin())

    if projectionType.toString() == "MERCATOR":
        setattr(var, "lonCenter", projectionData.getLonCenter())

    return

###-------------------------------------------------------------------------###
def storeWEAttributes(var, we, timeList, databaseID, clipArea):
    "Stores attributes in the netCDF file for any weather element"
    
    # Note that geo information is modified based on the clip info.

    # TimeRanges
    setattr(var, "validTimes", timeList) 

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


###-------------------------------------------------------------------------###
### Stores the specified Scalar WE in the netCDF file whose grids fall within
### the specified timeRange.
def storeScalarWE(we, trList, file, timeRange, databaseID,
                  mask, trim, clipArea, krunch):
    "Stores a weather element to the netCDF file"

    # get the data and store it in a Numeric array.
    cube = []
    timeList = []
    times  = []
    wec = WECache(we, trList)
    for t in trList:
        interTR = intersection(t, timeRange)
        if interTR is not None:
            times.append(t)
            timeList.append(interTR[0])
            timeList.append(interTR[1])

    ### Make sure we found some grids
    varName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()
    gridCount = len(timeList) / 2

    if gridCount == 0:
        logVerbose("No", varName, "grids found")

    # clipped size
    clipSize = (clipArea[1] - clipArea[0] + 1, clipArea[3] - clipArea[2] + 1)

    #get the dimension List
    dimNames = ["ngrids_" + varName, "y", "x"]
    dims = getDims(file,  (gridCount, clipSize[1], clipSize[0]), dimNames)

    for idx in range(0,len(times)):
        cube = clipToExtrema(wec[times[idx]][0], clipArea)
        # adjust for time changes
        if we.getGpi().isRateParm():
            durRatio = (float(times[idx][1] - times[idx][0])) / float((times[idx][1] - times[idx][0]))
            cube = (cube * durRatio).astype(numpy.float32)
        cube = numpy.resize(cube, (clipSize[1],clipSize[0]))
        # Round the values according to the precision
        if trim:
            precision = pow(10, we.getGpi().getPrecision())
    
            if krunch:
                format, multipler, offset, fillValue, pythonType = \
                  calcKrunchValues(we)
            else:
                format, multipler, offset, fillValue, pythonType = \
                 ('f', None, None, -30000.0, numpy.float32)
    
            # krunch
            if multipler is not None:
                cube = ((cube - offset) * multipler)
                roundMask = numpy.where(numpy.greater(cube, 0), 1.0, -1.0)
                cube = (cube + (0.5 * roundMask)).astype(pythonType)
            # normal trim
            else:
                roundMask = numpy.where(numpy.greater(cube, 0), 1.0, -1.0)
                trimmed = (cube * precision + (0.5 * roundMask))
                trimmed = numpy.array(trimmed).astype(numpy.int32)
                cube = numpy.array(trimmed).astype(numpy.float32)
                cube = numpy.array(cube / precision).astype(numpy.float32)
    
        else:
            format, multipler, offset, fillValue, pythonType = \
             ('f', None, None, -30000.0, numpy.float32)
    
        # mask the data
        cube = numpy.where(mask, cube, fillValue).astype(pythonType)
    
        if(idx == 0):
            # create the variable
            var = file.createVariable(varName, format, dims)
            if multipler is not None:
                setattr(var, "dataMultiplier", 1.0 / multipler)
                setattr(var, "dataOffset", offset)
        
            # Store the attributes
            storeWEAttributes(var, we, timeList, databaseID, clipArea)
            setattr(var, "fillValue", fillValue)
        
            ## Extract the GridDataHistory info and save it
            storeGridDataHistory(file, we, wec, trList, timeRange)
        # Save the grids to the netCDF file
        var[idx] = numpy.flipud(cube)

    logEvent("Saved ", gridCount, " ", varName, " grids")

    return gridCount

###-------------------------------------------------------------------------###
### Stores the specified Vector WE in the netCDF file whose grids fall within
### the specified timeRange.
def storeVectorWE(we, trList, file, timeRange,
                  databaseID, mask, trim, clipArea, krunch):
    "Stores a vector weather element to the netCDF file"

    # get the data and store it in a Numeric array.
    magCube = []
    dirCube = []
    timeList = []
    times = []
    wec = WECache(we, trList)
    for t in trList:
        interTR = intersection(t, timeRange)
        if interTR is not None:
            times.append(t)
            timeList.append(interTR[0])
            timeList.append(interTR[1])

    varName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()
    gridCount = len(timeList) / 2
    
    ### Make sure we found some grids
    if gridCount == 0:
        logVerbose("No", varName, "grids found")

    # clipped size
    clipSize = (clipArea[1] - clipArea[0] + 1, clipArea[3] - clipArea[2] + 1)

    # make the variable name
    magVarName = we.getParmid().getParmName() + "_Mag_" + we.getParmid().getParmLevel()
    dirVarName = we.getParmid().getParmName() + "_Dir_" + we.getParmid().getParmLevel()
    varName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()

    #get the dimension List
    dimNames = ["ngrids_" + varName, "y", "x"]
    dims = getDims(file, (gridCount, clipSize[1], clipSize[0]), dimNames)

    for idx in range(0,len(times)):
        vecData = wec[times[idx]][0]
        mag = clipToExtrema(vecData[0], clipArea)
        dir = clipToExtrema(vecData[1], clipArea)
        if we.getGpi().isRateParm():
            durRatio = (float(times[idx][1] - times[idx][0])) / float((times[idx][1] - times[idx][0]))
            mag = (mag * durRatio).astype(numpy.float32)
        mag = numpy.resize(mag, (clipSize[1],clipSize[0]))
        dir = numpy.resize(dir, (clipSize[1],clipSize[0]))

        # Round the values according to the precision
        if trim:
            mprecision = pow(10, we.getGpi().getPrecision())
    
            if krunch:
                mformat, mmultipler, moffset, mfillValue, mpythonType = \
                  calcKrunchValues(we)
                dformat, dmultipler, doffset, dfillValue, dpythonType = \
                  ('b', 0.1, 0.0, -127, numpy.dtype(numpy.int8))
            else:
                mformat, mmultipler, moffset, mfillValue, mpythonType = \
                 ('f', None, None, -30000.0, numpy.dtype(numpy.float32))
                dformat, dmultipler, doffset, dfillValue, dpythonType = \
                 ('f', None, None, -30000.0, numpy.dtype(numpy.float32))
    
            # krunch magnitude
            if mmultipler is not None:
                mag = ((mag - moffset) * mmultipler)
                roundMask = numpy.where(numpy.greater(mag, 0), 1.0, -1.0)
                mag = (mag + (0.5 * roundMask)).astype(mpythonType)
            # normal trim for magnitude
            else:
                roundMask = numpy.where(numpy.greater(mag, 0), 1.0, -1.0)
                trimmed = (mag * mprecision + (0.5 * roundMask))
                trimmed = numpy.array(trimmed).astype(numpy.int32)
                mag = numpy.array(trimmed).astype(numpy.float32)
                mag = numpy.array(mag / mprecision).astype(numpy.float32)
    
            # krunch direction
            if dmultipler is not None:
                dir = ((dir - doffset) * dmultipler)
                roundMask = numpy.where(numpy.greater(dir, 0), 1.0, -1.0)
                dir = (dir + (0.5 * roundMask)).astype(dpythonType)
    
            # normal trim for direction
            else:
                dir = numpy.array((dir + (0.5 * 10)) / 10).astype(numpy.int32)
                dir = numpy.array(dir * 10).astype(numpy.float32)
                mask360 = numpy.greater_equal(dir, 360.0)
                dir = numpy.where(mask360, dir - 360.0, dir).astype(numpy.float32)
    
        else:
            mformat, mmultipler, moffset, mfillValue, mpythonType = \
              ('f', None, None, -30000.0, numpy.dtype(numpy.float32))
            dformat, dmultipler, doffset, dfillValue, dpythonType = \
              ('f', None, None, -30000.0, numpy.dtype(numpy.float32))

        
        mag = numpy.where(mask, mag, mfillValue).astype(mpythonType)
        dir = numpy.where(mask, dir, dfillValue).astype(dpythonType)

        if(idx == 0):
            # create the variable
            magVar = file.createVariable(magVarName, numpy.dtype(mpythonType), dims)
            dirVar = file.createVariable(dirVarName, numpy.dtype(dpythonType), dims)
            if mmultipler is not None:
                setattr(magVar, "dataMultiplier", 1.0 / mmultipler)
                setattr(magVar, "dataOffset", moffset)
            if dmultipler is not None:
                setattr(dirVar, "dataMultiplier", 1.0 / dmultipler)
                setattr(dirVar, "dataOffset", doffset)
    
            
        
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
            storeGridDataHistory(file, we, wec, trList, timeRange)
        # Save the grid to the netCDF file
        magVar[idx] = numpy.flipud(mag)
        dirVar[idx] = numpy.flipud(dir)
        
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
def storeWeatherWE(we, trList, file, timeRange, databaseID, mask, clipArea):
    "Stores the Weather weather element to the netCDF file"

    # get the data and store it in a Numeric array.
    byteCube = []
    keyList = []
    timeList = []
    times = []
    wec = WECache(we, trList)
    for t in trList:
        interTR = intersection(t, timeRange)
        if interTR is not None:
            times.append(t)
            wx = wec[t]
            # Save times for these grids in a list 
            timeList.append(interTR[0])
            timeList.append(interTR[1])
            keyList.append(wx[0][1])

    # make the variable name
    varName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()
    gridCount = len(timeList) / 2

    ### Make sure we found some grids
    if gridCount == 0:
        logVerbose("No", varName, "grids found")

    # clipped size
    clipSize = (clipArea[1] - clipArea[0] + 1, clipArea[3] - clipArea[2] + 1)

    #get the dimension List
    dimNames = ["ngrids_" + varName, "y", "x"]
    dims = getDims(file, (gridCount, clipSize[1], clipSize[0]), dimNames)
    
    # create the netCDF variable - 'b' for byte type
    var = file.createVariable(varName, 'b', dims)
    fillValue = -127
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

    for idx in range(0,len(times)):
        byteCube = clipToExtrema(wec[times[idx]][0][0], clipArea)
        byteCube = numpy.resize(byteCube, (clipSize[1],clipSize[0]))

        #  Process the weather keys so we store only what is necessary
        (keyList[idx], byteCube) = collapseKey(keyList[idx], byteCube)

        # Mask the values
        byteCube = numpy.where(mask, byteCube, fillValue).astype(numpy.int8)

        # Save the grids to the netCDF file
        var[idx] = numpy.flipud(byteCube)
        
        # now save the weather keys in the netCDF file
        for k in range(0, len(keyList[idx])):
            for c in range(0, len(keyList[idx][k])):
                chars[idx][k][c] = keyList[idx][k][c]
        if len(byteCube):
            keyVar[idx:] = chars[idx]

        if idx == 0:
            # Store the attributes
            storeWEAttributes(var, we, timeList, databaseID, clipArea)
            setattr(var, "fillValue", fillValue)
        
            ## Extract the GridDataHistory info and save it
            storeGridDataHistory(file, we, wec, trList, timeRange)

    logEvent("Saved", gridCount, varName, "grids")

    return gridCount

###-------------------------------------------------------------------------###
# Stores the specified Discrete WE in the netCDF file whose grids fall within
### the specified timeRange.
def storeDiscreteWE(we, trList, file, timeRange, databaseID, mask, clipArea):
    "Stores the Weather weather element to the netCDF file"

    # get the data and store it in a Numeric array.
    byteCube = []
    keyList = []
    timeList = []
    times = []
    wec = WECache(we, trList)
    for t in trList:
        interTR = intersection(t, timeRange)
        if interTR is not None:
            times.append(t)
            dis = wec[t][0]
            # Save times for these grids in a list
            timeList.append(interTR[0])
            timeList.append(interTR[1])
            keyList.append(dis[1])

    # make the variable name
    varName = we.getParmid().getParmName() + "_" + we.getParmid().getParmLevel()
    gridCount = len(timeList) / 2

    ### Make sure we found some grids
    if gridCount == 0:
        logVerbose("No", varName, "grids found")

    # clipped size
    clipSize = (clipArea[1] - clipArea[0] + 1, clipArea[3] - clipArea[2] + 1)

    #get the dimension List
    dimNames = ["ngrids_" + varName, "y", "x"]
    dims = getDims(file, (gridCount, clipSize[1], clipSize[0]), dimNames)

    # create the netCDF variable - 'b' for byte type
    var = file.createVariable(varName, 'b', dims)
    fillValue = -127
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

    for idx in range(0,len(times)):
        byteCube = clipToExtrema(wec[times[idx]][0][0], clipArea)
        byteCube = numpy.resize(byteCube, (clipSize[1],clipSize[0]))
        #  Process the discrete keys so we store only what is necessary
        (keyList[idx], byteCube) = collapseKey(keyList[idx], byteCube)

        # Mask the values
        byteCube = numpy.where(mask, byteCube, fillValue).astype(numpy.int8)

        # Save the grids to the netCDF file
        var[idx] = numpy.flipud(byteCube)

        # now save the discrete keys in the netCDF file
        for k in range(0, len(keyList[idx])):
            for c in range(0, len(keyList[idx][k])):
                chars[idx][k][c] = keyList[idx][k][c]
        if len(byteCube):
            keyVar[idx:] = chars[idx]

        if idx == 0:
            # Store the attributes
            storeWEAttributes(var, we, timeList, databaseID, clipArea)
            setattr(var, "fillValue", fillValue)

            ## Extract the GridDataHistory info and save it
            storeGridDataHistory(file, we, wec, trList, timeRange)

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





def executeIfpNetCDF(host, port, outputFilename, parmList, databaseID, startTime, endTime, mask, geoInfo, compressFileFlag, configFileName, compressFileFactor, trim, krunch, userID):

    if ifpNetcdfLogger is None:
        initLogger()
        
    if type(parmList) != list:
        parmList = JUtil.javaStringListToPylist(parmList)
        
    argDict = {"host":host, "port":port, "outputFilename":outputFilename, "parmList":parmList,
               "databaseID":databaseID, "startTime":startTime, "endTime":endTime, "mask":mask,
               "geoInfo":geoInfo, "compressFile":compressFileFlag, "configFileName":configFileName,
               "compressFileFactor":compressFileFactor, "trim":trim, "krunch":krunch, "userID":userID}

    argDict = checkArgs(argDict) 
    
    start = time.time()
    client = None

    try:
        timeRange = makeTimeRange(argDict['startTime'], argDict['endTime'])
    except:
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
    validPointCount = numpy.add.reduce(numpy.add.reduce(maskGrid)) 

    # Determine sampling definition
    samplingDef = getSamplingDefinition(client, argDict['configFileName'])
    logVerbose("Sampling Definition:", samplingDef)

    # Open the netCDF file
    file = netcdf.netcdf_file(argDict['outputFilename'], 'w')

    totalGrids = 0
    for p in argDict['parmList']:

        we = db.getItem(p)
        
        #determine inventory that we want to keep
        weInv = determineSamplingValues(samplingDef, p, we.getKeys(), start)
        if len(weInv) != 0:
            gridType = str(we.getGpi().getGridType())
    
            if gridType == "SCALAR":
                nGrids = storeScalarWE(we, weInv, file, timeRange,
                  argDict['databaseID'], maskGrid, argDict['trim'], clipArea,
                  argDict['krunch'])
            elif gridType == "VECTOR":
                nGrids = storeVectorWE(we, weInv, file, timeRange,
                  argDict['databaseID'], maskGrid, argDict['trim'], clipArea,
                  argDict['krunch'])
            elif gridType == "WEATHER":
                nGrids = storeWeatherWE(we, weInv, file, timeRange,
                  argDict['databaseID'], maskGrid, clipArea)
            elif gridType == "DISCRETE":
                nGrids = storeDiscreteWE(we, weInv, file, timeRange,
                  argDict['databaseID'], maskGrid, clipArea)
            else:
                s = "Grids of type: " + we.gridType + " are not supported, " + \
                  "parm=" + p
                logProblem(s)
                raise Exception, s
    
            totalGrids = totalGrids + nGrids

    # store the topo and lat, lon grids if the -g was present
    if argDict["geoInfo"]:
        storeTopoGrid(client, file, argDict['databaseID'], maskGrid, clipArea)
        storeLatLonGrids(client, file, argDict['databaseID'], maskGrid,
          argDict['krunch'], clipArea)
        totalGrids = totalGrids + 3

    storeGlobalAtts(file, argDict);

    file.close()

    fu = os.stat(argDict['outputFilename'])[stat.ST_SIZE]
    mb = fu / (1024.0 * 1024.0)
    logEvent("Uncompressed Size: ", "%-.3f" % (mb), " MBytes")
    stop1 = time.time()
    a = os.times()
    cpu = a[0] + a[1]

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
    stop = time.time()
    logEvent("Elapsed/CPU time: ", "%-.2f" % (stop1 - start),
      "/", "%-.2f" % cpu, "processing,", "%-.2f" % (stop - stop1), "/",
      "%-.2f" % (cpugz - cpu), "compress,",
      "%-.2f" % (stop - start), "/", "%-.2f" % (cpugz), "total")
    #logEvent("stats: ", client.getStats())
    logEvent("ifpnetCDF Finished")
    


###-------------------------------------------------------------------------###
### Main program
def main(argv): 
    
    #LogStream.logEvent(AFPS.DBSubsystem_getBuildDate(),
    #  AFPS.DBSubsystem_getBuiltBy(), AFPS.DBSubsystem_getBuiltOn(),
    #  AFPS.DBSubsystem_getBuildVersion())

    if type(argv) != 'list':
        argv = JUtil.javaStringListToPylist(argv) 
    argDict = getArgs(argv) 
    logEvent("ifpnetCDF Starting")
    logEvent("Command: ",iscUtil.tupleToString(*argv))
     

    executeIfpNetCDF(argDict["host"], argDict["port"], argDict["outputFilename"], argDict["parmList"], argDict["databaseID"],
                     argDict["startTime"], argDict["endTime"], argDict["mask"], argDict["geoInfo"], argDict["compressFile"],
                     argDict["configFileName"], argDict["compressFileFactor"], argDict["trim"], argDict["krunch"], argDict["userID"])


if __name__ == "__main__":
    pass
    #main()
    # profile stuff
#     import profile, pstats
#     profile.run('main()', 'pyprof.out')
#     p = pstats.Stats('pyprof.out')
#     p.strip_dirs()
#     p.sort_stats('time', 'calls').print_stats(15)
#     p.print_callers(15)

