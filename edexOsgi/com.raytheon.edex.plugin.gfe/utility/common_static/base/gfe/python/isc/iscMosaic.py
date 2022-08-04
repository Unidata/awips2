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
# ------------------------------------------------------------------------------
#
# Port of iscMosaic.py
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Jul 06, 2009 1995      bphillip  Initial Creation.
# Jan 17, 2013 15588     jdynina   Fixed Publish history removal
# Mar 12, 2013 1759      dgilling  Remove unnecessary command line processing.
# Apr 24, 2013 1941      dgilling  Re-port WECache to match A1.
# May 08, 2013 1988      dgilling  Fix history handling bug in__getDbGrid().
# May 23, 2013 1759      dgilling  Remove unnecessary imports.
# Jun 05, 2013 2063      dgilling  Change __siteInDbGrid() to call 
#                                  IFPWE.history() like A1.
# Sep 05, 2013 2307      dgilling  Fix breakage caused by #2044.
# Oct 31, 2013 2508      randerso  Change to use DiscreteGridSlice.getKeys()
# Nov 05, 2013 2517      randerso  Restructured logging so it could be used
#                                  by WECache
#                                  Changed WECache to limit the number of
#                                  cached grids kept in memory
# Jan 09, 2014 16952     randerso  Fix regression made in #2517 which caused
#                                  errors with overlapping grids
# Feb 04, 2014 17042     ryu       Check in changes for randerso.
# Apr 03, 2014 2737      randerso  Allow iscMosaic to blankOtherPeriods even
#                                  when no grids received
# Apr 11, 2014 17242     dgilling  (code checked in by zhao)
# Jul 22, 2014 17484     randerso  Update cluster lock time to prevent time out
# Aug 07, 2014 3517      randerso  Improved memory utilization and error
#                                  handling when unzipping input file.
# Aug 14, 2014 3526      randerso  Fix bug in WECache that could incorrectly
#                                  delete grids in the destination database
# Feb 17, 2015 4139      randerso  Replaced call to iscTime.timeFromComponents
#                                  with call to calendar.timegm
# Apr 23, 2015 4383      randerso  Changed to log arguments to aid in
#                                  troubleshooting
# Apr 23, 2015 4259      njensen   Updated for new JEP API
# Apr 25, 2015 4952      njensen   Updated for new JEP API
# Aug 06, 2015 4718      dgilling  Optimize casting when using where with
#                                  NumPy 1.9.
# Oct 05, 2015 4951      randerso  Fixed siteInDbGrid to retrieve history from
#                                  the cache so it sees changes that have not
#                                  yet been written to the database
# Oct 05, 2015 4961      randerso  Fix __calcBlankingTimes to handle persistent
#                                  grids
# Sep 12, 2016 5861      randerso  Remove references to IFPServerConfigManager
#                                  which was largely redundant with IFPServer.
# Feb 22, 2017 6143      randerso  Moved renewal of cluster lock into inner
#                                  loop to avoid unexpected time out
# Oct 31, 2016 5979      njensen   Cast to primitives for compatibility
# Feb 06, 2017 5959      randerso  Removed Java .toString() calls 
# Apr 05, 2017 6224      randerso  Include dbID in cluster lock name
# Oct 05, 2015 4951      randerso  Fixed siteInDbGrid to retrieve history
#                                  from the cache so it sees changes that
#                                  have not yet been written to the database
# Oct 05, 2015 4961      randerso  Fix __calcBlankingTimes to handle
#                                  persistent grids
# Feb 22, 2017 6143      randerso  Moved renewal of cluster lock into inner
#                                  loop to avoid unexpected time out
# Apr 05, 2017 6224      randerso  Include dbID in cluster lock name
# Jan 04, 2018 7178      randerso  Change to use setKeys() instead of setKey()
# Jan 16, 2018 6867      dgilling  Remove use of LinkedHashMap for HashMap.
# Aug 08, 2018 7178      randerso  Fixed gzip file handling
# Aug 08, 2018 DCS 19452 dfriedman Create jobs for and accept work from
#                                  IscMosaicJobManager.
# Nov 09, 2018 DR 21001  dfriedman Fix handling of renamed grids.
# May 03, 2019 7842      dgilling  Update to use library netcdf4-python.
#
##

##
# This is a base file that is not intended to be overridden.
##



import os, stat, time, calendar
import gzip
import shutil
import iscTime, iscUtil, mergeGrid
import netCDF4
import numpy
import JUtil

from java.lang import System
from java.util import ArrayList
from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DFloat
from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DByte
from com.raytheon.uf.common.time import TimeRange
from org.locationtech.jts.geom import Coordinate
from java.awt import Point

from com.raytheon.edex.plugin.gfe.server import IFPServer
from com.raytheon.edex.plugin.gfe.smartinit import IFPDB
from com.raytheon.uf.common.dataplugin.gfe import GridDataHistory
from com.raytheon.uf.common.dataplugin.gfe import RemapGrid
OriginType = GridDataHistory.OriginType
from com.raytheon.uf.common.dataplugin.gfe.config import ProjectionData
ProjectionType = ProjectionData.ProjectionType
from com.raytheon.uf.common.dataplugin.gfe.db.objects import GridLocation
from com.raytheon.uf.common.dataplugin.gfe.slice import DiscreteGridSlice
from com.raytheon.uf.common.dataplugin.gfe.slice import ScalarGridSlice
from com.raytheon.uf.common.dataplugin.gfe.slice import VectorGridSlice
from com.raytheon.uf.common.dataplugin.gfe.slice import WeatherGridSlice
from com.raytheon.uf.common.dataplugin.gfe.discrete import DiscreteKey
from com.raytheon.uf.common.dataplugin.gfe.weather import WeatherKey
from com.raytheon.uf.common.dataplugin.gfe.server.notify import UserMessageNotification
from com.raytheon.edex.plugin.gfe.util import SendNotifications
from com.raytheon.uf.common.status import UFStatus
Priority = UFStatus.Priority
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID
CoordinateType = ReferenceData.CoordinateType
from com.raytheon.uf.edex.database.cluster import ClusterLockUtils

BATCH_DELAY = 0.0

MAX_CACHE_BYTES = 64 * 1024 * 1024 # 64 MB

ISC_USER = "isc"

logger = None

## Logging methods ##
def initLogger(logName):
    import logging
    global logger
    logger = iscUtil.getLogger("iscMosaic", logName=logName, logLevel=logging.INFO)

def printTR(tr):
    """
    Format time range for printing (yymmdd_hhmm,yymmdd_hhmm)
    Works with list or tuple
    
    Args:
        tr: the time range to format
    
    Returns:
        the formatted time range string
    """
    if tr is not None:
        fmt = "%Y%m%d_%H%M"
        s = '(' + time.strftime(fmt, time.gmtime(tr[0])) + ',' + \
          time.strftime(fmt, time.gmtime(tr[1])) + ')'
        return s
    else:
        return "None"

def printShortTR(tr):
    """
    Format time range for printing (dd/hh,dd/hh)
    Works with list or tuple
    
    Args:
        tr: the time range to format
    
    Returns:
        the formatted time range string
    """
    if tr is not None:
        fmt = "%d/%H"
        s = '(' + time.strftime(fmt, time.gmtime(tr[0])) + '->' + \
          time.strftime(fmt, time.gmtime(tr[1])) + ')'
        return s
    else:
        return "None"

def now():
    return os.times()[4]

class WECache(object):
    """
    Cache representing the grids for a weather element that overlap a time range.
    The cache will keep a limited number of grids in memory. This limit is determined
    at initialization to be the number of grids that will fit in MAX_CACHE_BYTES (or a minimum of 2).
    
    This is not a general purpose cache. It's behavior is designed to match the access patterns of iscMosaic
    """
    def __init__(self, we, tr=None):
        self._we = we
        logger.debug("WECache creating: %s", str(self._we.getParmid()))

        gridType = self._we.getGridType()
        if gridType == "SCALAR":
            bytesPerCell = 4
        elif gridType == "VECTOR":
            bytesPerCell = 8
        elif gridType == "WEATHER":
            bytesPerCell = 1
        elif gridType == "DISCRETE":
            bytesPerCell = 1

        gloc = self._we.getGpi().getGridLoc()
        gridBytes = int(gloc.getNx()) * int(gloc.getNy()) * bytesPerCell

        self._maxCacheSize = max(2, MAX_CACHE_BYTES // gridBytes)
        self._batchSize = self._maxCacheSize // 2
        logger.debug("WECache max grids: %d, batch grids: %d", self._maxCacheSize, self._batchSize)

        self._inv = {}              # Map containing keys for all inventory
        self._invCache = None       # Cache of inventory sorted by start time
        self._dirty = set()         # Cache written to but not flushed to disk
        self._populated = set()     # Grid is currently in the cache
        self._loaded = set()        # Grid has been loaded into cache at least once

        # get only keys that overlap tr
        javaInv = self._we.getKeys(iscUtil.toJavaTimeRange(tr))
        pyInv = []
        for i in range(javaInv.size()):
            pyInv.append(iscUtil.transformTime(javaInv.get(i)))

        # create unpopulated entries for the entire inventory
        for invTr in pyInv:
            self._inv[invTr] = None

        # populate first BATCH_READCOUNT grids
        if len(pyInv) > self._batchSize:
            trList = pyInv[:self._batchSize - 1]
            # add on the last tr since it is used by __splitGridsOnProcBoundary
            trList.append(pyInv[-1])

        else:
            trList = pyInv

        self.__loadGrids(trList)

    def __loadGrids(self, trList):
        javaTRs = ArrayList(len(trList))
        for tr in trList:
            javaTRs.add(iscUtil.toJavaTimeRange(tr))

        gridsAndHist = self._we.get(javaTRs, True)
        for idx in range(gridsAndHist.size()):
            pair = gridsAndHist.get(idx)
            tr = iscUtil.transformTime(pair.getFirst().getValidTime())

            if tr in self._loaded:
                logger.debug("WECache reloading: %s", printTR(tr))
            else:
                logger.debug("WECache loading: %s", printTR(tr))

            g = self.__encodeGridSlice(pair.getFirst())
            h = self.__encodeGridHistory(pair.getSecond())
            self._inv[tr] = (g, h)
            self._populated.add(tr)
            self._loaded.add(tr)

    def keys(self):
        if not self._invCache:
            self._invCache = tuple(sorted(self._inv.keys(), key=lambda t: t[0]))
        return self._invCache

    def __iter__(self):
        return iter(self.keys())

    def __contains__(self, item):
        return item in self.keys()

    def __getitem__(self, tr):
        logger.debug("WECache getting: %s", printTR(tr))
        if tr in self._populated or tr in self._dirty:
            return self._inv[tr]

        if tr in self._inv:
            self.__handleCacheMiss(tr)
            return self._inv[tr]
        else:
            return None

    def __handleCacheMiss(self, tr):
        """
        This function is called when a time range is requested that is not currently in the cache.
        It will load the next batch of unpopulated grids in time range order starting with the time range passed in.
        
        If the cache does not have room for a batch of grids to be loaded without exceeding the max cache size
        the earliest dirty grids (or clean if not enough dirty grids are found) are flushed to disk before reading
        the next batch.
        
        Args:
            tr: the missing time range
        """
        logger.debug("WECache miss: %s", printTR(tr))
        # determine next batch of keys to read
        toRead = self.keys()
        toRead = toRead[toRead.index(tr):]
        toRead = sorted(set(toRead) - self._populated, key=lambda t: t[0])
        toRead = toRead[:self._batchSize]

        # if not room to read another batch
        if len(self._populated) + self._batchSize > self._maxCacheSize:
            toFlush = []
            # first flush dirty grids
            toFlush += self._populated & self._dirty

            # then flush clean grids if necessary
            toFlush += self._populated - self._dirty

            # flush only enough to read a batch
            toFlush = sorted(toFlush, key=lambda t: t[0])
            toFlush = toFlush[:self._maxCacheSize - self._batchSize]
            self.__flushGrids(toFlush)

        self.__loadGrids(toRead)

    def __flushGrids(self, trList):
        """
        Flush a list of time ranges from the cache.
        Dirty time ranges will be written to disk.
        Writes will be done in _batchSize groups
        
        Args:
            trList: the list of time ranges to be flushed
        """
        logger.debug("WECache flushing: %d grids", len(trList))

        saveRequest = {}
        saveList = []  # python time ranges covered by this saveRequest
        saveSize = 0   # number of grids in saveRequest

        # get full time range for flush
        if (len(trList)):
            sortedList = sorted(trList, key=lambda t: t[0])
            flushTR = (sortedList[0][0], sortedList[-1][1])
        else:
            flushTR = (0, 2 ** 31 - 1) # all times

        timeSpan = None  # time span if this contiguous batch
        gridsToSave = []  # grids in this contiguous batch
        saveBatch = False
        for tr in self:
            if tr[1] <= flushTR[0]:
                continue
            if tr[0] >= flushTR[1]:
                break

            dirty = tr in self._dirty
            if dirty:
                logger.debug("WECache storing: %s", printTR(tr))
                saveList.append(tr)

                pyGrid, pyHist = self._inv[tr]
                if pyGrid is not None:
                    javaGrid = self.__buildJavaGridSlice(tr, pyGrid, pyHist)
                    gridsToSave.append(javaGrid)
                if timeSpan is None:
                    timeSpan = [tr[0], tr[1]]
                else:
                    timeSpan[1] = tr [1]
                saveBatch = len(gridsToSave) >= self._batchSize

            else: # clean grid
                # save contiguous dirty blocks
                saveBatch = timeSpan is not None

                # if populated and clean just purge from cache
                if tr in self._populated:
                    logger.debug("WECache purging: %s", printTR(tr))
                    self._inv[tr] = None
                    self._populated.remove(tr)
                else:
                    # skip any clean unpopulated grids
                    logger.debug("WECache skipping: %s", printTR(tr))

            if saveBatch:
                # add this contiguous batch to saveRequest
                logger.debug("WECache saving %d grids in %s", len(gridsToSave), printTR(timeSpan))
                gridSaveTR = iscUtil.toJavaTimeRange(timeSpan)
                saveRequest[gridSaveTR] = gridsToSave
                timeSpan = None
                saveBatch = False
                saveSize += len(gridsToSave)
                gridsToSave = []

            # if this saveRequest has reached the batchSize save it
            if saveSize >= self._batchSize:
                try:
                    self._we.put(saveRequest)
                except:
                    raise
                else: # no exceptions on save, clear saved grids from cache
                    # depopulate save grids
                    for tr in saveList:
                        self._inv[tr] = None
                        self._populated.discard(tr)
                        self._dirty.remove(tr)
                    saveRequest.clear()
                    saveList = []
                    saveSize = 0

        # save partial batch if necessary
        if len(saveList):
            if timeSpan is not None:
                logger.debug("WECache saving %d grids in %s", len(gridsToSave), printTR(timeSpan))
                gridSaveTR = iscUtil.toJavaTimeRange(timeSpan)
                saveRequest[gridSaveTR] = gridsToSave

            try:
                self._we.put(saveRequest)
            except:
                raise
            else: # no exceptions on save, clear saved grids from cache
                # depopulate save grids
                for tr in saveList:
                    self._inv[tr] = None
                    self._populated.discard(tr)
                    self._dirty.remove(tr)

        return

    def __setitem__(self, tr, value):
        if value is None:
            logger.debug("WECache erasing: %s", printTR(tr))
            grid = hist = None
        else:
            logger.debug("WECache setting: %s", printTR(tr))
            grid, hist = value

        # Remove any overlapping grids
        tokill = []
        for itr in self._inv:
            if self.overlaps(tr, itr):
                tokill.append(itr)
        for i in tokill:
            del self._inv[i]
            self._dirty.discard(i)
            self._populated.discard(i)
            self._loaded.discard(i)
            self._invCache = None

        # if cache full flush some grids to disk
        if len(self._populated) >= self._maxCacheSize:
            toFlush = []
            # first flush dirty grids
            toFlush += self._populated & self._dirty

            # then flush clean grids if necessary
            toFlush += self._populated - self._dirty

            # flush a full batch is possible
            toFlush = sorted(toFlush, key=lambda t: t[0])
            toFlush = toFlush[:self._batchSize]
            self.__flushGrids(toFlush)

        # Now add the new grid
        self._inv[tr] = (grid, hist)
        self._dirty.add(tr)
        self._loaded.add(tr)
        self._invCache = None
        if grid is not None:
            self._populated.add(tr)

    def flush(self):
        """Writes all dirty time ranges in the WECache to HDF5/DB"""
        # flush entire inventory
        self.__flushGrids(self.keys())

    def overlaps(self, tr1, tr2):
        if (tr1[0] >= tr2[0] and tr1[0] < tr2[1]) or \
           (tr2[0] >= tr1[0] and tr2[0] < tr1[1]):
            return True
        return False

    def __encodeGridSlice(self, grid):
        gridType = self._we.getGridType()
        if gridType == "SCALAR":
            return grid.getNDArray()
        elif gridType == "VECTOR":
            vecGrids = grid.getNDArray()
            return (vecGrids[0], vecGrids[1])
        elif gridType == "WEATHER" or gridType == "DISCRETE":
            keys = grid.getKeys()
            keyList = []
            for theKey in keys:
                keyList.append(str(theKey))
            return (grid.getNDArray(), keyList)

    def __encodeGridHistory(self, histories):
        retVal = []
        for i in range(histories.size()):
            retVal.append(histories.get(i).getCodedString())
        return tuple(retVal)

    def __buildJavaGridSlice(self, tr, grid, history):
        javaTR = iscUtil.toJavaTimeRange(tr)
        javaHist = self.__buildJavaGridHistory(history)

        gridType = self._we.getGridType()
        if gridType == "SCALAR":
            return self._we.buildScalarSlice(javaTR, grid.astype(numpy.float32), javaHist)
        elif gridType == "VECTOR":
            return self._we.buildVectorSlice(javaTR, grid[0].astype(numpy.float32), grid[1].astype(numpy.float32), javaHist)
        elif gridType == "WEATHER":
            return self._we.buildWeatherSlice(javaTR, grid[0].astype(numpy.byte), str(grid[1]), javaHist)
        elif gridType == "DISCRETE":
            return self._we.buildDiscreteSlice(javaTR, grid[0].astype(numpy.byte), str(grid[1]), javaHist)

    def __buildJavaGridHistory(self, histories):
        retVal = ArrayList()
        blankPubTime = "Fcst" in str(self._we.getParmid().getDbId())
        for histEntry in histories:
            javaHist = GridDataHistory(histEntry)
            # strip out publish time to allow for publishing correctly
            # when merging Fcst out of A1
            if blankPubTime:
                javaHist.setPublishTime(None)
            retVal.add(javaHist)
        return retVal


class IscMosaic:
    def __init__(self, args):
        self.__mysite = args['siteID']
        ifpServer = IFPServer.getActiveServer(self.__mysite)
        if ifpServer is None:
            raise Exception("No active IFPServer for site: " + self.__mysite)
        
        self.__config = ifpServer.getConfig()
        
        self.__userID = args['userID']
        self.__db = None    # ifpServer database object
        self.__dbGrid = None
        self.__parmsToProcess = list(args['parmsToProcess'])
        self.__blankOtherPeriods = args['blankOtherPeriods']
        self.__altMask = args['altMask']
        self.__replaceOnly = args['replaceOnly']
        self.__eraseFirst = args['eraseFirst']
        self.__announce = args['announce']
        self.__renameWE = args['renameWE']
        self.__iscSends = args['iscSends']
        if args['databaseID'] is not None:
            self.__databaseID = args['databaseID']
        else:
            self.__databaseID = self.__mysite + "_GRID__ISC_00000000_0000"
        self.__inFile = args['inFile']
        self.__ignoreMask = args['ignoreMask']
        self.__adjustTranslate = args['adjustTranslate']
        self.__parmsToIgnore = args['parmsToIgnore']
        self.__gridDelay = args['gridDelay']
        self.__logFile = args['logFileName']

        startTime = 0
        if args['startTime'] is not None:
            startTime = self.__decodeTimeString(args['startTime'])
        endTime = int(2 ** 30 - 1 + 2 ** 30)
        if args['endTime'] is not None:
            endTime = self.__decodeTimeString(args['endTime'])
        self.__processTimePeriod = (startTime, endTime)

        self.__ncFile = None
        self.__parmMap = {}

        initLogger(self.__logFile)
        logger.info("iscMosaic Starting args: %s", str(args))

    # Scan the input file to determine the lock names that will be needed and
    # add them to the job.
    def prepare(self, job):
        self.__commonStartup()
        self.__prepareInputFile(self.__inFile, job)
        logger.info("iscMosaic prepare finished")

    def __commonStartup(self):
        # get the WxDefinition and DiscreteDefinition
        self.__wxDef = self.__config.getWxDefinition()
        self.__disDef = self.__config.getDiscreteDefinition()

        self.__db = IFPDB(self.__databaseID)

        # parms in database
        if len(self.__parmsToProcess) == 0:
            parmsInDb = self.__db.getKeys()
            for i in range(0, parmsInDb.size()):
                self.__parmsToProcess.append(str(parmsInDb.get(i)))

        # get office type information
        self.__myOfficeType = self.__config.officeType()

    def __prepareInputFile(self, filename, job):
        start = now()

        ncfile = self.__openNcFile(filename)

        for lockName in self.__getParmMap(ncfile):
            job.addLockName(lockName)

        ncfile.close()

        stop = now()
        logger.info("Elapsed time for prepare: %-.2f", stop - start)

    def __openNcFile(self, filename):
        logger.info("Processing file=%s", filename)
        fsize = os.stat(filename)[stat.ST_SIZE]
        logger.info("Input file size: %d", fsize)

        gzipFile = None
        unzippedFile = None
        gzipped = True
        try:
            with gzip.open(filename, 'rb') as gzipFile:
                # If not gzipped, may not raise an exception until read is attempted
                gzipFile.read(1)
                gzipFile.seek(0)
                
                with open(filename + ".unzipped", 'wb') as unzippedFile:
                    shutil.copyfileobj(gzipFile, unzippedFile)
        except OSError as e:
            if str(e).startswith("Not a gzipped file"):
                gzipped = False
            else:
                raise
        else:
            # no errors, close and rename the file
            os.rename(unzippedFile.name, gzipFile.filename)
            gzipFile = unzippedFile = None
        finally:
            # close the files in case of error
            if unzippedFile is not None and not gzipped:
                os.remove(unzippedFile.name)

        ncfile = netCDF4.Dataset(filename, "r")

        # check version
        fileV = ncfile.fileFormatVersion
        if fileV != "20010816" and fileV != "20030117":
            logger.error("Incompatible file format found")
            raise Exception("Incompatible file format")

        # get creation time
        self.__creTime = ncfile.creationTime
        creTimeString = time.asctime(time.gmtime(self.__creTime))
        logger.info("CreationTime: %s" , creTimeString)

        # get file start/end processing times
        self.__modProcTime = self.__getFileProcessingTimes(ncfile)
        if self.__modProcTime is None:
            return None
        logger.info("Process TR: %s", printTR(self.__modProcTime))
        return ncfile

    def __getParmMap(self, ncfile):
        parmMap = {}

        # process each parm in the netCDF file
        for parm in ncfile.variables:
            tup = self.__getNetCDFInputVariables(ncfile, parm)

            if tup is None:
                continue
            else:
                parmName, ncvars, remapHistory = tup

            # rename weather element
            if self.__renameWE:
                siteID = str(ncvars[0].siteID)
                incomingOfficeType = self.__config.getOfficeType(siteID)
                if incomingOfficeType != self.__myOfficeType:
                    idx = parmName.rfind("_")
                    parmName = parmName[0:idx] + incomingOfficeType + \
                      parmName[idx:]
                    logger.info("Renamed to: %s data from %s", parmName, siteID)

            # ignore this parm?
            if parmName in self.__parmsToIgnore:
                logger.info("Ignoring %s", parmName)
                continue

            # match in ifp database?
            if not parmName in self.__parmsToProcess and \
              len(self.__parmsToProcess) != 0:
                logger.info("Skipping %s", parmName)
                continue

            lockName = parmName + ":" + self.__databaseID
            parmMap[lockName] = parmName, ncvars, remapHistory

        return parmMap

    def processParm(self, job, lockName):
        start = now()
        logger.debug('processing parm for lock name %s', lockName)
        if self.__ncFile is None:
            self.__commonStartup()
            self.__ncFile = self.__openNcFile(self.__inFile)
            self.__parmMap = self.__getParmMap(self.__ncFile)
            self.__areaMask = None

        # prepare for the notification message
        self.__adjDataMsg = []

        parmName, ncvars, remapHistory = self.__parmMap[lockName]
        (pName, pTR, nGrids, nFail) = self.__processParm(parmName, ncvars, remapHistory, self.__inFile, lockName)

        #announce storage
        totalGrids = nGrids
        totalFails = nFail
        totalTimeRange = pTR
        if len(self.__announce) and totalGrids > 0:
            msg = self.__announce + self.__siteID + ' ' + repr(pName) + ' ' + printShortTR(totalTimeRange) + ' #Grids=' + repr(totalGrids)
            if totalFails:
                msg = msg + '[' + repr(totalFails) + ' FAILED]'
                notification = UserMessageNotification(msg, Priority.CRITICAL, "ISC", self.__mysite)
            else:
                notification = UserMessageNotification(msg, Priority.EVENTA, "ISC", self.__mysite)
            logger.info(msg)
            SendNotifications.send(notification)

        # announce "modified/adjusted" data
        if len(self.__announce) and len(self.__adjDataMsg):
            msg = "ISC Data Modified:\n" + "\n".join(self.__adjDataMsg)
            notification = UserMessageNotification(msg, Priority.EVENTA, "ISC", self.__mysite)
            SendNotifications.send(notification)

        logger.info("Elapsed time: %-.2f", now() - start)

    def dispose(self):
        if self.__ncFile is not None:
            self.__ncFile.close()
            self.__ncFile = None

    def __processParm(self, parmName, ncvars, history, filename, lockName):

        retries = 5
        retryAttempt = 0
        pName = parmName.replace("_SFC", "")
        totalTimeRange = None
        inTimesProc = []
        numFailed = 0

        self.__siteID = str(ncvars[0].siteID)
        inTimes = self.__getIncomingValidTimes(ncvars[0])
        logger.info("Processing %s #Grids=%d Site=%s", parmName, len(inTimes), self.__siteID)

        if self.__blankOtherPeriods or self.__eraseFirst or len(inTimes) > 0:
            while retryAttempt != retries:
                try:
                    # open up the ifpServer weather element
                    self.__dbwe = self.__db.getItem(parmName, ISC_USER)
                    self._wec = WECache(self.__dbwe, tr=self.__modProcTime)
                    self.__rateParm = self.__dbwe.getGpi().isRateParm()
                    self.__parmName = parmName

                    # get general info for the parm from the input file and output db
                    inGeoDict = self.__getInputGeoInfo(ncvars[0])
                    inFillV = self.__determineFillValue(ncvars[0])

                    gridType = ncvars[0].gridType
                    minV = self.__dbwe.getGpi().getMinValue()
                    # compute the site mask

                    if self.__areaMask is None:
                        self.__areaMask = self.__computeAreaMask().getGrid().getNDArray().astype(numpy.bool)

                    # create the mergeGrid class
                    mGrid = mergeGrid.MergeGrid(self.__creTime, self.__siteID, inFillV,
                      minV, self.__areaMask, gridType, self.__dbwe.getDiscreteKeys())

                    # erase all existing grids first?
                    self.__dbinv = self._wec.keys()
                    try:
                        self.__splitGridsOnProcBoundary(self.__modProcTime)
                    except:
                        logger.exception('Failure to splitGridsOnProcBoundary Parm=%s Time=%s',
                          parmName, printTR(self.__modProcTime))

                    if self.__eraseFirst:
                        self.__eraseAllGrids(self.__modProcTime)

                    # process each incoming grid
                    inTimesProc = []
                    numFailed = 0

                    # process incoming grids
                    for i in range(len(inTimes)):
                        # Put in a delay so we don't hammer the server so hard.
                        if self.__gridDelay > 0.0:
                            time.sleep(self.__gridDelay)
                        tr = iscTime.intersection(inTimes[i], self.__modProcTime)
                        if tr is not None:
                            inTimesProc.append(tr)
                            try:
                                logger.debug("Processing Grid: %s TR=%s", parmName, printTR(tr))

                                # get the grid and remap it
                                grid = self.__getGridFromNetCDF(gridType, ncvars, i)

                                # if WEATHER or DISCRETE, then validate and adjust keys
                                if self.__adjustTranslate:
                                    if gridType == "DISCRETE":
                                        grid = self.__validateAdjustDiscreteKeys(grid,
                                          self.__parmName, tr)
                                    elif gridType == "WEATHER":
                                        grid = self.__validateAdjustWeatherKeys(grid,
                                          self.__parmName, tr)

                                grid = self.__remap(self.__dbwe, grid, inGeoDict, inFillV)

                                # if rate parm, then may need to adjust the values
                                if self.__rateParm and inTimes[i] != tr:
                                    grid = self.__adjustForTime(inTimes[i], tr, grid,
                                      inFillV)

                                # merge the grids
                                self.__processIncomingGrid(lockName, grid, history[i],
                                  mGrid, tr, inFillV)

                            except:
                                logger.exception('Failure to process grid in file [%s] Parm=%s Time=%s',
                                  filename, parmName, printTR(tr))
                                numFailed = numFailed + 1

                        else:
                            logger.debug("Skipping Grid: %s TR=%s outside start/end range",
                                         parmName, printTR(tr))


                    # blank out any gaps
                    if self.__blankOtherPeriods == 1:
                        blankTimes = self.__calcBlankingTimes(inTimesProc)
                        # get updated inventory

                        for i in range(len(blankTimes)):
                            tr = iscTime.intersection(blankTimes[i], self.__modProcTime)
                            if tr is not None:
                                try:
                                    logger.debug("Processing Blank: %s TR=%s",
                                                 parmName, printTR(tr))
                                    self.__processBlankTime(mGrid, tr)
                                except:
                                    logger.exception('Failure to process grid blanking Parm=%s Time=%s',
                                                     parmName, printTR(tr))



                    # Returns tuple of (parmName, TR, #grids, #fails)
                    if len(inTimesProc):
                        totalTimeRange = (inTimesProc[0][0], inTimesProc[ -1][ -1] - 3600)
                    self._wec.flush()
                    retryAttempt = retries
                except:
                    retryAttempt = retryAttempt + 1
                    logger.exception("Error saving ISC data. Retrying ( %d / %d )", retryAttempt, retries)
                    time.sleep(1)

        return (pName, totalTimeRange, len(inTimesProc), numFailed)

    def __processIncomingGrid(self, lockName, remappedGrid, remappedHistory, mGrid, tr, inFillV):
        # calculate merge
        merge = iscTime.mergeTR(tr, self.__dbinv)

        # get the associated db grids, merge, and store
        for m in merge:
            # update cluster lock time to avoid time out
            ClusterLockUtils.updateLockTime("ISC Write Lock", lockName , System.currentTimeMillis())

            logger.debug("Merge: %s %s %s", printTR(m[0]),
            printTR(m[1]), m[2])
            gotGrid = self.__getDbGrid(m[0])

            if gotGrid is not None:
                destGrid = gotGrid[0]
                oldHist = gotGrid[1]
            else:
                destGrid = None
                oldHist = None

            # non-rate parms -- keep the data values the same
            if not self.__rateParm:

                # merge the grids, but only if the overlaps flag is set,
                # we use the minimum value for the fill value since we don't
                # support sparse populated grids
                if m[2] == 1 or (m[2] == 0 and m[0] is None):
                    if self.__replaceOnly:
                        mergedGrid = mGrid.mergeGrid(
                          (remappedGrid, remappedHistory), None)
                    else:
                        mergedGrid = mGrid.mergeGrid (
                          (remappedGrid, remappedHistory), (destGrid, oldHist))

                else:
                    mergedGrid = (destGrid, oldHist)

            # rate parms -- adjust data values based on times
            else:
                # merge the grids, but only if the overlaps flag is set,
                # we use the minimum value for the fill value since we don't
                # support sparse populated grids
                if m[2] == 1 or (m[2] == 0 and m[0] is None):
                    if self.__replaceOnly:
                        adjGrid = self.__adjustForTime(tr, m[1], remappedGrid,
                          inFillV)
                        mergedGrid = mGrid.mergeGrid(
                          (adjGrid, remappedHistory), None)
                    else:
                        adjGridIn = self.__adjustForTime(tr, m[1],
                          remappedGrid, inFillV)
                        adjGridDb = self.__adjustForTime(m[0], m[1], destGrid,
                          0.0)
                        mergedGrid = mGrid.mergeGrid(\
                          (adjGridIn, remappedHistory),
                          (adjGridDb, oldHist))

                else:
                    adjGrid = self.__adjustForTime(m[0], m[1], destGrid, 0.0)
                    mergedGrid = (adjGrid, oldHist)

            # store merged grid
            self.__storeGrid(m[1], mergedGrid)

    def __storeGrid(self, tr, grid):
        if grid is not None and grid[1] is not None and grid[0] is not None:
            logger.debug("Store: %s", printTR(tr))
            self._wec[tr] = grid

            if tr not in self.__dbinv:
                self.__dbinv = self._wec.keys()
        else:
            logger.debug("Erase: %s", printTR(tr))
            self._wec[tr] = None
            self.__dbinv = self._wec.keys()

    #---------------------------------------------------------------------
    # get db grid
    # Gets the needed database grid
    #  tr = desired grid, identified by time range
    # Returns tuple of (grid, history) (or None if unknown)
    #---------------------------------------------------------------------
    def __getDbGrid(self, tr):
        if tr is None:
            return None

        if self.__dbGrid is None or tr != self.__dbGrid[2]:
            self.__dbGrid = None
            grid = self._wec[tr]
            if grid is not None:
                destGrid, history = grid
                self.__dbGrid = (destGrid, history, tr)
            else:
                logger.error("Unable to access grid for %s for %s", printTR(tr), self.__parmName)
                return None

        return (self.__dbGrid[0], self.__dbGrid[1])

    #---------------------------------------------------------------------
    # calculate file start/end processing times
    # Returns (startTime, endTime) or None for processing
    # file= netcdf file descriptor object
    #---------------------------------------------------------------------
    def __getFileProcessingTimes(self, ncfile):

       # try:
        startTime = self.__decodeTimeString(ncfile.startProcTime)
        endTime = self.__decodeTimeString(ncfile.endProcTime)
        modProcTime = iscTime.intersection((startTime, endTime),
          self.__processTimePeriod)

        if modProcTime is None:
            logger.error("Skipping file due to non overlapping periods")
        return modProcTime

    def __decodeTimeString(self, timeStr):
        "Create an Integer time from a string: YYYYMMDD_HHMM"

        importError = True
        while importError:
            try:
                timeTuple = time.strptime(timeStr, "%Y%m%d_%H%M")
                importError = False
            except ImportError:
                importError = True
            except:
                logger.exception("%s is not a valid time string.  Use YYYYMMDD_HHMM", timeStr)
                raise Exception("Bad date format YYYYMMDD_HHMM")

        return calendar.timegm(timeTuple)

    #---------------------------------------------------------------------
    # get netcdf input variables
    # Gets the input variables from the netCDF file based on the parm name.
    # The netCDF file is opened on file.
    # Returns them as three tuples:  (dbParmName, ncvars, history[])
    # The ncvars is an array depending upon the data type:
    #   scalar [0], vector [0=mag,1=dir], wx [0=grid,1=key].
    # The history is a list of history strings.
    #---------------------------------------------------------------------
    def __getNetCDFInputVariables(self, ncfile, parmName):

        var = ncfile.variables[parmName]

        # make sure it is a weather element variable
        if not hasattr(var, "validTimes"):
            return None

        gridType = var.gridType

        # get the history info
        if gridType == 'SCALAR':
            pn = parmName + "_GridHistory"
        elif gridType == 'VECTOR':
            indx = parmName.find("_Mag_")
            if indx == -1:
                return None
            pn = parmName[0:indx + 1] + parmName[indx + 5:] + "_GridHistory"
        elif gridType == 'WEATHER':
            pn = parmName + "_GridHistory"
        elif gridType == 'DISCRETE':
            pn = parmName + "_GridHistory"
        hvar = ncfile.variables[pn]
        history = []
        for i in range(0, hvar.shape[0]):
            h = hvar[i].data.tostring().decode().rstrip('\0').strip()
            history.append(h.split('^'))

        # handle special cases for Vector and Wx, need to use a second
        # variable for wind and weather
        gridType = var.gridType

        if gridType == 'SCALAR':
            return (parmName, [var], history)

        elif gridType == 'VECTOR':
            indx = parmName.find("_Mag_")
            if indx != -1:
                dirparm = parmName[0:indx] + "_Dir_" + parmName[indx + 5:]
                varDir = ncfile.variables[dirparm]
                dbparmName = parmName[0:indx] + parmName[indx + 4:]
                return (dbparmName, [var, varDir], history)
            else:
                return None

        elif gridType == 'WEATHER':
            varKey = ncfile.variables[parmName + "_wxKeys"]
            return (parmName, [var, varKey], history)

        elif gridType == 'DISCRETE':
            varKey = ncfile.variables[parmName + "_keys"]
            return (parmName, [var, varKey], history)

        else:
            return None

    #-------------------------------------------------------------------------
    # Get Geographical Input Information
    # var is the netCDF variable
    #-------------------------------------------------------------------------
    def __getInputGeoInfo(self, var):

        # define minimum standard
        inProjData = {
            'latIntersect': 0.0,
            'latLonOrigin': (0.0, 0.0),
            'stdParallelTwo': 0.0,
            'stdParallelOne': 0.0,
            'lonCenter': 0.0,
            'lonOrigin': 0.0,
            'projectionID': 'hi'
            }

        # all projections have this information
        data = var.latLonLL
        inProjData['latLonLL'] = (float(data[0]), float(data[1]))
        data = var.latLonUR
        inProjData['latLonUR'] = (float(data[0]), float(data[1]))
        inProjData['projectionType'] = str(getattr(var, "projectionType"))
        data = var.gridPointLL
        inProjData['gridPointLL'] = (int(data[0]), int(data[1]))
        data = var.gridPointUR
        inProjData['gridPointUR'] = (int(data[0]), int(data[1]))

        # lambert conformal specific information
        if inProjData['projectionType'] == 'LAMBERT_CONFORMAL':
            data = var.latLonOrigin
            inProjData['latLonOrigin'] = (float(data[0]), float(data[1]))
            data = var.stdParallelOne
            inProjData['stdParallelOne'] = float(data)
            data = var.stdParallelTwo
            inProjData['stdParallelTwo'] = float(data)

        # polar stereographic
        elif inProjData['projectionType'] == 'POLAR_STEREOGRAPHIC':
            data = var.lonOrigin
            inProjData['lonOrigin'] = float(data)

        # mercator
        elif inProjData['projectionType'] == 'MERCATOR':
            data = var.lonCenter
            inProjData['lonCenter'] = float(data)

        # get specific grid sizes and domains
        data = var.gridSize
        inProjData['gridSize'] = (int(data[0]), int(data[1]))
        origin = var.domainOrigin
        extent = var.domainExtent
        inProjData['gridDomain'] = \
          ((float(origin[0]), float(origin[1])), (float(extent[0]), float(extent[1])))

        return inProjData

    #---------------------------------------------------------------------
    # determine fill value for input
    # vars = netCDF variables
    # Returns fill value to use
    # Note: file fill value may be different from processing fill value
    # since data may have to be multiplied and offset.
    #---------------------------------------------------------------------
    def __determineFillValue(self, var):
        gridType = var.gridType
        if gridType == 'SCALAR' or gridType == 'VECTOR':
            return -30000.0
        else:
            return -127

        #---------------------------------------------------------------------
    # compute the area mask
    # Returns areaMask to use based on the siteID. for ISC data,
    # the edit area is normally ISC_xxx where xxx is the WFO.
    #---------------------------------------------------------------------
    def __computeAreaMask(self):
        if self.__ignoreMask:
            domain = self.__dbwe.getGpi().getGridLoc()
            #maskDims = (int(domain.getNy()), int(domain.getNx()))
            #areaMask = numpy.ones(maskDims)
            #areaMask.setGloc(domain)


            areaMask = ReferenceData(domain, ReferenceID("full"), None, CoordinateType.GRID)
            areaMask.getGrid()
            areaMask.invert()

        elif self.__altMask is not None:
            try:
                areaMask = iscUtil.getEditArea(self.__altMask, self.__mysite)
                areaMask.setGloc(self.__dbwe.getGpi().getGridLoc())
            except:
                logger.exception("Unable to access edit mask [%s]",
                  self.__altMask)
                raise Exception("Unknown edit area mask [%s]" % self.__altMask)
        else:
            maskName = "ISC_" + self.__siteID
            try:
                areaMask = iscUtil.getEditArea(maskName, self.__mysite)
                areaMask.setGloc(self.__dbwe.getGpi().getGridLoc())
            except:
                logger.exception("Unable to access edit mask [%s]", maskName)
                raise Exception("Unknown edit area mask [%s]" % maskName)

        return areaMask

    #---------------------------------------------------------------------
    # split grids on processing time, so to preserve all grids that
    # overlap partially the processing time
    #  processTimePeriod = time range to process grids for splits
    #---------------------------------------------------------------------
    def __splitGridsOnProcBoundary(self, processTimePeriod):
        mergeInfo = iscTime.mergeTR(processTimePeriod, self.__dbinv)
        oldGrid = None
        oldTR = None
        for m in mergeInfo:
            if m[0] != m[1]: #split grid needed
                if m[0] != oldTR:
                    oldGrid = self.__getDbGrid(m[0])
                    oldTR = m[0]
                if oldGrid is not None:
                    if self.__rateParm:
                        adjGrid = self.__adjustForTime(m[0], m[1], oldGrid[0],
                          0.0)  #old db grids don't have missing value flags
                        self.__storeGrid(m[1], (adjGrid, oldGrid[1]))
                    else:
                        self.__storeGrid(m[1], oldGrid)
        self.__dbGrid = None

        #-------------------------------------------------------------------------
    # Get Incoming netCDF file grid valid times
    # netCDFfile, var is the netCDF variable
    #-------------------------------------------------------------------------
    def __getIncomingValidTimes(self, var):
        inTimesA = var.validTimes
        ntimes = len(inTimesA) // 2
        times = []
        for t in range(ntimes):
            times.append((inTimesA[t * 2], inTimesA[t * 2 + 1]))
        return times

        #---------------------------------------------------------------------
    # get grid from netCDF file.
    # gridType = type of grid: scalar, vector, weather
    # ncvars = netCDF variables
    # index = grid index
    # Returns grid as:
    #   scalar = grid
    #   vector = (magGrid, dirGrid)
    #   weather = (grid, key)
    # Note: the values in the grid may need to be changed if their is
    # a dataMultiplier or dataOffset attributes present. This will
    # also change the fill Value.
    #---------------------------------------------------------------------
    def __getGridFromNetCDF(self, gridType, ncvars, index):
        if gridType == 'SCALAR':
            grid = numpy.flipud(ncvars[0][index])
            return self.__scaleGrid(ncvars[0], grid)

        elif gridType == 'VECTOR':
            magGrid = numpy.flipud(ncvars[0][index])
            dirGrid = numpy.flipud(ncvars[1][index])
            return (self.__scaleGrid(ncvars[0], magGrid),
              self.__scaleGrid(ncvars[1], dirGrid))

        elif gridType == 'WEATHER':
            compKey = self.__compressKey(ncvars[1][index].data)
            grid = (numpy.flipud(ncvars[0][index]), compKey)

        elif gridType == 'DISCRETE':
            compKey = self.__compressKey(ncvars[1][index].data)
            grid = (numpy.flipud(ncvars[0][index]), compKey)

        return grid

    #---------------------------------------------------------------------
    # scaling changes for incoming grids
    # var = netCDF variable
    # grid = input grid
    # only should be called for SCALAR/VECTOR
    #---------------------------------------------------------------------
    def __scaleGrid(self, var, grid):
        #scaling changes
        inFillV = var.fillValue

        # any scaling needed?
        try:
            multiplier = var.dataMultiplier
            offset = var.dataOffset
        except:
            multiplier = None
            offset = None

        outFillV = self.__determineFillValue(var)
        if outFillV == inFillV and multiplier is None:
            return grid    # no changes needed
        outFillV = numpy.float32(outFillV)

        # get mask of valid points
        goodDataMask = numpy.not_equal(grid, inFillV)

        # apply the scaling, make a float32 grid
        if multiplier is not None:
            tempGrid = (grid.astype(numpy.float32) * multiplier) + offset
            grid = numpy.where(goodDataMask, tempGrid, outFillV)
        # no scaling needed, but the fill value needs changing
        else:
            grid = numpy.where(goodDataMask, grid, outFillV)

        return grid.astype(numpy.float32)

    def __remap(self, we, grid, inGeoDict, inFillV):
        gpi = we.getGpi()

        gridType = str(gpi.getGridType())

        gs = self.__decodeGridSlice(we, grid, TimeRange())

        pd = self.__decodeProj(inGeoDict)
        fill = inFillV

        origin = Coordinate(float(str(inGeoDict['gridDomain'][0][0])), float(str(inGeoDict['gridDomain'][0][1])))
        extent = Coordinate(float(str(inGeoDict['gridDomain'][1][0])), float(str(inGeoDict['gridDomain'][1][1])))

        gl = GridLocation("iscMosaic", pd, self.__getSize(gs), origin, extent, "GMT")
        mapper = RemapGrid(gl, gpi.getGridLoc())

        if gridType == 'SCALAR':
            newGrid = mapper.remap(gs.getScalarGrid(), fill, gpi.getMaxValue(), gpi.getMinValue(), fill)
            return newGrid.getNDArray()

        elif gridType == 'VECTOR':
            magGrid = Grid2DFloat(int(gs.getGridParmInfo().getGridLoc().getNx()), int(gs.getGridParmInfo().getGridLoc().getNy()))
            dirGrid = Grid2DFloat(int(gs.getGridParmInfo().getGridLoc().getNx()), int(gs.getGridParmInfo().getGridLoc().getNy()))
            mapper.remap(gs.getMagGrid(), gs.getDirGrid(), fill, gpi.getMaxValue(), gpi.getMinValue(), fill, magGrid, dirGrid)
            return (magGrid.getNDArray(), dirGrid.getNDArray())

        elif gridType == 'WEATHER':
            newGrid = mapper.remap(gs.getWeatherGrid(), fill, fill)
            return (newGrid.getNDArray(), grid[1])

        elif gridType == 'DISCRETE':
            newGrid = mapper.remap(gs.getDiscreteGrid(), fill, fill)
            return (newGrid.getNDArray(), grid[1])

    def __decodeGridSlice(self, we, value, tr, history=None):
        pid = we.getParmid()
        gpi = we.getGpi()

        gridType = str(gpi.getGridType())

        hist = ArrayList()

        if history is None:
            hist.add(GridDataHistory(OriginType.INITIALIZED, pid, tr))
        else:
            #FIXME
            for i in range(0, len(history)):
                hist.add(history[i])

        if gridType == 'SCALAR':
            data = Grid2DFloat.createGrid(value.shape[1], value.shape[0], value)
            gridSlice = ScalarGridSlice(tr, gpi, hist, data)
        elif gridType == 'VECTOR':

            magVal = value[0]
            dirVal = value[1]

            magGrid = Grid2DFloat.createGrid(magVal.shape[1], magVal.shape[0], magVal)
            dirGrid = Grid2DFloat.createGrid(dirVal.shape[1], dirVal.shape[0], dirVal)
            gridSlice = VectorGridSlice(tr, gpi, hist, magGrid, dirGrid)
        elif gridType == 'WEATHER':
            data = Grid2DByte.createGrid(value[0].shape[1], value[0].shape[0], value[0])
            keyList = ArrayList()
            for key in value[1]:
                keyList.add(WeatherKey())
            gridSlice = WeatherGridSlice()
            gridSlice.setValidTime(tr)
            gridSlice.setGridParmInfo(gpi)
            gridSlice.setGridDataHistory(hist)
            gridSlice.setWeatherGrid(data)
            gridSlice.setKeys(keyList)
        elif gridType == 'DISCRETE':
            data = Grid2DByte.createGrid(value[0].shape[1], value[0].shape[0], value[0])
            keyList = ArrayList()
            for key in value[1]:
                keyList.add(DiscreteKey())
            gridSlice = DiscreteGridSlice()
            gridSlice.setValidTime(tr)
            gridSlice.setGridParmInfo(gpi)
            gridSlice.setGridDataHistory(hist)
            gridSlice.setDiscreteGrid(data)
            gridSlice.setKeys(keyList)
        return gridSlice

    def __decodeProj(self, pdata):

        pid = "GrandUnifiedRemappingProj"
        projType = ProjectionType.valueOf(pdata["projectionType"])
        llLL = Coordinate(pdata["latLonLL"][0], pdata["latLonLL"][1])
        llUR = Coordinate(pdata["latLonUR"][0], pdata["latLonUR"][1])
        llo = Coordinate(pdata["latLonOrigin"][0], pdata["latLonOrigin"][1])
        sp1 = pdata["stdParallelOne"]
        sp2 = pdata["stdParallelTwo"]
        gpll = Point(pdata["gridPointLL"][0], pdata["gridPointLL"][1])
        gpur = Point(pdata["gridPointUR"][0], pdata["gridPointUR"][1])
        lati = pdata["latIntersect"]
        lonc = pdata["lonCenter"]
        lono = pdata["lonOrigin"]

        return ProjectionData(pid, projType, llLL, llUR, llo, sp1, sp2, gpll, gpur, lati, lonc, lono)

    def __getSize(self, gs):
        gridType = str(gs.getGridParmInfo().getGridType())

        if gridType == "SCALAR" or gridType == "VECTOR":
            return Point(gs.getScalarGrid().getXdim(), gs.getScalarGrid().getYdim())
        elif gridType == "WEATHER":
            return Point(gs.getWeatherGrid().getXdim(), gs.getWeatherGrid().getYdim())
        elif gridType == "DISCRETE":
            return Point(gs.getDiscreteGrid().getXdim(), gs.getDiscreteGrid().getYdim())
        else:
            return None

    #---------------------------------------------------------------------
    # compress key (weather or discrete)
    # eliminates the "blank" keys that may exist in the input key
    # from the netCDF file.
    #---------------------------------------------------------------------
    def __compressKey(self, keys):
        return [s for s in (k.tostring().decode().strip('\0').strip() for k in keys) if s]

    #---------------------------------------------------------------------
    # adjust for time
    # Adjusts a rate dependent grid based on time durations. No processing
    # occurs if the grid, or the times are not valid.
    # Returns the new grid.
    #  trOrg = original grid time range
    #  trNew = new grid time range
    #  grid = old grid (NOT HISTORY)
    #  fillValue = grid fill value
    #   where the grid is a scalar, vector pair, or weather grid/key pair)
    # Returns new grid, adjusted by duration changes.
    #---------------------------------------------------------------------
    def __adjustForTime(self, trOrg, trNew, grid, fillValue):
        if not self.__rateParm or grid is None:
            return grid
        newDuration = float(trNew[1] - trNew[0])
        orgDuration = float(trOrg[1] - trOrg[0])
        durationRatio = newDuration / orgDuration

        dataType = str(self.__dbwe.getGpi().getGridType())
        if dataType == 'SCALAR':
            fillMask = numpy.equal(grid, fillValue)
            return numpy.where(fillMask, grid, grid * durationRatio)
        elif dataType == 'VECTOR':
            fillMask = numpy.equal(grid[0], fillValue)
            newMagGrid = numpy.where(fillMask, grid[0], (grid[0] * durationRatio))
            return (newMagGrid, grid[1])
        else:
            return grid

    def __calcBlankingTimes(self, inTimes):
        out = []
        if len(inTimes) == 0:
            out.append((0, 2 ** 30 - 1 + 2 ** 30))
        else:
            for t in range(len(inTimes)):
                if t == 0 and inTimes[t][0] != 0:
                    out.append((0, inTimes[t][0]))
                elif t != 0 :
                    tr = (inTimes[t - 1][1], inTimes[t][0])
                    if tr[0] != tr[1]:
                        out.append(tr)

            endIndex = len(inTimes) - 1
            tr = (inTimes[endIndex][1], 2 ** 30 - 1 + 2 ** 30)
            if tr[0] != tr[1]:
                out.append(tr)

        # now limit to the modProcTime
        outLimit = []
        for t in out:
            inter = iscTime.intersection(t, self.__modProcTime)
            if inter is not None:
                outLimit.append(inter)

        return outLimit

    def __processBlankTime(self, mGrid, tr):

        # calculate the merge
        merge = iscTime.mergeTR(tr, self.__dbinv)

        for m in merge:
            # blank the grids, but only if the overlaps flag is clear,
            if m[0] is not None and m[2] == 1:
                if self.__siteInDbGrid(m[0]):
                    try:
                        (destGrid, oldHist) = self.__getDbGrid(m[0])
                    except:
                        destGrid = None
                        oldHist = None

                    if self.__rateParm:
                        adjGrid = self.__adjustForTime(m[0], m[1], destGrid,
                          0.0) #old db grids don't have missing data flags
                        mergedGrid = mGrid.mergeGrid(None, \
                          (adjGrid, oldHist))
                    else:
                        mergedGrid = mGrid.mergeGrid(None, (destGrid, oldHist))
                    self.__storeGrid(m[1], mergedGrid)

    def __siteInDbGrid(self, tr):
        if tr is None:
            return None

        grid, history = self._wec[tr]

        if history:
            for h in history:
                if self.__siteID + "_GRID" in h:
                    return True

        return False

    #---------------------------------------------------------------------
    # validateAdjustDiscreteKeys()
    # grid = incoming grid (grid, key)
    # parmName = parm name
    # tr = time range of grid
    #
    # returns 'adjusted grid' with a potentially modified key.  The key
    # is guaranteed to be compatible with the current ifpServer definition.
    #---------------------------------------------------------------------
    def __validateAdjustDiscreteKeys(self, grid, parmName, tr):

        (g, key) = grid   #separate out for processing

        if parmName.find("_") == -1:
            parmName = parmName + "_SFC"  #need parmName_level for dict

        # set up error message
        smsg = "Adjusting DiscreteKey for Compatibility: " + parmName + \
          ' tr=' + printTR(tr)

        # get the list of discrete keys for this parameter that are allowed
        dd = self.__disDef.keys(parmName)
        if dd.size() == 0:
            logger.error("Unable to validate keys for %s - no def in DiscreteDefinition",
              parmName)
            return grid

        #now go through the incoming grid's keys and validate each one
        for idx in range(len(key)):  #each index of the discrete key
            keyentry = key[idx]    #each discrete key entry
            oldEntry = keyentry  #save an unchanged copy for reporting
            changedReasons = []

            #overlap check
            ci = keyentry.find("^")
            if ci != -1 and not self.__disDef.overlaps(parmName):
                keyentry = keyentry[0:ci]   #reset it to only 1st portion
                changedReasons.append("No Overlap Allowed")

            eachKey = keyentry.split("^")
            for idx1 in range(len(eachKey)):
                ke = eachKey[idx1]   #each discretesubkey in a discrete key
                ai = ke.find(":")  #look for aux data
                if ai != -1:
                    aux = ke[ai + 1:]
                    base = ke[0:ai]

                    #too long of aux data check
                    if len(aux) > self.__disDef.auxDataLength(parmName):
                        ke = base   #eliminate the aux data
                        changedReasons.append("AuxData Length Exceeded")

                else:
                    aux = None
                    base = ke   #with no aux data

                #valid key specified check
                validKey = False
                for i in range(dd.size()):
                    if dd.get(i).getSymbol() == base:
                        validKey = True
                        break
                if not validKey:
                    if aux:
                        ke = dd.get(0).getSymbol() + ":" + aux
                    else:
                        ke = dd.get(0).getSymbol()  #use 1st one

                    changedReasons.append("Unknown Key")

                eachKey[idx1] = ke  #store back into list

            keyentry = "^".join(eachKey)  #join back to string
            if len(changedReasons):
                logger.error("%s from [%s] to [%s] (%s)",
                             smsg, oldEntry, keyentry, ",".join(changedReasons))

                msg = "%s %s %s [%s] -> [%s] (%s)" % \
                      (self.__siteID, parmName, printShortTR(tr), oldEntry, keyentry, ",".join(changedReasons))
                self.__adjDataMsg.append(msg)

            key[idx] = keyentry   #store back into list


        return (g, key)

    #---------------------------------------------------------------------
    # validateAdjustWeatherKeys()
    # grid = incoming grid (grid, key)
    # parmName = parm name
    # tr = time range of grid
    #
    # returns 'adjusted grid'
    #---------------------------------------------------------------------
    def __validateAdjustWeatherKeys(self, grid, parmName, tr):
        (g, key) = grid   #separate out for processing

        if parmName.find("_") == -1:
            parmName = parmName + "_SFC"  #need parmName_level for output

        # set up error message
        smsg = "Adjusting WeatherKey for Compatibility: " + parmName + \
          ' tr=' + printTR(tr)

        #now go through the incoming grid's keys and validate each one
        for idx in range(len(key)):  #each index of the weather key
            changedReasons = []
            keyentry = key[idx]    #each weather key entry
            oldEntry = keyentry  #save an unchanged copy for reporting
            ikeys = keyentry.split("^")  #split into individual subkeys
            for idx1 in range(len(ikeys)):
                cov, typ, inten, vis, attrs = ikeys[idx1].split(":")

                # check the visibility
                visibilities = self.__wxDef.getWeatherVisibilities()
                validViz = False
                for i in range(visibilities.size()):
                    if visibilities.get(i).getSymbol() == vis:
                        validViz = True
                        break
                if not validViz:
                    vis = visibilities.get(0).getSymbol() #assign 1st one
                    changedReasons.append("Unknown Visibility")

                # check the type
                types = self.__wxDef.getWeatherTypes()
                validType = False
                for i in range(types.size()):
                    if types.get(i).getSymbol() == typ:
                        validType = True
                        break
                if not validType:
                    oldEntry = keyentry
                    typ = "<NoWx>"   #no choice but to set to no weather
                    de = self.__wxDef.typeIndex(typ)
                    cov = self.__wxDef.coverageSymbol(de, 0)
                    inten = self.__wxDef.intensitySymbol(de, 0)
                    vis = self.__wxDef.visibilitySymbol(0)
                    attrs = ""
                    changedReasons.append("Unknown Weather Type")

                # type is known, validate other components
                else:
                    de = self.__wxDef.typeIndex(typ)

                    # validate coverage
                    if self.__wxDef.coverageIndex(typ, cov) == -1:
                        cov = self.__wxDef.coverageSymbol(de, 0)
                        changedReasons.append("Unknown Coverage")

                    # validate intensity
                    if self.__wxDef.intensityIndex(typ, inten) == -1:
                        inten = self.__wxDef.intensitySymbol(de, 0)
                        changedReasons.append("Unknown Intensity")

                    # validate attributes
                    if len(attrs):
                         atts = attrs.split(",")  #get individual attributes
                         #determine the attributes that are valid
                         keepAttr = []
                         for a in atts:
                             if self.__wxDef.attributeIndex(typ, a) != -1:
                                 keepAttr.append(a)
                         if len(atts) != len(keepAttr):
                            attrs = ",".join(keepAttr)
                            changedReasons.append("Unknown Attribute")

                # update record
                ikeys[idx1] = cov + ":" + typ + ":" + inten + ":" + vis + ":" + attrs
                keyentry = "^".join(ikeys)  #assemble subkeys
                key[idx] = keyentry   #put back to original format

            # report any changes
            if len(changedReasons):
                logger.error("%s from [%s] to [%s] (%s)",
                             smsg, oldEntry, keyentry, ",".join(changedReasons))

                msg = "%s %s %s [%s] -> [%s] (%s)" % \
                      (self.__siteID, parmName, printShortTR(tr), oldEntry, keyentry, ",".join(changedReasons))

                self.__adjDataMsg.append(msg)

        return (g, key)

    #---------------------------------------------------------------------
    # erase all grids from the given weather element over the
    # processTimePeriod procesTimePeriod = time range to remove grids
    #---------------------------------------------------------------------
    def __eraseAllGrids(self, processTimePeriod):
        self.__storeGrid(processTimePeriod, None)
        self.__dbGrid = None


def convertList(unknownList):
    retVal = unknownList
    if hasattr(unknownList, 'java_name'):
        retVal = JUtil.javaObjToPyVal(unknownList)
    return retVal

def prepareJob(args, job):
    mosaic = IscMosaic(args)
    job = job.createAssociatedJob(args)
    mosaic.prepare(job)

# This is used for jobs created in response to an ExecuteIscMosaicRequest
def prepareMosaicRequest(siteID, userID, databaseID, parmsToProcess, blankOtherPeriods,
        startTime, endTime, altMask, replaceOnly, eraseFirst,
        announce, renameWE, iscSends, inFiles, ignoreMask,
        adjustTranslate, deleteInput, parmsToIgnore, gridDelay, logFileName,
        additionalRoutingSiteID,
        job):
    import serverConfig

    additionalISCRouting = []
    if additionalRoutingSiteID and serverConfig.AdditionalISCRouting:
        additionalISCRouting = serverConfig.AdditionalISCRouting

    # convert Java types to python and send to IscMosaic for execution
    parmsToProcess = convertList(parmsToProcess)
    inFiles = convertList(inFiles)
    parmsToIgnore = convertList(parmsToIgnore)
    for inputFile in inFiles:
        argv = {"siteID": siteID,
                "userID": userID,
                "databaseID": databaseID,
                "parmsToProcess": parmsToProcess,
                "blankOtherPeriods": bool(blankOtherPeriods),
                "startTime": startTime,
                "endTime": endTime,
                "altMask": altMask,
                "replaceOnly": bool(replaceOnly),
                "eraseFirst": bool(eraseFirst),
                "announce": announce,
                "renameWE": bool(renameWE),
                "iscSends": bool(iscSends),
                "inFile": inputFile,
                "ignoreMask": bool(ignoreMask),
                "adjustTranslate": bool(adjustTranslate),
                "parmsToIgnore": parmsToIgnore,
                "gridDelay": float(gridDelay),
                "logFileName": logFileName}
        prepareJob(argv, job)

        for entry in additionalISCRouting:
            (parms, dbName, editAreaPrefix) = entry
            parmNameList = [parm[0] + "_SFC" for parm in parms]
            argv = dict(argv)
            argv['parmsToProcess'] = parmNameList
            argv['databaseID'] = siteID + "_GRID__" + dbName + "_00000000_0000"
            argv['altMask'] = editAreaPrefix + additionalRoutingSiteID
            prepareJob(argv, job)

lastJob = None
lastIscMosaic = None

# Process one parm for the given job and parm lockName.  Try to reuse the
# IscMosaic object from the previous call if the job is the same.
def processParm(job, lockName):
    global lastIscMosaic, lastJob

    if not job.equals(lastJob):
        if lastJob is not None:
            lastJob = None
            lastIscMosaic.dispose()
            lastIscMosaic = None
        args = JUtil.javaObjToPyVal(job.getArgs())
        lastIscMosaic = IscMosaic(args)
        lastJob = job
    lastIscMosaic.processParm(job, lockName)

def cleanUpJob():
    global lastIscMosaic, lastJob
    try:
        if lastIscMosaic is not None:
            lastIscMosaic.dispose()
    finally:
        lastJob = None
        lastIscMosaic = None
