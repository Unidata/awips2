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


import os, stat, time, string, bisect, getopt, sys, traceback
import LogStream, iscTime, iscUtil, mergeGrid
#import pupynere as NetCDF
try:
    # dev environment
    from Scientific.IO import NetCDF
except:
    # runtime we don't have the whole scientific package
    import NetCDF
import numpy
import JUtil

from java.util import ArrayList
from java.util import LinkedHashMap
from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DFloat
from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DByte
from com.raytheon.uf.common.time import TimeRange
from com.vividsolutions.jts.geom import Coordinate
from java.awt import Point

from com.raytheon.edex.plugin.gfe.server import GridParmManager
from com.raytheon.edex.plugin.gfe.config import IFPServerConfigManager
from com.raytheon.edex.plugin.gfe.smartinit import IFPDB
from com.raytheon.uf.common.dataplugin.gfe import GridDataHistory
from com.raytheon.uf.common.dataplugin.gfe import RemapGrid
from com.raytheon.uf.common.dataplugin.gfe import GridDataHistory_OriginType as OriginType
from com.raytheon.uf.common.dataplugin.gfe.config import ProjectionData
from com.raytheon.uf.common.dataplugin.gfe.config import ProjectionData_ProjectionType as ProjectionType
from com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID
from com.raytheon.uf.common.dataplugin.gfe.db.objects import GridLocation 
from com.raytheon.uf.common.dataplugin.gfe.slice import DiscreteGridSlice
from com.raytheon.uf.common.dataplugin.gfe.slice import ScalarGridSlice
from com.raytheon.uf.common.dataplugin.gfe.slice import VectorGridSlice
from com.raytheon.uf.common.dataplugin.gfe.slice import WeatherGridSlice
from com.raytheon.uf.common.dataplugin.gfe.discrete import DiscreteKey
from com.raytheon.uf.common.dataplugin.gfe.weather import WeatherKey
from com.raytheon.uf.common.dataplugin.gfe.server.notify import UserMessageNotification
from com.raytheon.edex.plugin.gfe.util import SendNotifications
from com.raytheon.uf.common.status import UFStatus_Priority as Priority
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData_CoordinateType as CoordinateType
from com.raytheon.uf.edex.database.cluster import ClusterLockUtils
from com.raytheon.uf.edex.database.cluster import ClusterTask

#
# Port of iscMosaic.py
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/06/09        1995          bphillip       Initial Creation.
#    01/17/13        15588         jdynina        Fixed Publish history removal
#    03/12/13        1759          dgilling       Remove unnecessary command line
#                                                 processing.
#    04/24/13        1941          dgilling       Re-port WECache to match A1.
# 
# 


BATCH_WRITE_COUNT = 10
BATCH_DELAY = 0.0

ISC_USER="isc"

class WECache(object): 
    def __init__(self, we, tr=None):
        self._we = we
        self._inv = {}
        self._invCache = None
        
        javaInv = self._we.getKeys()
        pyInv = []
        for i in xrange(javaInv.size()):
            pyInv.append(iscUtil.transformTime(javaInv.get(i)))

        # Dont get grids outside of the passed in timerange.
        if tr:
            tokill = []
            for i, t in enumerate(pyInv):
                if not self.overlaps(tr, t):
                    tokill.append(i)
            tokill.reverse()
            for i in tokill:
                del pyInv[i]

        javaTRs = ArrayList()
        for tr in pyInv:
            javaTRs.add(iscUtil.toJavaTimeRange(tr))
        gridsAndHist = self._we.get(javaTRs, True)
        for idx, tr in enumerate(pyInv):
            pair = gridsAndHist.get(idx)
            g = self.__encodeGridSlice(pair.getFirst())
            h = self.__encodeGridHistory(pair.getSecond())
            self._inv[tr] = (g, h)

    def keys(self):
        if not self._invCache:
            self._invCache = tuple(sorted(self._inv.keys(), key=lambda t: t[0]))
        return self._invCache

    def __getitem__(self, key):
        try:
            return self._inv[key]
        except KeyError:
            grid = self._we.getItem(iscUtil.toJavaTimeRange(key))
            pyGrid = self.__encodeGridSlice(grid)
            history = grid.getGridDataHistory()
            pyHist = self.__encodeGridHistory(history)
            return (pyGrid, pyHist)

    def __setitem__(self, tr, value):
        if value is None:
            grid = hist = None
        else:
            grid, hist = value 
        
        # Remove any overlapping grids
        tokill = []
        for itr in self._inv:
            if self.overlaps(tr, itr):
                tokill.append(itr)
        for i in tokill:
            del self._inv[i]
            self._invCache = None
        
        # Now add the new grid if it exists
        if grid is not None:
            self._inv[tr] = (grid, hist)
            self._invCache = None            

    def flush(self):
        """Actually writes the contents of the WECache to HDF5/DB"""
        # get cache inventory in time range order
        # we want to write to disk in contiguous time range blocks so we only
        # overwrite what we have full sets of grids for.        
        inv = list(self.keys())
        # Don't believe the grid slices need to be in time order when saving
        # but leaving them that way just in case.
        gridsToSave = LinkedHashMap()
        while inv:
            # retrieve the next BATCH of grids to persist
            i = inv[:BATCH_WRITE_COUNT]
            # pre-compute the replacement TR for the save requests generated by
            # IFPWE.put().
            # since the inventory is in order it's the start time of the 
            # first TR and the end time of the last TR.
            gridSaveTR = iscUtil.toJavaTimeRange((i[0][0], i[-1][1]))
            for tr in i:
                javaTR = iscUtil.toJavaTimeRange(tr)
                pyGrid, pyHist = self._inv[tr]
                javaHist = self.__buildJavaGridHistory(pyHist) 
                javaGrid = self.__buildJavaGridSlice(javaTR, pyGrid, javaHist)
                gridsToSave.put(javaTR, javaGrid)
            self._we.put(gridsToSave, gridSaveTR)
            # delete the persisted items from the cache and our copy of the
            # inventory
            gridsToSave.clear()
            for tr in i:
                del self._inv[tr]
            self._invCache = None
            inv = inv[BATCH_WRITE_COUNT:]
            time.sleep(BATCH_DELAY)
            

    def overlaps(self, tr1, tr2):
        if (tr1[0] >= tr2[0] and tr1[0] < tr2[1]) or \
           (tr2[0] >= tr1[0] and tr2[0] < tr1[1]): 
            return True
        return False
    
    def __encodeGridSlice(self, grid):
        gridType = self._we.getGridType()
        if gridType == "SCALAR":
            return grid.__numpy__[0]
        elif gridType == "VECTOR":
            vecGrids = grid.__numpy__
            return (vecGrids[0], vecGrids[1])
        elif gridType == "WEATHER":
            keys = grid.getKeys()
            keyList = []
            for theKey in keys:
                keyList.append(theKey.toString())
            return (grid.__numpy__[0], keyList)
        elif gridType =="DISCRETE":
            keys = grid.getKey()
            keyList = []
            for theKey in keys:
                keyList.append(theKey.toString())
            return (grid.__numpy__[0], keyList)
    
    def __encodeGridHistory(self, histories):
        retVal = []
        for i in xrange(histories.size()):
            retVal.append(histories.get(i).getCodedString())
        return tuple(retVal)
    
    def __buildJavaGridSlice(self, tr, grid, history):
        gridType = self._we.getGridType()
        if gridType == "SCALAR":
            return self._we.buildScalarSlice(tr, grid.astype(numpy.float32), history)
        elif gridType == "VECTOR":
            return self._we.buildVectorSlice(tr, grid[0].astype(numpy.float32), grid[1].astype(numpy.float32), history)
        elif gridType == "WEATHER":
            return self._we.buildWeatherSlice(tr, grid[0].astype(numpy.byte), str(grid[1]), history)
        elif gridType == "DISCRETE":
            return self._we.buildDiscreteSlice(tr, grid[0].astype(numpy.byte), str(grid[1]), history)
    
    def __buildJavaGridHistory(self, histories):
        retVal = ArrayList()
        blankPubTime = "Fcst" in self._we.getParmid().getDbId().toString()
        for histEntry in histories:
            javaHist = GridDataHistory(histEntry)
            # strip out publish time to allow for publishing correctly
            # when merging Fcst out of A1 
            if blankPubTime:
                javaHist.setPublishTime(None)
            retVal.add(javaHist)
        return retVal


class IscMosaic:
        
        ## Logging methods ##
    def __initLogger(self):
        import logging, siteConfig
        self.__logger=iscUtil.getLogger("iscMosaic",self.__logFile)
        
    def __init__(self, args):
        self.__mysite = args['siteID']
        self.__userID = args['userID']
        self.__db = None    # ifpServer database object
        self.__dbGrid = None
        self.__parmsToProcess = args['parmsToProcess']
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
        self.__inFiles = args['inFiles']
        self.__ignoreMask = args['ignoreMask']
        self.__adjustTranslate = args['adjustTranslate']
        self.__deleteInput = args['deleteInput']
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

        self.__initLogger()

        
    def logEvent(self,*msg):
        self.__logger.info(iscUtil.tupleToString(*msg))
    
    def logProblem(self,*msg):
        self.__logger.error(iscUtil.tupleToString(*msg))
        
    def logException(self,*msg):
        self.__logger.exception(iscUtil.tupleToString(*msg))    
    
    def logVerbose(self,*msg):
        self.__logger.debug(iscUtil.tupleToString(*msg))
        
    def logDebug(self,*msg):
        self.logVerbose(iscUtil.tupleToString(*msg))
    
    def execute(self):
        self.logEvent("iscMosaic Starting")
    
        # get the WxDefinition and DiscreteDefinition
        config = IFPServerConfigManager.getServerConfig(self.__mysite)
        self.__wxDef = config.getWxDefinition()
        self.__disDef = config.getDiscreteDefinition()
        
        self.__db = IFPDB(self.__databaseID)

        # parms in database
        parmsInDb = self.__db.getKeys()
        if len(self.__parmsToProcess) == 0: 
            for i in range(0, parmsInDb.size()): 
                self.__parmsToProcess.append(parmsInDb.get(i).toString())

        # get office type information 
        self.__myOfficeType = IFPServerConfigManager.getServerConfig(DatabaseID(self.__databaseID).getSiteId()).officeType()
        
        #process each input file
        for file in self.__inFiles:
            self.__areaMask = None
            self.__processInputFile(file)
            
            if self.__deleteInput:
                os.remove(file)

        self.logEvent("iscMosaic Finished")
        
    def __processInputFile(self, filename):
        
        a = os.times()
        cpu0 = a[0] + a[1]
        start = a[4]

        self.logEvent("Processing file=", filename)
        fsize = os.stat(filename)[stat.ST_SIZE]
        self.logEvent("Input file size: ", fsize)
        
        gzipFile = None
        unzippedFile = None
        try:
            import gzip
            gzipFile = gzip.open(filename, 'rb')
            unzippedFile = open(filename + ".unzipped", 'w')
            unzippedFile.write(gzipFile.read())
            unzippedFile.close()
            gzipFile.close()
            os.rename(unzippedFile.name, gzipFile.filename)
        except:
            # Not gzipped
            if gzipFile is not None:
                gzipFile.close()
            if unzippedFile is not None:
                unzippedFile.close()
                os.remove(unzippedFile.name)
        
        a = os.times()
        cpu = a[0] + a[1]
        stop1 = a[4]

        if hasattr(NetCDF, "netcdf_file"):
            # use this for pupynere
            
            # TODO: Remove False flag passed to constructor to resolve memory 
            # allocation error found in #7788. If AWIPS2 ever moves to 64-bit
            # we'll probably have enough address space to allow the file to be
            # memory-mapped.
            file = NetCDF.netcdf_file(filename, "r", False) 
        else:
            # use this for ScientificIO.NetCDF
            file = NetCDF.NetCDFFile(filename, "r")  
        
        # check version
        fileV = getattr(file, 'fileFormatVersion')
        if fileV != "20010816" and fileV != "20030117":
            self.logProblem("Incompatible file format found")
            raise Exception, "Incompatible file format"
        
        # get creation time
        self.__creTime = getattr(file, 'creationTime')
        creTimeString = time.asctime(time.gmtime(self.__creTime))
        self.logEvent("CreationTime:" , creTimeString)
        
        # get file start/end processing times
        self.__modProcTime = self.__getFileProcessingTimes(file)
        if self.__modProcTime is None:
            return None
        self.logEvent("Process TR: ", self.__printTR(self.__modProcTime))
        
                # prepare for the notification message
        totalTimeRange = None
        totalGrids = 0
        totalFails = 0
        pParms = []
        self.__adjDataMsg = []
         
        # process each parm in the netCDF file
        # Only use one area mask for all parms.  This will break
        # if we ever use parms with differing dims in a database.
        areaMask = None
        inFileVars = file.variables.keys()   #parm names
        
        for parm in inFileVars:
            tup = self.__getNetCDFInputVariables(file, parm) 
            
            if tup is None:
                    continue
            else:
                parmName = tup[0]
                vars = tup[1]
                remapHistory = tup[2] 
                
            # rename weather element
            if self.__renameWE:
                siteID = str(getattr(vars[0], "siteID"))
                incomingOfficeType = IFPServerConfigManager.getServerConfig(self.__mysite).getOfficeType(siteID)
                if incomingOfficeType != self.__myOfficeType:
                    idx = parmName.rfind("_")
                    parmName = parmName[0:idx] + incomingOfficeType + \
                      parmName[idx:]
                    self.logEvent("Renamed to: " + parmName + \
                      " data from " + siteID)
                    
            # ignore this parm?
            if parmName in self.__parmsToIgnore:
                self.logEvent("Ignoring ", parmName)
                continue
            
            # match in ifp database? 
            if not parmName in self.__parmsToProcess and \
              len(self.__parmsToProcess) != 0:
                self.logEvent("Skipping", parmName)
                continue
            
            (pName, pTR, nGrids, nFail) = self.__processParm(parmName, vars, remapHistory, filename)
            
            # save info for the notification message 
            pParms.append(pName)
            if pTR is not None:
                if totalTimeRange is None:
                    totalTimeRange = pTR
                else:
                    if totalTimeRange[0] > pTR[0]:
                        totalTimeRange = (pTR[0], totalTimeRange[1])
                    if totalTimeRange[1] < pTR[1]:
                        totalTimeRange = (totalTimeRange[0], pTR[1])
            totalGrids = totalGrids + nGrids
            totalFails = totalFails + nFail  
        
        file.close()
        
        #announce storage
        if len(self.__announce) and totalGrids > 0:
            msg = self.__announce + self.__siteID + ' ' + `pParms` + ' ' + self.__printShortTR(totalTimeRange) + ' #Grids=' + `totalGrids`
            if totalFails:
                msg = msg + '[' + `totalFails` + ' FAILED]' 
                notification = UserMessageNotification(msg, Priority.CRITICAL, "ISC", self.__mysite) 
            else: 
                notification = UserMessageNotification(msg, Priority.EVENTA, "ISC", self.__mysite) 
            self.logEvent(msg)
            SendNotifications.send(notification)
        
        # announce "modified/adjusted" data
        if len(self.__announce) and len(self.__adjDataMsg):
            msg = "ISC Data Modified:\n" + "\n".join(self.__adjDataMsg)
            notification = UserMessageNotification(msg, Priority.EVENTA, "ISC", self.__mysite)
            SendNotifications.send(notification)
            
        a = os.times()
        cpugz = a[0] + a[1]
        stop = a[4]
        self.logEvent("Elapsed/CPU time: ", 
          "%-.2f" % (stop1 - start), "/", "%-.2f" % (cpu - cpu0), "decompress,", 
          "%-.2f" % (stop - stop1),  "/", "%-.2f" % (cpugz - cpu), "processing,",
          "%-.2f" % (stop - start), "/", "%-.2f" % (cpugz - cpu0), "total")
        
    def __processParm(self, parmName, vars, history, filename):
        
        retries = 5
        retryAttempt = 0
        pName = string.replace(parmName, "_SFC", "")
        totalTimeRange = None
        inTimesProc = []
        numFailed = 0
            
        while retryAttempt != retries:
            self.logDebug("iscMosaic: Attempting to acquire cluster lock for:",parmName)
            startTime = time.time()
            clusterLock = ClusterLockUtils.lock("ISC Write Lock",parmName , 120000, True)
            elapsedTime = (time.time() - startTime)*1000
            self.logDebug("iscMosaic: Request for",parmName+" took",elapsedTime,"ms")
            if str(clusterLock.getLockState()) == "SUCCESSFUL":
                self.logDebug("iscMosaic: Successfully acquired cluster lock for:",parmName)
                try:
                    # open up the ifpServer weather element
                    self.__dbwe = self.__db.getItem(parmName,ISC_USER)
                    self._wec = WECache(self.__dbwe, self.__modProcTime)
                    self.__rateParm = self.__dbwe.getGpi().isRateParm()
                    self.__parmName = parmName
                    
                    # get general info for the parm from the input file and output db
                    inGeoDict = self.__getInputGeoInfo(vars[0])
                    inFillV = self.__determineFillValue(vars[0])
            
                    gridType = getattr(vars[0], "gridType")
                    minV = self.__dbwe.getGpi().getMinValue()
                    # compute the site mask 
                    self.__siteID = str(getattr(vars[0], "siteID"))

                    if self.__areaMask is None:
                        self.__areaMask = self.__computeAreaMask().getGrid().__numpy__[0]

                    # create the mergeGrid class
                    mGrid = mergeGrid.MergeGrid(self.__creTime, self.__siteID, inFillV,
                      minV, self.__areaMask, gridType, self.__dbwe.getDiscreteKeys())
                    
                    # erase all existing grids first?
                    #self.__dbinv = self.__dbwe.keys()
                    self.__dbinv = self._wec.keys()
                    if self.__eraseFirst:
                        self.__eraseAllGrids(self.__modProcTime)
                    else:
                        try:
                            self.__splitGridsOnProcBoundary(self.__modProcTime)
                        except:
                            self.logProblem('Failure to splitGridsOnProcBoundary ',
                              ' Parm=', parmName, ' Time=',
                              self.__printTR(self.__modProcTime), traceback.format_exc())
                    
                    # process each incoming grid
                    inTimes = self.__getIncomingValidTimes(vars[0])
                    inTimesProc = []
                    numFailed = 0
                    self.logEvent("Processing ", parmName, " #Grids=",
                      len(inTimes), " Site=", self.__siteID)
                    
                    # process incoming grids
                    for i in xrange(len(inTimes)):
                        # Put in a delay so we don't hammer the server so hard.
                        if self.__gridDelay > 0.0:
                            time.sleep(self.__gridDelay)
                        tr = iscTime.intersection(inTimes[i], self.__modProcTime)
                        if tr is not None: 
                            inTimesProc.append(tr) 
                            try:
                                self.logDebug("Processing Grid: ", parmName, \
                                " TR=", self.__printTR(tr))
            
                                # get the grid and remap it
                                grid = self.__getGridFromNetCDF(gridType, vars, i)
                                
                                # if WEATHER or DISCRETE, then validate and adjust keys
                                if self.__adjustTranslate:
                                    if gridType == "DISCRETE":
                                        grid = self.__validateAdjustDiscreteKeys(grid,
                                          self.__parmName, tr)
                                    elif gridType == "WEATHER":
                                        grid = self.__validateAdjustWeatherKeys(grid,
                                          self.__parmName, tr)
                                
                                grid = self.__remap(self.__dbwe.getParmid(), grid, inGeoDict, inFillV)
                                
                                # if rate parm, then may need to adjust the values
                                if self.__rateParm and inTimes[i] != tr:
                                    grid = self.__adjustForTime(inTimes[i], tr, grid,
                                      inFillV)
            
                                # get inventory original inventory
                               # self.__dbinv = self.__dbwe.getKeys()
                               # print self.__dbinv
            
                                # merge the grids
                                self.__processIncomingGrid(parmName, grid, history[i],
                                  mGrid, tr, inFillV)
            
                            except:
                                
                                self.logProblem('Failure to process grid in file [',
                                  filename, '] Parm=', parmName, ' Time=',
                                  self.__printTR(tr), traceback.format_exc())
                                numFailed = numFailed + 1
            
                        else:
                            self.logDebug("Skipping Grid: ", parmName, " TR=", self.__printTR(tr), "outside start/end range")
            
            
                    # blank out any gaps
                    if self.__blankOtherPeriods == 1:
                        blankTimes = self.__calcBlankingTimes(inTimesProc)
                        # get updated inventory
                        
                        for i in xrange(len(blankTimes)):
                            tr = iscTime.intersection(blankTimes[i], self.__modProcTime)
                            if tr is not None:
                                try:
                                    self.__processBlankTime(mGrid, tr)
                                except:
                                    self.logProblem('Failure to process grid blanking Parm=', parmName, ' Time=', self.__printTR(tr), traceback.format_exc())
                            
                    
                    
                    # Returns tuple of (parmName, TR, #grids, #fails)
                    if len(inTimesProc):
                        totalTimeRange = (inTimesProc[0][0], inTimesProc[ -1][ -1] - 3600)
                    self._wec.flush()
                    
                    retryAttempt = retries
                except:
                    retryAttempt = retryAttempt + 1
                    self.logProblem("Error saving ISC data. Retrying (", retryAttempt, "/", retries, ")",traceback.format_exc())
                    time.sleep(1)
                finally:
                    self.logDebug("iscMosaic: Attempting to release cluster lock for:",parmName)
                    ClusterLockUtils.unlock(clusterLock, False)
                    self.logDebug("iscMosaic: Successfully released cluster lock for:",parmName)
            elif str(clusterLock.getLockState()) == "OLD":
                retryAttempt = retryAttempt + 1
                # Clear old lock to retry
                self.logDebug("Old lock retrieved for ISC write. Attempting to renew lock")
                ClusterLockUtils.unlock(clusterLock, False)
            elif str(clusterLock.getLockState()) == "FAILED":
                retryAttempt = retryAttempt + 1
                if retryAttempt == retries:
                    self.logProblem("Cluster lock could not be established for ",self._we.getParmid(),"at time range",TimeRange(tr[0],tr[1]),"Data was not saved.")
                else:
                    self.logProblem("Cluster lock request failed for ISC write.", retries, "Retrying (", retryAttempt, "/", retries, ")")
                    time.sleep(1)

        return (pName, totalTimeRange, len(inTimesProc), numFailed)
        
    def __processIncomingGrid(self, parmName, remappedGrid, remappedHistory, mGrid, tr, inFillV):
        # calculate merge
        merge = iscTime.mergeTR(tr, self.__dbinv)

        # get the associated db grids, merge, and store
        for m in merge:
            
            self.logDebug("Merge: ", self.__printTR(m[0]),
            self.__printTR(m[1]), m[2])
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
                if m[2] == 1 or (m[2] == 0 and m[0] == None):
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
                if m[2] == 1 or (m[2] == 0 and m[0] == None):
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
            #try:
                #self.__dbwe.histSave(tr, grid[0], grid[1])
            self._wec[tr] = grid
            #except Exception, e:
            #    self.logProblem("StoreFailed: ", tr, `grid[1]`)
            #    raise e

            if tr not in self.__dbinv:
                self.__dbinv = self._wec.keys()
                #self.__dbinv = self.__dbwe.keys()
            self.logDebug("Store:", self.__printTR(tr))
        else:
            self._wec[tr] = None
            self.__dbinv = self._wec.keys()
            #self.__dbwe[tr] = None
            #self.__dbinv = self.__dbwe.keys()
            self.logDebug("Erase:", self.__printTR(tr))

        
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
            #grid = self.__dbwe.getGridAndHist(tr)
            grid = self._wec[tr]
            if grid is not None:
                destGrid, history = grid
                
                tempHistory = []
                for hist in history:
                    tempHistory.append(hist.getCodedString())
                history = tempHistory
                
                self.__dbGrid = (destGrid, history, tr)
            else:
                self.logProblem("Unable to access grid for ",
                  self.__printTR(tr), "for ", self.__parmName)
                return None

        return (self.__dbGrid[0], self.__dbGrid[1])
    #---------------------------------------------------------------------
    # calculate file start/end processing times
    # Returns (startTime, endTime) or None for processing
    # file= netcdf file descriptor object
    #---------------------------------------------------------------------
    def __getFileProcessingTimes(self, file):
        
       # try:
        startTime = self.__decodeTimeString(getattr(file, 'startProcTime'))
        endTime = self.__decodeTimeString(getattr(file, 'endProcTime'))
        modProcTime = iscTime.intersection((startTime, endTime),
          self.__processTimePeriod)

        if modProcTime is None:
            self.logProblem("Skipping file due to non overlapping periods")
        return modProcTime
    
    #-------------------------------------------------------------------------
    # Get printable time range (yymmdd_hhmm,yymmdd_hhmm)
    # Works with list or tuple
    #-------------------------------------------------------------------------
    def __printTR(self, tr):
        if tr is not None:
            format = "%Y%m%d_%H%M"
            s = '(' + time.strftime(format, time.gmtime(tr[0])) + ',' + \
              time.strftime(format, time.gmtime(tr[1])) + ')'
            return s
        else:
            return "None"
        
    def __decodeTimeString(self, timeStr):
        "Create an Integer time from a string: YYYYMMDD_HHMM"

        importError = True
        while importError:
            try:
                intTime = time.strptime(timeStr, "%Y%m%d_%H%M")
                importError = False
            except ImportError:
                importError = True
            except:
                self.logProblem(timeStr, \
                  "is not a valid time string.  Use YYYYMMDD_HHMM", traceback.format_exc())
                raise Exception, "Bad date format YYYYMMDD_HHMM"

        return iscTime.timeFromComponents(intTime)
    
    #---------------------------------------------------------------------
    # get netcdf input variables
    # Gets the input variables from the netCDF file based on the parm name.
    # The netCDF file is opened on file.
    # Returns them as three tuples:  (dbParmName, vars, history[])
    # The vars is an array depending upon the data type:
    #   scalar [0], vector [0=mag,1=dir], wx [0=grid,1=key].
    # The history is a list of history strings.
    #---------------------------------------------------------------------
    def __getNetCDFInputVariables(self, file, parmName):

        var = file.variables[parmName]

        # make sure it is a weather element variable
        if not hasattr(var, "validTimes"):
            return None

        gridType = getattr(var, "gridType")

        # get the history info
        if gridType == 'SCALAR':
            pn = parmName + "_GridHistory"
        elif gridType == 'VECTOR':
            indx = string.find(parmName, "_Mag_")
            if indx == -1:
                return None
            pn = parmName[0:indx + 1] + parmName[indx + 5:] + "_GridHistory"
        elif gridType == 'WEATHER':
            pn = parmName + "_GridHistory"
        elif gridType == 'DISCRETE':
            pn = parmName + "_GridHistory"
        hvar = file.variables[pn]
        history = []
        for i in xrange(0, hvar.shape[0]):
            h = string.strip(hvar[i].tostring())
            history.append(string.split(h, '^'))

        # handle special cases for Vector and Wx, need to use a second
        # variable for wind and weather
        gridType = getattr(var, "gridType")

        if gridType == 'SCALAR':
            return (parmName, [var], history)

        elif gridType == 'VECTOR':
            indx = string.find(parmName, "_Mag_")
            if indx != -1:
                dirparm = parmName[0:indx] + "_Dir_" + parmName[indx + 5:]
                varDir = file.variables[dirparm]
                dbparmName = parmName[0:indx] + parmName[indx + 4:]
                return (dbparmName, [var, varDir], history)
            else:
                return None

        elif gridType == 'WEATHER':
            varKey = file.variables[parmName + "_wxKeys"]
            return (parmName, [var, varKey], history)

        elif gridType == 'DISCRETE':
            varKey = file.variables[parmName + "_keys"]
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
            'latIntersect': 0.0,
            'projectionID': 'hi'
            }

        # all projections have this information
        data = getattr(var, "latLonLL")
        inProjData['latLonLL'] = (float(data[0]), float(data[1]))
        data = getattr(var, "latLonUR")
        inProjData['latLonUR'] = (float(data[0]), float(data[1]))
        inProjData['projectionType'] = str(getattr(var, "projectionType"))
        data = getattr(var, "gridPointLL")
        inProjData['gridPointLL'] = (int(data[0]), int(data[1]))
        data = getattr(var, "gridPointUR")
        inProjData['gridPointUR'] = (int(data[0]), int(data[1]))

        # lambert conformal specific information
        if inProjData['projectionType'] == 'LAMBERT_CONFORMAL':
            data = getattr(var, "latLonOrigin")
            inProjData['latLonOrigin'] = (float(data[0]), float(data[1]))
            data = getattr(var, "stdParallelOne")
            inProjData['stdParallelOne'] = float(data)
            data = getattr(var, "stdParallelTwo")
            inProjData['stdParallelTwo'] = float(data)

        # polar stereographic
        elif inProjData['projectionType'] == 'POLAR_STEREOGRAPHIC':
            data = getattr(var, "lonOrigin")
            inProjData['lonOrigin'] = float(data)

        # mercator
        elif inProjData['projectionType'] == 'MERCATOR':
            data = getattr(var, "lonCenter")
            inProjData['lonCenter'] = float(data)

        # get specific grid sizes and domains
        data = getattr(var, "gridSize")
        inProjData['gridSize'] = (int(data[0]), int(data[1]))
        origin = getattr(var, "domainOrigin")
        extent = getattr(var, "domainExtent")
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
        gridType = getattr(var, "gridType")
        if gridType == 'SCALAR' or gridType == 'VECTOR':
            return - 30000.0
        else:
            return - 127
        
        #---------------------------------------------------------------------
    # compute the area mask
    # Returns areaMask to use based on the siteID. for ISC data,
    # the edit area is normally ISC_xxx where xxx is the WFO.
    #---------------------------------------------------------------------
    def __computeAreaMask(self):
        if self.__ignoreMask:
            domain = self.__dbwe.getGpi().getGridLoc()
            #maskDims = (domain.getNy().intValue(), domain.getNx().intValue())
            #areaMask = numpy.ones(maskDims)
            #areaMask.setGloc(domain)
            
            
            areaMask = ReferenceData(domain, ReferenceID("full"), None, CoordinateType.GRID);
            areaMask.getGrid();
            areaMask.invert();

        elif self.__altMask is not None:
            try:
                areaMask = iscUtil.getEditArea(self.__altMask, self.__mysite)
                areaMask.setGloc(self.__dbwe.getGpi().getGridLoc())
            except:
                self.logProblem("Unable to access edit mask [",
                  self.__altMask, "]", traceback.format_exc())
                raise Exception, "Unknown edit area mask [" + self.__altMask + ']'
        else:
            maskName = "ISC_" + self.__siteID
            try:
                areaMask = iscUtil.getEditArea(maskName, self.__mysite)
                areaMask.setGloc(self.__dbwe.getGpi().getGridLoc())
            except:
                self.logProblem("Unable to access edit mask [",
                  maskName, "]", traceback.format_exc())
                raise Exception, "Unknown edit area mask [" + maskName + ']'

        return areaMask
    
    #---------------------------------------------------------------------
    # split grids on processing time, so to preserve all grids that
    # overlap partially the processing time
    #  processTimePeriod = time range to process grids for splits
    #---------------------------------------------------------------------
    def __splitGridsOnProcBoundary(self, processTimePeriod):
        dbinv = self.__dbinv
        mergeInfo = iscTime.mergeTR(processTimePeriod, dbinv)
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
        inTimesA = getattr(var, "validTimes")
        ntimes = len(inTimesA) / 2
        times = []
        for t in xrange(ntimes):
            times.append((inTimesA[t * 2], inTimesA[t * 2 + 1]))
        return times
    
        #---------------------------------------------------------------------
    # get grid from netCDF file.
    # gridType = type of grid: scalar, vector, weather
    # vars = netCDF variables
    # index = grid index
    # Returns grid as:
    #   scalar = grid
    #   vector = (magGrid, dirGrid)
    #   weather = (grid, key)
    # Note: the values in the grid may need to be changed if their is
    # a dataMultiplier or dataOffset attributes present. This will
    # also change the fill Value.
    #---------------------------------------------------------------------
    def __getGridFromNetCDF(self, gridType, vars, index):
        if gridType == 'SCALAR':
            grid = numpy.flipud(vars[0][index])
            return self.__scaleGrid(vars[0], grid)

        elif gridType == 'VECTOR':
            magGrid = numpy.flipud(vars[0][index])
            dirGrid = numpy.flipud(vars[1][index])
            return (self.__scaleGrid(vars[0], magGrid),
              self.__scaleGrid(vars[1], dirGrid))

        elif gridType == 'WEATHER':
            compKey = self.__compressKey(vars[1][index, :, :])
            grid = (numpy.flipud(vars[0][index]), compKey)
            
        elif gridType == 'DISCRETE':
            compKey = self.__compressKey(vars[1][index, :, :])
            grid = (numpy.flipud(vars[0][index]), compKey)
        
        return grid
    
    #---------------------------------------------------------------------
    # scaling changes for incoming grids
    # var = netCDF variable
    # grid = input grid
    # only should be called for SCALAR/VECTOR
    #---------------------------------------------------------------------
    def __scaleGrid(self, var, grid):
        #scaling changes
        inFillV = getattr(var, "fillValue")

        # any scaling needed?
        try:
            multiplier = getattr(var, "dataMultiplier")
            offset = getattr(var, "dataOffset")
        except:
            multiplier = None
            offset = None

        outFillV = self.__determineFillValue(var)
        if outFillV == inFillV and multiplier is None:
            return grid    # no changes needed

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
    
    def __remap(self, pid, grid, inGeoDict, inFillV):

        gpi = GridParmManager.getGridParmInfo(pid).getPayload()

        gridType = gpi.getGridType().toString()
        
        gs = self.__decodeGridSlice(pid, grid, TimeRange())
        
        pd = self.__decodeProj(inGeoDict)
        fill = inFillV
        ifill = int(inFillV)

        origin = Coordinate(float(str(inGeoDict['gridDomain'][0][0])), float(str(inGeoDict['gridDomain'][0][1])))
        extent = Coordinate(float(str(inGeoDict['gridDomain'][1][0])), float(str(inGeoDict['gridDomain'][1][1])))
        
        gl = GridLocation("iscMosaic", pd, self.__getSize(gs), origin, extent, "GMT")
        mapper = RemapGrid(gl, gpi.getGridLoc())
        
        if gridType == 'SCALAR': 
            newGrid = mapper.remap(gs.getScalarGrid(), fill, gpi.getMaxValue(), gpi.getMinValue(), fill)
            return newGrid.__numpy__[0]
 
        elif gridType == 'VECTOR':
            magGrid = Grid2DFloat(gs.getGridParmInfo().getGridLoc().getNx().intValue(), gs.getGridParmInfo().getGridLoc().getNy().intValue())
            dirGrid = Grid2DFloat(gs.getGridParmInfo().getGridLoc().getNx().intValue(), gs.getGridParmInfo().getGridLoc().getNy().intValue())
            mapper.remap(gs.getMagGrid(), gs.getDirGrid(), fill, gpi.getMaxValue(), gpi.getMinValue(), fill, magGrid, dirGrid)
            return (magGrid.__numpy__[0], dirGrid.__numpy__[0])
        
        elif gridType == 'WEATHER':
            newGrid = mapper.remap(gs.getWeatherGrid(), fill, fill)
            return (newGrid.__numpy__[0], grid[1])
        
        elif gridType == 'DISCRETE':
            newGrid = mapper.remap(gs.getDiscreteGrid(), fill, fill)
            return (newGrid.__numpy__[0], grid[1])
        
    def __decodeGridSlice(self, pid, value, tr, history=None): 
        
        gpi = GridParmManager.getGridParmInfo(pid).getPayload()
        gridType = gpi.getGridType().toString()
        
        hist = ArrayList()
         
        if history is None:
            hist.add(GridDataHistory(OriginType.INITIALIZED, pid, tr))
        else:
            #FIXME
            for i in range(0, len(history)):
                hist.add(history[i]);

        if gridType == 'SCALAR':
            data = Grid2DFloat.createGrid(value.shape[1], value.shape[0], value)
            slice = ScalarGridSlice(tr, gpi, hist, data)
        elif gridType == 'VECTOR': 
            
            magVal = value[0]
            dirVal = value[1]
            
            magGrid = Grid2DFloat.createGrid(magVal.shape[1], magVal.shape[0], magVal)
            dirGrid = Grid2DFloat.createGrid(dirVal.shape[1], dirVal.shape[0], dirVal)
            slice = VectorGridSlice(tr, gpi, hist, magGrid, dirGrid)
        elif gridType == 'WEATHER':         
            data = Grid2DByte.createGrid(value[0].shape[1], value[0].shape[0], value[0])
            keyList = ArrayList()
            for key in value[1]:
                keyList.add(WeatherKey())
            slice = WeatherGridSlice();
            slice.setValidTime(tr)
            slice.setGridParmInfo(gpi)
            slice.setGridDataHistory(hist)
            slice.setWeatherGrid(data)
            slice.setKey(keyList)
        elif gridType == 'DISCRETE':         
            data = Grid2DByte.createGrid(value[0].shape[1], value[0].shape[0], value[0])
            keyList = ArrayList()
            for key in value[1]:
                keyList.add(DiscreteKey())
            slice = DiscreteGridSlice();
            slice.setValidTime(tr)
            slice.setGridParmInfo(gpi)
            slice.setGridDataHistory(hist)
            slice.setDiscreteGrid(data)
            slice.setKey(keyList)
        return slice
        
    def __decodeProj(self, pdata):
        
        pid = "GrandUnifiedRemappingProj"
        type = ProjectionType.valueOf(pdata["projectionType"])
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
        
        return ProjectionData(pid, type, llLL, llUR, llo, sp1, sp2, gpll, gpur, lati, lonc, lono)
        
    def __getSize(self, gs):
        gridType = gs.getGridParmInfo().getGridType().toString()
        
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
        outKeys = []
        shape = keys.shape
        for k in xrange(shape[0]):
            s = ""
            for i in xrange(shape[1]):
                c = str(keys[k][i])
                if c != '\0':
                    s = s + c
            s = string.strip(s)
            if len(s) > 0:
                outKeys.append(s)
        return outKeys
        
    def __printShortTR(self, tr):
        if tr is not None:
            format = "%d/%H"
            s = '(' + time.strftime(format, time.gmtime(tr[0])) + '->' + \
              time.strftime(format, time.gmtime(tr[1])) + ')'
            return s
        else:
            return "None"
        
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

        dataType = self.__dbwe.getGpi().getGridType().toString()
        if dataType == 'SCALAR':
            fillMask = numpy.equal(grid, fillValue)
            return numpy.where(fillMask, grid, grid * durationRatio)
        elif dataType == 'VECTOR':
            fillMask = numpy.equal(grid[0], fillValue)
            newMagGrid = numpy.where(fillMask, grid[0], (grid[0] * durationRatio))
            return (grid[0], grid[1])
        else:
            return grid
        
    def __calcBlankingTimes(self, inTimes):
        out = []
        for t in range(len(inTimes)):
            if t == 0 and inTimes[t][0] != 0:
                out.append((0, inTimes[t][0]))
            elif t != 0 :
                tr = (inTimes[t - 1][1], inTimes[t][0])
                if tr[0] != tr[1]:
                    out.append(tr)
        if len(out) == 0:
            out.append((0, 2 ** 30 - 1 + 2 ** 30))
        else:
            endIndex = len(inTimes) - 1
            out.append((inTimes[endIndex][1], 2 ** 30 - 1 + 2 ** 30))

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
            if m[0] != None and m[2] == 1:
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
        history = self.__dbwe.getItem(iscUtil.toJavaTimeRange(tr)).getHistory()
        
        for i in range(0, len(history)):
            index = string.find(history[i].getCodedString(), self.__siteID + "_GRID")
            if index != -1:
                return 1
        return 0
    
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
          ' tr=' + self.__printTR(tr)

        # get the list of discrete keys for this parameter that are allowed
        dd = self.__disDef.keys(parmName)
        if dd.size() == 0:
            self.logProblem("Unable to validate keys for ",
              parmName, " - no def in DiscreteDefinition")
            return grid

        #now go through the incoming grid's keys and validate each one
        for idx in xrange(len(key)):  #each index of the discrete key
            keyentry = key[idx]    #each discrete key entry
            oldEntry = keyentry  #save an unchanged copy for reporting
            changedReasons = []

            #overlap check
            ci = keyentry.find("^")
            if ci != -1 and not self.__disDef.overlaps(parmName):
                keyentry = keyentry[0:ci]   #reset it to only 1st portion
                changedReasons.append("No Overlap Allowed")

            eachKey = keyentry.split("^")
            for idx1 in xrange(len(eachKey)):
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
                for i in xrange(dd.size()):
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
                self.logProblem(smsg,
                    "from [" + oldEntry + "] to [" + keyentry + "]",
                    "(" + ",".join(changedReasons) + ")")
                msg = self.__siteID + " " + parmName + " " + \
                  self.__printShortTR(tr) + \
                  " [" + oldEntry + "] -> [" + keyentry + "] (" + \
                  ",".join(changedReasons) + ")"
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
          ' tr=' + self.__printTR(tr)

        #now go through the incoming grid's keys and validate each one
        for idx in xrange(len(key)):  #each index of the weather key
            changedReasons = []
            keyentry = key[idx]    #each weather key entry
            oldEntry = keyentry  #save an unchanged copy for reporting
            ikeys = keyentry.split("^")  #split into individual subkeys
            for idx1 in xrange(len(ikeys)):
                cov, typ, inten, vis, attrs = ikeys[idx1].split(":")

                # check the visibility
                visibilities = self.__wxDef.getWeatherVisibilities()
                validViz = False
                for i in xrange(visibilities.size()):
                    if visibilities.get(i).getSymbol() == vis:
                        validViz = True
                        break
                if not validViz:
                    vis = visibilities.get(0).getSymbol() #assign 1st one
                    changedReasons.append("Unknown Visibility")

                # check the type
                types = self.__wxDef.getWeatherTypes()
                validType = False
                for i in xrange(types.size()):
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
                self.logProblem(smsg,
                  " from [" + oldEntry + "] to [" + keyentry + "]",
                    "(" + ",".join(changedReasons) + ")")
                msg = self.__siteID + " " + parmName + " " + \
                  self.__printShortTR(tr) + \
                  " [" + oldEntry + "] -> [" + keyentry + "] (" + \
                  ",".join(changedReasons) + ")"
                self.__adjDataMsg.append(msg)

        return (g, key)
            
    #---------------------------------------------------------------------
    # erase all grids from the given weather element over the
    # processTimePeriod procesTimePeriod = time range to remove grids
    #---------------------------------------------------------------------
    def __eraseAllGrids(self, processTimePeriod):
        dbinv = self.__dbinv
        mergeInfo = iscTime.mergeTR(processTimePeriod, dbinv)
        oldGrid = None
        oldTR = None
        for m in mergeInfo:
            if m[2] == 0:  #split grid, don't erase
                if m[0] != oldTR:
                    oldGrid = self.__getDbGrid(m[0])
                    oldTR = m[0]
                if oldGrid is not None:
                    if self.__rateParm:
                        adjGrid = self.__adjustForTime(m[0], m[1], oldGrid[0],
                          0.0) #old db grids don'thave missing value flags
                        self.__storeGrid(m[1], (adjGrid, oldGrid[1]))
                    else:
                        self.__storeGrid(m[1], oldGrid)
            elif m[2] == 1: #overlaps mean erase
                self.__storeGrid(m[1], None)

        self.__dbGrid = None


def convertList(unknownList):
    retVal = unknownList
    try:
        len(unknownList)
    except TypeError:
        retVal = JUtil.javaObjToPyVal(unknownList)
    return retVal

def main(siteID, userID, databaseID, parmsToProcess, blankOtherPeriods, 
        startTime, endTime, altMask, replaceOnly, eraseFirst, 
        announce, renameWE, iscSends, inFiles, ignoreMask, 
        adjustTranslate, deleteInput, parmsToIgnore, gridDelay, logFileName):    
    # convert Java types to python and send to IscMosaic for execution
    parmsToProcess = convertList(parmsToProcess)
    inFiles = convertList(inFiles)
    parmsToIgnore = convertList(parmsToIgnore)
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
            "inFiles": inFiles,
            "ignoreMask": bool(ignoreMask), 
            "adjustTranslate": bool(adjustTranslate),
            "deleteInput": bool(deleteInput), 
            "parmsToIgnore": parmsToIgnore,
            "gridDelay": float(gridDelay), 
            "logFileName": logFileName}
    mosaic = IscMosaic(argv)
    mosaic.execute()
    mosaic = None
