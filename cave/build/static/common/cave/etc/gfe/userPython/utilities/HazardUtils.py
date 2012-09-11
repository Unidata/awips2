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
# HazardUtils
#
# Author:
# ----------------------------------------------------------------------------

import SmartScript
import time, string
import VTECTable
import LogStream
import numpy
from AbsTime import AbsTime
from AbsTime import current
from TimeRange import TimeRange
from java.util import Date
from java.util import ArrayList
import jep
from JUtil import JavaWrapperClass
#from Numeric import *

def LOCK_HOURS():
    return 192

def HOUR_SECONDS():
    return 3600

MODEL = "Fcst"
ELEMENT = "Hazards"
LEVEL = "SFC"

# Status return codes for _separateHazardGrids
SUCCESS = 1
FAIL_REDUNDANT = 0
FAIL_LOCK = -1

class HazardUtils(SmartScript.SmartScript):
    def __init__(self, dbss, eaMgr, mdMode=None, toolType="numeric"):
        SmartScript.SmartScript.__init__(self, dbss)
        
        # self.setUp(eaMgr, mdMode, toolType)

    ##
    # Get timeRanges that make up the inventory of the given weather element.
    # This is normally only used for the hazards inventory, so model is "Fcst"
    # and level is "SFC" in the lookup.
    #
    # @param WEName: The weather element whose time ranges are to be acquired.
    # @type WEName: string
    # @param timeRange: optional time range of the inventory. If not specified,
    #                   the default is from 24 hours ago to ten days from now.
    # @type timeRange: Java or Python TimeRange
    # @param asJava: If True, the inventory is returned as a list of Java 
    #                TimeRanges; if False, the inventory is returned as a list 
    #                of Python TimeRanges. The default is False.
    # @type asJava: boolean
    # @return: The time ranges for WEName that overlap the specified or default 
    #          time range.
    def _getWEInventory(self, WEName, timeRange=None, asJava=False):
        # set up a timeRange if it is None
        if timeRange is None:
            now = current()
            yesterday = now - (24 * 3600) # one day ago
            later = now + 10 * 24 * 3600  # 10 days from now
            timeRange = self._makeTimeRange(yesterday.unixTime(), later.unixTime())
        parm = self.getParm(MODEL, WEName, LEVEL)
        trList = []
        if parm is not None:
            if isinstance(timeRange, JavaWrapperClass):
                timeRange = timeRange.toJavaObj()
            gridInventory = parm.getGridInventory(timeRange)
            for g in gridInventory:
                gridTimeRange = g.getGridTime()
                tr = gridTimeRange.clone()
                if not asJava:
                    tr = TimeRange(tr)
                trList.append(tr)

        return trList

    # makes a TimeRange from a start and end integers
    def _makeTimeRange(self, start, end):
        return TimeRange(AbsTime(start), AbsTime(end))

    ##
    # Get timeRanges that correspond to gaps in the specified WEName inventory
    # within the specified time ranges.
    #
    # @param WEName: A weather element name
    # @type WEName: string
    # @param trList: Time ranges of interest
    # @type trList: list of Python or Java TimeRange
    # @return: time ranges overlapping timeRange that are missing from the 
    #          inventory of WEName
    # @rtype: list of Python TimeRanges 
    def _getGaps(self, WEName, trList):

        fullHazardInv = self._getWEInventory(WEName)
        gaps = []

        for timeRange in trList:
        
            # Convert Java TimeRange to Python for comparisons
            if not isinstance(timeRange, TimeRange):
                timeRange = TimeRange(timeRange)
                
            hazInv = []
            for h in fullHazardInv:
                if timeRange.overlaps(h):
                    hazInv.append(h)
    
            # check for empty inventory
            if len(hazInv) == 0:   # no grids at all
                gaps.append(timeRange)
                continue
    
            # see if we have a gap at the beginning
            if timeRange.startTime() < hazInv[0].startTime():
                tr = TimeRange(timeRange.startTime(),
                               hazInv[0].startTime())
                gaps.append(tr)
    
            # Find any gaps in the middle of the inventory
            for i in range(len(hazInv) - 1):
                if hazInv[i].endTime() != hazInv[i+1].startTime():
                    gapTR = TimeRange(hazInv[i].endTime(),
                                      hazInv[i+1].startTime())
                    gaps.append(gapTR)
    
            # see if we have a gap at the end of the inventory
            if timeRange.endTime() > hazInv[-1].endTime():
                tr = TimeRange(hazInv[-1].endTime(),
                               timeRange.endTime())
                gaps.append(tr)

        return gaps
    
    ##
    # Create an empty hazards-type grid with the specified
    # name and timeRange
    #
    # @param weName: The name of the weather element to create.
    # @type weName: string
    # @param timeRange: The time range of the new grid.
    # @type timeRange: a Java or Python TimeRange
    # @raise JepException: when raised by SmartScript methods. 
    def _makeEmptyHazardGrid(self, weName, timeRange):
        gridShape = self.getGridShape()
        byteGrid = numpy.zeros(gridShape, dtype='int8')
        hazKeys = self.getDiscreteKeys(ELEMENT)
        currentKeys = ["<None>"]
        # make the grid
        if weName == ELEMENT:
            self.createGrid(MODEL, weName, "DISCRETE", (byteGrid, currentKeys),
                            timeRange, discreteKeys=hazKeys,
                            discreteAuxDataLength=4, discreteOverlap=1)
        else:
            hazard = self._tempWENameToKey(weName)
            discreteKeys = ["<None>", hazard]
            hazKeyDesc = self._addHazardDesc(discreteKeys)
            self.createGrid(MODEL, weName, "DISCRETE", (byteGrid, currentKeys),
                            timeRange, discreteKeys=hazKeyDesc,
                            discreteAuxDataLength=4, discreteOverlap=0,
                            defaultColorTable="YesNo")
        return
    
    ##
    # Prepare the Hazards inventory so that it can be merged with the 
    # activeTable.  This includes splitting grids and adding new ones where
    # we have gaps.
    #
    # @param weName: Name of a weather element
    # @type weName: string
    # @param trList: Time ranges of interest
    # @type trList: list of Python or Java TimeRanges
    def _setupHazardsInventory(self, weName, trList):
        # see if the element exists yet, if not, make a new grid
        # This is a painful way just to see if the grid exists
        # but all other techniques fail for temporary weather elements
        now = current()
        yesterday = now - (24 * 3600) # one day ago
        later = now + 10 * 24 * 3600  # 10 days from now
        timeRange = TimeRange(yesterday, later).toJavaObj()
        try:
            gridInfo = self.getGridInfo(MODEL, weName, LEVEL, timeRange)
        except:  # this means the WE does not exist, so make a grid
            if len(trList) <= 0:
                return
            for tr in trList:
                self._makeEmptyHazardGrid(weName, tr)
            return
        # fill any gaps in the inventory
        gapList = self._getGaps(weName, trList)
        for g in gapList:
            self._makeEmptyHazardGrid(weName, g)

        # Split the grids at the timeRange boundaries
        unix_now = now.unixTime()
        for tr in trList:
            # If tr is a java timerange, convert it to a python TimeRange
            if not isinstance(tr, TimeRange):
                tr = TimeRange(tr)
            end = tr.endTime().unixTime()
            if end > unix_now:
                # parm.splitTR() will split timeRanges with non-zero minutes
                # to the next hour.  So, truncate start and end times to the 
                # previous hour and then split
                start = tr.startTime().unixTime()
                start = int(start / 3600) * 3600
                end = int(end / 3600) * 3600
                roundedTR =  TimeRange(AbsTime(start), AbsTime(end)).toJavaObj()
                parm = self.getParm(MODEL, weName, LEVEL)
                self.splitCmd([weName], roundedTR)

        return

    # returns a Numeric mask where each zone in zoneList is set to 1
    def _makeMask(self, zoneList):
        gridSize = self.getGridShape()
        mask = numpy.zeros(gridSize, dtype='int8')
        eaList = self.editAreaList()
        for z in zoneList:
            if z in eaList:
                zoneArea = self.getEditArea(z)
                zoneMask = self.encodeEditArea(zoneArea)
                mask = numpy.logical_or(mask, zoneMask)

        return mask

    # Fetches the gridSize from the GFE and returns it as a tuple.
    def _getGridSize(self):
        gridLoc = self.getGridLoc()
        xSize = gridLoc.gridSize().x
        ySize = gridLoc.gridSize().y
        gridSize = (ySize, xSize)
        return gridSize

    ##
    # Determine whether temporary weather elements are loaded.
    #
    # @return: 1 if temporary weather elements are loaded;
    #          0 otherwise.
    def _tempWELoaded(self):
        parms = self.loadedParms()
        for weName, level, dbID in parms:
            if string.find(weName, "haz") == 0:
                return 1

        return 0

    ##
    # Create a temporary weather element name from key.
    #
    # @param key: String like BZ.W:1234, or LCLKEY, or BZ.W
    # @type key: string
    # @return: key with 'haz' prepended and any '.' or ':' chars removed.
    # @rtype: string
    def _makeTempWEName(self, key):
        "Create a temporary weather element name from a key string."
        #key is BZ.W:1234, or LCLKEY, or BZ.W
        key = string.replace(key, ".","")
        key = string.replace(key, ":","")
        weName = "haz" + key
        return weName
 
    ##
    # Create a key string from a temporary weather element name.
    # 
    # @param wename: A temporary weather element name
    # @type wename: string
    # @return: The key string from which the temporary element was derived.
    # @rtype: string
    def _tempWENameToKey(self, wename):
        "Make a key string from a temporary weather element name."
        #wename is hazBZW, hazBZW1234, hazLCLK
        if len(wename) > 3 and wename[0:3] == 'haz':
            key = wename[3:]  #eliminate "haz"
            if len(key) >= 3:
                vkey = key[0:2] + '.' + key[2]
                if VTECTable.VTECTable.has_key(vkey):
                    seg = key[3:]
                    if len(seg):
                        return vkey + ':' + seg
                    else:
                        return vkey
                # local key, look for segment via digits
                else:
                    lkey = key
                    for i in xrange(len(key)):
                        if key[i:].isdigit():
                            lkey = key[0:i] + ":" + key[i:]
                            break
                    return lkey
            else:
                # TODO: or should I fail?
                return key
        else:
            raise Exception, "Illegal wename: " + wename
                    
    ##
    # Gets the unique list of keys over the specified mask
    # if no mask is passed, the entire grid is used
    #
    # @param byteGrid: Grid of indices
    # @type byteGrid: Numpy array of int8
    # @param keys: Keys associated with byteGrid. If byteGrid[2,2] is 3, then
    #              keys[3] describes its state.
    # @type keys: List of strings
    # @param mask: Optional mask of points to include; defaults to all ones.
    # @type mask: Numpy array of boolean, same dimensions as byteGrid;
    # @return: The keys referenced by the masked byteGrid, without duplicates.
    # @rtype: List of strings
    def _getUniqueKeys(self, byteGrid, keys, mask = None):
        uniqueKeys = []

        # if mask is None, make a mask of the whole area
        if mask is None:
            mask = numpy.ones(byteGrid.shape)

        # get the list of values over the mask area only
        valueList = numpy.compress(mask.flat, byteGrid.flat)
       
        # remove the duplciates to get unique values
        uniqueValues = list( numpy.unique(valueList) )

        # extract the keys that correspond to the byte values
        for u in uniqueValues:
            uniqueKeys.append(keys[u])

        return uniqueKeys

    ##
    # Get the phen portion of key.  
    # If key is not a VTEC hazard key, returns ""
    # @param key: A grid key
    # @type key: string
    # @return: The phen portion of key.
    # @rtype: string
    def _keyPhen(self, key):
        pos = string.find(key, ".")
        if pos == -1:   # not found
            return ""

        return key[0:pos]

    ##
    # Get the sig portion of key.
    # If key is not a VTEC hazard key, returns ""
    # 
    # @param key: A grid key.
    # @type key: string
    # @return: The sig portion of key.
    # @rtype: string
    def _keySig(self, key):
        pos = string.find(key, ".")
        if pos == -1:   # not found
            return ""

        return key[pos + 1]

    ##
    # Combine newKey with subKeys and return a new combined key.  Enforces the 
    # rule that keys with the same phen returns the one key with the highest
    # priority sig.
    #
    # @param subKeys: The old key.
    # @type subKeys: string
    # @param newKey: The key to add.
    # @type newKey: string
    # @return: The key made by combining subKeys with newKey.
    # @rtype: string
    def _combinedKey(self, subKeys, newKey):
        if newKey is None:
            return subKeys

        subKeyList = string.split(subKeys, "^")

        # check for same keys
        if newKey in subKeyList:
            return subKeys

        defaultCombo = subKeys + "^" + newKey

        # check for non-VTEC key
        if string.find(newKey, ".") == -1:
            return defaultCombo

        # more exceptions - these phens are above the law
        exceptions = ["TO", "SV", "FF"]
        sigList = ["W", "Y", "A"]
        if self._keyPhen(newKey) in exceptions:
            return defaultCombo

        subKeyList = string.split(subKeys, "^")
        for sk in subKeyList:
            if self._keyPhen(sk) == self._keyPhen(newKey):
                subSig = self._keySig(sk)
                newSig = self._keySig(newKey)
                if subSig == newSig:
                    return subKeys

                if subSig not in sigList or newSig not in sigList:
                    continue
                
                if sigList.index(subSig) > sigList.index(newSig):
                    subKeys = subKeys.replace(sk, newKey)

                return subKeys

        return defaultCombo


    # Makes a new hazard given the old key oldKey and a new watch phenSig.
    # @param oldKey: The old key
    # @type oldKey: string
    # @param phenSig: The new watch phen and sig
    # @type phenSig: string
    # @return: A new combined key.
    # @rtype: string
    def _makeNewKey(self, oldKey, phenSig):
        # check for the dumb cases
        if oldKey == "<None>" or oldKey == phenSig:
            return phenSig

        # split up the key, add the hazard, sort, and reassemble
        parts = string.split(oldKey, "^")
        parts.append(phenSig)
        parts.sort()   # makes sure the same set of subKeys look the same

        # assemble the new key
        newKey = ""
        for p in parts:
            if newKey == "":
                newKey = p
            else:
                newKey = self._combinedKey(newKey, p)

        # just in case
        if newKey == "":
            newKey = "<None>"

        return newKey

    ##
    # Get the subkeys of key
    #
    # @param key: A key to divide into subkeys
    # @type key: String
    # @return: The subkeys of key
    # @rtype: List of strings 
    def _getSubKeys(self, key):
        parts = string.split(key, "^")
        if "<None>" in parts:
            parts.remove("<None>")
        return parts

    def _removeSubKey(self, key, subKey):
        newKey = ""
        for p in string.split(key, "^"):
            if p == subKey:
                continue
            if newKey == "":
                newKey = p
            else:
                newKey = newKey + "^" + p

        if newKey == "":
            newKey = "<None>"

        return newKey

    ##
    # Take a sequence or set of time ranges and produce a set of time ranges by
    # combining all adjacent or overlapping time ranges in the sequence.
    #
    # @param timeranges: the timeranges to merge
    # @type timeranges : sequence, set or frozenset of TimeRange
    # @return: the merged timeranges
    # @rtype: set of TimeRange
    def _mergeTimeranges(self, timeranges):
        trset = set(timeranges)
        # Loop until a pass doesn't merge any time ranges
        moreToDo = True
        while moreToDo:
            moreToDo = False
            merged = []
            for tr in trset:
                found = False
                for idx, mtr in enumerate(merged):
                    if tr == mtr:
                        found = True
                        break
                    elif tr.overlaps(mtr) or tr.isAdjacentTo(mtr):
                        found = True
                        merged[idx] = mtr.join(tr)
                        moreToDo = True
                        break
                if not found:
                    merged.append(tr)
            trset = set(merged)
        return trset

    ##
    # Determine whether the time ranges of any (temporary) parm in hazParms
    # overlaps a locked time range of the Hazards element. If not, add the
    # time ranges of the temporary parms to the locked time ranges of the
    # Hazards parm.
    #
    # @param hazParms: Temporary hazard parm names.
    # @type hazParms: sequence of string
    # @return: 0 if there are not conflicting locks, 1 if there are
    # @rtype: int
    def _conflictingLocks(self, hazParms):
        # find all the time ranges that should be locked
        neededTRs = set()
        
        for hazParm in hazParms:
            trList = self._getWEInventory(hazParm)
            neededTRs = neededTRs.union(trList)
        
        # Find all the time ranges that are locked in Hazards
        myTRs = self.lockedByMe(ELEMENT, LEVEL)
        myTRs = set(myTRs)
        
        # Add locks we already have to the needed TRs,
        # in case grids were deleted
        neededTRs = neededTRs.union(myTRs)

        # Squish the TRs into contiguous blocks
        neededTRs = self._mergeTimeranges(neededTRs)
        
        # See if there are any blocks we don't have yet
        missingTRs = neededTRs - myTRs
        
        # If not, then there are no conflicts and we're done.
        if len(missingTRs) == 0:
            return 0

        startTimes = jep.jarray(len(missingTRs), Date)
        
        midx = 0
        for missingTR in missingTRs:
            startTimes[midx] = missingTR.toJavaObj().getStart()
            midx += 1

        hazardParm = self.getParm(MODEL, ELEMENT, LEVEL)
        gridData = None
        try:
            gridData = hazardParm.startParmEdit(startTimes)
        except RuntimeError, runtimeErr:
            if runtimeErr.message is None:
                raise
            if runtimeErr.message.startswith("com.raytheon.viz.gfe.GFEOperationFailedException:"):
                return 1
            else:
                raise

        if gridData is not None and len(gridData) != 0:
            if not hazardParm.endParmEdit():
                return 1

        # The locks acquired in the endParmEdit() call may not have been quite right.
        # However, we needed to end the parm edit.
        # Negotiate the locks we _really_ need now that it's done.
        locktable = hazardParm.getLockTable()
        LOCK = locktable.getClass().getLockMode("LOCK");
        
        from com.raytheon.uf.common.dataplugin.gfe.server.request import LockRequest
        desiredLocks = ArrayList()
        for missingTR in missingTRs:
            newLock = LockRequest()
            newLock.setParmId(hazardParm.getParmID())
            newLock.setTimeRange(missingTR.toJavaObj())
            newLock.setMode(LOCK)
            desiredLocks.add(newLock)

        client = hazardParm.getDataManager().getClient()
        serverResponse = client.requestLockChange(desiredLocks)
        if not serverResponse.isOkay():
            hazardParm.undo()
            return 1
            
        return 0
        
    ##
    # Create a list of (key, desc) tuples from keys.
    # For each key in keys, look up the key in VTECTable.
    # If the key is found, use its headline value as its description;
    # otherwise, use the key as its own description.
    #
    # @param keys: Keys to look up descriptions for.
    # @type keys: iterable of strings
    # @return: keys and descriptions for the key
    # @rtype: list of 2-tuples 
    def _addHazardDesc(self, keys):
        newKeys = []
        for k in keys:
            index = string.find(k, ':')
            if index != -1:
                k = k[0:index]   #eliminate the colon and segment #
            if not VTECTable.VTECTable.has_key(k):
                desc = k
            else:
                # get the description
                desc = VTECTable.VTECTable[k]['hdln']

            newKeys.append((k, desc))

        return newKeys

    ##
    # Determine whether the Hazards forecast weather element is loaded.
    #
    # @param weName: The name of the weather element. Defaults to "Hazards".
    # @type wename: string
    # @return: 1 if the weather element is loaded, 0 otherwise
    # @rtype: int
    def _hazardsLoaded(self, weName=ELEMENT):

        tupleList = self.loadedParms()
        ##  look for the Hazards Weather element
        for element, level, databaseID in tupleList:
            modelName = databaseID.modelName()
            if element == weName and level == LEVEL and modelName == MODEL:
                return 1

        # if we got this far we didn't find it.
        return 0

    ##
    # Remove any grids for weName whose end times are in the past
    #
    # @param weName: A weather element name.
    # @type weName: string
    # @raise JepException: if calls to Java methods fail.
    def _removeOldGrids(self, weName):
        # get the inventory
        trList = self._getWEInventory(weName)

        for tr in trList:
            if tr.endTime().unixTime() < current().unixTime():
                self.deleteCmd([weName], tr)

        return

    ##
    # Remove any data grids for MODEL, ELEMENT, and LEVEL over the default
    # inventory timerange (from now to 10 days in the future). The parm 
    # remains in the parm manager.
    def _removeAllHazardsGrids(self):

        removeTRList = self._getWEInventory(ELEMENT, asJava=True)

        # Remove the real Hazards grid 
        for tr in removeTRList:
            if not self.deleteGrid(MODEL, ELEMENT, LEVEL, tr):
                return False
        return True


    ##
    # Destroy all the temporary hazards (they are removed from the parm manager).
    #
    def _removeTempHazardWEs(self):
        parms = self.loadedParms()

        for weName, level, dbID in parms:
            if string.find(weName, "haz") == 0 and len(weName) > 3:
                self.unloadWE(MODEL, weName, level)

        return

    ##
    # Determine whether the indicated grids are consecutive in time and
    # identical in value at every point.
    # @attention: This method assumes timeRange1 begins before timeRange2.
    #             It will give wrong answers if their order is reversed
    #
    # @param weName: Weather element name
    # @type weName: string
    # @param timeRange1: First time range for weather element
    # @type timeRange1: Python TimeRange
    # @param timeRange2: Second time range for weather element
    # @type timeRange2: Python TimeRange
    # @return: True if the end time for timeRange1 matches the start time of 
    #          timeRange2 and the grid for weName during timeRange1 is identical
    #          to the grid for weName during timeRange2, False otherwise.
    # @rtype: boolean
    def _consecutiveIdenticalGrids(self, weName, timeRange1, timeRange2):
        if timeRange1.endTime() == timeRange2.startTime():
            # get the grids
            firstGrid, key = self.getGrids(MODEL, weName, LEVEL, 
                                           timeRange1.toJavaObj(), mode="First", cache=0)
            secondGrid, key = self.getGrids(MODEL, weName, LEVEL,
                                            timeRange2.toJavaObj(), mode="First", cache=0)
            if numpy.sometrue(numpy.logical_xor(firstGrid, secondGrid)):
                return 0
            else:
                return 1

        return 0

    ##
    # Replace existing grids for weName with a single grid over the
    # time range from groupStart to groupEnd.
    #
    # This function should only be used by _consolidateTimes(); it 
    # exists only to be sure we create the consolidated grid the same way in
    # the "last timeRange" code block as we do in the "broke the string" block.
    # @param groupStart: Starting time as seconds since the epoch
    # @type groupStart: int
    # @param groupEnd: Ending time as seconds since the epoch
    # @type groupEnd: int
    # @param weName: (temporary) weather element name
    # @type weName: string
    # @return: None
    def _createConsolidatedGrid(self, groupStart, groupEnd, weName):
        "Used internally by _consolidateTimes()"
        timeRange = self._makeTimeRange(groupStart, groupEnd).toJavaObj()
        byteGrid, hazKey = self.getGrids(MODEL, weName, LEVEL, 
                                      timeRange, mode="First", cache=0)
        if isinstance(hazKey, str):
            hazKey = eval(hazKey)
        self.createGrid(MODEL, weName, "DISCRETE", (byteGrid, hazKey),
                        timeRange, discreteOverlap=1,
                        discreteAuxDataLength=4)

    ##
    # Consolidate grid times for each weather element in weNameList.
    # For each weather element, find time ranges that touch whose grids are
    # identical and turn them into a single grid for the combined time range.
    def _consolidateTimes(self, weNameList):
        for weName in weNameList:
            # Get "all" the time ranges for this element
            trList = self._getWEInventory(weName)
            if len(trList) == 0:
                return
            
            count = 1
            groupStart = int(trList[0].startTime().unixTime())
            groupEnd = int(trList[0].endTime().unixTime())

            for i in range(0, len(trList) - 1):
                if self._consecutiveIdenticalGrids(weName, trList[i], trList[i+1]):
                    # keep looking for the end
                    count = count + 1
                    groupEnd = int(trList[i+1].endTime().unixTime())
                else:  # broke the string of grids
                    if count > 1:  # store the new time-consolidated grid
                        self._createConsolidatedGrid(groupStart, groupEnd, weName)
                    # reset the times
                    groupStart = int(trList[i+1].startTime().unixTime())
                    groupEnd = int(trList[i+1].endTime().unixTime())
                    count = 1
                    
            # make sure we catch the last timeRange
            if count > 1:  # store the new time-consolidated grid
                self._createConsolidatedGrid(groupStart, groupEnd, weName)
                    
        return

    ##
    # Lock any grids in the hazards parm from now to 10 hours in the future.
    # 
    # @return: the hazards parm and its igrids
    # @rtype: a 2-tuple; the first item is a Parm, the second is a list of IGridDatas,
    #         which, for discrete grids, translate to a 2-tuple containing a numpy
    #         array and a key string. So, like this:
    #             (parm,[(arr0,key0), (arr1,key1), ...])
    #           
    def _lockHazards(self):
        "Flag the hazards parm as being edited. Return the hazards parm and its grid."
        hazParm = self.getParm(MODEL, ELEMENT, LEVEL)
        startAbsTime = AbsTime(int(current().unixTime() /3600)*3600) 
        endAbsTime = startAbsTime + LOCK_HOURS() * HOUR_SECONDS()
        timeRange = TimeRange(startAbsTime, endAbsTime)
        
        inventory = self._getWEInventory(ELEMENT, timeRange, asJava=True)
        startTimes = jep.jarray(len(inventory), Date) 
        for trNum in range(len(inventory)):
            startTimes[trNum] = inventory[trNum].getStart()
        gridData = None
        try:
            # startParmEdit() refreshes the grids and sets up the times that endParmEdit() will lock.
            gridData = hazParm.startParmEdit(startTimes)
        except RuntimeError, runtimeErr:
            if runtimeErr.message is None:
                raise
            if runtimeErr.message.startswith("com.raytheon.viz.gfe.GFEOperationFailedException:"):
                self.statusBarMsg("There are conflicting locks.  " + \
                                  "Please resolve these before adding any hazards", "S")
                hazParm = None
            else:
                raise

        # endParmEdit() locks the grids.
        # The locks will be released when the forecast is saved.
        if hazParm is not None:
            locked = True
            if len(startTimes) != 0:
                locked = hazParm.endParmEdit()
            if locked:
                locked = hazParm.forceLockTR(timeRange.toJavaObj())
            if not locked:
                self.statusBarMsg("There are conflicting locks.  " + \
                                  "Please resolve these before adding any hazards", "S")
                hazParm = None
        
        return (hazParm, gridData)

    ##
    # Let other users edit the hazards parm.
    #
    # @return: True for success, False otherwise.
    # @raise JepException: if the hazards parm was not being edited.
    def _endEdit(self):
        "Let other users edit the hazards parm. Return True for success."
        hazParm = self.getParm(MODEL, ELEMENT, LEVEL)
        return hazParm.endParmEdit()
        
    ##
    # Make temporary hazard grids for each hazard subkey.
    # Hazards are "being edited" until they are merged again.
    #
    # @return: True if separation succeeded, false otherwise.
    #
    def _separateHazardGrids(self):
        "Make temporary hazard grids for each hazard subkey."

        # if any temp hazard grids are loaded, don't separate again
        if self._tempWELoaded():
            return FAIL_REDUNDANT  #already separated
        
        hazParm, gridData = self._lockHazards()
        if hazParm is None:
            return FAIL_LOCK # unavailable

        # get a collection of distinct Java TimeRange objects
        trSet = set()
        for gd in gridData:
            trSet.add(gd.getGridTime())

        # Create a set of temporary weather element names
        weNameSet = set()
        
        for tr in trSet:
            # Get the index grid and key list for the real Hazards element
            byteGrid, hazKey = self.getGrids(MODEL, ELEMENT, LEVEL, tr, 
                                             mode="First")
            if isinstance(hazKey, str):
                hazKey = eval(hazKey)

            # Only work with the keys that have points in the grid
            uniqueKeys = self._getUniqueKeys(byteGrid, hazKey)
            if len(uniqueKeys) > 0:
                # build list of split hazKeys for use in loop below
                splitHazKeys = []
                for haz in hazKey:
                    splitHazKeys.append(self._getSubKeys(haz))

            for uKey in uniqueKeys:

                if uKey == "<None>":
                    continue

                # split the current key into its subkeys
                subKeys = self._getSubKeys(uKey)
                for subKey in subKeys:
                    # make the temporary name
                    weName = self._makeTempWEName(subKey)

                    # make the mask - find all areas that contain the subKey
                    mask = numpy.zeros(byteGrid.shape, dtype='bool')
                    for hazIndex in range(len(hazKey)):
                        if subKey in splitHazKeys[hazIndex]:
                            mask |= (byteGrid==hazIndex)

                    # make the grid
                    self._addHazard(weName, tr, subKey, mask)
                    pytr = TimeRange(tr)
                    logmsg = " ".join(["Separate", weName,
                      self._printTime(pytr.startTime().unixTime()),
                      self._printTime(pytr.endTime().unixTime()), subKey])
                    LogStream.logEvent(logmsg)

                    # save the weNames for later
                    weNameSet.add(weName)

        # Combine time ranges for the temporary weather elements we created
        self._consolidateTimes(weNameSet)

        return SUCCESS

    ##
    # Add the specified hazard to weName over the specified timeRange
    # and spatially over the specified mask.  Combines the specified
    # hazard with the existing hazards by default.  For replaceMode,
    # specify 0 in the combineField.
    #
    # @param weName: The weather element name.
    # @type wename: string
    # @param timeRange: Time range of the hazard.
    # @type timeRange: Java or Python TimeRange
    # @param addHaz: Key for the new hazard
    # @type addHaz: string
    # @return: None
    def _addHazard(self, weName, timeRange, addHaz, mask, combine=1):
        # Python TimeRanges are easy to compare.
        # Java methods require Java TimeRanges.
        # Make sure we have one of each.
        if isinstance(timeRange, JavaWrapperClass):
            pyTimeRange = timeRange
            timeRange = timeRange.toJavaObj()
        else:
            pyTimeRange = TimeRange(timeRange)
        # refuse to make new grids that are more than one hour in the past
        if pyTimeRange.endTime().unixTime() < current().unixTime() - HOUR_SECONDS():
            msg = "skipped time range creation: %s < %s" % (pyTimeRange.endTime().string(), current().string())
            return

        # set up the inventory first
        self._setupHazardsInventory(weName, [timeRange])
        
        # get the inventory
        trList = self._getWEInventory(weName, timeRange, asJava=True)
        
        # coerce mask into a boolean array if it isn't already
        if not (isinstance(mask, numpy.ndarray) and mask.dtype==numpy.bool):
            mask = numpy.array(mask, dtype=numpy.bool)

        for tr in trList:
            # get the grid of index values and list of keys those indices select
            byteGrid, hazKey = self.getGrids(MODEL, weName, LEVEL, tr,
                                             mode="First", cache=0)
            if isinstance(hazKey, str):
                hazKey = eval(hazKey)

            # Eliminate keys that aren't in the grid from the list.
            uniqueKeys = self._getUniqueKeys(byteGrid, hazKey, mask)
            for uKey in uniqueKeys:
                # Figure out what the new key is
                if combine:
                    newKey = self._makeNewKey(uKey, addHaz)
                else:   #replace
                    newKey = addHaz

                # Find the index number for the old key
                oldIndex = self.getIndex(uKey, hazKey)
                # Find the index number for the new key (newKey is added if not in hazKey)
                newIndex = self.getIndex(newKey, hazKey)
                
                # calculate the mask - intersection of mask and oldIndex values
                editMask = (byteGrid==oldIndex) & mask
        
                # poke in the new values
                byteGrid[editMask] = newIndex

            # Save the updated byteGrid and hazKey
            if weName == ELEMENT:
                self.createGrid(MODEL, ELEMENT, "DISCRETE", (byteGrid, hazKey),
                                tr, discreteOverlap=1, discreteAuxDataLength=4)
            else:  # it's a temporary WE - special key
                hazKey = ["<None>", addHaz]
                hazKeyDesc = self._addHazardDesc(hazKey)
                self.createGrid(MODEL, weName, "DISCRETE", (byteGrid, hazKey),
                                tr, discreteOverlap=0, discreteAuxDataLength=4,
                                discreteKeys=hazKeyDesc,
                                defaultColorTable="YesNo")

        # remove any grids that are completely in the past
        self._removeOldGrids(weName)
        
        return

    ##
    # Removes the specified hazard from the specified grid over the mask.
    #
    # @param weName: Name of the weather element to remove hazards from.
    # @type weName: string
    # @param timeRange: Time range from which to remove hazards.
    # @type timeRange: Python or Java TimeRange
    # @param removeHaz: Hazard phensig to remove
    # @type removeHaz: string
    # @param mask: Grid that is True for points where removeHaz should be removed,
    #              false where it should not. Defaults to all points selected if
    #              omitted or passed as None.
    # @type mask: numpy array of boolean or Nonetype
    def _removeHazard(self, weName, timeRange, removeHaz, mask = None):
            
        # get the inventory
        trList = self._getWEInventory(weName, timeRange)

        # make sure we have a real mask
        if mask is None:
            gridShape = self._getGridSize()
            mask = numpy.ones(gridShape, bool)

        for tr in trList:
            byteGrid, hazKey = self.getGrids(MODEL, weName, LEVEL, tr,
                                             mode="First", cache=0)
            uniqueKeys = self._getUniqueKeys(byteGrid, hazKey, mask)

            for uKey in uniqueKeys:
                if string.find(uKey, removeHaz) >= 0:
                    newKey = self._removeSubKey(uKey, removeHaz)
                    oldIndex = self.getIndex(uKey, hazKey)
                    newIndex = self.getIndex(newKey, hazKey)

                    # calculate the mask - intersection of mask and oldIndex values
                    editMask = (byteGrid == oldIndex) & mask
                    
                    # poke in the new values
                    byteGrid[editMask] = newIndex

            # see if there's any hazards left and if not, delete the whole grid
            noneIndex = self.getIndex("<None>", hazKey)
            noneGrid = (byteGrid == noneIndex)
            if noneGrid.all():
                self.deleteCmd([weName], tr)
            else:
                self.createGrid(MODEL, weName, "DISCRETE", (byteGrid, hazKey),
                                tr, discreteOverlap= 0, 
                                discreteAuxDataLength=4,
                                defaultColorTable="YesNo")

        return

    ##
    # Format time as yyyymmdd_hhmm
    #
    # @param t: Time
    # @type t: seconds since the epoch
    # @return: Formatted version of t
    # @rtype: string
    def _printTime(self, t):
        gm = time.gmtime(t)
        s = time.strftime("%Y%m%d_%H%M", gm)
        return s

    #print areas, from dictionary
    def _printAreas(self, areas):
        ara = list(areas)
        ara.sort()
        return ara

    #filter vtec table based on gfe operating mode, returns vtec table
    def _filterVTECBasedOnGFEMode(self, vtecTable):
        #get gfe mode
        rawGfeMode = self.gfeOperatingMode()
        gfeMode = rawGfeMode
        if gfeMode is None:
            gfeMode = ""

        gfeMode = gfeMode.strip().lower()
        #practice mode = accept all records
        if "practice" == gfeMode:
            return vtecTable  #allow all records

        #test mode -- only accept records that have "T" vtec
        elif "test" == gfeMode:
            fvtecTable = []
            for rec in vtecTable:
                testEntry = (rec['vtecstr'].find('/T.') == 0)
                if testEntry:
                    fvtecTable.append(rec)
            return fvtecTable
                
        #regular/operational mode -- accept records that don't have "T" vtec
        elif "standard" == gfeMode or "operational" == gfeMode:
            fvtecTable = []
            for rec in vtecTable:
                testEntry = (rec['vtecstr'].find('/T.') == 0)
                if not testEntry:
                    fvtecTable.append(rec)
            return fvtecTable

        else:
            raise Exception, "Unknown GFE operating mode: " + rawGfeMode

    ##
    # A Python access to the looseLocks() method of the Hazards parm.
    def _unlockHazards(self):
        hazParm = self.getParm(MODEL, ELEMENT, LEVEL)
        hazParm.looseLocks()
