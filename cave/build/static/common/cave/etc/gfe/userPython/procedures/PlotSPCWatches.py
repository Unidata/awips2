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
# PlotSPCWatches
#
# This procedure synchonizes the hazards from SPC that are in the active table.
#
#
# Author: lefebvre
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Hazards"]

from numpy import *
import SmartScript
import time
import HazardUtils
import logging
import UFStatusHandler
import JUtil
from java.io import File
from java.lang import System

PLUGIN_NAME = 'com.raytheon.viz.gfe'
CATEGORY = 'GFE'

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        self._dbss = dbss
 
        logging.basicConfig(level=logging.INFO)
        self.log = logging.getLogger("PlotSPCWatches")
        
        self.log.addHandler(UFStatusHandler.UFStatusHandler(PLUGIN_NAME, CATEGORY, level=logging.WARNING))    
    
    def getWatches(self):
        nonSPCRecords = []
        spcRecords = []
        spcCANRecords = []
        areas = []
        marineAreas = []

        vtecTable = self.vtecActiveTable()
        vtecTable = self._hazUtils._filterVTECBasedOnGFEMode(vtecTable)

        phenSigList = ["TO.A", "SV.A"]
        pilList = ['WOU','WCN']
        spcActions = ['NEW','CON','EXT','EXA','EXB']
        othActions = ['NEW','CON','EXT','EXA','EXB','CAN','EXP','UPG']
        spcActionsCAN = ['CAN']

        # Get the local WFO id
        siteId = self._dbss.getSiteID()

        # step 1:  Separate into SPC/nonSPC/spcCAN, keep only certain actions
        for v in vtecTable:
            # filter based on phen/sig
            phenSig = v['phen'] + "." + v['sig']
            if not phenSig in phenSigList:
                continue

            # eliminate non-interesting products
            if v['pil'] not in pilList:
                continue
            
            # eliminate records in the past
            if v['endTime'] < self._gmtime().unixTime():
                continue

            # add to appropriate list
            if v['officeid'] == 'KWNS':
                if v['act'] in spcActions:
                    spcRecords.append(v)
                elif v['act'] in spcActionsCAN:
                    spcCANRecords.append(v)
            else:
                if v['act'] in othActions:
                    nonSPCRecords.append(v)
 
        #LogStream.logUse("step1 PlotSPCWatches: spcRec=", spcRecords,
        #  "\nspcCANRecords=", spcCANRecords, "\n nonSPCRec=", nonSPCRecords)

        # step 2: eliminate records in SPC that are also in non-SPC. Filter
        # based on etn, id, phen/sig. Ignore action, i.e., CAN in the nonSPC
        # record will override any action in SPC records.  Remaining records
        # will be the "NEW" watch.
        compare = ['etn','id','phen','sig']
        filteredSPCWatches = []
        
        for spcRec in spcRecords:
            removeRecord = 0
            for nonSPCRec in nonSPCRecords:
                if self._recordCompare(spcRec, nonSPCRec, compare):
                    removeRecord = 1  #match found in nonSPCRecord
                    break
            if not removeRecord:
                filteredSPCWatches.append(spcRec)

        #LogStream.logUse("step2 PlotSPCWatches: elim SPC in nonSPC. ",
          #"spcRec=", filteredSPCWatches)

        # step 3: eliminate records in non-SPC that are CAN, EXP
        eliminateActions = ['CAN','EXP']
        filteredNonSPCWatches = []
        for nonSPCRec in nonSPCRecords:
            if nonSPCRec['act'] not in eliminateActions:
                filteredNonSPCWatches.append(nonSPCRec)

        #LogStream.logUse("step3 PlotSPCWatches: elim nonSPC CANEXP. ",
        #  "nonSPCRec=", filteredSPCWatches)

        # step 4: combine the two data sets, now we have both the new
        # watches and the old watches (still in effect) in the same
        # table.
        watchTable = filteredNonSPCWatches
        watchTable.extend(filteredSPCWatches)

        #LogStream.logUse("step4 PlotSPCWatches: combine nonSPC SPC: ",
          #watchTable)

        # step 5: Looking at the spcCANrecords, eliminate any records
        # in the watchTable that have a matching CAN. This will be records
        # from WCNs that are "active", but now SPC has "CAN" the watch.
        tmp = []
        compare = ['etn','id','phen','sig']
        for r in watchTable:
            removeRecord = 0
            for s in spcCANRecords:
                if self._recordCompare(r, s, compare):
                    removeRecord = 1  #match fround in nonSPCRecord
                    break
            if not removeRecord:
                tmp.append(r)
        watchTable = tmp

        #LogStream.logUse("step5 PlotSPCWatches: remove active in nonSPC that ",
          #" are CAN by SPC", watchTable)
        



        # step 6: eliminate overlapping watches. Can't have multiple watches
        # in the same zone.  Also trim down the start time
        zoneDict = self._convertToZoneDict(watchTable)

        zones = zoneDict.keys()
    
        for zone in zones:
            watch = self._removeSupersededWatches(zoneDict[zone])
            watch['startTime'] = int(watch['startTime'] / 3600) * 3600
            zoneDict[zone] = watch

        #LogStream.logUse("step6 PlotSPCWatches: elim overlap: ",
          #watchTable)

        return zoneDict

    # compares two dictionary records for equality
    def _recordCompare(self, rec1, rec2, fields):
        #Compares two records for equality, based on the fields given.
        #Records are dictionaries.  Fields are assumed to exist in both recs.
        for f in fields:
            if rec1[f] != rec2[f]:
                return 0
        return 1


    def removeAllWatches(self):
        # remove all SV.A and TO.A grids from the Hazards inventory
        trList = self._hazUtils._getWEInventory("Hazards")
        for tr in trList:
            byteGrid, hazKey = self.getGrids("Fcst", "Hazards", "SFC", tr,
                                             mode="First", cache=0)
            uniqueKeys = self._hazUtils._getUniqueKeys(byteGrid, hazKey)
            for uKey in uniqueKeys:
                subKeys = self._hazUtils._getSubKeys(uKey)
                if subKeys is not None:
                    for subKey in subKeys:
                        phen = self._hazUtils._keyPhen(subKey)
                        if phen in ["SV", "TO"]:
                            self._hazUtils._removeHazard("Hazards", tr, subKey)

    def _convertToZoneDict(self, watchTable):
        #returns a dictionary organized by zone for each hazard
        hazardsByZone = {}
        for h in watchTable:
            if hazardsByZone.has_key(h['id']):
                hazardsByZone[h['id']].append(h)
            else:
                hazardsByZone[h['id']] = [h]
        return hazardsByZone


    def _removeSupersededWatches(self, zoneRecords):
        # looks for multiple watches in the same zone, eliminates the
        # lower etn (older) version.  Returns a single record.

        # nothing needs calculating
        if len(zoneRecords) == 1:
            return zoneRecords[0]

        # TEST watches are etns >= 9000, eliminate test watches if there
        # are any real watches
        testWatches = 0  #etn >= 9000
        normalWatches = 0  #etn < 9000
        for zr in zoneRecords:
            if zr['etn'] >= 9000:
                testWatches = 1
            else:
                normalWatches = 0
        if normalWatches:
            list = []
            for zr in zoneRecords:
                if zr['etn'] < 9000:
                    list.append(zr)
            zoneRecords = list

        # any left?
        if len(zoneRecords) == 1:
            return zoneRecords[0]

        # find the higher watch etn for this year
        watch = zoneRecords[0]   #final choice
        for index in xrange(1, len(zoneRecords)):
            recYear = time.gmtime(zoneRecords[index]['issueTime'])[0]
            watchYear = time.gmtime(watch['issueTime'])[0]  
            if recYear > watchYear or (recYear == watchYear and \
                   zoneRecords[index]['etn'] > watch['etn']):
                watch = zoneRecords[index]  #higher watch
        return watch 
        
        
    def execute(self):
        # get the hazard utilities
        self._hazUtils = HazardUtils.HazardUtils(self._dbss, None)

        self.setToolType("numeric")

        # see if the Hazards WE is loaded in the GFE, if not abort the tool
        if not self._hazUtils._hazardsLoaded():
            self.log.warning("Hazards Weather Element must be loaded in the GFE before running PlotSPCWatches.")
            self.cancel()


        # if there are any temp grids loaded, refuse to run
        if self._hazUtils._tempWELoaded():
            self.log.warning("There are temporary hazard grids loaded. " +\
                     "Please merge all hazards grids before running PlotSPCWatches.")
            self.cancel()
            
        if self.lockedByOther('Hazards', 'SFC'):
            self.log.warning("There are conflicting locks (red locks - owned by others) on Hazards.  " + \
                "Please resolve these before running PlotSPCWatches")
            self.cancel()

        watchTable = self.getWatches()
        self.removeAllWatches()
        
        sumMakeTime = 0.0
        sumMakeMask = 0.0
        sumAddHazard = 0.0
        wtv = watchTable.values()
        
        if len(wtv) > 0:
            def sortkey(x):
                key = x['phen'] + x['sig'] + str(x['etn']) + \
                      str(self._hazUtils._makeTimeRange(x['startTime'], x['endTime'])) + \
                      x['id']               
                return key
            
            wtv.sort(key = sortkey)
            #for zone in watchTable.keys():
            #    zh = watchTable[zone]
            prevKey = None
            prevStart = None
            prevEnd = None
            ids = []
            for zh in wtv:
                key = zh['phen'] + '.' + zh['sig'] + ":" + str(zh['etn'])
                if key != prevKey or zh['startTime'] != prevStart or zh['endTime'] != prevEnd:
                    # new alert
                    if len(ids) > 0:
                        zoneMask = self._hazUtils._makeMask(ids)
                        timeRange = self._hazUtils._makeTimeRange(prevStart, prevEnd)
                        self._hazUtils._addHazard("Hazards", timeRange, prevKey, zoneMask)
                    ids = []
                    prevKey = key
                    prevStart = zh['startTime']
                    prevEnd = zh['endTime']
                ids.append(zh['id'])
                    
            # handle the last zones
            zoneMask = self._hazUtils._makeMask(ids)
            timeRange = self._hazUtils._makeTimeRange(prevStart, prevEnd)
            self._hazUtils._addHazard("Hazards", timeRange, prevKey, zoneMask)
#            LogStream.logEvent(self._hazUtils._printTime(zh['startTime']),
#              self._hazUtils._printTime(zh['endTime']),
#              key, zh['id'])

        return
