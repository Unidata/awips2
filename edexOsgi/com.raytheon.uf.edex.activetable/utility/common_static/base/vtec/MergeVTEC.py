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

# MergeVTEC - merges two "active" tables together.
# Originally written by Mark Mathewson FSL

#
# Port of MergeVTEC code from AWIPS1
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/25/13        1447          dgilling       Initial Creation.
#    03/19/13        1447          dgilling       Merge A1 DR 21434.
#    06/11/13        #2083         randerso       Move backups to edex_static
#    01/24/14        #2504         randerso       change to use iscUtil.getLogger for consistency 
# 
#


import copy
import cPickle
import gzip
import os
import time

import iscUtil
import ActiveTableRecord
import siteConfig
import VTECPartners
import VTECTableSqueeze
import VTECTableUtil

from java.util import ArrayList
from com.raytheon.uf.common.activetable import MergeResult
from com.raytheon.uf.common.activetable import VTECChange
from com.raytheon.uf.common.localization import PathManagerFactory
from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType
from com.raytheon.uf.common.site import SiteMap


class MergeVTEC(VTECTableUtil.VTECTableUtil):

    def __init__(self, activeTable, activeTableMode, newRecords, offsetSecs=0.0, 
      makeBackups=True, logger=None):
        # activeTable - current activeTable records
        # activeTableMode - which table is being modified--OPERATIONAL or PRACTICE
        # newRecords - records to merge in to activeTable
        # inputIsGZIP (0,1) - remote input file is gzipped
        # offsetSecs - Number of seconds +/- current time
        # makeBackups (False, True) - make backups of previous table
        # logger - python logging object to send all logs to
        if logger is not None:
            self._log = logger
        else:
            self._log = self.__initLogging()

        # create a dummy name to simplify the file access code in VTECTableUtil
        pathMgr = PathManagerFactory.getPathManager()
        edexSiteCx = pathMgr.getContextForSite(
                        LocalizationType.EDEX_STATIC, siteConfig.GFESUITE_SITEID)
        filePath = pathMgr.getFile(edexSiteCx,"vtec").getPath()
        fileName = os.path.join(filePath, activeTableMode + ".tbl")
        
        # to ensure time calls are based on Zulu
        os.environ['TZ'] = "GMT0"
        self._time = time.time() + offsetSecs   #present time
        
        self._makeBackups = makeBackups
        
        VTECTableUtil.VTECTableUtil.__init__(self, fileName)

        # get the SPC site id from the configuration file
        self._spcSite = getattr(VTECPartners, "VTEC_SPC_SITE", "KWNS")
        self._tpcSite = getattr(VTECPartners, "VTEC_TPC_SITE", "KNHC")

        # get our site
        siteid = siteConfig.GFESUITE_SITEID
        self._ourSite = self._get4ID(siteid)
                                                                                
        self._siteFilter = self._getFilterSites()

        self._log.info("MergeVTEC Starting")
        self._log.info("localFN= " + self._activeTableFilename + " sites= " 
                       + repr(self._siteFilter))

        #read table to merge
        otherTable = newRecords
        self._log.info("Remote Table size: %d", len(otherTable))

        #read active table
        self._log.info("Active Table size: %d", len(activeTable))

        #save a copy for later backup purposes
        oldActiveTable = copy.deepcopy(activeTable)

        #delete "obsolete" records from our table and the other table
        vts = VTECTableSqueeze.VTECTableSqueeze(self._time)
        activeTable, tossRecords = vts.squeeze(activeTable)
        self._log.info("Active Table squeezed size: %d", len(activeTable))
        self._log.info("Other Table size: %d", len(otherTable))
        otherTable, tossRecordsOther = vts.squeeze(otherTable)
        self._log.info("Other Table squeezed size: %d", len(otherTable))

        #merge the tables
        updatedTable, toDelete, changes = self._mergeTable(activeTable, otherTable)
        self._log.info("Updated Active Table size: %d", len(updatedTable))
        updatedTable, tossRecordsMerged = vts.squeeze(updatedTable)
        self._log.info("Updated Active Table squeeze size: %d", 
          len(updatedTable))
        del vts

        self._updatedTable = []
        self._purgedTable = []
        self._changes = []
        
        #notify the ifpServer of changes, save a backup copy
        if tossRecords or tossRecordsMerged or changes:
            self._log.debug("#tossRecords: %d", len(tossRecords))
            self._log.debug("#tossRecordsMerged: %d", len(tossRecordsMerged))
            self._log.debug("#changes: %d", len(changes))
            
            # save lists for later retrieval
            self._updatedTable = updatedTable
            self._purgedTable.extend(tossRecords)
            self._purgedTable.extend(toDelete)
            self._purgedTable.extend([rec for rec in tossRecordsMerged if rec in oldActiveTable])
            self._changes = changes

            #save backup copy
            if self._makeBackups:
                oldActiveTable = self._convertTableToPurePython(oldActiveTable, siteid)
                self.saveOldActiveTable(oldActiveTable)
                pTime = getattr(VTECPartners, "VTEC_BACKUP_TABLE_PURGE_TIME", 
                  168)
                self.purgeOldSavedTables(pTime)

        self._log.info("MergeVTEC Finished")

    # merges the active and other table together and returns the merged
    # table along with the list of changes that occurred.
    def _mergeTable(self, activeTable, otherTable):
        changes = []
        purges = []
        compare = ['id', 'phen', 'sig', 'officeid', 'etn', 'pil']
        compare1 = ['phen', 'sig', 'officeid']
        compare2 = ['officeid', 'pil', 'phen', 'sig', 'etn']
        missingEntriesPast = []
        missingEntriesAct = []
        newReplaceEntriesPast = []
        oldReplaceEntriesPast = []
        newReplaceEntriesAct = []
        oldReplaceEntriesAct = []
        ignoredNewReplaceAct = []
        ignoredOldReplaceAct = []

        currentYear = time.gmtime(self._time)[0]
        terminations = ('CAN', 'EXP', 'UPG')
        
        # Remove all records from the received table for events that
        # have been cancelled and compacted in our active table
        for rec in activeTable:
            if rec['act'] not in terminations:
                continue
            
            recYear = time.gmtime(rec['issueTime'])[0]

            # check if there are other related records
            single = True
            for rec2 in activeTable:
                if self.hazardCompare(rec2, rec, compare2):
                    rec2Year = time.gmtime(rec2['issueTime'])[0]
                    if recYear == rec2Year and rec != rec2:
                        single = False
                        break
            
            if single:
                # remove all records for this event from the received table
                for othRec in otherTable[::-1]:
                    if self.hazardCompare(rec, othRec, compare2):
                        othRecYear = time.gmtime(othRec['issueTime'])[0]
                        if othRecYear == recYear:
                            otherTable.remove(othRec)


        # we process each entry in the other (received) table
        for othRec in otherTable:
            # filter out all other sites we aren't interested in
            if self._siteFilter is not None and \
              othRec['officeid'] not in self._siteFilter:
                continue

            # filter out ROU and COR codes
            if othRec['act'] in ["ROU","COR"]:
                continue

            othRecYear = time.gmtime(othRec['issueTime'])[0]

            # if the remote table has a single canceled record, 
            # copy the record if needed and remove the rest for the event

            canceled = othRec['act'] in terminations
            if canceled:
                # determine if the remote table has a single record
                for othRec2 in otherTable:
                    if self.hazardCompare(othRec2, othRec, compare2):
                       recYear = time.gmtime(othRec2['issueTime'])[0]
                       if recYear == othRecYear and othRec2['id'] != othRec['id']:
                           canceled = False
                           break

            if canceled:
                # find all the record in our active table for this event
                matches = []
                for i, rec in enumerate(activeTable):
                    if self.hazardCompare(rec, othRec, compare2):
                        atRecYear = time.gmtime(rec['issueTime'])[0]
                        if othRecYear == atRecYear:
                            matches.append(i)

                changed = False

                found = False 
                matches.reverse()
                for i in matches:
                    rec = activeTable[i]
                    if rec['id'] == othRec['id']:
                        found = True
                        # replace record if not the same
                        if rec != othRec:
                            newReplaceEntriesAct.append(othRec)
                            oldReplaceEntriesAct.append(rec)
                            activeTable[i] = othRec
                            changed = True
                    else:
                        # remove other records for this event
                        oldReplaceEntriesAct.append(rec)
                        del activeTable[i]
                        changed = True

                if not found:
                    # add the remote record
                    missingEntriesAct.append(othRec)
                    activeTable.append(othRec)
                    changed = True

                if changed:
                    chgRec = (othRec['officeid'], othRec['pil'], othRec['phensig'])
                    if chgRec not in changes:
                        changes.append(chgRec)

            # currently active events
            elif othRec['endTime'] >= self._time:

                # find a match in otherTable that is in our active table
                # and replace it if newer, but only if it is from the same
                # issuance year.
                found = 0
                for i in xrange(len(activeTable)):
                    if self.hazardCompare(activeTable[i], othRec, compare):
                       found = 1
                       atRecYear = time.gmtime(activeTable[i]['issueTime'])[0]
                       if othRec['issueTime'] > activeTable[i]['issueTime']:
                           if othRecYear == atRecYear:
                               newReplaceEntriesAct.append(othRec)
                               oldReplaceEntriesAct.append(activeTable[i])
                               activeTable[i] = othRec  #replace the record
                               chgRec = (activeTable[i]['officeid'], 
                                 activeTable[i]['pil'], activeTable[i]['phensig'])
                               if chgRec not in changes:
                                   changes.append(chgRec)
                           else:
                               ignoredNewReplaceAct.append(othRec)
                               ignoredOldReplaceAct.append(activeTable[i])
                       break
    
                # if a match wasn't found, then we may need to add the record
                # into our active table
                if found == 0:
                    missingEntriesAct.append(othRec)
                    activeTable.append(othRec)   #add the record
                    chgRec = (othRec['officeid'], othRec['pil'], othRec['phensig'])
                    if chgRec not in changes:
                        changes.append(chgRec)

            # past events 
            else:

                othRecYear = time.gmtime(othRec['issueTime'])[0]
                if currentYear != othRecYear:
                    continue   #only care about this year

                # find the highest ETN for the current year per phen/sig
                # in active table and compare to the other table. If found
                # higher 'remote' record, replace the record.  
                maxETN = None
                maxETNIndex = None
                for i in xrange(len(activeTable)):
                    a  = activeTable[i]
                    if self.hazardCompare(a, othRec, compare1) and \
                      time.gmtime(a['issueTime'])[0] == currentYear:
                        if maxETN is None or a['etn'] > maxETN:
                            maxETN = a['etn']   #save maxETN
                            maxETNIndex = i     #save the index
                            
                if maxETN is not None and othRec['etn'] > maxETN:
                    newReplaceEntriesPast.append(othRec)
                    oldReplaceEntriesPast.append(activeTable[maxETNIndex])
                    activeTable[maxETNIndex] = othRec #replace record
                    chgRec = (othRec['officeid'], othRec['pil'], othRec['phensig'])
                    if chgRec not in changes:
                        changes.append(chgRec)

                #if phen/sig not found, then add it
                if maxETN is None:
                    missingEntriesPast.append(othRec)
                    activeTable.append(othRec) #add the record
                    chgRec = (othRec['officeid'], othRec['pil'], othRec['phensig'])
                    if chgRec not in changes:
                        changes.append(chgRec)
                    
        # log the changes
        if len(missingEntriesAct):
            self._log.debug("Active Missing entries added: %s",
              self.printActiveTable(missingEntriesAct, 1))
        if len(newReplaceEntriesAct):
            self._log.debug("Active Replacement entries (new): %s",
              self.printActiveTable(newReplaceEntriesAct, 1))
        if len(oldReplaceEntriesAct):
            self._log.debug("Active Entries Replaced (old): %s",
              self.printActiveTable(oldReplaceEntriesAct, 1))
        if len(missingEntriesPast):
            self._log.debug("Past Missing entries added %s",
              self.printActiveTable(missingEntriesPast, 1))
        if len(newReplaceEntriesPast):
            self._log.debug("Past Replacement entries (new): %s",
              self.printActiveTable(newReplaceEntriesPast, 1))
        if len(oldReplaceEntriesPast):
            self._log.debug("Past Entries Replaced (old): %s",
              self.printActiveTable(oldReplaceEntriesPast, 1))
        if len(ignoredNewReplaceAct):
            self._log.debug("Ignored Different Year Issuance (new): %s",
              self.printActiveTable(ignoredNewReplaceAct, 1))
            self._log.debug("Ignored Different Year Issuance (old): %s",
              self.printActiveTable(ignoredOldReplaceAct, 1))
        self._log.debug("Table Changes: %s", changes)
        
        purges.extend(oldReplaceEntriesAct)
        purges.extend(oldReplaceEntriesPast)
        return activeTable, purges, changes
    
    def getMergeResults(self):
        if not self._updatedTable and not self._purgedTable and not self._changes:
            return None
        
        updatedList = ArrayList()
        for rec in self._updatedTable:
            updatedList.add(rec.javaRecord())
    
        purgedList = ArrayList()
        for rec in self._purgedTable:
            purgedList.add(rec.javaRecord())
    
        changeList = ArrayList()
        for c in self._changes:
            changeList.add(VTECChange(c[0],c[1],c[2]))

        result = MergeResult(updatedList, purgedList, changeList)
        return result

    def _getFilterSites(self):
        #gets the list of filter sites, which is the list specified, plus
        #SPC plus our own site.  Returns None for no-filtering.
        sites = getattr(VTECPartners, "VTEC_MERGE_SITES", [])
        if sites is None:
            return None
        sites.append(self._spcSite)
        sites.append(self._tpcSite)
        sites.append(self._ourSite)
        self._log.debug("Filter Sites: %s", sites)
        return sites

    #convert 3 letter to 4 letter site ids
    def _get4ID(self, id):
        return SiteMap.getInstance().getSite4LetterId(id)

    def __initLogging(self):
        import logging
        return iscUtil.getLogger("MergeVTEC", logLevel=logging.INFO)

def merge(activeTable, activeTableMode, newRecords, drt=0.0, makeBackups=True,
      logger=None):
    pyActive = []
    for i in range(activeTable.size()):
        pyActive.append(ActiveTableRecord.ActiveTableRecord(activeTable.get(i)))
    
    pyNew = []
    for i in range(newRecords.size()):
        pyNew.append(ActiveTableRecord.ActiveTableRecord(newRecords.get(i)))
    
    decoder = MergeVTEC(pyActive, activeTableMode, pyNew, drt, makeBackups, logger)
    mergeResults = decoder.getMergeResults()
    decoder = None
    
    return mergeResults

