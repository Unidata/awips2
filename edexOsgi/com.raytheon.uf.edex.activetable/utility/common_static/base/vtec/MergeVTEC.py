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
#    03/25/14        #2884         randerso       Added xxxid to VTECChange
#    05/15/14        #3157         dgilling       Support multiple TPC and SPC sites.
#    03/04/2015      #4129         randerso       Log the active table changes at info level 
#                                                 in the active table change log
#    Apr 25, 2015     4952         njensen        Updated for new JEP API
#    May 22, 2015     4522         randerso       Create proper primary key for ActiveTableRecord
#    06/21/16        #5709         dgilling       Use TropicalCycloneUtil to bin tropical storms
#                                                 when comparing records.
#    08/04/16        #5747         dgilling       Remove references to edex_static.
#    09/01/16         5872         dgilling       Fix error-handling in previous revision.
#    02/01/17         6107         dgilling       Ensure backups are written outside of localization.
#    06/09/17         6312         dgilling       Check issue times when overwriting events.
#    07/31/17        20212         bhunderm       Fix issue with activetable sharing.
#    03/28/18        20496         ryu            Fix VTEC table change computation so clients 
#                                                 are not unduly notified.
##

##
# This is a base file that is not intended to be overridden.
##



import copy
import os
import time

import iscUtil
import ActiveTableRecord
import siteConfig
import TropicalCycloneUtil
import VTECPartners
import VTECTableSqueeze
import VTECTableUtil
import JUtil

from java.util import ArrayList
from com.raytheon.uf.common.activetable import MergeResult
from com.raytheon.uf.common.activetable import VTECChange
from com.raytheon.uf.common.site import SiteMap
from com.raytheon.uf.common.activetable import VTECPartners as JavaVTECPartners
from com.raytheon.uf.edex.core import EDEXUtil


class MergeVTEC(VTECTableUtil.VTECTableUtil):

    def __init__(self, activeTable, activeTableMode, newRecords, offsetSecs=0.0, 
      makeBackups=True, logger=None, atChangeLog=None):
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

        # get our site
        siteid = siteConfig.GFESUITE_SITEID
        self._ourSite = self._get4ID(siteid)

        # create a dummy name to simplify the file access code in VTECTableUtil
        filePath = os.path.join(EDEXUtil.getEdexData(), "activetable", siteid) 
        fileName = os.path.join(filePath, activeTableMode + ".tbl")
        
        # to ensure time calls are based on Zulu
        os.environ['TZ'] = "GMT0"
        self._time = time.time() + offsetSecs   #present time
        
        self._makeBackups = makeBackups
        
        VTECTableUtil.VTECTableUtil.__init__(self, fileName)

        # get the SPC site id from the configuration file
        self._spcSite = JUtil.javaObjToPyVal(JavaVTECPartners.getInstance(siteid).getSpcSites("KWNS"))
        self._tpcSite = JUtil.javaObjToPyVal(JavaVTECPartners.getInstance(siteid).getTpcSites("KNHC"))

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
        updatedTable, toDelete, changes = self._mergeTable(activeTable, otherTable, atChangeLog)
        self._log.info("Updated Active Table size: %d", len(updatedTable))
        updatedTable, tossRecordsMerged = vts.squeeze(updatedTable)
        self._log.info("Updated Active Table squeeze size: %d", 
          len(updatedTable))
        del vts

        if atChangeLog is not None:
            atChangeLog.info("Entries tossed after merge: " +
                  self.printActiveTable(tossRecordsMerged, 1))

        # determine final changes
        finalChanges = []
        for rec in updatedTable:
            if rec['state'] != 'Existing':
                item = (rec['officeid'], rec['pil'], rec['phensig'], rec['xxxid'])
                if item not in finalChanges:
                    finalChanges.append(item)

        changes = finalChanges
        if atChangeLog is not None:
            atChangeLog.info("Table Changes: " + str(changes))

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
    def _mergeTable(self, activeTable, otherTable, atChangeLog):
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

        currentYear = time.gmtime(self._time).tm_year
        terminations = ('CAN', 'EXP', 'UPG')
        
        # Remove all records from the received table for events that
        # have been cancelled and compacted in our active table
        superseded = []
        for i, rec in enumerate(activeTable):
            if rec['act'] not in terminations:
                continue
            
            recYear = time.gmtime(rec['issueTime']).tm_year

            # check if there are other related records
            single = True
            for rec2 in activeTable:
                if self.hazardCompare(rec2, rec, compare2):
                    rec2Year = time.gmtime(rec2['issueTime']).tm_year
                    if recYear == rec2Year and rec != rec2:
                        single = False
                        break
            
            if single:
                matches = []
                for othRec in otherTable[::-1]:
                    if self.hazardCompare(rec, othRec, compare2):
                        othRecYear = time.gmtime(othRec['issueTime']).tm_year
                        if othRecYear == recYear:
                            matches.append(othRec)
                
                # we have to make a choice based on which activetable has the
                # newer record(s):
                #   1. if our local single CAN is newest: we skip all records
                #      from the other table.
                #   2. if the other table's records are all newer than our
                #      single CAN, we mark the CAN to be removed from the
                #      activetable.
                replace = any([othRec for othRec in matches if othRec['issueTime'] > rec['issueTime']])
                if replace:
                    superseded.append(i)
                else:
                    for othRec in matches:
                        otherTable.remove(othRec)
        
        for i in reversed(superseded):
            # remove other records for this event
            rec = activeTable[i]
            oldReplaceEntriesAct.append(rec)
            purges.append(rec)
            del activeTable[i]
            

        # we process each entry in the other (received) table
        for othRec in otherTable:
            # filter out all other sites we aren't interested in
            if self._siteFilter is not None and \
              othRec['officeid'] not in self._siteFilter:
                continue

            # filter out ROU and COR codes
            if othRec['act'] in ["ROU","COR"]:
                continue

            othRecYear = time.gmtime(othRec['issueTime']).tm_year

            # if the remote table has a single canceled record, 
            # copy the record if needed and remove the rest for the event

            canceled = othRec['act'] in terminations
            if canceled:
                # determine if the remote table has a single record
                for othRec2 in otherTable:
                    if self.hazardCompare(othRec2, othRec, compare2):
                       recYear = time.gmtime(othRec2['issueTime']).tm_year
                       if recYear == othRecYear and othRec2['id'] != othRec['id']:
                           canceled = False
                           break

            if canceled:
                # find all the record in our active table for this event
                matches = []
                for i, rec in enumerate(activeTable):
                    if self.hazardCompare(rec, othRec, compare2):
                        atRecYear = time.gmtime(rec['issueTime']).tm_year
                        if othRecYear == atRecYear:
                            matches.append(i)

                # only replace the records if the single CAN is newest
                replace = not any([i for i in matches if activeTable[i]['issueTime'] > othRec['issueTime']])
                
                changed = False
                found = False 
                
                if replace:
                    for i in reversed(matches):
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
                            purges.append(rec)
                            del activeTable[i]
                            changed = True
    
                    if not found:
                        # add the remote record
                        missingEntriesAct.append(othRec)
                        activeTable.append(othRec)
                        changed = True
    
                    if changed:
                        chgRec = (othRec['officeid'], othRec['pil'], othRec['phensig'], othRec['xxxid'])
                        if chgRec not in changes:
                            changes.append(chgRec)

            # currently active events
            elif othRec['endTime'] >= self._time:

                # find a match in otherTable that is in our active table
                # and replace it if newer, but only if it is from the same
                # issuance year.
                found = False
                for i, atRec in enumerate(activeTable):
                    if self.hazardCompare(atRec, othRec, compare):
                       found = True
                       atRecYear = time.gmtime(atRec['issueTime']).tm_year
                       if othRec['issueTime'] > atRec['issueTime']:
                           if othRecYear == atRecYear:
                               newReplaceEntriesAct.append(othRec)
                               oldReplaceEntriesAct.append(atRec)
                               activeTable[i] = othRec  #replace the record
                               chgRec = (atRec['officeid'], 
                                 atRec['pil'], atRec['phensig'], atRec['xxxid'])
                               if chgRec not in changes:
                                   changes.append(chgRec)
                           else:
                               ignoredNewReplaceAct.append(othRec)
                               ignoredOldReplaceAct.append(atRec)
                       break
    
                # if a match wasn't found, then we may need to add the record
                # into our active table
                if not found:
                    missingEntriesAct.append(othRec)
                    activeTable.append(othRec)   #add the record
                    chgRec = (othRec['officeid'], othRec['pil'], othRec['phensig'], othRec['xxxid'])
                    if chgRec not in changes:
                        changes.append(chgRec)

            # past events 
            else:

                othRecYear = time.gmtime(othRec['issueTime']).tm_year
                if currentYear != othRecYear:
                    continue   #only care about this year

                # find the highest ETN for the current year per phen/sig
                # in active table and compare to the other table. If found
                # higher 'remote' record, replace the record.  
                maxETN = None
                maxETNIndex = None
                for i in range(len(activeTable)):
                    a  = activeTable[i]
                    if self.hazardCompare(a, othRec, compare1) and \
                      time.gmtime(a['issueTime']).tm_year == currentYear:
                        # special case for tropical storms
                        # ensure we have the same "class" of tropical storm
                        # class is determined by ETN
                        if a['phen'] in TropicalCycloneUtil.TROPICAL_PHENS:
                            othRecBasin = None
                            aRecBasin = None
                            try:
                                othRecBasin = TropicalCycloneUtil.get_tropical_storm_basin(othRec)
                            except ValueError:
                                self._log.error("Tropical Hazard record has invalid ETN: " + self.printEntry(othRec))
                                continue
                            try:
                                aRecBasin = TropicalCycloneUtil.get_tropical_storm_basin(a)
                            except ValueError:
                                self._log.error("Tropical Hazard record has invalid ETN: " + self.printEntry(a))
                                continue
                            if not aRecBasin or not othRecBasin or aRecBasin != othRecBasin:
                                continue
                        
                        if maxETN is None or a['etn'] > maxETN:
                            maxETN = a['etn']   #save maxETN
                            maxETNIndex = i     #save the index
                            
                if maxETN is not None and othRec['etn'] > maxETN:
                    newReplaceEntriesPast.append(othRec)
                    oldReplaceEntriesPast.append(activeTable[maxETNIndex])
                    activeTable[maxETNIndex] = othRec #replace record
                    chgRec = (othRec['officeid'], othRec['pil'], othRec['phensig'], othRec['xxxid'])
                    if chgRec not in changes:
                        changes.append(chgRec)

                #if phen/sig not found, then add it
                if maxETN is None:
                    missingEntriesPast.append(othRec)
                    activeTable.append(othRec) #add the record
                    chgRec = (othRec['officeid'], othRec['pil'], othRec['phensig'], othRec['xxxid'])
                    if chgRec not in changes:
                        changes.append(chgRec)
                    
        # log the changes
        if atChangeLog is not None:
            atChangeLog.info("\n" + "*" * 80)
            if len(missingEntriesAct):
                atChangeLog.info("Active Missing entries added: " +
                  self.printActiveTable(missingEntriesAct, 1))
            if len(newReplaceEntriesAct):
                atChangeLog.info("Active Replacement entries (new): " +
                  self.printActiveTable(newReplaceEntriesAct, 1))
            if len(oldReplaceEntriesAct):
                atChangeLog.info("Active Entries Replaced (old): " +
                  self.printActiveTable(oldReplaceEntriesAct, 1))
            if len(missingEntriesPast):
                atChangeLog.info("Past Missing entries added " +
                  self.printActiveTable(missingEntriesPast, 1))
            if len(newReplaceEntriesPast):
                atChangeLog.info("Past Replacement entries (new): " +
                  self.printActiveTable(newReplaceEntriesPast, 1))
            if len(oldReplaceEntriesPast):
                atChangeLog.info("Past Entries Replaced (old): " +
                  self.printActiveTable(oldReplaceEntriesPast, 1))
            if len(ignoredNewReplaceAct):
                atChangeLog.info("Ignored Different Year Issuance (new): " +
                  self.printActiveTable(ignoredNewReplaceAct, 1))
                atChangeLog.info("Ignored Different Year Issuance (old): " +
                  self.printActiveTable(ignoredOldReplaceAct, 1))
        
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
            changeList.add(VTECChange(c[0],c[1],c[2],c[3]))

        result = MergeResult(updatedList, purgedList, changeList)
        return result

    def _getFilterSites(self):
        #gets the list of filter sites, which is the list specified, plus
        #SPC plus our own site.  Returns None for no-filtering.
        sites = getattr(VTECPartners, "VTEC_MERGE_SITES", [])
        if sites is None:
            return None
        sites.extend(self._spcSite)
        sites.extend(self._tpcSite)
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
      logger=None, atChangeLog=None):
    pyActive = []
    for i in range(activeTable.size()):
        rec = ActiveTableRecord.ActiveTableRecord(activeTable.get(i), "Existing")
        pyActive.append(rec)
    
    pyNew = []
    for i in range(newRecords.size()):
        rec = ActiveTableRecord.ActiveTableRecord(newRecords.get(i), "Incoming")
        pyNew.append(rec)
    
    decoder = MergeVTEC(pyActive, activeTableMode, pyNew, drt, makeBackups, logger, atChangeLog)
    mergeResults = decoder.getMergeResults()
    decoder = None
    
    return mergeResults

