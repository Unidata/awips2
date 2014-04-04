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
#
# Code mostly separated from legacy VTECDecoder.py
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/11/13        #2083         randerso       Log active table changes, save backups
#    03/06/14        #2883         randerso       Pass siteId into mergeFromJava
#    03/25/14        #2884         randerso       Added xxxid to VTECChange
#

import time
import copy
import os
import VTECTableUtil, VTECTableSqueeze, VTECPartners
import LogStream, ActiveTableVtec, ActiveTableRecord
from java.util import ArrayList
from com.raytheon.uf.common.localization import PathManagerFactory
from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType
from com.raytheon.uf.common.localization import LocalizationContext_LocalizationLevel as LocalizationLevel

class ActiveTable(VTECTableUtil.VTECTableUtil):
    
    def __init__(self, activeTableMode):
        self._time = time.time()

        # create a dummy name to simplify the file access code in VTECTableUtil
        pathMgr = PathManagerFactory.getPathManager()
        edexSiteCx = pathMgr.getContext(LocalizationType.EDEX_STATIC, LocalizationLevel.SITE)
        filePath = pathMgr.getFile(edexSiteCx,"vtec").getPath()
        VTECTableUtil.VTECTableUtil.__init__(self, os.path.join(filePath, activeTableMode + ".tbl"))

    def updateActiveTable(self, activeTable, newRecords, offsetSecs=0):
        #merges the previous active table and new records into a new table.
        #Returns:
        #  (updated active table, purged records, changesforNotify, changeFlag)
        updatedTable = []
        changes = []
        changedFlag = False

        #delete "obsolete" records from the old table.
        vts = VTECTableSqueeze.VTECTableSqueeze(self._time+offsetSecs)
        activeTable, tossRecords = vts.squeeze(activeTable)
        for r in tossRecords:
            r['state'] = "Purged"
        del vts
        if len(tossRecords):
            changedFlag = True

        #expand out any 000 UGC codes, such as FLC000, to indicate all
        #zones.
        newRecExpanded = []
        compare1 = ['phen', 'sig', 'officeid', 'etn', 'pil']
        for newR in newRecords:
            if newR['id'][3:6] == "000":
                for oldR in activeTable:
                    if self.hazardCompare(oldR, newR, compare1) and \
                      oldR['id'][0:2] == newR['id'][0:2] and \
                      (oldR['act'] not in ['EXP', 'CAN', 'UPG'] or \
                       oldR['act'] == 'EXP' and oldR['endTime'] > newR['issueTime']):
                        newE = copy.deepcopy(newR)
                        newE['id'] = oldR['id']
                        newRecExpanded.append(newE)
            else:
                newRecExpanded.append(newR)
        newRecords = newRecExpanded

        # match new records with old records, with issue time is different
        # years and event times overlap. Want to reassign ongoing events
        # from last year's issueTime to be 12/31/2359z, rather than the
        # real issuetime (which is this year's).        
        compare = ['phen', 'sig', 'officeid', 'pil', 'etn']
        for newR in newRecords:
            cyear = time.gmtime(newR['issueTime'])[0]  #current year issuance time
            lastYearIssueTime = time.mktime((cyear-1, 12, 31, 23, 59,
                                                0, -1, -1, -1))
            for oldR in activeTable:
                if self.hazardCompare(oldR, newR, compare):
                  oldYear = time.gmtime(oldR['issueTime'])[0]
                  newYear = time.gmtime(newR['issueTime'])[0]
                  if oldYear < newYear:
                      if (newR['act'] == "EXP" and newR['endTime'] == oldR['endTime']) or \
                        self.__overlaps((oldR['startTime'],oldR['endTime']), (newR['startTime'],newR['endTime'])):
                          LogStream.logVerbose("Reset issuance time to last year:",
                            "\nNewRec: ", self.printEntry(newR),
                            "OldRec: ", self.printEntry(oldR))
                          newR['issueTime'] = lastYearIssueTime


        # split records out by issuance year for processing
        newRecDict = {}   #key is issuance year
        oldRecDict = {}
        years = []
        for newR in newRecords:
            issueYear = time.gmtime(newR['issueTime'])[0]
            records = newRecDict.get(issueYear, [])
            records.append(newR)
            newRecDict[issueYear] = records
            if issueYear not in years:
                years.append(issueYear)
        for oldR in activeTable:
            issueYear = time.gmtime(oldR['issueTime'])[0]
            records = oldRecDict.get(issueYear, [])
            records.append(oldR)
            oldRecDict[issueYear] = records
            if issueYear not in years:
                years.append(issueYear)
    
        # process each year
        compare = ['id', 'phen', 'sig', 'officeid', 'pil']

        for year in years:
            newRecords = newRecDict.get(year,[])
            oldRecords = oldRecDict.get(year,[])

            # now process the old and new records
            for oldR in oldRecords:

                keepflag = 1
                for newR in newRecords:

                    if newR['act'] == "ROU":
                        continue

                    if self.hazardCompare(oldR, newR, compare):
                        #we don't keep older records with same etns
                        if newR['etn'] == oldR['etn']:
                            keepflag = 0   #don't bother keeping this record
                            break

                        #higher etns
                        elif newR['etn'] > oldR['etn']:
                            #only keep older etns if end time hasn't passed
                            #or old record is UFN and CAN:
                            ufn = oldR.get('ufn', 0)
                            if self._time > oldR['endTime'] or \
                              (oldR['act'] == "CAN" and ufn) or \
                              oldR['act'] in ['EXP','UPG','CAN']:
                                keepflag = 0
                                break

                        #lower etns, ignore (keep processing)

                if keepflag == 0:
                    oldR['state'] = "Replaced"
                    changedFlag = True
                updatedTable.append(oldR)

        #always add in the new records (except for ROU)
        compare = ['id', 'phen', 'sig', 'officeid', 'pil', 'etn']
        for year in newRecDict.keys():
            newRecords = newRecDict[year]
            for newR in newRecords:
                if newR['act'] != "ROU":

                    #for COR, we need to find the original action, and 
                    #substitute it.
                    if newR['act'] == "COR":
                        for rec in updatedTable:
                            if self.hazardCompare(rec, newR, compare):
                                LogStream.logVerbose(\
                                  "COR record matched with:",
                                  "\nNewRec: ", self.printEntry(newR),
                                  "OldRec: ", self.printEntry(rec),
                                  "\nReassign action to: ", rec['act'])
                                newR['act'] = rec['act']
                                break
                        #due to above code, this should never execute
                        if newR['act'] == "COR":
                            LogStream.logProblem("COR match not found for:\n",
                              self.printEntry(newR), "\nRecord discarded.")

                    if newR['act'] != "COR":
                        updatedTable.append(newR)
                        changedFlag = True

                        #determine changes for notifications
                        rec = (newR['officeid'], newR['pil'], newR['phensig'], newR['xxxid'])
                        if rec not in changes:
                            changes.append(rec)

        #filter out any captured text and overviewText if not in the categories
        cats = self._getTextCaptureCategories()
        if cats is not None:
            for rec in updatedTable:
                if rec['pil'] not in cats:
                    if rec.has_key('segText'):
                        del rec['segText']
                    if rec.has_key('overviewText'):
                        del rec['overviewText']

        return updatedTable, tossRecords, changes, changedFlag

    # time overlaps, if tr1 overlaps tr2 (adjacent is not an overlap)
    def __overlaps(self, tr1, tr2):
        if self.__containsT(tr2, tr1[0]) or self.__containsT(tr1, tr2[0]):
            return 1
        return 0
    
    def __containsT(self, tr, t):
        return (t >= tr[0] and t < tr[1])
    
    def _getTextCaptureCategories(self):
        #gets the list of product categories that need their text captured.
        #if the list is empty, then all products are captured into the
        #active table and None is returned.
        cats = getattr(VTECPartners, "VTEC_CAPTURE_TEXT_CATEGORIES", [])
        if len(cats) == 0:
            return None
        LogStream.logDebug("Text Capture Categories: ", cats)
        return cats
    
    def activeTableMerge(self, activeTable, newRecords, offsetSecs=0):
        #add in the Previous state
        for r in activeTable:
            r['state'] = "Previous"
        
        updatedTable, purgedRecords, changes, changedFlag = self.updateActiveTable(activeTable, newRecords, offsetSecs)
        
         #strip out the "state" field
        outTable = []        
        for r in updatedTable:
            if r['state'] not in ["Replaced", "Purged"]:
                del r['state']
                outTable.append(r)
            else:
                purgedRecords.append(r)
                
        return outTable, purgedRecords, changes, changedFlag

def mergeFromJava(siteId, activeTable, newRecords, logger, mode, offsetSecs=0):
    pyActive = []
    szActive = activeTable.size()
    for i in range(szActive):
        pyActive.append(ActiveTableRecord.ActiveTableRecord(activeTable.get(i)))
    
    decoderSites = VTECPartners.VTEC_DECODER_SITES
    decoderSites.append(VTECPartners.get4ID(siteId))
    decoderSites.append(VTECPartners.VTEC_SPC_SITE)
    decoderSites.append(VTECPartners.VTEC_TPC_SITE)
    
    backup = False
    pyNew = []
    szNew = newRecords.size()
    for i in range(szNew):
        rec = ActiveTableRecord.ActiveTableRecord(newRecords.get(i))
        if rec['officeid'] in decoderSites:
            backup = True
        pyNew.append(rec)
    
    active = ActiveTable(mode)
    
    if backup:
        oldActiveTable = active._convertTableToPurePython(pyActive, siteId)
        active.saveOldActiveTable(oldActiveTable)
        pTime = getattr(VTECPartners, "VTEC_BACKUP_TABLE_PURGE_TIME",168)
        active.purgeOldSavedTables(pTime)
    
    updatedTable, purgeRecords, changes, changedFlag = active.activeTableMerge(pyActive, pyNew, offsetSecs)
    
    logger.info("Updated " + mode + " Active Table: purged\n" +
       active.printActiveTable(purgeRecords, combine=1))

    replaced  = []
    decoded = []
    other = []
    for r in updatedTable:
        if r['state'] == "Replaced":
            replaced.append(r)
        elif r['state'] == "Decoded":
            decoded.append(r)
        else:
            other.append(r)
        
    logger.info("Updated " + mode + " Active Table: replaced\n" +
       active.printActiveTable(replaced,  combine=1))
    logger.info("Updated " + mode + " Active Table: decoded\n" +
       active.printActiveTable(decoded, combine=1))

    updatedList = ArrayList()
    for x in updatedTable:
        updatedList.add(x.javaRecord())
    
    purgedList = ArrayList()
    for x in purgeRecords:
        purgedList.add(x.javaRecord())
    
    changeList = ArrayList()
    if (changedFlag):
        from com.raytheon.uf.common.activetable import VTECChange
        for c in changes:
            changeList.add(VTECChange(c[0],c[1],c[2],c[3]))

    from com.raytheon.uf.common.activetable import MergeResult
    result = MergeResult(updatedList, purgedList, changeList)
    return result
