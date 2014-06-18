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
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/17/13        #3296         randerso       Default debug to False to avoid logging overhead        
#

import os, sys, time, copy, LogStream

# This class takes a VTEC active table and eliminates unnecessary 
# records.  Records purged consist of old SPC watches, old Tropical
# events, last year's records, and extraneous records not needed for
# new event ETN determination.

class VTECTableSqueeze:

    #constructor
    def __init__(self, currentTime, debug=False):
        self.__ctime = currentTime
        self.__thisYear = time.gmtime(self.__ctime)[0]
        self.__debug = debug

    #squeezes the active table by eliminating entries that are no longer
    #needed for VTEC calculations.
    def squeeze(self, table):

        if self.__debug:
            LogStream.logDebug("************** ORIGINAL TABLE *********************")
            LogStream.logDebug(self.__printActiveTable(table))

        # modify old UFN events (in case fcstrs didn't CAN them)
        table, modTable = self.__modifyOldUFNEvents(table)
        if self.__debug:
            LogStream.logDebug("************** MOD UFN TABLE *********************")
            for old, new in modTable:
                t = [old, new]
                LogStream.logDebug(self.__printActiveTable(t))
                LogStream.logDebug("    -----------")

        # remove the national center and short fused events 
        shortWFO, shortNC, purgeT = \
          self.__removeOldNationalAndShortFusedEvents(table)
        if self.__debug:
            LogStream.logDebug("************** SHORT WFO TABLE *********************")
            LogStream.logDebug(self.__printActiveTable(shortWFO))
            LogStream.logDebug("************** SHORT NATL CENTER TABLE *************")
            LogStream.logDebug(self.__printActiveTable(shortNC))
            LogStream.logDebug("************** INITIAL PURGE TABLE *************")
            LogStream.logDebug(self.__printActiveTable(purgeT))

        # separate out the shortWFO into dictionary structure
        dict = self.__separateTable(shortWFO)

        # purge old entries with LowerETNs that aren't in effect
        shorterT, purgeT2 = self.__purgeOldEntriesWithLowerETNs(dict)
        if self.__debug:
            LogStream.logDebug("************** TRIMMED WFO TABLE ******************")
            LogStream.logDebug(self.__printActiveTable(shorterT))
            LogStream.logDebug("************** ADDITIONAL PURGE TABLE *************")
            LogStream.logDebug(self.__printActiveTable(purgeT2))

        #add in any shortNC entries to final table
        for r in shortNC:
            shorterT.append(r)

        #add in the purged entries from before
        for r in purgeT2:
            purgeT.append(r)

        if self.__debug:
            LogStream.logDebug("************** FINAL TABLE ********************")
            LogStream.logDebug(self.__printActiveTable(shorterT))

        return shorterT, purgeT


    # separates table into a series of nested dictionaries and returns the
    # dictionary.  Dictionary returned is organized by:
    # oid, phensig, issuanceYear, etn, id, and contains a sequence of records
    def __separateTable(self, table):
        d = {}
        for rec in table:
            oid = rec['officeid']
            phensig = (rec['phen'], rec['sig'])
            issuance = time.gmtime(rec['issueTime'])[0]
            etn = rec['etn']
            id = rec['id']
        
            # oid
            if not d.has_key(oid):
                d[oid] = {}
        
            #phensig
            if not d[oid].has_key(phensig):
                d[oid][phensig] = {}
        
            #issuance year
            if not d[oid][phensig].has_key(issuance):
                d[oid][phensig][issuance] = {}
        
            #etn
            if not d[oid][phensig][issuance].has_key(etn):
                d[oid][phensig][issuance][etn] = {}
        
            #ids
            if not d[oid][phensig][issuance][etn].has_key(id):
                d[oid][phensig][issuance][etn][id] = []
        
            prevRecords = d[oid][phensig][issuance][etn][id]
            prevRecords.append(rec)
    
        return d
            
    #figure out any "obsolete records" from the old table.  Obsolete
    #if there is a newer record available, regardless of action.
    #Skip over ROU codes.  Returns tuple of shortened table WFO, 
    #shortened table NC,  and purged table.
    #entries.
    def __removeOldNationalAndShortFusedEvents(self, table):
        compare = ['id', 'phen', 'sig', 'officeid', 'pil']
        convWatch=[('SV','A'), ('TO','A')]
        #tropicalPhen=['TR','TY','HU'] Removed to disable 24hour purging for OB 8.2
        tropicalPhen=[]
        #shortFused=[('FA','W'), ('FF','W'), ('FL','W'), ('MA','W'),
        #  ('SV','W'), ('TO','W'), ('TS','W')]
        shortFused = []   #no longer purge shortFused events
    
        purgeTable = []
        shortTableWFO = []
        shortTableNC = []
        for oldR in table:

            #toss any old convective watch entries that are beyond their
            #times plus an hour.  We don't need to keep around these 
            #entries for ETN determination.
            if (oldR['phen'], oldR['sig']) in convWatch and \
              self.__ctime > oldR['endTime'] + (1*3600): 
                purgeTable.append(oldR)
                continue
        
            #toss any old tropical entries that are beyond their
            #times.  Since the ending time is UFN, we have to use the
            #current time and issuance time for comparison:
            #We don't need to keep around these entries for ETN determination
            #TPC should issue these every 6 hours, so we err on the
            #safe side by saying 24 hr
            if oldR['phen'] in tropicalPhen and \
              self.__ctime > oldR['issueTime'] + (24*3600):
                purgeTable.append(oldR)
                continue
        
            #toss any short-fused warning entries that are older than 1 hour
            if (oldR['phen'], oldR['sig']) in shortFused and \
              self.__ctime > oldR['endTime'] + (1*3600):
                purgeTable.append(oldR)
                continue

            #toss any events that ended last year
            if self.__thisYear > time.gmtime(oldR['endTime'])[0]:
                purgeTable.append(oldR)
                continue
            
            #keep this record 
#            if oldR['officeid'] in ['KWNS', 'KNHC']:
            if oldR['officeid'] in ['KWNS']:
                shortTableNC.append(oldR)
            else:
                shortTableWFO.append(oldR)

        return shortTableWFO, shortTableNC, purgeTable

    # Modify UntilFurtherNotice events that are very old to be "CAN"
    # Returns list of events that were modified, and the updated table.
    def __modifyOldUFNEvents(self, table):
        modTable = []
        for x in xrange(len(table)):
            entry = table[x]

            ufn = entry.get('ufn', 0)

            #UFN events only, old active events (2 weeks old)
            if ufn and self.__ctime > entry['issueTime'] + (14*86400) and \
              entry['act'] not in ['CAN','EXP','UPG']:

                oldR = copy.deepcopy(entry)
                entry['act'] = "CAN"  #force to CANcel event
                modTable.append((oldR, entry))  #save old and mod events

        return table, modTable

    # Only keep the lowest ids from each phen/sig.  The "d" is a separated
    # out VTEC active table. All of the records in "d" end in the current
    # year (or future year).
    # d is [oid][phensig][issueYear][etn][id] dictionary of records
    def __purgeOldEntriesWithLowerETNs(self, d):
        saveRec = []
        purgeRec = []
        tropicalPhen=['TR','TY','HU']
        for o in d.keys():
            phensig = d[o].keys()
            for ps in phensig:
                issueYears = d[o][ps].keys()
                for iy in issueYears:
                    dd = d[o][ps][iy]
                    etns = dd.keys()

                    #get two maxetn for tropicalPhen (one for site created and the other
                    #for NHC created. The etn will be more than 100 for NHC hazards), 
                    #and the minimum id for this hazard
                    maxetn1 = max(etns)
                    maxetn2 = 0
                    if maxetn1 >= 1000: 
                       for my_etn in etns:
                          if my_etn > maxetn2 and my_etn < 1000:
                             maxetn2 = my_etn
                    else:
                       maxetn2 = maxetn1
                             
                    ids1 = dd[maxetn1]
                    minid1 = min(ids1)

                    ids2 = ids1
                    minid2 = minid1

                    if maxetn2 > 0 and maxetn2 != maxetn1:
                       ids2 = dd[maxetn2]
                       minid2 = min(ids2)

                    #determine what to keep and what to toss 
                    for etn in etns:
                        all_hourOld = True
                        all_cancelled = True
                        all_twoWeeksOld = True
                        ufn = None

                        for id in dd[etn].keys():
                            for rec in dd[etn][id]:
                                if ufn is None:
                                    ufn = rec.get('ufn', False)
                                hourOld = self.__ctime > rec['endTime'] + (1*3600)
                                twoWeeksOld = self.__ctime > rec['issueTime'] + (14*86400)
                                cancelled = rec['act'] in ['CAN','UPG','EXP']
                                all_hourOld &= hourOld
                                all_cancelled &= cancelled
                                all_twoWeeksOld &= twoWeeksOld

                        # keep records if the event:
                        # 1. is UFN, not cancelled, and not older then two weeks.
                        # 2. not UFN, and not ended in the last hour
                        # 3. cancelled, from this year, keep only records that are minid, and maxetn

                        if ufn and not all_cancelled and not all_twoWeeksOld: # 1
                            for id in dd[etn].keys():
                                for rec in dd[etn][id]:
                                    saveRec.append(rec)

                        elif not ufn and not all_hourOld: # 2
                            for id in dd[etn].keys():
                                for rec in dd[etn][id]:
                                    saveRec.append(rec)

                        elif iy == self.__thisYear and etn == maxetn1: # 3
                            for id in dd[etn].keys():
                                if (id == minid1):
                                    for rec in dd[etn][id]:
                                        saveRec.append(rec)
                                else:
                                    for rec in dd[etn][id]:
                                        LogStream.logDebug("******** WILL PURGE *******", rec['vtecstr'])
                                        purgeRec.append(rec)
                
                        elif (iy == self.__thisYear) and (etn == maxetn2): # 3
                            for id in dd[etn].keys():
                                if (id == minid2):
                                    for rec in dd[etn][id]:
                                        saveRec.append(rec)
                                else:
                                    for rec in dd[etn][id]:
                                        LogStream.logDebug("******** WILL PURGE *******", rec['vtecstr'])
                                        purgeRec.append(rec)
                
                        else:
                            for id in dd[etn].keys():
                                for rec in dd[etn][id]:
                                    LogStream.logDebug("******** WILL PURGE *******", rec['vtecstr'])
                                    purgeRec.append(rec)

        return saveRec, purgeRec
    
    #prints the dictionary organized by oid, phensig, issueYear, etn, id
    def __printTable(self, d):
        oid = d.keys()
        oid.sort()
        for o in oid:
            LogStream.logDebug("----------------------")
            LogStream.logDebug("OFFICE: ", o)
            phensig = d[o].keys()
            phensig.sort()
        
            for ps in phensig:
                LogStream.logDebug("    phensig: ", ps)

                issuances = d[o][ps].keys()
                issuances.sort()

                for iy in issuances:
                    LogStream.logDebug("      issueyear: ", iy)
                    etns = d[o][ps][iy].keys()
                    etns.sort()
            
                    for etn in etns:
                        #
                        LogStream.logDebug("        etn: ", etn)
                        ids = d[o][ps][iy][etn].keys()
                        for id in ids:
                            LogStream.logDebug("            id: ", id)
                            self.__printActiveTable(d[o][ps][iy][etn][id])

    #LogStream.logDebug(a list of records)
    def __printActiveTable(self, activeTable):
        #Pretty-prints the active table, returns the string, skip output
        #of captured text. if combineID is set, then all identical records
        #except for id will be combined.
        s = ''
        combinedEntries = []
        compare = ['vtecstr', 'startTime', 'act', 'endTime', 'phensig', 'phen', 'sig',
          'officeid', 'etn', 'seg', 'purgeTime', 'issueTime']
        for a in activeTable:

            found = 0
            for b in combinedEntries:
                if self._recordCompare(a, b, compare):
                    found = 1
                    ids = b['id']
                    ids.append(a['id'])
                    b['id'] = ids   #update list of ids
                    break

            if found == 0:
                b = copy.deepcopy(a)
                b['id'] = [b['id']]
                combinedEntries.append(b)


        for a in combinedEntries:
            s = s + self.__printRecord(a) + "\n"

        return s

    def __printRecord(self, a):
        #Pretty-prints a single record.
        state = a.get('state', "???")

        if a['id'] is list:
            id = `a['id']`
        else:
            id = a['id']

        ufn = a.get('ufn', 0)

        t = 'Vtec: ' + a['vtecstr'] + \
          '\nStart:  ' +  time.asctime(time.gmtime(a['startTime'])) +\
          ' ' + `a['startTime']` + '  Action: ' + a['act'] + '  Pil: ' + \
          a['pil'] + '\nEnd:    ' + \
          time.asctime(time.gmtime(a['endTime'])) + ' ' + `a['endTime']` + \
          '\nPurge:  ' +  time.asctime(time.gmtime(a['purgeTime'])) + \
          " " + `a['purgeTime']` + '  Key: ' + a['phensig'] + \
          '\nIssue:  ' +  time.asctime(time.gmtime(a['issueTime'])) + \
          " " + `a['issueTime']` + '  UFN: ' + `ufn` + \
          '\nPhen: ' + a['phen'] + '   Sig: ' + a['sig'] + '   Office: ' +\
          a['officeid'] + '   Etn: ' + "%04i" % int(a['etn']) + "   Seg: " + \
          `a['seg']` + '\nZone: ' + `id` + "\n"
        return t

    def _recordCompare(self, rec1, rec2, fields):
        #Compares two records for equality, based on the fields given.
        #Records are dictionaries.  Fields are assumed to exist in both recs.
        for f in fields:
            if rec1[f] != rec2[f]:
                return 0
        return 1


    
    
