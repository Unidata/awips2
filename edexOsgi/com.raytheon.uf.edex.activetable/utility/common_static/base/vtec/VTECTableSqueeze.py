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
#                                                 Added performance logging
#    06/20/16        #5709         dgilling       Use TropicalCycloneUtil to bin tropical storms
#                                                 when purging, keep one record from every ended
#                                                 hazard.
#    09/01/16         5872         dgilling       Fix error-handling in previous revision.
#    09/14/16        19351         ryu            Modified code to perform normal squeeze action 
#                                                 for each pil separately. Needed due to TCV, MWW,
#                                                 and HLS (for HFO) all issuing tropical events so
#                                                 a CAN record is kept for each pil.
#    05/15/16        19995         ryu            Remove lower ETN records from active table.
#    06/07/17        #6312         dgilling       Modify purging to keep last record for
#                                                 each tropical hazard until 24 hrs have
#                                                 passed without update. 
#    02/25/20        21590         ryu            Keep one of the latest issued records (instead of 
#                                                 the one for the first zone) when an event is squeezed.
#

##
# This is a base file that is not intended to be overridden.
##



import time, copy, LogStream

import TropicalCycloneUtil

from com.raytheon.uf.common.time.util import TimeUtil
from com.raytheon.uf.common.status import PerformanceStatus
perfStat = PerformanceStatus.getHandler("ActiveTable")
timer = TimeUtil.getTimer()

# This class takes a VTEC active table and eliminates unnecessary 
# records.  Records purged consist of old SPC watches, old Tropical
# events, last year's records, and extraneous records not needed for
# new event ETN determination.
#
# Note on Tropical Hazard Events:
# Tropical hazards do not conform to the same rules as other hazards for VTEC
# purposes. The numbers do not increment like other hazards and a lower ETNs
# can be issued after a higher ETN, depending on when a storm develops in
# relation to affecting the U.S.. Additionally, it is possible for a storm to
# exit the U.S. only to come back later where the same ETN will be used with
# NEW once again.

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
            LogStream.logDebug("************** STARTING VTEC TABLE SQUEEZE *********************")
            LogStream.logDebug(time.asctime(time.gmtime(self.__ctime)))
            LogStream.logDebug("************** ORIGINAL TABLE *********************")
            LogStream.logDebug(self.__printActiveTable(table))

        # modify old UFN events (in case fcstrs didn't CAN them)
        timer.reset()
        timer.start()
        table, modTable = self.__modifyOldUFNEvents(table)
        timer.stop()
        perfStat.logDuration("updateActiveTable squeeze __modifyOldUFNEvents", timer.getElapsedTime())
        if self.__debug:
            LogStream.logDebug("************** MOD UFN TABLE *********************")
            for old, new in modTable:
                t = [old, new]
                LogStream.logDebug(self.__printActiveTable(t))
                LogStream.logDebug("    -----------")

        # remove the national center and short fused events 
        timer.reset()
        timer.start()
        shortWFO, shortNC, purgeT = \
          self.__removeOldNationalAndShortFusedEvents(table)
        timer.stop()
        perfStat.logDuration("updateActiveTable squeeze __removeOldNationalAndShortFusedEvents", timer.getElapsedTime())
        if self.__debug:
            LogStream.logDebug("************** SHORT WFO TABLE *********************")
            LogStream.logDebug(self.__printActiveTable(shortWFO))
            LogStream.logDebug("************** SHORT NATL CENTER TABLE *************")
            LogStream.logDebug(self.__printActiveTable(shortNC))
            LogStream.logDebug("************** INITIAL PURGE TABLE *************")
            LogStream.logDebug(self.__printActiveTable(purgeT))

        # separate out the shortWFO into dictionary structure
        timer.reset()
        timer.start()
        dict = self.__separateTable(shortWFO)
        timer.stop()
        perfStat.logDuration("updateActiveTable squeeze __separateTable", timer.getElapsedTime())

        # purge old entries with LowerETNs that aren't in effect
        timer.reset()
        timer.start()
        shorterT, purgeT2 = self.__purgeOldEntriesWithLowerETNs(dict)
        timer.stop()
        perfStat.logDuration("updateActiveTable squeeze __purgeOldEntriesWithLowerETNs", timer.getElapsedTime())
        if self.__debug:
            LogStream.logDebug("************** TRIMMED WFO TABLE ******************")
            LogStream.logDebug(self.__printActiveTable(shorterT))
            LogStream.logDebug("************** ADDITIONAL PURGE TABLE *************")
            LogStream.logDebug(self.__printActiveTable(purgeT2))

        #add in any shortNC entries to final table
        shorterT.extend(shortNC)

        #add in the purged entries from before
        purgeT.extend(purgeT2)

        if self.__debug:
            LogStream.logDebug("************** FINAL TABLE ********************")
            LogStream.logDebug(self.__printActiveTable(shorterT))

        return shorterT, purgeT


    # separates table into a series of nested dictionaries and returns the
    # dictionary.  Dictionary returned is organized by:
    # oid, pil, phensig, issuanceYear, etn, id, and contains a sequence of records
    def __separateTable(self, table):
        d = {}
        for rec in table:
            oid = rec['officeid']
            pil = rec['pil']
            phensig = self.__getPhenSigKey(rec)
            issuance = time.gmtime(rec['issueTime'])[0]
            etn = rec['etn']
            id = rec['id']
        
            # oid
            if oid not in d:
                d[oid] = {}
        
            # pil
            if pil not in d[oid]:
                d[oid][pil]= {}

            #phensig
            if phensig not in d[oid][pil]:
                d[oid][pil][phensig] = {}
        
            #issuance year
            if issuance not in d[oid][pil][phensig]:
                d[oid][pil][phensig][issuance] = {}
        
            #etn
            if etn not in d[oid][pil][phensig][issuance]:
                d[oid][pil][phensig][issuance][etn] = {}
        
            #ids
            if id not in d[oid][pil][phensig][issuance][etn]:
                d[oid][pil][phensig][issuance][etn][id] = []
        
            prevRecords = d[oid][pil][phensig][issuance][etn][id]
            prevRecords.append(rec)
    
        return d
    
    def __getPhenSigKey(self, record):
        if record['phen'] not in TropicalCycloneUtil.TROPICAL_PHENS:
            return (record['phen'], record['sig'])
        else:
            try:
                basin = TropicalCycloneUtil.get_tropical_storm_basin(record)
                return (record['phen'], record['sig'], basin)
            except ValueError:
                LogStream.logProblem("Tropical Hazard record has invalid ETN: ", self.__printRecord(record))
                return (record['phen'], record['sig'])
            
    #figure out any "obsolete records" from the old table.  Obsolete
    #if there is a newer record available, regardless of action.
    #Skip over ROU codes.  Returns tuple of shortened table WFO, 
    #shortened table NC,  and purged table.
    #entries.
    def __removeOldNationalAndShortFusedEvents(self, table):
        compare = ['id', 'phen', 'sig', 'officeid', 'pil']
        convWatch = [('SV','A'), ('TO','A')]
        
        # To make it simpler to deal with the case where tropical hazards can 
        # go from NEW -> CAN -> NEW we will purge any event that has not been
        # updated after 24 hours
        tropicalPhen = TropicalCycloneUtil.TROPICAL_PHENS
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
        for x in range(len(table)):
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
    # d is [oid][pil][phensig][issueYear][etn][id] dictionary of records
    def __purgeOldEntriesWithLowerETNs(self, d):
        saveRec = []
        purgeRec = []
        for o in d:
            pils = d[o]
            for pil in pils:
                phensigs = pils[pil]
                for ps in phensigs:
                    issueYears = phensigs[ps]
                    for iy in issueYears:
                        etns = issueYears[iy]

                        #determine what to keep and what to toss 
                        maxetn = max(etns)
                        for etn in etns:
                            all_hourOld = True
                            all_cancelled = True
                            all_twoWeeksOld = True
                            ufn = None

                            ids = etns[etn]
                            for id in ids:
                                for rec in ids[id]:
                                    if ufn is None:
                                        ufn = rec.get('ufn', False)
                                    hourOld = self.__ctime > rec['endTime'] + (1*3600)
                                    twoWeeksOld = self.__ctime > rec['issueTime'] + (14*86400)
                                    cancelled = rec['act'] in ['CAN','UPG','EXP']
                                    all_hourOld = all_hourOld and hourOld
                                    all_cancelled = all_cancelled and cancelled
                                    all_twoWeeksOld = all_twoWeeksOld and twoWeeksOld

                            # keep records if the event:
                            # 1. is UFN, not cancelled, and not older then two weeks.
                            # 2. not UFN, and not ended in the last hour
                            # 3. cancelled, from this year, keep only records that are minid

                            if ufn and not all_cancelled and not all_twoWeeksOld: # 1
                                for id in ids:
                                    for rec in ids[id]:
                                        saveRec.append(rec)

                            elif not ufn and not all_hourOld: # 2
                                for id in ids:
                                    for rec in ids[id]:
                                        saveRec.append(rec)

                            elif iy == self.__thisYear: # 3
                                keep_id = None
                                # We keep a record of every in process 
                                # or "ended" tropical hazard to deal with 
                                # hazards that hit land out of ETN order
                                if ps[0] in TropicalCycloneUtil.TROPICAL_PHENS or etn == maxetn:
                                    # need to keep the record with the latest issuance time 
                                    # so it will not be superseded by an earlier live record
                                    # in case a neighbor site did not receive the full cancellation.
                                    max_issuetime = 0
                                    for id in ids:
                                        for rec in ids[id]:
                                            if rec['issueTime'] > max_issuetime:
                                                max_issuetime = rec['issueTime']
                                                keep_id = id
                                                
                                for id in ids:
                                    if id == keep_id:
                                        for rec in ids[id]:
                                            saveRec.append(rec)
                                    else:
                                        for rec in ids[id]:
                                            LogStream.logDebug("******** WILL PURGE *******", rec['vtecstr'])
                                            purgeRec.append(rec)

                            else:
                                for id in ids:
                                    for rec in ids[id]:
                                        LogStream.logDebug("******** WILL PURGE *******", rec['vtecstr'])
                                        purgeRec.append(rec)

        return saveRec, purgeRec
    
    #prints the dictionary organized by oid, phensig, issueYear, etn, id
    def __printTable(self, d):
        for (o, phensigs) in sorted(d.items()):
            LogStream.logDebug("----------------------")
            LogStream.logDebug("OFFICE: ", o)

            for (ps, issuances) in sorted(phensigs.items()):
                LogStream.logDebug("    phensig: ", ps)

                for (iy, etns) in sorted(issuances.items()):
                    LogStream.logDebug("      issueyear: ", iy)

                    for (etn, ids) in sorted(etns.items()):
                        LogStream.logDebug("        etn: ", etn)

                        for (id, entry) in ids.items():
                            LogStream.logDebug("            id: ", id)
                            self.__printActiveTable(entry)

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
        if a['id'] is list:
            id = repr(a['id'])
        else:
            id = a['id']

        ufn = a.get('ufn', 0)

        t = 'Vtec: ' + a['vtecstr'] + \
          '\nStart:  ' +  time.asctime(time.gmtime(a['startTime'])) +\
          ' ' + repr(a['startTime']) + '  Action: ' + a['act'] + '  Pil: ' + \
          a['pil'] + '\nEnd:    ' + \
          time.asctime(time.gmtime(a['endTime'])) + ' ' + repr(a['endTime']) + \
          '\nPurge:  ' +  time.asctime(time.gmtime(a['purgeTime'])) + \
          " " + repr(a['purgeTime']) + '  Key: ' + a['phensig'] + \
          '\nIssue:  ' +  time.asctime(time.gmtime(a['issueTime'])) + \
          " " + repr(a['issueTime']) + '  UFN: ' + repr(ufn) + \
          '\nPhen: ' + a['phen'] + '   Sig: ' + a['sig'] + '   Office: ' +\
          a['officeid'] + '   Etn: ' + "%04i" % int(a['etn']) + "   Seg: " + \
          repr(a['seg']) + '\nZone: ' + repr(id) + "\n"
        return t

    def _recordCompare(self, rec1, rec2, fields):
        #Compares two records for equality, based on the fields given.
        #Records are dictionaries.  Fields are assumed to exist in both recs.
        for f in fields:
            if rec1[f] != rec2[f]:
                return 0
        return 1
