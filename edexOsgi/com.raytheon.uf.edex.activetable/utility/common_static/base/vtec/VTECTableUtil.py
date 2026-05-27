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
# Utility classes for the VTEC util table
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- ---------------------------------------------
# Jun 17, 2013  3296     randerso  Moved active table backup and purging to a
#                                  separate thread in java.
# Apr 23, 2015  4259     njensen   Updated for new JEP API
# Feb 01, 2017  6107     dgilling  Improve file name uniqueness when writing
#                                  active table backups.
# Apr 29, 2020  8151     randerso  Use SiteMap.getSite4LetterId()
#
##

##
# This is a base file that is not intended to be overridden.
##

import copy
import pickle
import datetime
import errno
import glob
import gzip
import os
import stat
import time

import LogStream
import JUtil

from java.util import ArrayList
from com.raytheon.uf.common.activetable import ActiveTableUtil


class VTECTableUtil:

    def __init__(self, activeTableFileName=None):
        self._activeTableFilename = activeTableFileName
        self._activeTableLockFD = None
        if activeTableFileName is not None:
            self._activeTableLockFilename = self._activeTableFilename + "lock"
        else:
            self._activeTableLockFilename = None

#------------------------------------------------------------------
# Consolidate Utilities
#------------------------------------------------------------------

    # given the table, will consolidate like records and return a table
    # with identical hazards, but with multiple id entries
    def consolidateByID(self, ptable, compare=None):
        if compare is None:
            compare = ['etn', 'vtecstr', 'ufn', 'areaPoints', 'valuePoints', 'hdln',
              'phensig', 'previousStart', 'previousEnd', 'purgeTime', 'issueTime',
              'downgradeFrom', 'upgradeFrom', 'startTime', 'endTime', 'act', 'pil', 'phen',
              'sig', 'officeid', 'seg', 'state']
        ctable = []
        for a in ptable:
            found = 0
            for c in ctable:
                if self.hazardCompare(a, c, compare):
                    found = 1
                    if isinstance(a['id'], list):
                        zones = a['id']
                    else:
                        zones = [a['id']]

                    allzones = c['id']
                    for z in zones:
                        allzones.append(z)
                    c['id'] = allzones
                    break
            if found == 0:
                newc = copy.deepcopy(a)
                if newc['id'] is not list:
                    newc['id'] = [newc['id']]
                ctable.append(newc)

        return ctable

#------------------------------------------------------------------
# Printing Utilities
#------------------------------------------------------------------

    # Pretty-prints the given table or table entry, returns the string
    def printActiveTable(self, table, combine=False, idType='zone'):
        if table is None:
            s = "Table is None"
            return s
        elif hasattr(table, "java_name"):
            table = JUtil.javaObjToPyVal(table)
            print(type(table))

        # dictionary, single record
        if isinstance(table, dict):
            ptable = []
            ptable.append(table)
        # list of dictionaries
        else:
            ptable = table

        s = '\n'  # Return value

        # Use the zone or city printEntry method
        if idType == 'zone':
            printEntry = self.printEntry
            compare = None
        elif idType == 'city':
            printEntry = self.printCityEntry
            compare = ['start', 'end', 'phen', 'sig', 'seg']

        # combine mode, attempt to combine records
        if combine:
            ctable = self.consolidateByID(ptable, compare=compare)
            for c in ctable:
                s = s + printEntry(c)
        # non-combine mode
        else:
            for p in ptable:
                s = s + printEntry(p)

        return s

    # Pretty-prints an entry in the table
    def printEntry(self, entry):
        # formatting for the etn
        if isinstance(entry['etn'], int):
            etn = "%04i" % entry['etn']
        else:
            etn = entry['etn']

        # formatting for the vstr
        if 'vtecstr' in entry:
            vstr = entry['vtecstr']
        else:
            vstr = "????"

        # formatting for the id depending upon the type of table
        zones = ''
        if isinstance(type(entry['id']), list):
            zones = sorted(entry['id'])
            zones = repr(zones)
        else:
            zones = entry['id']

        # until further notice
        ufn = entry.get('ufn', 0)

        # formatting for areaPoints, valuePoints if available
        apvp = ''
        if 'areaPoints' in entry and 'valuePoints' in entry:
            ratio = (entry['valuePoints'] / float(entry['areaPoints'])) * 100.0
            apvp = 'AreaPoints:  ' + '%6i' % entry['areaPoints'] +\
                '    ValuePoints: ' + '%6i' % entry['valuePoints'] + \
                '    % Area: ' + '%6.1f' % ratio + '%\n'

        # formatting for hdln
        if 'hdln' in entry:
            hdln = entry['hdln']
        else:
            hdln = "????"

        # formatting for key
        if 'phensig' in entry:
            key = entry['phensig']
        else:
            key = "????"

        # formatting for previousStart/End
        if 'previousStart' in entry and 'previousEnd' in entry:
            prev = '\nPrevStart: ' + \
                time.asctime(time.gmtime(entry['previousStart'])) + \
                ' ' + repr(entry['previousStart']) + '\nPrevEnd: ' + \
                time.asctime(time.gmtime(entry['previousEnd'])) + \
                ' ' + repr(entry['previousEnd'])
        else:
            prev = ''

        # formatting for purgeTime
        if 'purgeTime' in entry:
            expireT = '\nPurge:  ' + \
                time.asctime(time.gmtime(entry['purgeTime'])) + \
                ' ' + repr(entry['purgeTime'])
        else:
            expireT = ''

        # formatting for issueTime
        if 'issueTime' in entry:
            issueT = '\nIssue:  ' + \
                time.asctime(time.gmtime(entry['issueTime'])) + \
                ' ' + repr(entry['issueTime'])
        else:
            issueT = ''

        # formatting for recState
        if 'state' in entry:
            recState = " RecState: " + entry['state']
        else:
            recState = ''

        # formatting for upgrades/downgrades special records
        if 'downgradeFrom' in entry:
            related = entry['downgradeFrom']
            duType = "DowngradeFrom: "
        elif 'upgradeFrom' in entry:
            related = entry['upgradeFrom']
            duType = "UpgradeFrom: "
        else:
            related = None
        if related is not None:
            if 'phensig' in related:
                rkey = related['phensig']
            else:
                rkey = "????"
            if isinstance(related['etn'], int):
                retn = "%04i" % related['etn']
            else:
                retn = related['etn']
            relatedText = (
                f"{duType}Action: {related['act']}  Phen: {related['phen']}   Sig: {related['sig']}   Key: {rkey}  Etn:{retn}\n"
                f"{duType}Start: {time.asctime(time.gmtime(related['startTime']))} {related['startTime']!r}\n"
                f"{duType}End:   {time.asctime(time.gmtime(related['endTime']))} {related['endTime']!r}\n"
            )
        else:
            relatedText = ""

        t = (
            f"Vtec: {vstr}\n"
            f"Hdln: {hdln}\n"
            f"Start:  {time.asctime(time.gmtime(entry['startTime']))} {entry['startTime']!r}  Action: {entry['act']}  Pil: {entry['pil']}\n"
            f"End:    {time.asctime(time.gmtime(entry['endTime']))} {entry['endTime']!r}  UFN: {ufn!r}{recState}{expireT}{issueT}{prev}\n"
            f"Phen: {entry['phen']}   Sig: {entry['sig']}   Office: {entry['officeid']}   Etn: {etn} Seg: {entry['seg']!r}   Key: {key}\n"
            f"{apvp}Zone: {zones}\n"
            f"{relatedText}\n"
        )
        return t

    # Pretty-prints an entry in the table
    def printCityEntry(self, entry):
        # formatting for the id depending upon the type of table
        zones = ''
        if isinstance(entry['id'], list):
            zones = sorted(entry['id'])
            zones = repr(zones)
        else:
            zones = entry['id']

        # formatting for key
        if 'key' in entry:
            key = entry['key']
        else:
            key = "????"

        # until further notice
        ufn = entry.get('ufn', 0)

        start = entry['startTime']
        end = entry['endTime']
        t = 'Phen: ' + entry['phen'] + '   Sig: ' + entry['sig'] + \
            '  Seg: ' + repr(entry['seg']) + '   Key: ' + key + \
            '  UFN: ' + repr(ufn) + \
            '\nStart:  ' + time.asctime(time.gmtime(start)) + ' ' + repr(start) +\
            '\nEnd:    ' + time.asctime(time.gmtime(end)) + ' ' + repr(end) +\
            '\nCities: ' + zones + '\n\n'
        return t

    # Comparison routine for two hazard entries.   Returns 1 if the
    # two records are equal.  The fields to compare are provided.
    def hazardCompare(self, rec1, rec2, fields):
        for f in fields:
            inRec1 = f in rec1
            inRec2 = f in rec2
            if inRec1 and inRec2:
                if rec1[f] != rec2[f]:
                    return 0
            elif inRec1 or inRec2:
                return 0  # one record has the field, set it not equal
            else:
                continue  # neither record has this field, that is ok
        return 1

    def _get4ID(self, id):
        from com.raytheon.uf.common.site import SiteMap
        return SiteMap.getInstance().getSite4LetterId(id)

#------------------------------------------------------------------
# Table lock/unlock read/write utility
#------------------------------------------------------------------

    def saveOldActiveTable(self, oldActiveTable):
        # saves off the specified table and time stamps it

        if self._activeTableFilename is None:
            raise Exception("saveOldActiveTable without filename")

        # determine filename
        directory = os.path.join(
            os.path.dirname(
                self._activeTableFilename),
            "backup")
        try:
            os.makedirs(directory)
        except OSError as e:
            if e.errno != errno.EEXIST:
                LogStream.logProblem("Could not create active table backup directory:",
                                     directory, LogStream.exc())
                raise e

        baseN = os.path.basename(self._activeTableFilename)
        fn = datetime.datetime.utcnow().strftime("%Y%m%d_%H%M%S_%f") + "_" + baseN
        filename = os.path.join(directory, fn + ".gz")

        t = time.time()
        try:
            os.chmod(filename, 0o666)
            os.remove(filename)
        except Exception:
            pass

        # output file
        # gzip it to save space
        with gzip.GzipFile(filename, 'wb', 9) as fd:
            buf = pickle.dumps(oldActiveTable)
            fd.write(buf)
        os.chmod(filename, 0o664)

        t1 = time.time()
        tstr = "%.3f" % (t1 - t)
        LogStream.logVerbose("Saved Previous Active Table: ", fn, "t=",
                             tstr, "sec.")

    def purgeOldSavedTables(self, purgeTime):
        # purges old saved tables

        if self._activeTableFilename is None:
            raise Exception("purgeOldSavedTables without filename")

        # calculate purge time
        purgeTime = time.time() - (purgeTime * 3600)

        #directory and files
        directory = os.path.join(
            os.path.dirname(
                self._activeTableFilename),
            "backup")
        baseN = os.path.basename(self._activeTableFilename)
        idx = baseN.find(".")
        if idx != -1:
            baseN = baseN[0:idx]
        files = glob.glob(directory + "/*" + baseN + "*.gz")

        # delete files older than purgeTime
        for f in files:
            try:
                modTime = os.stat(f)[stat.ST_MTIME]
                if modTime < purgeTime:
                    os.remove(f)
                    LogStream.logDebug("Removing old file: ", f)
            except Exception:
                LogStream.logProblem("Problem Removing old backup file: ", f,
                                     LogStream.exc())

    def _convertTableToPurePython(self, table, siteId):
        javaRecList = ArrayList()
        for rec in table:
            javaRecList.add(rec.javaRecord())

        javaDictFormat = ActiveTableUtil.convertToDict(javaRecList, siteId)

        return JUtil.javaObjToPyVal(javaDictFormat)


def backupActiveTable(activeTable, activeTableMode, filePath, siteId):
    import ActiveTableRecord
    pyActive = []
    szActive = activeTable.size()
    for i in range(szActive):
        pyActive.append(
            ActiveTableRecord.ActiveTableRecord(
                activeTable.get(i)))

    # create a dummy name to simplify the file access code in VTECTableUtil
    util = VTECTableUtil(os.path.join(filePath, activeTableMode + ".tbl"))
    oldActiveTable = util._convertTableToPurePython(pyActive, siteId)
    util.saveOldActiveTable(oldActiveTable)
