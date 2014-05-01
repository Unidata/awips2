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
# MergeVTEC - merges two "active" tables together.
# Ported from AWIPS1 code, originally written by Mark Mathewson FSL
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    ??/??/??                      wldougher      Initial Creation.
#    02/22/13        1447          dgilling       Re-ported to better match
#                                                 requestAT/sendAT.
# 
#

import argparse
import collections
import cPickle
import gzip
import logging
import os
import re
import sys

from dynamicserialize.dstypes.com.raytheon.uf.common.activetable.request import MergeActiveTableRequest

from ufpy import ThriftClient
from ufpy import TimeUtil
from ufpy import UsageArgumentParser


logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s", 
                    datefmt="%H:%M:%S", 
                    level=logging.INFO)
log = logging.getLogger('MergeVTEC')


class CaseInsensitiveStringSet(collections.Set):
    def __init__(self, iterable):
        self.__internalSet = frozenset(iterable)
        
    def __contains__(self, x):
        return x.upper() in (item.upper() for item in self.__internalSet)
    
    def __len__(self):
        return len(self.__internalSet)
    
    def __iter__(self):
        return iter(self.__internalSet)

class StoreDrtTimeAction(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        drtInfoTuple = TimeUtil.determineDrtOffset(values)
        setattr(namespace, self.dest, drtInfoTuple[0])

class MergeVTEC(object):
    def __init__(self, serverHost, serverPort, site, removeRemote, 
      remoteATName, atName, inputIsGZIP, drt=0, makeBackups=True, 
      xmlIncoming=None, fromIngestAT=False):
        # serverHost - host name to send merge request to
        # serverPort - port EDEX is running on
        # site - site to perform merge operation as
        # removeRemote (False, True) to remove remote table file upon completion
        # remoteATName - name of remote active table file
        # atName (OPERATIONAL, PRACTICE) - name of active table name
        # inputIsGZIP (False, True) - remote input file is gzipped
        # drt mode - None, or number of seconds +/- current time
        # makeBackups (False, True) - make backups of table changes
        # xmlIncoming - None, or XML data from MHS, only used in ingestAT mode
        # fromIngestAT (False, True) - run in ingestAT mode, only affects logging
        self._thriftClient = ThriftClient.ThriftClient(serverHost, serverPort, '/services')
        self._site = site
        self._deleteAfterProcessing = removeRemote
        self._remoteTableFilename = remoteATName
        self._activeTableFilename = atName
        drt = 0 if drt is None else drt
        self._drtInfo = float(drt)
        self._makeBackups = makeBackups
        self._xmlIncoming = xmlIncoming
        self._fromIngestAT = fromIngestAT

        log.info("MergeVTEC Starting")
        log.info("remoteFN= " + self._remoteTableFilename + 
          " localFN= " + self._activeTableFilename + 
          " siteID= " + self._site)

        # read table to merge
        otherTable = self._readActiveTable(self._remoteTableFilename,
          inputIsGZIP)
        log.info("Remote Table size: %d", len(otherTable))
        
        self._mergeTable(otherTable)
        
        # delete remote file, if desired
        if self._deleteAfterProcessing:
            os.remove(self._remoteTableFilename)

        log.info("MergeVTEC Finished")
    
    def _mergeTable(self, otherTable):
        # Send the new active table to the server.
        # The real merge is done server-side.
        request = MergeActiveTableRequest(otherTable, self._activeTableFilename, 
                    self._site, self._drtInfo, self._xmlIncoming, 
                    self._fromIngestAT, self._makeBackups)        
        response = self._thriftClient.sendRequest(request)
        if not response.getTaskSuccess():
            raise RuntimeError("Error performing merge: " + response.getErrorMessage())

    def _readActiveTable(self, filename, inputIsGZIP=False):
        # reads the active table and returns the list of records

        records = []

        # get the file and unpickle it
        if not inputIsGZIP:
            fd = open(filename, 'rb')
        else:
            fd = gzip.open(filename, 'rb')

        buf = fd.read()
        fd.close()
        log.debug("read active table, size: %d", len(buf))
        records = cPickle.loads(buf)
        log.debug("cPickle.loads, #records: %d", len(records))
        
        if records and records[0].has_key('oid'):
            self._convertToNewFormat(records)
        
        return records
    
    def _convertToNewFormat(self, table):
        '''Convert an AWIPS I table to AWIPS 2 internally'''
    
        maxFutureTime = long(float(2**31-1)) 
        for entry in table:
            entry['officeid'] = entry['oid']
            entry['vtecstr'] = entry['vstr']
            entry['endTime'] = int(entry['end'])
            entry['startTime'] = int(entry['start'])
            entry['purgeTime'] = int(entry['purgeTime'])
            entry['issueTime'] = int(entry['issueTime'])
            entry['phensig'] = entry['key']
            entry['state'] = 'Decoded'
            if entry['endTime'] >= maxFutureTime:
                entry['ufn'] = True
            else:
                entry['ufn'] = False
            entry['productClass'] = entry['vtecstr'][1]
            if not entry.has_key('rawMessage'):
                entry['rawMessage'] = ''
            
            # Note: this is not always correct
            entry['xxxid'] = entry['officeid'][1:]
    
            if entry.has_key('text'):
                entry['segText'] = entry['text']
                # adapted from WarningDecoder...
                lines = entry['segText'].split('\n')
                for count in xrange(len(lines)-1):
                    dtg_search = re.search(r' ([0123][0-9][012][0-9][0-5][0-9])', 
                                           lines[count])
                    if dtg_search:
                        pil_search = re.search(r'^([A-Z]{3})(\w{3}|\w{2}|\w{1})', 
                                               lines[count+1])
                        if pil_search:
                            entry['xxxid'] = pil_search.group(2)
                            break


def process_command_line():
    parser = UsageArgumentParser.UsageArgumentParser(prog='MergeVTEC', conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="serverHost",
                        required=True, metavar="serverHost",
                        help="host name of the EDEX server")
    parser.add_argument("-p", action="store", type=int, dest="serverPort",
                        required=True, metavar="serverPort",
                        help="port number for the EDEX server")
    parser.add_argument("-d", action="store_true", 
                        dest="removeRemote", 
                        help="delete remote active table when done")
    parser.add_argument("-a", action="store", dest="atName", 
                        choices=CaseInsensitiveStringSet(['OPERATIONAL', 'PRACTICE']), 
                        required=True, metavar="atName", default="OPERATIONAL", 
                        help="name of the active table (OPERATIONAL or PRACTICE)")
    parser.add_argument("-r", action="store", dest="remoteATName",
                        required=True, metavar="rmtATName",
                        help="location of the active table (remote)")
    parser.add_argument("-z", action=StoreDrtTimeAction, dest="drt",
                        metavar="drtMode", help="Run in DRT mode")
    parser.add_argument("-n", action="store_false", dest="makeBackups",
                        help="Don't make backups of vtec table")
    parser.add_argument("-g", action="store_true", 
                        dest="inputIsGZIP", 
                        help="Remote active table is compressed")
    parser.add_argument("-s", action="store", dest="site", metavar="siteID",
                        required=True,
                        help="site to merge AT records into")
    return vars(parser.parse_args())

def merge(serverHost, serverPort, site, removeRemote, remoteATName, 
      atName, inputIsGZIP=False, drt=0, makeBackups=True, xmlIncoming=None, 
      fromIngestAT=False):
    decoder = MergeVTEC(serverHost, serverPort, site, removeRemote, 
      remoteATName, atName, inputIsGZIP, drt, makeBackups, xmlIncoming, 
      fromIngestAT)
    decoder = None
    return

def main():
    args = process_command_line()
    try:
        merge(**args)
        sys.exit(0)
    except:
        log.exception("Caught Exception: ")
        sys.exit(1)

if __name__ == "__main__":
    main()
