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
#    Name:
#       TextThread.py
#       GFS1-NHD:A7824.0000-SCRIPT;1.15
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.15 (DELIVERED)
#         Created:  01-AUG-2008 15:44:47      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 1.14 (DELIVERED)
#         Created:  11-JUN-2008 14:51:59      OBERFIEL
#           Removed references to obsolete products
#       
#       Revision 1.13 (DELIVERED)
#         Created:  09-NOV-2007 09:32:58      OBERFIEL
#           Increased Buffers to support development at NWSHQ (this
#           isn't active at WFO).  Removed debug messages too.
#       
#       Revision 1.12 (DELIVERED)
#         Created:  28-JUL-2005 15:24:10      OBERFIEL
#           Fix buggy programming
#       
#       Revision 1.11 (APPROVED)
#         Created:  06-JUL-2005 18:16:42      TROJAN
#           spr 6548
#       
#       Revision 1.10 (DELIVERED)
#         Created:  01-JUN-2005 17:41:23      OBERFIEL
#           Bug fixes since RHE3 initial snapshot
#       
#       Revision 1.9 (DELIVERED)
#         Created:  07-MAY-2005 11:39:23      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.8 (DELIVERED)
#         Created:  28-APR-2005 19:30:44      TROJAN
#           spr 6816
#       
#       Revision 1.7 (APPROVED)
#         Created:  28-APR-2005 13:45:52      OBERFIEL
#           Fixed argument passing to notification server to return a
#           AvnBunch rather than a tuple
#       
#       Revision 1.6 (DELIVERED)
#         Created:  18-APR-2005 17:32:26      OBERFIEL
#           Changes to support gamin
#       
#       Revision 1.5 (DELIVERED)
#         Created:  11-MAR-2005 15:55:31      TROJAN
#           spr 6717
#       
#       Revision 1.4 (DELIVERED)
#         Created:  15-FEB-2005 13:47:38      TROJAN
#           spr 6650
#       
#       Revision 1.3 (APPROVED)
#         Created:  21-OCT-2004 19:29:02      TROJAN
#           spr 6418
#       
#       Revision 1.2 (APPROVED)
#         Created:  30-SEP-2004 20:22:11      TROJAN
#           stdr 873
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:45:27      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7385
#       	Action Date:       11-OCT-2008 12:56:11
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Handle missing LLWS sources better
#       
#

import logging, os, Queue, time
import gamin
import Avn, TextThreadP

_Logger = logging.getLogger(__name__)

##############################################################################
class Server(TextThreadP.Server):
    """Processes TAFs, TWEBs, METARs and CCFP reports
Expects files from acqserver. File name determines data type.
"""
    _Offset = 24    # number of bytes to skip in incoming file
    _BufLen = 2048    # number of slots for incoming files
    _NumBins = 16   # must be the same as in cleanup.py
    
    def __init__(self, inqueue, outqueue, info):
        TextThreadP.Server.__init__(self, inqueue, outqueue, info)
        self._buffer = [(0, None)]*self._BufLen # time, file name

    def __decodeFile(self, directory, fname):
        # Dispatches bulletin to proper methods based on WMO header
        path = os.path.join(directory, fname)
        try:
            fh = file(path)
            fh.seek(self._Offset, 0)
            data = fh.read().translate(self._Ident, self._Delchars)
            bulletin = filter(None, [x.rstrip() for x in data.split('\n')])
            fh.close()
        except IOError:
            _Logger.exception('Cannot access %s' % path)
            return 0
        
        if fname.startswith('SA') or fname.startswith('SP'):
            return self.doMetar(bulletin)
        elif fname.startswith('FT'):
            return self.doTaf(bulletin)
        elif fname.startswith('FAUS2') or fname.startswith('FAUS30'):
            return self.doCCFP(bulletin)
        else:
            _Logger.info('Unknown data type: %s' % fname)
            return 0

    def processFile(self, code, direct, fname):
        if code == gamin.GAMChanged:     # modified
            raise Queue.Empty
        elif code == gamin.GAMCreated: 
            ftime = time.time()
        elif code == gamin.GAMExists:     
            # old file, any positive is ok
            ftime = 1
        # Adds file names to a buffer for later processing
        for i in range(self._BufLen):
            if self._buffer[i][1] == fname:
                # duplicate
                break
        else:
            for i in range(self._BufLen):
                if self._buffer[i][0] == 0:
                    # free slot
                    self._buffer[i] = (ftime, fname)
                    break
            else:
                _Logger.info('Buffer full, losing data from %s', fname)
        # Processes files from buffer
        cutoff = time.time() - self._Tmout
        for i in range(self._BufLen):
            if not 0 < self._buffer[i][0] < cutoff:
                continue
            fname = self._buffer[i][1]
            rcode = self.__decodeFile(direct, fname)
            self._buffer[i] = (0, None)
            self.dispose(rcode, direct, fname)

    def onStartup(self):
        self.getSites()
        # Processes files from overflow bins
        for n in range(self._NumBins):
            bin = 'bin%02d' % n
            direct = os.path.join(self.datapath, bin)
            for fname in os.listdir(direct):
                rcode = self.__decodeFile(direct, fname)
                self.dispose(rcode, direct, fname)

    def paths(self):
        return [os.path.join(self.datapath, 'raw')]
