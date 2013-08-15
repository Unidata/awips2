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
#       GuidanceThread.py
#       GFS1-NHD:A8834.0000-SCRIPT;2
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 2 (DELIVERED)
#         Created:  31-JAN-2006 08:35:24      TROJAN
#           Change in naming convention for MOS/LAMP data, added LAMP
#           to plotting module
#       
#       Revision 1 (DELIVERED)
#         Created:  10-JUL-2005 18:23:25      TROJAN
#           spr 6915
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_STDR_900
#       	Action Date:       21-MAR-2006 15:26:42
#       	Relationship Type: In Response to
#       	Status:           DELIVERED
#       	Title:             AvnFPS: Creation of guidance TAFs
#       
#
# GuidanceThread.py
# acesses BUFR data
# Author: George Trojan, SAIC/MDL, May 2005
# last update: 01/30/06

import logging, os, Queue, re, time
import Avn, AvnParser, EtaData, MosData

_Pattern = re.compile(r'[0-9]{8}_[0-9]{4}')

_Logger = logging.getLogger(__name__)

###############################################################################
class Server(object):
    WaitTime = 120.0  # time between reads (seconds)

    def __init__(self, inqueue, outqueue, info):
        self.inqueue = inqueue
        self.outqueue = outqueue
        self.name = info['name']
        self.nhours = int(info['nhours'])
        self.info = info.copy()
        del self.info['name']
        del self.info['nhours']
        del self.info['module']
        self.siteinfo = AvnParser.getAllSiteIds()

    def __retrieve(self, direct, fname):
        # find data type
        for key in self.info:
            if direct == self.info[key]:
                break
        else:
            return
        idlist = [x for x in self.siteinfo[key] if x]
        path = os.path.join(direct, fname)
        if key == 'eta':
            ids = EtaData.retrieve(path, self.nhours, filter(None, idlist))
        else:
            ids = MosData.retrieve(path, self.nhours, key, filter(None, idlist))
        if ids:
            self.outqueue.put(Avn.Bunch(src=self.name, ident='ALL'))

    def paths(self):
        return filter(None, self.info.values())

    def run(self):
        while True:
            lasttime = time.time()
            flist = []
            # optimization
            while True:
                try:
                    code, fname, direct = self.inqueue.get(True, 6)
                    if code == 0:   # end thread
                        _Logger.info('Got exit request')
                        raise SystemExit
                    if _Pattern.match(fname) and (direct, fname) not in flist:
                        flist.append((direct, fname))
                except Queue.Empty:
                    pass
                if time.time() > lasttime + self.WaitTime:
                    break
            try:
                for item in flist:
                    self.__retrieve(*item)
            except Exception:
                _Logger.exception('Unexpected error')
                break
