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
#       DataIngestServ.py
#       GFS1-NHD:A7790.0000-SCRIPT;1.15
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.15 (DELIVERED)
#         Created:  06-JUL-2005 18:16:36      TROJAN
#           spr 6548
#       
#       Revision 1.14 (DELIVERED)
#         Created:  07-JUN-2005 18:01:10      OBERFIEL
#           Fixed code to survive OB5.1 to OB6 transistion
#       
#       Revision 1.13 (DELIVERED)
#         Created:  01-JUN-2005 17:41:08      OBERFIEL
#           Bug fixes since RHE3 initial snapshot
#       
#       Revision 1.12 (DELIVERED)
#         Created:  07-MAY-2005 11:32:14      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.11 (DELIVERED)
#         Created:  28-APR-2005 13:45:59      OBERFIEL
#           Fixed argument passing to notification server to return a
#           AvnBunch rather than a tuple
#       
#       Revision 1.10 (DELIVERED)
#         Created:  18-APR-2005 17:31:44      OBERFIEL
#           Changes to support gamin
#       
#       Revision 1.9 (DELIVERED)
#         Created:  11-MAR-2005 15:55:30      TROJAN
#           spr 6717
#       
#       Revision 1.8 (DELIVERED)
#         Created:  15-FEB-2005 13:47:37      TROJAN
#           spr 6650
#       
#       Revision 1.7 (APPROVED)
#         Created:  23-JAN-2005 18:42:22      TROJAN
#           spr 6604
#       
#       Revision 1.6 (APPROVED)
#         Created:  07-DEC-2004 18:13:19      TROJAN
#           spr 6510
#       
#       Revision 1.5 (APPROVED)
#         Created:  08-NOV-2004 19:01:13      OBERFIEL
#           Changes to support LLWS
#       
#       Revision 1.4 (APPROVED)
#         Created:  21-OCT-2004 19:31:17      TROJAN
#           spr 6419
#       
#       Revision 1.3 (APPROVED)
#         Created:  30-SEP-2004 20:22:09      TROJAN
#           stdr 873
#       
#       Revision 1.2 (APPROVED)
#         Created:  19-AUG-2004 20:37:07      OBERFIEL
#           Code change
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:38:35      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_6548
#       	Action Date:       09-AUG-2005 14:09:33
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS:  Data acquistion change in OB6
#       
#
# DataIngestServ.py
# Monitors data sources using fam. Actual processing is done by threads 
# Author: George Trojan, SAIC/MDL, December 2003
# last update: 06/06/05

import atexit, logging, os, select, time, threading, Queue
import Pyro.errors
if os.path.exists('/usr/lib/libglib-2.0.so.0'):
     import gamin
else:
     import fam as gamin

import Avn, AvnParser, AvnThread

_Logger = logging.getLogger(__name__)

##############################################################################
class Server(object):
    _Tmout = 5.0
    _Wanted = [gamin.GAMChanged, gamin.GAMExists, gamin.GAMCreated]
    def __init__(self, host, modulenames):
        self._host = host
        self._resultQueue = Queue.Queue()
        self._ingestThread = {}
        self._exitflag = False
        self._modules = []
        for x in AvnParser.getServerCfg()['dis']:
            if modulenames and x['name'] not in modulenames:
                continue
            x['module'] = __import__(x['module'])
            self._modules.append(x)

    def __startNotifyThread(self):
        self._notifyThread = threading.Thread(target=self.__notifier)
        self._notifyThread.setDaemon(1)
        self._notifyThread.start()

    def __startIngestThreads(self):
        for x in self._modules:
            key = x['name']
            try:
                if self._ingestThread[key]['thread'].isAlive():
                    continue
            except KeyError:
                queue = Queue.Queue()
                klass = x['module'].Server(queue, self._resultQueue, x)
                self._ingestThread[key] = {'queue': queue, 'klass': klass, \
                    'thread': None}
            _Logger.info('Starting thread %s', key)
            self._ingestThread[key]['thread'] = threading.Thread( \
                target=self._ingestThread[key]['klass'].run)
            self._ingestThread[key]['thread'].setDaemon(1)
            self._ingestThread[key]['thread'].start()

    def __makeFamConnection(self):
        self.mon = gamin.WatchMonitor()
        args = []
        for x in self._modules:
            key = x['name']
            if key not in self._ingestThread:
                continue
            dirs = self._ingestThread[key]['klass'].paths()
            args.extend([(d, key) for d in dirs if d])
        for arg in args:
            self.mon.watch_directory(arg[0], Avn.curry(self.__callback, *arg))

    def __callback(self, directory, key, fname, event):
        if event not in self._Wanted or fname.startswith('/'):
            return
        self._ingestThread[key]['queue'].put((event, fname, directory))

    def __notifier(self):
        last_time = 0
        while not self._exitflag:
            now = time.time()
            if now > last_time + 29.0:
                msg = Avn.Bunch(src='INGEST-'+self._host, value='ALIVE')
                self._resultQueue.put(msg)
                last_time = now
            try:
                msg = self._resultQueue.get(True, self._Tmout)
                self._publisher.publish(msg)
                _Logger.debug('Msg: %s', str(msg.__dict__))
            except Queue.Empty:
                pass

    def __shutdown(self):
        try:
            del self.mon
        except:
            pass
        self._exitflag = True
        for x in self._modules:
            t = self._ingestThread[x['name']]
            t['queue'].put((0, None, None)) # tell it to quit
            t['thread'].join()              # wait till finishes
        self._notifyThread.join()

    def run(self):
        try:
            self._publisher = AvnThread.Publisher()
        except Pyro.errors.NamingError:
            _Logger.error('Cannot connect to Event Server')
            raise SystemExit
        self.__startNotifyThread()
        self.__startIngestThreads()
        self.__makeFamConnection()
        try:
            while True:
                if not self._notifyThread.isAlive():
                    raise Avn.AvnError('bad thing happened')
                # restart ingest threads if needed
                time.sleep(2)
                self.__startIngestThreads()
                ret = self.mon.event_pending()
                if ret > 0:
                    ret = self.mon.handle_events()
        except Avn.AvnError, e:
            self.__shutdown()
            _Logger.exception(str(e))
            raise SystemExit
