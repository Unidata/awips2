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
#       AvnServer.py
#       GFS1-NHD:A7965.0000-SCRIPT;1.6
#
#    Status:
#       DELIVERED
#    
# AvnServer.py
# wrappers for name and event servers
# George Trojan, SAIC/MDL, August 2004
# last update: 06/04/05

import os, threading
import Pyro.core, Pyro.naming, Pyro.util, Pyro.constants
from Pyro.errors import *
import AvnPyro

class NameServer(threading.Thread):
    def __init__(self, **kw):
        threading.Thread.__init__(self)
        self.setDaemon(1)
        self.kw = kw
        self.starter = Pyro.naming.NameServerStarter()

    def run(self):
        print "Launching Pyro Name Server"
        kw = {'hostname': self.kw.get('-n', '')}
        kw.update({'persistent': 1, 'dbdir': 'nsdb', 'verbose': 1,\
            'Guards': (AvnPyro.NSGuard(), AvnPyro.BCGuard())})
        self.starter.start(**kw)

    def waitUntilStarted(self, timeout=None):
        return self.starter.waitUntilStarted(timeout)

##############################################################################
import Pyro.EventService.Server

Log=Pyro.util.Log
        
class EventServer(threading.Thread):
    def __init__(self, **kw):
        threading.Thread.__init__(self)
        self.setDaemon(1)
        self.kw = kw
        self.starter = EventServiceStarter()

    def run(self):
        Log.msg('Launching Pyro Event Server')
        self.starter.start(hostname=self.kw.get('-n', ''))

    def waitUntilStarted(self):
        return self.starter.waitUntilStarted()

###############################################################################
# EventServiceStarter is copied from Pyro.EventService.Server, with minor
# modifications, to avoid the need for interactive call

class EventServiceStarter:
    def __init__(self, identification=None):
        self.running=1
        self.identification=identification
        self.started = Pyro.util.getEventObject()

    def start(self, *args, **kwargs):
        # see _start for allowed arguments
        self._start(startloop=1, *args, **kwargs)

    def initialize(self, *args, **kwargs):      
        # see _start for allowed arguments
        self._start(startloop=0, *args, **kwargs)

    def getServerSockets(self):
        return self.daemon.getServerSockets()

    def waitUntilStarted(self, timeout=None):
        self.started.wait(timeout)
        return self.started.isSet()

    def _start(self, hostname='', port=None, startloop=1, useNameServer=1):
        daemon = Pyro.core.Daemon(host=hostname, port=port)
        if self.identification:
            # Requiring connection authentication
            daemon.setAllowedIdentifications([self.identification])

        if useNameServer:
            locator = Pyro.naming.NameServerLocator( \
                identification=self.identification)
            try:
                port = int(os.environ['PYRO_NS_PORT'])
            except KeyError:
                port = None
            ns = locator.getNS(port=port)
    
            # check if ES already running
            try:
                ns.resolve(Pyro.constants.EVENTSERVER_NAME)
                Log.warn('Event Server appears to be already running.')
#               ns.unregister(Pyro.constants.EVENTSERVER_NAME)
            except NamingError:
                pass
    
            daemon.useNameServer(ns)

        es = Pyro.EventService.Server.EventService()

        esURI=daemon.connectPersistent(es, 
            Pyro.constants.EVENTSERVER_NAME)
        Log.msg('URI=%s' % esURI)

        message = daemon.validateHostnameAndIP()
        if message:
            Log.warn('WARNING: ' + message)

        Log.msg('Event Server started.')

        self.started.set()      # signal that we've started.

        if startloop:
            Log.msg('ES daemon','This is the Pyro Event Server.')
            try:
                daemon.setTimeout(20)  # XXX fixed timeout
                daemon.requestLoop(lambda s=self: s.running)
            except KeyboardInterrupt:
                Log.warn('ES daemon', 'shutdown on user break signal')
                self.shutdown(es)
            except:
                try:
                    import traceback
                    (exc_type, exc_value, exc_trb) = sys.exc_info()
                    out = ''.join(traceback.format_exception(exc_type, 
                        exc_value, exc_trb)[-5:])
                    Log.error('ES daemon', 'Unexpected exception, type',
                        exc_type,
                        '\n--- partial traceback of this exception follows:\n',
                        out,'\n--- end of traceback')
                finally:    
                    del exc_type, exc_value, exc_trb
            Log.msg('ES daemon','Shut down gracefully.')
        else:
            # no loop, store the required objects for 
            # getServerSockets()
            self.daemon=daemon
            self.es=es
            daemon.setTimeout(20)  # XXX fixed timeout

    def mustContinueRunning(self):
        return self.running

    def handleRequests(self, timeout=None):
        # this method must be called from a custom event loop
        self.daemon.handleRequests(timeout=timeout)

    def shutdown(self, es):
        if es:
            # internal shutdown call with specified ES object
            daemon=es.getDaemon()
        else:
            # custom shutdown call w/o specified ES object, 
            # use stored instance
            daemon=self.daemon
            es=self.es
            del self.es, self.daemon
        try:
            daemon.disconnect(es) # clean up nicely
        except NamingError,x:
            Log.warn('ES daemon', 'disconnect error during shutdown:',x)
        except ConnectionClosedError,x:
            Log.warn('ES daemon',
                'lost connection with Name Server, cannot unregister')
        self.running=0
        daemon.shutdown()
