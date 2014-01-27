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
#       AvnThread.py
#       GFS1-NHD:A7967.0000-SCRIPT;1.7
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.7 (DELIVERED)
#         Created:  06-JUL-2005 18:16:35      TROJAN
#           spr 6548
#       
#       Revision 1.6 (DELIVERED)
#         Created:  07-MAY-2005 11:30:36      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.5 (DELIVERED)
#         Created:  11-MAR-2005 15:55:29      TROJAN
#           spr 6717
#       
#       Revision 1.4 (DELIVERED)
#         Created:  23-JAN-2005 18:42:22      TROJAN
#           spr 6604
#       
#       Revision 1.3 (APPROVED)
#         Created:  07-DEC-2004 18:28:38      TROJAN
#           spr 6485
#       
#       Revision 1.2 (APPROVED)
#         Created:  30-SEP-2004 20:22:08      TROJAN
#           stdr 873
#       
#       Revision 1.1 (APPROVED)
#         Created:  19-AUG-2004 21:09:56      OBERFIEL
#           date and time created 08/19/04 21:09:56 by oberfiel
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
# AvnThread.py
# More Pyro stuff specific to AvnFPS
# Author: George Trojan, SAIC/MDL, August 2004
# last update: 06/04/05

import logging, os, socket, threading, time
import Pyro, Pyro.core, Pyro.protocol, Pyro.naming, Pyro.constants
import Pyro.EventService.Clients
from Pyro.errors import *

_Logger = logging.getLogger(__name__)

###############################################################################
# code copied from "Python in a Nutshell" by Alex Martelli
class Worker(threading.Thread):
    requestId = 0
    def __init__(self, requestQueue, resultsQueue, **kwds):
        threading.Thread.__init__(self, **kwds)
        self.setDaemon(1)
        self.workRequestQueue = requestQueue
        self.resultQueue = resultsQueue
        self.start()

    def performWork(self, callable, *args, **kwds):
        Worker.requestId += 1
        self.workRequestQueue.put((Worker.requestId, callable, args, kwds))
        return Worker.requestId

    def run(self):
        while True:
            requestId, callable, args, kwds = self.workRequestQueue.get()
            self.resultQueue.put((requestId, callable(*args, **kwds)))

###############################################################################
# PUBLISHER: publishes events. Registers its name with Name Server.
# The code is copied from Pyro/EventService/Clients.py with modified publish()
# method.
class Publisher:
    def __init__(self, ident=None):
        Pyro.core.initClient()
        locator = Pyro.naming.NameServerLocator(identification=ident)
        try:
            port = int(os.environ['AVN_NS_PORT'])
        except KeyError:
            port = None
        ns = locator.getNS(port=port)
        uri = ns.resolve(Pyro.constants.EVENTSERVER_NAME)
        self.eventservice = Pyro.core.getProxyForURI(uri)
        self.eventservice._setIdentification(ident)
        self.subjects = Pyro.config.PYRO_NS_DEFAULTGROUP
        ns._release()
            
    def publish(self, msg):
        try:
            self.eventservice.publish(self.subjects, msg)
        except PyroError, e:
            _Logger.error(str(e))
            try:
                self.eventservice.adapter.rebindURI(1)
            except PyroError, e:
                _Logger.error(str(e))

###############################################################################
# SUBSCRIBER: subscribes to AvnFPS3x events
# runs in a separate thread, periodically reconnects with the event server
class _Subscriber(Pyro.EventService.Clients.Subscriber):
    def __init__(self, resultsQueue, requestId):
        self.resultQueue = resultsQueue
        self.requestId = requestId
        Pyro.EventService.Clients.Subscriber.__init__(self)

    def event(self, event):
        self.resultQueue.put((self.requestId, event))

class Subscriber(threading.Thread):
    requestId = 0
    def __init__(self, resultsQueue, **kwds):
        threading.Thread.__init__(self, **kwds)
        self.subjects = Pyro.config.PYRO_NS_DEFAULTGROUP
        self.resultQueue = resultsQueue
        self.subscriber = _Subscriber(self.resultQueue, self.requestId)
        self.setDaemon(1)
        self.start()

    def subscribe(self):
        try:
            self.subscriber.subscribe(self.subjects)
        except (ProtocolError, ConnectionClosedError):
            try:
                # 3 times, 1 second wait time
                self.subscriber.eventservice.adapter.rebindURI(5, 1)
                self.subscriber.subscribe(self.subjects)
            except PyroError:
                pass
        except Exception:
            raise

    def run(self):
        while 1:
            try:
                self.subscriber.subscribe(self.subjects)
                break
            except PyroError:
                time.sleep(10.0)
        self.subscriber.listen()

    def abort(self):
        try:
            self.subscriber.unsubscribe(self.subjects)
            self.subscriber.abort()
        except: 
            pass
