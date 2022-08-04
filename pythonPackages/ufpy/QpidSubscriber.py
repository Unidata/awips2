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
# Provides a Python-based interface for subscribing to qpid queues and topics.
#   
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer     Description
# ------------- -------- ------------ --------------------------------------------
# Nov 17, 2010           njensen      Initial Creation.
# Aug 15, 2013  2169     bkowal       Optionally gzip decompress any data that is read.
# Aug 04, 2016  2416     tgurney      Add queueStarted property
# Feb 16, 2017  6084     bsteffen     Support ssl connections
# Sep 07, 2017  6175     tgurney      Remove "decompressing" log message
# Jul 23, 2019  7724     mrichardson  Upgrade Qpid to Qpid Proton
# Nov 04, 2019  7724     tgurney      Fix topic creation
# Jun 24, 2020  8187     randerso     Added qpid connection_id
#

from __future__ import print_function
import logging

import os
import os.path
import pwd
import socket
import zlib
from ssl import SSLContext, PROTOCOL_TLS

from proton import SSLDomain
from proton.handlers import MessagingHandler
from proton.reactor import Container

logging.basicConfig(level=logging.INFO, datefmt='%H:%M:%S',
                    format='[%(process)s] %(asctime)s %(levelname)s: %(message)s')
log = logging.getLogger('QpidSubscriber')

SSL_PASSWORD = 'password'
QPID_USERNAME = 'guest'
QPID_PASSWORD = 'guest'

class QpidSubscriber(MessagingHandler):
    
    def __init__(self, host='127.0.0.1', port=5672, decompress=False, ssl=None, program="QpidSubscriber"):
        super(QpidSubscriber, self).__init__(auto_accept=True)
        #__init__ should only handle setting up properties;
        # any connection and subscription actions should be handled
        # by the reactor functions
        
        self.queues = {}
        
        self.scheme = 'amqp'
        self.rest_scheme = 'https'
        self.ssl_context = None
        self.host = host
        self.port = port
        self.decompress = decompress
        self.__queueStarted = False
        self.__subscribed = False
        
        pwuid = pwd.getpwuid(os.getuid())
        if "QPID_SSL_CERT_DB" in os.environ:
            certdbloc = os.environ["QPID_SSL_CERT_DB"]
        else:
            certdbloc = pwuid.pw_dir + "/.qpid/"
        if "QPID_SSL_CERT_NAME" in os.environ:
            certname = os.environ["QPID_SSL_CERT_NAME"]
        else:
            certname = QPID_USERNAME
        
        certfile = os.path.join(certdbloc, certname + ".crt")
        certkey = os.path.join(certdbloc, certname + ".key")
        if ssl or (ssl is None and os.path.isfile(certfile) and os.path.isfile(certkey)):
            self.scheme = "amqps"
            self.rest_scheme = 'https'
            self.ssl_context = SSLContext(PROTOCOL_TLS)
            self.ssl_context.load_cert_chain(certfile, certkey)
            self.cert_file = certfile
            self.cert_key = certkey
        self.url = '{}://{}:{}@{}:{}'.format(self.scheme, QPID_USERNAME, QPID_PASSWORD, self.host, self.port)
        self.clientID = ":".join([
            socket.gethostname(), 
            pwuid.pw_name, 
            program, 
            str(os.getpid()), 
        ])

    def topicSubscribe(self, topicName, callback):
        self.topicName = topicName
        self.callback = callback
        Container(self).run()

    def on_start(self, event):
        '''
        # if the queue is edex.alerts, set decompress to true always for now to
        # maintain compatibility with existing python scripts.
        '''
        if self.topicName == 'edex.alerts':
            self.decompress = True

        self.container = event.container
        queueName = 'amq.topic/' + self.topicName

        self.ssl_domain = None
        if self.scheme == "amqps" and self.cert_file and self.cert_key:
            self.ssl_domain = SSLDomain(mode=SSLDomain.MODE_CLIENT)
            self.ssl_domain.set_credentials(self.cert_file, self.cert_key, SSL_PASSWORD)

        event.container.container_id = self.clientID
        self.conn = event.container.connect(self.url, ssl_domain=self.ssl_domain)
        self.receiver = event.container.create_receiver(self.conn, queueName)
        self.__queueStarted = True
        self.__subscribed = True

    def on_message(self, event):
        message = event.message
        content = message.body
        self.process_message(content)
        if not self.__subscribed:
            self.close()

    def process_message(self, content):
        if (self.decompress):
            try:
                # http://stackoverflow.com/questions/2423866/python-decompressing-gzip-chunk-by-chunk
                d = zlib.decompressobj(16 + zlib.MAX_WBITS)
                content = d.decompress(content)
            except Exception:
                # decompression failed, return the original content
                pass
        self.callback(content)

    def close(self):
        self.__queueStarted = False
        self.unsubscribe()
        try:
            self.receiver.close()
            self.conn.close()
        except:
            # already closed
            pass

    @property
    def queueStarted(self):
        return self.__queueStarted

    @property
    def subscribed(self):
        return self.__subscribed

    def unsubscribe(self):
        self.__subscribed = False
