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
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/17/10                      njensen       Initial Creation.
#    
# 
#

import qpid

from Queue import Empty
from qpid.exceptions import Closed

class QpidSubscriber:
    
    def __init__(self, host='127.0.0.1', port=5672):
        self.host = host
        self.port = port
        socket = qpid.util.connect(host, port)
        self.__connection = qpid.connection.Connection(sock=socket, username='guest', password='guest')
        self.__connection.start()
        self.__session = self.__connection.session(str(qpid.datatypes.uuid4()))
        self.subscribed = True
    
    def topicSubscribe(self, topicName, callback):
        print "Establishing connection to broker on", self.host
        queueName = topicName + self.__session.name
        self.__session.queue_declare(queue=queueName, exclusive=True, auto_delete=True, arguments={'qpid.max_count':100, 'qpid.policy_type':'ring'})
        self.__session.exchange_bind(exchange='amq.topic', queue=queueName, binding_key=topicName)
        self.__innerSubscribe(queueName, callback)                
    
    def __innerSubscribe(self, serverQueueName, callback):
        local_queue_name = 'local_queue_' + serverQueueName        
        queue = self.__session.incoming(local_queue_name)
        self.__session.message_subscribe(serverQueueName, destination=local_queue_name)
        queue.start()
        print "Connection complete to broker on", self.host
        
        while self.subscribed:
            try:
                message = queue.get(timeout=10)
                content = message.body
                self.__session.message_accept(qpid.datatypes.RangedSet(message.id))                
                callback(content)
            except Empty:
                pass
            except Closed:
                self.close()

    def close(self):
        self.subscribed = False
        try:
            self.__session.close(timeout=10)
        except:
            pass
    
        
        