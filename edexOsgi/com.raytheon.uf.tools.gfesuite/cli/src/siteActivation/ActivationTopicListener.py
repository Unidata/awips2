##
##
#
# Listens for and prints site activation messages
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/10/14         #3623        randerso        Initial Creation.
#
## 

import threading
import datetime
import traceback

import dynamicserialize
from dynamicserialize.dstypes.com.raytheon.uf.common.site.notify import ClusterActivationNotification

from awips import QpidSubscriber

class ActivationTopicListener(threading.Thread):
    def __init__(self, hostname='localHost', portNumber='5762', topicName="edex.alerts.siteActivate" ):
        self.hostname = hostname
        self.portNumber = portNumber
        self.topicName = topicName
        self.qs = None
        threading.Thread.__init__(self)
        
    def run(self):
        self.qs = QpidSubscriber.QpidSubscriber(self.hostname, self.portNumber)        
        self.qs.topicSubscribe(self.topicName, self.receivedMessage)
    
    def stop(self):
        self.qs.close()
        
    def receivedMessage(self, msg):
        try:
            obj = dynamicserialize.deserialize(msg)
            print datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S"), obj
            if type(obj) == ClusterActivationNotification:
                self.stop()
        except:
           traceback.print_exc()

