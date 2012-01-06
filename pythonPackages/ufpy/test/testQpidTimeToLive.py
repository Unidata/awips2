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
# 
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/09/11                      njensen       Initial Creation.
#    
# 
#

import time, sys
import threading

import dynamicserialize

TIME_TO_SLEEP = 300

class ListenThread(threading.Thread):

    def __init__(self, hostname, portNumber, topicName):
        self.hostname = hostname
        self.portNumber = portNumber
        self.topicName = topicName
        self.nMessagesReceived = 0
        self.waitSecond = 0
        self.stopped = False
        threading.Thread.__init__(self)
    
    def run(self):
        from ufpy import QpidSubscriber
        self.qs = QpidSubscriber.QpidSubscriber(self.hostname, self.portNumber)        
        self.qs.topicSubscribe(self.topicName, self.receivedMessage)
    
    def receivedMessage(self, msg):
        print "Received message"
        self.nMessagesReceived += 1
        if self.waitSecond == 0:
            fmsg = open('/tmp/rawMessage', 'w')
            fmsg.write(msg)
            fmsg.close()
        
        while self.waitSecond < TIME_TO_SLEEP and not self.stopped:
            if self.waitSecond % 60 == 0:
                print time.strftime('%H:%M:%S'), "Sleeping and stuck in not so infinite while loop"
            self.waitSecond += 1
            time.sleep(1)
        
        print time.strftime('%H:%M:%S'), "Received", self.nMessagesReceived, "messages"                     
        
    def stop(self):
        print "Stopping"
        self.stopped = True
        self.qs.close()
        
        

def main():
    print "Starting up at", time.strftime('%H:%M:%S')
    
    topic = 'edex.alerts'
    host = 'localhost'
    port = 5672
    
    thread = ListenThread(host, port, topic)    
    try:
        thread.start()
        while True:
            time.sleep(3)
    except KeyboardInterrupt:
        pass
    finally:        
        thread.stop()
    
    
if __name__ == '__main__':
    main()
    
        
        
