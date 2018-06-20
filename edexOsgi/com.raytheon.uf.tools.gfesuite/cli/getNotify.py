#!/usr/bin/env python
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
# Provides a Python-based interface listening to gfe notifications
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/18/10                      njensen       Initial Creation.
#    06/13/13        #2044         randerso      Fixed to use correct python
#    06/23/2015      #4573         randerso      Removed obsolete A1 options, added new options for A2
# 
#

import time, sys
import threading
from collections import OrderedDict

import dynamicserialize
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.server.notify import *
from dynamicserialize.dstypes.com.raytheon.uf.common.activetable import VTECTableChangeNotification

printoutMap = OrderedDict([
    ('L', 'LOCK'),
    ('G', 'GRID'),
    ('D', 'DB'),
    ('U', 'USER'),
    ('V', 'VTEC'),
    ('C', 'COMBO'),
    ('S', 'SBU'),
    ('H', 'HISTORY'),
])

classMap = {
    LockNotification: 'L',
    GridUpdateNotification: 'G',
    DBInvChangeNotification: 'D',
    UserMessageNotification: 'U',
    VTECTableChangeNotification: 'V',
    CombinationsFileChangedNotification: 'C',
    ServiceBackupJobStatusNotification: 'S',
    GridHistoryUpdateNotification: 'H'
}
topicList = ['edex.alerts.gfe', 'edex.alerts.vtec']
messageQueueMap = {}
for letter in printoutMap.keys():
    messageQueueMap[letter] = []


class GetGfeNotifyTopicListener(threading.Thread):
    
    def __init__(self, topicName, hostname, portNumber, L, G, D, U, V, C, S, H):
        self.hostname = hostname
        self.portNumber = portNumber
        self.topicName = topicName
        self.L = L
        self.G = G
        self.D = D
        self.U = U
        self.V = V
        self.C = C
        self.S = S
        self.H = H
        self.qs = None        
        threading.Thread.__init__(self)
            
    def run(self):        
        from awips import QpidSubscriber
        self.qs = QpidSubscriber.QpidSubscriber(self.hostname, self.portNumber)        
        self.qs.topicSubscribe(self.topicName, self.receivedMessage) 
    
    def addMessageToQueue(self, obj, t):
        if classMap.has_key(t):
            messageQueueMap[classMap[t]].append(obj)
        else:
            print "Does not yet support type", t

    def receivedMessage(self, msg):
        try:
            obj = dynamicserialize.deserialize(msg)
            t = type(obj)
            if t is list:
                for notification in obj:
                    self.addMessageToQueue(notification, type(notification))
            else:
                self.addMessageToQueue(obj, t)
        except:
            import traceback
            traceback.print_exc()

    def stop(self):
        self.qs.close()     



def decodeOptions():
    import getopt
    optionlist, arglist = getopt.getopt(sys.argv[1:], 'h:p:lgduvcsH')
    
    badFormat = False
    optionMap = {"hostname": "", "portNumber": 5672, }
    for letter in printoutMap.keys():
        optionMap[letter] = False
    
    for option in optionlist:
        if option[0] == '-h':
            optionMap["hostname"] = option[1]
        elif option[0] == '-p':
            optionMap["portNumber"] = int(option[1])
        elif option[0] == '-l':
            optionMap["L"] = True
        elif option[0] == '-g':
            optionMap["G"] = True
        elif option[0] == '-d':
            optionMap["D"] = True
        elif option[0] == '-u':
            optionMap["U"] = True
        elif option[0] == '-v':
            optionMap["V"] = True
        elif option[0] == '-c':
            optionMap["C"] = True
        elif option[0] == '-s':
            optionMap["S"] = True
        elif option[0] == '-H':
            optionMap["H"] = True
        else:
            badFormat = True
            break
    
    if len(optionMap["hostname"]) == 0 == 0 or badFormat:
        usage()
        return None
    
    return optionMap

def usage():
    s = """
Usage: getNotify -h <hostname> [-p <portNumber>] [-l] [-g] [-d] [-u] [-v] [-c] [-s] [-H]
  -h hostname : upon which the JMS broker is running
  -p portNumber: the port that JMS broker is using, defaults to 5672
  -l: display detailed lock notifications
  -g: display detailed grid update notifications
  -d: display detailed database update notifications
  -u: display detailed user message notifications
  -v: display detailed VTEC notifications
  -c: display detailed combinations file change notifications
  -s: display detailed service backup job status notifications
  -H: display detailed grid history update notifications
    """
    
    print s
    
def printLoop(options):
    try:
        while True:
            time.sleep(3)
            msg = ''
            for letter in printoutMap.keys():
                msg += letter + '=' + str(len(messageQueueMap[letter])) + ","            
            print msg
            for letter in printoutMap.keys():
                if options[letter] and len(messageQueueMap[letter]) > 0:
                    msg = printoutMap[letter] + "=[" + "\n"
                    for obj in messageQueueMap[letter]:
                        msg += str(obj) + "," + "\n"
                        messageQueueMap[letter].remove(obj)
                    msg += ']'
                    print msg
                else:
                    messageQueueMap[letter] = []
            sys.stdout.flush()
    except KeyboardInterrupt:
        pass

def main():
    print "Get Notifications Diagnostic Program"
    
    options = decodeOptions()
    if options is not None:
        threadList = []
        
        for topic in topicList:
            options["topicName"] = topic
            thread = GetGfeNotifyTopicListener(**options)
            threadList.append(thread)
            
        try:
            for thread in threadList:
                thread.start()
            printLoop(options)
        finally:
            for thread in threadList:
                thread.stop()

    
    
if __name__ == '__main__':
    main()
    
        
        