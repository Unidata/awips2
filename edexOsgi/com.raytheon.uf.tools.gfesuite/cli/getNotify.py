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
#    
# 
#

import time, sys
import threading

import dynamicserialize
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.server.notify import *
from dynamicserialize.dstypes.com.raytheon.uf.common.activetable import VTECTableChangeNotification

letterList = ['L', 'G', 'D', 'S', 'R', 'C', 'T', 'U', 'B', 'V']
printoutMap = {'L': 'LOCK',
                    'G': 'GRID',
                    'D': 'DB',
                    'S': 'SAMPLE',
                    'R': 'REF',
                    'C': 'COLOR',
                    'T': 'TEXT',
                    'U': 'USER',
                    'B': 'PROC',
                    'V': 'VTEC' }
classMap = {
            LockNotification: 'L',
            GridUpdateNotification: 'G',
            DBInvChangeNotification: 'D',            
            UserMessageNotification: 'U',
            VTECTableChangeNotification: 'V',                
            }
topicList = ['edex.alerts.gfe', 'edex.alerts.vtec']
messageQueueMap = {}
for letter in letterList:
    messageQueueMap[letter] = []


class GetGfeNotifyTopicListener(threading.Thread):
    
    def __init__(self, topicName, hostname, portNumber, user, L, G, D, S, R, C, T, U, B, V):
        self.hostname = hostname
        self.portNumber = portNumber
        self.user = user
        self.topicName = topicName
        self.L = L
        self.G = G
        self.D = D
        self.S = S
        self.R = R
        self.C = C
        self.T = T
        self.U = U
        self.B = B
        self.V = V
        self.qs = None        
        threading.Thread.__init__(self)
            
    def run(self):        
        from awips import QpidSubscriber
        self.qs = QpidSubscriber.QpidSubscriber(self.hostname, self.portNumber)        
        self.qs.topicSubscribe(self.topicName, self.receivedMessage) 
    
    def addMessageToQueue(self, obj, t):
        if classMap.has_key(t):
            messageQueueMap[classMap[t]].append(obj)
#        else:
#            print "Does not yet support type", t

    def receivedMessage(self, msg):
        obj = dynamicserialize.deserialize(msg)
        t = type(obj)
        if t is list:
            for notification in obj:
                self.addMessageToQueue(notification, type(notification))
        else:
            self.addMessageToQueue(obj, t)

    def stop(self):
        self.qs.close()     



def decodeOptions():
    import getopt
    optionlist, arglist = getopt.getopt(sys.argv[1:], 'h:p:u:lgdsrcbmtv')
    
    badFormat = False
    optionMap = {"hostname": "", "portNumber": 0, "user": None}
    for letter in letterList:
        optionMap[letter] = False
    
    for option in optionlist:
        if option[0] == '-l':
            optionMap["L"] = True
        elif option[0] == '-g':
            optionMap["G"] = True
        elif option[0] == '-v':
            optionMap["V"] = True
        elif option[0] == '-d':
            optionMap["D"] = True
        elif option[0] == '-s':
            optionMap["S"] = True
        elif option[0] == '-r':
            optionMap["R"] = True
        elif option[0] == '-c':
            optionMap["C"] = True
        elif option[0] == '-t':
            optionMap["T"] = True
        elif option[0] == '-b':
            optionMap["B"] = True
        elif option[0] == '-m':
            optionMap["U"] = True
        elif option[0] == '-p':
            optionMap["portNumber"] = int(option[1])
        elif option[0] == '-h':
            optionMap["hostname"] = option[1]
        elif option[0] == '-u':
            optionMap["user"] = option[1]
        else:
            badFormat = True
            break
    
    if len(optionMap["hostname"]) == 0 or optionMap["portNumber"] == 0 or badFormat:
        usage()
        return None
    
    return optionMap

def usage():
    s = """
Usage: getNotify -h <hostname> -p <portNumber> [-u <user>]
[-l] [-g] [-d] [-s] [-r] [-c] [-m] [-t] [-b] [-v]
  -h hostname : upon which the ifpServer is running
  -p portNumber: the RPC port that ifpServer is serving
  -u user: user to connect to ifpServer
  -l: display detailed lock notifications
  -g: display detailed grid update notifications
  -d: display detailed database update notifications
  -s: display detailed sample notifications
  -r: display detailed edit area notifications
  -c: display detailed color table notifications
  -b: display detailed server process notifications
  -m: display detailed user message notifications
  -t: display detailed TEXT notifications
  -v: display detailed VTEC notifications
    """
    
    print s
    
def printLoop(options):
    try:
        while True:
            time.sleep(3)
            msg = ''
            for letter in letterList:
                msg += letter + '=' + str(len(messageQueueMap[letter])) + ","            
            print msg
            for letter in letterList:
                if options[letter] and len(messageQueueMap[letter]) > 0:
                    msg = printoutMap[letter] + "=[" + "\n"
                    for obj in messageQueueMap[letter]:
                        msg += str(obj) + "," + "\n"
                        messageQueueMap[letter].remove(obj)
                    msg += ']'
                    print msg
                else:
                    messageQueueMap[letter] = []
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
    
        
        