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
import base64
import getopt
import logging
import sys
import pprint
import threading
import time
from xml.dom.minidom import parseString

#import stomp
from ufpy import stomp

exit_cond = None

class CL(stomp.ConnectionListener):

    def __init__(self, conn, xml):
        self.conn = conn
        self.xml = xml

    def on_connecting(self, host_and_port):
        #print 'on_connecting...'
        self.conn.connect(login='', passcode='')

    def on_connected(self, headers, body):
        #print 'on_connected...', headers, body
        
        queueName = '/queue/RadarServer'
        replyQueueName = '/queue/otrtool-' + headers['session']
        self.conn.subscribe(destination=replyQueueName)
        self.conn.send(self.xml, destination=queueName, receipt='sent',
                       reply_to=replyQueueName)

    def on_disconnected(self):
        print 'disconn!'
        #sys.exit(0)
        self.doExit()

    def on_message(self, headers, body):
        # pprint.pprint(body)
        try:
            d = parseString(body)
            n = d.documentElement.getAttributeNode("error")
            if n is not None:
                print >> sys.stderr, "Error: %s" % n.nodeValue
            else:
                print "request sent"
        except:
            print >> sys.stderr, "Error while processing result: %s\n%s\n" % \
                (sys.exc_info()[1], body)
        # stomp.ConnectionListener.on_message(self, headers, body)
        self.doExit()

    def on_receipt(self, headers, body):
        #print 'ok?'        
        #self.conn.disconnect()
        #self.doExit()
        pass

    def on_error(self, headers, body):
        print >> sys.stderr, "STOMP error:", body
        #self.doExit()
        #self.conn.stop()
        #sys.exit(1)
        
    def doExit(self):
        #self.conn.disconnect()
        exit_cond.acquire()
        try:
            exit_cond.notifyAll()
        finally:
            exit_cond.release()

def printHelp():
    print """usage: rpstool.py -r <radar ID> [options] <RPS List file>
    
    -r <radar> Radar name.  Multiple radars can be specified.
    
    -v <vcp>   List contains definitions for the specified VCP.  Not needed
               if it is specified in the file itself.  If the radar is not
               current using the specified VCP, the list will not be sent.   
    
    -s         Permantly store the RPS list as the local custom list.
    
    -x,
    --show-xml    Print XML messages to standard error. 
      
    -h         Show this help message.
    
    <RPS List file> may be in AWIPS 1 format or XML.
"""

def run():
    global exit_cond
    
    logging.basicConfig()
    
    connections = [('localhost', 8814)]
    radarIDs = []
    vcp = None
    store = False
    show_xml = False
    
    opts, args = getopt.getopt(sys.argv[1:], 'hr:sv:x', ['show-xml'])
    
    for k, v in opts:
        if False:
            pass
        elif k == '-h':
            printHelp()
            sys.exit(0)
        elif k == '-r':
            radarIDs.append(v.lower())
        elif k == '-s':
            store = True    
        elif k == '-v':
            vcp = int(v)
        elif k == '-x' or k == '--show-xml':
            show_xml = True
        else:
            raise "Unknown option '%s'" % k

    if len(args) < 1:
        print >> sys.stderr, "RPS list not specified"
        printHelp()
        sys.exit(1)
    elif len(args) > 1:
        printHelp()
        sys.exit(1)
        
    f = open(args[0], "rb")
    data = f.read()
    f.close()
        
    if True:
        if not radarIDs:
            print >> sys.stderr, "No radars specified"
            printHelp()
            sys.exit(1)

        xml = '''
		<sendRpsListData>
		  <radarIDs>%s</radarIDs>
		  %s <!-- <- vcp -->
		  <store>%s</store>
		  <listData>%s</listData>
		</sendRpsListData>
		''' % (' '.join(radarIDs),
			vcp is None and " " or ('<vcp>%d</vcp>' % vcp),
			store, 
			base64.encodestring(data))
    else:
        xml = '<debugCommand command="LOG_OTR_STATUS"/>'
    
    if show_xml:
        print >> sys.stderr, xml

    exit_cond = threading.Condition()

    conn = stomp.Connection(connections)
    conn.add_listener(CL(conn, xml))
    conn.start()
    
    # TODO: if never connects, we never exit
    
    exit_cond.acquire()
    try:
        exit_cond.wait()
    finally:
        exit_cond.release()

    conn.stop()
    sys.exit(0)

if __name__ == '__main__':
    run()
    