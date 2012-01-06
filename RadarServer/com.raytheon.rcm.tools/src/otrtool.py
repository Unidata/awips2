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
import getopt
import logging
import sys
import pprint
import threading
import time
from xml.dom.minidom import parseString

#import stomp
from ufpy import stomp

def encodeElevation(elev):
    if elev >= 0:
        return min(elev, 1800);
    else:
        if elev > -1800:
            return 3600 + elev
        else:
            return 1801

class Request(object):
    SPECIFIC_ELEVATION = 0;
    LOWER_ELEVATIONS = 1 << 13;
    ALL_ELEVATIONS = 1 << 14;
    LOWER_CUTS = (1 << 14) | (1 << 13)

    def __init__(self):
        self.productCode = 0
        self.pdw = [0,0,0,0,0,0 ]
        self.highPriority = False
        self.mapRequested = False
        self.sequence = 0
        self.count = 1
        self.interval = 1
        self.volumeScanSelection = -1
        self.volumeScanTime = None
       
    def setElevation(self, elevation):
        self.pdw[2] = encodeElevation(elevation)
        
    def selectAllElevations(self, elevation = 0):
        self.pdw[2] = Request.ALL_ELEVATIONS | encodeElevation(elevation)
    
    def selectLowerElevations(self, highest_elevation):
        self.pdw[2] = Request.LOWER_ELEVATIONS | encodeElevation(highest_elevation);
    
    def selectLowerCuts(self, n_cuts):
        self.pdw[2] = Request.LOWER_CUTS | encodeElevation(n_cuts); 
        
    def toXml(self):
        return '''
        <request>
        <productCode>%d</productCode>
        <highPriority>%s</highPriority>
        <mapRequested>%s</mapRequested>
        <sequence>%d</sequence>
        <count>%d</count>
        <interval>%d</interval>
        <volumeScanSelection>%d</volumeScanSelection>
        %s
        <pdw20>%d</pdw20>
        <pdw21>%d</pdw21>
        <pdw22>%d</pdw22>
        <pdw23>%d</pdw23>
        <pdw24>%d</pdw24>
        <pdw25>%d</pdw25>
        </request>
        ''' % tuple([self.productCode, 
               str(self.highPriority).lower(),
               str(self.mapRequested).lower(),
               self.sequence, self.count, self.interval,
               self.volumeScanSelection,
               (self.volumeScanTime is not None) and 
                ('<volumeScanTime>'+str(self.volumeScanTime)+'</volumeScanTime>') or ''
               ] + self.pdw)

class RequestBuilder(object):
    def __init__(self):
        self.r = None
        
    def req(self):
        if self.r is None:
            self.r = Request()
        return self.r
    
    def getRequest(self):
        return self.r
    
    def restart(self):
        self.r = None

exit_cond = None

class CL(stomp.ConnectionListener):

    def __init__(self, conn, xml):
        self.conn = conn
        self.xml = xml

    def on_connecting(self, host_and_port):
        #print 'on_connecting...'
        self.conn.connect(login='',passcode='')

    def on_connected(self, headers, body):
        #print 'on_connected...', headers, body
        
        queueName = '/queue/RadarServer'
        replyQueueName = '/queue/otrtool-' + headers['session']
        self.conn.subscribe(destination = replyQueueName)
        self.conn.send(self.xml, destination = queueName, receipt = 'sent',
                       reply_to = replyQueueName) #receipt='zinger'

    def on_disconnected(self):
        print 'Diconnected'
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
    print """usage: otrtool.py -r <radar ID> [options]
         ... Send OTRs

       otrtool.py --status
         ... Print OTR status in RadarServer log
    
    -r <radar> Radar name.  Multiple radars can be specified.
    
    -p <code>  Radar product code   
    
    -A         Request all elevations
    -a <elev>  Request all elevations matching <elev> (for TDWR mini-volumes).
    -e <elev>  Request single elevation angle
    -l <elev>  Request elevations up to and including <elev>
    -n <cuts>  Request the lowest <cuts> elevations
    
    -C         Request "current" product
    -L         Request "latest available" product
    -S <time>  Request from volume scan <time> (Not implemented yet.)
    
    -c <count>    Request repeat count   (Default: 1)
    -i <interval> Request scan interval  (Default: 1)
    
    -x,
    --show-xml    Print XML messages to standard error. 
      
    -h         Show this help message.
"""

def run():
    global exit_cond
    
    logging.basicConfig()
    
    connections = [('localhost',8814)]
    requests = []
    radarIDs = []
    get_status = False
    show_xml = False
    
    opts, args = getopt.getopt(sys.argv[1:],'ACLS:a:c:e:hi:l:n:p:r:x',['status','show-xml'])
    
    rb = RequestBuilder()
    
    for k, v in opts:
        if False:
            pass
        elif k == '-A':
            rb.req().selectAllElevations()
        elif k == '-L':
            rb.req().volumeScanSelection = -2 # latest available
        elif k == '-C':
            rb.req().volumeScanSelection = -1 # current
        elif k == '-S':
            print >> sys.stderr, "-S not implemented"
            sys.exit(1)
        elif k == 'a':
            rb.req().selectAllElevations(int(float(v) * 10))
        elif k == '-c':
            rb.req().count = int(v)
            if rb.req().count < 1 or rb.req().count > 9:
                # I confuse -c (count) with -p (product code)
                # also, the rpg does not actually limit it to nine
                print >> sys.stderr, "count must in the range 1..9."
                sys.exit(1)
        elif k == '-e':
            rb.req().setElevation(int(float(v) * 10))
        elif k == '-h':
            printHelp()
            sys.exit(0)
        elif k == '-i':
            rb.req().interval = int(v)
        elif k == 'l':
            rb.req().selectLowerElevations(int(float(v) * 10))
        elif k == '-n':
            rb.req().selectLowerCuts(int(v))
        elif k == '-p':
            rb.req().productCode = int(v)
        elif k == '-r':
            radarIDs.append(v.lower())
        elif k == '--status':
            get_status = True
        elif k == '-x' or k == '--show-xml':
            show_xml = True
        else:
            raise "Unknown option '%s'" % k
        
    if rb.getRequest() is not None:
        requests.append(rb.getRequest())
    
    if not get_status:
        if not radarIDs:
            print >> sys.stderr, "No radars specified"
            printHelp()
            sys.exit(1)

        if not requests:
            print >> sys.stderr, "No requests specified"
            printHelp()
            sys.exit(1)
        xml = '''
		<sendOneTimeRequests>
		<radarIDs>%s</radarIDs>
		<!-- requests -->
		%s
		<!-- /requests -->
		</sendOneTimeRequests>
		''' % (' '.join(radarIDs),''.join([r.toXml() for r in requests]))
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
    