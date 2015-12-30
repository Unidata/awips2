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


from __future__ import with_statement

import getopt, sys, os, time, socket, threading, SocketServer, LogStream, select
import cPickle, stat, tempfile
import iscDataRec
from com.raytheon.edex.plugin.gfe.isc import IRTManager

#
#  Port of msg_send script
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/06/09        1995          bphillip       Initial Creation.
#    
# 
#

ISC_DATA_REC = "iscDataRec"  # Assume it is on the PATH

class Server(object):
    
    class Handler(SocketServer.StreamRequestHandler):
        def handle(self):
            func = cPickle.load(self.rfile)
            print "func: ", func
            if func == 'gettable':
                with self.server.tlock:
                    msg = cPickle.dumps(self.server.table)
                self.wfile.write(msg)
                self.wfile.flush()
            elif func == 'msg_send':
                meta = cPickle.load(self.rfile)
                print "meta: ", meta
                nfiles = cPickle.load(self.rfile)
                print "nfiles: ", nfiles
                fnames = []
                while nfiles > 0:
                    size = cPickle.load(self.rfile)
                    print "reading: ", size
                    fname = tempfile.mktemp()
                    fnames.append(fname)
                    fpout = open(fname, 'wb')  
                    while size > 0:
                        buf = self.rfile.read(min(4096, size))
                        fpout.write(buf)
                        size = size - len(buf)
                        sys.stdout.write('.')
                    print "done size: ", size
                    fpout.close()
                    nfiles = nfiles - 1
                   
                iscDataRec.execIscDataRec(os.path.basename(fname), meta['subject'], fnames) 

                fnames.append(fname) 


    def __init__(self, wanid):
        self.__shutdown = False
        self.wanid = wanid
        self.table = {}
        self.tlock = threading.Lock()
        self.tscan = time.time()

    def discover(self):
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        s.bind(("0.0.0.0", 10000))
        poll_interval = 1.0
        while not self.__shutdown:
            r, w, e = select.select([s], [], [], poll_interval)
            if r:
                data, addr = s.recvfrom(1024, socket.MSG_WAITALL)
                if not data:
                    break
                data = cPickle.loads(data)
                if data[0] == "tcpaddr":
                    with self.tlock:
                        self.table[data[1]] = (addr[0], data[2], data[3])
                        now = time.time()
                        if now - self.tscan > 15:
                            dlist = []
                            for k, v in self.table.iteritems():
                                if now - v[2] > 15:
                                    dlist.append(k)
                            for k in dlist:
                                del self.table[k]
                            self.tscan = now
                #print "received packet: ", self.table


    def broadcast(self):
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        s.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
        s.bind(("<broadcast>", 10000))
        while not self.__shutdown:
            # msg type, wanid, host, port, time of update.
            msg = cPickle.dumps(("tcpaddr", self.wanid, self.addr[1],
                                 time.time()))
            s.sendto(msg, socket.MSG_WAITALL,
                     ("<broadcast>", 10000))
            time.sleep(5)

    def startThreads(self):
        LogStream.logEvent("Starting Threads")
        self.t1 = threading.Thread(target=self.broadcast)
        self.t1.setDaemon(True)
        self.t1.start()
        self.t2 = threading.Thread(target=self.discover)
        self.t2.setDaemon(True)
        self.t2.start()

    def run(self):
        LogStream.logEvent("Running with ShutDown Value:", self.__shutdown)
        tcps = SocketServer.TCPServer(('', 0), self.Handler)
        tcps.table = self.table
        tcps.tlock = self.tlock
        self.addr = tcps.server_address
        self.startThreads()
        
        poll_interval = 2.0
        
        while not self.__shutdown:
            r, w, e = select.select([tcps], [], [], poll_interval)
            if r:
                tcps.handle_request()
                
            if IRTManager.getInstance().isRegistered(self.wanid) == False:
                LogStream.logEvent("Shutting Down GFE Socket Server for site [", self.wanid, "]...")
                self.__shutdown = True
                LogStream.logEvent("Stopping Broadcast thread for site [", self.wanid, "]...")
                self.t1.join()
                LogStream.logEvent("Stopping Discovery thread for site [", self.wanid, "]...")
                self.t2.join()
                LogStream.logEvent("GFE Socket Server for site [", self.wanid, "] shut down")

def serve(wanid):
    s = Server(wanid)
    s.run()

def gettable():
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(("<broadcast>", 10000))
    data, addr = s.recvfrom(1024, socket.MSG_WAITALL)
    if not data:
        pass # something bad happened.
    data = cPickle.loads(data)
    s.close()
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((addr[0], data[2]))
    s.sendall(cPickle.dumps("gettable"), socket.MSG_WAITALL)
    data = s.recv(1024, socket.MSG_WAITALL)
    data = cPickle.loads(data)
    return data

def msg_send(addrs, files, meta):
    
    table = gettable()
    
    for addr in addrs:
        if table.has_key(addr):
            host, port, time = table[addr]
            print "send to: ", host, port
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            s.connect((host, port))
            s.sendall(cPickle.dumps("msg_send"), socket.MSG_WAITALL)
            s.sendall(cPickle.dumps(meta), socket.MSG_WAITALL)
            s.sendall(cPickle.dumps(len(files)), socket.MSG_WAITALL)  
            fp = s.makefile('rw')
            for f in files:
                size = os.stat(f)[stat.ST_SIZE]
                print "sending: ", f, size, "(bytes)"
                s.sendall(cPickle.dumps(size), socket.MSG_WAITALL)
                fpin = open(f, 'rb')
                buf = fpin.read(4096)
                while buf != "":
                    fp.write(buf)
                    buf = fpin.read(4096)
                fpin.close()
                fp.flush()
        else:
            print "No table entry for", addr,"in table"
            print table

# typical args /awips/ops/bin/msg_send -s %SUBJECT -a %ADDRESSES -i %WMOID -c 11 -p 0 -e %ATTACHMENTS
def msg_send_main(args):
    optlist, args = getopt.getopt(args, 'a:e:s:i:c:p:')
    addrs = []
    files = []
    meta = {}
    for opt in optlist:
        k, v = opt
        if k == '-a':
            addrs = v.split(',')
        elif k == '-e':
            files = v.split(',')
        elif k == '-s':
            meta['subject'] = v
        elif k == '-i':
            meta['wmoid'] = v
    msg_send(addrs, files, meta)


def usage(status=0):
    print """Usage:
    mmhs --help          (This message)

    mmhs --server=WANID --iscdr=/path/to/iscDataRec
                              (Start server mode where WANID is awips wan id)

    msg_send [args]   (run in msg_send mode [args are just like msg_send])
    """
    sys.exit(status)

def main():
    global ISC_DATA_REC

    mode = os.path.basename(sys.argv[0])

    if mode == 'msg_send':
        msg_send_main(sys.argv[1:])
        sys.exit(0)

    # server mode
    try:
        optlist, args = getopt.getopt(sys.argv[1:], '',
                                      ['server=', 'iscdr='])

    except (getopt.GetoptError, getopt.error):
        usage(1)

    mhid = ""
    msg_args = []

    for opt in optlist:
        k, v = opt
        if k == '--server':
            mhid = v
        elif k == '--iscdr':
            ISC_DATA_REC = v
        elif k == '--help':
            usage()
        else:
            usage(1)

    serve(mhid)

#if __name__ == "__main__":
#    main()
