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
# Logger for logging from multiple pypies processes at once.  Should be run as a unique
# process.  Inspired by
# http://docs.python.org/library/logging.html#logging-to-a-single-file-from-multiple-processes
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/20/10                      njensen       Initial Creation.
#    01/17/13        1490          bkowal        Retrieves the logging tcp port
#                                                from configuration instead of
#                                                using the default.
# 
#


import cPickle
import logging
import logging.handlers
import SocketServer
import struct

LOG_THRESHOLD = 0.3 # seconds

import logConfig
logCfg = logConfig.LogConfig()

class LogRecordStreamHandler(SocketServer.StreamRequestHandler):
    """Handler for a streaming logging request.

    This basically logs the record using whatever logging policy is
    configured locally.
    """
    
    import StatsThread            
    statsThread = StatsThread.StatsThread(logCfg)    
    statsThread.start()
    SECTION_KEYS = StatsThread.SECTION_KEYS

    def handle(self):
        """
        Handle multiple requests - each expected to be a 4-byte length,
        followed by the LogRecord in pickle format. Logs the record
        according to whatever policy is configured locally.
        """
        while True:
            try:
                chunk = self.connection.recv(4)
                if len(chunk) < 4:
                    break
                slen = struct.unpack(">L", chunk)[0]
                chunk = self.connection.recv(slen)
                while len(chunk) < slen:
                    chunk = chunk + self.connection.recv(slen - len(chunk))
                obj = self.unPickle(chunk)
                msg = obj['msg']
                if type(msg) is str:
                    record = logging.makeLogRecord(obj)
                    self.handleLogRecord(record)
                else:
                    self.statsThread.addRecord(msg)
                    timeDict = msg['time']                   
                    if timeDict['total'] > LOG_THRESHOLD:                    
                        #obj['msg'] = 'Processed ' + msg['request'] + ' on ' + msg['file'] + ' in ' + ('%.3f' % msg['time']['total']) + ' seconds'
                        logMsg = 'Processed ' + msg['request'] + ' on ' + msg['file'] + '. Timing entries in seconds: '
                        addComma=False
                        for SECTION in self.SECTION_KEYS:
                            timeKey=SECTION.strip()
                            if timeDict.has_key(timeKey):
                                if addComma:
                                    logMsg += ','
                                else:
                                    addComma = True
                                logMsg += ' ' + timeKey + ' ' + ('%.3f' % timeDict[timeKey])
                                
                        obj['msg'] = logMsg
                        record = logging.makeLogRecord(obj)
                        self.handleLogRecord(record)
            except Exception, e:
                import sys, traceback, string
                t, v, tb = sys.exc_info()
                print string.join(traceback.format_exception(t, v, tb))     
                

    def unPickle(self, data):
        return cPickle.loads(data)

    def handleLogRecord(self, record):        
        logCfg.getPypiesLogger().handle(record)

class LogRecordSocketReceiver(SocketServer.ThreadingTCPServer):
    """simple TCP socket-based logging receiver suitable for testing.
    """

    allow_reuse_address = 1

    def __init__(self, host='localhost',
                 port=logCfg.getLoggingPort(),
                 handler=LogRecordStreamHandler):
        SocketServer.ThreadingTCPServer.__init__(self, (host, port), handler)
        self.abort = 0
        self.timeout = 1
        self.logname = None

    def serve_until_stopped(self):
        logCfg.getPypiesLogger().info('Starting log process')
        import select
        abort = 0
        while not abort:
            rd, wr, ex = select.select([self.socket.fileno()],
                                       [], [],
                                       self.timeout)
            if rd:
                self.handle_request()
            abort = self.abort               

def main():
    tcpserver = LogRecordSocketReceiver()    
    print "About to start TCP server..."
    tcpserver.serve_until_stopped()
    print "Stopping"

if __name__ == "__main__":
    main()

