#!/bin/bash
#
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
##############################################################################
# Provides a simple wrapper to the qpidNotify Python module allowing that module
# to be executed as a command line tool without requiring the .py extension.
#
# Critical: the first line of this file must point to a valid AWIPS II
#           provided installation of Python.
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/19/11        8804          MHuang         Initial creation
#    10/09/12        DR 13901      D. Friedman    Limit execution time
#    02/20/13        DR 15836      D. Friedman    Handle comma-delimited args.
#                                                 Improve logging.
#    05/27/16        DR 16834      D. Friedman    Search for WMO heading.
##############################################################################

from awips import qpidingest
from lib.Util import doWithinTime

import logging
import os
import os.path
import re
from StringIO import StringIO
import sys
import traceback

wmoRE = re.compile(r'([A-Z]{3}[A-Z0-9](?:\d{2}|[A-Z]{2}) [A-Z0-9]{4} \d{6}(?: [A-Z]{3})?)[^\r\n]*[\r]*[\n]')

def getMessageHeader(data):
    m = wmoRE.search(data)
    if m is not None:
        return m.group(1)
    else:
        return StringIO(data).readline()

class mhsFileIngest:
    def startConnection(self):
        #Find current QPID running hostname
        server=os.getenv('DEFAULT_HOST')

        #Make connection to QPID
        try:
            cnn=qpidingest.IngestViaQPID(host=server, port=5672)
        except:
            log.error("Cannot connect qpid server: %s", server)
            sys.exit(1)

        self.conn = cnn

    def qpidNotify(self):
        cnn = self.conn
        
        #Get uplink files
        #
        # If using WAN backup for the SBN, the list of files is
        # passed as a single comma-delimited argument.
        if len(sys.argv) == 2 and ',' in sys.argv[1]:
            args = sys.argv[1].split(',')
        else:
            args = sys.argv[1:]

        size=len(args)
        log.info("%d files will be sent to EDEX via qpiningest", size)
        
        fileCount=0
        errCount=0
        for outfile in args:
            #Make sure incoming file exists in /data_store/mhs directory 
#           print "outfle:", outfile
            if os.path.exists(outfile):
                try:
                    f=open(outfile, 'r')
                except(IOError), e:
                    log.error("Unable to open the file %s: %s", outfile, e)
                    errCount += 1
                else:
                    #Parse wmoId header info
                    header=getMessageHeader(f.read(256))
#                    print "WMO header:", wmoHdr
                    f.close()
                    #Send message to the external dropbox queue for file to be ingested
                    cnn.sendmessage(outfile, header)
                    log.info("Sent %s to EDEX via qpidingest with header '%s'", outfile, header)
                    fileCount += 1
            else:
                log.error("%s does not exist", outfile)
                errCount += 1

        cnn.close()
        if fileCount == size:
            log.info("Successfully sent %d file(s) to EDEX via qpidingest", fileCount)
            return 0
        elif errCount == size:
            log.info("Failed to send %d file(s) to EDEX via qpidingest", fileCount)            
            return 1
        elif errCount > 0 and fileCount < size:
            log.info("%d out of %d failed to be sent to EDEX via qpidingest", errCount, size)
            return 2

def run():
    global log
    logging.basicConfig(level=logging.INFO, datefmt='%H:%M:%S',
                        format='[%(process)s] %(asctime)s %(levelname)s: %(message)s')
    log = logging.getLogger('qpidNotify')
    try:
        m = mhsFileIngest()
        doWithinTime(m.startConnection, description='connect to qpid')
        exit_code = doWithinTime(m.qpidNotify, description='send messages', max_tries=1)
    except:
        traceback.print_exc()
        sys.exit(1)
    else:
        sys.exit(exit_code)

if __name__ == '__main__':
    run()
