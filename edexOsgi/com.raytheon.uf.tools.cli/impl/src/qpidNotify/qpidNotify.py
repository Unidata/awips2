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
##############################################################################

from ufpy import qpidingest
import os
import os.path
import sys

class mhsFileIngest:
    def __init__(self):
        print "Initializing mhsFileIngest object"

    def qpidNotify(*args):
        #Find current QPID running hostname
        server=os.getenv('DEFAULT_HOST')
#        print "Current EDEX server:", server

        #Make connection to QPID
        try:
            cnn=qpidingest.IngestViaQPID(host=server, port=5672)
        except:
            print "Cannot connect qpid server:", server
            sys.exit(1)
        
        #Get uplink files
        size=len(sys.argv) - 1
        print size, "files will be sent to EDEX via qpiningest"
        
        fileCount=0
        errCount=0
        for outfile in sys.argv[1:]:
            #Make sure incoming file exists in /data_store/mhs directory 
#           print "outfle:", outfile
            if os.path.exists(outfile):
                try:
                    f=open(outfile, 'r')
                except(IOerror), e:
                    print "Unable to open the file", outfile, e
                    errCount += 1
                else:
                    #Parse wmoId header info
                    firstLine=f.readline()
                    wmoHdr=firstLine[0:11]
#                    print "WMO header:", wmoHdr
                    f.close()
                    #Send message to the external dropbox queue for file to be ingested
                    cnn.sendmessage(outfile, wmoHdr)
                    print "Sent", outfile, "to EDEX via qpidingest"
                    fileCount += 1
            else:
                print outfile, "does not exist"
                errCount += 1

        cnn.close()
        if fileCount == size:
            print "Successfully sent", fileCount, "file(s) to EDEX via qpidingest"
            sys.exit(0)
        elif errCount == size:
            print "Failed to send", fileCount, "file(s) to EDEX via qpidingest"            
            sys.exit(1)
        elif errCount > 0 and fileCount < size:
            print errcount, "out of", size, "failed to be sent to EDEX via qpidingest"
            sys.exit(2)
   
if __name__ == '__main__':
    q = mhsFileIngest()
    q.qpidNotify()
