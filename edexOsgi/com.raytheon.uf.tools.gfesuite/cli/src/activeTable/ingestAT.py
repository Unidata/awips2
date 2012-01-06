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
import tempfile, os, stat, getopt, sys
import logging, time, traceback, string, MergeVTEC

logging.basicConfig()
log = logging.getLogger("ingestAT")
log.info('************* ingestAT ************************')

startT = time.time()


#--------------------------------------------------------------------
# decode command line   ingestAT -f remoteActiveTable [-z drtMode]
#                                     -a activeTableName [-n] -X xmlInfo
#                                     -s site
#--------------------------------------------------------------------
log.info('Cmd: %s', str(sys.argv[1:]))
fname = None
ztime = None
atName = 'OPERATIONAL'
xmlIncoming = None
makeBackups = 1
ourSites = None
host = 'localhost'
port = 9581

try:
    optlist, args = getopt.getopt(sys.argv[1:], 'f:z:a:nX:s:h:p:')
    for opt in optlist:
        if opt[0] == "-f":
            fname = opt[1]
        elif opt[0] == "-z":
            ztime = opt[1]
        elif opt[0] == "-a":
            atName = opt[1]
        elif opt[0] == "-n":
            makeBackups = 0
        elif opt[0] == "-X":
            fd = open(opt[1], 'rb') #open the xml source file
            xmlIncoming = fd.read()
            fd.close()
            os.remove(opt[1])       #delete the incoming xml file
        elif opt[0] == "-s":
            if ourSites is None:
                ourSites = []
            ourSites.append(opt[1])
        elif opt[0] == "-h":
            host = opt[1]
        elif opt[0] == "-p":
            port = int(opt[1])
except:
    t, v, tb = sys.exc_info()
    tstr = string.join(traceback.format_exception(t, v, tb))
    log.error('Error parsing command line args: %s %s', str(sys.argv), tstr)
    sys.exit(1)

try:
    inputIsGZIP = 1
    removeRemoteFile = False
    MergeVTEC.merge(removeRemoteFile, fname, atName, inputIsGZIP, drt=ztime,
      makeBackups=makeBackups, xmlIncoming=xmlIncoming, ourSites=ourSites,
      host=host, port=port)
    removeRemoteFile = True
except:
    t, v, tb = sys.exc_info()
    tstr = string.join(traceback.format_exception(t, v, tb))
    log.error("MergeVTEC fail: " + tstr)

try:
    if removeRemoteFile:
        os.remove(fname)
except OSError:
    pass

#--------------------------------------------------------------------
# Finish
#--------------------------------------------------------------------
endT = time.time()
log.info('Final: wctime: %-0.2f, cputime: %-0.2f',(endT - startT), time.clock())
