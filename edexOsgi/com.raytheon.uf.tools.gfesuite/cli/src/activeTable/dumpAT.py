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
# dumpAT - outputs the active table
# dumpAT -w siteid -p phen -s signif -a action -e etn -n tableName
import getopt, sys
import logging, time, traceback, string, gzip
from dynamicserialize.dstypes.com.raytheon.uf.common.activetable import DumpActiveTableRequest
from ufpy import ThriftClient
from getVtecAttribute import getVtecAttribute

## sorting function
#def sortFunc(r1, r2):
#    #first by issuance time
#    if r1['issueTime'] < r2['issueTime']:
#        return 1
#    elif r1['issueTime'] > r2['issueTime']:
#        return -1
#    #2nd by phen
#    if r1['phen'] < r2['phen']:
#        return -1
#    elif r1['phen'] > r2['phen']:
#        return 1
#    return 0

# usage function
def usage():
    print """
usage: dumpAT - dumps the active table
dumpAT [-w siteid] [-p phen] [-s signif] [-a action] [-e etn] [-n tableName]
 -w siteid:   specifies the site id (e.g., KLWX). 
 -p phen:     specifies the VTEC phenomena code (e.g., WS)
 -s signif:   specifies the VTEC significance (e.g., W). 
 -a action:   specifies the VTEC action code (e.g., NEW)
 -e etn:      specifies the VTEC etn (e.g., 25)
 -l pil:      specifies the VTEC product pil (e.g., WSW)
 -i id:       specifies the VTEC geographical area (e.g., COZ023)
 -n tableName: specifies the VTEC table name (defaults to 'operational')
 -h host      specifies the host from which to obtain the active table (defaults to 'localhost')
 -r port      specifies the port on the host (defaults to 9581)
---
The -w, -p, -s, -l, -a, -i, and -e switches are filters.  If not specified, 
then all entries of that type will be permitted through.  If specified, then
only entries containing that particular value will be printed. These
switches may have multiple entries, e.g., -w KLWX -w KCLE.
"""
    

logging.basicConfig(level=logging.INFO)
log = logging.getLogger("dumpAT")
log.info('*********** dumpAT ****************')
startT = time.time()

sites = []
phens = []
sigs = []
actions = []
etns = []
pils = []
ids = []
tableName = "operational"
fileName = None
host = "localhost"
port = 9581

#--------------------------------------------------------------------
# decode command line, 
# dumpAT -w siteid -p phen -s signif -a action -e etn -i id -n tableName
#--------------------------------------------------------------------
log.info('Cmd: %s'," ".join(sys.argv[1:]))
try:
    optlist, args = getopt.getopt(sys.argv[1:], 'w:p:s:a:e:n:l:i:h:r:f:')
    for opt in optlist:
        if opt[0] == '-w':
            sites.append(opt[1])
        elif opt[0] == '-p':
            phens.append(opt[1])
        elif opt[0] == '-s':
            sigs.append(opt[1])
        elif opt[0] == '-a':
            actions.append(opt[1])
        elif opt[0] == '-i':
            ids.append(opt[1])
        elif opt[0] == '-l':
            pils.append(opt[1])
        elif opt[0] == '-e':
            etns.append(int(opt[1]))
        elif opt[0] == '-n':
            tableName = opt[1]
        elif opt[0] == '-h':
            host = opt[1]
        elif opt[0] == '-r':
            port = int(opt[1])
        elif opt[0] == '-f':
            fileName = opt[1]
except:
    t, v, tb = sys.exc_info()
    tstr = string.join(traceback.format_exception(t, v, tb))
    log.error('Error parsing command line args: %s %s', str(sys.argv), tstr)
    usage()
    raise ValueError('Error parsing command line args')

ourSite = None
try:
    import gfeConfig
    ourSite = gfeConfig.GFESUITE_SITEID
except ImportError:
    import os
    ourSite = os.getenv("GFESUITE_SITEID")
        
request = DumpActiveTableRequest()
request.setFromSite(ourSite)
request.setSites(sites)
request.setPhens(phens)
request.setSigs(sigs)
request.setPils(pils)
request.setIds(ids)
request.setActions(actions)
if fileName:
    # It would be better to decompress on the server, but attempting to pass a 
    # compressed file through Thrift currently corrupts it.
    if fileName.endswith(".gz"):
        fd = gzip.open(fileName, "rb")
    else:
        fd = open(fileName, "rb")
    request.setFileContent(fd.read())
    request.setFileName(fileName)
    request.setMode("FILE")
else:
    request.setMode(tableName.upper())
    if 'ACTIVE' == request.mode:
        request.setMode('OPERATIONAL')
    
thriftClient = ThriftClient.ThriftClient(host, port, "/services")

response = None
log.info("Requesting active table dump from server.")
response = thriftClient.sendRequest(request);
log.info("Response received from server.")
if response is not None:
    if response.message:
        log.error(response.message)
    
    if response.unfilteredCount>0 or response.dump or not response.message: 
        log.info("Number of records (unfiltered): %d", response.unfilteredCount)
        log.info("Number of records (filtered): %d", response.filteredCount)
        log.info("Records: %s", response.dump)
endT = time.time()
log.info('Final: wctime:%-0.2f, cputime:%-0.2f', (endT - startT), time.clock())
    
#--------------------------------------------------------------------
# read active table, filter the records
#--------------------------------------------------------------------
#recs = []
#try:
#    recs = getAT.getActiveTable(tableName, sites, host, port)
#except Exception, exx:
#    log.exception("Error getting active table:")
#        
#filt = []
#for r in recs:
#    if (len(sites) == 0 or r['officeid'] in sites) and \
#      (len(phens) == 0 or r['phen'] in phens) and \
#      (len(sigs) == 0 or r['sig'] in sigs) and \
#      (len(pils) == 0 or r['pil'] in pils) and \
#      (len(ids) == 0 or r['id'] in ids) and \
#      (len(actions) == 0 or r['act'] in actions) and \
#      (len(etns) == 0 or r['etn'] in etns):
#        filt.append(r)
#
##sort records by issuance time later first
#filt.sort(sortFunc)
#
#util = VTECTableUtil.VTECTableUtil()
##--------------------------------------------------------------------
## output
##--------------------------------------------------------------------
#log.info("Number of records (unfiltered): %d", len(recs))
#log.info("Number of records (filtered): %d", len(filt))
#log.info("Records: %s", util.printActiveTable(filt, True))
#
##--------------------------------------------------------------------
## Finish
##--------------------------------------------------------------------
#endT = time.time()
#log.info('Final: wctime:', "%-6.2f" % (endT - startT),
#                       ',cputime:', "%-6.2f" % time.clock())
