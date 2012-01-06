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

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GetOfficialDbNameRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GetLockTablesRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GetActiveSitesRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import LockChangeRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.server.request import LockTableRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.server.request import LockRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID
from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId
from dynamicserialize import DynamicSerializationManager
import sys
import os
import logging

from ufpy import ThriftClient
from ufpy.UsageOptionParser import UsageOptionParser

#
# Provides a command-line utility to break all locks.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/05/2011      8826          rferrel        Initial Creation.
#    
# 
#

def __WsId() :
    return WsId(progName="ifpBreakAllLocks")

def officialDB(id, officialDBs):
    for odb in officialDBs:
        if id.getSiteId() == odb.getSiteId() and \
           id.getDbType() == odb.getDbType() and \
           id.getFormat() == odb.getFormat() and \
           id.getModelId() == odb.getModelId():
            return True
    return False

def breakAllLocksGRIDRequest(officialDBs, lockTables, databaseIDs, allLocks):
    # break all locks (except those from official database)
    req = []
    for lt in lockTables:
        if officialDB(lt.getParmId().getDbId(), officialDBs):
            continue
        if not allLocks and `lt.getParmId().getDbId()` not in databaseIDs:
            continue
        if lt.getParmId().getDbId().getFormat() == "GRID":
            locks = lt.getLocks()
            for lock in locks:
                tr = TimeRange()
                tr.setStart(lock.getStartTime()/1000.0)
                tr.setEnd(lock.getEndTime()/1000.0)
                logInfo('Lock: %s %s' % (lt.getParmId(), tr))
                lr = LockRequest()
                lr.setParmId(lt.getParmId())
                lr.setTimeRange(tr)
                lr.setMode("BREAK_LOCK")
                if len(req) == 0:
                    siteID = lt.getParmId().getDbId().getSiteId()
                req.append(lr)
    if len(req) == 0:
        lockChangeRequest = None
    else:
        lockChangeRequest = LockChangeRequest()
        lockChangeRequest.setRequests(req)
        lockChangeRequest.setSiteID(siteID)
        lockChangeRequest.setWorkstationID(__WsId())
    return lockChangeRequest
        
def __initLogger():
    logger = logging.getLogger("ifpBreakAllLocks.py")
    logger.setLevel(logging.INFO)
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    formatter = logging.Formatter("%(asctime)s %(name)s %(levelname)s:  %(message)s", "%H:%M:%S")
    ch.setFormatter(formatter)
    logger.addHandler(ch)

def logInfo(msg):
    logging.getLogger("ifpBreakAllLocks.py").info(msg)
def logError(msg):
    logging.getLogger("ifpBreakAllLocks.py").error(msg)
    
def validateSiteId(siteId, thriftClient):
    sitesResp = thriftClient.sendRequest(GetActiveSitesRequest())
    if not sitesResp.isOkay():
        logError("Unable to validate siteId")
        sys.exit(1)
    sites = sitesResp.getPayload()
    if not siteId in sites:
        logError('Invalid or not installed siteID: "%s"' % siteId)
        sys.exit(1)

def findSiteID(thriftClient):
    sitesResp = thriftClient.sendRequest(GetActiveSitesRequest())
    if not sitesResp.isOkay():
        logError("Unable to obtain siteId")
        sys.exit(1)
    sites = sitesResp.getPayload()
    if len(sites) > 1 :
        s = []
        while len(sites) > 0 : s.append(sites.pop())
        logError("Must use the -s option to specify one of the following sites: %s " % ", ".join(s))
        sys.exit(1)
    elif len(sites) == 0:
        logError("No sites configured")
        sys.exit(1)
    return sites.pop()
    
def main():
    (options, args) = validateArgs()
    __initLogger()
    logInfo('Break All Locks starting')
    try:
        thriftClient = ThriftClient.ThriftClient(options.host, options.port, "/services")
        siteID = options.siteID
        if siteID:
            validateSiteId(options.siteID, thriftClient)
        else:
            siteID = findSiteID(thriftClient)
        officialDbNamesRequest = getOfficialDbNamesRequest(siteID)
        officialDbNameResponse = thriftClient.sendRequest(officialDbNamesRequest)
        lockTablesRequest = getLockTablesRequest(siteID)
        lockTableResponse = thriftClient.sendRequest(lockTablesRequest)
    except Exception, e:
        logError("Unhandled exception thrown during break all locks: \n %s" % str(e))
        sys.exit(1)
    
    if (not officialDbNameResponse.isOkay()):
        logError("Errors occurred during break all locks: ", officialDbNameResponse.message())
        sys.exit(1)
    officialDBs = officialDbNameResponse.getPayload()
    
    if (not lockTableResponse.isOkay()):
        logError("Errors occurred during break all locks: ", lockTableResponse.message())
        sys.exit(1)
    
    lockTables = lockTableResponse.getPayload()
    breakRequest = breakAllLocksGRIDRequest(officialDBs, lockTables, options.databaseIDs, options.allLocks)
    if not breakRequest:
        logInfo('No locks found')
    else :                
        try :
            breakResponse = thriftClient.sendRequest(breakRequest)
        except Exception, e:
            import traceback
            logError("Unhandled exception thrown during break all locks: \n%s" % str(e))
            print traceback.print_exc(file=sys.stdout)
            sys.exit(1)
            
        if  not breakResponse.isOkay():
            logError('Unable to break all locks.')
            sys.exit(1)
    logInfo('Break All Locks Finished')
        
def validateArgs():
    usage = """%prog -h hostname -p port -s siteID -a -d databaseID ...
    
    \tEither -a or at least one -d is required"""
    parser = UsageOptionParser(usage=usage, conflict_handler="resolve")
    parser.add_option("-h", action="store", type="string", dest="host",
                      help="ifpServer host name", 
                      metavar="hostname")
    parser.add_option("-p", action="store", type="int", dest="port", 
                      help="port number of the ifpServer",
                      metavar="port")
    parser.add_option("-s", action="store", type="string", dest="siteID", 
                      help="Site ID",
                      metavar="siteID")
    parser.set_defaults(allLocks=False)
    parser.add_option("-a", action="store_true", dest="allLocks",
                      help="Break all database identifier's locks.",
                      metavar="allLocks")
    parser.add_option("-d", action="append", type="string", dest="databaseIDs", 
                      help="database identifier",
                      metavar="databaseIDs")
    
    (options, args) = parser.parse_args()
        
    if options.host == None:
        if "CDSHOST" in os.environ:
            options.host = os.environ["CDSHOST"]
        else:
            parser.error("No server hostname defined.")
        
    if options.port == None:
        if "CDSPORT" in os.environ:
            options.port = int(os.environ["CDSPORT"])
        else:
            parser.error("No server port defined.")
        
    if (options.allLocks == False and (options.databaseIDs is None or len(options.databaseIDs) == 0)):
        parser.error("Must have -a or at least one DatabaseID (-d) must be provided.")

    msg = []
    if not options.allLocks:
        for db in options.databaseIDs:
            if not DatabaseID(dbIdentifier=db).isValid():
                msg.append('Invalid database identifier "%s"' % db)
            if len(msg) > 0:
                parser.error("\n".join(msg))
    return (options, args)
    

def getLockTablesRequest(siteID):
    req = GetLockTablesRequest()
    
    req.setWorkstationID(__WsId())
    req.setSiteID(siteID)
    req.setRequests([LockTableRequest()])
    return req
    
def getOfficialDbNamesRequest(siteID):    
    req = GetOfficialDbNameRequest()
    
    req.setWorkstationID(__WsId())
    req.setSiteID(siteID)
    return req

if __name__ == '__main__':
    main()
    