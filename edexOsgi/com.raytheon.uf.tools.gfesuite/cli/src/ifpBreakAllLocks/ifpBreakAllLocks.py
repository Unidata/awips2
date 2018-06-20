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
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import LockChangeRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.server.request import LockTableRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.server.request import LockRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID
from dynamicserialize.dstypes.com.raytheon.uf.common.site.requests import GetActiveSitesRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId
from dynamicserialize import DynamicSerializationManager
import sys
import os
import logging

from awips import ThriftClient
from awips import UsageArgumentParser

#
# Provides a command-line utility to break all locks.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/05/2011      8826          rferrel        Initial Creation.
#    06/12/2013      2099          dgilling       Code cleanup, improve logging.
#    
# 
#

logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s", 
                    datefmt="%H:%M:%S", 
                    level=logging.INFO)
log = logging.getLogger('ifpBreakAllLocks')


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
                log.info('Lock: {} {} {}'.format(lt.getParmId(), lock.getTimeRange(), lock.getWsId().toPrettyString()))
                lr = LockRequest()
                lr.setParmId(lt.getParmId())
                lr.setTimeRange(lock.getTimeRange())
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
    
def getActiveSites(thriftClient):
    sites = thriftClient.sendRequest(GetActiveSitesRequest())
    return sites

def validateArgs():
    parser = UsageArgumentParser.UsageArgumentParser(prog='ifpBreakAllLocks', conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="host",
                      help="The host the ifpServer is running on", 
                      metavar="host")
    parser.add_argument("-p", action="store", type=int, dest="port", 
                      help="The port number the server is using",
                      metavar="port")
    parser.add_argument("-s", action="store", dest="siteID", 
                      help="Site ID",
                      metavar="siteID")
    parser.add_argument("-a", action="store_true", dest="allLocks",
                      help="Break locks on all databases")
    parser.add_argument("-d", action="append", dest="databaseIDs", default=[],
                        help="Break locks on specified database identifier",
                        metavar="databaseID")
    options = parser.parse_args()
        
    if options.host == None:
        if "CDSHOST" in os.environ:
            options.host = os.environ["CDSHOST"]
        else:
            parser.error("Error: host is not specified.")
    if options.port == None:
        if "CDSPORT" in os.environ:
            options.port = int(os.environ["CDSPORT"])
        else:
            parser.error("Error: port is not specified.")
    if options.allLocks == False and not options.databaseIDs:
        parser.error("Error: either -a or -d are required.")
    invalidDbIds = []
    if not options.allLocks:
        for db in options.databaseIDs:
            if not DatabaseID(dbIdentifier=db).isValid():
                invalidDbIds.append(db)
        if invalidDbIds:
            parser.error("Invalid DatabaseIDs specified: {}".format(invalidDbIds))

    return options

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

def main():
    log.info('Break All Locks starting')
    
    options = validateArgs()
    log.info('allLocks= {}, Ids= {}'.format(options.allLocks, options.databaseIDs))
    
    thriftClient = ThriftClient.ThriftClient(options.host, options.port, "/services")
    
    activeSites = []
    try:
        activeSites = getActiveSites(thriftClient)
    except:
        log.exception("Could not retrieve current active sites:")
        sys.exit(1)
    
    if options.siteID and options.siteID in activeSites:
        siteID = options.siteID
    elif not options.siteID and len(activeSites) == 1:
        siteID = activeSites[0]
    else:
        if options.siteID and options.siteID not in activeSites:
            log.error("Invalid site ID {} specified, only sites {} are valid".format(options.siteID, activeSites))
        else:
            log.error("Must use the -s option to specify one of the following sites {}".format(activeSites))
        sys.exit(1)
        
    try:
        officialDbNamesRequest = getOfficialDbNamesRequest(siteID)
        officialDbNameResponse = thriftClient.sendRequest(officialDbNamesRequest)
    except:
        log.exception("Unable to retrieve official databases:")
        sys.exit(1)
    if not officialDbNameResponse.isOkay():
        log.error("Unable to retrieve official databases: ", officialDbNameResponse.message())
        sys.exit(1)
    officialDBs = officialDbNameResponse.getPayload()
        
    try:
        lockTablesRequest = getLockTablesRequest(siteID)
        lockTableResponse = thriftClient.sendRequest(lockTablesRequest)
    except:
        log.exception("Unable to retrieve lock table:")
        sys.exit(1)
    if (not lockTableResponse.isOkay()):
        log.error("Unable to retrieve lock table: ", lockTableResponse.message())
        sys.exit(1)
    lockTables = lockTableResponse.getPayload()
    
    breakRequest = breakAllLocksGRIDRequest(officialDBs, lockTables, options.databaseIDs, options.allLocks)
    if breakRequest:             
        try:
            breakResponse = thriftClient.sendRequest(breakRequest)
        except:
            log.exception("Unhandled exception thrown during break all locks:")
            sys.exit(1)
        if not breakResponse.isOkay():
            log.error('Unable to break all locks.')
            sys.exit(1)
    log.info('Break All Locks Finished')


if __name__ == '__main__':
    main()
