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
# Provides a command-line utility to purge selected GFE grids.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/23/10                      dgilling       Initial Creation.
#    03/07/13         1759         dgilling       Modified to support refactored
#                                                 PurgeGfeGridsRequest.
#
# 
#


import logging
import os
import sys

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import PurgeGfeGridsRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId
from ufpy import ThriftClient
from ufpy import UsageArgumentParser
from ufpy.UsageArgumentParser import StoreDatabaseIDAction as StoreDatabaseIDAction


logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s", 
                    datefmt="%H:%M:%S", 
                    level=logging.INFO)
log = logging.getLogger('purgeAllGrids')



def main():
    options = validateArgs()
    log.debug("Command-line args: " + repr(options))
        
    try:
        purgeRequest = createRequest(options.databaseID)
        log.debug("Sending request: " + str(purgeRequest))
        thriftClient = ThriftClient.ThriftClient(options.host, options.port, "/services")
        serverResponse = thriftClient.sendRequest(purgeRequest)
    except Exception as e:
        log.error("Unhandled exception thrown during grid purge: \n" + str(e))
        sys.exit(1)
    
    if not serverResponse:
        log.error("Errors occurred during grid purge: " + serverResponse.message())
        sys.exit(1)

def validateArgs():
    usage = "%(prog)s -h hostname -p port -d databaseID"
    parser = UsageArgumentParser.UsageArgumentParser(prog='purgeAllGrids',
                usage=usage, conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="host",
                help="ifpServer host name", 
                metavar="hostname")
    parser.add_argument("-p", action="store", type=int, dest="port", 
                help="port number of the ifpServer",
                metavar="port")
    parser.add_argument("-d", action=StoreDatabaseIDAction, dest="databaseID",
                required=True, help="database identifier",
                metavar="databaseID")
    
    options = parser.parse_args()

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
    
    return options
    

def createRequest(dbId):    
    obj = PurgeGfeGridsRequest()
    obj.setDatabaseID(dbId)   
    obj.setWorkstationID(WsId(progName="purgeAllGrids"))
    obj.setSiteID(dbId.getSiteId())
    
    return obj

if __name__ == '__main__':
    main()
    