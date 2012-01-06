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

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import PurgeGfeGridsRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId
from dynamicserialize import DynamicSerializationManager
import sys
import os
from ufpy import ThriftClient
from ufpy.UsageOptionParser import UsageOptionParser

#
# Provides a command-line utility to purge selected GFE grids.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/23/10                      dgilling       Initial Creation.
#    
# 
#


def main():
    (options, args) = validateArgs()
        
    try:
        purgeRequest = createRequest()
        thriftClient = ThriftClient.ThriftClient(options.host, options.port, "/services")
        serverResponse = thriftClient.sendRequest(purgeRequest)
    except Exception, e:
        print >> sys.stderr, "Unhandled exception thrown during grid purge: \n", str(e)
        sys.exit(1)
    
    if (not serverResponse.isOkay()):
        print >> sys.stderr, "Errors occurred during grid purge: ", serverResponse.message()
        sys.exit(1)

def validateArgs():
    usage = "%prog -h hostname -p port -d databaseID"
    parser = UsageOptionParser(usage=usage, conflict_handler="resolve")
    parser.add_option("-h", action="store", type="string", dest="host",
                      help="ifpServer host name", 
                      metavar="hostname")
    parser.add_option("-p", action="store", type="int", dest="port", 
                      help="port number of the ifpServer",
                      metavar="port")
    parser.add_option("-d", action="store", type="string", dest="databaseID", 
                      help="database identifier",
                      metavar="databaseID")
    
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
            
    if (options.databaseID is None):
        parser.error("-d: At least one DatabaseID must be provided.")
    
    return (options, args)
    

def createRequest():    
    obj = PurgeGfeGridsRequest()
    
    wsId = WsId(progName="purgeAllGrids")
    
    obj.setWorkstationID(wsId)
    obj.setSiteID("")
    obj.setArgString(" ".join(sys.argv[1:]))
    return obj

if __name__ == '__main__':
    main()
    