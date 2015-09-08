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
# Revisions:
# Date            Ticket#       Engineer       Description
# ------------    ----------    -----------    -------------------------------
# 07/15/2015       #4013        randerso       Initial creation
#
##

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import RsyncGridsToCWFRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId

from ufpy import ThriftClient
from ufpy import UsageArgumentParser

import os

def main():
    args = validateArgs()
    
    request = createRequest(args.site)
    thriftClient = ThriftClient.ThriftClient(args.host, args.port, "/services")
    
    try:
        thriftClient.sendRequest(request)
    except Exception, ex:
        print "Caught exception submitting RsyncGridsToCWFRequest: ", str(ex)
        return 1

def validateArgs():
    parser = UsageArgumentParser.UsageArgumentParser(conflict_handler="resolve", prog='ifpInit')
    parser.add_argument("-h", action="store", dest="host",
                      help="ifpServer host name", 
                      metavar="hostname")
    parser.add_argument("-p", action="store", type=int, dest="port", 
                      help="rpc port number of ifpServer",
                      metavar="port")
    parser.add_argument("site", action="store", 
                      help="site to rsync grids for",
                      metavar="site")
    args = parser.parse_args()
    
    if args.host is  None and "CDSHOST" in os.environ:
        args.host = os.environ["CDSHOST"]

    if args.port is None and "CDSPORT" in os.environ:
        args.port = int(os.environ["CDSPORT"])

    if args.host is None: 
        args.host = str(os.getenv("DEFAULT_HOST", "localhost"))

    if args.port is None:
        args.port = int(os.getenv("DEFAULT_PORT", "9581"))
    return args
    

def createRequest(site):    
    request = RsyncGridsToCWFRequest(site)
    
    wsId = WsId(progName="rsyncGridsToCWF")
    
    request.setWorkstationID(wsId)
    
    return request
    
if __name__ == '__main__':
    main()