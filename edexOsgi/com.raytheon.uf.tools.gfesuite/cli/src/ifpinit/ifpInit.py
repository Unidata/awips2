##
##

import sys
import os

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import SmartInitRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId

from awips import ThriftClient
from awips import UsageArgumentParser


##
# Provides a command-line utility to purge selected GFE grids.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/12/10                      dgilling       Initial Creation.
#    12/08/10        7656          cjeanbap       Moved environment variables to setup.env.
# 
#
## 

def main():
    args = validateArgs()
    
    purgeRequest = createRequest(args.Model, args.site, args.modelTime, args.all)
    thriftClient = ThriftClient.ThriftClient(args.host, args.port, "/services")
    
    try:
        serverResponse = thriftClient.sendRequest(purgeRequest)
    except Exception, ex:
        print "Caught exception submitting SmartInitRequest: ", str(ex)
        sys.exit(1)
    
    if (not serverResponse.isOkay()):
        print "Errors occurred adding smart inits to queue: ", serverResponse.message()
        sys.exit(1)

def validateArgs():
    parser = UsageArgumentParser.UsageArgumentParser(conflict_handler="resolve", prog='ifpInit')
    parser.add_argument("-h", action="store", dest="host",
                      help="ifpServer host name", 
                      metavar="hostname")
    parser.add_argument("-p", action="store", type=int, dest="port", 
                      help="rpc port number of ifpServer",
                      metavar="port")
    parser.add_argument("-t", action="store", dest="modelTime", 
                      help="modelTime string YYYYMMDD_hhmm", default="",
                      metavar="modelTime")
    parser.add_argument("-s", action="store", dest="site", required=True,
                      help="site to run the smart init for",
                      metavar="site")
    parser.add_argument("-u", action="store", dest="userID", 
                      help="The user ID to connect with",
                      metavar="userID")
    parser.add_argument("-a", action="store_true", dest="all", 
                      help="Run calc on all available grids")
    parser.add_argument("Model", action="store",
                      help="name of model to initialize")
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
    

def createRequest(smartInitName, site, modelTime, calcAll):    
    obj = SmartInitRequest()
    
    wsId = WsId(progName="ifpInit")
    
    obj.setWorkstationID(wsId)
    obj.setSiteID(site)
    
    obj.setModuleName(smartInitName)
    obj.setModelTime(modelTime)
    obj.setCalculateAll(calcAll)
    
    return obj
    
if __name__ == '__main__':
    main()