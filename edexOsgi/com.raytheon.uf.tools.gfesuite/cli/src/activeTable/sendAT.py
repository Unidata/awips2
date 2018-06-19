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

# sendAT - sends active table to remote site
# sendAT -s reqSite -a mhsSite -f filterSite -f filterSite -f filterSite...
#   [-c countDict] [-t timeStamp] -v vtecData  [-X serverXMLInfo]
#   -H serverhost -P serverPort -L serverProtocol -M serverMHS -S serverSite
#   -x xmtScript

#
# Port of sendAT code from AWIPS1
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/28/13        1447          dgilling       Initial Creation.
# 
#


import logging
import sys

from dynamicserialize.dstypes.com.raytheon.uf.common.activetable.request import SendActiveTableRequest
from ufpy import ThriftClient
from ufpy import UsageArgumentParser


logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s", 
                    datefmt="%H:%M:%S", 
                    level=logging.INFO)
log = logging.getLogger('sendtAT')


#--------------------------------------------------------------------
# decode command line, -s siteToSend -f filterSite -a mhsSite
#    -c countDict (siteid:records)  -t issueTime
#    -v vtecData  [-X serverXMLInfo] -x transmitScript
#--------------------------------------------------------------------
def process_command_line():
    parser = UsageArgumentParser.UsageArgumentParser(prog='sendAT', conflict_handler="resolve")
    parser.add_argument("-s", action="append", dest="sites",
                        metavar="siteToSend")
    parser.add_argument("-f", action="append", dest="filterSites",
                        metavar="filterSite")
    parser.add_argument("-a", action="append", dest="mhsSites",
                        metavar="mhsSite")
    parser.add_argument("-t", action="store", type=float, dest="issueTime",
                        metavar="issueTime")
    parser.add_argument("-c", action="store", dest="countDict",
                        metavar="countDict")
    parser.add_argument("-v", action="store", dest="fname",
                        metavar="vtecData")
    parser.add_argument("-X", action="store", dest="xmlIncoming",
                        metavar="serverXMLInfo")
    parser.add_argument("-H", action="store", dest="myServerHost",
                        metavar="ourHost")
    parser.add_argument("-P", action="store", type=int, dest="myServerPort",
                        metavar="ourPort")
    parser.add_argument("-L", action="store", dest="myServerProtocol",
                        metavar="ourProto")
    parser.add_argument("-M", action="store", dest="myServerMHSID",
                        metavar="ourMHSID")
    parser.add_argument("-S", action="store", dest="myServerSite",
                        metavar="ourSiteID")
    parser.add_argument("-x", action="store", dest="xmtScript",
                        metavar="transmitScript")
    
    args = parser.parse_args()
    if args.countDict is not None:
        exec "countDict = " + args.countDict
        setattr(args, "countDict", countDict)
    return args

def build_request(args):
    req = SendActiveTableRequest(args.myServerHost, args.myServerPort, 
                                 args.myServerProtocol, args.myServerSite, 
                                 args.myServerMHSID, args.sites, 
                                 args.filterSites, args.mhsSites, 
                                 args.issueTime, args.countDict, args.fname, 
                                 args.xmlIncoming, args.xmtScript)
    return req

def main():
    options = process_command_line()
    log.debug("Command-line options: " + repr(options))
    
    req = build_request(options)
    log.debug("Request: " + repr(req))
    
    thriftClient = ThriftClient.ThriftClient(host=options.myServerHost)
    try:
        response = thriftClient.sendRequest(req)
    except:
        log.exception("Error posting request.")
        sys.exit(1)
        
    if not response.getTaskSuccess():
        log.error("Error executing sendAT: " + response.getErrorMessage())
        sys.exit(1)

if __name__ == '__main__':
    main()
