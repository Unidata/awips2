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
# Port of requestAT code from AWIPS1
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

from dynamicserialize.dstypes.com.raytheon.uf.common.activetable.request import RetrieveRemoteActiveTableRequest

from ufpy import ThriftClient
from ufpy import UsageArgumentParser


logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s", 
                    datefmt="%H:%M:%S", 
                    level=logging.INFO)
log = logging.getLogger('requestAT')


#--------------------------------------------------------------------
# command line: requestAT -H ourHost -P ourPort -L ourProto -M mhsid -s siteID
#   -t irtServiceAddr -x xmtScript
#--------------------------------------------------------------------
def process_command_line():
    parser = UsageArgumentParser.UsageArgumentParser(prog='requestAT', conflict_handler="resolve")
    parser.add_argument("-H", action="store", dest="serverHost",
                        required=True, metavar="ourHost")
    parser.add_argument("-P", action="store", type=int, dest="serverPort",
                        required=True, metavar="ourPort")
    parser.add_argument("-L", action="store", dest="serverProtocol",
                        required=True, metavar="ourProto")
    parser.add_argument("-M", action="store", dest="mhsid",
                        required=True, metavar="mhsid")
    parser.add_argument("-S", action="store", dest="siteID",
                        required=True, metavar="siteID")
    parser.add_argument("-a", action="store", dest="ancf")
    parser.add_argument("-b", action="store", dest="bncf")
    parser.add_argument("-x", action="store", dest="xmtScript",
                        metavar="xmtScript")
    return parser.parse_args()

def build_request(args):
    req = RetrieveRemoteActiveTableRequest(args.serverHost, args.serverPort, 
                                           args.serverProtocol, args.mhsid, 
                                           args.siteID, args.ancf, args.bncf, 
                                           args.xmtScript)
    return req

def main():
    options = process_command_line()
    log.debug("Command-line options: " + repr(options))
    
    req = build_request(options)
    log.debug("Request: " + repr(req))
    
    thriftClient = ThriftClient.ThriftClient(host=options.serverHost)
    try:
        response = thriftClient.sendRequest(req)
    except:
        log.exception("Error posting request.")
        sys.exit(1)
        
    if not response.getTaskSuccess():
        log.error("Error executing requestAT: " + response.getErrorMessage())
        sys.exit(1)


if __name__ == '__main__':
    main()
