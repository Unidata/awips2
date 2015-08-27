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
# Provides a command-line utility to a site's grids to a folder and later
# sent to NDFD.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    ??/??/??                      ????????       Initial Creation.
#    04/05/13         1761         dgilling       Rewrite based on 
#                                                 ExportGridsRequest. 
#    08/27/2015       4812         randerso       Changed to expect JobProgress object to be
#                                                 returned from the ExportGridsRequest
# 
#


import argparse
import sys

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import ExportGridsRequest

from ufpy import ThriftClient
from ufpy import UsageArgumentParser


class SendGridsToNDFD:    
    def send(self, site, host, port):       
        thriftClient = ThriftClient.ThriftClient(host, port)
        
        ndfdRequest = ExportGridsRequest()
        ndfdRequest.setSite(site)
        ndfdRequest.setMode("GRIB2")
        
        status = str(thriftClient.sendRequest(ndfdRequest))
        if (status == "SUCCESS"):
            return 0
        else:
            print "sendGridsToNDFD returned status:", status,"\n   See edex-request logs for details."
            return 1
        

def processArgs():
    parser = UsageArgumentParser.UsageArgumentParser(
                prog="sendGridsToNDFD.sh", conflict_handler="resolve")
    parser.add_argument("site", action="store", nargs=1, 
                      help="site whose grids will be sent to NDFD")
    parser.add_argument("host", action="store", nargs=1, 
                      help="upon which the ifpServer is running")
    parser.add_argument("port", action="store", type=int, nargs=1,  
                      help="the port that ifpServer is serving")
    parsed = parser.parse_args()
    options = argparse.Namespace(
                    site = parsed.site[0],
                    host = parsed.host[0],
                    port = parsed.port[0])

    
    return options

def main():
    options = processArgs()    
    sender = SendGridsToNDFD()
    return sender.send(options.site, options.host, options.port)

if __name__ == "__main__":
    main()