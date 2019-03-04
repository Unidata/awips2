#!/usr/bin/env python

##
##

##
# CLI utility to validate GFE localConfig.py/siteConfig.py.
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/12/16        5888          dgilling       Initial Creation.
#
##

from __future__ import print_function

import argparse
import os
import sys
import traceback

from dynamicserialize.dstypes.com.raytheon.uf.common.site.requests import ValidateConfigRequest
from awips import ThriftClient
from awips import UsageArgumentParser


def validate_args():
    parser = UsageArgumentParser.UsageArgumentParser(conflict_handler="resolve", prog='validateConfig')
    parser.add_argument("-h", action="store", dest="host",
                      help="host name of edex request server", 
                      default=str(os.getenv("DEFAULT_HOST", "localhost")),
                      metavar="hostname")
    parser.add_argument("-r", action="store", type=int, dest="port", 
                      help="port number of edex request server",
                      default=int(os.getenv("DEFAULT_PORT", "9581")),
                      metavar="port")
    parser.add_argument("-s", action="store", dest="site", required=True,
                      help="site to activate",
                      metavar="site")
    args = parser.parse_args()
    
    return args

def main():
    args = validate_args()
    
    request = ValidateConfigRequest(args.site, "gfe")
    thriftClient = ThriftClient.ThriftClient(args.host, args.port, "/services")
    
    print ("Validating configuration for site {}.".format(request.getSiteID()))
    
    try:
        response = thriftClient.sendRequest(request)
        print("Response received from server:", str(response))
    except ThriftRequestException:
        print("Failed to process {} for site {}".format(type(request).__name__, request.getSiteID()), file=sys.stderr)
        traceback.print_exc()
        return -1



if __name__ == '__main__':
    main()
