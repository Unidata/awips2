#!/awips2/python/bin/python3

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

##
# CLI utility to validate GFE localConfig.py/siteConfig.py.
#
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Sep 12, 2016  5888     dgilling  Initial Creation.
# Dec 10, 2020  8239     randerso  Fix reference to ThriftRequestException
#
##



import argparse
import os
import sys
import traceback

from dynamicserialize.dstypes.com.raytheon.uf.common.site.requests import ValidateConfigRequest
from ufpy import ThriftClient
from ufpy import UsageArgumentParser


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
    except ThriftClient.ThriftRequestException:
        print("Failed to process {} for site {}".format(type(request).__name__, request.getSiteID()), file=sys.stderr)
        traceback.print_exc()
        return -1



if __name__ == '__main__':
    main()
