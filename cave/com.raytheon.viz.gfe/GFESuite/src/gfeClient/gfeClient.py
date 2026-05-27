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
# -----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- ---------------------------------------------
# Jan 24, 2017  6092     randerso  Initial Creation
#
##

import os
import sys
import time
import argparse

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request.GfeClientRequest import GfeClientRequest
from dynamicserialize.dstypes.java.util import Date
from ufpy import ThriftClient
from ufpy import UsageArgumentParser
from ufpy.UsageArgumentParser import StoreTimeAction
from ufpy.UsageArgumentParser import TIME_FORMAT

def validateArgs(args=None):

    parser = UsageArgumentParser.UsageArgumentParser(conflict_handler="resolve",
                                                     prog='gfeClient', add_help=False)
    parser.add_argument("script", action="store",
                      help=argparse.SUPPRESS,
                      metavar="script")
    parser.add_argument("-h", "--host", action="store", dest="host",
                      help="host name of edex request server",
                      default=str(os.getenv("DEFAULT_HOST", "localhost")),
                      metavar="hostname")
    parser.add_argument("-p", "--port", action="store", type=int, dest="port",
                      help="port number of edex request server",
                      default=int(os.getenv("DEFAULT_PORT", "9581")),
                      metavar="port")

    ############################################################################
    # -site is used for backward compatibility, --site is preferred
    # long names with single dash are non-standard in Unix/Linux
    ############################################################################
    parser.add_argument("--site", "-site", action="store", dest="site", required=True,
                      help="site ID",
                      metavar="site")
    parser.add_argument("-c", "--config", action="store", dest="configFile", required=False,
                      default="gfeConfig",
                      help="GFE config file -- default gfeConfig",
                      metavar="configFile")
    parser.add_argument("-u", action="store", dest="userName", required=False,
                        help="user name -- default SITE",
                        default="SITE",
                        metavar="userName")
    parser.add_argument("-z", "--drt", action=StoreTimeAction, dest="drt", required=False,
                      help="displaced real time -- format YYYYMMDD_hhmm",
                      metavar="drt")

    ############################################################################
    # adding this arguments so -s is not recognized as -site in other scripts
    # -s is not used by this script
    ############################################################################
    parser.add_argument("-s", action="store", dest="startTime", required=False,
                        help=argparse.SUPPRESS)

    args, scriptArgs = parser.parse_known_args(args)
    return parser, args, scriptArgs

def main(args):

    # if no args other than script add --help so usage is displayed
    if len(args) < 2:
        args.extend(["--help"])

    # if --help in args add dummy --site arg so we can display
    # full script usage, not just the gfeClient.py usage
    if "--help" in args:
        args.extend(["--site", "XXX"])

    parser, gfeClientArgs, scriptArgs = validateArgs(args)

    # add config and user option to scriptArgs
    scriptArgs.extend(["-c", gfeClientArgs.configFile, "-u", gfeClientArgs.userName])

    # add drt option if specified
    if gfeClientArgs.drt:
        timeString = time.strftime(TIME_FORMAT, gfeClientArgs.drt)
        scriptArgs.extend(["-z", timeString])

    # add startTime option if specified
    if gfeClientArgs.startTime:
        scriptArgs.extend(["-s", gfeClientArgs.startTime])

    # shutdown isn't a real script and has no gfeClientArgs to validate
    if gfeClientArgs.script.lower() != "shutdown":

        # call the validateArgs() method in the target script
        scriptGlobals = {}
        scriptLocals = {}
        exec(compile(open(gfeClientArgs.script, "rb").read(), gfeClientArgs.script, 'exec'), scriptGlobals, scriptLocals)
        scriptLocals["validateArgs"](args, [parser])

    elif "--help" in args:
        # Don't do shutdown if --help specified
        # this is only for ifpIMAGE since it's calling shutdown until
        # PngWriter can be fixed to run more than once in a session
        sys.exit(0)

    request = GfeClientRequest(gfeClientArgs.script, gfeClientArgs.site,
                               gfeClientArgs.configFile, gfeClientArgs.userName,
                               scriptArgs)
    if gfeClientArgs.drt:
        import calendar

        timeInMillis = calendar.timegm(gfeClientArgs.drt) * 1000
        request.setTime(Date(timeInMillis))

    thriftClient = ThriftClient.ThriftClient(gfeClientArgs.host, gfeClientArgs.port, "/services")
    thriftClient.sendRequest(request)

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
