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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# runIFPText.py
# Main program and class for running text formatters from the command-line.
# Based on AWIPS-1 TextFormatter.py code written by hansen.
#
# Author: dgilling
# ----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- ---------------------------------------------
# Feb 07, 2017  6092     randerso  Refactored to support calling validateArgs()
#                                  from gfeClient.py
# Jan 07, 2019  21019    ryu       Fix issue of gfeclient hanging on exit.
# Feb 04, 2019  20718    smoorthy  Added the suggested change from Redmine to properly process -drt arguments
import logging
import sys

LOGGER = None

def runFormatter(args):
    ############################################################################
    # Import of FormatterRunner and loadConfig are nested in this function
    # because can only be run under Jep. This allows validateArgs to be called
    # from a pure Python environment
    ############################################################################

    import FormatterRunner
    import loadConfig
    import time

    from com.raytheon.viz.gfe.core import DataManagerUIFactory
    from com.raytheon.viz.gfe.core import DataManager
    from ufpy.UsageArgumentParser import TIME_FORMAT
    from com.raytheon.viz.gfe.core import DataManagerFactory

    LOGGER.info("TextFormatter Starting")
    LOGGER.info("CmdLine: " + str(sys.argv[1:]))

    # set default gfe config so no popup windows appear
    loadConfig.loadPreferences(args.configFile)

    dataMgr = DataManagerUIFactory.getInstance(None)

    forecasts = FormatterRunner.runFormatter(str(args.databaseID),
                                             dataMgr.getSiteID(),
                                             args.forecastList, args.varDict,
                                             args.vtecMode, args.userName, dataMgr,
                                             args.serverFile, args.editAreas,
                                             args.timeRanges, args.timePeriod,
                                             args.drt,
                                             args.vtecActiveTable,
                                             args.testMode,
                                             args.experimentalMode,
                                             args.serverOutputFile,
                                             args.startTime, args.endTime,
                                             args.language,
                                             args.outputFile, args.appendFile)

    LOGGER.info("Text Formatter Finished")

def validateArgs(args=None, parents=[]):
    ############################################################################
    # imports required for this method must be here so it can be invoked
    # from gfeClient.py
    ############################################################################
    from ufpy import UsageArgumentParser
    from ufpy.UsageArgumentParser import StoreDatabaseIDAction
    from ufpy.UsageArgumentParser import StoreTimeAction
    from ufpy.UsageArgumentParser import TIME_FORMAT
    import time

    parser = UsageArgumentParser.UsageArgumentParser(conflict_handler="resolve",
                                                     parents=parents,
                                                     prog='runIFPText')
    parser.add_argument("-d", action=StoreDatabaseIDAction, dest="databaseID", required=True,
                        help="database to run formatter against",
                        metavar="databaseID")
    parser.add_argument("-t", action="append", dest="forecastList", required=True,
                        help="forecastType",
                        metavar="forecastList")
    parser.add_argument("-c", "--config", action="store", dest="configFile", required=False,
                      default="gfeConfig",
                      help="GFE config file -- default gfeConfig",
                      metavar="configFile")
    parser.add_argument("-u", action="store", dest="userName", required=False,
                        help="user name -- default SITE",
                        default="SITE",
                        metavar="userName")
    parser.add_argument("-o", action="store", dest="outputFile", required=False,
                        help="output file for text -- default None",
                        metavar="outputFile")
    parser.add_argument("-O", action="store", dest="serverFile", required=False,
                        help="server output file for text -- default None",
                        metavar="serverFile")
    parser.add_argument("-S", action="store", dest="serverOutputFile", required=False,
                        help="server controlled output file -- default None",
                        metavar="serverOutputFile")
    parser.add_argument("-A", action="store", dest="appendFile", required=False,
                        help="append text to given file name",
                        metavar="appendFile")
    parser.add_argument("-l", action="store", dest="language", required=False,
                        help="language -- english, french, spanish: default english",
                        choices=['english', 'french', 'spanish'],
                        metavar="language")
    parser.add_argument("-z", "--drt", action=StoreTimeAction, dest="drt", required=False,
                      help="displaced real time -- format YYYYMMDD_hhmm",
                      metavar="drt")
    group = parser.add_mutually_exclusive_group(required=False)
    group.add_argument("-T", action="store_true", dest="testMode", required=False,
                        help="Generates a \"TEST\" product")
    group.add_argument("-E", action="store_true", dest="experimentalMode", required=False,
                        help="Generates a \"EXPERIMENTAL\" product.")
    parser.add_argument("-v", action="store", dest="vtecMode", required=False,
                        choices=['X', 'O', 'T', 'E'],
                        help="Specifies vtec mode ('X','O','T','E')",
                        metavar="vtecMode")
    parser.add_argument("-a", action="store", dest="vtecActiveTable", required=False,
                        choices=['active', 'PRACTICE'],
                        help="Specifies active table -- 'active' or 'PRACTICE'",
                        default='active',
                        metavar="vtecActiveTable")
    parser.add_argument("-V", action="store", dest="varDict", required=False,
                        help="""use this option to provide a run-time VariableList
                                instead of displaying the user dialog.
                                The dictionary must be in the form of a Python
                                dictionary string, e.g.,
                                '{("Forecast Product", "productType"): "Morning",
                                  ("Issuance", "issuanceType"): "Routine"}'
                                The entries must be complete or the product will be cancelled.""",
                        default="{}",
                        metavar="varDict")
    parser.add_argument("-r", action="append", dest="editAreas", required=False,
                        help="Edit Area Name",
                        default=[],
                        metavar="editAreas")
    parser.add_argument("-w", action="append", dest="timeRanges", required=False,
                        help="named time range (e.g. Today, Tonight)",
                        default=[],
                        metavar="timeRanges")
    parser.add_argument("-s", action=StoreTimeAction, dest="startTime", required=False,
                        help="startTime -- format YYYYMMDD_hhmm",
                        metavar="startTime")
    parser.add_argument("-e", action=StoreTimeAction, dest="endTime", required=False,
                        help="endTime -- format YYYYMMDD_hhmm",
                        metavar="endTime")
    parser.add_argument("-i", action="store", dest="timePeriod", required=False,
                        type=float,
                        help="Period for Tables with variable period (rows or columns)",
                        metavar="timePeriod")

    args = parser.parse_args(args)

    #force VTEC mode to "T" if in TEST mode and another vtecCode is specified
    if args.testMode and args.vtecMode is not None:
        args.vtecMode = "T"

    #force VTEC mode to "E" if in EXPERIMENTAL mode and another vtecCode
    #is specified
    elif args.experimentalMode and args.vtecMode is not None:
        args.vtecMode = "E"

    #force into TEST mode, if vtec code is 'T'
    if args.vtecMode == "T":
        args.testMode = True
        args.experimentalMode = False
    elif args.vtecMode == "E":
        args.experimentalMode = True
        args.testMode = False

    if args.drt is not None:
        args.drt = time.strftime(TIME_FORMAT, args.drt)

    return args

def usage():
    validateArgs(['--help'])

def error(msg):
    print("ERROR: %s\n" % msg)

def main2(argv):
    # Parse command line
    args = validateArgs()
    runFormatter(args)

def __initLogger():
    global LOGGER
    logging.basicConfig(level=logging.INFO,
                        format="%(asctime)s %(name)s %(levelname)s:  %(message)s",
                        datefmt="%H:%M:%S")
    LOGGER = logging.getLogger("runIFPText.py")

PROFILE = False
def profMain(arg):
    __initLogger()
    if PROFILE:
        import profile, pstats, sys
        limit = 20
        profile.run('main2(sys.argv)', 'pyprof.out')
        p = pstats.Stats('pyprof.out')
        p.strip_dirs()
        p.sort_stats('time', 'calls').print_stats(limit)
        p.print_callers(limit)
    else:
        try:
            main2(arg)
        except:
            LOGGER.exception("Caught Exception: ")

main = profMain
if __name__ == "__main__":
    profMain(sys.argv)
