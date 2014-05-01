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

import logging
import os
import sys

from ufpy import ThriftClient
from ufpy import UsageArgumentParser
from ufpy.UsageArgumentParser import StoreDatabaseIDAction as StoreDatabaseIDAction
from ufpy.UsageArgumentParser import AppendParmNameAndLevelAction as AppendParmNameAndLevelAction

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import ExecuteIscMosaicRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId


#
# Provides a command-line utility to send ISC grids to GFE.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/20/10                      dgilling       Initial Creation.
#    03/12/13         1759         dgilling       Re-factor to use a more robust
#                                                 request object.
#    
# 
#



logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s", 
                    datefmt="%H:%M:%S", 
                    level=logging.INFO)
log = logging.getLogger('iscMosaic')


def get_args():
    usage = """
%(prog)s -h hostname -r port -d outputDatabaseID [-p parm] -f inputFile
  [-b] [-s startTime] [-e endTime] [-i parm] [-x] [-z] [-n]
  [-a altMask] [-w messagePrefix] [-D gridDelay] [-o] """
  
    parser = UsageArgumentParser.UsageArgumentParser(prog='iscMosaic', 
                usage=usage, conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="host",
                      help="name of the ifpServer host", 
                      metavar="hostname", required=True)
    parser.add_argument("-r", action="store", type=int, dest="port", 
                      help="port number of the ifpServer",
                      metavar="port", required=True)
    parser.add_argument("-u", action="store", dest="userID", 
                      help="The user ID to connect with",
                      metavar="userID", default="SITE")
    parser.add_argument("-d", action=StoreDatabaseIDAction, dest="databaseID", 
                      help="id for output database in ifpServer",
                      metavar="outputDatabaseID")
    parser.add_argument("-p", action=AppendParmNameAndLevelAction, 
                      dest="parmsToProcess", default=[], 
                      help="name of parm to output, can have multiple switches",
                      metavar="parm")
    parser.add_argument("-f", action="append", dest="inFiles", 
                      help="name of incoming netCDF files, can have multiple",
                      metavar="inputFile", default=[])
    parser.add_argument("-b", action="store_true", dest="blankOtherPeriods", 
                      help="blank data that does not overlap incoming grids")
    parser.add_argument("-s", action="store", dest="startTime", 
                      help="only output grids that occur after this YYYYMMDD_HHMM",
                      metavar="startTime")
    parser.add_argument("-e", action="store", dest="endTime", 
                      help="only output grids that occur before this YYYYMMDD_HHMM",
                      metavar="endTime")
    parser.add_argument("-x", action="store_true", dest="replaceOnly", 
                      help="replace grids with input grids, do not perform merge")
    parser.add_argument("-z", action="store_true", dest="eraseFirst", 
                      help="erase all grids in destination parm before storing")
    parser.add_argument("-n", action="store_true", dest="ignoreMask", 
                      help="do not mask the grids upon storage (ignore incoming siteID)")
    parser.add_argument("-a", action="store", dest="altMask", 
                      help="use an alternative mask, rather than the siteID",
                      metavar="altMask")
    parser.add_argument("-w", action="store", dest="announce", 
                      help="alert users of data storage",
                      metavar="message", default="")
    parser.add_argument("-k", action="store_true", dest="deleteInput", 
                      help="Delete the input file after processing.")
    parser.add_argument("-i", action=AppendParmNameAndLevelAction, 
                      dest="parmsToIgnore", default=[], 
                      help="Do not process parm by this name.",
                      metavar="parm")
    parser.add_argument("-D", action="store", type=float, dest="gridDelay", 
                      help="Delay between grids during processing (Default 0.00)",
                      metavar="gridDelay", default=0.0)
    parser.add_argument("-o", action="store_true", dest="renameWE", 
                      help="Use office type information to rename incoming data.")
    parser.add_argument("-S", action="store_true", dest="iscSends", 
                      help="If storing in Fcst database, also force ISC send queueing.")
    parser.add_argument("-T", action="store_true", dest="adjustTranslate", 
                      help="Translate WEATHER and DISCRETE data into compatible values known by destination ifpServer.")
    parser.add_argument("-v", action="store", dest="logFileName", 
                      help="Log file destination.")
    
    options = parser.parse_args()
    
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
    
    if len(options.inFiles) > 1 and options.eraseFirst:
        parser.error("Multiple input files [-f switches] and -z switch not compatible.")
        
    if options.ignoreMask and options.altMask is not None:
        parser.error("-n and -a altMask switches are not compatible")
    
    return options
    

def create_request(host, port, userID, databaseID, parmsToProcess, blankOtherPeriods, 
                  startTime, endTime, altMask, replaceOnly, eraseFirst, 
                  announce, renameWE, iscSends, inFiles, ignoreMask, 
                  adjustTranslate, deleteInput, parmsToIgnore, gridDelay, 
                  logFileName):    
    obj = ExecuteIscMosaicRequest(userID, databaseID, parmsToProcess, 
            blankOtherPeriods, startTime, endTime, altMask, replaceOnly, 
            eraseFirst, announce, renameWE, iscSends, inFiles, ignoreMask, 
            adjustTranslate, deleteInput, parmsToIgnore, gridDelay, logFileName)
    return obj


def main():
    log.info("Starting iscMosaic")
    options = get_args()
    log.debug("Command-line args: " + repr(options))
    
    iscRequest = create_request(**vars(options))
    log.debug("Sending request: " + str(iscRequest))
    
    try:
        thriftClient = ThriftClient.ThriftClient(options.host, options.port, "/services")
        serverResponse = thriftClient.sendRequest(iscRequest)
    except:
        log.exception("Unhandled exception thrown during iscMosaic processing:")
        sys.exit(1)
    
    if not serverResponse:
        log.error("Errors occurred during iscMosaic processing: " + str(serverResponse.message()))
        sys.exit(1)


if __name__ == '__main__':
    main()
    