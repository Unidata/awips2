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

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import ExecuteIscMosaicRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId
from dynamicserialize import DynamicSerializationManager
import sys
import os
from ufpy.UsageOptionParser import UsageOptionParser
from ufpy import ThriftClient

#
# Provides a command-line utility to send ISC grids to GFE.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/20/10                      dgilling       Initial Creation.
#    
# 
#

def main():
    (options, args) = validateArgs()
    
    try:
        iscRequest = createRequest()
        thriftClient = ThriftClient.ThriftClient(options.host, options.port, "/services")
        serverResponse = thriftClient.sendRequest(iscRequest)
    except Exception, e:
        print "Unhandled exception thrown during iscMosaic processing: \n", str(e)
        sys.exit(1)
    
    if (not serverResponse.isOkay()):
        print "Errors occurred during iscMosaic processing: ", serverResponse.message()
        sys.exit(1)

def validateArgs():
    usage = """
%prog -h hostname -r port -d outputDatabaseID [-p parm] -f inputFile
  [-b] [-s startTime] [-e endTime] [-i parm] [-x] [-z] [-n]
  [-a altMask] [-w messagePrefix] [-D gridDelay] [-o] """
  
    parser = UsageOptionParser(usage=usage, conflict_handler="resolve")
    parser.add_option("-h", action="store", type="string", dest="host",
                      help="name of the ifpServer host", 
                      metavar="hostname")
    parser.add_option("-r", action="store", type="int", dest="port", 
                      help="port number of the ifpServer",
                      metavar="port")
    parser.add_option("-u", action="store", type="string", dest="user", 
                      help="The user ID to connect with",
                      metavar="userID")
    parser.add_option("-d", action="store", type="string", dest="outputDatabaseID", 
                      help="id for output database in ifpServer",
                      metavar="outputDatabaseID")
    parser.add_option("-p", action="append", type="string", dest="processParm", 
                      help="name of parm to output, can have multiple switches",
                      metavar="parm")
    parser.add_option("-f", action="append", type="string", dest="inputFiles", 
                      help="name of incoming netCDF files, can have multiple",
                      metavar="inputFile")
    parser.add_option("-b", action="store_true", dest="blankOtherPeriods", 
                      help="blank data that does not overlap incoming grids")
    parser.add_option("-s", action="store", type="string", dest="startTime", 
                      help="only output grids that occur after this YYYYMMDD_HHMM",
                      metavar="startTime")
    parser.add_option("-e", action="store", type="string", dest="endTime", 
                      help="only output grids that occur before this YYYYMMDD_HHMM",
                      metavar="endTime")
    parser.add_option("-x", action="store_true", dest="replaceOnly", 
                      help="replace grids with input grids, do not perform merge")
    parser.add_option("-z", action="store_true", dest="eraseFirst", 
                      help="erase all grids in destination parm before storing")
    parser.add_option("-n", action="store_true", dest="ignoreMask", 
                      help="do not mask the grids upon storage (ignore incoming siteID)")
    parser.add_option("-a", action="store", type="string", dest="altMask", 
                      help="use an alternative mask, rather than the siteID",
                      metavar="altMask")
    parser.add_option("-w", action="store", type="string", dest="message", 
                      help="alert users of data storage",
                      metavar="message")
    parser.add_option("-k", action="store_true", dest="deleteInput", 
                      help="Delete the input file after processing.")
    parser.add_option("-i", action="append", type="string", dest="ignoreParm", 
                      help="Do not process parm by this name.",
                      metavar="parm")
    parser.add_option("-D", action="store", type="float", dest="gridDelay", 
                      help="Delay between grids during processing (Default 0.00)",
                      metavar="gridDelay")
    parser.add_option("-o", action="store_true", dest="renameWE", 
                      help="Use office type information to rename incoming data.")
    parser.add_option("-S", action="store_true", dest="iscSends", 
                      help="If storing in Fcst database, also force ISC send queueing.")
    parser.add_option("-T", action="store_true", dest="adjustTranslate", 
                      help="Translate WEATHER and DISCRETE data into compatible values known by destination ifpServer.")
    parser.add_option("-v", action="store_true", dest="logFile", 
                      help="Log file destination.")
    
    (options, args) = parser.parse_args()
    
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
            
    if (options.inputFiles is None):
        parser.error("-f: At least one input file must be provided.")
    
    if (len(options.inputFiles) > 1 and options.eraseFirst):
        parser.error("Multiple input files [-f switches] and -z switch not compatible.")
        
    if (options.ignoreMask and options.altMask is not None):
        parser.error("-n and -a altMask switches are not compatible")
    
    return (options, args)
    

def createRequest():    
    obj = ExecuteIscMosaicRequest()
    
    wsId = WsId(progName="iscMosaic")
    
    obj.setWorkstationID(wsId)
    obj.setSiteID("")
    obj.setArgString(" ".join(sys.argv[1:]))
    return obj

if __name__ == '__main__':
    main()
    