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
# Provides a command-line utility to export selected grids to netCDF format.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/23/10                      dgilling       Initial Creation.
#    03/07/13         1759         dgilling       Populate siteID field of 
#                                                 request based on specified 
#                                                 DatabaseID. 
#    
# 
#


import logging
import os
import sys

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import ExecuteIfpNetCDFGridRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId
from ufpy import ThriftClient
from ufpy import UsageArgumentParser
from ufpy.UsageArgumentParser import StoreDatabaseIDAction as StoreDatabaseIDAction
from ufpy.UsageArgumentParser import AppendParmNameAndLevelAction as AppendParmNameAndLevelAction


RETRY_ATTEMPTS = 3

logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s", 
                    datefmt="%H:%M:%S", 
                    level=logging.INFO)
log = logging.getLogger('ifpnetCDF')

 

def get_and_check_args():
    parser = UsageArgumentParser.UsageArgumentParser(prog='ifpnetCDF', conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="host",
                        help="Host name upon which the ifpServer is running.", 
                        metavar="hostname")
    parser.add_argument("-r", action="store", type=int, dest="port", 
                        help="Port upon which the ifpServer is running.",
                        metavar="port")
    parser.add_argument("-u", action="store", dest="userID", 
                        help="The user ID to connect with",
                        default="SITE", metavar="userID")
    parser.add_argument("-o", action="store", dest="outputFilename", 
                        help="Specifies the name of the output file.",
                        default="ifpnetCDFFile.cdf", metavar="outputFile")                                   
    parser.add_argument("-d", action=StoreDatabaseIDAction, dest="databaseID",
                        required= True,
                        help="id for output database in ifpServer",
                        metavar="databaseID")
    parser.add_argument("-p", action=AppendParmNameAndLevelAction, dest="parmList", 
                        help="Optional. If none specified, get all parms for the database listed.  There may be several parm names specified.",
                        default=[], metavar="parmName")
    parser.add_argument("-s", action="store", dest="startTime",
                        help="Optional.  If no start time specified, make start time = 0. format: YYYYMMDD_HHMM (e.g. 19980604_1200)",
                        default="19700101_0000", metavar="startTime")
    parser.add_argument("-e", action="store", dest="endTime", 
                        help="Optional.  If no end time specified, make end time = Abstime::MaxFutureTime(). format: (19980604_1200)",
                        default="20371231_2359", metavar="endTime")                      
    parser.add_argument("-m", action="store", dest="mask", 
                        help="Optional. Specifies the edit area to be used as the clip mask. If no mask was specified then the entire domain will be used. All values outside the mask will be assigned a missing value.",
                        metavar="mask", default="")
    parser.add_argument("-t", action="store_true", dest="trim", 
                        help="Optional. If present, trim data resolution.")
    parser.add_argument("-g", action="store_true", dest="geoInfo", 
                        help="Optional. If present, topography, latitude and longitude grids will be stored.")
    parser.add_argument("-c", action="store_true", dest="compressFile", 
                        help="Optional. If present, the netCDF file will be compressed by the gzip.")                                            
    parser.add_argument("-f", action="store", type=int, dest="compressFileFactor", 
                        help="Optional. When provided in conjunction with the -c switch, provides the compression factor of gzip (1-9). Default is 6.",
                        metavar="factor", default=6)
    parser.add_argument("-k", action="store_true", dest="krunch", 
                        help="Optional. If present, the netCDF file is really shrunk, by using bytes and shorts to represent floats. Requires the -t switch.")
    parser.add_argument("-C", action="store", dest="configFileName", 
                        help="Optional. If present, controls the interval/spacing of the grids. Identifies a configuration file defining the timing constraints. The filename identifies a file within the ifpServer TEXT/Utility directory and must be a Python file.",
                        metavar="configIntervalFilename")
    parser.add_argument("-v", action="store", dest="logFileName", 
                        help="Optional. If present, the output from the script will be logged to the specified file.  If not present, logging will default to ifpnetCDF.log located in GFESUITE_LOGDIR")
    
    options = parser.parse_args()
    
    if options.host == None:
        if "CDSHOST" in os.environ:
            options.host = os.environ["CDSHOST"]
        else:
            parser.error("You must specify an EDEX server host name.")
        
    if options.port == None:
        if "CDSPORT" in os.environ:
            options.port = int(os.environ["CDSPORT"])
        else:
            parser.error("You must specify an EDEX server port number.")
            
    # must have trim, if krunch specified
    if options.krunch and not options.trim:
        setattr(options, "krunch", False)
    
    return options   

def create_request(host, port, outputFilename, parmList, databaseID, startTime,
                   endTime, mask, geoInfo, compressFile, configFileName, 
                   compressFileFactor, trim, krunch, userID, logFileName):   
    obj = ExecuteIfpNetCDFGridRequest(outputFilename, parmList, databaseID, 
            startTime, endTime, mask, geoInfo, compressFile, configFileName, 
            compressFileFactor, trim, krunch, userID, logFileName)
    return obj


def main():
    log.info("Starting ifpnetCDF")
    options = get_and_check_args()
    log.debug("Command-line args: " + repr(options))
    
    netCdfRequest = create_request(**vars(options))
    log.debug("Sending request: " + str(netCdfRequest))
    
    for i in range(1, RETRY_ATTEMPTS+1):
        log.info("Attempt number: %d", i)
        
        try:
            thriftClient = ThriftClient.ThriftClient(options.host, options.port, "/services")
            serverResponse = thriftClient.sendRequest(netCdfRequest)
        except:
            log.exception("Unhandled exception thrown during ifpnetCDF processing:")
            sys.exit(1)
    
        if serverResponse:
            break
        else:
            log.error("Errors occurred during ifpnetCDF processing: " + serverResponse.message())
            if (i == RETRY_ATTEMPTS):
                log.error("Final attempt failed - exiting")
                sys.exit(1)


if __name__ == '__main__':
    main()
    
