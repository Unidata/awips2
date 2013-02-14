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

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import CreateNetCDFGridRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId
from dynamicserialize import DynamicSerializationManager
import sys
import os
from ufpy import ThriftClient
from ufpy.UsageOptionParser import UsageOptionParser

#
# Provides a command-line utility to export selected grids to netCDF format.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/23/10                      dgilling       Initial Creation.
#    
# 
#


def main():
    (options, args) = validateArgs()
    for i in range(1,4):
        print >> sys.stderr, "Attempt number: ", i
        
        try:
            netCdfRequest = createRequest()
            netCdfRequest.setArgString(netCdfRequest.getArgString() + " -h " + options.host + " -r " + str(options.port))
            thriftClient = ThriftClient.ThriftClient(options.host, options.port, "/services")
            serverResponse = thriftClient.sendRequest(netCdfRequest)
        except Exception, e:
            print >> sys.stderr, "Unhandled exception thrown during ifpnetCDF processing: \n", str(e)
            sys.exit(1)
    
        if (serverResponse.isOkay()):
            break
        else:
            print >> sys.stderr, "Errors occurred during ifpnetCDF processing: ", serverResponse.message()
 
            if (i == 3):
                print >> sys.stderr, "Final attempt failed - exiting"
                sys.exit(1)
 

def validateArgs():
    parser = UsageOptionParser(conflict_handler="resolve")
    parser.add_option("-h", action="store", type="string", dest="host",
                      help="Host name upon which the ifpServer is running.", 
                      metavar="hostname")
    parser.add_option("-r", action="store", type="int", dest="port", 
                      help="Port upon which the ifpServer is running.",
                      metavar="port")
    parser.add_option("-u", action="store", type="string", dest="userID", 
                      help="The user ID to connect with",
                      metavar="userID")
    parser.add_option("-o", action="store", type="string", dest="outputFile", 
                      help="Specifies the name of the output file.",
                      metavar="outputFile")                                   
    parser.add_option("-d", action="store", type="string", dest="databaseID", 
                      help="id for output database in ifpServer",
                      metavar="databaseID")
    parser.add_option("-p", action="append", type="string", dest="parmList", 
                      help="Optional. If none specified, get all parms for the database listed.  There may be several parm names specified.",
                      metavar="parmName")
    parser.add_option("-s", action="store", type="string", dest="startTime", 
                      help="Optional.  If no start time specified, make start time = 0. format: YYYYMMDD_HHMM (e.g. 19980604_1200)",
                      metavar="startTime")
    parser.add_option("-e", action="store", type="string", dest="endTime", 
                      help="Optional.  If no end time specified, make end time = Abstime::MaxFutureTime(). format: (19980604_1200)",
                      metavar="endTime")                      
    parser.add_option("-m", action="store", type="string", dest="mask", 
                      help="Optional. Specifies the edit area to be used as the clip mask. If no mask was specified then the entire domain will be used. All values outside the mask will be assigned a missing value.",
                      metavar="mask")
    parser.add_option("-t", action="store_true", dest="trim", 
                      help="Optional. If present, trim data resolution.")
    parser.add_option("-g", action="store_true", dest="geoInfo", 
                      help="Optional. If present, topography, latitude and longitude grids will be stored.")
    parser.add_option("-c", action="store_true", dest="compressFile", 
                      help="Optional. If present, the netCDF file will be compressed by the gzip.")                                            
    parser.add_option("-f", action="store", type="int", dest="compressFileFactor", 
                      help="Optional. When provided in conjunction with the -c switch, provides the compression factor of gzip (1-9). Default is 6.",
                      metavar="factor")
    parser.add_option("-k", action="store_true", dest="krunch", 
                      help="Optional. If present, the netCDF file is really shrunk, by using bytes and shorts to represent floats. Requires the -t switch.")
    parser.add_option("-C", action="store", type="string", dest="configFileName", 
                      help="Optional. If present, controls the interval/spacing of the grids. Identifies a configuration file defining the timing constraints. The filename identifies a file within the ifpServer TEXT/Utility directory and must be a Python file.",
                      metavar="configIntervalFilename")
    parser.add_option("-v", action="store_true", dest="logFile", 
                      help="Optional. If present, the output from the script will be logged to the specified file.  If not present, logging will default to ifpnetCDF.log located in GFESUITE_LOGDIR")
    
    (options, args) = parser.parse_args()
    
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
            
    if (options.databaseID is None):
        parser.error("You must specify a database id.")
    
    return (options, args)
    

def createRequest():    
    obj = CreateNetCDFGridRequest()
    
    wsId = WsId(progName = "ifpnetCDF")
    
    obj.setWorkstationID(wsId)
    obj.setSiteID("")
    obj.setArgString(" ".join(sys.argv[1:]))
    return obj

if __name__ == '__main__':
    main()
    
