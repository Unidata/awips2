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

import argparse
import logging
import os
import pwd
import sys
import time

import numpy

from dynamicserialize.dstypes.com.raytheon.uf.common.auth.resp import SuccessfulExecution
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GetASCIIGridsRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import SaveASCIIGridsRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.localization import LocalizationContext
from dynamicserialize.dstypes.com.raytheon.uf.common.localization import LocalizationLevel
from dynamicserialize.dstypes.com.raytheon.uf.common.localization import LocalizationType
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.msgs import DeleteUtilityCommand
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.msgs import PrivilegedUtilityRequestMessage
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.stream import LocalizationStreamGetRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.stream import LocalizationStreamPutRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId
from ufpy import ThriftClient
from ufpy import UsageArgumentParser


##
# The asciiFormatter formats input and output files in ASCII format.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/12/11        8983          dgilling       Initial Creation.
# 
#
##


## Constants ##
BUFFER_SIZE = 512 * 1024


## Custom actions for ArgumentParser object ##
class AppendDatabaseIDAction(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        did = DatabaseID(values)
        if did.isValid():
            if (hasattr(namespace, self.dest)) and \
               (getattr(namespace, self.dest) is not None):
                currentValues = getattr(namespace, self.dest)
                currentValues.append(did)
                setattr(namespace, self.dest, currentValues)
            else:
                setattr(namespace, self.dest, [did])
        else:
            parser.error("DatabaseID [" + values + "] not valid identifier")
            
class StoreTimeAction(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        (isValid, dateTime) = DatabaseID.decodeDtg(values)
        if isValid:
            setattr(namespace, self.dest, dateTime)
        else:
            parser.error(str(self.dest) + " not in yyyymmdd_hhmm format")


## Logging methods ##
def __initLogger():
    logger = logging.getLogger("ifpAG.py")
    logger.setLevel(logging.INFO)
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    formatter = logging.Formatter("%(asctime)s %(name)s %(levelname)s:  %(message)s", "%H:%M:%S")
    ch.setFormatter(formatter)
    logger.addHandler(ch)
    
def logEvent(msg):
    logging.getLogger("ifpAG.py").info(msg)

def logProblem(msg):
    logging.getLogger("ifpAG.py").error(msg)
    
def logException(msg):
    logging.getLogger("ifpAG.py").exception(msg)    

def logVerbose(msg):
    logging.getLogger("ifpAG.py").debug(msg)


def main():
    options = decodeArguments()
    __initLogger()
    
    logEvent("ifpAG Starting")
    
    if hasattr(options, "outputFile"):
        logVerbose("outputFile=" + str(options.outputFile))
    if hasattr(options, "inputFile"):
        logVerbose("inputFile=" + str(options.inputFile))
    logVerbose("hostname=" + str(options.hostname))
    logVerbose("portNumber=" + str(options.portNumber))
    logVerbose("databaseIds=" + str(options.databaseIds))
    logVerbose("parmIds=" + str(options.parmIds))
    logVerbose("startTime=" + str(options.startTime))
    logVerbose("endTime=" + str(options.endTime))
    logVerbose("userName=" + str(options.userName))
    logVerbose("coordConversionString=" + str(options.coordConversionString))
    
    # access the database
    db = ThriftClient.ThriftClient(options.hostname, options.portNumber)
    
    statusCode = 0
    if hasattr(options, "outputFile") and options.outputFile not in ["", None]:
        statusCode = outputAG(db, options.databaseIds, options.parmIds, options.startTime,\
                 options.endTime, options.outputFile, options.coordConversionString, options.userName)
    elif hasattr(options, "inputFile") and options.inputFile not in ["", None]:
        statusCode = inputAG(db, options.inputFile, options.userName)
        
    logEvent("ifpAG Finished")
    sys.exit(statusCode)

def inputAG(db, inputFile, userName):
    logEvent("Reading in ASCII Grid File")
    
    (aGrids, isSaved) = sendGridTempData(db, inputFile, userName)
    
    if not isSaved:
        logProblem("Problems sending ASCII grid data to EDEX server.")
        return 2
        
    if not saveGridSlices(db, aGrids):
        logProblem("Problems saving some grids")
        return 3
    
    return 0

def outputAG(db, databaseIds, parmIds, startTime, endTime, outputFile, coordConversionString, userName):    
    # get the grid slices
    (serverFile, okay) = getGridSlices(db, databaseIds, parmIds, startTime, endTime, coordConversionString)
    if not okay:
        return 3
    
    # output the grid slices to the output file
    fh = None
    if outputFile != "-":
        try:
            fh = open(outputFile, 'w')
        except:
            logException("Unable to open the file " + outputFile + " for output")
            return 2
    else:
        fh = sys.stdout
        
    logEvent("Outputting ASCII Grid File")
    
    request = LocalizationStreamGetRequest()
    request.setOffset(0)
    request.setNumBytes(BUFFER_SIZE)
    ctx = LocalizationContext()
    ll = LocalizationLevel("USER")
    type = LocalizationType("COMMON_STATIC")
    ctx.setLocalizationType(type)
    ctx.setLocalizationLevel(ll)
    ctx.setContextName(userName)
    request.setContext(ctx)
    request.setMyContextName(ctx.getContextName())
    request.setFileName(serverFile)
    
    finished = False
    while not finished:
        try:
            serverResponse = db.sendRequest(request)
            if not isinstance(serverResponse, SuccessfulExecution):
                message = ""
                if hasattr(serverResponse, "getMessage"):
                    message = serverResponse.getMessage()
                raise RuntimeError(message)
            serverResponse = serverResponse.getResponse()
        except:
            logException("Could not retrieve ASCIIGrids file " + serverFile + " from localization server")
            okay = False
            break
        
        # serverResponse will be returned as a LocalizationStreamPutRequest
        # object. we'll use its methods to read back the serialized file 
        # data.
        # bytes get returned to us as an numpy.ndarray
        bytes = serverResponse.getBytes()
        fh.write(bytes.tostring())
        request.setOffset(request.getOffset() + len(bytes))
        finished = serverResponse.getEnd()
        
    if fh is not None:
        fh.close()
        
    # delete server-side file
    req = PrivilegedUtilityRequestMessage()
    cmds = []
    cmd = DeleteUtilityCommand()
    cmd.setContext(ctx)
    cmd.setFilename(serverFile)
    cmd.setMyContextName(ctx.getContextName())
    cmds.append(cmd)
    req.setCommands(cmds)
    try:
        serverResponse = db.sendRequest(req)
        if not isinstance(serverResponse, SuccessfulExecution):
            message = ""
            if hasattr(serverResponse, "getMessage"):
                message = serverResponse.getMessage()
            raise RuntimeError(message)
    except:
        logException("Could not delete temporary file " + serverFile + " from localization server")
        okay = False
            
    if okay:
        return 0
    else:
        return 3
    
def decodeArguments():
    parser = UsageArgumentParser.UsageArgumentParser(prog='ifpAG', conflict_handler="resolve", usage=
"""ifpAG -o outputFile -h hostname -r rpcPortNumber -d databaseID [-p parmID] [-s startTime] [-e endTime] [-u username] [-c coordConversionString]
or
ifpAG -i inputFile -h hostname -r rpcPortNumber [-u username]
""")
    modeGroup = parser.add_mutually_exclusive_group(required=True)
    modeGroup.add_argument("-o", action="store", dest="outputFile", 
                        help="Specifies the name of the output file.",
                        metavar="outputFile")
    modeGroup.add_argument("-i", action="store", dest="inputFile", 
                      help="Specifies the name of the input file.",
                      metavar="inputFile")
    parser.add_argument("-h", action="store", dest="hostname",
                      help="""Host name upon which the EDEX server is running. 
                              Please specify the host name of the dx3 or dx4 server.""", 
                      required=True, metavar="hostname")
    parser.add_argument("-r", action="store", type=int, dest="portNumber", 
                      help="the port that ifpServer is serving",
                      required=True, metavar="portNumber")
    parser.add_argument("-u", action="store", dest="userName", 
                      help="If no username is specified, then your username is used.",
                      metavar="username")    
    parser.add_argument("-d", action=AppendDatabaseIDAction, dest="databaseIds", 
                      help="""DatabaseID from which to get the data.  There may
                              be several DatabaseIDs specified.
                             format: (DEN_GRID__eta_19980604_1200)""", 
                      metavar="databaseID")
    parser.add_argument("-p", action="append", dest="parmNames", 
                      help="""If none specified, get all parms for
                               all databases listed.  There may be several
                               parmIDs specified.
                               format: (Temp)""",
                      metavar="parmID")
    parser.add_argument("-s", action=StoreTimeAction, dest="startTime", 
                      help="""If no start time specified, make start
                              time = 0. format: (19980604_1200)""",
                      metavar="startTime")
    parser.add_argument("-e", action=StoreTimeAction, dest="endTime", 
                      help="""If no end time specified, make end
                              time = Abstime::MaxFutureTime().
                              format: (19980604_1200)""",
                      metavar="endTime")
    parser.add_argument("-c", action="store", dest="coordConversionString", 
                      help="""If no coordinate conversion string
                               is specified, then output is done in the
                               server's inherinet resolution. Must be quoted.
                             String format:
                             xsize ysize projID originX originY extentX extentY
                             "35 35 Grid211 35.0 14.0 9.0 9.0"
                             Coord conversion is automatic on input.""",
                      metavar="coordinateConversionString")   
    options = parser.parse_args()
    options.parmIds = []
    
    # check that we have the required arguments for either output or input
    if options.outputFile is not None:
        # for output: hostname, RPC port number, database id are required
        if options.databaseIds is None:
            parser.error("The database id argument is missing.")
        
        # if no start time specified, set it to 0
        if options.startTime is None:
            options.startTime = TimeRange.allTimes().getStart()
            
        if options.endTime is None:
            options.endTime = TimeRange.allTimes().getEnd()
            
        # if parms are specified, append them the parmIds
        if options.parmNames is not None:
            for parm in options.parmNames:
                for dbID in options.databaseIds:
                    pos = parm.find("_")
                    if pos > -1:
                        options.parmIds.append(ParmID(parm[:pos], dbID, parm[pos+1:]))
                    else:
                        options.parmIds.append(ParmID(parm, dbID))
    
    # if no username, assign SITE
    if options.userName is None:
        options.userName = pwd.getpwuid(os.getuid()).pw_name
        
    return options

def getGridSlices(db, databaseIds, parmIds, startTime, endTime, coordConversionString):
    logEvent("Getting Grids from ifpServer")
    request = GetASCIIGridsRequest()
    request.setDatabaseIds(databaseIds)
    request.setParmIds(parmIds)
    tr = TimeRange()
    tr.setStart(startTime)
    tr.setEnd(endTime)
    request.setTimeRange(tr)
    request.setCoordConversionString(coordConversionString)
    request.setWorkstationID(__WsId())
    request.setSiteID(databaseIds[0].getSiteId())
    
    try:
        serverResponse = db.sendRequest(request)
    except:
        logException("Unhandled exception thrown:")
        return ("", False)
        
    if (not serverResponse.isOkay()):
        logProblem("Problem getting grids from db: " +  str(serverResponse.message()))
        return ("", False)
        
    return (serverResponse.getPayload(), True)

def sendGridTempData(db, inputFile, userName):
    fh = None
    if inputFile != "-":
        try:
            fh = open(inputFile, 'r')
        except:
            logException("Unable to open the file " + inputFile + " for input")
            return ("", False)
    else:
        fh = sys.stdin
        
    # Store the main file
    # need to convert python bytearray type (which is just a list of ints)
    # to numpy.int8 type to ensure this data is serialized as bytes
    
    request = LocalizationStreamPutRequest()
    ctx = LocalizationContext()
    ll = LocalizationLevel("USER")
    type = LocalizationType("COMMON_STATIC")
    ctx.setLocalizationType(type)
    ctx.setLocalizationLevel(ll)
    ctx.setContextName(userName)
    request.setContext(ctx)
    request.setMyContextName(userName)
    request.setFileName("/gfe/ifpAG/ifpAG" + str(hash(__WsId())) + ".txt")
    
    totalSent = 0
    finished = False
    okay = True
    aGrids = request.getFileName()
    
    while not finished:
        bytes = numpy.asarray(bytearray(fh.read(BUFFER_SIZE), 'ascii'), dtype=numpy.int8)
        sendSize = len(bytes)
        request.setOffset(totalSent)
        totalSent += sendSize
        request.setBytes(bytes)
        request.setEnd(sendSize==0)
        finished = request.getEnd()
        try:
            serverResponse = db.sendRequest(request)
            if not isinstance(serverResponse, SuccessfulExecution):
                message = ""
                if hasattr(serverResponse, "getMessage"):
                    message = serverResponse.getMessage()
                raise RuntimeError(message)
            serverResponse = serverResponse.getResponse()
        except:
            logException("Unable to save temp ASCII grids file " + aGrids + " to EDEX server.")
            okay = False
            break
        
    if fh is not None:
        fh.close()
        
    return (aGrids, okay)

def saveGridSlices(db, agrid):
    logEvent("Saving Grids to ifpServer")
    
    request = SaveASCIIGridsRequest()
    request.setAsciiGridData(agrid)
    request.setWorkstationID(__WsId())
    request.setSiteID("")
    
    try:
        serverResponse = db.sendRequest(request)
    except Exception, e:
        logException("Unhandled exception thrown:")
        return False
        
    if not serverResponse.isOkay():
        logProblem("Problem saving grids to db: " + str(serverResponse.message()))
        return False
    
    return True
    
def __WsId() :
    return WsId(progName="ifpAG")

    
if __name__ == '__main__':
    main()    