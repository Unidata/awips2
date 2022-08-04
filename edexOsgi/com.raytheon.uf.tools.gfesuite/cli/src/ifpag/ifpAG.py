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
import base64
import logging
import sys
import urllib.request
import urllib.error
import urllib.parse

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GetASCIIGridsRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import SaveASCIIGridsRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId
from ufpy import ThriftClient
from ufpy import UsageArgumentParser
from ufpy.localization import LocalizationUtil


##
# The asciiFormatter formats input and output files in ASCII format.
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Apr 12, 2011  8983     dgilling  Initial Creation.
# Aug 08, 2019  7882     dgilling  Updated for Python 3
# Aug 20, 2020  7882     dgilling  Re-factor based on localization REST service
# Sep 08, 2020  8224     randerso  Clean up localization REST implementation
#
##


logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s",
                    datefmt="%H:%M:%S",
                    # level=logging.DEBUG)
                    level=logging.INFO)
logger = logging.getLogger("ifpAG.py")

url = urllib.parse.urlunparse(["http",
                               "{host}:{port}",
                               "/services/localization/common_static/user/{userName}/{locPath}",
                               "",
                               "",
                               ""])

WSID = None

## Custom actions for ArgumentParser object ##
class AppendDatabaseIDAction(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        did = DatabaseID(values)
        if did.isValid():
            if hasattr(namespace, self.dest) and getattr(
                    namespace, self.dest) is not None:
                currentValues = getattr(namespace, self.dest)
                currentValues.append(did)
                setattr(namespace, self.dest, currentValues)
            else:
                setattr(namespace, self.dest, [did])
        else:
            parser.error("DatabaseID [" + values + "] not valid identifier")


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
    
    global WSID
    WSID = WsId(userName=options.userName, progName="ifpAG")


    logEvent("ifpAG Starting")

    if hasattr(options, "outputFile"):
        logVerbose("outputFile=" + str(options.outputFile))
    if hasattr(options, "inputFile"):
        logVerbose("inputFile=" + str(options.inputFile))
    logVerbose("hostName=" + str(options.hostName))
    logVerbose("portNumber=" + str(options.portNumber))
    logVerbose("databaseIds=" + str(options.databaseIds))
    logVerbose("parmIds=" + str(options.parmIds))
    logVerbose("startTime=" + str(options.startTime))
    logVerbose("endTime=" + str(options.endTime))
    logVerbose("userName=" + str(options.userName))
    logVerbose("coordConversionString=" + str(options.coordConversionString))

    statusCode = 0
    if hasattr(options, "outputFile") and options.outputFile:
        statusCode = outputAG(
            options.hostName,
            options.portNumber,
            options.databaseIds,
            options.parmIds,
            options.startTime,
            options.endTime,
            options.outputFile,
            options.coordConversionString,
            options.userName)
    elif hasattr(options, "inputFile") and options.inputFile:
        statusCode = inputAG(options.inputFile, options.hostName, options.portNumber, options.userName)

    logEvent("ifpAG Finished")
    sys.exit(statusCode)


def inputAG(inputFile, hostName, portNumber, userName):
    logEvent("Reading in ASCII Grid File")

    (aGrids, isSaved) = sendGridTempData(inputFile, hostName, portNumber, userName)

    if not isSaved:
        logProblem("Problems sending ASCII grid data to EDEX server.")
        return 2

    if not saveGridSlices(hostName, portNumber, userName, aGrids):
        logProblem("Problems saving some grids")
        return 3

    return 0


def outputAG(hostName, portNumber, databaseIds, parmIds, startTime, endTime,
             outputFile, coordConversionString, userName):
    
    # get the grid slices
    (serverFile, okay) = getGridSlices(hostName, portNumber, userName, databaseIds,
                                       parmIds, startTime, endTime, coordConversionString)
    if not okay:
        return 3

    # output the grid slices to the output file
    fh = None
    if outputFile != "-":
        try:
            fh = open(outputFile, 'wb')
        except Exception:
            logException(
                "Unable to open the file " +
                outputFile +
                " for output")
            return 2
    else:
        fh = sys.stdout.buffer

    logEvent("Outputting ASCII Grid File")

    srcUrl = url.format(host=hostName, port=portNumber, userName=userName, locPath=serverFile)
    logVerbose(f"Source URL: {srcUrl}")
    checksum = 'NON_EXISTENT_CHECKSUM'
    try:
        with urllib.request.urlopen(srcUrl) as response:
            fh.write(response.read())
            checksum = response.headers["Content-MD5"]
    except urllib.error.HTTPError:
        logException("Failed to retrieve ifpAG file [" + srcUrl + "]")
        okay = False
    finally:
        if fh:
            fh.close()

    # delete server-side file
    base64string = base64.b64encode('{0}:{0}'.format(userName).encode())
    authString = "Basic {}".format(base64string.decode())
    headers = {"If-Match": checksum, "Authorization": authString, }
    request = urllib.request.Request(srcUrl, headers=headers, method='DELETE')
    try:
        urllib.request.urlopen(request)
    except urllib.error.HTTPError as e:
        if e.code == 409:
            logException(
                "File version conflict detected: " +
                e.read().decode())
        else:
            logException(
                "Could not delete temporary file [" +
                srcUrl +
                "] from localization server")
        okay = False

    if okay:
        return 0
    else:
        return 3


def decodeArguments():
    parser = UsageArgumentParser.UsageArgumentParser(prog='ifpAG', conflict_handler="resolve", usage="""ifpAG -o outputFile -h hostName -r rpcPortNumber -d databaseID [-p parmID] [-s startTime] [-e endTime] [-u username] [-c coordConversionString]
or
ifpAG -i inputFile -h hostName -r rpcPortNumber [-u username]
""")
    modeGroup = parser.add_mutually_exclusive_group(required=True)
    modeGroup.add_argument(
        "-o",
        action="store",
        dest="outputFile",
        help="Specifies the name of the output file.",
        metavar="outputFile")
    modeGroup.add_argument(
        "-i",
        action="store",
        dest="inputFile",
        help="Specifies the name of the input file.",
        metavar="inputFile")
    parser.add_argument(
        "-h",
        action="store",
        dest="hostName",
        help=(
            "Host name upon which the EDEX server is running. "
            "Please specify the host name of the dv3 or dv4 server."),
        required=True,
        metavar="hostName")
    parser.add_argument(
        "-r",
        action="store",
        type=int,
        dest="portNumber",
        help="the port that ifpServer is serving",
        required=True,
        metavar="portNumber")
    parser.add_argument(
        "-u",
        action="store",
        dest="userName",
        help="If no username is specified, then your username is used.",
        metavar="username")
    parser.add_argument(
        "-d",
        action=AppendDatabaseIDAction,
        dest="databaseIds",
        help=(
            "DatabaseID from which to get the data.  There may "
            "be several DatabaseIDs specified. "
            "format: (DEN_GRID__eta_19980604_1200)"),
        metavar="databaseID")
    parser.add_argument(
        "-p",
        action="append",
        dest="parmNames",
        help=(
            "If none specified, get all parms for "
            "all databases listed.  There may be several "
            "parmIDs specified. "
            "format: (Temp)"),
        metavar="parmID")
    parser.add_argument(
        "-s",
        action=UsageArgumentParser.StoreTimeAction,
        dest="startTime",
        help=(
            "If no start time specified, make start"
            "time = 0. format: (19980604_1200)"),
        metavar="startTime")
    parser.add_argument(
        "-e",
        action=UsageArgumentParser.StoreTimeAction,
        dest="endTime",
        help=(
            "If no end time specified, make end "
            "time = Abstime::MaxFutureTime(). "
            "format: (19980604_1200)"),
        metavar="endTime")
    parser.add_argument(
        "-c",
        action="store",
        dest="coordConversionString",
        help=(
            "If no coordinate conversion string "
            "is specified, then output is done in the "
            "server's inherent resolution. Must be quoted. "
            "String format:"
            "xsize ysize projID originX originY extentX extentY"
            "\"35 35 Grid211 35.0 14.0 9.0 9.0\""
            "Coord conversion is automatic on input."),
        metavar="coordinateConversionString")
    options = parser.parse_args()
    options.parmIds = []

    # check that we have the required arguments for either output or input
    if options.outputFile is not None:
        # for output: hostName, RPC port number, database id are required
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
                        options.parmIds.append(
                            ParmID(parm[:pos], dbID, parm[pos + 1:]))
                    else:
                        options.parmIds.append(ParmID(parm, dbID))

    # if no username, assign SITE
    if options.userName is None:
        options.userName = LocalizationUtil.getUser()

    return options


def getGridSlices(hostName, portNumber, userName, databaseIds, parmIds, startTime,
                  endTime, coordConversionString):
    logEvent("Getting Grids from ifpServer")
    request = GetASCIIGridsRequest()
    request.setDatabaseIds(databaseIds)
    request.setParmIds(parmIds)
    tr = TimeRange()
    tr.setStart(startTime)
    tr.setEnd(endTime)
    request.setTimeRange(tr)
    request.setCoordConversionString(coordConversionString)
    request.setWorkstationID(WSID)
    request.setSiteID(databaseIds[0].getSiteId())

    # access the database
    db = ThriftClient.ThriftClient(hostName, portNumber)


    try:
        serverResponse = db.sendRequest(request)
    except Exception:
        logException("Unhandled exception thrown:")
        return ("", False)

    if (not serverResponse.isOkay()):
        logProblem("Problem getting grids from db: " +
                   str(serverResponse.message()))
        return ("", False)

    return (serverResponse.getPayload(), True)


def sendGridTempData(inputFile, hostName, portNumber, userName):
    fh = None
    if inputFile != "-":
        try:
            fh = open(inputFile, 'rb')
        except Exception:
            logException("Unable to open the file " + inputFile + " for input")
            return ("", False)
    else:
        fh = sys.stdin.buffer

    # Store the main file
    filename = "/gfe/ifpAG/ifpAG" + str(hash(WSID)) + ".txt"
    destUrl = url.format(host=hostName, port=portNumber, userName=userName, locPath=filename)
    logVerbose(f"Destination URL: {destUrl}")
    checksum = "NON_EXISTENT_CHECKSUM"
    base64string = base64.b64encode('{0}:{0}'.format(userName).encode())
    authString = "Basic {}".format(base64string.decode())
    headers = {'If-Match': checksum,
               'Authorization': authString,
               'Content-Type': 'application/octet-stream', }
    request = urllib.request.Request(
        destUrl,
        data=fh.read(),
        headers=headers,
        method='PUT')

    okay = True
    try:
        urllib.request.urlopen(request)
    except urllib.error.HTTPError:
        logException(
            "Unable to save temp ASCII grids file " +
            inputFile +
            " to EDEX server.")
        okay = False

    if fh is not None:
        fh.close()

    return (filename, okay)


def saveGridSlices(hostName, portNumber, userName, agrid):
    logEvent("Saving Grids to ifpServer")

    request = SaveASCIIGridsRequest()
    request.setAsciiGridData(agrid)
    request.setWorkstationID(WSID)
    request.setSiteID("")

    # access the database
    db = ThriftClient.ThriftClient(hostName, portNumber)

    try:
        serverResponse = db.sendRequest(request)
    except Exception as e:
        logException("Unhandled exception thrown:")
        return False

    if not serverResponse.isOkay():
        logProblem("Problem saving grids to db: " +
                   str(serverResponse.message()))
        return False

    return True


if __name__ == '__main__':
    main()
