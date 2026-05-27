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

import logging
import sys

from ufpy import UsageArgumentParser
from ufpy.gfe import IFPClient

from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.server.request import CommitGridRequest


logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s",
                    datefmt="%H:%M:%S",
                    # level=logging.DEBUG)
                    level=logging.INFO)
logger = logging.getLogger("publishGFE")


def validateArgs():
    parser = UsageArgumentParser.UsageArgumentParser(
        prog='publishGFE', conflict_handler="resolve")
    parser.add_argument(
        "-h",
        action="store",
        dest="host",
        help="where the ifpServer is running",
        required=True,
        metavar="hostname")
    parser.add_argument(
        "-r",
        action="store",
        type=int,
        dest="port",
        help="the port that ifpServer is serving",
        required=True,
        metavar="portNumber")
    parser.add_argument(
        "-t",
        action="store",
        dest="definedTR",
        help="(Optional) named time range, supersedes -s and -e",
        default="",
        metavar="namedTR")
    parser.add_argument(
        "-s",
        action=UsageArgumentParser.StoreTimeAction,
        dest="sT",
        help="(Optional) start time, format = yyyymmdd_hhmm",
        metavar="startTime")
    parser.add_argument(
        "-e",
        action=UsageArgumentParser.StoreTimeAction,
        dest="eT",
        help="(Optional) end time, format = yyyymmdd_hhmm",
        metavar="endTime")
    parser.add_argument(
        "-u",
        action="store",
        dest="user",
        help="(Optional) user, defaults to SITE",
        default="SITE",
        metavar="user")
    parser.add_argument(
        "-o",
        action="store",
        dest="site",
        help="(Optional) site to publish grids for, defaults to server's primary site",
        metavar="site")
    parser.add_argument(
        "-p",
        action=UsageArgumentParser.AppendParmNameAndLevelAction,
        dest="parmNamesAndLevels",
        help="(Optional) parm, can have multiple switches; if none, defaults to all parms.",
        default=[],
        metavar="parm")
    options = parser.parse_args()

    tr = TimeRange.allTimes()
    if options.sT is not None:
        tr.setStart(options.sT)
    if options.eT is not None:
        tr.setEnd(options.eT)
    setattr(options, 'tr', tr)

    return options


def expandToParmInv(inv, definedTR):
    expandedTR = definedTR
    for tr in inv:
        if tr.overlaps(definedTR):
            expandedTR = expandedTR.combineWith(tr)

    return expandedTR


def main():
    logger.info("Publish Fcst Data to Official")

    args = validateArgs()
    logger.debug("Command-line arguments: %s", args)

    # build IFPClient object
    db = IFPClient.IFPClient(
        args.host,
        args.port,
        args.user,
        args.site,
        "publishGFE")

    # get site id
    siteID = args.site
    if not siteID:
        sr = db.getSiteID()
        if not sr.isOkay():
            logger.error("Unable to determine site id: %s", sr.message())
            sys.exit(1)
        siteID = sr.getPayload()[0]

    # calculate list of parms
    dbid = DatabaseID(siteID + "_GRID__Fcst_00000000_0000")
    sr = db.getParmList(dbid)
    if not sr.isOkay():
        logger.error("Unable to determine parms: %s", sr.message())
        sys.exit(1)
    parmsInDb = sr.getPayload()
    commitParmList = []
    if len(args.parmNamesAndLevels) == 0:
        commitParmList = parmsInDb
    else:
        for parm in args.parmNamesAndLevels:
            tx = ParmID.parmNameAndLevel(parm)
            found = False
            for dbParm in parmsInDb:
                if dbParm.getParmName(
                ) == tx[0] and dbParm.getParmLevel() == tx[1]:
                    commitParmList.append(dbParm)
                    found = True
            if not found:
                logger.warning(
                    "Unable to find parm [%s] in database [%s]", parm, dbid)

    # calculate time ranges
    tr = args.tr
    if len(args.definedTR) > 0:
        sr = db.getSelectTR(args.definedTR)
        if not sr.isOkay():
            logger.error(
                "Unable to find select tr definition: %s",
                sr.message())
            sys.exit(1)
        tr = sr.getPayload()

    # CommitGridRequest(parmid, commitTime)
    requests = []
    for parm in commitParmList:
        inv = []
        sr = db.getGridInventory(parm)
        if not sr.isOkay():
            logger.error("Unable to get grid inventory: %s", sr.message())
            sys.exit(1)
        inv = sr.getPayload()
        expandTR = expandToParmInv(inv, tr)
        req = CommitGridRequest()
        req.setParmId(parm)
        req.setTimeRange(expandTR)
        requests.append(req)
        logger.debug("%s %s", parm.getParmName(), tr)

    # commit grid
    sr = db.commitGrid(requests)
    if not sr.isOkay():
        logger.error("Unable to publish grids: %s", sr.message())
        sys.exit(1)
    else:
        logger.info("Publish completed.")


if __name__ == '__main__':
    main()
