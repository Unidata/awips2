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

# Gets all available raob data in the A-II database over a specified range of
# times. The data is output to stdout as ASCII.
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    2014-10-15      3598          nabowle        Initial creation. Extracted common code from a2rdmdl*.py
#

import argparse
import numpy

from datetime import datetime
from ufpy.dataaccess import DataAccessLayer
from dynamicserialize.dstypes.com.raytheon.uf.common.time import DataTime
from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange

def get_parser():
    parser = argparse.ArgumentParser(conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="host",
                    help="EDEX server hostname (optional)", metavar="hostname")
    parser.add_argument("--date", action="store", dest="date", 
                    help="The date in YYYY-MM-DD", metavar="date")
    parser.add_argument("--hour", action="store", dest="hour", 
                    help="The hour in HH", metavar="hour")
    parser.add_argument("--fcst", action="store", dest="fcst", 
                    help="The forecast time in hours", metavar="fcst")
    parser.add_argument("--srcId", action="store", dest="srcId", 
                    help="Unique alphanumeric name for gridded data source",
                    metavar="srcId")
    parser.add_argument("--varAbrev", action="store", dest="varAbrev", 
                    help="Variable abreviation", metavar="varAbrev")
    parser.add_argument("--lvlOne", action="store", dest="lvlOne", 
                    help="Level One value", metavar="lvlOne", type=float)
    parser.add_argument("--lvlTwo", action="store", dest="lvlTwo", 
                    help="Level Two value", metavar="lvlTwo", type=float)
    parser.add_argument("--lvlName", action="store", dest="lvlName", 
                    help="Master level name", metavar="lvlName")
    return parser

def do_request(user_args):
    if user_args.host:
        DataAccessLayer.changeEDEXHost(user_args.host)

    srcId = user_args.srcId
    varAbrev = user_args.varAbrev

    if not srcId or not varAbrev:
        raise Exception("srcId or varAbrev not provided")
        return

    date = user_args.date
    hour = user_args.hour
    fcst = user_args.fcst

    if not date or not hour or not fcst:
        raise Exception("date, hour, or fcst not provided")
        return

    dt = datetime.strptime( str(date) + " " + str(hour) + ":00:00.0", "%Y-%m-%d %H:%M:%S.%f")

# check for and build date range if necessary
    daterange = None
    if varAbrev.endswith("hr"):
        import re
        matches = re.findall(r'\d+', varAbrev)
        if matches:
            from datetime import timedelta
            hourRange = int(matches[-1])
            endDate = dt + timedelta(hours=int(fcst))
            beginDate = endDate - timedelta(hours=hourRange)
            daterange = TimeRange(beginDate, endDate)

# convert hours to seconds because DataTime does the reverse
    time = DataTime(dt, int(fcst)*3600, daterange) 

    req = DataAccessLayer.newDataRequest("grid")
    req.setParameters(varAbrev)
    req.addIdentifier("info.datasetId", srcId)

# To handle partial level matches, use identifiers instead of a Level.
    if user_args.lvlName is not None:
        req.addIdentifier("info.level.masterLevel.name", user_args.lvlName)
    if user_args.lvlOne is not None:
        req.addIdentifier("info.level.levelonevalue", numpy.float64(user_args.lvlOne))
    if user_args.lvlTwo is not None:
        req.addIdentifier("info.level.leveltwovalue", numpy.float64(user_args.lvlTwo))

    grids = DataAccessLayer.getGridData(req, [time])

    if not grids:
#        print "Data not available"
        raise Exception("")

    grid = grids[0]
    rawData = grid.getRawData()

    yLen = len(rawData[0]) 
    xLen = len(rawData)

    return grid, xLen, yLen

def get_indices(j, rowLen):
# the lengths are reversed from how getRawData() returns the data and forces
# the need to calculate the dataIdx and arrIdx instead of
# for row in reversed(rawData): for k in row: ...
# it's important to check that arrIdx < len(arr) when arrIdx is incremented
    dataIdx = int(j / rowLen) # index into rawData
    arrIdx = j % rowLen       # index into rawData[dataIdx]
    return dataIdx, arrIdx

