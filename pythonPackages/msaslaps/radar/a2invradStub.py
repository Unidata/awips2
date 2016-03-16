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

# Gets inventories of radar grid data from the A-II database.  The data is
# output to stdout as ASCII. Inventories are limited to Radial and Raster
# formats.
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    2014-10-27      3600          nabowle        Initial modification. Convert to DAF.
#    2014-12-18      3600          nabowle        Use new getAvailableLevels() to speed up retrieval.
#

import argparse
import numpy
import sys

from datetime import datetime
from datetime import timedelta

from awips.dataaccess import DataAccessLayer
from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.level import Level


def get_args():
    parser = argparse.ArgumentParser(conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="host",
                    help="EDEX server hostname (optional)", metavar="hostname")

    parser.add_argument("--icao", action="store", dest="icao", 
                    help="The ICAO (optional)", metavar="icao")

    parser.add_argument("--productCode", action="store", dest="productCode", 
                    help="Product Code (optional)", metavar="productCode")

    parser.add_argument("--angle", action="store", dest="angle", type=float,
                    help="The Primary Elevation Angle (optional)",
                    metavar="angle")

    parser.add_argument("--date", action="store", dest="date", 
                    help="A date to find data within a minute of. (optional, --time required if provided)",
                    metavar="YYYY-MM-DD")

    parser.add_argument("--time", action="store", dest="time", 
                    help="A time to find data within a minute of. (optional, --date required if provided)",
                    metavar="HH:MM")

    parser.add_argument("--outputDate", action="store_true", 
                    dest="outputDate", help="Output the datetime (optional)")

    parser.add_argument("--outputTrue", action="store_const", 
                    dest="outputAngle", const="true",
                    help="Output true elevation angle, if relevant. (optional,"
                    + " exclusive with --outputPrimary)")

    parser.add_argument("--outputPrimary", action="store_const",
                    dest="outputAngle", const="primary",
                    help="Output primary elevation angle, if relevant. "
                    + "(optional, exclusive with --outputTrue)")
    return parser.parse_args()

def main():
    user_args = get_args()

    if user_args.host:
        DataAccessLayer.changeEDEXHost(user_args.host)

    if (user_args.date and not user_args.time) or (user_args.time and not user_args.date):
        print >> sys.stderr, "date and time must be provided together"
        return

    # If a time range is provided, results will be filtered based on available times
    timeRange = None
    if user_args.date:
        midRange = datetime.strptime( user_args.date + " " + user_args.time, "%Y-%m-%d %H:%M")
        beginRange = midRange - timedelta(0, 60)
        endRange = midRange + timedelta(0, 60)
        timeRange = TimeRange(beginRange, endRange)

    req = create_request(user_args)
    if user_args.icao:
        if user_args.productCode: # retrieve available times and/or true or primary elevation angles
            if timeRange:
                tr = timeRange
            else:
                tr = None
            lines = set()

            if user_args.outputAngle:
                levels = DataAccessLayer.getAvailableLevels(req)
                for level in levels:
                    line = ""
                    req.setLevels(level)
                    if user_args.outputDate:
                        times = DataAccessLayer.getAvailableTimes(req)
                        for time in times:
                            if not tr or tr.contains(time.getValidPeriod()):
                                line = str(time) + ".0"
                                line += " "
                                if user_args.outputAngle == "true":
                                    line += "%.1f"%level.getLeveltwovalue()
                                else:
                                    line += "%.1f"%level.getLevelonevalue()
                                lines.add(line)
                    else:
                        if not tr or data_in_time_range(req, tr):
                            if user_args.outputAngle == "true":
                                line = "%.1f"%level.getLeveltwovalue()
                            else:
                                line = "%.1f"%level.getLevelonevalue()
                        lines.add(line)
            else : # just output time
                times = DataAccessLayer.getAvailableTimes(req)
                for time in times:
                    if not tr or tr.contains(time.getValidPeriod()):
                        lines.add(str(time) + ".0")
            msg = "\n".join(lines)
        else: #retrieve available product codes
            unfiltered = DataAccessLayer.getAvailableParameters(req)
            productCodes = []
            for parameter in unfiltered: #filter to just productCodes
                if parameter.isdigit(): 
                    productCodes.append(parameter)
            if timeRange:
                unfiltered = productCodes
                productCodes = []
                for productCode in unfiltered:
                    req = create_request(user_args)
                    req.setParameters(productCode)
                    if data_in_time_range(req, timeRange):
                        productCodes.append(productCode)
            msg = "\n".join(productCodes)
            
    else: # retrieve available icaos
        icaos = DataAccessLayer.getAvailableLocationNames(req)
        if timeRange:
            unfiltered = icaos
            icaos = []
            for icao in unfiltered:
                req = create_request(user_args)
                req.addIdentifier("icao", icao)
                if data_in_time_range(req, timeRange):
                    icaos.append(icao)
                
        msg = "\n".join(icaos)

    print msg.strip()

def create_request(user_args):
    req = DataAccessLayer.newDataRequest("radar")
    if user_args.icao:
        req.addIdentifier("icao", user_args.icao)
    if user_args.productCode:
        req.setParameters(user_args.productCode)
    if user_args.angle is not None:
        level = Level()
        level.setLevelonevalue(user_args.angle)
        req.setLevels(level)
    # Indicate that when providing or requesting levels, the Levelonevalue
    # is the primaryElevationAngle and the Leveltwovalue value is the
    # trueElevationAngle
    req.addIdentifier("level.one.field", "primaryElevationAngle")
    req.addIdentifier("level.two.field", "trueElevationAngle")

    return req

def data_in_time_range(req, timeRange):
    times = DataAccessLayer.getAvailableTimes(req)
    for time in times:
        if timeRange.contains(time.getValidPeriod()):
            return True
    return False

if __name__ == '__main__':
    main()
