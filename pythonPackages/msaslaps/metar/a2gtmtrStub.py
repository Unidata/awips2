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

# Gets all available metar data in the A-II database over a specified range of
# times within a specifed area.  The data is output to stdout as ASCII.
# Each line is one time/station combination. The individual data items are comma
# delimited.
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/15/2014      3593          nabowle        Initial modification. Fix losing first record.
#    09/15/2014      3593          nabowle        Replace UEngine with DAF.
#
#

import argparse
import sys

from datetime import datetime
from awips.dataaccess import DataAccessLayer
from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange

def get_args():
    parser = argparse.ArgumentParser(conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="host",
                        help="EDEX server hostname (optional)",
                        metavar="hostname")
    parser.add_argument("-b", action="store", dest="start", 
                    help="The start of the time range in YYYY-MM-DD HH:MM",
                    metavar="start")
    parser.add_argument("-e", action="store", dest="end", 
                    help="The end of the time range in YYYY-MM-DD HH:MM",
                    metavar="end")
    parser.add_argument("--lat-min", action="store", dest="latMin", type=float,
                    help="Minimum latitude", default=0.0, metavar="lat")
    parser.add_argument("--lat-max", action="store", dest="latMax", type=float,
                    help="Maximum latitude", default=90.0, metavar="lat")
    parser.add_argument("--lon-min", action="store", dest="lonMin", type=float,
                    help="Minimum longitude", default=-180.0, metavar="lon")
    parser.add_argument("--lon-max", action="store", dest="lonMax", type=float,
                    help="Maximum longitude", default=180.0, metavar="lon")
    return parser.parse_args()


def main():
    user_args = get_args()

    if user_args.host:
        DataAccessLayer.changeEDEXHost(user_args.host)

    start = user_args.start
    end = user_args.end

    if not start or not end:
        print >> sys.stderr, "Start or End date not provided"
        return

    latMin = user_args.latMin
    latMax = user_args.latMax
    lonMin = user_args.lonMin
    lonMax = user_args.lonMax

    beginRange = datetime.strptime( start + ":00.0", "%Y-%m-%d %H:%M:%S.%f")
    endRange = datetime.strptime( end + ":59.9", "%Y-%m-%d %H:%M:%S.%f")
    timerange = TimeRange(beginRange, endRange)

    req = DataAccessLayer.newDataRequest("obs")
    req.setParameters("stationName","timeObs","wmoId","autoStationType",
                      "elevation","seaLevelPress","temperature","dewpoint",
                      "windDir","windSpeed","altimeter" )
    geometries = DataAccessLayer.getGeometryData(req, timerange)

    if not geometries :
#        print "No data available."
        return

    msg = ""
    for geo in geometries :
        lon = geo.getGeometry().x
        lat = geo.getGeometry().y
        if lon < lonMin or lon > lonMax or lat < latMin or lat > latMax:
            continue

        sName = geo.getString("stationName")
        tobs = geo.getNumber("timeObs")
        elev = geo.getNumber("elevation")
        ista = geo.getString("wmoId")
        atype = geo.getString("autoStationType")
        msl = geo.getNumber("seaLevelPress")
        temp = geo.getNumber("temperature")
        dpt = geo.getNumber("dewpoint")
        dir = geo.getNumber("windDir")
        spd = geo.getNumber("windSpeed")
        alt = geo.getNumber("altimeter")

        msg += sName + ","
        msg += str(tobs/1000) + ","
        msg += "%.4f"%lat + ","
        msg += "%.4f"%lon + ","
        msg += "%.0f"%elev + ","
        msg += str(ista) + ","
        msg += atype + " ,"
        msg += "%.2f"%msl + ","
        msg += "%.1f"%temp + ","
        msg += "%.1f"%dpt + ","
        msg += "%.0f"%dir + ","
        msg += "%.1f"%spd + ","
        msg += "%.2f"%alt + "\n"

    print msg.strip()

if __name__ == '__main__':
    main()
