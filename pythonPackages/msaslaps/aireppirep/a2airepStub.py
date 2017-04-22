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
# Gets all available pirep data in the A-II database over a specified range of
# times. The data is output to stdout as ASCII.  Each line is one record.
# The individual data items are comma delimited.
#
#
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/25/2014      3405          nabowle        Initial modification. Replaces UEngine with DAF.
#
#


import a2dafcommon
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
    parser.add_argument("--match-legacy", action="store_true", dest="match",
                        help="If set, the legacy script output will be matched.",
                        default=False)
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

    beginRange = datetime.strptime( start + ":00.0", "%Y-%m-%d %H:%M:%S.%f")
    endRange = datetime.strptime( end + ":59.9", "%Y-%m-%d %H:%M:%S.%f")
    timerange = TimeRange(beginRange, endRange)

    req = DataAccessLayer.newDataRequest("airep")
    req.setParameters("id", "flightLevel", "temp", "windDirection", "windSpeed",
                      "flightWeather", "flightHazard", "flightConditions")
    geometries = DataAccessLayer.getGeometryData(req, timerange)

    if not geometries :
#        print "No data available."
        return

    msg = "";

    geometries.sort(key=lambda geo: str(geo.getDataTime()))
    for geoData in geometries :
        mytime = geoData.getDataTime()
        if not mytime:
            continue
        mytime = a2dafcommon.datatime_to_string(mytime)

        geo = geoData.getGeometry()
        if not geo:
            continue

        mylon = geo.x
        mylat = geo.y
        if a2dafcommon.is_no_data(mylat) or a2dafcommon.is_no_data(mylon):
             continue
        mylat = "%.4f"%mylat
        mylon = "%.4f"%mylon

        myflvl = geoData.getNumber("flightLevel")
        if a2dafcommon.is_no_data(myflvl) :
             myflvl = "1e37"
        else :
             myflvl = "%d"%myflvl

#        Temp is not stored.
#        mytemp = geoData.getString("temp")
#        if a2dafcommon.is_no_data(mytemp) :
#            mytemp = "1e37"
#        else :
#            mytemp = "%.1f"%float(mytemp)
        mytemp = "1e37"

#        Wind Direction is not stored.
#        mydir = geoData.getString("windDirection")
#        if a2dafcommon.is_no_data(mydir) :
#            mydir = "1e37"
#        else :
#            mydir = "%d"%int(mydir)
        mydir = "1e37"

        myspd = geoData.getNumber("windSpeed")
        if a2dafcommon.is_no_data(myspd) :
            myspd = "1e37"
        else :
            myspd = "%.1f"%myspd

        myfwx = geoData.getNumber("flightWeather")
        if myfwx :
            myfwx = "-1"
        else :
            myfwx = "%d"%myfwx

        myhaz = geoData.getNumber("flightHazard")
        if a2dafcommon.is_no_data(myhaz) :
            myhaz = "-1"
        else :
            myhaz = "%d"%myhaz

        mycond = geoData.getNumber("flightConditions")
        if a2dafcommon.is_no_data(mycond) :
            mycond = "-1"
        else :
            mycond = "%d"%mycond

        if user_args.match:
#           Wind Speed not returned to legacy script.
            myspd = "1e37"       

        msg += mylat + "|" + mylon + "," + mytime + "," + myflvl + ",AIREP,," + \
            mytemp + "," + mydir + "," + myspd + ",1e37," + \
            myfwx + "," + myhaz + "," + mycond + ",,0,,,,0,,,,,0,,,,\n"

    print msg.strip()

if __name__ == '__main__':
    main()
