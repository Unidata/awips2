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

# Gets all available Laps maritime data in the A-II database over a specified
# range of times. The data is output to stdout as ASCII.  Each line is one
# time/platform combination.  The individual data items are comma delimited.
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/18/2014      3591          nabowle        Initial modification. Replace UEngine with DAF.
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

    req = DataAccessLayer.newDataRequest("sfcobs")
    req.setParameters("stationId","timeObs","elevation","reportType",
                      "wx_present","visibility","seaLevelPress","stationPress",
                      "pressChange3Hour","pressChangeChar","temperature",
                      "dewpoint","seaSurfaceTemp","wetBulb","windDir",
                      "windSpeed","equivWindSpeed10m","windGust","precip1Hour",
                      "precip6Hour","precip24Hour" )
    geometries = DataAccessLayer.getGeometryData(req, timerange)

    if not geometries :
#        print "No data available."
        return

    # Initialize conversion array for wx. 
    wxstr = [ " ", " ", " ", " ", "FU", "HZ", "DU", "BLSA", "PO", "VCSS", \
        "BR", "BCFG", "MIFG", "VCTS", "VCSH", "VCSH", "VCSH", " ", "SQ", "+FC", \
        "DZ", "RA", "SN", "RA SN", "FZRA", "SHRA", "SHRA SHSN", "SHGR", "FG FZFG", "TS", \
        "SS", "SS", "SS", "+SS", "+SS", "+SS", "DRSN", " ", "BLSN", "+BLSN", \
        "VCFG", "BCFG", "FG FZFG", "FG FZFG", "FG FZFG", "FG FZFG", "FG FZFG", "FG FZFG", "FZFG", "FZFG", \
        "-DZ", "-DZ", "DZ", "DZ", "+DZ", "+DZ", "-FZDZ", "FZDZ", "-DZ -RA", "DZ RA", \
        "-RA", "-RA", "RA", "RA", "+RA", "+RA", "-FZRA", "FZRA", "-RA -SN", "RA SN", \
        "-SN", "-SN", "SN", "SN", "+SN", "+SN", "IC", "SG", "IC", "PE", \
        "-SHRA", "SHRA", "+SHRA", "-SHSN -SHRA", "SHSN SHRA", "-SNSN", "SHSN", "-SHPE", "SHPE", " ", \
        "SHGR", "-RA", "+RA", "-RA -SN -GR", "+RA +SN +GR", "TSRA", "TSPE", "+TSRA", " ", "+TSPE" ]

    msg = ""
    for geo in geometries :
        lon = geo.getGeometry().x
        lat = geo.getGeometry().y

        sName = geo.getString("stationId")
        tobs = geo.getNumber("timeObs")
        elev = geo.getNumber("elevation")
        typ = geo.getNumber("reportType")
        wx = geo.getNumber("wx_present")
        vis = geo.getNumber("visibility")
        msl = geo.getNumber("seaLevelPress")
        p = geo.getNumber("stationPress")
        pchg = geo.getNumber("pressChange3Hour")
        pchr =  geo.getNumber("pressChangeChar")
        temp = geo.getNumber("temperature")
        dpt = geo.getNumber("dewpoint")
        th2o = geo.getNumber("seaSurfaceTemp")
        tw = geo.getNumber("wetBulb")
        dir = geo.getNumber("windDir")
        spd = geo.getNumber("windSpeed")
        s10 = geo.getNumber("equivWindSpeed10m")
        gust = geo.getNumber("windGust")
        pr1 = geo.getNumber("precip1Hour")
        pr6 = geo.getNumber("precip6Hour")
        pr24 = geo.getNumber("precip24Hour")

        msg += sName + ","
        msg += str(tobs/1000) + ","
        msg += "%.4f"%lat + ","
        msg += "%.4f"%lon + ","
        msg += "%.0f"%elev + ","
        if typ < 1001 or typ > 1007 :
            msg += "-32767,"
        elif typ == 1001 or typ == 1004 or typ == 1005 :
            msg += "0,"
        else :
            msg += "1,"
        if wx < 0 or wx > 99 :
            msg += " ,"
        else :
            msg += wxstr[wx] + ","
        msg += str(vis) + ","
        msg += "%.2f"%msl + ","
        msg += "%.2f"%p + ","
        msg += "%.0f"%pchg + ","
        if pchr <= -9999 :
           pchr = -32767
        msg += str(pchr) + " ,"
        msg += "%.1f"%temp + ","
        msg += "%.1f"%dpt + ","
        msg += "%.1f"%th2o + ","
        msg += "%.1f"%tw + ","
        msg += "%.0f"%dir + ","
        msg += "%.1f"%spd + ","
        msg += "%.1f"%s10 + ","
        msg += "%.1f"%gust + ","
        msg += "%.2f"%pr1 + ","
        msg += "%.2f"%pr6 + ","
        msg += "%.2f"%pr24 + "\n"

    print msg.strip()

if __name__ == '__main__':
    main()
