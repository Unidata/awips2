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

# Gets all available Laps metar data in the A-II database over a specified range
# of times within a specifed area.  The data is output to stdout as ASCII.
# Each line is one time/station combination. The individual data items are comma
# delimited.
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/15/2014      3593          nabowle        Initial modification. Fix index issues on 2D parameters.
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
    # The multi-dimensional parameters.
    PRES_PARAMS = set(["presWeather"])
    SKY_PARAMS = set(["skyCover", "skyLayerBase"])

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
        "elevation","reportType","presWeather","visibility","skyCover",
        "skyLayerBase","altimeter","seaLevelPress","pressChange3Hour",
        "pressChangeChar","temperature","tempFromTenths","dewpoint",
        "dpFromTenths","windDir","windSpeed","windGust","maxTemp24Hour",
        "minTemp24Hour""","precip1Hour","precip3Hour","precip6Hour",
        "precip24Hour")
    geometries = DataAccessLayer.getGeometryData(req, timerange)

    if not geometries :
#        print "No data available."
        return

    msg = ""
    wx = []
    cvr = []
    bas = []
    for geoData in geometries:
        if set(geoData.getParameters()) & PRES_PARAMS :
            wx.append(geoData.getString("presWeather"))
            continue
        if set(geoData.getParameters()) & SKY_PARAMS :
            cvr.append(geoData.getString("skyCover"))
            bas.append(geoData.getNumber("skyLayerBase"))
            continue

        lon = geoData.getGeometry().x
        lat = geoData.getGeometry().y
        if lon < lonMin or lon > lonMax or lat < latMin or lat > latMax:
            wx = []
            cvr = []
            bas = []
            continue

        sName = geoData.getString("stationName")
        tobs = geoData.getNumber("timeObs")
        elev = geoData.getNumber("elevation")
        ista = geoData.getNumber("wmoId")
        atype = geoData.getString("autoStationType")
        repTyp = geoData.getString("reportType")
        vis = geoData.getNumber("visibility")
        alt = geoData.getNumber("altimeter")
        msl = geoData.getNumber("seaLevelPress")
        pchg = geoData.getNumber("pressChange3Hour")
        pchr =  geoData.getString("pressChangeChar")
        temp = geoData.getNumber("temperature")
        t10 = geoData.getNumber("tempFromTenths")
        dpt = geoData.getNumber("dewpoint")
        td10 = geoData.getNumber("dpFromTenths")
        dir = geoData.getNumber("windDir")
        spd = geoData.getNumber("windSpeed")
        gust = geoData.getNumber("windGust")
        tmx = geoData.getNumber("maxTemp24Hour")
        tmn = geoData.getNumber("minTemp24Hour")
        pr1 = geoData.getNumber("precip1Hour")
        pr3 = geoData.getNumber("precip3Hour")
        pr6 = geoData.getNumber("precip6Hour")
        pr24 = geoData.getNumber("precip24Hour")


        msg += sName + ","
        msg += str(tobs/1000) + ","
        msg += "%.4f"%lat + ","
        msg += "%.4f"%lon + ","
        msg += "%.0f"%elev + ","
        if ista < 0 :
            msg += "-99,"
        else :
            msg += str(ista) + ","
        msg += atype + " ,"
        msg += repTyp + " ,"
        msg += wx[0] + " ,"
        msg += "%.3f"%vis + ","


        msg += cvr[0];
        kk = 5
        while kk > 0 and cvr[0+kk] == "" :
            kk -= 1
        k = 1
        while k <= kk :
            msg += "|" + cvr[0+k];
            k += 1
        msg += " ,"
        msg += "%.1f"%bas[0];
        kk = 5
        while kk > 0 and bas[0+kk] < -9998 :
            kk -= 1
        k = 1
        while k <= kk :
            msg += "|" + "%.1f"%bas[0+k];
            k += 1
        msg += ","

        msg += "%.2f"%alt + ","
        msg += "%.2f"%msl + ","
        msg += "%.0f"%pchg + ","
        msg += pchr + " ,"
        msg += "%.1f"%temp + ","
        msg += "%.1f"%t10 + ","
        msg += "%.1f"%dpt + ","
        msg += "%.1f"%td10 + ","
        msg += "%.0f"%dir + ","
        msg += "%.1f"%spd + ","
        msg += "%.1f"%gust + ","
        msg += "%.1f"%tmx + ","
        msg += "%.1f"%tmn + ","
        msg += "%.2f"%pr1 + ","
        msg += "%.2f"%pr3 + ","
        msg += "%.2f"%pr6 + ","
        msg += "%.2f"%pr24 + "\n"

        wx = []
        cvr = []
        bas = []

    print msg.strip()

if __name__ == '__main__':
    main()
