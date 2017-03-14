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
#    Oct 10, 2014    3595          nabowle        Initial modification. Fix Man and SigW indices.
#    Oct 10, 2014    3595          nabowle        Replace UEngine with DAF.
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
    parser.add_argument("--use-station-name", action='store_true', default=False,
                    dest="stationname", help="Output the station name instead of station id for the first output field.")
    return parser.parse_args()

def main():
    # The multi-dimensional parameters.
    MAN_PARAMS = set(['prMan', 'htMan', 'tpMan', 'tdMan', 'wdMan', 'wsMan'])
    TROP_PARAMS = set(['prTrop', 'tpTrop', 'tdTrop', 'wdTrop', 'wsTrop'])
    MAXW_PARAMS = set(['prMaxW', 'wdMaxW', 'wsMaxW'])
    SIGT_PARAMS = set(['prSigT', 'tpSigT', 'tdSigT'])
    SIGW_PARAMS = set(['htSigW', 'wdSigW', 'wsSigW'])

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

    req = DataAccessLayer.newDataRequest("bufrua")
    req.setParameters("wmoStaNum", "validTime", "rptType", "staElev", "numMand",
                      "numSigT", "numSigW", "numTrop", "numMwnd", "staName")
    req.getParameters().extend(MAN_PARAMS)
    req.getParameters().extend(TROP_PARAMS)
    req.getParameters().extend(MAXW_PARAMS)
    req.getParameters().extend(SIGT_PARAMS)
    req.getParameters().extend(SIGW_PARAMS)

    geometries = DataAccessLayer.getGeometryData(req, timerange)

    if not geometries :
#        print "couldn't get data"
        return


    manGeos = []
    tropGeos = []
    maxwGeos = []
    sigtGeos = []
    sigwGeos = []
    # D-2D format files expect depression
    tdman = []
    tdsigt = []
    tdtrop = []
    msg = ""
    for geoData in geometries :
        if set(geoData.getParameters()) & MAN_PARAMS :
            manGeos.append(geoData)
            td = geoData.getNumber("tdMan")
            tp = geoData.getNumber("tpMan")
            if td >150 and td<=tp :
               tdman.append(tp-td)
            else :
               tdman.append(-9999.0)
            continue
        if set(geoData.getParameters()) & TROP_PARAMS :
            tropGeos.append(geoData)
            td = geoData.getNumber("tdTrop")
            tp = geoData.getNumber("tpTrop")
            if td>150 and td<=tp :
               tdtrop.append(tp-td)
            else :
               tdtrop.append(-9999.0)
            continue
        if set(geoData.getParameters()) & MAXW_PARAMS :
            maxwGeos.append(geoData)
            continue
        if set(geoData.getParameters()) & SIGT_PARAMS :
            sigtGeos.append(geoData)
            td = geoData.getNumber("tdSigT")
            tp = geoData.getNumber("tpSigT")
            if td>150 and td<=tp :
               tdsigt.append(tp-td)
            else :
               tdsigt.append(-9999.0)
            continue
        if set(geoData.getParameters()) & SIGW_PARAMS :
            sigwGeos.append(geoData)
            continue

        if len(manGeos) == 0 and len(sigtGeos) == 0 and len(sigwGeos) == 0 or geoData.getNumber("rptType") > 2022 :
            manGeos = []
            tropGeos = []
            maxwGeos = []
            sigtGeos = []
            sigwGeos = []
            tdman = []
            tdsigt = []
            tdtrop = []
            continue

        if user_args.stationname and geoData.getString("staName") :
            msg += geoData.getString("staName") + ","
        else :
            msg += geoData.getString("wmoStaNum") + "," 
        msg += str(geoData.getNumber("validTime")/1000) + ","
        msg += "%.4f"%geoData.getGeometry().y + ","
        msg += "%.4f"%geoData.getGeometry().x + ","
        msg += "%.0f"%geoData.getNumber("staElev") + ","
        msg += geoData.getString("wmoStaNum") + ","

        kk = len(manGeos)
        msg += str(kk) + ","
        if kk>0 : 
            msg += "%.1f"%manGeos[0].getNumber("prMan")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%manGeos[k].getNumber("prMan")
                k += 1
            msg += ","

            msg += "%.1f"%geoData.getNumber("staElev")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%manGeos[k].getNumber("htMan")
                k += 1
            msg += ","

            msg += "%.1f"%manGeos[0].getNumber("tpMan")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%manGeos[k].getNumber("tpMan")
                k += 1
            msg += ","

            msg += "%.1f"%tdman[0]
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%tdman[k];
                k += 1
            msg += ","

            msg += "%.1f"%manGeos[0].getNumber("wsMan")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%manGeos[k].getNumber("wsMan")
                k += 1
            msg += ","

            msg += "%.1f"%manGeos[0].getNumber("wdMan")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%manGeos[k].getNumber("wdMan")
                k += 1
            msg += ","
        else :
            msg += ",,,,,,"

        kk = len(sigtGeos)
        msg += str(kk) + ","
        if kk>0 : 
            msg += "%.1f"%sigtGeos[0].getNumber("prSigT")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%sigtGeos[k].getNumber("prSigT")
                k += 1
            msg += ","

            msg += "%.1f"%sigtGeos[0].getNumber("tpSigT")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%sigtGeos[k].getNumber("tpSigT")
                k += 1
            msg += ","

            msg += "%.1f"%tdsigt[0]
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%tdsigt[k]
                k += 1
            msg += ","
        else :
            msg += ",,,"
            
        kk = len(sigwGeos)
        msg += str(kk) + ","
        if kk>0 : 
            msg += "%.1f"%sigwGeos[0].getNumber("htSigW")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%sigwGeos[k].getNumber("htSigW")
                k += 1
            msg += ","

            msg += "%.1f"%sigwGeos[0].getNumber("wsSigW")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%sigwGeos[k].getNumber("wsSigW")
                k += 1
            msg += ","

            msg += "%.1f"%sigwGeos[0].getNumber("wdSigW")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%sigwGeos[k].getNumber("wdSigW")
                k += 1
            msg += ","
        else :
            msg += ",,,"

        kk = len(tropGeos)
        msg += str(kk) + ","
        if kk>0 : 
            msg += "%.1f"%tropGeos[0].getNumber("prTrop")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%tropGeos[k].getNumber("prTrop")
                k += 1
            msg += ","

            msg += "%.1f"%tropGeos[0].getNumber("tpTrop")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%tropGeos[k].getNumber("tpTrop")
                k += 1
            msg += ","

            msg += "%.1f"%tdtrop[0]
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%tdtrop[k]
                k += 1
            msg += ","

            msg += "%.1f"%tropGeos[0].getNumber("wsTrop")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%tropGeos[k].getNumber("wsTrop")
                k += 1
            msg += ","

            msg += "%.1f"%tropGeos[0].getNumber("wdTrop")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%tropGeos[k].getNumber("wdTrop")
                k += 1
            msg += ","
        else :
            msg += ",,,,,"

        kk = len(maxwGeos)
        msg += str(kk) + ","
        if kk>0 : 
            msg += "%.1f"%maxwGeos[0].getNumber("prMaxW")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%maxwGeos[k].getNumber("prMaxW")
                k += 1
            msg += ","

            msg += "%.1f"%maxwGeos[0].getNumber("wsMaxW")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%maxwGeos[k].getNumber("wsMaxW")
                k += 1
            msg += ","

            msg += "%.1f"%maxwGeos[0].getNumber("wdMaxW")
            k = 1
            while k < kk :
                msg += "|" + "%.1f"%maxwGeos[k].getNumber("wdMaxW")
                k += 1
        else :
            msg += ",,"
        msg += "\n"

        manGeos = []
        tropGeos = []
        maxwGeos = []
        sigtGeos = []
        sigwGeos = []
        tdman = []
        tdsigt = []
        tdtrop = []

    print msg.strip()

if __name__ == '__main__':
    main()
