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
# Gets all available pirep data in the A-II database over a specified time
# range. The data is output to stdout as ASCII.  Each line is one record. The
# individual data items are comma delimited.
#
# The legacy script does not retrieve any values not stored in the postgres db.
# To compensate for this in side-by-side comparison, a --match-legacy flag is
# provided that will ignore these fields.
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
    parser.add_argument("-t", action="store_true", dest="typecode",
                    help="If set, more type information is displayed.",
                    default=False)
    return parser.parse_args()


def main():
    MULTI_DIM_PARAMS = set(["hazardType", 
                            "turbType", "turbBaseHeight", "turbTopHeight",
                            "iceType", "iceBaseHeight", "iceTopHeight",
                            "skyCover1", "skyCover2", "skyBaseHeight", "skyTopHeight"
                            ])

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

    req = DataAccessLayer.newDataRequest("pirep")
    req.setParameters('id', 'flightLevel', 'temp', 'windDirection', 'windSpeed',
                      'horzVisibility', 'aircraftType', 'weatherGroup')
    req.getParameters().extend(MULTI_DIM_PARAMS)
    geometries = DataAccessLayer.getGeometryData(req, timerange)

    if not geometries :
#        print "No data available."
        return

    typecode = user_args.typecode
    match = user_args.match
    msg = ""
    layerData = []
    combinedGeos = []
    for geoData in geometries :
#       The DAF returns multi-dimensional parameters as separate results before
#       the base result that contain the single-dimensional parameters.
#       Because of the separation of parameters and known ordering of result
#       types, we can easily figure out what each result is and correlate the
#       ancillary data with the base data.
        if set(geoData.getParameters()) & MULTI_DIM_PARAMS :
            layerData.append(geoData)
            continue

        combinedGeos.append({"base":geoData, "layers":layerData})
        layerData = []

    combinedGeos.sort(key=lambda geoMap: str(geoMap['base'].getDataTime()))

    for geoMap in combinedGeos :
        geoData = geoMap['base']
        layerData = geoMap['layers']

        mytime = geoData.getDataTime()
        if not mytime:
            continue
        mytime = mytime = a2dafcommon.datatime_to_string(mytime)

        geo = geoData.getGeometry()
        if not geo:
            continue

        mylon = geo.x
        mylat = geo.y
        if a2dafcommon.is_no_data(mylat) or a2dafcommon.is_no_data(mylon) :
            continue
        mylat = "%.4f"%mylat
        mylon = "%.4f"%mylon

        myflvl = geoData.getNumber("flightLevel")
        if a2dafcommon.is_no_data(myflvl) :
            myflvl = "1e37"
        else :
            myflvl = "%d"%myflvl

#        Temp is not stored.
#        mytemp = geoData.getNumber("temp")
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

        myvis = geoData.getNumber("horzVisibility")
        if a2dafcommon.is_no_data(myvis) :
            myvis = "1e37"
        else :
            myvis = "%.1f"%myvis

        mycraft = geoData.getString("aircraftType")
        if a2dafcommon.is_no_data(mycraft) :
            mycraft = ""

        mywx = geoData.getString("weatherGroup")
        if a2dafcommon.is_no_data(mywx) :
            mywx = ""


        cc = 0
        cldBas = ""
        cldTop = ""
        cldVal = ""
        ii = 0
        icgBas = ""
        icgTop = ""
        icgTyp = ""
        icgVal = ""
        tt = 0
        trbBas = ""
        trbTop = ""
        trbTyp = ""
        trbVal = ""

        if match :
#           Speed, Horizontal Visibility, aircraft type, and weather group are
#           not returned to the legacy script. Flightlevel of -9999 is output
#           as such instead of being replaced by "1e37"
            myspd = "1e37"
            myvis = "-9999998.0"
            mycraft = ""
            mywx = ""
            if geoData.getNumber("flightLevel") == -9999:
                myflvl = "-9999"
        else :
            for pld in layerData:
                sep = ""
                ltyp = pld.getString("hazardType")
                if a2dafcommon.is_no_data(ltyp) :
                    continue

                fval = pld.getString("skyCover1")
                if fval ==  "None" :
                    fval = ""

                sval = pld.getString("skyCover2")
                if sval ==  "None" :
                    sval = ""
                if ltyp == "CLOUD" :
                    if fval == "TOP" :
                        fval = ""
                    if sval == "TOP" :
                        sval = ""
                    if sval != "" :
                        fval += "-"+sval
                    if typecode :
                        if fval == "CLR" :
                            fval = "0"
                        elif fval == "OVC" :
                            fval = "8"
                        elif fval == "SCT" :
                            fval = "11"
                        elif fval == "BKN" :
                            fval = "12"
                        elif fval == "FEW" :
                            fval = "13"
                        else :
                            continue
                    if cldBas != "" :
                        sep = "|"

                    base = pld.getNumber("skyBaseHeight")
                    if a2dafcommon.is_no_data(base) or base == 99999 :
                        base = "1e37"
                    else :
                        base = "%.0f"%float(base)

                    top = pld.getNumber("skyTopHeight")
                    if a2dafcommon.is_no_data(top) or top == 99999 :
                        top = "1e37"
                    else :
                        top = "%.0f"%float(top)

                    cldBas += sep+base
                    cldTop += sep+top
                    cldVal += sep+fval
                    cc += 1
                elif ltyp == "ICING" :
                    dtyp = pld.getString("iceType")
                    if a2dafcommon.is_no_data(dtyp) :
                        dtyp = ""

                    if sval != "" :
                        fval += "-"+sval
                    if icgBas != "" :
                        sep = "|"
                    if typecode :
                        if dtyp == "RIME" :
                            dtyp = "1"
                        elif dtyp == "CLR" :
                            dtyp = "2"
                        elif dtyp == "MXD" :
                            dtyp = "3"
                        else :
                            dtyp = "-9999"
                        if fval == "NEG" :
                            fval = "0";
                        elif fval == "TRACE" :
                            fval = "1"
                        elif fval == "TRACE-LGT" :
                            fval = "2"
                        elif fval == "LGT" :
                            fval = "3"
                        elif fval == "LGT-MOD" :
                            fval = "4"
                        elif fval == "MOD" :
                            fval = "5"
                        elif fval == "MOD-SEV" :
                            fval = "7"
                        elif fval == "SEV" :
                            fval = "8"
                        else :
                            fval = "-9999"
                        if fval == "-9999" and dtyp == "-9999" :
                            continue

                    base = pld.getNumber("iceBaseHeight")
                    if a2dafcommon.is_no_data(base) or base == 99999 :
                        base = "1e37"
                    else :
                        base = "%.0f"%float(base)

                    top = pld.getNumber("iceTopHeight")
                    if a2dafcommon.is_no_data(top) or top == 99999 :
                        top = "1e37"
                    else :
                        top = "%.0f"%float(top)

                    icgBas += sep+base
                    icgTop += sep+top
                    icgTyp += sep+dtyp
                    icgVal += sep+fval
                    ii += 1
                elif ltyp == "TURBC" :
                    dtyp = pld.getString("turbType")
                    if a2dafcommon.is_no_data(dtyp) :
                        dtyp = ""

                    if sval != "" :
                        fval += "-"+sval
                    if typecode :
                        if dtyp == "CAT" :
                            dtyp = "1"
                        elif dtyp == "CHOP" :
                            dtyp = "2"
                        else :
                            dtyp = "-9999"
                        if fval == "NEG" :
                            fval = "0";
                        elif fval == "LGT" :
                            fval = "2"
                        elif fval == "LGT-MOD" :
                            fval = "3"
                        elif fval == "MOD" :
                            fval = "4"
                        elif fval == "MOD-SEV" :
                            fval = "5"
                        elif fval == "SEV" :
                            fval = "6"
                        elif fval == "EXTRM" :
                            fval = "8"
                        else :
                            fval = "-9999"
                        if fval == "-9999" and dtyp == "-9999" :
                            continue
                    if trbBas != "" :
                        sep = "|"

                    base = pld.getNumber("turbBaseHeight")
                    if a2dafcommon.is_no_data(base) or base == 99999 :
                        base = "1e37"
                    else :
                        base = "%.0f"%float(base)

                    top = pld.getNumber("turbTopHeight")
                    if a2dafcommon.is_no_data(top) or top == 99999 :
                        top = "1e37"
                    else :
                        top = "%.0f"%float(top)

                    trbBas += sep+base
                    trbTop += sep+top
                    trbTyp += sep+dtyp
                    trbVal += sep+fval
                    tt += 1

        msg += mylat + "|" + mylon + "," + mytime + "," + myflvl + ",PIREP," + \
            mycraft + "," + mytemp + "," + mydir + "," + myspd + "," + \
            myvis + ",-1,-1,-1," + mywx + "," + \
            str(cc) + "," + cldBas + "," + cldTop + "," + cldVal + "," + \
            str(ii) + "," + icgBas + "," + icgTop + "," + \
            icgTyp + "," + icgVal + "," + \
            str(tt) + "," + trbBas + "," + trbTop + "," + \
            trbTyp + "," + trbVal + "\n"

    print msg.strip()

if __name__ == '__main__':
    main()
