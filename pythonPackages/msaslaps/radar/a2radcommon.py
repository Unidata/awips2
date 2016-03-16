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
# Common methods for the a2gtrad and a2advrad scripts.
#
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/13/2014      3393          nabowle        Initial creation to contain common
#                                                 code for a2*radStub scripts.
#
#

import argparse
import sys

from datetime import datetime
from datetime import timedelta
from awips import ThriftClient

from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.level import Level
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.radar.request import GetRadarDataRecordRequest

def get_default_host():
    from awips.dataaccess import DataAccessLayer
    return DataAccessLayer.THRIFT_HOST


def get_args_parser():
    parser = argparse.ArgumentParser(conflict_handler="resolve")
    parser.add_argument("--host", action="store", dest="host",
                        help="EDEX server hostname",
                        metavar="hostname")
    parser.add_argument("--datetime", action="store", dest="datetime", 
                    help="The start of the time range in YYYY-MM-DD HH:MM",
                    metavar="datetime")
    parser.add_argument("--radar", action="store", dest="radar", 
                    help="The ICAO code for the radar",
                    metavar="radar")
    parser.add_argument("--code", action="store", dest="code", 
                    help="The product code.", type=int,
                    metavar="code")
    parser.add_argument("--angle", action="store", dest="angle", default=0,
                    help="The Elevation Angle", metavar="angle")
    parser.add_argument("--description", action="store", dest="description", 
                    help="The description.",
                    metavar="desc")
    parser.add_argument("--slop", action="store", dest="slop", default=60,
                    help="The amount of slop, in seconds, to allow around the datetime.",
                    metavar="slop",  type=int)
    return parser


def send_request(user_args):
    slop = user_args.slop

    dateTimeStr = user_args.datetime
    if not dateTimeStr:
        print >> sys.stderr, "DateTime not provided"
        return
    dateTime = datetime.strptime(dateTimeStr, "%Y-%m-%d %H:%M")
    beginRange = dateTime - timedelta(0, slop)
    endRange = dateTime + timedelta(0, slop)

    timerange = TimeRange(beginRange, endRange)

    radar = user_args.radar
    if not radar:
        print >> sys.stderr, "Radar code not provided"
        return

    code = user_args.code
    if not code:
        print >> sys.stderr, "Product code not provided"
        return

    angle = user_args.angle

    slop = int(user_args.slop)

    host = user_args.host
    if not host:
        host = get_default_host()

    client = ThriftClient.ThriftClient(host)

    # Perform a GetRadarHDF5Request
    req = GetRadarDataRecordRequest()
    req.setRadarId(radar)
    req.setPrimaryElevationAngle(float(angle))
    req.setTimeRange(timerange)
    req.setProductCode(int(code))

    response = client.sendRequest(req)

    if response is None:
        # print "Data not available"
        return

    records = response.getData()
    return records


def get_datetime_str(record):
    #2014-07-16 00:00:00 (0) => 2014-07-16_00:03:00.0
    return str(record.getDataTime())[0:19].replace(" ","_") + ".0"


def get_data_type(azdat):
    if azdat:
        dattyp = "radial"
    else :
        dattyp = "raster"
    return dattyp


def get_hdf5_data(idra):
    rdat = []
    azdat = []
    depVals = []
    threshVals = []
    if len(idra) > 0:
        for ii in range(len(idra)):
           if idra[ii].getName() == "Data":
              rdat = idra[ii]
           elif idra[ii].getName() == "Angles":
              azdat = idra[ii]
              dattyp = "radial"
           elif idra[ii].getName() == "DependentValues":
              depVals = idra[ii].getShortData()
## Commented out from the original. May not be available.
#          elif idra[ii].getName() == "ProductVals":
#             prodVals = idra[ii].getByteData()
#          elif idra[ii].getName() == "RecordVals":
#             recVals = idra[ii].getByteData()
#           elif idra[ii].getName() == "StormIds":
#             stormVals = idra[ii].getByteData()
#          elif idra[ii].getName() == "Symbology":
#             symVals = idra[ii].getByteData()
#          elif idra[ii].getName() == "SymbologyData":
#             symData = idra[ii].getByteData()
##
           elif idra[ii].getName() == "Thresholds":
              threshVals = idra[ii].getShortData()

    return rdat,azdat,depVals,threshVals


def get_header(record, format, xLen, yLen, azdat, description):
    # Encode dimensions, time, mapping, description, tilt, and VCP
    mytime = get_datetime_str(record) 
    dattyp = get_data_type(azdat)

    if format :
        msg = str(xLen) + " " + str(yLen) + " " + mytime + " " + \
            dattyp + " " + str(record.getLatitude()) + " " +  \
            str(record.getLongitude()) + " " +  \
            str(record.getElevation()) + " " +  \
            str(record.getElevationNumber()) + " " +  \
            description + " " + str(record.getTrueElevationAngle()) + " " + \
            str(record.getVolumeCoveragePattern()) + "\n"
#"%.1f"%
    else :
        msg = str(xLen) + " " + str(yLen) + " " + mytime + " " + \
            dattyp + " " + description + " " + \
            str(record.getTrueElevationAngle()) + " " + \
            str(record.getVolumeCoveragePattern()) + "\n" 

    return msg


def encode_thresh_vals(threshVals):
    spec = [".", "TH", "ND", "RF", "BI", "GC", "IC", "GR", "WS", "DS",
            "RA", "HR", "BD", "HA", "UK"]
    nnn = len(threshVals)
    j = 0
    msg = ""
    while j<nnn :
        lo = threshVals[j] % 256
        hi = threshVals[j] / 256
        msg += " "
        j += 1
        if hi < 0 :
            if lo > 14 :
                msg += "."
            else :
                msg += spec[lo]
            continue
        if hi % 16 >= 8 :
            msg += ">"
        elif hi % 8 >= 4 :
            msg += "<"
        if hi % 4 >= 2 :
            msg += "+"
        elif hi % 2 >= 1 :
            msg += "-"
        if hi >= 64 :
            msg += "%.2f"%(lo*0.01)
        elif hi % 64 >= 32 :
            msg += "%.2f"%(lo*0.05)
        elif hi % 32 >= 16 :
            msg += "%.1f"%(lo*0.1)
        else :
            msg += str(lo)
    msg += "\n"
    return msg


def encode_dep_vals(depVals): 
    nnn = len(depVals)
    j = 0
    msg = ""
    while j<nnn :
        msg += " " + str(depVals[j])
        j += 1
    msg += "\n" 
    return msg


def encode_radial(azVals):
    azValsLen = len(azVals)
    j = 0
    msg = ""
    while j<azValsLen :
        msg += "%.1f"%azVals[j] + " "
        j += 1
    msg += "\n" 
    return msg

