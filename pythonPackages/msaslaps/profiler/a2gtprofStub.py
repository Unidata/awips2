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

# Gets all available profiler data in the A-II database over a specified range
# of times.  The data is output to stdout as ASCII. Each line is one
# time/station combination. The individual data variables are comma delimited,
# and when what is returned for a data item is a profile, each item in the
# profile is vertical bar delimited
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    Oct 6, 2014     3594          nabowle        Initial modification. Replace UEngine with DAF.
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
    # The multi-dimensional parameters.
    MULTI_DIM_PARAMS = set(['vComponent', 'uComponent', 'peakPower', 
                            'levelMode', 'uvQualityCode', 'consensusNum', 
                            'HorizSpStdDev', 'wComponent', 'height', 
                            'VertSpStdDev'])

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

    req = DataAccessLayer.newDataRequest("profiler")
    req.setParameters('numProfLvls', 'elevation', 'windDirSfc', 'validTime', 
                      'windSpeedSfc', 'pressure', 'submode', 'relHumidity', 
                      'profilerId', 'rainRate', 'temperature')
    req.getParameters().extend(MULTI_DIM_PARAMS)

    geometries = DataAccessLayer.getGeometryData(req, timerange)

    if not geometries :
#        print "couldn't get data"
        return

    
    subgeos = []
    msg = ""
    for geoData in geometries :
        if set(geoData.getParameters()) & MULTI_DIM_PARAMS :
            subgeos.append(geoData)
            continue

        elev = geoData.getNumber("elevation")
        msg += geoData.getString("profilerId") + ","
        msg += str(geoData.getNumber("validTime")/1000) + ","
        msg += "%.4f"%geoData.getGeometry().y + ","
        msg += "%.4f"%geoData.getGeometry().x + ","
        msg += "%.0f"%elev + ","
        msg += "%.1f"%geoData.getNumber("pressure") + ","
        msg += "%.1f"%geoData.getNumber("temperature") + ","
        msg += "%.1f"%geoData.getNumber("relHumidity") + ","
        msg += "%.0f"%geoData.getNumber("windDirSfc") + ","
        msg += "%.1f"%geoData.getNumber("windSpeedSfc") + ","
        msg += "%.1f"%geoData.getNumber("rainRate") + ","
        msg += str(geoData.getNumber("submode")) + ","

        kk = len(subgeos)
#        msg += str(kk) + ","

        subgeos[0].getString("consensusNum")
        if kk>0 : msg += "%.0f"%(subgeos[0].getNumber("height")-elev)
        k = 1
        while k < kk :
            msg += "|" + "%.0f"%(subgeos[k].getNumber("height")-elev)
            k += 1
        msg += ","
        if kk>0 : msg += str(subgeos[0].getNumber("levelMode"))
        k = 1
        while k < kk :
            msg += "|" + str(subgeos[k].getNumber("levelMode"))
            k += 1
        msg += ","

        if kk>0 : msg += "%.1f"%subgeos[0].getNumber("uComponent")
        k = 1
        while k < kk :
            msg += "|" + "%.1f"%subgeos[k].getNumber("uComponent")
            k += 1
        msg += ","
        if kk>0 : msg += "%.1f"%subgeos[0].getNumber("vComponent")
        k = 1
        while k < kk :
            msg += "|" + "%.1f"%subgeos[k].getNumber("vComponent")
            k += 1
        msg += ","
        if kk>0 : msg += "%.2f"%subgeos[0].getNumber("wComponent")
        k = 1
        while k < kk :
            msg += "|" + "%.2f"%subgeos[k].getNumber("wComponent")
            k += 1
        msg += ","

        if kk>0 : msg += "%.1f"%subgeos[0].getNumber("peakPower")
        k = 1
        while k < kk :
            msg += "|" + "%.1f"%subgeos[k].getNumber("peakPower")
            k += 1
        msg += ","
        if kk>0 : msg += "%.1f"%subgeos[0].getNumber("HorizSpStdDev")
        k = 1
        while k < kk :
            msg += "|" + "%.1f"%subgeos[k].getNumber("HorizSpStdDev")
            k += 1
        msg += ","
        if kk>0 : msg += "%.1f"%subgeos[0].getNumber("VertSpStdDev")
        k = 1
        while k < kk :
            msg += "|" + "%.1f"%subgeos[k].getNumber("VertSpStdDev")
            k += 1
        msg += ","

        if kk>0 : msg += subgeos[0].getString("uvQualityCode")
        k = 1
        while k < kk :
            msg += "|" + subgeos[k].getString("uvQualityCode")
            k += 1
        msg += ","
        if kk>0 : msg += subgeos[0].getString("consensusNum")
        k = 1
        while k < kk :
            msg += "|" + subgeos[k].getString("consensusNum")
            k += 1
        msg += "\n"
        subgeos = []

    print msg.strip()

if __name__ == '__main__':
    main()
