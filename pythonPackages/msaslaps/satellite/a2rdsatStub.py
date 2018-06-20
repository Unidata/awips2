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

# Gets data for a single satellite sector from the A-II database. The result is
# output to stdout as ASCII. The first line returned has the dimensions of the
# image, the time, and the source satellite of the data set returned. The rest
# is one line per row of satellite data.  The data for each row undergoes second
# order compression Each pixel value of 0 or 255 is encoded as @ or #,
# respectively. Otherwise the first pixel on the row and any pixel that is more
# than 20 counts  different than the previous one is encoded as two hex digits.
# Pixels the same as the previous are encoded as a period, pixels from 1 to 20
# counts less than the previous are encoded as G through Z, and pixels from 1 to
# 20 counts more than the previous are encoded as g through z. There are no
# delimeters between the encoding for each pixel.
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    Sep 29, 2014    3596          nabowle        Initial modification. Replace UEngine with DAF.
#    Nov 10, 2016    5900          bsteffen       Correct grid shape, simplify

#
#

import a2dafcommon
import argparse
import sys
import numpy

from datetime import datetime
from datetime import timedelta
from awips.dataaccess import DataAccessLayer
from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange

def get_args():
    parser = argparse.ArgumentParser(conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="host",
                    help="EDEX server hostname (optional)",
                    metavar="hostname")
    parser.add_argument("--datetime", action="store", dest="datetime", 
                    help="The start of the time range in YYYY-MM-DD HH:MM",
                    metavar="datetime")
    parser.add_argument("--slop", action="store", dest="slop", default=180,
                    help="The amount of slop, in seconds, to allow around the datetime.",
                    metavar="slop",  type=int)
    parser.add_argument("--sector", action="store", dest="sector", 
                    help="The sector ID.", metavar="sectorID")
    parser.add_argument("--physical", action="store", dest="physical", 
                    help="The physical element.", metavar="physicalElement")
    parser.add_argument("--entity", action="store", dest="entity", 
                    help="The creating entity (optional)",
                    metavar="creatingEntity")
    parser.add_argument("--partition", action="store", dest="partition", 
                    help="Upper case letter indicating partition to get.",
                    metavar="partition", default="0")
    parser.add_argument("--hex",  action='store_const', dest="encoding",
                    const=1, help="Hex encoding.", metavar="encoding")
    parser.add_argument("--int",  action='store_const', dest="encoding",
                    const=0, help="Delimited integer encoding.",
                    metavar="encoding")
    return parser.parse_args()


def main():
    user_args = get_args()

    if user_args.host:
        DataAccessLayer.changeEDEXHost(user_args.host)

    slop = user_args.slop

    dateTimeStr = user_args.datetime
    if not dateTimeStr:
        print >> sys.stderr, "DateTime not provided"
        return

    physicalElement = user_args.physical
    if not physicalElement:
        print >> sys.stderr, "PhysicalElement not provided"
        return

    sectorID = user_args.sector
    if not sectorID:
        print >> sys.stderr, "SectorID not provided"
        return

    creatingEntity = user_args.entity
    part = user_args.partition
    encoding = user_args.encoding

    dateTime = datetime.strptime(dateTimeStr, "%Y-%m-%d %H:%M")
    beginRange = dateTime - timedelta(0, slop)
    endRange = dateTime + timedelta(0, slop)

    timerange = TimeRange(beginRange, endRange)

    req = DataAccessLayer.newDataRequest("satellite")
    req.setParameters(physicalElement)
    req.setLocationNames(sectorID)

    if creatingEntity:
        req.addIdentifier("creatingEntity", creatingEntity)

    grids = DataAccessLayer.getGridData(req, timerange)

    if not grids:
#        print "Data not available"
        return

    grid = grids[0]
    data = grid.getRawData()
    myent = grid.getAttribute("creatingEntity")
    mytime = a2dafcommon.datatime_to_string(grid.getDataTime()) + ".0"

    if data is None or len(data) == 0:
#        print "No data."
        return

    data[numpy.isnan(data)] = 0
    yLen, xLen = data.shape

    plus  = " ghijklmnopqrstuvwxyz"
    minus = " GHIJKLMNOPQRSTUVWXYZ"
    limit = 10000000
    if encoding == 1 :
       limit = limit/2
    elif  encoding == 0 :
       limit = limit/8

    k = xLen * ( yLen / 4 )
    j = 0
    nxy = yLen*xLen
    if part=="D" :
       j = k+k+k
    elif part=="C" :
       j = k+k
       nxy = j+k
    elif part=="B" :
       j = k
       nxy = j+k
    elif part=="A" or nxy>limit :
       nxy = k

    msg = ""
    if part<="A" :
       msg += str(xLen) + " " + str(yLen) + " "
       msg += mytime + " " + myent + "\n"

    i = 0
    kk = None
    while j<yLen :
        i=0
        kk = int(data[j, i])
        if kk<0 : kk += 256
        if encoding == 0 :
            msg += str(kk)
        elif encoding == 1 :
            msg += "%2.2x"%kk
        elif kk == 0 :
            msg += "@"
        elif kk == 255 :
            msg += "#"
        else :
            msg += "%2.2x"%kk
        i+=1
        while i<xLen :
            k = int(data[j, i])
            if k<0 : k += 256
            if encoding == 0 :
              msg += " "+str(k)
            elif encoding == 1 :
              msg += "%2.2x"%k
            elif k==0 :
              msg += "@"
            elif k == 255 :
              msg += "#"
            elif k==kk :
              msg += "."
            elif k>kk+20 or k<kk-20 :
              msg += "%2.2x"%k
            elif k>kk :
              msg += plus[k-kk]
            else :
              msg += minus[kk-k]
            kk = k
            i+=1

        msg += "\n"
        j+= 1

    print msg.strip()

if __name__ == '__main__':
    main()
