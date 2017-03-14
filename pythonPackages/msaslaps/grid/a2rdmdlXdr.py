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
#    2014-10-15      3598          nabowle        Initial modification. Changed to use DataAccessLayer.
#    2016-11-10      5900          bsteffen       Correct grid shape, simplify
#

import a2rdmdlCommon
import argparse
import numpy
import xdrlib
import bz2
import sys

def get_args():
    return a2rdmdlCommon.get_parser().parse_args()

def main():
    user_args = get_args()

    try:
        grid, xLen, yLen = a2rdmdlCommon.do_request(user_args)
    except Exception as e:
        print >> sys.stderr, str(e)
        return

    rawData = grid.getRawData()

    msg = str(xLen) + " " + str(yLen) + "\n"

    j = yLen - 1
    mypacker = xdrlib.Packer()
    mypacker.reset()
    while j>=0 :
        i = 0
        while i<xLen:

            k = rawData[j,i]
            if numpy.isnan(k) :
                k = 0

            mypacker.pack_float(float(k))
            i += 1

        j -= 1

    packLen = len(mypacker.get_buffer())
    xdrbuf = bz2.compress(mypacker.get_buffer())
    cmpLen = len(xdrbuf)
    msg += str(packLen)+" "+str(cmpLen*2)+"\t\n"
    i = 0
    while i<cmpLen :
       msg += "%2.2x"%ord(xdrbuf[i])
       i += 1
    msg += "\t\n"

    print msg.strip() + "\t"

if __name__ == '__main__':
    main()
