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
#    2014-10-14      3598          nabowle        Initial modification. Changed to use DataAccessLayer.
#

import a2rdmdlCommon
import argparse
import numpy
import sys

def get_args():
    parser = a2rdmdlCommon.get_parser()
    parser.add_argument("--dimLine", action="store_true", dest="dimLine", 
                    help="Output dimensions", default=False)
    return parser.parse_args()

def main():
    user_args = get_args()

    try:
        grid, xLen, yLen = a2rdmdlCommon.do_request(user_args)
    except Exception as e:
        print >> sys.stderr, str(e)
        return
 
    rawData = grid.getRawData()

    msg = ""
    if user_args.dimLine:
        msg += str(xLen) + " " + str(yLen) + "\n"

    nxy = yLen*xLen
    j = nxy-xLen
    while j>=0 :
        dataIdx, arrIdx = a2rdmdlCommon.get_indices(j, yLen)
        arr = rawData[dataIdx]
        i = 0
        while i<xLen:
            if arrIdx >= yLen:
                arrIdx = 0
                dataIdx += 1
                arr = rawData[dataIdx]

            k = arr[arrIdx]
            if numpy.isnan(k) :
                k = 0
            if k<0 :
                a = -k
            else :
                a = k
            if a>=999998 :
                msg += "1e37 "
            elif a<0.00005 :
                msg += "%g"%k + " "
            elif a<0.0009 :
                if round(k,8) == round(k,4) :
                    msg += "%.4f"%k + " "
                elif round(k,8) == round(k,5) :
                    msg += "%.5f"%k + " "
                elif round(k,8) == round(k,6) :
                    msg += "%.6f"%k + " "
                elif round(k,8) == round(k,7) :
                    msg += "%.7f"%k + " "
                else :
                    msg += "%.8f"%k + " "
            elif a<0.009 :
                if round(k,7) == round(k,3) :
                    msg += "%.3f"%k + " "
                elif round(k,7) == round(k,4) :
                    msg += "%.4f"%k + " "
                elif round(k,7) == round(k,5) :
                    msg += "%.5f"%k + " "
                elif round(k,7) == round(k,6) :
                    msg += "%.6f"%k + " "
                else :
                    msg += "%.7f"%k + " "
            elif a<0.09 :
                if round(k,6) == round(k,2) :
                    msg += "%.2f"%k + " "
                elif round(k,6) == round(k,3) :
                    msg += "%.3f"%k + " "
                elif round(k,6) == round(k,4) :
                    msg += "%.4f"%k + " "
                elif round(k,6) == round(k,5) :
                    msg += "%.5f"%k + " "
                else :
                    msg += "%.6f"%k + " "
            elif a<0.9 :
                if round(k,5) == round(k,1) :
                    msg += "%.1f"%k + " "
                elif round(k,5) == round(k,2) :
                    msg += "%.2f"%k + " "
                elif round(k,5) == round(k,3) :
                    msg += "%.3f"%k + " "
                elif round(k,5) == round(k,4) :
                    msg += "%.4f"%k + " "
                else :
                    msg += "%.5f"%k + " "
            elif a<9 :
                if round(k,4) == round(k,0) :
                    msg += "%.0f"%k + " "
                elif round(k,4) == round(k,1) :
                    msg += "%.1f"%k + " "
                elif round(k,4) == round(k,2) :
                    msg += "%.2f"%k + " "
                elif round(k,4) == round(k,3) :
                    msg += "%.3f"%k + " "
                else :
                    msg += "%.4f"%k + " "
            elif a<99 :
                if round(k,3) == round(k,0) :
                    msg += "%.0f"%k + " "
                elif round(k,3) == round(k,1) :
                    msg += "%.1f"%k + " "
                elif round(k,3) == round(k,2) :
                    msg += "%.2f"%k + " "
                else :
                    msg += "%.3f"%k + " "
            elif a<999 :
                if round(k,2) == round(k,0) :
                    msg += "%.0f"%k + " "
                elif round(k,2) == round(k,1) :
                    msg += "%.1f"%k + " "
                else :
                    msg += "%.2f"%k + " "
            elif a<9999 :
                if round(k,1) == round(k,0) :
                    msg += "%.0f"%k + " "
                else :
                    msg += "%.1f"%k + " "
            else :
                msg += "%.0f"%k + " "
            i += 1
            arrIdx += 1

        msg += "\n"
        j -= xLen

    print msg.strip() + " "

if __name__ == '__main__':
    main()
