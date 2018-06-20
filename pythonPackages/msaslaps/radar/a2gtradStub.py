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
# Gets data for a single radar product from the A-II database.  The result is
# output to stdout as ASCII.  This uses a data-specific Request/Response instead
# of the DataAccessLayer in order to preserve data-genericness of the interface.
#
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/11/2014      3393          nabowle        Initial modification. Replaces UEngine 
#                                                 with a custom Request/Response.
#    12/16/2014      3393          nabowle        Fix negative k values.
#
#

import argparse
import a2radcommon
import sys

def get_args():
    parser = a2radcommon.get_args_parser()
    parser.add_argument("--hex",  action='store_const', dest="encoding",
                    const=1, help="Hex encoding.", metavar="encoding")
    parser.add_argument("--int",  action='store_const', dest="encoding",
                    const=0, help="Delimited integer encoding.",
                    metavar="encoding")
    parser.add_argument("--extended", action='store_true', default=False,
                    dest="extended", help="Output the extended header.")
    return parser.parse_args()


def main():
    user_args = get_args()

    records = a2radcommon.send_request(user_args)

    if not records:
        # print "Data not available"
        return

    description = user_args.description
    if not description:
        print >> sys.stderr, "Description not provided"
        return

    format = user_args.extended
    encoding = user_args.encoding

    print_record(records[0], format, description, encoding)


def print_record(record, format, description, encoding):

    idra = record.getHdf5Data()

    rdat,azdat,depVals,threshVals = a2radcommon.get_hdf5_data(idra)
    
    if not rdat:
        # Graphic, XY
        # print "Unsupported radar format"
        return

    dim = rdat.getDimension()
    if dim != 2:
        # print "Data not available"
        return

    yLen = rdat.getSizes()[0]
    xLen = rdat.getSizes()[1]

    # byte[] -- the raw data
    array = rdat.getByteData()
    arraySize = len(array)
    if xLen * yLen != arraySize:
       # print "Data not available"
       return

    # get data for azimuth angles if we have them.
    if azdat :
        azVals = azdat.getFloatData()
        azValsLen = len(azVals)
        if yLen != azValsLen:
            # print "Data not available"
            return

    msg = a2radcommon.get_header(record, format, xLen, yLen, azdat, description)

    msg += a2radcommon.encode_thresh_vals(threshVals)
    msg += a2radcommon.encode_dep_vals(depVals)
    if azdat :
        msg += a2radcommon.encode_radial(azVals)
    msg += encode_data(yLen, xLen, array, encoding)

    print msg.strip()


def encode_data(yLen, xLen, array, encoding):
    plus  = " ghijklmnopqrstuvwxyz"
    minus = " GHIJKLMNOPQRSTUVWXYZ"
    nxy = yLen*xLen
    j = 0
    msg = ""
    while j<nxy :
        i = 0
        kk = array[i+j]
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
        i += 1
        while i<xLen :
            k = array[i+j]
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
            i += 1
        msg += "\n"
        j += xLen  
    return msg


if __name__ == '__main__':
    main()

