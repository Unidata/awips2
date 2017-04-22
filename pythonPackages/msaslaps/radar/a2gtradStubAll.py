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
#
#

import argparse
import a2radcommon
import sys

def get_args():
    parser = a2radcommon.get_args_parser()
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

    print_record(records[0], description)


def print_record(record, description):

    idra = record.getHdf5Data()

    rdat,azdat,depVals,prodVals,recVals,stormVals,symVals,symData,threshVals = get_hdf5_data(idra)
    
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

    msg = get_header(record, xLen, yLen, azdat, description)
    msg += encode_dep_vals(depVals)
    msg += encode_prod_vals(prodVals)
    msg += encode_rec_vals(recVals)
    msg += encode_storm_vals(stormVals)
    msg += encode_sym_vals(symVals)
    msg += encode_sym_data(symData)
    msg += encode_thresh_vals(threshVals)
    
    if azdat :
        msg += a2radcommon.encode_radial(azVals)

    msg += encode_data(yLen, xLen, array)

    print msg


def get_hdf5_data(idra):
    rdat = []
    azdat = []
    depVals = []
    prodVals = []
    recVals = []
    stormVals = []
    symVals = []
    symData = []
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
            elif idra[ii].getName() == "ProductVals":
                prodVals = idra[ii].getByteData()
            elif idra[ii].getName() == "RecordVals":
                recVals = idra[ii].getByteData()
            elif idra[ii].getName() == "StormIds":
                stormVals = idra[ii].getByteData()
            elif idra[ii].getName() == "Symbology":
                symVals = idra[ii].getByteData()
            elif idra[ii].getName() == "SymbologyData":
                symData = idra[ii].getByteData()
            elif idra[ii].getName() == "Thresholds":
                threshVals = idra[ii].getShortData()

    return rdat,azdat,depVals,prodVals,recVals,stormVals,symVals,symData,threshVals


def get_header(record, xLen, yLen, azdat, description):
    # Encode dimensions, time, mapping, description, tilt, and VCP
    mytime = a2radcommon.get_datetime_str(record) 
    dattyp = a2radcommon.get_data_type(azdat)    

    msg = str(xLen) + " " + str(yLen) + " " + mytime + " " + dattyp + \
          " " + description + "\n" + \
          str(record.getTrueElevationAngle()) + " " + \
          str(record.getVolumeCoveragePattern()) + "\n"
    return msg


def encode_dep_vals(depVals):
   nnn = len(depVals)
   msg = str(nnn)
   j = 0
   while j<nnn :
      if depVals[j]<0 :
         msg += " " + "%4.4X"%(65536+depVals[j])
      else :
         msg += " " + "%4.4X"%depVals[j]
      j += 1
   msg += "\n"
   return msg


def encode_prod_vals(prodVals):
   nnn = len(prodVals)
   msg = str(nnn)
   j = 0
   while j<nnn :
      if prodVals[j]<0 :
         msg += " " + "%2.2X"%(255+prodVals[j])
      else :
         msg += " " + "%2.2X"%prodVals[j]
      j += 1
   msg += "\n"
   return msg


def encode_rec_vals(recVals):
   nnn = len(recVals)
   msg = str(nnn)
   j = 0
   while j<nnn :
      if recVals[j]<0 :
         msg += " " + "%2.2X"%(255+recVals[j])
      else :
         msg += " " + "%2.2X"%recVals[j]
      j += 1
   msg += "\n"
   return msg


def encode_storm_vals(stormVals):
   nnn = len(stormVals)
   msg = str(nnn)
   j = 0
   while j<nnn :
      if stormVals[j]<0 :
         msg += " " + "%2.2X"%(255+stormVals[j])
      else :
         msg += " " + "%2.2X"%stormVals[j]
      j += 1
   msg += "\n"
   return msg


def encode_sym_vals(symVals):
   nnn = len(symVals)
   msg = str(nnn)
   j = 0
   while j<nnn :
      if symVals[j]<0 :
         msg += " " + "%2.2X"%(255+symVals[j])
      else :
         msg += " " + "%2.2X"%symVals[j]
      j += 1
   msg += "\n"
   return msg


def encode_sym_data(symData):
   nnn = len(symData)
   msg = str(nnn)
   j = 0
   while j<nnn :
      if symData[j]<0 :
         msg += " " + "%2.2X"%(255+symData[j])
      else :
         msg += " " + "%2.2X"%symData[j]
      j += 1
   msg += "\n"
   return msg


def encode_thresh_vals(threshVals):
    msg = str(len(threshVals)) + a2radcommon.encode_thresh_vals(threshVals)
    return msg


def encode_data(yLen, xLen, array):
    msg = ""
    nxy = yLen*xLen
    j = 0
    while j<nxy :
        i = 0
        while i<xLen :
            if array[i+j]<0 :
                msg += str(256+array[i+j]) + " "
            else :
                msg += str(array[i+j]) + " "
            i += 1
        msg += "\n"
        j += xLen
    return msg[0:-1]


if __name__ == '__main__':
    main()
