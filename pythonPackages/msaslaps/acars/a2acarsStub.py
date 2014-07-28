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
# Gets all available acars data in the A-II database over a specified range of
# times. The data is output to stdout as ASCII.  Each line is one time/platform
# combination.  The individual data items are comma delimited.
#
#
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/23/2014       3392           nabowle      Initial modification. Replaces UEngine with DAF.
#    07/28/2014       3392           nabowle      Strip tail and receiver to match original formatting.
#
#


import argparse
import sys

from datetime import datetime
from ufpy.dataaccess import DataAccessLayer
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
# Values used to indicate no data.
    NO_DATA = [None, "None", -9999, -9999.0, "-9999", "-9999.0", ""]

    user_args = get_args()

    # Set the host in the DataAcessLayer if supplied
    if user_args.host:
        DataAccessLayer.changeEDEXHost(user_args.host)

    start = user_args.start
    end = user_args.end

    if not start or not end:
        print >> sys.stderr, "Start or End date not provided"
        return


    req = DataAccessLayer.newDataRequest("acars")
    req.setParameters("tailNumber", "receiver", "pressure", "flightPhase", 
                      "rollAngleQuality", "temp", "windDirection", "windSpeed",
                      "humidity", "mixingRatio", "icing")



    beginRange = datetime.strptime( start + ":00.0", "%Y-%m-%d %H:%M:%S.%f")
    endRange = datetime.strptime( end + ":59.9", "%Y-%m-%d %H:%M:%S.%f")
    timerange = TimeRange(beginRange, endRange)

    geometries = DataAccessLayer.getGeometryData(req, timerange)

    if len(geometries) == 0:
#       print("No data available.")
       return

    for geoData in geometries:
       mytail = geoData.getString("tailNumber")
       if mytail in NO_DATA:
           mytail = ""
       else:
           mytail = mytail.strip()

       mytime = geoData.getDataTime()
       if mytime == None:
           continue
       #2014-07-16 00:00:00 (0) => 2014-07-16_00:00:00
       mytime = str(mytime)[0:19].replace(" ","_")

       geo = geoData.getGeometry()
       if geo == None:
           continue
       mylon = geo.x
       mylat = geo.y
       if (mylat in NO_DATA or mylon in NO_DATA):
           continue
       mylat = "%.4f"%float(mylat)
       mylon = "%.4f"%float(mylon)

       myrec = geoData.getString("receiver")
       if myrec in NO_DATA:
          myrec = ""
       else:
          myrec = myrec.strip()

       mypres = geoData.getNumber("pressure")
       if mypres in NO_DATA:
          mypres = "1e37"
       else :
          mypres = "%.0f"%mypres

       myphs = geoData.getString("flightPhase")
       if myphs in NO_DATA:
          myphs = "7"
       else :
          myphs = "%d"%int(myphs)

       myrol = geoData.getString("rollAngleQuality")
       if myrol in NO_DATA:
          myrol = "3"
       else :
          myrol = "%d"%int(myrol)

       mytemp = geoData.getNumber("temp")
       if mytemp in NO_DATA:
          mytemp = "1e37"
       else :
          mytemp = "%.1f"%mytemp

       mydir = geoData.getString("windDirection")
       if mydir in NO_DATA:
          mydir = "1e37"
       else :
          mydir = "%d"%int(mydir)

       myspd = geoData.getNumber("windSpeed")
       if myspd in NO_DATA:
          myspd = "1e37"
       else :
          myspd = "%.1f"%myspd

       myhum = geoData.getNumber("humidity")
       if myhum in NO_DATA:
          myhum = "1e37"
       else :
          myhum = "%.0f"%myhum

       mymix = geoData.getNumber("mixingRatio")
       if mymix in NO_DATA:
          mymix = "1e37"
       else :
          mymix = "%.2f"%mymix

# Icing was commented out of the uengine version 
#       myicg = geoData.getString("icing")
#       if myicg in NO_DATA:
#          myicg = "1e37"
#       else :
#          myicg = "%d"%int(myicg)

       msg = mytail + "," + mytime + "," + mylat + "," + mylon + "," + \
          myrec + "," + mypres + "," + myphs + "," + myrol + "," + \
          mytemp + "," + mydir + "," + myspd + "," + myhum + "," + mymix
       print msg

if __name__ == '__main__':
    main()
