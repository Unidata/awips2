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

# Gets all available metar data in the A-II database over a specified range of
# times within a specifed area.  The data is output to stdout as ASCII.
# Each line is one time/station combination. The individual data items are comma
# delimited.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/15/2014      3593          nabowle        Initial modification. Fix losing first record.
#
#

# pointDataQuery.stationName_lat_lon.py
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
import PointDataQuery

# 1. 
pdq = PointDataQuery.PointDataQuery("obs")

# 3.  the stuff we want returned to us in PointDataContainer
reqPar = "stationName,timeObs"
reqPar += ",latitude,longitude,elevation,wmoId,autoStationType"
reqPar += ",seaLevelPress,temperature,dewpoint,windDir,windSpeed,altimeter"
pdq.setRequestedParameters(reqPar)

# 2.  some constraints
pdq.addConstraint("dataTime","BBBBB:00.0",">=")
pdq.addConstraint("dataTime","EEEEE:00.0","<=")
pdq.addConstraint("location.longitude","LNMN",">=")
pdq.addConstraint("location.longitude","LNMX","<=")
pdq.addConstraint("location.latitude","LTMN",">=")
pdq.addConstraint("location.latitude","LTMX","<=")

# 5.1  execute() returns a ResponseMessageGeneric
rmg = pdq.execute()

# 5.1, cont'd.  RMG's payload is a PointDataContainer
pdc = rmg.getContents()
#return ResponseMessageGeneric(pdc)

# Get the data for each requested parameter.
sName = pdc.getPointDataTypes().get("stationName").getStringData()
tobs = pdc.getPointDataTypes().get("timeObs").getLongData()
lat = pdc.getPointDataTypes().get("latitude").getFloatData()
lon = pdc.getPointDataTypes().get("longitude").getFloatData()
elev = pdc.getPointDataTypes().get("elevation").getFloatData()
ista = pdc.getPointDataTypes().get("wmoId").getIntData()
atype = pdc.getPointDataTypes().get("autoStationType").getStringData()
msl = pdc.getPointDataTypes().get("seaLevelPress").getFloatData()
temp = pdc.getPointDataTypes().get("temperature").getFloatData()
dpt = pdc.getPointDataTypes().get("dewpoint").getFloatData()
dir = pdc.getPointDataTypes().get("windDir").getFloatData()
spd = pdc.getPointDataTypes().get("windSpeed").getFloatData()
alt = pdc.getPointDataTypes().get("altimeter").getFloatData()

# 5.2 and 5.3
if len(tobs) == 0 :
   msg = "couldn't get data"
   return ResponseMessageGeneric(msg)

msg = "\n\n"
i = 0
while i < len(tobs) :
    msg += sName[i] + ","
    msg += str(tobs[i]/1000) + ","
    msg += "%.4f"%lat[i] + ","
    msg += "%.4f"%lon[i] + ","
    msg += "%.0f"%elev[i] + ","
    msg += str(ista[i]) + ","
    msg += atype[i] + " ,"
    msg += "%.2f"%msl[i] + ","
    msg += "%.1f"%temp[i] + ","
    msg += "%.1f"%dpt[i] + ","
    msg += "%.0f"%dir[i] + ","
    msg += "%.1f"%spd[i] + ","
    msg += "%.2f"%alt[i] + "\n"
    i += 1

return ResponseMessageGeneric(msg)

