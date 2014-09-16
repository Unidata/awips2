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

# Gets all available Laps metar data in the A-II database over a specified range
# of times within a specifed area.  The data is output to stdout as ASCII.
# Each line is one time/station combination. The individual data items are comma
# delimited.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/15/2014      3593          nabowle        Initial modification. Fix index issues on 2D parameters.
#
#

# pointDataQuery.stationName_lat_lon.py
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
import PointDataQuery

# 1. 
pdq = PointDataQuery.PointDataQuery("obs")

# 3.  the stuff we want returned to us in PointDataContainer
reqPar = "stationName,timeObs"
reqPar += ",latitude,longitude,elevation,wmoId"
reqPar += ",autoStationType,reportType"
reqPar += ",presWeather,visibility,skyCover,skyLayerBase"
reqPar += ",altimeter,seaLevelPress,pressChange3Hour,pressChangeChar"
reqPar += ",temperature,tempFromTenths,dewpoint,dpFromTenths"
reqPar += ",windDir,windSpeed,windGust,maxTemp24Hour,minTemp24Hour"
reqPar += ",precip1Hour,precip3Hour,precip6Hour,precip24Hour"
pdq.setRequestedParameters(reqPar)

# 2.  some constraints
pdq.addConstraint("dataTime","BBBBB:00.0",">=")
pdq.addConstraint("dataTime","EEEEE:00.0","<=")
pdq.addConstraint("location.longitude","LNMN",">=")
pdq.addConstraint("location.longitude","LNMX","<=")
pdq.addConstraint("location.latitude","LTMN",">=")
pdq.addConstraint("location.latitude","LTMX","<=")

# 5.1  execute() returns a ResponseMessageGeneric
pdq.requestAllLevels()
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
repTyp = pdc.getPointDataTypes().get("reportType").getStringData()
wx = pdc.getPointDataTypes().get("presWeather").getStringData()
vis = pdc.getPointDataTypes().get("visibility").getFloatData()
cvr = pdc.getPointDataTypes().get("skyCover").getStringData()
bas = pdc.getPointDataTypes().get("skyLayerBase").getFloatData()
alt = pdc.getPointDataTypes().get("altimeter").getFloatData()
msl = pdc.getPointDataTypes().get("seaLevelPress").getFloatData()
pchg = pdc.getPointDataTypes().get("pressChange3Hour").getFloatData()
pchr =  pdc.getPointDataTypes().get("pressChangeChar").getStringData()
temp = pdc.getPointDataTypes().get("temperature").getFloatData()
t10 = pdc.getPointDataTypes().get("tempFromTenths").getFloatData()
dpt = pdc.getPointDataTypes().get("dewpoint").getFloatData()
td10 = pdc.getPointDataTypes().get("dpFromTenths").getFloatData()
dir = pdc.getPointDataTypes().get("windDir").getFloatData()
spd = pdc.getPointDataTypes().get("windSpeed").getFloatData()
gust = pdc.getPointDataTypes().get("windGust").getFloatData()
tmx = pdc.getPointDataTypes().get("maxTemp24Hour").getFloatData()
tmn = pdc.getPointDataTypes().get("minTemp24Hour").getFloatData()
pr1 = pdc.getPointDataTypes().get("precip1Hour").getFloatData()
pr3 = pdc.getPointDataTypes().get("precip3Hour").getFloatData()
pr6 = pdc.getPointDataTypes().get("precip6Hour").getFloatData()
pr24 = pdc.getPointDataTypes().get("precip24Hour").getFloatData()

# 5.2 and 5.3
if len(tobs) == 0 :
   msg = "couldn't get data"
   return ResponseMessageGeneric(msg)

msg = "\n\n"
i = i5 = i6 = 0
while i < len(tobs) :
    msg += sName[i] + ","
    msg += str(tobs[i]/1000) + ","
    msg += "%.4f"%lat[i] + ","
    msg += "%.4f"%lon[i] + ","
    msg += "%.0f"%elev[i] + ","
    if ista[i] < 0 :
        msg += "-99,"
    else :
        msg += str(ista[i]) + ","
    msg += atype[i] + " ,"
    msg += repTyp[i] + " ,"
    msg += wx[i5] + " ,"
    msg += "%.3f"%vis[i] + ","

    
    msg += cvr[i6];
    kk = 5
    while kk > 0 and cvr[i6+kk] == "" :
        kk -= 1
    k = 1
    while k <= kk :
        msg += "|" + cvr[i6+k];
        k += 1
    msg += " ,"
    msg += "%.1f"%bas[i6];
    kk = 5
    while kk > 0 and bas[i6+kk] < -9998 :
        kk -= 1
    k = 1
    while k <= kk :
        msg += "|" + "%.1f"%bas[i6+k];
        k += 1
    msg += ","

    msg += "%.2f"%alt[i] + ","
    msg += "%.2f"%msl[i] + ","
    msg += "%.0f"%pchg[i] + ","
    msg += pchr[i] + " ,"
    msg += "%.1f"%temp[i] + ","
    msg += "%.1f"%t10[i] + ","
    msg += "%.1f"%dpt[i] + ","
    msg += "%.1f"%td10[i] + ","
    msg += "%.0f"%dir[i] + ","
    msg += "%.1f"%spd[i] + ","
    msg += "%.1f"%gust[i] + ","
    msg += "%.1f"%tmx[i] + ","
    msg += "%.1f"%tmn[i] + ","
    msg += "%.2f"%pr1[i] + ","
    msg += "%.2f"%pr3[i] + ","
    msg += "%.2f"%pr6[i] + ","
    msg += "%.2f"%pr24[i] + "\n"
    i += 1
    i5 += 5
    i6 += 6;

return ResponseMessageGeneric(msg)

