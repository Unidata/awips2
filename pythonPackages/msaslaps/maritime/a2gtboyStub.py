# pointDataQuery.stationName_lat_lon.py
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
import PointDataQuery

# 1. 
pdq = PointDataQuery.PointDataQuery("sfcobs")

# 3.  the stuff we want returned to us in PointDataContainer
reqPar = "stationId,timeObs"
reqPar += ",latitude,longitude,elevation,seaLevelPress,stationPress"
reqPar += ",temperature,dewpoint,windDir,windSpeed,pressChange3Hour"
pdq.setRequestedParameters(reqPar)

# 2.  some constraints
pdq.addConstraint("dataTime","BBBBB:00.0",">=")
pdq.addConstraint("dataTime","EEEEE:00.0","<=")

# 5.1  execute() returns a ResponseMessageGeneric
rmg = pdq.execute()

# 5.1, cont'd.  RMG's payload is a PointDataContainer
pdc = rmg.getContents()
#return ResponseMessageGeneric(pdc)

# Get the data for each requested parameter.
sName = pdc.getPointDataTypes().get("stationId").getStringData()
tobs = pdc.getPointDataTypes().get("timeObs").getLongData()
lat = pdc.getPointDataTypes().get("latitude").getFloatData()
lon = pdc.getPointDataTypes().get("longitude").getFloatData()
elev = pdc.getPointDataTypes().get("elevation").getFloatData()
msl = pdc.getPointDataTypes().get("seaLevelPress").getFloatData()
p = pdc.getPointDataTypes().get("stationPress").getFloatData()
temp = pdc.getPointDataTypes().get("temperature").getFloatData()
dpt = pdc.getPointDataTypes().get("dewpoint").getFloatData()
dir = pdc.getPointDataTypes().get("windDir").getFloatData()
spd = pdc.getPointDataTypes().get("windSpeed").getFloatData()
pchg = pdc.getPointDataTypes().get("pressChange3Hour").getFloatData()

# 5.2 and 5.3
if len(tobs) == 0 :
   msg = "couldn't get data"
   return ResponseMessageGeneric(msg)

msg = "\n"
i = 0
while i < len(tobs) :
    msg += sName[i] + ","
    msg += str(tobs[i]/1000) + ","
    msg += "%.4f"%lat[i] + ","
    msg += "%.4f"%lon[i] + ","
    msg += "%.0f"%elev[i] + ","
    msg += "%.2f"%msl[i] + ","
    msg += "%.2f"%p[i] + ","
    msg += "%.1f"%temp[i] + ","
    msg += "%.1f"%dpt[i] + ","
    msg += "%.0f"%dir[i] + ","
    msg += "%.1f"%spd[i] + ","
    msg += "%.0f"%pchg[i] + "\n"
    i += 1

return ResponseMessageGeneric(msg)

