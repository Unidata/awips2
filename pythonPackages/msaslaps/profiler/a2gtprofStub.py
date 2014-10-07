# pointDataQuery.stationName_lat_lon.py
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
import PointDataQuery

# 1. 
pdq = PointDataQuery.PointDataQuery("profiler")

# 3.  the stuff we want returned to us in PointDataContainer
reqPar = "profilerId,validTime"
reqPar += ",latitude,longitude,elevation"
reqPar += ",pressure,temperature,relHumidity,windDirSfc,windSpeedSfc,rainRate"
reqPar += ",submode,numProfLvls"
reqPar += ",height,levelMode,uComponent,vComponent,wComponent"
reqPar += ",peakPower,HorizSpStdDev,VertSpStdDev,uvQualityCode,consensusNum"
pdq.setRequestedParameters(reqPar)

# 2.  some constraints
pdq.addConstraint("dataTime","BBBBB:00.0",">=")
pdq.addConstraint("dataTime","EEEEE:00.0","<=")

# 5.1  execute() returns a ResponseMessageGeneric
pdq.requestAllLevels()
rmg = pdq.execute()

# 5.1, cont'd.  RMG's payload is a PointDataContainer
pdc = rmg.getContents()
#return ResponseMessageGeneric(pdc)

# Get the data for each requested parameter.
stn = pdc.getPointDataTypes().get("profilerId").getStringData()
tobs = pdc.getPointDataTypes().get("validTime").getLongData()
lat = pdc.getPointDataTypes().get("latitude").getFloatData()
lon = pdc.getPointDataTypes().get("longitude").getFloatData()
elev = pdc.getPointDataTypes().get("elevation").getFloatData()
mslp = pdc.getPointDataTypes().get("pressure").getFloatData()
t0 = pdc.getPointDataTypes().get("temperature").getFloatData()
rh0 = pdc.getPointDataTypes().get("relHumidity").getFloatData()
dd0 = pdc.getPointDataTypes().get("windDirSfc").getFloatData()
ff0 = pdc.getPointDataTypes().get("windSpeedSfc").getFloatData()
rr0 = pdc.getPointDataTypes().get("rainRate").getFloatData()
subm = pdc.getPointDataTypes().get("submode").getIntData()
nz = pdc.getPointDataTypes().get("numProfLvls").getIntData()
z = pdc.getPointDataTypes().get("height").getFloatData()
lvlm = pdc.getPointDataTypes().get("levelMode").getIntData()
u = pdc.getPointDataTypes().get("uComponent").getFloatData()
v = pdc.getPointDataTypes().get("vComponent").getFloatData()
w = pdc.getPointDataTypes().get("wComponent").getFloatData()
pp = pdc.getPointDataTypes().get("peakPower").getFloatData()
uv2 = pdc.getPointDataTypes().get("HorizSpStdDev").getFloatData()
w2 = pdc.getPointDataTypes().get("VertSpStdDev").getFloatData()
qc = pdc.getPointDataTypes().get("uvQualityCode").getIntData()
cnum = pdc.getPointDataTypes().get("consensusNum").getIntData()

# 5.2 and 5.3
if len(tobs) == 0 :
   msg = "couldn't get data"
   return ResponseMessageGeneric(msg)

msg = "\n"
i = i3 = 0
while i < len(tobs) :

    msg += stn[i] + ","
    msg += str(tobs[i]/1000) + ","
    msg += "%.4f"%lat[i] + ","
    msg += "%.4f"%lon[i] + ","
    msg += "%.0f"%elev[i] + ","
    msg += "%.1f"%mslp[i] + ","
    msg += "%.1f"%t0[i] + ","
    msg += "%.1f"%rh0[i] + ","
    msg += "%.0f"%dd0[i] + ","
    msg += "%.1f"%ff0[i] + ","
    msg += "%.1f"%rr0[i] + ","
    msg += str(subm[i]) + ","

#   msg += str(nz[i]) + ","
    kk = i3 + nz[i]

    if nz[i]>0 : msg += "%.0f"%(z[i3]-elev[i])
    k = i3 + 1
    while k < kk :
        msg += "|" + "%.0f"%(z[k]-elev[i])
        k += 1
    msg += ","
    if nz[i]>0 : msg += str(lvlm[i3])
    k = i3 + 1
    while k < kk :
        msg += "|" + str(lvlm[k])
        k += 1
    msg += ","

    if nz[i]>0 : msg += "%.1f"%u[i3]
    k = i3 + 1
    while k < kk :
        msg += "|" + "%.1f"%u[k]
        k += 1
    msg += ","
    if nz[i]>0 : msg += "%.1f"%v[i3]
    k = i3 + 1
    while k < kk :
        msg += "|" + "%.1f"%v[k]
        k += 1
    msg += ","
    if nz[i]>0 : msg += "%.2f"%w[i3]
    k = i3 + 1
    while k < kk :
        msg += "|" + "%.2f"%w[k]
        k += 1
    msg += ","

    if nz[i]>0 : msg += "%.1f"%pp[i3]
    k = i3 + 1
    while k < kk :
        msg += "|" + "%.1f"%pp[k]
        k += 1
    msg += ","
    if nz[i]>0 : msg += "%.1f"%uv2[i3]
    k = i3 + 1
    while k < kk :
        msg += "|" + "%.1f"%uv2[k]
        k += 1
    msg += ","
    if nz[i]>0 : msg += "%.1f"%w2[i3]
    k = i3 + 1
    while k < kk :
        msg += "|" + "%.1f"%w2[k]
        k += 1
    msg += ","

    if nz[i]>0 : msg += str(qc[i3])
    k = i3 + 1
    while k < kk :
        msg += "|" + str(qc[k])
        k += 1
    msg += ","
    if nz[i]>0 : msg += str(cnum[i3])
    k = i3 + 1
    while k < kk :
        msg += "|" + str(cnum[k])
        k += 1
    msg += "\n"

    i += 1
    i3 += 64

return ResponseMessageGeneric(msg)

