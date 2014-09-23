# pointDataQuery.stationName_lat_lon.py
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
import PointDataQuery

# 1. 
pdq = PointDataQuery.PointDataQuery("sfcobs")

# 3.  the stuff we want returned to us in PointDataContainer
reqPar = "stationId,timeObs"
reqPar += ",latitude,longitude,elevation,reportType,wx_present,visibility"
reqPar += ",seaLevelPress,stationPress,pressChange3Hour,pressChangeChar"
reqPar += ",temperature,dewpoint,seaSurfaceTemp,wetBulb"
reqPar += ",windDir,windSpeed,equivWindSpeed10m,windGust"
reqPar += ",precip1Hour,precip6Hour,precip24Hour"
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

# Initialize conversion array for wx.  Would be better to read from a
# file, but do not know how to do this through the UEngine.
wxstr = [ " ", " ", " ", " ", "FU", "HZ", "DU", "BLSA", "PO", "VCSS", \
  "BR", "BCFG", "MIFG", "VCTS", "VCSH", "VCSH", "VCSH", " ", "SQ", "+FC", \
  "DZ", "RA", "SN", "RA SN", "FZRA", "SHRA", "SHRA SHSN", "SHGR", "FG FZFG", "TS", \
  "SS", "SS", "SS", "+SS", "+SS", "+SS", "DRSN", " ", "BLSN", "+BLSN", \
  "VCFG", "BCFG", "FG FZFG", "FG FZFG", "FG FZFG", "FG FZFG", "FG FZFG", "FG FZFG", "FZFG", "FZFG", \
  "-DZ", "-DZ", "DZ", "DZ", "+DZ", "+DZ", "-FZDZ", "FZDZ", "-DZ -RA", "DZ RA", \
  "-RA", "-RA", "RA", "RA", "+RA", "+RA", "-FZRA", "FZRA", "-RA -SN", "RA SN", \
  "-SN", "-SN", "SN", "SN", "+SN", "+SN", "IC", "SG", "IC", "PE", \
  "-SHRA", "SHRA", "+SHRA", "-SHSN -SHRA", "SHSN SHRA", "-SNSN", "SHSN", "-SHPE", "SHPE", " ", \
  "SHGR", "-RA", "+RA", "-RA -SN -GR", "+RA +SN +GR", "TSRA", "TSPE", "+TSRA", " ", "+TSPE" ]

# Get the data for each requested parameter.
sName = pdc.getPointDataTypes().get("stationId").getStringData()
tobs = pdc.getPointDataTypes().get("timeObs").getLongData()
lat = pdc.getPointDataTypes().get("latitude").getFloatData()
lon = pdc.getPointDataTypes().get("longitude").getFloatData()
elev = pdc.getPointDataTypes().get("elevation").getFloatData()
typ = pdc.getPointDataTypes().get("reportType").getIntData()
wx = pdc.getPointDataTypes().get("wx_present").getIntData()
vis = pdc.getPointDataTypes().get("visibility").getIntData()
msl = pdc.getPointDataTypes().get("seaLevelPress").getFloatData()
p = pdc.getPointDataTypes().get("stationPress").getFloatData()
pchg = pdc.getPointDataTypes().get("pressChange3Hour").getFloatData()
pchr =  pdc.getPointDataTypes().get("pressChangeChar").getIntData()
temp = pdc.getPointDataTypes().get("temperature").getFloatData()
dpt = pdc.getPointDataTypes().get("dewpoint").getFloatData()
th2o = pdc.getPointDataTypes().get("seaSurfaceTemp").getFloatData()
tw = pdc.getPointDataTypes().get("wetBulb").getFloatData()
dir = pdc.getPointDataTypes().get("windDir").getFloatData()
spd = pdc.getPointDataTypes().get("windSpeed").getFloatData()
s10 = pdc.getPointDataTypes().get("equivWindSpeed10m").getFloatData()
gust = pdc.getPointDataTypes().get("windGust").getFloatData()
pr1 = pdc.getPointDataTypes().get("precip1Hour").getFloatData()
pr6 = pdc.getPointDataTypes().get("precip6Hour").getFloatData()
pr24 = pdc.getPointDataTypes().get("precip24Hour").getFloatData()

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
    if typ[i] < 1001 or typ[i] > 1007 :
        msg += "-32767,"
    elif typ[i] == 1001 or typ[i] == 1004 or typ[i] == 1005 :
        msg += "0,"
    else :
        msg += "1,"
    if wx[i] < 0 or wx[i] > 99 :
        msg += " ,"
    else :
        msg += wxstr[wx[i]] + ","
    msg += str(vis[i]) + ","
    msg += "%.2f"%msl[i] + ","
    msg += "%.2f"%p[i] + ","
    msg += "%.0f"%pchg[i] + ","
    if pchr[i] <= -9999 :
       pchr[i] = -32767
    msg += str(pchr[i]) + " ,"
    msg += "%.1f"%temp[i] + ","
    msg += "%.1f"%dpt[i] + ","
    msg += "%.1f"%th2o[i] + ","
    msg += "%.1f"%tw[i] + ","
    msg += "%.0f"%dir[i] + ","
    msg += "%.1f"%spd[i] + ","
    msg += "%.1f"%s10[i] + ","
    msg += "%.1f"%gust[i] + ","
    msg += "%.2f"%pr1[i] + ","
    msg += "%.2f"%pr6[i] + ","
    msg += "%.2f"%pr24[i] + "\n"
    i += 1

return ResponseMessageGeneric(msg)

