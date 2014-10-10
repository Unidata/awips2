# pointDataQuery.stationName_lat_lon.py
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
import PointDataQuery

# 1. 
pdq = PointDataQuery.PointDataQuery("bufrua")

# 3.  the stuff we want returned to us in PointDataContainer
reqPar = "wmoStaNum,validTime,rptType"
reqPar += ",latitude,longitude,staElev"
reqPar += ",numMand,prMan,htMan,tpMan,tdMan,wsMan,wdMan"
reqPar += ",numSigT,prSigT,tpSigT,tdSigT"
reqPar += ",numSigW,htSigW,wsSigW,wdSigW"
reqPar += ",numTrop,prTrop,tpTrop,tdTrop,wsTrop,wdTrop"
reqPar += ",numMwnd,prMaxW,wsMaxW,wdMaxW"
pdq.setRequestedParameters(reqPar)

# 2.  some constraints
pdq.addConstraint("dataTime","BBBBB:00.0",">=")
pdq.addConstraint("dataTime","EEEEE:00.0","<=")

# 5.1  execute() returns a ResponseMessageGeneric
pdq.requestAllLevels()
rmg = pdq.execute()

# 5.1, cont'd.  RMG's payload is a PointDataContainer
pdc = rmg.getContents()
# return ResponseMessageGeneric(pdc)

# Get the data for each requested parameter.
ista = pdc.getPointDataTypes().get("wmoStaNum").getIntData()
tobs = pdc.getPointDataTypes().get("validTime").getLongData()
rtyp = pdc.getPointDataTypes().get("rptType").getIntData()
lat = pdc.getPointDataTypes().get("latitude").getFloatData()
lon = pdc.getPointDataTypes().get("longitude").getFloatData()
elev = pdc.getPointDataTypes().get("staElev").getFloatData()
nman = pdc.getPointDataTypes().get("numMand").getIntData()
pman = pdc.getPointDataTypes().get("prMan").getFloatData()
zman = pdc.getPointDataTypes().get("htMan").getFloatData()
tman = pdc.getPointDataTypes().get("tpMan").getFloatData()
tdman = pdc.getPointDataTypes().get("tdMan").getFloatData()
ffman = pdc.getPointDataTypes().get("wsMan").getFloatData()
ddman = pdc.getPointDataTypes().get("wdMan").getFloatData()
nsigt = pdc.getPointDataTypes().get("numSigT").getIntData()
psigt = pdc.getPointDataTypes().get("prSigT").getFloatData()
tsigt = pdc.getPointDataTypes().get("tpSigT").getFloatData()
tdsigt = pdc.getPointDataTypes().get("tdSigT").getFloatData()
nsigw = pdc.getPointDataTypes().get("numSigW").getIntData()
zsigw = pdc.getPointDataTypes().get("htSigW").getFloatData()
ffsigw = pdc.getPointDataTypes().get("wsSigW").getFloatData()
ddsigw = pdc.getPointDataTypes().get("wdSigW").getFloatData()
ntrop = pdc.getPointDataTypes().get("numTrop").getIntData()
ptrop = pdc.getPointDataTypes().get("prTrop").getFloatData()
ttrop = pdc.getPointDataTypes().get("tpTrop").getFloatData()
tdtrop = pdc.getPointDataTypes().get("tdTrop").getFloatData()
fftrop = pdc.getPointDataTypes().get("wsTrop").getFloatData()
ddtrop = pdc.getPointDataTypes().get("wdTrop").getFloatData()
nmxw = pdc.getPointDataTypes().get("numMwnd").getIntData()
pmxw = pdc.getPointDataTypes().get("prMaxW").getFloatData()
ffmxw = pdc.getPointDataTypes().get("wsMaxW").getFloatData()
ddmxw = pdc.getPointDataTypes().get("wdMaxW").getFloatData()

# 5.2 and 5.3
if len(tobs) == 0 :
   msg = "couldn't get data"
   return ResponseMessageGeneric(msg)

# D-2D format files expect depression
i = len(tman)-1
while i>=0 :
    if tdman[i]>150 and tdman[i]<=tman[i] :
       tdman[i] = tman[i]-tdman[i]
    else :
       tdman[i] = -9999.0
    i -= 1
i = len(tsigt)-1
while i>=0 :
    if tdsigt[i]>150 and tdsigt[i]<=tsigt[i] :
       tdsigt[i] = tsigt[i]-tdsigt[i]
    else :
       tdsigt[i] = -9999.0
    i -= 1
i = len(ttrop)-1
while i>=0 :
    if tdtrop[i]>150 and tdtrop[i]<=ttrop[i] :
       tdtrop[i] = ttrop[i]-tdtrop[i]
    else :
       tdtrop[i] = -9999.0
    i -= 1

msg = "\n"
i = iMan = iSigT = iSigW = iTrop = iMxW = 0
while i < len(tobs) :
    if nman[i]<0 : nman[i] = 0
    if nsigt[i]<0 : nsigt[i] = 0
    if nsigw[i]<0 : nsigw[i] = 0
    if ntrop[i]<0 : ntrop[i] = 0
    if nmxw[i]<0 : nmxw[i] = 0
    if nman[i]==0 and nsigt[i]==0 and nsigw[i]==0 or rtyp[i]>2022:
       iMan += 100
       iSigT += 120
       iSigW += 120
       iTrop += 5
       iMxW += 5
       i += 1
       continue

    msg += str(ista[i]) + ","
    msg += str(tobs[i]/1000) + ","
    msg += "%.4f"%lat[i] + ","
    msg += "%.4f"%lon[i] + ","
    msg += "%.0f"%elev[i] + ","
    msg += str(ista[i]) + ","

    msg += str(nman[i]) + ","
    kk = iMan + nman[i]
    if nman[i]>0 : msg += "%.1f"%pman[iMan]
    k = iMan + 1
    while k < kk :
        msg += "|" + "%.1f"%pman[k];
        k += 1
    msg += ","
    if nman[i]>0 : msg += "%.1f"%elev[i]
    k = iMan + 1
    while k < kk :
        msg += "|" + "%.1f"%zman[k];
        k += 1
    msg += ","
    if nman[i]>0 : msg += "%.1f"%tman[iMan]
    k = iMan + 1
    while k < kk :
        msg += "|" + "%.1f"%tman[k];
        k += 1
    msg += ","
    if nman[i]>0 : msg += "%.1f"%tdman[iMan]
    k = iMan + 1
    while k < kk :
        msg += "|" + "%.1f"%tdman[k];
        k += 1
    msg += ","
    if nman[i]>0 : msg += "%.1f"%ffman[iMan]
    k = iMan + 1
    while k < kk :
        msg += "|" + "%.1f"%ffman[k];
        k += 1
    msg += ","
    if nman[i]>0 : msg += "%.1f"%ddman[iMan]
    k = iMan + 1
    while k < kk :
        msg += "|" + "%.1f"%ddman[k];
        k += 1
    msg += ","
    iMan += 100

    msg += str(nsigt[i]) + ","
    kk = iSigT + nsigt[i]
    if nsigt[i]>0 : msg += "%.1f"%psigt[iSigT]
    k = iSigT + 1
    while k < kk :
        msg += "|" + "%.1f"%psigt[k];
        k += 1
    msg += ","
    if nsigt[i]>0 : msg += "%.1f"%tsigt[iSigT]
    k = iSigT + 1
    while k < kk :
        msg += "|" + "%.1f"%tsigt[k];
        k += 1
    msg += ","
    if nsigt[i]>0 : msg += "%.1f"%tdsigt[iSigT]
    k = iSigT + 1
    while k < kk :
        msg += "|" + "%.1f"%tdsigt[k];
        k += 1
    msg += ","
    iSigT += 120

    msg += str(nsigw[i]) + ","
    kk = iSigW + nsigw[i]
    if nsigw[i]>0 : msg += "%.1f"%zsigw[iSigW]
    k = iSigW + 1
    while k < kk :
        msg += "|" + "%.1f"%zsigw[k];
        k += 1
    msg += ","
    if nsigw[i]>0 : msg += "%.1f"%ffsigw[iSigW]
    k = iSigW + 1
    while k < kk :
        msg += "|" + "%.1f"%ffsigw[k];
        k += 1
    msg += ","
    if nsigw[i]>0 : msg += "%.1f"%ddsigw[iSigW]
    k = iSigW + 1
    while k < kk :
        msg += "|" + "%.1f"%ddsigw[k];
        k += 1
    msg += ","
    iSigW += 120

    msg += str(ntrop[i]) + ","
    kk = iTrop + ntrop[i]
    if ntrop[i]>0 : msg += "%.1f"%ptrop[iTrop]
    k = iTrop + 1
    while k < kk :
        msg += "|" + "%.1f"%ptrop[k];
        k += 1
    msg += ","
    if ntrop[i]>0 : msg += "%.1f"%ttrop[iTrop]
    k = iTrop + 1
    while k < kk :
        msg += "|" + "%.1f"%ttrop[k];
        k += 1
    msg += ","
    if ntrop[i]>0 : msg += "%.1f"%tdtrop[iTrop]
    k = iTrop + 1
    while k < kk :
        msg += "|" + "%.1f"%tdtrop[k];
        k += 1
    msg += ","
    if ntrop[i]>0 : msg += "%.1f"%fftrop[iTrop]
    k = iTrop + 1
    while k < kk :
        msg += "|" + "%.1f"%fftrop[k];
        k += 1
    msg += ","
    if ntrop[i]>0 : msg += "%.1f"%ddtrop[iTrop]
    k = iTrop + 1
    while k < kk :
        msg += "|" + "%.1f"%ddtrop[k];
        k += 1
    msg += ","
    iTrop += 5

    msg += str(nmxw[i]) + ","
    kk = iMxW + nmxw[i]
    if nmxw[i]>0 : msg += "%.1f"%pmxw[iMxW]
    k = iMxW + 1
    while k < kk :
        msg += "|" + "%.1f"%pmxw[k];
        k += 1
    msg += ","
    if nmxw[i]>0 : msg += "%.1f"%ffmxw[iMxW]
    k = iMxW + 1
    while k < kk :
        msg += "|" + "%.1f"%ffmxw[k];
        k += 1
    msg += ","
    if nmxw[i]>0 : msg += "%.1f"%ddmxw[iMxW]
    k = iMxW + 1
    while k < kk :
        msg += "|" + "%.1f"%ddmxw[k];
        k += 1
    msg += "\n"
    iMxW += 5

    i += 1

return ResponseMessageGeneric(msg)

