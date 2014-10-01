import BaseRequest
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from com.raytheon.edex.plugin.satellite.dao import SatelliteDao

# Perform a satellite request for data of interest
sr = BaseRequest.BaseRequest("satellite")
sr.addParameter("creatingEntity","EEEEE","=")
sr.addParameter("sectorID","SSSSS","=")
sr.addParameter("physicalElement","CCCCC","=")
sr.addParameter("dataTime","AAAAA",">=")
sr.addParameter("dataTime","BBBBB","<=")

result = sr.execute()
size = result.size()
if size == 0:
   return ResponseMessageGeneric("Data not available")

# ResponseMessageGeneric.  Payload is SatelliteRecord
rmg = result.get(0)
part = "PPPPP"
encoding = XXXXX

# SatelliteRecord
srec = rmg.getContents()
mytime = srec.getDataURI().split('/',4)[2]
myent = srec.getCreatingEntity()

# SatelliteDao.  Inherits from PluginDao, which has a getHDF5Data method,
# which takes a PluginDataObject as an arg.
satdao = SatelliteDao("satellite")

# returns IDataRecord[].  IDataRecord is implemented by only one class --
# AbstractStorageRecord.  ASR is extended by a few *DataRecord classes; one
# of them is ByteDataRecord
idra = satdao.getHDF5Data(srec,-1)

msg = "No data."
if len(idra) > 0:
    # pick an arbitrary datarecord
    idr = idra[0]

    # this hints at the IDR's concrete class:  ByteDataRecord
    #print "true type of IDataRecord:", idr.getDataObject().toString()

    dim = idr.getDimension()
    if dim != 2:
        return ResponseMessageGeneric(msg)

    xLen = idr.getSizes()[0]
    yLen = idr.getSizes()[1]

    # byte[] -- the raw data
    barray = idr.getByteData()
    barraySize = len(barray)
    if xLen * yLen != barraySize:
        return ResponseMessageGeneric(msg)

    plus  = " ghijklmnopqrstuvwxyz"
    minus = " GHIJKLMNOPQRSTUVWXYZ"
    limit = 10000000
    if encoding == 1 :
       limit = limit/2
    elif  encoding == 0 :
       limit = limit/8

    k = xLen * ( yLen / 4 )
    j = 0
    nxy = yLen*xLen
    if part=="D" :
       j = k+k+k
    elif part=="C" :
       j = k+k
       nxy = j+k
    elif part=="B" :
       j = k
       nxy = j+k
    elif part=="A" or nxy>limit :
       nxy = k

    msg = "\n"
    if part<="A" :
       msg += str(xLen) + " " + str(yLen) + " "
       msg += mytime + " " + myent + "\n"

    while j<nxy :
        i = 0
        kk = barray[i+j]
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
           k = barray[i+j]
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

return ResponseMessageGeneric(msg)
