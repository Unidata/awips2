import BaseRequest
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from com.raytheon.edex.plugin.radar.dao import RadarDao

# Perform a radar request for data of interest
rr = BaseRequest.BaseRequest("radar")
rr.addParameter("icao","KKKK","=")
rr.addParameter("productCode","MMMM","=") 
rr.addParameter("primaryElevationAngle","EEEE","=")
rr.addParameter("dataTime","AAAAA",">=")
rr.addParameter("dataTime","BBBBB","<=")
hedfmt = "FFF"

result = rr.execute()
size = result.size()
if size == 0:
   return ResponseMessageGeneric("Data not available")

# ResponseMessageGeneric.  Payload is RadarRecord
rmg = result.get(0)
# return rmg

# RadarRecord
rrec = rmg.getContents()

#
#  From here to the end is the part we know how to do for radar but not
#  for radar.
#
mytime = rrec.getDataURI().split('/',4)[2]

# RadarDao.  Inherits from PluginDao, which has a getHDF5Data method,
# which takes a PluginDataObject as an arg.
raddao = RadarDao("radar")

# returns IDataRecord[].  IDataRecord is implemented by only one class --
# AbstractStorageRecord.  ASR is extended by a few *DataRecord classes; one
# of them is ByteDataRecord
idra = raddao.getHDF5Data(rrec,-1)

msg = "No data."
if len(idra) > 0:
   # pick radar Data record
   # record 0 contains Angles             getFloatData
   # record 1 contains Data               getByteData
   # record 2 contains DependentValues    getShortData
   # record 3 contains ProductVals        getByteData
   # record 4 contains RecordVals         getByteData
   # record 5 contains StormIds           getByteData
   # record 6 contains Symbology          getByteData
   # record 7 contains SymbologyData      getByteData
   # record 8 contains Thresholds         getShortData
   dattyp = "raster"
   for ii in range(len(idra)):
      if idra[ii].getName() == "Data":
         rdat = idra[ii]
      elif idra[ii].getName() == "Angles":
         azdat = idra[ii]
         dattyp = "radial"
      elif idra[ii].getName() == "DependentValues":
         depVals = idra[ii].getShortData()
#     elif idra[ii].getName() == "ProductVals":
#        prodVals = idra[ii].getByteData()
#     elif idra[ii].getName() == "RecordVals":
#        recVals = idra[ii].getByteData()
#     elif idra[ii].getName() == "StormIds":
#        stormVals = idra[ii].getByteData()
#     elif idra[ii].getName() == "Symbology":
#        symVals = idra[ii].getByteData()
#     elif idra[ii].getName() == "SymbologyData":
#        symData = idra[ii].getByteData()
      elif idra[ii].getName() == "Thresholds":
         threshVals = idra[ii].getShortData()

   # this hints at the IDR's concrete class:  ByteDataRecord
   #print "true type of IDataRecord:", idr.getDataObject().toString()

   dim = rdat.getDimension()
   if dim != 2:
       return ResponseMessageGeneric(msg)

   yLen = rdat.getSizes()[0]
   xLen = rdat.getSizes()[1]

   # byte[] -- the raw data
   array = rdat.getByteData()
   arraySize = len(array)
   if xLen * yLen != arraySize:
      return ResponseMessageGeneric(msg)

   # get data for azimuth angles if we have them.
   if dattyp == "radial" :
      azVals = azdat.getFloatData()
      azValsLen = len(azVals)
      if yLen != azValsLen:
         return ResponseMessageGeneric(msg)
   description = "DDDDD"

   # Encode dimensions, time, mapping, description, tilt, and VCP
   if hedfmt == "x" :
      msg = "\n"+ str(xLen) + " " + str(yLen) + " " + mytime + " " + \
             dattyp + " " + str(rrec.getLatitude()) + " " +  \
             str(rrec.getLongitude()) + " " +  \
             str(rrec.getElevation()) + " " +  \
             str(rrec.getElevationNumber()) + " " +  \
             description + " " + str(rrec.getTrueElevationAngle()) + " " + \
             str(rrec.getVolumeCoveragePattern()) + "\n"
   else :
      msg = "\n"+ str(xLen) + " " + str(yLen) + " " + mytime + " " + \
             dattyp + " " + description + " " + \
             str(rrec.getTrueElevationAngle()) + " " + \
             str(rrec.getVolumeCoveragePattern()) + "\n"
   
   # Encode level labels
   spec = [".", "TH", "ND", "RF", "BI", "GC", "IC", "GR", "WS", "DS",
           "RA", "HR", "BD", "HA", "UK"]
   nnn = len(threshVals)
   j = 0
   while j<nnn :
      lo = threshVals[j] % 256
      hi = threshVals[j] / 256
      msg += " "
      j += 1
      if hi < 0 :
         if lo > 14 :
            msg += "."
         else :
            msg += spec[lo]
         continue
      if hi % 16 >= 8 :
         msg += ">"
      elif hi % 8 >= 4 :
         msg += "<"
      if hi % 4 >= 2 :
         msg += "+"
      elif hi % 2 >= 1 :
         msg += "-"
      if hi >= 64 :
         msg += "%.2f"%(lo*0.01)
      elif hi % 64 >= 32 :
         msg += "%.2f"%(lo*0.05)
      elif hi % 32 >= 16 :
         msg += "%.1f"%(lo*0.1)
      else :
         msg += str(lo)
   msg += "\n"

   # Encode product dependent parameters 17 through 26
   nnn = len(depVals)
   j = 0
   while j<nnn :
      msg += " " + str(depVals[j])
      j += 1
   msg += "\n"

   if dattyp == "radial" :
      j = 0
      while j<azValsLen :
          msg += "%.1f"%azVals[j] + " "
          j += 1
      msg += "\n"

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

return ResponseMessageGeneric(msg)

