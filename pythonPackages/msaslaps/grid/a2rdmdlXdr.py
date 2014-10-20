# Example to retrieve a single grid point
import xdrlib
import bz2
import GridRequest
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from com.raytheon.edex.uengine.tasks.decode import FileIn

# Perform a grid request for data of interest
gr = GridRequest.GridRequest("grid")
gr.addParameter("info.datasetId", "SSSSS")
gr.addParameter("info.level.masterLevel.name", "TTTTT")
gr.addParameter("info.level.levelonevalue", "LLLLL")
gr.addParameter("info.level.leveltwovalue", "22222")
gr.addParameter("info.parameter.abbreviation", "VVVVV")
gr.addParameter("dataTime", "DDDDD HHHHH:00:00.0 (FFFFF)RRRRR")

result = gr.query.execute()
size = result.size()
if size == 0:
   return ResponseMessageGeneric("Data not available")

# For the purposes of this script, take the first response
currentQuery = result.get(0)

# Get the data in memory
fileIn = FileIn("grid", currentQuery)
data = fileIn.execute()

if data:

   # Pull out float[] and get sizes
   rawData = data.getFloatData()
   xLen = data.getSizes()[0]
   yLen = data.getSizes()[1]
   dimStr = str(xLen) + " " + str(yLen) + "\n"

   # Pull out float[] and get sizes
   rawData = data.getFloatData()
   xLen = data.getSizes()[0]
   yLen = data.getSizes()[1]
   msg = "\n"
   msg += dimStr
   nxy = yLen*xLen
   j = nxy-xLen
   mypacker = xdrlib.Packer()
   mypacker.reset()
   while j>=0 :
       i = 0
       while i<xLen :
           mypacker.pack_float(float(rawData[i+j]))
           i += 1
       j -= xLen
   packLen = len(mypacker.get_buffer())
   xdrbuf = bz2.compress(mypacker.get_buffer())
   cmpLen = len(xdrbuf)
   msg += str(packLen)+" "+str(cmpLen*2)+"\t\n"
   i = 0
   while i<cmpLen :
       msg += "%2.2x"%ord(xdrbuf[i])
       i += 1
   msg += "\t\n"
   return ResponseMessageGeneric(msg)

return ResponseMessageGeneric("Data not available")
