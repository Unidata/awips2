# Example to retrieve a single grid point

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
   while j>=0 :
       i = 0
       while i<xLen :
          k = rawData[i+j]
          if k<0 :
             a = -k
          else :
             a = k
          if a>=999998 :
             msg += "1e37 "
          elif a<0.00005 :
             msg += "%g"%k + " "
          elif a<0.0009 :
             if round(k,8) == round(k,4) :
                msg += "%.4f"%k + " "
             elif round(k,8) == round(k,5) :
                msg += "%.5f"%k + " "
             elif round(k,8) == round(k,6) :
                msg += "%.6f"%k + " "
             elif round(k,8) == round(k,7) :
                msg += "%.7f"%k + " "
             else :
                msg += "%.8f"%k + " "
          elif a<0.009 :
             if round(k,7) == round(k,3) :
                msg += "%.3f"%k + " "
             elif round(k,7) == round(k,4) :
                msg += "%.4f"%k + " "
             elif round(k,7) == round(k,5) :
                msg += "%.5f"%k + " "
             elif round(k,7) == round(k,6) :
                msg += "%.6f"%k + " "
             else :
                msg += "%.7f"%k + " "
          elif a<0.09 :
             if round(k,6) == round(k,2) :
                msg += "%.2f"%k + " "
             elif round(k,6) == round(k,3) :
                msg += "%.3f"%k + " "
             elif round(k,6) == round(k,4) :
                msg += "%.4f"%k + " "
             elif round(k,6) == round(k,5) :
                msg += "%.5f"%k + " "
             else :
                msg += "%.6f"%k + " "
          elif a<0.9 :
             if round(k,5) == round(k,1) :
                msg += "%.1f"%k + " "
             elif round(k,5) == round(k,2) :
                msg += "%.2f"%k + " "
             elif round(k,5) == round(k,3) :
                msg += "%.3f"%k + " "
             elif round(k,5) == round(k,4) :
                msg += "%.4f"%k + " "
             else :
                msg += "%.5f"%k + " "
          elif a<9 :
             if round(k,4) == round(k,0) :
                msg += "%.0f"%k + " "
             elif round(k,4) == round(k,1) :
                msg += "%.1f"%k + " "
             elif round(k,4) == round(k,2) :
                msg += "%.2f"%k + " "
             elif round(k,4) == round(k,3) :
                msg += "%.3f"%k + " "
             else :
                msg += "%.4f"%k + " "
          elif a<99 :
             if round(k,3) == round(k,0) :
                msg += "%.0f"%k + " "
             elif round(k,3) == round(k,1) :
                msg += "%.1f"%k + " "
             elif round(k,3) == round(k,2) :
                msg += "%.2f"%k + " "
             else :
                msg += "%.3f"%k + " "
          elif a<999 :
             if round(k,2) == round(k,0) :
                msg += "%.0f"%k + " "
             elif round(k,2) == round(k,1) :
                msg += "%.1f"%k + " "
             else :
                msg += "%.2f"%k + " "
          elif a<9999 :
             if round(k,1) == round(k,0) :
                msg += "%.0f"%k + " "
             else :
                msg += "%.1f"%k + " "
          else :
                msg += "%.0f"%k + " "
          i += 1
       msg += "\n"
       j -= xLen
   return ResponseMessageGeneric(msg)
   
return ResponseMessageGeneric("Data not available")
