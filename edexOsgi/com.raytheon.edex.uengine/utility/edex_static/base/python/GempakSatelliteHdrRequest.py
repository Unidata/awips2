#
# GempakSatelliteHdrRequest
#   
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a BaseRequest for a satellite "image header" from GEMPAK.
#
#     Usage:
#    import GempakSatelliteHdrRequest
#    dataRequest = GempakSatelliteHdrRequest.GempakSatelliteHdrRequest()
#    dataRequest.setPhysicalElement("...")
#    dataRequest.setSectorID("...")
#    dataRequest.setTime("...")
#    return dataRequest.execute()
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    10/14/09        173            mgamazaychikov       Initial Creation
#

import BaseRequest
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from java.util import ArrayList
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert


class GempakSatelliteHdrRequest(BaseRequest.BaseRequest):

    def __init__(self):
        BaseRequest.BaseRequest.__init__(self, "satellite")
#
# Converts time form GEMPAK format to database format
# and sets the dataTime parameter for the query
#
    def setTime(self, aDattim):
        convert = GempakConvert()
        name = "dataTime"
        operand = "between"
        aw2Time = convert.dattimToDbtime(aDattim)
        timeRange = convert.setTimeRange(aw2Time)
        self.query.addParameter(name, timeRange, operand)
        
#
# Sets the Physical Element parameter for the query
#
    def setPhysicalElement(self, aPhysEl):
        name = "physicalElement"
        operand = "like"
        thePhysEl = "%" + aPhysEl + "%"
        self.query.addParameter(name, thePhysEl, operand)

#
# Sets the Sector ID parameter for the query
#
    def setSectorID(self, aSecId):
        name = "sectorID"
        operand = "like"
        theSecId = "%" + aSecId + "%"
        self.query.addParameter(name, theSecId, operand)
 
#
# Execute the BaseRequest and calls the appropriate response function
#       
    def execute(self):
        self.query.setCount(1)
        self.queryResults = self.query.execute()
        if self.queryResults is None or self.queryResults.size() == 0:
            self.makeNullResponse()
        else:
            return self.__makeResponse()

#
# Builds the return string content and adds it to the response ArrayList
#
    def __makeResponse(self):

        response = ArrayList()
        size = self.queryResults.size()
        for i in range(size):
            currentQuery = self.queryResults.get(i)
            
            content = GempakConvert.getSatHdrContent(currentQuery.getSpatialObject())
            content = content + ";" + currentQuery.getCreatingEntity() + ";" + currentQuery.getPhysicalElement()
            aDbTime = "%s" % currentQuery.getDataTime()
            aDattim = GempakConvert.dbtimeToSatDattim(aDbTime)
            content = content + ";" + aDattim          
            response.add(ResponseMessageGeneric(content))

        return response
    
#
# Returns a string with null response  
#
    def makeNullResponse(self):
        response = ArrayList()
        response.add(ResponseMessageGeneric("Database Query returned no results"))
        return response