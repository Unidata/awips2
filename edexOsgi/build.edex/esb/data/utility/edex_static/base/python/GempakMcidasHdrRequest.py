#
# GempakMcidasHdrRequest
#   
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a BaseRequest for a McIDAS "image header" from GEMPAK.
#
#    Usage:
#    import GempakMcidasHdrRequest
#    dataRequest = GempakMcidasHdrRequest.GempakMcidasHdrRequest()
#    dataRequest.setImageType("...")
#    dataRequest.setAreaName("...")
#    dataRequest.setResolution("...")
#    dataRequest.setTime("...")
#    return dataRequest.execute()
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    12/22/09        173_partB     mgamazaychikov       Initial Creation
#

import BaseRequest
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from java.util import ArrayList
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert
from com.raytheon.edex.uengine.tasks.query import SqlQueryTask


class GempakMcidasHdrRequest(BaseRequest.BaseRequest):

    def __init__(self):
        BaseRequest.BaseRequest.__init__(self, "mcidas")
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
# Sets the Image Type parameter for the query
#
    def setImageType(self, aImageType):
        name = "imageType"
        operand = "like"
        theImageType = "%" + aImageType + "%"
        self.query.addParameter(name, theImageType, operand)

#
# Sets the Area Name parameter for the query
#
    def setAreaName(self, aAreaName):
        name = "areaName"
        operand = "like"
        theAreaName = "%" + aAreaName + "%"
        self.query.addParameter(name, theAreaName, operand)
        
#
# Sets the Resolution parameter for the query
#
    def setResolution(self, aResolution):
        name = "resolution"
        operand = "="
        self.query.addParameter(name, aResolution, operand)
 
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
            
            content = GempakConvert.getMcidasHdrContent(currentQuery.getSpatialObject())
            
            content = content + ";" + "%s" % currentQuery.getSatelliteId() + \
                      ";" + "%s" % currentQuery.getImageTypeNumber() + \
                      ";" + "%s" % currentQuery.getCalType()
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