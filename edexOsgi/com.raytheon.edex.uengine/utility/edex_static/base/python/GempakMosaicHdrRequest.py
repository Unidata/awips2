#
# GempakMosaicHdrRequest
#   
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a BaseRequest for a MOsaic "image header" from GEMPAK.
#
#     Usage:
#    import GempakMosaicHdrRequest
#    dataRequest = GempakMosaicHdrRequest.GempakMosaicHdrRequest()
#    dataRequest.setProduct("...")
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


class GempakMosaicHdrRequest(BaseRequest.BaseRequest):

    def __init__(self):
        BaseRequest.BaseRequest.__init__(self, "mosaic")
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
# Sets the ICAO parameter for the query
#
#    def setSector(self, aIcao):
#        name = "icao"
#        self.query.addParameter(name, aIcao)

#
# Sets the Product Code parameter for the query
#
    def setProduct(self, aProduct):
        name = "prodName"
        self.query.addParameter(name, aProduct)
        
#
# Sets the Gate Resolution parameter for the query
#
    def setResolution(self, aResolution):
        name = "resolution"
        theResolution = "%s" % int ( float(aResolution) * 1000 )
        self.query.addParameter(name, theResolution)
 
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
        from com.raytheon.edex.uengine.tasks.decode import FileIn
        response = ArrayList()
        size = self.queryResults.size()
        for i in range(size):
            currentQuery = self.queryResults.get(i)
            aLat = "%s" % currentQuery.getLatitude()
            aLon = "%s" % currentQuery.getLongitude()
            aProd = "%s" % currentQuery.getProductCode()
            aNx = "%s" % currentQuery.getNx()
            aNy = "%s" % currentQuery.getNy()
            aDbTime = "%s" % currentQuery.getDataTime()
            aDattim = GempakConvert.dbtimeToSatDattim(aDbTime)
            content = aLat + ";" + aLon + ";" + aProd + ";" + aNx + ";" + aNy + ";" + aDattim
            fileIn = FileIn(self.plugin, currentQuery)
            records = fileIn.retrieveGroup()
            convert = GempakConvert();
            thresholds = convert.getRadarThresholds(currentQuery, records);
            rpgIdDec = "10000"
            content =  content + ";"+ thresholds[0:-1] + ";" + rpgIdDec
            response.add(ResponseMessageGeneric(content))
    
        return response
    
#
# Returns a string with null response  
#
    def makeNullResponse(self):
        response = ArrayList()
        response.add(ResponseMessageGeneric("Database Query returned no results"))
        return response