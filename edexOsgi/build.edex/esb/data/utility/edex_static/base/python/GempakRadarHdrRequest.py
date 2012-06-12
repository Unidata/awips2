#
# GempakRadarHdrRequest
#   
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a BaseRequest for a radar "image header" from GEMPAK.
#
#     Usage:
#    import GempakRadarHdrRequest
#    dataRequest = GempakRadarHdrRequest.GempakRadarHdrRequest()
#    dataRequest.setPhysicalElement("...")
#    dataRequest.setSectorID("...")
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


class GempakRadarHdrRequest(BaseRequest.BaseRequest):

    def __init__(self):
        BaseRequest.BaseRequest.__init__(self, "radar")
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
    def setIcao(self, aIcao):
        name = "icao"
        self.query.addParameter(name, aIcao)

#
# Sets the Product Code parameter for the query
#
    def setProduct(self, aProduct):
        name = "productCode"
        self.query.addParameter(name, aProduct)
        
#
# Sets the Gate Resolution parameter for the query
#
    def setResolution(self, aResolution):
        name = "gateResolution"
        theResolution = "%s" % int ( float(aResolution) * 1000 )
        self.query.addParameter(name, theResolution)

#
# Sets the Elevation Angle parameter for the query
#
    def setAngle(self, aAngle):
        name = "trueElevationAngle"
        self.query.addParameter(name, aAngle)
 
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
            aAngle = "%s" % currentQuery.getTrueElevationAngle()
            aNumBins = "%s" % currentQuery.getNumBins()
            aFormat = "%s" % currentQuery.getFormat()
            aDbTime = "%s" % currentQuery.getDataTime()
            aDattim = GempakConvert.dbtimeToSatDattim(aDbTime)
            content = aLat + ";" + aLon + ";" + aProd + ";" + aAngle + ";" + aNumBins + ";" + aFormat + ";" + aDattim
            fileIn = FileIn(self.plugin, currentQuery)
            records = fileIn.retrieveGroup()
            convert = GempakConvert();
            thresholds = convert.getRadarThresholds(currentQuery, records);
            rpgIdDec = currentQuery.getSpatialObject().getRpgIdDec()
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