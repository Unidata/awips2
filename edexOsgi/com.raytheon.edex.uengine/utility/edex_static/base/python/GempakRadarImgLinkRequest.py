#
# GempakRadarImgLinkRequest
#   
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a BaseRequest for a radar image from GEMPAK, and stores
# the image data in the uEngineProducts directory for later transfer.
#
#    Usage:
#    import GempakRadarImgLinkRequest
#    dataRequest = GempakRadarImgLinkRequest.GempakRadarImgLinkRequest()
#    dataRequest.setProduct("...")
#    dataRequest.setIcao("...")
#    dataRequest.setResolution("...")
#    dataRequest.setAngle("...")
#    dataRequest.setTime("...")
#    return dataRequest.execute()
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    12/22/09        173_partB     mgamazaychikov       Initial Creation
#    05/20/14        2913          bsteffen             Remove unused imports
#

import BaseRequest
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from java.util import ArrayList
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert


class GempakRadarImgLinkRequest(BaseRequest.BaseRequest):

    def __init__(self):
        BaseRequest.BaseRequest.__init__(self, "radar")
        self.format = "db2g"
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
        from com.raytheon.edex.uengine.tasks.output import FileOut
        from gov.noaa.nws.ncep.edex.uengine.tasks.radar import GempakDecodeRadarImage
        response = ArrayList()
        size = self.queryResults.size() 
        for i in range(size):
            currentQuery = self.queryResults.get(i)
            #
            # Call FileIn constructor
            #
            fileIn = FileIn(self.plugin, currentQuery)
            records = fileIn.retrieveGroup()
            radarImage = GempakDecodeRadarImage(currentQuery, records)

            #
            # Call the execute method, getting back the data record
            #
            record = radarImage.execute()
            
            fileOut = FileOut(record, self.format)
            
            #
            # Execute method of FileOut class stores the data 
            # in a file in uEngineProducts directory with the extension
            # contained in the string self.format 
            #
            writeFile = fileOut.execute()
            
            #
            # Separate the file name from the complete path and store in content
            #
            content = ("%s" % writeFile).split("/")[len(("%s" % writeFile).split("/") ) - 1]
            
            #
            # Return content wrapped in a generic XML message
            #
            response.add(ResponseMessageGeneric(content))
            
        return response
    
#
# Returns a string with null response  
#
    def makeNullResponse(self):
        response = ArrayList()
        response.add(ResponseMessageGeneric("Database Query returned no results"))
        return response