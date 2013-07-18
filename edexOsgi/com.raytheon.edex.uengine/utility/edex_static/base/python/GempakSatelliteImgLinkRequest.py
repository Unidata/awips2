#
# GempakSatelliteImgLinkRequest
#   
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a BaseRequest for a satellite image from GEMPAK, and stores
# the image data in the uEngineProducts directory for later transfer.
#
#     Usage:
#    import GempakSatelliteImgLinkRequest
#    dataRequest = GempakSatelliteImgLinkRequest.GempakSatelliteImgLinkRequest()
#    dataRequest.setPhysicalElement("...")
#    dataRequest.setSectorID("...")
#    dataRequest.setTime("090519/1100")
#    return dataRequest.execute()
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    10/14/09        173            mgamazaychikov       Initial Creation.
#

import BaseRequest
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from java.util import ArrayList
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert

class GempakSatelliteImgLinkRequest(BaseRequest.BaseRequest):

    def __init__(self):
        BaseRequest.BaseRequest.__init__(self, "satellite")
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
        response = ArrayList()
        size = self.queryResults.size() 
        for i in range(size):
            currentQuery = self.queryResults.get(i)
            #
            # Call FileIn constructor
            #
            fileIn = FileIn(self.plugin, currentQuery)
            
            #
            # Call the execute method, getting back the data record
            #
            record = fileIn.execute()
            
            fileOut = FileOut(record.getDataObject(), self.format)
            
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