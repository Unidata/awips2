#
# GempakGridLinkRequest
#   
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a BaseRequest for a grid data from GEMPAK, and stores
# the float data in the uEngineProducts directory for later transfer.
#
#     Usage:
#    import GempakGridLinkRequest
#    dataRequest = GempakGridLinkRequest.GempakGridLinkRequest("grib")
#    dataRequest.setDataUri("...")
#    return dataRequest.execute()
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    06/02/10        173_partC     mgamazaychikov       Initial Creation.
#

import BaseRequest
from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert

class GempakGridLinkRequest(BaseRequest.BaseRequest):

    def __init__(self,pluginName):
        BaseRequest.BaseRequest.__init__(self, pluginName)
        self.format = "grid"
        self.pluginName = pluginName
        
#
# Sets the dataURI parameter for the query
#
    def setDataUri(self, aDataUri):
        name = "dataURI"
        operand = "="
        theDataUri = aDataUri
        self.query.addParameter(name, theDataUri, operand)

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
        from gov.noaa.nws.ncep.edex.uengine.output import GridOut        
        response = ArrayList()
        size = self.queryResults.size() 
        for i in range(size):
            currentQuery = self.queryResults.get(i)
            
            #
            # grid coverage
            #
            if self.pluginName == 'grib':
                gridNavInfo = GempakConvert.getGridNavigationContent(currentQuery.getSpatialObject()).split(";")
            elif self.pluginName == 'ncgrib':
                gridNavInfo = GempakConvert.getNcgridNavigationContent(currentQuery.getSpatialObject()).split(";")
            
            nx = gridNavInfo[1]
            ny = gridNavInfo[2]

            #
            # Call FileIn constructor
            #
            fileIn = FileIn(self.plugin, currentQuery)

            #
            # Call the execute method, getting back the data record
            #
            record = fileIn.execute()
            self.nxStr = "%s" % nx
            self.nyStr = "%s" % ny
            fileOut = GridOut(record.getDataObject(), self.format, self.nxStr, self.nyStr)
            
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
            #
            # 
            import socket
            hostname = socket.gethostbyaddr(socket.gethostname())[2][0]
            #
            # Return content wrapped in a generic XML message
            #
            response.add(ResponseMessageGeneric(hostname+"|"+content))
            
        return response
    
#
# Returns a string with null response  
#
    def makeNullResponse(self):
        response = ArrayList()
        response.add(ResponseMessageGeneric("Database Query returned no results"))
        return response