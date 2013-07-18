#
# GempakNcgridNavigationRequest
#   
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a BaseRequest for a grid navigation parameters from GEMPAK.
#
#     Usage:
#    import GempakNcgridNavigationRequest
#    dataRequest = GempakNcgridNavigationRequest.GempakNcgridNavigationRequest()
#    dataRequest.setGridId("...")
#    return dataRequest.execute()
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    06/02/10        173_partC     mgamazaychikov       Initial Creation
#

import BaseRequest
from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from com.raytheon.edex.uengine.tasks.query import SqlQueryTask
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert

class GempakNcgridNavigationRequest(BaseRequest.BaseRequest):

    def __init__(self):
        BaseRequest.BaseRequest.__init__(self, "ncgrib")

#
# Sets the ICAO parameter for the query
#
    def setGridId(self, aGridName):
        self.gridName= aGridName

#
# Execute the BaseRequest and calls the appropriate response function
#       
    def execute(self):
        #
        # Construct the SQL query to retrieve record IDs from bufrua table 
        #
        gridIdQueryHead = "SELECT DISTINCT id FROM ncgrib_models WHERE modelname='"
        gridIdQueryTail = "'"
        gridIdQuery = gridIdQueryHead + self.gridName + gridIdQueryTail

        #
        #
        # Create an instance of SQL Query and execute it
        #
        self.sqlGridIDQuery = SqlQueryTask(gridIdQuery)
        sqlGridIDQueryResults = self.sqlGridIDQuery.execute()

        #
        # Retrieve the rows into the ArrayList of grid IDs
        #
        gridID = ArrayList()
        gridID = sqlGridIDQueryResults.getRows()
        gridIDList = ArrayList()
        for gid in gridID:
            strID = "%s" % gid
            gridIDList.add(strID[1:-1])
        szID = gridIDList.size()
        if szID == 0:
            return self.makeNullResponse()
        singleGridId = gridIDList.get(0)
        self.query.setCount(1)
        modelInfoId = "%s" % singleGridId
        self.query.addParameter("modelInfo.id","%s" % singleGridId)
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
            content = GempakConvert.getNcgridNavigationContent(currentQuery.getSpatialObject())
            response.add(ResponseMessageGeneric(content))
        return response

#
# Returns a string with null response  
#
    def makeNullResponse(self):
        response = ArrayList()
        response.add(ResponseMessageGeneric("Database Query returned no results"))
        return response