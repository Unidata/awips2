#
# GempakGridNavigationRequest
#   
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a BaseRequest for a grid navigation parameters from GEMPAK.
#
#     Usage:
#    import GempakGridNavigationRequest
#    dataRequest = GempakGridNavigationRequest.GempakGridNavigationRequest()
#    dataRequest.setGridId("...")
#    return dataRequest.execute()
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    06/02/10        173_partC     mgamazaychikov       Initial Creation
#    02/02/11                      mli                  add eventName for dynamic model names
#

import BaseRequest
from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from com.raytheon.edex.uengine.tasks.query import SqlQueryTask
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert

class GempakGridNavigationRequest(BaseRequest.BaseRequest):

    def __init__(self, pluginName='grib'):
        self.eventName = None
        self.pluginName = pluginName
        if self.pluginName == 'grib':
            self.tableName = 'grib_models'
        elif self.pluginName == 'ncgrib':
            self.tableName = 'ncgrib_models'
        BaseRequest.BaseRequest.__init__(self, self.pluginName)

#
# Sets the ICAO parameter for the query
#
    def setGridIdParms(self, aGridName, *parms):
        for ii in range(len(parms)):
            if ii == 0: 
                #print "setting time to", parms[0]
                convert = GempakConvert()
                self.query.addParameter("dataTime", convert.dattimToDbtime(parms[0]))
            elif ii == 1:
                #print "setting eventName to", parms[1]
                self.query.addParameter("modelInfo.eventName", parms[1])

        self.gridName= aGridName

#
# Execute the BaseRequest and calls the appropriate response function
#       
    def execute(self):
             #
        # set up the db query for grib plugin
        #
        if self.pluginName == 'grib':            
            #
            # Construct the SQL query to retrieve record IDs from bufrua table 
            #
            gridIdQueryHead = "SELECT DISTINCT id FROM " + self.tableName + " WHERE modelname='"
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
            #print "modelInfoId=", modelInfoId
            self.query.addParameter("modelInfo.id","%s" % singleGridId)
        #
        # set up the db query for ncgrib plugin
        #
        elif self.pluginName == 'ncgrib':
            self.query.addParameter("modelInfo.modelName","%s" % self.gridName)
#            if (self.eventName != None):
#                self.query.addParameter("modelInfo.eventName","%s" % self.eventName)
            self.query.setCount(1)
        #
        # execute the query
        #
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
            if self.pluginName == 'grib':
                content = GempakConvert.getGridNavigationContent(currentQuery.getSpatialObject())
            elif self.pluginName == 'ncgrib':
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