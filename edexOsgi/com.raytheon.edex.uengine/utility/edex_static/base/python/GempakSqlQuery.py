#
# GempakSqlQuery
#   
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a BaseRequest for a grid data from GEMPAK, and stores
# the float data in the uEngineProducts directory for later transfer.
#
#     Usage:
#    import GempakSqlQuery
#    gsq = GempakSqlQuery.GempakSqlQuery()
#    gsq.setQuery("...")
#    [gsq.setReturnTest()]
#    return gsq.execute()
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    06/02/10        173_partC     mgamazaychikov       Initial Creation.
#    09/09/10                      mgamazaychikov       Added setSeparator function
#
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from com.raytheon.uf.common.dataquery.db import QueryResult
from com.raytheon.uf.edex.database.tasks import SqlQueryTask
from java.util import ArrayList

class GempakSqlQuery(): 
#
# Initializes the query
#
    def __init__ (self, dbName="metadata"):       
        self.dbname = dbName
        self.queryResults = None
        self.returnText = False
        self.isTextSeparated = False
        
    def setQuery (self, aQuery):
        self.query = aQuery
    
    def setReturnText (self):
        self.returnText = True
    
    def setSeparator(self, aSeparator):
        self.separator = aSeparator
        self.isTextSeparated = True

#
# Returns a string with null response  
#
    def makeNullResponse(self):
        nullStr = "Database Query returned no results"
        if self.returnText:
            return nullStr
        else:
            response = ArrayList()
            response.add(ResponseMessageGeneric(nullStr))
            return response 


#
# Returns a string with response  
#
    def makeResponse(self):
        response = ArrayList()
        returnString=""
        queryRows = ArrayList()
        queryRows = self.queryResults.getRows()
        for qrow in queryRows:
            rowStr = "%s" % qrow
            if self.returnText:
                returnString = returnString + rowStr[1:-1]  
                if self.isTextSeparated:
                    returnString = returnString + self.separator
            else:
                response.add(ResponseMessageGeneric(rowStr[1:-1]))
        if self.returnText:
            if self.isTextSeparated:
                return returnString[0:-1]
            else:
                return returnString
        else:
            return response       
    
# 
# Executes the query and calls appropriate response functions
#
    def execute(self):
        #self.queryResults = ArrayList()
        
        #
        # Create an instance of SQL Query and execute it
        #
        self.sqlQuery = SqlQueryTask(self.query, self.dbname)
        self.queryResults = self.sqlQuery.execute()
            
        #
        # Make response based on the query results
        #        
        if self.queryResults is None:
            return self.makeNullResponse()
        else:
            return self.makeResponse()