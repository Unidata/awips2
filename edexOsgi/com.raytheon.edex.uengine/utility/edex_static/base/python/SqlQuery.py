##
##

##
# This is a base file that is not intended to be overridden.
##

##
# uengine is deprecated and will be removed from the system soon. Migrate your
# apps to using the Data Access Framework (DAF).
##

#
# Generalized query script for querying arbitrary rows out of any table in any database
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/16/08        #1615         bphillip       Initial Creation.
#    07/13/15        4500          rjpeter        Remove SqlQueryTask.
# 



from com.raytheon.uf.edex.database.dao import CoreDao
from com.raytheon.uf.edex.database.dao import DaoConfig
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from java.util import ArrayList


class SqlQuery():
    
    def __init__(self, sqlQuery,dbName="metadata"):
        self.__query = sqlQuery
        self.__dbName = dbName
    
    def execute(self):
        dao = CoreDao(DaoConfig.forDatabase(self.__dbName))
        queryResults = dao.executeMappedSQLQuery(self.__query)
        response = ArrayList()
        response.add(ResponseMessageGeneric(queryResults))
        return response 