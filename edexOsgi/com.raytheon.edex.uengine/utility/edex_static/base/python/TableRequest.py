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
# Request of table data 
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/17/08                      njensen       Initial Creation.
#    
#



from java.util import ArrayList
from com.raytheon.edex.uengine.tasks.query import TableQuery
from com.raytheon.edex.uengine.tasks.response import MakeResponseXml
from com.raytheon.edex.uengine.tasks.response import MakeResponseNull
from com.raytheon.uf.common.message.response import ResponseMessageGeneric


class TableRequest():
    def __init__(self, db, tbl):
        self.__database = db
        self.__table = tbl
        self.__queryResults = None
        self.__query = TableQuery(self.__database, self.__table)
    
    def addParameter(self, name, value, operand="="):
        self.__query.addParameter(name, value, operand)
    
    def addList(self, name, value):
        self.__query.addList(name, value)

    def setCount(self, count):
        self.__query.setCount(count)
    
    def setSortValue(self, sortValue):
        self.__query.setSortBy(sortValue)
    
    def setTableName(self, name):
        self.__table = name
    
    def setDatabase(self, name):
        self.__database = name
    
    def __makeXmlResponse(self):
        size = self.__queryResults.size()
        response = ArrayList()
        for i in range(size):
            response.add(ResponseMessageGeneric(self.__queryResults.get(i)))
        return response
    
    def execute(self):
        self.__queryResults = self.__query.execute()
        return self.__makeXmlResponse()
         