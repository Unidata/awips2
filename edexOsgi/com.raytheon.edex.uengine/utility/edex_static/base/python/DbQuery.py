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
#    06/05/08        #875          bphillip       Initial Creation.
#    06/12/08                      M. Duff        Added setOrderByList.
#    08/07/08                      M. Duff        Added maxResults.
#    
# 



from com.raytheon.edex.uengine.tasks.query import CatalogQuery


class DbQuery():
    
    def __init__(self, dbName, className):
        self.__cat = CatalogQuery(dbName,className)
        
    def setDistinctField(self,field):
        self.__cat.setDistinctField(field)   
    
    def addReturnedField(self, name):
        self.__cat.addReturnedField(name)
        
    def setReturnedFieldList(self,nameList):
        self.__cat.setReturnedFieldList(nameList)
    
    def addConstraint(self, name, value, operand="="):   
        self.__cat.addParameter(name, value, operand)
        
    def setOrderByList(self,orderList,ascending):
        self.__cat.setSortBy(orderList, ascending)
        
    def setCount(self, count):
        self.__cat.setCount(count)
        
    def setMaxResults(self,maxResults):
        self.__cat.setCount(maxResults)
    
    def execute(self):
        return self.__cat.execute()