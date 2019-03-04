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
# XML request of data
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/14/08                      njensen       Initial Creation.
#    
# 



from java.util import ArrayList
from com.raytheon.edex.uengine.tasks.query import TermQuery
from com.raytheon.edex.uengine.tasks.query import TableQuery
from com.raytheon.uf.common.message.response import ResponseMessageGeneric


class BaseRequest():
    
    def __init__(self, pluginName,database=None,className=None):
        self.plugin = pluginName
        self.queryResults = None
        self.dbName = database
        self.clazz = className
        if database is None:
            self.query = TermQuery(self.plugin)
        else:
            self.query = TableQuery(database,className)
        
    def setDistinctField(self,field,className=None):
        self.query.setDistinctField(field,className)
        
    def addReturnedField(self,name,className=None):
        self.query.addReturnedField(name,className)
        
    def setReturnedFieldList(self,nameList,className=None):
        self.query.setReturnedFieldList(nameList,className)
        
    def addParameter(self, name, value, operand="=",className=None):    
        self.query.addParameter(name, value, operand,className)
    
    def addList(self, name, value,className=None):
        self.query.addList(name, value,className)
        
    def addJoinField(self,class1,class2,field1,field2=None):
        self.query.addJoinField(class1,class2,field1,field2)
        
    def setJoinFields(self,joinList):
        self.query.setJoinList(joinList)

    def setCount(self, count):
        self.query.setCount(count)
 
    def setSortValue(self, sortValue,order,className=None):
        self.query.setSortBy(sortValue,order,className)
        
    def setOrderByList(self,orderList,ascending,className=None):
        self.query.setSortBy(orderList, ascending,className)
    
    def makeResponse(self):
        return ResponseMessageGeneric.wrap(self.queryResults)
    
    def execute(self):
        self.queryResults = self.query.execute()
        return self.makeResponse()
                                