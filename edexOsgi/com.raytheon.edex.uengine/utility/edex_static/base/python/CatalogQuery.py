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
# Query of data catalog
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/14/08                      njensen       Initial Creation.
#    
# 



from com.raytheon.edex.uengine.tasks.query import MetadataCatalogQuery
from com.raytheon.uf.common.message.response import ResponseMessageCatalog


class CatalogQuery():
    
    def __init__(self, pluginName):
        self.__cat = MetadataCatalogQuery(pluginName)
        
    def setDistinctField(self,field):
        self.__cat.setDistinctField(field)   
        
    def addReturnedField(self, name):
        self.__cat.addReturnedField(name,None)
        
    def addMaxReturnedField(self, name):
        self.__cat.addMaxReturnedField(name, None)
    
    def addConstraint(self, name, value, operand="="):   
        self.__cat.addParameter(name, value, operand)
    
    def execute(self):
        return self.__cat.execute()
    
    def executeWrapped(self):
        return ResponseMessageCatalog.wrap(self.__cat.execute())
