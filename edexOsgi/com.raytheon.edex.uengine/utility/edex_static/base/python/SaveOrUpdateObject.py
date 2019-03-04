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
#    12/11/2008      1777          bphillip       Initial Creation.
#    
# 



from com.raytheon.uf.edex.database.tasks import SaveOrUpdateTask
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from java.util import ArrayList


class SaveOrUpdateObject():
    
    def __init__(self, dbName="metadata"):
        self.__task = SaveOrUpdateTask(dbName)
        
    def addObject(self, object):
        self.__task.addObject(object)
    
    def execute(self):
        resultCount = self.__task.execute()
        response = ArrayList()
        response.add(ResponseMessageGeneric(resultCount))
        return response 