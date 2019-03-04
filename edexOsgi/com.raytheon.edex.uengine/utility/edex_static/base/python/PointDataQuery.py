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
# Query of point data
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/15/09                      chammack       Initial Creation.
#    
# 



from java.util import ArrayList
from com.raytheon.uf.edex.pointdata import PointDataQuery as JavaPointDataQuery
from com.raytheon.uf.common.message.response import ResponseMessageGeneric


class PointDataQuery():
    
    def __init__(self, pluginName):
        self._pdq = JavaPointDataQuery(pluginName)
        
    def setRequestedParameters(self, parameters):
        self._pdq.setParameters(parameters)   

    def addConstraint(self, name, value, operand="="):   
        self._pdq.addParameter(name, value, operand)
        
    def requestAllLevels(self):
        self._pdq.requestAllLevels()
        
    def requestSpecificLevel(self, levelParameter, levelValues):
        self._pdq.requestSpecificLevel(levelParameter, levelValues)
    
    def getAvailableParameters(self):
        return self._pdq.getAvailableParameters()
    
    def makeNullResponse(self):        
        response = ArrayList()
        return response
    
    def execute(self):
        return ResponseMessageGeneric(self._pdq.execute())