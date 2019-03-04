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
# Request of metar records (provides interface to pointdata)
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/03/09                      chammack       Initial Creation.
#    
# 



import PointDataQuery
from java.util import ArrayList
from com.raytheon.edex.plugin.obs.metar import MetarPointDataTransform
from com.raytheon.uf.common.message.response import ResponseMessageGeneric


class ObsRequest(PointDataQuery.PointDataQuery):
    
    def __init__(self, pluginNameDummyVar):
        PointDataQuery.PointDataQuery.__init__(self, "obs")

    def addParameter(self, name, value, operand="=",className=None):    
        self._pdq.addParameter(name, value, operand)
    
    def addList(self, name, value,className=None):
        self._pdq.addParameter(name, value, "in")

    def setCount(self, count):
        # self._pdq.setCount(count)
        pass
 
    def setSortValue(self, sortValue,order,className=None):
        self._pdq.setSortBy(sortValue,order,className)
        
    def setOrderByList(self,orderList,ascending,className=None):
        self._pdq.setSortBy(orderList, ascending,className)


    def execute(self):
        self.requestAllLevels()
        self._pdq.setParameters(MetarPointDataTransform.ALL_PARAMS_LIST)        
        self.queryResults = self._pdq.execute()
        if self.queryResults is None:
            response = ArrayList()
            return response
        else:
            records = MetarPointDataTransform.toMetarRecords(self.queryResults)
            size = len(records)
            response = ArrayList()
            for i in range(size):
                response.add(ResponseMessageGeneric(records[i]))
            return response
    