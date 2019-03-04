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
# Request of FSSObs records (provides interface to pointdata)
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------



import PointDataQuery
from java.util import ArrayList
from com.raytheon.uf.common.dataplugin.fssobs import FSSObsRecordTransform
from com.raytheon.uf.common.message.response import ResponseMessageGeneric


class FSSObsRequest(PointDataQuery.PointDataQuery):
    
    def __init__(self, pluginNameDummyVar):
        PointDataQuery.PointDataQuery.__init__(self, "fssobs")

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
        print "FSSObsRequest.py"
        self.requestAllLevels()
        self._pdq.setParameters(FSSObsRecordTransform.FSSOBS_PARAMS_LIST)        
        self.queryResults = self._pdq.execute()
        if self.queryResults is None:
            self.makeNullResponse()
        else:
            print "performing transform"
            records = FSSObsRecordTransform.toFSSObsRecords(self.queryResults)
            size = len(records)
            response = ArrayList()
            for i in range(size):
                response.add(ResponseMessageGeneric(records[i]))
            return response
    