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
#    10/28/10        5705          cjeanbap       Updated SfcObsPointDataTransform.MAN_PARAMS_LISTs
#    01/13/11        5705          cjeanbap       Added makeNullResponse method.
#    12/05/13        2537          bsteffen       Update package for the transform.
# 
#



import PointDataQuery
from java.util import ArrayList
from com.raytheon.edex.plugin.sfcobs import SfcObsPointDataTransform
from com.raytheon.uf.common.message.response import ResponseMessageGeneric


class SfcObsRequest(PointDataQuery.PointDataQuery):
    
    def __init__(self, pluginNameDummyVar):
        PointDataQuery.PointDataQuery.__init__(self, "sfcobs")

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
        print "SfcObsRequest.py"
        self.requestAllLevels()
        self._pdq.setParameters(SfcObsPointDataTransform.MAN_PARAMS_LIST)        
        self.queryResults = self._pdq.execute()
        if self.queryResults is None:
            return self.makeNullResponse()
        else:
            print "performing transform"
            records = SfcObsPointDataTransform.toSfcObsRecords(self.queryResults)
            size = len(records)
            response = ArrayList()
            for i in range(size):
                response.add(ResponseMessageGeneric(records[i]))
            return response
    