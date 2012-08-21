##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

import PointDataQuery
from java.util import ArrayList
from com.raytheon.edex.plugin.obs.metar import MetarPointDataTransform
from com.raytheon.uf.common.message.response import ResponseMessageGeneric

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
    