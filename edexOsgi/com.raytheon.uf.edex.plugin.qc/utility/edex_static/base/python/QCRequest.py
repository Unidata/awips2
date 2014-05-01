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
from com.raytheon.uf.edex.plugin.qc.common import QCDataQuery as JavaQCPointDataQuery
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from com.raytheon.uf.common.message.response import ResponseMessageError

#
# Request of mesonet qc records
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/01/09        3408          bphillip       Initial Creation.
#    
# 

class QCRequest(PointDataQuery.PointDataQuery):
    
    def __init__(self, pluginNameDummyVar):
        self._pdq = JavaQCPointDataQuery()

    def addParameter(self, name, value, operand="=",className=None):    
        self._pdq.addParameter(name, value, operand)
    
    def addList(self, name, value,className=None):
        self._pdq.addParameter(name, value, "in")

    def setCount(self, count):
        pass
 
    def setSortValue(self, sortValue,order,className=None):
        self._pdq.setSortBy(sortValue,order,className)
        
    def setOrderByList(self,orderList,ascending,className=None):
        self._pdq.setSortBy(orderList, ascending,className)


    def execute(self):
        result = self._pdq.execute()
        if result is None:
            errorResponse =  ResponseMessageError()
            errorResponse.setErrorMsg("Error retrieving QC data!")
            return errorResponse
        else:
            return ResponseMessageGeneric(result)
    