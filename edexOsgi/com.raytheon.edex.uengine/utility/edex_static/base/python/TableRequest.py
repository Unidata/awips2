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

from java.util import ArrayList
from com.raytheon.edex.uengine.tasks.query import TableQuery
from com.raytheon.edex.uengine.tasks.response import MakeResponseXml
from com.raytheon.edex.uengine.tasks.response import MakeResponseNull
from com.raytheon.uf.common.message.response import ResponseMessageGeneric

#
# Request of table data 
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/17/08                      njensen       Initial Creation.
#    
#

class TableRequest():
    def __init__(self, db, tbl):
        self.__database = db
        self.__table = tbl
        self.__queryResults = None
        self.__query = TableQuery(self.__database, self.__table)
    
    def addParameter(self, name, value, operand="="):
        self.__query.addParameter(name, value, operand)
    
    def addList(self, name, value):
        self.__query.addList(name, value)

    def setCount(self, count):
        self.__query.setCount(count)
    
    def setSortValue(self, sortValue):
        self.__query.setSortBy(sortValue)
    
    def setTableName(self, name):
        self.__table = name
    
    def setDatabase(self, name):
        self.__database = name
    
    def __makeXmlResponse(self):
        size = self.__queryResults.size()
        response = ArrayList()
        for i in range(size):
            response.add(ResponseMessageGeneric(self.__queryResults.get(i)))
        return response
    
    def execute(self):
        self.__queryResults = self.__query.execute()
        return self.__makeXmlResponse()
         