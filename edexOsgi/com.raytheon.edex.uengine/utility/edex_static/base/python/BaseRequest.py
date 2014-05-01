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
from com.raytheon.edex.uengine.tasks.query import TermQuery
from com.raytheon.edex.uengine.tasks.query import TableQuery
from com.raytheon.uf.common.message.response import ResponseMessageGeneric

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
                                