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

from com.raytheon.edex.uengine.tasks.query import CatalogQuery

#
# Generalized query script for querying arbitrary rows out of any table in any database
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/05/08        #875          bphillip       Initial Creation.
#    06/12/08                      M. Duff        Added setOrderByList.
#    08/07/08                      M. Duff        Added maxResults.
#    
# 

class DbQuery():
    
    def __init__(self, dbName, className):
        self.__cat = CatalogQuery(dbName,className)
        
    def setDistinctField(self,field):
        self.__cat.setDistinctField(field)   
    
    def addReturnedField(self, name):
        self.__cat.addReturnedField(name)
        
    def setReturnedFieldList(self,nameList):
        self.__cat.setReturnedFieldList(nameList)
    
    def addConstraint(self, name, value, operand="="):   
        self.__cat.addParameter(name, value, operand)
        
    def setOrderByList(self,orderList,ascending):
        self.__cat.setSortBy(orderList, ascending)
        
    def setCount(self, count):
        self.__cat.setCount(count)
        
    def setMaxResults(self,maxResults):
        self.__cat.setCount(maxResults)
    
    def execute(self):
        return self.__cat.execute()