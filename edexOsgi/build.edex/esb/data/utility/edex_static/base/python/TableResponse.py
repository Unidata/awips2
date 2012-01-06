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
from com.raytheon.edex.uengine.tasks.process import TableUpdate
from com.raytheon.uf.common.message.response import ResponseMessageGeneric

#
# Update of table data 
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/17/08                      njensen       Initial Creation.
#    
# 

class TableResponse():
    
    def __init__(self, db, table, row):
        self.__database = db
        self.__tableName = table
        self.__rowName = row
        self.__updateResults = None
        self.__update = TableUpdate(self.__database, self.__tableName, self.__rowName)
    
    def execute(self):
        self.__updateResults = self.__update.execute()
        return self.__updateResults
    
    def makeXmlResponse(self):
        size = self.__updateResults.size()
        response = ArrayList()
        for i in range(size):
            makeResponse = ResponseMessageGeneric(self.__updateResults.get(i))
            response.add(makeResponse.execute())
        return response
    