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

from com.raytheon.uf.edex.database.tasks import SqlQueryTask
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from java.util import ArrayList

#
# Generalized query script for querying arbitrary rows out of any table in any database
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/16/08        #1615          bphillip       Initial Creation.
#    
# 

class SqlQuery():
    
    def __init__(self, sqlQuery,dbName="metadata"):
        self.__query = SqlQueryTask(sqlQuery,dbName)
    
    def execute(self):
        queryResults = self.__query.execute()
        response = ArrayList()
        response.add(ResponseMessageGeneric(queryResults))
        return response 