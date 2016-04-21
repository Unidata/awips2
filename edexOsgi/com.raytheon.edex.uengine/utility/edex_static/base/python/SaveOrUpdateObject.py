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

from com.raytheon.uf.edex.database.tasks import SaveOrUpdateTask
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
#    12/11/2008      1777          bphillip       Initial Creation.
#    
# 

class SaveOrUpdateObject():
    
    def __init__(self, dbName="metadata"):
        self.__task = SaveOrUpdateTask(dbName)
        
    def addObject(self, object):
        self.__task.addObject(object)
    
    def execute(self):
        resultCount = self.__task.execute()
        response = ArrayList()
        response.add(ResponseMessageGeneric(resultCount))
        return response 