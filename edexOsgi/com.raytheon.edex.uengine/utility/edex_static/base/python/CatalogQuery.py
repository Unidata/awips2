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

from com.raytheon.edex.uengine.tasks.query import MetadataCatalogQuery
from com.raytheon.uf.common.message.response import ResponseMessageCatalog

#
# Query of data catalog
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/14/08                      njensen       Initial Creation.
#    
# 

class CatalogQuery():
    
    def __init__(self, pluginName):
        self.__cat = MetadataCatalogQuery(pluginName)
        
    def setDistinctField(self,field):
        self.__cat.setDistinctField(field)   
        
    def addReturnedField(self, name):
        self.__cat.addReturnedField(name,None)
        
    def addMaxReturnedField(self, name):
        self.__cat.addMaxReturnedField(name, None)
    
    def addConstraint(self, name, value, operand="="):   
        self.__cat.addParameter(name, value, operand)
    
    def execute(self):
        return self.__cat.execute()
    
    def executeWrapped(self):
        return ResponseMessageCatalog.wrap(self.__cat.execute())
