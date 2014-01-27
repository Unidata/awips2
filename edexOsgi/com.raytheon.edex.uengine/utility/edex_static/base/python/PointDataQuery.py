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
from com.raytheon.uf.edex.pointdata import PointDataQuery as JavaPointDataQuery
from com.raytheon.uf.common.message.response import ResponseMessageGeneric

#
# Query of point data
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/15/09                      chammack       Initial Creation.
#    
# 

class PointDataQuery():
    
    def __init__(self, pluginName):
        self._pdq = JavaPointDataQuery(pluginName)
        
    def setRequestedParameters(self, parameters):
        self._pdq.setParameters(parameters)   

    def addConstraint(self, name, value, operand="="):   
        self._pdq.addParameter(name, value, operand)
        
    def requestAllLevels(self):
        self._pdq.requestAllLevels()
        
    def requestSpecificLevel(self, levelParameter, levelValues):
        self._pdq.requestSpecificLevel(levelParameter, levelValues)
    
    def getAvailableParameters(self):
        return self._pdq.getAvailableParameters()
    
    def makeNullResponse(self):        
        response = ArrayList()
        return response
    
    def execute(self):
        return ResponseMessageGeneric(self._pdq.execute())