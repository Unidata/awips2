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

import BaseRequest
from java.util import ArrayList
from com.raytheon.edex.uengine.tasks.decode import FileIn

#
# Request of grid image script  
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/14/08                      njensen       Initial Creation.
#    05/20/14        2913          bsteffen       Remove image creation
#    
#
 
class GridRequest(BaseRequest.BaseRequest):
    
    def __init__(self, pluginName):
        BaseRequest.BaseRequest.__init__(self, pluginName)
        self.__reproject = False
        self.__colormap = "BW"
        self.__format = "png"
        self.__scaleFactor = 1
        self.__kml = False

    def setColormap(self, colormap):
        self.__colormap = colormap
        
    def setFormat(self, format):
        self.__format = format
    
    def setScaleFactor(self, scale):
        self.__scaleFactor = scale
    
    def reprojectImage(self, reproject):
        self.__reproject = reproject
        
    def requestKml(self, kml):
        self.__kml = kml

    def execute(self):        
        self.queryResults = self.query.execute()
        if self.queryResults is None or self.queryResults.size() == 0:
            return self.makeNullResponse()
        else:
            return self.makeResponse()
    
    def makeResponse(self):        
        from com.raytheon.uf.common.message.response import ResponseMessageGeneric
        count = self.queryResults.size()
        response = ArrayList()
        for i in range(count):
            currentQuery = self.queryResults.get(i)          
            response.add(ResponseMessageGeneric(currentQuery))
        return response
    
    def makeNullResponse(self):        
        response = ArrayList()
        return response
                