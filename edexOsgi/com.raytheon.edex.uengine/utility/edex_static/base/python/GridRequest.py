##
##

##
# This is a base file that is not intended to be overridden.
##

##
# uengine is deprecated and will be removed from the system soon. Migrate your
# apps to using the Data Access Framework (DAF).
##

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



import BaseRequest
from java.util import ArrayList
from com.raytheon.edex.uengine.tasks.decode import FileIn

 
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
                