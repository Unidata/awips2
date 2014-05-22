import BaseRequest
from java.util import ArrayList

#
# Request of mcidas image script
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/2009         144           T. Lee         Initial Creation.
#    05/20/14        2913          bsteffen       Remove image creation
#
#

class McidasRequest(BaseRequest.BaseRequest):

    def __init__(self,):
        BaseRequest.BaseRequest.__init__(self, "mcidas")
        self.__reproject = False
        self.__colormap = "BW"
        self.__format = "png"
        self.__kml = False

    def setColormap(self, colormap):
        self.__colormap = colormap

    def setFormat(self, format):
        self.__format = format

    def reprojectImage(self, reproject):
        self.__reproject = reproject

    def requestKml(self, kml):
        self.__kml = kml

    def execute(self):
        self.queryResults = self.query.execute()
        if self.queryResults is None or self.queryResults.size() == 0:
            self.makeNullResponse()
        else:
            return self.makeResponse()
