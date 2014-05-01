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
#
#

class McidasRequest(BaseRequest.BaseRequest):

    def __init__(self,):
        BaseRequest.BaseRequest.__init__(self, "mcidas")
        self.__createImage = False
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

    def requestImage(self, image):
        self.__createImage = image

    def requestKml(self, kml):
        self.__kml = kml

    def execute(self):
        self.queryResults = self.query.execute()
        if self.queryResults is None or self.queryResults.size() == 0:
            self.makeNullResponse()
        else:
            if self.__createImage:
                return self.__makeImageResponse()
            else:
                return self.makeResponse()

    def __makeImageResponse(self):
        from com.raytheon.edex.uengine.tasks.decode import FileIn
        from com.raytheon.edex.uengine.tasks.process import ColorMapImage, ReprojectImage, ImageOut
        from com.raytheon.edex.uengine.tasks.output import FileOut
        from com.raytheon.edex.uengine.tasks.response import MakeResponseUri
        from com.raytheon.uf.common.geospatial import MapUtil
        response = ArrayList()
        size = self.queryResults.size()
        for i in range(size):
            currentQuery = self.queryResults.get(i)
            geom = MapUtil.getGridGeometry(currentQuery.getSpatialObject())
            crs = geom.getCoordinateReferenceSystem()
            fileIn = FileIn(self.plugin, currentQuery)
            record = fileIn.execute()
            colorMap = ColorMapImage(self.__colormap, record.getDataObject(), geom)
            imageOut = None
            if self.__reproject:
                reproject = ReprojectImage(colorMap.execute(), geom, crs)
                reprojectedImage = reproject.execute()
                imageOut = ImageOut(reprojectedImage, self.__format, reproject.getGridGeometry())
            else:
                imageOut = ImageOut(colorMap.execute(), self.__format, geom)
            fileOut = FileOut(imageOut.execute(), self.__format)
            writeFile = fileOut.execute()
            makeResponse = MakeResponseUri(writeFile, None, currentQuery.getIdentifier(), self.__format)
            response.add(makeResponse.execute())
            if self.__kml:
                from com.raytheon.edex.uengine.tasks.output import KmlImage
                kmlImage = KmlImage(writeFile, geom)
                kmlFile = kmlImage.execute()
                kmlResponse = MakeResponseUri(kmlFile, None, None, "kml")
                response.add(kmlResponse.execute())
        return response