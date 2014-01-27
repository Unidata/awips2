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
#    
#
 
class GridRequest(BaseRequest.BaseRequest):
    
    def __init__(self, pluginName):
        BaseRequest.BaseRequest.__init__(self, pluginName)
        self.__createImage = False
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
    
    def requestImage(self, image):
        self.__createImage = image
        
    def requestKml(self, kml):
        self.__kml = kml

    def execute(self):        
        self.queryResults = self.query.execute()
        if self.queryResults is None or self.queryResults.size() == 0:
            return self.makeNullResponse()
        else:
            if self.__createImage:
                return self.__makeImageResponse()
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
    
    def __makeImageResponse(self):                
        from com.raytheon.edex.uengine.tasks.grib import GribMap
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
            gribMap = GribMap(self.plugin, self.__colormap, fileIn.execute(), geom)
            gribMap.setScaleFactor(self.__scaleFactor)
            imageData = gribMap.execute()
            geom = gribMap.getGridGeometry()
            colorMap = ColorMapImage(self.__colormap, imageData, geom)
            imageOut = None
            if self.__reproject:
                reproject = ReprojectImage(colorMap.execute(), geom, crs)
                reprojectedImage = reproject.execute()
                imageOut = ImageOut(reprojectedImage, self.__format, reproject.getGridGeometry())
            else:
                imageOut = ImageOut(colorMap.execute(), self.__format,geom)
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
                