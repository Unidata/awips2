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

#
# Request of satellite image script 
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/14/08                      njensen       Initial Creation.
#    
# 

class RadarRequest(BaseRequest.BaseRequest):
	
	def __init__(self):
		BaseRequest.BaseRequest.__init__(self, "radar")
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
		from com.raytheon.edex.uengine.tasks.radar import DecodeRadarImage
		from com.raytheon.edex.uengine.tasks.process import ColorMapImage, ReprojectImage, ImageOut
		from com.raytheon.edex.uengine.tasks.output import FileOut
		from com.raytheon.edex.uengine.tasks.response import MakeResponseUri
		response = ArrayList()
		size = self.queryResults.size()
		for i in range(size):
			currentQuery = self.queryResults.get(i)
			fileIn = FileIn(self.plugin, currentQuery)
			records = fileIn.retrieveGroup()
			radarImage = DecodeRadarImage(currentQuery, records)
			geom = radarImage.getGridGeometry()
			crs = radarImage.getCrs()
			colorMap = ColorMapImage(self.__colormap, radarImage.execute(), geom)
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
				