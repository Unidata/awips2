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
#    05/20/14        2913          bsteffen       Remove image creation
#    
# 

class RadarRequest(BaseRequest.BaseRequest):
	
	def __init__(self):
		BaseRequest.BaseRequest.__init__(self, "radar")
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
				