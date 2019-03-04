##
##

# File auto-generated against equivalent DynamicSerialize Java class
# 
#      SOFTWARE HISTORY
# 
#     Date            Ticket#       Engineer       Description
#     ------------    ----------    -----------    --------------------------
#     Aug 19, 2014                  nabowle        Generated

import numpy

class GetRadarDataRecordRequest(object):

    def __init__(self):
        self.timeRange = None
        self.productCode = None
        self.radarId = None
        self.primaryElevationAngle = None

    def getTimeRange(self):
        return self.timeRange

    def setTimeRange(self, timeRange):
        self.timeRange = timeRange

    def getProductCode(self):
        return self.productCode

    def setProductCode(self, productCode):
        self.productCode = productCode

    def getRadarId(self):
        return self.radarId

    def setRadarId(self, radarId):
        self.radarId = radarId

    def getPrimaryElevationAngle(self):
        return self.primaryElevationAngle

    def setPrimaryElevationAngle(self, primaryElevationAngle):
        self.primaryElevationAngle = numpy.float64(primaryElevationAngle)

